--  ipc-pty.adb: PTY creation and management.
--  Copyright (C) 2024 streaksu
--
--  This program is free software: you can redistribute it and/or modify
--  it under the terms of the GNU General Public License as published by
--  the Free Software Foundation, either version 3 of the License, or
--  (at your option) any later version.
--
--  This program is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with this program.  If not, see <http://www.gnu.org/licenses/>.

with System.Address_To_Access_Conversions;
with Ada.Unchecked_Deallocation;
with Scheduler;
with Lib.Messages;
with Devices.TermIOs; use Devices.TermIOs;
with Userland.Process; use Userland.Process;
with Arch.Local;

package body IPC.PTY is
   pragma Suppress (All_Checks);

   procedure Free is new Ada.Unchecked_Deallocation (Inner, Inner_Acc);
   package   Conv is new System.Address_To_Access_Conversions (Inner);

   Tracked_Lock : aliased Lib.Synchronization.Binary_Semaphore :=
      Lib.Synchronization.Unlocked_Semaphore;
   Tracked_Name : Natural := 1;

   function Create return Inner_Acc is
      Name_Index : Natural;
      Modes      : constant Devices.TermIOs.Local_Modes := (others => False);
      Result     : Inner_Acc;
      Resource   : Devices.Resource;
      Success    : Boolean;
      Num_Str    : Lib.Messages.Translated_String;
      Num_Len    : Natural;
   begin
      Lib.Synchronization.Seize (Tracked_Lock);
      Name_Index := Tracked_Name;
      Tracked_Name := Tracked_Name + 1;
      Lib.Synchronization.Release (Tracked_Lock);

      Result := new Inner'
         (Primary_Mutex      => Lib.Synchronization.Unlocked_Semaphore,
          Secondary_Mutex    => Lib.Synchronization.Unlocked_Semaphore,
          Global_Data_Mutex  => Lib.Synchronization.Unlocked_Semaphore,
          Primary_Read       => True,
          Primary_Transmit   => True,
          Secondary_Read     => True,
          Secondary_Transmit => True,
          Device_Handle      => Devices.Error_Handle,
          Name_Index         => Name_Index,
          Term_Info          => (0, 0, 0, Modes, (others => 0), 0, 0),
          Term_Size          => (others => 0),
          Was_Closed         => False,
          Termios_Changed    => False,
          Primary_Length     => 0,
          Secondary_Length   => 0,
          Primary_Data       => (others => 0),
          Secondary_Data     => (others => 0));

      Lib.Messages.Image (Unsigned_32 (Name_Index), Num_Str, Num_Len);

      Resource :=
         (Data        => Conv.To_Address (Conv.Object_Pointer (Result)),
          ID          => (others => 0),
          Is_Block    => False,
          Block_Size  => 4096,
          Block_Count => 0,
          Read        => Dev_Read'Access,
          Write       => Dev_Write'Access,
          Sync        => null,
          Sync_Range  => null,
          IO_Control  => null,
          Mmap        => null,
          Poll        => null,
          Remove      => null);

      declare
         Final_Name : constant String := "pty" &
            Num_Str (Num_Str'Last - Num_Len + 1 .. Num_Str'Last);
      begin
         Devices.Register (Resource, Final_Name, Success);
         if Success then
            Result.Device_Handle := Devices.Fetch (Final_Name);
            return Result;
         else
            Free (Result);
            return null;
         end if;
      end;
   end Create;

   procedure Close (Closed : in out Inner_Acc) is
      Discard : Boolean;
   begin
      Lib.Synchronization.Seize (Closed.Primary_Mutex);
      Lib.Synchronization.Seize (Closed.Secondary_Mutex);
      Lib.Synchronization.Seize (Closed.Global_Data_Mutex);
      if Closed.Was_Closed then
         Devices.Remove (Closed.Device_Handle, Discard);
         Free (Closed);
      else
         Closed.Was_Closed := True;
         Lib.Synchronization.Release (Closed.Primary_Mutex);
         Lib.Synchronization.Release (Closed.Secondary_Mutex);
         Lib.Synchronization.Release (Closed.Global_Data_Mutex);
      end if;
   end Close;

   procedure Read_Primary
      (To_Read     : Inner_Acc;
       Data        : out Devices.Operation_Data;
       Is_Blocking : Boolean;
       Ret_Count   : out Natural;
       Success     : out Status)
   is
   begin
      Read_From_End
         (To_Read.Primary_Mutex'Access, To_Read.Primary_Length'Access,
          To_Read.Primary_Data'Access, Is_Blocking,
          To_Read.Primary_Read, Data, Ret_Count);
      Success := PTY_Success;
   end Read_Primary;

   procedure Write_Primary
      (To_Write    : Inner_Acc;
       Data        : Devices.Operation_Data;
       Is_Blocking : Boolean;
       Ret_Count   : out Natural;
       Success     : out Status)
   is
   begin
      Write_To_End
         (To_Write.Secondary_Mutex'Access, To_Write.Secondary_Length'Access,
          To_Write.Secondary_Data'Access, Is_Blocking,
          To_Write.Primary_Transmit, Data, Ret_Count);
      Success := PTY_Success;
   end Write_Primary;

   procedure Read_Secondary
      (To_Read     : Inner_Acc;
       Data        : out Devices.Operation_Data;
       Is_Blocking : Boolean;
       Ret_Count   : out Natural;
       Success     : out Status)
   is
   begin
      Read_From_End
         (To_Read.Secondary_Mutex'Access, To_Read.Secondary_Length'Access,
          To_Read.Secondary_Data'Access, Is_Blocking,
          To_Read.Secondary_Read, Data, Ret_Count);
      Success := PTY_Success;
   end Read_Secondary;

   procedure Write_Secondary
      (To_Write    : Inner_Acc;
       Data        : Devices.Operation_Data;
       Is_Blocking : Boolean;
       Ret_Count   : out Natural;
       Success     : out Status)
   is
   begin
      Write_To_End
         (To_Write.Primary_Mutex'Access, To_Write.Primary_Length'Access,
          To_Write.Primary_Data'Access, Is_Blocking,
          To_Write.Secondary_Transmit, Data, Ret_Count);
      Success := PTY_Success;
   end Write_Secondary;

   procedure Poll_Primary
      (P         : Inner_Acc;
       Can_Read  : out Boolean;
       Can_Write : out Boolean;
       Can_Prio  : out Boolean)
   is
   begin
      Lib.Synchronization.Seize (P.Global_Data_Mutex);
      Can_Prio := P.Termios_Changed;
      P.Termios_Changed := False;
      Lib.Synchronization.Release (P.Global_Data_Mutex);

      Lib.Synchronization.Seize (P.Primary_Mutex);
      Can_Read  := P.Primary_Length /= 0;
      Can_Write := P.Primary_Length /= P.Primary_Data'Length;
      Lib.Synchronization.Release (P.Primary_Mutex);
   end Poll_Primary;

   procedure Poll_Secondary
      (P         : Inner_Acc;
       Can_Read  : out Boolean;
       Can_Write : out Boolean)
   is
   begin
      Lib.Synchronization.Seize (P.Secondary_Mutex);
      Can_Read  := P.Secondary_Length /= 0;
      Can_Write := P.Secondary_Length /= P.Secondary_Data'Length;
      Lib.Synchronization.Release (P.Secondary_Mutex);
   end Poll_Secondary;

   procedure Get_TermIOs (P : Inner_Acc; T : out Devices.TermIOs.Main_Data) is
   begin
      Lib.Synchronization.Seize (P.Global_Data_Mutex);
      T := P.Term_Info;
      Lib.Synchronization.Release (P.Global_Data_Mutex);
   end Get_TermIOs;

   procedure Set_TermIOs (P : Inner_Acc; T : Devices.TermIOs.Main_Data) is
   begin
      Lib.Synchronization.Seize (P.Global_Data_Mutex);
      P.Term_Info := T;
      P.Termios_Changed := True;
      Lib.Synchronization.Release (P.Global_Data_Mutex);
   end Set_TermIOs;

   procedure Get_WinSize (P : Inner_Acc; W : out Devices.TermIOs.Win_Size) is
   begin
      Lib.Synchronization.Seize (P.Global_Data_Mutex);
      W := P.Term_Size;
      Lib.Synchronization.Release (P.Global_Data_Mutex);
   end Get_WinSize;

   procedure Set_WinSize (P : Inner_Acc; W : Devices.TermIOs.Win_Size) is
   begin
      Lib.Synchronization.Seize (P.Global_Data_Mutex);
      P.Term_Size := W;
      P.Termios_Changed := True;
      Lib.Synchronization.Release (P.Global_Data_Mutex);
   end Set_WinSize;

   procedure Get_Name (P : Inner_Acc; Str : out String; Len : out Natural) is
      Root_Name  : constant String := "/dev/pty";
      Buffer     : Lib.Messages.Translated_String;
      Buffer_Len : Natural;
   begin
      Lib.Messages.Image (Unsigned_32 (P.Name_Index), Buffer, Buffer_Len);
      Len := Root_Name'Length + Buffer_Len;
      if Str'Length >= Len then
         Str (Str'First .. Str'First + Root_Name'Length - 1) := Root_Name;
         Str (Str'First + Root_Name'Length .. Str'First + Len - 1) :=
            Buffer (Buffer'Last - Buffer_Len + 1 .. Buffer'Last);
      end if;
   end Get_Name;

   procedure Flush_Primary (P : Inner_Acc; To_Read, To_Transmit : Boolean) is
   begin
      if To_Read then
         Lib.Synchronization.Seize (P.Primary_Mutex);
         P.Primary_Data   := (others => 0);
         P.Primary_Length := 0;
         Lib.Synchronization.Release (P.Primary_Mutex);
      end if;
      if To_Transmit then
         Lib.Synchronization.Seize (P.Secondary_Mutex);
         P.Secondary_Data   := (others => 0);
         P.Secondary_Length := 0;
         Lib.Synchronization.Release (P.Secondary_Mutex);
      end if;
   end Flush_Primary;

   procedure Flush_Secondary (P : Inner_Acc; To_Read, To_Transmit : Boolean) is
   begin
      if To_Read then
         Lib.Synchronization.Seize (P.Secondary_Mutex);
         P.Secondary_Data   := (others => 0);
         P.Secondary_Length := 0;
         Lib.Synchronization.Release (P.Secondary_Mutex);
      end if;
      if To_Transmit then
         Lib.Synchronization.Seize (P.Primary_Mutex);
         P.Primary_Data   := (others => 0);
         P.Primary_Length := 0;
         Lib.Synchronization.Release (P.Primary_Mutex);
      end if;
   end Flush_Secondary;

   procedure Start_Primary (P : Inner_Acc; To_Read, To_Transmit : Boolean) is
   begin
      Lib.Synchronization.Seize (P.Global_Data_Mutex);
      if To_Read then
         P.Primary_Read := True;
      end if;
      if To_Transmit then
         P.Primary_Transmit := True;
      end if;
      Lib.Synchronization.Release (P.Global_Data_Mutex);
   end Start_Primary;

   procedure Start_Secondary (P : Inner_Acc; To_Read, To_Transmit : Boolean) is
   begin
      Lib.Synchronization.Seize (P.Global_Data_Mutex);
      if To_Read then
         P.Secondary_Read := True;
      end if;
      if To_Transmit then
         P.Secondary_Transmit := True;
      end if;
      Lib.Synchronization.Release (P.Global_Data_Mutex);
   end Start_Secondary;

   procedure Stop_Primary (P : Inner_Acc; To_Read, To_Transmit : Boolean) is
   begin
      Lib.Synchronization.Seize (P.Global_Data_Mutex);
      if To_Read then
         P.Primary_Read := False;
      end if;
      if To_Transmit then
         P.Primary_Transmit := False;
      end if;
      Lib.Synchronization.Release (P.Global_Data_Mutex);
   end Stop_Primary;

   procedure Stop_Secondary (P : Inner_Acc; To_Read, To_Transmit : Boolean) is
   begin
      Lib.Synchronization.Seize (P.Global_Data_Mutex);
      if To_Read then
         P.Secondary_Read := False;
      end if;
      if To_Transmit then
         P.Secondary_Transmit := False;
      end if;
      Lib.Synchronization.Release (P.Global_Data_Mutex);
   end Stop_Secondary;

   function IO_Control
      (PTY        : Inner_Acc;
       Is_Primary : Boolean;
       Request    : Unsigned_64;
       Argument   : System.Address) return Boolean
   is
      Proc : constant PID := Arch.Local.Get_Current_Process;
      Result_Info : Main_Data with Import, Address => Argument;
      Result_Size :  Win_Size with Import, Address => Argument;
      Action      :   Integer with Import, Address => Argument;
      Do_R, Do_T  :   Boolean;
      Success     :   Boolean;
   begin
      case Request is
         when TCGETS =>
            Get_TermIOs (PTY, Result_Info);
         when TCSETS | TCSETSW | TCSETSF =>
            Set_TermIOs (PTY, Result_Info);
         when TIOCGWINSZ =>
            Get_WinSize (PTY, Result_Size);
         when TIOCSWINSZ =>
            Set_WinSize (PTY, Result_Size);
         when TCFLSH =>
            case Action is
               when TCIFLUSH | TCOFLUSH | TCIOFLUSH =>
                  Do_R := Action = TCIFLUSH or Action = TCIOFLUSH;
                  Do_T := Action = TCOFLUSH or Action = TCIOFLUSH;
                  if Is_Primary then
                     Flush_Primary (PTY, Do_R, Do_T);
                  else
                     Flush_Secondary (PTY, Do_R, Do_T);
                  end if;
               when others =>
                  return False;
            end case;
         when TCXONC =>
            case Action is
               when TCOOFF | TCIOFF =>
                  Do_R := Action = TCOOFF;
                  Do_T := Action = TCIOFF;
                  if Is_Primary then
                     Stop_Primary (PTY, Do_R, Do_T);
                  else
                     Stop_Secondary (PTY, Do_R, Do_T);
                  end if;
               when TCOON | TCION =>
                  Do_R := Action = TCOON;
                  Do_T := Action = TCION;
                  if Is_Primary then
                     Start_Primary (PTY, Do_R, Do_T);
                  else
                     Start_Secondary (PTY, Do_R, Do_T);
                  end if;
               when others =>
                  return False;
            end case;
         when TIOCSCTTY =>
            Set_Controlling_TTY (Proc, PTY, Success);
            return Success;
         when TIOCNOTTY =>
            Clear_Controlling_TTY (Proc, PTY, Success);
            return Success;
         when others =>
            return False;
      end case;

      return True;
   end IO_Control;
   ----------------------------------------------------------------------------
   procedure Read_From_End
      (End_Mutex   : access Lib.Synchronization.Binary_Semaphore;
       Inner_Len   : access Data_Length;
       Inner_Data  : access TTY_Data;
       Is_Blocking : Boolean;
       Is_Able_To  : Boolean;
       Data        : out Devices.Operation_Data;
       Ret_Count   : out Natural)
   is
   begin
      Data := (others => 0);
      if not Is_Able_To then
         Ret_Count := 0;
         return;
      end if;

      if Is_Blocking then
         loop
            if Inner_Len.all /= 0 then
               Lib.Synchronization.Seize (End_Mutex.all);
               exit when Inner_Len.all /= 0;
               Lib.Synchronization.Release (End_Mutex.all);
            end if;
            Scheduler.Yield_If_Able;
         end loop;
      else
         Lib.Synchronization.Seize (End_Mutex.all);
         if Inner_Len.all = 0 then
            Ret_Count := 0;
            Lib.Synchronization.Release (End_Mutex.all);
            return;
         end if;
      end if;

      if Data'Length > Inner_Len.all then
         Ret_Count := Inner_Len.all;
      else
         Ret_Count := Data'Length;
      end if;
      if Data'First > Natural'Last - Ret_Count then
         Ret_Count := Natural'Last - Data'First;
      end if;

      Data (Data'First .. Data'First + Ret_Count - 1) :=
         Inner_Data (1 .. Ret_Count);
      for I in 1 .. Ret_Count loop
         for J in Inner_Data'First .. Inner_Data'Last - 1 loop
            Inner_Data (J) := Inner_Data (J + 1);
         end loop;
         if Inner_Len.all > 0 then
            Inner_Len.all := Inner_Len.all - 1;
         else
            exit;
         end if;
      end loop;

      Lib.Synchronization.Release (End_Mutex.all);
   end Read_From_End;

   procedure Write_To_End
      (End_Mutex   : access Lib.Synchronization.Binary_Semaphore;
       Inner_Len   : access Data_Length;
       Inner_Data  : access TTY_Data;
       Is_Blocking : Boolean;
       Is_Able_To  : Boolean;
       Data        : Devices.Operation_Data;
       Ret_Count   : out Natural)
   is
      Final : Natural;
   begin
      if not Is_Able_To then
         Ret_Count := 0;
         return;
      end if;

      if Is_Blocking then
         loop
            if Inner_Len.all /= Inner_Data'Length then
               Lib.Synchronization.Seize (End_Mutex.all);
               exit when Inner_Len.all /= Inner_Data'Length;
               Lib.Synchronization.Release (End_Mutex.all);
            end if;
            Scheduler.Yield_If_Able;
         end loop;
      else
         Lib.Synchronization.Seize (End_Mutex.all);
         if Inner_Len.all = Data'Length then
            Ret_Count := 0;
            Lib.Synchronization.Release (End_Mutex.all);
            return;
         end if;
      end if;

      if Data'Length > Inner_Data'Length or else
         Data'Length > Inner_Data'Length - Inner_Len.all
      then
         Final := Inner_Data'Length;
         Ret_Count := Inner_Data'Length - Inner_Len.all;
      else
         Final := Inner_Len.all + Data'Length;
         Ret_Count := Data'Length;
      end if;
      if Data'First > Natural'Last - Ret_Count then
         Ret_Count := Natural'Last - Data'First;
         Final := Inner_Len.all + Ret_Count;
      end if;

      Inner_Data (Inner_Len.all + 1 .. Final) :=
         Data (Data'First .. Data'First + Ret_Count - 1);
      Inner_Len.all := Final;
      Lib.Synchronization.Release (End_Mutex.all);
   end Write_To_End;
   ----------------------------------------------------------------------------
   procedure Dev_Read
      (Key         : System.Address;
       Offset      : Unsigned_64;
       Data        : out Devices.Operation_Data;
       Ret_Count   : out Natural;
       Success     : out Boolean;
       Is_Blocking : Boolean)
   is
      pragma Unreferenced (Offset);

      PTY  : constant Inner_Acc := Inner_Acc (Conv.To_Pointer (Key));
      Succ : Status;
   begin
      Read_Secondary
         (To_Read     => PTY,
          Data        => Data,
          Is_Blocking => Is_Blocking,
          Ret_Count   => Ret_Count,
          Success     => Succ);
      Success := Succ = PTY_Success;
   end Dev_Read;

   procedure Dev_Write
      (Key         : System.Address;
       Offset      : Unsigned_64;
       Data        : Devices.Operation_Data;
       Ret_Count   : out Natural;
       Success     : out Boolean;
       Is_Blocking : Boolean)
   is
      pragma Unreferenced (Offset);

      PTY  : constant Inner_Acc := Inner_Acc (Conv.To_Pointer (Key));
      Succ : Status;
   begin
      Write_Secondary
         (To_Write    => PTY,
          Data        => Data,
          Is_Blocking => Is_Blocking,
          Ret_Count   => Ret_Count,
          Success     => Succ);
      Success := Succ = PTY_Success;
   end Dev_Write;

   function Dev_IO_Control
      (Key      : System.Address;
       Request  : Unsigned_64;
       Argument : System.Address) return Boolean
   is
   begin
      return IO_Control
         (PTY        => Inner_Acc (Conv.To_Pointer (Key)),
          Is_Primary => False,
          Request    => Request,
          Argument   => Argument);
   end Dev_IO_Control;
end IPC.PTY;
