--  arch-syscall.adb: Syscall table and implementation.
--  Copyright (C) 2021 streaksu
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

with System.Storage_Elements; use System.Storage_Elements;
with Ada.Characters.Latin_1;
with Config;
with System; use System;
with Lib.Messages;
with Lib;
with Networking;
with Userland.Loader;
with VFS.File; use VFS.File;
with VFS; use VFS;
with Scheduler; use Scheduler;
with Memory.Virtual; use Memory.Virtual;
with Memory.Physical;
with Memory; use Memory;
with Ada.Unchecked_Deallocation;
with Ada.Unchecked_Conversion;
with Interfaces.C;
with Arch.Hooks;
with Arch.Local;
with Cryptography.Random;
with Cryptography.AES;
with Arch.Snippets;
with Userland.MAC;

package body Userland.Syscall with SPARK_Mode => Off is
   --  Whether we are to print syscall information and MAC.
   Is_Tracing    : Boolean := False;
   MAC_Is_Locked : Boolean := False;

   procedure Free_Str is new Ada.Unchecked_Deallocation
      (String, Userland.String_Acc);
   procedure Free_File is new Ada.Unchecked_Deallocation
      (VFS.File.File, VFS.File.File_Acc);

   procedure Set_Tracing (Value : Boolean) is
   begin
      Is_Tracing := Value;
   end Set_Tracing;

   procedure Syscall_Exit (Code : Unsigned_64; Errno : out Errno_Value) is
      Curr_Proc : constant Process_Data_Acc := Arch.Local.Get_Current_Process;
   begin
      if Is_Tracing then
         Lib.Messages.Put ("syscall exit(");
         Lib.Messages.Put (Code);
         Lib.Messages.Put_Line (")");
      end if;

      if MAC_Is_Locked and not Curr_Proc.Perms.Caps.Can_Exit_Itself then
         Errno := Error_Bad_Access;
         Execute_MAC_Failure ("exit", Curr_Proc);
         return;
      end if;

      Do_Exit (Curr_Proc, Unsigned_8 (Code));
   end Syscall_Exit;

   procedure Do_Exit (Proc : Process_Data_Acc; Code : Unsigned_8) is
   begin
      --  Remove all state but the return value and keep the zombie around
      --  until we are waited.
      Userland.Process.Flush_Threads (Proc);
      Userland.Process.Flush_Files   (Proc);
      Proc.Exit_Code := Code;
      Proc.Did_Exit  := True;
      Scheduler.Bail;
   end Do_Exit;

   function Syscall_Arch_PRCtl
      (Code     : Unsigned_64;
       Argument : Unsigned_64;
       Errno    : out Errno_Value) return Unsigned_64
   is
      Arg : constant System.Address := To_Address (Integer_Address (Argument));
   begin
      if Is_Tracing then
         Lib.Messages.Put ("syscall arch_prctl(");
         Lib.Messages.Put (Code);
         Lib.Messages.Put (", ");
         Lib.Messages.Put (Argument, False, True);
         Lib.Messages.Put_Line (")");
      end if;

      if Argument = 0 then
         Errno := Error_Would_Fault;
         return Unsigned_64'Last;
      end if;

      if not Arch.Hooks.PRCTL_Hook (Natural (Code), Arg) then
         Errno := Error_Invalid_Value;
         return Unsigned_64'Last;
      else
         Errno := Error_No_Error;
         return 0;
      end if;
   end Syscall_Arch_PRCtl;

   function Syscall_Open
      (Address : Unsigned_64;
       Flags   : Unsigned_64;
       Mode    : Unsigned_64;
       Errno   : out Errno_Value) return Unsigned_64
   is
      pragma Unreferenced (Mode);
      Addr : constant System.Address := To_Address (Integer_Address (Address));
   begin
      if not Check_Userland_Access (To_Integer (Addr)) then
         if Is_Tracing then
            Lib.Messages.Put ("syscall open(BAD_MEM, ");
            Lib.Messages.Put (Flags);
            Lib.Messages.Put_Line (")");
         end if;
         Errno := Error_Would_Fault;
         return Unsigned_64'Last;
      end if;
      declare
         Path_Length  : constant Natural := Lib.C_String_Length (Addr);
         Path_String  : String (1 .. Path_Length) with Address => Addr;
         Current_Proc : constant Userland.Process.Process_Data_Acc :=
            Arch.Local.Get_Current_Process;
         Open_Mode    : VFS.File.Access_Mode;
         Opened_File  : VFS.File.File_Acc;
         File_Perms   : MAC.Filter_Permissions;
         Returned_FD  : Natural;
         Close_On_Exec : constant Boolean := (Flags and O_CLOEXEC) /= 0;
         Flags_Read    : constant Boolean := (Flags and O_RDONLY)  /= 0;
         Flags_Write   : constant Boolean := (Flags and O_WRONLY)  /= 0;
      begin
         if Is_Tracing then
            Lib.Messages.Put ("syscall open(");
            Lib.Messages.Put (Path_String);
            Lib.Messages.Put (", ");
            Lib.Messages.Put (Flags);
            Lib.Messages.Put_Line (")");
         end if;

         --  Parse the mode.
         if Flags_Read and Flags_Write then
            Open_Mode := VFS.File.Access_RW;
         elsif Flags_Read then
            Open_Mode := VFS.File.Access_R;
         elsif Flags_Write then
            Open_Mode := VFS.File.Access_W;
         else
            Errno := Error_Invalid_Value;
            return Unsigned_64'Last;
         end if;

         if MAC_Is_Locked then
            File_Perms := MAC.Check_Path_Permissions
               (Path_String, Current_Proc.Perms.Filters);
            case Open_Mode is
               when VFS.File.Access_RW =>
                  if File_Perms.Can_Read and File_Perms.Can_Write then
                     goto Resume;
                  end if;
               when VFS.File.Access_R =>
                  if File_Perms.Can_Read then
                     goto Resume;
                  end if;
               when VFS.File.Access_W =>
                  if File_Perms.Can_Write then
                     goto Resume;
                  end if;
            end case;
            Errno := Error_Bad_Access;
            Execute_MAC_Failure ("open", Current_Proc);
            return Unsigned_64'Last;
         end if;

         --  Actually open the file.
      <<Resume>>
         Opened_File := VFS.File.Open (Path_String, Open_Mode);

         if Opened_File = null then
            Errno := Error_No_Entity;
            return Unsigned_64'Last;
         elsif not Userland.Process.Add_File
            (Current_Proc, Opened_File, Returned_FD, Close_On_Exec)
         then
            Free_File (Opened_File);
            Errno := Error_Too_Many_Files;
            return Unsigned_64'Last;
         else
            Errno := Error_No_Error;
            return Unsigned_64 (Returned_FD);
         end if;
      end;
   end Syscall_Open;

   function Syscall_Close
      (File_D : Unsigned_64;
       Errno  : out Errno_Value) return Unsigned_64
   is
      Current_Process : constant Userland.Process.Process_Data_Acc :=
         Arch.Local.Get_Current_Process;
   begin
      if Is_Tracing then
         Lib.Messages.Put ("syscall close(");
         Lib.Messages.Put (File_D);
         Lib.Messages.Put_Line (")");
      end if;

      if not Userland.Process.Is_Valid_File (Current_Process, File_D) then
         Errno := Error_Bad_File;
         return Unsigned_64'Last;
      end if;

      Userland.Process.Remove_File (Current_Process, Natural (File_D));
      Errno := Error_No_Error;
      return 0;
   end Syscall_Close;

   function Syscall_Read
      (File_D : Unsigned_64;
       Buffer : Unsigned_64;
       Count  : Unsigned_64;
       Errno  : out Errno_Value) return Unsigned_64
   is
      Buffer_Addr : constant System.Address :=
         To_Address (Integer_Address (Buffer));
      Current_Process : constant Userland.Process.Process_Data_Acc :=
            Arch.Local.Get_Current_Process;
      File : constant VFS.File.File_Acc :=
         Userland.Process.Get_File (Current_Process, File_D);
   begin
      if Is_Tracing then
         Lib.Messages.Put ("syscall read(");
         Lib.Messages.Put (File_D);
         Lib.Messages.Put (", ");
         Lib.Messages.Put (Buffer, False, True);
         Lib.Messages.Put (", ");
         Lib.Messages.Put (Count);
         Lib.Messages.Put_Line (")");
      end if;

      if File = null then
         Errno := Error_Bad_File;
         return Unsigned_64'Last;
      elsif not Check_Userland_Access (To_Integer (Buffer_Addr)) then
         Errno := Error_Would_Fault;
         return Unsigned_64'Last;
      elsif File.Flags /= Access_R and File.Flags /= Access_RW then
         Errno := Error_Invalid_Value;
         return Unsigned_64'Last;
      else
         Errno := Error_No_Error;
         return VFS.File.Read (File, Count, Buffer_Addr);
      end if;
   end Syscall_Read;

   function Syscall_Write
      (File_D : Unsigned_64;
       Buffer : Unsigned_64;
       Count  : Unsigned_64;
       Errno  : out Errno_Value) return Unsigned_64
   is
      Buffer_Addr     : constant System.Address :=
         To_Address (Integer_Address (Buffer));
      Current_Process : constant Userland.Process.Process_Data_Acc :=
            Arch.Local.Get_Current_Process;
      File : constant File_Acc :=
         Userland.Process.Get_File (Current_Process, File_D);
   begin
      if Is_Tracing then
         Lib.Messages.Put ("syscall write(");
         Lib.Messages.Put (File_D);
         Lib.Messages.Put (", ");
         Lib.Messages.Put (Buffer, False, True);
         Lib.Messages.Put (", ");
         Lib.Messages.Put (Count);
         Lib.Messages.Put_Line (")");
      end if;

      if File = null then
         Errno := Error_Bad_File;
         return Unsigned_64'Last;
      elsif not Check_Userland_Access (To_Integer (Buffer_Addr)) then
         Errno := Error_Would_Fault;
         return Unsigned_64'Last;
      elsif File.Flags /= Access_W and File.Flags /= Access_RW then
         Errno := Error_Invalid_Value;
         return Unsigned_64'Last;
      else
         Errno := Error_No_Error;
         return VFS.File.Write (File, Count, Buffer_Addr);
      end if;
   end Syscall_Write;

   function Syscall_Seek
      (File_D : Unsigned_64;
       Offset : Unsigned_64;
       Whence : Unsigned_64;
       Errno  : out Errno_Value) return Unsigned_64
   is
      Current_Process : constant Userland.Process.Process_Data_Acc :=
            Arch.Local.Get_Current_Process;
      File : constant VFS.File.File_Acc :=
         Userland.Process.Get_File (Current_Process, File_D);
      Stat_Val : VFS.File_Stat;
   begin
      if Is_Tracing then
         Lib.Messages.Put ("syscall seek(");
         Lib.Messages.Put (File_D);
         Lib.Messages.Put (", ");
         Lib.Messages.Put (Offset);
         Lib.Messages.Put (", ");
         Lib.Messages.Put (Whence);
         Lib.Messages.Put_Line (")");
      end if;

      if File = null then
         Errno := Error_Bad_File;
         return Unsigned_64'Last;
      end if;
      if not VFS.File.Stat (File, Stat_Val) then
         Errno := Error_Invalid_Seek;
         return Unsigned_64'Last;
      end if;
      case Whence is
         when SEEK_SET =>
            File.Index := Offset;
         when SEEK_CURRENT =>
            File.Index := File.Index + Offset;
         when SEEK_END =>
            File.Index := Stat_Val.Byte_Size + Offset;
         when others =>
            Errno := Error_Invalid_Value;
            return Unsigned_64'Last;
      end case;

      Errno := Error_No_Error;
      return File.Index;
   end Syscall_Seek;

   function Get_Mmap_Prot (P : Unsigned_64) return Arch.MMU.Page_Permissions is
   begin
      return (
         User_Accesible => True,
         Read_Only      => (P and Protection_Write)    = 0,
         Executable     => (P and Protection_Execute) /= 0,
         Global         => False,
         Write_Through  => False
      );
   end Get_Mmap_Prot;

   function Syscall_Mmap
      (Hint       : Unsigned_64;
       Length     : Unsigned_64;
       Protection : Unsigned_64;
       Flags      : Unsigned_64;
       File_D     : Unsigned_64;
       Offset     : Unsigned_64;
       Errno      : out Errno_Value) return Unsigned_64
   is
      Map_Flags : constant Arch.MMU.Page_Permissions :=
         Get_Mmap_Prot (Protection);
      Proc : constant Process_Data_Acc := Arch.Local.Get_Current_Process;
      Map  : constant Page_Map_Acc     := Proc.Common_Map;
      Final_Hint : Unsigned_64 := Hint;
   begin
      if Is_Tracing then
         Lib.Messages.Put ("syscall mmap(");
         Lib.Messages.Put (Hint, False, True);
         Lib.Messages.Put (", ");
         Lib.Messages.Put (Length, False, True);
         Lib.Messages.Put (", ");
         Lib.Messages.Put (Protection, False, True);
         Lib.Messages.Put (", ");
         Lib.Messages.Put (Flags, False, True);
         Lib.Messages.Put (", ");
         Lib.Messages.Put (File_D);
         Lib.Messages.Put (", ");
         Lib.Messages.Put (Offset, False, True);
         Lib.Messages.Put_Line (")");
      end if;

      --  Check that we got a length.
      if Length = 0 then
         Errno := Error_Invalid_Value;
         return Unsigned_64'Last;
      end if;

      --  Check for our own hint if none was provided.
      if Hint = 0 then
         if (Flags and Map_Fixed) /= 0 then
            Errno := Error_Invalid_Value;
            return Unsigned_64'Last;
         else
            Final_Hint      := Proc.Alloc_Base;
            Proc.Alloc_Base := Proc.Alloc_Base + Length;
         end if;
      end if;

      --  Do mmap anon or pass it to the VFS.
      if (Flags and Map_Anon) /= 0 then
         if not Memory.Virtual.Map_Range (
            Map,
            Virtual_Address (Final_Hint),
            Memory.Physical.Alloc (Interfaces.C.size_t (Length)) -
                                   Memory_Offset,
            Length,
            Map_Flags
         )
         then
            --  I dont really know what to return in this case.
            Errno := Error_Invalid_Value;
            return Unsigned_64'Last;
         else
            Errno := Error_No_Error;
            return Final_Hint;
         end if;
      else
         declare
            File : constant File_Acc   := Get_File (Proc, File_D);
            Did_Map : constant Boolean := VFS.File.Mmap (
               F           => File,
               Address     => Virtual_Address (Final_Hint),
               Length      => Length,
               Map_Read    => True,
               Map_Write   => not Map_Flags.Read_Only,
               Map_Execute => Map_Flags.Executable
            );
         begin
            if Did_Map then
               Errno := Error_No_Error;
               return Final_Hint;
            else
               Errno := Error_Bad_File;
               return Unsigned_64'Last;
            end if;
         end;
      end if;
   end Syscall_Mmap;

   function Syscall_Munmap
      (Address    : Unsigned_64;
       Length     : Unsigned_64;
       Errno      : out Errno_Value) return Unsigned_64
   is
      Proc : constant Process_Data_Acc := Arch.Local.Get_Current_Process;
      Map : constant Memory.Virtual.Page_Map_Acc := Proc.Common_Map;
      Addr : constant Physical_Address :=
         Memory.Virtual.Virtual_To_Physical (Map, Virtual_Address (Address));
   begin
      if Is_Tracing then
         Lib.Messages.Put ("syscall munmap(");
         Lib.Messages.Put (Address, False, True);
         Lib.Messages.Put (", ");
         Lib.Messages.Put (Length, False, True);
         Lib.Messages.Put_Line (")");
      end if;

      if MAC_Is_Locked and not Proc.Perms.Caps.Can_Deallocate_Memory then
         Errno := Error_Bad_Access;
         Execute_MAC_Failure ("munmap", Proc);
         return Unsigned_64'Last;
      end if;

      --  We only support MAP_ANON and MAP_FIXED, so we can just assume we want
      --  to free.
      --  TODO: Actually unmap, not only free.
      Memory.Physical.Free (Interfaces.C.size_t (Addr));
      Errno := Error_No_Error;
      return 0;
   end Syscall_Munmap;

   function Syscall_Get_PID return Unsigned_64 is
      Current_Process : constant Userland.Process.Process_Data_Acc :=
            Arch.Local.Get_Current_Process;
   begin
      if Is_Tracing then
         Lib.Messages.Put_Line ("syscall getpid()");
      end if;
      return Unsigned_64 (Current_Process.Process_PID);
   end Syscall_Get_PID;

   function Syscall_Get_Parent_PID return Unsigned_64 is
      Current_Process : constant Userland.Process.Process_Data_Acc :=
            Arch.Local.Get_Current_Process;
      Parent_Process : constant Natural := Current_Process.Parent_PID;
   begin
      if Is_Tracing then
         Lib.Messages.Put_Line ("syscall getppid()");
      end if;
      return Unsigned_64 (Parent_Process);
   end Syscall_Get_Parent_PID;

   function Syscall_Exec
      (Address : Unsigned_64;
       Argv    : Unsigned_64;
       Envp    : Unsigned_64;
       Errno   : out Errno_Value) return Unsigned_64
   is
      --  FIXME: This type should be dynamic ideally and not have a maximum.
      type Arg_Arr is array (1 .. 40) of Unsigned_64;

      Current_Thread  : constant Scheduler.TID :=
         Arch.Local.Get_Current_Thread;
      Current_Process : constant Userland.Process.Process_Data_Acc :=
         Arch.Local.Get_Current_Process;

      Addr : constant System.Address := To_Address (Integer_Address (Address));
      Path_Length : constant Natural := Lib.C_String_Length (Addr);
      Path_String : String (1 .. Path_Length) with Address => Addr;
      Opened_File : constant File_Acc := Open (Path_String, Access_R);

      Args_Raw : Arg_Arr with Address => To_Address (Integer_Address (Argv));
      Env_Raw  : Arg_Arr with Address => To_Address (Integer_Address (Envp));
      Args_Count : Natural := 0;
      Env_Count  : Natural := 0;
   begin
      if Is_Tracing then
         Lib.Messages.Put ("syscall exec(" & Path_String & ")");
      end if;

      if Opened_File = null then
         Errno := Error_No_Entity;
         return Unsigned_64'Last;
      end if;

      if not Check_Userland_Access (To_Integer (Addr)) then
         Errno := Error_Would_Fault;
         return Unsigned_64'Last;
      end if;

      --  Count the args and envp we have, and copy them to Ada arrays.
      for I in Args_Raw'Range loop
         exit when Args_Raw (I) = 0;
         Args_Count := Args_Count + 1;
      end loop;
      for I in Env_Raw'Range loop
         exit when Env_Raw (I) = 0;
         Env_Count := Env_Count + 1;
      end loop;

      declare
         Args : Userland.Argument_Arr    (1 .. Args_Count);
         Env  : Userland.Environment_Arr (1 .. Env_Count);
      begin
         for I in 1 .. Args_Count loop
            declare
               Addr : constant System.Address :=
                  To_Address (Integer_Address (Args_Raw (I)));
               Arg_Length : constant Natural := Lib.C_String_Length (Addr);
               Arg_String : String (1 .. Arg_Length) with Address => Addr;
            begin
               Args (I) := new String'(Arg_String);
            end;
         end loop;
         for I in 1 .. Env_Count loop
            declare
               Addr : constant System.Address :=
                  To_Address (Integer_Address (Env_Raw (I)));
               Arg_Length : constant Natural := Lib.C_String_Length (Addr);
               Arg_String : String (1 .. Arg_Length) with Address => Addr;
            begin
               Env (I) := new String'(Arg_String);
            end;
         end loop;

         --  Free state.
         Userland.Process.Flush_Threads (Current_Process);
         Userland.Process.Flush_Exec_Files (Current_Process);

         --  Create a new map for the process.
         Memory.Virtual.Delete_Map (Current_Process.Common_Map);
         Current_Process.Common_Map := Memory.Virtual.New_Map;

         --  Start the actual program.
         if not Userland.Loader.Start_Program
            (Opened_File, Args, Env, Current_Process)
         then
            Errno := Error_Bad_Access;
            return Unsigned_64'Last;
         end if;

         for Arg of Args loop
            Free_Str (Arg);
         end loop;
         for En of Env loop
            Free_Str (En);
         end loop;

         Userland.Process.Remove_Thread (Current_Process, Current_Thread);
         Scheduler.Bail;
         Errno := Error_No_Error;
         return 0;
      end;
   end Syscall_Exec;

   function Syscall_Fork
      (State_To_Fork : Arch.Context.GP_Context_Acc;
       Errno         : out Errno_Value) return Unsigned_64
   is
      Parent : constant Process_Data_Acc := Arch.Local.Get_Current_Process;
      Child  : Process_Data_Acc;
   begin
      if Is_Tracing then
         Lib.Messages.Put_Line ("syscall fork()");
      end if;

      if MAC_Is_Locked and not Parent.Perms.Caps.Can_Create_Others then
         Errno := Error_Bad_Access;
         Execute_MAC_Failure ("fork", Parent);
         return Unsigned_64'Last;
      end if;

      Child := Create_Process (Parent);
      if Child = null then
         Errno := Error_Would_Block;
         return Unsigned_64'Last;
      end if;

      --  Fork the child state.
      Child.Common_Map := Memory.Virtual.Fork_Map (Parent.Common_Map);
      for I in Parent.File_Table'Range loop
         Child.File_Table (I) := (
            Close_On_Exec => Parent.File_Table (I).Close_On_Exec,
            Inner         => Duplicate (Parent.File_Table (I).Inner)
         );
      end loop;

      --  Create a running thread cloning the caller.
      if not Add_Thread (Child,
         Scheduler.Create_User_Thread
            (State_To_Fork, Child.Common_Map, Child.Process_PID))
      then
         Errno := Error_Would_Block;
         return Unsigned_64'Last;
      end if;

      Errno := Error_No_Error;
      return Unsigned_64 (Child.Process_PID);
   end Syscall_Fork;

   function Syscall_Wait
      (Waited_PID, Exit_Addr, Options : Unsigned_64;
       Errno                          : out Errno_Value) return Unsigned_64
   is
      --  TODO: Support things like WCONTINUE once signals work.

      Current_Process : constant Userland.Process.Process_Data_Acc :=
            Arch.Local.Get_Current_Process;
      Exit_Value : Unsigned_32
         with Address => To_Address (Integer_Address (Exit_Addr)), Import;

      Dont_Hang : constant Boolean := (Options and Wait_WNOHANG) /= 0;
   begin
      if Is_Tracing then
         Lib.Messages.Put      ("syscall wait(");
         Lib.Messages.Put      (Waited_PID);
         Lib.Messages.Put      (", ");
         Lib.Messages.Put      (Exit_Addr, False, True);
         Lib.Messages.Put      (", ");
         Lib.Messages.Put      (Options, False, True);
         Lib.Messages.Put_Line (")");
      end if;

      --  Fail on having to wait on the process group, we dont support that.
      if Waited_PID = 0 then
         Errno := Error_Invalid_Value;
         return Unsigned_64'Last;
      end if;

      if not Check_Userland_Access (Integer_Address (Exit_Addr)) then
         Errno := Error_Would_Fault;
         return Unsigned_64'Last;
      end if;

      --  If -1, we have to wait for any of the children.
      --  TODO: Do not hardcode this to the first child.
      if Waited_PID = Unsigned_64 (Unsigned_32'Last) then
         return Syscall_Wait
            (Unsigned_64 (Current_Process.Children (1)),
             Exit_Addr, Options, Errno);
      end if;

      --  Check the callee is actually the parent, else we are doing something
      --  weird.
      for PID_Item of Current_Process.Children loop
         if Natural (Waited_PID) = PID_Item then
            goto Is_Parent;
         end if;
      end loop;

      Errno := Error_Child;
      return Unsigned_64'Last;

   <<Is_Parent>>
      declare
         Waited_Process : constant Userland.Process.Process_Data_Acc :=
            Userland.Process.Get_By_PID (Natural (Waited_PID));
      begin
         --  Actually wait if we are to.
         if Dont_Hang and then not Waited_Process.Did_Exit then
            Errno := Error_No_Error;
            return 0;
         else
            while not Waited_Process.Did_Exit loop
               Scheduler.Yield;
            end loop;
         end if;

         --  Set the return value if we are to.
         if Exit_Value'Address /= System.Null_Address then
            Exit_Value := Unsigned_32 (Waited_Process.Exit_Code);
         end if;

         --  Now that we got the exit code, finally allow the process to die.
         --  TODO: Deleting the map here deadlocks down the road, check why.
         Memory.Virtual.Delete_Map       (Waited_Process.Common_Map);
         Userland.Process.Delete_Process (Waited_Process);
         Errno := Error_No_Error;
         return Waited_PID;
      end;
   end Syscall_Wait;

   function Syscall_Uname
      (Address : Unsigned_64;
       Errno   : out Errno_Value) return Unsigned_64
   is
      Addr : constant System.Address := To_Address (Integer_Address (Address));
      UTS  : UTS_Name with Address => Addr;
      Host_Len : Networking.Hostname_Len;
   begin
      if Is_Tracing then
         Lib.Messages.Put      ("syscall uname(");
         Lib.Messages.Put      (Address);
         Lib.Messages.Put_Line (")");
      end if;

      if not Check_Userland_Access (Integer_Address (Address)) then
         Errno := Error_Would_Fault;
         return Unsigned_64'Last;
      end if;

      Networking.Get_Hostname (UTS.Node_Name, Host_Len);
      UTS.Node_Name (Host_Len + 1) := Ada.Characters.Latin_1.NUL;

      UTS.System_Name (1 .. Config.Name'Length + 1) :=
         Config.Name & Ada.Characters.Latin_1.NUL;
      UTS.Release (1 .. Config.Version'Length + 1) :=
         Config.Version & Ada.Characters.Latin_1.NUL;
      UTS.Version (1 .. Config.Version'Length + 1) :=
         Config.Version & Ada.Characters.Latin_1.NUL;
      UTS.Machine (1 .. Config.Arch_Name'Length + 1) :=
         Config.Arch_Name & Ada.Characters.Latin_1.NUL;

      Errno := Error_No_Error;
      return 0;
   end Syscall_Uname;

   function Syscall_Set_Hostname
      (Address : Unsigned_64;
       Length  : Unsigned_64;
       Errno   : out Errno_Value) return Unsigned_64
   is
      Proc : constant Process_Data_Acc := Arch.Local.Get_Current_Process;
      Len  : constant Natural := Natural (Length);
      Addr : constant System.Address := To_Address (Integer_Address (Address));
      Name : String (1 .. Len) with Address => Addr;
      Success : Boolean;
   begin
      if Is_Tracing then
         Lib.Messages.Put      ("syscall set_hostname(");
         Lib.Messages.Put      (Address);
         Lib.Messages.Put      (", ");
         Lib.Messages.Put      (Length);
         Lib.Messages.Put_Line (")");
      end if;

      if MAC_Is_Locked and not Proc.Perms.Caps.Can_Manage_Networking then
         Errno := Error_Bad_Access;
         Execute_MAC_Failure ("set_hostname", Proc);
         return Unsigned_64'Last;
      end if;

      if not Check_Userland_Access (Integer_Address (Address)) then
         Errno := Error_Would_Fault;
         return Unsigned_64'Last;
      end if;

      Networking.Set_Hostname (Name, Success);

      if not Success then
         Errno := Error_Invalid_Value;
         return Unsigned_64'Last;
      else
         Errno := Error_No_Error;
         return 0;
      end if;
   end Syscall_Set_Hostname;

   function Inner_Stat
      (F       : VFS.File.File_Acc;
       Address : Unsigned_64) return Boolean
   is
      Stat_Val : VFS.File_Stat;
      Stat_Buf : Stat with Address => To_Address (Integer_Address (Address));
   begin
      if VFS.File.Stat (F, Stat_Val) then
         Stat_Buf := (
            Device_Number => F.Dev_Data.Stat.Unique_Identifier,
            Inode_Number  => Stat_Val.Unique_Identifier,
            Mode          => Stat_Val.Mode,
            Number_Links  => Unsigned_32 (Stat_Val.Hard_Link_Count),
            UID           => 0,
            GID           => 0,
            Inner_Device  => F.Dev_Data.Stat.Unique_Identifier,
            File_Size     => Stat_Val.Byte_Size,
            Access_Time   => (Seconds => 0, Nanoseconds => 0),
            Modify_Time   => (Seconds => 0, Nanoseconds => 0),
            Create_Time   => (Seconds => 0, Nanoseconds => 0),
            Block_Size    => Unsigned_64 (Stat_Val.IO_Block_Size),
            Block_Count   => Stat_Val.IO_Block_Count
         );

         --  Set the access part of mode.
         case Stat_Val.Type_Of_File is
            when VFS.File_Regular =>
               Stat_Buf.Mode := Stat_Buf.Mode or Stat_IFREG;
            when VFS.File_Directory =>
               Stat_Buf.Mode := Stat_Buf.Mode or Stat_IFDIR;
            when VFS.File_Symbolic_Link =>
               Stat_Buf.Mode := Stat_Buf.Mode or Stat_IFLNK;
            when VFS.File_Character_Device =>
               Stat_Buf.Mode := Stat_Buf.Mode or Stat_IFCHR;
            when VFS.File_Block_Device =>
               Stat_Buf.Mode := Stat_Buf.Mode or Stat_IFBLK;
         end case;

         return True;
      else
         return False;
      end if;
   end Inner_Stat;

   function Syscall_FStat
      (File_D  : Unsigned_64;
       Address : Unsigned_64;
       Errno   : out Errno_Value) return Unsigned_64
   is
      Proc : constant Userland.Process.Process_Data_Acc :=
         Arch.Local.Get_Current_Process;
      File : constant VFS.File.File_Acc :=
         Userland.Process.Get_File (Proc, File_D);
   begin
      if Is_Tracing then
         Lib.Messages.Put ("syscall fstat(");
         Lib.Messages.Put (File_D);
         Lib.Messages.Put (", ");
         Lib.Messages.Put (Address, False, True);
         Lib.Messages.Put_Line (")");
      end if;

      if File = null then
         Errno := Error_Bad_File;
         return Unsigned_64'Last;
      elsif not Check_Userland_Access (Integer_Address (Address)) then
         Errno := Error_Would_Fault;
         return Unsigned_64'Last;
      end if;

      if Inner_Stat (File, Address) then
         Errno := Error_No_Error;
         return 0;
      else
         Errno := Error_Bad_File;
         return Unsigned_64'Last;
      end if;
   end Syscall_FStat;

   function Syscall_LStat
      (Path    : Unsigned_64;
       Address : Unsigned_64;
       Errno   : out Errno_Value) return Unsigned_64
   is
      Addr : constant System.Address := To_Address (Integer_Address (Path));
      Path_Length  : constant Natural := Lib.C_String_Length (Addr);
      Path_String  : String (1 .. Path_Length) with Address => Addr;
      File : constant VFS.File.File_Acc :=
         VFS.File.Open (Path_String, VFS.File.Access_R);
   begin
      if Is_Tracing then
         Lib.Messages.Put ("syscall lstat(");
         Lib.Messages.Put (Path_String);
         Lib.Messages.Put (", ");
         Lib.Messages.Put (Address, False, True);
         Lib.Messages.Put_Line (")");
      end if;

      if not Check_Userland_Access (Integer_Address (Address)) then
         Errno := Error_Would_Fault;
         return 0;
      end if;

      if Inner_Stat (File, Address) then
         Errno := Error_No_Error;
         return 0;
      else
         Errno := Error_Bad_File;
         return Unsigned_64'Last;
      end if;
   end Syscall_LStat;

   function Syscall_Get_CWD
      (Buffer : Unsigned_64;
       Length : Unsigned_64;
       Errno  : out Errno_Value) return Unsigned_64
   is
      Addr : constant System.Address := To_Address (Integer_Address (Buffer));
      Len  : constant Natural := Natural (Length);
      Path : String (1 .. Len) with Address => Addr;

      Process : constant Userland.Process.Process_Data_Acc :=
            Arch.Local.Get_Current_Process;
   begin
      if Is_Tracing then
         Lib.Messages.Put ("syscall getcwd(");
         Lib.Messages.Put (Buffer, False, True);
         Lib.Messages.Put (", ");
         Lib.Messages.Put (Length);
         Lib.Messages.Put_Line (")");
      end if;

      if not Check_Userland_Access (Integer_Address (Buffer)) then
         Errno := Error_Would_Fault;
         return 0;
      end if;
      if Len = 0 then
         Errno := Error_Invalid_Value;
         return 0;
      end if;
      if Len < Process.Current_Dir_Len then
         Errno := Error_Not_Big_Enough;
         return 0;
      end if;

      Path (1 .. Process.Current_Dir_Len) :=
         Process.Current_Dir (1 .. Process.Current_Dir_Len);
      Errno := Error_No_Error;
      return Buffer;
   end Syscall_Get_CWD;

   function Syscall_Chdir
      (Path  : Unsigned_64;
       Errno : out Errno_Value) return Unsigned_64
   is
      Addr    : constant System.Address := To_Address (Integer_Address (Path));
      Process : constant Userland.Process.Process_Data_Acc :=
            Arch.Local.Get_Current_Process;
   begin
      if not Check_Userland_Access (Integer_Address (Path)) then
         if Is_Tracing then
            Lib.Messages.Put_Line ("syscall chdir(BAD_MEM)");
         end if;
         Errno := Error_Would_Fault;
         return Unsigned_64'Last;
      end if;

      declare
         Path_Length : constant Natural := Lib.C_String_Length (Addr);
         Path_String : String (1 .. Path_Length) with Address => Addr;
      begin
         if Is_Tracing then
            Lib.Messages.Put ("syscall chdir(");
            Lib.Messages.Put (Path_String);
            Lib.Messages.Put_Line (")");
         end if;

         if Path_Length > Process.Current_Dir'Length then
            Errno := Error_String_Too_Long;
            return Unsigned_64'Last;
         end if;

         Process.Current_Dir_Len := Path_Length;
         Process.Current_Dir (1 .. Path_Length) := Path_String;
         Errno := Error_No_Error;
         return 0;
      end;
   end Syscall_Chdir;

   function Syscall_IOCTL
      (FD       : Unsigned_64;
       Request  : Unsigned_64;
       Argument : Unsigned_64;
       Errno    : out Errno_Value) return Unsigned_64
   is
      Arg : constant System.Address := To_Address (Integer_Address (Argument));
      Current_Process : constant Userland.Process.Process_Data_Acc :=
         Arch.Local.Get_Current_Process;
      File : constant VFS.File.File_Acc :=
         Userland.Process.Get_File (Current_Process, FD);
   begin
      if Is_Tracing then
         Lib.Messages.Put      ("syscall ioctl(");
         Lib.Messages.Put      (FD);
         Lib.Messages.Put      (", ");
         Lib.Messages.Put      (Request, False, True);
         Lib.Messages.Put      (", ");
         Lib.Messages.Put      (Argument, False, True);
         Lib.Messages.Put_Line (")");
      end if;

      if File = null then
         Errno := Error_Bad_File;
         return Unsigned_64'Last;
      elsif not Check_Userland_Access (Integer_Address (Argument)) then
         Errno := Error_Would_Fault;
         return Unsigned_64'Last;
      end if;

      if VFS.File.IO_Control (File, Request, Arg) then
         Errno := Error_No_Error;
         return 0;
      else
         Errno := Error_Not_A_TTY;
         return Unsigned_64'Last;
      end if;
   end Syscall_IOCTL;

   function Syscall_Sched_Yield (Errno : out Errno_Value) return Unsigned_64 is
   begin
      if Is_Tracing then
         Lib.Messages.Put_Line ("syscall sched_yield()");
      end if;

      Scheduler.Yield;
      Errno := Error_No_Error;
      return 0;
   end Syscall_Sched_Yield;

   function Syscall_Set_Deadlines
      (Run_Time, Period : Unsigned_64;
       Errno : out Errno_Value) return Unsigned_64
   is
      Proc : constant Process_Data_Acc := Arch.Local.Get_Current_Process;
      Current_TID : constant Scheduler.TID := Arch.Local.Get_Current_Thread;
   begin
      if Is_Tracing then
         Lib.Messages.Put ("syscall set_deadlines(");
         Lib.Messages.Put (Run_Time);
         Lib.Messages.Put (", ");
         Lib.Messages.Put (Period);
         Lib.Messages.Put_Line ("");
      end if;

      if MAC_Is_Locked and not Proc.Perms.Caps.Can_Change_Scheduling then
         Errno := Error_Bad_Access;
         Execute_MAC_Failure ("setdeadlines", Proc);
         return Unsigned_64'Last;
      elsif not Scheduler.Set_Deadlines
         (Current_TID, Positive (Run_Time), Positive (Period))
      then
         Errno := Error_Invalid_Value;
         return Unsigned_64'Last;
      else
         Errno := Error_No_Error;
         return 0;
      end if;
   end Syscall_Set_Deadlines;

   function Syscall_Dup
      (Old_FD : Unsigned_64;
       Errno  : out Errno_Value) return Unsigned_64
   is
      Process : constant Userland.Process.Process_Data_Acc :=
         Arch.Local.Get_Current_Process;
      Old_File  : constant VFS.File.File_Acc :=
         Userland.Process.Get_File (Process, Old_FD);
      New_FD    : VFS.File.File_Acc;
      Result_FD : Natural;
   begin
      if Is_Tracing then
         Lib.Messages.Put      ("syscall dup(");
         Lib.Messages.Put      (Old_FD);
         Lib.Messages.Put_Line (")");
      end if;

      if Old_File = null then
         Errno := Error_Bad_File;
         return Unsigned_64'Last;
      end if;

      New_FD := VFS.File.Duplicate (Old_File);
      if New_FD = null then
         Errno := Error_Bad_File;
         return Unsigned_64'Last;
      elsif not Userland.Process.Add_File (Process, New_FD, Result_FD) then
         Errno := Error_Too_Many_Files;
         return Unsigned_64'Last;
      else
         Errno := Error_No_Error;
         return Unsigned_64 (Result_FD);
      end if;
   end Syscall_Dup;

   function Syscall_Dup2
      (Old_FD, New_FD : Unsigned_64;
       Errno          : out Errno_Value) return Unsigned_64
   is
      Process : constant Userland.Process.Process_Data_Acc :=
         Arch.Local.Get_Current_Process;
      Old_File : constant VFS.File.File_Acc :=
         Userland.Process.Get_File (Process, Old_FD);
      New_File : VFS.File.File_Acc;
   begin
      if Is_Tracing then
         Lib.Messages.Put      ("syscall dup2(");
         Lib.Messages.Put      (Old_FD);
         Lib.Messages.Put      (", ");
         Lib.Messages.Put      (New_FD);
         Lib.Messages.Put_Line (")");
      end if;

      if Old_File = null then
         Errno := Error_Bad_File;
         return Unsigned_64'Last;
      end if;

      New_File := VFS.File.Duplicate (Old_File);
      if New_File = null or else
         not Userland.Process.Replace_File
            (Process, New_File, Natural (New_FD))
      then
         Errno := Error_Bad_File;
         return Unsigned_64'Last;
      else
         Errno := Error_No_Error;
         return New_FD;
      end if;
   end Syscall_Dup2;

   function Syscall_Dup3
      (Old_FD, New_FD : Unsigned_64;
       Flags          : Unsigned_64;
       Errno          : out Errno_Value) return Unsigned_64
   is
      Process : constant Userland.Process.Process_Data_Acc :=
         Arch.Local.Get_Current_Process;
      Old_File  : constant VFS.File.File_Acc :=
         Userland.Process.Get_File (Process, Old_FD);
      New_File : VFS.File.File_Acc;
      Close_On_Exec : constant Boolean := (Flags and O_CLOEXEC) /= 0;
   begin
      if Is_Tracing then
         Lib.Messages.Put      ("syscall dup3(");
         Lib.Messages.Put      (Old_FD);
         Lib.Messages.Put      (", ");
         Lib.Messages.Put      (New_FD);
         Lib.Messages.Put      (", ");
         Lib.Messages.Put      (Flags);
         Lib.Messages.Put_Line (")");
      end if;

      if Old_FD = New_FD or Flags /= 0 or Flags /= O_CLOEXEC then
         Errno := Error_Invalid_Value;
         return Unsigned_64'Last;
      elsif Old_File = null then
         Errno := Error_Bad_File;
         return Unsigned_64'Last;
      end if;

      New_File := VFS.File.Duplicate (Old_File);
      if New_File = null or else
         not Userland.Process.Replace_File
            (Process, New_File, Natural (New_FD), Close_On_Exec)
      then
         Errno := Error_Bad_File;
         return Unsigned_64'Last;
      else
         Errno := Error_No_Error;
         return New_FD;
      end if;
   end Syscall_Dup3;

   function Syscall_Access
      (Path, Mode : Unsigned_64;
       Errno      : out Errno_Value) return Unsigned_64
   is
      Addr : constant System.Address := To_Address (Integer_Address (Path));
   begin
      if not Check_Userland_Access (Integer_Address (Path)) then
         if Is_Tracing then
            Lib.Messages.Put ("syscall access(BAD_MEM, ");
            Lib.Messages.Put (Mode);
            Lib.Messages.Put_Line (")");
         end if;
         Errno := Error_Would_Fault;
         return Unsigned_64'Last;
      end if;
      if Mode = 0 then
         if Is_Tracing then
            Lib.Messages.Put_Line ("syscall access(..., 0)");
         end if;
         Errno := Error_Invalid_Value;
         return Unsigned_64'Last;
      end if;
      declare
         Path_Length : constant Natural := Lib.C_String_Length (Addr);
         Path_String : String (1 .. Path_Length) with Address => Addr;
      begin
         if Is_Tracing then
            Lib.Messages.Put ("syscall access(");
            Lib.Messages.Put (Path_String);
            Lib.Messages.Put (", ");
            Lib.Messages.Put (Mode, False, True);
            Lib.Messages.Put_Line (")");
         end if;

         if VFS.File.Check_Permissions (
               Path      => Path_String,
               Exists    => (Mode and Access_Exists)    /= 0,
               Can_Read  => (Mode and Access_Can_Read)  /= 0,
               Can_Write => (Mode and Access_Can_Write) /= 0,
               Can_Exec  => (Mode and Access_Can_Exec)  /= 0
         )
         then
            Errno := Error_No_Error;
            return 0;
         else
            Errno := Error_No_Entity;
            return Unsigned_64'Last;
         end if;
      end;
   end Syscall_Access;

   function Syscall_Get_Thread_Sched
      (Errno : out Errno_Value) return Unsigned_64
   is
      Ret  : Unsigned_64            := 0;
      Curr : constant Scheduler.TID := Arch.Local.Get_Current_Thread;
   begin
      if Is_Tracing then
         Lib.Messages.Put_Line ("syscall get_thread_sched()");
      end if;

      if Scheduler.Is_Mono_Thread (Curr) then
         Ret := Ret or Thread_MONO;
      end if;

      Errno := Error_No_Error;
      return Ret;
   end Syscall_Get_Thread_Sched;

   function Syscall_Set_Thread_Sched
      (Flags : Unsigned_64;
       Errno : out Errno_Value) return Unsigned_64
   is
      Proc : constant Process_Data_Acc := Arch.Local.Get_Current_Process;
      Curr : constant Scheduler.TID    := Arch.Local.Get_Current_Thread;
   begin
      if Is_Tracing then
         Lib.Messages.Put      ("syscall set_thread_sched(");
         Lib.Messages.Put      (Flags);
         Lib.Messages.Put_Line (")");
      end if;

      if MAC_Is_Locked and not Proc.Perms.Caps.Can_Change_Scheduling then
         Errno := Error_Bad_Access;
         Execute_MAC_Failure ("set_thread_sched", Proc);
         return Unsigned_64'Last;
      end if;

      Scheduler.Set_Mono_Thread (Curr, (Flags and Thread_MONO) /= 0);
      Errno := Error_No_Error;
      return 0;
   end Syscall_Set_Thread_Sched;

   function Syscall_Fcntl
      (FD       : Unsigned_64;
       Command  : Unsigned_64;
       Argument : Unsigned_64;
       Errno    : out Errno_Value) return Unsigned_64
   is
      Current_Process : constant Userland.Process.Process_Data_Acc :=
         Arch.Local.Get_Current_Process;
      Returned : Unsigned_64 := 0;
   begin
      if Is_Tracing then
         Lib.Messages.Put      ("syscall fcntl(");
         Lib.Messages.Put      (FD);
         Lib.Messages.Put      (", ");
         Lib.Messages.Put      (Command);
         Lib.Messages.Put      (", ");
         Lib.Messages.Put      (Argument);
         Lib.Messages.Put_Line (")");
      end if;

      if not Userland.Process.Is_Valid_File (Current_Process, FD) then
         Errno := Error_Bad_File;
         return Unsigned_64'Last;
      end if;

      case Command is
         --  Set and get the file descriptor flags, so far only FD_CLOEXEC is
         --  supported.
         when F_GETFD =>
            if Current_Process.File_Table (Natural (FD)).Close_On_Exec then
               Returned := FD_CLOEXEC;
            end if;
         when F_SETFD =>
            if (Argument and FD_CLOEXEC) /= 0 then
               Current_Process.File_Table (Natural (FD)).Close_On_Exec := True;
            end if;
         when others =>
            Errno := Error_Invalid_Value;
            return Unsigned_64'Last;
      end case;

      Errno := Error_No_Error;
      return Returned;
   end Syscall_Fcntl;

   function Syscall_Spawn
      (Address : Unsigned_64;
       Argv    : Unsigned_64;
       Envp    : Unsigned_64;
       Errno   : out Errno_Value) return Unsigned_64
   is
      --  FIXME: This type should be dynamic ideally and not have a maximum.
      type Arg_Arr is array (1 .. 40) of Unsigned_64;

      Current_Process : constant Userland.Process.Process_Data_Acc :=
         Arch.Local.Get_Current_Process;
      Child : constant Process_Data_Acc := Create_Process (Current_Process);

      Addr : constant System.Address := To_Address (Integer_Address (Address));
      Path_Length : constant Natural := Lib.C_String_Length (Addr);
      Path_String : String (1 .. Path_Length) with Address => Addr;
      Opened_File : constant File_Acc := Open (Path_String, Access_R);
      File_Perms  : MAC.Filter_Permissions;

      Args_Raw : Arg_Arr with Address => To_Address (Integer_Address (Argv));
      Env_Raw  : Arg_Arr with Address => To_Address (Integer_Address (Envp));
      Args_Count : Natural := 0;
      Env_Count  : Natural := 0;
   begin
      if Is_Tracing then
         Lib.Messages.Put_Line ("syscall spawn(" & Path_String & ")");
      end if;

      if MAC_Is_Locked then
         File_Perms := MAC.Check_Path_Permissions
            (Path_String, Current_Process.Perms.Filters);
         if not Current_Process.Perms.Caps.Can_Create_Others or
            not File_Perms.Can_Execute
         then
            Errno := Error_Bad_Access;
            Execute_MAC_Failure ("spawn", Current_Process);
            return Unsigned_64'Last;
         end if;
      end if;

      if Opened_File = null then
         Errno := Error_No_Entity;
         return Unsigned_64'Last;
      end if;

      if Child = null then
         Errno := Error_Would_Block;
         return Unsigned_64'Last;
      end if;

      --  Count the args and envp we have.
      for I in Args_Raw'Range loop
         exit when Args_Raw (I) = 0;
         Args_Count := Args_Count + 1;
      end loop;
      for I in Env_Raw'Range loop
         exit when Env_Raw (I) = 0;
         Env_Count := Env_Count + 1;
      end loop;

      --  Copy the argv and envp to Ada arrays and boot the process.
      declare
         Args  : Userland.Argument_Arr    (1 .. Args_Count);
         Env   : Userland.Environment_Arr (1 .. Env_Count);
      begin
         for I in 1 .. Args_Count loop
            declare
               Addr : constant System.Address :=
                  To_Address (Integer_Address (Args_Raw (I)));
               Arg_Length : constant Natural := Lib.C_String_Length (Addr);
               Arg_String : String (1 .. Arg_Length) with Address => Addr;
            begin
               Args (I) := new String'(Arg_String);
            end;
         end loop;
         for I in 1 .. Env_Count loop
            declare
               Addr : constant System.Address :=
                  To_Address (Integer_Address (Env_Raw (I)));
               Arg_Length : constant Natural := Lib.C_String_Length (Addr);
               Arg_String : String (1 .. Arg_Length) with Address => Addr;
            begin
               Env (I) := new String'(Arg_String);
            end;
         end loop;

         Child.Common_Map := Memory.Virtual.New_Map;
         if not Loader.Start_Program (Opened_File, Args, Env, Child) then
            Errno := Error_Bad_Access;
            return Unsigned_64'Last;
         end if;
         Child.File_Table (0 .. 2) := (
            (False, Duplicate (Current_Process.File_Table (0).Inner)),
            (False, Duplicate (Current_Process.File_Table (1).Inner)),
            (False, Duplicate (Current_Process.File_Table (2).Inner))
         );

         for Arg of Args loop
            Free_Str (Arg);
         end loop;
         for En of Env loop
            Free_Str (En);
         end loop;

         Errno := Error_No_Error;
         return Unsigned_64 (Child.Process_PID);
      end;
   end Syscall_Spawn;

   function Syscall_Get_Random
     (Address : Unsigned_64;
      Length  : Unsigned_64;
      Errno   : out Errno_Value) return Unsigned_64
   is
      Proc  : constant Process_Data_Acc := Arch.Local.Get_Current_Process;
      Result : Cryptography.Random.Crypto_Data (1 .. Natural (Length / 4))
         with Address => To_Address (Integer_Address (Address)), Import;
   begin
      if Is_Tracing then
         Lib.Messages.Put      ("syscall getrandom(");
         Lib.Messages.Put      (Address, False, True);
         Lib.Messages.Put      (", ");
         Lib.Messages.Put      (Length);
         Lib.Messages.Put_Line (")");
      end if;
      if MAC_Is_Locked and not Proc.Perms.Caps.Can_Access_Entropy then
         Errno := Error_Bad_Access;
         Execute_MAC_Failure ("getrandom", Proc);
         return Unsigned_64'Last;
      elsif not Check_Userland_Access (Integer_Address (Address)) then
         Errno := Error_Would_Fault;
         return Unsigned_64'Last;
      else
         Cryptography.Random.Fill_Data (Result);
         Errno := Error_No_Error;
         return Result'Length * 4;
      end if;
   end Syscall_Get_Random;

   function Syscall_MProtect
     (Address    : Unsigned_64;
      Length     : Unsigned_64;
      Protection : Unsigned_64;
      Errno      : out Errno_Value) return Unsigned_64
   is
      Proc  : constant Process_Data_Acc := Arch.Local.Get_Current_Process;
      Map   : constant Page_Map_Acc     := Proc.Common_Map;
      Flags : constant Arch.MMU.Page_Permissions := Get_Mmap_Prot (Protection);
      Addr  : constant Integer_Address := Integer_Address (Address);
   begin
      if Is_Tracing then
         Lib.Messages.Put      ("syscall mprotect(");
         Lib.Messages.Put      (Address, False, True);
         Lib.Messages.Put      (", ");
         Lib.Messages.Put      (Length);
         Lib.Messages.Put      (", ");
         Lib.Messages.Put      (Protection, False, True);
         Lib.Messages.Put_Line (")");
      end if;

      if MAC_Is_Locked and not Proc.Perms.Caps.Can_Allocate_Memory then
         Errno := Error_Bad_Access;
         Execute_MAC_Failure ("mprotect", Proc);
         return Unsigned_64'Last;
      end if;

      if not Check_Userland_Access (Addr) or else
         not Remap_Range (Map, Addr, Length, Flags)
      then
         Errno := Error_Would_Fault;
         return Unsigned_64'Last;
      else
         Errno := Error_No_Error;
         return 0;
      end if;
   end Syscall_MProtect;

   function Syscall_Crypto_Request
      (Request  : Unsigned_64;
       Argument : Unsigned_64;
       Errno    : out Errno_Value) return Unsigned_64
   is
      Data_Addr : constant Integer_Address := Integer_Address (Argument);
      Data : Crypto_AES_Data with Import, Address => To_Address (Data_Addr);
   begin
      if Is_Tracing then
         Lib.Messages.Put      ("syscall crypto_request(");
         Lib.Messages.Put      (Request, False, True);
         Lib.Messages.Put      (", ");
         Lib.Messages.Put      (Argument, False, True);
         Lib.Messages.Put_Line (")");
      end if;

      if not Check_Userland_Access (Data_Addr) or else
         not Check_Userland_Access (To_Integer (Data.Data))
      then
         Errno := Error_Would_Fault;
         return Unsigned_64'Last;
      end if;

      if not Arch.Snippets.Supports_AES_Accel then
         Errno := Error_Not_Implemented;
         return Unsigned_64'Last;
      end if;

      declare
         AES_Data : Cryptography.AES.AES_Data
            (1 .. Data.Length / (Unsigned_128'Size / 8))
            with Import, Address => Data.Data;
      begin
         case Request is
            when CRYPTO_AES128_ECB_ENCRYPT =>
               Cryptography.AES.Encrypt_ECB (Data.Key, AES_Data);
            when CRYPTO_AES128_ECB_DECRYPT =>
               Cryptography.AES.Decrypt_ECB (Data.Key, AES_Data);
            when CRYPTO_AES128_CBC_ENCRYPT =>
               Cryptography.AES.Encrypt_CBC (Data.Key, Data.IV, AES_Data);
            when CRYPTO_AES128_CBC_DECRYPT =>
               Cryptography.AES.Decrypt_CBC (Data.Key, Data.IV, AES_Data);
            when CRYPTO_AES128_CTR_CRYPT =>
               Cryptography.AES.Crypt_CTR (Data.Key, Data.IV, AES_Data);
            when others =>
               Errno := Error_Not_Implemented;
               return Unsigned_64'Last;
         end case;
      end;
      Errno := Error_No_Error;
      return 0;
   end Syscall_Crypto_Request;

   function Syscall_Set_MAC_Capabilities
      (Bits  : Unsigned_64;
       Errno : out Errno_Value) return Unsigned_64
   is
      P : constant Process_Data_Acc := Arch.Local.Get_Current_Process;
      EI : constant Boolean := (Bits and MAC_EXIT_ITSELF)   /= 0;
      CO : constant Boolean := (Bits and MAC_CREATE_OTHERS) /= 0;
      CS : constant Boolean := (Bits and MAC_CHANGE_SCHED)  /= 0;
      AE : constant Boolean := (Bits and MAC_ACC_ENTROPY)   /= 0;
      AM : constant Boolean := (Bits and MAC_ALLOC_MEM)     /= 0;
      DM : constant Boolean := (Bits and MAC_DEALLOC_MEM)   /= 0;
      MN : constant Boolean := (Bits and MAC_MANAGE_NET)    /= 0;
   begin
      if Is_Tracing then
         Lib.Messages.Put      ("syscall set_mac(");
         Lib.Messages.Put      (Bits, False, True);
         Lib.Messages.Put_Line (")");
      end if;

      if MAC_Is_Locked then
         P.Perms.Caps := (
            Can_Exit_Itself       => P.Perms.Caps.Can_Exit_Itself       and EI,
            Can_Create_Others     => P.Perms.Caps.Can_Create_Others     and CO,
            Can_Change_Scheduling => P.Perms.Caps.Can_Change_Scheduling and CS,
            Can_Access_Entropy    => P.Perms.Caps.Can_Access_Entropy    and AE,
            Can_Allocate_Memory   => P.Perms.Caps.Can_Allocate_Memory   and AM,
            Can_Deallocate_Memory => P.Perms.Caps.Can_Deallocate_Memory and DM,
            Can_Manage_Networking => P.Perms.Caps.Can_Manage_Networking and MN
         );
      else
         P.Perms.Caps := (
            Can_Exit_Itself       => EI,
            Can_Create_Others     => CO,
            Can_Change_Scheduling => CS,
            Can_Access_Entropy    => AE,
            Can_Allocate_Memory   => AM,
            Can_Deallocate_Memory => DM,
            Can_Manage_Networking => MN
         );
      end if;
      Errno := Error_No_Error;
      return 0;
   end Syscall_Set_MAC_Capabilities;

   function Syscall_Lock_MAC (Errno : out Errno_Value) return Unsigned_64 is
      Proc : constant Process_Data_Acc := Arch.Local.Get_Current_Process;
   begin
      if Is_Tracing then
         Lib.Messages.Put_Line ("syscall lock_mac()");
      end if;

      if MAC_Is_Locked then
         Errno := Error_Bad_Access;
         Execute_MAC_Failure ("lock_mac", Proc);
         return Unsigned_64'Last;
      else
         MAC_Is_Locked := True;
         Errno := Error_No_Error;
         return 0;
      end if;
   end Syscall_Lock_MAC;

   function Syscall_Add_MAC_Filter
      (Filter_Addr : Unsigned_64;
       Errno       : out Errno_Value) return Unsigned_64
   is
      Proc   : constant Process_Data_Acc := Arch.Local.Get_Current_Process;
      Addr   : constant Integer_Address := Integer_Address (Filter_Addr);
      Filt   : MAC_Filter with Import, Address => To_Address (Addr);
      Xlated : MAC.Filter;
   begin
      if Is_Tracing then
         Lib.Messages.Put      ("syscall add_mac_filter(");
         Lib.Messages.Put      (Filter_Addr, False, True);
         Lib.Messages.Put_Line (")");
      end if;

      if not Check_Userland_Access (Addr) then
         Errno := Error_Would_Fault;
         return Unsigned_64'Last;
      end if;

      Xlated := (
         Path   => Filt.Path,
         Length => Filt.Length,
         Perms  => (
            Includes_Files       => (Filt.Perms and MAC_FILTER_INC_FILES) /= 0,
            Includes_Directories => (Filt.Perms and MAC_FILTER_INC_DIRS)  /= 0,
            Can_Read             => (Filt.Perms and MAC_FILTER_R)         /= 0,
            Can_Write            => (Filt.Perms and MAC_FILTER_W)         /= 0,
            Can_Execute          => (Filt.Perms and MAC_FILTER_EXEC)      /= 0
         )
      );

      if MAC.Is_Conflicting (Xlated, Proc.Perms.Filters) then
         Errno := Error_Invalid_Value;
         return Unsigned_64'Last;
      else
         for Item of Proc.Perms.Filters loop
            if Item.Length = 0 then
               Item := Xlated;
               goto Func_End;
            end if;
         end loop;
         Errno := Error_Would_Block;
         return Unsigned_64'Last;
      end if;

   <<Func_End>>
      Errno := Error_No_Error;
      return 0;
   end Syscall_Add_MAC_Filter;

   function Set_MAC_Enforcement
      (Action : Unsigned_64;
       Errno  : out Errno_Value) return Unsigned_64
   is
      Proc : constant Process_Data_Acc := Arch.Local.Get_Current_Process;
   begin
      if Is_Tracing then
         Lib.Messages.Put      ("syscall set_mac_enforcement(");
         Lib.Messages.Put      (Action, False, True);
         Lib.Messages.Put_Line (")");
      end if;

      if MAC_Is_Locked then
         Errno := Error_Bad_Access;
         Execute_MAC_Failure ("set_mac_enforcement", Proc);
         return Unsigned_64'Last;
      end if;

      case Action is
         when MAC_DENY            => Proc.Perms.Action := MAC.Deny;
         when MAC_DENY_AND_SCREAM => Proc.Perms.Action := MAC.Deny_And_Scream;
         when MAC_KILL            => Proc.Perms.Action := MAC.Kill;
         when others              => null;
      end case;

      Errno := Error_No_Error;
      return 0;
   end Set_MAC_Enforcement;

   procedure Execute_MAC_Failure (Name : String; Curr_Proc : Process_Data_Acc)
   is
      Discard : Errno_Value;
   begin
      case Curr_Proc.Perms.Action is
         when MAC.Deny =>
            null;
         when MAC.Deny_And_Scream =>
            Lib.Messages.Put      ("PID: ");
            Lib.Messages.Put      (Curr_Proc.Process_PID);
            Lib.Messages.Put_Line (", MAC failure: " & Name);
         when MAC.Kill =>
            --  TODO: Kill and not exit, once we have such a thing.
            --  The semantics of SIGTERM and SIGKILL matter.
         --  https://linuxhandbook.com/content/images/2020/06/dont-sigkill.jpeg
            Do_Exit (Curr_Proc, 42);
      end case;
   end Execute_MAC_Failure;
end Userland.Syscall;
