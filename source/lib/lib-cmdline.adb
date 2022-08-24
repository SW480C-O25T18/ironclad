--  lib-messages.ads: Parsing command line options.
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

package body Lib.Cmdline is
   --  Unit passes GNATprove AoRTE, GNAT does not know this.
   pragma Suppress (All_Checks);

   function Get_Parameter (Cmdline, Key : String) return String_Acc is
      Curr_Index : Integer;
      Last_Index : Integer;
   begin
      for I in 1 .. Cmdline'Length - Key'Length + 1 loop
         Curr_Index := Cmdline'First + (I - 1);
         if Key = Cmdline (Curr_Index .. Curr_Index + (Key'Length - 1)) and
            Curr_Index < Integer'Last - Key'Length - 1
         then
            Last_Index := Curr_Index;
            Curr_Index := Curr_Index + Key'Length + 1;
            for J in Curr_Index .. Cmdline'Last loop
               exit when Cmdline (J) = ' ' or Last_Index = Integer'Last;
               Last_Index := J;
               pragma Loop_Invariant (Last_Index <= Cmdline'Last);
            end loop;
            goto Found_Value;
         end if;
         pragma Loop_Invariant (Curr_Index <= Cmdline'Last);
      end loop;
      return null;

   <<Found_Value>>
      return new String'(Cmdline (Curr_Index .. Last_Index));
   end Get_Parameter;

   function Is_Key_Present (Cmdline, Key : String) return Boolean is
      Curr_Index : Integer;
   begin
      for I in 1 .. Cmdline'Length - Key'Length + 1 loop
         Curr_Index := Cmdline'First + (I - 1);
         if Key = Cmdline (Curr_Index .. Curr_Index + (Key'Length - 1)) then
            return True;
         end if;
      end loop;
      return False;
   end Is_Key_Present;
end Lib.Cmdline;
