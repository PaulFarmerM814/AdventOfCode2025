with Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Text_IO;
with Ada.Text_IO;

package body day3_2025 is

   function Get_Input (S : in Ada.Text_IO.File_Type) return string
   is

      USV : constant Ada.Strings.Unbounded.Unbounded_String :=
        Ada.Strings.Unbounded.Text_IO.Get_Line (S);

      SV : constant String := Ada.Strings.Unbounded.To_String (USV);

   begin

      return SV;

   end Get_Input;

   procedure run is

      Input_File   : Ada.Text_IO.File_Type;

      max_joltage : Long_Long_Integer := 0;
      current_max_joltage : Long_Long_Integer := 0;

   begin

      Ada.Text_IO.Open
        (File => Input_File,
         Mode => Ada.Text_IO.In_File,
         Name => "day3_2025.txt");

      while not Ada.Text_IO.End_Of_File (Input_File) loop

         declare

            current_string : constant string := Get_Input (Input_File);
            current_joltage : Long_Long_Integer := 0;

         begin

            current_max_joltage := 0;

            for x in current_string'Range
            loop
               for y in x+1..current_string'Length
               loop
                  current_joltage := Long_Long_Integer'Value(current_string(x..x) & current_string(y..y));
                  if (current_joltage > current_max_joltage)
                  then
                     current_max_joltage := current_joltage;
                  end if;
               end loop;
            end loop;
         end;

         Ada.Text_IO.Put_Line ("current_max_joltage " & current_max_joltage'Image);
         max_joltage := max_joltage + current_max_joltage;

      end loop;

      Ada.Text_IO.Put_Line ("max_joltage " & max_joltage'Image);

      Ada.Text_IO.Close(Input_File);

   end run;

end day3_2025;
