with Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Text_IO;
with Ada.Text_IO;

package body day1_2025 is

   zero_count : integer := 0;

   type dial_type is mod 100;

   current_dial : dial_type := 50;

   type direction_type is (L, R);

   procedure Get_Input (S              : in     Ada.Text_IO.File_Type;
                        direction      :    out direction_type;
                        rotation       :    out dial_type;
                        through_itself :    out integer)
   is

      USV : constant Ada.Strings.Unbounded.Unbounded_String :=
        Ada.Strings.Unbounded.Text_IO.Get_Line (S);

      SV : constant String := Ada.Strings.Unbounded.To_String (USV);

      temp_dial : integer;

   begin

      direction := direction_type'Value(SV(SV'First..SV'First));

      temp_dial := integer'Value(SV(SV'First+1..SV'Last));

      through_itself := temp_dial / 100;
      temp_dial := temp_dial rem 100;
      rotation := dial_type(temp_dial);

      Ada.Text_IO.Put_Line (Item => direction'Img & " " & rotation'Img);

   end Get_Input;

   procedure run is

      Input_File   : Ada.Text_IO.File_Type;

      direction : direction_type;
      rotation  : dial_type;
      through_itself : integer;

   begin

      Ada.Text_IO.Open
        (File => Input_File,
         Mode => Ada.Text_IO.In_File,
         Name => "day1_2025.txt");

      while not Ada.Text_IO.End_Of_File (Input_File) loop

         Get_Input (Input_File, direction, rotation, through_itself);

         case direction is
            when L =>
               current_dial := current_dial - rotation;
            when R =>
               current_dial := current_dial + rotation;
         end case;

         -- zero_count := zero_count + through_itself;

         if (current_dial = 0)
         then
            zero_count := zero_count + 1;
         end if;

      end loop;

      Ada.Text_IO.Put_Line ("zero_count " & zero_count'Image);

      Ada.Text_IO.Close(Input_File);

   end run;

end day1_2025;
