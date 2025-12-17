with Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Text_IO;
with Ada.Text_IO;

with gnat.Dynamic_Tables;

package body day7_2025 is

   package input_type is new
     gnat.Dynamic_Tables(Table_Component_Type => ada.strings.Unbounded.Unbounded_String,
                         Table_Index_Type     => Positive);


   input : input_type.Instance;

   splitter_count : Long_Long_Integer := 0;

   procedure Get_Input (S : Ada.Text_IO.File_Type)
   is

      USV : Ada.Strings.Unbounded.Unbounded_String;

   begin

      while not Ada.Text_IO.End_Of_File (S) loop

         USV := Ada.Strings.Unbounded.Text_IO.Get_Line (S);

         input.append(USV);

      end loop;

   end Get_Input;

   procedure run is

      Input_File   : Ada.Text_IO.File_Type;

   begin
      Ada.Text_IO.Open
        (File => Input_File,
         Mode => Ada.Text_IO.In_File,
         Name => "day7_2025.txt");

      Get_Input (Input_File);

      declare
         subtype input_str_type is string(1..ada.strings.unbounded.length(input.table(1)));
         type input_array_type is array(1..input.last) of input_str_type;

         input_array : input_array_type;

      begin
         for x in input_type.first..input.last
         loop
            input_array(x) := ada.strings.unbounded.to_string(input.table(x));
         end loop;

         for x in input_array'Range
         loop
            for y in input_array(x)'Range
            loop
               if (input_array(x)(y) = 'S')
               then
                  input_array(x+1)(y) := '|';
               elsif (input_array(x)(y) = '^')
               then
                  if (input_array(x-1)(y) = '|')
                  then
                     if (input_array(x)(y-1) /= '|'
                         or else
                         input_array(x)(y+1) /= '|')
                     then
                        splitter_count := @ + 1;
                     end if;

                     input_array(x)(y-1) := '|';
                     input_array(x)(y+1) := '|';
                     input_array(x+1)(y-1) := '|';
                     input_array(x+1)(y+1) := '|';
                  end if;
               elsif (input_array(x)(y) = '|')
               then
                  null;
               elsif (input_array(x)(y) = '.')
               then
                  if (x /= input_array'First
                      and then
                      input_array(x-1)(y) = '|')
                  then
                     input_array(x)(y) := '|';
                  end if;
               end if;
            end loop;
         end loop;

         for x in input_array'Range
         loop
            Ada.Text_IO.Put_Line (input_array(x));
         end loop;
      end;

      Ada.Text_IO.Put_Line ("splitter_count " & splitter_count'Image);

      Ada.Text_IO.Close(Input_File);

   end run;

end day7_2025;
