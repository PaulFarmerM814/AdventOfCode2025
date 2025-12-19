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
   beam_count : Long_Long_Integer := 0;

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
         type input_int_type is array(1..ada.strings.unbounded.length(input.table(1))) of Long_Long_Integer;
         type input_int_array_type is array(1..input.last) of input_int_type;

         input_array : input_array_type;
         input_array_pt2 : input_array_type;
         input_int_array_pt2 : input_int_array_type := (others => (others => 0));

         procedure beam(start_x : in integer;
                        start_y : in integer)
         is
         begin
            if (input_array_pt2(start_y)(start_x) = '^')
            then
               beam(start_x - 1, start_y + 1);
               beam(start_x + 1, start_y + 1);
            elsif (input_array_pt2(start_y)(start_x) = '.' or else input_array_pt2(start_y)(start_x) = 'S')
            then
               if (start_y >= input_array_pt2'Last)
               then
                  beam_count := @ + 1;
                  --  Ada.Text_IO.Put_Line ("beam_count " & beam_count'Image);
               else
                  if (start_y + 1 >= input_array_pt2'Last)
                  then
                     beam_count := @ + 1;
                     --  Ada.Text_IO.Put_Line ("beam_count " & beam_count'Image);
                  else
                     for y in start_y + 1..input_array_pt2'Last
                     loop
                        if input_array_pt2(y)(start_x) = '^'
                        then
                           beam(start_x - 1, y + 1);
                           beam(start_x + 1, y + 1);
                           exit;
                        elsif (y = input_array_pt2'Last)
                        then
                           beam_count := @ + 1;
                           --  Ada.Text_IO.Put_Line ("beam_count " & beam_count'Image);
                        end if;
                     end loop;
                  end if;
               end if;
            end if;
         end beam;

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
                  input_array(x)(y) := '|';
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

         Ada.Text_IO.Put_Line ("splitter_count " & splitter_count'Image);

         input_array_pt2 := input_array;

         for x in input_array_pt2'Range
         loop
            for y in input_array_pt2(x)'Range
            loop
               if (input_array_pt2(x)(y) = 'S')
               then
                  null;
               elsif (input_array_pt2(x)(y) = '^')
               then
                  null;
               elsif (input_array_pt2(x)(y) = '|')
               then
                  if (x = input_array_pt2'First)
                  then
                     input_int_array_pt2(x)(y) := 1;
                  else
                     input_int_array_pt2(x)(y) := input_int_array_pt2(x-1)(y);
                  end if;

                  if (y /= input_array_pt2(x)'First and then input_array_pt2(x)(y-1) = '^')
                  then
                     input_int_array_pt2(x)(y) := @ + input_int_array_pt2(x-1)(y-1);
                  end if;
                  if (y /= input_array_pt2(x)'Last and then input_array_pt2(x)(y+1) = '^')
                  then
                     input_int_array_pt2(x)(y) := @ + input_int_array_pt2(x-1)(y+1);
                  end if;
               elsif (input_array_pt2(x)(y) = '.')
               then
                  null;
               end if;
            end loop;
         end loop;

         for y in input_int_array_pt2(input_int_array_pt2'Last)'Range
         loop
            beam_count := @ + input_int_array_pt2(input_int_array_pt2'Last)(y);
         end loop;

         --  why do I never learn and try recursion...... doh!!!!
         outer:
         for x in input_array_pt2'Range
         loop
            for y in input_array_pt2(x)'Range
            loop
               if (input_array_pt2(x)(y) = 'S')
               then
                  --  beam(y,x);
                  exit outer;
               end if;
            end loop;
         end loop outer;

      end;

      Ada.Text_IO.Put_Line ("beam_count " & beam_count'Image);

      Ada.Text_IO.Close(Input_File);

   end run;

end day7_2025;
