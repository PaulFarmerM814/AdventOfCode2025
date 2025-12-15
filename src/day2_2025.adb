with Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Text_IO;
with Ada.Strings.Fixed;
with Ada.Text_IO;

package body day2_2025 is

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

      range_start : Long_Long_Integer;
      range_end   : Long_Long_Integer;

      range_start_len : integer;
      range_end_len   : Integer;

      invalid_range_sum : Long_Long_Integer := 0;
      invalid_range_sum_pt2 : Long_Long_Integer := 0;

      range_valid : boolean;

      start_pos : integer;
      divider   : integer := -1;
      comma     : integer;

      matching_at_end : boolean := false;
      current_substr_len : integer;
      matching_substr : boolean;

   begin

      Ada.Text_IO.Open
        (File => Input_File,
         Mode => Ada.Text_IO.In_File,
         Name => "day2_2025.txt");

      while not Ada.Text_IO.End_Of_File (Input_File) loop

         start_pos := 1;

         declare
            input_string : constant string := Get_Input (Input_File);
         begin
            while (divider /=0)
            loop

               divider := ada.strings.fixed.index(Source  => input_string,
                                                  Pattern => "-",
                                                  from    => start_pos);

               comma := ada.strings.fixed.index(Source  => input_string,
                                                Pattern => ",",
                                                from    => start_pos);

               if (comma = 0)
               then
                  comma := input_string'Length + 1;
               end if;

               if (divider /= 0)
               then

                  range_start_len := divider - start_pos;
                  range_end_len   := comma - start_pos - range_start_len - 1;

                  range_start := long_long_integer'Value(input_string(start_pos..divider-1));
                  range_end   := long_long_integer'Value(input_string(divider+1..comma-1));

                  start_pos := comma + 1;

                  Ada.Text_IO.Put_Line (Item => range_start'Img & " " & range_end'Img & " " & range_start_len'Img & " " & range_end_len'Img);

                  --  both odd number ranges, so can't have invalid
                  if (range_start_len mod 2 = 1 and then range_end_len mod 2 = 1)
                  then
                     null;
                  else
                     for x in range_start..range_end
                     loop
                        --  odd number range length, so can't be invalid
                        if ((x'Img'Length - 1) mod 2 = 0)
                        then
                           range_valid := false;

                           declare
                              x_string : constant string := x'Img(2..x'Img'Length);
                           begin
                              this_range_loop:
                              for y in x_string'First..x_string'First + x_string'Length/2 -1
                              loop
                                 if (x_string(y) /= x_string(x_string'Length/2 + y))
                                 then
                                    range_valid := true;
                                    exit this_range_loop;
                                 end if;
                              end loop this_range_loop;
                           end;
                           if (not range_valid)
                           then
                              ada.text_io.put_line("invalid_id " & x'Img);
                              invalid_range_sum := invalid_range_sum + x;
                           end if;
                        end if;
                     end loop;

                  end if;

                  for x in range_start..range_end
                  loop

                     declare
                        x_string : constant string := x'Img(2..x'Img'Length);
                     begin
                        current_substr_len := x_string'Length - 1;

                        matching_at_end := false;

                        substr_loop:
                        while current_substr_len >= 1 and then not matching_at_end
                        loop
                           declare
                              current_substr : constant string := x_string(x_string'First..x_string'First+current_substr_len-1);
                              current_substr_start : integer := x_string'First+current_substr_len;
                           begin

                              matching_substr := true;

                              while matching_substr
                              loop
                                 if current_substr_start+current_substr_len-1 > x_string'Last
                                 then
                                    matching_substr := false;
                                    matching_at_end := false;
                                 elsif x_string(current_substr_start..current_substr_start+current_substr_len-1) = current_substr
                                 then
                                    if (current_substr_start+current_substr_len-1 = x_string'Last)
                                    then
                                       matching_substr := false;
                                       matching_at_end := true;
                                    end if;
                                 else
                                    matching_at_end := false;
                                    matching_substr := false;
                                 end if;

                                 current_substr_start := current_substr_start + current_substr_len;
                              end loop;
                           end;
                           current_substr_len := current_substr_len - 1;
                        end loop substr_loop;
                     end;

                     if (matching_at_end)
                     then
                        ada.text_io.put_line("invalid_id " & x'Img & " current_substr_len " & current_substr_len'Img);
                        invalid_range_sum_pt2 := invalid_range_sum_pt2 + x;
                     end if;

                  end loop;

               end if;
            end loop;
         end;
      end loop;

      Ada.Text_IO.Put_Line ("invalid_range_sum " & invalid_range_sum'Image);
      Ada.Text_IO.Put_Line ("invalid_range_sum_pt2 " & invalid_range_sum_pt2'Image);

      Ada.Text_IO.Close(Input_File);

   end run;

end day2_2025;
