with Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Text_IO;
with Ada.Text_IO;

with gnat.Dynamic_Tables;

with gnat.String_Split;

package body day6_2025 is

   type operation_type is (noop, add, multiply);

   type equation_type is record
      parameters  : ada.strings.Unbounded.Unbounded_String;
      result      : Long_Long_Integer := 0;
      operation   : operation_type := noop;
   end record;

   package all_equations_type is new
     gnat.Dynamic_Tables(Table_Component_Type => equation_type,
                         Table_Index_Type     => Positive);

   package pt2_all_equations_type is new
     gnat.Dynamic_Tables(Table_Component_Type => ada.strings.Unbounded.Unbounded_String,
                         Table_Index_Type     => Positive);


   equations     : all_equations_type.Instance;
   equations_pt2 : pt2_all_equations_type.Instance;

   full_result : Long_Long_Integer := 0;
   full_result_pt2 : Long_Long_Integer := 0;

   current_line : natural := 1;

   procedure Get_Input (S : Ada.Text_IO.File_Type)
   is

      USV : Ada.Strings.Unbounded.Unbounded_String;

      split : natural;

      split_string : GNAT.String_Split.Slice_Set;

      current_valid_parameter : positive := 1;

   begin

      while not Ada.Text_IO.End_Of_File (S) loop

         USV := Ada.Strings.Unbounded.Text_IO.Get_Line (S);

         equations_pt2.append(USV);

         split := ada.Strings.Unbounded.Index(Source  => USV,
                                              Pattern => "*");

         gnat.String_Split.Create(S          => split_string,
                                  From       => ada.strings.unbounded.To_String(USV),
                                  Separators => " ",
                                  Mode       => gnat.String_Split.Multiple);

         if (split = 0)
         then
            current_valid_parameter := 1;

            for x in gnat.String_Split.First_Cursor(split_string)..gnat.String_Split.Slice_Count (split_string)
            loop
               if current_line = 1
               then

                  equations.append((parameters => ada.Strings.Unbounded.Null_Unbounded_String,
                                    result     => 0,
                                    operation  => noop));
               end if;

               if gnat.String_Split.Slice
                 (S     => split_string,
                  index => x) /= ""
               then
                  equations.table(current_valid_parameter).parameters.append(gnat.String_Split.Slice
                                                                             (S     => split_string,
                                                                              index => x) & " ");
                  current_valid_parameter := @ + 1;
               end if;
            end loop;
         elsif ada.Strings.Unbounded.Index_Non_Blank(Source => USV) /= 0
         then
            current_valid_parameter := 1;
            for x in gnat.String_Split.First_Cursor(split_string)..gnat.String_Split.Slice_Count (split_string)
            loop
               if gnat.String_Split.Slice
                 (S     => split_string,
                  index => x) = "+"
               then
                  equations.table(current_valid_parameter).operation := add;
                  equations.table(current_valid_parameter).result := 0;
                  current_valid_parameter := @ + 1;
               elsif gnat.String_Split.Slice
                 (S     => split_string,
                  index => x) = "*"
               then
                  equations.table(current_valid_parameter).operation := multiply;
                  equations.table(current_valid_parameter).result := 1;
                  current_valid_parameter := @ + 1;
               end if;
            end loop;
         end if;

         current_line := @ + 1;

      end loop;

   end Get_Input;

   procedure run is

      Input_File   : Ada.Text_IO.File_Type;

      split_string : GNAT.String_Split.Slice_Set;

      parameter : Long_Long_Integer;

      current_result : Long_Long_Integer;

   begin
      Ada.Text_IO.Open
        (File => Input_File,
         Mode => Ada.Text_IO.In_File,
         Name => "day6_2025.txt");

      Get_Input (Input_File);

      for x in all_equations_type.first..equations.last
      loop

         gnat.String_Split.Create(S          => split_string,
                                  From       => ada.strings.unbounded.To_String(equations.table(x).parameters),
                                  Separators => " ",
                                  Mode       => gnat.String_Split.Multiple);

         single_equation_loop:
         for y in gnat.String_Split.First_Cursor(split_string)..gnat.String_Split.Slice_Count (split_string)
         loop
            if (gnat.String_Split.Slice
                (S     => split_string,
                 index => y) /= "")
            then
               parameter :=
                 long_long_integer'Value(gnat.String_Split.Slice
                                         (S     => split_string,
                                          index => y));

               case equations.Table(x).operation is
               when noop =>
                  null;
               when add =>
                  equations.table(x).result := @ + parameter;
               when multiply =>
                  equations.table(x).result := @ * parameter;
               end case;
            end if;
         end loop single_equation_loop;

         full_result := @ + equations.table(x).result;
      end loop;

      Ada.Text_IO.Put_Line ("full_result " & full_result'Image);

      declare
         subtype parameter_str_type is string(1..current_line - 1);
         null_string : constant parameter_str_type := (others => ' ');

         transposed : array(1..ada.strings.unbounded.length(equations_pt2.table(1))) of parameter_str_type;
         current_operator : operation_type := noop;

         temp_string : parameter_str_type;
      begin
         for x in pt2_all_equations_type.first..equations_pt2.last
         loop

            for y in 1..equations_pt2.Table(1).length
            loop
               transposed(y)(x) := ada.strings.Unbounded.element(equations_pt2.Table(x),y);
            end loop;

         end loop;

         for x in transposed'Range
         loop
            if (transposed(x)(transposed(x)'Last) = '+')
            then
               current_operator := add;
               current_result := 0;
            elsif (transposed(x)(transposed(x)'Last) = '*')
            then
               current_operator := multiply;
               current_result := 1;
            end if;
            temp_string := transposed(x);
            temp_string(temp_string'Last) := ' ';
            if (temp_string /= null_string)
            then
               if (current_operator = add)
               then
                  current_result := @ + Long_Long_Integer'Value(temp_string);
               else
                  current_result := @ * Long_Long_Integer'Value(temp_string);
               end if;
            else
               full_result_pt2 := full_result_pt2 + current_result;
               current_result := 0;
            end if;

         end loop;

         full_result_pt2 := full_result_pt2 + current_result;

      end;

      Ada.Text_IO.Put_Line ("full_result_pt2 " & full_result_pt2'Image);

      Ada.Text_IO.Close(Input_File);

   end run;

end day6_2025;
