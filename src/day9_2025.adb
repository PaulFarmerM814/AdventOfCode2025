with Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Text_IO;
with Ada.Text_IO;

with gnat.Dynamic_Tables;
with gnat.String_Split;

with GNAT.Heap_Sort_G;

package body day9_2025 is

   type coordinate_type is
      record
         x          : Long_Long_Integer;
         y          : Long_Long_Integer;
      end record;

   type line_type is array(1..2) of coordinate_type;

   type area_type is
      record
         area    : Long_Long_Integer;
         start_x : Long_Long_Integer;
         start_y : Long_Long_Integer;
         end_x   : Long_Long_Integer;
         end_y   : Long_Long_Integer;
      end record;

   package input_type is new
     gnat.Dynamic_Tables(Table_Component_Type => coordinate_type,
                         Table_Index_Type     => Positive);

   input : input_type.Instance;

   procedure Get_Input (S : Ada.Text_IO.File_Type)
   is

      USV : Ada.Strings.Unbounded.Unbounded_String;

      split_string : GNAT.String_Split.Slice_Set;

   begin

      while not Ada.Text_IO.End_Of_File (S) loop

         USV := Ada.Strings.Unbounded.Text_IO.Get_Line (S);

         gnat.String_Split.Create(S          => split_string,
                                  From       => ada.strings.unbounded.To_String(USV),
                                  Separators => ",",
                                  Mode       => gnat.String_Split.Multiple);

         input.append((x => Long_Long_Integer'Value(gnat.String_Split.Slice(split_string,1)),
                       y => Long_Long_Integer'Value(gnat.String_Split.Slice(split_string,2))));

      end loop;

   end Get_Input;

   procedure run is

      Input_File   : Ada.Text_IO.File_Type;

   begin
      Ada.Text_IO.Open
        (File => Input_File,
         Mode => Ada.Text_IO.In_File,
         Name => "day9_2025.txt");

      Get_Input (Input_File);

      declare
         biggest_area : Long_Long_Integer;

         current_line : line_type;
         vertical : boolean;
         in_area : boolean;

         type sequential_rectangles_type is array(0..input_type.last(input) * input_type.last(input)) of area_type;
         sequential_rectangles : sequential_rectangles_type := (others => (0,0,0,0,0));

         procedure Move (From : Natural; To : Natural)
         is
         begin
            sequential_rectangles(to) := sequential_rectangles(from);
         end Move;
         pragma inline(Move);

         function Lt (Op1, Op2 : Natural) return Boolean
         is
         begin
            return (sequential_rectangles(op1).area < sequential_rectangles(op2).area);
         end Lt;
         pragma inline(Lt);

         package sort_type is new GNAT.Heap_Sort_G(Move => Move,
                                                   Lt   => Lt);

      begin
         for x in input_type.first..input_type.last(input)
         loop
            for y in input_type.first..input_type.last(input)
            loop
               if (sequential_rectangles(((y - 1) * input_type.last(input)) + x)) = (0,0,0,0,0)
                 and then
                   x /= y
               then
                  sequential_rectangles(((x - 1) * input_type.last(input)) + y) :=
                    (area    =>
                       ((abs(input.Table(x).x - input.Table(y).x) + 1) *
                        (abs(input.Table(x).y - input.Table(y).y) + 1)),
                     start_x => Long_Long_Integer'Min(input.Table(x).x,input.Table(y).x),
                     start_y => Long_Long_Integer'Min(input.Table(x).y,input.Table(y).y),
                     end_x   => Long_Long_Integer'Max(input.Table(x).x,input.Table(y).x),
                     end_y   => Long_Long_Integer'Max(input.Table(x).y,input.Table(y).y));
               end if;

            end loop;
         end loop;

         sort_type.sort(sequential_rectangles_type'Last);

         ada.Text_IO.Put_Line("Part 1 - largest area " & sequential_rectangles(sequential_rectangles'Last)'Img);

         main_rect_loop_pt2:
         for a in reverse sequential_rectangles_type'Range
         loop
            in_area := true;
            for x in input_type.first..input_type.last(input)
            loop
               if (x /= input_type.last(input))
               then
                  if (input.Table(x).y = input.Table(x+1).y)
                  then
                     vertical := true;
                     current_line := ((Long_Long_Integer'Min(input.Table(x).x,input.Table(x+1).x), input.Table(x).y),
                                      (Long_Long_Integer'Max(input.Table(x).x,input.Table(x+1).x), input.Table(x).y));

                  else
                     vertical := false;
                     current_line := ((input.Table(x).x, Long_Long_Integer'Min(input.Table(x).y,input.Table(x+1).y)),
                                      (input.Table(x).x, Long_Long_Integer'Max(input.Table(x).y,input.Table(x+1).y)));
                  end if;
               else
                  if (input.Table(x).y = input.Table(1).y)
                  then
                     vertical := true;
                     current_line := ((Long_Long_Integer'Min(input.Table(x).x,input.Table(1).x), input.Table(x).y),
                                      (Long_Long_Integer'Max(input.Table(x).x,input.Table(1).x), input.Table(x).y));

                  else
                     vertical := false;
                     current_line := ((input.Table(x).x, Long_Long_Integer'Min(input.Table(x).y,input.Table(1).y)),
                                      (input.Table(x).x, Long_Long_Integer'Max(input.Table(x).y,input.Table(1).y)));

                  end if;
               end if;

               if (vertical)
               then
                  if ((sequential_rectangles(a).start_y >= current_line(1).y
                       and then
                       sequential_rectangles(a).end_y >= current_line(1).y)
                      or else
                        (sequential_rectangles(a).start_y <= current_line(1).y
                         and then
                         sequential_rectangles(a).end_y <= current_line(1).y))
                    or else
                      (sequential_rectangles(a).start_x >= current_line(2).x)
                      or else
                        (sequential_rectangles(a).end_x <= current_line(1).x)
                  then
                     null;
                  else
                     in_area := false;
                  end if;
               else
                  if ((sequential_rectangles(a).start_x <= current_line(1).x
                       and then
                       sequential_rectangles(a).end_x <= current_line(1).x)
                      or else
                        (sequential_rectangles(a).start_x >= current_line(1).x
                         and then
                         sequential_rectangles(a).end_x >= current_line(1).x))
                    or else
                      (sequential_rectangles(a).start_y >= current_line(2).y)
                      or else
                        (sequential_rectangles(a).end_y <= current_line(1).y)
                  then
                     null;
                  else
                     in_area := false;
                  end if;
               end if;
            end loop;

            biggest_area := sequential_rectangles(a).area;

            exit main_rect_loop_pt2 when in_area;
         end loop main_rect_loop_pt2;

         ada.Text_IO.Put_Line("Part 2 - largest area " & biggest_area'Img);

      end;

      Ada.Text_IO.Close(Input_File);

   end run;

end day9_2025;
