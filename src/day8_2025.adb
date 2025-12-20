with Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Text_IO;
with Ada.Text_IO;

with gnat.Dynamic_Tables;
with gnat.String_Split;

with GNAT.Heap_Sort_G;

package body day8_2025 is

   type coordinate_type is
      record
         x          : Long_Long_Integer;
         y          : Long_Long_Integer;
         z          : Long_Long_Integer;
         in_circuit : Natural;
      end record;

   type circuit_type is array(0..1000) of natural;

   type distance_type is
      record
         distance    : Long_Long_Integer;
         start_coord : natural;
         end_coord   : natural;
      end record;

   package input_type is new
     gnat.Dynamic_Tables(Table_Component_Type => coordinate_type,
                         Table_Index_Type     => Positive);

   input : input_type.Instance;

   circuits         : circuit_type := (others => 0);
   circuit_count    : natural := 0;
   connection_count : natural := 0;

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

         input.append((x          => Long_Long_Integer'Value(gnat.String_Split.Slice(split_string,1)),
                       y          => Long_Long_Integer'Value(gnat.String_Split.Slice(split_string,2)),
                       z          => Long_Long_Integer'Value(gnat.String_Split.Slice(split_string,3)),
                       in_circuit => 0));

      end loop;

   end Get_Input;

   procedure run is

      Input_File   : Ada.Text_IO.File_Type;

   begin
      Ada.Text_IO.Open
        (File => Input_File,
         Mode => Ada.Text_IO.In_File,
         Name => "day8_2025.txt");

      Get_Input (Input_File);

      declare
         type sequential_distances_type is array(0..input_type.last(input) * input_type.last(input)) of distance_type;
         sequential_distances : sequential_distances_type := (others => (0,0,0));

         procedure Move (From : Natural; To : Natural)
         is
         begin
            sequential_distances(to) := sequential_distances(from);
         end Move;
         pragma inline(Move);

         function Lt (Op1, Op2 : Natural) return Boolean
         is
         begin
            return (sequential_distances(op1).distance < sequential_distances(op2).distance);
         end Lt;
         pragma inline(Lt);

         package sort_type is new GNAT.Heap_Sort_G(Move => Move,
                                                   Lt   => Lt);

         procedure Move1 (From : Natural; To : Natural)
         is
         begin
            circuits(to) := circuits(from);
         end Move1;
         pragma inline(Move1);

         function Lt1 (Op1, Op2 : Natural) return Boolean
         is
         begin
            return (circuits(op1) > circuits(op2));
         end Lt1;
         pragma inline(Lt1);

         package sort_type1 is new GNAT.Heap_Sort_G(Move => Move1,
                                                    Lt   => Lt1);

         local_circuit : natural;

      begin
         for x in input_type.first..input_type.last(input)
         loop
            for y in input_type.first..input_type.last(input)
            loop
               if (sequential_distances(((y - 1) * input_type.last(input)) + x)) = (0,0,0)
                 and then
                   x /= y
               then
                  sequential_distances(((x - 1) * input_type.last(input)) + y) :=
                    (distance    => ((input.Table(x).x - input.Table(y).x)**2)
                     +
                       ((input.Table(x).y - input.Table(y).y)**2)
                         +
                       ((input.Table(x).z - input.Table(y).z)**2),
                     start_coord => x,
                     end_coord   => y);
               end if;

            end loop;
         end loop;

         sort_type.sort(sequential_distances_type'Last);

         for x in 1..sequential_distances_type'Last
         loop
            if (sequential_distances(x).distance /= 0)
            then
               connection_count := @ + 1;
               if (input.Table(sequential_distances(x).start_coord).in_circuit /= 0)
               then
                  if (input.Table(sequential_distances(x).end_coord).in_circuit = 0)
                  then
                     circuits(input.Table(sequential_distances(x).start_coord).in_circuit) :=  @ + 1;
                     input.Table(sequential_distances(x).end_coord).in_circuit :=
                       input.Table(sequential_distances(x).start_coord).in_circuit;
                  else
                     if (input.Table(sequential_distances(x).start_coord).in_circuit /=
                           input.Table(sequential_distances(x).end_coord).in_circuit)
                     then
                        local_circuit := input.Table(sequential_distances(x).end_coord).in_circuit;
                        circuits(input.Table(sequential_distances(x).start_coord).in_circuit) :=
                          @ + circuits(input.Table(sequential_distances(x).end_coord).in_circuit);
                        circuits(input.Table(sequential_distances(x).end_coord).in_circuit) := 0;
                        for y in input_type.First..input_type.Last(input)
                        loop
                           if (input.Table(y).in_circuit = local_circuit)
                           then
                              input.Table(y).in_circuit := input.Table(sequential_distances(x).start_coord).in_circuit;
                           end if;
                        end loop;
                     end if;
                  end if;
               elsif (input.Table(sequential_distances(x).end_coord).in_circuit = 0)
               then
                  circuit_count := circuit_count + 1;
                  input.Table(sequential_distances(x).start_coord).in_circuit := circuit_count;
                  input.Table(sequential_distances(x).end_coord).in_circuit := circuit_count;
                  circuits(circuit_count) := 2;
               else
                  circuits(input.Table(sequential_distances(x).end_coord).in_circuit) :=  @ + 1;
                  input.Table(sequential_distances(x).start_coord).in_circuit :=
                    input.Table(sequential_distances(x).end_coord).in_circuit;
               end if;
            end if;

            exit when connection_count = 1000;
         end loop;

         sort_type1.Sort(1000);

         ada.text_io.put_line(circuits(1)'Img & " " & circuits(2)'Img & " " & circuits(3)'Img);
         ada.text_io.put_line(Integer'Image(circuits(1) * circuits(2) * circuits(3)));

      end;

      Ada.Text_IO.Close(Input_File);

   end run;

end day8_2025;
