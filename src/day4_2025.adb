with Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Text_IO;
use Ada.Strings.Unbounded;
with Ada.Text_IO;

with gnat.Dynamic_Tables;

package body day4_2025 is

   package grid_type is new
     gnat.Dynamic_Tables(Table_Component_Type => ada.strings.unbounded.Unbounded_String,
                         Table_Index_Type     => Positive);

   grid : grid_type.Instance;

   fewer_than_four_count : natural := 0;
   fewer_than_four_count_pt2 : natural := 0;

   procedure Get_Input (S : Ada.Text_IO.File_Type)
   is

      USV : Ada.Strings.Unbounded.Unbounded_String :=
        Ada.Strings.Unbounded.Text_IO.Get_Line (S);

   begin

      USV := "." & USV & ".";

      if (grid.is_empty)
      then
         declare
            blank_string : constant string(1..ada.Strings.Unbounded.Length(USV)) := (others => '.');
            blank_line : constant ada.strings.Unbounded.Unbounded_String :=
              ada.strings.unbounded.To_Unbounded_String(blank_string);
         begin
            grid.append(blank_line);
         end;
      end if;

      grid.Append(USV);

   end Get_Input;

   procedure remove_rolls
   is

      rolls_removed : boolean := true;

   begin

       declare
         Tab : grid_type.Table_Type renames grid.Table (Positive'First .. grid.Last);

         row_length : constant positive := ada.strings.Unbounded.Length(tab(positive'First));

         adjacent_count : natural := 0;

      begin

         loop

            rolls_removed := false;

            for x in positive range Positive'First + 1 .. grid.Last - 1
            loop
               for y in positive range 2..row_length - 1
               loop
                  if ada.strings.Unbounded.Element(tab(x),y) = '@'
                  then
                     adjacent_count := 0;
                     if (ada.strings.Unbounded.Element(tab(x+1),y) = '@')
                     then
                        adjacent_count := @ + 1;
                     end if;
                     if (ada.strings.Unbounded.Element(tab(x+1),y-1) = '@')
                     then
                        adjacent_count := @ + 1;
                     end if;
                     if (ada.strings.Unbounded.Element(tab(x+1),y+1) = '@')
                     then
                        adjacent_count := @ + 1;
                     end if;
                     if (ada.strings.Unbounded.Element(tab(x-1),y) = '@')
                     then
                        adjacent_count := @ + 1;
                     end if;
                     if (ada.strings.Unbounded.Element(tab(x-1),y-1) = '@')
                     then
                        adjacent_count := @ + 1;
                     end if;
                     if (ada.strings.Unbounded.Element(tab(x-1),y+1) = '@')
                     then
                        adjacent_count := @ + 1;
                     end if;
                     if (ada.strings.Unbounded.Element(tab(x),y-1) = '@')
                     then
                        adjacent_count := @ + 1;
                     end if;
                     if (ada.strings.Unbounded.Element(tab(x),y+1) = '@')
                     then
                        adjacent_count := @ + 1;
                     end if;

                     if (adjacent_count < 4)
                     then
                        rolls_removed := true;
                        fewer_than_four_count_pt2 := @ + 1;
                        ada.strings.Unbounded.Replace_Element(tab(x),y,'.');
                     end if;
                  end if;
               end loop;
            end loop;

            exit when not rolls_removed;
         end loop;
      end;

   end remove_rolls;

   procedure run is

      Input_File   : Ada.Text_IO.File_Type;

   begin
      Ada.Text_IO.Open
        (File => Input_File,
         Mode => Ada.Text_IO.In_File,
         Name => "day4_2025.txt");

      --  Input_Stream := Ada.Streams.Stream_IO.Stream (Input_File);

      while not Ada.Text_IO.End_Of_File (Input_File) loop

         Get_Input (Input_File);

      end loop;

      declare
         blank_string : constant string(1..ada.Strings.Unbounded.Length(grid.Table(1))) := (others => '.');
         blank_line : constant ada.strings.Unbounded.Unbounded_String :=
           ada.strings.unbounded.To_Unbounded_String(blank_string);
      begin
         grid.append(blank_line);
      end;

      declare
         Tab : grid_type.Table_Type renames grid.Table (Positive'First .. grid.Last);

         row_length : constant positive := ada.strings.Unbounded.Length(tab(positive'First));

         adjacent_count : natural := 0;

      begin

         for x in positive range Positive'First + 1 .. grid.Last - 1
         loop
            for y in positive range 2..row_length - 1
            loop
               if ada.strings.Unbounded.Element(tab(x),y) = '@'
               then
                  adjacent_count := 0;
                  if (ada.strings.Unbounded.Element(tab(x+1),y) = '@')
                  then
                     adjacent_count := @ + 1;
                  end if;
                  if (ada.strings.Unbounded.Element(tab(x+1),y-1) = '@')
                  then
                     adjacent_count := @ + 1;
                  end if;
                  if (ada.strings.Unbounded.Element(tab(x+1),y+1) = '@')
                  then
                     adjacent_count := @ + 1;
                  end if;
                  if (ada.strings.Unbounded.Element(tab(x-1),y) = '@')
                  then
                     adjacent_count := @ + 1;
                  end if;
                  if (ada.strings.Unbounded.Element(tab(x-1),y-1) = '@')
                  then
                     adjacent_count := @ + 1;
                  end if;
                  if (ada.strings.Unbounded.Element(tab(x-1),y+1) = '@')
                  then
                     adjacent_count := @ + 1;
                  end if;
                  if (ada.strings.Unbounded.Element(tab(x),y-1) = '@')
                  then
                     adjacent_count := @ + 1;
                  end if;
                  if (ada.strings.Unbounded.Element(tab(x),y+1) = '@')
                  then
                     adjacent_count := @ + 1;
                  end if;

                  if (adjacent_count < 4)
                  then
                     fewer_than_four_count := @ + 1;
                  end if;
               end if;
            end loop;
         end loop;
      end;

      Ada.Text_IO.Put_Line ("fewer_than_four_count " & fewer_than_four_count'Image);

      remove_rolls;
      Ada.Text_IO.Put_Line ("fewer_than_four_count_pt2 " & fewer_than_four_count_pt2'Image);

      Ada.Text_IO.Close(Input_File);

   end run;

end day4_2025;
