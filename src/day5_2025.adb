with Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Text_IO;
with Ada.Text_IO;

with gnat.Dynamic_Tables;

package body day5_2025 is

   type range_type is record
      start  : Long_Long_Integer;
      finish : Long_Long_Integer;
   end record;

   package ranges_type is new
     gnat.Dynamic_Tables(Table_Component_Type => range_type,
                         Table_Index_Type     => Positive);

   ingredient_ranges : ranges_type.Instance;

   package ingredient_type is new
     gnat.Dynamic_Tables(Table_Component_Type => Long_Long_Integer,
                         Table_Index_Type     => Positive);


   ingredients : ingredient_type.Instance;

   fresh_count : Long_Long_Integer := 0;

   procedure Get_Input (S : Ada.Text_IO.File_Type)
   is

      USV : Ada.Strings.Unbounded.Unbounded_String;

      split : natural;

   begin

      while not Ada.Text_IO.End_Of_File (S) loop

         USV := Ada.Strings.Unbounded.Text_IO.Get_Line (S);

         split := ada.Strings.Unbounded.Index(Source  => USV,
                                              Pattern => "-");

         if (split /= 0)
         then
            ingredient_ranges.append
              ((start  => Long_Long_Integer'Value(ada.Strings.Unbounded.To_String(USV)(1..split-1)),
                finish => Long_Long_Integer'Value(ada.Strings.Unbounded.To_String(USV)(split+1..ada.Strings.Unbounded.Length(USV)))));
         elsif ada.Strings.Unbounded.Index_Non_Blank(Source => USV) /= 0
         then
            ingredients.append(Long_Long_Integer'Value(ada.Strings.Unbounded.To_String(USV)));
         end if;

      end loop;

   end Get_Input;

   procedure run is

      Input_File   : Ada.Text_IO.File_Type;

   begin
      Ada.Text_IO.Open
        (File => Input_File,
         Mode => Ada.Text_IO.In_File,
         Name => "day5_2025.txt");

      Get_Input (Input_File);

      for x in ranges_type.first..ingredients.last
      loop
         ingredient_ranges_loop:
         for y in ranges_type.first..ingredient_ranges.last
         loop

            if (ingredients.Table(x) in ingredient_ranges.Table(y).start..ingredient_ranges.Table(y).finish)
            then
               fresh_count := @ + 1;
               exit ingredient_ranges_loop;
            end if;

         end loop ingredient_ranges_loop;
      end loop;

      Ada.Text_IO.Put_Line ("fresh_count " & fresh_count'Image);

      Ada.Text_IO.Close(Input_File);

   end run;

end day5_2025;
