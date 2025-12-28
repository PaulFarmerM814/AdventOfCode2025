with Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Text_IO;
with Ada.Text_IO;

with GNAT.String_Split;

with GNAT.Lists;
with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Strings.Hash;

package body Day11_2025 is


   subtype List_Element_Type is String (1 .. 3);

   procedure Destroy_List_Element (Elem : in out List_Element_Type)
   is
   begin
      null;
   end Destroy_List_Element;

   function Equivalent_Keys (Left, Right : in List_Element_Type)
                            return Boolean
   is
   begin

      return Left = Right;

   end Equivalent_Keys;

   package String_List is new
     Gnat.Lists.Doubly_Linked_Lists (Element_Type    => List_Element_Type,
                                     "="             => "=",
                                     Destroy_Element => Destroy_List_Element);


   type Hash_Table_Type is record
      Key  : List_Element_Type;
      List : String_List.Doubly_Linked_List;
   end record;

   function "=" (Left, Right : in Hash_Table_Type)
                return Boolean
   is
   begin
      return Left.Key = Right.Key;
   end "=";

   package Hashed_Input_Store is new
     Ada.Containers.Indefinite_Hashed_Maps (Key_Type        => List_Element_Type,
                                            Element_Type    => Hash_Table_Type,
                                            Hash            => Ada.Strings.Hash,
                                            Equivalent_Keys => Equivalent_Keys,
                                            "="             => "=");

   Input : Hashed_Input_Store.Map;

   function Get_Input (S : Ada.Text_IO.File_Type) return Hash_Table_Type
   is

      USV : constant Ada.Strings.Unbounded.Unbounded_String := Ada.Strings.Unbounded.Text_IO.Get_Line (S);

      Split_String : GNAT.String_Split.Slice_Set;

      Current_Line : Hash_Table_Type;

      Separator    : constant Natural := Ada.Strings.Unbounded.Index (USV, ":");

   begin

      Current_Line.Key := Ada.Strings.Unbounded.Slice (Source => USV,
                                                       Low    => 1,
                                                       High   => Separator - 1);

      Current_Line.List := String_List.Create;

      Gnat.String_Split.Create (S          => Split_String,
                                From       => Ada.Strings.Unbounded.Slice (Source => USV,
                                                                           Low    => Separator + 2,
                                                                           High   => Ada.Strings.Unbounded.Length (USV)),
                                Separators => " ",
                                Mode       => Gnat.String_Split.Multiple);

      for X in 1 .. Split_String.Slice_Count
      loop
         Current_Line.List.Append (Split_String.Slice (X));
      end loop;

      return Current_Line;

   end Get_Input;

   procedure Run is

      Input_File   : Ada.Text_IO.File_Type;

      Start        : Hash_Table_Type;

      Path_Count   : Natural := 0;

      procedure Scan_Paths (Start_Point : in     Hash_Table_Type;
                            Path_Count  : in out Natural)
      is

         Iter    : String_List.Iterator;
         Element : List_Element_Type;

         New_Start : Hash_Table_Type;

      begin

         Iter := String_List.Iterate (Start_Point.List);
         while String_List.Has_Next (Iter) loop
            String_List.Next (Iter, Element);
            if (Element /= "out")
            then
               New_Start := Input.Element (Key => Element);
               Scan_Paths (Start_Point => New_Start,
                           Path_Count  => Path_Count);
            else
               Path_Count := @ + 1;
            end if;
         end loop;

      end Scan_Paths;

   begin
      Ada.Text_IO.Open
        (File => Input_File,
         Mode => Ada.Text_IO.In_File,
         Name => "day11_2025.txt");

      while not Ada.Text_IO.End_Of_File (Input_File) loop
         declare
            Current_Line : constant Hash_Table_Type := Get_Input (S => Input_File);
         begin
            Input.Insert (Key      => Current_Line.Key,
                          New_Item => Current_Line);
         end;
      end loop;

      Start :=  Input.Element (Key => "you");

      Scan_Paths (Start_Point => Start,
                  Path_Count  => Path_Count);

      Ada.Text_IO.Put_Line("Day 11 - Part 1 - Path Count " & Path_Count'Img);

      Ada.Text_IO.Close (Input_File);

   end Run;

end Day11_2025;
