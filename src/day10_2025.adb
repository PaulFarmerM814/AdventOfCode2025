with Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Text_IO;
with Ada.Text_IO;

with gnat.String_Split;


package body day10_2025 is

   type lights_type is array(natural range <>) of boolean;

   type button_type is array(positive range <>) of natural;

   type button_access_type is access all button_type;

   type buttons_type is array(positive range <>) of button_access_type;

   type joltage_type is array(natural range <>) of natural;

   type single_input_line_type
     (light_count  : natural;
      button_count : positive)
   is
      record
         lights  : lights_type(0..light_count)   := (others => false);
         buttons : buttons_type(1..button_count) := (others => null);
         joltage : joltage_type(0..light_count)  := (others => 0);
      end record;

   function Get_Input (S : Ada.Text_IO.File_Type) return single_input_line_type
   is

      use type gnat.String_Split.Slice_Number;

      USV : constant Ada.Strings.Unbounded.Unbounded_String := Ada.Strings.Unbounded.Text_IO.Get_Line (S);

      split_string : GNAT.String_Split.Slice_Set :=
        gnat.String_Split.Create(From       => ada.strings.unbounded.To_String(USV),
                                 Separators => " ",
                                 Mode       => gnat.String_Split.Multiple);

      split_string_button : GNAT.String_Split.Slice_Set;

      lights_str : constant string := gnat.String_Split.Slice(split_string,1);

      light_count : constant natural := lights_str(1..lights_str'Length-2)'Length;

      button_count : constant Positive := Positive(split_string.slice_count) - 2;

      current_line : single_input_line_type(light_count  => light_count - 1,
                                            button_count => button_count);

   begin

      for x in current_line.lights'Range
      loop
         current_line.lights(x) := lights_str(x+2) = '#';
      end loop;

      for x in 2..split_string.slice_count-1
      loop
         split_string_button :=
           gnat.String_Split.Create(From       =>
                                      split_string.slice(x)(split_string.slice(x)'First+1..split_string.slice(x)'Last - 1),
                                    Separators => ",",
                                    Mode       => gnat.String_Split.Multiple);

         current_line.buttons(integer(x)-1) := new button_type(1..positive(split_string_button.slice_count));

         for y in 1..split_string_button.slice_count
         loop
            current_line.buttons(integer(x)-1)(integer(y)) := natural'Value(split_string_button.slice(y));
         end loop;
      end loop;

      split_string :=
        gnat.String_Split.Create(From       =>
                                   split_string.slice(split_string.slice_count)
                                 (split_string.slice(split_string.slice_count)'First+1..split_string.slice(split_string.slice_count)'Last - 1),
                                 Separators => ",",
                                 Mode       => gnat.String_Split.Multiple);

      for x in current_line.joltage'Range
      loop
         current_line.joltage(x) := natural'Value(split_string.slice(gnat.String_Split.Slice_Number(x+1)));
      end loop;


      return current_line;

   end Get_Input;

   procedure run is

      Input_File   : Ada.Text_IO.File_Type;

      switch_count : natural;
      current_switch_count : natural;
      machine_total : natural := 0;

   begin
      Ada.Text_IO.Open
        (File => Input_File,
         Mode => Ada.Text_IO.In_File,
         Name => "day10_2025.txt");

      while not Ada.Text_IO.End_Of_File (Input_File) loop
         declare
            current_line : constant single_input_line_type := Get_Input(S => Input_File);

            button_iterator : natural := 2**current_line.button_count-1;
            type power_as_bool_type is array(0..15) of boolean;
            pragma pack(power_as_bool_type);
            power_as_bool : power_as_bool_type;
            for power_as_bool'Address use button_iterator'Address;
            lights : lights_type(0..current_line.light_count) := (others => false);

            solved : boolean;

         begin

            current_switch_count := positive'Last;
            solved := false;
            while button_iterator > 0
            loop
               switch_count := 0;
               lights := (others => false);
               power_as_bool_loop:
               for x in power_as_bool'Range
               loop
                  if (power_as_bool(x))
                  then
                     switch_count := @ + 1;
                     for light in 1..current_line.buttons(x+1)'Length
                     loop
                        lights(current_line.buttons(x+1)(light)) :=
                          not lights(current_line.buttons(x+1)(light));
                     end loop;
                     if (lights = current_line.lights)
                     then
                        if (switch_count < current_switch_count)
                        then
                           current_switch_count := switch_count;
                           solved := true;
                           exit power_as_bool_loop;
                        end if;
                     end if;
                  end if;
               end loop power_as_bool_loop;
               button_iterator := button_iterator - 1;
            end loop;
            machine_total := @ + current_switch_count;

            if not solved
            then
               Ada.Text_IO.Put_Line("not solved " & current_line'Img);
            end if;

         end;

      end loop;

      Ada.Text_IO.Put_Line("Part 1 : machine_total " & machine_total'Img);

      Ada.Text_IO.Close(Input_File);

   end run;

end day10_2025;
