with Ada.Text_IO;         use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Direct_IO;
with Ada.Strings; use Ada.Strings;
with Ada.Numerics.Float_Random; use Ada.Numerics.Float_Random;

with Ada.Strings.Fixed; use Ada.Strings.Fixed;

procedure Compare_Hash_Probing is



   Hash_Table_Size : constant := 128;
   type String16 is array(1..16) of Character;
   subtype KeyString is String16;
   type Two_Chars is array(1 .. 2) of Character;

   type Record_Type is
      record
         Key        : KeyString;
         HashAddr   : Integer := 0;
         ProbeCount : Integer := 0;
      end record;


   package Linear_IO_Sum    is new Ada.Direct_IO(Record_Type);
   package Linear_IO_Burris is new Ada.Direct_IO(Record_Type);
   package Random_IO_Sum    is new Ada.Direct_IO(Record_Type);
   package Random_IO_Burris is new Ada.Direct_IO(Record_Type);

   Linear_File_Sum    : Linear_IO_Sum.File_Type;
   Linear_File_Burris : Linear_IO_Burris.File_Type;
   Random_File_Sum    : Random_IO_Sum.File_Type;
   Random_File_Burris : Random_IO_Burris.File_Type;
   Text_Keys : Ada.Text_IO.File_Type;

   Empty_Key : constant KeyString := (others => ' ');
   Key_List  : array(1..200) of KeyString;



   -- === Functions ===
   function To_KeyString(S: String) return KeyString is
      K : KeyString := (others => ' ');
   begin
      for I in S'Range loop
         exit when I > 16;
         K(I) := S(I);
      end loop;
      return K;
   end;

   function Sum_ASCII(Key: KeyString) return Integer is
      Sum : Integer := 0;
   begin
      for I in Key'Range loop
         if Key(I) /= ' ' then
            Sum := Sum + Character'Pos(Key(I));
         else
            exit; -- Stop at first blank (early termination)
         end if;
      end loop;
      return Sum;
   end Sum_ASCII;

   function Hash(Key: KeyString) return Integer is
   begin
      return (Sum_ASCII(Key) mod Hash_Table_Size) + 1;
   end;



   -- Function to convert two characters to an integer

   function To_Int(T : Two_Chars) return Long_Integer is
      Char1_Pos_Str : String := Trim(Integer'Image(Character'Pos(T(1))), Left);
      Char2_Pos_Str : String := Trim(Integer'Image(Character'Pos(T(2))), Left);
      Combined_String : String := Char1_Pos_Str & Char2_Pos_Str;
   begin
      return Long_Integer'Value(Combined_String);
   exception
      when others =>
         raise Constraint_Error with " Value too large.";
   end To_Int;

   function Burris_Hash(Key : KeyString) return Positive_Count is
      v2 : Integer;
      v34, v67 : Two_Chars;  -- Two_Chars for the 2-char combination
      combined_v34, combined_v67 : Long_Integer;  -- Use Long_Integer for larger values
      sum : Float;
      result : Integer;
   begin
      -- ASCII of second character (Key(2))
      v2 := Character'Pos(Key(2));

      -- For characters 3 and 4, combine them into a Two_Chars for "CD"
      v34(1) := Key(3);
      v34(2) := Key(4);

      -- For characters 6 and 7, combine them into a Two_Chars for "FG"
      v67(1) := Key(6);
      v67(2) := Key(7);

      -- Convert the combined ASCII values of "CD" and "FG" to Long_Integer
      combined_v34 := To_Int(v34);  -- e.g., "CD" -> 6768 (ASCII values 67 and 68)
      combined_v67 := To_Int(v67);  -- e.g., "FG" -> 7071 (ASCII values 70 and 71)

      -- Apply the hash formula
      sum := Float(v2) + Float(combined_v67) / Float(2**13) + Float(combined_v34) / 100.0 - 23.0;

      -- Mod by table size
      result := Integer(sum) mod Hash_Table_Size;
      if result <= 0 then
         result := result + Hash_Table_Size;
      end if;

      return Positive_Count(result);
   end Burris_Hash;











   function Is_Empty(Rec: Record_Type) return Boolean is
   begin
      return Rec.Key = Empty_Key;
   end;

   -- === Insert Procedures ===
   procedure Insert_Linear_Sum(Key: KeyString) is
      Rec : Record_Type;
      Addr : Integer := Hash(Key);
      Probes : Integer := 1;
   begin
      loop
         Linear_IO_Sum.Read(Linear_File_Sum, Rec, Linear_IO_Sum.Positive_Count(Addr));
         if Is_Empty(Rec) then
            Rec := (Key, Addr, Probes);
            Linear_IO_Sum.Write(Linear_File_Sum, Rec, Linear_IO_Sum.Positive_Count(Addr));
            exit;
         else
            Addr := (Addr mod Hash_Table_Size) + 1;
            Probes := Probes + 1;
         end if;
      end loop;
   end;

   procedure Insert_Linear_Burris(Key: KeyString) is
      Rec : Record_Type;
      Addr : Integer := Integer(Burris_Hash(Key));
      Probes : Integer := 1;
   begin
      loop
         Linear_IO_Burris.Read(Linear_File_Burris, Rec, Linear_IO_Burris.Positive_Count(Addr));
         if Is_Empty(Rec) then
            Rec := (Key, Addr, Probes);
            Linear_IO_Burris.Write(Linear_File_Burris, Rec, Linear_IO_Burris.Positive_Count(Addr));
            exit;
         else
            Addr := (Addr mod Hash_Table_Size) + 1;
            Probes := Probes + 1;
         end if;
      end loop;
   end;

   procedure Insert_Random_Sum(Key: KeyString) is
      Rec : Record_Type;
      Addr : Integer := Hash(Key);
      Probes : Integer := 1;
      Step : Integer;
      R:Integer:=1;
   begin
      loop
         Random_IO_Sum.Read(Random_File_Sum, Rec, Random_IO_Sum.Positive_Count(Addr));
         if Is_Empty(Rec) then
            Rec := (Key, Addr, Probes);
            Random_IO_Sum.Write(Random_File_Sum, Rec, Random_IO_Sum.Positive_Count(Addr));
            exit;
         else
            R := (5 * R) mod (2 ** (7 + 2));
            Step := R / 4;
            if Step = 0 then
               Step := 1;
            end if;

            Addr := (Addr + Step) mod Hash_Table_Size;
            if Addr = 0 then
               Addr := 1;
            end if;

            Probes := Probes + 1;
         end if;
      end loop;

   end;

   procedure Insert_Random_Burris(Key : KeyString) is
      Rec    : Record_Type;
      Addr   : Integer := Integer(Burris_Hash(Key));
      Probes : Integer := 1;
      Step   : Integer;
      R      : Integer := 1;
   begin
      loop
         Random_IO_Burris.Read(Random_File_Burris, Rec, Random_IO_Burris.Positive_Count(Addr));

         if Is_Empty(Rec) then
            Rec := (Key, Addr, Probes);
            Random_IO_Burris.Write(Random_File_Burris, Rec, Random_IO_Burris.Positive_Count(Addr));
            exit;
         else
            -- Generate the next pseudo-random number
            R := (5 * R) mod (2 ** (7 + 2));  -- 2**9 = 512
            Step := R / 4;                   -- Scale back to [0..127]

            if Step = 0 then
               Step := 1;
            end if;

            Addr := (Addr + Step) mod Hash_Table_Size;
            if Addr = 0 then
               Addr := 1;
            end if;

            Probes := Probes + 1;
         end if;
      end loop;
   end Insert_Random_Burris;

   -- === Dump Full Table ===
   procedure Dump_Tables is
      Rec1, Rec2, Rec3, Rec4 : Record_Type;
   begin

      Put_Line("| Addr | Linear Sum Key    | L.Sum Probes  |Linear Burris Key | L.Burris Probes | Random Sum Key | R.Sum Probes   | Random Burris Key | R.Burris Probes       |");
      Put_Line("$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$");
      for I in 1..Hash_Table_Size loop
         Linear_IO_Sum.Read(Linear_File_Sum, Rec1, Linear_IO_Sum.Positive_Count(I));
         Linear_IO_Burris.Read(Linear_File_Burris, Rec2, Linear_IO_Burris.Positive_Count(I));
         Random_IO_Sum.Read(Random_File_Sum, Rec3, Random_IO_Sum.Positive_Count(I));
         Random_IO_Burris.Read(Random_File_Burris, Rec4, Random_IO_Burris.Positive_Count(I));

         Put("| "); Put(I, 4); Put(" | ");
         for J in Rec1.Key'Range loop Put(Rec1.Key(J)); end loop;
         Put(" | "); Put(Rec1.ProbeCount, 15); Put(" | ");
         for J in Rec2.Key'Range loop Put(Rec2.Key(J)); end loop;
         Put(" | "); Put(Rec2.ProbeCount, 15); Put(" | ");
         for J in Rec3.Key'Range loop Put(Rec3.Key(J)); end loop;
         Put(" | "); Put(Rec3.ProbeCount, 15); Put(" | ");
         for J in Rec4.Key'Range loop Put(Rec4.Key(J)); end loop;
         Put(" | "); Put(Rec4.ProbeCount, 15); Put(" |");
         New_Line;
      end loop;
      Put_Line("$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$");
   end Dump_Tables;

   -- === Show Statistics ===
procedure Show_Stats(Num_Keys : Integer) is
   Rec : Record_Type;
   Total, Min_Probes, Max_Probes, Probes : Integer;
   Addr, Step, R : Integer;

      procedure Print_Stats(Label : String; Use_Linear : Boolean; Use_Sum : Boolean) is
      begin
         Total := 0;
         Min_Probes := Integer'Last;
         Max_Probes := Integer'First;

         for I in 1 .. Num_Keys loop
            -- Choose hash function
            if Use_Sum then
               Addr := Hash(Key_List(I));
            else
               Addr := Integer(Burris_Hash(Key_List(I)));
            end if;

            Probes := 1;
            R := 1;

            loop
               -- Choose correct file to read from
               if Use_Linear and Use_Sum then
                  Linear_IO_Sum.Read(Linear_File_Sum, Rec, Linear_IO_Sum.Positive_Count(Addr));
               elsif Use_Linear then
                  Linear_IO_Burris.Read(Linear_File_Burris, Rec, Linear_IO_Burris.Positive_Count(Addr));
               elsif Use_Sum then
                  Random_IO_Sum.Read(Random_File_Sum, Rec, Random_IO_Sum.Positive_Count(Addr));
               else
                  Random_IO_Burris.Read(Random_File_Burris, Rec, Random_IO_Burris.Positive_Count(Addr));
               end if;

               exit when Rec.Key = Key_List(I) or Is_Empty(Rec);

               if Use_Linear then
                  Addr := (Addr mod Hash_Table_Size) + 1;
               else
                  R := (5 * R) mod 512;
                  Step := R / 4;
                  if Step = 0 then Step := 1; end if;
                  Addr := (Addr + Step) mod Hash_Table_Size;
                  if Addr = 0 then Addr := 1; end if;
               end if;

               Probes := Probes + 1;
            end loop;

            Total := Total + Probes;
            if Probes < Min_Probes then Min_Probes := Probes; end if;
            if Probes > Max_Probes then Max_Probes := Probes; end if;
         end loop;

         Put_Line(Label & " Min=" & Integer'Image(Min_Probes) &
                    " Max=" & Integer'Image(Max_Probes) &
                    " Avg=" & Float'Image(Float(Total) / Float(Num_Keys)));
      end;

   begin
      -- Sum Hash
      Print_Stats("Linear Sum Hash", True, True);
      Print_Stats("Random Sum Hash", False, True);

      -- Burris Hash
      Print_Stats("Linear Burris Hash", True, False);
      Print_Stats("Random Burris Hash", False, False);
   end Show_Stats;



begin


   -- Open and load 200 keys
   Ada.Text_IO.Open(Text_Keys, Ada.Text_IO.In_File, "C:\\Users\\kaden\\Downloads\\Words200D16 (1).txt");
   for I in 1..200 loop
      declare
         Line : String(1..80);
         Len : Natural;
      begin
         Ada.Text_IO.Get_Line(Text_Keys, Line, Len);
         Key_List(I) := To_KeyString(Line(1..Len));
      end;
   end loop;
   Ada.Text_IO.Close(Text_Keys);

   -- Create all 4 hash tables
   Linear_IO_Sum.Create(Linear_File_Sum, Linear_IO_Sum.Inout_File, "LinearSum.dat");
   Linear_IO_Burris.Create(Linear_File_Burris, Linear_IO_Burris.Inout_File, "LinearBurris.dat");
   Random_IO_Sum.Create(Random_File_Sum, Random_IO_Sum.Inout_File, "RandomSum.dat");
   Random_IO_Burris.Create(Random_File_Burris, Random_IO_Burris.Inout_File, "RandomBurris.dat");

   -- Initialize files
   declare
      Blank : Record_Type := (Key => Empty_Key, HashAddr => 0, ProbeCount => 0);
   begin
      for I in 1..Hash_Table_Size loop
         Linear_IO_Sum.Write(Linear_File_Sum, Blank, Linear_IO_Sum.Positive_Count(I));
         Linear_IO_Burris.Write(Linear_File_Burris, Blank, Linear_IO_Burris.Positive_Count(I));
         Random_IO_Sum.Write(Random_File_Sum, Blank, Random_IO_Sum.Positive_Count(I));
         Random_IO_Burris.Write(Random_File_Burris, Blank, Random_IO_Burris.Positive_Count(I));
      end loop;
   end;

   -- Insert first 85 keys
   for I in 1..25 loop
      Insert_Linear_Sum(Key_List(I));
      Insert_Linear_Burris(Key_List(I));
      Insert_Random_Sum(Key_List(I));
      Insert_Random_Burris(Key_List(I));
   end loop;
   Put_Line("first 25");
   Put_Line("theoretical random and linear is 1.11");
  Show_Stats(25);  -- after inserting 1..25
   for I in 26..50 loop
      Insert_Linear_Sum(Key_List(I));
      Insert_Linear_Burris(Key_List(I));
      Insert_Random_Sum(Key_List(I));
      Insert_Random_Burris(Key_List(I));
   end loop;
   Put_Line("next 25");
   Put_Line("theoretical random is 1.27");
   Put_Line("theoretical linear is 1.32");
   Show_Stats(50);  -- after inserting 26..50

   for I in 51..85 loop
      Insert_Linear_Sum(Key_List(I));
      Insert_Linear_Burris(Key_List(I));
      Insert_Random_Sum(Key_List(I));
      Insert_Random_Burris(Key_List(I));
   end loop;

   Put_Line("next 35");
   Put_Line("theoretical Linear Probe 1.99");
   Put_Line("theoretical random probe 1.64");

   Show_Stats(85);  -- final stats after all inserted

   Dump_Tables;



   -- Close all files
   Linear_IO_Sum.Close(Linear_File_Sum);
   Linear_IO_Burris.Close(Linear_File_Burris);
   Random_IO_Sum.Close(Random_File_Sum);
   Random_IO_Burris.Close(Random_File_Burris);

end Compare_Hash_Probing;

