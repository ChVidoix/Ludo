with Ada.Numerics.Float_Random;
use Ada.Numerics.Float_Random;
with Ada.Numerics.Elementary_Functions;
use Ada.Numerics.Elementary_Functions;
with Ada.Exceptions;
use Ada.Exceptions;
with Ada;
use Ada;
with Ada.Text_IO;
use Ada.Text_IO;
with Ada.Strings.Unbounded;
use Ada.Strings.Unbounded;
with Ada.Text_IO.Unbounded_IO;
use Ada.Text_IO.Unbounded_IO;
with ada.numerics.discrete_random;
with Ada.Real_Time;
use Ada.Real_Time;

procedure ludo is
   
   type Point is record
      X : Integer;
      Y : Integer;
   end record;

   type Pawn is record
      ID        : Integer;
      Name      : Ada.Strings.Unbounded.Unbounded_String;
      Coord     : Point;
      Road      : Integer;
      Is_Active : Boolean;
   end record;

   type Field is record
      Label        : Ada.Strings.Unbounded.Unbounded_String;
      Is_Available : Boolean;
   end record;
   
   type Pawn_List is array(1..4) of Pawn;
   type My_Array is array(Integer range <>, Integer range <>) of Field;
   type My_Vector is array(Integer range <>) of Point;
   
   Board  : My_Array(1..11, 1..11);
   Trace  : My_Vector(1..40);
   Okres  : constant Ada.Real_Time.Time_Span := Ada.Real_Time.Milliseconds(80);
   waiter : Integer := 0;
   Tmp_W  : Integer;
   Coords : Point;
   c      : Character;
   rand   : Integer;
   
   procedure Clear_Screen is
   begin
      Ada.Text_IO.Put(ASCII.ESC & "[2J");
   end Clear_Screen;
   
   
   procedure Print_Board (Tab : My_Array) is
   begin
      Clear_Screen;
      for I in Tab'Range(1) loop
         for J in Tab'Range(2) loop
            Put(Tab(I,J).Label);
         end loop;
         Put_Line("");
      end loop;
   end Print_Board;

   task Red_Player;
   task Blue_Player;
   task Yellow_Player;
   task Green_Player;
   task Game is
      entry Start;
      entry Stop;
      entry Set_Pawns(Pawns : Pawn_List);
   end Game;

   
   function Dice_Roll return Integer is
      type randRange is new Integer range 1..6;
      package Rand_Int is new ada.numerics.discrete_random(randRange);
      use Rand_Int;
      gen : Rand_Int.Generator;
      num : Integer;
   begin
      Put_Line("Press enter to roll the dice");
      Get_Immediate(c);
      c := Character'Val(0);
      
      reset(gen);
      num := Integer(Rand_Int.Random(gen));
      return num;
   end Dice_Roll;

   
   procedure Launch_Pawn(Pawns : in out Pawn_List; Num : in out Integer) is
   begin
               
      for I in Integer range 1..4 loop
         if Pawns(I).Is_Active = True then
            goto Continue;
         else
            
            Pawns(I).Is_Active := True;
            Board(Coords.X, Coords.Y) := (Pawns(I).Name, False);
            exit;
            
         end if;
         <<Continue>>
      end loop;
   end Launch_Pawn;
   
   
   procedure Decide(Players_Pawns : in out Pawn_List; Num : in out Integer) is
      Decision : Integer;
   begin
      
      for I in Integer range 1..4 loop
         if Players_Pawns(I).Is_Active = True or Num = 6 then
            goto Can_Decide;
         end if;
      end loop;
      
      Put_Line("Sorry, you don't have any moves");
      Put_Line("Press enter to give the dice to the next player");
      Get_Immediate(c);
      c := Character'Val(0);
      waiter := ((Players_Pawns(1).ID+1) mod 4);
      goto Next_Player;
      
      
      <<Can_Decide>>
      Print_Board(Board);
      Put_Line("Give number of your choice");
      Put_Line("1. Move pawn");
      if Num = 6 then
         Put_Line("2. Introduce new pawn to the game");
      end if;
      
      Decision := Integer'Value(Get_Line);
      
      case Decision is
         when 1 => 
            Put_Line("Which pawn would you like to move? Give the name");
            null;
         when 2 =>
            Coords := Trace(Integer(10*(Players_Pawns(1).ID mod 4) + 1));
            if Board(Coords.X, Coords.Y).Is_Available = True then
               Launch_Pawn(Players_Pawns, Num);
            else
               goto Can_Decide;
            end if;   
               
         when others =>
            Put_Line("Please, give correct number");
            goto Can_Decide;
      end case;
      
      Put_Line("Press enter to give the dice to the next player");
      Get_Immediate(c);
      c := Character'Val(0);
      waiter := (Players_Pawns(1).ID+1) mod 4;
      <<Next_Player>>
   end Decide;
   
   
   procedure Move_Pawn(P : Pawn) is
   begin
      null;
   end Move_Pawn;
   
   
   procedure Introduce_Player(P : Pawn) is
   begin   
      Put_Line(" ");
      case p.ID is
         when 0      => Put_Line("Red Player Turn");
         when 1      => Put_Line("Blue Player Turn");
         when 2      => Put_Line("Yellow Player Turn");
         when 3      => Put_Line("Green Player Turn");
         when others => null;
      end case;
      Put_Line(" ");
   end Introduce_Player;
   
   
   procedure Player_Turn(Pawns: in out Pawn_List) is
   begin
      if waiter = (Pawns(1).ID mod 4) then
         
         Print_Board(Board);
         Introduce_Player(Pawns(1));
         
         rand := Dice_Roll;
         Put_Line("Your number: " & rand'img);
         Tmp_W := Pawns(1).ID mod 4;
         
         Decide(Pawns, rand);

      else
         delay 1.0;
      end if;
   end Player_Turn;
   
   
   task body Red_Player is
      
      R1    : Pawn := (0, Ada.Strings.Unbounded.To_Unbounded_String ("R1"), (1, 1), 0, False);
      R2    : Pawn := (4, Ada.Strings.Unbounded.To_Unbounded_String ("R2"), (1, 2), 0, False);
      R3    : Pawn := (8, Ada.Strings.Unbounded.To_Unbounded_String ("R3"), (2, 1), 0, False);
      R4    : Pawn := (12, Ada.Strings.Unbounded.To_Unbounded_String ("R4"), (2, 2), 0, False);
      Red_Pawns : Pawn_List := (R1, R2, R3, R4);
   begin
      Game.Set_Pawns(Red_Pawns);
      loop
         Player_Turn(Red_Pawns);
      end loop;
   end Red_Player;

   
   task body Blue_Player is
      B1   : Pawn := (1, Ada.Strings.Unbounded.To_Unbounded_String ("B1"), (1, 10), 0, False);
      B2   : Pawn := (5, Ada.Strings.Unbounded.To_Unbounded_String ("B2"), (1, 11), 0, False);
      B3   : Pawn := (9, Ada.Strings.Unbounded.To_Unbounded_String ("B3"), (2, 10), 0, False);
      B4   : Pawn := (13, Ada.Strings.Unbounded.To_Unbounded_String ("B4"), (2, 11), 0, False);
      Blue_Pawns : Pawn_List := (B1, B2, B3, B4);
   begin
      Game.Set_Pawns(Blue_Pawns);
      loop
         Player_Turn(Blue_Pawns);
      end loop;
      
   end Blue_Player;

   
   task body Yellow_Player is
      Y1 : Pawn := (2,  Ada.Strings.Unbounded.To_Unbounded_String ("Y1"), (10, 10), 0, False);
      Y2 : Pawn := (6, Ada.Strings.Unbounded.To_Unbounded_String ("Y2"), (10, 11), 0, False);
      Y3 : Pawn := (10, Ada.Strings.Unbounded.To_Unbounded_String ("Y3"), (11, 10), 0, False);
      Y4 : Pawn := (14, Ada.Strings.Unbounded.To_Unbounded_String ("Y4"), (11, 11), 0, False);
      Yellow_Pawns : Pawn_List := (Y1, Y2, Y3, Y4);
   begin
      Game.Set_Pawns(Yellow_Pawns);
      loop
         Player_Turn(Yellow_Pawns);
      end loop;
   end Yellow_Player;

   
   task body Green_Player is
      G1 : Pawn := (3, Ada.Strings.Unbounded.To_Unbounded_String ("G1"), (10, 1), 0, False);
      G2 : Pawn := (7, Ada.Strings.Unbounded.To_Unbounded_String ("G2"), (10, 2), 0, False);
      G3 : Pawn := (11, Ada.Strings.Unbounded.To_Unbounded_String ("G3"), (11, 1), 0, False);
      G4 : Pawn := (15, Ada.Strings.Unbounded.To_Unbounded_String ("G4"), (11, 2), 0, False);
      Green_Pawns : Pawn_List := (G1, G2, G3, G4);
   begin
      Game.Set_Pawns(Green_Pawns);
      loop
         Player_Turn(Green_Pawns);
      end loop;
   end Green_Player;
   
   
   task body Game is
   begin
      accept Start;

      for I in Board'Range(1) loop
         for J in Board'Range(2) loop
            Board(I,J) := (Ada.Strings.Unbounded.To_Unbounded_String (" O"), True);
         end loop;
      end loop;
      
      Board (1,3) := (Ada.Strings.Unbounded.To_Unbounded_String ("  "), True);
      Board (1,4) := (Ada.Strings.Unbounded.To_Unbounded_String ("  "), True);
      Board (1,8) := (Ada.Strings.Unbounded.To_Unbounded_String ("  "), True);
      Board (1,9) := (Ada.Strings.Unbounded.To_Unbounded_String ("  "), True);

      Board (2,3) := (Ada.Strings.Unbounded.To_Unbounded_String ("  "), True);
      Board (2,4) := (Ada.Strings.Unbounded.To_Unbounded_String ("  "), True);
      Board (2,8) := (Ada.Strings.Unbounded.To_Unbounded_String ("  "), True);
      Board (2,9) := (Ada.Strings.Unbounded.To_Unbounded_String ("  "), True);

      Board (3,1) := (Ada.Strings.Unbounded.To_Unbounded_String ("  "), True);
      Board (3,2) := (Ada.Strings.Unbounded.To_Unbounded_String ("  "), True);
      Board (3,3) := (Ada.Strings.Unbounded.To_Unbounded_String ("  "), True);
      Board (3,4) := (Ada.Strings.Unbounded.To_Unbounded_String ("  "), True);
      Board (3,8) := (Ada.Strings.Unbounded.To_Unbounded_String ("  "), True);
      Board (3,9) := (Ada.Strings.Unbounded.To_Unbounded_String ("  "), True);
      Board (3,10) := (Ada.Strings.Unbounded.To_Unbounded_String ("  "), True);
      Board (3,11) := (Ada.Strings.Unbounded.To_Unbounded_String ("  "), True);

      Board (4,1) := (Ada.Strings.Unbounded.To_Unbounded_String ("  "), True);
      Board (4,2) := (Ada.Strings.Unbounded.To_Unbounded_String ("  "), True);
      Board (4,3) := (Ada.Strings.Unbounded.To_Unbounded_String ("  "), True);
      Board (4,4) := (Ada.Strings.Unbounded.To_Unbounded_String ("  "), True);
      Board (4,8) := (Ada.Strings.Unbounded.To_Unbounded_String ("  "), True);
      Board (4,9) := (Ada.Strings.Unbounded.To_Unbounded_String ("  "), True);
      Board (4,10) := (Ada.Strings.Unbounded.To_Unbounded_String ("  "), True);
      Board (4,11) := (Ada.Strings.Unbounded.To_Unbounded_String ("  "), True);

      Board (8,1) := (Ada.Strings.Unbounded.To_Unbounded_String ("  "), True);
      Board (8,2) := (Ada.Strings.Unbounded.To_Unbounded_String ("  "), True);
      Board (8,3) := (Ada.Strings.Unbounded.To_Unbounded_String ("  "), True);
      Board (8,4) := (Ada.Strings.Unbounded.To_Unbounded_String ("  "), True);
      Board (8,8) := (Ada.Strings.Unbounded.To_Unbounded_String ("  "), True);
      Board (8,9) := (Ada.Strings.Unbounded.To_Unbounded_String ("  "), True);
      Board (8,10) := (Ada.Strings.Unbounded.To_Unbounded_String ("  "), True);
      Board (8,11) := (Ada.Strings.Unbounded.To_Unbounded_String ("  "), True);

      Board (9,1) := (Ada.Strings.Unbounded.To_Unbounded_String ("  "), True);
      Board (9,2) := (Ada.Strings.Unbounded.To_Unbounded_String ("  "), True);
      Board (9,3) := (Ada.Strings.Unbounded.To_Unbounded_String ("  "), True);
      Board (9,4) := (Ada.Strings.Unbounded.To_Unbounded_String ("  "), True);
      Board (9,8) := (Ada.Strings.Unbounded.To_Unbounded_String ("  "), True);
      Board (9,9) := (Ada.Strings.Unbounded.To_Unbounded_String ("  "), True);
      Board (9,10) := (Ada.Strings.Unbounded.To_Unbounded_String ("  "), True);
      Board (9,11) := (Ada.Strings.Unbounded.To_Unbounded_String ("  "), True);

      Board (10,3) := (Ada.Strings.Unbounded.To_Unbounded_String ("  "), True);
      Board (10,4) := (Ada.Strings.Unbounded.To_Unbounded_String ("  "), True);
      Board (10,8) := (Ada.Strings.Unbounded.To_Unbounded_String ("  "), True);
      Board (10,9) := (Ada.Strings.Unbounded.To_Unbounded_String ("  "), True);

      Board (11,3) := (Ada.Strings.Unbounded.To_Unbounded_String ("  "), True);
      Board (11,4) := (Ada.Strings.Unbounded.To_Unbounded_String ("  "), True);
      Board (11,8) := (Ada.Strings.Unbounded.To_Unbounded_String ("  "), True);
      Board (11,9) := (Ada.Strings.Unbounded.To_Unbounded_String ("  "), True);

      Board (2,6) := (Ada.Strings.Unbounded.To_Unbounded_String (" +"), True);
      Board (3,6) := (Ada.Strings.Unbounded.To_Unbounded_String (" +"), True);
      Board (4,6) := (Ada.Strings.Unbounded.To_Unbounded_String (" +"), True);
      Board (5,6) := (Ada.Strings.Unbounded.To_Unbounded_String (" +"), True);

      Board (6,2) := (Ada.Strings.Unbounded.To_Unbounded_String (" +"), True);
      Board (6,3) := (Ada.Strings.Unbounded.To_Unbounded_String (" +"), True);
      Board (6,4) := (Ada.Strings.Unbounded.To_Unbounded_String (" +"), True);
      Board (6,5) := (Ada.Strings.Unbounded.To_Unbounded_String (" +"), True);

      Board (6,7) := (Ada.Strings.Unbounded.To_Unbounded_String (" +"), True);
      Board (6,8) := (Ada.Strings.Unbounded.To_Unbounded_String (" +"), True);
      Board (6,9) := (Ada.Strings.Unbounded.To_Unbounded_String (" +"), True);
      Board (6,10) := (Ada.Strings.Unbounded.To_Unbounded_String (" +"), True);

      Board (7,6) := (Ada.Strings.Unbounded.To_Unbounded_String (" +"), True);
      Board (8,6) := (Ada.Strings.Unbounded.To_Unbounded_String (" +"), True);
      Board (9,6) := (Ada.Strings.Unbounded.To_Unbounded_String (" +"), True);
      Board (10,6) := (Ada.Strings.Unbounded.To_Unbounded_String (" +"), True);

      Board (6,6) := (Ada.Strings.Unbounded.To_Unbounded_String (" X"), True);

      Trace(1) := (5,1);
      Trace(2) := (5,2);
      Trace(3) := (5,3);
      Trace(4) := (5,4);
      Trace(5) := (5,5);
      Trace(6) := (4,5);
      Trace(7) := (3,5);
      Trace(8) := (2,5);
      Trace(9) := (1,5);
      Trace(10) := (1,6);
      Trace(11) := (1,7);
      Trace(12) := (2,7);
      Trace(13) := (3,7);
      Trace(14) := (4,7);
      Trace(15) := (5,7);
      Trace(16) := (5,8);
      Trace(17) := (5,9);
      Trace(18) := (5,10);
      Trace(19) := (5,11);
      Trace(20) := (6,11);
      Trace(21) := (7,11);
      Trace(22) := (7,10);
      Trace(23) := (7,9);
      Trace(24) := (7,8);
      Trace(25) := (7,7);
      Trace(26) := (8,7);
      Trace(27) := (9,7);
      Trace(28) := (10,7);
      Trace(29) := (11,7);
      Trace(30) := (11,6);
      Trace(31) := (11,5);
      Trace(32) := (10,5);
      Trace(33) := (9,5);
      Trace(34) := (8,5);
      Trace(35) := (7,5);
      Trace(36) := (7,4);
      Trace(37) := (7,3);
      Trace(38) := (7,2);
      Trace(39) := (7,1);
      Trace(40) := (6,1);
      
      loop
         select
            accept Set_Pawns(Pawns: Pawn_List) do
               for I in Integer range 1..4 loop
                  Board(Pawns(I).Coord.X, Pawns(I).Coord.Y) := (Pawns(I).Name, False); 
               end loop;
            end Set_Pawns;
         or
            accept Stop;
            exit;
         end select;
      end loop;
   end Game;
begin
   Game.Start;
   Game.Stop;
end ludo;
