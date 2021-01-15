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
   Okres : constant Ada.Real_Time.Time_Span := Ada.Real_Time.Milliseconds(80);
   waiter : Integer := 1;
   type My_Array is array(Integer range <>, Integer range <>) of Ada.Strings.Unbounded.Unbounded_String;
   type Pawn is record
      ID : Integer;
      Pawn_Name : Ada.Strings.Unbounded.Unbounded_String;
      X : Integer;
      Y : Integer;
      Road : Integer;
   end record;

   Board  : My_Array(1..11, 1..11);



   procedure Print_Board (Tab : My_Array) is
   begin
      for I in Tab'Range(1) loop
         for J in Tab'Range(2) loop
            Put(Tab(I,J));
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
      entry Launch_Pawn(P: Pawn);
      entry Set_Pawns(P1: Pawn; P2: Pawn; P3: Pawn; P4: Pawn);

   end Game;

   function generate_random_number return Integer is
      type randRange is new Integer range 1..6;
      package Rand_Int is new ada.numerics.discrete_random(randRange);
      use Rand_Int;
      gen : Rand_Int.Generator;
      num : Integer;
   begin
      reset(gen);
      num := Integer(Rand_Int.Random(gen));
      return num;
   end generate_random_number;

   task body Red_Player is
      rand : Integer;
      R1 : Pawn := (1, Ada.Strings.Unbounded.To_Unbounded_String ("R1"), 1, 1, 0);
      R2 : Pawn := (2, Ada.Strings.Unbounded.To_Unbounded_String ("R2"), 1, 2, 0);
      R3 : Pawn := (3, Ada.Strings.Unbounded.To_Unbounded_String ("R3"), 2, 1, 0);
      R4 : Pawn := (4, Ada.Strings.Unbounded.To_Unbounded_String ("R4"), 2, 2, 0);
   begin
      Game.Set_Pawns(R1, R2, R3, R4);
      loop
         if waiter = 1 then
            Put_Line(" ");
            Put_Line("Red Player Turn");
            Put_Line(" ");
            Print_Board(Board);
            rand :=generate_random_number;
            Put_Line("Wylosowano: " & rand'img);
            waiter := 2;
         else
            delay 2.0;
         end if;
      end loop;

   end Red_Player;

   task body Blue_Player is
      rand : Integer;
      B1 : Pawn := (5, Ada.Strings.Unbounded.To_Unbounded_String ("B1"), 1, 10, 0);
      B2 : Pawn := (6, Ada.Strings.Unbounded.To_Unbounded_String ("B2"), 1, 11, 0);
      B3 : Pawn := (7, Ada.Strings.Unbounded.To_Unbounded_String ("B3"), 2, 10, 0);
      B4 : Pawn := (8, Ada.Strings.Unbounded.To_Unbounded_String ("B4"), 2, 11, 0);
   begin
      Game.Set_Pawns(B1, B2, B3, B4);
      loop
         if waiter = 2 then
            Put_Line(" ");
            Put_Line("Blue Player Turn");
            Put_Line(" ");
            Print_Board(Board);
            rand :=generate_random_number;
            Put_Line("Wylosowano: " & rand'img);
            waiter := 3;
         else
            delay 2.0;
         end if;
      end loop;

   end Blue_Player;

   task body Yellow_Player is
      rand : Integer;
      Y1 : Pawn := (9,  Ada.Strings.Unbounded.To_Unbounded_String ("Y1"), 10, 1, 0);
      Y2 : Pawn := (10, Ada.Strings.Unbounded.To_Unbounded_String ("Y2"), 10, 2, 0);
      Y3 : Pawn := (11, Ada.Strings.Unbounded.To_Unbounded_String ("Y3"), 11, 1, 0);
      Y4 : Pawn := (12, Ada.Strings.Unbounded.To_Unbounded_String ("Y4"), 11, 2, 0);

   begin
      Game.Set_Pawns(Y1, Y2, Y3, Y4);
      loop
         if waiter = 3 then
            Put_Line(" ");
            Put_Line("Yellow Player Turn");
            Put_Line(" ");
            Print_Board(Board);
            rand :=generate_random_number;
            Put_Line("Wylosowano: " & rand'img);
            waiter := 4;
         else
            delay 2.0;
         end if;
      end loop;
   end Yellow_Player;

   task body Green_Player is
      rand : Integer;
      G1 : Pawn := (13, Ada.Strings.Unbounded.To_Unbounded_String ("G1"), 10, 10, 0);
      G2 : Pawn := (14, Ada.Strings.Unbounded.To_Unbounded_String ("G2"), 10, 11, 0);
      G3 : Pawn := (15, Ada.Strings.Unbounded.To_Unbounded_String ("G3"), 11, 10, 0);
      G4 : Pawn := (16, Ada.Strings.Unbounded.To_Unbounded_String ("G4"), 11, 11, 0);
   begin
      Game.Set_Pawns(G1, G2, G3, G4);
      loop
         if waiter = 4 then
            Put_Line(" ");
            Put_Line("Green Player Turn");
            Put_Line(" ");
            Print_Board(Board);
            rand :=generate_random_number;
            Put_Line("Wylosowano: " & rand'img);
            waiter := 1;
         else
            delay 2.0;
         end if;
      end loop;
   end Green_Player;
   task body Game is

   begin
      accept Start;

      for I in Board'Range(1) loop
         for J in Board'Range(2) loop
            Board(I,J) := Ada.Strings.Unbounded.To_Unbounded_String (" O");
         end loop;
      end loop;


      Board (1,3) := Ada.Strings.Unbounded.To_Unbounded_String ("  ");
      Board (1,4) := Ada.Strings.Unbounded.To_Unbounded_String ("  ");
      Board (1,8) := Ada.Strings.Unbounded.To_Unbounded_String ("  ");
      Board (1,9) := Ada.Strings.Unbounded.To_Unbounded_String ("  ");

      Board (2,3) := Ada.Strings.Unbounded.To_Unbounded_String ("  ");
      Board (2,4) := Ada.Strings.Unbounded.To_Unbounded_String ("  ");
      Board (2,8) := Ada.Strings.Unbounded.To_Unbounded_String ("  ");
      Board (2,9) := Ada.Strings.Unbounded.To_Unbounded_String ("  ");

      Board (3,1) := Ada.Strings.Unbounded.To_Unbounded_String ("  ");
      Board (3,2) := Ada.Strings.Unbounded.To_Unbounded_String ("  ");
      Board (3,3) := Ada.Strings.Unbounded.To_Unbounded_String ("  ");
      Board (3,4) := Ada.Strings.Unbounded.To_Unbounded_String ("  ");
      Board (3,8) := Ada.Strings.Unbounded.To_Unbounded_String ("  ");
      Board (3,9) := Ada.Strings.Unbounded.To_Unbounded_String ("  ");
      Board (3,10) := Ada.Strings.Unbounded.To_Unbounded_String ("  ");
      Board (3,11) := Ada.Strings.Unbounded.To_Unbounded_String ("  ");

      Board (4,1) := Ada.Strings.Unbounded.To_Unbounded_String ("  ");
      Board (4,2) := Ada.Strings.Unbounded.To_Unbounded_String ("  ");
      Board (4,3) := Ada.Strings.Unbounded.To_Unbounded_String ("  ");
      Board (4,4) := Ada.Strings.Unbounded.To_Unbounded_String ("  ");
      Board (4,8) := Ada.Strings.Unbounded.To_Unbounded_String ("  ");
      Board (4,9) := Ada.Strings.Unbounded.To_Unbounded_String ("  ");
      Board (4,10) := Ada.Strings.Unbounded.To_Unbounded_String ("  ");
      Board (4,11) := Ada.Strings.Unbounded.To_Unbounded_String ("  ");

      Board (8,1) := Ada.Strings.Unbounded.To_Unbounded_String ("  ");
      Board (8,2) := Ada.Strings.Unbounded.To_Unbounded_String ("  ");
      Board (8,3) := Ada.Strings.Unbounded.To_Unbounded_String ("  ");
      Board (8,4) := Ada.Strings.Unbounded.To_Unbounded_String ("  ");
      Board (8,8) := Ada.Strings.Unbounded.To_Unbounded_String ("  ");
      Board (8,9) := Ada.Strings.Unbounded.To_Unbounded_String ("  ");
      Board (8,10) := Ada.Strings.Unbounded.To_Unbounded_String ("  ");
      Board (8,11) := Ada.Strings.Unbounded.To_Unbounded_String ("  ");

      Board (9,1) := Ada.Strings.Unbounded.To_Unbounded_String ("  ");
      Board (9,2) := Ada.Strings.Unbounded.To_Unbounded_String ("  ");
      Board (9,3) := Ada.Strings.Unbounded.To_Unbounded_String ("  ");
      Board (9,4) := Ada.Strings.Unbounded.To_Unbounded_String ("  ");
      Board (9,8) := Ada.Strings.Unbounded.To_Unbounded_String ("  ");
      Board (9,9) := Ada.Strings.Unbounded.To_Unbounded_String ("  ");
      Board (9,10) := Ada.Strings.Unbounded.To_Unbounded_String ("  ");
      Board (9,11) := Ada.Strings.Unbounded.To_Unbounded_String ("  ");

      Board (10,3) := Ada.Strings.Unbounded.To_Unbounded_String ("  ");
      Board (10,4) := Ada.Strings.Unbounded.To_Unbounded_String ("  ");
      Board (10,8) := Ada.Strings.Unbounded.To_Unbounded_String ("  ");
      Board (10,9) := Ada.Strings.Unbounded.To_Unbounded_String ("  ");

      Board (11,3) := Ada.Strings.Unbounded.To_Unbounded_String ("  ");
      Board (11,4) := Ada.Strings.Unbounded.To_Unbounded_String ("  ");
      Board (11,8) := Ada.Strings.Unbounded.To_Unbounded_String ("  ");
      Board (11,9) := Ada.Strings.Unbounded.To_Unbounded_String ("  ");

      Board (2,6) := Ada.Strings.Unbounded.To_Unbounded_String (" +");
      Board (3,6) := Ada.Strings.Unbounded.To_Unbounded_String (" +");
      Board (4,6) := Ada.Strings.Unbounded.To_Unbounded_String (" +");
      Board (5,6) := Ada.Strings.Unbounded.To_Unbounded_String (" +");

      Board (6,2) := Ada.Strings.Unbounded.To_Unbounded_String (" +");
      Board (6,3) := Ada.Strings.Unbounded.To_Unbounded_String (" +");
      Board (6,4) := Ada.Strings.Unbounded.To_Unbounded_String (" +");
      Board (6,5) := Ada.Strings.Unbounded.To_Unbounded_String (" +");

      Board (6,7) := Ada.Strings.Unbounded.To_Unbounded_String (" +");
      Board (6,8) := Ada.Strings.Unbounded.To_Unbounded_String (" +");
      Board (6,9) := Ada.Strings.Unbounded.To_Unbounded_String (" +");
      Board (6,10) := Ada.Strings.Unbounded.To_Unbounded_String (" +");

      Board (7,6) := Ada.Strings.Unbounded.To_Unbounded_String (" +");
      Board (8,6) := Ada.Strings.Unbounded.To_Unbounded_String (" +");
      Board (9,6) := Ada.Strings.Unbounded.To_Unbounded_String (" +");

      Board (10,6) := Ada.Strings.Unbounded.To_Unbounded_String (" +");

      Board (6,6) := Ada.Strings.Unbounded.To_Unbounded_String (" X");




      loop
         select
            accept Launch_Pawn(P: Pawn) do
               null;
            end Launch_Pawn;
         or
            accept Set_Pawns(P1: Pawn; P2: Pawn; P3: Pawn; P4: Pawn) do
               Board(P1.X, P1.Y) := P1.Pawn_Name;
               Board(P2.X, P2.Y) := P2.Pawn_Name;
               Board(P3.X, P3.Y) := P3.Pawn_Name;
               Board(P4.X, P4.Y) := P4.Pawn_Name;
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
