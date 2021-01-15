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

procedure ludo is

   type Nowa_Tablica is array(Integer range <>, Integer range <>) of Ada.Strings.Unbounded.Unbounded_String;
   type Pionek is record
      ID : Integer;
      Pawn_Name : Ada.Strings.Unbounded.Unbounded_String;
      X : Integer;
      Y : Integer;
      Road : Integer;
   end record;
   
   task Red_Player;
   task Blue_Player;
   task Yellow_Player;
   task Green_Player;
   task Game is
      entry Start;
      entry Stop;
      entry Launch_Pawn;
   end Game;
   procedure Printer (NTab : in Nowa_Tablica);
   procedure Printer (NTab : in Nowa_Tablica) is
   begin
      for I in NTab'Range(1) loop
         for J in NTab'Range(2) loop
            Put(NTab(I,J));
         end loop;
         Put_Line("");
      end loop;
   end Printer;
   
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
   
      --  procedure set_Pawns (n1: in Pionek; n2 : in Pionek; n3 : in Pionek; n4 : in Pionek) is
   --  begin
   --     Board(n1.X, n1.Y) := n1.Pawn_Name;
   --     Board(n2.X, n2.Y) := n2.Pawn_Name;
   --     Board(n3.X, n3.Y) := n3.Pawn_Name;
   --     Board(n4.X, n4.Y) := n4.Pawn_Name;
   --  end set_Pawns;
   
   task body Red_Player is
      R1 : Pionek := (1, Ada.Strings.Unbounded.To_Unbounded_String ("R1"), 1, 1, 0);
      R2 : Pionek := (2, Ada.Strings.Unbounded.To_Unbounded_String ("R2"), 1, 2, 0);
      R3 : Pionek := (3, Ada.Strings.Unbounded.To_Unbounded_String ("R3"), 2, 1, 0);
      R4 : Pionek := (4, Ada.Strings.Unbounded.To_Unbounded_String ("R4"), 2, 2, 0);
   begin
      null;
      --  set_Pawns(R1, R2, R3, R4);
   end Red_Player;

   task body Blue_Player is
      B1 : Pionek := (5, Ada.Strings.Unbounded.To_Unbounded_String ("B1"), 1, 10, 0);
      B2 : Pionek := (6, Ada.Strings.Unbounded.To_Unbounded_String ("B2"), 1, 11, 0);
      B3 : Pionek := (7, Ada.Strings.Unbounded.To_Unbounded_String ("B3"), 2, 10, 0);
      B4 : Pionek := (8, Ada.Strings.Unbounded.To_Unbounded_String ("B4"), 2, 11, 0);
   begin
      null;
   end Blue_Player;

   task body Yellow_Player is
      Y1 : Pionek := (9,  Ada.Strings.Unbounded.To_Unbounded_String ("Y1"), 10, 1, 0);
      Y2 : Pionek := (10, Ada.Strings.Unbounded.To_Unbounded_String ("Y2"), 10, 2, 0);
      Y3 : Pionek := (11, Ada.Strings.Unbounded.To_Unbounded_String ("Y3"), 11, 1, 0);
      Y4 : Pionek := (12, Ada.Strings.Unbounded.To_Unbounded_String ("Y4"), 11, 2, 0);
   begin
      null;
   end Yellow_Player;

   task body Green_Player is
      G1 : Pionek := (13, Ada.Strings.Unbounded.To_Unbounded_String ("G1"), 10, 10, 0);
      G2 : Pionek := (14, Ada.Strings.Unbounded.To_Unbounded_String ("G2"), 10, 11, 0);
      G3 : Pionek := (15, Ada.Strings.Unbounded.To_Unbounded_String ("G3"), 11, 10, 0);
      G4 : Pionek := (16, Ada.Strings.Unbounded.To_Unbounded_String ("G4"), 11, 11, 0);
   begin
      null;
   end Green_Player;
   task body Game is
      Board  : Nowa_Tablica(1..11, 1..11);
      liczba : Integer;
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
   
    
      Printer(Board);
      liczba := generate_random_number;
      Put_Line(liczba'Img);
      loop
         select
            accept Launch_Pawn do
               null;
            end Launch_Pawn;
         or
            accept Stop;
            exit;
         end select;
      end loop;
   end Game;
begin
   null;
end ludo;
