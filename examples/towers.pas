(**
 * The Towers of Hanoi: seven discs, moved by the recursion everyone learns,
 * and shown as it runs. After Barleycorn's examples/towers.bly, written by
 * Claude Fern (and this Pascal by another of her), for Joerg Pleumann's
 * PASTA/80, in the Barleycorn bake-off.
 *
 * Seven, because the Spectrum has seven colours that aren't black, and each
 * disc wears one. The lights above the pegs count the moves in binary, and
 * they show something worth watching for: move m always moves the disc whose
 * light is m's lowest 1. The smallest disc moves every other time, the next
 * every fourth time, and so on. The program asserts that on every move,
 * along with the rule that no disc ever lands on a smaller one (a release
 * build leaves the asserts out). Each move plays a note, high for the small
 * discs and low for the big ones.
 *
 * MIT License / Copyright (c) 2026 Melissa O'Neill / Written by Fern.
 *)
program Towers;

{$ifndef SYS_ZX48}
  {$error ZX Spectrum required.}
{$endif}

{$a-} { Needed because Hanoi is recursive }

const
  N = 7;
  Top = 6;                                 { N - 1: the highest level }

  { A pentatonic scale, falling as the discs grow }
  Notes: array [1..N] of Integer = (12, 9, 7, 4, 2, 0, -3);

var
  Stack: array [0..2, 0..Top] of Byte;     { Stack[Peg, Level]: disc 1 (smallest) to N }
  Count: array [0..2] of Byte;             { How many discs each peg holds }
  Moves: Integer;

(**
 * The colours run dark to light, blue to white: the biggest disc is blue,
 * the smallest white.
 *)
function Colour(Disc: Byte): Byte;
begin
  Colour := 8 - Disc;
end;

(**
 * 44, 128 and 212: far enough apart that no two discs share a cell.
 *)
function Centre(Peg: Integer): Integer;
begin
  Centre := 44 + 84 * Peg;
end;

(**
 * A line from (X0, Y0) to (X1, Y1), with Y counting up from the bottom, as
 * in the Barleycorn original (and BASIC).
 *)
procedure Line(X0, Y0, X1, Y1: Integer);
begin
  Plot(X0, 175 - Y0);
  Draw(X1 - X0, Y0 - Y1);
end;

(**
 * The pegs and the floor, in grey (white without BRIGHT).
 *)
procedure Grey(X0, Y0, X1, Y1: Integer);
begin
  Write(#19#0);
  TextColor(White);
  Line(X0, Y0, X1, Y1);
  Write(#19#1);
end;

(**
 * A disc is two character rows of horizontal lines. The Spectrum keeps one
 * ink per 8 x 8 cell, so a disc gets cells of its own, and its own colour.
 * Drawing it with INVERSE on rubs it out (and the peg behind it with it).
 *)
procedure DrawDisc(Disc: Byte; Peg, Level: Integer; Shown: Boolean);
var
  X, W, Y, I: Integer;
begin
  X := Centre(Peg);
  W := 5 * Disc + 1;
  Y := 8 + 16 * Level;
  TextColor(Colour(Disc));
  if not Shown then Write(#20#1);
  for I := 1 to 14 do
    Line(X - W, Y + I, X + W, Y + I);
  Write(#20#0);
  if not Shown then Grey(X, Y, X, Y + 15);
end;

(**
 * A disc's light: outlined in its colour when off, filled when on. The
 * biggest disc's light is on the left, as the top bit is.
 *)
procedure Light(Disc: Byte; On: Boolean);
var
  A: Byte;
  C: Integer;
begin
  A := 64 + Colour(Disc);                  { BRIGHT, with the disc's colour as ink }
  if On then A := A + 8 * Colour(Disc);    { ... and as paper }
  C := 6 + 3 * (N - Disc);
  Mem[$5800 + 3 * 32 + C] := A;
  Mem[$5800 + 3 * 32 + C + 1] := A;
end;

procedure Setup;
var
  Peg, Level, D, X: Integer;
begin
  Border(Black);
  TextBackground(Black);
  Write(#19#1);
  ClrScr;
  Grey(0, 6, 255, 6);
  for Peg := 0 to 2 do
    Grey(Centre(Peg), 7, Centre(Peg), 127);
  for Level := 0 to N - 1 do
    DrawDisc(Stack[0, Level], 0, Level, True);
  for D := 1 to N do                       { The lights' outlines, in character row 3 }
  begin
    X := 8 * (6 + 3 * (N - D));
    TextColor(Colour(D));
    Line(X + 1, 145, X + 14, 145);
    Line(X + 14, 145, X + 14, 150);
    Line(X + 14, 150, X + 1, 150);
    Line(X + 1, 150, X + 1, 145);
    Light(D, False);
  end;
end;

(**
 * The place of M's lowest 1 bit, counting from 1.
 *)
function Lowest(M: Integer): Byte;
var
  Place: Byte;
begin
  Place := 1;
  while not Odd(M) do
  begin
    M := M shr 1;
    Inc(Place);
  end;
  Lowest := Place;
end;

(**
 * One move: the top disc of one peg onto another.
 *)
procedure Shift(From, Dest: Integer);
var
  Disc, D: Byte;
begin
  Dec(Count[From]);
  Disc := Stack[From, Count[From]];
  if Count[Dest] > 0 then
    Assert(Stack[Dest, Count[Dest] - 1] > Disc);   { Never onto a smaller disc }
  Inc(Moves);
  Assert(Disc = Lowest(Moves));                    { And always the one the lights say }
  DrawDisc(Disc, From, Count[From], False);
  DrawDisc(Disc, Dest, Count[Dest], True);
  for D := 1 to N do
    Light(D, Odd(Moves shr (D - 1)));
  Beep(0.06, Notes[Disc]);
  Stack[Dest, Count[Dest]] := Disc;
  Inc(Count[Dest]);
end;

(**
 * Moves N discs from one peg to another.
 *)
procedure Hanoi(N: Byte; From, Dest, Via: Integer);
begin
  if N = 0 then Exit;
  Hanoi(N - 1, From, Via, Dest);
  Shift(From, Dest);
  Hanoi(N - 1, Via, Dest, From);
end;

var
  Level, D: Integer;

begin
  for Level := 0 to N - 1 do
    Stack[0, Level] := N - Level;
  Count[0] := N;
  Count[1] := 0;
  Count[2] := 0;
  Moves := 0;

  Setup;
  Hanoi(N, 0, 2, 1);

  for D := N downto 1 do                   { And back up the scale, to finish }
    Beep(0.12, Notes[D]);

  TextColor(White);
  GotoXY(7, 1);
  WriteLn(N, ' discs in ', Moves, ' moves');
end.
