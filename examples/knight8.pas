(**
 * A knight's tour of the whole 8x8 board by Warnsdorff's rule (always jump
 * to the square with the fewest ways onward), drawn as it goes: the squares
 * colour in as the knight reaches them, dark early and light late, and each
 * move plays a note a little higher than the last. Then the rule is tried
 * from every square, and a little map shows where it makes a tour (green),
 * a closed one (white), or gets stuck (red).
 *
 * Fern's knight8.bly (Barleycorn), in PASTA/80's Pascal, for niceness: the
 * same rule, ties broken in the same move order as knight.pas.
 *
 * MIT License / Copyright (c) 2026 Melissa O'Neill / Written by Fern.
 *)
program Knight8;

{$ifndef SYS_ZX}
  {$error ZX Spectrum 48K/128K/Next required.}
{$endif}

const
  DR: array[0..7] of Integer = (1, 2, 2, 1, -1, -2, -2, -1);
  DC: array[0..7] of Integer = (2, 1, -1, -2, 2, 1, -1, -2);
  Pentatonic: array[0..4] of Integer = (0, 2, 4, 7, 9);

var
  Seen: array[0..7, 0..7] of Boolean;
  LastR, LastC: Integer;
  N, R, C, Tours, Loops: Integer;
  Colour: Byte;

function Free(R, C: Integer): Boolean;
begin
  if (R < 0) or (R > 7) or (C < 0) or (C > 7) then
    Free := False
  else
    Free := not Seen[R, C];
end;

function Onward(R, C: Integer): Integer;  (* unvisited squares a knight on R, C could jump to *)
var
  I, N: Integer;
begin
  N := 0;
  for I := 0 to 7 do
    if Free(R + DR[I], C + DC[I]) then N := N + 1;
  Onward := N;
end;

function NextSquare(var R, C: Integer): Boolean;  (* the rule; False when there's nowhere left *)
var
  I, N, Fewest, BestR, BestC: Integer;
begin
  Fewest := 9;
  for I := 0 to 7 do
    if Free(R + DR[I], C + DC[I]) then
    begin
      N := Onward(R + DR[I], C + DC[I]);
      if N < Fewest then
      begin
        Fewest := N;
        BestR := R + DR[I];
        BestC := C + DC[I];
      end;
    end;
  NextSquare := Fewest < 9;
  if Fewest < 9 then
  begin
    R := BestR;
    C := BestC;
  end;
end;

(* The board: each square two character cells across and two down. *)

procedure Square(R, C: Integer; A: Byte);
var
  Base: Integer;
begin
  Base := $5800 + (2 + 2 * R) * 32 + 1 + 2 * C;
  Mem[Base] := A;
  Mem[Base + 1] := A;
  Mem[Base + 32] := A;
  Mem[Base + 33] := A;
end;

procedure Visit(R, C, K: Integer; Here: Boolean);  (* bright, in six bands: red ... white *)
var
  A: Byte;
begin
  A := 64 + 8 * (2 + K * 6 div 64);
  if Here then A := A + 128;
  Square(R, C, A);
end;

procedure Step(R0, C0, R1, C1, K: Integer);
var
  S: Integer;
begin
  Visit(R0, C0, K - 1, False);
  Visit(R1, C1, K, True);
  Plot(15 + 16 * C0, 24 + 16 * R0);           (* y counts from the top here *)
  Draw(16 * (C1 - C0), 16 * (R1 - R0));
  S := K div 4;
  Beep(0.09, Pentatonic[S mod 5] + 12 * (S div 5) - 12);
end;

function Tour(R0, C0: Integer; Show: Boolean): Integer;  (* how many squares the rule reaches *)
var
  R, C, PR, PC, Count: Integer;
begin
  for R := 0 to 7 do
    for C := 0 to 7 do
      Seen[R, C] := False;
  Seen[R0, C0] := True;
  R := R0;
  C := C0;
  Count := 1;
  if Show then Visit(R, C, 0, True);
  PR := R;
  PC := C;
  while (Count < 64) and NextSquare(R, C) do
  begin
    Seen[R, C] := True;
    if Show then Step(PR, PC, R, C, Count);
    PR := R;
    PC := C;
    Count := Count + 1;
  end;
  LastR := R;
  LastC := C;
  Tour := Count;
end;

function Closed(R0, C0: Integer): Boolean;  (* does the tour end a knight's move from its start? *)
begin
  Closed := Abs(LastR - R0) * Abs(LastC - C0) = 2;
end;

begin
  Border(0);
  TextBackground(0);
  TextColor(7);
  ClrScr;
  for R := 0 to 7 do
    for C := 0 to 7 do
      Square(R, C, 7 + 8 * ((R + C) mod 2));
  Write(#16#8#17#8#18#8#19#8);                (* INK 8, PAPER 8, FLASH 8, BRIGHT 8: drawing keeps the colours *)

  N := Tour(0, 0, True);
  Visit(LastR, LastC, N - 1, False);
  if (N = 64) and Closed(0, 0) then
  begin
    Plot(15 + 16 * LastC, 24 + 16 * LastR);
    Draw(-16 * LastC, -16 * LastR);
    Beep(0.15, 24); Beep(0.15, 28); Beep(0.15, 31); Beep(0.4, 36);
  end;
  GotoXY(2, 1);
  if N < 64 then
    WriteLn('From the corner: stuck after ', N)
  else if Closed(0, 0) then
    WriteLn('From the corner: a closed tour')
  else
    WriteLn('From the corner: an open tour');

  Mem[$5800 + 11 * 32 + 21] := 8 * 4; GotoXY(24, 12); Write('a tour');
  Mem[$5800 + 12 * 32 + 21] := 8 * 7; GotoXY(24, 13); Write('closed');
  Mem[$5800 + 13 * 32 + 21] := 8 * 2; GotoXY(24, 14); Write('stuck');

  Tours := 0;
  Loops := 0;
  for R := 0 to 7 do
    for C := 0 to 7 do
    begin
      Colour := 2;
      if Tour(R, C, False) = 64 then
      begin
        Tours := Tours + 1;
        Colour := 4;
        if Closed(R, C) then
        begin
          Loops := Loops + 1;
          Colour := 7;
        end;
      end;
      Mem[$5800 + (2 + R) * 32 + 21 + C] := 64 + 8 * Colour;
      Beep(0.02, Colour * 3);
    end;
  GotoXY(2, 20);
  WriteLn(Tours, ' of 64 starts make a tour,');
  GotoXY(2, 21);
  WriteLn(Loops, ' of them closed, ', 64 - Tours, ' stuck');
end.
