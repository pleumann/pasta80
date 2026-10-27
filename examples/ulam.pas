(**
 * The Ulam spiral: count up from 1, winding outwards from the middle of the
 * screen, and put a dot on every prime. After Barleycorn's examples/ulam.bly,
 * written by Claude Fern (and this Pascal by another of her), for Joerg
 * Pleumann's PASTA/80, in the Barleycorn bake-off.
 *
 * Stanislaw Ulam doodled it in 1963 during a dull talk, and saw the primes
 * gather on diagonals. Each diagonal holds the values of a quadratic, and
 * some quadratics are far fonder of primes than others (Euler's n*n + n + 41
 * is prime for every n from 0 to 39). Why, nobody has proved. The beeper
 * clicks for each pair of twin primes, p and p + 2, higher as the spiral
 * grows.
 *
 * MIT License / Copyright (c) 2026 Melissa O'Neill / Written by Fern.
 *)
program Ulam;

{$ifndef SYS_ZX48}
  {$error ZX Spectrum required.}
{$endif}

const
  Side = 160;                    { The spiral fills a Side x Side square }
  N = 25600;                     { Side * Side: it counts from 1 up to N }
  SieveTop = 3200;               { N div 8: a bit for each number from 0 to N }

  { North, east, south, west: turning left is (D + 3) mod 4 }
  DR: array [0..3] of Integer = (-1, 0, 1, 0);
  DC: array [0..3] of Integer = (0, 1, 0, -1);

var
  Composite: array [0..SieveTop] of Byte;          { The sieve of Eratosthenes, packed }

function IsComposite(I: Integer): Boolean;
begin
  IsComposite := Composite[I shr 3] and (1 shl (I and 7)) <> 0;
end;

procedure MarkComposite(I: Integer);
begin
  Composite[I shr 3] := Composite[I shr 3] or (1 shl (I and 7));
end;

(**
 * Side * Side = N, so the primes below Side cross out every composite.
 *)
procedure Sieve;
var
  P, K: Integer;
begin
  FillChar(Composite, SizeOf(Composite), 0);
  MarkComposite(0);
  MarkComposite(1);
  for P := 2 to Side - 1 do
    if not IsComposite(P) then
      for K := P to N div P do
        MarkComposite(K * P);
end;

var
  I, R, C, D, Leg, Steps, Turns, Primes, Twins: Integer;

begin
  TextBackground(Black);
  TextColor(Cyan);
  Write(#19#1);
  Border(Black);
  ClrScr;

  Sieve;

  R := 80;                       { Where 1 goes, so the square sits in the }
  C := 127;                      { middle of the top 20 rows }
  D := 1;                        { East: 2 goes to the right of 1 }
  Leg := 1;                      { The legs go 1, 1, 2, 2, 3, 3, ... }
  Steps := 1;                    { ... with a left turn after each }
  Turns := 0;
  Primes := 0;
  Twins := 0;

  for I := 1 to N do
  begin
    if not IsComposite(I) then
    begin
      Inc(Primes);
      Plot(C, R);
      if (I >= 5) and not IsComposite(I - 2) then
      begin
        Inc(Twins);
        Beep(0.001, Leg div 4 + 20);
      end;
    end;

    R := R + DR[D];
    C := C + DC[D];
    Dec(Steps);
    if Steps = 0 then
    begin
      D := (D + 3) mod 4;
      Inc(Turns);
      if not Odd(Turns) then Inc(Leg);
      Steps := Leg;
    end;
  end;

  TextColor(White);
  GotoXY(5, 21);
  Write(Primes, ' primes up to ', N);
  GotoXY(5, 22);
  Write(Twins, ' pairs of twin primes');
end.
