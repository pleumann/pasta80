(**
 * Barnsley fern fractal.
 *)
program Farn;

{$ifndef SYS_AGON}
  {$error Agon required.}
{$endif}

const
    Width = 1024;
    Height = 768;
    Cycles = -1;  //65535 due to wrap-around

var
  X, Y, XN, YN, R, FX, FY: Real;
  T, U, PX, PY, OX, OY, V: Integer;
  C: Char;
  B: Integer;  //multiplier for the number of points for Agon

(**
 * Queries a point at the given coordinates, returning the
 * specific colour instead of true/false, using the Agon RTL
 *)
function ColPoint(X1, Y1: Integer): Integer; register;      external 'al_point';

  procedure ColPlot(X2, Y2: Integer);
  begin
    (* Increases the colour value when the same point is plotted repeatedly *)
    V := ColPoint(PX, PY);
    if V < 3 then begin
      SetColor(V+1);
      Plot(PX, PY);
    end;
  end;

begin

    SetGraphMode(19);
(* this uses VDU 19, l, p, r, g, b: Define logical colour 
 * below colours are set using the value in p,
 * ignoring r,g,b\
 *)

(*
 * The below colour scheme gives a more
 * "spring" seasonal feel compared to the default
 * below which is more "autumn/fall"
 *    Write(#19,#1,#12,#0,#0,#0);
 *    Write(#19,#2,#29,#0,#0,#0);
 *    Write(#19,#3,#46,#0,#0,#0);
 *)
    Write(#19,#1,#12,#0,#0,#0);
    Write(#19,#2,#60,#0,#0,#0);
    Write(#19,#3,#57,#0,#0,#0);
    SetColor(3);

  TextBackground(Black);
  TextColor(Green);
  ClrScr;

  X := 0.0;
  Y := 0.0;
  T := Cycles;

  FX := Width / 5;
  FY := Height / 10;

  OX := Width div 2 - 5;
  OY := Height - 1;

  while T <> 0 do
  begin
    R := Random;

    if R < 0.01 then
    begin
      XN :=  0.0;
      YN :=  0.16 * Y;
    end
    else if R < 0.86 then
    begin
      XN :=  0.85 * X + 0.04 * Y;
      YN := -0.04 * X + 0.85 * Y + 1.6;
    end
    else if R < 0.93 then
    begin
      XN :=  0.20 * X - 0.26 * Y;
      YN :=  0.23 * X + 0.22 * Y + 1.6;
    end
    else
    begin
      XN := -0.15 * X + 0.28 * Y;
      YN :=  0.26 * X + 0.24 * Y + 0.44;
    end;

    { x: -2.5..2.5 -> 0..256 (Faktor 51), y: 0..10 -> 175..0 (Faktor 17) }
    PX := OX + Trunc(XN * FX);
    PY := OY - Trunc(YN * FY);

    if (PX >= 0) and (PX < Width) and (PY >= 0) and (PY < Height) then
      ColPlot(PX, PY);

    X := XN;
    Y := YN;

    B := (B+1) AND 3; // adjust bitmask to halve (1) or double (7) the number of cycles
    if (B = 0) then Dec(T); //Dec(T) only executes when the inner loop B cycles

  end;

  Write('Barnsley Fern');

  C := ReadKey;

  SetGraphMode(0);

end.
