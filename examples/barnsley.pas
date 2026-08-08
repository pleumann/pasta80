(**
 * Barnsley fern fractal.
 *)
program Farn;

{$ifdef SYS_CPM}
  {$error Agon or ZX Spectrum 48K/128K/Next required.}
{$endif}

const
  {$ifdef SYS_AGON}
    Width = 1024;
    Height = 768;
    Cycles = -1;  //65535
  {$else}
    Width = 256;
    Height = 176;
    Cycles = 8191;
  {$endif}

var
  X, Y, XN, YN, R, FX, FY: Real;
  T, U, PX, PY, OX, OY, V: Integer;
  C: Char;

begin
  {$ifdef SYS_ZXNEXT}
  SetCpuSpeed(3);
  {$endif}

  {$ifdef SYS_AGON}
    SetGraphMode(19);
    // VDU 19, l, p, r, g, b: Define logical colour 
    Write(#19,#2,#12,#0,#255,#0);
    SetColor(2);
  {$else}
    Border(0);
  {$endif}

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
      Plot(PX, PY);

    X := XN;
    Y := YN;

    Dec(T);
  end;

  Write('Barnsley Fern');

  C := ReadKey;

  {$ifdef SYS_AGON}
  SetGraphMode(0);
  {$endif}

end.
