program Farn;

const
  {$ifdef SYS_AGON}
  Width = 640;
  Height = 480;
  {$endif}

  {$ifdef SYS_ZXNEXT}
  Width = 256;
  Height = 192;
  {$endif}

  MaxT = 32767;

var
  X, Y, XN, YN, R, FX, FY: Real;
  T, U, PX, PY, OX, OY, V: Integer;
  C: Char;

{$ifdef SYS_ZXNEXT}
procedure L2Enable; register; external 'l2_enable';
procedure L2SetPixel(X, Y, Color: Byte); register; external 'l2_set_pixel';
function L2GetPixel(X, Y: Byte): Byte; register; external 'l2_get_pixel';
{$l hat256.asm}
{$endif}

begin
  {$ifdef SYS_AGON}
  ClrScr;
  GotoXY(5, 5);
  {$endif}

  {$ifdef SYS_ZXNEXT}
  SetCpuSpeed(3);
  L2Enable;

  Border(0);
  TextBackground(0);
  TextColor(28);
  Write(#14);
  {$endif}

  X := 0.0;
  Y := 0.0;
  T := -32768;

  FX := Width / 5;
  FY := Height / 10;

  OX := Width div 2 - 5;
  OY := Height - 1;

  while T < MaxT do
  begin
    R := RandomReal;

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
    begin
      {$ifdef SYS_AGON}
      Plot(PX, PY);
      {$endif}

      {$ifdef SYS_ZXNEXT}
      (* Increase green channel by 1. *)
      V := L2GetPixel(PX, PY);
      if V <= 24 then L2SetPixel(PX, PY, V + 4);
      {$endif}
    end;

    X := XN;
    Y := YN;

    Inc(T);
  end;

  Write('Barnsley Fern ');

  C := ReadKey;
end.
