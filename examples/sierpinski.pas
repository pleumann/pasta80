(**
 * Sierpinski triangle.
 *)
program Sierpinski;

{$ifdef SYS_CPM}
  {$error Agon or ZX Spectrum 48K/128K/Next required.}
{$endif}

procedure DrawTriangle(X1, Y1, X2, Y2, X3, Y3: Integer);
begin
  Plot(X1, Y1);

  Draw(X2 - X1, Y2 - Y1);
  Draw(X3 - X2, Y3 - Y2);
  Draw(X1 - X3, Y1 - Y3);
end;

{$a-}
procedure DrawSierpinski(X1, Y1, X2, Y2, X3, Y3: Real; Depth: Integer);
var
  MX12, MY12, MX23, MY23, MX13, MY13: Real;
begin
  if Depth = 0 then
    DrawTriangle(Round(X1), Round(Y1), Round(X2), Round(Y2), Round(X3), Round(Y3))
  else
  begin
    MX12 := (X1 + X2) / 2;  MY12 := (Y1 + Y2) / 2;
    MX23 := (X2 + X3) / 2;  MY23 := (Y2 + Y3) / 2;
    MX13 := (X1 + X3) / 2;  MY13 := (Y1 + Y3) / 2;
    DrawSierpinski(X1,   Y1,   MX12, MY12, MX13, MY13, Depth - 1);
    DrawSierpinski(MX12, MY12, X2,   Y2,   MX23, MY23, Depth - 1);
    DrawSierpinski(MX13, MY13, MX23, MY23, X3,   Y3,   Depth - 1);
  end;
end;
{$a+}

begin
  {$ifdef SYS_ZXNEXT}
  SetCpuSpeed(3);
  {$endif}

  ClrScr;

  {$ifdef SYS_AGON}
  DrawSierpinski(40, 479, 599, 479, 320.5, 0, 6);
  {$else}
  DrawSierpinski(20, 175, 235, 175, 127.5, 0, 5);
  {$endif}

  GotoXY(12, 13);
  WriteLn('Sierpinski');
  GotoXY(12, 14);
  WriteLn(' Triangle');
  ReadKey;
end.
