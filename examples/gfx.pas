program Gfx;

{$k+}

var
  I, J, K, L: Integer;

begin
  ClrScr;

  for I := 0 to 1000 do
    Plot(Random(128), 48 + Random(128));

  Plot(0, 48);
  Draw(127, 0);
  Draw(0, 127);
  Draw(-127, 0);
  Draw(0, -127);

  I := 128;
  while I <= 208 do
  begin
    Plot(I, 128);
    Draw(47, 47);
    Plot(I, 175);
    Draw(47, -47);
    Inc(I, 4);
  end;

  I := 63;
  while I > 0 do
  begin
    Circle(192, 63, I);
    Dec(I, 16);
  end;

  FloodFill(135, 63);
  FloodFill(165, 63);

  GotoXY(1, 1);
  Write('*Pascal*');

  for I := 0 to 63 do
    for J := 0 to 7 do
      if Point(I, J) then
      begin
        K := 2 * I;
        L := 16 + 2 * J;

        Plot(K, L);
        Plot(K + 1, L);
        Plot(K, L + 1);
        Plot(K + 1, L + 1);
      end;

  (*ClrScr;
  Fill(128, 88); *)
end.
