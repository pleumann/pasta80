(**
 * Adapted from an article by Markus Lautenbacher in MC 12/1986.
 *)
program Mandelbrot;

var
  X, Y, OldX, OldY, ReC, ImC, DC: Real;
  K, L, N, I: Integer;

procedure L2Enable; register; external 'l2_enable';
procedure L2SetPixel(X, Y, Color: Byte); register; external 'l2_set_pixel';

{$l hat256.asm}

begin
  SetCpuSpeed(3);

  { ClrScr; }
  L2Enable;
  TextBackground($B6);
  for I := 0 to 191 do
    Write('    ');  

  DC := 3 / 256;
  for K := 0 TO 255 do
  begin
    ReC := -2.25 + K * DC;

    for L := 0 TO 95 do
    begin
      ImC := L * DC;
      X := 0; Y := 0;
      N := 0;

      repeat
        OldX := X; OldY := Y;
        X := Sqr(OldX) - Sqr(OldY) + ReC;
        Y := 2 * OldX * OldY + ImC;
        N := N + 1;
      until (N > 100) or ((Sqr(X) + Sqr(Y) ) > 100);

      TextColor(N and 255);
//      if N > 100 (* or (N < 11) and (N mod 2 = 0) *) then
      begin
        L2SetPixel(K, 88 + L, N and 255);
        L2SetPixel(K, 88 - L, N and 255);
      end
    end;
  end;
end.
