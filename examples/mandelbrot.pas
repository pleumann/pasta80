(**
 * Adapted from an article by Markus Lautenbacher in MC 12/1986.
 *)
program Mandelbrot;

var
  X, Y, OldX, OldY, ReC, ImC, DC : Real;
  K, L, N : Integer;

begin
  SetCpuSpeed(3);
  ClrScr;
  DC := 3.0 / 256.0;
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

      if N > 100 (* or (N < 11) and (N mod 2 = 0) *) then
      begin
        Plot(K, 88 + L);
        Plot(K, 88 - L);
      end
    end;
  end;
end.
