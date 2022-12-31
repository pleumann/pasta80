program Crt;

var
  I, Y, X, Z, F, B;

begin
  ClrScr;
  I := 0;
  while I < 5000 do
  begin
    X := 1 + Random mod 54;
    Y := 1 + Random mod 23;
    repeat
      F := Random mod 8;
      B := Random mod 8;
    until F <> B;
    GotoXY(X,Y);
    TextColor(F);
    TextBackground(B);
    Write('Thank you! tnylpo rocks! :)');
    I := I + 1;
  end;
end.
