program Crt;

var
  I, Y, X, Z, F, B: Integer;

begin
  ClrScr;
  I := 0;
  while I < 10000 do
  begin
    X := 1 + Random(71);
    Y := 1 + Random(23);
    repeat
      F := Random(8);
      B := Random(8);
    until F <> B;
    GotoXY(X,Y);
    TextColor(F);
    TextBackground(B);
    Write('* Pascal *');
    I := I + 1;
  end;
end.
