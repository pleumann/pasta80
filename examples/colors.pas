program Colors;

var
  B, C: Integer;
begin
  ClrScr;
  for B := 0 to 7 do
  begin
    TextBackground(B);
    for C := 0 to 7 do
    begin
      TextColor(C);
      GotoXY(1 + C * 9, 1 + B * 2);
      Write(' Pascal ');
    end;
  end;
end.