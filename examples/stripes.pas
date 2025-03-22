program Stripes;

var
  B: Byte;

begin
  TextColor(0);
  TextBackground(7);
  Border(7);
  ClrScr;

  GotoXY(6, 12);
  Write('Press BREAK to exit...');

  B := 0;

  while True do
  begin
    Border(B);

    Inc(B);
    if B = 8 then B := 0;

    CheckBreak;
  end;
end.