program ZXAttrs;

procedure Flash(B: Boolean);
begin
  Write(#18, Char(B));
end;

procedure Bright(B: Boolean);
begin
  Write(#19, Char(B));
end;

procedure Inverse(B: Boolean);
begin
  Write(#20, Char(B));
end;

var
  I: Byte;

begin
  ClrScr;

  for I := 0 to 7 do
  begin
    TextBackground(I);
    TextColor(7 - I);

    GotoXY(1, 1 + I);
    Write('Normal ');

    GotoXY(9, 1 + I);
    Flash(True);
    Write('Flash  ');
    Flash(False);

    GotoXY(17, 1 + I);
    Bright(True);
    Write('Bright ');
    Bright(False);

    GotoXY(25, 1 + I);
    Inverse(True);
    Write('Inverse');
    Inverse(False);
  end;
end.