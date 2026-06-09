(**
 * Demonstrates the various ZX Spectrum screen attributes.
 *)
program ZXAttrs;

{$ifndef SYS_ZX}
  {$error ZX Spectrum 48K/128K/Next required.}
{$endif}

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
  I, B: Byte;

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
  
  GotoXY(5, 12);
  WriteLn('Press BREAK to exit...');

  B := 0;

  while True do
  begin
    Border(B);

    Inc(B);
    if B = 8 then B := 0;

    CheckBreak;
  end;
end.
