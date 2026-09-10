program StrCpyHi;

{ Copy with a Start above 255 must abort. Start is an Integer, but only
  values 1..255 can address a character, and the wrappers in rtl/system.asm
  used to look at its low byte alone -- which silently turned Copy(S, 300, 3)
  into Copy(S, 44, 3). __idxchk now sees the high byte too.
  Expected output:
    before
    Invalid string index
  ("after" must not print.) }

var
  S: String[20];
  Start: Integer;
begin
  Start := 300;
  WriteLn('before');
  S := Copy('Hello', Start, 3);
  WriteLn('after: ', S);
end.
