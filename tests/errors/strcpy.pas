program StrCpy;

{ Copy with Start=0 must abort -- TP 3.0 raises a runtime error here,
  matching real TP3 as reported by Joerg (OPEN-ITEMS-EN.md D1). __strcpy
  already carried a Carry-based signal for this internally, but that same
  flag is shared with harmless clamping cases further down in the
  routine, so __copy (rtl/system.asm) checks Start directly before
  calling __strcpy at all.
  Expected output:
    before
    Invalid string index
  ("after" must not print.) }

var
  S: String[20];
begin
  WriteLn('before');
  S := Copy('Hello', 0, 3);
  WriteLn('after: ', S);
end.
