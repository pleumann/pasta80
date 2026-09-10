program StrIns;

{ Insert with Start=0 must abort -- same story as strcpy.pas, but for
  __insert/__strins (OPEN-ITEMS-EN.md D1/D2).
  Expected output:
    before
    Invalid string index
  ("after" must not print.) }

var
  T: String[20];
begin
  T := 'Hello';
  WriteLn('before');
  Insert('XY', T, 0);
  WriteLn('after: ', T);
end.
