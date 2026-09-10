program StrDel;

{ Delete with Start=0 must abort -- same story as strcpy.pas, but for
  __delete/__strdel. Not originally tracked as a D-item at all; found
  while investigating D1/D2 (OPEN-ITEMS-EN.md).
  Expected output:
    before
    Invalid string index
  ("after" must not print.) }

var
  S: String[20];
begin
  S := 'ABC';
  WriteLn('before');
  Delete(S, 0, 3);
  WriteLn('after: ', S);
end.
