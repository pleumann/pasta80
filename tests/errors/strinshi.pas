program StrInsHi;

{ Insert with a negative Start must abort -- the same check as in
  strcpyhi.pas from the other end: -1 arrives as $FFFF and would pass for
  255 if only the low byte were looked at, quietly appending instead of
  failing.
  Expected output:
    before
    Invalid string index
  ("after" must not print.) }

var
  T: String[20];
  Start: Integer;
begin
  T := 'Hello';
  Start := -1;
  WriteLn('before');
  Insert('XY', T, Start);
  WriteLn('after: ', T);
end.
