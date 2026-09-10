program StrDelHi;

{ Delete with a Start above 255 must abort -- same story as strcpyhi.pas,
  but for __delete, which picks Start off the stack differently.
  Expected output:
    before
    Invalid string index
  ("after" must not print.) }

var
  S: String[20];
  Start: Integer;
begin
  S := 'ABC';
  Start := 300;
  WriteLn('before');
  Delete(S, Start, 3);
  WriteLn('after: ', S);
end.
