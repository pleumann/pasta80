program ModZero;

{ Integer "mod" by zero must abort, not silently return 0 (OPEN-ITEMS B3).
  Expected output:
    before
    Division by zero
  ("after" must not print.) }

var
  A, B, C: Integer;
begin
  WriteLn('before');
  A := 7;
  B := 0;
  C := A mod B;
  WriteLn('after: ', C);
end.
