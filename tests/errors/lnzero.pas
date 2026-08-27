program LnZero;

{ Ln(0) must abort, not silently return 0.0 (OPEN-ITEMS B3). LN in
  math48.asm already flags this via Carry; __ln is the wrapper that acts
  on it.
  Expected output:
    before
    Invalid floating point operation
  ("after" must not print.) }

var
  R: Real;
begin
  WriteLn('before');
  R := Ln(0.0);
  WriteLn('after: ', R:0:3);
end.
