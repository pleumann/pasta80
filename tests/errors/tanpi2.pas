program TanPi2;

{ Tan(Pi/2) must abort too -- TAN computes SIN(X)/COS(X) internally via a
  plain (unwrapped) FPDIV, and Cos(Pi/2) rounds to exactly 0 in this Real
  representation, so this hits the exact same Carry+Zero "Division by
  zero" path as rdivzero.pas, just reached through TAN instead of "/"
  (OPEN-ITEMS B3).
  Expected output:
    before
    Division by zero
  ("after" must not print.) }

var
  R: Real;
begin
  WriteLn('before');
  R := Tan(Pi / 2.0);
  WriteLn('after: ', R:0:3);
end.
