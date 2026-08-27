program RDivOvf;

{ Real division that overflows with a NON-zero divisor must be reported as
  "Real overflow", not "Division by zero" (OPEN-ITEMS B3) -- this is the
  case __fpdiv's Carry+Zero distinction exists for.
  Expected output:
    before
    Real overflow
  ("after" must not print.) }

var
  R, Small: Real;
begin
  WriteLn('before');
  R := 1.0;
  R := R + R;      { 2.0 }
  Small := 1e-30;
  R := R / Small;
  R := R / Small;
  R := R / Small;
  WriteLn('after: ', R);
end.
