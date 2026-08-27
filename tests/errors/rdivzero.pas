program RDivZero;

{ Real division by zero must abort (OPEN-ITEMS B3). The zero divisor is
  what __fpdiv's Carry+Zero check should recognize as "Division by zero",
  not as a generic "Real overflow".
  Expected output:
    before
    Division by zero
  ("after" must not print.) }

var
  R: Real;
begin
  WriteLn('before');
  R := 7.0 / 0.0;
  WriteLn('after: ', R:0:3);
end.
