program RSubOvf;

{ Real subtraction overflow must abort with "Real overflow" too -- FPSUB is
  implemented as FPNEG+FPADD internally, but this exercises the "-"
  operator (__fpsub) directly rather than assuming it behaves like "+"
  (OPEN-ITEMS B3). Huge is doubled up to just below the exponent limit,
  then Huge - (-Huge) = 2*Huge crosses it.
  Expected output:
    before
    Real overflow
  ("after" must not print.) }

var
  Huge, R: Real;
  I: Integer;
begin
  WriteLn('before');
  Huge := 1.0;
  for I := 1 to 126 do Huge := Huge + Huge;
  R := Huge - (-Huge);
  WriteLn('after: ', R);
end.
