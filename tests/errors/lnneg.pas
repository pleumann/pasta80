program LnNeg;

{ Ln of a negative number must abort too, same check as Ln(0) but the
  other branch of LN's Carry logic (OPEN-ITEMS B3).
  Expected output:
    before
    Invalid floating point operation
  ("after" must not print.) }

var
  R: Real;
begin
  WriteLn('before');
  R := Ln(-5.0);
  WriteLn('after: ', R:0:3);
end.
