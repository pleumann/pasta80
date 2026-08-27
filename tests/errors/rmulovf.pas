program RMulOvf;

{ Real multiplication overflow must abort with "Real overflow" instead of
  silently producing a garbage result (OPEN-ITEMS B3).
  Expected output:
    before
    Real overflow
  ("after" must not print.) }

var
  R: Real;
begin
  WriteLn('before');
  R := 1e19;
  R := R * R;       { 1e38, past the ~1.7e38 exponent range }
  WriteLn('after: ', R);
end.
