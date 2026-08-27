program LogZero;

{ Log(0) must abort too. LOG calls LN internally and already passes its
  Carry through via "ret c" -- this confirms __log inherits that instead
  of swallowing it (OPEN-ITEMS B3).
  Expected output:
    before
    Invalid floating point operation
  ("after" must not print.) }

var
  R: Real;
begin
  WriteLn('before');
  R := Log(0.0);
  WriteLn('after: ', R:0:3);
end.
