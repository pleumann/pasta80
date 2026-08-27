program RAddOvf;

{ Real addition overflow must abort with "Real overflow", isolated from
  multiplication so it actually exercises __fpadd's own check, not just
  __fpmul's (OPEN-ITEMS B3). Doubling 1.0 127 times crosses the ~1.7e38
  exponent range on the 127th addition.
  Expected output:
    before
    Real overflow
  ("after" must not print.) }

var
  R: Real;
  I: Integer;
begin
  WriteLn('before');
  R := 1.0;
  for I := 1 to 127 do R := R + R;
  WriteLn('after: ', R);
end.
