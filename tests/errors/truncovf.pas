program TruncOvf;

{ Trunc must abort if the result does not fit into the 16-bit signed
  Integer range (-32768..32767), matching TP3/TP5 behaviour. FIX
  (math48.asm) already carries a reliable Carry flag for this; __trunc
  (rtl/system.asm) wraps it and reports it the same way TP5 does: as
  "Invalid floating point operation" (OPEN-ITEMS B-Trunc/Round).
  Expected output:
    before
    Invalid floating point operation
  ("after" must not print.) }

var
  R: Real;
  I: Integer;
begin
  WriteLn('before');
  R := 1.0e10;
  I := Trunc(R);
  WriteLn('after: ', I);
end.
