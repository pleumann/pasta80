program SqrtNeg;

{ Sqrt of a negative number must abort, not silently return the argument
  unchanged (OPEN-ITEMS B3). SQR in math48.asm already flags this via
  Carry; __sqrt is the wrapper that acts on it.
  Expected output:
    before
    Invalid floating point operation
  ("after" must not print.) }

var
  R: Real;
begin
  WriteLn('before');
  R := Sqrt(-1.0);
  WriteLn('after: ', R:0:3);
end.
