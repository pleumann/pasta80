program RoundOvf;

{ Same as truncovf.pas, but via Round -- __fltrnd adds/subtracts 0.5
  through the checked __fpadd/__fpsub before tail-jumping into __trunc,
  so this exercises that path too (OPEN-ITEMS B-Trunc/Round).
  Expected output:
    before
    Invalid floating point operation
  ("after" must not print.) }

var
  R: Real;
  I: Integer;
begin
  WriteLn('before');
  R := -1.0e10;
  I := Round(R);
  WriteLn('after: ', I);
end.
