program ModTest;

function SafeMod(A, B: Real): Real;
var
  D: Real;
begin
  WriteLn('SafeMod: ', A, ', ', B);
  D := Int(A / B);
  SafeMod := A - B * D;
end;

begin
  WriteLn(SafeMod(1000000001.0e-0,31e-0)*1e0);
  WriteLn(SafeMod(1000000001.0e-1,31e-1)*1e1);
  WriteLn(SafeMod(1000000001.0e-2,31e-2)*1e2);
  WriteLn(SafeMod(1000000001.0e-3,31e-3)*1e3);
  WriteLn(SafeMod(1000000001.0e-4,31e-4)*1e4);
  WriteLn(SafeMod(1000000001.0e-5,31e-5)*1e5);
  WriteLn(SafeMod(1000000001.0e-6,31e-6)*1e6);
end.
