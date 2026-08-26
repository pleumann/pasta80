(**
 * Shows how to add an assembly function and make that available to Pascal
 * through a function with "register" calling convention.
 *)
program DMTest;

{$l divmod.asm}

(**
 * Integer division that also provides the remainder. Useful if both div
 * and mod are required because the internal 16 bit division calculates
 * them anyway.
 *)
function DivMod(Divisor, Dividend: Integer; var Quotient: Integer): Integer; register; external '__divmod';

var
  Q, R: Integer;

procedure Test(Divisor, Dividend: Integer);
begin
  R := DivMod(Divisor, Dividend, Q);
  WriteLn(Dividend, ' div ', Divisor, ' = ', Q, ' rem ', R);
end;

begin
  Test(3, 10);
  Test(3, -10);
  Test(-3, 10);
  Test(-3, -10);
  Test(7, 0);
  Test(1, 32767);
  Test(2, -32768);
end.
