(**
 * Calculates some factorial the iterative way.
 *)
program Factorial;

var
  I: Integer;

function Fact(I: Integer): Integer;
var
  J: Integer;
begin
  J := 1;

  while I > 1 do
  begin
    J := J * I;
    I := I - 1;
  end;

  Fact := J;
end;

begin
  WriteLn('Iterative factorials');
  WriteLn;

  for I := 1 to 7 do
    WriteLn(I:2, '! = ', Fact(I):5);
end.

