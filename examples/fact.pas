program Factorial;

var
  I, J: Integer;

procedure Fact;
begin
  J := 1;
  while I > 1 do
  begin
    J := J * I;
    I := I - 1;
  end;
end;

begin
  I := 5;
  Fact;
  WriteLn(J);
end.

