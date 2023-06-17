program Squares;

var
  I, J: Integer;

function Square(x: Integer): Integer;
begin
  Square := x * x;
end;

begin
  Write('How many squares do you want? ');
  ReadLn(J);

  if J < 0 then WriteLn('Naturally!');

  for I := 1 to J do
    WriteLn(Square(I));
end.
