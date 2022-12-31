(* Squares *)

var
  I, J: Integer;

function Square(x: Integer): Integer;
begin
   Square := x * x;
end;

begin
   WriteLn('How many squares do you want?');
   ? J;
   if J < 0 then WriteLn('Naturally!');

   for I := 1 to J do
     WriteLn(Square(I));
end.
