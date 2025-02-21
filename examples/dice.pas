program Dice; (* Only works w/o optimization. Why? *)

var
  Rolls, Dice, I, J, K, Sum: Integer;

begin
  Write('How many rolls? '); ReadLn(Rolls);
  Write('How many dice? '); ReadLn(Dice);

  for I := 1 to Rolls do
  begin
    Sum := 0;
    for J := 1 to Dice do
    begin
      K := Random(20);
      Write(K:4);
      Inc(Sum, K);
    end;
    WriteLn(' -> ', Sum:5);
  end;
end.