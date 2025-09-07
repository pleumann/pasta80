program D;

type
  MyInt = Integer;
  MyOtherInt = MyInt;

var
  I: Integer;
  J: MyInt;
  K: MyOtherInt;

begin
  I := 1;
  J := I;
  K := I;

  WriteLn(I:3, J:3, K:3);

  J := 2;
  I := J;
  K := J;

  WriteLn(I:3, J:3, K:3);

  K := 3;
  I := K;
  J := K;

  WriteLn(I:3, J:3, K:3);
end.