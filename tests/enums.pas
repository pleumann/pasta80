program Enums;

type
  Color = (Red, Yellow, Green);

var
  C: Color;
  A: array[3] of Color;
  I: Integer;

begin
  WriteLn('--- TestEnums ---');
  C := Red;
  Assert(C <> Green);

  C := Green;
  Assert(C = Green);
  
  C := Yellow;
  Assert(C > Red);
  Assert(C >= Yellow);
  Assert(C <= Yellow);
  Assert(C < Green);

  Assert(Red = Pred(Yellow));
  Assert(Succ(Red) = Yellow);

  Assert(Yellow = Pred(Green));
  Assert(Succ(Yellow) = Green);

  Assert(Even(Red));
  Assert(Odd(Yellow));

  I := 0;
  for C := Red to Green do
  begin
    Assert(Ord(C) = I);
    Assert(C = Color(I));
    I := I + 1;
  end;

  A[0] := Red;
  A[1] := Yellow;
  A[2] := Green;

  Assert(A[0] = Red);
  Assert(A[1] = Yellow);
  Assert(A[2] = Green);

  I := 0;
  for C := Red to Green do
  begin
    Assert(C = A[I]);
    I := I + 1;
  end;

  I := 2;
  for C := Green downto Red do
  begin
    Assert(C = A[I]);
    I := I - 1;
  end;

  WriteLn;
  for C := Red to Green do
  begin
    WriteLn('The light is ', C, '.');
  end;
  WriteLn;

  (* 
  TODO
  Retrofit Boolean as enum,
  allow enum as array index,
  enforce 8-Bit,
  *)
end.