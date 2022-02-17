program Enums;

type
  Color = (Red, Yellow, Green);

var
  C: Color;
  A: array[3] of Color;
  I: Integer;

begin
  C := Red;
  Assert(C <> Green);

  C := Green;
  Assert(C = Green);
  
  C := Yellow;
  Assert(C > Red);
  Assert(C >= Yellow);
  Assert(C <= Yellow);
  Assert(C < Green);

(*
  Assert(Red = Pred(Yellow));
  Assert(Succ(Red) = Yellow);

  Assert(Yellow = Pred(Green));
  Assert(Succ(Yellow) = Green);
*)

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

  (* 
  Pred, Succ,
  Literale ausgeben,
  als Array-Index erlauben
  Boolean retrofitten,
  8-Bit enforcen,
  *)
end.