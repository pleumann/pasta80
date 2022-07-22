type
  Rec = record
    A, B: Real;
  end;

var
  C: Rec;
  D: ^Rec;

procedure TestWith;
var
  A, B, I, J, K: Integer;
  R: Rec;
  S: array[20] of Rec;
begin
  A := 1;
  B := 2;
(*
  with C do
  begin
    A := 3.0;
    B := 4.0;
  end;

  with D^ do
  begin
    A := 5.0;
    B := 6.0;
  end;
*)
  with R do
  begin
    A := 7.0;
    B := 8.0;
  end;

  I := 2; J := 3; K := 4;
  with S[I * J + K] do
  begin
    A := 9.0;
    B := 10.0;
  end;

  WriteLn(A, B);

  with C do 
    WriteLn(A, B);

  with D^ do 
    WriteLn(A, B);

  with R do 
    WriteLn(A, B);

  with S[I * J + K] do
    WriteLn(A, B);
end;

begin
  D := Ptr(Addr(C));
  TestWith;
end.
