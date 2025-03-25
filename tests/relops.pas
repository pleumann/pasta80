program RelOps;

var
  A, B: Integer;

procedure TestIf(A, B: Integer);
begin
  if A = B then WriteLn(A, ' = ', B);
  if A <> B then WriteLn(A, ' <> ', B);
  if A < B then WriteLn(A, ' < ', B);
  if A <= B then WriteLn(A, ' <= ', B);
  if A > B then WriteLn(A, ' > ', B);
  if A >= B then WriteLn(A, ' >= ', B);
end;

procedure TestExpr(A, B: Integer);
begin
  WriteLn(A, ' = ', B, ': ', A = B);
  WriteLn(A, ' <> ', B, ': ', A <> B);
  WriteLn(A, ' < ', B, ': ', A < B);
  WriteLn(A, ' <= ', B, ': ', A <= B);
  WriteLn(A, ' > ', B, ': ', A > B);
  WriteLn(A, ' >= ', B, ': ', A >= B);
end;

begin
  TestIf(100, 100);
  TestIf(99, 100);
  TestIf(100, 99);

  TestExpr(100, 100);
  TestExpr(99, 100);
  TestExpr(100, 99);
end.