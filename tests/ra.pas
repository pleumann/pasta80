var
  A, B: array [10] of Integer;

  R, S: record
    X, Y: Integer;
  end;

  I: Integer;

begin
  for I := 0 to 9 do 
    A[I] := I * I;

  B := A;

  Write('A: ');
  for I := 0 to 9 do 
    Write(A[I], ' ');
  WriteLn;

  Write('B: ');
  for I := 0 to 9 do 
    Write(B[I], ' ');
  WriteLn;

  A[5] := 100;
  B[5] := 200;

  Write('A: ');
  for I := 0 to 9 do 
    Write(A[I], ' ');
  WriteLn;

  Write('B: ');
  for I := 0 to 9 do 
    Write(B[I], ' ');
  WriteLn;

  R.X := 300;
  R.Y := 400;

  S := R;

  WriteLn('R: X=', R.X, ' Y=', R.Y);
  WriteLn('S: X=', S.X, ' Y=', S.Y);

  R.X := 1234;
  S.Y := 5678;

  WriteLn('R: X=', R.X, ' Y=', R.Y);
  WriteLn('S: X=', S.X, ' Y=', S.Y);
end.