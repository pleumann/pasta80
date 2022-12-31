program Fibonacci;

var
  A, B, I: Integer;

procedure Fibo;
var
  C: Integer;
begin
  WriteLn(A);

  C := A + B;
  A := B;
  B := C;
end;

begin
  A := 0;
  B := 1;
  I := 1;

  while I < 25 do
  begin
    Fibo;
    I := I + 1;
  end;
end.
