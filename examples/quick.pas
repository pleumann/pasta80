program Quick;

const
  Count = 200;

type
  TIntArray = array[Count] of Integer;

var
  Numbers: TIntArray;
  I: Integer;

procedure Dump(var A: TIntArray);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Write(A[I], ' ');
  WriteLn;
  WriteLn;
end;

procedure Sort(var A: TIntArray; Low, High: Integer);
var
  Pivot, I, J, Temp: Integer;
begin
  Pivot := A[(Low + High) / 2];

  I := Low;
  J := High;

  repeat
    while A[I] < Pivot do I := I + 1;
    while A[J] > Pivot do J := J - 1;

    if I <= J then
    begin
      Temp := A[I];
      A[I] := A[J];
      A[J] := Temp;

      I := I + 1;
      J := J - 1;
    end;
  until I > J;

  if J > Low then Sort(A, Low, J);
  if I < High then Sort(A, I, High);
end;

begin
  WriteLn('*** Quicksort ***');
  WriteLn;

  WriteLn('Generating', Count, ' random numbers...');
  WriteLn;
  for I := 0 to Count - 1 do
    Numbers[I] := Random(Count);
  
  Dump(Numbers);

  WriteLn('Sorting array of', Count, ' numbers...');
  WriteLn;
  Sort(Numbers, 0, Count - 1);

  Dump(Numbers);

  WriteLn('Done.');
end.