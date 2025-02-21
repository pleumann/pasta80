program Quick;

{$a-}

const
  Count = 200;

type
  TIntArray = array[1..Count] of Integer;

var
  Numbers: TIntArray;
  I: Integer;

procedure Dump(var A: TIntArray);
var
  I: Integer;
begin
  for I := 1 to Count do
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

  WriteLn('Generating ', Count, ' random numbers...');
  WriteLn;
  for I := 1 to Count do
    Numbers[I] := Random(Count);
  
  Dump(Numbers);

  WriteLn('Sorting array of ', Count, ' numbers...');
  WriteLn;
  Sort(Numbers, 1, Count);

  Dump(Numbers);

  WriteLn('Done.');
end.