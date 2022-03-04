program Bubble;

const
  Count = 200;

type
  TArray = array[Count] of Integer;

var
  Numbers: TArray;
  I: Integer;

procedure Dump(var A: TArray);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Write(A[I], ' ');
  WriteLn;
  WriteLn;
end;

procedure Sort(var A: TArray);
var
  I, J, Temp: Integer;
  Changed: Boolean;
begin
  for I := Count - 1 downto 1 do
  begin
    Changed := False;
    for J := 0 to I - 1 do
    begin
      if (A[J] > A[J + 1]) then
      begin
        Temp := A[J];
        A[J] := A[J+1];
        A[J+1] := Temp;
        Changed := True;
      end;
    end;
    if not Changed then Exit;
  end;
end;

begin
  WriteLn('*** Bubblesort ***');
  WriteLn;

  WriteLn('Generating', Count, ' random numbers...');
  WriteLn;
  for I := 0 to Count - 1 do
    Numbers[I] := Random(Count);
  
  Dump(Numbers);

  WriteLn('Sorting array of', Count, ' numbers...');
  WriteLn;
  Sort(Numbers);

  Dump(Numbers);

  WriteLn('Done.');
end.