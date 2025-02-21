program Dumbo;

const
  Values: array[0..9] of array[0..9] of Integer = (
    (2,5,2,4,2,5,5,3,3,1),
    (1,1,3,5,6,2,5,8,8,1),
    (2,8,3,8,3,5,3,8,6,3),
    (1,6,6,2,3,1,2,3,6,5),
    (6,8,4,7,8,3,5,8,2,5),
    (2,1,8,5,6,8,4,3,6,7),
    (6,8,7,4,2,1,2,8,3,1),
    (5,3,8,7,2,4,7,8,1,1),
    (2,2,5,5,4,8,2,8,7,5),
    (8,5,2,8,5,5,7,1,3,1)      
  );

var
  Flashes, Round, Total, I, J: Integer;

procedure Dump;
var
  I, J, K: Integer;
begin
  WriteLn('Step', Round, ' has', Flashes, ' flashes.');
  for I := 0 to 9 do
  begin
    for J := 0 to 9 do 
    begin
      K := Values[I][J];
      if K > 9 then
      begin
        Write(' *');
        Values[I][J] := 0;
      end
      else Write(K);
    end;  
    WriteLn;
  end;
  WriteLn;
end;

procedure Cycle(I, J: Integer);
begin
  if (I < 0) or (I > 9) or (J < 0) or (J > 9) then Exit;

  if Values[I][J] < 10 then
  begin
    Values[I][J] := Values[I][J] + 1;
    if Values[I][J] > 9 then
    begin
      Flashes := Flashes + 1;

      Cycle(I - 1, J - 1);
      Cycle(I - 1, J    );
      Cycle(I - 1, J + 1);
      Cycle(I    , J - 1);
      Cycle(I    , J + 1);
      Cycle(I + 1, J - 1);
      Cycle(I + 1, J    );
      Cycle(I + 1, J + 1);
    end;
  end;
end;

begin
  WriteLn('*** AoC 2021.11 Dumbo Octopus ***');
  WriteLn;

  Round := 0;
  Total := 0;

  Dump;

  repeat
    Round := Round + 1;
    Flashes := 0;

    for I := 0 to 9 do
      for J := 0 to 9 do
        Cycle(I, J);

    Dump;

    if Round <= 100 then Total := Total + Flashes;
  until Flashes = 100;

  WriteLn('Sum of flashes after 100 steps was', Total, '.');
end.