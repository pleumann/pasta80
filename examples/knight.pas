program Knight;

{$a-}

var
  Seen: array[0..24] of Boolean;
  Path: array[0..25] of Char;
  I, J: Byte;
  Total: Integer;

procedure Tour(X, Y, Move: Integer);
var
  I, Z: Integer;
begin
  CheckBreak;

  Z := X * 5 + Y;
  if not Seen[Z] then
  begin
    Seen[Z] := True;
    Path[Move] := Char(65 + Z);

    if Move = 24 then
    begin
      for I := 0 to 24 do
        Write(Path[I]);
      WriteLn;
      Total := Total + 1;
    end
    else
    begin
      if X < 4 then if Y < 3 then Tour(X + 1, Y + 2, Move + 1);
      if X < 3 then if Y < 4 then Tour(X + 2, Y + 1, Move + 1);
      if X < 3 then if Y > 0 then Tour(X + 2, Y - 1, Move + 1);
      if X < 4 then if Y > 1 then Tour(X + 1, Y - 2, Move + 1);

      if X > 0 then if Y < 3 then Tour(X - 1, Y + 2, Move + 1);
      if X > 1 then if Y < 4 then Tour(X - 2, Y + 1, Move + 1);
      if X > 1 then if Y > 0 then Tour(X - 2, Y - 1, Move + 1);
      if X > 0 then if Y > 1 then Tour(X - 1, Y - 2, Move + 1);
    end;

    Seen[Z] := False;
  end;
end;

begin
  WriteLn('*** 5x5 Knight''s Tour ***');
  WriteLn;
  WriteLn('(This might take a while.)');
  WriteLn;

  Total := 0;

  for I := 0 to 24 do
    Seen[I] := False;

  for I := 0 to 4 do
    for J := 0 to 4 do
      Tour(I, J, 0);

  WriteLn;
  WriteLn(Total, ' solutions found.');
end.