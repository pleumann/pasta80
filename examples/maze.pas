program Maze;

{$a-}
{$k-}

procedure Recurse(OldX, OldY, X, Y: Integer);
var
  Seen: Byte;
begin
  if (X < 0) or (X > 255) or (Y < 0) or (Y > 175) then Exit;
  if Point(X, Y) then Exit;

  Draw(OldX, OldY, X, Y);

  Seen := 0;
  while Seen <> 15 do
  begin
    case Random(4) of
      0: if (Seen and 1) = 0 then
         begin
           Recurse(X, Y, X + 4, Y);
           Seen := Seen or 1;
         end;
      1: if (Seen and 2) = 0 then
         begin
           Recurse(X, Y, X - 4, Y);
           Seen := Seen or 2;
         end;
      2: if (Seen and 4) = 0 then
         begin
           Recurse(X, Y, X, Y + 4);
           Seen := Seen or 4;
         end;
      3: if (Seen and 8) = 0 then
         begin
           Recurse(X, Y, X, Y - 4);
           Seen := Seen or 8;
         end;
    end;
  end;
end;

begin
  ClrScr;
  Randomize;
  Recurse(128, 88, 128, 88);
end.