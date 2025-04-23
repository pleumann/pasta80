program Maze;

{$a-}
{$k-}

procedure Recurse(OldX, OldY, X, Y: Integer);
var
  Seen: Byte;
begin
  if Point(X, Y) then Exit;

  Plot(OldX, OldY);
  Draw(X - OldX, Y - OldY);

  Seen := 0;
  while Seen <> 15 do
  begin
    case Random(4) of
      0: if (Seen and 1) = 0 then
         begin
           Recurse(X, Y, X + 5, Y);
           Seen := Seen or 1;
         end;
      1: if (Seen and 2) = 0 then
         begin
           Recurse(X, Y, X - 5, Y);
           Seen := Seen or 2;
         end;
      2: if (Seen and 4) = 0 then
         begin
           Recurse(X, Y, X, Y + 5);
           Seen := Seen or 4;
         end;
      3: if (Seen and 8) = 0 then
         begin
           Recurse(X, Y, X, Y - 5);
           Seen := Seen or 8;
         end;
    end;
  end;
end;

var
  TDraw, TFill: Real;

begin
  Randomize;

  TDraw := 0.0;
  TFill := 0.0;

  repeat
    ClrScr;
    Write('Draw: ', TDraw * 20:5:0, ' ms    Fill: ', TFill * 20:5:0, ' ms');

    TDraw := Frames;

    Plot(0, 10);
    Draw(255, 00);
    Draw(0, 165);
    Draw(-255, 0);
    Draw(0, -165);

    Recurse(125, 85, 125, 85);

    TDraw := Frames - TDraw;

    TFill := Frames;
    FloodFill(126, 86);
    TFill := Frames - TFill;
  until KeyPressed;
end.