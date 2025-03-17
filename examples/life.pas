program Life;

const
  Width    = 32;
  Height   = 20;
  Percent  = 20;

var
  Map: array[0..1] of array[0..19] of array[0..31] of Byte;
  This, Next, Cycle, Live, Color: Integer;
  Cell: string[2];
  C: Char;

procedure Paint;
var
  X, Y: Integer;
begin
  for Y := 0 to Height - 1 do
  begin
    GotoXY(8, 1 + Y);
    for X := 0 to Width - 1 do
      if Map[This][Y][X] = 1 then
      begin
        TextBackground(Color);
        Write(Cell);
        TextBackground(Black);
      end
      else
        Write(Cell);
  end;

  Inc(Color);
  if (Color = 8) then Color := 1;
end;

function Alive(X,Y: Integer): Boolean;
var
  L, R, U, D, Count: Integer;
begin
  if X = 0 then L := Width - 1 else L := X - 1;
  if Y = 0 then U := Height - 1 else U := Y - 1;

  if X = Width-1 then R := 0 else R := X + 1;
  if Y = Height-1 then D := 0 else D := Y + 1;

  Count := Map[This][U][L] + Map[This][U][X] + Map[This][U][R]
         + Map[This][Y][L]                   + Map[This][Y][R]
         + Map[This][D][L] + Map[This][D][X] + Map[This][D][R];

  Alive := (Map[This][Y][X] = 1) and (Count=2) or (Count=3);
end;

procedure Think;
var
  X, Y, Temp: Integer;
begin
  Live := 0;

  for Y := 0 to Height - 1 do
    for X := 0 to Width  - 1 do
      if Alive(X, Y) then
      begin
        Map[Next][Y][X] := 1;
        Live := Live + 1;
      end
      else
        Map[Next][Y][X] := 0;

  Temp := This; This := Next; Next := Temp;
end;

procedure Setup;
var
  X, Y: Integer;
begin
  if ScreenWidth = 32 then
  begin
    Cell := ' ';
  end
  else
  begin
    Cell := '  ';
  end;

  Live := 0; This := 0; Next := 1;

  for Y := 0 to Height - 1 do
    for X := 0 to Width - 1 do
      if Random(100) < Percent then
      begin
        Map[This][Y][X] := 1;
        Inc(Live);
      end
      else
        Map[This][Y][X] := 0
end;

begin
  CursorOff;

  ClrScr;
  Randomize;
  Setup;

  Color := 1;

  while not KeyPressed do
  begin
    Inc(Cycle);
    TextColor(1 + Cycle mod 6);
    Paint;
    TextBackground(7);
    TextColor(0);
    GotoXY(8, 22);
    Write(' Cycle:  ', Cycle:3,'  Alive: ', Live:3, '  Press any key to exit. ':40);

    TextBackground(0);
    TextColor(7);

    Think;
  end;

  C := ReadKey;

  CursorOn;
end.