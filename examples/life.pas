program Life;
const
  Width    = 30;
  Height   = 20;
  Percent  = 10;

var
  Map: array[0..1] of array[0..19] of array[0..29] of Integer;
  This, Next, Cycle, Live: Integer;

procedure Paint;
var
  X,Y: Integer;
begin
  GotoXY(1,1);
  for Y := 0 to Height - 1 do
  begin
    for X := 0 to Width - 1 do
      if Map[This][Y][X] = 1 then
        Write('#')
      else
        Write(' ');
    WriteLn;
  end;
  WriteLn;
end;

function Alive(X,Y: Integer): Boolean;
var
  L,R,U,D,Count: Integer;
begin
  if X = 0 then L := Width - 1 else L := X-1;
  if Y = 0 then U := Height - 1 else U := Y-1;

  if X = Width-1 then R := 0 else R := X+1;
  if Y = Height-1 then D := 0 else D := Y+1;

  Count := Map[This][U][L] + Map[This][U][X] + Map[This][U][R]
         + Map[This][Y][L]                   + Map[This][Y][R]
         + Map[This][D][L] + Map[This][D][X] + Map[This][D][R];

  Alive := (Map[This][Y][X]  = 1) and (Count=2) or (Count=3);
end;

procedure Think;
var
  X,Y,Temp: Integer;
begin
  Temp := This; This := Next; Next := Temp;

  Live := 0;

  for Y := 0 to Height - 1 do
    for X := 0 to Width  - 1 do
      if Alive(X,Y) then
      begin
        Map[Next][Y][X] := 1;
        Live := Live + 1;
      end
      else
        Map[Next][Y][X] := 0;
end;

procedure Setup;
var
  X,Y: Integer;
begin
  This := 0; Next := 1;

  for Y := 0 to Height - 1 do
    for X := 0 to Width - 1 do
      if Random(100) < Percent then
        Map[Next][Y][X] := 1
      else
        Map[Next][Y][X] := 0
end;
  
begin
  ClrScr;
  Setup;

  for Cycle := 1 to 800 do
  begin
    TextColor(1 + Cycle mod 6);
    CursorOff;
    Paint;
    TextColor(7);
    Write('Cycle: ', Cycle,'  Alive: ', Live, '  ');
    CursorOn;

    Think;
  end;
end.