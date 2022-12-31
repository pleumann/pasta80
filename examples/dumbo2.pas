program Dumbo;

const
  Colors: array[4] of Integer = (
    224, 252, 28, 31
  );

  UDGs: array[80] of Integer = (
    $00,$00,$00,$18,$24,$0F,$1B,$3F,
    $00,$00,$00,$30,$48,$E0,$B0,$F8,
    $78,$BF,$BF,$BF,$2A,$4A,$92,$94,
    $3C,$FA,$FA,$FA,$A8,$A4,$94,$A4,
    $78,$BF,$BF,$BF,$2A,$4A,$52,$4A,
    $3C,$FA,$FA,$FA,$A8,$A4,$94,$52,
    $00,$20,$10,$08,$04,$8F,$9B,$BF,
    $00,$08,$10,$20,$40,$E2,$B2,$FA,
    $78,$38,$3C,$3F,$2A,$4A,$92,$94,
    $3C,$38,$78,$F8,$A8,$A4,$94,$52
  );

  Values: array[10] of array[10] of Integer = (
    (2, 5, 2, 4, 2, 5, 5, 3, 3, 1),
    (1, 1, 3, 5, 6, 2, 5, 8, 8, 1),
    (2, 8, 3, 8, 3, 5, 3, 8, 6, 3),
    (1, 6, 6, 2, 3, 1, 2, 3, 6, 5),
    (6, 8, 4, 7, 8, 3, 5, 8, 2, 5),
    (2, 1, 8, 5, 6, 8, 4, 3, 6, 7),
    (6, 8, 7, 4, 2, 1, 2, 8, 3, 1),
    (5, 3, 8, 7, 2, 4, 7, 8, 1, 1),
    (2, 2, 5, 5, 4, 8, 2, 8, 7, 5),
    (8, 5, 2, 8, 5 ,5, 7, 1, 3, 1)      
  );

  FrontBuffer: Integer = 18;
  BackBuffer: Integer = 24;

var
  Flashes, Round, Total, I, J: Integer;
  Dir: Boolean;

procedure HLine(X, Y, Length, Color: Integer);
var
  I: Integer;
begin
  for I := X to X + Length - 1 do
    SetPixel(I, Y, Color);
end;

procedure VLine(X, Y, Length, Color: Integer);
var
  I: Integer;
begin
  for I := Y to Y + Length - 1 do
    SetPixel(X, I, Color);
end;

procedure Dump;
var
  I, J, V, C: Integer;
begin
  for I := 0 to 9 do
  begin
    C := I mod 4;
    GotoXY(2, 3 + I * 2);
    for J := 0 to 9 do
    begin
      TextColor(Colors[C]);
      V := Values[I][J];
      if V > 9 then
      begin
        Values[I][J] := 0;
        TextBackground(255);
        Write(#150#151#8#8#10#152#153#11);
      end
      else if Dir then
      begin
        TextBackground(182);
        Write(#144#145#8#8#10#146#147#11)
      end
      else
      begin
        TextBackground(182);
        Write(#144#145#8#8#10#148#149#11);
      end;
      C := (C + 1) mod 4;
    end;
    Dir := not Dir;
  end;
  Dir := not Dir;
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

procedure Setup;
var
  I: Integer;
begin
  TextColor(0);
  TextBackground(182);
  ClrScr;

  TextBackground(0);
  TextColor(255);

  Write('AoC 2021.11 Dumbo Octopus       ');

  for I := 0 to 7 do
  begin
    HLine(208 + I, 7 - I, 8, 224);
    HLine(216 + I, 7 - I, 8, 252);
    HLine(224 + I, 7 - I, 8, 28);
    HLine(232 + I, 7 - I, 8, 31);
  end;

  TextColor(255);
  TextBackground(0);

  GotoXY(24, 8);
  Write('Part 1 ');
  HLine(183, 72, 58, 0);
  VLine(183, 56, 17, 0);
  VLine(240, 56, 17, 0);

  GotoXY(24, 16);
  Write('Part 2 ');
  HLine(183, 136, 58, 0);
  VLine(183, 120, 17, 0);
  VLine(240, 120, 17, 0);

  TextColor(0);
  TextBackground(255);

  GotoXY(24, 9);
  Write(' 0     ');

  GotoXY(24, 17);
  Write(' 0     ');

  TextColor(0);
  TextBackground(182);
end;

procedure SwitchBuffers;
var
  Temp: Integer;
begin
  Temp := FrontBuffer;
  FrontBuffer := BackBuffer;
  BackBuffer := Temp;
  SetBackBuffer(BackBuffer);
  SetFrontBuffer(FrontBuffer);
  WaitForVSync;  
end;

begin
  SetFrontBuffer(FrontBuffer);
  SetBackBuffer(BackBuffer);
  Setup;
  SwitchBuffers;
  Setup;

  for I := 0 to 79 do
    Mem[-168 + I] := UDGs[I];

  Round := 0;
  Total := 0;

  Dir := False;

  Dump;

  SwitchBuffers;

  repeat
    Round := Round + 1;
    Flashes := 0;

    for I := 0 to 9 do
      for J := 0 to 9 do
        Cycle(I, J);

    if Round <= 100 then Total := Total + Flashes;

    TextColor(0);
    TextBackground(255);

    GotoXY(24, 9);
    Write(Total);

    GotoXY(24, 17);
    Write(Round);

    Dump;

    SwitchBuffers;
  until Flashes = 100;

  SetFrontBuffer(18);
  SetBackBuffer(18);

  TextBackground(182);
  TextColor(0);

  GotoXY(32, 24); Write(' ');
end.