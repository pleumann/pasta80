program Jacques;

procedure Note(Duration: Real; Pitch: Integer);
begin
  Border((Pitch + 15) mod 8);
  Beep(Duration * 1.5, Pitch);
end;

var
  I: Integer;
begin
  Border(7);
  TextBackground(7);
  TextColor(0);
  ClrScr;

  WriteLn;
  Write('   1... ');
  Delay(1000);
  Write('2... ');
  Delay(1000);
  Write('3... ');
  Delay(1000);
  WriteLn('here we go!');

  for I := 0 to 1 do
  begin
    GotoXY(10, 6 + 2 * I);
    TextColor(Blue);
    Write('Frere Jacques');

    Note(0.25, 0);
    Note(0.25, 2);
    Note(0.25, 4);
    Note(0.25, 0);
  end;

  for I := 0 to 1 do
  begin
    GotoXY(10, 10 + 2 * I);
    TextColor(Red);
    Write('Dormez vous?');

    Note(0.25, 4);
    Note(0.25, 5);
    Note(0.5, 7);
  end;

  for I := 0 to 1 do
  begin
    GotoXY(8, 14 + 2 * I);
    TextColor(Magenta);
    Write('Sonnez les matines');

    Note(0.125, 7);
    Note(0.125, 9);
    Note(0.125, 7);
    Note(0.125, 5);
    Note(0.25, 4);
    Note(0.25, 0);
  end;

  for I := 0 to 1 do
  begin
    GotoXY(9, 18 + 2 * I);
    TextColor(Green);
    Write('Ding ding dong');

    Note(0.25, 0);
    Note(0.25, -5);
    Note(0.5, 0);
  end;
end.