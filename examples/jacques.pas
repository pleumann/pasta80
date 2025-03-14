program Jaques;

procedure Note(Duration: Real; Pitch: Integer);
begin
  Border((Pitch + 15) mod 8);
  Beep(Duration * 2, Pitch);
end;

var
  I: Integer;
begin
  Border(7);
  TextBackground(7);
  TextColor(0);
  ClrScr;

  for I := 1 to 2 do
  begin
    WriteLn;
    WriteLn('Frere Jacques');

    Note(0.25, 0);
    Note(0.25, 2);
    Note(0.25, 4);
    Note(0.25, 0);
  end;
  
  for I := 1 to 2 do
  begin
    WriteLn;
    WriteLn('Dormez vous?');

    Note(0.25, 4);
    Note(0.25, 5);
    Note(0.5, 7);
  end;
  
  for I := 1 to 2 do
  begin
    WriteLn;
    WriteLn('Sonnez les matines');

    Note(0.125, 7);
    Note(0.125, 9);
    Note(0.125, 7);
    Note(0.125, 5);
    Note(0.25, 4);
    Note(0.25, 0);
  end;
  
  for I := 1 to 2 do
  begin
    WriteLn;
    WriteLn('Ding ding dong');

    Note(0.25, 0);
    Note(0.25, -5);
    Note(0.5, 0);
  end;
end.