program Timer;

var
  S, I: Integer;
  
begin
  Write('How many seconds (0..32767)? ');
  ReadLn(S);
  WriteLn;

  for I := S downto 1 do
  begin
    WriteLn(I);
    Delay(1000);
  end;

  WriteLn;
  WriteLn('Beep! Beep! Beep!');
end.
