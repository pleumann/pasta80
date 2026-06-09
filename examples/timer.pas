(**
 * A simple timer using Delay.
 *)
program Timer;

var
  S, I: Integer;
  
begin
  ClrScr;

  WriteLn('*** Timer ***');
  WriteLn;

  Write('How many seconds? ');
  ReadLn(S);
  WriteLn;

  for I := S downto 1 do
  begin
    GotoXY(1, 5);
    Write('Alarm in ', I:5, ' seconds.');
    Delay(1000);
  end;

  WriteLn;
  WriteLn;

  for I := 1 to 3 do
  begin
    WriteLn('Beep! Beep!');
    {$ifndef SYS_CPM}
    Beep(0.25, 12);
    Beep(0.25, 0);
    {$endif}
  end;
end.
