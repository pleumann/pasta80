{.PA}
{*******************************************************************}
{*  SOURCE CODE MODULE: MC-MOD00                                   *}
{*  PURPOSE:            Micellaneous utilities an commands.        *}
{*******************************************************************}


procedure Msg(S: AnyString);
begin
  GotoXY(1,24);
  ClrEol;
  Write(S);
end;

procedure Flash(X: integer; S: AnyString;  Blink: boolean);
begin
  HighVideo;
  GotoXY(X,23);
  Write(S);
  if Blink then
  begin
    repeat
      GotoXY(X,23);
      Blink:=not Blink; if Blink then HighVideo else LowVideo;
      Write(S);
      Delay(175);
    until KeyPressed;
  end;
  LowVideo;
end;

procedure Auto;
begin
  AutoCalc:=not AutoCalc;
  if AutoCalc then  Flash(65,'AutoCalc: ON ',false)
  else Flash(65,'AutoCalc: OFF',false);
end;