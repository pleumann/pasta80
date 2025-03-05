{.PA}

{*******************************************************************}
{*  SOURCE CODE MODULE: MC-MOD01                                   *}
{*  PURPOSE:            Display grid and initialize all cells      *}
{*                      in the spread sheet.                       *}
{*******************************************************************}



procedure Grid;
var I: integer;
    Count: Char;
begin
  HighVideo;
  For Count:='A' to FXMax do
  begin
    GotoXY(XPos[Count],1);
    Write(Count);
  end;
  GotoXY(1,2);
  for I:=1 to FYMax do writeln(I:2);
  LowVideo;
  if AutoCalc then  Flash(65,'AutoCalc: ON' ,false)
  else Flash(65,'AutoCalc: OFF',false);
  Flash(33,'  Type / for Commands',false);
end;


procedure Init;
var
  I: ScreenIndex;
  J: Integer;
  LastName: string[2];
begin
  for I:='A' to FXMAX do
  begin
    for J:=1 to FYMAX do
    begin
      with Screen[I,J] do
      begin
        CellStatus:=[Txt];
        Contents:='';
        Value:=0;
        DEC:=2;              { Default number of decimals        }
        FW:=10;              { Default field whith               }
      end;
    end;
  end;
  AutoCalc:=True;
  FX:='A'; FY:=1;            { First field in upper left corner  }
end;

procedure Clear;
begin
  HighVideo;
  GotoXY(1,24); ClrEol;
  Write('Clear this worksheet? (Y/N) ');
  repeat Ch := ReadKey until Upcase(Ch) in ['Y','N'];
  Write(Upcase(Ch));
  if UpCase(Ch)='Y' then
  begin
    ClrScr;
    Init;
    Grid;
  end;
end;