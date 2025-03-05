{.PA}
{*******************************************************************}
{*  SOURCE CODE MODULE: MC-MOD02                                   *}
{*  PURPOSE:            Display values in cells and move between   *}
{*                      cells in the spread sheet.                 *}
{*******************************************************************}


procedure FlashType;
begin
  with Screen[FX,FY] do
  begin
    GotoXY(1,23);
    Write(FX,FY:2,' ');
    if Formula in CellStatus  then write('Formula:')  else
    if Constant in CellStatus then Write('Numeric ') else
    if Txt in CellStatus then Write('Text    ');
    GotoXY(1,24); ClrEol;
    if Formula in CellStatus then Write(Contents);
  end;
end;


{ The following procedures move between the Cells on the calc sheet.}
{ Each Cell has an associated record containing its X,Y coordinates }
{ and data. See the type definition for "Cell".                     }

procedure GotoCell(GX: ScreenIndex; GY: integer);
begin
  with Screen[GX,GY] do
  begin
    HighVideo;
    GotoXY(XPos[GX],GY+1);
    Write('           ');
    GotoXY(XPos[GX],GY+1);
    if Txt in CellStatus then Write(Contents)
    else
    begin
      if DEC>=0 then Write(Value:FW:DEC)
      else Write(Value:FW);
    end;
    FlashType;
    GotoXY(XPos[GX],GY+1);
  end;
  LowVideo;
end;

{.CP20}

procedure LeaveCell(FX:ScreenIndex;FY: integer);
begin
  with Screen[FX,FY] do
  begin
    GotoXY(XPos[FX],FY+1);
    LowVideo;
    if Txt in CellStatus then Write(Contents)
    else
    begin
      if DEC>=0 then Write(Value:FW:DEC)
      else Write(Value:FW);
    end;
  end;
end;


{.CP20}

procedure Update;
var
  UFX: ScreenIndex;
  UFY: integer;
begin
  ClrScr;
  Grid;
  for UFX:='A' to FXMax do for UFY:=1 to FYMax do
  if Screen[UFX,UFY].Contents<>'' then LeaveCell(UFX,UFY);
  GotoCell(FX,FY);
end;

{.CP20}

procedure MoveDown;
var Start: integer;
begin
  LeaveCell(FX,FY);
  Start:=FY;
  repeat
    FY:=FY+1;
    if FY>FYMax then FY:=1;
  until (Screen[FX,FY].CellStatus*[OverWritten,Locked]=[]) or (FY=Start);
  if FY<>Start then GotoCell(FX,FY);
end;

{.CP20}

procedure MoveUp;
var Start: integer;
begin
  LeaveCell(FX,FY);
  Start:=FY;
  repeat
    FY:=FY-1;
    if FY<1 then FY:=FYMax;
  until (Screen[FX,FY].CellStatus*[OverWritten,Locked]=[]) or  (FY=Start);
  if FY<>Start then GotoCell(FX,FY);
end;

{.CP20}

procedure MoveRight;
var Start: ScreenIndex;
begin
  LeaveCell(FX,FY);
  Start:=FX;
  repeat
    FX:=Succ(FX);
    if FX>FXMax then
    begin
      FX:='A';
      FY:=FY+1;
      if FY>FYMax then FY:=1;
    end;
  until (Screen[FX,FY].CellStatus*[OverWritten,Locked]=[]) or (FX=Start);
  if FX<>Start then GotoCell(FX,FY);
end;

{.CP20}

procedure MoveLeft;
var Start: ScreenIndex;
begin
  LeaveCell(FX,FY);
  Start:=FX;
  repeat
    FX:=Pred(FX);
    if FX<'A' then
    begin
      FX:=FXMax;
      FY:=FY-1;
      if FY<1 then FY:=FYMax;
    end;
  until (Screen[FX,FY].CellStatus*[OverWritten,Locked]=[]) or (FX=Start);
  if FX<>Start then GotoCell(FX,FY);
end;