{.PA}
{*******************************************************************}
{*  SOURCE CODE MODULE: MC-MOD03                                   *}
{*  PURPOSE:            Read, Save and Print a spread sheet.       *}
{*                      Display on-line manual.                    *}
{*******************************************************************}

type
  String3 = string[3];

var
  FileName: string[14];
  Line: string[100];

function Exist(FileN: AnyString): boolean;
var F: file;
begin
   {$I-}
   assign(F,FileN);
   reset(F);
   {$I+}
   if IOResult<>0 then Exist:=false
   else Exist:=true;
end;


procedure GetFileName(var Line: AnyString; FileType:String3);
begin
  Line:='';
  repeat
    Ch := ReadKey;
    if Upcase(Ch) in ['A'..'Z',^M] then
    begin
      write(Upcase(Ch));
      Line:=Line+Ch;
    end;
  until (Ch=^M) or (length(Line)=8);
  if Ch=^M then Delete(Line,Length(Line),1);
  if Line<>'' then Line:=Line+'.'+FileType;
end;

{.CP20}

procedure Save;
var I: screenIndex;
J: integer;
begin
  HighVideo;
  Msg('Save: Enter filename  ');
  GetFileName(Filename,'MCS');
  if FileName<>'' then
  begin
    Assign(MCFile,FileName);
    Rewrite(MCFile);
    for I:='A' to FXmax do
    begin
      for J:=1 to FYmax do
      write(MCfile,Screen[I,J]);
    end;
    Grid;
    Close(MCFile);
    LowVideo;
    GotoCell(FX,FY);
  end;
end;

{.CP30}

procedure Load;
begin
  HighVideo;
  Msg('Load: Enter filename  ');
  GetFileName(Filename,'MCS');
  if (Filename<>'') then if (not exist(FileName)) then
  repeat
    Msg('File not Found: Enter another filename  ');
    GetFileName(Filename,'MCS');
  until exist(FileName) or (FileName='');
  if FileName<>'' then
  begin
    ClrScr;
    Msg('Please Wait. Loading definition...');
    Assign(MCFile,FileName);
    Reset(MCFile);
    for FX:='A' to FXmax do
     for FY:=1 to FYmax do read(MCFile,Screen[FX,FY]);
    FX:='A'; FY:=1;
    LowVideo;
    UpDate;
  end;
  GotoCell(FX,FY);
end;


{.PA}

procedure Print;
var
  I:      screenIndex;
  J,Count,
  LeftMargin: Integer;
  P:          string[20];
  MCFile:     Text;
begin
  HighVideo;
  Msg('Print: Enter filename "P" for Printer> ');
  GetFileName(Filename,'LST');
  Msg('Left margin > ');  Read(LeftMargin);
  if FileName='P.LST' then FileName:='Printer';
  Msg('Printing to: ' + FileName + '....');
  Assign(MCFile,FileName);
  Rewrite(MCFile);
  For Count:=1 to 5 do Writeln(MCFile);
  for J:=1 to FYmax do
  begin
    Line:='';
    for I:='A' to FXmax do
    begin
      with Screen[I,J] do
      begin
        while (Length(Line)<XPOS[I]-4) do Line:=Line+' ';
        if (Constant in CellStatus) or (Formula in CellStatus) then
        begin
          if not (Locked in CellStatus) then
          begin
            if DEC>0 then Str(Value:FW:DEC,P) else Str(Value:FW,P);
            Line:=Line+P;
          end;
        end else Line:=Line+Contents;
      end; { With }
    end; { One line }
    For Count:=1 to LeftMargin do Write(MCFile,' ');
    writeln(MCFile,Line);
  end; { End Column }
  Grid;
  Close(MCFile);
  LowVideo;
  GotoCell(FX,FY);
end;

{.PA}

procedure Help;
var
  H: text;
  HelpFileName: string[14];
  Line: string[80];
  I,J: integer;
  Bold: boolean;

begin
  if Exist('MC.HLP') then
  begin
    Assign(H,'MC.HLP');
    Reset(H);
    while not Eof(H) do
    begin
      Readln(H,Line);
      ClrScr; I:=1; Bold:=false; LowVideo;
      repeat
        For J:=1 to Length(Line) do
        begin
          if Line[J]=^B then
          begin
            Bold:=not Bold;
            if Bold then HighVideo else LowVideo;
          end else write(Line[J]);
        end;
        Writeln;
        I:=I+1;
        Readln(H,Line);
      until Eof(H) or (I>23) or (Copy(Line,1,3)='.PA');
      GotoXY(26,24); HighVideo;
      write('<<< Please press any key to continue >>>');
      LowVideo;
      Ch := ReadKey;
    end;
    GotoXY(20,24); HighVideo;
    write('<<< Please press <RETURN> to start MicroCalc >>>');
    LowVideo;
    Readln(Ch);
    UpDate;
  end else { Help file did not exist }
  begin
    Msg('To get help the file MC.HLP must be on your disk. Press <RETURN>');
    repeat Ch := ReadKey until Ch=^M;
    GotoCell(FX,FY);
  end;
end;