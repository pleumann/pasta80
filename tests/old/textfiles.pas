program TextTest;

{$a+}
{$i /Users/joerg/Projekte/pl0/lib/cpm22.pas}

var
  T: TextRec;

procedure DumpLines(var T: TextRec);
var
  S: String;
  I: Integer;
begin
  WriteLn;

  I := 0;

  TextReset(T);
  while not TextEof(T) do
  begin
    TextReadLine(T, S);
    WriteLn(S);
    Inc(I);
  end;
  TextClose(T);

  WriteLn('-- File has ', I, ' lines in ', BlockFileSize(T.FCB), ' blocks ---');
end;

procedure DumpChars(var T: TextRec);
var
  C: Char;
  I: Integer;
begin
  WriteLn;

  I := 0;
  
  TextReset(T);
  while not TextEof(T) do
  begin
    TextReadChar(T, C);
    Write(C);
    Inc(I);
  end;
  TextClose(T);

  WriteLn('-- File has ', I, ' chars in ', BlockFileSize(T.FCB), ' blocks ---');
end;

begin
  BlockAssign(T.FCB, 'test.txt');
  BlockErase(T.FCB);

  WriteLn;
  WriteLn('[Writing new file]');

  TextRewrite(T);
  TextWriteLine(T, 'Why did Monty die so fast?');
  TextClose(T);

  DumpLines(T);

  WriteLn;
  WriteLn('[Appending to existing file]');

  TextAppend(T);
  TextWriteLine(T, 'Aren''t three lives enough to last');
  TextClose(T);

  DumpLines(T);

  WriteLn;
  WriteLn('[Appending to existing file]');

  TextAppend(T);
  TextWriteLine(T, 'The hazards that confront a mole');
  TextWriteLine(T, 'In his search for precious coal?');
  TextWriteLine(T, 'Don''t let Monty die in vain,');
  TextWriteLine(T, 'Press a key and try again!');
  TextClose(T);

  DumpLines(T);
  DumpChars(T);
end.