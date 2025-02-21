program TxtFiles;

var
  T: Text;

procedure DumpLines;
var
  S: String;
  I: Integer;
begin
  WriteLn;

  I := 0;

  Reset(T);
  while not Eof(T) do
  begin
    ReadLn(T, S);
    WriteLn(S);
    Inc(I);
  end;
  Close(T);

  WriteLn('-- File has ', I, ' lines ---');
end;

procedure DumpChars;
var
  C: Char;
  I: Integer;
begin
  WriteLn;

  I := 0;
  
  Reset(T);
  while not Eof(T) do
  begin
    Read(T, C);
    Write(C);
    Inc(I);
  end;
  Close(T);

  WriteLn('-- File has ', I, ' chars ---');
end;

begin
  Assign(T, 'monty.txt');
  Erase(T);

  WriteLn;
  WriteLn('[Writing new file]');

  Rewrite(T);
  WriteLn(T, 'Why did Monty die so fast?');
  Close(T);

  DumpLines;

  WriteLn;
  WriteLn('[Appending to existing file]');

  Append(T);
  WriteLn(T, 'Aren''t three lives enough to last');
  Close(T);

  DumpLines;

  WriteLn;
  WriteLn('[Appending to existing file]');

  Append(T);
  WriteLn(T, 'The hazards that confront a mole');
  WriteLn(T, 'In his search for precious coal?');
  WriteLn(T, 'Don''t let Monty die in vain,');
  WriteLn(T, 'Press a key and try again!');
  Close(T);

  DumpLines;
  DumpChars;
end.