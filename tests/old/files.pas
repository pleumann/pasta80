program Files;

{$a+}
procedure TestRawFiles;
var
  Raw: File;

  Buffer: array[0..255] of Char;

  C: Char;

  Actual: Integer;

begin
  Assign(Raw, 'test.raw');
  Erase(Raw);
  Rewrite(Raw);

  for C := '0' to '9' do
  begin
    WriteLn('Writing record of ', C, 's at pos ', FilePos(Raw));
    FillChar(Buffer, 128, C);
    BlockWrite(Raw, Buffer, 1, Actual);
  end;

  Close(Raw);

  WriteLn('File size: ', FileSize(Raw));

  Reset(Raw);

  Seek(Raw, 4);

  WriteLn('Writing record of As at pos ', FilePos(Raw));
  FillChar(Buffer, 128, 'A');
  BlockWrite(Raw, Buffer, 1, Actual);
  WriteLn('Writing record of Bs at pos ', FilePos(Raw));
  FillChar(Buffer, 128, 'B');
  BlockWrite(Raw, Buffer, 1, Actual);

  Seek(Raw, FileSize(Raw));

  WriteLn('Writing 2 records of Zs at pos ', FilePos(Raw));
  FillChar(Buffer, 256, 'Z');
  BlockWrite(Raw, Buffer, 2, Actual);

  Close(Raw);

  WriteLn('File size: ', FileSize(Raw));

  WriteLn('Let''s walk through the file contents...');

  Reset(Raw);

  while not Eof(Raw) do
  begin
    Write('#', FilePos(Raw), ': ');
    BlockRead(Raw, Buffer, 1, Actual);
    WriteLn(Buffer[0], '..', Buffer[127]);
  end;

  Close(Raw);
end;

procedure TestTxtFiles;
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
end;

procedure TestBinFiles;
type
  ComputerRec = record
    Name: String[12];
    Year: Integer;
    Cool: Boolean;
  end;

var
  Bin: file of ComputerRec;

  C: ComputerRec;
  I: Integer;

begin
  Assign(Bin, 'test.dat');
  Erase(Bin);

  Rewrite(Bin);

  with C do
  begin
    Name := 'Apple II';
    Year := 1977;
    Cool := True;
  end;

  Write(Bin, C);

  with C do
  begin
    Name := 'IBM PC';
    Year := 1981;
    Cool := False;
  end;

  Write(Bin, C);
  with C do
  begin
    Name := 'ZX Spectrum';
    Year := 1982;
    Cool := True;
  end;

  Write(Bin, C);

  with C do
  begin
    Name := 'Commodore 64';
    Year := 1982;
    Cool := False;
  end;

  Write(Bin, C);

  with C do
  begin
    Name := 'Archimedes';
    Year := 1987;
    Cool := True;
  end;

  Write(Bin, C);

  Close(Bin);

  WriteLn;

  Reset(Bin);
  while not Eof(Bin) do
  begin
    Read(Bin, C);
    Write('-> ', C.Name, ' (', C.Year, ', ');
    if C.Cool then WriteLn('cool)') else WriteLn('uncool)');
  end;
  Close(Bin);

  WriteLn;
  WriteLn('Let''s read the file in reverse order using seek...');
  WriteLn;

  Reset(Bin);
  for I := FileSize(Bin) - 1 downto 0 do
  begin
    Seek(Bin, I);
    Read(Bin, C);
    Write('-> ', C.Name, ' (', C.Year, ', ');
    if C.Cool then WriteLn('cool)') else WriteLn('uncool)');
  end;
  Close(Bin);
end;

begin
  (*TestRawFiles;*)
  TestBinFiles;
  (*TestTxtFiles;*)
end.