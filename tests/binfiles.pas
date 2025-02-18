program BinFiles;

{$a+}
{$i/Users/joerg/Projekte/pl0/lib/cpm22.pas}

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
end.
