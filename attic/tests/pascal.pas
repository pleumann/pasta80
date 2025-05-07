program Pascal;

{$a+}
{$i/Users/joerg/Projekte/pl0/lib/cpm22.pas}

type
  ComputerRec = record
    Name: String[12];
    Year: Integer;
    Cool: Boolean;
  end;

var
  F: FileRec;
  C: ComputerRec;
  I: Integer;

begin
  FileAssign(F, 'test.dat', SizeOf(ComputerRec));
  BlockErase(F.FCB);

  FileRewrite(F);

  with C do
  begin
    Name := 'Apple II';
    Year := 1977;
    Cool := True;
  end;

  FileWrite(F, C);

  with C do
  begin
    Name := 'VIC-20';
    Year := 1980;
    Cool := False;
  end;

  FileWrite(F, C);

  with C do
  begin
    Name := 'ZX 81';
    Year := 1981;
    Cool := True;
  end;

  FileWrite(F, C);

  with C do
  begin
    Name := 'IBM PC';
    Year := 1981;
    Cool := False;
  end;

  FileWrite(F, C);
  with C do
  begin
    Name := 'ZX Spectrum';
    Year := 1982;
    Cool := True;
  end;

  FileWrite(F, C);

  with C do
  begin
    Name := 'Commodore 64';
    Year := 1982;
    Cool := False;
  end;

  FileWrite(F, C);

  with C do
  begin
    Name := 'Atari ST';
    Year := 1985;
    Cool := True;
  end;

  FileWrite(F, C);

  with C do
  begin
    Name := 'Amiga';
    Year := 1985;
    Cool := True;
  end;

  FileWrite(F, C);

  with C do
  begin
    Name := 'Archimedes';
    Year := 1987;
    Cool := True;
  end;

  FileWrite(F, C);

  FileClose(F);

  WriteLn;

  FileReset(F);
  while not FileEof(F) do
  begin
    FileRead(F, C);
    Write('-> ', C.Name, ' (', C.Year, ', ');
    if C.Cool then WriteLn('cool)') else WriteLn('uncool)');
  end;
  FileClose(F);

  WriteLn;
  WriteLn('Let''s read the file in reverse order using seek...');
  WriteLn;

  FileReset(F);
  for I := FileSize(F) - 1 downto 0 do
  begin
    FileSeek(F, I);
    FileRead(F, C);
    Write('-> ', C.Name, ' (', C.Year, ', ');
    if C.Cool then WriteLn('cool)') else WriteLn('uncool)');
  end;
  FileClose(F);
end.