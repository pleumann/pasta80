program Files;

{$a+}

type
  Color = (Red, Green, Blue, Yellow);
  ComputerRec = record
    Name: String[12];
    Year: Integer;
    Cool: Boolean;
  end;

var
  F: Text;
  F2: Text;
  I: Integer;
  R: Real;
  C: Color;
  B: Boolean;
  RawFile: File;
  BinFile: file of ComputerRec;
  Buffer: array[0..255] of Char;
  Actual: Integer;
  ComputerRecVar: ComputerRec;
  S: String;
  Exists: Boolean;

function FileExists(Filename: String): Boolean;
var
  TestFile: File;
  Result: Integer;
  Dummy: Integer;
begin
  {$i-}
  Assign(TestFile, Filename);
  Reset(TestFile);
  {$i+}
  Result := IOResult;
  if Result = 0 then
  begin
    {$i-}
    Close(TestFile);
    {$i+}
    Dummy := IOResult; { Discard close error }
  end;
  FileExists := Result = 0;
end;

(* Overlay 0 *)

overlay procedure TestFileReadInt;
var
  I1, I2, I3: Integer;
begin
  WriteLn('--- TestFileReadInt ---');

  Assign(F, 'INT.TMP');
  Rewrite(F);
  WriteLn(F, '  42  ');
  WriteLn(F, '-123');
  WriteLn(F, '0');
  Close(F);

  Reset(F);
  ReadLn(F, I1);
  ReadLn(F, I2);
  ReadLn(F, I3);
  Close(F);

  Assert(I1 = 42);
  Assert(I2 = -123);
  Assert(I3 = 0);

  Erase(F);
end;

overlay procedure TestFileReadFloat;
var
  R1, R2, R3: Real;
  B1, B2: Boolean;
begin
  WriteLn('--- TestFileReadFloat ---');

  Assign(F, 'FLT.TMP');
  Rewrite(F);
  WriteLn(F, '  3.14  ');
  WriteLn(F, '-2.5');
  WriteLn(F, '0.0');
  Close(F);

  Reset(F);
  ReadLn(F, R1);
  ReadLn(F, R2);
  ReadLn(F, R3);
  Close(F);

  B1 := (R1 > 3.1) and (R1 < 3.2);
  Assert(B1);
  B2 := (R2 > -2.6) and (R2 < -2.4);
  Assert(B2);
  Assert(R3 = 0.0);

  Erase(F);
end;

overlay procedure TestFileReadEnum;
var
  C1, C2, C3, C4: Color;
begin
  WriteLn('--- TestFileReadEnum ---');

  Assign(F, 'ENM.TMP');
  Rewrite(F);
  Write(F, Red, ' ', Green, ' ', Blue, ' ', Yellow);
  Close(F);

  Reset(F);
  Read(F, C1);
  Read(F, C2);
  Read(F, C3);
  Read(F, C4);
  Close(F);

  Assert(C1 = Red);
  Assert(C2 = Green);
  Assert(C3 = Blue);
  Assert(C4 = Yellow);

  Erase(F);
end;

overlay procedure TestFileSeekEoln;
var
  I1: Integer;
  B1, B2, B3: Boolean;
begin
  WriteLn('--- TestFileSeekEoln ---');

  Assign(F, 'EOL.TMP');
  Rewrite(F);
  WriteLn(F, '  42  ');
  WriteLn(F, '  ');
  Close(F);

  Reset(F);

  { Before reading: spaces before '42', SeekEoln should skip them and return False }
  B1 := SeekEoln(F);
  B1 := not B1;
  Assert(B1);

  { Read the number, then spaces remain before CR -> SeekEoln returns True }
  Read(F, I1);
  B2 := SeekEoln(F);
  Assert(B2);
  Assert(I1 = 42);

  ReadLn(F);

  { Second line is all whitespace -> SeekEoln should return True }
  B3 := SeekEoln(F);
  Assert(B3);

  Close(F);
  Erase(F);
end;

overlay procedure TestFileSeekEof;
var
  I1, I2: Integer;
  B1, B2, B3: Boolean;
begin
  WriteLn('--- TestFileSeekEof ---');

  Assign(F, 'EOF.TMP');
  Rewrite(F);
  WriteLn(F, '  42  ');
  WriteLn(F, '  ');
  WriteLn(F, '  99');
  Close(F);

  Reset(F);

  { First line has a number -> SeekEof should skip whitespace, find digit, return False }
  B1 := SeekEof(F);
  B1 := not B1;
  Assert(B1);

  ReadLn(F, I1);
  Assert(I1 = 42);

  { Second line is all whitespace, third has a number -> SeekEof skips both, returns False }
  B2 := SeekEof(F);
  B2 := not B2;
  Assert(B2);

  ReadLn(F, I2);
  Assert(I2 = 99);

  { Past last line -> SeekEof should return True }
  B3 := SeekEof(F);
  Assert(B3);

  Close(F);
  Erase(F);
end;

overlay procedure TestRawFileBlockIO;
const
  Expected = '01234AB6789ZZ';
var
  Idx: Integer;
  Ch: Char;
  FS: Integer;
begin
  WriteLn('--- TestRawFileBlockIO ---');

  Assign(RawFile, 'RAW.TMP');
  {$i-}
  Erase(RawFile);
  {$i+}
  Rewrite(RawFile);

  { Write 10 blocks with characters '0'..'9' }
  for Ch := '0' to '9' do
  begin
    FillChar(Buffer, 128, Ch);
    BlockWrite(RawFile, Buffer, 1, Actual);
    Assert(Actual = 1);
  end;

  FS := FileSize(RawFile);
  Assert(FS = 10);

  Close(RawFile);

  { Reopen in update mode, seek to position 4 }
  Reset(RawFile);
  Seek(RawFile, 4);

  { Overwrite with 'A' }
  FillChar(Buffer, 128, 'A');
  BlockWrite(RawFile, Buffer, 1, Actual);
  Assert(Actual = 1);

  { Overwrite with 'B' }
  FillChar(Buffer, 128, 'B');
  BlockWrite(RawFile, Buffer, 1, Actual);
  Assert(Actual = 1);

  { Seek to end and write 2 blocks of 'Z' }
  Seek(RawFile, FileSize(RawFile));
  FillChar(Buffer, 256, 'Z');
  BlockWrite(RawFile, Buffer, 2, Actual);
  Assert(Actual = 2);

  FS := FileSize(RawFile);
  Assert(FS = 12);

  Close(RawFile);

  { Verify by reading back }
  Reset(RawFile);

  Idx := 0;
  while not Eof(RawFile) do
  begin
    BlockRead(RawFile, Buffer, 1, Actual);
    WriteLn('Record #', Idx, ': ', Buffer[0], '...', Buffer[127]);
    Assert(Actual = 1);
    // FIXME Assert(Buffer[0] = Expected[Idx]);
    Inc(Idx);
  end;
  Assert(Idx = 12);

  Close(RawFile);
  Erase(RawFile);
end;

overlay procedure TestTextFileAppend;
var
  LineCount: Integer;
  CharCount: Integer;
  TempCh: Char;
begin
  WriteLn('--- TestTextFileAppend ---');

  Assign(F, 'TXT.TMP');
  {$i-}
  Erase(F);
  {$i+}

  { Initial Rewrite }
  Rewrite(F);
  WriteLn(F, 'Why did Monty die so fast?');
  Close(F);

  { First Append }
  Append(F);
  WriteLn(F, 'Aren''t three lives enough to last');
  Close(F);

  { Second Append }
  Append(F);
  WriteLn(F, 'The hazards that confront a mole');
  WriteLn(F, 'In his search for precious coal?');
  WriteLn(F, 'Don''t let Monty die in vain,');
  WriteLn(F, 'Press a key and try again!');
  Close(F);

  { Verify line count }
  Reset(F);
  LineCount := 0;
  while not Eof(F) do
  begin
    ReadLn(F, S);
    WriteLn(S);
    Inc(LineCount);
  end;
  Close(F);
  Assert(LineCount = 6);

  { Verify character count }
  Reset(F);
  CharCount := 0;
  while not Eof(F) do
  begin
    Read(F, TempCh);
    Write(TempCh);
    Inc(CharCount);
  end;
  Close(F);
  Assert(CharCount = 189);

  Erase(F);
end;

overlay procedure TestTypedFileIO;
const
  CoolStr: array[Boolean] of String = ('Uncool', 'Cool');
var
  RecCount, RecIdx, FS, LastYear: Integer;
  LastCool: Boolean;
begin
  WriteLn('--- TestTypedFileIO ---');

  Assign(BinFile, 'BIN.TMP');
  {$i-}
  Erase(BinFile);
  {$i+}

  Rewrite(BinFile);

  { Write 5 computer records }
  with ComputerRecVar do
  begin
    Name := 'Apple II';
    Year := 1977;
    Cool := True;
  end;
  Write(BinFile, ComputerRecVar);

  with ComputerRecVar do
  begin
    Name := 'IBM PC';
    Year := 1981;
    Cool := False;
  end;
  Write(BinFile, ComputerRecVar);

  with ComputerRecVar do
  begin
    Name := 'ZX Spectrum';
    Year := 1982;
    Cool := True;
  end;
  Write(BinFile, ComputerRecVar);

  with ComputerRecVar do
  begin
    Name := 'Commodore 64';
    Year := 1982;
    Cool := False;
  end;
  Write(BinFile, ComputerRecVar);

  with ComputerRecVar do
  begin
    Name := 'Archimedes';
    Year := 1987;
    Cool := True;
  end;
  Write(BinFile, ComputerRecVar);

  Close(BinFile);

  { Verify FileSize }
  Reset(BinFile);
  FS := FileSize(BinFile);
  Assert(FS = 5);

  { Read forward and verify first record }
  Read(BinFile, ComputerRecVar);
  Assert(ComputerRecVar.Year = 1977);
  Assert(ComputerRecVar.Cool = True);

  { Seek to position 4 (last record) and read }
  Seek(BinFile, 4);
  Read(BinFile, ComputerRecVar);
  Assert(ComputerRecVar.Year = 1987);
  Assert(ComputerRecVar.Cool = True);

  { Verify reverse read using Seek }
  RecCount := 0;
  LastYear := 9999;
  LastCool := False;
  for RecIdx := FS - 1 downto 0 do
  begin
    Seek(BinFile, RecIdx);
    Read(BinFile, ComputerRecVar);
    with ComputerRecVar do
      WriteLn('#', RecCount, ': ', Name, ' (', Year, ', ', CoolStr[Cool], ')');
    Inc(RecCount);
    Assert(ComputerRecVar.Year <= LastYear);
    Assert(ComputerRecVar.Cool = not LastCool);
    LastYear := ComputerRecVar.Year;
    LastCool := ComputerRecVar.Cool;
  end;
  Assert(RecCount = FS);

  Close(BinFile);
  Erase(BinFile);
end;

overlay procedure TestFileErase;
var
  TestFile: Text;
begin
  WriteLn('--- TestFileErase ---');

  Assign(TestFile, 'ERS.TMP');
  {$i-}
  Erase(TestFile);
  {$i+}

  { Create file by writing }
  Rewrite(TestFile);
  WriteLn(TestFile, 'This file will be erased');
  Close(TestFile);

  { Verify it exists }
  Assert(FileExists('ERS.TMP'));

  { Erase it }
  Erase(TestFile);

  { Verify it no longer exists }
  Assert(not FileExists('ERS.TMP'));

  { Verify no error when erasing already-erased file }
  {$i-}
  Erase(TestFile);
  {$i+}

  Assert(True);
end;

overlay procedure TestFileRename;
var
  DummyFile: File;
begin
  WriteLn('--- TestFileRename ---');

  { Clean up old files if they exist }
  Assign(DummyFile, 'OLD.TMP');
  {$i-}
  Erase(DummyFile);
  {$i+}

  { Create file }
  Rewrite(DummyFile);
  BlockWrite(DummyFile, Buffer, 1, Actual);
  Close(DummyFile);

  { Verify original file exists }
  Assert(FileExists('OLD.TMP'));

  { Rename file }
  Rename(DummyFile, 'NEW.TMP');

  { Verify old name no longer exists }
  Assert(not FileExists('OLD.TMP'));

  { Verify new name exists }
  Assert(FileExists('NEW.TMP'));

  { Clean up }
  Assign(DummyFile, 'NEW.TMP');
  Erase(DummyFile);

  { Verify cleanup successful }
  Assert(not FileExists('NEW.TMP'));
end;

begin
  WriteLn;
  WriteLn('*** PASTA/80 Test Suite ***');
  WriteLn;

  TestFileReadInt;
  TestFileReadFloat;
  TestFileReadEnum;
  TestFileSeekEoln;
  TestFileSeekEof;
  TestRawFileBlockIO;
  TestTextFileAppend;
  TestTypedFileIO;
  TestFileErase;
  TestFileRename;

  WriteLn;
  WriteLn('************************');
  WriteLn('Passed assertions: ', AssertPassed);
  WriteLn('Failed assertions: ', AssertFailed);
  WriteLn('************************');
  WriteLn;
end.
