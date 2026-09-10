program Files;

{$a+}

type
  Color = (Red, Green, Blue, Yellow);

  ComputerRec = record
    Name: String[12];
    Year: Integer;
    Cool: Boolean;
  end;

const
  Monty: array[1..6] of String = (
    'Why did Monty die so fast?',
    'Aren''t three lives enough to last',
    'The hazards that confront a mole',
    'In his search for precious coal?',
    'Don''t let Monty die in vain,',
    'Press a key and try again!'
  );

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

{ --- Helpers --- }

function FileExists(Filename: String): Boolean;
var
  TestFile: File;
  Result: Boolean;
  Dummy: Integer;
begin
  {$i-}
  Assign(TestFile, Filename);
  Reset(TestFile);
  Result := IOResult = 0;
  {$i+}
  if Result then
  begin
    {$i-}
    Close(TestFile);
    Dummy := IOResult; { Discard close error }
    {$i+}
  end;
  FileExists := Result;
end;

{ --- General tests --- }

overlay procedure TestFileErase;
var
  TestFile: Text;
begin
  WriteLn('--- TestFileErase ---');

  Assign(TestFile, 'ERS.TMP');

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

  { Non-existing file }
  {$i-}
  Erase(TestFile);
  Assert(IOResult <> 0);
  {$i+}
end;

overlay procedure TestFileRename;
var
  DummyFile: File;
begin
  WriteLn('--- TestFileRename ---');

  {$i-}
  Assign(DummyFile, 'NEW.TMP');
  Erase(DummyFile);
  I := IOREsult;

  Assign(DummyFile, 'OLD.TMP');

  { Create file }
  Rewrite(DummyFile);
  BlockWrite(DummyFile, Buffer, 1, Actual);
  Close(DummyFile);

  { Verify original file exists }
  Assert(IOResult = 0);
  Assert(FileExists('OLD.TMP'));

  { Rename file }
  Rename(DummyFile, 'NEW.TMP');

  { Verify old name no longer exists }
  Assert(IOResult = 0);
  Assert(not FileExists('OLD.TMP'));

  { Verify new name exists }
  Assert(IOResult = 0);
  Assert(FileExists('NEW.TMP'));

  { Clean up }
  Assign(DummyFile, 'NEW.TMP');
  Erase(DummyFile);
  Assert(IOResult = 0);

  { Old file does not exist }

  Assign(DummyFile, 'OLD.TMP');
  Rename(DummyFile, 'NEW.TMP');
  Assert(IOResult <> 0);

  { New file already exists }

  Assign(DummyFile, 'OLD.TMP');
  Rewrite(DummyFile);
  BlockWrite(DummyFile, Buffer, 1, Actual);
  Close(DummyFile);

  Assign(DummyFile, 'NEW.TMP');
  Rewrite(DummyFile);
  BlockWrite(DummyFile, Buffer, 1, Actual);
  Close(DummyFile);
  Assert(IOResult = 0);

  Assign(DummyFile, 'OLD.TMP');
  Rename(DummyFile, 'NEW.TMP');
  Assert(IOResult <> 0);
  {$i+}
end;

{ --- Raw files --- }

overlay procedure TestUntypedFiles;
const
  Expected: string = '0123AB6789ZZ';
var
  Idx: Integer;
  Ch: Char;
  FS: Integer;
begin
  WriteLn('--- TestUntypedFiles ---');

  Assign(RawFile, 'RAW.TMP');
  Rewrite(RawFile);

  { CP/M has no file size of its own: BDOS 35 fills in the record count when
    the file is opened and never again, so the RTL mirrors it in the FCB and
    grows it by hand on every write that reaches past the end (the
    "if F.RL > F.SL" in BlockBlockWrite, rtl/cpm.pas).

    That hand-kept count is only observable while the file is still open --
    every other FileSize below is preceded by a Close/Reset and therefore
    reads the size back from the directory, which would look right even if
    the bookkeeping were broken. So it gets checked here, before anything
    is closed. }
  Assert(FileSize(RawFile) = 0);

  { Write 10 blocks with characters '0'..'9' }
  Idx := 0;
  for Ch := '0' to '9' do
  begin
    WriteLn(Ch);
    FillChar(Buffer, 128, Ch);
    BlockWrite(RawFile, Buffer, 1, Actual);
    Assert(Actual = 1);

    Inc(Idx);
    Assert(FileSize(RawFile) = Idx);
  end;

  { Give the OS a chance to update the file size }
  Close(RawFile);
  Reset(RawFile);

  FS := FileSize(RawFile);
  Assert(FS = 10);

  Close(RawFile);

  { Reopen in update mode, seek to position 4 }
  Reset(RawFile);
  Seek(RawFile, 4);
  Assert(FilePos(RawFile) = 4);

  { Overwrite with 'A' }
  FillChar(Buffer, 128, 'A');
  BlockWrite(RawFile, Buffer, 1, Actual);
  Assert(Actual = 1);
  Assert(FilePos(RawFile) = 5);

  { Overwrite with 'B' }
  FillChar(Buffer, 128, 'B');
  BlockWrite(RawFile, Buffer, 1, Actual);
  Assert(Actual = 1);
  Assert(FilePos(RawFile) = 6);

  { The other half of the same bookkeeping: writing *inside* the file must
    not grow it. Both blocks above landed at 4 and 5, well short of the end. }
  Assert(FileSize(RawFile) = 10);

  { Seek to end and write 2 blocks of 'Z' }
  Seek(RawFile, FileSize(RawFile));
  FillChar(Buffer, 256, 'Z');
  BlockWrite(RawFile, Buffer, 2, Actual);
  Assert(Actual = 2);
  Assert(FilePos(RawFile) = 12);

  { Grown past the end this time, and still open, so this is the mirrored
    count again -- the Close/Reset below re-checks the same 12 the slow way. }
  Assert(FileSize(RawFile) = 12);

  { Give the OS a chance to update the file size }
  Close(RawFile);
  Reset(RawFile);

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
    Assert(Buffer[0] = Expected[Idx + 1]);
    Inc(Idx);
  end;
  Assert(Idx = 12);

  Close(RawFile);

  Erase(RawFile);
  Assert(not FileExists('RAW.TMP'));
end;

{ --- Text --- }

overlay procedure TestTextWithStrings;
var
  LineCount: Integer;
  CharCount: Integer;
  TempCh: Char;
begin
  WriteLn('--- TestTextWithStrings ---');

  Assign(F, 'TXT.TMP');

  { Initial Rewrite }
  Rewrite(F);
  WriteLn(F, Monty[1]);
  Close(F);

  { First Append }
  Append(F);
  WriteLn(F, Monty[2]);
  Close(F);

  { Second Append }
  Append(F);
  for I := 3 to 6 do
    WriteLn(F, Monty[I]);
  Close(F);

  { Verify line count }
  Reset(F);
  LineCount := 0;
  while not Eof(F) do
  begin
    ReadLn(F, S);
    WriteLn(S);
    Inc(LineCount);
    Assert(S = Monty[LineCount]);
  end;
  Close(F);
  WriteLn(LineCount, ' lines');
  Assert(LineCount = 6);

  WriteLn;

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

  WriteLn(CharCount, ' characters');
  Assert(CharCount = 177 + 6 * Length(LineBreak));

  Erase(F);
end;

(* Write(F, X:W) for Char and String goes through the same EmitStr1 -- and
   thus the same __strc_fmt/__strs_fmt -- that Str(X:W, S) uses. Both were
   broken by the same bug, so both are fixed by the same change, but only
   the Str side ever had coverage. This closes that gap. *)
overlay procedure TestTextWriteFormatted;
var
  T: Text;
  Line: String;
  Ch: Char;
  Txt: String;
begin
  WriteLn('--- TestTextWriteFormatted ---');

  Ch := 'X';
  Txt := 'Hi';

  Assign(T, 'FMT.TMP');
  Rewrite(T);
  Write(T, Ch:5, '|', Txt:5, '|', Ch, '|', Txt, '|');
  WriteLn(T);
  Close(T);

  Reset(T);
  ReadLn(T, Line);
  Close(T);

  WriteLn('[', Line, ']');
  Assert(Line = '    X|   Hi|X|Hi|');

  Erase(T);
end;

(* A malformed number is an I/O error like any other, so the i-minus
   directive has to be able to catch it through IOResult rather than the
   program stopping. Checked against TP 3.0 (OPEN-ITEMS-EN.md B9). The
   i-plus half cannot live in a suite because it terminates, and is covered
   by tests/errors/numfmt.pas instead. *)
overlay procedure TestTextReadIOResult;
var
  T: Text;
  I: Integer;
  R: Real;
  C: Color;
  E: Integer;
  B: Boolean;
begin
  WriteLn('--- TestTextReadIOResult ---');

  Assign(T, 'IOR.TMP');
  Rewrite(T);
  WriteLn(T, 'abc');           { not an Integer }
  WriteLn(T, '42');
  WriteLn(T, 'xyz');           { not a Real    }
  WriteLn(T, '2.5');
  WriteLn(T, 'nope');          { not a Color   }
  WriteLn(T, 'Blue');
  WriteLn(T, '');              { nothing at all }
  Close(T);

  Reset(T);

  I := -1;
  {$i-}
  Read(T, I);
  E := IOResult;
  {$i+}
  Assert(E <> 0);              { reported }
  Assert(I = -1);              { and the target left alone }
  ReadLn(T);

  I := -1;
  {$i-}
  ReadLn(T, I);
  E := IOResult;
  {$i+}
  Assert(E = 0);               { a good one reports nothing }
  Assert(I = 42);

  R := -1.0;
  {$i-}
  Read(T, R);
  E := IOResult;
  {$i+}
  Assert(E <> 0);
  B := (R > -1.1) and (R < -0.9);
  Assert(B);
  ReadLn(T);

  R := -1.0;
  {$i-}
  ReadLn(T, R);
  E := IOResult;
  {$i+}
  Assert(E = 0);
  B := (R > 2.4) and (R < 2.6);
  Assert(B);

  C := Red;
  {$i-}
  Read(T, C);
  E := IOResult;
  {$i+}
  Assert(E <> 0);
  Assert(C = Red);
  ReadLn(T);

  C := Red;
  {$i-}
  ReadLn(T, C);
  E := IOResult;
  {$i+}
  Assert(E = 0);
  Assert(C = Blue);

  (* Reading nothing at all is an error here, where TP 3.0 would quietly
     leave the variable as it was. Deliberate, and the reason a data file
     with a trailing blank line now stops a read loop -- see B9. *)
  I := -1;
  {$i-}
  ReadLn(T, I);
  E := IOResult;
  {$i+}
  Assert(E <> 0);
  Assert(I = -1);

  Close(T);
  Erase(T);
end;

overlay procedure TestTextWithIntegers;
var
  I1, I2, I3: Integer;
begin
  WriteLn('--- TestTextWithIntegers ---');

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

overlay procedure TestTextWithReals;
var
  R1, R2, R3: Real;
  B1, B2: Boolean;
begin
  WriteLn('--- TestTextWithReals ---');

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

overlay procedure TestTextWithEnums;
var
  C1, C2, C3, C4: Color;
begin
  WriteLn('--- TestTextWithEnums ---');

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

procedure TestTextSeekEoln;
var
  I1: Integer;
  B1, B2, B3: Boolean;
begin
  WriteLn('--- TestTextSeekEoln ---');

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

overlay procedure TestTextSeekEof;
var
  I1, I2: Integer;
  B1, B2, B3: Boolean;
begin
  WriteLn('--- TestTextSeekEof ---');

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

overlay procedure TestTextEoln;
var
  T: Text;
  C: Char;
begin
  WriteLn('--- TestTextEoln ---');

  Assign(T, 'TXT.TMP');
  Rewrite(T);
  WriteLn(T, 'Hello, World!');
  Close(T);

  Reset(T);
  for I := 1 to 13 do
  begin
    Read(T, C);
    Write(C, ' ');
  end;

  if Eoln(T) then WriteLn('<EOL>') else WriteLn;
  Assert(Eoln(T));

  Close(T);

  { Eoln must also be True on an empty file, even though there's
    no CR to find -- Eof already is True there, Eoln has to agree . }
  Assign(T, 'TXT.TMP');
  Rewrite(T);
  Close(T);

  Reset(T);
  Assert(Eof(T));
  Assert(Eoln(T));
  Close(T);
end;

overlay procedure TestTextEof;
var
  T: Text;
  S: String;
begin
  WriteLn('--- TestTextEof ---');

  Assign(T, 'TXT.TMP');
  Rewrite(T);
  for I := 1 to 6 do
    WriteLn(T, Monty[I]);
  Close(T);

  Reset(T);
  for I := 1 to 6 do
    ReadLn(T, S);

  Assert(Eof(T));

  Close(T);
end;

(**
 * Regression test for a bug we had in Eof/Eoln/SeekEof/SeekEoln where a 
 * random 16 bit value was put on the stack into the result slot. Functions
 * returning a Boolean value only modify the lower 8 bits of that slot, so
 * if someone cared to Ord() or otherwise cast the result there would have
 * been garbage inside.
 *)
overlay procedure TestTextBooleanResultBytes;
var
  T: Text;
  S: String;
begin
  WriteLn('--- TestTextBooleanResultBytes ---');

  Assign(T, 'BOO.TMP');
  Rewrite(T);
  WriteLn(T, 'ab');
  Close(T);

  Reset(T);

  I := -1;
  Assert(Ord(Eof(T)) = 0);
  I := -1;
  Assert(Ord(Eoln(T)) = 0);
  I := -1;
  Assert(Ord(SeekEof(T)) = 0);
  I := -1;
  Assert(Ord(SeekEoln(T)) = 0);

  ReadLn(T, S);
  Assert(S = 'ab');

  I := -1;
  Assert(Ord(Eof(T)) = 1);
  I := -1;
  Assert(Ord(SeekEof(T)) = 1);

  Close(T);
  Erase(T);
end;

(**
 * Regression test for ReadLn(T, S) writing past the declared capacity of
 * S. TextReadStr had a hardcoded limit of 255 characters and was never
 * told the target's actual size.
 *)
overlay procedure TestTextReadLnBounds;
var
  T: Text;
  Guard1: Integer;
  Target: String[10];
  Guard2: Integer;
  Guard3: array[0..9] of Byte;
  I: Integer;
  Line: String;
begin
  WriteLn('--- TestTextReadLnBounds ---');

  Assign(T, 'RLB.TMP');
  Rewrite(T);
  WriteLn(T, '0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ');
  WriteLn(T, 'second');
  Close(T);

  Guard1 := 1111;
  Guard2 := 2222;
  for I := 0 to 9 do Guard3[I] := 77;

  Reset(T);
  ReadLn(T, Target);

  Assert(Length(Target) = 10);
  Assert(Target = '0123456789');

  { Nothing beyond the string may have been touched. }
  Assert(Guard1 = 1111);
  Assert(Guard2 = 2222);
  for I := 0 to 9 do Assert(Guard3[I] = 77);

  { The rest of the long line must still have been skipped, not left
    behind for the next ReadLn to pick up. }
  ReadLn(T, Line);
  Assert(Line = 'second');
  Assert(Eof(T));

  Close(T);
  Erase(T);
end;

{ --- Typed 'file of' --- }

overlay procedure TestTypedFiles;
const
  CoolStr: array[Boolean] of String = ('Uncool', 'Cool');
var
  RecCount, RecIdx, FS, LastYear: Integer;
  LastCool: Boolean;
begin
  WriteLn('--- TestTypedFileIO ---');

  Assign(BinFile, 'BIN.TMP');
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

{ --- $i directive and IOResult --- }

overlay procedure TestIOResult;
var
  T: Text;
  I: Integer;
begin
  WriteLn('--- TestIOResult ---');

  Assign(T, 'TXT.TMP');
  {$i-}
  Erase(T);
  I := IOResult;

  Reset(T);
  Reset(T);
  Rewrite(T);
  WriteLn(T, 'You should not see this.');
  Close(T);

  Assert(IOResult <> 0);
  {$i-}

  Assert(not FileExists('TXT.TMP'));
end;

begin
  {$ifdef SYS_ZXNEXT}
  SetCpuSpeed(3);
  {$endif}
  
  WriteLn;
  WriteLn('*** PASTA/80 Test Suite ***');
  WriteLn;

  TestFileErase;
  TestFileRename;

  TestUntypedFiles;

  TestTextWithStrings;
  TestTextWriteFormatted;
  TestTextReadIOResult;
  TestTextWithIntegers;
  TestTextWithReals;
  TestTextWithEnums;
  TestTextSeekEoln;
  TestTextSeekEof;
  TestTextEoln;
  TestTextEof;
  TestTextBooleanResultBytes;
  TestTextReadLnBounds;

  TestTypedFiles;

  TestIOResult;

  WriteLn;
  WriteLn('************************');
  WriteLn('Passed assertions: ', AssertPassed);
  WriteLn('Failed assertions: ', AssertFailed);
  WriteLn('************************');
  WriteLn;
end.
