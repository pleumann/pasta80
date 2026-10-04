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

  { Both are still on disk at this point -- OLD.TMP because the rename just
    above was supposed to fail, NEW.TMP because its existing is what made it
    fail. Everything else in this file erases what it created; these two were
    the exception, and the test left them behind in whatever directory it
    happened to run in. }
  Assign(DummyFile, 'OLD.TMP');
  Erase(DummyFile);
  Assert(IOResult = 0);

  Assign(DummyFile, 'NEW.TMP');
  Erase(DummyFile);
  Assert(IOResult = 0);
  {$i+}

  Assert(not FileExists('OLD.TMP'));
  Assert(not FileExists('NEW.TMP'));
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

  { For a typed file the size is the number of *components*, not the number
    of 128-byte records the thing occupies on disk. Nothing on CP/M knows
    that count, so the RTL keeps it itself: CompCount in the FileRec, bumped
    by FileWrite and written into the file header on Close.

    ComputerRec is 16 bytes, so all five below plus the 4-byte header come to
    84 bytes -- a single CP/M record. A size measured the way the OS measures
    would say 1 here; FileSize has to say 5. That is the whole point of the
    counter, and it is only observable while the file is still open: the
    FileSize further down is preceded by a Close/Reset and reads the count
    back from the header, which looks right even if the bookkeeping is not. }
  Assert(FileSize(BinFile) = 0);

  { Write 5 computer records }
  with ComputerRecVar do
  begin
    Name := 'Apple II';
    Year := 1977;
    Cool := True;
  end;
  Write(BinFile, ComputerRecVar);
  Assert(FileSize(BinFile) = 1);

  with ComputerRecVar do
  begin
    Name := 'IBM PC';
    Year := 1981;
    Cool := False;
  end;
  Write(BinFile, ComputerRecVar);
  Assert(FileSize(BinFile) = 2);

  with ComputerRecVar do
  begin
    Name := 'ZX Spectrum';
    Year := 1982;
    Cool := True;
  end;
  Write(BinFile, ComputerRecVar);
  Assert(FileSize(BinFile) = 3);

  with ComputerRecVar do
  begin
    Name := 'Commodore 64';
    Year := 1982;
    Cool := False;
  end;
  Write(BinFile, ComputerRecVar);
  Assert(FileSize(BinFile) = 4);

  with ComputerRecVar do
  begin
    Name := 'Archimedes';
    Year := 1987;
    Cool := True;
  end;
  Write(BinFile, ComputerRecVar);
  Assert(FileSize(BinFile) = 5);

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

  { The counter only grows at the end -- FileWrite bumps it on
    "if CompIndex = CompCount" -- so overwriting a component in the middle
    has to leave the size alone. Note this file came from Reset, so the count
    being tested was loaded from the header rather than counted up from zero
    the way it was above. }
  Seek(BinFile, 2);
  Write(BinFile, ComputerRecVar);
  Assert(FileSize(BinFile) = 5);

  { Writing past the end does grow it, still with nothing closed in between.
    No FilePos check here on purpose: it is the only use of FilePos on a
    typed file in this program, and pulling FileFilePos in costs 66 resident
    bytes -- a fifth of the heap that is left on the Next -- to say something
    FileSize has already said. }
  Seek(BinFile, FileSize(BinFile));
  Write(BinFile, ComputerRecVar);
  Assert(FileSize(BinFile) = 6);

  { ...and Close has to carry that into the header }
  Close(BinFile);
  Reset(BinFile);
  Assert(FileSize(BinFile) = 6);

  Close(BinFile);
  Erase(BinFile);
end;

{ --- Files beyond 32K and 64K (issue #165) --- }

(**
 * Byte offsets inside a file used to be 16 bit on the Next: BlockFileSize
 * went negative from 32K on, and BlockSeek dropped the upper half of the
 * offset, so a seek past 64K landed at the start of the file again. Every
 * file kind sits on top of those two, so each one gets a file that is
 * larger than 64K here, and each one is checked on both sides of 32K and
 * past 64K. Loop results are collected in a Boolean so that the number of
 * assertions stays the same no matter how many records there are.
 *)
overlay procedure TestLargeFiles;
const
  Blocks = 600;                         { 76800 bytes }
  Comps = 4500;                         { 4 + 4500 * 16 = 72004 bytes }
  Lines = 520;                          { 520 * 132 = 68640 bytes and up }
var
  Idx, Tag: Integer;
  Ok: Boolean;
begin
  WriteLn('--- TestLargeFiles ---');

  { Untyped: every block carries its own number in the first two bytes }
  Assign(RawFile, 'BIG.TMP');
  Rewrite(RawFile);
  FillChar(Buffer, 128, '.');
  for Idx := 0 to Blocks - 1 do
  begin
    Buffer[0] := Chr(Lo(Idx));
    Buffer[1] := Chr(Hi(Idx));
    BlockWrite(RawFile, Buffer, 1, Actual);
  end;
  Assert(FileSize(RawFile) = Blocks);
  Close(RawFile);

  Reset(RawFile);
  Assert(FileSize(RawFile) = Blocks);  { from the OS this time }

  Ok := True;
  for Idx := 0 to 5 do
  begin
    Tag := Idx * 110 + 7;               { 7, 117, ..., 557 }
    Seek(RawFile, Tag);
    BlockRead(RawFile, Buffer, 1, Actual);
    Ok := Ok and (Actual = 1) and (Ord(Buffer[0]) + 256 * Ord(Buffer[1]) = Tag)
      and (FilePos(RawFile) = Tag + 1);
  end;
  Assert(Ok);

  { Overwrite a block past 64K, then check its neighbors are untouched }
  Seek(RawFile, 520);
  Buffer[0] := 'X';
  Buffer[1] := 'Y';
  BlockWrite(RawFile, Buffer, 1, Actual);

  Seek(RawFile, 519);
  BlockRead(RawFile, Buffer, 2, Actual);
  Assert(Actual = 2);
  Assert((Ord(Buffer[0]) + 256 * Ord(Buffer[1]) = 519)
    and (Buffer[128] = 'X') and (Buffer[129] = 'Y'));

  Seek(RawFile, Blocks - 1);
  BlockRead(RawFile, Buffer, 1, Actual);
  Assert(Eof(RawFile));
  Assert(FileSize(RawFile) = Blocks);  { overwriting did not grow it }

  { The first block must have survived all of that }
  Seek(RawFile, 0);
  BlockRead(RawFile, Buffer, 1, Actual);
  Assert((Buffer[0] = #0) and (Buffer[1] = #0));

  Close(RawFile);
  Erase(RawFile);

  { Typed: 16 byte components straddle block boundaries every now and then }
  Assign(BinFile, 'BIG.TMP');
  Rewrite(BinFile);
  ComputerRecVar.Name := 'Big';
  ComputerRecVar.Cool := True;
  for Idx := 0 to Comps - 1 do
  begin
    ComputerRecVar.Year := Idx;
    Write(BinFile, ComputerRecVar);
  end;
  Close(BinFile);

  Reset(BinFile);
  Assert(FileSize(BinFile) = Comps);

  Ok := True;
  for Idx := 0 to 5 do
  begin
    Tag := Idx * 850 + 3;               { 3, 853, ..., 4253 }
    Seek(BinFile, Tag);
    Read(BinFile, ComputerRecVar);
    Ok := Ok and (ComputerRecVar.Year = Tag);
  end;
  Assert(Ok);

  { Overwrite components past 64K; FileFlush has to find its block again }
  ComputerRecVar.Year := -1;
  Seek(BinFile, 4200);
  Write(BinFile, ComputerRecVar);
  Seek(BinFile, Comps - 1);
  Write(BinFile, ComputerRecVar);
  Close(BinFile);

  Reset(BinFile);
  Assert(FileSize(BinFile) = Comps);
  Seek(BinFile, 4199);
  Read(BinFile, ComputerRecVar);
  Tag := ComputerRecVar.Year;
  Read(BinFile, ComputerRecVar);
  Assert((Tag = 4199) and (ComputerRecVar.Year = -1));
  Read(BinFile, ComputerRecVar);
  Assert(ComputerRecVar.Year = 4201);
  Seek(BinFile, Comps - 1);
  Read(BinFile, ComputerRecVar);
  Assert(ComputerRecVar.Year = -1);
  Assert(Eof(BinFile));
  Close(BinFile);
  Erase(BinFile);

  { Text: Append seeks to the last block via BlockFileSize }
  FillChar(S, 129, '-');
  S[0] := #128;
  Assign(F, 'BIG.TMP');
  Rewrite(F);
  for Idx := 1 to Lines do
    WriteLn(F, S);
  Close(F);

  Append(F);
  WriteLn(F, 'The End');
  Close(F);

  Reset(F);
  Idx := 0;
  Ok := True;
  while not Eof(F) do
  begin
    ReadLn(F, S);
    Inc(Idx);
    if Idx <= Lines then Ok := Ok and (Length(S) = 128);
  end;
  Close(F);
  Assert(Ok);
  Assert(Idx = Lines + 1);
  Assert(S = 'The End');

  Erase(F);
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

  TestLargeFiles;

  TestIOResult;

  WriteLn;
  WriteLn('************************');
  WriteLn('Passed assertions: ', AssertPassed);
  WriteLn('Failed assertions: ', AssertFailed);
  WriteLn('************************');
  WriteLn;

  {$ifdef SYS_AGON}
    inline($3e / $00 / $d3 / $00);
  {$endif}
  {$ifdef SYS_ZXNEXT}
    QuitEmulator;
  {$endif}
  {$ifdef SYS_ZX128}
    inline($3e / $00 / $d3 / $1f);
  {$endif}

end.
