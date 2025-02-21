program FF;

{$a+}
{$i /Users/joerg/Projekte/pl0/lib/cpm22.pas}

var
  IntFile: file of Integer;
  TxtFile: Text;
  RawFile: file; (* untyped *)

  I: Integer;
  B: Boolean;

  Buffer: array[0..255] of Char;
  Actual: Integer;

begin
  WriteLn(SizeOf(FileControlBlock));
  WriteLn(SizeOf(TextRec));
  WriteLn(SizeOf(FileRec));

  Assign(IntFile, 'test.dat');
  Reset(IntFile);
  Seek(IntFile, 0);
  (* SeekEof(IntFile); *)
  (* SeekEoln(IntFile); *)
  Rewrite(IntFile);
  Flush(IntFile);
  Close(IntFile);

  I := FileSize(IntFile);
  I := FilePos(IntFile);
  B := Eof(IntFile);
  (* B := Eoln(TxtFile); *)

  Assign(TxtFile, 'test.txt');
  Reset(TxtFile);
  (* Seek(TxtFile, 0); *)
  SeekEof(TxtFile);
  SeekEoln(TxtFile);
  Rewrite(TxtFile);
  Flush(TxtFile);
  Close(TxtFile);

  (* I := FileSize(TxtFile); *)
  (* I := FilePos(TxtFile); *)
  B := Eof(TxtFile);
  B := Eoln(TxtFile);

  Assign(RawFile, 'text.raw');
  Reset(RawFile);
  Seek(RawFile, 0);
  (* SeekEof(RawFile); *)
  (* SeekEoln(RawFile); *)
  Rewrite(RawFile);
  (* Flush(RawFile); *)
  Close(RawFile);

  BlockRead(RawFile, Buffer, 1);
  BlockRead(RawFile, Buffer, 2, Actual);
  BlockWrite(RawFile, Buffer, 1);
  BlockWrite(RawFile, Buffer, 2, Actual);

  I := FileSize(RawFile);
  I := FilePos(RawFile);
  B := Eof(RawFile);
  (* B := Eoln(RawFile); *)
end.
