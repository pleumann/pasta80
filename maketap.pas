(**
 * Super-simple .tap file creator. No error checking or other convenience
 * features.
 *)
program MakeTap;

var
  (**
   * The file we are writing to.
   *)
  Target: File;

  (**
   * Our outgoing buffer. Files cannot be larger than 64K.
   *)
  Block: array[0..65535] of Byte;

  (**
   * The next buffer location to write to.
   *)
  Position: Integer;

  (**
   * The checksum (xor of all bytes). Needs to be reset when a block is
   * begin written to disk.
   *)
  CheckSum: Byte;

(**
 * Resets the buffer position and checksum, so that a new block can be
 * started.
 *)
procedure ClearTarget;
begin
  Position := 0;
  CheckSum := 0;
end;

(**
 * Writes a single byte.
 *)
procedure WriteByte(B: Byte);
begin
  Block[Position] := B;
  Inc(Position);
  CheckSum := CheckSum xor B;
end;

(**
 * Writes a 16-bit integer.
 *)
procedure WriteInt(I: Integer);
begin
  WriteByte(Lo(I));
  WriteByte(Hi(I));
end;

(**
 * Writes a character.
 *)
procedure WriteChar(C: Char);
begin
  WriteByte(Ord(C));
end;

(**
 * Writes a whole array bytewise.
 *)
procedure WriteArray(Bytes: array of Byte; Count: Integer);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    WriteByte(Bytes[I]);
end;

(**
 * Adds a header block to the output file. Headers are always 17 bytes and
 * contain metadata such as name, type and length.
 *)
procedure AddHeader(Name, Data: string);
var
  Items: array[0..3] of Integer;
  I, P, Q: Integer;
begin
  Name := Copy(Name, 1, 10);
  if Length(Name) < 10 then
    Name := Name + Copy('          ', Length(Name) + 1, 255);

  I := 0;
  P := 1;
  while P <= Length(Data) do
  begin
    Q := Pos(',', Data, P + 1);
    if Q = 0 then Q := 255;
    Val(Copy(Data, P, Q - P), Items[I]);
    Inc(I);
    P := Q + 1;
  end;

  ClearTarget;

  WriteInt(19);

  CheckSum := 0;

  WriteByte(0);
  WriteByte(Lo(Items[0]));

  for I := 1 to Length(Name) do
    WriteChar(Name[I]);

  for I := 1 to 3 do
    WriteInt(Items[I]);

  WriteByte(CheckSum);

  BlockWrite(Target, Block, Position);
end;

(**
 * Adds a data block with the contents of a given file.
 *)
procedure AddData(const Path: string);
var
  Source: File;
  Buffer: array[0..511] of Byte;
  Count: Integer;
begin
  Assign(Source, Path);
  Reset(Source, 1);

  ClearTarget;

  WriteInt(FileSize(Source) + 2);
  CheckSum := 0;
  WriteByte(255);

  repeat
    BlockRead(Source, Buffer, SizeOf(Buffer), Count);
    WriteArray(Buffer, Count);
  until Count < SizeOf(Buffer);

  Close(Source);

  WriteByte(CheckSum);

  BlockWrite(Target, Block, Position);
end;

(**
 * Displays usage help.
 *)
procedure Help;
begin
  WriteLn('Usage: maketap { -h <name> <type>,<length>,<param1>,<param1> | -d <name> }');
  WriteLn;

  Halt;
end;

(**
 * Handles command-line arguments.
 *)
procedure Parameters;
var
  I: Integer;
begin
  I := 2;
  while I <= ParamCount do
  begin
    if ParamStr(I) = '-h' then
    begin
      AddHeader(ParamStr(I + 1), ParamStr(I + 2));
      Inc(I, 3);
    end
    else if ParamStr(I) = '-d' then
    begin
      AddData(ParamStr(I + 1));
      Inc(I, 2);
    end
    else
    begin
      WriteLn('Invalid argument: ', ParamStr(I));
      Halt(1);
    end;
  end;
end;

begin
  if ParamCount = 0 then Help;

  Assign(Target, ParamStr(1));
  Rewrite(Target, 1);
  Parameters;
  Close(Target);
end.