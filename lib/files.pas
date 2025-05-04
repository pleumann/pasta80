(* --- Text file routines, use TextRec as representation -------------------- *)

type
  TextRec = record                      (* Internal text file representation  *)
    FCB: FileControlBlock;              (* FCB, *must* start at offset 0      *)
    Readable: Boolean;                  (* File is open for reading           *)
    Writable: Boolean;                  (* File is open for writing           *)
    EndOfFile: Boolean;
    Offset: Integer;                    (* Offset within 128 byte buffer      *)
    DMA: array[0..127] of Char;         (* Internal sector buffer             *)
  end;

  FileRec = record                      (* Internal typed file representation *)
    FCB: FileControlBlock;              (* FCB, *must* start at offset 0      *)
    CompSize: Integer;                  (* Size of component type             *)
    CompCount: Integer;                 (* Number of components in file       *)
    CompIndex: Integer;                 (* Index of current component         *)
    Offset: Integer;                    (* Offset within 128 byte buffer      *)
    Modified: Boolean;                  (* Current record has been modified   *)
    case Boolean of
      False: (DMA: array[0..127] of Char;); (* Internal sector buffer         *)
      True:  (HdrCount, HdrSize: Integer;); (* Typed file header              *)
  end;

procedure TextAssign(var T: TextRec; Name: String);
begin
  BlockAssign(T.FCB, Name);
end;

procedure TextReset(var T: TextRec);
var
  E: Integer;
begin
  with T do
  begin
    BlockReset(FCB);
    BlockBlockRead(FCB, DMA, 1, E);

    if LastError <> 0 then Exit;

    Readable := True;
    Writable := False;

    Offset := 0;
    EndOfFile := E = 0;
  end;
end;

procedure TextRewrite(var T: TextRec);
begin
  with T do
  begin
    BlockRewrite(FCB);

    if LastError <> 0 then Exit;

    Readable := False;
    Writable := True;

    Offset := 0;
  end;
end;

procedure TextReadChar(var T: TextRec; var C: Char);
var
  E: Integer;
begin
  with T do
  begin
    if EndOfFile then
    begin
      C := #26;
      Exit;
    end;

    C := DMA[Offset];
    if C <> #26 then
    begin
      Inc(Offset);
      if Offset = 128 then
      begin
        BlockBlockRead(FCB, DMA, 1, E);
        if LastError <> 0 then Exit;
        Offset := 0;
        EndOfFile := E = 0;
      end;
    end;
  end;
end;

procedure TextReadStr(var T: TextRec; var S: String);
var
  C: Char;
begin
  S := '';

  while Length(S) < 255 do
  begin
    TextReadChar(T, C);
    if LastError <> 0 then Exit;

    if C = #10 then Break;
    if C = #13 then
    begin
      if T.DMA[T.Offset] = #10 then TextReadChar(T, C);
      Break;
    end;
    if C = #26 then Break;

    if C >= ' ' then S := S + C;
  end;
end;

procedure TextReadWord(var T: TextRec; var S: String);
var
  C: Char;
begin
  S := '';

  while Length(S) < 255 do
  begin
    TextReadChar(T, C);
    if LastError <> 0 then Exit;

    if C > ' ' then S := S + C else Break;
  end;
end;

procedure TextReadInt(var T: TextRec; var I: Integer);
var
  S: String;
  E: Integer;
begin
  TextReadWord(T, S);
  if LastError <> 0 then Exit;
  Val(S, I, E);
end;

procedure TextReadFloat(var T: TextRec; var R: Real);
var
  S: String;
  E: Integer;
begin
  TextReadWord(T, S);
  if LastError <> 0 then Exit;
  Val(S, R, E);
end;

procedure TextWriteChar(var T: TextRec; C: Char);
var
  E: Integer;
begin
  with T do
  begin
    DMA[Offset] := C;
    Inc(Offset);
    if Offset = 128 then
    begin
      BlockBlockWrite(FCB, DMA, 1, E);
      if LastError <> 0 then Exit;
      Offset := 0;
    end;
  end;
end;

procedure TextFlush(var T: TextRec);
var
  E: Integer;
begin
  with T do
  begin
    (* FIXME !!! *)
    TextWriteChar(T, #26);

    if Offset <> 0 then
      BlockBlockWrite(FCB, DMA, 1, E);

    if LastError <> 0 then Exit;

    Readable := False;
    Writable := False;
  end;
end;

procedure TextClose(var T: TextRec);
var
  E: Integer;
begin
  with T do
  begin
    if Writable then TextFlush(T);

    BlockClose(FCB);

    if LastError <> 0 then Exit;

    Readable := False;
    Writable := False;
  end;
end;

procedure TextWriteStr(var T: TextRec; S: String);
var
  I: Byte;
begin
  for I := 1 to Length(S) do
  begin
    TextWriteChar(T, S[I]);
    if LastError <> 0 then Exit;
  end;
end;

procedure TextWriteEoln(var T: TextRec);
begin
  TextWriteStr(T, LineBreak);
end;

function TextEoln(var T: TextRec): Boolean;
begin
  with T do
    TextEoln := DMA[Offset] = #13;
end;

function TextEof(var T: TextRec): Boolean;
begin
  with T do
    TextEof := EndOfFile or (DMA[Offset] = #26);
end;

procedure TextSeekEof(var T: TextRec);
var
  E: Integer;
  C: Char;
begin
  with T do
  begin
    BlockSeek(FCB, BlockFileSize(FCB) - 1);
    BlockBlockRead(FCB, DMA, 1, E);

    if LastError <> 0 then Exit;

    while not TextEof(T) do
      TextReadChar(T, C);
  end;
end;

procedure TextSeekEoln(var T: TextRec);
var
  C: Char;
begin
  with T do
  begin
    while DMA[Offset] <> #13 do
    begin
      TextReadChar(T, C);
      if LastError <> 0 then Exit;
    end;
  end;
end;

procedure TextAppend(var T: TextRec);
begin
  with T do
  begin
    TextReset(T);
    TextSeekEof(T);

    if LastError <> 0 then Exit;

    BlockSeek(FCB, FCB.RL - 1);

    Readable := False;
    Writable := True;
  end;
end;

(* --- Typed file routines, use FileRec as representation ------------------- *)

procedure FileAssign(var F: FileRec; Name: String; Size: Integer);
begin
  if LastError <> 0 then Exit;

  with F do
  begin
    BlockAssign(FCB, Name);
    CompSize := Size;
  end;
end;

procedure FileReset(var F: FileRec);
var
  E: Integer;
begin
  if LastError <> 0 then Exit;

  with F do
  begin
    BlockReset(FCB);
    BlockBlockRead(FCB, DMA, 1, E);

    if LastError <> 0 then Exit;

    if CompSize <> HdrSize then
    begin
      WriteLn('Invalid file type');
      Halt;
    end;

    CompCount := HdrCount;
    CompIndex := 0;

    Offset := 4;
    Modified := False;

    (*WriteLn('Opened existing file, size=', CompSize, ' count=', CompCount);
    ReadLn;*)
  end;
end;

procedure FileRewrite(var F: FileRec);
var
  E: Integer;
begin
  if LastError <> 0 then Exit;

  with F do
  begin
    BlockRewrite(FCB);

    if LastError <> 0 then Exit;

    HdrSize := CompSize;
    HdrCount := 0;

    CompCount := 0;
    CompIndex := 0;

    Offset := 4;
    Modified := True;

    BlockBlockWrite(FCB, DMA, 1, E); (* TODO Delay this until Flush/Close? *)

    if LastError <> 0 then Exit;

    (*WriteLn('Created new file, size=', CompSize, ' count=', CompCount);*)
  end;
end;

function FileFileSize(var F: FileRec): Integer;
begin
  if LastError <> 0 then Exit;

  FileFileSize := F.CompCount;
end;

function FileFilePos(var F: FileRec): Integer;
begin
  if LastError <> 0 then Exit;

  FileFilePos := F.CompIndex;
end;

function FileEof(var F: FileRec): Boolean;
begin
  if LastError <> 0 then Exit;

  with F do
    FileEof := CompIndex = CompCount;
end;

procedure FileFlush(var F: FileRec);
var
  E: Integer;
begin
  if LastError <> 0 then Exit;

  with F do
  begin
    if Modified then
    begin
      BlockSeek(FCB, FCB.RL - 1);
      BlockBlockWrite(FCB, DMA, 1, E);
      if LastError <> 0 then Exit;
      Modified := False;
    end;
  end;
end;

procedure FileSeek(var F: FileRec; I: Integer);
var
  P, S: Real;
  E: Integer;
begin
  if LastError <> 0 then Exit;

  with F do
  begin
    FileFlush(F);

    if I > CompCount then
    begin
      WriteLn('*** ', I, ' > ', CompCount);
      Halt;
    end;

    if LastError <> 0 then Exit;

    P := 4.0 + I * 1.0 * CompSize;    (* Should we use Real here?    *)
    S := Int(P / 128);
    (* WriteLn('Seeking to index ', I, ' offset ', P, ' sector ', S);*)
    BlockSeek(FCB, Trunc(S));  (* Why does div not work here? *)
    if I < CompCount then
      BlockBlockRead(FCB, DMA, 1, E)
    else
      BlockSeek(FCB, FCB.RL + 1);

    if LastError <> 0 then Exit;

    Offset := Trunc(P - 128 * S); (* Abs(P mod 128); *)

    CompIndex := I;
  end;
end;

procedure FileRead(var F: FileRec; var Comp);
var
  Address, Need, Avail, Bytes, E: Integer;
  (*Mem: array[0..65535] of Byte absolute 0;*)
  P: ^Byte absolute Address;
begin
  if LastError <> 0 then Exit;

  (* WriteLn('Read entry #', FileFilePos(F)); *)

  with F do
  begin
    Address := Addr(Comp);
    Need := CompSize;

    while Need <> 0 do
    begin
      Avail := 128 - Offset;
      if Avail >= Need then
        Bytes := Need
      else
        Bytes := Avail;

      Move(DMA[Offset], P^, Bytes); (* Hmm... feels like a hack. *)
      Inc(Address, Bytes);
      Inc(Offset, Bytes);
      Dec(Need, Bytes);

      if Offset = 128 then
      begin
        FileFlush(F);
        if FileEof(F) then
          BlockSeek(FCB, FCB.RL + 1)
        else
          BlockBlockRead(FCB, DMA, 1, E);
        if LastError <> 0 then Exit;
        Offset := 0;
      end;
    end;

    Inc(CompIndex);
  end;
end;

procedure FileWrite(var F: FileRec; var Comp);
var
  Address, Need, Avail, Bytes, E: Integer;
  (*Mem: array[0..65535] of Byte absolute 0;*)
  P: ^Byte absolute Address;
begin
  if LastError <> 0 then Exit;

  (* WriteLn('Wrote entry #', FileFilePos(F)); *)

  with F do
  begin
    Address := Addr(Comp);
    Need := CompSize;

    while Need <> 0 do
    begin
      Avail := 128 - Offset;
      if Avail >= Need then
        Bytes := Need
      else
        Bytes := Avail;

      Move(P^, DMA[Offset], Bytes); (* Hmm... feels like a hack. *)
      Inc(Address, Bytes);
      Inc(Offset, Bytes);
      Dec(Need, Bytes);
      Modified := True;

      if Offset = 128 then
      begin
        FileFlush(F);
        if LastError <> 0 then Exit;

        if FileEof(F) then
          BlockSeek(FCB, FCB.RL + 1)
        else
          BlockBlockRead(FCB, DMA, 1, E);

        Offset := 0;
      end;
    end;

    if CompIndex = CompCount then Inc(CompCount);
    Inc(CompIndex);
  end;
end;

procedure FileClose(var F: FileRec);
var
  E: Integer;
begin
  if LastError <> 0 then Exit;

  with F do
  begin
    FileFlush(F);

    BlockSeek(FCB, 0);
    BlockBlockRead(FCB, DMA, 1, E);

    if LastError <> 0 then Exit;

    HdrCount := CompCount;

    (*WriteLn('File closing: ', CompCount, '*', CompCount, ' --- ', HdrCount, '*', HdrSize);
    ReadLn; *)

    BlockSeek(FCB, 0);
    BlockBlockWrite(FCB, DMA, 1, E);
    BlockClose(FCB);
  end;
  (* WriteLn('Closed file'); *)
end;