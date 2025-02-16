(* -------------------------------------------------------------------------- *)
(* --- CP/M 2.2 low-level file support -------------------------------------- *)
(* -------------------------------------------------------------------------- *)

type
  FileControlBlock = record             (* CP/M file control block            *)
    DR: Byte;                           (* Drive number                       *)
    FN: array[0..7] of Char;            (* File name, 8 chars, space-padded   *)
    TN: array[0..2] of Char;            (* Extension, 3 chars, space-padded   *)
    EX, S1, S2, RC: Byte;               (* CP/M internal stuff                *)
    AL: array[0..15] of Byte;           (* CP/M internal stuff                *)
    CR: Byte;                           (* CP/M internal stuff                *)
    RL: Integer; RH: Byte;              (* 24 bit random record number        *)
  end;

  TextRec = record                      (* Internal text file representation  *)
    FCB: FileControlBlock;              (* FCB, *must* start at offset 0      *)
    Readable: Boolean;                  (* File is open for reading           *)
    Writable: Boolean;                  (* File is open for writing           *)
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
      True:  (Count, Size: Integer;);       (* Typed file header              *)
  end;

(* --- Untyped file routines, use FileControlBlock as representation -------- *)

procedure BlockAssign(var F: FileControlBlock; S: String);
var
  I, L, P, Q: Integer;
begin
  with F do
  begin
    L := Length(S);

    if (L > 1) and (S[2] = ':') then
    begin
      DR := Ord(UpCase(S[1])) - 64;
      Delete(S, 1, 2);
      Dec(L, 2);
    end
    else DR := 0;

    P := Pos('.', S);
    if P = 0 then P := L + 1;

    Q := P - 1;
    if Q > 8 then Q := 8;

    for I := 1 to Q do FN[I - 1] := UpCase(S[I]);
    for I := Q + 1 to 8 do FN[I - 1] := ' ';

    Q := L - P;
    if Q > 3 then Q := 3;

    for I := 1 to Q do TN[I - 1] := UpCase(S[P + I]);
    for I := Q + 1 to 3 do TN[I - 1] := ' ';
(*
    Write(DR, ':');
    for I := 0 to 7 do Write(FN[I]);
    Write('.');
    for I := 0 to 2 do Write(TN[I]);
    WriteLn;
*)
  end;
end;

procedure BlockErase(var F: FileControlBlock);
var
  A: Byte;
begin
  A := Bdos(19, Addr(F));
end;

procedure BlockRename(var F: FileControlBlock; S: String);
var
  G: FileControlBlock;
  A: Byte;
begin
  BlockAssign(G, S);
  Move(G, F.AL, 12);
  A := Bdos(23, Addr(F));
end;

procedure BlockReset(var F: FileControlBlock);
var
  A: Byte;
begin
  with F do
  begin
    EX := 0;
    S1 := 0;
    S2 := 0;
    RC := 0;
    CR := 0;

    RL := 0;
    RH := 0;
  end;

  A := Bdos(15, Addr(F));
end;

procedure BlockRewrite(var F: FileControlBlock);
var
  A: Byte;
begin
  with F do
  begin
    EX := 0;
    S1 := 0;
    S2 := 0;
    RC := 0;
    CR := 0;

    RL := 0;
    RH := 0;
  end;

  A := Bdos(19, Addr(F));
  A := Bdos(22, Addr(F));
end;

procedure BlockClose(var F: FileControlBlock);
var
  A: Byte;
begin
  A := Bdos(16, Addr(F));
end;

function BlockFilePos(var F: FileControlBlock): Integer;
begin
  BlockFilePos := F.RL;
end;

function BlockFileSize(var F: FileControlBlock): Integer;
var
  I: Integer;
  A: Byte;
begin
  with F do
  begin
    I := RL;
    A := Bdos(35, Addr(F));
    BlockFileSize := RL;
    RL := I;
  end;
end;

function BlockEof(var F: FileControlBlock): Boolean;
begin
  BlockEof := BlockFilePos(F) = BlockFileSize(F);
end;

procedure BlockSeek(var F: FileControlBlock; I: Integer);
begin
  F.RL := I;
end;

procedure BlockRead(var F: FileControlBlock; var Buffer; Count: Integer; var Actual: Integer);
var
  A: Byte;
  DMA: Integer;
begin
  DMA := Addr(Buffer);
  Actual := 0;

  while Count > 0 do
  begin
    A := Bdos(26, DMA);
    A := Bdos(33, Addr(F));

    Inc(F.RL);
    Inc(DMA, 128);
    Inc(Actual);
    Dec(Count);
  end;
end;

procedure BlockWrite(var F: FileControlBlock; var Buffer; Count: Integer; var Actual: Integer);
var
  A: Byte;
  DMA: Integer;
begin
  DMA := Addr(Buffer);
  Actual := 0;

  while Count > 0 do
  begin
    A := Bdos(26, DMA);
    A := Bdos(34, Addr(F));

    Inc(F.RL);
    Inc(DMA, 128);
    Inc(Actual);
    Dec(Count);
  end;
end;

(* --- Text file routines, use TextRec as representation -------------------- *)

procedure TextReset(var T: TextRec);
var
  E: Integer;
begin
  with T do
  begin
    BlockReset(FCB);
    BlockRead(FCB, DMA, 1, E);

    Readable := True;
    Writable := False;

    Offset := 0;
  end;
end;

procedure TextRewrite(var T: TextRec);
begin
  with T do
  begin
    BlockRewrite(FCB);

    Readable := False;
    Writable := True;

    Offset := 0;
  end;
end;

procedure TextSeekEof(var T: TextRec);
var
  E: Integer;
begin
  with T do
  begin
    BlockSeek(FCB, BlockFileSize(FCB) - 1);
    BlockRead(FCB, DMA, 1, E);
    
    for Offset := 0 to 127 do
      if DMA[Offset] = #26 then Exit;
  end;
end;

procedure TextAppend(var T: TextRec);
var
  A: Byte;
begin
  with T do
  begin
    TextReset(T);
    TextSeekEof(T);

    Dec(FCB.RL);

    Readable := False;
    Writable := True;
  end;
end;

procedure TextReadChar(var T: TextRec; var C: Char);
var
  E: Integer;
begin
  with T do
  begin
    C := DMA[Offset];
    if C <> #26 then
    begin
      Inc(Offset);
      if Offset = 128 then
      begin
        BlockRead(FCB, DMA, 1, E);
        Offset := 0;
      end;
    end;
  end;
end;

procedure TextSeekEol(var T: TextRec);
var
  C: Char;
begin
  with T do
  begin
    while DMA[Offset] <> #13 do
      TextReadChar(T, C);
  end;
end;

procedure TextReadLine(var T: TextRec; var S: String);
var
  C: Char;
begin
  S := '';

  while Length(S) < 255 do
  begin
    TextReadChar(T, C);

    if C = #10 then Break;
    if C = #26 then Break;

    if C >= ' ' then S := S + C;
  end;
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
      BlockWrite(FCB, DMA, 1, E);
      Offset := 0;
    end;
  end;
end;

procedure TextClose(var T: TextRec);
var
  E: Integer;
begin
  with T do
  begin
    if Writable then
    begin
      TextWriteChar(T, #26);

      if Offset <> 0 then
        BlockWrite(FCB, DMA, 1, E);
    end;

    BlockClose(FCB);

    Readable := False;
    Writable := False;
  end;
end;

procedure TextWriteLine(var T: TextRec; S: String);
var
  I: Byte;
begin
  for I := 1 to Length(S) do
    TextWriteChar(T, S[I]);

  TextWriteChar(T, #13);
  TextWriteChar(T, #10);
end;

function TextEof(var T: TextRec): Boolean;
begin
  with T do
    TextEof := DMA[Offset] = #26;
end;        

(* --- Typed file routines, use FileRec as representation ------------------- *)

procedure FileAssign(var F: FileRec; Name: String; Size: Integer);
begin
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
  with F do
  begin
    BlockReset(FCB);
    BlockRead(FCB, DMA, 1, E);

    if CompSize <> Size then WriteLn('Invalid file type'); (* Halt *)
    CompCount := Count;
    CompIndex := 0;

    Offset := 4;
    Modified := False;
  end;
end;

procedure FileRewrite(var F: FileRec);
begin
  with F do
  begin
    BlockRewrite(FCB);

    Count := 0;
    Size := CompSize;

    Offset := 4;
    Modified := True;
  end;
end;

function FileSize(var F: FileRec): Integer;
begin
  FileSize := F.CompCount;
end;

function FilePos(var F: FileRec): Integer;
begin
  FilePos := F.CompIndex;
end;

function FileEof(var F: FileRec): Boolean;
begin
  with F do
    FileEof := CompIndex = CompCount;
end;

procedure FileSeek(var F: FileRec; I: Integer);
var
  P, E: Integer;
begin
  with F do
  begin
    P := 4 + I * CompSize;    (* Should we use Real here?    *)
    BlockSeek(FCB, P / 128);  (* Why does div not work here? *)
    if I < CompCount then
      BlockRead(FCB, DMA, 1, E);
    Offset := P mod 128;
  end;
end;

procedure FileRead(var F: FileRec; var Comp);
var
  Address, Need, Avail, Bytes, E: Integer;
  Mem: array[0..65535] of Byte absolute 0;
begin
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

      Move(DMA[Offset], Mem[Address], Bytes); (* Hmm... feels like a hack. *)
      Inc(Address, Bytes);
      Inc(Offset, Bytes);
      Dec(Need, Bytes);

      if Offset = 128 then
      begin
        if Modified then
        begin
          BlockSeek(FCB, BlockFilePos(FCB) - 1);
          BlockWrite(FCB, DMA, 1, E);
          Modified := False;
          Offset := 0;
        end;
      end;
    end;
  end;
end;

procedure FileWrite(var F: FileRec; var Comp);
var
  Address, Need, Avail, Bytes, E: Integer;
  Mem: array[0..65535] of Byte absolute 0;
begin
  with F do
  begin
    Need := CompSize;

    while Need <> 0 do
    begin
      Avail := 128 - Offset;
      if Avail >= Need then
        Bytes := Need
      else
        Bytes := Avail;

      Move(Mem[Address], DMA[Offset], Bytes); (* Hmm... feels like a hack. *)
      Inc(Address, Bytes);
      Inc(Offset, Bytes);
      Dec(Need, Bytes);
      Modified := True;

      if Offset = 128 then
      begin
        if Modified then
        begin
          BlockSeek(FCB, BlockFilePos(FCB) - 1);
          BlockWrite(FCB, DMA, 1, E);
          Modified := False;
          Offset := 0;
        end;
      end;
    end;
  end;
end;

procedure FileClose(var F: FileRec);
var
  E: Integer;
begin
  with F do
  begin
    if Modified then
    begin
      BlockSeek(FCB, BlockFilePos(FCB) - 1);
      BlockWrite(FCB, DMA, 1, E);
    end;

    BlockSeek(FCB, 0);
    BlockRead(FCB, DMA, 1, E);
    Count := CompCount;
    BlockSeek(FCB, 0);
    BlockWrite(FCB, DMA, 1, E);

    BlockClose(FCB);
  end;
end;
