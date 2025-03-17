{$l cpm.asm}

{$i system.pas}

(* -------------------------------------------------------------------------- *)
(* --- VT52 terminal support ------------------------------------------------ *)
(* -------------------------------------------------------------------------- *)

const
  ScreenWidth = 80;
  ScreenHeight = 24;

procedure ConOut(C: Char); register;        external '__conout';

procedure ClrScr; register;                 external '__clrscr';
procedure GotoXY(X, Y: Integer); register;  external '__gotoxy';
procedure CursorOn; register;               external '__cursor_on';
procedure CursorOff; register;              external '__cursor_off';

procedure ClrEol; register; inline
(
  $3e / 27 /                  (* ld   a,27      *)
  $cd / ConOut /              (* call ConOut    *)
  $3e / 'K' /                 (* ld   a,'K'     *)
  $cd / ConOut /              (* call ConOut    *)
  $c9                         (* ret            *)
);

procedure ClrEos; register; inline
(
  $3e / 27 /                  (* ld   a,27      *)
  $cd / ConOut /              (* call ConOut    *)
  $3e / 'J' /                 (* ld   a,'J'     *)
  $cd / ConOut /              (* call ConOut    *)
  $c9                         (* ret            *)
);

procedure InsLine; register; inline
(
  $3e / 27 /                  (* ld   a,27      *)
  $cd / ConOut /              (* call ConOut    *)
  $3e / 'L' /                 (* ld   a,'L'     *)
  $cd / ConOut /              (* call ConOut    *)
  $c9                         (* ret            *)
);

procedure DelLine; register; inline
(
  $2e / 27 /                  (* ld   l,27      *)
  $cd / ConOut /              (* call ConOut    *)
  $3e / 'M' /                 (* ld   l,'M'     *)
  $cd / ConOut /              (* call ConOut    *)
  $c9                         (* ret            *)
);

procedure TextColor(I: Integer); register;      external '__textfg';
procedure TextBackground(I: Integer); register; external '__textbg';

procedure HighVideo; register; inline
(
  $c9
);

procedure LowVideo; register; inline
(
  $c9
);

procedure NormVideo; register; inline
(
  $c9
);

(* -------------------------------------------------------------------------- *)
(* --- Keyboard support ----------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

function KeyPressed: Boolean;
begin
  KeyPressed := BDOS(11, 0) <> 0;
end;

function ReadKey: CHar;
begin
  repeat until KeyPressed;
  ReadKey := Chr(BDOS(6, 255));
end;

procedure Delay(MS: Integer);
var
  A: Byte;
begin
  A := BDos(141, MS div 20);
end;

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
      True:  (HdrCount, HdrSize: Integer;); (* Typed file header              *)
  end;

(* --- Untyped file routines, use FileControlBlock as representation -------- *)

const
  LastError: Byte = 0;

function IOResult: Byte;
begin
  IOResult := LastError;
  LastError := 0;
end;

procedure BDosCatch(Func: Byte; Param: Integer);
var
  A: Byte;
begin
  if LastError <> 0 then Exit;
  A := BDos(Func, Param);
  (* WriteLn('BDos(', Func, ') returned A=' , A); *)
  if A = 255 then LastError := 1;
end;

procedure BDosThrow;
begin
  if LastError <> 0 then
  begin
    WriteLn('BDos error');
    Halt;
  end;
end;

procedure BlockAssign(var F: FileControlBlock; S: String);
var
  I, L, P, Q: Integer;
begin
  if LastError <> 0 then Exit;

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
  if LastError <> 0 then Exit;
  A := BDos(*Catch*)(19, Addr(F));
end;

procedure BlockRename(var F: FileControlBlock; S: String);
var
  G: FileControlBlock;
  A: Byte;
begin
  if LastError <> 0 then Exit;
  BlockAssign(G, S);
  if LastError <> 0 then Exit;
  Move(G, F.AL, 12);
  BDosCatch(23, Addr(F));
end;

procedure BlockReset(var F: FileControlBlock);
var
  A: Byte;
begin
  if LastError <> 0 then Exit;

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

  BDosCatch(15, Addr(F));
end;

procedure BlockRewrite(var F: FileControlBlock);
var
  A: Byte;
begin
  if LastError <> 0 then Exit;

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

  A := BDos(*Catch*)(19, Addr(F));
  BDosCatch(22, Addr(F));
end;

procedure BlockClose(var F: FileControlBlock);
begin
  BDosCatch(16, Addr(F));
end;

function BlockFilePos(var F: FileControlBlock): Integer;
begin
  BlockFilePos := F.RL;
end;

function BlockFileSize(var F: FileControlBlock): Integer;
var
  I: Integer;
begin
  with F do
  begin
    I := RL;
    BDosCatch(35, Addr(F));
    if LastError <> 0 then Exit;
    BlockFileSize := RL;
    RL := I;
  end;
end;

function BlockEof(var F: FileControlBlock): Boolean;
begin
  if LastError <> 0 then Exit;

  BlockEof := BlockFilePos(F) = BlockFileSize(F);
end;

procedure BlockSeek(var F: FileControlBlock; I: Integer);
begin
  if LastError <> 0 then Exit;

  F.RL := I;
end;

procedure BlockBlockRead(var F: FileControlBlock; var Buffer; Count: Integer; var Actual: Integer);
var
  DMA: Integer;
begin
  if LastError <> 0 then Exit;

  DMA := Addr(Buffer);
  Actual := 0;

  while Count > 0 do
  begin
    BDosCatch(26, DMA);
    BDosCatch(33, Addr(F));
    if LastError <> 0 then Exit;

    Inc(F.RL);
    Inc(DMA, 128);
    Inc(Actual);
    Dec(Count);
  end;
end;

procedure BlockBlockWrite(var F: FileControlBlock; var Buffer; Count: Integer; Actual: Integer);
var
  DMA: Integer;
begin
  if LastError <> 0 then Exit;

  DMA := Addr(Buffer);
  (*Actual := 0;*)

  while Count > 0 do
  begin
    BDosCatch(26, DMA);
    BDosCatch(34, Addr(F));
    if LastError <> 0 then Exit;

    Inc(F.RL);
    Inc(DMA, 128);
(*    Inc(Actual);*)
    Dec(Count);
  end;
end;

(* --- Text file routines, use TextRec as representation -------------------- *)

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

procedure TextSeekEof(var T: TextRec);
var
  E: Integer;
begin
  with T do
  begin
    BlockSeek(FCB, BlockFileSize(FCB) - 1);
    BlockBlockRead(FCB, DMA, 1, E);

    if LastError <> 0 then Exit;

    for Offset := 0 to 127 do
      if DMA[Offset] = #26 then Exit;
  end;
end;

procedure TextAppend(var T: TextRec);
begin
  with T do
  begin
    TextReset(T);
    TextSeekEof(T);

    if LastError <> 0 then Exit;

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
        BlockBlockRead(FCB, DMA, 1, E);
        if LastError <> 0 then Exit;
        Offset := 0;
      end;
    end;
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
  TextWriteChar(T, #13);
  TextWriteChar(T, #10);
end;

function TextEoln(var T: TextRec): Boolean;
begin
  with T do
    TextEoln := DMA[Offset] = #13;
end;

function TextEof(var T: TextRec): Boolean;
begin
  with T do
    TextEof := DMA[Offset] = #26;
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
      Dec(FCB.RL);
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
      Inc(FCB.RL);

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
          Inc(FCB.RL)
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
          Inc(FCB.RL)
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

var
  CmdLine: String absolute $80;

function ParamCount: Byte;
var
  C, D: Boolean;
  I, J: Byte;
begin
  C := True;
  J := 0;

  for I := 1 to Length(CmdLine) do
  begin
    D := CmdLine[I] > ' ';
    if not C and D then Inc(J);
    C := D;
  end;

  ParamCount := J;
end;

function ParamStr(I: Byte): String;
var
  C, D: Boolean;
  J, K: Byte;
begin
  C := True;
  K := 1;

  for J := 1 to Length(CmdLine) do
  begin
    D := CmdLine[J] > ' ';

    if not C and D then
      K := J
    else if C and not D then
    begin
      if I = 0 then
      begin
        Dec(J);
        Break;
      end;

      Dec(I);
    end;

    C := D;
  end;

  if I = 0 then
    ParamStr := Copy(CmdLine, K, J - K + 1)
  else
    ParamStr := '';
end;

end.
