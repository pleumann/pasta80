(* Built-ins that do not have to be defined in the compiler itself. *)

{$a+}

{$l system.z80}

(* -------------------------------------------------------------------------- *)
(* --- String support ------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

(* Built-in: procedure Val(S: String; var Scalar; var E: Integer); *)
(* Built-in: procedure Str(N: Scalar; var S: String);              *)

procedure Delete(var S: String; Start, Count: Integer);     external '__delete';
procedure Insert(S: String; var T: String; Start: Integer); external '__insert';

(* Built-in: function Concat(S: String, ...): String;              *)

function Copy(S: String; Start, Count: Integer): String;    external '__copy';
function Length(S: String): Integer;                        external '__length';
function Pos(S, T: String): Integer;                        external '__pos';

(* -------------------------------------------------------------------------- *)
(* --- Set support ---------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

(* Built-in: procedure Include(var S: Set; E: Element);     *)
(* Built-in: procedure Exclude(var S: Set; E: Element);     *)

(* -------------------------------------------------------------------------- *)
(* --- File support --------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

(* TBD *)

(* -------------------------------------------------------------------------- *)
(* --- Heap management ------------------------------------------------------ *)
(* -------------------------------------------------------------------------- *)

type
  PBlock = ^TBlock;
  TBlock = record
    Next: PBlock;
    Size: Integer;
  end;

var
  HeapPtr: PBlock absolute '__heapptr';

(* Built-in: procedure New(var P: Pointer);       *)
(* Built-in: procedure Dispose(P: Pointer);       *)

procedure FreeMem(P: Pointer; Size: Integer);     register; external '__freemem';
procedure GetMem(var P: Pointer; Size: Integer);  register; external '__getmem';

function MemAvail: Integer;
var
  P: PBlock;
  I: Integer;
begin
  P := HeapPtr;
  I := 0;
  while P <> nil do
  begin
    I := I + P^.Size;
    P := P^.Next;
  end;

  MemAvail := I;
end;

function MaxAvail: Integer;
var
  P: PBlock;
  I: Integer;
begin
  P := HeapPtr;
  I := 0;
  while P <> nil do
  begin
    if P^.Size > I then I := P^.Size;
    P := P^.Next;
  end;

  MaxAvail := I;
end;

procedure InitHeap;
var
  EofMarker: Integer absolute 'eof';
  HeapStart, HeapBytes: Integer;
begin
  HeapStart := Addr(EofMarker);
  if (HeapStart >= 0) and (HeapStart < 24576) then HeapStart := 24576;
  HeapBytes := 57343 - HeapStart;
  HeapPtr := nil;

  if HeapBytes > 0 then
    FreeMem(Ptr(HeapStart), HeapBytes);
end;

(* -------------------------------------------------------------------------- *)
(* --- Standard procedures -------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

(* Built-in: procedure Break;                   *)
(* Built-in: procedure Continue;                *)
(* Built-in: procedure Exit;                    *)

const
  Black   = 0;
  Blue    = 1;
  Red     = 2;
  Magenta = 3;
  Green   = 4;
  Cyan    = 5;
  Yellow  = 6;
  White   = 7;

procedure ClrScr; register;                 external '__clrscr';
procedure GotoXY(X, Y: Integer); register;  external '__gotoxy';
procedure CursorOn; register;               external '__cursor_on';
procedure CursorOff; register;              external '__cursor_off';

procedure ConOut(C: Char); register;        external '__conout';

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
(* --- Arithmetic functions ------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

const
  MaxInt = 32767;
  MinInt = -32768;

(* Built-in: function Abs(I: Integer): Integer  *)
(* Built-in: function Abs(R: Real): Real        *)

function ArcTan(R: Real): Real; register; external 'ATN';
function Cos(R: Real): Real; register;    external 'COS';
function Exp(R: Real): Real; register;    external 'EXP';
function Frac(R: Real): Real; register;   external 'FRAC';
function Int(R: Real): Real; register;    external 'INT';
function Ln(R: Real): Real; register;     external 'LN';
function Log(R: Real): Real; register;    external 'LOG';
function Sin(R: Real): Real; register;    external 'SIN';
function Sqr(R: Real): Real; register;    external '__fltpwr2';
function Sqrt(R: Real): Real; register;   external 'SQR';
function Tan(R: Real): Real; register;    external 'TAN';

function Pi: Real; register;              external 'ACPI';

function MaxReal: Real; register; inline
(
  $01 / $7FFF /
  $11 / $FFFF /
  $21 / $FFFF /
  $c9
);

function MinReal: Real; register; inline
(
  $01 / $FFFF /
  $11 / $FFFF /
  $21 / $FFFF /
  $c9
);

(* -------------------------------------------------------------------------- *)
(* --- Scalar functions ----------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

(* Built-in: function Pred(Ordinal): Ordinal;     *)
(* Built-in: function Succ(Ordinal): Ordinal;     *)
(* Built-in: function Odd(Ordinal): Boolean;      *)
(* Built-in: function Even(Ordinal): Boolean;     *)

(* -------------------------------------------------------------------------- *)
(* --- Transfer functions --------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

(* Built-in: function Ord(Ordinal): Integer;      *)

function Round(R: Real): Integer; register; external '__fltrnd';
function Trunc(R: Real): Integer; register; external 'FIX';

function Chr(B: Byte): Char; register; inline
(
  $c9         (* ret          *)
);

(* -------------------------------------------------------------------------- *)
(* --- Miscellaneous standard functions ------------------------------------- *)
(* -------------------------------------------------------------------------- *)

(* Built-in: function KeyPressed: Boolean;        *)
(* Built-in: function SizeOf(XYZ): Integer;       *)
(* Built-in: function Addr(XYZ): Integer;         *)
(* Built-in: function Ptr(I: Integer): Pointer;   *)

function Hi(I: Integer): Byte; register; inline
(
  $6c /       (* ld   l,h     *)
  $26 / $00 / (* ld   h,0     *)
  $c9         (* ret          *)
);

function Lo(I: Integer): Byte; register; inline
(
  $26 / $00 / (* ld   h,0     *)
  $c9         (* ret          *)
);

function Swap(I: Integer): Integer; register; inline
(
  $7c /       (* ld   a,h     *)
  $65 /       (* ld   h,l     *)
  $6f /       (* ld   l,a     *)
  $c9         (* ret          *)
);

function UpCase(C: Char): Char; register; inline
(
  $7d /       (* ld   a,l     *)
  $fe / $61 / (* cp   'a'     *)
  $d8 /       (* ret  c       *)
  $fe / $7b / (* cp   'z' + 1 *)
  $d0 /       (* ret  nc      *)
  $cb / $ad / (* res  4,l     *)
  $c9         (* ret          *)
);

function LoCase(C: Char): Char; register; inline
(
  $7d /       (* ld   a,l     *)
  $fe / $41 / (* cp   'A'     *)
  $d8 /       (* ret  c       *)
  $fe / $5b / (* cp   'Z' + 1 *)
  $d0 /       (* ret  nc      *)
  $cb / $ed / (* set  4,l     *)
  $c9         (* ret          *)
);

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

var
  RandSeed1: Integer absolute 'seed1';
  RandSeed2: Integer absolute 'seed2';

function Random(Range: Integer): Integer; register; external '__random';
function RandomReal: Real; register;                external '__random48';

procedure Randomize; register; inline
(
  $ed / $5f /             (* ld   a,r             *)
  $2a / RandSeed1 /       (* ld   hl,(RandSeed1)  *)
  $ed / $5b / RandSeed2 / (* ld   de,(RandSeed2)  *)
  $53 /                   (* ld   d,e             *)
  $5c /                   (* ld   e,h             *)
  $65 /                   (* ld   h,l             *)
  $6f /                   (* ld   l,a             *)
  $22 / RandSeed1 /       (* ld   (RandSeed1),hl  *)
  $ed / $53 / RandSeed2 / (* ld   (RandSeed2),de  *)
  $c9                     (* ret                  *)
);

procedure CheckBreak; register; external '__checkbreak';

procedure CheckStack; register; external '__checkstack';

(* Built-in: procedure FillChar(var Dest; Length: Integer; Data); *)

procedure Move(var Source, Dest; Count: Integer); register; external '__move';

(* Built-in: procedure Halt([ExitCode: Byte]) *)

procedure Delay(MS: Integer);
var
  A: Byte;
begin
  A := BDos(141, MS div 20);
end;

function KeyPressed: Boolean;
begin
  KeyPressed := BDOS(11, 0) <> 0;
end;

function ReadKey: CHar;
begin
  repeat until KeyPressed;
  ReadKey := Chr(BDOS(6, 255));
end;

(* -------------------------------------------------------------------------- *)
(* --- Assertion support ---------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

(* Built-in: procedure Assert(B: Boolean); *)

var
  AssertPassed: Integer absolute '__assertpassed';
  AssertFailed: Integer absolute '__assertfailed';

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

end.