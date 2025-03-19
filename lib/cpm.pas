(* ===================================================================== *)
(* === CP/M run-time library =========================================== *)
(* ===================================================================== *)

{$a org 0x0100  }
{$a             }
{$a jp __init   }

{$i system.pas  }

{$l cpm.asm     }

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

{$i files.pas}

end.
