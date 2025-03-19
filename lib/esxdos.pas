(* -------------------------------------------------------------------------- *)
(* --- CP/M 2.2 low-level file support -------------------------------------- *)
(* -------------------------------------------------------------------------- *)

{$l esxdos.asm}

type
  FileControlBlock = record             (* CP/M file control block            *)
    Handle: Byte;
    FileName: array[0..31] of Char;
    RL: Integer;
    RH: Byte;
  end;

(* --- Untyped file routines, use FileControlBlock as representation -------- *)

type
  Registers = record
    AF, BC, DE, HL: Integer;
  end;

const
  LastError: Byte = 0;

function IOResult: Byte;
begin
  IOResult := LastError;
  LastError := 0;
end;

function EsxDos(I: Integer; var R: Registers): Byte; register; external 'esx_call';

procedure BDosThrow; (* -> CheckInOut *)
begin
  if LastError <> 0 then
  begin
    WriteLn('EsxDos error ', LastError);
    Halt;
  end;
end;

procedure BlockAssign(var F: FileControlBlock; S: String);
var
  L: Byte;
begin
  if LastError <> 0 then Exit;

  with F do
  begin
    Handle := 0;
    L := Length(S);
    Move(S[1], FileName, L);
    FileName[L] := #0;
  end;
end;

procedure BlockErase(var F: FileControlBlock);
var
  R: Registers;
begin
  if LastError <> 0 then Exit;

  R.HL := Addr(F.FileName);

  LastError := EsxDos($ad, R);
end;

procedure BlockRename(var F: FileControlBlock; S: String);
var
  G: FileControlBlock;
  R: Registers;
begin
  if LastError <> 0 then Exit;

  BlockAssign(G, S);
  R.HL := Addr(F.FileName);
  R.DE := Addr(G.FileName);

  LastError := EsxDos($e0, R);
  if LastError = 0 then BlockAssign(F, S); (* TODO Can we rename open files? *)
end;

procedure BlockReset(var F: FileControlBlock);
var
  R: Registers;
begin
  if LastError <> 0 then Exit;

  R.HL := Addr(F.FileName);
  R.BC := $0300;

  LastError := EsxDos($9a, R);
  if LastError = 0 then F.Handle := R.AF shr 8;
end;

procedure BlockRewrite(var F: FileControlBlock);
var
  R: Registers;
begin
  if LastError <> 0 then Exit;

  R.HL := Addr(F.FileName);
  R.BC := $0f00;

  LastError := EsxDos($9a, R);
  if LastError = 0 then F.Handle := R.AF shr 8;
end;

procedure BlockClose(var F: FileControlBlock);
var
  R: Registers;
begin
  if LastError <> 0 then Exit;

  R.AF := F.Handle shl 8;

  LastError := EsxDos($9b, R);
  if LastError = 0 then F.Handle := 0;
end;

function BlockFilePos(var F: FileControlBlock): Integer;
begin
  BlockFilePos := F.RL;
end;

function BlockFileSize(var F: FileControlBlock): Integer;
var
  R: Registers;
  B: array[0..10] of Byte;
begin
  if LastError <> 0 then Exit;

  R.AF := F.Handle shl 8;
  R.HL := Addr(B);

  LastError := EsxDos($a1, R);
  BlockFileSize := B[8] shl 8 + B[7];
end;

function BlockEof(var F: FileControlBlock): Boolean;
begin
  if LastError <> 0 then Exit;

  BlockEof := BlockFilePos(F) = BlockFileSize(F);
end;

procedure BlockSeek(var F: FileControlBlock; I: Integer);
var
  R: Registers;
begin
  if LastError <> 0 then Exit;

  F.RL := I;

  R.AF := F.Handle shl 8;
  R.BC := I shr 9;
  R.DE := I shl 7;

  LastError := EsxDos($a1, R);
end;

procedure BlockBlockRead(var F: FileControlBlock; var Buffer; Count: Integer; var Actual: Integer);
var
  R: Registers;
begin
  if LastError <> 0 then Exit;

  R.HL := Addr(Buffer);
  Actual := 0;

  while Count > 0 do
  begin
    R.BC := 128;

    LastError := EsxDos($9d, R);
    if LastError <> 0 then Exit;

    Inc(F.RL);
    Inc(Actual);
    Dec(Count);
  end;
end;

procedure BlockBlockWrite(var F: FileControlBlock; var Buffer; Count: Integer; var Actual: Integer);
var
  R: Registers;
begin
  if LastError <> 0 then Exit;

  R.HL := Addr(Buffer);
  Actual := 0;

  while Count > 0 do
  begin
    R.BC := 128;

    LastError := EsxDos($9e, R);
    if LastError <> 0 then Exit;

    Inc(F.RL);
    Inc(Actual);
    Dec(Count);
  end;
end;