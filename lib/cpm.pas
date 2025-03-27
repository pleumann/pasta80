(* ===================================================================== *)
(* === CP/M run-time library =========================================== *)
(* ===================================================================== *)

{$a org 0x0100  }
{$a             }
{$a jp __init   }

{$i system.pas  }

{$l cpm.asm     }

(* --------------------------------------------------------------------- *)
(* --- VT52 terminal support ------------------------------------------- *)
(* --------------------------------------------------------------------- *)

const
  (**
   * Defines the default with of the CP/M screen in characters.
   *)
  ScreenWidth = 80;

  (**
   * Defines the default height of the CP/M screen in characters.
   *)
  ScreenHeight = 24;

  (**
   * Defines the line break convention used by CP/M.
   *)
  LineBreak = #13#10;

procedure ConOut(C: Char); register;        external '__conout';

(**
 * Clears the screen. Uses the most recently defined text color and
 * background for the attribute area.
 *)
procedure ClrScr; register;                 external '__clrscr';

(**
 * Moves the the cursor (aka printing position) to a given location. Note
 * that Pascal expects the screen column (X) first, followed by the screen
 * row (Y).
 *)
procedure GotoXY(X, Y: Integer); register;  external '__gotoxy';

(**
 * Shows the cursor.
 *)
procedure CursorOn; register;               external '__cursor_on';

(**
 * Hides the cursor.
 *)
procedure CursorOff; register;              external '__cursor_off';

(**
 * Clear everything from the current cursor position to the end of the
 * line.
 *)
procedure ClrEol; register; inline
(
  $3e / 27 /                  (* ld   a,27      *)
  $cd / ConOut /              (* call ConOut    *)
  $3e / 'K' /                 (* ld   a,'K'     *)
  $cd / ConOut /              (* call ConOut    *)
  $c9                         (* ret            *)
);

(**
 * Clear everything from the current cursor position to the end of the
 * screen.
 *)
procedure ClrEos; register; inline
(
  $3e / 27 /                  (* ld   a,27      *)
  $cd / ConOut /              (* call ConOut    *)
  $3e / 'J' /                 (* ld   a,'J'     *)
  $cd / ConOut /              (* call ConOut    *)
  $c9                         (* ret            *)
);

(**
 * Inserts an empty line at the current cursor position, scrolling
 * everything that follows down.
 *)
procedure InsLine; register; inline
(
  $3e / 27 /                  (* ld   a,27      *)
  $cd / ConOut /              (* call ConOut    *)
  $3e / 'L' /                 (* ld   a,'L'     *)
  $cd / ConOut /              (* call ConOut    *)
  $c9                         (* ret            *)
);

(**
 * Deletes a line at the current cursor position, scrolling everything that
 * follows up.
 *)
procedure DelLine; register; inline
(
  $2e / 27 /                  (* ld   l,27      *)
  $cd / ConOut /              (* call ConOut    *)
  $3e / 'M' /                 (* ld   l,'M'     *)
  $cd / ConOut /              (* call ConOut    *)
  $c9                         (* ret            *)
);

(**
 * Sets the text color (0..7).
 *)
procedure TextColor(I: Integer); register;      external '__textfg';

(**
 * Sets the text background (0..7).
 *)
procedure TextBackground(I: Integer); register; external '__textbg';

(**
 * Sets the video to "high", whatever that is supposed be. Currently a
 * no-op and just defined to allow other code to build.
 *)
procedure HighVideo; register; inline
(
  $c9 (* TODO implement me! *)
);

(**
 * Sets the video to "low", whatever that is supposed be. Currently a
 * no-op and just defined to allow other code to build.
 *)
procedure LowVideo; register; inline
(
  $c9 (* TODO implement me! *)
);

(**
 * Sets the video to "norm", whatever that is supposed be. Currently a
 * no-op and just defined to allow other code to build.
 *)
procedure NormVideo; register; inline
(
  $c9 (* TODO implement me! *)
);

(* -------------------------------------------------------------------------- *)
(* --- Keyboard support ----------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

(**
 * Returns True if a key has been pressed (and can be queried using the
 * ReadKey function), False if not.
 *)
function KeyPressed: Boolean;
begin
  KeyPressed := BDOS(11, 0) <> 0;
end;

(**
 * Reads a key press and returns the corresponding ASCII character. Does
 * echo the character to the screen. Waits for a key press, if necessary,
 * so use KeyPressed first if you don't want your program to be delayed.
 *)
function ReadKey: CHar;
begin
  repeat until KeyPressed;
  ReadKey := Chr(BDOS(6, 255));
end;

(**
 * Waits for the given interval in milliseconds. Assumes a CP/M platform
 * with BDOS call 141 (i.e. normally CP/M 3, but tnylpo has this as an
 * extension) and a "tick" value of 20 ms (which is the case for both the
 * Spectrum Next's CP/M and tnylpo).
 *)
procedure Delay(Duration: Integer);
var
  A: Byte;
begin
  A := BDos(141, Duration div 20);
end;

(* -------------------------------------------------------------------------- *)
(* --- Command-line parameters ---------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

(**
 * Returns the number of command line parameters.
 *)
function ParamCount: Byte;
var
  CmdLine: String absolute $80;
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

(**
 * Returns the I'th command line parameter, if it exists, or an empty
 * string otherwise.
 *)
function ParamStr(I: Byte): String;
var
  CmdLine: String absolute $80;
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
(* --- CP/M BDOS interface including error handling ------------------------- *)
(* -------------------------------------------------------------------------- *)

const
  (**
   * The last error that occurred during file IO. Most file functions will
   * be disabled as long as this value is non-zero.
   *)
  LastError: Byte = 0;

(**
 * Queries the last error that happened during an esxDOS call and resets
 * the value to zero, so that further esxDOS calls can be made.
 *)
function IOResult: Byte;
begin
  IOResult := LastError;
  LastError := 0;
end;

(**
 * Performs a BDOS call for the given function number and parameter value.
 * TODO Check if we can merge this with the built-in BDos and BDosHL
 * functions.
 *)
procedure BDosCatch(Func: Byte; Param: Integer);
var
  A: Byte;
begin
  if LastError <> 0 then Exit;
  A := BDos(Func, Param);
  (* WriteLn('BDos(', Func, ') returned A=' , A); *)
  if A <> 0 then LastError := A;
end;

(**
 * Checks the last BDOS error and terminates the program if it is <> 0.
 * Calls to this function are automatically generated by the compiler in
 * {$i+} mode.
 *)
procedure BDosThrow;
begin
  if LastError <> 0 then
  begin
    WriteLn('BDos error ', LastError);
    Halt;
  end;
end;

(* --------------------------------------------------------------------- *)
(* --- Internal implementation of "raw" untyped files ------------------ *)
(* --------------------------------------------------------------------- *)

type
  (**
   * Represents a CP/M file control block.
   *)
  FileControlBlock = record             
    DR: Byte;                           (* Drive number                       *)
    FN: array[0..7] of Char;            (* File name, 8 chars, space-padded   *)
    TN: array[0..2] of Char;            (* Extension, 3 chars, space-padded   *)
    EX, S1, S2, RC: Byte;               (* CP/M internal stuff                *)
    AL: array[0..15] of Byte;           (* CP/M internal stuff                *)
    CR: Byte;                           (* CP/M internal stuff                *)
    RL: Integer; RH: Byte;              (* 24 bit random record number        *)
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

    if LastError = 1 then
    begin
      Mem[DMA] := 26;
      LastError := 0;
    end;

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
