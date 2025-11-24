(* ===================================================================== *)
(* === Agon run-time library =========================================== *)
(* ===================================================================== *)

{$l agonhead.asm}

{$i system.pas  } 

{$l agon.asm    }

(* --------------------------------------------------------------------- *)
(* --- Agon/Console8 VDP support ------------------------------------------- *)
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
 * Clear everything from the current cursor position to the end of the
 * line.
 *)
procedure ClrEol; register;                  external '__clrscr';

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
 * Sets the text color (0..7).
 *)
procedure TextColor(I: Integer); register;      external '__textfg';

(**
 * Sets the text background (0..7).
 *)
procedure TextBackground(I: Integer); register; external '__textbg';


//Need to decide which screen modes to use for these.
//Going to use "HIGH" as reverse text, "NORMAL" as normal, "LOW" as
//dim text, all assuming 16 colours available. This will be very
//adjustable based on each application.
// Assuming high means highlighted.
(**
 * Sets the video to "high", whatever that is supposed be. Currently a
 * no-op and just defined to allow other code to build.
 *)
procedure HighVideo; register;  external '__texthigh';


(**
 * Sets the video to "low", whatever that is supposed be. Currently a
 * no-op and just defined to allow other code to build.
 *)
procedure LowVideo; register;   external '__textlow';


(**
 * Sets the video to "norm", whatever that is supposed be. Currently a
 * no-op and just defined to allow other code to build.
 *)
procedure NormVideo; register;   external '__textnorm';


(* -------------------------------------------------------------------------- *)
(* --- Keyboard support ----------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

(**
 * Returns True if a key has been pressed (and can be queried using the
 * ReadKey function), False if not.
 *)
function KeyPressed: Boolean; register;        external '__keypressed';

(**
 * Reads a key press and returns the corresponding ASCII character. Does --- NOT ---
 * echo the character to the screen. Waits for a key press, if necessary,
 * so use KeyPressed first if you don't want your program to be delayed.
 * Also registers keys that have no value, so you may need the full blocking mode below.
 *)
function ReadKey: Char; register;        external '__readkey';

(**
 * Waits for the given interval in milliseconds. Assumes a CP/M platform
 * with BDOS call 141 (i.e. normally CP/M 3, but tnylpo has this as an
 * extension) and a "tick" value of 20 ms (which is the case for both the
 * Spectrum Next's CP/M and tnylpo).
 * Note: Agon has a refresh counter only, so 60Hz (or 75Hz sometimes)
 *)
procedure Delay(Duration: Integer);  register;        external '__delay';
//var
//  A: Byte;
//begin
//  A := BDos(141, Duration div 20);
//end;

(* -------------------------------------------------------------------------- *)
(* --- Command-line parameters ---------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

(**
 * Returns the number of command line parameters.
 *)
function ParamCount: Byte; register;        external '__getargc';

(**
 * Returns the MOS command line content for command number I
 *)
function ParamChar(Param: Byte; Bytenum: Byte): Char; register;        external '__getargvchar';


(**
 * Returns the I'th command line parameter, if it exists, or an empty
 * string otherwise. Can be done better - is grabbing one char at a time on Agon.
 *)
function ParamStr(I: Byte): String;
var
  CmdLine: String[128];
  Tmp: String[255];
  Tmpchar: Char;
  C, D: Boolean;
  J: Byte;
begin
  C := True;

  if I <= ParamCount then
    begin
      for J := 1 to 255 do
      begin
        Tmpchar := ParamChar(I,j);
        if Tmpchar <= ' ' then Break;
        Tmp[J] := Tmpchar;
      end;
    ParamStr := copy(Tmp,1,J-1);
    end
  else
    ParamStr := '';
end;

(* -------------------------------------------------------------------------- *)
(* --- Agon / Console 8 MOS API interface including error handling ---------- *)
(* -------------------------------------------------------------------------- *)


const
  (**
   * The last error that occurred during file IO. Most file functions will
   * be disabled as long as this value is non-zero.
   *)
  LastError: Byte = 0;


type
  (**
   * Represents a standard set of Z80 registers. Used for moving values
   * into and out of the API . Note that HL is always being used here.
   * AF is excluded as A is always used as the MOS API number
   *)
  Registers = record
    case Byte of
      1: (C, B, E, D, L, H: Byte;);
      2: (BC, DE, HL: Integer;);
  end;

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
 * Performs a MOS call. The R parameter is used for passing arguments
 * to the API call as well as getting results back. 
 *)
function MOSAPI(I: Integer; var R: Registers): Byte; register; external 'mos_call';
function MOSAPISeek(var R: Registers): Byte; register; external 'mos_call_seek';
function MOSAPILength(var R: Registers): Integer; register; external 'mos_file_length';

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
   * Internally represents an untyped file. Is also used inside the interal
   * representations of typed files and text files. The name is the same as
   * for the CP/M implementation because higher-level file functions depend
   * on it.
   *)
  FileControlBlock = record
    Handle: Byte;                   (* MOS API file handle, 0=file closed *)
    FileName: array[0..63] of Char; (* File name, 63 characters only      *)
    RL: Integer;                    (* Current record number low 16 bits  *)
                                    (* used in file.pas, record number??  *)
//    RH: Byte;                       (* Current record number high 8 bits  *) //not used in FILE.PAS yet!
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
    if L > 63 then L:= 63;  //safety
    Move(S[1], FileName, L);
    FileName[L] := #0;
  end;
end;


(*
  0x05: mos_del
  Delete a file or folder from the SD card

  Parameters:

  HL(U): Address of filepath (zero terminated)
  Preserves: HL(U)

  Returns:

  A: Status code
 *)
procedure BlockErase(var F: FileControlBlock);
var
  R: Registers;
begin
  if LastError <> 0 then Exit;

  R.HL := Addr(F.FileName);

  LastError := MOSAPI($05, R);
end;

(*
  0x06: mos_ren
  Rename a file on the SD card

  Parameters:

  HL(U): Address of source filepath string (zero terminated)
  DE(U): Address of destination filepath string (zero terminated)
*)
procedure BlockRename(var F: FileControlBlock; S: String); //Untested
var
  G: FileControlBlock;
  R: Registers;
begin
  if LastError <> 0 then Exit;

  BlockAssign(G, S);
//  R.A := Ord('*');
  R.HL := Addr(F.FileName);
  R.DE := Addr(G.FileName);

//  LastError := EsxDos($e0, R);
  LastError := MOSAPI($06, R);
  if LastError = 0 then BlockAssign(F, S); (* TODO Can we rename open files? *)
end;

(*
  0x0A: mos_fopen
  Get a file handle

  Parameters:

  HL(U): Address of filename (zero terminated)
  C: File open mode
  Preserves: HL(U), BC(U), DE(U), IX(U), IY(U)

  Returns:

  A: File handle, or 0 if couldn't open

  0x1C: mos_flseek (recommend the pointer but just want something that works for now)
  Move the read/write pointer in a file (Requires MOS 1.03 or above)
  Parameters:

  C: File handle
  HLU: Least significant 3 bytes of the offset from the start of the file
  E: Most significant byte of the offset (set to 0 for files < 16MB)
  Preserves: HL(U), BC(U), DE(U)

  Returns:

  A: Status code
 *)

procedure BlockReset(var F: FileControlBlock);  //opens file (unless already open) and resets to position 0
var
  R: Registers;
begin
//  if LastError <> 0 then Exit;

//  LastError := EsxDos($9a, R);
  if  F.Handle = 0 then
  begin
    R.HL := Addr(F.FileName);
//    WriteLn('Address of filename :',R.HL);
    // 0x01 FA_READ Open file for reading
    // 0x02  FA_WRITE  Open file for writing. Combine with FA_READ for read/write access
    // 0x10 FA_OPEN_ALWAYS  Open file if it exists, create it if it doesn't
    R.BC := $01+$02+$10;  // Open Always seems to be broken on the emulator
    F.Handle := MOSAPI($0A, R); //mos_fopen:   EQU 0Ah
    LastError := 0; //no errors ever ;-) - need to make it non-zero if F.Handle is 0
    //WriteLn(F.Filename[0],F.Filename[1],Ord(F.Filename[8]));
//    WriteLn('Mode: ',R.BC);
//useful    WriteLn('Reset HandleF: ',F.Handle);
  end
  else
  begin
    R.HL := 0;
    R.DE := 0;
    R.C := F.Handle;
    LastError := MOSAPI($1C, R); //mos_flseek:    EQU 1Ch
//useful    WriteLn('Already open, setting to 0');
  end

end;


procedure BlockRewrite(var F: FileControlBlock); //erases content of file and opens new file in the name
var
  R: Registers;
begin
//  if LastError <> 0 then Exit;

//  LastError := EsxDos($9a, R);
  if  F.Handle = 0 then
  begin
    R.HL := Addr(F.FileName);
    R.C := 8+2+1; //0x08  FA_CREATE_ALWAYS  Create a new file. If the file already exists it will be truncated and overwritten, +2+1 for R/W
    F.Handle := MOSAPI($0A, R); //mos_fopen:   EQU 0Ah
    LastError := 0; //no errors ever ;-)
//useful        WriteLn('Rewrite HandleF: ',F.Handle);
  end
  else
  begin
    R.HL := 0;
    R.DE := 0;
    R.C := F.Handle;
    LastError := MOSAPI($1C, R); //mos_flseek:    EQU 1Ch
  end

end;

(*
  0x0B: mos_fclose
  Close a file handle

  Parameters:

  C: File handle, or 0 to close all open files
  Preserves: HL(U), BC(U), DE(U), IX(U), IY(U)

  Returns:

  A: Number of files still open
 *)
procedure BlockClose(var F: FileControlBlock);
var
  R: Registers;
begin
//  if LastError <> 0 then Exit;

  R.C := F.Handle;
//  WriteLn('Closing handle #',R.C);

  if R.C <> 0 then //don't accidentally close all files
  begin
    LastError := MOSAPI($0b, R); //0x0B: mos_fclose
    F.Handle := 0;
//    WriteLn(Lasterror);
    LastError := 0;
  end

end;

function BlockFilePos(var F: FileControlBlock): Integer;
begin
  BlockFilePos := F.RL;
end;

function BlockFileSize(var F: FileControlBlock): Integer;
var
  R: Registers;
//  B: array[0..10] of Byte;
begin
  if LastError <> 0 then Exit;

  R.C := F.Handle;
//  R.HL := Addr(B);

//  LastError := EsxDos($a1, R);
//  BlockFileSize := 1; //(B[7] or (B[8] shl 8)) div 128;
  BlockFileSize := MOSAPILength(R); //no errors captured
end;

function BlockEof(var F: FileControlBlock): Boolean;
var
  R: Registers;
//  B: array[0..10] of Byte;
begin
  if LastError <> 0 then Exit;

//  BlockEof := BlockFilePos(F) = BlockFileSize(F);

  R.C := F.Handle;
  BlockEof := MOSAPI($0e, R) = 1; //0x0E: mos_feof
end;


(*
0x1C: mos_flseek
Move the read/write pointer in a file (Requires MOS 1.03 or above)

NB this API is deprecated and kept for compatibility reasons. You are advised to use the mos_flseek_p API instead. As this API requires a full 24-bit value to be provided in the HLU register it is not directly compatible with programs written to run in Z80 mode.

This API can be used to expand the size of a file, although you should note that the file data in the expanded part will be undefined.

Please note that on MOS releases prior to MOS 3.0, the status code returned in the A register will be incorrect.

Parameters:

C: File handle
HLU: Least significant 3 bytes of the offset from the start of the file
E: Most significant byte of the offset (set to 0 for files < 16MB)
Preserves: HL(U), BC(U), DE(U)

Returns:

A: Status code
*)
procedure BlockSeek(var F: FileControlBlock; I: Integer);
var
  R: Registers;
begin
  if LastError <> 0 then Exit;

  F.RL := I;

  R.C := F.Handle;
//  R.HL := 0;
  R.DE := 0;
//  R.HL := I*128;
  R.HL := I;
//  WriteLn('Seeking file handle ',R.C,' to byte ',R.HL);
//  LastError := MOSAPI($1c, R);
  LastError := MOSAPISeek(R); //specific routine to take advantage of 24 bit on HL when multiplying by 128.

end;

(*
  0x1A: mos_fread
  Read a block of data from a file (Requires MOS 1.03 or above)

  Parameters:

  C: File handle
  HLU: Pointer to a buffer to read the data into
  DEU: Number of bytes to read
  Preserves: HL(U), BC(U)

  Returns:

  DEU: Number of bytes read
 *)
procedure BlockBlockRead(var F: FileControlBlock; var Buffer; Count: Integer; var Actual: Integer);
var
  R: Registers;
begin
  if LastError <> 0 then Exit;

  R.HL := Addr(Buffer); // where to store data for the read
  Actual := 0;

  while Count > 0 do
  begin
    R.C := F.Handle;
    R.DE := 128;

//useful    WriteLn('Reading ',R.DE,' bytes from file handle ',R.C);

    LastError := MOSAPI($1a, R);
    if (LastError <> 0) or (R.DE = 0) then Exit;

    Inc(F.RL);
    Inc(Actual);
    Dec(Count);

// do we have to fill after EOF? Not sure this will work for us.
//    if R.DE < 128 then
//    begin
//      FillChar(R.HL, 128 - R.DE, #26);
//      Exit;
//    end;
  end;
end;


(*
  0x1B: mos_fwrite
  Write a block of data to a file (Requires MOS 1.03 or above)

  Parameters:

  C: File handle
  HLU: Pointer to a buffer that contains the data to write
  DEU: Number of bytes to write out
  Preserves: HL(U), BC(U)
 *)
procedure BlockBlockWrite(var F: FileControlBlock; var Buffer; Count: Integer; var Actual: Integer);
var
  R: Registers;
begin
//  if LastError <> 0 then Exit;

  R.HL := Addr(Buffer); // where to get the data for write
  Actual := 0;

//useful  WriteLn('Writing ',count,' blocks to file handle ',F.Handle,' content at ',r.hl);

  while Count > 0 do
  begin
    R.C := F.Handle;
    R.DE := 128;

    LastError := MOSAPI($1b, R);
//    if (LastError <> 0) or (R.DE = 0) then Exit; //not sure these exit conditions help
//    WriteLn('Last Error = ',LastError);
    Inc(F.RL);
    Inc(Actual);
    Dec(Count);
  end;
end;
{$i files.pas}

(* --------------------------------------------------------------------- *)
(* --- Simple graphics primitives -------------------------------------- *)
(* --------------------------------------------------------------------- *)

(* Note: Turbo Pascal has point (0,0) in the upper left corner, and so do we.
 * This is in contrast to ZX BASIC, so keep this in mind when porting graphics
 * code from there.
 *)

(**
 * Plots a point at the given coordinates.
 *)
procedure Plot(X, Y: Byte); register;               external 'al_plot';

(**
 * Queries a point at the given coordinates.
 *)
function Point(X, Y: Byte): Boolean; register;      external 'al_point';

(**
 * Draws a line from the last plot position to the given (relative) coordinates.
 *)
procedure Draw(DX, DY: Integer); register;               external 'al_draw';

(**
 * Draws a circle of the given radius around the given center point.
 *)
procedure Circle(CX, CY, Radius: Integer);  register;               external 'al_circle';

end.
