(* -------------------------------------------------------------------------- *)
(* --- Pascal Compiler ------------------------------------------------------ *)
(* -------------------------------------------------------------------------- *)

program Pasta80;

{$mode delphi}

uses
  Keyboard, Dos, Math, Process;

const
  Version = '0.95';

(* -------------------------------------------------------------------------- *)
(* --- Utility functions ---------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

function ReplaceChar(S: String; C, D: Char): String;
var
  I: Integer;
begin
  for I := 1 to Length(S) do
    if S[I] = C then S[I] := D;

  ReplaceChar := S;
end;

(**
 * Converts the given string to upper case.
 *)
function UpperStr(S: String): String;
var
  I: Integer;
begin
  for I := 1 to Length(S) do S[I] := UpCase(S[I]);
  UpperStr := S;
end;

(**
 * Converts the given string to lower case.
 *)
function LowerStr(S: String): String;
var
  I: Integer;
begin
  for I := 1 to Length(S) do S[I] := LowerCase(S[I]);
  LowerStr := S;
end;

function IsAlpha(C: Char): Boolean;
begin
  if (C >= 'a') and (C <= 'z') then
    IsAlpha := True
  else if (C >= 'A') and (C <= 'Z') then
    IsAlpha := True
  else
    IsAlpha := False;
end;

(**
 * Converts the given integer to a string.
 *)
function IntToStr(I: Integer): String;
const
  N = 0;
var
  S: String;
begin
  Str(I, S);
  IntToStr := S;
end;

(**
 * Converts the given integer to a hex number of the given length.
 *)
function IntToHex(Value, Digits: Integer): String;
const
  Hex = '0123456789ABCDEF';
var
  S: String;
begin
  S := '';

  while (Value <> 0) or (Digits <> 0) do
  begin
    S := Hex[1 + Value mod 16] + S;
    Value := Value div 16;
    Digits := Digits - 1;
  end;

  IntToHex := S;
end;

(**
 * Pads the string with spaces at the end, so it has at least the given number
 * of characters.
 *)
function PadStr(S: String; N: Integer): String;
const
  Spaces = '                ';
begin
  if Length(S) >= Abs(N) then
    PadStr := S
  else
  begin
    while Length(S) < N do
      S := S + Spaces;
    PadStr := Copy(S, 1, N);
  end
end;

(**
 * Trims the given string by removing all characters <= ' ' from left and right
 * sides.
 *)
function TrimStr(S: String): String;
var
  I, J: Integer;
begin
  I := 1;
  while ((I <= Length(S)) and (S[I] <= ' ')) do
    I := I + 1;
  J := Length(S) + 1;
  while ((J > I) and (S[J-1] <= ' ')) do
    J := J - 1;
  TrimStr := Copy(S, I, J - I);
end;

(**
 * Checks if the given string starts with the given substring.
 *)
function StartsWith(Str, SubStr: String): Boolean;
begin
  if Length(Str) < Length(SubStr) then
  begin
    StartsWith := False;
    Exit;
  end;

  SetLength(Str, Length(SubStr));
  StartsWith := Str = SubStr;
end;

(**
 * Encodes the given floating-point number (enclosed in a string) the way
 * the Real type needs it.
 *)
function EncodeReal(S: String): String;
var
  Number, Mantissa: Extended;
  Sign: Boolean;
  Exponent, I: Integer;
  Bytes: array[0..5] of Byte;
begin
  Val(S, Number);
  Sign := False;
  Mantissa := 0;
  Exponent := 0;

  if Number = 0 then
  begin
    Result := '$0000,$0000,$0000';
    Exit;
  end;

  if Number < 0 then
  begin
    Sign := True;
    Number := -Number;
  end;

  FRExp(Number, Mantissa, Exponent);

  Bytes[0] := Exponent - 1023 + 127;

  for I := 5 downto 1 do
  begin
    Mantissa := Mantissa * 256;
    Bytes[I] := Trunc(Mantissa);
    Mantissa := Frac(Mantissa);
  end;

  if not Sign then Dec(Bytes[5], 128);

  // TODO: If the 11.th. hex digit is larger than 7 the
  // number should be rounded by adding 1 to BCDEH.

  EncodeReal := '0x' + IntToHex(Bytes[1], 2) + IntToHex(Bytes[0], 2) + ',' +
                '0x' + IntToHex(Bytes[3], 2) + IntToHex(Bytes[2], 2) + ',' +
                '0x' + IntToHex(Bytes[5], 2) + IntToHex(Bytes[4], 2);
end;

function NativeToPosix(Native: String): String;
begin
  {$ifdef windows}
  NativeToPosix := ReplaceChar(Native, '\', '/');
  {$else}
  NativeToPosix := Native;
  {$endif}
end;

function PosixToNative(Posix : string): string;
begin
  {$ifdef windows}
  PosixToNative := ReplaceChar(Posix, '/', '\');
  {$else}
  PosixToNative := Posix;
  {$endif}
end;

(**
 * Returns the parent directory of the given directory.
 *)
function ParentDir(DirName: String): String;
var
  I: Integer;
begin
  I := Length(DirName);
  while (I > 0) and (DirName[I] <> '/') do
    I := I - 1;

  ParentDir := Copy(DirName, 1, I - 1);
end;

(**
 * Changes the extension of the given filename to a new one (or appends it, if
 * there is no file extension at all. The new extension is supposed to start
 * with a dot.
 *)
function ChangeExt(FileName, NewExt: String): String;
var
  I: Integer;
begin
  I := Pos('.', FileName);
  if I = 0 then
    ChangeExt := FileName + NewExt
  else
    ChangeExt := Copy(FileName, 1, I - 1) + NewExt;
end;

(**
 *
 *)
function FAbsolute(Name: String): String;
begin
  FAbsolute := NativeToPosix(FExpand(Name));
end;

(**
 * Returns a given file name as a relative file name based on the current
 * directory if that it possible (i.e. if is contained in that directory).
 * Otherwise it stays unchanged.
 *)
function FRelative(Name: String): String;
var
  Dir: String;
begin
  Dir := FAbsolute('.') + '/';
  if Copy(Name, 1, Length(Dir)) = Dir then
    FRelative := Copy(Name, Length(Dir) + 1, 255)
  else
    FRelative := Name;
end;

(**
 * Returns the size of the given file, or -1 if the file does not exist.
 *)
function FSize(Name: String): Integer;
var
  F: File;
begin
  {$I-}
  Assign(F, Name);
  Reset(F, 1);
  if IOResult = 0 then
  begin
    FSize := FileSize(F);
    Close(F);
  end
  else FSize := -1;
  {$I+}
end;

(**
 * Copies a file, returns Boolean reflecting success or failure.
 *)
function CopyFile(SrcName, DstName: String): Boolean;
var
  Src, Dst: File;
  Buffer: array[0..511] of Byte;
  Count: Integer;
begin
  {$I-}
  Assign(Dst, DstName);
  Rewrite(Dst, 1);

  Assign(Src, SrcName);
  Reset(Src, 1);

  BlockRead(Src, Buffer, 512, Count);

  while (IOResult = 0) and (Count <> 0) do
  begin
    BlockWrite(Dst, Buffer, Count);
    BlockRead(Src, Buffer, 512, Count);
  end;

  Close(Src);
  Close(Dst);
  {$I+}

  CopyFile := IOResult = 0;
end;

(**
 * Returns the compiler's home directory (where it is installed).
 *)
function GetHomeDir: String;
begin
  Result := NativeToPosix(ParentDir(ParamStr(0)));
  {$ifdef darwin}
  if Result = '' then
  begin
    RunCommand('which', [ParamStr(0)], Result);
    Result := TrimStr(Result);
  end;
  {$endif}
end;

(**
 * Returns the user's home directory (where we expect the config).
 *)
function GetUserDir: String;
begin
  {$ifdef windows}
  Result := NativeToPosix(GetEnv('USERPROFILE'));
  {$else}
  Result := NativeToPosix(GetEnv('HOME'));
  {$endif}
end;

(**
 * Executes the given program with the given arguments.
 *)
procedure Execute(const Path, Args: String);
begin
  {$ifdef darwin}
  Exec('/bin/sh', '-c "' + Path + ' ' + Args + '"');
  {$else}
  Exec(Path, Args);
  {$endif}
end;

(* -------------------------------------------------------------------------- *)
(* --- Config handling ------------------------------------------------------ *)
(* -------------------------------------------------------------------------- *)

type
  (**
   * The three platforms we currently support.
   *)
  TBinaryType = (btCPM, btZX, btZX128, btZXN);

  (**
   * The possible output formats.
   *)
  TTargetFormat = (tfBinary, tfPlus3Dos, tfTape, tfSnapshot);

var
  (**
   * The type of binary we are generating.
   *)
  Binary: TBinaryType = btCPM;

  (**
   * The format we are producing.
   *)
  Format: TTargetFormat = tfBinary;

var
  HomeDir, SjAsmCmd, ZasmCmd, NanoCmd, CodeCmd, TnylpoCmd, FuseCmd: String;
  AltEditor: Boolean;

(**
 * Tries to setup the compiler's home directory and the paths to various tools,
 * first by "guessing" via "which", then by loading a config file.
 *)
procedure LoadConfig;
const
  {$ifdef windows}
  EnvKey = 'USERPROFILE';
  Which = 'where';
  {$else}
  EnvKey = 'HOME';
  Which = 'which';
  {$endif}
var
  T: Text;
  UserDir, S, Key, Value: String;
  P: Integer;
begin
  HomeDir := GetHomeDir;
  UserDir := GetUserDir;

  WriteLn('Home dir is: ', HomeDir);
  WriteLn('User dir is: ', UserDir);

  SjAsmCmd  := 'sjasmplus';
  ZasmCmd   := 'zasm';
  NanoCmd   := 'nano';
  CodeCmd   := 'code';
  TnylpoCmd := 'tnylpo';
  FuseCmd   := {$ifdef darwin} 'open -a Fuse' {$else} 'fuse' {$endif};

  {$I-}
  Assign(T, UserDir + '/.pasta80.cfg');
  Reset(T);
  if IOResult = 0 then
  begin
    while not Eof(T) do
    begin
      ReadLn(T, S);
      if not StartsWith(S, '#') and (Length(S) <> 0) then
      begin
        P := Pos('=', S);
        if P <> 0 then
        begin
          Key := LowerStr(TrimStr(Copy(S, 1, P - 1)));
          Value := TrimStr(Copy(S, P + 1, 255));

          if StartsWith(Value, '~') then
            Value := UserDir + Copy(Value, 2, 255);

          if Key = 'home' then
            HomeDir := Value
          else if Key = 'sjasmplus' then
            SjAsmCmd := Value
          else if Key = 'nano' then
            NanoCmd := Value
          else if Key = 'vscode' then
            CodeCmd := Value
          else if Key = 'tnylpo' then
            TnylpoCmd := Value
          else if Key = 'fuse' then
            FuseCmd := Value
          else
          begin
            WriteLn('Invalid config key: ' + Key);
            Halt;
          end;
        end;
      end;
    end;
    Close(T);
  end
  else
  begin
    WriteLn('Warning: File ~/.pasta80.cfg not found. Please consider creating it.');
    WriteLn;
  end;
  {$I+}

  AltEditor := GetEnv('TERM_PROGRAM') = 'vscode'; // Are we running inside VSC?
end;

procedure Emit(Tag, Instruction, Comment: String); forward;
procedure EmitI(S: String); forward;
procedure Error(Message: String); forward;
procedure SetLibrary(FileName: String); forward;

(* -------------------------------------------------------------------------- *)
(* --- Input ---------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

type
  (**
   * Represents a source file being processed. We maintain a linked list of such
   * sources that we treat like a stack to deal with (potentially nested)
   * includes.
   *)
  PSource = ^TSource;
  TSource = record
    Name: String;
    Input:  Text;
    Buffer: String;
    Line:   Integer;
    Column: Integer;
    Next: PSource;
  end;

var
  (**
   * The pointer to the current source file.
   *)
  Source: PSource;

(**
 * Open the given input file. The same procedure is called for the initial
 * input file and for all nested includes.
 *)
procedure OpenInput(FileName: String);
var
  Tmp: PSource;
begin
  if (Source <> nil) and not StartsWith(FileName, '/') then
    FileName := ParentDir(FAbsolute(Source^.Name)) + '/' + FileName;

  Tmp := Source;
  while Tmp <> nil do
  begin
    if Tmp^.Name = FileName then Error('Cyclic include');
    Tmp := Tmp^.Next;
  end;

  New(Tmp);

  with Tmp^ do
  begin
    Name := FileName;
    {$I-}
    Assign(Input, Name);
    Reset(Input);
    if IOResult <> 0 then
    begin
      Dispose(Tmp);
      Error('File "' + PosixToNative(Name) + '" not found');
    end;
    {$I+}
    Buffer := '';
    Line := 0;
    Column := 1;
    Next := Source;
  end;

  Source := Tmp;
end;

(**
 * Closes the current input file and resumes scanning/parsing of the enclosing
 * input file (if one exists).
 *)
procedure CloseInput();
var
  Tmp: PSource;
begin
  Tmp := Source;
  Source := Tmp^.Next;
  Close(Tmp^.Input);
  Dispose(Tmp);
end;

procedure EmitC(S: String); forward;

(**
 * Returns (and consumes) the next input character. This function is called
 * routinely by the scanner.
 *)
function GetChar(): Char;
begin
  with Source^ do
  begin
    if Column > Length(Buffer) then
    begin
      if Eof(Input) then
      begin
        if Next <> nil then
        begin
          CloseInput;
          GetChar := ' ';
          Exit;
        end
        else Error('Unexpected end of source');
      end;

      ReadLn(Input, Buffer);
      EmitC('[' + IntToStr(Line) + '] ' + Buffer); // TODO Make this configurable?
      Buffer := Buffer + #13;
      Line := Line + 1;
      Column := 1;
    end;

    GetChar := Buffer[Column];
    Inc(Column);
  end;
end;

(**
 * Pushes back a single character. This - admittedly slighly ugly - procedure
 * is called by the scanner. It must not be called at the beginning of a line
 * or source code file, but this never happens.
 *)
procedure UngetChar();
begin
  Source^.Column := Source^.Column - 1;
end;

(* -------------------------------------------------------------------------- *)
(* --- String table --------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

function GetLabel(Prefix: String): String; forward;

type
  (**
   * A linked list of string constants used in the program. No duplicates. The
   * strings are written at the end of parsing.
   *
   * TODO Make this a hash table in case we want to run on 8 bit.
   * TODO Add something similar for Real constants and maybe for set constants.
   *)
  PStringLiteral = ^TStringLiteral;
  TStringLiteral = record
    Tag: String;
    Value: String;
    Next: PStringLiteral;
  end;

var
  (**
   * The head of our list.
   *)
  Strings: PStringLiteral;

(**
 * Adds a string to the list and returns a label under which it is accessible
 * for assembly code. If the string is already part of the list the existing
 * label will be returned.
 *)
function AddString(S: String): String;
var
  Temp: PStringLiteral;
begin
  if Strings = nil then
  begin
    New(Strings);
    Strings^.Tag := GetLabel('string');
    Strings^.Value := S;
    Strings^.Next := nil;
    AddString := Strings^.Tag;
  end
  else
  begin
    Temp := Strings;
    while Temp <> nil do
    begin
      if Temp^.Value = S then
      begin
        AddString := Temp^.Tag;
        Exit;
      end;

      if Temp^.Next = nil then
      begin
        New(Temp^.Next);
        Temp^.Next^.Tag := GetLabel('string');
        Temp^.Next^.Value := S;
        Temp^.Next^.Next := nil;
        AddString := Temp^.Next^.Tag;
        Exit;
      end;

      Temp := Temp^.Next;
    end;
  end;
end;

procedure ClearStrings;
var
  Tmp: PStringLiteral;
begin
  while Strings <> nil do
  begin
    Tmp := Strings;
    Strings := Tmp^.Next;
    Dispose(Tmp);
  end;
end;

(* -------------------------------------------------------------------------- *)
(* --- Symbol table --------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

type
  TSymbolClass = (scConst, scType, scArrayType, scRecordType, scEnumType,
                  scStringType, scSetType, scSubrangeType, scPointerType,
                  scFileType, scAliasType, scVar, scLabel, scProc, scFunc,
                  scScope);

  // Flags: Forward, External, Register, Magic, Reference, Writable, Relative, Sizable, Addressable

  (**
   * Represents an entry in our symbol table. The symbol table is a linked list
   * that operates a bit like a stack: New identifiers are put at the front and
   * may shadow existing ones. Identifiers are removed in reverse order once
   * they fall out of scope. Not all fields are used by all kinds of sybols,
   * some have a different meaning depending on the symbol they are used for.
   * "Value", for instance, can be the integer value of a constant, but also the
   * size of a type. This, although working, is slightly ugly and maybe worth a
   * clean-up.
   *)
  PSymbol = ^TSymbol;
  TSymbol = record
    Name: String;                       // Name of the symbol
    Kind: TSymbolClass;                 // Kind of the symbol
    DataType: PSymbol;                  // Data type, array type or return type
    ArgTypes: array[0..15] of PSymbol;  // Procecure/function parameter types
    ArgIsRef: array[0..15] of Boolean;  // Procedure/function "var" parameters
    Level: Integer;                     // Level of procedure/function, 0=global
    Value, Value2: Integer;             // Value(s) or size(s) of symbol
    Tag: String;                        // External label or address of symbol
    IndexType: PSymbol;                 // Index type of arrays
    Low, High: Integer;                 // Values of a range or an array index
    Prev, Next: PSymbol;                // Previous and next in linked list
    IsMagic: Boolean;                   // Magics are built into the compiler
    IsRef: Boolean;                     // Symbol is a reference (var param)
    IsStdCall: Boolean;                 // Procedure/function calling convention
    IsForward: Boolean;                 // Forward-declared procedure/function
    IsExternal: Boolean;                // External, to be resolved by assembler
    SavedParams: PSymbol;               // Original parameters, forward-case only
    IsAlive: Boolean;
    Deps: array[0..127] of PSymbol;
    DepCount: Integer;
  end;

var
  (**
   * The head (aka newest entry) of our symbol table. Searches for an identifier
   * usually start from here.
   *)
  SymbolTable: PSymbol = nil;

  (**
   * The most recent scope marker.
   *)
  CurrentScope: PSymbol = nil;

  (**
   * The last built-in identifier.
   *)
  LastBuiltIn: PSymbol = nil;

  CurrentBlock: PSymbol = nil;

  (**
   * Nesting level and variable offset. Maintained by compiler.
   *)
  Level, Offset: Integer;

  (**
   * Pointers to several important built-in base types.
   *)
  dtInteger, dtBoolean, dtChar, dtByte, dtString, dtReal, dtPointer, dtFile,
  dtText: PSymbol;

  (**
   * Pointers to the built-in Mem[] and Port[] pseudo-arrays.
   *)
  MemArray, PortArray: PSymbol;

  (**
   * Pointers to a whole lot of built-in procedures and fucntions that are
   * considered "magic" and cannot be defined in Pascal. This is mostly because
   * their number or type of parameters is not fixed.
   *)
  AssertProc, BreakProc, ContProc, ExitProc, HaltProc, StrProc, ReadProc,
  ReadLnProc, WriteProc, WriteLnProc, EraseProc, RenameProc, AssignProc,
  ResetProc, RewriteProc, AppendProc, FlushProc, CloseProc, SeekProc,
  SeekEofProc, SeekEolnProc, BlockReadProc, BlockWriteProc, FilePosFunc,
  FileSizeFunc, EolFunc, EofFunc, AbsFunc, AddrFunc, DisposeProc, EvenFunc,
  HighFunc, LowFunc, NewProc, OddFunc, OrdFunc, PredFunc, FillProc, IncProc,
  DecProc, ConcatFunc, ValProc, IncludeProc, ExcludeProc, PtrFunc, SizeFunc,
  SuccFunc, BDosFunc, BDosHLFunc: PSymbol;

  SmartLink: Boolean; (* TODO Move elsewhere *)

(**
 * Opens a scope, which mainly adds a new scope marker at the front of the
 * symbol table. If AdjustLevel is true (which is the case for procedures and
 * functions, especially nested ones) level and variable offset are affected,
 * too.
 *)
procedure OpenScope(AdjustLevel: Boolean);
var
  Sym: PSymbol;
begin
  New(Sym);
  FillChar(Sym^, SizeOf(TSymbol), 0);

  Sym^.Kind := scScope;
  Sym^.Prev := SymbolTable;
  Sym^.DataType := CurrentScope;
  if SymbolTable <> nil then SymbolTable^.Next := Sym;
  Sym^.Value := Offset;
  SymbolTable := Sym;
  CurrentScope := Sym;

  if AdjustLevel then
  begin
    if Level = 7 then Error('Nesting too deep.');

    Level := Level + 1;
    Offset := 0;
  end;
end;

(**
 * Closes a scope, removing all symbols up to and including the most recent
 * scope marker. Adjusts level, if requested. Will also detect unimplemented
 * forward procedures/functions in that scope and raise errors if any are
 * found.
 *)
procedure CloseScope(AdjustLevel: Boolean);
var
  Sym: PSymbol;
  Kind: TSymbolClass;
  I: Integer;
begin
  while SymbolTable <> CurrentScope do
  begin
    Kind := SymbolTable^.Kind;

    if ((Kind = scProc) or (Kind = scFunc)) then with SymbolTable^ do
    begin
      if IsForward then
        Error('Unresolved forward declaration "' + SymbolTable^.Name + '"');

      if SmartLink and (Level = 0) and IsStdCall and not IsExternal then
      begin
        if IsAlive then
        begin
          Emit('__USE' + Tag, 'equ 1', '');
//          for I := 0 to DepCount - 1 do
//            Deps[I]^.IsAlive := True;
        end
        else
          Emit('__USE' + Tag, 'equ 0', '');
      end;
    end;

    Sym := SymbolTable^.Prev;
    Dispose(SymbolTable);
    SymbolTable := Sym;
  end;

  CurrentScope := SymbolTable^.DataType;

  if AdjustLevel then
  begin
    Level := Level - 1;
    Offset := SymbolTable^.Value;
  end;

  Sym := SymbolTable^.Prev;
  Dispose(SymbolTable);
  SymbolTable := Sym;
end;

procedure Dependencies(Sym: PSymbol);
var
  I: Integer;
begin
  with Sym^ do
  begin
    //WriteLn('*** ', Name);

    if IsAlive then Exit;

    //WriteLn(Name, ' is alive');

    IsAlive := True;

    for I := 0 to DepCount - 1 do
      Dependencies(Deps[I]);
  end;
end;

procedure AddDependency(Src, Dst: PSymbol);
var
  I: Integer;
begin
    if (Src <> nil) then
    begin
      if Dst <> Src then with Src^ do
      begin
        for I := 0 to DepCount - 1 do
          if Deps[I] = Dst then Exit;

        if DepCount = 128 then Error('Too many dependencies'); (* TODO Fix me! *)

        //WriteLn(CurrentBlock^.Name, ' depends on ', Sym^.Name);

        Deps[DepCount] := Dst;
        Inc(DepCount);
      end
    end
    else
    begin
      //WriteLn('Main program depends on ', Sym^.Name);
      Dependencies(Dst);
    end;
  end;
(**
 * Searches the symbol table for the given identifier. Returns the symbol or
 * nil if it does not exist. The search starts and stops at the given symbols,
 * so passing the current scope as the second symbol would perform a local
 * search, while passing nil would perform a global one. Runtime is O(n) due
 * to the fact that we use a linked list for the symbol table (but modern
 * machines are so fast we don't notice).
 *)
function Lookup(Name: string; Start, Stop: PSymbol): PSymbol;
var
  Sym: PSymbol;
  I: Integer;
begin
  Name := UpperStr(Name);
  Sym := Start;
  while Sym <> Stop do
  begin
    if UpperStr(Sym^.Name) = Name then
    begin
      Lookup := Sym;

      (*
      Wenn lookup in main block stattfindet, nimm Symbol als Wurzel in irgendeine
      Liste auf. Noch nicht aktivieren. Beim Schließen des main blocks rekursive
      Durchläufe bei allen Wurzeln starten.
      *)


      Exit;
    end;
    Sym := Sym^.Prev;
  end;

  Lookup := nil;
end;

(**
 * Searches the local scope for the given identifier and returns the
 * corresponding symbol. Throws an error if the symbol cannot be found.
 *)
function LookupLocalOrFail(Name: String): PSymbol;
var
  Sym: PSymbol;
begin
  Sym := Lookup(Name, SymbolTable, CurrentScope);
  if Sym = nil then Error('Unknown identifier "' + Name + '"');
  LookupLocalOrFail := Sym;
end;

(**
 * Searches the global scope for the given identifier and returns the
 * corresponding symbol. Throws an error if the symbol cannot be found.
 *)
function LookupGlobalOrFail(Name: String): PSymbol;
var
  Sym: PSymbol;
begin
  Sym := Lookup(Name, SymbolTable, nil);
  if Sym = nil then Error('Unknown identifier "' + Name + '"');
  LookupGlobalOrFail := Sym;
end;

(**
 * Searches the built-ins for the given identifier and returns the
 * corresponding symbol. Throws an error if the symbol cannot be found.
 *)
function LookupBuiltInOrFail(Name: String): PSymbol;
var
  Sym: PSymbol;
begin
  Sym := Lookup(Name, LastBuiltIn, nil);
  if Sym = nil then Error('Not supported.');
  LookupBuiltInOrFail := Sym;
end;

(**
 * This procedure is called when a procedure or function header is fully
 * parsed. It calculates the final offsets of all parameters relative to the
 * base pointer according to their sizes. We cannot do this earlier because we
 * don't know in advance the number of parameters, nor their sizes.
 *)
procedure AdjustOffsets;
var
  Sym, Sym2: PSymbol;
  I: Integer;
begin
  Sym := SymbolTable;
  I := 0;
  while (Sym^.Kind<>scProc) and (Sym^.Kind<>scFunc) do
  begin
    if Sym^.Kind = scVar then
    begin
      Sym^.Value := Sym^.Value - Offset + 4;
      I := I + 1;
    end;
    Sym := Sym^.Prev;
  end;
  if Sym^.Kind = scFunc then
    I := I - 1;
  Sym^.Value := I;

  Sym2 := SymbolTable;
  while I > 0 do
  begin
    if Sym2^.Kind = scVar then
    begin
      Sym^.ArgTypes[I-1] := Sym2^.DataType;
      Sym^.ArgIsRef[I-1] := Sym2^.IsRef;
      I := I - 1;
    end;
    Sym2 := Sym2^.Prev;
  end;

  Offset := 0;
end;

(**
 * Creates a new symbol of the given kind. Makes sure the given name (if any)
 * is unique within the local scope.
 *)
function CreateSymbol(Kind: TSymbolClass; Name: String): PSymbol;
var
  Sym: PSymbol;
begin
  if (Length(Name) <> 0) and (Lookup(Name, SymbolTable, CurrentScope) <> nil) then
    Error('Duplicate symbol "' + Name + '"');

  New(Sym);
  FillChar(Sym^, SizeOf(TSymbol), 0);
  Sym^.Kind := Kind;
  Sym^.Name := Name;
  Sym^.Level := Level;
  Sym^.Prev := SymbolTable;
  Sym^.Tag := '';

  SymbolTable^.Next := Sym;
  SymbolTable := Sym;

  CreateSymbol := Sym;
end;

(**
 * Sets the data type of a variable or parameter. This is also the moment when
 * the offset of the symbol on the stack will be assigned (although, for
 * parameters, it will be adjusted later once all parameters are known).
 *)
procedure SetDataType(Sym: PSymbol; DataType: PSymbol);
begin
  Sym^.DataType := DataType;

  if (Level = 0) and (Sym^.Tag = '') then
  begin
    // TODO Improve this! It's an ugly way of detecting global variables.
    // TODO Move away from mixed code/data to a separate data segment.
  end
  else
  begin
    if Sym^.IsRef then
      Offset := Offset - 2                // A var parameter requires two bytes
    else if DataType^.Value = 1 then
      Offset := Offset - 2                // Two bytes is also the minimum on the stack
    else
      Offset := Offset - DataType^.Value; // Everything else just uses its real size

    Sym^.Value := Offset;
  end;
end;

// TODO Do we really need this?
function RegisterType(Name: String; Size: Integer): PSymbol;
var
  Sym: PSymbol;
begin
  Sym := CreateSymbol(scType, Name);
  Sym^.Value := Size;

  RegisterType := Sym;
end;

// TODO Do we really need this?
function NewEnumType(Name: String): PSymbol;
var
  Sym: PSymbol;
begin
  Sym := CreateSymbol(scEnumType, Name);
  Sym^.Value := 1;
  NewEnumType := Sym;
end;

// TODO Do we really need this?
function NewConst(Name: String; DataType: PSymbol; Value: Integer): PSymbol;
var
  Sym: PSymbol;
begin
  Sym := CreateSymbol(scConst, Name);
  Sym^.DataType := DataType;
  Sym^.Value := Value;
  NewConst := Sym;
end;

(**
 * Registers a new magic symbol of the given type and with the given name.
 * Magic symbols are those that get their own parsing, usually because
 * they have a variable parameter list.
 *)
function RegisterMagic(Kind: TSymbolClass; Name: String): PSymbol;
var
  Sym: PSymbol;
begin
  Sym := CreateSymbol(Kind, Name);
  Sym^.IsMagic := True;
  RegisterMagic := Sym;
end;

(**
 * Registers all symbols that must be baked into the compiler and cannot be
 * defined by means of Pascal source code.
 *)
procedure RegisterAllBuiltIns;
begin
  dtInteger := RegisterType('Integer', 2);
  dtInteger^.Low := -32768;
  dtInteger^.High := 32767;

  dtBoolean := NewEnumType('Boolean');
  dtBoolean^.Low := 0;
  dtBoolean^.High := 1;
  dtBoolean^.Tag := '__boolean_enum';
  NewConst('False', dtBoolean, 0);
  NewConst('True', dtBoolean, 1);

  dtChar := RegisterType('Char', 1);
  dtChar^.Low := 0;
  dtChar^.High := 255;
  dtChar^.Value := 1;
  dtByte := RegisterType('Byte', 1);
  dtByte^.Low := 0;
  dtByte^.High := 255;
  dtByte^.Value := 1;

  dtString := CreateSymbol(scStringType, 'String');
  dtString^.Value := 256;

  dtFile := CreateSymbol(scFileType, 'File');
  dtFile^.Value := 256; (* FIXME *)

  dtText := CreateSymbol(scFileType, 'Text');
  dtText^.Value := 256; (* FIXME *)

  dtReal := CreateSymbol(scType, 'Real');
  dtReal^.Value := 6;

  dtPointer := CreateSymbol(scPointerType, 'Pointer');
  dtPointer^.Value := 2;

  MemArray := CreateSymbol(scVar, 'Mem');
  MemArray^.DataType := dtByte;
  MemArray^.IsMagic := True;

  PortArray := CreateSymbol(scVar, 'Port');
  PortArray^.DataType := dtByte;
  PortArray^.IsMagic := True;

  AssertProc := RegisterMagic(scProc, 'Assert');
  BreakProc := RegisterMagic(scProc, 'Break');
  ContProc := RegisterMagic(scProc, 'Continue');
  DisposeProc := RegisterMagic(scProc, 'Dispose');
  ExitProc := RegisterMagic(scProc, 'Exit');
  HaltProc := RegisterMagic(scProc, 'Halt');
  NewProc := RegisterMagic(scProc, 'New');
  StrProc := RegisterMagic(scProc, 'Str');
  ReadProc := RegisterMagic(scProc, 'Read');
  ReadLnProc := RegisterMagic(scProc, 'ReadLn');
  WriteProc := RegisterMagic(scProc, 'Write');
  WriteLnProc := RegisterMagic(scProc, 'WriteLn');

  EraseProc := RegisterMagic(scProc, 'Erase');
  RenameProc := RegisterMagic(scProc, 'Rename');

  AssignProc := RegisterMagic(scProc, 'Assign');
  ResetProc := RegisterMagic(scProc, 'Reset');
  RewriteProc := RegisterMagic(scProc, 'Rewrite');
  AppendProc := RegisterMagic(scProc, 'Append');
  CloseProc := RegisterMagic(scProc, 'Close');
  FlushProc := RegisterMagic(scProc, 'Flush');
  SeekProc := RegisterMagic(scProc, 'Seek');
  SeekEofProc := RegisterMagic(scProc, 'SeekEof');
  SeekEolnProc := RegisterMagic(scProc, 'SeekEoln');

  BlockReadProc := RegisterMagic(scProc, 'BlockRead');
  BlockWriteProc := RegisterMagic(scProc, 'BlockWrite');

  FilePosFunc := RegisterMagic(scFunc, 'FilePos');
  FileSizeFunc := RegisterMagic(scFunc, 'FileSize');
  EolFunc := RegisterMagic(scFunc, 'Eoln');
  EofFunc := RegisterMagic(scFunc, 'Eof');

  IncProc := RegisterMagic(scProc, 'Inc');
  DecProc := RegisterMagic(scProc, 'Dec');

  ConcatFunc := RegisterMagic(scFunc, 'Concat');
  ValProc := RegisterMagic(scProc, 'Val');

  IncludeProc := RegisterMagic(scProc, 'Include');
  ExcludeProc := RegisterMagic(scProc, 'Exclude');

  FillProc := RegisterMagic(scProc, 'FillChar');

  AbsFunc := RegisterMagic(scFunc, 'Abs');
  AddrFunc := RegisterMagic(scFunc, 'Addr');
  EvenFunc := RegisterMagic(scFunc, 'Even');
  HighFunc := RegisterMagic(scFunc, 'High');
  LowFunc := RegisterMagic(scFunc, 'Low');
  OddFunc := RegisterMagic(scFunc, 'Odd');
  OrdFunc := RegisterMagic(scFunc, 'Ord');
  PredFunc := RegisterMagic(scFunc, 'Pred');
  PtrFunc := RegisterMagic(scFunc, 'Ptr');
  SizeFunc := RegisterMagic(scFunc, 'SizeOf');
  SuccFunc := RegisterMagic(scFunc, 'Succ');

  if Binary = btCPM then
  begin
    BDosFunc := RegisterMagic(scFunc, 'Bdos');
    BDosHLFunc := RegisterMagic(scFunc, 'BdosHL');
  end;
end;

(* --------------------------------------------------------------------- *)
(* --- Scanner --------------------------------------------------------- *)
(* --------------------------------------------------------------------- *)

type
  (**
   * An enumeration type containing all possible tokens. The order is
   * significant and must not be changed without changing the TokenStr array
   * accordingly. Also all tokens representing keywords are in one large,
   * alphabetically sorted block.
   *)
  TToken = (toNone,
            toIdent, toNumber, toString, toFloat, toAdd, toSub, toMul, toDiv,
            toEq, toNeq, toLt, toLeq, toGt, toGeq, toLParen, toRParen, toLBrack, toRBrack,
            toBecomes, toComma, toColon, toSemicolon, toPeriod, toCaret, toRange,
            toAbsolute, toAnd, toArray, toBegin, toCase, toConst, toDivKw, toDo, toDownto, toElse, toEnd,
            toExternal, toFile, toFor, toForward, toFunction, toGoto, toIf, toIn, toInline, toLabel, toMod,
            toNil, toNot, toOf, toOr, toOverlay, toPacked, toProcedure, toProgram, toRecord, toRepeat, toSet,
            toShl, toShr, toStringKw, toThen, toTo, toType, toUntil, toVar, toWhile, toWith, toXor,
            toEof);

  (**
   * Describes the state of our scanner, basically the last generated token.
   * TODO Eliminate this type. We only ever need a single one.
   *)
  TScanner = record
    Token:    TToken;
    StrValue: String;
    NumValue: Integer;
  end;

const
  (**
   * An array containing textual representations of all possible tokens. The
   * order is significant and must not be changed without changing the TToken
   * type accordingly. Also all tokens representing keywords are in one large,
   * alphabetically sorted block.
   *)
  TokenStr: array[TToken] of String =
           ('<nul>',
            'Identifier', 'Number', 'String', 'Real', '+', '-', '*', '/',
            '=', '#', '<', '<=', '>', '>=', '(', ')', '[', ']',
            ':=', ',', ':', ';', '.', '^', '..',
            'absolute', 'and', 'array', 'begin', 'case', 'const', 'div', 'do', 'downto', 'else', 'end',
            'external', 'file', 'for', 'forward', 'function', 'goto', 'if', 'in', 'inline', 'label', 'mod',
            'nil', 'not', 'of', 'or', 'overlay', 'packed', 'procedure', 'program', 'record', 'repeat', 'set',
            'shl', 'shr', 'string', 'then', 'to', 'type', 'until', 'var', 'while', 'with', 'xor',
            '<eof>');

  (**
   * Indices of the first and last keywords. Required for keyword detection.
   *)
  FirstKeyword = toAbsolute;
  LastKeyword = toXor;

var
  (**
   * The state of our scanner. Basically the lookahead token, i.e. the next
   * token to process.
   *)
  Scanner: TScanner;

  (**
   * The line and column of the current token.
   *)
  TokenLine, TokenColumn: Integer;

(* TODO Doesn't belong here. Fix later. *)
const
  AbsCode: Boolean = False;
  CheckBreak: Boolean = False;
  IOMode: Boolean = True;
  StackMode: Boolean = False;

(**
 * Returns True if the given character is a valid Pascal identifier head.
 *)
function IsIdentHead(C: Char): Boolean;
begin
  IsIdentHead := (C >= 'A') and (C <= 'Z') or (C >= 'a') and (C <= 'z') or (C = '_');
end;

(**
 * Returns True if the given character is a valid Pascal identifier tail.
 *)
function IsIdentTail(C: Char): Boolean;
begin
  IsIdentTail := IsIdentHead(C) or (C >= '0') and (C <= '9');
end;

(**
 * Returns True if the given character is a decimal digit.
 *)
function IsDecDigit(C: Char): Boolean;
begin
  IsDecDigit := (C >= '0') and (C <= '9');
end;

(**
 * Returns True if the given character is a valid hexadecimal digit.
 *)
function IsHexDigit(C: Char): Boolean;
begin
  IsHexDigit := IsDecDigit(C) or (C >= 'A') and (C <= 'F') or (C >= 'a') and (C <= 'f');
end;

(**
 * Returns True if the given character is a valid binary digit.
 *)
function IsBinDigit(C: Char): Boolean;
begin
  IsBinDigit := (C = '0') or (C = '1');
end;

(**
 * Looks up a keyword and returns its token. Returns toIdent if the string is
 * an ordinary identifier.
 *
 * TODO Make this a binary search or use hashing.
 *)
function LookupKeyword(Ident: String): TToken;
var
  T: TToken;
begin
  Ident := LowerStr(Ident);

  for T := FirstKeyword to LastKeyword do
    if TokenStr[T] = Ident then
    begin
      LookupKeyword := T;
      Exit;
    end;

  LookupKeyword := toIdent;
end;

var
  (**
   * The lookahead character, i.e. the next character to process.
   *)
  C: Char;

(**
 * Scans the input and generates the next token. A bit lengthy, but it does the
 * job.
 *
 * TODO Pull a couple of longer blocks into their own procedures.
 *)
procedure NextToken();
var
  S: String;
  I: Integer;
begin
  // Ignore whitespace
  while (C <= ' ') do
  begin
    C := GetChar;
  end;

  with Scanner do
  begin
    // Start a new token
    Token := toNone;
    StrValue := '';
    NumValue := 0;
    TokenLine := Source^.Line;
    TokenColumn := Source^.Column - 1;

    if IsIdentHead(C) then
    begin
      // Identifier
      StrValue := C;
      C := GetChar;
      while IsIdentTail(C) do
      begin
        StrValue := StrValue + C;
        C := GetChar;
      end;
      Token := LookupKeyword(StrValue);
    end
    else if IsDecDigit(C) then
    begin
      // Let's start with an integer number
      Token := toNumber;
      StrValue := C;
      NumValue := Ord(C) - Ord('0');
      C := GetChar;
      while IsDecDigit(C) do
      begin
        StrValue := StrValue + C;
        NumValue := 10 * NumValue + (Ord(C) - Ord('0'));
        C := GetChar;
      end;

      // Might turn out to be floating point
      if C = '.' then
      begin
        C := GetChar;
        if C = '.' then
        begin
          UngetChar;
          Exit;
        end;

        Token := toFloat;
        StrValue := StrValue + '.';

        while IsDecDigit(C) do
        begin
          StrValue := StrValue + C;
          C := GetChar;
        end;
      end;

      // And even have an exponential part
      if (C = 'E') or (C = 'e') then
      begin
        Token := toFloat;

        StrValue := StrValue + C;
        C := GetChar;

        if (C = '+') or (C = '-') then
        begin
          StrValue := StrValue + C;
          C := GetChar;
        end;

        if not IsDecDigit(C) then Error('Digit expected');

        while IsDecDigit(C) do
        begin
          StrValue := StrValue + C;
          C := GetChar;
        end;
      end;

    end
    else if C = '$' then
    begin
      // Hex numbers are numbers, too.
      Token := toNumber;
      StrValue := '$';
      C := GetChar;

      if not IsHexDigit(C) then Error('Hex digit expected');

      repeat
        StrValue := StrValue + C;
        NumValue := (NumValue shl 4);
        if (C >= '0') and (C <= '9') then
          NumValue := NumValue + Ord(C) - Ord('0')
        else if (C >= 'A') and (C <= 'F') then
          NumValue := NumValue + Ord(C) - Ord('A') + 10
        else
          NumValue := NumValue + Ord(C) - Ord('a') + 10;

        C := GetChar;
      until not IsHexDigit(C);
    end
    else if C = '%' then
    begin
      // Binary numbers are numbers, too.
      Token := toNumber;
      StrValue := '%';
      C := GetChar;

      if not IsBinDigit(C) then Error('Binary digit expected');

      repeat
        StrValue := StrValue + C;
        NumValue := (NumValue shl 1);
        if C = '1' then NumValue := NumValue + 1;
        C := GetChar;
      until not IsBinDigit(C);
    end
    else if (C = '''') or (C = '#') then
    begin
      // Strings come in various forms
      Token := toString;

      repeat
        if C = '''' then
        begin
          // Standard string delimited by apostrophes
          C := GetChar;
          while True do
          begin
            while (C <> '''') and (C <> #26) do
            begin
              StrValue := StrValue + C;
              C := GetChar;
            end;

            if C = #26 then Error('Unterminated String') else C := GetChar; (* ??? *)

            if C = '''' then
            begin
              StrValue := StrValue + '''';
              C := GetChar;
            end
            else Break;
          end;
        end
        else
        begin
          // Hash sign followed by an ASCII code
          I := 0;
          C := GetChar;
          if not IsDecDigit(C) then Error('Dec digit expected');
          repeat
            I := I * 10 + Ord(C) - Ord('0');
            C := GetChar;
          until not IsDecDigit(C);
          StrValue := StrValue + Char(I);
        end;
      until (C <> '''') and (C <> '#');
    end
    else if C = '{' then
    begin
      // Standard Pascal comments
      S := '{';
      repeat
        C := GetChar;
        S := S + C;
      until C = '}';
      C := GetChar;

      // Might turn out to be compiler directives
      if LowerStr(Copy(S, 2, 3)) = '$i ' then
        OpenInput(NativeToPosix(TrimStr(Copy(S, 4, Length(S) - 4))))
      else if LowerStr(Copy(S, 2, 3)) = '$a ' then
        EmitI(TrimStr(Copy(S, 4, Length(S) - 4)))
      else if LowerStr(Copy(S, 2, 2)) = '$u' then
        CheckBreak := S[4] = '+'
      else if LowerStr(Copy(S, 2, 2)) = '$a' then
        AbsCode := S[4] = '+'
      else if LowerStr(Copy(S, 2, 2)) = '$i' then
        IOMode := S[4] = '+'
      else if LowerStr(Copy(S, 2, 2)) = '$k' then
        StackMode := S[4] = '+'
      else if LowerStr(Copy(S, 2, 2)) = '$l' then
        SetLibrary(NativeToPosix(TrimStr(Copy(S, 4, Length(S) - 4))));

      NextToken;
      Exit;
    end
    else
    begin
      // Various Single-character tokens
      case C of
        '+': Token := toAdd;
        '-': Token := toSub;
        '*': Token := toMul;
        '/': Token := toDiv;
        '=': Token := toEq;
        '<': Token := toLt;
        '>': Token := toGt;
        '(': Token := toLParen;
        ')': Token := toRParen;
        '[': Token := toLBrack;
        ']': Token := toRBrack;
        ':': Token := toColon;
        ',': Token := toComma;
        ';': Token := toSemicolon;
        '.': Token := toPeriod;
        '^': Token := toCaret;
        else Error('Invalid character "' + C + '"');
      end;

      C := GetChar;

      // These might turn out to be multi-character tokens
      case Token of
        toDiv:
          if C = '/' then               // C-style one-line comment
          begin
            Source^.Column := Length(Source^.Buffer) + 1;
            C := GetChar;
            NextToken; (* TODO Eliminate recursion??? *)
          end;
        toLt:
          if C = '>' then               // Not equal
          begin
            Token := toNeq;
            C := GetChar;
          end
          else if C = '=' then          // Less than or equal
          begin
            Token := toLeq;
            C := GetChar;
          end;

        toGt:
          if C = '=' then               // Greater than or equal
          begin
            Token := toGeq;
            C := GetChar;
          end;

        toLParen:
          if C = '.' then               // Alternative notation for [
          begin
            Token := toLBrack;
            C := GetChar;
          end
          else if C = '*' then          // Alternative notation for comments
          begin
            C := GetChar;
            S := '(*' + C;
            repeat
              while C <> '*' do
              begin
                C := GetChar;
                S := S + C;
              end;
              C := GetChar;
              S := S + C;
            until C = ')';
            C := GetChar;

            // TODO Do we really need to support directives for both comment types?
            if LowerStr(Copy(S, 3, 2)) = '$i' then
              OpenInput(NativeToPosix(TrimStr(Copy(S, 5, Length(S) - 6))));

            NextToken;
            Exit;
          end;

        toColon:
          if C = '=' then               // Assignment operator
          begin
            Token := toBecomes;
            C := GetChar;
          end;

        toPeriod:
          if C = ')' then               // Alternative notation for ]
          begin
            Token := toRBrack;
            C := GetChar;
          end
          else if C = '.' then          // Double dot for ranges
          begin
            Token := toRange;
            C := GetChar;
          end;
      end;
    end;
  end;
end;

(**
 * Returns a control character. Called if the previous character was a '^'.
 *)
function GetCtrlChar: Integer;
begin
  if not (C in ['@'..'_']) then Error('Invalid control character ^' + C);
  GetCtrlChar := Ord(C) - 64;
  C := GetChar;
end;

(**
 * Throws an error if the current (lookahead) token type is not the expected
 * one.
 *)
procedure Expect(Token: TToken);
begin
  if Scanner.Token <> Token then
    Error('Expected "' + TokenStr[Token] + '", but got "' + TokenStr[Scanner.Token] + '"');
end;

(* -------------------------------------------------------------------------- *)
(* --- Error handling ------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

var
  (**
   * The line and column where an error occurred.
   *)
  ErrorLine, ErrorColumn: Integer;

  (**
   * The stored execution state to LongJmp to after an error.
   *)
  StoredState: Jmp_Buf;

(**
   * Whether we have assigned a stored state we can jump to.
   *)
  HasStoredState: Boolean;

(**
 * Reports an error and performs a LongJmp back to the point where parsing was
 * started.
 *)
procedure Error(Message: String);
var
  I: Integer;
begin
  WriteLn;

  if Source <> nil then
  begin
    WriteLn(Source^.Buffer);
    for I := 1 to TokenColumn - 1 do Write(' ');
    WriteLn('^');
    WriteLn('*** Error at ', TokenLine, ',', TokenColumn, ': ', Message);
    ErrorLine := TokenLine;
    ErrorColumn := TokenColumn;
  end
  else
  begin
    WriteLn('*** Error: ', Message);
    ErrorLine := 0;
    ErrorColumn := 0;
  end;

  WriteLn();

  // TODO Get rid of need for LongJmp by making "IDE" a separate program?
  if HasStoredState then
    LongJmp(StoredState, 1)
  else
    Halt(1);
end;

(* -------------------------------------------------------------------------- *)
(* --- Emitter -------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

type
  (**
   * An element of "outgoing code". These are organized in a linked list and
   * subject to optimization. The list will be flushed on each label that is
   * being written and at the end of a procedure or function.
   *
   * TODO Simplify this. Write either a label (which always goes directly into
   * the file) or an instruction. Split instruction into opcode and the two
   * arguments, so the optimizer's life gets easier. Maybe drop comments
   * completely.
   *)
  PCode = ^TCode;
  TCode = record
    Tag, Instruction, Comment: String;  // Label, instruction, and comment
    Next, Prev: PCode;                  // Successor and predecessor

    // TODO Make this Instr, Oper1, Oper2
  end;

var
  (**
   * The text file we are writing assembly code to.
   *)
  Target: Text;

  (**
   * The index of the next label we will generate.
   *)
  NextLabel: Integer;

  (**
   * Reflects whether the optimizer is enabled.
   *
   * TODO Move this into a central place together with other compiler switches.
   *)
  Optimize: Boolean;

  (**
   * Contains the current jump target for the Exit statement.
   *
   * TODO Should this be elsewhere, maybe together with Break/Continue?
   *)
  ExitTarget: String;

  (**
   * The most recent line of assembly code generated.
   *
   * TODO Should we have a sentinel here to make the optimizer code simpler?
   *)
  Code: PCode = nil;

(**
 * Appends a line of assembly code consisting of the three given elements, any
 * of which may be the empty string.
 *)
procedure AppendCode(Tag, Instruction, Comment: String);
var
  Temp: PCode;
begin
  New(Temp);
  Temp^.Tag := Tag;
  Temp^.Instruction := Instruction;
  Temp^.Comment := Comment;
  Temp^.Prev := Code;
  Temp^.Next := nil;
  if Code <> nil then
    Code^.Next := Temp;
  Code := Temp;
end;

(**
 * Removes the most-recently generated line of assembly code.
 *)
procedure RemoveCode();
var
  Temp: PCode;
begin
  Temp := Code;
  Code := Code^.Prev;
  if Code <> nil then
    Code^.Next := nil;
  Dispose(Temp);
end;

(**
 * Opens the target file.
 *)
procedure OpenTarget(Filename: String);
begin
  Assign(Target, Filename);
  Rewrite(Target);
end;

(**
 * Generates a new, unique label with the given prefix.
 *
 * TODO Is this in the right place?
 *)
function GetLabel(Prefix: String): String;
begin
  GetLabel := Prefix + IntToStr(NextLabel);
  NextLabel := NextLabel + 1;
end;

// Emit(Col1, Col2, Col3, Col4)

procedure Emit0(Tag, Instruction, Comment: String);
var
  P: Integer;
  S: String;
begin
  if (Tag = '') and (Instruction = '') and (Comment <> '') then
  begin
    WriteLn(Target, '; ', Comment);
    Exit;
  end;

  S := '';

  if Tag <> '' then S := S + Tag + ':';

  if Instruction <> '' then
  begin
(*
    if Instruction = 'ld de,(hl)' then
      Instruction := 'ld e,(hl) \ inc hl \ ld d,(hl)'
    else if Instruction = 'ld (hl),de' then
      Instruction := 'ld (hl),e \ inc hl \ ld (hl),d'
    else if Instruction = 'ld bc,(hl)' then
      Instruction := 'ld c,(hl) \ inc hl \ ld b,(hl)';
*)

    P := Pos(' ', Instruction);
    if P <> 0 then
      Instruction := PadStr(Copy(Instruction, 1, P-1), 8) + Copy(Instruction, P+1, 255);

    S := PadStr(S, 16) + Instruction;
  end;

  if Comment <> '' then
    S := PadStr(S, 40) + '; ' + Comment;

  WriteLn(Target, S);
end;

procedure Flush;
var
  Temp: PCode;
begin
  if not Optimize or (Code = nil) then Exit;

  while Code^.Prev <> nil do
    Code := Code^.Prev;

  while Code <> nil do
  begin
    if (Code^.Tag <> '') or (Code^.Instruction <> '') or (Code^.Comment <> '') then
      Emit0(Code^.Tag, Code^.Instruction, Code^.Comment);
    Temp := Code;
    Code := Code^.Next;
    Dispose(Temp);
  end;
end;

procedure SetLibrary;
begin
  if not StartsWith(FileName, '/') then
    FileName := ParentDir(Source^.Name) + '/' + FileName;

  Flush;
  Emit0('', 'include "' + FileName + '"', '');
end;

function DoOptimize: Boolean;
var
  Op1, Op2, S: String;
  Prev: PCode;
begin
  (* Try to eliminate the most embarrassing generated instruction pairs. *)

  DoOptimize := True;

  if (Code <> nil) then
  begin
    Prev := Code^.Prev;
    if Prev = nil then
    begin
      DoOptimize := False;
      Exit;
    end;

    if StartsWith(Prev^.Instruction, 'db ') and StartsWith(Code^.Instruction, 'db ') then
    begin
      Prev^.Instruction := Prev^.Instruction + ',' + Copy(Code^.Instruction, 4, 255);
      RemoveCode;
      Exit;
    end;

    if StartsWith(Prev^.Instruction, 'dw ') and StartsWith(Code^.Instruction, 'dw ') then
    begin
      Prev^.Instruction := Prev^.Instruction + ',' + Copy(Code^.Instruction, 4, 255);
      RemoveCode;
      Exit;
    end;

    if StartsWith(Prev^.Instruction, 'push ') and StartsWith(Code^.Instruction, 'pop ') then
    begin
      Op1 := Copy(Prev^.Instruction, 6, 255);
      Op2 := Copy(Code^.Instruction, 5, 255);

      if Op1 = Op2 then
      begin
        RemoveCode;
        RemoveCode;
        Exit;
      end;

      if (Op1 <> 'af') and (Op2 <> 'af') then
      begin
        if (Op1 = 'de') and (Op2 = 'hl') or (Op1 = 'hl') and (Op2 = 'de') then
          Prev^.Instruction := 'ex de,hl'
        else
          Prev^.Instruction := 'ld ' + Op2 + ',' + Op1;
        RemoveCode;
        Exit;
      end
      // else if (Op1 = 'hl') and (Op2 = 'af') then
      // begin
      //   RemoveCode;
      //   RemoveCode;
      //   Code^.Instruction := 'ccf';
      //   Code^.Prev.Instruction := 'dec a';
      //   Exit;
      // end;
    end;

    if (Prev^.Instruction = 'pushfp') and (Code^.Instruction = 'popfp') then
    begin
      RemoveCode;
      RemoveCode;
      Exit;
    end;

    if (Code^.Instruction = 'ex de,hl') and (Prev^.Instruction = 'ex de,hl') then
    begin
      RemoveCode;
      RemoveCode;
      Exit;
    end;

    if (Prev^.Instruction = 'ld l,a') and (Code^.Instruction = 'bit 0,l') then
    begin
      if (Prev^.Prev <> nil) and (Prev^.Prev^.Instruction = 'ld h,0') then
      begin
        RemoveCode;
        RemoveCode;
        Code^.Instruction := 'and a';
        Exit;
      end;
    end;

    if (Code^.Instruction = '') and (Code^.Comment <> '') and StartsWith(Prev^.Instruction, 'push') then
    begin
      Code^.Instruction := Prev^.Instruction;
      Prev^.Instruction := '';

      Prev^.Comment := Code^.Comment;
      Code^.Comment := '';
      Exit;
    end;

    if (Code^.Instruction = 'ld bc,de') and StartsWith(Prev^.Instruction, 'ld de,') then
    begin
      RemoveCode;
      Code^.Instruction := 'ld bc,' + Copy(Code^.Instruction, 7, 255);
      Exit;
    end;

    if (Code^.Instruction = 'ld bc,hl') and (Prev^.Instruction = 'ld l,a') then
    begin
      if (Prev^.Prev <> nil) and (Prev^.Prev^.Instruction = 'ld h,0') then
      begin
        RemoveCode;
        RemoveCode;
        Code^.Instruction := 'ld c,a';
        Exit;
      end;
    end;

    if (Code^.Instruction = 'add hl,de') and (Prev^.Instruction = 'ld de,2') then
    begin
      Prev^.Instruction := 'inc hl';
      Code^.Instruction := 'inc hl';
      Exit;
    end;

    if (Code^.Instruction = 'add hl,de') and (Prev^.Instruction = 'ld de,1') then
    begin
      Prev^.Instruction := 'inc hl';
      RemoveCode;
      Exit;
    end;

    if (Code^.Instruction = 'add hl,de') and (Prev^.Instruction = 'ld de,0') then
    begin
      RemoveCode;
      RemoveCode;
      Exit;
    end;

    if (Code^.Instruction = 'add hl,de') and (Prev^.Instruction = 'ld de,-1') then
    begin
      Prev^.Instruction := 'dec hl';
      RemoveCode;
      Exit;
    end;

    if (Code^.Instruction = 'add hl,de') and (Prev^.Instruction = 'ld de,-2') then
    begin
      Prev^.Instruction := 'dec hl';
      Code^.Instruction := 'dec hl';
      Exit;
    end;

    (* --- for later ---
    if (Code^.Instruction = 'ld (hl),de') and StartsWith(Prev^.Instruction, 'ld de,') and (StartsWith(Prev^.Prev^.Instruction, 'ld hl,global')) then
    begin

      Addr := Copy(Prev^.Prev^.Instruction, 7, 255);

      Prev^.Prev^.Instruction := '';
      Code^.Instruction := 'ld (' + Addr + '),de';
      WriteLn('--> ', Code^.Instruction);

      Exit;
    end;
    *)

    if (Code^.Instruction = 'ld a,e') and StartsWith(Prev^.Instruction, 'ld de,') then
    begin
      RemoveCode;
      Code^.Instruction := 'ld a,' + Copy(Code^.Instruction, 7, 255);
      Exit;
    end;

    if (Code^.Instruction = 'ex de,hl') and StartsWith(Prev^.Instruction, 'ld de,') then
    begin
      if IsDecDigit(Prev^.Instruction[7]) then
      begin
        Prev^.Instruction := 'ld hl,' + Copy(Prev^.Instruction, 7, 255);
        RemoveCode;
        Exit;
      end;
    end;

    if (Code^.Instruction = 'ld de,(hl)') and StartsWith(Prev^.Instruction, 'ld hl,') then
    begin
      Prev^.Instruction := 'ld hl,(' + Copy(Prev^.Instruction, 7, 255) + ')';
      Code^.Instruction := 'ex de,hl';
      Exit;
    end;

    if (Code^.Instruction = 'pop hl') and StartsWith(Prev^.Instruction, 'ld de,') then
    begin
      if (Prev^.Prev <> nil) and (Prev^.Prev^.Instruction = 'push hl') then
      begin
        Prev^.Prev^.Instruction := Prev^.Instruction;
        Prev^.Instruction := 'push hl';
        Exit;
      end;
    end;

    if (Code^.Instruction = 'push de') and (Prev^.Instruction = 'ex de,hl') then
    begin
      RemoveCode;
      Code^.Instruction := 'push hl';
      Exit;
    end;

    if (Code^.Instruction = 'pop de') and StartsWith(Prev^.Instruction, 'ld hl,') and (Prev^.Prev^.Instruction = 'push de') then
    begin
      Prev^.Prev^.Instruction := Prev^.Instruction;
      RemoveCode;
      RemoveCode;
      Exit;
    end;

    if (Code^.Instruction = 'pop hl') and StartsWith(Prev^.Instruction, 'ld de,') then
    begin
      if (Prev^.Prev <> nil) and (Prev^.Prev^.Instruction = 'push de') then
      begin
        Prev^.Prev^.Instruction := 'ex de,hl';
        RemoveCode;

        if Prev^.Prev^.Prev <> nil then
        begin
          S := Prev^.Prev^.Prev^.Instruction;

          if S = 'ex de,hl' then
          begin
            Prev^.Prev^.Prev^.Instruction := '';
            Prev^.Prev^.Instruction := '';
          end
          else if StartsWith(S, 'ld de,') and IsDecDigit(S[7]) then
          begin
            Prev^.Prev^.Prev^.Instruction := 'ld hl,' + Copy(S, 7, 255);
            Prev^.Prev^.Instruction := '';
          end;
        end;

        Exit;
      end;
    end;

    if (Code^.Instruction = 'call __int16_eq') and (Prev^.Instruction = 'ld de,0') then
    begin
      Prev^.Instruction := 'call __int16_eq0';
      RemoveCode;
      Exit;
    end;

    if (Code^.Instruction = 'call __int16_neq') and (Prev^.Instruction = 'ld de,0') then
    begin
      Prev^.Instruction := 'call __int16_neq0';
      RemoveCode;
      Exit;
    end;

    if (Code^.Instruction = 'call __int_shl') and (Prev^.Instruction = 'ld de,8') then
    begin
      Prev^.Instruction := 'ld h,l';
      Code^.Instruction := 'ld l,0';
      Exit;
    end;

    if (Code^.Instruction = 'call __int_shr') and (Prev^.Instruction = 'ld de,8') then
    begin
      Prev^.Instruction := 'ld l,h';
      Code^.Instruction := 'ld h,0';
      Exit;
    end;

  end;

  DoOptimize := False;
end;

procedure Emit(Tag, Instruction, Comment: String);
begin
  if not Optimize then
  begin
    Emit0(Tag, Instruction, Comment);
    Exit;
  end;

  if Tag <> '' then
    Flush;

  AppendCode(Tag, Instruction, Comment);

  while DoOptimize do
  begin
  end;
end;

procedure EmitI(S: String);
begin
  Emit('', S, '');
end;

procedure EmitC(S: String);
begin
  Emit('', '', S)
  //Flush;
  //WriteLn(Target, '; ', S);
end;

procedure EmitInclude(S: String);
begin
  EmitI('include "' +  S + '"');
end;

procedure EmitClear(Bytes: Integer); forward;

procedure EmitCall(Sym: PSymbol);
var
  I, J, K: Integer;
begin
  with Sym^ do
    if SmartLink and (Level = 0) and IsStdCall and not IsExternal then
      AddDependency(CurrentBlock, Sym);

  if not Sym^.IsStdCall then
  begin
    (* This is a bit broken. Checks should probably be done when parsing the
       signatures. Also, what happens if we don't have enough (or the right)
       registers for all parameters? *)
    if (Sym^.Value = 1) and (Sym^.ArgTypes[0] = dtReal) then
      EmitI('popfp')
    else
    begin
      if Sym^.Value >= 3 then
        EmitI('pop bc');
      if Sym^.Value >= 2 then
        EmitI('pop de');
      if Sym^.Value >= 1 then
        EmitI('pop hl');
    end;
  end;

  EmitI('call ' + Sym^.Tag);

  if not Sym^.IsStdCall then
  begin
    if Sym^.Kind = scFunc then
    begin
      if Sym^.DataType = dtReal then
        EmitI('pushfp')
      else
        EmitI('push hl');
    end;
  end
  else
  begin
    K := 0;
    for I := 0 to Sym^.Value - 1 do
    begin
      if Sym^.ArgIsRef[I] then J := 2 else
      begin
        J := Sym^.ArgTypes[I]^.Value;
        if J < 2 then J := 2;
      end;
      K:= K + J;
    end;

    if K <> 0 then
    begin
      EmitC('Post call cleanup ' + IntToStr(K) + ' bytes');
      EmitClear(K);
    end;
  end;
end;

(**
 * Emits the file header.
 *)
procedure EmitHeader(Home: String; SrcFile: String);
begin
  EmitC('');
  EmitC('program ' + SrcFile);
  EmitC('');
  if Binary = btCPM then
  begin
    Emit('CPM', 'equ 1', 'Target is CP/M');
    EmitI('device NOSLOT64K');
  end
  else if Binary = btZX then
  begin
    Emit('ZX', 'equ 1', 'Target is ZX 48K');
    EmitI('device ZXSPECTRUM48');
  end
  else if Binary = btZX128 then
  begin
    Emit('ZX', 'equ 1', 'Target is ZX 128K');
    EmitI('device ZXSPECTRUM128');
  end
  else if Binary = btZXN then
  begin
    Emit('NXT', 'equ 1', 'Target is Next .dot file');
    EmitI('device ZXSPECTRUMNEXT');
  end;
end;

(**
 * Emits the file footer.
 *)
procedure EmitFooter(BinFile: String);
begin
  EmitC('');
  Emit('HEAP', '', '');
  EmitC('');
  EmitC('end');
  EmitC('');

  if Binary = btCPM then
    EmitI('savebin "' + BinFile + '",$0100,HEAP-$0100')
  else if Format = tfBinary then
    EmitI('savebin "' + BinFile + '",$8000,HEAP-$8000')
  else if Format = tfPlus3Dos then
    EmitI('save3dos "' + BinFile + '",$8000,HEAP-$8000,3,$8000')
  else if Format = tfTape then
    EmitI('savetap "' + BinFile + '",CODE,"mcode",$8000,HEAP-$8000')
  else if Format = tfSnapshot then
    EmitI('savesna "' + BinFile + '",$8000');
end;

(**
 * Collects (textual) info about all variables from the given symbol to the most
 * recent scope marker, so they can be added to the generated assembly.
 *)
procedure CollectVars(Sym: PSymbol; var S: String);
begin
  if Sym^.Kind <> scScope then CollectVars(Sym^.Prev, S);

  if Sym^.Kind = scVar then
  begin
    if S <> '' then S := S + ', ' + Sym^.Name else S := Sym^.Name;
    S := S + '(';
    if Sym^.Tag <> '' then
      S := S + '@' + Sym^.Tag + ')'
    else
    begin
    if Sym^.Value > 0 then S := S + '+';
    S := S + IntToStr(Sym^.Value) + ')';
    end;
  end;
end;

(**
 * Encodes the given Pascal string so that the assembler will accept it in a db
 * statement. Mostly escaping of problematic characters.
 *)
function EncodeAsmStr(const S: String): String;
var
  T: String;
  Quotes: Boolean;
  I: Integer;
  C: Char;
begin
  T := IntToStr(Length(S));
  Quotes := False;

  for I := 1 to Length(S) do
  begin
    C := S[I];
    if (C < ' ') or (C > '~') or (C = '"') or (C = '´') or (C = '\') then
    begin
      if Quotes then
      begin
        T := T + '"';
        Quotes := False;
      end;
      T := T + ',' + IntToStr(Ord(C));
    end
    else
    begin
      if not Quotes then
      begin
        T := T + ',"';
        Quotes := True;
      end;
      T := T + C;
    end;
  end;

  if Quotes then T := T + '"';

  EncodeAsmStr := T;
end;

(**
 * Emits all strings collected in the string list as db items. Called once after
 * the whole program code has been processed.
 *)
procedure EmitStrings();
var
  Temp: PStringLiteral;
begin
  Temp := Strings;

  while Temp <> nil do
  begin
    EmitC('');
    Emit(Temp^.Tag, 'db ' + EncodeAsmStr(Temp^.Value), '');
    Temp := Temp^.Next;
  end;
end;

procedure EmitSpace(Bytes: Integer); forward;

(*
 * Emits the prologue for a procedure or function that uses the standard
 * calling convention. The stack frame looks like this:
 *
 * +------------------+
 * | result (if any)  |
 * | 1st argument     |
 * | ...              |
 * | nth argument     |
 * | return addr      |
 * | old base pointer | <-- frame pointer
 * | 1st local        |
 * | ...              |
 * | nth local        | <-- stack pointer
 * +------------------+
 *
 * All access to local variables that are stored on the stack happens via the
 * frame pointer. The frame pointer is kept in the display entry matching the
 * nesting level of the procedure of function. It is not kept in a register
 * pair. The old frame pointer for that nesting level is stored on the stack.
 * Arguments are cleanup up by the caller.
 *)
procedure EmitPrologue(Sym: PSymbol);
var
  V: String;
begin
  V := '';
  CollectVars(SymbolTable, V);
  if V <> '' then
  begin
    EmitC('var ' + V);
    EmitC('');
  end;

  if Sym = Nil then
  begin
    Emit('main', '', '');
    EmitI('ld (display),sp');
    EmitCall(LookupBuiltInOrFail('InitHeap'));
  end
  else
  begin
    Emit(Sym^.Tag, '', 'Prologue');
    EmitI('ld hl,(display+' + IntToStr(Level * 2) + ')');
    EmitI('push hl');

    EmitI('ld (display+' + IntToStr(Level * 2) + '),sp');

    EmitSpace(-Offset);

    if StackMode then
      EmitCall(LookupBuiltInOrFail('CheckStack'));
  end;
end;

(**
 * Emits the epilogue to a procedure or function that uses standard calling
 * convention. Restores the old frame pointer.
 *)
procedure EmitEpilogue(Sym: PSymbol);
begin
  if Sym <> nil then
  begin
    Emit(ExitTarget, 'ld sp,(display+' + IntToStr(Level * 2) + ')', 'Epilogue');

    EmitI('pop hl');
    EmitI('ld (display+' + IntToStr(Level * 2) + '),hl');
  end;

  EmitI('ret');
end;

(**
 * Emits code that puts the address of the given symbol on the stack. Takes into
 * account the addressing mode (global vs. local), indirection and a possible
 * offset.
 *)
procedure EmitAddress(Sym: PSymbol);
begin
  with Sym^ do
  begin
    if (Tag <> '') and (Tag <> 'RESULT') then (* TODO: This is ugly. *)
    begin
      Emit('', 'ld hl,' + Tag, 'Global ' + Name);
      EmitI('push hl');
    end
    else
    begin
      // TODO: Make this a macro to ease optimization?
      Emit('', 'ld hl,(display+' + IntToStr(Level * 2) + ')', 'Local ' + Name);
      if Value <> 0 then
      begin
        Emit('', 'ld de,' + IntToStr(Value), '');
        Emit('', 'add hl,de', '');
      end;
      Emit('', 'push hl', '');
    end;

    if IsRef then
    begin
      // TODO: Make this a macro to ease optimization?
      Emit('', 'pop hl', '');
      Emit('', 'ld e,(hl)', '');
      Emit('', 'inc hl', '');
      Emit('', 'ld d,(hl)', '');
      Emit('', 'push de', '');
    end;

    if Value2 <> 0 then
    begin
      EmitI('pop hl');
      if Value2 <> 0 then
      begin
        EmitI('ld de,' + IntToStr(Value2));
        EmitI('add hl,de');
      end;
      EmitI('push hl');
    end;
  end;
end;

(**
 * Emits code that pops an address off the stack and pushes data located at
 * that address on the stack. Takes into accout the given datatype (mostly
 * for the size)
 *)
procedure EmitLoad(DataType: PSymbol);
begin
  if DataType^.Kind = scStringType then
  begin
    EmitI('pop hl');
    EmitI('call __loadstr');
    Exit;
  end;

  if DataType = dtReal then
  begin
    EmitI('pop hl');
    EmitI('call __loadfp');
    EmitI('pushfp');
    Exit;
  end;

  case DataType^.Value of
    1: begin
          EmitI('pop hl');
          EmitI('ld d,0');
          EmitI('ld e,(hl)');
          EmitI('push de');
        end;
    2: begin
          EmitI('pop hl');
          EmitI('ld de,(hl)');
          EmitI('push de');
       end;
    else
    begin
      Emit('', 'pop hl', 'Load');
      EmitI('ld bc,' + IntToStr(DataType^.Value));
      EmitI('call __load16');
    end
  end;
end;

(**
 * Emits code that pops an address off the stack and pushes data located at
 * that address on the stack. Takes into accout the given datatype (mostly
 * for the size)
 *)procedure EmitStore(DataType: PSymbol);
begin
  if DataType^.Kind = scStringType then
  begin
    if Optimize and (Code <> nil) then
    begin
      if Code^.Instruction = 'call __loadstr' then
      begin
        RemoveCode;
        EmitI('pop de');
        EmitI('ld a,' + IntToStr(DataType^.Value - 1));
        EmitI('call __movestr');
        Exit;
      end;
    end;

    EmitI('ld a,' + IntToStr(DataType^.Value - 1));
    EmitI('call __storestr');
    Exit;
  end;

  if DataType = dtReal then
  begin
    EmitI('popfp');
    EmitI('exx');
    EmitI('pop hl');
    EmitI('call __storefp');
    Exit;
  end;


  case DataType^.Value of
    1: begin
          EmitI('pop de');
          EmitI('pop hl');
          EmitI('ld (hl),e');
        end;
    2: begin
          EmitI('pop de');
          EmitI('pop hl');
          EmitI('ld (hl),de');
        end;
    else
    begin
      if Optimize and (Code^.Instruction = 'call __load16') then
      begin
        RemoveCode;
        EmitI('pop de');
        EmitI('ldir');
      end
      else
      begin
        EmitI('ld bc,' + IntToStr(DataType^.Value));
        EmitI('call __store16');
      end;
    end;
  end;
end;

procedure EmitLiteral(Value: Integer);
begin
  Emit('', 'ld de,' + IntToStr(Value), 'Literal ' + IntToStr(Value));
  EmitI('push de');
end;

(**
 * Emits code that grows the stack by the given number of bytes.
 *)
procedure EmitSpace(Bytes: Integer);
begin
  if Bytes <= 10 then
  begin
    while Bytes > 1 do
    begin
      EmitI('push hl');
      Bytes := Bytes - 2;
    end;
    if Bytes > 0 then EmitI('dec sp');
  end
  else if Bytes = 256 then
    EmitI('call __mkstr')
  else
  begin
    Emit('', 'ld hl,-' + IntToStr(Bytes), 'Space');
    EmitI('add hl,sp');
    EmitI('ld sp,hl');
  end;
end;

(**
 * Emits code that shrinks the stack by the given number of bytes.
 *)
procedure EmitClear(Bytes: Integer);
begin
  if Bytes <= 10 then
  begin
    while Bytes > 1 do
    begin
      EmitI('pop hl');
      Bytes := Bytes - 2;
    end;
    if Bytes > 0 then EmitI('inc sp');
  end
  else if Bytes = 256 then
    EmitI('call __rmstr')
  else
  begin
    Emit('', 'ld hl,' + IntToStr(Bytes), 'Clear');
    EmitI('add hl,sp');
    EmitI('ld sp,hl');
  end;
end;

(**
 * Emits code implementing the given relational operator for 16 bit integers.
 *)
procedure EmitRelOp(Op: TToken);
begin
  if (Op = toGt) or (Op = toLeq) then
  begin
    Emit('','pop hl', 'RelOp ' + IntToStr(Ord(Op)));
    EmitI('pop de');
  end
  else
  begin
    Emit('','pop de', 'RelOp ' + IntToStr(Ord(Op)));
    EmitI('pop hl');
  end;

  case Op of
    toEq:         EmitI('call __int16_eq');
    toNeq:        EmitI('call __int16_neq');
    toLt, toGt:   EmitI('call __int16_lt');
    toLeq, toGeq: EmitI('call __int16_geq');
  end;

  Emit('', 'ld h,0', '');
  Emit('', 'ld l,a', '');
  Emit('', 'push hl', '');
end;

procedure EmitUnOp(Op: TToken; DataType: PSymbol); forward;

(**
 * Emits code implementing the given relational operator for strings.
 *)
procedure EmitStrOp(Op: TToken);
var
  Invert: Boolean;
begin
  Invert := (Op = toNeq) or (Op = toGt) or (Op = toGeq);

  case Op of
    toEq, toNeq: EmitI('call __streq');
    toLt, toGeq: EmitI('call __strlt');
    toGt, toLeq: EmitI('call __strleq');
  end;

  EmitClear(512);

  EmitI('push de');
  if Invert then EmitUnOp(toNot, dtBoolean);
end;

procedure EmitNeg(DataType: PSymbol); forward;

(**
 * Emits the given set operation.
 *)
procedure EmitSetOp(Op: TToken);
var
  Invert: Boolean;
begin
  Invert := Op = toNeq;

  if Op = toIn then
  begin
    EmitI('call __setmember');
    EmitClear(34);
    EmitI('push de');
  end
  else
  begin
    case Op of
      toAdd: EmitI('call __setadd');
      toSub: EmitI('call __setsub');
      toMul: EmitI('call __setmul');

      toEq, toNeq: EmitI('call __seteq');
      toLeq: EmitI('call __setleq');
      toGeq: EmitI('call __setgeq');
    end;

    if Op in [toAdd, toSub, toMul] then
      EmitClear(32)
    else
    begin
      EmitClear(64);
      EmitI('push de');
    end;

    if Invert then EmitUnOp(toNot, dtBoolean);
  end;
end;

(**
 * Emits code implementing the given mathematic operation for floats.
 *)
procedure EmitFloatOp(Op: TToken);
var
  Invert: Boolean;
begin
  EmitI('popfp');
  EmitI('exx');
  EmitI('popfp');

  Invert := (Op = toNeq) or (Op = toGt) or (Op = toGeq);

  case Op of
    toAdd: EmitI('call FPADD');
    toSub: EmitI('call FPSUB');
    toMul: EmitI('call FPMUL');
    toDiv: EmitI('call FPDIV');
    toMod: EmitI('call MOD');

    toEq, toNeq: EmitI('call __flteq');
    toLt, toGeq: EmitI('call __fltlt');
    toGt, toLeq: EmitI('call __fltleq');
  end;

  if Op in [toAdd, toSub, toMul, toDiv, toMod] then
  begin
    EmitI('pushfp');
  end
  else EmitI('push de');

  if Invert then EmitUnOp(toNot, dtBoolean);
end;

(**
 * Emits code that performs an unconditional jump to the given target.
 *)
procedure EmitJump(Target: String);
begin
    EmitI('jp ' + Target);
end;

(**
 * Emits code that performs a conditional jump to the given target.
 *)
procedure EmitJumpIf(When: Boolean; Target: String);
begin
  EmitI('pop hl');
  EmitI('bit 0,l');
  if When then
    EmitI('jp nz,' + Target)
  else
    EmitI('jp z,' + Target);
end;

(**
 * Emits code the performs the given math or bit binary operation on integers.
 * Data type is also passed as a parameter because some 8 bit cases can be
 * implemented by shorter code.
 *)
procedure EmitBinOp(Op: TToken; DataType: PSymbol);
begin
  Emit('', 'pop de', '');
  Emit('', 'pop hl', '');

  case Op of
    toAdd: EmitI('add hl,de');
    toSub: begin
             EmitI('xor a');
             EmitI('sbc hl,de');
           end;
    toMul: Emit('', 'call __mul16', 'Mul');
    toDivKW: Emit('', 'call __sdiv16', 'Div');
    toMod: begin Emit('', 'call __sdiv16', 'Mod'); EmitI('ex hl,de'); end;
    toAnd: begin
             if DataType = dtInteger then
             begin
               EmitI('ld a,h'); EmitI('and d'); EmitI('ld h,a');
             end;
             EmitI('ld a,l'); EmitI('and e'); EmitI('ld l,a');
           end;
    toOr: begin
             if DataType = dtInteger then
             begin
               EmitI('ld a,h'); EmitI('or d'); EmitI('ld h,a');
             end;
             EmitI('ld a,l'); EmitI('or e'); EmitI('ld l,a');
           end;
    toXor: begin
             if DataType = dtInteger then
             begin
               EmitI('ld a,h'); EmitI('xor d'); EmitI('ld h,a');
            end;
             EmitI('ld a,l'); EmitI('xor e'); EmitI('ld l,a');
           end;
    toShl: begin
              EmitI('call __int_shl');
           end;
    toShr: begin
              EmitI('call __int_shr');
           end;
    end;
  Emit('', 'push hl', '');
end;

(**
 * Emits code the performs the given math or bit unary operation on integers.
 * Data type is also passed as a parameter because some 8 bit cases can be
 * implemented by shorter code.
 *)
procedure EmitUnOp(Op: TToken; DataType: PSymbol);
begin
  case Op of
    toNot:
      begin
        Emit('', 'pop hl', 'Not');
        if DataType = dtBoolean then
        begin
          EmitI('ld a,1');
          EmitI('xor l');
          EmitI('ld l,a');
        end
        else
        begin
          if DataType = dtInteger then
          begin
            EmitI('ld a,h');
            EmitI('cpl');
            EmitI('ld h,a');
          end;
          EmitI('ld a,l');
          EmitI('cpl');
          EmitI('ld l,a');
        end;
        EmitI('push hl');
      end;
  end;
end;

(**
 * Emits code that implements negation for the data type passed as a parameter.
 *)
procedure EmitNeg(DataType: PSymbol);
begin
  if (DataType = dtInteger) or (DataType = dtByte) then
  begin
    EmitI('pop de');
    EmitI('and a');
    EmitI('sbc hl,hl');
    EmitI('sbc hl,de');
    EmitI('push hl');
  end
  else if DataType = dtReal then
  begin
    EmitI('popfp');

    EmitI('ld a,b');
    EmitI('xor 128');
    EmitI('ld b,a');

    EmitI('pushfp');
  end
  else Error('Invalid type ' + DataType^.Name);
end;

(**
 * Emits code that performs increment-by-one for the given data type (basically
 * 8 or 16 bit integer).
 *)
procedure EmitInc(DataType: PSymbol);
begin
  EmitI('pop hl');

  if DataType = dtInteger then
    EmitI('call __inc16')
  else
    EmitI('inc (hl)');
end;

(**
 * Emits code that performs decrement-by-one for the given data type (basically
 * 8 or 16 bit integer).
 *)
procedure EmitDec(DataType: PSymbol);
begin
  EmitI('pop hl');

  if DataType = dtInteger then
    EmitI('call __dec16')
  else
    EmitI('dec (hl)');
end;

(**
 * Emits code that performs shift-left-by-one for the given data type (basically
 * 8 or 16 bit integer).
 *)
procedure EmitShl;
begin
  EmitI('pop hl');
  EmitI('add hl,hl');
  EmitI('push hl');
end;

(**
 * Emits the Str(V, S) statement with zero format specifiers. Type of value
 * and string variable are given as parameters. All values are on the stack.
 *)
procedure EmitStr(DataType, VarType: PSymbol);
begin
  while DataType^.Kind = scSubrangeType do
    DataType := DataType^.DataType;

  EmitI('ld a,' + IntToStr(VarType^.Value - 1));

  if (DataType = dtInteger) or (DataType = dtByte) then
  begin
    EmitI('pop de');
    EmitI('pop hl');
    EmitI('call __strn');
  end
  else if DataType = dtChar then
  begin
    EmitI('pop de');
    EmitI('pop hl');
    EmitI('call __strc');
  end
  else if DataType^.Kind = scStringType then
  begin
    EmitI('pop de');
    { String on stack }
    EmitI('call __strs');
  end
  else if DataType = dtReal then
  begin
    EmitI('pop de');
    EmitI('exx');
    EmitI('popfp');
    EmitI('call __strf');
  end
  else if DataType^.Kind = scEnumType then
  begin
    EmitI('pop de');
    EmitI('pop bc');
    EmitI('ld hl,' + DataType^.Tag);
    EmitI('call __stre');
  end
  else Error('Unprintable type: ' + DataType^.Name);
end;

(**
 * Emits the Str(V, S) statement with one format specifier. Type of value
 * and string variable are given as parameters. All values are on the stack.
 *)
procedure EmitStr1(DataType, VarType: PSymbol);
begin
  while DataType^.Kind = scSubrangeType do
    DataType := DataType^.DataType;

  EmitI('ld a,' + IntToStr(VarType^.Value - 1));

  if (DataType = dtInteger) or (DataType = dtByte) then
  begin
    EmitI('pop de');
    EmitI('pop bc');
    EmitI('pop hl');
    EmitI('call __strn_fmt');
  end
  else if DataType = dtChar then
  begin
    EmitI('pop de');
    EmitI('pop hl');
    EmitI('call __strc');
  end
  else if DataType^.Kind = scStringType then
  begin
    EmitI('pop de');
    { String on stack }
    EmitI('call __strs');
  end
  else if DataType = dtReal then
  begin
    EmitI('pop de'); (* addr *)
    EmitI('pop bc'); (* width *)
    EmitI('exx');
    EmitI('popfp');
    EmitI('call __strf_exp');
  end
  else if DataType^.Kind = scEnumType then
  begin
    EmitI('pop de');
    EmitI('pop bc');
    EmitI('ld hl,' + DataType^.Tag);
    EmitI('call __stre');
  end
  else Error('Unprintable type: ' + DataType^.Name);
end;

(**
 * Emits the Str(V, S) statement with two format specifiers. Type of value
 * and string variable are given as parameters. All values are on the stack.
 *)
procedure EmitStr2(DataType, VarType: PSymbol);
begin
  EmitI('ld a,' + IntToStr(VarType^.Value - 1));

  if DataType = dtReal then
  begin
    EmitI('pop hl'); (* address *)
    EmitI('pop de'); (* decimals  *)
    EmitI('pop bc'); (* width    *)
    EmitI('exx');
    EmitI('popfp');
    EmitI('call __strf_fix');
  end
  else Error('Unprintable type: ' + DataType^.Name);
end;

(**
 * Emits the console Write statement with zero format specifiers. The type of
 * the value is given as parameter. The value resides on the stack.
 *
 * TODO Merge with or leverage code from EmitStr?
 *)
procedure EmitWrite(DataType: PSymbol);
begin
  while DataType^.Kind = scSubrangeType do
    DataType := DataType^.DataType;

  if (DataType = dtInteger) or (DataType = dtByte) then
  begin
    Emit('', 'pop hl', '');
    EmitI('call __putn');
  end
  else if DataType = dtChar then
  begin
    Emit('', 'pop de', '');
    EmitI('ld a,e');
    EmitI('call __putc');
  end
  else if DataType^.Kind = scStringType then
  begin
    if Optimize and (Code <> nil) then
    begin
      if Code^.Instruction = 'call __loadstr' then
      begin
        RemoveCode;
        EmitI('call __puts');
        Exit;
      end;
    end;

    EmitI('ld hl,0');
    EmitI('add hl,sp');
    EmitI('call __puts');
    EmitI('call __rmstr');
  end
  else if DataType = dtReal then
  begin
    EmitI('popfp');
    EmitI('call __putf');
  end
  else if DataType^.Kind = scEnumType then
  begin
    EmitI('pop hl');
    EmitI('ld de,' + DataType^.Tag);
    EmitI('call __pute');
  end
  else Error('Unprintable type: ' + DataType^.Name);
end;

(**
 * Emits the console Write statement with one format specifier. The type of
 * the value is given as parameter. All values reside on the stack.
 *
 * TODO Merge with or leverage code from EmitStr1?
 *)
procedure EmitWrite1(DataType: PSymbol);
begin
  while DataType^.Kind = scSubrangeType do
    DataType := DataType^.DataType;

  Emit('', 'pop bc', '');

  if (DataType = dtInteger) or (DataType = dtByte) then
  begin
    Emit('', 'pop hl', '');
    EmitI('call __putn_fmt');
  end
  else if DataType = dtChar then
  begin
    Emit('', 'pop hl', '');
    EmitI('call __putc_fmt');
  end
  else if DataType^.Kind = scStringType then
  begin
    EmitI('ld hl,0');
    EmitI('add hl,sp');
    EmitI('call __puts_fmt');
    EmitI('call __rmstr');
  end
  else if DataType = dtReal then
  begin
    EmitI('exx');
    EmitI('popfp');
    EmitI('call __putf_exp');
  end
  else if DataType^.Kind = scEnumType then
  begin
    EmitI('pop hl');
    EmitI('ld de,' + DataType^.Tag);
    EmitI('call __pute_fmt');
  end
  else Error('Unprintable type: ' + DataType^.Name);
end;

(**
 * Emits the console Write statement with two format specifiers. The type of
 * the value is given as parameter. All values reside on the stack.
 *
 * TODO Merge with or leverage code from EmitStr2?
 *)
procedure EmitWrite2(DataType: PSymbol);
begin
  if DataType <> dtReal then Error('Unprintable type for format 2: ' + DataType^.Name);

  EmitI('pop bc');
  EmitI('pop de');
  EmitI('exx');
  EmitI('popfp');
  EmitI('call __putf_fix');
end;

(**
 * Flushes and closes the assembly target file.
 *)
procedure CloseTarget();
begin
  Flush;
  Close(Target);
end;

(**
 * Emits a promotion from a single character to a string.
 *)
procedure EmitCharToString;
begin
  EmitI('pop de');
  EmitI('call __char2str');
end;

(* -------------------------------------------------------------------------- *)
(* --- Type checker --------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

type
  (**
   * The types of type check we can perform.
   *
   * TODO Check whether this distinction still makes sense.
   *)
  TTypeCheck = (tcExact, tcAssign, tcExpr);

(**
 * Performs assignment type check. Might promote either type to the other
 * depending on the kind of check.
 *)
function TypeCheck(Left, Right: PSymbol; Check: TTypeCheck): PSymbol;
begin
  if Left = nil then Error('Error in TypeCheck: Left = nil');
  if Right = nil then Error('Error in TypeCheck: Right = nil');

  if Left^.Kind = scSubrangeType then Left := Left^.DataType;
  if Right^.Kind = scSubrangeType then Right := Right^.DataType;

  if Left = Right then
  begin
    TypeCheck := Left;
    Exit;
  end;

  if (Left^.Kind = scPointerType) and (Right^.Kind = scPointerType) then
  begin
    if (Left^.DataType <> nil) and (Right^.DataType <> nil) and (Left^.DataType <> Right^.DataType) then
      Error('Incompatible pointer types');

    TypeCheck := Left;
    Exit;
  end;

  if (Left = dtReal) and ((Right = dtInteger) or (Right = dtByte)) then
  begin
    EmitI('pop hl');
    EmitI('call FLOAT');
    EmitI('pushfp');
    TypeCheck := dtReal;
    Exit;
  end;

  if ((Left = dtInteger) or (Left = dtByte)) and (Right = dtReal) then
  begin
    EmitI('popfp');
    EmitI('exx');
    EmitI('pop hl');
    EmitI('exx');
    EmitI('pushfp');
    EmitI('exx');
    EmitI('call FLOAT');
    EmitI('exx');
    EmitI('popfp');
    EmitI('exx');
    EmitI('pushfp');
    EmitI('exx');
    EmitI('pushfp');
    TypeCheck := dtReal;
    Exit;
  end;

  if Check = tcExpr then
  begin
    if (Left^.Kind = scStringType) and (Right^.Kind = scStringType) then
    begin
      TypeCheck := dtString;
      Exit;
    end;

    if (Left = dtInteger) and (Right = dtByte) or (Left = dtByte) and (Right = dtInteger) then
    begin
      TypeCheck := dtInteger;
      Exit;
    end;
  end
  else if Check = tcAssign then
  begin
    if (Left^.Kind = scStringType) and (Right^.Kind = scStringType) then
    begin
      TypeCheck := Left;
      Exit;
    end;

    if (Left^.Kind = scSetType) and (Right^.Kind = scSetType) then
    begin
(*      if (Right^.DataType <> nil) and (Left^.DataType <> Right^.DataType) then
        Error('Sets not compatible'); *)

      TypeCheck := Left;
      Exit;
    end;

    if (Left = dtInteger) and (Right = dtByte) then
    begin
      TypeCheck := dtInteger;
      Exit;
    end
    else if (Left = dtByte) and (Right = dtInteger) then
    begin
      TypeCheck := dtByte;
      Exit;
    end
  end;

  if (Left^.Kind = scStringType) and (Right = dtChar) then
  begin
    EmitCharToString;
    TypeCheck := dtString;
    Exit;
  end;

  if Check = tcExact then
  begin
    if (Left^.Kind = scStringType) and (Right^.Kind = scStringType) then
    begin
      TypeCheck := dtString;
      Exit;
    end;
  end;

  Error('Type error, expected ' + Left^.Name + ', got ' + Right^.Name);
end;

(* -------------------------------------------------------------------------- *)
(* --- Parser --------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

(**
 * Expects a certain token then consumes it. Shorthand for a very frequent
 * statement sequence. Saves a line of code here and there.
 *)
procedure ParseToken(T: TToken);
begin
  Expect(T);
  NextToken;
end;

function ParseExpression: PSymbol; forward;

(**
 * Parses access to a variable, result in the final address being on the stack.
 * This includes potentially multiple stages of array indexing, record field
 * seletion or following pointers. We try to delay the emitting of the address
 * as long as possible in order to avoid additions at runtime when could let
 * the assembler do them at compile-time.
 *)
function ParseVariableAccess(Symbol: PSymbol): PSymbol;
var
  Variable, DataType: PSymbol;
  Size: Integer;
  Offset: Integer;

  procedure DelayedEmitAddr;
  begin
    if Offset >= 0 then
    begin
      EmitI('ld hl,' + Variable^.Tag + ' + ' + IntToStr(Offset));
      EmitI('push hl');
      Offset := -1;
    end;
  end;

begin
  Variable := Symbol;

  if (Symbol^.Tag <> '') and (Symbol^.Tag <> 'RESULT') then
    Offset := 0
  else
  begin
    EmitAddress(Symbol);
    Offset := -1;
  end;

  DataType := Symbol^.DataType;

  while Scanner.Token in [toLBrack, toPeriod, toCaret] do
  begin
    if Scanner.Token = toLBrack then
    begin
      DelayedEmitAddr;

      repeat
        NextToken;
        if DataType^.Kind = scArrayType then
        begin
          TypeCheck(DataType^.IndexType, ParseExpression(), tcExpr);

          if DataType^.IndexType^.Low <> 0 then
          begin
            EmitLiteral(DataType^.IndexType^.Low);
            EmitBinOp(toSub, dtInteger);
          end;

          DataType := DataType^.DataType;

          Size := DataType^.Value;
          if Size > 1 then
          begin
            case Size of
              2: EmitShl;
              4: begin EmitShl; EmitShl; end;
              else begin
                EmitLiteral(Size);
                EmitBinOp(toMul, dtInteger);
              end;
            end;
          end;
          EmitBinOp(toAdd, dtInteger);
        end
        else if DataType^.Kind = scStringType then
        begin
          TypeCheck(dtInteger, ParseExpression(), tcExpr);

          DataType := dtChar;
          EmitBinOp(toAdd, dtInteger);
        end
        else Error('Not an array or a string');
      until Scanner.Token <> toComma;

      Expect(toRBrack);

      NextToken;
    end
    else if Scanner.Token = toPeriod then
    begin
      if DataType^.Kind <> scRecordType then
        Error('Not a record');

      NextToken;
      Expect(toIdent);
      Symbol := Lookup(Scanner.StrValue, DataType^.DataType, nil);
      if Symbol = nil  then Error('Unknown field "' + Scanner.StrValue + '"');

      DataType := Symbol^.DataType;

      if Offset >= 0 then
        Inc(Offset, Symbol^.Value)
      else
      begin
      if Symbol^.Value <> 0 then
      begin
        EmitLiteral(Symbol^.Value);
        EmitBinOp(toAdd, dtInteger);
        end;
      end;

      NextToken;
    end
    else
    begin
      DelayedEmitAddr;

      if DataType^.Kind <> scPointerType then
        Error('Not a pointer');

      if DataType = dtPointer then
        Error('Cannot deref generic pointer');

      NextToken;

      EmitI('pop hl');
      EmitI('ld e,(hl)');
      EmitI('inc hl');
      EmitI('ld d,(hl)');
      EmitI('push de');

      DataType := DataType^.DataType;
    end;

  end;

  DelayedEmitAddr;

  ParseVariableAccess := DataType;
end;

(**
 * Parses a single actual argument for a procedure or function call.
 *)
procedure ParseArgument(Sym: PSymbol; I: Integer);
var
  Sym2, T: PSymbol;
begin
  if Sym^.ArgIsRef[I] then
  begin
    Expect(toIdent);
    Sym2 := LookupGlobalOrFail(Scanner.StrValue);
    if Sym2^.Kind <> scVar then
      Error('Not a variable: ' + Scanner.StrValue);

    NextToken;

    T := ParseVariableAccess(Sym2);

    if Sym^.ArgTypes[I] <> nil then
      TypeCheck(Sym^.ArgTypes[I], T, tcAssign);
  end
  else
    TypeCheck(Sym^.ArgTypes[I], ParseExpression, tcAssign);
end;

(**
 * Parses the list of actual arguments for a procedure or function call.
 *)
procedure ParseArguments(Sym: PSymbol);
var
  I, J: Integer;
begin
  I := 0;
  J := Sym^.Value;

  if J <> 0 then
  begin
    Expect(toLParen);
    NextToken;

    ParseArgument(Sym, I);
    I := I + 1;

    while I < Sym^.Value do
    begin
      Expect(toComma);
      NextToken;
      ParseArgument(Sym, I);
      I := I + 1;
    end;

    Expect(toRParen);
    NextToken;
  end
  else if Scanner.Token = toLParen then Error('Arguments not allowed here')
end;

(**
 * Parses a reference to a variable.
 *)
function ParseVariableRef: PSymbol;
var
  Sym: PSymbol;
begin
  Expect(toIdent);
  Sym := LookupGlobalOrFail(Scanner.StrValue);
  if Sym^.Kind <> scVar then Error('Variable expected');
  NextToken;
  ParseVariableRef := ParseVariableAccess(Sym);
end;

(**
 * Parses read access to a "magic" built-in variable. Currently only used for
 * Mem[] and Port[].
 *)
function ParseBuiltInVarRead(Sym: PSymbol): PSymbol;
begin
  if (Sym = MemArray) or (Sym = PortArray) then
  begin
    ParseToken(toLBrack);
    TypeCheck(dtInteger, ParseExpression, tcAssign);
    ParseToken(toRBrack);

    if Sym = MemArray then
    begin
      EmitI('pop hl');
      EmitI('ld e,(hl)')
    end
    else
    begin
      EmitI('pop bc');
      EmitI('in e,(c)');
    end;

    EmitI('ld d,0');
    EmitI('push de');
  end
  else Error('Invalid magic var: ' + Sym^.Name);

  ParseBuiltInVarRead := dtByte;
end;

(**
 * Parses write access to a "magic" built-in variable. Currently only used for
 * Mem[] and Port[].
 *)
procedure ParseBuiltInVarWrite(Sym: PSymbol);
begin
  if (Sym = MemArray) or (Sym = PortArray) then
  begin
    ParseToken(toLBrack);
    TypeCheck(dtInteger, ParseExpression, tcAssign);
    ParseToken(toRBrack);
    ParseToken(toBecomes);
    TypeCheck(dtByte, ParseExpression, tcAssign);

    if Sym = MemArray then
    begin
      EmitI('pop de');
      EmitI('pop hl');
      EmitI('ld (hl),e');
    end
    else
    begin
      EmitI('pop hl');
      EmitI('pop bc');
      EmitI('out (c),l');
    end;
  end
  else Error('Invalid magic var: ' + Sym^.Name);
end;

(**
 * Parses a built-in function, which is basically every pre-defined function
 * that does not have a fixed syntax (variable parameters, for instance).
 *
 * TODO Split this into multiple procedures?
 *)
function ParseBuiltInFunction(Func: PSymbol): PSymbol;
var
  T, Sym: PSymbol;
  Old: PCode;
begin
  if Func^.Kind <> scFunc then
    Error('Not a built-in function: ' + Func^.Name);

  NextToken; Expect(toLParen); NextToken;

  if Func = AbsFunc then
  begin
    Sym := ParseExpression;

    if (Sym = dtInteger) or (Sym = dtByte) then
    begin
      EmitI('pop hl');
      EmitI('call __abs16');
      EmitI('push hl');

      ParseBuiltInFunction := dtInteger;
    end
    else if Sym = dtReal then
    begin
      EmitI('popfp');
      EmitI('res 7,b');
      EmitI('pushfp');

      ParseBuiltInFunction := dtReal;
    end
    else Error('Integer or Real expected');
  end
  else if Func = AddrFunc then
  begin
    Expect(toIdent);
    Sym := LookupGlobalOrFail(Scanner.StrValue);
    NextToken;
    ParseVariableAccess(Sym);
    ParseBuiltInFunction := dtInteger;
  end
  else if Func = PtrFunc then
  begin
    TypeCheck(dtInteger, ParseExpression, tcAssign);
    ParseBuiltInFunction := dtPointer;
  end
  else if Func = SizeFunc then
  begin
    Expect(toIdent);
    Sym := LookupGlobalOrFail(Scanner.StrValue);
    NextToken;

    if Sym^.Kind = scVar then
    begin
      Old := Code;
      Sym := ParseVariableAccess(Sym);
      while Code <> Old do RemoveCode;
    end;

    if Sym^.Kind in [scType, scArrayType, scRecordType, scEnumType, scStringType, scFileType] then
      EmitLiteral(Sym^.Value)
    else
      Error('Cannot apply SizeOf to ' + Sym^.Name);

    ParseBuiltInFunction := dtInteger;
  end
  else if Func = OrdFunc then
  begin
    ParseExpression; // TODO TypeCheck for Scalar needed
    ParseBuiltInFunction := dtInteger;
  end
  else if Func = OddFunc then
  begin
    ParseExpression; // TODO TypeCheck for Scalar needed
    Emit('', 'pop hl', 'Odd');
    EmitI('ld a,l');
    EmitI('and 1');
    EmitI('ld l,a');
    EmitI('ld h,0');
    EmitI('push hl');
    ParseBuiltInFunction := dtBoolean;
  end
  else if Func = EvenFunc then
  begin
    ParseExpression; // TODO TypeCheck for Scalar needed
    Emit('', 'pop hl', 'Even');
    EmitI('ld a,l');
    EmitI('and 1');
    EmitI('xor 1');
    EmitI('ld l,a');
    EmitI('ld h,0');
    EmitI('push hl');
    ParseBuiltInFunction := dtBoolean;
  end
  else if Func = PredFunc then
  begin
    ParseBuiltInFunction := ParseExpression; // TODO TypeCheck for Scalar needed
    // This should probably go elsewhere.
    EmitI('pop de');
    EmitI('dec de');
    EmitI('push de');
  end
  else if Func = SuccFunc then
  begin
    ParseBuiltInFunction := ParseExpression; // TODO TypeCheck for Scalar needed
    // This should probably go elsewhere.
    EmitI('pop de');
    EmitI('inc de');
    EmitI('push de');
  end
  else if (Func = BdosFunc) or (Func = BDosHLFunc) then
  begin
    TypeCheck(dtInteger, ParseExpression, tcAssign);
    if Scanner.Token = toComma then
    begin
      NextToken;
      TypeCheck(dtInteger, ParseExpression, tcAssign);
    end
    else EmitLiteral(0);

    EmitI('pop de');
    EmitI('pop hl');
    //EmitI('push ix');
    EmitI('ld c,l');
    EmitI('call 5');
    //EmitI('pop ix');

    if Func = BdosFunc then
    begin
      EmitI('ld l,a');
      EmitI('ld h,0');
    end;

    EmitI('push hl');

    ParseBuiltInFunction := dtByte;
  end
  else if (Func = HighFunc) or (Func = LowFunc) then
  begin
    Expect(toIdent);
    Sym := LookupGlobalOrFail(Scanner.StrValue);
    NextToken;
    if Sym^.Kind = scVar then
    begin
      Old := Code;
      Sym := ParseVariableAccess(Sym);
      while Code <> Old do RemoveCode;
    end;
    if Sym^.Kind = scArrayType then Sym := Sym^.IndexType;
    if Sym^.Kind = scSetType then Sym := Sym^.DataType;
    if Sym = nil then Error('Eeek!');
    if Func = HighFunc then EmitLiteral(Sym^.High) else EmitLiteral(Sym^.Low);

    if Sym^.Kind = scSubrangeType then Sym := Sym^.DataType;

    ParseBuiltInFunction := Sym;
  end
  else if Func = ConcatFunc then
  begin
    TypeCheck(dtString, ParseExpression, tcExact);

    while Scanner.Token = toComma do
    begin
      NextToken;
      TypeCheck(dtString, ParseExpression, tcExact);
      EmitI('call __stradd');
      EmitClear(256);
    end;

    ParseBuiltInFunction := dtString;
  end
  else if (Func = FilePosFunc) or (Func = FileSizeFunc) or (Func = EolFunc) or (Func = EofFunc) then
  begin
    EmitI('push de');

    T := ParseVariableRef;
    if T^.Kind <> scFileType then Error('File type expected');

    if (T = dtFile) or (T^.DataType = dtFile) then
      Sym := LookupBuiltInOrFail('Block' + Func^.Name)
    else if T = dtText then
      Sym := LookupBuiltInOrFail('Text' + Func^.Name)
    else
      Sym := LookupBuiltInOrFail('File' + Func^.Name);

    EmitCall(Sym);
    ParseBuiltInFunction := Sym^.DataType;
  end
  else
    Error('Cannot handle: ' + Func^.Name);

    Expect(toRParen); NextToken;
end;

(**
 * Parses a format specifier (used in Write, WriteLn and Str statements)
 * and puts the results on the stack.
 *)
function ParseFormat: Integer;
begin
  if Scanner.Token = toColon then
  begin
    NextToken;
    TypeCheck(dtInteger, ParseExpression, tcExpr);
    if Scanner.Token = toColon then
    begin
      NextToken;
      TypeCheck(dtInteger, ParseExpression, tcExpr);
      ParseFormat := 2;
    end
    else ParseFormat := 1;
  end
  else ParseFormat := 0;
end;

(**
 * Emits a console read for the given data type.
 *)
procedure EmitReadConsole(T: PSymbol);
begin
  if T = dtInteger then
    EmitI('call __getn')
  else if T = dtChar then
    EmitI('call __getc')
  else if T = dtReal then
    EmitI('call __getr')
  else if T^.Kind = scEnumType then
  begin
    EmitI('ld de,' + T^.Tag);
    EmitI('ld b,' + IntToStr(T^.High + 1));
    EmitI('call __gete');
  end
  else if T^.Kind = scStringType then
  begin
    EmitI('ld a,' + IntToStr(T^.Value - 1));
    EmitI('call __gets');
  end
  else Error('Unreadable type');
end;

(**
 * Parses a built-in procedure, which is basically every pre-defined procedure
 * that does not have a fixed syntax (variable parameters, for instance).
 *
 * TODO Split this into multiple procedures?
 *)
procedure ParseBuiltInProcedure(Proc: PSymbol; BreakTarget, ContTarget: String);
var
  Sym, T, TT, V, U: PSymbol;
  F: Integer;
begin
  if Proc^.Kind <> scProc then
    Error('Not a built-in procedure: ' + Proc^.Name);

  if Proc = AssertProc then
  begin
    NextToken; Expect(toLParen); NextToken;

    TypeCheck(dtBoolean, ParseExpression, tcExact);

    Emit('', 'pop bc', '');
    Emit('', 'ld hl, ' + AddString(Source^.Name), '');
    Emit('', 'ld de, ' + IntToStr(Source^.Line), '');
    Emit('', 'call __assert', '');

    Expect(toRParen); NextToken;
  end
  else if Proc = BreakProc then
  begin
    if BreakTarget = '' then Error('Not in loop');
    Emit('', 'jp ' + BreakTarget, 'Break');
    NextToken;
  end
  else if Proc = ContProc then
  begin
    if ContTarget = '' then Error('Not in loop');
    Emit('', 'jp ' + ContTarget, 'Continue');
    NextToken;
  end
  else if Proc = ExitProc then
  begin
    Emit('', 'jp ' + ExitTarget, 'Exit');
    NextToken;
  end
  else if Proc = HaltProc then
    begin
    NextToken;
    if Scanner.Token = toLParen then
    begin
      NextToken;
      TypeCheck(dtByte, ParseExpression, tcExpr);
      (* TODO Currently ignored. CP/M 2.2 does not support this. *)
      Expect(toRParen);
      NextToken;
    end;

    EmitI('jp __done')
  end
  else if (Proc = IncludeProc) or (Proc = ExcludeProc) then
  begin
    NextToken;
    Expect(toLParen);
    NextToken;

    V := ParseVariableRef;

    if V^.Kind <> scSetType then Error('Set variable expected');

    Expect(toComma);
    NextToken;

    T := ParseExpression;
(*    if T <> V^.DataType^.DataType then Error('Does not match set type'); *)

    Expect(toRParen);
    NextToken;

    EmitI('pop de');
    EmitI('pop hl');

    if Proc = IncludeProc then
      EmitI('call __setinclude')
    else
      EmitI('call __setexclude');
  end
  else if (Proc = ReadProc) or (Proc = ReadLnProc) then
  begin
    NextToken;

    if Scanner.Token = toLParen then
    begin
      NextToken;
      T := ParseVariableRef();

      if T = dtText then
      begin
        EmitI('pop hl');
        EmitI('ld (__cur_file),hl');

        while Scanner.Token = toComma do
        begin
          NextToken;

          EmitI('ld hl,(__cur_file)');
          EmitI('push hl');

          T := ParseVariableRef;

          if T^.Kind = scStringType then
            EmitCall(LookupBuiltInOrFail('TextReadStr'))
          else if T = dtChar then
            EmitCall(LookupBuiltInOrFail('TextReadChar'))
          else if T = dtInteger then
            EmitCall(LookupBuiltInOrFail('TextReadInt'))
          else if T = dtReal then
            EmitCall(LookupBuiltInOrFail('TextReadFloat'))
          else
            Error('Unreadable type');
        end;

        if (Proc = ReadLnProc) and (T^.Kind <> scStringType) then
        begin
          EmitI('ld hl,(__cur_file)');
          EmitI('push hl');
          EmitCall(LookupBuiltInOrFail('TextSeekEoln'));
        end;
      end
      else if T^.Kind = scFileType then
      begin
        EmitI('pop hl');
        EmitI('ld (__cur_file),hl');

        while Scanner.Token = toComma do
        begin
          NextToken;

          EmitI('ld hl,(__cur_file)');
          EmitI('push hl');

          TT := ParseVariableRef;
          if TT <> T^.DataType then Error('Type mismatch');

          EmitCall(LookupBuiltInOrFail('FileRead'));
        end;
      end
      else
      begin
        EmitI('call __getline');

        EmitI('pop hl');
        EmitReadConsole(T);
        while Scanner.Token = toComma do
        begin
          NextToken;
          T := ParseVariableRef();
          EmitI('pop hl');
          EmitReadConsole(T);
        end;
      end;

      Expect(toRParen);
      NextToken;
    end
    else EmitI('call __getline');
  end
  else if (Proc = WriteProc) or (Proc = WriteLnProc) then
  begin
    NextToken;

    if Scanner.Token = toLParen then
    begin
      NextToken;

      T := ParseExpression();

      if T = dtText then
      begin
        EmitI('pop hl');

        EmitI('ld (__cur_file),hl');
        EmitSpace(256);
        EmitI('ld hl,0');
        EmitI('add hl,sp');
        EmitI('ld (__text_buf), hl');

        while Scanner.Token = toComma do
        begin
          NextToken;

          T := ParseExpression;
          F := ParseFormat;

          EmitI('ld hl,(__text_buf)');
          EmitI('push hl');

          V := dtString;

          case F of
            0: EmitStr(T, V);
            1: EmitStr1(T, V); (* Broken? *)
            2: EmitStr2(T, V); (* Broken? *)
          end;

          EmitI('ld hl,(__cur_file)');
          EmitI('push hl');
          EmitI('ld hl,(__text_buf)');
          EmitI('call __loadstr');
          EmitCall(LookupBuiltInOrFail('TextWriteStr'));
        end;

        EmitClear(256);

        if Proc = WriteLnProc then
        begin
          EmitI('ld hl,(__cur_file)');
          EmitI('push hl');
          EmitCall(LookupBuiltInOrFail('TextWriteEoln'));
        end;
      end
      else if T^.Kind = scFileType then
      begin
        EmitI('pop hl');
        EmitI('ld (__cur_file),hl');

        while Scanner.Token = toComma do
        begin
          NextToken;

          EmitI('ld hl,(__cur_file)');
          EmitI('push hl');

          TT := ParseVariableRef;
          if TT <> T^.DataType then Error('Type mismatch');

          EmitCall(LookupBuiltInOrFail('FileWrite'));
        end;

        if IOMode then EmitCall(LookupBuiltInOrFail('BDosThrow'));
      end
      else
      begin
        case ParseFormat of
          0: EmitWrite(T);
          1: EmitWrite1(T);
          2: EmitWrite2(T);
        end;

        while Scanner.Token = toComma do
        begin
          NextToken;
          T := ParseExpression();
          case ParseFormat of
            0: EmitWrite(T);
            1: EmitWrite1(T);
            2: EmitWrite2(T);
          end;
        end;

        if Proc = WriteLnProc then EmitI('call __newline');
      end;

      Expect(toRParen);
      NextToken;
    end
    else if Proc = WriteLnProc then EmitI('call __newline');
  end
  else if Proc = IncProc then
  begin
    NextToken;
    Expect(toLParen);
    NextToken;
    V := ParseVariableRef;
    if V = dtInteger then
    begin
      if Scanner.Token = toComma then
      begin
        NextToken;
        TypeCheck(dtInteger, ParseExpression, tcExpr);
        EmitI('pop bc');
        EmitI('pop hl');
        EmitI('call __inc16by');
      end
      else EmitInc(dtInteger);
    end
    else if (V = dtByte) or (V = dtChar) or (V^.Kind = scEnumType) then
    begin
      if Scanner.Token = toComma then
      begin
        NextToken;
        TypeCheck(dtInteger, ParseExpression, tcExpr);
        EmitI('pop bc');
        EmitI('pop hl');
        EmitI('ld a,(hl)');
        EmitI('add a,c');
        EmitI('ld (hl),a');
      end
      else EmitInc(V);
    end
    else Error('Invalid type, need Integer or Byte');

    Expect(toRParen);
    NextToken;
  end
  else if Proc = DecProc then
  begin
    NextToken;
    Expect(toLParen);
    NextToken;
    V := ParseVariableRef;
    if V = dtInteger then
    begin
      if Scanner.Token = toComma then
      begin
        NextToken;
        TypeCheck(dtInteger, ParseExpression, tcExpr);
        EmitI('pop bc');
        EmitI('pop hl');
        EmitI('call __dec16by');
      end
      else EmitDec(V);
    end
    else if (V = dtByte) or (V = dtChar) or (V^.Kind = scEnumType) then
    begin
      if Scanner.Token = toComma then
      begin
        NextToken;
        TypeCheck(dtInteger, ParseExpression, tcExpr);
        EmitI('pop bc');
        EmitI('pop hl');
        EmitI('ld a,(hl)');
        EmitI('sub a,c');
        EmitI('ld (hl),a');
      end
      else EmitDec(V);
    end
    else Error('Invalid type, need Integer or Byte');

    Expect(toRParen);
    NextToken;
  end
  else if Proc = StrProc then
  begin
    NextToken;
    Expect(toLParen);
    NextToken;

    T := ParseExpression;
    F := ParseFormat;

    Expect(toComma);
    NextToken;

    V := ParseVariableRef;

    case F of
      0: EmitStr(T, V);
      1: EmitStr1(T, V);
      2: EmitStr2(T, V);
    end;

    Expect(toRParen);
    NextToken;
  end
  else if Proc = ValProc then
  begin
    NextToken;
    Expect(toLParen);
    NextToken;

    if ParseExpression^.Kind <> scStringType then Error('String expected');

    Expect(toComma);
    NextToken;

    T := ParseVariableRef;
    if (T <> dtInteger) and (T <> dtReal) and (T^.Kind <> scEnumType) then Error('Numeric variable expected');

    Expect(toComma);
    NextToken;

    U := ParseVariableRef;
    if U <> dtInteger then Error('Integer variable expected');

    if T = dtInteger then
      EmitI('call __val_int')
    else if T^.Kind = scEnumType then
    begin
      EmitI('ld de,' + T^.Tag);
      EmitI('ld b,' + IntToStr(T^.High + 1));
      EmitI('call __val_enum');
    end
    else
      EmitI('call __val_float');

    Expect(toRParen);
    NextToken;
  end
  else if Proc = NewProc then
  begin
    NextToken; Expect(toLParen); NextToken;

    Expect(toIdent);
    Sym := LookupGlobalOrFail(Scanner.StrValue);
    NextToken;
    T := ParseVariableAccess(Sym);

    // EmitI('pop hl');
    // EmitI('ld e,(hl)');
    // EmitI('inc hl');
    // EmitI('ld d,(hl)');
    // EmitI('push de');

    EmitLiteral(T^.DataType^.Value);
    EmitCall(LookupBuiltInOrFail('GetMem'));

    Expect(toRParen); NextToken;
  end
  else if Proc = DisposeProc then
  begin
    NextToken; Expect(toLParen); NextToken;

    T := ParseExpression;
    if T^.Kind <> scPointerType then Error('Pointer type needed');
    (*
    Expect(toIdent);
    Sym := LookupGlobal(Scanner.StrValue);
    if Sym = nil then Error('Identifier "' + Scanner.StrValue + '" not found.');
    NextToken;
    T := ParseVariableAccess(Sym);
    *)
    // EmitI('pop hl');
    // EmitI('ld e,(hl)');
    // EmitI('inc hl');
    // EmitI('ld d,(hl)');
    // EmitI('push de');

    EmitLiteral(T^.DataType^.Value);
    EmitCall(LookupBuiltInOrFail('FreeMem'));

    Expect(toRParen); NextToken;
  end
  else if Proc = FillProc then
  begin
    NextToken;
    Expect(toLParen);
    NextToken;

    T := ParseVariableRef;

    Expect(toComma);
    NextToken;

    T := ParseExpression;
    if T <> dtInteger then Error('Integer expr expected');

    Expect(toComma);
    NextToken;

    T := ParseExpression;
    if not ((T = dtInteger) or (T = dtByte) or (T = dtChar) or (T = dtBoolean) or (T^.Kind <> scEnumType)) then Error('Ordinal expr expected');

    Expect(toRParen);
    NextToken;

    EmitI('pop bc');
    EmitI('pop de');
    EmitI('pop hl');
    EmitI('call __fillchar');
  end
  else if Proc = AssignProc then
  begin
    NextToken;
    Expect(toLParen);
    NextToken;
    T := ParseVariableRef;
    if T^.Kind <> scFileType then Error('File type expected');
    Expect(toComma);
    NextToken;
    if ParseExpression^.Kind <> scStringType then Error('String expected');
    Expect(toRParen);
    NextToken;

    if (T = dtFile) or (T^.DataType = dtFile) then
      EmitCall(LookupBuiltInOrFail('BlockAssign'))
    else if T = dtText then
      EmitCall(LookupBuiltInOrFail('TextAssign'))
    else
    begin
      EmitLiteral(T^.DataType^.Value);
      EmitCall(LookupBuiltInOrFail('FileAssign'));
    end;
  end
  else if  (Proc = EraseProc) or  (Proc = RenameProc) or (Proc = ResetProc) or (Proc = RewriteProc) or (Proc = AppendProc) or (Proc = CloseProc) or (Proc = FlushProc) or (Proc = SeekEofProc) or (Proc = SeekEolnProc) then
  begin
    NextToken;
    Expect(toLParen);
    NextToken;
    T := ParseVariableRef;
    if T^.Kind <> scFileType then Error('File type expected');
    Expect(toRParen);
    NextToken;

    if (T = dtFile) or (T^.DataType = dtFile) or (Proc = EraseProc) or (Proc = RenameProc) then
      Sym := LookupBuiltInOrFail('Block' + Proc^.Name)
    else if T = dtText then
      Sym := LookupBuiltInOrFail('Text' + Proc^.Name)
    else
      Sym := LookupBuiltInOrFail('File' + Proc^.Name);

    EmitCall(Sym);

    if IOMode then EmitCall(LookupBuiltInOrFail('BDosThrow'));
  end
  else if Proc = SeekProc then
  begin
    NextToken;
    Expect(toLParen);
    NextToken;
    T := ParseVariableRef;
    if T^.Kind <> scFileType then Error('File type expected');

    Expect(toComma);
    NextToken;

    ParseExpression; // ^.DataType <> dtByte and dtInteger then Error('Integer expected');

    Expect(toRParen);
    NextToken;

    if (T = dtFile) or (T^.DataType = dtFile) then
      EmitCall(LookupBuiltInOrFail('BlockSeek'))
    else if T <> dtText then
      EmitCall(LookupBuiltInOrFail('FileSeek'))
    else
      Error('Not allowed.');

    if IOMode then EmitCall(LookupBuiltInOrFail('BDosThrow'));
  end
  else if (Proc = BlockReadProc) or (Proc = BlockWriteProc) then
  begin
    NextToken;

    Expect(toLParen);
    NextToken;

    T := ParseVariableRef;
    if T^.Kind <> scFileType then Error('File type expected');

    Expect(toComma);
    NextToken;

    ParseVariableRef; (* Src or dest *)

    Expect(toComma);
    NextToken;

    TypeCheck(dtInteger, ParseExpression, tcExpr);

    if Scanner.Token = toComma then
    begin
      NextToken;
      ParseVariableRef; (* Actual bytes, check for integer!? *)
    end
    else EmitLiteral(0);

    Expect(toRParen);
    NextToken;

    EmitCall(LookupBuiltInOrFail('Block' + Proc^.Name));
    if IOMode then EmitCall(LookupBuiltInOrFail('BDosThrow'));

    // By Name umlenken auf Implementierung.
  end
  else
    Error('Cannot handle: ' + Proc^.Name);
end;

(**
 * Parses a set constant and generates a 32 byte block containing the proper
 * bits.
 *
 * TODO Can we keep set constants in a table similar to strings, ensure
 * uniqueness and write them later in one go?
 *)
function ParseSetConstant: PSymbol;
var
  I, J, K: Integer;
  Sym, T: PSymbol;
  S3: string;
  BA: array[0..31] of Byte;
begin
  for I := 0 to 31 do BA[I] := 0;

  T := nil;
  while Scanner.Token <> toRBrack do
  begin
    if Scanner.Token = toNumber then
    begin
      if (T <> nil) and (T <> dtInteger) then Error('Integer expected');

      T := dtInteger;

      I := Scanner.NumValue;
      J := I;
      NextToken;
      if Scanner.Token = toRange then
      begin
        NextToken;
        Expect(toNumber);
        J := Scanner.NumValue;
        NextToken;
      end
    end
    else if (Scanner.Token = toString) or (Scanner.Token = toCaret) then
    begin
      if (T <> nil) and (T <> dtChar) then Error('Char expected');
      if (Length(Scanner.StrValue) <> 1) and (Scanner.Token <> toCaret) then Error('Char expected');

      T := dtChar;

      if Scanner.Token = toCaret then
        I := GetCtrlChar
      else
      I := Ord(Scanner.StrValue[1]);

      J := I;
      NextToken;
      if Scanner.Token = toRange then
      begin
        NextToken;

        if (Scanner.Token <> toString) and (Scanner.Token <> toCaret) then
          Error('Char expected');
        if (Length(Scanner.StrValue) <> 1) and (Scanner.Token <> toCaret) then Error('Char expected');

        if Scanner.Token = toCaret then
          J := GetCtrlChar
        else
        J := Ord(Scanner.StrValue[1]);
        NextToken;
      end
    end
    else if Scanner.Token = toIdent then
    begin
      Sym := LookupGlobalOrFail(Scanner.StrValue);
      if (Sym^.Kind <> scConst) or (Sym^.DataType^.Kind <> scEnumType) then Error('Not an enum const');

      if (T <> nil) and (T <> Sym^.DataType) then Error('Wrong type');
      T := Sym^.DataType;

      I := Sym^.Value;
      J := I;
      NextToken;
      if Scanner.Token = toRange then
      begin
        NextToken;
        Expect(toIdent);
        Sym := LookupGlobalOrFail(Scanner.StrValue);
        if (Sym^.Kind <> scConst) or (Sym^.DataType^.Kind <> scEnumType) then Error('Not an enum const');
        if (T <> nil) and (T <> Sym^.DataType) then Error('Wrong type');

        J := Sym^.Value;
        NextToken;
      end
    end
    else Error('Scalar expected');

    //WriteLn('Adding: ', I, ' to ', J);
    for K := I to J do
    begin
      BA[K shr 3] := BA[K shr 3] or (1 shl (K and 7));
    end;
    if Scanner.Token = toComma then NextToken;
  end;

  S3 := '$' + IntToHex(BA[0], 2);;
  for K := 1 to 31 do S3 := S3 + ',$' + IntToHex(BA[K], 2);
  EmitI('db ' + S3);

  Sym := CreateSymbol(scSetType, '');
  Sym^.Value := 32;
  Sym^.DataType := T;
  T := Sym;

  ParseSetConstant := T;
end;

(**
 * Parses a factor, which is the smallest building block of an expression. A
 * factor can be a constant, a variable, a function call or a literal number,
 * string, set or pointer. We also handle unary operators on this level.
 *)
function ParseFactor: PSymbol;
var
  Sym: PSymbol; T: PSymbol;
  Op: TToken;
  S1, S2, Tag: String;
begin
  if Scanner.Token = toIdent then
  begin
    Sym := LookupGlobalOrFail(Scanner.StrValue);

    (* This is a dirty hack, but ok for now. *)
    if (Sym^.Kind = scVar) and (Sym^.Tag = 'RESULT') then
      Sym := Sym^.Prev^.Prev;

    if (Sym^.Kind = scVar) and (Sym^.IsMagic) then
    begin
      NextToken;
      T := ParseBuiltInVarRead(Sym);
    end
    else if Sym^.Kind = scVar then
    begin
      NextToken;
      T := ParseVariableAccess(Sym);

      // if not (T^.Kind in [scType, scEnumType]) then Error('" ' + Sym^.Name + '" is not a simple type.');
      (* File types are never communicated by value, only by reference. *)
      if (T^.Kind <> scFileType) and (T <> dtText) then EmitLoad(T);
    end
    else if Sym^.Kind = scConst then
    begin
      T := Sym^.DataType;
      if T = dtReal then
      begin
        EmitI('constfp ' + Sym^.Tag);
        EmitI('pushfp');
      end
      else
      begin
        if Sym^.Tag <> '' then
        begin
          EmitI('ld hl,' + Sym^.Tag);
          EmitI('call __loadstr');
        end
        else
          EmitLiteral(Sym^.Value);
      end;
      NextToken;
    end
    else if (Sym^.Kind = scFunc) and (Sym^.IsMagic) then
    begin
      T := ParseBuiltInFunction(Sym);
    end
    else if Sym^.Kind = scFunc then
    begin
      T := Sym^.DataType; (* Type = Type(func) *)
      if Sym^.IsStdCall then
      begin
        if T.Value = 1 then EmitLiteral(0) else EmitSpace(T.Value); (* Result *)
      end;
      NextToken;
      ParseArguments(Sym);
      EmitCall(Sym);
    end
    else if Sym^.Kind in [scType,scEnumType] then
    begin
      NextToken;
      Expect(toLParen);
      NextToken;
      ParseExpression();
      Expect(toRParen);
      NextToken;

      T := Sym;
    end
    else
    begin
      Error('"' + Scanner.StrValue + '" cannot be used in expressions.');
    end;
  end
  else if Scanner.Token = toString then
  begin
    if Length(Scanner.StrValue)=1 then
    begin
      T := dtChar;
      EmitLiteral(Ord(Scanner.StrValue[1]));
    end
    else
    begin
      T := dtString;
      Tag := AddString(Scanner.StrValue);
      EmitI('ld hl,' + Tag);
      EmitI('push hl');
      EmitLoad(T);
    end;
    NextToken;
  end
  else if Scanner.Token = toCaret then
  begin
    T := dtChar;
    EmitLiteral(GetCtrlChar);
    NextToken;
  end
  else if Scanner.Token = toFloat then
  begin
    T := dtReal;
    //Tag := AddString(Scanner.StrValue + ' ');
    EmitI('constfp ' + EncodeReal(Scanner.StrValue));
    EmitI('pushfp');
    NextToken;
  end
  else if Scanner.Token = toNumber then
  begin
    T := dtInteger;
    EmitLiteral(Scanner.NumValue);
    NextToken;
  end
  else if Scanner.Token = toNil then
  begin
    T := dtPointer;
    EmitLiteral(0);
    NextToken;
  end
  (* true false + String constants *)
  else if Scanner.Token = toLParen then
  begin
    NextToken; T := ParseExpression; Expect(toRParen); NextToken;
  end
  else if Scanner.Token = toLBrack then
  begin
    NextToken;

    S1 := GetLabel('set');
    S2 := GetLabel('set');
    EmitI('jr ' + S2);
    Emit(S1, '', '');

    T := ParseSetConstant();

    Emit(S2, '', '');

    Emit('', 'ld hl,' + S1, 'Load set constant');
    EmitI('push hl');
    EmitLoad(T);

    Expect(toRBrack);
    NextToken;
  end
  else if Scanner.Token in [toAdd, toSub, toNot] then
  begin
    Op := Scanner.Token;
    NextToken;
    T := ParseFactor();
    if T = dtChar then Error('not only applicable to Integer, Byte, Real or Boolean');
    case Op of
      toAdd: begin (* Nop *) end;
      toSub: EmitNeg(T);
      toNot: EmitUnOp(toNot, T);
    end;
  end
  else Error('Factor expected');

  ParseFactor := T;
end;

(**
 * Parses a term, which can be a factor or any number of factors connected by
 * multiplicative, dividing or bitshifting operators.
 *)
function ParseTerm: PSymbol;
var
  Op: TToken;
  T: PSymbol;
begin
  T := ParseFactor;

  if (T = dtInteger) or (T = dtByte) then
    while Scanner.Token in [toMul, toDiv, toDivKW, toMod, toAnd, toShl, toShr] do
    begin
      Op := Scanner.Token;
      NextToken;
      if Op = toDiv then T := TypeCheck(dtReal, T, tcExpr);
      T := TypeCheck(T, ParseFactor, tcExpr);
      if T = dtReal then EmitFloatOp(Op) else EmitBinOp(Op, T);
    end
  else if T = dtBoolean then
    while Scanner.Token = toAnd do
    begin
      Op := Scanner.Token;
      NextToken;
      T := TypeCheck(T, ParseFactor, tcExact);
      EmitBinOp(Op, T);
    end
  else if T = dtReal then
    while Scanner.Token in [toMul, toDiv, toMod] do
    begin
      Op := Scanner.Token;
      NextToken;
      T := TypeCheck(T, ParseFactor, tcExpr);
      EmitFloatOp(Op);
    end
  else if T^.Kind = scSetType then
    while Scanner.Token in [toMul] do
    begin
      Op := Scanner.Token;
      NextToken;
      T := ParseFactor; (* TypeCheck(T, , tcExpr); *)
      EmitSetOp(Op);
    end;

  ParseTerm := T;
end;

(**
 * Parses a simple expression, which can be a term or any number of terms
 * connected by additive or subtrative operators.
 *)
function ParseSimpleExpression: PSymbol;
var
  Op: TToken;
  T: PSymbol;
begin
  T := ParseTerm;

  if (T = dtInteger) or (T = dtByte) then
    while Scanner.Token in [toAdd, toSub, toOr, toXor] do
    begin
      Op := Scanner.Token;
      NextToken;
      T := TypeCheck(T, ParseTerm, tcExpr);
      if T = dtReal then EmitFloatOp(Op) else EmitBinOp(Op, T);
    end
  else if T = dtBoolean then
    while Scanner.Token in [toOr, toXor] do
    begin
      Op := Scanner.Token;
      NextToken;
      T := TypeCheck(T, ParseTerm, tcExact);
      EmitBinOp(Op, T);
    end
  else if (T = dtChar) or (T^.Kind = scStringType) then
  begin
    while Scanner.Token = toAdd do
    begin
      T := TypeCheck(dtString, T, tcExact);
      NextToken;
      T := TypeCheck(dtString, ParseTerm, tcExact);
      EmitI('call __stradd');
      EmitClear(256);
    end
  end
  else if T = dtReal then
    while Scanner.Token in [toAdd, toSub] do
    begin
      Op := Scanner.Token;
      NextToken;
      T := TypeCheck(T, ParseTerm, tcExpr);
      EmitFloatOp(Op);
    end
  else if T^.Kind = scSetType then
    while Scanner.Token in [toAdd, toSub] do
    begin
      Op := Scanner.Token;
      NextToken;
      T := ParseTerm; (* TypeCheck(T, , tcExpr); *)
      EmitSetOp(Op);
    end;


(* TODO Error case? *)

  (* WriteLn('Type of SimpleExpression is ', T); *)

  ParseSimpleExpression := T;
end;

(**
 * Parses an expression, which can be one simple expression or two simple
 * expressions connected by a relational operator.
 *)
function ParseExpression: PSymbol;
var
  Op: TToken;
  T, U: PSymbol;
begin
  T := ParseSimpleExpression;
  if Scanner.Token = toIn then
  begin
    if not (T^.Kind in [scType, scEnumType, scSubrangeType]) then Error('Scalar needed');
    (* if T^.Value <> 1 then Error('8 bit needed'); *) (* FIXME!!! *)

    NextToken;
    U := ParseExpression;
    if U^.Kind <> scSetType then Error('Set needed');
    (* if U^.DataType <> T then Error('Incompatible');    *) (* FIXME!!! *)

    EmitSetOp(toIn);

    T := dtBoolean;
  end
  else if (T^.Kind = scSetType) and (Scanner.Token in [toEq, toNeq, toLeq, toGeq]) then
  begin
    Op := Scanner.Token;
    //WriteLn(Op);
    NextToken;

    U := ParseExpression;
    if U^.Kind <> scSetType then Error('Set needed');
    // if U^.DataType <> T^.DataType then Error('Incompatible');  // FIXME!!!

    EmitSetOp(Op);

    T := dtBoolean;
  end
  else if (Scanner.Token >= toEq) and (Scanner.Token <= toGeq) then
  begin
    Op := Scanner.Token;
    NextToken;
    (*
      * ii ib bi bb zz cc
      *)
    T := TypeCheck(T, ParseSimpleExpression, tcExpr);
    if T^.Kind = scStringType then
      EmitStrOp(Op)
    else if T = dtReal then
      EmitFloatOp(Op)
    else
      EmitRelOp(Op);
    T := dtBoolean;                       // We know it will be Boolean.
  end;

  (* WriteLn('Type of Expression is ', T); *)

  ParseExpression := T;
end;

(**
 * Parses an assignment.
 *)
procedure ParseAssignment(Sym: PSymbol);
var
  T: PSymbol;
begin
  if Sym^.Kind <> scVar then Error('"' + Scanner.StrValue + '" not a var.');
  NextToken;

  T := ParseVariableAccess(Sym);

  //if not (T^.Kind in [scType, scEnumType]) then Error('" ' + Sym^.Name + '" is not a simple type.');

  Expect(toBecomes);
  NextToken;

  T := TypeCheck(T, ParseExpression, tcAssign);

  EmitStore(T); (* T? *)
end;

procedure ParseStatement(ContTarget, BreakTarget: String); forward;

(**
 * Parses a single "with" variable, which must be of a record type. Creates
 * "proxy" variables for all fields contained in the record.
 *
 * TODO Should we simply create a scope instead of this "stunt" using anonymous
 * variables?
 *)
procedure ParseWithVar;
var
  Sym, Field, FieldRef, DataType: PSymbol;
  Address: Integer;
begin
  Expect(toIdent);
  Sym := LookupGlobalOrFail(Scanner.StrValue);
  if Sym^.Kind <> scVar then Error('Variable expected');
  NextToken;

  Offset := Offset - 2;
  Address := Offset;
  DataType := ParseVariableAccess(Sym);

  if DataType^.Kind <> scRecordType then Error('Record type expected');

  Field := DataType^.DataType;
  while Field <> nil do
  begin
    FieldRef := CreateSymbol(scVar, ''); // Create anonymously so duplicate
    FieldRef^.Name := Field^.Name;       // identifiers are not a problem
    FieldRef^.DataType := Field^.DataType;
    FieldRef^.Value := Address;
    FieldRef^.Value2 := Field^.Value;
    FieldRef^.IsRef := True;
    FieldRef^.Level := Level;
    // WriteLn('Create ' , Field^.Name, ' at level ', Level, 'in ', Sym^.Name, ' address ', Address, ' Offset ', Field^.Value);
    Field := Field^.Prev;
  end;
end;

(**
 * Parses a "with" statement, "opening" the scopes of a single record variable
 * or a list of such variables and making the contained fields directly
 * accessible as variables.
 *)
procedure ParseWith(ContTarget, BreakTarget: String);
var
  OldSymbols, Sym: PSymbol;
  Count, I: Integer;
begin
  OldSymbols := SymbolTable;

  ParseWithVar;
  Count := 1;

  while Scanner.Token = toComma do
  begin
    NextToken;
    ParseWithVar;
    Inc(Count);
  end;

  Expect(toDo);
  NextToken;

  // TODO Check global (not in unit tests), Local, Var, Nested, Comma
  // Optimize simple case 'with R do X := 5;' to avoid double addressing

  // TODO What happens to the stack if we exit a with statement early via
  // Break or Exit?

  ParseStatement(ContTarget, BreakTarget);

  Offset := Offset + 2 * Count;
  for I := 1 to Count do EmitI('pop bc');

  while SymbolTable <> OldSymbols do
  begin
    Sym := SymbolTable;
    SymbolTable := SymbolTable^.Prev;
    // WriteLn('Drop ' , Sym^.Name);
    FreeMem(Sym);
  end;
end;

(**
 * Parses an inline term, which is, in the easiest case, a number representing
 * a Z80 opcode, but can also refer to identifiers such as constants, variables,
 * procedures or functions (in which case either the value or the address would
 * be inserted into the outgoing assembly code). The special symbol "*" refers
 * to the "current address" and allows PC-relative addressing (useful for
 * relative jumps).
 *)
procedure ParseInlineTerm(var S: String; var W: Boolean);
var
  Sym: PSymbol;
begin
  if Scanner.Token = toNumber then
  begin
    S := S + IntToStr(Scanner.NumValue);
    if Scanner.NumValue > 255 then W := True;
    NextToken;
  end
  else if Scanner.Token = toIdent then
  begin
    Sym := LookupGlobalOrFail(Scanner.StrValue);

    if (Sym^.Kind = scConst) and (Sym^.DataType = dtInteger) then
    begin
      S := S + IntToStr(Sym^.Value);
      if Sym^.Value > 255 then W := True;
    end
    else if (Sym^.Kind = scVar) and (Sym^.Level = 0) then
    begin
      S := S + Sym^.Tag;
      W := True;
    end
    else if (Sym^.Kind = scVar) and (Sym^.Level > 0) then
    begin
      S := S + IntToStr(Sym^.Value);
      W := True;
    end
    else if (Sym^.Kind = scProc) or (Sym^.Kind = scFunc) then
    begin
      S := S + Sym^.Tag;
      W := True;
    end
    else
      Error(S + ' not allowed here');

    NextToken;
  end
  else if Scanner.Token = toMul then
  begin
    S := S + '$';
    W := True;
    NextToken;
  end
  else if (Scanner.Token = toString) and (Length(Scanner.StrValue) = 1) then
  begin
    S := S + IntToStr(Ord(Scanner.StrValue[1]));
    NextToken;
  end
  else Error('Invalid inline code');
end;

(**
 * Parses a single expression inside an inline statement, which is mainly a list
 * of inline terms connected by additive or subtractive operators.
 *)
procedure ParseInlineExpr;
var
  S: String;
  W: Boolean;
  O: TToken;
begin
  S := '';
  W := False;
  O := toNone;

  if Scanner.Token in [toLt, toGt] then
  begin
    O := Scanner.Token;
    NextToken;
  end;

  ParseInlineTerm(S, W);
  while Scanner.Token in [toAdd, toSub] do
  begin
    W := True;
    if Scanner.Token = toAdd then S := S + '+' else S := S + '-';
    NextToken;
    ParseInlineTerm(S, W);
  end;

  if O = toLt then
    EmitI('db lo(' + S + ')')
  else if (O = toGt) or W then
    EmitI('dw ' + S)
  else EmitI('db ' + S);
end;

(**
 * Parses an inline statement including the opening and closing parentheses. It
 * consists of a number of inline expressions separated by "/" characters.
 *)
procedure ParseInline;
begin
  Expect(toLParen);
  NextToken;

  if Scanner.Token <> toRParen then
  begin
    ParseInlineExpr;
    while Scanner.Token = toDiv do
    begin
      NextToken;
      ParseInlineExpr;
    end;
  end;

  Expect(toRParen);
  NextToken;
end;

procedure ParseCaseValue(T: PSymbol; var V: Integer);
var
  C: PSymbol;
begin
  if Scanner.Token = toIdent then
  begin
    C := LookupGlobalOrFail(Scanner.StrValue);
    if C^.Kind <> scConst then Error('Const expected');
    if C^.DataType <> T then Error('Invalid type');
    V := C^.Value;
  end
  else if Scanner.Token = toNumber then
  begin
    if T <> dtInteger then Error('Invalid type');
    V := Scanner.NumValue;
  end
  else if Scanner.Token = toCaret then
  begin
    if T <> dtChar then Error('Invalid type');
    V := GetCtrlChar;
  end
  else (* toString *)
  begin
    if Length(Scanner.StrValue) <> 1 then Error('Invalid type');
    if T <> dtChar then Error('Invalid type');
    V := Ord(Scanner.StrValue[1]);
  end;

  NextToken;
end;

procedure ParseCaseLabel(T: PSymbol; OfTarget: String);
var
  Low, High: Integer;
begin
  ParseCaseValue(T, Low);
  High := Low;
  if Scanner.Token = toRange then
  begin
    NextToken;
    ParseCaseValue(T, High);
  end;

  if High = Low then
  begin
    EmitI('ld hl,' + IntToStr(High));
    EmitI('call __int16_eq');
    EmitI('and a');
    EmitI('jp nz,' + OfTarget);
  end
  else
  begin
    EmitI('ld bc,' + IntToStr(Low));
    EmitI('ld hl,' + IntToStr(High));
    EmitI('call __int16_case');
    EmitI('and a');
    EmitI('jp nz,' + OfTarget);
  end;
end;

procedure ParseStatementList(ContTarget, BreakTarget: String); forward;

procedure ParseCaseStatement(ContTarget, BreakTarget: String);
var
  T: PSymbol;
  OfTarget, NextTest, EndTarget: String;
begin
  EndTarget := GetLabel('end');

  NextToken;
  T := ParseExpression;

  EmitI('pop de');

  Expect(toOf);
  NextToken;

  while Scanner.Token in [toIdent, toNumber, toString, toCaret] do
  begin
    OfTarget := GetLabel('case');
    NextTest := GetLabel('test');

    ParseCaseLabel(T, OfTarget);
    while Scanner.Token = toComma do
    begin
      NextToken;
      ParseCaseLabel(T, OfTarget);
    end;

    Expect(toColon);
    NextToken;

    EmitI('jp ' + NextTest);

    Emit(OfTarget, '', '');

    ParseStatement(ContTarget, BreakTarget);
    Expect(toSemicolon);
    NextToken;

    EmitI('jp ' + EndTarget);

    Emit(NextTest, '', '');
  end;

  if Scanner.Token = toElse then
  begin
    NextToken;
    ParseStatementList(ContTarget, BreakTarget);
  end;

  Emit(EndTarget, '', '');

  Expect(toEnd);
  NextToken;
end;

(**
 * Parses a statement, which can be an atomic statement like a procedure call,
 * an assignment, a label or a piece of inline assembly, or a compound statement
 * like if/while/repeat/for or a begin..end block.
 *)
procedure ParseStatement(ContTarget, BreakTarget: String);
var
  Sym: PSymbol;
  Tag, Tag2, Tag3, Tag4: String;
  Delta: Integer;
  (*T: PSymbol;*)
begin
  if CheckBreak then EmitI('call __checkbreak');

  if Scanner.Token = toNumber then
  begin
    Sym := LookupGlobalOrFail(IntToStr(Scanner.NumValue));

    if Sym^.Kind = scLabel then
    begin
      NextToken;
      Expect(toColon);
      NextToken;
      Emit(Sym^.Tag, '', '');
    end;
  end
  else if Scanner.Token = toIdent then
  begin
    Sym := LookupGlobalOrFail(Scanner.StrValue);

    if Sym^.Kind = scLabel then
    begin
      NextToken;
      Expect(toColon);
      NextToken;
      Emit(Sym^.Tag, '', '');
    end;
  end;

  if Scanner.Token = toIdent then
  begin
    Sym := LookupGlobalOrFail(Scanner.StrValue);

    if (Sym^.Kind = scProc) and (Sym^.IsMagic) then
    begin
      ParseBuiltInProcedure(Sym, BreakTarget, ContTarget);
    end
    else if Sym^.Kind = scProc then
    begin
      NextToken;
      if Scanner.Token = toBecomes then Error('"' + Sym^.Name + '" not a var.');

      ParseArguments(Sym);

      EmitCall(Sym);
    end
    else if (Sym^.Kind = scVar) and (Sym^.IsMagic) then
    begin
      NextToken;
      ParseBuiltInVarWrite(Sym);
    end
    else
    begin
      ParseAssignment(Sym);
    end;
  end
  else if Scanner.Token = toBegin then
  begin
    NextToken; ParseStatement(ContTarget, BreakTarget);
    while Scanner.Token = toSemicolon do
    begin
      NextToken;
      ParseStatement(ContTarget, BreakTarget);
    end;
    Expect(toEnd); NextToken;
  end
  else if Scanner.Token = toIf then
  begin
    NextToken; TypeCheck(dtBoolean, ParseExpression, tcExact); Expect(toThen); NextToken;

    Tag := GetLabel('false');

    EmitJumpIf(False, Tag);

    ParseStatement(ContTarget, BreakTarget);

    if Scanner.Token = toElse then
    begin
      Tag2 := GetLabel('endif');
      EmitI('jp ' + Tag2);

      Emit(Tag, '', '');

      NextToken;
      ParseStatement(ContTarget, BreakTarget);

      Emit(Tag2, '', '');
    end
    else
    begin
      Emit(Tag, '', '');
    end;

  end
  else if Scanner.Token = toWhile then
  begin
    (* Break would jump to false *)

    Tag := GetLabel('while');
    Tag2 := GetLabel('false');

    Emit(Tag, '', '');

    NextToken; TypeCheck(dtBoolean, ParseExpression, tcExact); Expect(toDo); NextToken;

    EmitJumpIf(False, Tag2);

    ParseStatement(Tag, Tag2);

    EmitI('jp ' + Tag);         (* TODO Move this elsewhere. *)
    Emit(Tag2, '', '');
  end
  else if Scanner.Token = toRepeat then
  begin
    Tag := GetLabel('repeat');
    Tag2 := GetLabel('break');

    (* break would jump to label after until *)

    Emit(Tag, '', '');

    NextToken; ParseStatement(Tag, Tag2);
    while Scanner.Token = toSemicolon do
    begin
      NextToken;
      ParseStatement(Tag, Tag2);
    end;
    Expect(toUntil); NextToken;

    TypeCheck(dtBoolean, ParseExpression, tcExact);

    EmitJumpIf(False, Tag);
    Emit(Tag2, '', '');
  end
  else if Scanner.Token = toFor then
  begin
    (* break would jump to cleanup after for *)

    NextToken;
    Expect(toIdent);

    Sym := LookupGlobalOrFail(Scanner.StrValue);
    if Sym^.Kind <> scVar then Error('Identifier "' + Scanner.StrValue + '" not a var.');

    EmitAddress(Sym);
    NextToken; Expect(toBecomes); NextToken; TypeCheck(Sym^.DataType, ParseExpression, tcAssign);
    EmitStore(Sym^.DataType);

    if Scanner.Token = toTo then
      Delta := 1
    else if Scanner.Token = toDownTo then
      Delta := -1
    else
      Error('"to" or "downto" expected.');

    NextToken; TypeCheck(Sym^.DataType, ParseExpression, tcAssign); Expect(toDo); (* final value on stack *)

    Tag := GetLabel('forloop');
    Tag3 := GetLabel('forbreak');
    Tag4 := GetLabel('fornext');

    Emit('', 'pop de','Dup and pre-check limit');
    Emit('', 'push de','');
    Emit('', 'push de', '');

    Offset := Offset - 2;

    EmitAddress(Sym);
    EmitLoad(Sym^.DataType);

    if Delta = 1 then EmitRelOp(toGeq) else EmitRelOp(toLeq); (* Operands swapped! *)

    EmitJumpIf(False, Tag3);

    Emit(Tag, '', '');

    NextToken;
    ParseStatement(Tag4, Tag3);

    Emit(Tag4, '', '');

    Emit('', 'pop de','Dup and check limit');
    Emit('', 'push de','');
    Emit('', 'push de', '');

    EmitAddress(Sym);
    EmitLoad(Sym^.DataType);

    if Delta = 1 then EmitRelOp(toGt) else EmitRelOp(toLt); (* Operands swapped! *)

    EmitJumpIf(False, Tag3);

    EmitAddress(Sym);
    if Delta = 1 then EmitInc(Sym^.DataType) else EmitDec(Sym^.DataType);

    EmitJump(Tag);

    Emit(Tag3, 'pop de', 'Cleanup limit'); (* Cleanup loop variable *)

    Offset := Offset + 2;
  end
  else if Scanner.Token = toCase then
  begin
    ParseCaseStatement(ContTarget, BreakTarget);
  end
  else if Scanner.Token = toWith then
  begin
    NextToken;
    ParseWith(ContTarget, BreakTarget);
  end
  else if Scanner.Token = toInline then
  begin
    NextToken;
    ParseInline;
  end
  else if Scanner.Token = toGoto then
  begin
    NextToken;

    if Scanner.Token = toIdent then
    begin
      Sym := LookupLocalOrFail(Scanner.StrValue);
      // ...
      EmitI('jp ' + Sym^.Tag);
    end
    else if Scanner.Token = toNumber then
    begin
      Sym := LookupLocalOrFail(IntToStr(Scanner.NumValue));
      // ...
      EmitI('jp ' + Sym^.Tag);
    end
    else Error('Ident or number expected');

    NextToken;
  end;
end;

function ParseTypeDef: PSymbol; forward;

(**
 * Parses a typed constant (aka an initialized variable). Typed constants are
 * always allocated statically.
 *)
procedure ParseConstValue(DataType: PSymbol);
var
  I, Sign, Value, Offset: Integer;
  Sym: PSymbol;
  Test: Boolean;
begin
  if DataType^.Kind = scArrayType then
  begin
    Expect(toLParen);
    NextToken;

    for I := DataType^.IndexType^.Low to DataType^.IndexType^.High - 1 do
    begin
      ParseConstValue(DataType^.DataType);
      Expect(toComma);
      NextToken;
    end;
    ParseConstValue(DataType^.DataType);

    Expect(toRParen);
    NextToken;
  end
  else if DataType^.Kind = scRecordType then
  begin
    Expect(toLParen);
    NextToken;

    Offset := 0;
    Test := True;
    while Test do
    begin
      Test := False;

      if Scanner.Token = toIdent then
      begin
        Sym := Lookup(Scanner.StrValue, DataType^.DataType, nil);
        if Sym = nil then Error('Unknown field "' + Scanner.StrValue + '"');

        if Sym^.Value < Offset then Error('Invalid offset');
        if Sym^.Value > Offset then
        begin
          EmitI('ds ' + IntToStr(Sym^.Value - Offset) + ',0');
          Offset := Sym^.Value;
        end;

        NextToken;
        Expect(toColon);
        NextToken;

        ParseConstValue(Sym^.DataType);

        Offset := Offset + Sym^.DataType^.Value;
      end;

      if Scanner.Token = toSemicolon then
      begin
        Test := True;
        NextToken;
      end;
    end;

    if Offset < DataType^.Value then
      EmitI('ds ' + IntToStr(DataType^.Value - Offset) + ',0');

    Expect(toRParen);
    NextToken;
  end
  else if DataType^.Kind = scEnumType then
  begin
    Expect(toIdent);
    Sym := LookupGlobalOrFail(Scanner.StrValue);
    if (Sym^.Kind <> scConst) or (Sym^.DataType <> DataType) then
      Error('Invalid enum constant');

    Emit('', 'db ' + IntToStr(Sym^.Value), '');

    NextToken;
  end
  else
  begin
    if DataType = dtInteger then
    begin
      if Scanner.Token = toSub then
      begin
        Sign := -1;
        NextToken;
      end
      else Sign := 1;
      Expect(toNumber);

      Value := Sign * Scanner.NumValue;
      Emit('', 'dw ' + IntToStr(Value), '');

      NextToken;
    end
    else if DataType = dtByte then
    begin
      Expect(toNumber);

      Emit('', 'db ' + IntToStr(Scanner.NumValue), '');

      NextToken;
    end
    else if DataType = dtReal then
    begin
      if Scanner.Token = toSub then
      begin
        Sign := -1;
        NextToken;
      end
      else Sign := 1;
      Expect(toFloat);

      Emit('', 'dw ' + EncodeReal(Scanner.StrValue), '');

      NextToken;
    end
    else if DataType = dtChar then
    begin
      Expect(toString);
      if Length(Scanner.StrValue) <> 1 then Error('Char expected');
      Emit('', 'db "' + Scanner.StrValue + '"', '');
      NextToken;
    end
    else if DataType^.Kind = scStringType then
    begin
      Expect(toString);
      // Emit('', 'db ' + IntToStr(Length(Scanner.StrValue)) + ', "' +  EncodeAsmStr(Scanner.StrValue) + '"', '');
      Emit('', 'db ' + EncodeAsmStr(Scanner.StrValue), '');
      Emit('', 'ds ' + IntToStr(DataType^.Value - Length(Scanner.StrValue) - 1) + ',0', '');
      NextToken;
    end
    else if DataType^.Kind = scSetType then
    begin
      Expect(toLBrack);
      NextToken;

      ParseSetConstant; (* FIXME: Type check!!! *)

      Expect(toRBrack);
      NextToken;
    end
    else
      Error('Not supported');
  end;
end;

(**
 * Parses a constant declaration, which can either be a real constant or a
 * so-called typed constant, which is basically an initialized variable.
 *)
procedure ParseConst;
var
  Name: String;
  Sym, Sym2: PSymbol;
  Sign: Integer;
begin
  Expect(toIdent);
  Name := Scanner.StrValue;
  NextToken;

  if Scanner.Token = toColon then
  begin
    NextToken;

    Sym := CreateSymbol(scVar, Name);
    Sym^.DataType := ParseTypeDef();
    Sym^.Tag := GetLabel('const');
    Sym^.Level := 0; (* Force global *)

    Emit(Sym^.Tag, '', '');

    Expect(toEq);
    NextToken;

    ParseConstValue(Sym^.DataType);
  end
  else
  begin
    Sym := CreateSymbol(scConst, Name);

    Expect(toEq);
    NextToken;

    if Scanner.Token = toString then
    begin
      Sym^.DataType := dtString;
      Sym^.Tag := AddString(Scanner.StrValue);
    end
    else if Scanner.Token = toFloat then
    begin
      Sym^.DataType := dtReal;
      Sym^.Tag := EncodeReal(Scanner.StrValue);
    end
    else if Scanner.Token = toIdent then
    begin
      Sym2 := LookupGlobalOrFail(Scanner.StrValue);
      if Sym2^.Kind <> scConst then Error('Not a constant: ' + Scanner.StrValue);

      Sym^.DataType := Sym2^.DataType;
      Sym^.Value := Sym2^.Value;
    end
    else if Scanner.Token = toCaret then
    begin
      Sym^.DataType := dtChar;
      Sym^.Value := GetCtrlChar;
    end
    else
    begin
      if Scanner.Token = toSub then
      begin
        Sign := -1;
        NextToken;
      end
      else Sign := 1;

      if Scanner.Token = toFloat then
      begin
        Sym^.DataType := dtReal;
        Sym^.Tag := EncodeReal('-' + Scanner.StrValue);
      end
      else
      begin
        Sym^.Value := Sign * Scanner.NumValue;
        Sym^.DataType := dtInteger;
      end;    end;

    NextToken;
  end;
end;

(**
 * Parses a single record field name (just the name, no type).
 *)
procedure ParseField(var Fields: PSymbol);
var
  Sym: PSymbol;
begin
  Expect(toIdent);
  if Lookup(Scanner.StrValue, Fields, nil) <> nil then Error('Duplicate field "' + Scanner.StrValue + '"');

  New(Sym);
  FillChar(Sym^, SizeOf(TSymbol), 0);
  Sym^.Kind := scVar;
  Sym^.Name := Scanner.StrValue;
  Sym^.Prev := Fields;
  Fields := Sym;

  NextToken;
end;

(**
 * Parses a field group, that is, a comma-separated list of record field names
 * with a single type specification.
 *)
procedure ParseFieldGroup(var Fields: PSymbol; var Offset: Integer);
var
  Sym, Typ: PSymbol;
  Len, I: Integer;
begin
  ParseField(Fields);
  Len := 1;

  while Scanner.Token = toComma do
  begin
    NextToken;
    ParseField(Fields);
    Len := Len + 1;
  end;

  Expect(toColon);
  NextToken;
  Typ := ParseTypeDef();

  Offset := Offset + Len * Typ^.Value;
  Sym := Fields;

  for I := 1 to Len do
  begin
    Sym^.DataType := Typ;
    Sym^.Value := Offset - I * Typ^.Value;
    Sym := Sym^.Prev;
  end;
end;

(**
 * Parses a record type definition including a potentially variant part.
 *)
procedure ParseRecord(RecSym: PSymbol);
var
  Sym, CaseType: PSymbol;
  FixedSize, VariantSize: Integer;
  Ident: String;
begin
  while Scanner.Token = toIdent do
  begin
    ParseFieldGroup(RecSym^.DataType, RecSym^.Value);
    Expect(toSemicolon);
    NextToken;
  end;

  if Scanner.Token = toCase then
  begin
    NextToken;
    Expect(toIdent);
    Ident := Scanner.StrValue;
    NextToken;
    if Scanner.Token = toColon then
    begin
      NextToken;
      Expect(toIdent);
      CaseType := LookupGlobalOrFail(Scanner.StrValue);

      New(Sym);
      FillChar(Sym^, SizeOf(TSymbol), 0);
      Sym^.Kind := scVar;
      Sym^.Name := Ident;
      Sym^.DataType := CaseType;
      Sym^.Value := RecSym^.Value;
      Sym^.Prev := RecSym^.DataType;
      RecSym^.DataType := Sym;

      RecSym^.Value := RecSym^.Value + CaseType^.Value;

      NextToken;
    end
    else
    begin
      CaseType := LookupGlobalOrFail(Ident);
    end;

    FixedSize := RecSym^.Value;
    VariantSize := FixedSize;

    Expect(toOf);
    NextToken;

    while Scanner.Token in [toIdent, toNumber] do
    begin
      NextToken; (* Type check? *)
      while Scanner.Token = toComma do
      begin
        NextToken;
        NextToken; (* Type check? *)
      end;

      Expect(toColon);
      NextToken;

      Expect(toLParen);
      NextToken;

      ParseRecord(RecSym);
      if RecSym^.Value > VariantSize then VariantSize := RecSym^.Value;
      RecSym^.Value := FixedSize;

      Expect(toRParen);
      NextToken;

      Expect(toSemicolon);
      NextToken;
    end;

    RecSym^.Value := VariantSize;
  end;
end;

(**
 * Parses an array type definition. We support the standard form (array of
 * array) and the alternative shorthand form (one pair of brackets overall,
 * comma-separated dimensions specifiers).
 *
 * TODO Allow C-style array sizes with array[n] meaning array[0..n-1]?
 *)
procedure ParseArray(DataType: PSymbol; First: Boolean);
begin
  if First then
  begin
    Expect(toLBrack);
    NextToken;
  end;

  DataType^.IndexType := ParseTypeDef;
  if not (DataType^.IndexType^.Kind in [scType, scEnumType, scSubrangeType]) then
    Error('Not an ordinal type');

  if Scanner.Token = toComma then
  begin
    NextToken;
    DataType^.DataType := CreateSymbol(scArrayType, '');
    ParseArray(DataType^.DataType, False);
  end
  else
  begin
    Expect(toRBrack);
    NextToken;

    Expect(toOf);
    NextToken;

    DataType^.DataType := ParseTypeDef();
  end;

  DataType^.Value := (DataType^.IndexType^.High - DataType^.IndexType^.Low + 1) * DataType^.DataType^.Value;
end;

(**
 * Parses a type definition, which can be an array, a string, a file, a record,
 * a pointer, a set, a range, or a reference to an existing type (and
 * potentially nested, of course).
 *)
function ParseTypeDef: PSymbol;
var
  DataType, Sym: PSymbol;
  I: Integer;
begin
  if Scanner.Token = toArray then
  begin
    DataType := CreateSymbol(scArrayType, '');
    NextToken;

    ParseArray(DataType, True);
  end
  else if Scanner.Token = toStringKW then
  begin
    NextToken;

    DataType := CreateSymbol(scStringType, '');

    if Scanner.Token = toLBrack then
    begin
      NextToken;
      Expect(toNumber);
      DataType^.Value := Scanner.NumValue + 1;
      NextToken;
      Expect(toRBrack);
      NextToken;
    end
    else DataType^.Value := 256;
  end
  else if Scanner.Token = toFile then
  begin
    NextToken;

    if Scanner.Token = toOf then
    begin
      DataType := CreateSymbol(scFileType, '');
      NextToken;
      DataType^.DataType := ParseTypeDef;
      DataType^.Value := 256;
    end
    else DataType := dtFile;
  end
  else if Scanner.Token = toRecord then
  begin
    DataType := CreateSymbol(scRecordType, '');
    NextToken;

    ParseRecord(DataType);
    Expect(toEnd);
    NextToken;
  end
  else if Scanner.Token = toSet then
  begin
    NextToken;
    Expect(toOf);
    NextToken;

    DataType := CreateSymbol(scSetType, '');
    DataType^.DataType := ParseTypeDef;

    if not (DataType^.DataType^.Kind in [scType, scEnumType, scSubrangeType]) then
      Error('Scalar type required');

    if DataType^.DataType^.Value > 1 then
      Error('Base type too large');

    DataType^.Value := 32;
  end
  else if Scanner.Token = toCaret then
  begin
    DataType := CreateSymbol(scPointerType, '');
    DataType^.Value := 2;
    NextToken;

    if Scanner.Token = toIdent then
    begin
      DataType^.DataType := Lookup(Scanner.StrValue, SymbolTable, nil);
      if DataType^.DataType = nil then DataType^.Tag := Scanner.StrValue;
      NextToken;
    end
    else DataType^.DataType := ParseTypeDef;
  end
  else if Scanner.Token = toLParen then
  begin
    DataType := CreateSymbol(scEnumType, '');
    DataType^.Tag := GetLabel('enumlit');
    DataType^.Value := 1;
    I := 0;

    Emit(DataType^.Tag, '', '');

    repeat
      NextToken;
      Expect(toIdent);

      Sym := CreateSymbol(scConst, Scanner.StrValue);
      Sym^.Value := I;
      // Sym^.Value2 := AddString(Sym^.Name);

      Emit('', 'dw ' + AddString(Sym^.Name), '');

      Sym^.DataType := DataType;
      I := I + 1;
      NextToken;
    until Scanner.Token <> toComma;

    Emit('', 'dw 0', '');

    DataType^.Low := 0;
    DataType^.High := I - 1;

    Expect(toRParen);
    NextToken;
  end
  else if Scanner.Token = toNumber then
  begin
    DataType := CreateSymbol(scSubrangeType, '');
    DataType^.DataType := dtInteger;

    DataType^.Low := Scanner.NumValue;
    NextToken;
    Expect(toRange);
    NextToken;

    if Scanner.Token = toIdent then
    begin
      Sym := LookupGlobalOrFail(Scanner.StrValue);

      if Sym^.Kind <> scConst then Error('Not a constant');
      if Sym^.DataType <> dtInteger then Error('Not an integer');

      DataType^.High := Sym^.Value;

      if Sym^.Value < 256 then DataType^.Value := 1 else DataType^.Value := 2;
    end
    else
    begin
      Expect(toNumber);
      DataType^.High := Scanner.NumValue;

      if Scanner.NumValue < 256 then DataType^.Value := 1 else DataType^.Value := 2;
    end;

    NextToken;
  end
  else if Scanner.Token = toString then
  begin
    if Length(Scanner.StrValue) <> 1 then Error('Char expected');

    DataType := CreateSymbol(scSubrangeType, '');
    DataType^.DataType := dtChar;

    DataType^.Low := Ord(Scanner.StrValue[1]);
    NextToken;
    Expect(toRange);
    NextToken;
    Expect(toString);
    if Length(Scanner.StrValue) <> 1 then Error('Char expected');
    DataType^.High := Ord(Scanner.StrValue[1]);

    DataType^.Value := 1;

    NextToken;
  end
  else begin
    Expect(toIdent);

    Sym := LookupGlobalOrFail(Scanner.StrValue);

    if Sym^.Kind = scConst then
    begin
      DataType := CreateSymbol(scSubrangeType, '');
      DataType^.DataType := Sym^.DataType;
      DataType^.Value := 1;

      Sym := LookupGlobalOrFail(Scanner.StrValue);
      DataType^.Low := Sym^.Value;
      NextToken;
      Expect(toRange);
      NextToken;

      if Scanner.Token = toIdent then
      begin
        Sym := LookupGlobalOrFail(Scanner.StrValue);

        if Sym^.Kind <> scConst then Error('Not a constant');
        if Sym^.DataType <> dtInteger then Error('Not an integer');

        DataType^.High := Sym^.Value;

        if Sym^.Value < 256 then DataType^.Value := 1 else DataType^.Value := 2;
      end
      else
      begin
        Expect(toNumber);
        DataType^.High := Scanner.NumValue;

        if Scanner.NumValue < 256 then DataType^.Value := 1 else DataType^.Value := 2;
      end;

      NextToken;
    end
    else
    begin
      while Sym^.Kind = scAliasType do Sym := Sym^.DataType;

      if not (Sym^.Kind in [scType, scArrayType, scRecordType, scEnumType, scStringType, scSetType, scPointerType, scSubrangeType, scFileType]) then
        Error('Not a type: ' + Scanner.StrValue);

      DataType := Sym;
      NextToken;
    end;
  end;

  ParseTypeDef := DataType;
end;

(**
 * Parses a type reference, which is basically the name of a built-in or
 * user-defined type. Needed for parameters. These are not allowed to come
 * up with new (and hence anonymous) types.
 *)
function ParseTypeRef: PSymbol;
var
  DataType: PSymbol;
begin
  if Scanner.Token = toStringKW then
  begin
    DataType := dtString;
  end
  else
  begin
    Expect(toIdent);
    DataType := LookupGlobalOrFail(Scanner.StrValue);

    if not (DataType.Kind in [scType, scArrayType, scRecordType, scEnumType, scStringType, scSetType, scSubrangeType, scPointerType]) then Error('Type expected');
  end;

  NextToken;

  ParseTypeRef := DataType;
end;

(**
 * Parses a single variable or parameter name.
 *
 * TODO Is this worth its own procedure? Only used by variable and parameter lists.
 *)
procedure ParseVar;
var
  Name: String;
begin
  Expect(toIdent);
  Name := Scanner.StrValue;
  NextToken;

  CreateSymbol(scVar, Name);
  (*
  if Sym^.Level = 0 then
  begin
    Sym^.Tag := GetLabel('global');
    Emit0(Sym^.Tag, 'ds ' + IntToStr(Sym^.Bounds * 2), 'Global ' + Sym^.Name);
  end;
  *)
end;

(**
 * Parses a variable list (actually this should probably be called a variable
 * group), that is, a comma separated list of variable identifiers followed by a
 * type name or definition.
 *)
procedure ParseVarList();
var
  Old: PSymbol;
  DataType, Sym: PSymbol;
  Multi, IsAbs: Boolean;
  Tag: String;
begin
  Multi := False;
  IsAbs := False;

  Old := SymbolTable;
  ParseVar;
  while Scanner.Token = toComma do
  begin
    Multi := True;
    NextToken; ParseVar;
  end;

  Expect(toColon);
  NextToken;

  DataType := ParseTypeDef;

    // WriteLn('Var type is ', DataType^.Name);
    // if DataType^.Kind = scArrayType then WriteLn(' of ', DataType^.DataType^.Name);

  if Scanner.Token = toAbsolute then
  begin
    if Multi then Error('"absolute" only allowed for single variables');

    NextToken;
    if Scanner.Token = toNumber then
      Tag := IntToStr(Scanner.NumValue)
    else if Scanner.Token = toString then
      Tag := Scanner.StrValue
    else if Scanner.Token = toIdent then
    begin
      Sym := LookupGlobalOrFail(Scanner.StrValue);
      if Sym^.Tag = '' then Error('Not addressable');
      Tag := Sym^.Tag;
    end
    else Error('Address expected');

    NextToken;
    IsAbs := True;
  end;

  while Old<>SymbolTable do
  begin
    Old := Old^.Next;
    if Old^.Kind = scVar then
    begin
      if AbsCode and (DataType^.Value <= 32) then Old^.Level := 0;

      SetDataType(Old, DataType);

      // TODO Should this be part of SetDataType?
      if IsAbs then Old^.Tag := Tag
      else if Old^.Level = 0 then
      begin
        Old^.Tag := GetLabel('global');
        Emit(Old^.Tag, 'ds ' + IntToStr(Old^.DataType^.Value) + ',0', 'Global ' + Old^.Name);
      end;
    end;

//    Tmp^.DataType := DataType;
    //if High >= Low then Tmp^.Bounds := High - Low + 1;
//    Tmp := Tmp^.Prev;
    //Offset := Offset + (High - Low + 1) * 2 - 2;
  end;
end;

(**
 * Parses a parameter list (actually this should probably be called a parameter
 * group), that is, a comma-separated list of variable names followed by a
 * single type reference. Whether these are "var" parameters or not is being
 * passed as a parameter.
 *)
procedure ParseParamList(IsRef: Boolean);
var
  Old: PSymbol;
  DataType: PSymbol;
begin
  Old := SymbolTable;
  ParseVar;
  SymbolTable^.IsRef := IsRef;
  while Scanner.Token = toComma do
  begin
    NextToken; ParseVar;
    SymbolTable^.IsRef := IsRef;
  end;

  if not IsRef or (Scanner.Token = toColon) then
  begin
    Expect(toColon);
    NextToken;
    DataType := ParseTypeRef;
  end
  else DataType := nil;

  (*if (DataType^.Kind in [scArrayType, scRecordType]) and not isRef then
    Error('Structured parameters must be passed by reference');*)

  while Old<>SymbolTable do
  begin
    Old := Old^.Next;
    if Old^.Kind = scVar then
      SetDataType(Old, DataType);
  end;
end;

(**
 * Parses a label definition, which can be numeric or alphanimeric.
 *)
procedure ParseLabelDef;
var
  S: PSymbol;
begin
  if Scanner.Token = toIdent then
  begin
    S := CreateSymbol(scLabel, Scanner.StrValue);
  end
  else if Scanner.Token = toNumber then
  begin
    S := CreateSymbol(scLabel, IntToStr(Scanner.NumValue));
  end
  else Error('Identifier or number expected');

  S^.Tag := GetLabel('label');
  NextToken;
end;

procedure ParseBlock(Sym: PSymbol); forward;

(**
 * Parses a procedure or function declaration. It can be either a forward
 * declaration or the actual implementation, in which case the corresponding
 * block is parsed, too.
 *)
procedure ParseProcFunc(Sym: PSymbol);
var
  NewSym, ResVar, FwdSym, P: PSymbol;
  Token: TToken;
  S: String;
  IsRef: Boolean;
begin
  Token := Scanner.Token;
  NextToken; Expect(toIdent);

  FwdSym := Lookup(Scanner.StrValue, SymbolTable, CurrentScope);
  if (FwdSym <> nil) and (FwdSym^.IsForward) then
  begin
    if Token = toProcedure then
      NewSym := CreateSymbol(scProc, '')
    else
      NewSym := CreateSymbol(scFunc, '');

    if NewSym^.Kind <> FwdSym^.Kind then Error('Proc/Func mismatch');

    FwdSym^.IsForward := False;

    OpenScope(True);

    NewSym^.Tag := FwdSym^.Tag;
    if FwdSym^.SavedParams <> nil then
    begin
      P := FwdSym^.SavedParams;
      while P^.Prev <> nil do
      begin
        P := P^.Prev;
      end;
      P^.Prev := SymbolTable;
      SymbolTable^.Next := P;
      SymbolTable := FwdSym^.SavedParams;
    end;

    NextToken;
    Expect(toSemicolon);
    NextToken;

    NewSym := FwdSym;
  end
  else
  begin
    if Token = toProcedure then
    begin
      NewSym := CreateSymbol(scProc, Scanner.StrValue);
      NewSym^.Tag := GetLabel('__' + NewSym^.Name);
    end
    else
    begin
      NewSym := CreateSymbol(scFunc, Scanner.StrValue);
      NewSym^.Tag := GetLabel('__' + NewSym^.Name);
    end;

    OpenScope(True);
  end;

  NewSym^.IsStdCall := True;

  if FwdSym = nil then
  begin
    if Token = toFunction then
    begin
      CreateSymbol(scVar, Scanner.StrValue)^.Tag := 'RESULT';
      //SetDataType(SymbolTable, dtInteger, 0);
      ResVar := SymbolTable;
    end;
    NextToken;

    if Scanner.Token = toLParen then
    begin
      NextToken;

      IsRef := False;
      if Scanner.Token = toVar then
      begin
        IsRef := True;
        NextToken;
      end;

      ParseParamList(IsRef);
      while Scanner.Token = toSemicolon do
      begin
        NextToken;

        IsRef := False;
        if Scanner.Token = toVar then
        begin
          IsRef := True;
          NextToken;
        end;

        ParseParamList(IsRef); (* ParamGroup *)
      end;
      Expect(toRParen);
      NextToken;
    end;

    if NewSym^.Kind = scFunc then
    begin
      Expect(toColon);
      NextToken;

      NewSym^.DataType := ParseTypeRef();
      ResVar^.DataType := NewSym^.DataType;
    end;

    AdjustOffsets;

    Expect(toSemicolon);
    NextToken;

    while Scanner.Token in [toExternal,toForward,toIdent] do
    begin
      if Scanner.Token = toExternal then
      begin
        NextToken;
        NewSym^.IsExternal := True;
        if Scanner.Token = toString then
        begin
          NewSym^.Tag := Scanner.StrValue;
          NextToken;
        end
        else
          NewSym^.Tag := '__' + LowerStr(NewSym^.Name);
      end
      else if Scanner.Token = toForward then
      begin
        NewSym^.IsForward := True;
        NextToken;
      end
      else
      begin
        S := LowerCase(Scanner.StrValue);
        if S = 'stdcall' then
          NewSym^.IsStdCall := True
        else if S = 'register' then
          NewSym^.IsStdCall := False
        else Error('Unknown modifier ' + S);

        NextToken;
      end;

      Expect(toSemicolon);
      NextToken;
    end;

    if NewSym^.IsForward then
    begin
      if SymbolTable^.Kind = scScope then
      begin
        NewSym^.SavedParams := nil
      end
      else
      begin
        NewSym^.SavedParams := SymbolTable;
        NewSym^.Next^.Next^.Prev := nil;
        NewSym^.Next^.Next := nil;
        SymbolTable := NewSym^.Next;
      end;
    end
  end;

  if not NewSym^.IsExternal and not NewSym^.IsForward then
  begin
    if (Scanner.Token = toInline) then
    begin
      Emit(NewSym.Tag, '', '');
      NextToken;
      ParseInline;
      EmitI('ret');
      Expect(toSemicolon);
      NextToken;
    end
    else
    begin
      ParseBlock(NewSym);
      Expect(toSemicolon);
      NextToken;
    end;
  end;

  CloseScope(True);

  EmitC('');
end;

(**
 * Parses a declaration part, which can contain the following elements any
 * number of times and in any order: constants, types, variables, labels,
 * procedures, and functions.
 *)
procedure ParseDeclarations(Sym: PSymbol);
var
  OldSyms, P, T: PSymbol;
  Name: String;
begin
  while Scanner.Token in [toConst, toType, toVar, toLabel, toOverlay, toProcedure, toFunction] do
  begin
    if Scanner.Token = toConst then
    begin
      NextToken;
      repeat
        ParseConst;
        while Scanner.Token = toComma do
        begin
          NextToken; ParseConst;
        end;
        Expect(toSemicolon);
        NextToken;
      until Scanner.Token <> toIdent;
    end

    else if Scanner.Token = toType then
    begin
      OldSyms := SymbolTable;

      NextToken;
      Expect(toIdent);

      repeat
        Name := Scanner.StrValue;

        if Lookup(Name, SymbolTable, CurrentScope) <> nil then
          Error('Duplicate identifier "' + Name + '"');

        NextToken;
        Expect(toEq);
        NextToken;

        T := ParseTypeDef;
        if T^.Name <> '' then
        begin
          P := CreateSymbol(scAliasType, Name);
          P^.DataType := T;
        end
        else T^.Name := Name;

        Expect(toSemicolon);
        NextToken;
      until Scanner.Token <> toIdent;

      P := SymbolTable;
      while P <> OldSyms do
      begin
        if (P^.Kind = scPointerType) and (P^.DataType = nil) then
        begin
          P^.DataType := Lookup(P^.Tag, SymbolTable, CurrentScope);
          if P^.DataType = nil then Error('Unresolved forward declaration "' + P^.Name + '"');
          P^.Tag := '';
        end;

        P := P^.Prev;
      end;
    end

    else if Scanner.Token = toVar then
    begin
      NextToken;
      repeat
        parseVarList();
        Expect(toSemicolon);
        NextToken;
      until Scanner.Token <> toIdent;
    end

    else if Scanner.Token = toLabel then
    begin
      NextToken;
      parseLabelDef();
      while Scanner.Token = toComma do
      begin
        NextToken;
        parseLabelDef();
      end;
      Expect(toSemicolon);
      NextToken;
    end

    else if Scanner.Token = toOverlay then
    begin
      repeat
        NextToken;

        if not (Scanner.Token = toProcedure) or (Scanner.Token = toFunction) then
          Error('Procedure or function expected');

        if Level <> 0 then
          Error('Overlays only allowed on top level');

        ParseProcFunc(Sym);
      until Scanner.Token <> toOverlay;
    end

    else if (Scanner.Token = toProcedure) or (Scanner.Token = toFunction) then
      ParseProcFunc(Sym);
  end;
end;

(**
 * Parses a statement list, which is a (possibly emtpy) list of statements
 * separated by semicolons.
 *)
procedure ParseStatementList(ContTarget, BreakTarget: String);
begin
  ParseStatement(ContTarget, BreakTarget);
  while Scanner.Token = toSemicolon do
  begin
    NextToken;
    ParseStatement(ContTarget, BreakTarget);
  end;
end;

(**
 * Parses a block, which is a set of declarations followed by a statement list
 * enclose in "begin" and "end".
 *)
procedure ParseBlock(Sym: PSymbol);
var
  PrevBlock: PSymbol;
begin
  if SmartLink and (Sym <> nil) and (Sym^.Level = 0) then EmitI('if __USE' + Sym^.Tag);

  ParseDeclarations(Sym);

  //if Sym = nil then
    //WriteLn('Entering level ', Sym^.Level , ' block ', Sym^.Name)
  //else
  //begin
    //Writeln('Entering main block');
    //MainBlock := True;
  //end;

  ExitTarget := GetLabel('exit');
  EmitPrologue(Sym);

  Expect(toBegin);
  NextToken;

  if SmartLink and (Sym <> nil) and (Sym^.Level = 0) then
  begin
  PrevBlock := CurrentBlock;
  CurrentBlock := Sym;
  end;

  ParseStatementList('', '');

  if SmartLink and (Sym <> nil) and (Sym^.Level = 0) then
  begin
  CurrentBlock := PrevBlock;
  end;

  //if Sym <> nil then
    //WriteLn('Leaving level ', Sym^.Level , ' block ', Sym^.Name)
  //else
    //Writeln('Leaving main block');

  Expect(toEnd);
  NextToken;

  EmitEpilogue(Sym);

  if SmartLink and (Sym <> nil) and (Sym^.Level = 0) then EmitI('endif');
end;

(**
 * Parses the main program. This includes automatic inclusion of the correct
 * system library (which needs its own "end." so we know when exactly the actual
 * program code starts).
 *)
procedure ParseProgram;
begin
  OpenScope(False);
  RegisterAllBuiltIns;

  if Binary = btCPM then
    OpenInput(HomeDir + '/rtl/cpm.pas')
  else if Binary = btZX then
    OpenInput(HomeDir + '/rtl/zx.pas')
  else if Binary = btZX128 then
    OpenInput(HomeDir + '/rtl/zx.pas')
  else
    OpenInput(HomeDir + '/rtl/next.pas');

  NextToken;
  ParseDeclarations(nil);

  Expect(toEnd);
  NextToken;
  Expect(toPeriod);
  NextToken;

  LastBuiltIn := SymbolTable;

  OpenScope(False);
  if Scanner.Token = toProgram then
  begin
    NextToken;
    Expect(toIdent);
    NextToken;
    Expect(toSemicolon);
    NextToken;
  end;
  parseBlock(nil);
  Expect(toPeriod);
(*
  EmitC('');
  Emit('globals', 'ds ' + IntToStr(Offset), 'Globals');
*)
  EmitStrings();
  EmitC('');
  Emit('display', 'ds 16,0', 'Display');
  EmitC('');
  Emit('eof', '', 'End of file');

  CloseScope(False);
  CloseScope(False);
end;

var
  SrcFile, AsmFile, BinFile: String;

(**
 * Performs a build. All relevant information is assumed to be in the respective
 * global variables. The procedure does everything up to and including a
 * possible conversion to a specialized file format.
 *)
function Build: Integer;
var
  Dir: String;
  Org, Len, HeapStart, HeapBytes: Integer;
begin
  Dir := FExpand('.');

  AsmFile := ChangeExt(SrcFile, '.z80');

  if Binary = btCPM then
    BinFile := ChangeExt(SrcFile, '.com')
  else if (Format = tfBinary) or (Format = tfPlus3Dos) then
    BinFile := ChangeExt(SrcFile, '.bin')
  else if Format = tfTape then
    BinFile := ChangeExt(SrcFile, '.tap')
  else if Format = tfSnapshot then
    BinFile := ChangeExt(SrcFile, '.sna');

  if SetJmp(StoredState) = 0 then
  begin
    HasStoredState := True;

    Build := 1;

    WriteLn('Compiling...');
    WriteLn('  ', PosixToNative(FRelative(SrcFile)),
            ' -> ', PosixToNative(FRelative(AsmFile)));

    ErrorLine := 0;
    ErrorColumn := 0;
    Level := 0;
    Offset := 0;
    Scanner.Token := toNone;
    while Source <> nil do CloseInput;

    C := #0;

    while SymbolTable <> nil do CloseScope(True);

    CurrentScope := nil;
    LastBuiltIn := nil;

    ClearStrings;

    Source := nil;

    Code := nil;

    AbsCode := True;
    CheckBreak := False;
    IOMode := False;
    StackMode := False;

    OpenInput(SrcFile);
    OpenTarget(AsmFile);
    EmitHeader(HomeDir, SrcFile);  (* TODO Move this elsewhere. *)

    ParseProgram;

    EmitFooter(BinFile);           (* TODO Move this elsewhere. *)
    CloseTarget();
    CloseInput();

    Build := 2;

    CopyFile(HomeDir + '/misc/loader.tap', BinFile);

    WriteLn('Assembling...');
    WriteLn('  ', PosixToNative(FRelative(AsmFile)), ' -> ', PosixToNative(FRelative(BinFile)));
    WriteLn;

    //Exec(ZasmCmd,  '-w ' + AsmFile + ' ' + BinFile);
    if Binary = btZXN then
      Execute(SjAsmCmd,  '--zxnext --syntax=abf --nologo --msg=err ' + AsmFile)
    else
      Execute(SjAsmCmd, '--syntax=abf --nologo --msg=err ' + AsmFile);

    if DosError <> 0 then
      Error('Error ' + IntToStr(DosError) + ' starting assembler');
    if DosExitCode <> 0 then
      Error('Failure! :(');

    if (Format <> tfTape) and (Format <> tfSnapshot) then
    begin
    if Binary = btCPM then Org := 256 else Org := 32768;
    Len := FSize(BinFile);

    HeapStart := Org + Len;
    if HeapStart < 24576 then HeapStart := 24576;
    HeapBytes := 57343 - HeapStart;
    if HeapBytes < 0 then HeapBytes := 0;

    WriteLn('Code : $', IntToHex(Org, 4), '-$', IntToHex(Org + Len - 1, 4), ' (', Len:5, ' bytes)');
    WriteLn('Heap : $', IntToHex(HeapStart, 4), '-$', IntToHex(57343, 4), ' (', HeapBytes:5, ' bytes)');
    WriteLn('Stack: $', IntToHex(57344, 4), '-$FFFF ( 8192 bytes)');
    end;
  end;

  HasStoredState := False;
end;

(* --- Interactive Menu --- *)

const
  (**
   * Printable names of supported platforms. Must be aligned with TBinaryType.
   *)
  BinaryStr: array[TBinaryType] of String = ('CP/M', 'ZX 48K', 'ZX 128K', 'ZX Next');

  (**
   * Printable names of supported formats. Must be aligned with TTargetFormat.
   *)
  FormatStr: array[TTargetFormat] of String = ('Raw binary', '+3DOS binary', 'Tape file', 'Snapshot');

  (**
   * Yes/no strings. Why is this here and not in the IDE sestion?
   *)
  YesNoStr: array[Boolean] of String = ('No ', 'Yes');

var
  MainFile, WorkFile: String;

(**
 * Reads a key from console.
 *)
function GetKey: Char;
var
  C: Char;
  K: TKeyEvent;
begin
  InitKeyboard;
  WriteLn;
  Write('>');

  repeat
    K:=GetKeyEvent;
    K:=TranslateKeyEvent(K);
  until GetKeyEventFlags(K) = kbASCII;

  C := GetKeyEventChar(K);

  WriteLn(C);
  GetKey := C;
  DoneKeyboard;
end;

(**
 * Asks the user for a filename.
 *)
function GetFile(const S: String): String;
var
  T: String;
begin
  Write(S);
  ReadLn(T);
  if Length(T) <> 0 then
  begin
    if Pos('.', T) = 0 then T := T + '.pas';
    GetFile := FAbsolute(NativeToPosix(T));
  end;
end;

(**
 * Changes the current directory.
 *)
procedure DoDirectory;
var
  Dir: String;
begin
  Write('New directory: ');
  ReadLn(Dir);
  ChDir(Dir);
end;

(**
 * Changes the project's main file.
 *)
procedure DoMainFile;
begin
  MainFile := GetFile('Main file name: ');
end;

(**
 * Changes the current work file.
 *)
procedure DoWorkFile;
begin
  WorkFile := GetFile('Work file name: ');
end;

(**
 * Starts the editor. Asks for a file name, if necessary. Jumps to the given
 * line and column if these are non-zero (after a compile error).
 *)
procedure DoEdit(Line, Column: Integer);
var
  S: String;
begin
  if (WorkFile = '') and (MainFile = '') then DoWorkFile;

  if WorkFile <> '' then
    S := WorkFile
  else if MainFile <> '' then
    S := MainFile
  else
    Exit;

  if AltEditor then
  begin
    if (Line <> 0) and (Column <> 0) then
      Exec(CodeCmd, '-g ' + S + ':' + IntToStr(Line) + ':' + IntToStr(Column))
    else
      Exec(CodeCmd, S)
  end
  else
  begin
    if (Line <> 0) and (Column <> 0) then
      Exec(NanoCmd, '--minibar -Aicl --rcfile ' + HomeDir + '/misc/pascal.nanorc +' + IntToStr(Line) + ',' + IntToStr(Column) + ' ' + S)
    else
      Exec(NanoCmd, '--minibar -Aicl --rcfile ' + HomeDir + '/misc/pascal.nanorc ' + S);
  end;
end;

(**
 * Starts a compilation. Asks for a file name if necessary. In case of an error
 * during the Pascal to Assembly translation opens the editor with the Pascal
 * file and jumps to the error position.
 *)
procedure DoCompile;
begin
  if (WorkFile = '') and (MainFile = '') then DoWorkFile;

  if MainFile <> '' then
    SrcFile := MainFile
  else if WorkFile <> '' then
    SrcFile := WorkFile
  else
    Exit;

  if Build = 1 then
  begin
    if not AltEditor then GetKey;
    DoEdit(ErrorLine, ErrorColumn);
  end
end;

(**
 * Runs a compiles program by invoking a suitable emulator for the current
 * target platform.
 *)
procedure DoRun(Alt: Boolean);
begin
  if Length(BinFile) <> 0 then
  begin
    if Binary = btCPM then
    begin
      if Alt then
        Exec(TnylpoCmd, '-soy -t @ ' + BinFile)
      else
        Exec(TnylpoCmd, BinFile)
    end
    else if Binary = btZX then
    begin
      {$ifdef darwin}
      if Alt then
        Exec('/usr/bin/open', '-a Fuse --args --debugger-command "br 32768" --tape ' + ChangeExt(BinFile, '.tap'))
      else
        Exec('/usr/bin/open', '-a Fuse --args --debugger-command "del" --tape ' + ChangeExt(BinFile, '.tap'))
      {$else}
      if Alt then
        Exec(FuseCmd, ' --debugger-command "br 32768" --tape ' + ChangeExt(BinFile, '.tap'))
      else
        Exec(FuseCmd, ' --debugger-command "del" --tape ' + ChangeExt(BinFile, '.tap'))
      {$endif}
    end
    else
    begin
// FIXME CSpect / Zesarux
//      Exec('/Users/joerg/Library/bin/hdfmonkey', 'put /Users/joerg/Downloads/tbblue.mmc ' + BinFile + ' /autoexec.dot');
//      if Alt then
//        Exec('/Library/Frameworks/Mono.framework/Versions/Current/Commands/mono', '/Users/joerg/Downloads/CSpect2_16_5/CSpect.exe -zxnext -w4 -r -brk -nextrom -mouse -sound -mmc=/Users/joerg/Downloads/tbblue.mmc')
//      else
//        Exec('/Library/Frameworks/Mono.framework/Versions/Current/Commands/mono', '/Users/joerg/Downloads/CSpect2_16_5/CSpect.exe -zxnext -w4 -r -nextrom -mouse -sound -mmc=/Users/joerg/Downloads/tbblue.mmc')
    end;
  end;
end;

(**
 * Runs a shell command without leaving interactive mode.
 *)
procedure DoShell(Alt: Boolean);
var
  Cmd: String;
begin
  if Alt then Exec('/bin/bash', '')
  else
  begin
    Write('Command: ');
    ReadLn(Cmd);
    Exec('/bin/bash', '-c "' + Cmd + '"');
  end;
end;

(**
 * Lists files in the current directory that match a given pattern.
 *)
procedure DoFiles;
var
  Pattern, S: String;
  Dir: SearchRec;
  I: Integer;
begin
  Write('Mask: ');
  ReadLn(Pattern);
  {$ifdef windows}
  if Pattern = '' then Pattern := '*.*';
  {$else}
  if Pattern = '' then Pattern := '*';
  {$endif}

  I := 0;
  FindFirst(Pattern, Archive + Directory, Dir);
  while DosError = 0 do
  begin
    I := I + 1;
    if I = 5 then
    begin
      WriteLn;
      I := 0;
    end;

    S := Dir.Name;
    if Dir.Attr and Directory <> 0 then
      S := '[' + S + ']';
    Write(S + Space(15 - Length(S)) + ' ');
    FindNext(Dir);
  end;

  FindClose(Dir);
  WriteLn;
end;

(**
 * Translates a given string into a string with highlights for VT100. Basically,
 * any character following a ~ will be highlighted. Used for displaying menus.
 *)
function TermStr(S: String): String;
var
  T: String;
  I: Integer;
begin
  T := '';
  I := 1;
  while I <= Length(S) do
  begin
    C := S[I];
    if C = '~' then
    begin
      T := T + #27'[1m' + S[I + 1] + #27'[m';
      Inc(I);
    end
    else T := T + C;

    Inc(I);
  end;

  TermStr := T;
end;

(**
 * Implements the options menu of interactive mode, which is where the user can
 * select the target platform and change compiler switches.
 *)
procedure DoOptions;
var
  C: Char;
begin
  repeat
    Write(#27'[2J'#27'[H');

    WriteLn(TermStr('~Target machine     : '), BinaryStr[Binary]);
    WriteLn(TermStr('Output ~Format      : '), FormatStr[Format]);
    WriteLn(TermStr('Peephole ~optimizer : '), YesNoStr[Optimize]);
    WriteLn(TermStr('~Dependency analysis: '), YesNoStr[SmartLink]);
    WriteLn;
    WriteLn(TermStr('~Back'));

    C := GetKey;
    case C of
      't': if Binary = btZXN then Binary := btCPM else Binary := Succ(Binary);
      'f': if Format = tfSnapshot then Format := tfBinary else Format := Succ(Format);
      'o': Optimize := not Optimize;
      'd': SmartLink := not SmartLink;
    end;
  until C = 'b';
end;

(**
 * Shows the copyright header.
 *)
procedure Copyright(Ide: Boolean);
begin
  WriteLn('----------------------------------------');
  if Ide then
    WriteLn(#27'[1m', 'PASTA/80 Pascal System', #27'[m', 'Version ' + Version:18)
  else
    WriteLn('PASTA/80 Pascal System', 'Version ' + Version:18);

  if Binary = btZXN then
    WriteLn(BinaryStr[Binary] + ', Z80N':40)
  else
    WriteLn(BinaryStr[Binary] + ', Z80':40);
  WriteLn;
  WriteLn('Copyright (C) 2020-25 by  Joerg Pleumann');
  WriteLn('----------------------------------------');
  WriteLn;
end;

(**
 * Implements the main screen/menu of interactive mode.
 *)
procedure Interactive;
var
  C: Char;
  I: Integer;
begin
  while True do
  begin
    Write(#27'[2J'#27'[H');

    Copyright(True);

    WriteLn(TermStr('~Active directory: '), FExpand('.'));
    WriteLn;

    Write(TermStr('~Work file: '), PosixToNative(FRelative(WorkFile)));
    if WorkFile <> '' then
    begin
      I := FSize(WorkFile);
      if I = -1 then WriteLn(' (new file)') else WriteLn(' (', I, ' bytes)');
    end
    else WriteLn;

    Write(TermStr('~Main file: '), PosixToNative(FRelative(MainFile)));
    if MainFile <> '' then
    begin
      I := FSize(MainFile);
      if I = -1 then WriteLn(' (new file)') else WriteLn(' (', I, ' bytes)');
    end
    else WriteLn;

    WriteLn;
    WriteLn(TermStr('~Edit      ~Compile   ~Run       '));
    WriteLn(TermStr('~Shell     ~Files     ~Options   ~Quit'));

    while True do
    begin
      C := GetKey;

      case C of
        'a': DoDirectory;
        'm': DoMainFile;
        'w': DoWorkFile;
        'e': DoEdit(0, 0);
        'c': DoCompile;
        'r', 'R': DoRun(C = 'R');
        's', 'S': DoShell(C = 'S');
        'f': DoFiles;
        'o', 'O': DoOptions;
        'q': begin WriteLn('Bye!'); WriteLn; Halt(0); end;
        else Break;
      end;
    end;
  end;
end;

(**
 * Process command-line parameters and, ultimately, either start a build or
 * enter interactive mode.
 *)
procedure Parameters;
var
  Ide: Boolean;
  I: Integer;
begin
  if ParamCount = 0 then
  begin
    WriteLn('Usage:');
    WriteLn('  pasta80 { <option> } <input>');
    WriteLn;
    WriteLn('Options:');
    WriteLn('  --cpm          Sets target to CP/M (default)');
    WriteLn('  --zx48         Sets target to ZX Spectrum 48K');
    WriteLn('  --zx128        Sets target to ZX Spectrum 128K');
    WriteLn('  --zxnext       Sets target to ZX Spectrum Next');
    WriteLn;
    WriteLn('  --bin          Generates raw binary file (default)');
    WriteLn('  --3dos         Generates binary with +3DOS header');
    WriteLn('  --tap          Generates .tap file with loader');
    WriteLn('  --sna          Generates .sna snapshot file');
    WriteLn;
    WriteLn('  --dep          enable dependency analysis');
    WriteLn('  --opt          enable peephole optimizations');
    WriteLn;
    WriteLn('  --ide          starts interactive mode');
    WriteLn('  --version      shows just the version number');
    WriteLn;
    Halt(1);
  end;

  Ide := False;

  I := 1;
  SrcFile := ParamStr(I);
  while Copy(SrcFile, 1, 2) = '--' do
  begin
    if SrcFile = '--cpm' then
      Binary := btCPM
    else if SrcFile = '--zx48' then
      Binary := btZX
    else if SrcFile = '--zx128' then
      Binary := btZX128
    else if SrcFile = '--zxnext' then
      Binary := btZXN
    else if SrcFile = '--bin' then
      Format := tfBinary
    else if SrcFile = '--3dos' then
      Format := tfPlus3Dos
    else if SrcFile = '--tap' then
      Format := tfTape
    else if SrcFile = '--sna' then
      Format := tfSnapshot
    else if SrcFile = '--opt' then
      Optimize := True
    else if SrcFile = '--dep' then
      SmartLink := True
    else if SrcFile = '--ide' then
      Ide := True
    else
      Error('Invalid option: ' + SrcFile);

    I := I + 1;
    SrcFile := ParamStr(I);
  end;

  if SrcFile = '' then
  begin
    if not Ide then Error('No input file');
  end
  else
  begin

    SrcFile := NativeToPosix(SrcFile);
//    WriteLn(SrcFile);
    if Pos('.', SrcFile) = 0 then SrcFile := SrcFile + '.pas';
//    if FSize(SrcFile) < 0 then Error('Input file does not exist');
    AsmFile := ChangeExt(SrcFile, '.z80');
  end;

  if (Binary = btCPM) and (Format <> tfBinary) or
     (Binary = btZXN) and (Format = tfSnapshot) then
    Error('Invalid machine/format combination');

  if Ide then
  begin
    if SrcFile <> '' then WorkFile := SrcFile;
    Interactive;
  end
  else Build;

  WriteLn;
end;

(* -------------------------------------------------------------------------- *)
(* --- Main program --------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

begin
  if ParamStr(1) = '--version' then
  begin
    WriteLn(Version);
    Halt;
  end;

  Copyright(False);
  LoadConfig;
  Parameters;
end.

(*
TODO
- Record typed constants
- Too many arguments
- var X absolute $1234
- var X absolute Y;
- Complex types on the stack (parameters, locals), what does Turbo 3 allow?
- Enums as array indices
- Alternative array syntax
- Subrange types
- Pointers & heap management
- Allow assignment from Byte to Integer (TypeCheck probably needs to return type)
*)
