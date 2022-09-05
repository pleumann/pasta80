program PL0;

{$Mode delphi}

uses
  Keyboard, Dos, Math;

(* -------------------------------------------------------------------------- *)
(* --- Utility functions ---------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

function UpperStr(S: String): String;
var
  I: Integer;
begin
  for I := 1 to Length(S) do S[I] := UpCase(S[I]);
  UpperStr := S;  
end;

function LowerStr(S: String): String;
var
  I: Integer;
begin
  for I := 1 to Length(S) do S[I] := LowerCase(S[I]);
  LowerStr := S;  
end;

function Replace(S: String; C, D: Char): String;
var
  I: Integer;
begin
  for I := 1 to Length(S) do
    if S[I] = C then
      S[I] := D;
  Replace := S;
end;

function Int2Str(I: Integer (*); N: Integer*) ): String;
const
  N = 0;
var
  S: String;
begin
  Str(I, S);
  (*)
  if N < 0 then
    S := Replace(S, ' ', '0');
  *)
  Int2Str := S;
end;

function HexStr(Value: Integer; Digits: Integer): String;
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

  HexStr := S;
end;

(*
function EncodeReal(Src: String): String;
var
  S: Boolean;
  E, D, I: LongInt;
  R, M: Real;
  Res: String;
begin
  Val(Src, R);
  S := False;
  E := 0;
  M := 0;

  if R <> 0 then
  begin
    if R < 0 then
    begin
        S := True;
        R := -R;
    end;

    FRExp(R, M, E);
  end;

  E := E + 128;

  M := M * 256;
  D := Floor(M);
  if not S then D := D and not 128;
  M := Frac(M);
  Res := HexStr(D, 2);

  for I := 1 to 4 do
  begin
    M := M * 256;
    D := Floor(M);
    M := Frac(M);
    Res := Res + HexStr(D, 2);
  end;

  Res := Res + HexStr(E, 2);

  EncodeReal := '0x' + Copy(Res, 1, 4) + ',0x' + Copy(Res, 5, 4) + ',0x' + Copy(Res, 9, 4);
end;
*)

function AlignStr(S: String; N: Integer): String;
const
  Spaces = '                ';
begin
  if Length(S) >= Abs(N) then
    AlignStr := S
  else if N > 0 then
  begin
    while Length(S) < N do
      S := S + Spaces;
    AlignStr := Copy(S, 1, N);  
  end
  else begin
    N := -N;
    while Length(S) < N do
      S := Spaces + S;
    AlignStr := Copy(S, Length(S) - N + 1, N);
  end
end;

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

function StartsWith(S, T: String): Boolean;
begin
  if Length(S) < Length(T) then
  begin
    StartsWith := False;
    Exit;
  end;

  SetLength(S, Length(T));
  StartsWith := S = T;
end;

function ParentDir(Name: String): String;
var
  I: Integer;
begin
  I := Length(Name);
  while (I > 0) and (Name[I] <> '/') do
    I := I - 1;

  ParentDir := Copy(Name, 1, I - 1);
end;

function ChangeExt(Name, Ext: String): String;
var
  I: Integer;
begin
  I := Pos('.', Name);
  if I = 0 then
    ChangeExt := Name + Ext
  else
    ChangeExt := Copy(Name, 1, I-1) + Ext;
end;

function FRelative(Name, Dir: String): String;
begin
  Dir := Dir + '/';
  if Copy(Name, 1, Length(Dir)) = Dir then
    FRelative := Copy(Name, Length(Dir) + 1, 255)
  else
    FRelative := Name;  
end;

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

// Debug / Warning / Error

(* -------------------------------------------------------------------------- *)
(* --- Input ---------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

type 
  TSource = record
    Name: String;
    Input:  Text;
    Buffer: String;
  (*  Count:  Integer; *)
    Line:   Integer;
    Column: Integer;
  end;

var
  Source: array[Boolean] of TSource;
  Include: Boolean = False;
  StoredState: Jmp_Buf;
  TokenLine, TokenColumn, ErrorLine, ErrorColumn: Integer;

  AltEditor: Boolean;

procedure Error(Message: String);
var  I, L, C: Integer;
begin
  WriteLn;
  WriteLn(Source[Include].Buffer);
  for I := 1 to TokenColumn - 1 do Write(' ');
  WriteLn('^');
  WriteLn('*** Error at ', TokenLine, ',', TokenColumn, ': ', Message);
  ErrorLine := TokenLine;
  ErrorColumn := TokenColumn;
  WriteLn();

  LongJmp(StoredState, 1);
end;

procedure OpenInput(FileName: String);
begin
  with Source[Include] do
  begin
    Name := FileName;
    Assign(Input, FileName);
    Reset(Input);
    Buffer := '';
    Line := 0;
    Column := 1;
  end;
end;

procedure CloseInput();
begin
  with Source[Include] do
    Close(Input);
end;

procedure SetInclude(FileName: String);
begin
  if Include then
    Error('Nested includes not allowed');

  Include := True;

  OpenInput(FileName);
end;

procedure EmitC(S: String); forward;

function GetChar(): Char;
begin
  with Source[Include] do
  begin
    if Column > Length(Buffer) then
    begin
      if Eof(Input) then
      begin
        if Include then
        begin
          Close(Input);
          Include := False;
          GetChar := ' ';
          Exit;
        end
        else Error('Unexpected end of source');
        (* GetChar := #26;
        Exit; *)
      end;

      ReadLn(Input, Buffer);
      EmitC('[' + Int2Str(Line) + '] ' + Buffer);
      Buffer := Buffer + #13;
      Line := Line + 1;
      Column := 1;

      (* WriteLn('[', Line, '] ', Buffer); *)
    end;

    GetChar := Buffer[Column];
    (* Write(Source.Buffer[Source.Column]); *)
    Inc(Column);
  end;
end;

procedure UngetChar();
begin
  Source[Include].Column := Source[Include].Column - 1;
end; 
    
(* -------------------------------------------------------------------------- *)
(* --- String table --------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

function GetLabel(Prefix: String): String; forward;

type
  PStringLiteral = ^TStringLiteral;
  TStringLiteral = record
    Tag: String;
    Value: String;
    Next: PStringLiteral;
  end;

var
  Strings: PStringLiteral;

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

(* -------------------------------------------------------------------------- *)
(* --- Symbol table --------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

type
  TSymbolClass = (scConst, scType, scArrayType, scRecordType, scEnumType, scStringType, scSetType, scSubrangeType, scPointerType, scVar, scProc, scFunc, scScope);

  // Flags: Forward, External, Register, Magic, Reference, Writable, Relative, Sizable, Addressable

  PSymbol = ^TSymbol;
  TSymbol = record
    Name: String;
    Kind: TSymbolClass;
    DataType: PSymbol;
    ArgTypes: array[0..15] of PSymbol;
    ArgIsRef: array[0..15] of Boolean;
    Level: Integer;
    Value, Value2: Integer;
    StrVal: String;
    Tag: String;
    Bounds: Integer;
    Prev, Next: PSymbol;
    IsMagic: Boolean;
    IsRef: Boolean;
    IsStdCall: Boolean;
    IsForward: Boolean;
    IsExternal: Boolean;
    SavedParams: PSymbol;
  end;

var
  SymbolTable: PSymbol = nil;
  Level, Offset: Integer;
  dtInteger, dtBoolean, dtChar, dtByte, dtString, dtReal, dtPointer: PSymbol;

  AssertProc, BreakProc, ContProc, ExitProc, WriteProc, WriteLnProc: PSymbol;

  AbsFunc, AddrFunc, DisposeProc, EvenFunc, NewProc, OddFunc, OrdFunc, PredFunc,
  PtrFunc, SizeFunc, SuccFunc, BDosFunc, BDosHLFunc: PSymbol;

procedure OpenScope();
var
  Sym: PSymbol;
begin
  New(Sym);
  Sym^.Kind := scScope;
  Sym^.Prev := SymbolTable;
  if SymbolTable <> nil then SymbolTable^.Next := Sym;
  Sym^.Value := Offset;
  SymbolTable := Sym;

  Level := Level + 1;
  Offset := 0;
end;

procedure OpenWith();
var
  Sym: PSymbol;
begin
  New(Sym);
  Sym^.Kind := scScope;
  Sym^.Prev := SymbolTable;
  if SymbolTable <> nil then SymbolTable^.Next := Sym;
  Sym^.Value := Offset;
  SymbolTable := Sym;
end;

procedure Emit(Tag, Instruction, Comment: String); forward;

procedure CloseScope();
var
  Sym: PSymbol;
  Kind: TSymbolClass;
begin
  repeat
    Kind := SymbolTable^.Kind;

    if ((Kind = scProc) or (Kind = scFunc)) and (SymbolTable^.IsForward) then
      Error('Unimplemented forward proc/func: ' + SymbolTable^.Name);

    Sym := SymbolTable^.Prev;
    Offset := SymbolTable^.Value;
    Dispose(SymbolTable);
    SymbolTable := Sym;
  until Kind = scScope;

  Level := Level - 1;
end;

procedure CloseWith();
var
  Sym: PSymbol;
  Kind: TSymbolClass;
begin
  repeat
    Kind := SymbolTable^.Kind;

    if ((Kind = scProc) or (Kind = scFunc)) and (SymbolTable^.IsForward) then
      Error('Unimplemented forward proc/func: ' + SymbolTable^.Name);

    Sym := SymbolTable^.Prev;
    Offset := SymbolTable^.Value;
    Dispose(SymbolTable);
    SymbolTable := Sym;
  until Kind = scScope;
end;

function LookupLocal(Name: String): PSymbol;
var
  Sym: PSymbol;
begin
  Name := UpperStr(Name);
  Sym := SymbolTable;
  while Sym^.Kind <> scScope do
  begin
    if UpperStr(Sym^.Name) = Name then
    begin
      LookupLocal := Sym;
      Exit;
    end;
    Sym := Sym^.Prev;
  end;

  LookupLocal := nil;
end;

function LookupGlobal(Name: String): PSymbol;
var
  Sym: PSymbol;
begin
  Name := UpperStr(Name);
  Sym := SymbolTable;
  while Sym <> nil do
  begin
    if UpperStr(Sym^.Name) = Name then
    begin
      LookupGlobal := Sym;
      Exit;
    end;
    Sym := Sym^.Prev;
  end;

  LookupGlobal := nil;
end;

function FindField(Fields: PSymbol; Name: String): PSymbol;
var
  Sym: PSymbol;
begin
  Name := UpperStr(Name);
  Sym := Fields;
  while Sym <> nil do
  begin
    if UpperStr(Sym^.Name) = Name then
    begin
      FindField := Sym;
      Exit;
    end;
    Sym := Sym^.Prev;
  end;

  FindField := nil;
end;

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
      Sym^.Value := Sym^.Value - Offset + 6;    
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

function CreateSymbol(Kind: TSymbolClass; Name: String; Bounds: Integer): PSymbol;
var
  Sym: PSymbol;
begin
  if (Length(Name) <> 0) and (LookupLocal(Name) <> nil) then
  begin
    Error('Duplicate symbol "' + Name + '".');
  end;

  New(Sym);
  FillChar(Sym^, SizeOf(TSymbol), 0);
  Sym^.Kind := Kind;
  Sym^.Name := Name;
  Sym^.Level := Level;
  Sym^.Prev := SymbolTable;
  Sym^.Bounds := Bounds;
  Sym^.Tag := '';

  SymbolTable^.Next := Sym;
  SymbolTable := Sym;

(*
  if Kind = scVar then
  begin
    if Bounds = 0 then SIze := 2 else Size := 2 * Bounds;
    if Level = 1 then
    begin
      Sym^.Value := Offset;
      Offset := Offset + Size;
    end
    else
    begin
      Offset := Offset - Size;
      Sym^.Value := Offset + 2;
    end;
  end;
*)
  CreateSymbol := Sym;
end;

procedure SetDataType(Sym: PSymbol; DataType: PSymbol; Bounds: Integer);
var
  Size: Integer;
begin
  Sym^.DataType := DataType;
//  Sym^.Bounds := Bounds;

//  if Bounds = 0 then Size := 2 else Size := 2 * Bounds;
  if (Level = 1) and (Sym^.Tag = '') then
  begin
(* TODO This is be distinguished in a better way
    Sym^.Value := Offset;
    Offset := Offset + DataType^.Value;
*)
  end
  else
  begin
    if Sym^.IsRef then
      Offset := Offset - 2 (* var parameters *)
    else if DataType^.Value = 1 then 
      Offset := Offset - 2 (* Boolean, Char and Byte take 2 bytes on stack. *)
    else
      Offset := Offset - DataType^.Value;
    Sym^.Value := Offset;
  end;
end;

function RegisterType(Name: String; Size: Integer): PSymbol;
var
  Sym: PSymbol;
begin
  Sym := CreateSymbol(scType, Name, 0);
  Sym^.Value := Size;

  RegisterType := Sym;
end;

function NewEnumType(Name: String): PSymbol;
var
  Sym: PSymbol;
begin
  Sym := CreateSymbol(scEnumType, Name, 0);
  Sym^.Value := 1;
  NewEnumType := Sym;
end;

function NewConst(Name: String; DataType: PSymbol; Value: Integer): PSymbol;
var
  Sym: PSymbol;
begin
  Sym := CreateSymbol(scConst, Name, 0);
  Sym^.DataType := DataType;
  Sym^.Value := Value;
  NewConst := Sym;
end;

function RegisterBuiltIn(Kind: TSymbolClass; Name: String; Args: Integer; Tag: String): PSymbol;
var
  Sym: PSymbol;
begin
  Sym := CreateSymbol(Kind, Name, 0);
  Sym^.Level := 0;
  Sym^.Value := Args;
  Sym^.Tag := Tag;
  Sym^.IsStdCall := False;

  RegisterBuiltIn := Sym;
end;

function RegisterMagic(Kind: TSymbolClass; Name: String): PSymbol;
var
  Sym: PSymbol;
begin
  Sym := CreateSymbol(Kind, Name, 0);
  Sym^.IsMagic := True;
  RegisterMagic := Sym;
end;


procedure RegisterAllBuiltIns(Graphics: Boolean);
var
  Sym, Sym2: PSymbol;
begin
  dtInteger := RegisterType('Integer', 2);

  dtBoolean := NewEnumType('Boolean');
  NewConst('False', dtBoolean, 0);
  NewConst('True', dtBoolean, 1);

  dtChar := RegisterType('Char', 1);
  dtChar^.Value := 1;
  dtByte := RegisterType('Byte', 1);
  dtByte^.Value := 1;

  dtString := CreateSymbol(scStringType, 'String', 256);
  dtString^.Value := 256;

  dtReal := CreateSymbol(scType, 'Real', 6);
  dtReal^.Value := 6;

  dtPointer := CreateSymbol(scPointerType, 'Pointer', 2);
  dtPointer^.Value := 2;

  Sym := CreateSymbol(scArrayType, '', 65536);
  Sym^.DataType := dtByte;

  Sym2 := CreateSymbol(scVar, 'Mem', 0);
  Sym2^.DataType := Sym;
  Sym2^.Tag := '0';
  Sym2^.Value := 0;

  AssertProc := RegisterMagic(scProc, 'Assert');
  BreakProc := RegisterMagic(scProc, 'Break');
  ContProc := RegisterMagic(scProc, 'Continue');
  DisposeProc := RegisterMagic(scProc, 'Dispose');
  ExitProc := RegisterMagic(scProc, 'Exit');
  NewProc := RegisterMagic(scProc, 'New');
  WriteProc := RegisterMagic(scProc, 'Write');
  WriteLnProc := RegisterMagic(scProc, 'WriteLn');

  AbsFunc := RegisterMagic(scFunc, 'Abs');
  AddrFunc := RegisterMagic(scFunc, 'Addr');
  EvenFunc := RegisterMagic(scFunc, 'Even');
  OddFunc := RegisterMagic(scFunc, 'Odd');
  OrdFunc := RegisterMagic(scFunc, 'Ord');
  PredFunc := RegisterMagic(scFunc, 'Pred');
  PtrFunc := RegisterMagic(scFunc, 'Ptr');
  SizeFunc := RegisterMagic(scFunc, 'SizeOf');
  SuccFunc := RegisterMagic(scFunc, 'Succ');

  BDosFunc := RegisterMagic(scFunc, 'Bdos');
  BDosHLFunc := RegisterMagic(scFunc, 'BdosHL');

  Sym := RegisterBuiltIn(scProc, 'Poke', 2, '__poke');
  Sym^.ArgTypes[0] := dtInteger;
  Sym^.ArgTypes[1] := dtInteger;

  RegisterBuiltIn(scFunc, 'GetHeapStart', 0, '__get_heap_start')^.DataType := dtPointer;
//  RegisterBuiltIn(scFunc, 'GetHeapBytes', 0, '__get_heap_bytes')^.DataType := dtInteger;

  if Graphics then
  begin
    Sym := RegisterBuiltIn(scProc, 'SetPixel', 3, '__set_pixel');
    Sym^.ArgTypes[0] := dtInteger;
    Sym^.ArgTypes[1] := dtInteger;
    Sym^.ArgTypes[2] := dtInteger;
    Sym := RegisterBuiltIn(scFunc, 'GetPixel', 2, '__get_pixel');
    Sym^.ArgTypes[0] := dtInteger;
    Sym^.ArgTypes[1] := dtInteger;

    Sym := RegisterBuiltIn(scProc, 'SetFrontBuffer', 1, '__set_frontbuf');
    Sym^.ArgTypes[0] := dtInteger;
    Sym := RegisterBuiltIn(scProc, 'SetBackBuffer', 1, '__set_backbuf');
    Sym^.ArgTypes[0] := dtInteger;
    Sym := RegisterBuiltIn(scProc, 'WaitForVSync', 0, '__wait_vsync');
  end;
end;

(* --------------------------------------------------------------------- *)
(* --- Scanner --------------------------------------------------------- *)
(* --------------------------------------------------------------------- *)

type
  TToken = (toNone,
            toIdent, toNumber, toString, toFloat, toAdd, toSub, toMul, toDiv,
            toEq, toNeq, toLt, toLeq, toGt, toGeq, toLParen, toRParen, toLBrack, toRBrack,
            toBecomes, toComma, toColon, toSemicolon, toPeriod, toCaret, toRange,
            toAbsolute, toAnd, toArray, toBegin, toCase, toConst, toDivKw, toDo, toDownto, toElse, toEnd,
            toExternal, toFile, toFor, toForward, toFunction, toGoto, toIf, toIn, toInline, toLabel, toMod,
            toNil, toNot, toOf, toOr, toOverlay, toPacked, toProcedure, toProgram, toRecord, toRepeat, toSet,
            toShl, toShr, toStringKw, toThen, toTo, toType, toUntil, toVar, toWhile, toWith, toXor,
            toEof);

  TScanner = record
    Token:    TToken;
    StrValue: String;
    NumValue: Integer;
  end;

const
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

  FirstKeyword = toAbsolute;
  LastKeyword = toXor;

var
  Scanner: TScanner;

function IsIdentHead(C: Char): Boolean;
begin
  IsIdentHead := (C >= 'A') and (C <= 'Z') or (C >= 'a') and (C <= 'z') or (C = '_');
end;

function IsIdentTail(C: Char): Boolean;
begin
  IsIdentTail := IsIdentHead(C) or (C >= '0') and (C <= '9');
end;

function IsDecDigit(C: Char): Boolean;
begin
  IsDecDigit := (C >= '0') and (C <= '9');
end;

function IsHexDigit(C: Char): Boolean;
begin
  IsHexDigit := IsDecDigit(C) or (C >= 'A') and (C <= 'F') or (C >= 'a') and (C <= 'f');
end;

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
  C: Char;

procedure NextToken();
var
  S: String;
  I: Integer;
begin
  while (C <= ' ') do
  begin
    C := GetChar;
  end;

  with Scanner do
  begin
    Token := toNone;
    StrValue := '';
    NumValue := 0;
    TokenLine := Source[Include].Line;
    TokenColumn := Source[Include].Column - 1;

    if IsIdentHead(C) then
    begin
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
    else if (C = '''') or (C = '#') then
    begin
      Token := toString;

      repeat
        if C = '''' then
        begin
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
      S := '{';
      repeat
        C := GetChar;
        S := S + C;
      until C = '}';
      C := GetChar;

      if LowerStr(Copy(S, 2, 2)) = '$i' then
        SetInclude(TrimStr(Copy(S, 4, Length(S) - 4)));

      NextToken;
      Exit;
    end
    else 
    begin
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

      case Token of
        toLt:
          if C = '>' then
          begin
            Token := toNeq;
            C := GetChar;
          end
          else if C = '=' then
          begin
            Token := toLeq;
            C := GetChar;
          end;

        toGt:
          if C = '=' then
          begin
            Token := toGeq;
            C := GetChar;
          end;

        toLParen:
          if C = '.' then
          begin
            Token := toLBrack;
            C := GetChar;
          end
          else if C = '*' then
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

            if LowerStr(Copy(S, 3, 2)) = '$i' then
              SetInclude(TrimStr(Copy(S, 5, Length(S) - 6)));

            NextToken;
            Exit;
          end;

        toColon:
          if C = '=' then
          begin
            Token := toBecomes;
            C := GetChar;
          end;

        toPeriod:
          if C = ')' then
          begin
            Token := toRBrack;
            C := GetChar;
          end
          else if C = '.' then
          begin
            Token := toRange;
            C := GetChar;
          end;
      end;
    end;

    // WriteLn('{', Token, '->', StrValue , '}');
  end;
end;

procedure Expect(Token: TToken);
begin
  (* Write('<', Token, '/', Scanner.Token, '>'); *)
  if Scanner.Token <> Token then Error('Expected "' + TokenStr[Token] + '", but got "' + TokenStr[Scanner.Token] + '"');
end;

(* -------------------------------------------------------------------------- *)
(* --- Emitter -------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

type
  TBinaryType = (btCom, btDot);

  TGraphicsMode = (gmNone, gmLowRes, gmHighRes);

  PCode = ^TCode;
  TCode = record
    Tag, Instruction, Comment: String;
    Next, Prev: PCode;
  end;

const
  BinaryStr: array[TBinaryType] of String = ('CP/M COM', 'Next DOT');
  GraphicsStr: array[TGraphicsMode] of String = ('None', 'Lo-res', 'Hi-res');
  YesNoStr: array[Boolean] of String = ('No ', 'Yes');

var
  Binary: TBinaryType;
  Graphics: TGraphicsMode;
  Target: Text;
  NextLabel: Integer;
  Optimize: Boolean;
  ExitTarget: String;

  Code: PCode = nil;

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

procedure OpenTarget(Filename: String);
begin
  Assign(Target, Filename);
  Rewrite(Target);
end;

function GetLabel(Prefix: String): String;
begin
  GetLabel := Prefix + Int2Str(NextLabel);
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
    P := Pos(' ', Instruction);
    if P<>0 then
      Instruction := AlignStr(Copy(Instruction, 1, P-1), 8) + Copy(Instruction, P+1, 255);

    S := AlignStr(S, 16) + Instruction;
  end;

  if Comment <> '' then
    S := AlignStr(S, 40) + '; ' + Comment;

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

function DoOptimize: Boolean;
var
  TwoOp: String;
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

    if StartsWith(Prev^.Instruction, 'db ') and StartsWith(Code^.Instruction, 'db') then
    begin
      Prev^.Instruction := Prev^.Instruction + ',' + Copy(Code^.Instruction, 4, 255);
      RemoveCode;
      Exit;
    end;

    if StartsWith(Prev^.Instruction, 'dw ') and StartsWith(Code^.Instruction, 'dw') then
    begin
      Prev^.Instruction := Prev^.Instruction + ',' + Copy(Code^.Instruction, 4, 255);
      RemoveCode;
      Exit;
    end;

    TwoOp := Prev^.Instruction + '/' + Code^.Instruction;
    //if StartsWith(TwoOp, 'push af') then
    //  WriteLn(TwoOp);

    if (TwoOp = 'push hl/pop hl') or (TwoOp = 'push de/pop de') or (TwoOp = 'push af/pop af') then
    begin
      // WriteLn('*** Eliminate ', TwoOp);
      RemoveCode;
      RemoveCode;
      Exit;
    end
    else if TwoOp = 'push de/pop hl' then
    begin
      RemoveCode;
      Code^.Instruction := 'ld hl,de';
      // WriteLn('*** Replace ', TwoOp, ' with ', Code^.Instruction);
      Exit;
    end
    else if TwoOp = 'push de/pop bc' then
    begin
      RemoveCode;
      Code^.Instruction := 'ld bc,de';
      // WriteLn('*** Replace ', TwoOp, ' with ', Code^.Instruction);
      Exit;
    end
    else if TwoOp = 'push hl/pop de' then
    begin
      RemoveCode;
      Code^.Instruction := 'ld de,hl';
      // WriteLn('*** Replace ', TwoOp, ' with ', Code^.Instruction);
      Exit;
    end
    else if TwoOp = 'push af/pop de' then
    begin
      RemoveCode;
      Code^.Instruction := 'ld d,a';
      // WriteLn('*** Replace ', TwoOp, ' with ', Code^.Instruction);
      Exit;
    end
    else if TwoOp = 'push af/pop hl' then
    begin
      RemoveCode;
      Code^.Instruction := 'ld h,a';
      // WriteLn('*** Replace ', TwoOp, ' with ', Code^.Instruction);
      Exit;
    end
    else if TwoOp = 'pushfp/popfp' then
    begin
      RemoveCode;
      RemoveCode;
      // WriteLn('*** Replace ', TwoOp, ' with ', Code^.Instruction);
      Exit;
    end
    (*
    else if StartsWith(TwoOp, 'push hl/ld de,') then
    begin
      // WriteLn('*** Fast lane for ', Instruction, ' in ', TwoOp);
      Prev^.Instruction := Code^.Instruction;
      Code^.Instruction := 'push hl';
      Exit;
    end
    else if StartsWith(TwoOp, 'ld de,') and not StartsWith(TwoOp, 'ld de,hl') and (Code^.Instruction='pop hl') then
    begin
      // WriteLn('*** Fast lane for ', Code^.Instruction, ' in ', TwoOp);
      Code^.Instruction := Prev^.Instruction;
      Prev^.Instruction := 'pop hl';
      Exit;
    end;
    *)
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
  Flush;
  WriteLn(Target, '; ', S);
end;

procedure EmitInclude(S: String);
begin
  EmitI('include "' +  S + '"');
end;

procedure EmitClear(Bytes: Integer); forward;

procedure EmitCall(Sym: PSymbol);
var
  I, J: Integer;
begin
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
    for I := 0 to Sym^.Value - 1 do
    begin
      J := Sym^.ArgTypes[I]^.Value;
      EmitC('Cleanup ' + Int2Str(J) + ' bytes');
      if Sym^.ArgIsRef[I] or (J < 2) then J := 2;
      EmitClear(J);
    end;
  end;
end;

procedure EmitHeader(Home: String; SrcFile: String);
begin
  EmitC('');
  EmitC('program ' + SrcFile);
  EmitC('');
  if Binary = btCom then
  begin
    Emit('CPM', 'equ 1', 'Target is CP/M .com file');
  end
  else if Binary = btDot then
  begin
    Emit('NXT', 'equ 1', 'Target is Next .dot file');

    if Graphics = gmLowRes then 
      Emit('LORES', 'equ 1', 'Low-res graphics support')
    else if Graphics = gmHighRes then 
      Emit('HIRES', 'equ 1', 'High-res graphics support');
  end;

  EmitC('');
  EmitInclude(Home + '/pl0.z80');
  EmitC('');

  Emit('', 'jp main', '');
  EmitC('');

end;

procedure EmitFooter();
begin
  EmitC('');
  EmitC('HEAP:');
  EmitC('');
  EmitC('end');
  EmitC('');
end;

procedure CollectVars(Sym: PSymbol; var S: String);
begin
  if Sym^.Kind <> scScope then CollectVars(Sym^.Prev, S);

  if Sym^.Kind = scVar then
  begin
    if S <> '' then S := S + ', ' + Sym^.Name else S := Sym^.Name;
    S := S + '(';
    if Sym^.Value > 0 then S := S + '+';
    S := S + Int2Str(Sym^.Value) + ')';
  end;
end;

function EncodeAsmStr(S: String): String;
var
  T: String;
  Quotes: Boolean;
  I: Integer;
begin
  T := Int2Str(Length(S));
  Quotes := False;

  for I := 1 to Length(S) do
  begin
    C := S[I];
    if (C < ' ') or (C > '~') or (C = '"') then
    begin
      if Quotes then
      begin
        T := T + '"';
        Quotes := False;
      end;
      T := T + ',' + Int2Str(Ord(C));
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

(*)
procedure CollectString(Sym: PSymbol);
var
  S: String;
begin
  if Sym^.Kind <> scScope then CollectString(Sym^.Prev);

  if Sym^.Kind = scString then
  begin
      S := Sym^.Name;
      EmitC('');
      Emit(Sym^.Tag, 'db ' + Int2Str(Length(S)) + ',"' + S + '"', '');
  end;
end;
*)

(*
  Stack frame layout

          +------------------+
          | result (if any)  |
          | 1st argument     |
          | ...              |
          | nth argument     |
          | return addr      |
  ix+2 -> | old ix           |
  ix ---> | old display      |
  ix-2 -> | 1st local        |
          | ...              |
  sp ---> | nth local        |
          +------------------+

  Caller cleans up arguments.

  ix used for arguments and
  local variables.

  iy used for intermediate-level
  variables (neither local nor
  global), happens when procedures
  are nested (Pascal-speciality).

  Fast call via registers possible
  for run-time library functions.
*)
procedure EmitPrologue(Sym: PSymbol);
var
  I: Integer;
  V: String; 
begin
(*
  if Sym = Nil then
    EmitC('main entry point')
  else if Sym^.Kind = scFunc then
    EmitC('function ' + Sym^.Name)
  else
    EmitC('procedure ' + Sym^.Name);

  EmitC('');
*)
  V := '';
  CollectVars(SymbolTable, V);
  if V <> '' then
  begin
    EmitC('var ' + V);
    EmitC('');
  end;

  if Sym = Nil then
  begin
    Emit('main', 'call __init', '');
    EmitI('ld ix,0');
    EmitI('add ix,sp');
  end
  else
  begin
    Emit(Sym^.Tag, 'push ix', 'Prologue');

    EmitI('ld hl,(display+' + Int2Str(Level * 2) + ')');
    EmitI('push hl');

    EmitI('ld ix,0');
    EmitI('add ix,sp');

    EmitI('ld (display+' + Int2Str(Level * 2) + '),ix');

    EmitI('ld hl,' + Int2Str(Offset));
    EmitI('add hl,sp');
    EmitI('ld sp,hl');
(*
    I := Offset;
    if I < 0 then
    begin
      Emit('', 'ld de,0', 'Space for locals');
      while I < 0 do
      begin
        EmitI('push de');
        I := I + 2;
      end;
    end;
*)
  end;
end;

procedure EmitEpilogue(Sym: PSymbol);
begin
  if Sym = nil then
    Emit(ExitTarget, 'call __done', '')
  else
  begin
    Emit(ExitTarget, 'ld sp,ix', 'Epilogue');

    EmitI('pop hl');
    EmitI('ld (display+' + Int2Str(Level * 2) + '),hl');

    EmitI('pop ix');
  end;

  EmitI('ret');
end;

function RelativeAddr(Base: String; Offset: Integer): String;
begin
  if Offset < 0 then
    RelativeAddr := Base + Int2Str(Offset)
  else if Offset > 0 then
    RelativeAddr := Base + '+' + Int2Str(Offset)
  else
    RelativeAddr := Base;
end;

procedure EmitAddress(Sym: PSymbol);
var
  L: Integer;
begin
  L := Level - Sym^.Level;

  if (Sym^.Level = 1) and (Sym^.Tag <> '') then
  begin
    Emit('', 'ld hl,' + Sym^.Tag, 'Get global ' + Sym^.Name);
    EmitI('push hl');
  end
  else if L = 0 then
  begin
    Emit('', 'ld de,ix', '');
    Emit('', 'ld hl,' + Int2Str(Sym^.Value), '');
    Emit('', 'add hl,de', '');
    Emit('', 'push hl', '');
  end
  else
  begin
    Emit('', 'ld hl,(display+' + Int2Str(Sym^.Level * 2) + ')', 'Get outer ' + Sym^.Name);
    Emit('', 'ld de,' + Int2Str(Sym^.Value), '');
    Emit('', 'add hl,de', '');
    Emit('', 'push hl', '');
  end;

  if Sym^.IsRef then
  begin
    Emit('', 'pop hl', '');
    Emit('', 'ld de,(hl)', '');
    Emit('', 'push de', '');
  end;

  if Sym^.Value2 <> 0 then
  begin
    EmitI('pop hl');
    EmitI('ld de,' + Int2Str(Sym^.Value2));
    EmitI('add hl,de');
    EmitI('push hl');
  end;
end;

procedure EmitLoad(DataType: PSymbol);
begin
  if DataType^.Kind = scStringType then
  begin
    EmitI('pop de');
    EmitI('ld hl,-256');
    EmitI('add hl,sp');
    EmitI('ld sp,hl');
    EmitI('ex hl,de');
    EmitI('ld b,0');
    EmitI('ld c,(hl)');
    EmitI('inc bc');
    EmitI('ldir');
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
      Emit('', 'pop de', 'Load');
      EmitI('ld hl,-' + Int2Str(DataType^.Value));
      EmitI('add hl,sp');
      EmitI('ld sp,hl');
      EmitI('ex hl,de');
      EmitI('ld bc,' + Int2Str(DataType^.Value));
      Emit('', 'ldir', 'Load end');
    end
  end;
end;

procedure EmitStore(DataType: PSymbol);
var
  Tag, Len: String;
begin
  if DataType^.Kind = scStringType then
  begin
      Tag := GetLabel('ok');
      Len := Int2Str(DataType^.Value - 1);

      EmitI('ld hl,0');
      EmitI('add hl,sp');
      EmitI('ld a,' + Len);
      EmitI('cp (hl)');
      EmitI('jp nc,' + Tag);
      EmitI('ld (hl),a');
      Emit(Tag, '', '');
      //EmitI('inc a');
      EmitI('inc h');
      EmitI('ld de,(hl)');
      EmitI('dec h');
      EmitI('ld b,0');
      EmitI('ld c,a');
      EmitI('inc bc');
      EmitI('ldir');

      EmitI('ld hl,258');
      EmitI('add hl,sp');
      EmitI('ld sp,hl');
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
      Emit('', 'ld bc,' + Int2Str(DataType^.Value), 'Store');
      EmitI('ld hl,bc');
      EmitI('add hl,sp');
      EmitI('ld de,(hl)');
      EmitI('ld hl,0');
      EmitI('add hl,sp');
      EmitI('ldir');
      EmitI('ld sp,hl');
      Emit('', 'pop hl', 'Store end');
    end;
  end;
end;

procedure EmitLiteral(Value: Integer);
begin
  Emit('', 'ld de,' + Int2Str(Value), 'Literal ' + Int2Str(Value));
  EmitI('push de');
end;

procedure EmitSpace(Bytes: Integer);
begin
  Emit('', 'ld hl,-' + Int2Str(Bytes), 'Space');
  EmitI('add hl,sp');
  EmitI('ld sp,hl');
end;

procedure EmitClear(Bytes: Integer);
begin
  Emit('', 'ld hl,' + Int2Str(Bytes), 'Clear');
  EmitI('add hl,sp');
  EmitI('ld sp,hl');
end;

procedure EmitRelOp(Op: TToken);
begin
//  EmitI('pop de');
//  EmitI('pop hl');

  if (Op = toGt) or (Op = toLeq) then
  begin
    Emit('','pop hl', 'RelOp ' + Int2Str(Ord(Op)));
    EmitI('pop de');
  end
  else
  begin
    Emit('','pop de', 'RelOp ' + Int2Str(Ord(Op)));
    EmitI('pop hl');
  end;

//  if (Op = toGt) or (Op = toLeq) then EmitI('ex hl,de');

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

procedure EmitStrOp(Op: TToken);
var
  Invert: Boolean;
begin
//  EmitI('pop de');
//  EmitI('pop hl');

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

procedure EmitSetOp(Op: TToken);
var
  Invert: Boolean;
begin
//  EmitI('pop de');
//  EmitI('pop hl');

  Invert := Op = toNeq;

  if Op = toIn then
  begin
    EmitI('call __setin');
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

    toEq, toNeq: EmitI('call __flteq');
    toLt, toGeq: EmitI('call __fltlt');
    toGt, toLeq: EmitI('call __fltleq');
  end;

  if Op in [toAdd, toSub, toMul, toDiv] then
  begin
    EmitI('pushfp');
  end
  else EmitI('push de');

  if Invert then EmitUnOp(toNot, dtBoolean);
end;

procedure EmitJump(Target: String);
begin
    EmitI('jp ' + Target);
end;

procedure EmitJumpIf(When: Boolean; Target: String);
begin
  EmitI('pop hl');
  EmitI('bit 0,l');
  if When then
    EmitI('jp nz,' + Target)
  else
    EmitI('jp z,' + Target);
end;

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
    toDiv: Emit('', 'call __sdiv16', 'Div');
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
    end;
  Emit('', 'push hl', '');
end;

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

procedure EmitNeg(DataType: PSymbol);
begin
  if (DataType = dtInteger) or (DataType = dtByte) then
  begin
    EmitI('pop de');
    EmitI('ld hl,0');
    EmitI('xor a');
    EmitI('sbc hl,de');
    EmitI('push hl');
  end
  else if DataType = dtReal then
  begin
    EmitI('popfp');
    
    EmitI('ld a,b');
    EmitI('xor a,128');
    EmitI('ld b,a');

    EmitI('pushfp');
  end
  else Error('Invalid type ' + DataType^.Name);  
end;

procedure EmitInc(DataType: PSymbol);
begin
  EmitI('pop hl');
  
  if DataType = dtInteger then
  begin
    EmitI('ld de,(hl)');
    EmitI('inc de');
    EmitI('ld (hl),de');
  end
  else
  begin
    EmitI('inc (hl)');
  end;
end;

procedure EmitDec(DataType: PSymbol);
begin
  EmitI('pop hl');

  if DataType = dtInteger then
  begin
    EmitI('ld de,(hl)');
    EmitI('dec de');
    EmitI('ld (hl),de');
  end
  else
  begin
    EmitI('dec (hl)');
  end;
end;

procedure EmitShl;
begin
  EmitI('pop hl');
  EmitI('add hl,hl');
  EmitI('push hl');
end;

procedure EmitInputNum(S: String);
begin
  Emit('', 'call __getn', 'Get ' + S);
  EmitI('push de');
  Emit('', 'call __newline', '');
end;

procedure EmitWrite(DataType: PSymbol);
begin
  if (DataType = dtInteger) or (DataType = dtByte) then
  begin
    Emit('', 'pop hl', '');
    EmitI('call __putn');
  end
  else if DataType = dtBoolean then
  begin
    Emit('', 'pop hl', '');
    EmitI('call __putb')
  end
  else if DataType = dtChar then
  begin
    Emit('', 'pop hl', '');
    EmitI('ld a,l');
    EmitI('call __putc');
  end
  else if DataType^.Kind = scStringType then
  begin
    EmitI('ld hl,0');
    EmitI('add hl,sp');
    EmitI('call __puts');
    EmitI('ld hl,256');
    EmitI('add hl,sp');
    EmitI('ld sp,hl');
  end
  else if DataType = dtReal then
  begin
    EmitI('popfp');
    EmitI('call __putf');
  end
  else if DataType^.Kind = scEnumType then
  begin
    Emit('', 'pop hl', '');
    EmitI('ld de,' + DataType^.Tag);
    EmitI('add hl,hl');
    EmitI('add hl,de');
    EmitI('ld de,(hl)');
    EmitI('ex hl,de');
    EmitI('call __puts');
  end
  else Error('Unprintable type: ' + DataType^.Name);
end;

procedure EmitPrintStr(S: String);
var
  Tag: String;
begin
  if Length(S) = 1 then
  begin
    Emit('', 'ld a,' + Int2Str(Ord(S[1])), '');
    Emit('', 'call __putc', '');
  end
  else
  begin
    Tag := AddString(S);
    Emit('', 'ld hl,' + Tag, '');
    Emit('', 'call __puts', '');
  end;
end;

procedure EmitPrintNewLine();
begin
  Emit('', 'call __newline', '');
end;

procedure EmitAssertFailed(S: String; L: Integer);
begin
  Emit('', 'ld hl, ' + S, '');
  Emit('', 'ld de, ' + Int2Str(L), '');
  Emit('', 'call __assertfailed', '');
end;

procedure CloseTarget();
begin
  Flush;
  Close(Target);
end;

(* -------------------------------------------------------------------------- *)
(* --- Parser --------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

type
  TTypeCheck = (tcExact, tcAssign, tcExpr);

(**
 * Performs assignment type check. Might promote either type to the other
 * depending on the kind of check.
 *)
function TypeCheck(Left, Right: PSymbol; Check: TTypeCheck): PSymbol;
begin
  if Left = nil then Error('Error in TypeCheck: Left = nil');
  if Right = nil then Error('Error in TypeCheck: Right = nil');

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
    TypeCheck := Left;
    EmitI('pop de');
    EmitI('ld hl,-254');
    EmitI('add hl,sp');
    EmitI('ld sp,hl');
    EmitI('ld d,e');
    EmitI('ld e,1');
    EmitI('push de');

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

function ParseExpression: PSymbol; forward;

function ParseVariableAccess(Symbol: PSymbol): PSymbol;
var
  DataType: PSymbol;
  Size: Integer;
begin
  EmitAddress(Symbol);

  DataType := Symbol^.DataType;

  while Scanner.Token in [toLBrack, toPeriod, toCaret] do
  begin
    if Scanner.Token = toLBrack then
    begin
      if DataType^.Kind = scArrayType then
      begin
      NextToken;

      TypeCheck(dtInteger, ParseExpression(), tcExpr);

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
        NextToken;

        TypeCheck(dtInteger, ParseExpression(), tcExpr);

        DataType := dtChar;
        EmitBinOp(toAdd, dtInteger);
      end
      else Error('Not an array or a string');
      
      Expect(toRBrack);

      NextToken;
    end
    else if Scanner.Token = toPeriod then
    begin
      if DataType^.Kind <> scRecordType then
        Error('Not a record');

      NextToken;
      Expect(toIdent);
      Symbol := FindField(DataType^.DataType, Scanner.StrValue);
      if Symbol = nil  then Error('Field not found: ' + Scanner.StrValue);

      DataType := Symbol^.DataType;

      EmitLiteral(Symbol^.Value);
      EmitBinOp(toAdd, dtInteger);

      NextToken;
    end
    else
    begin
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
  ParseVariableAccess := DataType;
end;

procedure ParseArgument(Sym: PSymbol; I: Integer);
var
  Sym2: PSymbol;
begin
  if Sym^.ArgIsRef[I] then
  begin
    Expect(toIdent);
    Sym2 := LookupGlobal(Scanner.StrValue);
    if Sym2 = nil then
      Error('Identifier not found: ' + Scanner.StrValue);
    if Sym2^.Kind <> scVar then
      Error('Not a variable: ' + Scanner.StrValue);

    NextToken;
    
    TypeCheck(Sym^.ArgTypes[I], ParseVariableAccess(Sym2), tcAssign);
  end
  else
    TypeCheck(Sym^.ArgTypes[I], ParseExpression, tcAssign);  
end;

procedure ParseArguments(Sym: PSymbol);
var
  I: Integer;
begin
  I := 0;
  if Scanner.Token = toLParen then
  begin
    NextToken;
    ParseArgument(Sym, I);
    I := I + 1;

    while Scanner.Token = toComma do
    begin
      NextToken;
      ParseArgument(Sym, I);
      I := I + 1;
    end;

    Expect(toRParen);
    NextToken;
  end;

  if I <> Sym^.Value then Error('Wrong number of arguments');
end;

function ParseBuiltInFunction(Func: PSymbol): PSymbol;
var
  Sym: PSymbol;
begin
  if Func^.Kind <> scFunc then
    Error('Not a built-in function: ' + Func^.Name);

  NextToken; Expect(toLParen); NextToken;

  if Func = AbsFunc then
  begin
    Sym := ParseExpression;

    if Sym = dtInteger then
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
    Sym := LookupGlobal(Scanner.StrValue);
    if Sym = nil then Error('Identifier "' + Scanner.StrValue + '" not found.');
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
    Sym := LookupGlobal(Scanner.StrValue);
    if Sym = nil then Error('Identifier "' + Scanner.StrValue + '" not found.');
    NextToken;

    if Sym^.Kind = scVar then
      EmitLiteral(Sym^.DataType^.Value)
    else if Sym^.Kind in [scType, scArrayType, scRecordType, scEnumType, scStringType] then
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
    EmitI('push ix');
    EmitI('ld c,l');
    EmitI('call 5');
    EmitI('pop ix');

    if Func = BdosFunc then
    begin
      EmitI('ld l,a');
      EmitI('ld h,0');
    end;

    EmitI('push hl');

    ParseBuiltInFunction := dtByte;
  end
  else
    Error('Cannot handle: ' + Func^.Name);

    Expect(toRParen); NextToken;
end;

procedure ParseBuiltInProcedure(Proc: PSymbol; BreakTarget, ContTarget: String);
var
  Sym, T: PSymbol;
  Tag: String;
begin
  if Proc^.Kind <> scProc then
    Error('Not a built-in procedure: ' + Proc^.Name);

  if Proc = AssertProc then
  begin
    NextToken; Expect(toLParen); NextToken;

    TypeCheck(dtBoolean, ParseExpression, tcExact);

    Emit('', 'pop bc', '');
    Emit('', 'ld hl, ' + AddString(Source[Include].Name), '');
    Emit('', 'ld de, ' + Int2Str(Source[Include].Line), '');
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
  else if (Proc = WriteProc) or (Proc = WriteLnProc) then
  begin
    NextToken;

    if Scanner.Token = toLParen then
    begin
      NextToken;
      EmitWrite(ParseExpression);

      while Scanner.Token = toComma do
      begin
        NextToken;
        EmitWrite(ParseExpression);
      end;

      Expect(toRParen);
      NextToken;
    end;

    if Proc = WriteLnProc then EmitPrintNewLine;
  end
  else if Proc = NewProc then
  begin
    NextToken; Expect(toLParen); NextToken;

    Expect(toIdent);
    Sym := LookupGlobal(Scanner.StrValue);
    if Sym = nil then Error('Identifier "' + Scanner.StrValue + '" not found.');
    NextToken;
    T := ParseVariableAccess(Sym);

    // EmitI('pop hl');
    // EmitI('ld e,(hl)');
    // EmitI('inc hl');
    // EmitI('ld d,(hl)');
    // EmitI('push de');

    EmitLiteral(T^.DataType^.Value);
    EmitCall(LookupGlobal('GetMem'));

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
    EmitCall(LookupGlobal('FreeMem'));

    Expect(toRParen); NextToken;
  end
  else
    Error('Cannot handle: ' + Proc^.Name);
end;

function ParseFactor: PSymbol;
var
  Sym: PSymbol; T: PSymbol;
  Op: TToken;
  S1, S2, S3, Tag: String;
  I, J, K: Integer;
  BA: array[0..31] of Byte;
begin
  if Scanner.Token = toIdent then
  begin
    Sym := LookupGlobal(Scanner.StrValue);
    if Sym = nil then Error('Identifier "' + Scanner.StrValue + '" not found.');

    (* This is a dirty hack, but ok for now. *)
    if (Sym^.Kind = scVar) and (Sym^.Tag = 'RESULT') then
      Sym := Sym^.Prev^.Prev; 

    if Sym^.Kind = scVar then
    begin
      NextToken;
      T := ParseVariableAccess(Sym);

      // if not (T^.Kind in [scType, scEnumType]) then Error('" ' + Sym^.Name + '" is not a simple type.');
      EmitLoad(T);
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
        if T.Value = 1 then EmitSpace(2) else EmitSpace(T.Value); (* Result *)
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
  else if Scanner.Token = toFloat then
  begin
    T := dtReal;
    Tag := AddString(Scanner.StrValue + ' ');
    EmitI('constfp ' + Tag);
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
    for I := 0 to 31 do BA[I] := 0;

    T := nil;
    NextToken;
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
      else if Scanner.Token = toString then
      begin
        if (T <> nil) and (T <> dtChar) then Error('Char expected');
        if Length(Scanner.StrValue) <> 1 then Error('Char expected');

        T := dtChar;

        I := Ord(Scanner.StrValue[1]);
        J := I;
        NextToken;
        if Scanner.Token = toRange then
        begin
          NextToken;
          Expect(toString);
          if Length(Scanner.StrValue) <> 1 then Error('Char expected');
          J := Ord(Scanner.StrValue[1]);
          NextToken;
        end
      end
      else if Scanner.Token = toIdent then
      begin
        Sym := LookupGlobal(Scanner.StrValue);
        if Sym = nil then Error('Identifier ' + Scanner.StrValue + ' not found');
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
          Sym := LookupGlobal(Scanner.StrValue);
          if Sym = nil then Error('Identifier ' + Scanner.StrValue + ' not found');
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
    NextToken;

    //for K := 0 to 31 do Write(BA[K], ' ');

    EmitSpace(32); // EmitBytes

    S1 := GetLabel('set');
    S2 := GetLabel('set');
    EmitI('jr ' + S2);
    Emit(S1, '', '');
    S3 := '';
    for K := 0 to 31 do S3 := S3 + HexStr(BA[K], 2);
    EmitI('dm $' + S3);
    Emit(S2, '', '');
    EmitI('ld hl,0');
    EmitI('add hl,sp');
    EmitI('ld de,' + S1);
    EmitI('ex de,hl');
    EmitI('ld bc,32');
    EmitI('ldir');

    Sym := CreateSymbol(scSetType, '', 32);
    Sym^.DataType := T;
    T := Sym;
  end
  else if Scanner.Token = toNot then
  begin
    NextToken;

    Op := toNone;
    if (Scanner.Token = toAdd) or (Scanner.Token = toSub) then
    begin
      Op := Scanner.Token;
      NextToken;
    end;

    if Op = toSub then
      EmitLiteral(0);

    T := ParseFactor();
    if T = dtChar then Error('not only applicable to Integer, Byte or Boolean');

    if Op = toSub then EmitBinOp(toSub, T);

    EmitUnOp(toNot, T);
  end
  else Error('Factor expected');

  ParseFactor := T;
end;

function ParseTerm: PSymbol;
var
  Op: TToken;
  T: PSymbol;
begin
  T := ParseFactor;

  if (T = dtInteger) or (T = dtByte) then
    while Scanner.Token in [toMul, toDiv, toMod, toAnd] do
    begin
      Op := Scanner.Token;
      NextToken;
      T := TypeCheck(T, ParseFactor, tcExpr);
      EmitBinOp(Op, T);
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
    while Scanner.Token in [toMul, toDiv] do
    begin
      Op := Scanner.Token;
      NextToken;
      T := TypeCheck(T, ParseTerm, tcExpr);
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

function ParseSimpleExpression: PSymbol;
var
  Op: TToken;
  T: PSymbol;
begin
  Op := toNone;

  (* Integer only *)
  if (Scanner.Token = toAdd) or (Scanner.Token = toSub) then
  begin
    Op := Scanner.Token;
    NextToken;
  end;

  // if Op = toSub then EmitLiteral(0);

  T := ParseTerm;

  if (Op <> toNone) then
  begin
    if (T <> dtInteger) and (T <> dtByte) and (T <> dtReal) then
      Error('Number expected.'); // T := TypeCheck(dtInteger, T, tcExpr); // +=Byte, -=Integer

    if Op = toSub then EmitNeg(T);
  //if Op = toSub then EmitBinOp(toSub, T);
  end;

  if (T = dtInteger) or (T = dtByte) then
    while Scanner.Token in [toAdd, toSub, toOr, toXor] do
    begin
      Op := Scanner.Token;
      NextToken;
      T := TypeCheck(T, ParseTerm, tcExpr);
      EmitBinOp(Op, T);
    end
  else if T = dtBoolean then
    while Scanner.Token in [toOr, toXor] do
    begin
      Op := Scanner.Token;
      NextToken;
      T := TypeCheck(T, ParseTerm, tcExact);
      EmitBinOp(Op, T);
    end
  else if T^.Kind = scStringType then
    while Scanner.Token = toAdd do
    begin
      Op := Scanner.Token;
      NextToken;
      T := TypeCheck(T, ParseTerm, tcExact);
      EmitI('call __stradd');
      EmitClear(256);
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

function ParseExpression: PSymbol;
var
  Op: TToken;
  T, U: PSymbol;
begin
  T := ParseSimpleExpression;
  if Scanner.Token = toIn then
  begin
    if not (T^.Kind in [scType, scEnumType, scSubrangeType]) then Error('Scalar needed');
    if T^.Value <> 1 then Error('8 bit needed');

    NextToken;
    U := ParseExpression;
    if U^.Kind <> scSetType then Error('Set needed');    
    if U^.DataType <> T then Error('Incompatible');    

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
    if U^.DataType <> T^.DataType then Error('Incompatible');    

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
    TypeCheck(T, ParseSimpleExpression, tcExpr); // Check type, but ignore result.
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

procedure ParseAssignment(Sym: PSymbol; Again: Boolean);
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

procedure ParseWith(ContTarget, BreakTarget: String);
var
  OldSymbols, Sym, Field, FieldRef, DataType: PSymbol;
  OldOffset, Address: Integer;
begin
  OldSymbols := SymbolTable;
  OldOffset := Offset;

  NextToken;
  Expect(toIdent);
  Sym := LookupGlobal(Scanner.StrValue);
  if Sym = nil then Error('Identifier not found');
  if Sym^.Kind <> scVar then Error('Variable expected');
  NextToken;

  Offset := Offset - 2;
  Address := Offset;
  DataType := ParseVariableAccess(Sym);

  if DataType^.Kind <> scRecordType then Error('Record type expected');
  Expect(toDo);
  NextToken;

    // Check: Global (not in unit tests), Local, Var, Nested, Comma
    // Optimize simple case 'with R do X := 5;' to avoid double addressing

  //WriteLn('Level ', Level, ' with at offset ', Address);

  Field := DataType^.DataType;
  while Field <> nil do
  begin
    FieldRef := CreateSymbol(scVar, '', 0);
    FieldRef^.Name := Field^.Name;
    FieldRef^.DataType := Field^.DataType;
    FieldRef^.Value := Address;
    FieldRef^.Value2 := Field^.Value;
    FieldRef^.IsRef := True;
    FieldRef^.Level := Level;
    //WriteLn('Create ' , Field^.Name, ' at level ', Level, ' address ', Address, ' Offset ', Field^.Value);
    Field := Field^.Prev;
  end;

  ParseStatement(ContTarget, BreakTarget);

  Offset := Offset + 2;
  EmitI('pop bc');

  while SymbolTable <> OldSymbols do
  begin
    Sym := SymbolTable;
    SymbolTable := SymbolTable^.Prev;
    //WriteLn('Drop ' , Sym^.Name);
    FreeMem(Sym);
  end;
end;

procedure ParseInlineTerm(var S: String; var W: Boolean);
var
  Sym: PSymbol;
begin
  if Scanner.Token = toNumber then
  begin
    S := S + Int2Str(Scanner.NumValue);
    if Scanner.NumValue > 255 then W := True;
    NextToken;
  end
  else if Scanner.Token = toIdent then
  begin
    Sym := LookupGlobal(Scanner.StrValue);
    if Sym = nil then Error('Unknown identifier: ' + Scanner.StrValue);

    if (Sym^.Kind = scConst) and (Sym^.DataType = dtInteger) then
    begin
      S := S + Int2Str(Sym^.Value);
      if Sym^.Value > 255 then W := True;
    end
    else if (Sym^.Kind = scVar) and (Sym^.Level = 1) then
    begin
      S := S + Sym^.Tag;
      W := True;
    end
    else if (Sym^.Kind = scVar) and (Sym^.Level > 1) then
    begin
      S := S + Int2Str(Sym^.Value);
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
  else Error('Invalid inline code');
end;

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

procedure ParseInline;
begin
  Expect(toLParen);
  NextToken;

  ParseInlineExpr;
  while Scanner.Token = toDiv do
  begin
    NextToken;
    ParseInlineExpr;
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
    C := LookupGlobal(Scanner.StrValue);
    if C = nil then Error('Not found');
    if C^.Kind <> scConst then Error('Const expected');
    if C^.DataType <> T then Error('Invalid type');
    V := C^.Value;
  end
  else if Scanner.Token = toNumber then
  begin
    if T <> dtInteger then Error('Invalid type');
    V := Scanner.NumValue;
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
    EmitI('ld hl,' + Int2Str(High));
    EmitI('call __int16_eq');
    EmitI('and a');
    EmitI('jp nz,' + OfTarget);
  end
  else
  begin
    EmitI('ld bc,' + Int2Str(Low));
    EmitI('ld hl,' + Int2Str(High));
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

  while Scanner.Token in [toIdent, toNumber, toString] do
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

procedure ParseStatement(ContTarget, BreakTarget: String);
var
  Sym, Sym2, F: PSymbol;
  Tag, Tag2, Tag3, Tag4: String;
  Delta: Integer;
  (*T: PSymbol;*)
  NewLine: Boolean;
begin
  if Scanner.Token = toIdent then
  begin
    Sym := LookupGlobal(Scanner.StrValue);
    if Sym = nil then Error('Identifier "' + Scanner.StrValue + '" not found.');

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
    else
    begin
      ParseAssignment(Sym, false);
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

    Sym := LookupGlobal(Scanner.StrValue);
    if Sym = nil then Error('Identifier "' + Scanner.StrValue + '" not found.');
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
  end
  else if Scanner.Token = toCase then
  begin
    ParseCaseStatement(ContTarget, BreakTarget);
  end
  else if Scanner.Token = toWith then
  begin
    ParseWith(ContTarget, BreakTarget);
  end
  else if Scanner.Token = toInline then
  begin
    NextToken;
    ParseInline;
  end;
end;

function ParseTypeDef: PSymbol; forward;

procedure ParseConstValue(DataType: PSymbol);
var
  I, Sign, Value: Integer;
  Sym: PSymbol;
begin
  if DataType^.Kind = scArrayType then
  begin
    Expect(toLParen);
    NextToken;

    for I := 1 to DataType^.Bounds - 1 do
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
    Error('Not supported');
  end
  else if DataType^.Kind = scEnumType then
  begin
    Expect(toIdent);
    Sym := LookupLocal(Scanner.StrValue);
    if (Sym^.Kind <> scConst) or (Sym^.DataType <> DataType) then
      Error('Invalid enum constant');

    Emit('', 'dw ' + Int2Str(Sym^.Value), '');

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
      Emit('', 'dw ' + Int2Str(Value), '');

      NextToken;
    end
    else if DataType = dtChar then
    begin
      Expect(toString);
      if Length(Scanner.StrValue) <> 1 then Error('Char expected');
      Emit('', 'dw "' + Scanner.StrValue + '"', '');     
      NextToken; 
    end
    else
      Error('Not supported');
  end;
end;

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

    Sym := CreateSymbol(scVar, Name, 0);
    Sym^.DataType := ParseTypeDef();
    Sym^.Tag := GetLabel('const');
    Sym^.Level := 1; (* Force global *)

    Emit(Sym^.Tag, '', '');

    Expect(toEq);
    NextToken;

    ParseConstValue(Sym^.DataType);
  end
  else
  begin
    Sym := CreateSymbol(scConst, Name, 0);

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
      Sym^.Tag := AddString(Scanner.StrValue + ' ');
    end
    else if Scanner.Token = toIdent then
    begin
      Sym2 := LookupGlobal(Scanner.StrValue);
      if Sym2 = nil then Error('Identifier not found: ' + Scanner.StrValue);
      if Sym2^.Kind <> scConst then Error('Not a constant: ' + Scanner.StrValue);

      Sym^.DataType := Sym2^.DataType;
      Sym^.Value := Sym2^.Value;
    end
    else
    begin
      if Scanner.Token = toSub then
      begin
        Sign := -1;
        NextToken;
      end
      else Sign := 1;
      Expect(toNumber);

      Sym^.Value := Sign * Scanner.NumValue;
      Sym^.DataType := dtInteger;
    end;

    NextToken;
  end;
end;

function ParseFieldGroup(var Fields: PSymbol): PSymbol;
var
  Name: String;
  Sym: PSymbol;
begin
  Name := Scanner.StrValue;
  NextToken;

  New(Sym);
  Sym^.Kind := scVar;
  Sym^.Name := Name;
  Sym^.Prev := Fields;
  Fields := Sym;

  if Scanner.Token = toComma then
  begin
    NextToken;
    Expect(toIdent);
    Sym^.DataType := ParseFieldGroup(Fields);
  end
  else
  begin
    Expect(toColon);
    NextToken;
    Sym^.DataType := ParseTypeDef();
  end;

  ParseFieldGroup := Sym^.DataType;
end;

function GetFieldOffset(Field: PSymbol): Integer;
begin
  if Field = nil then
    GetFieldOffset := 0
  else
  begin
    Field^.Value := GetFieldOffset(Field^.Prev);
    GetFieldOffset := Field^.Value + Field^.DataType^.Value;
  end;
end;

function ParseTypeDef: PSymbol;
var
  DataType, Sym: PSymbol;
  I: Integer;
begin
  if Scanner.Token = toArray then
  begin
    DataType := CreateSymbol(scArrayType, '', 0);
    NextToken;

    Expect(toLBrack);
    NextToken;

    if Scanner.Token = toNumber then
      DataType^.Bounds := Scanner.NumValue
    else if Scanner.Token = toIdent then
    begin
      Sym := LookupGlobal(Scanner.StrValue);
      if Sym = nil then
        Error(Scanner.StrValue + ' not found');
      if (Sym^.Kind <> scConst) or (Sym^.DataType <> dtInteger) then
        Error(Scanner.StrValue + ' is not an integer constant');
      DataType^.Bounds := Sym^.Value;
    end
    else
      Error('Integer literal or constant expected');     

    NextToken;
(*
    if Scanner.Token = toDots then
    begin
      NextToken;

      Expect(toNumber);
      NextToken;
    end;
*)
    Expect(toRBrack);
    NextToken;

    Expect(toOf);
    NextToken;

    DataType^.DataType := ParseTypeDef();
    DataType^.Value := DataType^.Bounds * DataType^.DataType^.Value;
  end
  else if Scanner.Token = toStringKW then
  begin
    NextToken;

    DataType := CreateSymbol(scStringType, '', 0);

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
  else if Scanner.Token = toRecord then
  begin
    DataType := CreateSymbol(scRecordType, '', 0);
    NextToken;

    while Scanner.Token = toIdent do
    begin
      ParseFieldGroup(DataType^.DataType);
      Expect(toSemicolon);
      NextToken;
    end;

    Expect(toEnd);
    NextToken;

    if DataType^.DataType <> nil then DataType^.Value := GetFieldOffset(DataType^.DataType);
  end
  else if Scanner.Token = toSet then
  begin
    NextToken;
    Expect(toOf);
    NextToken;

    DataType := CreateSymbol(scSetType, '', 0);
    DataType^.DataType := ParseTypeDef;

    if not (DataType^.DataType^.Kind in [scType, scEnumType, scSubrangeType]) then
      Error('Scalar type required');

    if DataType^.DataType^.Value > 1 then
      Error('Base type too large');

    DataType^.Value := 32;
  end
  else if Scanner.Token = toCaret then
  begin
    DataType := CreateSymbol(scPointerType, '', 0);
    DataType^.Value := 2;
    NextToken;

    if Scanner.Token = toIdent then
    begin
      DataType^.DataType := LookupGlobal(Scanner.StrValue);
      if DataType^.DataType = nil then DataType^.Tag := Scanner.StrValue;
      NextToken;
    end
    else DataType^.DataType := ParseTypeDef;
  end
  else if Scanner.Token = toLParen then
  begin
    DataType := CreateSymbol(scEnumType, '', 0);
    DataType^.Tag := GetLabel('enumlit');
    DataType^.Value := 1;
    I := 0;

    Emit(DataType^.Tag, '', '');

    repeat
      NextToken;
      Expect(toIdent);

      Sym := CreateSymbol(scConst, Scanner.StrValue, 0);
      Sym^.Value := I;
      // Sym^.Value2 := AddString(Sym^.Name);

      Emit('', 'dw ' + AddString(Sym^.Name), '');

      Sym^.DataType := DataType;
      I := I + 1;
      NextToken;
    until Scanner.Token <> toComma;

    Expect(toRParen);
    NextToken;
  end
  else if Scanner.Token = toNumber then
  begin
    DataType := CreateSymbol(scSubrangeType, '', 0);
    DataType^.DataType := dtInteger;

    NextToken;
    Expect(toRange);
    NextToken;
    Expect(toNumber);

    if Scanner.NumValue < 256 then DataType^.Value := 1 else DataType^.Value := 2;

    NextToken;
  end
  else if Scanner.Token = toString then
  begin
    if Length(Scanner.StrValue) <> 1 then Error('Char expected');

    DataType := CreateSymbol(scSubrangeType, '', 0);
    DataType^.DataType := dtChar;

    NextToken;
    Expect(toRange);
    NextToken;
    Expect(toString);
    if Length(Scanner.StrValue) <> 1 then Error('Char expected');

    DataType^.Value := 1;

    NextToken;
  end
  else begin
    Expect(toIdent);

    Sym := LookupGlobal(Scanner.StrValue);

    if Sym = nil then
      Error('Identifier not found: ' + Scanner.StrValue);

    if Sym^.Kind = scConst then
    begin
      DataType := CreateSymbol(scSubrangeType, '', 0);
      DataType^.DataType := Sym^.DataType;
      DataType^.Value := 1;

      NextToken;
      Expect(toRange);
      NextToken;
      Expect(toIdent);
      NextToken;
    end
    else
    begin
      if not (Sym^.Kind in [scType, scArrayType, scRecordType, scEnumType, scStringType, scSetType, scPointerType, scSubrangeType]) then
        Error('Not a type: ' + Scanner.StrValue);

      DataType := Sym;
      NextToken;
    end;
  end;

  ParseTypeDef := DataType;
end;

procedure ParseVar;
var
  Name: String;
begin
  Expect(toIdent);
  Name := Scanner.StrValue;
  NextToken;

  CreateSymbol(scVar, Name, 0);
  (*
  if Sym^.Level = 1 then
  begin
    Sym^.Tag := GetLabel('global');
    Emit0(Sym^.Tag, 'ds ' + Int2Str(Sym^.Bounds * 2), 'Global ' + Sym^.Name);
  end;
  *)
end;

procedure ParseVarList();
var
  Old: PSymbol;
  DataType, Sym: PSymbol;
  Low, High: Integer;
  Multi, IsAbs: Boolean;
  Tag: String;
begin
  Low := 0;
  High := -1;

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
      Tag := Int2Str(Scanner.NumValue)
    else if Scanner.Token = toString then
      Tag := Scanner.StrValue
    else if Scanner.Token = toIdent then
    begin
      Sym := LookupLocal(Scanner.StrValue);
      if Sym = nil then Error('Ident not found');
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
      SetDataType(Old, DataType, High - Low + 1);

      if IsAbs then Old^.Tag := Tag
      else if Old^.Level = 1 then
      begin
        Old^.Tag := GetLabel('global');
        Emit(Old^.Tag, 'ds ' + Int2Str(Old^.DataType^.Value), 'Global ' + Old^.Name);
      end;
    end;

//    Tmp^.DataType := DataType;
    //if High >= Low then Tmp^.Bounds := High - Low + 1;
//    Tmp := Tmp^.Prev;
    //Offset := Offset + (High - Low + 1) * 2 - 2;
  end;
end;

procedure ParseParamList(IsRef: Boolean);
var
  Old, Sym: PSymbol;
  DataType: PSymbol;
  Low, High: Integer;
begin
  Low := 0;
  High := -1;

  Old := SymbolTable;
  ParseVar;
  SymbolTable^.IsRef := IsRef;
  while Scanner.Token = toComma do
  begin
    NextToken; ParseVar;
    SymbolTable^.IsRef := IsRef;
  end;

  Expect(toColon);
  NextToken;

  Expect(toIdent);

  DataType := LookupGlobal(Scanner.StrValue);

  if DataType = nil then Error('Unknown identifier');
  if not (DataType.Kind in [scType, scArrayType, scRecordType, scEnumType, scStringType, scSetType, scPointerType]) then Error('Type expected');

  NextToken;

  (*if (DataType^.Kind in [scArrayType, scRecordType]) and not isRef then
    Error('Structured parameters must be passed by reference');*)

  Sym := SymbolTable;
  while Sym<>Old do
  begin
    if Sym^.Kind = scVar then
    begin
      SetDataType(Sym, DataType, High - Low + 1);
    end;
    Sym := Sym^.Prev;
  end;
end;

procedure ParseBlock(Sym: PSymbol); forward;

procedure ParseDeclarations(Sym: PSymbol);
var
  NewSym, ResVar, FwdSym, OldSyms, P: PSymbol;
  Token: TToken;
  Name, S: String;
  IsRef: Boolean;
begin
  while Scanner.Token in [toConst, toType, toVar, toProcedure, toFunction] do
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

        if LookupLocal(Name) <> nil then Error('Duplicate identifier ' + Name);

        NextToken;
        Expect(toEq);
        NextToken;

        ParseTypeDef^.Name := Name;

        Expect(toSemicolon);
        NextToken;
      until Scanner.Token <> toIdent;

      P := SymbolTable;
      while P <> OldSyms do
      begin
        if (P^.Kind = scPointerType) and (P^.DataType = nil) then
        begin
          P^.DataType := LookupGlobal(P^.Tag);
          if P^.DataType = nil then Error('Unresolved forward pointer ' + P^.Name);
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

    else if (Scanner.Token = toProcedure) or (Scanner.Token = toFunction) then
    begin
      Token := Scanner.Token;
      NextToken; Expect(toIdent);

      FwdSym := LookupLocal(Scanner.StrValue);
      if (FwdSym <> nil) and (FwdSym^.IsForward) then
      begin
        if Token = toProcedure then
          NewSym := CreateSymbol(scProc, '', 0)
        else
          NewSym := CreateSymbol(scFunc, '', 0);

        if NewSym^.Kind <> FwdSym^.Kind then Error('Proc/Func mismatch');

        FwdSym^.IsForward := False;

        OpenScope;

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
      end
      else
      begin
        if Token = toProcedure then
        begin
          NewSym := CreateSymbol(scProc, Scanner.StrValue, 0);
          NewSym^.Tag := GetLabel('proc');
        end
        else
        begin
          NewSym := CreateSymbol(scFunc, Scanner.StrValue, 0);
          NewSym^.Tag := GetLabel('func');
        end;

        OpenScope;
      end;

      NewSym^.IsStdCall := True;

      if FwdSym = nil then
      begin
        if Token = toFunction then
        begin
          CreateSymbol(scVar, Scanner.StrValue, 0)^.Tag := 'RESULT';
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

          NewSym^.DataType := ParseTypeDef();
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
        ParseBlock(NewSym);
        Expect(toSemicolon);
        NextToken;
      end;

      CloseScope;

      EmitC('');
    end;
  end;
end;

procedure ParseStatementList(ContTarget, BreakTarget: String);
begin
  ParseStatement(ContTarget, BreakTarget);
  while Scanner.Token = toSemicolon do
  begin
    NextToken;
    ParseStatement(ContTarget, BreakTarget);
  end;
end;

procedure ParseBlock(Sym: PSymbol);
begin
  ParseDeclarations(Sym);

  ExitTarget := GetLabel('exit');
  EmitPrologue(Sym);

  Expect(toBegin);
  NextToken;

  ParseStatementList('', '');

  Expect(toEnd);
  NextToken;

  EmitEpilogue(Sym);
end;

var
  SrcFile, MainFile, WorkFile, AsmFile, BinFile, HomeDir, AsmTool, S: String; 
  I: Integer;

procedure ParseProgram;
begin
  OpenScope;
  RegisterAllBuiltIns((Binary = btDot) and (Graphics <> gmNone));

  SetInclude(HomeDir + '/lib/system.pas');
  NextToken;
  ParseDeclarations(nil);

  if Scanner.Token = toProgram then
  begin
    NextToken;
    Expect(toIdent);
    NextToken;
    Expect(toSemicolon);
    NextToken;
  end;
  parseBlock(Nil);
  Expect(toPeriod);
(*
  EmitC('');
  Emit('globals', 'ds ' + Int2Str(Offset), 'Globals');
*)
  EmitStrings();
  EmitC('');
  Emit('display', 'ds 32', 'Display');
  EmitC('');
  Emit('eof', '', 'End of file');
  CloseScope;
end;

function Build: Integer;
var
  Dir: String;
  Org, Len: Integer;
begin
  Dir := FExpand('.');

  AsmFile := ChangeExt(SrcFile, '.z80');

  WriteLn('Compiling...');
  WriteLn('  ', FRelative(SrcFile, Dir), ' -> ', FRelative(AsmFile, Dir));

  if Binary = btCom then
    BinFile := ChangeExt(SrcFile, '.com')
  else if Binary = btDot then
    BinFile := ChangeExt(SrcFile, '.dot');

  if SetJmp(StoredState) = 0 then
  begin
    Build := 1;

    ErrorLine := 0;
    ErrorColumn := 0;
    Level := 0;
    Offset := 0;
    Scanner.Token := toNone;
    Include := False;
    C := #0;

    while SymbolTable <> nil do CloseScope;

    OpenInput(SrcFile);
    OpenTarget(AsmFile);
    EmitHeader(HomeDir, SrcFile);  (* TODO Move this elsewhere. *)

    ParseProgram;

    EmitFooter();                  (* TODO Move this elsewhere. *)
    CloseTarget();
    CloseInput();

    Build := 2;

    WriteLn('Assembling...');
    WriteLn('  ', FRelative(AsmFile, Dir), ' -> ', FRelative(BinFile, Dir));
    WriteLn;

    Exec(AsmTool,  '-v1 -w ' + AsmFile + ' ' + BinFile);
    if DosError <> 0 then
      Error('Error ' + Int2Str(DosError) + ' starting ' + AsmTool);
    if DosExitCode <> 0 then
      Error('Failure! :(');

    if Binary = btCom then Org := 256 else Org := 8192;
    Len := FSize(BinFile);
    //WriteLn;
    WriteLn(Len, ' bytes (', HexStr(Org, 4), '-', HexStr(Org + Len - 1, 4), ')');

    Build := 0;
  end;
end;

(* --- Interactive Menu --- *)

type
  TCharSet = set of Char;

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

function GetFile(const S: String): String;
var
  T: String;
begin
  Write(S);
  ReadLn(T);
  if Length(T) <> 0 then
  begin
    if Pos('.', T) = 0 then T := T + '.pas';
    GetFile := FExpand(T);
  end;
end;

procedure DoDirectory;
var
  Dir: String;
begin
  Write('New directory: ');
  ReadLn(Dir);
  ChDir(Dir);  
end;

procedure DoMainFile;
begin
  MainFile := GetFile('Main file name: ');
end;

procedure DoWorkFile;
begin
  WorkFile := GetFile('Work file name: ');
end;

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
      Exec('/Applications/Visual Studio Code.app/Contents/MacOS/Electron', '-g ' + S + ':' + Int2Str(Line) + ':' + Int2Str(Column))
    else
      Exec('/Applications/Visual Studio Code.app/Contents/MacOS/Electron', S)
  end
  else
  begin
    if (Line <> 0) and (Column <> 0) then
      Exec('/opt/local/bin/nano', '--minibar -Aicl --rcfile ' + HomeDir + '/etc/pl0.nanorc +' + Int2Str(Line) + ',' + Int2Str(Column) + ' ' + S)
    else
      Exec('/opt/local/bin/nano', '--minibar -Aicl --rcfile ' + HomeDir + '/etc/pl0.nanorc ' + S);
  end;
end;

procedure DoCompile;
var
  I, Org, Len: Integer;
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

procedure DoRun(Alt: Boolean);
begin
  if Length(BinFile) <> 0 then
  begin
    if Binary = btCom then
    begin
      if Alt then
        Exec('/Users/joerg/Library/bin/tnylpo', '-soy -t @ ' + BinFile)
      else
        Exec('/Users/joerg/Library/bin/tnylpo', BinFile)
    end
    else
    begin
      Exec('/Users/joerg/Library/bin/hdfmonkey', 'put /Users/joerg/Downloads/tbblue.mmc ' + BinFile + ' /autoexec.dot');
      if Alt then
        Exec('/Library/Frameworks/Mono.framework/Versions/Current/Commands/mono', '/Users/joerg/Downloads/CSpect2_16_5/CSpect.exe -zxnext -w4 -r -brk -nextrom -mouse -sound -mmc=/Users/joerg/Downloads/tbblue.mmc')
      else
        Exec('/Library/Frameworks/Mono.framework/Versions/Current/Commands/mono', '/Users/joerg/Downloads/CSpect2_16_5/CSpect.exe -zxnext -w4 -r -nextrom -mouse -sound -mmc=/Users/joerg/Downloads/tbblue.mmc')
    end;
  end;
end;

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

procedure DoFiles;
var
  Pattern, S: String;
  Dir: SearchRec;
  I: Integer;
begin
  Write('Mask: ');
  ReadLn(Pattern);
  if Pattern = '' then Pattern := '*';

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

function TermStr(S: String): String;
var
  P: Integer;
  T: String;
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

procedure DoOptions;
var
  C: Char;
begin
  repeat
    Write(TermStr('~Target:   '), BinaryStr[Binary]);
    if Binary = btDot then WriteLn('  ', TermStr('~Graphics: '), GraphicsStr[Graphics]) else WriteLn;
    WriteLn(TermStr('~Optimize: '), YesNoStr[Optimize], '       ', TermStr('~Back'));

    C := GetKey;
    case C of
      't': if Binary = btCom then Binary := btDot else Binary := btCom;
      'g': if Graphics = gmHighRes then Graphics := gmNone else Inc(Graphics);
      'o': Optimize := not Optimize;
    end;
  until C = 'b';
end;

procedure Copyright;
begin
  WriteLn('----------------------------------------');
  WriteLn('PL/0 Compiler for Z80       Version 1.50');
  WriteLn;
  WriteLn('Copyright (C) 2020-2022 by Jörg Pleumann');
  WriteLn('----------------------------------------');
  WriteLn;
end;

procedure Interactive;
var
  C: Char;
  I: Integer;
begin
  while True do
  begin
    Write(#27'[2J'#27'[H');

    Copyright;

    WriteLn(TermStr('~Directory: '), FExpand('.'));
    WriteLn;
    
    Write(TermStr('~Work file: '), FRelative(WorkFile, FExpand('.')));
    if WorkFile <> '' then
    begin
      I := FSize(WorkFile);
      if I = -1 then WriteLn(' (new file)') else WriteLn(' (', I, ' bytes)');
    end
    else WriteLn;

    Write(TermStr('~Main file: '), FRelative(MainFile, FExpand('.')));
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
        'd': DoDirectory;
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

procedure Parameters;
var
  Ide: Boolean;
begin
  if ParamCount = 0 then
  begin
    WriteLn('Usage:');
    WriteLn('  pl0 { <option> } <input>');
    WriteLn;
    WriteLn('Options:');
(*    WriteLn('  --asm <path>   sets assembler binary'); *)
    WriteLn('  --com          compiles to CP/M ''.com''');
    WriteLn('  --dot          compiles to Next ''.dot''');
    WriteLn('  --gfx <lo|hi>  enables graphics mode');
    WriteLn('  --opt          enables optimizations');
(*  WriteLn('  --chk          enables run-time checks'); *)
    WriteLn('  --ide          starts interactive mode');
    WriteLn;
    Halt(1);
  end;

  Ide := False;

  I := 1;  
  SrcFile := ParamStr(I);
  while Copy(SrcFile, 1, 2) = '--' do
  begin
    if SrcFile = '--asm' then
    begin
      AsmTool := ParamStr(I + 1);
      I := I + 1;
    end
    else if SrcFile = '--com' then
      Binary := btCom
    else if SrcFile = '--dot' then
      Binary := btDot
    else if SrcFile = '--gfx' then
    begin
      S := LowerStr(ParamStr(I + 1));

      if S = 'lo' then
        Graphics := gmLowRes
      else if S = 'hi' then
        Graphics := gmHighRes
      else
        Error('Invalid graphics mode: ' + S);

      I := I + 1;
    end
    else if SrcFile = '--opt' then
      Optimize := True
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
    if Pos('.', SrcFile)=0 then SrcFile := SrcFile + '.pas';
    if FSize(SrcFile) < 0 then Error('Input file does not exist');
    AsmFile := ChangeExt(SrcFile, '.z80');
  end;

  if Ide then
  begin
    if SrcFile <> '' then WorkFile := FExpand(SrcFile);
    Interactive;
  end
  else Build;

  WriteLn;
end;

(* -------------------------------------------------------------------------- *)
(* --- Main program --------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

begin
  Copyright;

  HomeDir := GetEnv('PL0_HOME');
  if HomeDir = '' then
    HomeDir := ParentDir(FExpand(ParamStr(0)));

  AsmTool := GetEnv('PL0_ASM');

  AltEditor := GetEnv('TERM_PROGRAM') = 'vscode';

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
