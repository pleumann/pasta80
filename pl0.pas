program PL0;

{$Mode delphi}

uses
  Dos;

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

procedure Error(Message: String);
var  I: Integer;
begin
  with Source[Include] do
  begin
    WriteLn;
    WriteLn(Buffer);
    for I := 1 to Column - 1 do Write(' ');
    WriteLn('^');
    WriteLn('*** Error: ', Message, ' in line ', Line, ', column ', Column);
    WriteLn();
    Halt(1);
  end;
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
  TSymbolClass = (scConst, scType, scArrayType, scRecordType, scEnumType, scStringType, scVar, scProc, scFunc, scScope);

  PSymbol = ^TSymbol;
  TSymbol = record
    Name: String;
    Kind: TSymbolClass;
    DataType: PSymbol;
    ArgTypes: array[0..15] of PSymbol;
    ArgIsRef: array[0..15] of Boolean;
    Level: Integer;
    Value: Integer;
    StrVal: String;
    Tag: String;
    Bounds: Integer;
    Prev, Next: PSymbol;
    IsMagic: Boolean;
    IsRef: Boolean;
    IsStdCall: Boolean;
  end;

var
  SymbolTable: PSymbol = nil;
  Level, Offset: Integer;
  dtInteger, dtBoolean, dtChar, dtByte, dtString: PSymbol;

  AddrFunc, SizeFunc, OrdFunc, OddFunc, EvenFunc, PredFunc, SuccFunc: PSymbol;

procedure OpenScope();
var
  Sym: PSymbol;
begin
  New(Sym);
  Sym^.Kind := scScope;
  Sym^.Prev := SymbolTable;
  Sym^.Value := Offset;
  SymbolTable := Sym;

  Level := Level + 1;
  Offset := 0;
end;

procedure Emit(Tag, Instruction, Comment: String); forward;

procedure CloseScope();
var
  Sym: PSymbol;
  Kind: TSymbolClass;
begin
  repeat
    Kind := SymbolTable^.Kind;
    Sym := SymbolTable^.Prev;
    Offset := SymbolTable^.Value;
    Dispose(SymbolTable);
    SymbolTable := Sym;
  until Kind = scScope;

  Level := Level - 1;
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
  Sym^.Kind := Kind;
  Sym^.Name := Name;
  Sym^.Level := Level;
  Sym^.Value := 0;
  Sym^.Prev := SymbolTable;
  Sym^.Bounds := Bounds;
  Sym^.Tag := '';
  Sym^.IsMagic := False;
  Sym^.IsRef := False;
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
  if Level = 1 then
  begin
    Sym^.Value := Offset;
    Offset := Offset + DataType^.Value;
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

  Sym := CreateSymbol(scArrayType, '', 65536);
  Sym^.DataType := dtByte;

  Sym2 := CreateSymbol(scVar, 'Mem', 0);
  Sym2^.DataType := Sym;
  Sym2^.Tag := '0';
  Sym2^.Value := 0;

  AddrFunc := RegisterBuiltIn(scFunc, 'Addr', 1, '');
  AddrFunc^.IsMagic := True;

  SizeFunc := RegisterBuiltIn(scFunc, 'SizeOf', 1, '');
  SizeFunc^.IsMagic := True;

  OrdFunc := RegisterBuiltIn(scFunc, 'Ord', 1, '');
  OrdFunc^.IsMagic := True;

  OddFunc := RegisterBuiltIn(scFunc, 'Odd', 1, '');
  OddFunc^.IsMagic := True;

  EvenFunc := RegisterBuiltIn(scFunc, 'Even', 1, '');
  EvenFunc^.IsMagic := True;

  PredFunc := RegisterBuiltIn(scFunc, 'Pred', 1, '');
  PredFunc^.IsMagic := True;

  SuccFunc := RegisterBuiltIn(scFunc, 'Succ', 1, '');
  SuccFunc^.IsMagic := True;

  Sym := RegisterBuiltIn(scFunc, 'Length', 1, '__length');
  Sym^.ArgTypes[0] := dtString;
  Sym^.DataType := dtInteger;
  Sym^.IsStdCall := True;

  Sym := RegisterBuiltIn(scFunc, 'Concat', 2, '__concat');
  Sym^.ArgTypes[0] := dtString;
  Sym^.ArgTypes[1] := dtString;
  Sym^.DataType := dtString;
  Sym^.IsStdCall := True;

  Sym := RegisterBuiltIn(scFunc, 'Pos', 2, '__pos');
  Sym^.ArgTypes[0] := dtString;
  Sym^.ArgTypes[1] := dtString;
  Sym^.DataType := dtInteger;
  Sym^.IsStdCall := True;

  Sym := RegisterBuiltIn(scFunc, 'Copy', 3, '__copy');
  Sym^.ArgTypes[0] := dtString;
  Sym^.ArgTypes[1] := dtInteger;
  Sym^.ArgTypes[2] := dtInteger;
  Sym^.DataType := dtString;
  Sym^.IsStdCall := True;

  Sym := RegisterBuiltIn(scProc, 'Insert', 3, '__insert');
  Sym^.ArgTypes[0] := dtString;
  Sym^.ArgTypes[1] := dtString;
  Sym^.ArgTypes[1].IsRef := True;
  Sym^.ArgIsRef[1] := True;
  Sym^.ArgTypes[2] := dtInteger;
  Sym^.IsStdCall := True;

  Sym := RegisterBuiltIn(scProc, 'Delete', 3, '__delete');
  Sym^.ArgTypes[0] := dtString;
  Sym^.ArgTypes[0].IsRef := True;
  Sym^.ArgIsRef[0] := True;
  Sym^.ArgTypes[1] := dtInteger;
  Sym^.ArgTypes[2] := dtInteger;
  Sym^.IsStdCall := True;

  Sym := RegisterBuiltIn(scProc, 'Poke', 2, '__poke');
  Sym^.ArgTypes[0] := dtInteger;
  Sym^.ArgTypes[1] := dtInteger;

  Sym := RegisterBuiltIn(scFunc, 'Random', 1, '__random');
  Sym^.ArgTypes[0] := dtInteger;
  Sym^.DataType := dtInteger;

  RegisterBuiltIn(scProc, 'ClrScr', 0, '__clrscr');
  
  Sym := RegisterBuiltIn(scProc, 'GotoXY', 2, '__gotoxy');
  Sym^.ArgTypes[0] := dtInteger;
  Sym^.ArgTypes[1] := dtInteger;

  RegisterBuiltIn(scProc, 'TextColor', 1, '__textfg')^.ArgTypes[0] := dtInteger;
  RegisterBuiltIn(scProc, 'TextBackground', 1, '__textbg')^.ArgTypes[0] := dtInteger;

  RegisterBuiltIn(scProc, 'CursorOn', 0, '__cursor_on');
  RegisterBuiltIn(scProc, 'CursorOff', 0, '__cursor_off');

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
            toIdent, toNumber, toString,
            toAdd, toSub, toMul, toDiv,
            toEq, toNeq, toLt, toLeq, toGt, toGeq,
            toLParen, toRParen, toLBrack, toRBrack,
            toBecomes, toComma, toColon, toSemicolon, toPeriod, toRange,
            toAnd, toOr, toXor, toNot, toMod,
            toArray, toOf,
            toProgram, toBegin, toEnd, toConst, toType, toVar,
            toStringKw, toRecord, toProcedure, toFunction,
            toIf, toThen, toElse, toWhile, toDo, toRepeat, toUntil,
            toFor, toTo, toDownTo, toCont, toBreak, toExit, toWrite, toWriteLn,
            toEof);

  TScanner = record
    Token:    TToken;
    StrValue: String;
    NumValue: Integer;
  end;

const
  TokenStr: array[TToken] of String = 
           ('<nul>',
            'Identifier', 'Number', 'String',
            '+', '-', '*', '/',
            '=', '#', '<', '<=', '>', '>=',
            '(', ')', '[', ']',
            ':=', ',', ':', ';', '.', '..',
            'and', 'or', 'xor', 'not', 'mod',
            'array', 'of',
            'program', 'begin', 'end', 'const', 'type', 'var',
            'string', 'record', 'procedure', 'function',
            'if', 'then', 'else', 'while', 'do', 'repeat', 'until',
            'for', 'to', 'downto', 'continue', 'break', 'exit', 'write', 'writeln',
            '<eof>');

  FirstKeyword = toAnd;
  LastKeyword = toWriteLn;

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
      NumValue := Ord(C) - Ord('0');
      C := GetChar;
      while IsDecDigit(C) do
      begin
        NumValue := 10 * NumValue + (Ord(C) - Ord('0'));
        C := GetChar;
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
          if C = '''' then
          begin
            C := GetChar;
            if S = '''' then StrValue := StrValue + '''';
          end
          else
          begin
            while (C <> '''') and (C <> #26) do
            begin
              StrValue := StrValue + C;
              C := GetChar;
            end;
            if C = #26 then Error('Unterminated String') else C := GetChar;
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
          if C = '*' then
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
          if C = '.' then
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
    Tag, Instruction, Comment: String[64];
    Next, Prev: PCode;
  end;

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
    if Sym^.Value >= 3 then
      EmitI('pop bc');    
    if Sym^.Value >= 2 then
      EmitI('pop de');    
    if Sym^.Value >= 1 then
      EmitI('pop hl');
  end;

  EmitI('call ' + Sym^.Tag);

  if not Sym^.IsStdCall then
  begin
    if Sym^.Kind = scFunc then
      EmitI('push hl');
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
    Emit('main', 'call __init', '')
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

  if Sym^.Level = 1 then
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

    if (Left = dtInteger) and (Right = dtByte) then
    begin
      TypeCheck := dtInteger;
      Exit;
    end
    else if (Left = dtByte) and (Right = dtInteger) then
    begin
      TypeCheck := dtByte;
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

  while Scanner.Token in [toLBrack, toPeriod] do
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
    else
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


procedure ParseWriteArgument;
begin
    if Scanner.Token = toString then
    begin
      EmitPrintStr(Scanner.StrValue);
      NextToken;
    end
    else
      EmitWrite(ParseExpression);
end;

function ParseBuiltInFunction(Func: PSymbol): PSymbol;
var
  Sym: PSymbol;
begin
  if Func^.Kind <> scFunc then
    Error('Not a built-in function: ' + Func^.Name);

  NextToken; Expect(toLParen); NextToken;

  if Func = AddrFunc then
  begin
    Expect(toIdent);
    Sym := LookupGlobal(Scanner.StrValue);
    if Sym = nil then Error('Identifier "' + Scanner.StrValue + '" not found.');
    NextToken;
    ParseVariableAccess(Sym);
    ParseBuiltInFunction := dtInteger;
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
  else
    Error('Cannot handle: ' + Func^.Name);

    Expect(toRParen); NextToken;
end;

function ParseFactor: PSymbol;
var
  Sym: PSymbol;T: PSymbol;
  Op: TToken;
  Tag: String;
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
      EmitLiteral(Sym^.Value);
      NextToken;
    end
    else if (Sym^.Kind = scFunc) and (Sym^.IsMagic) then
    begin
      T := ParseBuiltInFunction(Sym);
    end
    else if Sym^.Kind = scFunc then
    begin
      T := Sym^.DataType; (* Type = Type(func) *)
      if Sym^.IsStdCall then EmitSpace(T.Value); (* Result *)
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
  else if Scanner.Token = toNumber then
  begin
    T := dtInteger;
    EmitLiteral(Scanner.NumValue);
    NextToken;
  end
  (* true false + String constants *)
  else if Scanner.Token = toLParen then
  begin
    NextToken; T := ParseExpression; Expect(toRParen); NextToken;
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

  if Op = toSub then EmitLiteral(0);

  T := ParseTerm;

  if Op <> toNone then T := TypeCheck(dtInteger, T, tcExpr); // +=Byte, -=Integer

  if Op = toSub then EmitBinOp(toSub, T);

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
  else if T = dtString then
    while Scanner.Token = toAdd do
    begin
      Op := Scanner.Token;
      NextToken;
      T := TypeCheck(T, ParseTerm, tcExact);
      EmitI('call __stradd');
      EmitClear(256);
    end;

(* TODO Error case? *)

  (* WriteLn('Type of SimpleExpression is ', T); *)

  ParseSimpleExpression := T;
end;

function ParseExpression: PSymbol;
var
  Op: TToken;
  T: PSymbol;
begin
  T := ParseSimpleExpression;
  if (Scanner.Token >= toEq) and (Scanner.Token <= toGeq) then
  begin
    Op := Scanner.Token;
    NextToken;
    (*
      * ii ib bi bb zz cc 
      *)
    TypeCheck(T, ParseSimpleExpression, tcExpr); // Check type, but ignore result.
    if T^.Kind = scStringType then
      EmitStrOp(Op)
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

procedure ParseStatement(ContTarget, BreakTarget: String);
var
  Sym: PSymbol;
  Tag, Tag2, Tag3, Tag4: String;
  Delta: Integer;
  (*T: PSymbol;*)
  NewLine: Boolean;
begin
  if Scanner.Token = toIdent then
  begin
    if UpperStr(Scanner.StrValue) = 'ASSERT' then
    begin
      Tag := GetLabel('assert');
      NextToken;
      Expect(toLParen);

      NextToken;
      TypeCheck(dtBoolean, ParseExpression, tcExact);

      EmitJumpIf(True, Tag);
      EmitAssertFailed(AddString(Source[Include].Name), Source[Include].Line);
      Emit(Tag, '', '');

      Expect(toRParen);
      NextToken;

      Exit;
    end;

    Sym := LookupGlobal(Scanner.StrValue);
    if Sym = nil then Error('Identifier "' + Scanner.StrValue + '" not found.');

    if Sym^.Kind = scProc then
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
  else if Scanner.Token = toCont then
  begin
    if ContTarget = '' then Error('Not in loop');
    NextToken;
    Emit('', 'jp ' + ContTarget, 'Continue');    
  end
  else if Scanner.Token = toBreak then
  begin
    if BreakTarget = '' then Error('Not in loop');
    NextToken;
    Emit('', 'jp ' + BreakTarget, 'Break');    
  end
  else if Scanner.Token = toExit then
  begin
    NextToken;
    Emit('', 'jp ' + ExitTarget, 'Exit');    
  end
  else if (Scanner.Token = toWrite) or (Scanner.Token = toWriteLn) then
  begin
    NewLine := Scanner.Token = toWriteLn;
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

    if NewLine then EmitPrintNewLine;
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
  else
  begin
    Expect(toIdent);
    DataType := LookupGlobal(Scanner.StrValue);

    if DataType = nil then
      Error('Type not found: ' + Scanner.StrValue);
    if not (DataType^.Kind in [scType, scArrayType, scRecordType, scEnumType, scStringType]) then
      Error('Not a type: ' + Scanner.StrValue);

    NextToken;
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
  DataType: PSymbol;
  Low, High: Integer;
begin
  Low := 0;
  High := -1;

  Old := SymbolTable;
  ParseVar;
  while Scanner.Token = toComma do
  begin
    NextToken; ParseVar;
  end;

  Expect(toColon);
  NextToken;

  DataType := ParseTypeDef;

    // WriteLn('Var type is ', DataType^.Name);
    // if DataType^.Kind = scArrayType then WriteLn(' of ', DataType^.DataType^.Name);

  while Old<>SymbolTable do
  begin
    Old := Old^.Next;
    if Old^.Kind = scVar then
    begin
      SetDataType(Old, DataType, High - Low + 1);

      if Old^.Level = 1 then
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
  if not (DataType.Kind in [scType, scArrayType, scRecordType, scEnumType, scStringType]) then Error('Type expected');

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

procedure ParseBlock(Sym: PSymbol);
var
  NewSym, ResVar: PSymbol;
  Token: TToken;
  Name: String;
  IsRef: Boolean;
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
  end;

  if Scanner.Token = toType then
  begin
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
  end;

  if Scanner.Token = toVar then
  begin
    NextToken;
    repeat
      parseVarList();
      Expect(toSemicolon);
      NextToken;
    until Scanner.Token <> toIdent;
  end;

  while (Scanner.Token = toProcedure) or (Scanner.Token = toFunction) do
  begin
    Token := Scanner.Token;
    NextToken; Expect(toIdent);

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

    NewSym^.IsStdCall := True;
    OpenScope;
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
    NextToken; ParseBlock(NewSym);
    CloseScope;
    Expect(toSemicolon);
    NextToken;

    EmitC('');
  end;

  ExitTarget := GetLabel('exit');
  EmitPrologue(Sym);
  parseStatement('', '');
  EmitEpilogue(Sym);
end;

procedure ParseProgram;
begin
  OpenScope;
  RegisterAllBuiltIns((Binary = btDot) and (Graphics <> gmNone));
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
  CloseScope;
end;

(* -------------------------------------------------------------------------- *)
(* --- Main program --------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

var
  SrcFile, AsmFile, BinFile, HomeDir, AsmTool, S: String; 
  I: Integer;

begin
  WriteLn('PL/0 Compiler for Z80 Version 1.0');
  WriteLn('Copyright (c) 2020 by Joerg Pleumann');
  WriteLn;

  HomeDir := GetEnv('PL0_HOME');
  if HomeDir = '' then
    HomeDir := ParentDir(FExpand(ParamStr(0)));

  AsmTool := GetEnv('PL0_ASM');

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
    else
      Error('Invalid option: ' + SrcFile);

    I := I + 1;
    SrcFile := ParamStr(I);
  end;

  if SrcFile = '' then
  begin
    WriteLn('Usage:');
    WriteLn('  pl0 { <option> } <input>');
    WriteLn;
    WriteLn('Options:');
    WriteLn('  --asm <path>   sets assembler binary');
    WriteLn('  --com          selects CP/M .com target');
    WriteLn('  --dot          selects Next .dot target');
    WriteLn('  --gfx <lo|hi>  enables graphics mode');
    WriteLn('  --opt          enables optimizations');
(*  WriteLn('  --chk          enables run-time checks'); *)
    WriteLn;
    Halt(1);
  end;

  if Pos('.', SrcFile)=0 then SrcFile := SrcFile + '.pas';

  AsmFile := ChangeExt(SrcFile, '.z80');

  if Binary = btCom then
    BinFile := ChangeExt(SrcFile, '.com')
  else if Binary = btDot then
    BinFile := ChangeExt(SrcFile, '.dot');

  WriteLn('Compiling...');

  OpenInput(SrcFile);
  OpenTarget(AsmFile);
  EmitHeader(HomeDir, SrcFile);  (* TODO Move this elsewhere. *)
  NextToken;
  ParseProgram;
  EmitFooter();                  (* TODO Move this elsewhere. *)
  CloseTarget();
  CloseInput();

  if AsmTool <> '' then
  begin
    WriteLn('Assembling...');

    Exec(AsmTool, AsmFile + ' ' + BinFile);
    if DosError <> 0 then
      Error('Error ' + Int2Str(DosError) + ' starting ' + AsmTool);
    if DosExitCode <> 0 then
      Error('Failure! :(')
  end;

  WriteLn('Success! :)');
end.

(*
TODO
- Record typed constants
- Too many arguments
- var X absolute $1234
- var X absolute Y;
- Complex types on the stack (parameters, locals), what does Turbo 3 allow?
- Proper strings
- Enums as array indices
- Alternative array syntax
- Subrange types
- Set types
- Floating point type(s)
- Pointers & heap management
- Allow assignment from Byte to Integer (TypeCheck probably needs to return type)
- Integrate String literals as String data type, allow variables and parameters.
*)
