program PL0;

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
  Str(I:Abs(N), S);
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
  TDataType = (dtInteger, dtBoolean, dtChar, dtByte, dtString);

const
  TypeName: array [TDataType] of String = ('Integer', 'Boolean', 'Char', 'Byte', 'String');

type
  TSymbolClass = (scConst, scVar, scProc, scFunc, scScope);

  PSymbol = ^TSymbol;
  TSymbol = record
    Name: String;
    Kind: TSymbolClass;
    DataType: TDataType;
    ArgTypes: array[0..15] of TDataType;
    Level: Integer;
    Value: Integer;
    StrVal: String;
    Tag: String;
    Bounds: Integer;
    Prev, Next: PSymbol;
  end;

var
  SymbolTable: PSymbol = nil;
  Level, Offset: Integer;

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
    Sym^.ArgTypes[I-1] := Sym2^.DataType;
    I := I - 1;
    Sym2 := Sym2^.Prev;
  end;

  Offset := -2;
end;

function CreateSymbol(Kind: TSymbolClass; Name: String; Bounds: Integer): PSymbol;
var
  Sym: PSymbol;
  Size: Integer;
begin
  if LookupLocal(Name) <> nil then
  begin
    Error('Duplicate symbol "' + Name + '".');
  end;

  New(Sym);
  Sym^.Kind := Kind;
  Sym^.Name := Name;
  Sym^.Level := Level;
  Sym^.Prev := SymbolTable;
  Sym^.Bounds := Bounds;
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

procedure SetDataType(Sym: PSymbol; DataType: TDataType; Bounds: Integer);
var
  Size: Integer;
begin
  Sym^.DataType := DataType;
  Sym^.Bounds := Bounds;

  if Bounds = 0 then Size := 2 else Size := 2 * Bounds;
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

procedure RegisterBuiltIn(Kind: TSymbolClass; Name: String; Args: Integer; Tag: String);
var
  Sym: PSymbol;
begin
  Sym := CreateSymbol(Kind, Name, 0);
  Sym^.Level := 0;
  Sym^.Value := Args;
  Sym^.Tag := Tag;
end;

procedure RegisterAllBuiltIns(Graphics: Boolean);
begin
  RegisterBuiltIn(scFunc, 'Random', 1, '__random');
  RegisterBuiltIn(scProc, 'ClrScr', 0, '__clrscr');
  RegisterBuiltIn(scProc, 'GotoXY', 2, '__gotoxy');
  RegisterBuiltIn(scProc, 'TextColor', 1, '__textfg');
  RegisterBuiltIn(scProc, 'TextBackground', 1, '__textbg');
  RegisterBuiltIn(scProc, 'CursorOn', 0, '__cursor_on');
  RegisterBuiltIn(scProc, 'CursorOff', 0, '__cursor_off');

  if Graphics then
  begin
    RegisterBuiltIn(scProc, 'SetPixel', 3, '__set_pixel');
    RegisterBuiltIn(scFunc, 'GetPixel', 2, '__get_pixel');
  end;
end;

(* --------------------------------------------------------------------- *)
(* --- Scanner --------------------------------------------------------- *)
(* --------------------------------------------------------------------- *)

type
  TToken = (toNone,
            toIdent, toNumber, toString,
            toAdd, toSub, toMul, toDiv, toMod,
            toEq, toNeq, toLt, toLeq, toGt, toGeq,
            toLParen, toRParen, toLBrack, toRBrack,
            toSay, toAsk, toBecomes, toComma, toColon, toSemicolon, toPeriod, toRange,
            toAnd, toOr, toXor, toNot, toMod2,
            toOdd, toOrd,
            toBoolean, toInteger, toChar, toByte, toStringT, toTrue, toFalse, toArray, toOf,
            toProgram, toBegin, toEnd, toConst, toVar, toProcedure, toFunction,
            toCall, toIf, toThen, toElse, toWhile, toDo, toRepeat, toUntil,
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
            '+', '-', '*', '/', '%',
            '=', '#', '<', '<=', '>', '>=',
            '(', ')', '[', ']',
            '!', '?', ':=', ',', ':', ';', '.', '..',
            'and', 'or', 'xor', 'not', 'mod',
            'odd', 'ord',
            'boolean', 'integer', 'char', 'byte', 'string', 'true', 'false', 'array', 'of',
            'program', 'begin', 'end', 'const', 'var', 'procedure', 'function',
            'call', 'if', 'then', 'else', 'while', 'do', 'repeat', 'until',
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
      if Token = toMod2 then Token := toMod;
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
    else if C = '''' then
    begin
      Token := toString;
      C := GetChar;
      while (C <> '''') and (C <> #26) do
      begin
        StrValue := StrValue + C;
        C := GetChar;
      end;

      if C = #26 then Error('Unterminated String') else C := GetChar;
    end
    else if C = '"' then
    begin
      Token := toString;
      C := GetChar;
      while (C <> '"') and (C <> #26) do
      begin
        StrValue := StrValue + C;
        C := GetChar;
      end;

      if C = #26 then Error('Unterminated String') else C := GetChar;
    end
    else case C of 
      '+': 
      begin
        Token := toAdd;
        C := GetChar;
      end;
      '-': begin
        Token := toSub;
        C := GetChar;
      end;
      '*': begin
        Token := toMul;
        C := GetChar;
      end;
      '/': begin
        Token := toDiv;
        C := GetChar;
      end;
      '%': begin
        Token := toMod;
        C := GetChar;
      end;
      '=': begin
        Token := toEq;
        C := GetChar;
      end;
      '#': begin
        Token := toNeq;
        C := GetChar;
      end;
      '<': begin
        C := GetChar;
        if C = '>' then
        begin
          Token := toNeq;
          C := GetChar;
        end
        else if C = '=' then
        begin
          Token := toLeq;
          C := GetChar;
        end
        else Token := toLt;
      end;
      '>': begin
        C := GetChar;
        if C = '=' then
        begin
          Token := toGeq;
          C := GetChar;
        end
        else Token := toGt;
      end;
      '(': begin
        Token := toLParen;
        C := GetChar;

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
      end;
      ')': begin
        Token := toRParen;
        C := GetChar;
      end;
      '[': begin
        Token := toLBrack;
        C := GetChar;
      end;
      ']': begin
        Token := toRBrack;
        C := GetChar;
      end;
      '!': begin
        Token := toSay;
        C := GetChar;
      end;
      '?': begin
        Token := toAsk;
        C := GetChar;
      end; 
      ':': begin
        Token := toColon;
        C := GetChar;
        if C = '=' then
        begin
          Token := toBecomes;
          C := GetChar;
        end;
      end;
      ',': begin
        Token := toComma;
        C := GetChar;
      end;
      ';': begin
        Token := toSemicolon;
        C := GetChar;
      end;
      '.': begin
        Token := toPeriod;
        C := GetChar;
        if C = '.' then
        begin
          Token := toRange;
          C := GetChar;
        end;
      end
      else
        Error('Invalid token.');
    end;

    (* Write('{', Token, '->', StrValue , '}'); *)
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
  LastTag, LastInstruction, LastComment: String;
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
  end;

  DoOptimize := False;
end;

procedure Emit(Tag, Instruction, Comment: String);
var
  TwoOp, S: String;
  Temp: PCode; 
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

procedure EmitCall(Sym: PSymbol);
var
  I: Integer;
begin
  if Sym^.Level = 0 then
  begin
    if Sym^.Value >= 3 then
      EmitI('pop bc');    
    if Sym^.Value >= 2 then
      EmitI('pop de');    
    if Sym^.Value >= 1 then
      EmitI('pop hl');
  end;

  EmitI('call ' + Sym^.Tag);

  if Sym^.Level = 0 then
  begin
    if Sym^.Kind = scFunc then
      EmitI('push hl');
  end
  else
  begin
  for I := 1 to Sym^.Value do
    Emit('', 'pop hl', 'Cleanup arguments');
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

  Emit0('', 'jp main', '');
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

procedure EmitStrings();
var
  Temp: PStringLiteral;
begin
  Temp := Strings;

  while Temp <> nil do
  begin
    EmitC('');
    Emit(Temp^.Tag, 'db ' + Int2Str(Length(Temp^.Value)) + ',"' + Temp^.Value + '"', '');

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
  if Sym = Nil then
    EmitC('main entry point')
  else if Sym^.Kind = scFunc then
    EmitC('function ' + Sym^.Name)
  else
    EmitC('procedure ' + Sym^.Name);

  EmitC('');

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

    I := Offset;
    if I < -2 then
    begin
      Emit('', 'ld de,0', 'Space for locals');
      while I < -2 do
      begin
        EmitI('push de');
        I := I + 2;
      end;
    end;
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

procedure EmitGetVar(Sym: PSymbol);
var
  L: Integer;
begin
  L := Level - Sym^.Level;

  if Sym^.Level = 1 then
  begin
    if Sym^.Bounds <> 0 then
    begin
      EmitI('pop de');
      Emit('', 'ld hl,' + Sym^.Tag, 'Get global ' + Sym^.Name);
      EmitI('add hl,de');
      EmitI('add hl,de');
      EmitI('ld de,(hl)');
      EmitI('push de');
    end
    else
    begin
      Emit('', 'ld hl,(' + Sym^.Tag + ')', 'Get global ' + Sym^.Name);
      EmitI('push hl');
    end;
  end
  else if L = 0 then
  begin
    // ld hl,Sym^.Value
    Emit('', 'ld de,(' + RelativeAddr('ix', Sym^.Value) + ')', 'Get local ' + Sym^.Name);
    EmitI('push de');
  end
  else
  begin
    // ld hl,(display+...)
    // ld de,sym^.value
    // ld add hl,de
    // ld de,(hl)
    // push de
    Emit('', 'ld iy,(display+' + Int2Str(Sym^.Level * 2) + ')', 'Get outer ' + Sym^.Name);
    EmitI('ld de,(' + RelativeAddr('iy', Sym^.Value) + ')');
    EmitI('push de');
  end
end;
 
procedure EmitSetVar(Sym: PSymbol; Again: Boolean);
var
  L: Integer;
begin
  L := Level - Sym^.Level;

  if Sym^.Level = 1 then
  begin
    if Sym^.Bounds <> 0 then
    begin
      EmitI('pop de');
      EmitI('pop hl');
      Emit('', 'ld bc,' + Sym^.Tag, 'Set global ' + Sym^.Name);
      EmitI('add hl,hl');
      EmitI('add hl,bc');
      EmitI('ld (hl),de');
      if Again then EmitI('push de');
    end
    else
    begin
      EmitI('pop de');
      Emit('', 'ld (' + Sym^.Tag + '),de', 'Set global ' + Sym^.Name);
      if Again then EmitI('push hl');
    end;
  end
  else if L = 0 then
  begin
    EmitI('pop de');
    Emit('', 'ld (' + RelativeAddr('ix', Sym^.Value) + '),de', 'Set local ' + Sym^.Name);
    if Again then EmitI('push de');
  end
  else
  begin
    EmitI('pop de');
    Emit('', 'ld iy,(display+' + Int2Str(Sym^.Level * 2) + ')', 'Set outer ' + Sym^.Name);
    EmitI('ld (' + RelativeAddr('iy', Sym^.Value) + '),de');
    if Again then EmitI('push de');
  end
end;

procedure EmitLiteral(Value: Integer);
begin
  Emit('', 'ld de,' + Int2Str(Value), 'Literal ' + Int2Str(Value));
  EmitI('push de');
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

procedure EmitBinOp(Op: TToken; DataType: TDataType);
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

procedure EmitUnOp(Op: TToken; DataType: TDataType);
begin
  case Op of
    toOdd: 
      begin
        Emit('', 'pop hl', 'Odd');
        EmitI('ld a,l');
        EmitI('and 1');
        EmitI('ld l,a');
        EmitI('ld h,0');
        EmitI('push hl');
      end;
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

procedure EmitInputNum(S: String);
begin
  Emit('', 'call __getn', 'Get ' + S);
  EmitI('push de');
  Emit('', 'call __newline', '');
end;

procedure EmitWrite(DataType: TDataType);
begin
  Emit('', 'pop hl', '');
  case DataType of
    dtInteger, dtByte:
      EmitI('call __putn');
    dtBoolean:
      EmitI('call __putb');
    dtChar:
      begin
        EmitI('ld a,l');
        EmitI('call __putc');
      end;
    dtString:
      EmitI('call __puts');
  end;
end;

procedure EmitPrintStr(S: String);
var
  Sym: PSymbol;
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
function TypeCheck(Left, Right: TDataType; Check: TTypeCheck): TDataType;
begin
  if Left = Right then
  begin
    TypeCheck := Left;
    Exit;
  end;

  if (Left = dtInteger) and (Right = dtByte) or (Left = dtByte) and (Right = dtInteger) then
  begin
    TypeCheck := dtInteger;
    Exit;
  end;

  Error('Type error, expected ' + TypeName[Left] + ', got ' + TypeName[Right]);
end;

function ParseExpression: TDataType; forward;

procedure ParseArguments(Sym: PSymbol);
var
  I: Integer;
begin
  I := 0;
  if Scanner.Token = toLParen then
  begin
    NextToken;

    TypeCheck(Sym^.ArgTypes[I], ParseExpression, tcAssign);
    I := I + 1;

    while Scanner.Token = toComma do
    begin
      NextToken;
      TypeCheck(Sym^.ArgTypes[I], ParseExpression, tcAssign);
      I := I + 1;
    end;

    Expect(toRParen);
    NextToken;
  end;

  if I <> Sym^.Value then Error('Wrong number of arguments');
end;


procedure ParseWriteArgument;
var
  T: TDataType;
begin
    if Scanner.Token = toString then
    begin
      EmitPrintStr(Scanner.StrValue);
      NextToken;
    end
    else
      EmitWrite(ParseExpression);
end;

function ParseFactor: TDataType;
var
  Sym: PSymbol;T: TDataType;
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
      T := Sym^.DataType;
      NextToken;
      if Scanner.Token = toLBrack then
      begin
        if Sym^.Bounds = 0 then Error('" ' + Sym^.Name + '" is not an array.');
        NextToken;
        TypeCheck(dtInteger, ParseExpression, tcAssign); (* Index now on stack *)
        Expect(toRBrack);
        NextToken;
      end
      else if Sym^.Bounds <> 0 then Error('" ' + Sym^.Name + '" is an array.');
      EmitGetVar(Sym);
    end
    else if Sym^.Kind = scConst then
    begin
      T := Sym^.DataType;
      EmitLiteral(Sym^.Value);
      NextToken;
    end
    else if Sym^.Kind = scFunc then
    begin
      T := Sym^.DataType; (* Type = Type(func) *)
      if Sym^.Level <> 0 then EmitLiteral(0); (* Result *)
      NextToken;
      ParseArguments(Sym);
      EmitCall(Sym);     
    end
    else
      Error('"' + Scanner.StrValue + '" cannot be used in expressions.');
  end
  else if (Scanner.Token = toTrue) or (Scanner.Token = toFalse) then
  begin
    T := dtBoolean;
    if Scanner.Token = toTrue then EmitLiteral(1) else EmitLiteral(0);
    NextToken;
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
  else if Scanner.Token in [toOrd, toInteger, toBoolean, toChar, toByte] then
  begin
    Op := Scanner.Token;

    NextToken;
    Expect(toLParen);
    NextToken;
    ParseExpression();

    case Op of
      toOrd:      T := dtInteger;
      toInteger:  T := dtInteger;
      toBoolean:  T := dtBoolean;
      toChar:     T := dtChar;
      toByte:     T := dtByte;
    end;

    Expect(toRParen);
    NextToken;
  end
  else Error('Factor expected');

  ParseFactor := T;
end;

function ParseTerm: TDataType;
var
  Op: TToken;
  T: TDataType;
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

function ParseSimpleExpression: TDataType;
var
  Op: TToken;
  T: TDataType;
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
    end;

  (* WriteLn('Type of SimpleExpression is ', T); *)

  ParseSimpleExpression := T;
end;

function ParseExpression: TDataType;
var
  Op: TToken;
  T: TDataType;
begin
  if Scanner.Token = toOdd then (* Make this a function later *)
  begin
    NextToken;
    (*TypeCheck(dtInteger,*) T := ParseSimpleExpression (*), tcExpr)*);
    EmitUnOp(toOdd, T);
    T := dtBoolean;
  end 
  else
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
      T := dtBoolean;                       // We know it will be Boolean.
      EmitRelOp(Op);
    end;
  end;

  (* WriteLn('Type of Expression is ', T); *)

  ParseExpression := T;
end;

procedure ParseAssignment(Sym: PSymbol; Again: Boolean);
var
  Sym2: PSymbol;
begin
  if Sym^.Kind <> scVar then Error('"' + Scanner.StrValue + '" not a var.');
  NextToken;
  if Scanner.Token = toLBrack then
  begin
    if Sym^.Bounds = 0 then Error('" ' + Sym^.Name + '" is not an array.');
    NextToken;
    TypeCheck(dtInteger, ParseExpression, tcAssign); (* Index now on stack *)
    Expect(toRBrack);
    NextToken;
  end
  else if Sym^.Bounds <> 0 then Error('" ' + Sym^.Name + '" is an array.');

  if Scanner.Token = toComma then
  begin
    NextToken;
    Expect(toIdent);
    Sym2 := LookupGlobal(Scanner.StrValue);
    if Sym2 = nil then Error('Identifier "' + Scanner.StrValue + '" not found.');
    TypeCheck(Sym^.DataType, Sym2^.DataType, tcExact);
    ParseAssignment(Sym2, true);
  end
  else
  begin
    Expect(toBecomes); NextToken; TypeCheck(Sym^.DataType, ParseExpression, tcAssign);
  end;

  EmitSetVar(Sym, Again);
end;

procedure ParseStatement(ContTarget, BreakTarget: String);
var
  Sym: PSymbol;
  Tag, Tag2, Tag3, Tag4: String;
  Delta: Integer;
  T: TDataType;
  NewLine: Boolean;
begin
  if Scanner.Token = toIdent then
  begin
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
  else if Scanner.Token = toCall then
  begin
    NextToken; Expect(toIdent);
    Sym := LookupGlobal(Scanner.StrValue);
    if Sym = nil then Error('Identifier "' + Scanner.StrValue + '" not found.');
    if Sym^.Kind <> scProc then Error('"' + Scanner.StrValue + '" not a proc.');
    NextToken;

    ParseArguments(Sym);

    EmitCall(Sym);
  end
  else if Scanner.Token = toAsk then
  begin
    NextToken; Expect(toIdent);
    Sym := LookupGlobal(Scanner.StrValue);
    if Sym = nil then Error('Identifier "' + Scanner.StrValue + '" not found.');
    if Sym^.Kind <> scVar then Error('"' + Scanner.StrValue + '" not a var.');
    NextToken;

    EmitInputNum(Scanner.StrValue);
    EmitSetVar(Sym, false);
  end
  else if Scanner.Token = toSay then
  begin
    NextToken;
    EmitWrite(ParseExpression);
    EmitPrintNewLine;
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

    NextToken; Expect(toBecomes); NextToken; TypeCheck(Sym^.DataType, ParseExpression, tcAssign);
    EmitSetVar(Sym, false);

    if Scanner.Token = toTo then
      Delta := 1
    else if Scanner.Token = toDownTo then
      Delta := -1
    else
      Error('"to" or "downto" expected.');

    NextToken; TypeCheck(Sym^.DataType, ParseExpression, tcAssign); Expect(toDo); NextToken; (* final value on stack *)

    Tag := GetLabel('forloop');
    Tag3 := GetLabel('forbreak');
    Tag4 := GetLabel('fornext');

    Emit('', 'pop de','Dup and pre-check limit');
    Emit('', 'push de','');
    Emit('', 'push de', '');

    EmitGetVar(Sym);

    if Delta = 1 then EmitRelOp(toGeq) else EmitRelOp(toLeq); (* Operands swapped! *)

    EmitJumpIf(False, Tag3);

    Emit(Tag, '', '');

    ParseStatement(Tag4, Tag3);

    Emit(Tag4, '', '');
    
    Emit('', 'pop de','Dup and check limit');
    Emit('', 'push de','');
    Emit('', 'push de', '');

    EmitGetVar(Sym);

    if Delta = 1 then EmitRelOp(toGt) else EmitRelOp(toLt); (* Operands swapped! *)

    EmitJumpIf(False, Tag3);

    EmitGetVar(Sym);
    Emit('', 'ld de,' + Int2Str(Delta), 'Inc/dec counter');
    Emit('', 'push de', '');
    EmitBinOp(toAdd, dtInteger);
    EmitSetVar(Sym, false);
  
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

procedure ParseConst;
var
  Sym: PSymbol;
  Sign: Integer;
begin
  Expect(toIdent);
  Sym := CreateSymbol(scConst, Scanner.StrValue, 0);
  NextToken;
  Expect(toEq);
  NextToken;
  if Scanner.Token = toSub then
  begin
    Sign := -1;
    NextToken;
  end
  else Sign := 1;
  Expect(toNumber);

  Sym^.Value := Sign * Scanner.NumValue;
  Sym^.DataType := dtInteger;

  NextToken;
end;

procedure ParseVar;
var
  Name: String;
  Bounds: Integer;
  Sym: PSymbol;
begin
  Expect(toIdent);
  Name := Scanner.StrValue;
  NextToken;

  Bounds := 0;

  if Scanner.Token = toLBrack then
  begin
    NextToken;
    Expect(toNumber);
    Bounds := Scanner.NumValue;
    NextToken;
    Expect(toRBrack);
    NextToken;
  end;

  Sym := CreateSymbol(scVar, Name, Bounds);
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
  Old, Tmp: PSymbol;
  DataType: TDataType;
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
  
  DataType := dtInteger;

  if Scanner.Token = toColon then
  begin
    NextToken;

    if Scanner.Token = toArray then
    begin
      NextToken;
      Expect(toLBrack);
      NextToken;
      Expect(toNumber);
      Low := Scanner.NumValue;
      NextToken;
      Expect(toRange);
      NextToken;
      Expect(toNumber);
      High := Scanner.NumValue;
      NextToken;
      Expect(toRBrack);
      NextToken;
      Expect(toOf);
      NextToken;
    end;

    case Scanner.Token of
      toBoolean: DataType := dtBoolean;
      toInteger: DataType := dtInteger;
      toChar: DataType := dtChar;
      toByte: DataType := dtByte;
      toStringT: DataType := dtString;
      else Error('Type expected');
    end;
    NextToken;
  end;

  while Old<>SymbolTable do
  begin
    Old := Old^.Next;
    SetDataType(Old, DataType, High - Low + 1);

    if Old^.Level = 1 then
    begin
      Old^.Tag := GetLabel('global');
      if Old^.Bounds = 0 then
        Emit0(Old^.Tag, 'ds 2', 'Global ' + Old^.Name)
      else
        Emit0(Old^.Tag, 'ds ' + Int2Str(Old^.Bounds * 2), 'Global ' + Old^.Name);
    end;


//    Tmp^.DataType := DataType;
    //if High >= Low then Tmp^.Bounds := High - Low + 1;
//    Tmp := Tmp^.Prev;
    //Offset := Offset + (High - Low + 1) * 2 - 2;
  end;
end;

procedure ParseBlock(Sym: PSymbol);
var
  NewSym, ResVar, Old, Tmp: PSymbol;
  Token: TToken;
  DataType: TDataType;
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

    OpenScope;
    if Token = toFunction then
    begin
      CreateSymbol(scVar, Scanner.StrValue, 0)^.Tag := 'RESULT';
      SetDataType(SymbolTable, dtInteger, 0);
      ResVar := SymbolTable;
    end;
    NextToken; 
    
    if Scanner.Token = toLParen then
    begin
      NextToken;
      ParseVarList();
      while Scanner.Token = toSemicolon do
      begin
        NextToken;
        ParseVarList();
      end;
      Expect(toRParen);
      NextToken;
    end;

    if Scanner.Token = toColon then
    begin
      NextToken;
      if Scanner.Token = toInteger then
        NewSym^.DataType := dtInteger
      else if Scanner.Token = toBoolean then
        NewSym^.DataType := dtBoolean
      else if Scanner.Token = toChar then
        NewSym^.DataType := dtChar
      else if Scanner.Token = toStringT then
        NewSym^.DataType := dtString
      else
        Error('Type expected');
      ResVar^.DataType := NewSym^.DataType;
      NextToken;
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

  if Pos('.', SrcFile)=0 then SrcFile := SrcFile + '.pl0';

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
- Integrate new types correctly
- Allow assignment from Byte to Integer (TypeCheck probably needs to return type)
- Check why Boolean loops don't work correctly
- Integrate String literals as String data type, allow variables and parameters.
- Build super-simple test infrastruture
- Cleanup existing tests
  ...
*)
