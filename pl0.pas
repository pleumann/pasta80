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
  J := Length(S);
  while ((J > I) and (S[I] <= ' ')) do
    J := J - 1;
  TrimStr := Copy(S, I, J - I);
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
    Count:  Integer;
    Line:   Integer;
    Column: Integer;
  end;

var
  Source: TSource;

procedure Error(Message: String);
var  I: Integer;
begin
  WriteLn;
  WriteLn(Source.Buffer);
  for I := 1 to Source.Column - 1 do Write(' ');
  WriteLn('^');
  WriteLn('*** Error: ', Message, ' in line ', Source.Line, ', column ', Source.Column);
  WriteLn();
  Halt(1);
end;

procedure OpenInput(FileName: String);
begin
  Assign(Source.Input, FileName);
  Reset(Source.Input);
  Source.Column := 1;
end;

procedure CloseInput();
begin
  Close(Source.Input);
end;

function GetChar(): Char;
begin
  if Source.Column > Length(Source.Buffer) then
  begin
    if Eof(Source.Input) then
    begin
      Error('Unexpected end of source');
      (* GetChar := #26;
      Exit; *)
    end;

    ReadLn(Source.Input, Source.Buffer);
    Source.Buffer := Source.Buffer + #13;
    Source.Line := Source.Line + 1 ;
    Source.Column := 1;

    (* WriteLn('[', Source.Line, '] ', Source.Buffer); *)
  end;

  GetChar := Source.Buffer[Source.Column];
  Write(Source.Buffer[Source.Column]);
  Inc(Source.Column);
end;

(* -------------------------------------------------------------------------- *)
(* --- Symbol table --------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

type
  TDataType = (dtInteger, dtBoolean);

const
  TypeName: array [TDataType] of String = ('Integer', 'Boolean');

type
  TSymbolClass = (scConst, scVar, scProc, scFunc, scScope, scString);

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
    if (Sym^.Kind <> scString) and (UpperStr(Sym^.Name) = Name) then
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
    if (Sym^.Kind <> scString) and (UpperStr(Sym^.Name) = Name) then
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
  if (Kind <> scString) and (Name <> '') and (LookupLocal(Name) <> nil) then
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
  RegisterBuiltIn(scFunc, 'Random', 0, '__rand16');
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
            toAnd, toOr, toXor, toNot,
            toOdd,
            toBoolean, toInteger, toTrue, toFalse, toArray, toOf,
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
            'and', 'or', 'xor', 'not',
            'odd',
            'boolean', 'integer', 'true', 'false', 'array', 'of',
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
        if C = '=' then
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
          repeat
            while C <> '*' do C := GetChar;
            C := GetChar;
          until C = ')';
          C := GetChar;
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

var
  Binary: TBinaryType;
  Graphics: TGraphicsMode;
  Target: Text;
  NextLabel: Integer;
  LastTag, LastInstruction, LastComment: String;
  Optimize: Boolean;
  ExitTarget: String;

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
begin
  if not Optimize then Exit;

  if (LastTag <> '') or (LastInstruction <> '') or (LastComment <> '') then
  begin
    Emit0(LastTag, LastInstruction, LastComment);
    LastTag := '';
    LastInstruction := '';
    LastComment := '';
  end;
end;

procedure Emit(Tag, Instruction, Comment: String);
var
  TwoOp: String;
begin
  if not Optimize then
  begin
    Emit0(Tag, Instruction, Comment);
    Exit;
  end;

  if Tag <> '' then
  begin
    Flush;
    LastTag := Tag;
    LastInstruction := Instruction;
    LastComment := Comment;
    Exit;
  end;

  if (LastInstruction = '') then
  begin
    LastInstruction := Instruction;
    LastComment := Comment;
    Exit;
  end;

  (* Try to eliminate the most embarrassing generated instruction pairs. *)
  TwoOp := LastInstruction + '/' + Instruction;

  if (TwoOp = 'push hl/pop hl') or (TwoOp = 'push de/pop de') or (TwoOp = 'push af/pop af') then
  begin
    LastInstruction := '';
    LastComment := '';
    Exit;
  end
  else if TwoOp = 'push de/pop hl' then
  begin
    LastInstruction := 'ld hl,de';
    Exit;
  end
  else if TwoOp = 'push de/pop bc' then
  begin
    LastInstruction := 'ld bc,de';
    Exit;
  end
  else if TwoOp = 'push hl/pop de' then
  begin
    LastInstruction := 'ld de,hl';
    Exit;
  end;

  Flush;
  LastTag := Tag;
  LastInstruction := Instruction;
  LastComment := Comment;
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

  CollectString(SymbolTable);
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
      Emit('', 'ld hl,' + RelativeAddr('globals', Sym^.Value), 'Get global ' + Sym^.Name);
      EmitI('add hl,de');
      EmitI('add hl,de');
      EmitI('ld de,(hl)');
      EmitI('push de');
    end
    else
    begin
      Emit('', 'ld hl,(' + RelativeAddr('globals', Sym^.Value) + ')', 'Get global ' + Sym^.Name);
      EmitI('push hl');
    end;
  end
  else if L = 0 then
  begin
    Emit('', 'ld de,(' + RelativeAddr('ix', Sym^.Value) + ')', 'Get local ' + Sym^.Name);
    EmitI('push de');
  end
  else
  begin
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
      EmitI('pop bc');
      EmitI('pop de');
      Emit('', 'ld hl,' + RelativeAddr('globals', Sym^.Value), 'Set global ' + Sym^.Name);
      EmitI('add hl,de');
      EmitI('add hl,de');
      EmitI('ld (hl),bc');
      if Again then EmitI('push bc');
    end
    else
    begin
      EmitI('pop hl');
      Emit('', 'ld (' + RelativeAddr('globals', Sym^.Value) + '),hl', 'Set global ' + Sym^.Name);
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
  Emit('','pop de', 'RelOp ' + Int2Str(Ord(Op)));
  EmitI('pop hl');

  if (Op = toGt) or (Op = toLeq) then EmitI('ex hl,de');

  case Op of
    toEq:         EmitI('call __int16_eq');
    toNeq:        EmitI('call __int16_neq');
    toLt, toGt:   EmitI('call __int16_lt');
    toLeq, toGeq: EmitI('call __int16_geq');
  end;

  Emit('', 'push af', '');
end;

procedure EmitJumpIf(When: Boolean; Target: String);
begin
  EmitI('pop af');
  EmitI('and a');
  if When then
    EmitI('jp nz,' + Target)
  else
    EmitI('jp z,' + Target);
end;

procedure EmitBinOp(Op: TToken);
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
             EmitI('ld a,h'); EmitI('and d'); EmitI('ld h,a');
             EmitI('ld a,l'); EmitI('and e'); EmitI('ld l,a');
           end;
    toOr: begin
             EmitI('ld a,h'); EmitI('or d'); EmitI('ld h,a');
             EmitI('ld a,l'); EmitI('or e'); EmitI('ld l,a');
           end;
    toXor: begin
             EmitI('ld a,h'); EmitI('xor d'); EmitI('ld h,a');
             EmitI('ld a,l'); EmitI('xor e'); EmitI('ld l,a');
           end;
    end;
  Emit('', 'push hl', '');
end;

procedure EmitUnOp(Op: TToken);
begin

  case Op of
    toOdd: begin
             Emit('', 'pop hl', 'Odd');
             EmitI('ld a,l');
             EmitI('and 1');
             EmitI('push af');
           end;
    toNot: begin
             Emit('', 'pop hl', 'Not');
             EmitI('ld a,255');
             EmitI('xor h');
             EmitI('ld h,a');
             EmitI('ld a,255');
             EmitI('xor l');
             EmitI('ld l,a');
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

procedure EmitPrintBoolean();
begin
  Emit('', 'pop hl', '');
  Emit('', 'call __putb', '');
end;

procedure EmitPrintNum(S: String);
begin
  Emit('', 'pop hl', '');
  Emit('', 'call __putn', '');
end;

procedure EmitPrintStr(S: String);
var
  Sym: PSymbol;
begin
  Sym := CreateSymbol(scString, S, 0);
  Sym^.Tag := GetLabel('string');
  Emit('', 'ld hl,' + Sym^.Tag, '');
  Emit('', 'call __puts', '');
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

procedure TypeCheck(Expected, Found: TDataType);
begin
  if Expected <> Found then Error('Type error, expected ' + TypeName[Expected] + ', got ' + TypeName[Found]);
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

    TypeCheck(Sym^.ArgTypes[I], ParseExpression);
    I := I + 1;

    while Scanner.Token = toComma do
    begin
      NextToken;
      TypeCheck(Sym^.ArgTypes[I], ParseExpression);
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
    begin
      T := ParseExpression;
      if T = dtInteger then
        EmitPrintNum(Scanner.StrValue)
      else if T = dtBoolean then
        EmitPrintBoolean
      else
        Error('Type expected');
    end;
end;

function ParseFactor: TDataType;
var
  Sym: PSymbol;T: TDataType;
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
        TypeCheck(dtInteger, ParseExpression); (* Index now on stack *)
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
    if Scanner.Token = toTrue then EmitLiteral(65535) else EmitLiteral(0);
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
    T := ParseFactor();
    EmitUnOp(toNot);
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
  while Scanner.Token in [toMul, toDiv, toMod, toAnd] do
  begin
    Op := Scanner.Token;
    NextToken;
    TypeCheck(T, ParseFactor);
    EmitBinOp(Op);
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

  if Op <> toNone then TypeCheck(T, dtInteger);

  if Op = toSub then EmitBinOp(toSub);

  while Scanner.Token in [toAdd, toSub, toOr, toXor] do
  begin
    Op := Scanner.Token;
    NextToken;
    TypeCheck(T, ParseTerm);
    EmitBinOp(Op);
  end;

  WriteLn('Type of SimpleExpression is ', T);

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
    TypeCheck(dtInteger, ParseSimpleExpression);
    T := dtBoolean;
    EmitUnOp(toOdd);
  end 
  else
  begin
    T := ParseSimpleExpression;
    if (Scanner.Token >= toEq) and (Scanner.Token <= toGeq) then
    begin
      Op := Scanner.Token;
      NextToken;
      TypeCheck(T, ParseSimpleExpression);
      T := dtBoolean;
      EmitRelOp(Op);
    end;
  end;

  WriteLn('Type of Expression is ', T);

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
    TypeCheck(dtInteger, ParseExpression); (* Index now on stack *)
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
    TypeCheck(Sym^.DataType, Sym2^.DataType);
    ParseAssignment(Sym2, true);
  end
  else
  begin
    Expect(toBecomes); NextToken; TypeCheck(Sym^.DataType, ParseExpression);
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

    if Scanner.Token = toString then
    begin
      EmitPrintStr(Scanner.StrValue);
      EmitPrintNewLine;
      NextToken;
    end
    else
    begin
      T := ParseExpression;
      case T of
        dtBoolean: EmitPrintBoolean();
        dtInteger: EmitPrintNum(Scanner.StrValue);
      end;
      EmitPrintNewLine;
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
    NextToken; TypeCheck(dtBoolean, ParseExpression); Expect(toThen); NextToken;
    
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

    NextToken; TypeCheck(dtBoolean, ParseExpression); Expect(toDo); NextToken;

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
    
    TypeCheck(dtBoolean, ParseExpression);

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

    NextToken; Expect(toBecomes); NextToken; ParseExpression;
    EmitSetVar(Sym, false);

    if Scanner.Token = toTo then
      Delta := 1
    else if Scanner.Token = toDownTo then
      Delta := -1
    else
      Error('"to" or "downto" expected.');

    NextToken; ParseExpression; Expect(toDo); NextToken; (* final value on stack *)

    Tag := GetLabel('forloop');
    Tag2 := GetLabel('forcheck');
    Tag3 := GetLabel('forbreak');
    Tag4 := GetLabel('fornext');

    Emit('', 'jp ' + Tag2, 'Limit on stack, start loop');

    Emit(Tag, '', '');

    ParseStatement(Tag4, Tag3);

    Emit(Tag4, '', '');
    
    EmitGetVar(Sym);
    Emit('', 'ld de,' + Int2Str(Delta), 'Inc/dec counter');
    Emit('', 'push de', '');
    EmitBinOp(toAdd);
    EmitSetVar(Sym, false);
  
    Emit(Tag2, '', '');

    Emit('', 'pop de','Dup and check limit');
    Emit('', 'push de','');
    Emit('', 'push de', '');

    EmitGetVar(Sym);

    if Delta = 1 then EmitRelOp(toGeq) else EmitRelOp(toLeq); (* Operands swapped! *)

    EmitJumpIf(True, Tag);

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
      ParseWriteArgument;

      while Scanner.Token = toComma do
      begin
        NextToken;
        ParseWriteArgument;
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
begin
  Expect(toIdent);
  Sym := CreateSymbol(scConst, Scanner.StrValue, 0);
  NextToken;
  Expect(toEq);
  NextToken;
  Expect(toNumber);

  Sym^.Value := Scanner.NumValue;
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
      else Error('Type expected');
    end;
    NextToken;
  end;

  while Old<>SymbolTable do
  begin
    Old := Old^.Next;
    SetDataType(Old, DataType, High - Low + 1);
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
  EmitC('');
  Emit('globals', 'ds ' + Int2Str(Offset), 'Globals');
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
