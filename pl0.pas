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
  (* Write(Source.Buffer[Source.Column]); *)
  Inc(Source.Column);
end;

(* -------------------------------------------------------------------------- *)
(* --- Symbol table --------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

type
  TSymbolClass = (scConst, scVar, scProc, scScope, scString);

  PSymbol = ^TSymbol;
  TSymbol = record
    Name: String;
    Kind: TSymbolClass;
    Level: Integer;
    Value: Integer;
    StrVal: String;
    Tag: String;
    Shared: Boolean;  (* IR4: local accessed from a nested procedure *)
    Next: PSymbol;
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
  Sym^.Next := SymbolTable;
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
    Sym := SymbolTable^.Next;
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
    Sym := Sym^.Next;
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
    Sym := Sym^.Next;
  end;

  LookupGlobal := nil;
end;

function CreateSymbol(Kind: TSymbolClass; Name: String): PSymbol;
var
  Sym: PSymbol;
begin
  if (Name <> '') and (LookupLocal(Name) <> nil) then
  begin
    Error('Duplicate symbol "' + Name + '".');
  end;

  New(Sym);
  Sym^.Kind := Kind;
  Sym^.Name := Name;
  Sym^.Level := Level;
  Sym^.Shared := False;
  Sym^.Tag := '';
  Sym^.Next := SymbolTable;
  SymbolTable := Sym;

  if Kind = scVar then
  begin
    Sym^.Value := Offset;
    Offset := Offset + 2;
  end;

  CreateSymbol := Sym;
end;

(* --------------------------------------------------------------------- *)
(* --- Scanner --------------------------------------------------------- *)
(* --------------------------------------------------------------------- *)

type
  TToken = (toNone,
            toIdent, toNumber, toString,
            toAdd, toSub, toMul, toDiv, toMod,
            toEq, toNeq, toLt, toLeq, toGt, toGeq,
            toLParen, toRParen,
            toSay, toAsk, toBecomes, toComma, toSemicolon, toPeriod,
            toOdd,
            toBegin, toEnd, toConst, toVar, toProcedure, toCall, 
            toIf, toThen, toElse, toWhile, toDo,
            toEof);

  TScanner = record
    Token:    TToken;
    StrValue: String;
    NumValue: Integer;
  end;

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

type
  TKeyword = record
    Token: TToken;
    Ident: String;
end;

const
  MaxKeywords = 12;

var
  NumKeywords: Integer;
  Keywords: array[0..MaxKeywords-1] of TKeyword;
    
procedure RegisterKeyword(Token: TToken; Ident: String);
begin
  if (NumKeywords = MaxKeywords) then Error('Too many keywords.');

  Keywords[NumKeywords].Token := Token;
  Keywords[NumKeywords].Ident := UpperStr(Ident);

  NumKeywords := NumKeywords+1;
end;

function LookupKeyword(Ident: String): TToken;
var
  I: Integer;
begin
  Ident := UpperStr(Ident);

  for I := 0 to MaxKeywords-1 do
    if Keywords[I].Ident = Ident then
    begin
      LookupKeyword := Keywords[I].Token;
      Exit;
    end;

  LookupKeyword := toIdent;
end;

procedure RegisterAllKeywords;
begin
  RegisterKeyword(toBegin, 'begin');
  RegisterKeyword(toEnd, 'end');
  RegisterKeyword(toConst, 'const');
  RegisterKeyword(toVar, 'var');
  RegisterKeyword(toProcedure, 'procedure');
  RegisterKeyword(toCall, 'call');
  RegisterKeyword(toIf, 'if');
  RegisterKeyword(toThen, 'then');
  RegisterKeyword(toElse, 'else');
  RegisterKeyword(toWhile, 'while');
  RegisterKeyword(toDo, 'do');
  RegisterKeyword(toOdd, 'odd');
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
      '!': begin
        Token := toSay;
        C := GetChar;
      end;
      '?': begin
        Token := toAsk;
        C := GetChar;
      end; 
      ':': begin
        C := GetChar;
        if C = '=' then
        begin
          Token := toBecomes;
          C := GetChar;
        end
        else Error('"=" expected.')
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
  if Scanner.Token <> Token then Error('Parser error');
end;

(* -------------------------------------------------------------------------- *)
(* --- Emitter -------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

type
  TBinaryType = (btCom, btDot);

var
  Binary: TBinaryType;
  Target: Text;
  NextLabel: Integer;

  (* IR4th backend: declarations (var, fwd, data) go to Decl, function
     bodies to Target. IR4th needs everything declared before use, so the
     two are concatenated at the end. *)
  Ir4: Boolean;
  Decl: Text;
  CurShared: Boolean;

procedure IrDecl(S: String);
begin
  WriteLn(Decl, S);
end;

procedure IrCode(S: String);
begin
  WriteLn(Target, '  ', S);
end;

function IrEscape(S: String): String;
var
  I: Integer;
  T: String;
begin
  T := '';
  for I := 1 to Length(S) do
    if (S[I] = '"') or (S[I] = '\') then T := T + '\' + S[I] else T := T + S[I];
  IrEscape := T;
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

procedure Emit(Tag, Instruction, Comment: String);
var
  P: Integer;
  S: String;
begin
  if (Tag = '') and (Instruction = '') and (Comment <> '') then
  begin
    if Ir4 then WriteLn(Target, '// ', Comment) else WriteLn(Target, '; ', Comment);
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

procedure EmitI(S: String);
begin
  Emit('', S, '');
end;

procedure EmitC(S: String);
begin
  if Ir4 then WriteLn(Target, '// ', S) else WriteLn(Target, '; ', S);
end;

procedure EmitInclude(S: String);
begin
  EmitI('include "' +  S + '"');
end;

procedure EmitCall(Sym: PSymbol);
begin
  if Ir4 then
    IrCode(Sym^.Tag)
  else
    EmitI('call ' + Sym^.Tag);
end;

procedure EmitHeader(Home: String; SrcFile: String);
begin
  if Ir4 then
  begin
    IrDecl('// program ' + SrcFile);
    IrDecl('');
    IrDecl('require ' + Home + '/pl0rt.ir4');
    IrDecl('');
    IrDecl('var display :bytes 32');
    IrDecl('');
    Exit;
  end;

  EmitC('');
  EmitC('program ' + SrcFile);
  EmitC('');
  if Binary = btCom then
  begin
    Emit('', 'DEFINE CPM', 'Target is CP/M .com file');
  end
  else if Binary = btDot then
  begin
    Emit('', 'DEFINE NXT', 'Target is Next .dot file');
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
  if Sym^.Kind <> scScope then CollectVars(Sym^.Next, S);

  if Sym^.Kind = scVar then
    if S <> '' then S := S + ', ' + Sym^.Name else S := Sym^.Name;
end;

procedure CollectString(Sym: PSymbol);
var
  S: String;
begin
  if Sym^.Kind <> scScope then CollectString(Sym^.Next);

  if Sym^.Kind = scString then
  begin
      S := Sym^.Name;
      EmitC('');
      Emit(Sym^.Tag, 'db ' + Int2Str(Length(S)) + ',"' + S + '"', '');
  end;
end;

(* Collects IR4th locals of the current scope (in declaration order). Locals
   accessed from nested procedures live in a frame instead (see below). *)
procedure IrLocals(Sym: PSymbol; var S: String; var AnyShared: Boolean);
begin
  if Sym^.Kind = scScope then Exit;
  IrLocals(Sym^.Next, S, AnyShared);
  if Sym^.Kind = scVar then
  begin
    if Sym^.Shared then
      AnyShared := True
    else
      S := S + ' ' + Sym^.Tag + ' :i';
  end;
end;

function IrOffset(Value: Integer): String;
begin
  if Value = 0 then IrOffset := '' else IrOffset := ' ' + Int2Str(Value) + ' +';
end;

function DisplaySlot(L: Integer): String;
begin
  DisplaySlot := 'display ' + Int2Str(L * 2) + ' +';
end;

procedure IrPrologue(Sym: PSymbol);
var
  S: String;
  I: Integer;
begin
  if Sym = nil then
  begin
    EmitC('main entry point');
    WriteLn(Target, 'fn main ( )');
    CurShared := False;
    Exit;
  end;

  EmitC('procedure ' + Sym^.Name);

  S := '';
  CurShared := False;
  IrLocals(SymbolTable, S, CurShared);

  (* Shared locals: a frame whose address goes into the display, so nested
     procedures find the current activation like in the Z80 backend. *)
  if CurShared then
    S := S + ' frame :bytes ' + Int2Str(Offset) + ' saved :a';

  if S = '' then
    WriteLn(Target, 'fn ', Sym^.Tag, ' ( )')
  else
    WriteLn(Target, 'fn ', Sym^.Tag, ' ( |', S, ' )');

  if CurShared then
  begin
    IrCode(DisplaySlot(Level) + ' @a -> saved');
    if Offset <= 8 then
    begin
      I := 0;
      while I < Offset do
      begin
        IrCode('0 frame' + IrOffset(I) + ' !i');
        I := I + 2;
      end;
    end
    else
      IrCode('frame ' + Int2Str(Offset) + ' 0 fillchar');
    IrCode('frame ' + DisplaySlot(Level) + ' !a');
  end;
end;

procedure IrEpilogue;
begin
  if CurShared then
    IrCode('saved ' + DisplaySlot(Level) + ' !a');
  WriteLn(Target, 'end');
  WriteLn(Target);
end;

procedure EmitPrologue(Sym: PSymbol);
var
  I: Integer;
  V: String;
begin
  if Ir4 then
  begin
    IrPrologue(Sym);
    Exit;
  end;

  if Sym = Nil then
    EmitC('main entry point')
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

    EmitI('ld de,0');
    for I := 0 to Offset div 2 - 1 do
      EmitI('push de');
  end;
end;

procedure EmitEpilogue(Sym: PSymbol);
begin
  if Ir4 then
  begin
    IrEpilogue;
    Exit;
  end;

  if Sym = nil then
    Emit('', 'call __done', '')
  else
  begin
    Emit('', 'ld sp,ix', 'Epilogue');

    EmitI('pop hl');
    EmitI('ld (display+' + Int2Str(Level * 2) + '),hl');

    EmitI('pop ix');
  end;

  EmitI('ret');

  CollectString(SymbolTable);
end;

(* IR4th: pushes the address of a frame-resident (shared) local. *)
procedure IrSharedAddr(Sym: PSymbol);
begin
  if Level = Sym^.Level then
    IrCode('frame' + IrOffset(Sym^.Value))
  else
    IrCode(DisplaySlot(Sym^.Level) + ' @a' + IrOffset(Sym^.Value));
end;

procedure IrGetVar(Sym: PSymbol);
begin
  if (Sym^.Level > 1) and (Level > Sym^.Level) then Sym^.Shared := True;

  if Sym^.Shared then
  begin
    IrSharedAddr(Sym);
    IrCode('@i');
  end
  else
    IrCode(Sym^.Tag);
end;

procedure IrSetVar(Sym: PSymbol);
begin
  if (Sym^.Level > 1) and (Level > Sym^.Level) then Sym^.Shared := True;

  if Sym^.Shared then
  begin
    IrSharedAddr(Sym);
    IrCode('!i');
  end
  else
    IrCode('-> ' + Sym^.Tag);
end;

procedure EmitGetVar(Sym: PSymbol);
var
  L: Integer;
begin
  if Ir4 then
  begin
    IrGetVar(Sym);
    Exit;
  end;

  L := Level - Sym^.Level;

  if Sym^.Level = 1 then
  begin
    Emit('', 'ld de,(globals+' + Int2Str(Sym^.Value) + ')', 'Get global ' + Sym^.Name);
    EmitI('push de');
  end
  else if L = 0 then
  begin
    Emit('', 'ld d,(ix-' + Int2Str(Sym^.Value+1) + ')', 'Get local ' + Sym^.Name);
    EmitI('ld e,(ix-' + Int2Str(Sym^.Value+2) + ')');
    EmitI('push de');
  end
  else
  begin
    Emit('', 'ld iy,(display+' + Int2Str(Sym^.Level * 2) + ')', 'Get outer ' + Sym^.Name);
    EmitI('ld d,(iy-' + Int2Str(Sym^.Value+1) + ')');
    EmitI('ld e,(iy-' + Int2Str(Sym^.Value+2) + ')');
    EmitI('push de');
  end
end;

procedure EmitSetVar(Sym: PSymbol);
var
  L: Integer;
begin
  if Ir4 then
  begin
    IrSetVar(Sym);
    Exit;
  end;

  L := Level - Sym^.Level;

  if Sym^.Level = 1 then
  begin
    EmitI('pop de');
    Emit('', 'ld (globals+' + Int2Str(Sym^.Value) + '),de', 'Set global ' + Sym^.Name);
  end
  else if L = 0 then
  begin
    Emit('', 'pop de', 'Set local ' + Sym^.Name);
    EmitI('ld (ix-' + Int2Str(Sym^.Value+1) + '),d');
    EmitI('ld (ix-' + Int2Str(Sym^.Value+2) + '),e');
  end
  else
  begin
    EmitI('pop de');
    Emit('', 'ld iy,(display+' + Int2Str(Sym^.Level * 2) + ')', 'Set outer ' + Sym^.Name);
    EmitI('ld (iy-' + Int2Str(Sym^.Value+1) + '),d');
    EmitI('ld (iy-' + Int2Str(Sym^.Value+2) + '),e');
  end
end;

procedure EmitLiteral(Value: Integer);
begin
  if Ir4 then begin IrCode(Int2Str(Value)); Exit; end;
  Emit('', 'ld de,' + Int2Str(Value), 'Literal ' + Int2Str(Value));
  EmitI('push de');
end;

procedure EmitOdd();
var
  S: String;
begin
  if Ir4 then begin IrCode('1 and 0 !='); Exit; end;
  Emit('', 'pop hl', 'Odd');
  EmitI('bit 0,l');
  EmitI('ld de,0');

  S := GetLabel('false');

  EmitI('jr z,' + S);
  EmitI('ld de,255');
  Emit(S, 'push de', '');
end;

procedure EmitJumpIf(Op: TToken; Target: String);
var
  S: String;
begin
  Emit('','pop de', 'RelOp ' + Int2Str(Ord(Op)));
  EmitI('pop hl');

  if (Op = toGt) or (Op = toLeq) then EmitI('ex hl,de');

  EmitI('call __comp16');

  S := GetLabel('false');

  case Op of
    toEq:   EmitI('jr z,' + Target);
    toNeq:  EmitI('jr nz,' + Target);
    toLt:   EmitI('jr c,' + Target);
    toGt:   EmitI('jr c,' + Target);
    toLeq:  EmitI('jr nc,' + S);
    toGeq:  EmitI('jr nc,' + S);
  end;
end;

procedure EmitComp(Op: TToken);
var
  S, T: String;
begin
  if Ir4 then
  begin
    case Op of
      toEq:  IrCode('==');
      toNeq: IrCode('!=');
      toLt:  IrCode('<');
      toGt:  IrCode('>');
      toLeq: IrCode('<=');
      toGeq: IrCode('>=');
    end;
    Exit;
  end;

  Emit('','pop de', 'RelOp ' + Int2Str(Ord(Op)));
  EmitI('pop hl');

  if (Op = toGt) or (Op = toLeq) then EmitI('ex hl,de');

  EmitI('call __comp16');

  S := GetLabel('true');
  T := GetLabel('exit');

  case Op of
    toEq:   EmitI('jr z,' + S);
    toNeq:  EmitI('jr nz,' + S);
    toLt:   EmitI('jr c,' + S);
    toGt:   EmitI('jr c,' + S);
    toLeq:  EmitI('jr nc,' + S);
    toGeq:  EmitI('jr nc,' + S);
  end;

  EmitI('xor a');
  EmitI('jr ' + T);

  Emit(S, 'ld a,255', '');
  Emit(T, 'push af', '');
end;

procedure EmitMul();
begin
  if Ir4 then begin IrCode('*'); Exit; end;
  Emit('', 'pop de', '');
  Emit('', 'pop hl', '');
  Emit('', 'call __mul16', 'Mul');
  Emit('', 'push hl', '');
end;

procedure EmitDiv();
begin
  if Ir4 then begin IrCode('/'); Exit; end;
  Emit('', 'pop de', '');
  Emit('', 'pop hl', '');
  Emit('', 'call __sdiv16', 'Div');
  Emit('', 'push hl', '');
end;

procedure EmitAdd();
begin
  if Ir4 then begin IrCode('+'); Exit; end;
  Emit('', 'pop de', 'Add');
  EmitI('pop hl');
  EmitI('add hl,de');
  EmitI('push hl');
end;

procedure EmitSub();
begin
  if Ir4 then begin IrCode('-'); Exit; end;
  Emit('', 'pop de', 'Sub');
  EmitI('pop hl');
  EmitI('xor a');
  EmitI('sbc hl,de');
  EmitI('push hl');
end;

procedure EmitInputNum(S: String);
begin
  if Ir4 then begin IrCode('pl0_getn'); Exit; end;
  Emit('', 'call __getn', 'Get ' + S);
  EmitI('push de');
end;

procedure EmitPrintNum(S: String);
begin
  if Ir4 then begin IrCode('pl0_putn'); Exit; end;
  Emit('', 'pop hl', '');
  Emit('', 'call __putn', '');
end;

procedure EmitPrintStr(S: String);
var
  Sym: PSymbol;
begin
  Sym := CreateSymbol(scString, S);
  Sym^.Tag := GetLabel('string');

  if Ir4 then
  begin
    IrDecl('data ' + Sym^.Tag);
    IrDecl('  ' + Int2Str(Length(S)) + ' :i8 "' + IrEscape(S) + '"');
    IrDecl('end');
    IrDecl('');
    IrCode(Sym^.Tag + ' pl0_puts');
    Exit;
  end;

  Emit('', 'ld hl,' + Sym^.Tag, '');
  Emit('', 'call __puts', '');
end;

procedure EmitIfThen(Tag: String);
begin
  if Ir4 then begin IrCode('if'); Exit; end;
  EmitI('pop af');
  EmitI('and a');
  EmitI('jp z,' + Tag);
end;

procedure EmitIfEnd(Tag: String);
begin
  if Ir4 then begin IrCode('then'); Exit; end;
  Emit(Tag, '', '');
end;

procedure EmitWhileHead(Tag: String);
begin
  if Ir4 then begin IrCode('begin'); Exit; end;
  Emit(Tag, '', '');
end;

procedure EmitWhileDo(Tag2: String);
begin
  if Ir4 then begin IrCode('while'); Exit; end;
  EmitI('pop af');
  EmitI('and a');
  EmitI('jp z,' + Tag2);
end;

procedure EmitWhileEnd(Tag, Tag2: String);
begin
  if Ir4 then begin IrCode('repeat'); Exit; end;
  EmitI('jp ' + Tag);
  Emit(Tag2, '', '');
end;

procedure CloseTarget();
begin
  Close(Target);
end;

(* -------------------------------------------------------------------------- *)
(* --- Parser --------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

procedure ParseExpression; forward;

procedure ParseFactor;
var
  Sym: PSymbol;
begin
  if Scanner.Token = toIdent then
  begin
    Sym := LookupGlobal(Scanner.StrValue);
    if Sym = nil then Error('Identifier "' + Scanner.StrValue + '" not found.');
    if (Sym^.Kind <> scVar) and (Sym^.Kind <> scConst) then Error('"' + Scanner.StrValue + '" neither var nor const.');

    if Sym^.Kind = scVar then
      EmitGetVar(Sym)
    else
      EmitLiteral(Sym^.Value);

    NextToken;
  end
  else if Scanner.Token = toNumber then
  begin
    EmitLiteral(Scanner.NumValue);

    NextToken;
  end
  else if Scanner.Token = toLParen then
  begin
    NextToken; ParseExpression; Expect(toRParen); NextToken;
  end
  else Error('Factor expected');
end;

procedure ParseTerm;
var
  Op: TToken;
begin
  ParseFactor;
  while (Scanner.Token = toMul) or (Scanner.Token = toDiv) do
  begin
    Op := Scanner.Token;
    NextToken;
    ParseFactor;

    if Op = toMul then EmitMul() else EmitDiv();
  end;
end;

procedure ParseExpression;
var
  Op: TToken;
begin
  Op := toNone;

  if (Scanner.Token = toAdd) or (Scanner.Token = toSub) then
  begin
    Op := Scanner.Token;
    NextToken;
  end;

  if Op = toSub then EmitLiteral(0);

  ParseTerm;

  if Op = toSub then EmitSub();

  while (Scanner.Token = toAdd) or (Scanner.Token = toSub) do
  begin
    Op := Scanner.Token;
    NextToken;
    ParseTerm;

    if Op = toAdd then EmitAdd() else EmitSub();
  end;
end;

procedure ParseCondition;
var
  Op: TToken; 
begin
  if Scanner.Token = toOdd then
  begin
    NextToken; ParseExpression;

    EmitOdd();
  end 
  else
  begin
    ParseExpression;
    if (Scanner.Token >= toEq) and (Scanner.Token <= toGeq) then
    begin
      Op := Scanner.Token;
      NextToken;
    end
    else Error('RelOp expected');
    ParseExpression;

    EmitComp(Op);
  end;
end;

procedure ParseStatement;
var
  Sym: PSymbol;
  Tag, Tag2: String;
begin
  if Scanner.Token = toIdent then
  begin
    Sym := LookupGlobal(Scanner.StrValue);
    if Sym = nil then Error('Identifier "' + Scanner.StrValue + '" not found.');
    if Sym^.Kind <> scVar then Error('"' + Scanner.StrValue + '" not a var.');
    NextToken; Expect(toBecomes); NextToken; ParseExpression;

    EmitSetVar(Sym);
  end
  else if Scanner.Token = toCall then
  begin
    NextToken; Expect(toIdent);
    Sym := LookupGlobal(Scanner.StrValue);
    if Sym = nil then Error('Identifier "' + Scanner.StrValue + '" not found.');
    if Sym^.Kind <> scProc then Error('"' + Scanner.StrValue + '" not a proc.');
    NextToken;

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
    EmitSetVar(Sym);
  end
  else if Scanner.Token = toSay then
  begin
    NextToken;

    if Scanner.Token = toString then
    begin
      EmitPrintStr(Scanner.StrValue);
      NextToken;
    end
    else
    begin
      ParseExpression;
      EmitPrintNum(Scanner.StrValue);
    end;
  end
  else if Scanner.Token = toBegin then
  begin
    NextToken; ParseStatement;
    while Scanner.Token = toSemicolon do
    begin
      NextToken;
      ParseStatement;
    end;
    Expect(toEnd); NextToken;
  end
  else if Scanner.Token = toIf then
  begin
    NextToken; ParseCondition; Expect(toThen); NextToken;
    
    Tag := GetLabel('false');

    EmitIfThen(Tag);

    ParseStatement;

    EmitIfEnd(Tag);
  end
  else if Scanner.Token = toWhile then
  begin
    Tag := GetLabel('while');
    Tag2 := GetLabel('false');

    EmitWhileHead(Tag);

    NextToken; ParseCondition; Expect(toDo); NextToken;

    EmitWhileDo(Tag2);

    ParseStatement;

    EmitWhileEnd(Tag, Tag2);
  end
end;

procedure ParseConst;
var
  Sym: PSymbol;
begin
  Expect(toIdent);
  Sym := CreateSymbol(scConst, Scanner.StrValue);
  NextToken;
  Expect(toEq);
  NextToken;
  Expect(toNumber);

  Sym^.Value := Scanner.NumValue;

  NextToken;
end;

procedure ParseVar;
var
  Sym: PSymbol;
begin
  Expect(toIdent);
  Sym := CreateSymbol(scVar, Scanner.StrValue);
  if Level = 1 then
  begin
    Sym^.Tag := 'g_' + UpperStr(Sym^.Name);
    if Ir4 then IrDecl('var ' + Sym^.Tag + ' :i');
  end
  else
    Sym^.Tag := 'l_' + UpperStr(Sym^.Name);
  NextToken;
end;

procedure ParseBlock(Sym: PSymbol);
var
  NewSym: PSymbol;
begin
  if Scanner.Token = toConst then
  begin
    NextToken; ParseConst;
    while Scanner.Token = toComma do
    begin
      NextToken; ParseConst;
    end;
    Expect(toSemicolon);
    NextToken;
  end;

  if Scanner.Token = toVar then
  begin
    NextToken; ParseVar;
    while Scanner.Token = toComma do
    begin
      NextToken; ParseVar;
    end;
    Expect(toSemicolon);
    NextToken;
  end;

  while Scanner.Token = toProcedure do
  begin
    NextToken; Expect(toIdent);
    NewSym := CreateSymbol(scProc, Scanner.StrValue);
    NewSym^.Tag := GetLabel('proc');
    if Ir4 then IrDecl('fwd ' + NewSym^.Tag + ' ( )');

    OpenScope;
    NextToken; Expect(toSemicolon);
    NextToken; ParseBlock(NewSym);
    CloseScope;
    Expect(toSemicolon);
    NextToken;

    EmitC('');
  end;

  EmitPrologue(Sym);
  parseStatement();
  EmitEpilogue(Sym);
end;

procedure ParseProgram;
begin
  OpenScope;
  parseBlock(Nil);
  Expect(toPeriod);
  if not Ir4 then
  begin
    EmitC('');
    Emit('globals', 'ds ' + Int2Str(Offset), 'Globals');
    EmitC('');
    Emit('display', 'ds 32', 'Display');
  end;
  CloseScope;
end;

(* -------------------------------------------------------------------------- *)
(* --- Main program --------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

var
  SrcFile, AsmFile, BinFile, HomeDir, AsmTool, IrTool, BodyFile, Line: String;
  I: Integer;
  Body: Text;

begin
  WriteLn('PL/0 Compiler for Z80 Version 1.0');
  WriteLn('Copyright (c) 2020 by Joerg Pleumann');
  WriteLn;

  HomeDir := GetEnv('PL0_HOME');
  if HomeDir = '' then
    HomeDir := ParentDir(FExpand(ParamStr(0)));

  AsmTool := GetEnv('PL0_ASM');
  IrTool := GetEnv('PL0_IR4C');

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
    else if SrcFile = '--ir4' then
      Ir4 := True
    else if SrcFile = '--ir4c' then
    begin
      IrTool := ParamStr(I + 1);
      I := I + 1;
    end
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
    WriteLn('  --asm <path>   calls external assembler');
    WriteLn('  --com          target is CP/M .com file');
    WriteLn('  --dot          target is Next .dot file');
    WriteLn('  --ir4          generate IR4th instead of Z80 assembly');
    WriteLn('  --ir4c <path>  calls IR4th compiler (CP/M only)');
    WriteLn;
    Halt(1);
  end;

  if Pos('.', SrcFile)=0 then SrcFile := SrcFile + '.pl0';

  if Ir4 then
  begin
    if Binary <> btCom then Error('IR4th backend supports CP/M only');
    AsmFile := ChangeExt(SrcFile, '.ir4');
    BodyFile := ChangeExt(SrcFile, '.ir4b');
  end
  else
    AsmFile := ChangeExt(SrcFile, '.z80');

  if Binary = btCom then
    BinFile := ChangeExt(SrcFile, '.com')
  else if Binary = btDot then
    BinFile := ChangeExt(SrcFile, '.dot');

  WriteLn('Compiling...');

  RegisterAllKeywords();

  OpenInput(SrcFile);
  if Ir4 then
  begin
    Assign(Decl, AsmFile);
    Rewrite(Decl);
    OpenTarget(BodyFile);
  end
  else
    OpenTarget(AsmFile);
  EmitHeader(HomeDir, SrcFile);  (* TODO Move this elsewhere. *)
  NextToken;
  ParseProgram;
  if not Ir4 then
    EmitFooter();                (* TODO Move this elsewhere. *)
  CloseTarget();
  CloseInput();

  if Ir4 then
  begin
    (* Declarations first, then bodies: IR4th needs declare-before-use. *)
    Assign(Body, BodyFile);
    Reset(Body);
    while not Eof(Body) do
    begin
      ReadLn(Body, Line);
      WriteLn(Decl, Line);
    end;
    Close(Body);
    Erase(Body);
    Close(Decl);

    if IrTool <> '' then
    begin
      WriteLn('Compiling IR4th...');

      (* ir4c resolves lib/ relative to the current directory *)
      GetDir(0, Line);
      BinFile := FExpand(BinFile);
      AsmFile := FExpand(AsmFile);
      IrTool := FExpand(IrTool);
      ChDir(ParentDir(ParentDir(IrTool)));
      Exec(IrTool, '-t z80 --cpm -o ' + BinFile + ' ' + AsmFile);
      ChDir(Line);
      if DosError <> 0 then
        Error('Error ' + Int2Str(DosError) + ' starting ' + IrTool);
      if DosExitCode <> 0 then
        Error('Failure! :(')
    end;
  end
  else if AsmTool <> '' then
  begin
    WriteLn('Assembling...');

    Exec(AsmTool, '--raw=' + BinFile + ' ' + AsmFile);
    if DosError <> 0 then
      Error('Error ' + Int2Str(DosError) + ' starting ' + AsmTool);
    if DosExitCode <> 0 then
      Error('Failure! :(')
  end;

  WriteLn('Success! :)');
end.
