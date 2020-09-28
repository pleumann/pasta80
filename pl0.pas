program PL0;

uses
  Dos;

(* Utility *)

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
  if N < 0 then
    S := Replace(S, ' ', '0');
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

(* --- Input ----------------------------------------------------------- *)

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
      GetChar := #26;
      Exit;
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

(* Table *)

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
  S: String;
begin
  repeat
    Kind := SymbolTable^.Kind;
    Sym := SymbolTable^.Next;
    Offset := SymbolTable^.Value;

    if Kind = scString then
    begin
      S := SymbolTable^.Name;
      Emit(SymbolTable^.Tag, 'db ' + Int2Str(Length(S)) + ',"' + SymbolTable^.Name + '"', '');
    end;

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

procedure require(Token: TToken);
begin
  (* Write('<', Token, '/', Scanner.Token, '>'); *)
  if Scanner.Token <> Token then Error('Parser error');
end;

(* Emitter *)

type
  TBinaryType = (btCpm, btDot);

var
  Binary: TBinaryType;
  Target: Text;
  NextLabel: Integer;

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
begin
  if Tag <> '' then WriteLn(Target, Tag, ':');

  if Instruction <> '' then
  begin
    P := Pos(' ', Instruction);
    if P<>0 then
      Instruction := AlignStr(Copy(Instruction, 1, P-1), 8) + Copy(Instruction, P+1, 255);

    Write(Target, '        ');
    if Comment <> '' then 
      Write(Target, AlignStr(Instruction, 32))
    else
      WriteLn(Target, Instruction);
  end;

  if (Comment <> '') or (Tag = '') and (Instruction = '') then
    WriteLn(Target, '; ' + Comment);
end;

procedure EmitS(S: String);
begin
  WriteLn(Target, S);
end;

procedure EmitL(S: String);
begin
  Emit(S, '', '');
end;

procedure EmitI(S: String);
begin
  Emit('', S, '');
end;

procedure EmitC(S: String);
begin
  Emit('', '', S);
end;

procedure EmitInclude(S: String);
begin
  WriteLn(Target, ' include "', S, '"');
end;

procedure EmitCall(Sym: PSymbol);
begin
  (*
  if Sym.Level > Level then
  begin
    Emit('', 'push iy', 'Call nested');
    Emit('', 'call ' + Sym.Tag, '');
    Emit('', 'pop ix', '');
  end
  else if Sym.Level = Level then
  begin
    Emit('', 'call ' + Sym.Tag, 'Call same');
  end
  else
  begin
    Emit('', 'push iy', '');
    Emit('', 'ld h,(iy-1)', 'Call nested');
    Emit('', 'ld l,(iy-2)');
    Emit('', 'pushl hl', '');

    Emit('', 'push ix', 'Call nested');
    Emit('', 'push ix', 'Call nested');
    Emit('', 'call ' + Sym.Tag, '');
    Emit('', 'pop ix', '');
  end

  EmitL(Sym^.Tag);    EmitI('push ix');    EmitC('Prologue');

  Emit(Sym^.Tag, 'push ix', 'Prologue');
  EmitI('push iy');
  *)
end;

procedure EmitPrologue(Sym: PSymbol);
var
  I: Integer;
begin
  // EmitL(Sym^.Tag);
  
  Emit(Sym^.Tag, 'push ix', 'Prologue');

  EmitI('ld hl,(__display+' + Int2Str(Level * 2) + ')');
  EmitI('push hl');

  EmitI('ld ix,0');
  EmitI('add ix,sp');

  EmitI('ld (__display+' + Int2Str(Level * 2) + '),ix');

  EmitI('ld de,0');
  for I := 0 to Offset div 2 - 1 do
    EmitI('push de');
end;

procedure EmitEpilogue(Sym: PSymbol);
begin
  Emit('', 'ld sp,ix', 'Epilogue');

  EmitI('pop hl');
  EmitI('ld (__display+' + Int2Str(Level * 2) + '),hl');

  EmitI('pop ix');
  EmitI('ret');
end;

procedure EmitGetVar(Sym: PSymbol);
var
  L: Integer;
  S: String;
begin
  L := Level - Sym^.Level;

  if Sym^.Level = 1 then
  begin
    Emit('', 'ld de,(data+' + Int2Str(Sym^.Value) + ')', 'Get global ' + Sym^.Name);
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
    Emit('', 'ld iy,(__display+' + Int2Str(Sym^.Level * 2) + ')', 'Get outer ' + Sym^.Name);
    EmitI('ld d,(iy-' + Int2Str(Sym^.Value+1) + ')');
    EmitI('ld e,(iy-' + Int2Str(Sym^.Value+2) + ')');
    EmitI('push de');
  end
end;

procedure EmitSetVar(Sym: PSymbol);
var
  L: Integer;
  S: String;
begin
  L := Level - Sym^.Level;

  if Sym^.Level = 1 then
  begin
    EmitI('pop de');
    Emit('', 'ld (data+' + Int2Str(Sym^.Value) + '),de', 'Set global ' + Sym^.Name);
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
    Emit('', 'ld iy,(__display+' + Int2Str(Sym^.Level * 2) + ')', 'Set outer ' + Sym^.Name);
    EmitI('ld (iy-' + Int2Str(Sym^.Value+1) + '),d');
    EmitI('ld (iy-' + Int2Str(Sym^.Value+2) + '),e');
  end
end;

procedure EmitLiteral(Value: Integer);
begin
  Emit('', 'ld de,' + Int2Str(Value), 'Literal ' + Int2Str(Value));
  EmitI('push de');
end;

procedure EmitOdd();
var
  S: String;
begin
  Emit('', 'pop hl', 'Odd');
  EmitI('bit 0,l');
  EmitI('ld de,0');

  S := GetLabel('false');

  EmitI('jr z,' + S);
  EmitI('ld de,255');
  Emit(S, 'push de', '');
end;

procedure EmitCompare(Op: TToken);
var
  S, T: String;
begin
  Emit('','pop de', 'RelOp ' + Int2Str(Ord(Op)));
  EmitI('pop hl');

  if (Op = toGt) or (Op = toLeq) then EmitI('ex hl,de');

  T := GetLabel('SameSign');

  if (Op <> toEq) and (Op <> toNeq) then
  begin
    EmitI('ld a,h');
    EmitI('xor d');
    EmitI('and 128');
    EmitI('jr z,' + T);
    EmitI('ex hl,de');
    EmitL(T);
  end;

  if (Op = toGt) or (Op = toLeq) then EmitI('ex hl,de');

  EmitI('xor a');
  EmitI('sbc hl,de');

  S := GetLabel('false');

  case Op of
    toEq:   EmitI('jr nz,' + S);
    toNeq:  EmitI('jr z,' + S);
    toLt:   EmitI('jr nc,' + S);
    toGt:   EmitI('jr nc,' + S);
    toLeq:  EmitI('jr c,' + S);
    toGeq:  EmitI('jr c,' + S);
  end;

  EmitI('ld a,255');
  Emit(S, 'push af', '');
end;

procedure EmitJumpIf(Op: TToken; Target: String);
var
  S, T: String;
begin
  Emit('','pop de', 'RelOp ' + Int2Str(Ord(Op)));
  EmitI('pop hl');

  if (Op = toGt) or (Op = toLeq) then EmitI('ex hl,de');

  EmitI('call __comp16');

(*
  T := GetLabel('SameSign');

  if (Op <> toEq) and (Op <> toNeq) then
  begin
    EmitI('ld a,h');
    EmitI('xor d');
    EmitI('and 128');
    EmitI('jr z,' + T);
    EmitI('ex hl,de');
    EmitL(T);
  end;

  if (Op = toGt) or (Op = toLeq) then EmitI('ex hl,de');

  EmitI('xor a');
  EmitI('sbc hl,de');
*)
  S := GetLabel('false');

  case Op of
    toEq:   EmitI('jr z,' + Target);
    toNeq:  EmitI('jr nz,' + Target);
    toLt:   EmitI('jr c,' + Target);
    toGt:   EmitI('jr c,' + Target);
    toLeq:  EmitI('jr nc,' + S);
    toGeq:  EmitI('jr nc,' + S);
  end;

//  EmitI('ld a,255');
//  Emit(S, 'push af', '');
end;

procedure EmitCOmpare2(Op: TToken);
var
  S, T: String;
begin
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
  Emit('', 'pop de', '');
  Emit('', 'pop hl', '');
  Emit('', 'call __mul16', 'Mul');
  Emit('', 'push hl', '');
end;

procedure EmitDiv();
begin
  Emit('', 'pop de', '');
  Emit('', 'pop hl', '');
  Emit('', 'call __sdiv16', 'Div');
  Emit('', 'push hl', '');
end;

procedure EmitAdd();
begin
  Emit('', 'pop de', 'Add');
  EmitI('pop hl');
  EmitI('add hl,de');
  EmitI('push hl');
end;

procedure EmitSub();
begin
  Emit('', 'pop de', 'Sub');
  EmitI('pop hl');
  EmitI('xor a');
  EmitI('sbc hl,de');
  EmitI('push hl');
end;

procedure EmitInputNum(S: String);
begin
  Emit('', 'call __getn', 'Get ' + S);
  EmitI('push de');
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
  Sym := CreateSymbol(scString, S);
  Sym^.Tag := GetLabel('string');
  Emit('', 'ld hl,' + Sym^.Tag, '');
  Emit('', 'call __puts', '');
end;

procedure CloseTarget();
begin
  Close(Target);
end;

(* Parser *)

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
    NextToken; ParseExpression; Require(toRParen); NextToken;
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
  L: String;
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

    EmitCompare2(Op);
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
    NextToken; Require(toBecomes); NextToken; ParseExpression;

    EmitSetVar(Sym);
  end
  else if Scanner.Token = toCall then
  begin
    NextToken; Require(toIdent);
    Sym := LookupGlobal(Scanner.StrValue);
    if Sym = nil then Error('Identifier "' + Scanner.StrValue + '" not found.');
    if Sym^.Kind <> scProc then Error('"' + Scanner.StrValue + '" not a proc.');
    NextToken;

    EmitI('call ' + Sym^.Tag);
  end
  else if Scanner.Token = toAsk then
  begin
    NextToken; Require(toIdent);
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
    Require(toEnd); NextToken;
  end
  else if Scanner.Token = toIf then
  begin
    NextToken; ParseCondition; Require(toThen); NextToken;
    
    Tag := GetLabel('false');

    EmitI('pop af');
    EmitI('and a');
    EmitI('jp z,' + Tag);    

    ParseStatement;

    Emit(Tag, '', '');
  end
  else if Scanner.Token = toWhile then
  begin
    Tag := GetLabel('while');
    Tag2 := GetLabel('false');

    Emit(Tag, '', '');

    NextToken; ParseCondition; Require(toDo); NextToken;

    EmitI('pop af');
    EmitI('and a');
    EmitI('jp z,' + Tag2);    

    ParseStatement;

    EmitI('jp ' + Tag);    
    Emit(Tag2, '', '');
  end
end;

procedure ParseConst;
var
  Sym: PSymbol;
begin
  Require(toIdent);
  Sym := CreateSymbol(scConst, Scanner.StrValue);
  NextToken;
  Require(toEq);
  NextToken;
  Require(toNumber);

  Sym^.Value := Scanner.NumValue;

  NextToken;
end;

procedure ParseVar;
begin
  Require(toIdent);
  CreateSymbol(scVar, Scanner.StrValue);

  (* TODO: Put address into symbol. *)

  NextToken;
end;

procedure ParseBlock(Sym: PSymbol);
var
  S: String;
  NewSym: PSymbol;
begin
  if Scanner.Token = toConst then
  begin
    NextToken; ParseConst;
    while Scanner.Token = toComma do
    begin
      NextToken; ParseConst;
    end;
    require(toSemicolon);
    NextToken;
  end;

  if Scanner.Token = toVar then
  begin
    NextToken; ParseVar;
    S := 'var ' + SymbolTable^.Name;
    while Scanner.Token = toComma do
    begin
      NextToken; ParseVar;
      S := S + ', ' + SymbolTable^.Name;
    end;
    require(toSemicolon);
    NextToken;

    EmitC(S);
    EmitC('');
  end;

  while Scanner.Token = toProcedure do
  begin
    NextToken; Require(toIdent);
    NewSym := CreateSymbol(scProc, Scanner.StrValue);
    NewSym^.Tag := GetLabel('proc');

    EmitC('procedure ' + Scanner.StrValue);
    EmitC('');

    OpenScope;
    NextToken; Require(toSemicolon);
    NextToken; ParseBlock(NewSym);
    CloseScope;
    Require(toSemicolon);
    NextToken;

    EmitC('');
  end;

  if Sym <> Nil then EmitPrologue(Sym) else
  begin
    Emit('data', 'ds ' + Int2Str(Offset), 'Globals');
    EmitC('');
    EmitL('__main');
  end;

  parseStatement();
  if Sym <> Nil then EmitEpilogue(Sym) else EmitI('ret');
end;

procedure ParseProgram;
begin
  EmitC('');
  EmitC('program ' + ParamStr(1));
  EmitC('');
  if Binary = btCpm then
  begin
    EmitS('#define CPM 1');
    EmitC('');
    EmitI('org $100');
  end
  else if Binary = btDot then
  begin
    EmitS('#define NXT 1');
    EmitC('');
    EmitI('org $2000');
  end;

  EmitC('');
  EmitI('call __init');
  EmitI('call __main');
  EmitI('call __done');
  EmitI('ret');
  EmitC('');
  EmitInclude('pl0.z80');
  EmitC('');

  OpenScope;

  parseBlock(Nil);
  require(toPeriod);

  CloseScope;

  EmitC('');
  EmitC('end');
  EmitC('');
end;

(*
	Bytes 0...7	- +3DOS signature - 'PLUS3DOS'
	Byte 8		- 1Ah (26) Soft-EOF (end of file)
	Byte 9		- Issue number
	Byte 10		- Version number
	Bytes 11...14	- Length of the file in bytes, 32 bit number,
			    least significant byte in lowest address
	Bytes 15...22	- +3 BASIC header data
	Bytes 23...126	- Reserved (set to 0)
	Byte 127	- Checksum (sum of bytes 0...126 modulo 256)
*)

var
  H: array[0..127] of Byte;
  B: array[0..4095] of Byte;

procedure MakeNextFile(FileName: String);
var
  F: File of Byte;
  L, I: Integer;
begin
  WriteLn('*');

  Assign(F, FileName);
  Reset(F);
  L := FileSize(F);
  BlockRead(F, B, L);
  Close(F);

  WriteLn('**');

  for I := 0 to 127 do H[I] := 0;

  H[0] := Ord('P');
  H[1] := Ord('L');
  H[2] := Ord('U');
  H[3] := Ord('S');
  H[4] := Ord('3');
  H[5] := Ord('D');
  H[6] := Ord('O');
  H[7] := Ord('S');
  H[8] := 26;
  H[9] := 1;
  H[10] := 0;
  H[11] := 128;
  H[12] := 16;
  H[15] := 3;
  H[16] := 0; // 128;
  H[17] := 16;
  H[18] := 0;
  H[19] := 128;
  for I := 0 to 126 do H[127] := (H[127] + H[I]) mod 256;

  WriteLn('***');

  Rewrite(F);
  BlockWrite(F, H, 128);
  BlockWrite(F, B, 4096);
  Close(F);

  WriteLn('****');
end;

var
  SrcFile, AsmFile, BinFile: String;
  I: Integer;

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

  I := 1;  
  SrcFile := ParamStr(I);
  while Copy(SrcFile, 1, 2) = '--' do
  begin
    if SrcFile = '--cpm' then
      Binary := btCpm
    else if SrcFile = '--dot' then
      Binary := btDot
    else
      Error('***');
    I := I + 1;
    SrcFile := ParamStr(I);
  end;

  if SrcFile = '' then
  begin
    WriteLn('Usage: pl0 [ --cpm | --dot ] <src>');
    WriteLn;
    Halt(1);
  end;

  if Pos('.', SrcFile)=0 then SrcFile := SrcFile + '.pl0';

  AsmFile := ChangeExt(SrcFile, '.z80');

  if Binary = btCpm then
    BinFile := ChangeExt(SrcFile, '.com')
  else if Binary = btDot then
    BinFile := ChangeExt(SrcFile, '.dot');

  WriteLn('Compiling...');

  OpenInput(SrcFile);
  OpenTarget(AsmFile);
  NextToken;
  ParseProgram;
  CloseTarget();
  CloseInput();

  WriteLn('Assembling...');

  Exec('/Users/joerg/Library/bin/zasm', AsmFile + ' ' + BinFile);

  // MakeNextFile(BinFile);

  WriteLn('Success!');
end.
