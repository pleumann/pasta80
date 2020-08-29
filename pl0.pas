program PL0;

(* Utility *)

function UpperStr(S: String): String;
var
  I: Integer;
begin
  for I := 1 to Length(S) do S[I] := UpCase(S[I]);
  UpperStr := S;  
end;

// Debug / Warning / Error

procedure Error(Message: String);
begin
  WriteLn;
  WriteLn('*** Error: ', Message);
  Halt(1);
end;
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

    WriteLn('[', Source.Line, '] ', Source.Buffer);
  end;

  GetChar := Source.Buffer[Source.Column];
  Write(Source.Buffer[Source.Column]);
  Inc(Source.Column);
end;

(* Table *)

type
  TSymbolClass = (scConst, scVar, scProc, scScope);

  PSymbol = ^TSymbol;
  TSymbol = record
    Name: String;
    Kind: TSymbolClass;
    Next: PSymbol;
  end;

var
  SymbolTable: PSymbol;

procedure OpenScope();
var
  Sym: PSymbol;
begin
  New(Sym);
  Sym^.Kind := scScope;
  Sym^.Next := SymbolTable;
  SymbolTable := Sym;
end;

procedure CloseScope();
var
  Sym: PSymbol;
  Kind: TSymbolClass;
begin
  repeat
    Kind := SymbolTable^.Kind;
    Sym := SymbolTable^.Next;
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
    if UpperStr(Sym^.Name) = Name then
    begin
      LookupGlobal := Sym;
      Exit;
    end;
    Sym := Sym^.Next;
  end;

  LookupGlobal := nil;
end;

procedure CreateSymbol(Kind: TSymbolClass; Name: String);
var
  Sym: PSymbol;
begin
  if LookupLocal(Name) <> nil then
  begin
    Error('Duplicate symbol "' + Name + '".');
  end;

  New(Sym);
  Sym^.Kind := Kind;
  Sym^.Name := Name;
  Sym^.Next := SymbolTable;
  SymbolTable := Sym;
end;

(* --- Scanner --------------------------------------------------------- *)

type
  TToken = (toNone,
            toIdent, toNumber,
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

    Write('{', Token, '->', StrValue , '}');
  end;
end;

procedure require(Token: TToken);
begin
  Write('<', Token, '/', Scanner.Token, '>');
  if Scanner.Token <> Token then Error('Parser error');
end;

(* Emitter *)

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
    NextToken;
  end
  else if Scanner.Token = toNumber then
  begin
    NextToken;
  end
  else if Scanner.Token = toLParen then
  begin
    NextToken; ParseExpression; Require(toRParen); NextToken;
  end
  else Error('Factor expected');
end;

procedure ParseTerm;
begin
  ParseFactor;
  while (Scanner.Token = toMul) or (Scanner.Token = toDiv) do
  begin
    NextToken;
    ParseFactor;
  end;
end;

procedure ParseExpression;
begin
  if (Scanner.Token = toAdd) or (Scanner.Token = toSub) then
  begin
    NextToken;
  end;

  ParseTerm;

  while (Scanner.Token = toAdd) or (Scanner.Token = toSub) do
  begin
    NextToken;
    ParseTerm;
  end;
end;

procedure ParseCondition;
begin
  if Scanner.Token = toOdd then
  begin
    NextToken; ParseExpression;
  end 
  else
  begin
    ParseExpression;
    case Scanner.Token of
      toEq:  begin NextToken; end;
      toNeq: begin NextToken; end;
      toLt:  begin NextToken; end;
      toLeq: begin NextToken; end;
      toGt:  begin NextToken; end;
      toGeq: begin NextToken; end;
      else Error('RelOp expected');
    end;
    ParseExpression;
  end;
end;

procedure ParseStatement;
var
  Sym: PSymbol;
begin
  if Scanner.Token = toIdent then
  begin
    Sym := LookupGlobal(Scanner.StrValue);
    if Sym = nil then Error('Identifier "' + Scanner.StrValue + '" not found.');
    if Sym^.Kind <> scVar then Error('"' + Scanner.StrValue + '" not a var.');
    NextToken; Require(toBecomes); NextToken; ParseExpression;
  end
  else if Scanner.Token = toCall then
  begin
    NextToken; Require(toIdent);
    Sym := LookupGlobal(Scanner.StrValue);
    if Sym = nil then Error('Identifier "' + Scanner.StrValue + '" not found.');
    if Sym^.Kind <> scProc then Error('"' + Scanner.StrValue + '" not a proc.');
    NextToken;
  end
  else if Scanner.Token = toAsk then
  begin
    NextToken; Require(toIdent);
    Sym := LookupGlobal(Scanner.StrValue);
    if Sym = nil then Error('Identifier "' + Scanner.StrValue + '" not found.');
    if Sym^.Kind <> scVar then Error('"' + Scanner.StrValue + '" not a var.');
    NextToken;
  end
  else if Scanner.Token = toSay then
  begin
    NextToken; ParseExpression;
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
    NextToken; ParseCondition; Require(toThen); NextToken; ParseStatement;
  end
  else if Scanner.Token = toWhile then
  begin
    NextToken; ParseCondition; Require(toDo); NextToken; ParseStatement;
  end
end;

procedure ParseConst;
begin
  Require(toIdent);
  CreateSymbol(scConst, Scanner.StrValue);
  NextToken;
  Require(toEq);
  NextToken;
  Require(toNumber);
  NextToken;
end;

procedure ParseVar;
begin
  Require(toIdent);
  CreateSymbol(scVar, Scanner.StrValue);
  NextToken;
end;

procedure ParseBlock;
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
    while Scanner.Token = toComma do
    begin
      NextToken; ParseVar;
    end;
    require(toSemicolon);
    NextToken;
  end;

  while Scanner.Token = toProcedure do
  begin
    NextToken; Require(toIdent);
    CreateSymbol(scProc, Scanner.StrValue);
    OpenScope;
    NextToken; Require(toSemicolon);
    NextToken; ParseBlock;
    CloseScope;
    Require(toSemicolon);
    NextToken;
  end;

  parseStatement();
end;

procedure ParseProgram;
begin
  parseBlock;
  require(toPeriod);  
end;

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

  OpenScope;

  OpenInput(ParamStr(1));
  NextToken;
  ParseProgram;
  CloseInput();
end.
