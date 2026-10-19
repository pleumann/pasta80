(**
 * Simple command-line calculator.
 *
 * Evaluates each command line argument as a separate real-valued
 * math expression and prints the result. Supports +, -, *, / ,
 * parentheses, E-notation (e.g. 1.5E-3), the functions Sin, Cos,
 * Tan, Sqrt, Ln, Exp, the constant Pi, and Res, which yields the
 * result of the previous expression (0 for the first one). Since
 * the shell already splits arguments on spaces, expressions may not
 * contain any whitespace themselves. Each expression is evaluated
 * on the fly while parsing, in typical Pascal fashion. There is no
 * syntax tree.
 *
 * The ZX Spectrum has no command line, so there the program prompts
 * for one expression after another instead (spaces are ignored) and
 * stops at an empty line. Errors don't end the program there, they
 * just skip the current expression.
 *
 * Grammar:
 *   Expr   ::= Term { ('+' | '-') Term }
 *   Term   ::= Factor { ('*' | '/') Factor }
 *   Factor ::= ['+' | '-'] (Number | '(' Expr ')' | Ident ['(' Expr ')'])
 *)
program Calc;

{$a-} { Needed because grammar is recursive }

type
  TCharSet = set of Char;

const
  Digits: TCharSet = ['0'..'9'];
  Alpha: TCharSet = ['A'..'Z', 'a'..'z'];

var
  Expr: String;
  Pos: Integer;
  I: Integer;
  Arg: String;
  Value, Result: Real;
  Error: Boolean;

{$ifdef SYS_ZX}
(**
 * Simulates command line arguments by prompting for them. The index is
 * ignored, each call reads a new line. Spaces are removed, since they
 * would otherwise end the expression.
 *)
function ParamStr(I: Byte): String;
var
  S, T: String;
  J: Integer;
begin
  Write('> ');
  ReadLn(S);

  T := '';
  for J := 1 to Length(S) do
    if S[J] <> ' ' then
      T := T + S[J];

  ParamStr := T;
end;
{$endif}

(**
 * Exits with an error message. On the ZX Spectrum, only reports the first
 * error and moves to the end of the expression, so the parser unwinds
 * without consuming anything else.
 *)
procedure Fail(Msg: String);
begin
  if Error then
    Exit;

  WriteLn;
  WriteLn('Error: ', Msg, ' at position ', Pos, '.');
{$ifdef SYS_ZX}
  Error := True;
  Pos := Length(Expr);
{$else}
  Halt(1);
{$endif}
end;

(**
 * Checks a single time for a character from the given set and skips it.
 * Returns True if a character has been skipped.
 *)
function SkipOne(Chars: TCharSet): Boolean;
begin
  if Expr[Pos] in Chars then
  begin
    SkipOne := True;
    Inc(Pos);
  end
  else
    SkipOne := False;
end;

(**
 * Checks multiple times for a character from the given set and skips it.
 * Returns True if at least one character has been skipped.
 *)
function SkipMany(Chars: TCharSet): Boolean;
begin
  SkipMany := False;

  while Expr[Pos] in Chars do
  begin
    SkipMany := True;
    Inc(Pos);
  end;
end;

function ParseExpr: Real; forward;

(**
 * Parses a full floating point number and returns it as a Real value.
 *)
function ParseNumber: Real;
var
  Start: Integer;
  S: String;
  Value: Real;
  ErrPos: Integer;
begin
  Start := Pos;

  SkipMany(Digits);

  if SkipOne(['.']) then
    SkipMany(Digits);

  if SkipOne(['E', 'e']) then
  begin
    SkipOne(['+', '-']);
    SkipMany(Digits);
  end;

  S := Copy(Expr, Start, Pos - Start);

  Val(S, Value, ErrPos);
  if ErrPos <> 0 then
  begin
    Value := 0;
    Fail('invalid number "' + S + '"');
  end;

  ParseNumber := Value;
end;

(**
 * Parses a full identifier and returns it as a String.
 *)
function ParseIdent: String;
var
  S: String;
begin
  S := '';

  while Expr[Pos] in Alpha do
  begin
    S := S + UpCase(Expr[Pos]);
    Inc(Pos);
  end;

  ParseIdent := S;
end;

(**
 * Parses a function call, performs it, and return the result as a Real.
 *)
function ParseCall(Name: String): Real;
var
  Arg: Real;
begin
  ParseCall := 0;
  Arg := 0;

  if Expr[Pos] = '(' then
  begin
    Inc(Pos);
    Arg := ParseExpr;

    if Expr[Pos] = ')' then
      Inc(Pos)
    else
      Fail('missing closing parenthesis');
  end
  else
    Fail('( expected after function name');

  if Name = 'SIN' then
    ParseCall := Sin(Arg)
  else if Name = 'COS' then
    ParseCall := Cos(Arg)
  else if Name = 'TAN' then
    ParseCall := Tan(Arg)
  else if Name = 'SQRT' then
  begin
    if Arg < 0 then
      Fail('square root of negative number')
    else
      ParseCall := Sqrt(Arg);
  end
  else if Name = 'LN' then
  begin
    if Arg <= 0 then
      Fail('logarithm of non-positive number')
    else
      ParseCall := Ln(Arg);
  end
  else if Name = 'EXP' then
    ParseCall := Exp(Arg)
  else
    Fail('unknown function "' + Name + '"');
end;

(**
 * Parses a factor, which can basically be be a number, a function call, or
 * another expression enclosed in paretheses.
 *)
function ParseFactor: Real;
var
  Value: Real;
  Negate: Boolean;
  Name: String;
begin
  Value := 0;
  Negate := False;
  if Expr[Pos] = '-' then
  begin
    Negate := True;
    Inc(Pos);
  end
  else if Expr[Pos] = '+' then
    Inc(Pos);

  if Expr[Pos] = '(' then
  begin
    Inc(Pos);
    Value := ParseExpr;

    if Expr[Pos] = ')' then
      Inc(Pos)
    else
      Fail('missing closing parenthesis');
  end
  else if Expr[Pos] in Digits then
    Value := ParseNumber
  else if Expr[Pos] in Alpha then
  begin
    Name := ParseIdent;
    if Name = 'PI' then
      Value := Pi
    else if Name = 'RES' then
      Value := Result
    else
      Value := ParseCall(Name);
  end
  else
    Fail('number, identifier or parenthesis expected');

  if Negate then
    Value := -Value;

  ParseFactor := Value;
end;

(**
 * Parses a term, which is basically composed of factors and multiplicative
 * operators.
 *)
function ParseTerm: Real;
var
  Value, Divisor: Real;
  Op: Char;
begin
  Value := ParseFactor;

  while (Expr[Pos] = '*') or (Expr[Pos] = '/') do
  begin
    Op := Expr[Pos];
    Inc(Pos);

    if Op = '*' then
      Value := Value * ParseFactor
    else
    begin
      Divisor := ParseFactor;
      if Divisor = 0 then
        Fail('division by zero')
      else
        Value := Value / Divisor;
    end;
  end;

  ParseTerm := Value;
end;

(**
 * Parses an expression, which is basically composed of terms and additive
 * operators.
 *)
function ParseExpr;
var
  Value: Real;
  Op: Char;
begin
  Value := ParseTerm;

  while (Expr[Pos] = '+') or (Expr[Pos] = '-') do
  begin
    Op := Expr[Pos];
    Inc(Pos);

    if Op = '+' then
      Value := Value + ParseTerm
    else
      Value := Value - ParseTerm;
  end;

  ParseExpr := Value;
end;

begin
  Arg := ParamStr(1);
{$ifndef SYS_ZX}
  if Arg = '' then
  begin
    WriteLn('Usage: calc <expression> [<expression> ...]');
    Halt(1);
  end;
{$endif}

  Result := 0;
  I := 1;

  while Arg <> '' do
  begin
    { The trailing space can never occur inside an expression (the
      shell already split argument on spaces), so it works as an
      end-of-input sentinel: Expr[Pos] is thus always valid and the
      scanning loops don't need to test Pos against Length(Expr). }
    Expr := Arg + ' ';

    Pos := 1;
    Error := False;
    Value := ParseExpr;

    if Expr[Pos] <> ' ' then
      Fail('unexpected character');

    if not Error then
    begin
      Result := Value;
      WriteLn(Arg, ' = ', Result:0:6);
    end;

    Inc(I);
    Arg := ParamStr(I);
  end;
end.
