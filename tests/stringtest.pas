program Strings;

type
  Str255 = string;

function Replace(S: Str255; C, D: Char): Str255;
var
  I: Integer;
begin
  for I := 1 to Length(S) do
    if S[I] = C then
      S[I] := D;
  Replace := S;
end;

function Real2Str(R: Real; N: Integer): Str255;
var
  S: Str255;
begin
  Str(R:Abs(N):0, S);
  if N < 0 then
    S := Replace(S, ' ', '0');
  Real2Str := S;
end;

function Int2Str(I: Integer; N: Integer): Str255;
var
  S: Str255;
begin
  Str(I:Abs(N), S);
  if N < 0 then
    S := Replace(S, ' ', '0');
  Int2Str := S;
end;

var
  S: string;
  R: Real;

begin
  R := 3.1415;

  Str(R, S);
  WriteLn(R, ' -> ', S);
  Str(R:15, S);
  WriteLn(R:15, ' -> ', S);
  Str(R:15:5, S);
  WriteLn(R:15:5, ' -> ', S);

  WriteLn(Real2Str(3.1415, 5));
  WriteLn(Int2Str(42, 5));
end.
