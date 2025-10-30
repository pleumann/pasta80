program Anagrams;

{$A-}
{$U+}

var
  B: Byte;
  R: Real;
  S: String;

procedure Recurse(var S: String; Left, Right: Byte);
var
  C: Char;
  I: Byte;
begin
  if Left = Right then
  begin
    Write(S:16);
    B := B + 16;
    if B = ScreenWidth then
    begin
      WriteLn;
      B := 0;
    end;
    R := R + 1;
//    B := (B + 1) mod 4;
//    if B = 0 then WriteLn;
    Exit;
  end;

  C := S[Left];
  Recurse(S, Left + 1, Right);
  for I := Left + 1 to Right do
  begin
    S[Left] := S[I];
    S[I] := C;
    Recurse(S, Left + 1, Right);
    S[I] := S[Left];
    S[Left] := C;
  end;
end;

begin
  ClrScr;
  WriteLn('This program generates anagrams.');
  WriteLn;
  Write('Enter a word: ');
  ReadLn(S);
  WriteLn;

  R := 0;
  B := 0;
  Recurse(S, 1, Length(S));

  if B <> 0 then WriteLn;
  WriteLn;
  WriteLn(R:0:0, ' anagram(s) found.');
  WriteLn;
end.