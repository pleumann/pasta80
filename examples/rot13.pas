program Rot13;

type
  Str255 = string[255];
  
function Crypt(S: Str255): Str255;
var
  I: Integer;
  C: Char;
begin
  for I := 1 to Length(S) do
  begin
    C := S[I];
    if (C >= 'A') and (C <= 'Z') then
      S[I] := Char(65 + (Ord(C) - 65 + 13) mod 26)
    else if (C >= 'a') and (C <= 'z') then
      S[I] := Char(97 + (Ord(C) - 97 + 13) mod 26);
  end;

  Crypt := S;
end;

begin
  repeat
    WriteLn('Enter a message (leave empty to exit).');
    ReadLn(S);
    WriteLn(Crypt(S));
    WriteLn;
  until S = '';
end.
