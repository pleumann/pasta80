program Rot13;

function Crypt(S: string): string;
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

var
  S: string;

begin
  repeat
    WriteLn('Enter a message (leave empty to exit).');
    ReadLn(S);
    WriteLn(Crypt(S));
    WriteLn;
  until S = '';
end.
