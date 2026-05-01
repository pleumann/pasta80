program Sets;

const
  Primes: set of Byte = [2, 3, 5, 7, 11, 13];

type
  CharSet = set of Char;
 
procedure Dump(S: CharSet);
var
  C: Char;
begin
  for C := ' ' to #126 do
    if C in S then Write(C);
  WriteLn;
end;

var
  C, X, Z: Char;
  S: CharSet;

begin
  S := ['a', 'b', 'c', 'x'..'z'];

  Dump(S);

  C := 'c';
  X := 'x';
  Z := 'z';

  S := ['a', 'b', C, X .. Z];
  Dump(S);

  S := ['a', 'b', C, Z .. X];
  Dump(S);

  S := [Chr(321)];
  Dump(S);
end.