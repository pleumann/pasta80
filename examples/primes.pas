program Primes;

var
  Primes: array[0..9999] of Integer;
  Count, N: Integer;

function IsPrime(N: Integer): Boolean;
var
  I, P: Integer;
begin
  IsPrime := False;
  I := 0;
  P := Primes[I];
  while P * P <= N do
  begin
    if N mod Primes[I] = 0 then Exit;
    Inc(I);
    P := Primes[I];
  end;
  IsPrime := True;
end;

begin
  Write(2:8);
  Primes[0] := 2;
  Count := 1;
  N := 3;
  while Count < 1000 do
  begin
    if IsPrime(N) then
    begin
      Write(N:8);
      Primes[Count] := N;
      Inc(Count);
      if Count mod (ScreenWidth / 8) = 0 then WriteLn;
    end;
    Inc(N);
  end;
end.
