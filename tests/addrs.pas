program A;

var
  X, Y, Z: Integer;

procedure B(P, Q, R: Integer);
var
  S, T, U: Integer;
begin
  WriteLn('@P=', Addr(P));
  WriteLn('@Q=', Addr(Q));
  WriteLn('@R=', Addr(R));

  WriteLn('@S=', Addr(S));
  WriteLn('@T=', Addr(T));
  WriteLn('@U=', Addr(U));

end;

begin
  WriteLn('@X=', Addr(X));
  WriteLn('@Y=', Addr(Y));
  WriteLn('@Z=', Addr(Z));

  B(1, 2, 3);
end.