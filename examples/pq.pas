program PQ;

var
  P, Q, D: Real;

begin
  ClrScr;

  WriteLn('PQ Formula');
  WriteLn('----------');
  WriteLn;
  WriteLn('This program solves quadratic');
  WriteLn('equations that are given in');
  WriteLn('the following normal form:');
  WriteLn;
  WriteLn('  x^2 + p * x + q = 0');
  WriteLn;

  Write('p='); ReadLn(P);
  Write('q='); ReadLn(Q);

  WriteLn;

  D := P * P / 4 - Q;


  if D < 0 then
    WriteLn('The equation is unsolvable!')
  else if D = 0 then
  begin
    WriteLn('The equation has one solution:');
    WriteLn;
    WriteLn('x=', -P / 2:10:5);
  end
  else
  begin
    WriteLn('The equation has two solutions:');
    WriteLn;
    WriteLn('x1=', -P / 2+Sqr(D):10:5);
    WriteLn('x2=', -P / 2-Sqr(D):10:5);
  end;
end.
