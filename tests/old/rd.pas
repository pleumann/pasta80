Program ReadTest;
  
var
  C: Char;
  I: Integer;
  R: Real;
  S: String[2];
  T: String[8];
  U: String;

begin
  Write('>');
  ReadLn(C, I, R, S, T, U);

  WriteLn('C=', C);
  WriteLn('I=', I);
  WriteLn('R=', R);
  WriteLn('S=', S);
  WriteLn('T=', T);
  WriteLn('U=', U);

(*
  ReadLn(I);
  WriteLn('I=', I);
  
  ReadLn(I, J);
  WriteLn('I=', I);
  WriteLn('J=', J);
  
  ReadLn(R);
  WriteLn(R);

  ReadLn(C);
  WriteLn(C);
  Read(C, D);
  WriteLn(C, '/', D);

  ReadLn(S);
  WriteLn(S);

  ReadLn(T);
  WriteLn(T);

  ReadLn(T, U);
  WriteLn(T);
  WriteLn(U);
*)
end.