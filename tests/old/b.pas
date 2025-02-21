var
  S: String;
  T: String[3];
  U: String[0];

begin
  Str(12345, S);
  WriteLn(S);
  Str(12345, T);
  WriteLn(T);
  Str(12345, U);
  WriteLn(U);

  WriteLn('-----');
  Str(12345:3, S);
  WriteLn('*', S, '* ', Length(S));
  Str(12345:5, S);
  WriteLn('*', S, '* ', Length(S));
  Str(12345:7, S);
  WriteLn('*', S, '* ', Length(S));

  Str(12345:10, T);
  WriteLn(T);
  Str(12345:10, U);
  WriteLn(U);

  WriteLn('-----');

  Str('X', S);
  WriteLn(S);
  Str('Y', T);
  WriteLn(T);
  Str('Z', U);
  WriteLn(U);

  WriteLn('-----');

  Str('ABCDE', S);
  WriteLn(S);
  Str('ABCDE', T);
  WriteLn(T);
  Str('ABCDE', U);
  WriteLn(U);

  WriteLn('-----');

  Str(1.2345, S);
  WriteLn(S);
  Str(1.2345, T);
  WriteLn(T);
  Str(1.2345, U);
  WriteLn(U);

  WriteLn('-----');

  Str(True, S);
  WriteLn(S);
  Str(True, T);
  WriteLn(T);
  Str(True, U);
  WriteLn(U);
end.