program Io;

var
  T: Text;
  S: string;

begin
  Assign(T, 'notthere.txt');

  {$i-}

  WriteLn('Now doing unchecked IO...');

  Reset(T);
  ReadLn(T, S);
  WriteLn(S);
  Close(T);

  WriteLn('IOResult=', IOResult);

  {$i+}

  WriteLn('Now doing checked IO...');

  Reset(T);
  ReadLn(T, S);
  WriteLn(S);
  Close(T);

  WriteLn('You should not see this.');
end.
