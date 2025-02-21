program TextTest;

{$a+}
{$i /Users/joerg/Projekte/pl0/lib/cpm22.pas}

var
  T: Text;
  S: String;
  I: Integer;
  R: Real;

begin
  Assign(T, 'test.txt');
  Rewrite(T);
  WriteLn(T, 'Hello, world!');
  WriteLn(T, 42, 3.1415, ' :-)');
  WriteLn(T, 'That is ', True, '!');
  Close(T);

  Reset(T);
  ReadLn(T, S);
  WriteLn('Read a line: ', S);
  Read(T, I);
  WriteLn('Read an int: ', I);
  Read(T, R);
  WriteLn('Read a real: ', R);
  ReadLn(T, S);
  WriteLn('Read a face: ', S);
  ReadLn(T, S);
  WriteLn('Read a line: ', S);
  Close(T);
end.