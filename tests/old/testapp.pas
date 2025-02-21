program TestApp;

{$I /Users/joerg/Projekte/pl0/lib/files.pas}

var
  T: Text;

begin
  Assign(T, 'test.txt');
  Rewrite(T);
  WriteLine(T, 'Hallo, Welt!');
  Close(T);

  Assign(T, 'test.txt');
  Append(T);
  WriteLine(T, 'Hello, world!');
  Close(T);
end.