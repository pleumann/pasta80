program FF;

{$a+}
{$i /Users/joerg/Projects/pl0/lib/cpm22.pas}

type
  TIntFile = file of Integer;
  TRecFile = file of record A, B, C: Integer; end;
  TAnyFile = File;

var
  IntFile: file of Integer;
  RecFile: file of record A, B, C: Integer; end;
  AnyFile: File;

  TxtFile: Text;

begin
  Assign(AnyFile, 'anyfile.bin');
  Assign(IntFile, 'intfile.dat');
  Assign(TxtFile, 'txtfile.txt');
end.
