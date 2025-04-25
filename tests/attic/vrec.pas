program VRec;

type
  MyRec = record
    C: Char;
    case B: Boolean of
      False: (I: Integer;);
      True:  (Lo, Hi: Byte;);
  end;

var
  R1: MyRec;

const
  R2: MyRec = (
    C: 'A';
    B: False;
    I: 42;
  );

  R3: MyRec = (
    C: 'B';
    B: True;
    Lo: 1;
    Hi: 2;
  );
  
procedure Dump(var R: MyRec; Name: TString);
begin
  WriteLn('--- ', Name, ' is ', SizeOf(R2), ' bytes at ', Addr(R), ' ---');
  WriteLn('@C=  ', Addr(R.C), ' C=', R.C);
  WriteLn('@B=  ', Addr(R.B), ' B=', R.B);
  WriteLn('@I=  ', Addr(R.I), ' I=', R.I);
  WriteLn('@Lo= ', Addr(R.Lo), ' Lo=', R.Lo);
  WriteLn('@Hi= ', Addr(R.Hi), ' Hi=', R.Hi);
  WriteLn;
end;

begin
  R1.C := 'X';
  R1.B := False;
  R1.I := 16383;

  Dump(R1, 'R1');

  R1.B := True;
  R1.Hi := 64;
  R1.Lo := 0;

  Dump(R1, 'R1');
  Dump(R2, 'R2');
  Dump(R3, 'R3');
end.