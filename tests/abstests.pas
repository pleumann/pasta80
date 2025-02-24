program AbsTests;

type
  MyRec = record
    A: Integer;
    B: record
      C, D: Integer;
    end;
  end;

var
  R: MyRec;

  B: Byte;

begin
  R.A := 1;
  R.B.C := 2;
  R.B.D := 3;

  WriteLn(R.B.D);

  B := 0;
end.