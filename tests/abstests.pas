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

procedure TestAbs;
var
  R: MyRec;
begin
  with R.B do
  begin
    C := 3;
    D := 4;
  end;

  WriteLn(R.B.C, ' ', R.B.D);
end;

{$a-}
procedure TestRel;
var
  R: MyRec;
begin
  with R.B do
  begin
    C := 5;
    D := 6;
  end;

  WriteLn(R.B.C, ' ', R.B.D);
end;
{$a+}

begin
  with R.B do
  begin
    C := 1;
    D := 2;
  end;

  WriteLn(R.B.C, ' ', R.B.D);

  TestAbs;
  TestRel;
end.