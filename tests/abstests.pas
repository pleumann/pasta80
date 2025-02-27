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

procedure TestAbs;
var
  R: MyRec;
begin
  with R.B do
  begin
    C := 3;
    D := 4;
  end;
end;

{$a-}
procedure Test1;
var
  R: MyRec;
begin
  with R.B do
  begin
    C := 5;
    D := 6;
  end;
end;
{$a+}

begin
  with R.B do
  begin
    C := 1;
    D := 2;
  end;
end.