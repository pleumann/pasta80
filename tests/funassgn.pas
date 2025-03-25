program FunAssgn;

type
  PStuff = ^TStuff;
  TStuff = record
    X, Y: Integer;
  end;

var
  Stuff: TStuff;

function GetVar: PStuff;
begin
  GetVar := Ptr(Addr(Stuff));
end;

begin
  GetVar^.X := 1;
  GetVar^.Y := 2;

  WriteLn(Stuff.X, ' ', Stuff.Y);
end.