program Breakpoints;

var
  I: Integer;

begin
  WriteLn('Foo');
  Debug;
  WriteLn('Bar');

  for I := 1 to 20 do
  begin
    WriteLn(I);
    Delay(100);
    Debug(I mod 6 = 0);
  end;
end.