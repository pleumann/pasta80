program P;

var
  I: Integer;

begin
  for I := 1 to ParamCount do
    WriteLn('#', I:2, ': ', ParamStr(I));
end.