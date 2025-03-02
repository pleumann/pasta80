program Params;

var
  I, J: Integer;

begin
  I := ParamCount;

  WriteLn('You provided ', I, ' command line parameters.');
  WriteLn;

  for J := 1 to I do
    WriteLn('Param #', J, ': ', ParamStr(J));

  WriteLn;
end.