program Params;

{$ifdef SYS_ZX}
  {$error Agon or CP/M required.}
{$endif}

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