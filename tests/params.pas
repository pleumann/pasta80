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

  WriteLn('Param #0: ', ParamStr(0));
  WriteLn('Param #', I + 1, ': ', ParamStr(I + 1));

  WriteLn;

  Assert(ParamCount= 3);
  Assert(ParamStr(0) = '');
  Assert(ParamStr(1) = 'TIC');
  Assert(ParamStr(2) = 'TAC');
  Assert(ParamStr(3) = 'TOE');
  Assert(ParamStr(4) = '');

  WriteLn;
  WriteLn('************************');
  WriteLn('Passed assertions: ', AssertPassed);
  WriteLn('Failed assertions: ', AssertFailed);
  WriteLn('************************');
  WriteLn;

  {$ifdef SYS_AGON}
    inline($3e / $00 / $d3 / $00);
  {$endif}
  {$ifdef SYS_ZXNEXT}
    QuitEmulator;
  {$endif}
end.
