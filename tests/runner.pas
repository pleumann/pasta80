program Runner;

uses
  Dos;

var
  S: String;

begin
  Write('> ');
  ReadLn(S);
  Exec(S, '');
  WriteLn('DosError=', DosError, ' ExitCode=', DosExitCode);
end.