program CtrlChrs;

const
  Upper: set of Char = ['A'..'Z'];

var
  Lower: set of Char;

var
  C: Char;

begin
  WriteLn('Press key to analyze (ESC to quit).');
  WriteLn;

  Lower := ['a'..'z'];

  repeat
    C := ReadKey;
    if C in ['0'..'9'] then
      WriteLn('Digit   ', C)
    else if C in Lower then
      WriteLn('Normal  ', C)
    else if C in Upper then
      WriteLn('Shift   ', C)
    else if C in [^A..^Z] then
      WriteLn('Control ' + Chr(Ord(C) + 64))
    else
      WriteLn('Other   #', Ord(C));
  until C = ^[;
end.