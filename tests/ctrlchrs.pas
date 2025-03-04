program CtrlChrs;

var
  C: Char;

begin
  repeat
    C := ReadKey;
    if C in ['0'..'9'] then
      WriteLn('Digit   ', C)
    else if C in ['A'..'Z'] then
      WriteLn('Normal  ', C)
    else if C in ['a'..'z'] then
      WriteLn('Shift   ', C)
    else if C in [^A..^Z] then
      WriteLn('Control ' + Chr(Ord(C) + 64))
  until C = ^[;
end.