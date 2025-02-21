program R;

var
  I, J: Integer;
  S: String;
  T: String[5];
begin
  Write('>>>');
  Read(I, J);
  WriteLn(I:6, J:6);
  Write('>>>');
  ReadLn(I, J);
  WriteLn(I:6, J:6);
  Write('>>>');
  ReadLn(S);
  WriteLn(S);
  Write('>>>');
  ReadLn(T);
  WriteLn(T);
end.