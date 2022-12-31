(* Test *)

program Hello;

var
  I: Integer;
  
begin
  for I := 0 to 7 do
  begin
    inline($DD/$01/$00/$00);
    TextBackground(I);
    TextColor(7 - I);
    WriteLn('Hello, Pascal!');
  end;
end.
