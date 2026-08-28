program Stack;

{$k+}

procedure Recurse(I: Integer);
begin
  Recurse(I + 1);
end;

begin
  WriteLn('before');
  Recurse(0);
  WriteLn('after');
end.