program Overflow;

{$k+}

procedure Recurse(I: Integer);
begin
  WriteLn(I);
  Recurse(I + 1);
end;

begin
  Recurse(0);
end.