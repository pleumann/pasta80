program Stack;

{ Unbounded recursion must be caught by the stack check rather than running
  into the heap. Only active with the k-plus directive below; without it the
  program would simply crash (OPEN-ITEMS B18).
  Expected output:
    before
    Stack overflow
  ("after" must not print.) }

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