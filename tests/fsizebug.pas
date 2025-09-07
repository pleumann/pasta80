program FSizeBug;

var
  F: File;

  B: array[0..127] of Byte;

  I: Integer;

begin
  Assign(F, 'test.tmp');
  Rewrite(F);

  WriteLn('File created, size is ', FileSize(F));

  BlockWrite(F, B, 1, I);

  WriteLn('Record written, size is ', FileSize(F));

  Close(F);
end.