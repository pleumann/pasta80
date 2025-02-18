program RawFiles;

{$a+}
{$i/Users/joerg/Projekte/pl0/lib/cpm22.pas}

var
  Raw: File;

  Buffer: array[0..255] of Char;

  C: Char;

  Actual: Integer;

begin
  Assign(Raw, 'test.raw');
  Erase(Raw);
  Rewrite(Raw);

  for C := '0' to '9' do
  begin
    WriteLn('Writing record of ', C, 's at pos ', FilePos(Raw));
    FillChar(Buffer, 128, C);
    BlockWrite(Raw, Buffer, 1, Actual);
  end;

  Close(Raw);

  WriteLn('File size: ', FileSize(Raw));

  Reset(Raw);

  Seek(Raw, 4);

  WriteLn('Writing record of As at pos ', FilePos(Raw));
  FillChar(Buffer, 128, 'A');
  BlockWrite(Raw, Buffer, 1, Actual);
  WriteLn('Writing record of Bs at pos ', FilePos(Raw));
  FillChar(Buffer, 128, 'B');
  BlockWrite(Raw, Buffer, 1, Actual);

  Seek(Raw, FileSize(Raw));

  WriteLn('Writing 2 records of Zs at pos ', FilePos(Raw));
  FillChar(Buffer, 256, 'Z');
  BlockWrite(Raw, Buffer, 2, Actual);

  Close(Raw);

  WriteLn('File size: ', FileSize(Raw));

  WriteLn('Let''s walk through the file contents...');

  Reset(Raw);

  while not Eof(Raw) do
  begin
    Write('#', FilePos(Raw), ': ');
    BlockRead(Raw, Buffer, 1, Actual);
    WriteLn(Buffer[0], '..', Buffer[127]);
  end;

  Close(Raw);
end.