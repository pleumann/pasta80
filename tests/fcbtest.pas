program FcbTest;

{$a+}
{$i/Users/joerg/Projekte/pl0/lib/cpm22.pas}

var
  B: FileControlBlock;

  R: array[0..255] of Char;

  C: Char;

  E: Integer;

begin
  BlockAssign(B, 'test.blk');
  BlockErase(B);
  BlockRewrite(B);

  for C := '0' to '9' do
  begin
    WriteLn('Writing record of ', C, 's at pos ', BlockFilePos(B));
    FillChar(R, 128, C);
    BlockWrite(B, R, 1, E);
  end;

  BlockClose(B);

  WriteLn('File size: ', BlockFileSize(B));

  BlockReset(B);

  BlockSeek(B, 4);

  WriteLn('Writing record of As at pos ', BlockFilePos(B));
  FillChar(R, 128, 'A');
  BlockWrite(B, R, 1, E);
  WriteLn('Writing record of Bs at pos ', BlockFilePos(B));
  FillChar(R, 128, 'B');
  BlockWrite(B, R, 1, E);

  BlockSeek(B, BlockFileSize(B));

  WriteLn('Writing 2 records of Zs at pos ', BlockFilePos(B));
  FillChar(R, 256, 'Z');
  BlockWrite(B, R, 2, E);

  BlockClose(B);

  WriteLn('File size: ', BlockFileSize(B));
end.