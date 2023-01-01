program F;

procedure TestWriteFormat;

type
  Color = (Red, Green, Blue);
  
const
  C: Char = 'X';

  procedure TestWriteDefault;
  begin
    Writeln('Now testing default format...');
    WriteLn;

    WriteLn('|', 0, '|');
    WriteLn('|', 1, '|');
    WriteLn('|', -1, '|');
    WriteLn('|', 1234, '|');
    WriteLn('|', -1234, '|');
    WriteLn('|', 12345, '|');
    WriteLn('|', -12345, '|');

    Writeln;

    WriteLn('|', C, '|');

    Writeln;

    WriteLn('|', False, '|');
    WriteLn('|', True, '|');
    WriteLn('|', Red, '|');
    WriteLn('|', Green, '|');
    WriteLn('|', Blue, '|');

    Writeln;

    WriteLn('|', '', '|');
    WriteLn('|', 'ZX', '|');
    WriteLn('|', 'Spectrum', '|');

    Writeln;

    WriteLn('|', 1.0, '|');
    WriteLn('|', 0.0, '|');
    WriteLn('|', -1.0, '|');
    WriteLn('|', 1234.5678, '|');
    WriteLn('|', -1234.5678, '|');

    WriteLn;
  end;

  procedure TestWriteFormat1(Width: Integer);
  begin
    Writeln('Now testing :', Width, ' format...');
    WriteLn;

    WriteLn('|', 0:Width, '|');
    WriteLn('|', 1:Width, '|');
    WriteLn('|', -1:Width, '|');
    WriteLn('|', 1234:Width, '|');
    WriteLn('|', -1234:Width, '|');
    WriteLn('|', 12345:Width, '|');
    WriteLn('|', -12345:Width, '|');

    Writeln;

    WriteLn('|', C:Width, '|');

    Writeln;

    WriteLn('|', False:Width, '|');
    WriteLn('|', True:Width, '|');
    WriteLn('|', Red:Width, '|');
    WriteLn('|', Green:Width, '|');
    WriteLn('|', Blue:Width, '|');

    Writeln;

    WriteLn('|', '':Width, '|');
    WriteLn('|', 'ZX':Width, '|');
    WriteLn('|', 'Spectrum':Width, '|');

    Writeln;

    WriteLn('|', 1.0:Width, '|');
    WriteLn('|', 0.0:Width, '|');
    WriteLn('|', -1.0:Width, '|');
    WriteLn('|', 1234.5678:Width, '|');
    WriteLn('|', -1234.5678:Width, '|');

    WriteLn;
  end;

  procedure TestWriteFormat2(Width: Integer; Decimals: Integer);
  begin
    Writeln('Now testing :', Width, ':', Decimals, ' format...');
    WriteLn;

    WriteLn('|', 1.0:Width:Decimals, '|');
    WriteLn('|', 0.0:Width:Decimals, '|');
    WriteLn('|', -1.0:Width:Decimals, '|');
    WriteLn('|', 1234.5678:Width:Decimals, '|');
    WriteLn('|', -1234.5678:Width:Decimals, '|');

    WriteLn;
  end;

begin
  Writeln('--- TestWriteFormat ---');
  TestWriteDefault;

  TestWriteFormat1(0);
  TestWriteFormat1(5);
  TestWriteFormat1(10);
  TestWriteFormat1(20);

  TestWriteFormat2(0, 0);
  TestWriteFormat2(0, 5);
  TestWriteFormat2(0, 10);
  TestWriteFormat2(5, 5);
  TestWriteFormat2(10, 5);
  TestWriteFormat2(20, 5);
end;

begin
  TestWriteFormat;
end.