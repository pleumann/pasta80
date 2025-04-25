program TestCase;

var
  I: Integer;
  S: String[255];

begin
  for I := 0 to 12 do
  begin
    Write(I, ' is ');

    case I of
      0:          WriteLn('nothing');
      2..3, 5, 7: WriteLn('prime');
      4, 9:       WriteLn('square');
      10:         WriteLn('two hands');
      12:         Writeln('a dozen');
    else
      WriteLn('just that');
    end;
  end;

  WriteLn;
  
  S := 'ZX Spectrum 128K+';

  for I := 1 to Length(S) do
  begin
    case S[I] of
      'A'..'Z':   Write(Char(Ord(S[I]) + 32));
      'a'..'z':   Write(Char(Ord(S[I]) - 32));
    else
      Write(S[I]);
    end;
  end;

  WriteLn;
end.