program HexDump;

{$u+}

function HexStr(Value: Integer; Digits: Integer): String;
const
  Hex: String = '0123456789ABCDEF';
var
  S: String;
begin
  S := '';

  while (Value <> 0) or (Digits <> 0) do
  begin
    S := Hex[1 + Value mod 16] + S;
    Value := Value div 16;
    Digits := Digits - 1;
  end;

  HexStr := S;
end;

var
  T: Text;
  I: Integer;
  C: Char;
  S: String;
begin
  Assign(T, ParamStr(1));
  Reset(T);

  I := 0;

  Write('00000000');
  S := '';

  repeat
    Read(T, C);
    Write('  $', HexStr(Ord(C), 2));
    if (C >= ' ') and (C <= '~') then
      S := S + C + ' '
    else
      S := S + '. ';
    I := I + 1;

    if I mod 8 = 0 then
    begin
      WriteLn('  ', S);
      S := '';
      Write(HexStr(I, 8));
    end;
  until C = #26;

  while I mod 8 <> 0 do
  begin
    Write('     ');
    Inc(I);
  end;
  
  WriteLn('  ', S);

  Close(T);
end.