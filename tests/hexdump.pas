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

begin
  Assign(T, ParamStr(1));
  Reset(T);

  I := 0;

  Write('00000000');

  repeat
    Read(T, C);
    Write('   ', HexStr(Ord(C), 2));
    if (C >= ' ') and (C <= '~') then Write(' '#27'p', C, #27'q') else Write('  ');
    I := I + 1;

    if I mod 8 = 0 then
    begin
      WriteLn;
      Write(HexStr(I, 8));
    end;
  until C = #26;

  WriteLn;

  Close(T);
end.