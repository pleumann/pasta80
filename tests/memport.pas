program MemPort;

var
  B: Byte;

begin
  Mem[32768] := 255;
  B := Mem[32768];

  Port[32768] := 255;
  B := Port[32768];
end.