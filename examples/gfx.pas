program Gfx;

var
  I: Integer;
  
begin
  for I := 0 to 1000 do
    Plot(Random(128), Random(128));

  Draw(0, 0, 127, 0);
  Draw(127, 0, 127, 127);
  Draw(127, 127, 0, 127);
  Draw(0, 127, 0, 0);

  I := 128;
  while I <= 208 do
  begin
    Draw(I, 128, I + 47, 175);
    Draw(I, 175, I + 47, 128);
    Inc(I, 4);
  end; 

  I := 63;
  while I > 0 do
  begin
    Circle(192, 63, I);
    Dec(I, 16);
  end;
end.
