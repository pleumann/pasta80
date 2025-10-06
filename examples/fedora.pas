program Fedora;

var
  XI, ZI, XL, YY, X1, Y1: Integer;
  XP, XR, XF, XT, ZT, ZS: Real;
begin
  SetCpuSpeed(3);

  XP := 144;
  XR := 4.71238905;
  XF := XR / XP;

  for ZI := -64 to 64 do
  begin
    ZT := ZI * 2.25;
    ZS := ZT * ZT;
    XL := Round(Sqrt(20736 - ZS) + 0.5);

    for XI := 0 - XL to XL do
    begin
      XT := Sqrt(XI * XI + ZS) * XF;
      YY := Round((Sin(XT) + Sin(XT * 3) * 0.4) * 56);
      X1 := XI + ZI + 160;
      Y1 := 90 - YY + ZI;
      if (X1 >= 0) and (X1 <= 255) and (Y1 >= 0) and (Y1 <= 175) then
        Plot(X1, Y1)
    end;
  end;
end.