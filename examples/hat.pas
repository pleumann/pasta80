program Hat;

const
  P = 127;
  Q = 88;

var
  XF, XP, XR, XT, YP, YR, ZP, YF, ZF, ZS, ZT, XX, YY, ZZ: Real;
  XI, XL, ZI, X1, Y1: Integer;

begin
  SetCpuSpeed(3);

  XP := 144;
  XR := 1.5 * Pi;

  YP := 56;
  YR := 1;
  ZP := 64;

  XF := XR / XP;
  YF := YP / YR;
  ZF := XR / ZP;

  for ZI := -Q to Q - 1 do
  begin
    if (ZI < -ZP) or (ZI > ZP) then Continue;

    ZT := ZI * XP / ZP;
    ZZ := ZI;

    ZS := ZT * ZT;
    XL := Round(0.5 + Sqrt(XP * XP - ZS));

    for XI := -XL to XL do
    begin
      XT := Sqrt(XI * XI + ZS) * XF;
      XX := XI;
      YY := (Sin(XT) + 0.4 * Sin(3 * XT)) * YF;

      X1 := Round(0.5 + XX + ZZ + P);
      Y1 := Round(0.5 + YY - ZZ + Q);

      Plot(X1, Y1);
      // if Y1 <> 0 then
    end;
  end;
end.

{
10 VISMEM: CLEAR
20 P=160: Q=100
30 XP=144: XR=1. 5*3. 1415927
40 YP=56: YR=1: ZP=64
50 XF=XR/XP: YF=YP/YR: ZF=XR/ZP
60 FOR ZI=-Q TO Q-1
70 IF ZK-ZP OR ZI>ZP GOTO 150
80 ZT=ZI*XP/ZP: ZZ=ZI
90 XL=INT( .5+SQR(XP*XP-ZT*ZT) )
100 FOR XI=-XL TO XL
110 XT=SQR{XI*XI+ZT*ZT)*XF: XX=XI|
120 YY= (SIN(XT)+.4*SIN{3*XT) ) *YF
130 GOSUB 170
140 NEXT XI
150 NEXT ZI
160 STOP
170 X1=XX+ZZ+P
180 Y1=YY-ZZ+Q
190 GMODE 1: MOVE XI, Yl: WRPIX
200 IF Y1=0 GOTO 220
210 GMODE 2: LINE Xl,Yl-l,Xl,0
220 RETURN
}