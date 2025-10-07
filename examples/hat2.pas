program HatTP;
{ Turbo Pascal 7 / Borland Pascal
  Port des Python/PIL/Numpy-Codes auf BGI-Grafik.
  Zeichnet den "Hut" in grün auf schwarzem Hintergrund. }

const
  { Originalwerte aus der Commodore-PET-Anzeige }
  x0 = 320;
  y0 = 200;
  r0 = 144;
  h0 = 56;
  n0 = 64;

var
  scrX, scrY      : integer;
  nSlices         : integer;
  a0              : real;
  hatHgt, hatRad  : real;
  stagger, xf     : real;
  zSlice, stepZ   : real;
  z               : real;
  xl, x           : integer;
  xt, y           : real;
  x1, y1          : integer;
  zz, szx, szy    : Real;

begin
  SetCpuSpeed(3);
  ClrScr;

  { Bildschirmgröße holen }
  scrX := 256; // GetMaxX + 1;                      { z.B. 640 }
  scrY := 176; // Round(scrX * y0 / x0);            { Seitenverhältnis wie 320x200 }
  //if scrY > GetMaxY + 1 then                { ggf. auf verfügbare Höhe begrenzen }
  //  scrY := GetMaxY + 1;

  { Parameter wie im Python-Code }
  nSlices := 32;                             { kannst du ändern }
  a0      := 3.0 * Pi / 2.0;            { 270° }
  hatHgt  := h0 / y0 * scrY;
  hatRad  := r0 / x0 * scrX;
  stagger := n0 / x0 * scrX;
  xf      := a0 / hatRad;

  stepZ  := 1.0 / nSlices;
  zSlice := -1.0;

  while zSlice < 1.0 do
  begin
    z  := zSlice * hatRad;
    zz := z * z;                                  { -hatRad .. +hatRad }
    xl := Round(Sqrt(hatRad * hatRad - zz));              { Halbbreite je Slice }

    szx := stagger * zSlice + scrX / 2.0;
    szy := stagger * zSlice - scrY / 2.0;

    for x := -xl to xl do
    begin
      { Hut-Oberfläche als Rotationskörper }
      xt := xf * Sqrt(x * x + zz);                        { Strecke entlang der Sinuskurve }
      y  := (Sin(xt) + 0.4 * Sin(xt * 3.0)) * hatHgt;

      { Versatz (stagger) für Fake-3D + ins Screen-Koordinatensystem }
      x1 := Round(x + szx);
      y1 := Round(scrY - (y - szy));  { vertikal flippen }

      if (x1 >= 0) and (x1 < scrX) and (y1 >= 0) and (y1 < scrY) then
      begin
        { Punkt der aktuellen Schicht setzen (grün) }
        Plot(x1, y1);

        { "Vorherige" Schicht darunter ausradieren (schwarz), wie im Python-Code }
        if y1 + 1 < scrY then
        begin
          Mem[$5C91] := 4;
          // Line(x1, y1 + 1, x1, scrY - 1);
          //Plot(x1, 0);
          Draw(0, scrY - y1 - 1);
          Mem[$5C91] := 0;
        end;
      end;
    end;

    zSlice := zSlice + stepZ;    { nächste Scheibe, von hinten nach vorne }
  end;

  repeat until KeyPressed;
end.
