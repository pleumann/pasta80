(**
 * Conversion of a Python program written by Mike Markowski, which itself is
 * a conversion of the two original BASIC programs by Charles Bachand (Analog
 * Computing, issue 7, 1982) and David Cox (Compute Magazine, issue 10, 1981).
 *
 * As an improvement, this version draws the slices front-to-back and keeps
 * track of the highest pixel drawn per x-value so far. That way it can avoid
 * having to erase existing content. It's also nicer to look at. :)
 *
 * This is a monochrome version targeting the ZX Spectrum ULA screen. It will
 * work on a classic ZX Spectrum if you comment out the calls to SetCpuSpeed.
 *)
program Hat;

const
  { Original values from Commodore PET ad }
  x0              = 320; // Screen res math originally targeted
  y0              = 200; // Radius of hat
  r0              = 144; // Height of hat
  h0              = 56;  // Number of slices, 2x64 total
  n0              = 64;  // Max angle of sinusoidal to revolve, 270 deg

  { Values for ZX Spectrum }
  scrX            = 256; // Screen width in pixels
  scrY            = 176; // Screen height in pixels
  nSlices         = 32;  // Half the number of slices

var
  { Working variables from Python version }
  a0              : real;
  hatHgt, hatRad  : real;
  stagger, xf     : real;
  zSlice, stepZ   : real;
  z               : real;
  xl, x           : integer;
  xt, y           : real;
  x1, y1          : integer;

  { New variables for Pascal conversion }
  zz, szx, szy    : Real;                   // For optimization
  Watermarks      : array[0..255] of Byte;  // Highest pixel drawn per x-value
  Stripes         : Boolean;                // True means draw filled stripes
  Count           : Byte;                   // Stripe counter
  C               : Char;                   // Keyboard input

begin
  SetCpuSpeed(3);
  ClrScr;

  GotoXY(4, 11);
  WriteLn('Do you want stripes (y/n)?');
  repeat
    C := ReadKey;
  until (C = 'y') or (c = 'n');
  Stripes := C = 'y';

  ClrScr;

  FillChar(Watermarks, 256, 176);
  Count := 0;

  { Set values for our size of hat }
  a0      := 3.0 * Pi / 2.0;
  hatHgt  := h0 / y0 * scrY;
  hatRad  := r0 / x0 * scrX;
  stagger := n0 / x0 * scrX;
  xf      := a0 / hatRad;
  stepZ  := 1.0 / nSlices;
  zSlice := 1.0;

  { Slices, front to back }
  while zSlice > -1.0 do
  begin
    z  := zSlice * hatRad;                    // -hatRad to hatRad
    zz := z * z;                              // Common subexpression
    xl := Round(Sqrt(hatRad * hatRad - zz));  // Endpoints of hat, this slice
    szx := stagger * zSlice + scrX / 2.0;     // Common subexpression
    szy := stagger * zSlice - scrY / 2.0;     // Common subexpression

    { Step through points on slice }
    for x := -xl to xl do
    begin
      xt := xf * Sqrt(x * x + zz);            // Dist along hat's sinusoid
      y  := (Sin(xt) + 
            0.4 * Sin(xt * 3.0)) * hatHgt;    // Point on surface of hat

      { Stagger layer for fake 3d }
      x1 := Round(x + szx);
      y1 := Round(scrY - (y - szy));          // Flip vertically

      if y1 < Watermarks[x1] then
      begin
        { Set a pixel  }
        Plot(x1, y1);
        { Fill every other slice }
        if Stripes and Odd(Count) and (Watermarks[x1] < 176) then
          Draw(0, Watermarks[x1] - y1 - 1);
        Watermarks[x1] := y1;
      end;
    end;

    zSlice := zSlice - stepZ;                 // Next slice
    Inc(Count);
  end;

  repeat until KeyPressed;

  SetCpuSpeed(0);
end.
