(**
 * Conversion of a Python program written by Mike Markowski, which itself is
 * a conversion of the two original BASIC programs by Charles Bachand (Analog
 * Computing, issue 7, 1982) and David Cox (Compute Magazine, issue 10, 1981).
 *
 * As an improvement, this version draws the slices front-to-back and keeps
 * track of the highest pixel drawn per x-value so far. That way it can avoid
 * having to erase existing content. It's also nicer to look at. :)
 *
 * This is a 256 color version targeting the ZX Spectrum Next Layer 2 screen.
 * It will not work on a classic ZX Spectrum.
 *)
program Hat256;

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
  Count, I        : Byte;                   // Stripe and loop counters
  C               : Char;                   // Keyboard input

const
  { Layer 2 indices of bright red/yellow/gree/cyan }
  Colors: array[0..3] of Byte = (
    $1F, $1C, $FC, $E0
  );

procedure L2Enable; register; external 'l2_enable';
procedure L2SetPixel(X, Y, Color: Byte); register; external 'l2_set_pixel';
function L2GetPixel(X, Y: Byte): Byte; register; external 'l2_get_pixel';

{$l hat256.asm}

function GetPalette(I: Byte): Byte;
begin
  Port[$243b] := $43;
  Port[$253b] := %10010000;

  Port[$243b] := $40;
  Port[$253b] := I;

  Port[$243b] := $41;
  GetPalette := Port[$253b];
end;

procedure SetPalette(I, B: Byte);
begin
  Port[$243b] := $43;
  Port[$253b] := %10010000;

  Port[$243b] := $40;
  Port[$253b] := I;

  Port[$243b] := $41;
  Port[$253b] := B;
end;

begin
  SetCpuSpeed(3);
  ClrScr;

  GotoXY(4, 11);
  WriteLn('Do you want stripes (y/n)?');
  repeat
    C := ReadKey;
  until (C = 'y') or (c = 'n');
  Stripes := C = 'y';

  { ClrScr; }
  L2Enable;
  TextBackground($B6);
  for I := 0 to 191 do
    Write('    ');

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
        L2SetPixel(x1, y1, 0);
        { Fill every slice }
        if Stripes and (Watermarks[x1] < 176) then
          for I := y1 + 1 to Watermarks[x1] - 1 do
            L2SetPixel(x1, I, Colors[Count and 3]);

        Watermarks[x1] := y1;
      end;
    end;

    zSlice := zSlice - stepZ;                 // Next slice
    Inc(Count);
  end;

  repeat
    I := GetPalette(Colors[3]);
    SetPalette(Colors[3], GetPalette(Colors[2]));
    SetPalette(Colors[2], GetPalette(Colors[1]));
    SetPalette(Colors[1], GetPalette(Colors[0]));
    SetPalette(Colors[0], I);

    Delay(200);
  until KeyPressed;

  SetCpuSpeed(0);
end.
