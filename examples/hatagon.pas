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
 * It will not work on a classic ZX Spectrum or the Agon.
 *)
program HatAgon;

{$ifndef SYS_AGON}
  {$error Agon required.}
{$endif}

const
  { Original values from Commodore PET ad }
  x0              = 512;  //320; // Screen res math originally targeted
  y0              = 384;  //200; // Radius of hat
  r0              = 144; // Height of hat
  h0              = 56;  // Number of slices, 2x64 total
  n0              = 64;  // Max angle of sinusoidal to revolve, 270 deg

  { Values for Agon }
  scrX            = 512; // Screen width in pixels
  scrY            = 384; // Screen height in pixels
  nSlices         = 32;  // Half the number of slices

var
  { Working variables from Python version }
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
  Watermarks      : array[0..639] of integer;  // Highest pixel drawn per x-value (scrX-1)
  Stripes         : Boolean;                // True means draw filled stripes
  Count, I        : integer;                   // Stripe and loop counters
  C               : Char;                   // Keyboard input

const
  { Layer 2 indices of bright red/yellow/gree/cyan }
  Colors: array[0..3] of Byte = ($09, $0B , $0A, $0E);
  Color9: array[0..2] of Byte = (255, 0, 0);
  ColorB: array[0..2] of Byte = (255, 255, 0);
  ColorA: array[0..2] of Byte = (0, 255, 0);
  ColorE: array[0..2] of Byte = (0, 255, 255);

procedure SetUpPalette();
var
  I: Byte;
begin
  (** steps are:
  *   Clear all palettes (ID = -1)
  *     Command 1: Delete a palette VDU 23, 0, &C4, 1, <paletteID>;
  *   Create 1 palette // command not required - Command 0: Create a palette VDU 23, 0, &C4, 0, <paletteID>;
  *   Create Palette entries for each one
  *     Command 2: Set a palette entry VDU 23, 0, &C4, 2, <paletteID>; <index>, <red>, <green>, <blue>
  *   put each signal list in a buffer
  *   set current signal list
  **)
  Write(chr(23),chr(0),chr($c4),chr($1),chr(255),chr(255)); //VDU 23, 0, &C4, 1, 65535;

  for I := 0 to 3 do
  begin
        Write(chr(23),chr(0),chr($c4),chr(0),chr(I+1),chr(0)); //VDU 23, 0, &C4, 0, <paletteID>;
        Write(chr(23),chr(0),chr($c4),chr($2),chr(I+1),chr($0),chr(((I + 0) and 3)+1),
            chr(Color9[0]),chr(Color9[1]),chr(Color9[2]));
            //VDU 23, 0, &C4, 2, <paletteID>; <index>, <red>, <green>, <blue>
        Write(chr(23),chr(0),chr($c4),chr($2),chr(I+1),chr($0),chr(((I + 1) and 3)+1),
            chr(ColorB[0]),chr(ColorB[1]),chr(ColorB[2]));
            //VDU 23, 0, &C4, 2, <paletteID>; <index>, <red>, <green>, <blue>
        Write(chr(23),chr(0),chr($c4),chr($2),chr(I+1),chr($0),chr(((I + 2) and 3)+1),
            chr(ColorA[0]),chr(ColorA[1]),chr(ColorA[2]));
            //VDU 23, 0, &C4, 2, <paletteID>; <index>, <red>, <green>, <blue>
        Write(chr(23),chr(0),chr($c4),chr($2),chr(I+1),chr($0),chr(((I + 3) and 3)+1),
            chr(ColorE[0]),chr(ColorE[1]),chr(ColorE[2]));
            //VDU 23, 0, &C4, 2, <paletteID>; <index>, <red>, <green>, <blue>
        //Now do the matching buffer, using 256 + I : Command 2: Clear a buffer - VDU 23, 0 &A0, bufferId; 2
        Write(chr(23),chr(0),chr($a0),chr(i),chr($1),chr(2));
        // Write it to the buffer, all buffers are only 4 bytes long for a single palette.
        // VDU 23, 0, &A0, bufferId; 0, length; <buffer-data>
        Write(chr(23),chr(0),chr($a0),chr(i),chr($1),chr(0),chr(4),chr(0),chr(1),chr(0),chr(i+1),chr(0));
  end;
end;

procedure SelectPalette(var Pal: Byte);
begin
  // Command 3: Set/Update the signal list VDU 23, 0, &C4, 3, <bufferId>;
  Write(chr(23),chr(0),chr($c4),chr(3),chr(Pal),chr(1));
end;

  var
    Pal: Byte;
begin


  SetGraphMode(21);
  Write(chr(23),chr(0),chr($f8),chr($10),chr($3),chr(0),chr(0)); //VDU 23, 0, &F8, &310; 0; - enable copper effects
  SetUpPalette();
  Pal := 0;
  SelectPalette(Pal);

  GotoXY(4, 4);
  Write('Do you want stripes (y/n)?');
  repeat
    C := ReadKey;
  until (C = 'y') or (c = 'n');
  Stripes := C = 'y'; WriteLn(C);

  {FillChar(Watermarks, scrX, scrY);}
  for I := 0 to (scrX-1) do
    Watermarks[I] := scrY;

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

    { Step through points on slice }
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
        SetColor(15); Plot(x1,y1); { L2SetPixel(x1, y1, 0); }
        { Fill every slice }
        if Stripes and (Watermarks[x1] < scrY) then
          begin
            //SetColor(Colors[Count and 3]);
            SetColor((Count and 3)+1);
            for I := y1 + 1 to Watermarks[x1] - 1 do
              Plot(x1,I); { L2SetPixel(x1, I, Colors[Count and 3]); }
          end;
        Watermarks[x1] := y1;
      end;
    end;

    zSlice := zSlice - stepZ;                 // Next slice
    Inc(Count);
  end;

  I := 0;
  repeat
    I := (I+1) and 3;
    SelectPalette(I);
    Delay(200);
  until KeyPressed;
  SetGraphMode(0);
end.
