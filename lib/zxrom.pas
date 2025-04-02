(* ===================================================================== *)
(* === ZX Spectrum 48K run-time library ================================ *)
(* ===================================================================== *)

{$l zxrom.asm}

(* --------------------------------------------------------------------- *)
(* --- Screen and keyboard standard functions -------------------------- *)
(* --------------------------------------------------------------------- *)

const
  (**
   * Defines the default with of the Spectrum screen in characters.
   *)
  ScreenWidth  = 32;

  (**
   * Defines the default height of the Spectrum screen in characters.
   *)
  ScreenHeight = 22;

  (**
   * Defines the line break convention used by the Spectrum.
   *)
  LineBreak = #13;

(**
 * Clears the screen. Uses the most recently defined text color and
 * background for the attribute area.
 *)
procedure ClrScr; register;                         external 'zx_clrscr';

(**
 * Moves the the cursor (aka printing position) to a given location. Note
 * that, unlike ZX BASIC, Pascal expects the screen column (X) first,
 * followed by the screen row (Y).
 *)
procedure GotoXY(X, Y: Integer); register;          external 'zx_gotoxy';

(**
 * Sets the text color (0..7).
 *)
procedure TextColor(Color: Integer); register;      external 'zx_color';

(**
 * Sets the text background (0..7).
 *)
procedure TextBackground(Color: Integer); register; external 'zx_background';

(**
 * Returns True if a key has been pressed (and can be queried using the
 * ReadKey function), False if not.
 *)
function KeyPressed: Boolean; register;             external 'zx_testkey';

(**
 * Reads a key press and returns the corresponding ASCII character. Does
 * echo the character to the screen. Waits for a key press, if necessary,
 * so use KeyPressed first if you don't want your program to be delayed.
 *)
function ReadKey: Char; register;                   external 'zx_readkey';

(* --------------------------------------------------------------------- *)
(* --- Misc. standard functions ---------------------------------------- *)
(* --------------------------------------------------------------------- *)

(**
 * Waits for the given interval in milliseconds. Note that, since the
 * maskable 50 Hz interrupt is being used as a basis, the actual interval
 * is a multiple of 20 ms plus/minus "a bit", depending on at which point
 * in the interrupt cycle the procedure is invoked. If interupts are
 * disabled, calling this procedure will hang the machine.
 *)
procedure Delay(Duration: Integer); register;       external 'zx_delay';

(* --------------------------------------------------------------------- *)
(* --- Machine-specific functions -------------------------------------- *)
(* --------------------------------------------------------------------- *)

(**
 * Sets the border color (0..7) using the appropriate ROM routine.
 *)
procedure Border(Color: Integer); register;         external 'zx_border';

(**
 * Plays a tone. Frequency is in Hz. Duration is in milliseconds. The
 * Spectrum does not have dedicated sound hardware, so the speaker has to
 * be turned on and off with precise timing by a ROM routine. As a result,
 * the procedure is synchronous, i.e. the program has to wait for the whole
 * duration of the tone. When calling this on a ZX Spectrum Next in the
 * native personality, set the CPU Speed to 3.5 MHz first. Otherwise both
 * the frequency and the duration will be wrong.
 *)
procedure Sound(Frequency, Duration: Integer);
var
  Cycles: Real;

  procedure RomBeep(HL, DE: Integer); register;     external '$03b5';

begin
  Cycles := 3500000.0 / (2 * Frequency);
  RomBeep(Trunc(Cycles / 4 - 30.125), Trunc(Duration * Frequency));
end;

(**
 * Plays a note. Duration is in seconds. Pitch is the key number on a
 * piano, similar to MIDI, but with 0 being the middle C. See the BEEP
 * command in the ZX Spectrum BASIC manual for details. The Spectrum does
 * not have dedicated sound hardware, so the speaker has to be turned on
 * and off with precise timing by a ROM routine. As a result, the procedure
 * is synchronous, i.e. the program has to wait for the whole duration of
 * the note. When calling this on a ZX Spectrum Next in the native
 * personality, set the CPU Speed to 3.5 MHz first. Otherwise both the
 * frequency and the duration will be wrong.
 *)
procedure Beep(Duration: Real; Pitch: Integer);
const
  LN2 = 0.693147180559945;
var
  Frequency, Cycles: Real;
  DE, HL: Integer;

  procedure RomBeep(HL, DE: Integer); register;     external '$03b5';

begin
  Frequency := 440.0 * Exp(((Pitch - 9) / 12.0) * LN2);
  Cycles := 3500000.0 / (2 * Frequency);
  RomBeep(Trunc(Cycles / 4 - 30.125), Trunc(Duration * Frequency));
end;

(**
 * Plots a point at the given coordinates.
 *)
procedure Plot(X, Y: Integer);
  procedure AsmPlot(XY: Integer); register;         external 'zx_plot';
begin
  AsmPlot(Lo(X) or Lo(Y) shl 8);
end;

(**
 * Draws a line from the first coordinate pair to the second.
 *)
procedure Draw(X1, Y1, X2, Y2: Integer);
var
  DX, DY, AX, AY, SX, SY: Integer;

  procedure AsmDraw(DXY, Sgn: Integer); register;   external 'zx_draw';

begin
  Plot(X1, Y1);

  DX := X2 - X1;
  DY := Y2 - Y1;

  AX := Abs(DX);
  AY := Abs(DY);

  if DX >= 0 then SX := 1 else SX := 255;
  if DY >= 0 then SY := 1 else SY := 255;

  AsmDraw(AX or AY shl 8, SX or SY shl 8);
end;

(**
 * Draws a circle of the given radius around the given center point.
 *)
procedure Circle(CX, CY, Radius: Integer);
var
  X, Y, D: Integer;
begin
  X := 0;
  Y := Radius;
  D := 3 - 2 * Radius;

  while X <= Y do
  begin
    Plot(CX + X, CY + Y);
    Plot(CX - X, CY + Y);
    Plot(CX + X, CY - Y);
    Plot(CX - X, CY - Y);
    Plot(CX + Y, CY + X);
    Plot(CX - Y, CY + X);
    Plot(CX + Y, CY - X);
    Plot(CX - Y, CY - X);

    if D < 0 then
      D := D + 4 * X + 6
    else
    begin
      D := D + 4 * (X - Y) + 10;
      Dec(Y);
    end;

    Inc(X);
  end;
end;