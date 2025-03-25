(* ===================================================================== *)
(* === ZX Spectrum 48K run-time library ================================ *)
(* ===================================================================== *)

{$l zxrom.asm}

(* --------------------------------------------------------------------- *)
(* --- Standard Pascal screen and keyboard functions ------------------- *)
(* --------------------------------------------------------------------- *)

const
  (**
   * Defines the default with of the screen in characters.
   *)
  ScreenWidth  = 32;

  (**
   * Defines the default height of the screen in characters.
   *)
  ScreenHeight = 22;

  (**
   * Line break convention used for this platform.
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
 * Reads a key press and returns the corresponding ASCII character.
 * Waits for a key press, if necessary, so use KeyPressed first if you
 * don't want your program to be delayed.
 *)
function ReadKey: Char; register;                   external 'zx_readkey';

(* --------------------------------------------------------------------- *)
(* --- Machine-specific functions -------------------------------------- *)
(* --------------------------------------------------------------------- *)

const
  (**
   * Controls scrolling behavior. If True the screen will auto-scroll
   * once the bottom row is full. If False the Sinclair-typical "Scroll?"
   * message will be displayed every 22 rows.
   *)
  AutoScroll: Boolean = False;

(**
 * Sets the border color (0..7) using the appropriate ROM routine.
 *)
procedure Border(Color: Integer); register;         external 'zx_border';

(**
 * Waits for the given interval in milliseconds. Note that, since the
 * maskable 50 Hz interrupt is being used as a basis, the actual interval
 * is a multiple of 20 ms plus/minus "a bit", depending on at which point
 * in the interrupt cycle the procedure is invoked. If interupts are
 * disabled, calling this procedure will hang the machine.
 *)
procedure Delay(Duration: Integer); register;       external 'zx_delay';

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
