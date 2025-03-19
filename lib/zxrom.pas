(* ===================================================================== *)
(* === ZX Spectrum 48K run-time library ================================ *)
(* ===================================================================== *)

{$l zxrom.asm}

(* --------------------------------------------------------------------- *)
(* --- Standard screen and keyboard functions -------------------------- *)
(* --------------------------------------------------------------------- *)

const
  ScreenWidth  = 32;
  ScreenHeight = 22;

procedure ClrScr; register;                         external 'zx_clrscr';
procedure GotoXY(X, Y: Integer); register;          external 'zx_gotoxy';

procedure TextColor(Color: Integer); register;      external 'zx_color';
procedure TextBackground(Color: Integer); register; external 'zx_background';

function KeyPressed: Boolean; register;             external 'zx_testkey';
function ReadKey: Char; register;                   external 'zx_readkey';

(* --------------------------------------------------------------------- *)
(* --- Machine-specific functions -------------------------------------- *)
(* --------------------------------------------------------------------- *)

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
 * duration of the tone.
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
 * the note.
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
