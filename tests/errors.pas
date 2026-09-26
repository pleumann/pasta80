program Errors;

(* The negative tests, all of them, in one program.

   Every case here is a program that must *not* reach its end: it provokes a
   runtime error, and the run-time library has to stop the program on the
   spot. That is why these cannot be assertions inside one of the positive
   suites -- an assertion needs a program that is still running to report to.

   Because the program dies, exactly one case runs per start. Which one is
   picked by number:

     CP/M, Agon   the number is the first command line parameter:
                    errors 3

     ZX           there is no command line, so nothing is passed in at all:
                  the program counts for itself. One call runs one case, the
                  next call the one after it, and it reports through the exit
                  code whether another one is coming -- zero while cases
                  remain, -1 when they are all done. USR yields that (it is
                  what __done leaves in BC), so the loader needs no count of
                  its own and never has to be touched when a case is added:

                    10 CLEAR 32767: LOAD "" CODE 32768
                    20 IF USR 32768<>65535 THEN GO TO 20

                  One tape load for all of them, because __done
                  (rtl/zxrom.asm) restores the stack and returns to BASIC
                  even when it was reached through a runtime error, so the
                  NEXT simply runs. The counter survives in between for the
                  same reason the heap does: it is an ordinary global, which
                  means static data in the image, and main does not
                  initialise it -- see NextCase below. It starts over on the
                  next LOAD, not between calls, so an interrupted sweep
                  continues rather than restarting.

   That counting only happens in a printer build, where the run is
   unattended and the screen is not being watched: there, a menu would just
   be noise in the log. Built for the screen, the ZX asks the same way
   everything else does, so a single case can still be picked by hand.

   Given no number, or one out of range, the program shows that menu -- on
   a screen build. In a printer build there is nobody to answer it, so the
   same situation means the opposite: nothing left to do. The program says
   so and then stops the emulator itself (QuitHost), which is how an
   unattended run ends on every target. On Agon that is the last line of
   autoexec.txt calling it with a number past the end; on the ZX it happens
   by itself once the counter runs off the end.

   Every case announces itself before it runs:

     --- Case 1: divzero
     Expected output: Division by zero
     Division by zero

   So the rule for checking a log is the same for every case: the last
   non-empty line of a case must be what its "Expected output:" line
   promised.
   Nothing else needs to be known about the case. A line that must not be
   printed ("after: ...") would be the last one if it ever appeared, and so
   would anything a Halt failed to prevent.

   Two cases depend on how the program was built rather than on the target
   alone, and both say so in their own output instead of quietly proving
   nothing:

     overnest    only means anything in an overlay build. Without --ovr the
                 "overlay" markers are inert, the nested calls are ordinary
                 calls, and there is no limit to run into. OPT_OVERLAYS
                 (set by the compiler when --ovr is given) decides which of
                 the two the case is. CP/M has no overlays at all, so it is
                 skipped there for good.

     rdint..iopend
                 need a file system, which the plain Spectrums do not have.

   Case 33 (heapfull) has to stay the LAST one. It is the only case that
   leaves the heap behind in a state the next case would inherit: on the ZX
   all cases share one load, and __heapptr (rtl/system.asm) is static data
   in the image rather than something __init resets, so an exhausted heap
   stays exhausted until the tape is loaded again. Anything added later goes
   in front of it. *)

{ The five cases that read from a text file need a file system. The plain
  Spectrums have none, so there they report themselves as skipped rather
  than quietly proving nothing. }

{ FUSE marks the two targets whose emulator is Fuse, which cannot be stopped
  by an instruction the way CSpect and the Agon emulator can -- it needs the
  breakpoint from misc/quitfuse.brk, and something to trip it. }

{$ifdef SYS_ZX48}
  {$define NOFILES}
  {$define FUSE}
{$endif}
{$ifdef SYS_ZX128}
  {$define NOFILES}
  {$define FUSE}
{$endif}

{ The unattended path: a ZX with its output on the printer, which is how the
  whole suite is run. No command line to take a number from and no point in
  asking for one, so the program walks the cases by itself. See the header. }

{$ifdef SYS_ZX}
  {$ifdef OPT_PRINTER}
    {$define SWEEP}
  {$endif}
{$endif}

const
  CaseCount = 33;
  HaltCode  = 42;

  { Indexed by the same number as the dispatcher in Run, and grouped the same
    way, so that adding a case means touching two lists that sit next to each
    other rather than a name buried in a third place. }
  CaseName: array[1..CaseCount] of String[8] = (
    'divzero',  'modzero',  'rdivzero', 'tanpi2',
    'raddovf',  'rsubovf',  'rmulovf',  'rdivovf',
    'truncovf', 'roundovf', 'lnzero',   'lnneg',    'logzero', 'sqrtneg',
    'valint',   'valreal',  'valenum',
    'rdint',    'rdreal',   'rdenum',   'rdempty',  'iopend',
    'strcpy',   'strcpyhi', 'strdel',   'strdelhi', 'strins',  'strinshi',
    'stack',
    'halt',     'halt42',
    'overnest',
    'heapfull'
  );

type
  Color = (Red, Green, Blue);

  { One allocation unit for the heapfull case. Big enough that even a large
    heap is used up in a sensible number of steps. }
  PChunk = ^TChunk;
  TChunk = array[0..255] of Byte;

var
  TestNo: Integer;

{$ifdef SWEEP}
  { The case to run on the next call. A plain global on purpose: globals are
    static data in the image (the generated assembly declares this one as
    "ds 2,0"), and main starts straight into the program without
    initialising them, so this keeps its value from one RANDOMIZE USR to the
    next. Zero after loading, hence the Inc before it is used. }
  NextCase: Integer;
{$endif}

(* -------------------------------------------------------------------------- *)
(* --- Reporting ------------------------------------------------------------ *)
(* -------------------------------------------------------------------------- *)

(**
 * Prints the header of a case: its number, its name, and the one line the
 * log has to end with if the case did what it promises.
 *)
procedure Announce(Wants: String);
begin
  WriteLn;
  WriteLn('--- Case ', TestNo, ': ', CaseName[TestNo]);
  WriteLn('Expected output: ', Wants);
end;

(**
 * For cases that cannot run on this target. The expectation and the output
 * are the same line, so the "last line must match" rule holds here as well.
 *)
procedure Skipped;
begin
  Announce('skipped on this target');
  WriteLn('skipped on this target');
end;

(* -------------------------------------------------------------------------- *)
(* --- Division by zero ----------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

(* Integer "div" by zero must abort, not silently return 0 (OPEN-ITEMS B3). *)
overlay procedure CaseDivZero;
var
  A, B, C: Integer;
begin
  Announce('Division by zero');

  A := 7;
  B := 0;
  C := A div B;

  WriteLn('after: ', C);
end;

(* Same for "mod", which goes through the same checked entry point. *)
overlay procedure CaseModZero;
var
  A, B, C: Integer;
begin
  Announce('Division by zero');

  A := 7;
  B := 0;
  C := A mod B;

  WriteLn('after: ', C);
end;

(* Real division by zero must abort as well. The zero divisor is what
   __fpdiv's Carry+Zero check should recognise as "Division by zero" rather
   than as a generic "Real overflow". *)
overlay procedure CaseRDivZero;
var
  R, Zero: Real;
begin
  Announce('Division by zero');

  Zero := 0.0;
  R := 7.0 / Zero;

  WriteLn('after: ', R:0:3);
end;

(* Tan(Pi/2) reaches the same place through the back door: TAN computes
   SIN(X)/COS(X) with a plain, unwrapped FPDIV, and Cos(Pi/2) rounds to
   exactly 0 in this Real representation. *)
overlay procedure CaseTanPi2;
var
  R: Real;
begin
  Announce('Division by zero');

  R := Tan(Pi / 2.0);

  WriteLn('after: ', R:0:3);
end;

(* -------------------------------------------------------------------------- *)
(* --- Real overflow -------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

(* Addition, isolated from multiplication so that it exercises __fpadd's own
   check. Doubling 1.0 crosses the ~1.7e38 range on the 127th addition. *)
overlay procedure CaseRAddOvf;
var
  R: Real;
  I: Integer;
begin
  Announce('Real overflow');

  R := 1.0;
  for I := 1 to 127 do R := R + R;

  WriteLn('after: ', R);
end;

(* Subtraction is FPNEG+FPADD internally, but this goes through the "-"
   operator (__fpsub) rather than assuming it behaves like "+". Huge is
   doubled to just below the limit, then Huge - (-Huge) crosses it. *)
overlay procedure CaseRSubOvf;
var
  Huge, R: Real;
  I: Integer;
begin
  Announce('Real overflow');

  Huge := 1.0;
  for I := 1 to 126 do Huge := Huge + Huge;
  R := Huge - (-Huge);

  WriteLn('after: ', R);
end;

(* Multiplication must abort instead of producing garbage. *)
overlay procedure CaseRMulOvf;
var
  R: Real;
begin
  Announce('Real overflow');

  R := 1e19;
  R := R * R;              { 1e38, past the ~1.7e38 exponent range }

  WriteLn('after: ', R);
end;

(* Division that overflows with a NON-zero divisor must be reported as
   "Real overflow", not "Division by zero" -- this is the case __fpdiv's
   Carry+Zero distinction exists for. *)
overlay procedure CaseRDivOvf;
var
  R, Small: Real;
begin
  Announce('Real overflow');

  R := 1.0;
  R := R + R;              { 2.0 }
  Small := 1e-30;
  R := R / Small;
  R := R / Small;
  R := R / Small;

  WriteLn('after: ', R);
end;

(* -------------------------------------------------------------------------- *)
(* --- Invalid floating point operation ------------------------------------- *)
(* -------------------------------------------------------------------------- *)

(* Trunc must abort if the result does not fit into the 16-bit signed Integer
   range, the way TP3/TP5 do. FIX (math48.asm) carries a reliable Carry flag
   for this; __trunc wraps it and reports it as TP5 does. *)
overlay procedure CaseTruncOvf;
var
  R: Real;
  I: Integer;
begin
  Announce('Invalid floating point operation');

  R := 1.0e10;
  I := Trunc(R);

  WriteLn('after: ', I);
end;

(* Same thing via Round, which adds or subtracts 0.5 through the checked
   __fpadd/__fpsub before tail-jumping into __trunc. *)
overlay procedure CaseRoundOvf;
var
  R: Real;
  I: Integer;
begin
  Announce('Invalid floating point operation');

  R := -1.0e10;
  I := Round(R);

  WriteLn('after: ', I);
end;

(* Ln(0) must abort, not silently return 0.0. LN (math48.asm) flags this via
   Carry; __ln is the wrapper that acts on it. *)
overlay procedure CaseLnZero;
var
  R, Zero: Real;
begin
  Announce('Invalid floating point operation');

  Zero := 0.0;
  R := Ln(Zero);

  WriteLn('after: ', R:0:3);
end;

(* The other branch of the same Carry logic. *)
overlay procedure CaseLnNeg;
var
  R: Real;
begin
  Announce('Invalid floating point operation');

  R := Ln(-5.0);

  WriteLn('after: ', R:0:3);
end;

(* Log calls LN internally and passes its Carry through with "ret c". This
   confirms __log inherits that instead of swallowing it. *)
overlay procedure CaseLogZero;
var
  R, Zero: Real;
begin
  Announce('Invalid floating point operation');

  Zero := 0.0;
  R := Log(Zero);

  WriteLn('after: ', R:0:3);
end;

(* Sqrt of a negative number must abort rather than return the argument
   unchanged. *)
overlay procedure CaseSqrtNeg;
var
  R: Real;
begin
  Announce('Invalid floating point operation');

  R := Sqrt(-1.0);

  WriteLn('after: ', R:0:3);
end;

(* -------------------------------------------------------------------------- *)
(* --- Format error --------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

(* Val without an error variable has nowhere to report to, so it stops the
   program. Unlike Read it is not an I/O operation -- in Turbo Pascal Val
   never touches IOResult, it carries its own Code parameter -- so this abort
   sits outside the i-minus/i-plus machinery and cannot be suppressed
   (OPEN-ITEMS B9). The three-argument form is exercised in core.pas, where
   it reports through E and the program carries on.

   Integer target, so this goes through __conv_int. *)
overlay procedure CaseValInt;
var
  V: Integer;
begin
  Announce('Format error');

  Val('abc', V);

  WriteLn('after: ', V);
end;

(* Real target: __conv_real. *)
overlay procedure CaseValReal;
var
  V: Real;
begin
  Announce('Format error');

  Val('xyz', V);

  WriteLn('after: ', V:0:3);
end;

(* Enumeration target: __conv_enum. *)
overlay procedure CaseValEnum;
var
  V: Color;
begin
  Announce('Format error');

  Val('nope', V);

  WriteLn('after');
end;

(* -------------------------------------------------------------------------- *)
(* --- I/O error ------------------------------------------------------------ *)
(* -------------------------------------------------------------------------- *)

(* Reading a value that is not a valid number is an I/O error, reported the
   way a disk error is: LastError is set, i-minus can catch it through
   IOResult, and under the default i-plus the compiler-generated check stops
   the program (OPEN-ITEMS B9). Verified against TP 3.0, where i-minus covers
   this case too. 255 is the code because it collides with nothing BDOS,
   esxDOS or MOS report themselves.

   The well-behaved counterpart -- reading IOResult and carrying on -- is
   TestTextReadIOResult in tests/files.pas. *)
overlay procedure CaseRdInt;
{$ifdef NOFILES}
begin
  Skipped;
end;
{$else}
var
  F: Text;
  I: Integer;
begin
  Announce('I/O error 255');

  Assign(F, 'RDINT.TMP');
  Rewrite(F);
  WriteLn(F, 'abc');
  Close(F);

  Reset(F);
  Read(F, I);

  WriteLn('after: ', I);
end;
{$endif}

(* A word that is not a Real. *)
overlay procedure CaseRdReal;
{$ifdef NOFILES}
begin
  Skipped;
end;
{$else}
var
  F: Text;
  V: Real;
begin
  Announce('I/O error 255');

  Assign(F, 'RDREAL.TMP');
  Rewrite(F);
  WriteLn(F, 'xyz');
  Close(F);

  Reset(F);
  Read(F, V);

  WriteLn('after: ', V:0:3);
end;
{$endif}

(* A word that names no enumeration constant. *)
overlay procedure CaseRdEnum;
{$ifdef NOFILES}
begin
  Skipped;
end;
{$else}
var
  F: Text;
  V: Color;
begin
  Announce('I/O error 255');

  Assign(F, 'RDENUM.TMP');
  Rewrite(F);
  WriteLn(F, 'nope');
  Close(F);

  Reset(F);
  Read(F, V);

  WriteLn('after');
end;
{$endif}

(* Nothing at all -- an empty line. Deliberately different from TP 3.0, which
   ignores empty input and leaves the variable as it was. Treating it as
   invalid is why a data file with a trailing blank line now stops a read
   loop. *)
overlay procedure CaseRdEmpty;
{$ifdef NOFILES}
begin
  Skipped;
end;
{$else}
var
  F: Text;
  V: Integer;
begin
  Announce('I/O error 255');

  Assign(F, 'RDEMPTY.TMP');
  Rewrite(F);
  WriteLn(F, '');
  Close(F);

  Reset(F);
  Read(F, V);

  WriteLn('after: ', V);
end;
{$endif}

(* An I/O error raised while checks are off does not evaporate when they are
   turned back on: if nobody collected it with IOResult, switching back to
   i-plus stops the program. Verified against TP 3.0, which does the same.
   The compiler emits the check at the directive itself, and only on a real
   transition, so it costs three bytes per directive rather than three per
   call site. *)
overlay procedure CaseIoPend;
{$ifdef NOFILES}
begin
  Skipped;
end;
{$else}
var
  F: Text;
  I: Integer;
begin
  Announce('I/O error 255');

  Assign(F, 'IOPEND.TMP');
  Rewrite(F);
  WriteLn(F, 'abc');
  Close(F);

  Reset(F);

  {$i-}
  Read(F, I);
  {$i+}

  WriteLn('after: ', I);
end;
{$endif}

(* -------------------------------------------------------------------------- *)
(* --- Invalid string index ------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

(* Copy with Start=0 must abort -- TP 3.0 raises a runtime error here
   (OPEN-ITEMS D1). __strcpy carried a Carry-based signal for this
   internally, but that flag is shared with harmless clamping cases further
   down, so __copy checks Start directly before calling __strcpy at all. *)
overlay procedure CaseStrCpy;
var
  S: String[20];
  Start: Integer;
begin
  Announce('Invalid string index');

  Start := 0;
  S := Copy('Hello', Start, 3);

  WriteLn('after: ', S);
end;

(* A Start above 255 must abort too. Start is an Integer, but only 1..255 can
   address a character, and the wrappers used to look at its low byte alone --
   which silently turned Copy(S, 300, 3) into Copy(S, 44, 3). __idxchk now
   sees the high byte as well. *)
overlay procedure CaseStrCpyHi;
var
  S: String[20];
  Start: Integer;
begin
  Announce('Invalid string index');

  Start := 300;
  S := Copy('Hello', Start, 3);

  WriteLn('after: ', S);
end;

(* Delete with Start=0: same story, but through __delete/__strdel. *)
overlay procedure CaseStrDel;
var
  S: String[20];
  Start: Integer;
begin
  Announce('Invalid string index');

  S := 'ABC';
  Start := 0;
  Delete(S, Start, 3);

  WriteLn('after: ', S);
end;

(* Delete with a Start above 255, which picks Start off the stack
   differently than Copy does. *)
overlay procedure CaseStrDelHi;
var
  S: String[20];
  Start: Integer;
begin
  Announce('Invalid string index');

  S := 'ABC';
  Start := 300;
  Delete(S, Start, 3);

  WriteLn('after: ', S);
end;

(* Insert with Start=0: __insert/__strins (OPEN-ITEMS D1/D2). *)
overlay procedure CaseStrIns;
var
  T: String[20];
  Start: Integer;
begin
  Announce('Invalid string index');

  T := 'Hello';
  Start := 0;
  Insert('XY', T, Start);

  WriteLn('after: ', T);
end;

(* Insert with a negative Start -- the same check as CaseStrCpyHi from the
   other end: -1 arrives as $FFFF and would pass for 255 if only the low byte
   were looked at, quietly appending instead of failing. *)
overlay procedure CaseStrInsHi;
var
  T: String[20];
  Start: Integer;
begin
  Announce('Invalid string index');

  T := 'Hello';
  Start := -1;
  Insert('XY', T, Start);

  WriteLn('after: ', T);
end;

(* -------------------------------------------------------------------------- *)
(* --- Stack overflow ------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

(* Unbounded recursion must be caught by the stack check rather than running
   into the heap (OPEN-ITEMS B18). Only the recursion itself is compiled with
   k-plus; everything else in this program keeps the default so that no other
   case pays for it. *)

{$k+}
procedure Recurse(I: Integer);
begin
  Recurse(I + 1);
end;
{$k-}

overlay procedure CaseStack;
begin
  Announce('Stack overflow');

  Recurse(0);

  WriteLn('after');
end;

(* -------------------------------------------------------------------------- *)
(* --- Halt ----------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

(* Halt stops the program where it stands. It does not return to the caller,
   so neither the statements after it nor the rest of the call chain it was
   reached through ever run -- which is why this halts from three levels down
   rather than from the case procedure itself.

   Both forms are tested, because they are two different pieces of generated
   code rather than one with a default: Halt(N) evaluates N and stores it,
   plain Halt stores a literal zero, and only then do the two meet at "jp
   __done" (pasta80.pas HaltProc). Stopping is all they have in common -- an
   exit code that never gets written would go unnoticed if only the form
   carrying one were tried.

   The code itself is the part that cannot be checked from the inside: by the
   time it exists, the program is gone. Where to look for it differs per
   target, hence the note each case prints. *)

var
  WithCode: Boolean;

procedure Innermost;
begin
  //WriteLn('Three levels down, about to halt.');

  if WithCode then
  begin
    //WriteLn('Halting with exit code ', HaltCode, '...');
    Halt(HaltCode);
  end
  else
  begin
    //WriteLn('Halting without an exit code...');
    Halt;
  end;

  WriteLn('You should not see this (Innermost, after Halt).');
end;

procedure Middle;
begin
  Innermost;
  WriteLn('You should not see this (Middle, after Innermost).');
end;

procedure Outer;
begin
  Middle;
  WriteLn('You should not see this (Outer, after Middle).');
end;

(**
 * Prints where the exit code of this target can be looked at, since the
 * program cannot look at it itself.
 *)
procedure WhereToLook;
begin
  {$ifdef SYS_CPM}
    //WriteLn('On CP/M there is nothing to check from outside: CP/M 2.2');
    //WriteLn('has no return code, so __done warm-boots via RST 0 and the');
    //WriteLn('exit code is dropped. Only the missing output proves it.');
  {$endif}

  {$ifdef SYS_ZX}
    //WriteLn('On the ZX the exit code comes back in BC, which is what the');
    //WriteLn('BASIC function USR yields. PRINT USR 32768 shows it -- the');
    //WriteLn('generated loader says RANDOMIZE USR, which discards it.');
  {$endif}

  {$ifdef SYS_AGON}
    //WriteLn('On Agon the exit code is handed to MOS as the process return');
    //WriteLn('code, so the shell you started this from is where to look.');
  {$endif}
end;

(* Halt without a parameter. The exit code is a literal zero here, so a
   target that reports one has to report 0 rather than whatever happened to
   be in __exitcode from before. *)
overlay procedure CaseHalt;
begin
  Announce(''); //Halting without an exit code...');

  WhereToLook;
  WithCode := False;
  Outer;

  WriteLn('You should not see this (CaseHalt, after Outer).');
end;

(* Halt with a parameter. *)
overlay procedure CaseHalt42;
begin
  Announce(''); //Halting with exit code 42...');

  WhereToLook;
  WithCode := True;
  Outer;

  WriteLn('You should not see this (CaseHalt42, after Outer).');
end;

(* -------------------------------------------------------------------------- *)
(* --- Too many nested overlay calls ---------------------------------------- *)
(* -------------------------------------------------------------------------- *)

(* Overlay calls may nest, but only so far: the overlay stack in
   rtl/overlays.asm holds 16 levels, and the seventeenth has to be refused
   rather than quietly overwriting the sixteenth. A and B call each other
   through Helper, so every step down is a switch to the other overlay and
   costs one level; the counter runs to 20 so that the limit is reached well
   before the recursion would end on its own.

   The constants between the two overlay procedures are not decoration: they
   end one overlay and start the next. Without them A and B share a segment,
   no switch happens, and the case proves nothing. *)

{$ifdef OPT_OVERLAYS}

var
  Depth: Integer;

procedure NestHelper; forward;

overlay procedure OverlayA;
begin
  Inc(Depth);
  //WriteLn('A depth=', Depth);
  if Depth < 20 then NestHelper;
end;

const Dummy1 = 0;

overlay procedure OverlayB;
begin
  Inc(Depth);
  //WriteLn('B depth=', Depth);
  if Depth < 20 then OverlayA;
end;

const Dummy2 = 0;

procedure NestHelper;
begin
  OverlayB;
end;

{$endif}

procedure CaseOverNest;
{$ifdef OPT_OVERLAYS}
begin
  Announce('Too many nested overlay calls');

  Depth := 0;
  OverlayA;

  WriteLn('after, max depth reached=', Depth);
end;
{$else}
begin
  { Built without --ovr, so the markers are inert and there is no nesting
    limit to reach. Nothing to prove here. }
  Skipped;
end;
{$endif}

(* -------------------------------------------------------------------------- *)
(* --- Out of memory -------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

(* A heap request that cannot be served must stop the program, not hand back
   a nil pointer the program would then write through. __malloc walks the
   free list, finds nothing big enough, and reports it (rtl/system.asm
   __malloc_out_of_memory).

   This case has to stay last; see the header. *)
overlay procedure CaseHeapFull;
var
  P: PChunk;
  N: Integer;
begin
  Announce('Out of memory');

  N := 0;
  while True do
  begin
    New(P);
    Inc(N);
  end;

  WriteLn('after: ', N, ' blocks');
end;

(* -------------------------------------------------------------------------- *)
(* --- Dispatch ------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

procedure Run;
begin
  case TestNo of
     1: CaseDivZero;
     2: CaseModZero;
     3: CaseRDivZero;
     4: CaseTanPi2;
     5: CaseRAddOvf;
     6: CaseRSubOvf;
     7: CaseRMulOvf;
     8: CaseRDivOvf;
     9: CaseTruncOvf;
    10: CaseRoundOvf;
    11: CaseLnZero;
    12: CaseLnNeg;
    13: CaseLogZero;
    14: CaseSqrtNeg;
    15: CaseValInt;
    16: CaseValReal;
    17: CaseValEnum;
    18: CaseRdInt;
    19: CaseRdReal;
    20: CaseRdEnum;
    21: CaseRdEmpty;
    22: CaseIoPend;
    23: CaseStrCpy;
    24: CaseStrCpyHi;
    25: CaseStrDel;
    26: CaseStrDelHi;
    27: CaseStrIns;
    28: CaseStrInsHi;
    29: CaseStack;
    30: CaseHalt;
    31: CaseHalt42;
    32: CaseOverNest;
    33: CaseHeapFull;
  end;
end;

(**
 * Asks for a case number. Only reached when none was supplied, so this is
 * the by-hand path; the batch path never gets here.
 *)
procedure Ask;
var
  S: String[10];
  I, E: Integer;
begin
  WriteLn;
  WriteLn('--- Negative tests ---');
  WriteLn;

  for I := 1 to CaseCount do
  begin
    Write(I:3, ' ', CaseName[I]);
    if (I mod 2) = 0 then
      WriteLn
    else
      Write('':10 - Length(CaseName[I]));
  end;
  WriteLn;

  WriteLn;
  Write('Case (1-', CaseCount, '), anything else quits: ');
  ReadLn(S);

  { Read into a string and convert by hand: reading an Integer from a
    mistyped line would abort with the very error case 18 tests for. }
  Val(S, TestNo, E);
  if E <> 0 then TestNo := 0;
end;

{$ifdef OPT_PRINTER}
(**
 * Stops the emulator. A printer build is an unattended run -- the output
 * goes to a file and nobody is sitting in front of the window -- so asking
 * for a case that does not exist is the signal that there is nothing left
 * to do, and the machine can go away by itself.
 *
 * Every target says that differently: CSpect exits on its break opcode when
 * started with -exit, the Agon emulator on a write to port 0, and Fuse on a
 * breakpoint that has to be tripped by a write to port $1F -- harmless on
 * real hardware, where $1F is the read-only Kempston port. CP/M has no
 * emulator to stop: tnylpo ends when the program does.
 *)
procedure QuitHost;
begin
  {$ifdef SYS_AGON}
    inline($3e / $00 / $d3 / $00);
  {$endif}

  {$ifdef SYS_ZXNEXT}
    QuitEmulator;
  {$endif}

  {$ifdef FUSE}
    inline($3e / $00 / $d3 / $1f);
  {$endif}
end;
{$endif}

(* -------------------------------------------------------------------------- *)
(* --- Main ----------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

{$ifndef SYS_ZX}
var
  E: Integer;
{$endif}

begin
  TestNo := 0;

  {$ifdef SWEEP}
    { One call, one case, in order. The exit code tells the loader whether
      another one is coming: zero while cases remain, -1 once they are all
      done. It has to be set on every call, because __exitcode is static
      data in the image just like NextCase -- a sentinel left over from an
      earlier sweep would stop the next one after its first case. }
    ExitCode := 0;

    Inc(NextCase);
    TestNo := NextCase;
  {$else}
    {$ifndef SYS_ZX}
      if ParamCount >= 1 then
      begin
        Val(ParamStr(1), TestNo, E);
        if E <> 0 then TestNo := 0;
      end;
    {$endif}

    { No menu in a printer build: it would go into the log instead of onto a
      screen, and there is nobody there to answer it. }
    {$ifndef OPT_PRINTER}
      if (TestNo < 1) or (TestNo > CaseCount) then Ask;
    {$endif}
  {$endif}

  if (TestNo >= 1) and (TestNo <= CaseCount) then
    Run
  else
  begin
    WriteLn;
    {$ifdef OPT_PRINTER}
      WriteLn('No case ', TestNo, ' -- all ', CaseCount, ' have run.');

      {$ifdef SWEEP}
        { The loader stops on this, so it never needs to know how many cases
          there are. Ends up in BC via __done, which is what USR yields, and
          BASIC reads it as 65535. Resetting the counter lets a second sweep
          start over without reloading the tape. }
        ExitCode := -1;
        NextCase := 0;
      {$endif}

      QuitHost;
    {$else}
      WriteLn('Nothing to do.');
    {$endif}
  end;

  { Only reached when the case did NOT do its job, or when no case ran at
    all. A case that works never comes back here. }
  WriteLn;
end.
