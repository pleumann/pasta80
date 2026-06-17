(**
 * Example for exchanging data between classic ZX Spectrum BASIC and Pascal. We
 * use the printer buffer for passing data from BASIC to Pascal and the BC
 * register pair for passing a 16 bit value back to BASIC. When the program
 * terminates, BC receives its value from the ExitCode variable (which can also
 * be set by the Halt procedure).
 *
 * Suitable modified BASIC loader could look like this:
 *
 * 10 CLEAR 32767
 * 20 LOAD "bin" CODE 32768
 * 30 FOR n=0 to 255: POKE 23296+n, n: NEXT n
 * 40 PRINT USR 32768
 *
 * This should print 32640, which is the sum of all numbers from 0 to 255.
 *)
program ZXSum;

{$ifndef SYS_ZX48}
  {$ifndef SYS_ZX128}
    {$error This program only work on classic Spectrums.}
  {$endif}
{$endif}

var
  B: array [0..255] of Byte absolute 23296; // Array sits in printer buffer.
  I, J: Integer;

begin
  J := 0;

  for I := 0 to 255 do
    Inc(J, B[I]);

  ExitCode := J;
end.