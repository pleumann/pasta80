program HaltTest;
(* Halt stops the program where it stands. It does not return to the caller,
   so neither the statements after it nor the rest of the call chain it was
   reached through ever run -- which is why this test halts from three levels
   down rather than from the main block.

   The exit code is the part that cannot be checked from the inside: by the
   time it exists, the program is gone. It has to be looked at from outside,
   and that is a different mechanism on every target -- see the note the
   program prints for the one it was built for. Halt(N) and "ExitCode := N;
   Halt" are the same thing; the compiler stores both in __exitcode and jumps
   to __done ([pasta.pas] HaltProc).

   This is not an error case, so it stays here rather than in tests/errors:
   nothing goes wrong, the program just stops early on purpose. Run it by
   hand and compare against:

     --- HaltTest ---
     <the platform note>
     Three levels down, about to halt.
     Halting with exit code 42...

   Everything below that point is a "you should not see this" line. If any of
   them appears, Halt did not do its job. *)

const
  Code = 42;

procedure Innermost;
begin
  WriteLn('Three levels down, about to halt.');
  WriteLn('Halting with exit code ', Code, '...');

  Halt(Code);

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

begin
  WriteLn('--- HaltTest ---');
  WriteLn;

  {$ifdef SYS_CPM}
    WriteLn('On CP/M there is nothing to check from outside: CP/M 2.2 has');
    WriteLn('no return code, so __done warm-boots via RST 0 and the exit');
    WriteLn('code is dropped. Only the missing output proves Halt worked.');
  {$endif}

  {$ifdef SYS_ZX}
    WriteLn('On the ZX the exit code comes back in BC, which is what the');
    WriteLn('BASIC function USR yields. Type PRINT USR 32768 to see it --');
    WriteLn('the generated loader says RANDOMIZE USR, which discards it.');
  {$endif}

  {$ifdef SYS_AGON}
    WriteLn('On Agon the exit code is handed to MOS as the process return');
    WriteLn('code, so the shell you started this from is where to look.');
  {$endif}

  WriteLn;

  Outer;

  WriteLn('You should not see this (main, after Outer).');
end.
