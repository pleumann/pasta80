# PL/0 on IR4th – an experiment

This branch (`ir4-hack`) is a throwaway experiment. It is not meant to be
merged. The question was: what would it feel like to use
[IR4th](https://github.com/stefanhaustein/IR4th) as the backend of PASTA/80?
Rewiring all of PASTA/80 just to find out would be too expensive, so the
experiment starts from the PL/0 compiler PASTA/80 grew out of (commit
`0d323ab`, "I think I am going to call this version 1.0"). It adds an IR4th
backend next to the existing Z80 one. The same compiler can then produce
both, and the results can be compared directly.

## What IR4th is

IR4th is a typed, Forth-like intermediate language by Stefan Haustein. Its
compiler `ir4c` is about 6k lines of strict C89 and generates machine code
directly, without an assembler. It has two passes and removes unreachable
functions. Targets are Z80 (CP/M), 6502 (sim65, C64), ARM64 and RV32. The
stack holds three types: `:i` (machine word), `:a` (address) and `:f` (float).
On the Z80, `:f` uses Hejlsberg's math48, the same 6-byte Real format as
PASTA/80. Its `tools/math48.asm` is byte-identical to `rtl/math48.asm`.

`ir4c` analyzes the call graph. Functions that are not recursive get static
locals, and recursive ones get stack frames. This is what Turbo Pascal's
`{$A-}` does, except here it happens automatically.

## Building and running

```bash
# IR4th, from a local copy in ../IR4th. Raise MAX_TOK in src/ir4.h from
# 32 to 128 first: absolute require paths don't fit into 32 chars.
(cd ../IR4th && make)

fpc pl0
export PL0_HOME=$PWD     # an old PL0_HOME from 2020 may still be around

./pl0 --asm $(which sjasmplus) examples/wirth.pl0                   # Z80 asm  -> .com
./pl0 --ir4 --ir4c ../IR4th/build/ir4c examples/wirth.pl0           # IR4th    -> .com
./pl0 --ir4-target sim65 --ir4c ../IR4th/build/ir4c examples/wirth.pl0   # -> .sim
./pl0 --ir4-target c64   --ir4c ../IR4th/build/ir4c examples/wirth.pl0   # -> .prg

tnylpo examples/wirth.com
sim65 examples/wirth.sim
```

The Z80 runtime (`pl0.z80`) was originally written for zasm. It was ported
to sjasmplus (`IFDEF`/`ENDIF`, `--raw`) so that the baseline builds again.

## How the backend works

| PL/0 | IR4th |
|---|---|
| global `var x` | `var g_X :i` (declared up front) |
| local `var x` | function local `l_X :i` |
| `x := e` | `e -> l_X` |
| `+ - * /`, relops, `odd` | `+ - * /`, `== != < > <= >=`, `1 and 0 !=` |
| `if c then s` | `c if s then` |
| `while c do s` | `begin c while s repeat` |
| `call p` | `procN` (plus `fwd procN ( )` up front) |
| `! "text"` | `data stringN` + `stringN pl0_puts` |
| `! e` / `? x` | `e pl0_putn` / `pl0_getn -> l_X` |

- **Two output buffers.** IR4th needs everything declared before use. It
  does not allow forward references to `data` at all, and calls need `fwd`.
  So declarations (`var`, `fwd`, `data`) and function bodies are written to
  separate files, and the two are concatenated at the end. This fits a
  single-pass compiler well.
- **Nested procedures.** IR4th has no nested functions and no access to
  another function's locals. Wirth's ordering helps here: all nested
  procedures are parsed before the enclosing procedure's body, and before
  its `fn` header is written. By then the compiler knows which locals are
  accessed from inner procedures. Only those move into a
  `frame :bytes N`. The prologue registers the frame's address in a global
  display, and the epilogue restores the old entry. All other locals stay
  native IR4th locals, so IR4th still optimizes them (static locals for
  non-recursive functions).
- **Runtime.** The IR4th version of `pl0.z80` is `pl0rt.ir4`, which is
  portable. Platform I/O is in `cpm/`, `sim65/` and `c64/pl0io.ir4`.
  IR4th's `require` picks the right one automatically. The output imitates
  `pl0.z80`, so the results can be compared byte for byte.

The backend adds about 370 lines to `pl0.pas`. Most of it is `if Ir4`
branches in the existing `Emit*` procedures.

## Results

All 16 tests and examples give the same output on all targets. The one
exception is `wirth` on the old Z80 backend: there, `odd` is always false
(see below), so 7×12 comes out as 0. IR4th gets it right on both CPUs.

Sizes in bytes:

| program | Z80 (asm) | Z80 (IR4th) | 6502 sim65 | C64 .prg |
|---|---:|---:|---:|---:|
| empty | 437 | 12 | 30 | 37 |
| hello | 457 | 252 | 695 | 810 |
| fact | 571 | 434 | 1140 | 1267 |
| fibo | 602 | 441 | 1188 | 1315 |
| primes | 747 | 632 | 1594 | 1721 |
| relops | 1867 | 1250 | 2575 | 2690 |
| scopes | 1364 | 1369 | 2925 | 3040 |
| squares2 | 655 | 1050 | 3311 | 3085 |
| wirth | 1356 | 1617 | 4823 | 4609 |

Runtime for `primes` (all primes below 32767):

| | wall time |
|---|---:|
| Z80 (asm) under tnylpo | 105.8 s |
| Z80 (IR4th) under tnylpo | 101.7 s |
| 6502 under sim65 | 96.3 s |

The emulators differ, so only the first two numbers can be compared with
each other.

The C64 `.prg` files look right (BASIC stub `10 SYS 2061`, BASIC ROM
switched off). They have not been run, though: VICE is not installed here.

## Findings

1. **The mapping is natural.** PL/0's control structures map one to one
   onto structured IR4th, and no labels are needed. The generated IR is
   easy to read (`g_X g_X * -> l_SQU`). The emitter was already a stack
   machine (push/pop on the Z80 stack), so the change is shallow.
2. **User code gets smaller, but not faster.** On the Z80, compiled user
   code is roughly 25–40 % smaller than the old asm backend's. Speed is
   about the same (−4 %). The comparison isn't entirely fair, because the
   old backend is naive, while PASTA/80's current peephole optimizer is
   much better. A quick test with recursive Fibonacci and a nested loop in
   current PASTA/80 also came out about even.
3. **A runtime written in IR4th is about twice the size of hand-written
   asm.** `? x` alone pulls in 551 bytes. All of `pl0.z80` is about 430.
   For PASTA/80, whose RTL is about 7,300 lines of Z80 asm, this means the
   RTL has to stay asm. IR4th would need a way to link it, either real
   assembly or object linking. `code ... end` with raw bytes and `extern`
   with fixed addresses are not enough.
4. **Nested procedures need no changes to IR4th.** The frame + display
   approach works, and the cost is only paid where outer locals are
   actually used.
5. **A second CPU comes nearly for free.** Moving the same compiler output
   to the 6502 took about 60 lines of platform I/O and no compiler changes.
   This is the real selling point.
6. **The 6502 code is 2–3× larger than the Z80 code.** IR4th's 6502 backend
   keeps the data stack in normal RAM, indexed by X with absolute
   addressing. A zero-page data stack, as in classic 6502 Forths, would
   probably make the code much smaller.
7. **A bug in the old backend.** In PL/0 1.0's Z80 backend, `odd` is always
   false. `EmitOdd` pushes DE (D = 0), and the following `pop af / and a`
   tests D.

## What it would mean for PASTA/80

This experiment only covers what PL/0 has: 16-bit integers, procedures
without parameters, nested scopes and simple I/O. Several things PASTA/80
needs are not covered:

- **Large values on the stack.** PASTA/80 pushes strings (up to 256 bytes),
  sets (32 bytes) and records onto the stack. IR4th only has word, address
  and float slots on its data stack. Part of this is covered by a feature
  that `spec.md` does not document (pointed out by Stefan): `:bytes` input
  parameters.

  ```forth
  fn mutate_copy ( b :bytes 16 -> :i )
    99 b !i8
    b @u8
  end
  ```

  The caller pushes an `:a`. On entry, the callee copies 16 bytes into a
  buffer of its own, and in the body `b` is the address of that copy.
  Changes to it don't reach the caller (see `tests/rtl.ir4`). The buffer
  is static in non-recursive functions and lives in the stack frame in
  recursive ones, like any local. This is exactly the semantics of
  Pascal's value parameters, so strings, records, arrays and sets passed
  by value map directly to `s :bytes 256`, `r :bytes 12` and so on.

  Some things remain open:

  - **Expressions.** `s1 + s2`, set operations and functions returning a
    string or record still need temporaries managed by the compiler
    (`| tmp1 :bytes 256`). Results can only be `:i`, `:a` or `:f`, so a
    function returning a large value gets a hidden destination-address
    parameter, as in TP5. (`lib/pascal.ir4` in IR4th already uses this
    style.)
  - **Locals are declared before the body.** IR4th wants every local in
    the `fn` header, but a single-pass compiler only knows which
    temporaries a body needs after parsing it. The fix is the same trick
    as the decl/body split in this experiment, one level down: buffer
    each function's body and write its header afterwards.
  - **Copy cost.** The copy is a byte loop generated as IR, not `LDIR`,
    and it runs on every call. For 256-byte strings this adds up. Hidden
    copies also count against the limits of 8 parameters and 16 locals,
    and so does an internal counter local.

  So the biggest parser change shrinks to "temporaries for expressions,
  plus buffering per function".
- **The RTL.** See finding 3.
- **Overlays and banking.** IR4th has one code buffer of up to 64K with a
  single base address. It has no sections and no far calls.
- **Limits.** IR4th allows 16 locals, 8 parameters, tokens of 32 chars and
  16K of data. These are compile-time constants and easy to raise.
- **Tooling.** PASTA/80's `.lst` output (used by CSpect for source-level
  debugging), the sjasmplus Lua size/overlay report and the mapping of
  runtime-error addresses back to the source would all have to be replaced.

PASTA/80 would not become a "transpiler" this way. IR4th is a low-level IR
without expressions, types or scopes. Type checking, memory layout, scope
resolution and lowering all stay in the front end. The setup is close to
classic P-code compilers (Pascal-P, UCSD).

## Feedback for IR4th

These are small papercuts found along the way:

- `MAX_TOK` = 32 is too small for absolute `require` paths.
- `lib/` is searched relative to the current directory, not to the
  location of `ir4c`. The compiler has to `ChDir` before calling it.
- There are no forward references to `data`. That's fine, but worth
  documenting next to `fwd`.
- Errors refer to lines in the `.ir4` file. A `#line`-style directive
  would let a front end report errors in its own source.
- The 6502 data stack could live in zero page (see finding 6).
- `:bytes` input parameters (copy-in value parameters) are not in
  `spec.md` yet.
- For a Pascal front end, it would help to declare locals inside the body
  instead of only in the `fn` header. Temporaries could then be added
  while the body is being parsed. A block copy (`LDIR` on the Z80) for
  `:bytes` parameters would also help.
