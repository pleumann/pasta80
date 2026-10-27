<p align="center"><a href="../README.md">Compiler Manual</a> | <a href="../docs/rtl-reference.md">Library Reference</a></p>

![Logo](../docs/images/logo.jpg)

# PASTA/80 Tests

This folder holds the compiler's test suites. Normally you run them all at once with `pasta80 --selftest`, which is described in the [Compiler Manual](../README.md#examples-and-tests). This page explains what is behind that, for anyone who wants to run a single suite, add tests, or find out why something failed.

## Suites

| Suite | What it tests | CP/M | 128K | Next | Agon |
|---|---|:-:|:-:|:-:|:-:|
| [core](core.pas) | The language and the run-time library: constants, operators, types, control flow, procedures, `Write`/`Str`/`Val`, built-ins and much more. | ✓ | ✓ ᴼ | ✓ ᴼ | ✓ |
| [heap](heap.pas) | The heap manager: `New`/`Dispose`, `GetMem`/`FreeMem`, small blocks, stress, defragmentation. | ✓ | ✓ | ✓ | ✓ |
| [files](files.pas) | File I/O: `Text`, typed and untyped files, `Seek`, `Erase`, `Rename`, `IOResult`, large files. | ✓ | | ✓ ᴼ | ✓ |
| [more](more.pas) | Regression tests that no longer fit into `core` (see below). | ✓ | ✓ | ✓ | ✓ |
| [params](params.pas) | `ParamCount` and `ParamStr`. | ✓ | | | ✓ |
| [sound](sound.pas) | `Sound`/`NoSound` (Agon) and `SoundMs` (Spectrum). | | ✓ | ✓ | ✓ |
| [overlays](overlays.pas) | Far calls between overlays and resident code in all directions, including functions, `var` parameters, recursion and nested procedures. | ✓ | ✓ ᴼ | ✓ ᴼ | ✓ ᴼ |
| [errors](errors.pas) | Runtime errors that must stop the program: division by zero, `Real` overflow, invalid input to `Val`/`Read`, bad string indices, stack overflow, full heap, `Halt`. | ✓ | ✓ ᴼ | ✓ ᴼ | ✓ |

ᴼ built with overlays.

On CP/M, `overlays.pas` runs as an ordinary program, since CP/M has no overlays. The 48K Spectrum is not supported, because `core.pas` doesn't fit there. The 128K has no file system, so `files.pas` doesn't run there.

The authoritative version of this table is `SelfTest` in `pasta80.pas` (`TestSuites`, `MandatoryFor`, `NeedsOverlays`). If the two disagree, the code is right.

## How a run works

`--selftest` builds each suite with `--printer`, so all output goes to the emulated printer and from there into a log file (`tests/*.log`). At the end, each suite closes its emulator itself. When the compiler has collected all the logs, it counts the results and prints the report.

The normal suites use `Assert`. A failed `Assert` doesn't stop the program: it counts and continues. Each suite ends by printing the two totals:

```
Passed assertions: 1574
Failed assertions: 0
```

`SelfTest` reads these two lines. If a log doesn't contain them, for instance because the program crashed, the report shows `???` for that suite. `TotalTests` in `SelfTest` holds the expected total per target, so a run that ended early does not count as a pass.

`errors.pas` works differently, because every case there ends the program. Only one case runs per start, and the number of the case is passed in:

- **CP/M**: as a command line parameter. The compiler starts tnylpo once per case.
- **Agon**: as a command line parameter, too. The commands come from `misc/agonerrs.txt`, which becomes the `autoexec.txt`.
- **128K and Next**: There is no command line, so the program counts for itself. A BASIC loader (`misc/errors128.bas`, `misc/errorsnext.bas`) calls it again until it reports that no cases are left.

Each case first prints the output it expects, e.g. `Expected output: Division by zero`. The last line of the case must match it. The comment at the top of `errors.pas` has the details.

## Running a single suite

Build the suite without `--printer` and start it like any other program. The output then appears on the screen, and the emulator stays open at the end:

```bash
$ pasta80 tests/heap.pas && tnylpo tests/heap.com   # CP/M
$ pasta80 --128 --tape tests/core.pas               # Spectrum 128K, then load core.tap in Fuse
```

`--selftest` uses the formats `--tape` (Spectrum) and the default binary (CP/M, Agon). Overlays are on by default, so there is no need to pass `--ovr`.

`params.pas` expects the parameters `TIC TAC     TOE`, with several spaces before `TOE`.

## Making changes

If you are adding/removing tests or otherwise changing the test suite, keep these things in mind:

- **Size matters.** `core.pas` barely fits into the 64K of CP/M, and on the 128K and the Next only with overlays. That is why `more.pas` exists. Tests that don't fit into `core` go there, and every new test should be weighed against its size.
- **Assertions embed full filenames.** That means, the actual size of a compiled test suite depends on where the source code is located. With `core.pas` that close to the limit, the checkout location can decide whether the tests compile **for you**.
- **Never remove the `overlay` markers or the `{$m ...}` directives.** They are what makes `core` and `files` build on the Spectrum targets. Code inside an `overlay` procedure costs overlay space, not resident space.
- **A new suite** needs one entry in each of the tables in `SelfTest`. At its end, it must print the two totals and close the emulator in a `--printer` build. See the end of any existing suite for how.
- **Adjust `TotalTests`** in `SelfTest` whenever the number of assertions changes, or the report will say FAIL.
