![Logo](docs/images/logo.png)

# PASTA/80

PASTA/80 is a simple [Pascal](https://en.wikipedia.org/wiki/Pascal_(programming_language)) cross compiler targeting the [Z80](https://en.wikipedia.org/wiki/Zilog_Z80) microprocessor. It generates code for [CP/M](https://en.wikipedia.org/wiki/CP/M), the classic [ZX Spectrum 48K](https://en.wikipedia.org/wiki/Sinclair_ZX_Spectrum) and the [ZX Spectrum Next](https://www.specnext.com). The compiler follows [Niklaus Wirth's](https://de.wikipedia.org/wiki/Niklaus_Wirth) classical single-pass recursive-descent approach, so it doesn't have an explicit syntax tree, but instead generates code on the fly during parsing.

## Supported language elements

The supported Pascal dialect is an almost exact clone of the original [Turbo Pascal 3.0](https://en.wikipedia.org/wiki/Turbo_Pascal) for CP/M (see [this manual](https://bitsavers.trailing-edge.com/pdf/borland/turbo_pascal/Turbo_Pascal_Version_3.0_Reference_Manual_1986.pdf) for details). So you have at your disposal these language elements:

* All the basic data types (`Boolean`, `Byte`, `Char`, `Integer`, `Pointer`, `Real` and `String`).
* `array of`, `record`, `set of`, enumerations, subranges and pointers as a way of building new data types.
* The decision-making elements `if..then..else` and `case..of`.
* The loop elements `for..do`, `while..do` and `repeat..until`.
* The `with..do` notation for "opening" records.
* `procedure` and `function` including value and `var` parameters and nesting.
* The standard procedures for screen input and output (i.e. `ReadLn`, `WriteLn` etc.).
* All conversion and utility procedures and functions that Turbo Pascal 3.0 had.
* The three kinds of disk files, that is untyped (`file`), typed (`file of`) and `Text`.
* A dynamic heap of up to 32767 bytes with `GetMem`, `FreeMem`, `New` and `Dispose`.
* Inline assembly (via opcodes, not via mnemonics, so [this page](https://clrhome.org/table/) might be handy).
* Some compiler directives:
  * `$i <file>` for including Pascal source files (including nesting and cycle detection)
  * `$l <file>` for including an assembly file (aka "linking" a library)
  * `$a(+/-)`   for enabling or disabling absolute mode (default is on, disable for recursion)
  * `$i(+/-)`   for enabling or disabling IO checking (when off, check `IOResult` after calls)
  * `$k(+/-)`   for enabling or disabling stack overflow checking
  * `$u(+/-)`   for enabling or disabling Ctrl-C checking

The compiler also has some features that were borrowed from or inspired by later versions of Turbo Pascal:

  * C-style `//` one-line comments in addition to `{..}` and `(*..*)`.
  * Binary literals (using a `%` prefix).
  * `Break` and `Continue` for loop control.
  * Querying the keyboard via `KeyPressed` and `ReadKey`.
  * Color support via `TextColor` and `TextBackground` with constants for the 8 Spectrum Next colors.
  * `Inc` and `Dec` for more efficient increasing and decreasing of variables.
  * `Include` and `Exclude` for more efficient handling of sets.
  * A simple `Assert` facility that counts passes/fails and shows the failed line number.

Since that covers most of the functionality of Turbo Pascal 3 you might ask what is missing. These are the current limitations:

* All the remaining compiler directives are not yet supported.
* `Mark`/`Release` are not currently supported.
* The standard files `Input`, `Output`, `Kbd`, `Con` and `Lst` are not supported.
* Overlays are not yet supported.
* `Chain` and `Execute` are not supported.
* Add-on libraries from the PC version of Turbo Pascal 3.0 (graphics etc.) are not yet supported.
* The [new instructions of the Z80N CPU](https://wiki.specnext.dev/Extended_Z80_instruction_set) inside the ZX Spectrum Next are not yet being leveraged.
* No separate compilation. Everything is compiled from source, always.
* Binary size is quite large compared to the original.

The runtime library, being partially written in Pascal itself, gets quite large when compiled. I hope to bring this down again by reimplementing more of it in Z80 assembly (or improve the code generator, which, although it has a peephole optimizer, is not generating super-efficient Z80 code).

## Building and setting up the compiler

The compiler is itself written in Pascal. You can compile it with [Free Pascal](https://www.freepascal.org) (I use version 3.2.2). Just run

```
$ fpc pasta
```

The Pascal compiler generates Z80 assembler code and relies on [sjasmplus](https://z00m128.github.io/sjasmplus) as a backend for the final translation step to binary. It can also, in `--ide` mode (see below), make use of `nano`, Visual Studio Code (via the `code` command) and `tnylpo`.

The compiler tries to detect external tools automatically, but it's best to create a file `.pasta80.cfg` in your home directory specifying necessary paths (there is a sample in `misc` that you can adapt):

```
# PASTA/80 config

HOME      = ~/Projects/pasta80
SJASMPLUS = ~/Library/bin/sjasmplus
TNYLPO    = ~/Library/bin/tnylpo
NANO      = /opt/local/bin/nano
VSCODE    = /usr/local/bin/code
```

## Using the compiler

To run the compiler just invoke the executable with the name of a Pascal source file to translate. The default target is CP/M. There is an optional parameter that enables some simple peephole optimizations and another one that uses dependency analysis to eliminate unused Pascal procedures and functions:

```
$ pasta hello.pas             # Compiles hello.pas to hello.com
$ pasta hello                 # Source file .pas suffix is optional
$ pasta --opt hello.pas       # Enables peephole optimizations
$ pasta --opt --dep hello.pas # The same plus dependency analysis
```

You can run the resulting `.com` files on a real CP/M machine or in a CP/M emulator. I recommend [tnylpo](https://gitlab.com/gbrein/tnylpo). For programs that use VT52 control codes you have to start tnylpo in full-screen mode:

```
$ tnylpo hello                # Run in line-mode
$ tnylpo -s -t @ hello        # Run in b/w full-screen mode, wait for key when finished
$ tnylpo -soy,4,0 -t @ hello  # Run in full-screen mode with (Spectrum Next) colors
```

| "Hello, World" in line mode  | "Hello, World" in full-screen |
| :-------: | :----: |
| ![Screenshot](docs/images/hello1.png) | ![Screenshot](docs/images/hello2.png) |

To generate binaries for the ZX Spectrum 48K and ZX Spectrum Next targets, use the `--zx` and `--zxn` parameters, respectively.

```
$ pasta --zx hello.pas        # Compiles for ZX Spectrum 48K
$ pasta --zxn hello.pas       # Compiles for ZX Spectrum Next
```

The main difference between the two (currently) is that the ZX Spectrum Next target supports file IO, while the ZX Spectrum 48K target does not. The other routines are mostly the same. Screen output is handled via `rst $10` in the ROM. In both cases the binaries are expected to be run from address 0x8000.

The default output format is a raw binary file that contains exactly the bytes of the compiled program. For your convenience, the compiler can also generate tape files or files with a +3DOS header:

```
$ pasta --zx --tap hello.pas  # .tap file with BASIC loader
$ pasta --zxn --dos hello.pas # .bin file with +3DOS header
```

## Examples and tests

There is a folder containing `examples` and a folder containing `tests` for the compiler. The main test suite `all.pas` needs to be compiled with `--opt` because of its size. Otherwise it won't fit into 64K (neither of the Spectrum targets can currently handle it). Both the examples and the tests should give you a pretty good overview of what the compiler can do.

## Minimalistic IDE

As a fun little gimmick the compiler can be started like this

```
$ pasta --ide
```

to run it in an interactive mode that has an interface reminiscient of Turbo Pascal 3.0.

| Main menu | Editor |
| :-------: | :----: |
| ![Screenshot](docs/images/idemenu.png) | ![Screenshot](docs/images/ideedit.png) |

When started in an ordinary terminal, this mode relies on the editor `nano` being present on your system (on MacOS you might want to install the real `nano` via a package manager because Apple sells you the much more limited `pico` editor as `nano`). You can also run it in a shell within Visual Studio Code, in which case it would automatically use VSC's editor (via the `code` command, which, on a Mac, you might [have to make available from VCS's settings](https://code.visualstudio.com/docs/setup/mac#_configure-the-path-with-vs-code)).

In both cases `tnylpo` is expected to be available for running CP/M programs. Press \<R\> to run a program in line mode and \<Shift-R\> to run it in full-screen mode.

## Application Gallery

The following screenshots show some applications compiled for the CP/M target and running in the `tnylpo` emulator.

| 2048 | Game of Life |
| :-------: | :----: |
| ![Screenshot](docs/images/2048.png) | ![Screenshot](docs/images/life.png) |

| Micro Calc | Galactic Empire |
| :-------: | :----: |
| ![Screenshot](docs/images/microcalc.png) | ![Screenshot](docs/images/empire.png) |

These screenshots show some applications compiled for the ZX Spectrum 48K target and running in the FUSE emulator.

| 2048 | Game of Life |
| :-------: | :----: |
| ![Screenshot](docs/images/2048zx.png) | ![Screenshot](docs/images/lifezx.png) |

| Graphics Demo | Equation Solver |
| :-------: | :----: |
| ![Screenshot](docs/images/graphics.png) | ![Screenshot](docs/images/pqformula.png) |

I also solved all puzzles of [Advent of Code 2022](https://github.com/pleumann/aoc22) with an earlier version of the compiler and made [YouTube videos](https://youtube.com/playlist?list=PLcjDDXgGeSQ6E3NLeSOH0Tn7UorYBgUOH&si=SAoOqUbi70c4ezgi) of the solutions running on the ZX Spectrum Next, in CP/M mode.

# License

**PASTA/80 Pascal Compiler**

Copyright (c) 2020-2025 by Jörg Pleumann

The PASTA/80 compiler is free software: you can redistribute it and/or modify
it under the terms of the **GNU General Public License (GPL)** as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

* The runtime library (folder `rtl`) comes with a **linking exception** that makes sure the GPL does not transfer to binaries created using PASTA/80.

* The examples (folder `examples`) are considered **public domain** or whatever comes closest to that in your jurisdiction.

* Individual files or folders may use different licenses, so you might want to double check.

Everything is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR
A PARTICULAR PURPOSE. See the GNU General Public License for more details.

What does this mean for you?

* You can **use the compiler**, free of charge, to build any application, open-source or prioprietary, free or paid, and distribute the generated binary without restriction. You can **distribute binaries** created with PASTA/80 under a **license of your choosing**.

* You can **modify the compiler** according to your needs. If you **distribute the compiler** or parts of it, binary or source, modified or not, you have to **comply with the rules laid out in the GPL** (copyright info, source code, ...) unless the linking exception applies.

# Acknowledgements

The math48 library is coypright (c) 1980 by Anders Hejlsberg, used by [permission](https://github.com/pleumann/pasta80/issues/7).

Some assembly routines adapted from Leventhal/Saville, "Z80 Assembly Subroutines", Osborne/McGraw-Hill 1983.

Turbo Pascal is a registered trademark of Code Gear LLC / Embarcadero.

Z80 is a registered trademark of Zilog, Inc.
