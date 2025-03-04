# Pascal Compiler for Z80

This is a [Pascal](https://en.wikipedia.org/wiki/Pascal_(programming_language)) (cross) compiler targeting the [Z80](https://en.wikipedia.org/wiki/Zilog_Z80) microprocessor. It generates code for [CP/M 2.2](https://en.wikipedia.org/wiki/CP/M) and the [ZX Spectrum Next](https://www.specnext.com) (the latter is currently not working, but I hope to get it back on par with CP/M soon).

## Supported language elements

The supported Pascal dialect is an almost exact clone of the original [Turbo Pascal 3.0](https://en.wikipedia.org/wiki/Turbo_Pascal) for CP/M (see [this manual](https://bitsavers.trailing-edge.com/pdf/borland/turbo_pascal/Turbo_Pascal_Version_3.0_Reference_Manual_1986.pdf) for details). So you have at your disposal these language elements:

* All the basic data types (`Boolean`, `Byte`, `Char`, `Integer`, `Real`, `String` and `Pointer`).
* `array of`, `record`, `set` and pointers as a way of building new data types.
* The typical control flow elements `if..then..else`, `case..of`, `while..do` and `repeat..until`.
* All conversion and utility procedures and functions that Turbo Pascal 3.0 had.
* The standard procedures for screen input and output (i.e. `ReadLn`, `WriteLn` etc.).
* The three kinds of disk files, that is untyped (`file`), typed (`file of`) and `Text`. 
* A dynamic heap of up to 32767 bytes with `GetMem`, `FreeMem`, `New` and `Dispose`.
* Inline assembly (via opcodes, not via mnemonics).

Since that list sounds quite exhaustive, so you might ask what is missing. These are the current limitations:

* Most compiler directives are not yet supported with the exception of
  * `$i <file>` for including Pascal source files
  * `$l <file>` for including an assembly file (aka "linking" a library)
  * `$a(+/-)`   for enabling or disabling absolute mode (default is on, disable for recursion)
  * `$i(+/-)`   for enabling or disabling IO checking (when off, check `IOResult` after calls)
  * `$k(+/-)`   for enabling or disabling stack overflow checking
  * `$u(+/-)`   for enabling or disabling Ctrl-C checking
* `Mark`/`Release` are not currently supported.
* Overlays are not yet supported.
* Binary size. The runtime library, being partially written in Pascal itself, gets quite large when compiled. I hope to bring this down again by reimplementing more of it in Z80 assembly (or improve the code generator, which, following [Niklaus Wirth's](https://de.wikipedia.org/wiki/Niklaus_Wirth) classical single-pass recursive-descent design, is not generating super-efficient Z80 code).

## Building and setting up the compiler

The compiler is itself written in Pascal. You can compile it with [Free Pascal](https://www.freepascal.org) (I use version 3.2.2). Just run

```
$ fpc pl0
```

The Pascal compiler generates Z80 assembler code and relies on [zasm](https://k1.spdns.de/Develop/Projects/zasm/Documentation/index.html) as a backend for the final translation step to binary. It can also, in `--ide` mode (see below) make use of `nano`, Visual Studio Code (needs `code` command in path on MacOS) and `tnylpo`.

The compiler tries to detect external tools automatically, but it's best to create a file `.pl0.cfg` in your home directory specifying necessary paths (there is a sample in `etc` that you can adapt):

```
# PL/0 config

HOME   = ~/Projects/pl0
ZASM   = ~/Library/bin/zasm
NANO   = /opt/local/bin/nano
VSCODE = /usr/local/bin/code
TNYLPO = ~/Library/bin/tnylpo
```

## Using the compiler

To run the compiler just invoke the executable with the name of a Pascal source file to translate. There is an optional parameter that enables some simple peephole optimizations:

```
$ pl0 hello.pas        # Compiles hello.pas to hello.com
$ pl0 --opt hello.pas  # Does the same with optimizations
```

You can run the resulting `.com` files on a real CP/M machine or in a CP/M emulator. I recommend [tnylpo](https://gitlab.com/gbrein/tnylpo). For programs that use VT52 control codes you have to start tnylpo in full-screen mode:

```
$ tnylpo hello                # Run in line-mode
$ tnylpo -s -t @ hello        # Run in b/w full-screen mode, wait for key press when finished
$ tnylpo -soy,4,0 -t @ hello  # Run in full-screen mode with (Spectrum Next) colors
```

There is a folder containing `examples` and a folder containing `tests` for the compiler. The main test suite `all.pas` needs to be compiled with `--opt` because of its size. Otherwise it won't fit into 64K. Both the examples and the tests should give you a pretty good overview of what the compiler can do.

## Minimalistic IDE

As a little gimmick the compiler can be started like this

```
$ pl0 --ide
```

to run it in an interactive mode that has an interface similar to Turbo Pascal 3.0. When started in an ordinary terminal, this mode relies on the editor `nano` being present on your system. You can also run it in a shell within Visual Studio Code, in which case it would automatically use VSC's editor (via the `code` command, which, on a Mac, you might have to make available from VCS's settings). In both cases `tnylpo` is expected to be available for running programs. Press \<R\> to run a program in line mode and \<Shift-R\> to run it in full-screen mode.
