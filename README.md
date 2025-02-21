# Pascal Compiler for Z80

This is a [Pascal](https://en.wikipedia.org/wiki/Pascal_(programming_language)) (cross) compiler targeting the [Z80](https://en.wikipedia.org/wiki/Zilog_Z80) microprocessor. It generates code for [CP/M 2.2](https://en.wikipedia.org/wiki/CP/M) and the [ZX Spectrum Next](https://www.specnext.com) (the latter is currently not working, but I hope to get it back on par with CP/M soon). The Pascal dialect is an almost exact clone of the original [Turbo Pascal 3.0](https://en.wikipedia.org/wiki/Turbo_Pascal) for CP/M (see [this manual](https://bitsavers.trailing-edge.com/pdf/borland/turbo_pascal/Turbo_Pascal_Version_3.0_Reference_Manual_1986.pdf) for details).

The compiler is itself written in Pascal. You can compile it with [Free Pascal](https://www.freepascal.org) (I use version 3.2.2). Just run

```
$ fpc pl0
```

The Pascal compiler generates Z80 assembler code and relies on [zasm](https://k1.spdns.de/Develop/Projects/zasm/Documentation/index.html) as a backend for the final translation step to binary.

Add these lines to your `.bash_profile` to make sure everything is found:

```
export PL0_HOME=<path to pl0 folder>
export PL0_ASM=<path of zasm binary>
```

To run the compiler just invoke the executable with the name of a Pascal source file to translate. There is an optional parameter that enables some simple peephole optimizations:

```
$ pl0 hello.pas        # Compiles hello.pas to hello.com
$ pl0 --opt hello.pas  # Does the same with optimizations
```

You can run the resulting `.com` files on a real CP/M machine or in tnylpo. For programs that use VT52 control codes you have to start tnylpo in full-screen mode:

```
$ tnylpo hello     # Run in line-mode
$ tnylpo -s hello  # Run in b/w full-screen mode
$ tnylpo -soy,4,0  # Run in full-screen mode with (Spectrum Next) colors
```

There is a folder containing examples and a folder containing tests for the compiler. The main test suite `all.pas` needs to be compiled with optimizations because of its size.
