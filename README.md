# PL80 - PL/0 for Z80

This is a simple compiler for the (PL/0)[https://en.wikipedia.org/wiki/PL/0] programming language.

PL/0 is a "toy" programming language created by (Niklaus Wirth)[http://people.inf.ethz.ch/wirth/index.html] (inventor of (Pascal)[https://en.wikipedia.org/wiki/Pascal_(programming_language)], (Modula)[https://en.wikipedia.org/wiki/Modula-2] and (Oberon)[https://en.wikipedia.org/wiki/Oberon_(programming_language)]) with the sole purpose of teaching compiler construction. It is featured in his 1984 book "Compilerbau" and has been used since in many compiler construction courses around the world. PL/0 resembles Pascal, but is reduced to a small set of core features. Its only data type (at least in the pure form) is a signed 16-bit integer.

This implementation targets the (Z80)[https://en.wikipedia.org/wiki/Zilog_Z80], a 1970s microprocessor developed by (Zilog)[https://www.zilog.com] that is found in many classic 8-bit home computers made by Sinclair, Amstrad and others. The compiler is currently able to generate code for CP/M (.com files) and the ZX Spectrum Next (.dot files). It relies on zasm as an external assembler in both cases. For easy testing of CP/M binaries the tnylpo CP/M simulator is included in the bin folder.

## How to write a PL0 program

Use an editor of your choice. The full syntax for PL/0 is on the Wikipedia page (or in the original book). The following example demonstrates most of the functionality of PL/0. It prints the first n Fibonacci numbers.

```
(* Fibonacci numbers *)

var
  A, B, I, J;

procedure fibonacci;
var
  C;
begin
  ! A;
  C := A + B;
  A := B;
  B := C;
  I := I + 1;
end;

begin
  A := 0;
  B := 1;
  I := 1;
  
  ! "How many Fibonacci numbers do you want?"
  ? J;
  while I < J do
    call fibonacci;
end.
```

To be a bit more useful the ! statement (that prints integers to the console) was extended to also accept string literals.

## How to build the compiler

TBD

## How to use the compiler

For a PL0 program named ```fibo.pl0``` you would execute this command:

```pl0 fibo```

The ```.pl0``` suffix will be added automatically if there is none. You should see output similar to this if no compiler error occur:

```
```

TBD

### How to run the compiled program

TBD
