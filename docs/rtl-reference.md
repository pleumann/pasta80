# PASTA/80 Run-Time Library Reference

This reference describes all constants, types, variables, procedures and functions of the PASTA/80 run-time library. Platform-specific symbols carry the appropriate tag:

| Tag | Platform |
|-----|----------|
| **[All]** | Included on every target platform. |
| **[CPM]** | CP/M |
| **[ZX48]** | ZX Spectrum 48K, and 128K, and Next |
| **[ZX128]** | ZX Spectrum 128K only |
| **[ZXNext]** | ZX Spectrum Next only |
| **[Agon]** | Agon / Console8 |

Note that * indicates "magic" symbols built into the compiler that are not defined in any `.pas` source file.

---

## Constants

| Name | Type | Value | Platform | Description |
|------|------|-------|----------|-------------|
| `False` | `Boolean` | `0` | [All*] | Boolean value "false". |
| `True` | `Boolean` | `1` | [All*] | Boolean value "true". |
| `MaxInt` | `Integer` | `32767` | [All] | Largest representable `Integer` value. |
| `MinInt` | `Integer` | `-32768` | [All] | Smallest representable `Integer` value. |
| `Black` | `Integer` | Depends | [ZX48] [Agon] | Colour constant black. ZX attribute number: `0`; same on Agon. |
| `Blue` | `Integer` | Depends | [ZX48] [Agon] | Colour constant blue. ZX: `1`; Agon: `4`. |
| `Red` | `Integer` | Depends | [ZX48] [Agon] | Colour constant red. ZX: `2`; Agon: `1`. |
| `Magenta` | `Integer` | Depends | [ZX48] [Agon] | Colour constant magenta. ZX: `3`; Agon: `5`. |
| `Green` | `Integer` | Depends | [ZX48] [Agon] | Colour constant green. ZX: `4`; Agon: `2`. |
| `Cyan` | `Integer` | Depends | [ZX48] [Agon] | Colour constant cyan. ZX: `5`; Agon: `6`. |
| `Yellow` | `Integer` | Depends | [ZX48] [Agon] | Colour constant yellow. ZX: `6`; Agon: `3`. |
| `White` | `Integer` | Depends | [ZX48] [Agon] | Colour constant white. ZX: `7`; same on Agon. |
| `ScreenWidth` | `Integer` | Depends | [ZX48] [CPM] [Agon] | Default screen width in characters. ZX: `32`; CP/M and Agon: `80`. |
| `ScreenHeight` | `Integer` | Depends | [ZX48] [CPM] [Agon] | Default screen height in characters. ZX: `22`; CP/M and Agon: `24`. |
| `LineBreak` | `String` | Depends | [ZX48] [CPM] [Agon] | Line-break convention. ZX: `#13`; CP/M and Agon: `#13#10`. |

---

## Types

| Signature | Platform | Description |
|-----------|----------|-------------|
| `Integer` | [All*] | Signed 16-bit integer (`-32768`..`32767`). |
| `Boolean` | [All*] | Boolean type; enumeration with values `False` and `True`. |
| `Char` | [All*] | 8-bit character (`#0`..`#255`). |
| `Byte` | [All*] | Unsigned 8-bit integer (`0`..`255`). |
| `String` | [All*] | Pascal string; a length byte followed by up to 255 characters. |
| `Real` | [All*] | 6-byte floating-point number in Turbo Pascal format. |
| `Pointer` | [All*] | Untyped 16-bit pointer. |
| `File` | [All*] | Untyped file. |
| `Text` | [All*] | Text file. |

---

## Variables

| Signature | Platform | Description |
|-----------|----------|-------------|
| `Mem[I: Integer]: Byte` | [All*] | Byte-addressable read/write access to the entire Z80 address space. |
| `Port[I: Integer]: Byte` | [All*] | Byte-addressable read/write access to the Z80 I/O address space. |
| `AssertPassed: Integer` | [All] | Counts the number of `Assert` calls that passed. |
| `AssertFailed: Integer` | [All] | Counts the number of `Assert` calls that failed. |

---

## Procedures

### System Procedures

| Signature | Platform | Description |
|-----------|----------|-------------|
| `Assert(B: Boolean)` | [All*] | Evaluates `B`. If the assertion fails, the source file and line number are printed and the program is terminated. In release mode the code is removed entirely. |
| `Debug` | [All*] | Sets a debugger breakpoint (platform-specific encoding). Optional form: `Debug(B: Boolean)` sets the breakpoint only when `B` is true. In release mode these are removed entirely. |
| `Break` | [All*] | Exits the innermost `for`, `while` or `repeat` loop. |
| `Continue` | [All*] | Jumps to the next iteration of the innermost loop. |
| `Exit` | [All*] | Leaves the current procedure or function. In functions also usable as `Exit(Value)` to return a value. |
| `Halt` | [All*] | Terminates the program. Optional form: `Halt(ExitCode: Byte)`. |
| `New(var P: Pointer)` | [All*] | Allocates heap memory for the type that `P` points to and stores the address in `P`. |
| `Dispose(P: Pointer)` | [All*] | Frees the memory pointed to by `P` and returns it to the heap. |
| `Inc(var V [; N: Integer])` | [All*] | Increments `V` by 1 or by `N`. Valid for `Integer`, `Byte`, `Char` and enumeration types. |
| `Dec(var V [; N: Integer])` | [All*] | Decrements `V` by 1 or by `N`. Valid for `Integer`, `Byte`, `Char` and enumeration types. |
| `Val(S: String; var Scalar; var E: Integer)` | [All*] | Converts the string `S` to a number or enumeration value. `E` is 0 on success, otherwise the error position. |
| `Str(N: Scalar; var S: String)` | [All*] | Converts `N` to a string and stores the result in `S`. Optional format specifiers: `Str(N:Width, S)` or `Str(N:Width:Decimals, S)`. |
| `Include(var S: Set; E: Element)` | [All*] | Adds element `E` to set `S`. |
| `Exclude(var S: Set; E: Element)` | [All*] | Removes element `E` from set `S`. |
| `FillChar(var Dest; Length: Integer; Data)` | [All*] | Fills `Length` bytes starting at `Dest` with the value `Data`. |
| `Move(var Source, Dest; Count: Integer)` | [All] | Copies `Count` bytes from `Source` to `Dest`. |
| `GetMem(var P: Pointer; Size: Integer)` | [All] | Low-level heap allocation: reserves `Size` bytes and stores the address in `P`. |
| `FreeMem(P: Pointer; Size: Integer)` | [All] | Low-level heap deallocation: returns `Size` bytes at address `P` to the free list. |
| `Randomize` | [All] | Seeds the random-number generator from the Z80 R register. |

### Input / Output

| Signature | Platform | Description |
|-----------|----------|-------------|
| `Read(...)` | [All*] | Reads values from the console or from an open file. Syntax: `Read(Var)` or `Read(TextFile, Var1, Var2, ...)`. |
| `ReadLn(...)` | [All*] | Like `Read`, but discards the rest of the input line after reading. |
| `Write(...)` | [All*] | Writes values to the console or to an open file. Supports format specifiers: `Write(Value:Width)` and `Write(Value:Width:Decimals)`. |
| `WriteLn(...)` | [All*] | Like `Write`, but appends a line break afterwards. |

### File Operations

Note: Unless noted otherwise, `F` can be either an untyped `File`, a typed `file of`, or a `Text` file.

| Signature | Platform | Description |
|-----------|----------|-------------|
| `Assign(var F; Name: String)` | [All*] | Associates the file variable `F` with the file name `Name`. |
| `Reset(var F)` | [All*] | Opens file `F` for reading (and for writing in the case of untyped files). |
| `Rewrite(var F)` | [All*] | Creates or truncates file `F` and opens it for writing. |
| `Append(var F)` | [All*] | Opens file `F` for appending at the end. |
| `Close(var F)` | [All*] | Closes file `F` and flushes any pending buffer data. |
| `Flush(var F)` | [All*] | Writes any pending buffer data of file `F` to the medium. |
| `Seek(var F; I: Integer)` | [All*] | Sets the current position in typed or untyped file `F` to the record at index `I`. |
| `Erase(var F)` | [All*] | Deletes the file associated with file variable `F` from the storage medium. |
| `Rename(var F; NewName: String)` | [All*] | Renames the file associated with file variable `F` to `NewName`. |
| `BlockRead(var F: File; var Buf; Count: Integer [; var Actual: Integer])` | [All*] | Reads `Count` 128-byte blocks from untyped file `F` into `Buf`. The optional `Actual` receives the number actually read. |
| `BlockWrite(var F: File; var Buf; Count: Integer [; var Actual: Integer])` | [All*] | Writes `Count` 128-byte blocks from `Buf` to untyped file `F`. |

### Screen and Cursor

| Signature | Platform | Description |
|-----------|----------|-------------|
| `ClrScr` | [ZX48] [CPM] [Agon] | Clears the screen using the most recently set foreground and background attributes. |
| `GotoXY(X, Y: Integer)` | [ZX48] [CPM] [Agon] | Moves the cursor to column `X` (0-based) and row `Y` (0-based). |
| `TextColor(Color: Integer)` | [ZX48] [CPM] [Agon] | Sets the foreground colour (0..7). |
| `TextBackground(Color: Integer)` | [ZX48] [CPM] [Agon] | Sets the background colour (0..7). |
| `CursorOn` | [CPM] [Agon] | Shows the cursor. |
| `CursorOff` | [CPM] [Agon] | Hides the cursor. |
| `ClrEol` | [CPM] [Agon] | Clears from the cursor to the end of the current line. |
| `ClrEos` | [CPM] | Clears from the cursor to the end of the screen. |
| `InsLine` | [CPM] | Inserts a blank line at the cursor position and scrolls everything below down. |
| `DelLine` | [CPM] | Deletes the line at the cursor position and scrolls everything below up. |
| `HighVideo` | [CPM] [Agon] | Switches to high-intensity / reverse video. |
| `LowVideo` | [CPM] [Agon] | Switches to low-intensity video. |
| `NormVideo` | [CPM] [Agon] | Restores normal video intensity. |
| `Border(Color: Integer)` | [ZX48] | Sets the ZX Spectrum border colour (0..7) via the ROM routine. |

### Graphics

| Signature | Platform | Description |
|-----------|----------|-------------|
| `Plot(X, Y: Byte)` | [ZX48] [Agon] | Sets a pixel at coordinates `(X, Y)`. The origin (0, 0) is in the upper-left corner. |
| `Draw(DX, DY: Integer)` | [ZX48] [Agon] | Draws a line from the last plot position by the relative offset `(DX, DY)`. |
| `Circle(CX, CY, Radius: Integer)` | [ZX48] [Agon] | Draws a circle with centre `(CX, CY)` and the given radius. |
| `FloodFill(X, Y: Integer)` | [ZX48] | Performs a recursive flood-fill operation starting at `(X, Y)`. |

### Sound

| Signature | Platform | Description |
|-----------|----------|-------------|
| `Sound(Frequency, Duration: Integer)` | [ZX48] [Agon] | Plays a tone at the given frequency (Hz) for the given duration (ms). Synchronous; requires CPU speed 3.5 MHz. |
| `Beep(Duration: Real; Pitch: Integer)` | [ZX48] [Agon] | Plays a note. `Duration` is in seconds; `Pitch` is the piano key number relative to middle C (0 = middle C, like MIDI note 60). Synchronous. |

### Timing

| Signature | Platform | Description |
|-----------|----------|-------------|
| `Delay(Duration: Integer)` | [ZX48] [CPM] [Agon] | Waits approximately `Duration` milliseconds. On the ZX Spectrum the resolution is a multiple of 20 ms (50 Hz interrupt). On the Agon approximately 17 ms (60 Hz refresh). Does not work when interrupts are disabled. |

### Memory Management (ZX Spectrum 128K / Next)

| Signature | Platform | Description |
|-----------|----------|-------------|
| `SelectBank(Bank: Byte)` | [ZX128] | Pages 128K RAM bank `Bank` into the slot at `$C000`. |
| `SetMemPage(Slot, Page: Byte)` | [ZXNext] | Maps memory page `Page` to 8K slot `Slot` (0..7). Pages range from 0..111 (1 MB RAM) or 0..223 (2 MB RAM). |

### Next Registers

| Signature | Platform | Description |
|-----------|----------|-------------|
| `SetNextReg(Number, Value: Integer)` | [ZXNext] | Writes `Value` to ZX Spectrum Next register `Number`. |
| `SetCpuSpeed(Value: Integer)` | [ZXNext] | Sets the CPU speed: 0 = 3.5 MHz, 1 = 7 MHz, 2 = 14 MHz, 3 = 28 MHz. |

### esxDOS / MOS (Low Level)

| Signature | Platform | Description |
|-----------|----------|-------------|
| `EsxDos(I: Integer; var R: Registers): Byte` | [ZXNext] | Executes esxDOS call number `I`. Arguments and return values are passed via the `Registers` structure. |
| `MosApi(I: Integer; var R: Registers): Byte` | [Agon] | Executes MOS API call number `I`. |

### String Operations

| Signature | Platform | Description |
|-----------|----------|-------------|
| `Delete(var S: String; Start, Count: Integer)` | [All] | Deletes `Count` characters from string `S` starting at position `Start` (1-based). |
| `Insert(S: String; var T: String; Start: Integer)` | [All] | Inserts string `S` into string `T` at position `Start`. |

---

## Functions

### Mathematical Functions

| Signature | Platform | Description |
|-----------|----------|-------------|
| `Abs(I: Integer): Integer` | [All*] | Returns the absolute value of an integer. |
| `Abs(R: Real): Real` | [All*] | Returns the absolute value of a real number. |
| `ArcTan(R: Real): Real` | [All] | Arctangent of `R` (result in radians). |
| `Cos(R: Real): Real` | [All] | Cosine of `R` (radians). |
| `Exp(R: Real): Real` | [All] | Exponential function e^R. |
| `Frac(R: Real): Real` | [All] | Fractional part of `R`. |
| `Int(R: Real): Real` | [All] | Integer part of `R` (returned as `Real`). |
| `Ln(R: Real): Real` | [All] | Natural logarithm of `R`. |
| `Log(R: Real): Real` | [All] | Common logarithm (base 10) of `R`. |
| `Sin(R: Real): Real` | [All] | Sine of `R` (radians). |
| `Sqr(R: Real): Real` | [All] | Square of `R` (R²). |
| `Sqrt(R: Real): Real` | [All] | Square root of `R`. |
| `Tan(R: Real): Real` | [All] | Tangent of `R` (radians). |
| `Pi: Real` | [All] | Returns the constant π. |
| `MaxReal: Real` | [All] | Returns the largest representable floating-point number. |
| `MinReal: Real` | [All] | Returns the smallest representable floating-point number (largest negative value). |
| `Random(Range: Integer): Integer` | [All] | Returns a pseudo-random integer in the range 0..`Range`-1. |
| `RandomReal: Real` | [All] | Returns a uniformly distributed pseudo-random real number in the range 0.0..1.0. |
| `Round(R: Real): Integer` | [All] | Rounds `R` to the nearest integer and returns it as `Integer`. |
| `Trunc(R: Real): Integer` | [All] | Truncates the fractional part of `R` (towards zero) and returns the result as `Integer`. |

### Ordinal Functions

| Signature | Platform | Description |
|-----------|----------|-------------|
| `Odd(Ordinal): Boolean` | [All*] | Returns `True` if the ordinal value is odd. |
| `Even(Ordinal): Boolean` | [All*] | Returns `True` if the ordinal value is even. |
| `Ord(Ordinal): Integer` | [All*] | Returns the underlying integer value of an ordinal expression. |
| `Pred(Ordinal): Ordinal` | [All*] | Returns the predecessor of an ordinal value. |
| `Succ(Ordinal): Ordinal` | [All*] | Returns the successor of an ordinal value. |
| `Chr(B: Byte): Char` | [All] | Converts byte value `B` to a `Char`. |
| `High(X): Ordinal` | [All*] | Returns the highest index value of an array or set. |
| `Low(X): Ordinal` | [All*] | Returns the lowest index value of an array or set. |

### Bit and Byte Operations

| Signature | Platform | Description |
|-----------|----------|-------------|
| `Hi(I: Integer): Byte` | [All] | Returns the high byte of a 16-bit integer. |
| `Lo(I: Integer): Byte` | [All] | Returns the low byte of a 16-bit integer. |
| `Swap(I: Integer): Integer` | [All] | Swaps the high and low bytes of `I`. |

### Character Operations

| Signature | Platform | Description |
|-----------|----------|-------------|
| `UpCase(C: Char): Char` | [All] | Converts a lowercase letter (`a`..`z`) to the corresponding uppercase letter; other characters are unchanged. |
| `LoCase(C: Char): Char` | [All] | Converts an uppercase letter (`A`..`Z`) to the corresponding lowercase letter; other characters are unchanged. |

### Pointer and Memory

| Signature | Platform | Description |
|-----------|----------|-------------|
| `Addr(XYZ): Integer` | [All*] | Returns the memory address of variable or routine `XYZ` as an `Integer`. |
| `Ptr(I: Integer): Pointer` | [All*] | Converts integer value `I` to an untyped `Pointer`. |
| `SizeOf(XYZ): Integer` | [All*] | Returns the size in bytes of the type or variable `XYZ`. Evaluated at compile time. |
| `MemAvail: Integer` | [All] | Returns the total number of free bytes on the heap (sum of all free blocks). |
| `MaxAvail: Integer` | [All] | Returns the size of the largest contiguous free block on the heap. |

### String Functions

| Signature | Platform | Description |
|-----------|----------|-------------|
| `Concat(S: String; ...): String` | [All*] | Concatenates two or more strings and returns the result. |
| `Copy(S: String; Start, Count: Integer): String` | [All] | Returns `Count` characters from string `S` starting at position `Start` (1-based). |
| `Length(S: String): Integer` | [All] | Returns the current length of string `S`. |
| `Pos(S, T: String): Integer` | [All] | Searches for substring `S` in string `T` and returns the 1-based start position; `0` if not found. |

### File Functions

Note: Unless noted otherwise, `F` can be either an untyped `File`, a typed `file of`, or a `Text` file.

| Signature | Platform | Description |
|-----------|----------|-------------|
| `Eof(var F): Boolean` | [All*] | Returns `True` if the end of file has been reached or an error occurred. |
| `Eoln(var F: Text): Boolean` | [All*] | Returns `True` if the current character is the end-of-line marker. |
| `SeekEof(var F: Text): Boolean` | [All*] | Skips spaces and line breaks and returns `True` if end of file follows. |
| `SeekEoln(var F: Text): Boolean` | [All*] | Skips spaces and tabs and returns `True` if end of line or end of file follows. |
| `FilePos(var F): Integer` | [All*] | Returns the current file position (record index, 0-based). `F` must by either typed or untyped. |
| `FileSize(var F: File): Integer` | [All*] | Returns the total number of records in the file. `F` must by either typed or untyped. |
| `IOResult: Byte` | [CPM] [ZXNext] [Agon] | Returns `LastError` and resets it to `0` so that further file operations can proceed. |

### Keyboard and Timing

| Signature | Platform | Description |
|-----------|----------|-------------|
| `KeyPressed: Boolean` | [ZX48] [CPM] [Agon] | Returns `True` if a key has been pressed and `ReadKey` can be called without blocking. |
| `ReadKey: Char` | [ZX48] [CPM] [Agon] | Reads a key press and returns the corresponding ASCII character. Waits for a key press if none is pending. |
| `Frames: Real` | [ZX48] [Agon] | Returns the current value of the 24-bit frame counter. On the ZX Spectrum it runs at 50 Hz (multiply by 20 to get milliseconds). On the Agon at 60 Hz (multiply by 17 for an approximate millisecond value). |

### Graphics Queries

| Signature | Platform | Description |
|-----------|----------|-------------|
| `Point(X, Y: Byte): Boolean` | [ZX48] [Agon] | Returns `True` if the pixel at `(X, Y)` is set. |

### Next Register Reads

| Signature | Platform | Description |
|-----------|----------|-------------|
| `GetNextReg(Number: Integer): Integer` | [ZXNext] | Reads the current value of ZX Spectrum Next register `Number`. |
| `GetCpuSpeed: Integer` | [ZXNext] | Returns the current CPU speed: 0 = 3.5 MHz, 1 = 7 MHz, 2 = 14 MHz, 3 = 28 MHz. |
| `GetMemPage(Slot: Byte): Byte` | [ZXNext] | Returns the 8K memory page currently mapped to slot `Slot` (0..7). |


### CP/M / Agon OS Interface

| Signature | Platform | Description |
|-----------|----------|-------------|
| `Bdos(Func: Integer [; Param: Integer]): Byte` | [CPM] [Agon] | Executes a BDOS/MOS system call and returns the result in register A as a `Byte`. If `Param` is omitted, 0 is passed. |
| `BdosHL(Func: Integer [; Param: Integer]): Integer` | [CPM] [Agon] | Like `Bdos`, but returns the full HL register value as an `Integer`. |
| `ParamCount: Byte` | [CPM] [Agon] | Returns the number of command-line parameters. |
| `ParamStr(I: Byte): String` | [CPM] [Agon] | Returns the `I`-th command-line parameter, or an empty string if `I` is out of range. |

## Internal

These identifiers are part of the runtime infrastructure. They may be visible in user code but are not intended for direct use.

### Types

| Signature | Platform | Description |
|-----------|----------|--------------|
| `PBlock = ^TBlock` | [All] | Pointer to a free heap block. |
| `TBlock = record Next: PBlock; Size: Integer end` | [All] | Entry in the heap free list. `Next` points to the next block; `Size` gives its size in bytes. |
| `Registers` | [ZXNext] [Agon] | Helper type for passing Z80 registers to esxDOS / MOS calls. Variant with byte fields (`F, A, C, B, E, D, L, H`) and integer fields (`AF, BC, DE, HL`). |
| `FileControlBlock` | [CPM] [ZXNext] [Agon] | Internal file control block. CP/M: drive number, filename, extension and CP/M-internal fields. Next/Agon: handle, null-terminated filename and record position. |
| `TextRec` | [File] | Internal representation of a text file. Contains a `FileControlBlock`, status flags and a 128-byte sector buffer. |
| `FileRec` | [File] | Internal representation of a typed file. Contains a `FileControlBlock`, component size and count, and a 128-byte sector buffer. |

### Variables

| Signature | Platform | Description |
|-----------|----------|--------------|
| `HeapPtr: PBlock` | [All] | Points to the first block of the heap free list; `nil` when the heap is empty. |
| `LastError: Byte` | [CPM] [ZXNext] [Agon] | Last file-system error code; `0` means no error. Most file operations are skipped while this value is non-zero. |
| `RandSeed1: Integer` | [All] | First 16-bit half of the random-number generator seed. |
| `RandSeed2: Integer` | [All] | Second 16-bit half of the random-number generator seed. |

### Procedures

| Signature | Platform | Description |
|-----------|----------|--------------|
| `InitHeap` | [All] | Initialises the heap between the end of the program and the start of the stack area. Called automatically by the compiler. |
| `CheckBreak` | [All] | Tests whether the user tried to interrupt the program and terminates i program if so. The actual key combination depends on the platform: Break+Space on ZX machine, Ctrl-C elsewhere. Inserted automatically by the compiler in appropriate places when `{$u+}` is active. |
| `CheckStack` | [All] | Checks for stack overflow. Inserted automatically by the compiler at the start of every procedure/function when `{$k+}` is active. |
| `BDosThrow` | [CPM] [ZXNext] [Agon] | Checks `LastError` and terminates the program with an error message if the value is non-zero. Inserted automatically by the compiler after file operations in `{$i+}` mode. |
| `BDosCatch(Func: Byte; Param: Integer)` | [CPM] | Executes a BDOS call and stores a non-zero return value in `LastError`. |
| `ConOut(C: Char)` | [CPM] [Agon] | Sends character `C` directly to the console driver (BDOS call 2 / VDP). |
| `MOSAPISeek(var R: Registers): Byte` | [Agon] | Specialised variant of the MOS API call for seek operations. |

### Functions

| Signature | Platform | Description |
|-----------|----------|--------------|
| `MOSAPILength(var R: Registers): Integer` | [Agon] | Returns the length of a file opened via MOS. |
