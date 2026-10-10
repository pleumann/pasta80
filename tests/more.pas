program More;

(* Additional regression tests that currently do not fit into core.pas. The
   CP/M build of core.pas sits about 70 bytes below the ceiling, and CP/M has
   no overlays to fall back on, so these cases live here until there is room
   again -- through overlays loaded from disk, code generator improvements or
   smart linking of system.asm.

   Everything below covers a family of defects around subranges, where the
   width of a value and the type it is based on were confused for each other:
   #156 (code generation did not reduce subranges to their base type, and
   unary operators on Byte operands were computed in 8 instead of 16 bit),
   #157 (SizeOf reported the base type's size), #158 (the byte-sized storage
   optimization ignored the lower bound) and #159 (assignment stored with the
   base type's width). *)

const
  NegLo = -100;

type
  Small  = 10 .. 20;     { one byte wide }
  Wide   = 0 .. 30000;   { two bytes wide }
  Letter = 'a' .. 'z';   { one byte wide }
  Signed = NegLo .. 100; { needs two bytes because of the lower bound }

var
  Guard1: Integer;
  S:      Small;         { deliberately framed by the guards }
  Guard2: Integer;
  W:      Wide;
  Ch:     Letter;
  Neg:    Signed;
  B:      Byte;
  I:      Integer;

(* Unary minus used to be rejected outright for subranges ("Invalid type"),
   and not only complemented the low byte. Both now reduce the subrange to
   its base type first, so everything integral is computed in 16 bit. *)
procedure TestUnaryOnSubranges;
begin
  WriteLn('--- TestUnaryOnSubranges ---');

  S := 12;
  I := -S;
  Assert(I = -12);
  I := not S;
  Assert(I = -13);

  W := 300;
  I := -W;
  Assert(I = -300);
  I := not W;
  Assert(I = -301);
end;

(* Inc and Dec used to reject subranges ("Ordinal or pointer type expected").
   They now pick the 8 or 16 bit code path by the size of the variable, which
   is what matters: a subrange carries Integer as its base type but may well
   occupy a single byte. *)
procedure TestIncDecOnSubranges;
begin
  WriteLn('--- TestIncDecOnSubranges ---');

  S := 12;
  Inc(S);
  Assert(S = 13);
  Dec(S);
  Assert(S = 12);
  Inc(S, 5);
  Assert(S = 17);
  Dec(S, 7);
  Assert(S = 10);

  W := 255;
  Inc(W);
  Assert(W = 256);
  Dec(W);
  Assert(W = 255);
  Inc(W, 1000);
  Assert(W = 1255);
  Dec(W, 1255);
  Assert(W = 0);
end;

(* not on a Byte variable used to complement the low byte only, yielding 254
   for B = 1. Turbo Pascal 3 computes this in 16 bit and returns -2; the
   truncation to a byte happens on assignment, not in the expression. *)
procedure TestNotOnByte;
begin
  WriteLn('--- TestNotOnByte ---');

  B := 1;
  Assert(not B = -2);
  B := not B;
  Assert(B = 254);

  B := 0;
  Assert(not B = -1);

  B := 255;
  Assert(not B = -256);
end;

(* SizeOf used to report the size of the base type, so a byte sized subrange
   claimed to be two bytes wide -- enough for FillChar to run past the end of
   the variable. Issue #157. *)
procedure TestSizeOfSubranges;
begin
  WriteLn('--- TestSizeOfSubranges ---');

  Assert(SizeOf(S) = 1);
  Assert(SizeOf(W) = 2);
  Assert(SizeOf(Ch) = 1);
  Assert(SizeOf(Neg) = 2);
end;

(* A subrange was stored in a single byte whenever its upper bound fit into
   one, no matter what the lower bound was, so negative values came back
   zero-extended: -100 read as 156. Issue #158. *)
procedure TestNegativeSubrange;
begin
  WriteLn('--- TestNegativeSubrange ---');

  Neg := -100;
  I := Neg;
  Assert(I = -100);

  Neg := 0;
  I := Neg;
  Assert(I = 0);

  Neg := 100;
  I := Neg;
  Assert(I = 100);
end;

(* Negative literals as bounds, e.g. ARRAY[-1..2], used to be rejected with
   "Expected Identifier, but got -". *)
procedure TestNegativeLiteralBounds;
var
  A: array[-1..2] of Integer;
  N: -5 .. -1;
begin
  WriteLn('--- TestNegativeLiteralBounds ---');

  A[-1] := 7;
  A[2] := 9;
  Assert(A[-1] = 7);
  Assert(A[2] = 9);
  Assert(SizeOf(A) = 8);

  N := -5;
  I := N;
  Assert(I = -5);
end;

(* Assigning to a byte sized subrange used to write two bytes and clobber
   whatever followed the variable, because the assignment stored with the type
   TypeCheck returns -- the reduced base type -- instead of the type of the
   target variable. Issue #159. *)
procedure TestStoreWidth;
begin
  WriteLn('--- TestStoreWidth ---');

  Guard1 := 1111;
  Guard2 := 2222;

  S := 15;

  Assert(S = 15);
  Assert(Guard1 = 1111);
  Assert(Guard2 = 2222);
end;

(* A for loop over a Byte variable used to compare the byte sized loop
   variable against the 16 bit final value in every step. With a final value
   outside 0..255 the exit condition could never be met and the loop ran
   forever. Issue #162. Like Turbo Pascal 3, the loop now computes the number
   of iterations up front, in 16 bit and from the untruncated start value, and
   lets the loop variable wrap around. The expected values were checked on a
   real TP3. Issue #163. *)
procedure TestForByteLimit;
var
  Count: Byte;
  Last, Iterations: Integer;
begin
  WriteLn('--- TestForByteLimit ---');

  I := -1;
  W := 0;
  for B := 3 downto I do
    W := W + 1;
  Assert(W = 5);
  Assert(B = 255);

  I := 300;
  W := 0;
  for B := 250 to I do
    W := W + 1;
  Assert(W = 51);
  Assert(B = 44);

  W := 0;
  for B := 0 to 260 do
    W := W + 1;
  Assert(W = 261);

  Count := 0;
  W := 0;
  for B := Count - 1 downto 0 do
    W := W + 1;
  Assert(W = 0);

  (* 65536 iterations, one more than fits into the 16 bit count. *)
  Iterations := 0;
  Last := 0;
  for I := -32768 to 32767 do
  begin
    Iterations := Iterations + 1;
    Last := I;
  end;
  Assert(Iterations = 0);
  Assert(Last = 32767);
  Assert(I = 32767);
end;

(* A String or Real constant declared as an alias of another constant used to
   fail to compile, because the alias did not get the tag (the address of the
   value) of the original constant. Issue #172. *)
procedure TestConstAliases;
const
  A = 'Hello';  B = A;
  R = 3.5;      T = R;
begin
  WriteLn('--- TestConstAliases ---');

  Assert(B = 'Hello');
  Assert(Length(B) = 5);
  Assert(T = 3.5);
  Assert(T = R);
end;

(* Include directive including some edge cases that used to fail (#173). *)
procedure TestIncludeDirective;
var
  I: Integer;
begin
  WriteLn('--- TestIncludeDirective ---');

  I := 0;
  {$I more.inc}
  Assert(I = 1);
  {$i more.inc}
  Assert(I = 2);

  // An include directive followed by more text on the same line used to break,
  // because the scanner read the next character before opening the include.
  // A comment swallowed the whole include file, anything else was glued to its
  // first token.

  {$I more.inc}{ comment right after the include }
  Assert(I = 3);
  {$I more.inc}(* comment right after the include *)
  Assert(I = 4);
  (*$I more.inc*){ comment right after the include }
  Assert(I = 5);
  {$I more.inc}I := I + 4;
  Assert(I = 10);
end;

begin
  TestUnaryOnSubranges;
  TestIncDecOnSubranges;
  TestNotOnByte;
  TestSizeOfSubranges;
  TestNegativeSubrange;
  TestNegativeLiteralBounds;
  TestStoreWidth;
  TestForByteLimit;
  TestConstAliases;
  TestIncludeDirective;

  WriteLn;
  WriteLn('************************');
  WriteLn('Passed assertions: ', AssertPassed);
  WriteLn('Failed assertions: ', AssertFailed);
  WriteLn('************************');
  WriteLn;

  {$ifdef SYS_AGON}
    inline($3e / $00 / $d3 / $00);
  {$endif}
  {$ifdef SYS_ZXNEXT}
    QuitEmulator;
  {$endif}
  {$ifdef SYS_ZX128}
    inline($3e / $00 / $d3 / $1f);
  {$endif}

end.
