program More;

(* Additional regression tests that currently do not fit into all.pas. The
   CP/M build of all.pas sits about 70 bytes below the ceiling, and CP/M has
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

begin
  TestUnaryOnSubranges;
  TestIncDecOnSubranges;
  TestNotOnByte;
  TestSizeOfSubranges;
  TestNegativeSubrange;
  TestStoreWidth;

  WriteLn;
  WriteLn('************************');
  WriteLn('Passed assertions: ', AssertPassed);
  WriteLn('Failed assertions: ', AssertFailed);
  WriteLn('************************');
  WriteLn;
end.
