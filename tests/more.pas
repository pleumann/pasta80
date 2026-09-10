program More;

(* Additional regression tests that currently do not fit into all.pas. The
   CP/M build of all.pas sits about 70 bytes below the ceiling, and CP/M has
   no overlays to fall back on, so these cases live here until there is room
   again -- through overlays loaded from disk, code generator improvements or
   smart linking of system.asm.

   Everything below covers issue #156: subranges were not reduced to their
   base type during code generation, and unary operators on Byte operands
   were computed in 8 instead of 16 bit. *)

type
  Small = 10 .. 20;     { one byte wide }
  Wide  = 0 .. 30000;   { two bytes wide }

var
  S: Small;
  W: Wide;
  B: Byte;
  I: Integer;

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

begin
  TestUnaryOnSubranges;
  TestIncDecOnSubranges;
  TestNotOnByte;

  WriteLn;
  WriteLn('************************');
  WriteLn('Passed assertions: ', AssertPassed);
  WriteLn('Failed assertions: ', AssertFailed);
  WriteLn('************************');
  WriteLn;
end.
