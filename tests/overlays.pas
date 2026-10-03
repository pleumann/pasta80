program Overlays;

(* Tests for overlays and far calls. The procedures below are spread over
   several overlays (separated by type declarations) and call each other in
   various ways. Every call appends to a log string, which is then compared
   against the expected sequence. Since overlays can only be tested by
   observation, the main criterion is that everything comes out right and
   that the program doesn't crash.

   Without --ovr (or on CP/M) the overlay markers are ignored, so the tests
   must pass there, too. Local variables live on the stack ($a-), so they
   survive calls into other overlays and recursion. *)

{$a-}

var
  Trace: String;

(* --- Root (non-overlay) code ---------------------------------------------- *)

procedure Log(S: String);
begin
  Trace := Trace + S;
end;

procedure Check(Expected: String);
begin
  if Trace <> Expected then
    WriteLn('Expected "', Expected, '", got "', Trace, '"');
  Assert(Trace = Expected);
  Trace := '';
end;

(* Forward declarations of root routines that are called from overlays and
   call other overlays themselves. Note that a forward declaration and its
   body must be in the same overlay (or both in root code), so mutual
   recursion between overlays has to go through root code. *)
procedure RootToB; forward;
procedure RootRecB(N: Integer; var Sum: Integer); forward;

(* --- Overlay 0 ------------------------------------------------------------ *)

overlay procedure B(I: Integer);
begin
  Log('B' + Chr(48 + I));
end;

overlay function FB(I: Integer): Integer;
begin
  Log('b');
  FB := I * 2;
end;

overlay procedure VarB(var I: Integer);
begin
  Log('v');
  I := I + 100;
end;

type
  Separator1 = Integer;

(* --- Overlay 1 ------------------------------------------------------------ *)

overlay procedure A(I: Integer);
begin
  Log('A' + Chr(48 + I));
  B(I);
  Log('a' + Chr(48 + I));
end;

overlay procedure ANoCall(I: Integer);
begin
  Log('N' + Chr(48 + I));
end;

overlay function FA(I: Integer): Integer;
var
  L: Integer;
begin
  L := I + 1;                 (* Local must survive the far call. *)
  FA := FB(L) + L;
  Log('f');
end;

overlay procedure VarA;
var
  L: Integer;
begin
  L := 5;
  VarB(L);
  Assert(L = 105);
  Log('w');
end;

overlay procedure RecA(N: Integer; var Sum: Integer);
begin
  Inc(Sum, N);
  if N > 0 then RootRecB(N - 1, Sum);
end;

overlay procedure NestedA;
var
  L: Integer;

  procedure Inner;
  begin
    B(7);                     (* Far call from a nested procedure. *)
    L := L + 1;
  end;

begin
  L := 1;
  Inner;
  Inner;
  Assert(L = 3);
  Log('n');
end;

overlay procedure AToRoot;
begin
  Log('<');
  RootToB;                    (* Overlay -> root -> other overlay. *)
  Log('>');
end;

overlay procedure Sibling;
begin
  Log('s');
end;

overlay procedure CallsSibling;
begin
  Log('[');
  B(6);                       (* After returning from another overlay, *)
  Sibling;                    (* our own one must be banked in again.  *)
  Log(']');
end;

overlay procedure Chain1;
begin
  Log('1');
  B(8);
  Log('i');
end;

type
  Separator2 = Integer;

(* --- Overlay 2 ------------------------------------------------------------ *)

overlay procedure RecB(N: Integer; var Sum: Integer);
begin
  Inc(Sum, N * 10);
  if N > 0 then RecA(N - 1, Sum);
end;

overlay function FC(I: Integer): Integer;
begin
  Log('c');
  FC := FA(I) + FB(I);        (* Calls into two other overlays. *)
end;

overlay procedure Chain2;
begin
  Log('2');
  Chain1;
  Log('j');
end;

type
  Separator3 = Integer;

(* --- Overlay 3 ------------------------------------------------------------ *)

overlay procedure Chain3;
begin
  Log('3');
  Chain2;                     (* Overlay 3 -> 2 -> 1 -> 0 and back. *)
  Log('k');
end;

type
  Separator4 = Integer;

(* --- More root code ------------------------------------------------------- *)

procedure RootToB;
begin
  Log('r');
  B(5);
  Log('R');
end;

procedure RootRecB; (* (N: Integer; var Sum: Integer), see forward *)
begin
  RecB(N, Sum);
end;

(* --- Tests ---------------------------------------------------------------- *)

procedure TestSameOverlayTwice;
begin
  WriteLn('--- TestSameOverlayTwice ---');

  (* The second call into A finds A still banked in. A's call into B must
     still switch back to A afterwards. *)
  A(1);
  Check('A1B1a1');
  A(2);
  Check('A2B2a2');
  A(3);
  Check('A3B3a3');
end;

procedure TestPhases;
begin
  WriteLn('--- TestPhases ---');

  B(1);
  B(2);
  Check('B1B2');
  ANoCall(1);
  ANoCall(2);
  A(4);
  ANoCall(3);
  Check('N1N2A4B4a4N3');
  B(3);
  A(5);
  B(4);
  Check('B3A5B5a5B4');
end;

procedure TestFunctions;
var
  I: Integer;
begin
  WriteLn('--- TestFunctions ---');

  I := FA(3);                 (* (3 + 1) * 2 + 4 = 12 *)
  Assert(I = 12);
  Check('bf');

  I := FC(5);                 (* FA(5) + FB(5) = 18 + 10 = 28 *)
  Assert(I = 28);
  Check('cbfb');

  I := FB(FA(1) + FC(2));     (* (6 + (9 + 4)) * 2 = 38 *)
  Assert(I = 38);
  Check('bfcbfbb');
end;

procedure TestVarParams;
var
  I: Integer;
begin
  WriteLn('--- TestVarParams ---');

  VarA;
  Check('vw');

  I := 1;
  VarB(I);
  Assert(I = 101);
  Check('v');
end;

procedure TestRecursion;
var
  Sum: Integer;
begin
  WriteLn('--- TestRecursion ---');

  (* RecA(12) -> RecB(11) -> RecA(10) -> ... -> RecA(0), going through root
     code (RootRecB) on the way from A to B: 13 nested overlay switches (16
     are possible). Sum = (12 + 10 + ... + 0) + 10 * (11 + 9 + ... + 1). *)
  Sum := 0;
  RecA(12, Sum);
  Assert(Sum = 402);

  Sum := 0;
  RecB(5, Sum);               (* 50 + 4 + 30 + 2 + 10 + 0 = 96 *)
  Assert(Sum = 96);
end;

procedure TestNested;
begin
  WriteLn('--- TestNested ---');

  NestedA;
  Check('B7B7n');
  NestedA;
  Check('B7B7n');
end;

procedure TestOverlayRootOverlay;
begin
  WriteLn('--- TestOverlayRootOverlay ---');

  AToRoot;
  Check('<rB5R>');
  AToRoot;
  Check('<rB5R>');
  RootToB;
  Check('rB5R');
end;

procedure TestSibling;
begin
  WriteLn('--- TestSibling ---');

  CallsSibling;
  Check('[B6s]');
  CallsSibling;
  Check('[B6s]');
end;

procedure TestChain;
begin
  WriteLn('--- TestChain ---');

  Chain3;
  Check('321B8ijk');
  Chain3;
  Check('321B8ijk');
  Chain1;
  Chain2;
  Check('1B8i21B8ij');
end;

procedure TestMaxDepth;
var
  Sum: Integer;
begin
  WriteLn('--- TestMaxDepth ---');

  (* The local overlay stack has room for 16 nested overlay switches, one
     more aborts the program. RecA(15) -> RecB(14) -> ... -> RecB(0) are
     exactly 16 (each call switches, the first one from root code counts,
     too). Sum = (15 + 13 + ... + 1) + 10 * (14 + 12 + ... + 0). *)
  Sum := 0;
  RecA(15, Sum);
  Assert(Sum = 624);

  (* Do it again to make sure the stack was unwound completely. *)
  Sum := 0;
  RecA(15, Sum);
  Assert(Sum = 624);
end;

begin
  Trace := '';

  TestSameOverlayTwice;
  TestPhases;
  TestFunctions;
  TestVarParams;
  TestRecursion;
  TestNested;
  TestOverlayRootOverlay;
  TestSibling;
  TestChain;
  TestMaxDepth;

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
