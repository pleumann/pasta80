program All;

{$a+}

const
  NumberOfTheBeast = 666;

  MyPi = 3.1415;

  NegPi = -3.1415;

  NotTrue = False;

  FortyTwo: Integer = 42;

  NotTrueEither: Boolean = False;

  ThreeByThree: array[0..2] of array[0..2] of Integer = (
    (1, 2, 3),
    (4, 5, 6),
    (7, 8, 9)
  );

  SingleFloat: Real = 1.23456789;

  FloatArray: array[1..5] of Real = (1.0, 2.0, 3.0, 4.0, 5.0);

  Start = 10;

  Stop = 20;

type
  Color = (Red, Green, Blue);

  TIntArray100 = array[0..99] of Integer;

  TPoint = record
    X, Y: Integer;
  end;

  SubRange1 = 10 .. 20;
  SubRange2 = Start .. 20;
  SubRange3 = 10 .. Stop;
  SubRange4 = Start .. Stop;

  Str255 = string[255];

var
  X, Y, Z: Integer;
  B: Boolean;
  C: Char;

  BeforeA: Integer;
  A: TIntArray100;
  AfterA: Integer;

  AA: array[0..9] of array[0..9] of Integer;

  CA: array[0..2] of Color;

  GlobalIntArray: TIntArray100;

(* Overlay 0 *)

overlay procedure TestComment;
var
  I: Integer;
begin
  WriteLn('--- TestComment ---');

  I := 0;
  (* I := 1; *)
  Assert(I = 0);
  (* Left *) I := 1; (* Right *)
  Assert(I = 1);
  I (* Oh *) := (* no! *) 2;
  Assert(I = 2);
  (* This is a nasty one ) * ) I := 3; *)
  Assert(I = 2);
  (* Nested (* comments don't work either. I := 4; *)
  Assert(I = 2);
  (**) I := I + 1; (* *) I := I + 1; (***)
  Assert(I = 4);
  (*
   I := 5;
  *)
  Assert(I = 4);
  { I := 5 }
  Assert(I = 4);
  { (* Different comment types can be nested. *) }
  (* { Different comment types can be nested. } *)
  { *) Different comment types can't be mixed.   }
  (* } Different comment types can't be mixed.  *)

  I := 0;
  // And we support C-style comments, too!
  // Inc(I);
  Inc(I); // They can follow an instruction.
  // They can also be empty, apparently.
  //
  Inc(I);

  Assert(I = 2);
end;

overlay procedure TestConstHelp(Expected: Integer);
const
  Global: Integer = 666;
begin
  Assert(Global = Expected);
  Global := Global + 1;
end;

overlay procedure TestConst;
const
  BinConst = %10101010;
  HexConst = $0F0F;
var
  I, J, BinVar, HexVar: Integer;
begin
  WriteLn('--- TestConst ---');

  Assert(NumberOfTheBeast = 666);
  Assert(not NotTrue);

  Assert(not NotTrueEither);
  NotTrueEither := True;
  Assert(NotTrueEither);

  Assert(FortyTwo = 42);
  FortyTwo := 43;
  Assert(FortyTwo = 43);

  for I := 0 to 2 do
    for J := 0 to 2 do
      Assert(ThreeByThree[I][J] = 1 + I * 3 + J);

  TestConstHelp(666);
  TestConstHelp(667);
  TestConstHelp(668);

  Assert(MyPi = 3.1415);

  Assert(SingleFloat = 1.23456789);

  for I := 1 to 5 do
    Assert(FloatArray[I] = I);

  Assert(BinConst = 170);
  Assert(HexConst = 3855);

  BinVar := %01010101;
  HexVar := $ABCD;

  Assert(BinVar = 85);
  Assert(HexVar = -21555);
end;

overlay procedure TestAdd;
begin
  WriteLn('--- TestAdd ---');

  Assert(0 + 0 = 0);
  Assert(0 + 1 = 1);
  Assert(1 + 0 = 1);
  Assert(1 + 1 = 2);

  Assert(-2 + 1 = -1);
  Assert(-1 + 2 = 1);

  Assert(32767 + 1 = -32768);
end;

overlay procedure TestSubtract;
begin
  WriteLn('--- TestSubtract ---');

  Assert(0 - 0 = 0);
  Assert(1 - 1 = 0);
  Assert(1 - 0 = 1);
  Assert(1 - 1 = 0);

  Assert(2 - 1 = 1);
  Assert(1 - 2 = -1);

  Assert(-32768 - 1 = 32767);
end;

overlay procedure TestMultiply;
begin
  WriteLn('--- TestMultiply ---');

  Assert(0 * 0 = 0);
  Assert(1 * 1 = 1);
  Assert(1 * 0 = 0);
  Assert(0 * 1 = 0);

  Assert(3 * 4 = 12);
  Assert(3 * -4 = -12);
  Assert(-3 * 4 = -12);
  Assert(-3 * -4 = 12);
end;

overlay procedure TestDivide;
begin
  WriteLn('--- TestDivide ---');

  Assert(0 div 1 = 0);
  Assert(1 div 1 = 1);

  Assert(10 div 5 = 2);
  Assert(10 div -5 = -2);
  Assert(-10 div 5 = -2);
  Assert(-10 div -5 = 2);
end;

overlay procedure TestModulus;
begin
  WriteLn('--- TestModulus ---');

  Assert(10 mod 1 = 0);
  Assert(10 mod 2 = 0);
  Assert(10 mod 3 = 1);
  Assert(10 mod 4 = 2);
  Assert(10 mod 10 = 0);
  Assert(10 mod 11 = 10);
end;

overlay procedure TestComplex;
begin
  WriteLn('--- TestComplex ---');

  Assert(4 - 4 - 4 = -4);
  Assert(10 div 2 * 5 = 25);
  Assert(2 * (3 + 4 * (5 - 6 * (7 + 8 * (9 div 3)))) = -1442);
end;

overlay procedure TestShift;
begin
  WriteLn('--- TestShift ---');

  Assert(1 shl 0 = 1);
  Assert(1 shl 1 = 2);
  Assert(1 shl 8 = 256);
  Assert(1 shl 14 = 16384);
  Assert(1 shl 15 = -32768);

  Assert(1 shr 0 = 1);
  Assert(2 shr 1 = 1);
  Assert(256 shr 8 = 1);
  Assert(16384 shr 14 = 1);
  Assert(-32768 shr 15 = 1);
end;

overlay procedure TestRelOpsUnsigned;
begin
  WriteLn('--- TestRelOpsUnsigned ---');

  Assert(0 = 0);
  Assert(0 <= 0);
  Assert(0 >= 0);

  Assert(1 = 1);
  Assert(1 <= 1);
  Assert(1 >= 1);

  Assert(0 < 1);
  Assert(0 <= 1);
  Assert(0 <> 1);

  Assert(1 > 0);
  Assert(1 >= 0);
  Assert(1 <> 0);

  Assert(not(0 <> 0));
  Assert(not(0 > 0));
  Assert(not(0 < 0));

  Assert(not(1 <> 1));
  Assert(not(1 > 1));
  Assert(not(1 < 1));

  Assert(not(0 >= 1));
  Assert(not(0 > 1));
  Assert(not(0 = 1));

  Assert(not(1 <= 0));
  Assert(not(1 < 0));
  Assert(not(1 = 0));
end;

overlay procedure TestRelOpsSigned;
begin
  WriteLn('--- TestRelOpsSigned ---');

  Assert(-1 = -1);
  Assert(-1 <= -1);
  Assert(-1 >= -1);

  Assert(1 = 1);
  Assert(1 <= 1);
  Assert(1 >= 1);

  Assert(-1 < 1);
  Assert(-1 <= 1);
  Assert(-1 <> 1);

  Assert(1 > -1);
  Assert(1 >= -1);
  Assert(1 <> -1);

  Assert(not(-1 <> -1));
  Assert(not(-1 > -1));
  Assert(not(-1 < -1));

  Assert(not(1 <> 1));
  Assert(not(1 > 1));
  Assert(not(1 < 1));

  Assert(not(-1 >= 1));
  Assert(not(-1 > 1));
  Assert(not(-1 = 1));

  Assert(not(1 <= -1));
  Assert(not(1 < -1));
  Assert(not(1 = -1));
end;

overlay procedure TestRelOpsLarge;
begin
  WriteLn('--- TestRelOpsLarge ---');

  Assert(-23456 = -23456);
  Assert(-23456 <= -23456);
  Assert(-23456 >= -23456);

  Assert(23456 = 23456);
  Assert(23456 <= 23456);
  Assert(23456 >= 23456);

  Assert(-23456 < 23456);
  Assert(-23456 <= 23456);
  Assert(-23456 <> 23456);

  Assert(23456 > -23456);
  Assert(23456 >= -23456);
  Assert(23456 <> -23456);

  Assert(not(-23456 <> -23456));
  Assert(not(-23456 > -23456));
  Assert(not(-23456 < -23456));

  Assert(not(23456 <> 23456));
  Assert(not(23456 > 23456));
  Assert(not(23456 < 23456));

  Assert(not(-23456 >= 23456));
  Assert(not(-23456 > 23456));
  Assert(not(-23456 = 23456));

  Assert(not(23456 <= -23456));
  Assert(not(23456 < -23456));
  Assert(not(23456 = -23456));
end;

overlay procedure TestRelOpsLimits;
begin
  WriteLn('--- TestRelOpsLimits ---');

  Assert(0 < 32767);
  Assert(-32768 < 0);
  Assert(-32768 < 32767);
end;

overlay procedure TestRelOpsBoolean;
begin
  WriteLn('--- TestRelOpsBoolean ---');

  Assert(True = True);
  Assert(True <= True);
  Assert(True >= True);

  Assert(False = False);
  Assert(False <= False);
  Assert(False >= False);

  Assert(True > False);
  Assert(True >= False);
  Assert(True <> False);

  Assert(False < True);
  Assert(False <= True);
  Assert(False <> True);

  Assert(not(True <> True));
  Assert(not(True > True));
  Assert(not(True < True));

  Assert(not(False <> False));
  Assert(not(False > False));
  Assert(not(False < False));

  Assert(not(True <= False));
  Assert(not(True < False));
  Assert(not(True = False));

  Assert(not(False >= True));
  Assert(not(False > True));
  Assert(not(False = True));
end;

overlay procedure TestRelOpsChar;
begin
  WriteLn('--- TestRelOpsChar ---');

  Assert('A' = 'A');
  Assert('A' <= 'A');
  Assert('A' >= 'A');

  Assert('Z' = 'Z');
  Assert('Z' <= 'Z');
  Assert('Z' >= 'Z');

  Assert('A' < 'Z');
  Assert('A' <= 'Z');
  Assert('A' <> 'Z');

  Assert('Z' > 'A');
  Assert('Z' >= 'A');
  Assert('Z' <> 'A');

  Assert(not('A' <> 'A'));
  Assert(not('A' > 'A'));
  Assert(not('A' < 'A'));

  Assert(not('Z' <> 'Z'));
  Assert(not('Z' > 'Z'));
  Assert(not('Z' < 'Z'));

  Assert(not('A' >= 'Z'));
  Assert(not('A' > 'Z'));
  Assert(not('A' = 'Z'));

  Assert(not('Z' <= 'A'));
  Assert(not('Z' < 'A'));
  Assert(not('Z' = 'A'));
end;

overlay procedure TestLogOps;
begin
  WriteLn('--- TestLogOps ---');

  Assert(not(False and False));
  Assert(not(False and True));
  Assert(not(True and False));
  Assert(True and True);

  Assert(not(False or False));
  Assert(False or True);
  Assert(True or False);
  Assert(True or True);

  Assert(not(False xor False));
  Assert(False xor True);
  Assert(True xor False);
  Assert(not(True xor True));

  Assert(not False);
  Assert(True);
end;

overlay procedure TestBitOps;
begin
  WriteLn('--- TestBitOps ---');

  Assert(1536 or 768 = 1792);
  Assert(1536 and 768 = 512);
  Assert(1536 xor 768 = 1280);

  Assert(6 or 3 = 7);
  Assert(6 and 3 = 2);
  Assert(6 xor 3 = 5);

  Assert(not 0 = -1);
  Assert(not 1 = -2);
  Assert(not -1 = 0);
end;

overlay procedure TestVarGlobal;
begin
  WriteLn('--- TestVarGlobal ---');

  X := 1;
  Y := 2;
  Z := 3;
  Assert(X = 1);
  Assert(Y = 2);
  Assert(Z = 3);

  X := Y + Z;
  Assert(X = 5);

  X := 0;
  Assert(X = 0);
  X := 32767;
  Assert(X = 32767);
  X := -32768;
  Assert(X = -32768);

  B := True;
  Assert(B);
  B := False;
  Assert(not B);

  C := 'A';
  Assert(C = 'A');
  C := 'Z';
  Assert(C = 'Z');
end;

overlay procedure TestVarLocal;
var
  X, Y, Z: Integer;
  B : Boolean;
  C: Char;
begin
  WriteLn('--- TestVarLocal ---');

  X := 1;
  Y := 2;
  Z := 3;
  Assert(X = 1);
  Assert(Y = 2);
  Assert(Z = 3);

  X := Y + Z;
  Assert(X = 5);

  X := 0;
  Assert(X = 0);
  X := 32767;
  Assert(X = 32767);
  X := -32768;
  Assert(X = -32768);

  B := True;
  Assert(B);
  B := False;
  Assert(not B);

  C := 'A';
  Assert(C = 'A');
  C := 'Z';
  Assert(C = 'Z');
end;

overlay procedure TestVarNested;
var
  X, Y: Integer;
  B : Boolean;
  C: Char;

  procedure Inner;
  var
    X: Integer;
  begin
    X := 42;
    Assert(X = 42);
    Y := 666;
    Assert(Y = 666);
    Z := 4711;
    Assert(Z = 4711);
  end;

begin
  WriteLn('--- TestVarNested ---');

  X := 1;
  Y := 2;
  Z := 3;

  Assert(X = 1);
  Assert(Y = 2);
  Assert(Z = 3);

  Inner;

  Assert(X = 1);
  Assert(Y = 666);
  Assert(Z = 4711);
end;

(* Overlay 1 *)

const Dummy1 = 0;

overlay procedure TestArrays;
const
  Jan = 1;
  Dec = 12;
  DaysInMonth: array[Jan..Dec] of Integer = (
    31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31
  );
var
  RgbLed: array[Color] of Byte;
  IsPrime: array[2..20] of Boolean;
  UpperCase: array['a'..'z'] of Char;
  I: Integer;
  B: TIntArray100;
  C: Char;
  S: String[255];
  AndGate: array[Boolean] of array[Boolean] of Boolean;
  OrGate: array[Boolean, Boolean] of Boolean;
  P, Q: Boolean;
begin
  WriteLn('--- TestArrays ---');

  BeforeA := 32767;

  for I := 0 to 99 do
    A[I] := I * I;

  AfterA := 32767;

  Assert(BeforeA = 32767);

  for I := 0 to 99 do
    Assert(A[I] = I * I);

  Assert(AfterA = 32767);

  B := A;

  for I := 0 to 99 do
    Assert(B[I] = A[I]);

  for I := 0 to 99 do
    B[I] := B[I] + 1;

  for I := 0 to 99 do
    Assert(B[I] = A[I] + 1);

  (* Ugly alternative syntax works, too (although not perfectly) *)
  B(. 42 .) := 1000;
  Assert(B(. 42 .) = 1000);

  WriteLn('Color: ', Low(Color), '..', High(Color));
  WriteLn('RgbLed: ', Low(RgbLed), '..', High(RgbLed));
  WriteLn('IsPrime: ', Low(IsPrime), '..', High(IsPrime));

  Assert(Low(Color) = Red);
  Assert(High(Color) = Blue);
  Assert(Low(RgbLed) = Red);
  Assert(High(RgbLed) = Blue);
  Assert(Low(IsPrime) = 2);
  Assert(High(IsPrime) = 20);

  WriteLn;

  WriteLn('DaysInMonth: ', Low(DaysInMonth), '..', High(DaysInMonth));

  WriteLn;

  Assert(Low(DaysInMonth) = Jan);
  Assert(High(DaysInMonth) = Dec);

  for I := Jan to Dec do
    WriteLn('Month ', I, ' has ', DaysInMonth[I], ' days.');

  WriteLn;

  WriteLn('ToUpper: ', Low(UpperCase), '..', High(UpperCase));
  WriteLn;

  Assert(Low(UpperCase) = 'a');
  Assert(High(UpperCase) = 'z');

  for C := Low(UpperCase) to High(UpperCase) do
    UpperCase[C] := Char(Ord(C) - 32);

  S := 'The quick brown fox jumped over the lazy dog.';

  for I := 1 to Length(S) do
  begin
    C := S[I];
    if (C >= 'a') and (C <= 'z') then S[I] := UpperCase[C];
  end;

  WriteLn(S);

  Assert(S = 'THE QUICK BROWN FOX JUMPED OVER THE LAZY DOG.');

  AndGate[False][False] := False;
  AndGate[False][True] := False;
  AndGate[True, False] := False;
  AndGate[True, True] := True;

  OrGate[False][False] := False;
  OrGate[False][True] := True;
  OrGate[True, False] := True;
  OrGate[True, True] := True;

  for P := False to True do
    for Q := False to True do
    begin
      Assert(AndGate[P, Q] = P and Q);
      Assert(OrGate[P, Q] = P or Q);
    end;

  Assert(Low(AndGate) = False);
  Assert(High(AndGate) = True);
  Assert(Low(AndGate[False]) = False);
  Assert(High(AndGate[False]) = True);
  Assert(SizeOf(AndGate) = 4);
  Assert(SizeOf(AndGate[False]) = 2);

  Assert(Low(OrGate) = False);
  Assert(High(OrGate) = True);
  Assert(Low(OrGate[False]) = False);
  Assert(High(OrGate[False]) = True);
  Assert(SizeOf(OrGate) = 4);
  Assert(SizeOf(OrGate[False]) = 2);
end;

overlay procedure TestHighLow;
begin
  WriteLn('--- TestHighLow ---');

  Assert(Low(Integer) = -32768);
  Assert(High(Integer) = 32767);

  Assert(Low(Byte) = 0);
  Assert(High(Byte) = 255);

  Assert(Low(Char) = #0);
  Assert(High(Char) = #255);

  Assert(Low(Boolean) = False);
  Assert(High(Boolean) = True);
end;

overlay procedure SwapPointProc(var P: TPoint);
var
  I: Integer;
begin
  I := P.X;
  P.X := P.Y;
  P.Y := I;
end;

overlay function SwapPointFunc(P: TPoint): TPoint;
begin
  SwapPointFunc.X := P.Y;
  SwapPointFunc.Y := P.X;
end;

overlay procedure TestRecords;
var
  P, Q: TPoint;
begin
  WriteLn('--- TestRecords ---');

  P.X := 100;
  P.Y := 200;

  Assert(P.X = 100);
  Assert(P.Y = 200);

  Q := P;

  P.X := 0;
  P.Y := 0;

  Assert(Q.X = 100);
  Assert(Q.Y = 200);

  P := Q;

  Assert(P.X = 100);
  Assert(P.Y = 200);

  SwapPointProc(P);

  Assert(P.X = 200);
  Assert(P.Y = 100);

  Q := SwapPointFunc(P);

  Assert(Q.X = 100);
  Assert(Q.Y = 200);
end;

overlay procedure TestVariantRecords;
type
  Rec1 = record
    C: Char;
    case Boolean of
      False: (I: Integer;);
      True:  (Lo, Hi: Byte;);
  end;

  Rec2 = record
    C: Char;
    case B: Boolean of
      False: (I: Integer;);
      True:  (Lo, Hi: Byte;);
  end;

  Rec3 = record
    case Code: Integer of
      0: ( B: Boolean; );
      1: ( C: Char;    );
      2: ( Y: Byte;    );
      3: ( I: Integer; );
      4: ( R: Real;    );
      5: ( S: record
                case Boolean of
                  False: ( Text: String; );
                  True:  ( Len: Byte; Chrs: array[1..255] of Char; );
              end;
         );
  end;

  Rec4 = record
    X : Integer;
    case Byte of
      2 : ( Y : Integer;
            case Byte of
              3 : ( Z : Integer; );
              4:  ( P : Char; );
          );
      5: ( Q: Char; );
  end;

var
  R1: Rec1;
  R2: Rec2;

const
  R2a: Rec2 = (
    C: 'A';
    B: False;
    I: 42;
  );

  R2b: Rec2 = (
    C: 'B';
    B: True;
    Lo: 1;
    Hi: 2;
  );

  R2c: Rec2 = (
    B: True;
    Lo: 1;
    Hi: 2;
  );

  R2d: Rec2 = (
    C: 'D';
    Hi: 2;
  );

  R2e: Rec2 = (
    C: 'E';
  );

  R2f: Rec2 = (
  );

  R2g: Rec2 = (
    ;;;;;;;;;
  );

var
  R4: Rec4;
  A: Integer;

begin
  WriteLn('--- TestVariantRecords ---');

  A := Addr(R1);
  Assert(Addr(R1.C) = A);
  Assert(Addr(R1.I) = A + 1);
  Assert(Addr(R1.Lo) = A + 1);
  Assert(Addr(R1.Hi) = A + 2);
  Assert(SizeOf(R1) = 3);

  A := Addr(R2);
  Assert(Addr(R2.C) = A);
  Assert(Addr(R2.B) = A + 1);
  Assert(Addr(R2.I) = A + 2);
  Assert(Addr(R2.Lo) = A + 2);
  Assert(Addr(R2.Hi) = A + 3);
  Assert(SizeOf(R2) = 4);

  R2.C := 'X';
  R2.B := False;
  R2.I := 16383;

  Assert(R2.C = 'X');
  Assert(not R2.B);
  Assert(R2.I = 16383);
  Assert(R2.Lo = 255);
  Assert(R2.Hi = 63);

  R2.B := True;
  R2.Hi := 64;
  R2.Lo := 0;

  Assert(R2.B);
  Assert(R2.I = 16384);
  Assert(R2.Lo = 0);
  Assert(R2.Hi = 64);

  Assert(R2a.C = 'A');
  Assert(not R2a.B);
  Assert(R2a.I = 42);

  Assert(R2b.C = 'B');
  Assert(R2b.B);
  Assert(R2b.I = 513);

  Assert(R2c.B);
  Assert(R2c.I = 513);

  Assert(R2d.C = 'D');
  Assert(R2d.Hi = 2);

  Assert(R2e.C = 'E');

  A := Addr(R4);
  Assert(Addr(R4.X) = A);
  Assert(Addr(R4.Y) = A + 2);
  Assert(Addr(R4.Z) = A + 4);
  Assert(Addr(R4.P) = A + 4);
  Assert(Addr(R4.Q) = A + 2);
  Assert(SizeOf(R4) = 6);
end;

overlay procedure TestSets;
type
  Day = (Mon, Tue, Wed, Thu, Fri, Sat, Sun);

  DaySet = set of Day;

  String5 = String[255];

var
  AllDays, Working, Weekend, Mon_Thu, Thu_Sun: DaySet;

  Lower, Upper, Digit, Alpha: set of Char;

  Primes: set of Byte;

  I: Byte;

  procedure DumpSet(DS: DaySet);
  var
    D: Day;
    First: Boolean;
  begin
    Write('[');
    First := True;
    for D := Mon to Sun do
    begin
      if D in DS then
      begin
        if First then First := False else Write(',');
        Write(D);
      end;
    end;
    WriteLn(']');
  end;

  function Check(B: Boolean): String5;
  begin
    if B then Check := '  X  ' else Check := '     ';
  end;

begin
  WriteLn('--- TestRecords ---');
  WriteLn;

  AllDays := [Mon..Sun];
  Working := [Mon..Fri];
  Weekend := [Sat, Sun];
  Mon_Thu := [Mon..Thu];
  Thu_Sun := [Thu..Sun];

  Write('AllDays .............. '); DumpSet(AllDays);
  Write('Working .............. '); DumpSet(Working);
  Write('Weekend .............. '); DumpSet(Weekend);

  WriteLn;

  WriteLn('Fri in Weekend ....... ', Fri in Weekend);
  WriteLn('Sat in Weekend ....... ', Sat in Weekend);

  Assert(not (Fri in Weekend));
  Assert(Sat in Weekend);

  WriteLn;

  Write('Working + Weekend .... '); DumpSet(Working + Weekend);
  Write('AllDays - Weekend .... '); DumpSet(AllDays - Weekend);
  Write('Mon_Thu * Thu_Sun .... '); DumpSet(Mon_Thu * Thu_Sun);

  Assert(Working + Weekend = AllDays);
  Assert(AllDays - Weekend = Working);
  Assert(Mon_Thu * Thu_Sun = [Thu]);

  WriteLn;

  WriteLn('Weekend = Weekend .... ', Weekend = Weekend);
  WriteLn('Weekend <> Weekend ... ', Weekend <> Weekend);
  WriteLn('Working <= AllDays ... ', Working <= AllDays);
  WriteLn('Working >= AllDays ... ', Working >= AllDays);
  WriteLn('Weekend <= Weekend ... ', Weekend <= Weekend);
  WriteLn('Weekend >= Weekend ... ', Weekend >= Weekend);

  Assert(Weekend = Weekend);
  Assert(not (Weekend <> Weekend));
  Assert(Working <= AllDays);
  Assert(not(Working >= AllDays));
  Assert(Weekend <= Weekend);
  Assert(Weekend >= Weekend);


  Lower := ['a'..'z'];
  Upper := ['A'..'Z'];
  Digit := ['0'..'9'];
  Alpha := Lower + Upper + Digit;

  WriteLn;

  WriteLn('Chr Lower Upper Digit Alpha');
  WriteLn('''m'' ', Check('m' in Lower), ' ', Check('m' in Upper), ' ',
                   Check('m' in Digit), ' ', Check('m' in Alpha));
  WriteLn('''M'' ', Check('M' in Lower), ' ', Check('M' in Upper), ' ',
                   Check('M' in Digit), ' ', Check('M' in Alpha));
  WriteLn('''5'' ', Check('5' in Lower), ' ', Check('5' in Upper), ' ',
                   Check('5' in Digit), ' ', Check('5' in Alpha));

  Assert('m' in Lower);
  Assert('M' in Upper);
  Assert('5' in Digit);

  WriteLn;

  Primes := [2, 3, 5, 7, 11, 13, 17, 23, 29, 31];
  Write('Some primes: ');
  for I := 0 to 31 do
    if I in Primes then
      Write(I, ' ');
  WriteLn;

  (* FIXME
  Assert(11 in Primes);
  Assert(not (12 in Primes));
  Assert(13 in Primes);
  *)

  Include(Primes, 42);
  Assert(42 in Primes);
  Exclude(Primes, 42);
  Assert(not (42 in Primes));

  WriteLn;
end;

overlay procedure NoParamProc;
begin
  X := 1234;
end;

overlay function NoParamFunc: Integer;
begin
  NoParamFunc := 5678;
end;

overlay procedure SumProc(A, B: Integer);
begin
  X := A + B;
end;

overlay function SumFunc(A, B: Integer): Integer;
begin
  SumFunc := A + B;
end;

overlay procedure TestProcFunc;
var
  J: Integer;

  function MulFunc(P, Q: Integer): Integer;
  begin
    MulFunc := P * Q;
  end;

  function MulAddFunc(P, Q, R: Integer): Integer;
  begin
    MulAddFunc := P * Q + R;
  end;

begin
  WriteLn('--- TestProcFunc ---');

  NoParamProc;
  Assert(X = 1234);

  J := NoParamFunc;
  Assert(J = 5678);

  SumProc(123, 456);
  Assert(X = 579);

  J := SumFunc(123, 456);
  Assert(J = 579);

  J := MulFunc(100, 200);
  Assert(J = 20000);

  J := MulAddFunc(2, 3, 4);
  Assert(J = 10);
end;

const
  Overlay1 = 1;

overlay procedure AddPoints1(P, Q: TPoint; var R: TPoint);
begin
  R.X := P.X + Q.X;
  R.Y := P.Y + Q.Y;

  P.X := 0;
  P.Y := 0;
  Q.X := 0;
  Q.Y := 0;
end;

overlay function AddPoints2(P, Q: TPoint): TPoint;
begin
  AddPoints2.X := P.X + Q.X;
  AddPoints2.Y := P.Y + Q.Y;

  P.X := 0;
  P.Y := 0;
  Q.X := 0;
  Q.Y := 0;
end;

overlay function ReverseArray(A: TIntArray100; Count: Integer): TIntArray100;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
  begin
    ReverseArray[I] := A[Count - I - 1];
    A[Count - I - 1] := 0;
  end;
end;

overlay procedure TestComplexParams;
var
  P, Q, R: TPoint;
  A, B: TIntArray100;
  I: Integer;
begin
  WriteLn('--- TestComplexParams ---');

  P.X := 1;
  P.Y := 2;
  Q.X := 3;
  Q.Y := 4;

  AddPoints1(P, Q, R);

  Assert(R.X = 4);
  Assert(R.Y = 6);

  Assert(P.X = 1);
  Assert(P.Y = 2);
  Assert(Q.X = 3);
  Assert(Q.Y = 4);

  R.X := 0;
  R.Y := 0;

  R := AddPoints2(P, Q);

  Assert(R.X = 4);
  Assert(R.Y = 6);

  Assert(P.X = 1);
  Assert(P.Y = 2);
  Assert(Q.X = 3);
  Assert(Q.Y = 4);

  for I := 0 to 99 do
    A[I] :=  2 * I;

  B := ReverseArray(A, 100);

  for I := 0 to 99 do
  begin
    Assert(A[I] = 2 * I);
    Assert(B[99 - I] = 2 * I);
  end;
end;

overlay procedure SwapInteger(var X, Y: Integer);
var
  Z: Integer;
begin
  Z := X;
  X := Y;
  Y := Z;
end;

overlay procedure SwapBoolean(var X, Y: Boolean);
var
  Z: Boolean;
begin
  Z := X;
  X := Y;
  Y := Z;
end;

overlay procedure SwapRecord(var R: TPoint);
var
  Z: Integer;
begin
  Z := R.X;
  R.X := R.Y;
  R.Y := Z;
end;

overlay procedure Sort(var A: TIntArray100; Count: Integer);
var
  I, J: Integer;
  Changed: Boolean;
begin
  for I := Count - 1 downto 1 do
  begin
    Changed := False;
    for J := 0 to I - 1 do
    begin
      if (A[J] > A[J + 1]) then
      begin
        SwapInteger(A[J], A[J + 1]);
        Changed := True;
      end;
    end;
    if not Changed then Exit;
  end;
end;

overlay procedure TestSort(var Numbers: TIntArray100; Count: Integer);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Numbers[I] := Random(Count);

  Sort(Numbers, Count);

  I := 1;
  while I < Count do
  begin
    if Numbers[I - 1] > Numbers[I] then Break;
    I := I + 1;
  end;
  Assert(I = Count);
end;

overlay procedure TestVarParams;
var
  I, J: Integer;
  B, C: Boolean;
  LocalIntArray: TIntArray100;
  R: TPoint;
begin
  WriteLn('--- TestVarParams ---');
  WriteLn('(Might take a while.)');

  I := 1234;
  J := 5678;
  SwapInteger(I, J);
  Assert(I = 5678);
  Assert(J = 1234);

  B := True;
  C := False;
  SwapBoolean(B, C);
  Assert(not B);
  Assert(C);

  R.X := 4444;
  R.Y := 8888;
  SwapRecord(R);
  Assert(R.X = 8888);
  Assert(R.Y = 4444);

  TestSort(GlobalIntArray, 100);
  TestSort(LocalIntArray, 100);
end;

overlay function Fibonacci(I: Integer): Integer;
begin
  if I = 0 then
    Fibonacci := 0
  else if I = 1 then
    Fibonacci := 1
  else
    Fibonacci := Fibonacci(I - 1) + Fibonacci(I - 2);
end;

overlay function Factorial(I: Integer): Integer;
begin
  if I = 1 then
    Factorial := I
  else
    Factorial := I * Factorial(I - 1);
end;

overlay procedure TestRecursion;
begin
  WriteLn('--- TestRecursion ---');

  WriteLn('(Might take even longer.)');

  Assert(Fibonacci(23) = 28657);
  Assert(Factorial(7) = 5040);
end;

procedure F1; forward;

procedure F2(X, Y: Integer; var Z: Integer); forward;

function F3(X, Y: Integer): Integer; forward;

procedure TestForward;
var
  Z: Integer;
begin
  WriteLn('--- TestForward ---');

  Z := -1;
  F1;
  Assert(Z = -1);
  F2(11, 22, Z);
  Assert(Z = 33);
  Z := F3(33, 44);
  Assert(Z = 77);
end;

procedure F1;
begin
end;

procedure F2;
begin
  Z := X + Y;
end;

function F3;
begin
  F3 := X + Y;
end;

var
  AbsI: Integer;
  AbsJ: Integer absolute $6C;
  AbsX: Integer absolute AbsI;
  AbsY: Integer absolute AbsJ;
  AbsZ: Integer absolute '__buffer';

overlay procedure TestAbsolute;
begin
  WriteLn('--- TestAbsolute ---');

  Assert(Addr(AbsJ) = $6C);
  Assert(Addr(AbsX) = Addr(AbsI));
  Assert(Addr(AbsY) = Addr(AbsJ));
  Assert(Addr(AbsZ) <> 0);

  AbsI := 100;
  Assert(AbsI = 100);
  Assert(AbsX = 100);
  AbsJ := 200;
  Assert(AbsJ = 200);
  Assert(AbsY = 200);

  AbsX := 101;
  Assert(AbsI = 101);
  Assert(AbsX = 101);
  AbsY := 201;
  Assert(AbsJ = 201);
  Assert(AbsY = 201);
end;

overlay procedure TestArraysOfArrays;
var
  I, J: Integer;
begin
  WriteLn('--- TestArraysOfArrays ---');

  for I := 0 to 9 do
    for J := 0 to 9 do
      AA[I][J] := 10 * I + J;

  for I := 0 to 9 do
    for J := 0 to 9 do
      Assert(AA[I][J] = 10 * I + J);
end;

overlay procedure TestTypeChecks;
var
  I: Integer;
  B: Byte;
  A: array[0..4] of Byte;

  procedure Local(Expected: Integer; Actual: Byte);
  begin
    Assert(Expected = Actual);
  end;

begin
  WriteLn('--- TestTypeChecks ---');

  B := 255;
  Assert(B = 255);

  B := 513;
  Assert(B = 1);

  I := 255;
  Assert(I = 255);

  I := 513;
  Assert(I = 513);

  I := 255;
  B := I;
  I := B;
  Assert(B = 255);
  Assert(I = 255);

  I := 513;
  B := I;
  I := B;
  Assert(B = 1);
  Assert(I = 1);

  for I := 0 to 4 do
    A[I] := 10 * I;

  for I := 0 to 4 do
    Assert(A[I] = 10 * I);

  for I := 0 to 4 do
    A[I] := 1024 + 10 * I;

  for I := 0 to 4 do
    Assert(A[I] = 10 * I);

  Local(255, 255);
  Local(1, 513);
end;

overlay procedure TestIfThen;
var
  I, J: Integer;
begin
  WriteLn('--- TestIfThen ---');

  I := 0;
  J := 0;

  if True then I := 1;
  J := 1;
  Assert(I = 1);
  Assert(J = 1);

  if False then I := 2;
  J := 2;
  Assert(I = 1);
  Assert(J = 2);
end;

overlay procedure TestIfThenElse;
var
  I, J: Integer;
begin
  WriteLn('--- TestIfThenElse ---');

  I := 0;
  J := 0;

  if True then I := 1 else I := 2;
  J := 1;
  Assert(I = 1);
  Assert(J = 1);

  if False then I := 3 else I := 4;
  J := 2;
  Assert(I = 4);
  Assert(J = 2);
end;

overlay procedure TestIfBlocks;
var
  I, J: Integer;
begin
  WriteLn('--- TestIfBlocks ---');

  I := 0;
  J := 0;

  if True then
  begin
    I := 1;
  end
  else
  begin
    I := 2;
  end;
  J := 1;
  Assert(I = 1);
  Assert(J = 1);

  if False then
  begin
    I := 3;
  end
  else
  begin
    I := 4;
  end;
  J := 2;
  Assert(I = 4);
  Assert(J = 2);
end;

overlay procedure TestGoto;
label
  Foo, Bar, 123, 456, 789, Loop;
var
  S: string[255];
  I: Integer;
begin
  S := 'A';

  goto Foo;

  S := S + '1';

  Bar:
    S := S + 'C';
    goto 123;

  S := S + '2';

  Foo:
    S := S + 'B';
    goto Bar;

  S := S + '3';

  123:
    S := S + 'D';

  Assert(S = 'ABCD');

  S := '';

  goto 456;

  S := S + 'X';

  456: begin
    S := S + 'Y';
  end;

  S := S + 'Z';

  Assert(S = 'YZ');

  S := '';

  goto 789;

  S := S + 'X';

  begin
    789: S := S + 'Y';
  end;

  S := S + 'Z';

  Assert(S = 'YZ');

  I := 0;
  Loop:
    I := I + 1;
    if I < 10 then goto Loop;

  Assert(I = 10);
end;

overlay procedure TestCase;
var
  I: Integer;
  S: String[255];
begin
  WriteLn('--- TestCase ---');

  S := '';

  for I := 0 to 12 do
  begin
    Write(I, ' is ');

    case I of
      0:          begin WriteLn('nothing'); S := S + 'n'; end;
      2..3, 5, 7: begin WriteLn('prime'); S := S + 'p'; end;
      4, 9:       begin WriteLn('square'); S := S + 's'; end;
      10:         begin WriteLn('two hands'); S := S + 'h'; end;
      12:         begin Writeln('a dozen'); S := S + 'd'; end;
    else
      WriteLn('just that');
      S := S + 't';
    end;
  end;

  WriteLn;

  Assert(S = 'ntppsptptshtd');

  S := 'ZX Spectrum+ 128K';

  for I := 1 to Length(S) do
  begin
    C := S[I];
    case C of
      'A'..'Z':   C := Char(Ord(S[I]) + 32);
      'a'..'z':   C := Char(Ord(S[I]) - 32);
    end;

    Write(C);
    S[I] := C;
  end;

  WriteLn;

  Assert(S = 'zx sPECTRUM+ 128k');
end;

overlay procedure TestWhile;
var
  I, J, K: Integer;
begin
  WriteLn('--- TestWhile ---');

  I := 0;
  while False do
  begin
    I := I + 1;
  end;

  Assert(I = 0);

  I := 0; J := 0;
  while J < 10 do
  begin
    J := J + 1;
    I := I + J;
  end;

  Assert(I = 55);
  Assert(J = 10);

  I := 0;
  J := 0;
  while J < 10 do
  begin
    J := J + 1;
    K := 0;
    while K < 20 do
    begin
      K := K + 1;
      I := I + J * K;
    end;
  end;

  Assert(I = 11550);
  Assert(J = 10);
  Assert(K = 20);

  I := 0; J := 0;
  while J < 10 do
  begin
    J := J + 1;
    if J = 4 then Continue;
    if J = 8 then Break;
    I := I + J;
  end;

  Assert(I = 24);
  Assert(J = 8);
end;

overlay procedure TestRepeat;
var
  I, J, K: Integer;
begin
  WriteLn('--- TestRepeat ---');

  repeat
    (* Empty *)
  until True;

  Assert(True);

  I := 0;
  repeat
    I := I + 1;
  until True;

  Assert(I = 1);

  I := 0; J := 0;
  repeat
    J := J + 1;
    I := I + J;
  until J = 10;

  Assert(I = 55);
  Assert(J = 10);

  I := 0;
  J := 0;
  repeat
    J := J + 1;
    K := 0;
    repeat
      K := K + 1;
      I := I + J * K;
    until K = 20;
  until J = 10;

  Assert(I = 11550);
  Assert(J = 10);
  Assert(K = 20);

  I := 0; J := 0;
  repeat
    J := J + 1;
    if J = 4 then Continue;
    if J = 8 then Break;
    I := I + J;
  until J = 10;

  Assert(I = 24);
  Assert(J = 8);
end;

overlay procedure TestForInteger;
var
  I, J, K: Integer;
  B: Boolean;
  C: Char;
begin
  WriteLn('--- TestForInteger ---');

  I := 0; J := 0;
  for J := 1 to 0 do
  begin
    I := I + J;
  end;

  Assert(I = 0);
  Assert(J = 1);

  I := 0; J := 0;
  for J := 0 downto 1 do
  begin
    I := I + J;
  end;

  Assert(I = 0);
  Assert(J = 0);

  I := 0; J := 0;
  for J := 1 to 10 do
  begin
    I := I + J;
  end;

  Assert(I = 55);
  Assert(J = 10);

  I := 0; J := 10;
  for J := 10 downto 1 do
  begin
    I := I + J;
  end;

  Assert(I = 55);
  Assert(J = 1);

  I := 0;
  J := 0;
  for J := 1 to 10 do
  begin
    K := 0;
    for K := 1 to 20 do
    begin
      I := I + J * K;
    end;
  end;

  Assert(I = 11550);
  Assert(J = 10);
  Assert(K = 20);

  I := 0; J := 0;
  for J := 1 to 10 do
  begin
    if J = 4 then Continue;
    if J = 8 then Break;
    I := I + J;
  end;

  Assert(I = 24);
  Assert(J = 8);

  K := 0;
  for I := 10 downto 1 do
    for J := 0 to I - 1 do
      K := K + I * J;

  Assert(K = 1320);
end;

overlay procedure TestForBoolean;
var
  I, J: Integer;
  B: Boolean;
  C: Char;
begin
  WriteLn('--- TestForBoolean ---');

  I := 0;
  for B := False to True do
  begin
    if not B then I := 1 else I := I * 2;
  end;

  Assert(B);
  Assert(I = 2);

  I := 0;
  for B := True downto False do
  begin
    if B then I := 1 else I := I * 2;
  end;

  Assert(not B);
  Assert(I = 2);
end;

overlay procedure TestForChar;
var
  I, J: Integer;
  C: Char;
begin
  WriteLn('--- TestForChar ---');

  I := 0; J := 0;
  for C := 'A' to 'Z' do
  begin
    J := J + 1;
    I := I + J * Ord(C);
  end;

  Assert(C = 'Z');
  Assert(I = 28665);
  Assert(J = 26);

  I := 0; J := 0;
  for C := 'Z' downto 'A' do
  begin
    J := J + 1;
    I := I + J * Ord(C);
  end;

  Assert(C = 'A');
  Assert(I = 25740);
  Assert(J = 26);
end;

type
  WithRec = record
    A, B: Real;
  end;

var
  WithC: WithRec;
  WithD: ^WithRec;

overlay procedure TestWith;
var
  A, B, I, J, K: Integer;
  R: WithRec;
  S: array[0..19] of WithRec;
begin
  WriteLn('--- TestWith ---');

  WithD := Ptr(Addr(WithC));

  A := 1;
  B := 2;

  with WithC do
  begin
    A := 3.0;
  end;

  with WithD^ do
  begin
    B := 4.0;
  end;

  with R do
  begin
    A := 5.0;
    B := 6.0;
  end;

  I := 2; J := 3; K := 4;
  with S[I * J + K] do
  begin
    A := 7.0;
    B := 8.0;
  end;

  Assert(A = 1);
  Assert(B = 2);

  Assert(WithC.A = 3.0);
  Assert(WithC.B = 4.0);

  Assert(R.A = 5.0);
  Assert(R.B = 6.0);

  Assert(S[I * J + K].A = 7.0);
  Assert(S[I * J + K].B = 8.0);
end;

overlay procedure UpperCase(var Strg: Str255);
begin
  inline(
    $dd/$21/$04/$00/    (* ld id,4           *)
    $dd/$39/            (* add ix,sp         *)
    $dd/$6e/0/          (* ld l,[ix+0]       *)
    $dd/$66/1/          (* ld h,[ix+1]       *)
    $46/                (* ld b,(hl)         *)
    $04/                (* inc b             *)
                        (* l1:               *)
    $05/                (* dec b             *)
    $ca/*+20/           (* jp z,l2           *)
    $23/                (* inc hl            *)
    $7e/                (* ld a,(hl)         *)
    $fe/$61/            (* cp 'a'            *)
    $da/*-9/            (* jp c,l1           *)
    $fe/$7b/            (* cp 'z'+1          *)
    $d2/*-14/           (* jp nc,l1          *)
    $d6/$20/            (* sub 20h           *)
    $77/                (* ld (hl),a         *)
    $c3/*-20            (* jp l1             *)
  );                    (* l2:               *)
end;

overlay procedure TestInline;
var
  S: Str255;
begin
  WriteLn('--- TestInline ---');

  S := 'hello, inline world!';
  UpperCase(S);
  Assert(S = 'HELLO, INLINE WORLD!')
end;

overlay procedure TestWriteInteger;
begin
  WriteLn('--- TestWriteInteger ---');

  WriteLn;
  WriteLn('Expected output: 0,1,10,100,1000,10000,32767');
  WriteLn('Actual output  : ', 0, ',', 1, ',',  10, ',', 100, ',', 1000, ',', 10000, ',', 32767);
  WriteLn;
  WriteLn('Expected output: 0,-1,-10,-100,-1000,-10000,-32767');
  WriteLn('Actual output  : ', -0, ',', -1, ',', -10, ',', -100, ',', -1000, ',', -10000, ',', -32768);
  WriteLn;
end;

overlay procedure TestWriteBoolean;
begin
  WriteLn('--- TestWriteBoolean ---');

  WriteLn;
  WriteLn('*True* is ', True);   (* TODO: Want double quotes here *)
  WriteLn('*False* is ', False); (* TODO: Want double quotes here *)
  WriteLn;
end;

overlay procedure TestWriteChar;
var
  C: Char;
begin
  WriteLn('--- TestWriteChar ---');

  for C := ' ' to '~' do
  begin
    if Ord(C) mod 16 = 0 then WriteLn;
    Write(C, ' ');
  end;
  WriteLn;
  WriteLn;
end;

overlay procedure TestWriteByte;
var
  B: Byte;
begin
  WriteLn('--- TestWriteByte ---');

  for B := 0 to 255 do
  begin
    if B mod 8 = 0 then WriteLn;
    Write(B, ' ');
  end;
  WriteLn;
  WriteLn;
end;

overlay procedure TestWriteString;
begin
  WriteLn('--- TestWriteString ---');

  WriteLn;
  WriteLn('(No arguments)');
  WriteLn;      (* TODO Want empty arguments here *)
  WriteLn('(Empty string)');
  WriteLn('');
  WriteLn('(Some text)');
  WriteLn('The quick brown fox jumped over the lazy dog');
  WriteLn;
end;

overlay procedure TestEnums;
var
  C: Color;
  I: Integer;
begin
  WriteLn('--- TestEnums ---');
  C := Red;
  Assert(C <> Green);

  C := Green;
  Assert(C = Green);

  C := Green;
  Assert(C > Red);
  Assert(C >= Green);
  Assert(C <= Green);
  Assert(C < Blue);

  Assert(Red = Pred(Green));
  Assert(Succ(Red) = Green);

  Assert(Green = Pred(Blue));
  Assert(Succ(Green) = Blue);

  Assert(Even(Red));
  Assert(Odd(Green));

  Assert(Low(Color) = Red);
  Assert(High(Color) = Blue);

  I := 0;
  for C := Red to Blue do
  begin
    Assert(Ord(C) = I);
    Assert(C = Color(I));
    I := I + 1;
  end;

  CA[0] := Red;
  CA[1] := Green;
  CA[2] := Blue;

  Assert(CA[0] = Red);
  Assert(CA[1] = Green);
  Assert(CA[2] = Blue);

  I := 0;
  for C := Red to Blue do
  begin
    Assert(C = CA[I]);
    I := I + 1;
  end;

  I := 2;
  for C := Blue downto Red do
  begin
    Assert(C = CA[I]);
    I := I - 1;
  end;
end;

overlay procedure TestWriteEnums;
var
  C: Color;
begin
  WriteLn('--- TestWriteEnums ---');
  WriteLn;
  for C := Red to Blue do
  begin
    WriteLn('The light is ', C, '.');
  end;
  WriteLn;
end;

overlay procedure TestSizeOf;
begin
  WriteLn('--- TestSizeOf ---');

  Assert(SizeOf(Integer) = 2);
  Assert(SizeOf(Boolean) = 1);
  Assert(SizeOf(Char) = 1);

  Assert(SizeOf(Color) = 1);
  Assert(SizeOf(TIntArray100) = 200);
  Assert(SizeOf(TPoint) = 4);

  Assert(SizeOf(X) = 2);
  Assert(SizeOf(B) = 1);
  Assert(SizeOf(C) = 1);

  Assert(SizeOf(CA) = 3);

  Assert(SizeOf(GlobalIntArray) = 200);
end;

overlay procedure TestStrings;
type
  TStr255 = string[255];
  TStr31  = string[31];
  TStr15  = string[15];
  TStr0   = string[0];

  TStringArray = array[0..7] of String[31];

var
  MyStr255: TStr255;
  MyStr15: TStr15;
  MyStr0: TStr0;
  S, T: String;
  I: Integer;
  A: TStringArray;

  function ReverseFunc(S: TStr255): TStr255;
  var
    I, L: Integer;
  begin
    ReverseFunc[0] := S[0];
    L := Length(S);
    for I := 1 to L do
      ReverseFunc[I] := S[L - I + 1];
  end;

  procedure ReverseProc(var S: TStr255);
  var
    I, L: Integer;
    C: Char;
  begin
    L := Length(S);
    for I := 1 to L div 2 do
    begin
      C := S[I];
      S[I] := S[L - I + 1];
      S[L - I + 1] := C;
    end;
  end;

  procedure SwapString(var S, T: TStr31);
  var
    U: String[31];
  begin
    U := S;
    S := T;
    T := U;
  end;

  procedure Sort(var A: TStringArray; Count: Integer);
  var
    I, J: Integer;
    T: String;
    Changed: Boolean;
  begin
    for I := Count - 1 downto 1 do
    begin
      Changed := False;
      for J := 0 to I - 1 do
      begin
        if A[J] > A[J + 1] then
        begin
          SwapString(A[J], A[J + 1]);
          Changed := True;
        end;
      end;
      if not Changed then Exit;
    end;
  end;

begin
  WriteLn('--- TestStrings ---');
  WriteLn;

  Assert(SizeOf(TStr255) = 256);
  Assert(SizeOf(TStr15) = 16);
  Assert(SizeOf(TStr0) = 1);

  Assert(SizeOf(MyStr255) = 256);
  Assert(SizeOf(MyStr15) = 16);
  Assert(SizeOf(MyStr0) = 1);

  MyStr255 := 'Hello, ZX Spectrum Next!';
  Assert(Length(MyStr255) = 24);

  MyStr15 := 'Hello, ZX Spectrum Next!';
  Assert(Length(MyStr15) = 15);

  MyStr0 := 'Hello, ZX Spectrum Next!';
  Assert(Length(MyStr0) = 0);

  S := 'XYZ';
  Assert(Length(S) = 3);
  Assert(Ord(S[0]) = 3);

  Assert('ZX Spectrum' = 'ZX Spectrum');
  Assert('ZX Spectrum' <> 'ZX Spectrum +');

  Assert('ZX Spectrum' < 'ZX Spectrum +');
  Assert('ZX Spectrum' <= 'ZX Spectrum +');
  Assert('ZX Spectrum' <= 'ZX Spectrum');

  Assert('ZX Spectrum 128' > 'ZX Spectrum');
  Assert('ZX Spectrum 128' >= 'ZX Spectrum');
  Assert('ZX Spectrum 128' >= 'ZX Spectrum 128');

  S := 'ZX 80';
  Assert(S[5] = '0');
  S[5] := '1';
  Assert(S = 'ZX 81');

  A[7] := 'ZX Spectrum +';
  A[6] := 'ZX Spectrum 128 +3';
  A[5] := 'ZX Spectrum Next';
  A[4] := 'ZX 81';
  A[3] := 'ZX Spectrum 128';
  A[2] := 'ZX 80';
  A[1] := 'ZX Spectrum';
  A[0] := 'ZX Spectrum 128 +2';

  Assert(A[7] = 'ZX Spectrum +');
  Assert(A[6] = 'ZX Spectrum 128 +3');
  Assert(A[5] = 'ZX Spectrum Next');
  Assert(A[4] = 'ZX 81');
  Assert(A[3] = 'ZX Spectrum 128');
  Assert(A[2] = 'ZX 80');
  Assert(A[1] = 'ZX Spectrum');
  Assert(A[0] = 'ZX Spectrum 128 +2');

  for I := 0 to 7 do
    WriteLn('#', I, ': ', A[I]);

  WriteLn;
  WriteLn('Sorting...');
  WriteLn;

  Sort(A, 8);

  for I := 0 to 7 do
    WriteLn('#', I, ': ', A[I]);

  Assert(A[7] = 'ZX Spectrum Next');
  Assert(A[6] = 'ZX Spectrum 128 +3');
  Assert(A[5] = 'ZX Spectrum 128 +2');
  Assert(A[4] = 'ZX Spectrum 128');
  Assert(A[3] = 'ZX Spectrum +');
  Assert(A[2] = 'ZX Spectrum');
  Assert(A[1] = 'ZX 81');
  Assert(A[0] = 'ZX 80');

  S := 'ZX Spectrum';
  T := ReverseFunc(S);

  Assert(T <> S);
  Assert(T = 'murtcepS XZ');
  Assert(ReverseFunc(T) = S);

  T := S;
  ReverseProc(T);
  Assert(T <> S);
  Assert(T = 'murtcepS XZ');
  ReverseProc(T);
  Assert(T = S);

  Assert(('ZX' + ' Spectrum ' + 'Next') = 'ZX Spectrum Next');

  Assert(Concat('ZX', ' Spectrum') = 'ZX Spectrum');
  Assert(Concat('ZX', ' Spectrum ', 'Next') = 'ZX Spectrum Next');

  Assert(Copy(S, 5, 0) = '');
  Assert(Copy(S, 1, 2) = 'ZX');
  Assert(Copy(S, 4, 10) = 'Spectrum');
  Assert(Copy(S, 1, 255) = 'ZX Spectrum');

  Assert(Pos('Spectrum', 'ZX Spectrum Next') = 4);
  Assert(Pos('Sinclair', 'ZX Spectrum Next') = 0);

  S := 'ZX Next';
  Insert('Spectrum ', S, 4);
  Assert(S = 'ZX Spectrum Next');

  Delete(S, 12, 5);
  Assert(S = 'ZX Spectrum');

  Assert('Spectru' + 'm' = 'Spectrum');
  Assert('S' + 'pectrum' = 'Spectrum');
  WriteLn;
end;

const
  Overlay5 = 5;

overlay procedure TestReal;
var
  X, Y, Z: Real;
  I: Integer;
  S: String;

  function Equals(X, Y: Real): Boolean;
  begin
    X := Int(X * 100.0 + 0.5) / 100.0;
    Y := Int(Y * 100.0 + 0.5) / 100.0;
    Equals := X = Y;
  end;

begin
  WriteLn('--- TestReal ---');
  WriteLn;

  WriteLn(' 0.0=', 0.0);
  WriteLn(' 1.0=', 1.0);
  WriteLn('-1.0=', -1.0);
  WriteLn;
  WriteLn('MaxReal=', MaxReal);
  WriteLn;

  Str(MaxReal, S);
  Assert(S = ' 1.701411835E+38');

  Assert(0.0 = 0.0);
  Assert(0.0 <> 1.0);

  Assert(0.0 <= 0.0);
  Assert(0.0 <= 1.0);
  Assert(0.0 < 1.0);

  Assert(1.0 >= 1.0);
  Assert(1.0 >= 0.0);
  Assert(1.0 > 0.0);

  Assert(-1.0 < 1.0);

  Assert(1.0E37 > 1.0E36);
  Assert(-1.0E37 < -1.0E36);

  Assert(4.0 - 4.0 - 4.0 = -4.0);

  X := Pi;
  WriteLn('X=', X);
  Assert(X = 3.14159265359);

  Y := (1.5 + 0.5) * Pi;
  WriteLn('Y=', Y);
  Assert((Y > 6.28) and (Y < 6.29));

  Z := Pi / 8.0;
  WriteLn('Z=', Z);
  Assert((Z > 0.39) and (Z < 0.40));

  Writeln;
  WriteLn('|        X         |      Sin(X)      |      Cos(X)      |');
  WriteLn('|------------------|------------------|------------------|');

  X := 0.0;
  I := 0;
  while X <= Y do
  begin
    WriteLn('| ', X, ' | ', Sin(X), ' | ', Cos(X), ' |');
    X := X + Z;
    I := I + 1;
  end;

  Writeln;
  WriteLn('|        X         |      Sin(X)      |      Cos(X)      |');
  WriteLn('|------------------|------------------|------------------|');

  X := 0.0;
  I := 0;
  while X <= Y do
  begin
    WriteLn('| ', X:16:10, ' | ', Sin(X):16:10, ' | ', Cos(X):16:10, ' |');
    X := X + Z;
    I := I + 1;
  end;

  Assert(X > Y);
  Assert(I = 17);

  Assert(Sin(0.0) = 0.0);
  Assert(Sin(Pi / 2.0) = 1.0);

  Assert(Cos(0.0) = 1.0);
  Assert(Cos(Pi / 2.0) = 0.0);

  Assert(Tan(0.0) = 0.0);
  Assert(Equals(Tan(Pi / 4.0), Sin(Pi / 4.0) / Cos(Pi / 4.0)));

  Assert(Equals(ArcTan(Tan(Pi / 4.0)), Pi / 4.0));

  Assert(Sqr(4.0) = 16.0);
  Assert(Sqrt(16.0) = 4.0);

  Assert(Abs(10.0) = 10.0);
  Assert(Abs(-10.0) = 10.0);

  Assert(Equals(Exp(1.0), 2.71828182846));
  Assert(Equals(Ln(2.71828182846), 1.0));

  Assert(Equals(Log(10000.0), 4.0));

  Assert(Equals(Frac(Pi), 0.14159));

  WriteLn(Frac(-Pi));

  Assert(Equals(Frac(-Pi), -0.14159));

  Assert(Int(Pi) = 3.0);
  Assert(Int(-Pi) = -3.0);

  Assert(Trunc(Pi) = 3);
  Assert(Trunc(-Pi) = -3);

  X := 0.0;
  Y := 0;
  Assert(X = Y);

  WriteLn(Y);

  X := 1.0;
  Y := 1;
  Assert(X = Y);

  WriteLn(Y);

  Assert(1.0 + 1 = 2.0);
  Assert(2 + 1.0 = 3.0);

  Assert(4.0 + - 4.0 = 0.0);
  Assert(4.0 - - 4.0 = 8.0);

  Assert(2.0 * - 4.0 = - 8.0);
  Assert(8.0 / - 4.0 = - 2.0);

  Assert(10.0 / 2.0 * 5.0 = 25.0);

  WriteLn;
end;

overlay procedure TestWriteFormat;

const
  C: Char = 'X';

  procedure TestWriteDefault;
  begin
    Writeln('Now testing default format...');
    WriteLn;

    WriteLn('|', 0, '|');
    WriteLn('|', 1, '|');
    WriteLn('|', -1, '|');
    WriteLn('|', 1234, '|');
    WriteLn('|', -1234, '|');
    WriteLn('|', 12345, '|');
    WriteLn('|', -12345, '|');

    Writeln;

    WriteLn('|', C, '|');

    Writeln;

    WriteLn('|', False, '|');
    WriteLn('|', True, '|');
    WriteLn('|', Red, '|');
    WriteLn('|', Green, '|');
    WriteLn('|', Blue, '|');

    Writeln;

    WriteLn('|', '', '|');
    WriteLn('|', 'ZX', '|');
    WriteLn('|', 'Spectrum', '|');

    Writeln;

    WriteLn('|', 1.0, '|');
    WriteLn('|', 0.0, '|');
    WriteLn('|', -1.0, '|');
    WriteLn('|', 1234.5678, '|');
    WriteLn('|', -1234.5678, '|');

    WriteLn;
  end;

  procedure TestWriteFormat1(Width: Integer);
  begin
    Writeln('Now testing :', Width, ' format...');
    WriteLn;

    WriteLn('|', 0:Width, '|');
    WriteLn('|', 1:Width, '|');
    WriteLn('|', -1:Width, '|');
    WriteLn('|', 1234:Width, '|');
    WriteLn('|', -1234:Width, '|');
    WriteLn('|', 12345:Width, '|');
    WriteLn('|', -12345:Width, '|');

    Writeln;

    WriteLn('|', C:Width, '|');

    Writeln;

    WriteLn('|', False:Width, '|');
    WriteLn('|', True:Width, '|');
    WriteLn('|', Red:Width, '|');
    WriteLn('|', Green:Width, '|');
    WriteLn('|', Blue:Width, '|');

    Writeln;

    WriteLn('|', '':Width, '|');
    WriteLn('|', 'ZX':Width, '|');
    WriteLn('|', 'Spectrum':Width, '|');

    Writeln;

    WriteLn('|', 1.0:Width, '|');
    WriteLn('|', 0.0:Width, '|');
    WriteLn('|', -1.0:Width, '|');
    WriteLn('|', 1234.5678:Width, '|');
    WriteLn('|', -1234.5678:Width, '|');

    WriteLn;
  end;

  procedure TestWriteFormat2(Width: Integer; Decimals: Integer);
  begin
    Writeln('Now testing :', Width, ':', Decimals, ' format...');
    WriteLn;

    WriteLn('|', 1.0:Width:Decimals, '|');
    WriteLn('|', 0.0:Width:Decimals, '|');
    WriteLn('|', -1.0:Width:Decimals, '|');
    WriteLn('|', 1234.5678:Width:Decimals, '|');
    WriteLn('|', -1234.5678:Width:Decimals, '|');

    WriteLn;
  end;

begin
  Writeln('--- TestWriteFormat ---');
  WriteLn;

  TestWriteDefault;

  TestWriteFormat1(0);
  TestWriteFormat1(5);
  TestWriteFormat1(10);
  TestWriteFormat1(20);

  TestWriteFormat2(0, 0);
  TestWriteFormat2(0, 5);
  TestWriteFormat2(0, 10);
  TestWriteFormat2(5, 5);
  TestWriteFormat2(10, 5);
  TestWriteFormat2(20, 5);
end;

overlay procedure TestIncDec;
var
  B: Byte;
  I: Integer;
  C: Char;
  E: (East, North, West, South);
  Z: Boolean;
begin
  WriteLn('--- TestIncDec ---');

  B := 100;
  Inc(B);
  Assert(B = 101);
  Dec(B);
  Assert(B = 100);

  Inc(B, 155);
  Assert(B = 255);
  Inc(B, 2);
  Assert(B = 1);

  Dec(B, 2);
  Assert(B = 255);
  Dec(B, 155);
  Assert(B = 100);

  I := 255;
  Inc(I);
  Assert(I = 256);
  Dec(I);
  Assert(I = 255);

  Inc(I, 1000);
  Assert(I = 1255);
  Dec(I, 2000);
  Assert(I = -745);

  C := 'X';
  Inc(C);
  Assert(C = 'Y');
  Dec(C);
  Assert(C = 'X');
  Dec(C, 23);
  Assert(C = 'A');
  Inc(C, 25);
  Assert(C = 'Z');

  E := North;
  Inc(E);
  Assert(E = West);
  Dec(E);
  Assert(E = North);

  Z := False;
  Inc(Z);
  Assert(Z = True);
  Dec(Z);
  Assert(Z = False);
end;

overlay procedure TestBuiltIns;
var
  S, T: String[15];
begin
  WriteLn('--- TestBuiltIns ---');

  Assert(MaxInt = 32767);

  Assert(Hi($1234) = $12);
  Assert(Lo($1234) = $34);
  Assert(Swap($1234) = $3412);
  Assert(Chr(65) = 'A');

  S := 'Hello, World!';
  Move(S, T, SizeOf(S));
  Assert(T = S);

  Move(S[1], T[8], 5);
  Assert(T = 'Hello, Hello!');

  Move(S[8], T[1], 5);
  Assert(T = 'World, Hello!');

  T := '';
  Move(S, T, 0);
  Assert(Length(T) = 0);

  S := 'ABC';

  FillChar(S[2], 1, 'b');
  Assert(S = 'AbC');

  FillChar(S[3], 1, 99);
  Assert(S = 'Abc');

  FillChar(S[0], 1, True);
  Assert(S = 'A');

  S[0] := #3;
  FillChar(S[1], 3, 'Z');
  Assert(S = 'ZZZ');

  FillChar(S[2], 0, 'X');
  Assert(S = 'ZZZ');
end;

begin
  WriteLn('*** PASTA/80 Test Suite ***');
  WriteLn;

  TestComment;

  TestConst;

  TestAdd;
  TestSubtract;
  TestMultiply;
  TestDivide;
  TestModulus;
  TestComplex;
  TestShift;

  TestRelOpsUnsigned;
  TestRelOpsSigned;
  TestRelOpsLarge;
  TestRelOpsLimits;
  TestRelOpsBoolean;
  TestRelOpsChar;

  TestLogOps;
  TestBitOps;

  TestVarGlobal;
  TestVarLocal;
  TestVarNested;
  TestArrays;
  TestRecords;
  TestVariantRecords;
  TestArraysOfArrays;
  TestAbsolute;

  TestEnums;
  TestHighLow;

  TestStrings;
  TestReal;
  TestSets;

  TestSizeOf;

  TestProcFunc;
  TestComplexParams;
  TestVarParams;
  TestRecursion;
  TestForward;

  TestTypeChecks;

  TestIfThen;
  TestIfThenElse;
  TestIfBlocks;

  TestGoto;

  TestCase;

  TestWhile;
  TestRepeat;
  TestForInteger;
  TestForBoolean;
  TestForChar;

  TestWith;

  (* Additional 'with' tests for level 1 *)
  with WithC do
  begin
    A := 9.0;
    B := 10.0;
  end;
  Assert(WithC.A = 9.0);
  Assert(WithC.B = 10.0);

  TestInline;

  TestWriteInteger;
  TestWriteBoolean;
  TestWriteChar;
  TestWriteByte;
  TestWriteString;
  TestWriteEnums;

  TestWriteFormat;

  TestIncDec;

  TestBuiltIns;

  WriteLn;
  WriteLn('************************');
  WriteLn('Passed assertions: ', AssertPassed);
  WriteLn('Failed assertions: ', AssertFailed);
  WriteLn('************************');
  WriteLn;
end.