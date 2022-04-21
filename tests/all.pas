const
  Pi = 31415;

  NotTrue = False;

  FortyTwo: Integer = 42;

  NotTrueEither: Boolean = False;

  ThreeByThree: array[3] of array[3] of Integer = (
    (1, 2, 3),
    (4, 5, 6),
    (7, 8, 9)
  );

type
  Color = (Red, Yellow, Green);

  TIntArray100 = array[100] of Integer;

  TPoint = record
    X, Y: Integer;
  end;

var
  X, Y, Z: Integer;
  B: Boolean;
  C: Char;

  BeforeA: Integer;
  A: TIntArray100;
  AfterA: Integer;

  AA: array[10] of array[10] of Integer;

  CA: array[3] of Color;

  GlobalIntArray: TIntArray100;

procedure TestComment;
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
end;

procedure TestConstHelp(Expected: Integer);
const
  Global: Integer = 666;
begin
  Assert(Global = Expected);
  Global := Global + 1;  
end;

procedure TestConst;
var
  I, J: Integer;
begin
  WriteLn('--- TestConst ---');

  Assert(Pi = 31415);
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
end;

procedure TestAdd;
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

procedure TestSubtract;
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

procedure TestMultiply;
begin
  WriteLn('--- TestMultiply ---');

  Assert(0 * 0 = 0);
  Assert(1 * 1 = 1);
  Assert(1 * 0 = 0);
  Assert(0 * 1 = 0);

  Assert(3 * 4 = 12);
  Assert(3 * (-4) = -12);
  Assert((-3) * 4 = -12);
  Assert((-3) * (-4) = 12);
end;

procedure TestDivide;
begin
  WriteLn('--- TestDivide ---');

  Assert(0 / 1 = 0);
  Assert(1 / 1 = 1);

  Assert(10 / 5 = 2);
  Assert(10 / (-5) = -2);
  Assert((-10) / 5 = -2);
  Assert((-10) / (-5) = 2);
end;

procedure TestModulus;
begin
  WriteLn('--- TestModulus ---');

  Assert(10 mod 1 = 0);
  Assert(10 mod 2 = 0);
  Assert(10 mod 3 = 1);
  Assert(10 mod 4 = 2);
  Assert(10 mod 10 = 0);
  Assert(10 mod 11 = 10);
end;

procedure TestComplex;
begin
  WriteLn('--- TestComplex ---');

  Assert(4 - 4 - 4 = -4);
  Assert(2 * (3 + 4 * (5 - 6 * (7 + 8 * (9 / 3)))) = -1442);
end;

procedure TestRelOpsUnsigned;
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

procedure TestRelOpsSigned;
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

procedure TestRelOpsLarge;
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

procedure TestRelOpsLimits;
begin
  WriteLn('--- TestRelOpsLimits ---');

  Assert(0 < 32767);
  Assert(-32768 < 0);
  Assert(-32768 < 32767);
end;

procedure TestRelOpsBoolean;
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

procedure TestRelOpsChar;
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

procedure TestLogOps;
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

procedure TestBitOps;
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

procedure TestVarGlobal;
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

procedure TestVarLocal;
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

procedure TestVarNested;
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

procedure TestArrays;
var
  I: Integer;
  B: TIntArray100;
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
end;

procedure SwapPointProc(var P: TPoint);
var
  I: Integer;
begin
  I := P.X;
  P.X := P.Y;
  P.Y := I;
end;

function SwapPointFunc(P: TPoint): TPoint;
begin
  SwapPointFunc.X := P.Y;
  SwapPointFunc.Y := P.X;
end;

procedure TestRecords;
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

procedure NoParamProc;
begin
  X := 1234;  
end;

function NoParamFunc: Integer;
begin
  NoParamFunc := 5678;  
end;

procedure SumProc(A, B: Integer);
begin
  X := A + B;
end;

function SumFunc(A, B: Integer): Integer;
begin
  SumFunc := A + B;
end;

procedure TestProcFunc;
var
  J: Integer;

  function MulFunc(P, Q: Integer): Integer;
  begin
    MulFunc := P * Q;
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
end;

procedure AddPoints1(P, Q: TPoint; var R: TPoint);
begin
  WriteLn(P.X, P.Y, Q.X, Q.Y, R.X, R.Y);

  R.X := P.X + Q.X;
  R.Y := P.Y + Q.Y;

  P.X := 0;
  P.Y := 0;
  Q.X := 0;
  Q.Y := 0;
end;

function AddPoints2(P, Q: TPoint): TPoint;
begin
  AddPoints2.X := P.X + Q.X;
  AddPoints2.Y := P.Y + Q.Y;

  P.X := 0;
  P.Y := 0;
  Q.X := 0;
  Q.Y := 0;
end;

function ReverseArray(A: TIntArray100; Count: Integer): TIntArray100;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
  begin
    ReverseArray[I] := A[Count - I - 1];
    A[Count - I - 1] := 0;
  end;
end;

procedure TestComplexParams;
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

procedure SwapInteger(var X, Y: Integer);
var
  Z: Integer;
begin
  Z := X;
  X := Y;
  Y := Z;
end;

procedure SwapBoolean(var X, Y: Boolean);
var
  Z: Boolean;
begin
  Z := X;
  X := Y;
  Y := Z;
end;

procedure SwapRecord(var R: TPoint);
var
  Z: Integer;
begin
  Z := R.X;
  R.X := R.Y;
  R.Y := Z;
end;

procedure Sort(var A: TIntArray100; Count: Integer);
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

procedure TestSort(var Numbers: TIntArray100; Count: Integer);
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

procedure TestVarParams;
var
  I, J: Integer;
  B, C: Boolean;
  LocalIntArray: TIntArray100;
  R: TPoint;
begin
  WriteLn('--- TestVarParams ---');

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

function Fibonacci(I: Integer): Integer;
begin
  if I = 0 then
    Fibonacci := 0
  else if I = 1 then
    Fibonacci := 1
  else
    Fibonacci := Fibonacci(I - 1) + Fibonacci(I - 2);  
end;

function Factorial(I: Integer): Integer;
begin
  if I = 1 then
    Factorial := I
  else
    Factorial := I * Factorial(I - 1);
end;

procedure TestRecursion;
begin
  WriteLn('--- TestRecursion ---');

  Assert(Fibonacci(23) = 28657);
  Assert(Factorial(7) = 5040);
end;

procedure TestArraysOfArrays;
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

procedure TestTypeChecks;
var
  I: Integer;
  B: Byte;
  A: array[5] of Byte;

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

procedure TestIfThen;
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

procedure TestIfThenElse;
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

procedure TestIfBlocks;
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

procedure TestWhile;
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

procedure TestRepeat;
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

procedure TestForInteger;
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

  WriteLn(K);
end;

procedure TestForBoolean;
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

procedure TestForChar;
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

procedure TestWriteInteger;
begin
  WriteLn('--- TestWriteInteger ---');

  WriteLn;
  WriteLn('Expected output: 0, 1, 10, 100, 1000, 10000, 32767');
  WriteLn('Actual output  :', 0, ',', 1, ',',  10, ',', 100, ',', 1000, ',', 10000, ',', 32767);
  WriteLn;
  WriteLn('Expected output: 0,-1,-10,-100,-1000,-10000,-32767');
  WriteLn('Actual output  :', -0, ',', -1, ',', -10, ',', -100, ',', -1000, ',', -10000, ',', -32768);
  WriteLn;
end;

procedure TestWriteBoolean;
begin
  WriteLn('--- TestWriteBoolean ---');
  
  WriteLn;
  WriteLn('*True* is ', True);   (* TODO: Want double quotes here *)
  WriteLn('*False* is ', False); (* TODO: Want double quotes here *)
  WriteLn;
end;

procedure TestWriteChar;
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

procedure TestWriteByte;
var
  B: Byte;
begin
  WriteLn('--- TestWriteByte ---');

  for B := 0 to 255 do
  begin
    if B mod 8 = 0 then WriteLn;
    Write(B);
  end;
  WriteLn;
  WriteLn;
end;

procedure TestWriteString;
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

procedure TestEnums;
var
  C: Color;
  I: Integer;
begin
  WriteLn('--- TestEnums ---');
  C := Red;
  Assert(C <> Green);

  C := Green;
  Assert(C = Green);
  
  C := Yellow;
  Assert(C > Red);
  Assert(C >= Yellow);
  Assert(C <= Yellow);
  Assert(C < Green);

  Assert(Red = Pred(Yellow));
  Assert(Succ(Red) = Yellow);

  Assert(Yellow = Pred(Green));
  Assert(Succ(Yellow) = Green);

  Assert(Even(Red));
  Assert(Odd(Yellow));

  I := 0;
  for C := Red to Green do
  begin
    Assert(Ord(C) = I);
    Assert(C = Color(I));
    I := I + 1;
  end;

  CA[0] := Red;
  CA[1] := Yellow;
  CA[2] := Green;

  Assert(CA[0] = Red);
  Assert(CA[1] = Yellow);
  Assert(CA[2] = Green);

  I := 0;
  for C := Red to Green do
  begin
    Assert(C = CA[I]);
    I := I + 1;
  end;

  I := 2;
  for C := Green downto Red do
  begin
    Assert(C = CA[I]);
    I := I - 1;
  end;
end;

procedure TestWriteEnums;
var
  C: Color;
begin
  WriteLn('--- TestWriteEnums ---');
  WriteLn;
  for C := Red to Green do
  begin
    WriteLn('The light is ', C, '.');
  end;
  WriteLn;
end;

procedure TestSizeOf;
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

procedure TestStrings;
type
  TStr255 = string[255];
  TStr31  = string[31];
  TStr15  = string[15];
  TStr0   = string[0];

  TStringArray = array[8] of String[31];

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
    for I := 1 to L / 2 do
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

  Assert(Concat('ZX', Concat(' Spectrum ', 'Next')) = 'ZX Spectrum Next');

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

  WriteLn;
end;

begin
  TestComment;

  TestConst;

  TestAdd;
  TestSubtract;
  TestMultiply;
  TestDivide;
  TestModulus;
  TestComplex;

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
  TestArraysOfArrays;

  TestEnums;

  TestStrings;

  TestSizeOf;

  TestProcFunc;
  TestComplexParams;
  TestVarParams;
  TestRecursion;
  
  TestTypeChecks;

  TestIfThen;
  TestIfThenElse;
  TestIfBlocks;

  TestWhile;
  TestRepeat;
  TestForInteger;
  TestForBoolean;
  TestForChar;

  TestWriteInteger;
  TestWriteBoolean;
  TestWriteChar;
  TestWriteByte;
  TestWriteString;
  TestWriteEnums;
end.