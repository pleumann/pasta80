program Heap;

var
  P: Pointer;
  Org: Integer;

procedure DumpHeap(var Blocks, Total: Integer);
var
  P: PBlock;
begin
  Blocks := 0;
  Total := 0;

  P := HeapPtr;
  while P <> nil do
  begin
    WriteLn('* ', Ord(P), ': Next=', Ord(P^.Next), ' Size=', P^.Size);
    Inc(Blocks);
    Inc(Total, P^.Size);
    P := P^.Next; 
  end;
  WriteLn(Blocks, ' blocks, ', Total, ' bytes total');
end;

procedure TestSimple;
var
  P, Q: Pointer;
  Blocks, Total, Max: Integer;
begin
  WriteLn('--- TestSimple ---');
  WriteLn;

  Org := MemAvail;
  
  DumpHeap(Blocks, Total);
  Assert(Blocks = 1);
  Assert(Total = MemAvail);
  
  WriteLn;
  WriteLn('Allocating two blocks...');
  GetMem(P, 256);
  Writeln('P=', Ord(P));
  GetMem(Q, 256);
  Writeln('Q=', Ord(Q));

  DumpHeap(Blocks, Total);
  Assert(Blocks = 1);
  Assert(Total = MemAvail);

  Max := Total;

  WriteLn;
  WriteLn('Freeing first block...');
  FreeMem(P, 256);
  DumpHeap(Blocks, Total);
  Assert(Blocks = 2);
  Assert(Total = MemAvail);

  WriteLn;
  WriteLn('Freeing second block...');
  FreeMem(Q, 256);
  DumpHeap(Blocks, Total);
  Assert(Blocks = 3);
  Assert(Total = MemAvail);

  Assert(MemAvail = Org);
  Assert(MaxAvail = Max);
end;

procedure TestStress;
var
  P: array[0..255] of Pointer;
  Size, I, J, K, Org: Integer;
begin
  WriteLn('--- TestStress ---');
  WriteLn;

  Org := MemAvail;

  Size := 1024;
  while Size >= 4 do
  begin
    WriteLn('Block size =', Size:4);
    Write('Block count=   0');
    I := 0;
    while (I < 256) and (MaxAvail >= Size)do
    begin
      GetMem(P[I], Size);
      Inc(I);
      Write(#8#8#8#8, I:4);
    end;

    WriteLn;
    Assert(MemAvail < Org);

    for J := 0 to 9 do
    begin
      K := Random(I);
      if P[K] <> nil then
      begin
        FreeMem(P[K], Size);
        P[K] := nil;
      end;
    end;

    for J := 0 to I - 1 do
    begin
      if P[J] <> nil then FreeMem(P[J], Size);
    end;

    Size := Size div 2;

    Assert(MemAvail = Org);

    WriteLn;
  end;
end;

type
  PNode = ^TNode;
  TNode = record
    Data: string[30];
    Next: PNode;
  end;

function Setup(P: PNode; S: string): PNode;
var
  Q: PNode;
begin
  New(Q);
  Q^.Next := P;
  Q^.Data := S;
  Setup := Q;
end;

procedure Print(P: PNode);
begin
  while P <> nil do
  begin
    WriteLn(P^.Data);
    P := P^.Next;
  end;
end;

procedure Clear(P: PNode);
begin
  if P^.Next <> nil then Clear(P^.Next);
  Dispose(P);
end;

procedure TestNewDispose;
var
  TheFullMonty: PNode;
begin
  WriteLn('--- TestNewDispose ---');

  Org := MemAvail;

  TheFullMonty := Setup(Setup(Setup(Setup(Setup(Setup(nil,
    'Press a key and try again!'),
    'Don''t let Monty die in vain'),
    'In its search for precious coal?'),
    'The hazards that confront a mole'),
    'Aren''t 3 lives enough to last'),
    'Why did Monty die so fast?');

  WriteLn;  
  WriteLn('MemAvail: ', MemAvail, ' MaxAvail: ', MaxAvail, ' HeapPtr: ', Ord(HeapPtr));
  WriteLn;  

  Assert(MemAvail < Org);

  Print(TheFullMonty);
  Clear(TheFullMonty);

  WriteLn;  
  WriteLn('MemAvail: ', MemAvail, ' MaxAvail: ', MaxAvail, ' HeapPtr: ', Ord(HeapPtr));

  Assert(MemAvail = Org);
end;

begin
  WriteLn;
  WriteLn('*** PASTA/80 Test Suite ***');
  WriteLn;

  WriteLn('MemAvail: ', MemAvail, ' MaxAvail: ', MaxAvail, ' HeapPtr: ', Ord(HeapPtr));
  WriteLn;

  TestSimple;

  WriteLn;
  WriteLn('MemAvail: ', MemAvail, ' MaxAvail: ', MaxAvail, ' HeapPtr: ', Ord(HeapPtr));
  WriteLn;

  TestStress;

  WriteLn('MemAvail: ', MemAvail, ' MaxAvail: ', MaxAvail, ' HeapPtr: ', Ord(HeapPtr));
  WriteLn;

  TestNewDispose;

  WriteLn;
  WriteLn('************************');
  WriteLn('Passed assertions: ', AssertPassed);
  WriteLn('Failed assertions: ', AssertFailed);
  WriteLn('************************');
  WriteLn;
end.
