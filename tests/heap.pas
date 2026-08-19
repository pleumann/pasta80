program Heap;

var
  P: Pointer;
  Org: Integer;
  StartAvail, StartPtr: Integer;

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

type
  PByte = ^Byte;
  PSmall = ^TSmall;
  TSmall = record
    A, B: Byte;
  end;

(* Regression test for heap blocks smaller than a free list node. A node of *)
(* TBlock needs 4 bytes (Next + Size); a request for less must be rounded  *)
(* up, or __freemem would later write its header past the block's end. Size *)
(* 0 must be a complete no-op (TP 3/5 compatible): pointer and heap stay    *)
(* untouched. Size 4 exactly is included because it used to trip a boundary *)
(* bug in __chksize, where a stale flag from the size comparison got reused *)
(* as the "size is zero" signal and the allocation silently never happened. *)
procedure TestSmallBlocks;
var
  Org, Before: Integer;
  B1, B2: PByte;
  S1, S2: PSmall;
  P, Q, R: Pointer;
begin
  WriteLn('--- TestSmallBlocks ---');
  WriteLn;

  Org := MemAvail;

  (* New/Dispose on a pointer to a one byte type. *)
  New(B1);
  New(B2);
  B1^ := 11;
  B2^ := 22;
  Assert(Abs(Addr(B2^) - Addr(B1^)) >= 4);
  Dispose(B1);
  Assert(B2^ = 22);
  Dispose(B2);
  Assert(MemAvail = Org);

  (* New/Dispose on a two byte record. *)
  New(S1);
  New(S2);
  S1^.A := 1; S1^.B := 2;
  S2^.A := 3; S2^.B := 4;
  Assert(Abs(Addr(S2^) - Addr(S1^)) >= 4);
  Dispose(S1);
  Assert(S2^.A = 3);
  Assert(S2^.B = 4);
  Dispose(S2);
  Assert(MemAvail = Org);

  (* Explicit GetMem/FreeMem with sizes below the node size. *)
  GetMem(P, 1);
  GetMem(Q, 2);
  GetMem(R, 3);
  Assert(Ord(Q) - Ord(P) >= 4);
  Assert(Ord(R) - Ord(Q) >= 4);
  FreeMem(P, 1);
  FreeMem(Q, 2);
  FreeMem(R, 3);
  Assert(MemAvail = Org);

  (* Size 4 exactly: the __chksize boundary case. *)
  Before := MemAvail;
  GetMem(P, 4);
  Assert(P <> nil);
  Assert(MemAvail < Before);
  FreeMem(P, 4);
  Assert(MemAvail = Before);

  (* Size 0 must be a complete no-op: pointer and heap stay untouched. *)
  GetMem(P, 2);
  Q := P;
  Before := MemAvail;
  GetMem(P, 0);
  Assert(P = Q);
  Assert(MemAvail = Before);
  FreeMem(P, 0);
  Assert(MemAvail = Before);
  FreeMem(P, 2);
  Assert(MemAvail = Org);
end;

procedure TestCompact;
var
  Blocks, Total: Integer;
begin
  WriteLn('--- TestCompact ---');
  WriteLn;

  WriteLn('Before compaction:');
  DumpHeap(Blocks, Total);

  DefragMem;

  WriteLn;
  WriteLn('After compaction:');
  DumpHeap(Blocks, Total);

  (* All previous tests clean up after themselves, so by now the heap should  *)
  (* be fragmented into several free blocks that together still add up to    *)
  (* the original size. After compaction they must merge back into the one   *)
  (* single block we started out with, at the very same address.            *)
  Assert(Blocks = 1);
  Assert(Total = StartAvail);
  Assert(MemAvail = StartAvail);
  Assert(MaxAvail = StartAvail);
  Assert(Ord(HeapPtr) = StartPtr);
end;

begin
  {$ifdef SYS_ZXNEXT}
  SetCpuSpeed(3);
  {$endif}
  
  WriteLn;
  WriteLn('*** PASTA/80 Test Suite ***');
  WriteLn;

  WriteLn('MemAvail: ', MemAvail, ' MaxAvail: ', MaxAvail, ' HeapPtr: ', Ord(HeapPtr));
  WriteLn;

  StartAvail := MemAvail;
  StartPtr := Ord(HeapPtr);

  TestSimple;

  WriteLn;
  WriteLn('MemAvail: ', MemAvail, ' MaxAvail: ', MaxAvail, ' HeapPtr: ', Ord(HeapPtr));
  WriteLn;

  TestStress;

  WriteLn('MemAvail: ', MemAvail, ' MaxAvail: ', MaxAvail, ' HeapPtr: ', Ord(HeapPtr));
  WriteLn;

  TestNewDispose;

  WriteLn;
  WriteLn('MemAvail: ', MemAvail, ' MaxAvail: ', MaxAvail, ' HeapPtr: ', Ord(HeapPtr));
  WriteLn;

  TestSmallBlocks;

  WriteLn;
  WriteLn('MemAvail: ', MemAvail, ' MaxAvail: ', MaxAvail, ' HeapPtr: ', Ord(HeapPtr));
  WriteLn;

  TestCompact;

  WriteLn;
  WriteLn('************************');
  WriteLn('Passed assertions: ', AssertPassed);
  WriteLn('Failed assertions: ', AssertFailed);
  WriteLn('************************');
  WriteLn;
end.
