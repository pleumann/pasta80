program Heap;

var
  P: Pointer;

procedure DumpHeap;
var
  P: PBlock;
begin
  P := HeapPtr;
  while P <> nil do
  begin
    WriteLn(Ord(P), ':', Ord(P^.Next), P^.Size);
    P := P^.Next; 
  end;
end;

procedure SimpleTest;
var
  P: Pointer;
begin
  DumpHeap;
  
  GetMem(P, 256);

  Writeln('P=', Ord(P));

  DumpHeap;

  Writeln('P=', Ord(P));

  FreeMem(P, 256);

  DumpHeap;

  GetMem(P, 128);

  DumpHeap;

  FreeMem(P, 128);

  DumpHeap;

  
(*  
  WriteLn('P: ', Ord(P), ' HeapPtr: ', Ord(HeapPtr), ' MemAvail: ', MemAvail, ' MaxAvail: ', MaxAvail);
  
  FreeMem(P, 1000);
  WriteLn('P: ', Ord(P), ' HeapPtr: ', Ord(HeapPtr), ' MemAvail: ', MemAvail, ' MaxAvail: ', MaxAvail);

  GetMem(P, 1000);
  WriteLn('P: ', Ord(P), ' HeapPtr: ', Ord(HeapPtr), ' MemAvail: ', MemAvail, ' MaxAvail: ', MaxAvail);
  
  FreeMem(P, 1000);
  WriteLn('P: ', Ord(P), ' HeapPtr: ', Ord(HeapPtr), ' MemAvail: ', MemAvail, ' MaxAvail: ', MaxAvail);
  
  GetMem(P, 500);
  WriteLn('P: ', Ord(P), ' HeapPtr: ', Ord(HeapPtr), ' MemAvail: ', MemAvail, ' MaxAvail: ', MaxAvail);
  
  FreeMem(P, 500);    
  WriteLn('P: ', Ord(P), ' HeapPtr: ', Ord(HeapPtr), ' MemAvail: ', MemAvail, ' MaxAvail: ', MaxAvail);
*)
end;

procedure HeapStress;
var
  P: array[128] of Pointer;
  Size, I: Integer;
begin
  Size := 128;
  while Size >= 4 do
  begin
    Writeln('Block size=', Size);
    for I := 0 to 127 do
      GetMem(P[I], Size);
    for I := 0 to 127 do
      FreeMem(P[I], Size);
    Size := Size - 4;
  end;

  WriteLn;
end;

type
  PNode = ^TNode;
  TNode = record
    Data: TString;
    Next: PNode;
  end;

function Setup(P: PNode; S: TString): PNode;
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

var
  TheFullMonty: PNode;

begin
  InitHeap(16384);

{  SimpleTest;
  Exit;
 } 
  WriteLn('MemAvail: ', MemAvail, ' MaxAvail: ', MaxAvail, ' HeapPtr: ', Ord(HeapPtr));
  WriteLn;

  HeapStress;

  WriteLn('MemAvail: ', MemAvail, ' MaxAvail: ', MaxAvail, ' HeapPtr: ', Ord(HeapPtr));
  WriteLn;

  Exit;

  TheFullMonty := Setup(Setup(Setup(Setup(Setup(Setup(nil,
    'Press a key and try again!'),
    'Don''t let Monty die in vain'),
    'In its search for precious coal?'),
    'The hazards that confront a mole'),
    'Aren''t 3 lives enough to last'),
    'Why did Monty die so fast?');

  WriteLn('MemAvail: ', MemAvail, ' MaxAvail: ', MaxAvail, ' HeapPtr: ', Ord(HeapPtr));
  WriteLn;  

  Print(TheFullMonty);
  Clear(TheFullMonty);

  WriteLn;  
  WriteLn('MemAvail: ', MemAvail, ' MaxAvail: ', MaxAvail, ' HeapPtr: ', Ord(HeapPtr));
end.
