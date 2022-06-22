type
  PBlock = ^TBlock;
  TBlock = record
    Size: Integer;
    Next: PBlock;
  end;

var
  HeapPtr: PBlock;

procedure FreeMem(var P: Pointer; Size: Integer);
var
  Q: PBlock;
begin
  Q := P;
  Q^.Size := Size;
  Q^.Next := HeapPtr;
  HeapPtr := Q;
  P := nil;
end;

procedure GetMem(var P: Pointer; Size: Integer);
var
  Q, R: PBlock;
begin
  Q := nil;
  R := HeapPtr;
  while R <> nil do
  begin
    if R^.Size = Size then
    begin
      if Q = nil then
        HeapPtr := R^.Next
      else
        Q^.Next := R^.Next;

      P := R;

      Exit;
    end
    else if R^.Size >= Size + 4 then
    begin
      if Q = nil then
      begin
        HeapPtr := Ptr(Ord(R) + Size);
        HeapPtr^.Size := R^.Size - Size;
        HeapPtr^.Next := R^.Next;
      end
      else
      begin
        Q^.Next := Ptr(Ord(R) + Size);
        Q^.Next^.Size := R^.Size - Size;
        Q^.Next^.Next := R^.Next;
      end;

      P := R;

      Exit;
    end;

    Q := R;
    R := R^.Next;
  end;
end;

function MemAvail: Integer;
var
  P: PBlock;
  I: Integer;
begin
  P := HeapPtr;
  I := 0;
  while P <> nil do
  begin
    I := I + P^.Size;
    P := P^.Next;
  end;

  MemAvail := I;
end;

function MaxAvail: Integer;
var
  P: PBlock;
  I: Integer;
begin
  P := HeapPtr;
  I := 0;
  while P <> nil do
  begin
    if P^.Size > I then I := P^.Size;
    P := P^.Next;
  end;

  MaxAvail := I;
end;

procedure InitHeap(Bytes: Integer);
var
  P: Pointer;
begin
  HeapPtr := nil;
  P := GetHeapStart;
  FreeMem(P, Bytes);
end;