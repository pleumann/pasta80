program Pointers;

type
  PInt = ^Integer;
  PStr = ^String[50];
  PRec = ^record X, Y: Integer; end;
  PArr = ^array[10] of Char;

  APtr = array[10] of Pointer;
    
var
  PI: PInt;
  PS: PStr;
  PR: PRec;
  PA: PArr;
  
  PPPI: ^^^Integer;

  I: Integer;

type
  THeapFragment = record
    Size: Integer;
    Next: Pointer;
  end;

  PHeapFragment = ^THeapFragment;

var
  HeapPtr: PHeapFragment;

procedure InitHeap;
begin
  
end;

procedure GetMem(var P: Pointer; Size: Integer);
var
  Q: PHeapFragment;
begin
  if Size < 4 then Size := 4;
  
  Q := HeapPtr;
  while Q <> nil do
  begin
    if Q^.Size >= Size then
    begin
    end;
  end;
end;

procedure FreeMem(var P: Pointer; Size: Integer);
begin
      
end;

begin
  PI := Ptr(Addr(I));
  PI^ := 42;
  WriteLn(I);
end.
