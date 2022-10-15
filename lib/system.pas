(* Built-ins that do not have to be defined in the compiler itself. *)

type
  PBlock = ^TBlock;
  TBlock = record
    Next: PBlock;
    Size: Integer;
  end;

var
  HeapPtr: PBlock absolute '__heapptr';

  AssertPassed: Integer absolute '__assertpassed';
  AssertFailed: Integer absolute '__assertfailed';

procedure FreeMem(P: Pointer; Size: Integer); register; external '__freemem';
(*var
  Q: PBlock;
begin
  Q := P;
  Q^.Size := Size;
  Q^.Next := HeapPtr;
  HeapPtr := Q;
  P := nil;
end;
*)

procedure GetMem(var P: Pointer; Size: Integer); register; external '__getmem';
(*var
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

  WriteLn('Out of memory error');
  while True do;
end;
*)
procedure InitHeap(Bytes: Integer);
var
  P: Pointer;
begin
  HeapPtr := nil;
  P := Ptr(32768); (* GetHeapStart; *)
  FreeMem(P, Bytes);
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

type
  TString = String[255];

function Random(Range: Integer): Integer; register; external '__random';

function Length(S: TString): Integer; stdcall; external '__length';
function Concat(S, T: TString): TString; stdcall; external '__concat';
function Pos(S, T: TString): Integer; stdcall; external '__pos';
function Copy(S: TString; Start: Integer; Count: Integer): TString; stdcall; external '__copy';
procedure Insert(S: TString; var T: TString; Start: Integer); stdcall; external '__insert';
procedure Delete(var S: TString; Start: Integer; Count: Integer); stdcall; external '__delete';

procedure Val(S: TString; var I, Code: Integer); stdcall; external '__val_int';
{
procedure Str(I: Integer; var S: TString); stdcall; external '__str_int';
}

procedure ClrScr; register; external '__clrscr';
procedure GotoXY(X, Y: Integer); register; external '__gotoxy';
procedure TextColor(I: Integer); register; external '__textfg';
procedure TextBackground(I: Integer); register; external '__textbg';
procedure CursorOn; register; external '__cursor_on';
procedure CursorOff; register; external '__cursor_off';

(* Arithmetic functions *)

(* function Abs(I: Integer): Integer  *) (* built-in *)
(* function Abs(R: Real): Real        *) (* built-in *)
function ArcTan(R: Real): Real; register; external 'ATN';
function Cos(R: Real): Real; register; external 'COS';
function Exp(R: Real): Real; register; external 'EXP';
function Frac(R: Real): Real; register; external 'FRAC';
function Int(R: Real): Real; register; external 'INT';
function Ln(R: Real): Real; register; external 'LN';
function Log(R: Real): Real; register; external 'LOG';
function Pi: Real; register; external 'ACPI';
function Sin(R: Real): Real; register; external 'SIN';
function Sqr(R: Real): Real; register; external '__fltpwr2';
function Sqrt(R: Real): Real; register; external 'SQR';
function Tan(R: Real): Real; register; external 'TAN';

(* Scalar functions *

(* Pred, Succ, Odd *)

(* Transfer functions *)

(* Chr, Ord, Round, Trunc *)

(* Miscellaneous functions *)

(* 
  Hi ld l,h, ld h,0
  KeyPressed
  Lo ld h,0
  Random
  Random(I)
  ParamCount
  ParamStr
  SizeOf ok
  Swap ld a,h, ld h,l, ld l,a
  UpCase  -> lib z80
*)