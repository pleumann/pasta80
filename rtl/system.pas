(* Built-ins that do not have to be defined in the compiler itself. *)

{$a+}

{$l system.asm}

(* -------------------------------------------------------------------------- *)
(* --- String support ------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

(* Built-in: procedure Val(S: String; var Scalar; var E: Integer); *)
(* Built-in: procedure Str(N: Scalar; var S: String);              *)

procedure Delete(var S: String; Start, Count: Integer);     external '__delete';
procedure Insert(S: String; var T: String; Start: Integer); external '__insert';

(* Built-in: function Concat(S: String, ...): String;              *)

function Copy(S: String; Start, Count: Integer): String;    external '__copy';
function Length(S: String): Integer;                        external '__length';
function Pos(S, T: String): Integer;                        external '__pos';

(* -------------------------------------------------------------------------- *)
(* --- Set support ---------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

(* Built-in: procedure Include(var S: Set; E: Element);     *)
(* Built-in: procedure Exclude(var S: Set; E: Element);     *)

(* -------------------------------------------------------------------------- *)
(* --- File support --------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

(* TBD *)

(* -------------------------------------------------------------------------- *)
(* --- Heap management ------------------------------------------------------ *)
(* -------------------------------------------------------------------------- *)

type
  PBlock = ^TBlock;
  TBlock = record
    Next: PBlock;
    Size: Integer;
  end;

var
  HeapPtr: PBlock absolute '__heapptr';

(* Built-in: procedure New(var P: Pointer);       *)
(* Built-in: procedure Dispose(P: Pointer);       *)

procedure FreeMem(P: Pointer; Size: Integer);     register; external '__freemem';
procedure GetMem(var P: Pointer; Size: Integer);  register; external '__getmem';

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

(* Merges all free heap blocks that happen to be adjacent in memory. To be   *)
(* called if the heap gets too fragmented after GetMem/FreeMem/New/Dispose   *)
(* have been used a lot and GetMem(P, Size) or New(P) cannot be fulfilled    *)
(* although MemAvail > Size. Note this cannot guarantee that a block of the  *)
(* required size will be available afterwards. This depends on the actual    *)
(* heap content. Should the heap consist of only free blocks, these will be  *)
(* merged into a single, contiguous block of the full heap size.             *)
procedure DefragMem;
const
  SignBit = -32768; (* $8000; flips bit 15 so signed '<' sorts addresses as  *)
                    (* if they were unsigned, even across the $8000 boundary *)
                    (* where Addr() turns negative.                          *)
var
  P, Prev, Best, BestPrev, Sorted, Tail: PBlock;
  PAddr, BestAddr: Integer;
begin
  (* Selection sort the free list by address, using the existing nodes *)
  Sorted := nil;
  Tail := nil;

  while HeapPtr <> nil do
  begin
    Best := HeapPtr;
    BestAddr := Addr(Best^) xor SignBit;
    BestPrev := nil;
    Prev := HeapPtr;
    P := HeapPtr^.Next;

    while P <> nil do
    begin
      PAddr := Addr(P^) xor SignBit;
      if PAddr < BestAddr then
      begin
        Best := P;
        BestAddr := PAddr;
        BestPrev := Prev;
      end;
      Prev := P;
      P := P^.Next;
    end;

    if BestPrev = nil then
      HeapPtr := Best^.Next
    else
      BestPrev^.Next := Best^.Next;

    Best^.Next := nil;
    if Tail = nil then
      Sorted := Best
    else
      Tail^.Next := Best;
    Tail := Best;
  end;

  (* Now merge neighbours in a single forward pass *)
  P := Sorted;
  while (P <> nil) and (P^.Next <> nil) do
  begin
    if Addr(P^) + P^.Size = Addr(P^.Next^) then
    begin
      P^.Size := P^.Size + P^.Next^.Size;
      P^.Next := P^.Next^.Next;
    end
    else
      P := P^.Next;
  end;

  HeapPtr := Sorted;
end;

(* -------------------------------------------------------------------------- *)
(* --- Standard procedures -------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

(* Built-in: procedure Break;                   *)
(* Built-in: procedure Continue;                *)
(* Built-in: procedure Exit;                    *)

{$ifdef sys_agon}
const
  Black   = 0;
  Blue    = 4;
  Red     = 1;
  Magenta = 5;
  Green   = 2;
  Cyan    = 6;
  Yellow  = 3;
  White   = 7;
{$else}
const
  Black   = 0;
  Blue    = 1;
  Red     = 2;
  Magenta = 3;
  Green   = 4;
  Cyan    = 5;
  Yellow  = 6;
  White   = 7;
{$endif}



(* -------------------------------------------------------------------------- *)
(* --- Arithmetic functions ------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

const
  MaxInt = 32767;
  MinInt = -32768;

(* Built-in: function Abs(I: Integer): Integer  *)
(* Built-in: function Abs(R: Real): Real        *)

function ArcTan(R: Real): Real; register; external 'ATN';
function Cos(R: Real): Real; register;    external 'COS';
function Exp(R: Real): Real; register;    external 'EXP';
function Frac(R: Real): Real; register;   external 'FRAC';
function Int(R: Real): Real; register;    external 'INT';
function Ln(R: Real): Real; register;     external '__ln';
function Log(R: Real): Real; register;    external '__log';
function Sin(R: Real): Real; register;    external 'SIN';
function Sqr(R: Real): Real; register;    external '__fltpwr2';
function Sqrt(R: Real): Real; register;   external '__sqrt';
function Tan(R: Real): Real; register;    external '__tan';

function Pi: Real; register;              external 'ACPI';

function MaxReal: Real; register; inline
(
  $01 / $7FFF /
  $11 / $FFFF /
  $21 / $FFFF /
  $c9
);

function MinReal: Real; register; inline
(
  $01 / $FFFF /
  $11 / $FFFF /
  $21 / $FFFF /
  $c9
);

(* -------------------------------------------------------------------------- *)
(* --- Scalar functions ----------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

(* Built-in: function Pred(Ordinal): Ordinal;     *)
(* Built-in: function Succ(Ordinal): Ordinal;     *)
(* Built-in: function Odd(Ordinal): Boolean;      *)
(* Built-in: function Even(Ordinal): Boolean;     *)

(* -------------------------------------------------------------------------- *)
(* --- Transfer functions --------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

(* Built-in: function Ord(Ordinal): Integer;      *)

function Round(R: Real): Integer; register; external '__fltrnd';
function Trunc(R: Real): Integer; register; external '__intfix';

function Chr(B: Byte): Char; register; inline
(
  $c9         (* ret          *)
);

(* -------------------------------------------------------------------------- *)
(* --- Miscellaneous standard functions ------------------------------------- *)
(* -------------------------------------------------------------------------- *)

(* Built-in: function KeyPressed: Boolean;        *)
(* Built-in: function SizeOf(XYZ): Integer;       *)
(* Built-in: function Addr(XYZ): Integer;         *)
(* Built-in: function Ptr(I: Integer): Pointer;   *)

function Hi(I: Integer): Byte; register; inline
(
  $6c /       (* ld   l,h     *)
  $26 / $00 / (* ld   h,0     *)
  $c9         (* ret          *)
);

function Lo(I: Integer): Byte; register; inline
(
  $26 / $00 / (* ld   h,0     *)
  $c9         (* ret          *)
);

function Swap(I: Integer): Integer; register; inline
(
  $7c /       (* ld   a,h     *)
  $65 /       (* ld   h,l     *)
  $6f /       (* ld   l,a     *)
  $c9         (* ret          *)
);

function UpCase(C: Char): Char; register; inline
(
  $7d /       (* ld   a,l     *)
  $fe / $61 / (* cp   'a'     *)
  $d8 /       (* ret  c       *)
  $fe / $7b / (* cp   'z' + 1 *)
  $d0 /       (* ret  nc      *)
  $cb / $ad / (* res  4,l     *)
  $c9         (* ret          *)
);

function LoCase(C: Char): Char; register; inline
(
  $7d /       (* ld   a,l     *)
  $fe / $41 / (* cp   'A'     *)
  $d8 /       (* ret  c       *)
  $fe / $5b / (* cp   'Z' + 1 *)
  $d0 /       (* ret  nc      *)
  $cb / $ed / (* set  4,l     *)
  $c9         (* ret          *)
);



var
  RandSeed1: Integer absolute 'seed1';
  RandSeed2: Integer absolute 'seed2';
  ExitCode: Integer absolute '__exitcode';
  BufLen: Byte absolute '__linemax';

function RandomInt(Range: Integer): Integer; register; external '__random';
function RandomReal: Real; register;                   external '__random48';

procedure Randomize; register; inline
(
  $ed / $5f /             (* ld   a,r             *)
  $2a / RandSeed1 /       (* ld   hl,(RandSeed1)  *)
  $ed / $5b / RandSeed2 / (* ld   de,(RandSeed2)  *)
  $53 /                   (* ld   d,e             *)
  $5c /                   (* ld   e,h             *)
  $65 /                   (* ld   h,l             *)
  $6f /                   (* ld   l,a             *)
  $22 / RandSeed1 /       (* ld   (RandSeed1),hl  *)
  $ed / $53 / RandSeed2 / (* ld   (RandSeed2),de  *)
  $c9                     (* ret                  *)
);

procedure CheckBreak; register; external '__checkbreak';

procedure CheckStack; register; external '__checkstack';

(* Built-in: procedure FillChar(var Dest; Length: Integer; Data); *)

procedure Move(var Source, Dest; Count: Integer); register; external '__move';

(* Built-in: procedure Halt([ExitCode: Byte]) *)



(* -------------------------------------------------------------------------- *)
(* --- Assertion support ---------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

(* Built-in: procedure Assert(B: Boolean); *)

var
  AssertPassed: Integer absolute '__assertpassed';
  AssertFailed: Integer absolute '__assertfailed';
