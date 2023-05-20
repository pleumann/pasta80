(* Built-ins that do not have to be defined in the compiler itself. *)

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

procedure InitHeap(Bytes: Integer);
var
  P: Pointer;
begin
  HeapPtr := nil;
  P := Ptr(32768); (* GetHeapStart; *)
  FreeMem(P, Bytes);
end;

(* -------------------------------------------------------------------------- *)
(* --- Standard procedures -------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

(* Built-in: procedure Break;                   *)
(* Built-in: procedure Continue;                *)
(* Built-in: procedure Exit;                    *)

procedure ClrScr; register;                 external '__clrscr';
procedure GotoXY(X, Y: Integer); register;  external '__gotoxy';
procedure CursorOn; register;               external '__cursor_on';
procedure CursorOff; register;              external '__cursor_off';

procedure ConOut(C: Char); register;        external '__conout';

procedure ClrEol; register; inline
(
  $2e /                       (* ld   l,27      *)
  $cd / ConOut /              (* call ConOut    *)
  $3e / 'K' /                 (* ld   l,'K'     *)
  $cd / ConOut /              (* call ConOut    *)
  $c9                         (* ret            *)
);

procedure ClrEos; register; inline
(
  $2e /                       (* ld   l,27      *)
  $cd / ConOut /              (* call ConOut    *)
  $3e / 'J' /                 (* ld   l,'J'     *)
  $cd / ConOut /              (* call ConOut    *)
  $c9                         (* ret            *)
);

procedure InsLine; register; inline
(
  $2e /                       (* ld   l,27      *)
  $cd / ConOut /              (* call ConOut    *)
  $3e / 'L' /                 (* ld   l,'L'     *)
  $cd / ConOut /              (* call ConOut    *)
  $c9                         (* ret            *)
);

procedure DelLine; register; inline
(
  $2e /                       (* ld   l,27      *)
  $cd / ConOut /              (* call ConOut    *)
  $3e / 'M' /                 (* ld   l,'M'     *)
  $cd / ConOut /              (* call ConOut    *)
  $c9                         (* ret            *)
);

procedure TextColor(I: Integer); register;      external '__textfg';
procedure TextBackground(I: Integer); register; external '__textbg';

(* -------------------------------------------------------------------------- *)
(* --- Arithmetic functions ------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

{$A math48.z80}

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
function Ln(R: Real): Real; register;     external 'LN';
function Log(R: Real): Real; register;    external 'LOG';
function Sin(R: Real): Real; register;    external 'SIN';
function Sqr(R: Real): Real; register;    external '__fltpwr2';
function Sqrt(R: Real): Real; register;   external 'SQR';
function Tan(R: Real): Real; register;    external 'TAN';

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
function Trunc(R: Real): Integer; register; external 'FIX';

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
  CmdLine: String absolute $80;

function ParamCount: Byte;
var
  C, D: Boolean;
  I, J: Byte;
begin
  C := True;
  J := 0;

  for I := 1 to Length(CmdLine) do
  begin
    D := CmdLine[I] > ' ';
    if not C and D then Inc(J);
    C := D;
  end;

  ParamCount := J;
end;

function ParamStr(I: Byte): String;
var
  C, D: Boolean;
  J, K: Byte;
begin
  C := True;
  K := 1;

  for J := 1 to Length(CmdLine) do
  begin
    D := CmdLine[J] > ' ';

    if not C and D then
      K := J
    else if C and not D then
    begin
      if I = 0 then
      begin
        Dec(J);
        Break;
      end;

      Dec(I);
    end;

    C := D;
  end;

  if I = 0 then
    ParamStr := Copy(CmdLine, K, J - K + 1)
  else
    ParamStr := '';
end;

function Random(Range: Integer): Integer; register; external '__random';
function RandomReal: Real; register;                external '__random48';

procedure CheckBreak; register; external '__checkbreak';

(* Built-in: procedure FillChar(var Dest; Length: Integer; Data); *)

procedure Move(var Source, Dest; Count: Integer); register; external '__move';

(* -------------------------------------------------------------------------- *)
(* --- Assertion support ---------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

(* Built-in: procedure Assert(B: Boolean); *)

var
  AssertPassed: Integer absolute '__assertpassed';
  AssertFailed: Integer absolute '__assertfailed';
