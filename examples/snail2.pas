program Snailfish;

(*$I lib/files.pas*)

type
  PNode = ^TNode;
  TNode = record
    Value: Integer;
    Up, Left, Right: PNode;
  end;

function IsLeaf(Node: PNode): Boolean;
begin
  IsLeaf := (Node^.Left = nil) and (Node^.Right = nil);
end;

function Parse(S: TString): PNode;
var
  I: Integer;
  C: Char;

  function ParseHelper: PNode;
  var
    Left, Right, Node: PNode;
  begin
    (*WriteLn(Ord(HeapPtr));
    WriteLn(I, ' ', C);*)
    if C = '[' then
    begin
      I := I + 1;
      C := S[I];

      New(Node);
      Node^.Value := 0;

      Node^.Left := ParseHelper;
      Node^.Left^.Up := Node;

      if C <> ',' then WriteLn('"," expected at ', I);

      I := I + 1;
      C := S[I];

      Node^.Right := ParseHelper;
      Node^.Right^.Up := Node;

      if C <> ']' then WriteLn('"]" expected at ', I);

      I := I + 1;
      C := S[I];

      ParseHelper := Node;
      Exit;
    end
    else if (C >= '0') and (C <= '9') then
    begin
      New(Node);
      Node^.Left := nil;
      Node^.Right := nil;

      Node^.Value := Ord(C) - 48;
      I := I + 1;
      C := S[I];
      while (C >= '0') and (C <= '9') do
      begin
        Node^.Value := 10 * Node^.Value + Ord(C) - 48;
        I := I + 1;
        C := S[I];
      end;

      ParseHelper := Node;
      Exit;
    end
    else
    begin
      WriteLn('Unexpected "' + C + '" at ', I);
      ParseHelper := nil;
    end;
  end;
begin
  I := 1;
  C := S[I];

  Parse := ParseHelper;
end;

function Explode(N: PNode; Depth: Integer): Boolean;
var
  Again: Boolean;
  P, Q, L, R: PNode;
begin
  if N = nil then WriteLn('Warning: Explode(nil) called');

  Again := False;

  if (Depth >= 4) then if not IsLeaf(N) (*and IsLeaf(N^.Left) and IsLeaf(N^.Right)*) then
  begin
    P := N^.Up;
    Q := N;
    while (P <> nil) and (P^.Left = Q) do
    begin
      Q := P;
      P := P^.Up;
    end;

    if P <> nil then
    begin
      L := P^.Left;
      while not IsLeaf(L) do
        L := L^.Right;

      L^.Value := L^.Value + N^.Left^.Value;
    end;

    P := N^.Up;
    Q := N;
    while (P <> nil) and (P^.Right = Q) do
    begin
      Q := P;
      P := P^.Up;
    end;

    if P <> nil then
    begin
      R := P^.Right;
      while not IsLeaf(R) do
        R := R^.Left;

      R^.Value := R^.Value + N^.Right^.Value; 
    end;

    N^.Value := 0;
    Dispose(N^.Left);
    N^.Left := nil;
    Dispose(N^.Right);
    N^.Right := nil;

    Again := True;
  end;
  
  if N^.Left <> nil then
    if Explode(N^.Left, Depth + 1) then
      Again := True;

  if N^.Right <> nil then
    if Explode(N^.Right, Depth + 1) then
      Again := True;

  Explode := Again;
end;

function Split(N: PNode): Boolean;
begin
  if N = nil then WriteLn('Warning: Split(nil) called');

  if IsLeaf(N) then
  begin
    if N^.Value >= 10 then
    begin
      New(N^.Left);
      N^.Left^.Left := nil;
      N^.Left^.Right := nil;
      N^.Left^.Up := N;
      N^.Left^.Value := N^.Value / 2;

      New(N^.Right);
      N^.Right^.Left := nil;
      N^.Right^.Right := nil;
      N^.Right^.Up := N;
      N^.Right^.Value := N^.Value - N^.Left^.Value;

      Split := True;
      Exit;
    end;
  end
  else
  begin
    if Split(N^.Left) then
    begin
      Split := True;
      Exit;
    end;

    if Split(N^.Right) then
    begin
      Split := True;
      Exit;
    end;
  end;

  Split := False;
end;

function Magnitude(N: PNode): Integer;
begin
  if N = nil then WriteLn('Warning: Magnitude(nil) called');

  if IsLeaf(N) then
    Magnitude := N^.Value
  else
  begin
    Magnitude := 3 * Magnitude(N^.Left) + 2 * Magnitude(N^.Right);
    Dispose(N^.Left);
    N^.Left := nil;
    Dispose(N^.Right);
    N^.Right := nil;
  end;
end;

procedure Dump(N: PNode);
begin
  if IsLeaf(N) then
    Write(N^.Value)
  else
  begin
    Write('[');
    Dump(N^.Left);
    Write(',');
    Dump(N^.Right);
    Write(']');
  end;
end;

var
  Root: PNode;

procedure Process(S: TString);
var
  Again: Boolean;
  Node: PNode;
begin
  (*WriteLn(S);*)
  
  if Root = nil then
  begin
    Root := Parse(S);
    Root^.Up := nil;
  end
  else
  begin
    New(Node);
    Node^.Value := 0;
    Node^.Left := Root;
    Node^.Left^.Up := Node;
    Node^.Right := Parse(S);
    Node^.Right^.Up := Node;
    Node^.Up := nil;
    Root := Node;
  end;

  Again := True;
  while Again do
  begin
    Again := False;
    if Explode(Root, 0) then
      Again := True
    else if Split(Root) then
      Again := True;
  end;
end;

var
  F: Text;
  S: array[100] of String[79]; (* FIXME: Smaller strings result in wrong behavior *)
  I, J, M, Part1, Part2: Integer;
  T: TString;

begin
  InitHeap(8192);

  WriteLn('*** AoC 2021.18 Snailfish ***');
  WriteLn;
  WriteLn('HeapPtr=', Ord(HeapPtr), ' MemAvail=', MemAvail, ' MaxAvail=', MaxAvail);
  WriteLn;

  Assign(F, 'INPUT   .TXT');
  Reset(F);
  for I := 0 to 99 do
  begin
    (*WriteLn(Ord(HeapPtr), ' ', MemAvail, ' ', MaxAvail);*)
    ReadLine(F, T);
    S[I] := T;
  end;
  Close(F);

  Root := nil;

  for I := 0 to 99 do
  begin
    Process(S[I]);
    Write('.');
    if I mod 25 = 24 then WriteLn;
  end;

  WriteLn;
  WriteLn('Part 1: ', Magnitude(Root));
  WriteLn;

  Dispose(Root);
  Root := nil;
   (*) WriteLn(Ord(HeapPtr), ' ', MemAvail, ' ', MaxAvail); *)

  for I := 0 to 99 do
  begin
    Write('.');
    if I mod 25 = 24 then WriteLn;

    for J := 0 to 99 do
      if I <> J then
      begin
        Root := nil;
        Process(S[I]);
        Process(S[J]);
        M := Magnitude(Root);
        Dispose(Root);
        Root := nil;
        if M > Part2 then Part2 := M;
      end;
  end;

  WriteLn;
  WriteLn('Part 2: ', Part2);
  WriteLn;

  WriteLn('HeapPtr=', Ord(HeapPtr), ' MemAvail=', MemAvail, ' MaxAvail=', MaxAvail);
  WriteLn;
end.