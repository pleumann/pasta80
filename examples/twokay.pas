program TwoKay;

const
  Margin: string = '                         ';

const
  Background: array[0..11] of Integer = (0, 1, 2, 3, 4, 5, 6, 7, 7, 7, 7, 7);
  Foreground: array[0..11] of Integer = (7, 7, 7, 7, 0, 0, 0, 0, 1, 2, 3, 4);

  Logo: array[0..4] of string = (
    ' 22222   6666  4    4  5555  ',
    '      2 6    6 4    4 5    5 ',
    '  2222  6    6 444444  5555  ',
    ' 2      6    6      4 5    5 ',
    ' 222222  6666       4  5555  '
  );

var
  Board: array[0..5, 0..5] of Integer;
  Score: Integer;
  Color: Boolean;

function ReadKey: CHar;
begin
  ReadKey := Chr(BDOS(1, 0));
end;

procedure SetColors(Index: Integer);
begin
  if Color then
    Write(#27'S', Background[Index], #27'T', Foreground[Index])
  else if Index = 0 then 
    Write(#27'q')
  else
    Write(#27'p');
end;

procedure PrintLogo(S: string);
var
  I: Integer;
  C: Char;
begin
  Write(Margin);
  for I := 1 to Length(S) do
  begin
    C := S[I];
    if C = ' ' then Write(' ') else
    begin
      SetColors(Ord(C) - 48);
      Write(' ');
      SetColors(0);
    end;
  end;
  WriteLn;
end;

procedure StartGame;
var
  I, J: Integer;
begin
  for I := 1 to 4 do
  begin
    Board[0, I] := -1;
    Board[4, I] := -1;
    Board[I, 0] := -1;
    Board[I, 4] := -1;

    for J := 1 to 4 do
      Board[I, J] := 0;
  end;
end;

procedure DrawBoard;
var
  I, J, K: Integer;
begin
  Write(#27'f');

  GotoXY(1, 13);
  WriteLn(Margin, '+------+------+------+------+');

  for I := 1 to 4 do
  begin
    Write(Margin, '|');

    for J := 1 to 4 do
    begin
      K := Board[I, J];

      SetColors(K);
      if K = 0 then
        Write('      ')
      else
        Write(1 shl K:5, ' ');
      SetColors(0);
      Write('|');
    end;

    WriteLn;
    WriteLn(Margin, '+------+------+------+------+');
  end;

  WriteLn;
  Write(#27'e');
end;

function Max(I, J: Integer): Integer;
begin
  if I > J then Max := I else Max := J;
end;

procedure AddTile;
var
  I, J, K: Integer;
begin
  if Random(10) = 0 then K := 2 else K := 1;

  repeat
    I := 1 + Random(4);
    J := 1 + Random(4);
    WriteLn(I, '/', J, '=', Board[I, J]);
  until Board[I, J] = 0;

  Board[I, J] := K;
  Score := Max(Score, K);
end;

function CanMove: Boolean;
var
  I, J, H, V, LastH, LastV: Integer;
begin
  CanMove := True;

  for I := 1 to 4 do
  begin
    LastH := -1;
    LastV := -1;

    for J := 1 to 4 do
    begin
      H := Board[I, J];
      V := Board[J, I];

      if (H = 0) or (V = 0) or (H = LastH) or (V = LastV) then Exit;

      LastH := H;
      LastV := V;
    end;
  end;

  CanMove := False;
end;

function MoveEast: Boolean;
var
  I, J, K, L: Integer;
  B: Boolean;
begin
  MoveEast := False;

  for I := 1 to 4 do
  begin
    B := True;

    for J := 3 downto 1 do
    begin
      K := Board[I, J];

      if K <> 0 then
      begin
        L := J;

        while Board[I, L + 1] = 0 do
        begin
          Board[I, L] := 0;
          Inc(L);
          Board[I, L] := K;
          MoveEast := True;
        end;

        if (Board[I, L + 1] = K) and B then
        begin
          Board[I, L] := 0;
          Inc(L);
          Board[I, L] := K + 1;
          Score := Max(Score, K + 1);
          MoveEast := True;
          B := False;
        end
        else B := True;
      end;
    end;
  end;  
end;

procedure Rotate(Dir: Integer);
var
  I, J: Integer;
begin
  while Dir > 0 do
  begin
    for I := 1 to 3 do
    begin
      J := Board[1, I];
      Board[1, I] := Board[I, 4];
      Board[I, 4] := Board[4, 5 - I];
      Board[4, 5 - I] := Board[5 - I, 1];
      Board[5 - I, 1] := J;
    end;

    J := Board[2, 2];
    Board[2, 2] := Board[2, 3];
    Board[2, 3] := Board[3, 3];
    Board[3, 3] := Board[3, 2];
    Board[3, 2] := J;

    Dec(Dir);
  end;
end;

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

function GetR: Integer; register; inline
(
  $ed / $5f /
  $26 / $00 /
  $6f /
  $c9
);

var
  C: Char;
  Moved: Boolean;
  I, J, K: Integer;

begin
  Write('Do you want ZX Spectrum Next colors (y/n)? '); C := ReadKey;
  Color := C = 'y';

  (*Randomize;*)

  ClrScr;

  SetColors(0);
  WriteLn(Margin, '  Joerg''s Pascal version of');
  WriteLn;

  for I := 0 to 4 do
    PrintLogo(Logo[I]);

  WriteLn;
  WriteLn(Margin, '  for classic 8-bit CP/M-80');
  WriteLn;
  WriteLn(Margin, 'Use WASD to move & X to exit.');
  WriteLn;

  StartGame;
  AddTile;
  AddTile;
  DrawBoard;

  while CanMove and (Score <= 10) do
  begin
    Write(Margin, 'Your move: '); C := ReadKey;

    Moved := False;

    case C of
      'w': begin Rotate(3); Moved := MoveEast; Rotate(1); end;
      'a': begin Rotate(2); Moved := MoveEast; Rotate(2); end;
      's': begin Rotate(1); Moved := MoveEast; Rotate(3); end;
      'd': Moved := MoveEast;
      'x': Break;
    end;

    GotoXY(42, 23);

    if Moved then
    begin
      Write(#27'K');
      AddTile;
    end
    else Write('Invalid move!'#7);

    DrawBoard;
  end;

  GotoXY(42, 23);

  if C = 'x' then
    Write('     Goodbye!')
  else if Score > 10 then
    Write(' You win! :-)') 
  else
    Write('You lose! :-(');
end.

(* Tonight: ReadKey, Color y/n, Halt, * for most recent *)