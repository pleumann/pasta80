program Snailfish;

(*$I lib/files.pas*)

var
  Number: TString;

function IndexOf(C: Char; S: TString; From: Integer): Integer;
var
  I: Integer;
begin
  for I := From to Length(S) do
    if S[I] = C then
    begin
      IndexOf := I;
      Exit;
    end;

  IndexOf := 0;
end;

function IsDigit(C: Char): Boolean;
begin
  IsDigit := (C >= '0') and (C <= '9');
end;

function FastVal(I: Integer; J: Integer): Integer;
var
  K: Integer;
begin
  K := 0;
  for I := I to J - 1 do
    K := 10 * K + Ord(Number[I]) - 48;

  FastVal := K;
end;

function IntToStr(I: Integer): TString;
var
  S: TString;
begin
  Str(I, S);
  Delete(S, 1, 1);
  IntToStr := S;
end;

function StrToInt(S: TString): Integer;
var
  I, J: Integer;
begin
  Val(S, I, J);
  StrToInt := I;
end;

procedure Replace(var S: TString; Pos: Integer; Len: Integer; T: TString); (* FIXME *)
begin
  Delete(S, Pos, Len);
  Insert(T, S, Pos);
end;

function Explode: Boolean;
var
  Level, I, J, K, L, M, A, B: Integer;
  C: Char;
begin
  (* Write('E'); *)

  Level := 0;

  for I := 1 to Length(Number) do
  begin
    C := Number[I];
    
    if C = '[' then
    begin
      Level := Level + 1;
      if (Level > 4) and IsDigit(Number[I + 1]) then
      begin
        (* Find the two elements belonging to this pair. *)
        J := IndexOf(',', Number, I + 1);
        K := IndexOf(']', Number, J + 1);

        (* Guaranteed to work ("always regular numbers") *)
        A := FastVal(I + 1, J); (* StrToInt(Copy(Number, I + 1, J - I - 1)); *)
        B := FastVal(J + 1, K); (* StrToInt(Copy(Number, J + 1, K - J - 1)); *)

        (* Find first regular number to the right *)
        L := K + 1;
        while (L <= Length(Number)) and not IsDigit(Number[L]) do
          L := L + 1;

        M := L + 1;
        while (M <= Length(Number)) and IsDigit(Number[M]) do
          M := M + 1;

        (* Add b, if regular number was found *)
        if L <= Length(Number) then
          Replace(Number, L, M - L, IntToStr(B + FastVal(L, M))); (* StrToInt(Copy(Number, L, M - L)))); *)

        (* Replace old pair by 0 *)
        Replace(Number, I, K - I + 1, '0');

        (* Find first regular number to the left *)
        M := I - 1;
        while (M >= 1) and not IsDigit(Number[M]) do
          M := M - 1;

        L := M - 1;
        while (L >= 1) and IsDigit(Number[L]) do
          L := L - 1;

        (* Add a, if regular number was found *)
        if L >= 1 then
          Replace(Number, L + 1, M - L, IntToStr(A + FastVal(L + 1, M + 1))); (* StrToInt(Copy(Number, L + 1, M - L)))); *)

        (* WriteLn('Explode  : ', Number); *)

        Explode := True;
        Exit;
      end;
    end
    else if C = ']' then Level := Level - 1;
  end;

  Explode := False;
end;

function Split: Boolean;
var
  I, J, A, B, C: Integer;
begin
  (* Write('S'); *)

  for I := 1 to Length(Number) do
  begin
    if IsDigit(Number[I]) then
    begin
      J := I + 1;
      while (J <= Length(Number)) and IsDigit(Number[J]) do
        J := J + 1;

      if J > I + 1 then
      begin
        A := FastVal(I, J); (* StrToInt(Copy(Number, I, J - I)); *)
        B := A / 2;
        C := A - B;

        Replace(Number, I, J - I, '' + '[' + IntToStr(B) + ',' + IntToStr(C) + ']');

        (* WriteLn('Split    : ', Number); *)

        Split := True;
        Exit;
      end;
    end;
  end;

  Split := False;
end;

function Magnitude: Integer;
var
  A, B, I, J, K, L, M: Integer;
  Again: Boolean;
begin
  (* Write('M'); *)
  (* WriteLn('Magnitude: ', Number); *)

  Again := True;
  while Again do
  begin

    Again := False;

    for I := 1 to Length(Number) - 5 do
    begin
      if Number[I] <> '[' then Continue;

      J := I + 1;
      if not IsDigit(Number[J]) then Continue;

      K := J + 1;
      while IsDigit(Number[K]) do K := K + 1;

      if Number[K] <> ',' then Continue;

      L := K + 1;
      if not IsDigit(Number[L]) then Continue;

      M := L + 1;
      while IsDigit(Number[M]) do M := M + 1;

      A := FastVal(J, K); (* StrToInt(Copy(Number, J, K - J)); *)
      B := FastVal(L, M); (* StrToInt(Copy(Number, L, M - L)); *)
      Replace(Number, I, M - I + 1, IntToStr(3 * A + 2 * B));
    
      Again := True;
      Break;
    end;
  end;

  Magnitude := StrToInt(Number);
end;

procedure Process(S: TString);
var
  Again: Boolean;
begin
  if Length(Number) = 0 then
    Number := S
  else
    Number := '' + '[' + Number + ',' + S + ']';

  (* WriteLn(Number); *)

  Again := True;
  while Again do
  begin
    Again := False;
    if Explode then
      Again := True
    else if Split then
      Again := True;
  end;
end;

var
  F: Text;
  S: array[100] of TString; (* FIXME: Smaller strings result in wrong behavior *)
  I, J, M, Part1, Part2: Integer;

begin
  WriteLn('*** AoC 2021.18 Snailfish ***');
  WriteLn;
    
  Number := '';

  Assign(F, 'INPUT   .TXT');
  Reset(F);
  for I := 0 to 99 do
    ReadLine(F, S[I]);
  Close(F);

  for I := 0 to 99 do
  begin
    Process(S[I]);
    Write('.');
    if I mod 25 = 24 then WriteLn;
  end;

  WriteLn;
  WriteLn('Part 1: ', Magnitude);
  WriteLn;

  for I := 0 to 99 do
  begin
    Write('.');
    if I mod 25 = 24 then WriteLn;

    for J := 0 to 99 do
      if I <> J then
      begin
        Number := '';
        Process(S[I]);
        Process(S[J]);
        M := Magnitude;
        if M > Part2 then Part2 := M;
      end;
  end;

  WriteLn;
  WriteLn('Part 2: ', Part2);
  WriteLn;
end.