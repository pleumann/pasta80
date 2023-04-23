type
  FileControlBlock = record
    DR: Byte;
    FN: array[0..7] of Char;
    TN: array[0..2] of Char;
    EX, S1, S2, RC: Byte;
    AL: array[0..15] of Byte;
    CR: Byte;
    RN: array[0..2] of Byte;
  end;

  Text = record
    Offset: Integer;
    Writing: Boolean;
    FCB: FileControlBlock;
    DMA: array[0..127] of Char;
  end;

procedure Assign(var T: Text; S: String);
var
  I, L, P, Q: Integer;
begin
  with T.FCB do
  begin
    L := Length(S);

    if (L > 1) and (S[2] = ':') then
    begin
      DR := Ord(UpCase(S[1])) - 64;
      Delete(S, 1, 2);
      Dec(L, 2);
    end
    else DR := 0;

    P := Pos('.', S);
    if P = 0 then P := L + 1;

    Q := P - 1;
    if Q > 8 then Q := 8;

    for I := 1 to Q do FN[I - 1] := UpCase(S[I]);
    for I := Q + 1 to 8 do FN[I - 1] := ' ';

    Q := L - P;
    if Q > 3 then Q := 3;

    for I := 1 to Q do TN[I - 1] := UpCase(S[P + I]);
    for I := Q + 1 to 3 do TN[I - 1] := ' ';

    Write(DR, ':');
    for I := 0 to 7 do Write(FN[I]);
    Write('.');
    for I := 0 to 2 do Write(TN[I]);
    WriteLn;
  end;
end;

procedure Reset(var T: Text);
var
  A: Integer;
begin
  T.FCB.EX := 0;
  T.FCB.S1 := 0;
  T.FCB.S2 := 0;
  T.FCB.RC := 0;
  T.FCB.CR := 0;

  T.Writing := False;

  A := Bdos(15, Addr(T.FCB));

  T.Offset := 128;
end;

procedure Rewrite(var T: Text);
var
  A: Integer;
begin
  T.FCB.EX := 0;
  T.FCB.S1 := 0;
  T.FCB.S2 := 0;
  T.FCB.RC := 0;
  T.FCB.CR := 0;

  T.Writing := True;

  A := Bdos(19, Addr(T.FCB));
  A := Bdos(22, Addr(T.FCB));

  T.Offset := 0;
end;

procedure ReadRec(var T: Text);
var
  A: Integer;
begin
  A := Bdos(26, Addr(T.DMA));
  A := Bdos(20, Addr(T.FCB));
  T.Offset := 0;
end;

function ReadChar(var T: Text): Char;
var
  C: Char;
begin
  if T.Offset > 127 then ReadRec(T);
  C := T.DMA[T.Offset];
  if C <> #26 then T.Offset := T.Offset + 1;
  ReadChar := C;
end;

procedure ReadLine(var T: Text; var S: String);
var
  C: Char;
begin
  S := '';

  while Length(S) < 255 do
  begin
    C := ReadChar(T);

    if C = #10 then Break;
    if C = #26 then Break;

    if C >= ' ' then S := S + C;
  end;
end;

procedure WriteRec(var T: Text);
var
  A: Integer;
begin
  A := Bdos(26, Addr(T.DMA));
  A := Bdos(21, Addr(T.FCB));
  T.Offset := 0;
end;


procedure WriteChar(var T: Text; C: Char);
begin
  if T.Offset > 127 then WriteRec(T);
  T.DMA[T.Offset] := C;
  T.Offset := T.Offset + 1;
end;

procedure WriteLine(var T: Text; S: String);
var
  I: Integer;
begin
  for I := 1 to Length(S) do WriteChar(T, S[I]);

  WriteChar(T, #13);
  WriteChar(T, #10);
end;

function IsEof(var T: Text): Boolean;
begin
  if T.Offset > 127 then ReadRec(T);
  IsEof := T.DMA[T.Offset] = #26;
end;        

procedure Close(var T: Text);
var
  A: Integer;
begin
  if T.Writing then
  begin
    WriteChar(T, #26);
    WriteRec(T);
  end;

  A := Bdos(16, Addr(T.FCB));
end;
