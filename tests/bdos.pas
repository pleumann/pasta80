type
  String255 = string[255];

  FileControlBlock = record
    DR: Byte;
    FN: array[8] of Char;
    TN: array[3] of Char;
    EX, S1, S2, RC: Byte;
    AL: array[16] of Byte;
    CR: Byte;
    RN: array[3] of Byte;
  end;

  Text = record
    Offset: Integer;
    Writing: Boolean;
    FCB: FileControlBlock;
    DMA: array[128] of Char;
  end;

var
  T: Text;
  S: String255;

procedure Assign(var T: Text; S: String255);
var
  I: Integer;
begin
  T.FCB.DR := 0;

  for I := 1 to 8 do
    T.FCB.FN[I - 1] := S[I];

  for I := 10 to 12 do
    T.FCB.TN[I - 10] := S[I];
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

procedure ReadLine(var T: Text; var S: String255);
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

procedure WriteLine(var T: Text; S: String255);
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

begin
  Assign(T, 'TEST    .TXT');

  Rewrite(T);

  WriteLine(T, 'The small brown');
  WriteLine(T, 'fox jumped over');
  WriteLine(T, 'the lazy dog.');

  Close(T);

  Reset(T);

  while not IsEof(T) do
  begin
    ReadLine(T, S);
    WriteLn(S);
  end;

  Close(T);
end.