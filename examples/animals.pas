program Animals;

type
  AnimalRec = record
    Yes, No: Integer;
    Text: String[75];
  end;

var
  F: file of AnimalRec;

procedure OpenFile;
var
  R: AnimalRec;
begin
  Assign(F, 'animals.dat');
  {$i-}
  Reset(F);
  {$i+}
  if IOResult <> 0 then
  begin
    Rewrite(F);

    with R do
    begin
      Text := 'Does it eat ants?';
      Yes := 1; No := 2;
    end;

    Write(F, R);

    with R do
    begin
      Text := 'a pangolin';
      Yes := -1; No := -1;
    end;

    Write(F, R);

    with R do
    begin
      Text := 'an ant';
      Yes := -1; No := -1;
    end;

    Write(F, R);
  end;
end;

procedure CloseFile;
begin
  Close(F);
end;

function YesOrNo: Boolean;
var
  C: Char;
begin
  Write('> ');

  repeat
    C := UpCase(ReadKey);
  until (C = 'Y') or (C = 'N');

  if C = 'Y' then
  begin
    WriteLn('Yes');
    YesOrNo := True;
  end
  else
  begin
    WriteLn('No');
    YesOrNo := False;
  end;
end;

procedure PlayGame;
var
  R, Q, A: AnimalRec;
begin
  Seek(F, 0);
  Read(F, R);

  with R do
  begin
    while Yes <> -1 do
    begin
      WriteLn(Text, ' (y/n)?');
      if YesOrNo then Seek(F, Yes) else Seek(F, No);
      Read(F, R);
    end;

    WriteLn('It must be ', Text, '.');
    WriteLn('Is that correct (y/n)?');
  end;

  if YesOrNo then
    WriteLn('Ha! I got it. :)')
  else
  begin
    WriteLn('Oh, a new animal. Exciting!');
    WriteLn('What is it?');
    
    Write('> It is ');
    ReadLn(A.Text);

    WriteLn('Please give me a question that');
    WriteLn('distinguishes ', A.Text);
    WriteLn('from ', R.Text, '.');
    Write('> ');
    ReadLn(Q.Text);

    Q.Text[1] := UpCase(Q.Text[1]);
    if Q.Text[Length(Q.Text)] <> '?' then Q.Text := Q.Text + '?';

    A.Yes := -1;
    A.No := -1;

    WriteLn('And for ', A.Text, ',');
    WriteLn('what would the answer be (y/n)?');
    if YesOrNo then
    begin
      Q.Yes := FileSize(F);
      Q.No := FileSize(F) + 1;
    end
    else
    begin
      Q.Yes := FileSize(F) + 1;
      Q.No := FileSize(F);
    end;

    Seek(F, FilePos(F) - 1);
    Write(F, Q);

    Seek(F, FileSize(F));
    Write(F, A);
    Write(F, R);

    WriteLn('Thank you! I''ve memorized that.');
  end;
end;

begin
  OpenFile;

  ClrScr;

  WriteLn('Let''s play a game!');
  WriteLn;
  WriteLn('You think of an animal.');
  WriteLn('I will try to guess it.');
  WriteLn;
  WriteLn('Shall we start (y/n)?');
  
  while YesOrNo do
  begin
    PlayGame;

    WriteLn;
    WriteLn('Do you want to play again (y/n)?');
  end;

  WriteLn;
  Write('Bye!');
  WriteLn;

  CloseFile;
end.