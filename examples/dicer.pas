program Dice;

const
  Stats: array[1..6] of string = ('STR', 'CON', 'DEX', 'INT', 'WIS', 'CHA');

var
  Rolls, Dice, I, J, K, Sum: Integer;
  DnD: Boolean;

begin
  Randomize;

  WriteLn('This program rolls six-sided dice for you.');
  WriteLn;

  Write('How many rolls? '); ReadLn(Rolls);
  Write('How many dice? '); ReadLn(Dice);
  WriteLn;

  DnD := (Rolls = 6) and (Dice = 3);

  if DnD then
  begin
    WriteLn('Okay, here''s your new DnD character. :)');
    WriteLn;
  end;

  for I := 1 to Rolls do
  begin
    Sum := 0;
    for J := 1 to Dice do
    begin
      K := 1 + Random(6);
      Write(K:4);
      Inc(Sum, K);
    end;
    Write(' -> ', Sum:5);
    if DnD then Write(' ', Stats[I]);
    WriteLn;
  end;

  WriteLn;
end.