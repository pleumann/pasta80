program Sets;

type
  Day = (Mon, Tue, Wed, Thu, Fri, Sat, Sun);

  DaySet = set of Day;

  String5 = String[255];

var
  AllDays, Working, Weekend, Mon_Thu, Thu_Sun: DaySet;

  Lower, Upper, Digit, Alpha: set of Char;

  Primes: set of Byte;

  I: Byte;

procedure DumpSet(DS: DaySet);
var
  D: Day;
  First: Boolean;
begin
  Write('[');
  First := True;
  for D := Mon to Sun do
  begin
    if D in DS then
    begin
      if First then First := False else Write(',');
      Write(D);
    end;
  end;
  WriteLn(']');
end;

function Check(B: Boolean): String5;
begin
  if B then Check := '  X  ' else Check := '     ';  
end;

begin
  AllDays := [Mon..Sun];
  Working := [Mon..Fri];
  Weekend := [Sat, Sun];
  Mon_Thu := [Mon..Thu];
  Thu_Sun := [Thu..Sun];

  Write('AllDays .............. '); DumpSet(AllDays);
  Write('Working .............. '); DumpSet(Working);
  Write('Weekend .............. '); DumpSet(Weekend);

  WriteLn;

  WriteLn('Fri in Weekend ....... ', Fri in Weekend);
  WriteLn('Sat in Weekend ....... ', Sat in Weekend);

  WriteLn;

  Write('Working + Weekend .... '); DumpSet(Working + Weekend);
  Write('AllDays - Weekend .... '); DumpSet(AllDays - Weekend);
  Write('Mon_Thu * Thu_Sun .... '); DumpSet(Mon_Thu * Thu_Sun);

  WriteLn;

  WriteLn('Weekend = Weekend .... ', Weekend = Weekend);
  WriteLn('Weekend <> Weekend ... ', Weekend <> Weekend);
  WriteLn('Working <= AllDays ... ', Working <= AllDays);
  WriteLn('Working >= AllDays ... ', Working >= AllDays);
  WriteLn('Weekend <= Weekend ... ', Weekend <= Weekend);
  WriteLn('Weekend >= Weekend ... ', Weekend >= Weekend);

  Lower := ['a'..'z'];
  Upper := ['A'..'Z'];
  Digit := ['0'..'9'];
  Alpha := Lower + Upper + Digit;

  WriteLn;

  WriteLn('Chr Lower Upper Digit Alpha');
  WriteLn('''m'' ', Check('m' in Lower), ' ', Check('m' in Upper), ' ', 
                   Check('m' in Digit), ' ', Check('m' in Alpha));
  WriteLn('''M'' ', Check('M' in Lower), ' ', Check('M' in Upper), ' ', 
                   Check('M' in Digit), ' ', Check('M' in Alpha));
  WriteLn('''5'' ', Check('5' in Lower), ' ', Check('5' in Upper), ' ', 
                   Check('5' in Digit), ' ', Check('5' in Alpha));

  WriteLn;

  Primes := [2, 3, 5, 7, 11, 13, 17, 23, 29, 31];
  Write('Some primes:');
  for I := 0 to 31 do
    if I in Primes then
      Write(I);
  WriteLn;
end.
