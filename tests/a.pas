program A;

type
  Month = (Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sep, Oct, Nov, Dec);

  Day = (Working, Weekend, Holiday);

var
  Cal1: array [Month] of array [1..31] of Day;
  Cal2: array [Month, 1..31] of Day;

begin
  Cal1[Jan, 1] := Holiday;
  WriteLn('Jan 1st is ', Cal1[Jan][1]);
  WriteLn;

  Cal2[Dec, 25] := Holiday;
  WriteLn('Dec 25th is ', Cal2[Dec][25]);
  WriteLn;

  WriteLn('Cal1');
  WriteLn('- Dim 1: ', Low(Cal1), '..', High(Cal1), ', size=', SizeOf(Cal1));
  WriteLn('- Dim 2: ', Low(Cal1[Jan]), '..', High(Cal2[Jan]), ', size=', SizeOf(Cal1[Jan]));
  WriteLn;

  WriteLn('Cal2');
  WriteLn('- Dim 1: ', Low(Cal2), '..', High(Cal2), ', size=', SizeOf(Cal2));
  WriteLn('- Dim 2: ', Low(Cal2[Jan]), '..', High(Cal2[Jan]), ', size=', SizeOf(Cal2[Jan]));
  WriteLn;
end.