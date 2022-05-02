program Sets;

type
  Dozen = 1..12;
  Days = (Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday);
  Weekdays = Monday..Friday;
  Weekends = Saturday..Sunday;

  Workdays = set of Days;
  
var
  Num: Dozen;
  D: Days;
  WD: Weekdays;
  WE: Weekends;
  Dirs: (North, South, East, West);
  Horz: North..South;
  Vert: East..West;

  Move: set of Dirs;

begin
  WriteLn(Left in Move);
end.
