(**
 * Hilbert curve via recursive Turtle graphics.
 *)
program Hilbert;

{$ifdef SYS_CPM}
  {$error Agon or ZX Spectrum 48K/128K/Next required.}
{$endif}

{$a-}

const
  {$ifdef SYS_AGON}
    StartX = 100; StartY = 20; Order = 6; Step = 7;
  {$else}
    StartX =  50; StartY = 10; Order = 5; Step = 5;
  {$endif}

procedure Recurse(N, DX, DY : Integer);
begin
  if N > 0 then
  begin
    Recurse(N - 1,  DY,  DX); Draw( DX,  DY);
    Recurse(N - 1,  DX,  DY); Draw( DY,  DX);
    Recurse(N - 1,  DX,  DY); Draw(-DX, -DY);
    Recurse(N - 1, -DY, -DX)
  end
end;

begin
  ClrScr;
  Plot(StartX, StartY);
  Recurse(Order, Step, 0)
end.