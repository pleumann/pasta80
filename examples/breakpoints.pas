(**
 * Demo for breakpoints via "Debug". Start in debugger to see how it works.
 *)
program Breakpoints;

var
  I: Integer;

begin
  WriteLn('Foo');
  Debug;                  (* Unconditional breakpoint. *)
  WriteLn('Bar');

  for I := 1 to 20 do
  begin
    WriteLn(I);
    Delay(100);
    Debug(I mod 6 = 0);   (* Conditional beakpoint.    *)
  end;
end.