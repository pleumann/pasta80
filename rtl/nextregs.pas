(* --------------------------------------------------------------------- *)
(* --- Next registers -------------------------------------------------- *)
(* --------------------------------------------------------------------- *)

(**
 * Gets the current value of the given Next register.
 *)
function GetNextReg(Number: Integer): Integer; register; external 'nxt_getreg';

(**
 * Sets the given Next register to the given value.
 *)
procedure SetNextReg(Number, Value: Integer); register; external 'nxt_setreg';

(**
 * Gets the current CPU speed, with 0..3 meaning 3.5 MHz, 7 MHz, 14 MHz and
 * 28 MHz, respectively.
 *)
function GetCpuSpeed: Integer;
begin
  GetCpuSpeed := GetNextReg(7) and 3;
end;

(**
 * Sets the CPU speed, with 0..3 meaning 3.5 MHz, 7 MHz, 14 MHz and 28 MHz,
 * respectively.
 *)
procedure SetCpuSpeed(Value: Integer);
begin
  SetNextReg(7, Value and 3);
end;

{$l nextregs.asm}