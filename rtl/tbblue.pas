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

(**
 * Returns the 8K memory page currently visible in the given slot. Slots range
 * from 0..7. Pages range from 0..111 for 1 MB RAM and from 0..223 for 2 MB RAM.
 *)
function GetMemPage(Slot: Byte): Byte;
begin
end;

(**
 * Sets the 8K memory page currently visible in the given slot. Slots range
 * from 0..7. Pages range from 0..111 for 1 MB RAM and from 0..223 for 2 MB RAM.
 * The caller is responsible for all necessary precautions.
 *)
procedure SetMemPage(Slot, Page: Byte);
begin
end;

{$l tbblue.asm}
{$l overlays.asm}