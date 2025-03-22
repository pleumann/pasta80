(* --------------------------------------------------------------------- *)
(* --- Next registers -------------------------------------------------- *)
(* --------------------------------------------------------------------- *)

function GetNextReg(Number: Integer): Integer; register; external 'nxt_getreg';
procedure SetNextReg(Number, Value: Integer); register; external 'nxt_setreg';

function GetCpuSpeed: Integer;
begin
  GetCpuSpeed := GetNextReg(7) and 3;
end;

procedure SetCpuSpeed(Value: Integer);
begin
  SetNextReg(7, Value and 3);
end;

{$l nextregs.asm}