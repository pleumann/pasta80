type
  Str = string[255];

var
  S: Str;
  
procedure UpperCase(var Strg: Str);
begin
  inline(
    $dd/$7e/<Strg/      (* ld a,[ix+Strg]    *)
    $6f/                (* ld l,a            *)
    $dd/$7e/<Strg+1/    (* ld a,[ix+Strg+1]  *)
    $67/                (* ld h,a            *)
    $46/                (* ld b,(hl)         *)
    $04/                (* inc b             *)
                        (* l1:               *)
    $05/                (* dec b             *)
    $ca/*+20/           (* jp z,l2           *)
    $23/                (* inc hl            *)
    $7e/                (* ld a,(hl)         *)
    $fe/$61/            (* cp 'a'            *)
    $da/*-9/            (* jp c,l1           *)
    $fe/$7b/            (* cp 'z'+1          *)
    $d2/*-14/           (* jp nc,l1          *)
    $d6/$20/            (* sub 20h           *)
    $77/                (* ld (hl),a         *)
    $c3/*-20            (* jp l1             *)
  );                    (* l2:               *)
end;

begin
  S := 'hello, inline world!';
  WriteLn(S);
  UpperCase(S);
  WriteLn(S);
end.
