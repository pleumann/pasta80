type
  Str = string[255];

var
  S: Str;
  
procedure UpperCase(var Strg: Str);
begin
  inline(
    $dd/$6e/<Strg/      (* ld l,[ix+Strg]    *)
    $dd/$66/<Strg+1/    (* ld h,[ix+Strg+1]  *)
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
