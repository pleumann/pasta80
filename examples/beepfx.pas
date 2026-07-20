(**
 * Simple demo for Shiru's BeepFX audio player.
 *)
program BeepFX;

{$l beepfx.asm}

(**
 * Plays the given effect. Valid numbers are 0 to 58. There is no check.
 *)
procedure Effect(Number: Integer); register; external 'playBasic';

var
  I: Integer;

begin
  ClrScr;
  WriteLn('*** BeepFX Demo ***');

  WriteLn;

  for I := 0 to 58 do
  begin
    GotoXY(1, 3);
    Write('Sound effect #', I:2, '...');
    Effect(I);
  end;
end.