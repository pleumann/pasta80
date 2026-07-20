(**
 * Simple demo for Shiru's BeepFX audio player.
 *)
program BeepFX;

{$l beepfx.asm}

(**
 * Plays the given effect. Valid numbers are 0 to 58. There is no check.
 *
 *  0  SHOT_1        15  HIT_1         30  ITEM_3        45  SELECT_6
 *  1  SHOT_2        16  HIT_2         31  ITEM_4        46  SELECT_7
 *  2  JUMP_1        17  HIT_3         32  ITEM_5        47  ALARM_1
 *  3  JUMP_2        18  HIT_4         33  ITEM_6        48  ALARM_2
 *  4  PICK          19  JET_BURST     34  SWITCH_1      49  ALARM_3
 *  5  DROP_1        20  BOOM_1        35  SWITCH_2      50  EAT
 *  6  DROP_2        21  BOOM_2        36  POWER_OFF     51  GULP
 *  7  GRAB_1        22  BOOM_3        37  SCORE         52  ROBOBLIP
 *  8  GRAB_2        23  BOOM_4        38  CLANG         53  NOPE
 *  9  FAT_BEEP_1    24  BOOM_5        39  WATER_TAP     54  UH_HUH
 * 10  FAT_BEEP_2    25  BOOM_6        40  SELECT_1      55  OLD_COMPUTER
 * 11  FAT_BEEP_3    26  BOOM_7        41  SELECT_2      56  YEAH
 * 12  HARSH_BEEP_1  27  BOOM_8        42  SELECT_3      57  AWW
 * 13  HARSH_BEEP_2  28  ITEM_1        43  SELECT_4      58  GRR
 * 14  HARSH_BEEP_3  29  ITEM_2        44  SELECT_5
 *)
procedure Effect(Number: Integer); register; external 'playBasic';

var
  I: Integer;

begin
  ClrScr;
  GotoXY(7, 10);
  WriteLn(#18#1'*** BeepFX Demo ***'#18#0);

  for I := 0 to 58 do
  begin
    GotoXY(7, 12);
    Write('Sound effect #', I:2, '...');
    Effect(I);
  end;
end.