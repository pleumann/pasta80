program SoundTest;

begin
  WriteLn('Simple sound test. 0.5 seconds at 440Hz');
  Sound(440);
  Delay(500);
  WriteLn('Part 2: 0.5 seconds at 880Hz');
  Sound(880);
  Delay(500);
  WriteLn('Part 3: 0.5 seconds at 440Hz');
  Sound(440);
  Delay(500);
  WriteLn('Part 4: sound off');
  NoSound;
end.
