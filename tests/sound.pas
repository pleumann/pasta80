program Sounds;

{$ifdef SYS_CPM}
  {$error Agon or ZX Spectrum 48K/128K/Next required.}
{$endif}

procedure PlayAndWait(Freq, Ms: Integer);
begin
  WriteLn('Playing ', Freq, ' Hz for ', Ms, ' ms...');

  {$ifdef SYS_AGON}
    Sound(Freq);
    Delay(Ms);
    NoSound;
  {$else}
    SoundMs(Freq, Ms);
  {$endif}

  WriteLn('Sound off');
end;

begin
  PlayAndWait(262, 500);
  PlayAndWait(330, 500);
  PlayAndWait(392, 500);
  PlayAndWait(523, 1000);
end.
