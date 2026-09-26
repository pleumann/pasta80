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
  Delay(1000);

  PlayAndWait(262, 500);
  Assert(True);
  PlayAndWait(330, 500);
  Assert(True);
  PlayAndWait(392, 500);
  Assert(True);
  PlayAndWait(523, 1000);
  Assert(True);

  WriteLn;
  WriteLn('************************');
  WriteLn('Passed assertions: ', AssertPassed);
  WriteLn('Failed assertions: ', AssertFailed);
  WriteLn('************************');
  WriteLn;

  Delay(1000);

  {$ifdef SYS_AGON}
    inline($3e / $00 / $d3 / $00);
  {$endif}
  {$ifdef SYS_ZXNEXT}
    QuitEmulator;
  {$endif}

end.
