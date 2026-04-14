program PPTests;

var
  I: Integer;
begin
  WriteLn('--- TestDirectives ---');

  I := 0;

  // Check simple cases for undefined symbol
  {$ifdef abc}
    Assert(False);
  {$endif}

  {$ifdef abc}
    Assert(False);
  {$else}
    Inc(I);
  {$endif}
  Assert(I = 1);

  {$ifndef abc}
    Inc(I);
  {$else} 
    Assert(False);
  {$endif}
  Assert(I = 2);

  // Check simple cases for defined symbol
  {$define abc}

  {$ifdef abc}
    Inc(I);
  {$endif}
  Assert(I = 3);

  {$ifdef abc}
    Inc(I);
  {$else}
    Assert(False);
  {$endif}
  Assert(I = 4);

  {$ifndef abc}
    Assert(False);
  {$else}
    Inc(I);
  {$endif}
  Assert(I = 5);

  // Undefine symbol again
  {$undef abc}

  {$ifdef abc}
    Assert(False);
  {$endif}

  // (Un)define must have no effect in inactive branches
  {$define foo}

  {$ifdef xyz}
    Assert(False);
    {$undef foo}
    {$define bar}
  {$endif}

  {$ifdef foo}
    Inc(I);
  {$endif}
  Assert(I = 6);

  {$ifdef bar}
    Assert(False);
  {$endif}

  // Check nested cases
  {$define abc}

  {$ifdef abc}
    {$ifdef xyz}
      Assert(False);
    {$else}
      Inc(I);
    {$endif}
    Inc(I);
  {$endif}
  Assert(I = 8);

  {$ifdef abc}
    {$ifndef xyz}
      Inc(I);
    {$else}
      Inc(I);
    {$endif}
    Inc(I);
  {$endif}
  Assert(I = 10);

  // Check PASTA/80 compiler
  {$ifdef pasta}
    Assert(True);
    Write('This is PASTA/80 for ');
  {$else}
    Assert(False);
  {$endif}

  Write('This is ');

  // Check target system
  {$ifdef sys_cpm}
    Write('CP/M');
  {$else}
    {$ifdef sys_agon}
      Write('Agon Light/Console8');
    {$else}
      {$ifdef sys_specnext}
        Write('ZX Spectrum Next');
      {$else}
        Write('ZX Spectrum 128K');
      {$endif}
    {$endif}
  {$endif}

  Write(' running on ');

  // Check target CPU
  {$ifdef cpu_z80n}
    WriteLn('a Z80N.');
  {$else}
    {$ifdef cpu_ez80}
      WriteLn('an eZ80.');
    {$else}
      WriteLn('a good old Z80.');
    {$endif}
  {$endif}
end.