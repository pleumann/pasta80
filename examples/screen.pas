program Screen;

{$ifndef SYS_AGON}
  {$error This program is for Agon only.}
{$endif}

const
  LastMode = 53;

  { Standard MOS/FatFS status code for "Invalid Parameter", used as Halt's exit code. }
  ErrInvalidParameter = 19;

type
  TScreenMode = record
    Mode: Integer;
    Width: Integer;
    Height: Integer;
    Colors: Integer;
    Refresh: Integer;
  end;

const
  (**
   * All Agon screen modes, as documented at
   * https://agonplatform.github.io/agon-docs/vdp/Screen-Modes/
   * Mode 7 is the teletext mode, which has no pixel resolution
   * (Width/Height are 0).
   * Modes 128 and up are double-buffered variants of some of the modes
   * above (mode number + 128). Owing to memory limitations, not all modes
   * can be double-buffered, so those mode numbers are not contiguous.
   *)
  Modes: array[0..LastMode] of TScreenMode = (
    (Mode:   0; Width:  640; Height: 480; Colors: 16; Refresh: 60),
    (Mode:   1; Width:  640; Height: 480; Colors:  4; Refresh: 60),
    (Mode:   2; Width:  640; Height: 480; Colors:  2; Refresh: 60),
    (Mode:   3; Width:  640; Height: 240; Colors: 64; Refresh: 60),
    (Mode:   4; Width:  640; Height: 240; Colors: 16; Refresh: 60),
    (Mode:   5; Width:  640; Height: 240; Colors:  4; Refresh: 60),
    (Mode:   6; Width:  640; Height: 240; Colors:  2; Refresh: 60),
    (Mode:   7; Width:    0; Height:   0; Colors: 16; Refresh: 60),
    (Mode:   8; Width:  320; Height: 240; Colors: 64; Refresh: 60),
    (Mode:   9; Width:  320; Height: 240; Colors: 16; Refresh: 60),
    (Mode:  10; Width:  320; Height: 240; Colors:  4; Refresh: 60),
    (Mode:  11; Width:  320; Height: 240; Colors:  2; Refresh: 60),
    (Mode:  12; Width:  320; Height: 200; Colors: 64; Refresh: 70),
    (Mode:  13; Width:  320; Height: 200; Colors: 16; Refresh: 70),
    (Mode:  14; Width:  320; Height: 200; Colors:  4; Refresh: 70),
    (Mode:  15; Width:  320; Height: 200; Colors:  2; Refresh: 70),
    (Mode:  16; Width:  800; Height: 600; Colors:  4; Refresh: 60),
    (Mode:  17; Width:  800; Height: 600; Colors:  2; Refresh: 60),
    (Mode:  18; Width: 1024; Height: 768; Colors:  2; Refresh: 60),
    (Mode:  19; Width: 1024; Height: 768; Colors:  4; Refresh: 60),
    (Mode:  20; Width:  512; Height: 384; Colors: 64; Refresh: 60),
    (Mode:  21; Width:  512; Height: 384; Colors: 16; Refresh: 60),
    (Mode:  22; Width:  512; Height: 384; Colors:  4; Refresh: 60),
    (Mode:  23; Width:  512; Height: 384; Colors:  2; Refresh: 60),
    (Mode:  24; Width:  640; Height: 512; Colors: 16; Refresh: 60),
    (Mode:  25; Width:  640; Height: 512; Colors:  4; Refresh: 60),
    (Mode:  26; Width:  640; Height: 512; Colors:  2; Refresh: 60),
    (Mode:  27; Width:  640; Height: 256; Colors: 64; Refresh: 60),
    (Mode:  28; Width:  640; Height: 256; Colors: 16; Refresh: 60),
    (Mode:  29; Width:  640; Height: 256; Colors:  4; Refresh: 60),
    (Mode:  30; Width:  640; Height: 256; Colors:  2; Refresh: 60),
    (Mode: 129; Width:  640; Height: 480; Colors:  4; Refresh: 60),
    (Mode: 130; Width:  640; Height: 480; Colors:  2; Refresh: 60),
    (Mode: 132; Width:  640; Height: 240; Colors: 16; Refresh: 60),
    (Mode: 133; Width:  640; Height: 240; Colors:  4; Refresh: 60),
    (Mode: 134; Width:  640; Height: 240; Colors:  2; Refresh: 60),
    (Mode: 136; Width:  320; Height: 240; Colors: 64; Refresh: 60),
    (Mode: 137; Width:  320; Height: 240; Colors: 16; Refresh: 60),
    (Mode: 138; Width:  320; Height: 240; Colors:  4; Refresh: 60),
    (Mode: 139; Width:  320; Height: 240; Colors:  2; Refresh: 60),
    (Mode: 140; Width:  320; Height: 200; Colors: 64; Refresh: 70),
    (Mode: 141; Width:  320; Height: 200; Colors: 16; Refresh: 70),
    (Mode: 142; Width:  320; Height: 200; Colors:  4; Refresh: 70),
    (Mode: 143; Width:  320; Height: 200; Colors:  2; Refresh: 70),
    (Mode: 145; Width:  800; Height: 600; Colors:  2; Refresh: 60),
    (Mode: 146; Width: 1024; Height: 768; Colors:  2; Refresh: 60),
    (Mode: 149; Width:  512; Height: 384; Colors: 16; Refresh: 60),
    (Mode: 150; Width:  512; Height: 384; Colors:  4; Refresh: 60),
    (Mode: 151; Width:  512; Height: 384; Colors:  2; Refresh: 60),
    (Mode: 153; Width:  640; Height: 512; Colors:  4; Refresh: 60),
    (Mode: 154; Width:  640; Height: 512; Colors:  2; Refresh: 60),
    (Mode: 156; Width:  640; Height: 256; Colors: 16; Refresh: 60),
    (Mode: 157; Width:  640; Height: 256; Colors:  4; Refresh: 60),
    (Mode: 158; Width:  640; Height: 256; Colors:  2; Refresh: 60)
  );

(**
 * Formats the mode at the given index of the Modes table into a readable
 * string, e.g. "Mode  21:  512x384,  16 colors, 60 Hz".
 *)
function ModeStr(Index: Integer): String;
var
  S, T: String;
begin
  with Modes[Index] do
  begin
    Str(Mode:3, S);
    S := 'Mode ' + S + ': ';

    if Width = 0 then
      S := S + ' --- x ---'
    else
    begin
      Str(Width:4, T);
      S := S + T;
      Str(Height:3, T);
      S := S + ' x ' + T;
    end;

    Str(Colors:2, T);
    S := S + ' x ' + T;

    Str(Refresh:2, T);
    S := S + ' @ ' + T;
  end;

  ModeStr := S;
end;

(**
 * Finds the index of the given mode number in the Modes table, or -1 if
 * the mode number is not one of the known ones.
 *)
function FindMode(Mode: Integer): Integer;
var
  I: Integer;
begin
  FindMode := -1;

  for I := 0 to LastMode do
    if Modes[I].Mode = Mode then
    begin
      FindMode := I;
      Exit;
    end;
end;

(**
 * Lists all screen modes matching the given filters, one per line. A
 * filter value of -1 means "any", i.e. that field is not filtered on.
 *)
procedure ListModes(FilterWidth, FilterHeight, FilterColors, FilterRefresh: Integer);
var
  I: Integer;
begin
  for I := 0 to LastMode do
    if ((FilterWidth = -1) or (Modes[I].Width = FilterWidth)) and
       ((FilterHeight = -1) or (Modes[I].Height = FilterHeight)) and
       ((FilterColors = -1) or (Modes[I].Colors = FilterColors)) and
       ((FilterRefresh = -1) or (Modes[I].Refresh = FilterRefresh)) then
      WriteLn(ModeStr(I));
end;

(**
 * Parses a "list" filter argument into Value, leaving Value untouched if
 * S is '*'. Halts with an error message if S is neither '*' nor a number.
 *)
procedure ParseFilter(S: String; var Value: Integer);
var
  ErrPos: Integer;
begin
  if S = '*' then Exit;

  Val(S, Value, ErrPos);
  if ErrPos <> 0 then
    Halt(ErrInvalidParameter);
end;

procedure PrintUsage;
begin
  WriteLn('Purpose:');
  WriteLn;
  WriteLn(' Get, set, or list Agon screen modes.');
  WriteLn;
  WriteLn('Usage:');
  WriteLn;
  WriteLn(' screen get');
  WriteLn(' screen set  <mode>');
  WriteLn(' screen info <mode>');
  WriteLn(' screen list <horz> <vert> <cols> <rate>');
  WriteLn;
  WriteLn('When listing modes, use * if you don''t');
  WriteLn('care about a value. All trailing * are');
  WriteLn('optional.');
end;

var
  Command: String;
  Mode, Index, ErrPos: Integer;
  FilterWidth, FilterHeight, FilterColors, FilterRefresh: Integer;

begin
  if ParamCount = 0 then
  begin
    PrintUsage;
    Halt(ErrInvalidParameter);
  end;

  Command := ParamStr(1);

  if Command = 'get' then
  begin
    Mode := GetGraphMode;
    Index := FindMode(Mode);

    if Index = -1 then
      WriteLn('Mode ', Mode, ' (unknown).')
    else
      WriteLn(ModeStr(Index));
  end
  else if Command = 'set' then
  begin
    if ParamCount < 2 then
    begin
      PrintUsage;
      Halt(ErrInvalidParameter);
    end;

    Val(ParamStr(2), Mode, ErrPos);
    if ErrPos <> 0 then
    begin
      WriteLn('Invalid mode: ', ParamStr(2));
      Halt(ErrInvalidParameter);
    end;

    SetGraphMode(Mode);
  end
  else if Command = 'info' then
  begin
    if ParamCount < 2 then
    begin
      PrintUsage;
      Halt(ErrInvalidParameter);
    end;

    Val(ParamStr(2), Mode, ErrPos);
    if ErrPos <> 0 then
      Halt(ErrInvalidParameter);

    Index := FindMode(Mode);
    if Index = -1 then
      Halt(ErrInvalidParameter);

    WriteLn(ModeStr(Index));
  end
  else if Command = 'list' then
  begin
    FilterWidth := -1;
    FilterHeight := -1;
    FilterColors := -1;
    FilterRefresh := -1;

    if ParamCount >= 2 then ParseFilter(ParamStr(2), FilterWidth);
    if ParamCount >= 3 then ParseFilter(ParamStr(3), FilterHeight);
    if ParamCount >= 4 then ParseFilter(ParamStr(4), FilterColors);
    if ParamCount >= 5 then ParseFilter(ParamStr(5), FilterRefresh);

    ListModes(FilterWidth, FilterHeight, FilterColors, FilterRefresh);
  end
  else
  begin
    PrintUsage;
    Halt(ErrInvalidParameter);
  end;
end.
