(**
 * Shows all combinations of standard colors for text and background.
 *)
program Colors;

var
  B, C: Integer;
begin
  ClrScr;
  for B := 0 to 7 do
  begin
    TextBackground(B);
    for C := 0 to 7 do
    begin
      TextColor(C);
      {$ifdef SYS_ZX}
      GotoXY(1 + (C mod 4) * 8, 1 + B * 2 + C div 4);
      {$else}
      GotoXY(1 + C * 9, 1 + B * 2);
      {$endif}
      Write(' Pascal ');
    end;
  end;
end.
