(* Test for "too many nested overlays" *)
program OverNest;

var
  Depth: Integer;

procedure Helper; forward;

overlay procedure OverlayA;
begin
  Inc(Depth);
  WriteLn('A depth=', Depth);
  if Depth < 20 then Helper;
end;

const Dummy1 = 0;

overlay procedure OverlayB;
begin
  Inc(Depth);
  WriteLn('B depth=', Depth);
  if Depth < 20 then OverlayA;
end;

const Dummy2 = 0;

procedure Helper;
begin
  OverlayB;
end;

begin
  WriteLn('before');
  Depth := 0;
  OverlayA;
  WriteLn('after, max depth reached=', Depth);
end.
