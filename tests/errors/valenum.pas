program ValEnum;
(* Val without an error variable has nowhere to report to, so it stops the
   program instead. Unlike Read it is not an I/O operation -- in Turbo Pascal
   Val never touches IOResult, it carries its own Code parameter -- so this
   abort is deliberately outside the i-minus/i-plus machinery and cannot be
   suppressed (OPEN-ITEMS-EN.md B9).

   The target is an enumeration type, so this goes through __conv_enum. The three-argument form is
   exercised in tests/all.pas instead, where it reports through E and the
   program carries on.

   Expected output:
     before
     Format error
   ("after" must not print.) *)
type
  Color = (Red, Green, Blue);

var
  V: Color;
begin
  WriteLn('before');
  Val('nope', V);
  WriteLn('after');
end.
