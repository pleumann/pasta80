program ValReal;
(* Val without an error variable has nowhere to report to, so it stops the
   program instead. Unlike Read it is not an I/O operation -- in Turbo Pascal
   Val never touches IOResult, it carries its own Code parameter -- so this
   abort is deliberately outside the i-minus/i-plus machinery and cannot be
   suppressed (OPEN-ITEMS-EN.md B9).

   The target is a Real, so this goes through __conv_real. The three-argument form is
   exercised in tests/all.pas instead, where it reports through E and the
   program carries on.

   Expected output:
     before
     Format error
   ("after" must not print.) *)
var
  V: Real;
begin
  WriteLn('before');
  Val('xyz', V);
  WriteLn('after');
end.
