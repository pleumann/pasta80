program IoPend;
(* An I/O error raised while checks are off does not simply evaporate when
   they are turned back on: if nobody collected it with IOResult, switching
   back to i-plus stops the program. Verified against TP 3.0, which behaves
   the same way (OPEN-ITEMS-EN.md B9).

   The compiler emits the check at the directive itself, and only on a real
   transition, so it costs three bytes per directive rather than three per
   call site.

   The well-behaved counterpart -- reading IOResult and carrying on -- is
   TestTextReadIOResult in tests/files.pas.

   Expected output:
     before
     I/O error 255
   ("after" must not print.) *)
var
  F: Text;
  I: Integer;
begin
  Assign(F, 'IOPEND.TMP');
  Rewrite(F);
  WriteLn(F, 'abc');
  Close(F);

  Reset(F);
  WriteLn('before');

  {$i-}
  Read(F, I);
  {$i+}

  WriteLn('after: ', I);
end.
