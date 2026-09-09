program RdReal;
(* A word that is not a Real, read from a text file under the default i-plus, so the
   compiler-generated check stops the program. The i-minus counterpart --
   the same failure surfacing through IOResult, with the program carrying
   on -- is TestTextReadIOResult in tests/files.pas. See OPEN-ITEMS-EN.md B9.

   Expected output:
     before
     I/O error 255
   ("after" must not print.) *)
var
  F: Text;
  V: Real;
begin
  Assign(F, 'RDREAL.TMP');
  Rewrite(F);
  WriteLn(F, 'xyz');
  Close(F);

  Reset(F);
  WriteLn('before');
  Read(F, V);
  WriteLn('after');
end.
