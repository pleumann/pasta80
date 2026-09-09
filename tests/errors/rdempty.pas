program RdEmpty;
(* Nothing at all -- an empty line, read from a text file under the default i-plus, so the
   compiler-generated check stops the program. The i-minus counterpart --
   the same failure surfacing through IOResult, with the program carrying
   on -- is TestTextReadIOResult in tests/files.pas. See OPEN-ITEMS-EN.md B9.

   Deliberately different from TP 3.0, which ignores empty input and leaves
   the variable as it was. Treating it as invalid is why a data file with a
   trailing blank line now stops a read loop.

   Expected output:
     before
     I/O error 255
   ("after" must not print.) *)
var
  F: Text;
  V: Integer;
begin
  Assign(F, 'RDEMPTY.TMP');
  Rewrite(F);
  WriteLn(F, '');
  Close(F);

  Reset(F);
  WriteLn('before');
  Read(F, V);
  WriteLn('after');
end.
