program RdInt;
(* Reading a value that is not a valid number is an I/O error, reported the
   same way a disk error is: LastError is set, the i-minus directive can catch
   it through IOResult, and under the default i-plus the compiler-generated
   check stops the program (OPEN-ITEMS-EN.md B9). Verified against TP 3.0,
   where i-minus covers this case too.

   255 is used as the code because it does not collide with anything BDOS,
   esxDOS or MOS report themselves.

   Note that Val is deliberately *not* part of this: it is not an I/O
   operation and has its own error parameter. Val(S, V) without one still
   aborts outright with "Format error".

   Expected output:
     before
     I/O error 255
   ("after" must not print.) *)
var
  F: Text;
  I: Integer;
begin
  Assign(F, 'RDINT.TMP');
  Rewrite(F);
  WriteLn(F, 'abc');
  Close(F);

  Reset(F);
  WriteLn('before');
  Read(F, I);
  WriteLn('after: ', I);
end.
