program VRec;

type
  Rec0 = record
    B: Byte;
    C: Char;
    I, J, K: Integer;
    R: record
      X, Y, Z: Real;
    end;
  end;

  Rec1 = record
    C: Char;
    case Boolean of
      False: (I: Integer;);
      True:  (Lo, Hi: Byte;);
  end;

  Rec2 = record
    C: Char;
    case B: Boolean of
      False: (I: Integer;);
      True:  (Lo, Hi: Byte;);
  end;

  Rec3 = record
    case Code: Integer of
      0: ( B: Boolean; );
      1: ( C: Char;    );
      2: ( B: Byte;    );
      3: ( I: Integer; );
      4: ( R: Real;    );
      5: ( S: record
                case Boolean of
                  False: ( Text: String; );
                  True:  ( Len: Byte; Chrs: array[255] of Char; );
              end;
         );
  end;

  Rec4 = record  
    X : Integer;  
    case Byte of  
      2 : ( Y : Integer;
            case Byte of  
              3 : ( Z : Integer; );
              4:  ( P : Char; );
          );
      5: ( Q: Char; ); 
  end;

var
  R2: Rec2;
  
const
  R2a: Rec2 = (
    C: 'A';
    B: False;
    I: 42;
  );

  R2b: Rec2 = (
    C: 'B';
    B: True;
    Lo: 1;
    Hi: 2;
  );

  Const2c: Rec2 = (
    B: True;
    Lo: 1;
    Hi: 2;
  );

  Const2d: Rec2 = (
    C: 'D';
    Hi: 2;
  );

  Const2e: Rec2 = (
    C: 'E';
  );

  Const2f: Rec2 = (
  );

  Const2g: Rec2 = (
    ;;;;;;;;;
  );

var
  R1: Rec1;
  R4: Rec4;
  
procedure Dump(var R: Rec2; Name: TString);
begin
  WriteLn('--- ', Name, ' is ', SizeOf(R2), ' bytes at ', Addr(R), ' ---');
  WriteLn('@C=  ', Addr(R.C), ' C=', R.C);
  WriteLn('@B=  ', Addr(R.B), ' B=', R.B);
  WriteLn('@I=  ', Addr(R.I), ' I=', R.I);
  WriteLn('@Lo= ', Addr(R.Lo), ' Lo=', R.Lo);
  WriteLn('@Hi= ', Addr(R.Hi), ' Hi=', R.Hi);
  WriteLn;
end;

begin
(*
  WriteLn('@R=    ', Addr(R1));
  WriteLn('@R1.C= ', Addr(R1.C));
  WriteLn('@R1.I= ', Addr(R1.I));
  WriteLn('@R1.Lo=', Addr(R1.Lo));
  WriteLn('@R1.Hi=', Addr(R1.Hi));
  WriteLn(SizeOf(R1), ' bytes');

  WriteLn;

  WriteLn('@R2=    ', Addr(R2));
  WriteLn('@R2.C=  ', Addr(R2.C));
  WriteLn('@R2.B=  ', Addr(R2.B));
  WriteLn('@R2.I=  ', Addr(R2.I));
  WriteLn('@R2.Lo= ', Addr(R2.Lo));
  WriteLn('@R2.Hi= ', Addr(R2.Hi));
  WriteLn(SizeOf(R2), ' bytes');

  WriteLn;
*)

  R2.C := 'X';
  R2.B := True;
  R2.I := 16383;

  Dump(R2, 'R2');
(*  WriteLn(R2.C);
  WriteLn(R2.B);
  WriteLn(R2.I);
  WriteLn(R2.Lo);
  WriteLn(R2.Hi); *)

  R2.Hi := 64;
  R2.Lo := 0;

  Dump(R2, 'R2');

  Dump(R2a, 'R2a');
  Dump(R2b, 'R2b');
  (*
  WriteLn(R2.I);

  WriteLn;

  WriteLn('@R4=    ', Addr(R4));
  WriteLn('@R4.X=  ', Addr(R4.X));
  WriteLn('@R4.Y=  ', Addr(R4.Y));
  WriteLn('@R4.Z=  ', Addr(R4.Z));
  WriteLn('@R4.P=  ', Addr(R4.P));
  WriteLn('@R4.Q=  ', Addr(R4.Q));
  *)
end.