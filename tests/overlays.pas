program Overlays;

procedure A;
begin
  WriteLn('A');
  Delay(500);
end;

overlay Procedure B1;
begin
  WriteLn('B1');
  Delay(500);
end;

overlay Procedure B2;
begin
  WriteLn('B2');
  Delay(500);
end;

procedure C;
begin
  WriteLn('C');
  Delay(500);
end;

overlay Procedure D;
begin
  WriteLn('D');
  Delay(500);
  B1;
end;

procedure E;
begin
  WriteLn('E');
  Delay(500);
  A;
  B1;
  B2;
  C;
  D;
end;

overlay Procedure F;
begin
  WriteLn('F');
  Delay(500);
  A;
  B1;
  B2;
  C;
  D;
end;

overlay Procedure G;
begin
  F;
  WriteLn('G');
  Delay(500);
end;

function Overload: Integer; register; external 'overload';

var
  I: Integer;

begin
  // WriteLn('Loading overlay...');

  //I := Overload;
  //WriteLn('rc=', I);
  //if I = 0 then WriteLn('OK') else
  //begin
  //  WriteLn('Fail');
  //  Halt;
  //send;

  //for I := 0 to 23 do
  //  WriteLn(I:5, ' ', Mem[32768 + I]:3);

  //Halt;

  WriteLn('Here we go...');

  A;
  B1;
  B2;
  C;
  D;
  E;
  F;

  WriteLn('And we''re done.');
end.
