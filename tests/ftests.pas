program FTests;

type
  RealFile = file of Real;

  UnrealFile = RealFile;

procedure A(var F: File);
begin
end;

procedure B(var F: RealFile);
begin
end;

procedure C(var F: UnrealFile);
begin
end;

procedure D(var F: Text);
begin
end;

begin
end.