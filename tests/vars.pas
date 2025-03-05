program Vars;

{$A-}

type
  AnyString = string;
  String3 = string[255];

var
  FileName: string[14];
  Line: string[100];
  Ch: Char;

procedure GetFileName(var Line: AnyString; FileType:String3);
begin
  Line:='Hallo';
end;

procedure Load;
begin
  GetFileName(Filename,'MCS');
  WriteLn(Filename);
end;


begin
  Load;
  WriteLn(Filename);
end.