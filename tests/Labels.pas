program Labels;

label
  Foo, Bar, 123;

begin
  WriteLn('Jumping Jack');

  goto Foo;

  WriteLn('You should never see this');

  Bar:
    WriteLn('With skill his story');
    goto 123;

  WriteLn('You should never see this');

  Foo:
    WriteLn('Is quick and bold');
    goto Bar;

  WriteLn('You should never see this');

  123:
    WriteLn('Will unfold');
end.