program tdefaultfield27;

{$MODE DELPHI}

type
  TFoo = record
    DefaultField: string default;
  end;

var
  a: TFoo;
begin
  a := 'Hello from default field!';
  WriteLn(a);
end.