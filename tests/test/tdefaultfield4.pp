program tdefaultfield4;

{$MODE DELPHI}

type
  TFoo = record
    DefaultValue: Integer default;
  end;

var
  a: TFoo;
  i: Integer;
begin
  a := 123;
  i := a;
  if i <> 123 then
    Halt(1);
  i := 321;
  a := i;
  if a <> 321 then
    Halt(2);
end.