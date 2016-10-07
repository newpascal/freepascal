program tdefaultfield1;

{$MODE DELPHI}

type
  TFoo = record
    DefaultValue: Integer default;
    Value: Integer; 
  end;

var
  a, b: TFoo;
begin
  a := 321;
  a.Value := 123;
  b := a; 
  if b.DefaultValue <> 321 then
    Halt(1);
  if b.Value <> 123 then
    Halt(2);
end.
