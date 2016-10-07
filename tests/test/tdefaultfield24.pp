program tdefaultfield24;

{$MODE DELPHI}

type
  TFoo = record
    Field1: Integer;
    DefaultField: Integer default;
    Field2: Integer;
  end;

var
  a: TFoo;
begin
  a := 123;
  if a <> 123 then
    Halt(1);
  if 123 <> a then
    Halt(2);  
end.