program tdefaultfield12;

{$MODE DELPHI}

type
  TFoo = record
    Field1: Integer;
    DefaultValue: TProcedure default;
    Field2: Integer;
  end;
  
var
  ok: boolean = false;  
  
procedure Foo;
begin
  ok := true;
end;

var  
  a: TFoo;
begin
  a := Foo;
  a;
  if not ok then
    Halt(1);
end.