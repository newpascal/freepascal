program tdefaultfield23;

{$MODE DELPHI}

type
  TFoo = record
    DefaultValue: TProcedure default;
  end;
  
var
  ok: boolean = false;  
  
procedure Foo;
begin
  ok := true;
end;

var  
  a: TFoo;
  pa: ^TFoo;
begin
  a := Foo;
  pa := @a;
  pa^;
  if not ok then
    Halt(1);
end.