program tdefaultfield19;

{$MODE DELPHI}

type
  TFoo = record
    DefaultField: TProcedure default;
  end;
  
procedure Foo;
begin
end;

var  
  a: TFoo;
  p: TProcedure;
begin
  a := Foo;  
  p := Foo;
  
  if @a <> @p then
    Halt(1);
end.