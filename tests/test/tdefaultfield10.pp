program tdefaultfield10;

{$MODE DELPHI}

type
  TFoo<T> = record
    Field1: Integer;
    DefaultValue: T default;
    Field2: Integer;
  end;

  TFooInt = TFoo<Integer>;
  TFooFooInt = TFoo<TFooInt>;

var  
  a: TFooFooInt;
begin
  a := 123;
  if a <> 123 then
    Halt(1);
  if a.DefaultValue <> 123 then
    Halt(2);
  if a.DefaultValue.DefaultValue <> 123 then
    Halt(3);
end.