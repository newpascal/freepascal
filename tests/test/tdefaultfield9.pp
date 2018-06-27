program tdefaultfield9;

{$MODE DELPHI}

type
  PFoo = ^TFoo;
  TFoo = record
    Field1: Integer;
    DefaultValue: PFoo default;
    Field2: Integer;
  end;

var  
  a: TFoo;
  pa: PFoo;
  ptrToDefault: ^PFoo;
begin
  pa := @a; // <=> pa := @a;
  a := pa;  // <=> a.DefaultValue := pa;
  
  ptrToDefault := @a; // <=> ptrToDefault := @a.DefaultValue;
  if ptrToDefault^ <> pa then
    Halt(1);
    
  if ptrToDefault <> @a.DefaultValue then
    Halt(2);
end.