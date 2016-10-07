program tdefaultfield7;

{$MODE DELPHI}

type
  PFoo = ^TFoo;
  TFoo = record
    Field1: Integer;
    DefaultValue: Integer default;
    Field2: Integer;
  end;

var  
  a: TFoo;
  pa: PFoo;
  ptrToFoo: Pointer;
begin
  ptrToFoo := @@a;
  pa := @a;
  
  if ptrToFoo <> pa then
    Halt(1); 
end.