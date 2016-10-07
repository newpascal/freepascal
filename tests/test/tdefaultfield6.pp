program tdefaultfield6;

{$MODE DELPHI}

type
  TFoo = record
    Field: Integer;
    DefaultValue: Integer default;
  end;

var  
  a: TFoo;
  ptrToFoo: pointer;
  ptrToDefault: pointer;
begin
  ptrToFoo := @@a;
  ptrToDefault := @a;
  
  if ptrToFoo = ptrToDefault then
    Halt(1); 
end.