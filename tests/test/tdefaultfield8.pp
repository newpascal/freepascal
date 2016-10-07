program tdefaultfield8;

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
begin
  if @@a = @a then 
    Halt(1);
end.