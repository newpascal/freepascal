{ %FAIL }

program tdefaultfield20;

{$MODE DELPHI}

type
  TFoo = record
    DefaultValue: Integer default;
  end;
  
var  
  a: TFoo;
  p: Pointer;
begin
  p := @@@a;
end.