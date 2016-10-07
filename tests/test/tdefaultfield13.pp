{ %FAIL }

program tdefaultfield13;

{$MODE DELPHI}
{$T+}

type
  TFoo = record
    Field1: Integer;
    DefaultValue: Integer default;
    Field2: Integer;
  end;

var  
  a: TFoo;
  p: PInteger;
begin
  p := @@a;
end.