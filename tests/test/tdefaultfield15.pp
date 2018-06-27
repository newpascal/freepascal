{ %FAIL }

program tdefaultfield15;

{$MODE DELPHI}

type
  TFoo = record
    DefaultValue1: Integer default;
    DefaultValue2: Integer default;
  end;
  
begin
end.