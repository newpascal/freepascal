{ %FAIL }

program tdefaultfield18;

{$MODE DELPHI}

type
  TFoo = record
    DefaultValue1: Integer default;
    case byte of
      0: (Field1: Integer);
      1: (Field2: Integer; 
          DefaultValue2: Integer default);
  end;
  
begin
end.