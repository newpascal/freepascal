{ %FAIL }

program tdefaultfield16;

{$MODE DELPHI}

type
  TFoo = record
    DefaultValue1: Integer;
    case byte of
      0: (Field1: Integer default);
      1: (Field2: Integer; 
          DefaultValue2: Integer default);
  end;
  
begin
end.