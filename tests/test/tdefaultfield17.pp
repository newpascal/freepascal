program tdefaultfield17;

{$MODE DELPHI}

type
  TFoo = record
    Field1: Integer;
    case byte of
      0: (Field2, Field3, Field4: Integer);
      1: (Field5: Integer; 
          DefaultField: Integer default;
          Field6: Integer);
      2: (Field7, Field8, Field9: Integer);
  end;

var  
  a: TFoo;
  pf3, pf8, pdf: PInteger;
begin
  a := 123;
  pdf := @a;
  pf3 := @a.Field3;
  pf8 := @a.Field8;
  
  if pdf <> pf3 then
    Halt(1);   

  if pdf <> pf8 then
    Halt(2);
    
  if pdf^ <> 123 then
    Halt(3);    
end.