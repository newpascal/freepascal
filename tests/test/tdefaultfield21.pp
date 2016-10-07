program tdefaultfield21;

{$MODE DELPHI}

type
  TFoo = record
    Field1: Integer;
    DefaultField: TProcedure default;
    Field2: Integer;
  end;

  TRawFoo = record
    Field1: Integer;
    DefaultField: CodePointer;
    Field2: Integer;
  end;
  
var  
  a: TFoo;
  ra: TRawFoo absolute a;
  p1: Pointer;
  p2: Pointer;
begin
  p1 := @@@a; // <=> raw_pp_from_rd1 := @@a.DefaultField
  p2 := @@a.DefaultField;

  if p1 <> p2 then
    Halt(1) ;  

  if p1 <> @ra.DefaultField then
    Halt(2);   
end.