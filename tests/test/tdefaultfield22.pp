program tdefaultfield22;

{$MODE DELPHI}

type
  TFoo = record
    Field1: Integer;
    DefaultField: TProcedure default;
    Field2: Integer;
  end;
  
var  
  a: TFoo = (DefaultField: nil);
  p1, p2, p3: Pointer;
begin
  p1 := @a; // <=> p1 := @a.DefaultField
  p2 := @@a; // <=> p2 := @raw_a
  p3 := @@@a; // <=> p3 := @@a.DefaultField
  
  if p1 = p2 then 
    Halt(1);
    
  if p1 = p3 then
    Halt(2);
    
  if p2 = p3 then
    Halt(3);
    
  if p1 <> nil then
    Halt(4);
end.