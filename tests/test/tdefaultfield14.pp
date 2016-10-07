program tdefaultfield14;

{$MODE DELPHI}

type
  TFoo = record
    Field1: Integer;
    DefaultValue: Integer default;
    Field2: Integer;
  end;

  TRawFoo = record
    Field1: Integer;
    DefaultValue: Integer;
    Field2: Integer;
  end;

var  
  a: TFoo;
  pa: ^TFoo;
  pa2: Pointer;
  raw_a: TRawFoo absolute a;
  raw_pa: ^TRawFoo;
begin
  pa := @a;
  pa2 := @@a;
  raw_pa := @raw_a;

  if Pointer(pa) <> Pointer(raw_pa) then
    Halt(1);    

  if pa2 <> Pointer(raw_pa) then
    Halt(2);
end.