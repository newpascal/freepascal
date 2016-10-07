program tdefaultfield11;

{$MODE DELPHI}

type
  TFoo<T> = record
    Field1: Integer;
    DefaultValue: T default;
    Field2: Integer;
  end;

  TFooInt = TFoo<Integer>;
  TFooFooInt = TFoo<TFooInt>;

var  
  a: TFooFooInt;
  pi: PInteger;
  pfi: ^TFooInt;
  pffi: ^TFooFooInt;
begin
  a := 123;
  
  pi := @a;
  pfi := @a;
  pffi := @a;
  
  if Pointer(pi) = Pointer(pfi) then
    Halt(1);

  if Pointer(pi) = Pointer(pffi) then
    Halt(2);

  if Pointer(pfi) = Pointer(pffi) then
    Halt(3);
    
  if pi^ <> 123 then
    Halt(4);
    
  if pfi^ <> 123 then
    Halt(5);

  if pffi^ <> 123 then
    Halt(6);
end.