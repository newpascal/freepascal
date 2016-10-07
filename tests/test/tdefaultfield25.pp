program tdefaultfield25;

{$MODE DELPHI}

type
  TFoo = record
    Field1: Integer;
    DefaultField: Integer default;
    Field2: Integer;
  end;
  
var
  ok: boolean = false;

procedure bar(a: Integer); overload;
begin
  if a <> 123 then
    Halt(1);
  ok := true;
end;

procedure bar(a: string); overload;
begin
  Halt(2);
end;

var
  a: TFoo;
begin
  a := 123;
  bar(a); 
  if not ok then
    Halt(3);
end.