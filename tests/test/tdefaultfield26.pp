program tdefaultfield26;

{$MODE DELPHI}

type
  TFoo = record
    Field1: Integer;
    DefaultField: Integer default;
    Field2: Integer;
    class operator Implicit(a: TFoo): string;
  end;
  
var
  ok: boolean = false;

class operator TFoo.Implicit(a: TFoo): string;
begin
  if a <> 123 then
    Halt(1);
  Result := 'test';
end;

procedure bar(a: boolean); overload;
begin
  Halt(2);
end;

procedure bar(a: integer); overload;
begin
  Halt(3);
end;

procedure bar(a: string); overload;
begin
  if a <> 'test' then
    Halt(4);
    
  ok := true;
end;

var
  a: TFoo;
begin
  a := 123;
  bar(a); 
  if not ok then
    Halt(5);
end.