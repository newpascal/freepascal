program tdefaultfield2;

{$MODE DELPHI}

type
  TFoo = record
    DefaultValue: Integer default;

    class operator Implicit(A: Integer): TFoo;
  end;

class operator TFoo.Implicit(A: Integer): TFoo;
begin
  Result.DefaultValue := A * 2;
end;

var
  a: TFoo;
begin
  a := 1;
  if a.DefaultValue <> 2 then
    Halt(1);
end.