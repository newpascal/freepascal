program tdefaultfield3;

{$MODE DELPHI}

type
  TFoo = record
    DefaultValue: Integer default;

    class operator Implicit(A: TFoo): Integer;
  end;

class operator TFoo.Implicit(A: TFoo): Integer;
begin
  Result := A.DefaultValue * 2;
end;

var
  a: TFoo;
  i: Integer;
begin
  a := 1;
  i := a;
  if i <> 2 then
    Halt(1);
end.