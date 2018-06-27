program tdefaultfield5;

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

procedure Bar(A: Integer);
begin
  if A <> 2 then
    Halt(1);
end;

var
  a: TFoo;
begin
  a := 1;
  Bar(a);
end.