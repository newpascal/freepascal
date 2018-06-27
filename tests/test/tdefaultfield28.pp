{ %FAIL }

program tdefaultfield28;

{$MODE DELPHI}

type
  TFoo = record
    DefaultField: Integer default;
    class operator Implicit(a: TFoo): Integer;
    class operator Implicit(a: TFoo): string;
  end;

class operator TFoo.Implicit(a: TFoo): Integer;
begin
end;

class operator TFoo.Implicit(a: TFoo): string;
begin
end;

procedure bar(a: integer); overload;
begin
end;

procedure bar(a: string); overload;
begin
end;

var
  a: TFoo;
begin
  bar(a);
end.