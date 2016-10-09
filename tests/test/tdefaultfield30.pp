{ %NORUN }

program tdefaultfield30;

{$MODE DELPHI}

type
  TFoo = record
    DefaultValue: Integer strict default;
  end;

begin
end.