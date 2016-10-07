{ %FAIL }

program tdefaultfield29;

{$MODE DELPHI}

type
  TSmartObj<T: TObject> = record
    Instance: T default;
  end;

  TA = class(TObject);

var
  a: TSmartObj<TObject>;
  d: array of TSmartObj<TA>;
begin
  SetLength(d, 1);
  d[0] := a;
end.