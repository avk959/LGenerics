{
  Source schema: values.jtd.json

  This unit was automatically created by JtdPasCodegen, do not edit.
}
unit values;

{$MODE OBJFPC}{$H+}{$B-}

interface

uses
  SysUtils, lgJson, lgJtdTypes;

type

  TValues = class sealed(specialize TJtdMap<TJtdString>)
    class function GetJtdClass: TJtdEntityClass; override;
  end;

implementation

{ TValues }

class function TValues.GetJtdClass: TJtdEntityClass;
begin
  Result := TValues;
end;

end.
