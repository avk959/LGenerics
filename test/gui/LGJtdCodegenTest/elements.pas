{
  Source schema: elements.jtd.json

  This unit was automatically created by JtdPasCodegen, do not edit.
}
unit elements;

{$MODE OBJFPC}{$H+}{$B-}

interface

uses
  SysUtils, lgJson, lgJtdTypes;

type

  TElements = class sealed(specialize TJtdList<TJtdString>)
    class function GetJtdClass: TJtdEntityClass; override;
  end;

implementation

{ TElements }

class function TElements.GetJtdClass: TJtdEntityClass;
begin
  Result := TElements;
end;

end.
