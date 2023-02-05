{
  Source schema: nullable_elements.jtd.json

  This unit was automatically created by JtdPasCodegen, do not edit.
}
unit nullable_elements;

{$MODE OBJFPC}{$H+}{$B-}

interface

uses
  SysUtils, lgJson, lgJtdTypes;

type

{ TNullableElements is nullable }
  TNullableElements = class sealed(specialize TJtdList<TJtdString>)
    class function GetJtdClass: TJtdEntityClass; override;
  end;

implementation

{ TNullableElements }

class function TNullableElements.GetJtdClass: TJtdEntityClass;
begin
  Result := TNullableElements;
end;

end.
