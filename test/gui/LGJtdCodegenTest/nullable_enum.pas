{
  Source schema: nullable_enum.jtd.json

  This unit was automatically created by JtdPasCodegen, do not edit.
}
unit nullable_enum;

{$MODE OBJFPC}{$H+}{$B-}

interface

uses
  SysUtils, lgJson, lgJtdTypes;

type

  TNullableEnum = (Foo, Bar, Baz);

{ Container for some TNullableEnum enumeration element; is nullable }
  TNullableEnumElem = class sealed(specialize TJtdEnum<TNullableEnum>)
    class function GetJtdClass: TJtdEntityClass; override;
  end;

implementation

{ TNullableEnumElem }

class function TNullableEnumElem.GetJtdClass: TJtdEntityClass;
begin
  Result := TNullableEnumElem;
end;

end.
