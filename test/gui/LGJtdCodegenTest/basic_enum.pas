{
  Source schema: basic_enum.jtd.json

  This unit was automatically created by JtdPasCodegen, do not edit.
}
unit basic_enum;

{$MODE OBJFPC}{$H+}{$B-}

interface

uses
  SysUtils, lgJson, lgJtdTypes;

type

  TBasicEnum = (Foo, Bar, Baz);

{ Container for some TBasicEnum enumeration element }
  TBasicEnumElem = class sealed(specialize TJtdEnum<TBasicEnum>)
    class function GetJtdClass: TJtdEntityClass; override;
  end;

implementation

{ TBasicEnumElem }

class function TBasicEnumElem.GetJtdClass: TJtdEntityClass;
begin
  Result := TBasicEnumElem;
end;

end.
