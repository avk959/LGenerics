{
  Source schema: enum_variant_collisions.jtd.json

  This unit was automatically created by JtdPasCodegen, do not edit.
}
unit enum_variant_collisions;

{$MODE OBJFPC}{$H+}{$B-}

interface

uses
  SysUtils, lgJson, lgJtdTypes;

type

  TEnumVariantCollisions = class sealed(TJtdStrEnum)
  protected
  public
    class function GetJtdClass: TJtdEntityClass; override;
    class function IsEnumElement(const aValue: string): Boolean; override;
  end;

implementation

{ TEnumVariantCollisions }

class function TEnumVariantCollisions.GetJtdClass: TJtdEntityClass;
begin
  Result := TEnumVariantCollisions;
end;

class function TEnumVariantCollisions.IsEnumElement(const aValue: string): Boolean;
begin
  case aValue of
    'foo',
    'Foo',
    'FOO': Result := True;
  else
    Result := False;
  end;
end;

end.
