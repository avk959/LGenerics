{
  Source schema: enum_variant_collisions.jtd.json

  This unit was automatically created by JtdPasCodegen.
}
unit enum_variant_collisions;

{$MODE OBJFPC}{$H+}{$B-}

interface

uses
  SysUtils, lgJson, lgJtdTypes;

type

  TEnumVariantCollisions = class sealed(TJtdStrEnum)
  public
    class function IsElement(const aValue: string): Boolean; override;
  end;

implementation

class function TEnumVariantCollisions.IsElement(const aValue: string): Boolean;
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
