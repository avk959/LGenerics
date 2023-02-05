{
  Source schema: empty_and_nonascii_enum_values.jtd.json

  This unit was automatically created by JtdPasCodegen, do not edit.
}
unit empty_and_nonascii_enum_values;

{$MODE OBJFPC}{$H+}{$B-}

interface

uses
  SysUtils, lgJson, lgJtdTypes;

type

  TEmptyAndNonasciiEnumValues = class sealed(TJtdStrEnum)
  protected
  public
    class function GetJtdClass: TJtdEntityClass; override;
    class function IsEnumElement(const aValue: string): Boolean; override;
  end;

implementation

{ TEmptyAndNonasciiEnumValues }

class function TEmptyAndNonasciiEnumValues.GetJtdClass: TJtdEntityClass;
begin
  Result := TEmptyAndNonasciiEnumValues;
end;

class function TEmptyAndNonasciiEnumValues.IsEnumElement(const aValue: string): Boolean;
begin
  case TJsonNode.PasStrToAsciiJson(aValue) of
    '""',
    '"$foo"',
    '"_foo"',
    '"0foo"',
    '"foo0bar"',
    '"foo bar"',
    '"foo\nbar"',
    '"foo\uFDFDbar"': Result := True;
  else
    Result := False;
  end;
end;

end.
