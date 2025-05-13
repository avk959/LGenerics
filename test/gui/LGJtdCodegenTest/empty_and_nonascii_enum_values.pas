{
  Source schema: empty_and_nonascii_enum_values.jtd.json

  This unit was automatically created by JtdPasCodegen.
}
unit empty_and_nonascii_enum_values;

{$MODE OBJFPC}{$H+}{$B-}

interface

uses
  SysUtils, lgJson, lgJtdTypes;

type

  TEmptyAndNonasciiEnumValues = class sealed(TJtdStrEnum)
  public
    class function IsElement(const aValue: string): Boolean; override;
  end;

implementation

class function TEmptyAndNonasciiEnumValues.IsElement(const aValue: string): Boolean;
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
