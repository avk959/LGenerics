{
  Source schema: basic_enum.jtd.json

  This unit was automatically created by JtdPasCodegen.
}
unit basic_enum;

{$MODE OBJFPC}{$H+}{$B-}

interface

uses
  SysUtils, lgJson, lgJtdTypes;

type

  TEnum = (Foo, Bar, Baz);

{ Container for some TEnum enumeration element }
  TEnumElem = class sealed(specialize TJtdEnum<TEnum>);

implementation

end.
