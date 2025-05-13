{
  Source schema: nullable_enum.jtd.json

  This unit was automatically created by JtdPasCodegen.
}
unit nullable_enum;

{$MODE OBJFPC}{$H+}{$B-}

interface

uses
  SysUtils, lgJson, lgJtdTypes;

type

  TEnum = (Foo, Bar, Baz);

{ Container for some TEnum enumeration element; is nullable }
  TNullableEnumElem = class sealed(specialize TJtdNullableEnum<TEnum>);

implementation

end.
