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

  { TRootObjectEnum }
  TRootObjectEnum = (Foo, Bar, Baz);

  { TRootObject

    TRootObject: container for some TRootObjectEnum enumeration element }
  TRootObject = class sealed(specialize TJtdEnum<TRootObjectEnum>);

implementation

end.
