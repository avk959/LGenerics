{
  Source schema: nullable_elements.jtd.json

  This unit was automatically created by JtdPasCodegen.
}
unit nullable_elements;

{$MODE OBJFPC}{$H+}{$B-}

interface

uses
  SysUtils, lgJson, lgJtdTypes;

type

{ TNullableElements is nullable }
  TNullableElements = class sealed(specialize TJtdNullableList<TJtdString>);

implementation

end.
