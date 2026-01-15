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

  { TRootObject is nullable }
  TRootObject = class sealed(specialize TJtdList<TJtdString>);

implementation

end.
