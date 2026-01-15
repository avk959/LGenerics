{
  Source schema: values.jtd.json

  This unit was automatically created by JtdPasCodegen.
}
unit values;

{$MODE OBJFPC}{$H+}{$B-}

interface

uses
  SysUtils, lgJson, lgJtdTypes;

type

  { TRootObject }
  TRootObject = class sealed(specialize TJtdMap<TJtdString>);

implementation

end.
