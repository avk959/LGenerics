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

  TValues = class sealed(specialize TJtdMap<TJtdString>);

implementation

end.
