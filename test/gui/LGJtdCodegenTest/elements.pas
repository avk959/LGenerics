{
  Source schema: elements.jtd.json

  This unit was automatically created by JtdPasCodegen.
}
unit elements;

{$MODE OBJFPC}{$H+}{$B-}

interface

uses
  SysUtils, lgJson, lgJtdTypes;

type

  TElements = class sealed(specialize TJtdList<TJtdString>);

implementation

end.
