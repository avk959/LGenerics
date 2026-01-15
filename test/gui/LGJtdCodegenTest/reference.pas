{
  Source schema: reference.jtd.json

  This unit was automatically created by JtdPasCodegen.
}
unit reference;

{$MODE OBJFPC}{$H+}{$B-}

interface

uses
  SysUtils, lgJson, lgJtdTypes;

type

  { TBaz }
  TBaz = TJtdString;

  { TBar }
  TBar = TBaz;

  { TFoo }
  TFoo = TBar;

  { TRootObject }
  TRootObject = TFoo;

implementation

end.
