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

  TBaz = TJtdString;

  TBar = TBaz;

  TFoo = TBar;

{ TRootObject: root unit class }
  TRootObject = TFoo;

implementation

end.
