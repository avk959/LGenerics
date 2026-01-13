{
  Source schema: definition_name_collisions.jtd.json

  This unit was automatically created by JtdPasCodegen.
}
unit definition_name_collisions;

{$MODE OBJFPC}{$H+}{$B-}

interface

uses
  SysUtils, lgJson, lgJtdTypes;

type

  TBar1 = TJtdString;

  TBar = TBar1;

  TFoo1 = TBar;

  TFoo = TFoo1;

{ TRootObject: root unit class }
  TRootObject = TFoo;

implementation

end.
