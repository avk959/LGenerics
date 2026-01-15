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

  { TBar1 }
  TBar1 = TJtdString;

  { TBar }
  TBar = TBar1;

  { TFoo1 }
  TFoo1 = TBar;

  { TFoo }
  TFoo = TFoo1;

  { TRootObject }
  TRootObject = TFoo;

implementation

end.
