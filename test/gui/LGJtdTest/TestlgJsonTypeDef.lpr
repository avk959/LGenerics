program TestlgJsonTypeDef;

{$mode objfpc}{$H+}

uses
  Interfaces, Forms, GuiTestRunner, lgJtdTest;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.

