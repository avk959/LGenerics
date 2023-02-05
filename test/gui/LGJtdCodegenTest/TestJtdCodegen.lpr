program TestJtdCodegen;

{$mode objfpc}{$H+}

uses
  Interfaces, Forms, GuiTestRunner, JtdCodegenTest;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.

