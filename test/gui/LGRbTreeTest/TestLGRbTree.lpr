program TestLGRbTree;

{$mode objfpc}{$H+}

uses
  Interfaces, Forms, GuiTestRunner, LGRbTreeTest;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.

