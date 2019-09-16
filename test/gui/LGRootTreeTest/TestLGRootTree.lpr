program TestLGRootTree;

{$mode objfpc}{$H+}

uses
  Interfaces, Forms, GuiTestRunner, LGRootTreeTest;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.

