program TestLGSparseGraph;

{$mode objfpc}{$H+}

uses
  Interfaces, Forms, GuiTestRunner, LGSparseGraphTest;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.

