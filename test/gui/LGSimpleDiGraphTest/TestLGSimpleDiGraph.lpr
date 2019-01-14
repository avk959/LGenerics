program TestLGSimpleDiGraph;

{$mode objfpc}{$H+}

uses
  Interfaces, Forms, GuiTestRunner, LGSimpleDiGraphTest;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.

