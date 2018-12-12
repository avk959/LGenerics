program TestLGSimpleGraph;

{$mode objfpc}{$H+}

uses
  Interfaces, Forms, GuiTestRunner, LGSimpleGraphTest;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.

