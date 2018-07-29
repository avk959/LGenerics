program TestLGTable2D;

{$mode objfpc}{$H+}

uses
  Interfaces, Forms, GuiTestRunner, LGTable2DTest;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.

