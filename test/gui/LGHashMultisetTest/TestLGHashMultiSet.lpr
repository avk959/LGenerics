program TestLGHashMultiSet;

{$mode objfpc}{$H+}

uses
  Interfaces, Forms, LGHashMultiSetTest, GuiTestRunner;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.

