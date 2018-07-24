program TestLGTreeMultiSet;

{$mode objfpc}{$H+}

uses
  Interfaces, Forms, LGTreeMultisetTest, GuiTestRunner;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.

