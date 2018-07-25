program TestLGList;

{$mode objfpc}{$H+}

uses
  Interfaces, Forms, GuiTestRunner, LGListTest;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.

