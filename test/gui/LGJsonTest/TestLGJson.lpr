program TestLGJson;

{$mode objfpc}{$H+}

uses
  Interfaces, Forms, GuiTestRunner, LGJsonTest;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.

