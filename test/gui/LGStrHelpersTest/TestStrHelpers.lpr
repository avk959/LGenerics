program TestStrHelpers;

{$mode objfpc}{$H+}

uses
  Interfaces, Forms, GuiTestRunner, LGStrHelpersTest;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.

