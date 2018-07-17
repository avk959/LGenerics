program TestLGUtils;

{$mode objfpc}{$H+}

uses
  Interfaces, Forms, LGUtilsTest, GuiTestRunner;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.

