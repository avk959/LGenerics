program TestLgJsonUtils;

{$mode objfpc}{$H+}

uses
  Interfaces, Forms, GuiTestRunner, LGJsonUtilsTest;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.

