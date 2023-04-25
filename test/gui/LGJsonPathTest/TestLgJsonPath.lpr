program TestLgJsonPath;

{$mode objfpc}{$H+}

uses
  Interfaces, Forms, GuiTestRunner, LgJsonPathTest;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.

