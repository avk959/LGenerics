program TestLgPdo;

{$mode objfpc}{$H+}

uses
  Interfaces, Forms, GuiTestRunner, lgPdoTest;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.

