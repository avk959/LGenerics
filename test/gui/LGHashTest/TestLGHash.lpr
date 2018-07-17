program TestLGHash;

{$mode objfpc}{$H+}

uses
  Interfaces, Forms, LGHashTest, GuiTestRunner;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.

