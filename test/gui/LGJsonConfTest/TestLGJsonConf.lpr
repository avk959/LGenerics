program TestLGJsonConf;

{$mode objfpc}{$H+}

uses
  Interfaces, Forms, GuiTestRunner, LGJsonConfTest;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.

