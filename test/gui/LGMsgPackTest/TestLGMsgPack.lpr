program TestLGMsgPack;

{$mode objfpc}{$H+}

uses
  Interfaces, Forms, GuiTestRunner, LGMsgPackTest;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.

