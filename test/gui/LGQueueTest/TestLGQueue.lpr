program TestLGQueue;

{$mode objfpc}{$H+}

uses
  Interfaces, Forms, LGQueueTest, GuiTestRunner;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.

