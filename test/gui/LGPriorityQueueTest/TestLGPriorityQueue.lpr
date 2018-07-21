program TestLGPriorityQueue;

{$mode objfpc}{$H+}

uses
  Interfaces, Forms, LGPriorityQueueTest, GuiTestRunner;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.

