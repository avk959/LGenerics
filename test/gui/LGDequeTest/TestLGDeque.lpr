program TestLGDeque;

{$mode objfpc}{$H+}

uses
  Interfaces, Forms, LGDequeTest, GuiTestRunner;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.

