program TestLGStack;

{$mode objfpc}{$H+}

uses
  Interfaces, Forms, LGStackTest, GuiTestRunner;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.

