program TestLGVector;

{$mode objfpc}{$H+}

uses
  Interfaces, Forms, LGVectorTest, GuiTestRunner;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.

