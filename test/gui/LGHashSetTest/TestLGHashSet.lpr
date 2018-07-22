program TestLGHashSet;

{$mode objfpc}{$H+}

uses
  Interfaces, Forms, LGHashSetTest, GuiTestRunner;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.

