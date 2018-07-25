program TestLGTreeMap;

{$mode objfpc}{$H+}

uses
  Interfaces, Forms, LGTreeMapTest, GuiTestRunner;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.

