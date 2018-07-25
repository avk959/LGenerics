program TestLGHashMap;

{$mode objfpc}{$H+}

uses
  Interfaces, Forms, LGHashMapTest, GuiTestRunner;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.

