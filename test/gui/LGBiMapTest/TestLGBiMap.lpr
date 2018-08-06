program TestLGBiMap;

{$mode objfpc}{$H+}

uses
  Interfaces, Forms, GuiTestRunner, LGBiMapTest;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.

