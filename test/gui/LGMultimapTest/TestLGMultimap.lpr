program TestLGMultimap;

{$mode objfpc}{$H+}

uses
  Interfaces, Forms, LGMultiMap, GuiTestRunner, LGMultimapTest;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.

