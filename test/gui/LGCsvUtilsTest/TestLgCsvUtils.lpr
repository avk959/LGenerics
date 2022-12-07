program TestLgCsvUtils;

{$mode objfpc}{$H+}

uses
  Interfaces, Forms, lgCsvUtilsTest, GuiTestRunner;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.

