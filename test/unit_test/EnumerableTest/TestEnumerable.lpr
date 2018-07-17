program TestEnumerable;

{$mode objfpc}{$H+}

uses
  Interfaces, Forms, EnumerableTest, GuiTestRunner;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.

