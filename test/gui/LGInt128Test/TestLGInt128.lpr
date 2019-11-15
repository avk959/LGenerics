program TestLGInt128;

{$mode objfpc}{$H+}

uses
  Interfaces, Forms, GuiTestRunner, LGInt128Test;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.

