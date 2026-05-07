program TestLGUnishox;

{$mode objfpc}{$H+}

uses
  Interfaces, Forms, GuiTestRunner, LgUnishoxTest;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.

