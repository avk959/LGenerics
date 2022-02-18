program TestSeqUtils;

{$mode objfpc}{$H+}

uses
  Interfaces, Forms, GuiTestRunner, lgSeqUtilsTest;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.

