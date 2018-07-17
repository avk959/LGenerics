program TestLGArrayHelpers;

{$mode objfpc}{$H+}

uses
  Interfaces, Forms, LGArrayHelpUtilTest, LGBaseArrayHelperTest, LGComparableArrayHelperTest,
  LGDelegatedArrayHelperTest, LGNestedArrayHelperTest, LGNumArrayHelperTest, LGRegularArrayHelperTest,
  GuiTestRunner;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.

