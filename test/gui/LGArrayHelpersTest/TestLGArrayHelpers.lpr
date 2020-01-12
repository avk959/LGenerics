program TestLGArrayHelpers;

{$mode objfpc}{$H+}

uses
  Interfaces, Forms, GuiTestRunner, LGArrayHelpUtilTest, LGBaseArrayHelperTest, LGComparableArrayHelperTest,
  LGDelegatedArrayHelperTest, LGRegularArrayHelperTest, LGNestedArrayHelperTest, LGNumArrayHelperTest;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.

