unit LGJsonTest;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, FileUtil,
  lgUtils,
  lgJson;

type

  { TTestJson }

  TTestJson = class(TTestCase)
  private
  published
    procedure RunSuit;
  end;

var
  TestFileList: TStringList = nil;

implementation


{ TTestJson }

procedure LoadFileList;
var
  Dir: string;
begin
  Dir := ExcludeTrailingPathDelimiter(ProgramDirectory);
  while (Dir <> '') and (ExtractFileName(Dir) <> 'test') do
    Dir := ExcludeTrailingPathDelimiter(ExtractFilePath(Dir));
  if Dir = '' then exit;
  Dir := Dir + DirectorySeparator + 'json_testset' +
         DirectorySeparator + 'testset' + DirectorySeparator;
  if not DirectoryExists(Dir) then exit;
  TestFileList := FindAllFiles(Dir);
end;

procedure TTestJson.RunSuit;
var
  Node: specialize TGAutoRef<TJsonNode>;
  Stream: specialize TGAutoRef<TStringStream>;
  CurrFile, fn: string;
  Result: Boolean;
  c: AnsiChar;
  Total: Integer = 0;
begin
  AssertTrue('File list not loaded', Assigned(TestFileList));
  for CurrFile in TestFileList do
    begin
      Stream.Instance.LoadFromFile(CurrFile);
      Result := Node.Instance.Parse(Stream.Instance.DataString);
      fn := ExtractFileName(CurrFile);
      c := fn[1];
      Inc(Total);
      if c = 'y' then
        AssertTrue(fn + ': expected True, but got False', Result)
      else
        if c = 'n' then
          AssertFalse(fn + ': expected False, but got True', Result);
    end;
  AssertTrue(Total = 288);
end;

initialization

  LoadFileList;
  RegisterTest(TTestJson);

finalization

  TestFileList.Free;

end.

