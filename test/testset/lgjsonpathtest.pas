unit LgJsonPathTest;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, FileUtil,
  LgUtils,
  LgJson,
  LgJsonPath;

const
  TEST_DIR    = 'json_testset';
  SPEC_DIR    = 'json_path';
  FUNC_DIR    = 'functions';
  BASIC_TEST  = 'basic.json';
  FILTER_TEST = 'filter.json';
  INDEX_TEST  = 'index_selector.json';
  NAME_TEST   = 'name_selector.json';
  SLICE_TEST  = 'slice_selector.json';

var
  TestDir: string = '';
  FunTestDir: string = '';

type

  { TTestJsonPath }

  TTestJsonPath = class(TTestCase)
  private
  published
    procedure TestBasic;
    procedure TestFilter;
    procedure TestIndex;
    procedure TestName;
    procedure TestSlice;
  end;

implementation

procedure TTestJsonPath.TestBasic;
var
  TestSet: specialize TGAutoRef<TJsonNode>;
begin
  AssertTrue(DirectoryExists(TestDir));
  {%H-}TestSet.Instance := TJsonNode.LoadFromFile(TestDir + BASIC_TEST);
  AssertTrue(TestSet.Instance <> nil);
end;

procedure TTestJsonPath.TestFilter;
var
  TestSet: specialize TGAutoRef<TJsonNode>;
begin
  {%H-}TestSet.Instance := TJsonNode.LoadFromFile(TestDir + FILTER_TEST);
  AssertTrue(TestSet.Instance <> nil);
end;

procedure TTestJsonPath.TestIndex;
var
  TestSet: specialize TGAutoRef<TJsonNode>;
begin
  {%H-}TestSet.Instance := TJsonNode.LoadFromFile(TestDir + INDEX_TEST);
  AssertTrue(TestSet.Instance <> nil);
end;

procedure TTestJsonPath.TestName;
var
  TestSet: specialize TGAutoRef<TJsonNode>;
begin
  {%H-}TestSet.Instance := TJsonNode.LoadFromFile(TestDir + NAME_TEST);
  AssertTrue(TestSet.Instance <> nil);
end;

procedure TTestJsonPath.TestSlice;
var
  TestSet: specialize TGAutoRef<TJsonNode>;
begin
  {%H-}TestSet.Instance := TJsonNode.LoadFromFile(TestDir + SLICE_TEST);
  AssertTrue(TestSet.Instance <> nil);
end;

procedure FindTestDir;
var
  Dir: string;
begin
  Dir := ExcludeTrailingPathDelimiter(ProgramDirectory);
  while (Dir <> '') and (ExtractFileName(Dir) <> 'test') do
    Dir := ExcludeTrailingPathDelimiter(ExtractFilePath(Dir));
  if Dir = '' then exit;
  Dir := Dir + DirectorySeparator + TEST_DIR + DirectorySeparator;
  Dir := Dir + SPEC_DIR + DirectorySeparator;
  if DirectoryExists(Dir) then
    begin
      TestDir := Dir;
      Dir := Dir + FUNC_DIR + DirectorySeparator;
      if DirectoryExists(Dir) then
        FunTestDir := Dir;
    end;

end;

initialization

  FindTestDir;
  RegisterTest(TTestJsonPath);

end.

