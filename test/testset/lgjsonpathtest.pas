unit LgJsonPathTest;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, FileUtil,
  LgUtils,
  LgJson,
  LgJsonPath;

const
  TEST_DIR     = 'json_testset';
  SPEC_DIR     = 'json_path';
  FUNC_DIR     = 'functions';
  BASIC_TEST   = 'basic.json';
  FILTER_TEST  = 'filter.json';
  INDEX_TEST   = 'index_selector.json';
  NAME_TEST    = 'name_selector.json';
  SLICE_TEST   = 'slice_selector.json';
  COUNT_TEST   = 'count.json';
  LENGTH_TEST  = 'length.json';
  MATCH_TEST   = 'match.json';
  SEARCH_TEST  = 'search.json';
  VALUE_TEST   = 'value.json';
  REGRESS_TEST = 'regress.json';

var
  TestDir: string = '';
  FunTestDir: string = '';

type

  { TTestJsonPath }

  TTestJsonPath = class(TTestCase)
  private
    function  FalseReject(const aName, aQuery: string): string;
    function  FalseAccept(const aName, aQuery: string): string;
    function  Expected(const aName: string; aExpect: TJsonNode; aGot: TJpValueList): string;
    function  TestEqual(aLeft: TJpValueList; aRight: TJsonNode): Boolean;
    procedure RunTestSet(aTestSet: TJsonNode);
  published
    procedure TestBasic;
    procedure TestFilter;
    procedure TestIndex;
    procedure TestName;
    procedure TestSlice;
    procedure TestCountFun;
    procedure TestLengthFun;
    procedure TestMatchFun;
    procedure TestSearchFun;
    procedure TestValueFun;
    procedure TestMore;
  end;

implementation

function TTestJsonPath.FalseReject(const aName, aQuery: string): string;
const
  Fmt = '"%s": query "%s" must be accepted';
begin
  Result := Format(Fmt, [aName, aQuery]);
end;

function TTestJsonPath.FalseAccept(const aName, aQuery: string): string;
const
  Fmt = '"%s": query "%s" must be rejected';
begin
  Result := Format(Fmt, [aName, aQuery]);
end;

function TTestJsonPath.Expected(const aName: string; aExpect: TJsonNode; aGot: TJpValueList): string;
const
  Fmt = '"%s": expected "%s", but got "%s"';
begin
  Result := Format(Fmt, [aName, aExpect.AsJson, aGot.AsJson]);
end;

function TTestJsonPath.TestEqual(aLeft: TJpValueList; aRight: TJsonNode): Boolean;
var
  I: SizeInt;
  Node: TJsonNode;
  Found: Boolean;
begin
  if not aRight.IsArray then exit(False);
  if Length(aLeft) <> aRight.Count then exit(False);
  for Node in aLeft do
    begin
      Found := False;
      for I := 0 to Pred(aRight.Count) do
        if Node.EqualTo(aRight.Items[I]) then
          begin
            Found := True;
            break;
          end;
      if not Found then exit(False);
    end;
  Result := True;
end;

procedure TTestJsonPath.RunTestSet(aTestSet: TJsonNode);
var
  CurrTest, Doc, Expect, Invalid: TJsonNode;
  GotOut: TJpValueList;
  TstName, Query: string;
  Path: IJsonPath;
begin
  for CurrTest in aTestSet do
    begin
      AssertTrue(CurrTest.IsObject);
      Query := CurrTest['selector'].AsString;
      TstName := CurrTest['name'].AsString;
      if CurrTest.Find('invalid_selector', Invalid) then
        AssertFalse(FalseAccept(TstName, Query), JpParseQuery(Query, Path))
      else
        begin
          AssertTrue(FalseReject(TstName, Query), JpParseQuery(Query, Path));
          Doc := CurrTest['document'];
          Expect := CurrTest['result'];
          GotOut := Path.MatchValues(Doc);
          AssertTrue(Expected(TstName, Expect, GotOut), TestEqual(GotOut, Expect));
        end;
    end;
end;

procedure TTestJsonPath.TestBasic;
var
  TestSet: specialize TGAutoRef<TJsonNode>;
  Tests: TJsonNode;
begin
  AssertTrue(DirectoryExists(TestDir));
  {%H-}TestSet.Instance := TJsonNode.LoadFromFile(TestDir + BASIC_TEST);
  AssertTrue(TestSet.Instance <> nil);
  AssertTrue(TestSet.Instance.Find('tests', Tests));
  AssertTrue(Tests.IsArray);
  RunTestSet(Tests);
end;

procedure TTestJsonPath.TestFilter;
var
  TestSet: specialize TGAutoRef<TJsonNode>;
  Tests: TJsonNode;
begin
  {%H-}TestSet.Instance := TJsonNode.LoadFromFile(TestDir + FILTER_TEST);
  AssertTrue(TestSet.Instance <> nil);
  AssertTrue(TestSet.Instance.Find('tests', Tests));
  AssertTrue(Tests.IsArray);
  RunTestSet(Tests);
end;

procedure TTestJsonPath.TestIndex;
var
  TestSet: specialize TGAutoRef<TJsonNode>;
  Tests: TJsonNode;
begin
  {%H-}TestSet.Instance := TJsonNode.LoadFromFile(TestDir + INDEX_TEST);
  AssertTrue(TestSet.Instance <> nil);
  AssertTrue(TestSet.Instance.Find('tests', Tests));
  AssertTrue(Tests.IsArray);
  RunTestSet(Tests);
end;

procedure TTestJsonPath.TestName;
var
  TestSet: specialize TGAutoRef<TJsonNode>;
  Tests: TJsonNode;
begin
  {%H-}TestSet.Instance := TJsonNode.LoadFromFile(TestDir + NAME_TEST);
  AssertTrue(TestSet.Instance <> nil);
  AssertTrue(TestSet.Instance.Find('tests', Tests));
  AssertTrue(Tests.IsArray);
  RunTestSet(Tests);
end;

procedure TTestJsonPath.TestSlice;
var
  TestSet: specialize TGAutoRef<TJsonNode>;
  Tests: TJsonNode;
begin
  {%H-}TestSet.Instance := TJsonNode.LoadFromFile(TestDir + SLICE_TEST);
  AssertTrue(TestSet.Instance <> nil);
  AssertTrue(TestSet.Instance.Find('tests', Tests));
  AssertTrue(Tests.IsArray);
  RunTestSet(Tests);
end;

procedure TTestJsonPath.TestCountFun;
var
  TestSet: specialize TGAutoRef<TJsonNode>;
  Tests: TJsonNode;
begin
  {%H-}TestSet.Instance := TJsonNode.LoadFromFile(FunTestDir + COUNT_TEST);
  AssertTrue(TestSet.Instance <> nil);
  AssertTrue(TestSet.Instance.Find('tests', Tests));
  AssertTrue(Tests.IsArray);
  RunTestSet(Tests);
end;

procedure TTestJsonPath.TestLengthFun;
var
  TestSet: specialize TGAutoRef<TJsonNode>;
  Tests: TJsonNode;
begin
  {%H-}TestSet.Instance := TJsonNode.LoadFromFile(FunTestDir + LENGTH_TEST);
  AssertTrue(TestSet.Instance <> nil);
  AssertTrue(TestSet.Instance.Find('tests', Tests));
  AssertTrue(Tests.IsArray);
  RunTestSet(Tests);
end;

procedure TTestJsonPath.TestMatchFun;
var
  TestSet: specialize TGAutoRef<TJsonNode>;
  Tests: TJsonNode;
begin
  {%H-}TestSet.Instance := TJsonNode.LoadFromFile(FunTestDir + MATCH_TEST);
  AssertTrue(TestSet.Instance <> nil);
  AssertTrue(TestSet.Instance.Find('tests', Tests));
  AssertTrue(Tests.IsArray);
  RunTestSet(Tests);
end;

procedure TTestJsonPath.TestSearchFun;
var
  TestSet: specialize TGAutoRef<TJsonNode>;
  Tests: TJsonNode;
begin
  {%H-}TestSet.Instance := TJsonNode.LoadFromFile(FunTestDir + SEARCH_TEST);
  AssertTrue(TestSet.Instance <> nil);
  AssertTrue(TestSet.Instance.Find('tests', Tests));
  AssertTrue(Tests.IsArray);
  RunTestSet(Tests);
end;

procedure TTestJsonPath.TestValueFun;
var
  TestSet: specialize TGAutoRef<TJsonNode>;
  Tests: TJsonNode;
begin
  {%H-}TestSet.Instance := TJsonNode.LoadFromFile(FunTestDir + VALUE_TEST);
  AssertTrue(TestSet.Instance <> nil);
  AssertTrue(TestSet.Instance.Find('tests', Tests));
  AssertTrue(Tests.IsArray);
  RunTestSet(Tests);
end;

procedure TTestJsonPath.TestMore;
var
  TestSet: specialize TGAutoRef<TJsonNode>;
  Tests, CurrTest, Doc, Expect: TJsonNode;
  GotOut: TJpValueList;
  TstName, Query: string;
  Path: IJsonPath;
begin
  AssertTrue(DirectoryExists(TestDir));
  {%H-}TestSet.Instance := TJsonNode.LoadFromFile(TestDir + REGRESS_TEST);
  AssertTrue(TestSet.Instance <> nil);
  AssertTrue(TestSet.Instance.Find('queries', Tests));
  AssertTrue(Tests.IsArray);

  for CurrTest in Tests do
    begin
      AssertTrue(CurrTest.IsObject);
      Query := CurrTest['selector'].AsString;
      TstName := CurrTest['id'].AsString;
      if CurrTest.Find('should_be', Expect) then
        begin
          if Expect.IsString and (Expect.AsString = 'NOT_SUPPORTED') then
            AssertFalse(FalseAccept(TstName, Query), JpParseQuery(Query, Path))
          else
            begin
              AssertTrue(FalseReject(TstName, Query), JpParseQuery(Query, Path));
              Doc := CurrTest['document'];
              GotOut := Path.MatchValues(Doc);
              AssertTrue(Expected(TstName, Expect, GotOut), TestEqual(GotOut, Expect));
            end;
        end
      else
        if CurrTest.Find('consensus', Expect) then
          begin
            if Expect.IsString and (Expect.AsString = 'NOT_SUPPORTED') then
              AssertFalse(FalseAccept(TstName, Query), JpParseQuery(Query, Path))
            else
              begin
                AssertTrue(FalseReject(TstName, Query), JpParseQuery(Query, Path));
                Doc := CurrTest['document'];
                GotOut := Path.MatchValues(Doc);
                AssertTrue(Expected(TstName, Expect, GotOut), TestEqual(GotOut, Expect));
              end;
          end
        else
          AssertFalse(FalseAccept(TstName, Query), JpParseQuery(Query, Path));
    end;
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

