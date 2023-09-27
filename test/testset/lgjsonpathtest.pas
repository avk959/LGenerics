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
    procedure TestFilterGarbage;
    procedure TestIndex;
    procedure TestName;
    procedure TestSlice;
    procedure TestCountFun;
    procedure TestLengthFun;
    procedure TestMatchFun;
    procedure TestSearchFun;
    procedure TestValueFun;
    procedure TestMore;
    procedure IRegexpPass;
    procedure IRegexpFail;
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
      AssertTrue(CurrTest.Find('selector', Doc));
      Query := Doc.AsString;
      AssertTrue(CurrTest.Find('name', Doc));
      TstName := Doc.AsString;
      if CurrTest.Find('invalid_selector', Invalid) then
        AssertFalse(FalseAccept(TstName, Query), JpParseQuery(Query, Path))
      else
        begin
          AssertTrue(FalseReject(TstName, Query), JpParseQuery(Query, Path));
          AssertTrue(CurrTest.Find('document', Doc));
          AssertTrue(CurrTest.Find('result', Expect));
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

procedure TTestJsonPath.TestFilterGarbage;
var
  I: IJsonPath;
begin
  AssertFalse(JpParseQuery('$[?@.name == "value")]', I));
  AssertFalse(JpParseQuery('$[?@.name == "value"0]', I));
  AssertFalse(JpParseQuery('$[?@.name == "value"(]', I));
  AssertFalse(JpParseQuery('$[?@.name == "value" null]', I));

  AssertFalse(JpParseQuery('$.list[?@.name == "value")]', I));
  AssertFalse(JpParseQuery('$.list[?@.name == "value"0]', I));
  AssertFalse(JpParseQuery('$.list[?@.name == "value"(]', I));
  AssertFalse(JpParseQuery('$.list[?@.name == "value" null]', I));
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
      AssertTrue(CurrTest.Find('selector', Doc));
      Query := Doc.AsString;
      AssertTrue(CurrTest.Find('id', Doc));
      TstName := Doc.AsString;
      if CurrTest.Find('should_be', Expect) then
        begin
          if Expect.IsString and (Expect.AsString = 'NOT_SUPPORTED') then
            AssertFalse(FalseAccept(TstName, Query), JpParseQuery(Query, Path))
          else
            begin
              AssertTrue(FalseReject(TstName, Query), JpParseQuery(Query, Path));
              AssertTrue(CurrTest.Find('document', Doc));
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
                AssertTrue(CurrTest.Find('document', Doc));
                GotOut := Path.MatchValues(Doc);
                AssertTrue(Expected(TstName, Expect, GotOut), TestEqual(GotOut, Expect));
              end;
          end
        else
          AssertFalse(FalseAccept(TstName, Query), JpParseQuery(Query, Path));
    end;
end;

procedure TTestJsonPath.IRegexpPass;
const
  Samples: TStringArray = (
  // branches
    'a|b', 'a|b', 'a|', '|', '||',
  // atom
    'a(a)a', '(b)(ab)c', 'a'#0'b', 'a'#3'b',  'a'#7'b', 'a'#11'b', 'a'#22'b',
    'ab(c(de(f(gh)?i){2}jk)+lmn)*op(rs){1,4}',
  // unicode category
    '\p{C}', '\p{Cc}', '\p{Cf}', '\p{Cn}', '\p{Co}', '\p{L}', '\p{Ll}', '\p{Lm}', '\p{Lo}',
    '\p{Lt}', '\p{Lu}', '\p{M}', '\p{Mc}', '\p{Me}', '\p{Mn}', '\p{N}', '\p{Nd}', '\p{Nl}',
    '\p{No}', '\p{P}', '\p{Pc}', '\p{Pd}', '\p{Pe}', '\p{Pf}', '\p{Pi}', '\p{Po}', '\p{Ps}',
    '\p{Z}', '\p{Zl}', '\p{Zp}', '\p{Zs}', '\p{S}', '\p{Sc}', '\p{Sk}', '\p{Sm}', '\p{So}',
  // caret
    'a^b', 'a^\^b',
  // char class expr
    '[\p{L}]', '[\p{M}]', '[\p{Lo}\p{Me}]',
    '[^-a]', '[^a-]', '[^-a-]', '[^-a-b]', '[^-^-^]', '[\p{So}]', '[^-\p{Mn}-]', '[^--]', '[--]',
    '[a^]', '[a^a]', '[^a^]', '[^^a^]', '[^a^a]',
  // singleCharEsc in char class expr
    '[\(]', '[\)]', '[\*]', '[\+]', '[\-]', '[\.]', '[\?]', '[\?]', '[\[]',
    '[\\]', '[\]]', '[\^]', '[\n]', '[\r]', '[\t]', '[\{]', '[\|]', '[\}]',
    '[\(\*\+\)]', '[\(\-\.\)]', '[\(\?\)]', '[\n\r\t]',
  // leading or trailing hyphen
    '[-a]', '[a-]', '[-a-]', '[-a-b]', '[-a-bc-]',
  // escape
    '\.', '\?', '\(', '\)', '\*', '\+', '\[', '\]', '\{', '\}', '\|', '\n', '\r', '\t',
    '[\r\n\t]', '[^\r\n\t]', '[^\r-\n\t-]',
  // dot
    '.', '..', '.*', '.?', '.+',
  // singleCharEsc
    '\(', '\)', '\*', '\+', '\-', '\.', '\?', '\?', '\[', '\\', '\]',
   '\^', '\n', '\r', '\t', '\{', '\|', '\}',
   '\(\*\+\)', '\(\-\.\)', '\(\?\)', '\n\r\t',
  // parens
    '(a)', '(a)+', '(a){2}', '(a){2,3}', '()', 'a()', '(0*)*1',
  // quantifiers
    'a*', 'a+', 'a?', 'a{2}', 'a{2,3}', 'a{4,}',
  // Appendix A
    '([0-9a-fA-F]{2}(:[0-9a-fA-F]{2})*)?', '[0-9a-fA-F]{2}(:[0-9a-fA-F]{2}){5}',
    '((:|[0-9a-fA-F]{0,4}):)([0-9a-fA-F]{0,4}:){0,5}', '(([^:]+:){6}(([^:]+:[^:]+)|(.*\..*)))|',
    '(([^:]+:){6}(([^:]+:[^:]+)|(.*\..*)))|', '(([^:]+:){6}(([^:]+:[^:]+)|(.*\..*)))|',
    '(([^:]+:){6}(([^:]+:[^:]+)|(.*\..*)))|', '((:|[0-9a-fA-F]{0,4}):)([0-9a-fA-F]{0,4}:){0,5}',
    '[0-9a-fA-F]*', '[aeiouy]*', '[A-Z][a-z]*', '\*', '[^\*].*',
    '[a-zA-Z_][a-zA-Z0-9\-_.]*', '.|..|[^xX].*|.[^mM].*|..[^lL].*', '([0-9a-fA-F]{2}(:[0-9a-fA-F]{2})*)?',
    '[0-9a-fA-F]{2}(:[0-9a-fA-F]{2}){5}', '([0-9a-fA-F]{2}(:[0-9a-fA-F]{2})*)?',
    '[0-9a-fA-F]{8}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-', '((:|[0-9a-fA-F]{0,4}):)([0-9a-fA-F]{0,4}:){0,5}',
    '[0-9\\.]*', '[0-9a-fA-F:\.]*', '((:|[0-9a-fA-F]{0,4}):)([0-9a-fA-F]{0,4}:){0,5}',
    '([0-9a-fA-F]){2}(:([0-9a-fA-F]){2}){0,254}', '([0-9a-fA-F]){2}(:([0-9a-fA-F]){2}){4,31}',
    '[0-9a-fA-F]*', '[a-zA-Z_][a-zA-Z0-9\-_.]*', '[xX][mM][lL].*', '[A-Z]{2}', '\*',
    '[0-9]{8}\.[0-9]{6}', '(2((2[4-9])|(3[0-9]))\.).*', '(([fF]{2}[0-9a-fA-F]{2}):).*',
    '[A-Z]{2}', '\*', '[^\*].*', '[0-9\.]*', '[a-zA-Z_][a-zA-Z0-9\-_.]*',
    '.|..|[^xX].*|.[^mM].*|..[^lL].*', '/?([a-zA-Z0-9\-_.]+)(/[a-zA-Z0-9\-_.]+)*',
    '([a-zA-Z0-9\-_.]+:)*', '[0-9a-fA-F]{2}(:[0-9a-fA-F]{2}){7}'
  );
  Fmt = 'Total = %d, but Passed = %d';
var
  s: string;
  Total, Passed: Integer;
  I: SizeInt;
begin
  Total := 0;
  Passed := 0;
  for s in Samples do
    begin
      Inc(Total);
      Inc(Passed, Ord(IRegexpCheck(s, I) = ircOk));
    end;
  AssertTrue(Format(Fmt, [Total, Passed]), Total = Passed);
end;

procedure TTestJsonPath.IRegexpFail;
const
  Samples: TStringArray = (
  // branch
    '|?',
  // unicode category
    '\p{Cx}', '\p{Lx}', '\p{Mx}', '\p{Nx}', '\p{Px}', '\p{Zx}', '\p{Sx}', '\p', '\p{', '\p{}',
  // char class expr
    '[\a]', '[\0]', '[\,]', '[',']', '[^]', '[a--b]', '[a-z-A-Z]', '[--a]', '[^\p{Cc}-\p{Me}]',
  // escape
    '\a', '\0', '\,',
  // parens
    '(', ')', '(a', 'a)',
  // quantifiers
    '*', '+', '?', 'a**', 'a++', 'a??', '{', '}', '{}', '{,}', '{4,}', '{,2}', 'a{', 'a}', 'a{}',
    'a{,}', 'a{,2}',
  // Appendix A
    '\p{IsBasicLatin}{0,255}', '\S(.*\S)?', '(([0-1](\.[1-3]?[0-9]))|(2\.(0|([1-9]\d*))))',
    '\d*(\.\d*){1,127}', '\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}(\.\d+)?', '\S+', '\S(.*\S)?',
    '(([0-1](\.[1-3]?[0-9]))|(2\.(0|([1-9]\d*))))', '\d*(\.\d*){1,127}',
    '\d{4}-\d{2}-\d{2}T\\d{2}:\d{2}:\d{2}(\.\d+)?', '\d{2}:\d{2}:\d{2}(\.\d+)?',
    '\d{2}:\d{2}:\d{2}(\.\d+)?', '\d{4}-\d{2}-\d{2}', 'Z|[\+\-]\d{2}:\d{2}',
    '\d{4}-\d{2}-\d{2}', '\d{4}-\d{2}-\d{2}', '[\S ]+'
  );
  Fmt = 'Total = %d, but Failed = %d';
var
  s: string;
  Total, Failed: Integer;
  I: SizeInt;
begin
  Total := 0;
  Failed := 0;
  for s in Samples do
    begin
      Inc(Total);
      Inc(Failed, Ord(IRegexpCheck(s, I) <> ircOk));
    end;
  AssertTrue(Format(Fmt, [Total, Failed]), Total = Failed);
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

