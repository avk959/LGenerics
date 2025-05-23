unit LgJsonPathTest;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, FileUtil,
  LgUtils,
  LgVector,
  LgJson,
  LgJsonPath;

const
  TEST_DIR     = 'json_testset';
  SPEC_DIR     = 'json_path';
  CTS_JSON     = 'cts.json';
  REGRESS_JSON = 'regress.json';

var
  TestDir: string = '';

type

  { TTestJsonPath }

  TTestJsonPath = class(TTestCase)
  private
  type
    TJsonVect = specialize TGLiteVector<TJsonNode>;
    TStrVect  = specialize TGLiteVector<string>;
    function  FalseReject(const aName, aQuery: string): string;
    function  FalseAccept(const aName, aQuery: string): string;
    function  Expected(const aName: string; aExpect: TJsonNode; aGot: TJpValueList): string;
    function  ExpectedValues(aLeft: TJpValueList; aRight: TJsonNode): Boolean;
    function  ExpectedList(const aList: TJpNodeList; aValues, aPaths: TJsonNode): Boolean;
  published
    procedure TestFilterGarbage;
    procedure CtsTest;
    procedure TestMore;
    procedure IRegexpPass;
    procedure IRegexpFail;
    procedure JpNodeToSegments;
  end;

  { TTestAuxFun }

  TTestAuxFun = class(TTestCase)
  private
  type
    TAutoStrList = specialize TGAutoRef<TStringList>;
    TTestData = record
      Value,
      Query,
      Expected: string;
    end;
    TTestSet = array of TTestData;
  const
    JsonFmt   = 'Item[%d]: invalid JSON';
    QueryFmt  = 'Item[%d]: invalid query';
    ExpectFmt = 'Item[%d]: expected "%s", but got "%s"';
    procedure RunTestSet(const aTestSet: TTestSet; aErrList: TStrings);
  published
    procedure TestKey;
    procedure TestParam;
    procedure TestIsBoolean;
    procedure TestIsNumber;
    procedure TestIsInteger;
    procedure TestIsString;
    procedure TestIsArray;
    procedure TestIsObject;
    procedure TestIsStruct;
    procedure TestContains;
    procedure TestContainsText;
    procedure TestHasWord;
    procedure TestHasWordCi;
    procedure TestSameText;
    procedure TestStartsWith;
    procedure TestEndsWith;
    procedure TestToUpper;
    procedure TestToLower;
    procedure TestConcat;
    procedure TestMin;
    procedure TestMax;
    procedure TestAbs;
    procedure TestSum;
    procedure TestDiff;
    procedure TestProd;
  end;

implementation
{$B-}

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

function TTestJsonPath.ExpectedValues(aLeft: TJpValueList; aRight: TJsonNode): Boolean;
var
  ValList: TJsonVect;
  I: SizeInt;
  Node: TJsonNode;
begin
  if not aRight.IsArray then exit(False);
  if Length(aLeft) <> aRight.Count then exit(False);
  for Node in aRight do
    ValList.Add(Node);
  for Node in aLeft do
    for I := 0 to Pred(ValList.Count) do
      if Node.EqualTo(ValList[I]) then
        begin
          ValList.Extract(I);
          break;
        end;
  Result := ValList.IsEmpty;
end;

function TTestJsonPath.ExpectedList(const aList: TJpNodeList; aValues, aPaths: TJsonNode): Boolean;
var
  VList: TJsonVect;
  PList: TStrVect;
  Node: TJsonNode;
  I, J: SizeInt;
begin
  for Node in aValues.Items[0] do
    VList.Add(Node);
  for Node in aPaths.Items[0] do
    PList.Add(Node.AsString);
  for I := 0 to High(aList) do
    for J := 0 to Pred(VList.Count) do
      if aList[I].Value.EqualTo(VList[J]) then begin
        if aList[I].Path <> PList[J] then exit(False);
        VList.Extract(J);
        PList.Extract(J);
        break;
      end;
  Result := VList.IsEmpty;
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

procedure TTestJsonPath.CtsTest;
var
  TestLog: specialize TGAutoRef<TStringList>;
  TestSet: specialize TGAutoRef<TJsonNode>;
  Tests, CurrTest, Sel, Doc, Res, ResPaths, Node: TJsonNode;
  Matcher: IJsonPath;
  GotList: TJpNodeList;
  TestFile, TstName: string;
  Log: TStringList;
  I, J: Integer;
const
  Fmt = 'Test #%d("%s"): %s';
begin
  Log := TestLog;
  I := -1;
  if not DirectoryExists(TestDir) then
    Log.Add('TestDir not found')
  else begin
    TestFile := TestDir + 'cts.json';
    if not FileExists(TestFile) then
      Log.Add('cts.json not found')
    else begin
      {%H-}TestSet.Instance := TJsonNode.LoadFromFile(TestFile);
      if not TestSet.Instance.Find('tests', Tests) then
        Log.Add('tests entry in cts.json not found')
      else
        if not Tests.IsArray then
          Log.Add('tests entry in cts.json is not array')
        else
          if Tests.Count = 0 then
            Log.Add('tests entry in cts.json is empty')
          else
            for CurrTest in Tests do begin
              Inc(I);
              if not(CurrTest.Find('name', Node) and Node.IsString) then begin
                Log.Add(Format('Test #%d does not have a name, ignored', [I]));
                continue;
              end;
              TstName := Node.AsString;
              if not(CurrTest.Find('selector', Sel) and Sel.IsString)then begin
                Log.Add(Format(Fmt, [I, TstName, 'selector not found, ignored']));
                continue;
              end;
              if CurrTest.Find('invalid_selector', Node) and Node.IsTrue then begin
                if JpParseQuery(Sel.AsString, Matcher) then
                  Log.Add(Format(Fmt, [I, TstName, 'false query accept']));
                continue;
              end else
                if not JpParseQuery(Sel.AsString, Matcher) then begin
                  Log.Add(Format(Fmt, [I, TstName, 'false query reject']));
                  continue;
                end;
              if not CurrTest.Find('document', Doc) then begin
                Log.Add(Format(Fmt, [I, TstName, 'document not found, ignored']));
                continue;
              end;
              GotList := Matcher.Match(Doc);
              if CurrTest.Find('results', Res) then begin
                if not Res.IsArray then begin
                  Log.Add(Format(Fmt, [I, TstName, 'non-array results, ignored']));
                  continue;
                end;
                if Res.Count < 2 then begin
                  Log.Add(Format(Fmt, [I, TstName, 'results too small, ignored']));
                  continue;
                end;
                if not(CurrTest.Find('results_paths', ResPaths) and ResPaths.IsArray) then begin
                  Log.Add(Format(Fmt, [I, TstName, 'results_paths not found, ignored']));
                  continue;
                end;
                if (Res.Count <> ResPaths.Count) or (Res.Items[0].Count <> ResPaths.Items[0].Count) then begin
                  Log.Add(Format(Fmt, [I, TstName, 'results size does not match paths size, ignored']));
                  continue;
                end;
                if not ExpectedList(GotList, Res, ResPaths) then begin
                  Log.Add(Format(Fmt, [I, TstName, 'unexpected result']));
                end;
              end else begin
                if not(CurrTest.Find('result', Res) and Res.IsArray) then
                  begin
                    Log.Add(Format(Fmt, [I, TstName, 'result array not found, ignored']));
                    continue;
                  end;
                if not(CurrTest.Find('result_paths', ResPaths) and ResPaths.IsArray) then
                  begin
                    Log.Add(Format(Fmt, [I, TstName, 'path array not found, ignored']));
                    continue;
                  end;
                if Res.Count <> ResPaths.Count then begin
                  Log.Add(Format(Fmt, [I, TstName, 'result size does not match path size, ignored']));
                  continue;
                end;
                if Length(GotList) <> Res.Count then begin
                  Log.Add(Format(Fmt, [I, TstName, 'unexpected result size']));
                  continue;
                end;
                for J := 0 to High(GotList) do begin
                  if not GotList[J].Value.EqualTo(Res.Items[J]) then begin
                    Log.Add(Format(Fmt, [I, TstName, 'result value mismatch']));
                    break;
                  end;
                  if GotList[J].Path <> ResPaths.Items[J].AsString then begin
                    Log.Add(Format(Fmt, [I, TstName, 'result path mismatch']));
                    break;
                  end;
                end;
              end;
            end;
    end;
  end;
  AssertTrue(Log.Text, Log.Count = 0);
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
  {%H-}TestSet.Instance := TJsonNode.LoadFromFile(TestDir + REGRESS_JSON);
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
              AssertTrue(Expected(TstName, Expect, GotOut), ExpectedValues(GotOut, Expect));
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
                AssertTrue(Expected(TstName, Expect, GotOut), ExpectedValues(GotOut, Expect));
              end;
          end
        else
          AssertFalse(FalseAccept(TstName, Query), JpParseQuery(Query, Path));
    end;
end;

procedure TTestJsonPath.IRegexpPass;
const
  Samples: TStringArray = (
  // empty
    '',
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
    '[\r\n\t]', '[^\r\n\t]', '[^\n-\r\t-]',
  // dot
    '.', '..', '.*', '.?', '.+',
  // singleCharEsc
    '\(', '\)', '\*', '\+', '\-', '\.', '\?', '\?', '\[', '\\', '\]',
   '\^', '\n', '\r', '\t', '\{', '\|', '\}',
   '\(\*\+\)', '\(\-\.\)', '\(\?\)', '\n\r\t',
  // parens
    '(a)', '(a)+', '(a){2}', '(a){2,3}', '()', 'a()', '(0*)*1', '((((((((((((((((((((x))))))))))))))))))))',
    '(|||)',
  // quantifiers
    'a*', 'a+', 'a?', 'a{2}', 'a{2,3}', 'a{4,}', 'a{0,}', 'a{0,0}', 'a{1,1}', '(|)*', '(|){0,3}',
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
  s, Msg: string;
  Total, Passed: Integer;
begin
  Total := 0;
  Passed := 0;
  for s in Samples do
    begin
      Inc(Total);
      Inc(Passed, Ord(IRegexpCheck(s, Msg)));
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
    '[c-a]', '[^z-x]',
  // escape
    '\a', '\0', '\,',
  // parens
    '(', ')', '(a', 'a)', '((ab)a',
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
  s, Msg: string;
  Total, Failed: Integer;
begin
  Total := 0;
  Failed := 0;
  for s in Samples do
    begin
      Inc(Total);
      Inc(Failed, Ord(not IRegexpCheck(s, Msg)));
    end;
  AssertTrue(Format(Fmt, [Total, Failed]), Total = Failed);
end;

procedure TTestJsonPath.JpNodeToSegments;
var
  Json: specialize TGAutoRef<TJsonNode>;
  List: TJpNodeList;
  Path: TStringArray;
  Node: TJpNode;
  Found: TJsonNode;
const
  SourceJson =
    '{ "store": {                              ' +
    '    "book": [                             ' +
    '      { "category": "reference",          ' +
    '        "author": "Nigel Rees",           ' +
    '        "title": "Sayings of the Century",' +
    '        "price": 8.95                     ' +
    '      },                                  ' +
    '      { "category": "fiction",            ' +
    '        "author": "Evelyn Waugh",         ' +
    '        "title": "Sword of Honour",       ' +
    '        "price": 12.99                    ' +
    '      },                                  ' +
    '      { "category": "fiction",            ' +
    '        "author": "Herman Melville",      ' +
    '        "title": "Moby Dick",             ' +
    '        "isbn": "0-553-21311-3",          ' +
    '        "price": 8.99                     ' +
    '      },                                  ' +
    '      { "category": "fiction",            ' +
    '        "author": "J. R. R. Tolkien",     ' +
    '        "title": "The Lord of the Rings", ' +
    '        "isbn": "0-395-19395-8",          ' +
    '        "price": 22.99                    ' +
    '      }                                   ' +
    '    ],                                    ' +
    '    "bicycle": {                          ' +
    '      "color": "red",                     ' +
    '      "price": 399                        ' +
    '    }                                     ' +
    '  }                                       ' +
    '}                                         ';

begin
  Json.Instance.AsJson := SourceJson;
  AssertTrue(JpMatch('$..*', Json.Instance, List));
  for Node in List do
    begin
      Path := Node.PathToSegments;
      AssertTrue(Path <> nil);
      AssertTrue(Json.Instance.FindPath(Path, Found));
      AssertTrue(Found = Node.Value);
    end;
end;

{ TTestAuxFun }

procedure TTestAuxFun.RunTestSet(const aTestSet: TTestSet; aErrList: TStrings);
var
  Root: specialize TGAutoRef<TJsonNode>;
  Path: IJsonPath;
  vList: string;
  I: Integer;
begin
  for I := 0 to High(aTestSet) do begin
    if not Root.Instance.TryParse(aTestSet[I].Value) then
      begin
        aErrList.Add(Format(JsonFmt, [I]));
        continue;
      end;
    if not JpParseQuery(aTestSet[I].Query, Path) then
      begin
        aErrList.Add(Format(QueryFmt, [I]));
        continue;
      end;
    vList := Path.MatchValues(Root.Instance).AsJson;
    if vList <> aTestSet[I].Expected then
      aErrList.Add(Format(ExpectFmt, [I, aTestSet[I].Expected, vList]));
  end;
end;

procedure TTestAuxFun.TestKey;
var
  ErrList: TAutoStrList;
const
  Tests: TTestSet = (
    (Value: '["a", 42]';                          Query: '$[?key()==2]';   Expected: '[]'),
    (Value: '["a", [2], 42]';                     Query: '$[?key()==2]';   Expected: '[42]'),
    (Value: '[null, false, [2]]';                 Query: '$[?key()==2]';   Expected: '[[2]]'),
    (Value: '[null, false, {"key":"value"}]';     Query: '$[?key()==2]';   Expected: '[{"key":"value"}]'),
    (Value: '[null, false, {"key":"value"}]';     Query: '$[?key()=="a"]'; Expected: '[]'),
    (Value: '{"a_":"value","b":[null],"c":[42]}'; Query: '$[?key()=="a"]'; Expected: '[]'),
    (Value: '{"aa":"value","a":42,"b":[null]}';   Query: '$[?key()=="a"]'; Expected: '[42]')
  );
begin
  RunTestSet(Tests, ErrList.Instance);
  AssertTrue(ErrList.Instance.Text, ErrList.Instance.Count = 0);
end;

procedure TTestAuxFun.TestParam;
var
  Root: specialize TGAutoRef<TJsonNode>;
  ErrList: TAutoStrList;
  Path: IJsonPath;
  Query, Expect, vList: string;
const
  Json = '[null,42,true,"key"]';
begin
  if Root.Instance.TryParse(Json) then begin
    Query := '$[?@==param("parValue")]';
    if JpParseQuery(Query, Path) then begin
      Expect := '[]';
      vList := Path.MatchValues(Root.Instance).AsJson;// 0
      if vList <> Expect then
        ErrList.Instance.Add(Format(ExpectFmt, [0, Expect, vList]));

      Path.Params['parValue'] := TJpValue.Nothing;
      vList := Path.MatchValues(Root.Instance).AsJson;// 1
      if vList <> Expect then
        ErrList.Instance.Add(Format(ExpectFmt, [1, Expect, vList]));

      Path.Params['parValue'] := TJpValue.NullValue;
      Expect := '[null]';
      vList := Path.MatchValues(Root.Instance).AsJson;// 2
      if vList <> Expect then
        ErrList.Instance.Add(Format(ExpectFmt, [2, Expect, vList]));

      Path.Params['parValue'] := TJpValue.TrueValue;
      Expect := '[true]';
      vList := Path.MatchValues(Root.Instance).AsJson;// 3
      if vList <> Expect then
        ErrList.Instance.Add(Format(ExpectFmt, [3, Expect, vList]));

      Path.Params['parValue'] := 42;
      Expect := '[42]';
      vList := Path.MatchValues(Root.Instance).AsJson;// 4
      if vList <> Expect then
        ErrList.Instance.Add(Format(ExpectFmt, [4, Expect, vList]));

      Path.Params['parValue'] := 'key';
      Expect := '["key"]';
      vList := Path.MatchValues(Root.Instance).AsJson;// 5
      if vList <> Expect then
        ErrList.Instance.Add(Format(ExpectFmt, [5, Expect, vList]));
    end else
      ErrList.Instance.Add('Invalid query');
  end else
    ErrList.Instance.Add('Invalid JSON');

  AssertTrue(ErrList.Instance.Text, ErrList.Instance.Count = 0);
end;

procedure TTestAuxFun.TestIsBoolean;
var
  ErrList: TAutoStrList;
const
  Tests: TTestSet = (
    (Value: '["a", 42, null]';         Query: '$[?is_boolean(@)]';     Expected: '[]'),
    (Value: '["a", 42, null, true]';   Query: '$[?is_boolean(@)]';     Expected: '[true]'),
    (Value: '[null, false, [2], {}]';  Query: '$[?is_boolean(@)]';     Expected: '[false]'),
    (Value: '[{"a":null},{"a":42}]';   Query: '$[?@[?is_boolean(@)]]'; Expected: '[]'),
    (Value: '[{"a":null},{"a":true}]'; Query: '$[?@[?is_boolean(@)]]'; Expected: '[{"a":true}]'),
    (Value: '[[],[42,"a"],[0,null]]';  Query: '$[?@[?is_boolean(@)]]'; Expected: '[]'),
    (Value: '[[],[42,"a"],[0,true]]';  Query: '$[?@[?is_boolean(@)]]'; Expected: '[[0,true]]')
  );
begin
  RunTestSet(Tests, ErrList.Instance);
  AssertTrue(ErrList.Instance.Text, ErrList.Instance.Count = 0);
end;

procedure TTestAuxFun.TestIsNumber;
var
  ErrList: TAutoStrList;
const
  Tests: TTestSet = (
    (Value: '["a", false, null]';       Query: '$[?is_number(@)]';     Expected: '[]'),
    (Value: '["a", 42, null, true]';    Query: '$[?is_number(@)]';     Expected: '[42]'),
    (Value: '[null, 2E25, [2], {}]';    Query: '$[?is_number(@)]';     Expected: '[2E25]'),
    (Value: '[{"a":null},{"a":"b"}]';   Query: '$[?@[?is_number(@)]]'; Expected: '[]'),
    (Value: '[{"a":"b"},{"a":27.456}]'; Query: '$[?@[?is_number(@)]]'; Expected: '[{"a":27.456}]'),
    (Value: '[[],[[],"a"],["",null]]';  Query: '$[?@[?is_number(@)]]'; Expected: '[]'),
    (Value: '[[],[-0.03,"a"],[true]]';  Query: '$[?@[?is_number(@)]]'; Expected: '[[-0.03,"a"]]')
  );
begin
  RunTestSet(Tests, ErrList.Instance);
  AssertTrue(ErrList.Instance.Text, ErrList.Instance.Count = 0);
end;

procedure TTestAuxFun.TestIsInteger;
var
  ErrList: TAutoStrList;
const
  Tests: TTestSet = (
    (Value: '["a", false, null]';           Query: '$[?is_integer(@)]';     Expected: '[]'),
    (Value: '["a", 42.001, null, true]';    Query: '$[?is_integer(@)]';     Expected: '[]'),
    (Value: '["a", 42, null, true]';        Query: '$[?is_integer(@)]';     Expected: '[42]'),
    (Value: '[null, 2.0E15, [2], {}]';      Query: '$[?is_integer(@)]';     Expected: '[2000000000000000]'),
    (Value: '[null, -2.0E15, [2], {}]';     Query: '$[?is_integer(@)]';     Expected: '[-2000000000000000]'),
    (Value: '[9007199254740992,0.14]';      Query: '$[?is_integer(@)]';     Expected: '[]'),
    (Value: '[-9007199254740992,3.72]';     Query: '$[?is_integer(@)]';     Expected: '[]'),
    (Value: '[9007199254740991.00,23.77]';  Query: '$[?is_integer(@)]';     Expected: '[9007199254740991]'),
    (Value: '[-9007199254740991.00,13.22]'; Query: '$[?is_integer(@)]';     Expected: '[-9007199254740991]'),
    (Value: '[[],[2.111,"a"],["",33.3E0]]'; Query: '$[?@[?is_integer(@)]]'; Expected: '[]'),
    (Value: '[[],[2.111,"a"],["",33.3E1]]'; Query: '$[?@[?is_integer(@)]]'; Expected: '[["",333]]'),
    (Value: '[{"a":"b"},{"a":27.1330E3}]';  Query: '$[?@[?is_integer(@)]]'; Expected: '[{"a":27133}]')
  );
begin
  RunTestSet(Tests, ErrList.Instance);
  AssertTrue(ErrList.Instance.Text, ErrList.Instance.Count = 0);
end;

procedure TTestAuxFun.TestIsString;
var
  ErrList: TAutoStrList;
const
  Tests: TTestSet = (
    (Value: '[42, false, null]';        Query: '$[?is_string(@)]';     Expected: '[]'),
    (Value: '["a", 42, null, true]';    Query: '$[?is_string(@)]';     Expected: '["a"]'),
    (Value: '[null, "", [2], {}]';      Query: '$[?is_string(@)]';     Expected: '[""]'),
    (Value: '[{"a":null},{"a":42}]';    Query: '$[?@[?is_string(@)]]'; Expected: '[]'),
    (Value: '[{"a":"b"},{"a":27.456}]'; Query: '$[?@[?is_string(@)]]'; Expected: '[{"a":"b"}]'),
    (Value: '[[],[[],12],[0,null]]';    Query: '$[?@[?is_string(@)]]'; Expected: '[]'),
    (Value: '[[],[[],"a"],[12,null]]';  Query: '$[?@[?is_string(@)]]'; Expected: '[[[],"a"]]')
  );
begin
  RunTestSet(Tests, ErrList.Instance);
  AssertTrue(ErrList.Instance.Text, ErrList.Instance.Count = 0);
end;

procedure TTestAuxFun.TestIsArray;
var
  ErrList: TAutoStrList;
const
  Tests: TTestSet = (
    (Value: '[42, false, {}]';         Query: '$[?is_array(@)]';     Expected: '[]'),
    (Value: '[[],42,true,"a"]';        Query: '$[?is_array(@)]';     Expected: '[[]]'),
    (Value: '[null, "", [{}], {}]';    Query: '$[?is_array(@)]';     Expected: '[[{}]]'),
    (Value: '[{"a":null},{"a":42}]';   Query: '$[?@[?is_array(@)]]'; Expected: '[]'),
    (Value: '[{"a":"b"},{"a":[]}]';    Query: '$[?@[?is_array(@)]]'; Expected: '[{"a":[]}]'),
    (Value: '[{},[{},12],[0,null]]';   Query: '$[?@[?is_array(@)]]'; Expected: '[]'),
    (Value: '[{},[[],"a"],[12,null]]'; Query: '$[?@[?is_array(@)]]'; Expected: '[[[],"a"]]')
  );
begin
  RunTestSet(Tests, ErrList.Instance);
  AssertTrue(ErrList.Instance.Text, ErrList.Instance.Count = 0);
end;

procedure TTestAuxFun.TestIsObject;
var
  ErrList: TAutoStrList;
const
  Tests: TTestSet = (
    (Value: '[42, false, []]';         Query: '$[?is_object(@)]';     Expected: '[]'),
    (Value: '[{},42,true,"a"]';        Query: '$[?is_object(@)]';     Expected: '[{}]'),
    (Value: '[null, [{}], {"a":0}]';   Query: '$[?is_object(@)]';     Expected: '[{"a":0}]'),
    (Value: '[{"a":null},{"a":42}]';   Query: '$[?@[?is_object(@)]]'; Expected: '[]'),
    (Value: '[{"a":"b"},{"a":{}}]';    Query: '$[?@[?is_object(@)]]'; Expected: '[{"a":{}}]'),
    (Value: '[{},[[],12],[0,null]]';   Query: '$[?@[?is_object(@)]]'; Expected: '[]'),
    (Value: '[{},[{},"a"],[12,null]]'; Query: '$[?@[?is_object(@)]]'; Expected: '[[{},"a"]]')
  );
begin
  RunTestSet(Tests, ErrList.Instance);
  AssertTrue(ErrList.Instance.Text, ErrList.Instance.Count = 0);
end;

procedure TTestAuxFun.TestIsStruct;
var
  ErrList: TAutoStrList;
const
  Tests: TTestSet = (
    (Value: '[42, false, []]';         Query: '$[?is_struct(@)]';     Expected: '[[]]'),
    (Value: '[{},42,true,"a"]';        Query: '$[?is_struct(@)]';     Expected: '[{}]'),
    (Value: '[null, [{}], {"a":0}]';   Query: '$[?is_struct(@)]';     Expected: '[[{}],{"a":0}]'),
    (Value: '[{"a":null},{"a":42}]';   Query: '$[?@[?is_struct(@)]]'; Expected: '[]'),
    (Value: '[{"a":"b"},{"a":{}}]';    Query: '$[?@[?is_struct(@)]]'; Expected: '[{"a":{}}]'),
    (Value: '[{},[[],12],[0,null]]';   Query: '$[?@[?is_struct(@)]]'; Expected: '[[[],12]]'),
    (Value: '[{},[{},"a"],[12,null]]'; Query: '$[?@[?is_struct(@)]]'; Expected: '[[{},"a"]]')
  );
begin
  RunTestSet(Tests, ErrList.Instance);
  AssertTrue(ErrList.Instance.Text, ErrList.Instance.Count = 0);
end;

procedure TTestAuxFun.TestContains;
var
  ErrList: TAutoStrList;
const
  Tests: TTestSet = (
    (Value: '[42, false, "dnaz"]';           Query: '$[?contains(@, "an")]';         Expected: '[]'),
    (Value: '["in", "one", "nand"]';         Query: '$[?contains(@, "an")]';         Expected: '["nand"]'),
    (Value: '{"один":[42],"два":[0]}';       Query: '$..[?contains(key(),"ди")]';    Expected: '[[42]]'),
    (Value: '[{"код":"000"},{"код":"010"}]'; Query: '$[?@[?contains(@, "10")]].код'; Expected: '["010"]')
  );
begin
  RunTestSet(Tests, ErrList.Instance);
  AssertTrue(ErrList.Instance.Text, ErrList.Instance.Count = 0);
end;

procedure TTestAuxFun.TestContainsText;
var
  ErrList: TAutoStrList;
const
  Tests: TTestSet = (
    (Value: '[42, false, "dnaz"]';           Query: '$[?contains_text(@, "an")]';         Expected: '[]'),
    (Value: '["in", "one", "nand"]';         Query: '$[?contains_text(@, "An")]';         Expected: '["nand"]'),
    (Value: '{"один":[42],"два":[0]}';       Query: '$..[?contains_text(key(),"ДИ")]';    Expected: '[[42]]'),
    (Value: '[{"код":"000"},{"код":"010"}]'; Query: '$[?@[?contains_text(@, "10")]].код'; Expected: '["010"]')
  );
begin
  RunTestSet(Tests, ErrList.Instance);
  AssertTrue(ErrList.Instance.Text, ErrList.Instance.Count = 0);
end;

procedure TTestAuxFun.TestHasWord;
var
  ErrList: TAutoStrList;
const
  Tests: TTestSet = (
    (Value: '[42, "sfoo", "fooz"]';       Query: '$[?has_word(@, "foo")]';     Expected: '[]'),
    (Value: '["barbar", "i bar", "bar"]'; Query: '$[?has_word(@, "bar")]';     Expected: '["i bar","bar"]'),
    (Value: '["in", "fin", "ino"]';       Query: '$[?has_word(@, "in")]';      Expected: '["in"]'),
    (Value: '{"из-под":[42],"низ":[0]}';  Query: '$..[?has_word(key(),"из")]'; Expected: '[[42]]'),
    (Value: '["кода","шкод","1-код-2"]';  Query: '$[?has_word(@, "код")]';     Expected: '["1-код-2"]')
  );
begin
  RunTestSet(Tests, ErrList.Instance);
  AssertTrue(ErrList.Instance.Text, ErrList.Instance.Count = 0);
end;

procedure TTestAuxFun.TestHasWordCi;
var
  ErrList: TAutoStrList;
const
  Tests: TTestSet = (
    (Value: '[42, "sfoo", "fooz"]';       Query: '$[?has_word_ci(@, "FOO")]';     Expected: '[]'),
    (Value: '["barbar", "i bar", "bar"]'; Query: '$[?has_word_ci(@, "BAR")]';     Expected: '["i bar","bar"]'),
    (Value: '["in", "fin", "ino"]';       Query: '$[?has_word_ci(@, "IN")]';      Expected: '["in"]'),
    (Value: '{"из-под":[42],"низ":[0]}';  Query: '$..[?has_word_ci(key(),"ИЗ")]'; Expected: '[[42]]'),
    (Value: '["кода","шкод","1-код-2"]';  Query: '$[?has_word_ci(@, "КОД")]';     Expected: '["1-код-2"]')
  );
begin
  RunTestSet(Tests, ErrList.Instance);
  AssertTrue(ErrList.Instance.Text, ErrList.Instance.Count = 0);
end;

procedure TTestAuxFun.TestSameText;
var
  ErrList: TAutoStrList;
const
  Tests: TTestSet = (
    (Value: '[42, false, "dnaz"]';           Query: '$[?same_text(@, "dna")]';         Expected: '[]'),
    (Value: '["in", "one", "nand"]';         Query: '$[?same_text(@, "nAnD")]';        Expected: '["nand"]'),
    (Value: '{"один":[42],"два":[0]}';       Query: '$..[?same_text(key(),"оДИн")]';   Expected: '[[42]]'),
    (Value: '[{"код":"000"},{"код":"010"}]'; Query: '$[?@[?same_text(@, "010")]].код'; Expected: '["010"]')
  );
begin
  RunTestSet(Tests, ErrList.Instance);
  AssertTrue(ErrList.Instance.Text, ErrList.Instance.Count = 0);
end;

procedure TTestAuxFun.TestStartsWith;
var
  ErrList: TAutoStrList;
const
  Tests: TTestSet = (
    (Value: '[42, false, "value"]';          Query: '$[?starts_with(@, "vo")]';         Expected: '[]'),
    (Value: '["tar", "sto", "topic"]';       Query: '$[?starts_with(@, "to")]';         Expected: '["topic"]'),
    (Value: '{"один":[42],"два":[0]}';       Query: '$..[?starts_with(key(),"дв")]';    Expected: '[[0]]'),
    (Value: '[{"код":"000"},{"код":"010"}]'; Query: '$[?@[?starts_with(@, "01")]].код'; Expected: '["010"]')
  );
begin
  RunTestSet(Tests, ErrList.Instance);
  AssertTrue(ErrList.Instance.Text, ErrList.Instance.Count = 0);
end;

procedure TTestAuxFun.TestEndsWith;
var
  ErrList: TAutoStrList;
const
  Tests: TTestSet = (
    (Value: '[42, false, "value"]';          Query: '$[?ends_with(@, "un")]';         Expected: '[]'),
    (Value: '["osp", "sto", "first"]';       Query: '$[?ends_with(@, "st")]';         Expected: '["first"]'),
    (Value: '{"один":[42],"два":[0]}';       Query: '$..[?ends_with(key(),"ин")]';    Expected: '[[42]]'),
    (Value: '[{"код":"000"},{"код":"010"}]'; Query: '$[?@[?ends_with(@, "00")]].код'; Expected: '["000"]')
  );
begin
  RunTestSet(Tests, ErrList.Instance);
  AssertTrue(ErrList.Instance.Text, ErrList.Instance.Count = 0);
end;

procedure TTestAuxFun.TestToUpper;
var
  ErrList: TAutoStrList;
const
  Tests: TTestSet = (
    (Value: '[42, false, "value"]';          Query: '$[?to_upper(@)=="FIRST"]';       Expected: '[]'),
    (Value: '["firsp", "forst", "first"]';   Query: '$[?to_upper(@)== "FIRST"]';      Expected: '["first"]'),
    (Value: '{"один":[42],"два":[0]}';       Query: '$..[?to_upper(key())=="ОДИН"]';  Expected: '[[42]]'),
    (Value: '[{"код":"000"},{"код":"010"}]'; Query: '$[?@[?to_upper(@)=="000"]].код'; Expected: '["000"]')
  );
begin
  RunTestSet(Tests, ErrList.Instance);
  AssertTrue(ErrList.Instance.Text, ErrList.Instance.Count = 0);
end;

procedure TTestAuxFun.TestToLower;
var
  ErrList: TAutoStrList;
const
  Tests: TTestSet = (
    (Value: '[42, false, "value"]';          Query: '$[?to_lower(@)=="first"]';       Expected: '[]'),
    (Value: '["Firsp", "Forst", "First"]';   Query: '$[?to_lower(@)== "first"]';      Expected: '["First"]'),
    (Value: '{"Один":[42],"Два":[0]}';       Query: '$..[?to_lower(key())=="один"]';  Expected: '[[42]]'),
    (Value: '[{"код":"000"},{"код":"010"}]'; Query: '$[?@[?to_lower(@)=="010"]].код'; Expected: '["010"]')
  );
begin
  RunTestSet(Tests, ErrList.Instance);
  AssertTrue(ErrList.Instance.Text, ErrList.Instance.Count = 0);
end;

procedure TTestAuxFun.TestConcat;
var
  ErrList: TAutoStrList;
const
  Tests: TTestSet = (
    (
      Value:
        '[' +
          '{"a": null, "b": "aabb","c": "aabb"},' +
          '{"a": "aabb", "b": false, "c": "aabb"},' +
          '{"a": "aabb", "d": "", "c": "aabb"},' +
          '{"a": "a", "b": "abb", "d": "aabb"},' +
          '{"a": "aab", "b": "ba", "c": "aabb"},' +
          '{"a": "aa", "b": "bb", "c": "aabb"}' +
        ']';
      Query: '$[?concat(@.a, @.b) == @.c].a';
      Expected: '["aa"]'
    )
  );
begin
  RunTestSet(Tests, ErrList.Instance);
  AssertTrue(ErrList.Instance.Text, ErrList.Instance.Count = 0);
end;

procedure TTestAuxFun.TestMin;
var
  ErrList: TAutoStrList;
const
  Tests: TTestSet = (
    (
      Value:
        '[' +
          '{"a": 1, "b": null,"c": 1},' +
          '{"a": 2, "b": "a", "c": 2},' +
          '{"a": 3, "d": 42, "c": 3},' +
          '{"a": 4, "b": 0, "c": 4},' +
          '{"a": 5, "b": 42, "c": 5},' +
          '{"a": 6, "b": "a", "c": "a"},' +
          '{"a": 6, "b": "a", "c": 6},' +
          '{"a": "a", "b": "b", "c": "b"},' +
          '{"a": "a", "b": "b", "c": "a"}' +
        ']';
      Query: '$[?min(@.a, @.b) == @.c].c';
      Expected: '[5,"a"]'
    )
  );
begin
  RunTestSet(Tests, ErrList.Instance);
  AssertTrue(ErrList.Instance.Text, ErrList.Instance.Count = 0);
end;

procedure TTestAuxFun.TestMax;
var
  ErrList: TAutoStrList;
const
  Tests: TTestSet = (
    (
      Value:
        '[' +
          '{"a": 1, "b": null,"c": 1},' +
          '{"a": 2, "b": "a", "c": 2},' +
          '{"a": 3, "d": 42, "c": 3},' +
          '{"a": 4, "b": 42, "c": 42},' +
          '{"a": 5, "b": 42, "c": 5},' +
          '{"a": 6, "b": "a", "c": "a"},' +
          '{"a": 6, "b": "a", "c": 6},' +
          '{"a": "a", "b": "b", "c": "b"},' +
          '{"a": "a", "b": "b", "c": "a"}' +
        ']';
      Query: '$[?max(@.a, @.b) == @.c].c';
      Expected: '[42,"b"]'
    )
  );
begin
  RunTestSet(Tests, ErrList.Instance);
  AssertTrue(ErrList.Instance.Text, ErrList.Instance.Count = 0);
end;

procedure TTestAuxFun.TestAbs;
var
  ErrList: TAutoStrList;
const
  Tests: TTestSet = (
    (
      Value:
        '[' +
          '{"a": null,  "b": 42},' +
          '{"a": false, "b": 42},' +
          '{"a": "a", "b": 42},' +
          '{"a": [], "b": 42},' +
          '{"a": {}, "b": 42},' +
          '{"a": 42.0, "b": 42},' +
          '{"a": -42.0, "b": 42}' +
        ']';
      Query: '$[?abs(@.a) == @.b].a';
      Expected: '[42,-42]'
    )
  );
begin
  RunTestSet(Tests, ErrList.Instance);
  AssertTrue(ErrList.Instance.Text, ErrList.Instance.Count = 0);
end;

procedure TTestAuxFun.TestSum;
var
  ErrList: TAutoStrList;
const
  Tests: TTestSet = (
    (
      Value:
        '[' +
          '{"a": 1, "b": null,"c": 1},' +
          '{"a": "a", "b": 2, "c": 2},' +
          '{"a": 3, "d": 42, "c": 45},' +
          '{"a": "a", "b": "b", "c": "ab"},' +
          '{"a": 5, "b": 42, "c": 46},' +
          '{"a": 6.0, "b": 42.0, "c": 48},' +
          '{"a": 7.5, "b": 8.5, "c": 16}' +
        ']';
      Query: '$[?sum(@.a, @.b) == @.c].c';
      Expected: '[48,16]'
    )
  );
begin
  RunTestSet(Tests, ErrList.Instance);
  AssertTrue(ErrList.Instance.Text, ErrList.Instance.Count = 0);
end;

procedure TTestAuxFun.TestDiff;
var
  ErrList: TAutoStrList;
const
  Tests: TTestSet = (
    (
      Value:
        '[' +
          '{"a": 1, "b": null,"c": 1},' +
          '{"a": 2, "b": "a", "c": 2},' +
          '{"a": 3, "d": 42, "c": -39},' +
          '{"a": "b", "b": "a", "c": 1},' +
          '{"a": 42, "b": 5, "c": 38},' +
          '{"a": 42.0, "b": 6.0, "c": 36},' +
          '{"a": 7, "b": 42, "c": -35},' +
          '{"a": 9.5, "b": 6.5, "c": 3}' +
        ']';
      Query: '$[?diff(@.a, @.b) == @.c].c';
      Expected: '[36,-35,3]'
    )
  );
begin
  RunTestSet(Tests, ErrList.Instance);
  AssertTrue(ErrList.Instance.Text, ErrList.Instance.Count = 0);
end;

procedure TTestAuxFun.TestProd;
var
  ErrList: TAutoStrList;
const
  Tests: TTestSet = (
    (
      Value:
        '[' +
          '{"a": 1, "b": null,"c": 0},' +
          '{"a": 2, "b": "a", "c": "a"},' +
          '{"a": 3, "d": 2, "c": 6},' +
          '{"a": "b", "b": "a", "c": "ab"},' +
          '{"a": 2, "b": 5, "c": 11},' +
          '{"a": 2.0, "b": 6.0, "c": 12},' +
          '{"a": 7, "b": -2, "c": -14},' +
          '{"a": 3.5, "b": 2.5, "c": 8.75}' +
        ']';
      Query: '$[?prod(@.a, @.b) == @.c].c';
      Expected: '[12,-14,8.75]'
    )
  );
begin
  RunTestSet(Tests, ErrList.Instance);
  AssertTrue(ErrList.Instance.Text, ErrList.Instance.Count = 0);
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
    TestDir := Dir;
end;

initialization

  FindTestDir;
  RegisterTest(TTestJsonPath);
  RegisterTest(TTestAuxFun);

end.

