unit LGJsonUtilsTest;

{$MODE OBJFPC}{$H+}
{$WARN 6058 OFF : Call to subroutine "$1" marked as inline is not inlined}

interface

uses
  Classes, SysUtils, FileUtil, fpcunit, testutils, testregistry,
  LgUtils,
  LgJson,
  LgJsonUtils;

type

  { TTestMergePatch }

  TTestMergePatch= class(TTestCase)
  private
  const
    NotFoundFmt = 'Test file "%s" not found';
    PropMissFmt = '"%s" property is missing';
  published
    procedure ParchTest;
    procedure DiffTest;
  end;

implementation

var
  TestFileName: string = '';

procedure TTestMergePatch.ParchTest;
var
  Tests, CurrTest, Doc, Patch, Expect, TstName: TJsonNode;
  TestSet, Ans: specialize TGAutoRef<TJsonNode>;
  s, p: string;
const
  FailFmt = 'Patch test "%s" failed(%d)';
  RetFmt  = 'Patch test "%s": ApplyPatch() return False(%d)';
begin
  AssertTrue(Format(NotFoundFmt, [TestFileName]), FileExists(TestFileName));
  AssertTrue('Invalid test JSON', TestSet.Instance.TryParseFile(TestFileName));
  AssertTrue('Patch tests not found', TestSet.Instance.Find('Patch_Tests', Tests));
  AssertTrue('Test JSON is not an array', Tests.IsArray);
  AssertTrue('Test JSON is empty', Tests.Count <> 0);
  for CurrTest in Tests do
    begin
      AssertTrue(Format(PropMissFmt, ['name']), CurrTest.Find('name', TstName));
      AssertTrue(Format(PropMissFmt, ['document']), CurrTest.Find('document', Doc));
      AssertTrue(Format(PropMissFmt, ['patch']), CurrTest.Find('patch', Patch));
      AssertTrue(Format(PropMissFmt, ['expected']), CurrTest.Find('expected', Expect));
      //1 (TJsonNode, TJsonNode)
      Ans.Instance.CopyFrom(Doc);
      TJsonMergePatch.ApplyPatch(Patch, Ans.Instance);
      AssertTrue(Format(FailFmt, [TstName.ToString, 1]), Expect.EqualTo(Ans.Instance));
      //2 (TJsonNode, string)
      s := Doc.AsJson;
      AssertTrue(Format(RetFmt, [TstName.ToString, 2]), TJsonMergePatch.ApplyPatch(Patch, s));
      Ans.Instance.AsJson := s;
      AssertTrue(Format(FailFmt, [TstName.ToString, 2]), Expect.EqualTo(Ans.Instance));
      //3 (string, TJsonNode)
      p := Patch.AsJson;
      Ans.Instance.CopyFrom(Doc);
      AssertTrue(Format(RetFmt, [TstName.ToString, 3]), TJsonMergePatch.ApplyPatch(p, Ans.Instance));
      AssertTrue(Format(FailFmt, [TstName.ToString, 3]), Expect.EqualTo(Ans.Instance));
      //4 (string, string);
      s := Doc.AsJson;
      AssertTrue(Format(RetFmt, [TstName.ToString, 4]), TJsonMergePatch.ApplyPatch(p, s));
      Ans.Instance.AsJson := s;
      AssertTrue(Format(FailFmt, [TstName.ToString, 4]), Expect.EqualTo(Ans.Instance));
    end;
end;

procedure TTestMergePatch.DiffTest;
var
  Tests, CurrTest, Src, Trg, Expect, TstName, Tmp: TJsonNode;
  TestSet, Patch: specialize TGAutoRef<TJsonNode>;
  MpTmp: TJsonMergePatch;
  MPatch: specialize TGUniqRef<TJsonMergePatch>;
  p: string;
const
  FailFmt = 'Diff test "%s" failed(%d)';
  RetFmt  = 'Diff test "%s": Diff...() return False(%d)';
begin
  AssertTrue(Format(NotFoundFmt, [TestFileName]), FileExists(TestFileName));
  AssertTrue('Invalid test JSON', TestSet.Instance.TryParseFile(TestFileName));
  AssertTrue('Diff tests not found', TestSet.Instance.Find('Diff_Tests', Tests));
  AssertTrue('Test JSON is not an array', Tests.IsArray);
  AssertTrue('Test JSON is empty', Tests.Count <> 0);
  for CurrTest in Tests do
    begin
      AssertTrue(Format(PropMissFmt, ['name']), CurrTest.Find('name', TstName));
      AssertTrue(Format(PropMissFmt, ['source']), CurrTest.Find('source', Src));
      AssertTrue(Format(PropMissFmt, ['target']), CurrTest.Find('target', Trg));
      AssertTrue(Format(PropMissFmt, ['expected']), CurrTest.Find('expected', Expect));
      // 1 DiffNode(TJsonNode, TJsonNode)
      Patch.Instance := TJsonMergePatch.DiffNode(Src, Trg);
      AssertTrue(Format(FailFmt, [TstName.ToString, 1]), Expect.EqualTo(Patch.Instance));
      //2  Diff(TJsonNode, TJsonNode)
      MPatch.Instance := TJsonMergePatch.Diff(Src, Trg);
      Patch.Instance.AsJson := MPatch.Instance.AsJson;
      AssertTrue(Format(FailFmt, [TstName.ToString, 2]), Expect.EqualTo(Patch.Instance));
      //3  Diff(string, string, TJsonNode)
      AssertTrue(Format(RetFmt, [TstName.ToString, 3]), TJsonMergePatch.Diff(Src.AsJson, Trg.AsJson, Tmp));
      Patch.Instance := Tmp;
      AssertTrue(Format(FailFmt, [TstName.ToString, 3]), Expect.EqualTo(Patch.Instance));
      //4  Diff(string, string, TJsonMergePatch)
      AssertTrue(Format(RetFmt, [TstName.ToString, 4]), TJsonMergePatch.Diff(Src.AsJson, Trg.AsJson, MpTmp));
      MPatch.Instance := MpTmp;
      Patch.Instance.AsJson := MpTmp.AsJson;
      AssertTrue(Format(FailFmt, [TstName.ToString, 4]), Expect.EqualTo(Patch.Instance));
      //5  DiffJson(TJsonNode, TJsonNode)
      Patch.Instance.AsJson := TJsonMergePatch.DiffJson(Src, Trg);
      AssertTrue(Format(FailFmt, [TstName.ToString, 5]), Expect.EqualTo(Patch.Instance));
      //6  DiffJson(string, string, string)
      AssertTrue(Format(RetFmt, [TstName.ToString, 6]), TJsonMergePatch.DiffJson(Src.AsJson, Trg.AsJson, p));
      Patch.Instance.AsJson := p;
      AssertTrue(Format(FailFmt, [TstName.ToString, 6]), Expect.EqualTo(Patch.Instance));
    end;
end;

procedure FindTestSet;
var
  fn: string;
const
  ds = DirectorySeparator;
begin
  fn := ExcludeTrailingPathDelimiter(ProgramDirectory);
  while (fn <> '') and (ExtractFileName(fn) <> 'test') do
    fn := ExcludeTrailingPathDelimiter(ExtractFilePath(fn));
  if fn = '' then exit;
  fn := fn + ds + 'json_testset' + ds + 'merge_patch' + ds + 'tests.json';
  if FileExists(fn) then
    TestFileName := fn;
end;

initialization

  FindTestSet;
  RegisterTest(TTestMergePatch);

end.

