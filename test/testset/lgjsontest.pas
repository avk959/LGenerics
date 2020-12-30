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
    procedure Parser;
    procedure Validator;
    procedure Parse;
    procedure JsonPointer;
    procedure JsonPointer1;
    procedure AddUniq;
    procedure Values;
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

procedure TTestJson.Parser;
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

procedure TTestJson.Validator;
var
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
      Result := TJsonNode.ValidJson(Stream.Instance.DataString);
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

procedure TTestJson.Parse;
const
  Json =
    '[{"userid":42,"name":"John","age":30,"online":false,"groups":["talk","humor","cook"],' +
    '"spouse":null},' +
    '{"userid":1001,"name":"Thomas","age":42,"online":true,"groups":["talk","games","math","art"],' +
    '"spouse":"Mary"}]';
var
  o: specialize TGAutoRef<TJsonNode>;
begin
  AssertTrue(o.Instance.Parse(Json));
  AssertTrue(o.Instance.AsJson = Json);
  AssertTrue(o.Instance.IsArray);
  AssertTrue(o.Instance.Count = 2);
  AssertTrue(o.Instance.Items[0].IsObject);
  AssertTrue(o.Instance.Items[0].Count = 6);
  AssertTrue(o.Instance.Items[0].Contains('online'));
  AssertFalse(o.Instance.Items[0].Values['online']);
  AssertTrue(o.Instance.Items[0].Values['age'] = 30);
  AssertTrue(o.Instance.Items[0].Items[4].Count = 3);
  AssertTrue(o.Instance.Items[1].IsObject);
  AssertTrue(o.Instance.Items[1].Contains('online'));
  AssertTrue(o.Instance.Items[1].Values['online']);
  AssertTrue(o.Instance.Items[1].Values['age'] = 42);
  AssertTrue(o.Instance.Items[1].Count = 6);
  AssertTrue(o.Instance.Items[1].Items[4].Count = 4);
end;

procedure TTestJson.JsonPointer;
var
  o: specialize TGAutoRef<TJsonNode>;
  Node: TJsonNode;
begin
  AssertTrue(o.Instance.Parse(
   '{ '+
   '  "foo": ["bar", "baz"], '+
   '  "":     0, '+
   '  "a/b":  1, '+
   '  "c%d":  2, '+
   '  "e^f":  3, '+
   '  "g|h":  4, '+
   '  "i\\j": 5, '+
   '  "k\"l": 6, '+
   '  " ":    7, '+
   '  "m~n":  8  '+
   '} '));
  AssertTrue(o.Instance.FindPath('', Node));
  AssertTrue(Node = o.Instance);
  AssertTrue(o.Instance.FindPath('/foo', Node));
  AssertTrue(Node.IsArray);
  AssertTrue(Node.Count = 2);
  AssertTrue(Node.Items[0].AsString = 'bar');
  AssertTrue(Node.Items[1].AsString = 'baz');
  AssertTrue(o.Instance.FindPath('/foo/0', Node));
  AssertTrue(Node.IsString);
  AssertTrue(Node.AsString = 'bar');
  AssertTrue(o.Instance.FindPath('/', Node));
  AssertTrue(Node.IsNumber);
  AssertTrue(Node.AsNumber = 0);
  AssertTrue(o.Instance.FindPath(TJsonNode.JsonPtrEncode(['a/b']), Node));
  AssertTrue(Node.IsNumber);
  AssertTrue(Node.AsNumber = 1);
  AssertTrue(o.Instance.FindPath('/c%d', Node));
  AssertTrue(Node.AsNumber = 2);
  AssertTrue(o.Instance.FindPath('/e^f', Node));
  AssertTrue(Node.AsNumber = 3);
  AssertTrue(o.Instance.FindPath('/g|h', Node));
  AssertTrue(Node.AsNumber = 4);
  AssertTrue(o.Instance.FindPath('/i\j', Node));
  AssertTrue(Node.AsNumber = 5);
  AssertTrue(o.Instance.FindPath('/k"l', Node));
  AssertTrue(Node.AsNumber = 6);
  AssertTrue(o.Instance.FindPath('/ ', Node));
  AssertTrue(Node.AsNumber = 7);
  AssertTrue(o.Instance.FindPath(TJsonNode.JsonPtrEncode(['m~n']), Node));
  AssertTrue(Node.AsNumber = 8);
end;

procedure TTestJson.JsonPointer1;
var
  o: specialize TGAutoRef<TJsonNode>;
  Node: TJsonNode;
begin
  o.Instance.AsJson := '["bar", "baz"]';
  AssertTrue(o.Instance.IsArray);
  AssertTrue(o.Instance.Count = 2);
  AssertTrue(o.Instance.FindPath('/-', Node));
  AssertTrue(Node.IsNull);
  Node.AsString := 'foo';
  AssertTrue(o.Instance.Count.ToString, o.Instance.Count = 3);
  AssertTrue(o.Instance.Items[2].AsString = 'foo');
end;

procedure TTestJson.AddUniq;
var
  o: specialize TGAutoRef<TJsonNode>;
  Node: TJsonNode;
begin
  AssertTrue(o.Instance.AddUniq('key', False));
  AssertTrue(o.Instance.Find('key', Node));
  AssertTrue(o.Instance.Count = 1);
  AssertFalse(o.Instance.AddUniq('key', 'data'));
  AssertTrue(o.Instance.Count = 1);
  AssertTrue(o.Instance.AddUniq('new key', 42));
  AssertTrue(o.Instance.Count = 2);
  AssertTrue(o.Instance.Find('new key', Node));
end;

procedure TTestJson.Values;
var
  o: specialize TGAutoRef<TJsonNode>;
  User: TJsonNode;
begin
  User := o.Instance.AddNode(jvkObject);
  User['firstName'] := 'John';
  User['lastName'] := 'Smith';
  User['age'] := 42;
  User['married'] := False;
  User['spouse'] := JNull;
  AssertTrue(o.Instance.Count = 1);
  User := o.Instance.Items[0];
  AssertTrue(User.IsObject);
  AssertTrue(User['firstName'] = 'John');
  AssertTrue(User['lastName'] = 'Smith');
  AssertTrue(User['age'] = 42);
  AssertFalse(User['married']);
  AssertTrue(User.Named['spouse'].IsNull);
end;

initialization

  LoadFileList;
  RegisterTest(TTestJson);

finalization

  TestFileList.Free;

end.
