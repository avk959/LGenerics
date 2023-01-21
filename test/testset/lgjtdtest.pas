unit lgJtdTest;

{$MODE OBJFPC}{$H+}{$WARN 5089 OFF}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, FileUtil,
  lgUtils, lgJson, lgJsonTypeDef;

type

  { TJtdUtilsTest }

  TJtdUtilsTest = class(TTestCase)
  protected
  published
    procedure IsRfc8927TimeStamp_InvalidYear;
    procedure IsRfc8927TimeStamp_InvalidMonth;
    procedure IsRfc8927TimeStamp_InvalidDay;
    procedure IsRfc8927TimeStamp_InvalidHour;
    procedure IsRfc8927TimeStamp_InvalidMinute;
    procedure IsRfc8927TimeStamp_InvalidSeconde;
  end;

  { TJtdTest }

  TJtdTest = class(TTestCase)
  protected
  const
    TEST_DIR        = 'json_testset';
    SPEC_DIR        = 'jtd_spec';
    INVALID_SCHEMAS = 'invalid_schemas.json';
    VALIDATION_SET  = 'validation.json';
  var
    TestDir: string;
    procedure SetUp; override;
    procedure NormalizeErrors(aNode: TJsonNode);
    function  EqualErrors(aExpect, aActual: TJsonNode): Boolean;
  published
    procedure TestInvalidSchemas;
    procedure TestMaxDepth;
    procedure TestMaxErrors;
    procedure TestValidation;
  end;

implementation

{ TJtdUtilsTest }

procedure TJtdUtilsTest.IsRfc8927TimeStamp_InvalidYear;
var
  s: string;
begin
  s := '101-09-20T13:19:15.01Z';
  AssertFalse('Accepts three-digit year', IsRfc8927TimeStamp(s));
  s := '$101-09-20T13:19:15Z';
  AssertFalse('Accepts garbage year', IsRfc8927TimeStamp(s));
  s := '2101:09-20T13:19:15';
  AssertFalse('Accepts colon as date delimiter', IsRfc8927TimeStamp(s));
end;

procedure TJtdUtilsTest.IsRfc8927TimeStamp_InvalidMonth;
var
  s: string;
begin
  s := '2001-19-20T13:19:15';
  AssertFalse('Accepts a month value greater than 12', IsRfc8927TimeStamp(s));
  s := '2001-00-20T13:19:15';
  AssertFalse('Accepts zero month value', IsRfc8927TimeStamp(s));
  s := '2011-1a-20T13:19:15';
  AssertFalse('Accepts garbage month', IsRfc8927TimeStamp(s));
end;

procedure TJtdUtilsTest.IsRfc8927TimeStamp_InvalidDay;
var
  s: string;
begin
  s := '2001-09-40T13:19:15';
  AssertFalse('Accepts a day value greater than 31', IsRfc8927TimeStamp(s));
  s := '2001-09-31T13:19:15';
  AssertFalse('Accepts a day value greater than day in month', IsRfc8927TimeStamp(s));
  s := '2001-02-00T13:19:15';
  AssertFalse('Accepts zero day value', IsRfc8927TimeStamp(s));
  s := '2001-02-29T13:19:15';
  AssertFalse('Accepts 29 as the value of the day in February of a non-leap year', IsRfc8927TimeStamp(s));
  s := '2011-10-2#T13:19:15';
  AssertFalse('Accepts garbage day', IsRfc8927TimeStamp(s));
end;

procedure TJtdUtilsTest.IsRfc8927TimeStamp_InvalidHour;
var
  s: string;
begin
  s := '2014-03-20T33:19:15';
  AssertFalse('Accepts a hour value greater than 23', IsRfc8927TimeStamp(s));
  s := '2014-03-20Tf3:19:15';
  AssertFalse('Accepts garbage hour', IsRfc8927TimeStamp(s));
   s := '2014-03-20T13-19:15';
  AssertFalse('Accepts dash as time delimiter', IsRfc8927TimeStamp(s));
end;

procedure TJtdUtilsTest.IsRfc8927TimeStamp_InvalidMinute;
var
  s: string;
begin
  s := '2004-05-20T21:60:15';
  AssertFalse('Accepts a minute value greater than 59', IsRfc8927TimeStamp(s));
  s := '2004-05-20T12:o9:15';
  AssertFalse('Accepts garbage minute', IsRfc8927TimeStamp(s));
end;

procedure TJtdUtilsTest.IsRfc8927TimeStamp_InvalidSeconde;
var
  s: string;
begin
  s := '1998-05-20T21:40:61';
  AssertFalse('Accepts a second value greater than 60', IsRfc8927TimeStamp(s));
  s := '1998-05-20T21:40:t9';
  AssertFalse('Accepts garbage second', IsRfc8927TimeStamp(s));
  s := '1998-05-20T23:59:60';
  AssertFalse('Accepts a non-existent leap second', IsRfc8927TimeStamp(s));
end;

procedure TJtdTest.TestInvalidSchemas;
var
  FileName: string;
  NodeRef: specialize TGUniqRef<TJsonNode>;
  SampleList: TJsonNode;
  p: TJsonNode.TPair;
  Schema: TJtdSchema;
  Loaded: Boolean;
  I: Integer;
begin
  AssertTrue('Test folder not found', TestDir <> '');
  FileName := TestDir + INVALID_SCHEMAS;
  AssertTrue('Test file "' + INVALID_SCHEMAS + '" not found', FileExists(FileName));
  AssertTrue('Can not parse file "' + INVALID_SCHEMAS + '"', TJsonNode.TryParseFile(FileName, SampleList));
  NodeRef.Instance := SampleList;
  I := 0;
  for p in SampleList.Enrties do
    begin
      Inc(I);
      Loaded := False;
      if TJtdSchema.TryLoad(p.Value, Schema) then
        begin
          Loaded := True;
          Schema.Free;
        end;
      AssertFalse(p.Key, Loaded);
    end;
  AssertTrue('Unexpected number of samples(' + IntToStr(I) + ')', I = 49);
end;

procedure TJtdTest.TestMaxDepth;
var
  Schema: TJtdSchema;
  SchemaRef: specialize TGUniqRef<TJtdSchema>;
  InstRef: specialize TGUniqRef<TJsonNode>;
  eList: TJtdErrorList = nil;
const
  SchemaJson = '{"definitions":{"loop":{"ref":"loop"}},"ref":"loop"}';
  InstanceJson = '[null]';
begin
  AssertTrue('Can not load schema', TJtdSchema.TryLoad(SchemaJson, Schema));
  SchemaRef.Instance := Schema;
  InstRef.Instance := TJsonNode.NewJson(InstanceJson);
  AssertTrue('Can not load instance', InstRef.Instance <> nil);
  AssertTrue('Expected result "jrvInvalidParam" did not occur',
             Validate(InstRef.Instance, Schema, eList, 1, 0) = jrvInvalidParam);
  AssertTrue('Expected result "jrvInvalidParam" did not occur',
             Validate(InstRef.Instance, Schema, eList, 1, 42) = jvrMaxDepthExceed);
end;

procedure TJtdTest.TestMaxErrors;
var
  Schema: TJtdSchema;
  SchemaRef: specialize TGUniqRef<TJtdSchema>;
  InstRef: specialize TGUniqRef<TJsonNode>;
  eList: TJtdErrorList = nil;
const
  SchemaJson = '{"elements":{"type":"int32"}}';
  InstanceJson = '["a",true,null,{},[]]';
begin
  AssertTrue('Can not load schema', TJtdSchema.TryLoad(SchemaJson, Schema));
  SchemaRef.Instance := Schema;
  InstRef.Instance := TJsonNode.NewJson(InstanceJson);
  AssertTrue('Can not load instance', InstRef.Instance <> nil);
  AssertTrue('Expected result "jrvInvalidParam" did not occur',
             Validate(InstRef.Instance, Schema, eList, 0) = jrvInvalidParam);
  AssertTrue('Expected result "jvrMaxErrorsExceed" did not occur',
             Validate(InstRef.Instance, Schema, eList, 4) = jvrMaxErrorsExceed);
  AssertTrue(Length(eList) = 4);
end;

procedure TJtdTest.TestValidation;
var
  FileName: string;
  NodeRef: specialize TGUniqRef<TJsonNode>;
  SampleList, SchemaNode, Instance, ExpectError, ActError: TJsonNode;
  p: TJsonNode.TPair;
  Schema: TJtdSchema;
  eList: TJtdErrorList;
  I: Integer;
begin
  AssertTrue('Test folder not found', TestDir <> '');
  FileName := TestDir + VALIDATION_SET;
  AssertTrue('Test file "' + VALIDATION_SET + '" not found', FileExists(FileName));
  AssertTrue('Can not parse file "' + VALIDATION_SET + '"', TJsonNode.TryParseFile(FileName, SampleList));
  NodeRef.Instance := SampleList;
  NormalizeErrors(SampleList);
  I := 0;
  for p in SampleList.Enrties do
    begin
      Inc(I);
      if p.Key = 'timestamp type schema - 1990-12-31T15:59:60-08:00' then
        continue;//is not real leap second
      AssertTrue('Can not find schema in "'+p.Key+'"', p.Value.Find('schema', SchemaNode));
      AssertTrue('Can not load schema "'+p.Key+'"', TJtdSchema.TryLoad(SchemaNode, Schema));
      try
        AssertTrue('Can not find instance in "'+p.Key+'"', p.Value.Find('instance', Instance));
        AssertTrue('Can not find errors in "'+p.Key+'"', p.Value.Find('errors', ExpectError));
        eList := nil;
        if Validate(Instance, Schema, eList) <> jvrOk then
          begin
            ActError := TJsonNode.NewJson(eList.AsJson);
            try
              AssertTrue('Errors in "'+p.Key+'" do not match actual', EqualErrors(ExpectError, ActError));
            finally
              ActError.Free;
            end;
          end
        else
          AssertTrue('TJtdSchema missed an errors in "'+p.Key+'"', ExpectError.Count = 0);
      finally
        Schema.Free;
      end;
    end;
  AssertTrue('Unexpected number of samples(' + IntToStr(I) + ')', I = 316);
end;

procedure TJtdTest.SetUp;
var
  Dir: string;
begin
  Dir := ExcludeTrailingPathDelimiter(ProgramDirectory);
  while (Dir <> '') and (ExtractFileName(Dir) <> 'test') do
    Dir := ExcludeTrailingPathDelimiter(ExtractFilePath(Dir));
  if Dir = '' then exit;
  TestDir := Dir + DirectorySeparator + TEST_DIR + DirectorySeparator;
  Dir := TestDir + SPEC_DIR + DirectorySeparator;
  if DirectoryExists(Dir) then
    TestDir := Dir;
end;

procedure TJtdTest.NormalizeErrors(aNode: TJsonNode);
var
  p: TJsonNode.TPair;
  Errors, ErrItem, Path: TJsonNode;
  a: TStringArray;
  JPtr: string;
  I: Integer;
begin
  if not aNode.IsObject then exit;
  for p in aNode.Enrties do
    begin
      if not p.Value.IsObject then continue;
      if not p.Value.Find('errors', Errors) then continue;
      if not Errors.IsArray or (Errors.Count = 0) then continue;
      for ErrItem in Errors do
        begin
          if not ErrItem.IsObject then continue;
          if not ErrItem.Find('instancePath', Path) then continue;
          JPtr := '';
          if Path.Count <> 0 then
            begin
              SetLength(a, Path.Count);
              for I := 0 to High(a) do
                a[I] := Path.Items[I].AsString;
              JPtr := TJsonPtr.ToPointer(a);
            end;
          Path.AsString := JPtr;
          if not ErrItem.Find('schemaPath', Path) then continue;
          JPtr := '';
          if Path.Count <> 0 then
            begin
              SetLength(a, Path.Count);
              for I := 0 to High(a) do
                a[I] := Path.Items[I].AsString;
              JPtr := TJsonPtr.ToPointer(a);
            end;
          Path.AsString := JPtr;
        end;
    end;
end;

function TJtdTest.EqualErrors(aExpect, aActual: TJsonNode): Boolean;
var
  ExpItem, ActItem: TJsonNode;
  Found: Boolean;
begin
  if not(aExpect.IsArray and aActual.IsArray) then
    exit(False);
  if aExpect.Count = 0 then
    exit(aActual.Count = 0)
  else
    if aActual.Count = 0 then
      exit(False);
  if aExpect.Count = 1 then
    Result := TJsonNode.Equal(aExpect.Items[0], aActual.Items[0])
  else
    begin
      for ExpItem in aExpect do
        begin
          Found := False;
          for ActItem in aActual do
            if TJsonNode.Equal(ActItem, ExpItem) then
              begin
                Found := True;
                break;
              end;
          if not Found then
            exit(False);
        end;
      Result := True;
    end;
end;

initialization
  RegisterTest(TJtdUtilsTest);
  RegisterTest(TJtdTest);
end.

