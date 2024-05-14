unit lgJtdTest;

{$MODE OBJFPC}{$H+}{$WARN 5089 OFF}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, FileUtil,
  lgUtils, lgJson, lgJsonTypeDef, lgJtdInfer;

type

  { TJtdUtilsTest }

  TJtdUtilsTest = class(TTestCase)
  protected
    function RandomDateTime: TDateTime;
    function EqualTs(const L, R: TTimeStamp): Boolean;
  published
    procedure IsRfc8927TimeStamp_InvalidYear;
    procedure IsRfc8927TimeStamp_InvalidMonth;
    procedure IsRfc8927TimeStamp_InvalidDay;
    procedure IsRfc8927TimeStamp_InvalidHour;
    procedure IsRfc8927TimeStamp_InvalidMinute;
    procedure IsRfc8927TimeStamp_InvalidSecond;
    procedure IsRfc8927TimeStamp_InvalidSecFraction;
    procedure IsRfc8927TimeStamp_InvalidOffset;
    procedure IsRfc8927TimeStamp_InvalidMisc;
    procedure Rfc8927TimeStamp_LS;
    procedure RoundTrip_TDateTime;
    procedure RoundTrip_TTimeStamp;
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

  { TJtdInferTest }

  TJtdInferTest = class(TTestCase)
  private
  type
    TInfer  = specialize TGUniqRef<TJtdInferrer>;
    TSchema = specialize TGAutoRef<TJsonNode>;
    THint  = TJtdInferrer.THint;
  published
    procedure TestCreate;
    procedure TestScalar;
    procedure TestEnum;
    procedure TestProperties;
    procedure TestElements;
    procedure TestValues;
    procedure TestDiscriminator;
  end;

implementation

{ TJtdUtilsTest }

function TJtdUtilsTest.RandomDateTime: TDateTime;
var
  d1, d2: TDateTime;
begin
  d1 := EncodeDate(Succ(Random(9999)), Succ(Random(12)), Succ(Random(28)));
  d2 := EncodeTime(Random(24), Random(60), Random(60), Random(1000));
  Result := ComposeDateTime(d1, d2);
end;

function TJtdUtilsTest.EqualTs(const L, R: TTimeStamp): Boolean;
begin
  Result := (L.Date = R.Date) and (L.Time = R.Time);
end;

procedure TJtdUtilsTest.IsRfc8927TimeStamp_InvalidYear;
var
  s: string;
begin
  s := '101-09-20T13:19:15.01Z';
  AssertFalse('Accepts three-digit year', IsRfc8927TimeStamp(s));
  s := '$101-09-20T13:19:15Z';
  AssertFalse('Accepts garbage year', IsRfc8927TimeStamp(s));
  s := '2101:09-20T13:19:15Z';
  AssertFalse('Accepts colon as a date separator', IsRfc8927TimeStamp(s));
end;

procedure TJtdUtilsTest.IsRfc8927TimeStamp_InvalidMonth;
var
  s: string;
begin
  s := '2001-19-20T13:19:15Z';
  AssertFalse('Accepts a month value greater than 12', IsRfc8927TimeStamp(s));
  s := '2001-00-20T13:19:15Z';
  AssertFalse('Accepts zero month value', IsRfc8927TimeStamp(s));
  s := '2011-1a-20T13:19:15Z';
  AssertFalse('Accepts garbage month', IsRfc8927TimeStamp(s));
end;

procedure TJtdUtilsTest.IsRfc8927TimeStamp_InvalidDay;
var
  s: string;
begin
  s := '2001-09-40T13:19:15Z';
  AssertFalse('Accepts a day value greater than 31', IsRfc8927TimeStamp(s));
  s := '2001-09-31T13:19:15Z';
  AssertFalse('Accepts a day value greater than day in month', IsRfc8927TimeStamp(s));
  s := '2001-02-00T13:19:15Z';
  AssertFalse('Accepts zero day value', IsRfc8927TimeStamp(s));
  s := '2001-02-29T13:19:15Z';
  AssertFalse('Accepts 29 as the value of the day in February of a non-leap year', IsRfc8927TimeStamp(s));
  s := '2011-10-2#T13:19:15Z';
  AssertFalse('Accepts garbage day', IsRfc8927TimeStamp(s));
end;

procedure TJtdUtilsTest.IsRfc8927TimeStamp_InvalidHour;
var
  s: string;
begin
  s := '2014-03-20T33:19:15Z';
  AssertFalse('Accepts a hour value greater than 23', IsRfc8927TimeStamp(s));
  s := '2014-03-20Tf3:19:15Z';
  AssertFalse('Accepts garbage hour', IsRfc8927TimeStamp(s));
   s := '2014-03-20T13-19:15Z';
  AssertFalse('Accepts dash as a time separator', IsRfc8927TimeStamp(s));
end;

procedure TJtdUtilsTest.IsRfc8927TimeStamp_InvalidMinute;
var
  s: string;
begin
  s := '2004-05-20T21:60:15Z';
  AssertFalse('Accepts a minute value greater than 59', IsRfc8927TimeStamp(s));
  s := '2004-05-20T12:o9:15Z';
  AssertFalse('Accepts garbage minute', IsRfc8927TimeStamp(s));
end;

procedure TJtdUtilsTest.IsRfc8927TimeStamp_InvalidSecond;
var
  s: string;
begin
  s := '1998-05-20T21:40:61Z';
  AssertFalse('Accepts a second value greater than 60', IsRfc8927TimeStamp(s));
  s := '1998-05-20T21:40:t9Z';
  AssertFalse('Accepts garbage second', IsRfc8927TimeStamp(s));
end;

procedure TJtdUtilsTest.IsRfc8927TimeStamp_InvalidSecFraction;
var
  s: string;
begin
  s := '1998-05-20T21:40:61,333Z';
  AssertFalse('Accepts a comma as the fractional second separator', IsRfc8927TimeStamp(s));
  s := '1998-05-20T21:40:59.Z';
  AssertFalse('Accepts the fractional part of a second without any digits', IsRfc8927TimeStamp(s));
  s := '1998-05-20T23:59:40.1g8Z';
  AssertFalse('Accepts garbage fraction of a second', IsRfc8927TimeStamp(s));
end;

procedure TJtdUtilsTest.IsRfc8927TimeStamp_InvalidOffset;
var
  s: string;
begin
  s := '2018-06-20T23:59:35 08:11';
  AssertFalse('Accepts a space as an local offset separator', IsRfc8927TimeStamp(s));
  s := '2018-06-20T23:59:35+24:11';
  AssertFalse('Accepts a local offset hours grater than 23', IsRfc8927TimeStamp(s));
  s := '2018-06-20T23:59:35+24:11';
  AssertFalse('Accepts a local offset hours value greater than 23', IsRfc8927TimeStamp(s));
  s := '2018-06-20T23:59:35-04:60';
  AssertFalse('Accepts a local offset minutes value greater than 59', IsRfc8927TimeStamp(s));
  s := '2018-06-20T23:59:35+$4:60';
  AssertFalse('Accepts garbage local offset', IsRfc8927TimeStamp(s));
end;

procedure TJtdUtilsTest.IsRfc8927TimeStamp_InvalidMisc;
var
  s: string;
begin
  s := '2018-06-20 23:59:35Z';
  AssertFalse('Accepts a space as the date/time separator', IsRfc8927TimeStamp(s));
  s := '2018-06-20u23:59:35Z';
  AssertFalse('Accepts an arbitrary character as the date/time separator', IsRfc8927TimeStamp(s));
  s := '2018-06-20t23:59:35Z';
  AssertFalse('Accepts lower case "t" as the date/time separator', IsRfc8927TimeStamp(s));
  s := '2018-06-20T23:59:35z';
  AssertFalse('Accepts lower case "z" as Zulu time zone', IsRfc8927TimeStamp(s));
  s := '2018-06-20T23:59:35.128';
  AssertFalse('Accepts the value without the "Z" character', IsRfc8927TimeStamp(s));

end;

procedure TJtdUtilsTest.Rfc8927TimeStamp_LS;
var
  ts: TTimeStamp;
  s: string;
begin
  s := '1995-12-31T23:59:60Z';
  AssertTrue(TryRfc8927TimeStampToUTC(s, ts));
  AssertTrue(UTCToRfc8927TimeStamp(ts) = s);
  s := '2015-06-30T23:59:60Z';
  AssertTrue(TryRfc8927TimeStampToUTC(s, ts));
  AssertTrue(UTCToRfc8927TimeStamp(ts) = s);
  s := '2012-06-30T23:59:60Z';
  AssertTrue(TryRfc8927TimeStampToUTC(s, ts));
  AssertTrue(UTCToRfc8927TimeStamp(ts) = s);
  s := '2014-06-30T23:59:60Z';
  AssertFalse(TryRfc8927TimeStampToUTC(s, ts));
end;

procedure TJtdUtilsTest.RoundTrip_TDateTime;
var
  d, d2: TDateTime;
  s: string;
  I: Integer;
const
  TestSize = 1000;
begin
  for I := 1 to TestSize do
    begin
      d := RandomDateTime;
      s := UTCToRfc8927TimeStamp(d);
      AssertTrue(TryRfc8927TimeStampToUTC(s, d2));
      AssertTrue(d2 = d);
    end;
end;

procedure TJtdUtilsTest.RoundTrip_TTimeStamp;
var
  ts, ts2: TTimeStamp;
  s: string;
  I: Integer;
const
  TestSize = 1000;
begin
  for I := 1 to TestSize do
    begin
      ts := DateTimeToTimeStamp(RandomDateTime);
      s := UTCToRfc8927TimeStamp(ts);
      AssertTrue(TryRfc8927TimeStampToUTC(s, ts2));
      AssertTrue(EqualTs(ts2, ts));
    end;
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
  for p in SampleList.Entries do
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
  for p in SampleList.Entries do
    begin
      Inc(I);
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
  for p in aNode.Entries do
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

{ TJtdInferTest }

procedure TJtdInferTest.TestCreate;
var
  Infer: TInfer;
  StrSamples: TStringArray;
  Samples: array of TJsonNode;
  Schema: TSchema;
begin
  StrSamples := nil;
  Infer.Instance := TJtdInferrer.Create(StrSamples, []);
  AssertTrue(Infer.Instance.MaxDepth = TJtdInferrer.DEF_DEPTH);
  AssertFalse(Infer.Instance.SkipBom);
  Schema.Instance := Infer.Instance.Execute;
  AssertTrue(Schema.Instance.AsJson = '{}');

  Samples := nil;
  Infer.Instance := TJtdInferrer.Create(Samples,[]);
  AssertTrue(Infer.Instance.MaxDepth = TJtdInferrer.DEF_DEPTH);
  AssertFalse(Infer.Instance.SkipBom);
  Schema.Instance := Infer.Instance.Execute;
  AssertTrue(Schema.Instance.AsJson = '{}');

  StrSamples := ['','a'];
  Infer.Instance := TJtdInferrer.Create(StrSamples,[]);
  Schema.Instance := Infer.Instance.Execute;
  AssertTrue(Schema.Instance.AsJson = '{}');
end;

procedure TJtdInferTest.TestScalar;
var
  Infer: TInfer;
  Schema: TSchema;
begin
  Infer.Instance := TJtdInferrer.Create(['42'], []);
  Schema.Instance := Infer.Instance.Execute;
  AssertTrue(Schema.Instance.AsJson = '{"type":"int8"}');

  Infer.Instance := TJtdInferrer.Create(['42'], [THint.Make(ntFloat64)]);
  Schema.Instance := Infer.Instance.Execute;
  AssertTrue(Schema.Instance.AsJson = '{"type":"float64"}');

  Infer.Instance := TJtdInferrer.Create(['42','null'], []);
  Schema.Instance := Infer.Instance.Execute;
  AssertTrue(Schema.Instance.AsJson = '{"type":"int8","nullable":true}');

  Infer.Instance := TJtdInferrer.Create(['42','"123"'], []);
  Schema.Instance := Infer.Instance.Execute;
  AssertTrue(Schema.Instance.AsJson = '{}');

  Infer.Instance := TJtdInferrer.Create(['false'], []);
  Schema.Instance := Infer.Instance.Execute;
  AssertTrue(Schema.Instance.AsJson = '{"type":"boolean"}');

  Infer.Instance := TJtdInferrer.Create(['false','true','null'], []);
  Schema.Instance := Infer.Instance.Execute;
  AssertTrue(Schema.Instance.AsJson = '{"type":"boolean","nullable":true}');

  Infer.Instance := TJtdInferrer.Create(['""'], []);
  Schema.Instance := Infer.Instance.Execute;
  AssertTrue(Schema.Instance.AsJson = '{"type":"string"}');

  Infer.Instance := TJtdInferrer.Create(['"a"','null'], []);
  Schema.Instance := Infer.Instance.Execute;
  AssertTrue(Schema.Instance.AsJson = '{"type":"string","nullable":true}');

  Infer.Instance := TJtdInferrer.Create(['"2015-10-12T23:59:15Z"'], []);
  Schema.Instance := Infer.Instance.Execute;
  AssertTrue(Schema.Instance.AsJson = '{"type":"timestamp"}');

  Infer.Instance := TJtdInferrer.Create(['"2015-10-12T23:59:15Z"','null'], []);
  Schema.Instance := Infer.Instance.Execute;
  AssertTrue(Schema.Instance.AsJson = '{"type":"timestamp","nullable":true}');

  Infer.Instance := TJtdInferrer.Create(['"2015-10-12T23:59:15Z"','""'], []);
  Schema.Instance := Infer.Instance.Execute;
  AssertTrue(Schema.Instance.AsJson = '{"type":"string"}');
end;

procedure TJtdInferTest.TestEnum;
var
  Infer: TInfer;
  Schema: TSchema;
begin
  Infer.Instance := TJtdInferrer.Create(['"aa"','"bb"','"cc"'], []);
  Schema.Instance := Infer.Instance.Execute;
  AssertTrue(Schema.Instance.AsJson = '{"type":"string"}');

  Infer.Instance := TJtdInferrer.Create(['"aa"','"bb"','"cc"'], [THint.Enum([])]);
  Schema.Instance := Infer.Instance.Execute;
  AssertTrue(Schema.Instance.AsJson = '{"enum":["aa","bb","cc"]}');

  Infer.Instance := TJtdInferrer.Create(['"aa"'], [THint.Enum(['aa','bb','cc'],[])]);
  Schema.Instance := Infer.Instance.Execute;
  AssertTrue(Schema.Instance.AsJson = '{"enum":["aa","bb","cc"]}');

  Infer.Instance := TJtdInferrer.Create(['"aa"','null'], [THint.Enum(['aa','bb','cc'],[])]);
  Schema.Instance := Infer.Instance.Execute;
  AssertTrue(Schema.Instance.AsJson = '{"enum":["aa","bb","cc"],"nullable":true}');

  Infer.Instance := TJtdInferrer.Create(['"aa"','42'], [THint.Enum(['aa','bb','cc'],[])]);
  Schema.Instance := Infer.Instance.Execute;
  AssertTrue(Schema.Instance.AsJson = '{}');
end;

procedure TJtdInferTest.TestProperties;
var
  Infer: TInfer;
  Schema, Expect: TSchema;
const
  j1   = '{"a":42,"b":1001}';
  j2   = '{"a":42,"b":1001,"c":"d"}';
  j3   = '{"a":42,"b":1001,"c":3.14}';
  exp1 = '{"properties":{"a":{"type":"int8"},"b":{"type":"int16"}}}';
  exp2 = '{"properties":{"a":{"type":"int8"},"b":{"type":"int16"}},"optionalProperties":{"c":{"type":"string"}}}';
  exp3 = '{"properties":{"a":{"type":"int8"},"b":{"type":"int16"}},"optionalProperties":{"c":{}}}';
  exp4 = '{"properties":{"a":{"type":"int8"},"b":{"type":"int16"}},' +
         '"optionalProperties":{"c":{"type":"string"}},' +
         '"additionalProperties":true}';
  exp5 = '{"properties":{"a":{"type":"int8"},"b":{"type":"int16"}},' +
         '"optionalProperties":{"c":{"type":"string"}},' +
         '"additionalProperties":true,"nullable":true}';
  exp6 = '{"properties":{"a":{"type":"int8","nullable":true},' +
         ' "b":{"type":"int16","nullable":true}},' +
         '"optionalProperties":{"c":{"type":"string","nullable":true}},' +
         '"additionalProperties":true}';
begin
  Infer.Instance := TJtdInferrer.Create([j1], []);
  Schema.Instance := Infer.Instance.Execute;
  Expect.Instance.AsJson := exp1;
  AssertTrue(Schema.Instance.EqualTo(Expect.Instance));

  Infer.Instance := TJtdInferrer.Create([j1, j2], []);
  Schema.Instance := Infer.Instance.Execute;
  Expect.Instance.AsJson := exp2;
  AssertTrue(Schema.Instance.EqualTo(Expect.Instance));

  Infer.Instance := TJtdInferrer.Create([j1, j2, j3], []);
  Schema.Instance := Infer.Instance.Execute;
  Expect.Instance.AsJson := exp3;
  AssertTrue(Schema.Instance.EqualTo(Expect.Instance));

  Infer.Instance := TJtdInferrer.Create([j1, j2], [THint.AddProps([])]);
  Schema.Instance := Infer.Instance.Execute;
  Expect.Instance.AsJson := exp4;
  AssertTrue(Schema.Instance.EqualTo(Expect.Instance));

  Infer.Instance := TJtdInferrer.Create([j1, j2], [THint.AddProps([]),THint.Nullable([])]);
  Schema.Instance := Infer.Instance.Execute;
  Expect.Instance.AsJson := exp5;
  AssertTrue(Schema.Instance.EqualTo(Expect.Instance));

  Infer.Instance := TJtdInferrer.Create([j1, j2], [THint.AddProps([]),THint.JpNullable('$.*')]);
  Schema.Instance := Infer.Instance.Execute;
  Expect.Instance.AsJson := exp6;
  AssertTrue(Schema.Instance.EqualTo(Expect.Instance));
end;

procedure TJtdInferTest.TestElements;
var
  Infer: TInfer;
  Schema: TSchema;
begin
  Infer.Instance := TJtdInferrer.Create(['[3,42,1001]'], []);
  Schema.Instance := Infer.Instance.Execute;
  AssertTrue(Schema.Instance.AsJson = '{"elements":{"type":"int16"}}');

  Infer.Instance := TJtdInferrer.Create(['[3,42,1001]','[3.14]'], []);
  Schema.Instance := Infer.Instance.Execute;
  AssertTrue(Schema.Instance.AsJson = '{"elements":{"type":"float64"}}');

  Infer.Instance := TJtdInferrer.Create(['[3,42,1001]','[3.14,null]'], []);
  Schema.Instance := Infer.Instance.Execute;
  AssertTrue(Schema.Instance.AsJson = '{"elements":{"type":"float64","nullable":true}}');

  Infer.Instance := TJtdInferrer.Create(['[3,42,1001]','[3.14,"a"]'], []);
  Schema.Instance := Infer.Instance.Execute;
  AssertTrue(Schema.Instance.AsJson = '{"elements":{}}');

  Infer.Instance := TJtdInferrer.Create(['[42,3.14,1001]'], [THint.Nullable([])]);
  Schema.Instance := Infer.Instance.Execute;
  AssertTrue(Schema.Instance.AsJson = '{"nullable":true,"elements":{"type":"float64"}}');

  Infer.Instance := TJtdInferrer.Create(['[42,3.14,1001]'], [THint.Nullable(['0'])]);
  Schema.Instance := Infer.Instance.Execute;
  AssertTrue(Schema.Instance.AsJson = '{"elements":{"nullable":true,"type":"float64"}}');
end;

procedure TJtdInferTest.TestValues;
var
  Infer: TInfer;
  Schema, Expect: TSchema;
const
  j1   = '{"a":42,"b":1001}';
  j2   = '{"c":333,"d":777}';
  j3   = '{"c":333,"d":""}';
  exp1 = '{"optionalProperties":{"a":{"type":"int8"},"b":{"type":"int16"},"c":{"type":"int16"},"d":{"type":"int16"}}}';
  exp2 = '{"values":{"type":"int16"}}';
  exp3 = '{"values":{"type":"int16","nullable":true}}';
  exp4 = '{"values":{"type":"int16"},"nullable":true}';
  exp5 = '{"values":{}}';
begin
  Infer.Instance := TJtdInferrer.Create([j1, j2], []);
  Schema.Instance := Infer.Instance.Execute;
  Expect.Instance.AsJson := exp1;
  AssertTrue(Schema.Instance.EqualTo(Expect.Instance));

  Infer.Instance := TJtdInferrer.Create([j1, j2], [THint.Map([])]);
  Schema.Instance := Infer.Instance.Execute;
  Expect.Instance.AsJson := exp2;
  AssertTrue(Schema.Instance.EqualTo(Expect.Instance));

  Infer.Instance := TJtdInferrer.Create([j1, j2], [THint.Map([]),THint.Nullable(['a'])]);
  Schema.Instance := Infer.Instance.Execute;
  Expect.Instance.AsJson := exp3;
  AssertTrue(Schema.Instance.EqualTo(Expect.Instance));

  Infer.Instance := TJtdInferrer.Create([j1, j2], [THint.Map([]),THint.Nullable([])]);
  Schema.Instance := Infer.Instance.Execute;
  Expect.Instance.AsJson := exp4;
  AssertTrue(Schema.Instance.EqualTo(Expect.Instance));

  Infer.Instance := TJtdInferrer.Create([j1, j3], [THint.Map([])]);
  Schema.Instance := Infer.Instance.Execute;
  Expect.Instance.AsJson := exp5;
  AssertTrue(Schema.Instance.EqualTo(Expect.Instance));
end;

procedure TJtdInferTest.TestDiscriminator;
var
  Infer: TInfer;
  Schema, Expect: TSchema;
const
  j1   = '{"tag":"a","d":1001}';
  j2   = '{"tag":"b","d":777,"e":"s"}';
  j3   = '{"tag":"c","f":"g","h":false}';
  j4   = '{"tag":"b","d":2048,"e":null}';
  j5   = '{"tog":"u","s":false}';
  j6   = '{"tag":true,"s":false}';
  exp1 = '{"properties":{"tag":{"type":"string"}},"optionalProperties":{"e":{"type":"string"},' +
         ' "d":{"type":"int16"},"f":{"type":"string"},"h":{"type":"boolean"}}}';
  exp2 = '{"discriminator":"tag","mapping":{"a":{"properties":{"d":{"type":"int16"}}},' +
         ' "b":{"properties":{"d":{"type":"int16"},"e":{"type":"string"}}},' +
         ' "c":{"properties":{"f":{"type":"string"},"h":{"type":"boolean"}}}}}';
  exp3 = '{"discriminator":"tag","mapping":{"a":{"properties":{"d":{"type":"int16"}}},' +
         ' "b":{"properties":{"d":{"type":"int16"},"e":{"type":"string","nullable":true}}},' +
         ' "c":{"properties":{"f":{"type":"string"},"h":{"type":"boolean"}}}}}';
begin
  Infer.Instance := TJtdInferrer.Create([j1, j2, j3], []);
  Schema.Instance := Infer.Instance.Execute;
  Expect.Instance.AsJson := exp1;
  AssertTrue(Schema.Instance.EqualTo(Expect.Instance));

  Infer.Instance := TJtdInferrer.Create([j1, j2, j3], [THint.Variant(['tag'])]);
  Schema.Instance := Infer.Instance.Execute;
  Expect.Instance.AsJson := exp2;
  AssertTrue(Schema.Instance.EqualTo(Expect.Instance));

  Infer.Instance := TJtdInferrer.Create([j1, j2, j3, j4], [THint.Variant(['tag'])]);
  Schema.Instance := Infer.Instance.Execute;
  Expect.Instance.AsJson := exp3;
  AssertTrue(Schema.Instance.EqualTo(Expect.Instance));

  Infer.Instance := TJtdInferrer.Create([j1, j2, j3, j5], [THint.Variant(['tag'])]);
  Schema.Instance := Infer.Instance.Execute;
  AssertTrue(Schema.Instance.AsJson = '{}');

  Infer.Instance := TJtdInferrer.Create([j1, j2, j3, j6], [THint.Variant(['tag'])]);
  Schema.Instance := Infer.Instance.Execute;
  AssertTrue(Schema.Instance.AsJson = '{}');
end;

initialization
  Randomize;
  RegisterTest(TJtdUtilsTest);
  RegisterTest(TJtdTest);
  RegisterTest(TJtdInferTest);
end.

