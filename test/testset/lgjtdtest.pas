unit lgJtdTest;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, FileUtil,
  lgUtils, lgJson, lgJsonTypeDef;

type

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
    procedure TestValidation;
  end;

implementation

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
    {$IF FPC_FULLVERSION < 30300}
       if (I >= 198) and (I <= 202) then continue;//TryISOStrToDateTime() in FPC-3.2.2 can not parse it
    {$ELSE }
      if (I = 200) or (I = 201) then continue;//TryISOStrToDateTime() doesn't like leap seconds
    {$ENDIF}
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

  RegisterTest(TJtdTest);

end.

