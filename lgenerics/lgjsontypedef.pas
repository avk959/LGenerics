{****************************************************************************
*                                                                           *
*   This file is part of the LGenerics package.                             *
*   Implementation of JSON Type Definition(aka RFC 8927) specifications,    *
*   a lightweight schema definition language for JSON documents.            *
*                                                                           *
*   Copyright(c) 2023 A.Koverdyaev(avk)                                     *
*                                                                           *
*   This code is free software; you can redistribute it and/or modify it    *
*   under the terms of the Apache License, Version 2.0;                     *
*   You may obtain a copy of the License at                                 *
*     http://www.apache.org/licenses/LICENSE-2.0.                           *
*                                                                           *
*  Unless required by applicable law or agreed to in writing, software      *
*  distributed under the License is distributed on an "AS IS" BASIS,        *
*  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. *
*  See the License for the specific language governing permissions and      *
*  limitations under the License.                                           *
*                                                                           *
*****************************************************************************}
unit lgJsonTypeDef;

{$MODE OBJFPC}{$H+}{$MODESWITCH ADVANCEDRECORDS}{$MODESWITCH TYPEHELPERS}

interface

uses
  Classes, SysUtils, lgHashSet, lgHashMap, lgJson;

type
  EJtdSchemaLoad     = class(Exception);
  EJtdSchemaVerify   = class(Exception);
  EJtdSchemaValidate = class(Exception);

const
  DEF_DEPTH = TJsonNode.DEF_DEPTH;

type
  TJtdSchema = class;

  { TJtdSchemaMap: just a minimalistic wrapper on top of TGLiteChainHashMap, useful due to its fast enumerators }
  TJtdSchemaMap = class
  private
  type
    TMapType = specialize TGLiteChainHashMap<string, TJtdSchema, string>;
    TMap     = TMapType.TMap;
  public
  type
    TEntry      = TMap.TEntry;
    TEnumerator = TMap.TEntryEnumerator;
    TKeys       = TMap.TKeys;
    TValues     = TMap.TValues;
  strict private
    FMap: TMap;
    function GetItem(const aKey: string): TJtdSchema; inline;
  public
    destructor Destroy; override;
    function GetEnumerator: TEnumerator; inline;
    function Keys: TKeys; inline;
    function Values: TValues; inline;
    function Contains(const aKey: string): Boolean; inline;
    function Add(const aKey: string; aValue: TJtdSchema): Boolean; inline;
    property Items[const aKey: string]: TJtdSchema read GetItem; default;
  end;

  { TJtdSchema: represents a JSON Type Definition schema }
  TJtdSchema = class
  public
  type
    TMetaDataMap = specialize TGObjHashMapLP<string, TJsonNode>;
    TStringSet   = specialize TGHashSetLP<string>;
    TFormKind    = (fkNone, fkEmpty, fkRef, fkType, fkEnum, fkElements, fkProperties, fkValues, fkDiscriminator);
    TJtdType     = (jtNone, jtBool, jtFloat32, jtFloat64, jtInt8, jtUInt8, jtInt16, jtUInt16, jtInt32, jtUInt32,
                    jtString, jtTimeStamp);
  private
  type
    TSchemaProp  = (spRef, spType, spEnum, spElements, spProperties, spOptionalProperties, spAdditionalProperties,
                    spValues, spDiscriminator, spMapping, spDefinitions, spNullable, spMetaData);
    TSignProp    = spRef..spMapping;
    TSignature   = set of TSignProp;
    TKeywordKind = (kkType, kkProp);

    TKeyword = record
      Kind: TKeywordKind;
      constructor MakeType(aType: TJtdType);
      constructor MakeProp(aProp: TSchemaProp);
      case Boolean of
        False: (JType: TJtdType);
        True:  (SProp: TSchemaProp);
    end;

    TKeywordMapType = specialize TGLiteChainHashMap<string, TKeyword, string>;
    TKeywordMap     = TKeywordMapType.TMap;
  const
  {$PUSH}{$J-}
    JTD_TYPES: array[TJtdType] of string = (
      '', 'boolean', 'float32', 'float64', 'int8', 'uint8', 'int16', 'uint16', 'int32', 'uint32',
      'string', 'timestamp');
    JTD_PROPS: array[TSchemaProp] of string = (
      'ref', 'type', 'enum', 'elements', 'properties', 'optionalProperties', 'additionalProperties',
      'values', 'discriminator', 'mapping', 'definitions', 'nullable', 'metadata');
  {$POP}
  class var
    CFKeywords: TKeywordMap;
    class constructor Init;
  var
    FDefs,                //Definitions
    FProps: TJtdSchemaMap;//Properties
    FVarInst: TObject;    //OptionalProperties or Elements or Mapping or Enum or Values
    FMetadata: TMetaDataMap;
    FStrValue: string;
    FKind: TFormKind;
    FType: TJtdType;
    FNullable,
    FAddProps: Boolean;
    function  GetDefinitions: TJtdSchemaMap; inline;
    function  GetDiscriminator: string; inline;
    function  GetElements: TJtdSchema; inline;
    function  GetEnum: TStringSet; inline;
    function  GetMapping: TJtdSchemaMap; inline;
    function  GetOptionalProperties: TJtdSchemaMap; inline;
    function  GetProperties: TJtdSchemaMap; inline;
    function  GetRef: string; inline;
    function  GetValues: TJtdSchema; inline;
    function  GetSignature: TSignature;
    procedure Verify(aRoot: TJtdSchema);
    procedure LoadNode(aNode: TJsonNode);
    class function  GetKeyword(const aKey: string; out aValue: TKeyword): Boolean; inline;
    class function  GetFormKind(const aSgn: TSignature): TFormKind; static;
    class procedure DoLoadSchema(aRoot: TJtdSchema; aNode: TJsonNode); static;
  public
    class function TryLoad(aJson: TJsonNode; out aSchema: TJtdSchema): Boolean; static;
    class function TryLoad(const aJson: string; out aSchema: TJtdSchema): Boolean; static;
    class function TryLoadFromStream(aStream: TStream; out aSchema: TJtdSchema): Boolean; static;
    class function TryLoadFromFile(const aFileName: string; out aSchema: TJtdSchema): Boolean; static;
    destructor Destroy; override;
    procedure Clear;
  {  }
    procedure Load(aJson: TJsonNode);
    procedure Load(const aJson: string; aMaxDepth: Integer = DEF_DEPTH);
    procedure LoadFromStream(aStream: TStream; aMaxDepth: Integer = DEF_DEPTH);
    procedure LoadFromFile(const aFileName: string; aMaxDepth: Integer = DEF_DEPTH);
    property  Kind: TFormKind read FKind;
    property  Ref: string read GetRef;
    property  ElType: TJtdType read FType;
    property  Enum: TStringSet read GetEnum;
    property  Elements: TJtdSchema read GetElements;
    property  Properties: TJtdSchemaMap read GetProperties;
    property  OptionalProperties: TJtdSchemaMap read GetOptionalProperties;
    property  AdditionalProperties: Boolean read FAddProps;
    property  Values: TJtdSchema read GetValues;
    property  Discriminator: string read GetDiscriminator;
    property  Mapping: TJtdSchemaMap read GetMapping;
    property  Definitions: TJtdSchemaMap read GetDefinitions;
    property  Nullable: Boolean read FNullable;
    property  Metadata: TMetaDataMap read FMetadata;
  end;

  TValidateError = record
  const
    INSTANCE_PATH = 'instancePath';
    SCHEMA_PATH   = 'schemaPath';
  strict private
    FInstPath,
    FSchemaPath: string;// actually JSON pointers
  public
    class function Equal(const L, R: TValidateError): Boolean; static; inline;
    class function HashCode(const aValue: TValidateError): SizeInt; static; inline;
    constructor Make(const aInstPath, aSchemaPath: array of string);
    property InstancePath: string read FInstPath;
    property SchemaPath: string read FSchemaPath;
  end;

  TJtdErrorList = array of TValidateError;

  TErrorListHelper = type helper for TJtdErrorList
    function AsJson: string;
  end;

const
  DEFAULT_DEPTH  = 256;
  DEFAULT_ERRORS = 16;

type
  TJtdValidateResult = (
    jvrOk, jrvInvalidParam, jvrNonUniqKeys, jvrMaxErrorsExceed, jvrMaxDepthExceed, jvrErrors);

{ validates aInstance against aSchema;
  returns jrvInvalidParam if aMaxErrors or aMaxDepth is less than 1,
  otherwise returns jvrNonUniqKeys if aInstance contains non-unique keys,
  otherwise returns jvrMaxErrorsExceed if the number errors exceeds aMaxErrors,
  otherwise returns jvrMaxDepthExceed if the number of references followed exceeds aMaxDepth,
  otherwise returns jvrErrors if errors are found,
  otherwise returns jvrOk;
  returns the list of TValidateError in aErrList in conformance with the JSON Typedef specification }
  function Validate(aInstance: TJsonNode; aSchema: TJtdSchema; out aErrList: TJtdErrorList;
                    aMaxErrors: Integer = DEFAULT_ERRORS; aMaxDepth: Integer = DEFAULT_DEPTH): TJtdValidateResult;
  { todo: make it extensible using metadata? }

{
  returns True if s is valid Rfc8927TimeStamp, False otherwise;
  allows leading and trailing spaces;

    QUOTE from RFC 8927:
      timestamp:
        a JSON string that follows the standard format described in [RFC3339],
        as refined by Section 3.3 of [RFC4287]

    QUOTE from RFC 4287, Section 3.3:
      A Date construct is an element whose content MUST conform to the
      "date-time" production in [RFC3339]. In addition, an uppercase "T"
      character MUST be used to separate date and time, and an uppercase
      "Z" character MUST be present in the absence of a numeric time zone offset.

}
  function IsRfc8927TimeStamp(const s: string): Boolean;
  function IsRfc8927TimeStamp(p: PChar; aCount: SizeInt): Boolean;

implementation
{$B-}{$COPERATORS ON}{$POINTERMATH ON}

uses
  TypInfo, Math, DateUtils, lgUtils, lgVector, lgHash, lgStrConst;

resourcestring
  SEJtdValidationInternalFmt = 'Internal validation error %d';

{ TJtdSchemaMap }

function TJtdSchemaMap.GetItem(const aKey: string): TJtdSchema;
begin
  if not FMap.TryGetValue(aKey, Result) then Result := nil;
end;

destructor TJtdSchemaMap.Destroy;
var
  Schema: TJtdSchema;
begin
  for Schema in FMap.Values do Schema.Free;
  inherited;
end;

function TJtdSchemaMap.GetEnumerator: TEnumerator;
begin
  Result := FMap.GetEnumerator;
end;

function TJtdSchemaMap.Keys: TKeys;
begin
  Result := FMap.Keys;
end;

function TJtdSchemaMap.Values: TValues;
begin
  Result := FMap.Values;
end;

function TJtdSchemaMap.Contains(const aKey: string): Boolean;
begin
  Result := FMap.Contains(aKey);
end;

function TJtdSchemaMap.Add(const aKey: string; aValue: TJtdSchema): Boolean;
begin
  Result := FMap.Add(aKey, aValue);
end;

{ TJtdSchema.TKeyword }

constructor TJtdSchema.TKeyword.MakeType(aType: TJtdType);
begin
  Kind := kkType;
  JType := aType;
end;

constructor TJtdSchema.TKeyword.MakeProp(aProp: TSchemaProp);
begin
  Kind := kkProp;
  SProp := aProp;
end;

{ TJtdSchema }

class constructor TJtdSchema.Init;
var
  Prop: TSchemaProp;
  Typ: TJtdType;
begin
  for Prop in TSchemaProp do
    CFKeywords.Add(JTD_PROPS[Prop], TKeyword.MakeProp(Prop));
  for Typ := jtBool to jtTimeStamp do
    CFKeywords.Add(JTD_TYPES[Typ], TKeyword.MakeType(Typ));
end;

function TJtdSchema.GetDefinitions: TJtdSchemaMap;
begin
  Result := FDefs;
end;

function TJtdSchema.GetDiscriminator: string;
begin
  if Kind = fkDiscriminator then
    Result := FStrValue
  else
    Result := '';
end;

function TJtdSchema.GetElements: TJtdSchema;
begin
  if Kind = fkElements then
    Result := TJtdSchema(FVarInst)
  else
    Result := nil;
end;

function TJtdSchema.GetEnum: TStringSet;
begin
  if Kind = fkEnum then
    Result := TStringSet(FVarInst)
  else
    Result := nil;
end;

function TJtdSchema.GetMapping: TJtdSchemaMap;
begin
  if Kind = fkDiscriminator then
    Result := TJtdSchemaMap(FVarInst)
  else
    Result := nil;
end;

function TJtdSchema.GetOptionalProperties: TJtdSchemaMap;
begin
  if Kind = fkProperties then
    Result := TJtdSchemaMap(FVarInst)
  else
    Result := nil;
end;

function TJtdSchema.GetProperties: TJtdSchemaMap;
begin
  if Kind = fkProperties then
    Result := FProps
  else
    Result := nil;
end;

function TJtdSchema.GetRef: string;
begin
  if Kind = fkRef then
    Result := FStrValue
  else
    Result := '';
end;

function TJtdSchema.GetValues: TJtdSchema;
begin
  if Kind = fkValues then
    Result := TJtdSchema(FVarInst)
  else
    Result := nil;
end;

function TJtdSchema.GetSignature: TSignature;
begin
  Result := [];
  if Ref <> '' then Include(Result, spRef);
  if ElType <> jtNone then Include(Result, spType);
  if Enum <> nil then Include(Result, spEnum);
  if Elements <> nil then Include(Result, spElements);
  if Properties <> nil then Include(Result, spProperties);
  if OptionalProperties <> nil then Include(Result, spOptionalProperties);
  if AdditionalProperties then Include(Result, spAdditionalProperties);
  if Values <> nil then Include(Result, spValues);
  if Discriminator <> '' then Include(Result, spDiscriminator);
  if Mapping <> nil then Include(Result, spMapping);
end;

procedure TJtdSchema.Verify(aRoot: TJtdSchema);
var
  Schema: TJtdSchema;
  Key: string;
begin
  if (Kind = fkNone) or (TJtdSchema.GetFormKind(GetSignature) <> Kind) then
    raise EJtdSchemaVerify.Create(SEJtdInvalidForm);
  if Definitions <> nil then begin
    if Self <> aRoot then
      raise EJtdSchemaVerify.Create(SEJtdNonRootDefs);
    for Schema in Definitions.Values do
      Schema.Verify(aRoot);
  end;
  if Ref <> '' then
    if (aRoot.Definitions = nil) or not aRoot.Definitions.Contains(Ref) then
      raise EJtdSchemaVerify.Create(SERefDefNonExistent);
  if Enum <> nil then
    if Enum.IsEmpty then
      raise EJtdSchemaVerify.Create(SEEnumMustNotEmpty);
  if Elements <> nil then
    Elements.Verify(aRoot);
  if Properties <> nil then
    for Schema in Properties.Values do
      Schema.Verify(aRoot);
  if OptionalProperties <> nil then begin
    for Schema in OptionalProperties.Values do
      Schema.Verify(aRoot);
    if Properties <> nil then
      for Key in Properties.Keys do
        if OptionalProperties.Contains(Key) then
          raise EJtdSchemaVerify.Create(SEPropsOptPropsKeyShare);
  end;
  if Values <> nil then
    Values.Verify(aRoot);
  if Mapping <> nil then
    for Schema in Mapping.Values do begin
      Schema.Verify(aRoot);
      if Schema.Nullable then
        raise EJtdSchemaVerify.Create(SEMappingIsNullable);
      if Schema.Kind <> fkProperties then
        raise EJtdSchemaVerify.Create(SEMappingNotPropForm);
      if (Schema.Properties <> nil) and Schema.Properties.Contains(Discriminator) then
        raise EJtdSchemaVerify.Create(SEDiscrMapPropsKeyShare);
      if (Schema.OptionalProperties <> nil) and Schema.OptionalProperties.Contains(Discriminator) then
        raise EJtdSchemaVerify.Create(SEDiscrMapOptPropsKeyShare);
    end;
end;

procedure TJtdSchema.LoadNode(aNode: TJsonNode);
begin
  Clear;
  DoLoadSchema(Self, aNode);
  Verify(Self);
end;

class function TJtdSchema.GetKeyword(const aKey: string; out aValue: TKeyword): Boolean;
begin
  Result := CFKeywords.TryGetValue(aKey, aValue);
end;

class function TJtdSchema.GetFormKind(const aSgn: TSignature): TFormKind;
begin
  if aSgn = [] then exit(fkEmpty) else
    if aSgn = [spRef] then exit(fkRef) else
      if aSgn = [spType] then exit(fkType) else
        if aSgn = [spEnum] then exit(fkEnum) else
          if aSgn = [spElements] then exit(fkElements) else
            if (aSgn = [spProperties])or(aSgn = [spOptionalProperties])or
               (aSgn = [spProperties, spOptionalProperties])or
               (aSgn = [spProperties, spAdditionalProperties])or
               (aSgn = [spProperties, spOptionalProperties, spAdditionalProperties])or
               (aSgn = [spOptionalProperties, spAdditionalProperties]) then exit(fkProperties) else
              if aSgn = [spValues] then exit(fkValues) else
                if aSgn = [spDiscriminator, spMapping] then exit(fkDiscriminator) else
                  Result := fkNone;
end;

class procedure TJtdSchema.DoLoadSchema(aRoot: TJtdSchema; aNode: TJsonNode);
  function JsKind2Str(aValue: TJsValueKind): string;
  begin
    Result := GetEnumName(TypeInfo(TJsValueKind), Integer(aValue));
  end;
  procedure LoadRef(aSchema: TJtdSchema; aNode: TJsonNode); inline;
  begin
    if aSchema.Kind <> fkEmpty then
      raise EJtdSchemaLoad.Create(SEKindAforeAssigned);
    if not aNode.IsString then
      raise EJtdSchemaLoad.CreateFmt(SEJtdRefNotStrFmt, [JsKind2Str(aNode.Kind)]);
    aSchema.FKind := fkRef;
    aSchema.FStrValue := aNode.AsString;
  end;
  procedure LoadType(aSchema: TJtdSchema; aNode: TJsonNode); inline;
  var
    kw: TKeyword;
  begin
    if aSchema.Kind <> fkEmpty then
      raise EJtdSchemaLoad.Create(SEKindAforeAssigned);
    if not aNode.IsString then
      raise EJtdSchemaLoad.CreateFmt(SEJtdRefNotStrFmt, [JsKind2Str(aNode.Kind)]);
    if not GetKeyword(aNode.AsString, kw) then
      raise EJtdSchemaLoad.CreateFmt(SEUnknownKeywordFmt, [aNode.AsString]);
    if not(kw.Kind = kkType) then
      raise EJtdSchemaLoad.CreateFmt(SEIrrelevantFormKwdFmt, [aNode.AsString]);
    aSchema.FKind := fkType;
    aSchema.FType := kw.JType;
  end;
  procedure LoadEnum(aSchema: TJtdSchema; aNode: TJsonNode);
  var
    I: SizeInt;
  begin
    if aSchema.Kind <> fkEmpty then
      raise EJtdSchemaLoad.Create(SEKindAforeAssigned);
    if not aNode.IsArray then
      raise EJtdSchemaLoad.CreateFmt(SEJtdEnumNotArrayFmt, [JsKind2Str(aNode.Kind)]);
    if aNode.Count = 0 then
      raise EJtdSchemaLoad.Create(SEEnumMustNotEmpty);
    aSchema.FVarInst := TStringSet.Create;
    for I := 0 to Pred(aNode.Count) do begin
      if not aNode.Items[I].IsString then
        raise EJtdSchemaLoad.CreateFmt(SEEnumElemNotStrFmt, [JsKind2Str(aNode.Items[I].Kind)]);
      if not TStringSet(aSchema.FVarInst).Add(aNode.Items[I].AsString) then
        raise EJtdSchemaLoad.CreateFmt(SENonUniqEnumElemFmt, [aNode.Items[I].AsString]);
    end;
    aSchema.FKind := fkEnum;
  end;
  procedure LoadSchema(aSchema: TJtdSchema; aNode: TJsonNode); forward;
  procedure LoadElements(aSchema: TJtdSchema; aNode: TJsonNode);
  begin
    if aSchema.Kind <> fkEmpty then
      raise EJtdSchemaLoad.Create(SEKindAforeAssigned);
    aSchema.FVarInst := TJtdSchema.Create;
    aSchema.FKind := fkElements;
    LoadSchema(TJtdSchema(aSchema.FVarInst), aNode);
  end;
  procedure LoadProps(aSchema: TJtdSchema; aNode: TJsonNode);
  var
    p: TJsonNode.TPair;
    s: TJtdSchema;
  begin
    if not (aSchema.Kind in [fkEmpty, fkProperties]) then
      raise EJtdSchemaLoad.Create(SEKindAforeAssigned);
    if not aNode.IsObject then
      raise EJtdSchemaLoad.CreateFmt(SEPropsNotObjectFmt, [JsKind2Str(aNode.Kind)]);
    aSchema.FKind := fkProperties;
    aSchema.FProps := TJtdSchemaMap.Create;
    for p in aNode.Enrties do begin
      s := TJtdSchema.Create;
      aSchema.FProps.Add(p.Key, s);
      LoadSchema(s, p.Value);
    end;
  end;
  procedure LoadOptProps(aSchema: TJtdSchema; aNode: TJsonNode);
  var
    p: TJsonNode.TPair;
    s: TJtdSchema;
  begin
    if not (aSchema.Kind in [fkEmpty, fkProperties]) then
      raise EJtdSchemaLoad.Create(SEKindAforeAssigned);
    if not aNode.IsObject then
      raise EJtdSchemaLoad.CreateFmt(SEPropsNotObjectFmt, [JsKind2Str(aNode.Kind)]);
    aSchema.FKind := fkProperties;
    aSchema.FVarInst := TJtdSchemaMap.Create;
    for p in aNode.Enrties do begin
      s := TJtdSchema.Create;
      TJtdSchemaMap(aSchema.FVarInst).Add(p.Key, s);
      LoadSchema(s, p.Value);
    end;
  end;
  procedure LoadAddProps(aSchema: TJtdSchema; aNode: TJsonNode); inline;
  begin
    if not (aSchema.Kind in [fkEmpty, fkProperties]) then
      raise EJtdSchemaLoad.Create(SEKindAforeAssigned);
    if not aNode.IsBoolean then
      raise EJtdSchemaLoad.CreateFmt(SEAddtPropsNotBoolFmt, [JsKind2Str(aNode.Kind)]);
    aSchema.FKind := fkProperties;
    aSchema.FAddProps := aNode.AsBoolean;
  end;
  procedure LoadValues(aSchema: TJtdSchema; aNode: TJsonNode);
  begin
    if aSchema.Kind <> fkEmpty then
      raise EJtdSchemaLoad.Create(SEKindAforeAssigned);
    if not aNode.IsObject then
      raise EJtdSchemaLoad.CreateFmt(SEValuesNotObjectFmt, [JsKind2Str(aNode.Kind)]);
    aSchema.FKind := fkValues;
    aSchema.FVarInst := TJtdSchema.Create;
    LoadSchema(TJtdSchema(aSchema.FVarInst), aNode);
  end;
  procedure LoadDiscr(aSchema: TJtdSchema; aNode: TJsonNode); inline;
  begin
    if not (aSchema.Kind in [fkEmpty, fkDiscriminator]) then
      raise EJtdSchemaLoad.Create(SEKindAforeAssigned);
    if not aNode.IsString then
      raise EJtdSchemaLoad.CreateFmt(SEDiscriminNotStrFmt, [JsKind2Str(aNode.Kind)]);
    aSchema.FKind := fkDiscriminator;
    aSchema.FStrValue := aNode.AsString;
  end;
  procedure LoadMapping(aSchema: TJtdSchema; aNode: TJsonNode);
  var
    p: TJsonNode.TPair;
    s: TJtdSchema;
  begin
    if not (aSchema.Kind in [fkEmpty, fkDiscriminator]) then
      raise EJtdSchemaLoad.Create(SEKindAforeAssigned);
     if not aNode.IsObject then
      raise EJtdSchemaLoad.CreateFmt(SEMappingNotObjectFmt, [JsKind2Str(aNode.Kind)]);
     aSchema.FKind := fkDiscriminator;
     aSchema.FVarInst := TJtdSchemaMap.Create;
     for p in aNode.Enrties do begin
       s := TJtdSchema.Create;
       TJtdSchemaMap(aSchema.FVarInst).Add(p.Key, s);
       LoadSchema(s, p.Value);
     end;
  end;
  procedure LoadDefs(aSchema: TJtdSchema; aNode: TJsonNode);
  var
    p: TJsonNode.TPair;
    s: TJtdSchema;
  begin
    if aSchema <> aRoot then
      raise EJtdSchemaLoad.Create(SENonRootSchemaDefs);
    if not aNode.IsObject then
      raise EJtdSchemaLoad.CreateFmt(SEDefsNotObjectFmt, [JsKind2Str(aNode.Kind)]);
    aSchema.FDefs := TJtdSchemaMap.Create;
    for p in aNode.Enrties do begin
      s := TJtdSchema.Create;
      aSchema.FDefs.Add(p.Key, s);
      LoadSchema(s, p.Value);
    end;
  end;
  procedure LoadNullable(aSchema: TJtdSchema; aNode: TJsonNode); inline;
  begin
    if not aNode.IsBoolean then
      raise EJtdSchemaLoad.CreateFmt(SENullableNotBoolFmt, [JsKind2Str(aNode.Kind)]);
    aSchema.FNullable := aNode.AsBoolean;
  end;
  procedure LoadMeta(aSchema: TJtdSchema; aNode: TJsonNode);
  var
    p: TJsonNode.TPair;
  begin
    if not aNode.IsObject then
      raise EJtdSchemaLoad.CreateFmt(SEMetaNotObjectFmt, [JsKind2Str(aNode.Kind)]);
    aSchema.FMetadata := TMetaDataMap.Create([moOwnsValues]);
    for p in aNode.Enrties do
      aSchema.FMetadata.Add(p.Key, p.Value.Clone);
  end;
  procedure LoadSchema(aSchema: TJtdSchema; aNode: TJsonNode);
  var
    p: TJsonNode.TPair;
    kw: TKeyword;
  begin
    if not aNode.IsObject then
      raise EJtdSchemaLoad.CreateFmt(SESchemaNotObjectFmt, [JsKind2Str(aNode.Kind)]);
    aSchema.FKind := fkEmpty;
    if aNode.Count = 0 then exit;
    for p in aNode.Enrties do begin
      if not GetKeyword(p.Key, kw) then
        raise EJtdSchemaLoad.CreateFmt(SEUnknownKeywordFmt, [p.Key]);
      if not(kw.Kind = kkProp) then
        raise EJtdSchemaLoad.CreateFmt(SEUnknownSchemaPropFmt, [p.Key]);
      case kw.SProp of
        spRef:                  LoadRef(aSchema, p.Value);
        spType:                 LoadType(aSchema, p.Value);
        spEnum:                 LoadEnum(aSchema, p.Value);
        spElements:             LoadElements(aSchema, p.Value);
        spProperties:           LoadProps(aSchema, p.Value);
        spOptionalProperties:   LoadOptProps(aSchema, p.Value);
        spAdditionalProperties: LoadAddProps(aSchema, p.Value);
        spValues:               LoadValues(aSchema, p.Value);
        spDiscriminator:        LoadDiscr(aSchema, p.Value);
        spMapping:              LoadMapping(aSchema, p.Value);
        spDefinitions:          LoadDefs(aSchema, p.Value);
        spNullable:             LoadNullable(aSchema, p.Value);
        spMetaData:             LoadMeta(aSchema, p.Value);
      end;
    end;
  end;
begin
  LoadSchema(aRoot, aNode);
end;

class function TJtdSchema.TryLoad(aJson: TJsonNode; out aSchema: TJtdSchema): Boolean;
var
  Schema: TJtdSchema = nil;
begin
  Result := False;
  aSchema := nil;
  Schema := TJtdSchema.Create;
  try
    Schema.Load(aJson);
    aSchema := Schema;
    Result := True;
  except
    Schema.Free;
  end;
end;

class function TJtdSchema.TryLoad(const aJson: string; out aSchema: TJtdSchema): Boolean;
var
  Schema: TJtdSchema = nil;
begin
  Result := False;
  aSchema := nil;
  Schema := TJtdSchema.Create;
  try
    Schema.Load(aJson);
    aSchema := Schema;
    Result := True;
  except
    Schema.Free;
  end;
end;

class function TJtdSchema.TryLoadFromStream(aStream: TStream; out aSchema: TJtdSchema): Boolean;
var
  Schema: TJtdSchema = nil;
begin
  Result := False;
  aSchema := nil;
  Schema := TJtdSchema.Create;
  try
    Schema.LoadFromStream(aStream);
    aSchema := Schema;
    Result := True;
  except
    Schema.Free;
  end;
end;

class function TJtdSchema.TryLoadFromFile(const aFileName: string; out aSchema: TJtdSchema): Boolean;
var
  Schema: TJtdSchema = nil;
begin
  Result := False;
  aSchema := nil;
  Schema := TJtdSchema.Create;
  try
    Schema.LoadFromFile(aFileName);
    aSchema := Schema;
    Result := True;
  except
    Schema.Free;
  end;
end;

destructor TJtdSchema.Destroy;
begin
  FDefs.Free;
  FProps.Free;
  FVarInst.Free;
  FMetadata.Free;
  inherited;
end;

procedure TJtdSchema.Clear;
begin
  FreeAndNil(FDefs);
  FreeAndNil(FProps);
  FreeAndNil(FVarInst);
  FreeAndNil(FMetadata);
  FStrValue := '';
  FKind := fkNone;
  FType := jtNone;
  FNullable := False;
  FAddProps := False;
end;

procedure TJtdSchema.Load(aJson: TJsonNode);
begin
  if not TJsonNode.DupeNamesFree(aJson) then
    raise EJtdSchemaLoad.Create(SESchemaKeysNotUniq);
  LoadNode(aJson);
end;

procedure TJtdSchema.Load(const aJson: string; aMaxDepth: Integer);
var
  Node: TJsonNode = nil;
begin
  if not TJsonNode.TryParse(aJson, Node, aMaxDepth) then
    raise EJtdSchemaLoad.Create(SECantParseJsStr);
  try
    if not TJsonNode.DupeNamesFree(Node) then
      raise EJtdSchemaLoad.Create(SESchemaKeysNotUniq);
    LoadNode(Node);
  finally
    Node.Free;
  end;
end;

procedure TJtdSchema.LoadFromStream(aStream: TStream; aMaxDepth: Integer);
var
  Node: TJsonNode = nil;
begin
  if not TJsonNode.TryParse(aStream, Node, aMaxDepth) then
    raise EJtdSchemaLoad.Create(SECantLoadJsStream);
  try
    if not TJsonNode.DupeNamesFree(Node) then
      raise EJtdSchemaLoad.Create(SESchemaKeysNotUniq);
    LoadNode(Node);
  finally
    Node.Free;
  end;
end;

procedure TJtdSchema.LoadFromFile(const aFileName: string; aMaxDepth: Integer);
var
  Node: TJsonNode = nil;
begin
  if not TJsonNode.TryParseFile(aFileName, Node, aMaxDepth) then
    raise EJtdSchemaLoad.Create(SECantLoadJsFile);
  try
    if not TJsonNode.DupeNamesFree(Node) then
      raise EJtdSchemaLoad.Create(SESchemaKeysNotUniq);
    LoadNode(Node);
  finally
    Node.Free;
  end;
end;

{ TValidateError }

class function TValidateError.Equal(const L, R: TValidateError): Boolean;
begin
  Result := (L.InstancePath = R.InstancePath) and (L.SchemaPath = R.SchemaPath);
end;

class function TValidateError.HashCode(const aValue: TValidateError): SizeInt;
begin
  Result := TxxHash32LE.HashStr(aValue.SchemaPath, DWord(TxxHash32LE.HashStr(aValue.InstancePath)));
end;

constructor TValidateError.Make(const aInstPath, aSchemaPath: array of string);
begin
  FInstPath := TJsonPtr.ToPointer(aInstPath);
  FSchemaPath := TJsonPtr.ToPointer(aSchemaPath);
end;

{ TErrorListHelper }

{$PUSH}{$WARN 5089 OFF}
function TErrorListHelper.AsJson: string;
var
  WriterRef: specialize TGUniqRef<TJsonStrWriter>;
  Writer: TJsonStrWriter;
  I: SizeInt;
begin
  WriterRef.Instance := TJsonStrWriter.Create(0);
  Writer := WriterRef;
  Writer.BeginArray;
    for I := 0 to System.High(Self) do
      Writer
        .BeginObject
           .Add(TValidateError.INSTANCE_PATH, Self[I].InstancePath)
           .Add(TValidateError.SCHEMA_PATH, Self[I].SchemaPath)
        .EndObject;
  Writer.EndArray;
  Result := Writer.JsonString;
end;
{$POP}

type
  TStrList        = specialize TGLiteVector<string>;
  TSchemaList     = specialize TGLiteVector<TStrList>;
  EMaxDepthExceed = class(Exception);
  EMaxErrorExceed = class(Exception);

function Validate(aInstance: TJsonNode; aSchema: TJtdSchema; out aErrList: TJtdErrorList; aMaxErrors: Integer;
  aMaxDepth: Integer): TJtdValidateResult;
var
  InstancePath: TStrList;
  SchemaPath: TSchemaList;
  ErrorList: TJtdErrorList = nil;
  ErrListPos: SizeInt = 0;
  Root: TJtdSchema = nil;
  procedure PushError;
  var
    pList: ^TStrList;
  begin
    if ErrListPos = aMaxErrors then raise EMaxErrorExceed.Create('');
    pList := SchemaPath.UncMutable[Pred(SchemaPath.Count)];
    ErrorList[ErrListPos] := TValidateError.Make(
      InstancePath.UncMutable[0][0..Pred(InstancePath.Count)],
      pList^.UncMutable[0][0..Pred(pList^.Count)]);
    Inc(ErrListPos);
  end;
  procedure SchemaPathPushRoot; inline;
  begin
    if SchemaPath.Count = aMaxDepth then raise EMaxDepthExceed.Create('');
    SchemaPath.Add(Default(TStrList));
  end;
  procedure SchemaPathPush(const aPart: string); inline;
  begin
    SchemaPath.UncMutable[Pred(SchemaPath.Count)]^.Add(aPart);
  end;
  procedure SchemaPathPop; inline;
  begin
    SchemaPath.UncMutable[Pred(SchemaPath.Count)]^.DeleteLast;
  end;
  procedure DoValidate(aInst: TJsonNode; aSchema: TJtdSchema; const aParentTag: string = ''); forward;
  procedure DoRef(aInst: TJsonNode; aSchema: TJtdSchema);
  begin
    SchemaPathPushRoot;
    SchemaPathPush(aSchema.JTD_PROPS[spDefinitions]);
    SchemaPathPush(aSchema.Ref);
    DoValidate(aInst, Root.Definitions[aSchema.Ref]);
    SchemaPath.DeleteLast;
  end;
  procedure DoType(aInst: TJsonNode; aSchema: TJtdSchema);
  var
    d: Double;
    I: Int64;
    Typ: TJtdSchema.TJtdType;
  begin
    SchemaPathPush(aSchema.JTD_PROPS[spType]);
    Typ := aSchema.ElType;
    case Typ of
      jtBool: if not aInst.IsBoolean then PushError;
      jtFloat32, jtFloat64:
        if not aInst.IsNumber then PushError else
          if Typ = jtFloat32 then begin
            d := aInst.AsNumber;
            if (d < Math.MinSingle) or (d > Math.MaxSingle) then PushError;
          end;
      jtInt8, jtUInt8, jtInt16, jtUInt16, jtInt32, jtUInt32:
        if not aInst.IsNumber then PushError else
          if not lgJson.IsExactInt(aInst.AsNumber, I) then PushError else
            case Typ of
              jtInt8:   if (I < System.Low(ShortInt)) or (I > System.High(ShortInt)) then PushError;
              jtUInt8:  if (I < 0) or (I > System.High(Byte)) then PushError;
              jtInt16:  if (I < System.Low(SmallInt)) or (I > System.High(SmallInt)) then PushError;
              jtUInt16: if (I < 0) or (I > System.High(Word)) then PushError;
              jtInt32:  if (I < System.Low(LongInt)) or (I > System.High(LongInt)) then PushError;
              jtUInt32: if (I < 0) or (I > System.High(DWord)) then PushError;
            else
              raise EJtdSchemaValidate.CreateFmt(SEJtdValidationInternalFmt, [1]);
            end;
      jtString: if not aInst.IsString then PushError;
      jtTimeStamp:
        if not aInst.IsString then PushError
        else
          if not IsRfc8927TimeStamp(aInst.AsString) then PushError;//todo: need replacement ???
    else
      raise EJtdSchemaValidate.CreateFmt(SEJtdValidationInternalFmt, [2]);
    end;
    SchemaPathPop;
  end;
  procedure DoEnum(aInst: TJsonNode; aSchema: TJtdSchema);
  begin
    SchemaPathPush(aSchema.JTD_PROPS[spEnum]);
    if not aInst.IsString or (aSchema.Enum = nil) then
      PushError
    else
      if not aSchema.Enum.Contains(aInst.AsString) then
        PushError;
    SchemaPathPop;
  end;
  procedure DoElements(aInst: TJsonNode; aSchema: TJtdSchema);
  var
    I: SizeInt;
  begin
    SchemaPathPush(aSchema.JTD_PROPS[spElements]);
    if not aInst.IsArray then PushError else
      for I := 0 to Pred(aInst.Count) do begin
        InstancePath.Add(IntToStr(I));
        DoValidate(aInst.Items[I], aSchema.Elements);
        InstancePath.DeleteLast;
      end;
    SchemaPathPop;
  end;
  procedure DoProps(aInst: TJsonNode; aSchema: TJtdSchema; const aParentTag: string = '');
  var
    e: TJtdSchemaMap.TEntry;
    Node: TJsonNode;
    Prop: string;
  begin
    if aInst.IsObject then begin
      if aSchema.Properties <> nil then begin
        SchemaPathPush(aSchema.JTD_PROPS[spProperties]);
        for e in aSchema.Properties do begin
          SchemaPathPush(e.Key);
          if aInst.Find(e.Key, Node) then begin
            InstancePath.Add(e.Key);
            DoValidate(Node, e.Value);
            InstancePath.DeleteLast;
          end else PushError;
          SchemaPathPop;
        end;
        SchemaPathPop;
      end;
      if aSchema.OptionalProperties <> nil then begin
        SchemaPathPush(aSchema.JTD_PROPS[spOptionalProperties]);
        for e in aSchema.OptionalProperties do begin
          SchemaPathPush(e.Key);
          if aInst.Find(e.Key, Node) then begin
            InstancePath.Add(e.Key);
            DoValidate(Node, e.Value);
            InstancePath.DeleteLast;
          end;
          SchemaPathPop;
        end;
        SchemaPathPop;
      end;
      if not aSchema.AdditionalProperties then
        for Prop in aInst.Names do
          if not(((aSchema.Properties <> nil) and (aSchema.Properties.Contains(Prop))) or
                 ((aSchema.OptionalProperties <> nil) and (aSchema.OptionalProperties.Contains(Prop))) or
                 (Prop = aParentTag)) then begin
            InstancePath.Add(Prop);
            PushError;
            InstancePath.DeleteLast;
          end;
    end else begin
      if aSchema.Properties = nil then
        SchemaPathPush(aSchema.JTD_PROPS[spOptionalProperties])
      else
        SchemaPathPush(aSchema.JTD_PROPS[spProperties]);
      PushError;
      SchemaPathPop;
    end;
  end;
  procedure DoValues(aInst: TJsonNode; aSchema: TJtdSchema);
  var
    p: TJsonNode.TPair;
  begin
    SchemaPathPush(aSchema.JTD_PROPS[spValues]);
    if aInst.IsObject then
      for p in aInst.Enrties do begin
        InstancePath.Add(p.Key);
        DoValidate(p.Value, aSchema.Values);
        InstancePath.DeleteLast;
      end
    else PushError;
    SchemaPathPop;
  end;
  procedure DoDiscr(aInst: TJsonNode; aSchema: TJtdSchema);
  var
    Node: TJsonNode = nil;
    DiscrKey: string;
  begin
    if aInst.IsObject then begin
      if aInst.Find(aSchema.Discriminator, Node) then begin
        if Node.IsString then begin
          DiscrKey := Node.AsString;
          if aSchema.Mapping.Contains(DiscrKey) then begin
            SchemaPathPush(aSchema.JTD_PROPS[spMapping]);
            SchemaPathPush(DiscrKey);
            DoValidate(aInst, aSchema.Mapping[DiscrKey], aSchema.Discriminator);
            SchemaPathPop;
            SchemaPathPop;
          end else begin
            SchemaPathPush(aSchema.JTD_PROPS[spMapping]);
            InstancePath.Add(aSchema.Discriminator);
            PushError;
            InstancePath.DeleteLast;
            SchemaPathPop;
          end;
        end else begin
          SchemaPathPush(aSchema.JTD_PROPS[spDiscriminator]);
          InstancePath.Add(aSchema.Discriminator);
          PushError;
          InstancePath.DeleteLast;
          SchemaPathPop;
        end;
      end else begin
        SchemaPathPush(aSchema.JTD_PROPS[spDiscriminator]);
        PushError;
        SchemaPathPop;
      end;
    end else begin
      SchemaPathPush(aSchema.JTD_PROPS[spDiscriminator]);
      PushError;
      SchemaPathPop;
    end;
  end;
  procedure DoValidate(aInst: TJsonNode; aSchema: TJtdSchema; const aParentTag: string = '');
  begin
    if aSchema.Nullable and aInst.IsNull then exit;
    case aSchema.Kind of
      fkEmpty:         ;
      fkRef:           DoRef(aInst, aSchema);
      fkType:          DoType(aInst, aSchema);
      fkEnum:          DoEnum(aInst, aSchema);
      fkElements:      DoElements(aInst, aSchema);
      fkProperties:    DoProps(aInst, aSchema, aParentTag);
      fkValues:        DoValues(aInst, aSchema);
      fkDiscriminator: DoDiscr(aInst, aSchema);
    else
      raise EJtdSchemaValidate.CreateFmt(SEJtdValidationInternalFmt, [3]);
    end;
  end;
begin
  aErrList := nil;
  if not TJsonNode.DupeNamesFree(aInstance) then exit(jvrNonUniqKeys);
  if (aMaxErrors < 1) or (aMaxDepth < 1) then exit(jrvInvalidParam);
  Result := jvrOk;
  System.SetLength(ErrorList, aMaxErrors);
  Root := aSchema;
  SchemaPathPushRoot;
  try
    DoValidate(aInstance, aSchema);
  except
    on EMaxDepthExceed do Result := jvrMaxDepthExceed;
    on EMaxErrorExceed do Result := jvrMaxErrorsExceed;
    on Exception do raise;
  end;
  System.SetLength(ErrorList, ErrListPos);
  aErrList := ErrorList;
  if (Result = jvrOk) and (aErrList <> nil) then
    Result := jvrErrors;
end;

function DaysPerMonth(aMonth: Word; aLeapYear: Boolean): Word; inline;
begin
  case aMonth of
    1, 3, 5, 7, 8, 10, 12:
      Result := 31;
    2:
      if aLeapYear then
        Result := 29
      else Result := 28;
    4, 6, 9, 11:
      Result := 30;
  else
    Result := 0;
  end;
end;

function RealLeapSecond06(aYear: Word): Boolean; inline;
begin
  case aYear of
    1972, 1981, 1982, 1983, 1985, 1992, 1993, 1994, 1997, 2012, 2015:
      Result := True;
  else
    Result := False;
  end;
end;

function RealLeapSecond12(aYear: Word): Boolean; inline;
begin
  case aYear of
    1972, 1973, 1974, 1975, 1976, 1977, 1978, 1979,
    1987, 1989, 1990, 1995, 1998, 2005, 2008, 2016:
      Result := True;
  else
    Result := False;
  end;
end;

type
  TDtFragments = record
    Year,
    Month,
    MDay,
    Hour,
    Minute,
    Second: Word;
    TZOffset: SmallInt; // local offset in minutes
    SecFraction: array[0..7] of Char;// decimal digits of the fractional part of a second
                                     // in the order in which they appear
  end;

function ValidTimeStamp(p: PChar; aCount: SizeInt; out aFrags: TDtFragments): Boolean;
var
  I, J: Integer;
  Val: Word;
const
  Digits: array['0'..'9'] of Word = (0,1,2,3,4,5,6,7,8,9);
begin
  if aCount < 20 then exit(False);
// Year
  if p[0] in ['0'..'9'] then Val := Digits[p[0]] else exit(False);
  if p[1] in ['0'..'9'] then Val := Val*10 + Digits[p[1]] else exit(False);
  if p[2] in ['0'..'9'] then Val := Val*10 + Digits[p[2]] else exit(False);
  if p[3] in ['0'..'9'] then Val := Val*10 + Digits[p[3]] else exit(False);
  aFrags.Year := Val;
  //////////////
  if p[4] <> '-' then exit(False);
  //////////////
// Month
  if p[5] in ['0'..'1'] then Val := Digits[p[5]] else exit(False);
  if p[6] in ['0'..'9'] then Val := Val*10 + Digits[p[6]] else exit(False);
  if (Val = 0) or (Val > 12) then exit(False);
  aFrags.Month := Val;
  /////////////
  if p[7] <> '-' then exit(False);
  /////////////
// MDay
  if p[8] in ['0'..'3'] then Val := Digits[p[8]] else exit(False);
  if p[9] in ['0'..'9'] then Val := Val*10 + Digits[p[9]] else exit(False);
  if (Val = 0) or (Val > DaysPerMonth(aFrags.Month, IsLeapYear(aFrags.Year))) then exit(False);
  aFrags.MDay := Val;
  /////////////
  if p[10] <> 'T' then exit(False);
  /////////////
// Hour
  if p[11] in ['0'..'2'] then Val := Digits[p[11]] else exit(False);
  if p[12] in ['0'..'9'] then Val := Val*10 + Digits[p[12]] else exit(False);
  if Val > 23 then exit(False);
  aFrags.Hour := Val;
  ///////////////
  if p[13] <> ':' then exit(False);
  ///////////////
// Minute
  if p[14] in ['0'..'5'] then Val := Digits[p[14]] else exit(False);
  if p[15] in ['0'..'9'] then Val := Val*10 + Digits[p[15]] else exit(False);
  if Val > 59 then exit(False);
  aFrags.Minute := Val;
  ///////////////
  if p[16] <> ':' then exit(False);
  ///////////////
// Second
  if p[17] in ['0'..'6'] then Val := Digits[p[17]] else exit(False);
  if p[18] in ['0'..'9'] then Val := Val*10 + Digits[p[18]] else exit(False);
  if Val > 60 then exit(False);
  if Val = 60 then begin //positive leap second?
    if (aFrags.Year > 2016) or (aFrags.Hour <> 23) or (aFrags.Minute <> 59) then exit(False);
    if aFrags.Month = 6 then begin
      if aFrags.MDay <> 30 then exit(False);
      if not RealLeapSecond06(aFrags.Year) then exit(False);
    end else
      if aFrags.Month = 12 then begin
        if aFrags.MDay <> 31 then exit(False);
        if not RealLeapSecond12(aFrags.Year) then exit(False);
      end else exit(False);
  end;
  aFrags.Second := Val;
  ////////////////////
  FillChar(aFrags.SecFraction, SizeOf(aFrags.SecFraction), '0');
  aFrags.TZOffset := 0;
  ///////////////////
  I := 19;
  if p[I] = '.' then begin // maybe second fraction
    if I = Pred(aCount) then exit(False);
    Inc(I);
    J := 0;
    while (I < aCount) and (p[I] in ['0'..'9']) do begin
      if J < System.Length(aFrags.SecFraction[J]) then begin
        aFrags.SecFraction[J] := p[I];
        Inc(J);
      end;
      Inc(I);
    end;
  end;

  if I = aCount then exit(False)
  else
    if (I = Pred(aCount)) and (p[I] = 'Z') then exit(True);

  if not (p[I] in ['+', '-']) then exit(False);
  Inc(I);
  if aCount - I <> 5 then exit(False);
//local offset
  if p[I] in ['0'..'9'] then Val := Digits[p[I]] else exit(False);
  if p[I+1] in ['0'..'9'] then Val := Val*10 + Digits[p[I+1]] else exit(False);
  if Val > 23 then exit(False);
  aFrags.TZOffset := Integer(Val) * 60;
  if p[I+2] <> ':' then exit(False);
  if p[I+3] in ['0'..'9'] then Val := Digits[p[I+3]] else exit(False);
  if p[I+4] in ['0'..'9'] then Val := Val*10 + Digits[p[I+4]] else exit(False);
  if Val > 59 then exit(False);
  aFrags.TZOffset += Integer(Val);

  if p[I-1] = '-' then
    aFrags.TZOffset := -aFrags.TZOffset;

  Result := True;
end;

function IsRfc8927TimeStamp(const s: string): Boolean;
begin
  Result := IsRfc8927TimeStamp(Pointer(s), System.Length(s));
end;

function IsRfc8927TimeStamp(p: PChar; aCount: SizeInt): Boolean;
var
  Fragments: TDtFragments;
begin
  while (aCount > 0) and (p^ in [#9, ' ']) do begin
    Inc(p);
    Dec(aCount);
  end;

  while (aCount > 0) and (p[Pred(aCount)] in [#9, ' ']) do
    Dec(aCount);

  Result := ValidTimeStamp(p, aCount, Fragments);
end;

end.

