{****************************************************************************
*                                                                           *
*   This file is part of the LGenerics package.                             *
*   Generating JTD schemas from JSON examples.                              *
*                                                                           *
*   Copyright(c) 2023-2024 A.Koverdyaev(avk)                                *
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
unit lgJtdInfer;

{$MODE OBJFPC}{$H+}{$MODESWITCH ADVANCEDRECORDS}

interface

uses
  SysUtils, Math,
  lgArrayHelpers,
  lgVector,
  lgMultiMap,
  lgJson,
  lgJsonTypeDef;

type
  { TJtdInferrer: tries to generate a JSON Typedef schema from the example data;
    by default TJtdInferrer will never generate Enum, Values, or Discriminator
    schemas, if the result can be improved with an Enum, Values, or Discriminator schema,
    just give it appropriate hints }
  TJtdInferrer = class
  public
  type

    TNumberType = (ntNone, ntInt8, ntUInt8, ntInt16, ntUInt16, ntInt32, ntUInt32, ntFloat32, ntFloat64);

    THintKind = (
      hkUnknown,
      hkDefNumberType,  { suggests the default numeric type in the THint.NumberType field;
                          if no THint.Path/JsonPath is specified, the hint refers to the whole sample,
                          otherwise, only to the specified element;
                          by default TJtdInferrer will guess the narrowest number type }
      hkUseEnum,        { hints that the string value pointed to by THint.Path/JsonPath is an element
                          of an enumeration; the complete list of elements may be specified
                          in the THint.EnumList field }
      hkUseMap,         { suggests treating the JSON object pointed to by the THint.Path/JsonPath
                          as a Dictionary(Values form); by default TJtdInferrer will treat the JSON object
                          as a Record(Properties form) }
      hkUseVariant,     { suggests treating the JSON object whose element is pointed to by THint.Path as
                          a Tagged Union(or Variant, if you prefer) for which this element is a discriminator;
                          specified element must be of type String }
      hkAdditionalProps,{ hints that the JSON object pointed to by THint.Path/JsonPath can have additional properties }
      hkNullable        { hints that the element pointed to by THint.Path/JsonPath is nullable }
    );

    THint = record
      Kind: THintKind;
      NumberType: TNumberType;
      EnumList,
      Path: TStringArray;
      JsonPath: string; //if the JsonPath string is not empty, it has a higher priority than Path
      constructor Make(aDefNumType: TNumberType);
      constructor Make(const aPath: TStringArray; aDefNumType: TNumberType);
      constructor Enum(const aPath: TStringArray);
      constructor Enum(const aEnumList, aPath: TStringArray);
      constructor Map(const aPath: TStringArray);
      constructor Variant(const aPath: TStringArray);
      constructor AddProps(const aPath: TStringArray);
      constructor Nullable(const aPath: TStringArray);
    { in all constructors declared below, aPath must be a valid JSONPath expression }
      constructor JpMake(const aPath: string; aDefNumType: TNumberType);
      constructor JpEnum(const aPath: string);
      constructor JpEnum(const aEnumList: TStringArray; const aPath: string);
      constructor JpMap(const aPath: string);
      constructor JpAddProps(const aPath: string);
      constructor JpNullable(const aPath: string);
    end;

  const
    DEFAULT_NUM_TYPE = ntInt8;
    DEF_DEPTH        = TJsonNode.DEF_DEPTH;

  private
  type
    TFormKind = TJtdSchema.TFormKind;
    PHint     = ^THint;
    THintMap  = specialize TGLiteHashMultiMap<TJsonNode, PHint, TObject>;
    THintList = specialize TGLiteVector<THint>;
    TNodeList = specialize TGLiteVector<TJsonNode>;
    TStrType  = (stNone, stString, stTimeStamp);

    TNumType = record
      MinValue,
      MaxValue: Double;
      IsInt: Boolean;
    end;

  const
  {$PUSH}{$J-}
    BOOL_KEY: string      = 'boolean';
    TYPE_KEY: string      = 'type';
    ENUM_KEY: string      = 'enum';
    ELEMS_KEY: string     = 'elements';
    PROPS_KEY: string     = 'properties';
    OPT_PROPS_KEY: string = 'optionalProperties';
    ADD_PROPS_KEY: string = 'additionalProperties';
    VALUES_KEY: string    = 'values';
    DISCRI_KEY: string    = 'discriminator';
    MAPPING_KEY: string   = 'mapping';
    NULLBL_KEY: string    = 'nullable';
    FORM_KIND_KEY: string = '_JTDFormKind';

    NUM_NAMES: array[TNumberType] of string = (
      '', 'int8', 'uint8', 'int16', 'uint16', 'int32', 'uint32', 'float32', 'float64');

    STR_NAMES: array[TStrType] of string = ('', 'string', 'timestamp');

    NUM_TYPES: array[ntInt8..ntFloat64] of TNumType = (
      (MinValue:        -128; MaxValue:        127; IsInt: True),
      (MinValue:           0; MaxValue:        255; IsInt: True),
      (MinValue:      -32768; MaxValue:      32767; IsInt: True),
      (MinValue:           0; MaxValue:      65535; IsInt: True),
      (MinValue: -2147483648; MaxValue: 2147483647; IsInt: True),
      (MinValue:           0; MaxValue: 4294967295; IsInt: True),
      (MinValue:  -MaxSingle; MaxValue:  MaxSingle; IsInt: False),
      (MinValue:  -MaxDouble; MaxValue:  MaxDouble; IsInt: False)
    );
  {$POP}
  var
    FSampleList: TStringArray;
    FNodeList: TNodeList;
    FHintList: THintList;
    FHintMap: THintMap;
    FDefNumType: TNumberType;
    FMaxDepth: Integer;
    FBom: Boolean;
    class function  GetNumType(const aValue: string): TNumberType; static; inline;
    class function  ContainsNum(aValue: Double; aType: TNumberType): Boolean; static; inline;
  { returns True if type L contains type R }
    class function  ContainsType(L, R: TNumberType): Boolean; static; inline;
    class function  RefineNumType(aValue: Double; aCurrType: TNumberType): TNumberType; static;
    class function  GetStrType(const aValue: string): TStrType; static; inline;
    class function  RefineStrType(const aValue: string; aCurrType: TStrType = stNone): TStrType; static; inline;
    class function  GetFormKind(aNode: TJsonNode): TFormKind; static; inline;
    class procedure SetFormKind(aNode: TJsonNode; aKind: TFormKind); static; inline;
    class procedure MovePropToOptional(aNode: TJsonNode; const aProp: string); static;
    class procedure CorrectEnumList(aNode: TJsonNode); static;
    class procedure ClearAssistInfo(aNode: TJsonNode); static;
    procedure DoInferBool(aSchema, aSample: TJsonNode);
    procedure DoInferNumber(aSchema, aSample: TJsonNode);
    procedure DoInferString(aSchema, aSample: TJsonNode);
    procedure DoInitEnum(aSchema, aSample: TJsonNode; aHint: PHint);
    procedure DoInferArray(aSchema, aSample: TJsonNode);
    procedure DoInferObject(aSchema, aSample: TJsonNode);
    procedure DoInitMap(aSchema, aSample: TJsonNode);
    procedure DoInferMap(aSchema, aSample: TJsonNode);
    function  DoInitVariant(aSchema, aSample: TJsonNode; aHint: PHint): TJsonNode;
    procedure DoInferVariant(aSchema, aSample: TJsonNode);
    procedure DoInitProps(aSchema, aSample: TJsonNode; AddProps: Boolean);
    procedure DoInferProps(aSchema, aSample: TJsonNode; AddProps: Boolean);
    procedure DoInfer(aSchema, aSample: TJsonNode);
    procedure CollectHints(const aHints: array of THint);
    property  DefaultNumType: TNumberType read FDefNumType;
  public
  { aSamples specifies a set of examples as JSON values(i.e. as strings);
    returns the resulting schema as a DOM structure }
    class function Infer(const aSamples: array of string; const aHints: array of THint;
                         aSkipBom: Boolean = False; aMaxDepth: Integer = DEF_DEPTH): TJsonNode;
  { aSamples specifies a set of examples as DOM structures;
    returns the resulting schema as a DOM structure }
    class function Infer(const aSamples: array of TJsonNode; const aHints: array of THint): TJsonNode;
  { aSamples specifies a set of examples as JSON values(i.e. as strings);
    returns the resulting schema as a JSON value(i.e. as text) }
    class function InferJson(const aSamples: array of string; const aHints: array of THint;
                             aSkipBom: Boolean = False; aMaxDepth: Integer = DEF_DEPTH): string;
  { aSamples specifies a set of examples as DOM structures;
    returns the resulting schema as a JSON value(i.e. as text) }
    class function InferJson(const aSamples: array of TJsonNode; const aHints: array of THint): string;
    constructor Create(const aSamples: array of string; const aHints: array of THint);
    constructor Create(const aSamples: array of TJsonNode; const aHints: array of THint);
    destructor Destroy; override;
    function Execute: TJsonNode;
    property MaxDepth: Integer read FMaxDepth write FMaxDepth;
    property SkipBom: Boolean read FBom write FBom;
  end;

implementation
{$B-}{$COPERATORS ON}{$POINTERMATH ON}

uses
  lgJsonPath, lgStrConst;

{ TJtdInferrer.THint }

constructor TJtdInferrer.THint.Make(aDefNumType: TNumberType);
begin
  Kind := hkDefNumberType;
  NumberType := aDefNumType;
  EnumList := nil;
  Path := nil;
  JsonPath := '';
end;

constructor TJtdInferrer.THint.Make(const aPath: TStringArray; aDefNumType: TNumberType);
begin
  Kind := hkDefNumberType;
  NumberType := aDefNumType;
  EnumList := nil;
  Path := aPath;
  JsonPath := '';
end;

constructor TJtdInferrer.THint.Enum(const aPath: TStringArray);
begin
  Kind := hkUseEnum;
  NumberType := ntNone;
  EnumList := nil;
  Path := aPath;
  JsonPath := '';
end;

constructor TJtdInferrer.THint.Enum(const aEnumList, aPath: TStringArray);
begin
  Kind := hkUseEnum;
  NumberType := ntNone;
  EnumList := aEnumList;
  Path := aPath;
  JsonPath := '';
end;

constructor TJtdInferrer.THint.Map(const aPath: TStringArray);
begin
  Kind := hkUseMap;
  NumberType := ntNone;
  EnumList := nil;
  Path := aPath;
  JsonPath := '';
end;

constructor TJtdInferrer.THint.Variant(const aPath: TStringArray);
begin
  Kind := hkUseVariant;
  NumberType := ntNone;
  EnumList := nil;
  Path := aPath;
  JsonPath := '';
end;

constructor TJtdInferrer.THint.AddProps(const aPath: TStringArray);
begin
  Kind := hkAdditionalProps;
  NumberType := ntNone;
  EnumList := nil;
  Path := aPath;
  JsonPath := '';
end;

constructor TJtdInferrer.THint.Nullable(const aPath: TStringArray);
begin
  Kind := hkNullable;
  NumberType := ntNone;
  EnumList := nil;
  Path := aPath;
  JsonPath := '';
end;

constructor TJtdInferrer.THint.JpMake(const aPath: string; aDefNumType: TNumberType);
begin
  Kind := hkDefNumberType;
  NumberType := aDefNumType;
  EnumList := nil;
  Path := nil;
  JsonPath := aPath;
end;

constructor TJtdInferrer.THint.JpEnum(const aPath: string);
begin
  Kind := hkUseEnum;
  NumberType := ntNone;
  EnumList := nil;
  Path := nil;
  JsonPath := aPath;
end;

constructor TJtdInferrer.THint.JpEnum(const aEnumList: TStringArray; const aPath: string);
begin
  Kind := hkUseEnum;
  NumberType := ntNone;
  EnumList := aEnumList;
  Path := nil;
  JsonPath := aPath;
end;

constructor TJtdInferrer.THint.JpMap(const aPath: string);
begin
  Kind := hkUseMap;
  NumberType := ntNone;
  EnumList := nil;
  Path := nil;
  JsonPath := aPath;
end;

constructor TJtdInferrer.THint.JpAddProps(const aPath: string);
begin
  Kind := hkAdditionalProps;
  NumberType := ntNone;
  EnumList := nil;
  Path := nil;
  JsonPath := aPath;
end;

constructor TJtdInferrer.THint.JpNullable(const aPath: string);
begin
  Kind := hkNullable;
  NumberType := ntNone;
  EnumList := nil;
  Path := nil;
  JsonPath := aPath;
end;

{ TJtdInferrer }

class function TJtdInferrer.GetNumType(const aValue: string): TNumberType;
begin
  case aValue[1] of
    'f':
      if aValue = 'float32' then
        Result := ntFloat32
      else
        if aValue = 'float64' then
          Result := ntFloat64
        else
          Result := ntNone;
    'i':
      if aValue = 'int8' then
        Result := ntInt8
      else
        if aValue = 'int16' then
          Result := ntInt16
        else
          if aValue = 'int32' then
            Result := ntInt32
          else
            Result := ntNone;
    'u':
      if aValue = 'uint8' then
        Result := ntUInt8
      else
        if aValue = 'uint16' then
          Result := ntUInt16
        else
          if aValue = 'uint32' then
            Result := ntUInt32
          else
            Result := ntNone;
  else
    Result := ntNone;
  end;
end;

class function TJtdInferrer.ContainsNum(aValue: Double; aType: TNumberType): Boolean;
begin
  if NUM_TYPES[aType].IsInt and (Frac(aValue) <> 0.0) then exit(False);
  Result := (aValue >= NUM_TYPES[aType].MinValue) and (aValue <= NUM_TYPES[aType].MaxValue);
end;

class function TJtdInferrer.ContainsType(L, R: TNumberType): Boolean;
begin
  if L = R then
    Result := True
  else
    if NUM_TYPES[L].IsInt <> NUM_TYPES[R].IsInt then
      case L of
        ntFloat32: Result := R < ntInt32;
        ntFloat64: Result := True;
      else
        Result := False;
      end
    else
      Result :=
        (NUM_TYPES[R].MinValue >= NUM_TYPES[L].MinValue)and(NUM_TYPES[R].MaxValue <= NUM_TYPES[L].MaxValue);
end;


class function TJtdInferrer.RefineNumType(aValue: Double; aCurrType: TNumberType): TNumberType;
var
  nt: TNumberType;
begin
  if aCurrType = ntNone then
    aCurrType := DEFAULT_NUM_TYPE;
  if ContainsNum(aValue, aCurrType) then
    exit(aCurrType);
  for nt := ntInt8 to ntUInt32 do
    if ContainsNum(aValue, nt) then exit(nt);
  Result := ntFloat64;
end;

class function TJtdInferrer.GetStrType(const aValue: string): TStrType;
begin
  case aValue[1] of
    's':
      if aValue = 'string' then
        Result := stString
      else
        Result := stNone;
    't':
      if aValue = 'timestamp' then
        Result := stTimeStamp
      else
        Result := stNone;
  else
    Result := stNone;
  end;
end;

class function TJtdInferrer.RefineStrType(const aValue: string; aCurrType: TStrType): TStrType;
begin
  case aCurrType of
    stNone:
      if IsRfc8927TimeStamp(aValue) then
        Result := stTimeStamp
      else
        Result := stString;
    stString: Result := stString;
  else // stTimeStamp
    if IsRfc8927TimeStamp(aValue) then
      Result := stTimeStamp
    else
      Result := stString;
  end;
end;

class function TJtdInferrer.GetFormKind(aNode: TJsonNode): TFormKind;
begin
  Result := TFormKind(Trunc(aNode.AsObject[FORM_KIND_KEY].AsNumber));
end;

class procedure TJtdInferrer.SetFormKind(aNode: TJsonNode; aKind: TFormKind);
begin
  if aKind = fkEmpty then
    aNode.Clear;
  aNode[FORM_KIND_KEY].AsNumber := Integer(aKind);
end;

class procedure TJtdInferrer.MovePropToOptional(aNode: TJsonNode; const aProp: string);
var
  Node, n: TJsonNode;
begin
  if not aNode.Find(PROPS_KEY, Node) then exit;
  if Node.Find(aProp, n) then
    begin
      aNode[OPT_PROPS_KEY][aProp].CopyFrom(n);
      Node.Remove(aProp);
    end;
end;

class procedure TJtdInferrer.CorrectEnumList(aNode: TJsonNode);
var
  n: TJsonNode;
  a: TStringArray;
  s: string;
begin
  if aNode.Find(ENUM_KEY, n) then
    begin
      a := n.Names.ToArray;
      for s in a do
        n.Add(s);
    end;
end;

class procedure TJtdInferrer.ClearAssistInfo(aNode: TJsonNode);
var
  n: TJsonNode;
begin
  if aNode.IsObject then
    begin
      if GetFormKind(aNode) = fkEnum then
        CorrectEnumList(aNode);
      aNode.Remove(FORM_KIND_KEY);
      if aNode.Count = 1 then
        aNode.Remove(NULLBL_KEY);
      if aNode.Find(PROPS_KEY, n) and (n.Count = 0) then
        aNode.Remove(PROPS_KEY);
      for n in aNode do
        ClearAssistInfo(n);
    end;
end;

procedure TJtdInferrer.DoInferBool(aSchema, aSample: TJsonNode);
var
  Hint: PHint;
begin
  for Hint in FHintMap[aSample] do
    if Hint^.Kind = hkNullable then
      aSchema[NULLBL_KEY].AsBoolean := True;
  case GetFormKind(aSchema) of
    fkNone:
      begin
        SetFormKind(aSchema, fkType);
        aSchema[TYPE_KEY].AsString := BOOL_KEY;
      end;
    fkEmpty: ;
    fkType:
      if aSchema[TYPE_KEY].AsString <> BOOL_KEY then
        SetFormKind(aSchema, fkEmpty);
  else
    SetFormKind(aSchema, fkEmpty);
  end;
end;

procedure TJtdInferrer.DoInferNumber(aSchema, aSample: TJsonNode);
var
  Hint: PHint;
  NumType, CurrNumType: TNumberType;
begin
  NumType := DefaultNumType;
  for Hint in FHintMap[aSample] do
    case Hint^.Kind of
      hkDefNumberType:
        if Hint^.NumberType > NumType then
          NumType := Hint^.NumberType;
      hkNullable:
        aSchema[NULLBL_KEY].AsBoolean := True;
    else
    end;
  case GetFormKind(aSchema) of
    fkNone:
      begin
        SetFormKind(aSchema, fkType);
        aSchema[TYPE_KEY].AsString := NUM_NAMES[RefineNumType(aSample.AsNumber, NumType)];
      end;
    fkEmpty: ;
    fkType:
      begin
        CurrNumType := GetNumType(aSchema[TYPE_KEY].AsString);
        if CurrNumType = ntNone then
          SetFormKind(aSchema, fkEmpty)
        else
          begin
            if (NumType <> ntNone) and ContainsType(NumType, CurrNumType) then
              CurrNumType := NumType;
            aSchema[TYPE_KEY].AsString := NUM_NAMES[RefineNumType(aSample.AsNumber, CurrNumType)];
          end;
      end;
  else
    SetFormKind(aSchema, fkEmpty);
  end;
end;

procedure TJtdInferrer.DoInferString(aSchema, aSample: TJsonNode);
var
  Hint, UseHint: PHint;
begin
  UseHint := nil;
  for Hint in FHintMap[aSample] do
    case Hint^.Kind of
      hkUseEnum:  if UseHint = nil then UseHint := Hint;
      hkNullable: aSchema[NULLBL_KEY].AsBoolean := True;
    else
    end;
  case GetFormKind(aSchema) of
    fkNone:
      if UseHint <> nil then
        DoInitEnum(aSchema, aSample, UseHint)
      else
        begin
          SetFormKind(aSchema, fkType);
          aSchema[TYPE_KEY].AsString := STR_NAMES[RefineStrType(aSample.AsString)];
        end;
    fkEmpty: ;
    fkEnum:
      aSchema[ENUM_KEY].TryAddNull(aSample.AsString);
    fkType:
      if GetStrType(aSchema[TYPE_KEY].AsString) = stNone then
        SetFormKind(aSchema, fkEmpty)
      else
        aSchema[TYPE_KEY].AsString :=
          STR_NAMES[RefineStrType(aSample.AsString, GetStrType(aSchema[TYPE_KEY].AsString))];
  else
    SetFormKind(aSchema, fkEmpty);
  end;
end;

procedure TJtdInferrer.DoInitEnum(aSchema, aSample: TJsonNode; aHint: PHint);
var
  s: string;
  n: TJsonNode;
begin
  SetFormKind(aSchema, fkEnum);
  n := aSchema[ENUM_KEY];
  if aHint <> nil then
    for s in aHint^.EnumList do
      n.TryAddNull(s);
  n.TryAddNull(aSample.AsString);
end;

procedure TJtdInferrer.DoInferArray(aSchema, aSample: TJsonNode);
var
  Hint: PHint;
  s, n: TJsonNode;
  fk: TFormKind;
begin
  for Hint in FHintMap[aSample] do
    if Hint^.Kind = hkNullable then
      aSchema[NULLBL_KEY].AsBoolean := True;
  fk := GetFormKind(aSchema);
  case fk of
    fkNone, fkElements:
      begin
        if fk = fkNone then
          SetFormKind(aSchema, fkElements);
        s := aSchema[ELEMS_KEY].AsObject;
        for n in aSample do
          DoInfer(s, n);
      end;
    fkEmpty: ;
  else
    SetFormKind(aSchema, fkEmpty);
  end;
end;

procedure TJtdInferrer.DoInferObject(aSchema, aSample: TJsonNode);
var
  Hint, UseHint: PHint;
  n: TJsonNode;
  AddProps: Boolean;
begin
  UseHint := nil;
  AddProps := False;
  for Hint in FHintMap[aSample] do
    case Hint^.Kind of
      hkUseMap, hkUseVariant:
        if UseHint = nil then
          UseHint := Hint;
      hkAdditionalProps: AddProps := True;
      hkNullable: aSchema[NULLBL_KEY].AsBoolean := True;
    else
    end;
  case GetFormKind(aSchema) of
    fkNone:
      begin
        if UseHint <> nil then
          begin
            if UseHint^.Kind = hkUseMap then
              DoInitMap(aSchema, aSample)
            else
              begin
                n := DoInitVariant(aSchema, aSample, UseHint);
                if n <> nil then
                  DoInfer(n, aSample)
                else
                  DoInitProps(aSchema, aSample, AddProps);
              end;
          end
        else
          DoInitProps(aSchema, aSample, AddProps);
      end;
    fkProperties:
      DoInferProps(aSchema, aSample, AddProps);
    fkValues:
      DoInferMap(aSchema, aSample);
    fkDiscriminator:
      DoInferVariant(aSchema, aSample);
  else
    SetFormKind(aSchema, fkEmpty);
  end;
end;

procedure TJtdInferrer.DoInitMap(aSchema, aSample: TJsonNode);
var
  s, n: TJsonNode;
begin
  SetFormKind(aSchema, fkValues);
  s := aSchema[VALUES_KEY].AsObject;
  for n in aSample do
    DoInfer(s, n);
end;

procedure TJtdInferrer.DoInferMap(aSchema, aSample: TJsonNode);
var
  s, n: TJsonNode;
begin
  s := aSchema[VALUES_KEY].AsObject;
  for n in aSample do
    DoInfer(s, n);
end;

function TJtdInferrer.DoInitVariant(aSchema, aSample: TJsonNode; aHint: PHint): TJsonNode;
var
  n: TJsonNode;
  discr, v: string;
begin
  if aHint = nil then exit(nil);
  if System.Length(aHint^.EnumList) <> 1 then exit(nil);
  discr := aHint^.EnumList[0];
  if not aSample.Find(discr, n) then exit(nil);
  if not n.IsString then exit(nil);
  SetFormKind(aSchema, fkDiscriminator);
  aSchema[DISCRI_KEY].AsString := discr;
  v := n.AsString;
  aSample.Remove(discr);
  Result := aSchema[MAPPING_KEY][v].AsObject;
end;

procedure TJtdInferrer.DoInferVariant(aSchema, aSample: TJsonNode);
var
  n: TJsonNode;
  discr, v: string;
begin
  discr := aSchema[DISCRI_KEY].AsString;
  if aSample.Find(discr, n) and n.IsString then
    begin
      v := n.AsString;
      aSample.Remove(discr);
      DoInfer(aSchema[MAPPING_KEY][v].AsObject, aSample);
    end
  else
    SetFormKind(aSchema, fkEmpty);
end;

procedure TJtdInferrer.DoInitProps(aSchema, aSample: TJsonNode; AddProps: Boolean);
var
  p: TJsonNode.TPair;
  s: TJsonNode;
begin
  SetFormKind(aSchema, fkProperties);
  if AddProps then
    aSchema[ADD_PROPS_KEY].AsBoolean := True;
  s := aSchema[PROPS_KEY].AsObject;
  for p in aSample.Entries do
    DoInfer(s[p.Key].AsObject, p.Value);
end;

procedure TJtdInferrer.DoInferProps(aSchema, aSample: TJsonNode; AddProps: Boolean);
var
  p: TJsonNode.TPair;
  s, n: TJsonNode;
begin
  if AddProps then
    aSchema[ADD_PROPS_KEY].AsBoolean := True;
  for p in aSchema[PROPS_KEY].Entries do
    if aSample.Find(p.Key, n) then
      DoInfer(p.Value, n)
    else
      MovePropToOptional(aSchema, p.Key);
  s := aSchema[PROPS_KEY];
  for p in aSample.Entries do
    if not s.Find(p.Key, n) then
      DoInfer(aSchema[OPT_PROPS_KEY][p.Key].AsObject, p.Value);
end;

procedure TJtdInferrer.DoInfer(aSchema, aSample: TJsonNode);
begin
  case aSample.Kind of
    jvkNull:
      aSchema[NULLBL_KEY].AsBoolean := True;
    jvkFalse, jvkTrue:
      DoInferBool(aSchema, aSample);
    jvkNumber:
      DoInferNumber(aSchema, aSample);
    jvkString:
      DoInferString(aSchema, aSample);
    jvkArray:
      DoInferArray(aSchema, aSample);
    jvkObject:
      DoInferObject(aSchema, aSample);
  end;
end;

procedure TJtdInferrer.CollectHints(const aHints: array of THint);
var
  I: SizeInt;
begin
  for I := 0 to System.High(aHints) do
    begin
      case aHints[I].Kind of
        hkUnknown: continue;
        hkDefNumberType:
          if (aHints[I].JsonPath = '') and (aHints[I].Path = nil) and
             (aHints[I].NumberType > FDefNumType) then
            begin
              FDefNumType := aHints[I].NumberType;
              continue;
            end;
        hkUseVariant: // here JsonPath just ignored
          begin
            if aHints[I].Path = nil then continue;
            with FHintList.UncMutable[FHintList.Add(aHints[I])]^ do
              begin
                EnumList := [Path[System.High(Path)]];
                System.SetLength(Path, System.High(Path));
              end;
            continue;
          end
      else
      end;
      FHintList.Add(aHints[I]);
    end;
end;

class function TJtdInferrer.Infer(const aSamples: array of string; const aHints: array of THint;
  aSkipBom: Boolean; aMaxDepth: Integer): TJsonNode;
begin
  Result := nil;
  with TJtdInferrer.Create(aSamples, aHints) do
    try
      SkipBom := aSkipBom;
      MaxDepth := aMaxDepth;
      Result := Execute;
    finally
      Free;
    end;
end;

class function TJtdInferrer.Infer(const aSamples: array of TJsonNode; const aHints: array of THint): TJsonNode;
begin
  Result := nil;
  with TJtdInferrer.Create(aSamples, aHints) do
    try
      Result := Execute;
    finally
      Free;
    end;
end;

class function TJtdInferrer.InferJson(const aSamples: array of string; const aHints: array of THint;
  aSkipBom: Boolean; aMaxDepth: Integer): string;
var
  Schema: TJsonNode;
begin
  Result := '';
  Schema := nil;
  try
    Schema := Infer(aSamples, aHints, aSkipBom, aMaxDepth);
    Result := Schema.AsJson;
  finally
    Schema.Free;
  end;
end;

class function TJtdInferrer.InferJson(const aSamples: array of TJsonNode; const aHints: array of THint): string;
var
  Schema: TJsonNode;
begin
  Result := '';
  Schema := nil;
  try
    Schema := Infer(aSamples, aHints);
    Result := Schema.AsJson;
  finally
    Schema.Free;
  end;
end;

constructor TJtdInferrer.Create(const aSamples: array of string; const aHints: array of THint);
begin
  FDefNumType := DEFAULT_NUM_TYPE;
  FMaxDepth := DEF_DEPTH;
  FSampleList := specialize TGArrayHelpUtil<string>.CreateCopy(aSamples);
  CollectHints(aHints);
end;

constructor TJtdInferrer.Create(const aSamples: array of TJsonNode; const aHints: array of THint);
var
  Node: TJsonNode;
begin
  FDefNumType := DEFAULT_NUM_TYPE;
  FMaxDepth := DEF_DEPTH;
  for Node in aSamples do
    FNodeList.Add(Node.Clone);
  CollectHints(aHints);
end;

destructor TJtdInferrer.Destroy;
var
  Node: TJsonNode;
begin
  for Node in FNodeList do
    Node.Free;
  inherited;
end;

function TJtdInferrer.Execute: TJsonNode;
var
  JpValueList: TJpValueList;
  Node, Pointed: TJsonNode;
  Sample: string;
  pH: PHint;
begin
  if FNodeList.IsEmpty then
    for Sample in FSampleList do
      if TJsonNode.TryParse(Sample, Node, SkipBom, MaxDepth) then
        if TJsonNode.DuplicateFree(Node) then
          FNodeList.Add(Node)
        else
          Node.Free;
  for Node in FNodeList do
    for pH in FHintList.Mutables do
      if pH^.Kind <> hkUseVariant then
        begin
          if pH^.JsonPath <> '' then // JsonPath priority
            begin
              if Node.TryMatchValues(pH^.JsonPath, JpValueList) then
                for Pointed in JpValueList do
                  FHintMap.Add(Pointed, pH);
            end
          else
            if Node.FindPath(pH^.Path, Pointed) then
              FHintMap.Add(Pointed, pH);
        end
      else // JsonPath ignored
        if Node.FindPath(pH^.Path, Pointed) then
          FHintMap.Add(Pointed, pH);
  Result := TJsonNode.NewNode(jvkObject);
  for Node in FNodeList do
    try
      DoInfer(Result, Node);
    except
      Result.Clear;
      Result.AsObject;
    end;
  ClearAssistInfo(Result);
end;

end.

