{****************************************************************************
*                                                                           *
*   This file is part of the LGenerics package.                             *
*   Pascal code generator from JSON TypeDef schemas.                        *                                            *
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
unit lgJtdCodegen;

{$MODE OBJFPC}{$H+}

interface

uses
  Classes, SysUtils,
  lgUtils, lgVector, lgHashSet, lgHashMap, lgJson, lgJsonTypeDef;
{
  supported metadata keywords:

    "description"      - description of the entity, will be displayed in the comments to this entity.

    "enumDescription"  - a description of the specific elements of the enumeration.

    "preferredName"    - desired name for the entity (without T), has absolute priority.

    "propsDescription" - descriptions of the properties of the entity, which may not coincide
                         with the descriptions of the corresponding types.

    however, all of these keywords are not portable.
}
type
  TFormKind = TJtdSchema.TFormKind;
  TJtdType  = TJtdSchema.TJtdType;

  TJtdTemplate = class abstract
  private
    FDescription,
    FTypeName: string;
    FAsciiNames,
    FNullable: Boolean;
    function  GetDescription: string;
  public
    procedure WriteDescription(aText: TStrings; aComment: Boolean);
    procedure WriteDeclaration(aText: TStrings; aComment: Boolean); virtual; abstract;
    procedure WriteImplementation(aText: TStrings) virtual;
    property  Description: string read FDescription write FDescription;
    property  TypeName: string read FTypeName;
    property  HasAsciiNames: Boolean read FAsciiNames write FAsciiNames;
    property  Nullable: Boolean read FNullable write FNullable;
  end;

  { TJtdTemplater }
  TJtdTemplater = class
  strict private
  type
    TTemplateList = specialize TGObjectVector<TJtdTemplate>;
    TStrSetType   = specialize TGLiteChainHashSet<string, string>;
    TStrSet       = TStrSetType.TSet;
    TTypeMapType  = specialize TGLiteChainHashMap<string, string, string>;
    TTypeMap      = TTypeMapType.TMap;
  var
    FRootSchema: TJtdSchema;
    EnumElemSet,
    FPasKeywords,
    FGlobNameTable: TStrSet;
    FTypeMap,
    FDefsMap: TTypeMap;
    FTemplateList: TTemplateList;
    FDeclarationList,
    FImplementationList: TStringList;
    FUniqEnumElems,
    FComments: Boolean;
    FRootClassName: string;
  private
    function  GetTypeAlias(aType: TJtdType): string;
    procedure FillPasKeywords;
    function  IsPasKeyword(const s: string): Boolean;
    function  IsIdentifier(const s: string): Boolean;
    function  IsAsciiEncoded(const s: string): Boolean;
    function  IsUniqIdentifierList(const a: array of string): Boolean;
    function  IsAsciiWordList(const a: array of string): Boolean;
    function  CapitalizeFirst(const s: string): string; inline;
    function  ProperCase(const s: string): string; inline;
    function  AsIdentifier(const s: string): string;
    function  AsPasIdentifier(const s: string): string;
    function  AsUniqIdentifier(const s: string): string;
    function  AsUniqIdentifier(const s: string; var aSet: TStrSet): string;
    function  AsPasUniqIdentifier(const s: string; var aSet: TStrSet): string;
    function  AsTypeName(const s: string): string;
    function  AsPasTypeName(const s: string): string;
    function  AsUniqTypeName(const s: string): string;
    function  AsPasUniqTypeName(const s: string): string;
    function  AsFieldName(const s: string): string;
    procedure CheckUniqEnumElements(const aElems: TStringArray);
    function  HasDescription(aSchema: TJtdSchema; out aDescr: string): Boolean;
    function  HasEnumDescription(aSchema: TJtdSchema; out aDescr: TJsonNode): Boolean;
    function  HasPreferName(aSchema: TJtdSchema; out aName: string): Boolean;
    function  GetPreferNameDef(aSchema: TJtdSchema; const aDefault: string): string;
    ////////////////////////////
    function  HandleEmpty(aSchema: TJtdSchema; const aTypeName: string = ''): string;
    function  HandleRef(aSchema: TJtdSchema): string;
    function  HandleType(aSchema: TJtdSchema; const aTypeName: string = ''): string;
    function  HandleEnum(aSchema: TJtdSchema; const aTypeName: string = ''): string;
    function  HandleElements(aSchema: TJtdSchema; const aTypeName: string = ''): string;
    function  HandleValues(aSchema: TJtdSchema; const aTypeName: string = ''): string;
    function  HandleProperties(aSchema: TJtdSchema; const aTypeName: string = ''): string;
    function  HandleDiscriminator(aSchema: TJtdSchema; const aTypeName: string = ''): string;
    function  HandleSchema(aSchema: TJtdSchema; const aParentTag: string = ''): string;
    function  ProcessSchema: string;
  public
    constructor Create(aSchema: TJtdSchema);
    destructor Destroy; override;
  { returns the type name of the root class }
    function  Execute: string;
    property  HasUniqEnumElements: Boolean read FUniqEnumElems;
    property  ShowComments: Boolean read FComments write FComments;
  { preferred root class type name(without T), optional }
    property  RootClassName: string read FRootClassName write FRootClassName;
    property  DeclarationList: TStringList read FDeclarationList;
    property  ImplementationList: TStringList read FImplementationList;
  end;

  { TJtdPasCodegen }
  TJtdPasCodegen = class
  private
  const
    DEFAULT_SWITCHES  = '{$MODE OBJFPC}{$H+}{$B-}';
    DISPLAY_NAME      = 'JtdPasCodegen';
    DEFAULT_UNIT_NAME = 'Unit1';
  var
    FEngine: TJtdTemplater;
    FSource: TStringList;
    FUnitName,
    FPreferRootName,
    FFinalRootName,
    FSwitches,
    FCustomHeader: string;
    FUseUnits: TStringArray;
    FComments: Boolean;
    procedure WriteHeader(aText: TStrings);
    procedure WriteUnitClause(aText: TStrings);
    procedure WriteSwitches(aText: TStrings);
    procedure WriteUsesClause(aText: TStrings);
    procedure WriteImplClause(aText: TStrings);
    procedure WriteUnitEnd(aText: TStrings);
    property  UseUnits: TStringArray read FUseUnits;
    property  Switches: string read FSwitches;
  public
    constructor Create(aSchema: TJtdSchema);
    destructor Destroy; override;
  { after the execution of the RunTemplater proc, the FinalRootClassName
    contains the type name of the root class }
    procedure RunTemplater;
  { after the execution of the Execute proc, the Source contains the full
    text of the unit }
    procedure Execute;
    procedure SaveToStream(aStream: TStream);
    procedure SaveToFile(const aFileName: string);
    property  Source: TStringList read FSource;
  { preferred root class type name(without T), optional }
    property  PreferRootClassName: string read FPreferRootName write FPreferRootName;
  { final root class type name }
    property  FinalRootClassName: string read FFinalRootName;
  { unit name, optional, by default is "Unit1" }
    property  UnitName: string read FUnitName write FUnitName;
  { some additional text that will be placed in the unit header }
    property  CustomHeader: string read FCustomHeader write FCustomHeader;
  { whether to display comments, the default is True }
    property  ShowComments: Boolean read FComments write FComments;
  end;

implementation
{$B-}{$COPERATORS ON}{$POINTERMATH ON}

uses
  TypInfo, Math, lgStrConst;

const
  DESCRIPTION_TAG      = 'description';
  ENUM_DESCRIPTION_TAG = 'enumDescription';
  PROP_DESCRIPTION_TAG = 'propsDescription';
  PREFERRED_NAME_TAG   = 'preferredName';
  ANY_TAG              = 'Any';
  ENUM_TAG             = 'Enum';
  ELEM_TAG             = 'Elem';
  ELEMENTS_TAG         = 'List';
  VALUES_TAG           = 'Dictionary';
  OBJECT_TAG           = 'JObject';
  VARIANT_TAG          = 'Union';
  EMPTY_NAME           = 'EmptyName';
  NULLABLE_COMMENT     = ' is nullable';
  KEYWORD_SUFFIX       = '_';
  ASCII_PREFIX         = 'P';
  ASCII_LETTERS        = ['a'..'z', 'A'..'Z'];
  ALPHA_CHARS          = ASCII_LETTERS + ['_'];
  ALPHANUM_CHARS       = ALPHA_CHARS + ['0'..'9'];

type
  TJtdEnumAlias = class(TJtdTemplate)
  private
    FOldTypeName: string;
  public
    constructor Create(const aTypeName, aOldTypeName: string);
    procedure WriteDeclaration(aText: TStrings; aComment: Boolean); override;
  end;

  TJtdGenAny = class(TJtdTemplate)
  public
    class function DefaultTypeName: string; static;
    constructor Create;
    constructor Create(const aTypeName: string);
    procedure WriteDeclaration(aText: TStrings; aComment: Boolean); override;
  end;

  TJtdTypeTemplate = class(TJtdTemplate)
  private
    FType: TJtdType;
  public
    class function DefaultTypeName(aType: TJtdType): string; static;
    constructor Create(aType: TJtdType);
    constructor Create(aType: TJtdType; const aTypeName: string);
    function  GetType: TJtdType;
    procedure WriteDeclaration(aText: TStrings; aComment: Boolean); override;
  end;

  TJtdEnumType = class(TJtdTemplate)
  private
    FElList: TStringArray;
    FEnumDescription: TJsonNode;
  public
    constructor Create(const aTypeName: string; const aEls: TStringArray);
    procedure WriteDeclaration(aText: TStrings; aComment: Boolean); override;
    property  ElemList: TStringArray read FElList;
    property  EnumDescription: TJsonNode read FEnumDescription write FEnumDescription;
  end;

  TJtdImplemented = class abstract(TJtdTemplate)
  public
    procedure WriteImplementation(aText: TStrings); override;
  end;

  TJtdEnumElem = class(TJtdImplemented)
  private
    FEnumType: TJtdEnumType;
  public
    constructor Create(const aTypeName: string; aEnumType: TJtdEnumType);
    procedure WriteDeclaration(aText: TStrings; aComment: Boolean); override;
  end;

  TJtdStrEnumElem = class(TJtdImplemented)
  private
    FElemList: TStringArray;
    FEnumDescription: TJsonNode;
  public
    constructor Create(const aTypeName: string; const aElemList: TStringArray);
    procedure WriteDeclaration(aText: TStrings; aComment: Boolean); override;
    procedure WriteImplementation(aText: TStrings); override;
    property  ElemList: TStringArray read FElemList;
    property  EnumDescription: TJsonNode read FEnumDescription write FEnumDescription;
  end;

  TJtdElements = class(TJtdImplemented)
  private
    FSpecType: string;
  public
    constructor Create(const aTypeName, aSpecType: string);
    procedure WriteDeclaration(aText: TStrings; aComment: Boolean); override;
  end;

  TJtdValues = class(TJtdImplemented)
  private
    FSpecType: string;
  public
    constructor Create(const aTypeName, aSpecType: string);
    procedure WriteDeclaration(aText: TStrings; aComment: Boolean); override;
  end;

  TJtdPropInfo = record
    JsonPropName,
    PropName,
    PropFieldName,
    PropType,
    Descryption: string;
    Nullable: Boolean;
  end;
  TJtdPropList = array of TJtdPropInfo;

  TJtdProps = class(TJtdImplemented)
  private
    FProps,
    FOptProps: TJtdPropList;
    FAdditionalProps: Boolean;
    procedure WriteClearProc(aText: TStrings);
    procedure WriteSetters(aText: TStrings);
    procedure WriteReadPropsFromNode(aText: TStrings);
    procedure WriteReadPropsReader(aText: TStrings);
    procedure WritePropsWriteJson(aText: TStrings);
    procedure WritePropDescription(const aProp: TJtdPropInfo; aText: TStrings; aComment: Boolean);
    procedure WriteOptPropsWarning(aText: TStrings; aComment: Boolean);
  public
    constructor Create(const aTypeName: string; const aProps, aOptProps: TJtdPropList);
    procedure WriteDeclaration(aText: TStrings; aComment: Boolean); override;
    procedure WriteImplementation(aText: TStrings); override;
    property  Props: TJtdPropList read FProps;
    property  OptionalProps: TJtdPropList read FOptProps;
    property  AdditionalProps: Boolean read FAdditionalProps write FAdditionalProps;
  end;

  TTagMap = record
    TagValue,
    PropName,
    PropType,
    Description: string;
    Nullable: Boolean;
  end;
  TTagMapping = array of TTagMap;

  TJtdVarTemplate = class(TJtdImplemented)
  private
    FTagJsonName: string;
    FMapping: TTagMapping;
    procedure WritePropDescription(const aDescr, aTagValue: string; aText: TStrings; aComment: Boolean);
    procedure WriteGetTagJsonName(aText: TStrings);
    procedure WriteValidTagValue(aText: TStrings);
    procedure WriteGetInstanceClass(aText: TStrings);
    procedure WriteGetters(aText: TStrings);
    procedure WriteSetters(aText: TStrings);
  public
    constructor Create(const aTagName, aTypeName: string; const aMapping: TTagMapping);
    procedure WriteDeclaration(aText: TStrings; aComment: Boolean); override;
    procedure WriteImplementation(aText: TStrings); override;
    property  TagJsonName: string read FTagJsonName;
    property  Mapping: TTagMapping read FMapping;
  end;

function TJtdTemplate.GetDescription: string;
var
  s: string;
begin
  if Nullable then begin
    if Description = '' then
      s := TypeName + NULLABLE_COMMENT
    else
      if Description[System.Length(Description)] in [',', '.', ';', '?', '!'] then
        s := Description + NULLABLE_COMMENT
      else
        s := Description + ';' + NULLABLE_COMMENT;
  end else
    s := Description;
  if s = '' then exit('');
  Result := '{ ' + SysUtils.WrapText(s, LineEnding + '  ', [' ', '-', #9], 72) + ' }';
end;

procedure TJtdTemplate.WriteDescription(aText: TStrings; aComment: Boolean);
var
  s: string;
begin
  if aComment then begin
    s := GetDescription;
    if s <> '' then aText.Add(s);
  end;
end;

procedure TJtdTemplate.WriteImplementation(aText: TStrings);
begin
  Assert(aText = aText);
end;

constructor TJtdEnumAlias.Create(const aTypeName, aOldTypeName: string);
begin
  FTypeName := aTypeName;
  FOldTypeName := aOldTypeName;
end;

procedure TJtdEnumAlias.WriteDeclaration(aText: TStrings; aComment: Boolean);
begin
  WriteDescription(aText, aComment);
  aText.Add(Format('  %s = %s;', [TypeName, FOldTypeName]));
  aText.Add('');
  Assert(aComment = aComment);
end;

class function TJtdGenAny.DefaultTypeName: string;
begin
  Result := 'TJtdAny';
end;

constructor TJtdGenAny.Create;
begin
  FTypeName := DefaultTypeName;
end;

constructor TJtdGenAny.Create(const aTypeName: string);
begin
  FTypeName := aTypeName;
end;

procedure TJtdGenAny.WriteDeclaration(aText: TStrings; aComment: Boolean);
begin
  WriteDescription(aText, aComment);
  aText.Add(Format('  %s = %s;', [TypeName, DefaultTypeName]));
  aText.Add('');
end;

class function TJtdTypeTemplate.DefaultTypeName(aType: TJtdType): string;
begin
  case aType of
    jtNone:      Result := '';
    jtBool:      Result := 'TJtdBool';
    jtFloat32:   Result := 'TJtdFloat32';
    jtFloat64:   Result := 'TJtdFloat64';
    jtInt8:      Result := 'TJtdInt8';
    jtUInt8:     Result := 'TJtdUInt8';
    jtInt16:     Result := 'TJtdInt16';
    jtUInt16:    Result := 'TJtdUInt16';
    jtInt32:     Result := 'TJtdInt32';
    jtUInt32:    Result := 'TJtdUInt32';
    jtString:    Result := 'TJtdString';
    jtTimeStamp: Result := 'TJtdDateTimeUTC';
  end;
end;

constructor TJtdTypeTemplate.Create(aType: TJtdType);
begin
  FType := aType;
  FTypeName := DefaultTypeName(FType);
end;

constructor TJtdTypeTemplate.Create(aType: TJtdType; const aTypeName: string);
begin
  FType := aType;
  FTypeName := aTypeName;
end;

function TJtdTypeTemplate.GetType: TJtdType;
begin
  Result := FType;
end;

procedure TJtdTypeTemplate.WriteDeclaration(aText: TStrings; aComment: Boolean);
begin
  WriteDescription(aText, aComment);
  aText.Add(Format('  %s = %s;', [TypeName, DefaultTypeName(FType)]));
  aText.Add('');
end;

constructor TJtdEnumType.Create(const aTypeName: string; const aEls: TStringArray);
begin
  FTypeName := aTypeName;
  FElList := aEls;
end;

procedure TJtdEnumType.WriteDeclaration(aText: TStrings; aComment: Boolean);
var
  s: string;
  n: TJsonNode;
  I: SizeInt;
begin
  if not aComment then begin
    aText.Add(Format('  %s = (%s);', [TypeName, string.Join(', ', ElemList)]));
    aText.Add('');
    exit;
  end;
  s := GetDescription;
  if s <> '' then aText.Add(s);
  if (EnumDescription <> nil) and (EnumDescription.IsObject) then begin
    aText.Add(Format('  %s = (', [TypeName]));
    for I := 0 to Pred(System.High(ElemList)) do
      if EnumDescription.Find(ElemList[I], n) and n.IsString then
        aText.Add(Format('    %s, // %s', [ElemList[I], n.AsString]))
      else
        aText.Add(Format('    %s,', [ElemList[I]]));
    if EnumDescription.Find(ElemList[System.High(ElemList)], n) and n.IsString then
      aText.Add(Format('    %s // %s', [ElemList[System.High(ElemList)], n.AsString]))
    else
      aText.Add(Format('    %s', [ElemList[System.High(ElemList)]]));
    aText.Add('  );');
  end else
    aText.Add(Format('  %s = (%s);', [TypeName, string.Join(', ', ElemList)]));
  aText.Add('');
end;

procedure TJtdImplemented.WriteImplementation(aText: TStrings);
begin
  aText.Add(Format('{ %s }', [TypeName]));
  aText.Add('');
  aText.Add(Format('class function %s.GetJtdClass: TJtdEntityClass;', [TypeName]));
  aText.Add('begin');
  aText.Add(Format('  Result := %s;', [TypeName]));
  aText.Add('end;');
  aText.Add('');
end;

constructor TJtdEnumElem.Create(const aTypeName: string; aEnumType: TJtdEnumType);
begin
  FTypeName := aTypeName;
  FEnumType := aEnumType;
end;

procedure TJtdEnumElem.WriteDeclaration(aText: TStrings; aComment: Boolean);
begin
  WriteDescription(aText, aComment);
  aText.Add(Format('  %s = class sealed(specialize TJtdEnum<%s>)', [TypeName, FEnumType.TypeName]));
  aText.Add('    class function GetJtdClass: TJtdEntityClass; override;');
  aText.Add('  end;');
  aText.Add('');
end;

constructor TJtdStrEnumElem.Create(const aTypeName: string; const aElemList: TStringArray);
begin
  FTypeName := aTypeName;
  FElemList := aElemList;
  FAsciiNames := True;
end;

procedure TJtdStrEnumElem.WriteDeclaration(aText: TStrings; aComment: Boolean);
var
  s: string;
  n: TJsonNode;
  I: Integer;
begin
  if aComment then begin
    s := ' ' + SysUtils.WrapText(Description, LineEnding + '  ', [' ', '-', #9], 72);
    if s <> '' then s := s + LineEnding;
    if (EnumDescription <> nil) and (EnumDescription.IsObject) then begin
      for I := 0 to System.High(ElemList) - 1 do
        if EnumDescription.Find(ElemList[I], n) and n.IsString then
          s := s + Format('    %s :  %s', [ElemList[I], n.AsString]) + LineEnding
        else
          s := s + Format('    %s', [ElemList[I]]) + LineEnding;
      if EnumDescription.Find(ElemList[System.High(ElemList)], n) and n.IsString then
        s := s + Format('    %s :  %s', [ElemList[System.High(ElemList)], n.AsString])
      else
        s := s + Format('    %s', [ElemList[System.High(ElemList)]]);

      if s <> '' then begin
        s := '{' + s + ' }';
        aText.Add(s);
      end;
    end;
  end;
  aText.Add(Format('  %s = class sealed(TJtdStrEnum)', [TypeName]));
  aText.Add('  protected');
  aText.Add('  public');
  aText.Add('    class function GetJtdClass: TJtdEntityClass; override;');
  aText.Add('    class function IsEnumElement(const aValue: string): Boolean; override;');
  aText.Add('  end;');
  aText.Add('');
end;

procedure TJtdStrEnumElem.WriteImplementation(aText: TStrings);
var
  I: Integer;
begin
  inherited WriteImplementation(aText);
  aText.Add(Format('class function %s.IsEnumElement(const aValue: string): Boolean;', [TypeName]));
  aText.Add('begin');
  if HasAsciiNames then
    aText.Add('  case aValue of')
  else
    aText.Add('  case TJsonNode.PasStrToAsciiJson(aValue) of');
  for I := 0 to System.High(ElemList) - 1 do
    aText.Add(Format('    ''%s'',', [ElemList[I]]));
  aText.Add(Format('    ''%s'': Result := True;', [ElemList[System.High(ElemList)]]));
  aText.Add('  else');
  aText.Add('    Result := False;');
  aText.Add('  end;');
  aText.Add('end;');
  aText.Add('');
end;

constructor TJtdElements.Create(const aTypeName, aSpecType: string);
begin
  FTypeName := aTypeName;
  FSpecType := aSpecType;
end;

procedure TJtdElements.WriteDeclaration(aText: TStrings; aComment: Boolean);
begin
  WriteDescription(aText, aComment);
  aText.Add(Format('  %s = class sealed(specialize TJtdList<%s>)', [TypeName, FSpecType]));
  aText.Add('    class function GetJtdClass: TJtdEntityClass; override;');
  aText.Add('  end;');
  aText.Add('');
end;

constructor TJtdValues.Create(const aTypeName, aSpecType: string);
begin
  FTypeName := aTypeName;
  FSpecType := aSpecType;
end;

procedure TJtdValues.WriteDeclaration(aText: TStrings; aComment: Boolean);
begin
  WriteDescription(aText, aComment);
  aText.Add(Format('  %s = class sealed(specialize TJtdMap<%s>)', [TypeName, FSpecType]));
  aText.Add('    class function GetJtdClass: TJtdEntityClass; override;');
  aText.Add('  end;');
  aText.Add('');
end;

procedure TJtdProps.WriteClearProc(aText: TStrings);
var
  I: Integer;
begin
  aText.Add(Format('procedure %s.Clear;', [TypeName]));
  aText.Add('begin');
  for I := 0 to System.High(Props) do
    aText.Add(Format('  FreeAndNil(%s);', [Props[I].PropFieldName]));
  for I := 0 to System.High(OptionalProps) do
    aText.Add(Format('  FreeAndNil(%s);', [OptionalProps[I].PropFieldName]));
  aText.Add('end;');
  aText.Add('');
end;

procedure TJtdProps.WriteSetters(aText: TStrings);
  procedure DoWrite(const Info: TJtdPropInfo; aText: TStrings);
  begin
    aText.Add(Format('procedure %s.Set%s(aValue: %s);', [TypeName, Info.PropName, Info.PropType]));
    aText.Add('begin');
    aText.Add(Format('  if aValue = %s then exit;', [Info.PropFieldName]));
    aText.Add(Format('  %s.Free;', [Info.PropFieldName]));
    aText.Add(Format('  %s := aValue;', [Info.PropFieldName]));
    aText.Add('end;');
    aText.Add('');
  end;
var
  I: Integer;
begin
  for I := 0 to System.High(Props) do
    DoWrite(Props[I], aText);
  for I := 0 to System.High(OptionalProps) do
    DoWrite(OptionalProps[I], aText);
end;

procedure TJtdProps.WriteReadPropsFromNode(aText: TStrings);
var
  I: Integer;
begin
  aText.Add('{$PUSH}{$WARN 5057 OFF}');
  aText.Add(Format('procedure %s.DoReadJson(aNode: TJsonNode);', [TypeName]));
  if (Props <> nil) or (OptionalProps <> nil) then begin
    aText.Add('var');
    aText.Add('  p: TJsonNode.TPair;');
    if Props <> nil then
      aText.Add(Format('  Flags: array[0..%d] of Boolean;', [System.High(Props)]));
    aText.Add('  I: Integer;');
    aText.Add('begin');
    aText.Add('  if not aNode.IsObject then ReadError;');
    aText.Add('  Clear;');
    if Props <> nil then
      aText.Add('  System.FillChar(Flags, SizeOf(Flags), 0);');
    aText.Add('  for p in aNode.Entries do');
    if HasAsciiNames then
      aText.Add('    case p.Key of')
    else
      aText.Add('    case TJsonNode.PasStrToAsciiJson(p.Key) of');
    for I := 0 to System.High(Props) do
      begin
        aText.Add(Format('      ''%s'':', [Props[I].JsonPropName]));
        aText.Add('        begin');
        aText.Add(Format('          %s := %s(%s.ReadJson(p.Value));',
          [Props[I].PropFieldName, Props[I].PropType, Props[I].PropType]));
        aText.Add(Format('          Flags[%d] := True;', [I]));
        aText.Add('        end;');
      end;
    for I := 0 to System.High(OptionalProps) do
      begin
        aText.Add(Format('      ''%s'':', [OptionalProps[I].JsonPropName]));
        aText.Add(Format('         %s := %s(%s.ReadJson(p.Value));',
          [OptionalProps[I].PropFieldName, OptionalProps[I].PropType, OptionalProps[I].PropType]));
      end;
    aText.Add('    else');
    if not AdditionalProps then
      aText.Add('      UnknownProp(p.Key);');
    aText.Add('    end;');
    if FProps <> nil then begin
      aText.Add('  for I := 0 to System.High(Flags) do');
      aText.Add('    if not Flags[I] then');
      aText.Add('      case I of');
      for I := 0 to System.High(Props) do
        aText.Add(Format('        %d: PropNotFound(''%s'');', [I, Props[I].JsonPropName]));
      aText.Add('      else');
      aText.Add('      end;');
    end;
  end else
    aText.Add('begin');
  aText.Add('end;');
  aText.Add('{$POP}');
  aText.Add('');
end;

procedure TJtdProps.WriteReadPropsReader(aText: TStrings);
var
  I: Integer;
begin
  aText.Add('{$PUSH}{$WARN 5057 OFF}');
  aText.Add(Format('procedure %s.DoReadJson(aReader: TJsonReader);', [TypeName]));
  if (Props <> nil) or (OptionalProps <> nil) then begin
    aText.Add('var');
    if Props <> nil then
      aText.Add(Format('  Flags: array[0..%d] of Boolean;', [System.High(Props)]));
    aText.Add('  I: Integer;');
  end;
  aText.Add('begin');
  aText.Add('  if aReader.TokenKind <> tkObjectBegin then ReadError;');
  aText.Add('  Clear;');
  if Props <> nil then
    aText.Add('  System.FillChar(Flags, SizeOf(Flags), 0);');
  aText.Add('  repeat');
  aText.Add('    if not aReader.Read then ReadError;');
  aText.Add('    if aReader.TokenKind = tkObjectEnd then break;');
  if (Props <> nil) or (OptionalProps <> nil) then begin
    if HasAsciiNames then
      aText.Add('    case aReader.Name of')
    else
      aText.Add('    case TJsonNode.PasStrToAsciiJson(aReader.Name) of');
    for I := 0 to System.High(Props) do
      begin
        aText.Add(Format('      ''%s'':', [Props[I].JsonPropName]));
        aText.Add('        begin');
        aText.Add(Format('          %s := %s(%s.ReadJson(aReader));',
          [Props[I].PropFieldName, Props[I].PropType, Props[I].PropType]));
        aText.Add(Format('          Flags[%d] := True;', [I]));
        aText.Add('        end;');
      end;
    for I := 0 to System.High(OptionalProps) do
      begin
        aText.Add(Format('      ''%s'':', [OptionalProps[I].JsonPropName]));
        aText.Add(Format('         %s := %s(%s.ReadJson(aReader));',
          [OptionalProps[I].PropFieldName, OptionalProps[I].PropType, OptionalProps[I].PropType]));
      end;
    aText.Add('    else');
    if not AdditionalProps then
      aText.Add('      UnknownProp(aReader.Name);');
    aText.Add('    end;');
  end;
  aText.Add('  until False;');
  if FProps <> nil then begin
    aText.Add('  for I := 0 to System.High(Flags) do');
    aText.Add('    if not Flags[I] then');
    aText.Add('      case I of');
    for I := 0 to System.High(Props) do
      aText.Add(Format('        %d: PropNotFound(''%s'');', [I, Props[I].JsonPropName]));
    aText.Add('      else');
    aText.Add('      end;');
  end;
  aText.Add('end;');
  aText.Add('{$POP}');
  aText.Add('');
end;

procedure TJtdProps.WritePropsWriteJson(aText: TStrings);
var
  I: Integer;
begin
  aText.Add(Format('procedure %s.DoWriteJson(aWriter: TJsonStrWriter);', [TypeName]));
  aText.Add('begin');
  aText.Add('  aWriter.BeginObject;');
  for I := 0 to System.High(Props) do begin
    if HasAsciiNames then
      aText.Add(Format('  aWriter.AddName(''%s'');', [Props[I].JsonPropName]))
    else
      aText.Add(Format('  aWriter.AddName(TJsonNode.JsonStrToPas(''%s''));', [Props[I].JsonPropName]));
    aText.Add(Format('  %s.WriteJson(aWriter);', [Props[I].PropName]));
  end;
  for I := 0 to System.High(OptionalProps) do begin
    aText.Add(Format('  if %s <> nil then begin', [OptionalProps[I].PropName]));
    if HasAsciiNames then
      aText.Add(Format('    aWriter.AddName(''%s'');', [OptionalProps[I].JsonPropName]))
    else
      aText.Add(Format('    aWriter.AddName(TJsonNode.JsonStrToPas(''%s''));', [OptionalProps[I].JsonPropName]));
    aText.Add(Format('    %s.WriteJson(aWriter);', [OptionalProps[I].PropName]));
    aText.Add('  end;');
  end;
  aText.Add('  aWriter.EndObject;');
  aText.Add('end;');
  aText.Add('');
end;

procedure TJtdProps.WritePropDescription(const aProp: TJtdPropInfo; aText: TStrings; aComment: Boolean);
var
  c: string;
begin
  if not aComment then exit;
  if HasAsciiNames then
    c := Format('refers to "%s" JSON property', [aProp.JsonPropName])
  else
    c := Format('refers to %s JSON property', [aProp.JsonPropName]);
  if aProp.Nullable then
    c := c + ';' + NULLABLE_COMMENT;
  if aProp.Descryption <> '' then
    if aProp.Descryption[System.Length(aProp.Descryption)] in [',', '.', ';', '?', '!'] then
      c := aProp.Descryption + ' ' + c
    else
      c := aProp.Descryption + '; ' + c;
  aText.Add('  { ' + SysUtils.WrapText(c, LineEnding + '    ', [' ', '-', #9], 70) + ' }');
end;

procedure TJtdProps.WriteOptPropsWarning(aText: TStrings; aComment: Boolean);
begin
  if aComment and (FOptProps <> nil) then begin
    aText.Add('');
    aText.Add('  { All of the following properties are optional }');
    aText.Add('');
  end;
end;

constructor TJtdProps.Create(const aTypeName: string; const aProps, aOptProps: TJtdPropList);
begin
  FTypeName := aTypeName;
  FProps := aProps;
  FOptProps := aOptProps;
  FAsciiNames := True;
end;

procedure TJtdProps.WriteDeclaration(aText: TStrings; aComment: Boolean);
var
  I: Integer;
begin
  WriteDescription(aText, aComment);
  aText.Add(Format('  %s = class sealed(TJtdObject)', [TypeName]));
  aText.Add('  private');
  for I := 0 to System.High(Props) do
    aText.Add(Format('    %s: %s;', [Props[I].PropFieldName, Props[I].PropType]));
  for I := 0 to System.High(OptionalProps) do
    aText.Add(Format('    %s: %s;', [OptionalProps[I].PropFieldName, OptionalProps[I].PropType]));

  for I := 0 to System.High(Props) do
    aText.Add(Format('    procedure Set%s(aValue: %s);', [Props[I].PropName, Props[I].PropType]));
  for I := 0 to System.High(OptionalProps) do
    aText.Add(Format('    procedure Set%s(aValue: %s);', [OptionalProps[I].PropName, OptionalProps[I].PropType]));

  aText.Add('  protected');
  aText.Add('    procedure DoReadJson(aNode: TJsonNode); override;');
  aText.Add('    procedure DoReadJson(aReader: TJsonReader); override;');
  aText.Add('    procedure DoWriteJson(aWriter: TJsonStrWriter); override;');
  aText.Add('  public');
  aText.Add('    class function GetJtdClass: TJtdEntityClass; override;');
  aText.Add('    procedure Clear; override;');
  for I := 0 to System.High(Props) do begin
    WritePropDescription(Props[I], aText, aComment);
    aText.Add(Format('    property %s: %s read %s write Set%s;',
      [Props[I].PropName, Props[I].PropType, Props[I].PropFieldName, Props[I].PropName]));
  end;
  WriteOptPropsWarning(aText, aComment);
  for I := 0 to System.High(OptionalProps) do begin
    WritePropDescription(OptionalProps[I], aText, aComment);
    aText.Add(Format('    property %s: %s read %s write Set%s;',
      [OptionalProps[I].PropName, OptionalProps[I].PropType, OptionalProps[I].PropFieldName,
       OptionalProps[I].PropName]));
  end;
  aText.Add('  end;');
  aText.Add('');
end;

procedure TJtdProps.WriteImplementation(aText: TStrings);
begin
  inherited WriteImplementation(aText);
  WriteClearProc(aText);
  WriteSetters(aText);
  WriteReadPropsFromNode(aText);
  WriteReadPropsReader(aText);
  WritePropsWriteJson(aText);
end;

procedure TJtdVarTemplate.WritePropDescription(const aDescr, aTagValue: string; aText: TStrings;
  aComment: Boolean);
var
  s: string;
begin
  if not aComment then exit;
  if aDescr = '' then
    aText.Add(Format('  { matches the "%s" tag }', [aTagValue]))
  else begin
    s := SysUtils.WrapText(aDescr, LineEnding + '    ', [' ', '-', #9], 70) + LineEnding +
      Format('    matches the "%s" tag', [aTagValue]);
    aText.Add(Format('  { %s }', [s]));
  end;
end;

procedure TJtdVarTemplate.WriteGetTagJsonName(aText: TStrings);
begin
  aText.Add(Format('class function %s.GetTagJsonName: string;', [TypeName]));
  aText.Add('begin');
  if HasAsciiNames then
    aText.Add(Format('  Result := ''%s'';', [TagJsonName]))
  else
    aText.Add(Format('  Result := TJsonNode.JsonStrToPas(''%s'');', [TagJsonName]));
  aText.Add('end;');
  aText.Add('');
end;

procedure TJtdVarTemplate.WriteValidTagValue(aText: TStrings);
var
  I: Integer;
begin
  aText.Add(Format('class function %s.ValidTagValue(const aValue: string): Boolean;', [TypeName]));
  aText.Add('begin');
  if HasAsciiNames then
    aText.Add('  case aValue of')
  else
    aText.Add('  case TJsonNode.PasStrToAsciiJson(aValue) of');
  for I := 0 to System.High(FMapping) - 1 do
    aText.Add(Format('    ''%s'',', [FMapping[I].TagValue]));
  aText.Add(Format('    ''%s'': Result := True;', [FMapping[System.High(FMapping)].TagValue]));
  aText.Add('  else');
  aText.Add('    Result := False;');
  aText.Add('  end;');
  aText.Add('end;');
  aText.Add('');
end;

procedure TJtdVarTemplate.WriteGetInstanceClass(aText: TStrings);
var
  I: Integer;
begin
  aText.Add(Format('class function %s.GetInstanceClass(const aTag: string): TJtdEntityClass;', [TypeName]));
  aText.Add('begin');
  if HasAsciiNames then
    aText.Add('  case aTag of')
  else
    aText.Add('  case TJsonNode.PasStrToAsciiJson(aTag) of');
  for I := 0 to System.High(Mapping) do
    aText.Add(Format('    ''%s'': Result := %s;', [Mapping[I].TagValue, Mapping[I].PropType]));
  aText.Add('  else');
  aText.Add('    Result := nil;');
  aText.Add('  end;');
  aText.Add('end;');
  aText.Add('');
end;

procedure TJtdVarTemplate.WriteGetters(aText: TStrings);
var
  I: Integer;
begin
  for I := 0 to System.High(Mapping) do begin
    aText.Add(Format('function %s.Get%s: %s;', [TypeName, Mapping[I].PropName, Mapping[I].PropType]));
    aText.Add('begin');
    aText.Add(Format('  Result := FInstance as %s;', [Mapping[I].PropType]));
    aText.Add('end;');
    aText.Add('');
  end;
end;

procedure TJtdVarTemplate.WriteSetters(aText: TStrings);
var
  I: Integer;
begin
  for I := 0 to System.High(Mapping) do begin
    aText.Add(Format('procedure %s.Set%s(aValue: %s);', [TypeName, Mapping[I].PropName, Mapping[I].PropType]));
    aText.Add('begin');
    aText.Add('  if aValue = FInstance then exit;');
    aText.Add('  FInstance.Free;');
    aText.Add('  FInstance := aValue;');
    if HasAsciiNames then
      aText.Add(Format('  FTag := ''%s'';', [Mapping[I].TagValue]))
    else
      aText.Add(Format('  FTag := TJsonNode.JsonStrToPas(''%s'');', [Mapping[I].TagValue]));
    aText.Add('end;');
    aText.Add('');
  end;
end;

constructor TJtdVarTemplate.Create(const aTagName, aTypeName: string; const aMapping: TTagMapping);
begin
  FTagJsonName := aTagName;
  FTypeName := aTypeName;
  FMapping := aMapping;
  FAsciiNames := True;
end;

procedure TJtdVarTemplate.WriteDeclaration(aText: TStrings; aComment: Boolean);
var
  I: Integer;
begin
  WriteDescription(aText, aComment);
  aText.Add(Format('  %s = class sealed(TJtdVariant)', [TypeName]));
  aText.Add('  protected');
  for I := 0 to System.High(Mapping) do
    aText.Add(Format('    function Get%s: %s;', [Mapping[I].PropName, Mapping[I].PropType]));
  for I := 0 to System.High(Mapping) do
    aText.Add(Format('    procedure Set%s(aValue: %s);', [Mapping[I].PropName, Mapping[I].PropType]));
  aText.Add('    class function GetTagJsonName: string; override;');
  aText.Add('    class function ValidTagValue(const aValue: string): Boolean; override;');
  aText.Add('    class function GetInstanceClass(const aTag: string): TJtdEntityClass; override;');
  aText.Add('  public');
  aText.Add('    class function GetJtdClass: TJtdEntityClass; override;');
  for I := 0 to System.High(Mapping) do begin
    WritePropDescription(Mapping[I].Description, Mapping[I].TagValue, aText, aComment);
    aText.Add(Format('    property %s: %s read Get%s write Set%s;',
      [Mapping[I].PropName, Mapping[I].PropType, Mapping[I].PropName, Mapping[I].PropName]));
  end;
  aText.Add('  end;');
  aText.Add('');
end;

procedure TJtdVarTemplate.WriteImplementation(aText: TStrings);
begin
  inherited WriteImplementation(aText);
  WriteGetTagJsonName(aText);
  WriteValidTagValue(aText);
  WriteGetInstanceClass(aText);
  WriteGetters(aText);
  WriteSetters(aText);
end;

{ TJtdTemplater }

function TJtdTemplater.GetTypeAlias(aType: TJtdType): string;
begin
  case aType of
    jtNone:      Result := '';
    jtBool:      Result := 'Bool';
    jtFloat32:   Result := 'Float32';
    jtFloat64:   Result := 'Float64';
    jtInt8:      Result := 'Int8';
    jtUInt8:     Result := 'UInt8';
    jtInt16:     Result := 'Int16';
    jtUInt16:    Result := 'UInt16';
    jtInt32:     Result := 'Int32';
    jtUInt32:    Result := 'UInt32';
    jtString:    Result := 'String';
    jtTimeStamp: Result := 'Timestamp';
  end;
end;

procedure TJtdTemplater.FillPasKeywords;
const
  Keywords =
    'absolute;and;array;asm;begin;case;const;constructor;destructor;div;do;'      +
    'downto;else;end;file;for;function;goto;if;implementation;in;inherited;'      +
    'inline;interface;label;mod;nil;not;object;of;on;operator;or;packed;'         +
    'procedure;program;record;reintroduce;repeat;self;set;shl;shr;string;then;'   +
    'to;type;unit;until;uses;var;while;with;xor;dispose;exit;false;new;true;'     +
    'as;class;dispinterface;except;exports;finalization;finally;initialization;'  +
    'inline;is;library;on;out;packed;property;raise;resourcestring;threadvar;try;'+
    'private;published;length;setlength';
begin
  FPasKeywords.AddAll(Keywords.Split([';'], TStringSplitOptions.ExcludeEmpty));
end;

function TJtdTemplater.IsPasKeyword(const s: string): Boolean;
begin
  Result := FPasKeywords.Contains(LowerCase(s));
end;

function TJtdTemplater.IsIdentifier(const s: string): Boolean;
var
  I: Integer;
begin
  if (s = '') or not (s[1] in ALPHA_CHARS) or IsPasKeyword(s) then exit(False);
  for I := 2 to System.Length(s) do
    if not(s[I] in ALPHANUM_CHARS) then exit(False);
  Result := True;
end;

function TJtdTemplater.IsAsciiEncoded(const s: string): Boolean;
var
  I: Integer;
begin
  for I := 1 to System.Length(s) do
    if not(s[I] in [#32..#127]) then exit(False);
  Result := True;
end;

function TJtdTemplater.IsUniqIdentifierList(const a: array of string): Boolean;
var
  StrSet: TStrSet;
  s: string;
begin
  for s in a do
    if not IsIdentifier(s) or not StrSet.Add(UpCase(s)) then
      exit(False);
  Result := True;
end;

function TJtdTemplater.IsAsciiWordList(const a: array of string): Boolean;
var
  s: string;
begin
  for s in a do
    if not IsAsciiEncoded(s) then
      exit(False);
  Result := True;
end;

function TJtdTemplater.CapitalizeFirst(const s: string): string;
begin
  if s = '' then exit('');
  Result := System.UpCase(s[1]) + System.Copy(s, 2, System.Length(s));
end;

function TJtdTemplater.ProperCase(const s: string): string;
begin
  if s = '' then exit('');
  Result := System.UpCase(s[1]) + LowerCase(System.Copy(s, 2, System.Length(s)));
end;

function TJtdTemplater.AsIdentifier(const s: string): string;
var
  I: Integer;
  p: PChar;
begin
  if s = '' then exit('');
  if IsPasKeyword(s) then
    Result := CapitalizeFirst(s) + KEYWORD_SUFFIX
  else begin
    if not (s[1] in ALPHA_CHARS) then
      Result := ASCII_PREFIX + s
    else
      Result := CapitalizeFirst(s);
    p := Pointer(Result);
    for I := 1 to Pred(System.Length(Result)) do
      if not (p[I] in ALPHANUM_CHARS) then
        p[I] := '_';
  end;
end;

function TJtdTemplater.AsPasIdentifier(const s: string): string;
var
  a: TStringArray;
  I: Integer;
begin
  if s = '' then exit('');
  if not IsIdentifier(s) then exit(AsIdentifier(s));
  a := s.Split(['_'], TStringSplitOptions.ExcludeEmpty);
  if System.Length(a) > 1 then begin
    for I := 0 to System.High(a) do
      a[I] := ProperCase(a[I]);
    Result := string.Join('', a);
  end else Result := CapitalizeFirst(s);
end;

function TJtdTemplater.AsUniqIdentifier(const s: string): string;
begin
  Result := AsUniqIdentifier(s, FGlobNameTable);
end;

function TJtdTemplater.AsUniqIdentifier(const s: string; var aSet: TStrSet): string;
var
  id: string;
  I: Integer = 0;
begin
  if s = '' then exit('');
  id := AsIdentifier(s);
  Result := id;
  while not aSet.Add(Result) do begin
    Inc(I);
    Result := id + I.ToString;
  end;
end;

function TJtdTemplater.AsPasUniqIdentifier(const s: string; var aSet: TStrSet): string;
var
  id: string;
  I: Integer = 0;
begin
  if s = '' then exit('');
  if not IsIdentifier(s) then exit(AsUniqIdentifier(s, aSet));
  id := AsPasIdentifier(s);
  Result := id;
  while not aSet.Add(Result) do begin
    Inc(I);
    Result := id + I.ToString;
  end;
end;

function TJtdTemplater.AsTypeName(const s: string): string;
begin
  if s = '' then exit('');
  Result := 'T' + CapitalizeFirst(AsIdentifier(s));
end;

function TJtdTemplater.AsPasTypeName(const s: string): string;
begin
  if not IsIdentifier(s) then exit(AsTypeName(s));
  Result := 'T' + CapitalizeFirst(AsPasIdentifier(s));
end;

function TJtdTemplater.AsUniqTypeName(const s: string): string;
var
  tn: string;
  I: Integer = 0;
begin
  if s = '' then exit('');
  tn := AsTypeName(s);
  Result := tn;
  while not FGlobNameTable.Add(Result) do begin
    Inc(I);
    Result := tn + I.ToString;
  end;
end;

function TJtdTemplater.AsPasUniqTypeName(const s: string): string;
var
  tn: string;
  I: Integer = 0;
begin
  if not IsIdentifier(s) then exit(AsUniqTypeName(s));
  tn := AsPasTypeName(s);
  Result := tn;
  while not FGlobNameTable.Add(Result) do begin
    Inc(I);
    Result := tn + I.ToString;
  end;
end;

function TJtdTemplater.AsFieldName(const s: string): string;
begin
  if s = '' then exit('');
  Result := 'F' + CapitalizeFirst(s);
end;

procedure TJtdTemplater.CheckUniqEnumElements(const aElems: TStringArray);
var
  s: string;
begin
  if not HasUniqEnumElements then exit;
  for s in aElems do
    if not EnumElemSet.Add(UpCase(s)) then begin
      FUniqEnumElems := False;
      exit;
    end;
end;

function TJtdTemplater.HasDescription(aSchema: TJtdSchema; out aDescr: string): Boolean;
var
  n: TJsonNode;
begin
  if aSchema.Metadata = nil then exit(False);
  Result := aSchema.Metadata.TryGetValue(DESCRIPTION_TAG, n) and n.IsString;
  if Result then aDescr := n.AsString;
end;

function TJtdTemplater.HasEnumDescription(aSchema: TJtdSchema; out aDescr: TJsonNode): Boolean;
begin
  if aSchema.Metadata = nil then exit(False);
  Result := aSchema.Metadata.TryGetValue(ENUM_DESCRIPTION_TAG, aDescr);
end;

function TJtdTemplater.HasPreferName(aSchema: TJtdSchema; out aName: string): Boolean;
var
  n: TJsonNode;
begin
  if aSchema.Metadata = nil then exit(False);
  Result := aSchema.Metadata.TryGetValue(PREFERRED_NAME_TAG, n) and n.IsString;
  if Result then aName := n.AsString;
end;

function TJtdTemplater.GetPreferNameDef(aSchema: TJtdSchema; const aDefault: string): string;
var
  n: TJsonNode;
begin
  Result := aDefault;
  if aSchema.Metadata = nil then exit;
  if aSchema.Metadata.TryGetValue(PREFERRED_NAME_TAG, n) and n.IsString then
    Result := n.AsString;
end;

function TJtdTemplater.HandleEmpty(aSchema: TJtdSchema; const aTypeName: string): string;
var
  Template: TJtdGenAny;
  s, TypName: string;
begin
  TypName := AsUniqTypeName(AsPasIdentifier(GetPreferNameDef(aSchema, aTypeName)) + ANY_TAG);
  Template := TJtdGenAny.Create(TypName);
  if HasDescription(aSchema, s) then
    Template.Description := s;
  Template.Nullable := aSchema.Nullable;
  FTemplateList.Add(Template);
  Result := Template.TypeName;
end;

function TJtdTemplater.HandleRef(aSchema: TJtdSchema): string;
var
  Schema: TJtdSchema;
begin
  if FDefsMap.TryGetValue(aSchema.Ref, Result) then exit;
  Schema := FRootSchema.Definitions[aSchema.Ref];
  Result := HandleSchema(Schema, aSchema.Ref);
  FDefsMap.Add(aSchema.Ref, Result);
end;

function TJtdTemplater.HandleType(aSchema: TJtdSchema; const aTypeName: string): string;
var
  Template: TJtdTypeTemplate;
  s, TypName: string;
begin
  TypName := AsUniqTypeName(AsPasIdentifier(GetPreferNameDef(aSchema, aTypeName)) + GetTypeAlias(aSchema.ElType));
  Template := TJtdTypeTemplate.Create(aSchema.ElType, TypName);
  if HasDescription(aSchema, s) then
    Template.Description := s;
  Template.Nullable := aSchema.Nullable;
  FTemplateList.Add(Template);
  Result := Template.TypeName;
end;

function TJtdTemplater.HandleEnum(aSchema: TJtdSchema; const aTypeName: string): string;
var
  EnumTemplate: TJtdEnumType;
  ElemTemplate: TJtdEnumElem;
  StrEnum: TJtdStrEnumElem;
  ElemList: TStringArray;
  s, TypName, PrefName: string;
  n: TJsonNode;
  I: Integer;
begin
  ElemList := aSchema.Enum.ToArray;
  PrefName := AsPasUniqTypeName(GetPreferNameDef(aSchema, aTypeName));

  s := string.Join('_', ElemList);
  if FTypeMap.TryGetValue(s, TypName) then begin
    if PrefName <> '' then begin
      FTemplateList.Add(TJtdEnumAlias.Create(PrefName, TypName));
      if HasDescription(aSchema, s) then
        FTemplateList[FTemplateList.Count-1].Description := s;
      TypName := PrefName;
    end;
    exit(TypName);
  end;

  if PrefName <> '' then
    TypName := PrefName
  else
    TypName := AsUniqTypeName(ENUM_TAG);

  if IsUniqIdentifierList(ElemList) then begin
    EnumTemplate := TJtdEnumType.Create(TypName, ElemList);
    CheckUniqEnumElements(ElemList);
    FGlobNameTable.Add(EnumTemplate.TypeName);
    FTemplateList.Add(EnumTemplate);
    if HasDescription(aSchema, s) then
      EnumTemplate.Description := s;
    if HasEnumDescription(aSchema, n) then
      EnumTemplate.EnumDescription := n;
    ElemTemplate := TJtdEnumElem.Create(
      AsUniqTypeName(System.Copy(TypName, 2, System.Length(TypName)) + ELEM_TAG), EnumTemplate);
    ElemTemplate.Description := Format(
      'Container for some %s enumeration element', [EnumTemplate.TypeName]);
    ElemTemplate.Nullable := aSchema.Nullable;
    FTemplateList.Add(ElemTemplate);
    Result := ElemTemplate.TypeName;
  end else begin
    StrEnum := TJtdStrEnumElem.Create(TypName, ElemList);
    if not IsAsciiWordList(ElemList) then begin
      for I := 0 to System.High(ElemList) do
        ElemList[I] := TJsonNode.PasStrToAsciiJson(ElemList[I]);
      StrEnum.HasAsciiNames := False;
    end;
    FTemplateList.Add(StrEnum);
    if HasDescription(aSchema, s) then
      StrEnum.Description := s;
    if HasEnumDescription(aSchema, n) then
      StrEnum.EnumDescription := n;
    StrEnum.Nullable := aSchema.Nullable;
    Result := StrEnum.TypeName;
  end;
  FTypeMap.Add(string.Join('_', ElemList), Result);
end;

function TJtdTemplater.HandleElements(aSchema: TJtdSchema; const aTypeName: string): string;
var
  Template: TJtdElements;
  s, ElemName, ElemAlias, TypName: string;
begin
  TypName := AsPasUniqTypeName(GetPreferNameDef(aSchema, aTypeName));
  case aSchema.Elements.Kind of
    fkEmpty:
      begin
        ElemName := TJtdGenAny.DefaultTypeName;
        ElemAlias := ANY_TAG;
      end;
    fkType:
      begin
        ElemName := TJtdTypeTemplate.DefaultTypeName(aSchema.Elements.ElType);
        ElemAlias := GetTypeAlias(aSchema.ElType);
      end;
  else
    ElemName := HandleSchema(aSchema.Elements);
    ElemAlias := System.Copy(ElemName, 2, System.Length(ElemName));
  end;
  if TypName = '' then
    TypName := AsUniqTypeName(ElemAlias + ELEMENTS_TAG);
  Template := TJtdElements.Create(TypName, ElemName);
  if HasDescription(aSchema, s) then
    Template.Description := s;
  Template.Nullable := aSchema.Nullable;
  FTemplateList.Add(Template);
  Result := Template.TypeName;
end;

function TJtdTemplater.HandleValues(aSchema: TJtdSchema; const aTypeName: string): string;
var
  Template: TJtdValues;
  s, ElemName, ElemAlias, TypName: string;
begin
  TypName := AsPasUniqTypeName(GetPreferNameDef(aSchema, aTypeName));
  case aSchema.Values.Kind of
    fkEmpty:
      begin
        ElemName := TJtdGenAny.DefaultTypeName;
        ElemAlias := ANY_TAG;
      end;
    fkType:
      begin
        ElemName := TJtdTypeTemplate.DefaultTypeName(aSchema.Values.ElType);
        ElemAlias := GetTypeAlias(aSchema.ElType);
      end;
  else
    ElemName := HandleSchema(aSchema.Values);
    ElemAlias := System.Copy(ElemName, 2, System.Length(ElemName));
  end;
  if TypName = '' then
    TypName := AsUniqTypeName(ElemAlias + VALUES_TAG);
  Template := TJtdValues.Create(TypName, ElemName);
  if HasDescription(aSchema, s) then
    Template.Description := s;
  Template.Nullable := aSchema.Nullable;
  FTemplateList.Add(Template);
  Result := Template.TypeName;
end;

function TJtdTemplater.HandleProperties(aSchema: TJtdSchema; const aTypeName: string): string;
var
  PropNameSet: TStrSet;
  PropsDesr: TJsonNode = nil;
  PureAsciiNames: Boolean = True;
  function FindPropInfo(aMap: TJtdSchemaMap): TJtdPropList;
  type
    TPropList = specialize TGLiteVector<TJtdPropInfo>;
  var
    List: TPropList;
    Node: TJsonNode;
    PropInfo: TJtdPropInfo;
    e: TJtdSchemaMap.TEntry;
    s: string;
  begin
    for e in aMap do begin
      if PureAsciiNames then
        PropInfo.JsonPropName := e.Key
      else
        PropInfo.JsonPropName := TJsonNode.PasStrToAsciiJson(e.Key);
      if e.Key = '' then
        s := EMPTY_NAME
      else
        s := e.Key;
      PropInfo.PropName := AsPasUniqIdentifier(s, PropNameSet);
      PropInfo.PropFieldName := AsFieldName(PropInfo.PropName);
      case e.Value.Kind of
        fkEmpty: PropInfo.PropType := TJtdGenAny.DefaultTypeName;
        fkType:  PropInfo.PropType := TJtdTypeTemplate.DefaultTypeName(e.Value.ElType);
      else
        PropInfo.PropType := HandleSchema(e.Value, PropInfo.PropName);
      end;
      s := '';
      if (PropsDesr <> nil) and PropsDesr.Find(e.Key, Node) and Node.IsString then
        s := Node.AsString
      else
        if not HasDescription(e.Value, s) then s := '';
      PropInfo.Descryption := s;
      PropInfo.Nullable := e.Value.Nullable;
      List.Add(PropInfo);
    end;
    Result := List.ToArray;
  end;
var
  PropList: TJtdPropList = nil;
  OptPropList: TJtdPropList = nil;
  Template: TJtdProps;
  s, TypName: string;
begin
  TypName := AsPasUniqTypeName(GetPreferNameDef(aSchema, aTypeName));
  if aSchema.Properties <> nil then
    for s in aSchema.Properties.Keys do
      if not IsAsciiEncoded(s) then begin
        PureAsciiNames := False;
        break;
      end;
  if (aSchema.OptionalProperties <> nil) and PureAsciiNames then
    for s in aSchema.OptionalProperties.Keys do
      if not IsAsciiEncoded(s) then begin
        PureAsciiNames := False;
        break;
      end;
  if aSchema.Metadata <> nil then
    if not(aSchema.Metadata.TryGetValue(PROP_DESCRIPTION_TAG, PropsDesr) and PropsDesr.IsObject) then
      PropsDesr := nil;
  if aSchema.Properties <> nil then
    PropList := FindPropInfo(aSchema.Properties);
  if aSchema.OptionalProperties <> nil then
    OptPropList := FindPropInfo(aSchema.OptionalProperties);
  if TypName = '' then
    TypName := AsUniqTypeName(OBJECT_TAG);
  Template := TJtdProps.Create(TypName, PropList, OptPropList);
  Template.HasAsciiNames := PureAsciiNames;
  Template.Nullable := aSchema.Nullable;
  FTemplateList.Add(Template);
  if HasDescription(aSchema, s) then
    Template.Description := s;
  Result := Template.TypeName;
end;

{$PUSH}{$WARN 5091 OFF}
function TJtdTemplater.HandleDiscriminator(aSchema: TJtdSchema; const aTypeName: string): string;
var
  PropNameSet: TStrSet;
  Mapping: TTagMapping;
  s, TypName: string;
  Template: TJtdVarTemplate;
  I: Integer;
  e: TJtdSchemaMap.TEntry;
  AsciiNames: Boolean = True;
begin
  TypName := AsPasUniqTypeName(GetPreferNameDef(aSchema, aTypeName));
  System.SetLength(Mapping, aSchema.Mapping.Count);
  for s in aSchema.Mapping.Keys do
    if not IsAsciiEncoded(s) then begin
      AsciiNames := False;
      break;
    end;
  I := 0;
  for e in aSchema.Mapping do begin
    if AsciiNames then
      Mapping[I].TagValue := e.Key
    else
      Mapping[I].TagValue := TJsonNode.PasStrToAsciiJson(e.Key);
    if HasPreferName(e.Value, s) then
      Mapping[I].PropName := AsUniqIdentifier(s, PropNameSet)
    else begin
      if e.Key = '' then
        s := EMPTY_NAME
      else
        s := e.Key;
      Mapping[I].PropName := AsPasUniqIdentifier(s, PropNameSet);
    end;
    Mapping[I].PropType := HandleSchema(e.Value, Mapping[I].PropName);
    if HasDescription(e.Value, s) then
      Mapping[I].Description := s;
    Inc(I);
  end;
  if TypName = '' then
    TypName := AsUniqTypeName(aSchema.Discriminator + VARIANT_TAG);
  if AsciiNames then
    s := aSchema.Discriminator
  else
    s := TJsonNode.PasStrToAsciiJson(aSchema.Discriminator);
  Template := TJtdVarTemplate.Create(s, TypName, Mapping);
  if HasDescription(aSchema, s) then
    Template.Description := s;
  Template.HasAsciiNames := AsciiNames;
  Template.Nullable := aSchema.Nullable;
  FTemplateList.Add(Template);
  Result := Template.TypeName;
end;
{$POP}

function TJtdTemplater.HandleSchema(aSchema: TJtdSchema; const aParentTag: string): string;
begin
  case aSchema.Kind of
    fkEmpty:         Result := HandleEmpty(aSchema, aParentTag);
    fkRef:           Result := HandleRef(aSchema);
    fkType:          Result := HandleType(aSchema, aParentTag);
    fkEnum:          Result := HandleEnum(aSchema, aParentTag);
    fkElements:      Result := HandleElements(aSchema, aParentTag);
    fkValues:        Result := HandleValues(aSchema, aParentTag);
    fkProperties:    Result := HandleProperties(aSchema, aParentTag);
    fkDiscriminator: Result := HandleDiscriminator(aSchema, aParentTag);
  else
    Result := '';
  end;
end;

function TJtdTemplater.ProcessSchema: string;
begin
  EnumElemSet.Clear;
  FGlobNameTable.Clear;
  FTypeMap.Clear;
  FDefsMap.Clear;
  FTemplateList.Clear;
  Result := HandleSchema(FRootSchema, RootClassName);
end;

constructor TJtdTemplater.Create(aSchema: TJtdSchema);
begin
  FRootSchema := aSchema;
  FTemplateList := TTemplateList.Create;
  FDeclarationList := TStringList.Create;
  FImplementationList := TStringList.Create;
  FUniqEnumElems := True;
  FillPasKeywords;
end;

destructor TJtdTemplater.Destroy;
begin
  FTemplateList.Free;
  FDeclarationList.Free;
  FImplementationList.Free;
  inherited;
end;

function TJtdTemplater.Execute: string;
var
  Template: TJtdTemplate;
begin
  Result := ProcessSchema;
  FDeclarationList.Clear;
  FImplementationList.Clear;
  FDeclarationList.Add('type');
  FDeclarationList.Add('');
  for Template in FTemplateList do begin
    Template.WriteDeclaration(FDeclarationList, ShowComments);
    Template.WriteImplementation(FImplementationList);
  end;
end;

{ TJtdPasCodegen }

procedure TJtdPasCodegen.WriteHeader(aText: TStrings);
begin
  aText.Add('{');
  if CustomHeader <> '' then begin
    aText.Add('  ' + SysUtils.WrapText(CustomHeader, LineEnding + '  ', [' ', '-', #9], 78));
    aText.Add('');
  end;
  aText.Add(Format('  This unit was automatically created by %s, do not edit.', [DISPLAY_NAME]));
  aText.Add('}');
end;

procedure TJtdPasCodegen.WriteUnitClause(aText: TStrings);
begin
  aText.Add('unit %s;',[UnitName]);
  aText.Add('');
end;

procedure TJtdPasCodegen.WriteSwitches(aText: TStrings);
begin
  if FSwitches <> '' then begin
    aText.Add(Switches);
    aText.Add('');
  end;
end;

procedure TJtdPasCodegen.WriteUsesClause(aText: TStrings);
begin
  aText.Add('interface');
  aText.Add('');
  if UseUnits <> nil then begin
    aText.Add('uses');
    aText.Add('  ' + string.Join(', ', UseUnits) + ';');
    aText.Add('');
  end;
end;

procedure TJtdPasCodegen.WriteImplClause(aText: TStrings);
begin
  if aText[Pred(aText.Count)] <> '' then
    aText.Add('');
  aText.Add('implementation');
  aText.Add('');
end;

procedure TJtdPasCodegen.WriteUnitEnd(aText: TStrings);
begin
  if aText[Pred(aText.Count)] <> '' then
    aText.Add('');
  aText.Add('end.');
end;

constructor TJtdPasCodegen.Create(aSchema: TJtdSchema);
begin
  FUseUnits := ['SysUtils', 'lgJson', 'lgJtdTypes'];
  FUnitName := DEFAULT_UNIT_NAME;
  FComments := True;
  FEngine := TJtdTemplater.Create(aSchema);
  FSource := TStringList.Create;
end;

destructor TJtdPasCodegen.Destroy;
begin
  FEngine.Free;
  FSource.Free;
  inherited;
end;

procedure TJtdPasCodegen.RunTemplater;
begin
  FEngine.ShowComments := ShowComments;
  FEngine.RootClassName := PreferRootClassName;
  FFinalRootName := FEngine.Execute;
end;

procedure TJtdPasCodegen.Execute;
begin
  Source.Clear;
  RunTemplater;
  if FEngine.HasUniqEnumElements then
    FSwitches := DEFAULT_SWITCHES
  else
    FSwitches := DEFAULT_SWITCHES + '{$SCOPEDENUMS ON}';
  WriteHeader(Source);
  WriteUnitClause(Source);
  WriteSwitches(Source);
  WriteUsesClause(Source);
  Source.AddStrings(FEngine.DeclarationList);
  WriteImplClause(Source);
  Source.AddStrings(FEngine.ImplementationList);
  WriteUnitEnd(Source);
end;

procedure TJtdPasCodegen.SaveToStream(aStream: TStream);
begin
  if Source.Count = 0 then
    Execute;
  Source.SaveToStream(aStream);
end;

procedure TJtdPasCodegen.SaveToFile(const aFileName: string);
begin
  if Source.Count = 0 then
    Execute;
  Source.SaveToFile(aFileName);
end;

end.
