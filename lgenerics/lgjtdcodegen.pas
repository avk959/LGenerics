{****************************************************************************
*                                                                           *
*   This file is part of the LGenerics package.                             *
*   Pascal code generator from JSON TypeDef schemas.                        *                                            *
*                                                                           *
*   Copyright(c) 2023-2026 A.Koverdyaev(avk)                                *
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

{$MODE OBJFPC}{$H+}{$MODESWITCH ADVANCEDRECORDS}

interface

uses
  Classes, SysUtils,
  lgUtils, lgVector, lgHashSet, lgHashMap, lgJson, lgJsonTypeDef;
{
  supported metadata keywords:

    "description"      - description of the entity, will be displayed in the comments to this entity.

    "enumDescription"  - description of the specific elements of the enumeration.

    "preferredName"    - desired name for the entity (without T), has absolute priority.

    "propsDescription" - descriptions of the properties of the entity, which may not coincide
                         with the descriptions of the corresponding types.

    however, all of these keywords are not portable.
}
type
  ECodegen  = class(Exception);

  TFormKind = TJtdSchema.TFormKind;
  TJtdType  = TJtdSchema.TJtdType;

  { TJtdTemplate }
  TJtdTemplate = class abstract
  strict private
    FNullable: Boolean;
  private
  const
    STOP_CHARS: TSysCharSet = [#9, ' ', '-', ',', '.', ';', '?', '!'];
    SPEC_TAG: string = 'specialize';
  type
    TDependence = record
      DepName: string;
      IsSpecDecl: Boolean;
      constructor Make(const aName: string; aSpec: Boolean = False);
    end;
    TDependList = array of TDependence;
  var
    FDescription,
    FTypeName: string;
    FAsciiNames: Boolean;
    function MakeDescription: string;
    class function IsMultiline(const s: string; out aLines: TStringArray): Boolean; static;
    class function IsSpecDecl(const s: string; out aSpecName: string): Boolean; static;
    class function IsSpecDecl(const s: string; out aGenType, aSpecType: string): Boolean; static;
  public
    constructor Create(aNullable: Boolean);
    procedure WriteDescription(aText: TStrings; aComment: Boolean);
    procedure WriteDeclaration(aText: TStrings; aComment: Boolean); virtual; abstract;
    procedure WriteImplementation(aText: TStrings); virtual;
    function  Dependencies: TDependList; virtual;
    property  Description: string read FDescription write FDescription;
    property  TypeName: string read FTypeName;
    property  HasAsciiNames: Boolean read FAsciiNames write FAsciiNames;
    property  Nullable: Boolean read FNullable;
  end;

  { TJtdTemplater }
  TJtdTemplater = class
  public
  const
    MAX_DEPTH = 512;
  strict private
  type
    TTemplateList = specialize TGObjectVector<TJtdTemplate>;
    TStrSetType   = specialize TGLiteChainHashSet<string, string>;
    TStrSet       = TStrSetType.TSet;
    TStrMapType   = specialize TGLiteChainHashMap<string, string, string>;
    TStrMap       = TStrMapType.TMap;
  var
    FRootSchema: TJtdSchema;
    FPasKeywords,
    EnumElemSet,
    FGlobNameTable: TStrSet;
    FRefMap: TStrMap;
    FTemplateList: TTemplateList;
    FDeclarationList,
    FImplementationList: TStringList;
    FRootClassName: string;
    FDepth: Integer;
    EUsePasEnums,
    FUniqEnumElems,
    FRepeatPropDescrs,
    FComments: Boolean;
  private
    procedure FillPasKeywords;
    function  IsPasKeyword(const s: string): Boolean;
    function  IsIdentifier(const s: string): Boolean;
    function  IsAsciiEncoded(const s: string): Boolean;
    function  InUpperCase(const s: string): Boolean;
    function  IsUniqIdentifierList(const a: array of string): Boolean;
    procedure CheckUniqEnumElements(const aElems: TStringArray);
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
    function  HasDescription(aSchema: TJtdSchema; out aDescr: string): Boolean;
    function  HasEnumDescription(aSchema: TJtdSchema; out aDescr: TJsonNode): Boolean;
    function  HasPreferName(aSchema: TJtdSchema; out aName: string): Boolean;
    function  GetPreferName(aSchema: TJtdSchema; const aPredefName: string; aRoot: Boolean): string;
    function  GetSpecElemAliasName(const aGenType, aSpecType: string): string;
    ////////////////////////////
    function  HandleEmpty(aSchema: TJtdSchema; const aTypeName: string = ''; aRoot: Boolean = False): string;
    function  HandleRef(aSchema: TJtdSchema; const aTypeName: string = ''; aRoot: Boolean = False): string;
    function  HandleType(aSchema: TJtdSchema; const aTypeName: string = ''; aRoot: Boolean = False): string;
    function  HandleEnum(aSchema: TJtdSchema; const aTypeName: string = ''; aRoot: Boolean = False): string;
    function  HandleElements(aSchema: TJtdSchema; const aTypeName: string = ''; aRoot: Boolean = False): string;
    function  HandleValues(aSchema: TJtdSchema; const aTypeName: string = ''; aRoot: Boolean = False): string;
    function  HandleProperties(aSchema: TJtdSchema; const aTypeName: string = ''; aRoot: Boolean = False): string;
    function  HandleDiscriminator(aSchema: TJtdSchema; const aTypeName: string = ''; aRoot: Boolean = False): string;
    function  HandleSchema(aSchema: TJtdSchema; const aTypeName: string = ''; aRoot: Boolean = False): string;
    procedure InitGlobNameTable;
    procedure CheckCyclicDeps;
    function  ProcessSchema: string;
  public
    constructor Create(aSchema: TJtdSchema);
    destructor Destroy; override;
  { returns the type name of the root class }
    function  Execute: string;
  { preferred root class type name(without T), optional }
    property  RootClassName: string read FRootClassName write FRootClassName;
    property  DeclarationList: TStringList read FDeclarationList;
    property  ImplementationList: TStringList read FImplementationList;
    property  HasUniqEnumElements: Boolean read FUniqEnumElems;
    property  ShowComments: Boolean read FComments write FComments;
    property  UsePasEnums: Boolean read EUsePasEnums write EUsePasEnums;
    property  RepeatPropDescriptions: Boolean read FRepeatPropDescrs write FRepeatPropDescrs;
  end;

  TCodegenOption = (
    cgoDisablePasEnums,       // disable using Pascal's enumerations
    cgoRepeatDescriptions,    // repeat the description of the property type
                              // in the property declaration
    cgoDisableComments        // disable code comments
  );
  TCodegenOptions = set of TCodegenOption;

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
    FUsePasEnums,
    FWriteTS,
    FRepeatPropDescrs,
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
    class function  CreateUnit(aSchema: TJtdSchema; const aUnitName: string = '';
                               const aRootClassName: string = ''; const aCustomHeader: string = '';
                               aOptions: TCodegenOptions = []): string; static;
    class procedure CreateUnitFile(const aFileName: string; aSchema: TJtdSchema;
                                   const aUnitName: string = ''; const aRootClassName: string = '';
                                   const aCustomHeader: string = ''; aOptions: TCodegenOptions = []); static;
    constructor Create(aSchema: TJtdSchema; aOptions: TCodegenOptions = []);
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
  { unit name, optional, default is "Unit1" }
    property  UnitName: string read FUnitName write FUnitName;
  { some additional text that will be placed in the unit header, optional }
    property  CustomHeader: string read FCustomHeader write FCustomHeader;
  { True if code comments are enabled }
    property  ShowComments: Boolean read FComments;
  { True if the use of Pascal enumerations is enabled }
    property  UsePasEnums: Boolean read FUsePasEnums;
  { if True, the header will contain the unit creation timestamp, default is False }
    property  WriteTimeStamp: Boolean read FWriteTS write FWriteTS;
    property  RepeatPropDescriptions: Boolean read FRepeatPropDescrs;
  end;

implementation
{$B-}{$COPERATORS ON}{$POINTERMATH ON}

uses
  Math, DateUtils, lgJtdTypes, lgStrConst;

const
  DESCRIPTION_TAG      = 'description';
  ENUM_DESCRIPTION_TAG = 'enumDescription';
  PROP_DESCRIPTION_TAG = 'propsDescription';
  PREFERRED_NAME_TAG   = 'preferredName';
  DEF_ROOT_NAME        = 'RootObject';
  TYPE_TAG             = 'Type';
  ENUM_TAG             = 'Enum';
  ELEM_TAG             = 'Elem';
  LIST_TAG             = 'List';
  LIST_OF_TAG          = 'ListOf';
  MAP_TAG              = 'Map';
  MAP_OF_TAG           = 'MapOf';
  OBJECT_TAG           = 'JObject';
  VARIANT_TAG          = '_Union';
  EMPTY_NAME           = 'EmptyName';
  NULLABLE_COMMENT     = ' is nullable';
  KEYWORD_SUFFIX       = '_';
  ASCII_PREFIX         = 'P';
  ASCII_LOW_CASE       = ['a'..'z'];
  ASCII_LETTERS        = ASCII_LOW_CASE + ['A'..'Z'];
  ALPHA_CHARS          = ASCII_LETTERS + ['_'];
  ALPHANUM_CHARS       = ALPHA_CHARS + ['0'..'9'];

type
  TJtdTypeAlias = class(TJtdTemplate)
  private
    FOldTypeName: string;
  public
    constructor Create(const aTypeName, aOldTypeName: string; aNullable: Boolean);
    procedure WriteDeclaration(aText: TStrings; aComment: Boolean); override;
    function  Dependencies: TDependList; override;
  end;

  TJtdForwardDecl = class(TJtdTemplate)
    constructor Create(const aTypeName: string);
    procedure WriteDeclaration(aText: TStrings; aComment: Boolean); override;
  end;

  TJtdGenAny = class(TJtdTemplate)
  public
    class function DefaultTypeName: string; static;
    constructor Create(const aTypeName: string);
    procedure WriteDeclaration(aText: TStrings; aComment: Boolean); override;
    function  Dependencies: TDependList; override;
  end;

  TJtdTypeTemplate = class(TJtdTemplate)
  private
    FType: TJtdType;
  public
    class function DefaultTypeName(aType: TJtdType): string; static;
    constructor Create(aType: TJtdType; const aTypeName: string; aNullable: Boolean);
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

  TJtdEnumElem = class(TJtdTemplate)
  private
    FEnumType: TJtdEnumType;
  public
    class function GetSpecDecl(const aSpecType: string): string; static;
    constructor Create(const aTypeName: string; aEnumType: TJtdEnumType; aNullable: Boolean);
    procedure WriteDeclaration(aText: TStrings; aComment: Boolean); override;
    function  Dependencies: TDependList; override;
  end;

  TJtdStrEnumElem = class(TJtdTemplate)
  private
    FElemList: TStringArray;
    FEnumDescription: TJsonNode;
  public
    constructor Create(const aTypeName: string; const aElemList: TStringArray; aNullable: Boolean);
    procedure WriteDeclaration(aText: TStrings; aComment: Boolean); override;
    procedure WriteImplementation(aText: TStrings); override;
    property  ElemList: TStringArray read FElemList;
    property  EnumDescription: TJsonNode read FEnumDescription write FEnumDescription;
  end;

  TJtdElements = class(TJtdTemplate)
  private
    FSpecType: string;
  public
    class function GetSpecDecl(const aSpecType: string): string; static;
    constructor Create(const aTypeName, aSpecType: string; aNullable: Boolean);
    procedure WriteDeclaration(aText: TStrings; aComment: Boolean); override;
    function  Dependencies: TDependList; override;
    property  SpecType: string read FSpecType;
  end;

  TJtdValues = class(TJtdTemplate)
  private
    FSpecType: string;
  public
    class function GetSpecDecl(const aSpecType: string): string; static;
    constructor Create(const aTypeName, aSpecType: string; aNullable: Boolean);
    procedure WriteDeclaration(aText: TStrings; aComment: Boolean); override;
    function  Dependencies: TDependList; override;
    property  SpecType: string read FSpecType;
  end;

  TJtdPropInfo = record
    JsonPropName,
    PropName,
    PropFieldName,
    PropType,
    PropDescription,
    Description: string;
    Nullable: Boolean;
  end;
  TJtdPropList = array of TJtdPropInfo;

  TJtdProps = class(TJtdTemplate)
  private
    FProps,
    FOptProps: TJtdPropList;
    FAdditionalProps: Boolean;
    procedure WriteClearProc(aText: TStrings);
    procedure WriteSetters(aText: TStrings);
    procedure WriteReadPropsNodeReader(aText: TStrings);
    procedure WriteReadPropsReader(aText: TStrings);
    procedure WritePropsWriteJson(aText: TStrings);
    procedure WritePropDescription(const aProp: TJtdPropInfo; aText: TStrings; aComment: Boolean);
    procedure WriteOptPropsWarning(aText: TStrings; aComment: Boolean);
  public
    constructor Create(const aTypeName: string; const aProps, aOptProps: TJtdPropList; aNullable: Boolean);
    procedure WriteDeclaration(aText: TStrings; aComment: Boolean); override;
    procedure WriteImplementation(aText: TStrings); override;
    function  Dependencies: TDependList; override;
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

  TJtdUnionTemplate = class(TJtdTemplate)
  private
    FTagJsonName: string;
    FMapping: TTagMapping;
    procedure WritePropDescription(const aDescr, aTagValue: string; aText: TStrings; aComment: Boolean);
    procedure WriteGetTagJsonName(aText: TStrings);
    procedure WriteGetInstanceClass(aText: TStrings);
    procedure WriteGetters(aText: TStrings);
    procedure WriteSetters(aText: TStrings);
  public
    constructor Create(const aTagName, aTypeName: string; const aMapping: TTagMapping; aNullable: Boolean);
    procedure WriteDeclaration(aText: TStrings; aComment: Boolean); override;
    procedure WriteImplementation(aText: TStrings); override;
    function  Dependencies: TDependList; override;
    property  TagJsonName: string read FTagJsonName;
    property  Mapping: TTagMapping read FMapping;
  end;


constructor TJtdTemplate.TDependence.Make(const aName: string; aSpec: Boolean);
begin
  DepName := aName;
  IsSpecDecl := aSpec;
end;

function TJtdTemplate.MakeDescription: string;
var
  s, d: string;
  a: TStringArray;
  I: SizeInt;
begin
  s := '  { ' + TypeName;
  if Nullable then
    s += NULLABLE_COMMENT;
  d := Description;
  if d = '' then exit(s + ' }');
  if Nullable then s += ';';
  s += LineEnding;
  if IsMultiline(d, a) then begin
    Insert([s], a, 0);
    a[System.High(a)] := a[System.High(a)] + ' }';
    for I := 1 to System.High(a) do
      a[I] := '    ' + a[I];
    Result := string.Join(LineEnding, a);
  end else begin
    s += LineEnding + '    ';
    Result := s + SysUtils.WrapText(d, LineEnding + '    ', STOP_CHARS, 76) + ' }';
  end;
end;

class function TJtdTemplate.IsMultiline(const s: string; out aLines: TStringArray): Boolean;
begin
  Result := False;
  with TStringList.Create do
    try
      TrailingLineBreak := False;
      Text := s;
      if Count > 1 then begin
        aLines := ToStringArray;
        Result := True;
      end;
    finally
      Free;
    end;
end;

class function TJtdTemplate.IsSpecDecl(const s: string; out aSpecName: string): Boolean;
var
  KeyPos, OpenPos, ClosePos: SizeInt;
begin
  KeyPos := System.Pos(SPEC_TAG, s);
  Result := KeyPos > 0;
  if Result then
    begin
      OpenPos := System.Pos('<', s, KeyPos + System.Length(SPEC_TAG));
      if OpenPos = 0 then exit(False);
      ClosePos := System.Pos('>', s, OpenPos + 1);
      if ClosePos = 0 then exit(False);
      aSpecName := System.Copy(s, OpenPos + 1, ClosePos - OpenPos - 1).Trim;
    end;
end;

class function TJtdTemplate.IsSpecDecl(const s: string; out aGenType, aSpecType: string): Boolean;
var
  KeyPos, OpenPos, ClosePos: SizeInt;
begin
  KeyPos := System.Pos(SPEC_TAG, s);
  Result := KeyPos > 0;
  if Result then
    begin
      KeyPos += System.Length(SPEC_TAG);
      OpenPos := System.Pos('<', s, KeyPos);
      if OpenPos = 0 then exit(False);
      ClosePos := System.Pos('>', s, OpenPos + 1);
      if ClosePos = 0 then exit(False);
      aGenType := System.Copy(s, KeyPos + 1, OpenPos - KeyPos - 1).Trim;
      aSpecType := System.Copy(s, OpenPos + 1, ClosePos - OpenPos - 1).Trim;
    end;
end;

constructor TJtdTemplate.Create(aNullable: Boolean);
begin
  FNullable := aNullable;
end;

procedure TJtdTemplate.WriteDescription(aText: TStrings; aComment: Boolean);
var
  s: string;
begin
  if aComment then begin
    s := MakeDescription;
    if s <> '' then aText.Add(s);
  end;
end;

procedure TJtdTemplate.WriteImplementation(aText: TStrings);
begin
  Assert(aText = aText);
end;

function TJtdTemplate.Dependencies: TDependList;
begin
  Result := nil;
end;

constructor TJtdTypeAlias.Create(const aTypeName, aOldTypeName: string; aNullable: Boolean);
begin
  inherited Create(aNullable);
  FTypeName := aTypeName;
  FOldTypeName := aOldTypeName;
end;

procedure TJtdTypeAlias.WriteDeclaration(aText: TStrings; aComment: Boolean);
begin
  WriteDescription(aText, aComment);
  aText.Add(Format('  %s = %s;', [TypeName, FOldTypeName]));
  aText.Add('');
end;

function TJtdTypeAlias.Dependencies: TDependList;
begin
  Result := [TDependence.Make(FOldTypeName)];
end;

constructor TJtdForwardDecl.Create(const aTypeName: string);
begin
  inherited Create(False);
  FTypeName := aTypeName;
end;

procedure TJtdForwardDecl.WriteDeclaration(aText: TStrings; aComment: Boolean);
begin
  Assert(aComment = aComment);
  aText.Add(Format('  %s = class;', [TypeName]));
  aText.Add('');
end;

class function TJtdGenAny.DefaultTypeName: string;
begin
  Result := JTD_FORM_ANCESTORS[TJtdFormKind.fkEmpty];
end;

constructor TJtdGenAny.Create(const aTypeName: string);
begin
  inherited Create(True);
  FTypeName := aTypeName;
end;

procedure TJtdGenAny.WriteDeclaration(aText: TStrings; aComment: Boolean);
begin
  WriteDescription(aText, aComment);
  aText.Add(Format('  %s = %s;', [TypeName, DefaultTypeName]));
  aText.Add('');
end;

function TJtdGenAny.Dependencies: TDependList;
begin
  Result := [TDependence.Make(DefaultTypeName)];
end;

class function TJtdTypeTemplate.DefaultTypeName(aType: TJtdType): string;
begin
  if aType = jtNone then exit('');
  Result := JTD_TYPE_IMPL[aType].ClassName;
end;

constructor TJtdTypeTemplate.Create(aType: TJtdType; const aTypeName: string; aNullable: Boolean);
begin
  inherited Create(aNullable);
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
  s := MakeDescription;
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

class function TJtdEnumElem.GetSpecDecl(const aSpecType: string): string;
const
  Fmt = 'specialize %s<%s>';
begin
  Result := Format(Fmt, [JTD_FORM_ANCESTORS[fkEnum], aSpecType]);
end;

constructor TJtdEnumElem.Create(const aTypeName: string; aEnumType: TJtdEnumType; aNullable: Boolean);
const
  Fmt = '%s: container for some %s enumeration element';
begin
  inherited Create(aNullable);
  FTypeName := aTypeName;
  FEnumType := aEnumType;
  Description := Format(Fmt, [aTypeName, aEnumType.TypeName]);
end;

procedure TJtdEnumElem.WriteDeclaration(aText: TStrings; aComment: Boolean);
const
  Fmt = '  %s = class sealed(specialize %s<%s>);';
begin
  WriteDescription(aText, aComment);
  aText.Add(Format(Fmt, [TypeName, JTD_FORM_ANCESTORS[fkEnum], FEnumType.TypeName]));
  aText.Add('');
end;

function TJtdEnumElem.Dependencies: TDependList;
begin
  Result := [TDependence.Make(FEnumType.TypeName)];
end;

constructor TJtdStrEnumElem.Create(const aTypeName: string; const aElemList: TStringArray; aNullable: Boolean);
begin
  inherited Create(aNullable);
  FTypeName := aTypeName;
  FElemList := aElemList;
  FAsciiNames := True;
end;

procedure TJtdStrEnumElem.WriteDeclaration(aText: TStrings; aComment: Boolean);
var
  d: string;
  n: TJsonNode;
  I: Integer;
begin
  if aComment then begin
    d := MakeDescription;
    if (EnumDescription <> nil) and (EnumDescription.IsObject) then begin
      if d <> '' then begin
        Delete(d, System.Length(d), 1);
        d := d + LineEnding;
      end;
      for I := 0 to System.High(ElemList) - 1 do
        if EnumDescription.Find(ElemList[I], n) and n.IsString then
          d := d + Format('    %s :  %s', [ElemList[I], n.AsString]) + LineEnding
        else
          d := d + Format('    %s', [ElemList[I]]) + LineEnding;
      if EnumDescription.Find(ElemList[System.High(ElemList)], n) and n.IsString then
        d := d + Format('    %s :  %s', [ElemList[System.High(ElemList)], n.AsString])
      else
        d := d + Format('    %s', [ElemList[System.High(ElemList)]]);

      if d <> '' then begin
        d := d + ' }';
      end;
    end;
    if d <> '' then
      aText.Add(d);
  end;
  aText.Add(Format('  %s = class sealed(TJtdStrEnum)', [TypeName]));
  aText.Add('  public');
  aText.Add('    class function IsElement(const aValue: string): Boolean; override;');
  aText.Add('  end;');
  aText.Add('');
end;

procedure TJtdStrEnumElem.WriteImplementation(aText: TStrings);
var
  I: Integer;
begin
  aText.Add(Format('{ %s }', [TypeName]));
  aText.Add('');
  aText.Add(Format('class function %s.IsElement(const aValue: string): Boolean;', [TypeName]));
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

class function TJtdElements.GetSpecDecl(const aSpecType: string): string;
const
  Fmt = 'specialize %s<%s>';
begin
  Result := Format(Fmt, [JTD_FORM_ANCESTORS[fkElements], aSpecType]);
end;

constructor TJtdElements.Create(const aTypeName, aSpecType: string; aNullable: Boolean);
begin
  inherited Create(aNullable);
  FTypeName := aTypeName;
  FSpecType := aSpecType;
end;

procedure TJtdElements.WriteDeclaration(aText: TStrings; aComment: Boolean);
const
  Fmt = '  %s = class sealed(specialize %s<%s>);';
begin
  WriteDescription(aText, aComment);
  aText.Add(Format(Fmt, [TypeName, JTD_FORM_ANCESTORS[fkElements], FSpecType]));
  aText.Add('');
end;

function TJtdElements.Dependencies: TDependList;
begin
  Result := [TDependence.Make(FSpecType)];
end;

class function TJtdValues.GetSpecDecl(const aSpecType: string): string;
const
  Fmt = 'specialize %s<%s>';
begin
  Result := Format(Fmt, [JTD_FORM_ANCESTORS[fkValues], aSpecType]);
end;

constructor TJtdValues.Create(const aTypeName, aSpecType: string; aNullable: Boolean);
begin
  inherited Create(aNullable);
  FTypeName := aTypeName;
  FSpecType := aSpecType;
end;

procedure TJtdValues.WriteDeclaration(aText: TStrings; aComment: Boolean);
const
  Fmt = '  %s = class sealed(specialize %s<%s>);';
begin
  WriteDescription(aText, aComment);
  aText.Add(Format(Fmt, [TypeName, JTD_FORM_ANCESTORS[fkValues], FSpecType]));
  aText.Add('');
end;

function TJtdValues.Dependencies: TDependList;
begin
  Result := [TDependence.Make(FSpecType)];
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

procedure TJtdProps.WriteReadPropsNodeReader(aText: TStrings);
var
  I: Integer;
begin
  aText.Add('{$PUSH}{$WARN 5057 OFF}');
  aText.Add(Format('procedure %s.DoReadJson(aNode: TJsonNode);', [TypeName]));
  if Props <> nil then begin
    aText.Add('var');
    aText.Add(Format('  Flags: array[0..%d] of Boolean;', [System.High(Props)]));
    aText.Add('  e: TJsonNode.TPair;');
    aText.Add('  I: Integer;');
  end else
    if OptionalProps <> nil then begin
      aText.Add('var');
      aText.Add('  e: TJsonNode.TPair;');
    end;
  aText.Add('begin');
  aText.Add('  if not aNode.IsObject then ExpectObject(aNode);');
  if (Props <> nil) or (OptionalProps <> nil) then
    aText.Add('  Clear;');
  if Props <> nil then
    aText.Add('  System.FillChar(Flags, SizeOf(Flags), 0);');
  if (Props <> nil) or (OptionalProps <> nil) then begin
    aText.Add('  for e in aNode.Entries do begin');
    aText.Add('    if (FTagField <> '''') and (e.Key = FTagField) then continue;');
    if HasAsciiNames then
      aText.Add('    case e.Key of')
    else
      aText.Add('    case TJsonNode.PasStrToAsciiJson(e.Key) of');
    for I := 0 to System.High(Props) do
      begin
        aText.Add(Format('      ''%s'':', [Props[I].JsonPropName]));
        aText.Add(Format('        if not Flags[%d] then begin', [I]));
        aText.Add(Format('          %s := %s(%s.LoadInstance(e.Value));',
          [Props[I].PropFieldName, Props[I].PropType, Props[I].PropType]));
        aText.Add(Format('          Flags[%d] := True;', [I]));
        aText.Add('        end else DuplicateProp(e.Key);');
      end;
    for I := 0 to System.High(OptionalProps) do
      begin
        aText.Add(Format('      ''%s'':', [OptionalProps[I].JsonPropName]));
        aText.Add(Format('         if %s = nil then', [OptionalProps[I].PropFieldName]));
        aText.Add(Format('           %s := %s(%s.LoadInstance(e.Value))',
          [OptionalProps[I].PropFieldName, OptionalProps[I].PropType, OptionalProps[I].PropType]));
        aText.Add('         else DuplicateProp(e.Key);');
      end;
    aText.Add('    else');
    if not AdditionalProps then
      aText.Add('      UnknownProp(e.Key);');
    aText.Add('    end;');
    aText.Add('  end;');
  end;
  if Props <> nil then begin
    aText.Add('  for I := 0 to System.High(Flags) do');
    aText.Add('    if not Flags[I] then');
    aText.Add('      case I of');
    for I := 0 to System.High(Props) do
      aText.Add(Format('        %d: PropNotFound(''%s'');', [I, Props[I].JsonPropName]));
    aText.Add('      end;');
  end;
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
  if Props <> nil then begin
    aText.Add('var');
    aText.Add(Format('  Flags: array[0..%d] of Boolean;', [System.High(Props)]));
    aText.Add('  I: Integer;');
  end;
  aText.Add('begin');
  aText.Add('  if aReader.TokenKind <> tkObjectBegin then ExpectObject(aReader);');
  if (Props <> nil) or (OptionalProps <> nil) then
    aText.Add('  Clear;');
  if Props <> nil then
    aText.Add('  System.FillChar(Flags, SizeOf(Flags), 0);');
  aText.Add('  repeat');
  aText.Add('    if not aReader.Read then ReaderFail(aReader);');
  aText.Add('    if aReader.TokenKind = tkObjectEnd then break;');
  aText.Add('    if (FTagField <> '''') and (aReader.Name = FTagField) then continue;');
  if (Props <> nil) or (OptionalProps <> nil) then begin
    if HasAsciiNames then
      aText.Add('    case aReader.Name of')
    else
      aText.Add('    case TJsonNode.PasStrToAsciiJson(aReader.Name) of');
    for I := 0 to System.High(Props) do
      begin
        aText.Add(Format('      ''%s'':', [Props[I].JsonPropName]));
        aText.Add(Format('        if not Flags[%d] then begin', [I]));
        aText.Add(Format('          %s := %s(%s.LoadInstance(aReader));',
          [Props[I].PropFieldName, Props[I].PropType, Props[I].PropType]));
        aText.Add(Format('          Flags[%d] := True;', [I]));
        aText.Add('        end else DuplicateProp(aReader);');
      end;
    for I := 0 to System.High(OptionalProps) do
      begin
        aText.Add(Format('      ''%s'':', [OptionalProps[I].JsonPropName]));
        aText.Add(Format('         if %s = nil then', [OptionalProps[I].PropFieldName]));
        aText.Add(Format('           %s := %s(%s.LoadInstance(aReader))',
          [OptionalProps[I].PropFieldName, OptionalProps[I].PropType, OptionalProps[I].PropType]));
        aText.Add('         else DuplicateProp(aReader);');
      end;
    aText.Add('    else');
    if AdditionalProps then begin
      aText.Add('      if aReader.IsStartToken(aReader.TokenKind) then aReader.Skip;');
    end else
      aText.Add('      UnknownProp(aReader.Name, aReader);');
    aText.Add('    end;');
  end;
  aText.Add('  until False;');
  if Props <> nil then begin
    aText.Add('  for I := 0 to System.High(Flags) do');
    aText.Add('    if not Flags[I] then');
    aText.Add('      case I of');
    for I := 0 to System.High(Props) do
      aText.Add(Format('        %d: PropNotFound(''%s'', aReader);', [I, Props[I].JsonPropName]));
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
  aText.Add(Format('procedure %s.DoWriteProps(aWriter: TJsonStrWriter);', [TypeName]));
  aText.Add('begin');
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
  aText.Add('end;');
  aText.Add('');
end;

procedure TJtdProps.WritePropDescription(const aProp: TJtdPropInfo; aText: TStrings; aComment: Boolean);
var
  d, c: string;
  a: TStringArray;
  I: SizeInt;
begin
  if not aComment then exit;
  if HasAsciiNames then
    c := Format('  { refers to "%s" JSON property', [aProp.JsonPropName])
  else
    c := Format('  { refers to %s JSON property', [aProp.JsonPropName]);
  if aProp.Nullable then
    c += ';' + NULLABLE_COMMENT;
  if aProp.PropDescription <> '' then begin
    d := aProp.PropDescription;
    if IsMultiline(d, a) then begin
      Insert([c + ';'], a, 0);
      for I := 1 to System.High(a) do
        a[I] := '    ' + a[I];
      c := string.Join(LineEnding, a);
    end else begin
      c += '; ' + LineEnding + '    ';
      c += SysUtils.WrapText(d, LineEnding + '    ', STOP_CHARS, 76);
    end;
    if aProp.Description <> '' then begin
      d := aProp.Description;
      c += LineEnding + LineEnding;
      if IsMultiline(d, a) then begin
        for I := 1 to System.High(a) do
          a[I] := '    ' + a[I];
        c += string.Join(LineEnding, a);
      end else
        c += SysUtils.WrapText(d, LineEnding + '    ', STOP_CHARS, 76);
    end;
  end else
    if aProp.Description <> '' then begin
      d := aProp.Description;
      if IsMultiline(d, a) then begin
        Insert([c + ';'], a, 0);
        for I := 1 to System.High(a) do
          a[I] := '    ' + a[I];
        c := string.Join(LineEnding, a);
      end else begin
        c += ';' + LineEnding + '    ';
        c += SysUtils.WrapText(d, LineEnding + '    ', STOP_CHARS, 76);
      end;
    end;
  aText.Add(c + ' }');
end;

procedure TJtdProps.WriteOptPropsWarning(aText: TStrings; aComment: Boolean);
begin
  if aComment and (FOptProps <> nil) then begin
    aText.Add('');
    aText.Add('  { All of the following properties are optional, so may contain NILs }');
    aText.Add('');
  end;
end;

constructor TJtdProps.Create(const aTypeName: string; const aProps, aOptProps: TJtdPropList; aNullable: Boolean);
begin
  inherited Create(aNullable);
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
  aText.Add(
    Format('  %s = class sealed(%s)', [TypeName, JTD_FORM_ANCESTORS[fkProperties]]));
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
  aText.Add('    procedure DoWriteProps(aWriter: TJsonStrWriter); override;');
  aText.Add('  public');
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
  aText.Add(Format('{ %s }', [TypeName]));
  aText.Add('');
  WriteSetters(aText);
  WriteReadPropsNodeReader(aText);
  WriteReadPropsReader(aText);
  WritePropsWriteJson(aText);
  WriteClearProc(aText);
end;

function TJtdProps.Dependencies: TDependList;
var
  I: SizeInt;
  s, tn: string;
  Deps: specialize TGLiteVector<TDependence>;
begin
  for I := 0 to System.High(Props) do begin
    s := Props[I].PropType;
    if s = TypeName then continue;
    if IsSpecDecl(s, tn) then begin
      if tn <> TypeName then
        Deps.Add(TDependence.Make(tn, True));
    end else
      Deps.Add(TDependence.Make(s));
  end;
  for I := 0 to System.High(OptionalProps) do begin
    s := OptionalProps[I].PropType;
    if s = TypeName then continue;
    if IsSpecDecl(s, tn) then begin
      if tn <> TypeName then
        Deps.Add(TDependence.Make(tn, True));
    end else
      Deps.Add(TDependence.Make(s));
  end;
  Result := Deps.ToArray;
end;

procedure TJtdUnionTemplate.WritePropDescription(const aDescr, aTagValue: string; aText: TStrings;
  aComment: Boolean);
var
  c: string;
  a: TStringArray;
  I: SizeInt;
begin
  if not aComment then exit;
  c := Format('  { matches the "%s" tag', [aTagValue]);
  if aDescr = '' then begin
    aText.Add(c + ' }');
    exit;
  end;
  if IsMultiline(aDescr, a) then begin
    Insert([c + ';'], a, 0);
    a[System.High(a)] := a[System.High(a)] + ' }';
    for I := 1 to System.High(a) do
      a[I] := '    ' + a[I];
    aText.Add(string.Join(LineEnding, a));
  end else begin
    c += ';' + LineEnding + '    ';
    aText.Add(c + SysUtils.WrapText(aDescr, LineEnding + '    ', STOP_CHARS, 76) + ' }');
  end;
end;

procedure TJtdUnionTemplate.WriteGetTagJsonName(aText: TStrings);
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

procedure TJtdUnionTemplate.WriteGetInstanceClass(aText: TStrings);
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

procedure TJtdUnionTemplate.WriteGetters(aText: TStrings);
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

procedure TJtdUnionTemplate.WriteSetters(aText: TStrings);
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

constructor TJtdUnionTemplate.Create(const aTagName, aTypeName: string; const aMapping: TTagMapping;
  aNullable: Boolean);
begin
  inherited Create(aNullable);
  FTagJsonName := aTagName;
  FTypeName := aTypeName;
  FMapping := aMapping;
  FAsciiNames := True;
end;

procedure TJtdUnionTemplate.WriteDeclaration(aText: TStrings; aComment: Boolean);
var
  I: Integer;
begin
  WriteDescription(aText, aComment);
  aText.Add(Format('  %s = class sealed(%s)', [TypeName, JTD_FORM_ANCESTORS[fkDiscriminator]]));
  aText.Add('  protected');
  for I := 0 to System.High(Mapping) do
    aText.Add(Format('    function  Get%s: %s;', [Mapping[I].PropName, Mapping[I].PropType]));
  for I := 0 to System.High(Mapping) do
    aText.Add(Format('    procedure Set%s(aValue: %s);', [Mapping[I].PropName, Mapping[I].PropType]));
  aText.Add('    class function GetTagJsonName: string; override;');
  aText.Add('    class function GetInstanceClass(const aTag: string): TJtdEntityClass; override;');
  aText.Add('  public');
  for I := 0 to System.High(Mapping) do begin
    WritePropDescription(Mapping[I].Description, Mapping[I].TagValue, aText, aComment);
    aText.Add(Format('    property %s: %s read Get%s write Set%s;',
      [Mapping[I].PropName, Mapping[I].PropType, Mapping[I].PropName, Mapping[I].PropName]));
  end;
  aText.Add('  end;');
  aText.Add('');
end;

procedure TJtdUnionTemplate.WriteImplementation(aText: TStrings);
begin
  aText.Add(Format('{ %s }', [TypeName]));
  aText.Add('');
  WriteGetTagJsonName(aText);
  WriteGetInstanceClass(aText);
  WriteGetters(aText);
  WriteSetters(aText);
end;

function TJtdUnionTemplate.Dependencies: TDependList;
var
  I: SizeInt;
  s, tn: string;
  sa: TDependList;
begin
  SetLength(sa, System.Length(Mapping));
  for I := 0 to System.High(Mapping) do begin
    s := Mapping[I].PropType;
    if IsSpecDecl(s, tn) then
      sa[I] := TDependence.Make(tn, True)
    else
      sa[I] := TDependence.Make(s);
  end;
  Result := sa;
end;

{ TJtdTemplater }

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

function TJtdTemplater.InUpperCase(const s: string): Boolean;
var
  I: Integer;
begin
  for I := 1 to System.Length(s) do
    if not(s[I] in ALPHANUM_CHARS) or (s[I] in ASCII_LOW_CASE) then
      exit(False);
  Result := True;
end;

function TJtdTemplater.IsUniqIdentifierList(const a: array of string): Boolean;
var
  ss: TStrSet;
  s: string;
begin
  for s in a do begin
    if not IsIdentifier(s) or FGlobNameTable.Contains(UpCase(s)) then
      exit(False);
    if not ss.Add(UpCase(s)) then
      exit(False);
  end;
  Result := True;
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
  if IsPasKeyword(s) then exit(CapitalizeFirst(s) + KEYWORD_SUFFIX);
  if not (s[1] in ALPHA_CHARS) then
    Result := ASCII_PREFIX + s
  else
    Result := CapitalizeFirst(s);
  p := Pointer(Result);
  for I := 1 to Pred(System.Length(Result)) do
    if not (p[I] in ALPHANUM_CHARS) then
      p[I] := '_';
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
  end else
    if InUpperCase(s) then
      Result := ProperCase(s)
    else
      Result := CapitalizeFirst(s);
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
  while not aSet.Add(UpCase(Result)) do begin
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
  while not aSet.Add(UpCase(Result)) do begin
    Inc(I);
    Result := id + I.ToString;
  end;
end;

function TJtdTemplater.AsTypeName(const s: string): string;
begin
  if s = '' then exit('');
  Result := 'T' + AsIdentifier(s);
end;

function TJtdTemplater.AsPasTypeName(const s: string): string;
begin
  if not IsIdentifier(s) then exit(AsTypeName(s));
  Result := 'T' + AsPasIdentifier(s);
end;

function TJtdTemplater.AsUniqTypeName(const s: string): string;
var
  tn: string;
  I: Integer = 0;
begin
  if s = '' then exit('');
  tn := AsTypeName(s);
  Result := tn;
  while not FGlobNameTable.Add(UpCase(Result)) do begin
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
  while not FGlobNameTable.Add(UpCase(Result)) do begin
    Inc(I);
    Result := tn + I.ToString;
  end;
end;

function TJtdTemplater.AsFieldName(const s: string): string;
begin
  if s = '' then exit('');
  Result := 'F' + CapitalizeFirst(s);
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

function TJtdTemplater.GetPreferName(aSchema: TJtdSchema; const aPredefName: string; aRoot: Boolean): string;
var
  s: string;
begin
  if aPredefName <> '' then exit(aPredefName);
  if HasPreferName(aSchema, s) then exit(AsUniqTypeName(s));
  if aRoot then
    Result := AsUniqTypeName(DEF_ROOT_NAME)
  else
    Result := '';
end;

function TJtdTemplater.GetSpecElemAliasName(const aGenType, aSpecType: string): string;
var
  Suffix: string;
begin
  if aGenType = JTD_FORM_ANCESTORS[fkElements] then begin
    Suffix := System.Copy(
      aSpecType, Succ(System.Length(aSpecType)-System.Length(LIST_TAG)), System.Length(aSpecType));
    if Suffix = LIST_TAG then
      Result := AsUniqTypeName(LIST_OF_TAG + System.Copy(aSpecType, 2, System.Length(aSpecType)))
    else
      Result := AsUniqTypeName(System.Copy(aSpecType, 2, System.Length(aSpecType)) + LIST_TAG)
  end else begin
    Suffix := System.Copy(
      aSpecType, Succ(System.Length(aSpecType)-System.Length(MAP_TAG)), System.Length(aSpecType));
    if Suffix = MAP_TAG then
      Result := AsUniqTypeName(MAP_OF_TAG + System.Copy(aSpecType, 2, System.Length(aSpecType)))
    else
      Result := AsUniqTypeName(System.Copy(aSpecType, 2, System.Length(aSpecType)) + MAP_TAG);
  end;
end;

function TJtdTemplater.HandleEmpty(aSchema: TJtdSchema; const aTypeName: string; aRoot: Boolean): string;
var
  Template: TJtdGenAny;
  d, TypName: string;
begin
  TypName := GetPreferName(aSchema, aTypeName, aRoot);

  if TypName = '' then exit(TJtdGenAny.DefaultTypeName);

  Template := TJtdGenAny.Create(TypName);
  if HasDescription(aSchema, d) then
    Template.Description := d;
  FTemplateList.Add(Template);
  Result := Template.TypeName;
end;

function TJtdTemplater.HandleRef(aSchema: TJtdSchema; const aTypeName: string; aRoot: Boolean): string;
var
  Schema: TJtdSchema;
  TypName, Alias, r, d: string;
  Template: TJtdTypeAlias;
begin
  if FRefMap.TryGetValue(aSchema.Ref, TypName) then exit(TypName);
  Alias := '';
  TypName := '';
  if aRoot then
    Alias := GetPreferName(aSchema, aTypeName, aRoot)
  else
    TypName := GetPreferName(aSchema, '', aRoot);
  if TypName = '' then
    TypName := AsPasUniqTypeName(aSchema.Ref);

  FRefMap.Add(aSchema.Ref, TypName);
  Schema := FRootSchema.Definitions[aSchema.Ref];
  r := HandleSchema(Schema, TypName);
  if Schema.Kind = fkRef then begin
    Template := TJtdTypeAlias.Create(TypName, r, Schema.Nullable);
    if HasDescription(Schema, d) then
      Template.Description := d
    else
      Template.Description := '';
    FTemplateList.Add(Template);
  end;
  if aRoot then begin
    Template := TJtdTypeAlias.Create(Alias, TypName, aSchema.Nullable);
    if HasDescription(aSchema, d) then
      Template.Description := d
    else
      Template.Description := '';
    FTemplateList.Add(Template);
    Result := Alias;
  end else
    Result := TypName;
end;

function TJtdTemplater.HandleType(aSchema: TJtdSchema; const aTypeName: string; aRoot: Boolean): string;
var
  Template: TJtdTypeTemplate;
  d, TypName: string;
begin
  TypName := GetPreferName(aSchema, aTypeName, aRoot);

  if TypName = '' then
    exit(TJtdTypeTemplate.DefaultTypeName(aSchema.ElType));

  Template := TJtdTypeTemplate.Create(aSchema.ElType, TypName, aSchema.Nullable);
  if HasDescription(aSchema, d) then
    Template.Description := d;
  FTemplateList.Add(Template);
  Result := Template.TypeName;
end;

function TJtdTemplater.HandleEnum(aSchema: TJtdSchema; const aTypeName: string; aRoot: Boolean): string;
var
  EnumTemplate: TJtdEnumType;
  ElemTemplate: TJtdEnumElem;
  StrEnum: TJtdStrEnumElem;
  ElemList: TStringArray;
  d, TypName, EnumName: string;
  n: TJsonNode;
  I: Integer;
begin
  TypName := GetPreferName(aSchema, aTypeName, aRoot);

  ElemList := aSchema.Enum.ToArray;

  if UsePasEnums and IsUniqIdentifierList(ElemList) and (System.Length(ElemList) <= 256) then begin
    CheckUniqEnumElements(ElemList);
    if TypName = '' then
      EnumName := AsUniqTypeName(ENUM_TAG)
    else begin
      d := System.Copy(TypName, 2, System.Length(TypName));
      if Pos(UpCase(ENUM_TAG), UpCase(d)) > 0 then
        EnumName := AsUniqTypeName(d + TYPE_TAG)
      else
        EnumName := AsUniqTypeName(d + ENUM_TAG);
    end;
    EnumTemplate := TJtdEnumType.Create(EnumName, ElemList);
    FGlobNameTable.Add(UpCase(EnumTemplate.TypeName));
    FTemplateList.Add(EnumTemplate);
    if HasDescription(aSchema, d) then
      EnumTemplate.Description := d;
    if HasEnumDescription(aSchema, n) then
      EnumTemplate.EnumDescription := n;
    if TypName = '' then exit(TJtdEnumElem.GetSpecDecl(EnumName));
    ElemTemplate := TJtdEnumElem.Create(TypName, EnumTemplate, aSchema.Nullable);
    FTemplateList.Add(ElemTemplate);
    Result := ElemTemplate.TypeName;
  end else begin
    if TypName = '' then
      TypName := AsUniqTypeName(ENUM_TAG);
    StrEnum := TJtdStrEnumElem.Create(TypName, ElemList, aSchema.Nullable);
    if not IsAsciiWordList(ElemList) then begin
      for I := 0 to System.High(ElemList) do
        ElemList[I] := TJsonNode.PasStrToAsciiJson(ElemList[I]);
      StrEnum.HasAsciiNames := False;
    end;
    FTemplateList.Add(StrEnum);
    if HasDescription(aSchema, d) then
      StrEnum.Description := d;
    if HasEnumDescription(aSchema, n) then
      StrEnum.EnumDescription := n;
    Result := StrEnum.TypeName;
  end;
end;

function TJtdTemplater.HandleElements(aSchema: TJtdSchema; const aTypeName: string; aRoot: Boolean): string;
var
  Template: TJtdElements;
  s, d, ElemName, TypName: string;
begin
  TypName := GetPreferName(aSchema, aTypeName, aRoot);

  ElemName := HandleSchema(aSchema.Elements);

  if TypName = '' then begin
    if TJtdTemplate.IsSpecDecl(ElemName, s, d) then begin
      d := GetSpecElemAliasName(s, d);
      FTemplateList.Add(TJtdTypeAlias.Create(d, ElemName, aSchema.Elements.Nullable));
      ElemName := d;
    end;
    exit(TJtdElements.GetSpecDecl(ElemName));
  end;

  if TJtdTemplate.IsSpecDecl(ElemName, d) then begin
    d := AsUniqTypeName(System.Copy(TypName, 2, System.Length(TypName)) + ELEM_TAG);
    FTemplateList.Add(TJtdTypeAlias.Create(d, ElemName, aSchema.Elements.Nullable));
    ElemName := d;
  end;

  Template := TJtdElements.Create(TypName, ElemName, aSchema.Nullable);
  if HasDescription(aSchema, d) then
    Template.Description := d;
  FTemplateList.Add(Template);
  Result := Template.TypeName;
end;

function TJtdTemplater.HandleValues(aSchema: TJtdSchema; const aTypeName: string; aRoot: Boolean): string;
var
  Template: TJtdValues;
  s, d, ElemName, TypName: string;
begin
  TypName := GetPreferName(aSchema, aTypeName, aRoot);

  ElemName := HandleSchema(aSchema.Values);

  if TypName = '' then begin
    if TJtdTemplate.IsSpecDecl(ElemName, s, d) then begin
      d := GetSpecElemAliasName(s, d);
      FTemplateList.Add(TJtdTypeAlias.Create(d, ElemName, aSchema.Values.Nullable));
      ElemName := d;
    end;
    exit(TJtdValues.GetSpecDecl(ElemName));
  end;

  if TJtdTemplate.IsSpecDecl(ElemName, d) then begin
    d := AsUniqTypeName(System.Copy(TypName, 2, System.Length(TypName)) + ELEM_TAG);
    FTemplateList.Add(TJtdTypeAlias.Create(d, ElemName, aSchema.Values.Nullable));
    ElemName := d;
  end;

  Template := TJtdValues.Create(TypName, ElemName, aSchema.Nullable);
  if HasDescription(aSchema, d) then
    Template.Description := d;
  FTemplateList.Add(Template);
  Result := Template.TypeName;
end;

function TJtdTemplater.HandleProperties(aSchema: TJtdSchema; const aTypeName: string; aRoot: Boolean): string;
var
  PropNameSet: TStrSet;
  PropsDescr: TJsonNode = nil;
  PureAsciiNames: Boolean = True;
  function FindPropInfo(aMap: TJtdSchemaMap): TJtdPropList;
  type
    TPropList = specialize TGLiteVector<TJtdPropInfo>;
  var
    List: TPropList;
    Node: TJsonNode;
    PropInfo: TJtdPropInfo;
    e: TJtdSchemaMap.TEntry;
    d: string;
  begin
    for e in aMap do begin
      if PureAsciiNames then
        PropInfo.JsonPropName := e.Key
      else
        PropInfo.JsonPropName := TJsonNode.PasStrToAsciiJson(e.Key);
      if e.Key = '' then
        d := EMPTY_NAME
      else
        d := e.Key;
      PropInfo.PropName := AsPasUniqIdentifier(d, PropNameSet);
      PropInfo.PropFieldName := AsFieldName(PropInfo.PropName);
      if HasPreferName(e.Value, d) or not(PureAsciiNames and(e.Value.Kind in[fkEnum,fkProperties,fkDiscriminator]))then
        PropInfo.PropType := HandleSchema(e.Value)
      else
        PropInfo.PropType := HandleSchema(e.Value, AsUniqTypeName(PropInfo.PropName));
      if (PropsDescr <> nil) and PropsDescr.Find(e.Key, Node) and Node.IsString then
        PropInfo.PropDescription := Node.AsString
      else
        PropInfo.PropDescription := '';
      if TJtdTemplate.IsSpecDecl(PropInfo.PropType, d) or       //////////////////////
         RepeatPropDescriptions or (e.Value.Kind = fkRef) then  ////////////////////////////
        if HasDescription(e.Value, d) then                      ///////////
          PropInfo.Description := d
        else
          PropInfo.Description := '';
      PropInfo.Nullable := e.Value.Nullable;
      List.Add(PropInfo);
    end;
    Result := List.ToArray;
  end;
var
  PropList: TJtdPropList = nil;
  OptPropList: TJtdPropList = nil;
  Template: TJtdProps;
  d, TypName: string;
begin
  TypName := GetPreferName(aSchema, aTypeName, aRoot);

  if TypName = '' then
    TypName := AsUniqTypeName(OBJECT_TAG);

  if aSchema.Properties <> nil then
    for d in aSchema.Properties.Keys do
      if not IsAsciiEncoded(d) then begin
        PureAsciiNames := False;
        break;
      end;
  if (aSchema.OptionalProperties <> nil) and PureAsciiNames then
    for d in aSchema.OptionalProperties.Keys do
      if not IsAsciiEncoded(d) then begin
        PureAsciiNames := False;
        break;
      end;
  if aSchema.Metadata <> nil then
    if not(aSchema.Metadata.TryGetValue(PROP_DESCRIPTION_TAG, PropsDescr) and PropsDescr.IsObject) then
      PropsDescr := nil;
  if aSchema.Properties <> nil then
    PropList := FindPropInfo(aSchema.Properties);
  if aSchema.OptionalProperties <> nil then
    OptPropList := FindPropInfo(aSchema.OptionalProperties);
  Template := TJtdProps.Create(TypName, PropList, OptPropList, aSchema.Nullable);
  Template.HasAsciiNames := PureAsciiNames;
  FTemplateList.Add(Template);
  if HasDescription(aSchema, d) then
    Template.Description := d;
  Template.AdditionalProps := aSchema.AdditionalProperties;
  Result := Template.TypeName;
end;

{$PUSH}{$WARN 5091 OFF}
function TJtdTemplater.HandleDiscriminator(aSchema: TJtdSchema; const aTypeName: string; aRoot: Boolean): string;
var
  PropNameSet: TStrSet;
  Mapping: TTagMapping;
  s, TypName: string;
  Template: TJtdUnionTemplate;
  I: Integer;
  e: TJtdSchemaMap.TEntry;
  AsciiNames: Boolean;
begin
  TypName := GetPreferName(aSchema, aTypeName, aRoot);

  if TypName = '' then
    TypName := AsPasUniqTypeName(aSchema.Discriminator + VARIANT_TAG);

  AsciiNames := IsAsciiEncoded(aSchema.Discriminator);

  System.SetLength(Mapping, aSchema.Mapping.Count);
  if AsciiNames then
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
    s := '';
    Mapping[I].PropType := HandleSchema(e.Value, AsPasUniqTypeName(Mapping[I].PropName));
    if TJtdTemplate.IsSpecDecl(Mapping[I].PropType, s) or RepeatPropDescriptions then /////////
      if HasDescription(e.Value, s) then  /////////////////////////
        Mapping[I].Description := s
      else
        Mapping[I].Description := '';
    Inc(I);
  end;
  if AsciiNames then
    s := aSchema.Discriminator
  else
    s := TJsonNode.PasStrToAsciiJson(aSchema.Discriminator);
  Template := TJtdUnionTemplate.Create(s, TypName, Mapping, aSchema.Nullable);
  if HasDescription(aSchema, s) then
    Template.Description := s;
  Template.HasAsciiNames := AsciiNames;
  FTemplateList.Add(Template);
  Result := Template.TypeName;
end;
{$POP}

function TJtdTemplater.HandleSchema(aSchema: TJtdSchema; const aTypeName: string; aRoot: Boolean): string;
begin
  if FDepth = MAX_DEPTH then
    raise ECodegen.Create(SEMaxRecurseDepthExceed);
  Inc(FDepth);
  case aSchema.Kind of
    fkEmpty:         Result := HandleEmpty(aSchema, aTypeName, aRoot);
    fkRef:           Result := HandleRef(aSchema, aTypeName, aRoot);
    fkType:          Result := HandleType(aSchema, aTypeName, aRoot);
    fkEnum:          Result := HandleEnum(aSchema, aTypeName, aRoot);
    fkElements:      Result := HandleElements(aSchema, aTypeName, aRoot);
    fkValues:        Result := HandleValues(aSchema, aTypeName, aRoot);
    fkProperties:    Result := HandleProperties(aSchema, aTypeName, aRoot);
    fkDiscriminator: Result := HandleDiscriminator(aSchema, aTypeName, aRoot);
  else
    Result := '';
  end;
  Dec(FDepth);
end;

procedure TJtdTemplater.InitGlobNameTable;
var
  I: TJtdType;
begin
  FGlobNameTable.Clear;
  FGlobNameTable.Add(UpCase(TJtdGenAny.DefaultTypeName));
  for I := jtBool to jtTimeStamp do
    FGlobNameTable.Add(UpCase(TJtdTypeTemplate.DefaultTypeName(I)));
end;

type
  TTempVec = specialize TGLiteVector<TJtdTemplate>;
  TDeferData = record
    Index: SizeInt;
    Defers: TTempVec;
    constructor Make(aIndex: SizeInt);
  end;

  TDeferMapType = specialize TGLiteChainHashMap<string, TDeferData, string>;
  TDeferMap     = TDeferMapType.TMap;

constructor TDeferData.Make(aIndex: SizeInt);
begin
  Index := aIndex;
end;

{$PUSH}{$WARN 5089 OFF : Local variable "$1" of a managed type does not seem to be initialized}
procedure TJtdTemplater.CheckCyclicDeps;
var
  Declared: TStrSet;
  DeferMap: TDeferMap;
  procedure AddTemplate(aTemplate: TJtdTemplate);
  var
    Template: TJtdTemplate;
    p: TDeferMap.PValue;
  begin
    Declared.Add(aTemplate.TypeName);
    FTemplateList.Add(aTemplate);
    p := DeferMap.GetMutValueDef(aTemplate.TypeName, Default(TDeferData));
    for Template in p^.Defers do
      AddTemplate(Template);
    p^.Defers.MakeEmpty;
  end;
var
  Temp: TTempVec;
  Deps: specialize TGLiteVector<string>;
  ForwardDecls: TStrSet;
  Template: TJtdTemplate;
  I, J, Idx: SizeInt;
  tn, dn, sn: string;
  CurrDep: TJtdTemplate.TDependence;
  jt: TJtdType;
  Deferred: Boolean;
begin
  Temp.AddAll(FTemplateList);
  FTemplateList.OwnsObjects := False;
  FTemplateList.Clear;
  FTemplateList.OwnsObjects := True;
  for I := 0 to Pred(Temp.Count) do
    DeferMap.Add(Temp[I].TypeName, TDeferData.Make(I));
  Declared.Add(TJtdGenAny.DefaultTypeName);
  for jt := jtBool to jtTimeStamp do
    Declared.Add(TJtdTypeTemplate.DefaultTypeName(jt));
  I := 0;
  for Template in Temp do begin
    if Template is TJtdElements then begin
      tn := TJtdElements(Template).SpecType;
      if Declared.NonContains(tn) then begin
        DeferMap.GetMutValueDef(tn, Default(TDeferData))^.Defers.Add(Template);
        continue;
      end;
    end else
      if Template is TJtdValues then begin
        tn := TJtdValues(Template).SpecType;
        if Declared.NonContains(tn) then begin
          DeferMap.GetMutValueDef(tn, Default(TDeferData))^.Defers.Add(Template);
          continue;
        end;
      end else begin
        Deferred := False;
        dn := '';
        J := NULL_INDEX;
        Deps.MakeEmpty;
        for CurrDep in Template.Dependencies do begin
          tn := CurrDep.DepName;
          if Declared.NonContains(tn) then begin
            Deps.Add(tn);
            if CurrDep.IsSpecDecl then begin
              Deferred := True;
              Idx := DeferMap.GetMutValueDef(tn, Default(TDeferData))^.Index;
              if Idx > J then begin
                J := Idx;
                dn := tn;
              end;
            end;
          end;
        end;
        if Deferred then begin
          DeferMap.GetMutValueDef(dn, Default(TDeferData))^.Defers.Add(Template);
          continue;
        end else
          for tn in Deps do
            if not(TJtdTemplate.IsSpecDecl(tn, sn) or ForwardDecls.Contains(tn))then begin
              FTemplateList.Add(TJtdForwardDecl.Create(tn));
              ForwardDecls.Add(tn);
            end;
      end;
    AddTemplate(Template);
  end;
end;
{$POP}

function TJtdTemplater.ProcessSchema: string;
var
  s: string;
begin
  EnumElemSet.Clear;
  FRefMap.Clear;
  FTemplateList.Clear;
  InitGlobNameTable;
  FDepth := 0;
  Result := HandleSchema(FRootSchema, AsUniqTypeName(RootClassName), True);
  if FRootSchema.Definitions <> nil then CheckCyclicDeps;
  if UsePasEnums and HasUniqEnumElements then
    for s in EnumElemSet do
      if FGlobNameTable.Contains(s) then begin
        FUniqEnumElems := False;
        break;
      end;
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
const
  HEAD_FMT = '  This unit was automatically created by %s%s.';
  DATE_FMT = '" on "yyyy-mm-dd hh:mm:ss';
var
  d: TDateTime;
begin
  aText.Add('{');
  if CustomHeader <> '' then begin
    aText.Add('  ' + SysUtils.WrapText(CustomHeader, LineEnding + '  ', [' ', '-', #9], 78));
    aText.Add('');
  end;
  if WriteTimeStamp then begin
    d := DateUtils.LocalTimeToUniversal(SysUtils.Now);
    aText.Add(Format(HEAD_FMT,[DISPLAY_NAME, SysUtils.FormatDateTime(DATE_FMT, d)]))
  end else
    aText.Add(Format(HEAD_FMT, [DISPLAY_NAME, '']));
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

class function TJtdPasCodegen.CreateUnit(aSchema: TJtdSchema; const aUnitName: string; const aRootClassName: string;
  const aCustomHeader: string; aOptions: TCodegenOptions): string;
begin
  Result := '';
  with TJtdPasCodegen.Create(aSchema, aOptions) do
    try
      UnitName := aUnitName;
      PreferRootClassName := aRootClassName;
      CustomHeader := aCustomHeader;
      Execute;
      Result := Source.Text;
    finally
      Free;
    end;
end;

class procedure TJtdPasCodegen.CreateUnitFile(const aFileName: string; aSchema: TJtdSchema; const aUnitName: string;
  const aRootClassName: string; const aCustomHeader: string; aOptions: TCodegenOptions);
begin
  with TJtdPasCodegen.Create(aSchema, aOptions) do
    try
      UnitName := aUnitName;
      PreferRootClassName := aRootClassName;
      CustomHeader := aCustomHeader;
      Execute;
      Source.SaveToFile(aFileName);
    finally
      Free;
    end;
end;

constructor TJtdPasCodegen.Create(aSchema: TJtdSchema; aOptions: TCodegenOptions);
begin
  FUseUnits := ['SysUtils', 'lgJson', 'lgJtdTypes'];
  FUnitName := DEFAULT_UNIT_NAME;
  FUsePasEnums := not (cgoDisablePasEnums in aOptions);
  FComments := not (cgoDisableComments in aOptions);
  FRepeatPropDescrs := cgoRepeatDescriptions in aOptions;
  FEngine := TJtdTemplater.Create(aSchema);
  FEngine.UsePasEnums := UsePasEnums;
  FEngine.RepeatPropDescriptions := RepeatPropDescriptions;
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
