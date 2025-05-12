{****************************************************************************
*                                                                           *
*   This file is part of the LGenerics package.                             *
*   A set of classes handy when generating code from JTD schemas.           *                                            *
*                                                                           *
*   Copyright(c) 2023-2025 A.Koverdyaev(avk)                                *
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
unit lgJtdTypes;

{$MODE OBJFPC}{$H+}
{$MODESWITCH TYPEHELPERS}

interface

uses
  Classes, SysUtils, lgUtils, lgHelpers, lgVector, lgHashMap, lgJson, lgJsonTypeDef;

type
  EJtdException = class(Exception);

  EJtdReadJson = class(EJtdException)
  private
    FPath: string;
  public
    constructor Create(const aMessage, aPath: string);
    property JsonPath: string read FPath;
  end;

  TJtdEntity      = class;
  TJtdEntityClass = class of TJtdEntity;

  { TJtdEntity: abstract ancestor class }
  TJtdEntity = class abstract
  public
  const
    DEF_DEPTH    = TJsonReader.DEF_DEPTH;
    DEF_BUF_SIZE = TJsonReader.DEF_BUF_SIZE;
  protected
    procedure DoReadJson(aReader: TJsonReader); virtual; abstract;
    procedure DoWriteJson(aWriter: TJsonStrWriter); virtual; abstract;
    class function  IsNullable: Boolean; virtual;
    class function  GetJtdClass: TJtdEntityClass; virtual;
    class procedure Error(const aMessage: string); static;
    class procedure ReadError(const aMessage: string; aReader: TJsonReader); static;
    class procedure ReadError(const aMessage, aPath: string); static;
    class procedure ReaderFail(aReader: TJsonReader); static;
    class procedure ExpectObject(aReader: TJsonReader); static;
    class procedure PropNotFound(const aJsonPropName: string; aReader: TJsonReader); static;
    class procedure UnknownProp(const aJsonPropName: string; aReader: TJsonReader); static;
    class procedure DuplicateProp(aReader: TJsonReader); static;
    class procedure InternalError(aNumber: Byte); static;
  public
  { if the loading fails, an exception will be raised }
    class function LoadInstance(aReader: TJsonReader): TJtdEntity;
    class function LoadInstance(aBuffer: PAnsiChar; aCount: SizeInt): TJtdEntity;
    class function LoadInstance(const aJson: string): TJtdEntity;
    class function LoadInstance(aStream: TStream; aSkipBom: Boolean = False; aBufSize: SizeInt = DEF_BUF_SIZE;
                                aMaxDepth: SizeInt = DEF_DEPTH): TJtdEntity;
  { returns NIL in case of failed loading }
    class function TryLoad(aBuffer: PAnsiChar; aCount: SizeInt): TJtdEntity;
    class function TryLoad(const aJson: string): TJtdEntity;
    class function TryLoad(aStream: TStream; aSkipBom: Boolean = False; aBufSize: SizeInt = DEF_BUF_SIZE;
                          aMaxDepth: SizeInt = DEF_DEPTH): TJtdEntity;
    class function TryLoadFile(const aFileName: string; aSkipBom: Boolean = False;
                               aBufSize: SizeInt = DEF_BUF_SIZE; aMaxDepth: SizeInt = DEF_DEPTH): TJtdEntity;

    constructor Create; virtual;
  { if the loading fails, an exception will be raised }
    procedure ReadJson(aReader: TJsonReader); virtual;
    procedure Load(aBuffer: PAnsiChar; aCount: SizeInt);
    procedure Load(const aJson: string);
    procedure Load(aStream: TStream; aSkipBom: Boolean = False; aBufSize: SizeInt = DEF_BUF_SIZE;
                   aMaxDepth: SizeInt = DEF_DEPTH);
    procedure LoadFile(const aFileName: string; aSkipBom: Boolean = False; aBufSize: SizeInt = DEF_BUF_SIZE;
                       aMaxDepth: SizeInt = DEF_DEPTH);

    procedure WriteJson(aWriter: TJsonStrWriter); virtual;
    function  AsJson: string;
    property  Nullable: Boolean read IsNullable;
  end;

  { TJtdNullableEntity: abstract ancestor class }
  TJtdNullableEntity = class abstract(TJtdEntity)
  protected
    function  GetIsNull: Boolean; virtual; abstract;
    procedure DoSetNull; virtual; abstract;
    class function IsNullable: Boolean; override;
  public
    procedure SetNull; virtual;
    procedure WriteJson(aWriter: TJsonStrWriter); override;
    property  IsNull: Boolean read GetIsNull;
  end;

  { TJtdNullable: abstract ancestor class }
  TJtdNullable = class abstract(TJtdNullableEntity)
  strict private
    FAssigned: Boolean;
  protected
    function  GetIsNull: Boolean; override;
    procedure DoSetNull; override;
    procedure CheckNull; inline;
    procedure DoAssign; inline;
  public
    procedure ReadJson(aReader: TJsonReader); override;
  end;

  { TJtdAny: container for JTD "empty" form, can contain arbitrary data }
  TJtdAny = class sealed(TJtdNullableEntity)
  private
    FInstance: TJsonNode;
  protected
    procedure DoReadJson(aReader: TJsonReader); override;
    procedure DoWriteJson(aWriter: TJsonStrWriter); override;
    function  GetIsNull: Boolean; override;
    procedure DoSetNull; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    property Instance: TJsonNode read FInstance;
  end;

  { TJtdGenValue: abstract ancestor class }
  generic TJtdGenValue<T> = class abstract(TJtdEntity)
  protected
    FValue: T;
  public
    constructor Create(const aValue: T); virtual; overload;
    property Value: T read FValue write FValue;
  end;

  { TJtdGenNullableValue: abstract ancestor class }
  generic TJtdGenNullableValue<T> = class abstract(TJtdNullable)
  protected
    FValue: T;
    function  GetValue: T; inline;
    procedure SetValue(const aValue: T); virtual;
  public
    constructor Create(const aValue: T); virtual; overload;
    function OrElse(const aDefault: T): T; inline;
    property Value: T read GetValue write SetValue;
  end;

  { TJtdValue: abstract ancestor class }
  generic TJtdValue<T> = class abstract(specialize TJtdGenValue<T>)
  protected
    procedure DoReadJson(aReader: TJsonReader); override;
    procedure DoWriteJson(aWriter: TJsonStrWriter); override;
  end;

  { TJtdNullableValue: abstract ancestor class }
  generic TJtdNullableValue<T> = class abstract(specialize TJtdGenNullableValue<T>)
  protected
    procedure DoReadJson(aReader: TJsonReader); override;
    procedure DoWriteJson(aWriter: TJsonStrWriter); override;
  end;

  { TJtdBool: container for JTD "boolean" type }
  TJtdBool = class sealed(specialize TJtdValue<Boolean>);

  { TJtdNullableBool: container for nullable JTD "boolean" type }
  TJtdNullableBool = class sealed(specialize TJtdNullableValue<Boolean>);

  { TJtdFloat32: container for JTD "float32" type }
  TJtdFloat32 = class sealed(specialize TJtdValue<Single>);

  { TJtdNullableFloat32: container for nullable JTD "float32" type }
  TJtdNullableFloat32 = class sealed(specialize TJtdNullableValue<Single>);

  { TJtdFloat64: container for JTD "float64" type }
  TJtdFloat64 = class sealed(specialize TJtdValue<Double>);

  { TJtdNullableFloat64: container for JTD nullable "float64" type }
  TJtdNullableFloat64 = class sealed(specialize TJtdNullableValue<Double>);

  { TJtdInt8: container for JTD "int8" type }
  TJtdInt8 = class sealed(specialize TJtdValue<Int8>);

  { TJtdNullableInt8: container for JTD nullable "int8" type }
  TJtdNullableInt8 = class sealed(specialize TJtdNullableValue<Int8>);

  { TJtdUInt8: container for JTD "uint8" type }
  TJtdUInt8 = class sealed(specialize TJtdValue<UInt8>);

  { TJtdNullableUInt8: container for JTD nullable "uint8" type }
  TJtdNullableUInt8 = class sealed(specialize TJtdNullableValue<UInt8>);

  { TJtdInt16: container for JTD "int16" type }
  TJtdInt16 = class sealed(specialize TJtdValue<Int16>);

  { TJtdNullableInt16: container for JTD nullable "int16" type }
  TJtdNullableInt16 = class sealed(specialize TJtdNullableValue<Int16>);

  { TJtdUInt16: container for JTD "uint16" type }
  TJtdUInt16 = class sealed(specialize TJtdValue<UInt16>);

  { TJtdNullableUInt16: container for JTD nullable "uint16" type }
  TJtdNullableUInt16 = class sealed(specialize TJtdNullableValue<UInt16>);

  { TJtdInt32: container for JTD "int32" type }
  TJtdInt32 = class sealed(specialize TJtdValue<Int32>);

  { TJtdNullableInt32: container for JTD nullable "int32" type }
  TJtdNullableInt32 = class sealed(specialize TJtdNullableValue<Int32>);

  { TJtdUInt32: container for JTD "uint32" type }
  TJtdUInt32 = class sealed(specialize TJtdValue<UInt32>);

  { TJtdNullableUInt32: container for JTD nullable "uint32" type }
  TJtdNullableUInt32 = class sealed(specialize TJtdNullableValue<UInt32>);

  { TJtdString: container for JTD "string" type }
  TJtdString = class sealed(specialize TJtdValue<string>);

  { TJtdNullableString: container for JTD nullable "string" type }
  TJtdNullableString = class sealed(specialize TJtdNullableValue<string>);

  { TJtdDateTimeUTC: container for JTD "timestamp" type }
  TJtdDateTimeUTC = class sealed(specialize TJtdValue<TDateTime>);

  { TJtdNullableDateTimeUTC: container for JTD nullable "timestamp" type }
  TJtdNullableDateTimeUTC = class sealed(specialize TJtdNullableValue<TDateTime>);

  { TJtdEnum: generic container for JDT "enum" form when the enum elements are valid Pascal identifiers }
  generic TJtdEnum<TEnum> = class(specialize TJtdGenValue<TEnum>)
  protected
    procedure DoReadJson(aReader: TJsonReader); override;
    procedure DoWriteJson(aWriter: TJsonStrWriter); override;
  end;

  { TJtdNullableEnum: generic container for JDT nullable "enum" form when the enum elements
    are valid Pascal identifiers }
  generic TJtdNullableEnum<T> = class abstract(specialize TJtdGenNullableValue<T>)
  protected
    procedure DoReadJson(aReader: TJsonReader); override;
    procedure DoWriteJson(aWriter: TJsonStrWriter); override;
  end;

  { TJtdStrEnum: container for JDT "enum" form when the enum elements are not valid Pascal identifiers }
  TJtdStrEnum = class abstract(specialize TJtdGenValue<string>)
  protected
    procedure SetValue(const aValue: string);
    procedure DoReadJson(aReader: TJsonReader); override;
    procedure DoWriteJson(aWriter: TJsonStrWriter); override;
  public
    class function IsElement(const aValue: string): Boolean; virtual; abstract;
    constructor Create(const aValue: string); override;
    property Value: string read FValue write SetValue;
  end;

  { TJtdNullableStrEnum: container for JDT nullable "enum" form when the enum elements
    are not valid Pascal identifiers }
  TJtdNullableStrEnum = class abstract(specialize TJtdGenNullableValue<string>)
  protected
    procedure SetValue(const aValue: string); override;
    procedure DoReadJson(aReader: TJsonReader); override;
    procedure DoWriteJson(aWriter: TJsonStrWriter); override;
  public
    class function IsElement(const aValue: string): Boolean; virtual; abstract;
    constructor Create(const aValue: string); override;
  end;

  { TJtdGenContainer: abstract ancestor class }
  TJtdGenContainer = class abstract(TJtdEntity)
  protected
    procedure DoClear; virtual; abstract;
  public
    destructor Destroy; override;
    procedure ReadJson(aReader: TJsonReader); override;
  end;

  { TJtdGenNullableContainer: abstract ancestor class }
  TJtdGenNullableContainer = class abstract(TJtdNullable)
  protected
    procedure DoClear; virtual; abstract;
    procedure DoSetNull; override;
  public
    destructor Destroy; override;
  end;

  { TJtdContainer: abstract ancestor class }
  TJtdContainer = class abstract(TJtdGenContainer)
    procedure Clear; virtual;
  end;

  { TJtdNullableContainer: abstract ancestor class }
  TJtdNullableContainer = class abstract(TJtdGenNullableContainer)
    procedure Clear; virtual;
  end;

  { TJtdList: generic container for JTD "elements" form }
  generic TJtdList<T: TJtdEntity> = class(TJtdContainer)
  protected
  type
    TList = specialize TGLiteVector<T>;
  public
  type
    TEnumerator = TList.TEnumerator;
  private
    FList: TList;
    function  GetCount: SizeInt; inline;
    function  GetItem(aIndex: SizeInt): T; inline;
    procedure SetItem(aIndex: SizeInt; aValue: T);
  protected
    procedure DoReadJson(aReader: TJsonReader); override;
    procedure DoWriteJson(aWriter: TJsonStrWriter); override;
    procedure DoClear; override;
  public
    function  GetEnumerator: TEnumerator; inline;
    function  Add(aValue: T): SizeInt;
    procedure Insert(aIndex: SizeInt; aValue: T);
    procedure Delete(aIndex: SizeInt); inline;
    property  Count: SizeInt read GetCount;
    property  Items[aIndex: SizeInt]: T read GetItem write SetItem; default;
  end;

  { TJtdNullableList: generic container for JTD nullable "elements" form }
  generic TJtdNullableList<T: TJtdEntity> = class(TJtdNullableContainer)
  protected
  type
    TList = specialize TGLiteVector<T>;
  public
  type
    TEnumerator = TList.TEnumerator;
  private
    FList: TList;
    function  GetCount: SizeInt; inline;
    function  GetItem(aIndex: SizeInt): T; inline;
    procedure SetItem(aIndex: SizeInt; aValue: T);
  protected
    procedure DoReadJson(aReader: TJsonReader); override;
    procedure DoWriteJson(aWriter: TJsonStrWriter); override;
    procedure DoClear; override;
  public
    function  GetEnumerator: TEnumerator; inline;
    function  Add(aValue: T): SizeInt;
    procedure Insert(aIndex: SizeInt; aValue: T);
    procedure Delete(aIndex: SizeInt); inline;
    property  Count: SizeInt read GetCount;
    property  Items[aIndex: SizeInt]: T read GetItem write SetItem; default;
  end;

  { TJtdMap: generic container for JTD "values" form }
  generic TJtdMap<T: TJtdEntity> = class(TJtdContainer)
  protected
  type
    TMapType = specialize TGLiteChainHashMap<string, T, string>;
    TMap     = TMapType.TMap;
  public
  type
    TJtdMapEntry      = TMap.TEntry;
    TJtdMapEnumerator = TMap.TEntryEnumerator;
    TJtdMapKeys       = TMap.TKeys;
    TJtdMapValues     = TMap.TValues;
  private
    FMap: TMap;
    function  GetCount: SizeInt; inline;
    function  GetItem(const aKey: string): T; inline;
  protected
    procedure DoReadJson(aReader: TJsonReader); override;
    procedure DoWriteJson(aWriter: TJsonStrWriter); override;
    procedure DoClear; override;
  public
  { enumerates all pairs (aKey, aValue) }
    function  GetEnumerator: TJtdMapEnumerator; inline;
  { enumerates all keys }
    function  Keys: TJtdMapKeys; inline;
  { enumerates all values }
    function  Values: TJtdMapValues; inline;
    function  Contains(const aKey: string): Boolean; inline;
  { returns True and adds the (aKey, aValue) pair only if the instance does not contain aKey }
    function  Add(const aKey: string; aValue: T): Boolean; inline;
    procedure AddOrSetValue(const aKey: string; aValue: T);
  { returns True and aValue mapped to aKey if contains aKey,
    False otherwise, in this case aValue is undefined }
    function  TryGetValue(const aKey: string; out aValue: T): Boolean; inline;
    function  Remove(const aKey: string): Boolean;
  { GetItem getter returns nil if aKey is not present in the instance }
    property  Items[const aKey: string]: T read GetItem write AddOrSetValue; default;
  end;

  { TJtdNullableMap: generic container for JTD nullable "values" form }
  generic TJtdNullableMap<T: TJtdEntity> = class(TJtdNullableContainer)
  protected
  type
    TMapType = specialize TGLiteChainHashMap<string, T, string>;
    TMap     = TMapType.TMap;
  public
  type
    TJtdMapEntry      = TMap.TEntry;
    TJtdMapEnumerator = TMap.TEntryEnumerator;
    TJtdMapKeys       = TMap.TKeys;
    TJtdMapValues     = TMap.TValues;
  private
    FMap: TMap;
    function  GetCount: SizeInt; inline;
    function  GetItem(const aKey: string): T; inline;
  protected
    procedure DoReadJson(aReader: TJsonReader); override;
    procedure DoWriteJson(aWriter: TJsonStrWriter); override;
    procedure DoClear; override;
  public
  { enumerates all pairs (aKey, aValue) }
    function  GetEnumerator: TJtdMapEnumerator; inline;
  { enumerates all keys }
    function  Keys: TJtdMapKeys; inline;
  { enumerates all values }
    function  Values: TJtdMapValues; inline;
    function  Contains(const aKey: string): Boolean; inline;
  { returns True and adds the (aKey, aValue) pair only if the instance does not contain aKey }
    function  Add(const aKey: string; aValue: T): Boolean; inline;
    procedure AddOrSetValue(const aKey: string; aValue: T);
  { returns True and aValue mapped to aKey if contains aKey,
    False otherwise, in this case aValue is undefined }
    function  TryGetValue(const aKey: string; out aValue: T): Boolean; inline;
    function  Remove(const aKey: string): Boolean;
  { GetItem getter returns nil if aKey is not present in the instance }
    property  Items[const aKey: string]: T read GetItem write AddOrSetValue; default;
  end;

  { TJtdObject: abstract ancestor class for JTD "properties" form }
  TJtdObject = class abstract(TJtdGenContainer)
  protected
    procedure CreateFields; virtual; abstract;
    procedure ClearFields; virtual; abstract;
    procedure WriteFields(aWriter: TJsonStrWriter); virtual; abstract;
    procedure DoWriteJson(aWriter: TJsonStrWriter); override;
  public
    constructor Create; override;
    destructor Destroy; override;
  end;

  { TJtdNullableObject: abstract ancestor class for JTD nullable "properties" form }
  TJtdNullableObject = class abstract(TJtdGenNullableContainer)
  protected
    procedure CreateFields; virtual; abstract;
    procedure WriteFields(aWriter: TJsonStrWriter); virtual; abstract;
    procedure CheckFields;
    procedure DoWriteJson(aWriter: TJsonStrWriter); override;
  public
    procedure ReadJson(aReader: TJsonReader); override;
  end;

  { TJtdUnion: abstract ancestor class for JTD "discriminator" form }
  TJtdUnion = class abstract(TJtdGenContainer)
  protected
    FTag: string;
    FInstance: TJtdEntity;
    procedure DoReadJson(aReader: TJsonReader); override;
    procedure DoWriteJson(aWriter: TJsonStrWriter); override;
    procedure DoClear; override;
    class function GetTagJsonName: string; virtual; abstract;
    class function GetInstanceClass(const aTag: string): TJtdEntityClass; virtual; abstract;
  public
    function GetInstanceClass: TJtdEntityClass;
    property Tag: string read FTag;
  end;

  { TJtdNullableUnion: abstract ancestor class for nullable JTD "discriminator" form }
  TJtdNullableUnion = class abstract(TJtdGenNullableContainer)
  protected
    FTag: string;
    FInstance: TJtdEntity;
    procedure DoReadJson(aReader: TJsonReader); override;
    procedure DoWriteJson(aWriter: TJsonStrWriter); override;
    procedure DoClear; override;
    class function GetTagJsonName: string; virtual; abstract;
    class function GetInstanceClass(const aTag: string): TJtdEntityClass; virtual; abstract;
  public
    function GetInstanceClass: TJtdEntityClass;
    property Tag: string read FTag;
  end;

resourcestring
  SEJtdInternalFmt = 'JTD types internal error #%u';

type
  TTokenKind   = TJsonReader.TTokenKind;
  TJtdFormKind = TJtdSchema.TFormKind;
  TJtdType     = TJtdSchema.TJtdType;

const
{$PUSH}{$J-}
  JTD_TYPE_IMPL: array[TJtdType] of array[Boolean] of TJtdEntityClass = (
    (nil, nil),
    (TJtdBool, TJtdNullableBool),
    (TJtdFloat32, TJtdNullableFloat32),
    (TJtdFloat64, TJtdNullableFloat64),
    (TJtdInt8, TJtdNullableInt8),
    (TJtdUInt8, TJtdNullableUInt8),
    (TJtdInt16, TJtdNullableInt16),
    (TJtdUInt16, TJtdNullableUInt16),
    (TJtdInt32, TJtdNullableInt32),
    (TJtdUInt32, TJtdNullableUInt32),
    (TJtdString, TJtdNullableString),
    (TJtdDateTimeUTC, TJtdNullableDateTimeUTC)
  );
  JTD_FORM_ANCESTORS: array[TJtdFormKind] of array[Boolean] of string = (
    ('', ''),
    ('TJtdAny', 'TJtdAny'),
    ('', ''),
    ('', ''),
    ('TJtdEnum', 'TJtdNullableEnum'),
    ('TJtdList', 'TJtdNullableList'),
    ('TJtdObject', 'TJtdNullableObject'),
    ('TJtdMap', 'TJtdNullableMap'),
    ('TJtdUnion', 'TJtdNullableUnion')
  );
  JTD_STR_ENUM: array[Boolean] of TJtdEntityClass = (TJtdStrEnum, TJtdNullableStrEnum);
{$POP}

const
  CSJtdBool       = 'Boolean';
  CSJtdNumber     = 'Number';
  CSJtdString     = 'String';
  CSJtdInteger    = 'Integer';
  CSJtdFractional = 'Fractional';
  CSJtdArray      = 'Array';
  CSJtdObject     = 'Object';
{$PUSH}{$J-}
  TOKEN_NAMES: array[TTokenKind] of string = (
    'trash', 'Null', 'Boolean', 'Boolean', 'Number', 'String', 'Array', 'Object', 'trash', 'trash'
  );
{$POP}

implementation
{$B-}{$COPERATORS ON}{$POINTERMATH ON}
uses
  TypInfo, Math, lgStrConst;

{ EJtdReadJson }

constructor EJtdReadJson.Create(const aMessage, aPath: string);
begin
  inherited Create(aMessage);
  FPath := aPath;
end;

procedure JtdReadError(const aMessage: string; aReader: TJsonReader);
begin
  TJtdEntity.ReadError(aMessage, aReader);
end;

{ TJtdEntity }

class function TJtdEntity.IsNullable: Boolean;
begin
  Result := False;
end;

class function TJtdEntity.GetJtdClass: TJtdEntityClass;
begin
  Result := Self;
end;

class procedure TJtdEntity.Error(const aMessage: string);
begin
  raise EJtdException.Create(aMessage);
end;

class procedure TJtdEntity.ReadError(const aMessage: string; aReader: TJsonReader);
begin
  raise EJtdReadJson.Create(aMessage, aReader.Path);
end;

class procedure TJtdEntity.ReadError(const aMessage, aPath: string);
begin
  raise EJtdReadJson.Create(aMessage, aPath);
end;

class procedure TJtdEntity.ReaderFail(aReader: TJsonReader);
begin
  if aReader.ReadState = rsEof then
    raise EJtdReadJson.Create(SEUnexpectJsonEnd, '')
  else
    raise EJtdReadJson.Create(SEInvalidJsonInst, aReader.Path);
end;

class procedure TJtdEntity.ExpectObject(aReader: TJsonReader);
begin
  ReadError(Format(SEJtdExpectGotFmt, [CSJtdObject, TOKEN_NAMES[aReader.TokenKind]]), aReader);
end;

class procedure TJtdEntity.PropNotFound(const aJsonPropName: string; aReader: TJsonReader);
begin
  ReadError(Format(SERequiredJPropNotFoundFmt, [ClassName, aJsonPropName]), aReader);
end;

class procedure TJtdEntity.UnknownProp(const aJsonPropName: string; aReader: TJsonReader);
begin
  ReadError(Format(SEUnknownPropFoundFmt, [ClassName, aJsonPropName]), aReader);
end;

class procedure TJtdEntity.DuplicateProp(aReader: TJsonReader);
begin
  ReadError(Format(SEJtdDupPropNameFmt, [aReader.Name]), aReader);
end;

class procedure TJtdEntity.InternalError(aNumber: Byte);
begin
  Error(Format(SEJtdInternalFmt, [aNumber]));
end;

class function TJtdEntity.LoadInstance(aReader: TJsonReader): TJtdEntity;
begin
  Result := GetJtdClass.Create;
  try
    Result.ReadJson(aReader);
  except
    FreeAndNil(Result);
    raise;
  end;
end;

class function TJtdEntity.LoadInstance(aBuffer: PAnsiChar; aCount: SizeInt): TJtdEntity;
var
  Reader: TJsonReader;
begin
  Reader := TJsonReader.Create(aBuffer, aCount);
  try
    Result := LoadInstance(Reader);
  finally
    Reader.Free;
  end;
end;

class function TJtdEntity.LoadInstance(const aJson: string): TJtdEntity;
begin
  Result := LoadInstance(Pointer(aJson), System.Length(aJson));
end;

class function TJtdEntity.LoadInstance(aStream: TStream; aSkipBom: Boolean; aBufSize: SizeInt;
  aMaxDepth: SizeInt): TJtdEntity;
var
  Reader: TJsonReader;
begin
  Reader := TJsonReader.Create(aStream, aBufSize, aMaxDepth, aSkipBom);
  try
    Result := LoadInstance(Reader);
  finally
    Reader.Free;
  end;
end;

class function TJtdEntity.TryLoad(aBuffer: PAnsiChar; aCount: SizeInt): TJtdEntity;
begin
  try
    Result := LoadInstance(aBuffer, aCount);
  except
  end;
end;

class function TJtdEntity.TryLoad(const aJson: string): TJtdEntity;
begin
  try
    Result := LoadInstance(aJson);
  except
  end;
end;

class function TJtdEntity.TryLoad(aStream: TStream; aSkipBom: Boolean; aBufSize: SizeInt;
  aMaxDepth: SizeInt): TJtdEntity;
begin
  try
    Result := LoadInstance(aStream, aSkipBom, aBufSize, aMaxDepth);
  except
  end;
end;

class function TJtdEntity.TryLoadFile(const aFileName: string; aSkipBom: Boolean; aBufSize: SizeInt;
  aMaxDepth: SizeInt): TJtdEntity;
begin
  Result := nil;
  try
    with TFileStream.Create(aFileName, fmOpenRead or fmShareDenyWrite) do
      try
        Result := LoadInstance(TStream(GetSelfRef), aSkipBom, aBufSize, aMaxDepth);
      finally
        Free;
      end;
  except
  end;
end;

constructor TJtdEntity.Create;
begin
  inherited Create;
end;

procedure TJtdEntity.ReadJson(aReader: TJsonReader);
begin
  if (aReader.ReadState = rsStart) and not aReader.Read then ReaderFail(aReader);
  if aReader.TokenKind = tkNull then
    ReadError(Format(SEJtdNullNotAcceptFmt, [ClassName]), aReader);
  DoReadJson(aReader);
end;

procedure TJtdEntity.Load(aBuffer: PAnsiChar; aCount: SizeInt);
var
  Reader: TJsonReader;
begin
  Reader := TJsonReader.Create(aBuffer, aCount);
  try
    ReadJson(Reader);
  finally
    Reader.Free;
  end;
end;

procedure TJtdEntity.Load(const aJson: string);
begin
  Load(Pointer(aJson), System.Length(aJson));
end;

procedure TJtdEntity.Load(aStream: TStream; aSkipBom: Boolean; aBufSize: SizeInt; aMaxDepth: SizeInt);
var
  Reader: TJsonReader;
begin
  Reader := TJsonReader.Create(aStream, aBufSize, aMaxDepth, aSkipBom);
  try
    ReadJson(Reader);
  finally
    Reader.Free;
  end;
end;

procedure TJtdEntity.LoadFile(const aFileName: string; aSkipBom: Boolean; aBufSize: SizeInt; aMaxDepth: SizeInt);
var
  fs: TStream;
begin
  fs := TFileStream.Create(aFileName, fmOpenRead or fmShareDenyWrite);
  try
    Load(fs, aSkipBom, aBufSize, aMaxDepth);
  finally
    fs.Free;
  end;
end;

procedure TJtdEntity.WriteJson(aWriter: TJsonStrWriter);
begin
  DoWriteJson(aWriter);
end;

function TJtdEntity.AsJson: string;
var
  Writer: TJsonStrWriter;
begin
  Writer := TJsonStrWriter.Create;
  try
    WriteJson(Writer);
    Result := Writer.JsonString;
  finally
    Writer.Free;
  end;
end;

{ TJtdNullableEntity }

class function TJtdNullableEntity.IsNullable: Boolean;
begin
  Result := True;
end;

procedure TJtdNullableEntity.SetNull;
begin
  if not IsNull then DoSetNull;
end;

procedure TJtdNullableEntity.WriteJson(aWriter: TJsonStrWriter);
begin
  if IsNull then
    aWriter.AddNull
  else
    DoWriteJson(aWriter);
end;

{ TJtdNullable }

function TJtdNullable.GetIsNull: Boolean;
begin
  Result := not FAssigned;
end;

procedure TJtdNullable.DoSetNull;
begin
  FAssigned := False;
end;

procedure TJtdNullable.CheckNull;
begin
  if not FAssigned then Error(SEJtdCantReadNullValue);
end;

procedure TJtdNullable.DoAssign;
begin
  FAssigned := True;
end;

procedure TJtdNullable.ReadJson(aReader: TJsonReader);
begin
  if (aReader.ReadState = rsStart) and not aReader.Read then ReaderFail(aReader);
  SetNull;
  if aReader.TokenKind = tkNull then exit;
  DoReadJson(aReader);
  DoAssign;
end;

{ TJtdAny }

procedure TJtdAny.DoReadJson(aReader: TJsonReader);
var
  s: string;
begin
  case aReader.TokenKind of
    tkNull:   Instance.AsNull;
    tkFalse:  Instance.AsBoolean := False;
    tkTrue:   Instance.AsBoolean := True;
    tkNumber: Instance.AsNumber := aReader.AsNumber;
    TTokenKind.tkString: Instance.AsString := aReader.AsString;
    tkArrayBegin, tkObjectBegin:
      begin
        if not aReader.CopyStruct(s) then ReaderFail(aReader);
        if not Instance.TryParse(s) then ReadError(SEInvalidJsonInst, aReader);
      end;
  else
    InternalError(1);
  end;
end;

procedure TJtdAny.DoWriteJson(aWriter: TJsonStrWriter);
begin
  aWriter.Add(Instance);
end;

function TJtdAny.GetIsNull: Boolean;
begin
  Result := FInstance.IsNull;
end;

procedure TJtdAny.DoSetNull;
begin
  FInstance.Clear;
end;

constructor TJtdAny.Create;
begin
  inherited Create;
  FInstance := TJsonNode.Create;
end;

destructor TJtdAny.Destroy;
begin
  FInstance.Free;
  inherited;
end;

type
  { TJtdBoolHelper }
  TJtdBoolHelper = type helper(TBooleanHelper) for Boolean
    procedure ReadJson(aReader: TJsonReader);
    procedure WriteJson(aWriter: TJsonStrWriter);
  end;

  { TJtdSingleHelper }
  TJtdSingleHelper = type helper(TGSingleHelper) for Single
    procedure ReadJson(aReader: TJsonReader);
    procedure WriteJson(aWriter: TJsonStrWriter);
  end;

  { TJtdDoubleHelper }
  TJtdDoubleHelper = type helper(TGDoubleHelper) for Double
    procedure ReadJson(aReader: TJsonReader);
    procedure WriteJson(aWriter: TJsonStrWriter);
  end;

  { TJtdShortIntHelper }
  TJtdShortIntHelper = type helper(TGShortIntHelper) for ShortInt
    procedure ReadJson(aReader: TJsonReader);
    procedure WriteJson(aWriter: TJsonStrWriter);
  end;

  { TJtdByteHelper }
  TJtdByteHelper = type helper(TGByteHelper) for Byte
    procedure ReadJson(aReader: TJsonReader);
    procedure WriteJson(aWriter: TJsonStrWriter);
  end;

  { TJtdSmallIntHelper }
  TJtdSmallIntHelper = type helper(TGSmallIntHelper) for SmallInt
    procedure ReadJson(aReader: TJsonReader);
    procedure WriteJson(aWriter: TJsonStrWriter);
  end;

  { TJtdWordHelper }
  TJtdWordHelper = type helper(TGWordHelper) for Word
    procedure ReadJson(aReader: TJsonReader);
    procedure WriteJson(aWriter: TJsonStrWriter);
  end;

  { TJtdLongIntHelper }
  TJtdLongIntHelper = type helper(TGLongIntHelper) for LongInt
    procedure ReadJson(aReader: TJsonReader);
    procedure WriteJson(aWriter: TJsonStrWriter);
  end;

  { TJtdDWordHelper }
  TJtdDWordHelper = type helper(TGDWordHelper) for DWord
    procedure ReadJson(aReader: TJsonReader);
    procedure WriteJson(aWriter: TJsonStrWriter);
  end;

  { TJtdStrHelper }
  TJtdStrHelper = type helper(TAStrHelper) for ansistring
    procedure ReadJson(aReader: TJsonReader);
    procedure WriteJson(aWriter: TJsonStrWriter);
  end;

  { TJtdDateTimeHelper }
  TJtdDateTimeHelper = type helper(TGDateTimeHelper) for TDateTime
    procedure ReadJson(aReader: TJsonReader);
    procedure WriteJson(aWriter: TJsonStrWriter);
  end;

{ TJtdBoolHelper }
procedure TJtdBoolHelper.ReadJson(aReader: TJsonReader);
begin
  if not(aReader.TokenKind in [tkFalse, tkTrue]) then
    JtdReadError(Format(SEJtdExpectGotFmt, [CSJtdBool, TOKEN_NAMES[aReader.TokenKind]]), aReader);
  Self := aReader.TokenKind = tkTrue;
end;

procedure TJtdBoolHelper.WriteJson(aWriter: TJsonStrWriter);
begin
  if Self then
    aWriter.AddTrue
  else
    aWriter.AddFalse;
end;

{ TJtdSingleHelper }
procedure TJtdSingleHelper.ReadJson(aReader: TJsonReader);
var
  d: Double;
begin
  if aReader.TokenKind <> tkNumber then
    JtdReadError(Format(SEJtdExpectGotFmt, [CSJtdNumber, TOKEN_NAMES[aReader.TokenKind]]), aReader);
  d := aReader.AsNumber;
  if System.Abs(d) > Math.MaxSingle then
    JtdReadError(Format(SEJtdNumRangeFmt, ['Float32']), aReader);
  Self := d;
end;

procedure TJtdSingleHelper.WriteJson(aWriter: TJsonStrWriter);
begin
  aWriter.Add(Self);
end;

{ TJtdDoubleHelper }
procedure TJtdDoubleHelper.ReadJson(aReader: TJsonReader);
begin
  if aReader.TokenKind <> tkNumber then
    JtdReadError(Format(SEJtdExpectGotFmt, [CSJtdNumber, TOKEN_NAMES[aReader.TokenKind]]), aReader);
  Self := aReader.AsNumber;
end;

procedure TJtdDoubleHelper.WriteJson(aWriter: TJsonStrWriter);
begin
  aWriter.Add(Self);
end;

{ TJtdShortIntHelper }
procedure TJtdShortIntHelper.ReadJson(aReader: TJsonReader);
var
  I: Int64;
begin
  if aReader.TokenKind <> tkNumber then
    JtdReadError(Format(SEJtdExpectGotFmt, [CSJtdNumber, TOKEN_NAMES[aReader.TokenKind]]), aReader);
  if not Double.IsExactInt(aReader.AsNumber, I) then
    JtdReadError(Format(SEJtdExpectGotFmt, [CSJtdInteger, CSJtdFractional]), aReader);
  if (I < System.Low(Self)) or (I > System.High(Self)) then
    JtdReadError(Format(SEJtdNumRangeFmt, ['Int8']), aReader);
  Self := I;
end;

procedure TJtdShortIntHelper.WriteJson(aWriter: TJsonStrWriter);
begin
  aWriter.Add(Self);
end;

{ TJtdByteHelper }
procedure TJtdByteHelper.ReadJson(aReader: TJsonReader);
var
  I: Int64;
begin
  if aReader.TokenKind <> tkNumber then
    JtdReadError(Format(SEJtdExpectGotFmt, [CSJtdNumber, TOKEN_NAMES[aReader.TokenKind]]), aReader);
  if not Double.IsExactInt(aReader.AsNumber, I) then
    JtdReadError(Format(SEJtdExpectGotFmt, [CSJtdInteger, CSJtdFractional]), aReader);
  if (I < System.Low(Self)) or (I > System.High(Self)) then
    JtdReadError(Format(SEJtdNumRangeFmt, ['UInt8']), aReader);
  Self := I;
end;

procedure TJtdByteHelper.WriteJson(aWriter: TJsonStrWriter);
begin
  aWriter.Add(Self);
end;

{ TJtdSmallIntHelper }
procedure TJtdSmallIntHelper.ReadJson(aReader: TJsonReader);
var
  I: Int64;
begin
  if aReader.TokenKind <> tkNumber then
    JtdReadError(Format(SEJtdExpectGotFmt, [CSJtdNumber, TOKEN_NAMES[aReader.TokenKind]]), aReader);
  if not Double.IsExactInt(aReader.AsNumber, I) then
    JtdReadError(Format(SEJtdExpectGotFmt, [CSJtdInteger, CSJtdFractional]), aReader);
  if (I < System.Low(Self)) or (I > System.High(Self)) then
    JtdReadError(Format(SEJtdNumRangeFmt, ['Int16']), aReader);
  Self := I;
end;

procedure TJtdSmallIntHelper.WriteJson(aWriter: TJsonStrWriter);
begin
  aWriter.Add(Self);
end;

{ TJtdWordHelper }
procedure TJtdWordHelper.ReadJson(aReader: TJsonReader);
var
  I: Int64;
begin
  if aReader.TokenKind <> tkNumber then
    JtdReadError(Format(SEJtdExpectGotFmt, [CSJtdNumber, TOKEN_NAMES[aReader.TokenKind]]), aReader);
  if not Double.IsExactInt(aReader.AsNumber, I) then
    JtdReadError(Format(SEJtdExpectGotFmt, [CSJtdInteger, CSJtdFractional]), aReader);
  if (I < System.Low(Self)) or (I > System.High(Self)) then
    JtdReadError(Format(SEJtdNumRangeFmt, ['UInt16']), aReader);
  Self := I;
end;

procedure TJtdWordHelper.WriteJson(aWriter: TJsonStrWriter);
begin
  aWriter.Add(Self);
end;

{ TJtdLongIntHelper }
procedure TJtdLongIntHelper.ReadJson(aReader: TJsonReader);
var
  I: Int64;
begin
  if aReader.TokenKind <> tkNumber then
    JtdReadError(Format(SEJtdExpectGotFmt, [CSJtdNumber, TOKEN_NAMES[aReader.TokenKind]]), aReader);
  if not Double.IsExactInt(aReader.AsNumber, I) then
    JtdReadError(Format(SEJtdExpectGotFmt, [CSJtdInteger, CSJtdFractional]), aReader);
  if (I < System.Low(Self)) or (I > System.High(Self)) then
    JtdReadError(Format(SEJtdNumRangeFmt, ['Int32']), aReader);
  Self := I;
end;

procedure TJtdLongIntHelper.WriteJson(aWriter: TJsonStrWriter);
begin
  aWriter.Add(Self);
end;

{ TJtdDWordHelper }
procedure TJtdDWordHelper.ReadJson(aReader: TJsonReader);
var
  I: Int64;
begin
  if aReader.TokenKind <> tkNumber then
    JtdReadError(Format(SEJtdExpectGotFmt, [CSJtdNumber, TOKEN_NAMES[aReader.TokenKind]]), aReader);
  if not Double.IsExactInt(aReader.AsNumber, I) then
    JtdReadError(Format(SEJtdExpectGotFmt, [CSJtdInteger, CSJtdFractional]), aReader);
  if (I < System.Low(Self)) or (I > System.High(Self)) then
    JtdReadError(Format(SEJtdNumRangeFmt, ['UInt32']), aReader);
  Self := I;
end;

procedure TJtdDWordHelper.WriteJson(aWriter: TJsonStrWriter);
begin
  aWriter.Add(Self);
end;

{ TJtdStrHelper }
procedure TJtdStrHelper.ReadJson(aReader: TJsonReader);
begin
  if aReader.TokenKind <> TTokenKind.tkString then
    JtdReadError(Format(SEJtdExpectGotFmt, [CSJtdString, TOKEN_NAMES[aReader.TokenKind]]), aReader);
  Self := aReader.AsString;
end;

procedure TJtdStrHelper.WriteJson(aWriter: TJsonStrWriter);
begin
  aWriter.Add(Self);
end;

{ TJtdDateTimeHelper }
procedure TJtdDateTimeHelper.ReadJson(aReader: TJsonReader);
var
  d: TDateTime;
begin
  if aReader.TokenKind <> TTokenKind.tkString then
    JtdReadError(Format(SEJtdExpectGotFmt, [CSJtdString, TOKEN_NAMES[aReader.TokenKind]]), aReader);
  if not TryRfc8927TimeStampToUTC(aReader.AsString, d) then
    JtdReadError(Format(SEJtdIllform8927TSFmt, [aReader.AsString]), aReader);
  Self := d;
end;

procedure TJtdDateTimeHelper.WriteJson(aWriter: TJsonStrWriter);
begin
  aWriter.Add(UTCToRfc8927TimeStamp(Self));
end;

{ TJtdGenValue }

constructor TJtdGenValue.Create(const aValue: T);
begin
  inherited Create;
  Value := aValue;
end;

{ TJtdGenNullableValue }

function TJtdGenNullableValue.GetValue: T;
begin
  CheckNull;
  Result := FValue;
end;

procedure TJtdGenNullableValue.SetValue(const aValue: T);
begin
  if IsNull then DoAssign;
  FValue := aValue;
end;

constructor TJtdGenNullableValue.Create(const aValue: T);
begin
  inherited Create;
  Value := aValue;
end;

function TJtdGenNullableValue.OrElse(const aDefault: T): T;
begin
  if IsNull then
    Result := aDefault
  else
    Result := FValue;
end;

{ TJtdValue }

procedure TJtdValue.DoReadJson(aReader: TJsonReader);
begin
  FValue.ReadJson(aReader);
end;

procedure TJtdValue.DoWriteJson(aWriter: TJsonStrWriter);
begin
  FValue.WriteJson(aWriter);
end;

{ TJtdNullableValue }

procedure TJtdNullableValue.DoReadJson(aReader: TJsonReader);
begin
  FValue.ReadJson(aReader);
end;

procedure TJtdNullableValue.DoWriteJson(aWriter: TJsonStrWriter);
begin
  FValue.WriteJson(aWriter);
end;

{ TJtdEnum }

procedure TJtdEnum.DoReadJson(aReader: TJsonReader);
var
  v: Integer;
begin
  if aReader.TokenKind <> TTokenKind.tkString then
    ReadError(Format(SEJtdExpectGotFmt, [CSJtdString, TOKEN_NAMES[aReader.TokenKind]]), aReader);
  v := GetEnumValue(TypeInfo(TEnum), aReader.AsString);
  if v < 0 then
    ReadError(Format(SEJtdIllegalEnumFmt, [aReader.AsString]), aReader);
  Value := TEnum(v);
end;

procedure TJtdEnum.DoWriteJson(aWriter: TJsonStrWriter);
begin
  aWriter.Add(GetEnumName(TypeInfo(TEnum), Integer(Value)));
end;

{ TJtdNullableEnum }

procedure TJtdNullableEnum.DoReadJson(aReader: TJsonReader);
var
  v: Integer;
begin
  if aReader.TokenKind <> TTokenKind.tkString then
    ReadError(Format(SEJtdExpectGotFmt, [CSJtdString, TOKEN_NAMES[aReader.TokenKind]]), aReader);
  v := GetEnumValue(TypeInfo(T), aReader.AsString);
  if v < 0 then
    ReadError(Format(SEJtdIllegalEnumFmt, [aReader.AsString]), aReader);
  Value := T(v);
end;

procedure TJtdNullableEnum.DoWriteJson(aWriter: TJsonStrWriter);
begin
  aWriter.Add(GetEnumName(TypeInfo(T), Integer(FValue)));
end;

{ TJtdStrEnum }

procedure TJtdStrEnum.SetValue(const aValue: string);
begin
  if Value = aValue then exit;
  if not IsElement(aValue) then
    Error(Format(SEStrNotEnumElemFmt, [aValue, ClassName]));
  Value := aValue;
end;

procedure TJtdStrEnum.DoReadJson(aReader: TJsonReader);
begin
  if aReader.TokenKind <> TTokenKind.tkString then
    ReadError(Format(SEJtdExpectGotFmt, [CSJtdString, TOKEN_NAMES[aReader.TokenKind]]), aReader);
  if not IsElement(aReader.AsString) then
    ReadError(Format(SEStrNotEnumElemFmt, [aReader.AsString, ClassName]), aReader);
  FValue := aReader.AsString;
end;

procedure TJtdStrEnum.DoWriteJson(aWriter: TJsonStrWriter);
begin
  aWriter.Add(Value);
end;

constructor TJtdStrEnum.Create(const aValue: string);
begin
  if not IsElement(aValue) then Error(Format(SEStrNotEnumElemFmt, [aValue, ClassName]));
  inherited Create(aValue);
end;

{ TJtdNullableStrEnum }

procedure TJtdNullableStrEnum.SetValue(const aValue: string);
begin
  if not IsElement(aValue) then Error(Format(SEStrNotEnumElemFmt, [aValue, ClassName]));
  inherited SetValue(aValue);
end;

procedure TJtdNullableStrEnum.DoReadJson(aReader: TJsonReader);
begin
  if aReader.TokenKind <> TTokenKind.tkString then
    ReadError(Format(SEJtdExpectGotFmt, [CSJtdString, TOKEN_NAMES[aReader.TokenKind]]), aReader);
  if not IsElement(aReader.AsString) then
    ReadError(Format(SEStrNotEnumElemFmt, [aReader.AsString, ClassName]), aReader);
  Value := aReader.AsString;
end;

procedure TJtdNullableStrEnum.DoWriteJson(aWriter: TJsonStrWriter);
begin
  aWriter.Add(FValue);
end;

constructor TJtdNullableStrEnum.Create(const aValue: string);
begin
  if not IsElement(aValue) then Error(Format(SEStrNotEnumElemFmt, [aValue, ClassName]));
  inherited Create(aValue);
end;

{ TJtdGenContainer }

destructor TJtdGenContainer.Destroy;
begin
  DoClear;
  inherited Destroy;
end;

procedure TJtdGenContainer.ReadJson(aReader: TJsonReader);
begin
  DoClear;
  inherited ReadJson(aReader);
end;

{ TJtdGenNullableContainer }

destructor TJtdGenNullableContainer.Destroy;
begin
  DoClear;
  inherited Destroy;
end;

procedure TJtdGenNullableContainer.DoSetNull;
begin
  DoClear;
  inherited DoSetNull;
end;

{ TJtdContainer }

procedure TJtdContainer.Clear;
begin
  DoClear;
end;

{ TJtdNullableContainer }

procedure TJtdNullableContainer.Clear;
begin
  DoClear;
end;

{ TJtdList }

function TJtdList.GetCount: SizeInt;
begin
  Result := FList.Count;
end;

function TJtdList.GetItem(aIndex: SizeInt): T;
begin
  Result := FList[aIndex];
end;

procedure TJtdList.SetItem(aIndex: SizeInt; aValue: T);
var
  v: T;
begin
  v := FList[aIndex];
  if v <> aValue then
    begin
      v.Free;
      FList[aIndex] := aValue;
    end;
end;

procedure TJtdList.DoReadJson(aReader: TJsonReader);
begin
  if aReader.TokenKind <> tkArrayBegin then
    ReadError(Format(SEJtdExpectGotFmt, [CSJtdArray, TOKEN_NAMES[aReader.TokenKind]]), aReader);
  repeat
    if not aReader.Read then ReaderFail(aReader);
    if aReader.TokenKind = tkArrayEnd then break;
    FList.Add(T(T.LoadInstance(aReader)));
  until False;
end;

procedure TJtdList.DoWriteJson(aWriter: TJsonStrWriter);
var
  I: SizeInt;
begin
  aWriter.BeginArray;
  for I := 0 to Pred(Count) do
    FList.UncMutable[I]^.WriteJson(aWriter);
  aWriter.EndArray;
end;

procedure TJtdList.DoClear;
var
  I: SizeInt;
begin
  for I := 0 to Pred(FList.Count) do
    FList.UncMutable[I]^.Free;
  FList.MakeEmpty;
end;

function TJtdList.GetEnumerator: TEnumerator;
begin
  Result := FList.GetEnumerator;
end;

function TJtdList.Add(aValue: T): SizeInt;
begin
  Result := FList.Add(aValue);
end;

procedure TJtdList.Insert(aIndex: SizeInt; aValue: T);
begin
  FList.Insert(aIndex, aValue);
end;

procedure TJtdList.Delete(aIndex: SizeInt);
var
  v: T;
begin
  v := FList.Extract(aIndex);
  v.Free;
end;

{ TJtdNullableList }

function TJtdNullableList.GetCount: SizeInt;
begin
  CheckNull;
  Result := FList.Count;
end;

function TJtdNullableList.GetItem(aIndex: SizeInt): T;
begin
  CheckNull;
  Result := FList[aIndex];
end;

procedure TJtdNullableList.SetItem(aIndex: SizeInt; aValue: T);
var
  v: T;
begin
  CheckNull;
  v := FList[aIndex];
  if v <> aValue then
    begin
      v.Free;
      FList[aIndex] := aValue;
    end;
end;

procedure TJtdNullableList.DoReadJson(aReader: TJsonReader);
begin
  if aReader.TokenKind <> tkArrayBegin then
    ReadError(Format(SEJtdExpectGotFmt, [CSJtdArray, TOKEN_NAMES[aReader.TokenKind]]), aReader);
  repeat
    if not aReader.Read then ReaderFail(aReader);
    if aReader.TokenKind = tkArrayEnd then break;
    FList.Add(T(T.LoadInstance(aReader)));
  until False;
end;

procedure TJtdNullableList.DoWriteJson(aWriter: TJsonStrWriter);
var
  I: SizeInt;
begin
  aWriter.BeginArray;
  for I := 0 to Pred(Count) do
    FList.UncMutable[I]^.WriteJson(aWriter);
  aWriter.EndArray;
end;

procedure TJtdNullableList.DoClear;
var
  I: SizeInt;
begin
  for I := 0 to Pred(FList.Count) do
    FList.UncMutable[I]^.Free;
  FList.MakeEmpty;
end;

function TJtdNullableList.GetEnumerator: TEnumerator;
begin
  CheckNull;
  Result := FList.GetEnumerator;
end;

function TJtdNullableList.Add(aValue: T): SizeInt;
begin
  Result := FList.Add(aValue);
  DoAssign;
end;

procedure TJtdNullableList.Insert(aIndex: SizeInt; aValue: T);
begin
  FList.Insert(aIndex, aValue);
  DoAssign;
end;

procedure TJtdNullableList.Delete(aIndex: SizeInt);
var
  v: T;
begin
  CheckNull;
  v := FList.Extract(aIndex);
  v.Free;
end;

{ TJtdMap }

function TJtdMap.GetCount: SizeInt;
begin
  Result := FMap.Count;
end;

function TJtdMap.GetItem(const aKey: string): T;
begin
  if not FMap.TryGetValue(aKey, Result) then Result := nil;
end;

procedure TJtdMap.DoReadJson(aReader: TJsonReader);
var
  p: TMap.PValue;
begin
  if aReader.TokenKind <> tkObjectBegin then ExpectObject(aReader);
  repeat
    if not aReader.Read then ReaderFail(aReader);
    if aReader.TokenKind = tkObjectEnd then break;
    p := FMap.GetMutValueDef(aReader.Name, nil);
    if p^ <> nil then
      ReadError(Format(SEJtdDupValuesKeyFmt, [aReader.Name]), aReader);
    p^ := T(T.LoadInstance(aReader));
  until False;
end;

procedure TJtdMap.DoWriteJson(aWriter: TJsonStrWriter);
var
  e: TJtdMapEntry;
begin
  aWriter.BeginObject;
  for e in FMap do
    begin
      aWriter.AddName(e.Key);
      e.Value.WriteJson(aWriter);
    end;
  aWriter.EndObject;
end;

procedure TJtdMap.DoClear;
var
  v: T;
begin
  for v in FMap.Values do
    v.Free;
  FMap.MakeEmpty;
end;

function TJtdMap.GetEnumerator: TJtdMapEnumerator;
begin
  Result := FMap.GetEnumerator;
end;

function TJtdMap.Keys: TJtdMapKeys;
begin
  Result := FMap.Keys;
end;

function TJtdMap.Values: TJtdMapValues;
begin
  Result := FMap.Values;
end;

function TJtdMap.Contains(const aKey: string): Boolean;
begin
  Result := FMap.Contains(aKey);
end;

function TJtdMap.Add(const aKey: string; aValue: T): Boolean;
begin
  Result := FMap.Add(aKey, aValue);
end;

procedure TJtdMap.AddOrSetValue(const aKey: string; aValue: T);
var
  p: TMap.PValue;
begin
  if FMap.FindOrAddMutValue(aKey, p) and (p^ <> aValue) then
    p^.Free;
  p^ := aValue;
end;

function TJtdMap.TryGetValue(const aKey: string; out aValue: T): Boolean;
begin
  Result := FMap.TryGetValue(aKey, aValue);
end;

function TJtdMap.Remove(const aKey: string): Boolean;
var
  v: T = nil;
begin
  Result := FMap.Extract(aKey, v);
  if Result then
    v.Free;
end;

{ TJtdNullableMap }

function TJtdNullableMap.GetCount: SizeInt;
begin
  CheckNull;
  Result := FMap.Count;
end;

function TJtdNullableMap.GetItem(const aKey: string): T;
begin
  CheckNull;
  if not FMap.TryGetValue(aKey, Result) then Result := nil;
end;

procedure TJtdNullableMap.DoReadJson(aReader: TJsonReader);
var
  p: TMap.PValue;
begin
  if aReader.TokenKind <> tkObjectBegin then ExpectObject(aReader);
  repeat
    if not aReader.Read then ReaderFail(aReader);
    if aReader.TokenKind = tkObjectEnd then break;
    p := FMap.GetMutValueDef(aReader.Name, nil);
    if p^ <> nil then
      ReadError(Format(SEJtdDupValuesKeyFmt, [aReader.Name]), aReader);
    p^ := T(T.LoadInstance(aReader));
  until False;
end;

procedure TJtdNullableMap.DoWriteJson(aWriter: TJsonStrWriter);
var
  e: TJtdMapEntry;
begin
  aWriter.BeginObject;
  for e in FMap do
    begin
      aWriter.AddName(e.Key);
      e.Value.WriteJson(aWriter);
    end;
  aWriter.EndObject;
end;

procedure TJtdNullableMap.DoClear;
var
  v: T;
begin
  for v in FMap.Values do
    v.Free;
  FMap.MakeEmpty;
end;

function TJtdNullableMap.GetEnumerator: TJtdMapEnumerator;
begin
  CheckNull;
  Result := FMap.GetEnumerator;
end;

function TJtdNullableMap.Keys: TJtdMapKeys;
begin
  CheckNull;
  Result := FMap.Keys;
end;

function TJtdNullableMap.Values: TJtdMapValues;
begin
  CheckNull;
  Result := FMap.Values;
end;

function TJtdNullableMap.Contains(const aKey: string): Boolean;
begin
  CheckNull;
  Result := FMap.Contains(aKey);
end;

function TJtdNullableMap.Add(const aKey: string; aValue: T): Boolean;
begin
  Result := FMap.Add(aKey, aValue);
  DoAssign;
end;

procedure TJtdNullableMap.AddOrSetValue(const aKey: string; aValue: T);
var
  p: TMap.PValue;
begin
  if FMap.FindOrAddMutValue(aKey, p) and (p^ <> aValue) then
    p^.Free;
  p^ := aValue;
  DoAssign;
end;

function TJtdNullableMap.TryGetValue(const aKey: string; out aValue: T): Boolean;
begin
  CheckNull;
  Result := FMap.TryGetValue(aKey, aValue);
end;

function TJtdNullableMap.Remove(const aKey: string): Boolean;
var
  v: T = nil;
begin
  CheckNull;
  Result := FMap.Extract(aKey, v);
  if Result then
    v.Free;
end;

{ TJtdObject }

procedure TJtdObject.DoWriteJson(aWriter: TJsonStrWriter);
begin
  aWriter.BeginObject;
  WriteFields(aWriter);
  aWriter.EndObject;
end;

constructor TJtdObject.Create;
begin
  inherited Create;
  CreateFields;
end;

destructor TJtdObject.Destroy;
begin
  ClearFields;
  inherited Destroy;
end;

{ TJtdNullableObject }

procedure TJtdNullableObject.CheckFields;
begin
  if IsNull then
    begin
      CreateFields;
      DoAssign;
    end;
end;

procedure TJtdNullableObject.DoWriteJson(aWriter: TJsonStrWriter);
begin
  aWriter.BeginObject;
  WriteFields(aWriter);
  aWriter.EndObject;
end;

procedure TJtdNullableObject.ReadJson(aReader: TJsonReader);
begin
  if (aReader.ReadState = rsStart) and not aReader.Read then ReaderFail(aReader);
  SetNull;
  if aReader.TokenKind = tkNull then exit;
  CreateFields;
  DoReadJson(aReader);
  DoAssign;
end;

{ TJtdUnion }

procedure TJtdUnion.DoReadJson(aReader: TJsonReader);
var
  s, TagName, TagValue, LPath: string;
  Node, TagNode: TJsonNode;
  InstClass: TJtdEntityClass;
begin
  if aReader.TokenKind <> tkObjectBegin then
    ReadError(Format(SEJtdExpectGotFmt, [CSJtdObject, TOKEN_NAMES[aReader.TokenKind]]), aReader);
  LPath := aReader.Path;
  if not aReader.CopyStruct(s) then ReaderFail(aReader);
  if not TJsonNode.TryParse(s, Node) then InternalError(2);
  try
    TagName := GetTagJsonName;
    if not Node.Find(TagName, TagNode) then
      ReadError(Format(SEJtdTagNotFoundFmt, [TagName]), LPath);
    if not TagNode.IsString then ReadError(SEJtdDiscriTagNotStr, LPath);
    TagValue := TagNode.AsString;
    InstClass := GetInstanceClass(TagValue);
    if InstClass = nil then
      ReadError(Format(SEJtdIllegalTagValueFmt, [TagValue]), LPath);
    if not Node.Remove(TagName) then InternalError(3);
    FTag := '';
    FreeAndNil(FInstance);
    try
      FInstance := InstClass.LoadInstance(Node.AsJson);
    except
      ReadError(SEJtdInvalidDiscriInst, LPath);
    end;
    FTag := TagValue;
  finally
    Node.Free;
  end;
end;

procedure TJtdUnion.DoWriteJson(aWriter: TJsonStrWriter);
begin
  aWriter.BeginObject;
  aWriter.Add(GetTagJsonName, Tag);
  TJtdObject(FInstance).WriteFields(aWriter);
  aWriter.EndObject;
end;

procedure TJtdUnion.DoClear;
begin
  FTag := '';
  FInstance.Free;
end;

function TJtdUnion.GetInstanceClass: TJtdEntityClass;
begin
  Result := GetInstanceClass(Tag);
end;

{ TJtdNullableUnion }

procedure TJtdNullableUnion.DoReadJson(aReader: TJsonReader);
var
  s, TagName, TagValue, LPath: string;
  Node, TagNode: TJsonNode;
  InstClass: TJtdEntityClass;
begin
  if aReader.TokenKind <> tkObjectBegin then
    ReadError(Format(SEJtdExpectGotFmt, [CSJtdObject, TOKEN_NAMES[aReader.TokenKind]]), aReader);
  LPath := aReader.Path;
  if not aReader.CopyStruct(s) then ReaderFail(aReader);
  if not TJsonNode.TryParse(s, Node) then InternalError(4);
  try
    TagName := GetTagJsonName;
    if not Node.Find(TagName, TagNode) then
      ReadError(Format(SEJtdTagNotFoundFmt, [TagName]), LPath);
    if not TagNode.IsString then ReadError(SEJtdDiscriTagNotStr, LPath);
    TagValue := TagNode.AsString;
    InstClass := GetInstanceClass(TagValue);
    if InstClass = nil then
      ReadError(Format(SEJtdIllegalTagValueFmt, [TagValue]), LPath);
    if not Node.Remove(TagName) then InternalError(5);
    try
      FInstance := InstClass.LoadInstance(Node.AsJson);
    except
      ReadError(SEJtdInvalidDiscriInst, LPath);
    end;
    FTag := TagValue;
  finally
    Node.Free;
  end;
end;

procedure TJtdNullableUnion.DoWriteJson(aWriter: TJsonStrWriter);
begin
  aWriter.BeginObject;
  aWriter.Add(GetTagJsonName, Tag);
  TJtdObject(FInstance).WriteFields(aWriter);
  aWriter.EndObject;
end;

procedure TJtdNullableUnion.DoClear;
begin
  FTag := '';
  FreeAndNil(FInstance);
end;

function TJtdNullableUnion.GetInstanceClass: TJtdEntityClass;
begin
  Result := GetInstanceClass(Tag);
end;

end.
