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

{$MODE OBJFPC}{$H+}{$OBJECTCHECKS OFF}

interface

uses
  Classes, SysUtils, lgUtils, lgHelpers, lgVector, lgHashMap, lgJson, lgJsonTypeDef;

type
  EJtdException = class(Exception);

  EJtdReadJson = class(EJtdException)
  private
    FPath: string;
  public
    constructor Create(const aMessage, aPath: string); overload;
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
  private
    FIsNull: Boolean;
  protected
    procedure DoReadJson(aNode: TJsonNode); virtual;
    procedure DoReadJson(aReader: TJsonReader); virtual;
    procedure DoWriteJson(aWriter: TJsonStrWriter); virtual;
    class function  GetJtdClass: TJtdEntityClass; virtual;
    class procedure Error(const aMessage: string); static;
    class procedure NotImplemented(const aMethod: string); static;
    class procedure ReadError(const aMessage: string; aReader: TJsonReader); static;
    class procedure ReadError(const aFmt: string; const Args: array of const; aReader: TJsonReader); static;
    class procedure ReadError(const aMessage: string); static;
    class procedure ReadError(const aFmt: string; const Args: array of const); static;
    class procedure ReaderFail(aReader: TJsonReader); static;
    class function  ReadInt(aReader: TJsonReader): Int64; static;
    class function  ReadInt(aNode: TJsonNode): Int64; static;
    class procedure ExpectObject(aReader: TJsonReader); static;
    class procedure ExpectObject(aNode: TJsonNode); static;
    class procedure InternalError(aNumber: Byte); static;
  public
  { if the loading fails, an exception will be raised }
    class function LoadInstance(aNode: TJsonNode): TJtdEntity;
    class function LoadInstance(aReader: TJsonReader): TJtdEntity;
    class function LoadInstance(aBuffer: PAnsiChar; aCount: SizeInt): TJtdEntity;
    class function LoadInstance(const aJson: string): TJtdEntity;
    class function LoadInstance(aStream: TStream; aSkipBom: Boolean = False; aBufSize: SizeInt = DEF_BUF_SIZE;
                                aMaxDepth: SizeInt = DEF_DEPTH): TJtdEntity;
  { returns NIL in case of failed loading }
    class function TryLoad(aBuffer: PAnsiChar; aCount: SizeInt): TJtdEntity;
    class function TryLoad(aNode: TJsonNode): TJtdEntity;
    class function TryLoad(const aJson: string): TJtdEntity;
    class function TryLoad(aStream: TStream; aSkipBom: Boolean = False; aBufSize: SizeInt = DEF_BUF_SIZE;
                          aMaxDepth: SizeInt = DEF_DEPTH): TJtdEntity;
    class function TryLoadFile(const aFileName: string; aSkipBom: Boolean = False;
                               aBufSize: SizeInt = DEF_BUF_SIZE; aMaxDepth: SizeInt = DEF_DEPTH): TJtdEntity;
    constructor Create; virtual;
    constructor CreateNull; virtual;
    procedure ReadJson(aNode: TJsonNode); virtual;
    procedure ReadJson(aReader: TJsonReader); virtual;
  { if the loading fails, an exception will be raised }
    procedure Load(aBuffer: PAnsiChar; aCount: SizeInt);
    procedure Load(const aJson: string);
    procedure Load(aStream: TStream; aSkipBom: Boolean = False; aBufSize: SizeInt = DEF_BUF_SIZE;
                   aMaxDepth: SizeInt = DEF_DEPTH);
    procedure LoadFile(const aFileName: string; aSkipBom: Boolean = False; aBufSize: SizeInt = DEF_BUF_SIZE;
                       aMaxDepth: SizeInt = DEF_DEPTH);
    function  IsNull: Boolean; virtual;
    procedure SetNull;
    procedure WriteJson(aWriter: TJsonStrWriter); virtual;
    function  AsJson: string;
  end;

  { TJtdValue }
  generic TJtdValue<T> = class abstract(TJtdEntity)
  protected
    FValue: T;
  public
    constructor Create(const aValue: T); virtual; overload;
    function OrElse(const aDefault: T): T;
    property Value: T read FValue write FValue;
  end;

  { TJtdAny: container for JTD "empty" form }
  TJtdAny = class sealed(TJtdEntity)
  private
    FInstance: TJsonNode;
  protected
    procedure DoReadJson(aNode: TJsonNode); override;
    procedure DoReadJson(aReader: TJsonReader); override;
    procedure DoWriteJson(aWriter: TJsonStrWriter); override;
  public
    constructor Create; override;
    destructor Destroy; override;
    function  IsNull: Boolean; override;
    property Instance: TJsonNode read FInstance;
  end;

  { TJtdBool: container for JTD "boolean" type }
  TJtdBool = class sealed(specialize TJtdValue<Boolean>)
  protected
    procedure DoReadJson(aNode: TJsonNode); override;
    procedure DoReadJson(aReader: TJsonReader); override;
    procedure DoWriteJson(aWriter: TJsonStrWriter); override;
  end;

  { TJtdFloat32: container for JTD "float32" type }
  TJtdFloat32 = class sealed(specialize TJtdValue<Single>)
  protected
    procedure DoReadJson(aNode: TJsonNode); override;
    procedure DoReadJson(aReader: TJsonReader); override;
    procedure DoWriteJson(aWriter: TJsonStrWriter); override;
  end;

  { TJtdFloat64: container for JTD "float64" type }
  TJtdFloat64 = class sealed(specialize TJtdValue<Double>)
  protected
    procedure DoReadJson(aNode: TJsonNode); override;
    procedure DoReadJson(aReader: TJsonReader); override;
    procedure DoWriteJson(aWriter: TJsonStrWriter); override;
  end;

  { TJtdInt8: container for JTD "int8" type }
  TJtdInt8 = class sealed(specialize TJtdValue<Int8>)
  protected
    procedure DoReadJson(aNode: TJsonNode); override;
    procedure DoReadJson(aReader: TJsonReader); override;
    procedure DoWriteJson(aWriter: TJsonStrWriter); override;
  end;

  { TJtdUInt8: container for JTD "uint8" type }
  TJtdUInt8 = class sealed(specialize TJtdValue<UInt8>)
  protected
    procedure DoReadJson(aNode: TJsonNode); override;
    procedure DoReadJson(aReader: TJsonReader); override;
    procedure DoWriteJson(aWriter: TJsonStrWriter); override;
  end;

  { TJtdInt16: container for JTD "int16" type }
  TJtdInt16 = class sealed(specialize TJtdValue<Int16>)
  protected
    procedure DoReadJson(aNode: TJsonNode); override;
    procedure DoReadJson(aReader: TJsonReader); override;
    procedure DoWriteJson(aWriter: TJsonStrWriter); override;
  end;

  { TJtdUInt16: container for JTD "uint16" type }
  TJtdUInt16 = class sealed(specialize TJtdValue<UInt16>)
  protected
    procedure DoReadJson(aNode: TJsonNode); override;
    procedure DoReadJson(aReader: TJsonReader); override;
    procedure DoWriteJson(aWriter: TJsonStrWriter); override;
  end;

  { TJtdInt32: container for JTD "int32" type }
  TJtdInt32 = class sealed(specialize TJtdValue<Int32>)
  protected
    procedure DoReadJson(aNode: TJsonNode); override;
    procedure DoReadJson(aReader: TJsonReader); override;
    procedure DoWriteJson(aWriter: TJsonStrWriter); override;
  end;

  { TJtdUInt32: container for JTD "uint32" type }
  TJtdUInt32 = class sealed(specialize TJtdValue<UInt32>)
  protected
    procedure DoReadJson(aNode: TJsonNode); override;
    procedure DoReadJson(aReader: TJsonReader); override;
    procedure DoWriteJson(aWriter: TJsonStrWriter); override;
  end;

  { TJtdString: container for JTD "string" type }
  TJtdString = class sealed(specialize TJtdValue<string>)
  protected
    procedure DoReadJson(aNode: TJsonNode); override;
    procedure DoReadJson(aReader: TJsonReader); override;
    procedure DoWriteJson(aWriter: TJsonStrWriter); override;
  end;

  { TJtdDateTimeUTC: container for JTD "timestamp" type }
  TJtdDateTimeUTC = class sealed(specialize TJtdValue<TDateTime>)
  protected
    procedure DoReadJson(aNode: TJsonNode); override;
    procedure DoReadJson(aReader: TJsonReader); override;
    procedure DoWriteJson(aWriter: TJsonStrWriter); override;
  end;

  { TJtdEnum: generic container for JDT "enum" form when the enum elements are valid Pascal identifiers }
  generic TJtdEnum<TEnum> = class(specialize TJtdValue<TEnum>)
  protected
    procedure DoReadJson(aNode: TJsonNode); override;
    procedure DoReadJson(aReader: TJsonReader); override;
    procedure DoWriteJson(aWriter: TJsonStrWriter); override;
  end;

  { TJtdStrEnum: container for JDT "enum" form when the enum elements are not valid Pascal identifiers }
  TJtdStrEnum = class abstract(specialize TJtdValue<string>)
  protected
    procedure SetValue(const aValue: string);
    procedure DoReadJson(aNode: TJsonNode); override;
    procedure DoReadJson(aReader: TJsonReader); override;
    procedure DoWriteJson(aWriter: TJsonStrWriter); override;
  public
    class function IsElement(const aValue: string): Boolean; virtual; abstract;
    constructor Create(const aValue: string); override;
    property Value: string read FValue write SetValue;
  end;

  { TJtdGenContainer: abstract ancestor class }
  TJtdGenContainer = class abstract(TJtdEntity)
    destructor Destroy; override;
    procedure Clear; virtual; abstract;
  end;

  { TJtdList: generic container for JTD "elements" form }
  generic TJtdList<T: TJtdEntity> = class(TJtdGenContainer)
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
    procedure DoReadJson(aNode: TJsonNode); override;
    procedure DoReadJson(aReader: TJsonReader); override;
    procedure DoWriteJson(aWriter: TJsonStrWriter); override;
  public
    function  GetEnumerator: TEnumerator; inline;
    procedure Clear; override;
    function  Add(aValue: T): SizeInt;
    procedure Insert(aIndex: SizeInt; aValue: T);
    procedure Delete(aIndex: SizeInt); inline;
    property  Count: SizeInt read GetCount;
    property  Items[aIndex: SizeInt]: T read GetItem write SetItem; default;
  end;

  { TJtdMap: generic container for JTD "values" form }
  generic TJtdMap<T: TJtdEntity> = class(TJtdGenContainer)
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
    procedure DoReadJson(aNode: TJsonNode); override;
    procedure DoReadJson(aReader: TJsonReader); override;
    procedure DoWriteJson(aWriter: TJsonStrWriter); override;
  public
  { enumerates all pairs (aKey, aValue) }
    function  GetEnumerator: TJtdMapEnumerator; inline;
  { enumerates all keys }
    function  Keys: TJtdMapKeys; inline;
  { enumerates all values }
    function  Values: TJtdMapValues; inline;
    procedure Clear; override;
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
    class procedure PropNotFound(const aJsonPropName: string; aReader: TJsonReader); static;
    class procedure PropNotFound(const aJsonPropName: string); static;
    class procedure UnknownProp(const aJsonPropName: string; aReader: TJsonReader); static;
    class procedure UnknownProp(const aJsonPropName: string); static;
    class procedure DuplicateProp(aReader: TJsonReader); static;
    class procedure DuplicateProp(const aProp: string); static;
  end;

  { TJtdUnion: abstract ancestor class for JTD "discriminator" form }
  TJtdUnion = class abstract(TJtdEntity)
  protected
    FTag: string;
    FInstance: TJtdEntity;
    procedure DoReadJson(aNode: TJsonNode); override;
    procedure DoReadJson(aReader: TJsonReader); override;
    procedure DoWriteJson(aWriter: TJsonStrWriter); override;
    class function GetTagJsonName: string; virtual; abstract;
    class function GetInstanceClass(const aTag: string): TJtdEntityClass; virtual; abstract;
  public
    destructor Destroy; override;
    function GetInstanceClass: TJtdEntityClass;
    property Tag: string read FTag;
  end;

type
  TTokenKind   = TJsonReader.TTokenKind;
  TJtdFormKind = TJtdSchema.TFormKind;
  TJtdType     = TJtdSchema.TJtdType;
  TJsValueKind = LgJson.TJsValueKind;

const
{$PUSH}{$J-}
  JTD_TYPE_IMPL: array[TJtdType] of TJtdEntityClass = (
    nil, TJtdBool, TJtdFloat32, TJtdFloat64, TJtdInt8, TJtdUInt8, TJtdInt16,
    TJtdUInt16, TJtdInt32, TJtdUInt32, TJtdString, TJtdDateTimeUTC
  );
  JTD_FORM_ANCESTORS: array[TJtdFormKind] of string = (
    '', 'TJtdAny', '', '', 'TJtdEnum', 'TJtdList', 'TJtdObject', 'TJtdMap', 'TJtdUnion'
  );
{$POP}

const
  CSJtdNull       = 'Null';
  CSJtdBool       = 'Boolean';
  CSJtdNumber     = 'Number';
  CSJtdString     = 'String';
  CSJtdInteger    = 'Integer';
  CSJtdFractional = 'Fractional';
  CSJtdArray      = 'Array';
  CSJtdObject     = 'Object';
{$PUSH}{$J-}
  TOKEN_NAMES: array[TTokenKind] of string = (
    'trash', CSJtdNull, CSJtdBool, CSJtdBool, CSJtdNumber, CSJtdString,
    CSJtdArray, CSJtdObject, 'trash', 'trash'
  );
  //(jvkNull, jvkFalse, jvkTrue, jvkNumber, jvkString, jvkArray, jvkObject);
  JKIND_NAMES: array[TJsValueKind] of string = (
    CSJtdNull, CSJtdBool, CSJtdBool, CSJtdNumber, CSJtdString, CSJtdArray, CSJtdObject
  );
{$POP}

implementation
{$B-}{$COPERATORS ON}{$POINTERMATH ON}
uses
  TypInfo, Math, lgStrConst;

const
  SEJtdInternalFmt = 'JTD internal error #%u';
  SEJtdNotImplFmt  = 'Method %s.%s not implemented';

{ EJtdReadJson }

constructor EJtdReadJson.Create(const aMessage, aPath: string);
begin
  Create(aMessage);
  FPath := aPath;
end;

{ TJtdEntity }

procedure TJtdEntity.DoReadJson(aNode: TJsonNode);
begin
  Assert(aNode = aNode); // make compiler happy
  NotImplemented({$I %currentRoutine%});
end;

procedure TJtdEntity.DoReadJson(aReader: TJsonReader);
begin
  Assert(aReader = aReader); // make compiler happy
  NotImplemented({$I %currentRoutine%});
end;

procedure TJtdEntity.DoWriteJson(aWriter: TJsonStrWriter);
begin
  Assert(aWriter = aWriter); // make compiler happy
  NotImplemented({$I %currentRoutine%});
end;

class function TJtdEntity.GetJtdClass: TJtdEntityClass;
begin
  Result := Self;
end;

class procedure TJtdEntity.Error(const aMessage: string);
begin
  raise EJtdException.Create(aMessage);
end;

class procedure TJtdEntity.NotImplemented(const aMethod: string);
begin
  Error(Format(SEJtdNotImplFmt, [ClassName, aMethod]));
end;

class procedure TJtdEntity.ReadError(const aMessage: string; aReader: TJsonReader);
begin
  raise EJtdReadJson.Create(aMessage, aReader.Path);
end;

class procedure TJtdEntity.ReadError(const aFmt: string; const Args: array of const; aReader: TJsonReader);
begin
  raise EJtdReadJson.Create(Format(aFmt, Args), aReader.Path);
end;

class procedure TJtdEntity.ReadError(const aMessage: string);
begin
  raise EJtdReadJson.Create(aMessage);
end;

class procedure TJtdEntity.ReadError(const aFmt: string; const Args: array of const);
begin
  raise EJtdReadJson.Create(Format(aFmt, Args));
end;

class procedure TJtdEntity.ReaderFail(aReader: TJsonReader);
begin
  if aReader.ReadState = rsEof then
    raise EJtdReadJson.Create(SEUnexpectJsonEnd)
  else
    raise EJtdReadJson.Create(SEInvalidJsonInst, aReader.Path);
end;

class function TJtdEntity.ReadInt(aReader: TJsonReader): Int64;
begin
  if aReader.TokenKind <> tkNumber then
    ReadError(SEJtdExpectGotFmt, [CSJtdNumber, TOKEN_NAMES[aReader.TokenKind]], aReader);
  if not Double.IsExactInt(aReader.AsNumber, Result) then
    ReadError(SEJtdExpectGotFmt, [CSJtdInteger, TOKEN_NAMES[aReader.TokenKind]], aReader);
end;

class function TJtdEntity.ReadInt(aNode: TJsonNode): Int64;
begin
  if not aNode.IsNumber then
    ReadError(SEJtdExpectGotFmt, [CSJtdNumber, JKIND_NAMES[aNode.Kind]]);
  if not Double.IsExactInt(aNode.AsNumber, Result) then
    ReadError(SEJtdExpectGotFmt, [CSJtdInteger, JKIND_NAMES[aNode.Kind]]);
end;

class procedure TJtdEntity.ExpectObject(aReader: TJsonReader);
begin
  ReadError(SEJtdExpectGotFmt, [CSJtdObject, TOKEN_NAMES[aReader.TokenKind]], aReader);
end;

class procedure TJtdEntity.ExpectObject(aNode: TJsonNode);
begin
  ReadError(SEJtdExpectGotFmt, [CSJtdObject, JKIND_NAMES[aNode.Kind]]);
end;

class procedure TJtdEntity.InternalError(aNumber: Byte);
begin
  Error(Format(SEJtdInternalFmt, [aNumber]));
end;

class function TJtdEntity.LoadInstance(aNode: TJsonNode): TJtdEntity;
begin
  Result := GetJtdClass.Create;
  Result.ReadJson(aNode);
end;

class function TJtdEntity.LoadInstance(aReader: TJsonReader): TJtdEntity;
begin
  Result := GetJtdClass.Create;
  Result.ReadJson(aReader);
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
    FreeAndNil(Result);
  end;
end;

class function TJtdEntity.TryLoad(aNode: TJsonNode): TJtdEntity;
begin
  try
    Result := LoadInstance(aNode);
  except
    FreeAndNil(Result);
  end;
end;

class function TJtdEntity.TryLoad(const aJson: string): TJtdEntity;
begin
  try
    Result := LoadInstance(aJson);
  except
    FreeAndNil(Result);
  end;
end;

class function TJtdEntity.TryLoad(aStream: TStream; aSkipBom: Boolean; aBufSize: SizeInt;
  aMaxDepth: SizeInt): TJtdEntity;
begin
  try
    Result := LoadInstance(aStream, aSkipBom, aBufSize, aMaxDepth);
  except
    FreeAndNil(Result);
  end;
end;

class function TJtdEntity.TryLoadFile(const aFileName: string; aSkipBom: Boolean; aBufSize: SizeInt;
  aMaxDepth: SizeInt): TJtdEntity;
begin
  Result := nil;
  try
    with TFileStream.Create(aFileName, fmOpenRead or fmShareDenyWrite) do
      try
        Result := TryLoad(TStream(GetSelfRef), aSkipBom, aBufSize, aMaxDepth);
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

constructor TJtdEntity.CreateNull;
begin
  inherited Create;
  FIsNull := True;
end;

procedure TJtdEntity.ReadJson(aNode: TJsonNode);
begin
  FIsNull := False;
  if aNode.IsNull then
    begin
      SetNull;
      exit;
    end;
  DoReadJson(aNode);
end;

procedure TJtdEntity.ReadJson(aReader: TJsonReader);
begin
  if (aReader.ReadState = rsStart) and not aReader.Read then ReaderFail(aReader);
  FIsNull := False;
  if aReader.TokenKind = tkNull then
    begin
      SetNull;
      exit;
    end;
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

function TJtdEntity.IsNull: Boolean;
begin
  Result := (Self = nil) or FIsNull;
end;

procedure TJtdEntity.SetNull;
begin
  FIsNull := True;
end;

procedure TJtdEntity.WriteJson(aWriter: TJsonStrWriter);
begin
  if IsNull then
    aWriter.AddNull
  else
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

{ TJtdValue }

constructor TJtdValue.Create(const aValue: T);
begin
  inherited Create;
  Value := aValue;
end;

function TJtdValue.OrElse(const aDefault: T): T;
begin
  if IsNull then
    Result := aDefault
  else
    Result := FValue;
end;

{ TJtdAny }

procedure TJtdAny.DoReadJson(aNode: TJsonNode);
begin
  if not TJsonNode.DuplicateFree(aNode) then
    ReadError(SEJtdInputKeysNotUniq);
  FInstance.CopyFrom(aNode);
end;

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
        if not TJsonNode.DuplicateFree(Instance) then ReadError(SEJtdInputKeysNotUniq);
      end;
  else
    InternalError(1);
  end;
end;

procedure TJtdAny.DoWriteJson(aWriter: TJsonStrWriter);
begin
  aWriter.Add(Instance);
end;

constructor TJtdAny.Create;
begin
  inherited Create;
  FInstance := TJsonNode.Create;
end;

destructor TJtdAny.Destroy;
begin
  FInstance.Free;
  inherited Destroy;
end;

function TJtdAny.IsNull: Boolean;
begin
  Result := inherited;
  if not Result then
    Result := FInstance.IsNull;
end;

{ TJtdBool }

procedure TJtdBool.DoReadJson(aNode: TJsonNode);
begin
  if not aNode.IsBoolean then
    ReadError(SEJtdExpectGotFmt, [CSJtdBool, JKIND_NAMES[aNode.Kind]]);
  FValue := aNode.IsTrue;
end;

procedure TJtdBool.DoReadJson(aReader: TJsonReader);
begin
  if not(aReader.TokenKind in [tkFalse, tkTrue]) then
    ReadError(SEJtdExpectGotFmt, [CSJtdBool, TOKEN_NAMES[aReader.TokenKind]], aReader);
  FValue := aReader.TokenKind = tkTrue;
end;

procedure TJtdBool.DoWriteJson(aWriter: TJsonStrWriter);
begin
  if FValue then
    aWriter.AddTrue
  else
    aWriter.AddFalse;
end;

{ TJtdFloat32 }

procedure TJtdFloat32.DoReadJson(aNode: TJsonNode);
var
  d: Double;
begin
  if not aNode.IsNumber then
    ReadError(SEJtdExpectGotFmt, [CSJtdNumber, JKIND_NAMES[aNode.Kind]]);
  d := aNode.AsNumber;
  if System.Abs(d) > Math.MaxSingle then
    ReadError(SEJtdNumRangeFmt, [ClassName]);
  FValue := d;
end;

procedure TJtdFloat32.DoReadJson(aReader: TJsonReader);
var
  d: Double;
begin
  if aReader.TokenKind <> tkNumber then
    ReadError(SEJtdExpectGotFmt, [CSJtdNumber, TOKEN_NAMES[aReader.TokenKind]], aReader);
  d := aReader.AsNumber;
  if System.Abs(d) > Math.MaxSingle then
    ReadError(SEJtdNumRangeFmt, [ClassName], aReader);
  FValue := d;
end;

procedure TJtdFloat32.DoWriteJson(aWriter: TJsonStrWriter);
begin
  aWriter.Add(FValue);
end;

{ TJtdFloat64 }

procedure TJtdFloat64.DoReadJson(aNode: TJsonNode);
begin
  if not aNode.IsNumber then
    ReadError(SEJtdExpectGotFmt, [CSJtdNumber, JKIND_NAMES[aNode.Kind]]);
  FValue := aNode.AsNumber;
end;

procedure TJtdFloat64.DoReadJson(aReader: TJsonReader);
begin
  if aReader.TokenKind <> tkNumber then
    ReadError(SEJtdExpectGotFmt, [CSJtdNumber, TOKEN_NAMES[aReader.TokenKind]], aReader);
  FValue := aReader.AsNumber;
end;

procedure TJtdFloat64.DoWriteJson(aWriter: TJsonStrWriter);
begin
  aWriter.Add(FValue);
end;

{ TJtdInt8 }

procedure TJtdInt8.DoReadJson(aNode: TJsonNode);
var
  I: Int64;
begin
  I := ReadInt(aNode);
  if (I < System.Low(Value)) or (I > System.High(Value)) then
    ReadError(SEJtdNumRangeFmt, [ClassName]);
  FValue := I;
end;

procedure TJtdInt8.DoReadJson(aReader: TJsonReader);
var
  I: Int64;
begin
  I := ReadInt(aReader);
  if (I < System.Low(Value)) or (I > System.High(Value)) then
    ReadError(SEJtdNumRangeFmt, [ClassName], aReader);
  FValue := I;
end;

procedure TJtdInt8.DoWriteJson(aWriter: TJsonStrWriter);
begin
  aWriter.Add(FValue);
end;

{ TJtdUInt8 }

procedure TJtdUInt8.DoReadJson(aNode: TJsonNode);
var
  I: Int64;
begin
  I := ReadInt(aNode);
  if (I < System.Low(Value)) or (I > System.High(Value)) then
    ReadError(SEJtdNumRangeFmt, [ClassName]);
  FValue := I;
end;

procedure TJtdUInt8.DoReadJson(aReader: TJsonReader);
var
  I: Int64;
begin
  I := ReadInt(aReader);
  if (I < System.Low(Value)) or (I > System.High(Value)) then
    ReadError(SEJtdNumRangeFmt, [ClassName], aReader);
  FValue := I;
end;

procedure TJtdUInt8.DoWriteJson(aWriter: TJsonStrWriter);
begin
  aWriter.Add(FValue);
end;

{ TJtdInt16 }

procedure TJtdInt16.DoReadJson(aNode: TJsonNode);
var
  I: Int64;
begin
  I := ReadInt(aNode);
  if (I < System.Low(Value)) or (I > System.High(Value)) then
    ReadError(SEJtdNumRangeFmt, [ClassName]);
  FValue := I;
end;

procedure TJtdInt16.DoReadJson(aReader: TJsonReader);
var
  I: Int64;
begin
  I := ReadInt(aReader);
  if (I < System.Low(Value)) or (I > System.High(Value)) then
    ReadError(SEJtdNumRangeFmt, [ClassName], aReader);
  FValue := I;
end;

procedure TJtdInt16.DoWriteJson(aWriter: TJsonStrWriter);
begin
  aWriter.Add(FValue);
end;

{ TJtdUInt16 }

procedure TJtdUInt16.DoReadJson(aNode: TJsonNode);
var
  I: Int64;
begin
  I := ReadInt(aNode);
  if (I < System.Low(Value)) or (I > System.High(Value)) then
    ReadError(SEJtdNumRangeFmt, [ClassName]);
  FValue := I;
end;

procedure TJtdUInt16.DoReadJson(aReader: TJsonReader);
var
  I: Int64;
begin
  I := ReadInt(aReader);
  if (I < System.Low(Value)) or (I > System.High(Value)) then
    ReadError(SEJtdNumRangeFmt, [ClassName], aReader);
  FValue := I;
end;

procedure TJtdUInt16.DoWriteJson(aWriter: TJsonStrWriter);
begin
  aWriter.Add(FValue);
end;

{ TJtdInt32 }

procedure TJtdInt32.DoReadJson(aNode: TJsonNode);
var
  I: Int64;
begin
  I := ReadInt(aNode);
  if (I < System.Low(Value)) or (I > System.High(Value)) then
    ReadError(SEJtdNumRangeFmt, [ClassName]);
  FValue := I;
end;

procedure TJtdInt32.DoReadJson(aReader: TJsonReader);
var
  I: Int64;
begin
  I := ReadInt(aReader);
  if (I < System.Low(Value)) or (I > System.High(Value)) then
    ReadError(SEJtdNumRangeFmt, [ClassName], aReader);
  FValue := I;
end;

procedure TJtdInt32.DoWriteJson(aWriter: TJsonStrWriter);
begin
  aWriter.Add(FValue);
end;

{ TJtdUInt32 }

procedure TJtdUInt32.DoReadJson(aNode: TJsonNode);
var
  I: Int64;
begin
  I := ReadInt(aNode);
  if (I < System.Low(Value)) or (I > System.High(Value)) then
    ReadError(SEJtdNumRangeFmt, [ClassName]);
  FValue := I;
end;

procedure TJtdUInt32.DoReadJson(aReader: TJsonReader);
var
  I: Int64;
begin
  I := ReadInt(aReader);
  if (I < System.Low(Value)) or (I > System.High(Value)) then
    ReadError(SEJtdNumRangeFmt, [ClassName], aReader);
  FValue := I;
end;

procedure TJtdUInt32.DoWriteJson(aWriter: TJsonStrWriter);
begin
  aWriter.Add(FValue);
end;

{ TJtdString }

procedure TJtdString.DoReadJson(aNode: TJsonNode);
begin
  if not aNode.IsString then
    ReadError(SEJtdExpectGotFmt, [CSJtdString, JKIND_NAMES[aNode.Kind]]);
  FValue := aNode.AsString;
end;

procedure TJtdString.DoReadJson(aReader: TJsonReader);
begin
  if aReader.TokenKind <> TTokenKind.tkString then
    ReadError(SEJtdExpectGotFmt, [CSJtdString, TOKEN_NAMES[aReader.TokenKind]], aReader);
  FValue := aReader.AsString;
end;

procedure TJtdString.DoWriteJson(aWriter: TJsonStrWriter);
begin
  aWriter.Add(FValue);
end;

{ TJtdDateTimeUTC }

procedure TJtdDateTimeUTC.DoReadJson(aNode: TJsonNode);
var
  d: TDateTime;
begin
  if not aNode.IsString then
    ReadError(SEJtdExpectGotFmt, [CSJtdString, JKIND_NAMES[aNode.Kind]]);
  if not TryRfc8927TimeStampToUTC(aNode.AsString, d) then
    ReadError(Format(SEJtdIllform8927TSFmt, [aNode.AsString]));
  FValue := d;
end;

procedure TJtdDateTimeUTC.DoReadJson(aReader: TJsonReader);
var
  d: TDateTime;
begin
  if aReader.TokenKind <> TTokenKind.tkString then
    ReadError(SEJtdExpectGotFmt, [CSJtdString, TOKEN_NAMES[aReader.TokenKind]], aReader);
  if not TryRfc8927TimeStampToUTC(aReader.AsString, d) then
    ReadError(Format(SEJtdIllform8927TSFmt, [aReader.AsString]), aReader);
  FValue := d;
end;

procedure TJtdDateTimeUTC.DoWriteJson(aWriter: TJsonStrWriter);
begin
  aWriter.Add(UTCToRfc8927TimeStamp(FValue));
end;

{ TJtdEnum }

procedure TJtdEnum.DoReadJson(aNode: TJsonNode);
var
  v: Integer;
begin
  if not aNode.IsString then
    ReadError(SEJtdExpectGotFmt, [CSJtdString, JKIND_NAMES[aNode.Kind]]);
  v := GetEnumValue(TypeInfo(TEnum), aNode.AsString);
  if v < 0 then
    ReadError(Format(SEJtdIllegalEnumFmt, [aNode.AsString]));
  FValue := TEnum(v);
end;

procedure TJtdEnum.DoReadJson(aReader: TJsonReader);
var
  v: Integer;
begin
  if aReader.TokenKind <> TTokenKind.tkString then
    ReadError(SEJtdExpectGotFmt, [CSJtdString, TOKEN_NAMES[aReader.TokenKind]], aReader);
  v := GetEnumValue(TypeInfo(TEnum), aReader.AsString);
  if v < 0 then
    ReadError(Format(SEJtdIllegalEnumFmt, [aReader.AsString]), aReader);
  FValue := TEnum(v);
end;

procedure TJtdEnum.DoWriteJson(aWriter: TJsonStrWriter);
begin
  aWriter.Add(GetEnumName(TypeInfo(TEnum), Integer(FValue)));
end;

{ TJtdStrEnum }

procedure TJtdStrEnum.SetValue(const aValue: string);
begin
  if not IsElement(aValue) then
    Error(Format(SEStrNotEnumElemFmt, [aValue, ClassName]));
  FValue := aValue;
end;

procedure TJtdStrEnum.DoReadJson(aNode: TJsonNode);
begin
  if not aNode.IsString then
    ReadError(SEJtdExpectGotFmt, [CSJtdString, JKIND_NAMES[aNode.Kind]]);
  if not IsElement(aNode.AsString) then
    ReadError(Format(SEStrNotEnumElemFmt, [aNode.AsString, ClassName]));
  FValue := aNode.AsString;
end;

procedure TJtdStrEnum.DoReadJson(aReader: TJsonReader);
begin
  if aReader.TokenKind <> TTokenKind.tkString then
    ReadError(SEJtdExpectGotFmt, [CSJtdString, TOKEN_NAMES[aReader.TokenKind]], aReader);
  if not IsElement(aReader.AsString) then
    ReadError(Format(SEStrNotEnumElemFmt, [aReader.AsString, ClassName]), aReader);
  FValue := aReader.AsString;
end;

procedure TJtdStrEnum.DoWriteJson(aWriter: TJsonStrWriter);
begin
  aWriter.Add(FValue);
end;

constructor TJtdStrEnum.Create(const aValue: string);
begin
  if not IsElement(aValue) then Error(Format(SEStrNotEnumElemFmt, [aValue, ClassName]));
  inherited Create(aValue);
end;

{ TJtdGenContainer }

destructor TJtdGenContainer.Destroy;
begin
  Clear;
  inherited Destroy;
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

procedure TJtdList.DoReadJson(aNode: TJsonNode);
var
  n: TJsonNode;
begin
  if not aNode.IsArray then
    ReadError(SEJtdExpectGotFmt, [CSJtdArray, JKIND_NAMES[aNode.Kind]]);
  Clear;
  for n in aNode do
    FList.Add(T(T.LoadInstance(n)));
end;

procedure TJtdList.DoReadJson(aReader: TJsonReader);
begin
  if aReader.TokenKind <> tkArrayBegin then
    ReadError(SEJtdExpectGotFmt, [CSJtdArray, TOKEN_NAMES[aReader.TokenKind]], aReader);
  Clear;
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

function TJtdList.GetEnumerator: TEnumerator;
begin
  Result := FList.GetEnumerator;
end;

procedure TJtdList.Clear;
var
  I: SizeInt;
begin
  if FList.IsEmpty then exit;
  for I := 0 to Pred(FList.Count) do
    FList.UncMutable[I]^.Free;
  FList.MakeEmpty;
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

{ TJtdMap }

function TJtdMap.GetCount: SizeInt;
begin
  Result := FMap.Count;
end;

function TJtdMap.GetItem(const aKey: string): T;
begin
  if not FMap.TryGetValue(aKey, Result) then Result := nil;
end;

procedure TJtdMap.DoReadJson(aNode: TJsonNode);
var
  e: TJsonNode.TPair;
  p: TMap.PValue;
begin
  if not aNode.IsObject then ExpectObject(aNode);
  Clear;
  for e in aNode.Entries do begin
    p := FMap.GetMutValueDef(e.Key, nil);
    if p^ <> nil then
      ReadError(Format(SEJtdDupValuesKeyFmt, [e.Key]));
    p^ := T(T.LoadInstance(e.Value));
  end;
end;

procedure TJtdMap.DoReadJson(aReader: TJsonReader);
var
  p: TMap.PValue;
begin
  if aReader.TokenKind <> tkObjectBegin then ExpectObject(aReader);
  Clear;
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

procedure TJtdMap.Clear;
var
  v: T;
begin
  if FMap.IsEmpty then exit;
  for v in FMap.Values do
    v.Free;
  FMap.MakeEmpty;
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

{ TJtdObject }

class procedure TJtdObject.PropNotFound(const aJsonPropName: string; aReader: TJsonReader);
begin
  ReadError(SERequiredJPropNotFoundFmt, [ClassName, aJsonPropName], aReader);
end;

class procedure TJtdObject.PropNotFound(const aJsonPropName: string);
begin
  ReadError(SERequiredJPropNotFoundFmt, [ClassName, aJsonPropName]);
end;

class procedure TJtdObject.UnknownProp(const aJsonPropName: string; aReader: TJsonReader);
begin
  ReadError(SEUnknownPropFoundFmt, [ClassName, aJsonPropName], aReader);
end;

class procedure TJtdObject.UnknownProp(const aJsonPropName: string);
begin
  ReadError(SEUnknownPropFoundFmt, [ClassName, aJsonPropName]);
end;

class procedure TJtdObject.DuplicateProp(aReader: TJsonReader);
begin
  ReadError(SEJtdDupPropNameFmt, [aReader.Name], aReader);
end;

class procedure TJtdObject.DuplicateProp(const aProp: string);
begin
  ReadError(SEJtdDupPropNameFmt, [aProp]);
end;

{ TJtdUnion }

procedure TJtdUnion.DoReadJson(aNode: TJsonNode);
var
  Node, TagNode: TJsonNode;
  TagName: string;
  InstClass: TJtdEntityClass;
begin
  if not aNode.IsObject then ExpectObject(aNode);
  TagName := GetTagJsonName;
  if not aNode.Contains(TagName) then
    ReadError(Format(SEJtdTagNotFoundFmt, [TagName]));
  Node := aNode.Clone;
  try
    Node.Find(TagName, TagNode);
    if not TagNode.IsString then ReadError(SEJtdDiscriTagNotStr);
    InstClass := GetInstanceClass(TagNode.AsString);
    if InstClass = nil then
      ReadError(Format(SEJtdIllegalTagValueFmt, [TagNode.AsString]));
    FTag := TagNode.AsString;
    if not Node.Remove(TagName) then InternalError(2);
    FreeAndNil(FInstance);
    FInstance := InstClass.LoadInstance(Node);
  finally
    Node.Free;
  end;
end;

procedure TJtdUnion.DoReadJson(aReader: TJsonReader);
var
  s, TagName, TagValue: string;
  Node, TagNode: TJsonNode;
  InstClass: TJtdEntityClass;
begin
  if aReader.TokenKind <> tkObjectBegin then ExpectObject(aReader);
  if not aReader.CopyStruct(s) then ReaderFail(aReader);
  if not TJsonNode.TryParse(s, Node) then InternalError(3);
  try
    TagName := GetTagJsonName;
    if not Node.Find(TagName, TagNode) then
      ReadError(Format(SEJtdTagNotFoundFmt, [TagName]));
    if not TagNode.IsString then ReadError(SEJtdDiscriTagNotStr);
    TagValue := TagNode.AsString;
    InstClass := GetInstanceClass(TagValue);
    if InstClass = nil then
      ReadError(Format(SEJtdIllegalTagValueFmt, [TagValue]));
    if not Node.Remove(TagName) then InternalError(4);
    FreeAndNil(FInstance);
    try
      FInstance := InstClass.LoadInstance(Node);
    except
      ReadError(SEJtdInvalidDiscriInst);
    end;
    FTag := TagValue;
  finally
    Node.Free;
  end;
end;

procedure TJtdUnion.DoWriteJson(aWriter: TJsonStrWriter);
var
  n: TJsonNode;
begin
  if not TJsonNode.TryParse(FInstance.AsJson, n) then InternalError(5);
  try
    if not n.IsObject then InternalError(6);
    n.Add(GetTagJsonName, Tag);
    aWriter.Add(n);
  finally
    n.Free;
  end;
end;

destructor TJtdUnion.Destroy;
begin
  FInstance.Free;
  inherited Destroy;
end;

function TJtdUnion.GetInstanceClass: TJtdEntityClass;
begin
  Result := GetInstanceClass(Tag);
end;

end.
