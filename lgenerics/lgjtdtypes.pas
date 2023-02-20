{****************************************************************************
*                                                                           *
*   This file is part of the LGenerics package.                             *
*   A set of classes handy when generating code from JTD schemas.           *                                            *
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
unit lgJtdTypes;

{$MODE OBJFPC}{$H+}

interface

uses
  Classes, SysUtils, lgUtils, lgVector, lgHashMap, lgJson;

const
  DEFAULT_DEPTH    = TJsonReader.DEF_DEPTH;
  DEFAULT_BUF_SIZE = TJsonReader.DEF_BUF_SIZE;

type

  EJtdReadJson = class(Exception);
  EJtdEntity   = class(Exception);

  TJtdEntity      = class;
  TJtdEntityClass = class of TJtdEntity;

  TJtdEntity = class abstract
  protected
    procedure DoReadJson(aNode: TJsonNode); virtual; abstract;
    procedure DoReadJson(aReader: TJsonReader); virtual; abstract;
    procedure DoWriteJson(aWriter: TJsonStrWriter); virtual; abstract;
    class procedure ReadError;
    class procedure Error(const aMessage: string);
  public
    class function GetJtdClass: TJtdEntityClass; virtual;
    class function ReadJson(aNode: TJsonNode): TJtdEntity;
    class function ReadJson(aReader: TJsonReader): TJtdEntity;
    class function ReadJson(const aJson: string): TJtdEntity;
    constructor Create; virtual;
    function  IsNull: Boolean; inline;
    function  NotNull: Boolean; inline;
    procedure LoadJson(aNode: TJsonNode);
    procedure LoadJson(const aJson: string);
    procedure LoadFromStream(aStream: TStream; aSkipBom: Boolean = False;
      aBufSize: SizeInt = DEFAULT_BUF_SIZE; aMaxDepth: SizeInt = DEFAULT_DEPTH);
    procedure LoadFromFile(const aFileName: string; aSkipBom: Boolean = False;
      aBufSize: SizeInt = DEFAULT_BUF_SIZE; aMaxDepth: SizeInt = DEFAULT_DEPTH);
    procedure WriteJson(aWriter: TJsonStrWriter);
    function  AsJson: string;
  end;

  { TJtdAny can contain arbitrary data, matches the empty schema }
  TJtdAny = class sealed(TJtdEntity)
  protected
    FInstance: TJsonNode;
    procedure DoReadJson(aNode: TJsonNode); override;
    procedure DoReadJson(aReader: TJsonReader); override;
    procedure DoWriteJson(aWriter: TJsonStrWriter); override;
  public
    class function GetJtdClass: TJtdEntityClass; override;
    constructor Create; override;
    destructor Destroy; override;
    property Instance: TJsonNode read FInstance;
  end;

  generic TJtdValue<T> = class abstract(TJtdEntity)
  protected
    FValue: T;
  public
    constructor Create(const aValue: T); reintroduce; virtual; overload;
    function OrElse(const aDefault: T): T; inline;
    property Value: T read FValue write FValue;
  end;

  { TJtdBool may contain boolean value }
  TJtdBool = class sealed(specialize TJtdValue<Boolean>)
  protected
    procedure DoReadJson(aNode: TJsonNode); override;
    procedure DoReadJson(aReader: TJsonReader); override;
    procedure DoWriteJson(aWriter: TJsonStrWriter); override;
  public
    class function GetJtdClass: TJtdEntityClass; override;
  end;

  { TJtdFloat32 may contain a Single value }
  TJtdFloat32 = class sealed(specialize TJtdValue<Single>)
  protected
    procedure DoReadJson(aNode: TJsonNode); override;
    procedure DoReadJson(aReader: TJsonReader); override;
    procedure DoWriteJson(aWriter: TJsonStrWriter); override;
  public
    class function GetJtdClass: TJtdEntityClass; override;
  end;

  { TJtdFloat64 may contain a Double value }
  TJtdFloat64 = class sealed(specialize TJtdValue<Double>)
  protected
    procedure DoReadJson(aNode: TJsonNode); override;
    procedure DoReadJson(aReader: TJsonReader); override;
    procedure DoWriteJson(aWriter: TJsonStrWriter); override;
  public
    class function GetJtdClass: TJtdEntityClass; override;
  end;

  generic TJtdInteger<T> = class abstract(specialize TJtdValue<T>)
  protected
    procedure DoReadJson(aNode: TJsonNode); override;
    procedure DoReadJson(aReader: TJsonReader); override;
    procedure DoWriteJson(aWriter: TJsonStrWriter); override;
    class function CanConvert(d: Double; out aValue: T): Boolean; static;
  end;

  { TJtdInt8 may contain a ShortInt(Int8) value }
  TJtdInt8 = class sealed(specialize TJtdInteger<Int8>)
    class function GetJtdClass: TJtdEntityClass; override;
  end;

  { TJtdUInt8 may contain a Byte(UInt8) value }
  TJtdUInt8 = class sealed(specialize TJtdInteger<UInt8>)
    class function GetJtdClass: TJtdEntityClass; override;
  end;

  { TJtdInt16 may contain a SmallInt(Int16) value }
  TJtdInt16 = class sealed(specialize TJtdInteger<Int16>)
    class function GetJtdClass: TJtdEntityClass; override;
  end;

  { TJtdUInt16 may contain a Word(UInt16) value }
  TJtdUInt16 = class sealed(specialize TJtdInteger<UInt16>)
    class function GetJtdClass: TJtdEntityClass; override;
  end;

  { TJtdInt32 may contain a LongInt(Int32) value }
  TJtdInt32 = class sealed(specialize TJtdInteger<Int32>)
    class function GetJtdClass: TJtdEntityClass; override;
  end;

  { TJtdUInt32 may contain a DWord(Cardinal, UInt32) value }
  TJtdUInt32 = class sealed(specialize TJtdInteger<UInt32>)
    class function GetJtdClass: TJtdEntityClass; override;
  end;

  { TJtdString may contain a String value }
  TJtdString = class sealed(specialize TJtdValue<string>)
  protected
    procedure DoReadJson(aNode: TJsonNode); override;
    procedure DoReadJson(aReader: TJsonReader); override;
    procedure DoWriteJson(aWriter: TJsonStrWriter); override;
  public
    class function GetJtdClass: TJtdEntityClass; override;
  end;

  { TJtdDateTimeUTC may contain a Rfc8927TimeStamp value }
  TJtdDateTimeUTC = class sealed(specialize TJtdValue<TDateTime>)
  protected
    procedure DoReadJson(aNode: TJsonNode); override;
    procedure DoReadJson(aReader: TJsonReader); override;
    procedure DoWriteJson(aWriter: TJsonStrWriter); override;
  public
    class function GetJtdClass: TJtdEntityClass; override;
  end;

  generic TJtdEnum<T> = class abstract(specialize TJtdValue<T>)
  protected
    procedure DoReadJson(aNode: TJsonNode); override;
    procedure DoReadJson(aReader: TJsonReader); override;
    procedure DoWriteJson(aWriter: TJsonStrWriter); override;
  end;

  TJtdStrEnum = class abstract(specialize TJtdValue<string>)
  protected
    procedure SetValue(const aValue: string);
    procedure DoReadJson(aNode: TJsonNode); override;
    procedure DoReadJson(aReader: TJsonReader); override;
    procedure DoWriteJson(aWriter: TJsonStrWriter); override;
  public
    class function IsEnumElement(const aValue: string): Boolean; virtual; abstract;
    constructor Create(const aValue: string); override; overload;
    property Value: string read FValue write SetValue;
  end;

  TJtdContainer = class(TJtdEntity)
  public
    destructor Destroy; override;
    procedure Clear; virtual; abstract;
  end;

  generic TJtdList<T: TJtdEntity> = class abstract(TJtdContainer)
  protected
  type
    TList = specialize TGLiteVector<T>;
  var
    FList: TList;
    function  GetCount: SizeInt; inline;
    function  GetItem(aIndex: SizeInt): T; inline;
    procedure SetItem(aIndex: SizeInt; aValue: T);
    procedure DoReadJson(aNode: TJsonNode); override;
    procedure DoReadJson(aReader: TJsonReader); override;
    procedure DoWriteJson(aWriter: TJsonStrWriter); override;
  public
  type
    TEnumerator = TList.TEnumerator;
    function  GetEnumerator: TEnumerator; inline;
    procedure Clear; override;
    function  Add(aValue: T): SizeInt;
    procedure Insert(aIndex: SizeInt; aValue: T);
    procedure Delete(aIndex: SizeInt); inline;
    property  Count: SizeInt read GetCount;
    property  Items[aIndex: SizeInt]: T read GetItem write SetItem; default;
  end;

  generic TJtdMap<T: TJtdEntity> = class abstract(TJtdContainer)
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
  protected
    FMap: TMap;
    function  GetCount: SizeInt; inline;
    function  GetItem(const aKey: string): T; inline;
    procedure DoReadJson(aNode: TJsonNode); override;
    procedure DoReadJson(aReader: TJsonReader); override;
    procedure DoWriteJson(aWriter: TJsonStrWriter); override;
  public
    function  GetEnumerator: TJtdMapEnumerator; inline;
    function  Keys: TJtdMapKeys; inline;
    function  Values: TJtdMapValues; inline;
    procedure Clear; override;
    function  Contains(const aKey: string): Boolean; inline;
    function  Add(const aKey: string; aValue: T): Boolean; inline;
    procedure AddOrSetValue(const aKey: string; aValue: T);
    function  TryGetValue(const aKey: string; out aValue: T): Boolean; inline;
    function  Remove(const aKey: string): Boolean;
    property  Items[const aKey: string]: T read GetItem write AddOrSetValue; default;
  end;

  TJtdObject = class abstract(TJtdContainer)
  protected
    procedure PropNotFound(const aJsonPropName: string);
    procedure UnknownProp(const aJsonPropName: string);
  end;

  TJtdVariant = class abstract(TJtdEntity)//todo: what about enumeration based one ???
  protected
    FTag: string;
    FInstance: TJtdEntity;
    procedure DoReadJson(aNode: TJsonNode); override;
    procedure DoReadJson(aReader: TJsonReader); override;
    procedure DoWriteJson(aWriter: TJsonStrWriter); override;
    class function GetTagJsonName: string; virtual; abstract;
    class function ValidTagValue(const aValue: string): Boolean; virtual; abstract;
    class function GetInstanceClass(const aTag: string): TJtdEntityClass; virtual; abstract;
  public
    destructor Destroy; override;
    property Tag: string read FTag;
    //
  end;

  resourcestring
    SEInternJtdVariantError = 'Internal TJtdVariant error';

implementation
{$B-}{$COPERATORS ON}{$POINTERMATH ON}

uses
  TypInfo, Math, lgJsonTypeDef, lgStrConst;

class procedure TJtdEntity.ReadError;
begin
  raise EJtdReadJson.CreateFmt(SEErrorWhenReadClassFmt, [GetJtdClass.ClassName]);
end;

class procedure TJtdEntity.Error(const aMessage: string);
begin
  raise EJtdEntity.Create(aMessage);
end;

class function TJtdEntity.GetJtdClass: TJtdEntityClass;
begin
  Result := TJtdEntity;
end;

constructor TJtdEntity.Create;
begin
  inherited;
end;

function TJtdEntity.IsNull: Boolean;
begin
  Result := Self = nil;
end;

function TJtdEntity.NotNull: Boolean;
begin
  Result := Self <> nil;
end;

procedure TJtdEntity.LoadJson(aNode: TJsonNode);
begin
  DoReadJson(aNode);
end;

procedure TJtdEntity.LoadJson(const aJson: string);
var
  Reader: TJsonReader;
begin
  Reader := TJsonReader.Create(Pointer(aJson), System.Length(aJson));
  try
    if not Reader.Read then ReadError;
    DoReadJson(Reader);
  finally
    Reader.Free;
  end;
end;

procedure TJtdEntity.LoadFromStream(aStream: TStream; aSkipBom: Boolean; aBufSize, aMaxDepth: SizeInt);
var
  Reader: TJsonReader;
begin
  Reader := TJsonReader.Create(aStream, aBufSize, aMaxDepth, aSkipBom);
  try
    if not Reader.Read then ReadError;
    DoReadJson(Reader);
  finally
    Reader.Free;
  end;
end;

procedure TJtdEntity.LoadFromFile(const aFileName: string; aSkipBom: Boolean; aBufSize, aMaxDepth: SizeInt);
var
  fs: TStream;
begin
  fs := TFileStream.Create(aFileName, fmOpenRead or fmShareDenyWrite);
  try
    LoadFromStream(fs, aSkipBom, aBufSize, aMaxDepth);
  finally
    fs.Free;
  end;
end;

class function TJtdEntity.ReadJson(aNode: TJsonNode): TJtdEntity;
begin
  if aNode.IsNull then exit(nil);
  Result := GetJtdClass.Create;
  Result.DoReadJson(aNode);
end;

class function TJtdEntity.ReadJson(aReader: TJsonReader): TJtdEntity;
begin
  if (aReader.ReadState = rsStart) and not aReader.Read then
    ReadError;
  if aReader.TokenKind = tkNull then exit(nil);
  Result := GetJtdClass.Create;
  Result.DoReadJson(aReader);
end;

class function TJtdEntity.ReadJson(const aJson: string): TJtdEntity;
var
  Reader: TJsonReader;
begin
  Reader := TJsonReader.Create(Pointer(aJson), System.Length(aJson));
  try
    Result := ReadJson(Reader);
  finally
    Reader.Free;
  end;
end;

procedure TJtdEntity.WriteJson(aWriter: TJsonStrWriter);
begin
  if Self = nil then
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

{ TJtdAny }

procedure TJtdAny.DoReadJson(aNode: TJsonNode);
begin
  Instance.CopyFrom(aNode);
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
    TJsonReader.TTokenKind.tkString:
      Instance.AsString := aReader.AsString;
    tkArrayBegin, tkObjectBegin:
      begin
        if not aReader.CopyStruct(s) then ReadError;
        if Instance.Parse(s) then ReadError;
      end;
  else
    ReadError;
  end;
end;

procedure TJtdAny.DoWriteJson(aWriter: TJsonStrWriter);
begin
  aWriter.Add(Instance);
end;

class function TJtdAny.GetJtdClass: TJtdEntityClass;
begin
  Result := TJtdAny;
end;

constructor TJtdAny.Create;
begin
  inherited;
  FInstance := TJsonNode.Create;
end;

destructor TJtdAny.Destroy;
begin
  FInstance.Free;
  inherited;
end;

{ TJtdValue }

constructor TJtdValue.Create(const aValue: T);
begin
  FValue := aValue;
end;

function TJtdValue.OrElse(const aDefault: T): T;
begin
  if IsNull then
    Result := aDefault
  else
    Result := FValue;
end;

{ TJtdBool }

procedure TJtdBool.DoReadJson(aNode: TJsonNode);
begin
  if not aNode.IsBoolean then ReadError;
  FValue := aNode.AsBoolean;
end;

procedure TJtdBool.DoReadJson(aReader: TJsonReader);
begin
  if not(aReader.TokenKind in [tkFalse, tkTrue]) then  ReadError;
  FValue := aReader.TokenKind = tkTrue;
end;

procedure TJtdBool.DoWriteJson(aWriter: TJsonStrWriter);
begin
  if FValue then
    aWriter.AddTrue
  else
    aWriter.AddFalse;
end;

class function TJtdBool.GetJtdClass: TJtdEntityClass;
begin
  Result := TJtdBool;
end;

{ TJtdFloat32 }

procedure TJtdFloat32.DoReadJson(aNode: TJsonNode);
var
  d: Double;
begin
  if not aNode.IsNumber then ReadError;
  d := aNode.AsNumber;
  if (d < Math.MinSingle) or (d > Math.MaxSingle) then ReadError;
  FValue := d;
end;

procedure TJtdFloat32.DoReadJson(aReader: TJsonReader);
var
  d: Double;
begin
  if aReader.TokenKind <> tkNumber then ReadError;
  d := aReader.AsNumber;
  if (d < Math.MinSingle) or (d > Math.MaxSingle) then ReadError;
  FValue := d;
end;

procedure TJtdFloat32.DoWriteJson(aWriter: TJsonStrWriter);
begin
  aWriter.Add(FValue);
end;

class function TJtdFloat32.GetJtdClass: TJtdEntityClass;
begin
  Result := TJtdFloat32;
end;

{ TJtdFloat64 }

procedure TJtdFloat64.DoReadJson(aNode: TJsonNode);
begin
  if not aNode.IsNumber then ReadError;
  FValue := aNode.AsNumber;
end;

procedure TJtdFloat64.DoReadJson(aReader: TJsonReader);
begin
  if aReader.TokenKind <> tkNumber then ReadError;
  FValue := aReader.AsNumber;
end;

procedure TJtdFloat64.DoWriteJson(aWriter: TJsonStrWriter);
begin
  aWriter.Add(FValue);
end;

class function TJtdFloat64.GetJtdClass: TJtdEntityClass;
begin
  Result := TJtdFloat64;
end;

{ TJtdInteger }

procedure TJtdInteger.DoReadJson(aNode: TJsonNode);
var
  v: T;
begin
  if not(aNode.IsNumber and CanConvert(aNode.AsNumber, v)) then ReadError;
  FValue := v;
end;

procedure TJtdInteger.DoReadJson(aReader: TJsonReader);
var
  v: T;
begin
  if not((aReader.TokenKind = tkNumber) and CanConvert(aReader.AsNumber, v)) then ReadError;
  FValue := v;
end;

procedure TJtdInteger.DoWriteJson(aWriter: TJsonStrWriter);
begin
  aWriter.Add(FValue);
end;

class function TJtdInteger.CanConvert(d: Double; out aValue: T): Boolean;
var
  I: Int64;
begin
  aValue := 0;
  if not IsExactInt(d, I) then exit(False);
  if (I < System.Low(T)) or (I > System.High(T)) then exit(False);
  aValue := T(I);
  Result := True;
end;

{ TJtdInt8 }

class function TJtdInt8.GetJtdClass: TJtdEntityClass;
begin
  Result := TJtdInt8;
end;

{ TJtdUInt8 }

class function TJtdUInt8.GetJtdClass: TJtdEntityClass;
begin
  Result := TJtdUInt8;
end;

{ TJtdInt16 }

class function TJtdInt16.GetJtdClass: TJtdEntityClass;
begin
  Result := TJtdInt16;
end;

{ TJtdUInt16 }

class function TJtdUInt16.GetJtdClass: TJtdEntityClass;
begin
  Result := TJtdUInt16;
end;

{ TJtdInt32 }

class function TJtdInt32.GetJtdClass: TJtdEntityClass;
begin
  Result := TJtdInt32;
end;

{ TJtdUInt32 }

class function TJtdUInt32.GetJtdClass: TJtdEntityClass;
begin
  Result := TJtdUInt32;
end;

{ TJtdString }

procedure TJtdString.DoReadJson(aNode: TJsonNode);
begin
  if not aNode.IsString then ReadError;
  FValue := aNode.AsString;
end;

procedure TJtdString.DoReadJson(aReader: TJsonReader);
begin
  if aReader.TokenKind <> TJsonReader.TTokenKind.tkString then ReadError;
  FValue := aReader.AsString;
end;

procedure TJtdString.DoWriteJson(aWriter: TJsonStrWriter);
begin
  aWriter.Add(FValue);
end;

class function TJtdString.GetJtdClass: TJtdEntityClass;
begin
  Result := TJtdString;
end;

{ TJtdDateTimeUTC }

procedure TJtdDateTimeUTC.DoReadJson(aNode: TJsonNode);
var
  d: TDateTime;
begin
  if not(aNode.IsString and TryRfc8927TimeStampToUTC(aNode.AsString, d)) then ReadError;
  FValue := d;
end;

procedure TJtdDateTimeUTC.DoReadJson(aReader: TJsonReader);
var
  d: TDateTime;
begin
  if not((aReader.TokenKind = TJsonReader.TTokenKind.tkString) and
          TryRfc8927TimeStampToUTC(aReader.AsString, d)) then ReadError;
  FValue := d;
end;

procedure TJtdDateTimeUTC.DoWriteJson(aWriter: TJsonStrWriter);
begin
  aWriter.Add(UTCToRfc8927TimeStamp(FValue));
end;

class function TJtdDateTimeUTC.GetJtdClass: TJtdEntityClass;
begin
  Result := TJtdDateTimeUTC;
end;

{ TJtdEnum }

procedure TJtdEnum.DoReadJson(aNode: TJsonNode);
var
  v: Integer;
begin
  if not aNode.IsString then ReadError;
  v := GetEnumValue(TypeInfo(T), aNode.AsString);
  if v < 0 then ReadError;
  FValue := T(v);
end;

procedure TJtdEnum.DoReadJson(aReader: TJsonReader);
var
  v: Integer;
begin
  if aReader.TokenKind <> aReader.TTokenKind.tkString then ReadError;
  v := GetEnumValue(TypeInfo(T), aReader.AsString);
  if v < 0 then ReadError;
  FValue := T(v);
end;

procedure TJtdEnum.DoWriteJson(aWriter: TJsonStrWriter);
begin
  aWriter.Add(GetEnumName(TypeInfo(T), Integer(FValue)));
end;

{ TJtdStrEnum }

procedure TJtdStrEnum.SetValue(const aValue: string);
begin
  if FValue = aValue then exit;
  if not IsEnumElement(aValue) then
    Error(Format(SEStrNotEnumElemFmt, [aValue, GetJtdClass.ClassName]));
  FValue := aValue;
end;

procedure TJtdStrEnum.DoReadJson(aNode: TJsonNode);
var
  s: string;
begin
  if not aNode.IsString then ReadError;
  s := aNode.AsString;
  if not IsEnumElement(s) then ReadError;
  FValue := s;
end;

procedure TJtdStrEnum.DoReadJson(aReader: TJsonReader);
var
  s: string;
begin
  if aReader.TokenKind <> aReader.TTokenKind.tkString then ReadError;
  s := aReader.AsString;
  if not IsEnumElement(s) then ReadError;
  FValue := s;
end;

procedure TJtdStrEnum.DoWriteJson(aWriter: TJsonStrWriter);
begin
  aWriter.Add(FValue);
end;

constructor TJtdStrEnum.Create(const aValue: string);
begin
  if not IsEnumElement(aValue) then Error(Format(SEStrNotEnumElemFmt, [aValue, ClassName]));
  inherited Create(aValue);
end;

{ TJtdContainer }

destructor TJtdContainer.Destroy;
begin
  Clear;
  inherited;
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
  I: SizeInt;
begin
  if not aNode.IsArray then ReadError;
  Clear;
  for I := 0 to Pred(aNode.Count) do
    FList.Add(T(T.ReadJson(aNode.Items[I])));
end;

procedure TJtdList.DoReadJson(aReader: TJsonReader);
begin
  if aReader.TokenKind <> tkArrayBegin then ReadError;
  Clear;
  repeat
    if not aReader.Read then ReadError;
    if aReader.TokenKind = tkArrayEnd then break;
    FList.Add(T(T.ReadJson(aReader)));
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
begin
  if not aNode.IsObject then ReadError;
  Clear;
  for e in aNode.Entries do
    FMap.Add(e.Key, T(T.ReadJson(e.Value)));
end;

procedure TJtdMap.DoReadJson(aReader: TJsonReader);
begin
  if aReader.TokenKind <> tkObjectBegin then ReadError;
  Clear;
  repeat
    if not aReader.Read then ReadError;
    if aReader.TokenKind = tkObjectEnd then break;
    FMap.Add(aReader.Name, T(T.ReadJson(aReader)));
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
  Result := FMap.Keys
end;

function TJtdMap.Values: TJtdMapValues;
begin
  Result := FMap.Values;
end;

procedure TJtdMap.Clear;
var
  v: T;
begin
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

procedure TJtdObject.PropNotFound(const aJsonPropName: string);
begin
  raise EJtdReadJson.CreateFmt(SERequiredJPropNotFoundFmt, [GetJtdClass.ClassName, aJsonPropName]);
end;

procedure TJtdObject.UnknownProp(const aJsonPropName: string);
begin
  raise EJtdReadJson.CreateFmt(SEUnknownPropFoundFmt, [GetJtdClass.ClassName, aJsonPropName])
end;

{ TJtdVariant }

procedure TJtdVariant.DoReadJson(aNode: TJsonNode);
var
  Node, TagNode: TJsonNode;
  TagName: string;
begin
  if not aNode.IsObject then ReadError;
  TagName := GetTagJsonName;
  if not aNode.Contains(TagName) then ReadError;
  Node := aNode.Clone;
  try
    Node.Find(TagName, TagNode);
    if not TagNode.IsString then ReadError;
    if not ValidTagValue(TagNode.AsString) then ReadError;
    FTag := TagNode.AsString;
    if not Node.Remove(TagName) then Error(SEInternJtdVariantError);
    FreeAndNil(FInstance);
    FInstance := GetInstanceClass(Tag).ReadJson(Node);
  finally
    Node.Free;
  end;
end;

procedure TJtdVariant.DoReadJson(aReader: TJsonReader);
var
  s: string;
  n: TJsonNode;
begin
  if aReader.TokenKind <> tkObjectBegin then ReadError;
  if not aReader.CopyStruct(s) then ReadError;
  if not TJsonNode.TryParse(s, n) then ReadError;
  try
    DoReadJson(n);
  finally
    n.Free;
  end;
end;

procedure TJtdVariant.DoWriteJson(aWriter: TJsonStrWriter);
var
  n: TJsonNode;
begin
  if not TJsonNode.TryParse(FInstance.AsJson, n) then Error(SEInternJtdVariantError);
  try
    if not n.IsObject then Error(SEInternJtdVariantError);
    n.Add(GetTagJsonName, Tag);
    aWriter.Add(n);
  finally
    n.Free;
  end;
end;

destructor TJtdVariant.Destroy;
begin
  FInstance.Free;
  inherited;
end;

end.
