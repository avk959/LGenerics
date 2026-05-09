{****************************************************************************
*                                                                           *
*   This file is part of the LGenerics package.                             *
*   Some useful MessagePack utilites.                                       *
*                                                                           *
*   Copyright(c) 2026 A.Koverdyaev(avk)                                     *
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
unit lgMsgPackUtils;

{$MODE OBJFPC}{$H+}
{$MODESWITCH ADVANCEDRECORDS}
{$MODESWITCH NESTEDPROCVARS}
{$INLINE ON}

interface

uses
  Classes, SysUtils, ZStream,
  LgUtils,
  LgHelpers,
  LgVector,
  LgHashMap,
  LgMsgPack,
  LgPdo;

type

  TMpUserExt = class;

  { TMpCustomExt }
  TMpCustomExt = class
  const
    DEF_DEPTH = 7;
  private
    FHolder: TMpUserExt;
    FExtType: TUserExtType;
  protected
    FWriter: TMpWriter;
    FReader: TMpReader;
    function  GetDataType: PTypeInfo; virtual; abstract;
    function  GetExtType: TUserExtType; inline;
  public
    constructor Create(aExtType: TUserExtType; aMaxDepth: Integer = DEF_DEPTH);
    destructor Destroy; override;
    procedure Write(aData: Pointer; aWriter: TMpCustomWriter); virtual; abstract;
    function  TryRead(aData: Pointer; const aBlob: TMpExtBlob): Boolean; virtual; abstract;
    property  DataType: PTypeInfo read GetDataType;
    property  ExtType: TUserExtType read GetExtType;
    property  Holder: TMpUserExt read FHolder write FHolder;
  end;

  { TMpExtHook }
  generic TMpExtHook<T> = class abstract(TMpCustomExt)
  type
    TExtValue = T;
    PExtValue = ^T;
  protected
    function GetDataType: PTypeInfo; override;
  end;

  { TMpUserExt }
  TMpUserExt = class(TObject, IMpUserExt)
  private
  type
    TMapType = specialize TGLiteChainHashMap<PTypeInfo, TUserExtType, Pointer>;
    TMap     = TMapType.TMap;
  private
    FTypeList: array[TUserExtType] of TMpCustomExt;
    FTypeMap: TMap;
    FWriteProc: TMpNestWriteProc;
    FReadProc: TMpNestReadProc;
    function  GetHookCount: Integer; inline;
  protected
    procedure SetNestWriteProc(aProc: TMpNestWriteProc);
    procedure SetNestReadProc(aProc: TMpNestReadProc);
  public
    generic function CanWrite<T>(const aValue: T; out aBlob: TMpExtBlob): Boolean;
    generic function CanRead<T>(const aBlob: TMpExtBlob; out aValue: T): Boolean;
    constructor Create(const a: array of TMpCustomExt); overload;
    destructor Destroy; override;
    function  TryAddHook(aHook: TMpCustomExt): Boolean;
    function  TryAddAll(const a: array of TMpCustomExt): Integer;
    function  TryRemoveHook(aExtType: TUserExtType): Boolean;
    function  TryRemoveHook(aDataType: PTypeInfo): Boolean;
    function  Supports(aExtType: TUserExtType): Boolean; inline;
    function  Supports(aExtType: TUserExtType; out aDataType: PTypeInfo): Boolean; inline;
    function  Supports(aDataType: PTypeInfo): Boolean; inline;
    function  Supports(aDataType: PTypeInfo; out aExtType: TUserExtType): Boolean; inline;
    procedure WriteValue(aTypeInfo: PTypeInfo; const aValue; aWriter: TMpCustomWriter);
    procedure ReadValue(aTypeInfo: PTypeInfo; var aValue; aReader: TMpCustomReader);
    function  CanWrite(aData: Pointer; aType: PTypeInfo; aWriter: TMpCustomWriter): Boolean;
    function  CanWrite(aData: Pointer; aType: PTypeInfo; out aBlob: TMpExtBlob): Boolean;
    function  CanRead(aData: Pointer; aType: PTypeInfo; aReader: TMpCustomReader): Boolean;
    function  CanRead(aData: Pointer; aType: PTypeInfo; const aBlob: TMpExtBlob): Boolean;
    property  HookCount: Integer read GetHookCount;
  end;

  { TMpGuidExt }
  TMpGuidExt = class(specialize TMpExtHook<TGuid>)
    procedure Write(aData: Pointer; aWriter: TMpCustomWriter); override;
    function  TryRead(aData: Pointer; const aBlob: TMpExtBlob): Boolean; override;
  end;

  { TGOptionalExt }
  generic TGOptionalExt<T> = class(specialize TMpExtHook<specialize TGOptional<T>>)
    procedure Write(aData: Pointer; aWriter: TMpCustomWriter); override;
    function  TryRead(aData: Pointer; const aBlob: TMpExtBlob): Boolean; override;
  end;

  { TGLiteVectorExt }
  generic TGLiteVectorExt<T> = class(specialize TMpExtHook<specialize TGLiteVector<T>>)
    procedure Write(aData: Pointer; aWriter: TMpCustomWriter); override;
    function  TryRead(aData: Pointer; const aBlob: TMpExtBlob): Boolean; override;
  end;

  { TGLiteStrHashMapExt }
  generic TGLiteStrHashMapExt<T> = class(
      specialize TMpExtHook<specialize TGLiteChainHashMap<string, T, string>.TMap>)
    procedure Write(aData: Pointer; aWriter: TMpCustomWriter); override;
    function  TryRead(aData: Pointer; const aBlob: TMpExtBlob): Boolean; override;
  end;

  { TGLiteIntHashMapExt }
  generic TGLiteIntHashMapExt<T> = class(
      specialize TMpExtHook<specialize TGLiteChainHashMap<Int64, T, Int64>.TMap>)
    procedure Write(aData: Pointer; aWriter: TMpCustomWriter); override;
    function  TryRead(aData: Pointer; const aBlob: TMpExtBlob): Boolean; override;
  end;

  { TGLiteChainHashMapExt }
  generic TGLiteChainHashMapExt<TKey, TValue, TKeyEqRel> = class(
      specialize TMpExtHook<specialize TGLiteChainHashMap<TKey, TValue, TKeyEqRel>.TMap>)
    procedure Write(aData: Pointer; aWriter: TMpCustomWriter); override;
    function  TryRead(aData: Pointer; const aBlob: TMpExtBlob): Boolean; override;
  end;

  { TMpTextCompressHook }
  generic TMpTextCompressHook<T> = class abstract(specialize TMpExtHook<T>)
  public
  type
    TZLibCompressLevel = ZStream.TCompressionLevel;
  private
    FLevel: TCompressionLevel;
  protected
  const
    ZLIB_MAGIC = Byte('z');
  public
    constructor Create(aExtType: TUserExtType; aCompressLevel: TZLibCompressLevel = clMax);
    property CompressLevel: TZLibCompressLevel read FLevel;
  end;

  { TMpText }
  TMpText = record
  private
    FText: string;
  public
    class operator := (const txt: TMpText): string; inline;
    class operator := (const s: string): TMpText; inline;
    class operator Explicit(const txt: TMpText): string; inline;
    class operator Explicit(const s: string): TMpText; inline;
  end;

  { TTextCompressExt }
  TTextCompressExt = class(specialize TMpTextCompressHook<TMpText>)
  private
  const
    SHORT_CUTOFF = 512;
    BUF_SIZE     = SHORT_CUTOFF + SHORT_CUTOFF div 2;
  var
    FBuffer: array[0..Pred(BUF_SIZE)] of Byte;
  public
    procedure Write(aData: Pointer; aWriter: TMpCustomWriter); override;
    function  TryRead(aData: Pointer; const aBlob: TMpExtBlob): Boolean; override;
  end;

  { TMpUri }
  TMpUri = record
  private
    FUri: string;
  public
    class operator := (const u: TMpUri): string; inline;
    class operator := (const u: string): TMpUri; inline;
    class operator Explicit(const u: TMpUri): string; inline;
    class operator Explicit(const u: string): TMpUri; inline;
  end;

  { TUriCompressExt }
  TUriCompressExt = class(specialize TMpTextCompressHook<TMpUri>)
  private
  const
    SHORT_CUTOFF = 256;
    BUF_SIZE     = SHORT_CUTOFF + SHORT_CUTOFF div 2;
  var
    FBuffer: array[0..Pred(BUF_SIZE)] of Byte;
  public
    procedure Write(aData: Pointer; aWriter: TMpCustomWriter); override;
    function  TryRead(aData: Pointer; const aBlob: TMpExtBlob): Boolean; override;
  end;

  { TMpJson }
  TMpJson = record
  private
    FJson: string;
  public
    class operator := (const j: TMpJson): string; inline;
    class operator := (const j: string): TMpJson; inline;
    class operator Explicit(const j: TMpJson): string; inline;
    class operator Explicit(const j: string): TMpJson; inline;
  end;

  { TJsonCompressExt }
  TJsonCompressExt = class(specialize TMpTextCompressHook<TMpJson>)
  private
  const
    SHORT_CUTOFF = 512;
    BUF_SIZE     = SHORT_CUTOFF + SHORT_CUTOFF div 2;
  var
    FBuffer: array[0..Pred(BUF_SIZE)] of Byte;
  public
    procedure Write(aData: Pointer; aWriter: TMpCustomWriter); override;
    function  TryRead(aData: Pointer; const aBlob: TMpExtBlob): Boolean; override;
  end;

  { TMpXml }
  TMpXml = record
  private
    FXml: string;
  public
    class operator := (const x: TMpXml): string; inline;
    class operator := (const x: string): TMpXml; inline;
    class operator Explicit(const x: TMpXml): string; inline;
    class operator Explicit(const x: string): TMpXml; inline;
  end;

  { TXmlCompressExt }
  TXmlCompressExt = class(specialize TMpTextCompressHook<TMpXml>)
  private
  const
    SHORT_CUTOFF = 512;
    BUF_SIZE     = SHORT_CUTOFF + SHORT_CUTOFF div 2;
  var
    FBuffer: array[0..Pred(BUF_SIZE)] of Byte;
  public
    procedure Write(aData: Pointer; aWriter: TMpCustomWriter); override;
    function  TryRead(aData: Pointer; const aBlob: TMpExtBlob): Boolean; override;
  end;

  { TMpHtml }
  TMpHtml = record
  private
    FHtml: string;
  public
    class operator := (const h: TMpHtml): string; inline;
    class operator := (const h: string): TMpHtml; inline;
    class operator Explicit(const h: TMpHtml): string; inline;
    class operator Explicit(const h: string): TMpHtml; inline;
  end;

  { THtmlCompressExt }
  THtmlCompressExt = class(specialize TMpTextCompressHook<TMpHtml>)
  private
  const
    SHORT_CUTOFF = 512;
    BUF_SIZE     = SHORT_CUTOFF + SHORT_CUTOFF div 2;
  var
    FBuffer: array[0..Pred(BUF_SIZE)] of Byte;
  public
    procedure Write(aData: Pointer; aWriter: TMpCustomWriter); override;
    function  TryRead(aData: Pointer; const aBlob: TMpExtBlob): Boolean; override;
  end;

  TRecFieldData = record
    Name: string;
    TypeRef: PTypeInfo;
    Offset: Integer;
    class function Make(const aName: string; aType: PTypeInfo; aOfs: Integer): TRecFieldData; static; inline;
  end;

  { TRecFieldMapExt }
  TRecFieldMapExt = class(TMpCustomExt)
  private
  var
    FMap: array of TRecFieldData;
    FDataType: PTypeInfo;
    function FindField(const aName: string): Integer;
  protected
    function GetDataType: PTypeInfo; override;
  public
    constructor Create(aTypeInfo: PTypeInfo; aExtType: TUserExtType; const aFieldList: array of string;
                       aMaxDepth: Integer = DEF_DEPTH);
    procedure Write(aData: Pointer; aWriter: TMpCustomWriter); override;
    function  TryRead(aData: Pointer; const aBlob: TMpExtBlob): Boolean; override;
  end;

  { TGRecFieldMapExt }
  generic TGRecFieldMapExt<T: record> = class
  const
    DEF_DEPTH = TMpCustomExt.DEF_DEPTH;
    class function Make(aExtType: TUserExtType; const aFieldList: array of string;
                        aMaxDepth: Integer = DEF_DEPTH): TRecFieldMapExt;
  end;

implementation
{$B-}{$COPERATORS ON}{$POINTERMATH ON}
uses
  Math, TypInfo, LgUnishox;

{ TMpCustomExt }

function TMpCustomExt.GetExtType: TUserExtType;
begin
  Result := FExtType;
end;

constructor TMpCustomExt.Create(aExtType: TUserExtType; aMaxDepth: Integer);
begin
  inherited Create;
  FExtType := aExtType;
  FWriter := TMpWriter.Create;
  if aMaxDepth > 0 then
    FReader := TMpReader.Create(aMaxDepth)
  else
    FReader := TMpReader.Create(DEF_DEPTH);
end;

destructor TMpCustomExt.Destroy;
begin
  FWriter.Free;
  FReader.Free;
  inherited;
end;

{ TMpExtHook }

function TMpExtHook.GetDataType: PTypeInfo;
begin
  Result := System.TypeInfo(TExtValue);
end;

{ TMpUserExt }

function TMpUserExt.GetHookCount: Integer;
begin
  Result := FTypeMap.Count;
end;

procedure TMpUserExt.SetNestWriteProc(aProc: TMpNestWriteProc);
begin
  FWriteProc := aProc;
end;

procedure TMpUserExt.SetNestReadProc(aProc: TMpNestReadProc);
begin
  FReadProc := aProc;
end;

generic function TMpUserExt.CanWrite<T>(const aValue: T; out aBlob: TMpExtBlob): Boolean;
begin
  Result := CanWrite(@aValue, TypeInfo(aValue), aBlob);
end;

generic function TMpUserExt.CanRead<T>(const aBlob: TMpExtBlob; out aValue: T): Boolean;
begin
  aValue := Default(T);
  Result := CanRead(@aValue, TypeInfo(aValue), aBlob);
end;

constructor TMpUserExt.Create(const a: array of TMpCustomExt);
var
  h: TMpCustomExt;
begin
  inherited Create;
  for h in a do
    if not TryAddHook(h) then h.Free;
end;

destructor TMpUserExt.Destroy;
var
  e: TUserExtType;
begin
  for e in FTypeMap.Values do FTypeList[e].Free;
  inherited;
end;

function TMpUserExt.TryAddHook(aHook: TMpCustomExt): Boolean;
begin
  if (FTypeList[aHook.ExtType] <> nil) or FTypeMap.Contains(aHook.DataType) then
    exit(False);
  FTypeList[aHook.ExtType] := aHook;
  FTypeMap.Add(aHook.DataType, aHook.ExtType);
  aHook.Holder := Self;
  Result := True;
end;

function TMpUserExt.TryAddAll(const a: array of TMpCustomExt): Integer;
var
  h: TMpCustomExt;
begin
  Result := HookCount;
  for h in a do
    if not TryAddHook(h) then h.Free;
  Result := HookCount - Result;
end;

function TMpUserExt.TryRemoveHook(aExtType: TUserExtType): Boolean;
begin
  if FTypeList[aExtType] = nil then exit(False);
  FTypeMap.Remove(FTypeList[aExtType].DataType);
  FreeAndNil(FTypeList[aExtType]);
  Result := True;
end;

function TMpUserExt.TryRemoveHook(aDataType: PTypeInfo): Boolean;
var
  e: TUserExtType;
begin
  if not FTypeMap.Extract(aDataType, e) then exit(False);
  FreeAndNil(FTypeList[e]);
  Result := True;
end;

function TMpUserExt.Supports(aExtType: TUserExtType): Boolean;
begin
  Result := FTypeList[aExtType] <> nil;
end;

function TMpUserExt.Supports(aExtType: TUserExtType; out aDataType: PTypeInfo): Boolean;
begin
  aDataType := nil;
  Result := FTypeList[aExtType] <> nil;
  if Result then aDataType := FTypeList[aExtType].DataType;
end;

function TMpUserExt.Supports(aDataType: PTypeInfo): Boolean;
begin
  Result := FTypeMap.Contains(aDataType);
end;

function TMpUserExt.Supports(aDataType: PTypeInfo; out aExtType: TUserExtType): Boolean;
begin
  Result := FTypeMap.TryGetValue(aDataType, aExtType);
end;

procedure TMpUserExt.WriteValue(aTypeInfo: PTypeInfo; const aValue; aWriter: TMpCustomWriter);
begin
  if FWriteProc <> nil then
    FWriteProc(aTypeInfo, aValue, aWriter)
  else
    Pdo2MsgPack(aTypeInfo, aValue, aWriter, Self);
end;

procedure TMpUserExt.ReadValue(aTypeInfo: PTypeInfo; var aValue; aReader: TMpCustomReader);
begin
  if FReadProc <> nil then
    FReadProc(aTypeInfo, aValue, aReader)
  else
    MsgPack2Pdo(aTypeInfo, aValue, aReader, Self);
end;

function TMpUserExt.CanWrite(aData: Pointer; aType: PTypeInfo; aWriter: TMpCustomWriter): Boolean;
var
  e: TUserExtType;
begin
  Result := FTypeMap.TryGetValue(aType, e);
  if Result then FTypeList[e].Write(aData, aWriter);
end;

function TMpUserExt.CanWrite(aData: Pointer; aType: PTypeInfo; out aBlob: TMpExtBlob): Boolean;
var
  Writer: TMpWriter;
  e: TUserExtType;
  b: TBytes;
begin
  aBlob := nil;
  if not FTypeMap.TryGetValue(aType, e) then exit(False);
  Writer := TMpWriter.Create;
  try
    FTypeList[e].Write(aData, Writer);
    b := Writer.ToBytes;
  finally
    Writer.Free;
  end;
  case b[0] of
    $d4..$d8: System.Delete(b, 0, 1);
    $c7:      System.Delete(b, 0, 2);
    $c8:      System.Delete(b, 0, 3);
  else        System.Delete(b, 0, 5);
  end;
  aBlob := b;
  Result := True;
end;

function TMpUserExt.CanRead(aData: Pointer; aType: PTypeInfo; aReader: TMpCustomReader): Boolean;
begin
  if aReader.TokenKind <> mtkExt then exit(False);
  Result := CanRead(aData, aType, aReader.AsExtention);
end;

function TMpUserExt.CanRead(aData: Pointer; aType: PTypeInfo; const aBlob: TMpExtBlob): Boolean;
var
  e: TUserExtType;
begin
  if not FTypeMap.TryGetValue(aType, e) or (TUserExtType(aBlob[0]) <> FTypeList[e].ExtType) then
    exit(False);
  Result := FTypeList[e].TryRead(aData, aBlob);
end;

{ TMpGuidExt }

procedure TMpGuidExt.Write(aData: Pointer; aWriter: TMpCustomWriter);
var
  p: PExtValue absolute aData;
  g: TGuid;
begin
  g.D1 := NToLE(p^.D1);
  g.D2 := NToLE(p^.D2);
  g.D3 := NToLE(p^.D3);
  g.D4 := p^.D4;
  aWriter.AddExt(ExtType, g, SizeOf(g));
end;

{$PUSH}{$WARN 5057 OFF}
function TMpGuidExt.TryRead(aData: Pointer; const aBlob: TMpExtBlob): Boolean;
var
  p: PExtValue absolute aData;
  g: TGuid;
begin
  if System.Length(aBlob) <> SizeOf(TGuid) + 1 then exit(False);
  System.Move(aBlob[1], g, SizeOf(TGuid));
  p^.D1 := LEToN(g.D1);
  p^.D2 := LEToN(g.D2);
  p^.D3 := LEToN(g.D3);
  p^.D4 := g.D4;
  Result := True;
end;
{$POP}

{ TGOptionalExt }

procedure TGOptionalExt.Write(aData: Pointer; aWriter: TMpCustomWriter);
var
  p: PExtValue absolute aData;
begin
  if p^.Assigned then begin
    FWriter.Reset;
    Holder.WriteValue(TypeInfo(T), p^.Value, FWriter);
    aWriter.AddExt(ExtType, FWriter.BufPtr^, FWriter.TotalWritten);
  end else
    aWriter.AddExt(ExtType, []);
end;

function TGOptionalExt.TryRead(aData: Pointer; const aBlob: TMpExtBlob): Boolean;
var
  p: PExtValue absolute aData;
  Value: T;
begin
  p^.Clear;
  if System.Length(aBlob) = 1 then exit(True);
  FReader.Reset(@aBlob[1], System.Length(aBlob)-1);
  Value := Default(T);
  try
    Holder.ReadValue(TypeInfo(T), Value, FReader);
  except
    exit(False);
  end;
  p^ := Value;
  Result := True;
end;

{ TGLiteVectorExt }

procedure TGLiteVectorExt.Write(aData: Pointer; aWriter: TMpCustomWriter);
var
  p: PExtValue absolute aData;
  Info: PTypeInfo;
  I: SizeInt;
begin
  Info := TypeInfo(T);
  FWriter.Reset;
  FWriter.BeginArray(p^.Count);
  for I := 0 to Pred(p^.Count) do
    Holder.WriteValue(Info, p^.UncMutable[I]^, FWriter);
  aWriter.AddExt(ExtType, FWriter.BufPtr^, FWriter.TotalWritten);
end;

function TGLiteVectorExt.TryRead(aData: Pointer; const aBlob: TMpExtBlob): Boolean;
var
  p: PExtValue absolute aData;
  Info: PTypeInfo;
  Value: T;
  I: SizeInt;
begin
  p^.MakeEmpty;
  Info := TypeInfo(T);
  FReader.Reset(@aBlob[1], System.Length(aBlob)-1);
  FReader.Read;
  if FReader.TokenKind <> mtkArrayBegin then exit(False);
  p^.EnsureCapacity(FReader.StructUnread);
  try
    for I := 0 to Pred(FReader.StructUnread) do begin
      Value := Default(T);
      FReader.Read;
      Holder.ReadValue(Info, Value, FReader);
      p^.Add(Value);
    end
  except
    exit(False);
  end;
  FReader.Read;
  if FReader.TokenKind <> mtkArrayEnd then exit(False);
  FReader.Read;
  if FReader.ReadState <> mrsEof then exit(False);
  Result := True;
end;

{ TGLiteStrHashMapExt }

procedure TGLiteStrHashMapExt.Write(aData: Pointer; aWriter: TMpCustomWriter);
var
  p: PExtValue absolute aData;
  Info: PTypeInfo;
  e: TExtValue.TEntry;
begin
  Info := TypeInfo(T);
  FWriter.Reset;
  FWriter.BeginMap(p^.Count);
  for e in p^ do begin
    FWriter.Add(e.Key);
    Holder.WriteValue(Info, e.Value, FWriter);
  end;
  aWriter.AddExt(ExtType, FWriter.BufPtr^, FWriter.TotalWritten);
end;

function TGLiteStrHashMapExt.TryRead(aData: Pointer; const aBlob: TMpExtBlob): Boolean;
var
  p: PExtValue absolute aData;
  Info: PTypeInfo;
  k: string;
  v: T;
  I: SizeInt;
begin
  p^.MakeEmpty;
  Info := TypeInfo(T);
  FReader.Reset(@aBlob[1], System.Length(aBlob)-1);
  FReader.Read;
  if FReader.TokenKind <> mtkMapBegin then exit(False);
  p^.EnsureCapacity(FReader.StructUnread);
  try
    for I := 0 to Pred(FReader.StructUnread) do begin
      FReader.Read;
      if FReader.KeyValue.Kind <> mvkStr then exit(False);
      k := FReader.KeyValue.AsString;
      v := Default(T);
      Holder.ReadValue(Info, v, FReader);
      p^.Add(k, v);
    end;
  except
    exit(False);
  end;
  FReader.Read;
  if FReader.TokenKind <> mtkMapEnd then exit(False);
  FReader.Read;
  if FReader.ReadState <> mrsEof then exit(False);
  Result := True;
end;

{ TGLiteIntHashMapExt }

procedure TGLiteIntHashMapExt.Write(aData: Pointer; aWriter: TMpCustomWriter);
var
  p: PExtValue absolute aData;
  Info: PTypeInfo;
  e: TExtValue.TEntry;
begin
  Info := TypeInfo(T);
  FWriter.Reset;
  FWriter.BeginMap(p^.Count);
  for e in p^ do begin
    FWriter.Add(e.Key);
    Holder.WriteValue(Info, e.Value, FWriter);
  end;
  aWriter.AddExt(ExtType, FWriter.BufPtr^, FWriter.TotalWritten);
end;

function TGLiteIntHashMapExt.TryRead(aData: Pointer; const aBlob: TMpExtBlob): Boolean;
var
  p: PExtValue absolute aData;
  Info: PTypeInfo;
  k: Int64;
  v: T;
  I: SizeInt;
begin
  p^.MakeEmpty;
  Info := TypeInfo(T);
  FReader.Reset(@aBlob[1], System.Length(aBlob)-1);
  FReader.Read;
  if FReader.TokenKind <> mtkMapBegin then exit(False);
  p^.EnsureCapacity(FReader.StructUnread);
  try
    for I := 0 to Pred(FReader.StructUnread) do begin
      FReader.Read;
      if FReader.KeyValue.Kind <> mvkInt then exit(False);
      k := FReader.KeyValue.AsInt;
      v := Default(T);
      Holder.ReadValue(Info, v, FReader);
      p^.Add(k, v);
    end;
  except
    exit(False);
  end;
  FReader.Read;
  if FReader.TokenKind <> mtkMapEnd then exit(False);
  FReader.Read;
  if FReader.ReadState <> mrsEof then exit(False);
  Result := True;
end;

{ TGLiteChainHashMapExt }

procedure TGLiteChainHashMapExt.Write(aData: Pointer; aWriter: TMpCustomWriter);
var
  p: PExtValue absolute aData;
  KeyInfo, ValInfo: PTypeInfo;
  e: TExtValue.TEntry;
begin
  KeyInfo := TypeInfo(TKey);
  ValInfo := TypeInfo(TValue);
  FWriter.Reset;
  FWriter.BeginArray(p^.Count*2);
  for e in p^ do begin
    Holder.WriteValue(KeyInfo, e.Key, FWriter);
    Holder.WriteValue(ValInfo, e.Value, FWriter);
  end;
  aWriter.AddExt(ExtType, FWriter.BufPtr^, FWriter.TotalWritten);
end;

function TGLiteChainHashMapExt.TryRead(aData: Pointer; const aBlob: TMpExtBlob): Boolean;
var
  p: PExtValue absolute aData;
  KeyInfo, ValInfo: PTypeInfo;
  k: TKey;
  v: TValue;
  I, Cap: SizeInt;
begin
  p^.MakeEmpty;
  KeyInfo := TypeInfo(TKey);
  ValInfo := TypeInfo(TValue);
  FReader.Reset(@aBlob[1], System.Length(aBlob)-1);
  FReader.Read;
  if FReader.TokenKind <> mtkArrayBegin then exit(False);
  Cap := FReader.StructUnread div 2;
  p^.EnsureCapacity(Cap);
  try
    for I := 0 to Pred(Cap) do begin
      FReader.Read;
      k := Default(TKey);
      Holder.ReadValue(KeyInfo, k, FReader);
      FReader.Read;
      v := Default(TValue);
      Holder.ReadValue(ValInfo, v, FReader);
      p^.Add(k, v);
    end;
  except
    exit(False);
  end;
  FReader.Read;
  if FReader.TokenKind <> mtkArrayEnd then exit(False);
  FReader.Read;
  if FReader.ReadState <> mrsEof then exit(False);
  Result := True;
end;

{ TMpTextCompressHook }

constructor TMpTextCompressHook.Create(aExtType: TUserExtType; aCompressLevel: TZLibCompressLevel);
begin
  inherited Create(aExtType);
  FLevel := aCompressLevel;
end;

{ TSimpleText }

class operator TMpText.:=(const txt: TMpText): string;
begin
  Result := txt.FText;
end;

class operator TMpText.:=(const s: string): TMpText;
begin
  Result.FText := s;
end;

class operator TMpText.Explicit(const txt: TMpText): string;
begin
  Result := txt.FText;
end;

class operator TMpText.Explicit(const s: string): TMpText;
begin
  Result.FText := s;
end;

{ TTextCompressExt }

{$PUSH}{$WARN 5089 OFF : Local variable "$1" of a managed type does not seem to be initialized}
procedure TTextCompressExt.Write(aData: Pointer; aWriter: TMpCustomWriter);
var
  p: PExtValue absolute aData;
  s: string;
  ms: specialize TGAutoRef<TMemoryStream>;
  cs: specialize TGUniqRef<TCompressionStream>;
  Len: Integer;
  Magic: Byte = ZLIB_MAGIC;
begin
  s := p^;
  if s = '' then
    aWriter.AddExt(ExtType, Pointer(s)^, System.Length(s))
  else
    if System.Length(s) <= SHORT_CUTOFF then begin
      Len := unishox2_compress_preset(Pointer(s), System.Length(s), @FBuffer, SizeOf(FBuffer), upsFavorAlpha);
      aWriter.AddExt(ExtType, FBuffer, Len);
    end else begin
      ms.Instance.WriteBuffer(Magic, SizeOf(Magic));
      cs.Instance := TCompressionStream.Create(CompressLevel, ms.Instance, True);
      cs.Instance.WriteBuffer(Pointer(s)^, System.Length(s));
      cs.Clear;
      aWriter.AddExt(ExtType, ms.Instance.Memory^, ms.Instance.Size);
    end;
end;

function TTextCompressExt.TryRead(aData: Pointer; const aBlob: TMpExtBlob): Boolean;
var
  p: PExtValue absolute aData;
  s: string;
  ms, rs: specialize TGAutoRef<TMemoryStream>;
  dcs: specialize TGUniqRef<TDecompressionStream>;
  Len: Integer;
begin
  if System.Length(aBlob) > 1 then
    if (System.Length(aBlob) > 2) and (aBlob[1] = ZLIB_MAGIC) then begin
      ms.Instance.WriteBuffer(aBlob[2], System.Length(aBlob)-2);
      ms.Instance.Position := 0;
      dcs.Instance := TDecompressionStream.Create(ms.Instance, True);
      rs.Instance.CopyFrom(dcs.Instance, 0);
      System.SetLength(s, rs.Instance.Size);
      System.Move(rs.Instance.Memory^, Pointer(s)^, rs.Instance.Size);
    end else begin
      Len := unishox2_decompress_preset(@aBlob[1], System.Length(aBlob)-1, @FBuffer, SizeOf(FBuffer), upsFavorAlpha);
      if Len > SizeOf(FBuffer) then exit(False);
      System.SetLength(s, Len);
      System.Move(FBuffer[0], Pointer(s)^, Len);
    end;
  p^ := s;
  Result := True;
end;
{$POP}

{ TMpUri }

class operator TMpUri.:=(const u: TMpUri): string;
begin
  Result := u.FUri;
end;

class operator TMpUri.:=(const u: string): TMpUri;
begin
  Result.FUri := u;
end;

class operator TMpUri.Explicit(const u: TMpUri): string;
begin
  Result := u.FUri;
end;

class operator TMpUri.Explicit(const u: string): TMpUri;
begin
  Result.FUri := u;
end;

{ TUriCompressExt }

{$PUSH}{$WARN 5089 OFF : Local variable "$1" of a managed type does not seem to be initialized}
procedure TUriCompressExt.Write(aData: Pointer; aWriter: TMpCustomWriter);
var
  p: PExtValue absolute aData;
  s: string;
  ms: specialize TGAutoRef<TMemoryStream>;
  cs: specialize TGUniqRef<TCompressionStream>;
  Len: Integer;
  Magic: Byte = ZLIB_MAGIC;
begin
  s := p^;
  if s = '' then
    aWriter.AddExt(ExtType, Pointer(s)^, System.Length(s))
  else
    if System.Length(s) <= SHORT_CUTOFF then begin
      Len := unishox2_compress_preset(Pointer(s), System.Length(s), @FBuffer, SizeOf(FBuffer), upsUrl);
      aWriter.AddExt(ExtType, FBuffer, Len);
    end else begin
      ms.Instance.WriteBuffer(Magic, SizeOf(Magic));
      cs.Instance := TCompressionStream.Create(CompressLevel, ms.Instance, True);
      cs.Instance.WriteBuffer(Pointer(s)^, System.Length(s));
      cs.Clear;
      aWriter.AddExt(ExtType, ms.Instance.Memory^, ms.Instance.Size);
    end;
end;

function TUriCompressExt.TryRead(aData: Pointer; const aBlob: TMpExtBlob): Boolean;
var
  p: PExtValue absolute aData;
  s: string;
  ms, rs: specialize TGAutoRef<TMemoryStream>;
  dcs: specialize TGUniqRef<TDecompressionStream>;
  Len: Integer;
begin
  if System.Length(aBlob) > 1 then
    if (System.Length(aBlob) > 2) and (aBlob[1] = ZLIB_MAGIC) then begin
      ms.Instance.WriteBuffer(aBlob[2], System.Length(aBlob)-2);
      ms.Instance.Position := 0;
      dcs.Instance := TDecompressionStream.Create(ms.Instance, True);
      rs.Instance.CopyFrom(dcs.Instance, 0);
      System.SetLength(s, rs.Instance.Size);
      System.Move(rs.Instance.Memory^, Pointer(s)^, rs.Instance.Size);
    end else begin
      Len := unishox2_decompress_preset(@aBlob[1], System.Length(aBlob)-1, @FBuffer, SizeOf(FBuffer), upsUrl);
      if Len > SizeOf(FBuffer) then exit(False);
      System.SetLength(s, Len);
      System.Move(FBuffer[0], Pointer(s)^, Len);
    end;
  p^ := s;
  Result := True;
end;
{$POP}

{ TMpJson }

class operator TMpJson.:=(const j: TMpJson): string;
begin
  Result := j.FJson;
end;

class operator TMpJson.:=(const j: string): TMpJson;
begin
  Result.FJson := j;
end;

class operator TMpJson.Explicit(const j: TMpJson): string;
begin
  Result := j.FJson;
end;

class operator TMpJson.Explicit(const j: string): TMpJson;
begin
  Result.FJson := j;
end;

{ TJsonCompressExt }

{$PUSH}{$WARN 5089 OFF : Local variable "$1" of a managed type does not seem to be initialized}
procedure TJsonCompressExt.Write(aData: Pointer; aWriter: TMpCustomWriter);
var
  p: PExtValue absolute aData;
  s: string;
  ms: specialize TGAutoRef<TMemoryStream>;
  cs: specialize TGUniqRef<TCompressionStream>;
  Len: Integer;
  Magic: Byte = ZLIB_MAGIC;
begin
  s := p^;
  if s = '' then
    aWriter.AddExt(ExtType, Pointer(s)^, System.Length(s))
  else
    if System.Length(s) <= SHORT_CUTOFF then begin
      Len := unishox2_compress_preset(Pointer(s), System.Length(s), @FBuffer, SizeOf(FBuffer), upsJson);
      aWriter.AddExt(ExtType, FBuffer, Len);
    end else begin
      ms.Instance.WriteBuffer(Magic, SizeOf(Magic));
      cs.Instance := TCompressionStream.Create(CompressLevel, ms.Instance, True);
      cs.Instance.WriteBuffer(Pointer(s)^, System.Length(s));
      cs.Clear;
      aWriter.AddExt(ExtType, ms.Instance.Memory^, ms.Instance.Size);
    end;
end;

function TJsonCompressExt.TryRead(aData: Pointer; const aBlob: TMpExtBlob): Boolean;
var
  p: PExtValue absolute aData;
  s: string;
  ms, rs: specialize TGAutoRef<TMemoryStream>;
  dcs: specialize TGUniqRef<TDecompressionStream>;
  Len: Integer;
begin
  if System.Length(aBlob) > 1 then
    if (System.Length(aBlob) > 2) and (aBlob[1] = ZLIB_MAGIC) then begin
      ms.Instance.WriteBuffer(aBlob[2], System.Length(aBlob)-2);
      ms.Instance.Position := 0;
      dcs.Instance := TDecompressionStream.Create(ms.Instance, True);
      rs.Instance.CopyFrom(dcs.Instance, 0);
      System.SetLength(s, rs.Instance.Size);
      System.Move(rs.Instance.Memory^, Pointer(s)^, rs.Instance.Size);
    end else begin
      Len := unishox2_decompress_preset(@aBlob[1], System.Length(aBlob)-1, @FBuffer, SizeOf(FBuffer), upsJson);
      if Len > SizeOf(FBuffer) then exit(False);
      System.SetLength(s, Len);
      System.Move(FBuffer[0], Pointer(s)^, Len);
    end;
  p^ := s;
  Result := True;
end;
{$POP}

{ TMpXml }

class operator TMpXml.:=(const x: TMpXml): string;
begin
  Result := x.FXml;
end;

class operator TMpXml.:=(const x: string): TMpXml;
begin
  Result.FXml := x;
end;

class operator TMpXml.Explicit(const x: TMpXml): string;
begin
  Result := x.FXml;
end;

class operator TMpXml.Explicit(const x: string): TMpXml;
begin
  Result.FXml := x;
end;

{ TXmlCompressExt }

{$PUSH}{$WARN 5089 OFF : Local variable "$1" of a managed type does not seem to be initialized}
procedure TXmlCompressExt.Write(aData: Pointer; aWriter: TMpCustomWriter);
var
  p: PExtValue absolute aData;
  s: string;
  ms: specialize TGAutoRef<TMemoryStream>;
  cs: specialize TGUniqRef<TCompressionStream>;
  Len: Integer;
  Magic: Byte = ZLIB_MAGIC;
begin
  s := p^;
  if s = '' then
    aWriter.AddExt(ExtType, Pointer(s)^, System.Length(s))
  else
    if System.Length(s) <= SHORT_CUTOFF then begin
      Len := unishox2_compress_preset(Pointer(s), System.Length(s), @FBuffer, SizeOf(FBuffer), upsXml);
      aWriter.AddExt(ExtType, FBuffer, Len);
    end else begin
      ms.Instance.WriteBuffer(Magic, SizeOf(Magic));
      cs.Instance := TCompressionStream.Create(CompressLevel, ms.Instance, True);
      cs.Instance.WriteBuffer(Pointer(s)^, System.Length(s));
      cs.Clear;
      aWriter.AddExt(ExtType, ms.Instance.Memory^, ms.Instance.Size);
    end;
end;

function TXmlCompressExt.TryRead(aData: Pointer; const aBlob: TMpExtBlob): Boolean;
var
  p: PExtValue absolute aData;
  s: string;
  ms, rs: specialize TGAutoRef<TMemoryStream>;
  dcs: specialize TGUniqRef<TDecompressionStream>;
  Len: Integer;
begin
  if System.Length(aBlob) > 1 then
    if (System.Length(aBlob) > 2) and (aBlob[1] = ZLIB_MAGIC) then begin
      ms.Instance.WriteBuffer(aBlob[2], System.Length(aBlob)-2);
      ms.Instance.Position := 0;
      dcs.Instance := TDecompressionStream.Create(ms.Instance, True);
      rs.Instance.CopyFrom(dcs.Instance, 0);
      System.SetLength(s, rs.Instance.Size);
      System.Move(rs.Instance.Memory^, Pointer(s)^, rs.Instance.Size);
    end else begin
      Len := unishox2_decompress_preset(@aBlob[1], System.Length(aBlob)-1, @FBuffer, SizeOf(FBuffer), upsXml);
      if Len > SizeOf(FBuffer) then exit(False);
      System.SetLength(s, Len);
      System.Move(FBuffer[0], Pointer(s)^, Len);
    end;
  p^ := s;
  Result := True;
end;
{$POP}

{ TMpHtml }

class operator TMpHtml.:=(const h: TMpHtml): string;
begin
  Result := h.FHtml;
end;

class operator TMpHtml.:=(const h: string): TMpHtml;
begin
  Result.FHtml := h;
end;

class operator TMpHtml.Explicit(const h: TMpHtml): string;
begin
  Result := h.FHtml;
end;

class operator TMpHtml.Explicit(const h: string): TMpHtml;
begin
  Result.FHtml := h;
end;

{ THtmlCompressExt }

{$PUSH}{$WARN 5089 OFF : Local variable "$1" of a managed type does not seem to be initialized}
procedure THtmlCompressExt.Write(aData: Pointer; aWriter: TMpCustomWriter);
var
  p: PExtValue absolute aData;
  s: string;
  ms: specialize TGAutoRef<TMemoryStream>;
  cs: specialize TGUniqRef<TCompressionStream>;
  Len: Integer;
  Magic: Byte = ZLIB_MAGIC;
begin
  s := p^;
  if s = '' then
    aWriter.AddExt(ExtType, Pointer(s)^, System.Length(s))
  else
    if System.Length(s) <= SHORT_CUTOFF then begin
      Len := unishox2_compress_preset(Pointer(s), System.Length(s), @FBuffer, BUF_SIZE, upsHtml);
      aWriter.AddExt(ExtType, FBuffer, Len);
    end else begin
      ms.Instance.WriteBuffer(Magic, SizeOf(Magic));
      cs.Instance := TCompressionStream.Create(CompressLevel, ms.Instance, True);
      cs.Instance.WriteBuffer(Pointer(s)^, System.Length(s));
      cs.Clear;
      aWriter.AddExt(ExtType, ms.Instance.Memory^, ms.Instance.Size);
    end;
end;

function THtmlCompressExt.TryRead(aData: Pointer; const aBlob: TMpExtBlob): Boolean;
var
  p: PExtValue absolute aData;
  s: string;
  ms, rs: specialize TGAutoRef<TMemoryStream>;
  dcs: specialize TGUniqRef<TDecompressionStream>;
  Len: Integer;
begin
  if System.Length(aBlob) > 1 then
    if (System.Length(aBlob) > 2) and (aBlob[1] = ZLIB_MAGIC) then begin
      ms.Instance.WriteBuffer(aBlob[2], System.Length(aBlob)-2);
      ms.Instance.Position := 0;
      dcs.Instance := TDecompressionStream.Create(ms.Instance, True);
      rs.Instance.CopyFrom(dcs.Instance, 0);
      System.SetLength(s, rs.Instance.Size);
      System.Move(rs.Instance.Memory^, Pointer(s)^, rs.Instance.Size);
    end else begin
      Len := unishox2_decompress_preset(@aBlob[1], System.Length(aBlob)-1, @FBuffer, BUF_SIZE, upsHtml);
      if Len > SizeOf(FBuffer) then exit(False);
      System.SetLength(s, Len);
      System.Move(FBuffer[0], Pointer(s)^, Len);
    end;
  p^ := s;
  Result := True;
end;
{$POP}

{ TRecFieldData }

class function TRecFieldData.Make(const aName: string; aType: PTypeInfo; aOfs: Integer): TRecFieldData;
begin
  Result.Name := aName;
  Result.TypeRef := aType;
  Result.Offset := aOfs;
end;

{ TRecFieldMapExt }

function TRecFieldMapExt.FindField(const aName: string): Integer;
var
  I: Integer;
begin
  if aName <> '' then
    for I := 0 to System.High(FMap) do
      if FMap[I].Name = aName then exit(I);
  Result := -1;
end;

function TRecFieldMapExt.GetDataType: PTypeInfo;
begin
  Result := FDataType;
end;

constructor TRecFieldMapExt.Create(aTypeInfo: PTypeInfo; aExtType: TUserExtType;
  const aFieldList: array of string; aMaxDepth: Integer);
var
  pTypData: PTypeData;
  pField: PManagedField;
  I, Count: Integer;
begin
  inherited Create(aExtType, aMaxDepth);
  FDataType := aTypeInfo;
  if (aTypeInfo^.Kind <> tkRecord) or (System.Length(aFieldList) = 0) then exit;
  pTypData := GetTypeData(aTypeInfo);
  System.SetLength(FMap, Math.Min(pTypData^.TotalFieldCount, System.Length(aFieldList)));
  pField := PManagedField(AlignTypeData(PByte(@pTypData^.TotalFieldCount) + SizeOf(pTypData^.TotalFieldCount)));
  Count := 0;
  for I := 0 to System.High(FMap) do begin
    if aFieldList[I] <> '' then begin
      FMap[Count] := TRecFieldData.Make(aFieldList[I], pField^.TypeRef, pField^.FldOffset);
      Inc(Count);
    end;
    Inc(pField);
  end;
  System.SetLength(FMap, Count);
end;

procedure TRecFieldMapExt.Write(aData: Pointer; aWriter: TMpCustomWriter);
var
  I: Integer;
begin
  FWriter.Reset;
  FWriter.BeginMap(System.Length(FMap));
  for I := 0 to System.High(FMap) do begin
    FWriter.Add(FMap[I].Name);
    Holder.WriteValue(FMap[I].TypeRef, (pByte(aData) + FMap[I].Offset)^, FWriter);
  end;
  aWriter.AddExt(ExtType, FWriter.BufPtr^, FWriter.TotalWritten);
end;

function TRecFieldMapExt.TryRead(aData: Pointer; const aBlob: TMpExtBlob): Boolean;
var
  I, Cnt, J: SizeInt;
begin
  FReader.Reset(@aBlob[1], System.Length(aBlob)-1);
  try
    FReader.Read;
    if FReader.TokenKind <> mtkMapBegin then exit(False);
    Cnt := 0;
    for I := 0 to Pred(FReader.StructUnread) do begin
      FReader.Read;
      if FReader.KeyValue.Kind <> mvkStr then exit(False);
      J := FindField(FReader.KeyValue.AsString);
      if J = -1 then begin
        if FReader.TokenKind in MP_START_TOKENS then
          FReader.Skip;
        continue;
      end;
      Holder.ReadValue(FMap[J].TypeRef, (PByte(aData) + FMap[J].Offset)^, FReader);
      Inc(Cnt);
    end;
    if Cnt <> System.Length(FMap) then exit(False);// todo: ???
    FReader.Read;
    if FReader.TokenKind <> mtkMapEnd then exit(False);
    FReader.Read;
    if FReader.ReadState <> mrsEof then exit(False);
  except
    exit(False);
  end;
  Result := True;
end;

{ TGRecFieldMapExt }

class function TGRecFieldMapExt.Make(aExtType: TUserExtType; const aFieldList: array of string;
  aMaxDepth: Integer): TRecFieldMapExt;
begin
  Result := TRecFieldMapExt.Create(TypeInfo(T), aExtType, aFieldList, aMaxDepth);
end;

end.

