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

  { TGuidExt }
  TGuidExt = class(specialize TMpExtHook<TGuid>)
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

  { TSimpleText }
  TSimpleText = record
  private
    FText: string;
  public
    class operator := (const txt: TSimpleText): string; inline;
    class operator := (const s: string): TSimpleText; inline;
    class operator Explicit(const txt: TSimpleText): string; inline;
    class operator Explicit(const s: string): TSimpleText; inline;
  end;

  { TTextCompressExt }
  TTextCompressExt = class(specialize TMpExtHook<TSimpleText>)
  private
    FCompressLevel: TCompressionLevel;
  public
    constructor Create(aExtType: TUserExtType; aLevel: TCompressionLevel = clFastest);
    procedure Write(aData: Pointer; aWriter: TMpCustomWriter); override;
    function  TryRead(aData: Pointer; const aBlob: TMpExtBlob): Boolean; override;
  end;

implementation
{$B-}{$COPERATORS ON}{$POINTERMATH ON}

{ TGuidExt }

procedure TGuidExt.Write(aData: Pointer; aWriter: TMpCustomWriter);
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
function TGuidExt.TryRead(aData: Pointer; const aBlob: TMpExtBlob): Boolean;
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
  Writer: TMpWriter;
begin
  if p^.Assigned then begin
    Writer := TMpWriter.Create;
    try
      Pdo2MsgPack(TypeInfo(T), p^.Value, Writer, Holder);
      aWriter.AddExt(ExtType, Writer.ToBytes);
    finally
      Writer.Free;
    end;
  end else
    aWriter.AddExt(ExtType, []);
end;

function TGOptionalExt.TryRead(aData: Pointer; const aBlob: TMpExtBlob): Boolean;
var
  p: PExtValue absolute aData;
  Value: T;
  Reader: TMpReader;
begin
  p^.Clear;
  if System.Length(aBlob) = 1 then exit(True);
  Reader := TMpReader.Create(@aBlob[1], System.Length(aBlob)-1, MaxDepth);
  try
    Value := Default(T);
    try
      MsgPack2Pdo(TypeInfo(T), Value, Reader, Holder);
    except
      exit(False);
    end;
  finally
    Reader.Free;
  end;
  p^ := Value;
  Result := True;
end;

{ TGLiteVectorExt }

procedure TGLiteVectorExt.Write(aData: Pointer; aWriter: TMpCustomWriter);
var
  p: PExtValue absolute aData;
  Info: PTypeInfo;
  Writer: TMpWriter;
  I: SizeInt;
begin
  Info := TypeInfo(T);
  Writer := TMpWriter.Create;
  try
    Writer.BeginArray(p^.Count);
    for I := 0 to Pred(p^.Count) do
      Pdo2MsgPack(Info, p^.UncMutable[I]^, Writer, Holder);
    aWriter.AddExt(ExtType, Writer.BufPtr^, Writer.TotalWritten);
  finally
    Writer.Free;
  end;
end;

function TGLiteVectorExt.TryRead(aData: Pointer; const aBlob: TMpExtBlob): Boolean;
var
  p: PExtValue absolute aData;
  Info: PTypeInfo;
  Value: T;
  Reader: TMpReader;
  I: SizeInt;
begin
  p^.MakeEmpty;
  Info := TypeInfo(T);
  Reader := TMpReader.Create(@aBlob[1], System.Length(aBlob)-1, MaxDepth);
  try
    Reader.Read;
    if Reader.TokenKind <> mtkArrayBegin then exit(False);
    p^.EnsureCapacity(Reader.StructUnread);
    try
      for I := 0 to Pred(Reader.StructUnread) do begin
        Value := Default(T);
        Reader.Read;
        MsgPack2Pdo(Info, Value, Reader, Holder);
        p^.Add(Value);
      end
    except
      exit(False);
    end;
    Reader.Read;
    if Reader.TokenKind <> mtkArrayEnd then exit(False);
    Reader.Read;
    if Reader.ReadState <> mrsEof then exit(False);
  finally
    Reader.Free;
  end;
  Result := True;
end;

{ TGLiteStrHashMapExt }

procedure TGLiteStrHashMapExt.Write(aData: Pointer; aWriter: TMpCustomWriter);
var
  p: PExtValue absolute aData;
  Info: PTypeInfo;
  e: TExtValue.TEntry;
  Writer: TMpWriter;
begin
  Info := TypeInfo(T);
  Writer := TMpWriter.Create;
  try
    Writer.BeginMap(p^.Count);
    for e in p^ do begin
      Writer.Add(e.Key);
      Pdo2MsgPack(Info, e.Value, Writer, Holder);
    end;
    aWriter.AddExt(ExtType, Writer.BufPtr^, Writer.TotalWritten);
  finally
    Writer.Free;
  end;
end;

function TGLiteStrHashMapExt.TryRead(aData: Pointer; const aBlob: TMpExtBlob): Boolean;
var
  p: PExtValue absolute aData;
  Info: PTypeInfo;
  k: string;
  v: T;
  Reader: TMpReader;
  I: SizeInt;
begin
  p^.MakeEmpty;
  Info := TypeInfo(T);
  Reader := TMpReader.Create(@aBlob[1], System.Length(aBlob)-1, MaxDepth);
  try
    Reader.Read;
    if Reader.TokenKind <> mtkMapBegin then exit(False);
    p^.EnsureCapacity(Reader.StructUnread);
    try
      for I := 0 to Pred(Reader.StructUnread) do begin
        Reader.Read;
        if Reader.KeyValue.Kind <> mvkStr then exit(False);
        k := Reader.KeyValue.AsString;
        v := Default(T);
        MsgPack2Pdo(Info, v, Reader, Holder);
        p^.Add(k, v);
      end;
    except
      exit(False);
    end;
    Reader.Read;
    if Reader.TokenKind <> mtkMapEnd then exit(False);
    Reader.Read;
    if Reader.ReadState <> mrsEof then exit(False);
  finally
    Reader.Free;
  end;
  Result := True;
end;

{ TGLiteIntHashMapExt }

procedure TGLiteIntHashMapExt.Write(aData: Pointer; aWriter: TMpCustomWriter);
var
  p: PExtValue absolute aData;
  Info: PTypeInfo;
  e: TExtValue.TEntry;
  Writer: TMpWriter;
begin
  Info := TypeInfo(T);
  Writer := TMpWriter.Create;
  try
    Writer.BeginMap(p^.Count);
    for e in p^ do begin
      Writer.Add(e.Key);
      Pdo2MsgPack(Info, e.Value, Writer, Holder);
    end;
    aWriter.AddExt(ExtType, Writer.BufPtr^, Writer.TotalWritten);
  finally
    Writer.Free;
  end;
end;

function TGLiteIntHashMapExt.TryRead(aData: Pointer; const aBlob: TMpExtBlob): Boolean;
var
  p: PExtValue absolute aData;
  Info: PTypeInfo;
  k: Int64;
  v: T;
  Reader: TMpReader;
  I: SizeInt;
begin
  p^.MakeEmpty;
  Info := TypeInfo(T);
  Reader := TMpReader.Create(@aBlob[1], System.Length(aBlob)-1, MaxDepth);
  try
    Reader.Read;
    if Reader.TokenKind <> mtkMapBegin then exit(False);
    p^.EnsureCapacity(Reader.StructUnread);
    try
      for I := 0 to Pred(Reader.StructUnread) do begin
        Reader.Read;
        if Reader.KeyValue.Kind <> mvkInt then exit(False);
        k := Reader.KeyValue.AsInt;
        v := Default(T);
        MsgPack2Pdo(Info, v, Reader, Holder);
        p^.Add(k, v);
      end;
    except
      exit(False);
    end;
    Reader.Read;
    if Reader.TokenKind <> mtkMapEnd then exit(False);
    Reader.Read;
    if Reader.ReadState <> mrsEof then exit(False);
  finally
    Reader.Free;
  end;
  Result := True;
end;

{ TGLiteChainHashMapExt }

procedure TGLiteChainHashMapExt.Write(aData: Pointer; aWriter: TMpCustomWriter);
var
  p: PExtValue absolute aData;
  KeyInfo, ValInfo: PTypeInfo;
  e: TExtValue.TEntry;
  Writer: TMpWriter;
begin
  KeyInfo := TypeInfo(TKey);
  ValInfo := TypeInfo(TValue);
  Writer := TMpWriter.Create;
  try
    Writer.BeginArray(p^.Count*2);
    for e in p^ do begin
      Pdo2MsgPack(KeyInfo, e.Key, Writer, Holder);
      Pdo2MsgPack(ValInfo, e.Value, Writer, Holder);
    end;
    aWriter.AddExt(ExtType, Writer.BufPtr^, Writer.TotalWritten);
  finally
    Writer.Free;
  end;
end;

function TGLiteChainHashMapExt.TryRead(aData: Pointer; const aBlob: TMpExtBlob): Boolean;
var
  p: PExtValue absolute aData;
  KeyInfo, ValInfo: PTypeInfo;
  k: TKey;
  v: TValue;
  Reader: TMpReader;
  I, Cap: SizeInt;
begin
  p^.MakeEmpty;
  KeyInfo := TypeInfo(TKey);
  ValInfo := TypeInfo(TValue);
  Reader := TMpReader.Create(@aBlob[1], System.Length(aBlob)-1, MaxDepth);
  try
    Reader.Read;
    if Reader.TokenKind <> mtkArrayBegin then exit(False);
    Cap := Reader.StructUnread div 2;
    p^.EnsureCapacity(Cap);
    try
      for I := 0 to Pred(Cap) do begin
        Reader.Read;
        k := Default(TKey);
        MsgPack2Pdo(KeyInfo, k, Reader, Holder);
        Reader.Read;
        v := Default(TValue);
        MsgPack2Pdo(ValInfo, v, Reader, Holder);
        p^.Add(k, v);
      end;
    except
      exit(False);
    end;
    Reader.Read;
    if Reader.TokenKind <> mtkArrayEnd then exit(False);
    Reader.Read;
    if Reader.ReadState <> mrsEof then exit(False);
  finally
    Reader.Free;
  end;
  Result := True;
end;

{ TSimpleText }

class operator TSimpleText.:=(const txt: TSimpleText): string;
begin
  Result := txt.FText;
end;

class operator TSimpleText.:=(const s: string): TSimpleText;
begin
  Result.FText := s;
end;

class operator TSimpleText.Explicit(const txt: TSimpleText): string;
begin
  Result := txt.FText;
end;

class operator TSimpleText.Explicit(const s: string): TSimpleText;
begin
  Result.FText := s;
end;

{ TTextCompressExt }

constructor TTextCompressExt.Create(aExtType: TUserExtType; aLevel: TCompressionLevel);
begin
  inherited Create(aExtType);
  FCompressLevel := aLevel;
end;

{$PUSH}{$WARN 5089 OFF : Local variable "$1" of a managed type does not seem to be initialized}
procedure TTextCompressExt.Write(aData: Pointer; aWriter: TMpCustomWriter);
var
  pText: PExtValue absolute aData;
  s: string;
  ms: specialize TGAutoRef<TMemoryStream>;
  cs: specialize TGUniqRef<TCompressionStream>;
const
  MIN_LEN = 128;
begin
  s := pText^;
  if System.Length(s) >= MIN_LEN then begin
    cs.Instance := TCompressionStream.Create(FCompressLevel, ms.Instance, True);
    cs.Instance.WriteBuffer(Pointer(s)^, System.Length(s));
    cs.Clear;
    aWriter.AddExt(ExtType, ms.Instance.Memory^, ms.Instance.Size);
  end else
    aWriter.AddExt(ExtType, Pointer(s)^, System.Length(s));
end;

function TTextCompressExt.TryRead(aData: Pointer; const aBlob: TMpExtBlob): Boolean;
var
  pText: PExtValue absolute aData;
  s: string;
  ms, rs: specialize TGAutoRef<TMemoryStream>;
  dcs: specialize TGUniqRef<TDecompressionStream>;
begin
  ms.Instance.WriteBuffer(aBlob[1], System.Length(aBlob)-1);
  ms.Instance.Position := 0;
  dcs.Instance := TDecompressionStream.Create(ms.Instance, True);
  try
    rs.Instance.CopyFrom(dcs.Instance, 0);
    System.SetLength(s, rs.Instance.Size);
    System.Move(rs.Instance.Memory^, Pointer(s)^, rs.Instance.Size);
  except
    System.SetLength(s, System.Length(aBlob)-1);
    System.Move(aBlob[1], Pointer(s)^, System.Length(aBlob)-1);
  end;
  pText^ := s;
  Result := True;
end;
{$POP}

end.

