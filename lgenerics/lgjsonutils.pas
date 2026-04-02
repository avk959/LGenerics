{****************************************************************************
*                                                                           *
*   This file is part of the LGenerics package.                             *
*   Some useful JSON utilites.                                              *
*                                                                           *
*   Copyright(c) 2025-2026 A.Koverdyaev(avk)                                *
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
unit lgJsonUtils;

{$MODE OBJFPC}{$H+}
{$MODESWITCH ADVANCEDRECORDS}
{$MODESWITCH TYPEHELPERS}

interface

uses
  Classes, SysUtils, JsonScanner, JsonParser, FpJson,
  lgUtils,
  lgHelpers,
  lgJson,
  lgPdo;

type
  { TJsonDataHelper }
  TJsonDataHelper = class helper(TGObjectHelper) for TJsonData
  const
    DEF_PARSER_OPTS = [joUTF8, joStrict];
    class function TryParse(const s: TJSONStringType; out aData: TJsonData;
                            const aOptions: TJSONOptions = DEF_PARSER_OPTS): Boolean; static;
    class function TryParse(aStream: TStream; out aData: TJsonData;
                            const aOptions: TJSONOptions = DEF_PARSER_OPTS): Boolean; static;
  { may raise an exception if it can't open the file for reading }
    class function TryParseFile(const aFileName: string; out aData: TJsonData;
                                const aOptions: TJSONOptions = DEF_PARSER_OPTS): Boolean; static;
  { may raise an exception if the instance is an abstract type }
    function  AsCompactJson: TJSONStringType;
  { may raise an exception if the instance is an abstract type }
    procedure SaveToStream(aStream: TStream);
  { may raise an exception if it cannot open a file for writing or the instance is an abstract type }
    procedure SaveToFile(const aFileName: string);
  { tries to find an element using a path specified as an array of path segments }
    function  FindPath(const aPath: array of string; out aData: TJsonData): Boolean; overload;
  { tries to find the element using the path given by the JSON pointer as a Pascal string }
    function  FindPathPtr(const aPtr: string; out aData: TJsonData): Boolean;
  end;

  { TJsonMergePatch: implements JSON Merge Patch(RFC 7396), a simpler alternative
    to JSON Patch (RFC 6902) for updating JSON resources;
      main drawbacks:
        cannot change the structure of arrays, only replace the whole array
        cannot set any property in the target object to null }
  TJsonMergePatch = class
  private
    FPatch: TJsonNode;
    function GetLoaded: Boolean; inline;
    function GetAsJson: string; inline;
    class procedure DoDiff(aSource, aTarget, aPatch: TJsonNode); static;
  public
  const
    MIME_TYPE = 'application/merge-patch+json';
  { returns a patch that converts aSource to aTarget as TJsonNode }
    class function  DiffNode(aSource, aTarget: TJsonNode): TJsonNode; static;
  { returns a patch that converts aSource to aTarget as TJsonMergePatch }
    class function  Diff(aSource, aTarget: TJsonNode): TJsonMergePatch; static;
  { tries to create a patch that converts JSON aSource to JSON aTarget;
    returns False if aSource or aTarget is invalid JSON, otherwise returns
    True and the resulting patch in the aPatch parameter }
    class function  Diff(const aSource, aTarget: string; out aPatch: TJsonNode): Boolean; static;
    class function  Diff(const aSource, aTarget: string; out aPatch: TJsonMergePatch): Boolean; static;
  { returns a patch which converts aSource into aTarget as JSON }
    class function  DiffJson(aSource, aTarget: TJsonNode): string; static;
  { tries to create patch that converts the JSON aSource to JSON aTarget;
    returns False if aSource or aTarget is invalid JSON, otherwise returns True
    and resulting patch as JSON in the aPatch parameter }
    class function  DiffJson(const aSource, aTarget: string; out aPatch: string): Boolean; static;
  { applies patch aPatch to aTarget }
    class procedure ApplyPatch(aPatch, aTarget: TJsonNode); static;
  { tries to apply patch aPatch to JSON aTarget; returns False if aTarget
    is an invalid JSON, otherwise returns True }
    class function  ApplyPatch(aPatch: TJsonNode; var aTarget: string): Boolean; static;
  { tries to apply patch aPatch to JSON aTarget; returns False if aPatch is an
    invalid JSON, otherwise applies patch aPatch to aTarget and returns True }
    class function  ApplyPatch(const aPatch: string; aTarget: TJsonNode): Boolean; static;
  { tries to apply patch aPatch to JSON aTarget; returns False if aPatch or aTarget is an
    invalid JSON, otherwise applies patch aPatch to aTarget and returns True }
    class function  ApplyPatch(const aPatch: string; var aTarget: string): Boolean; static;
  { aJson must be a well formed JSON, otherwise the created instance will be empty }
    constructor Create(const aJson: string);
  { clones aPatchNode into an internal field }
    constructor Create(aPatchNode: TJsonNode);
    destructor Destroy; override;
  { tries to apply the internal patch to aTarget; returns False if no
    patch was loaded, otherwise applies patch to aTarget and returns True }
    function Apply(aTarget: TJsonNode): Boolean;
    function Apply(var aTarget: string): Boolean;
    property Loaded: Boolean read GetLoaded;
    property AsJson: string read GetAsJson;
  end;

  function JsonNode2Data(aNode: TJsonNode): TJsonData;
  function JsonData2Node(aData: TJsonData): TJsonNode;

type
  TOptString = specialize TGOptional<string>;
  POptString = ^TOptString;

  TOptStrHelper = type helper for TOptString
    class procedure WriteJson(p: Pointer; aWriter: TJsonStrWriter); static;
    class function  ReadJson(p: Pointer; aReader: TJsonReader; const aOpts: TJsonReadOptions): Boolean; static;
  end;

  TOptBoolean = specialize TGOptional<Boolean>;
  POptBoolean = ^TOptBoolean;

  TOptBoolHelper = type helper for TOptBoolean
    class procedure WriteJson(p: Pointer; aWriter: TJsonStrWriter); static;
    class function  ReadJson(p: Pointer; aReader: TJsonReader; const aOpts: TJsonReadOptions): Boolean; static;
  end;

  TOptShortInt = specialize TGOptional<ShortInt>;
  POptShortInt = ^TOptShortInt;

  TOptShortIntHelper = type helper for TOptShortInt
    class procedure WriteJson(p: Pointer; aWriter: TJsonStrWriter); static;
    class function  ReadJson(p: Pointer; aReader: TJsonReader; const aOpts: TJsonReadOptions): Boolean; static;
  end;

  TOptByte = specialize TGOptional<Byte>;
  POptByte = ^TOptByte;

  TOptByteHelper = type helper for TOptByte
    class procedure WriteJson(p: Pointer; aWriter: TJsonStrWriter); static;
    class function  ReadJson(p: Pointer; aReader: TJsonReader; const aOpts: TJsonReadOptions): Boolean; static;
  end;

  TOptSmallInt = specialize TGOptional<SmallInt>;
  POptSmallInt = ^TOptSmallInt;

  TOptSmallIntHelper = type helper for TOptSmallInt
    class procedure WriteJson(p: Pointer; aWriter: TJsonStrWriter); static;
    class function  ReadJson(p: Pointer; aReader: TJsonReader; const aOpts: TJsonReadOptions): Boolean; static;
  end;

  TOptWord = specialize TGOptional<Word>;
  POptWord = ^TOptWord;

  TOptWordHelper = type helper for TOptWord
    class procedure WriteJson(p: Pointer; aWriter: TJsonStrWriter); static;
    class function  ReadJson(p: Pointer; aReader: TJsonReader; const aOpts: TJsonReadOptions): Boolean; static;
  end;

  TOptInteger = specialize TGOptional<Integer>;
  POptInteger = ^TOptInteger;

  TOptIntHelper = type helper for TOptInteger
    class procedure WriteJson(p: Pointer; aWriter: TJsonStrWriter); static;
    class function  ReadJson(p: Pointer; aReader: TJsonReader; const aOpts: TJsonReadOptions): Boolean; static;
  end;

  TOptCardinal = specialize TGOptional<Cardinal>;
  POptCardinal = ^TOptCardinal;

  TOptCardinalHelper = type helper for TOptCardinal
    class procedure WriteJson(p: Pointer; aWriter: TJsonStrWriter); static;
    class function  ReadJson(p: Pointer; aReader: TJsonReader; const aOpts: TJsonReadOptions): Boolean; static;
  end;

  TOptDouble = specialize TGOptional<Double>;
  POptDouble = ^TOptDouble;

  TOptDoubleHelper = type helper for TOptDouble
    class procedure WriteJson(p: Pointer; aWriter: TJsonStrWriter); static;
    class function  ReadJson(p: Pointer; aReader: TJsonReader; const aOpts: TJsonReadOptions): Boolean; static;
  end;

  TDateTimeUtc = record
  private
    FValue: TDateTime;
  public
    class procedure WriteJson(p: Pointer; aWriter: TJsonStrWriter); static;
    class function  ReadJson(p: Pointer; aReader: TJsonReader; const aOpts: TJsonReadOptions): Boolean; static;
    class operator := (const dtu: TDateTimeUtc): TDateTime; inline;
    class operator := (const dt: TDateTime): TDateTimeUtc; inline;
    class operator Explicit(const dt: TDateTime): TDateTimeUtc; inline;
    class operator Explicit(const dtu: TDateTimeUtc): TDateTime; inline;
  end;
  PDateTimeUtc = ^TDateTimeUtc;

  TOptDateTimeUtc = specialize TGOptional<TDateTimeUtc>;
  POptDateTimeUtc = ^TOptDateTimeUtc;

  TOptDateTimeUtcPdoHelper = type helper for TOptDateTimeUtc
    class procedure WriteJson(p: Pointer; aWriter: TJsonStrWriter); static;
    class function  ReadJson(p: Pointer; aReader: TJsonReader; const aOpts: TJsonReadOptions): Boolean; static;
  end;

  TGuidPdoHelper = type helper(TGGuidHelper) for TGuid
    class procedure WriteJson(p: Pointer; aWriter: TJsonStrWriter); static;
    class function  ReadJson(p: Pointer; aReader: TJsonReader; const aOpts: TJsonReadOptions): Boolean; static;
  end;

  TOptGuid = specialize TGOptional<TGuid>;
  POptGuid = ^TOptGuid;

  TOptGuidHelper = type helper for TOptGuid
    class procedure WriteJson(p: Pointer; aWriter: TJsonStrWriter); static;
    class function  ReadJson(p: Pointer; aReader: TJsonReader; const aOpts: TJsonReadOptions): Boolean; static;
  end;

  TInt64 = record
  private
    FValue: Int64;
  public
    class procedure WriteJson(p: Pointer; aWriter: TJsonStrWriter); static;
    class function  ReadJson(p: Pointer; aReader: TJsonReader; const aOpts: TJsonReadOptions): Boolean; static;
    class operator := (const i: TInt64): Int64; inline;
    class operator := (const i: Int64): TInt64; inline;
    class operator Explicit(const i: TInt64): Int64; inline;
    class operator Explicit(const i: Int64): TInt64; inline;
  end;
  PInt64t = ^TInt64;

  TOptInt64 = specialize TGOptional<TInt64>;
  POptInt64 = ^TOptInt64;

  TOptInt64Helper = type helper for TOptInt64
    class procedure WriteJson(p: Pointer; aWriter: TJsonStrWriter); static;
    class function  ReadJson(p: Pointer; aReader: TJsonReader; const aOpts: TJsonReadOptions): Boolean; static;
  end;

  TUInt64 = record
  private
    FValue: UInt64;
  public
    class procedure WriteJson(p: Pointer; aWriter: TJsonStrWriter); static;
    class function  ReadJson(p: Pointer; aReader: TJsonReader; const aOpts: TJsonReadOptions): Boolean; static;
    class operator := (const u: TUInt64): UInt64; inline;
    class operator := (const u: UInt64): TUInt64; inline;
    class operator Explicit(const u: TUInt64): UInt64; inline;
    class operator Explicit(const u: UInt64): TUInt64; inline;
  end;
  PUInt64t = ^TUInt64;

  TOptUInt64 = specialize TGOptional<TUInt64>;
  POptUInt64 = ^TOptUInt64;

  TOptUInt64Helper = type helper for TOptUInt64
    class procedure WriteJson(p: Pointer; aWriter: TJsonStrWriter); static;
    class function  ReadJson(p: Pointer; aReader: TJsonReader; const aOpts: TJsonReadOptions): Boolean; static;
  end;

  TCurrency = record
  private
    FValue: Currency;
  public
    class procedure WriteJson(p: Pointer; aWriter: TJsonStrWriter); static;
    class function  ReadJson(p: Pointer; aReader: TJsonReader; const aOpts: TJsonReadOptions): Boolean; static;
    class operator := (const c: TCurrency): Currency; inline;
    class operator := (const c: Currency): TCurrency; inline;
    class operator Explicit(const c: TCurrency): Currency; inline;
    class operator Explicit(const c: Currency): TCurrency; inline;
  end;
  PCurrencyT = ^TCurrency;

  TOptCurrency = specialize TGOptional<TCurrency>;
  POptCurrency = ^TOptCurrency;

  TOptCurrencyHelper = type helper for TOptCurrency
    class procedure WriteJson(p: Pointer; aWriter: TJsonStrWriter); static;
    class function  ReadJson(p: Pointer; aReader: TJsonReader; const aOpts: TJsonReadOptions): Boolean; static;
  end;

implementation
{$B-}{$COPERATORS ON}{$POINTERMATH ON}
uses
  LgJsonTypeDef, lgStrConst;

function JsonNode2Data(aNode: TJsonNode): TJsonData;
  function CopyNode(aNode: TJsonNode): TJsonData;
  var
    I: SizeInt;
    d: Double;
    I64: Int64;
  begin
    Result := nil;
    case aNode.Kind of
      jvkNull:  Result := CreateJson;
      jvkFalse: Result := CreateJson(False);
      jvkTrue:  Result := CreateJson(True);
      jvkNumber:
        begin
          d := aNode.AsNumber;
          if Double.IsExactInt(d, I64) then
            if (I64 < System.Low(Integer)) or (I64 > System.High(Integer)) then
              Result := CreateJson(I64)
            else
              Result := CreateJson(Integer(I64))
          else
            Result := CreateJson(d);
        end;
      jvkString: Result := CreateJson(aNode.AsString);
      jvkArray:
        begin
          Result := TJsonArray.Create;
          for I := 0 to Pred(aNode.Count) do
            TJsonArray(Result).Add(CopyNode(aNode.Items[I]));
        end;
      jvkObject:
        begin
          Result := TJsonObject.Create;
          for I := 0 to Pred(aNode.Count) do
            with aNode.Pairs[I] do
              TJsonObject(Result).Add(Key, CopyNode(Value));
        end;
    end;
  end;
begin
  if aNode = nil then exit(nil);
  Result := CopyNode(aNode);
end;

function JsonData2Node(aData: TJsonData): TJsonNode;
  procedure CopyData(aSrc: TJsonData; aDst: TJsonNode);
  var
    I: SizeInt;
    e: TJsonEnum;
  begin
    case aSrc.JSONType of
      TJSONtype.jtUnknown: raise EJsException.Create(SEUnknownTJsonDataType);
      TJSONtype.jtNull:    aDst.AsNull;
      TJSONtype.jtBoolean: aDst.AsBoolean := aSrc.AsBoolean;
      TJSONtype.jtNumber:  aDst.AsNumber := aSrc.AsFloat;
      TJSONtype.jtString:  aDst.AsString := aSrc.AsString;
      TJSONtype.jtArray:
        begin
          aDst.AsArray;
          for I := 0 to Pred(aSrc.Count) do
            CopyData(aSrc.Items[I], aDst.AddNode);
        end;
      TJSONtype.jtObject:
        begin
          aDst.AsObject;
          for e in aSrc do
            CopyData(e.Value, aDst.AddNode(e.Key));
        end;
    end;
  end;
begin
  if aData = nil then exit(nil);
  Result := TJsonNode.Create;
  CopyData(aData, Result);
end;

{ TJsonDataHelper }

class function TJsonDataHelper.TryParse(const s: TJSONStringType; out aData: TJsonData;
  const aOptions: TJSONOptions): Boolean;
begin
  aData := nil;
  Result := False;
  try
    with TJSONParser.Create(s, aOptions) do
      try
        aData := Parse;
        Result := True;
      finally
        Free;
      end;
  except
  end;
end;

class function TJsonDataHelper.TryParse(aStream: TStream; out aData: TJsonData;
  const aOptions: TJSONOptions): Boolean;
begin
  aData := nil;
  Result := False;
  try
    with TJSONParser.Create(aStream, aOptions) do
      try
        aData := Parse;
        Result := True;
      finally
        Free;
      end;
  except
  end;
end;

class function TJsonDataHelper.TryParseFile(const aFileName: string; out aData: TJsonData;
  const aOptions: TJSONOptions): Boolean;
var
  fs: TStream;
begin
  Result := False;
  fs := TFileStream.Create(aFileName, fmOpenRead or fmShareDenyWrite);
  try
    Result := TryParse(fs, aData, aOptions);
  finally
    fs.Free
  end;
end;

function TJsonDataHelper.AsCompactJson: TJSONStringType;
var
  ss: TStringStream;
begin
  Result := '';
  ss := TStringStream.Create('', CP_UTF8);
  try
    DumpJSON(ss);
    Result := ss.DataString;
  finally
    ss.Free;
  end;
end;

procedure TJsonDataHelper.SaveToStream(aStream: TStream);
var
  ms: TMemoryStream;
begin
  if aStream is TFpJsStream then
    DumpJSON(TFpJsStream(aStream))
  else begin
    ms := TMemoryStream.Create;
    try
      DumpJSON(ms);
      aStream.WriteBuffer(ms.Memory^, ms.Size);
    finally
      ms.Free;
    end;
  end;
end;

procedure TJsonDataHelper.SaveToFile(const aFileName: string);
var
  fs: TStream;
begin
  fs := TFileStream.Create(aFileName, fmCreate);
  try
    SaveToStream(fs);
  finally
    fs.Free;
  end;
end;

function TJsonDataHelper.FindPath(const aPath: array of string; out aData: TJsonData): Boolean;
var
  d: TJsonData;
  I, Idx: SizeInt;
begin
  if System.Length(aPath) = 0 then
    begin
      aData := Self;
      exit(True);
    end;
  aData := nil;
  d := Self;
  for I := 0 to System.High(aPath) do
    begin
      if d.Count = 0 then
        exit(False);
      if d.JsonType = jtArray then
        begin
          if not(IsNonNegativeInt(aPath[I], Idx) and (Idx < d.Count)) then
            exit(False);
          d := TJsonArray(d)[Idx];
        end
      else
        begin
          d := TJsonObject(d).Find(aPath[I]);
          if d = nil then
            exit(False);
        end;
    end;
  aData := d;
  Result := d <> nil;
end;

function TJsonDataHelper.FindPathPtr(const aPtr: string; out aData: TJsonData): Boolean;
var
  Segments: TStringArray;
begin
  if aPtr = '' then
    begin
      aData := Self;
      exit(True);
    end;
  if not TJsonPtr.TryGetSegments(aPtr, Segments) then
    begin
      aData := nil;
      exit(False);
    end;
  Result := FindPath(Segments, aData);
end;

{ TJsonMergePatch }

function TJsonMergePatch.GetLoaded: Boolean;
begin
  Result := FPatch <> nil;
end;

function TJsonMergePatch.GetAsJson: string;
begin
  if Loaded then
    Result := FPatch.AsJson
  else
    Result := '';
end;

class procedure TJsonMergePatch.DoDiff(aSource, aTarget, aPatch: TJsonNode);
var
  p: TJsonNode.TPair;
  n: TJsonNode;
begin
  if not(aSource.IsObject and aTarget.IsObject) then
    begin
      aPatch.CopyFrom(aTarget);
      exit;
    end;
  aPatch.AsObject;
  for p in aSource.Entries do
    if not aTarget.Contains(p.Key) then
      aPatch.AddNull(p.Key);
  for p in aTarget.Entries do
    if aSource.Find(p.Key, n) then
      if not p.Value.EqualTo(n) then
        DoDiff(n, p.Value, aPatch.AddNode(p.Key))else
    else
      aPatch.AddNode(p.Key).CopyFrom(p.Value);
end;

class function TJsonMergePatch.DiffNode(aSource, aTarget: TJsonNode): TJsonNode;
begin
  Result := TJsonNode.Create;
  DoDiff(aSource, aTarget, Result);
end;

class function TJsonMergePatch.Diff(aSource, aTarget: TJsonNode): TJsonMergePatch;
begin
  Result := TJsonMergePatch.Create('');
  Result.FPatch := DiffNode(aSource, aTarget);
end;

class function TJsonMergePatch.Diff(const aSource, aTarget: string; out aPatch: TJsonNode): Boolean;
var
  Src, Trg: specialize TGAutoRef<TJsonNode>;
begin
  aPatch := nil;
  if not(Src.Instance.TryParse(aSource) and Trg.Instance.TryParse(aTarget)) then exit(False);
  aPatch := DiffNode(Src.Instance, Trg.Instance);
  Result := True;
end;

class function TJsonMergePatch.Diff(const aSource, aTarget: string; out aPatch: TJsonMergePatch): Boolean;
var
  Src, Trg: specialize TGAutoRef<TJsonNode>;
begin
  aPatch := nil;
  if not(Src.Instance.TryParse(aSource) and Trg.Instance.TryParse(aTarget)) then exit(False);
  aPatch := Diff(Src.Instance, Trg.Instance);
  Result := True;
end;

class function TJsonMergePatch.DiffJson(aSource, aTarget: TJsonNode): string;
begin
  Result := '';
  with DiffNode(aSource, aTarget) do
    try
      Result := AsJson;
    finally
      Free;
    end;
end;

class function TJsonMergePatch.DiffJson(const aSource, aTarget: string; out aPatch: string): Boolean;
var
  n: TJsonNode;
begin
  aPatch := '';
  Result := Diff(aSource, aTarget, n);
  if Result then
    try
      aPatch := n.AsJson;
    finally
      n.Free;
    end;
end;

class procedure TJsonMergePatch.ApplyPatch(aPatch, aTarget: TJsonNode);
var
  p: TJsonNode.TPair;
  n: TJsonNode;
begin
  if aPatch.IsObject then
    begin
      aTarget.AsObject;
      for p in aPatch.Entries do
        if p.Value.IsNull then
          aTarget.Remove(p.Key)
        else
          begin
            aTarget.FindOrAdd(p.Key, n);
            ApplyPatch(p.Value, n);
          end;
    end
  else
    aTarget.CopyFrom(aPatch);
end;

class function TJsonMergePatch.ApplyPatch(aPatch: TJsonNode; var aTarget: string): Boolean;
var
  Target: specialize TGAutoRef<TJsonNode>;
begin
  if not Target.Instance.TryParse(aTarget) then exit(False);
  ApplyPatch(aPatch, Target.Instance);
  aTarget := Target.Instance.AsJson;
  Result := True;
end;

class function TJsonMergePatch.ApplyPatch(const aPatch: string; aTarget: TJsonNode): Boolean;
var
  Patch: specialize TGAutoRef<TJsonNode>;
begin
  if not Patch.Instance.TryParse(aPatch) then exit(False);
  ApplyPatch(Patch.Instance, aTarget);
  Result := True;
end;

class function TJsonMergePatch.ApplyPatch(const aPatch: string; var aTarget: string): Boolean;
var
  Patch, Target: specialize TGAutoRef<TJsonNode>;
begin
  if not(Patch.Instance.TryParse(aPatch) and Target.Instance.TryParse(aTarget))then exit(False);
  ApplyPatch(Patch.Instance, Target.Instance);
  aTarget := Target.Instance.AsJson;
  Result := True;
end;

constructor TJsonMergePatch.Create(const aJson: string);
var
  n: TJsonNode;
begin
  if TJsonNode.TryParse(aJson, n) then
    FPatch := n;
end;

constructor TJsonMergePatch.Create(aPatchNode: TJsonNode);
begin
  FPatch := aPatchNode.Clone;
end;

destructor TJsonMergePatch.Destroy;
begin
  FPatch.Free;
  inherited;
end;

function TJsonMergePatch.Apply(aTarget: TJsonNode): Boolean;
begin
  if not Loaded then exit(False);
  ApplyPatch(FPatch, aTarget);
  Result := True;
end;

function TJsonMergePatch.Apply(var aTarget: string): Boolean;
begin
  if not Loaded then exit(False);
  Result := ApplyPatch(FPatch, aTarget);
end;

{ TOptStrHelper }

class procedure TOptStrHelper.WriteJson(p: Pointer; aWriter: TJsonStrWriter);
begin
  if POptString(p)^.Assigned then
    aWriter.Add(POptString(p)^.Value)
  else
    aWriter.AddNull;
end;

class function TOptStrHelper.ReadJson(p: Pointer; aReader: TJsonReader; const aOpts: TJsonReadOptions): Boolean;
begin
  if aReader.TokenKind = rtkNull then
    begin
      Result := not(jroRejectNulls in aOpts);
      if Result then POptString(p)^.Clear;
    end
  else
    begin
      Result := aReader.TokenKind = rtkString;
      if Result then POptString(p)^ := aReader.AsString;
    end;
end;

{ TOptBoolHelper }

class procedure TOptBoolHelper.WriteJson(p: Pointer; aWriter: TJsonStrWriter);
begin
  if POptBoolean(p)^.Assigned then
    if POptBoolean(p)^.Value then
      aWriter.AddTrue
    else
      aWriter.AddFalse
  else
    aWriter.AddNull;
end;

class function TOptBoolHelper.ReadJson(p: Pointer; aReader: TJsonReader; const aOpts: TJsonReadOptions): Boolean;
begin
  if aReader.TokenKind = rtkNull then
    begin
      Result := not(jroRejectNulls in aOpts);
      if Result then POptBoolean(p)^.Clear;
    end
  else
    begin
      Result := aReader.TokenKind in [rtkFalse, rtkTrue];
      if Result then POptBoolean(p)^ := aReader.AsBoolean;
    end;
end;

{ TOptShortIntHelper }

class procedure TOptShortIntHelper.WriteJson(p: Pointer; aWriter: TJsonStrWriter);
begin
  if POptShortInt(p)^.Assigned then
    aWriter.Add(POptShortInt(p)^.Value)
  else
    aWriter.AddNull;
end;

{$PUSH}{$WARN 5036 OFF : Local variable "$1" does not seem to be initialized}
class function TOptShortIntHelper.ReadJson(p: Pointer; aReader: TJsonReader; const aOpts: TJsonReadOptions): Boolean;
var
  I: Int64;
begin
  if aReader.TokenKind = rtkNull then
    begin
      Result := not(jroRejectNulls in aOpts);
      if Result then POptShortInt(p)^.Clear;
    end
  else
    begin
      Result := (aReader.TokenKind = rtkNumber) and aReader.AsNumber.IsExactInt(I) and
                (I >= Low(ShortInt)) and (I <= High(ShortInt));
      if Result then POptShortInt(p)^ := ShortInt(I);
    end;
end;

{ TOptByteHelper }

class procedure TOptByteHelper.WriteJson(p: Pointer; aWriter: TJsonStrWriter);
begin
  if POptByte(p)^.Assigned then
    aWriter.Add(POptByte(p)^.Value)
  else
    aWriter.AddNull;
end;

class function TOptByteHelper.ReadJson(p: Pointer; aReader: TJsonReader; const aOpts: TJsonReadOptions): Boolean;
var
  I: Int64;
begin
  if aReader.TokenKind = rtkNull then
    begin
      Result := not(jroRejectNulls in aOpts);
      if Result then POptByte(p)^.Clear;
    end
  else
    begin
      Result := (aReader.TokenKind = rtkNumber) and aReader.AsNumber.IsExactInt(I) and
                (I >= 0) and (I <= High(Byte));
      if Result then POptByte(p)^ := Byte(I);
    end;
end;

{ TOptSmallIntHelper }

class procedure TOptSmallIntHelper.WriteJson(p: Pointer; aWriter: TJsonStrWriter);
begin
  if POptSmallInt(p)^.Assigned then
    aWriter.Add(POptSmallInt(p)^.Value)
  else
    aWriter.AddNull;
end;

class function TOptSmallIntHelper.ReadJson(p: Pointer; aReader: TJsonReader; const aOpts: TJsonReadOptions): Boolean;
var
  I: Int64;
begin
  if aReader.TokenKind = rtkNull then
    begin
      Result := not(jroRejectNulls in aOpts);
      if Result then POptSmallInt(p)^.Clear;
    end
  else
    begin
      Result := (aReader.TokenKind = rtkNumber) and aReader.AsNumber.IsExactInt(I) and
                (I >= Low(SmallInt)) and (I <= High(SmallInt));
      if Result then POptSmallInt(p)^ := SmallInt(I);
    end;
end;

{ TOptWordHelper }

class procedure TOptWordHelper.WriteJson(p: Pointer; aWriter: TJsonStrWriter);
begin
  if POptWord(p)^.Assigned then
    aWriter.Add(POptWord(p)^.Value)
  else
    aWriter.AddNull;
end;

class function TOptWordHelper.ReadJson(p: Pointer; aReader: TJsonReader; const aOpts: TJsonReadOptions): Boolean;
var
  I: Int64;
begin
  if aReader.TokenKind = rtkNull then
    begin
      Result := not(jroRejectNulls in aOpts);
      if Result then POptWord(p)^.Clear;
    end
  else
    begin
      Result := (aReader.TokenKind = rtkNumber) and aReader.AsNumber.IsExactInt(I) and
                (I >= 0) and (I <= High(Word));
      if Result then POptWord(p)^ := Word(I);
    end;
end;

{ TOptIntHelper }

class procedure TOptIntHelper.WriteJson(p: Pointer; aWriter: TJsonStrWriter);
begin
  if POptInteger(p)^.Assigned then
    aWriter.Add(POptInteger(p)^.Value)
  else
    aWriter.AddNull;
end;

class function TOptIntHelper.ReadJson(p: Pointer; aReader: TJsonReader; const aOpts: TJsonReadOptions): Boolean;
var
  I: Int64;
begin
  if aReader.TokenKind = rtkNull then
    begin
      Result := not(jroRejectNulls in aOpts);
      if Result then POptInteger(p)^.Clear;
    end
  else
    begin
      Result := (aReader.TokenKind = rtkNumber) and aReader.AsNumber.IsExactInt(I) and
                (I >= Low(Integer)) and (I <= High(Integer));
      if Result then POptInteger(p)^ := Integer(I);
    end;
end;

{ TOptCardinalHelper }

class procedure TOptCardinalHelper.WriteJson(p: Pointer; aWriter: TJsonStrWriter);
begin
  if POptCardinal(p)^.Assigned then
    aWriter.Add(POptCardinal(p)^.Value)
  else
    aWriter.AddNull;
end;

class function TOptCardinalHelper.ReadJson(p: Pointer; aReader: TJsonReader; const aOpts: TJsonReadOptions): Boolean;
var
  I: Int64;
begin
  if aReader.TokenKind = rtkNull then
    begin
      Result := not(jroRejectNulls in aOpts);
      if Result then POptCardinal(p)^.Clear;
    end
  else
    begin
      Result := (aReader.TokenKind = rtkNumber) and aReader.AsNumber.IsExactInt(I) and
                (I >= 0) and (I <= High(Cardinal));
      if Result then POptCardinal(p)^ := Cardinal(I);
    end;
end;
{$POP}

{ TOptDoubleHelper }

class procedure TOptDoubleHelper.WriteJson(p: Pointer; aWriter: TJsonStrWriter);
begin
  if POptDouble(p)^.Assigned then
    aWriter.Add(POptDouble(p)^.Value)
  else
    aWriter.AddNull;
end;

class function TOptDoubleHelper.ReadJson(p: Pointer; aReader: TJsonReader; const aOpts: TJsonReadOptions): Boolean;
begin
  if aReader.TokenKind = rtkNull then
    begin
      Result := not(jroRejectNulls in aOpts);
      if Result then POptDouble(p)^.Clear;
    end
  else
    begin
      Result := aReader.TokenKind = rtkNumber;
      if Result then POptDouble(p)^ := aReader.AsNumber;
    end;
end;

{ TDateTimeUtc }

class procedure TDateTimeUtc.WriteJson(p: Pointer; aWriter: TJsonStrWriter);
begin
  aWriter.Add(UTCToRfc8927TimeStamp(PDateTimeUtc(p)^.FValue));
end;

class function TDateTimeUtc.ReadJson(p: Pointer; aReader: TJsonReader; const aOpts: TJsonReadOptions): Boolean;
begin
  Assert((aOpts = []) or (aOpts <> [])); //
  Result := (aReader.TokenKind = rtkString) and TryRfc8927TimeStampToUTC(aReader.AsString, PDateTimeUtc(p)^.FValue);
end;

class operator TDateTimeUtc.:=(const dtu: TDateTimeUtc): TDateTime;
begin
  Result := dtu.FValue;
end;

class operator TDateTimeUtc.:=(const dt: TDateTime): TDateTimeUtc;
begin
  Result.FValue := dt;
end;

class operator TDateTimeUtc.Explicit(const dt: TDateTime): TDateTimeUtc;
begin
  Result.FValue := dt;
end;

class operator TDateTimeUtc.Explicit(const dtu: TDateTimeUtc): TDateTime;
begin
  Result := dtu.FValue;
end;

{ TOptDateTimeUtcPdoHelper }

class procedure TOptDateTimeUtcPdoHelper.WriteJson(p: Pointer; aWriter: TJsonStrWriter);
begin
  if POptDateTimeUtc(p)^.Assigned then
    aWriter.Add(UTCToRfc8927TimeStamp(POptDateTimeUtc(p)^.Value.FValue))
  else
    aWriter.AddNull;
end;

class function TOptDateTimeUtcPdoHelper.ReadJson(p: Pointer; aReader: TJsonReader;
  const aOpts: TJsonReadOptions): Boolean;
var
  dtu: TDateTimeUtc;
begin
  if aReader.TokenKind = rtkNull then
    begin
      Result := not(jroRejectNulls in aOpts);
      if Result then POptDateTimeUtc(p)^.Clear;
    end
  else
    begin
      Result := (aReader.TokenKind = rtkString) and TryRfc8927TimeStampToUTC(aReader.AsString, dtu.FValue);
      if Result then POptDateTimeUtc(p)^ := dtu;
    end;
end;

{ TGuidPdoHelper }

class procedure TGuidPdoHelper.WriteJson(p: Pointer; aWriter: TJsonStrWriter);
begin
  aWriter.Add(PGuid(p)^.ToString(True));
end;

class function TGuidPdoHelper.ReadJson(p: Pointer; aReader: TJsonReader; const aOpts: TJsonReadOptions): Boolean;
begin
  Assert((aOpts = []) or (aOpts <> [])); //
  Result := (aReader.TokenKind = rtkString) and TGuid.TryParse(aReader.AsString, PGuid(p)^);
end;

{ TOptGuidHelper }

class procedure TOptGuidHelper.WriteJson(p: Pointer; aWriter: TJsonStrWriter);
begin
  if POptGuid(p)^.Assigned then
    aWriter.Add(POptGuid(p)^.Value.ToString(True))
  else
    aWriter.AddNull;
end;

class function TOptGuidHelper.ReadJson(p: Pointer; aReader: TJsonReader; const aOpts: TJsonReadOptions): Boolean;
var
  g: TGuid;
begin
  if aReader.TokenKind = rtkNull then
    begin
      Result := not(jroRejectNulls in aOpts);
      if Result then POptGuid(p)^.Clear;
    end
  else
    begin
      Result := (aReader.TokenKind = rtkString) and TGuid.TryParse(aReader.AsString, g);
      if Result then POptGuid(p)^ := g;
    end;
end;

{ TInt64 }

class procedure TInt64.WriteJson(p: Pointer; aWriter: TJsonStrWriter);
begin
  aWriter.Add(PInt64t(p)^.FValue.ToString);
end;

class function TInt64.ReadJson(p: Pointer; aReader: TJsonReader; const aOpts: TJsonReadOptions): Boolean;
begin
  Assert((aOpts = []) or (aOpts <> [])); //
  Result := (aReader.TokenKind = rtkString) and TryStrToInt64(aReader.AsString, PInt64t(p)^.FValue);
end;

class operator TInt64.:=(const i: TInt64): Int64;
begin
  Result := i.FValue;
end;

class operator TInt64.:=(const i: Int64): TInt64;
begin
  Result.FValue := i;
end;

class operator TInt64.Explicit(const i: TInt64): Int64;
begin
  Result := i.FValue;
end;

class operator TInt64.Explicit(const i: Int64): TInt64;
begin
  Result.FValue := i;
end;

{ TOptInt64Helper }

class procedure TOptInt64Helper.WriteJson(p: Pointer; aWriter: TJsonStrWriter);
begin
  if POptInt64(p)^.Assigned then
    aWriter.Add(Int64(POptInt64(p)^.Value).ToString)
  else
    aWriter.AddNull;
end;

class function TOptInt64Helper.ReadJson(p: Pointer; aReader: TJsonReader; const aOpts: TJsonReadOptions): Boolean;
var
  I: Int64;
begin
  if aReader.TokenKind = rtkNull then
    begin
      Result := not(jroRejectNulls in aOpts);
      if Result then POptInt64(p)^.Clear;
    end
  else
    begin
      Result := (aReader.TokenKind = rtkString) and TryStrToInt64(aReader.AsString, I);
      if Result then POptInt64(p)^ := TInt64(I);
    end;
end;

{ TUInt64 }

class procedure TUInt64.WriteJson(p: Pointer; aWriter: TJsonStrWriter);
begin
  aWriter.Add(PUInt64t(p)^.FValue.ToString);
end;

class function TUInt64.ReadJson(p: Pointer; aReader: TJsonReader; const aOpts: TJsonReadOptions): Boolean;
begin
  Assert((aOpts = []) or (aOpts <> [])); //
  Result := (aReader.TokenKind = rtkString) and TryStrToUInt64(aReader.AsString, PUInt64t(p)^.FValue);
end;

class operator TUInt64.:=(const u: TUInt64): UInt64;
begin
  Result := u.FValue;
end;

class operator TUInt64.:=(const u: UInt64): TUInt64;
begin
  Result.FValue := u;
end;

class operator TUInt64.Explicit(const u: TUInt64): UInt64;
begin
  Result := u.FValue;
end;

class operator TUInt64.Explicit(const u: UInt64): TUInt64;
begin
  Result.FValue := u;
end;

{ TOptUInt64Helper }

class procedure TOptUInt64Helper.WriteJson(p: Pointer; aWriter: TJsonStrWriter);
begin
  if POptUInt64(p)^.Assigned then
    aWriter.Add(UInt64(POptUInt64(p)^.Value).ToString)
  else
    aWriter.AddNull;
end;

class function TOptUInt64Helper.ReadJson(p: Pointer; aReader: TJsonReader;
  const aOpts: TJsonReadOptions): Boolean;
var
  I: UInt64;
begin
  if aReader.TokenKind = rtkNull then
    begin
      Result := not(jroRejectNulls in aOpts);
      if Result then POptUInt64(p)^.Clear;
    end
  else
    begin
      Result := (aReader.TokenKind = rtkString) and TryStrToUInt64(aReader.AsString, I);
      if Result then POptUInt64(p)^ := TUInt64(I);
    end;
end;

{ TCurrency }

class procedure TCurrency.WriteJson(p: Pointer; aWriter: TJsonStrWriter);
var
  pCurr: PCurrencyT absolute p;
begin
  aWriter.Add(IntToStr(PInt64(@pCurr^.FValue)^));
end;

class function TCurrency.ReadJson(p: Pointer; aReader: TJsonReader; const aOpts: TJsonReadOptions): Boolean;
var
  pCurr: PCurrencyT absolute p;
begin
  Assert((aOpts = []) or (aOpts <> [])); //
  Result := (aReader.TokenKind = rtkString) and TryStrToInt64(aReader.AsString, PInt64(@pCurr^.FValue)^);
end;

class operator TCurrency.:=(const c: TCurrency): Currency;
begin
  Result := c.FValue;
end;

class operator TCurrency.:=(const c: Currency): TCurrency;
begin
  Result.FValue := c;
end;

class operator TCurrency.Explicit(const c: TCurrency): Currency;
begin
  Result := c.FValue;
end;

class operator TCurrency.Explicit(const c: Currency): TCurrency;
begin
  Result.FValue := c;
end;

{ TOptCurrencyHelper }

class procedure TOptCurrencyHelper.WriteJson(p: Pointer; aWriter: TJsonStrWriter);
var
  pOptCurr: POptCurrency absolute p;
  c: Currency;
begin
  if pOptCurr^.Assigned then
    begin
      c := pOptCurr^.Value;
      aWriter.Add(IntToStr(PInt64(@c)^));
    end
  else
    aWriter.AddNull;
end;

class function TOptCurrencyHelper.ReadJson(p: Pointer; aReader: TJsonReader;
  const aOpts: TJsonReadOptions): Boolean;
var
  I: Int64;
begin
  if aReader.TokenKind = rtkNull then
    begin
      Result := not(jroRejectNulls in aOpts);
      if Result then POptCurrency(p)^.Clear;
    end
  else
    begin
      Result := (aReader.TokenKind = rtkString) and TryStrToInt64(aReader.AsString, I);
      if Result then POptCurrency(p)^ := TCurrency(PCurrency(@I)^);
    end;
end;

type
  TJsonFloatNum = class(TJsonFloatNumber)
  protected
    function GetAsString: TJSONStringType; override;
  end;

function TJsonFloatNum.GetAsString: TJSONStringType;
begin
  Result := Double2Str(AsFloat, True);
end;

initialization
  FpJson.SetJsonInstanceType(TJsonInstanceType.jitNumberFloat, TJsonFloatNum);
end.

