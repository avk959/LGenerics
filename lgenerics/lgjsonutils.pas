{****************************************************************************
*                                                                           *
*   This file is part of the LGenerics package.                             *
*   Some useful JSON utilites.                                              *
*                                                                           *
*   Copyright(c) 2025 A.Koverdyaev(avk)                                     *
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

interface

uses
  Classes, SysUtils, JsonScanner, JsonParser, FpJson,
  lgUtils,
  lgHelpers,
  lgJson,
  lgStrConst;

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

implementation
{$B-}{$COPERATORS ON}{$POINTERMATH ON}

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
      jtUnknown: raise EJsException.Create(SEUnknownJsDataType);
      jtNull:    aDst.AsNull;
      jtBoolean: aDst.AsBoolean := aSrc.AsBoolean;
      jtNumber:  aDst.AsNumber := aSrc.AsFloat;
      jtString:  aDst.AsString := aSrc.AsString;
      jtArray:
        begin
          aDst.AsArray;
          for I := 0 to Pred(aSrc.Count) do
            CopyData(aSrc.Items[I], aDst.AddNode);
        end;
      jtObject:
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

