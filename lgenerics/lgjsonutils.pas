{****************************************************************************
*                                                                           *
*   This file is part of the LGenerics package.                             *
*   Some useful JSON utilites.                                              *
*                                                                           *
*   Copyright(c) 2024 A.Koverdyaev(avk)                                     *
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
  lgJson,
  lgStrConst;

type
  { TJsonDataHelper }
  TJsonDataHelper = class helper for TJsonData
  const
    DEF_OPTS = [joUTF8,joStrict];
    class function TryParse(const s: TJSONStringType; out aData: TJsonData;
                            const aOptions: TJSONOptions = DEF_OPTS): Boolean; static;
    class function TryParse(aStream: TStream; out aData: TJsonData;
                            const aOptions: TJSONOptions = DEF_OPTS): Boolean; static;
  { may raise an exception if it can't open the file for reading }
    class function TryParseFile(const aFileName: string; out aData: TJsonData;
                                const aOptions: TJSONOptions = DEF_OPTS): Boolean; static;
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
          if IsExactInt(d, I64) then
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

