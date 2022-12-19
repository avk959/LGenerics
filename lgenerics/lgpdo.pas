{****************************************************************************
*                                                                           *
*   This file is part of the LGenerics package.                             *
*   Plain Data Objects and serialization.                                   *
*                                                                           *
*   Copyright(c) 2022 A.Koverdyaev(avk)                                     *
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
unit lgPdo;

{$MODE OBJFPC}{$H+}

interface

uses
  SysUtils;

{ PDO - Plain Data Object - just regular record(or array of records) which fields are
  represented by numeric(note that they are stored as Double), boolean or string types
  or PDO or arrays of PDO;
  as for static arrays, only one-dimensional arrays are currently supported(arrays having more
  dimensions are written as one-dimensional) }

{ converts PDO to JSON; unsupported data types will be written as "unknown data";
  fields of unregistered records will be named as "field1, field2, ..."; it also assumes
  that the strings are UTF-8 encoded }
  function PdoToJson(aTypeInfo: Pointer; const aValue): string;
{ the type being registered must be a record; associates the field names aFieldNames with
  the record fields by their indexes; to exclude a field from serialization, it is sufficient
  to specify its name as an empty string; to avoid name mapping errors, it makes sense to use
  the $OPTIMIZATION NOORDERFIELDS directive in the record declaration }
  function RegisterPdo(aTypeInfo: Pointer; const aFieldNames: TStringArray): Boolean;
  function RegisteredPdo(aTypeInfo: Pointer; out aFieldNames: TStringArray): Boolean;
  function UnRegisterPdo(aTypeInfo: Pointer): Boolean;

implementation
{$B-}{$COPERATORS ON}{$POINTERMATH ON}
uses
  Math, TypInfo, lgUtils, lgHashMap, lgJson;

type
  TPdoField = record
    Name: string;
    Offset: Integer;
    Info: PTypeInfo;
  end;

  TPdoFieldMap  = array of TPdoField;
  TPdoCacheType = specialize TGLiteChainHashMap<Pointer, TPdoFieldMap, Pointer>;
  TPdoCache     = TPdoCacheType.TMap;

var
  GlobLock: TRtlCriticalSection;
  PdoCache: TPdoCache;

function AddPdo(aTypeInfo: Pointer; const aFieldNames: TStringArray): Boolean;
var
  pTypData: PTypeData;
  pManField: PManagedField;
  FieldMap: TPdoFieldMap;
  I, Count: Integer;
begin
  Result := False;
  if PdoCache.Contains(aTypeInfo) then exit(False);
  try
    pTypData := GetTypeData(aTypeInfo);
    Count := pTypData^.TotalFieldCount;
    System.SetLength(FieldMap, Count);
    pManField := PManagedField(
      AlignTypeData(PByte(@pTypData^.TotalFieldCount) + SizeOf(pTypData^.TotalFieldCount)));
    for I := 0 to Math.Min(Pred(Count), System.High(aFieldNames)) do
      begin
        FieldMap[I].Name := aFieldNames[I];
        FieldMap[I].Offset := pManField^.FldOffset;
        FieldMap[I].Info := pManField^.TypeRef;
        Inc(pManField);
      end;
  except
    exit(False);
  end;
  EnterCriticalSection(GlobLock);
  try
    Result := PdoCache.Add(aTypeInfo, FieldMap);
  finally
    LeaveCriticalSection(GlobLock);
  end;
end;

function GetPdoFieldMap(aTypeInfo: Pointer): TPdoFieldMap;
begin
  Result := nil;
  EnterCriticalSection(GlobLock);
  try
    PdoCache.TryGetValue(aTypeInfo, Result);
  finally
    LeaveCriticalSection(GlobLock);
  end;
end;

function RegisterPdo(aTypeInfo: Pointer; const aFieldNames: TStringArray): Boolean;
begin
  if aTypeInfo = nil then exit(False);
  if PTypeInfo(aTypeInfo)^.Kind <> tkRecord then exit(False);
  Result := AddPdo(aTypeInfo, aFieldNames);
end;

function RegisteredPdo(aTypeInfo: Pointer; out aFieldNames: TStringArray): Boolean;
var
  FieldMap: TPdoFieldMap;
  I: Integer;
begin
  aFieldNames := nil;
  EnterCriticalSection(GlobLock);
  try
    Result := PdoCache.TryGetValue(aTypeInfo, FieldMap);
  finally
    LeaveCriticalSection(GlobLock);
  end;
  if Result then
    begin
      System.SetLength(aFieldNames, System.Length(FieldMap));
      for I := 0 to System.High(FieldMap) do
        aFieldNames[I] := FieldMap[I].Name;
    end;
end;

function UnRegisterPdo(aTypeInfo: Pointer): Boolean;
begin
  EnterCriticalSection(GlobLock);
  try
    Result := PdoCache.Remove(aTypeInfo);
  finally
    LeaveCriticalSection(GlobLock);
  end;
end;

const
  UnknownData = 'unknown data';
  FieldAlias  = 'field';
  SupportedKinds = [
    tkInteger, tkChar, tkFloat, tkSString, tkLString, tkAString, tkWString, tkArray,
    tkRecord, tkWChar, tkBool, tkInt64, tkQWord, tkDynArray, tkUString, tkUChar];

function PdoToJson(aTypeInfo: Pointer; const aValue): string;
var
  Writer: TJsonStrWriter;
  procedure WriteInteger(aTypData: PTypeData; aData: Pointer); inline;
  var
    d: Double;
  begin
    case aTypData^.OrdType of
      otSByte:  d := PShortInt(aData)^;
      otUByte:  d := PByte(aData)^;
      otSWord:  d := PSmallInt(aData)^;
      otUWord:  d := PWord(aData)^;
      otSLong:  d := PLongInt(aData)^;
      otULong:  d := PDword(aData)^;
      otSQWord: d := PInt64(aData)^;
      otUQWord: d := PQWord(aData)^;
    end;
    Writer.Add(d);
  end;
  procedure WriteFloat(aTypData: PTypeData; aData: Pointer); inline;
  var
    d: Double;
  begin
    case aTypData^.FloatType of
      ftSingle:   d := PSingle(aData)^;
      ftDouble:   d := PSingle(aData)^;
      ftExtended: d := PExtended(aData)^;
      ftComp:     d := PComp(aData)^;
      ftCurr:     d := PCurrency(aData)^;
    end;
    Writer.Add(d);
  end;
  procedure WriteBool(aTypData: PTypeData; aData: Pointer); inline;
  var
    IsTrue: Boolean;
  begin
    case aTypData^.OrdType of
      otSByte:  IsTrue := PShortInt(aData)^ > 0;
      otUByte:  IsTrue := PByte(aData)^ > 0;
      otSWord:  IsTrue := PSmallInt(aData)^ > 0;
      otUWord:  IsTrue := PWord(aData)^ > 0;
      otSLong:  IsTrue := PLongInt(aData)^ > 0;
      otULong:  IsTrue := PDword(aData)^ > 0;
      otSQWord: IsTrue := PInt64(aData)^ > 0;
      otUQWord: IsTrue := PQWord(aData)^ > 0;
    end;
    if IsTrue then
      Writer.AddTrue
    else
      Writer.AddFalse;
  end;
  procedure WriteField(aTypeInfo, aData: Pointer); forward;
  procedure WriteRegRecord(aData: Pointer; const aFieldMap: TPdoFieldMap);
  var
    I: Integer;
  begin
    Writer.BeginObject;
    for I := 0 to System.High(aFieldMap) do
      if aFieldMap[I].Name <> '' then
        begin
          Writer.AddName(aFieldMap[I].Name);
          WriteField(aFieldMap[I].Info, PByte(aData) + aFieldMap[I].Offset);
        end;
    Writer.EndObject;
  end;
  procedure WriteUnregRecord(aTypeInfo, aData: Pointer);
  var
    pTypData: PTypeData;
    pManField: PManagedField;
    I: Integer;
  begin
    pTypData := GetTypeData(aTypeInfo);
    pManField := PManagedField(
      AlignTypeData(PByte(@pTypData^.TotalFieldCount) + SizeOf(pTypData^.TotalFieldCount)));
    Writer.BeginObject;
    for I := 0 to Pred(pTypData^.TotalFieldCount) do
      begin
        Writer.AddName(FieldAlias + Succ(I).ToString);
        WriteField(pManField^.TypeRef, PByte(aData) + pManField^.FldOffset);
        Inc(pManField);
      end;
    Writer.EndObject;
  end;
  procedure WriteRecord(aTypeInfo, aData: Pointer); inline;
  var
    FieldMap: TPdoFieldMap;
  begin
    FieldMap := GetPdoFieldMap(aTypeInfo);
    if FieldMap <> nil then
      begin
        WriteRegRecord(aData, FieldMap);
        exit;
      end;
    WriteUnregRecord(aTypeInfo, aData);
  end;
  procedure WriteArray(aTypeInfo, aData: Pointer);
  var
    FieldMap: TPdoFieldMap;
    pTypData: PTypeData;
    ElSize: SizeUInt;
    ElType: PTypeInfo;
    Arr: PByte;
    I, Count: SizeInt;
  begin
    pTypData := GetTypeData(aTypeInfo);
    ElType := pTypData^.ArrayData.ElType;
    Count := pTypData^.ArrayData.ElCount;
    ElSize := pTypData^.ArrayData.Size div Count;
    Arr := aData;
    Writer.BeginArray;
    try
      FieldMap := GetPdoFieldMap(ElType);
      if FieldMap <> nil then
        begin
          for I := 0 to Pred(Count) do
            begin
              WriteRegRecord(Arr, FieldMap);
              Arr += ElSize;
            end;
          exit;
        end;
      for I := 0 to Pred(Count) do
        begin
          WriteField(ElType, Arr);
          Arr += ElSize;
        end;
    finally
      Writer.EndArray;
    end;
  end;
  procedure WriteDynArray(aTypeInfo, aData: Pointer);
  var
    FieldMap: TPdoFieldMap;
    pTypData: PTypeData;
    ElSize: SizeUInt;
    ElType: PTypeInfo;
    Arr: PByte;
    I: SizeInt;
  begin
    pTypData := GetTypeData(aTypeInfo);
    ElSize := pTypData^.elSize;
    ElType := pTypData^.ElType2;
    Arr := Pointer(aData^);
    Writer.BeginArray;
    try
      FieldMap := GetPdoFieldMap(ElType);
      if FieldMap <> nil then
        begin
          for I := 0 to Pred(DynArraySize(Arr)) do
            begin
              WriteRegRecord(Arr, FieldMap);
              Arr += ElSize;
            end;
          exit;
        end;
      for I := 0 to Pred(DynArraySize(Arr)) do
        begin
          WriteField(ElType, Arr);
          Arr += ElSize;
        end;
    finally
      Writer.EndArray;
    end;
  end;
  procedure WriteField(aTypeInfo, aData: Pointer);
  var
    pTypData: PTypeData;
  begin
    if not (PTypeInfo(aTypeInfo)^.Kind in SupportedKinds) then //todo: tkVariant ???
      begin
        Writer.Add(UnknownData);
        exit;
      end;
    pTypData := GetTypeData(PTypeInfo(aTypeInfo));
    case PTypeInfo(aTypeInfo)^.Kind of
      tkInteger, tkInt64, tkQWord:
        WriteInteger(pTypData, aData);
      tkFloat:
        WriteFloat(pTypData, aData);
      tkChar:
        Writer.Add(string(PChar(aData)^));
      tkSString:
        Writer.Add(PShortString(aData)^);
      tkLString, tkAString:
        Writer.Add(PString(aData)^);  ////////////
      tkWString:
        Writer.Add(string(PWideString(aData)^)); //////////////
      tkArray:
        WriteArray(aTypeInfo, aData);
      tkRecord:
        WriteRecord(aTypeInfo, aData);
      tkWChar:
        Writer.Add(string(widestring(PWideChar(aData)^))); //////////
      tkBool:
        WriteBool(pTypData, aData);
      tkDynArray:
        WriteDynArray(aTypeInfo, aData);
      tkUString:
        Writer.Add(string(PUnicodeString(aData)^)); ///////////
      tkUChar:
        Writer.Add(string(unicodestring(PUnicodeChar(aData)^)));/////////
    else
    end;
  end;
var
  WriterRef: specialize TGAutoRef<TJsonStrWriter>;
begin
  Result := '';
  if aTypeInfo = nil then exit;
  Writer := WriterRef;
  WriteField(aTypeInfo, @aValue);
  Result := Writer.JsonString;
end;

initialization
  InitCriticalSection(GlobLock);
finalization
  DoneCriticalSection(GlobLock);
end.

