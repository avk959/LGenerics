{****************************************************************************
*                                                                           *
*   This file is part of the LGenerics package.                             *
*   Plain Data Objects serialization.                                       *
*                                                                           *
*   Copyright(c) 2022-2026 A.Koverdyaev(avk)                                *
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

{$MODE OBJFPC}{$H+}{$INLINE ON}

interface

uses
  Classes, SysUtils, TypInfo, lgUtils, lgHelpers, lgJson, LgMsgPack;

{ PDO - Plain Data Objects is a conventional collective name for Pascal data structures
  that can be user-transparently(well, almost) stored/loaded to/from another format,
  currently only JSON and MsgPack:
   - numeric, boolean or string types, some limited support of Variant;
   - enumerations;
   - sets;
   - regular records, it is possible to register a list of field names or
       register custom callback for JSON;
       use MsgPack extension;
   - classes, using published properties or by registering a custom callback(TStrings and
     TCollection as a special case) or using MsgPack extension;
   - objects(only by registering a serialization callback or using MsgPack extension);
   - static arrays of PDO(only one-dimensional, multidimensional arrays are written as one-dimensional);
   - dynamic arrays of PDO;
   - variant arrays(currently only one-dimensional);
}

const
  SUPPORT_KINDS = [
    tkInteger, tkChar, tkEnumeration, tkFloat, tkSet, tkSString, tkLString,
    tkAString, tkWString, tkVariant, tkArray, tkRecord, tkClass, tkObject,
    tkWChar, tkBool, tkInt64, tkQWord, tkDynArray, tkUString, tkUChar];

  DEFAULT_DEPTH = TJsonReader.DEF_DEPTH;
  DEFAULT_LEN   = lgJson.TJsonStrWriter.DEFAULT_LEN;

type
  EPdoStoreJson = class(Exception);

{ saves PDO in JSON format; unregistered records will be written as unnamed tuples;
  raises an exception if an unsupported type is encountered }
  function PdoToJson(aTypeInfo: PTypeInfo; const aValue; aInitWriterLen: Integer = DEFAULT_LEN): string;
  generic function PdoToJson<T>(const aValue: T; aInitWriterLen: Integer = DEFAULT_LEN): string;

  { the type being registered must be a record; associates the field names aFieldNames with
    the record fields by their indexes; to exclude a field from serialization, it is sufficient
    to specify its name as an empty string; returns True on successful registration;
    has a higher priority than custom callbacks }
  function RegisterRecordFields(aTypeInfo: PTypeInfo; const aFieldNames: TStringArray): Boolean;
  function RecordFieldsRegistered(aTypeInfo: PTypeInfo; out aFieldNames: TStringArray): Boolean;
  function UnregisterRecordFields(aTypeInfo: PTypeInfo): Boolean;
  function UnregisterPdo(aTypeInfo: PTypeInfo): Boolean;

type
  TJsonStrWriter = lgJson.TJsonStrWriter;
  TPdoToJsonProc = procedure(p: Pointer; aWriter: TJsonStrWriter);

{ associates a custom JSON serialization routine with a type, which must be a record, class or object;
  only one routine can be associated with each type; returns True if registration is successful }
  function RegisterPdoToJsonProc(aTypeInfo: PTypeInfo; aProc: TPdoToJsonProc): Boolean;
  function PdoToJsonProcRegistered(aTypeInfo: PTypeInfo): Boolean;
  function UnregisterPdoToJsonProc(aTypeInfo: PTypeInfo): Boolean;

type
  EPdoLoadJson = class(Exception)
  private
    FPath: string;
  public
    constructor Create(const aMessage, aPath: string);
    property JsonPath: string read FPath;
  end;

  TJsonReadOption  = (
    jroSkipUnknownProps,   { properties not found will be silently ignored; by default an exception will be raised }
    jroIgnoreNameCase,     { property names will be treated as case insensitive; case sensitive by default }
    jroRejectNulls,        { an exception will be raised in case of JSON.null value; by default null values
                             are accepted for sets, classes, strings, dynamic arrays, and Variants }
    jroTryCreateClassInst);{ will try to call a constructor for an unassigned class instance;
                             raises an exception by default }
  TJsonReadOptions = set of TJsonReadOption;

{ loads PDO directly from JSON; records and objects MUST have some form of registration;
  raises EPdoLoadJson in any unexpected cases }
  generic procedure PdoLoadJson<T>(var aValue: T; const aJson: string; const aOptions: TJsonReadOptions = [];
                                   aMaxDepth: Integer = DEFAULT_DEPTH; aSkipBom: Boolean = False);

  procedure PdoLoadJson(aTypeInfo: PTypeInfo; var aValue; const aJson: string; const aOptions: TJsonReadOptions = [];
                        aMaxDepth: Integer = DEFAULT_DEPTH; aSkipBom: Boolean = False);

type
{ if the callback returns False then EPdoLoadJson will be raised }
  TJsonToPdoProc = function(p: Pointer; aReader: TJsonReader; const aOpts: TJsonReadOptions): Boolean;

  function RegisterJsonToPdoProc(aTypeInfo: PTypeInfo; aProc: TJsonToPdoProc): Boolean;
  function JsonToPdoProcRegistered(aTypeInfo: PTypeInfo): Boolean;
  function UnregisterJsonToPdoProc(aTypeInfo: PTypeInfo): Boolean;

  function RegisterPdoJsonProcs(aTypeInfo: PTypeInfo; aWriteProc: TPdoToJsonProc;
                                aReadProc: TJsonToPdoProc): Boolean;


type
  EPdoToMsgPack = class(Exception);

{ saves a PDO to a string in MessagePack format;
  raises an exception if an unsupported type is encountered }
  generic function PdoToMsgPackStr<T>(const aValue: T; aUserExt: IMpUserExt = nil): rawbytestring;
  function PdoToMsgPackStr(aTypeInfo: PTypeInfo; const aValue; aUserExt: IMpUserExt = nil): rawbytestring;

{ saves a PDO to a byte array in MessagePack format;
  raises an exception if an unsupported type is encountered }
  generic function PdoToMsgPack<T>(const aValue: T; aUserExt: IMpUserExt = nil): TBytes;
  function PdoToMsgPack(aTypeInfo: PTypeInfo; const aValue; aUserExt: IMpUserExt = nil): TBytes;
{  }
  procedure Pdo2MsgPack(aTypeInfo: PTypeInfo; const aValue; aWriter: TMpCustomWriter; aUserExt: IMpUserExt);

type
  EPdoLoadMsgPack = class(Exception);

{ loads PDO directly from the MessagePack string buffer; raises EPdoLoadMsgPack in any unexpected cases }
  generic procedure PdoLoadMsgPack<T>(var aValue: T; const aBuffer: rawbytestring; aUserExt: IMpUserExt = nil;
                                      aMaxDepth: Integer = DEFAULT_DEPTH);
  procedure PdoLoadMsgPack(aTypeInfo: PTypeInfo; var aValue; const aBuffer: rawbytestring;
                           aUserExt: IMpUserExt = nil; aMaxDepth: Integer = DEFAULT_DEPTH);

{ loads PDO directly from the MessagePack buffer; raises EPdoLoadMsgPack in any unexpected cases }
  generic procedure PdoLoadMsgPack<T>(var aValue: T; const aBuffer: TBytes; aUserExt: IMpUserExt = nil;
                                      aMaxDepth: Integer = DEFAULT_DEPTH);
  procedure PdoLoadMsgPack(aTypeInfo: PTypeInfo; var aValue; const aBuffer: TBytes; aUserExt: IMpUserExt = nil;
                           aMaxDepth: Integer = DEFAULT_DEPTH);
{  }
  procedure MsgPack2Pdo(aTypeInfo: PTypeInfo; var aValue; aReader: TMpCustomReader; aUserExt: IMpUserExt);

implementation
{$B-}{$COPERATORS ON}{$POINTERMATH ON}
uses
  Math, Variants, lgHashMap, lgStrConst;

type
  TRecField = record
    Name: string;
    Offset: Integer;
    Info: PTypeInfo;
  end;

  TRecFieldMap = array of TRecField;

  TPdoCacheEntry = record
    FieldMap: TRecFieldMap;
    PdoToJsonProc,
    JsonToPdoProc: Pointer;
  end;
  PPdoCacheEntry = ^TPdoCacheEntry;
  TPdoCacheType = specialize TGLiteChainHashMap<Pointer, TPdoCacheEntry, Pointer>;
  TPdoCache     = TPdoCacheType.TMap;
{$PUSH}{$J-}
const
  DEF_CACHE_ENTRY: TPdoCacheEntry = (FieldMap: nil; PdoToJsonProc: nil; JsonToPdoProc: nil);
{$POP}
  CUSTOM_KINDS = [tkRecord, tkClass, tkObject];

var
  GlobLock: IReadWriteSync;
  PdoCache: TPdoCache;

function AddPdo(aTypeInfo: Pointer; const aFieldNames: TStringArray): Boolean;
var
  pe: PPdoCacheEntry;
  pTypData: PTypeData;
  pManField: PManagedField;
  FieldMap: TRecFieldMap = nil;
  I, Count: Integer;
begin
  Result := False;
  GlobLock.BeginWrite;
  try
    pe := PdoCache.GetMutValueDef(aTypeInfo, DEF_CACHE_ENTRY);
    if pe^.FieldMap <> nil then exit;
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
    pe^.FieldMap := FieldMap;
    Result := True;
  finally
    GlobLock.EndWrite;
  end;
end;

function GetGlobPdoEntry(aTypeInfo: Pointer; out e: TPdoCacheEntry): Boolean;
var
  pce: PPdoCacheEntry;
begin
  GlobLock.BeginRead;
  try
    Result := PdoCache.TryGetMutValue(aTypeInfo, pce);
    if Result then begin
      e.FieldMap := System.Copy(pce^.FieldMap);
      e.PdoToJsonProc := pce^.PdoToJsonProc;
      e.JsonToPdoProc := pce^.JsonToPdoProc;
    end;
  finally
    GlobLock.EndRead;
  end;
end;

function RegisterRecordFields(aTypeInfo: PTypeInfo; const aFieldNames: TStringArray): Boolean;
begin
  if (aTypeInfo = nil) or (aTypeInfo^.Kind <> tkRecord) or (aFieldNames = nil) then
    exit(False);
  Result := AddPdo(aTypeInfo, aFieldNames);
end;

function RecordFieldsRegistered(aTypeInfo: PTypeInfo; out aFieldNames: TStringArray): Boolean;
var
  pe: PPdoCacheEntry;
  I: Integer;
begin
  pe := nil;
  GlobLock.BeginRead;
  try
    if not(PdoCache.TryGetMutValue(aTypeInfo, pe) and (pe^.FieldMap <> nil)) then exit(False);
    System.SetLength(aFieldNames, System.Length(pe^.FieldMap));
    for I := 0 to System.High(pe^.FieldMap) do
      aFieldNames[I] := pe^.FieldMap[I].Name;
  finally
    GlobLock.EndRead;
  end;
  Result := True;
end;

function GetRecFieldMap(aTypeInfo: Pointer; out aMap: TRecFieldMap): Boolean;
var
  pe: PPdoCacheEntry;
begin
  pe := nil;
  GlobLock.BeginRead;
  try
    if not(PdoCache.TryGetMutValue(aTypeInfo, pe) and (pe^.FieldMap <> nil)) then exit(False);
    aMap := pe^.FieldMap;
  finally
    GlobLock.EndRead;
  end;
  Result := True;
end;

function UnregisterRecordFields(aTypeInfo: PTypeInfo): Boolean;
var
  pe: PPdoCacheEntry;
begin
  pe := nil;
  GlobLock.BeginWrite;
  try
    if not(PdoCache.TryGetMutValue(aTypeInfo, pe) and (pe^.FieldMap <> nil)) then exit(False);
    pe^.FieldMap := nil;
    if (pe^.PdoToJsonProc = nil) and (pe^.JsonToPdoProc = nil) then PdoCache.Remove(aTypeInfo);
  finally
    GlobLock.EndWrite;
  end;
  Result := True;
end;

function UnregisterPdo(aTypeInfo: PTypeInfo): Boolean;
begin
  GlobLock.BeginWrite;
  try
    Result := PdoCache.Remove(aTypeInfo);
  finally
    GlobLock.EndWrite;
  end;
end;

function RegisterPdoToJsonProc(aTypeInfo: PTypeInfo; aProc: TPdoToJsonProc): Boolean;
var
  pe: PPdoCacheEntry;
begin
  if (aTypeInfo = nil) or not(aTypeInfo^.Kind in CUSTOM_KINDS) then exit(False);
  if aProc = nil then exit(False);
  GlobLock.BeginWrite;
  try
    pe := PdoCache.GetMutValueDef(aTypeInfo, DEF_CACHE_ENTRY);
    Result := pe^.PdoToJsonProc = nil;
    if Result then pe^.PdoToJsonProc := aProc;
  finally
    GlobLock.EndWrite;
  end;
end;

function PdoToJsonProcRegistered(aTypeInfo: PTypeInfo): Boolean;
var
  pe: PPdoCacheEntry;
begin
  if not(aTypeInfo^.Kind in CUSTOM_KINDS) then exit(False);
  pe := nil;
  GlobLock.BeginRead;
  try
    Result := PdoCache.TryGetMutValue(aTypeInfo, pe) and (pe^.PdoToJsonProc <> nil);
  finally
    GlobLock.EndRead;
  end;
end;

function UnregisterPdoToJsonProc(aTypeInfo: PTypeInfo): Boolean;
var
  pe: PPdoCacheEntry;
begin
  if not(aTypeInfo^.Kind in CUSTOM_KINDS) then exit(False);
  pe := nil;
  GlobLock.BeginWrite;
  try
    if not(PdoCache.TryGetMutValue(aTypeInfo, pe) and (pe^.PdoToJsonProc <> nil)) then exit(False);
    pe^.PdoToJsonProc := nil;
    if (pe^.FieldMap = nil) and (pe^.JsonToPdoProc = nil) then PdoCache.Remove(aTypeInfo);
  finally
    GlobLock.EndWrite;
  end;
end;

type
  TPdoEntryInfo = record
    Data: TPdoCacheEntry;
    Tested,
    Registered: Boolean;
  end;
  PPdoEntryInfo      = ^TPdoEntryInfo;
  TLocalPdoCacheType = specialize TGLiteHashMapLP<Pointer, TPdoEntryInfo, Pointer>;
  TLocalPdoCache     = TLocalPdoCacheType.TMap;

{$PUSH}{$J-}
const
  DEF_ENTRY_INFO: TPdoEntryInfo = (
    Data: (FieldMap: nil; PdoToJsonProc: nil; JsonToPdoProc: nil);
    Tested: False; Registered: False
  );
{$POP}


{$PUSH}{$WARN 5089 OFF : Local variable "$1" of a managed type does not seem to be initialized}
function PdoToJson(aTypeInfo: PTypeInfo; const aValue; aInitWriterLen: Integer): string;
var
  Writer: TJsonStrWriter;
  procedure WriteInteger(aTypData: PTypeData; aData: Pointer); inline;
  var
    d: Double;
  begin
    d := 0;
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
  function GetOrdValue(aTypData: PTypeData; aData: Pointer): Int64; inline;
  begin
    Result := 0;
    case aTypData^.OrdType of
      otSByte:  Result := PShortInt(aData)^;
      otUByte:  Result := Int64(PByte(aData)^);
      otSWord:  Result := PSmallInt(aData)^;
      otUWord:  Result := Int64(PWord(aData)^);
      otSLong:  Result := PLongInt(aData)^;
      otULong:  Result := Int64(PDword(aData)^);
      otSQWord: Result := PInt64(aData)^;
      otUQWord: Result := Int64(PQWord(aData)^);
    end;
  end;
  procedure WriteFloat(aTypData: PTypeData; aData: Pointer); inline;
  var
    d: Double;
  begin
    d := 0;
    case aTypData^.FloatType of
      ftSingle:   d := PSingle(aData)^;
      ftDouble:   d := PDouble(aData)^;
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
    IsTrue := False;
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

var
  LocalPdoCache: TLocalPdoCache;

  function GetPdoEntry(aTypeInfo: PTypeInfo; out e: TPdoCacheEntry): Boolean;
  var
    p: PPdoEntryInfo;
    pce: TPdoCacheEntry;
  begin
    p := LocalPdoCache.GetMutValueDef(aTypeInfo, DEF_ENTRY_INFO);
    if not p^.Tested then begin
      if GetGlobPdoEntry(aTypeInfo, pce) then begin
        p^.Data := pce;
        p^.Registered := True;
      end;
      p^.Tested := True;
    end;
    Result := p^.Registered;
    if Result then e := p^.Data;
  end;

  procedure WriteField(aTypeInfo: PTypeInfo; aData: Pointer); forward;
  procedure WriteRegRecord(aData: Pointer; const aFieldMap: TRecFieldMap);
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
  procedure WriteUnregRecord(aTypeInfo: PTypeInfo; aData: Pointer);
  var
    pTypData: PTypeData;
    pManField: PManagedField;
    I: Integer;
  begin
    pTypData := GetTypeData(aTypeInfo);
    pManField := PManagedField(
      AlignTypeData(PByte(@pTypData^.TotalFieldCount) + SizeOf(pTypData^.TotalFieldCount)));
    Writer.BeginArray;
    for I := 0 to Pred(pTypData^.TotalFieldCount) do
      begin
        WriteField(pManField^.TypeRef, PByte(aData) + pManField^.FldOffset);
        Inc(pManField);
      end;
    Writer.EndArray;
  end;
  procedure WriteRecord(aTypeInfo: PTypeInfo; aData: Pointer); inline;
  var
    e: TPdoCacheEntry;
  begin
    if GetPdoEntry(aTypeInfo, e) then
      if e.FieldMap <> nil then begin
        WriteRegRecord(aData, e.FieldMap);
        exit;
      end else
        if e.PdoToJsonProc <> nil then begin
          TPdoToJsonProc(e.PdoToJsonProc)(aData, Writer);
          exit;
        end;
    WriteUnregRecord(aTypeInfo, aData);
  end;
  procedure WriteArray(aTypeInfo: PTypeInfo; aData: Pointer);
  var
    e: TPdoCacheEntry;
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
    if (ElType^.Kind in CUSTOM_KINDS) and GetPdoEntry(ElType, e) then
      if e.FieldMap <> nil then begin
        for I := 0 to Pred(Count) do begin
          WriteRegRecord(Arr, e.FieldMap);
          Arr += ElSize;
        end;
        Writer.EndArray;
        exit;
      end else
        if e.PdoToJsonProc <> nil then begin
          for I := 0 to Pred(Count) do begin
            TPdoToJsonProc(e.PdoToJsonProc)(Arr, Writer);
            Arr += ElSize;
          end;
          Writer.EndArray;
          exit;
        end;
    for I := 0 to Pred(Count) do
      begin
        WriteField(ElType, Arr);
        Arr += ElSize;
      end;
    Writer.EndArray;
  end;
  procedure WriteDynArray(aTypeInfo: PTypeInfo; aData: Pointer);
  var
    e: TPdoCacheEntry;
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
    if (ElType^.Kind in CUSTOM_KINDS) and GetPdoEntry(ElType, e) then
      if e.FieldMap <> nil then begin
        for I := 0 to Pred(DynArraySize(Arr)) do begin
          WriteRegRecord(Arr, e.FieldMap);
          Arr += ElSize;
        end;
        Writer.EndArray;
        exit;
      end else
        if e.PdoToJsonProc <> nil then begin
          for I := 0 to Pred(DynArraySize(Arr)) do
            begin
              TPdoToJsonProc(e.PdoToJsonProc)(Arr, Writer);
              Arr += ElSize;
            end;
          Writer.EndArray;
          exit;
        end;
    for I := 0 to Pred(DynArraySize(Arr)) do
      begin
        WriteField(ElType, Arr);
        Arr += ElSize;
      end;
    Writer.EndArray;
  end;
  procedure WriteVariant(const v: Variant);
  var
    I: SizeInt;
    d: Double;
  begin
    if VarIsEmpty(v) or VarIsNull(v) then
      begin
        Writer.AddNull;
        exit;
      end;
    if VarIsArray(v) then
      begin
        Writer.BeginArray;
        for I := VarArrayLowBound(v, 1) to VarArrayHighBound(v, 1) do
          WriteVariant(v[I]);
        Writer.EndArray;
        exit;
      end;
    case VarType(v) of
      varSmallInt, varInteger, varSingle, varShortInt, varDouble, varCurrency,
      varDate, varByte, varWord, varLongWord, varInt64, varQWord:
        begin
          d := v;
          Writer.Add(d);
        end;
      varBoolean:
        if Boolean(v) then
          Writer.AddTrue
        else
          Writer.AddFalse;
      varOleStr,
      varString,
      varUString:
        Writer.Add(string(v));
    else
      raise EPdoStoreJson.CreateFmt(SEVariantNotSupportFmt, [VarType(v)]);
    end;
  end;
  procedure WriteSet(aTypeInfo: PTypeInfo; aData: Pointer); forward;
  procedure WriteClass(aTypeInfo: PTypeInfo; o: TObject); forward;
  procedure NotSupported(aTypeInfo: PTypeInfo);
  begin
    raise EPdoStoreJson.CreateFmt(SEPdoTypeNotSupportFmt,
      [aTypeInfo^.Name, GetEnumName(TypeInfo(TTypeKind), Integer(aTypeInfo^.Kind))]);
  end;
  procedure WriteClassProp(o: TObject; aPropInfo: PPropInfo);
  var
    pTypInfo: PTypeInfo;
    I: Int64;
    e: Extended;
    p: Pointer;
  begin
    pTypInfo := aPropInfo^.PropType;
    case pTypInfo^.Kind of
      tkInteger, tkChar, tkEnumeration, tkWChar, tkBool, tkInt64, tkQWord, tkUChar:
        begin
          I := GetOrdProp(o, aPropInfo);
          WriteField(pTypInfo, @I);
        end;
      tkFloat:
        begin
          e := GetFloatProp(o, aPropInfo);
          WriteField(pTypInfo, @e);
        end;
      tkSet:
         begin
           I := GetOrdProp(o, aPropInfo);
           WriteSet(pTypInfo, @I);
         end;
      tkSString, tkLString, tkAString:
        begin
          p := Pointer(GetStrProp(o, aPropInfo));
          WriteField(pTypInfo, @p);
        end;
      tkWString:
        begin
          p := Pointer(GetWideStrProp(o, aPropInfo));
          WriteField(pTypInfo, @p);
        end;
      tkVariant: WriteVariant(GetVariantProp(o, aPropInfo));
      tkClass: WriteClass(pTypInfo, GetObjectProp(o, aPropInfo));
      tkDynArray:
        begin
          p := GetDynArrayProp(o, aPropInfo);
          WriteField(pTypInfo, @p);
        end;
      tkUString:
        begin
          p := Pointer(GetUnicodeStrProp(o, aPropInfo));
          WriteField(pTypInfo, @p);
        end;
    else
      NotSupported(pTypInfo);
    end;
  end;
  procedure WriteClass(aTypeInfo: PTypeInfo; o: TObject);
  var
    I, J, PropCount: SizeInt;
    c: TCollection;
    pProps: PPropList;
    pInfo: PPropInfo;
    e: TPdoCacheEntry;
  begin
    if o = nil then
      Writer.AddNull
    else
      if GetPdoEntry(aTypeInfo, e) and (e.PdoToJsonProc <> nil) then
        TPdoToJsonProc(e.PdoToJsonProc)(o, Writer)
      else
        if o is TCollection then begin
          c := TCollection(o);
          if c.Count = 0 then begin
            Writer.BeginArray;
            Writer.EndArray;
          end else begin
            Writer.BeginArray;
            PropCount := GetPropList(c.ItemClass, pProps);
            try
              for I := 0 to Pred(c.Count) do begin
                Writer.BeginObject;
                for J := 0 to Pred(PropCount) do begin
                  pInfo := pProps^[J];
                  if not IsReadableProp(pInfo) then continue;
                  Writer.AddName(pInfo^.Name);
                  WriteClassProp(c.Items[I], pInfo);
                end;
                Writer.EndObject;
              end;
            finally
              FreeMem(pProps);
            end;
            Writer.EndArray;
          end;
        end else
          if o is TStrings then begin
            Writer.BeginArray;
            with TStrings(o) do
              for I := 0 to Pred(Count) do
                Writer.Add(Strings[I]);
            Writer.EndArray;
          end else begin
            PropCount := GetPropList(o, pProps);
            try
              Writer.BeginObject;
              for I := 0 to Pred(PropCount) do begin
                pInfo := pProps^[I];
                if not IsReadableProp(pInfo) then continue;
                Writer.AddName(pInfo^.Name);
                WriteClassProp(o, pInfo);
              end;
              Writer.EndObject;
            finally
              FreeMem(pProps);
            end;
          end;
  end;
  procedure WriteObject(aTypeInfo, aData: Pointer); inline;
  var
    e: TPdoCacheEntry;
  begin
    if GetPdoEntry(aTypeInfo, e) and (e.PdoToJsonProc <> nil) then
      TPdoToJsonProc(e.PdoToJsonProc)(aData, Writer)
    else
      NotSupported(aTypeInfo);
  end;
  procedure WriteSet(aTypeInfo: PTypeInfo; aData: Pointer);
  var
    I, CurrVal, Offset, RestBytes, CurrSize: Integer;
    pElType: PTypeInfo;
    pIntData: PInteger;
    ElKind: TTypeKind;
    s: string[3];
  begin
    with GetTypeData(aTypeInfo)^ do
      begin
        pElType := CompType;
        RestBytes := SetSize;
        ElKind := CompType^.Kind;
      end;
    pIntData := aData;
    Offset := 0;
    Writer.BeginArray;
    while RestBytes > 0 do
      begin
        CurrSize := Math.Min(RestBytes, SizeOf(Integer));
        for I := 0 to Pred(CurrSize * 8) do            //todo: BsfDWord() ???
          if (Integer(1) shl I) and pIntData^ <> 0 then
            begin
              CurrVal := I + Offset;
              case ElKind of
                tkChar: begin
                    s := Char(CurrVal);
                    Writer.Add(s);
                  end;
                tkEnumeration: Writer.Add(GetEnumName(pElType, CurrVal));
              else
                Writer.Add(CurrVal);
              end;
            end;
        RestBytes -= CurrSize;
        Offset += CurrSize * 8;
        Inc(pIntData);
      end;
    Writer.EndArray;
  end;
  procedure WriteField(aTypeInfo: PTypeInfo; aData: Pointer);
  begin
    case aTypeInfo^.Kind of
      tkInteger, tkInt64,
      tkQWord:              WriteInteger(GetTypeData(aTypeInfo), aData);
      tkChar:               Writer.Add(string(PChar(aData)^)); //////////////
      tkEnumeration:        Writer.Add(GetEnumName(aTypeInfo, GetOrdValue(GetTypeData(aTypeInfo), aData)));
      tkFloat:              WriteFloat(GetTypeData(aTypeInfo), aData);
      tkSet:                WriteSet(aTypeInfo, aData);
      tkSString:            Writer.Add(PShortString(aData)^);
      tkLString, tkAString: Writer.Add(PString(aData)^);  ////////////
      tkWString:            Writer.Add(string(PWideString(aData)^)); //////////////
      tkVariant:            WriteVariant(PVariant(aData)^);
      tkArray:              WriteArray(aTypeInfo, aData);
      tkRecord:             WriteRecord(aTypeInfo, aData);
      tkClass:              WriteClass(aTypeInfo, TObject(aData^));
      tkObject:             WriteObject(aTypeInfo, aData);
      tkWChar:              Writer.Add(string(widestring(PWideChar(aData)^))); //////////
      tkBool:               WriteBool(GetTypeData(aTypeInfo), aData);
      tkDynArray:           WriteDynArray(aTypeInfo, aData);
      tkUString:            Writer.Add(string(PUnicodeString(aData)^)); ///////////
      tkUChar:              Writer.Add(string(unicodestring(PUnicodeChar(aData)^)));/////////
    else
      NotSupported(aTypeInfo);
    end;
  end;
var
  WriterRef: specialize TGUniqRef<TJsonStrWriter>;
begin
  Result := '';
  if aTypeInfo = nil then exit;
  WriterRef.Instance := TJsonStrWriter.Create(aInitWriterLen);
  Writer := WriterRef;
  WriteField(aTypeInfo, @aValue);
  Result := Writer.JsonString;
end;
{$POP}

generic function PdoToJson<T>(const aValue: T; aInitWriterLen: Integer): string;
begin
  Result := PdoToJson(TypeInfo(T), aValue, aInitWriterLen);
end;

{ EPdoLoadJson }
constructor EPdoLoadJson.Create(const aMessage, aPath: string);
begin
  inherited Create(aMessage);
  FPath := aPath;
end;

function RegisterJsonToPdoProc(aTypeInfo: PTypeInfo; aProc: TJsonToPdoProc): Boolean;
var
  pe: PPdoCacheEntry;
begin
  if (aTypeInfo = nil) or not(aTypeInfo^.Kind in CUSTOM_KINDS) then exit(False);
  if aProc = nil then exit(False);
  GlobLock.BeginWrite;
  try
    pe := PdoCache.GetMutValueDef(aTypeInfo, DEF_CACHE_ENTRY);
    Result := pe^.JsonToPdoProc = nil;
    if Result then pe^.JsonToPdoProc := aProc;
  finally
    GlobLock.EndWrite;
  end;
end;

function JsonToPdoProcRegistered(aTypeInfo: PTypeInfo): Boolean;
var
  pe: PPdoCacheEntry;
begin
  if not(aTypeInfo^.Kind in CUSTOM_KINDS) then exit(False);
  pe := nil;
  GlobLock.BeginRead;
  try
    Result := PdoCache.TryGetMutValue(aTypeInfo, pe) and (pe^.JsonToPdoProc <> nil);
  finally
    GlobLock.EndRead;
  end;
end;

function UnregisterJsonToPdoProc(aTypeInfo: PTypeInfo): Boolean;
var
  pe: PPdoCacheEntry;
begin
  if not(aTypeInfo^.Kind in CUSTOM_KINDS) then exit(False);
  pe := nil;
  GlobLock.BeginWrite;
  try
    if not(PdoCache.TryGetMutValue(aTypeInfo, pe) and (pe^.JsonToPdoProc <> nil)) then exit(False);
    pe^.JsonToPdoProc := nil;
    if (pe^.FieldMap = nil) and (pe^.PdoToJsonProc = nil) then PdoCache.Remove(aTypeInfo);
  finally
    GlobLock.EndWrite;
  end;
end;

function RegisterPdoJsonProcs(aTypeInfo: PTypeInfo; aWriteProc: TPdoToJsonProc;
  aReadProc: TJsonToPdoProc): Boolean;
var
  pe: PPdoCacheEntry;
begin
  Result := False;
  if (aTypeInfo = nil) or not(aTypeInfo^.Kind in CUSTOM_KINDS) then exit;
  if (aWriteProc = nil) or (aReadProc = nil) then exit;
  GlobLock.BeginWrite;
  try
    pe := PdoCache.GetMutValueDef(aTypeInfo, DEF_CACHE_ENTRY);
    if (pe^.PdoToJsonProc <> nil) or (pe^.JsonToPdoProc <> nil) then exit;
    pe^.PdoToJsonProc := aWriteProc;
    pe^.JsonToPdoProc := aReadProc;
    Result := True;
  finally
    GlobLock.EndWrite;
  end;
end;

generic procedure PdoLoadJson<T>(var aValue: T; const aJson: string; const aOptions: TJsonReadOptions;
  aMaxDepth: Integer; aSkipBom: Boolean);
begin
  PdoLoadJson(TypeInfo(T), aValue, aJson, aOptions, aMaxDepth, aSkipBom);
end;

{$PUSH}{$WARN 5024 OFF}{$WARN 5036 OFF}
{$WARN 5089 OFF : Local variable "$1" of a managed type does not seem to be initialized}
procedure PdoLoadJson(aTypeInfo: PTypeInfo; var aValue; const aJson: string; const aOptions: TJsonReadOptions;
  aMaxDepth: Integer; aSkipBom: Boolean);
type
  TTokenKind = TJsonReader.TTokenKind;
var
  Reader: TJsonReader = nil;
  SkipUnknownProps, IgnoreNameCase, RejectNulls: Boolean;
  procedure Error(const aMessage: string);
  begin
    raise EPdoLoadJson.Create(aMessage, Reader.Path);
  end;
  procedure ReadNext; inline;
  begin
    if not Reader.Read then
      case Reader.ReadState of
        rsEOF:   Error(SEUnexpectJsonEnd);
        rsError: Error(SEInvalidJsonInst);
      else
      end;
  end;
  procedure CheckNull; inline;
  begin
    if RejectNulls then
      Error(SEJsonNullsNotAcceptable);
  end;
  function TokenKindName(aKind: TTokenKind): string;
  begin
    Result := GetEnumName(TypeInfo(TTokenKind), Integer(aKind));
  end;
  function TypeKindName(aKind: TTypeKind): string;
  begin
    Result := GetEnumName(TypeInfo(TTypeKind), Integer(aKind));
  end;
  procedure NotSupported(aTypeInfo: PTypeInfo);
  begin
    raise EPdoLoadJson.CreateFmt(SEPdoTypeNotSupportFmt,
      [aTypeInfo^.Name, GetEnumName(TypeInfo(TTypeKind), Integer(aTypeInfo^.Kind))]);
  end;
  procedure ReadInteger(aTypeInfo: PTypeInfo; aData: Pointer);
  var
    I: Int64;
  begin
    if Reader.TokenKind <> rtkNumber then
      Error(Format(SEUnexpectJsonTokenFmt, [TokenKindName(rtkNumber), TokenKindName(Reader.TokenKind)]));
    if not Double.IsExactInt(Reader.AsNumber, I) then
      Error(Format(SECantAssignJsonNumFmt, [TypeKindName(aTypeInfo^.Kind)]));
    case GetTypeData(aTypeInfo)^.OrdType of
      otSByte:
        begin
          if (I < System.Low(ShortInt)) or (I > System.High(ShortInt)) then
            Error(Format(SEPdoRangeErrorFmt, ['ShortInt', I]));
          PShortInt(aData)^ := I;
        end;
      otUByte:
        begin
          if (I < 0) or (I > System.High(Byte)) then
            Error(Format(SEPdoRangeErrorFmt, ['Byte', I]));
          PByte(aData)^ := I;
        end;
      otSWord:
        begin
          if (I < System.Low(SmallInt)) or (I > System.High(SmallInt)) then
            Error(Format(SEPdoRangeErrorFmt, ['SmallInt', I]));
          PSmallInt(aData)^ := I;
        end;
      otUWord:
        begin
          if (I < 0) or (I > System.High(Word)) then
            Error(Format(SEPdoRangeErrorFmt, ['Word', I]));
          PWord(aData)^ := I;
        end;
      otSLong:
        begin
          if (I < System.Low(LongInt)) or (I > System.High(LongInt)) then
            Error(Format(SEPdoRangeErrorFmt, ['LongInt', I]));
          PLongInt(aData)^ := I;
        end;
      otULong:
        begin
          if (I < 0) or (I > System.High(Dword)) then
            Error(Format(SEPdoRangeErrorFmt, ['Dword', I]));
          PDword(aData)^ := I;
        end;
      otSQWord: PInt64(aData)^ := I;
      otUQWord:
        begin
          if I < 0 then
            Error(Format(SEPdoRangeErrorFmt, ['QWord', I]));
          PQWord(aData)^ := I;
        end;
    end;
  end;
  procedure ReadFloat(aTypeInfo: PTypeInfo; aData: Pointer);
  var
    I: Int64;
    d: Double;
  begin
    if Reader.TokenKind <> rtkNumber then
      Error(Format(SEUnexpectJsonTokenFmt, [TokenKindName(rtkNumber), TokenKindName(Reader.TokenKind)]));
    d := Reader.AsNumber;
    case GetTypeData(aTypeInfo)^.FloatType of
      ftSingle:
        begin
          if System.Abs(d) > Math.MaxSingle then
            Error(Format(SEPdoOverflowErrorFmt, ['Single', d]));
          PSingle(aData)^ := d;
        end;
      ftDouble:   PDouble(aData)^ := d;
      ftExtended: PExtended(aData)^ := d;
      ftComp:
        begin
          if not Double.IsExactInt(Reader.AsNumber, I) then
            Error(Format(SECantAssignJsonNumFmt, ['ftComp']));
          PComp(aData)^ := I;
        end;
      ftCurr:
        begin
          if (d < Currency.MinValue) or (d > Currency.MaxValue) then
            Error(Format(SEPdoOverflowErrorFmt, ['Currency', d]));
          PCurrency(aData)^ := d;
        end;
    end;
  end;
  procedure ReadChar(aTypeInfo: PTypeInfo; aData: Pointer);
  var
    s: string;
  begin
    if Reader.TokenKind <> rtkString then
      Error(Format(SEUnexpectJsonTokenFmt,[TokenKindName(rtkString), TokenKindName(Reader.TokenKind)]));
    s := Reader.AsString;
    if System.Length(s) <> 1 then
      Error(Format(SEUnexpectStringLenFmt, [System.Length(s)]));
    PChar(aData)^ := s[1];
  end;
  procedure ReadEnum(aTypeInfo: PTypeInfo; aData: Pointer);
  var
    s: string;
    I: Integer;
  begin
    if Reader.TokenKind <> rtkString then
      Error(Format(SEUnexpectJsonTokenFmt,[TokenKindName(rtkString), TokenKindName(Reader.TokenKind)]));
    s := Reader.AsString;
    I := GetEnumValue(aTypeInfo, s);
    if I < 0 then
      Error(Format(SEJsonStrNotMatchEnumFmt, [s, aTypeInfo^.Name]));
    case GetTypeData(aTypeInfo)^.OrdType of
      otSByte:  PShortInt(aData)^ := I;
      otUByte:  PByte(aData)^ := I;
      otSWord:  PSmallInt(aData)^ := I;
      otUWord:  PWord(aData)^ := I;
      otSLong:  PLongInt(aData)^ := I;
      otULong:  PDword(aData)^ := I;
      otSQWord: PInt64(aData)^ := I;
      otUQWord: PQWord(aData)^ := I;
    end;
  end;
  procedure ReadBool(aTypeInfo: PTypeInfo; aData: Pointer);
  var
    b: Boolean;
  begin
    if not(Reader.TokenKind in [rtkFalse, rtkTrue]) then
      Error(Format(SEUnexpectJsonTokenFmt2, [TokenKindName(rtkFalse), TokenKindName(rtkTrue),
                   TokenKindName(Reader.TokenKind)]));
    b := Reader.TokenKind = rtkTrue;
    case GetTypeData(aTypeInfo)^.OrdType of
      otSByte:  PShortInt(aData)^ := Ord(b);
      otUByte:  PByte(aData)^ := Ord(b);
      otSWord:  PSmallInt(aData)^ := Ord(b);
      otUWord:  PWord(aData)^ := Ord(b);
      otSLong:  PLongInt(aData)^ := Ord(b);
      otULong:  PDword(aData)^ := Ord(b);
      otSQWord: PInt64(aData)^ := Ord(b);
      otUQWord: PQWord(aData)^ := Ord(b);
    end;
  end;

var
  LocalPdoCache: TLocalPdoCache;

  function GetPdoEntry(aTypeInfo: PTypeInfo; out e: TPdoCacheEntry): Boolean;
  var
    p: PPdoEntryInfo;
    pce: TPdoCacheEntry;
    m: TRecFieldMap;
    I, J: SizeInt;
  begin
    p := LocalPdoCache.GetMutValueDef(aTypeInfo, DEF_ENTRY_INFO);
    if not p^.Tested then begin
      if GetGlobPdoEntry(aTypeInfo, pce) then begin
        if pce.FieldMap <> nil then begin
          m := pce.FieldMap;
          J := 0;
          for I := 0 to System.High(m) do
            if m[I].Name <> '' then begin
              if I <> J then m[J] := m[I];
              if IgnoreNameCase then
                m[J].Name := LowerCase(m[J].Name);
              Inc(J);
            end;
          System.SetLength(pce.FieldMap, J);
        end;
        p^.Data := pce;
        p^.Registered := True;
      end;
      p^.Tested := True;
    end;
    Result := p^.Registered;
    if Result then e := p^.Data;
  end;

  function IndexOf(const aMap: TRecFieldMap; const aName: string): SizeInt;
  var
    I: SizeInt;
    s: string;
  begin
    if IgnoreNameCase then
      s := LowerCase(aName)
    else
      s := aName;
    for I := 0 to System.High(aMap) do
      if aMap[I].Name = s then exit(I);
    Result := NULL_INDEX;
  end;

  procedure ReadDynArray(aTypeInfo: PTypeInfo; aData: Pointer); forward;
  procedure ReadValue(aTypeInfo: PTypeInfo; aData: Pointer); forward;
  procedure ReadUnregRecord(aTypeInfo: PTypeInfo; aData: Pointer);
  var
    pTypData: PTypeData;
    pManField: PManagedField;
    I: Integer;
  begin
    if Reader.TokenKind <> rtkArrayBegin then
      Error(Format(SEUnexpectJsonTokenFmt,[TokenKindName(rtkArrayBegin), TokenKindName(Reader.TokenKind)]));
    pTypData := GetTypeData(aTypeInfo);
    pManField := PManagedField(
      AlignTypeData(PByte(@pTypData^.TotalFieldCount) + SizeOf(pTypData^.TotalFieldCount)));
    for I := 0 to Pred(pTypData^.TotalFieldCount) do begin
      ReadNext;
      ReadValue(pManField^.TypeRef, PByte(aData) + pManField^.FldOffset);
      Inc(pManField);
    end;
    ReadNext;
    if Reader.TokenKind <> rtkArrayEnd then
      Error(Format(SEUnexpectJsonTokenFmt,[TokenKindName(rtkArrayEnd), TokenKindName(Reader.TokenKind)]));
  end;
  procedure ReadRecord(aTypeInfo: PTypeInfo; aData: Pointer);
  var
    I: Integer;
    e: TPdoCacheEntry;
  begin
    if not GetPdoEntry(aTypeInfo, e) then begin
      ReadUnregRecord(aTypeInfo, aData);
      exit;
    end;
    if e.FieldMap <> nil then begin
      if Reader.TokenKind <> rtkObjectBegin then
        Error(Format(SEUnexpectJsonTokenFmt,[TokenKindName(rtkObjectBegin), TokenKindName(Reader.TokenKind)]));
      repeat
        ReadNext;
        if Reader.TokenKind = rtkObjectEnd then break;
        I := IndexOf(e.FieldMap, Reader.Name);
        if  I < 0 then
          begin
            if not SkipUnknownProps then
              Error(Format(SERecordFieldNotFoundFmt, [aTypeInfo^.Name, Reader.Name]));
            if TJsonReader.IsStartToken(Reader.TokenKind) then
              Reader.Skip;
          end
        else
          ReadValue(e.FieldMap[I].Info, PByte(aData) + e.FieldMap[I].Offset);
      until False;
    end else
      if e.JsonToPdoProc <> nil then begin
        if not TJsonToPdoProc(e.JsonToPdoProc)(aData, Reader, aOptions) then
          Error(Format(SECallbackFalseRetFmt, [aTypeInfo^.Name]));
      end else
        Error(Format(SEUnsupportPdoTypeFmt, [aTypeInfo^.Name]));
  end;
  procedure ReadVariant(aTypeInfo: PTypeInfo; aData: Pointer);
  var
    a: array of Variant;
    I: Int64;
    d: Double;
  begin
    case Reader.TokenKind of
      rtkNull:
        begin
          CheckNull;
          Variant(aData^) := Null;
        end;
      rtkFalse: Variant(aData^) := False;
      rtkTrue: Variant(aData^) := True;
      rtkNumber:
        begin
          d := Reader.AsNumber;
          if Double.IsExactInt(d, I) then
            Variant(aData^) := I
          else
            Variant(aData^) := d;
        end;
      rtkString: Variant(aData^) := Reader.AsString;
      rtkArrayBegin:
        begin
          a := nil;
          ReadDynArray(TypeInfo(a), @a);
          Variant(aData^) := VarArrayOf(a);
        end;
    else
      Error(Format(SECantReadJson2VarFmt, [TokenKindName(Reader.TokenKind)]))
    end;
  end;

  procedure ReadMappedRec(aTypeInfo: PTypeInfo; aData: Pointer; const aMap: TRecFieldMap);
  var
    I: SizeInt;
  begin
    if Reader.TokenKind <> rtkObjectBegin then
      Error(Format(SEUnexpectJsonTokenFmt,[TokenKindName(rtkObjectBegin), TokenKindName(Reader.TokenKind)]));
    repeat
      ReadNext;
      if Reader.TokenKind = rtkObjectEnd then break;
      I := IndexOf(aMap, Reader.Name);
      if I < 0 then
        begin
          if not SkipUnknownProps then
            Error(Format(SERecordFieldNotFoundFmt, [aTypeInfo^.Name, Reader.Name]));
          if TJsonReader.IsStartToken(Reader.TokenKind) then
            Reader.Skip;
        end
      else
        ReadValue(aMap[I].Info, PByte(aData) + aMap[I].Offset);
    until False;
  end;
  procedure ReadArray(aTypeInfo: PTypeInfo; aData: Pointer);
  var
    pTypData: PTypeData;
    ElSize: SizeUInt;
    ElType: PTypeInfo;
    Arr: PByte;
    I, Count: SizeInt;
    e: TPdoCacheEntry;
    Registered: Boolean;
  begin
    if Reader.TokenKind <> rtkArrayBegin then
      Error(Format(SEUnexpectJsonTokenFmt, [TokenKindName(rtkArrayBegin), TokenKindName(Reader.TokenKind)]));
    pTypData := GetTypeData(aTypeInfo);
    ElType := pTypData^.ArrayData.ElType;
    Count := pTypData^.ArrayData.ElCount;
    ElSize := pTypData^.ArrayData.Size div Count;
    Arr := aData;
    case ElType^.Kind of
      tkRecord:
        Registered := GetPdoEntry(ElType, e)and((e.FieldMap<>nil)or(e.JsonToPdoProc<>nil));
      tkObject:
        if not(GetPdoEntry(ElType, e)and(e.JsonToPdoProc <> nil)) then
          Error(Format(SEUnsupportPdoTypeFmt, [ElType^.Name]));
    else
    end;
    I := 0;
    repeat
      ReadNext;
      if Reader.TokenKind = rtkArrayEnd then break;
      if I = Count then
        Error(Format(SEJsonArraySizeNotFitFmt, [Count]));
      case ElType^.Kind of
        tkRecord:
          if Registered then begin
            if e.FieldMap <> nil then
              ReadMappedRec(ElType, Arr, e.FieldMap)
            else
              if not TJsonToPdoProc(e.JsonToPdoProc)(Arr, Reader, aOptions) then
                Error(Format(SECallbackFalseRetFmt, [ElType^.Name]));
          end else
            ReadUnregRecord(ElType, Arr);
        tkObject:
          if not TJsonToPdoProc(e.JsonToPdoProc)(Arr, Reader, aOptions) then
            Error(Format(SECallbackFalseRetFmt, [ElType^.Name]));
      else
        ReadValue(ElType, Arr);
      end;
      Inc(I);
      Arr += ElSize;
    until False;
    if I < Count then
      Error(Format(SEJsonArraySizeNotFitFmt, [Count]));
  end;
  procedure ReadDynArray(aTypeInfo: PTypeInfo; aData: Pointer);
  var
    ElSize: SizeUInt;
    ElType: PTypeInfo;
    I, Size: SizeInt;
    e: TPdoCacheEntry;
    Registered: Boolean;
  const
    InitSize = 8;
  begin
    if Reader.TokenKind <> rtkArrayBegin then
      if Reader.TokenKind = rtkNull then
        begin
          CheckNull;
          DynArrayClear(Pointer(aData^), aTypeInfo);
          exit;
        end
      else
        Error(Format(SEUnexpectJsonTokenFmt, [TokenKindName(rtkArrayBegin), TokenKindName(Reader.TokenKind)]));
    ElSize := GetTypeData(aTypeInfo)^.ElSize;
    ElType := GetTypeData(aTypeInfo)^.ElType2;
    case ElType^.Kind of
      tkRecord:
        Registered := GetPdoEntry(ElType,e)and((e.FieldMap<>nil)or(e.JsonToPdoProc<>nil));
      tkObject:
        if not(GetPdoEntry(ElType, e) and (e.JsonToPdoProc <> nil)) then
          Error(Format(SEUnsupportPdoTypeFmt, [ElType^.Name]));
    else
    end;
    I := NULL_INDEX;
    repeat
      ReadNext;
      Inc(I);
      if Reader.TokenKind = rtkArrayEnd then break;
      if I = DynArraySize(Pointer(aData^)) then
        begin
          if I < InitSize then
            Size := InitSize
          else
            Size := I * 2;
          DynArraySetLength(Pointer(aData^), aTypeInfo, 1, @Size);
        end;
      case ElType^.Kind of
        tkRecord:
          if Registered then begin
            if e.FieldMap <> nil then
              ReadMappedRec(ElType, PByte(aData^) + SizeUInt(I) * ElSize, e.FieldMap)
            else
              if not TJsonToPdoProc(e.JsonToPdoProc)(PByte(aData^) + SizeUInt(I) * ElSize, Reader, aOptions) then
                Error(Format(SECallbackFalseRetFmt, [ElType^.Name]));
          end else
            ReadUnregRecord(ElType, PByte(aData^) + SizeUInt(I) * ElSize);
        tkObject:
          if not TJsonToPdoProc(e.JsonToPdoProc)(PByte(aData^) + SizeUInt(I) * ElSize, Reader, aOptions) then
            Error(Format(SECallbackFalseRetFmt, [ElType^.Name]));
      else
        ReadValue(ElType, PByte(aData^) + SizeUInt(I) * ElSize);
      end;
    until False;
    DynArraySetLength(Pointer(aData^), aTypeInfo, 1, @I);
  end;

  procedure ReadClassProp(o: TObject; aPropInfo: PPropInfo);
  var
    pTypInfo: PTypeInfo;
    I: Int64 = 0;
    e: Extended = 0;
    v: Variant;
    ss: shortstring;
    p: Pointer = nil;
  begin
    pTypInfo := aPropInfo^.PropType;
    case pTypInfo^.Kind of
      tkInteger, tkChar, tkEnumeration, tkInt64, tkQWord, tkWChar, tkBool, tkUChar:
        begin
          ReadValue(pTypInfo, @I);
          SetOrdProp(o, aPropInfo, I);
        end;
      tkFloat:
        begin
          ReadFloat(pTypInfo, @e);
          SetFloatProp(o, aPropInfo, e);
        end;
      tkSet:
        begin
          ReadValue(pTypInfo, @I);
          SetOrdProp(o, aPropInfo, I);
        end;
      tkSString:
        begin
          ReadValue(pTypInfo, @ss);
          SetStrProp(o, aPropInfo, ss);
        end;
      tkLString, tkAString:
        begin
          ReadValue(pTypInfo, @p);
          SetStrProp(o, aPropInfo, string(p));
          string(p) := '';
        end;
      tkWString:
        begin
          ReadValue(pTypInfo, @p);
          SetWideStrProp(o, aPropInfo, widestring(p));
          widestring(p) := '';
        end;
      tkVariant:
        begin
          ReadVariant(pTypInfo, @v);
          SetVariantProp(o, aPropInfo, v);
        end;
      tkClass:
        begin
          ReadValue(pTypInfo, @p);
          SetObjectProp(o, aPropInfo, TObject(p));
        end;
      tkDynArray:
        begin
          ReadDynArray(pTypInfo, @p);
          SetDynArrayProp(o, aPropInfo, p);
          DynArrayClear(p, pTypInfo);
        end;
      tkUString:
        begin
          ReadValue(pTypInfo, @p);
          SetUnicodeStrProp(o, aPropInfo, unicodestring(p));
          unicodestring(p) := '';
        end;
    else
      Error(Format(SEUnsuppPdoPropKindFmt, [GetEnumName(TypeInfo(TTypeKind), Integer(pTypInfo^.Kind))]));
    end;
  end;

type
  TPropMapType = specialize TGLiteChainHashMap<string, PPropInfo, string>;
  TPropMap     = TPropMapType.TMap;

  procedure ReadMappedClass(o: TObject; const aMap: TPropMap);
  var
    pInfo: PPropInfo;
    s: string;
  begin
    if Reader.TokenKind <> rtkObjectBegin then
      Error(Format(SEUnexpectJsonTokenFmt, [TokenKindName(rtkObjectBegin), TokenKindName(Reader.TokenKind)]));
    repeat
      ReadNext;
      if Reader.TokenKind = rtkObjectEnd then break;
      if IgnoreNameCase then
        s := LowerCase(Reader.Name)
      else
        s := Reader.Name;
      if not aMap.TryGetValue(s, pInfo) then
        begin
          if not SkipUnknownProps then
            Error(Format(SEClassPropNotFoundFmt, [o.ClassName, Reader.Name]));
          if TJsonReader.IsStartToken(Reader.TokenKind) then
            Reader.Skip;
        end
      else
        ReadClassProp(o, pInfo);
    until False;
  end;
  procedure GetPropMap(aList: PPropList; aCount: Integer; out aMap: TPropMap);
  var
    I: Integer;
  begin
    aMap.EnsureCapacity(aCount);
    for I := 0 to Pred(aCount) do
      if IsWriteableProp(aList^[I]) then
        if IgnoreNameCase then
          aMap.Add(LowerCase(aList^[I]^.Name), aList^[I])
        else
          aMap.Add(aList^[I]^.Name, aList^[I]);
  end;
  procedure GetPropMap(o: TObject; out aMap: TPropMap);
  var
    pl: PPropList;
    Count: Integer;
  begin
    Count := GetPropList(o, pl);
    try
      GetPropMap(pl, Count, aMap);
    finally
      FreeMem(pl);
    end;
  end;
  procedure ReadSimpleClass(o: TObject);
  var
    Map: TPropMap;
  begin
    if Reader.TokenKind <> rtkObjectBegin then
      Error(Format(SEUnexpectJsonTokenFmt, [TokenKindName(rtkObjectBegin), TokenKindName(Reader.TokenKind)]));
    GetPropMap(o, Map);
    ReadMappedClass(o, Map);
  end;
  procedure GetPropMap(aClass: TClass; out aMap: TPropMap);
  var
    pl: PPropList;
    Count: Integer;
  begin
    Count := GetPropList(aClass, pl);
    try
      GetPropMap(pl, Count, aMap);
    finally
      FreeMem(pl);
    end;
  end;
  procedure ReadCollection(aCol: TCollection);
  var
    Map: TPropMap;
  begin
    if Reader.TokenKind <> rtkArrayBegin then
      Error(Format(SEUnexpectJsonTokenFmt, [TokenKindName(rtkArrayBegin), TokenKindName(Reader.TokenKind)]));
    aCol.Clear;
    GetPropMap(aCol.ItemClass, Map);
    repeat
      ReadNext;
      if Reader.TokenKind = rtkArrayEnd then break;
      ReadMappedClass(aCol.Add, Map);
    until False;
  end;
  procedure ReadString(aTypeInfo: PTypeInfo; aData: Pointer); forward;
  procedure ReadStrings(aStrings: TStrings);
  var
    s: string;
  begin
    if Reader.TokenKind <> rtkArrayBegin then
      Error(Format(SEUnexpectJsonTokenFmt, [TokenKindName(rtkArrayBegin), TokenKindName(Reader.TokenKind)]));
    aStrings.Clear;
    repeat
      ReadNext;
      if Reader.TokenKind = rtkArrayEnd then break;
      s := '';
      ReadString(TypeInfo(s), @s);
      aStrings.Add(s);
    until False;
  end;
  procedure ReadClass(aTypeInfo: PTypeInfo; aData: Pointer);
  var
    o: TObject;
    e: TPdoCacheEntry;
  begin
    if Reader.TokenKind = rtkNull then begin
      CheckNull;
      FreeAndNil(TObject(aData^));
      exit;
    end;
    if TObject(aData^) = nil then
      if jroTryCreateClassInst in aOptions then
        // if the constructor is not virtual, then this step may be useless,
        // or the class may not have a parameterless constructor
        TObject(aData^) := GetTypeData(aTypeInfo)^.ClassType.Create
      else
        Error(Format(SEUnassignClassInstFmt, [aTypeInfo^.Name]));
    if (GetPdoEntry(aTypeInfo, e) and (e.JsonToPdoProc <> nil)) then begin
      if not TJsonToPdoProc(e.JsonToPdoProc)(aData, Reader, aOptions) then
        Error(Format(SECallbackFalseRetFmt, [aTypeInfo^.Name]));
      exit;
    end;
    o := TObject(aData^);
    if o is TCollection then
      ReadCollection(TCollection(o))
    else
      if o is TStrings then
        ReadStrings(TStrings(o))
      else
        ReadSimpleClass(o);
  end;
  procedure ReadObject(aTypeInfo: PTypeInfo; aData: Pointer);
  var
    e: TPdoCacheEntry;
  begin
    if not(GetPdoEntry(aTypeInfo, e) and (e.JsonToPdoProc <> nil)) then
      NotSupported(aTypeInfo);
    if not TJsonToPdoProc(e.JsonToPdoProc)(aData, Reader, aOptions) then
      Error(Format(SECallbackFalseRetFmt, [aTypeInfo^.Name]));
  end;
  procedure ReadSString(aTypeInfo: PTypeInfo; aData: Pointer);
  var
    s: string;
    MaxLen: Byte;
  begin
    if Reader.TokenKind <> rtkString then
      if Reader.TokenKind = rtkNull then
        begin
          CheckNull;
          PShortString(aData)^ := '';
          exit;
        end
      else
        Error(Format(SEUnexpectJsonTokenFmt,[TokenKindName(rtkString), TokenKindName(Reader.TokenKind)]));
    s := Reader.AsString;
    MaxLen := GetTypeData(aTypeInfo)^.MaxLength;
    if System.Length(s) > MaxLen then
      Error(Format(SEJsonStrExceedSSLenFmt, [System.Length(s), MaxLen]));
    System.SetLength(PShortString(aData)^, System.Length(s));
    System.Move(Pointer(s)^, PShortString(aData)^[1], System.Length(PShortString(aData)^));
  end;
  procedure ReadString(aTypeInfo: PTypeInfo; aData: Pointer);
  begin
    if Reader.TokenKind <> rtkString then
      if Reader.TokenKind = rtkNull then
        begin
          CheckNull;
          PString(aData)^ := '';
          exit;
        end
      else
        Error(Format(SEUnexpectJsonTokenFmt,[TokenKindName(rtkString), TokenKindName(Reader.TokenKind)]));
    PString(aData)^ := Reader.AsString;
  end;
  procedure ReadWString(aTypeInfo: PTypeInfo; aData: Pointer);
  var
    s: string = '';
  begin
    ReadString(TypeInfo(s), @s);
    PWideString(aData)^ := widestring(s);
  end;
  procedure ReadUString(aTypeInfo: PTypeInfo; aData: Pointer);
  var
    s: string = '';
  begin
    ReadString(TypeInfo(s), @s);
    PUnicodeString(aData)^ := unicodestring(s);
  end;
  procedure ReadWChar(aTypeInfo: PTypeInfo; aData: Pointer);
  var
    ws: widestring = '';
  begin
    ReadWString(TypeInfo(ws), @ws);
    if System.Length(ws) <> 1 then
      Error(Format(SEStrLenNotMatchSizeFmt, ['WideString', System.Length(ws), 'WideChar']));
    PWideChar(aData)^ := ws[1];
  end;
  procedure ReadUChar(aTypeInfo: PTypeInfo; aData: Pointer);
  var
    us: unicodestring = '';
  begin
    ReadUString(TypeInfo(us), @us);
    if System.Length(us) <> 1 then
      Error(Format(SEStrLenNotMatchSizeFmt, ['UnicodeString', System.Length(us), 'UnicodeChar']));
    PUnicodeChar(aData)^ := us[1];
  end;
  procedure ReadSet(aTypeInfo: PTypeInfo; aData: Pointer);
  var
    I: Int64 = 0;
    Size, MaxBit: Integer;
    pElType: PTypeInfo;
    pIntData: PInteger;
    ElKind: TTypeKind;
    c: Char = ' ';
  begin
    if Reader.TokenKind <> rtkArrayBegin then
      if Reader.TokenKind = rtkNull then
        begin
          CheckNull;
          System.FillChar(aData^, GetTypeData(aTypeInfo)^.SetSize, 0);
          exit;
        end
      else
        Error(Format(SEUnexpectJsonTokenFmt,[TokenKindName(rtkArrayBegin), TokenKindName(Reader.TokenKind)]));
    with GetTypeData(aTypeInfo)^ do
      begin
        pElType := CompType;
        Size := SetSize;
        ElKind := CompType^.Kind;
      end;
    MaxBit := Pred(Size * 8);
    pIntData := aData;
    System.FillChar(pIntData^, Size, 0);
    repeat
      ReadNext;
      if Reader.TokenKind = rtkArrayEnd then break;
      case ElKind of
        tkChar:
          begin
            ReadValue(TypeInfo(c), @c);
            pIntData[Ord(c) shr 5] := pIntData[Ord(c) shr 5] or Integer(1) shl (Ord(c) and 31);
          end;
        tkEnumeration:
          begin
            ReadValue(pElType, @I);
            pIntData[I shr 5] := pIntData[I shr 5] or Integer(1) shl (I and 31);
          end;
      else
        ReadValue(pElType, @I);
        if (I < 0) or (I > MaxBit) then
          Error(Format(SEPdoRangeErrorFmt, ['Set element', I]));
        pIntData[I shr 5] := pIntData[I shr 5] or Integer(1) shl (I and 31);
      end;
    until False;
  end;
  procedure ReadValue(aTypeInfo: PTypeInfo; aData: Pointer);
  begin
    case aTypeInfo^.Kind of
      tkInteger, tkInt64, tkQWord: ReadInteger(aTypeInfo, aData);
      tkFloat:                     ReadFloat(aTypeInfo, aData);
      tkChar:                      ReadChar(aTypeInfo, aData);
      tkEnumeration:               ReadEnum(aTypeInfo, aData);
      tkSet:                       ReadSet(aTypeInfo, aData);
      tkSString:                   ReadSString(aTypeInfo, aData);
      tkLString, tkAString:        ReadString(aTypeInfo, aData);
      tkWString:                   ReadWString(aTypeInfo, aData);
      tkVariant:                   ReadVariant(aTypeInfo, aData);
      tkArray:                     ReadArray(aTypeInfo, aData);
      tkRecord:                    ReadRecord(aTypeInfo, aData);
      tkClass:                     ReadClass(aTypeInfo, aData);
      tkObject:                    ReadObject(aTypeInfo, aData);
      tkWChar:                     ReadWChar(aTypeInfo, aData);
      tkBool:                      ReadBool(aTypeInfo, aData);
      tkDynArray:                  ReadDynArray(aTypeInfo, aData);
      tkUString:                   ReadUString(aTypeInfo, aData);
      tkUChar:                     ReadUChar(aTypeInfo, aData);
    else
      NotSupported(aTypeInfo);
    end;
  end;
var
  ReaderRef: specialize TGUniqRef<TJsonReader>;
  s: string;
begin
  s := aJson;
  ReaderRef.Instance := TJsonReader.Create(Pointer(s), System.Length(s), aMaxDepth, aSkipBom);
  Reader := ReaderRef;
  ReadNext;
  SkipUnknownProps := jroSkipUnknownProps in aOptions;
  IgnoreNameCase := jroIgnoreNameCase in aOptions;
  RejectNulls := jroRejectNulls in aOptions;
  try
    ReadValue(aTypeInfo, @aValue);
    if Reader.ReadState = rsGo then Reader.Read;
    if Reader.ReadState <> rsEof then
      Error(SEJsonTrailingGarbage);
  except
    on e: EPdoLoadJson do raise;
    on e: Exception do
      raise EPdoLoadJson.Create(Format(SEExceptWhileJsonLoadFmt, [e.ClassName, e.Message]), Reader.Path);
  end;
end;
{$POP}

const
  MPACK_VAR_ARRAY = High(Word);

procedure Pdo2MsgPack(aTypeInfo: PTypeInfo; const aValue; aWriter: TMpCustomWriter; aUserExt: IMpUserExt);

  procedure NotSupported(aTypeInfo: PTypeInfo);
  begin
    raise EPdoToMsgPack.CreateFmt(SEPdoTypeNotSupportFmt,
      [aTypeInfo^.Name, GetEnumName(TypeInfo(TTypeKind), Integer(aTypeInfo^.Kind))]);
  end;

  procedure WriteInteger(aTypeData: PTypeData; aData: Pointer);
  begin
    case aTypeData^.OrdType of
      otSByte:  aWriter.Add(Int64(PShortInt(aData)^));
      otUByte:  aWriter.Add(Int64(PByte(aData)^));
      otSWord:  aWriter.Add(Int64(PSmallInt(aData)^));
      otUWord:  aWriter.Add(Int64(PWord(aData)^));
      otSLong:  aWriter.Add(Int64(PLongInt(aData)^));
      otULong:  aWriter.Add(Int64(PDWord(aData)^));
      otSQWord: aWriter.Add(PInt64(aData)^);
      otUQWord: aWriter.Add(Int64(PQWord(aData)^));
    end;
  end;

  function GetOrdValue(aTypeData: PTypeData; aData: Pointer): Int64;
  begin
    Result := 0;
    case aTypeData^.OrdType of
      otSByte:  Result := Int64(PShortInt(aData)^);
      otUByte:  Result := Int64(PByte(aData)^);
      otSWord:  Result := Int64(PSmallInt(aData)^);
      otUWord:  Result := Int64(PWord(aData)^);
      otSLong:  Result := Int64(PLongInt(aData)^);
      otULong:  Result := Int64(PDword(aData)^);
      otSQWord: Result := PInt64(aData)^;
      otUQWord: Result := Int64(PQWord(aData)^);
    end;
  end;

  procedure WriteFloat(aTypeData: PTypeData; aData: Pointer);
  begin
    case aTypeData^.FloatType of
      ftSingle:   aWriter.Add(PSingle(aData)^);
      ftDouble:   aWriter.Add(PDouble(aData)^);
      ftExtended: aWriter.Add(Double(PExtended(aData)^));
      ftComp:     aWriter.Add(PInt64(aData)^);
      ftCurr:     aWriter.Add(PInt64(aData)^);
    end;
  end;

  procedure WriteBool(aTypeData: PTypeData; aData: Pointer);
  begin
    case aTypeData^.OrdType of
      otSByte:  aWriter.Add(PShortInt(aData)^ > 0);
      otUByte:  aWriter.Add(PByte(aData)^ > 0);
      otSWord:  aWriter.Add(PSmallInt(aData)^ > 0);
      otUWord:  aWriter.Add(PWord(aData)^ > 0);
      otSLong:  aWriter.Add(PLongInt(aData)^ > 0);
      otULong:  aWriter.Add(PDword(aData)^ > 0);
      otSQWord: aWriter.Add(PInt64(aData)^ > 0);
      otUQWord: aWriter.Add(PQWord(aData)^ > 0);
    end;
  end;

  procedure WriteSet(aTypeInfo: PTypeInfo; aData: Pointer);
  var
    RestSize, CurrSize: Integer;
    b: TBytes;
    p: PDWord;
    I, ByteIdx: Integer;
  begin
    RestSize := GetTypeData(aTypeInfo)^.SetSize;
    System.SetLength(b, RestSize);
    case RestSize of
      1: b[0] := PByte(aData)^;
      2:
        begin
          b[0] := PByte(aData)^;
          b[1] := Byte(PWord(aData)^ shr 8);
        end;
    else
      ByteIdx := 0;
      p := aData;
      while RestSize <> 0 do begin
        CurrSize := Math.Min(RestSize, SizeOf(DWord));
        for I := 0 to Pred(CurrSize) do begin
          b[ByteIdx] := Byte(p^ shr (I * 8));
          Inc(ByteIdx);
        end;
        Dec(RestSize, CurrSize);
        Inc(p);
      end;
    end;
    aWriter.Add(b);
  end;

  procedure WriteVariant(const v: Variant);
  var
    I: SizeInt;
    vType: TVarType;
  begin
    if VarIsEmpty(v) or VarIsNull(v) then
      begin
        aWriter.AddNil;
        exit;
      end;
    if VarIsArray(v) then
      begin
        aWriter.BeginArray(2);
        aWriter.Add(Int64(MPACK_VAR_ARRAY));
        aWriter.BeginArray(Succ(VarArrayHighBound(v, 1) - VarArrayLowBound(v, 1)));
        for I := VarArrayLowBound(v, 1) to VarArrayHighBound(v, 1) do
          WriteVariant(v[I]);
        exit;
      end;
    vType := VarType(v);
    case vType of
      varSmallInt, varInteger, varSingle, varDouble, varCurrency, varDate, varOleStr,
      varShortInt, varByte, varWord, varLongWord, varInt64, varQWord, varBoolean,
      varString:
        begin
          aWriter.BeginArray(2);
          aWriter.Add(Int64(vType));
          case vType of
            varSmallInt: aWriter.Add(Int64(TVarData(v).vSmallInt));
            varInteger:  aWriter.Add(Int64(TVarData(v).vInteger));
            varSingle:   aWriter.Add(TVarData(v).vSingle);
            varDouble:   aWriter.Add(TVarData(v).vDouble);
            varCurrency: aWriter.Add(PInt64(@TVarData(v).vCurrency)^);
            varDate:     aWriter.Add(TVarData(v).vDate);
            varOleStr:   aWriter.Add(string(widestring(v)));
            varBoolean:  aWriter.Add(TVarData(v).vBoolean);
            varShortInt: aWriter.Add(Int64(TVarData(v).vShortInt));
            varByte:     aWriter.Add(Int64(TVarData(v).vByte));
            varWord:     aWriter.Add(Int64(TVarData(v).vWord));
            varLongWord: aWriter.Add(Int64(TVarData(v).vLongWord));
            varInt64:    aWriter.Add(TVarData(v).vInt64);
            varQWord:    aWriter.Add(Int64(TVarData(v).vQWord));
            varString:   aWriter.Add(string(v));
          else
          end;
        end;
    else
      raise EPdoToMsgPack.CreateFmt(SEVariantNotSupportFmt, [VarType(v)]);
    end;
  end;

  procedure WriteValue(aTypeInfo: PTypeInfo; aData: Pointer); forward;

  procedure WriteArray(aTypeInfo: PTypeInfo; aData: Pointer);
  var
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
    if ElSize = 1 then
      aWriter.Add(Arr[0..Pred(Count)])
    else begin
      aWriter.BeginArray(Count);
      for I := 0 to Pred(Count) do begin
        WriteValue(ElType, Arr);
        Arr += ElSize;
      end;
    end;
  end;

type
  TLocCacheEntry = record
    FieldMap: TRecFieldMap;
    Count: Integer;
    Tested: Boolean;
  end;

  TRecFieldMapData = record
    Map: TRecFieldMap;
    Count: Integer;
  end;

  TLocalCacheType = specialize TGLiteEquatableHashMap<PTypeInfo, TLocCacheEntry, Pointer>;
  TLocalCache     = TLocalCacheType.TMap;

var
  LocalCache: TLocalCache;

  function GetFieldMapData(aTypeInfo: PTypeInfo; out aData: TRecFieldMapData): Boolean;
  var
    pe: ^TLocCacheEntry;
    pce: TPdoCacheEntry;
    I, Cnt: Integer;
  begin
    pe := LocalCache.GetMutValueDef(aTypeInfo, Default(TLocCacheEntry));
    if not pe^.Tested then begin
      if GetGlobPdoEntry(aTypeInfo, pce) then begin
        pe^.FieldMap := pce.FieldMap;
        if pe^.FieldMap <> nil then begin
          Cnt := 0;
          for I := 0 to System.High(pe^.FieldMap) do
            Inc(Cnt, Ord(pe^.FieldMap[I].Name <> ''));
          pe^.Count := Cnt;
        end;
      end;
      pe^.Tested := True;
    end;
    Result := pe^.FieldMap <> nil;
    if Result then begin
      aData.Map := pe^.FieldMap;
      aData.Count := pe^.Count;
    end;
  end;

  procedure WriteRecord(aTypeInfo: PTypeInfo; aData: Pointer);
  var
    MapData: TRecFieldMapData;
    pTypData: PTypeData;
    pManField: PManagedField;
    I: Integer;
  begin
    if GetFieldMapData(aTypeInfo, MapData) then begin
      aWriter.BeginMap(MapData.Count);
      for I := 0 to System.High(MapData.Map) do
        if MapData.Map[I].Name <> '' then begin
          aWriter.Add(MapData.Map[I].Name);
          WriteValue(MapData.Map[I].Info, PByte(aData) + MapData.Map[I].Offset);
        end;
    end else begin
      if (aUserExt <> nil) and aUserExt.CanWrite(aData, aTypeInfo, aWriter) then exit;
      pTypData := GetTypeData(aTypeInfo);
      pManField := PManagedField(
        AlignTypeData(PByte(@pTypData^.TotalFieldCount) + SizeOf(pTypData^.TotalFieldCount)));
      aWriter.BeginArray(pTypData^.TotalFieldCount);
      for I := 0 to Pred(pTypData^.TotalFieldCount) do begin
        WriteValue(pManField^.TypeRef, PByte(aData) + pManField^.FldOffset);
        Inc(pManField);
      end;
    end;
  end;

  procedure WriteClass(aTypeInfo: PTypeInfo; o: TObject); forward;

  procedure WriteClassProp(o: TObject; aPropInfo: PPropInfo);
  var
    pTypInfo: PTypeInfo;
    I: Int64;
    e: Extended;
    p: Pointer;
  begin
    pTypInfo := aPropInfo^.PropType;
    case pTypInfo^.Kind of
      tkInteger, tkChar, tkEnumeration, tkWChar, tkBool, tkInt64, tkQWord, tkUChar:
        begin
          I := GetOrdProp(o, aPropInfo);
          WriteValue(pTypInfo, @I);
        end;
      tkFloat:
        begin
          e := GetFloatProp(o, aPropInfo);
          WriteValue(pTypInfo, @e);
        end;
      tkSet:
         begin
           I := GetOrdProp(o, aPropInfo);
           WriteValue(pTypInfo, @I);
         end;
      tkSString, tkLString, tkAString:
        begin
          p := Pointer(GetStrProp(o, aPropInfo));
          WriteValue(pTypInfo, @p);
        end;
      tkWString:
        begin
          p := Pointer(GetWideStrProp(o, aPropInfo));
          WriteValue(pTypInfo, @p);
        end;
      tkVariant: WriteVariant(GetVariantProp(o, aPropInfo));
      tkClass: WriteClass(pTypInfo, GetObjectProp(o, aPropInfo));
      tkDynArray:
        begin
          p := GetDynArrayProp(o, aPropInfo);
          WriteValue(pTypInfo, @p);
        end;
      tkUString:
        begin
          p := Pointer(GetUnicodeStrProp(o, aPropInfo));
          WriteValue(pTypInfo, @p);
        end;
    else
      NotSupported(pTypInfo);
    end;
  end;

  procedure WriteClass(aTypeInfo: PTypeInfo; o: TObject);
  var
    I, J, PropCount, ReadableCount: SizeInt;
    c: TCollection;
    pProps: PPropList;
    pInfo: PPropInfo;
  begin
    if (aUserExt <> nil) and aUserExt.CanWrite(@o, aTypeInfo, aWriter) then exit;
    if o = nil then
      aWriter.AddNil
    else
      if o is TCollection then begin
        c := TCollection(o);
        if c.Count = 0 then begin
          aWriter.BeginArray(0);
        end else begin
          aWriter.BeginArray(c.Count);
          PropCount := GetPropList(c.ItemClass, pProps);
          try
            ReadableCount := 0;
            for J := 0 to Pred(PropCount) do
              Inc(ReadableCount, Ord(IsReadableProp(pProps^[J])));
            for I := 0 to Pred(c.Count) do begin
              aWriter.BeginMap(ReadableCount);
              for J := 0 to Pred(ReadableCount) do begin
                pInfo := pProps^[J];
                if not IsReadableProp(pInfo) then continue;
                aWriter.Add(pInfo^.Name);
                WriteClassProp(c.Items[I], pInfo);
              end;
            end;
          finally
            FreeMem(pProps);
          end;
        end;
      end else
        if o is TStrings then begin
          aWriter.BeginArray(TStrings(o).Count);
          with TStrings(o) do
            for I := 0 to Pred(Count) do
              aWriter.Add(Strings[I]);
        end else begin
          PropCount := GetPropList(o, pProps);
          try
            ReadableCount := 0;
            for J := 0 to Pred(PropCount) do
              Inc(ReadableCount, Ord(IsReadableProp(pProps^[J])));
            aWriter.BeginMap(ReadableCount);
            for I := 0 to Pred(PropCount) do begin
              pInfo := pProps^[I];
              if not IsReadableProp(pInfo) then continue;
              aWriter.Add(pInfo^.Name);
              WriteClassProp(o, pInfo);
            end;
          finally
            FreeMem(pProps);
          end;
        end;
  end;

  procedure WriteObject(aTypeInfo: PTypeInfo; aData: Pointer);
  begin
    if (aUserExt <> nil) and aUserExt.CanWrite(aData, aTypeInfo, aWriter) then
      exit;
    NotSupported(aTypeInfo);
  end;

  procedure WriteDynArray(aTypeInfo: PTypeInfo; aData: Pointer);
  var
    pTypData: PTypeData;
    ElSize: SizeUInt;
    ElType: PTypeInfo;
    Arr: PByte;
    I, Count: SizeInt;
  begin
    pTypData := GetTypeData(aTypeInfo);
    ElSize := pTypData^.elSize;
    ElType := pTypData^.ElType2;
    Arr := Pointer(aData^);
    Count := System.DynArraySize(Arr);
    if ElSize = 1 then
      aWriter.Add(Arr[0..Pred(Count)])
    else begin
      aWriter.BeginArray(Count);
      for I := 0 to Pred(Count) do begin
        WriteValue(ElType, Arr);
        Arr += ElSize;
      end;
    end;
  end;

  procedure WriteValue(aTypeInfo: PTypeInfo; aData: Pointer);
  begin
    case aTypeInfo^.Kind of
      tkInteger, tkInt64,
      tkQWord:              WriteInteger(GetTypeData(aTypeInfo), aData);
      tkChar:               aWriter.Add(PAnsiChar(aData)^);
      tkEnumeration:        aWriter.Add(Int64(GetOrdValue(GetTypeData(aTypeInfo), aData)));
      tkFloat:              WriteFloat(GetTypeData(aTypeInfo), aData);
      tkSet:                WriteSet(aTypeInfo, aData);
      tkSString:            aWriter.Add(PShortString(aData)^);
      tkLString, tkAString: aWriter.Add(PString(aData)^);
      tkWString:            aWriter.Add(string(PWideString(aData)^));
      tkVariant:            WriteVariant(PVariant(aData)^);
      tkArray:              WriteArray(aTypeInfo, aData);
      tkRecord:             WriteRecord(aTypeInfo, aData);
      tkClass:              WriteClass(aTypeInfo, TObject(aData^));
      tkObject:             WriteObject(aTypeInfo, aData);
      tkWChar:              aWriter.Add(string(widestring(PWideChar(aData)^)));
      tkBool:               WriteBool(GetTypeData(aTypeInfo), aData);
      tkDynArray:           WriteDynArray(aTypeInfo, aData);
      tkUString:            aWriter.Add(string(PUnicodeString(aData)^));
      tkUChar:              aWriter.Add(string(unicodestring(PUnicodeChar(aData)^)));
    else
      if (aUserExt <> nil) and aUserExt.CanWrite(aData, aTypeInfo, aWriter) then exit;
      NotSupported(aTypeInfo);
    end;
  end;

begin
  if aTypeInfo = nil then exit;
  WriteValue(aTypeInfo, @aValue);
end;

generic function PdoToMsgPackStr<T>(const aValue: T; aUserExt: IMpUserExt): rawbytestring;
begin
  Result := PdoToMsgPackStr(TypeInfo(aValue), aValue, aUserExt);
end;

function PdoToMsgPackStr(aTypeInfo: PTypeInfo; const aValue; aUserExt: IMpUserExt): rawbytestring;
var
  Writer: TMpWriter;
begin
  Result := '';
  Writer := TMpWriter.Create;
  try
    Pdo2MsgPack(aTypeInfo, aValue, Writer, aUserExt);
    Result := Writer.ToStrRaw;
  finally
    Writer.Free;
  end;
end;

generic function PdoToMsgPack<T>(const aValue: T; aUserExt: IMpUserExt): TBytes;
begin
  Result := PdoToMsgPack(TypeInfo(aValue), aValue, aUserExt);
end;

function PdoToMsgPack(aTypeInfo: PTypeInfo; const aValue; aUserExt: IMpUserExt): TBytes;
var
  Writer: TMpWriter;
begin
  Result := nil;
  Writer := TMpWriter.Create;
  try
    Pdo2MsgPack(aTypeInfo, aValue, Writer, aUserExt);
    Result := Writer.ToBytes;
  finally
    Writer.Free;
  end;
end;

procedure MsgPack2Pdo(aTypeInfo: PTypeInfo; var aValue; aReader: TMpCustomReader; aUserExt: IMpUserExt);

  procedure Error(const aMsg: string);
  begin
    raise EPdoLoadMsgPack.Create(aMsg);
  end;

  procedure Error(const aFmt: string; const aParams: array of const);
  begin
    raise EPdoLoadMsgPack.CreateFmt(aFmt, aParams);
  end;

  procedure NotSupported(aTypeInfo: PTypeInfo);
  begin
    raise EPdoLoadMsgPack.CreateFmt(SEPdoTypeNotSupportFmt,
      [aTypeInfo^.Name, GetEnumName(TypeInfo(TTypeKind), Integer(aTypeInfo^.Kind))]);
  end;

  procedure InvalidVarFormat;
  begin
    Error(SEInvalidMPackVarFormat);
  end;

  function TokenKindName(aKind: TMpTokenKind): string;
  begin
    Result := GetEnumName(TypeInfo(TMpTokenKind), Integer(aKind));
  end;

  function TypeKindName(aKind: TTypeKind): string;
  begin
    Result := GetEnumName(TypeInfo(TTypeKind), Integer(aKind));
  end;

  procedure UnexpectedToken(aExpected, aGot: TMpTokenKind);
  begin
    Error(SEUnexpectMPackTokenFmt, [TokenKindName(aExpected), TokenKindName(aGot)])
  end;

  procedure ReadNext; inline;
  begin
    if not aReader.Read then
      case aReader.ReadState of
        mrsEOF:   Error(SEUnexpectMPackEnd);
        mrsError: Error(SEInvalidMPackInst);
      else
      end;
  end;

  procedure ReadInteger(aTypeInfo: PTypeInfo; aData: Pointer);
  var
    I: Int64;
  begin
    if aReader.TokenKind <> mtkInt then UnexpectedToken(mtkInt, aReader.TokenKind);
    I := aReader.AsInt;
    case GetTypeData(aTypeInfo)^.OrdType of
      otSByte:
        begin
          if (I < System.Low(ShortInt)) or (I > System.High(ShortInt)) then
            Error(SEMPackRangeErrorFmt, ['ShortInt', I]);
          PShortInt(aData)^ := I;
        end;
      otUByte:
        begin
          if (I < 0) or (I > System.High(Byte)) then
            Error(SEMPackRangeErrorFmt, ['Byte', I]);
          PByte(aData)^ := I;
        end;
      otSWord:
        begin
          if (I < System.Low(SmallInt)) or (I > System.High(SmallInt)) then
            Error(SEMPackRangeErrorFmt, ['SmallInt', I]);
          PSmallInt(aData)^ := I;
        end;
      otUWord:
        begin
          if (I < 0) or (I > System.High(Word)) then
            Error(SEMPackRangeErrorFmt, ['Word', I]);
          PWord(aData)^ := I;
        end;
      otSLong:
        begin
          if (I < System.Low(LongInt)) or (I > System.High(LongInt)) then
            Error(SEMPackRangeErrorFmt, ['LongInt', I]);
          PLongInt(aData)^ := I;
        end;
      otULong:
        begin
          if (I < 0) or (I > System.High(DWord)) then
            Error(SEMPackRangeErrorFmt, ['DWord', I]);
          PDword(aData)^ := I;
        end;
      otSQWord: PInt64(aData)^ := I;
      otUQWord: PQWord(aData)^ := QWord(I);
    end;
  end;

  procedure ReadFloat(aTypeInfo: PTypeInfo; aData: Pointer);
  begin
    case GetTypeData(aTypeInfo)^.FloatType of
      ftSingle:
        begin
          if aReader.TokenKind <> mtkSingle then UnexpectedToken(mtkSingle, aReader.TokenKind);
          PSingle(aData)^ := aReader.AsSingle;
        end;
      ftDouble:
        begin
          if aReader.TokenKind <> mtkDouble then UnexpectedToken(mtkDouble, aReader.TokenKind);
          PDouble(aData)^ := aReader.AsDouble;
        end;
      ftExtended:
        begin
          if aReader.TokenKind <> mtkDouble then UnexpectedToken(mtkDouble, aReader.TokenKind);
          PExtended(aData)^ := aReader.AsDouble;
        end;
      ftComp:
        begin
          if aReader.TokenKind <> mtkInt then UnexpectedToken(mtkInt, aReader.TokenKind);
          PInt64(aData)^ := aReader.AsInt;
        end;
      ftCurr:
        begin
          if aReader.TokenKind <> mtkInt then UnexpectedToken(mtkInt, aReader.TokenKind);
          PInt64(aData)^ := aReader.AsInt;
        end;
    end;
  end;

  procedure ReadChar(aData: Pointer);
  var
    s: string;
  begin
    if aReader.TokenKind <> mtkString then UnexpectedToken(mtkString, aReader.TokenKind);
    s := aReader.AsString;
    if System.Length(s) <> 1 then Error(SEUnexpectStringLenFmt, [System.Length(s)]);
    PAnsiChar(aData)^ := s[1];
  end;

  procedure ReadEnum(aTypeInfo: PTypeInfo; aData: Pointer);
  var
    I: Int64;
  begin
    if aReader.TokenKind <> mtkInt then UnexpectedToken(mtkInt, aReader.TokenKind);
    I := aReader.AsInt;
    with GetTypeData(aTypeInfo)^ do begin
      if (I < MinValue) or (I > MaxValue) then
        Error(SEMPackRangeErrorFmt, [aTypeInfo^.Name, I]);
      case OrdType of
        otSByte:  PShortInt(aData)^ := aReader.AsInt;
        otUByte:  PByte(aData)^ := aReader.AsInt;
        otSWord:  PSmallInt(aData)^ := aReader.AsInt;
        otUWord:  PWord(aData)^ := aReader.AsInt;
        otSLong:  PLongInt(aData)^ := aReader.AsInt;
        otULong:  PDword(aData)^ := aReader.AsInt;
        otSQWord: PInt64(aData)^ := aReader.AsInt;
        otUQWord: PQWord(aData)^ := QWord(aReader.AsInt);
      end;
    end;
  end;

  procedure ReadSet(aTypeInfo: PTypeInfo; aData: Pointer);
  var
    RestSize, CurrSize, I, ByteIdx: Integer;
    p: PDWord;
    b: TBytes;
  begin
    if aReader.TokenKind <> mtkBin then UnexpectedToken(mtkBin, aReader.TokenKind);
    b := aReader.AsBinary;
    RestSize := GetTypeData(aTypeInfo)^.SetSize;
    if System.Length(b) <> RestSize then
      Error(SEMPackSetSizeErrorFmt, [aTypeInfo^.Name, System.Length(b)]);
    case RestSize of
      1: PByte(aData)^ := b[0];
      2: PWord(aData)^ := Word(b[1]) shl 8 or b[0];
    else
      ByteIdx := 0;
      p := aData;
      System.FillChar(p^, RestSize, 0);
      while RestSize <> 0 do begin
        CurrSize := Math.Min(RestSize, SizeOf(DWord));
        for I := 0 to Pred(CurrSize) do begin
          p^ := p^ or DWord(b[ByteIdx]) shl (I * 8);
          Inc(ByteIdx);
        end;
        Dec(RestSize, CurrSize);
        Inc(p);
      end;
    end;
  end;

  procedure ReadSString(aTypeInfo: PTypeInfo; aData: Pointer);
  var
    s: string;
    MaxLen: Byte;
  begin
    if aReader.TokenKind <> mtkString then UnexpectedToken(mtkString, aReader.TokenKind);
    s := aReader.AsString;
    MaxLen := GetTypeData(aTypeInfo)^.MaxLength;
    if System.Length(s) > MaxLen then
      Error(Format(SEMPackStrExceedSSLenFmt, [System.Length(s), MaxLen]));
    System.SetLength(PShortString(aData)^, System.Length(s));
    System.Move(Pointer(s)^, PShortString(aData)^[1], System.Length(PShortString(aData)^));
  end;

  procedure ReadString(aData: Pointer);
  begin
    if aReader.TokenKind <> mtkString then UnexpectedToken(mtkString, aReader.TokenKind);
    PString(aData)^ := aReader.AsString;
  end;

  procedure ReadWString(aData: Pointer);
  var
    s: string = '';
  begin
    ReadString(@s);
    PWideString(aData)^ := widestring(s);
  end;

  procedure ReadUString(aData: Pointer);
  var
    s: string = '';
  begin
    ReadString(@s);
    PUnicodeString(aData)^ := unicodestring(s);
  end;

  procedure ReadDynArray(aTypeInfo: PTypeInfo; aData: Pointer); forward;

  procedure ReadVariant(aData: Pointer);
  var
    a: array of Variant;
    I: Int64;
    vType: Word;
  begin
    if aReader.TokenKind = mtkNil then begin
      Variant(aData^) := Variants.Null;
      exit;
    end;
    if aReader.TokenKind <> mtkArrayBegin then UnexpectedToken(mtkArrayBegin, aReader.TokenKind);
    if aReader.StructUnread <> 2 then InvalidVarFormat;
    ReadNext;
    if aReader.TokenKind <> mtkInt then InvalidVarFormat;
    if (aReader.AsInt < 0) or (aReader.AsInt > System.High(Word)) then InvalidVarFormat;
    vType := aReader.AsInt;
    ReadNext;
    case vType of
      varSmallInt:
        begin
          if aReader.TokenKind <> mtkInt then InvalidVarFormat;
          I := aReader.AsInt;
          if (I < System.Low(SmallInt)) or (I > System.High(SmallInt)) then
            InvalidVarFormat;
          Variant(aData^) := SmallInt(I);
        end;
      varInteger:
        begin
          if aReader.TokenKind <> mtkInt then InvalidVarFormat;
          I := aReader.AsInt;
          if (I < System.Low(LongInt)) or (I > System.High(LongInt)) then InvalidVarFormat;
          Variant(aData^) := LongInt(I);
        end;
      varSingle:
        begin
          if aReader.TokenKind <> mtkSingle then InvalidVarFormat;
          VarClear(Variant(aData^));
          TVarData(aData^).vType := vType;
          TVarData(aData^).vSingle := aReader.AsSingle;
        end;
      varDouble:
        begin
          if aReader.TokenKind <> mtkDouble then InvalidVarFormat;
          Variant(aData^) := aReader.AsDouble;
        end;
      varCurrency:
        begin
          if aReader.TokenKind <> mtkInt then InvalidVarFormat;
          I := aReader.AsInt;
          Variant(aData^) := PCurrency(@I)^;
        end;
      varDate:
        begin
          if aReader.TokenKind <> mtkDouble then InvalidVarFormat;
          Variant(aData^) := TDateTime(aReader.AsDouble);
        end;
      varOleStr:
        begin
          if aReader.TokenKind <> mtkString then InvalidVarFormat;
          Variant(aData^) := widestring(aReader.AsString);
        end;
      varBoolean:
        begin
          if aReader.TokenKind <> mtkBool then InvalidVarFormat;
          Variant(aData^) := aReader.AsBoolean;
        end;
      varShortInt:
        begin
          if aReader.TokenKind <> mtkInt then InvalidVarFormat;
          I := aReader.AsInt;
          if (I < System.Low(ShortInt)) or (I > System.High(ShortInt)) then InvalidVarFormat;
          Variant(aData^) := ShortInt(I);
        end;
      varByte:
        begin
          if aReader.TokenKind <> mtkInt then InvalidVarFormat;
          I := aReader.AsInt;
          if (I < 0) or (I > System.High(Byte)) then InvalidVarFormat;
          Variant(aData^) := Byte(I);
        end;
      varWord:
        begin
          if aReader.TokenKind <> mtkInt then InvalidVarFormat;
          I := aReader.AsInt;
          if (I < 0) or (I > System.High(Word)) then InvalidVarFormat;
          Variant(aData^) := Word(I);
        end;
      varLongWord:
        begin
          if aReader.TokenKind <> mtkInt then InvalidVarFormat;
          I := aReader.AsInt;
          if (I < 0) or (I > System.High(LongWord)) then InvalidVarFormat;
          Variant(aData^) := LongWord(I);
        end;
      varInt64:
        begin
          if aReader.TokenKind <> mtkInt then InvalidVarFormat;
          Variant(aData^) := aReader.AsInt;
        end;
      varQWord:
        begin
          if aReader.TokenKind <> mtkInt then InvalidVarFormat;
          Variant(aData^) := QWord(aReader.AsInt);
        end;
      varString:
        begin
          if aReader.TokenKind <> mtkString then InvalidVarFormat;
          Variant(aData^) := aReader.AsString;
        end;
      MPACK_VAR_ARRAY:
        begin
          a := nil;
          ReadDynArray(TypeInfo(a), @a);
          Variant(aData^) := VarArrayOf(a);
        end;
    else
      Error(SEVariantNotSupportFmt, [vType]);
    end;
    ReadNext;
    if aReader.TokenKind <> mtkArrayEnd then UnexpectedToken(mtkArrayEnd, aReader.TokenKind);
  end;

  procedure ReadValue(aTypeInfo: PTypeInfo; aData: Pointer); forward;

  procedure ReadArray(aTypeInfo: PTypeInfo; aData: Pointer);
  var
    TypData: PTypeData;
    ElSize: SizeUInt;
    ElType: PTypeInfo;
    Arr: PByte;
    I, Count: SizeInt;
  begin
    if aReader.TokenKind <> mtkArrayBegin then UnexpectedToken(mtkArrayBegin, aReader.TokenKind);
    TypData := GetTypeData(aTypeInfo);
    ElType := TypData^.ArrayData.ElType;
    Count := TypData^.ArrayData.ElCount;
    ElSize := TypData^.ArrayData.Size div Count;
    if aReader.StructUnread <> Count then Error(SEMPackArrSizeMismatchFmt, [Count]);
    Arr := aData;
    for I := 0 to Pred(Count) do begin
      ReadNext;
      ReadValue(ElType, Arr);
      Arr += ElSize;
    end;
    ReadNext;
    if aReader.TokenKind <> mtkArrayEnd then UnexpectedToken(mtkArrayEnd, aReader.TokenKind);
  end;

  procedure ReadDynArray(aTypeInfo: PTypeInfo; aData: Pointer);
  var
    LocElSize: SizeUInt;
    LocElType: PTypeInfo;
    Arr: PByte;
    I, Size: SizeInt;
  begin
    if aReader.TokenKind <> mtkArrayBegin then UnexpectedToken(mtkArrayBegin, aReader.TokenKind);
    Size := aReader.StructUnread;
    DynArraySetLength(Pointer(aData^), aTypeInfo, 1, @Size);
    with GetTypeData(aTypeInfo)^ do begin
      LocElSize := ElSize;
      LocElType := ElType2;
    end;
    Arr := PByte(aData^);
    for I := 0 to Pred(Size) do begin
      ReadNext;
      ReadValue(LocElType, Arr);
      Arr += LocElSize;
    end;
    ReadNext;
    if aReader.TokenKind <> mtkArrayEnd then UnexpectedToken(mtkArrayEnd, aReader.TokenKind);
  end;

type
  TLocCacheEntry = record
    FieldMap: TRecFieldMap;
    Tested: Boolean;
  end;

  TLocalCacheType = specialize TGLiteEquatableHashMap<PTypeInfo, TLocCacheEntry, Pointer>;
  TLocalCache     = TLocalCacheType.TMap;

var
  LocalCache: TLocalCache;

  function GetFieldMap(aTypeInfo: PTypeInfo; out aFieldMap: TRecFieldMap): Boolean;
  var
    pe: ^TLocCacheEntry;
    pce: TPdoCacheEntry;
  begin
    pe := LocalCache.GetMutValueDef(aTypeInfo, Default(TLocCacheEntry));
    if not pe^.Tested then begin
      if GetGlobPdoEntry(aTypeInfo, pce) then
        pe^.FieldMap := pce.FieldMap;
      pe^.Tested := True;
    end;
    aFieldMap := pe^.FieldMap;
    Result := aFieldMap <> nil;
  end;

  function FindFieldName(const aName: string; const aMap: TRecFieldMap): Integer;
  var
    I: Integer;
  begin
    if aName <> '' then
      for I := 0 to System.High(aMap) do
        if aName = aMap[I].Name then exit(I);
    Result := -1;
  end;

  procedure ReadRecord(aTypeInfo: PTypeInfo; aData: Pointer);
  var
    Map: TRecFieldMap;
    pTypData: PTypeData;
    pField: PManagedField;
    I, J: Integer;
  const
    MAP_KEY_NAMES: array[TMpVarKind] of string = ('Null', 'Integer', 'String', 'Bytes');
  begin
    if aReader.TokenKind = mtkExt then begin
      if (aUserExt <> nil) and aUserExt.CanRead(aData, aTypeInfo, aReader) then
        exit
      else
        Error(SEMPackExtNotSupportFmt, [TUserExtType(aReader.AsExtention[0])]);
    end;
    if GetFieldMap(aTypeInfo, Map) then begin
      if aReader.TokenKind <> mtkMapBegin then UnexpectedToken(mtkMapBegin, aReader.TokenKind);
      for I := 0 to Pred(aReader.StructUnread) do begin
        ReadNext;
        if aReader.KeyValue.Kind <> mvkStr then
          Error(SEMPackNonStrFieldNameFmt, [MAP_KEY_NAMES[aReader.KeyValue.Kind]]);
        J := FindFieldName(aReader.KeyValue.AsString, Map);
        if J = -1 then begin
          if aReader.TokenKind in MP_START_TOKENS then
            aReader.Skip;
          continue;
        end;
        ReadValue(Map[J].Info, PByte(aData) + Map[J].Offset);
      end;
      ReadNext;
      if aReader.TokenKind <> mtkMapEnd then UnexpectedToken(mtkMapEnd, aReader.TokenKind);
    end else begin
      if aReader.TokenKind <> mtkArrayBegin then UnexpectedToken(mtkArrayBegin, aReader.TokenKind);
      pTypData := GetTypeData(aTypeInfo);
      pField := PManagedField(AlignTypeData(PByte(@pTypData^.TotalFieldCount) + SizeOf(pTypData^.TotalFieldCount)));
      for I := 0 to Pred(pTypData^.TotalFieldCount) do begin
        ReadNext;
        ReadValue(pField^.TypeRef, PByte(aData) + pField^.FldOffset);
        Inc(pField);
      end;
      ReadNext;
      if aReader.TokenKind <> mtkArrayEnd then UnexpectedToken(mtkArrayEnd, aReader.TokenKind);
    end;
  end;

  procedure ReadClassProp(o: TObject; aPropInfo: PPropInfo);
  var
    pTypInfo: PTypeInfo;
    I: Int64 = 0;
    e: Extended = 0;
    v: Variant;
    ss: shortstring;
    p: Pointer = nil;
  begin
    pTypInfo := aPropInfo^.PropType;
    case pTypInfo^.Kind of
      tkInteger, tkChar, tkEnumeration, tkInt64, tkQWord, tkWChar, tkBool, tkUChar:
        begin
          ReadValue(pTypInfo, @I);
          SetOrdProp(o, aPropInfo, I);
        end;
      tkFloat:
        begin
          ReadFloat(pTypInfo, @e);
          SetFloatProp(o, aPropInfo, e);
        end;
      tkSet:
        begin
          ReadValue(pTypInfo, @I);
          SetOrdProp(o, aPropInfo, I);
        end;
      tkSString:
        begin
          ReadValue(pTypInfo, @ss);
          SetStrProp(o, aPropInfo, ss);
        end;
      tkLString, tkAString:
        begin
          ReadValue(pTypInfo, @p);
          SetStrProp(o, aPropInfo, string(p));
          string(p) := '';
        end;
      tkWString:
        begin
          ReadValue(pTypInfo, @p);
          SetWideStrProp(o, aPropInfo, widestring(p));
          widestring(p) := '';
        end;
      tkVariant:
        begin
          ReadVariant(@v);
          SetVariantProp(o, aPropInfo, v);
        end;
      tkClass:
        begin
          ReadValue(pTypInfo, @p);
          SetObjectProp(o, aPropInfo, TObject(p));
        end;
      tkDynArray:
        begin
          ReadDynArray(pTypInfo, @p);
          SetDynArrayProp(o, aPropInfo, p);
          DynArrayClear(p, pTypInfo);
        end;
      tkUString:
        begin
          ReadValue(pTypInfo, @p);
          SetUnicodeStrProp(o, aPropInfo, unicodestring(p));
          unicodestring(p) := '';
        end;
    else
      Error(SEUnsuppPdoPropKindFmt, [GetEnumName(TypeInfo(TTypeKind), Integer(pTypInfo^.Kind))]);
    end;
  end;

type
  TPropMapType = specialize TGLiteChainHashMap<string, PPropInfo, string>;
  TPropMap     = TPropMapType.TMap;

  procedure ReadMappedClass(o: TObject; const aMap: TPropMap);
  var
    Count, I: SizeInt;
    pInfo: PPropInfo;
    s: string;
  begin
    //here aReader.TokenKind already checked
    Count := aReader.StructUnread;
    for I := 0 to Pred(Count) do begin
      ReadNext;
      if aReader.KeyValue.Kind <> mvkStr then Error(
        SEMPackUnexpectKeyFmt, [
          GetEnumName(TypeInfo(TMpVarKind), Integer(mvkStr)),
          GetEnumName(TypeInfo(TMpVarKind), Integer(aReader.KeyValue.Kind))
        ]
      );
      s := aReader.KeyValue.AsString;
      if aMap.TryGetValue(s, pInfo) then
        ReadClassProp(o, pInfo)
      else
        Error(SEClassPropNotFoundFmt, [o.ClassName, s]);
    end;
    ReadNext;
    if aReader.TokenKind <> mtkMapEnd then UnexpectedToken(mtkMapEnd, aReader.TokenKind);
  end;

  procedure GetPropMap(aList: PPropList; aCount: Integer; out aMap: TPropMap);
  var
    I: Integer;
  begin
    aMap.EnsureCapacity(aCount);
    for I := 0 to Pred(aCount) do
      if IsWriteableProp(aList^[I]) then
        aMap.Add(aList^[I]^.Name, aList^[I]);
  end;

  procedure GetPropMap(o: TObject; out aMap: TPropMap);
  var
    pl: PPropList;
    Count: Integer;
  begin
    Count := GetPropList(o, pl);
    try
      GetPropMap(pl, Count, aMap);
    finally
      FreeMem(pl);
    end;
  end;

  procedure ReadSimpleClass(o: TObject);
  var
    Map: TPropMap;
  begin
    if aReader.TokenKind <> mtkMapBegin then UnexpectedToken(mtkMapBegin, aReader.TokenKind);
    GetPropMap(o, Map);
    ReadMappedClass(o, Map);
  end;

  procedure GetPropMap(aClass: TClass; out aMap: TPropMap);
  var
    pl: PPropList;
    Count: Integer;
  begin
    Count := GetPropList(aClass, pl);
    try
      GetPropMap(pl, Count, aMap);
    finally
      FreeMem(pl);
    end;
  end;

  procedure ReadCollection(aCol: TCollection);
  var
    Map: TPropMap;
    I, Count: SizeInt;
  begin
    if aReader.TokenKind <> mtkArrayBegin then UnexpectedToken(mtkArrayBegin, aReader.TokenKind);
    Count := aReader.StructUnread;
    aCol.Clear;
    GetPropMap(aCol.ItemClass, Map);
    for I := 0 to Pred(Count) do begin
      ReadNext;
      ReadMappedClass(aCol.Add, Map);
    end;
    ReadNext;
    if aReader.TokenKind <> mtkArrayEnd then UnexpectedToken(mtkArrayEnd, aReader.TokenKind);
  end;

  procedure ReadStrings(aStrings: TStrings);
  var
    s: string;
    I, Count: SizeInt;
  begin
    if aReader.TokenKind <> mtkArrayBegin then UnexpectedToken(mtkArrayBegin, aReader.TokenKind);
    Count := aReader.StructUnread;
    aStrings.Clear;
    for I := 0 to Pred(Count) do begin
      ReadNext;
      s := '';
      ReadString(@s);
      aStrings.Add(s);
    end;
    ReadNext;
    if aReader.TokenKind <> mtkArrayEnd then UnexpectedToken(mtkArrayEnd, aReader.TokenKind);
  end;

  procedure ReadClass(aTypeInfo: PTypeInfo; aData: Pointer);
  var
    o: TObject;
  begin
    if aReader.TokenKind = mtkExt then begin
      if (aUserExt <> nil) and aUserExt.CanRead(aData, aTypeInfo, aReader) then
        exit
      else
        Error(SEMPackExtNotSupportFmt, [TUserExtType(aReader.AsExtention[0])]);
    end else
      if aReader.TokenKind = mtkNil then begin
        FreeAndNil(TObject(aData^));
        exit;
      end;
    if TObject(aData^) = nil then
      TObject(aData^) := GetTypeData(aTypeInfo)^.ClassType.Create;
    o := TObject(aData^);
    if o is TCollection then
      ReadCollection(TCollection(o))
    else
      if o is TStrings then
        ReadStrings(TStrings(o))
      else
        ReadSimpleClass(o);
  end;

  procedure ReadObject(aTypeInfo: PTypeInfo; aData: Pointer);
  begin
    if aReader.TokenKind = mtkExt then
      if (aUserExt <> nil) and aUserExt.CanRead(aData, aTypeInfo, aReader) then
        exit
      else
        Error(SEMPackExtNotSupportFmt, [TUserExtType(aReader.AsExtention[0])]);
    NotSupported(aTypeInfo);
  end;

  procedure ReadWChar(aData: Pointer);
  var
    ws: widestring = '';
  begin
    ReadWString(@ws);
    if System.Length(ws) <> 1 then
      Error(SEStrLenNotMatchSizeFmt, ['WideString', System.Length(ws), 'WideChar']);
    PWideChar(aData)^ := ws[1];
  end;

  procedure ReadBool(aTypeInfo: PTypeInfo; aData: Pointer);
  begin
    if aReader.TokenKind <> mtkBool then UnexpectedToken(mtkBool, aReader.TokenKind);
    case GetTypeData(aTypeInfo)^.OrdType of
      otSByte:  PShortInt(aData)^ := Ord(aReader.AsBoolean);
      otUByte:  PByte(aData)^ := Ord(aReader.AsBoolean);
      otSWord:  PSmallInt(aData)^ := Ord(aReader.AsBoolean);
      otUWord:  PWord(aData)^ := Ord(aReader.AsBoolean);
      otSLong:  PLongInt(aData)^ := Ord(aReader.AsBoolean);
      otULong:  PDword(aData)^ := Ord(aReader.AsBoolean);
      otSQWord: PInt64(aData)^ := Ord(aReader.AsBoolean);
      otUQWord: PQWord(aData)^ := Ord(aReader.AsBoolean);
    end;
  end;

  procedure ReadUChar(aData: Pointer);
  var
    us: unicodestring = '';
  begin
    ReadUString(@us);
    if System.Length(us) <> 1 then
      Error(SEStrLenNotMatchSizeFmt, ['UnicodeString', System.Length(us), 'UnicodeChar']);
    PUnicodeChar(aData)^ := us[1];
  end;

  procedure ReadValue(aTypeInfo: PTypeInfo; aData: Pointer);
  begin
    case aTypeInfo^.Kind of
      tkInteger, tkInt64,
      tkQWord:       ReadInteger(aTypeInfo, aData);
      tkFloat:       ReadFloat(aTypeInfo, aData);
      tkChar:        ReadChar(aData);
      tkEnumeration: ReadEnum(aTypeInfo, aData);
      tkSet:         ReadSet(aTypeInfo, aData);
      tkSString:     ReadSString(aTypeInfo, aData);
      tkLString,
      tkAString:     ReadString(aData);
      tkWString:     ReadWString(aData);
      tkVariant:     ReadVariant(aData);
      tkArray:       ReadArray(aTypeInfo, aData);
      tkRecord:      ReadRecord(aTypeInfo, aData);
      tkClass:       ReadClass(aTypeInfo, aData);
      tkObject:      ReadObject(aTypeInfo, aData);
      tkWChar:       ReadWChar(aData);
      tkBool:        ReadBool(aTypeInfo, aData);
      tkDynArray:    ReadDynArray(aTypeInfo, aData);
      tkUString:     ReadUString(aData);
      tkUChar:       ReadUChar(aData);
    else
      NotSupported(aTypeInfo);
    end;
  end;
begin
  if aReader.ReadState = mrsStart then ReadNext;
  ReadValue(aTypeInfo, @aValue);
end;

procedure MsgPackToPdo(aTypeInfo: PTypeInfo; var aValue; aReader: TMpCustomReader; aUserExt: IMpUserExt);
begin
  try
    MsgPack2Pdo(aTypeInfo, aValue, aReader, aUserExt);
    if aReader.ReadState = mrsRead then aReader.Read;
    if aReader.ReadState <> mrsEof then
      raise EPdoLoadMsgPack.Create(SEMPackTrailingGarbage);
  except
    on e: EPdoLoadMsgPack do raise;
    on e: Exception do
      raise EPdoLoadMsgPack.CreateFmt(SEExceptWhenMPackLoadFmt, [e.ClassName, e.Message]);
  end;
end;

generic procedure PdoLoadMsgPack<T>(var aValue: T; const aBuffer: rawbytestring; aUserExt: IMpUserExt;
  aMaxDepth: Integer);
begin
  PdoLoadMsgPack(TypeInfo(aValue), aValue, aBuffer, aUserExt);
end;

procedure PdoLoadMsgPack(aTypeInfo: PTypeInfo; var aValue; const aBuffer: rawbytestring; aUserExt: IMpUserExt;
  aMaxDepth: Integer);
var
  Reader: TMpReader;
begin
  Reader := TMpReader.Create(Pointer(aBuffer), System.Length(aBuffer), aMaxDepth);
  try
    MsgPackToPdo(aTypeInfo, aValue, Reader, aUserExt);
  finally
    Reader.Free;
  end;
end;

generic procedure PdoLoadMsgPack<T>(var aValue: T; const aBuffer: TBytes; aUserExt: IMpUserExt;
  aMaxDepth: Integer);
begin
  PdoLoadMsgPack(TypeInfo(aValue), aValue, aBuffer, aUserExt);
end;

procedure PdoLoadMsgPack(aTypeInfo: PTypeInfo; var aValue; const aBuffer: TBytes; aUserExt: IMpUserExt;
  aMaxDepth: Integer);
var
  Reader: TMpReader;
begin
  Reader := TMpReader.Create(Pointer(aBuffer), System.Length(aBuffer), aMaxDepth);
  try
    MsgPackToPdo(aTypeInfo, aValue, Reader, aUserExt);
  finally
    Reader.Free;
  end;
end;

initialization
  GlobLock := TMultiReadExclusiveWriteSynchronizer.Create;
finalization
  GlobLock := nil;
end.

