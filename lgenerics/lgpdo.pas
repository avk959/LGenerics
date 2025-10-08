{****************************************************************************
*                                                                           *
*   This file is part of the LGenerics package.                             *
*   Plain Data Objects marshalling.                                         *
*                                                                           *
*   Copyright(c) 2022-2025 A.Koverdyaev(avk)                                *
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

{$MODE OBJFPC}{$H+}{$MODESWITCH ADVANCEDRECORDS}{$MODESWITCH TYPEHELPERS}

interface

uses
  Classes, SysUtils, TypInfo, lgUtils, lgHelpers, lgJson;

{ PDO - Plain Data Objects is a conventional collective name for Pascal data structures
  that can be user-transparently(well, almost) stored/loaded to/from another format(currently only JSON):
   - numeric, boolean or string types, some limited support of Variant;
   - enumerations(are stored in string form as the name of the corresponding constant);
   - sets(are stored as an array of its element);
   - regular records, it is possible to register a list of field names or a custom callback;
   - classes, using published properties or by registering a custom callback(TStrings and
     TCollection as a special case);
   - objects(only by registering a serialization callback);
   - static arrays of PDO(only one-dimensional, multidimensional arrays are written as one-dimensional);
   - dynamic arrays of PDO;
   - variant arrays(currently only one-dimensional);
}

const
  DEFAULT_LEN = lgJson.TJsonStrWriter.DEFAULT_LEN;

{ saves PDO in JSON format; if aStrict is False, unsupported data types will be written as
  "unknown data"(see UNKNOWN_ALIAS), otherwise an exception will be raised;
  fields of unregistered records will be named as "field1, field2, ..."(see FIELD_ALIAS) }
  function PdoToJson(aTypeInfo: PTypeInfo; const aValue; aInitWriterLen: Integer = DEFAULT_LEN;
                     aStrict: Boolean = False): string;
  generic function PdoToJson<T>(const aValue: T; aInitWriterLen: Integer = DEFAULT_LEN;
                                aStrict: Boolean = False): string;
  { the type being registered must be a record; associates the field names aFieldNames with
    the record fields by their indexes; to exclude a field from serialization, it is sufficient
    to specify its name as an empty string; returns True on successful registration;
    has a higher priority than custom callbacks }
  function RegisterRecordFields(aTypeInfo: PTypeInfo; const aFieldNames: TStringArray): Boolean;
  function RecordFieldsRegistered(aTypeInfo: PTypeInfo; out aFieldNames: TStringArray): Boolean;
  function UnregisterRecordFields(aTypeInfo: PTypeInfo): Boolean;
  function UnregisterPdo(aTypeInfo: PTypeInfo): Boolean;

type
  EPdoStoreJson  = class(Exception);
  TJsonStrWriter = lgJson.TJsonStrWriter;
  TPdoToJsonProc = procedure(p: Pointer; aWriter: TJsonStrWriter);

{ associates a custom JSON serialization routine with a type, which must be a record, class or object;
  only one routine can be associated with each type; returns True if registration is successful }
  function RegisterPdoToJsonProc(aTypeInfo: PTypeInfo; aProc: TPdoToJsonProc): Boolean;
  function PdoToJsonProcRegistered(aTypeInfo: PTypeInfo): Boolean;
  function UnregisterPdoToJsonProc(aTypeInfo: PTypeInfo): Boolean;

const
  UNKNOWN_ALIAS = 'unknown data';
  FIELD_ALIAS   = 'field';
  SUPPORT_KINDS = [
    tkInteger, tkChar, tkEnumeration, tkFloat, tkSet, tkSString, tkLString,
    tkAString, tkWString, tkVariant, tkArray, tkRecord, tkClass, tkObject,
    tkWChar, tkBool, tkInt64, tkQWord, tkDynArray, tkUString, tkUChar];

  DEFAULT_DEPTH = TJsonReader.DEF_DEPTH;

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


type
  TOptString = specialize TGOptional<string>;

  TOStrHelper = type helper for TOptString
    class procedure WriteJson(p: Pointer; aWriter: TJsonStrWriter); static;
    class function  ReadJson(p: Pointer; aReader: TJsonReader; const aOpts: TJsonReadOptions): Boolean; static;
  end;

  TOptBoolean = specialize TGOptional<Boolean>;

  TOBoolHelper = type helper for TOptBoolean
    class procedure WriteJson(p: Pointer; aWriter: TJsonStrWriter); static;
    class function  ReadJson(p: Pointer; aReader: TJsonReader; const aOpts: TJsonReadOptions): Boolean; static;
  end;

  TOptShortInt = specialize TGOptional<ShortInt>;

  TOShortIntHelper = type helper for TOptShortInt
    class procedure WriteJson(p: Pointer; aWriter: TJsonStrWriter); static;
    class function  ReadJson(p: Pointer; aReader: TJsonReader; const aOpts: TJsonReadOptions): Boolean; static;
  end;

  TOptByte = specialize TGOptional<Byte>;

  TOptByteHelper = type helper for TOptByte
    class procedure WriteJson(p: Pointer; aWriter: TJsonStrWriter); static;
    class function  ReadJson(p: Pointer; aReader: TJsonReader; const aOpts: TJsonReadOptions): Boolean; static;
  end;

  TOptSmallInt = specialize TGOptional<SmallInt>;

  TOSmallIntHelper = type helper for TOptSmallInt
    class procedure WriteJson(p: Pointer; aWriter: TJsonStrWriter); static;
    class function  ReadJson(p: Pointer; aReader: TJsonReader; const aOpts: TJsonReadOptions): Boolean; static;
  end;

  TOptWord = specialize TGOptional<Word>;

  TOptWordHelper = type helper for TOptWord
    class procedure WriteJson(p: Pointer; aWriter: TJsonStrWriter); static;
    class function  ReadJson(p: Pointer; aReader: TJsonReader; const aOpts: TJsonReadOptions): Boolean; static;
  end;

  TOptInteger = specialize TGOptional<Integer>;

  TOIntHelper = type helper for TOptInteger
    class procedure WriteJson(p: Pointer; aWriter: TJsonStrWriter); static;
    class function  ReadJson(p: Pointer; aReader: TJsonReader; const aOpts: TJsonReadOptions): Boolean; static;
  end;

  TOptCardinal = specialize TGOptional<Cardinal>;

  TOCardinalHelper = type helper for TOptCardinal
    class procedure WriteJson(p: Pointer; aWriter: TJsonStrWriter); static;
    class function  ReadJson(p: Pointer; aReader: TJsonReader; const aOpts: TJsonReadOptions): Boolean; static;
  end;

  TOptDouble = specialize TGOptional<Double>;

  TODoubleHelper = type helper for TOptDouble
    class procedure WriteJson(p: Pointer; aWriter: TJsonStrWriter); static;
    class function  ReadJson(p: Pointer; aReader: TJsonReader; const aOpts: TJsonReadOptions): Boolean; static;
  end;

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

function GetPdoEntry(aTypeInfo: Pointer; out e: TPdoCacheEntry): Boolean;
begin
  GlobLock.BeginRead;
  try
    Result := PdoCache.TryGetValue(aTypeInfo, e);
  finally
    GlobLock.EndRead;
  end;
end;

function RegisterRecordFields(aTypeInfo: PTypeInfo; const aFieldNames: TStringArray): Boolean;
begin
  if (aTypeInfo = nil) or (aTypeInfo^.Kind <> tkRecord) then exit(False);
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

generic function PdoToJson<T>(const aValue: T; aInitWriterLen: Integer; aStrict: Boolean = False): string;
begin
  Result := PdoToJson(TypeInfo(T), aValue, aInitWriterLen, aStrict);
end;

{$PUSH}{$WARN 5089 OFF : Local variable "$1" of a managed type does not seem to be initialized}
function PdoToJson(aTypeInfo: PTypeInfo; const aValue; aInitWriterLen: Integer; aStrict: Boolean): string;
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
    Writer.BeginObject;
    for I := 0 to Pred(pTypData^.TotalFieldCount) do
      begin
        Writer.AddName(FIELD_ALIAS + Succ(I).ToString);
        WriteField(pManField^.TypeRef, PByte(aData) + pManField^.FldOffset);
        Inc(pManField);
      end;
    Writer.EndObject;
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
      varDate, varByte, varWord, varLongWord, varInt64, varDecimal, varQWord:
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
      if aStrict then
        raise EPdoStoreJson.CreateFmt(SEVariantNotSupportFmt, [VarType(v)]);
      Writer.Add(UNKNOWN_ALIAS);
    end;
  end;
  procedure WriteSet(aTypeInfo: PTypeInfo; aData: Pointer); forward;
  procedure WriteClass(aTypeInfo: PTypeInfo; o: TObject); forward;
  function UnknownAlias(aTypeInfo: PTypeInfo): string;
  begin
    if aStrict then
      raise EPdoStoreJson.CreateFmt(SEPdoTypeNotSupportFmt,
        [aTypeInfo^.Name, GetEnumName(TypeInfo(TTypeKind), Integer(aTypeInfo^.Kind))]);
    Result := UNKNOWN_ALIAS;
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
      Writer.Add(UnknownAlias(pTypInfo));
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
          end else begin // todo: if PropCount = 0 ???
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
    e := DEF_CACHE_ENTRY;
    if GetPdoEntry(aTypeInfo, e) and (e.PdoToJsonProc <> nil) then
      TPdoToJsonProc(e.PdoToJsonProc)(aData, Writer)
    else
      Writer.Add(UnknownAlias(aTypeInfo));// todo: empty object ???
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
      Writer.Add(UnknownAlias(aTypeInfo));
    end;
  end;
var
  WriterRef: specialize TGUniqRef<TJsonStrWriter>;
begin
  Result := '';
  if aTypeInfo = nil then exit;
  WriterRef.Instance := TJsonStrWriter.Create(aInitWriterLen);
  Writer := WriterRef;
  GlobLock.BeginRead;
  try
    WriteField(aTypeInfo, @aValue);
  finally
    GlobLock.EndRead;
  end;
  Result := Writer.JsonString;
end;
{$POP}

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

type
  TRecFieldInfo = record
    Info: PTypeInfo;
    Offset: Integer;
    constructor Make(aInfo: PTypeInfo; aOffset: Integer);
  end;

constructor TRecFieldInfo.Make(aInfo: PTypeInfo; aOffset: Integer);
begin
  Info := aInfo;
  Offset := aOffset;
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
  procedure ReadInteger(aTypeInfo: PTypeInfo; aData: Pointer);
  var
    I: Int64;
  begin
    if Reader.TokenKind <> tkNumber then
      Error(Format(SEUnexpectJsonTokenFmt, [TokenKindName(tkNumber), TokenKindName(Reader.TokenKind)]));
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
    if Reader.TokenKind <> tkNumber then
      Error(Format(SEUnexpectJsonTokenFmt, [TokenKindName(tkNumber), TokenKindName(Reader.TokenKind)]));
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
    if Reader.TokenKind <> tkString then
      Error(Format(SEUnexpectJsonTokenFmt,[TokenKindName(tkString), TokenKindName(Reader.TokenKind)]));
    s := Reader.AsString;
    if System.Length(s) <> 1 then
      Error(Format(SEUnexpectJsonStrLenFmt, [System.Length(s)]));
    PChar(aData)^ := s[1];
  end;
  procedure ReadEnum(aTypeInfo: PTypeInfo; aData: Pointer);
  var
    s: string;
    I: Integer;
  begin
    if Reader.TokenKind <> tkString then
      Error(Format(SEUnexpectJsonTokenFmt,[TokenKindName(tkString), TokenKindName(Reader.TokenKind)]));
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
    if not(Reader.TokenKind in [tkFalse, tkTrue]) then
      Error(Format(SEUnexpectJsonTokenFmt2, [TokenKindName(tkFalse), TokenKindName(tkTrue),
                   TokenKindName(Reader.TokenKind)]));
    b := Reader.TokenKind = tkTrue;
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
  procedure ReadDynArray(aTypeInfo: PTypeInfo; aData: Pointer); forward;
  procedure ReadValue(aTypeInfo: PTypeInfo; aData: Pointer); forward;
  procedure ReadRecord(aTypeInfo: PTypeInfo; aData: Pointer);
  var
    Map: TRecFieldMap;
    procedure CopyFieldMap(const aMap: TRecFieldMap);
    var
      I, J: Integer;
    begin
      System.SetLength(Map, System.Length(aMap));
      J := 0;
      for I := 0 to System.High(aMap) do
        if aMap[I].Name <> '' then
          begin
            Map[J] := aMap[I];
            if IgnoreNameCase then
              Map[J].Name := LowerCase(Map[J].Name);
            Inc(J);
          end;
      System.SetLength(Map, J);
    end;
    function IndexOf(const aName: string): Integer;
    var
      I: Integer;
      s: string;
    begin
      if IgnoreNameCase then
        s := LowerCase(aName)
      else
        s := aName;
      for I := 0 to System.High(Map) do
        if Map[I].Name = s then exit(I);
      Result := NULL_INDEX;
    end;
  var
    I: Integer;
    e: TPdoCacheEntry;
  begin
    e := DEF_CACHE_ENTRY;
    Map := nil;
    if not GetPdoEntry(aTypeInfo, e) then
      Error(Format(SEUnsupportPdoTypeFmt, [aTypeInfo^.Name]));
    if e.FieldMap <> nil then begin
      if Reader.TokenKind <> tkObjectBegin then
        Error(Format(SEUnexpectJsonTokenFmt,[TokenKindName(tkObjectBegin), TokenKindName(Reader.TokenKind)]));
      CopyFieldMap(e.FieldMap);
      repeat
        ReadNext;
        if Reader.TokenKind = tkObjectEnd then break;
        I := IndexOf(Reader.Name);
        if  I < 0 then
          begin
            if not SkipUnknownProps then
              Error(Format(SERecordFieldNotFoundFmt, [aTypeInfo^.Name, Reader.Name]));
            if TJsonReader.IsStartToken(Reader.TokenKind) then
              Reader.Skip;
          end
        else
          ReadValue(Map[I].Info, PByte(aData) + Map[I].Offset);
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
      tkNull:
        begin
          CheckNull;
          Variant(aData^) := Null;
        end;
      tkFalse: Variant(aData^) := False;
      tkTrue: Variant(aData^) := True;
      tkNumber:
        begin
          d := Reader.AsNumber;
          if Double.IsExactInt(d, I) then
            Variant(aData^) := I
          else
            Variant(aData^) := d;
        end;
      tkString: Variant(aData^) := Reader.AsString;
      tkArrayBegin:
        begin
          a := nil;
          ReadDynArray(TypeInfo(a), @a);
          Variant(aData^) := VarArrayOf(a);
        end;
    else
      Error(Format(SECantReadJson2VarFmt, [TokenKindName(Reader.TokenKind)]))
    end;
  end;

type
  TFieldsMapType = specialize TGLiteChainHashMap<string, TRecFieldInfo, string>;
  TRecFieldsMap  = TFieldsMapType.TMap;

  procedure ReadMappedRec(aTypeInfo: PTypeInfo; aData: Pointer; const aMap: TRecFieldsMap);
  var
    rfi: TRecFieldInfo;
    s: string;
  begin
    if Reader.TokenKind <> tkObjectBegin then
      Error(Format(SEUnexpectJsonTokenFmt,[TokenKindName(tkObjectBegin), TokenKindName(Reader.TokenKind)]));
    repeat
      ReadNext;
      if Reader.TokenKind = tkObjectEnd then break;
      if IgnoreNameCase then
        s := LowerCase(Reader.Name)
      else
        s := Reader.Name;
      if not aMap.TryGetValue(s, rfi) then
        begin
          if not SkipUnknownProps then
            Error(Format(SERecordFieldNotFoundFmt, [aTypeInfo^.Name, Reader.Name]));
          if TJsonReader.IsStartToken(Reader.TokenKind) then
            Reader.Skip;
        end
      else
        ReadValue(rfi.Info, PByte(aData) + rfi.Offset);
    until False;
  end;
  procedure ReadArray(aTypeInfo: PTypeInfo; aData: Pointer);
  var
    pTypData: PTypeData;
    ElSize: SizeUInt;
    ElType: PTypeInfo;
    Arr: PByte;
    I, Count: SizeInt;
    Map: TRecFieldsMap;
    e: TPdoCacheEntry;
  begin
    if Reader.TokenKind <> tkArrayBegin then
      Error(Format(SEUnexpectJsonTokenFmt, [TokenKindName(tkArrayBegin), TokenKindName(Reader.TokenKind)]));
    e := DEF_CACHE_ENTRY;
    pTypData := GetTypeData(aTypeInfo);
    ElType := pTypData^.ArrayData.ElType;
    Count := pTypData^.ArrayData.ElCount;
    ElSize := pTypData^.ArrayData.Size div Count;
    Arr := aData;
    case ElType^.Kind of
      tkRecord:
        begin
          if not(GetPdoEntry(ElType, e)and((e.FieldMap<>nil)or(e.JsonToPdoProc<>nil))) then
            Error(Format(SEUnsupportPdoTypeFmt, [ElType^.Name]));
          if e.FieldMap <> nil then with e do
            for I := 0 to System.High(FieldMap) do
              with FieldMap[I] do
                if Name <> '' then
                  if IgnoreNameCase then
                    Map.Add(LowerCase(Name), TRecFieldInfo.Make(Info, Offset))
                  else
                    Map.Add(Name, TRecFieldInfo.Make(Info, Offset));
        end;
      tkObject:
          if not(GetPdoEntry(ElType, e)and(e.JsonToPdoProc <> nil)) then
            Error(Format(SEUnsupportPdoTypeFmt, [ElType^.Name]));
    else
    end;
    I := 0;
    repeat
      ReadNext;
      if Reader.TokenKind = tkArrayEnd then break;
      if I = Count then
        Error(Format(SEJsonArraySizeNotFitFmt, [Count]));
      case ElType^.Kind of
        tkRecord:
          if e.FieldMap <> nil then
            ReadMappedRec(ElType, Arr, Map)
          else
            if not TJsonToPdoProc(e.JsonToPdoProc)(Arr, Reader, aOptions) then
              Error(Format(SECallbackFalseRetFmt, [ElType^.Name]));
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
    Map: TRecFieldsMap;
    e: TPdoCacheEntry;
  const
    InitSize = 8;
  begin
    if Reader.TokenKind <> tkArrayBegin then
      if Reader.TokenKind = tkNull then
        begin
          CheckNull;
          DynArrayClear(Pointer(aData^), aTypeInfo);
          exit;
        end
      else
        Error(Format(SEUnexpectJsonTokenFmt, [TokenKindName(tkArrayBegin), TokenKindName(Reader.TokenKind)]));
    ElSize := GetTypeData(aTypeInfo)^.ElSize;
    ElType := GetTypeData(aTypeInfo)^.ElType2;
    e := DEF_CACHE_ENTRY;
    case ElType^.Kind of
      tkRecord:
        begin
          if not(GetPdoEntry(ElType,e)and((e.FieldMap<>nil)or(e.JsonToPdoProc<>nil))) then
            Error(Format(SEUnsupportPdoTypeFmt, [ElType^.Name]));
          if e.FieldMap <> nil then with e do
            for I := 0 to System.High(FieldMap) do
              with FieldMap[I] do
                if Name <> '' then
                  if IgnoreNameCase then
                    Map.Add(LowerCase(Name), TRecFieldInfo.Make(Info, Offset))
                  else
                    Map.Add(Name, TRecFieldInfo.Make(Info, Offset));
        end;
      tkObject:
          if not(GetPdoEntry(ElType, e) and (e.JsonToPdoProc <> nil)) then
            Error(Format(SEUnsupportPdoTypeFmt, [ElType^.Name]));
    else
    end;
    I := NULL_INDEX;
    repeat
      ReadNext;
      Inc(I);
      if Reader.TokenKind = tkArrayEnd then break;
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
          if e.FieldMap <> nil then
            ReadMappedRec(ElType, PByte(aData^) + SizeUInt(I) * ElSize, Map)
          else
            if not TJsonToPdoProc(e.JsonToPdoProc)(PByte(aData^) + SizeUInt(I) * ElSize, Reader, aOptions) then
              Error(Format(SECallbackFalseRetFmt, [ElType^.Name]));
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
      Error(Format(SEUnsupportPdoKindFmt, [GetEnumName(TypeInfo(TTypeKind), Integer(pTypInfo^.Kind))]));
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
    if Reader.TokenKind <> tkObjectBegin then
      Error(Format(SEUnexpectJsonTokenFmt, [TokenKindName(tkObjectBegin), TokenKindName(Reader.TokenKind)]));
    repeat
      ReadNext;
      if Reader.TokenKind = tkObjectEnd then break;
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
    if Reader.TokenKind <> tkObjectBegin then
      Error(Format(SEUnexpectJsonTokenFmt, [TokenKindName(tkObjectBegin), TokenKindName(Reader.TokenKind)]));
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
    if Reader.TokenKind <> tkArrayBegin then
      Error(Format(SEUnexpectJsonTokenFmt, [TokenKindName(tkArrayBegin), TokenKindName(Reader.TokenKind)]));
    aCol.Clear;
    GetPropMap(aCol.ItemClass, Map);
    repeat
      ReadNext;
      if Reader.TokenKind = tkArrayEnd then break;
      ReadMappedClass(aCol.Add, Map);
    until False;
  end;
  procedure ReadString(aTypeInfo: PTypeInfo; aData: Pointer); forward;
  procedure ReadStrings(aStrings: TStrings);
  var
    s: string;
  begin
    if Reader.TokenKind <> tkArrayBegin then
      Error(Format(SEUnexpectJsonTokenFmt, [TokenKindName(tkArrayBegin), TokenKindName(Reader.TokenKind)]));
    aStrings.Clear;
    repeat
      ReadNext;
      if Reader.TokenKind = tkArrayEnd then break;
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
    if Reader.TokenKind = tkNull then begin
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
    e := DEF_CACHE_ENTRY;
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
    e := DEF_CACHE_ENTRY;
    if not(GetPdoEntry(aTypeInfo, e) and (e.JsonToPdoProc <> nil)) then
      Error(Format(SEUnsupportPdoTypeFmt, [aTypeInfo^.Name]));
    if not TJsonToPdoProc(e.JsonToPdoProc)(aData, Reader, aOptions) then
      Error(Format(SECallbackFalseRetFmt, [aTypeInfo^.Name]));
  end;
  procedure ReadSString(aTypeInfo: PTypeInfo; aData: Pointer);
  var
    s: string;
    MaxLen: Byte;
  begin
    if Reader.TokenKind <> tkString then
      if Reader.TokenKind = tkNull then
        begin
          CheckNull;
          PShortString(aData)^ := '';
          exit;
        end
      else
        Error(Format(SEUnexpectJsonTokenFmt,[TokenKindName(tkString), TokenKindName(Reader.TokenKind)]));
    s := Reader.AsString;
    MaxLen := GetTypeData(aTypeInfo)^.MaxLength;
    if System.Length(s) > MaxLen then
      Error(Format(SEJsonStrExceedSSLenFmt, [System.Length(s), MaxLen]));
    System.SetLength(PShortString(aData)^, System.Length(s));
    System.Move(Pointer(s)^, PShortString(aData)^[1], System.Length(PShortString(aData)^));
  end;
  procedure ReadString(aTypeInfo: PTypeInfo; aData: Pointer);
  begin
    if Reader.TokenKind <> tkString then
      if Reader.TokenKind = tkNull then
        begin
          CheckNull;
          PString(aData)^ := '';
          exit;
        end
      else
        Error(Format(SEUnexpectJsonTokenFmt,[TokenKindName(tkString), TokenKindName(Reader.TokenKind)]));
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
    if Reader.TokenKind <> tkArrayBegin then
      if Reader.TokenKind = tkNull then
        begin
          CheckNull;
          System.FillChar(aData^, GetTypeData(aTypeInfo)^.SetSize, 0);
          exit;
        end
      else
        Error(Format(SEUnexpectJsonTokenFmt,[TokenKindName(tkArrayBegin), TokenKindName(Reader.TokenKind)]));
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
      if Reader.TokenKind = tkArrayEnd then break;
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
      Error(Format(SEUnsupportPdoKindFmt, [GetEnumName(TypeInfo(TTypeKind), Integer(aTypeInfo^.Kind))]));
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
  GlobLock.BeginRead;
  try
    try
      ReadValue(aTypeInfo, @aValue);
    except
      on e: EPdoLoadJson do raise;
      on e: Exception do
        raise EPdoLoadJson.Create(Format(SEExceptWhileJsonLoadFmt, [e.ClassName, e.Message]), Reader.Path);
    end;
  finally
    GlobLock.EndRead;
  end;
end;
{$POP}

class procedure TOStrHelper.WriteJson(p: Pointer; aWriter: TJsonStrWriter);
var
  pStr: ^TOptString absolute p;
begin
  if pStr^.Assigned then
    aWriter.Add(pStr^.Value)
  else
    aWriter.AddNull;
end;

class function TOStrHelper.ReadJson(p: Pointer; aReader: TJsonReader; const aOpts: TJsonReadOptions): Boolean;
var
  pStr: ^TOptString absolute p;
begin
  if aReader.TokenKind = tkNull then exit(not(jroRejectNulls in aOpts));
  if aReader.TokenKind <> tkString then exit(False);
  pStr^ := aReader.AsString;
  Result := True;
end;

{ TOBoolHelper }

class procedure TOBoolHelper.WriteJson(p: Pointer; aWriter: TJsonStrWriter);
var
  pBool: ^TOptBoolean absolute p;
begin
  if pBool^.Assigned then
    if pBool^.Value then
      aWriter.AddTrue
    else
      aWriter.AddFalse
  else
    aWriter.AddNull;
end;

class function TOBoolHelper.ReadJson(p: Pointer; aReader: TJsonReader; const aOpts: TJsonReadOptions): Boolean;
var
  pBool: ^TOptBoolean absolute p;
begin
  if aReader.TokenKind = tkNull then exit(not(jroRejectNulls in aOpts));
  if not(aReader.TokenKind in [tkFalse, tkTrue]) then exit(False);
  pBool^ := aReader.AsBoolean;
  Result := True;
end;

{ TOShortIntHelper }

class procedure TOShortIntHelper.WriteJson(p: Pointer; aWriter: TJsonStrWriter);
var
  pI: ^TOptShortInt absolute p;
begin
  if pI^.Assigned then
    aWriter.Add(pI^.Value)
  else
    aWriter.AddNull;
end;

class function TOShortIntHelper.ReadJson(p: Pointer; aReader: TJsonReader; const aOpts: TJsonReadOptions): Boolean;
var
  pI: ^TOptShortInt absolute p;
  I: Int64;
begin
  if aReader.TokenKind = tkNull then exit(not(jroRejectNulls in aOpts));
  if aReader.TokenKind <> tkNumber then exit(False);
  if not aReader.AsNumber.IsExactInt(I) then exit(False);
  if (I < Low(ShortInt)) or (I > High(ShortInt)) then exit(False);
  pI^ := ShortInt(I);
  Result := True;
end;

{ TOptByteHelper }

class procedure TOptByteHelper.WriteJson(p: Pointer; aWriter: TJsonStrWriter);
var
  pB: ^TOptByte absolute p;
begin
  if pB^.Assigned then
    aWriter.Add(pB^.Value)
  else
    aWriter.AddNull;
end;

class function TOptByteHelper.ReadJson(p: Pointer; aReader: TJsonReader; const aOpts: TJsonReadOptions): Boolean;
var
  pB: ^TOptByte absolute p;
  I: Int64;
begin
  if aReader.TokenKind = tkNull then exit(not(jroRejectNulls in aOpts));
  if aReader.TokenKind <> tkNumber then exit(False);
  if not aReader.AsNumber.IsExactInt(I) then exit(False);
  if (I < 0) or (I > High(Byte)) then exit(False);
  pB^ := Byte(I);
  Result := True;
end;

{ TOSmallIntHelper }

class procedure TOSmallIntHelper.WriteJson(p: Pointer; aWriter: TJsonStrWriter);
var
  pI: ^TOptSmallInt absolute p;
begin
  if pI^.Assigned then
    aWriter.Add(pI^.Value)
  else
    aWriter.AddNull;
end;

class function TOSmallIntHelper.ReadJson(p: Pointer; aReader: TJsonReader; const aOpts: TJsonReadOptions): Boolean;
var
  pI: ^TOptSmallInt absolute p;
  I: Int64;
begin
  if aReader.TokenKind = tkNull then exit(not(jroRejectNulls in aOpts));
  if aReader.TokenKind <> tkNumber then exit(False);
  if not aReader.AsNumber.IsExactInt(I) then exit(False);
  if (I < Low(SmallInt)) or (I > High(SmallInt)) then exit(False);
  pI^ := SmallInt(I);
  Result := True;
end;

{ TOptWordHelper }

class procedure TOptWordHelper.WriteJson(p: Pointer; aWriter: TJsonStrWriter);
var
  pW: ^TOptWord absolute p;
begin
  if pW^.Assigned then
    aWriter.Add(pW^.Value)
  else
    aWriter.AddNull;
end;

class function TOptWordHelper.ReadJson(p: Pointer; aReader: TJsonReader; const aOpts: TJsonReadOptions): Boolean;
var
  pW: ^TOptWord absolute p;
  I: Int64;
begin
  if aReader.TokenKind = tkNull then exit(not(jroRejectNulls in aOpts));
  if aReader.TokenKind <> tkNumber then exit(False);
  if not aReader.AsNumber.IsExactInt(I) then exit(False);
  if (I < 0) or (I > High(Word)) then exit(False);
  pW^ := Word(I);
  Result := True;
end;

{ TOIntHelper }

class procedure TOIntHelper.WriteJson(p: Pointer; aWriter: TJsonStrWriter);
var
  pI: ^TOptInteger absolute p;
begin
  if pI^.Assigned then
    aWriter.Add(pI^.Value)
  else
    aWriter.AddNull;
end;

class function TOIntHelper.ReadJson(p: Pointer; aReader: TJsonReader; const aOpts: TJsonReadOptions): Boolean;
var
  pI: ^TOptInteger absolute p;
  I: Int64;
begin
  if aReader.TokenKind = tkNull then exit(not(jroRejectNulls in aOpts));
  if aReader.TokenKind <> tkNumber then exit(False);
  if not aReader.AsNumber.IsExactInt(I) then exit(False);
  if (I < Low(Integer)) or (I > High(Integer)) then exit(False);
  pI^ := Integer(I);
  Result := True;
end;

{ TOCardinalHelper }

class procedure TOCardinalHelper.WriteJson(p: Pointer; aWriter: TJsonStrWriter);
var
  pC: ^TOptCardinal absolute p;
begin
  if pC^.Assigned then
    aWriter.Add(pC^.Value)
  else
    aWriter.AddNull;
end;

class function TOCardinalHelper.ReadJson(p: Pointer; aReader: TJsonReader; const aOpts: TJsonReadOptions): Boolean;
var
  pC: ^TOptCardinal absolute p;
  I: Int64;
begin
  if aReader.TokenKind = tkNull then exit(not(jroRejectNulls in aOpts));
  if aReader.TokenKind <> tkNumber then exit(False);
  if not aReader.AsNumber.IsExactInt(I) then exit(False);
  if (I < 0) or (I > High(Cardinal)) then exit(False);
  pC^ := Cardinal(I);
  Result := True;
end;

{ TODoubleHelper }

class procedure TODoubleHelper.WriteJson(p: Pointer; aWriter: TJsonStrWriter);
var
  pD: ^TOptDouble absolute p;
begin
  if pD^.Assigned then
    aWriter.Add(pD^.Value)
  else
    aWriter.AddNull;
end;

class function TODoubleHelper.ReadJson(p: Pointer; aReader: TJsonReader; const aOpts: TJsonReadOptions): Boolean;
var
  pD: ^TOptDouble absolute p;
begin
  if aReader.TokenKind = tkNull then exit(not(jroRejectNulls in aOpts));
  if aReader.TokenKind <> tkNumber then exit(False);
  pD^ := aReader.AsNumber;
  Result := True;
end;

initialization
{$WARN 5058 OFF : Variable "$1" does not seem to be initialized}
  GlobLock := TMultiReadExclusiveWriteSynchronizer.Create;
finalization
  GlobLock := nil;
end.

