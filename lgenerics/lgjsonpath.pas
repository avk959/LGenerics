{****************************************************************************
*                                                                           *
*   This file is part of the LGenerics package.                             *
*   JSONPath: query expressions for JSON.                                   *
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
unit lgJsonPath;

{$MODE OBJFPC}{$H+}
{$MODESWITCH ADVANCEDRECORDS}
{$MODESWITCH TYPEHELPERS}

interface

uses
  SysUtils, lgUtils, lgJson;

{

  Basic elements:

  $               - root node identifier
  @               - current node identifier (valid only within filter selectors)
  [<selectors>]   - child segment: selects zero or more children of a node;
                    contains one or more selectors, separated by commas
  .name           - shorthand for ['name'] (or ["name"])
  .*              - shorthand for [*]
  ..[<selectors>] - descendant segment: selects zero or more descendants of a node;
                    contains one or more selectors, separated by commas
  ..name          - shorthand for ..['name']
  ..*             - shorthand for ..[*]
  'name'          - name selector: selects a named child of an object
  *               - wildcard selector: selects all children of a node
  42              - index selector: selects an indexed child of an array (from 0)
  0:42:5          - array slice selector: start:end:step for arrays
  ?<logical-expr> - filter selector: selects particular children using a logical expression
  length(@.foo)   - function extension: invokes a function in a filter expression

  Operators supported in filter expressions in descending order of precedence:

  5   - grouping   ( ... )
  4   - logical NOT   !
  3   - relations   ==, !=, <, >, <=, >=
  2   - logical AND  &&
  1   - logical OR   ||

  The implementation tries to be compliant with
    https://www.ietf.org/archive/id/draft-ietf-jsonpath-base-13.html
}
type
  { TJpNode represents JSONPath Node - a value along with its location within the root }
  TJpNode = record
  const
  {$PUSH}{$J-}
    NODE_PATH: string  = 'location';
    NODE_VALUE: string = 'value';
  {$POP}
  var
    Path: string; // Normalized Path, uses the bracket notation and single quotes
    Value: TJsonNode;
    constructor Make(const aPath: string; aValue: TJsonNode);
    function AsJson: string;
  end;

  {TJpNodeList represent a JSONPath Nodelist }
  TJpNodeList = array of TJpNode;

  TJpNodeListHelper = type helper for TJpNodeList
    function AsJson: string;
  end;

  TJpValueList = array of TJsonNode;

  TJpValueListHelper = type helper for TJpValueList
    function AsJson: string;
  end;

{$PUSH}{$INTERFACES COM}
  IJsonPath = interface
  ['{3D2794A4-F950-40DA-A611-51F7B37A6C11}']
  { returns NodeList that matches its internal, previously parsed JSONPath query }
    function  Match(aRoot: TJsonNode): TJpNodeList;
  { returns True and NodeList as JSON if aRoot is valid JSON, False otherwise }
    function  Match(const aRoot: string; out aNodeList: string): boolean;
  { returns an array of TJsonNode that matches its internal, previously parsed JSONPath query }
    function  MatchValues(aRoot: TJsonNode): TJpValueList;
  { returns True and an array of JSON values if aRoot is valid JSON, False otherwise }
    function  MatchValues(const aRoot: string; out aNodeArray: string): Boolean;
  { returns the first element along with its location that matches its internal,
    previously parsed JSONPath query, if any, otherwise returns ('', nil) }
    function  MatchFirst(aRoot: TJsonNode): TJpNode;
  { returns True and the first matching element along with its location
    (empty object if no matches) as JSON if aRoot is valid JSON, False otherwise. }
    function  MatchFirst(const aRoot: string; out aNode: string): Boolean;
  { returns the first element that matches its internal, previously parsed JSONPath query if any;
    otherwise returns NIL }
    function  MatchFirstValue(aRoot: TJsonNode): TJsonNode;
  { returns True and first matching element as JSON (an empty string if no matches)
    if aRoot is valid JSON, False otherwise }
    function  MatchFirstValue(const aRoot: string; out aValue: string): Boolean;
  end;
{$POP}

{ Parses a JSONPath expression aQuery which must be a well formed UTF-8 string starting with $;
  returns nil if aQuery is invalid, in this case, the aMsg parameter contains an error message }
  function JsonPathParse(const aQuery: string; out aMsg: string): IJsonPath;
{ same as above, but for the case where the content of the error message is of no interest }
  function ParseJsonPath(const aQuery: string; out aPath: IJsonPath): Boolean;
{ applies a aQuery to the JSON value aRoot; returns False if aQuery is invalid, in this case,
  the aMsg parameter contains an error message }
  function JsonPathMatch(const aQuery: string; aRoot: TJsonNode; out aList: TJpValueList;
                        out aMsg: string): Boolean;
{ same as above, but for the case where the content of the error message is of no interest }
  function JsonPathMatch(const aQuery: string; aRoot: TJsonNode; out aList: TJpValueList): Boolean;
{ applies a query aQuery to the JSON value aRoot and returns only the first match;
  returns False if aQuery is invalid, in this case, the aMsg parameter contains an error message }
  function JsonPathMatchFirst(const aQuery: string; aRoot: TJsonNode; out aNode: TJsonNode; out aMsg: string): Boolean;
{ same as above, but for the case where the content of the error message is of no interest }
  function JsonPathMatchFirst(const aQuery: string; aRoot: TJsonNode; out aNode: TJsonNode): Boolean;

/////////////////////////////////////////////////////

type
  TJpValueType = (jvtNull, jvtFalse, jvtTrue, jvtString, jvtNumber, jvtNode);

  { TJpValue }
  TJpValue = record
    procedure SetNull; inline;
    function AsBoolean: Boolean; inline;
    class function ValNull: TJpValue; static; inline;
    class function ValFalse: TJpValue; static; inline;
    class function ValTrue: TJpValue; static; inline;
    class operator := (b: Boolean): TJpValue; inline;
    class operator := (const s: string): TJpValue; inline;
    class operator := (d: Double): TJpValue; inline;
    class operator := (aNode: TJsonNode): TJpValue; inline;
    class operator  = (const L, R: TJpValue): Boolean;
    class operator  < (const L, R: TJpValue): Boolean;
    class operator <= (const L, R: TJpValue): Boolean; inline;
    class operator  > (const L, R: TJpValue): Boolean; inline;
    class operator >= (const L, R: TJpValue): Boolean; inline;
  public
    ValType: TJpValueType;
    StrValue: string;
    case Byte of
      0: (NumValue: Double);
      1: (NodeValue: TJsonNode);
  end;

  TJpInstanceType = (jitNothing, jitValue, jitLogical, jitNodeList);

  { TJpInstance }
  TJpInstance = record
    function AsBoolean: Boolean; inline;
    class function Nothing: TJpInstance; static; inline;
    class operator := (const s: string): TJpInstance; inline;
    class operator := (d: Double): TJpInstance; inline;
    class operator := (const v: TJpValue): TJpInstance; inline;
    class operator := (const aList: TJpValueList): TJpInstance; inline;
    class operator := (aNode: TJsonNode): TJpInstance; inline;
    class operator := (b: Boolean): TJpInstance; inline;
    class operator  = (const L, R: TJpInstance): Boolean;
    class operator  < (const L, R: TJpInstance): Boolean; inline;
    class operator <= (const L, R: TJpInstance): Boolean; inline;
    class operator  > (const L, R: TJpInstance): Boolean; inline;
    class operator >= (const L, R: TJpInstance): Boolean; inline;
  public
    InstType: TJpInstanceType;
    Logical: Boolean;
    Value: TJpValue;
    NodeList: TJpValueList;
  end;

  TJpFunParamDefs = array of TJpInstanceType;
  TJpParamList    = array of TJpInstance;
  TJpFunctionCall = procedure(const aParams: TJpParamList; out aResult: TJpInstance);

  { TJpFunctionDef }
  TJpFunctionDef = record
    ParamDefs: TJpFunParamDefs;
    ResultType: TJpInstanceType;
    OnExecute: TJpFunctionCall;
    constructor Make(const aDefs: TJpFunParamDefs; aResType: TJpInstanceType; aOnExec: TJpFunctionCall);
  end;

  function JpRegisterFunction(const aName: string; const aFunDef: TJpFunctionDef): Boolean;

implementation
{$B-}{$COPERATORS ON}{$POINTERMATH ON}

uses
  Math, RegExpr, lgVector, lgHashMap, lgSeqUtils, lgStrConst;

type
  { TStrBuilder }
  TStrBuilder = record
  strict private
    FBuffer: specialize TGDynArray<AnsiChar>;
    FCount: SizeInt;
  public
    constructor Create(aCapacity: SizeInt);
    procedure EnsureCapacity(aCapacity: SizeInt); inline;
    procedure Append(c: AnsiChar); inline;
    procedure Append(const s: string); inline;
    function  ToString: string; inline;
    property  Count: SizeInt read FCount;
  end;

{ TStrBuilder }

constructor TStrBuilder.Create(aCapacity: SizeInt);
begin
  if aCapacity > 0 then
    FBuffer.Length := lgUtils.RoundUpTwoPower(aCapacity)
  else
    FBuffer.Length := DEFAULT_CONTAINER_CAPACITY;
  FCount := 0;
end;

procedure TStrBuilder.EnsureCapacity(aCapacity: SizeInt);
begin
  if aCapacity > FBuffer.Length then
    FBuffer.Length := lgUtils.RoundUpTwoPower(aCapacity);
end;

procedure TStrBuilder.Append(c: AnsiChar);
begin
  EnsureCapacity(Count + 1);
  FBuffer[Count] := c;
  Inc(FCount);
end;

procedure TStrBuilder.Append(const s: string);
begin
  if s = '' then exit;
  EnsureCapacity(Count + System.Length(s));
  System.Move(Pointer(s)^, FBuffer.Ptr[Count], System.Length(s));
  FCount += System.Length(s);
end;

{$PUSH}{$WARN 5094 OFF}
function TStrBuilder.ToString: string;
begin
  System.SetLength(Result, Count);
  System.Move(FBuffer.Ptr^, Pointer(Result)^, Count);
  FCount := 0;
end;
{$POP}

{ TJpNode }

constructor TJpNode.Make(const aPath: string; aValue: TJsonNode);
begin
  Path := aPath;
  Value := aValue;
end;

function TJpNode.AsJson: string;
const
{$PUSH}{$J-}
  Fmt: string = '{"%s":"%s","%s":%s}';
{$POP}
begin
  if Self.Value = nil then
    Result := '{}'
  else
    Result := Format(Fmt, [NODE_PATH, Path, NODE_VALUE, Value.AsJson]);
end;

{ TJpNodeListHelper }
{$PUSH}{$WARN 6058 OFF}
function TJpNodeListHelper.AsJson: string;
var
  sb: TStrBuilder;
  I: SizeInt;
const
  INIT_CAP = 1024;
begin
  if Self = nil then exit('[]');
  sb := TStrBuilder.Create(INIT_CAP);
  sb.Append('[');
  for I := 0 to Pred(System.High(Self)) do
    begin
      sb.Append('{');
      sb.Append('"');
      sb.Append(Self[I].NODE_PATH);
      sb.Append('"');
      sb.Append(':');
      sb.Append('"');
      sb.Append(Self[I].Path);
      sb.Append('"');
      sb.Append(',');
      sb.Append('"');
      sb.Append(Self[I].NODE_VALUE);
      sb.Append('"');
      sb.Append(':');
      sb.Append(Self[I].Value.AsJson);
      sb.Append('}');
      sb.Append(',');
    end;
  I := System.High(Self);
  sb.Append('{');
  sb.Append('"');
  sb.Append(Self[I].NODE_PATH);
  sb.Append('"');
  sb.Append(':');
  sb.Append('"');
  sb.Append(Self[I].Path);
  sb.Append('"');
  sb.Append(',');
  sb.Append('"');
  sb.Append(Self[I].NODE_VALUE);
  sb.Append('"');
  sb.Append(':');
  sb.Append(Self[I].Value.AsJson);
  sb.Append('}');
  sb.Append(']');
  Result := sb.ToString;
end;

{ TJpValueListHelper }

function TJpValueListHelper.AsJson: string;
var
  sb: TStrBuilder;
  I: SizeInt;
const
  INIT_CAP = 1024;
begin
  if Self = nil then exit('[]');
  sb := TStrBuilder.Create(INIT_CAP);
  sb.Append('[');
  for I := 0 to Pred(System.High(Self)) do
    begin
      sb.Append(Self[I].AsJson);
      sb.Append(',');
    end;
  sb.Append(Self[System.High(Self)].AsJson);
  sb.Append(']');
  Result := sb.ToString;
end;
{$POP}

{ TJpValue }

procedure TJpValue.SetNull;
begin
  ValType := jvtNull;
end;

function TJpValue.AsBoolean: Boolean;
begin
  case ValType of
    jvtTrue:  Result := True;
    jvtNode: Result := NodeValue.Kind = jvkTrue;
  else
    Result := False;
  end;
end;

class function TJpValue.ValNull: TJpValue;
begin
  Result.ValType := jvtNull;
end;

class function TJpValue.ValFalse: TJpValue;
begin
  Result.ValType := jvtFalse;
end;

class function TJpValue.ValTrue: TJpValue;
begin
  Result.ValType := jvtTrue;
end;

class operator TJpValue.=(const L, R: TJpValue): Boolean;
begin
  Result := False;
  if L.ValType = R.ValType then
    case L.ValType of
      jvtNull, jvtFalse, jvtTrue: Result := True;
      jvtString: Result := L.StrValue = R.StrValue;
      jvtNumber: Result := L.NumValue = R.NumValue;
      jvtNode:   Result := L.NodeValue.EqualTo(R.NodeValue);
    end
  else
    if (L.ValType = jvtNode) and (L.NodeValue <> nil) then
      case L.NodeValue.Kind of
        jvkNull:  Result := R.ValType = jvtNull;
        jvkFalse: Result := R.ValType = jvtFalse;
        jvkTrue:  Result := R.ValType = jvtTrue;
        jvkString:
            Result := (R.ValType = jvtString) and (L.NodeValue.AsString = R.StrValue);
        jvkNumber:
            Result := (R.ValType = jvtNumber) and (L.NodeValue.AsNumber = R.NumValue);
      else
      end
    else
      if (R.ValType = jvtNode) and (R.NodeValue <> nil) then
        case R.NodeValue.Kind of
          jvkNull:  Result := L.ValType = jvtNull;
          jvkFalse: Result := L.ValType = jvtFalse;
          jvkTrue:  Result := L.ValType = jvtTrue;
          jvkString:
              Result := (L.ValType = jvtString) and (R.NodeValue.AsString = L.StrValue);
          jvkNumber:
              Result := (L.ValType = jvtNumber) and (R.NodeValue.AsNumber = L.NumValue);
        else
        end
end;

class operator TJpValue.<(const L, R: TJpValue): Boolean;
begin
  Result := False;
  if L.ValType = R.ValType then
    case L.ValType of
      jvtString:
        Result := AnsiCompareStr(L.StrValue, R.StrValue) < 0;
      jvtNumber:
        Result := L.NumValue < R.NumValue;
      jvtNode:
        if (L.NodeValue <> nil) and (R.NodeValue <> nil) then
          case L.NodeValue.Kind of
            jvkString:
              if R.NodeValue.Kind = jvkString then
                Result := AnsiCompareStr(L.NodeValue.AsString, R.NodeValue.AsString) < 0;
            jvkNumber:
              if R.NodeValue.Kind = jvkNumber then
                Result := L.NodeValue.AsNumber < R.NodeValue.AsNumber;
          else
          end;
    else
    end
  else
    if (L.ValType = jvtNode) and (L.NodeValue <> nil) then
      case L.NodeValue.Kind of
        jvkString:
          if R.ValType = jvtString then
            Result := AnsiCompareStr(L.NodeValue.AsString, R.StrValue) < 0;
        jvkNumber:
          if R.ValType = jvtNumber then
            Result := L.NodeValue.AsNumber < R.NumValue;
      else
      end
    else
      if (R.ValType = jvtNode) and (R.NodeValue <> nil) then
        case R.NodeValue.Kind of
          jvkString:
            if L.ValType = jvtString then
              Result := AnsiCompareStr(L.StrValue, R.NodeValue.AsString) < 0;
          jvkNumber:
            if L.ValType = jvtNumber then
              Result := L.NumValue < R.NodeValue.AsNumber;
        else
        end;
end;

class operator TJpValue.<=(const L, R: TJpValue): Boolean;
begin
  Result := (L < R) or (L = R);
end;

class operator TJpValue.>(const L, R: TJpValue): Boolean;
begin
  Result := R < L;
end;

class operator TJpValue.>=(const L, R: TJpValue): Boolean;
begin
  Result := (R < L) or (R = L);
end;

class operator TJpValue.:=(b: Boolean): TJpValue;
begin
  if b then
    Result.ValType := jvtTrue
  else
    Result.ValType := jvtFalse;
end;

class operator TJpValue.:=(const s: string): TJpValue;
begin
  Result.ValType := jvtString;
  Result.StrValue := s;
end;

class operator TJpValue.:=(d: Double): TJpValue;
begin
  Result.ValType := jvtNumber;
  Result.NumValue := d;
end;

class operator TJpValue.:=(aNode: TJsonNode): TJpValue;
begin
  Result.ValType := jvtNode;
  Result.NodeValue := aNode;
end;

{ TJpInstance }

function TJpInstance.AsBoolean: Boolean;
begin
  case InstType of
    jitValue: Result := Value.AsBoolean;
    jitLogical:  Result := Logical;
  else
    Result := False;
  end;
end;

class function TJpInstance.Nothing: TJpInstance;
begin
  Result.InstType := jitNothing;
end;

class operator TJpInstance.=(const L, R: TJpInstance): Boolean;
begin
  Result := False;
  if L.InstType = R.InstType then
    case L.InstType of
      jitNothing:  Result := True;
      jitValue: Result := L.Value = R.Value;
      jitLogical: Result := L.Logical = R.Logical; ///////////
      jitNodeList:
        if L.NodeList = nil then
          Result := R.NodeList = nil;
    end
  else
    if (L.InstType = jitLogical) and (R.InstType = jitValue) then
      Result := L.Logical = R.Value.AsBoolean
    else
      if (R.InstType = jitLogical) and (L.InstType = jitValue) then
        Result := R.Logical = L.Value.AsBoolean;
end;

class operator TJpInstance.<(const L, R: TJpInstance): Boolean;
begin
  if (L.InstType = jitValue) and (R.InstType = jitValue) then
    Result := L.Value < R.Value
  else
    Result := False;
end;

class operator TJpInstance.<=(const L, R: TJpInstance): Boolean;
begin
  Result := (L < R) or (L = R);
end;

class operator TJpInstance.>(const L, R: TJpInstance): Boolean;
begin
  Result := R < L;
end;

class operator TJpInstance.>=(const L, R: TJpInstance): Boolean;
begin
  Result := (R < L) or (R = L);
end;

class operator TJpInstance.:=(const s: string): TJpInstance;
begin
  Result.InstType := jitValue;
  Result.Value := s;
end;

class operator TJpInstance.:=(d: Double): TJpInstance;
begin
  Result.InstType := jitValue;
  Result.Value := d;
end;

class operator TJpInstance.:=(const v: TJpValue): TJpInstance;
begin
  Result.InstType := jitValue;
  Result.Value := v;
end;

class operator TJpInstance.:=(const aList: TJpValueList): TJpInstance;
begin
  Result.InstType := jitNodeList;
  Result.NodeList := aList;
end;

class operator TJpInstance.:=(aNode: TJsonNode): TJpInstance;
begin
  Result.Value := aNode;
  Result.InstType := jitValue;
end;

class operator TJpInstance.:=(b: Boolean): TJpInstance;
begin
  Result.InstType := jitLogical;
  Result.Logical := b;
end;

{ TJpFunctionDef }

constructor TJpFunctionDef.Make(const aDefs: TJpFunParamDefs; aResType: TJpInstanceType; aOnExec: TJpFunctionCall);
begin
  ParamDefs := aDefs;
  ResultType := aResType;
  OnExecute := aOnExec;
end;

type

  TSelectorKind = (
    skNone,
    skIndex,    // index selector    - ([42])
    skName,     // name selector     - (.store or ['store'] or ["store"])
    skWildcard, // wildcard selector - ( * )
    skSlice,    // slice selector    - Python like slice ([start : end : step])
    skFilter    // filter selector   - ([?()])
    );

  TOptionalInt = specialize TGOptional<SizeInt>;

  TSlice = record
    Start,
    Stop,
    Step: TOptionalInt;
    procedure Clear;
  end;
  PSlice = ^TSlice;

  TSegment = class;
  TJpFilter  = class;

  { TSelector }
  TSelector = class
  strict protected
    FKind: TSelectorKind;
    function GetIndex: SizeInt; virtual;
    function GetName: string; virtual;
    function GetSlice: PSlice; virtual;
    function GetFilter: TJpFilter; virtual;
  public
    NextSegment: TSegment;
    property Kind: TSelectorKind read FKind;
    property Index: SizeInt read GetIndex;
    property Name: string read GetName;
    property Slice: PSlice read GetSlice;
    property Filter: TJpFilter read GetFilter;
  end;

  { TWildcardSelector }

  TWildcardSelector = class(TSelector)
    constructor Create;
  end;

  { TIndexSelector }
  TIndexSelector = class(TSelector)
  strict private
    FIndex: SizeInt;
  strict protected
    function GetIndex: SizeInt; override;
  public
    constructor Create(aIndex: SizeInt);
  end;

  { TNameSelector }
  TNameSelector = class(TSelector)
  strict private
    FName: string;
  strict protected
    function GetName: string; override;
  public
    constructor Create(const aName: string);
  end;

  { TSliceSelector }
  TSliceSelector = class(TSelector)
  strict private
    FSlice: TSlice;
  strict protected
    function GetSlice: PSlice; override;
  public
    constructor Create(const aSlice: TSlice);
  end;

  { TFilterSelector }
  TFilterSelector = class(TSelector)
  strict private
    FFilter: TJpFilter;
  strict protected
    function GetFilter: TJpFilter; override;
  public
    constructor Create(aFilter: TJpFilter);
    destructor Destroy; override;
  end;

  TSegmentKind  = (sgkNone, sgkChild, sgkDescendant);
  TSelectorList = specialize TGLiteVector<TSelector>;
  TSelectorEnum = TSelectorList.TEnumerator;

  { TSegment }
  TSegment = class
  strict private
    FNext: TSegment;
    procedure SetNext(aValue: TSegment);
  strict protected
    FSelectors: TSelectorList;
    FKind: TSegmentKind;
    function GetCount: SizeInt; inline;
  public
    destructor Destroy; override;
    function  GetEnumerator: TSelectorEnum; inline;
    function  GetFirstSelector: TSelector; inline;
    procedure AddSelector(aIndex: SizeInt); virtual;
    procedure AddSelector(const aName: string); virtual;
    procedure AddWildcard; virtual;
    procedure AddSelector(const s: TSlice); virtual;
    procedure AddSelector(aFilter: TJpFilter); virtual;
    property  Kind: TSegmentKind read FKind;
    property  Next: TSegment read FNext write SetNext;
    property  Count: SizeInt read GetCount;
  end;

  { TDescSegment }
  TDescSegment = class(TSegment)
    constructor Create;
  end;

  { TChildSegment }
  TChildSegment = class(TSegment)
  strict protected
  public
    constructor Create;
    procedure AddSelector(aIndex: SizeInt); override;
    procedure AddSelector(const aName: string); override;
    procedure AddWildcard; override;
    procedure AddSelector(const aSlice: TSlice); override;
    procedure AddSelector(aFilter: TJpFilter); override;
  end;

  { TJsonPathQuery }
  TJsonPathQuery = class
  strict protected
  type
    TApplyKind = (akCount, akFirstMatch, akMatchList);
  var
    FSegments: specialize TGLiteVector<TSegment>;
    FMatchCollector: specialize TGLiteVector<TJsonNode>;
    FRoot,
    FFirst: TJsonNode;
    FCount: SizeInt;
    FApply: TApplyKind;
    FDone: Boolean;
    procedure AddMatch(aNode: TJsonNode); inline;
    procedure ApplySlice(aSel: TSelector; aNode: TJsonNode);
    procedure ApplySelector(aSel: TSelector; aRoot: TJsonNode; aDesc: Boolean = False);
    procedure ApplySegment(aSegment: TSegment; aRoot: TJsonNode);
    property  Done: Boolean read FDone;
  public
    destructor Destroy; override;
    procedure Clear;
    function AddChildSegment: TSegment;
    function AddDescendant: TSegment;
    function IsSingular: Boolean;
    function GetCount(aRoot: TJsonNode): SizeInt;
    function Apply(aRoot: TJsonNode): TJpValueList;
    function GetFirst(aRoot: TJsonNode): TJsonNode;
    function TestExists(aRoot: TJsonNode): Boolean; inline;
  end;

  TChar2 = array[0..1] of AnsiChar;
  PChar2 = ^TChar2;
  TChar4 = array[0..3] of AnsiChar;
  PChar4 = ^TChar4;

  { TJsonSpecPathQuery }
  TJsonSpecPathQuery = class(TJsonPathQuery) //todo: quote type choice ???
  strict private
  const
  {$PUSH}{$J-}
    SEG_OPEN: TChar2  = '[''';
    SEG_CLOSE: TChar2 = ''']';
    HEX_CHARS_TBL: array[0..15] of AnsiChar = '0123456789abcdef';
  {$POP}
  strict protected
    FPathHolder: specialize TGLiteVector<string>;
    FMatchPathCollector: specialize TGLiteVector<TJpNode>;
    function  EncodeSegment(const s: string): string;
    procedure PathPush(const aSegment: string); inline;
    procedure PathPush(aSegment: SizeInt); inline;
    procedure PathPop; inline;
    procedure AddMatchWithPath(aNode: TJsonNode); inline;
    procedure ApplySliceWithPath(aSel: TSelector; aNode: TJsonNode);
    procedure ApplySelectorWithPath(aSel: TSelector; aRoot: TJsonNode; aDesc: Boolean = False);
    procedure ApplySegmentWithPath(aSegment: TSegment; aRoot: TJsonNode);
  public
    function  ApplyWithPath(aRoot: TJsonNode): TJpNodeList;
    function  GetFirstWithPath(aRoot: TJsonNode): TJpNode;
  end;

  TJpIdentifier = (jpiRoot, jpiCurrent);

  TJpFilterContext = record
    Root,
    Current: TJsonNode;
  end;

  { TJpExpression }
  TJpExpression = class
  strict protected
    FValue: TJpInstance;
    procedure Eval(const aCtx: TJpFilterContext; var v: TJpInstance); virtual;
  public
    function  GetValue(const aCtx: TJpFilterContext): TJpInstance; virtual;
    function  Apply(const aCtx: TJpFilterContext): Boolean; virtual;
  end;

  { TJpConstExpr }
  TJpConstExpr = class(TJpExpression)
  strict protected
  public
    constructor CreateNull;
    constructor Create(b: Boolean);
    constructor Create(const s: string);
    constructor Create(d: Double);
  end;

  { TRelPathQuery }
  TRelPathQuery = class(TJsonPathQuery)
  strict private
    FIdent: TJpIdentifier;
  public
    constructor Create(aId: TJpIdentifier);
    function GetNodeCount(const aCtx: TJpFilterContext): SizeInt; inline;
    function GetFirstNode(const aCtx: TJpFilterContext): TJsonNode; inline;
    function AsNodeList(const aCtx: TJpFilterContext): TJpValueList; inline;
    function AsBoolean(const aCtx: TJpFilterContext): Boolean; inline;
  end;

  { TJpNotPredicate }
  TJpNotPredicate = class(TJpExpression)
  strict private
    FExpr: TJpExpression;
  strict protected
    procedure Eval(const aCtx: TJpFilterContext; var v: TJpInstance); override;
  public
    constructor Create(aExpr: TJpExpression);
    destructor Destroy; override;
  end;

  { TJpBinaryExpr }
  TJpBinaryExpr = class(TJpExpression)
  strict protected
    FLeft,
    FRight: TJpExpression;
  public
    constructor Create(aLeft, aRight: TJpExpression);
    destructor Destroy; override;
    property Left: TJpExpression read FLeft;
    property Right: TJpExpression read FRight;
  end;

  { TJpAndPredicate }
  TJpAndPredicate = class(TJpBinaryExpr)
  strict protected
    procedure Eval(const aCtx: TJpFilterContext; var v: TJpInstance); override;
  end;

  { TJpOrPredicate }
  TJpOrPredicate = class(TJpBinaryExpr)
  strict protected
    procedure Eval(const aCtx: TJpFilterContext; var v: TJpInstance); override;
  end;

  { TJpRelQueryExpr }
  TJpRelQueryExpr = class(TJpExpression)
  strict private
    FPath: TRelPathQuery;
    FSingular,
    FTesting: Boolean;
  strict protected
    procedure Eval(const aCtx: TJpFilterContext; var v: TJpInstance); override;
  public
    constructor Create(aPath: TRelPathQuery);
    destructor Destroy; override;
    function Apply(const aCtx: TJpFilterContext): Boolean; override;
  end;

  { TJpEqualExpr }
  TJpEqualExpr = class(TJpBinaryExpr)
  strict protected
    procedure Eval(const aCtx: TJpFilterContext; var v: TJpInstance); override;
  end;

  { TJpNotEqualExpr }
  TJpNotEqualExpr = class(TJpBinaryExpr)
  strict protected
    procedure Eval(const aCtx: TJpFilterContext; var v: TJpInstance); override;
  end;

  { TJpLessThanExpr }
  TJpLessThanExpr = class(TJpBinaryExpr)
  strict protected
    procedure Eval(const aCtx: TJpFilterContext; var v: TJpInstance); override;
  end;

  { TJpLessOrEqualExpr }
  TJpLessOrEqualExpr = class(TJpBinaryExpr)
  strict protected
    procedure Eval(const aCtx: TJpFilterContext; var v: TJpInstance); override;
  end;

  { TJpGreaterThanExpr }
  TJpGreaterThanExpr = class(TJpBinaryExpr)
  strict protected
    procedure Eval(const aCtx: TJpFilterContext; var v: TJpInstance); override;
  end;

  { TJpGreaterOrEqualExpr }
  TJpGreaterOrEqualExpr = class(TJpBinaryExpr)
  strict protected
    procedure Eval(const aCtx: TJpFilterContext; var v: TJpInstance); override;
  end;

  TJpExprList = array of TJpExpression;

  { TJpFunctionExpr }
  TJpFunctionExpr = class(TJpExpression)
  strict protected
    FParamList: TJpExprList;
    FFunCall: TJpFunctionCall;
    procedure Eval(const aCtx: TJpFilterContext; var v: TJpInstance); override;
  public
    constructor Create(const aParams: TJpExprList; aFun: TJpFunctionCall);
    destructor Destroy; override;
  end;

  { TJpFilter }
  TJpFilter = class
  strict private
    FContext: TJpFilterContext;
    FExpr: TJpExpression;
    procedure SetRoot(aValue: TJsonNode);
  public
    constructor Create(aExpr: TJpExpression);
    destructor Destroy; override;
    function Apply(aNode: TJsonNode): Boolean;
    property Root: TJsonNode write SetRoot;
  end;

type
  EJpParse = class(Exception);

  { TJpQueryParser }
  TJpQueryParser = class
  strict private
  type
    TIntParseResult = (iprOk, iprInvalid, iprRange);
    TJpTokenKind = (
      jtkLParen, jtkRParen, jtkBoolOr,  jtkBoolAnd, jtkBoolNot, jtkLess, jtkGreater, jtkEqual,
      jtkNotEqual, jtkLessOrEq,  jtkGreatOrEq, jtkNull, jtkFalse, jtkTrue, jtkString, jtkNumber,
      jtkFunction, jtkRootId, jtkCurrId, jtkComma, jtkRBracket
    );
  const
    LITERALS       = [jtkNull, jtkFalse, jtkTrue, jtkString, jtkNumber];
    DEC_DIGITS     = ['0'..'9'];
    MINUS_OR_DIGIT = DEC_DIGITS + ['-'];
    WHITE_SPACE    = [#9, #10, #13, ' '];
    NAME_FIRST     = ['A'..'Z', 'a'..'z', '_'];
    NAME_CHAR      = NAME_FIRST + ['0'..'9'];
    HEX_DIGITS     = ['0'..'9', 'A', 'B', 'C', 'D', 'E', 'F', 'a', 'b', 'c', 'd', 'e', 'f'];
    FUN_NAME_FIRST = ['a'..'z'];
    FUN_NAME_CHAR  = FUN_NAME_FIRST + ['0'..'9', '_'];
  var
    FQuery: string;
    FPath: TJsonPathQuery;
    FLook,
    FLast: PAnsiChar;
    FStrValue: string;
    FNumValue: Double;
    FFunDef: TJpFunctionDef;
    FToken: TJpTokenKind;
    class function ParseIntValue(p: PAnsiChar; aCount: SizeInt; out aValue: SizeInt): TIntParseResult; static;
    class function MakeString(p: PAnsiChar; aCount: SizeInt): string; static; inline;
    class function StrDecode(p: PAnsiChar; aCount: SizeInt): string; static;
    function  GetPos: SizeInt; inline;
    function  Eof: Boolean; inline;
    procedure SkipWhiteSpace; inline;
    procedure SkipChar; inline;
    procedure SkipCharThenWS; inline;
    function  CurrChar: AnsiChar; inline;
    procedure Fail(const aMsg: string);
    procedure Fail(const aFmt: string; const aArgs: array of const);
    procedure CheckEof; inline;
    procedure NextToken;
    procedure Number;
    procedure LiteralOrFun;
    procedure ShorthandName(aPath: TJsonPathQuery);
    procedure Descendant(aPath: TJsonPathQuery);
    procedure DotSegment(aPath: TJsonPathQuery);
    procedure UnicodeEntry;
    procedure QuoteNameEscape; inline;
    function  GetQuoteName: string;
    procedure DblQuoteNameEscape; inline;
    function  GetDblQuoteName: string;
    procedure ParseInt(out aValue: SizeInt);
    procedure IndexOrSlice(aSegment: TSegment); inline;
    procedure Slice(aSegment: TSegment; const aStart: TOptionalInt);
    function  ExprLevel0(aSkip: Boolean): TJpExpression;
    function  ExprLevel1(aSkip: Boolean): TJpExpression;
    function  ExprLevel2(aSkip: Boolean): TJpExpression;
    function  ExprLevel3(aSkip: Boolean): TJpExpression;
    function  ExprLevel4: TJpExpression;
    function  SubQueryExpr: TJpExpression;
    function  FunctionExpr: TJpExpression;
    function  GetFilter: TJpFilter;
    procedure BracketSegment(aPath: TJsonPathQuery);
    procedure DoParse(aPath: TJsonPathQuery);
    property  Position: SizeInt read GetPos;
    property  CurrToken: TJpTokenKind read FToken;
  public
    constructor Create(const aQuery: string; aPath: TJsonPathQuery);
    procedure TryParse;
  end;

  { TJpMatcher }
  TJpMatcher = class(TInterfacedObject, IJsonPath)
  strict private
    FPath: TJsonSpecPathQuery;
    FMessage: string;
  public
    constructor Create;
    destructor Destroy; override;
    function  TryParseQuery(const aQuery: string): Boolean;
    function  Match(aRoot: TJsonNode): TJpNodeList;
    function  Match(const aRoot: string; out aNodeList: string): boolean;
    function  MatchValues(aRoot: TJsonNode): TJpValueList;
    function  MatchValues(const aRoot: string; out aNodeArray: string): Boolean;
    function  MatchFirst(aRoot: TJsonNode): TJpNode;
    function  MatchFirst(const aRoot: string; out aNode: string): Boolean;
    function  MatchFirstValue(aRoot: TJsonNode): TJsonNode;
    function  MatchFirstValue(const aRoot: string; out aValue: string): Boolean;
    property  Message: string read FMessage;
  end;

{ TSlice }

procedure TSlice.Clear;
begin
  Start.Clear;
  Stop.Clear;
  Step.Clear;
end;

{ TSelector }

function TSelector.GetIndex: SizeInt;
begin
  Result := -1;
end;

function TSelector.GetName: string;
begin
  Result := '';
end;

function TSelector.GetSlice: PSlice;
begin
  Result := nil;
end;

function TSelector.GetFilter: TJpFilter;
begin
  Result := nil;
end;

{ TWildcardSelector }

constructor TWildcardSelector.Create;
begin
  FKind := skWildcard;
end;

{ TIndexSelector }

function TIndexSelector.GetIndex: SizeInt;
begin
  Result := FIndex;
end;

constructor TIndexSelector.Create(aIndex: SizeInt);
begin
  FKind := skIndex;
  FIndex := aIndex;
end;

{ TNameSelector }

function TNameSelector.GetName: string;
begin
  Result := FName;
end;

constructor TNameSelector.Create(const aName: string);
begin
  FKind := skName;
  FName := aName;
end;

{ TSliceSelector }

function TSliceSelector.GetSlice: PSlice;
begin
  Result := @FSlice;
end;

constructor TSliceSelector.Create(const aSlice: TSlice);
begin
  FKind := skSlice;
  FSlice := aSlice;
end;

{ TFilterSelector }

function TFilterSelector.GetFilter: TJpFilter;
begin
  Result := FFilter;
end;

constructor TFilterSelector.Create(aFilter: TJpFilter);
begin
  FKind := skFilter;
  FFilter := aFilter;
end;

destructor TFilterSelector.Destroy;
begin
  FFilter.Free;
  inherited;
end;

{ TSegment }

procedure TSegment.SetNext(aValue: TSegment);
var
  s: TSelector;
begin
  FNext := aValue;
  for s in FSelectors do
    s.NextSegment := aValue;
end;

function TSegment.GetCount: SizeInt;
begin
  Result := FSelectors.Count;
end;

destructor TSegment.Destroy;
var
  s: TSelector;
begin
  for s in FSelectors do
    s.Free;
  inherited;
end;

function TSegment.GetEnumerator: TSelectorEnum;
begin
  Result := FSelectors.GetEnumerator;
end;

function TSegment.GetFirstSelector: TSelector;
begin
  if FSelectors.IsEmpty then
    Result := nil
  else
    Result := FSelectors[0];
end;

procedure TSegment.AddSelector(aIndex: SizeInt);
begin
  Assert(aIndex = aIndex); // make compiler happy
end;

procedure TSegment.AddSelector(const aName: string);
begin
  Assert(aName = aName);
end;

procedure TSegment.AddWildcard;
begin
end;

procedure TSegment.AddSelector(const s: TSlice);
begin
  Assert(s.Start.Assigned = s.Start.Assigned);
end;

procedure TSegment.AddSelector(aFilter: TJpFilter);
begin
  Assert(aFilter = aFilter);
end;

{ TDescSegment }

constructor TDescSegment.Create;
begin
  FKind := sgkDescendant;
end;

{ TChildSegment }

constructor TChildSegment.Create;
begin
  FKind := sgkChild;
end;

procedure TChildSegment.AddSelector(aIndex: SizeInt);
begin
  FSelectors.Add(TIndexSelector.Create(aIndex));
end;

procedure TChildSegment.AddSelector(const aName: string);
begin
  FSelectors.Add(TNameSelector.Create(aName));
end;

procedure TChildSegment.AddWildcard;
begin
  FSelectors.Add(TWildcardSelector.Create);
end;

procedure TChildSegment.AddSelector(const aSlice: TSlice);
begin
  FSelectors.Add(TSliceSelector.Create(aSlice));
end;

procedure TChildSegment.AddSelector(aFilter: TJpFilter);
begin
  FSelectors.Add(TFilterSelector.Create(aFilter));
end;

{ TJsonPath }

procedure TJsonPathQuery.AddMatch(aNode: TJsonNode);
begin
  case FApply of
    akCount: Inc(FCount);
    akFirstMatch:
      begin
        FFirst := aNode;
        FDone := True;
      end;
    akMatchList: FMatchCollector.Add(aNode);
  end;
end;

procedure TJsonPathQuery.ApplySlice(aSel: TSelector; aNode: TJsonNode);
var
  I, Len, Start, Stop, Step: SizeInt;
begin
  Step := aSel.Slice^.Step.OrElse(1);
  if (aNode.Count = 0) or (Step = 0) then exit;

  Len := aNode.Count;

  if Step < 0 then begin
    Start := aSel.Slice^.Start.OrElse(Pred(Len));
    if Start < 0 then Start += Len; // normalize

    Stop := aSel.Slice^.Stop.OrElse(Pred(-Len));
    if Stop < 0 then Stop += Len;   // normalize

    Start := Math.Min(Math.Max(Start, -1), Pred(Len));
    Stop := Math.Min(Math.Max(Stop, -1), Pred(Len));
    I := Start;
    while I > Stop do begin
      if aSel.NextSegment = nil then
        AddMatch(aNode.Items[I])
      else
        ApplySegment(aSel.NextSegment, aNode.Items[I]);
      if Done then exit;
      Inc(I, Step);
    end;
  end else begin
    Start := aSel.Slice^.Start.OrElse(0);
    if Start < 0 then Start += Len; // normalize

    Stop := aSel.Slice^.Stop.OrElse(Len);
    if Stop < 0 then Stop += Len;   // normalize

    Start := Math.Min(Math.Max(Start, 0), Len);
    Stop := Math.Min(Math.Max(Stop, 0), Len);
    I := Start;
    while I < Stop do begin
      if aSel.NextSegment = nil then
        AddMatch(aNode.Items[I])
      else
        ApplySegment(aSel.NextSegment, aNode.Items[I]);
      if Done then exit;
      Inc(I, Step);
    end;
  end;
end;

procedure TJsonPathQuery.ApplySelector(aSel: TSelector; aRoot: TJsonNode; aDesc: Boolean);
var
  I: SizeInt;
  LValue: TJsonNode;
begin
  if Done then exit;
  case aSel.Kind of
    skIndex:
      if aRoot.Kind = jvkArray then
        begin
          I := aSel.Index;
          if I < 0 then
            I += aRoot.Count;
          if aRoot.Find(I, LValue) then
            if aSel.NextSegment = nil then
              AddMatch(LValue)
            else
              ApplySegment(aSel.NextSegment, LValue);
        end;

    skName:
      if (aRoot.Kind = jvkObject) and aRoot.Find(aSel.Name, LValue) then
        if aSel.NextSegment = nil then
          AddMatch(LValue)
        else
          ApplySegment(aSel.NextSegment, LValue);

    skWildcard:
      for LValue in aRoot do begin
        if aSel.NextSegment = nil then
          AddMatch(LValue)
        else
          ApplySegment(aSel.NextSegment, LValue);
        if Done then exit;
      end;

    skSlice:
      if aRoot.Kind = jvkArray then
        ApplySlice(aSel, aRoot);

    skFilter:
      begin
        aSel.Filter.Root := FRoot;
        for LValue in aRoot do
          if aSel.Filter.Apply(LValue) then begin
            if aSel.NextSegment = nil then
              AddMatch(LValue)
            else
              ApplySegment(aSel.NextSegment, LValue);
            if Done then exit;
          end;
      end;
  else
  end;
  if aDesc then
    for LValue in aRoot do begin
      if Done then exit;
      ApplySelector(aSel, LValue, True);
    end;
end;

procedure TJsonPathQuery.ApplySegment(aSegment: TSegment; aRoot: TJsonNode);
var
  Sel: TSelector;
begin
  if aSegment.Kind = sgkDescendant then
    for Sel in aSegment.Next do
      begin
        if Done then exit;
        ApplySelector(Sel, aRoot, True);
      end
  else
    for Sel in aSegment do
      begin
        if Done then exit;
        ApplySelector(Sel, aRoot);
      end;
end;

destructor TJsonPathQuery.Destroy;
begin
  Clear;
  inherited;
end;

procedure TJsonPathQuery.Clear;
var
  s: TSegment;
begin
  for s in FSegments do
    s.Free;
  FSegments.MakeEmpty;
end;

function TJsonPathQuery.AddChildSegment: TSegment;
var
  I: SizeInt;
begin
  I := FSegments.Add(TChildSegment.Create);
  Result := FSegments.UncMutable[I]^;
  if I > 0 then
    FSegments.UncMutable[Pred(I)]^.Next := Result;
end;

function TJsonPathQuery.AddDescendant: TSegment;
var
  I: SizeInt;
begin
  I := FSegments.Add(TDescSegment.Create);
  Result := FSegments.UncMutable[I]^;
  if I > 0 then
    FSegments.UncMutable[Pred(I)]^.Next := Result;
end;

function TJsonPathQuery.IsSingular: Boolean;
var
  Seg: TSegment;
  Sel: TSelector;
begin
  for Seg in FSegments do
    begin
      if (Seg.Kind <> sgkChild) or (Seg.Count > 1) then exit(False);
      Sel := Seg.GetFirstSelector;
      if (Sel <> nil) and not(Sel.Kind in [skIndex, skName]) then exit(False);
    end;
  Result := True;
end;

function TJsonPathQuery.GetCount(aRoot: TJsonNode): SizeInt;
begin
  if FSegments.IsEmpty then exit(1);
  FRoot := aRoot;
  FApply := akCount;
  FDone := False;
  ApplySegment(FSegments[0], aRoot);
  Result := FCount;
end;

function TJsonPathQuery.Apply(aRoot: TJsonNode): TJpValueList;
begin
  FMatchCollector.MakeEmpty;
  if FSegments.IsEmpty then exit([aRoot]);
  FRoot := aRoot;
  FApply := akMatchList;
  FDone := False;
  ApplySegment(FSegments[0], aRoot);
  Result := FMatchCollector.ToArray;
end;

function TJsonPathQuery.GetFirst(aRoot: TJsonNode): TJsonNode;
begin
  if FSegments.IsEmpty then exit(aRoot);
  FRoot := aRoot;
  FApply := akFirstMatch;
  FFirst := nil;
  FDone := False;
  ApplySegment(FSegments[0], aRoot);
  Result := FFirst;
end;

function TJsonPathQuery.TestExists(aRoot: TJsonNode): Boolean;
begin
  Result := GetFirst(aRoot) <> nil;
end;

{ TJsonSpecPathQuery }

{$PUSH}{$WARN 5091 OFF}
function TJsonSpecPathQuery.EncodeSegment(const s: string): string;
var
  r: string;
  I, Len: SizeInt;
  p: PAnsiChar;
begin
  System.SetLength(r, System.Length(s) * 6 + SizeOf(TChar2) * 2);
  p := Pointer(r);
  PChar2(p)^ := SEG_OPEN;
  p += 2;
  Len := 2;
  for I := 1 to System.Length(s) do
    case s[I] of
      #0..#7, #11, #14..#31:
        begin
           PChar4(p)^ := '\u00';
           p[4] := HEX_CHARS_TBL[Ord(s[I]) shr  4];
           p[5] := HEX_CHARS_TBL[Ord(s[I]) and 15];
           p += 6; Len += 6;
        end;
      #8 :  begin PChar2(p)^ := '\b'; p += 2; Len += 2; end;
      #9 :  begin PChar2(p)^ := '\t'; p += 2; Len += 2; end;
      #10:  begin PChar2(p)^ := '\n'; p += 2; Len += 2; end;
      #12:  begin PChar2(p)^ := '\f'; p += 2; Len += 2; end;
      #13:  begin PChar2(p)^ := '\r'; p += 2; Len += 2; end;
      '\':  begin PChar2(p)^ := '\\'; p += 2; Len += 2; end;
      '''': begin PChar2(p)^ := '\'''; p += 2; Len += 2; end;
      '"':  begin PChar2(p)^ := '\"'; p += 2; Len += 2; end;
    else
      p^ := s[I];
      Inc(p); Inc(Len);
    end;
  PChar2(p)^ := SEG_CLOSE;
  System.SetLength(r, Len + 2);
  Result := r;
end;
{$POP}

procedure TJsonSpecPathQuery.PathPush(const aSegment: string);
begin
  FPathHolder.Add(EncodeSegment(aSegment));
end;

procedure TJsonSpecPathQuery.PathPush(aSegment: SizeInt);
begin
  FPathHolder.Add('[' + SizeInt2Str(aSegment) + ']');
end;

procedure TJsonSpecPathQuery.PathPop;
begin
  FPathHolder.DeleteLast;
end;

procedure TJsonSpecPathQuery.AddMatchWithPath(aNode: TJsonNode);
begin
  FMatchPathCollector.Add(TJpNode.Make('$' + string.Join('', FPathHolder.ToArray), aNode));
  FDone := FApply <> akMatchList;
end;

procedure TJsonSpecPathQuery.ApplySliceWithPath(aSel: TSelector; aNode: TJsonNode);
var
  I, Len, Start, Stop, Step: SizeInt;
begin
  Step := aSel.Slice^.Step.OrElse(1);
  if (aNode.Count = 0) or (Step = 0) then exit;

  Len := aNode.Count;

  if Step < 0 then begin
    Start := aSel.Slice^.Start.OrElse(Pred(Len));
    if Start < 0 then Start += Len;

    Stop := aSel.Slice^.Stop.OrElse(Pred(-Len));
    if Stop < 0 then Stop += Len;

    Start := Math.Min(Math.Max(Start, -1), Pred(Len));
    Stop := Math.Min(Math.Max(Stop, -1), Pred(Len));
    I := Start;
    while I > Stop do begin
      PathPush(I);
      if aSel.NextSegment = nil then
        AddMatchWithPath(aNode.Items[I])
      else
        ApplySegmentWithPath(aSel.NextSegment, aNode.Items[I]);
      PathPop;
      if Done then exit;
      Inc(I, Step);
    end;
  end else begin
    Start := aSel.Slice^.Start.OrElse(0);
    if Start < 0 then Start += Len;

    Stop := aSel.Slice^.Stop.OrElse(Len);
    if Stop < 0 then Stop += Len;

    Start := Math.Min(Math.Max(Start, 0), Len);
    Stop := Math.Min(Math.Max(Stop, 0), Len);
    I := Start;
    while I < Stop do begin
      PathPush(I);
      if aSel.NextSegment = nil then
        AddMatchWithPath(aNode.Items[I])
      else
        ApplySegmentWithPath(aSel.NextSegment, aNode.Items[I]);
      PathPop;
      if Done then exit;
      Inc(I, Step);
    end;
  end;
end;

procedure TJsonSpecPathQuery.ApplySelectorWithPath(aSel: TSelector; aRoot: TJsonNode; aDesc: Boolean);
var
  I: SizeInt;
  LValue: TJsonNode;
begin
  if Done then exit;
  case aSel.Kind of
    skIndex:
      if aRoot.Kind = jvkArray then
        begin
          I := aSel.Index;
          if I < 0 then
            I += aRoot.Count;
          if aRoot.Find(I, LValue) then begin
            PathPush(I);
            if aSel.NextSegment = nil then
              AddMatchWithPath(LValue)
            else
              ApplySegmentWithPath(aSel.NextSegment, LValue);
            PathPop;
          end;
        end;

    skName:
      if (aRoot.Kind = jvkObject) and aRoot.Find(aSel.Name, LValue) then begin
        PathPush(aSel.Name);
        if aSel.NextSegment = nil then
          AddMatchWithPath(LValue)
        else
          ApplySegmentWithPath(aSel.NextSegment, LValue);
        PathPop;
      end;

    skWildcard:
      case aRoot.Kind of
        jvkArray:
          for I := 0 to Pred(aRoot.Count) do begin
            PathPush(I);
            if aSel.NextSegment = nil then
              AddMatchWithPath(aRoot.Items[I])
            else
              ApplySegmentWithPath(aSel.NextSegment, aRoot.Items[I]);
            PathPop;
            if Done then exit;
          end;
        jvkObject:
          for I := 0 to Pred(aRoot.Count) do
            with aRoot.Pairs[I] do begin
              PathPush(Key);
              if aSel.NextSegment = nil then
                AddMatchWithPath(Value)
              else
                ApplySegmentWithPath(aSel.NextSegment, Value);
              PathPop;
              if Done then exit;
            end;
      else
      end;

    skSlice:
      if aRoot.Kind = jvkArray then
        ApplySliceWithPath(aSel, aRoot);

    skFilter:
      begin
        aSel.Filter.Root := FRoot;
        case aRoot.Kind of
          jvkArray:
            for I := 0 to Pred(aRoot.Count) do begin
              PathPush(I);
              LValue := aRoot.Items[I];
              if aSel.Filter.Apply(LValue) then
                if aSel.NextSegment = nil then
                  AddMatchWithPath(LValue)
                else
                  ApplySegmentWithPath(aSel.NextSegment, LValue);
              PathPop;
              if Done then exit;
            end;
          jvkObject:
            for I := 0 to Pred(aRoot.Count) do
              with aRoot.Pairs[I] do begin
                PathPush(Key);
                if aSel.Filter.Apply(Value) then
                  if aSel.NextSegment = nil then
                    AddMatchWithPath(Value)
                  else
                    ApplySegmentWithPath(aSel.NextSegment, Value);
                PathPop;
                if Done then exit;
              end;
        else
        end;
      end;
  else
  end;
  if aDesc then
    case aRoot.Kind of
      jvkArray:
        for I := 0 to Pred(aRoot.Count) do begin
          if Done then exit;
          PathPush(I);
          ApplySelectorWithPath(aSel, aRoot.Items[I], True);
          PathPop;
        end;
      jvkObject:
        for I := 0 to Pred(aRoot.Count) do begin
          if Done then exit;
          with aRoot.Pairs[I] do begin
            PathPush(Key);
            ApplySelectorWithPath(aSel, Value, True);
            PathPop;
          end;
        end;
    else
    end;
end;

procedure TJsonSpecPathQuery.ApplySegmentWithPath(aSegment: TSegment; aRoot: TJsonNode);
var
  Sel: TSelector;
begin
  if aSegment.Kind = sgkDescendant then
    for Sel in aSegment.Next do
      begin
        if Done then exit;
        ApplySelectorWithPath(Sel, aRoot, True);
      end
  else
    for Sel in aSegment do
      begin
        if Done then exit;
        ApplySelectorWithPath(Sel, aRoot);
      end;
end;

function TJsonSpecPathQuery.ApplyWithPath(aRoot: TJsonNode): TJpNodeList;
begin
  FMatchPathCollector.MakeEmpty;
  if FSegments.IsEmpty then exit([TJpNode.Make('$', aRoot)]);
  FRoot := aRoot;
  FApply := akMatchList;
  FDone := False;
  ApplySegmentWithPath(FSegments[0], aRoot);
  Result := FMatchPathCollector.ToArray;
end;

function TJsonSpecPathQuery.GetFirstWithPath(aRoot: TJsonNode): TJpNode;
begin
  FMatchPathCollector.MakeEmpty;
  if FSegments.IsEmpty then exit(TJpNode.Make('$', aRoot));
  FRoot := aRoot;
  FApply := akFirstMatch;
  FDone := False;
  ApplySegmentWithPath(FSegments[0], aRoot);
  if FMatchPathCollector.NonEmpty then
    Result := FMatchPathCollector[0]
  else
    Result := TJpNode.Make('', nil);
end;

{ TJpExpression }

{$PUSH}{$WARN 5024 OFF}
procedure TJpExpression.Eval(const aCtx: TJpFilterContext; var v: TJpInstance);
begin
end;
{$POP}

function TJpExpression.GetValue(const aCtx: TJpFilterContext): TJpInstance;
begin
  Eval(aCtx, FValue);
  Result := FValue;
end;

function TJpExpression.Apply(const aCtx: TJpFilterContext): Boolean;
begin
  Eval(aCtx, FValue);
  Result := FValue.AsBoolean;
end;

{ TJpConstExpr }

constructor TJpConstExpr.CreateNull;
begin
  FValue := TJpValue.ValNull;
end;

constructor TJpConstExpr.Create(b: Boolean);
begin
  if b then
    FValue := TJpValue.ValTrue
  else
    FValue := TJpValue.ValFalse;
end;

constructor TJpConstExpr.Create(const s: string);
begin
  FValue := s;
end;

constructor TJpConstExpr.Create(d: Double);
begin
  FValue := d;
end;


{ TRelPathQuery }

constructor TRelPathQuery.Create(aId: TJpIdentifier);
begin
  FIdent := aId;
end;

function TRelPathQuery.GetNodeCount(const aCtx: TJpFilterContext): SizeInt;
begin
  if FIdent = jpiRoot then
    Result := GetCount(aCtx.Root)
  else
    Result := GetCount(aCtx.Current);
end;

function TRelPathQuery.GetFirstNode(const aCtx: TJpFilterContext): TJsonNode;
begin
  if FIdent = jpiRoot then
    Result := GetFirst(aCtx.Root)
  else
    Result := GetFirst(aCtx.Current);
end;

function TRelPathQuery.AsNodeList(const aCtx: TJpFilterContext): TJpValueList;
begin
  if FIdent = jpiRoot then
    Result := Apply(aCtx.Root)
  else
    Result := Apply(aCtx.Current);
end;

function TRelPathQuery.AsBoolean(const aCtx: TJpFilterContext): Boolean;
begin
  if FIdent = jpiRoot then
    Result := TestExists(aCtx.Root)
  else
    Result := TestExists(aCtx.Current);
end;

{ TNotPredicate }

procedure TJpNotPredicate.Eval(const aCtx: TJpFilterContext; var v: TJpInstance);
begin
  v := not FExpr.Apply(aCtx);
end;

constructor TJpNotPredicate.Create(aExpr: TJpExpression);
begin
  FExpr := aExpr;
end;

destructor TJpNotPredicate.Destroy;
begin
  FExpr.Free;
  inherited;
end;

{ TJpBinExpr }

constructor TJpBinaryExpr.Create(aLeft, aRight: TJpExpression);
begin
  FLeft := aLeft;
  FRight := aRight;
end;

destructor TJpBinaryExpr.Destroy;
begin
  FLeft.Free;
  FRight.Free;
  inherited;
end;

{ TAndPredicate }

procedure TJpAndPredicate.Eval(const aCtx: TJpFilterContext; var v: TJpInstance);
begin
  if not Left.Apply(aCtx) then
    v := False
  else
    v := Right.Apply(aCtx);
end;

{ TOrPredicate }

procedure TJpOrPredicate.Eval(const aCtx: TJpFilterContext; var v: TJpInstance);
begin
  if Left.Apply(aCtx) then
    v := True
  else
    v := Right.Apply(aCtx);
end;

{ TRelQueryExpr }

procedure TJpRelQueryExpr.Eval(const aCtx: TJpFilterContext; var v: TJpInstance);
begin
  if FTesting then
    v := FPath.AsBoolean(aCtx)
  else
    if FSingular then
      v := FPath.GetFirstNode(aCtx)
    else
      v := TJpInstance.Nothing;
end;

constructor TJpRelQueryExpr.Create(aPath: TRelPathQuery);
begin
  FPath := aPath;
  FSingular := aPath.IsSingular;
end;

destructor TJpRelQueryExpr.Destroy;
begin
  FPath.Free;
  inherited;
end;

function TJpRelQueryExpr.Apply(const aCtx: TJpFilterContext): Boolean;
begin
  FTesting := True;
  Result := inherited Apply(aCtx);
  FTesting := False;
end;

{ TJpEqualExpr }

procedure TJpEqualExpr.Eval(const aCtx: TJpFilterContext; var v: TJpInstance);
begin
  v := Left.GetValue(aCtx) = Right.GetValue(aCtx);
end;

{ TJpNotEqualExpr }

procedure TJpNotEqualExpr.Eval(const aCtx: TJpFilterContext; var v: TJpInstance);
begin
  v := Left.GetValue(aCtx) <> Right.GetValue(aCtx);
end;

{ TJpLessThanExpr }

procedure TJpLessThanExpr.Eval(const aCtx: TJpFilterContext; var v: TJpInstance);
begin
  v := Left.GetValue(aCtx) < Right.GetValue(aCtx);
end;

{ TJpLessThanOrEqualExpr }

procedure TJpLessOrEqualExpr.Eval(const aCtx: TJpFilterContext; var v: TJpInstance);
begin
  v := Left.GetValue(aCtx) <= Right.GetValue(aCtx);
end;

{ TJpGreaterThanExpr }

procedure TJpGreaterThanExpr.Eval(const aCtx: TJpFilterContext; var v: TJpInstance);
begin
  v := Left.GetValue(aCtx) > Right.GetValue(aCtx);
end;

{ TJpGreaterOrEqualExpr }

procedure TJpGreaterOrEqualExpr.Eval(const aCtx: TJpFilterContext; var v: TJpInstance);
begin
  v := Left.GetValue(aCtx) >= Right.GetValue(aCtx);
end;

{ TJpFunctionExpr }

procedure TJpFunctionExpr.Eval(const aCtx: TJpFilterContext; var v: TJpInstance);
var
  Params: TJpParamList = nil;
  I: SizeInt;
begin
  System.SetLength(Params, System.Length(FParamList));
  for I := 0 to System.High(FParamList) do
    Params[I] := FParamList[I].GetValue(aCtx);
  FFunCall(Params, v);
end;

constructor TJpFunctionExpr.Create(const aParams: TJpExprList; aFun: TJpFunctionCall);
begin
  FParamList := aParams;
  FFunCall := aFun;
end;

destructor TJpFunctionExpr.Destroy;
var
  e: TJpExpression;
begin
  for e in FParamList do
    e.Free;
  inherited;
end;

{ TJpFilter }

procedure TJpFilter.SetRoot(aValue: TJsonNode);
begin
  FContext.Root := aValue;
end;

constructor TJpFilter.Create(aExpr: TJpExpression);
begin
  FExpr := aExpr;
end;

destructor TJpFilter.Destroy;
begin
  FExpr.Free;
  inherited;
end;

function TJpFilter.Apply(aNode: TJsonNode): Boolean;
begin
  if FExpr <> nil then  ///////////////// ?????
    begin
      FContext.Current := aNode;
      Result := FExpr.Apply(FContext);
    end
  else
    Result := False;
end;

{ TJpParser }

{$PUSH}{$Q-}{$R-}{$J-}
class function TJpQueryParser.ParseIntValue(p: PAnsiChar; aCount: SizeInt; out aValue: SizeInt): TIntParseResult;
var
  v: SizeUInt;
  I, Len: SizeInt;
  IsNeg: Boolean = False;
const
  DECIMAL_DIGS: array['0'..'9'] of Byte = (0,1,2,3,4,5,6,7,8,9);
{$IF DEFINED(CPU32)}
  MAX_DIGITS = 10;
  TEST_BOUND = LongInt(1000000000);
{$ELSEIF DEFINED(CPU64)}
  MAX_DIGITS = 16;
{$ELSE }
  {$FATAL 8/16-bit Cpu's not supported}
{$ENDIF}
begin
  if aCount < 1 then exit(iprInvalid);
  I := 0;
  if (p^ = '-') then begin
    if aCount < 2 then exit(iprInvalid);
    IsNeg := True;
    Inc(I);
  end;

  if not(p[I] in DEC_DIGITS) then
    exit(iprInvalid)
  else
    if p[I] = '0' then
      if (aCount = Succ(I)) and not IsNeg then begin
        aValue := 0;
        exit(iprOk)
      end else
        exit(iprInvalid);

  v := DECIMAL_DIGS[p[I]];
  Inc(I);
  Len := 1;
  for I := I to Pred(aCount) do begin
    if not (p[I] in DEC_DIGITS) then exit(iprInvalid);
    v := v * 10 + DECIMAL_DIGS[p[I]];
    Inc(Len);
    if Len > MAX_DIGITS then exit(iprRange);
  end;
{$IF DEFINED(CPU32)}
  if (Len = MAX_DIGITS) and (v < TEST_BOUND) then exit(piOverflow);
  if (IsNeg and (v > SizeUInt(System.High(SizeInt))+1)) or (v > System.High(SizeInt)) then
    exit(piOverflow);
{$ELSEIF DEFINED(CPU64)}
  if v > 9007199254740991 then
    exit(iprRange); //integer value MUST be within the range of exact values
{$ENDIF }
  if IsNeg then
    aValue := -SizeInt(v)
  else
    aValue := SizeInt(v);
  Result := iprOk;
end;

class function TJpQueryParser.MakeString(p: PAnsiChar; aCount: SizeInt): string;
begin
  Result := '';
  if aCount > 0 then
    begin
      System.SetLength(Result, aCount);
      System.Move(p^, Pointer(Result)^, aCount);
    end;
end;

function HexCh4ToDWord(const aSeq: TChar4): DWord; inline;
const
  x: array['0'..'f'] of DWord = (
   0, 1, 2, 3, 4, 5, 6, 7, 8, 9,15,15,15,15,15,15,
  15,10,11,12,13,14,15,15,15,15,15,15,15,15,15,15,
  15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,
  15,10,11,12,13,14,15);
begin
  Result := x[aSeq[0]] shl 12 or x[aSeq[1]] shl 8 or x[aSeq[2]] shl 4 or x[aSeq[3]];
end;
{$POP}

{$PUSH}{$WARN 5091 OFF}
class function TJpQueryParser.StrDecode(p: PAnsiChar; aCount: SizeInt): string;
var
  r: string;
  I, J, Last: SizeInt;
  pR: PAnsiChar;
  uh, ul: DWord;
begin
  Result := '';
  if aCount < 1 then exit;
  System.SetLength(r, aCount);
  Last := aCount;
  I := 0;
  J := 0;
  pR := PAnsiChar(r);
  while I < Last do
    if p[I] <> '\' then begin
      pR[J] := p[I];
      Inc(I);
      Inc(J);
    end else
      case p[Succ(I)] of
        'b': begin pR[J] := #8;  I += 2; Inc(J); end;
        'f': begin pR[J] := #12; I += 2; Inc(J); end;
        'n': begin pR[J] := #10; I += 2; Inc(J); end;
        'r': begin pR[J] := #13; I += 2; Inc(J); end;
        't': begin pR[J] := #9;  I += 2; Inc(J); end;
        'u':
          begin
            uh := HexCh4ToDWord(PChar4(@p[I+2])^);
            I += 6;
            case uh of
              0..$7f: begin pR[J] := Char(uh); Inc(J); end;
              $80..$7ff: begin
                  pR[J] := Char((uh shr 6) or $c0);
                  pR[J+1] := Char((uh and $3f) or $80);
                  J += 2;
                end;
              $800..$d7ff,$e000..$ffff: begin
                  pR[J] := Char((uh shr 12) or $e0);
                  pR[J+1] := Char((uh shr 6) and $3f or $80);
                  pR[J+2] := Char((uh and $3f) or $80);
                  J += 3;
                end;
              $d800..$dbff: // high surrogate
                if (Last - I >= 5) and (p[I] = '\') and (p[I+1] = 'u') then begin
                  ul := HexCh4ToDWord(PChar4(@p[I+2])^);
                  if (ul >= $dc00) and (ul <= $dfff) then begin
                    I += 6;
                    ul := (uh - $d7c0) shl 10 + (ul xor $dc00);
                    pR[J] := Char(ul shr 18 or $f0);
                    pR[J+1] := Char((ul shr 12) and $3f or $80);
                    pR[J+2] := Char((ul shr 6) and $3f or $80);
                    pR[J+3] := Char(ul and $3f or $80);
                    J += 4;
                  end else begin
                    pR[J] := '?';
                    Inc(J);
                  end;
                end else begin
                  pR[J] := '?';
                  Inc(J);
                end;
              $dc00..$dfff: begin // low surrogate
                  pR[J] := '?';
                  Inc(J);
                end;
            else
            end;
          end;
      else
        pR[J] := p[Succ(I)];
        I += 2;
        Inc(J)
      end;
  System.SetLength(r, J);
  Result := r;
end;
{$POP}

function TJpQueryParser.GetPos: SizeInt;
begin
  Result := Succ(FLook - PAnsiChar(FQuery));
end;

function TJpQueryParser.Eof: Boolean;
begin
  Result := FLook > FLast;
end;

procedure TJpQueryParser.SkipWhiteSpace;
begin
  while not Eof and (FLook^ in WHITE_SPACE) do Inc(FLook);
end;

procedure TJpQueryParser.SkipChar;
begin
  Inc(FLook);
end;

procedure TJpQueryParser.SkipCharThenWS;
begin
  Inc(FLook);
  SkipWhiteSpace;
end;

function TJpQueryParser.CurrChar: AnsiChar;
begin
  Result := FLook^;
end;

procedure TJpQueryParser.Fail(const aMsg: string);
begin
  raise EJpParse.Create(aMsg);
end;

procedure TJpQueryParser.Fail(const aFmt: string; const aArgs: array of const);
begin
  raise EJpParse.CreateFmt(aFmt, aArgs);
end;

procedure TJpQueryParser.CheckEof;
begin
  if Eof then
    Fail(SEJPathPosErrorFmt, [Position, SEJPathUnexpectQueryEnd]);
end;

procedure TJpQueryParser.NextToken;
begin
  SkipWhiteSpace;
  CheckEof;
  case CurrChar of
    '''': begin
        FStrValue := GetQuoteName;
        FToken := jtkString;
      end;
    '"': begin
        FStrValue := GetDblQuoteName;
        FToken := jtkString;
      end;
    '!': begin
        SkipChar;
        if CurrChar = '=' then begin
          FToken := jtkNotEqual;
          SkipChar;
        end else
          FToken := jtkBoolNot;
      end;
    '=': begin
        SkipChar;
        if CurrChar <> '=' then
          Fail(SEJPathPosExpectFmt, [Position, SEJPathEqualSign, CurrChar]);
        FToken := jtkEqual;
        SkipChar;
      end;
    '<': begin
        SkipChar;
        if CurrChar = '=' then begin
          FToken := jtkLessOrEq;
          SkipChar;
        end else
          FToken := jtkLess;
      end;
    '>': begin
        SkipChar;
        if CurrChar = '=' then begin
          FToken := jtkGreatOrEq;
          SkipChar;
        end else
          FToken := jtkGreater;
      end;
    '|': begin
        SkipChar;
        if CurrChar <> '|' then
          Fail(SEJPathPosExpectFmt, [Position, SEJPathPipeSign, CurrChar]);
        FToken := jtkBoolOr;
        SkipChar;
      end;
    '&': begin
        SkipChar;
        if CurrChar <> '&' then
          Fail(SEJPathPosExpectFmt, [Position, SEJPathAmpersand, CurrChar]);
        FToken := jtkBoolAnd;
        SkipChar;
      end;
    '$': begin
        FToken := jtkRootId;
        SkipCharThenWS;
      end;
    '@': begin
        FToken := jtkCurrId;
        SkipCharThenWS;
      end;
    '(': begin
        FToken := jtkLParen;
        SkipChar;
      end;
    ')':
      begin
        FToken := jtkRParen;
        SkipChar;
      end;
    '-','0'..'9': Number;
    ',': FToken := jtkComma;
    ']': FToken := jtkRBracket;
  else
    if CurrChar in FUN_NAME_FIRST then
      LiteralOrFun
    else
      Fail(SEJPathPosUnexpectFmt, [Position, CurrChar]);
  end;
end;

procedure TJpQueryParser.Number;
var
  s: string;
  Start: PAnsiChar;
begin
  Assert(CurrChar in MINUS_OR_DIGIT);
  Start := FLook;
  repeat
    SkipChar;
  until not(CurrChar in (DEC_DIGITS + ['.', 'E', 'e', '-', '+']));
  s := MakeString(Start,  FLook - Start);
  if not TryStr2Double(s, FNumValue) then
    Fail(SEJPathPosErrorFmt, [Position, Format(SEJPathInvalidNumFmt, [s])]);
  FToken := jtkNumber;
end;

function FindFunctionDef(const aName: string; out aFunDef: TJpFunctionDef): Boolean; forward;

procedure TJpQueryParser.LiteralOrFun;
var
  s: string;
  Start: PAnsiChar;
begin
  Assert(CurrChar in FUN_NAME_FIRST);
  case CurrChar of
    'f':
      if (FLook[1] = 'a') and (FLook[2] = 'l') and (FLook[3] = 's') and
         (FLook[4] = 'e') and not (FLook[5] in NAME_CHAR) then begin
        FToken := jtkFalse;
        FLook += 5;
        exit;
      end;
    'n':
      if (FLook[1] = 'u') and (FLook[2] = 'l') and (FLook[3] = 'l') and not (FLook[4] in NAME_CHAR) then begin
        FToken := jtkNull;
        FLook += 4;
        exit;
      end;
    't':
      if (FLook[1] = 'r') and (FLook[2] = 'u') and (FLook[3] = 'e') and not (FLook[4] in NAME_CHAR) then begin
        FToken := jtkTrue;
        FLook += 4;
        exit;
      end;
  else
  end;
  Start := FLook;
  repeat
    SkipChar;
  until not(CurrChar in FUN_NAME_CHAR);
  s := MakeString(Start, FLook - Start);
  SkipWhiteSpace;
  if (CurrChar = '(') and FindFunctionDef(s, FFunDef) then
    FToken := jtkFunction
  else
    Fail(SEJPathPosErrorFmt, [Position, Format(SEJPathUnknownSymFmt, [s])]);
end;

procedure TJpQueryParser.ShorthandName(aPath: TJsonPathQuery);
var
  Start: PAnsiChar;
begin
  Assert(CurrChar in NAME_FIRST);
  Start := FLook;
  repeat
    SkipChar;
  until not(CurrChar in NAME_CHAR);
  aPath.AddChildSegment.AddSelector(MakeString(Start, FLook - Start));
  SkipWhiteSpace;
end;

procedure TJpQueryParser.Descendant(aPath: TJsonPathQuery);
begin
  Assert(CurrChar = '.');
  aPath.AddDescendant;
  SkipChar;
  if CurrChar in NAME_FIRST then //maybe followed by shorthand name(leading spaces are not allowed)
    ShorthandName(aPath)
  else
    begin // maybe followed by wildcard
      SkipWhiteSpace;
      if CurrChar = '*' then
        begin
          aPath.AddChildSegment.AddWildcard;
          SkipCharThenWS;
        end;
    end;
end;

procedure TJpQueryParser.DotSegment(aPath: TJsonPathQuery);
begin
  Assert(CurrChar = '.');
  SkipChar;
  case CurrChar of
    '.': Descendant(aPath);
    '*':
      begin //leading spaces before wildcard are not allowed
        aPath.AddChildSegment.AddWildcard;
        SkipCharThenWS;
      end;
  else
    if CurrChar in NAME_FIRST then //leading spaces in shorthand name are not allowed
      ShorthandName(aPath)
  end;
end;

procedure TJpQueryParser.UnicodeEntry;
begin
  //here assumed \u already read
  if FLast < FLook + 3 then
    Fail(SEJPathPosErrorFmt, [Position, SEJPathNoEnoughUniElems]);
  case CurrChar of
    '0'..'9', 'A'..'C' , 'E', 'F', 'a'..'c' , 'e', 'f': //seems non-surrogate
      if (FLook[1] in HEX_DIGITS)and(FLook[2] in HEX_DIGITS)and(FLook[3] in HEX_DIGITS)then begin
        FLook += 4;
      end else
        Fail(SEJPathPosErrorFmt, [Position, SEInvalidUnicodeEntry]);
    'D', 'd':
      case FLook[1] of
        '0'..'7':  //seems non-surrogate
          if (FLook[2] in HEX_DIGITS) and (FLook[3] in HEX_DIGITS)then begin
            FLook += 4;
          end else
            Fail(SEJPathPosErrorFmt, [Position, SEInvalidUnicodeEntry]);
      '8', '9', 'A', 'B', 'a', 'b': //seems high-surrogate
        begin
          if FLast < FLook + 9 then
            Fail(SEJPathPosErrorFmt, [Position, SEJPathNoEnoughSurrElems]);
          if (FLook[1] in HEX_DIGITS) and (FLook[2] in HEX_DIGITS)then begin
            FLook += 4;
          end else
            Fail(SEJPathPosErrorFmt, [Position, SEJPathInvalidHiSurr]);
          if (CurrChar = '\')and(FLook[1] = 'u')and(FLook[2] in ['D', 'd'])and
             (FLook[3] in ['C', 'D', 'E', 'F', 'c', 'd', 'e', 'f']) and
             (FLook[4] in HEX_DIGITS) and (FLook[5] in HEX_DIGITS)then begin
            FLook += 6;
          end else
            Fail(SEJPathPosErrorFmt, [Position, SEJPathInvalidLoSurr]);
        end;
      else
        Fail(SEJPathPosErrorFmt, [Position, SEInvalidUnicodeEntry]);
      end;
  else
    Fail(SEJPathPosErrorFmt, [Position, SEInvalidUnicodeEntry]);
  end;
end;

procedure TJpQueryParser.QuoteNameEscape;
begin
  // expected escapable chars
  case CurrChar of
    'b', 'f', 'n', 'r', 't', '/', '\', '''':
      SkipChar;
    'u':
      begin
        SkipChar;
        UnicodeEntry;
      end
  else
    Fail(SEJPathPosExpectFmt, [Position, SEJPathNameEscapable, CurrChar]);
  end;
end;

function TJpQueryParser.GetQuoteName: string;
var
  Start: PAnsiChar;
begin
  Assert(CurrChar = '''');
  SkipChar;
  CheckEof;
  Start := FLook;
  while not Eof and (CurrChar <> '''') do
    if CurrChar = '\' then
      begin
        SkipChar;
        QuoteNameEscape;
      end
    else
      SkipChar;
  if Eof then
    Fail(SEJPathPosErrorFmt, [Position, SEJPathEndQuoteMiss]);
  Result := StrDecode(Start, FLook - Start);
  SkipCharThenWS;
end;

procedure TJpQueryParser.DblQuoteNameEscape;
begin
  // expected escapable chars
  case CurrChar of
    'b', 'f', 'n', 'r', 't', '/', '\', '"':
      SkipChar;
    'u':
      begin
        SkipChar;
        UnicodeEntry;
      end
  else
    Fail(SEJPathPosExpectFmt, [Position, SEJPathDqNameEscapable, CurrChar]);
  end;
end;

function TJpQueryParser.GetDblQuoteName: string;
var
  Start: PAnsiChar;
begin
  Assert(CurrChar = '"');
  SkipChar;
  CheckEof;
  Start := FLook;
  while not Eof and (CurrChar <> '"') do
    if CurrChar = '\' then
      begin
        SkipChar;
        DblQuoteNameEscape;
      end
    else
      SkipChar;
  if Eof then
    Fail(SEJPathPosErrorFmt, [Position, SEJPathEndDblQuoteMiss]);
  Result := StrDecode(Start, FLook - Start);
  SkipCharThenWS;
end;

procedure TJpQueryParser.ParseInt(out aValue: SizeInt);
var
  Start: PAnsiChar;
begin
  Assert(not Eof and (CurrChar in MINUS_OR_DIGIT));
  Start := FLook;
  repeat
    SkipChar;
  until not(CurrChar in DEC_DIGITS);
  case ParseIntValue(Start, FLook - Start, aValue) of
    iprInvalid:
      Fail(SEJPathPosErrorFmt, [Position, Format(SEJPathInvalidIntFmt, [MakeString(Start, FLook - Start)])]);
    iprRange:
      Fail(SEJPathPosErrorFmt, [Position, Format(SEJPathIntRangeFmt, [MakeString(Start, FLook - Start)])]);
  else
  end;
  SkipWhiteSpace;
end;

procedure TJpQueryParser.IndexOrSlice(aSegment: TSegment);
var
  oi: TOptionalInt;
  Idx: SizeInt;
begin
  Assert(CurrChar in MINUS_OR_DIGIT);
  ParseInt(Idx);
  if CurrChar = ':' then
    begin
      oi := Idx;
      Slice(aSegment, oi);
    end
  else
    aSegment.AddSelector(Idx);
end;

{$PUSH}{$WARN 5089 OFF}
procedure TJpQueryParser.Slice(aSegment: TSegment; const aStart: TOptionalInt);
var
  s: TSlice;
  Value: SizeInt;
  procedure ParseStep; inline;
  begin
    SkipCharThenWS;
    if Eof then
      Fail(SEJPathPosErrorFmt, [Position, SEJPathUnexpectQueryEnd]);
    if CurrChar in MINUS_OR_DIGIT then
      begin
        ParseInt(Value);
        s.Step := Value;
        aSegment.AddSelector(s);
      end
    else
      aSegment.AddSelector(s);
  end;
begin
  Assert(CurrChar = ':');
  if aStart.Assigned then
    s.Start := aStart.Value;
  SkipCharThenWS;
  CheckEof;
  case CurrChar of
    '-', '0'..'9': //stop value?
      begin
        ParseInt(Value);
        s.Stop := Value;
        if CurrChar = ':' then // second colon
          ParseStep
        else
          aSegment.AddSelector(s);
      end;
    ':':  // second colon
      ParseStep;
  else
    aSegment.AddSelector(s);
  end;
end;
{$POP}

function TJpQueryParser.ExprLevel0(aSkip: Boolean): TJpExpression;
begin
  Result := ExprLevel1(aSkip);
  try
    while not(CurrToken in [jtkComma, jtkRBracket]) do
      if CurrToken = jtkBoolOr then
        Result := TJpOrPredicate.Create(Result, ExprLevel0(True))
      else
        break;
  except
    Result.Free;
    raise;
  end;
end;

function TJpQueryParser.ExprLevel1(aSkip: Boolean): TJpExpression;
begin
  Result := ExprLevel2(aSkip);
  try
    while not(CurrToken in [jtkComma, jtkRBracket]) do
      if CurrToken = jtkBoolAnd then
        Result := TJpAndPredicate.Create(Result, ExprLevel1(True))
      else
        break;
  except
    Result.Free;
    raise;
  end;
end;

function TJpQueryParser.ExprLevel2(aSkip: Boolean): TJpExpression;
begin
  Result := ExprLevel3(aSkip);
  try
    while not(CurrToken in [jtkComma, jtkRBracket]) do
      case CurrToken of
        jtkLess:      Result := TJpLessThanExpr.Create(Result, ExprLevel3(True));
        jtkGreater:   Result := TJpGreaterThanExpr.Create(Result, ExprLevel3(True));
        jtkEqual:     Result := TJpEqualExpr.Create(Result, ExprLevel3(True));
        jtkNotEqual:  Result := TJpNotEqualExpr.Create(Result, ExprLevel3(True));
        jtkLessOrEq:  Result := TJpLessOrEqualExpr.Create(Result, ExprLevel3(True));
        jtkGreatOrEq: Result := TJpGreaterOrEqualExpr.Create(Result, ExprLevel3(True));
      else
        break;
      end;
  except
    Result.Free;
    raise;
  end;
end;

function TJpQueryParser.ExprLevel3(aSkip: Boolean): TJpExpression;
begin
  if aSkip then
    NextToken;
  if CurrToken = jtkBoolNot then
    Result := TJpNotPredicate.Create(ExprLevel3(True))
  else
    Result := ExprLevel4;
end;

function TJpQueryParser.ExprLevel4: TJpExpression;
begin
  Result := nil;
  if CurrToken in LITERALS + [jtkFunction, jtkLParen, jtkRootId, jtkCurrId] then begin
    case CurrToken of
      jtkLParen: begin
          Result := ExprLevel0(True);
          if CurrToken <> jtkRParen then begin
            Result.Free;
            Fail(SEJPathPosErrorFmt, [Position, SEJPathCloseParenMiss]);
          end;
        end;
      jtkRootId,
      jtkCurrId: Result := SubQueryExpr;
      jtkFunction: begin
          Result := FunctionExpr;
          if CurrToken <> jtkRParen then begin
            Result.Free;
            Fail(SEJPathPosErrorFmt, [Position, SEJPathCloseParenMiss]);
          end;
        end;
      jtkNull:   Result := TJpConstExpr.CreateNull;
      jtkFalse:  Result := TJpConstExpr.Create(False);
      jtkTrue:   Result := TJpConstExpr.Create(True);
      jtkString: Result := TJpConstExpr.Create(FStrValue);
      jtkNumber: Result := TJpConstExpr.Create(FNumValue);
    else
    end;
    NextToken;
  end else
    Fail(SEJPathPosErrorFmt, [Position, SEJPathOperandMiss]);
end;

function TJpQueryParser.SubQueryExpr: TJpExpression;
var
  Path: TRelPathQuery;
begin
  if CurrToken = jtkRootId then
    Path := TRelPathQuery.Create(jpiRoot)
  else
    Path := TRelPathQuery.Create(jpiCurrent);
  try
    while CurrChar in ['.', '['] do
      begin
        if CurrChar = '.' then
          DotSegment(Path)
        else
          BracketSegment(Path);
        SkipWhiteSpace;
      end;
     Result := TJpRelQueryExpr.Create(Path);
  except
    Path.Free;
    raise;
  end;
end;

function TJpQueryParser.FunctionExpr: TJpExpression;
var
  ParamList: TJpExprList = nil;
  I: SizeInt;
begin
  Assert(CurrChar = '(');
  SkipChar;
  NextToken;
  System.SetLength(ParamList, System.Length(FFunDef.ParamDefs));
  try
    for I := 0 to System.High(ParamList) do
      begin
        ParamList[I] := ExprLevel0(False);
        if I < System.High(ParamList) then
          if CurrToken <> jtkComma then
            Fail(SEJPathPosErrorFmt, [Position, SEJPathFunParamMiss])
          else
            begin
              SkipChar;
              NextToken;
            end;
      end;
    Result := TJpFunctionExpr.Create(ParamList, FFunDef.OnExecute);
  except
    for I := 0 to System.High(ParamList) do
      ParamList[I].Free;
    raise;
  end;
end;

function TJpQueryParser.GetFilter: TJpFilter;
begin
  Assert(CurrChar = '?');
  SkipChar;
  NextToken;
  Result := TJpFilter.Create(ExprLevel0(False));
end;

{$PUSH}{$WARN 5089 OFF}
procedure TJpQueryParser.BracketSegment(aPath: TJsonPathQuery);
var
  Segment: TSegment;
  oi: TOptionalInt;
  Done: Boolean = False;
begin
  Assert(CurrChar = '[');
  SkipCharThenWS;
  Segment := aPath.AddChildSegment;
  while not Done do begin
    CheckEof;
    case CurrChar of
      '''': Segment.AddSelector(GetQuoteName);
      '"':  Segment.AddSelector(GetDblQuoteName);
      '*':  begin Segment.AddWildcard; SkipCharThenWS; end;
      '-', '0'..'9': IndexOrSlice(Segment);
      ':': Slice(Segment, oi);
      ',':
        if Segment.Count = 0 then
          Fail(SEJPathPosErrorFmt, [Position, SEJPathLeadingComma])
        else begin
          SkipCharThenWS;
          if Eof then
            Fail(SEJPathCloseBracketMiss)
          else
            if CurrChar = ']' then
              Fail(SEJPathPosErrorFmt, [Position, SEJPathTrailingComma]);
        end;
      '?': Segment.AddSelector(GetFilter);
      ']':
        if Segment.Count = 0 then
          Fail(SEJPathPosErrorFmt, [Position, SEJPathEmptySegment])
        else begin
          SkipCharThenWS;
          Done := True;
        end;
    else
      Fail(SEJPathPosUnexpectFmt, [Position, CurrChar]);
    end;
  end;
end;
{$POP}

procedure TJpQueryParser.DoParse(aPath: TJsonPathQuery);
begin
  Assert(CurrChar = '$');
  SkipCharThenWS;
  while not Eof do
    case CurrChar of
      '.': DotSegment(aPath);
      '[': BracketSegment(aPath);
    else
      Fail(SEJPathPosExpectFmt, [Position, SEJPathDotOrLB, CurrChar]);
    end;
end;

constructor TJpQueryParser.Create(const aQuery: string; aPath: TJsonPathQuery);
begin
  FQuery := aQuery;
  FPath := aPath;
  FLook := PAnsiChar(FQuery);
end;

procedure TJpQueryParser.TryParse;
begin
  FPath.Clear;
  // empty query is not provided in the specs
  if FQuery = '' then
    Fail(SEJPathQueryIsEmpty);
  // query must be a well-formed UTF-8 string
  if not lgSeqUtils.Utf8Validate(FQuery) then
    Fail(SEJPathMalformQuery);
  FLast := PAnsiChar(FQuery) + Pred(System.Length(FQuery));
  SkipWhiteSpace;
  //query must start with a root identifier
  if Eof or (CurrChar <> '$') then
    Fail(SEJPathRootMiss);
  //query can not end with a dot
  while FLast^ in WHITE_SPACE do
    Dec(FLast);
  if FLast^ = '.' then
    Fail(SEJPathPosErrorFmt, [Succ(FLast - PAnsiChar(FQuery)), SEJPathCantEndWithDot]);
  DoParse(FPath);
end;

{ TJpMatcher }

constructor TJpMatcher.Create;
begin
  inherited;
  FPath := TJsonSpecPathQuery.Create;
end;

destructor TJpMatcher.Destroy;
begin
  FPath.Free;
  inherited;
end;

function TJpMatcher.TryParseQuery(const aQuery: string): Boolean;
begin
  Result := False;
  with TJpQueryParser.Create(aQuery, FPath) do
    try
      try
        TryParse;
        Result := True;
      except
        on e: EJpParse do
          FMessage := e.Message;
        on e: Exception do ///////////////////
          FMessage := Format(SEJPathInternalParseFmt, [e.Message]);
      end;
    finally
      Free;
    end;
  if not Result then
    FPath.Clear;
end;

function TJpMatcher.Match(aRoot: TJsonNode): TJpNodeList;
begin
  Result := FPath.ApplyWithPath(aRoot);
end;

function TJpMatcher.Match(const aRoot: string; out aNodeList: string): boolean;
var
  Root: specialize TGAutoRef<TJsonNode>;
begin
  aNodeList := '';
  if not Root.Instance.Parse(aRoot) then exit(False);
  aNodeList := FPath.ApplyWithPath(Root.Instance).AsJson;
  Result := True;
end;

function TJpMatcher.MatchValues(aRoot: TJsonNode): TJpValueList;
begin
  Result := FPath.Apply(aRoot);
end;

function TJpMatcher.MatchValues(const aRoot: string; out aNodeArray: string): Boolean;
var
  Root: specialize TGAutoRef<TJsonNode>;
begin
  aNodeArray := '';
  if not Root.Instance.Parse(aRoot) then exit(False);
  aNodeArray := FPath.Apply(Root.Instance).AsJson;
  Result := True;
end;

function TJpMatcher.MatchFirst(aRoot: TJsonNode): TJpNode;
begin
  Result := FPath.GetFirstWithPath(aRoot);
end;

function TJpMatcher.MatchFirst(const aRoot: string; out aNode: string): Boolean;
var
  Root: specialize TGAutoRef<TJsonNode>;
begin
  aNode := '';
  if not Root.Instance.Parse(aRoot) then exit(False);
  aNode := FPath.GetFirstWithPath(Root.Instance).AsJson;
  Result := True;
end;

function TJpMatcher.MatchFirstValue(aRoot: TJsonNode): TJsonNode;
begin
  Result := FPath.GetFirst(aRoot);
end;

function TJpMatcher.MatchFirstValue(const aRoot: string; out aValue: string): Boolean;
var
  Root: specialize TGAutoRef<TJsonNode>;
  Node: TJsonNode;
begin
  aValue := '';
  if not Root.Instance.Parse(aRoot) then exit(False);
  Node := FPath.GetFirst(Root.Instance);
  if Node <> nil then
    aValue := Node.AsJson;
  Result := True;
end;

function JsonPathParse(const aQuery: string; out aMsg: string): IJsonPath;
var
  Matcher: TJpMatcher;
begin
  aMsg := '';
  Matcher := TJpMatcher.Create;
  if Matcher.TryParseQuery(aQuery) then
    Result := Matcher
  else
    begin
      aMsg := Matcher.Message;
      Result := nil;
      Matcher.Free;
    end;
end;

function ParseJsonPath(const aQuery: string; out aPath: IJsonPath): Boolean;
var
  msg: string;
begin
  aPath := JsonPathParse(aQuery, msg);
  Result := aPath <> nil;
end;

function JsonPathMatch(const aQuery: string; aRoot: TJsonNode; out aList: TJpValueList;
  out aMsg: string): Boolean;
var
  Matcher: specialize TGAutoRef<TJpMatcher>;
begin
  aMsg := '';
  aList := nil;
  if Matcher.Instance.TryParseQuery(aQuery) then
    begin
      aList := Matcher.Instance.MatchValues(aRoot);
      exit(True);
    end
  else
    aMsg := Matcher.Instance.Message;
  Result := False;
end;

function JsonPathMatch(const aQuery: string; aRoot: TJsonNode; out aList: TJpValueList): Boolean;
var
  msg: string;
begin
  Result := JsonPathMatch(aQuery, aRoot, aList, msg);
end;

function JsonPathMatchFirst(const aQuery: string; aRoot: TJsonNode; out aNode: TJsonNode;
  out aMsg: string): Boolean;
var
  Matcher: specialize TGAutoRef<TJpMatcher>;
begin
  aMsg := '';
  aNode := nil;
  if Matcher.Instance.TryParseQuery(aQuery) then
    begin
      aNode := Matcher.Instance.MatchFirstValue(aRoot);
      exit(True);
    end
  else
    aMsg := Matcher.Instance.Message;
  Result := False;
end;

function JsonPathMatchFirst(const aQuery: string; aRoot: TJsonNode; out aNode: TJsonNode): Boolean;
var
  msg: string;
begin
  Result := JsonPathMatchFirst(aQuery, aRoot, aNode, msg);
end;

procedure CallCountFun(const aList: TJpParamList; out aResult: TJpInstance);
begin
  if (System.Length(aList) = 1) and (aList[0].InstType = jitNodeList) then
    aResult := System.Length(aList[0].NodeList)
  else
    aResult := 0;
end;

procedure CallLengthFun(const aList: TJpParamList; out aResult: TJpInstance);
begin
  aResult := TJpInstance.Nothing;
  if (System.Length(aList) = 1) and (aList[0].InstType = jitValue) then
    case aList[0].Value.ValType of
      jvtString:
        aResult := Utf8StrLen(aList[0].Value.StrValue);
      jvtNode:
        if aList[0].Value.NodeValue <> nil then
          case aList[0].Value.NodeValue.Kind of
            jvkString:
              aResult := Utf8StrLen(aList[0].Value.NodeValue.AsString);
            jvkArray, jvkObject:
              aResult := aList[0].Value.NodeValue.Count;
          else
          end;
    else
    end;
end;

procedure CallValueFun(const aList: TJpParamList; out aResult: TJpInstance);
begin
  aResult := TJpInstance.Nothing;
  if System.Length(aList) = 1 then
    case aList[0].InstType of
      jitNodeList:
        if System.Length(aList[0].NodeList) = 1 then
          aResult := aList[0].NodeList[0];
      jitValue:
        if aList[0].Value.NodeValue <> nil then
          aResult := aList[0].Value.NodeValue;
    else
    end;
end;

function TryExecRegex(const aInput, aRegex: string; aOptions: TRegexReplaceOptions): Boolean;
begin
  try
    with TRegExpr.Create do
      try
        ModifierI := (rroModifierI in aOptions);
        ModifierR := (rroModifierR in aOptions);
        ModifierS := (rroModifierS in aOptions);
        ModifierG := (rroModifierG in aOptions);
        ModifierM := (rroModifierM in aOptions);
        ModifierX := (rroModifierX in aOptions);
        Expression := aRegex;
        Result := Exec(aInput);
      finally
        Free;
      end;
  except
    Result := False;
  end;
end;

function FindMatchParams(const aList: TJpParamList; out aInput, aRegex: string): Boolean;
begin
  if System.Length(aList) = 2 then
    begin
      with aList[0] do
        if InstType = jitValue then
          case Value.ValType of
            jvtString: aInput := Value.StrValue;
            jvtNode:
              if (Value.NodeValue <> nil) and (Value.NodeValue.IsString) then
                aInput := Value.NodeValue.AsString
              else
                exit(False)
          else
            exit(False);
          end
        else
          exit(False);
      with aList[1] do
        if InstType = jitValue then
          case Value.ValType of
            jvtString: aRegex := Value.StrValue;
            jvtNode:
              if (Value.NodeValue <> nil) and (Value.NodeValue.IsString) then
                aRegex := Value.NodeValue.AsString
              else
                exit(False)
          else
            exit(False);
          end
        else
          exit(False);
      Result := True;
    end
  else
    Result := (False);
end;

procedure CallMatchFun(const aList: TJpParamList; out aResult: TJpInstance);
var
  Input, Regex: string;
  b, e: Boolean;
begin
  aResult := False;
  if not FindMatchParams(aList, Input, Regex) then exit;
  if Regex = '' then
    Regex := '^$'
  else
    begin
      b := Regex[1] <> '^';
      e := Regex[System.Length(Regex)] <> '$';
      if b and e then
        Regex := '^' + Regex + '$'
      else
        if b then
          Regex := '^' + Regex
        else
          Regex := Regex + '$';
    end;
  aResult := TryExecRegex(Input, Regex, [rroModifierS]);
end;

procedure CallSearchFun(const aList: TJpParamList; out aResult: TJpInstance);
var
  Input, Regex: string;
begin
  aResult := False;
  if not FindMatchParams(aList, Input, Regex) then exit;
  aResult := TryExecRegex(Input, Regex, [rroModifierS]);
end;

type

  TFunCacheType = specialize TGLiteChainHashMap<string, TJpFunctionDef, string>;
  TFunCache     = TFunCacheType.TMap;

var
  GlobLock: TRtlCriticalSection;
  FunCache: TFunCache;

function AddFunDef(const aName: string; const aFunDef: TJpFunctionDef): Boolean;
begin
  EnterCriticalSection(GlobLock);
  try
    Result := FunCache.Add(aName, aFunDef);
  finally
    LeaveCriticalSection(GlobLock);
  end;
end;

function FindFunctionDef(const aName: string; out aFunDef: TJpFunctionDef): Boolean;
begin
  EnterCriticalSection(GlobLock);
  try
    Result := FunCache.TryGetValue(aName, aFunDef);
  finally
    LeaveCriticalSection(GlobLock);
  end;
end;

function ValidFunName(const aName: string): Boolean;
var
  I: SizeInt;
begin
  if aName = '' then
    exit(False)
  else
    if not(aName[1] in ['a'..'z']) then
      exit(False);
  for I := 2 to System.Length(aName) do
    if not(aName[I] in ['a'..'z', '_', '0'..'9']) then
      exit(False);
  Result := True;
end;

function JpRegisterFunction(const aName: string; const aFunDef: TJpFunctionDef): Boolean;
begin
  if ValidFunName(aName) and (aFunDef.OnExecute <> nil) then
    Result := AddFunDef(aName, aFunDef)
  else
    Result := False;
end;

procedure RegisterBuiltIns;
begin
  FunCache.Add('count', TJpFunctionDef.Make([jitNodeList], jitValue, @CallCountFun));
  FunCache.Add('length', TJpFunctionDef.Make([jitValue], jitValue, @CallLengthFun));
  FunCache.Add('value', TJpFunctionDef.Make([jitNodeList], jitValue, @CallValueFun));
  FunCache.Add('match', TJpFunctionDef.Make([jitValue, jitValue], jitLogical, @CallMatchFun));
  FunCache.Add('search', TJpFunctionDef.Make([jitValue, jitValue], jitLogical, @CallSearchFun));
end;

initialization
  RegisterBuiltIns;
  InitCriticalSection(GlobLock);
finalization
  DoneCriticalSection(GlobLock);
end.
