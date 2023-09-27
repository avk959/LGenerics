{****************************************************************************
*                                                                           *
*   This file is part of the LGenerics package.                             *
*   JSONPath implementation: query expressions for JSON.                    *
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

  Operators supported in the filter expressions in descending order of precedence:

    5   - grouping   ( ... )
    4   - logical NOT   !
    3   - relations   ==, !=, <, >, <=, >=
    2   - logical AND  &&
    1   - logical OR   ||

  Built-in functions:

    length(Arg) -
      Arg type must be ValueType(jitValue in this implementation);
      result type is ValueType;
        if Arg is a string, the result is the Utf8 length of that string;
        if Arg is an Array, the result is the number of elements in the Array;
        if Arg is an Object, the result is the number of pairs in the Object;
        for any other argument value, the result is jvtNothing.

    count(Arg) -
      Arg type must be Nodes(jitNodeList in this implementation);
      result type is ValueType;
        the result is the number of nodes in the Nodelist;

    match(Input, Regex) -
      Input type must be ValueType(string, to be more precise);
      Regex type must be ValueType(string);
      result type is LogicalType(jitLogical in this implementation);
        returns a boolean True if Input is a string, and Regex is a string,
        and the regular expression Regex matches the entire Input value,
        otherwise returns boolean False;
        it is also possible to activate the I-Regexp checker;

    search(Input, Regex) -
      Input type must be ValueType(string);
      Regex type must be ValueType(string);
      result type is LogicalType;
        returns a boolean True if Input is a string, and Regex is a string,
        and the regular expression Regex matches some substring of the Input value,
        otherwise returns boolean False;

    value(Arg) -
      Arg type must be Nodes;
      if the argument contains a single node, the result is the value of the node;
      If the argument is empty or contains multiple nodes, the result is jvtNothing;

  The implementation tries to be compliant with
    https://datatracker.ietf.org/doc/draft-ietf-jsonpath-base
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
  { returns the first matching element if any, otherwise returns NIL }
    function  MatchFirstValue(aRoot: TJsonNode): TJsonNode;
  { returns True and first matching element as JSON (an empty string if no matches)
    if aRoot is valid JSON, False otherwise }
    function  MatchFirstValue(const aRoot: string; out aValue: string): Boolean;
  end;
{$POP}

{ parses a JSONPath aQuery expression, which must be a well-formed UTF-8 string starting
  with the root identifier, leading and trailing spaces are not allowed;
  returns nil if aQuery is invalid, in which case the aMsg parameter contains an error message }
  function JpParseQuery(const aQuery: string; out aMsg: string): IJsonPath;

{ returns True and IJsonPath in aPath if aQuery was successfully parsed, otherwise returns False }
  function JpParseQuery(const aQuery: string; out aPath: IJsonPath): Boolean;


{ tries to apply aQuery to JSON value aRoot; returns False if aQuery is invalid, in which case
  the aMsg parameter contains an error message, otherwise returns the NodeList in the aList parameter }
  function JpMatch(const aQuery: string; aRoot: TJsonNode; out aList: TJpNodeList; out aMsg: string): Boolean;
{ same as above, but for the case where the content of the error message is of no interest }
  function JpMatch(const aQuery: string; aRoot: TJsonNode; out aList: TJpNodeList): Boolean;
{ tries to apply aQuery to JSON value aRoot; returns False if aRoot is invalid JSON
  or aQuery is invalid, in which case the aMsg parameter contains an error message,
  otherwise returns the NodeList as JSON in the aList parameter }
  function JpMatch(const aQuery, aRoot: string; out aList, aMsg: string): Boolean;
{ same as above, but for the case where the content of the error message is of no interest }
  function JpMatch(const aQuery, aRoot: string; out aList: string): Boolean;
{ tries to apply aQuery to the JSON value aRoot; returns False if aQuery is invalid, in which case,
  the aMsg parameter contains an error message }
  function JpMatchValues(const aQuery: string; aRoot: TJsonNode; out aList: TJpValueList; out aMsg: string): Boolean;
{ same as above, but for the case where the content of the error message is of no interest }
  function JpMatchValues(const aQuery: string; aRoot: TJsonNode; out aList: TJpValueList): Boolean;
{ tries to apply aQuery to the JSON value aRoot; returns False if aRoot is invalid JSON or
  aQuery is invalid, in which case, the aMsg parameter contains an error message }
  function JpMatchValues(const aQuery, aRoot: string; out aList, aMsg: string): Boolean;
{ same as above, but for the case where the content of the error message is of no interest }
  function JpMatchValues(const aQuery, aRoot: string; out aList: string): Boolean;
{ tries to apply aQuery to the JSON value aRoot and returns only the first match;
  returns False if aQuery is invalid, in which case the aMsg parameter contains an error message }
  function JpMatchFirst(const aQuery: string; aRoot: TJsonNode; out aNode: TJpNode; out aMsg: string): Boolean;
{ same as above, but for the case where the content of the error message is of no interest }
  function JpMatchFirst(const aQuery: string; aRoot: TJsonNode; out aNode: TJpNode): Boolean;
{ tries to apply aQuery to the JSON value aRoot and returns only the first match; returns False if aRoot
  is invalid JSON or aQuery is invalid, in which case the aMsg parameter contains an error message }
  function JpMatchFirst(const aQuery, aRoot: string; out aNode, aMsg: string): Boolean;
{ same as above, but for the case where the content of the error message is of no interest }
  function JpMatchFirst(const aQuery, aRoot: string; out aNode: string): Boolean;
{ tries to apply aQuery to the JSON value aRoot and returns only the first match;
  returns False if aQuery is invalid, in which case the aMsg parameter contains an error message }
  function JpMatchFirstValue(const aQuery: string; aRoot: TJsonNode; out aNode: TJsonNode; out aMsg: string): Boolean;
{ same as above, but for the case where the content of the error message is of no interest }
  function JpMatchFirstValue(const aQuery: string; aRoot: TJsonNode; out aNode: TJsonNode): Boolean;
{ tries to apply aQuery to the JSON value aRoot and returns only the first match; returns False if aRoot
  is invalid JSON or aQuery is invalid, in which case the aMsg parameter contains an error message }
  function JpMatchFirstValue(const aQuery, aRoot: string; out aNode, aMsg: string): Boolean;
{ same as above, but for the case where the content of the error message is of no interest }
  function JpMatchFirstValue(const aQuery, aRoot: string; out aNode: string): Boolean;


type
  TJpValueType = (jvtNothing, jvtNull, jvtFalse, jvtTrue, jvtString, jvtNumber, jvtNode);

  { TJpValue }
  TJpValue = record
    function AsBoolean: Boolean; inline;
    class function Nothing: TJpValue; static; inline;
    class function ValNull: TJpValue; static; inline;
    class function ValFalse: TJpValue; static; inline;
    class function ValTrue: TJpValue; static; inline;
    class operator := (b: Boolean): TJpValue; inline;
    class operator := (const s: string): TJpValue; inline;
    class operator := (d: Double): TJpValue; inline;
    class operator := (aNode: TJsonNode): TJpValue; inline;
    class operator  = (const L: TJpValue; R: TJsonNode): Boolean;
    class operator  = (const L, R: TJpValue): Boolean;
    class operator  < (const L: TJpValue; R: TJsonNode): Boolean;
    class operator  < (L: TJsonNode; const R: TJpValue): Boolean;
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

  TJpInstanceType = (jitLogical, jitValue, jitNodeList);

  { TJpInstance }
  TJpInstance = record
    function AsBoolean: Boolean; inline;
    class operator := (const s: string): TJpInstance; inline;
    class operator := (d: Double): TJpInstance; inline;
    class operator := (const v: TJpValue): TJpInstance; inline;
    class operator := (const aList: TJpValueList): TJpInstance; inline;
    class operator := (aNode: TJsonNode): TJpInstance; inline;
    class operator := (b: Boolean): TJpInstance; inline;
    class operator  = (const L, R: TJpInstance): Boolean; inline;
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

{ allows to register custom function; the function name must contain only lowercase letters
  from the range a-z, decimal numbers, underscores, and must begin with a letter }
  function JpRegisterFunction(const aName: string; const aFunDef: TJpFunctionDef): Boolean;


{ to enable an external regular expression engine, if any of the global
  variables listed below is assigned, the corresponding function will be used
  instead of the default function }
type
  TJpRegexMatch = function(const aInput, aRegex: string): Boolean;

var
{ must return True if the regular expression(second argument) matches the entire input string }
  JpRegexMatch: TJpRegexMatch = nil;
{ must return True if the regular expression matches some substring of the input string }
  JpRegexSearch: TJpRegexMatch = nil;

{ I-Regexp checker: https://datatracker.ietf.org/doc/draft-ietf-jsonpath-iregexp }
type

  TIRegexpCheck = (
     ircOk,               { it seems OK }
     ircInvalidQuantifier,{ invalid quantifier }
     ircMissingRBrace,    { missing closing brace }
     ircMissingRParen,    { missing closing parenthesis }
     ircMissingRBracket,  { missing closing bracket }
     ircUnexpectedEnd,    { unexpected end of expression }
     ircUnexpectMetachar, { expected normal char, but got metachar }
     ircInvalidCCChar,    { expected CC_CHAR or SingleCharEsc }
     ircInescapable,      { expected SingleCharEsc }
     ircInvalidUCategory, { invalid Unicode category }
     ircMaxDepthExceed,   { maximum recursion depth exceeded }
     ircExtraChars        { not at end of regexp after parsing last branch }
     );

  function IRegexpCheck(const aRegex: string; out aErrPos: SizeInt): TIRegexpCheck;

{ if uncomment define, the regexes in the arguments of built-in functions
  will be checked for I-Regexp conformance }
{ $DEFINE JP_IREGEXP_CHECK}

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

function TJpValue.AsBoolean: Boolean;
begin
  case ValType of
    jvtTrue:  Result := True;
    jvtNode: Result := NodeValue.Kind = jvkTrue;
  else
    Result := False;
  end;
end;

class function TJpValue.Nothing: TJpValue;
begin
  Result.ValType := jvtNothing;
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
  if aNode <> nil then
    begin
      Result.ValType := jvtNode;
      Result.NodeValue := aNode;
    end
  else
    Result.ValType := jvtNothing;
end;

class operator TJpValue.=(const L: TJpValue; R: TJsonNode): Boolean;
begin
  if R = nil then exit(L.ValType = jvtNothing);
  case L.ValType of
    jvtNothing: Result := False;
    jvtNull:    Result := R.IsNull;
    jvtFalse:   Result := R.IsFalse;
    jvtTrue:    Result := R.IsTrue;
    jvtString:  Result := R.IsString and (L.StrValue = R.AsString);
    jvtNumber:  Result := R.IsNumber and (L.NumValue = R.AsNumber);
    jvtNode:    Result := L.NodeValue.EqualTo(R);
  end;
end;

class operator TJpValue.=(const L, R: TJpValue): Boolean;
begin
  if L.ValType = R.ValType then
    case L.ValType of
      jvtNothing, jvtNull, jvtFalse, jvtTrue: Result := True;
      jvtString: Result := L.StrValue = R.StrValue;
      jvtNumber: Result := L.NumValue = R.NumValue;
      jvtNode:   Result := L.NodeValue.EqualTo(R.NodeValue);
    end
  else
    if L.ValType = jvtNode then
      Result := R = L.NodeValue
    else
      if R.ValType= jvtNode then
        Result := L = R.NodeValue
      else
        Result := False;
end;

class operator TJpValue.<(const L: TJpValue; R: TJsonNode): Boolean;
begin
  Result := False;
  if R = nil then exit;
  case L.ValType of
     jvtString: Result := R.IsString and (AnsiCompareStr(L.StrValue, R.AsString) < 0);
     jvtNumber: Result := R.IsNumber and (L.NumValue < R.AsNumber);
     jvtNode:
       case L.NodeValue.Kind of
         jvkString: Result := R.IsString and (AnsiCompareStr(L.NodeValue.AsString, R.AsString) < 0);
         jvkNumber: Result := R.IsNumber and (L.NodeValue.AsNumber < R.AsNumber);
       else
       end;
  else
  end;
end;

class operator TJpValue.<(L: TJsonNode; const R: TJpValue): Boolean;
begin
  Result := False;
  if L = nil then exit;
  case R.ValType of
     jvtString: Result := L.IsString and (AnsiCompareStr(L.AsString, R.StrValue) < 0);
     jvtNumber: Result := L.IsNumber and (L.AsNumber < R.NumValue);
     jvtNode:
       if R.NodeValue <> nil then
         case R.NodeValue.Kind of
           jvkString: Result := L.IsString and (AnsiCompareStr(L.AsString, R.NodeValue.AsString) < 0);
           jvkNumber: Result := L.IsNumber and (L.AsNumber < R.NodeValue.AsNumber);
         else
         end;
  else
  end;
end;

class operator TJpValue.<(const L, R: TJpValue): Boolean;
begin
  Result := False;
  if L.ValType = R.ValType then
    case L.ValType of
      jvtString: Result := AnsiCompareStr(L.StrValue, R.StrValue) < 0;
      jvtNumber: Result := L.NumValue < R.NumValue;
      jvtNode:   Result := L < R.NodeValue;
    else
    end
  else
    if L.ValType = jvtNode then
      Result := L.NodeValue < R
    else
      if R.ValType = jvtNode then
        Result := L < R.NodeValue;
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

class operator TJpInstance.=(const L, R: TJpInstance): Boolean;
begin
  Result := False;
  if L.InstType = R.InstType then
    case L.InstType of
      jitValue:   Result := L.Value = R.Value;
      jitLogical: Result := False;
      jitNodeList:
        if L.NodeList = nil then
          Result := R.NodeList = nil;
    end;
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
    FApplyKind: TApplyKind;
    FDone: Boolean;
    procedure AddMatch(aNode: TJsonNode); inline;
    procedure ApplySlice(aSel: TSelector; aNode: TJsonNode);
    procedure ApplySelector(aSel: TSelector; aRoot: TJsonNode; aDesc: Boolean = False);
    procedure ApplySegment(aSegment: TSegment; aRoot: TJsonNode);
    property  Done: Boolean read FDone;
    property  ApplyKind: TApplyKind read FApplyKind;
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
  TJsonSpecPathQuery = class(TJsonPathQuery)
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
    function GetValue(const aCtx: TJpFilterContext): TJpInstance; virtual;
    function Apply(const aCtx: TJpFilterContext): Boolean; virtual;
  end;

  { TJpConstExpr }
  TJpConstExpr = class(TJpExpression)
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
    function GetFirstNode(const aCtx: TJpFilterContext): TJsonNode; inline;
    function GetNodeList(const aCtx: TJpFilterContext): TJpValueList; inline;
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

  TJpLogicExpr = class(TJpBinaryExpr);

  { TJpAndExpr }
  TJpAndExpr = class(TJpLogicExpr)
  strict protected
    procedure Eval(const aCtx: TJpFilterContext; var v: TJpInstance); override;
  end;

  { TJpOrExpr }
  TJpOrExpr = class(TJpLogicExpr)
  strict protected
    procedure Eval(const aCtx: TJpFilterContext; var v: TJpInstance); override;
  end;

  { TJpRelQueryExpr }
  TJpRelQueryExpr = class(TJpExpression)
  strict private
    FPath: TRelPathQuery;
    FSingular: Boolean;
  strict protected
    procedure Eval(const aCtx: TJpFilterContext; var v: TJpInstance); override;
  public
    constructor Create(aPath: TRelPathQuery);
    destructor Destroy; override;
    function Apply(const aCtx: TJpFilterContext): Boolean; override;
    property IsSingular: Boolean read FSingular;
  end;

  TJpComparisonExpr = class(TJpBinaryExpr);

  { TJpEqualExpr }
  TJpEqualExpr = class(TJpComparisonExpr)
  strict protected
    procedure Eval(const aCtx: TJpFilterContext; var v: TJpInstance); override;
  end;

  { TJpNotEqualExpr }
  TJpNotEqualExpr = class(TJpComparisonExpr)
  strict protected
    procedure Eval(const aCtx: TJpFilterContext; var v: TJpInstance); override;
  end;

  { TJpLessThanExpr }
  TJpLessThanExpr = class(TJpComparisonExpr)
  strict protected
    procedure Eval(const aCtx: TJpFilterContext; var v: TJpInstance); override;
  end;

  { TJpLessOrEqualExpr }
  TJpLessOrEqualExpr = class(TJpComparisonExpr)
  strict protected
    procedure Eval(const aCtx: TJpFilterContext; var v: TJpInstance); override;
  end;

  { TJpGreaterThanExpr }
  TJpGreaterThanExpr = class(TJpComparisonExpr)
  strict protected
    procedure Eval(const aCtx: TJpFilterContext; var v: TJpInstance); override;
  end;

  { TJpGreaterOrEqualExpr }
  TJpGreaterOrEqualExpr = class(TJpComparisonExpr)
  strict protected
    procedure Eval(const aCtx: TJpFilterContext; var v: TJpInstance); override;
  end;

  TJpExprList = array of TJpExpression;

  { TJpFunctionExpr }
  TJpFunctionExpr = class(TJpExpression)
  strict protected
    FParamList: TJpExprList;
    FArgumentList: TJpParamList;
    FFunCall: TJpFunctionCall;
    FResultType: TJpInstanceType;
    procedure Eval(const aCtx: TJpFilterContext; var v: TJpInstance); override;
  public
    constructor Create(const aParams: TJpExprList; aFun: TJpFunctionCall; aResult: TJpInstanceType);
    destructor Destroy; override;
    property ResultType: TJpInstanceType read FResultType;
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
    COMPARISONS    = [jtkLess, jtkGreater, jtkEqual, jtkNotEqual, jtkLessOrEq,  jtkGreatOrEq];
    DEC_DIGITS     = ['0'..'9'];
    MINUS_OR_DIGIT = DEC_DIGITS + ['-'];
    WHITE_SPACE    = [#9, #10, #13, ' '];
    NAME_FIRST     = ['A'..'Z', 'a'..'z', '_', #$80..#$ff];
    NAME_CHAR      = NAME_FIRST + ['0'..'9'];
    HEX_DIGITS     = ['0'..'9', 'A', 'B', 'C', 'D', 'E', 'F', 'a', 'b', 'c', 'd', 'e', 'f'];
    FUN_NAME_FIRST = ['a'..'z'];
    FUN_NAME_CHAR  = FUN_NAME_FIRST + ['0'..'9', '_'];
    MAX_DEPTH      = 512;
  var
    FQuery: string;
    FPath: TJsonPathQuery;
    FLook,
    FLast: PAnsiChar;
    FDepth: SizeInt;
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
    procedure CheckBoolOperand(aExpr: TJpExpression); inline;
    function  ExprLevel0(aSkip: Boolean): TJpExpression;
    function  ExprLevel1(aSkip: Boolean): TJpExpression;
    procedure CheckCompOperand(aExpr: TJpExpression); inline;
    function  ExprLevel2(aSkip: Boolean): TJpExpression;
    function  ExprLevel3(aSkip: Boolean): TJpExpression;
    function  ExprLevel4: TJpExpression;
    function  SubQueryExpr: TJpExpression;
    procedure CheckFunParam(const aFunName: string; aType: TJpInstanceType; aExpr: TJpExpression);
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
  case ApplyKind of
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
      if aRoot.Kind = jvkArray then begin
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
      if aRoot.Find(aSel.Name, LValue) then
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
  if FSegments.IsEmpty then exit(True);
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
  FApplyKind := akCount;
  FDone := False;
  ApplySegment(FSegments[0], aRoot);
  Result := FCount;
end;

function TJsonPathQuery.Apply(aRoot: TJsonNode): TJpValueList;
begin
  FMatchCollector.MakeEmpty;
  if FSegments.IsEmpty then exit([aRoot]);
  FRoot := aRoot;
  FApplyKind := akMatchList;
  FDone := False;
  ApplySegment(FSegments[0], aRoot);
  Result := FMatchCollector.ToArray;
end;

function TJsonPathQuery.GetFirst(aRoot: TJsonNode): TJsonNode;
begin
  if FSegments.IsEmpty then exit(aRoot);
  FRoot := aRoot;
  FApplyKind := akFirstMatch;
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
  FDone := ApplyKind <> akMatchList;
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
      if aRoot.Kind = jvkArray then begin
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
      if aRoot.Find(aSel.Name, LValue) then begin
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
  FApplyKind := akMatchList;
  FDone := False;
  ApplySegmentWithPath(FSegments[0], aRoot);
  Result := FMatchPathCollector.ToArray;
end;

function TJsonSpecPathQuery.GetFirstWithPath(aRoot: TJsonNode): TJpNode;
begin
  FMatchPathCollector.MakeEmpty;
  if FSegments.IsEmpty then exit(TJpNode.Make('$', aRoot));
  FRoot := aRoot;
  FApplyKind := akFirstMatch;
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

function TRelPathQuery.GetFirstNode(const aCtx: TJpFilterContext): TJsonNode;
begin
  if FIdent = jpiRoot then
    Result := GetFirst(aCtx.Root)
  else
    Result := GetFirst(aCtx.Current);
end;

function TRelPathQuery.GetNodeList(const aCtx: TJpFilterContext): TJpValueList;
begin
  if FIdent = jpiRoot then
    Result := Apply(aCtx.Root)
  else
    Result := Apply(aCtx.Current);
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

procedure TJpAndExpr.Eval(const aCtx: TJpFilterContext; var v: TJpInstance);
begin
  if not Left.Apply(aCtx) then
    v := False
  else
    v := Right.Apply(aCtx);
end;

{ TOrPredicate }

procedure TJpOrExpr.Eval(const aCtx: TJpFilterContext; var v: TJpInstance);
begin
  if Left.Apply(aCtx) then
    v := True
  else
    v := Right.Apply(aCtx);
end;

{ TRelQueryExpr }

procedure TJpRelQueryExpr.Eval(const aCtx: TJpFilterContext; var v: TJpInstance);
begin
  if IsSingular then
    v := FPath.GetFirstNode(aCtx)
  else
    v := FPath.GetNodeList(aCtx);
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
  Eval(aCtx, FValue);
  if IsSingular then
    Result := FValue.Value.ValType <> jvtNothing
  else
    Result := FValue.NodeList <> nil;
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
  I: SizeInt;
begin
  for I := 0 to System.High(FParamList) do
    FArgumentList[I] := FParamList[I].GetValue(aCtx);
  FFunCall(FArgumentList, v);
end;

constructor TJpFunctionExpr.Create(const aParams: TJpExprList; aFun: TJpFunctionCall; aResult: TJpInstanceType);
begin
  FParamList := aParams;
  System.SetLength(FArgumentList, System.Length(FParamList));
  FFunCall := aFun;
  FResultType := aResult;
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
  FContext.Current := aNode;
  Result := FExpr.Apply(FContext);
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
  if (Len = MAX_DIGITS) and (v < TEST_BOUND) then exit(iprRange);
  if (IsNeg and (v > SizeUInt(System.High(SizeInt))+1)) or (v > System.High(SizeInt)) then
    exit(iprRange);
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
    begin
      FToken := jtkFunction;
      FStrValue := s;
    end
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
        end
      else
        if CurrChar <> '[' then
          Fail(SEJPathPosUnexpectFmt, [Position, CurrChar]);
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
    else
      Fail(SEJPathPosUnexpectFmt, [Position, CurrChar]);
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
  repeat
    case CurrChar of
      #0..#31:
        Fail(SEJPathPosErrorFmt, [Position, Format(SEJPathInvalidStrCharFmt, [Ord(CurrChar)])]);
      '''':
        break;
      '\':
        begin
          SkipChar;
          QuoteNameEscape;
        end;
    else
      SkipChar;
    end;
  until Eof;
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
  repeat
    case CurrChar of
      #0..#31:
        Fail(SEJPathPosErrorFmt, [Position, Format(SEJPathInvalidStrCharFmt, [Ord(CurrChar)])]);
      '"':
        break;
      '\':
        begin
          SkipChar;
          DblQuoteNameEscape;
        end;
    else
      SkipChar;
    end;
  until Eof;
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
    CheckEof;
    if CurrChar in MINUS_OR_DIGIT then
      begin
        ParseInt(Value);
        s.Step := Value;
      end;
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
      end;
    ':':  // second colon
      ParseStep;
  else
  end;
  SkipWhiteSpace;
  CheckEof;
  if not(CurrChar in [',', ']']) then
    Fail(SEJPathPosExpectFmt, [Position, SEJPathCommaOrRB]);
  aSegment.AddSelector(s);
end;
{$POP}

procedure TJpQueryParser.CheckBoolOperand(aExpr: TJpExpression);
begin
  if aExpr is TJpConstExpr then
    Fail(SEJPathUnexpectLogicFmt, [Position, SEJPathLiteral])
  else
    if (aExpr is TJpFunctionExpr) and (TJpFunctionExpr(aExpr).ResultType <> jitLogical) then
      Fail(SEJPathUnexpectLogicFmt, [Position, SEJPathNonLogicFun]);
end;

function TJpQueryParser.ExprLevel0(aSkip: Boolean): TJpExpression;
begin
  if FDepth = MAX_DEPTH then Fail(SEJPathMaxDepthExceed);
  Inc(FDepth);
  Result := ExprLevel1(aSkip);
  try
    if CurrToken = jtkBoolOr then
      begin
        CheckBoolOperand(Result);
        repeat
          Result := TJpOrExpr.Create(Result, ExprLevel0(True));
          CheckBoolOperand(TJpBinaryExpr(Result).Right);
        until CurrToken <> jtkBoolOr;
      end;
  except
    Result.Free;
    raise;
  end;
  Dec(FDepth);
end;

function TJpQueryParser.ExprLevel1(aSkip: Boolean): TJpExpression;
begin
  if FDepth = MAX_DEPTH then Fail(SEJPathMaxDepthExceed);
  Inc(FDepth);
  Result := ExprLevel2(aSkip);
  try
    if CurrToken = jtkBoolAnd then
      begin
        CheckBoolOperand(Result);
        repeat
          Result := TJpAndExpr.Create(Result, ExprLevel1(True));
          CheckBoolOperand(TJpBinaryExpr(Result).Right);
        until CurrToken <> jtkBoolAnd;
      end;
  except
    Result.Free;
    raise;
  end;
  Dec(FDepth);
end;

procedure TJpQueryParser.CheckCompOperand(aExpr: TJpExpression);
begin
  if (aExpr is TJpRelQueryExpr) and not TJpRelQueryExpr(aExpr).IsSingular then
    Fail(SEJPathUnexpectCompFmt, [Position, SEJPathNonSingularQuery])
  else
    if (aExpr is TJpFunctionExpr) and (TJpFunctionExpr(aExpr).ResultType = jitLogical) then
      Fail(SEJPathUnexpectCompFmt, [Position, SEJPathLogicFun])
    else
      if aExpr is TJpComparisonExpr then
        Fail(SEJPathUnexpectCompFmt, [Position, SEJPathComparison])
      else
        if (aExpr is TJpLogicExpr) or (aExpr is TJpNotPredicate) then
          Fail(SEJPathUnexpectCompFmt, [Position, SEJPathLogicExpr]);
end;

function TJpQueryParser.ExprLevel2(aSkip: Boolean): TJpExpression;
begin
  if FDepth = MAX_DEPTH then Fail(SEJPathMaxDepthExceed);
  Inc(FDepth);
  Result := ExprLevel3(aSkip);
  try
    if CurrToken in COMPARISONS then
      begin
        CheckCompOperand(Result);
        case CurrToken of
          jtkLess:      Result := TJpLessThanExpr.Create(Result, ExprLevel3(True));
          jtkGreater:   Result := TJpGreaterThanExpr.Create(Result, ExprLevel3(True));
          jtkEqual:     Result := TJpEqualExpr.Create(Result, ExprLevel3(True));
          jtkNotEqual:  Result := TJpNotEqualExpr.Create(Result, ExprLevel3(True));
          jtkLessOrEq:  Result := TJpLessOrEqualExpr.Create(Result, ExprLevel3(True));
          jtkGreatOrEq: Result := TJpGreaterOrEqualExpr.Create(Result, ExprLevel3(True));
        else
        end;
        CheckCompOperand(TJpBinaryExpr(Result).Right);
      end;
  except
    Result.Free;
    raise;
  end;
  Dec(FDepth);
end;

function TJpQueryParser.ExprLevel3(aSkip: Boolean): TJpExpression;
begin
  if FDepth = MAX_DEPTH then Fail(SEJPathMaxDepthExceed);
  Inc(FDepth);
  if aSkip then
    NextToken;
  if CurrToken = jtkBoolNot then
    begin
      Result := ExprLevel3(True);
      try
        CheckBoolOperand(Result);
        Result := TJpNotPredicate.Create(Result);
      except
        Result.Free;
        raise;
      end;
    end
  else
    Result := ExprLevel4;
  Dec(FDepth);
end;

function TJpQueryParser.ExprLevel4: TJpExpression;
begin
  if FDepth = MAX_DEPTH then Fail(SEJPathMaxDepthExceed);
  Inc(FDepth);
  Result := nil;
  try
    if CurrToken in LITERALS + [jtkFunction, jtkLParen, jtkRootId, jtkCurrId] then begin
      case CurrToken of
        jtkLParen: begin
            Result := ExprLevel0(True);
            if CurrToken <> jtkRParen then
              Fail(SEJPathPosErrorFmt, [Position, SEJPathCloseParenMiss]);
          end;
        jtkRootId,
        jtkCurrId: Result := SubQueryExpr;
        jtkFunction: begin
            Result := FunctionExpr;
            if CurrToken <> jtkRParen then
              Fail(SEJPathPosErrorFmt, [Position, SEJPathCloseParenMiss]);
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
      Fail(SEJPathPosErrorFmt, [Position, SEJPathOperandMiss]); // ???
  except
    Result.Free;
    raise;
  end;
  Dec(FDepth);
end;

function TJpQueryParser.SubQueryExpr: TJpExpression;
var
  Path: TRelPathQuery;
begin
  if FDepth = MAX_DEPTH then Fail(SEJPathMaxDepthExceed);
  Inc(FDepth);
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
  Dec(FDepth);
end;

procedure TJpQueryParser.CheckFunParam(const aFunName: string; aType: TJpInstanceType; aExpr: TJpExpression);
var
  WellTyped: Boolean;
begin
  case aType of
    jitLogical:
      if aExpr is TJpFunctionExpr then
        WellTyped := TJpFunctionExpr(aExpr).ResultType = jitLogical
      else
        WellTyped := not(aExpr is TJpConstExpr);
    jitValue:
      WellTyped := ((aExpr is TJpRelQueryExpr) and TJpRelQueryExpr(aExpr).IsSingular) or
                   (aExpr is TJpConstExpr);
    jitNodeList:
      WellTyped := aExpr is TJpRelQueryExpr;
  end;
  if not WellTyped then
    Fail(SEJPathParamMismatchFmt, [Position, aFunName]);
end;

function TJpQueryParser.FunctionExpr: TJpExpression;
var
  FunDef: TJpFunctionDef;
  FunName: string;
  ParamList: TJpExprList = nil;
  e: TJpExpression;
  I: SizeInt;
begin
  Assert(CurrChar = '(');
  if FDepth = MAX_DEPTH then Fail(SEJPathMaxDepthExceed);
  Inc(FDepth);
  SkipChar;
  NextToken;
  FunDef := FFunDef;
  FunName := FStrValue;
  System.SetLength(ParamList, System.Length(FunDef.ParamDefs));
  I := 0;
  try
    while CurrToken <> jtkRParen do begin
      e := ExprLevel0(False);
      ParamList[I] := e;
      if I > System.High(FunDef.ParamDefs) then
        Fail(SEJPathPosErrorFmt, [Position, Format(SEJPathFunParamExtraFmt, [FunName])]);
      CheckFunParam(FunName, FunDef.ParamDefs[I], e);
      if I < System.High(ParamList) then
        if CurrToken <> jtkComma then
          Fail(SEJPathPosUnexpectFmt, [Position, CurrChar])
        else begin
          SkipChar;
          NextToken;
        end;
      Inc(I);
    end;
    if I < System.Length(ParamList) then
      Fail(SEJPathPosErrorFmt, [Position, Format(SEJPathFunParamMissFmt, [FunName])]);
    Result := TJpFunctionExpr.Create(ParamList, FunDef.OnExecute, FunDef.ResultType);
  except
    for e in ParamList do
      e.Free;
    raise;
  end;
  Dec(FDepth);
end;

function TJpQueryParser.GetFilter: TJpFilter;
var
  e: TJpExpression;
begin
  Assert(CurrChar = '?');
  SkipChar;
  NextToken;
  e := ExprLevel0(False);
  if not(CurrToken in [jtkComma, jtkRBracket]) then
    begin
      e.Free;
      Fail(SEJPathPosErrorFmt, [Position, SEJPathTrailFiltGarbage]);
    end;
  if (e is TJpFunctionExpr) and (TJpFunctionExpr(e).ResultType <> jitLogical) or (e is TJpConstExpr) then
    begin
      e.Free;
      Fail(SEJPathPosErrorFmt, [Position, SEJPathLogicExprExpect]);
    end;
  Result := TJpFilter.Create(e);
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
  //leading spaces are not allowed
  if CurrChar in WHITE_SPACE then
    Fail(SEJLeadWSNotAllow);
  //query must start with a root identifier
  if CurrChar <> '$' then
    Fail(SEJPathRootMiss);
  //trailing spaces are not allowed
  if FLast^ in WHITE_SPACE then
    Fail(SEJTrailWSNotAllow);
  //query can not end with a dot
  if FLast^ = '.' then
    Fail(SEJPathCantEndWithDot);
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
        on e: Exception do
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

function JpParseQuery(const aQuery: string; out aMsg: string): IJsonPath;
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

function JpParseQuery(const aQuery: string; out aPath: IJsonPath): Boolean;
var
  Dummy: string;
begin
  aPath := JpParseQuery(aQuery, Dummy);
  Result := aPath <> nil;
end;

function JpMatch(const aQuery: string; aRoot: TJsonNode; out aList: TJpNodeList; out aMsg: string): Boolean;
var
  Matcher: specialize TGAutoRef<TJpMatcher>;
begin
  aMsg := '';
  aList := nil;
  Result := Matcher.Instance.TryParseQuery(aQuery);
  if Result then
    aList := Matcher.Instance.Match(aRoot)
  else
    aMsg := Matcher.Instance.Message;
end;

function JpMatch(const aQuery: string; aRoot: TJsonNode; out aList: TJpNodeList): Boolean;
var
  Dummy: string;
begin
  Result := JpMatch(aQuery, aRoot, aList, Dummy);
end;

function JpMatch(const aQuery, aRoot: string; out aList, aMsg: string): Boolean;
var
  Matcher: specialize TGAutoRef<TJpMatcher>;
begin
  aMsg := '';
  aList := '';
  Result := Matcher.Instance.TryParseQuery(aQuery);
  if Result then
    begin
      Result := Matcher.Instance.Match(aRoot, aList);
      if not Result then
        aMsg := SEInvalidJsonInst;
    end
  else
    aMsg := Matcher.Instance.Message;
end;

function JpMatch(const aQuery, aRoot: string; out aList: string): Boolean;
var
  Dummy: string;
begin
  Result := JpMatch(aQuery, aRoot, aList, Dummy);
end;

function JpMatchValues(const aQuery: string; aRoot: TJsonNode; out aList: TJpValueList;
  out aMsg: string): Boolean;
var
  Matcher: specialize TGAutoRef<TJpMatcher>;
begin
  aMsg := '';
  aList := nil;
  Result := Matcher.Instance.TryParseQuery(aQuery);
  if Result then
    aList := Matcher.Instance.MatchValues(aRoot)
  else
    aMsg := Matcher.Instance.Message;
end;

function JpMatchValues(const aQuery: string; aRoot: TJsonNode; out aList: TJpValueList): Boolean;
var
  Dummy: string;
begin
  Result := JpMatchValues(aQuery, aRoot, aList, Dummy);
end;

function JpMatchValues(const aQuery, aRoot: string; out aList, aMsg: string): Boolean;
var
  Matcher: specialize TGAutoRef<TJpMatcher>;
begin
  aMsg := '';
  aList := '';
  Result := Matcher.Instance.TryParseQuery(aQuery);
  if Result then
    begin
      Result := Matcher.Instance.MatchValues(aRoot, aList);
      if not Result then
        aMsg := SEInvalidJsonInst;
    end
  else
    aMsg := Matcher.Instance.Message;
end;

function JpMatchValues(const aQuery, aRoot: string; out aList: string): Boolean;
var
  Dummy: string;
begin
  Result := JpMatchValues(aQuery, aRoot, aList, Dummy);
end;

function JpMatchFirst(const aQuery: string; aRoot: TJsonNode; out aNode: TJpNode; out aMsg: string): Boolean;
var
  Matcher: specialize TGAutoRef<TJpMatcher>;
begin
  aMsg := '';
  aNode := TJpNode.Make('', nil);
  Result := Matcher.Instance.TryParseQuery(aQuery);
  if Result then
    aNode := Matcher.Instance.MatchFirst(aRoot)
  else
    aMsg := Matcher.Instance.Message;
end;

function JpMatchFirst(const aQuery: string; aRoot: TJsonNode; out aNode: TJpNode): Boolean;
var
  Dummy: string;
begin
  Result := JpMatchFirst(aQuery, aRoot, aNode, Dummy);
end;

function JpMatchFirst(const aQuery, aRoot: string; out aNode, aMsg: string): Boolean;
var
  Matcher: specialize TGAutoRef<TJpMatcher>;
begin
  aMsg := '';
  aNode := '';
  Result := Matcher.Instance.TryParseQuery(aQuery);
  if Result then
    begin
      Result := Matcher.Instance.MatchFirst(aRoot, aNode);
      if not Result then
        aMsg := SEInvalidJsonInst;
    end
  else
    aMsg := Matcher.Instance.Message;
end;

function JpMatchFirst(const aQuery, aRoot: string; out aNode: string): Boolean;
var
  Dummy: string;
begin
  Result := JpMatchFirst(aQuery, aRoot, aNode, Dummy);
end;

function JpMatchFirstValue(const aQuery: string; aRoot: TJsonNode; out aNode: TJsonNode; out aMsg: string): Boolean;
var
  Matcher: specialize TGAutoRef<TJpMatcher>;
begin
  aMsg := '';
  aNode := nil;
  Result := Matcher.Instance.TryParseQuery(aQuery);
  if Result then
    aNode := Matcher.Instance.MatchFirstValue(aRoot)
  else
    aMsg := Matcher.Instance.Message;
end;

function JpMatchFirstValue(const aQuery: string; aRoot: TJsonNode; out aNode: TJsonNode): Boolean;
var
  Dummy: string;
begin
  Result := JpMatchFirstValue(aQuery, aRoot, aNode, Dummy);
end;

function JpMatchFirstValue(const aQuery, aRoot: string; out aNode, aMsg: string): Boolean;
var
  Matcher: specialize TGAutoRef<TJpMatcher>;
begin
  aMsg := '';
  aNode := '';
  Result := Matcher.Instance.TryParseQuery(aQuery);
  if Result then
    begin
      Result := Matcher.Instance.MatchFirstValue(aRoot, aNode);
      if not Result then
        aMsg := SEInvalidJsonInst;
    end
  else
    aMsg := Matcher.Instance.Message;
end;

function JpMatchFirstValue(const aQuery, aRoot: string; out aNode: string): Boolean;
var
  Dummy: string;
begin
  Result := JpMatchFirstValue(aQuery, aRoot, aNode, Dummy);
end;

function IRegexpCheck(const aRegex: string; out aErrPos: SizeInt): TIRegexpCheck;
const
  NORMAL_CHARS    = [#0..#$27, #$2c, #$2d, #$2f..#$3e, #$40..#$5a, #$5e..#$7a, #$7e..#$ff];
  CC_CHAR         = [#0..#$2c, #$2e..#$5a, #$5e..#$ff];
  META_CHARS      = ['.', '*', '+', '?', '(', ')', '|', '{', '}', '[', '\', ']'];
  ESCAPABLE_CHARS = META_CHARS + ['-', '^', 'n', 'r', 't'];
  U_CATEGORIES    = ['C', 'L', 'M', 'N', 'P', 'S', 'Z'];
  U_LETTER_PROPS  = ['l', 'm', 'o', 't', 'u'];
  U_MARK_PROPS    = ['c', 'e', 'n'];
  U_NUMBER_PROPS  = ['d', 'l', 'o'];
  U_PUNCTUA_PROPS = ['c', 'd', 'e', 'f', 'i', 'o', 's'];
  U_SEPARAT_PROPS = ['l', 'p', 's'];
  U_SYMBOL_PROPS  = ['c', 'k', 'm', 'o'];
  U_OTHER_PROPS   = ['c', 'f', 'n', 'o'];
  DIGITS          = ['0'..'9'];
var
  pCurr: PAnsiChar = nil;
  pEnd: PAnsiChar = nil;

  function Error(aState: TIRegexpCheck): TIRegexpCheck; inline;
  begin
    aErrPos := Succ(pCurr - PAnsiChar(aRegex));
    Result := aState;
  end;

  function Eof: Boolean; inline;
  begin
    Result := pCurr >= pEnd;
  end;

  function CheckChar: TIRegexpCheck; inline;
  begin
    if Eof then
      exit(Error(ircUnexpectedEnd))
    else
      if pCurr^ in META_CHARS then
        exit(Error(ircUnexpectMetachar));
    while not Eof and (pCurr^ in NORMAL_CHARS) do
      pCurr += Utf8CodePointLength(pCurr, pEnd - pCurr);
    Result := ircOk;
  end;

  function CheckCategory: TIRegexpCheck; inline;
  var
    c: AnsiChar;
  begin
    if Eof then exit(Error(ircInvalidUCategory));
    if pCurr^ in U_CATEGORIES then begin
      c := pCurr^;
      Inc(pCurr);
      if Eof then
        exit(Error(ircInvalidUCategory))
      else
        if pCurr^ = '}' then begin
          Inc(pCurr); exit(ircOk);
        end;
      case c of
        'C':
          if not (pCurr^ in U_OTHER_PROPS) then exit(Error(ircInvalidUCategory));
        'L':
          if not (pCurr^ in U_LETTER_PROPS) then exit(Error(ircInvalidUCategory));
        'M':
          if not (pCurr^ in U_MARK_PROPS) then exit(Error(ircInvalidUCategory));
        'N':
          if not (pCurr^ in U_NUMBER_PROPS) then exit(Error(ircInvalidUCategory));
        'P':
          if not (pCurr^ in U_PUNCTUA_PROPS) then exit(Error(ircInvalidUCategory));
        'S':
          if not (pCurr^ in U_SYMBOL_PROPS) then exit(Error(ircInvalidUCategory));
        'Z':
          if not (pCurr^ in U_SEPARAT_PROPS) then exit(Error(ircInvalidUCategory));
      else
      end;
      Inc(pCurr);
      if Eof or (pCurr^ <> '}') then exit(Error(ircMissingRBrace));
      Inc(pCurr);
      Result := ircOk;
    end else
      Result := Error(ircInvalidUCategory);
  end;

  function CheckEscape: TIRegexpCheck; inline;
  begin
    if Eof then exit(Error(ircUnexpectedEnd));
    if pCurr^ in ESCAPABLE_CHARS then begin
      Inc(pCurr);
      Result := ircOk;
    end else
      if pCurr^ in ['P', 'p'] then begin
        Inc(pCurr);
        if Eof then
          exit(Error(ircUnexpectedEnd))
        else
          if pCurr^ <> '{' then
            exit(Error(ircInvalidUCategory));
        Inc(pCurr);
        Result := CheckCategory;
      end else
        Result := Error(ircInescapable);
  end;

  function CheckCCChar: TIRegexpCheck; inline;
  begin
    if pCurr^ = '\' then begin
      Inc(pCurr);
      if Eof then exit(Error(ircUnexpectedEnd));
      if pCurr^ in ESCAPABLE_CHARS then begin
        Inc(pCurr);
        Result := ircOk;
      end else
        Result := Error(ircInescapable);
    end else
      if pCurr^ in CC_CHAR then begin
        pCurr += Utf8CodePointLength(pCurr, pEnd - pCurr);
        Result := ircOk;
      end else
        Result := Error(ircInvalidCCChar);
  end;

  function CheckCharRange: TIRegexpCheck; inline;
  begin
    Result := CheckCCChar;
    if (Result = ircOk) and (pCurr^ = '-') then begin
      Inc(pCurr);
      if pCurr^ <> ']' then
        Result := CheckCCChar;
    end;
  end;

  function CheckCCE1: TIRegexpCheck;
  begin
    if Eof then exit(Error(ircUnexpectedEnd));
    if (pCurr^ = '\') and (pCurr[1] in ['P', 'p']) then begin
      pCurr += 2;
      if Eof or (pCurr^ <> '{') then exit(Error(ircInvalidUCategory));
      Inc(pCurr);
      Result := CheckCategory;
    end else
      if (pCurr^ = '-') and (pCurr[1] = ']') then begin
        Inc(pCurr);
        Result := ircOk;
      end else
        Result := CheckCharRange;
  end;

  function CheckCharClassExpr: TIRegexpCheck;
  begin
    if pCurr^ = '^' then Inc(pCurr);
    if pCurr^ = '-' then Inc(pCurr);
    repeat
      Result := CheckCCE1;
      if Result <> ircOk then exit;
      if Eof then exit(Error(ircMissingRBracket));
      if pCurr^ = ']' then begin
        Inc(pCurr); break;
      end;
    until False;
  end;

  function CheckQuantifier: TIRegexpCheck;
  begin
    Result := ircOk;
    case pCurr^ of
      '?', '*', '+': Inc(pCurr);
      '{':
        begin
          Inc(pCurr);
          if pCurr^ in DIGITS then begin
            repeat Inc(pCurr);
            until not(pCurr^ in DIGITS);
            case pCurr^ of
              '}': Inc(pCurr);
              ',':
                begin
                  Inc(pCurr);
                  if pCurr^ in DIGITS then begin
                    repeat Inc(pCurr);
                    until not(pCurr^ in DIGITS);
                    if pCurr^ = '}' then
                      Inc(pCurr)
                    else
                      exit(Error(ircMissingRBrace));
                  end else
                    if pCurr^ = '}' then
                      Inc(pCurr)
                    else
                      exit(Error(ircInvalidQuantifier));
                end;
            else
              exit(Error(ircInvalidQuantifier));
            end;
          end else
            exit(Error(ircInvalidQuantifier));
        end;
    else
    end;
  end;

  function CheckExpr: TIRegexpCheck; forward;

  function CheckAtom: TIRegexpCheck;
  begin
    case pCurr^ of
      '(':
        begin
          Inc(pCurr);
          if Eof then exit(Error(ircMissingRParen));
          if pCurr^ = ')' then begin // empty regexp
            Inc(pCurr); exit(ircOk);
          end;
          Result := CheckExpr;
          if Result <> ircOk then exit;
          if Eof or (pCurr^ <> ')') then exit(Error(ircMissingRParen));
          Inc(pCurr);
        end;
      '.':
        begin
          Result := ircOk;
          Inc(pCurr);
        end;
      '[':
        begin
          Inc(pCurr);
          Result := CheckCharClassExpr;
        end;
      '\':
        begin
          Inc(pCurr);
          Result := CheckEscape;
        end;
    else
      Result := CheckChar;
    end;
  end;

  function CheckBranch: TIRegexpCheck;
  begin
    repeat
      if pCurr^ = '|' then exit(ircOk); //zero pieces in a branch
      Result := CheckAtom;
      if Result = ircOk then
        Result := CheckQuantifier;
      if (Result <> ircOk) or Eof then exit;
    until not (pCurr^ in (NORMAL_CHARS + ['(', '.', '[', '\']));
  end;

var
  Depth: Integer = 0;
const
  MAX_DEPTH = 512;

  function CheckExpr: TIRegexpCheck;
  begin
    if Depth = MAX_DEPTH then exit(Error(ircMaxDepthExceed));
    Inc(Depth);
    repeat
      Result := CheckBranch;
      if Result <> ircOk then exit;
      if pCurr^ = '|' then
        Inc(pCurr)
      else
        break;
    until Eof;
    Dec(Depth);
  end;

begin
  aErrPos := 0;
  if aRegex = '' then exit(ircOk);
  pCurr := PAnsiChar(aRegex);
  pEnd := pCurr + System.Length(aRegex);
  Result := CheckExpr;
  if (Result = ircOk) and (pCurr < pEnd) then
    Result := Error(ircExtraChars);
end;

procedure CallCountFun(const aList: TJpParamList; out aResult: TJpInstance);
begin
  aResult := 0;
  if System.Length(aList) = 1 then
    if aList[0].InstType = jitNodeList then
      aResult := System.Length(aList[0].NodeList)
    else
      if (aList[0].InstType = jitValue) and (aList[0].Value.ValType = jvtNode) then
        aResult := 1;
end;

procedure CallLengthFun(const aList: TJpParamList; out aResult: TJpInstance);
begin
  aResult := TJpValue.Nothing;
  if (System.Length(aList) = 1) and (aList[0].InstType = jitValue) then
    with aList[0].Value do
      case ValType of
        jvtString:
          aResult := Utf8StrLen(StrValue);
        jvtNode:
          case NodeValue.Kind of
            jvkString:
              aResult := Utf8StrLen(NodeValue.AsString);
            jvkArray, jvkObject:
              aResult := NodeValue.Count;
          else
          end;
      else
      end;
end;

procedure CallValueFun(const aList: TJpParamList; out aResult: TJpInstance);
begin
  aResult := TJpValue.Nothing;
  if System.Length(aList) = 1 then
    case aList[0].InstType of
      jitNodeList:
        if System.Length(aList[0].NodeList) = 1 then
          aResult := aList[0].NodeList[0];
      jitValue:
        aResult := aList[0].Value.NodeValue;
    else
    end;
end;

function DefaultRegexMatch(const aInput, aRegex: string): Boolean;
begin
  try
    with TRegExpr.Create do
      try
        ModifierR := False;
        Expression := aRegex;
        Result := Exec(aInput);
      finally
        Free;
      end;
  except
    Result := False;
  end;
end;

function IsStringInst(const aInst: TJpInstance; out s: string): Boolean; inline;
begin
  if aInst.InstType = jitValue then
    case aInst.Value.ValType of
      jvtString:
        begin
          s := aInst.Value.StrValue;
          exit(True);
        end;
      jvtNode:
        if aInst.Value.NodeValue.IsString then
          begin
            s := aInst.Value.NodeValue.AsString;
            exit(True);
          end;
    else
    end;
  Result := False;
end;

function IRegexpCheck(const s: string): Boolean; inline;
var
  I: SizeInt;
begin
  Result := IRegexpCheck(s, I) = ircOk;
end;

{$PUSH}{$WARN 5091 OFF}
procedure CallMatchFun(const aList: TJpParamList; out aResult: TJpInstance);
var
  Input, Regex: string;
begin
  if (System.Length(aList) = 2) and IsStringInst(aList[0], Input) and
    IsStringInst(aList[1], Regex){$IFDEF JP_IREGEXP_CHECK}and IRegexpCheck(Regex){$ENDIF}then
    if JpRegexMatch = nil then
      aResult := DefaultRegexMatch(Input, '^(?:' + Regex + ')$')
    else
      aResult := JpRegexMatch(Input, Regex)
  else
    aResult := False;
end;

procedure CallSearchFun(const aList: TJpParamList; out aResult: TJpInstance);
var
  Input, Regex: string;
begin
  if (System.Length(aList) = 2) and IsStringInst(aList[0], Input) and
    IsStringInst(aList[1], Regex){$IFDEF JP_IREGEXP_CHECK}and IRegexpCheck(Regex){$ENDIF}then
    if JpRegexSearch = nil then
      aResult := DefaultRegexMatch(Input, Regex)
    else
      aResult := JpRegexSearch(Input, Regex)
  else
    aResult := False;
end;
{$POP}

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
