{****************************************************************************
*                                                                           *
*   This file is part of the LGenerics package.                             *
*   JSONPath(RFC 9535) implementation: query expressions for JSON.          *
*                                                                           *
*   Copyright(c) 2023-2024 A.Koverdyaev(avk)                                *
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
  https://www.rfc-editor.org/rfc/rfc9535
  ---------------------------------------------------------------------------------------

      Basic elements:

  ---------------------------------------------------------------------------------------
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
  ---------------------------------------------------------------------------------------

      Operators supported in the filter expressions in descending order of precedence:

  ---------------------------------------------------------------------------------------
    5 - grouping   ( ... )
    4 - logical NOT   !
    3 - relations   ==, !=, <, >, <=, >=
    2 - logical AND  &&
    1 - logical OR   ||
  ---------------------------------------------------------------------------------------

      Built-in functions:

  ---------------------------------------------------------------------------------------
    length(Arg)
      Arg type must be a ValueType(jitValue in this implementation);
      result type is a ValueType;
        if Arg is a string, returns the Utf8 length of that string;
        if Arg is an Array, returns the number of its elements;
        if Arg is an Object, returns the number of its pairs;
        for any other argument value returns Nothing(jvtNothing);

    count(Arg)
      the Arg type must be a Nodes(jitNodeList in this implementation);
      result type is a ValueType;
        returns the number of nodes in the Nodelist;

    match(Input, Regex)
      Input type must be a ValueType;
      Regex type must be a ValueType;
      result type is a LogicalType(jitLogical in this implementation);
        returns boolean True if Input is a string and Regex is a string and the regular
        expression Regex matches the entire(implicitly anchored) Input value(according to RFC 9485);

    search(Input, Regex)
      Input type must be a ValueType;
      Regex type must be a ValueType;
      result type is a LogicalType;
        returns boolean True if Input is a string and Regex is a string and the regular
        expression Regex matches some substring of the Input value(according to RFC 9485);

    NOTE: all backslashes in regular expressions specified as a literal in a JSONPath query
          need to be doubled(excluding sequences like \uxxxx).


    value(Arg)
      Arg type must be a Nodes;
      if the argument contains a single node, returns value of that node;
      if the argument is empty or contains multiple nodes, returns Nothing;
  ---------------------------------------------------------------------------------------

      Supplemental(non-standard) functions, their names and semantics may change
      in the future if such functions appear in the specifications:

  ---------------------------------------------------------------------------------------

    key()
      result type is a ValueType;
        returns Index(number) or Name(string) of the current node;
        parentheses are required;

    param(Arg) -- useful for repeated use of a query without reparsing, see IJsonPath.Params[]
      Arg type must be a ValueType;
      result type is a ValueType;
        if Arg is a string and a parameter with that name is defined, returns its value,
        otherwise returns Nothing;

    is_boolean(Arg)
      Arg type must be a ValueType;
      result type is a LogicalType;
        returns boolean True if Arg is a boolean value;

    is_number(Arg)
      Arg type must be a ValueType;
      result type is a LogicalType;
        returns boolean True if Arg is a number;

    is_integer(Arg)
      Arg type must be a ValueType;
      result type is a LogicalType;
        returns boolean True if Arg is a number and is an exact integer;

    is_string(Arg)
      Arg type must be a ValueType;
      result type is a LogicalType;
        returns boolean True if Arg is string value;

    is_array(Arg)
      Arg type must be a ValueType;
      result type is a LogicalType;
        returns boolean True if Arg is an array;

    is_object(Arg)
      Arg type must be ValueType;
      result type is a LogicalType;
        returns boolean True if Arg is an object;

    is_struct(Arg)
      Arg type must be ValueType;
      result type is a LogicalType;
        returns boolean True if Arg is an array or object;

    contains(Input, Pattern)
      Input type must be a ValueType;
      Pattern type must be a ValueType;
      result type is a LogicalType;
        returns boolean True if Input is a string and Pattern is a string
        and Pattern is substring of the Input value;

    contains_text(Input, Pattern)
      case insensitive version of contains()

    same_text(Arg1, Arg2)
      Arg1 type must be a ValueType;
      Arg2 type must be a ValueType;
      result type is a LogicalType;
        returns boolean True if Arg1 is a string and Pattern is a string
        and Arg1 is the same as Arg2(case insensitive);

    starts_with(Input, Pattern)
      Input type must be a ValueType;
      Pattern type must be a ValueType;
      result type is a LogicalType;
        returns boolean True if Input is a string and Pattern is a string
        and Input starts with a Pattern value;

    ends_with(Input, Pattern)
      Input type must be a ValueType;
      Pattern type must be a ValueType;
      result type is a LogicalType;
        returns boolean True if Input is a string and Pattern is a string
        and Input ends with a Pattern value;

    to_upper(Arg)
      Arg type must be a ValueType;
      result type is a ValueType;
        if Arg is a string, returns it in uppercase, otherwise returns Nothing;

    to_lower(Arg)
      Arg type must be a ValueType;
      result type is a ValueType;
        if Arg is a string, returns it in lowercase, otherwise returns Nothing;

    concat(Arg1, Arg2)
      Arg1 type must be a ValueType;
      Arg2 type must be a ValueType;
      result type is a ValueType;
        if Arg1 is a string and Arg2 is a string, returns their concatenation,
        otherwise returns Nothing;

    min(Arg1, Arg2)
      Arg1 type must be a ValueType;
      Arg2 type must be a ValueType;
      result type is a ValueType;
        if Arg1 is a string and Arg2 is a string, or if Arg1 is a number and Arg2 is a number,
        returns their minimum value, otherwise returns Nothing;

    max(Arg1, Arg2)
      Arg1 type must be a ValueType;
      Arg2 type must be a ValueType;
      result type is a ValueType;
        if Arg1 is a string and Arg2 is a string, or if Arg1 is a number and Arg2 is a number,
        returns their maximum value, otherwise returns Nothing;

    abs(Arg)
      Arg type must be a ValueType;
      result type is a ValueType;
        if Arg is a number, returns it absolute value, otherwise returns Nothing;

    sum(Arg1, Arg2)
      Arg1 type must be a ValueType;
      Arg2 type must be a ValueType;
      result type is a ValueType;
        if Arg1 is a number and Arg2 is a number, returns their sum value,
        otherwise returns Nothing;

    diff(Arg1, Arg2)
      Arg1 type must be a ValueType;
      Arg2 type must be a ValueType;
      result type is a ValueType;
        if Arg1 is a number and Arg2 is a number, returns the difference Arg1 - Arg2,
        otherwise returns Nothing;

    prod(Arg1, Arg2)
      Arg1 type must be a ValueType;
      Arg2 type must be a ValueType;
      result type is a ValueType;
        if Arg1 is a number and Arg2 is a number, returns their product,
        otherwise returns Nothing;

  -----------------------------------------------------------------------------------

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
  { splits Path into segments; returns an empty array if Path is not a valid normalized path }
    function PathToSegments: TStringArray;
  { tries to split the normalized path aPath into segments; returns False if aPath is not a valid normalized path }
    class function TrySplitNormalPath(const aPath: string; out aSegments: TStringArray): Boolean; static;
  end;

  {TJpNodeList represent a JSONPath Nodelist }
  TJpNodeList = array of TJpNode;

  TJpNodeListHelper = type helper for TJpNodeList
    function AsJson: string;
  { returns an array of normalized paths }
    function ToPathList: TStringArray;
  { returns an array of JSON Pointers }
    function ToPointerList: TStringArray;
  end;

  TJpValueList = array of TJsonNode;

  TJpValueListHelper = type helper for TJpValueList
    function AsJson: string;
  end;

  TJpValueType = (jvtNothing, jvtNull, jvtFalse, jvtTrue, jvtString, jvtNumber, jvtNode);

  { TJpValue }
  TJpValue = record
    function AsBoolean: Boolean; inline;
    class function Nothing: TJpValue; static; inline;
    class function NullValue: TJpValue; static; inline;
    class function FalseValue: TJpValue; static; inline;
    class function TrueValue: TJpValue; static; inline;
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

{$PUSH}{$INTERFACES COM}
  IJsonPath = interface
  ['{3D2794A4-F950-40DA-A611-51F7B37A6C11}']
    procedure SetParam(const aName: string; const aValue: TJpValue);
    function  GetParam(const aName: string): TJpValue;
    function  HasParam(const aName: string; out aParam: TJpValue): Boolean;
  { returns NodeList that matches its internal, previously parsed JSONPath query;
    the freeing of the obtained values is the responsibility of aRoot }
    function  Match(aRoot: TJsonNode): TJpNodeList;
  { returns True and NodeList in aNodeList if aRoot is valid JSON, False otherwise }
    function  Match(const aRoot: string; out aNodeList: TJpNodeList): Boolean;
  { returns True and NodeList as JSON in aNodeList if aRoot is valid JSON, False otherwise }
    function  Match(const aRoot: string; out aNodeList: string): Boolean;
  { returns an array of TJsonNode that matches its internal, previously parsed JSONPath query;
    the freeing of the obtained values is the responsibility of aRoot }
    function  MatchValues(aRoot: TJsonNode): TJpValueList;
  { returns True and an array of JSON values in aNodeArray if aRoot is valid JSON, False otherwise }
    function  MatchValues(const aRoot: string; out aNodeArray: TJpValueList): Boolean;
  { returns True and an array of JSON values as JSON in aNodeArray if aRoot is valid JSON, False otherwise }
    function  MatchValues(const aRoot: string; out aNodeArray: string): Boolean;
  { returns the first element along with its location(Node) that matches its internal,
    previously parsed JSONPath query, if any, otherwise returns just ('', NIL);
    the freeing of the obtained value is the responsibility of aRoot }
    function  MatchFirst(aRoot: TJsonNode): TJpNode;
  { returns True and the first matching element along with its location
    in aNode(('', NIL) if no matches) if aRoot is valid JSON, False otherwise }
    function  MatchFirst(const aRoot: string; out aNode: TJpNode): Boolean;
  { returns True and the first matching element along with its location
    (empty object if there is no matches) as JSON if aRoot is valid JSON, False otherwise }
    function  MatchFirst(const aRoot: string; out aNode: string): Boolean;
  { returns the first matching element if any, otherwise returns NIL;
    the freeing of the obtained value is the responsibility of aRoot }
    function  MatchFirstValue(aRoot: TJsonNode): TJsonNode;
  { returns True and first matching element (NIL if there is no matches) in aValue
    if aRoot is valid JSON, False otherwise }
    function  MatchFirstValue(const aRoot: string; out aValue: TJsonNode): Boolean;
  { returns True and first matching element as JSON (an empty string if there is no matches)
    if aRoot is valid JSON, False otherwise }
    function  MatchFirstValue(const aRoot: string; out aValue: string): Boolean;
  { allows to define parameters and set their values }
    property  Params[const aName: string]: TJpValue read GetParam write SetParam;
  end;
{$POP}

{ parses a JSONPath aQuery expression, which must be a well-formed UTF-8 string starting
  with the root identifier, leading and trailing spaces are not allowed;
  returns nil if aQuery is invalid, in which case the aMsg parameter contains an error message }
  function JpParseQuery(const aQuery: string; out aMsg: string): IJsonPath;
{ same as above, but for the case where the content of the error message is of no interest }
  function JpParseQuery(const aQuery: string): IJsonPath;
{ returns True and IJsonPath in aPath if aQuery was successfully parsed, otherwise returns False }
  function JpParseQuery(const aQuery: string; out aPath: IJsonPath): Boolean;
{ returns True and IJsonPath in aPath if aQuery was successfully parsed,
  otherwise returns False and aMsg parameter contains an error message }
  function JpParseQuery(const aQuery: string; out aPath: IJsonPath; out aMsg: string): Boolean;

{ tries to apply JSONPath query aQuery to JSON value aRoot; returns False if aQuery is invalid,
  in which case the aMsg parameter contains an error message, otherwise returns the NodeList
  in the aList parameter; the freeing of the obtained values is the responsibility of aRoot }
  function JpMatch(const aQuery: string; aRoot: TJsonNode; out aList: TJpNodeList; out aMsg: string): Boolean;
{ same as above, but for the case where the content of the error message is of no interest}
  function JpMatch(const aQuery: string; aRoot: TJsonNode; out aList: TJpNodeList): Boolean;
{ tries to apply aQuery to JSON value aRoot; returns False if aRoot is invalid JSON
  or aQuery is invalid, in which case the aMsg parameter contains an error message,
  otherwise returns the NodeList as JSON in the aList parameter }
  function JpMatch(const aQuery, aRoot: string; out aList, aMsg: string): Boolean;
  function JpMatch(const aQuery, aRoot: string; out aList: TJpNodeList; out aMsg: string): Boolean;
{ same as above, but for the case where the content of the error message is of no interest }
  function JpMatch(const aQuery, aRoot: string; out aList: string): Boolean;
  function JpMatch(const aQuery, aRoot: string; out aList: TJpNodeList): Boolean;
{ tries to apply aQuery to the JSON value aRoot; returns False if aQuery is invalid,
  in which case, the aMsg parameter contains an error message;
  the freeing of the obtained values is the responsibility of aRoot }
  function JpMatchValues(const aQuery: string; aRoot: TJsonNode; out aList: TJpValueList; out aMsg: string): Boolean;
{ same as above, but for the case where the content of the error message is of no interest }
  function JpMatchValues(const aQuery: string; aRoot: TJsonNode; out aList: TJpValueList): Boolean;
{ tries to apply aQuery to the JSON value aRoot; returns False if aRoot is invalid JSON or
  aQuery is invalid, in which case, the aMsg parameter contains an error message }
  function JpMatchValues(const aQuery, aRoot: string; out aList, aMsg: string): Boolean;
  function JpMatchValues(const aQuery, aRoot: string; out aList: TJpValueList; out aMsg: string): Boolean;
{ same as above, but for the case where the content of the error message is of no interest }
  function JpMatchValues(const aQuery, aRoot: string; out aList: string): Boolean;
  function JpMatchValues(const aQuery, aRoot: string; out aList: TJpValueList): Boolean;
{ tries to apply aQuery to the JSON value aRoot and returns only the first match;
  returns False if aQuery is invalid, in which case the aMsg parameter contains an error message;
  the freeing of the obtained value is the responsibility of aRoot }
  function JpMatchFirst(const aQuery: string; aRoot: TJsonNode; out aNode: TJpNode; out aMsg: string): Boolean;
{ same as above, but for the case where the content of the error message is of no interest }
  function JpMatchFirst(const aQuery: string; aRoot: TJsonNode; out aNode: TJpNode): Boolean;
{ tries to apply aQuery to the JSON value aRoot and returns only the first match; returns False if aRoot
  is invalid JSON or aQuery is invalid, in which case the aMsg parameter contains an error message }
  function JpMatchFirst(const aQuery, aRoot: string; out aNode, aMsg: string): Boolean;
  function JpMatchFirst(const aQuery, aRoot: string; out aNode: TJpNode; out aMsg: string): Boolean;
{ same as above, but for the case where the content of the error message is of no interest }
  function JpMatchFirst(const aQuery, aRoot: string; out aNode: string): Boolean;
  function JpMatchFirst(const aQuery, aRoot: string; out aNode: TJpNode): Boolean;
{ tries to apply aQuery to the JSON value aRoot and returns only the first match;
  returns False if aQuery is invalid, in which case the aMsg parameter contains an error message;
  the freeing of the obtained value is the responsibility of aRoot }
  function JpMatchFirstValue(const aQuery: string; aRoot: TJsonNode; out aNode: TJsonNode; out aMsg: string): Boolean;
{ same as above, but for the case where the content of the error message is of no interest }
  function JpMatchFirstValue(const aQuery: string; aRoot: TJsonNode; out aNode: TJsonNode): Boolean;
{ tries to apply aQuery to the JSON value aRoot and returns only the first match;
  returns False if aRoot is invalid JSON or aQuery is invalid, in which case
  the aMsg parameter contains an error message }
  function JpMatchFirstValue(const aQuery, aRoot: string; out aNode, aMsg: string): Boolean;
{ same as above, but for the case where the content of the error message is of no interest }
  function JpMatchFirstValue(const aQuery, aRoot: string; out aNode: string): Boolean;


type
  { TJsonPathNodeHelper }
  TJsonPathNodeHelper = class helper for TJsonNode
  { returns NodeList that matches previously parsed JSONPath query aPath }
    function Match(aPath: IJsonPath): TJpNodeList;
  { tries to apply JSONPath query aQuery to the instance;
    returns False if aQuery is invalid, in which case the aMsg parameter contains
    an error message, otherwise returns the NodeList in the aList parameter }
    function TryMatch(const aJpQuery: string; out aList: TJpNodeList; out aMsg: string): Boolean;
  { same as above, but for the case where the content of the error message is of no interest}
    function TryMatch(const aJpQuery: string; out aList: TJpNodeList): Boolean;
  { returns an array of TJsonNode that matches previously parsed JSONPath query aPath }
    function MatchValues(aPath: IJsonPath): TJpValueList;
  { tries to apply JSONPath query aQuery to the instance;
    returns False if aQuery is invalid, in which case the aMsg parameter contains an error message }
    function TryMatchValues(const aJpQuery: string; out aList: TJpValueList; out aMsg: string): Boolean;
  { same as above, but for the case where the content of the error message is of no interest }
    function TryMatchValues(const aJpQuery: string; out aList: TJpValueList): Boolean;
  { returns the first element along with its location that matches previously parsed
    JSONPath query aPath, if any, otherwise returns ('', NIL) }
    function MatchFirst(aPath: IJsonPath): TJpNode;
  { tries to apply JSONPath query aQuery to the instance and returns only the first match in aNode;
    returns False if aQuery is invalid, in which case the aMsg parameter contains an error message }
    function TryMatchFirst(const aJpQuery: string; out aNode: TJpNode; out aMsg: string): Boolean;
  { same as above, but for the case where the content of the error message is of no interest }
    function TryMatchFirst(const aJpQuery: string; out aNode: TJpNode): Boolean;
  { returns the first matching element if any, otherwise returns NIL }
    function MatchFirstValue(aPath: IJsonPath): TJsonNode;
  { tries to apply JSONPath query aQuery to the instance and returns only the first match;
    returns False if aQuery is invalid, in which case the aMsg parameter contains an error message }
    function TryMatchFirstValue(const aJpQuery: string; out aNode: TJsonNode; out aMsg: string): Boolean;
  { same as above, but for the case where the content of the error message is of no interest }
    function TryMatchFirstValue(const aJpQuery: string; out aNode: TJsonNode): Boolean;
  { tries to find an element using a path specified as a normalized path }
    function FindNormalPath(const aNormalPath: string; out aNode: TJsonNode): Boolean;
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

{ I-Regexp checker: just in case }
  function IRegexpCheck(const aRegex: string; out aMsg: string): Boolean;

implementation
{$B-}{$COPERATORS ON}{$POINTERMATH ON}

uses
  Math, lgVector, lgHashMap, lgSeqUtils, lgStrConst, Character, UnicodeData;

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


function TJpNode.PathToSegments: TStringArray;
begin
  TrySplitNormalPath(Path, Result);
end;

{$PUSH}{$WARN 5089 OFF}{$WARN 5094 OFF}{$WARN 5036 OFF}
class function TJpNode.TrySplitNormalPath(const aPath: string; out aSegments: TStringArray): Boolean;
  function Hex2Byte(c: AnsiChar): Byte; inline;
  begin
    case c of
      '0'..'9': Result := Ord(c) - Ord('0');
      'A'..'F': Result := Ord(c) - Ord('A') + 10;
      'a'..'f': Result := Ord(c) - Ord('a') + 10;
    else
      Result := $f;
    end;
  end;
var
  List: specialize TGLiteVector<string>;
  sb: TStrBuilder;
  p, pEnd: PAnsiChar;
const
  HexChars = ['0'..'9','A'..'F','a'..'f'];
begin
  aSegments := nil;
  if (System.Length(aPath) < 1) or (aPath[1] <> '$') then exit(False);
  p := Pointer(aPath);
  pEnd := p + System.Length(aPath);
  Inc(p);
  sb.Create(64);
  while p < pEnd do begin
    if p^ <> '[' then exit(False);
    Inc(p);
    case p^ of
      '0'..'9': begin
          repeat
            sb.Append(p^);
            Inc(p);
          until not(p^ in ['0'..'9']);
          if p^ <> ']' then exit(False);
          List.Add(sb.ToString);
          Inc(p);
        end;
      '''': begin
         Inc(p);
         if p >= pEnd then exit(False);
         while p^ <> '''' do begin
           if p^ = '\' then begin
             if p > pEnd - 2 then exit(False);
             Inc(p);
             case p^ of
               'b': begin sb.Append(#8); Inc(p); end;
               'f': begin sb.Append(#12); Inc(p); end;
               'n': begin sb.Append(#10); Inc(p); end;
               'r': begin sb.Append(#13); Inc(p); end;
               't': begin sb.Append(#9); Inc(p); end;
               'u': begin
                 if (p > pEnd - 5) or (p[1] <> '0') or (p[2] <> '0') or
                     not ((p[3] in HexChars) and (p[3] in HexChars)) then exit(False);
                 sb.Append(AnsiChar(Hex2Byte(p[3]) shl 4 or Hex2Byte(p[4])));
                 p += 5;
               end
             else
               sb.Append(p^);
               Inc(p);
             end;
           end else begin
             sb.Append(p^);
             Inc(p);
           end;
           if p >= pEnd then exit(False);
         end;
         Inc(p);
         if p^ <> ']' then exit(False);
         Inc(p);
         List.Add(sb.ToString);
       end;
    else
      exit(False);
    end;
  end;
  aSegments := List.ToArray;
  Result := True;
end;
{$POP}

{ TJpNodeListHelper }
{$PUSH}{$WARN 6058 OFF}{$WARN 5093 OFF}
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

function TJpNodeListHelper.ToPathList: TStringArray;
var
  r: TStringArray;
  I: SizeInt;
begin
  System.SetLength(r, System.Length(Self));
  for I := 0 to System.High(Self) do
    r[I] := Self[I].Path;
  Result := r;
end;

function TJpNodeListHelper.ToPointerList: TStringArray;
var
  r: TStringArray;
  I: SizeInt;
begin
  System.SetLength(r, System.Length(Self));
  for I := 0 to System.High(Self) do
    r[I] := TJsonPtr.ToPointer(Self[I].PathToSegments);
  Result := r;
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

class function TJpValue.NullValue: TJpValue;
begin
  Result.ValType := jvtNull;
end;

class function TJpValue.FalseValue: TJpValue;
begin
  Result.ValType := jvtFalse;
end;

class function TJpValue.TrueValue: TJpValue;
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

{$PUSH}{$WARN 5059 OFF}
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
{$POP}

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

{ TJsonPathNodeHelper }

function TJsonPathNodeHelper.Match(aPath: IJsonPath): TJpNodeList;
begin
  Result := aPath.Match(Self);
end;

function TJsonPathNodeHelper.TryMatch(const aJpQuery: string; out aList: TJpNodeList;
  out aMsg: string): Boolean;
begin
  Result := JpMatch(aJpQuery, Self, aList, aMsg);
end;

function TJsonPathNodeHelper.TryMatch(const aJpQuery: string; out aList: TJpNodeList): Boolean;
begin
  Result := JpMatch(aJpQuery, Self, aList);
end;

function TJsonPathNodeHelper.MatchValues(aPath: IJsonPath): TJpValueList;
begin
  Result := aPath.MatchValues(Self);
end;

function TJsonPathNodeHelper.TryMatchValues(const aJpQuery: string; out aList: TJpValueList;
  out aMsg: string): Boolean;
begin
  Result := JpMatchValues(aJpQuery, Self, aList, aMsg);
end;

function TJsonPathNodeHelper.TryMatchValues(const aJpQuery: string; out aList: TJpValueList): Boolean;
begin
  Result := JpMatchValues(aJpQuery, Self, aList);
end;

function TJsonPathNodeHelper.MatchFirst(aPath: IJsonPath): TJpNode;
begin
  Result := aPath.MatchFirst(Self);
end;

function TJsonPathNodeHelper.TryMatchFirst(const aJpQuery: string; out aNode: TJpNode;
  out aMsg: string): Boolean;
begin
  Result := JpMatchFirst(aJpQuery, Self, aNode, aMsg);
end;

function TJsonPathNodeHelper.TryMatchFirst(const aJpQuery: string; out aNode: TJpNode): Boolean;
begin
  Result := JpMatchFirst(aJpQuery, Self, aNode);
end;

function TJsonPathNodeHelper.MatchFirstValue(aPath: IJsonPath): TJsonNode;
begin
  Result := aPath.MatchFirstValue(Self);
end;

function TJsonPathNodeHelper.TryMatchFirstValue(const aJpQuery: string; out aNode: TJsonNode;
  out aMsg: string): Boolean;
begin
  Result := JpMatchFirstValue(aJpQuery, Self, aNode, aMsg);
end;

function TJsonPathNodeHelper.TryMatchFirstValue(const aJpQuery: string; out aNode: TJsonNode): Boolean;
begin
  Result := JpMatchFirstValue(aJpQuery, Self, aNode);
end;

function TJsonPathNodeHelper.FindNormalPath(const aNormalPath: string; out aNode: TJsonNode): Boolean;
var
  Segments: TStringArray;
begin
  aNode := nil;
  if not TJpNode.TrySplitNormalPath(aNormalPath, Segments) then exit(False);
  Result := FindPath(Segments, aNode);
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

  TJpOnGetParam = function(const aName: string; out aValue: TJpValue): Boolean of object;

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
    FOnGetParam: TJpOnGetParam;
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
    property OnGetParam: TJpOnGetParam read FOnGetParam write FOnGetParam;
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
  TJpKeyKind    = (jkkNone, jkkIndex, jkkName);
  TJpContextKey = record
    Kind: TJpKeyKind;
    Index: SizeInt;
    Name: string;
  end;

  TJpFilterContext = record
    Root,
    Current: TJsonNode;
    Key: TJpContextKey;
    OnGetParam: TJpOnGetParam;
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

  { TJpKeyFunction }
  TJpKeyFunction = class(TJpFunctionExpr)
  strict protected
    procedure Eval(const aCtx: TJpFilterContext; var v: TJpInstance); override;
  end;

  { TJpParamFunction }
  TJpParamFunction = class(TJpFunctionExpr)
  strict protected
    procedure Eval(const aCtx: TJpFilterContext; var v: TJpInstance); override;
  end;

  TIRegexp = class;

  { TJpReMatch }
  TJpReMatch = class(TJpFunctionExpr)
  strict protected
    FMatcher: TIRegexp;
    FLiteralRe: Boolean;
    function  CreateMatcher(const aRegex: string; aLiteral: Boolean): TIRegexp; virtual;
    procedure CheckRegexLiteral;
    function  TryMatch: Boolean;
    function  TryMatchParsed: Boolean;
    procedure Eval(const aCtx: TJpFilterContext; var v: TJpInstance); override;
    property  LiteralRegex: Boolean read FLiteralRe;
  public
    destructor Destroy; override;
    procedure AfterConstruction; override;
  end;

  { TJpReSearch }
  TJpReSearch = class(TJpReMatch)
  strict protected
    function CreateMatcher(const aRegex: string; aLiteral: Boolean): TIRegexp; override;
  end;

  { TJpFilter }
  TJpFilter = class
  strict private
    FContext: TJpFilterContext;
    FExpr: TJpExpression;
    procedure SetIndex(aValue: SizeInt);
    procedure SetName(const aValue: string);
    procedure SetOnGetParam(aValue: TJpOnGetParam);
    procedure SetRoot(aValue: TJsonNode);
  public
    constructor Create(aExpr: TJpExpression);
    destructor Destroy; override;
    function Apply(aNode: TJsonNode): Boolean;
    property Root: TJsonNode write SetRoot;
    property Index: SizeInt write SetIndex;
    property Name: string write SetName;
    property OnGetParam: TJpOnGetParam write SetOnGetParam;
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
      jtkFunction, jtkRootId, jtkCurrId, jtkComma, jtkRBracket);
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
  {$PUSH}{$J-}
    TOKENS: array[TJpTokenKind] of string = (
      '(', ')', '||',  '&&', '!', '<', '>', '==', '!=', '<=',  '>=', 'null', 'false', 'true',
      'string', 'number', 'function', '$', '@', ',', ']');
  {$POP}
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
    function  CheckFunParam(aType: TJpInstanceType; aExpr: TJpExpression): Boolean; inline;
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
  type
    TMapSpec = specialize TGLiteChainHashMap<string, TJpValue, string>;
  var
    FPath: TJsonSpecPathQuery;
    FParams: TMapSpec.TMap;
    FMessage: string;
  public
    constructor Create;
    destructor Destroy; override;
    procedure SetParam(const aName: string; const aValue: TJpValue);
    function  GetParam(const aName: string): TJpValue;
    function  HasParam(const aName: string; out aParam: TJpValue): Boolean;
    function  TryGetParam(const aName: string; out aValue: TJpValue): Boolean;
    function  TryParseQuery(const aQuery: string): Boolean;
    function  Match(aRoot: TJsonNode): TJpNodeList;
    function  Match(const aRoot: string; out aNodeList: TJpNodeList): Boolean;
    function  Match(const aRoot: string; out aNodeList: string): boolean;
    function  MatchValues(aRoot: TJsonNode): TJpValueList;
    function  MatchValues(const aRoot: string; out aNodeArray: TJpValueList): Boolean;
    function  MatchValues(const aRoot: string; out aNodeArray: string): Boolean;
    function  MatchFirst(aRoot: TJsonNode): TJpNode;
    function  MatchFirst(const aRoot: string; out aNode: TJpNode): Boolean;
    function  MatchFirst(const aRoot: string; out aNode: string): Boolean;
    function  MatchFirstValue(aRoot: TJsonNode): TJsonNode;
    function  MatchFirstValue(const aRoot: string; out aValue: TJsonNode): Boolean;
    function  MatchFirstValue(const aRoot: string; out aValue: string): Boolean;
    property  Message: string read FMessage;
  end;

  { TIRegexp: simple I-Regexp engine, just direct Thompson NFA simulation }
  TIRegexp = class
  public
  const
    Q_RANGE_LIMIT   = 512;
    MAX_QRANGE_LEN  = 3;
    MAX_REC_DEPTH   = 512;
  private
  const
    Q_RANGE_EXACT   = Integer(-1);
    Q_RANGE_MAX_ANY = Integer(-2);
    DISABLE_MOVE    = Integer(-1);
    NORMAL_CHARS    = [#0..#$27, #$2c, #$2d, #$2f..#$3e, #$40..#$5a, #$5e..#$7a, #$7e..#$ff];
    CC_CHARS        = [#0..#$2c, #$2e..#$5a, #$5e..#$ff];
    META_CHARS      = ['.', '*', '+', '?', '(', ')', '|', '{', '}', '[', '\', ']'];
    ESCAPABLE_CHARS = META_CHARS + ['-', '^', 'n', 'r', 't'];
    UCATEGORY_CHARS = ['C', 'L', 'M', 'N', 'P', 'S', 'Z'];
    DIGITS          = ['0'..'9'];
  type
    TUCategoryKind  = (
      uckCategory, uckLetter, uckMark, uckNumber, uckPunctuation, uckSeparator, uckSymbol, uckOther);
    TUCategory = record
    strict private
      FKind: TUCategoryKind;
      FCategory: TUnicodeCategory;
      FComplement: Boolean;
      class function GetCategory(c: Ucs4Char): TUnicodeCategory; inline; static;
    public
      constructor Make(aKind: TUCategoryKind; aComplement: Boolean = False);
      constructor Make(c: TUnicodeCategory; aComplement: Boolean = False);
      function Match(c: Ucs4Char): Boolean; inline;
    end;

    TCceItemKind = (cikChar, cikCategory, cikCharRange);

    PCceItem = ^TCceItem;
    TCceItem = record
      constructor Make(c: TUCategory);
      constructor Make(c: Ucs4Char);
      constructor Make(aLo, aHi: Ucs4Char);
      function Match(c: Ucs4Char): Boolean; inline;
      case Kind: TCceItemKind of
        cikChar:      (UChar: Ucs4Char);
        cikCategory:  (UCategory: TUCategory);
        cikCharRange: (CLow, CHigh: Ucs4Char);
    end;

    PCCStore = ^TCCStore;
    TCCStore = record
    strict private
    const
      INIT_LEN = 128;  // power of 2
    var
      FItems: specialize TGDynArray<TCceItem>;
      FCount: Integer;
      procedure Expand; inline;
      function  GetItems: PCceItem; inline;
    public
      procedure Reset; inline;
      procedure TrimToFit;
      function  Add(const aValue: TCceItem): Integer;
      property  Count: Integer read FCount;
      property  Items: PCceItem read GetItems;
    end;

    TCharClassExpr = record
    strict private
      FFirst,
      FLast: Integer;
      FStore: PCCStore;
      FComplement: Boolean;
    public
      procedure Init(aStore: PCCStore; aComplement: Boolean = False); inline;
      procedure Add(const aItem: TCceItem); inline;
      function  Match(c: Ucs4Char): Boolean;
    end;

    TCharClassKind = (cckAnyChar, cckWildcard, cckChar, cckCategory, cckClassExpr);

    TCharClass = record
      constructor Make(aKind: TCharClassKind);
      constructor Make(c: Ucs4Char);
      constructor Make(const c: TUCategory);
      constructor Make(const e: TCharClassExpr);
      function Match(c: Ucs4Char): Boolean; inline;
      case Kind: TCharClassKind of
        cckChar:      (VChar: Ucs4Char);
        cckCategory:  (VCategory: TUCategory);
        cckClassExpr: (VExpr: TCharClassExpr);
    end;

    TNodeKind  = (nkSplit, nkMove, nkMatch, nkFinal);

    PNfaNode = ^TNfaNode;
    TNfaNode = record
      Next1,
      Next2: Integer;
      Step: QWord;
      Matcher: TCharClass;
      Kind: TNodeKind;
      function Match(c: Ucs4Char): Boolean; inline;
    end;

    TIntDynArray = specialize TGDynArray<Integer>;

    TNfaTable = record
    strict private
    const
      INIT_LEN = 64;  //power of 2
    var
      FItems: specialize TGDynArray<TNfaNode>;
      FCount: Integer;
      procedure Expand; inline;
      function  GetItem(aIndex: Integer): PNfaNode; inline;
    public
      procedure Reset; inline;
      procedure TrimToFit;
      procedure AddRange(aCount: Integer);
      function  AddNode: Integer; inline;
      procedure Shrink;
      procedure Renumber(var aNewIndex: TIntDynArray);
      property  Items[aIndex: Integer]: PNfaNode read GetItem; default;
      property  Count: Integer read FCount;
    end;

    PReStack = ^TReStack;
    TReStack = record
    strict private
    const
      INIT_LEN = 64;  // power of 2
    var
      FItems: TIntDynArray;
      FCount: Integer;
      procedure Expand; inline;
    public
      function  Reset: PReStack; inline;
      function  NonEmpty: Boolean; inline;
      procedure Push(aValue: Integer); inline;
      function  TryPop(out aValue: Integer): Boolean; inline;
      property  Count: Integer read FCount;
    end;

  var
    FTable: TNfaTable;
    FCCStore: TCCStore;
    FStack1,
    FStack2: TReStack;
    FStep: QWord;
    FExpression,
    FMessage: string;
    FLook,
    FExprEnd: PAnsiChar;
    FStartNode,
    FFinalNode,
    FDepth: Integer;
    FLiteralExpr,
    FParseOk: Boolean;
    function  Fail(const aMessage: string): Boolean; inline;
    function  Eof: Boolean; inline;
    procedure SkipChar; inline;
    function  CurrChar: AnsiChar; inline;
    function  NextChar: AnsiChar; inline;
    function  AddFinal: Integer;
    function  AddMatch(const aClass: TCharClass; out aFinal: Integer): Integer;
    function  AddMove(out aFinal: Integer): Integer;
    function  AddSplit(aNext1, aNext2: Integer): Integer;
    procedure Patch(aNode: Integer; aKind: TNodeKind; aNext1: Integer; aNext2: Integer = DISABLE_MOVE);
    function  GetChar: Ucs4Char; inline;
    function  ParseNormalChar(out c: Ucs4Char): Boolean;
    function  GetNormalChar(out cc: TCharClass): Boolean;
    function  ParseUCategory(out uc: TUCategory): Boolean;
    function  GetUCategory(out cc: TCharClass): Boolean;
    function  ParseCharEsc(out c: Ucs4Char): Boolean;
    function  GetCharEsc(out cc: TCharClass): Boolean;
    function  ParseCcChar(out c: Ucs4Char): Boolean;
    function  ParseCceItem(out aItem: TCceItem): Boolean;
    function  ParseCharClassExpr(out cc: TCharClass): Boolean;
    function  ParseAtom(var aStart, aFinal: Integer): Boolean;
    procedure MakeQuestion(var aStart, aFinal: Integer);
    procedure MakeStar(var aStart, aFinal: Integer);
    procedure MakePlus(var aStart, aFinal: Integer);
    procedure CopyFragment(var aStart, aFinal: Integer; aFrom, aCount: Integer);
    procedure MakeQuantRange(var aStart, aFinal: Integer; aMin, aMax, aFragStart: Integer);
    function  ParseQuantRange(var aStart, aFinal: Integer; aFragStart: Integer): Boolean;
    function  ParseQuantifier(var aStart, aFinal: Integer; aFragStart: Integer): Boolean;
    function  ParseBranch(var aStart, aFinal: Integer): Boolean;
    function  ParseExpr(var aStart, aFinal: Integer): Boolean;
    function  TryParse: Boolean; virtual;
    procedure Parse;
    procedure RenumberNfa;
    procedure TrimToFit;
    procedure SetExpression(const aValue: string);
    procedure PushEclose(aNode: Integer; aStack: PReStack);
    class function  Str2Int(p: PAnsiChar; aCount: Integer; out aValue: Integer): Boolean; static;
    class procedure PtrSwap(var L, R: Pointer); static; inline;
  public
    constructor Create(const aExpr: string; aLiteral: Boolean);
    procedure AfterConstruction; override;
    function  Match(const aText: string): Boolean; virtual;
    property  Expression: string read FExpression write SetExpression;
    property  ParseOk: Boolean read FParseOk;
    property  Message: string read FMessage;
  end;

  { TIRegexpSearch }
  TIRegexpSearch = class(TIRegexp)
  private
    function TryParse: Boolean; override;
  public
    function Match(const aText: string): Boolean; override;
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
  LNode: TJsonNode;
begin
  if Done then exit;
  case aSel.Kind of
    skIndex:
      if aRoot.Kind = jvkArray then begin
        I := aSel.Index;
        if I < 0 then
          I += aRoot.Count;
        if aRoot.Find(I, LNode) then
          if aSel.NextSegment = nil then
            AddMatch(LNode)
          else
            ApplySegment(aSel.NextSegment, LNode);
      end;

    skName:
      if aRoot.Find(aSel.Name, LNode) then
        if aSel.NextSegment = nil then
          AddMatch(LNode)
        else
          ApplySegment(aSel.NextSegment, LNode);

    skWildcard:
      for LNode in aRoot do begin
        if aSel.NextSegment = nil then
          AddMatch(LNode)
        else
          ApplySegment(aSel.NextSegment, LNode);
        if Done then exit;
      end;

    skSlice:
      if aRoot.Kind = jvkArray then
        ApplySlice(aSel, aRoot);

    skFilter:
      begin
        with aSel.Filter do begin
          Root := FRoot;
          OnGetParam := Self.OnGetParam;
        end;
        case aRoot.Kind of
          jvkArray:
            for I := 0 to Pred(aRoot.Count) do begin
              aSel.Filter.Index := I;
              LNode := aRoot.Items[I];
              if aSel.Filter.Apply(LNode) then
                if aSel.NextSegment = nil then
                  AddMatch(LNode)
                else
                  ApplySegment(aSel.NextSegment, LNode);
              if Done then exit;
            end;
          jvkObject:
            for I := 0 to Pred(aRoot.Count) do
              with aRoot.Pairs[I] do begin
                aSel.Filter.Name := Key;
                if aSel.Filter.Apply(Value) then
                  if aSel.NextSegment = nil then
                    AddMatch(Value)
                  else
                    ApplySegment(aSel.NextSegment, Value);
                if Done then exit;
              end;
        else
        end;
      end;
  else
  end;
  if aDesc then
    for LNode in aRoot do begin
      if Done then exit;
      ApplySelector(aSel, LNode, True);
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
  LNode: TJsonNode;
begin
  if Done then exit;
  case aSel.Kind of
    skIndex:
      if aRoot.Kind = jvkArray then begin
        I := aSel.Index;
        if I < 0 then
          I += aRoot.Count;
        if aRoot.Find(I, LNode) then begin
          PathPush(I);
          if aSel.NextSegment = nil then
            AddMatchWithPath(LNode)
          else
            ApplySegmentWithPath(aSel.NextSegment, LNode);
          PathPop;
        end;
      end;

    skName:
      if aRoot.Find(aSel.Name, LNode) then begin
        PathPush(aSel.Name);
        if aSel.NextSegment = nil then
          AddMatchWithPath(LNode)
        else
          ApplySegmentWithPath(aSel.NextSegment, LNode);
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
        with aSel.Filter do begin
          Root := FRoot;
          OnGetParam := Self.OnGetParam;
        end;
        case aRoot.Kind of
          jvkArray:
            for I := 0 to Pred(aRoot.Count) do begin
              PathPush(I);
              aSel.Filter.Index := I;
              LNode := aRoot.Items[I];
              if aSel.Filter.Apply(LNode) then
                if aSel.NextSegment = nil then
                  AddMatchWithPath(LNode)
                else
                  ApplySegmentWithPath(aSel.NextSegment, LNode);
              PathPop;
              if Done then exit;
            end;
          jvkObject:
            for I := 0 to Pred(aRoot.Count) do
              with aRoot.Pairs[I] do begin
                PathPush(Key);
                aSel.Filter.Name := Key;
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
  FValue := TJpValue.NullValue;
end;

constructor TJpConstExpr.Create(b: Boolean);
begin
  if b then
    FValue := TJpValue.TrueValue
  else
    FValue := TJpValue.FalseValue;
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
  OnGetParam := aCtx.OnGetParam;
  if FIdent = jpiRoot then
    Result := GetFirst(aCtx.Root)
  else
    Result := GetFirst(aCtx.Current);
end;

function TRelPathQuery.GetNodeList(const aCtx: TJpFilterContext): TJpValueList;
begin
  OnGetParam := aCtx.OnGetParam;
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

{ TJpKeyFunction }

procedure TJpKeyFunction.Eval(const aCtx: TJpFilterContext; var v: TJpInstance);
begin
  case aCtx.Key.Kind of
    jkkIndex: v := aCtx.Key.Index;
    jkkName:  v := aCtx.Key.Name;
  else
    v := TJpValue.Nothing;
  end;
end;

{ TJpParamFunction }

procedure TJpParamFunction.Eval(const aCtx: TJpFilterContext; var v: TJpInstance);
var
  Inst: TJpInstance;
  Param: TJpValue;
begin
  if (aCtx.OnGetParam <> nil) and (System.Length(FParamList) = 1) then
    begin
      Inst := FParamList[0].GetValue(aCtx);
      if (Inst.InstType = jitValue) and (Inst.Value.ValType = jvtString) and
          aCtx.OnGetParam(Inst.Value.StrValue, Param) then
        v := Param
      else
        v := TJpValue.Nothing;
    end
  else
    v := TJpValue.Nothing;
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

{ TJpReMatch }

function TJpReMatch.CreateMatcher(const aRegex: string; aLiteral: Boolean): TIRegexp;
begin
  Result := TIRegexp.Create(aRegex, aLiteral);
end;

procedure TJpReMatch.CheckRegexLiteral;
var
  Regex: string;
begin
  if (System.Length(FParamList) = 2) and (FParamList[1] is TJpConstExpr) then begin
    FLiteralRe := True;
    if IsStringInst(FParamList[1].GetValue(Default(TJpFilterContext)), Regex) then begin
      FMatcher := CreateMatcher(Regex, True);
      if not FMatcher.ParseOk then
        FreeAndNil(FMatcher);
    end;
  end;
end;

function TJpReMatch.TryMatch: Boolean;
var
  Input: string = '';
  Regex: string = '';
begin
  Result := False;
  if not IsStringInst(FArgumentList[0], Input) then exit;
  if IsStringInst(FArgumentList[1], Regex) and Utf8Validate(Regex) then begin
    if FMatcher = nil then
      FMatcher := CreateMatcher(Regex, False)
    else
      FMatcher.Expression := Regex;
    if FMatcher.ParseOk then
      Result := FMatcher.Match(Input);
  end;
end;

function TJpReMatch.TryMatchParsed: Boolean;
var
  Input: string = '';
begin
  if not IsStringInst(FArgumentList[0], Input) then exit(False);
  Result := FMatcher.Match(Input);
end;

procedure TJpReMatch.Eval(const aCtx: TJpFilterContext; var v: TJpInstance);
begin
  if LiteralRegex then begin
    if FMatcher = nil then begin
      v := False;
      exit;
    end;
    FArgumentList[0] := FParamList[0].GetValue(aCtx);
    v := TryMatchParsed;
  end else begin
    FArgumentList[0] := FParamList[0].GetValue(aCtx);
    FArgumentList[1] := FParamList[1].GetValue(aCtx);
    v := TryMatch;
  end;
end;

destructor TJpReMatch.Destroy;
begin
  FMatcher.Free;
  inherited;
end;

procedure TJpReMatch.AfterConstruction;
begin
  inherited;
  CheckRegexLiteral;
end;

{ TJpReSearch }

function TJpReSearch.CreateMatcher(const aRegex: string; aLiteral: Boolean): TIRegexp;
begin
  Result := TIRegexpSearch.Create(aRegex, aLiteral);
end;

{ TJpFilter }

procedure TJpFilter.SetRoot(aValue: TJsonNode);
begin
  FContext.Root := aValue;
end;

procedure TJpFilter.SetIndex(aValue: SizeInt);
begin
  FContext.Key.Kind := jkkIndex;
  FContext.Key.Index := aValue;
end;

procedure TJpFilter.SetName(const aValue: string);
begin
  FContext.Key.Kind := jkkName;
  FContext.Key.Name := aValue;
end;

procedure TJpFilter.SetOnGetParam(aValue: TJpOnGetParam);
begin
  FContext.OnGetParam := aValue;
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

function Utf8StrPosFast(pStart, pCurr: PByte): SizeInt;
begin
  Result := 0;
  while pStart < pCurr do
    begin
      case pStart^ of
        $c2..$df: pStart += 2;
        $e0..$ed: pStart += 3;
        $f0..$f4: pStart += 4;
      else
        Inc(pStart);
      end;
      Inc(Result);
    end;
end;

function TJpQueryParser.GetPos: SizeInt;
begin
  //Result := Succ(FLook - PAnsiChar(FQuery));
  Result := Succ(Utf8StrPosFast(PByte(FQuery), PByte(FLook)));
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
      #0..#31: Fail(SEJPathPosErrorFmt, [Position, Format(SEJPathIllegalStrCharFmt, [Ord(CurrChar)])]);
      '''': break;
      '\': begin
          SkipChar;
          QuoteNameEscape;
        end;
    else
      SkipChar;
    end;
  until Eof;
  if Eof then Fail(SEJPathPosErrorFmt, [Position, SEJPathEndQuoteMiss]);
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
      #0..#31: Fail(SEJPathPosErrorFmt, [Position, Format(SEJPathIllegalStrCharFmt, [Ord(CurrChar)])]);
      '"': break;
      '\': begin
          SkipChar;
          DblQuoteNameEscape;
        end;
    else
      SkipChar;
    end;
  until Eof;
  if Eof then Fail(SEJPathPosErrorFmt, [Position, SEJPathEndDblQuoteMiss]);
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
    begin
      if not(CurrChar in [',', ']']) then
        Fail(SEJPathPosExpectFmt, [Position, SEJPathCommaOrRB, CurrChar]);
      aSegment.AddSelector(Idx);
    end;
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
  if not(CurrChar in [',', ']']) then
    Fail(SEJPathPosExpectFmt, [Position, SEJPathCommaOrRB, CurrChar]);
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
      Fail(SEJPathPosErrorFmt, [Position, Format(SEJPathUnexpectTokenFmt, [TOKENS[CurrToken]])]);
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

function TJpQueryParser.CheckFunParam(aType: TJpInstanceType; aExpr: TJpExpression): Boolean;
begin
  Result := False;
  case aType of
    jitLogical:
      if aExpr is TJpFunctionExpr then
        Result := TJpFunctionExpr(aExpr).ResultType = jitLogical
      else
        Result := not(aExpr is TJpConstExpr);
    jitValue:
      Result := (aExpr is TJpRelQueryExpr) and TJpRelQueryExpr(aExpr).IsSingular or
                   (aExpr is TJpFunctionExpr) and (TJpFunctionExpr(aExpr).ResultType = jitValue) or
                   (aExpr is TJpConstExpr);
    jitNodeList:
      Result := aExpr is TJpRelQueryExpr;
  end;
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
  FunDef := FFunDef;
  FunName := FStrValue;
  SkipChar;
  NextToken;
  System.SetLength(ParamList, System.Length(FunDef.ParamDefs));
  I := 0;
  try
    while CurrToken <> jtkRParen do begin
      e := ExprLevel0(False);
      ParamList[I] := e;
      if I > System.High(FunDef.ParamDefs) then
        Fail(SEJPathPosErrorFmt, [Position, Format(SEJPathFunParamExtraFmt, [FunName])]);
      if not CheckFunParam(FunDef.ParamDefs[I], e) then
        Fail(SEJPathParamMismatchFmt, [Position, FunName, Succ(I)]);
      if I < System.High(ParamList) then
        if CurrToken <> jtkComma then
          Fail(SEJPathPosErrorFmt, [Position, Format(SEJPathUnexpectTokenFmt, [TOKENS[CurrToken]])])
        else begin
          SkipChar;
          NextToken;
        end;
      Inc(I);
    end;
    if I < System.Length(ParamList) then
      Fail(SEJPathPosErrorFmt, [Position, Format(SEJPathFunParamMissFmt, [FunName])]);
    case FunName of
      'key':    Result := TJpKeyFunction.Create(ParamList, nil, FunDef.ResultType);
      'match':  Result := TJpReMatch.Create(ParamList, nil, FunDef.ResultType);
      'param':  Result := TJpParamFunction.Create(ParamList, nil, FunDef.ResultType);
      'search': Result := TJpReSearch.Create(ParamList, nil, FunDef.ResultType);
    else
      Result := TJpFunctionExpr.Create(ParamList, FunDef.OnExecute, FunDef.ResultType);
    end;
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
  s: string;
  Done: Boolean = False;
begin
  Assert(CurrChar = '[');
  SkipCharThenWS;
  Segment := aPath.AddChildSegment;
  while not Done do begin
    CheckEof;
    case CurrChar of
      '''': begin
          s := GetQuoteName;
          if not(CurrChar in [',', ']']) then
            Fail(SEJPathPosExpectFmt, [Position, SEJPathCommaOrRB, CurrChar]);
          Segment.AddSelector(s);
        end;
      '"': begin
          s := GetDblQuoteName;
          if not(CurrChar in [',', ']']) then
            Fail(SEJPathPosExpectFmt, [Position, SEJPathCommaOrRB, CurrChar]);
          Segment.AddSelector(s);
        end;
      '*': begin
          SkipCharThenWS;
          if not(CurrChar in [',', ']']) then
            Fail(SEJPathPosExpectFmt, [Position, SEJPathCommaOrRB, CurrChar]);
          Segment.AddWildcard;
        end;
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

procedure TJpMatcher.SetParam(const aName: string; const aValue: TJpValue);
begin
  FParams.AddOrSetValue(aName, aValue);
end;

function TJpMatcher.GetParam(const aName: string): TJpValue;
begin
  if not FParams.TryGetValue(aName, Result) then
    Result := TJpValue.Nothing;
end;

function TJpMatcher.HasParam(const aName: string; out aParam: TJpValue): Boolean;
begin
  Result := FParams.TryGetValue(aName, aParam);
  if not Result then
    aParam := TJpValue.Nothing;
end;

function TJpMatcher.TryGetParam(const aName: string; out aValue: TJpValue): Boolean;
begin
  Result := FParams.TryGetValue(aName, aValue);
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
  if Result then
    FPath.OnGetParam := @TryGetParam
  else
    FPath.Clear;
end;

function TJpMatcher.Match(aRoot: TJsonNode): TJpNodeList;
begin
  Result := FPath.ApplyWithPath(aRoot);
end;

function TJpMatcher.Match(const aRoot: string; out aNodeList: TJpNodeList): Boolean;
var
  Root: specialize TGAutoRef<TJsonNode>;
  I: SizeInt;
begin
  aNodeList := nil;
  if not Root.Instance.Parse(aRoot) then exit(False);
  aNodeList := FPath.ApplyWithPath(Root.Instance);
  for I := 0 to System.High(aNodeList) do
    aNodeList[I].Value := aNodeList[I].Value.Clone;
  Result := True;
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

function TJpMatcher.MatchValues(const aRoot: string; out aNodeArray: TJpValueList): Boolean;
var
  Root: specialize TGAutoRef<TJsonNode>;
  I: SizeInt;
begin
  aNodeArray := nil;
  if not Root.Instance.Parse(aRoot) then exit(False);
  aNodeArray := FPath.Apply(Root.Instance);
  for I := 0 to System.High(aNodeArray) do
    aNodeArray[I] := aNodeArray[I].Clone;
  Result := True;
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

function TJpMatcher.MatchFirst(const aRoot: string; out aNode: TJpNode): Boolean;
var
  Root: specialize TGAutoRef<TJsonNode>;
begin
  if not Root.Instance.Parse(aRoot) then
    begin
      aNode.Path := '';
      aNode.Value := nil;
      exit(False);
    end;
  aNode := FPath.GetFirstWithPath(Root.Instance);
  if aNode.Value <> nil then
    aNode.Value := aNode.Value.Clone;
  Result := True;
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

function TJpMatcher.MatchFirstValue(const aRoot: string; out aValue: TJsonNode): Boolean;
var
  Root: specialize TGAutoRef<TJsonNode>;
  Node: TJsonNode;
begin
  aValue := nil;
  if not Root.Instance.Parse(aRoot) then exit(False);
  Node := FPath.GetFirst(Root.Instance);
  if Node <> nil then
    aValue := Node.Clone;
  Result := True;
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

const
  LETTER_CATEGORIES =
    [TUnicodeCategory.ucUppercaseLetter, TUnicodeCategory.ucLowercaseLetter,
     TUnicodeCategory.ucTitlecaseLetter, TUnicodeCategory.ucModifierLetter,
     TUnicodeCategory.ucOtherLetter];

  MARK_CATEGORIES =
    [TUnicodeCategory.ucNonSpacingMark, TUnicodeCategory.ucCombiningMark, TUnicodeCategory.ucEnclosingMark];

  NUMBER_CATEGORIES =
    [TUnicodeCategory.ucDecimalNumber, TUnicodeCategory.ucLetterNumber, TUnicodeCategory.ucOtherNumber];

  PUNCTUATION_CATEGORIES =
    [TUnicodeCategory.ucConnectPunctuation, TUnicodeCategory.ucDashPunctuation,
     TUnicodeCategory.ucOpenPunctuation, TUnicodeCategory.ucClosePunctuation,
     TUnicodeCategory.ucInitialPunctuation, TUnicodeCategory.ucFinalPunctuation,
     TUnicodeCategory.ucOtherPunctuation];

  SEPARATOR_CATEGORIES =
    [TUnicodeCategory.ucSpaceSeparator, TUnicodeCategory.ucLineSeparator, TUnicodeCategory.ucParagraphSeparator];

  SYMBOL_CATEGORIES =
    [TUnicodeCategory.ucMathSymbol, TUnicodeCategory.ucCurrencySymbol,
     TUnicodeCategory.ucModifierSymbol, TUnicodeCategory.ucOtherSymbol];

  OTHER_CATEGORIES =
    [TUnicodeCategory.ucControl, TUnicodeCategory.ucFormat,
     TUnicodeCategory.ucPrivateUse, TUnicodeCategory.ucUnassigned];

  UNICODE_BAD_CHAR = $fffd;

function CpToUcs4Char(var p: PByte; aStrLen: SizeInt): Ucs4Char; inline;
begin
  case p^ of
    0..$7f: begin
        Inc(p);
        exit(p[-1]);
      end;
    $c2..$df:
      if (aStrLen > 1) and (p[1] in [$80..$bf]) then begin
        p += 2;
        exit(Ucs4Char(Ucs4Char(p[-2] and $1f) shl 6 or Ucs4Char(p[-1] and $3f)));
      end;
    $e0:
      if (aStrLen > 2) and (p[1] in [$a0..$bf]) and (p[2] in [$80..$bf]) then begin
        p += 3;
        exit(Ucs4Char(Ucs4Char(p[-3]and $f)shl 12 or Ucs4Char(p[-2]and $3f)shl 6 or Ucs4Char(p[-1] and $3f)));
      end;
    $e1..$ec, $ee..$ef:
      if (aStrLen > 2) and (p[1] in [$80..$bf]) and (p[2] in [$80..$bf]) then begin
        p += 3;
        exit(Ucs4Char(Ucs4Char(p[-3]and $f)shl 12 or Ucs4Char(p[-2]and $3f)shl 6 or Ucs4Char(p[-1]and $3f)));
      end;
    $ed:
      if (aStrLen > 2) and (p[1] in [$80..$9f]) and (p[2] in [$80..$bf]) then begin
        p += 3;
        exit(Ucs4Char(Ucs4Char(p[-3]and $f)shl 12 or Ucs4Char(p[-2]and $3f)shl 6 or Ucs4Char(p[-1]and $3f)));
      end;
    $f0:
      if(aStrLen > 3)and(p[1]in[$90..$bf])and(p[2]in[$80..$bf])and(p[3]in[$80..$bf])then begin
        p += 4;
        exit(Ucs4Char(Ucs4Char(p[-4] and $7) shl 18 or Ucs4Char(p[-3] and $3f) shl 12 or
             Ucs4Char(p[-2] and $3f) shl 6 or Ucs4Char(p[-1] and $3f)));
      end;
    $f1..$f3:
      if(aStrLen > 3)and(p[1]in[$80..$bf])and(p[2]in[$80..$bf])and(p[3]in[$80..$bf])then begin
        p += 4;
        exit(Ucs4Char(Ucs4Char(p[-4] and $7) shl 18 or Ucs4Char(p[-3] and $3f) shl 12 or
             Ucs4Char(p[-2] and $3f) shl 6 or Ucs4Char(p[-1] and $3f)));
      end;
    $f4:
      if(aStrLen > 3)and(p[1]in[$80..$8f])and(p[2]in[$80..$bf])and(p[3]in[$80..$bf])then begin
        p += 4;
        exit(Ucs4Char(Ucs4Char(p[-4] and $7) shl 18 or Ucs4Char(p[-3] and $3f) shl 12 or
             Ucs4Char(p[-2] and $3f) shl 6 or Ucs4Char(p[-1] and $3f)));
      end;
  else
  end;
  Inc(p);
  Result := UNICODE_BAD_CHAR;
end;

{ TIRegexp.TUCategory }

class function TIRegexp.TUCategory.GetCategory(c: Ucs4Char): TUnicodeCategory;
begin
  if c <= UC_TBL_HIGH then
    Result := TUnicodeCategory(UC_CATEGORY_TBL[c])
  else
    Result := TUnicodeCategory(UnicodeData.GetProps(c)^.Category);
end;

constructor TIRegexp.TUCategory.Make(aKind: TUCategoryKind; aComplement: Boolean);
begin
  FKind := aKind;
  FComplement := aComplement;
end;

constructor TIRegexp.TUCategory.Make(c: TUnicodeCategory; aComplement: Boolean);
begin
  FKind := uckCategory;
  FCategory := c;
  FComplement := aComplement;
end;

{$PUSH}{$WARN 5059 OFF}
function TIRegexp.TUCategory.Match(c: Ucs4Char): Boolean;
begin
  case FKind of
    uckCategory:    Result := (GetCategory(c) = FCategory) xor FComplement;
    uckLetter:      Result := (GetCategory(c) in LETTER_CATEGORIES) xor FComplement;
    uckMark:        Result := (GetCategory(c) in MARK_CATEGORIES) xor FComplement;
    uckNumber:      Result := (GetCategory(c) in NUMBER_CATEGORIES) xor FComplement;
    uckPunctuation: Result := (GetCategory(c) in PUNCTUATION_CATEGORIES) xor FComplement;
    uckSeparator:   Result := (GetCategory(c) in SEPARATOR_CATEGORIES) xor FComplement;
    uckSymbol:      Result := (GetCategory(c) in SYMBOL_CATEGORIES) xor FComplement;
    uckOther:       Result := (GetCategory(c) in OTHER_CATEGORIES) xor FComplement;
  end;
end;
{$POP}

{ TIRegexp.TCceItem }

constructor TIRegexp.TCceItem.Make(c: TUCategory);
begin
  Kind := cikCategory;
  UCategory := c;
end;

constructor TIRegexp.TCceItem.Make(c: Ucs4Char);
begin
  Kind := cikChar;
  UChar := c;
end;

constructor TIRegexp.TCceItem.Make(aLo, aHi: Ucs4Char);
begin
  Kind := cikCharRange;
  CLow := aLo;
  CHigh := aHi;
end;

{$PUSH}{$WARN 5059 OFF}
function TIRegexp.TCceItem.Match(c: Ucs4Char): Boolean;
begin
  case Kind of
    cikChar:      Result := UChar = c;
    cikCategory:  Result := UCategory.Match(c);
    cikCharRange: Result := (c >= CLow) and (c <= CHigh);
  end;
end;
{$POP}

{ TIRegexp.TCCStore }

procedure TIRegexp.TCCStore.Expand;
begin
  FItems.Length := FItems.Length * 2;
end;

function TIRegexp.TCCStore.GetItems: PCceItem;
begin
  Result := FItems.Ptr;
end;

procedure TIRegexp.TCCStore.Reset;
begin
  if FItems.IsEmpty then
    FItems.Length := INIT_LEN;
  FCount := 0;
end;

procedure TIRegexp.TCCStore.TrimToFit;
begin
  FItems.Length := Count;
end;

function TIRegexp.TCCStore.Add(const aValue: TCceItem): Integer;
begin
  if Count = FItems.Length then Expand;
  Result := Count;
  FItems.Ptr[Count] := aValue;
  Inc(FCount);
end;

{ TIRegexp.TCharClassExpr }

procedure TIRegexp.TCharClassExpr.Init(aStore: PCCStore; aComplement: Boolean);
begin
  FStore := aStore;
  FFirst := NULL_INDEX;
  FLast := NULL_INDEX;
  FComplement := aComplement;
end;

procedure TIRegexp.TCharClassExpr.Add(const aItem: TCceItem);
begin
  FLast := FStore^.Add(aItem);
  if FFirst = NULL_INDEX then
    FFirst := FLast;
end;

function TIRegexp.TCharClassExpr.Match(c: Ucs4Char): Boolean;
var
  I: Integer;
  p: PCceItem;
begin
  p := FStore^.Items;
  if FComplement then
    begin
      for I := FFirst to FLast do
        if p[I].Match(c) then exit(False);
      Result := True;
    end
  else
    begin
      for I := FFirst to FLast do
        if p[I].Match(c) then exit(True);
      Result := False;
    end;
end;

{ TIRegexp.TCharClass }

constructor TIRegexp.TCharClass.Make(aKind: TCharClassKind);
begin
  Kind := aKind;
end;

constructor TIRegexp.TCharClass.Make(c: Ucs4Char);
begin
  Kind := cckChar;
  VChar := c;
end;

constructor TIRegexp.TCharClass.Make(const c: TUCategory);
begin
  Kind := cckCategory;
  VCategory := c;
end;

constructor TIRegexp.TCharClass.Make(const e: TCharClassExpr);
begin
  Kind := cckClassExpr;
  VExpr := e;
end;

{$PUSH}{$WARN 5059 OFF}
function TIRegexp.TCharClass.Match(c: Ucs4Char): Boolean;
begin
  case Kind  of
    cckAnyChar:   Result := True;
    cckWildcard:  Result := (c <> 10) and (c <> 13);
    cckChar:      Result := VChar = c;
    cckCategory:  Result := VCategory.Match(c);
    cckClassExpr: Result := VExpr.Match(c);
  end;
end;
{$POP}

{ TIRegexp.TNfaNode }

function TIRegexp.TNfaNode.Match(c: Ucs4Char): Boolean;
begin
  Result := Matcher.Match(c);
end;

{ TIRegexp.TNfaTable }

procedure TIRegexp.TNfaTable.Expand;
begin
  FItems.Length := FItems.Length * 2;
end;

function TIRegexp.TNfaTable.GetItem(aIndex: Integer): PNfaNode;
begin
  Result := @FItems.Ptr[aIndex];
end;

procedure TIRegexp.TNfaTable.Reset;
begin
  if FItems.IsEmpty then
    FItems.Length := INIT_LEN;
  FCount := 0;
end;

procedure TIRegexp.TNfaTable.TrimToFit;
begin
  FItems.Length := Count;
end;

procedure TIRegexp.TNfaTable.AddRange(aCount: Integer);
var
  I: Integer;
begin
  if aCount < 1 then exit;
  aCount += Count;
  if FItems.Length < aCount then
    FItems.Length := LgUtils.RoundUpTwoPower(aCount);
  for I := Count to Pred(aCount) do
    FItems.Ptr[I].Step := 0;
  FCount := aCount;
end;

function TIRegexp.TNfaTable.AddNode: Integer;
begin
  if Count = FItems.Length then Expand;
  Result := Count;
  FItems.Ptr[Count].Step := 0;
  Inc(FCount);
end;

procedure TIRegexp.TNfaTable.Shrink;
var
  I: Integer;
  Item: PNfaNode;
begin
  Item := FItems.Ptr;
  for I := 0 to Pred(Count) do
    with Item[I] do
      case Kind of
        nkSplit: begin
            while Item[Next1].Kind = nkMove do
              Next1 := Item[Next1].Next1;
            while Item[Next2].Kind = nkMove do
              Next2 := Item[Next2].Next1;
          end;
        nkMatch:
          while Item[Next1].Kind = nkMove do
            Next1 := Item[Next1].Next1;
      else
      end;
end;

procedure TIRegexp.TNfaTable.Renumber(var aNewIndex: TIntDynArray);
var
  I, J: Integer;
  Item: PNfaNode;
begin
  aNewIndex.Length := Count;
  Item := FItems.Ptr;
  J := 0;
  for I := 0 to Pred(Count) do begin
    aNewIndex[I] := J;
    if Item[I].Kind = nkMove then continue;
    if I <> J then
      Item[J] := Item[I];
    Inc(J);
  end;
  FCount := J;
  for I := 0 to Pred(J) do
    with Item[I] do
      case Kind of
        nkSplit: begin
            Next1 := aNewIndex[Next1];
            Next2 := aNewIndex[Next2];
          end;
        nkMatch: Next1 := aNewIndex[Next1];
      else
      end;
end;

{ TIRegexp.TReStack }

procedure TIRegexp.TReStack.Expand;
begin
  FItems.Length := FItems.Length * 2;
end;

function TIRegexp.TReStack.Reset: PReStack;
begin
  if FItems.Length = 0 then
    FItems.Length := INIT_LEN;
  FCount := 0;
  Result := @Self;
end;

function TIRegexp.TReStack.NonEmpty: Boolean;
begin
  Result := Count <> 0;
end;

procedure TIRegexp.TReStack.Push(aValue: Integer);
begin
  if Count = FItems.Length then Expand;
  FItems.Ptr[Count] := aValue;
  Inc(FCount);
end;

function TIRegexp.TReStack.TryPop(out aValue: Integer): Boolean;
begin
  if Count = 0 then exit(False);
  aValue := FItems.Ptr[Pred(Count)];
  Dec(FCount);
  Result := True;
end;

{ TIRegexp }

function TIRegexp.Fail(const aMessage: string): Boolean;
begin
  FMessage := aMessage;
  Result := False;
end;

function TIRegexp.Eof: Boolean;
begin
  Result := FLook >= FExprEnd;
end;

procedure TIRegexp.SkipChar;
begin
  Inc(FLook);
end;

function TIRegexp.CurrChar: AnsiChar;
begin
  Result := FLook^;
end;

function TIRegexp.NextChar: AnsiChar;
begin
  Result := FLook[1];
end;

function TIRegexp.AddFinal: Integer;
begin
  Result := FTable.AddNode;
  with FTable[Result]^ do
    begin
      Kind := nkFinal;
      Next1 := DISABLE_MOVE;
      Next2 := DISABLE_MOVE;
    end;
end;

function TIRegexp.AddMatch(const aClass: TCharClass; out aFinal: Integer): Integer;
begin
  Result := FTable.AddNode;
  aFinal := AddFinal;
  with FTable[Result]^ do
    begin
      Kind := nkMatch;
      Matcher := aClass;
      Next1 := aFinal;
      Next2 := DISABLE_MOVE;
    end;
end;

function TIRegexp.AddMove(out aFinal: Integer): Integer;
begin
  Result := FTable.AddNode;
  aFinal := AddFinal;
  with FTable[Result]^ do
    begin
      Kind := nkMove;
      Next1 := aFinal;
      Next2 := DISABLE_MOVE;
    end;
end;

function TIRegexp.AddSplit(aNext1, aNext2: Integer): Integer;
begin
  Result := FTable.AddNode;
  with FTable[Result]^ do
    begin
      Kind := nkSplit;
      Next1 := aNext1;
      Next2 := aNext2;
    end;
end;

procedure TIRegexp.Patch(aNode: Integer; aKind: TNodeKind; aNext1: Integer; aNext2: Integer);
begin
  with FTable[aNode]^ do
    begin
      Kind := aKind;
      Next1 := aNext1;
      Next2 := aNext2;
    end;
end;

function TIRegexp.GetChar: Ucs4Char;
begin
  Result := CpToUcs4Char(PByte(FLook), FExprEnd - FLook);
end;

function TIRegexp.ParseNormalChar(out c: Ucs4Char): Boolean;
begin
  if Eof then exit(Fail(SEIreUnexpectEnd));
  if CurrChar in META_CHARS then exit(Fail(SEIreExpectNormalChar));
  c := GetChar;
  Result := True;
end;

function TIRegexp.GetNormalChar(out cc: TCharClass): Boolean;
var
  c: Ucs4Char;
begin
  if not ParseNormalChar(c) then exit(False);
  cc.Make(c);
  Result := True;
end;

function TIRegexp.ParseUCategory(out uc: TUCategory): Boolean;
var
  c: AnsiChar;
  IsComplement: Boolean;
begin
  Assert(CurrChar in ['P', 'p']);
  IsComplement := CurrChar = 'P';
  SkipChar;
  if Eof or (CurrChar <> '{') then exit(Fail(SEIreInvalidUCategory));
  SkipChar;
  if not(CurrChar in UCATEGORY_CHARS) then exit(Fail(SEIreInvalidUCategory));
  c := CurrChar;
  SkipChar;
  if Eof then exit(Fail(SEIreRBraceMiss));
  if CurrChar = '}' then begin
    SkipChar;
    case c of
      'C': uc.Make(uckOther, IsComplement);
      'L': uc.Make(uckLetter, IsComplement);
      'M': uc.Make(uckMark, IsComplement);
      'N': uc.Make(uckNumber, IsComplement);
      'P': uc.Make(uckPunctuation, IsComplement);
      'S': uc.Make(uckSymbol, IsComplement);
    else //'Z'
      uc.Make(uckSeparator, IsComplement);
    end;
    exit(True);
  end;
  case c of
    'C':
      case CurrChar of
        'c': uc.Make(TUnicodeCategory.ucControl, IsComplement);
        'f': uc.Make(TUnicodeCategory.ucFormat, IsComplement);
        'n': uc.Make(TUnicodeCategory.ucUnassigned, IsComplement);
        'o': uc.Make(TUnicodeCategory.ucPrivateUse, IsComplement);
      else
        exit(Fail(SEIreInvalidUCategory));
      end;
    'L':
      case CurrChar of
        'l': uc.Make(TUnicodeCategory.ucLowercaseLetter, IsComplement);
        'm': uc.Make(TUnicodeCategory.ucModifierLetter, IsComplement);
        'o': uc.Make(TUnicodeCategory.ucOtherLetter, IsComplement);
        't': uc.Make(TUnicodeCategory.ucTitlecaseLetter, IsComplement);
        'u': uc.Make(TUnicodeCategory.ucUppercaseLetter, IsComplement);
      else
        exit(Fail(SEIreInvalidUCategory));
      end;
    'M':
      case CurrChar of
        'c': uc.Make(TUnicodeCategory.ucCombiningMark, IsComplement);
        'e': uc.Make(TUnicodeCategory.ucEnclosingMark, IsComplement);
        'n': uc.Make(TUnicodeCategory.ucNonSpacingMark, IsComplement);
      else
        exit(Fail(SEIreInvalidUCategory));
      end;
    'N':
      case CurrChar of
        'd': uc.Make(TUnicodeCategory.ucDecimalNumber, IsComplement);
        'l': uc.Make(TUnicodeCategory.ucLetterNumber, IsComplement);
        'o': uc.Make(TUnicodeCategory.ucOtherNumber, IsComplement);
      else
        exit(Fail(SEIreInvalidUCategory));
      end;
    'P':
      case CurrChar of
        'c': uc.Make(TUnicodeCategory.ucConnectPunctuation, IsComplement);
        'd': uc.Make(TUnicodeCategory.ucDashPunctuation, IsComplement);
        'e': uc.Make(TUnicodeCategory.ucClosePunctuation, IsComplement);
        'f': uc.Make(TUnicodeCategory.ucFinalPunctuation, IsComplement);
        'i': uc.Make(TUnicodeCategory.ucInitialPunctuation, IsComplement);
        'o': uc.Make(TUnicodeCategory.ucOtherPunctuation, IsComplement);
        's': uc.Make(TUnicodeCategory.ucOpenPunctuation, IsComplement);
      else
        exit(Fail(SEIreInvalidUCategory));
      end;
    'S':
      case CurrChar of
        'c': uc.Make(TUnicodeCategory.ucCurrencySymbol, IsComplement);
        'k': uc.Make(TUnicodeCategory.ucModifierSymbol, IsComplement);
        'm': uc.Make(TUnicodeCategory.ucMathSymbol, IsComplement);
        'o': uc.Make(TUnicodeCategory.ucOtherSymbol, IsComplement);
      else
        exit(Fail(SEIreInvalidUCategory));
      end;
    'Z':
      case CurrChar of
        'l': uc.Make(TUnicodeCategory.ucLineSeparator, IsComplement);
        'p': uc.Make(TUnicodeCategory.ucParagraphSeparator, IsComplement);
        's': uc.Make(TUnicodeCategory.ucSpaceSeparator, IsComplement);
      else
        exit(Fail(SEIreInvalidUCategory));
      end;
  else
  end;
  SkipChar;
  if Eof or (CurrChar <> '}') then exit(Fail(SEIreRBraceMiss));
  SkipChar;
  Result := True;
end;

function TIRegexp.GetUCategory(out cc: TCharClass): Boolean;
var
  uc: TUCategory;
begin
  if not ParseUCategory(uc) then exit(False);
  cc.Make(uc);
  Result := True;
end;

function TIRegexp.ParseCharEsc(out c: Ucs4Char): Boolean; //SingleCharEsc
begin
  if Eof then exit(Fail(SEIreUnexpectEnd));
  if not(CurrChar in ESCAPABLE_CHARS) then exit(Fail(SEIreExpectEscapable));
  case CurrChar of
    'n': begin
        SkipChar;
        c := 10;
      end;
    'r': begin
        SkipChar;
        c := 13;
      end;
    't': begin
        SkipChar;
        c := 9;
      end;
  else
    c := GetChar;
  end;
  Result := True;
end;

function TIRegexp.GetCharEsc(out cc: TCharClass): Boolean;
var
  c: Ucs4Char;
begin
  if not ParseCharEsc(c) then exit(False);
  cc.Make(c);
  Result := True;
end;

function TIRegexp.ParseCcChar(out c: Ucs4Char): Boolean; //CCchar
begin
  if Eof then exit(Fail(SEIreRBracketMiss));
  if CurrChar = '\' then begin
    SkipChar;
    Result := ParseCharEsc(c);
  end else begin
    if not(CurrChar in CC_CHARS) then exit(Fail(SEIreExpectCCChar));
    c := GetChar;
    Result := True;
  end;
end;

function TIRegexp.ParseCceItem(out aItem: TCceItem): Boolean; //CCE1
var
  Lo, Hi: Ucs4Char;
  uc: TUCategory;
begin
  case CurrChar of
    '\': begin
        if NextChar in ['P', 'p'] then begin
          SkipChar;
          if not ParseUCategory(uc) then exit(False);
          aItem.Make(uc);
          exit(True);
        end else
          if not ParseCcChar(Lo) then exit(False);
      end;
    '-':
      if NextChar = ']' then begin
        SkipChar;
        aItem.Make(Ucs4Char('-'));
        exit(True);
      end else
        exit(Fail(SEIreExpectCCChar));
  else
    if not ParseCcChar(Lo) then exit(False);
  end;
  if (CurrChar = '-') and (NextChar <> ']') then begin
      SkipChar;
      if not ParseCcChar(Hi) then exit(False);
      if Hi < Lo then exit(Fail(SEIreInvalidCharRange));
      aItem.Make(Lo, Hi);
  end else
    aItem.Make(Lo);
  Result := True;
end;

{$PUSH}{$WARN 5036 OFF}
function TIRegexp.ParseCharClassExpr(out cc: TCharClass): Boolean;
var
  e: TCharClassExpr;
  i: TCceItem;
begin
  if Eof then exit(Fail(SEIreRBracketMiss));
  if CurrChar = ']' then exit(Fail(SEIreEmptyCharClassExpr));
  if CurrChar = '^' then begin
    SkipChar;
    if CurrChar = ']' then exit(Fail(SEIreEmptyCharClassExpr));
    e.Init(@FCCStore, True);
  end else
    e.Init(@FCCStore);
  if CurrChar = '-' then begin
    e.Add(TCceItem.Make(Ucs4Char('-')));
    SkipChar;
  end;
  while CurrChar <> ']' do begin
    if Eof then exit(Fail(SEIreRBracketMiss));
    if not ParseCceItem(i) then exit(False);
    e.Add(i);
  end;
  SkipChar;
  cc.Make(e);
  Result := True;
end;
{$POP}

function TIRegexp.ParseAtom(var aStart, aFinal: Integer): Boolean;
var
  cc: TCharClass;
begin
  if FDepth = MAX_REC_DEPTH then exit(Fail(SEIreMaxDepthExceed));
  Inc(FDepth);

  if Eof then // empty atom
    aStart := AddMove(aFinal)
  else
    case CurrChar of
      '|': aStart := AddMove(aFinal);// empty atom
      '(': begin
          SkipChar;
          if Eof then exit(Fail(SEIreRParenMiss));
          if CurrChar = ')' then begin // empty expression
            SkipChar;
            aStart := AddMove(aFinal);
          end else begin
            if not ParseExpr(aStart, aFinal) then exit(False);
            if CurrChar <> ')' then exit(Fail(SEIreRParenMiss));
            SkipChar;
          end;
        end;
      '.':  begin
          SkipChar;
          aStart := AddMatch(TCharClass.Make(cckWildcard), aFinal);
        end;
      '[':  begin
          SkipChar;
          if not ParseCharClassExpr(cc) then exit(False);
          aStart := AddMatch(cc, aFinal);
        end;
      '\': begin
          SkipChar;
          if CurrChar in ['P', 'p'] then begin
            if not GetUCategory(cc) then exit(False);
            aStart := AddMatch(cc, aFinal)
          end else begin
            if not GetCharEsc(cc) then exit(False);
            aStart := AddMatch(cc, aFinal);
          end;
        end;
    else
      if not GetNormalChar(cc) then exit(False);
      aStart := AddMatch(cc, aFinal);
    end;

  Dec(FDepth);
  Result := True;
end;

procedure TIRegexp.MakeQuestion(var aStart, aFinal: Integer);
begin
  aStart := AddSplit(aStart, aFinal);
end;

procedure TIRegexp.MakeStar(var aStart, aFinal: Integer);
var
  Fin: Integer;
begin
  Fin := AddFinal;
  Patch(aFinal, nkSplit, Fin, aStart);
  aStart := aFinal;
  aFinal := Fin;
end;

procedure TIRegexp.MakePlus(var aStart, aFinal: Integer);
var
  Fin: Integer;
begin
  Fin := AddFinal;
  Patch(aFinal, nkSplit, Fin, aStart);
  aFinal := Fin;
end;

procedure TIRegexp.CopyFragment(var aStart, aFinal: Integer; aFrom, aCount: Integer);
var
  I: Integer;
begin
  if aCount < 1 then exit;
  FTable.AddRange(aCount);
  for I := aFrom to Pred(aFrom + aCount) do
    with FTable[I]^ do
      case Kind of
        nkSplit: Patch(I + aCount, Kind, Next1 + aCount, Next2 + aCount);
        nkMove:  Patch(I + aCount, Kind, Next1 + aCount);
        nkMatch: begin
            Patch(I + aCount, Kind, Next1 + aCount);
            FTable[I + aCount]^.Matcher := Matcher;
          end;
        nkFinal: Patch(I + aCount, Kind, DISABLE_MOVE);
      end;
  aStart += aCount;
  aFinal += aCount;
end;

{$PUSH}{$MACRO ON}
{$DEFINE AppendCopyFragMacro :=
  Fin := aFinal;
  CopyFragment(aStart, aFinal, aFragStart, Len);
  Patch(Fin, nkMove, aStart);
  aFragStart += Len
}
procedure TIRegexp.MakeQuantRange(var aStart, aFinal: Integer; aMin, aMax, aFragStart: Integer);
var
  I, First, Fin, Len: Integer;
begin
  case aMin of
    0:
      case aMax of
        Q_RANGE_MAX_ANY:
          MakeStar(aStart, aFinal);
        Q_RANGE_EXACT, 0:
          Patch(aStart, nkMove, aFinal); // discard fragment ???
        1: MakeQuestion(aStart, aFinal);
      else
        MakeQuestion(aStart, aFinal);
        First := aStart;
        Len := FTable.Count - aFragStart;
        for I := 1 to Pred(aMax) do begin
          AppendCopyFragMacro;
        end;
        aStart := First;
      end;
    1:
      case aMax of
        Q_RANGE_MAX_ANY:
          MakePlus(aStart, aFinal);
        Q_RANGE_EXACT, 1: ; // just ignore
      else
        First := aStart;
        Len := FTable.Count - aFragStart;
        Fin := aFinal;
        CopyFragment(aStart, aFinal, aFragStart, Len);
        aFragStart += Len;
        MakeQuestion(aStart, aFinal);
        Patch(Fin, nkMove, aStart);
        Len := FTable.Count - aFragStart;
        for I := 1 to aMax - 2 do begin
          AppendCopyFragMacro;
        end;
        aStart := First;
      end;
  else
    First := aStart;
    Len := FTable.Count - aFragStart;
    for I := 1 to Pred(aMin) do begin
      AppendCopyFragMacro;
    end;
    if (aMin = aMax) or (aMax = Q_RANGE_EXACT) then begin
      aStart := First;
      exit;
    end;
    Fin := aFinal;
    if aMax = Q_RANGE_MAX_ANY then
      MakePlus(aStart, aFinal)
    else begin
      MakeQuestion(aStart, aFinal);
      Patch(Fin, nkMove, aStart);
      Len := FTable.Count - aFragStart;
      for I := 1 to aMax - aMin do begin
        AppendCopyFragMacro;
      end;
    end;
    aStart := First;
  end;
end;
{$UNDEF AppendCopyFragMacro}{$POP}

function TIRegexp.ParseQuantRange(var aStart, aFinal: Integer; aFragStart: Integer): Boolean;
var
  NumStart: PChar;
  VMin, VMax: Integer;
begin
  SkipChar;
  if not(CurrChar in DIGITS) then exit(Fail(SEIreInvalidRangeQuant));
  NumStart := FLook;
  repeat
    SkipChar;
  until not(CurrChar in DIGITS);
  if not Str2Int(NumStart, FLook - NumStart, VMin) then exit(Fail(SEIreRangeQuantTooBig));
  if VMin > Q_RANGE_LIMIT then exit(Fail(SEIreRangeQuantTooBig));
  if Eof then exit(Fail(SEIreRBraceMiss));
  case CurrChar of
    '}': begin
        SkipChar;
        VMax := Q_RANGE_EXACT;
      end;
    ',':
      begin
        SkipChar;
        if Eof then exit(Fail(SEIreRBraceMiss));
        if CurrChar in DIGITS then begin
          NumStart := FLook;
          repeat
            SkipChar;
          until not(CurrChar in DIGITS);
          if not Str2Int(NumStart, FLook - NumStart, VMax) then exit(Fail(SEIreRangeQuantTooBig));
          if VMax > Q_RANGE_LIMIT then exit(Fail(SEIreRangeQuantTooBig));
          if VMin > VMax then exit(Fail(SEIreRangeQuantMess));
          if Eof or (CurrChar <> '}') then exit(Fail(SEIreRBraceMiss));
          SkipChar;
        end else begin
          if CurrChar <> '}' then exit(Fail(SEIreRBraceMiss));
          SkipChar;
          VMax := Q_RANGE_MAX_ANY;
        end;
      end;
  else
    exit(Fail(SEIreInvalidRangeQuant));
  end;
  MakeQuantRange(aStart, aFinal, VMin, VMax, aFragStart);
  Result := True;
end;

function TIRegexp.ParseQuantifier(var aStart, aFinal: Integer; aFragStart: Integer): Boolean;
begin
  if not Eof then
    case CurrChar of
      '?': begin
          SkipChar;
          MakeQuestion(aStart, aFinal);
        end;
      '*': begin
          SkipChar;
          MakeStar(aStart, aFinal);
        end;
      '+': begin
          SkipChar;
          MakePlus(aStart, aFinal);
        end;
      '{': exit(ParseQuantRange(aStart, aFinal, aFragStart));
    else
    end;
  Result := True;
end;

function TIRegexp.ParseBranch(var aStart, aFinal: Integer): Boolean;
var
  Start, Fin: Integer;
  FragStart: Integer;
begin
  if FDepth = MAX_REC_DEPTH then exit(Fail(SEIreMaxDepthExceed));
  Inc(FDepth);

  FragStart := FTable.Count;
  if not ParseAtom(aStart, aFinal) then exit(False);
  if not ParseQuantifier(aStart, aFinal, FragStart) then exit(False);
  if not Eof and (CurrChar in (NORMAL_CHARS + ['(', '.', '[', '\'])) then begin
    Start := aStart;
    Fin := aFinal;
    if not ParseBranch(aStart, aFinal) then exit(False);
    Patch(Fin, nkMove, aStart);
    aStart := Start;
  end;

  Dec(FDepth);
  Result := True;
end;

function TIRegexp.ParseExpr(var aStart, aFinal: Integer): Boolean;
var
  Start, Fin: Integer;
begin
  if FDepth = MAX_REC_DEPTH then exit(Fail(SEIreMaxDepthExceed));
  Inc(FDepth);

  if not ParseBranch(aStart, aFinal) then exit(False);
  if CurrChar = '|' then begin
    SkipChar;
    if CurrChar <> ')' then begin
      Start := aStart;
      Fin := aFinal;
      if not ParseExpr(aStart, aFinal) then exit(False);
      aStart := AddSplit(Start, aStart);
      Patch(Fin, nkMove, aFinal);
    end;
  end;

  Dec(FDepth);
  Result := True;
end;

function TIRegexp.TryParse: Boolean;
begin
  FLook := Pointer(FExpression);
  FExprEnd := FLook + System.Length(FExpression);
  FParseOk := ParseExpr(FStartNode, FFinalNode);
  if ParseOk and not Eof then begin
    FParseOk := False;
    FMessage := SEIreTrailGarbage;
  end;
  Result := FParseOk;
end;

procedure TIRegexp.Parse;
begin
  if Expression = '' then begin
    FParseOk := True;
    exit;
  end;
  if not(FLiteralExpr or Utf8Validate(Expression)) then begin
    FMessage := SEIreBadExprEncoding;
    exit;
  end;
  FTable.Reset;
  FCCStore.Reset;
  try
    if TryParse then begin
      FTable.Shrink;
      if FLiteralExpr then begin
        RenumberNfa;
        TrimToFit;
      end;
    end;
  except
    on e: Exception do begin
      FParseOk := False;
      FMessage := Format(SEIreInternalErrorFmt, [e.ClassName, e.Message]);
    end;
  end;
end;

{$PUSH}{$WARN 5091 OFF}
procedure TIRegexp.RenumberNfa;
var
  NewIndex: TIntDynArray;
begin
  FTable.Renumber(NewIndex);
  FStartNode := NewIndex[FStartNode];
  FFinalNode := NewIndex[FFinalNode];
end;
{$POP}

procedure TIRegexp.TrimToFit;
begin
  FCCStore.TrimToFit;
  FTable.TrimToFit;
end;

procedure TIRegexp.SetExpression(const aValue: string);
begin
  if aValue = Expression then exit;
  FExpression := aValue;
  UniqueString(FExpression);
  Parse;
end;

procedure TIRegexp.PushEclose(aNode: Integer; aStack: PReStack);
begin
  with FTable[aNode]^ do begin
    if Step = FStep then exit;
    Step := FStep;
    if Kind = nkSplit then begin
      PushEclose(Next1, aStack);
      PushEclose(Next2, aStack);
    end else
      aStack^.Push(aNode);
  end;
end;

{$PUSH}{$Q-}{$R-}
class function TIRegexp.Str2Int(p: PAnsiChar; aCount: Integer; out aValue: Integer): Boolean;
var
  I: Integer;
begin
  if aCount < 1 then exit(False);
  I := 0;
  while (I < aCount) and (p[I] = '0') do
    Inc(I);
  if I = aCount then begin
    aValue := 0;
    exit(True);
  end else
    if aCount - I > MAX_QRANGE_LEN then
      exit(False);
  aValue := Ord(p[I]) - Ord('0');
  for I := Succ(I) to Pred(aCount) do
    aValue := aValue * 10 + Ord(p[I]) - Ord('0');
  Result := True;
end;
{$POP}

class procedure TIRegexp.PtrSwap(var L, R: Pointer);
var
  p: Pointer;
begin
  p := L;
  L := R;
  R := p;
end;

constructor TIRegexp.Create(const aExpr: string; aLiteral: Boolean);
begin
  inherited Create;
  FExpression := aExpr;
  UniqueString(FExpression);
  FLiteralExpr := aLiteral;
end;

procedure TIRegexp.AfterConstruction;
begin
  inherited;
  Parse;
end;

{$PUSH}{$WARN 5036 OFF}
function TIRegexp.Match(const aText: string): Boolean;
var
  pStack, pNextStack: PReStack;
  p, pEnd: PByte;
  I: Integer;
  c: Ucs4Char;
begin
  if Expression = '' then exit(aText = '');
  if not ParseOk then exit(False);
  pStack := FStack1.Reset;
  pNextStack := FStack2.Reset;
  p := PByte(aText);
  pEnd := p + System.Length(aText);
  Inc(FStep);
  PushEclose(FStartNode, pNextStack);
  while (p < pEnd) and pNextStack^.NonEmpty do begin
    PtrSwap(pStack, pNextStack);
    Inc(FStep);
    c := CpToUcs4Char(p, pEnd - p);
    while pStack^.TryPop(I) do
      with FTable[I]^ do
        if (Kind = nkMatch) and Match(c) then
          PushEclose(Next1, pNextStack);
  end;
  while pNextStack^.TryPop(I) do
    if FTable[I]^.Kind = nkFinal then exit(True);
  Result := False;
end;
{$POP}

{ TIRegexpSearch }

function TIRegexpSearch.TryParse: Boolean;
var
  Start, Fin: Integer;
begin
  Result := inherited;
  if Result then begin
    Start := AddMatch(TCharClass.Make(cckAnyChar), Fin);
    MakeStar(Start, Fin);
    Patch(Fin, nkMove, FStartNode);
    FStartNode := Start;
  end;
end;

{$PUSH}{$WARN 5036 OFF}
function TIRegexpSearch.Match(const aText: string): Boolean;
var
  pStack, pNextStack: PReStack;
  p, pEnd: PByte;
  I: Integer;
  c: Ucs4Char;
begin
  if Expression = '' then exit(True);
  if not ParseOk then exit(False);
  pStack := FStack1.Reset;
  pNextStack := FStack2.Reset;
  p := PByte(aText);
  pEnd := p + System.Length(aText);
  Inc(FStep);
  PushEclose(FStartNode, pNextStack);
  while (p < pEnd) and pNextStack^.NonEmpty do begin
    PtrSwap(pStack, pNextStack);
    Inc(FStep);
    c := CpToUcs4Char(p, pEnd - p);
    while pStack^.TryPop(I) do
      with FTable[I]^ do
        case Kind of
          nkMatch: if Match(c) then PushEclose(Next1, pNextStack);
          nkFinal: exit(True);
        else
        end;
  end;
  while pNextStack^.TryPop(I) do
    if FTable[I]^.Kind = nkFinal then exit(True);
  Result := False;
end;
{$POP}

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

function JpParseQuery(const aQuery: string): IJsonPath;
var
  Dummy: string;
begin
  Result := JpParseQuery(aQuery, Dummy);
end;

function JpParseQuery(const aQuery: string; out aPath: IJsonPath): Boolean;
var
  Dummy: string;
begin
  aPath := JpParseQuery(aQuery, Dummy);
  Result := aPath <> nil;
end;

function JpParseQuery(const aQuery: string; out aPath: IJsonPath; out aMsg: string): Boolean;
begin
  aPath := JpParseQuery(aQuery, aMsg);
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

function JpMatch(const aQuery, aRoot: string; out aList: TJpNodeList; out aMsg: string): Boolean;
var
  Matcher: specialize TGAutoRef<TJpMatcher>;
begin
  aMsg := '';
  aList := nil;
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

function JpMatch(const aQuery, aRoot: string; out aList: TJpNodeList): Boolean;
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

function JpMatchValues(const aQuery, aRoot: string; out aList: TJpValueList; out aMsg: string): Boolean;
var
  Matcher: specialize TGAutoRef<TJpMatcher>;
begin
  aMsg := '';
  aList := nil;
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

function JpMatchValues(const aQuery, aRoot: string; out aList: TJpValueList): Boolean;
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

function JpMatchFirst(const aQuery, aRoot: string; out aNode: TJpNode; out aMsg: string): Boolean;
var
  Matcher: specialize TGAutoRef<TJpMatcher>;
begin
  aMsg := '';
  aNode := TJpNode.Make('', nil);
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

function JpMatchFirst(const aQuery, aRoot: string; out aNode: TJpNode): Boolean;
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

function IRegexpCheck(const aRegex: string; out aMsg: string): Boolean;
begin
  with TIRegexp.Create(aRegex, False) do
    try
      Result := ParseOk;
      if not Result then
        aMsg := Message;
    finally
      Free;
    end;
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

{$PUSH}{$IF FPC_FULLVERSION>30300}{$WARN 6060 OFF}{$ENDIF}
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
{$POP}

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

{$PUSH}{$IF FPC_FULLVERSION>30300}{$WARN 6060 OFF}{$ENDIF}{$WARN 6058 OFF}{$WARN 5091 OFF}{$WARN 5036 OFF}
function IsNumberInst(const aInst: TJpInstance; out n: Double): Boolean; inline;
begin
  if aInst.InstType = jitValue then
    case aInst.Value.ValType of
      jvtNumber:
        begin
          n := aInst.Value.NumValue;
          exit(True);
        end;
      jvtNode:
        if aInst.Value.NodeValue.IsNumber then
          begin
            n := aInst.Value.NodeValue.AsNumber;
            exit(True);
          end;
    else
    end;
  Result := False;
end;

function IsBoolInst(const aInst: TJpInstance): Boolean; inline;
begin
  if aInst.InstType = jitValue then
    case aInst.Value.ValType of
      jvtFalse,
      jvtTrue: exit(True);
      jvtNode: exit(aInst.Value.NodeValue.IsBoolean);
    else
    end;
  Result := False;
end;
{ is_boolean() }
procedure CallIsBoolean(const aList: TJpParamList; out aResult: TJpInstance);
begin
  aResult := (System.Length(aList) = 1) and IsBoolInst(aList[0]);
end;
{ is_number() }
procedure CallIsNumber(const aList: TJpParamList; out aResult: TJpInstance);
var
  d: Double;
begin
  aResult := (System.Length(aList) = 1) and IsNumberInst(aList[0], d);
end;
{ is_integer() }
procedure CallIsInteger(const aList: TJpParamList; out aResult: TJpInstance);
var
  d: Double;
begin
  aResult := (System.Length(aList) = 1) and IsNumberInst(aList[0], d) and LgJson.IsExactInt(d);
end;
{ is_string() }
procedure CallIsString(const aList: TJpParamList; out aResult: TJpInstance);
var
  s: string;
begin
  aResult := (System.Length(aList) = 1) and IsStringInst(aList[0], s);
end;
{ is_array() }
procedure CallIsArray(const aList: TJpParamList; out aResult: TJpInstance);
begin
  aResult := (System.Length(aList) = 1) and (aList[0].InstType = jitValue) and
             (aList[0].Value.ValType = jvtNode) and aList[0].Value.NodeValue.IsArray;
end;
{ is_object() }
procedure CallIsObject(const aList: TJpParamList; out aResult: TJpInstance);
begin
  aResult := (System.Length(aList) = 1) and (aList[0].InstType = jitValue) and
             (aList[0].Value.ValType = jvtNode) and aList[0].Value.NodeValue.IsObject;
end;
{ is_struct() }
procedure CallIsStruct(const aList: TJpParamList; out aResult: TJpInstance);
begin
  aResult := (System.Length(aList) = 1) and (aList[0].InstType = jitValue) and
             (aList[0].Value.ValType = jvtNode) and aList[0].Value.NodeValue.IsStruct;
end;
{ contains() -- case sensitive }
procedure CallContains(const aList: TJpParamList; out aResult: TJpInstance);
var
  InText, Pattern: string;
begin
  aResult := (System.Length(aList) = 2) and IsStringInst(aList[0], InText) and
              IsStringInst(aList[1], Pattern) and (System.Pos(Pattern, InText) > 0);
end;
{ contains_text() -- case insensitive }
procedure CallContainsText(const aList: TJpParamList; out aResult: TJpInstance);
var
  InText, Pattern: string;
begin
  aResult :=
    (System.Length(aList) = 2) and IsStringInst(aList[0], InText) and IsStringInst(aList[1], Pattern) and
    (System.Pos(LgSeqUtils.Utf8ToLower(Pattern), LgSeqUtils.Utf8ToLower(InText)) > 0)
end;
{ same_text() -- case insensitive }
procedure CallSameText(const aList: TJpParamList; out aResult: TJpInstance);
var
  InText, Pattern: string;
begin
  aResult :=
    (System.Length(aList) = 2) and IsStringInst(aList[0], InText) and IsStringInst(aList[1], Pattern) and
    (LgSeqUtils.Utf8ToLower(InText) = LgSeqUtils.Utf8ToLower(Pattern))
end;
{ starts_with() -- case sensitive }
procedure CallStartsWith(const aList: TJpParamList; out aResult: TJpInstance);
var
  InText, Pattern: string;
begin
  aResult :=
    (System.Length(aList) = 2) and IsStringInst(aList[0], InText) and IsStringInst(aList[1], Pattern) and
     InText.StartsWith(Pattern);
end;
{ ends_with() -- case sensitive }
procedure CallEndsWith(const aList: TJpParamList; out aResult: TJpInstance);
var
  InText, Pattern: string;
begin
  aResult :=
    (System.Length(aList) = 2) and IsStringInst(aList[0], InText) and IsStringInst(aList[1], Pattern) and
     InText.EndsWith(Pattern);
end;
{ to_upper() }
procedure CallToUpper(const aList: TJpParamList; out aResult: TJpInstance);
var
  s: string;
begin
  if (System.Length(aList) = 1) and IsStringInst(aList[0], s) then
    aResult := LgSeqUtils.Utf8ToUpper(s)
  else
    aResult := TJpValue.Nothing;
end;
{ to_lower() }
procedure CallToLower(const aList: TJpParamList; out aResult: TJpInstance);
var
  s: string;
begin
  if (System.Length(aList) = 1) and IsStringInst(aList[0], s) then
    aResult := LgSeqUtils.Utf8ToLower(s)
  else
    aResult := TJpValue.Nothing;
end;
{ concat() }
procedure CallConcat(const aList: TJpParamList; out aResult: TJpInstance);
var
  L, R: string;
begin
  if (System.Length(aList) = 2) and IsStringInst(aList[0], L) and IsStringInst(aList[1], R) then
    aResult := L + R
  else
    aResult := TJpValue.Nothing;
end;
{ min() }
procedure CallMin(const aList: TJpParamList; out aResult: TJpInstance);
var
  s1, s2: string;
  d1, d2: Double;
begin
  if System.Length(aList) = 2 then
    begin
      if IsStringInst(aList[0], s1) and IsStringInst(aList[1], s2) then
        begin
          if s1 <= s2 then
            aResult := s1
          else
            aResult := s2
        end
      else
        if IsNumberInst(aList[0], d1) and IsNumberInst(aList[1], d2) then
          begin
            if d1 <= d2 then
              aResult := d1
            else
              aResult := d2
          end
        else
          aResult := TJpValue.Nothing;
    end
  else
    aResult := TJpValue.Nothing;
end;
{ max() }
procedure CallMax(const aList: TJpParamList; out aResult: TJpInstance);
var
  s1, s2: string;
  d1, d2: Double;
begin
  if System.Length(aList) = 2 then
    begin
      if IsStringInst(aList[0], s1) and IsStringInst(aList[1], s2) then
        begin
          if s1 >= s2 then
            aResult := s1
          else
            aResult := s2
        end
      else
        if IsNumberInst(aList[0], d1) and IsNumberInst(aList[1], d2) then
          begin
            if d1 >= d2 then
              aResult := d1
            else
              aResult := d2
          end
        else
          aResult := TJpValue.Nothing;
    end
  else
    aResult := TJpValue.Nothing;
end;
{ abs() }
procedure CallAbs(const aList: TJpParamList; out aResult: TJpInstance);
var
  d: Double;
begin
  if (System.Length(aList) = 1) and IsNumberInst(aList[0], d) then
    aResult := System.Abs(d)
  else
    aResult := TJpValue.Nothing;
end;
{ sum() }
procedure CallSum(const aList: TJpParamList; out aResult: TJpInstance);
var
  a, b: Double;
begin
  if (System.Length(aList) = 2) and IsNumberInst(aList[0], a) and IsNumberInst(aList[1], b) then
    aResult := a + b
  else
    aResult := TJpValue.Nothing;
end;
{ diff() }
procedure CallDiff(const aList: TJpParamList; out aResult: TJpInstance);
var
  a, b: Double;
begin
  if (System.Length(aList) = 2) and IsNumberInst(aList[0], a) and IsNumberInst(aList[1], b) then
    aResult := a - b
  else
    aResult := TJpValue.Nothing;
end;
{ prod() }
procedure CallProd(const aList: TJpParamList; out aResult: TJpInstance);
var
  a, b: Double;
begin
  if (System.Length(aList) = 2) and IsNumberInst(aList[0], a) and IsNumberInst(aList[1], b) then
    aResult := a * b
  else
    aResult := TJpValue.Nothing;
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
  FunCache.Add('count',        TJpFunctionDef.Make([jitNodeList], jitValue, @CallCountFun));
  FunCache.Add('length',       TJpFunctionDef.Make([jitValue], jitValue, @CallLengthFun));
  FunCache.Add('value',        TJpFunctionDef.Make([jitNodeList], jitValue, @CallValueFun));
  FunCache.Add('match',        TJpFunctionDef.Make([jitValue, jitValue], jitLogical, nil));
  FunCache.Add('search',       TJpFunctionDef.Make([jitValue, jitValue], jitLogical, nil));
  //////////////////
  FunCache.Add('key',          TJpFunctionDef.Make(nil, jitValue, nil));
  FunCache.Add('param',        TJpFunctionDef.Make([jitValue], jitValue, nil));
  FunCache.Add('is_boolean',   TJpFunctionDef.Make([jitValue], jitLogical, @CallIsBoolean));
  FunCache.Add('is_number',    TJpFunctionDef.Make([jitValue], jitLogical, @CallIsNumber));
  FunCache.Add('is_integer',   TJpFunctionDef.Make([jitValue], jitLogical, @CallIsInteger));
  FunCache.Add('is_string',    TJpFunctionDef.Make([jitValue], jitLogical, @CallIsString));
  FunCache.Add('is_array',     TJpFunctionDef.Make([jitValue], jitLogical, @CallIsArray));
  FunCache.Add('is_object',    TJpFunctionDef.Make([jitValue], jitLogical, @CallIsObject));
  FunCache.Add('is_struct',    TJpFunctionDef.Make([jitValue], jitLogical, @CallIsStruct));
  FunCache.Add('contains',     TJpFunctionDef.Make([jitValue, jitValue], jitLogical, @CallContains));
  FunCache.Add('contains_text',TJpFunctionDef.Make([jitValue, jitValue], jitLogical, @CallContainsText));
  FunCache.Add('same_text',    TJpFunctionDef.Make([jitValue, jitValue], jitLogical, @CallSameText));
  FunCache.Add('starts_with',  TJpFunctionDef.Make([jitValue, jitValue], jitLogical, @CallStartsWith));
  FunCache.Add('ends_with',    TJpFunctionDef.Make([jitValue, jitValue], jitLogical, @CallEndsWith));
  FunCache.Add('to_upper',     TJpFunctionDef.Make([jitValue], jitValue, @CallToUpper));
  FunCache.Add('to_lower',     TJpFunctionDef.Make([jitValue], jitValue, @CallToLower));
  FunCache.Add('concat',       TJpFunctionDef.Make([jitValue, jitValue], jitValue, @CallConcat));
  FunCache.Add('min',          TJpFunctionDef.Make([jitValue, jitValue], jitValue, @CallMin));
  FunCache.Add('max',          TJpFunctionDef.Make([jitValue, jitValue], jitValue, @CallMax));
  FunCache.Add('abs',          TJpFunctionDef.Make([jitValue], jitValue, @CallAbs));
  FunCache.Add('sum',          TJpFunctionDef.Make([jitValue, jitValue], jitValue, @CallSum));
  FunCache.Add('diff',         TJpFunctionDef.Make([jitValue, jitValue], jitValue, @CallDiff));
  FunCache.Add('prod',         TJpFunctionDef.Make([jitValue, jitValue], jitValue, @CallProd));
end;

initialization
  RegisterBuiltIns;
  InitCriticalSection(GlobLock);
finalization
  DoneCriticalSection(GlobLock);
end.
