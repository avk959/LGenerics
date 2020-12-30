{****************************************************************************
*                                                                           *
*   This file is part of the LGenerics package.                             *
*   Tiny JSON parser and utilites that try to follow RFC 8259.              *
*                                                                           *
*   Copyright(c) 2020 A.Koverdyaev(avk)                                     *
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
unit lgJson;

{$MODE OBJFPC}{$H+}
{$MODESWITCH ADVANCEDRECORDS}
{$MODESWITCH NESTEDPROCVARS}

interface

uses
  Classes, SysUtils, Math,
  lgUtils,
  lgAbstractContainer,
  lgQueue,
  lgVector,
  lgList,
  lgStrConst;

type
  TJsValueKind     = (jvkUnknown, jvkNull, jvkFalse, jvkTrue, jvkNumber, jvkString, jvkArray, jvkObject);
  TJsFormatOption  = (jfoSingleLine, // all arrays and objects in single line
                      jfoEgyptBrace, // egyptian braces(default Allman)
                      jfoUseTabs,    // tabs instead of spaces.
                      jfoStrAsIs);   // do not encode Pascal strings as JSON strings
  TJsFormatOptions = set of TJsFormatOption;
  EJsException     = class(Exception);

  TJVarKind = (vkNull, vkBool, vkNumber, vkString);

  TJVariant = record
  strict private
  type
    TValue = record
      case Integer of
        0: (Num: Double);
        1: (Ref: Pointer);
        2: (Bool: Boolean);
        3: (Int: Int64);
    end;

  var
    FValue: TValue;
    FKind: TJVarKind;
    procedure DoClear; inline;
    procedure ConvertError(const aSrc, aDst: string);
  private
    class operator Initialize(var v: TJVariant);
    class operator Finalize(var v: TJVariant);
    class operator Copy(constref aSrc: TJVariant; var aDst: TJVariant); inline;
    class operator AddRef(var v: TJVariant);
  public
    class function Null: TJVariant; static; inline;
    class operator := (aValue: Double): TJVariant; inline;
    class operator := (aValue: Boolean): TJVariant; inline;
    class operator := (const aValue: string): TJVariant; inline;
    class operator := (const v: TJVariant): Double; inline;
    class operator := (const v: TJVariant): Boolean; inline;
    class operator := (const v: TJVariant): string; inline;
    class operator = (const L, R: TJVariant): Boolean; inline;
    procedure Clear;
    function AsBoolean: Boolean; inline;
    function AsNumber: Double; inline;
    function AsString: string; inline;
    property Kind: TJVarKind read FKind;
  end;

  TJVarPair   = specialize TGMapEntry<string, TJVariant>;
  TJVarArray  = array of TJVariant;
  TJPairArray = array of TJVarPair;

function JNull: TJVariant; inline;
function JPair(const aName: string; const aValue: TJVariant): TJVarPair; inline;

type
  { TJsonNode is the entity used to validate, parse, generate, and navigate a json document;
    the lifetime of all nested elements is determined by the document root node;
    it is this (and only this) node that requires explicit free;
    the current implementation preserves the ordering of elements in objects;
    validator and parser are based on Douglas Crockford's JSON_checker code }
  TJsonNode = class
  public
  const
    DEF_DEPTH = 511;

  type
    TPair           = specialize TGMapEntry<string, TJsonNode>;
    TNodeEnumerator = specialize TGEnumerator<TJsonNode>;
    TPairEnumerator = specialize TGEnumerator<TPair>;
    TKeyEnumerator  = specialize TGEnumerator<string>;
    INodeEnumerable = specialize IGEnumerable<TJsonNode>;
    IPairEnumerable = specialize IGEnumerable<TPair>;
    IKeyEnumerable  = specialize IGEnumerable<string>;

    TNodeArray      = array of TJsonNode;

  private
  class var
    FmtSettings: TFormatSettings;

  type
    TJsArray  = specialize TGLiteVector<TJsonNode>;
    TJsObject = specialize TGLiteHashList2<string, TPair, string>;
    PJsArray  = ^TJsArray;
    PJsObject = ^TJsObject;

    TStrBuilder = record
    private
      FBuffer: array of AnsiChar;
      FCount: SizeInt;
    public
      constructor Create(aCapacity: SizeInt);
      function  IsEmpty: Boolean; inline;
      function  NonEmpty: Boolean; inline;
      procedure MakeEmpty; inline;
      procedure EnsureCapacity(aCapacity: SizeInt); inline;
      procedure Append(c: AnsiChar); inline;
      procedure Append(c: AnsiChar; aCount: SizeInt);
      procedure Append(const s: string); inline;
      procedure AppendEncode(const s: string);
      procedure Append(const s: shortstring); inline;
      procedure SaveToStream(aStream: TStream);
      function  ToString: string; inline;
      function  ToDecodeString: string;
      property  Count: SizeInt read FCount;
    end;

    TEmptyNodeEnumerator = class(TNodeEnumerator)
    protected
      function  GetCurrent: TJsonNode; override;
    public
      function  MoveNext: Boolean; override;
      procedure Reset; override;
    end;

    TEmptyKeyEnumerator = class(TKeyEnumerator)
    protected
      function  GetCurrent: string; override;
    public
      function  MoveNext: Boolean; override;
      procedure Reset; override;
    end;

    TEmptyPairEnumerator = class(TPairEnumerator)
    protected
      function  GetCurrent: TPair; override;
    public
      function  MoveNext: Boolean; override;
      procedure Reset; override;
    end;

    TANodeEnumerator = class(TNodeEnumerator)
    protected
      FEnum,
      FResEnum: TJsArray.TEnumerator;
      function  GetCurrent: TJsonNode; override;
    public
      constructor Create(const aEnum: TJsArray.TEnumerator);
      function  MoveNext: Boolean; override;
      procedure Reset; override;
    end;

    TONodeEnumerator = class(TNodeEnumerator)
    protected
      FEnum,
      FResEnum: TJsObject.TEnumerator;
      function  GetCurrent: TJsonNode; override;
    public
      constructor Create(const aEnum: TJsObject.TEnumerator);
      function  MoveNext: Boolean; override;
      procedure Reset; override;
    end;

    TOPairEnumerator = class(TPairEnumerator)
    protected
      FEnum,
      FConstEnum: TJsObject.TEnumerator;
      function  GetCurrent: TPair; override;
    public
      constructor Create(const aEnum: TJsObject.TEnumerator);
      function  MoveNext: Boolean; override;
      procedure Reset; override;
    end;

    TOKeyEnumerator = class(TKeyEnumerator)
    protected
      FEnum,
      FConstEnum: TJsObject.TEnumerator;
      function  GetCurrent: string; override;
    public
      constructor Create(const aEnum: TJsObject.TEnumerator);
      function  MoveNext: Boolean; override;
      procedure Reset; override;
    end;

    TOIdenticEnumerator = class(TPairEnumerator)
    protected
      FEnum: TJsObject.TIdenticEnumerator;
      function  GetCurrent: TPair; override;
    public
      constructor Create(const aEnum: TJsObject.TIdenticEnumerator);
      function  MoveNext: Boolean; override;
      procedure Reset; override;
    end;

    TPairs = specialize TGEnumCursor<TPair>;
    TKeys  = specialize TGEnumCursor<string>;
    TNodes = specialize TGEnumCursor<TJsonNode>;

    TValue = record
    case Integer of
      0: (Ref: Pointer);
      1: (Num: Double);
      2: (Int: Int64);
    end;

  const
    ARRAY_INIT_SIZE    = 8;
    SBUILDER_INIT_SIZE = 256;
    READ_BUFFER_SIZE   = 65536;

  var
    FValue: TValue;
    FKind: TJsValueKind;
    class function  CreateJsArray: PJsArray; static; inline;
    class procedure FreeJsArray(a: PJsArray); static;
    class function  CreateJsObject: PJsObject; static; inline;
    class procedure FreeJsObject(o: PJsObject); static;
    function  GetFString: string; inline;
    function  GetFArray: PJsArray; inline;
    function  GetFObject: PJsObject; inline;
    procedure SetFString(const aValue: string); inline;
    procedure SetFArray(aValue: PJsArray); inline;
    procedure SetFObject(aValue: PJsObject); inline;
    procedure DoClear;
    function  GetAsArray: TJsonNode; inline;
    function  GetAsObject: TJsonNode; inline;
    function  GetAsNull: TJsonNode; inline;
    function  GetAsBoolean: Boolean; inline;
    procedure SetAsBoolean(aValue: Boolean); inline;
    function  GetAsNumber: Double; inline;
    procedure SetAsNumber(aValue: Double);
    function  GetAsString: string; inline;
    procedure SetAsString(const aValue: string); inline;
    function  DoBuildJson: TStrBuilder;
    function  GetAsJson: string; inline;
    procedure SetAsJson(const aValue: string);
    function  GetCount: SizeInt; inline;
    function  CanArrayInsert(aIndex: SizeInt): Boolean; inline;
    function  CanObjectInsert(aIndex: SizeInt): Boolean; inline;
    function  GetItem(aIndex: SizeInt): TJsonNode;
    function  GetPair(aIndex: SizeInt): TPair;
    function  GetByName(const aName: string): TJsonNode;
    function  GetValue(const aName: string): TJVariant;
    procedure SetValue(const aName: string; const aValue: TJVariant);
    function  GetClassEnumerator: TNodeEnumerator;
    class constructor Init;
    property  FString: string read GetFString write SetFString;
    property  FArray: PJsArray read GetFArray write SetFArray;
    property  FObject: PJsObject read GetFObject write SetFObject;

  public
  type
    TEnumerator = record
    private
      FNode: TJsonNode;
      FCurrIndex: SizeInt;
      function GetCurrent: TJsonNode; inline;
    public
      function MoveNext: Boolean; inline;
      property Current: TJsonNode read GetCurrent;
    end;

    TTreeEnumerator = record
    private
    type
      TQueue = specialize TGLiteQueue<TJsonNode>;
    var
      FQueue: TQueue;
      FCurrent: TJsonNode;
      function  GetCurrent: TJsonNode; inline;
    public
      function  MoveNext: Boolean;
      property Current: TJsonNode read GetCurrent;
    end;

    TSubTree = record
    private
      FNode: TJsonNode;
    public
      function GetEnumerator: TTreeEnumerator;
    end;

  { validators: aDepth indicates the maximum nesting depth of structures;
    if aSkipBom is set to True then UTF-8 BOM(and only that) will be ignored }
    class function ValidJson(const s: string; aDepth: Integer = DEF_DEPTH;
                             aSkipBom: Boolean = False): Boolean; static;
    class function ValidJson(aJson: TStream; aDepth: Integer = DEF_DEPTH;
                             aSkipBom: Boolean = False): Boolean; static;
    class function ValidJsonFile(const aFileName: string; aDepth: Integer = DEF_DEPTH;
                                 aSkipBom: Boolean = False): Boolean; static;
    class function JsonStringValid(const s: string): Boolean; static;
    class function JsonNumberValid(const s: string): Boolean; static;
  { returns the parsing result; if the result is True, then the created
    object is returned in the aRoot parameter, otherwise nil is returned }
    class function TryParse(const s: string; out aRoot: TJsonNode;
                            aDepth: Integer = DEF_DEPTH; aSkipBom: Boolean = False): Boolean; static;
    class function TryParse(aStream: TStream; out aRoot: TJsonNode;
                            aDepth: Integer = DEF_DEPTH; aSkipBom: Boolean = False): Boolean; static;
    class function TryParseFile(const aFileName: string; out aRoot: TJsonNode;
                            aDepth: Integer = DEF_DEPTH; aSkipBom: Boolean = False): Boolean; static;
  { converts a pascal string to a JSON string }
    class function PasStrToJson(const s: string): string; static;
  { it is assumed that an array of path parts is passed;
    returns a JSON pointer (RFC 6901) as a pascal string }
    class function JsonPtrEncode(aPath: TStringArray): string; static;
  { it is assumed that a JSON pointer is passed (RFC 6901) as a pascal string;
    returns an array of path parts }
    class function JsonPtrDecode(const aPtr: string): TStringArray; static;
    class function NewNode: TJsonNode; static; inline;
    class function NewNull: TJsonNode; static; inline;
    class function NewNode(aValue: Boolean): TJsonNode; static; inline;
    class function NewNode(aValue: Double): TJsonNode; static; inline;
    class function NewNode(const aValue: string): TJsonNode; static; inline;
    class function NewNode(aKind: TJsValueKind): TJsonNode; static; inline;
  { parses the JSON string, returns nil in case of failure,
    otherwise its representation as TJsonNode }
    class function NewJson(const s: string): TJsonNode; static; inline;
    constructor Create;
    constructor CreateNull;
    constructor Create(aValue: Boolean);
    constructor Create(aValue: Double);
    constructor Create(const aValue: string);
    constructor Create(aKind: TJsValueKind);
    constructor Create(const a: TJVarArray);
    constructor Create(const a: TJPairArray);
    destructor Destroy; override;
    function  GetEnumerator: TEnumerator; inline;
    function  Nodes: INodeEnumerable; inline;
    function  SubTree: TSubTree; inline;
    function  Enrties: IPairEnumerable; inline;
    function  Keys: IKeyEnumerable; inline;
    function  IdenticKeys(const aKey: string): IPairEnumerable; inline;
    function  IsNull: Boolean; inline;
    function  IsFalse: Boolean; inline;
    function  IsTrue: Boolean; inline;
    function  IsNumber: Boolean; inline;
    function  IsString: Boolean; inline;
    function  IsArray: Boolean; inline;
    function  IsObject: Boolean; inline;
    function  IsBoolean: Boolean; inline;
    function  IsLiteral: Boolean; inline;
    function  IsScalar: Boolean; inline;
    function  IsStruct: Boolean; inline;
    procedure Clear; inline;
  { tries to load JSON from a string, in case of failure it returns False,
    in this case the content of the instance does not change }
    function  Parse(const s: string): Boolean;
  { adds null to the instance as to an array; if it is not an array,
    it is cleared and becomes an array - be careful; returns Self }
    function  AddNull: TJsonNode; inline;
  { adds Boolean value to the instance as to an array; if it is not
    an array, it is cleared and becomes an array - be careful; returns Self }
    function  Add(aValue: Boolean): TJsonNode; inline;
  { adds a number to the instance as to an array; if it is not an array,
    it is cleared and becomes an array - be careful; returns Self }
    function  Add(aValue: Double): TJsonNode; inline;
  { adds string value to the instance as to an array; if it is not an array,
    it is cleared and becomes an array - be careful; returns Self }
    function  Add(const aValue: string): TJsonNode; inline;
  { adds all elements from a to the instance as to an array; if it is not
    an array, it is cleared and becomes an array - be careful; returns Self }
    function  Add(const a: TJVarArray): TJsonNode;
  { adds a new object of the specified kind to the instance as to an array;
    if an instance is not an array, it is cleared and becomes an array - be careful;
    returns a new object }
    function  AddNode(aKind: TJsValueKind): TJsonNode; inline;
  { returns True and the created object in the aNode parameter,
    if the string s can be parsed; the new object is added as in an array - be careful }
    function  AddJson(const s: string; out aNode: TJsonNode): Boolean;
  { adds all pairs from a to the instance as to an object; if it is not
    an object, it is cleared and becomes an object - be careful; returns Self }
    function  Add(const a: TJPairArray): TJsonNode;
  { adds pair aName: null to the instance as to an object; if it is not an object,
    it is cleared and becomes an object - be careful; returns Self }
    function  AddNull(const aName: string): TJsonNode; inline;
  { adds pair (aName: aValue) the instance as to an object; if it is not an object,
    it is cleared and becomes an object - be careful; returns Self }
    function  Add(const aName: string; aValue: Boolean): TJsonNode; inline;
    function  Add(const aName: string; aValue: Double): TJsonNode; inline;
    function  Add(const aName, aValue: string): TJsonNode; inline;
    function  Add(const aName: string; const aValue: TJVarArray): TJsonNode;
    function  Add(const aName: string; const aValue: TJPairArray): TJsonNode;
  { adds a new object of the specified type associated with aName to the instance as to an object;
    if an instance is not an object, it is cleared and becomes an object - be careful;
    returns a new object }
    function  AddNode(const aName: string; aKind: TJsValueKind): TJsonNode; inline;
    function  AddJson(const aName, aJson: string; out aNode: TJsonNode): Boolean;
  { adds pair (aName: null) to the instance as to an object and returns True
    only if aName is unique within an instance, otherwise returns False;
    if an instance is not an object, it is cleared and becomes an object - be careful }
    function  AddUniqNull(const aName: string): Boolean;
    function  AddUniq(const aName: string; aValue: Boolean): Boolean;
    function  AddUniq(const aName: string; aValue: Double): Boolean;
    function  AddUniq(const aName, aValue: string): Boolean;
    function  AddUniq(const aName: string;  const aValue: TJVarArray; out aNode: TJsonNode): Boolean;
    function  AddUniq(const aName: string;  const aValue: TJPairArray; out aNode: TJsonNode): Boolean;
    function  AddUniqNode(const aName: string; out aNode: TJsonNode; aKind: TJsValueKind): Boolean;
    function  AddUniqJson(const aName, aJson: string; out aNode: TJsonNode): Boolean;
    function  InsertNull(aIndex: SizeInt): Boolean;
    function  Insert(aIndex: SizeInt; aValue: Boolean): Boolean;
    function  Insert(aIndex: SizeInt; aValue: Double): Boolean;
    function  Insert(aIndex: SizeInt; const aValue: string): Boolean;
    function  InsertNode(aIndex: SizeInt; out aNode: TJsonNode; aKind: TJsValueKind): Boolean;
    function  InsertNull(aIndex: SizeInt; const aName: string): Boolean;
    function  Insert(aIndex: SizeInt; const aName: string; aValue: Boolean): Boolean;
    function  Insert(aIndex: SizeInt; const aName: string; aValue: Double): Boolean;
    function  Insert(aIndex: SizeInt; const aName, aValue: string): Boolean;
    function  InsertNode(aIndex: SizeInt; const aName: string; out aNode: TJsonNode; aKind: TJsValueKind): Boolean;
    function  Contains(const aName: string): Boolean; inline;
    function  ContainsUniq(const aName: string): Boolean; inline;
    function  IndefOfName(const aName: string): SizeInt; inline;
    function  CountOfName(const aName: string): SizeInt; inline;
    function  Find(const aKey: string; out aValue: TJsonNode): Boolean;
    function  FindOrAdd(const aName: string; out aValue: TJsonNode): Boolean;
    function  FindUniq(const aName: string; out aValue: TJsonNode): Boolean;
    function  FindAll(const aName: string): TNodeArray;
    function  Find(aIndex: SizeInt; out aValue: TJsonNode): Boolean;
    function  FindPair(aIndex: SizeInt; out aValue: TPair): Boolean;
    function  FindName(aIndex: SizeInt; out aName: string): Boolean;
    function  Delete(aIndex: SizeInt): Boolean;
    function  Remove(const aName: string): Boolean;
    function  RemoveAll(const aName: string): SizeInt;
  { the JSON Pointer (RFC 6901) is assumed to be passed as pascal string;
    each node considered self as a root }
    function  FindPath(const aPtr: string; out aNode: TJsonNode): Boolean;
  { an array of path parts is assumed to be passed }
    function  FindPath(const aPath: array of string; out aNode: TJsonNode): Boolean;
    function  FormatJson(aOptions: TJsFormatOptions = []; aIndent: Integer = 2): string;
    procedure SaveToStream(aStream: TStream);
    procedure SaveToFile(const aFileName: string);
  { SetAsJson remark: if the parser fails to parse the original string,
    an exception will be raised. }
    property  AsJson: string read GetAsJson write SetAsJson;
    property  AsNull: TJsonNode read GetAsNull;
    property  AsBoolean: Boolean read GetAsBoolean write SetAsBoolean;
    property  AsNumber: Double read GetAsNumber write SetAsNumber;
    property  AsString: string read GetAsString write SetAsString;
    property  AsArray: TJsonNode read GetAsArray;
    property  AsObject: TJsonNode read GetAsObject;
    property  Kind: TJsValueKind read FKind;
    property  Count: SizeInt read GetCount;
  { will raise exception if aIndex out of bounds }
    property  Items[aIndex: SizeInt]: TJsonNode read GetItem;
  { will raise exception if aIndex out of bounds or an instance is not an object }
    property  Pairs[aIndex: SizeInt]: TPair read GetPair;
  { if instance is an object then acts as FindOrAdd, otherwise returns nil }
    property  Named[const aName: string]: TJsonNode read GetByName; //todo: need another prop name
  { if GetValue does not find aName or if the value found is an array or object,
    it will raise an exception; SetValue will make an object from an instance - be careful }
    property  Values[const aName: string]: TJVariant read GetValue write SetValue; default;
  end;

implementation
{$B-}{$COPERATORS ON}{$POINTERMATH ON}

{ TJVariant }

procedure TJVariant.DoClear;
begin
  if Kind = vkString then
    string(FValue.Ref) := ''
  else
    FValue.Int := 0;
end;

procedure TJVariant.ConvertError(const aSrc, aDst: string);
begin
  raise EInvalidCast.CreateFmt(SECantConvertFmt, [aSrc, aDst]);
end;

class operator TJVariant.Initialize(var v: TJVariant);
begin
  v.FValue.Int := 0;
  v.FKind := vkNull;
end;

class operator TJVariant.Finalize(var v: TJVariant);
begin
  v.DoClear;
end;

class operator TJVariant.Copy(constref aSrc: TJVariant; var aDst: TJVariant);
begin
  aDst.DoClear;
  if aSrc.Kind = vkString then
    string(aDst.FValue.Ref) := string(aSrc.FValue.Ref)
  else
    aDst.FValue := aSrc.FValue;
  aDst.FKind := aSrc.Kind;
end;

class operator TJVariant.AddRef(var v: TJVariant);
begin
  if v.Kind = vkString then
    UniqueString(string(v.FValue.Ref));
end;

class function TJVariant.Null: TJVariant;
begin
  Result.Clear;
end;

class operator TJVariant.:=(aValue: Double): TJVariant;
begin
  Result{%H-}.DoClear;
  Result.FValue.Num := aValue;
  Result.FKind := vkNumber;
end;

class operator TJVariant.:=(aValue: Boolean): TJVariant;
begin
  Result{%H-}.DoClear;
  Result.FValue.Bool := aValue;
  Result.FKind := vkBool;
end;

class operator TJVariant.:=(const aValue: string): TJVariant;
begin
  Result{%H-}.DoClear;
  string(Result.FValue.Ref) := aValue;
  Result.FKind := vkString;
end;

class operator TJVariant.:=(const v: TJVariant): Double;
begin
  case v.Kind of
    vkNull:   v.ConvertError('null', 'Double');
    vkBool:   v.ConvertError('Boolean', 'Double');
    vkString: v.ConvertError('string', 'Double');
  else
    exit(v.FValue.Num);
  end;
  Result := v.FValue.Num;
end;

class operator TJVariant.:=(const v: TJVariant): Boolean;
begin
  case v.Kind of
    vkNull:   v.ConvertError('null', 'Boolean');
    vkNumber: v.ConvertError('Double', 'Boolean');
    vkString: v.ConvertError('string', 'Boolean');
  else
    exit(v.FValue.Bool);
  end;
  Result := v.FValue.Bool;
end;

class operator TJVariant.:=(const v: TJVariant): string;
begin
  case v.Kind of
    vkNull:   v.ConvertError('null', 'string');
    vkBool:   v.ConvertError('Boolean', 'string');
    vkNumber: v.ConvertError('Double', 'string');
  else
    exit(string(v.FValue.Ref));
  end;
  Result := string(v.FValue.Ref);
end;

class operator TJVariant.= (const L, R: TJVariant): Boolean;
begin
  case L.Kind of
    vkBool:   Result := (R.Kind = vkBool) and (L.FValue.Bool = R.FValue.Bool);
    vkNumber: Result := (R.Kind = vkNumber) and SameValue(L.FValue.Num, R.FValue.Num);
    vkString: Result := (R.Kind = vkString) and (string(L.FValue.Ref) = string(R.FValue.Ref));
  else
    Result := False;
  end;
end;

procedure TJVariant.Clear;
begin
  DoClear;
  FKind := vkNull;
end;

function TJVariant.AsBoolean: Boolean;
begin
  Result := Self;
end;

function TJVariant.AsNumber: Double;
begin
  Result := Self;
end;

function TJVariant.AsString: string;
begin
  Result := Self;
end;

function JNull: TJVariant;
begin
  Result.Clear;
end;

function JPair(const aName: string; const aValue: TJVariant): TJVarPair;
begin
  Result := TJVarPair.Create(aName, aValue);
end;

type
  TParseMode = (pmNone, pmKey, pmArray, pmObject);
  PParseMode = ^TParseMode;

const
{$PUSH}{$J-}
  chOpenCurBr: AnsiChar  = '{';
  chClosCurBr: AnsiChar  = '}';
  chOpenSqrBr: AnsiChar  = '[';
  chClosSqrBr: AnsiChar  = ']';
  chQuote: AnsiChar      = '"';
  chColon: AnsiChar      = ':';
  chComma: AnsiChar      = ',';
  chSpace: AnsiChar      = ' ';
  chEscapeSym: AnsiChar  = '\';
  chBackSpSym: AnsiChar  = 'b';
  chTabSym: AnsiChar     = 't';
  chLineSym: AnsiChar    = 'n';
  chFormSym: AnsiChar    = 'f';
  chCarRetSym: AnsiChar  = 'r';
  chUnicodeSym: AnsiChar = 'u';
  chZero: AnsiChar       = '0';
{$POP}

  Space  = Integer( 0); //  space
  White  = Integer( 1); //  other whitespace
  LCurBr = Integer( 2); //  {
  RCurBr = Integer( 3); //  }
  LSqrBr = Integer( 4); //  [
  RSqrBr = Integer( 5); //  ]
  Colon  = Integer( 6); //  :
  Comma  = Integer( 7); //  ,
  Quote  = Integer( 8); //  "
  BSlash = Integer( 9); //  \
  Slash  = Integer(10); //  /
  Plus   = Integer(11); //  +
  Minus  = Integer(12); //  -
  Point  = Integer(13); //  .
  Zero   = Integer(14); //  0
  Digit  = Integer(15); //  123456789
  LowerA = Integer(16); //  a
  LowerB = Integer(17); //  b
  LowerC = Integer(18); //  c
  LowerD = Integer(19); //  d
  LowerE = Integer(20); //  e
  LowerF = Integer(21); //  f
  LowerL = Integer(22); //  l
  LowerN = Integer(23); //  n
  LowerR = Integer(24); //  r
  LowerS = Integer(25); //  s
  LowerT = Integer(26); //  t
  LowerU = Integer(27); //  u
  ABCDF  = Integer(28); //  ABCDF
  UpperE = Integer(29); //  E
  Etc    = Integer(30); //  everything else

  SymClassTable: array[0..127] of Integer = (
    -1,    -1,     -1,     -1,     -1,     -1,     -1,     -1,
    -1,    White,  White,  -1,     -1,     White,  -1,     -1,
    -1,    -1,     -1,     -1,     -1,     -1,     -1,     -1,
    -1,    -1,     -1,     -1,     -1,     -1,     -1,     -1,

    Space, Etc,    Quote,  Etc,    Etc,    Etc,    Etc,    Etc,
    Etc,   Etc,    Etc,    Plus,   Comma,  Minus,  Point,  Slash,
    Zero,  Digit,  Digit,  Digit,  Digit,  Digit,  Digit,  Digit,
    Digit, Digit,  Colon,  Etc,    Etc,    Etc,    Etc,    Etc,

    Etc,   ABCDF,  ABCDF,  ABCDF,  ABCDF,  UpperE, ABCDF,  Etc,
    Etc,   Etc,    Etc,    Etc,    Etc,    Etc,    Etc,    Etc,
    Etc,   Etc,    Etc,    Etc,    Etc,    Etc,    Etc,    Etc,
    Etc,   Etc,    Etc,    LSqrBr, BSlash, RSqrBr, Etc,    Etc,

    Etc,   LowerA, LowerB, LowerC, LowerD, LowerE, LowerF, Etc,
    Etc,   Etc,    Etc,    Etc,    LowerL, Etc,    LowerN, Etc,
    Etc,   Etc,    LowerR, LowerS, LowerT, LowerU, Etc,    Etc,
    Etc,   Etc,    Etc,    LCurBr, Etc,    RCurBr, Etc,    Etc
  );

  __ = Integer(-1);// error
  GO = Integer( 0);// start
  OK = Integer( 1);// ok
  OB = Integer( 2);// object
  KE = Integer( 3);// key
  CO = Integer( 4);// colon
  VA = Integer( 5);// value
  AR = Integer( 6);// array
  ST = Integer( 7);// string
  ES = Integer( 8);// escape
  U1 = Integer( 9);// u1
  U2 = Integer(10);// u2
  U3 = Integer(11);// u3
  U4 = Integer(12);// u4
  MI = Integer(13);// minus
  ZE = Integer(14);// zero
  IR = Integer(15);// integer
  FR = Integer(16);// fraction
  FS = Integer(17);// fraction
  E1 = Integer(18);// e
  E2 = Integer(19);// ex
  E3 = Integer(20);// exp
  T1 = Integer(21);// tr
  T2 = Integer(22);// tru
  T3 = Integer(23);// true
  F1 = Integer(24);// fa
  F2 = Integer(25);// fal
  F3 = Integer(26);// fals
  F4 = Integer(27);// false
  N1 = Integer(28);// nu
  N2 = Integer(29);// nul
  N3 = Integer(30);// null

  VldStateTransitions: array[GO..N3, Space..Etc] of Integer = (
{
  The state transition table takes the current state and the current symbol,
  and returns either a new state or an action. An action is represented as a
  negative number. A JSON text is accepted if at the end of the text the
  state is OK and if the mode is MODE_DONE.

             white                                      1-9                                   ABCDF  etc
         space |  {  }  [  ]  :  ,  "  \  /  +  -  .  0  |  a  b  c  d  e  f  l  n  r  s  t  u  |  E  | }
{start  GO}(GO,GO,-6,__,-5,__,__,__,ST,__,__,__,MI,__,ZE,IR,__,__,__,__,__,F1,__,N1,__,__,T1,__,__,__,__),
{ok     OK}(OK,OK,__,-8,__,-7,__,-3,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__),
{object OB}(OB,OB,__,-9,__,__,__,__,ST,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__),
{rey    KE}(KE,KE,__,__,__,__,__,__,ST,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__),
{colon  CO}(CO,CO,__,__,__,__,-2,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__),
{value  VA}(VA,VA,-6,__,-5,__,__,__,ST,__,__,__,MI,__,ZE,IR,__,__,__,__,__,F1,__,N1,__,__,T1,__,__,__,__),
{array  AR}(AR,AR,-6,__,-5,-7,__,__,ST,__,__,__,MI,__,ZE,IR,__,__,__,__,__,F1,__,N1,__,__,T1,__,__,__,__),
{string ST}(ST,__,ST,ST,ST,ST,ST,ST,-4,ES,ST,ST,ST,ST,ST,ST,ST,ST,ST,ST,ST,ST,ST,ST,ST,ST,ST,ST,ST,ST,ST),
{escape ES}(__,__,__,__,__,__,__,__,ST,ST,ST,__,__,__,__,__,__,ST,__,__,__,ST,__,ST,ST,__,ST,U1,__,__,__),
{u1     U1}(__,__,__,__,__,__,__,__,__,__,__,__,__,__,U2,U2,U2,U2,U2,U2,U2,U2,__,__,__,__,__,__,U2,U2,__),
{u2     U2}(__,__,__,__,__,__,__,__,__,__,__,__,__,__,U3,U3,U3,U3,U3,U3,U3,U3,__,__,__,__,__,__,U3,U3,__),
{u3     U3}(__,__,__,__,__,__,__,__,__,__,__,__,__,__,U4,U4,U4,U4,U4,U4,U4,U4,__,__,__,__,__,__,U4,U4,__),
{u4     U4}(__,__,__,__,__,__,__,__,__,__,__,__,__,__,ST,ST,ST,ST,ST,ST,ST,ST,__,__,__,__,__,__,ST,ST,__),
{minus  MI}(__,__,__,__,__,__,__,__,__,__,__,__,__,__,ZE,IR,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__),
{zero   ZE}(OK,OK,__,-8,__,-7,__,-3,__,__,__,__,__,FR,__,__,__,__,__,__,E1,__,__,__,__,__,__,__,__,E1,__),
{int    IR}(OK,OK,__,-8,__,-7,__,-3,__,__,__,__,__,FR,IR,IR,__,__,__,__,E1,__,__,__,__,__,__,__,__,E1,__),
{frac   FR}(__,__,__,__,__,__,__,__,__,__,__,__,__,__,FS,FS,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__),
{fracs  FS}(OK,OK,__,-8,__,-7,__,-3,__,__,__,__,__,__,FS,FS,__,__,__,__,E1,__,__,__,__,__,__,__,__,E1,__),
{e      E1}(__,__,__,__,__,__,__,__,__,__,__,E2,E2,__,E3,E3,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__),
{ex     E2}(__,__,__,__,__,__,__,__,__,__,__,__,__,__,E3,E3,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__),
{exp    E3}(OK,OK,__,-8,__,-7,__,-3,__,__,__,__,__,__,E3,E3,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__),
{tr     T1}(__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,T2,__,__,__,__,__,__),
{tru    T2}(__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,T3,__,__,__),
{true   T3}(__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,OK,__,__,__,__,__,__,__,__,__,__),
{fa     F1}(__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,F2,__,__,__,__,__,__,__,__,__,__,__,__,__,__),
{fal    F2}(__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,F3,__,__,__,__,__,__,__,__),
{fals   F3}(__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,F4,__,__,__,__,__),
{false  F4}(__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,OK,__,__,__,__,__,__,__,__,__,__),
{nu     N1}(__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,N2,__,__,__),
{nul    N2}(__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,N3,__,__,__,__,__,__,__,__),
{null   N3}(__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,OK,__,__,__,__,__,__,__,__)
  );

type
  TBomKind = (bkNone, bkUtf8, bkUtf16, bkUtf32);
const
  UTF8_BOM_LEN = 3;

function DetectBom(aBuf: PByte; aBufSize: SizeInt): TBomKind;
{$PUSH}{$J-}
const
  U16LE: array[0..1] of Byte = ($FF, $FE);
  U16BE: array[0..1] of Byte = ($FE, $FF);
  UTF8:  array[0..2] of Byte  =($EF, $BB, $BF);
  U32LE: array[0..3] of Byte = ($FF, $FE, $00, $00);
  U32BE: array[0..3] of Byte = ($00, $00, $FE, $FF);
{$POP}
  function IsUtf16(p: PByte): Boolean;
  begin
    Result := ((p[0] xor U16LE[0]) or (p[1] xor U16LE[1]) = 0) or
              ((p[0] xor U16BE[0]) or (p[1] xor U16BE[1]) = 0);
  end;
  function IsUtf8(p: PByte): Boolean;
  begin
    Result := (p[0] xor UTF8[0]) or (p[1] xor UTF8[1]) or (p[2] xor UTF8[2]) = 0;
  end;
  function IsUtf32(p: PByte): Boolean;
  begin
    Result := ((p[0] xor U32LE[0]) or (p[1] xor U32LE[1]) or
               (p[2] xor U32LE[2]) or (p[3] xor U32LE[3]) = 0) or
              ((p[0] xor U32BE[0]) or (p[1] xor U32BE[1]) or
               (p[2] xor U32BE[2]) or (p[3] xor U32BE[3]) = 0);
  end;
begin
  if (aBufSize >= 2) and IsUtf16(aBuf) then exit(bkUtf16);
  if (aBufSize >= 3) and IsUtf8(aBuf) then exit(bkUtf8);
  if (aBufSize >= 4) and IsUtf32(aBuf) then exit(bkUtf32);
  Result := bkNone;
end;

{$PUSH}{$MACRO ON}
{$DEFINE ValidateBufMacro :=
for I := 0 to Pred(Size) do
  begin
    if Buf[I] < 128 then
      begin
        NextClass := SymClassTable[Buf[I]];
        if NextClass = __ then exit(False);
      end
    else
      NextClass := Etc;
    NextState := VldStateTransitions[State, NextClass];
    if NextState >= 0 then
      State := NextState
    else
      case NextState of
        -9:
          begin
            if Stack[sTop] <> pmKey then exit(False);
            Dec(sTop);
            State := OK;
          end;
        -8:
          begin
            if Stack[sTop] <> pmObject then exit(False);
            Dec(sTop);
            State := OK;
          end;
        -7:
          begin
            if Stack[sTop] <> pmArray then exit(False);
            Dec(sTop);
            State := OK;
          end;
        -6:
          begin
            if sTop >= StackHigh then exit(False);
            Inc(sTop);
            Stack[sTop] := pmKey;
            State := OB;
          end;
        -5:
          begin
            if sTop >= StackHigh then exit(False);
            Inc(sTop);
            Stack[sTop] := pmArray;
            State := AR;
          end;
        -4:
          case Stack[sTop] of
            pmKey:                     State := CO;
            pmNone, pmArray, pmObject: State := OK;
          end;
        -3:
          case Stack[sTop] of
            pmObject:
              begin
                Stack[sTop] := pmKey;
                State := KE;
              end;
            pmArray: State := VA;
          else
            exit(False);
          end;
        -2:
          begin
            if Stack[sTop] <> pmKey then exit(False);
            Stack[sTop] := pmObject;
            State := VA;
          end;
      else
        exit(False);
      end;
  end
}

type
  TOpenArray = record
    Data: Pointer;
    Size: Integer;
    constructor Create(aData: Pointer; aSize: Integer);
  end;

constructor TOpenArray.Create(aData: Pointer; aSize: Integer);
begin
  Data := aData;
  Size := aSize;
end;

function ValidateBuf(Buf: PByte; Size: SizeInt; const aStack: TOpenArray): Boolean;
var
  Stack: PParseMode;
  I: SizeInt;
  NextState, NextClass, StackHigh: Integer;
  State: Integer = GO;
  sTop: Integer = 0;
begin
  Stack := aStack.Data;
  StackHigh := Pred(aStack.Size);
  Stack[0] := pmNone;
  ValidateBufMacro;
  Result := ((State = OK) or (State in [IR, FS, E3])) and (sTop = 0) and (Stack[0] = pmNone);
end;

function ValidateStrBuf(Buf: PByte; Size: SizeInt; const aStack: TOpenArray): Boolean;
var
  Stack: PParseMode;
  I: SizeInt;
  NextState, NextClass, StackHigh: Integer;
  State: Integer = GO;
  sTop: Integer = 0;
begin
  Stack := aStack.Data;
  StackHigh := Pred(aStack.Size);
  Stack[0] := pmNone;
  ValidateBufMacro;
  Result := (State = OK) and (sTop = 0) and (Stack[0] = pmNone);
end;

function ValidateNumBuf(Buf: PByte; Size: SizeInt; const aStack: TOpenArray): Boolean;
var
  Stack: PParseMode;
  I: SizeInt;
  NextState, NextClass, StackHigh: Integer;
  State: Integer = GO;
  sTop: Integer = 0;
begin
  Stack := aStack.Data;
  StackHigh := Pred(aStack.Size);
  Stack[0] := pmNone;
  ValidateBufMacro;
  Result := (State in [IR, FS, E3]) and (sTop = 0) and (Stack[0] = pmNone);
end;

function ValidateStream(s: TStream; aSkipBom: Boolean; const aStack: TOpenArray): Boolean;
var
  Stack: PParseMode;
  Buffer: array[0..Pred(TJsonNode.READ_BUFFER_SIZE div SizeOf(SizeUInt))] of SizeUInt;
  I, Size: SizeInt;
  NextState, NextClass, StackHigh: Integer;
  State: Integer = GO;
  sTop: Integer = 0;
  Buf: PByte;
begin
  Stack := aStack.Data;
  StackHigh := Pred(aStack.Size);
  Stack[0] := pmNone;
  Buf := @Buffer[0];
  Size := s.Read(Buffer, SizeOf(Buffer));
  if Size < 1 then exit(False);
  if aSkipBom then
    case DetectBom(Buf, Size) of
      bkNone: ;
      bkUtf8:
        begin
          if not aSkipBom then exit(False);
          Buf += UTF8_BOM_LEN;
          Size -= UTF8_BOM_LEN;
        end;
    else
      exit(False);
    end;
  ValidateBufMacro;
  Buf := @Buffer[0];
  repeat
    Size := s.Read(Buffer, SizeOf(Buffer));
    ValidateBufMacro;
  until Size < SizeOf(Buffer);
  Result := ((State = OK) or (State in [IR, FS, E3])) and (sTop = 0) and (Stack[0] = pmNone);
end;
{$POP}

{ TJsonNode.TStrBuilder }

constructor TJsonNode.TStrBuilder.Create(aCapacity: SizeInt);
begin
  if aCapacity > 0 then
    System.SetLength(FBuffer, lgUtils.RoundUpTwoPower(aCapacity))
  else
    System.SetLength(FBuffer, DEFAULT_CONTAINER_CAPACITY);
  FCount := 0;
end;

function TJsonNode.TStrBuilder.IsEmpty: Boolean;
begin
  Result := Count = 0;
end;

function TJsonNode.TStrBuilder.NonEmpty: Boolean;
begin
  Result := Count <> 0;
end;

procedure TJsonNode.TStrBuilder.MakeEmpty;
begin
  FCount := 0;
end;

procedure TJsonNode.TStrBuilder.EnsureCapacity(aCapacity: SizeInt);
begin
  if aCapacity <= System.Length(FBuffer) then exit;
  System.SetLength(FBuffer, lgUtils.RoundUpTwoPower(aCapacity));
end;

procedure TJsonNode.TStrBuilder.Append(c: AnsiChar);
begin
  EnsureCapacity(Count + 1);
  FBuffer[Count] := c;
  Inc(FCount);
end;

procedure TJsonNode.TStrBuilder.Append(c: AnsiChar; aCount: SizeInt);
begin
  EnsureCapacity(Count + aCount);
  FillChar(FBuffer[Count], aCount, c);
  FCount += aCount;
end;

procedure TJsonNode.TStrBuilder.Append(const s: string);
begin
  EnsureCapacity(Count + System.Length(s));
  System.Move(Pointer(s)^, FBuffer[Count], System.Length(s));
  FCount += System.Length(s);
end;

procedure TJsonNode.TStrBuilder.AppendEncode(const s: string);
var
  I: SizeInt;
const
  HexChars: PChar = '0123456789ABCDEF';
begin
  Append('"');
  for I := 1 to System.Length(s) do
    case s[I] of
      #0..#7, #11, #14..#31:
        begin
           Append(chEscapeSym);
           Append(chUnicodeSym);
           Append(chZero);
           Append(chZero);
           Append(HexChars[Ord(s[I]) shr  4]);
           Append(HexChars[Ord(s[I]) and 15]);
        end;
      #8 : begin Append(chEscapeSym); Append(chBackSpSym) end; //backspace
      #9 : begin Append(chEscapeSym); Append(chTabSym) end;    //tab
      #10: begin Append(chEscapeSym); Append(chLineSym) end;   //line feed
      #12: begin Append(chEscapeSym); Append(chFormSym) end;   //form feed
      #13: begin Append(chEscapeSym); Append(chCarRetSym) end; //carriage return
      '"': begin Append(chEscapeSym); Append('"') end;         //quote
      '\': begin Append(chEscapeSym); Append('\') end;         //backslash
    else
      Append(s[I]);
    end;
  Append('"');
end;

procedure TJsonNode.TStrBuilder.Append(const s: shortstring);
begin
  EnsureCapacity(Count + System.Length(s));
  System.Move(s[1], FBuffer[Count], System.Length(s));
  FCount += System.Length(s);
end;

procedure TJsonNode.TStrBuilder.SaveToStream(aStream: TStream);
begin
  aStream.WriteBuffer(Pointer(FBuffer)^, Count);
  FCount := 0;
end;

function TJsonNode.TStrBuilder.ToString: string;
begin
  System.SetLength(Result, Count);
  System.Move(Pointer(FBuffer)^, Pointer(Result)^, Count);
  FCount := 0;
end;

type
  TChar2 = array[0..1] of AnsiChar;
  TChar3 = array[0..2] of AnsiChar;
  TChar4 = array[0..3] of AnsiChar;
  PChar2 = ^TChar2;
  PChar3 = ^TChar3;
  PChar4 = ^TChar4;

function UxSeqToUtf8(const uSeq: TChar4): TChar4; inline;
const
  xV: array['0'..'f'] of DWord = (
   0, 1, 2, 3, 4, 5, 6, 7, 8, 9,15,15,15,15,15,15,
  15,10,11,12,13,14,15,15,15,15,15,15,15,15,15,15,
  15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,
  15,10,11,12,13,14,15);
var
  cPt: DWord;
begin
  cPt := xV[uSeq[0]] shl 12 or xV[uSeq[1]] shl 8 or xV[uSeq[2]] shl 4 or xV[uSeq[3]];
  case cPt of
    0..$7f:
      begin
        Result[0] := AnsiChar(cPt);
        Result[3] := #1;
      end;
    $80..$7ff:
      begin
        Result[0] := AnsiChar((cPt shr  6) + $c0);
        Result[1] := AnsiChar((cPt and $3f) + $80);
        Result[3] := #2;
      end;
    $800..$ffff:
      begin
        Result[0] := AnsiChar((cPt shr 12) + $e0);
        Result[1] := AnsiChar(((cPt shr 6) and $3f) + $80);
        Result[2] := AnsiChar((cPt and $3f) + $80);
        Result[3] := #3;
      end;
  else
    Result[3] := #0;
  end;
end;

function TJsonNode.TStrBuilder.ToDecodeString: string;
var
  r: string = '';
  I, J, Last: SizeInt;
  pR: PAnsiChar;
  c4: TChar4;
begin
  System.SetLength(r, Count);
  Last := Pred(Count);
  I := 1;
  J := 0;
  pR := PAnsiChar(r);
  while I < Last do
    if FBuffer[I] <> '\' then
      begin
        pR[J] := FBuffer[I];
        Inc(I);
        Inc(J);
      end
    else
      case FBuffer[Succ(I)] of
        'b':
          begin
            pR[J] := #8;
            I += 2;
            Inc(J);
          end;
        'f':
          begin
            pR[J] := #12;
            I += 2;
            Inc(J);
          end;
        'n':
          begin
            pR[J] := #10;
            I += 2;
            Inc(J);
          end;
        'r':
          begin
            pR[J] := #13;
            I += 2;
            Inc(J);
          end;
        't':
          begin
            pR[J] := #9;
            I += 2;
            Inc(J);
          end;
        'u', 'U':
          begin
            c4 := UxSeqToUtf8(PChar4(@FBuffer[I+2])^);
            case c4[3] of
              #1:
                begin
                  pR[J] := c4[0];
                  Inc(J);
                end;
              #2:
                begin
                  PChar2(@pR[J])^ := PChar2(@c4[0])^;
                  J += 2;
                end;
              #3:
                begin
                  PChar3(@pR[J])^ := PChar3(@c4[0])^;
                  J += 3;
                end;
            else
            end;
            I += 6;
          end;
      else
        pR[J] := FBuffer[Succ(I)];
        I += 2;
        Inc(J)
      end;
  System.SetLength(r, J);
  Result := r;
  FCount := 0;
end;

{ TJsonNode.TEmptyNodeEnumerator }

function TJsonNode.TEmptyNodeEnumerator.GetCurrent: TJsonNode;
begin
  Result := nil;
end;

function TJsonNode.TEmptyNodeEnumerator.MoveNext: Boolean;
begin
  Result := False;
end;

procedure TJsonNode.TEmptyNodeEnumerator.Reset;
begin
end;

{ TJsonNode.TEmptyKeyEnumerator }

function TJsonNode.TEmptyKeyEnumerator.GetCurrent: string;
begin
  Result := '';
end;

function TJsonNode.TEmptyKeyEnumerator.MoveNext: Boolean;
begin
  Result := False;
end;

procedure TJsonNode.TEmptyKeyEnumerator.Reset;
begin
end;

{ TJsonNode.TEmptyPairEnumerator }

function TJsonNode.TEmptyPairEnumerator.GetCurrent: TPair;
begin
  Result := Default(TPair);
end;

function TJsonNode.TEmptyPairEnumerator.MoveNext: Boolean;
begin
  Result := False;
end;

procedure TJsonNode.TEmptyPairEnumerator.Reset;
begin
end;

{ TJsonNode.TANodeEnumerator }

function TJsonNode.TANodeEnumerator.GetCurrent: TJsonNode;
begin
  Result := FEnum.Current;
end;

constructor TJsonNode.TANodeEnumerator.Create(const aEnum: TJsArray.TEnumerator);
begin
  FEnum := aEnum;
  FResEnum := aEnum;
end;

function TJsonNode.TANodeEnumerator.MoveNext: Boolean;
begin
  Result := FEnum.MoveNext;
end;

procedure TJsonNode.TANodeEnumerator.Reset;
begin
  FEnum := FResEnum;
end;

{ TJsonNode.TONodeEnumerator }

function TJsonNode.TONodeEnumerator.GetCurrent: TJsonNode;
begin
  Result := FEnum.Current.Value;
end;

constructor TJsonNode.TONodeEnumerator.Create(const aEnum: TJsObject.TEnumerator);
begin
  FEnum := aEnum;
  FResEnum := aEnum;
end;

function TJsonNode.TONodeEnumerator.MoveNext: Boolean;
begin
  Result := FEnum.MoveNext;
end;

procedure TJsonNode.TONodeEnumerator.Reset;
begin
  FEnum := FResEnum;
end;

{ TJsonNode.TOPairEnumerator }

function TJsonNode.TOPairEnumerator.GetCurrent: TPair;
begin
  Result := FEnum.Current;
end;

constructor TJsonNode.TOPairEnumerator.Create(const aEnum: TJsObject.TEnumerator);
begin
  FEnum := aEnum;
  FConstEnum := aEnum;
end;

function TJsonNode.TOPairEnumerator.MoveNext: Boolean;
begin
  Result := FEnum.MoveNext;
end;

procedure TJsonNode.TOPairEnumerator.Reset;
begin
  FEnum := FConstEnum;
end;

{ TJsonNode.TOKeyEnumerator }

function TJsonNode.TOKeyEnumerator.GetCurrent: string;
begin
  Result := FEnum.Current.Key;
end;

constructor TJsonNode.TOKeyEnumerator.Create(const aEnum: TJsObject.TEnumerator);
begin
  FEnum := aEnum;
  FConstEnum := aEnum;
end;

function TJsonNode.TOKeyEnumerator.MoveNext: Boolean;
begin
  Result := FEnum.MoveNext;
end;

procedure TJsonNode.TOKeyEnumerator.Reset;
begin
  FEnum := FConstEnum;
end;

{ TJsonNode.TOIdenticEnumerator }

function TJsonNode.TOIdenticEnumerator.GetCurrent: TPair;
begin
  Result := FEnum.Current;
end;

constructor TJsonNode.TOIdenticEnumerator.Create(const aEnum: TJsObject.TIdenticEnumerator);
begin
  FEnum := aEnum;
end;

function TJsonNode.TOIdenticEnumerator.MoveNext: Boolean;
begin
  Result := FEnum.MoveNext;
end;

procedure TJsonNode.TOIdenticEnumerator.Reset;
begin
  FEnum.Reset;
end;

{ TJsonNode }

class function TJsonNode.CreateJsArray: PJsArray;
begin
  Result := System.GetMem(SizeOf(TJsArray));
  FillChar(Result^, SizeOf(TJsArray), 0);
  Result^.EnsureCapacity(ARRAY_INIT_SIZE);
end;

class procedure TJsonNode.FreeJsArray(a: PJsArray);
var
  I: SizeInt;
begin
  if a <> nil then
    begin
      for I := 0 to Pred(a^.Count) do
        a^.UncMutable[I]^.Free;
      System.Finalize(a^);
      FreeMem(a);
    end;
end;

class function TJsonNode.CreateJsObject: PJsObject;
begin
  Result := GetMem(SizeOf(TJsObject));
  FillChar(Result^, SizeOf(TJsObject), 0);
end;

class procedure TJsonNode.FreeJsObject(o: PJsObject);
var
  I: SizeInt;
begin
  if o <> nil then
    begin
      for I := 0 to Pred(o^.Count) do
        o^.Mutable[I]^.Value.Free;
      System.Finalize(o^);
      FreeMem(o);
    end;
end;

function TJsonNode.GetFString: string;
begin
  Result := string(FValue.Ref);
end;

function TJsonNode.GetFArray: PJsArray;
begin
  Result := FValue.Ref;
end;

function TJsonNode.GetFObject: PJsObject;
begin
  Result := FValue.Ref;
end;

procedure TJsonNode.SetFString(const aValue: string);
begin
  string(FValue.Ref) := aValue;
end;

procedure TJsonNode.SetFArray(aValue: PJsArray);
begin
  FValue.Ref := aValue;
end;

procedure TJsonNode.SetFObject(aValue: PJsObject);
begin
  FValue.Ref := aValue;
end;

procedure TJsonNode.DoClear;
begin
  case FKind of
    jvkNumber: FValue.Int := 0;
    jvkString: FString := '';
    jvkArray:
      begin
        FreeJsArray(FValue.Ref);
        FValue.Ref := nil;
      end;
    jvkObject:
      begin
        FreeJsObject(FValue.Ref);
        FValue.Ref := nil;
      end;
  else
  end;
end;

function TJsonNode.GetAsArray: TJsonNode;
begin
  if Kind <> jvkArray then
    begin
      DoClear;
      FKind := jvkArray;
    end;
  Result := Self;
end;

function TJsonNode.GetAsObject: TJsonNode;
begin
  if Kind <> jvkObject then
    begin
      DoClear;
      FKind := jvkObject;
    end;
  Result := Self;
end;

function TJsonNode.GetAsNull: TJsonNode;
begin
  if Kind <> jvkNull then
    begin
      DoClear;
      FKind := jvkNull;
    end;
  Result := Self;
end;

function TJsonNode.GetAsBoolean: Boolean;
begin
  if Kind = jvkTrue then
    exit(True);
  if Kind <> jvkFalse then
    begin
      DoClear;
      FKind := jvkFalse;
    end;
  Result := False;
end;

procedure TJsonNode.SetAsBoolean(aValue: Boolean);
begin
  if aValue then
    if Kind <> jvkTrue then
      begin
        DoClear;
        FKind := jvkTrue;
      end else
  else
    if Kind <> jvkFalse then
      begin
        DoClear;
        FKind := jvkFalse;
      end;
end;

function TJsonNode.GetAsNumber: Double;
begin
  if Kind <> jvkNumber then
    begin
      DoClear;
      FKind := jvkNumber;
    end;
  Result := FValue.Num;
end;

procedure TJsonNode.SetAsNumber(aValue: Double);
begin
  if Kind <> jvkNumber then
    begin
      DoClear;
      FKind := jvkNumber;
    end;
  FValue.Num := aValue;
end;

function TJsonNode.GetAsString: string;
begin
  if Kind <> jvkString then
    begin
      DoClear;
      FKind := jvkString;
    end;
  Result := FString;
end;

procedure TJsonNode.SetAsString(const aValue: string);
begin
  if Kind <> jvkString then
    begin
      DoClear;
      FKind := jvkString;
    end;
  FString := aValue;
end;

function TJsonNode.DoBuildJson: TStrBuilder;
var
  sb: TStrBuilder;
  e: TPair;
  procedure BuildJson(aInst: TJsonNode);
  var
    I, Last: SizeInt;
  begin
    case aInst.Kind of
      jvkNull:   sb.Append('null');
      jvkFalse:  sb.Append('false');
      jvkTrue:   sb.Append('true');
      jvkNumber: sb.Append(FloatToStr(aInst.FValue.Num, FmtSettings));
      jvkString: sb.AppendEncode(aInst.FString);
      jvkArray:
        begin
          sb.Append(chOpenSqrBr);
          if aInst.FArray <> nil then
            begin
              Last := Pred(aInst.FArray^.Count);
              for I := 0 to Last do
                begin
                  BuildJson(aInst.FArray^.UncMutable[I]^);
                  if I <> Last then
                    sb.Append(chComma);
                end;
            end;
          sb.Append(chClosSqrBr);
        end;
      jvkObject:
        begin
          sb.Append(chOpenCurBr);
          if aInst.FObject <> nil then
            begin
              Last := Pred(aInst.FObject^.Count);
              for I := 0 to Last do
                begin
                  e := aInst.FObject^.Mutable[I]^;
                  sb.AppendEncode(e.Key);
                  sb.Append(chColon);
                  BuildJson(e.Value);
                  if I <> Last then
                    sb.Append(chComma);
                end;
            end;
          sb.Append(chClosCurBr);
        end;
    else
    end;
  end;
begin
  sb := TStrBuilder.Create(SBUILDER_INIT_SIZE);
  BuildJson(Self);
  Result := sb;
end;

function TJsonNode.GetAsJson: string;
begin
  Result := DoBuildJson.ToString;
end;

procedure TJsonNode.SetAsJson(const aValue: string);
begin
  if not Parse(aValue) then
    raise EJsException.Create(SECantParseJsStr);
end;

function TJsonNode.GetCount: SizeInt;
begin
  case Kind of
    jvkArray:  if FArray <> nil then exit(FArray^.Count);
    jvkObject: if FObject <> nil then exit(FObject^.Count);
  else
    exit(0);
  end;
  Result := 0;
end;

function TJsonNode.CanArrayInsert(aIndex: SizeInt): Boolean;
begin
  if aIndex <> 0 then
    exit((Kind = jvkArray)and(FValue.Ref <> nil)and(SizeUInt(aIndex) <= SizeUInt(FArray^.Count)));
  if aIndex = 0 then
    begin
      if AsArray.FValue.Ref = nil then
        FValue.Ref := CreateJsArray;
      exit(True);
    end;
  Result := False;
end;

function TJsonNode.CanObjectInsert(aIndex: SizeInt): Boolean;
begin
  if aIndex <> 0 then
    exit((Kind = jvkObject)and(FValue.Ref <> nil)and(SizeUInt(aIndex) <= SizeUInt(FObject^.Count)));
  if aIndex = 0 then
    begin
      if AsObject.FValue.Ref = nil then
        FValue.Ref := CreateJsObject;
      exit(True);
    end;
  Result := False;
end;

function TJsonNode.GetItem(aIndex: SizeInt): TJsonNode;
begin
  if SizeUInt(aIndex) < SizeUInt(Count) then
    case Kind of
      jvkArray:  exit(FArray^.Mutable[aIndex]^);
      jvkObject: exit(FObject^.Mutable[aIndex]^.Value);
    else
    end
  else
    raise EJsException.CreateFmt(SEIndexOutOfBoundsFmt, [aIndex]);
  Result := nil;
end;

function TJsonNode.GetPair(aIndex: SizeInt): TPair;
begin
  if SizeUInt(aIndex) < SizeUInt(Count) then
    case Kind of
      jvkObject: exit(FObject^.Mutable[aIndex]^);
    else
      raise EJsException.Create(SEJsonInstNotObj);
    end
  else
    raise EJsException.CreateFmt(SEIndexOutOfBoundsFmt, [aIndex]);
  Result := Default(TPair);
end;

function TJsonNode.GetByName(const aName: string): TJsonNode;
begin
  if Kind = jvkObject then
    FindOrAdd(aName, Result)
  else
    Result := nil;
end;

function TJsonNode.GetValue(const aName: string): TJVariant;
var
  Node: TJsonNode;
begin
  if Find(aName, Node) then
    case Node.Kind of
      jvkUnknown,
      jvkNull:   exit(TJVariant.Null);
      jvkFalse:  exit(False);
      jvkTrue:   exit(True);
      jvkNumber: exit(Node.AsNumber);
      jvkString: exit(Node.AsString);
      jvkArray:  raise EJsException.CreateFmt(SECantConvertFmt, ['Array', 'TJVariant']);
      jvkObject: raise EJsException.CreateFmt(SECantConvertFmt, ['Object', 'TJVariant']);
    end
  else
    raise EJsException.Create(SEValueNotFound);
  Result.Clear;
end;

procedure TJsonNode.SetValue(const aName: string; const aValue: TJVariant);
var
  Node: TJsonNode;
begin
  FindOrAdd(aName, Node);
  case aValue.Kind of
    vkNull:   Node.AsNull;
    vkBool:   Node.AsBoolean := aValue.AsBoolean;
    vkNumber: Node.AsNumber := aValue.AsNumber;
    vkString: Node.AsString := aValue.AsString;
  end;
end;

function TJsonNode.GetClassEnumerator: TNodeEnumerator;
begin
  case Kind of
    jvkArray:
      if FValue.Ref <> nil then exit(TANodeEnumerator.Create(FArray^.GetEnumerator));
    jvkObject:
      if FValue.Ref <> nil then exit(TONodeEnumerator.Create(FObject^.GetEnumerator));
  else
  end;
  Result := TEmptyNodeEnumerator.Create;
end;

class constructor TJsonNode.Init;
begin
  FmtSettings := DefaultFormatSettings;
  FmtSettings.DecimalSeparator := '.';
end;

class function TJsonNode.ValidJson(const s: string; aDepth: Integer; aSkipBom: Boolean): Boolean;
var
  Stack: array[0..DEF_DEPTH] of TParseMode;
  DynStack: array of TParseMode = nil;
  Buf: PByte;
  Size: SizeInt;
begin
  if aDepth < 1 then exit(False);
  Buf := Pointer(s);
  Size := System.Length(s);
  if Size < 1 then exit(False);
  if aSkipBom then
    case DetectBom(Buf, Size) of
      bkNone: ;
      bkUtf8:
        begin
          if not aSkipBom then exit(False);
          Buf += UTF8_BOM_LEN;
          Size -= UTF8_BOM_LEN;
        end;
    else
      exit(False);
    end;
  if aDepth <= DEF_DEPTH then
    Result := ValidateBuf(Buf, Size, TOpenArray.Create(@Stack[0], aDepth + 1))
  else
    begin
      System.SetLength(DynStack, aDepth + 1);
      Result := ValidateBuf(Buf, Size, TOpenArray.Create(Pointer(DynStack), aDepth + 1))
    end;
end;

class function TJsonNode.ValidJson(aJson: TStream; aDepth: Integer; aSkipBom: Boolean): Boolean;
var
  Stack: array[0..DEF_DEPTH] of TParseMode;
  DynStack: array of TParseMode = nil;
begin
  if aDepth < 1 then exit(False);
  if aDepth <= DEF_DEPTH then
    Result := ValidateStream(aJson, aSkipBom, TOpenArray.Create(@Stack[0], aDepth + 1))
  else
    begin
      System.SetLength(DynStack, aDepth + 1);
      Result := ValidateStream(aJson, aSkipBom, TOpenArray.Create(Pointer(DynStack), aDepth + 1));
    end;
end;

class function TJsonNode.ValidJsonFile(const aFileName: string; aDepth: Integer; aSkipBom: Boolean): Boolean;
var
  fs: TFileStream;
begin
  fs := TFileStream.Create(aFileName, fmOpenRead);
  try
    Result := ValidJson(fs, aDepth, aSkipBom);
  finally
    fs.Free;
  end;
end;

class function TJsonNode.JsonStringValid(const s: string): Boolean;
var
  Stack: array[0..3] of TParseMode;
begin
  if System.Length(s) < 2 then
    exit(False);
  Result := ValidateStrBuf(Pointer(s), System.Length(s), TOpenArray.Create(@Stack[0], 1));
end;

class function TJsonNode.JsonNumberValid(const s: string): Boolean;
var
  Stack: array[0..3] of TParseMode;
begin
  if System.Length(s) < 1 then
    exit(False);
  Result := ValidateNumBuf(Pointer(s), System.Length(s), TOpenArray.Create(@Stack[0], 1));
end;

function DoParseStr(Buf: PAnsiChar; Size: SizeInt; aNode: TJsonNode; const aStack: TOpenArray): Boolean; forward;

type
  TParseNode = record
    Node: TJsonNode;
    Mode: TParseMode;
    constructor Create(aNode: TJsonNode; aMode: TParseMode);
  end;
  PParseNode = ^TParseNode;

constructor TParseNode.Create(aNode: TJsonNode; aMode: TParseMode);
begin
  Node := aNode;
  Mode := aMode;
end;

class function TJsonNode.TryParse(const s: string; out aRoot: TJsonNode; aDepth: Integer;
  aSkipBom: Boolean): Boolean;
var
  Stack: array[0..DEF_DEPTH] of TParseNode;
  DynStack: array of TParseNode = nil;
  Buf: PAnsiChar;
  Size: SizeInt;
begin
  aRoot := nil;
  if aDepth < 1 then exit(False);
  Buf := Pointer(s);
  Size := System.Length(s);
  if Size < 1 then exit(False);
  if aSkipBom then
    case DetectBom(Pointer(Buf), Size) of
      bkNone: ;
      bkUtf8:
        begin
          if not aSkipBom then
            exit(False);
          Buf += UTF8_BOM_LEN;
          Size -= UTF8_BOM_LEN;
        end;
    else
      exit(False);
    end;
  aRoot := TJsonNode.Create;
  try
    if aDepth <= DEF_DEPTH then
      Result :=
        DoParseStr(Buf, Size, aRoot, TOpenArray.Create(@Stack[0], aDepth + 1))
    else
      begin
        System.SetLength(DynStack, aDepth + 1);
        Result :=
          DoParseStr(Buf, Size, aRoot, TOpenArray.Create(Pointer(DynStack), aDepth + 1));
      end;
  except
    Result := False;
  end;
  if not Result then
    FreeAndNil(aRoot);
end;

class function TJsonNode.TryParse(aStream: TStream; out aRoot: TJsonNode; aDepth: Integer;
  aSkipBom: Boolean): Boolean;
var
  s: rawbytestring = '';
begin
  System.SetLength(s, aStream.Size - aStream.Position);
  aStream.ReadBuffer(Pointer(s)^, System.Length(s));
  Result := TryParse(s, aRoot, aDepth, aSkipBom);
end;

class function TJsonNode.TryParseFile(const aFileName: string; out aRoot: TJsonNode; aDepth: Integer;
  aSkipBom: Boolean): Boolean;
var
  s: rawbytestring = '';
begin
  with TStringStream.Create do
    try
      LoadFromFile(aFileName);
      s := DataString;
    finally
      Free;
    end;
  Result := TryParse(s, aRoot, aDepth, aSkipBom);
end;

class function TJsonNode.PasStrToJson(const s: string): string;
var
  sb: TStrBuilder;
begin
  Result := '';
  sb := TStrBuilder.Create(System.Length(s)*2);
  sb.AppendEncode(s);
  Result := sb.ToString;
end;

class function TJsonNode.JsonPtrEncode(aPath: TStringArray): string;
var
  sb: TStrBuilder;
  I, J: SizeInt;
begin
  Result := '';
  sb := TStrBuilder.Create(SBUILDER_INIT_SIZE);
  for I := 0 to System.High(aPath) do
    begin
      sb.Append('/');
      for J := 1 to System.Length(aPath[I]) do
        case aPath[I][J] of
          '/':
            begin
              sb.Append('~');
              sb.Append('1');
            end;
          '~':
            begin
              sb.Append('~');
              sb.Append('0');
            end;
        else
          sb.Append(aPath[I][J]);
        end;
    end;
  Result := sb.ToString;
end;

class function TJsonNode.JsonPtrDecode(const aPtr: string): TStringArray;
var
  sb: TStrBuilder;
  I, J: SizeInt;
begin
  if (aPtr = '') or (aPtr[1] <> '/') then
    exit(nil);
  if aPtr = '/' then
    exit(['']);
  Result := System.Copy(aPtr, 2, System.Length(aPtr)).Split(['/']);
  sb := TStrBuilder.Create(System.Length(aPtr));
  for I := 0 to System.High(Result) do
    begin
      J := 1;
      while J <= System.Length(Result[I]) do
        if Result[I][J] <> '~' then
          begin
            sb.Append(Result[I][J]);
            Inc(J);
          end
        else
          if J < System.Length(Result[I]) then
            if Result[I][J + 1] = '0' then
               begin
                 sb.Append('~');
                 J += 2;
               end
            else
              if Result[I][J + 1] = '1' then
                begin
                  sb.Append('/');
                  J += 2;
                end
              else
                raise EJsException.Create(SEInvalidJsPtr)
          else
            raise EJsException.Create(SEInvalidJsPtr);
      Result[I] := sb.ToString;
    end;
end;

class function TJsonNode.NewNode: TJsonNode;
begin
  Result := TJsonNode.Create;
end;

class function TJsonNode.NewNull: TJsonNode;
begin
  Result := TJsonNode.CreateNull;
end;

class function TJsonNode.NewNode(aValue: Boolean): TJsonNode;
begin
  Result := TJsonNode.Create(aValue);
end;

class function TJsonNode.NewNode(aValue: Double): TJsonNode;
begin
  Result := TJsonNode.Create(aValue);
end;

class function TJsonNode.NewNode(const aValue: string): TJsonNode;
begin
  Result := TJsonNode.Create(aValue);
end;

class function TJsonNode.NewNode(aKind: TJsValueKind): TJsonNode;
begin
  Result := TJsonNode.Create(aKind);
end;

class function TJsonNode.NewJson(const s: string): TJsonNode;
begin
  TryParse(s, Result);
end;

constructor TJsonNode.Create;
begin
  Assert(Kind = jvkUnknown);
end;

constructor TJsonNode.CreateNull;
begin
  FKind := jvkNull;
end;

constructor TJsonNode.Create(aValue: Boolean);
begin
  if aValue then
    FKind := jvkTrue
  else
    FKind := jvkFalse;
end;

constructor TJsonNode.Create(aValue: Double);
begin
  FValue.Num := aValue;
  FKind := jvkNumber;
end;

constructor TJsonNode.Create(const aValue: string);
begin
  FString := aValue;
  FKind := jvkString;
end;

constructor TJsonNode.Create(aKind: TJsValueKind);
begin
  FKind := aKind;
end;

constructor TJsonNode.Create(const a: TJVarArray);
var
  I: SizeInt;
begin
  FValue.Ref := CreateJsArray;
  FKind := jvkArray;
  for I := 0 to System.High(a) do
    case a[I].Kind of
      vkNull:   FArray^.Add(TJsonNode.CreateNull);
      vkBool:   FArray^.Add(TJsonNode.Create(Boolean(a[I])));
      vkNumber: FArray^.Add(TJsonNode.Create(Double(a[I])));
      vkString: FArray^.Add(TJsonNode.Create(string(a[I])));
    end;
end;

constructor TJsonNode.Create(const a: TJPairArray);
var
  I: SizeInt;
begin
  FValue.Ref := CreateJsObject;
  FKind := jvkObject;
  for I := 0 to System.High(a) do
    with a[I] do
      case Value.Kind of
        vkNull:   FObject^.Add(TPair.Create(Key, TJsonNode.CreateNull));
        vkBool:   FObject^.Add(TPair.Create(Key, TJsonNode.Create(Boolean(Value))));
        vkNumber: FObject^.Add(TPair.Create(Key, TJsonNode.Create(Double(Value))));
        vkString: FObject^.Add(TPair.Create(Key, TJsonNode.Create(string(Value))));
      end;
end;

destructor TJsonNode.Destroy;
begin
  DoClear;
  inherited;
end;

function TJsonNode.GetEnumerator: TEnumerator;
begin
  Result.FNode := Self;
  Result.FCurrIndex := NULL_INDEX;
end;

function TJsonNode.Nodes: INodeEnumerable;
begin
  Result := TNodes.Create(GetClassEnumerator);
end;

function TJsonNode.SubTree: TSubTree;
begin
  Result.FNode := Self;
end;

function TJsonNode.Enrties: IPairEnumerable;
begin
  if (Kind = jvkObject) and (FValue.Ref <> nil) then
    exit(TPairs.Create(TOPairEnumerator.Create(FObject^.GetEnumerator)));
  Result := TPairs.Create(TEmptyPairEnumerator.Create);
end;

function TJsonNode.Keys: IKeyEnumerable;
begin
  if (Kind = jvkObject) and (FValue.Ref <> nil) then
    exit(TKeys.Create(TOKeyEnumerator.Create(FObject^.GetEnumerator)));
  Result := TKeys.Create(TEmptyKeyEnumerator.Create);
end;

function TJsonNode.IdenticKeys(const aKey: string): IPairEnumerable;
begin
  if (Kind = jvkObject) and (FValue.Ref <> nil) then
    exit(TPairs.Create(TOIdenticEnumerator.Create(FObject^.GetIdenticEnumerator(aKey))));
  Result := TPairs.Create(TEmptyPairEnumerator.Create);
end;

function TJsonNode.IsNull: Boolean;
begin
  Result := Kind = jvkNull;
end;

function TJsonNode.IsFalse: Boolean;
begin
  Result := Kind = jvkFalse;
end;

function TJsonNode.IsTrue: Boolean;
begin
  Result := Kind = jvkTrue;
end;

function TJsonNode.IsNumber: Boolean;
begin
  Result := Kind = jvkNumber;
end;

function TJsonNode.IsString: Boolean;
begin
  Result := Kind = jvkString;
end;

function TJsonNode.IsBoolean: Boolean;
begin
  Result := Kind in [jvkFalse, jvkTrue];
end;

function TJsonNode.IsArray: Boolean;
begin
  Result := Kind = jvkArray;
end;

function TJsonNode.IsObject: Boolean;
begin
  Result := Kind = jvkObject;
end;

function TJsonNode.IsLiteral: Boolean;
begin
  Result := Kind in [jvkNull, jvkFalse, jvkTrue];
end;

function TJsonNode.IsScalar: Boolean;
begin
  Result := Kind in [jvkNull, jvkFalse, jvkTrue, jvkNumber, jvkString];
end;

function TJsonNode.IsStruct: Boolean;
begin
  Result := Kind in [jvkArray, jvkObject];
end;

{ TJsonNode.TEnumerator }

function TJsonNode.TEnumerator.GetCurrent: TJsonNode;
begin
  case FNode.Kind of
    jvkArray:  Result := FNode.FArray^.Mutable[FCurrIndex]^;
    jvkObject: Result := FNode.FObject^.Mutable[FCurrIndex]^.Value;
  else
    Result := nil;
  end;
end;

function TJsonNode.TEnumerator.MoveNext: Boolean;
begin
  Inc(FCurrIndex);
  Result := FCurrIndex < FNode.Count;
end;

{ TJsonNode.TTreeEnumerator }

function TJsonNode.TTreeEnumerator.GetCurrent: TJsonNode;
begin
  Result := FCurrent;
end;

function TJsonNode.TTreeEnumerator.MoveNext: Boolean;
var
  Node: TJsonNode;
begin
  if FQueue.TryDequeue(FCurrent) then
    begin
      for Node in FCurrent do
        FQueue.Enqueue(Node);
      exit(True);
    end;
  Result := False;
end;

{ TJsonNode.TSubTree }

function TJsonNode.TSubTree.GetEnumerator: TTreeEnumerator;
var
  Node: TJsonNode;
begin
  Result.FQueue := Default(TTreeEnumerator.TQueue);
  for Node in FNode do
    Result.FQueue.Enqueue(Node);
  Result.FCurrent := nil;
end;

procedure TJsonNode.Clear;
begin
  DoClear;
  FKind := jvkUnknown;
end;

function TJsonNode.Parse(const s: string): Boolean;
var
  Node: TJsonNode;
begin
  if not TryParse(s, Node) then
    exit(False);
  try
    Clear;
    case Node.FKind of
      jvkNumber:
        begin
          Self.FValue.Num := Node.FValue.Num;
          Node.FValue.Int := 0;
          FKind := jvkNumber;
        end;
      jvkString:
        begin
          FString := Node.FString;
          Node.FString := '';
          FKind := jvkString;
        end;
      jvkArray:
        begin
          FArray := Node.FArray;
          Node.FArray := nil;
          FKind := jvkArray;
        end;
      jvkObject:
        begin
          FObject := Node.FObject;
          Node.FObject := nil;
          FKind := jvkObject;
        end;
    else
      FKind := Node.Kind;
    end;
    Node.FKind := jvkUnknown;
    Result := True;
  finally
    Node.Free;
  end;
end;

function TJsonNode.AddNull: TJsonNode;
begin
  if AsArray.FValue.Ref = nil then
    FValue.Ref := CreateJsArray;
  FArray^.Add(TJsonNode.CreateNull);
  Result := Self;
end;

function TJsonNode.Add(aValue: Boolean): TJsonNode;
begin
  if AsArray.FValue.Ref = nil then
    FValue.Ref := CreateJsArray;
  FArray^.Add(TJsonNode.Create(aValue));
  Result := Self;
end;

function TJsonNode.Add(aValue: Double): TJsonNode;
begin
  if AsArray.FValue.Ref = nil then
    FValue.Ref := CreateJsArray;
  FArray^.Add(TJsonNode.Create(aValue));
  Result := Self;
end;

function TJsonNode.Add(const aValue: string): TJsonNode;
begin
  if AsArray.FValue.Ref = nil then
    FValue.Ref := CreateJsArray;
  FArray^.Add(TJsonNode.Create(aValue));
  Result := Self;
end;

function TJsonNode.Add(const a: TJVarArray): TJsonNode;
var
  I: SizeInt;
begin
  if AsArray.FValue.Ref = nil then
    FValue.Ref := CreateJsArray;
  for I := 0 to System.High(a) do
    case a[I].Kind of
      vkNull:   FArray^.Add(TJsonNode.CreateNull);
      vkBool:   FArray^.Add(TJsonNode.Create(Boolean(a[I])));
      vkNumber: FArray^.Add(TJsonNode.Create(Double(a[I])));
      vkString: FArray^.Add(TJsonNode.Create(string(a[I])));
    end;
  Result := Self;
end;

function TJsonNode.AddNode(aKind: TJsValueKind): TJsonNode;
begin
  if AsArray.FValue.Ref = nil then
    FValue.Ref := CreateJsArray;
  Result := TJsonNode.Create(aKind);
  FArray^.Add(Result);
end;

function TJsonNode.AddJson(const s: string; out aNode: TJsonNode): Boolean;
begin
  if AsArray.FValue.Ref = nil then
    FValue.Ref := CreateJsArray;
  Result := TryParse(s, aNode);
  if Result then
    FArray^.Add(aNode);
end;

function TJsonNode.Add(const a: TJPairArray): TJsonNode;
var
  I: SizeInt;
begin
  if AsObject.FValue.Ref = nil then
    FValue.Ref := CreateJsObject;
  for I := 0 to System.High(a) do
    with a[I] do
      case Value.Kind of
        vkNull:   FObject^.Add(TPair.Create(Key, TJsonNode.CreateNull));
        vkBool:   FObject^.Add(TPair.Create(Key, TJsonNode.Create(Boolean(Value))));
        vkNumber: FObject^.Add(TPair.Create(Key, TJsonNode.Create(Double(Value))));
        vkString: FObject^.Add(TPair.Create(Key, TJsonNode.Create(string(Value))));
      end;
  Result := Self;
end;

function TJsonNode.AddNull(const aName: string): TJsonNode;
begin
  if AsObject.FValue.Ref = nil then
    FValue.Ref := CreateJsObject;
  FObject^.Add(TPair.Create(aName, TJsonNode.CreateNull));
  Result := Self;
end;

function TJsonNode.Add(const aName: string; aValue: Boolean): TJsonNode;
begin
  if AsObject.FValue.Ref = nil then
    FValue.Ref := CreateJsObject;
  FObject^.Add(TPair.Create(aName, TJsonNode.Create(aValue)));
  Result := Self;
end;

function TJsonNode.Add(const aName: string; aValue: Double): TJsonNode;
begin
  if AsObject.FValue.Ref = nil then
    FValue.Ref := CreateJsObject;
  FObject^.Add(TPair.Create(aName, TJsonNode.Create(aValue)));
  Result := Self;
end;

function TJsonNode.Add(const aName, aValue: string): TJsonNode;
begin
  if AsObject.FValue.Ref = nil then
    FValue.Ref := CreateJsObject;
  FObject^.Add(TPair.Create(aName, TJsonNode.Create(aValue)));
  Result := Self;
end;

function TJsonNode.Add(const aName: string; const aValue: TJVarArray): TJsonNode;
begin
   if AsObject.FValue.Ref = nil then
    FValue.Ref := CreateJsObject;
  FObject^.Add(TPair.Create(aName, TJsonNode.Create(aValue)));
  Result := Self;
end;

function TJsonNode.Add(const aName: string; const aValue: TJPairArray): TJsonNode;
begin
  if AsObject.FValue.Ref = nil then
    FValue.Ref := CreateJsObject;
  FObject^.Add(TPair.Create(aName, TJsonNode.Create(aValue)));
  Result := Self;
end;

function TJsonNode.AddNode(const aName: string; aKind: TJsValueKind): TJsonNode;
begin
  if AsObject.FValue.Ref = nil then
    FValue.Ref := CreateJsObject;
  Result := TJsonNode.Create(aKind);
  FObject^.Add(TPair.Create(aName, Result));
end;

function TJsonNode.AddJson(const aName, aJson: string; out aNode: TJsonNode): Boolean;
begin
  if AsObject.FValue.Ref = nil then
    FValue.Ref := CreateJsObject;
  Result := TryParse(aJson, aNode);
  if Result then
    FObject^.Add(TPair.Create(aName, aNode));
end;

function TJsonNode.AddUniqNull(const aName: string): Boolean;
var
  p: ^TPair;
begin
  if AsObject.FValue.Ref = nil then
    FValue.Ref := CreateJsObject;
  if FObject^.AddUniq(TPair.Create(aName, nil), p) then
    begin
      p^.Value := TJsonNode.CreateNull;
      exit(True);
    end;
  Result := False;
end;

function TJsonNode.AddUniq(const aName: string; aValue: Boolean): Boolean;
var
  p: ^TPair;
begin
  if AsObject.FValue.Ref = nil then
    FValue.Ref := CreateJsObject;
  if FObject^.AddUniq(TPair.Create(aName, nil), p) then
    begin
      p^.Value := TJsonNode.Create(aValue);
      exit(True);
    end;
  Result := False;
end;

function TJsonNode.AddUniq(const aName: string; aValue: Double): Boolean;
var
  p: ^TPair;
begin
  if AsObject.FValue.Ref = nil then
    FValue.Ref := CreateJsObject;
  if FObject^.AddUniq(TPair.Create(aName, nil), p) then
    begin
      p^.Value := TJsonNode.Create(aValue);
      exit(True);
    end;
  Result := False;
end;

function TJsonNode.AddUniq(const aName, aValue: string): Boolean;
var
  p: ^TPair;
begin
  if AsObject.FValue.Ref = nil then
    FValue.Ref := CreateJsObject;
  if FObject^.AddUniq(TPair.Create(aName, nil), p) then
    begin
      p^.Value := TJsonNode.Create(aValue);
      exit(True);
    end;
  Result := False;
end;

function TJsonNode.AddUniq(const aName: string; const aValue: TJVarArray; out aNode: TJsonNode): Boolean;
var
  p: ^TPair;
begin
  if AsObject.FValue.Ref = nil then
    FValue.Ref := CreateJsObject;
  if FObject^.AddUniq(TPair.Create(aName, nil), p) then
    begin
      aNode := TJsonNode.Create(aValue);
      p^.Value := aNode;
      exit(True);
    end;
  aNode := nil;
  Result := False;
end;

function TJsonNode.AddUniq(const aName: string; const aValue: TJPairArray; out aNode: TJsonNode): Boolean;
var
  p: ^TPair;
begin
  if AsObject.FValue.Ref = nil then
    FValue.Ref := CreateJsObject;
  if FObject^.AddUniq(TPair.Create(aName, nil), p) then
    begin
      aNode := TJsonNode.Create(aValue);
      p^.Value := aNode;
      exit(True);
    end;
  aNode := nil;
  Result := False;
end;

function TJsonNode.AddUniqNode(const aName: string; out aNode: TJsonNode; aKind: TJsValueKind): Boolean;
var
  p: ^TPair;
begin
  if AsObject.FValue.Ref = nil then
    FValue.Ref := CreateJsObject;
  if FObject^.AddUniq(TPair.Create(aName, nil), p) then
    begin
      aNode := TJsonNode.Create(aKind);
      p^.Value := aNode;
      exit(True);
    end;
  aNode := nil;
  Result := False;
end;

function TJsonNode.AddUniqJson(const aName, aJson: string; out aNode: TJsonNode): Boolean;
var
  p: ^TPair;
begin
  if TryParse(aJson, aNode) then
    begin
      if AsObject.FValue.Ref = nil then
        FValue.Ref := CreateJsObject;
      if FObject^.AddUniq(TPair.Create(aName, nil), p) then
        begin
          p^.Value := aNode;
          exit(True);
        end
      else
        FreeAndNil(aNode);
    end;
  Result := False;
end;

function TJsonNode.InsertNull(aIndex: SizeInt): Boolean;
begin
  if CanArrayInsert(aIndex) then
    begin
      FArray^.Insert(aIndex, TJsonNode.CreateNull);
      exit(True);
    end;
  Result := False;
end;

function TJsonNode.Insert(aIndex: SizeInt; aValue: Boolean): Boolean;
begin
  if CanArrayInsert(aIndex) then
    begin
      FArray^.Insert(aIndex, TJsonNode.Create(aValue));
      exit(True);
    end;
  Result := False;
end;

function TJsonNode.Insert(aIndex: SizeInt; aValue: Double): Boolean;
begin
  if CanArrayInsert(aIndex) then
    begin
      FArray^.Insert(aIndex, TJsonNode.Create(aValue));
      exit(True);
    end;
  Result := False;
end;

function TJsonNode.Insert(aIndex: SizeInt; const aValue: string): Boolean;
begin
  if CanArrayInsert(aIndex) then
    begin
      FArray^.Insert(aIndex, TJsonNode.Create(aValue));
      exit(True);
    end;
  Result := False;
end;

function TJsonNode.InsertNode(aIndex: SizeInt; out aNode: TJsonNode; aKind: TJsValueKind): Boolean;
begin
  if CanArrayInsert(aIndex) then
    begin
      aNode := TJsonNode.Create(aKind);
      FArray^.Insert(aIndex, aNode);
      exit(True);
    end;
  aNode := nil;
  Result := False;
end;

function TJsonNode.InsertNull(aIndex: SizeInt; const aName: string): Boolean;
begin
  if CanObjectInsert(aIndex) then
    begin
      FObject^.Insert(aIndex, TPair.Create(aName, TJsonNode.CreateNull));
      exit(True);
    end;
  Result := False;
end;

function TJsonNode.Insert(aIndex: SizeInt; const aName: string; aValue: Boolean): Boolean;
begin
  if CanObjectInsert(aIndex) then
    begin
      FObject^.Insert(aIndex, TPair.Create(aName, TJsonNode.Create(aValue)));
      exit(True);
    end;
  Result := False;
end;

function TJsonNode.Insert(aIndex: SizeInt; const aName: string; aValue: Double): Boolean;
begin
  if CanObjectInsert(aIndex) then
    begin
      FObject^.Insert(aIndex, TPair.Create(aName, TJsonNode.Create(aValue)));
      exit(True);
    end;
  Result := False;
end;

function TJsonNode.Insert(aIndex: SizeInt; const aName, aValue: string): Boolean;
begin
  if CanObjectInsert(aIndex) then
    begin
      FObject^.Insert(aIndex, TPair.Create(aName, TJsonNode.Create(aValue)));
      exit(True);
    end;
  Result := False;
end;

function TJsonNode.InsertNode(aIndex: SizeInt; const aName: string; out aNode: TJsonNode;
  aKind: TJsValueKind): Boolean;
begin
  if CanObjectInsert(aIndex) then
    begin
      aNode := TJsonNode.Create(aKind);
      FObject^.Insert(aIndex, TPair.Create(aName, aNode));
      exit(True);
    end;
  aNode := nil;
  Result := False;
end;

function TJsonNode.Contains(const aName: string): Boolean;
begin
  if (Kind = jvkObject) and (FValue.Ref <> nil) then
    exit(FObject^.Contains(aName));
  Result := False;
end;

function TJsonNode.ContainsUniq(const aName: string): Boolean;
begin
  if (Kind = jvkObject) and (FValue.Ref <> nil) then
    exit(FObject^.ContainsUniq(aName));
  Result := False;
end;

function TJsonNode.IndefOfName(const aName: string): SizeInt;
begin
  if (Kind = jvkObject) and (FValue.Ref <> nil) then
    exit(FObject^.IndexOf(aName));
  Result := NULL_INDEX;
end;

function TJsonNode.CountOfName(const aName: string): SizeInt;
begin
  if (Kind = jvkObject) and (FValue.Ref <> nil) then
    exit(FObject^.CountOf(aName));
  Result := 0;
end;

function TJsonNode.Find(const aKey: string; out aValue: TJsonNode): Boolean;
var
  p: ^TPair;
begin
  if (Kind = jvkObject) and (FValue.Ref <> nil) then
    begin
      p := FObject^.Find(aKey);
      if p <> nil then
        begin
          aValue := p^.Value;
          exit(True);
        end;
    end;
  aValue := nil;
  Result := False;
end;

function TJsonNode.FindOrAdd(const aName: string; out aValue: TJsonNode): Boolean;
var
  p: ^TPair;
begin
  if AsObject.FValue.Ref = nil then
    FValue.Ref := CreateJsObject;
  Result := FObject^.FindOrAdd(aName, p);
  if not Result then
    begin
      p^.Key := aName;
      p^.Value := TJsonNode.Create;
    end;
  aValue := p^.Value;
end;

function TJsonNode.FindUniq(const aName: string; out aValue: TJsonNode): Boolean;
var
  p: ^TPair;
begin
  if (Kind = jvkObject) and (FValue.Ref <> nil) then
    begin
      p := FObject^.FindUniq(aName);
      if p <> nil then
        begin
          aValue := p^.Value;
          exit(True);
        end;
    end;
  aValue := nil;
  Result := False;
end;

function TJsonNode.FindAll(const aName: string): TNodeArray;
var
  r: TNodeArray = nil;
  I: SizeInt;
  e: TPair;
begin
  if (Kind = jvkObject) and (FValue.Ref <> nil) then
    begin
      System.SetLength(r, ARRAY_INITIAL_SIZE);
      I := 0;
      for e in FObject^.IdenticalKeys(aName) do
        begin
          if I = System.Length(r) then
            System.SetLength(r, I * 2);
          r[I] := e.Value;
          Inc(I);
        end;
      System.SetLength(r, I);
      exit(r);
    end;
  Result := nil;
end;

function TJsonNode.Find(aIndex: SizeInt; out aValue: TJsonNode): Boolean;
begin
  if SizeUInt(aIndex) < SizeUInt(Count) then
    begin
      case Kind of
        jvkArray:  aValue := FArray^.UncMutable[aIndex]^;
        jvkObject: aValue := FObject^.Mutable[aIndex]^.Value;
      else
      end;
      exit(True);
    end;
  aValue := nil;
  Result := False;
end;

function TJsonNode.FindPair(aIndex: SizeInt; out aValue: TPair): Boolean;
begin
  if (Kind=jvkObject)and(FObject<>nil)and(SizeUInt(aIndex)<SizeUInt(FObject^.Count))then
     begin
       aValue := FObject^.Mutable[aIndex]^;
       exit(True);
     end;
  aValue := Default(TPair);
  Result := False;
end;

function TJsonNode.FindName(aIndex: SizeInt; out aName: string): Boolean;
begin
  if (Kind=jvkObject)and(FObject<>nil)and(SizeUInt(aIndex)<SizeUInt(FObject^.Count))then
     begin
       aName := FObject^.Mutable[aIndex]^.Key;
       exit(True);
     end;
  aName := '';
  Result := False;
end;

function TJsonNode.Delete(aIndex: SizeInt): Boolean;
var
  p: TPair;
begin
  if SizeUInt(aIndex) < SizeUInt(Count) then
    begin
      case Kind of
        jvkArray:  FArray^.Extract(aIndex).Free;
        jvkObject:
          begin
            FObject^.Delete(aIndex, p);
            p.Value.Free;
          end;
      else
      end;
      exit(True);
    end;
  Result := False;
end;

function TJsonNode.Remove(const aName: string): Boolean;
var
  p: TPair;
begin
  if (Kind = jvkObject) and (FObject <> nil) and FObject^.Remove(aName, p) then
    begin
      p.Value.Free;
      exit(True);
    end;
  Result := False;
end;

function TJsonNode.RemoveAll(const aName: string): SizeInt;
var
  p: TPair;
begin
  p := Default(TPair);
  if (Kind = jvkObject) and (FObject <> nil) then
    begin
      Result := FObject^.Count;
      while FObject^.Remove(aName, p) do
        p.Value.Free;
      Result := Result - FObject^.Count;
    end;
  Result := 0;
end;

function TJsonNode.FindPath(const aPtr: string; out aNode: TJsonNode): Boolean;
var
  Path: TStringArray;
begin
  if aPtr = '' then
    begin
      aNode := Self;
      exit(True);
    end;
  aNode := nil;
  Path := JsonPtrDecode(aPtr);
  if Path = nil then exit(False);
  Result := FindPath(Path, aNode);
end;

function TJsonNode.FindPath(const aPath: array of string; out aNode: TJsonNode): Boolean;
var
  Node: TJsonNode;
  I, Idx: SizeInt;
begin
  if System.Length(aPath) = 0 then
    begin
      aNode := Self;
      exit(True);
    end;
  Node := Self;
  aNode := nil;
  for I := 0 to System.High(aPath) do
    begin
      if aPath[I] = '' then
         exit(Node.FindUniq('', aNode));
      if Node.IsArray then
        begin
          if System.Length(aPath[I]) = 1 then
            begin
              if aPath[I] = '-' then
                begin
                  if I <> System.High(aPath) then
                    exit(False);
                  aNode := Node.AddNode(jvkNull);
                  exit(True);
                end;
              if not (aPath[I][1] in ['0','1','2','3','4','5','6','7','8','9']) then
                exit(False);
            end
          else
            //leading zeros are not allowed
            if aPath[I][1] = '0' then
              exit(False);
          Idx := StrToIntDef(aPath[I], -1);
          if not Node.Find(Idx, Node) then
            exit(False);
        end
      else
        if Node.IsObject and not Node.FindUniq(aPath[I], Node) then
          exit(False);
    end;
  aNode := Node;
  Result := Node <> nil;
end;

function TJsonNode.FormatJson(aOptions: TJsFormatOptions; aIndent: Integer): string;
var
  sb: TStrBuilder;
  Pair: TPair;
  MultiLine, UseTabs, StrEncode, BsdBrace, HasText: Boolean;
  procedure NewLine(Pos: Integer); inline;
  begin
    sb.Append(sLineBreak);
    if UseTabs then
      sb.Append(#9, Pos)
    else
      sb.Append(chSpace, Pos);
  end;
  procedure AppendString(const s: string); inline;
  begin
    if StrEncode then
      sb.AppendEncode(s)
    else
      begin
        sb.Append(chQuote);
        sb.Append(s);
        sb.Append(chQuote);
      end;
  end;
  procedure CheckHasText(Pos: Integer); inline;
  begin
    if HasText then
      if MultiLine and BsdBrace then
        NewLine(Pos) else
    else
      HasText := True;
  end;
  procedure BuildJson(aInst: TJsonNode; aPos: Integer);
  var
    I, Last: SizeInt;
  begin
    case aInst.Kind of
      jvkNull:   sb.Append('null');
      jvkFalse:  sb.Append('false');
      jvkTrue:   sb.Append('true');
      jvkNumber: sb.Append(FloatToStr(aInst.AsNumber, FmtSettings));
      jvkString: AppendString(aInst.FString);
      jvkArray: begin
          CheckHasText(aPos);
          sb.Append(chOpenSqrBr);
          if aInst.FArray <> nil then begin
            Last := Pred(aInst.FArray^.Count);
            for I := 0 to Last do begin
              if aInst.FArray^.UncMutable[I]^.IsScalar and MultiLine then
                NewLine(aPos + aIndent);
              BuildJson(aInst.FArray^.UncMutable[I]^, aPos + aIndent);
              if I <> Last then begin
                sb.Append(chComma);
                if not MultiLine or
                  (aInst.FArray^.UncMutable[I+1]^.IsStruct and not BsdBrace) then
                  sb.Append(chSpace);
              end;
            end;
          end;
          if MultiLine then NewLine(aPos);
          sb.Append(chClosSqrBr);
        end;
      jvkObject: begin
          CheckHasText(aPos);
          sb.Append(chOpenCurBr);
          if aInst.FObject <> nil then begin
            Last := Pred(aInst.FObject^.Count);
            for I := 0 to Last do begin
              if MultiLine then NewLine(aPos + aIndent);
              Pair := aInst.FObject^.Mutable[I]^;
              AppendString(Pair.Key);
              sb.Append(chColon);
              if Pair.Value.IsScalar or not MultiLine then begin
                sb.Append(chSpace);
                BuildJson(Pair.Value, aPos);
              end
              else begin
                if not BsdBrace then sb.Append(chSpace);
                BuildJson(Pair.Value, aPos + aIndent);
              end;
              if I <> Last then begin
                sb.Append(chComma);
                if not MultiLine then sb.Append(chSpace);
              end;
            end;
          end;
          if MultiLine then NewLine(aPos);
          sb.Append(chClosCurBr);
        end;
    else
    end;
  end;
begin
  sb := TStrBuilder.Create(SBUILDER_INIT_SIZE);
  MultiLine := not (jfoSingleLine in aOptions);
  UseTabs := jfoUseTabs in aOptions;
  StrEncode := not (jfoStrAsIs in aOptions);
  BsdBrace := not (jfoEgyptBrace in aOptions);
  HasText := False;
  BuildJson(Self, 0);
  Result := sb.ToString;
end;

procedure TJsonNode.SaveToStream(aStream: TStream);
begin
  DoBuildJson.SaveToStream(aStream);
end;

procedure TJsonNode.SaveToFile(const aFileName: string);
var
  fs: TFileStream = nil;
begin
  fs := TFileStream.Create(aFileName, fmOpenWrite or fmCreate);
  try
    SaveToStream(fs);
  finally
    fs.Free;
  end;
end;

const

  StateTransitions: array[GO..N3, Space..Etc] of Integer = (
{
  The state transition table takes the current state and the current symbol,
  and returns either a new state or an action. An action is represented as a
  number > 30.

             white                                      1-9                                   ABCDF  etc
         space |  {  }  [  ]  :  ,  "  \  /  +  -  .  0  |  a  b  c  d  e  f  l  n  r  s  t  u  |  E  | }
{start  GO}(GO,GO,34,__,35,__,__,__,ST,__,__,__,MI,__,ZE,IR,__,__,__,__,__,F1,__,N1,__,__,T1,__,__,__,__),
{ok     OK}(OK,OK,__,32,__,33,__,37,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__),
{object OB}(OB,OB,__,31,__,__,__,__,ST,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__),
{key    KE}(KE,KE,__,__,__,__,__,__,ST,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__),
{colon  CO}(CO,CO,__,__,__,__,38,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__),
{value  VA}(VA,VA,34,__,35,__,__,__,ST,__,__,__,MI,__,ZE,IR,__,__,__,__,__,F1,__,N1,__,__,T1,__,__,__,__),
{array  AR}(AR,AR,34,__,35,33,__,__,ST,__,__,__,MI,__,ZE,IR,__,__,__,__,__,F1,__,N1,__,__,T1,__,__,__,__),
{string ST}(ST,__,ST,ST,ST,ST,ST,ST,36,ES,ST,ST,ST,ST,ST,ST,ST,ST,ST,ST,ST,ST,ST,ST,ST,ST,ST,ST,ST,ST,ST),
{escape ES}(__,__,__,__,__,__,__,__,ST,ST,ST,__,__,__,__,__,__,ST,__,__,__,ST,__,ST,ST,__,ST,U1,__,__,__),
{u1     U1}(__,__,__,__,__,__,__,__,__,__,__,__,__,__,U2,U2,U2,U2,U2,U2,U2,U2,__,__,__,__,__,__,U2,U2,__),
{u2     U2}(__,__,__,__,__,__,__,__,__,__,__,__,__,__,U3,U3,U3,U3,U3,U3,U3,U3,__,__,__,__,__,__,U3,U3,__),
{u3     U3}(__,__,__,__,__,__,__,__,__,__,__,__,__,__,U4,U4,U4,U4,U4,U4,U4,U4,__,__,__,__,__,__,U4,U4,__),
{u4     U4}(__,__,__,__,__,__,__,__,__,__,__,__,__,__,ST,ST,ST,ST,ST,ST,ST,ST,__,__,__,__,__,__,ST,ST,__),
{minus  MI}(__,__,__,__,__,__,__,__,__,__,__,__,__,__,ZE,IR,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__),
{zero   ZE}(40,40,__,32,__,33,__,39,__,__,__,__,__,FR,__,__,__,__,__,__,E1,__,__,__,__,__,__,__,__,E1,__),
{int    IR}(40,40,__,32,__,33,__,39,__,__,__,__,__,FR,IR,IR,__,__,__,__,E1,__,__,__,__,__,__,__,__,E1,__),
{frac   FR}(__,__,__,__,__,__,__,__,__,__,__,__,__,__,FS,FS,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__),
{fracs  FS}(40,40,__,32,__,33,__,39,__,__,__,__,__,__,FS,FS,__,__,__,__,E1,__,__,__,__,__,__,__,__,E1,__),
{e      E1}(__,__,__,__,__,__,__,__,__,__,__,E2,E2,__,E3,E3,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__),
{ex     E2}(__,__,__,__,__,__,__,__,__,__,__,__,__,__,E3,E3,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__),
{exp    E3}(40,40,__,32,__,33,__,39,__,__,__,__,__,__,E3,E3,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__),
{tr     T1}(__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,T2,__,__,__,__,__,__),
{tru    T2}(__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,T3,__,__,__),
{true   T3}(__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,41,__,__,__,__,__,__,__,__,__,__),
{fa     F1}(__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,F2,__,__,__,__,__,__,__,__,__,__,__,__,__,__),
{fal    F2}(__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,F3,__,__,__,__,__,__,__,__),
{fals   F3}(__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,F4,__,__,__,__,__),
{false  F4}(__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,42,__,__,__,__,__,__,__,__,__,__),
{nu     N1}(__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,N2,__,__,__),
{nul    N2}(__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,N3,__,__,__,__,__,__,__,__),
{null   N3}(__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,43,__,__,__,__,__,__,__,__)
  );

function DoParseStr(Buf: PAnsiChar; Size: SizeInt; aNode: TJsonNode; const aStack: TOpenArray): Boolean;
const
  INF_EXP = QWord($7ff0000000000000);
var
  Stack: PParseNode;
  I: SizeInt;
  NextState, NextClass, StackHigh: Integer;
  State: Integer = GO;
  sTop: Integer = 1;
  sb: TJsonNode.TStrBuilder;
  KeyValue: string = '';
  function Number: Double; inline;
  var
    e: Integer;
  begin
    Val(sb.ToString, Result, e);
    if (e <> 0) or (QWord(Result) and INF_EXP = INF_EXP) then
      Abort;
  end;
begin
  Stack := aStack.Data;
  StackHigh := Pred(aStack.Size);
  Stack[0].Create(nil, pmNone);
  Stack[1].Create(aNode, pmNone);
  sb := TJsonNode.TStrBuilder.Create(TJsonNode.SBUILDER_INIT_SIZE);
  for I := 0 to Pred(Size) do
    begin
      if Buf[I] < #128 then
        begin
          NextClass := SymClassTable[Ord(Buf[I])];
          if NextClass = __ then exit(False);
        end
      else
        NextClass := Etc;
      NextState := StateTransitions[State, NextClass];
      if NextState <> -1 then
        if NextState < 31 then
          begin
            if DWord(NextState - ST) < DWord(14) then
              sb.Append(Buf[I]);
            State := NextState;
          end
        else
          case NextState of
            31: //end object - state = object
              if Stack[sTop].Mode = pmKey then begin
                Dec(sTop);
                State := OK;
              end else exit(False);
            32: //end object
              if Stack[sTop].Mode = pmObject then begin
                if (State = IR) or (State = FS) or (State = E3) then
                  Stack[sTop].Node.Add(KeyValue, Number);
                Dec(sTop);
                State := OK;
              end else exit(False);
            33: //end array
              if Stack[sTop].Mode = pmArray then begin
                if (State = IR) or (State = FS) or (State = E3) then
                  Stack[sTop].Node.Add(Number);
                Dec(sTop);
                State := OK;
              end else exit(False);
            34: //begin object
              if sTop < StackHigh then begin
                case Stack[sTop].Mode of
                  pmNone: begin
                      Stack[sTop].Node.AsObject;
                      Stack[sTop].Mode := pmKey;
                    end;
                  pmArray: begin
                      Stack[sTop+1].Create(Stack[sTop].Node.AddNode(jvkObject), pmKey);
                      Inc(sTop);
                    end;
                  pmObject: begin
                      Stack[sTop+1].Create(Stack[sTop].Node.AddNode(KeyValue, jvkObject), pmKey);
                      Inc(sTop);
                    end;
                else
                  exit(False);
                end;
                State := OB;
              end else exit(False);
            35: //begin array
              if sTop < StackHigh then begin
                case Stack[sTop].Mode of
                  pmNone: begin
                      Stack[sTop].Node.AsArray;
                      Stack[sTop].Mode := pmArray;
                    end;
                  pmArray: begin
                      Stack[sTop+1].Create(Stack[sTop].Node.AddNode(jvkArray), pmArray);
                      Inc(sTop);
                    end;
                  pmObject: begin
                      Stack[sTop+1].Create(Stack[sTop].Node.AddNode(KeyValue, jvkArray), pmArray);
                      Inc(sTop);
                    end;
                else
                  exit(False);
                end;
                State := AR;
              end else exit(False);
            36: //string value
              begin
                sb.Append(Buf[I]);
                case Stack[sTop].Mode of
                  pmKey: begin
                      KeyValue := sb.ToDecodeString;
                      State := CO;
                    end;
                  pmArray: begin
                      Stack[sTop].Node.Add(sb.ToDecodeString);
                      State := OK;
                    end;
                  pmObject: begin
                      Stack[sTop].Node.Add(KeyValue, sb.ToDecodeString);
                      State := OK;
                    end
                else
                  Stack[sTop].Node.AsString := sb.ToDecodeString;
                  Dec(sTop);
                  State := OK;
                end;
              end;
            37: //OK - comma
              case Stack[sTop].Mode of
                pmObject: begin
                    Stack[sTop].Mode := pmKey;
                    State := KE;
                  end;
                pmArray: State := VA;
              else
                exit(False);
              end;
            38: //colon
              if Stack[sTop].Mode = pmKey then begin
                Stack[sTop].Mode := pmObject;
                State := VA;
              end else exit(False);
            39: //end Number - comma
              case Stack[sTop].Mode of
                pmArray: begin
                    Stack[sTop].Node.Add(Number);
                    State := VA;
                  end;
                pmObject: begin
                    Stack[sTop].Node.Add(KeyValue, Number);
                    Stack[sTop].Mode := pmKey;
                    State := KE;
                  end;
              else
                exit(False);
              end;
            40: //end Number - white space
              begin
                case Stack[sTop].Mode of
                  pmArray:  Stack[sTop].Node.Add(Number);
                  pmObject: Stack[sTop].Node.Add(KeyValue, Number);
                else
                  //if Stack[sTop].Mode = pmNone then ????
                  Stack[sTop].Node.AsNumber := Number;
                  Dec(sTop);
                end;
                State := OK;
              end;
            41: //true literal
              begin
                case Stack[sTop].Mode of
                  pmArray:  Stack[sTop].Node.Add(True);
                  pmObject: Stack[sTop].Node.Add(KeyValue, True);
                else
                  Stack[sTop].Node.AsBoolean := True;
                  Dec(sTop);
                end;
                State := OK;
              end;
            42: //false literal
              begin
                case Stack[sTop].Mode of
                  pmArray:  Stack[sTop].Node.Add(False);
                  pmObject: Stack[sTop].Node.Add(KeyValue, False);
                else
                  Stack[sTop].Node.AsBoolean := False;
                  Dec(sTop);
                end;
                State := OK;
              end;
            43: //null literal
              begin
                case Stack[sTop].Mode of
                  pmArray:  Stack[sTop].Node.AddNull;
                  pmObject: Stack[sTop].Node.AddNull(KeyValue);
                else
                  Stack[sTop].Node.AsNull;
                  Dec(sTop);
                end;
                State := OK;
              end;
          else
            exit(False);
          end
      else
        exit(False);
    end;
  if State in [IR, FS, E3] then
    begin
      if Stack[sTop].Mode <> pmNone then exit(False);
      Stack[sTop].Node.AsNumber := Number;
      State := OK;
      Dec(sTop);
    end;
  Result := (State = OK) and (sTop = 0) and (Stack[0].Node = nil) and (Stack[0].Mode = pmNone);
end;

end.