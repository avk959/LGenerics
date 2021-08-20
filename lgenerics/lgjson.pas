{****************************************************************************
*                                                                           *
*   This file is part of the LGenerics package.                             *
*   JSON parser and utilites that try to follow RFC 8259.                   *
*                                                                           *
*   Copyright(c) 2020-2021 A.Koverdyaev(avk)                                *
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
  Classes, SysUtils, Math, BufStream,
  lgUtils,
  lgAbstractContainer,
  lgQueue,
  lgVector,
  lgList,
  lgStack,
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
    class operator := (const v: TJVariant): Int64; inline;
    class operator := (const v: TJVariant): Boolean; inline;
    class operator := (const v: TJVariant): string; inline;
    class operator = (const L, R: TJVariant): Boolean; inline;
    procedure Clear;
    procedure SetNull; inline;
    function  IsInteger: Boolean; inline;
  { returns a Boolean value of the instance; raises an exception if Kind <> vkBoolean }
    function  AsBoolean: Boolean; inline;
  { returns a numeric value of the instance; raises an exception if Kind <> vkNumber }
    function  AsNumber: Double; inline;
  { returns a integer value of the instance; raises an exception if Kind <> vkNumber
    or value is not exact integer }
    function  AsInteger: Int64; inline;
  { returns a string value of the instance; raises an exception if Kind <> vkString }
    function  AsString: string; inline;
  { returns a string representation of the instance }
    function  ToString: string; inline;
    property  Kind: TJVarKind read FKind;
  end;

  TJVarPair   = specialize TGMapEntry<string, TJVariant>;
  TJVarArray  = array of TJVariant;
  TJPairArray = array of TJVarPair;

function JNull: TJVariant; inline;
function JPair(const aName: string; const aValue: TJVariant): TJVarPair; inline;

type

  { TJsonPtr represents JSON Pointer(RFC 6901) }
  TJsonPtr = record
  private
    FSegments: TStringArray;
    function GetCount: SizeInt; inline;
    function GetSegment(aIndex: SizeInt): string; inline;
    class function Encode(const aSegs: TStringArray): string; static;
    class function Decode(const s: string): TStringArray; static;
  public
  type
    TEnumerator = record
    private
      FList: TStringArray;
      FIndex: SizeInt;
      function GetCurrent: string; inline;
    public
      function MoveNext: Boolean; inline;
      property Current: string read GetCurrent;
    end;
  { checks if a Pascal string s is a well-formed JSON Pointer }
    class function ValidPtr(const s: string): Boolean; static;
  { checks if a JSON string s is a well-formed JSON Pointer }
    class function ValidAlien(const s: string): Boolean; static;
    class function ToSegments(const s: string): TStringArray; static;
    class function ToString(const a: TStringArray): string; static;
    class operator = (const L, R: TJsonPtr): Boolean;
  { constructs a pointer from Pascal string, treats slash("/")
    as a path delimiter and "~" as a special character;
    use it only if the segments do not contain a slash or tilde;
    raises an exception if s is not a well-formed JSON Pointer }
    constructor From(const s: string);
  { constructs a pointer from path segments as Pascal strings }
    constructor From(const aPath: TStringArray);
  { constructs a pointer from JSON string, treats slash("/")
    as a path delimiter and "~" as a special character;
    raises an exception if s is not a well-formed JSON Pointer }
    constructor FromAlien(const s: string);
    function  GetEnumerator: TEnumerator; inline;
    function  IsEmpty: Boolean; inline;
    function  NonEmpty: Boolean; inline;
    procedure Clear; inline;
    procedure Append(const aSegment: string); inline;
  { returns a pointer as a Pascal string }
    function  ToString: string; inline;
  { returns a pointer as a JSON string }
    function  ToAlien: string;
    function  ToSegments: TStringArray; inline;
    property  Count: SizeInt read GetCount;
    property  Segments[aIndex: SizeInt]: string read GetSegment; default;
  end;

const
  DEF_INDENT = 4;

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
    TNodeArray      = array of TJsonNode;
    IPairEnumerable = specialize IGEnumerable<TPair>;

  private
  class var
    FmtSettings: TFormatSettings;

  const
    S_BUILD_INIT_SIZE = 256;
    RW_BUF_SIZE       = 65536;

  type
    TStrBuilder = record
    private
      FBuffer: array of AnsiChar;
      FCount: SizeInt;
    public
      constructor Create(aCapacity: SizeInt);
      constructor Create(const s: string);
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
      function  ToPChar: PAnsiChar; inline;
      property  Count: SizeInt read FCount;
    end;

    TJsArray        = specialize TGLiteVector<TJsonNode>;
    TJsObject       = specialize TGLiteHashList2<string, TPair, string>;
    PJsArray        = ^TJsArray;
    PJsObject       = ^TJsObject;
    TPairEnumerator = specialize TGEnumerator<TPair>;
    TPairs          = specialize TGEnumCursor<TPair>;
    TRwBuffer       = array[0..Pred(RW_BUF_SIZE div SizeOf(SizeUInt))] of SizeUInt;

    TEmptyPairEnumerator = class(TPairEnumerator)
    protected
      function  GetCurrent: TPair; override;
    public
      function  MoveNext: Boolean; override;
      procedure Reset; override;
    end;

    TEqualEnumerator = class(TPairEnumerator)
    protected
      FEnum: TJsObject.TEqualEnumerator;
      function  GetCurrent: TPair; override;
    public
      constructor Create(const aEnum: TJsObject.TEqualEnumerator);
      function  MoveNext: Boolean; override;
      procedure Reset; override;
    end;

    TValue = record
    case Integer of
      0: (Ref: Pointer);
      1: (Num: Double);
      2: (Int: Int64);
    end;

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
    procedure SetNArray(const aName: string; const aValue: TJVarArray);
    procedure SetNObject(const aName: string; const aValue: TJPairArray);
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

    TEntryEnumerator = record
    private
      FNode: TJsonNode;
      FCurrIndex: SizeInt;
      function GetCurrent: TPair; inline;
    public
      function MoveNext: Boolean; inline;
      property Current: TPair read GetCurrent;
    end;

    TEntries = record
    private
      FNode: TJsonNode;
    public
      function GetEnumerator: TEntryEnumerator;
    end;

    TNameEnumerator = record
    private
      FNode: TJsonNode;
      FCurrIndex: SizeInt;
      function GetCurrent: string; inline;
    public
      function MoveNext: Boolean; inline;
      property Current: string read GetCurrent;
    end;

    TNames = record
    private
      FNode: TJsonNode;
    public
      function GetEnumerator: TNameEnumerator;
    end;

  { checks if the content is well-formed JSON; aDepth indicates the maximum allowable
    nesting depth of structures; if aSkipBom is set to True then UTF-8 BOM(and only that)
    will be ignored }
    class function ValidJson(const s: string; aDepth: Integer = DEF_DEPTH;
                             aSkipBom: Boolean = False): Boolean; static;
    class function ValidJson(aStream: TStream; aDepth: Integer = DEF_DEPTH;
                             aSkipBom: Boolean = False): Boolean; static;
    class function ValidJson(aStream: TStream; aCount: SizeInt; aDepth: Integer = DEF_DEPTH;
                             aSkipBom: Boolean = False): Boolean; static;
    class function ValidJsonFile(const aFileName: string; aDepth: Integer = DEF_DEPTH;
                                 aSkipBom: Boolean = False): Boolean; static;
  { checks if s represents a valid JSON string }
    class function JsonStringValid(const s: string): Boolean; static;
  { checks if s represents a valid JSON number }
    class function JsonNumberValid(const s: string): Boolean; static;
    class function LikelyKind(aBuf: PAnsiChar; aSize: SizeInt): TJsValueKind; static;
  { returns the parsing result; if the result is True, then the created
    object is returned in the aRoot parameter, otherwise nil is returned }
    class function TryParse(const s: string; out aRoot: TJsonNode;
                            aDepth: Integer = DEF_DEPTH; aSkipBom: Boolean = False): Boolean; static;
    class function TryParse(aStream: TStream; out aRoot: TJsonNode;
                            aDepth: Integer = DEF_DEPTH; aSkipBom: Boolean = False): Boolean; static;
    class function TryParse(aStream: TStream; aCount: SizeInt; out aRoot: TJsonNode;
                            aDepth: Integer = DEF_DEPTH; aSkipBom: Boolean = False): Boolean; static;
    class function TryParseFile(const aFileName: string; out aRoot: TJsonNode;
                            aDepth: Integer = DEF_DEPTH; aSkipBom: Boolean = False): Boolean; static;
  { converts a pascal string to a JSON string }
    class function PasStrToJson(const s: string): string; static;
    class function NewNode: TJsonNode; static; inline;
    class function NewNull: TJsonNode; static; inline;
    class function NewNode(aValue: Boolean): TJsonNode; static; inline;
    class function NewNode(aValue: Double): TJsonNode; static; inline;
    class function NewNode(const aValue: string): TJsonNode; static; inline;
    class function NewNode(aKind: TJsValueKind): TJsonNode; static; inline;
    class function NewNode(aNode: TJsonNode): TJsonNode; static; inline;
  { parses the JSON string s, returns nil in case of failure,
    otherwise its representation as TJsonNode }
    class function NewJson(const s: string): TJsonNode; static; inline;
  { returns the maximum nesting depth of aNode, is recursive }
    class function MaxNestDepth(aNode: TJsonNode): SizeInt; static;
  { returns True if aNode has no non-unique names, is recursive }
    class function HasUniqueNames(aNode: TJsonNode): Boolean; static;
    constructor Create;
    constructor CreateNull;
    constructor Create(aValue: Boolean);
    constructor Create(aValue: Double);
    constructor Create(const aValue: string);
    constructor Create(aKind: TJsValueKind);
    constructor Create(const a: TJVarArray);
    constructor Create(const a: TJPairArray);
    constructor Create(aNode: TJsonNode);
    destructor Destroy; override;
    function  GetEnumerator: TEnumerator; inline;
    function  SubTree: TSubTree; inline;
    function  Enrties: TEntries; inline;
    function  Names: TNames; inline;
    function  EqualNames(const aName: string): IPairEnumerable; inline;
    function  IsNull: Boolean; inline;
    function  IsFalse: Boolean; inline;
    function  IsTrue: Boolean; inline;
    function  IsNumber: Boolean; inline;
    function  IsInteger: Boolean; inline;
    function  IsString: Boolean; inline;
    function  IsArray: Boolean; inline;
    function  IsObject: Boolean; inline;
    function  IsBoolean: Boolean; inline;
    function  IsLiteral: Boolean; inline;
    function  IsScalar: Boolean; inline;
    function  IsStruct: Boolean; inline;
    procedure Clear; inline;
  { duplicates an instance, is recursive }
    function  Clone: TJsonNode;
  { makes a deep copy of the aNode, does not check if aNode is assigned, is recursive }
    procedure CopyFrom(aNode: TJsonNode);
  { checks that an instance is element-wise equal to aNode, is recursive;
    returns false if any object contains a non-unique key }
    function  EqualTo(aNode: TJsonNode): Boolean;
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
  { adds a new array from the elements of the array a to the instance as to an array;
    if it is not an array, it is cleared and becomes an array - be careful; returns Self }
    function  Add(const a: TJVarArray): TJsonNode;
  { adds a new object from the elements of the array a to the instance as to an array;
    if it is not an array, it is cleared and becomes an array - be careful; returns Self }
    function  Add(const a: TJPairArray): TJsonNode;
  { adds a new object of the specified kind to the instance as to an array;
    if an instance is not an array, it is cleared and becomes an array - be careful;
    returns a new object }
    function  AddNode(aKind: TJsValueKind): TJsonNode; inline;
  { returns True and the created object in the aNode parameter,
    if the string s can be parsed; the new object is added as in an array - be careful }
    function  AddJson(const s: string; out aNode: TJsonNode): Boolean;
  { adds all elements from aNode to the instance as to an array;
    if it is not an array, it is cleared and becomes an array - be careful;
    if aNode is a structure it becomes empty; returns Self }
    function  Append(aNode: TJsonNode): TJsonNode;
  { adds pair aName: null to the instance as to an object; if it is not an object,
    it is cleared and becomes an object - be careful; returns Self }
    function  AddNull(const aName: string): TJsonNode; inline;
  { adds pair (aName: aValue) to the instance as to an object; if it is not an object,
    it is cleared and becomes an object - be careful; returns Self }
    function  Add(const aName: string; aValue: Boolean): TJsonNode; inline;
    function  Add(const aName: string; aValue: Double): TJsonNode; inline;
    function  Add(const aName, aValue: string): TJsonNode; inline;
  { adds pair (aName: array from aValue elements) to the instance as to an object;
    if it is not an object, it is cleared and becomes an object - be careful; returns Self }
    function  Add(const aName: string; const aValue: TJVarArray): TJsonNode;
  { adds pair (aName: object from aValue elements) to the instance as to an object;
    if it is not an object, it is cleared and becomes an object - be careful; returns Self }
    function  Add(const aName: string; const aValue: TJPairArray): TJsonNode;
  { adds a new object of the specified type associated with aName to the instance as to an object;
    if an instance is not an object, it is cleared and becomes an object - be careful;
    returns a new object }
    function  AddNode(const aName: string; aKind: TJsValueKind): TJsonNode; inline;
  { returns True and the created object associated with aName in the aNode parameter,
    if the string aJson can be parsed; the new object is added as to an object - be careful }
    function  AddJson(const aName, aJson: string; out aNode: TJsonNode): Boolean;
  { adds pair (aName: null) to the instance as to an object and returns True
    only if aName is unique within an instance, otherwise returns False;
    if an instance is not an object, it is cleared and becomes an object - be careful }
    function  AddUniqNull(const aName: string): Boolean;
  { adds pair (aName: aValue) to the instance as to an object and returns True
    only if aName is unique within an instance, otherwise returns False;
    if an instance is not an object, it is cleared and becomes an object - be careful }
    function  AddUniq(const aName: string; aValue: Boolean): Boolean;
    function  AddUniq(const aName: string; aValue: Double): Boolean;
    function  AddUniq(const aName, aValue: string): Boolean;
    function  AddUniq(const aName: string; const aValue: TJVarArray): Boolean;
    function  AddUniq(const aName: string; const aValue: TJPairArray): Boolean;
  { adds a new object of the specified type associated with aName to the instance as to an object
    only if aName is unique within an instance, otherwise returns False;
    if an instance is not an object, it is cleared and becomes an object - be careful;
    returns a new object }
    function  AddUniqNode(const aName: string; out aNode: TJsonNode; aKind: TJsValueKind): Boolean;
  { returns True and the created object associated with aName in the aNode parameter,
    only if aName is unique within an instance and the string aJson can be parsed;
    the new object is added as in an object - be careful }
    function  AddUniqJson(const aName, aJson: string; out aNode: TJsonNode): Boolean;
  { if aIndex = 0 then acts like AddNull;
    returns True and inserts null at position aIndex if aIndex is in the range [1..Count]
    and the instance is an array, otherwise it returns False }
    function  InsertNull(aIndex: SizeInt): Boolean;
  { if aIndex = 0 then acts like Add(aValue);
    returns True and inserts aValue at position aIndex if aIndex is in the range [1..Count]
    and the instance is an array, otherwise it returns False}
    function  Insert(aIndex: SizeInt; aValue: Boolean): Boolean;
    function  Insert(aIndex: SizeInt; aValue: Double): Boolean;
    function  Insert(aIndex: SizeInt; const aValue: string): Boolean;
  { if aIndex = 0 then acts like AddNode;
    returns True and inserts new object(aNode) of the specified kind
    at position aIndex if aIndex is in the range [1..Count]
    and the instance is an array, otherwise it returns False }
    function  InsertNode(aIndex: SizeInt; out aNode: TJsonNode; aKind: TJsValueKind): Boolean;
  { }
    function  InsertNull(aIndex: SizeInt; const aName: string): Boolean;
    function  Insert(aIndex: SizeInt; const aName: string; aValue: Boolean): Boolean;
    function  Insert(aIndex: SizeInt; const aName: string; aValue: Double): Boolean;
    function  Insert(aIndex: SizeInt; const aName, aValue: string): Boolean;
    function  InsertNode(aIndex: SizeInt; const aName: string; out aNode: TJsonNode; aKind: TJsValueKind): Boolean;
    function  Contains(const aName: string): Boolean; inline;
    function  ContainsUniq(const aName: string): Boolean; inline;
    function  IndexOfName(const aName: string): SizeInt; inline;
    function  CountOfName(const aName: string): SizeInt; inline;
    function  Find(const aKey: string; out aValue: TJsonNode): Boolean;
  { returns True if aName is found, otherwise adds a new pair with
    Value.Kind = jvkUnknown and returns False;
    if the instance is not an object, it is cleared and becomes an object - be careful }
    function  FindOrAdd(const aName: string; out aValue: TJsonNode): Boolean;
    function  FindUniq(const aName: string; out aValue: TJsonNode): Boolean;
    function  FindAll(const aName: string): TNodeArray;
    function  Find(aIndex: SizeInt; out aValue: TJsonNode): Boolean;
    function  FindPair(aIndex: SizeInt; out aValue: TPair): Boolean;
    function  FindName(aIndex: SizeInt; out aName: string): Boolean;
    function  Delete(aIndex: SizeInt): Boolean;
    function  Extract(aIndex: SizeInt; out aNode: TJsonNode): Boolean;
    function  Extract(aIndex: SizeInt; out aPair: TPair): Boolean;
    function  Extract(const aName: string; out aNode: TJsonNode): Boolean;
    function  Remove(const aName: string): Boolean;
    function  RemoveAll(const aName: string): SizeInt;
  { tries to find an element using the path specified as a JSON Pointer;
    each node considered self as a root }
    function  FindPath(const aPtr: TJsonPtr; out aNode: TJsonNode): Boolean;
  { tries to find an element using the path specified as an array of path segments;
    each node considered self as a root }
    function  FindPath(const aPath: array of string; out aNode: TJsonNode): Boolean;
  { returns a formatted JSON representation of an instance, is recursive }
    function  FormatJson(aOptions: TJsFormatOptions = []; aIndent: Integer = DEF_INDENT): string;
    function  GetValue(out aValue: TJVariant): Boolean;
    procedure SaveToStream(aStream: TStream);
    procedure SaveToFile(const aFileName: string);
    function  ToString: string; override;
  { GetAsJson returns the most compact JSON representation of an instance, is recursive;
    SetAsJson remark: if the parser fails to parse the original string,
    an exception will be raised. }
    property  AsJson: string read GetAsJson write SetAsJson;
  { converts an instance to null }
    property  AsNull: TJsonNode read GetAsNull;
  { converts an instance to a Boolean }
    property  AsBoolean: Boolean read GetAsBoolean write SetAsBoolean;
  { converts an instance to a number }
    property  AsNumber: Double read GetAsNumber write SetAsNumber;
  { converts an instance to a string }
    property  AsString: string read GetAsString write SetAsString;
  { converts an instance to an array }
    property  AsArray: TJsonNode read GetAsArray;
  { converts an instance to an object }
    property  AsObject: TJsonNode read GetAsObject;
    property  Kind: TJsValueKind read FKind;
    property  Count: SizeInt read GetCount;
  { will raise exception if aIndex out of bounds }
    property  Items[aIndex: SizeInt]: TJsonNode read GetItem;
  { will raise exception if aIndex out of bounds or an instance is not an object }
    property  Pairs[aIndex: SizeInt]: TPair read GetPair;
  { acts as FindOrAdd }
    property  NItems[const aName: string]: TJsonNode read GetByName; //todo: need another prop name?
  { if GetValue does not find aName or if the value found is an array or object,
    it will raise an exception; SetValue will make an object from an instance - be careful }
    property  Values[const aName: string]: TJVariant read GetValue write SetValue; default;
  { will make an object from an instance }
    property  NArrays[const aName: string]: TJVarArray write SetNArray;
  { will make an object from an instance }
    property  NObjects[const aName: string]: TJPairArray write SetNObject;
  end;

  { TJsonWriter provides a quick way of producing JSON document;
    no whitespace is added, so the results is presented in the most compact form;
    you yourself are responsible for the syntactic correctness of the generated document;
    each instance of TJsonWriter can produce one JSON document }
  TJsonWriter = class
  private
    FStream: TWriteBufStream;
    FStack: specialize TGLiteStack<Integer>;
    FsBuilder: TJsonNode.TStrBuilder;
    procedure ValueAdding; inline;
    procedure PairAdding; inline;
  public
    class function New(aStream: TStream): TJsonWriter; inline;
    constructor Create(aStream: TStream);
    destructor Destroy; override;
    function AddNull: TJsonWriter;
    function AddFalse: TJsonWriter;
    function AddTrue: TJsonWriter;
    function Add(aValue: Double): TJsonWriter;
    function Add(const s: string): TJsonWriter;
    function Add(aValue: TJsonNode): TJsonWriter;
    function AddJson(const aJson: string): TJsonWriter;
    function AddName(const aName: string): TJsonWriter;
    function AddNull(const aName: string): TJsonWriter;
    function AddFalse(const aName: string): TJsonWriter;
    function AddTrue(const aName: string): TJsonWriter;
    function Add(const aName: string; aValue: Double): TJsonWriter;
    function Add(const aName, aValue: string): TJsonWriter;
    function Add(const aName: string; aValue: TJsonNode): TJsonWriter;
    function AddJson(const aName, aJson: string): TJsonWriter;
    function BeginArray: TJsonWriter;
    function BeginObject: TJsonWriter;
    function EndArray: TJsonWriter;
    function EndObject: TJsonWriter;
  end;

  TParseMode = (pmNone, pmKey, pmArray, pmObject);
  PParseMode = ^TParseMode;

  { TJsonReader provides forward only navigation through the JSON stream
    with ability to skip some parts of the document; also has the ability
    to find specific place in JSON by a path from the document root }
  TJsonReader = class
  public
  const
    DEF_DEPTH = 511;
  type
    TIterateFun  = function(aIter: TJsonReader): Boolean of object;
    TNestIterate = function(aIter: TJsonReader): Boolean is nested;

    TReadState = (rsStart, rsGo, rsEOF, rsError);

    TTokenKind = (
      tkNone,
      tkNull,
      tkFalse,
      tkTrue,
      tkNumber,
      tkString,
      tkArrayBegin,
      tkObjectBegin,
      tkArrayEnd,
      tkObjectEnd);

    TStructKind = (skNone, skArray, skObject);
  private
  type
    TLevel = record
      Mode: TParseMode;
      Path: string;
      CurrIndex: SizeInt;
      constructor Create(aMode: TParseMode);
      constructor Create(aMode: TParseMode; aPath: string);
      constructor Create(aMode: TParseMode; aIndex: SizeInt);
    end;

  var
    FBuffer: TJsonNode.TRwBuffer;
    FStack: array of TLevel;
    FsBuilder,
    FsbHelp: TJsonNode.TStrBuilder;
    FStream: TStream;
    FByteCount,
    FPosition,
    FStackTop,
    FStackHigh: SizeInt;
    FState: Integer;
    FReadState: TReadState;
    FToken,
    FDeferToken: TTokenKind;
    FName: string;
    FValue: TJVariant;
    FReadMode,
    FCopyMode,
    FSkipBom,
    FFirstChunk: Boolean;
    function  GetIndex: SizeInt; inline;
    function  GetStructKind: TStructKind; inline;
    function  GetParentKind: TStructKind; inline;
    procedure UpdateArray; inline;
    function  NullValue: Boolean;
    function  FalseValue: Boolean;
    function  TrueValue: Boolean;
    function  NumValue: Boolean;
    procedure NameValue; inline;
    function  CommaAfterNum: Boolean;
    function  StringValue: Boolean;
    function  ArrayBegin: Boolean;
    function  ObjectBegin: Boolean;
    function  ArrayEnd: Boolean;
    function  ArrayEndAfterNum: Boolean;
    function  ObjectEnd: Boolean;
    function  ObjectEndAfterNum: Boolean;
    function  ObjectEndOb: Boolean;
    function  DeferredEnd: Boolean; inline;
    function  GetNextChunk: TReadState;
    function  GetNextToken: Boolean;
    function  GetIsNull: Boolean; inline;
    function  GetAsBoolean: Boolean; inline;
    function  GetAsNumber: Double; inline;
    function  GetAsString: string; inline;
    function  GetPath: string;
    function  GetParentName: string; inline;
    property  ReadMode: Boolean read FReadMode;
    property  CopyMode: Boolean read FCopyMode;
    property  DeferToken: TTokenKind read FDeferToken;
  public
    class function IsStartToken(aToken: TTokenKind): Boolean; static; inline;
    class function IsEndToken(aToken: TTokenKind): Boolean; static; inline;
    class function IsScalarToken(aToken: TTokenKind): Boolean; static; inline;
    constructor Create(aStream: TStream; aMaxDepth: SizeInt = DEF_DEPTH; aSkipBom: Boolean = False);
  { reads the next token from the stream, returns False if an error is encountered or the end
    of the stream is reached, otherwise it returns true; on error, the ReadState property
    will be set to rsError, and upon reaching the end of the stream, to rsEOF}
    function  Read: Boolean;
  { if the current token is the beginning of a structure, it skips its contents
    and stops at the closing token, otherwise it just performs one Read }
    procedure Skip;
  { iterates over all items in the current structure and calls the aFun function
    for each value item, passing Self as a parameter;
    if aFun returns False, the iteration stops immediately, otherwise it stops
    at the closing token of the current structure }
    procedure Iterate(aFun: TIterateFun);
  { iterates over all items in the current structure and calls the aFun function
    for each value item, passing Self as a parameter;
    if aFun returns False, the iteration stops immediately, otherwise it stops
    at the closing token of the current structure }
    procedure Iterate(aFun: TNestIterate);
  { iterates over all items in the current structure and calls the aOnValue function
    for each value item or aOnStruct function for each struct item, passing Self as
    a parameter; if the called function returns False, the iteration stops immediately,
    otherwise it stops at the closing token of the current structure }
    procedure Iterate(aOnStruct, aOnValue: TIterateFun);
  { iterates over all the items in the current structure and calls the aOnValue function
    for each value item or aOnStruct function for each struct item, passing Self as
    a parameter; if the called function returns False, the iteration stops immediately,
    otherwise it stops at the closing token of the current structure }
    procedure Iterate(aOnStruct, aOnValue: TNestIterate);
  { if the current token is the beginning of some structure(array or object),
    it copies this structure "as is" into aStruct and returns True, otherwise returns False }
    function  CopyStruct(out aStruct: string): Boolean;
  { moves to the next structure item without trying to enter nested structures;
    if the next item turns out to be a structure, it is skipped to the closing token;
    returns False if it cannot move to the next item, otherwise returns True }
    function  MoveNext: Boolean;
  { tries to find the specified key in the current structure without trying to enter
    nested structures;
    the key can be a name or a string representation of a non-negative integer;
    returns true if the key was found, otherwise returns false;
    in case of a successful search:
      if the current structure is an array and the value is a scalar,
      the search stops after reading the value with the specified index,
      otherwise the search stops at the opening token of the value;
      if the current structure is an object and the value is a scalar,
      the search stops after reading the value,
      otherwise the search stops at the opening token of the value }
    function  Find(const aKey: string): Boolean;
  { tries to find a specific place using the path specified as a JSON Pointer;
    search is possible only from the document root }
    function  FindPath(const aPtr: TJsonPtr): Boolean;
  { finds a specific place using the path specified as an array of path segments;
    search is possible only from the document root }
    function  FindPath(const aPath: TStringArray): Boolean;
  { True if current value is Null }
    property  IsNull: Boolean read GetIsNull;
  { returns the value as a Boolean, raises an exception if kind of the value <> vkBoolean }
    property  AsBoolean: Boolean read GetAsBoolean;
  { returns the value as a Double, raises an exception if kind of the value <> vkNumber }
    property  AsNumber: Double read GetAsNumber;
  { returns the value as a string, raises an exception if kind of the value <> vkString }
    property  AsString: string read GetAsString;
  { indicates the current structure index, or zero if the current structure is an object }
    property  Index: SizeInt read GetIndex;
  { indicates the current name or index if current structure is an array }
    property  Name: string read FName;
    property  Value: TJVariant read FValue;
  { returns current path as a JSON pointer (RFC 6901) }
    property  Path: string read GetPath;
    property  TokenKind: TTokenKind read FToken;
    property  StructKind: TStructKind read GetStructKind;
    property  ParentName: string read GetParentName;
    property  ParentKind: TStructKind read GetParentKind;
  { indicates the nesting depth of the current structure, zero based }
    property  Depth: SizeInt read FStackTop;
    property  ReadState: TReadState read FReadState;
    property  SkipBom: Boolean read FSkipBom;
  end;


function IsExactInt(aValue: Double): Boolean; inline;
function IsExactInt(aValue: Double; out aIntValue: Int64): Boolean; inline;
function SameDouble(L, R: Double): Boolean; inline;

implementation
{$B-}{$COPERATORS ON}{$POINTERMATH ON}

const
  MAX_EXACT_INT  = Double(9007199254740991); //2^53 - 1
  DBL_CMP_FACTOR = Double(1E12);
  JS_UNDEF       = 'undefined';
  JS_NULL        = 'null';
  JS_FALSE       = 'false';
  JS_TRUE        = 'true';

function IsExactInt(aValue: Double): Boolean;
begin
  Result := (Frac(aValue) = 0) and (Abs(aValue) <= MAX_EXACT_INT);
end;

function IsExactInt(aValue: Double; out aIntValue: Int64): Boolean;
begin
  if IsExactInt(aValue) then
    begin
      aIntValue := Trunc(aValue);
      exit(True);
    end;
  Result := False;
end;

function SameDouble(L, R: Double): Boolean;
begin
  Result := Abs(L - R) * DBL_CMP_FACTOR <= Min(Abs(L), Abs(R));
end;

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

class operator TJVariant.:=(const v: TJVariant): Int64;
begin
  if not IsExactInt(Double(v), Result) then
    v.ConvertError('Double', 'Int64');
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
    vkBool:   Result := (R.Kind = vkBool) and not(L.FValue.Bool xor R.FValue.Bool);
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

procedure TJVariant.SetNull;
begin
  Clear;
end;

function  TJVariant.IsInteger: Boolean;
begin
  if Kind <> vkNumber then
    exit(False);
  Result := IsExactInt(FValue.Num);
end;

function TJVariant.AsBoolean: Boolean;
begin
  Result := Self;
end;

function TJVariant.AsNumber: Double;
begin
  Result := Self;
end;

function TJVariant.AsInteger: Int64;
begin
  Result := Self;
end;

function TJVariant.AsString: string;
begin
  Result := Self;
end;

function TJVariant.ToString: string;
var
  I: Int64;
begin
  case Kind of
    vkNull:   Result := JS_NULL;
    vkBool:   Result := BoolToStr(FValue.Bool, JS_TRUE, JS_FALSE);
    vkNumber:
      if IsExactInt(FValue.Num, I) then
        Result := I.ToString
      else
        Result := FValue.Num.ToString;
    vkString: Result := string(FValue.Ref);
  end;
end;

function JNull: TJVariant;
begin
  Result.Clear;
end;

function JPair(const aName: string; const aValue: TJVariant): TJVarPair;
begin
  Result := TJVarPair.Create(aName, aValue);
end;

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
{key    KE}(KE,KE,__,__,__,__,__,__,ST,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__),
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
{$POP}

type
  TBomKind = (bkNone, bkUtf8, bkUtf16, bkUtf32);

const
  UTF8_BOM_LEN = 3;
  INF_EXP      = QWord($7ff0000000000000);
  NUM_STATES   = Integer(1 shl ZE or 1 shl IR or 1 shl FS or 1 shl E3);

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
    if Buf[I] < 128 then begin
      NextClass := SymClassTable[Buf[I]];
      if NextClass = __ then exit(False);
    end else
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
  Result := ((State = OK) or (State in [ZE, IR, FS, E3])) and (sTop = 0) and (Stack[0] = pmNone);
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
  Result := (State in [Ze, IR, FS, E3]) and (sTop = 0) and (Stack[0] = pmNone);
end;

function ValidateStream(s: TStream; aSkipBom: Boolean; const aStack: TOpenArray): Boolean;
var
  Stack: PParseMode;
  Buffer: TJsonNode.TRwBuffer;
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
  Result := ((State = OK) or (State in [ZE, IR, FS, E3])) and (sTop = 0) and (Stack[0] = pmNone);
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

constructor TJsonNode.TStrBuilder.Create(const s: string);
begin
  System.SetLength(FBuffer, System.Length(s));
  System.Move(Pointer(s)^, Pointer(FBuffer)^, System.Length(s));
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

function TJsonNode.TStrBuilder.ToPChar: PAnsiChar;
begin
  EnsureCapacity(Succ(Count));
  FBuffer[Count] := #0;
  FCount := 0;
  Result := Pointer(FBuffer);
end;

{ TJsonPtr.TEnumerator }

function TJsonPtr.TEnumerator.GetCurrent: string;
begin
  Result := FList[FIndex];
end;

function TJsonPtr.TEnumerator.MoveNext: Boolean;
begin
  if FIndex < System.High(FList) then
    begin
      Inc(FIndex);
      exit(True);
    end;
  Result := False;
end;

{ TJsonPtr }

function TJsonPtr.GetCount: SizeInt;
begin
  Result := System.Length(FSegments);
end;

function TJsonPtr.GetSegment(aIndex: SizeInt): string;
begin
  if SizeUInt(aIndex) >= SizeUInt(System.Length(FSegments)) then
    raise EJsException.CreateFmt(SEIndexOutOfBoundsFmt, [aIndex]);
  Result := FSegments[aIndex];
end;

class function TJsonPtr.Encode(const aSegs: TStringArray): string;
var
  sb: TJsonNode.TStrBuilder;
  I, J: SizeInt;
begin
  Result := '';
  sb := TJsonNode.TStrBuilder.Create(TJsonNode.S_BUILD_INIT_SIZE);
  for I := 0 to System.High(aSegs) do
    begin
      sb.Append('/');
      for J := 1 to System.Length(aSegs[I]) do
        case aSegs[I][J] of
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
          sb.Append(aSegs[I][J]);
        end;
    end;
  Result := sb.ToString;
end;

class function TJsonPtr.Decode(const s: string): TStringArray;
var
  sbSrc, sbDst: TJsonNode.TStrBuilder;
  Segs: TStringArray = nil;
  I: SizeInt;
  J: SizeInt = 0;
  procedure AddSegment;
  var
    II: SizeInt;
  begin
    if sbSrc.NonEmpty then
      begin
        if sbSrc.FBuffer[Pred(sbSrc.Count)] = '~' then
          raise EJsException.Create(SEInvalidJsPtr);
        II := 0;
        while II < sbSrc.Count do
          if sbSrc.FBuffer[II] = '~' then
            begin
              case sbSrc.FBuffer[II+1] of
                '0': sbDst.Append('~');
                '1': sbDst.Append('/');
              else
                raise EJsException.Create(SEInvalidJsPtr);
              end;
              II += 2;
            end
          else
            begin
              sbDst.Append(sbSrc.FBuffer[II]);
              Inc(II);
            end;
        sbSrc.MakeEmpty;
      end;
    if J = System.Length(Segs) then
      System.SetLength(Segs, J * 2);
    Segs[J] := sbDst.ToString;
    Inc(J);
  end;
begin
  Result := nil;
  if (s = '') then
    exit;
  if s[1] <> '/' then
    raise EJsException.Create(SEInvalidJsPtr);
  if s = '/' then
    exit(['']);
  System.SetLength(Segs, ARRAY_INITIAL_SIZE);
  sbSrc := TJsonNode.TStrBuilder.Create(System.Length(s));
  sbDst := TJsonNode.TStrBuilder.Create(System.Length(s));
  J := 0;
  for I := 2 to System.Length(s) do
    if s[I] = '/' then
      AddSegment
    else
      sbSrc.Append(s[I]);
  AddSegment;
  System.SetLength(Segs, J);
  Result := Segs;
end;

class function TJsonPtr.ValidPtr(const s: string): Boolean;
var
  I: SizeInt;
begin
  if s = '' then
    exit(True);
  if s[1] <> '/' then
    exit(False);
  for I := 2 to Pred(System.Length(s)) do
    if (s[I] = '~') and not (s[Succ(I)] in ['0'..'1']) then
      exit(False);
  if s[System.Length(s)] = '~' then
    exit(False);
  Result := True;
end;

class function TJsonPtr.ValidAlien(const s: string): Boolean;
var
  sb: TJsonNode.TStrBuilder;
begin
  if not TJsonNode.JsonStringValid(s) then
    exit(False);
  sb := TJsonNode.TStrBuilder.Create(s);
  Result := ValidPtr(sb.ToDecodeString);
end;

class function TJsonPtr.ToSegments(const s: string): TStringArray;
begin
  Result := Decode(s);
end;

class function TJsonPtr.ToString(const a: TStringArray): string;
begin
  Result := Encode(a);
end;

class operator TJsonPtr.=(const L, R: TJsonPtr): Boolean;
var
  I: SizeInt;
begin
  if System.Length(L.FSegments) <> System.Length(R.FSegments) then
    exit(False);
  for I := 0 to System.High(L.FSegments) do
    if L.FSegments[I] <> R.FSegments[I] then
      exit(False);
  Result := True;
end;

constructor TJsonPtr.From(const s: string);
begin
  FSegments := Decode(s);
end;

constructor TJsonPtr.From(const aPath: TStringArray);
begin
  FSegments := System.Copy(aPath);
end;

constructor TJsonPtr.FromAlien(const s: string);
var
  sb: TJsonNode.TStrBuilder;
begin
  sb := TJsonNode.TStrBuilder.Create(s);
  FSegments := Decode(sb.ToDecodeString);
end;

function TJsonPtr.GetEnumerator: TEnumerator;
begin
  Result.FList := FSegments;
  Result.FIndex := NULL_INDEX;
end;

function TJsonPtr.IsEmpty: Boolean;
begin
  Result := System.Length(FSegments) = 0;
end;

function TJsonPtr.NonEmpty: Boolean;
begin
  Result := System.Length(FSegments) <> 0;
end;

procedure TJsonPtr.Clear;
begin
  FSegments := nil;
end;

procedure TJsonPtr.Append(const aSegment: string);
begin
  System.Insert(aSegment, FSegments, Count);
end;

function TJsonPtr.ToString: string;
begin
  Result := Encode(FSegments);
end;

function TJsonPtr.ToAlien: string;
var
  sb: TJsonNode.TStrBuilder;
begin
  sb := TJsonNode.TStrBuilder.Create(TJsonNode.S_BUILD_INIT_SIZE);
  sb.AppendEncode(Encode(FSegments));
  Result := sb.ToString;
end;

function TJsonPtr.ToSegments: TStringArray;
begin
  Result := System.Copy(FSegments);
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

{ TJsonNode.TEqualEnumerator }

function TJsonNode.TEqualEnumerator.GetCurrent: TPair;
begin
  Result := FEnum.Current;
end;

constructor TJsonNode.TEqualEnumerator.Create(const aEnum: TJsObject.TEqualEnumerator);
begin
  FEnum := aEnum;
end;

function TJsonNode.TEqualEnumerator.MoveNext: Boolean;
begin
  Result := FEnum.MoveNext;
end;

procedure TJsonNode.TEqualEnumerator.Reset;
begin
  FEnum.Reset;
end;

{ TJsonNode }

class function TJsonNode.CreateJsArray: PJsArray;
begin
  Result := System.GetMem(SizeOf(TJsArray));
  FillChar(Result^, SizeOf(TJsArray), 0);
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
  IVal: Int64;
  procedure BuildJson(aInst: TJsonNode);
  var
    I, Last: SizeInt;
  begin
    case aInst.Kind of
      jvkNull:   sb.Append(JS_NULL);
      jvkFalse:  sb.Append(JS_FALSE);
      jvkTrue:   sb.Append(JS_TRUE);
      jvkNumber:
        if IsExactInt(aInst.FValue.Num, IVal) then
          sb.Append(IntToStr(IVal))
        else
          sb.Append(FloatToStr(aInst.FValue.Num, FmtSettings));
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
  sb := TStrBuilder.Create(S_BUILD_INIT_SIZE);
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
    jvkNull, jvkFalse, jvkTrue, jvkNumber, jvkString: exit(1);
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
  if AsArray.FValue.Ref = nil then
    FValue.Ref := CreateJsArray;
  Result := True;
end;

function TJsonNode.CanObjectInsert(aIndex: SizeInt): Boolean;
begin
  if aIndex <> 0 then
    exit((Kind = jvkObject)and(FValue.Ref <> nil)and(SizeUInt(aIndex) <= SizeUInt(FObject^.Count)));
  if AsObject.FValue.Ref = nil then
    FValue.Ref := CreateJsObject;
  Result := True;
end;

function TJsonNode.GetItem(aIndex: SizeInt): TJsonNode;
begin
  if SizeUInt(aIndex) < SizeUInt(Count) then
    case Kind of
      jvkArray:  exit(FArray^.UncMutable[aIndex]^);
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
    if Kind = jvkObject then
      exit(FObject^.Mutable[aIndex]^)
    else
      raise EJsException.Create(SEJsonInstNotObj)
  else
    raise EJsException.CreateFmt(SEIndexOutOfBoundsFmt, [aIndex]);
  Result := Default(TPair);
end;

function TJsonNode.GetByName(const aName: string): TJsonNode;
begin
  FindOrAdd(aName, Result);
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
      jvkNumber: exit(Node.FValue.Num);
      jvkString: exit(Node.FString);
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

procedure TJsonNode.SetNArray(const aName: string; const aValue: TJVarArray);
var
  Node: TJsonNode;
begin
  if FindOrAdd(aName, Node) then
    begin
      Node.DoClear;
      Node.FKind := jvkArray;
    end;
  Node.Add(aValue);
end;

procedure TJsonNode.SetNObject(const aName: string; const aValue: TJPairArray);
var
  Node: TJsonNode;
begin
  if FindOrAdd(aName, Node) then
    begin
      Node.DoClear;
      Node.FKind := jvkObject;
    end;
  Node.Add(aValue);
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

class function TJsonNode.ValidJson(aStream: TStream; aDepth: Integer; aSkipBom: Boolean): Boolean;
var
  Stack: array[0..DEF_DEPTH] of TParseMode;
  DynStack: array of TParseMode = nil;
begin
  if aDepth < 1 then exit(False);
  if aDepth <= DEF_DEPTH then
    Result := ValidateStream(aStream, aSkipBom, TOpenArray.Create(@Stack[0], aDepth + 1))
  else
    begin
      System.SetLength(DynStack, aDepth + 1);
      Result := ValidateStream(aStream, aSkipBom, TOpenArray.Create(Pointer(DynStack), aDepth + 1));
    end;
end;

class function TJsonNode.ValidJson(aStream: TStream; aCount: SizeInt; aDepth: Integer;
  aSkipBom: Boolean): Boolean;
var
  s: string;
begin
  with TStringStream.Create do
    try
      CopyFrom(aStream, aCount);
      s := DataString;
    finally
      Free;
    end;
  Result := ValidJson(s, aDepth, aSkipBom);
end;

class function TJsonNode.ValidJsonFile(const aFileName: string; aDepth: Integer;
  aSkipBom: Boolean): Boolean;
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

class function TJsonNode.LikelyKind(aBuf: PAnsiChar; aSize: SizeInt): TJsValueKind;
var
  I: SizeInt;
begin
  Result := jvkUnknown;
  for I := 0 to Pred(aSize) do
    case aBuf[I] of
      #9, #10, #13, ' ': ;
      '"':               exit(jvkString);
      '-', '0'..'9':     exit(jvkNumber);
      '[':               exit(jvkArray);
      '{':               exit(jvkObject);
      'f':               exit(jvkFalse);
      'n':               exit(jvkNull);
      't':               exit(jvkTrue);
    end;
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
  s: string = '';
begin
  System.SetLength(s, aStream.Size - aStream.Position);
  aStream.ReadBuffer(Pointer(s)^, System.Length(s));
  Result := TryParse(s, aRoot, aDepth, aSkipBom);
end;

class function TJsonNode.TryParse(aStream: TStream; aCount: SizeInt; out aRoot: TJsonNode;
  aDepth: Integer; aSkipBom: Boolean): Boolean;
var
  s: string;
begin
  with TStringStream.Create do
    try
      CopyFrom(aStream, aCount);
      s := DataString;
    finally
      Free;
    end;
  Result := TryParse(s, aRoot, aDepth, aSkipBom);
end;

class function TJsonNode.TryParseFile(const aFileName: string; out aRoot: TJsonNode;
  aDepth: Integer; aSkipBom: Boolean): Boolean;
var
  s: string = '';
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

class function TJsonNode.NewNode(aNode: TJsonNode): TJsonNode;
begin
  Result := aNode.Clone;
end;

class function TJsonNode.NewJson(const s: string): TJsonNode;
begin
  TryParse(s, Result);
end;

class function TJsonNode.MaxNestDepth(aNode: TJsonNode): SizeInt;
var
  MaxDep: SizeInt = 0;
  procedure Traverse(aNode: TJsonNode; aLevel: SizeInt);
  var
    I: SizeInt;
  begin
    if aLevel > MaxDep then
      MaxDep := aLevel;
    if aNode.Count > 0 then
      case aNode.Kind of
        jvkArray:
          for I := 0 to Pred(aNode.FArray^.Count) do
            Traverse(aNode.FArray^.UncMutable[I]^, Succ(aLevel));
        jvkObject:
          for I := 0 to Pred(aNode.FObject^.Count) do
            Traverse(aNode.FObject^.Mutable[I]^.Value, Succ(aLevel));
      else
      end;
  end;
begin
  Traverse(aNode, 0);
  Result := MaxDep;
end;

class function TJsonNode.HasUniqueNames(aNode: TJsonNode): Boolean;
  function NamesUnique(aNode: TJsonNode): Boolean;
  var
    I: SizeInt;
  begin
    if aNode.Count > 0 then
      case aNode.Kind of
        jvkArray:
          for I := 0 to Pred(aNode.FArray^.Count) do
            if not NamesUnique(aNode.FArray^.UncMutable[I]^) then
              exit(False);
        jvkObject:
          for I := 0 to Pred(aNode.FObject^.Count) do
            with aNode.FObject^.Mutable[I]^ do
              begin
                if not aNode.FObject^.ContainsUniq(Key) then
                  exit(False);
                if not NamesUnique(Value) then
                  exit(False);
              end;
      else
      end;
    Result := True;
  end;
begin
  Result := NamesUnique(aNode);
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

constructor TJsonNode.Create(aNode: TJsonNode);
begin
  Create;
  CopyFrom(aNode);
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

function TJsonNode.SubTree: TSubTree;
begin
  Result.FNode := Self;
end;

function TJsonNode.Enrties: TEntries;
begin
  Result.FNode := Self;
end;

function TJsonNode.Names: TNames;
begin
  Result.FNode := Self;
end;

function TJsonNode.EqualNames(const aName: string): IPairEnumerable;
begin
  if (Kind = jvkObject) and (FValue.Ref <> nil) then
    exit(TPairs.Create(TEqualEnumerator.Create(FObject^.GetEqualEnumerator(aName))));
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

function TJsonNode.IsInteger: Boolean;
begin
  if Kind <> jvkNumber then
    exit(False);
  Result := IsExactInt(FValue.Num);
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
    jvkArray:  Result := FNode.FArray^.UncMutable[FCurrIndex]^;
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
      if FCurrent.IsStruct then
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

{ TJsonNode.TEntryEnumerator }

function TJsonNode.TEntryEnumerator.GetCurrent: TPair;
begin
  Result := FNode.FObject^.Mutable[FCurrIndex]^
end;

function TJsonNode.TEntryEnumerator.MoveNext: Boolean;
begin
  if FNode.Kind <> jvkObject then
    exit(False);
  Inc(FCurrIndex);
  Result := FCurrIndex < FNode.Count;
end;

{ TJsonNode.TEntries }

function TJsonNode.TEntries.GetEnumerator: TEntryEnumerator;
begin
  Result.FNode := FNode;
  Result.FCurrIndex := NULL_INDEX;
end;

{ TJsonNode.TNameEnumerator }

function TJsonNode.TNameEnumerator.GetCurrent: string;
begin
  Result := FNode.FObject^.Mutable[FCurrIndex]^.Key;
end;

function TJsonNode.TNameEnumerator.MoveNext: Boolean;
begin
  if FNode.Kind <> jvkObject then
    exit(False);
  Inc(FCurrIndex);
  Result := FCurrIndex < FNode.Count;
end;

{ TJsonNode.TNames }

function TJsonNode.TNames.GetEnumerator: TNameEnumerator;
begin
  Result.FNode := FNode;
  Result.FCurrIndex := NULL_INDEX;
end;

procedure TJsonNode.Clear;
begin
  DoClear;
  FKind := jvkUnknown;
end;

function TJsonNode.Clone: TJsonNode;
begin
  Result := TJsonNode.Create;
  Result.CopyFrom(Self);
end;

procedure TJsonNode.CopyFrom(aNode: TJsonNode);
var
  I: SizeInt;
begin
  if aNode = Self then
    exit;
  Clear;
  case aNode.Kind of
    jvkUnknown: ;
    jvkNull:    AsNull;
    jvkFalse:   AsBoolean := False;
    jvkTrue:    AsBoolean := True;
    jvkNumber:  AsNumber := aNode.FValue.Num;
    jvkString:  AsString := aNode.FString;
    jvkArray:
     begin
       AsArray;
       if aNode.Count > 0 then
         begin
           FArray := CreateJsArray;
           FArray^.EnsureCapacity(aNode.FArray^.Count);
           for I := 0 to Pred(aNode.FArray^.Count) do
             FArray^.Add(aNode.FArray^.UncMutable[I]^.Clone);
         end;
     end;
    jvkObject:
     begin
       AsObject;
       if aNode.Count > 0 then
          begin
            FObject := CreateJsObject;
            FObject^.EnsureCapacity(aNode.FObject^.Count);
            for I := 0 to Pred(aNode.Count) do
              with aNode.FObject^.Mutable[I]^ do
                FObject^.Add(TPair.Create(Key, Value.Clone));
          end;
     end;
  end;
end;

function TJsonNode.EqualTo(aNode: TJsonNode): Boolean;
var
  I: SizeInt;
  p: ^TPair;
begin
  if aNode = Self then
    exit(True);
  if aNode = nil then
    exit(Self = nil);
  if (Kind <> aNode.Kind) or (Count <> aNode.Count) then
    exit(False);
  case aNode.Kind of
    jvkUnknown, jvkNull, jvkFalse, jvkTrue: ;
    jvkNumber:
      if not SameDouble(FValue.Num, aNode.FValue.Num) then
        exit(False);
    jvkString:
      if FString <> aNode.FString then
        exit(False);
    jvkArray:
     for I := 0 to Pred(Count) do
       if not FArray^.UncMutable[I]^.EqualTo(aNode.FArray^.UncMutable[I]^) then
         exit(False);
    jvkObject:
     begin
       for I := 0 to Pred(Count) do
         begin
           p := aNode.FObject^.FindUniq(FObject^.Mutable[I]^.Key);
           if p = nil then
             exit(False);
           if not FObject^.Mutable[I]^.Value.EqualTo(p^.Value) then
             exit(False);
         end;
     end;
  end;
  Result := True;
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
begin
  if AsArray.FValue.Ref = nil then
    FValue.Ref := CreateJsArray;
  FArray^.Add(TJsonNode.Create(a));
  Result := Self;
end;

function TJsonNode.Add(const a: TJPairArray): TJsonNode;
begin
  if AsArray.FValue.Ref = nil then
    FValue.Ref := CreateJsArray;
  FArray^.Add(TJsonNode.Create(a));
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

function TJsonNode.Append(aNode: TJsonNode): TJsonNode;
var
  I: SizeInt;
  Node: TJsonNode;
begin
  if AsArray.FValue.Ref = nil then
    FValue.Ref := CreateJsArray;
  case aNode.Kind of
    jvkNull:   AddNull;
    jvkFalse,
    jvkTrue:   Add(aNode.AsBoolean);
    jvkNumber: Add(aNode.FValue.Num);
    jvkString: Add(aNode.FString);
    jvkArray:
      if aNode.Count <> 0 then
        begin
          for I := 0 to Pred(aNode.Count) do
            FArray^.Add(aNode.FArray^.UncMutable[I]^);
          aNode.FArray^.Clear;
        end;
    jvkObject:
      if aNode.Count <> 0 then
        begin
          for I := 0 to Pred(aNode.Count) do
            begin
              Node := TJsonNode.Create(jvkObject);
              Node.FValue.Ref := CreateJsObject;
              Node.FObject^.Add(aNode.FObject^.Mutable[I]^);
              FArray^.Add(Node);
            end;
          aNode.FObject^.Clear;
        end;
  else
    //todo: what about undefined ???
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

function TJsonNode.AddUniq(const aName: string; const aValue: TJVarArray): Boolean;
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

function TJsonNode.AddUniq(const aName: string; const aValue: TJPairArray): Boolean;
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

function TJsonNode.IndexOfName(const aName: string): SizeInt;
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
      for e in FObject^.EqualKeys(aName) do
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
  Node: TJsonNode;
begin
  if Extract(aIndex, Node) then
    begin
      Node.Free;
      exit(True);
    end;
  Result := False;
end;

function TJsonNode.Extract(aIndex: SizeInt; out aNode: TJsonNode): Boolean;
var
  p: TPair;
begin
  aNode := nil;
  case Kind of
    jvkArray: exit(FArray^.TryExtract(aIndex, aNode));
    jvkObject:
      if (FObject <> nil) and FObject^.TryDelete(aIndex, p) then
        begin
          aNode := p.Value;
          exit(True);
        end;
  else
  end;
  Result := False;
end;

function TJsonNode.Extract(aIndex: SizeInt; out aPair: TPair): Boolean;
begin
  if (Kind = jvkObject) and (FObject <> nil) and FObject^.TryDelete(aIndex, aPair) then
    exit(True);
  aPair := Default(TPair);
  Result := False;
end;

function TJsonNode.Extract(const aName: string; out aNode: TJsonNode): Boolean;
var
  p: TPair;
begin
  if (Kind = jvkObject) and (FObject <> nil) and FObject^.Remove(aName, p) then
    begin
      aNode := p.Value;
      exit(True);
    end;
  aNode := nil;
  Result := False;
end;

function TJsonNode.Remove(const aName: string): Boolean;
var
  Node: TJsonNode;
begin
  if Extract(aName, Node) then
    begin
      Node.Free;
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

function TJsonNode.FindPath(const aPtr: TJsonPtr; out aNode: TJsonNode): Boolean;
begin
  if aPtr.IsEmpty then
    begin
      aNode := Self;
      exit(True);
    end;
  Result := FindPath(aPtr.ToSegments, aNode);
end;

function IsNonNegativeInteger(const s: string; out aInt: SizeInt): Boolean;
begin
  if s = '' then exit(False);
  if System.Length(s) = 1 then
    if s[1] in ['0','1','2','3','4','5','6','7','8','9'] then
      begin
        aInt := StrToInt(s);
        exit(True)
      end
    else
      exit(False);
  //leading zeros are not allowed
  if not (s[1] in ['1','2','3','4','5','6','7','8','9']) then
    exit(False);
  aInt := StrToIntDef(s, NULL_INDEX);
  Result := aInt <> NULL_INDEX;
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
          if aPath[I] = '-' then
            begin
              if I <> System.High(aPath) then
                exit(False);
              aNode := Node.AddNode(jvkNull);
              exit(True);
            end
          else
            if not IsNonNegativeInteger(aPath[I], Idx) then
              exit(False);
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
  IVal: Int64;
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
      jvkNull:   sb.Append(JS_NULL);
      jvkFalse:  sb.Append(JS_FALSE);
      jvkTrue:   sb.Append(JS_TRUE);
      jvkNumber:
        if IsExactInt(aInst.FValue.Num, IVal) then
          sb.Append(IntToStr(IVal))
        else
          sb.Append(FloatToStr(aInst.FValue.Num, FmtSettings));
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
  sb := TStrBuilder.Create(S_BUILD_INIT_SIZE);
  MultiLine := not (jfoSingleLine in aOptions);
  UseTabs := jfoUseTabs in aOptions;
  StrEncode := not (jfoStrAsIs in aOptions);
  BsdBrace := not (jfoEgyptBrace in aOptions);
  HasText := False;
  BuildJson(Self, 0);
  Result := sb.ToString;
end;

function TJsonNode.GetValue(out aValue: TJVariant): Boolean;
begin
  if not IsScalar then exit(False);
  case Kind of
    jvkNull:   aValue.SetNull;
    jvkFalse:  aValue := False;
    jvkTrue:   aValue := True;
    jvkNumber: aValue := FValue.Num;
    jvkString: aValue := string(FValue.Ref);
  else
  end;
  Result := True;
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

function TJsonNode.ToString: string;
var
  I64: Int64;
begin
  case Kind of
    jvkUnknown: Result := JS_UNDEF;
    jvkNull:    Result := JS_NULL;
    jvkFalse:   Result := JS_FALSE;
    jvkTrue:    Result := JS_TRUE;
    jvkNumber:
      if IsExactInt(FValue.Num, I64) then
        Result := I64.ToString
      else
        Result := FValue.Num.ToString;
    jvkString:  Result := FString;
    jvkArray,
    jvkObject:  Result := FormatJson([jfoSingleLine, jfoStrAsIs]);
  end;
end;

type
  TOWord = record
    Lo, Hi: QWord;
  end;

{$PUSH}{$Q-}{$R-}{$J-}
const
  ELDBL_LOW_POWER  = -325;
  ELDBL_HIGH_POWER = 308;

  TEN_POWER: array[0..22] of Double = (
    1e0,  1e1,  1e2,  1e3,  1e4,  1e5,  1e6,  1e7,  1e8,  1e9,  1e10, 1e11,
    1e12, 1e13, 1e14, 1e15, 1e16, 1e17, 1e18, 1e19, 1e20, 1e21, 1e22);


  EL_MANTIS_64: array[ELDBL_LOW_POWER..ELDBL_HIGH_POWER] of QWord = (
    QWord($a5ced43b7e3e9188), QWord($cf42894a5dce35ea), QWord($818995ce7aa0e1b2), QWord($a1ebfb4219491a1f),
    QWord($ca66fa129f9b60a6), QWord($fd00b897478238d0), QWord($9e20735e8cb16382), QWord($c5a890362fddbc62),
    QWord($f712b443bbd52b7b), QWord($9a6bb0aa55653b2d), QWord($c1069cd4eabe89f8), QWord($f148440a256e2c76),
    QWord($96cd2a865764dbca), QWord($bc807527ed3e12bc), QWord($eba09271e88d976b), QWord($93445b8731587ea3),
    QWord($b8157268fdae9e4c), QWord($e61acf033d1a45df), QWord($8fd0c16206306bab), QWord($b3c4f1ba87bc8696),
    QWord($e0b62e2929aba83c), QWord($8c71dcd9ba0b4925), QWord($af8e5410288e1b6f), QWord($db71e91432b1a24a),
    QWord($892731ac9faf056e), QWord($ab70fe17c79ac6ca), QWord($d64d3d9db981787d), QWord($85f0468293f0eb4e),
    QWord($a76c582338ed2621), QWord($d1476e2c07286faa), QWord($82cca4db847945ca), QWord($a37fce126597973c),
    QWord($cc5fc196fefd7d0c), QWord($ff77b1fcbebcdc4f), QWord($9faacf3df73609b1), QWord($c795830d75038c1d),
    QWord($f97ae3d0d2446f25), QWord($9becce62836ac577), QWord($c2e801fb244576d5), QWord($f3a20279ed56d48a),
    QWord($9845418c345644d6), QWord($be5691ef416bd60c), QWord($edec366b11c6cb8f), QWord($94b3a202eb1c3f39),
    QWord($b9e08a83a5e34f07), QWord($e858ad248f5c22c9), QWord($91376c36d99995be), QWord($b58547448ffffb2d),
    QWord($e2e69915b3fff9f9), QWord($8dd01fad907ffc3b), QWord($b1442798f49ffb4a), QWord($dd95317f31c7fa1d),
    QWord($8a7d3eef7f1cfc52), QWord($ad1c8eab5ee43b66), QWord($d863b256369d4a40), QWord($873e4f75e2224e68),
    QWord($a90de3535aaae202), QWord($d3515c2831559a83), QWord($8412d9991ed58091), QWord($a5178fff668ae0b6),
    QWord($ce5d73ff402d98e3), QWord($80fa687f881c7f8e), QWord($a139029f6a239f72), QWord($c987434744ac874e),
    QWord($fbe9141915d7a922), QWord($9d71ac8fada6c9b5), QWord($c4ce17b399107c22), QWord($f6019da07f549b2b),
    QWord($99c102844f94e0fb), QWord($c0314325637a1939), QWord($f03d93eebc589f88), QWord($96267c7535b763b5),
    QWord($bbb01b9283253ca2), QWord($ea9c227723ee8bcb), QWord($92a1958a7675175f), QWord($b749faed14125d36),
    QWord($e51c79a85916f484), QWord($8f31cc0937ae58d2), QWord($b2fe3f0b8599ef07), QWord($dfbdcece67006ac9),
    QWord($8bd6a141006042bd), QWord($aecc49914078536d), QWord($da7f5bf590966848), QWord($888f99797a5e012d),
    QWord($aab37fd7d8f58178), QWord($d5605fcdcf32e1d6), QWord($855c3be0a17fcd26), QWord($a6b34ad8c9dfc06f),
    QWord($d0601d8efc57b08b), QWord($823c12795db6ce57), QWord($a2cb1717b52481ed), QWord($cb7ddcdda26da268),
    QWord($fe5d54150b090b02), QWord($9efa548d26e5a6e1), QWord($c6b8e9b0709f109a), QWord($f867241c8cc6d4c0),
    QWord($9b407691d7fc44f8), QWord($c21094364dfb5636), QWord($f294b943e17a2bc4), QWord($979cf3ca6cec5b5a),
    QWord($bd8430bd08277231), QWord($ece53cec4a314ebd), QWord($940f4613ae5ed136), QWord($b913179899f68584),
    QWord($e757dd7ec07426e5), QWord($9096ea6f3848984f), QWord($b4bca50b065abe63), QWord($e1ebce4dc7f16dfb),
    QWord($8d3360f09cf6e4bd), QWord($b080392cc4349dec), QWord($dca04777f541c567), QWord($89e42caaf9491b60),
    QWord($ac5d37d5b79b6239), QWord($d77485cb25823ac7), QWord($86a8d39ef77164bc), QWord($a8530886b54dbdeb),
    QWord($d267caa862a12d66), QWord($8380dea93da4bc60), QWord($a46116538d0deb78), QWord($cd795be870516656),
    QWord($806bd9714632dff6), QWord($a086cfcd97bf97f3), QWord($c8a883c0fdaf7df0), QWord($fad2a4b13d1b5d6c),
    QWord($9cc3a6eec6311a63), QWord($c3f490aa77bd60fc), QWord($f4f1b4d515acb93b), QWord($991711052d8bf3c5),
    QWord($bf5cd54678eef0b6), QWord($ef340a98172aace4), QWord($9580869f0e7aac0e), QWord($bae0a846d2195712),
    QWord($e998d258869facd7), QWord($91ff83775423cc06), QWord($b67f6455292cbf08), QWord($e41f3d6a7377eeca),
    QWord($8e938662882af53e), QWord($b23867fb2a35b28d), QWord($dec681f9f4c31f31), QWord($8b3c113c38f9f37e),
    QWord($ae0b158b4738705e), QWord($d98ddaee19068c76), QWord($87f8a8d4cfa417c9), QWord($a9f6d30a038d1dbc),
    QWord($d47487cc8470652b), QWord($84c8d4dfd2c63f3b), QWord($a5fb0a17c777cf09), QWord($cf79cc9db955c2cc),
    QWord($81ac1fe293d599bf), QWord($a21727db38cb002f), QWord($ca9cf1d206fdc03b), QWord($fd442e4688bd304a),
    QWord($9e4a9cec15763e2e), QWord($c5dd44271ad3cdba), QWord($f7549530e188c128), QWord($9a94dd3e8cf578b9),
    QWord($c13a148e3032d6e7), QWord($f18899b1bc3f8ca1), QWord($96f5600f15a7b7e5), QWord($bcb2b812db11a5de),
    QWord($ebdf661791d60f56), QWord($936b9fcebb25c995), QWord($b84687c269ef3bfb), QWord($e65829b3046b0afa),
    QWord($8ff71a0fe2c2e6dc), QWord($b3f4e093db73a093), QWord($e0f218b8d25088b8), QWord($8c974f7383725573),
    QWord($afbd2350644eeacf), QWord($dbac6c247d62a583), QWord($894bc396ce5da772), QWord($ab9eb47c81f5114f),
    QWord($d686619ba27255a2), QWord($8613fd0145877585), QWord($a798fc4196e952e7), QWord($d17f3b51fca3a7a0),
    QWord($82ef85133de648c4), QWord($a3ab66580d5fdaf5), QWord($cc963fee10b7d1b3), QWord($ffbbcfe994e5c61f),
    QWord($9fd561f1fd0f9bd3), QWord($c7caba6e7c5382c8), QWord($f9bd690a1b68637b), QWord($9c1661a651213e2d),
    QWord($c31bfa0fe5698db8), QWord($f3e2f893dec3f126), QWord($986ddb5c6b3a76b7), QWord($be89523386091465),
    QWord($ee2ba6c0678b597f), QWord($94db483840b717ef), QWord($ba121a4650e4ddeb), QWord($e896a0d7e51e1566),
    QWord($915e2486ef32cd60), QWord($b5b5ada8aaff80b8), QWord($e3231912d5bf60e6), QWord($8df5efabc5979c8f),
    QWord($b1736b96b6fd83b3), QWord($ddd0467c64bce4a0), QWord($8aa22c0dbef60ee4), QWord($ad4ab7112eb3929d),
    QWord($d89d64d57a607744), QWord($87625f056c7c4a8b), QWord($a93af6c6c79b5d2d), QWord($d389b47879823479),
    QWord($843610cb4bf160cb), QWord($a54394fe1eedb8fe), QWord($ce947a3da6a9273e), QWord($811ccc668829b887),
    QWord($a163ff802a3426a8), QWord($c9bcff6034c13052), QWord($fc2c3f3841f17c67), QWord($9d9ba7832936edc0),
    QWord($c5029163f384a931), QWord($f64335bcf065d37d), QWord($99ea0196163fa42e), QWord($c06481fb9bcf8d39),
    QWord($f07da27a82c37088), QWord($964e858c91ba2655), QWord($bbe226efb628afea), QWord($eadab0aba3b2dbe5),
    QWord($92c8ae6b464fc96f), QWord($b77ada0617e3bbcb), QWord($e55990879ddcaabd), QWord($8f57fa54c2a9eab6),
    QWord($b32df8e9f3546564), QWord($dff9772470297ebd), QWord($8bfbea76c619ef36), QWord($aefae51477a06b03),
    QWord($dab99e59958885c4), QWord($88b402f7fd75539b), QWord($aae103b5fcd2a881), QWord($d59944a37c0752a2),
    QWord($857fcae62d8493a5), QWord($a6dfbd9fb8e5b88e), QWord($d097ad07a71f26b2), QWord($825ecc24c873782f),
    QWord($a2f67f2dfa90563b), QWord($cbb41ef979346bca), QWord($fea126b7d78186bc), QWord($9f24b832e6b0f436),
    QWord($c6ede63fa05d3143), QWord($f8a95fcf88747d94), QWord($9b69dbe1b548ce7c), QWord($c24452da229b021b),
    QWord($f2d56790ab41c2a2), QWord($97c560ba6b0919a5), QWord($bdb6b8e905cb600f), QWord($ed246723473e3813),
    QWord($9436c0760c86e30b), QWord($b94470938fa89bce), QWord($e7958cb87392c2c2), QWord($90bd77f3483bb9b9),
    QWord($b4ecd5f01a4aa828), QWord($e2280b6c20dd5232), QWord($8d590723948a535f), QWord($b0af48ec79ace837),
    QWord($dcdb1b2798182244), QWord($8a08f0f8bf0f156b), QWord($ac8b2d36eed2dac5), QWord($d7adf884aa879177),
    QWord($86ccbb52ea94baea), QWord($a87fea27a539e9a5), QWord($d29fe4b18e88640e), QWord($83a3eeeef9153e89),
    QWord($a48ceaaab75a8e2b), QWord($cdb02555653131b6), QWord($808e17555f3ebf11), QWord($a0b19d2ab70e6ed6),
    QWord($c8de047564d20a8b), QWord($fb158592be068d2e), QWord($9ced737bb6c4183d), QWord($c428d05aa4751e4c),
    QWord($f53304714d9265df), QWord($993fe2c6d07b7fab), QWord($bf8fdb78849a5f96), QWord($ef73d256a5c0f77c),
    QWord($95a8637627989aad), QWord($bb127c53b17ec159), QWord($e9d71b689dde71af), QWord($9226712162ab070d),
    QWord($b6b00d69bb55c8d1), QWord($e45c10c42a2b3b05), QWord($8eb98a7a9a5b04e3), QWord($b267ed1940f1c61c),
    QWord($df01e85f912e37a3), QWord($8b61313bbabce2c6), QWord($ae397d8aa96c1b77), QWord($d9c7dced53c72255),
    QWord($881cea14545c7575), QWord($aa242499697392d2), QWord($d4ad2dbfc3d07787), QWord($84ec3c97da624ab4),
    QWord($a6274bbdd0fadd61), QWord($cfb11ead453994ba), QWord($81ceb32c4b43fcf4), QWord($a2425ff75e14fc31),
    QWord($cad2f7f5359a3b3e), QWord($fd87b5f28300ca0d), QWord($9e74d1b791e07e48), QWord($c612062576589dda),
    QWord($f79687aed3eec551), QWord($9abe14cd44753b52), QWord($c16d9a0095928a27), QWord($f1c90080baf72cb1),
    QWord($971da05074da7bee), QWord($bce5086492111aea), QWord($ec1e4a7db69561a5), QWord($9392ee8e921d5d07),
    QWord($b877aa3236a4b449), QWord($e69594bec44de15b), QWord($901d7cf73ab0acd9), QWord($b424dc35095cd80f),
    QWord($e12e13424bb40e13), QWord($8cbccc096f5088cb), QWord($afebff0bcb24aafe), QWord($dbe6fecebdedd5be),
    QWord($89705f4136b4a597), QWord($abcc77118461cefc), QWord($d6bf94d5e57a42bc), QWord($8637bd05af6c69b5),
    QWord($a7c5ac471b478423), QWord($d1b71758e219652b), QWord($83126e978d4fdf3b), QWord($a3d70a3d70a3d70a),
    QWord($cccccccccccccccc), QWord($8000000000000000), QWord($a000000000000000), QWord($c800000000000000),
    QWord($fa00000000000000), QWord($9c40000000000000), QWord($c350000000000000), QWord($f424000000000000),
    QWord($9896800000000000), QWord($bebc200000000000), QWord($ee6b280000000000), QWord($9502f90000000000),
    QWord($ba43b74000000000), QWord($e8d4a51000000000), QWord($9184e72a00000000), QWord($b5e620f480000000),
    QWord($e35fa931a0000000), QWord($8e1bc9bf04000000), QWord($b1a2bc2ec5000000), QWord($de0b6b3a76400000),
    QWord($8ac7230489e80000), QWord($ad78ebc5ac620000), QWord($d8d726b7177a8000), QWord($878678326eac9000),
    QWord($a968163f0a57b400), QWord($d3c21bcecceda100), QWord($84595161401484a0), QWord($a56fa5b99019a5c8),
    QWord($cecb8f27f4200f3a), QWord($813f3978f8940984), QWord($a18f07d736b90be5), QWord($c9f2c9cd04674ede),
    QWord($fc6f7c4045812296), QWord($9dc5ada82b70b59d), QWord($c5371912364ce305), QWord($f684df56c3e01bc6),
    QWord($9a130b963a6c115c), QWord($c097ce7bc90715b3), QWord($f0bdc21abb48db20), QWord($96769950b50d88f4),
    QWord($bc143fa4e250eb31), QWord($eb194f8e1ae525fd), QWord($92efd1b8d0cf37be), QWord($b7abc627050305ad),
    QWord($e596b7b0c643c719), QWord($8f7e32ce7bea5c6f), QWord($b35dbf821ae4f38b), QWord($e0352f62a19e306e),
    QWord($8c213d9da502de45), QWord($af298d050e4395d6), QWord($daf3f04651d47b4c), QWord($88d8762bf324cd0f),
    QWord($ab0e93b6efee0053), QWord($d5d238a4abe98068), QWord($85a36366eb71f041), QWord($a70c3c40a64e6c51),
    QWord($d0cf4b50cfe20765), QWord($82818f1281ed449f), QWord($a321f2d7226895c7), QWord($cbea6f8ceb02bb39),
    QWord($fee50b7025c36a08), QWord($9f4f2726179a2245), QWord($c722f0ef9d80aad6), QWord($f8ebad2b84e0d58b),
    QWord($9b934c3b330c8577), QWord($c2781f49ffcfa6d5), QWord($f316271c7fc3908a), QWord($97edd871cfda3a56),
    QWord($bde94e8e43d0c8ec), QWord($ed63a231d4c4fb27), QWord($945e455f24fb1cf8), QWord($b975d6b6ee39e436),
    QWord($e7d34c64a9c85d44), QWord($90e40fbeea1d3a4a), QWord($b51d13aea4a488dd), QWord($e264589a4dcdab14),
    QWord($8d7eb76070a08aec), QWord($b0de65388cc8ada8), QWord($dd15fe86affad912), QWord($8a2dbf142dfcc7ab),
    QWord($acb92ed9397bf996), QWord($d7e77a8f87daf7fb), QWord($86f0ac99b4e8dafd), QWord($a8acd7c0222311bc),
    QWord($d2d80db02aabd62b), QWord($83c7088e1aab65db), QWord($a4b8cab1a1563f52), QWord($cde6fd5e09abcf26),
    QWord($80b05e5ac60b6178), QWord($a0dc75f1778e39d6), QWord($c913936dd571c84c), QWord($fb5878494ace3a5f),
    QWord($9d174b2dcec0e47b), QWord($c45d1df942711d9a), QWord($f5746577930d6500), QWord($9968bf6abbe85f20),
    QWord($bfc2ef456ae276e8), QWord($efb3ab16c59b14a2), QWord($95d04aee3b80ece5), QWord($bb445da9ca61281f),
    QWord($ea1575143cf97226), QWord($924d692ca61be758), QWord($b6e0c377cfa2e12e), QWord($e498f455c38b997a),
    QWord($8edf98b59a373fec), QWord($b2977ee300c50fe7), QWord($df3d5e9bc0f653e1), QWord($8b865b215899f46c),
    QWord($ae67f1e9aec07187), QWord($da01ee641a708de9), QWord($884134fe908658b2), QWord($aa51823e34a7eede),
    QWord($d4e5e2cdc1d1ea96), QWord($850fadc09923329e), QWord($a6539930bf6bff45), QWord($cfe87f7cef46ff16),
    QWord($81f14fae158c5f6e), QWord($a26da3999aef7749), QWord($cb090c8001ab551c), QWord($fdcb4fa002162a63),
    QWord($9e9f11c4014dda7e), QWord($c646d63501a1511d), QWord($f7d88bc24209a565), QWord($9ae757596946075f),
    QWord($c1a12d2fc3978937), QWord($f209787bb47d6b84), QWord($9745eb4d50ce6332), QWord($bd176620a501fbff),
    QWord($ec5d3fa8ce427aff), QWord($93ba47c980e98cdf), QWord($b8a8d9bbe123f017), QWord($e6d3102ad96cec1d),
    QWord($9043ea1ac7e41392), QWord($b454e4a179dd1877), QWord($e16a1dc9d8545e94), QWord($8ce2529e2734bb1d),
    QWord($b01ae745b101e9e4), QWord($dc21a1171d42645d), QWord($899504ae72497eba), QWord($abfa45da0edbde69),
    QWord($d6f8d7509292d603), QWord($865b86925b9bc5c2), QWord($a7f26836f282b732), QWord($d1ef0244af2364ff),
    QWord($8335616aed761f1f), QWord($a402b9c5a8d3a6e7), QWord($cd036837130890a1), QWord($802221226be55a64),
    QWord($a02aa96b06deb0fd), QWord($c83553c5c8965d3d), QWord($fa42a8b73abbf48c), QWord($9c69a97284b578d7),
    QWord($c38413cf25e2d70d), QWord($f46518c2ef5b8cd1), QWord($98bf2f79d5993802), QWord($beeefb584aff8603),
    QWord($eeaaba2e5dbf6784), QWord($952ab45cfa97a0b2), QWord($ba756174393d88df), QWord($e912b9d1478ceb17),
    QWord($91abb422ccb812ee), QWord($b616a12b7fe617aa), QWord($e39c49765fdf9d94), QWord($8e41ade9fbebc27d),
    QWord($b1d219647ae6b31c), QWord($de469fbd99a05fe3), QWord($8aec23d680043bee), QWord($ada72ccc20054ae9),
    QWord($d910f7ff28069da4), QWord($87aa9aff79042286), QWord($a99541bf57452b28), QWord($d3fa922f2d1675f2),
    QWord($847c9b5d7c2e09b7), QWord($a59bc234db398c25), QWord($cf02b2c21207ef2e), QWord($8161afb94b44f57d),
    QWord($a1ba1ba79e1632dc), QWord($ca28a291859bbf93), QWord($fcb2cb35e702af78), QWord($9defbf01b061adab),
    QWord($c56baec21c7a1916), QWord($f6c69a72a3989f5b), QWord($9a3c2087a63f6399), QWord($c0cb28a98fcf3c7f),
    QWord($f0fdf2d3f3c30b9f), QWord($969eb7c47859e743), QWord($bc4665b596706114), QWord($eb57ff22fc0c7959),
    QWord($9316ff75dd87cbd8), QWord($b7dcbf5354e9bece), QWord($e5d3ef282a242e81), QWord($8fa475791a569d10),
    QWord($b38d92d760ec4455), QWord($e070f78d3927556a), QWord($8c469ab843b89562), QWord($af58416654a6babb),
    QWord($db2e51bfe9d0696a), QWord($88fcf317f22241e2), QWord($ab3c2fddeeaad25a), QWord($d60b3bd56a5586f1),
    QWord($85c7056562757456), QWord($a738c6bebb12d16c), QWord($d106f86e69d785c7), QWord($82a45b450226b39c),
    QWord($a34d721642b06084), QWord($cc20ce9bd35c78a5), QWord($ff290242c83396ce), QWord($9f79a169bd203e41),
    QWord($c75809c42c684dd1), QWord($f92e0c3537826145), QWord($9bbcc7a142b17ccb), QWord($c2abf989935ddbfe),
    QWord($f356f7ebf83552fe), QWord($98165af37b2153de), QWord($be1bf1b059e9a8d6), QWord($eda2ee1c7064130c),
    QWord($9485d4d1c63e8be7), QWord($b9a74a0637ce2ee1), QWord($e8111c87c5c1ba99), QWord($910ab1d4db9914a0),
    QWord($b54d5e4a127f59c8), QWord($e2a0b5dc971f303a), QWord($8da471a9de737e24), QWord($b10d8e1456105dad),
    QWord($dd50f1996b947518), QWord($8a5296ffe33cc92f), QWord($ace73cbfdc0bfb7b), QWord($d8210befd30efa5a),
    QWord($8714a775e3e95c78), QWord($a8d9d1535ce3b396), QWord($d31045a8341ca07c), QWord($83ea2b892091e44d),
    QWord($a4e4b66b68b65d60), QWord($ce1de40642e3f4b9), QWord($80d2ae83e9ce78f3), QWord($a1075a24e4421730),
    QWord($c94930ae1d529cfc), QWord($fb9b7cd9a4a7443c), QWord($9d412e0806e88aa5), QWord($c491798a08a2ad4e),
    QWord($f5b5d7ec8acb58a2), QWord($9991a6f3d6bf1765), QWord($bff610b0cc6edd3f), QWord($eff394dcff8a948e),
    QWord($95f83d0a1fb69cd9), QWord($bb764c4ca7a4440f), QWord($ea53df5fd18d5513), QWord($92746b9be2f8552c),
    QWord($b7118682dbb66a77), QWord($e4d5e82392a40515), QWord($8f05b1163ba6832d), QWord($b2c71d5bca9023f8),
    QWord($df78e4b2bd342cf6), QWord($8bab8eefb6409c1a), QWord($ae9672aba3d0c320), QWord($da3c0f568cc4f3e8),
    QWord($8865899617fb1871), QWord($aa7eebfb9df9de8d), QWord($d51ea6fa85785631), QWord($8533285c936b35de),
    QWord($a67ff273b8460356), QWord($d01fef10a657842c), QWord($8213f56a67f6b29b), QWord($a298f2c501f45f42),
    QWord($cb3f2f7642717713), QWord($fe0efb53d30dd4d7), QWord($9ec95d1463e8a506), QWord($c67bb4597ce2ce48),
    QWord($f81aa16fdc1b81da), QWord($9b10a4e5e9913128), QWord($c1d4ce1f63f57d72), QWord($f24a01a73cf2dccf),
    QWord($976e41088617ca01), QWord($bd49d14aa79dbc82), QWord($ec9c459d51852ba2), QWord($93e1ab8252f33b45),
    QWord($b8da1662e7b00a17), QWord($e7109bfba19c0c9d), QWord($906a617d450187e2), QWord($b484f9dc9641e9da),
    QWord($e1a63853bbd26451), QWord($8d07e33455637eb2), QWord($b049dc016abc5e5f), QWord($dc5c5301c56b75f7),
    QWord($89b9b3e11b6329ba), QWord($ac2820d9623bf429), QWord($d732290fbacaf133), QWord($867f59a9d4bed6c0),
    QWord($a81f301449ee8c70), QWord($d226fc195c6a2f8c), QWord($83585d8fd9c25db7), QWord($a42e74f3d032f525),
    QWord($cd3a1230c43fb26f), QWord($80444b5e7aa7cf85), QWord($a0555e361951c366), QWord($c86ab5c39fa63440),
    QWord($fa856334878fc150), QWord($9c935e00d4b9d8d2), QWord($c3b8358109e84f07), QWord($f4a642e14c6262c8),
    QWord($98e7e9cccfbd7dbd), QWord($bf21e44003acdd2c), QWord($eeea5d5004981478), QWord($95527a5202df0ccb),
    QWord($baa718e68396cffd), QWord($e950df20247c83fd), QWord($91d28b7416cdd27e), QWord($b6472e511c81471d),
    QWord($e3d8f9e563a198e5), QWord($8e679c2f5e44ff8f));

  EL_MANTIS_128: array[ELDBL_LOW_POWER..ELDBL_HIGH_POWER] of QWord = (
    QWord($419ea3bd35385e2d), QWord($52064cac828675b9), QWord($7343efebd1940993), QWord($1014ebe6c5f90bf8),
    QWord($d41a26e077774ef6), QWord($8920b098955522b4), QWord($55b46e5f5d5535b0), QWord($eb2189f734aa831d),
    QWord($a5e9ec7501d523e4), QWord($47b233c92125366e), QWord($999ec0bb696e840a), QWord($c00670ea43ca250d),
    QWord($380406926a5e5728), QWord($c605083704f5ecf2), QWord($f7864a44c633682e), QWord($7ab3ee6afbe0211d),
    QWord($5960ea05bad82964), QWord($6fb92487298e33bd), QWord($a5d3b6d479f8e056), QWord($8f48a4899877186c),
    QWord($331acdabfe94de87), QWord($9ff0c08b7f1d0b14), QWord($7ecf0ae5ee44dd9),  QWord($c9e82cd9f69d6150),
    QWord($be311c083a225cd2), QWord($6dbd630a48aaf406), QWord($92cbbccdad5b108),  QWord($25bbf56008c58ea5),
    QWord($af2af2b80af6f24e), QWord($1af5af660db4aee1), QWord($50d98d9fc890ed4d), QWord($e50ff107bab528a0),
    QWord($1e53ed49a96272c8), QWord($25e8e89c13bb0f7a), QWord($77b191618c54e9ac), QWord($d59df5b9ef6a2417),
    QWord($4b0573286b44ad1d), QWord($4ee367f9430aec32), QWord($229c41f793cda73f), QWord($6b43527578c1110f),
    QWord($830a13896b78aaa9), QWord($23cc986bc656d553), QWord($2cbfbe86b7ec8aa8), QWord($7bf7d71432f3d6a9),
    QWord($daf5ccd93fb0cc53), QWord($d1b3400f8f9cff68), QWord($23100809b9c21fa1), QWord($abd40a0c2832a78a),
    QWord($16c90c8f323f516c), QWord($ae3da7d97f6792e3), QWord($99cd11cfdf41779c), QWord($40405643d711d583),
    QWord($482835ea666b2572), QWord($da3243650005eecf), QWord($90bed43e40076a82), QWord($5a7744a6e804a291),
    QWord($711515d0a205cb36), QWord($d5a5b44ca873e03),  QWord($e858790afe9486c2), QWord($626e974dbe39a872),
    QWord($fb0a3d212dc8128f), QWord($7ce66634bc9d0b99), QWord($1c1fffc1ebc44e80), QWord($a327ffb266b56220),
    QWord($4bf1ff9f0062baa8), QWord($6f773fc3603db4a9), QWord($cb550fb4384d21d3), QWord($7e2a53a146606a48),
    QWord($2eda7444cbfc426d), QWord($fa911155fefb5308), QWord($793555ab7eba27ca), QWord($4bc1558b2f3458de),
    QWord($9eb1aaedfb016f16), QWord($465e15a979c1cadc), QWord($bfacd89ec191ec9),  QWord($cef980ec671f667b),
    QWord($82b7e12780e7401a), QWord($d1b2ecb8b0908810), QWord($861fa7e6dcb4aa15), QWord($67a791e093e1d49a),
    QWord($e0c8bb2c5c6d24e0), QWord($58fae9f773886e18), QWord($af39a475506a899e), QWord($6d8406c952429603),
    QWord($c8e5087ba6d33b83), QWord($fb1e4a9a90880a64), QWord($5cf2eea09a55067f), QWord($f42faa48c0ea481e),
    QWord($f13b94daf124da26), QWord($76c53d08d6b70858), QWord($54768c4b0c64ca6e), QWord($a9942f5dcf7dfd09),
    QWord($d3f93b35435d7c4c), QWord($c47bc5014a1a6daf), QWord($359ab6419ca1091b), QWord($c30163d203c94b62),
    QWord($79e0de63425dcf1d), QWord($985915fc12f542e4), QWord($3e6f5b7b17b2939d), QWord($a705992ceecf9c42),
    QWord($50c6ff782a838353), QWord($a4f8bf5635246428), QWord($871b7795e136be99), QWord($28e2557b59846e3f),
    QWord($331aeada2fe589cf), QWord($3ff0d2c85def7621), QWord($fed077a756b53a9),  QWord($d3e8495912c62894),
    QWord($64712dd7abbbd95c), QWord($bd8d794d96aacfb3), QWord($ecf0d7a0fc5583a0), QWord($f41686c49db57244),
    QWord($311c2875c522ced5), QWord($7d633293366b828b), QWord($ae5dff9c02033197), QWord($d9f57f830283fdfc),
    QWord($d072df63c324fd7b), QWord($4247cb9e59f71e6d), QWord($52d9be85f074e608), QWord($67902e276c921f8b),
    QWord($ba1cd8a3db53b6),   QWord($80e8a40eccd228a4), QWord($6122cd128006b2cd), QWord($796b805720085f81),
    QWord($cbe3303674053bb0), QWord($bedbfc4411068a9c), QWord($ee92fb5515482d44), QWord($751bdd152d4d1c4a),
    QWord($d262d45a78a0635d), QWord($86fb897116c87c34), QWord($d45d35e6ae3d4da0), QWord($8974836059cca109),
    QWord($2bd1a438703fc94b), QWord($7b6306a34627ddcf), QWord($1a3bc84c17b1d542), QWord($20caba5f1d9e4a93),
    QWord($547eb47b7282ee9c), QWord($e99e619a4f23aa43), QWord($6405fa00e2ec94d4), QWord($de83bc408dd3dd04),
    QWord($9624ab50b148d445), QWord($3badd624dd9b0957), QWord($e54ca5d70a80e5d6), QWord($5e9fcf4ccd211f4c),
    QWord($7647c3200069671f), QWord($29ecd9f40041e073), QWord($f468107100525890), QWord($7182148d4066eeb4),
    QWord($c6f14cd848405530), QWord($b8ada00e5a506a7c), QWord($a6d90811f0e4851c), QWord($908f4a166d1da663),
    QWord($9a598e4e043287fe), QWord($40eff1e1853f29fd), QWord($d12bee59e68ef47c), QWord($82bb74f8301958ce),
    QWord($e36a52363c1faf01), QWord($dc44e6c3cb279ac1), QWord($29ab103a5ef8c0b9), QWord($7415d448f6b6f0e7),
    QWord($111b495b3464ad21), QWord($cab10dd900beec34), QWord($3d5d514f40eea742), QWord($cb4a5a3112a5112),
    QWord($47f0e785eaba72ab), QWord($59ed216765690f56), QWord($306869c13ec3532c), QWord($1e414218c73a13fb),
    QWord($e5d1929ef90898fa), QWord($df45f746b74abf39), QWord($6b8bba8c328eb783), QWord($66ea92f3f326564),
    QWord($c80a537b0efefebd), QWord($bd06742ce95f5f36), QWord($2c48113823b73704), QWord($f75a15862ca504c5),
    QWord($9a984d73dbe722fb), QWord($c13e60d0d2e0ebba), QWord($318df905079926a8), QWord($fdf17746497f7052),
    QWord($feb6ea8bedefa633), QWord($fe64a52ee96b8fc0), QWord($3dfdce7aa3c673b0), QWord($6bea10ca65c084e),
    QWord($486e494fcff30a62), QWord($5a89dba3c3efccfa), QWord($f89629465a75e01c), QWord($f6bbb397f1135823),
    QWord($746aa07ded582e2c), QWord($a8c2a44eb4571cdc), QWord($92f34d62616ce413), QWord($77b020baf9c81d17),
    QWord($ace1474dc1d122e),  QWord($d819992132456ba),  QWord($10e1fff697ed6c69), QWord($ca8d3ffa1ef463c1),
    QWord($bd308ff8a6b17cb2), QWord($ac7cb3f6d05ddbde), QWord($6bcdf07a423aa96b), QWord($86c16c98d2c953c6),
    QWord($e871c7bf077ba8b7), QWord($11471cd764ad4972), QWord($d598e40d3dd89bcf), QWord($4aff1d108d4ec2c3),
    QWord($cedf722a585139ba), QWord($c2974eb4ee658828), QWord($733d226229feea32), QWord($806357d5a3f525f),
    QWord($ca07c2dcb0cf26f7), QWord($fc89b393dd02f0b5), QWord($bbac2078d443ace2), QWord($d54b944b84aa4c0d),
    QWord($a9e795e65d4df11),  QWord($4d4617b5ff4a16d5), QWord($504bced1bf8e4e45), QWord($e45ec2862f71e1d6),
    QWord($5d767327bb4e5a4c), QWord($3a6a07f8d510f86f), QWord($890489f70a55368b), QWord($2b45ac74ccea842e),
    QWord($3b0b8bc90012929d), QWord($9ce6ebb40173744),  QWord($cc420a6a101d0515), QWord($9fa946824a12232d),
    QWord($47939822dc96abf9), QWord($59787e2b93bc56f7), QWord($57eb4edb3c55b65a), QWord($ede622920b6b23f1),
    QWord($e95fab368e45eced), QWord($11dbcb0218ebb414), QWord($d652bdc29f26a119), QWord($4be76d3346f0495f),
    QWord($6f70a4400c562ddb), QWord($cb4ccd500f6bb952), QWord($7e2000a41346a7a7), QWord($8ed400668c0c28c8),
    QWord($728900802f0f32fa), QWord($4f2b40a03ad2ffb9), QWord($e2f610c84987bfa8), QWord($dd9ca7d2df4d7c9),
    QWord($91503d1c79720dbb), QWord($75a44c6397ce912a), QWord($c986afbe3ee11aba), QWord($fbe85badce996168),
    QWord($fae27299423fb9c3), QWord($dccd879fc967d41a), QWord($5400e987bbc1c920), QWord($290123e9aab23b68),
    QWord($f9a0b6720aaf6521), QWord($f808e40e8d5b3e69), QWord($b60b1d1230b20e04), QWord($b1c6f22b5e6f48c2),
    QWord($1e38aeb6360b1af3), QWord($25c6da63c38de1b0), QWord($579c487e5a38ad0e), QWord($2d835a9df0c6d851),
    QWord($f8e431456cf88e65), QWord($1b8e9ecb641b58ff), QWord($e272467e3d222f3f), QWord($5b0ed81dcc6abb0f),
    QWord($98e947129fc2b4e9), QWord($3f2398d747b36224), QWord($8eec7f0d19a03aad), QWord($1953cf68300424ac),
    QWord($5fa8c3423c052dd7), QWord($3792f412cb06794d), QWord($e2bbd88bbee40bd0), QWord($5b6aceaeae9d0ec4),
    QWord($f245825a5a445275), QWord($eed6e2f0f0d56712), QWord($55464dd69685606b), QWord($aa97e14c3c26b886),
    QWord($d53dd99f4b3066a8), QWord($e546a8038efe4029), QWord($de98520472bdd033), QWord($963e66858f6d4440),
    QWord($dde7001379a44aa8), QWord($5560c018580d5d52), QWord($aab8f01e6e10b4a6), QWord($cab3961304ca70e8),
    QWord($3d607b97c5fd0d22), QWord($8cb89a7db77c506a), QWord($77f3608e92adb242), QWord($55f038b237591ed3),
    QWord($6b6c46dec52f6688), QWord($2323ac4b3b3da015), QWord($abec975e0a0d081a), QWord($96e7bd358c904a21),
    QWord($7e50d64177da2e54), QWord($dde50bd1d5d0b9e9), QWord($955e4ec64b44e864), QWord($bd5af13bef0b113e),
    QWord($ecb1ad8aeacdd58e), QWord($67de18eda5814af2), QWord($80eacf948770ced7), QWord($a1258379a94d028d),
    QWord($96ee45813a04330),  QWord($8bca9d6e188853fc), QWord($775ea264cf55347d), QWord($95364afe032a819d),
    QWord($3a83ddbd83f52204), QWord($c4926a9672793542), QWord($75b7053c0f178293), QWord($5324c68b12dd6338),
    QWord($d3f6fc16ebca5e03), QWord($88f4bb1ca6bcf584), QWord($2b31e9e3d06c32e5), QWord($3aff322e62439fcf),
    QWord($9befeb9fad487c2),  QWord($4c2ebe687989a9b3), QWord($f9d37014bf60a10),  QWord($538484c19ef38c94),
    QWord($2865a5f206b06fb9), QWord($f93f87b7442e45d3), QWord($f78f69a51539d748), QWord($b573440e5a884d1b),
    QWord($31680a88f8953030), QWord($fdc20d2b36ba7c3d), QWord($3d32907604691b4c), QWord($a63f9a49c2c1b10f),
    QWord($fcf80dc33721d53),  QWord($d3c36113404ea4a8), QWord($645a1cac083126e9), QWord($3d70a3d70a3d70a3),
    QWord($cccccccccccccccc), QWord($0),                QWord($0),                QWord($0),
    QWord($0),                QWord($0),                QWord($0),                QWord($0),
    QWord($0),                QWord($0),                QWord($0),                QWord($0),
    QWord($0),                QWord($0),                QWord($0),                QWord($0),
    QWord($0),                QWord($0),                QWord($0),                QWord($0),
    QWord($0),                QWord($0),                QWord($0),                QWord($0),
    QWord($0),                QWord($0),                QWord($0),                QWord($0),
    QWord($0),                QWord($4000000000000000), QWord($5000000000000000), QWord($a400000000000000),
    QWord($4d00000000000000), QWord($f020000000000000), QWord($6c28000000000000), QWord($c732000000000000),
    QWord($3c7f400000000000), QWord($4b9f100000000000), QWord($1e86d40000000000), QWord($1314448000000000),
    QWord($17d955a000000000), QWord($5dcfab0800000000), QWord($5aa1cae500000000), QWord($f14a3d9e40000000),
    QWord($6d9ccd05d0000000), QWord($e4820023a2000000), QWord($dda2802c8a800000), QWord($d50b2037ad200000),
    QWord($4526f422cc340000), QWord($9670b12b7f410000), QWord($3c0cdd765f114000), QWord($a5880a69fb6ac800),
    QWord($8eea0d047a457a00), QWord($72a4904598d6d880), QWord($47a6da2b7f864750), QWord($999090b65f67d924),
    QWord($fff4b4e3f741cf6d), QWord($bff8f10e7a8921a4), QWord($aff72d52192b6a0d), QWord($9bf4f8a69f764490),
    QWord($2f236d04753d5b4),  QWord($1d762422c946590),  QWord($424d3ad2b7b97ef5), QWord($d2e0898765a7deb2),
    QWord($63cc55f49f88eb2f), QWord($3cbf6b71c76b25fb), QWord($8bef464e3945ef7a), QWord($97758bf0e3cbb5ac),
    QWord($3d52eeed1cbea317), QWord($4ca7aaa863ee4bdd), QWord($8fe8caa93e74ef6a), QWord($b3e2fd538e122b44),
    QWord($60dbbca87196b616), QWord($bc8955e946fe31cd), QWord($6babab6398bdbe41), QWord($c696963c7eed2dd1),
    QWord($fc1e1de5cf543ca2), QWord($3b25a55f43294bcb), QWord($49ef0eb713f39ebe), QWord($6e3569326c784337),
    QWord($49c2c37f07965404), QWord($dc33745ec97be906), QWord($69a028bb3ded71a3), QWord($c40832ea0d68ce0c),
    QWord($f50a3fa490c30190), QWord($792667c6da79e0fa), QWord($577001b891185938), QWord($ed4c0226b55e6f86),
    QWord($544f8158315b05b4), QWord($696361ae3db1c721), QWord($3bc3a19cd1e38e9),  QWord($4ab48a04065c723),
    QWord($62eb0d64283f9c76), QWord($3ba5d0bd324f8394), QWord($ca8f44ec7ee36479), QWord($7e998b13cf4e1ecb),
    QWord($9e3fedd8c321a67e), QWord($c5cfe94ef3ea101e), QWord($bba1f1d158724a12), QWord($2a8a6e45ae8edc97),
    QWord($f52d09d71a3293bd), QWord($593c2626705f9c56), QWord($6f8b2fb00c77836c), QWord($b6dfb9c0f956447),
    QWord($4724bd4189bd5eac), QWord($58edec91ec2cb657), QWord($2f2967b66737e3ed), QWord($bd79e0d20082ee74),
    QWord($ecd8590680a3aa11), QWord($e80e6f4820cc9495), QWord($3109058d147fdcdd), QWord($bd4b46f0599fd415),
    QWord($6c9e18ac7007c91a), QWord($3e2cf6bc604ddb0),  QWord($84db8346b786151c), QWord($e612641865679a63),
    QWord($4fcb7e8f3f60c07e), QWord($e3be5e330f38f09d), QWord($5cadf5bfd3072cc5), QWord($73d9732fc7c8f7f6),
    QWord($2867e7fddcdd9afa), QWord($b281e1fd541501b8), QWord($1f225a7ca91a4226), QWord($3375788de9b06958),
    QWord($52d6b1641c83ae),   QWord($c0678c5dbd23a49a), QWord($f840b7ba963646e0), QWord($b650e5a93bc3d898),
    QWord($a3e51f138ab4cebe), QWord($c66f336c36b10137), QWord($b80b0047445d4184), QWord($a60dc059157491e5),
    QWord($87c89837ad68db2f), QWord($29babe4598c311fb), QWord($f4296dd6fef3d67a), QWord($1899e4a65f58660c),
    QWord($5ec05dcff72e7f8f), QWord($76707543f4fa1f73), QWord($6a06494a791c53a8), QWord($487db9d17636892),
    QWord($45a9d2845d3c42b6), QWord($b8a2392ba45a9b2),  QWord($8e6cac7768d7141e), QWord($3207d795430cd926),
    QWord($7f44e6bd49e807b8), QWord($5f16206c9c6209a6), QWord($36dba887c37a8c0f), QWord($c2494954da2c9789),
    QWord($f2db9baa10b7bd6c), QWord($6f92829494e5acc7), QWord($cb772339ba1f17f9), QWord($ff2a760414536efb),
    QWord($fef5138519684aba), QWord($7eb258665fc25d69), QWord($ef2f773ffbd97a61), QWord($aafb550ffacfd8fa),
    QWord($95ba2a53f983cf38), QWord($dd945a747bf26183), QWord($94f971119aeef9e4), QWord($7a37cd5601aab85d),
    QWord($ac62e055c10ab33a), QWord($577b986b314d6009), QWord($ed5a7e85fda0b80b), QWord($14588f13be847307),
    QWord($596eb2d8ae258fc8), QWord($6fca5f8ed9aef3bb), QWord($25de7bb9480d5854), QWord($af561aa79a10ae6a),
    QWord($1b2ba1518094da04), QWord($90fb44d2f05d0842), QWord($353a1607ac744a53), QWord($42889b8997915ce8),
    QWord($69956135febada11), QWord($43fab9837e699095), QWord($94f967e45e03f4bb), QWord($1d1be0eebac278f5),
    QWord($6462d92a69731732), QWord($7d7b8f7503cfdcfe), QWord($5cda735244c3d43e), QWord($3a0888136afa64a7),
    QWord($88aaa1845b8fdd0),  QWord($8aad549e57273d45), QWord($36ac54e2f678864b), QWord($84576a1bb416a7dd),
    QWord($656d44a2a11c51d5), QWord($9f644ae5a4b1b325), QWord($873d5d9f0dde1fee), QWord($a90cb506d155a7ea),
    QWord($9a7f12442d588f2),  QWord($c11ed6d538aeb2f),  QWord($8f1668c8a86da5fa), QWord($f96e017d694487bc),
    QWord($37c981dcc395a9ac), QWord($85bbe253f47b1417), QWord($93956d7478ccec8e), QWord($387ac8d1970027b2),
    QWord($6997b05fcc0319e),  QWord($441fece3bdf81f03), QWord($d527e81cad7626c3), QWord($8a71e223d8d3b074),
    QWord($f6872d5667844e49), QWord($b428f8ac016561db), QWord($e13336d701beba52), QWord($ecc0024661173473),
    QWord($27f002d7f95d0190), QWord($31ec038df7b441f4), QWord($7e67047175a15271), QWord($f0062c6e984d386),
    QWord($52c07b78a3e60868), QWord($a7709a56ccdf8a82), QWord($88a66076400bb691), QWord($6acff893d00ea435),
    QWord($583f6b8c4124d43),  QWord($c3727a337a8b704a), QWord($744f18c0592e4c5c), QWord($1162def06f79df73),
    QWord($8addcb5645ac2ba8), QWord($6d953e2bd7173692), QWord($c8fa8db6ccdd0437), QWord($1d9c9892400a22a2),
    QWord($2503beb6d00cab4b), QWord($2e44ae64840fd61d), QWord($5ceaecfed289e5d2), QWord($7425a83e872c5f47),
    QWord($d12f124e28f77719), QWord($82bd6b70d99aaa6f), QWord($636cc64d1001550b), QWord($3c47f7e05401aa4e),
    QWord($65acfaec34810a71), QWord($7f1839a741a14d0d), QWord($1ede48111209a050), QWord($934aed0aab460432),
    QWord($f81da84d5617853f), QWord($36251260ab9d668e), QWord($c1d72b7c6b426019), QWord($b24cf65b8612f81f),
    QWord($dee033f26797b627), QWord($169840ef017da3b1), QWord($8e1f289560ee864e), QWord($f1a6f2bab92a27e2),
    QWord($ae10af696774b1db), QWord($acca6da1e0a8ef29), QWord($17fd090a58d32af3), QWord($ddfc4b4cef07f5b0),
    QWord($4abdaf101564f98e), QWord($9d6d1ad41abe37f1), QWord($84c86189216dc5ed), QWord($32fd3cf5b4e49bb4),
    QWord($3fbc8c33221dc2a1), QWord($fabaf3feaa5334a),  QWord($29cb4d87f2a7400e), QWord($743e20e9ef511012),
    QWord($914da9246b255416), QWord($1ad089b6c2f7548e), QWord($a184ac2473b529b1), QWord($c9e5d72d90a2741e),
    QWord($7e2fa67c7a658892), QWord($ddbb901b98feeab7), QWord($552a74227f3ea565), QWord($d53a88958f87275f),
    QWord($8a892abaf368f137), QWord($2d2b7569b0432d85), QWord($9c3b29620e29fc73), QWord($8349f3ba91b47b8f),
    QWord($241c70a936219a73), QWord($ed238cd383aa0110), QWord($f4363804324a40aa), QWord($b143c6053edcd0d5),
    QWord($dd94b7868e94050a), QWord($ca7cf2b4191c8326), QWord($fd1c2f611f63a3f0), QWord($bc633b39673c8cec),
    QWord($d5be0503e085d813), QWord($4b2d8644d8a74e18), QWord($ddf8e7d60ed1219e), QWord($cabb90e5c942b503),
    QWord($3d6a751f3b936243), QWord($cc512670a783ad4),  QWord($27fb2b80668b24c5), QWord($b1f9f660802dedf6),
    QWord($5e7873f8a0396973), QWord($db0b487b6423e1e8), QWord($91ce1a9a3d2cda62), QWord($7641a140cc7810fb),
    QWord($a9e904c87fcb0a9d), QWord($546345fa9fbdcd44), QWord($a97c177947ad4095), QWord($49ed8eabcccc485d),
    QWord($5c68f256bfff5a74), QWord($73832eec6fff3111), QWord($c831fd53c5ff7eab), QWord($ba3e7ca8b77f5e55),
    QWord($28ce1bd2e55f35eb), QWord($7980d163cf5b81b3), QWord($d7e105bcc332621f), QWord($8dd9472bf3fefaa7),
    QWord($b14f98f6f0feb951), QWord($6ed1bf9a569f33d3), QWord($a862f80ec4700c8),  QWord($cd27bb612758c0fa),
    QWord($8038d51cb897789c), QWord($e0470a63e6bd56c3), QWord($1858ccfce06cac74), QWord($f37801e0c43ebc8),
    QWord($d30560258f54e6ba), QWord($47c6b82ef32a2069), QWord($4cdc331d57fa5441), QWord($e0133fe4adf8e952),
    QWord($58180fddd97723a6), QWord($570f09eaa7ea7648));

procedure UMul64To128(const x, y: QWord; out aProd: TOWord);
{$IF DEFINED(CPUX64)}{$ASMMODE INTEL} assembler; nostackframe;
asm
{$IFDEF MSWINDOWS}
  mov rax, rcx
  mul rdx
{$ELSE MSWINDOWS}
  mov rax, rdi
  mov r8,  rdx
  mul rsi
{$ENDIF MSWINDOWS}
  mov qword ptr[r8  ], rax
  mov qword ptr[r8+8], rdx
end;
{$ELSEIF DEFINED(CPUAARCH64)} assembler; nostackframe;
asm
  mul   x3, x0, x1
  umulh x4, x0, x1
  str   x3, [x2  ]
  str   x4, [x2,8]
end;
{$ELSE }
var
  p00, p01, mid: QWord;
begin
  p00 := QWord(DWord(x)) * DWord(y);
  p01 := DWord(x) * (y shr 32);
  mid := (x shr 32) * DWord(y) + p00 shr 32 + DWord(p01);
  aProd.Lo := mid shl 32 or DWord(p00);
  aProd.Hi := (x shr 32) * (y shr 32) + mid shr 32 + p01 shr 32;
end;
{$ENDIF CPUX64}

function TryBuildDoubleEiselLemire(aMan: QWord; const aPow10: Int64; aNeg: Boolean; out aValue: Double): Boolean; inline;
var
  ProdLo, ProdHi, Mid, MsBit, Mantissa: QWord;
  Exponent: Int64;
  LzCount: Integer;
  Prod: TOWord;
begin
  if (aPow10 >= -22) and (aPow10 <= 22) and (aMan <= 9007199254740991) then
    begin
      aValue := Double(aMan);
      if aPow10 < 0 then
        aValue /= TEN_POWER[-aPow10]
      else
        aValue *= TEN_POWER[aPow10];
      if aNeg then
        aValue := -aValue;
      exit(True);
    end;

  if aMan = 0 then
    begin
      if aNeg then
        aValue := Double(-0.0)
      else
        aValue := Double(0.0);
      exit(True);
    end;

  Exponent := SarInt64((152170 + 65536) * aPow10, 16) + 1024 + 63;
  LzCount := Pred(BitSizeOf(QWord)) - ShortInt(BsrQWord(aMan));
  aMan := aMan shl LzCount;

  UMul64To128(aMan, EL_MANTIS_64[aPow10], Prod);
  ProdLo := Prod.Lo;
  ProdHi := Prod.Hi;

  if(ProdHi and $1FF = $1FF)and(ProdLo + aMan < ProdLo)then
    begin
      UMul64To128(aMan, EL_MANTIS_128[aPow10], Prod);
      Mid := ProdLo + Prod.Hi;
      ProdHi += Ord(Mid < ProdLo);
      if(Mid + 1 = 0)and(ProdHi and $1FF = $1FF)and(Prod.Lo + aMan < Prod.Lo)then
        exit(False);
      ProdLo := Mid;
    end;

  MsBit := ProdHi shr 63;
  Mantissa := ProdHi shr (MsBit + 9);
  LzCount += Ord(MsBit xor 1);

  if(ProdLo = 0)and(ProdHi and $1FF = 0)and(Mantissa and 3 = 1)then
    exit(False);

  Mantissa := (Mantissa + Mantissa and 1) shr 1;

  if Mantissa >= QWord(1) shl 53 then
    begin
      Mantissa := QWord(1) shl 52;
      Dec(LzCount);
    end;
  Mantissa := Mantissa and not(QWord(1) shl 52);

  Exponent -= LzCount;
  if (Exponent < 1) or (Exponent > 2046) then
    exit(False);

  PQWord(@aValue)^ := Mantissa or QWord(Exponent shl 52) or QWord(aNeg) shl 63;
  Result := True;
end;

function TryPChar2DoubleDef(p: PAnsiChar; out aValue: Double): Boolean;
var
  Code: Integer;
const
  INF_EXP = QWord($7ff0000000000000);
begin
  Val(p, aValue, Code);
  Result := (Code = 0) and (QWord(aValue) and INF_EXP <> INF_EXP);
end;

function TryPChar2DoubleFast(p: PAnsiChar; out aValue: Double): Boolean;
var
  Man: QWord;
  Pow10, PowVal: Int64;
  DigCount: Integer;
  pOld, pDigStart, pTemp: PAnsiChar;
  IsNeg, PowIsNeg: Boolean;
const
  Decimals: array['0'..'9'] of QWord = (0, 1, 2, 3, 4, 5, 6, 7, 8, 9);
begin
  if p^ = #0 then exit(False);
  pOld := p;
  IsNeg := False;
  if p^ = '-' then begin
    Inc(p);
    IsNeg := True;
  end;
  if p^ = '0' then begin
    Man := 0;
    Inc(p);
    pDigStart := p;
  end else begin
    pDigStart := p;
    Man := Decimals[p^];
    Inc(p);
  end;
  while p^ in ['0'..'9'] do begin
    Man := Man * 10 + Decimals[p^];
    Inc(p);
  end;
  Pow10 := 0;
  if p^ = '.' then begin
    Inc(p);
    pTemp := p;
    while p^ in ['0'..'9'] do begin
      Man := Man * 10 + Decimals[p^];
      Inc(p);
    end;
    Pow10 := pTemp - p;
    DigCount := p - pDigStart - 1;
  end else
    DigCount := p - pDigStart;
  if p^ in ['e', 'E'] then begin
    PowIsNeg := False;
    Inc(p);
    if p^ = '-' then begin
      PowIsNeg := True;
      Inc(p);
    end else
      if p^ = '+' then
        Inc(p);
    PowVal := Int64(Decimals[p^]);
    Inc(p);
    while p^ in ['0'..'9'] do begin
      if PowVal < $100000000 then
        PowVal := PowVal * 10 + Decimals[p^];
      Inc(p);
    end;
    if PowIsNeg then Pow10 -= PowVal
    else Pow10 += PowVal;
  end;
  if DigCount >= 19 then begin
    pTemp := pDigStart;
    while pTemp^ in ['0', '.'] do
      Inc(pTemp);
    DigCount -= pTemp - pDigStart;
    if DigCount >= 19 then
      exit(TryPChar2DoubleDef(pOld, aValue));
  end;
  if (Pow10 < ELDBL_LOW_POWER) or (Pow10 > ELDBL_HIGH_POWER) then
    exit(TryPChar2DoubleDef(pOld, aValue));
  if not TryBuildDoubleEiselLemire(Man, Pow10, IsNeg, aValue) then
    exit(TryPChar2DoubleDef(pOld, aValue));
  Result := True;
end;
{$POP}

{$PUSH}{$J-}
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
{$POP}
function DoParseStr(Buf: PAnsiChar; Size: SizeInt; aNode: TJsonNode; const aStack: TOpenArray): Boolean;
var
  Stack: PParseNode;
  I: SizeInt;
  NextState, NextClass, StackHigh: Integer;
  State: Integer = GO;
  sTop: Integer = 1;
  sb: TJsonNode.TStrBuilder;
  KeyValue: string = '';
  function Number: Double; inline;
  begin
    if not TryPChar2DoubleFast(sb.ToPChar, Result) then
      Abort;
  end;
begin
  Stack := aStack.Data;
  StackHigh := Pred(aStack.Size);
  Stack[0].Create(nil, pmNone);
  Stack[1].Create(aNode, pmNone);
  sb := TJsonNode.TStrBuilder.Create(TJsonNode.S_BUILD_INIT_SIZE);
  for I := 0 to Pred(Size) do begin
    if Buf[I] < #128 then begin
      NextClass := SymClassTable[Ord(Buf[I])];
      if NextClass = __ then exit(False);
    end else
      NextClass := Etc;
    NextState := StateTransitions[State, NextClass];
    if NextState = __ then exit(False);
    if NextState < 31 then begin
      if DWord(NextState - ST) < DWord(14) then
        sb.Append(Buf[I]);
      State := NextState;
    end else
      case NextState of
        31: //end object - state = object
          if Stack[sTop].Mode = pmKey then begin
            Dec(sTop);
            State := OK;
          end else exit(False);
        32: //end object
          if Stack[sTop].Mode = pmObject then begin
            if Integer(1 shl State) and NUM_STATES <> 0 then
              Stack[sTop].Node.Add(KeyValue, Number);
            Dec(sTop);
            State := OK;
          end else exit(False);
        33: //end array
          if Stack[sTop].Mode = pmArray then begin
            if Integer(1 shl State) and NUM_STATES <> 0 then
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
  end;
  if Integer(1 shl State) and NUM_STATES <> 0 then begin
    if Stack[sTop].Mode <> pmNone then exit(False);
    Stack[sTop].Node.AsNumber := Number;
    State := OK;
    Dec(sTop);
  end;
  Result := (State = OK) and (sTop = 0) and (Stack[0].Node = nil) and (Stack[0].Mode = pmNone);
end;

const
  AN = Integer(7); //Array Next

{ TJsonWriter }

procedure TJsonWriter.ValueAdding;
begin
  case FStack.PeekItem^ of
    VA: FStack.PeekItem^ := OB;
    AR: FStack.PeekItem^ := AN;
    AN: FStream.WriteBuffer(chComma, SizeOf(chComma));
  else
  end;
end;

procedure TJsonWriter.PairAdding;
begin
  case FStack.PeekItem^ of
    OB: FStream.WriteBuffer(chComma, SizeOf(chComma));
    KE: FStack.PeekItem^ := OB;
  else
  end;
end;

class function TJsonWriter.New(aStream: TStream): TJsonWriter;
begin
  Result := TJsonWriter.Create(aStream);
end;

constructor TJsonWriter.Create(aStream: TStream);
begin
  FStream := TWriteBufStream.Create(aStream, TJsonNode.RW_BUF_SIZE);
  FsBuilder := TJsonNode.TStrBuilder.Create(TJsonNode.S_BUILD_INIT_SIZE);
  FStack.Push(OK);
end;

destructor TJsonWriter.Destroy;
begin
  FStream.Free;
  inherited;
end;

function TJsonWriter.AddNull: TJsonWriter;
begin
  ValueAdding;
  FStream.WriteBuffer(JS_NULL[1], System.Length(JS_NULL));
  Result := Self;
end;

function TJsonWriter.AddFalse: TJsonWriter;
begin
  ValueAdding;
  FStream.WriteBuffer(JS_FALSE[1], System.Length(JS_FALSE));
  Result := Self;
end;

function TJsonWriter.AddTrue: TJsonWriter;
begin
  ValueAdding;
  FStream.WriteBuffer(JS_TRUE[1], System.Length(JS_TRUE));
  Result := Self;
end;

function TJsonWriter.Add(aValue: Double): TJsonWriter;
var
  num: string;
begin
  ValueAdding;
  num := FloatToStr(aValue, TJsonNode.FmtSettings);
  FStream.WriteBuffer(Pointer(num)^, System.Length(num));
  Result := Self;
end;

function TJsonWriter.Add(const s: string): TJsonWriter;
begin
  ValueAdding;
  FsBuilder.AppendEncode(s);
  FsBuilder.SaveToStream(FStream);
  Result := Self;
end;

function TJsonWriter.Add(aValue: TJsonNode): TJsonWriter;
var
  s: string;
begin
  ValueAdding;
  s := aValue.AsJson;
  FStream.WriteBuffer(Pointer(s)^, System.Length(s));
  Result := Self;
end;

function TJsonWriter.AddJson(const aJson: string): TJsonWriter;
begin
  ValueAdding;
  FStream.WriteBuffer(Pointer(aJson)^, System.Length(aJson));
  Result := Self;
end;

function TJsonWriter.AddName(const aName: string): TJsonWriter;
begin
  case FStack.PeekItem^ of
    OB: FStream.WriteBuffer(chComma, SizeOf(chComma));
    KE: FStack.PeekItem^ := VA;
  else
  end;
  FsBuilder.AppendEncode(aName);
  FsBuilder.SaveToStream(FStream);
  FStream.WriteBuffer(chColon, SizeOf(chColon));
  Result := Self;
end;

function TJsonWriter.AddNull(const aName: string): TJsonWriter;
begin
  PairAdding;
  FsBuilder.AppendEncode(aName);
  FsBuilder.SaveToStream(FStream);
  FStream.WriteBuffer(chColon, SizeOf(chColon));
  FStream.WriteBuffer(JS_NULL[1], System.Length(JS_NULL));
  Result := Self;
end;

function TJsonWriter.AddFalse(const aName: string): TJsonWriter;
begin
  PairAdding;
  FsBuilder.AppendEncode(aName);
  FsBuilder.SaveToStream(FStream);
  FStream.WriteBuffer(chColon, SizeOf(chColon));
  FStream.WriteBuffer(JS_FALSE[1], System.Length(JS_FALSE));
  Result := Self;
end;

function TJsonWriter.AddTrue(const aName: string): TJsonWriter;
begin
  PairAdding;
  FsBuilder.AppendEncode(aName);
  FsBuilder.SaveToStream(FStream);
  FStream.WriteBuffer(chColon, SizeOf(chColon));
  FStream.WriteBuffer(JS_TRUE[1], System.Length(JS_TRUE));
  Result := Self;
end;

function TJsonWriter.Add(const aName: string; aValue: Double): TJsonWriter;
var
  num: string;
begin
  PairAdding;
  FsBuilder.AppendEncode(aName);
  FsBuilder.SaveToStream(FStream);
  FStream.WriteBuffer(chColon, SizeOf(chColon));
  num := FloatToStr(aValue, TJsonNode.FmtSettings);
  FStream.WriteBuffer(Pointer(num)^, System.Length(num));
  Result := Self;
end;

function TJsonWriter.Add(const aName, aValue: string): TJsonWriter;
begin
  PairAdding;
  FsBuilder.AppendEncode(aName);
  FsBuilder.SaveToStream(FStream);
  FStream.WriteBuffer(chColon, SizeOf(chColon));
  FsBuilder.AppendEncode(aValue);
  FsBuilder.SaveToStream(FStream);
  Result := Self;
end;

function TJsonWriter.Add(const aName: string; aValue: TJsonNode): TJsonWriter;
var
  s: string;
begin
  PairAdding;
  FsBuilder.AppendEncode(aName);
  FsBuilder.SaveToStream(FStream);
  FStream.WriteBuffer(chColon, SizeOf(chColon));
  s := aValue.AsJson;
  FStream.WriteBuffer(Pointer(s)^, System.Length(s));
  Result := Self;
end;

function TJsonWriter.AddJson(const aName, aJson: string): TJsonWriter;
begin
  PairAdding;
  FsBuilder.AppendEncode(aName);
  FsBuilder.SaveToStream(FStream);
  FStream.WriteBuffer(chColon, SizeOf(chColon));
  FStream.WriteBuffer(Pointer(aJson)^, System.Length(aJson));
  Result := Self;
end;

function TJsonWriter.BeginArray: TJsonWriter;
begin
  case FStack.PeekItem^ of
    VA: FStack.PeekItem^ := OB;
    AR: FStack.PeekItem^ := AN;
    AN: FStream.WriteBuffer(chComma, SizeOf(chComma));
  else
  end;
  FStream.WriteBuffer(chOpenSqrBr, SizeOf(chOpenSqrBr));
  FStack.Push(AR);
  Result := Self;
end;

function TJsonWriter.BeginObject: TJsonWriter;
begin
  case FStack.PeekItem^ of
    VA: FStack.PeekItem^ := OB;
    AR: FStack.PeekItem^ := AN;
    AN: FStream.WriteBuffer(chComma, SizeOf(chComma));
  else
  end;
  FStream.WriteBuffer(chOpenCurBr, SizeOf(chOpenCurBr));
  FStack.Push(KE);
  Result := Self;
end;

function TJsonWriter.EndArray: TJsonWriter;
begin
  FStream.WriteBuffer(chClosSqrBr, SizeOf(chClosSqrBr));
  FStack.Pop;
  Result := Self;
end;

function TJsonWriter.EndObject: TJsonWriter;
begin
  FStream.WriteBuffer(chClosCurBr, SizeOf(chClosCurBr));
  FStack.Pop;
  Result := Self;
end;

{ TJsonReader.TLevel }

constructor TJsonReader.TLevel.Create(aMode: TParseMode);
begin
  Mode := aMode;
  Path := '';
  CurrIndex := 0;
end;

constructor TJsonReader.TLevel.Create(aMode: TParseMode; aPath: string);
begin
  Mode := aMode;
  Path := aPath;
  CurrIndex := 0;
end;

constructor TJsonReader.TLevel.Create(aMode: TParseMode; aIndex: SizeInt);
begin
  Mode := aMode;
  Path := IntToStr(aIndex);
  CurrIndex := 0;
end;

{ TJsonReader }

function TJsonReader.GetIndex: SizeInt;
begin
  Result := FStack[Depth].CurrIndex;
end;

function TJsonReader.GetStructKind: TStructKind;
begin
  case FStack[Depth].Mode of
    pmArray:  Result := skArray;
    pmKey,
    pmObject: Result := skObject;
  else
    Result := skNone;
  end;
end;

function TJsonReader.GetParentKind: TStructKind;
begin
  if Depth > 0 then
    case FStack[Pred(Depth)].Mode of
      pmArray:         exit(skArray);
      pmKey, pmObject: exit(skObject);
    else
    end;
  Result := skNone;
end;

procedure TJsonReader.UpdateArray;
begin
  if FStack[Depth].Mode = pmArray then
    begin
      if ReadMode then
        FName := IntToStr(FStack[Depth].CurrIndex);
      Inc(FStack[Depth].CurrIndex);
    end;
end;

function TJsonReader.NullValue: Boolean;
begin
  if ReadMode then
    FValue.Clear;
  UpdateArray;
  FState := OK;
  FToken := tkNull;
  Result := True;
end;

function TJsonReader.FalseValue: Boolean;
begin
  if ReadMode then
    FValue := False;
  UpdateArray;
  FToken := tkFalse;
  FState := OK;
  Result := True;
end;

function TJsonReader.TrueValue: Boolean;
begin
  if ReadMode then
    FValue := True;
  UpdateArray;
  FState := OK;
  FToken := tkTrue;
  Result := True;
end;

function TJsonReader.NumValue: Boolean;
var
  d: Double;
  e: Integer;
begin
  if ReadMode then
    begin
      if not TryPChar2DoubleFast(FsBuilder.ToPChar, d) then
        exit(False);
      FValue := d;
    end;
  UpdateArray;
  FToken := tkNumber;
  Result := True;
end;

procedure TJsonReader.NameValue;
begin
  if ReadMode then
    FName := FsBuilder.ToDecodeString;
  FState := CO;
end;

function TJsonReader.CommaAfterNum: Boolean;
begin
  if not NumValue then
    exit(False);
  case FStack[Depth].Mode of
    pmArray: FState := VA;
    pmObject:
      begin
        FStack[Depth].Mode := pmKey;
        FState := KE;
      end;
  else
    exit(False);
  end;
  Result := True;
end;

function TJsonReader.StringValue: Boolean;
begin
  if ReadMode then
    FValue := FsBuilder.ToDecodeString;
  UpdateArray;
  FToken := tkString;
  FState := OK;
  Result := True;
end;

function TJsonReader.ArrayBegin: Boolean;
begin
  if Depth = FStackHigh then exit(False);
  case FStack[Depth].Mode of
    pmNone: FStack[Succ(Depth)] := TLevel.Create(pmArray);
    pmArray:
      if ReadMode then
        FStack[Succ(Depth)] := TLevel.Create(pmArray, FStack[Depth].CurrIndex)
      else
        FStack[Succ(Depth)] := TLevel.Create(pmArray);
    pmObject:
      if ReadMode then
        FStack[Succ(Depth)] := TLevel.Create(pmArray, FName)
      else
        FStack[Succ(Depth)] := TLevel.Create(pmArray);
  else
    exit(False);
  end;
  Inc(FStackTop);
  FToken := tkArrayBegin;
  FState := AR;
  Result := True;
end;

function TJsonReader.ObjectBegin: Boolean;
begin
  if Depth = FStackHigh then exit(False);
  case FStack[Depth].Mode of
    pmNone: FStack[Succ(Depth)] := TLevel.Create(pmKey);
    pmArray:
      if ReadMode then
        FStack[Succ(Depth)] := TLevel.Create(pmKey, FStack[Depth].CurrIndex)
      else
        FStack[Succ(Depth)] := TLevel.Create(pmKey);
    pmObject:
      if ReadMode then
        FStack[Succ(Depth)] := TLevel.Create(pmKey, FName)
      else
        FStack[Succ(Depth)] := TLevel.Create(pmKey);
  else
    exit(False);
  end;
  Inc(FStackTop);
  FToken := tkObjectBegin;
  FState := OB;
  Result := True;
end;

function TJsonReader.ArrayEnd: Boolean;
begin
  if FStack[Depth].Mode <> pmArray then
    exit(False);
  Dec(FStackTop);
  FToken := tkArrayEnd;
  UpdateArray;
  FState := OK;
  Result := True;
end;

function TJsonReader.ArrayEndAfterNum: Boolean;
begin
  if FStack[Depth].Mode <> pmArray then
    exit(False);
  if not NumValue then
    exit(False);
  FDeferToken := tkArrayEnd;
  FState := OK;
  Result := True;
end;

function TJsonReader.ObjectEnd: Boolean;
begin
  if FStack[Depth].Mode <> pmObject then
    exit(False);
  Dec(FStackTop);
  FToken := tkObjectEnd;
  UpdateArray;
  FState := OK;
  Result := True;
end;

function TJsonReader.ObjectEndAfterNum: Boolean;
begin
  if FStack[Depth].Mode <> pmObject then
    exit(False);
  if not NumValue then
    exit(False);
  FDeferToken := tkObjectEnd;
  FState := OK;
  Result := True;
end;

function TJsonReader.ObjectEndOb: Boolean;
begin
  if FStack[Depth].Mode <> pmKey then
    exit(False);
  FToken := tkObjectEnd;
  Dec(FStackTop);
  UpdateArray;
  FState := OK;
  Result := True;
end;

function TJsonReader.DeferredEnd: Boolean;
begin
  case DeferToken of
    tkArrayEnd:  Result := ArrayEnd;
    tkObjectEnd: Result := ObjectEnd;
  else
    Result := False;
  end;
  FDeferToken := tkNone;
end;

function TJsonReader.GetNextChunk: TReadState;
begin
  if ReadState > rsGo then
    exit(ReadState);
  FPosition := NULL_INDEX;
  FByteCount := FStream.Read(FBuffer, SizeOf(FBuffer));
  if FByteCount = 0 then
    begin
      FReadState := rsEOF;
      exit(ReadState);
    end;
  if FFirstChunk then
    begin
      FFirstChunk := False;
      if SkipBom then
        case DetectBom(@FBuffer, FByteCount) of
          bkNone: ;
          bkUtf8: FPosition += UTF8_BOM_LEN;
        else
          FReadState := rsError;
          exit(ReadState);
        end;
      FReadState := rsGo;
    end;
  Result := ReadState;
end;

function TJsonReader.GetNextToken: Boolean;
var
  NextState, NextClass: Integer;
  c: AnsiChar;
begin
  repeat
    if DeferToken <> tkNone then exit(DeferredEnd);
    if (FPosition >= Pred(FByteCount)) and (GetNextChunk > rsGo) then
      exit(False);
    Inc(FPosition);
    c := PChar(@FBuffer)[FPosition];
    if c < #128 then begin
      NextClass := SymClassTable[Ord(c)];
      if NextClass = __ then exit(False);
    end else
      NextClass := Etc;
    NextState := StateTransitions[FState, NextClass];
    if NextState = __ then exit(False);
    if CopyMode then FsbHelp.Append(c); //////////
    if NextState < 31 then begin
      if (DWord(NextState - ST) < DWord(14)) and ReadMode then
        FsBuilder.Append(c);
      FState := NextState;
    end else
    case NextState of
      31: exit(ObjectEndOb);  //end object when state = OB
      32:                     //end object when state = OK or in [ZE, IR, FS, E3]
        if Integer(1 shl FState) and NUM_STATES = 0 then exit(ObjectEnd)
        else exit(ObjectEndAfterNum);

      33:                     //end array when state = OK or in [ZE, IR, FS, E3]
        if Integer(1 shl FState) and NUM_STATES = 0 then exit(ArrayEnd)
        else exit(ArrayEndAfterNum);
      34: exit(ObjectBegin);  //begin object
      35: exit(ArrayBegin);   //begin array
      36:                     //string value
        begin
          if ReadMode then
            FsBuilder.Append(c);
          if FStack[Depth].Mode = pmKey then NameValue
          else exit(StringValue);
        end;
      37:                     //OK - comma
        case FStack[Depth].Mode of
          pmObject: begin
              FStack[Depth].Mode := pmKey;
              FState := KE;
            end;
          pmArray: FState := VA;
        else exit(False);
        end;
      38:                     //colon
        if FStack[Depth].Mode = pmKey then begin
          FStack[Depth].Mode := pmObject;
          FState := VA;
        end else exit(False);
      39: exit(CommaAfterNum);//end number - comma
      40: begin               //end number - white space
        FState := OK;
        exit(NumValue);
      end;
      41: exit(TrueValue);    //true literal
      42: exit(FalseValue);   //false literal
      43: exit(NullValue);    //null literal
    else exit(False);
    end;
  until False;
end;

function TJsonReader.GetIsNull: Boolean;
begin
  Result := FValue.Kind = vkNull;
end;

function TJsonReader.GetAsBoolean: Boolean;
begin
  Result := FValue;
end;

function TJsonReader.GetAsNumber: Double;
begin
  Result := FValue;
end;

function TJsonReader.GetAsString: string;
begin
  Result := FValue;
end;

function TJsonReader.GetPath: string;
  procedure Convert(const s: string);
  var
    I: SizeInt;
  begin
    for I := 1 to System.Length(s) do
      case s[I] of
        '/':
          begin
            FsbHelp.Append('~');
            FsbHelp.Append('1');
          end;
        '~':
          begin
            FsbHelp.Append('~');
            FsbHelp.Append('0');
          end;
      else
        FsbHelp.Append(s[I]);
      end;
  end;
var
  I: SizeInt;
begin
  if FStackTop = 0 then
    exit('');
  for I := 2 to FStackTop do
    begin
      FsbHelp.Append('/');
      Convert(FStack[I].Path);
    end;
  if TokenKind in [tkNull, tkFalse, tkTrue, tkNumber, tkString] then
    begin
      FsbHelp.Append('/');
      Convert(Name);
    end;
  Result := FsbHelp.ToString;
end;

function TJsonReader.GetParentName: string;
begin
  Result := FStack[Depth].Path;
end;

class function TJsonReader.IsStartToken(aToken: TTokenKind): Boolean;
begin
  Result := aToken in [tkArrayBegin, tkObjectBegin];
end;

class function TJsonReader.IsEndToken(aToken: TTokenKind): Boolean;
begin
  Result := aToken in [tkArrayEnd, tkObjectEnd];
end;

class function TJsonReader.IsScalarToken(aToken: TTokenKind): Boolean;
begin
  Result := aToken in [tkNull, tkFalse, tkTrue, tkNumber, tkString];
end;

constructor TJsonReader.Create(aStream: TStream; aMaxDepth: SizeInt; aSkipBom: Boolean);
begin
  FStream := aStream;
  if aMaxDepth < 31 then
    aMaxDepth := 31;
  System.SetLength(FStack, Succ(aMaxDepth));
  FStackHigh := aMaxDepth;
  FSkipBom := aSkipBom;
  FReadMode := True;
  FFirstChunk := True;
  FsBuilder := TJsonNode.TStrBuilder.Create(TJsonNode.S_BUILD_INIT_SIZE);
  FsbHelp := TJsonNode.TStrBuilder.Create(TJsonNode.S_BUILD_INIT_SIZE);
  FStack[0] := TLevel.Create(pmNone);
end;

function TJsonReader.Read: Boolean;
begin
  if ReadState > rsGo then
    exit(False);
  Result := GetNextToken;
  if not Result then
    if ReadState = rsEOF then
      begin
        if Depth <> 0 then
          begin
            FReadState := rsError;
            exit;
          end;
        if Integer(1 shl FState) and NUM_STATES <> 0 then
          begin
            FState := OK;
            if NumValue then
              exit(True)
            else
              FReadState := rsError;
          end;
        if FState <> OK then
          FReadState := rsError;
      end
    else
      FReadState := rsError;
end;

procedure TJsonReader.Skip;
var
  OldDepth: SizeInt;
begin
  if ReadState > rsGo then exit;
  if IsStartToken(TokenKind) then
    begin
      OldDepth := Pred(Depth);
      FReadMode := False;
      try
        while Read and (Depth > OldDepth) do;
      finally
        FReadMode := True;
      end;
    end
  else
    Read;
end;

procedure TJsonReader.Iterate(aFun: TIterateFun);
var
  OldDepth: SizeInt;
begin
  if aFun = nil then exit;
  OldDepth := Depth;
  while Read do
    case TokenKind of
      tkNone,
      tkArrayBegin,
      tkObjectBegin: ;
      tkArrayEnd,
      tkObjectEnd:
        if Depth < OldDepth then
          break;
    else
      if not aFun(Self) then
        exit;
    end;
end;

procedure TJsonReader.Iterate(aFun: TNestIterate);
var
  OldDepth: SizeInt;
begin
  if aFun = nil then exit;
  OldDepth := Depth;
  while Read do
    case TokenKind of
      tkNone,
      tkArrayBegin,
      tkObjectBegin: ;
      tkArrayEnd,
      tkObjectEnd:
        if Depth < OldDepth then
          break;
    else
      if not aFun(Self) then
        exit;
    end;
end;

procedure TJsonReader.Iterate(aOnStruct, aOnValue: TIterateFun);
var
  OldDepth: SizeInt;
begin
  OldDepth := Depth;
  while Read do
    case TokenKind of
      tkNone,
      tkArrayEnd,
      tkObjectEnd:
        if Depth < OldDepth then
          break;
      tkArrayBegin,
      tkObjectBegin:
        if (aOnStruct <> nil) and not aOnStruct(Self) then
          exit;
    else
      if (aOnValue <> nil) and not aOnValue(Self) then
        exit;
    end;
end;

procedure TJsonReader.Iterate(aOnStruct, aOnValue: TNestIterate);
var
  OldDepth: SizeInt;
begin
  OldDepth := Depth;
  while Read do
    case TokenKind of
      tkNone,
      tkArrayEnd,
      tkObjectEnd:
        if Depth < OldDepth then
          break;
      tkArrayBegin,
      tkObjectBegin:
        if (aOnStruct <> nil) and not aOnStruct(Self) then
          exit;
    else
      if (aOnValue <> nil) and not aOnValue(Self) then
        exit;
    end;
end;

function TJsonReader.CopyStruct(out aStruct: string): Boolean;
begin
  if ReadState > rsGo then
    exit(False);
  if not IsStartToken(TokenKind) then
    exit(False);
  FsbHelp.MakeEmpty;
  if TokenKind = tkArrayBegin then
    FsbHelp.Append(chOpenSqrBr)
  else
    FsbHelp.Append(chOpenCurBr);
  FCopyMode := True;
  try
    Skip;
  finally
    FCopyMode := False;
  end;
  aStruct := FsbHelp.ToString;
  Result := True;
end;

function TJsonReader.MoveNext: Boolean;
begin
  if ReadState > rsGo then
    exit(False);
  if not Read then
    exit(False);
  if IsEndToken(TokenKind) then
    exit(False);
  if IsStartToken(TokenKind) then
    Skip;
  Result := True;
end;

function TJsonReader.Find(const aKey: string): Boolean;
var
  Idx, OldDepth: SizeInt;
begin
  if ReadState > rsGo then
    exit(False);
  case StructKind of
    skArray:
      begin
        if not IsNonNegativeInteger(aKey, Idx) then
          exit(False);
        if Idx < Index then
          exit(False);
        OldDepth := Depth;
        while (FStack[OldDepth].CurrIndex < Idx) and MoveNext do;
        Result := (FStack[OldDepth].CurrIndex = Idx) and Read;
      end;
    skObject:
      begin
        if TokenKind = tkObjectBegin then
          Read;
        repeat
          if Name = aKey then exit(True);
          if IsStartToken(TokenKind) then
            Skip;
        until not Read or (TokenKind = tkObjectEnd);
        Result := False;
      end;
  else
    exit(False);
  end;
end;

function TJsonReader.FindPath(const aPtr: TJsonPtr): Boolean;
begin
  Result := FindPath(aPtr.ToSegments);
end;

function TJsonReader.FindPath(const aPath: TStringArray): Boolean;
var
  I: SizeInt;
begin
  if ReadState <> rsStart then
    exit(False);
  if not Read then
    exit(False);
  if aPath = nil then
    exit(True);
  for I := 0 to System.High(aPath) do
    if not Find(aPath[I]) then
      exit(False);
  Result := True;
end;

end.
