{****************************************************************************
*                                                                           *
*   This file is part of the LGenerics package.                             *
*   MessagePack utilites (https://msgpack.org).                             *
*                                                                           *
*   Copyright(c) 2026 A.Koverdyaev(avk)                                     *
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
unit LgMsgPack;

{$MODE OBJFPC}{$H+}
{$MODESWITCH TYPEHELPERS}
{$MODESWITCH ADVANCEDRECORDS}
{$MODESWITCH NESTEDPROCVARS}
{$INLINE ON}

interface

uses
  Classes, SysUtils, DateUtils, Math,
  LgUtils,
  LgHelpers,
  LgArrayHelpers,
  LgVector,
  LgList,
  lgJson;

type
  EMpDomError = class(Exception);
  EMpWrite    = class(Exception);
  TBytes      = SysUtils.TBytes;

  { TMpTimeStamp }
  TMpTimeStamp = record
  private
    FSeconds: Int64;
    FNanoSeconds: DWord;
  const
    NSEC_PER_MSEC = 1000000;
    NSEC_PER_SEC  = 1000000000;
    UNIX_MSEC_OFS = Int64(62135596800000);
    UNIX_MSEC_MAX = Int64(253402300799999);
  public
    class operator = (const L, R: TMpTimeStamp): Boolean; inline;
    class function Make(aSec: Int64; aNSec: DWord): TMpTimeStamp; static;
    class function Make(aValue: TDateTime): TMpTimeStamp; static;
    class function TryTsToDateTime(const aTs: TMpTimeStamp; out aValue: TDateTime): Boolean; static;
    function ToDateTime: TDateTime;
    function AsJson: string;
    property Seconds: Int64 read FSeconds;
    property NanoSeconds: DWord read FNanoSeconds;
    property AsUnixTime: Int64 read FSeconds;
  end;

  TMpVarKind = (mvkNull, mvkInt, mvkStr, mvkBin);

  { TMpVariant }
  TMpVariant = record
  private
  type
    TValue = record
    case Boolean of
      False: (Int: Int64);
      True:  (Ref: Pointer);
    end;
  var
    FValue: TValue;
    FKind: TMpVarKind;
    procedure DoClear; inline;
    class operator  Initialize(var v: TMpVariant); inline;
    class operator  Finalize(var v: TMpVariant); inline;
    class operator  Copy(constref aSrc: TMpVariant; var aDst: TMpVariant);
    class function  EqualBytes(const L, R: TBytes): Boolean; static; inline;
    class function  BytesHash(const b: TBytes): SizeInt; static; inline;
    class procedure CastError; static;
  public
    class operator := (aValue: Byte): TMpVariant; inline;
    class operator := (aValue: ShortInt): TMpVariant; inline;
    class operator := (aValue: Word): TMpVariant; inline;
    class operator := (aValue: SmallInt): TMpVariant; inline;
    class operator := (aValue: UInt32): TMpVariant; inline;
    class operator := (aValue: Int32): TMpVariant; inline;
    class operator := (aValue: Int64): TMpVariant; inline;
    class operator := (const aValue: string): TMpVariant; inline;
    class operator := (const aValue: TBytes): TMpVariant; inline;
    class operator = (const L, R: TMpVariant): Boolean;
    class function HashCode(const aValue: TMpVariant): SizeInt; static; inline;
    class function Equal(const L, R: TMpVariant): Boolean; static; inline;
    procedure Clear; inline;
    function AsInt: Int64; inline;
    function AsString: string; inline;
    function AsBinary: TBytes; inline;
    function ToString: string;
    property Kind: TMpVarKind read FKind;
  end;
  TMpVarArray = array of TMpVariant;

  TMpExtType = type Int8;   //MsgPack extention type
  TMpExtBlob = type TBytes; //first byte contains extention type;

  TMpNodeKind = (
    mnkNil, mnkBool, mnkInt, mnkSingle, mnkDouble, mnkString, mnkBin, mnkArray, mnkMap,
    mnkTStamp, mnkExt
  );

  TMpDuplicates = (mduAccept, mduRewrite, mduIgnore);

  { TMpDomNode: represents the DOM tree of the MsgPack document;
    each node is responsible for freeing the memory of its child nodes,
    so the user only needs to explicitly free the root node }
  TMpDomNode = class
  private
  type
    TMpArray   = specialize TGLiteSimpleObjList<TMpDomNode>;
    PMpArray   = ^TMpArray;
    TMpMap     = specialize TGLiteObjHashList<TMpVariant, TMpDomNode, TMpVariant>;
    PMpMap     = ^TMpMap;
    PMpDomNode = ^TMpDomNode;
  public
  type
    TPair = TMpMap.TPair;

    TEnumerator = record
    private
      FCurrNode,
      FLastNode: Pointer;
      FKind: TMpNodeKind;
      function GetCurrent: TMpDomNode; inline;
    public
      function MoveNext: Boolean; inline;
      property Current: TMpDomNode read GetCurrent;
    end;

    TPairEnumerator = record
    private
      FCurrNode,
      FLastNode: TMpMap.PNode;
      function GetCurrent: TPair; inline;
    public
      function GetEnumerator: TPairEnumerator; inline;
      function MoveNext: Boolean; inline;
      property Current: TPair read GetCurrent;
    end;

    TEqualKeys = record
    private
      FEnum: TMpMap.TEqualEnumerator;
      function  GetCurrent: TPair; inline;
      procedure Init(const aEnum: TMpMap.TEqualEnumerator);
    public
      function  GetEnumerator: TEqualKeys; inline;
      function  MoveNext: Boolean; inline;
      property  Current: TPair read GetCurrent;
    end;

  private
  type
    TMpValue = record
    case Integer of
      0: (Int: Int64);
      1: (Dbl: Double);
      2: (Ref: Pointer);
      3: (Flt: Single);
      4: (Bool: Boolean);
    end;
  var
    FValue: TMpValue;
    FNSec: DWord;
    FKind: TMpNodeKind;
    function  GetArrayRef: PMpArray; inline;
    function  GetMapRef: PMpMap; inline;
    procedure DoClear; inline;
    function  GetAsBool: Boolean;
    function  GetAsInt: Int64;
    function  GetAsSingle: Single;
    function  GetAsDouble: Double;
    function  GetAsString: string;
    function  GetAsBytes: TBytes;
    function  GetAsTStamp: TMpTimeStamp;
    function  GetAsExt: TMpExtBlob;
    function  GetAsArray: TMpDomNode;
    function  GetAsMap: TMpDomNode;
    function  GetAsMsgPack: TBytes;
    function  GetMsgPackStr: rawbytestring;
    function  GetAsJson: string;
    function  GetCount: SizeInt; inline;
    function  GetItem(aIndex: SizeInt): TMpDomNode;
    function  GetPair(aIndex: SizeInt): TPair;
    function  GetMapProp(const aKey: TMpVariant): TMpDomNode;
    procedure SetNil; inline;
    procedure SetAsBool(aValue: Boolean); inline;
    procedure SetAsInt(aValue: Int64); inline;
    procedure SetAsSingle(aValue: Single); inline;
    procedure SetAsDouble(aValue: Double); inline;
    procedure SetAsString(const s: string); inline;
    procedure SetAsBytes(const b: TBytes); inline;
    procedure SetAsTStamp(const ts: TMpTimeStamp); inline;
    procedure SetAsExt(const e: TMpExtBlob); inline;
    procedure SetAsMsgPack(const b: TBytes);
    procedure SetAsJson(const s: string);
    procedure SetMsgPackStr(const rbs: rawbytestring);
    class procedure MoveNode(aSrc, aDst: TMpDomNode); static;
  public
  const
    DEF_DEPTH = 512;
  {  }
    class function MakeNil: TMpDomNode; static;
    class function Make(aValue: Boolean): TMpDomNode; static;
    class function MakeI(aValue: Int64): TMpDomNode; static;
    class function MakeS(aValue: Single): TMpDomNode; static;
    class function MakeD(aValue: Double): TMpDomNode; static;
    class function Make(const s: string): TMpDomNode; static;
    class function Make(const a: array of Byte): TMpDomNode;
    class function MakeB(const b: TBytes): TMpDomNode; static;
    class function Make(const ts: TMpTimeStamp): TMpDomNode; static;
  { returns True and the created node in the aRoot parameter if parsing is successful,
    otherwise returns False and nil }
    class function TryParse(aBuffer: PByte; aCount: SizeInt; out aRoot: TMpDomNode;
                            aDuplicates: TMpDuplicates = mduAccept;
                            aMaxDepth: SizeInt = DEF_DEPTH): Boolean; static;
    class function TryParse(const s: rawbytestring; out aRoot: TMpDomNode;
                            aDuplicates: TMpDuplicates = mduAccept;
                            aMaxDepth: SizeInt = DEF_DEPTH): Boolean; static;
    class function TryParse(const aBytes: TBytes; out aRoot: TMpDomNode;
                            aDuplicates: TMpDuplicates = mduAccept;
                            aMaxDepth: SizeInt = DEF_DEPTH): Boolean; static;
    class function TryParse(aStream: TStream; out aRoot: TMpDomNode; aCount: SizeInt = 0;
                            aDuplicates: TMpDuplicates = mduAccept;
                            aMaxDepth: SizeInt = DEF_DEPTH): Boolean; static;
  { note: the responsibility for the existence and readability of the file lies with the user }
    class function TryParseFile(const aFileName: string; out aRoot: TMpDomNode;
                                aDuplicates: TMpDuplicates = mduAccept;
                                aMaxDepth: SizeInt = DEF_DEPTH): Boolean; static;
  { tries to parse a JSON document using TJsonReader; exact integer values are treated as Integer }
    class function TryLoadJson(const aJson: string; out aRoot: TMpDomNode;
                               aDuplicates: TMpDuplicates = mduAccept;
                               aMaxDepth: SizeInt = DEF_DEPTH): Boolean;
    destructor Destroy; override;
    function GetEnumerator: TEnumerator;
    function PairEnumerator: TPairEnumerator;
    function EqualKeys(const aKey: TMpVariant): TEqualKeys;
  { sets instance to Nil }
    procedure Clear; inline;
    procedure EnsureCapacity(aValue: SizeInt);
  { if parsing fails, the contents of the instance are not changed }
    function TryParse(aBuf: PByte; aCount: SizeInt; aDuplicates: TMpDuplicates = mduAccept;
                      aMaxDepth: SizeInt = DEF_DEPTH): Boolean;
    function TryParse(const s: rawbytestring; aDuplicates: TMpDuplicates = mduAccept;
                      aMaxDepth: SizeInt = DEF_DEPTH): Boolean;
    function TryParse(const aBytes: TBytes; aDuplicates: TMpDuplicates = mduAccept;
                      aMaxDepth: SizeInt = DEF_DEPTH): Boolean;
    function TryParse(aStream: TStream; aCount: SizeInt = 0; aDuplicates: TMpDuplicates = mduAccept;
                      aMaxDepth: SizeInt = DEF_DEPTH): Boolean;
    function TryParseFile(const aFileName: string; aDuplicates: TMpDuplicates = mduAccept;
                          aMaxDepth: SizeInt = DEF_DEPTH): Boolean;
    function TryLoadJson(const aJson: string; aDuplicates: TMpDuplicates = mduAccept;
                         aMaxDepth: SizeInt = DEF_DEPTH): Boolean;
    procedure CopyFrom(aNode: TMpDomNode);
    procedure CopyFrom(aNode: TJsonNode);
    function Clone: TMpDomNode;
    function IsNil: Boolean; inline;
    function IsBoolean: Boolean; inline;
    function IsInteger: Boolean; inline;
    function IsSingle: Boolean; inline;
    function IsDouble: Boolean; inline;
    function IsFloat: Boolean; inline;
    function IsString: Boolean; inline;
    function IsBinary: Boolean; inline;
    function IsScalar: Boolean; inline;
    function IsArray: Boolean; inline;
    function IsMap: Boolean; inline;
    function IsStruct: Boolean; inline;
  { sets the instance to Nil and returns it as a result }
    function AsNil: TMpDomNode;
  { treats the instance as an array; adds the specified value to the instance and returns Self }
    function AddNil: TMpDomNode; inline;
    function Add(aValue: Boolean): TMpDomNode; inline;
    function AddI(aValue: Int64): TMpDomNode; inline;
    function AddS(aValue: Single): TMpDomNode; inline;
    function AddD(aValue: Double): TMpDomNode; inline;
    function Add(const s: string): TMpDomNode; inline;
    function Add(const a: array of Byte): TMpDomNode;
    function AddB(const b: TBytes): TMpDomNode; inline;
    function Add(const ts: TMpTimeStamp): TMpDomNode; inline;
  { treats the instance as an array; adds new node to the instance and returns it as a result }
    function AddNode: TMpDomNode;
  { treats the instance as an array; attempts to insert the specified value at the specified
    position; returns the actual insertion position }
    function InsertNil(aIndex: SizeInt): SizeInt; inline;
    function Insert(aIndex: SizeInt; aValue: Boolean): SizeInt; inline;
    function InsertI(aIndex: SizeInt; aValue: Int64): SizeInt; inline;
    function InsertS(aIndex: SizeInt; aValue: Single): SizeInt; inline;
    function InsertD(aIndex: SizeInt; aValue: Double): SizeInt; inline;
    function Insert(aIndex: SizeInt; const s: string): SizeInt; inline;
    function Insert(aIndex: SizeInt; const a: array of Byte): SizeInt;
    function InsertB(aIndex: SizeInt; const b: TBytes): SizeInt; inline;
    function Insert(aIndex: SizeInt; const ts: TMpTimeStamp): SizeInt; inline;
  { removes a node and returns True if a node with the specified index exists,
    otherwise returns False }
    function Delete(aIndex: SizeInt): Boolean;
    function Extract(aIndex: SizeInt; out aNode: TMpDomNode): Boolean;
  { treats the instance as a map; adds the specified value associated with
    the specified key to the instance and returns Self }
    function AddNil(const aKey: TMpVariant): TMpDomNode;
    function Add(const aKey: TMpVariant; aValue: Boolean): TMpDomNode;
    function AddI(const aKey: TMpVariant; aValue: Int64): TMpDomNode;
    function AddS(const aKey: TMpVariant; aValue: Single): TMpDomNode;
    function AddD(const aKey: TMpVariant; aValue: Double): TMpDomNode;
    function Add(const aKey: TMpVariant; const s: string): TMpDomNode;
    function Add(const aKey: TMpVariant; const a: array of Byte): TMpDomNode;
    function AddB(const aKey: TMpVariant; const b: TBytes): TMpDomNode;
    function Add(const aKey: TMpVariant; const ts: TMpTimeStamp): TMpDomNode;
  { treats the instance as a map; adds new node associated with the specified key
    to the instance and returns it as a result }
    function AddNode(const aKey: TMpVariant): TMpDomNode;
  { treats the instance as a map; adds the specified value associated with the
    specified key to the instance and returns True if the map does not contain
    such a key, otherwise returns False }
    function TryAddNil(const aKey: TMpVariant): Boolean;
    function TryAdd(const aKey: TMpVariant; aValue: Boolean): Boolean;
    function TryAddI(const aKey: TMpVariant; aValue: Int64): Boolean;
    function TryAddS(const aKey: TMpVariant; aValue: Single): Boolean;
    function TryAddD(const aKey: TMpVariant; aValue: Double): Boolean;
    function TryAdd(const aKey: TMpVariant; const s: string): Boolean;
    function TryAdd(const aKey: TMpVariant; const a: array of Byte): Boolean;
    function TryAddB(const aKey: TMpVariant; const b: TBytes): Boolean;
    function TryAdd(const aKey: TMpVariant; const ts: TMpTimeStamp): Boolean;
  { treats the instance as a map; adds a new node to the instance associated with
    the specified key, returning True and that node in the aNode parameter if the map
    does not contain such a key, otherwise returning False }
    function TryAddNode(const aKey: TMpVariant; out aNode: TMpDomNode): Boolean;
    function Contains(const aKey: TMpVariant): Boolean;
    function ContainsUniq(const aKey: TMpVariant): Boolean;
    function IndexOf(const aKey: TMpVariant): SizeInt;
    function CountOf(const aKey: TMpVariant): SizeInt;
    function Find(aIndex: SizeInt; out aValue: TMpDomNode): Boolean;
    function Find(const aKey: TMpVariant; out aValue: TMpDomNode): Boolean;
    function FindUniq(const aKey: TMpVariant; out aValue: TMpDomNode): Boolean;
    function FindPair(aIndex: SizeInt; out aValue: TPair): Boolean;
    function FindOrAdd(const aKey: TMpVariant; out aValue: TMpDomNode): Boolean;
  { tries to find an element using a path specified as an array of path segments;
    if non-unique keys are encountered in the search path, the search terminates
    immediately and returns False }
    function FindPath(const aPath: array of TMpVariant; out aNode: TMpDomNode): Boolean;
    function FindPath(const aPath: array of string; out aNode: TMpDomNode): Boolean;
  { tries to find an element using a path specified as a Pascal string containing a JSON pointer }
    function FindPathPtr(const aPtr: string; out aNode: TMpDomNode): Boolean;
  {  }
    function Extract(const aKey: TMpVariant; out aNode: TMpDomNode): Boolean;
    function Remove(const aKey: TMpVariant): Boolean;
    function RemoveAll(const aKey: TMpVariant): SizeInt;
  { sorts the instance using the specified comparator and returns True
    if the instance is an array, otherwise returns False }
    function Sort(aCompare: specialize TGLessCompare<TMpDomNode>): Boolean;
    function Sort(aCompare: specialize TGOnLessCompare<TMpDomNode>): Boolean;
    function Sort(aCompare: specialize TGNestLessCompare<TMpDomNode>): Boolean;
  { sorts the instance using the specified comparator and returns True
    if the instance is a map, otherwise returns False }
    function Sort(aCompare: specialize TGLessCompare<TPair>): Boolean;
    function Sort(aCompare: specialize TGOnLessCompare<TPair>): Boolean;
    function Sort(aCompare: specialize TGNestLessCompare<TPair>): Boolean;
  { saves he contents of the instance to a stream; returns the number of bytes written }
    function SaveToStream(aStream: TStream): SizeInt;
    function SaveToFile(const aFileName: string): SizeInt;
    property Kind: TMpNodeKind read FKind;
    property AsBoolean: Boolean read GetAsBool write SetAsBool;
    property AsInteger: Int64 read GetAsInt write SetAsInt;
    property AsSingle: Single read GetAsSingle write SetAsSingle;
    property AsDouble: Double read GetAsDouble write SetAsDouble;
    property AsString: string read GetAsString write SetAsString;
    property AsBinary: TBytes read GetAsBytes write SetAsBytes;
    property AsTimeStamp: TMpTimeStamp read GetAsTStamp write SetAsTStamp;
    property AsArray: TMpDomNode read GetAsArray;
    property AsMap: TMpDomNode read GetAsMap;
    property AsExtention: TMpExtBlob read GetAsExt write SetAsExt;
    property AsMsgPack: TBytes read GetAsMsgPack write SetAsMsgPack;
    property AsMsgPackStr: rawbytestring read GetMsgPackStr write SetMsgPackStr;
    property AsJson: string read GetAsJson write SetAsJson;
    property Count: SizeInt read GetCount;
    property Items[aIndex: SizeInt]: TMpDomNode read GetItem;
    property Pairs[aIndex: SizeInt]: TPair read GetPair;
  { acts as FindOrAdd }
    property Values[const aKey: TMpVariant]: TMpDomNode read GetMapProp; default;
  end;

  { TMpWriter }
  TMpWriter = class
  private
  type
    TBuffer = specialize TGDynArray<Byte>;
  const
    INIT_BUF_LEN = 4096;
  var
    FBuffer: TBuffer;
    FCount: SizeInt;
    procedure EnsureCapacity(aValue: SizeInt); inline;
    procedure DoWrite(b: Byte);
    procedure DoWrite2(b1, b2: Byte);
    procedure DoWrite3(b1, b2, b3: Byte);
    procedure DoWrite(w: Word);
    procedure DoWrite(d: DWord);
    procedure DoWrite(q: QWord);
    procedure DoWrite(p: PByte; aCount: SizeInt);
    procedure DoError(const aMsg: string);
    procedure DoError(const aFmt: string; const aParams: array of const);
    class procedure WriteDom(aNode: TMpDomNode; aWriter: TMpWriter); static;
  public
    class function DomToRawStr(aNode: TMpDomNode): rawbytestring; static;
    class function DomToBytes(aNode: TMpDomNode): TBytes; static;
    constructor Create;
    function AddNil: TMpWriter;
    function Add(aValue: Boolean): TMpWriter;
    function Add(aValue: Int64): TMpWriter;
    function Add(aValue: Single): TMpWriter;
    function Add(aValue: Double): TMpWriter;
    function Add(const s: string): TMpWriter;
    function Add(const s: shortstring): TMpWriter;
    function Add(const a: array of Byte): TMpWriter;
    function Add(const ts: TMpTimeStamp): TMpWriter;
    function Add(const v: TMpVariant): TMpWriter;
    function AddExt(aType: TMpExtType; const aBytes: array of Byte): TMpWriter;
    function AddExt(aType: TMpExtType; const aBuffer; aCount: SizeInt): TMpWriter;
    function BeginArray(aCount: SizeInt): TMpWriter;
    function BeginMap(aCount: SizeInt): TMpWriter;
  { resets the counter of bytes written }
    procedure Reset; inline;
    function ToStrRaw: rawbytestring;
    function ToBytes: TBytes;
    function SaveToStream(aStream: TStream): SizeInt;
  { a counter of bytes written }
    property TotalBytes: SizeInt read FCount;
  end;

  TMpStructKind  = (mskNone, mskArray, mskMap);

  TMpReaderState = (mrsStart, mrsGo, mrsEof, mrsError);

  TMpTokenKind = (
    mtkNone, mtkArrayBegin, mtkMapBegin, mtkArrayEnd, mtkMapEnd, mtkNil, mtkBool,
    mtkInt, mtkSingle, mtkDouble, mtkString, mtkBin, mtkTStamp, mtkExt
  );

  { TMpReader }
  TMpReader = class
  private
  type
    TNode = record
      Count: Int64;
      Struct: TMpStructKind;
      constructor Make(aCount: Int64; aStruct: TMpStructKind);
    end;
  const
    START_TOKENS = [mtkArrayBegin, mtkMapBegin];
    END_TOKENS   = [mtkArrayEnd, mtkMapEnd];
  var
    FStack: array of TNode;
    FKey: TMpVariant;
    FTStamp: TMpTimeStamp;
    FString: string;
    FBytes: TBytes;
    FBuffer: PByte;
    FBufSize,
    FBufPos: SizeInt;
    FInt: Int64;
    FDouble: Double;
    FSingle: Single;
    FStackTop: Integer;
    FBool: Boolean;
    FState: TMpReaderState;
    FCurrToken: TMpTokenKind;
    FReadMode: Boolean;
    function  GetStruct: TMpStructKind; inline;
    function  TryPushStack(aCount: Int64; aStruct: TMpStructKind): Boolean;
    procedure TermDone; inline;
    procedure PopStack;
    function  GetMaxDepth: SizeInt; inline;
    function  DoReadByte(out b: Byte): Boolean; inline;
    function  DoSkipByte: Boolean; inline;
    function  DoReadWord(out w: Word): Boolean; inline;
    function  DoSkipWord: Boolean; inline;
    function  DoReadDWord(out d: DWord): Boolean; inline;
    function  DoSkipDWord: Boolean; inline;
    function  DoReadQWord(out q: QWord): Boolean; inline;
    function  DoSkipQWord: Boolean; inline;
    function  DoReadSingle(out s: Single): Boolean; inline;
    function  DoSkipSingle: Boolean; inline;
    function  DoReadDouble(out d: Double): Boolean; inline;
    function  DoSkipDouble: Boolean; inline;
    function  DoReadString(aLen: SizeInt; out s: string): Boolean;
    function  DoReadBytes(aLen: SizeInt; out b: TBytes): Boolean;
    function  DoSkipBytes(aLen: SizeInt): Boolean; inline;
    function  GetCurrUnread: Int64; inline;
    procedure DoReadFixInt(aPfx: Byte); inline;
    procedure DoReadNegFixInt(aPfx: Byte); inline;
    procedure DoReadNil; inline;
    procedure DoReadBool(aValue: Boolean); inline;
    function  DoBeginArray(aLen: SizeInt): Boolean; inline;
    function  DoBeginMap(aLen: SizeInt): Boolean; inline;
    function  DoReadBin(aLen: SizeInt): Boolean; inline;
    function  DoReadExt(aLen: SizeInt): Boolean; inline;
    function  DoReadBinKey(aLen: SizeInt): Boolean; inline;
    function  DoReadStr(aLen: SizeInt): Boolean; inline;
    function  DoReadStrKey(aLen: SizeInt): Boolean; inline;
    function  DoReadBin8: Boolean;
    function  DoReadBin16: Boolean;
    function  DoReadBin32: Boolean;
    function  DoReadBin8Key: Boolean;
    function  DoReadBin16Key: Boolean;
    function  DoReadBin32Key: Boolean;
    function  DoReadUInt8Key: Boolean;
    function  DoReadUInt16Key: Boolean;
    function  DoReadUInt32Key: Boolean;
    function  DoReadUInt64Key: Boolean;
    function  DoReadInt8Key: Boolean;
    function  DoReadInt16Key: Boolean;
    function  DoReadInt32Key: Boolean;
    function  DoReadInt64Key: Boolean;
    function  DoReadStr8Key: Boolean;
    function  DoReadStr16Key: Boolean;
    function  DoReadStr32Key: Boolean;
    function  DoReadKey: Boolean;
    function  DoReadTStamp32: Boolean;
    function  DoReadTStamp64: Boolean;
    function  DoReadTStamp96: Boolean;
    function  DoReadExt8: Boolean;
    function  DoReadExt16: Boolean;
    function  DoReadExt32: Boolean;
    function  DoReadFloat32: Boolean;
    function  DoReadFloat64: Boolean;
    function  DoReadUInt8: Boolean;
    function  DoReadUInt16: Boolean;
    function  DoReadUInt32: Boolean;
    function  DoReadUInt64: Boolean;
    function  DoReadInt8: Boolean;
    function  DoReadInt16: Boolean;
    function  DoReadInt32: Boolean;
    function  DoReadInt64: Boolean;
    function  DoReadFixExt1: Boolean;
    function  DoReadFixExt2: Boolean;
    function  DoReadFixExt4: Boolean;
    function  DoReadFixExt8: Boolean;
    function  DoReadFixExt16: Boolean;
    function  DoReadStr8: Boolean;
    function  DoReadStr16: Boolean;
    function  DoReadStr32: Boolean;
    function  DoBeginArray16: Boolean;
    function  DoBeginArray32: Boolean;
    function  DoBeginMap16: Boolean;
    function  DoBeginMap32: Boolean;
    function  DoRead: Boolean;
    property  StackTop: Integer read FStackTop;
    property  ReadMode: Boolean read FReadMode;
    class function DoReadDom(aReader: TMpReader; out aNode: TMpDomNode;
                             aDuplicates: TMpDuplicates = mduAccept): Boolean; static;
    class function DoFind(aReader: TMpReader; const aKey: TMpVariant): Boolean; static;
    class function DoFindPath(aReader: TMpReader; const aPath: array of TMpVariant): Boolean; static;
    class function DoFindPath(aReader: TMpReader; const aPath: array of string): Boolean; static;
  public
  const
    DEF_DEPTH = 512;
  { tries to load the DOM tree from the specified buffer }
    class function TryReadDom(aBuffer: PByte; aCount: SizeInt; out aNode: TMpDomNode;
                              aDuplicates: TMpDuplicates = mduAccept;
                              aMaxDepth: Integer = DEF_DEPTH): Boolean; static;
    constructor Create(p: PByte; aCount: SizeInt; aMaxDepth: Integer = DEF_DEPTH);
  { tries to read the next token from the buffer }
    function Read: Boolean;
  { if the current token is the beginning of the structure, it skips its contents and stops
    at the closing token, otherwise it performs only one read }
    procedure Skip;
  { tries to copy the current structure "as is" }
    function CopyStruct(out aStruct: TBytes): Boolean;
  { tries to find an element using a path specified as an array of path segments }
    function FindPath(const aPath: array of TMpVariant; out aNode: TMpDomNode): Boolean;
    function FindPath(const aPath: array of TMpVariant; out aBytes: TBytes): Boolean;
    function FindPath(const aPath: array of string; out aNode: TMpDomNode): Boolean;
    function FindPath(const aPath: array of string; out aBytes: TBytes): Boolean;
  { tries to find the specified key in the current structure without trying to enter
    nested structures if the current token is the beginning of the structure }
    function Find(const aKey: TMpVariant): Boolean;
    property AsBoolean: Boolean read FBool;
    property AsInt: Int64 read FInt;
    property AsSingle: Single read FSingle;
    property AsDouble: Double read FDouble;
    property AsString: string read FString;
    property AsBinary: TBytes read FBytes;
    property AsTimeStamp: TMpTimeStamp read FTStamp;
    property AsExtention: TMpExtBlob read FBytes;
    property KeyValue: TMpVariant read FKey;
    property Position: SizeInt read FBufPos;
    property BufferSize: SizeInt read FBufSize;
    property Depth: Integer read FStackTop;
    property MaxDepth: SizeInt read GetMaxDepth;
    property ReadState: TMpReaderState read FState;
    property TokenKind: TMpTokenKind read FCurrToken;
    property StructKind: TMpStructKind read GetStruct;
    property StructUnread: Int64 read GetCurrUnread;
  end;

implementation
{$B-}{$COPERATORS ON}{$POINTERMATH ON}
{$IF NOT(DEFINED(CPU32) OR DEFINED(CPU64))}{$FATAL CPU not supported}{$ENDIF}
{$WARN 6058 OFF : Call to subroutine "$1" marked as inline is not inlined}
uses
  LgHash, LgStrConst;

{ TMpTimeStamp }

class operator TMpTimeStamp.=(const L, R: TMpTimeStamp): Boolean;
begin
  Result := (L.Seconds = R.Seconds) and (L.NanoSeconds = R.NanoSeconds);
end;

{$PUSH}{$WARN 4081 OFF : Converting the operands to "$1" before doing the multiply could prevent overflow errors.}
class function TMpTimeStamp.Make(aSec: Int64; aNSec: DWord): TMpTimeStamp;
var
  s: DWord;
begin
  if aNSec >= NSEC_PER_SEC then begin
    s := aNSec div NSEC_PER_SEC;
{$PUSH}{$Q+}{$R+}
    aSec += Int64(s);
{$POP}
    aNSec -= s * NSEC_PER_SEC;
  end;
  Result.FSeconds := aSec;
  Result.FNanoSeconds := aNSec;
end;
{$POP}

class function TMpTimeStamp.Make(aValue: TDateTime): TMpTimeStamp;
var
  delta: Int64;
  ms: Integer;
begin
  delta := MillisecondsBetween(aValue, UnixEpoch);
  if aValue < UnixEpoch then
    begin
      Result.FSeconds := -delta div MSecsPerSec;
      ms := -delta - Result.FSeconds*MSecsPerSec;
      if ms <> 0 then begin
        ms += MSecsPerSec;
        Dec(Result.FSeconds);
      end;
    end
  else
    begin
      Result.FSeconds := delta div MSecsPerSec;
      ms := delta - Result.FSeconds*MSecsPerSec;
    end;
  Result.FNanoSeconds := DWord(ms * NSEC_PER_MSEC);
end;

class function TMpTimeStamp.TryTsToDateTime(const aTs: TMpTimeStamp; out aValue: TDateTime): Boolean;
var
  delta: Int64;
begin
  if aTs.Seconds < 0 then
    if aTs.Seconds <= System.Low(Int64) div MSecsPerSec then exit(False) else
  else
    if aTs.Seconds >= System.High(Int64) div MSecsPerSec then exit(False);
  delta := aTs.Seconds * MSecsPerSec + Round(Double(aTs.NanoSeconds)/NSEC_PER_MSEC);
  if (delta < -UNIX_MSEC_OFS) or (delta > UNIX_MSEC_MAX) then exit(False);
  aValue := DateUtils.IncMillisecond(UnixEpoch, delta);
  Result := True;
end;

function TMpTimeStamp.ToDateTime: TDateTime;
begin
  if not TryTsToDateTime(Self, Result) then
    raise EConvertError.Create('Failed to convert TMpTimeStamp to TDateTime');
end;

function TMpTimeStamp.AsJson: string;
begin
  Result := '{"sec":"' + Seconds.ToString + '","nsec":"' + NanoSeconds.ToString + '"}';
end;

{ TMpVariant }

procedure TMpVariant.DoClear;
begin
  case FKind of
    mvkStr:  string(FValue.Ref) := '';
    mvkBin:  TBytes(FValue.Ref) := nil;
  else
  end;
end;

class operator TMpVariant.Initialize(var v: TMpVariant);
begin
  v.FValue.Int := 0;
  v.FKind := mvkNull;
end;

class operator TMpVariant.Finalize(var v: TMpVariant);
begin
  v.DoClear;
  v.FKind := mvkNull;
end;

class operator TMpVariant.Copy(constref aSrc: TMpVariant; var aDst: TMpVariant);
begin
  aDst.DoClear;
  case aSrc.Kind of
    mvkNull: ;
    mvkStr:  string(aDst.FValue.Ref) := string(aSrc.FValue.Ref);
    mvkBin:  TBytes(aDst.FValue.Ref) := TBytes(aSrc.FValue.Ref);
  else
    aDst.FValue.Int := aSrc.FValue.Int;
  end;
  aDst.FKind := aSrc.Kind;
end;

class function TMpVariant.EqualBytes(const L, R: TBytes): Boolean;
begin
  if Pointer(L) = Pointer(R) then exit(True);
  if System.Length(L) <> System.Length(R) then exit(False);
  Result := System.CompareByte(Pointer(L)^, Pointer(R)^, System.Length(L)) = 0;
end;

class function TMpVariant.BytesHash(const b: TBytes): SizeInt;
begin
  Result := TxxHash32LE.HashBuf(Pointer(b), System.Length(b), 47);
end;

class procedure TMpVariant.CastError;
begin
  raise EInvalidCast.Create('TMpVariant invalid cast');
end;

{$PUSH}{$WARN 5093 OFF : Function result variable of a managed type does not seem to be initialized}
class operator TMpVariant.:=(aValue: Byte): TMpVariant;
begin
  with Result do begin
    DoClear;
    FValue.Int := aValue;
    FKind := mvkInt;
  end;
end;

class operator TMpVariant.:=(aValue: ShortInt): TMpVariant;
begin
  with Result do begin
    DoClear;
    FValue.Int := aValue;
    FKind := mvkInt;
  end;
end;

class operator TMpVariant.:=(aValue: Word): TMpVariant;
begin
  with Result do begin
    DoClear;
    FValue.Int := aValue;
    FKind := mvkInt;
  end;
end;

class operator TMpVariant.:=(aValue: SmallInt): TMpVariant;
begin
  with Result do begin
    DoClear;
    FValue.Int := aValue;
    FKind := mvkInt
  end;
end;

class operator TMpVariant.:=(aValue: UInt32): TMpVariant;
begin
  with Result do begin
    DoClear;
    FValue.Int := aValue;
    FKind := mvkInt;
  end;
end;

class operator TMpVariant.:=(aValue: Int32): TMpVariant;
begin
  with Result do begin
    DoClear;
    FValue.Int := aValue;
    FKind := mvkInt;
  end;
end;

class operator TMpVariant.:=(aValue: Int64): TMpVariant;
begin
  with Result do begin
    DoClear;
    FValue.Int := aValue;
    FKind := mvkInt;
  end;
end;

class operator TMpVariant.:=(const aValue: string): TMpVariant;
begin
  with Result do begin
    DoClear;
    string(FValue.Ref) := aValue;
    FKind := mvkStr;
  end;
end;

class operator TMpVariant.:=(const aValue: TBytes): TMpVariant;
begin
  with Result do begin
    DoClear;
    TBytes(FValue.Ref) := aValue;
    FKind := mvkBin;
  end;
end;
{$POP}

{$PUSH}{$WARN 5059 OFF : Function result variable does not seem to be initialized}
class operator TMpVariant.=(const L, R: TMpVariant): Boolean;
begin
  if L.Kind <> R.Kind then exit(False);
  case L.Kind of
    mvkNull: Result := True;
    mvkInt:  Result := L.FValue.Int = R.FValue.Int;
    mvkStr:  Result := string(L.FValue.Ref) = string(R.FValue.Ref);
    mvkBin:  Result := EqualBytes(TBytes(L.FValue.Ref), TBytes(R.FValue.Ref));
  end;
end;

class function TMpVariant.HashCode(const aValue: TMpVariant): SizeInt;
begin
  case aValue.FKind of
    mvkNull: Result := 111;
    mvkInt:  Result := TxxHash32LE.HashQWord(QWord(aValue.FValue.Int), 1013);
    mvkStr:  Result := TxxHash32LE.HashStr(string(aValue.FValue.Ref), 23);
    mvkBin:  Result := BytesHash(TBytes(aValue.FValue.Ref));
  end;
end;
{$POP}

class function TMpVariant.Equal(const L, R: TMpVariant): Boolean;
begin
  Result := L = R;
end;

procedure TMpVariant.Clear;
begin
  DoClear;
  FKind := mvkNull;
end;

function TMpVariant.AsInt: Int64;
begin
  if Kind <> mvkInt then CastError;
  Result := FValue.Int;
end;

function TMpVariant.AsString: string;
begin
  if Kind <> mvkStr then CastError;
  Result := string(FValue.Ref);
end;

function TMpVariant.AsBinary: TBytes;
begin
  if Kind <> mvkBin then CastError;
  Result := TBytes(FValue.Ref);
end;

function BinToHexStr(const aBin: array of Byte): string;
{$PUSH}{$J-}
const
  Hex: array[0..15] of AnsiChar = (
    '0','1','2','3','4','5','6','7','8','9','A','B','C','D','E','F');
{$POP}
var
  s: string;
  p: PAnsiChar;
  I, J, Len: SizeInt;
  b: Byte;
begin
  Len := System.Length(aBin);
  if Len = 0 then exit('');
  System.SetLength(s, Len * 2);
  p := PAnsiChar(s);
  J := 0;
  for I := 0 to Pred(Len) do begin
    b := aBin[I];
    p[ J ] := Hex[b shr  4];
    p[J+1] := Hex[b and $f];
    p += 2;
  end;
  Result := s;
end;

function TMpVariant.ToString: string;
begin
  case Kind of
    mvkNull: Result := 'null';
    mvkInt:  Result := FValue.Int.ToString;
    mvkStr:  Result := string(FValue.Ref);
    mvkBin:  Result := BinToHexStr(TBytes(FValue.Ref));
  else
    Result := '';
  end;
end;

{ TMpDomNode.TEnumerator }

function TMpDomNode.TEnumerator.GetCurrent: TMpDomNode;
begin
  case FKind of
    mnkArray: Result := PMpDomNode(FCurrNode)^;
    mnkMap:   Result := TMpMap.PNode(FCurrNode)^.Data.Value;
  else
    Result := nil;
  end;
end;

function TMpDomNode.TEnumerator.MoveNext: Boolean;
begin
  if FCurrNode < FLastNode then
    case FKind of
      mnkArray:
        begin
          Inc(PMpDomNode(FCurrNode));
          exit(True);
        end;
      mnkMap:
        begin
          Inc(TMpMap.PNode(FCurrNode));
          exit(True);
        end;
    else
    end;
  Result := False;
end;

{ TMpDomNode.TPairEnumerator }

function TMpDomNode.TPairEnumerator.GetCurrent: TPair;
begin
  Result := FCurrNode^.Data;
end;

function TMpDomNode.TPairEnumerator.GetEnumerator: TPairEnumerator;
begin
  Result := Self;
end;

function TMpDomNode.TPairEnumerator.MoveNext: Boolean;
begin
  if FCurrNode < FLastNode then
    begin
      Inc(FCurrNode);
      exit(True);
    end;
  Result := False;
end;

{ TMpDomNode.TEqualKeys }

function TMpDomNode.TEqualKeys.GetCurrent: TPair;
begin
  Result := FEnum.Current;
end;

procedure TMpDomNode.TEqualKeys.Init(const aEnum: TMpMap.TEqualEnumerator);
begin
  FEnum := aEnum;
end;

function TMpDomNode.TEqualKeys.GetEnumerator: TEqualKeys;
begin
  Result := Self;
end;

function TMpDomNode.TEqualKeys.MoveNext: Boolean;
begin
  Result := FEnum.MoveNext;
end;

{ TMpDomNode }

function TMpDomNode.GetArrayRef: PMpArray;
begin
  Result := @FValue.Ref;
end;

function TMpDomNode.GetMapRef: PMpMap;
begin
  Result := @FValue.Ref;
end;

procedure TMpDomNode.DoClear;
begin
  case FKind of
    mnkString: string(FValue.Ref) := '';
    mnkBin:    TBytes(FValue.Ref) := nil;
    mnkArray:  TMpArray(FValue.Ref).Clear;
    mnkMap:    TMpMap(FValue.Ref).Clear;
    mnkExt:    TMpExtBlob(FValue.Ref) := nil;
  else
  end;
end;

function TMpDomNode.GetAsBool: Boolean;
begin
  if Kind <> mnkBool then begin
    DoClear; FKind := mnkBool;
  end;
  Result := FValue.Bool;
end;

function TMpDomNode.GetAsInt: Int64;
begin
  if Kind <> mnkInt then begin
    DoClear; FKind := mnkInt;
  end;
  Result := FValue.Int;
end;

function TMpDomNode.GetAsSingle: Single;
begin
  if Kind <> mnkSingle then begin
    DoClear; FKind := mnkSingle;
  end;
  Result := FValue.Flt;
end;

function TMpDomNode.GetAsDouble: Double;
begin
  if Kind <> mnkDouble then begin
    DoClear; FKind := mnkDouble;
  end;
  Result := FValue.Dbl;
end;

function TMpDomNode.GetAsString: string;
begin
  if Kind <> mnkString then begin
    DoClear; FKind := mnkString;
  end;
  Result := string(FValue.Ref);
end;

function TMpDomNode.GetAsBytes: TBytes;
begin
  if Kind <> mnkBin then begin
    DoClear; FKind := mnkBin;
  end;
  Result := TBytes(FValue.Ref);
end;

function TMpDomNode.GetAsTStamp: TMpTimeStamp;
begin
  if Kind <> mnkTStamp then begin
    DoClear; FKind := mnkTStamp;
  end;
  Result := TMpTimeStamp.Make(FValue.Int, FNSec);
end;

function TMpDomNode.GetAsExt: TMpExtBlob;
begin
  if Kind <> mnkExt then begin
    DoClear; FKind := mnkExt;
  end;
  Result := TMpExtBlob(FValue.Ref);
end;

function TMpDomNode.GetAsArray: TMpDomNode;
begin
  if Kind <> mnkArray then begin
    DoClear; FKind := mnkArray;
  end;
  Result := Self;
end;

function TMpDomNode.GetAsMap: TMpDomNode;
begin
  if Kind <> mnkMap then begin
    DoClear; FKind := mnkMap;
  end;
  Result := Self;
end;

function TMpDomNode.GetAsMsgPack: TBytes;
begin
  Result := TMpWriter.DomToBytes(Self);
end;

function TMpDomNode.GetMsgPackStr: rawbytestring;
begin
  Result := TMpWriter.DomToRawStr(Self);
end;

function TMpDomNode.GetAsJson: string;
var
  Writer: TJsonStrWriter;
  procedure DoWriteNode(aNode: TMpDomNode);
  var
    I: SizeInt;
    q: QWord;
    I64: Int64;
  begin
    case aNode.Kind of
      mnkNil: Writer.AddNull;
      mnkBool:
        if aNode.AsBoolean then
          Writer.AddTrue
        else
          Writer.AddFalse;
      mnkInt:
        begin
          I64 := aNode.AsInteger;
          if (I64 >= -Double.MAX_EXACT_INT) and (I64 <= Double.MAX_EXACT_INT) then
            Writer.Add(I64)
          else
            Writer.Add(I64.ToString);
        end;
      mnkSingle: Writer.Add(aNode.AsSingle);
      mnkDouble: Writer.Add(aNode.AsDouble);
      mnkString: Writer.Add(aNode.AsString);
      mnkBin:    Writer.Add(BinToHexStr(aNode.AsBinary));
      mnkArray:
        begin
          Writer.BeginArray;
          for I := 0 to Pred(aNode.GetArrayRef^.Count) do
            DoWriteNode(aNode.GetArrayRef^.UncItems[I]);
          Writer.EndArray;
        end;
      mnkMap:
        begin
          Writer.BeginObject;
          for I := 0 to Pred(aNode.GetMapRef^.Count) do
            with aNode.GetMapRef^.UncMutPairs[I]^ do begin
              Writer.AddName(Key.ToString);
              DoWriteNode(Value);
            end;
          Writer.EndObject;
        end;
      mnkTStamp: Writer.AddJson(aNode.AsTimeStamp.AsJson);
      mnkExt:    Writer.Add(BinToHexStr(aNode.AsExtention));
    end;
  end;
var
  Ref: specialize TGAutoRef<TJsonStrWriter>;
begin
  Writer := Ref;
  DoWriteNode(Self);
  Result := Writer.JsonString;
end;

function TMpDomNode.GetCount: SizeInt;
begin
  case Kind of
    mnkArray: Result := GetArrayRef^.Count;
    mnkMap:   Result := GetMapRef^.Count;
  else
    Result := 0;
  end;
end;

function TMpDomNode.GetItem(aIndex: SizeInt): TMpDomNode;
begin
  if SizeUInt(aIndex) >= SizeUInt(Count) then
    raise EMpDomError.CreateFmt(SEIndexOutOfBoundsFmt, [aIndex]);
  case Kind of
    mnkArray: exit(GetArrayRef^.UncItems[aIndex]);
    mnkMap:   exit(GetMapRef^.UncItems[aIndex]);
  else
    Result := nil;
  end;
end;

function TMpDomNode.GetPair(aIndex: SizeInt): TPair;
begin
  if Kind <> mnkMap then
    raise EMpDomError.Create(SEMpNodeInstIsNotMap);
  if SizeUInt(aIndex) >= SizeUInt(GetMapRef^.Count) then
    raise EMpDomError.CreateFmt(SEIndexOutOfBoundsFmt, [aIndex]);
  Result := GetMapRef^.UncMutPairs[aIndex]^;
end;

function TMpDomNode.GetMapProp(const aKey: TMpVariant): TMpDomNode;
var
  n: TMpDomNode;
begin // workaround for optimizer bug (attempt)
  if FindOrAdd(aKey, n) then
    Result := n
  else
    Result := n;
end;

procedure TMpDomNode.SetNil;
begin
  if Kind <> mnkNil then begin
    DoClear; FKind := mnkNil;
  end;
end;

procedure TMpDomNode.SetAsBool(aValue: Boolean);
begin
  if Kind <> mnkBool then begin
    DoClear; FKind := mnkBool;
  end;
  FValue.Bool := aValue;
end;

procedure TMpDomNode.SetAsInt(aValue: Int64);
begin
  if Kind <> mnkInt then begin
    DoClear; FKind := mnkInt;
  end;
  FValue.Int := aValue;
end;

procedure TMpDomNode.SetAsSingle(aValue: Single);
begin
  if Kind <> mnkSingle then begin
    DoClear; FKind := mnkSingle;
  end;
  FValue.Flt := aValue;
end;

procedure TMpDomNode.SetAsDouble(aValue: Double);
begin
  if Kind <> mnkDouble then begin
    DoClear; FKind := mnkDouble;
  end;
  FValue.Dbl := aValue;
end;

procedure TMpDomNode.SetAsString(const s: string);
begin
  if Kind <> mnkString then begin
    DoClear; FKind := mnkString;
  end;
  string(FValue.Ref) := s;
end;

procedure TMpDomNode.SetAsBytes(const b: TBytes);
begin
  if Kind <> mnkBin then begin
    DoClear; FKind := mnkBin;
  end;
  TBytes(FValue.Ref) := b;
end;

procedure TMpDomNode.SetAsTStamp(const ts: TMpTimeStamp);
begin
  if Kind <> mnkTStamp then begin
    DoClear; FKind := mnkTStamp;
  end;
  FValue.Int := ts.Seconds;
  FNSec := ts.NanoSeconds;
end;

procedure TMpDomNode.SetAsExt(const e: TMpExtBlob);
begin
  if Kind <> mnkExt then begin
    DoClear; FKind := mnkExt;
  end;
  TMpExtBlob(FValue.Ref) := e;
end;

procedure TMpDomNode.SetAsMsgPack(const b: TBytes);
begin
  if not TryParse(b) then
    raise EMpDomError.Create(SEMpFailParseMpBin);
end;

procedure TMpDomNode.SetAsJson(const s: string);
begin
  if not TryLoadJson(s) then
    raise EMpDomError.Create(SECantParseJsStr);
end;

procedure TMpDomNode.SetMsgPackStr(const rbs: rawbytestring);
begin
  if not TryParse(rbs) then
    raise EMpDomError.Create(SEMpFailParseMpRaw);
end;

class procedure TMpDomNode.MoveNode(aSrc, aDst: TMpDomNode);
begin
  aDst.DoClear;
  aDst.FValue := aSrc.FValue;
  aDst.FKind := aSrc.Kind;
  aSrc.FValue.Int := 0;
  aSrc.FNSec := 0;
  aSrc.FKind := mnkNil;
end;

class function TMpDomNode.MakeNil: TMpDomNode;
begin
  Result := TMpDomNode.Create;
end;

class function TMpDomNode.Make(aValue: Boolean): TMpDomNode;
begin
  Result := TMpDomNode.Create;
  Result.FValue.Bool := aValue;
  Result.FKind := mnkBool;
end;

class function TMpDomNode.MakeI(aValue: Int64): TMpDomNode;
begin
  Result := TMpDomNode.Create;
  Result.FValue.Int := aValue;
  Result.FKind := mnkInt;
end;

class function TMpDomNode.MakeS(aValue: Single): TMpDomNode;
begin
  Result := TMpDomNode.Create;
  Result.FValue.Flt := aValue;
  Result.FKind := mnkSingle;
end;

class function TMpDomNode.MakeD(aValue: Double): TMpDomNode;
begin
  Result := TMpDomNode.Create;
  Result.FValue.Dbl := aValue;
  Result.FKind := mnkDouble;
end;

class function TMpDomNode.Make(const s: string): TMpDomNode;
begin
  Result := TMpDomNode.Create;
  string(Result.FValue.Ref) := s;
  Result.FKind := mnkString;
end;

class function TMpDomNode.Make(const a: array of Byte): TMpDomNode;
var
  b: TBytes = nil;
begin
  Result := TMpDomNode.Create;
  System.SetLength(b, System.Length(a));
  if b <> nil then System.Move(a[0], b[0], System.Length(a));
  TBytes(Result.FValue.Ref) := b;
  Result.FKind := mnkBin;
end;

class function TMpDomNode.MakeB(const b: TBytes): TMpDomNode;
begin
  Result := TMpDomNode.Create;
  TBytes(Result.FValue.Ref) := b;
  Result.FKind := mnkBin;
end;

class function TMpDomNode.Make(const ts: TMpTimeStamp): TMpDomNode;
begin
  Result := TMpDomNode.Create;
  Result.FValue.Int := ts.Seconds;
  Result.FNSec := ts.NanoSeconds;
  Result.FKind := mnkTStamp;
end;

class function TMpDomNode.TryParse(aBuffer: PByte; aCount: SizeInt; out aRoot: TMpDomNode; aDuplicates: TMpDuplicates;
  aMaxDepth: SizeInt): Boolean;
begin
  Result := TMpReader.TryReadDom(aBuffer, aCount, aRoot, aDuplicates, aMaxDepth);
end;

class function TMpDomNode.TryParse(const s: rawbytestring; out aRoot: TMpDomNode; aDuplicates: TMpDuplicates;
  aMaxDepth: SizeInt): Boolean;
begin
  Result := TryParse(PByte(s), System.Length(s), aRoot, aDuplicates, aMaxDepth);
end;

class function TMpDomNode.TryParse(const aBytes: TBytes; out aRoot: TMpDomNode; aDuplicates: TMpDuplicates;
  aMaxDepth: SizeInt): Boolean;
begin
  Result := TryParse(PByte(aBytes), System.Length(aBytes), aRoot, aDuplicates, aMaxDepth);
end;

class function TMpDomNode.TryParse(aStream: TStream; out aRoot: TMpDomNode; aCount: SizeInt;
  aDuplicates: TMpDuplicates; aMaxDepth: SizeInt): Boolean;
var
  s: rawbytestring;
begin
{$PUSH}{$Q+}{$R+}
  if aCount < 1 then
    aCount := aStream.Size - aStream.Position;
{$POP}
  System.SetLength(s, aCount);
  aStream.ReadBuffer(Pointer(s)^, aCount);
  Result := TryParse(PByte(s), aCount, aRoot, aDuplicates, aMaxDepth);
end;

class function TMpDomNode.TryParseFile(const aFileName: string; out aRoot: TMpDomNode;
  aDuplicates: TMpDuplicates; aMaxDepth: SizeInt): Boolean;
var
  fs: TFileStream;
begin
  fs := TFileStream.Create(aFileName, fmOpenRead or fmShareDenyWrite);
  try
    Result := TryParse(fs, aRoot, 0, aDuplicates, aMaxDepth);
  finally
    fs.Free;
  end;
end;

class function TMpDomNode.TryLoadJson(const aJson: string; out aRoot: TMpDomNode; aDuplicates: TMpDuplicates;
  aMaxDepth: SizeInt): Boolean;
var
  Reader: TJsonReader = nil;

  function DoReadNode(aNode: TMpDomNode): Boolean; forward;

  function DoReadArray(aNode: TMpDomNode): Boolean;
  begin
    if Reader.TokenKind <> rtkArrayBegin then exit(False);
    repeat
      if not Reader.Read then exit(False);
      if Reader.TokenKind = rtkArrayEnd then break;
      if not DoReadNode(aNode.AddNode) then exit(False);
    until False;
    Result := True;
  end;

  function DoReadObject(aNode: TMpDomNode): Boolean;
  var
    n: TMpDomNode;
  begin
    if Reader.TokenKind <> rtkObjectBegin then exit(False);
    repeat
      if not Reader.Read then exit(False);
      if Reader.TokenKind = rtkObjectEnd then break;
      case aDuplicates of
        mduAccept:  n := aNode.AddNode(Reader.Name);
        mduRewrite: n := aNode[Reader.Name].AsNil;
      else // mduIgnore
        if not aNode.TryAddNode(Reader.Name, n) then begin
          if Reader.IsStartToken(Reader.TokenKind) then
            Reader.Skip;
          continue;
        end;
      end;
      if not DoReadNode(n) then exit(False);
    until False;
    Result := True;
  end;

  function DoReadNode(aNode: TMpDomNode): Boolean;
  var
    I: Int64;
    d: Double;
  begin
    case Reader.TokenKind of
      rtkNull:  aNode.SetNil;
      rtkFalse: aNode.AsBoolean := False;
      rtkTrue:  aNode.AsBoolean := True;
      rtkNumber:
        begin
          d := Reader.AsNumber;
          if d.IsExactInt(I) then
            aNode.AsInteger := I
          else
            aNode.AsDouble := d;
        end;
      rtkString:       aNode.AsString := Reader.AsString;
      rtkArrayBegin:  if not DoReadArray(aNode) then exit(False);
      rtkObjectBegin: if not DoReadObject(aNode) then exit(False);
    else
      exit(False);
    end;
    Result := True;
  end;
begin
  Result := False;
  aRoot := TMpDomNode.Create;
  try
    Reader := TJsonReader.Create(PAnsiChar(aJson), System.Length(aJson), aMaxDepth);
    try
      try
        if not Reader.Read then exit(False);
        Result := DoReadNode(aRoot);
      except
      end;
    finally
      Reader.Free;
    end;
  finally
    if not Result then FreeAndNil(aRoot);
  end;
end;

destructor TMpDomNode.Destroy;
begin
  DoClear;
  inherited;
end;

function TMpDomNode.GetEnumerator: TEnumerator;
begin
  Result.FKind := Kind;
  case Kind of
    mnkArray:
      begin
        Result.FCurrNode := PMpDomNode(GetArrayRef^.List) - 1;
        Result.FLastNode := PMpDomNode(Result.FCurrNode) + GetArrayRef^.Count;
      end;
    mnkMap:
      begin
        Result.FCurrNode := TMpMap.PNode(GetMapRef^.List) - 1;
        Result.FLastNode := TMpMap.PNode(Result.FCurrNode) + GetMapRef^.Count;
      end
  else
    Result.FCurrNode := nil;
    Result.FLastNode := nil;
  end;
end;

function TMpDomNode.PairEnumerator: TPairEnumerator;
begin
  if Kind = mnkMap then
    begin
      Result.FCurrNode := GetMapRef^.List - 1;
      Result.FLastNode := Result.FCurrNode + GetMapRef^.Count;
    end
  else
    begin
      Result.FCurrNode := nil;
      Result.FLastNode := nil;
    end;
end;

function TMpDomNode.EqualKeys(const aKey: TMpVariant): TEqualKeys;
begin
  if Kind = mnkMap then
    Result.Init(GetMapRef^.GetEqualKeys(aKey))
  else
    Result.Init(TMpMap.GetEmptyEqualKeys);
end;

procedure TMpDomNode.Clear;
begin
  DoClear;
  FKind := mnkNil;
end;

procedure TMpDomNode.EnsureCapacity(aValue: SizeInt);
begin
  case Kind of
    mnkArray: GetArrayRef^.EnsureCapacity(aValue);
    mnkMap:   GetMapRef^.EnsureCapacity(aValue);
  else
  end;
end;

function TMpDomNode.TryParse(aBuf: PByte; aCount: SizeInt; aDuplicates: TMpDuplicates;
  aMaxDepth: SizeInt): Boolean;
var
  Node: TMpDomNode;
begin
  Result := TMpDomNode.TryParse(aBuf, aCount, Node, aDuplicates, aMaxDepth);
  if Result then
    try
      MoveNode(Node, Self);
    finally
      Node.Free;
    end;
end;

function TMpDomNode.TryParse(const s: rawbytestring; aDuplicates: TMpDuplicates; aMaxDepth: SizeInt): Boolean;
begin
  Result := TryParse(PByte(s), System.Length(s), aDuplicates, aMaxDepth);
end;

function TMpDomNode.TryParse(const aBytes: TBytes; aDuplicates: TMpDuplicates; aMaxDepth: SizeInt): Boolean;
begin
  Result := TryParse(PByte(aBytes), System.Length(aBytes), aDuplicates, aMaxDepth);
end;

function TMpDomNode.TryParse(aStream: TStream; aCount: SizeInt; aDuplicates: TMpDuplicates;
  aMaxDepth: SizeInt): Boolean;
var
  Node: TMpDomNode;
begin
  Result := TMpDomNode.TryParse(aStream, Node, aCount, aDuplicates, aMaxDepth);
  if Result then
    try
      MoveNode(Node, Self);
    finally
      Node.Free;
    end;
end;

function TMpDomNode.TryParseFile(const aFileName: string; aDuplicates: TMpDuplicates;
  aMaxDepth: SizeInt): Boolean;
var
  Node: TMpDomNode;
begin
  Result := TMpDomNode.TryParseFile(aFileName, Node, aDuplicates, aMaxDepth);
  if Result then
    try
      MoveNode(Node, Self);
    finally
      Node.Free;
    end;
end;

function TMpDomNode.TryLoadJson(const aJson: string; aDuplicates: TMpDuplicates; aMaxDepth: SizeInt): Boolean;
var
  Node: TMpDomNode;
begin
  Result := TMpDomNode.TryLoadJson(aJson, Node, aDuplicates, aMaxDepth);
  if Result then
    try
      MoveNode(Node, Self);
    finally
      Node.Free;
    end;
end;

procedure TMpDomNode.CopyFrom(aNode: TMpDomNode);
  procedure DoCopy(aSrc, aDst: TMpDomNode);
  var
    Node: TMpDomNode;
    I: SizeInt;
  begin
    case aSrc.Kind of
      mnkNil:    aDst.AsNil;
      mnkBool:   aDst.AsBoolean := aSrc.AsBoolean;
      mnkInt:    aDst.AsInteger := aSrc.AsInteger;
      mnkSingle: aDst.AsSingle := aSrc.AsSingle;
      mnkDouble: aDst.AsDouble := aSrc.AsDouble;
      mnkString: aDst.AsString := aSrc.AsString;
      mnkBin:    aDst.AsBinary := aSrc.AsBinary; // todo: Copy ???
      mnkArray:
        for I := 0 to Pred(aSrc.GetArrayRef^.Count) do begin
          Node := TMpDomNode.Create;
          DoCopy(aSrc.GetArrayRef^.UncItems[I], Node);
          aDst.AsArray.GetArrayRef^.Add(Node);
        end;
      mnkMap:
        for I := 0 to Pred(aSrc.GetMapRef^.Count) do
          with aSrc.GetMapRef^.UncMutPairs[I]^ do begin
            Node := TMpDomNode.Create;
            DoCopy(Value, Node);
            aDst.AsMap.GetMapRef^.Add(Key, Node);
          end;
      mnkTStamp: aDst.AsTimeStamp := aSrc.AsTimeStamp;
    else
    end;
  end;
begin
  if aNode = Self then exit;
  Clear;
  DoCopy(aNode, Self);
end;

procedure TMpDomNode.CopyFrom(aNode: TJsonNode);
  procedure DoCopy(aSrc: TJsonNode; aDst: TMpDomNode);
  var
    I64: Int64;
    d: Double;
    I: SizeInt;
    n: TMpDomNode;
    p: TJsonNode.TPair;
  begin
    case aSrc.Kind of
      jvkNull:  aDst.SetNil;
      jvkFalse: aDst.AsBoolean := False;
      jvkTrue:  aDst.AsBoolean := True;
      jvkNumber:
        begin
          d := aSrc.AsNumber;
          if d.IsExactInt(I64) then
            aDst.AsInteger := I64
          else
            aDst.AsDouble := d;
        end;
      jvkString: aDst.AsString := aSrc.AsString;
      jvkArray:
        for I := 0 to Pred(aSrc.Count) do begin
          n := TMpDomNode.Create;
          DoCopy(aSrc.Items[I], n);
          aDst.AsArray.GetArrayRef^.Add(n);
        end;
      jvkObject:
        for I := 0 to Pred(aSrc.Count) do begin
          n := TMpDomNode.Create;
          p := aSrc.Pairs[I];
          DoCopy(p.Value, n);
          aDst.AsMap.GetMapRef^.Add(p.Key, n);
        end;
    end;
  end;
begin
  Clear;
  DoCopy(aNode, Self);
end;

function TMpDomNode.Clone: TMpDomNode;
begin
  Result := TMpDomNode.Create;
  Result.CopyFrom(Self);
end;

function TMpDomNode.IsNil: Boolean;
begin
  Result := Kind = mnkNil;
end;

function TMpDomNode.IsBoolean: Boolean;
begin
  Result := Kind = mnkBool;
end;

function TMpDomNode.IsInteger: Boolean;
begin
  Result := Kind = mnkInt;
end;

function TMpDomNode.IsSingle: Boolean;
begin
  Result := Kind = mnkSingle;
end;

function TMpDomNode.IsDouble: Boolean;
begin
  Result := Kind = mnkDouble;
end;

function TMpDomNode.IsFloat: Boolean;
begin
  Result := Kind in [mnkSingle, mnkDouble];
end;

function TMpDomNode.IsString: Boolean;
begin
  Result := Kind = mnkString;
end;

function TMpDomNode.IsBinary: Boolean;
begin
  Result := Kind = mnkBin;
end;

function TMpDomNode.IsScalar: Boolean;
begin
  Result := not(Kind in [mnkArray, mnkMap]);
end;

function TMpDomNode.IsArray: Boolean;
begin
  Result := Kind = mnkArray;
end;

function TMpDomNode.IsMap: Boolean;
begin
  Result := Kind = mnkMap;
end;

function TMpDomNode.IsStruct: Boolean;
begin
  Result := Kind in [mnkArray, mnkMap];
end;

function TMpDomNode.AsNil: TMpDomNode;
begin
  SetNil;
  Result := Self;
end;

function TMpDomNode.AddNil: TMpDomNode;
begin
  AsArray.GetArrayRef^.Add(TMpDomNode.Create);
  Result := Self;
end;

function TMpDomNode.Add(aValue: Boolean): TMpDomNode;
begin
  AsArray.GetArrayRef^.Add(TMpDomNode.Make(aValue));
  Result := Self;
end;

function TMpDomNode.AddI(aValue: Int64): TMpDomNode;
begin
  AsArray.GetArrayRef^.Add(TMpDomNode.MakeI(aValue));
  Result := Self;
end;

function TMpDomNode.AddS(aValue: Single): TMpDomNode;
begin
  AsArray.GetArrayRef^.Add(TMpDomNode.MakeS(aValue));
  Result := Self;
end;

function TMpDomNode.AddD(aValue: Double): TMpDomNode;
begin
  AsArray.GetArrayRef^.Add(TMpDomNode.MakeD(aValue));
  Result := Self;
end;

function TMpDomNode.Add(const s: string): TMpDomNode;
begin
  AsArray.GetArrayRef^.Add(TMpDomNode.Make(s));
  Result := Self;
end;

function TMpDomNode.Add(const a: array of Byte): TMpDomNode;
begin
  AsArray.GetArrayRef^.Add(TMpDomNode.Make(a));
  Result := Self;
end;

function TMpDomNode.AddB(const b: TBytes): TMpDomNode;
begin
  AsArray.GetArrayRef^.Add(TMpDomNode.MakeB(b));
  Result := Self;
end;

function TMpDomNode.Add(const ts: TMpTimeStamp): TMpDomNode;
begin
  AsArray.GetArrayRef^.Add(TMpDomNode.Make(ts));
  Result := Self;
end;

function TMpDomNode.AddNode: TMpDomNode;
begin
  Result := TMpDomNode.Create;
  AsArray.GetArrayRef^.Add(Result);
end;

function TMpDomNode.InsertNil(aIndex: SizeInt): SizeInt;
begin
  Result := AsArray.GetArrayRef^.Insert(aIndex, TMpDomNode.Create);
end;

function TMpDomNode.Insert(aIndex: SizeInt; aValue: Boolean): SizeInt;
begin
  Result := AsArray.GetArrayRef^.Insert(aIndex, TMpDomNode.Make(aValue));
end;

function TMpDomNode.InsertI(aIndex: SizeInt; aValue: Int64): SizeInt;
begin
  Result := AsArray.GetArrayRef^.Insert(aIndex, TMpDomNode.MakeI(aValue));
end;

function TMpDomNode.InsertS(aIndex: SizeInt; aValue: Single): SizeInt;
begin
  Result := AsArray.GetArrayRef^.Insert(aIndex, TMpDomNode.MakeS(aValue));
end;

function TMpDomNode.InsertD(aIndex: SizeInt; aValue: Double): SizeInt;
begin
  Result := AsArray.GetArrayRef^.Insert(aIndex, TMpDomNode.MakeD(aValue));
end;

function TMpDomNode.Insert(aIndex: SizeInt; const s: string): SizeInt;
begin
  Result := AsArray.GetArrayRef^.Insert(aIndex, TMpDomNode.Make(s));
end;

function TMpDomNode.Insert(aIndex: SizeInt; const a: array of Byte): SizeInt;
begin
  Result := AsArray.GetArrayRef^.Insert(aIndex,TMpDomNode. Make(a));
end;

function TMpDomNode.InsertB(aIndex: SizeInt; const b: TBytes): SizeInt;
begin
  Result := AsArray.GetArrayRef^.Insert(aIndex, TMpDomNode.MakeB(b));
end;

function TMpDomNode.Insert(aIndex: SizeInt; const ts: TMpTimeStamp): SizeInt;
begin
  Result := AsArray.GetArrayRef^.Insert(aIndex, TMpDomNode.Make(ts));
end;

function TMpDomNode.Delete(aIndex: SizeInt): Boolean;
var
  Node: TMpDomNode;
begin
  case Kind of
    mnkArray:
      begin
        Result := GetArrayRef^.Extract(aIndex, Node);
        if Result then Node.Free;
      end;
    mnkMap:
      begin
        Result := GetMapRef^.Extract(aIndex, Node);
        if Result then Node.Free;
      end;
  else
    Result := False;
  end;
end;

function TMpDomNode.Extract(aIndex: SizeInt; out aNode: TMpDomNode): Boolean;
begin
  case Kind of
    mnkArray:  Result := GetArrayRef^.Extract(aIndex, aNode);
    mnkMap:    Result := GetMapRef^.Extract(aIndex, aNode);
  else
    aNode := nil;
    Result := False;
  end;
end;

function TMpDomNode.AddNil(const aKey: TMpVariant): TMpDomNode;
begin
  Result := AsMap;
  Result.GetMapRef^.Add(aKey, TMpDomNode.Create);
end;

function TMpDomNode.Add(const aKey: TMpVariant; aValue: Boolean): TMpDomNode;
begin
  AsMap.GetMapRef^.Add(aKey, TMpDomNode.Make(aValue));
  Result := Self;
end;

function TMpDomNode.AddI(const aKey: TMpVariant; aValue: Int64): TMpDomNode;
begin
  AsMap.GetMapRef^.Add(aKey, TMpDomNode.MakeI(aValue));
  Result := Self;
end;

function TMpDomNode.AddS(const aKey: TMpVariant; aValue: Single): TMpDomNode;
begin
  AsMap.GetMapRef^.Add(aKey, TMpDomNode.MakeS(aValue));
  Result := Self;
end;

function TMpDomNode.AddD(const aKey: TMpVariant; aValue: Double): TMpDomNode;
begin
  AsMap.GetMapRef^.Add(aKey, TMpDomNode.MakeD(aValue));
  Result := Self;
end;

function TMpDomNode.Add(const aKey: TMpVariant; const s: string): TMpDomNode;
begin
  AsMap.GetMapRef^.Add(aKey, TMpDomNode.Make(s));
  Result := Self;
end;

function TMpDomNode.Add(const aKey: TMpVariant; const a: array of Byte): TMpDomNode;
begin
  AsMap.GetMapRef^.Add(aKey, TMpDomNode.Make(a));
  Result := Self;
end;

function TMpDomNode.AddB(const aKey: TMpVariant; const b: TBytes): TMpDomNode;
begin
  AsMap.GetMapRef^.Add(aKey, TMpDomNode.MakeB(b));
  Result := Self;
end;

function TMpDomNode.Add(const aKey: TMpVariant; const ts: TMpTimeStamp): TMpDomNode;
begin
  AsMap.GetMapRef^.Add(aKey, TMpDomNode.Make(ts));
  Result := Self;
end;

function TMpDomNode.AddNode(const aKey: TMpVariant): TMpDomNode;
begin
  Result := TMpDomNode.Create;
  AsMap.GetMapRef^.Add(aKey, Result);
end;

function TMpDomNode.TryAddNil(const aKey: TMpVariant): Boolean;
var
  p: ^TPair;
begin
  Result := AsMap.GetMapRef^.AddUniq(aKey, p);
  if Result then p^.Value := TMpDomNode.Create;
end;

function TMpDomNode.TryAdd(const aKey: TMpVariant; aValue: Boolean): Boolean;
var
  p: ^TPair;
begin
  Result := AsMap.GetMapRef^.AddUniq(aKey, p);
  if Result then p^.Value := TMpDomNode.Make(aValue);
end;

function TMpDomNode.TryAddI(const aKey: TMpVariant; aValue: Int64): Boolean;
var
  p: ^TPair;
begin
  Result := AsMap.GetMapRef^.AddUniq(aKey, p);
  if Result then p^.Value := TMpDomNode.MakeI(aValue);
end;

function TMpDomNode.TryAddS(const aKey: TMpVariant; aValue: Single): Boolean;
var
  p: ^TPair;
begin
  Result := AsMap.GetMapRef^.AddUniq(aKey, p);
  if Result then p^.Value := TMpDomNode.MakeS(aValue);
end;

function TMpDomNode.TryAddD(const aKey: TMpVariant; aValue: Double): Boolean;
var
  p: ^TPair;
begin
  Result := AsMap.GetMapRef^.AddUniq(aKey, p);
  if Result then p^.Value := TMpDomNode.MakeD(aValue);
end;

function TMpDomNode.TryAdd(const aKey: TMpVariant; const s: string): Boolean;
var
  p: ^TPair;
begin
  Result := AsMap.GetMapRef^.AddUniq(aKey, p);
  if Result then p^.Value := TMpDomNode.Make(s);
end;

function TMpDomNode.TryAdd(const aKey: TMpVariant; const a: array of Byte): Boolean;
var
  p: ^TPair;
begin
  Result := AsMap.GetMapRef^.AddUniq(aKey, p);
  if Result then p^.Value := TMpDomNode.Make(a);
end;

function TMpDomNode.TryAddB(const aKey: TMpVariant; const b: TBytes): Boolean;
var
  p: ^TPair;
begin
  Result := AsMap.GetMapRef^.AddUniq(aKey, p);
  if Result then p^.Value := TMpDomNode.MakeB(b);
end;

function TMpDomNode.TryAdd(const aKey: TMpVariant; const ts: TMpTimeStamp): Boolean;
var
  p: ^TPair;
begin
  Result := AsMap.GetMapRef^.AddUniq(aKey, p);
  if Result then p^.Value := TMpDomNode.Make(ts);
end;

function TMpDomNode.TryAddNode(const aKey: TMpVariant; out aNode: TMpDomNode): Boolean;
var
  p: ^TPair;
begin
  aNode := nil;
  Result := AsMap.GetMapRef^.AddUniq(aKey, p);
  if Result then
    begin
      aNode := TMpDomNode.Create;
      p^.Value := aNode;
    end;
end;

function TMpDomNode.Contains(const aKey: TMpVariant): Boolean;
begin
  if Kind <> mnkMap then exit(False);
  Result := GetMapRef^.Contains(aKey);
end;

function TMpDomNode.ContainsUniq(const aKey: TMpVariant): Boolean;
begin
  if Kind <> mnkMap then exit(False);
  Result := GetMapRef^.ContainsUniq(aKey);
end;

function TMpDomNode.IndexOf(const aKey: TMpVariant): SizeInt;
begin
  if Kind <> mnkMap then exit(NULL_INDEX);
  Result := GetMapRef^.IndexOf(aKey);
end;

function TMpDomNode.CountOf(const aKey: TMpVariant): SizeInt;
begin
  if Kind <> mnkMap then exit(0);
  Result := GetMapRef^.CountOf(aKey);
end;

function TMpDomNode.Find(aIndex: SizeInt; out aValue: TMpDomNode): Boolean;
begin
  if SizeUInt(aIndex) < SizeUInt(Count) then
    begin
      case Kind of
        mnkArray: aValue := GetArrayRef^.UncItems[aIndex];
        mnkMap:   aValue := GetMapRef^.UncMutPairs[aIndex]^.Value;
      else
      end;
      exit(True);
    end;
  aValue := nil;
  Result := False;
end;

function TMpDomNode.Find(const aKey: TMpVariant; out aValue: TMpDomNode): Boolean;
var
  p: ^TPair;
begin
  if Kind = mnkMap then
    begin
      p := GetMapRef^.Find(aKey);
      if p <> nil then
        begin
          aValue := p^.Value;
          exit(True);
        end;
    end;
  aValue := nil;
  Result := False;
end;

function TMpDomNode.FindUniq(const aKey: TMpVariant; out aValue: TMpDomNode): Boolean;
var
  p: ^TPair;
begin
  aValue := nil;
  if Kind <> mnkMap then exit(False);
  p := GetMapRef^.FindUniq(aKey);
  Result := p <> nil;
  if Result then aValue := p^.Value;
end;

function TMpDomNode.FindPair(aIndex: SizeInt; out aValue: TPair): Boolean;
begin
  if (Kind = mnkMap) and (SizeUInt(aIndex) < SizeUInt(GetMapRef^.Count)) then
     begin
       aValue := GetMapRef^.UncPairs[aIndex];
       exit(True);
     end;
  aValue := Default(TPair);
  Result := False;
end;

function TMpDomNode.FindOrAdd(const aKey: TMpVariant; out aValue: TMpDomNode): Boolean;
var
  p: ^TPair;
begin
  Result := AsMap.GetMapRef^.FindOrAdd(aKey, p);
  if not Result then
    p^.Value := TMpDomNode.Create;
  aValue := p^.Value;
end;

function TMpDomNode.FindPath(const aPath: array of TMpVariant; out aNode: TMpDomNode): Boolean;
var
  Node: TMpDomNode;
  I: SizeInt;
  Idx: Int64;
begin
  if System.Length(aPath) = 0 then begin
    aNode := Self;
    exit(True);
  end;
  aNode := nil;
  Node := Self;
  for I := 0 to System.High(aPath) do
    if Node.IsArray then
      begin
        if aPath[I].Kind = mvkInt then
          Idx := aPath[I].AsInt
        else
          exit(False);
        if QWord(Idx) >= QWord(Node.GetArrayRef^.Count) then exit(False);
        Node := Node.GetArrayRef^.UncItems[Idx];
      end
    else
      if not Node.FindUniq(aPath[I], Node) then exit(False);
  aNode := Node;
  Result := True;
end;

function TMpDomNode.FindPath(const aPath: array of string; out aNode: TMpDomNode): Boolean;
var
  Node: TMpDomNode;
  I, Idx: SizeInt;
begin
  if System.Length(aPath) = 0 then begin
    aNode := Self;
    exit(True);
  end;
  aNode := nil;
  Node := Self;
  for I := 0 to System.High(aPath) do
    if Node.IsArray then begin
      if not IsNonNegativeInt(aPath[I], Idx) then exit(False);
      if not Node.Find(Idx, Node) then exit(False);
    end else
      if not Node.FindUniq(aPath[I], Node) then exit(False);
  aNode := Node;
  Result := True;
end;

function TMpDomNode.FindPathPtr(const aPtr: string; out aNode: TMpDomNode): Boolean;
var
  Segments: TStringArray = nil;
begin
  if aPtr = '' then begin
    aNode := Self;
    exit(True);
  end;
  if not TJsonPtr.TryGetSegments(aPtr, Segments) then begin
    aNode := nil;
    exit(False);
  end;
  Result := FindPath(Segments, aNode);
end;

function TMpDomNode.Extract(const aKey: TMpVariant; out aNode: TMpDomNode): Boolean;
begin
  if Kind <> mnkMap then
    begin
      aNode := nil;
      exit(False);
    end;
  Result := GetMapRef^.Extract(aKey, aNode);
end;

function TMpDomNode.Remove(const aKey: TMpVariant): Boolean;
var
  Node: TMpDomNode;
begin
  Result := Extract(aKey, Node);
  if Result then Node.Free;
end;

function TMpDomNode.RemoveAll(const aKey: TMpVariant): SizeInt;
begin
  Result := Count;
  while Remove(aKey) do;
  Result -= Count;
end;

function TMpDomNode.Sort(aCompare: specialize TGLessCompare<TMpDomNode>): Boolean;
begin
  Result := IsArray;
  if Result and GetArrayRef^.NonEmpty then
    specialize TGRegularArrayHelper<TMpDomNode>
    .Sort(GetArrayRef^.List[0..Pred(GetArrayRef^.Count)], aCompare);
end;

function TMpDomNode.Sort(aCompare: specialize TGOnLessCompare<TMpDomNode>): Boolean;
begin
  Result := IsArray;
  if Result and GetArrayRef^.NonEmpty then
    specialize TGDelegatedArrayHelper<TMpDomNode>
    .Sort(GetArrayRef^.List[0..Pred(GetArrayRef^.Count)], aCompare);
end;

function TMpDomNode.Sort(aCompare: specialize TGNestLessCompare<TMpDomNode>): Boolean;
begin
  Result := IsArray;
  if Result and GetArrayRef^.NonEmpty then
    specialize TGNestedArrayHelper<TMpDomNode>
    .Sort(GetArrayRef^.List[0..Pred(GetArrayRef^.Count)], aCompare);
end;

function TMpDomNode.Sort(aCompare: specialize TGLessCompare<TPair>): Boolean;
begin
  Result := IsMap;
  if Result then GetMapRef^.Sort(aCompare);
end;

function TMpDomNode.Sort(aCompare: specialize TGOnLessCompare<TPair>): Boolean;
begin
  Result := IsMap;
  if Result then GetMapRef^.Sort(aCompare);
end;

function TMpDomNode.Sort(aCompare: specialize TGNestLessCompare<TPair>): Boolean;
begin
  Result := IsMap;
  if Result then GetMapRef^.Sort(aCompare);
end;

function TMpDomNode.SaveToStream(aStream: TStream): SizeInt;
var
  Writer: TMpWriter;
begin
  Writer := TMpWriter.Create;
  try
    TMpWriter.WriteDom(Self, Writer);
    Result := Writer.SaveToStream(aStream);
  finally
    Writer.Free;
  end;
end;

function TMpDomNode.SaveToFile(const aFileName: string): SizeInt;
var
  fs: TFileStream;
begin
  fs := TFileStream.Create(aFileName, fmCreate);
  try
    Result := SaveToStream(fs);
  finally
    fs.Free;
  end;
end;

{ TMpWriter }

procedure TMpWriter.EnsureCapacity(aValue: SizeInt);
begin
  if aValue > FBuffer.Length then
    FBuffer.Length := LgUtils.RoundUpTwoPower(aValue);
end;

procedure TMpWriter.DoWrite(b: Byte);
begin
  EnsureCapacity(Succ(TotalBytes));
  FBuffer.Ptr[TotalBytes] := b;
  Inc(FCount);
end;

procedure TMpWriter.DoWrite2(b1, b2: Byte);
begin
  EnsureCapacity(TotalBytes + 2);
  FBuffer.Ptr[TotalBytes] := b1;
  FBuffer.Ptr[TotalBytes + 1] := b2;
  Inc(FCount, 2);
end;

procedure TMpWriter.DoWrite3(b1, b2, b3: Byte);
begin
  EnsureCapacity(TotalBytes + 3);
  FBuffer.Ptr[TotalBytes] := b1;
  FBuffer.Ptr[TotalBytes + 1] := b2;
  FBuffer.Ptr[TotalBytes + 2] := b3;
  Inc(FCount, 3);
end;

procedure TMpWriter.DoWrite(w: Word);
begin
  EnsureCapacity(TotalBytes + 2);
  PWord(Unaligned(@FBuffer.Ptr[TotalBytes]))^ := w;
  Inc(FCount, 2);
end;

procedure TMpWriter.DoWrite(d: DWord);
begin
  EnsureCapacity(TotalBytes + 4);
  PDWord(Unaligned(@FBuffer.Ptr[TotalBytes]))^ := d;
  Inc(FCount, 4);
end;

procedure TMpWriter.DoWrite(q: QWord);
begin
  EnsureCapacity(TotalBytes + 8);
  PQWord(Unaligned(@FBuffer.Ptr[TotalBytes]))^ := q;
  Inc(FCount, 8);
end;

procedure TMpWriter.DoWrite(p: PByte; aCount: SizeInt);
begin
  if aCount <= 0 then exit;
  EnsureCapacity(TotalBytes + aCount);
  System.Move(p^, FBuffer.Ptr[TotalBytes], aCount);
  Inc(FCount, aCount);
end;

procedure TMpWriter.DoError(const aMsg: string);
begin
  raise EMpWrite.Create(aMsg);
end;

procedure TMpWriter.DoError(const aFmt: string; const aParams: array of const);
begin
  raise EMpWrite.CreateFmt(aFmt, aParams);
end;

class procedure TMpWriter.WriteDom(aNode: TMpDomNode; aWriter: TMpWriter);
  procedure WriteNode(aNode: TMpDomNode);
  var
    I, Cnt: SizeInt;
  begin
    case aNode.Kind of
      mnkNil:    aWriter.AddNil;
      mnkBool:   aWriter.Add(aNode.AsBoolean);
      mnkInt:    aWriter.Add(aNode.AsInteger);
      mnkSingle: aWriter.Add(aNode.AsSingle);
      mnkDouble: aWriter.Add(aNode.AsDouble);
      mnkString: aWriter.Add(aNode.AsString);
      mnkBin:    aWriter.Add(aNode.AsBinary);
      mnkArray:
        begin
          Cnt := aNode.Count;
          aWriter.BeginArray(Cnt);
          for I := 0 to Pred(Cnt) do
            WriteNode(aNode.GetArrayRef^.UncItems[I]);
        end;
      mnkMap:
        begin
          Cnt := aNode.Count;
          aWriter.BeginMap(Cnt);
          for I := 0 to Pred(Cnt) do
            with aNode.GetMapRef^.UncMutPairs[I]^ do begin
              aWriter.Add(Key);
              WriteNode(Value);
            end;
        end;
      mnkTStamp: aWriter.Add(aNode.AsTimeStamp);
      mnkExt:    aWriter.Add(aNode.AsExtention);
    end;
  end;
begin
  WriteNode(aNode);
end;

class function TMpWriter.DomToRawStr(aNode: TMpDomNode): rawbytestring;
var
  Writer: TMpWriter;
begin
  Writer := TMpWriter.Create;
  try
    WriteDom(aNode, Writer);
    Result := Writer.ToStrRaw;
  finally
    Writer.Free;
  end;
end;

class function TMpWriter.DomToBytes(aNode: TMpDomNode): TBytes;
var
  Writer: TMpWriter;
begin
  Writer := TMpWriter.Create;
  try
    WriteDom(aNode, Writer);
    Result := Writer.ToBytes;
  finally
    Writer.Free;
  end;
end;

constructor TMpWriter.Create;
begin
  inherited;
  FBuffer.Length := INIT_BUF_LEN;
end;

function TMpWriter.AddNil: TMpWriter;
begin
  DoWrite(Byte($c0));
  Result := Self;
end;

function TMpWriter.Add(aValue: Boolean): TMpWriter;
begin
  if aValue then
    DoWrite(Byte($c3))
  else
    DoWrite(Byte($c2));
  Result := Self;
end;

function TMpWriter.Add(aValue: Int64): TMpWriter;
begin
  case aValue of
    System.Low(Int64)..System.Low(Int32)-1:
      begin
        DoWrite(Byte($d3));
        DoWrite(System.NToBE(QWord(aValue)));
      end;
    System.Low(Int32)..System.Low(Int16)-1:
      begin
        DoWrite(Byte($d2));
        DoWrite(System.NToBE(DWord(Int32(aValue))));
      end;
    System.Low(Int16)..System.Low(Int8)-1:
      begin
        DoWrite(Byte($d1));
        DoWrite(System.NToBE(Word(Int16(aValue))));
      end;
    System.Low(Int8)..-33:
      begin
        DoWrite(Byte($d0));
        DoWrite(Byte(Int8(aValue)));
      end;
    -32..-1:
      DoWrite(Byte(ShortInt(aValue)));
    $0..$7f:
      DoWrite(Byte(aValue));
    $80..$ff:
      begin
        DoWrite(Byte($cc));
        DoWrite(Byte(aValue));
      end;
    $100..$ffff:
      begin
        DoWrite(Byte($cd));
        DoWrite(System.NToBE(Word(aValue)));
      end;
    $10000..Int64($ffffffff):
      begin
        DoWrite(Byte($ce));
        DoWrite(System.NToBE(DWord(aValue)));
      end;
  else
    DoWrite(Byte($cf));
    DoWrite(System.NToBE(QWord(aValue)));
  end;
  Result := Self;
end;

function TMpWriter.Add(aValue: Single): TMpWriter;
begin
  DoWrite(Byte($ca));
  DoWrite(System.NToBE(DWord(aValue)));
  Result := Self;
end;

function TMpWriter.Add(aValue: Double): TMpWriter;
begin
  DoWrite(Byte($cb));
  DoWrite(System.NToBE(QWord(aValue)));
  Result := Self;
end;

function TMpWriter.Add(const s: string): TMpWriter;
var
  Len: SizeInt;
begin
  Len := System.Length(s);
  case Len of
    0..$1f:
      begin
        DoWrite(Byte($a0) or Byte(Len));
        DoWrite(PByte(s), Len);
      end;
    $20..$ff:
      begin
        DoWrite(Byte($d9));
        DoWrite(Byte(Len));
        DoWrite(PByte(s), Len);
      end;
    $100..$ffff:
      begin
        DoWrite(Byte($da));
        DoWrite(System.NToBE(Word(Len)));
        DoWrite(PByte(s), Len);
      end;
  else
{$IFDEF CPU64}
    if Len <= System.High(DWord) then begin
      DoWrite(Byte($db));
      DoWrite(System.NToBE(DWord(Len)));
      DoWrite(PByte(s), Len);
    end else
      DoError(SEMpStrLenExceedsLimitFmt, [Len]);
{$ELSE}
    DoWrite(Byte($db));
    DoWrite(System.NToBE(DWord(Len)));
    DoWrite(PByte(s), Len);
{$ENDIF}
  end;
  Result := Self;
end;

function TMpWriter.Add(const s: shortstring): TMpWriter;
var
  Len: SizeInt;
begin
  Len := System.Length(s);
  if Len <= $1f then begin
    DoWrite(Byte($a0) or Byte(Len));
    if Len <> 0 then
      DoWrite(PByte(@s[1]), Len);
  end else begin
    DoWrite(Byte($d9));
    DoWrite(Byte(Len));
    DoWrite(PByte(@s[1]), Len);
  end;
  Result := Self;
end;

function TMpWriter.Add(const a: array of Byte): TMpWriter;
var
  Len: SizeInt;
begin
  Len := System.Length(a);
  case Len of
    0..$ff:
      begin
        DoWrite(Byte($c4));
        DoWrite(Byte(Len));
        if Len <> 0 then
          DoWrite(PByte(@a[0]), Len);
      end;
    $100..$ffff:
      begin
        DoWrite(Byte($c5));
        DoWrite(System.NToBE(Word(Len)));
        DoWrite(PByte(@a[0]), Len);
      end;
  else
{$IFDEF CPU64}
    if Len <= System.High(DWord) then begin
      DoWrite(Byte($c6));
      DoWrite(System.NToBE(DWord(Len)));
      DoWrite(PByte(@a[0]), Len);
    end else
      DoError(SEMpBinLenExceedsLimitFmt, [Len]);
{$ELSE}
    DoWrite(Byte($c6));
    DoWrite(System.NToBE(DWord(Len)));
    DoWrite(PByte(@a[0]), Len);
{$ENDIF}
  end;
  Result := Self;
end;

function TMpWriter.Add(const ts: TMpTimeStamp): TMpWriter;
begin
  if (ts.Seconds shr 32 = 0) and (ts.NanoSeconds = 0) then begin
    DoWrite2(Byte($d6), Byte(-1));
    DoWrite(System.NToBE(DWord(ts.Seconds)));
  end else
    if ts.Seconds shr 34 = 0 then begin
      DoWrite2(Byte($d7), Byte(-1));
      DoWrite(System.NToBE(QWord(ts.NanoSeconds) shl 34 or QWord(ts.Seconds)));
    end else begin
      DoWrite3(Byte($c7), Byte(12), Byte(-1));
      DoWrite(System.NToBE(ts.NanoSeconds));
      DoWrite(System.NToBE(QWord(ts.Seconds)));
    end;
  Result := Self;
end;

function TMpWriter.Add(const v: TMpVariant): TMpWriter;
begin
  case v.Kind of
    mvkNull: AddNil;
    mvkInt:  Add(v.AsInt);
    mvkStr:  Add(v.AsString);
    mvkBin:  Add(v.AsBinary);
  end;
  Result := Self;
end;

function TMpWriter.AddExt(aType: TMpExtType; const aBytes: array of Byte): TMpWriter;
var
  Len: SizeInt;
begin
  Len := System.Length(aBytes);
  case Len of
    1:  DoWrite2(Byte($d4), Byte(aType));
    2:  DoWrite2(Byte($d5), Byte(aType));
    4:  DoWrite2(Byte($d6), Byte(aType));
    8:  DoWrite2(Byte($d7), Byte(aType));
    16: DoWrite2(Byte($d8), Byte(aType));
  else
    if Len <= System.High(Byte) then
      DoWrite3(Byte($c7), Byte(Len), Byte(aType))
    else
      if Len <= System.High(Word) then begin
        DoWrite(Byte($c8));
        DoWrite(System.NToBE(Word(Len)));
        DoWrite(Byte(aType));
      end else
{$IFDEF CPU64}
        if Len <= System.High(DWord) then begin
          DoWrite(Byte($c9));
          DoWrite(System.NToBE(DWord(Len)));
          DoWrite(Byte(aType));
        end else
          DoError(SEMpExtLenExceedsLimitFmt, [Len]);
{$ELSE}
        begin
          DoWrite(Byte($c9));
          DoWrite(System.NToBE(DWord(Len)));
          DoWrite(Byte(aType));
        end;
{$ENDIF}
  end;
  if Len > 0 then
    DoWrite(PByte(@aBytes[0]), Len);
  Result := Self;
end;

function TMpWriter.AddExt(aType: TMpExtType; const aBuffer; aCount: SizeInt): TMpWriter;
begin
  Result := AddExt(aType, PByte(@aBuffer)[0..Pred(aCount)]);
end;

function TMpWriter.BeginArray(aCount: SizeInt): TMpWriter;
begin
  if aCount < 0 then DoError(SEMpInvalidArraySizeFmt, [aCount]);
  if aCount <= $0f then
    DoWrite(Byte($90) or Byte(aCount))
  else
    if aCount <= System.High(Word) then begin
      DoWrite(Byte($dc));
      DoWrite(System.NToBE(Word(aCount)));
    end else
{$IFDEF CPU64}
      if aCount <= System.High(DWord) then begin
        DoWrite(Byte($dd));
        DoWrite(System.NToBE(DWord(aCount)));
      end else
        DoError(SEMpArrLenExceedsLimitFmt, [aCount]);
{$ELSE}
      begin
        DoWrite(Byte($dd));
        DoWrite(System.NToBE(DWord(aCount)));
      end;
{$ENDIF}
  Result := Self;
end;

function TMpWriter.BeginMap(aCount: SizeInt): TMpWriter;
begin
  if aCount < 0 then DoError(SEMpInvalidMapSizeFmt, [aCount]);
  if aCount <= $0f then
    DoWrite(Byte($80) or Byte(aCount))
  else
    if aCount <= System.High(Word) then begin
      DoWrite(Byte($de));
      DoWrite(System.NToBE(Word(aCount)));
    end else
{$IFDEF CPU64}
      if aCount <= System.High(DWord) then begin
        DoWrite(Byte($df));
        DoWrite(System.NToBE(DWord(aCount)));
      end else
        DoError(SEMpMapLenExceedsLimitFmt, [aCount]);
{$ELSE}
      begin
        DoWrite(Byte($df));
        DoWrite(System.NToBE(DWord(aCount)));
      end;
{$ENDIF}
  Result := Self;
end;

procedure TMpWriter.Reset;
begin
  FCount := 0;
end;

function TMpWriter.ToStrRaw: rawbytestring;
begin
  Result := '';
  System.SetLength(Result, TotalBytes);
  System.Move(FBuffer.Ptr^, Pointer(Result)^, TotalBytes);
end;

function TMpWriter.ToBytes: TBytes;
begin
  Result := nil;
  System.SetLength(Result, TotalBytes);
  System.Move(FBuffer.Ptr^, Pointer(Result)^, TotalBytes);
end;

function TMpWriter.SaveToStream(aStream: TStream): SizeInt;
begin
  Result := TotalBytes;
  aStream.WriteBuffer(FBuffer.Ptr^, Result);
end;

{ TMpReader.TNode }

constructor TMpReader.TNode.Make(aCount: Int64; aStruct: TMpStructKind);
begin
  Count := aCount;
  Struct := aStruct;
end;

{ TMpReader }

function TMpReader.GetStruct: TMpStructKind;
begin
  Result := FStack[StackTop].Struct;
end;

function TMpReader.TryPushStack(aCount: Int64; aStruct: TMpStructKind): Boolean;
begin
  if StackTop = System.High(FStack) then begin
    FState := mrsError;
    exit(False);
  end;
  Inc(FStackTop);
  FStack[StackTop].Make(aCount, aStruct);
  Result := True;
end;

procedure TMpReader.TermDone;
begin
  if StackTop <> 0 then Dec(FStack[StackTop].Count);
end;

procedure TMpReader.PopStack;
begin
  Dec(FStackTop);
  TermDone;
end;

function TMpReader.GetMaxDepth: SizeInt;
begin
  Result := System.High(FStack);
end;

function TMpReader.DoReadByte(out b: Byte): Boolean;
begin
  Result := FBufPos < FBufSize;
  if Result then begin
    b := FBuffer[FBufPos];
    Inc(FBufPos);
  end;
end;

function TMpReader.DoSkipByte: Boolean;
begin
  Result := FBufPos < FBufSize;
  if Result then Inc(FBufPos);
end;

function TMpReader.DoReadWord(out w: Word): Boolean;
begin
  Result := FBufPos <= FBufSize - SizeOf(Word);
  if Result then begin
    w := System.BEtoN(PWord(Unaligned(@FBuffer[FBufPos]))^);
    Inc(FBufPos, SizeOf(Word));
  end;
end;

function TMpReader.DoSkipWord: Boolean;
begin
  Result := FBufPos <= FBufSize - SizeOf(Word);
  if Result then Inc(FBufPos, SizeOf(Word));
end;

function TMpReader.DoReadDWord(out d: DWord): Boolean;
begin
  Result := FBufPos <= FBufSize - SizeOf(DWord);
  if Result then begin
    d := System.BEtoN(PDWord(Unaligned(@FBuffer[FBufPos]))^);
    Inc(FBufPos, SizeOf(DWord));
  end;
end;

function TMpReader.DoSkipDWord: Boolean;
begin
  Result := FBufPos <= FBufSize - SizeOf(DWord);
  if Result then Inc(FBufPos, SizeOf(DWord));
end;

function TMpReader.DoReadQWord(out q: QWord): Boolean;
begin
  Result := FBufPos <= FBufSize - SizeOf(QWord);
  if Result then begin
    q := System.BEtoN(PQWord(Unaligned(@FBuffer[FBufPos]))^);
    Inc(FBufPos, SizeOf(QWord));
  end;
end;

function TMpReader.DoSkipQWord: Boolean;
begin
  Result := FBufPos <= FBufSize - SizeOf(QWord);
  if Result then Inc(FBufPos, SizeOf(QWord));
end;

function TMpReader.DoReadSingle(out s: Single): Boolean;
var
  d: DWord absolute s;
begin
  Result := DoReadDWord(d);
end;

function TMpReader.DoSkipSingle: Boolean;
begin
  Result := FBufPos <= FBufSize - SizeOf(Single);
  if Result then Inc(FBufPos, SizeOf(Single));
end;

function TMpReader.DoReadDouble(out d: Double): Boolean;
var
  q: QWord absolute d;
begin
  Result := DoReadQWord(q);
end;

function TMpReader.DoSkipDouble: Boolean;
begin
  Result := FBufPos <= FBufSize - SizeOf(Double);
  if Result then Inc(FBufPos, SizeOf(Double));
end;

function TMpReader.DoReadString(aLen: SizeInt; out s: string): Boolean;
begin
  if FBufPos > FBufSize - aLen then exit(False);
  System.SetLength(s, aLen);
  System.Move(FBuffer[FBufPos], Pointer(s)^, aLen);
  Inc(FBufPos, aLen);
  Result := True;
end;

function TMpReader.DoReadBytes(aLen: SizeInt; out b: TBytes): Boolean;
begin
  if FBufPos > FBufSize - aLen then exit(False);
  System.SetLength(b, aLen);
  System.Move(FBuffer[FBufPos], Pointer(b)^, aLen);
  Inc(FBufPos, aLen);
  Result := True;
end;

function TMpReader.DoSkipBytes(aLen: SizeInt): Boolean;
begin
  if FBufPos > FBufSize - aLen then exit(False);
  Inc(FBufPos, aLen);
  Result := True;
end;

function TMpReader.GetCurrUnread: Int64;
begin
  Result := FStack[StackTop].Count;
end;

procedure TMpReader.DoReadFixInt(aPfx: Byte);
begin
  FInt := Int8(aPfx);
  FCurrToken := mtkInt;
  TermDone;
end;

procedure TMpReader.DoReadNegFixInt(aPfx: Byte);
begin
  FInt := Int8(aPfx);
  FCurrToken := mtkInt;
  TermDone;
end;

procedure TMpReader.DoReadNil;
begin
  FCurrToken := mtkNil;
  TermDone;
end;

procedure TMpReader.DoReadBool(aValue: Boolean);
begin
  FBool := aValue;
  FCurrToken := mtkBool;
  TermDone;
end;

function TMpReader.DoBeginArray(aLen: SizeInt): Boolean;
begin
  Result := TryPushStack(aLen, mskArray);
  if Result then FCurrToken := mtkArrayBegin;
end;

function TMpReader.DoBeginMap(aLen: SizeInt): Boolean;
begin
  Result := TryPushStack(aLen, mskMap);
  if Result then FCurrToken := mtkMapBegin;
end;

function TMpReader.DoReadBin(aLen: SizeInt): Boolean;
begin
  if ReadMode then
    Result := DoReadBytes(aLen, FBytes)
  else
    Result := DoSkipBytes(aLen);
  if Result then begin
    FCurrToken := mtkBin;
    TermDone;
  end;
end;

function TMpReader.DoReadExt(aLen: SizeInt): Boolean;
begin
  if ReadMode then
    Result := DoReadBytes(aLen, FBytes)
  else
    Result := DoSkipBytes(aLen);
  if Result then begin
    FCurrToken := mtkExt;
    TermDone;
  end;
end;

function TMpReader.DoReadBinKey(aLen: SizeInt): Boolean;
var
  b: TBytes;
begin
  if ReadMode then begin
    Result := DoReadBytes(aLen, b);
    if Result then FKey := b;
  end else
    Result := DoSkipBytes(aLen);
end;

function TMpReader.DoReadStr(aLen: SizeInt): Boolean;
begin
  if ReadMode then
    Result := DoReadString(aLen, FString)
  else
    Result := DoSkipBytes(aLen);
  if Result then begin
    FCurrToken := mtkString;
    TermDone;
  end;
end;

function TMpReader.DoReadStrKey(aLen: SizeInt): Boolean;
var
  s: string;
begin
  if ReadMode then begin
    Result := DoReadString(aLen, s);
    if Result then FKey := s;
  end else
    Result := DoSkipBytes(aLen);
end;

function TMpReader.DoReadBin8: Boolean;
var
  Len: Byte;
begin
  if not DoReadByte(Len) then exit(False);
  Result := DoReadBin(SizeInt(Len));
end;

function TMpReader.DoReadBin16: Boolean;
var
  Len: Word;
begin
  if not DoReadWord(Len) then exit(False);
  Result := DoReadBin(SizeInt(Len));
end;

function TMpReader.DoReadBin32: Boolean;
var
  Len: DWord;
begin
  if not DoReadDWord(Len) then exit(False);
  {$IFNDEF CPU64}if Len > System.High(SizeInt) then exit(False);{$ENDIF}
  Result := DoReadBin(SizeInt(Len));
end;

function TMpReader.DoReadBin8Key: Boolean;
var
  Len: Byte;
begin
  if not DoReadByte(Len) then exit(False);
  Result := DoReadBinKey(SizeInt(Len));
end;

function TMpReader.DoReadBin16Key: Boolean;
var
  Len: Word;
begin
  if not DoReadWord(Len) then exit(False);
  Result := DoReadBinKey(SizeInt(Len));
end;

function TMpReader.DoReadBin32Key: Boolean;
var
  Len: DWord;
begin
  if not DoReadDWord(Len) then exit(False);
  {$IFNDEF CPU64}if Len > System.High(SizeInt) then exit(False);{$ENDIF}
  Result := DoReadBinKey(SizeInt(Len));
end;

function TMpReader.DoReadUInt8Key: Boolean;
var
  b: Byte;
begin
  if ReadMode then begin
    Result := DoReadByte(b);
    if Result then FKey := b;
  end else
    Result := DoSkipByte;
end;

function TMpReader.DoReadUInt16Key: Boolean;
var
  w: Word;
begin
  if ReadMode then begin
    Result := DoReadWord(w);
    if Result then FKey := w;
  end else
    Result := DoSkipWord;
end;

function TMpReader.DoReadUInt32Key: Boolean;
var
  d: DWord;
begin
  if ReadMode then begin
    Result := DoReadDWord(d);
    if Result then FKey := d;
  end else
    Result := DoSkipDWord;
end;

function TMpReader.DoReadUInt64Key: Boolean;
var
  q: QWord;
begin
  if ReadMode then begin
    Result := DoReadQWord(q);
    if Result then FKey := q;
  end else
    Result := DoSkipQWord;
end;

function TMpReader.DoReadInt8Key: Boolean;
var
  b: Byte;
begin
  if ReadMode then begin
    Result := DoReadByte(b);
    if Result then FKey := Int8(b);
  end else
    Result := DoSkipByte;
end;

function TMpReader.DoReadInt16Key: Boolean;
var
  w: Word;
begin
  if ReadMode then begin
    Result := DoReadWord(w);
    if Result then FKey := Int16(w);
  end else
    Result := DoSkipWord;
end;

function TMpReader.DoReadInt32Key: Boolean;
var
  d: DWord;
begin
  if ReadMode then begin
    Result := DoReadDWord(d);
    if Result then FKey := Int32(d);
  end else
    Result := DoSkipDWord;
end;

function TMpReader.DoReadInt64Key: Boolean;
var
  q: QWord;
begin
  if ReadMode then begin
    Result := DoReadQWord(q);
    if Result then FKey := Int16(q);
  end else
    Result := DoSkipQWord;
end;

function TMpReader.DoReadStr8Key: Boolean;
var
  Len: Byte;
begin
  if not DoReadByte(Len) then exit(False);
  Result := DoReadStrKey(SizeInt(Len));
end;

function TMpReader.DoReadStr16Key: Boolean;
var
  Len: Word;
begin
  if not DoReadWord(Len) then exit(False);
  Result := DoReadStrKey(SizeInt(Len));
end;

function TMpReader.DoReadStr32Key: Boolean;
var
  Len: DWord;
begin
  if not DoReadDWord(Len) then exit(False);
  {$IFNDEF CPU64}if Len > System.High(SizeInt) then exit(False);{$ENDIF}
  Result := DoReadStrKey(SizeInt(Len));
end;

type
  TMsgPackFormat = (
    mpfFixInt,    // $00..$7f
    mpfFixMap,    // $80..$8f
    mpfFixArray,  // $90..$9f
    mpfFixStr,    // $a0..$bf
    mpfNil,       // $c0
    mpfUnused,    // $c1
    mpfFalse,     // $c2
    mpfTrue,      // $c3
    mpfBin8,      // $c4
    mpfBin16,     // $c5
    mpfBin32,     // $c6
    mpfExt8,      // $c7
    mpfExt16,     // $c8
    mpfExt32,     // $c9
    mpfFloat32,   // $ca
    mpfFloat64,   // $ca
    mpfUInt8,     // $cb
    mpfUInt16,    // $cc
    mpfUInt32,    // $ce
    mpfUInt64,    // $cf
    mpfInt8,      // $d0
    mpfInt16,     // $d1
    mpfInt32,     // $d2
    mpfInt64,     // $d3
    mpfFixExt1,   // $d4
    mpfFixExt2,   // $d5
    mpfFixExt4,   // $d6
    mpfFixExt8,   // $d7
    mpfFixExt16,  // $d8
    mpfStr8,      // $d9
    mpfStr16,     // $da
    mpfStr32,     // $db
    mpfArray16,   // $dc
    mpfArray32,   // $dd
    mpfMap16,     // $de
    mpfMap32,     // $df
    mpfNegFixInt  // $e0..$ff
  );

const
{$PUSH}{$J-}
  FORMAT_TBL: array[Byte] of TMsgPackFormat = (
    mpfFixInt,   mpfFixInt,   mpfFixInt,   mpfFixInt,   mpfFixInt,   mpfFixInt,   mpfFixInt,   mpfFixInt,
    mpfFixInt,   mpfFixInt,   mpfFixInt,   mpfFixInt,   mpfFixInt,   mpfFixInt,   mpfFixInt,   mpfFixInt,
    mpfFixInt,   mpfFixInt,   mpfFixInt,   mpfFixInt,   mpfFixInt,   mpfFixInt,   mpfFixInt,   mpfFixInt,
    mpfFixInt,   mpfFixInt,   mpfFixInt,   mpfFixInt,   mpfFixInt,   mpfFixInt,   mpfFixInt,   mpfFixInt,
    mpfFixInt,   mpfFixInt,   mpfFixInt,   mpfFixInt,   mpfFixInt,   mpfFixInt,   mpfFixInt,   mpfFixInt,
    mpfFixInt,   mpfFixInt,   mpfFixInt,   mpfFixInt,   mpfFixInt,   mpfFixInt,   mpfFixInt,   mpfFixInt,
    mpfFixInt,   mpfFixInt,   mpfFixInt,   mpfFixInt,   mpfFixInt,   mpfFixInt,   mpfFixInt,   mpfFixInt,
    mpfFixInt,   mpfFixInt,   mpfFixInt,   mpfFixInt,   mpfFixInt,   mpfFixInt,   mpfFixInt,   mpfFixInt,
    mpfFixInt,   mpfFixInt,   mpfFixInt,   mpfFixInt,   mpfFixInt,   mpfFixInt,   mpfFixInt,   mpfFixInt,
    mpfFixInt,   mpfFixInt,   mpfFixInt,   mpfFixInt,   mpfFixInt,   mpfFixInt,   mpfFixInt,   mpfFixInt,
    mpfFixInt,   mpfFixInt,   mpfFixInt,   mpfFixInt,   mpfFixInt,   mpfFixInt,   mpfFixInt,   mpfFixInt,
    mpfFixInt,   mpfFixInt,   mpfFixInt,   mpfFixInt,   mpfFixInt,   mpfFixInt,   mpfFixInt,   mpfFixInt,
    mpfFixInt,   mpfFixInt,   mpfFixInt,   mpfFixInt,   mpfFixInt,   mpfFixInt,   mpfFixInt,   mpfFixInt,
    mpfFixInt,   mpfFixInt,   mpfFixInt,   mpfFixInt,   mpfFixInt,   mpfFixInt,   mpfFixInt,   mpfFixInt,
    mpfFixInt,   mpfFixInt,   mpfFixInt,   mpfFixInt,   mpfFixInt,   mpfFixInt,   mpfFixInt,   mpfFixInt,
    mpfFixInt,   mpfFixInt,   mpfFixInt,   mpfFixInt,   mpfFixInt,   mpfFixInt,   mpfFixInt,   mpfFixInt,
    mpfFixMap,   mpfFixMap,   mpfFixMap,   mpfFixMap,   mpfFixMap,   mpfFixMap,   mpfFixMap,   mpfFixMap,
    mpfFixMap,   mpfFixMap,   mpfFixMap,   mpfFixMap,   mpfFixMap,   mpfFixMap,   mpfFixMap,   mpfFixMap,
    mpfFixArray, mpfFixArray, mpfFixArray, mpfFixArray, mpfFixArray, mpfFixArray, mpfFixArray, mpfFixArray,
    mpfFixArray, mpfFixArray, mpfFixArray, mpfFixArray, mpfFixArray, mpfFixArray, mpfFixArray, mpfFixArray,
    mpfFixStr,   mpfFixStr,   mpfFixStr,   mpfFixStr,   mpfFixStr,   mpfFixStr,   mpfFixStr,   mpfFixStr,
    mpfFixStr,   mpfFixStr,   mpfFixStr,   mpfFixStr,   mpfFixStr,   mpfFixStr,   mpfFixStr,   mpfFixStr,
    mpfFixStr,   mpfFixStr,   mpfFixStr,   mpfFixStr,   mpfFixStr,   mpfFixStr,   mpfFixStr,   mpfFixStr,
    mpfFixStr,   mpfFixStr,   mpfFixStr,   mpfFixStr,   mpfFixStr,   mpfFixStr,   mpfFixStr,   mpfFixStr,
    mpfNil,      mpfUnused,   mpfFalse,    mpfTrue,     mpfBin8,     mpfBin16,    mpfBin32,    mpfExt8,
    mpfExt16,    mpfExt32,    mpfFloat32,  mpfFloat64,  mpfUInt8,    mpfUInt16,   mpfUInt32,   mpfUInt64,
    mpfInt8,     mpfInt16,    mpfInt32,    mpfInt64,    mpfFixExt1,  mpfFixExt2,  mpfFixExt4,  mpfFixExt8,
    mpfFixExt16, mpfStr8,     mpfStr16,    mpfStr32,    mpfArray16,  mpfArray32,  mpfMap16,    mpfMap32,
    mpfNegFixInt,mpfNegFixInt,mpfNegFixInt,mpfNegFixInt,mpfNegFixInt,mpfNegFixInt,mpfNegFixInt,mpfNegFixInt,
    mpfNegFixInt,mpfNegFixInt,mpfNegFixInt,mpfNegFixInt,mpfNegFixInt,mpfNegFixInt,mpfNegFixInt,mpfNegFixInt,
    mpfNegFixInt,mpfNegFixInt,mpfNegFixInt,mpfNegFixInt,mpfNegFixInt,mpfNegFixInt,mpfNegFixInt,mpfNegFixInt,
    mpfNegFixInt,mpfNegFixInt,mpfNegFixInt,mpfNegFixInt,mpfNegFixInt,mpfNegFixInt,mpfNegFixInt,mpfNegFixInt
  );
{$POP}

function TMpReader.DoReadKey: Boolean;
var
  pfx: Byte;
begin
  if not DoReadByte(pfx) then exit(False);
  Result := True;
  case FORMAT_TBL[pfx] of
    mpfFixInt:    FKey := Int8(pfx);
    mpfFixStr:    Result := DoReadStrKey(pfx and $1f);
    mpfNil:       FKey.Clear;
    mpfBin8:      Result := DoReadBin8Key;
    mpfBin16:     Result := DoReadBin16Key;
    mpfBin32:     Result := DoReadBin32Key;
    mpfUInt8:     Result := DoReadUInt8Key;
    mpfUInt16:    Result := DoReadUInt16Key;
    mpfUInt32:    Result := DoReadUInt32Key;
    mpfUInt64:    Result := DoReadUInt64Key;
    mpfInt8:      Result := DoReadInt8Key;
    mpfInt16:     Result := DoReadInt16Key;
    mpfInt32:     Result := DoReadInt32Key;
    mpfInt64:     Result := DoReadInt64Key;
    mpfStr8:      Result := DoReadStr8Key;
    mpfStr16:     Result := DoReadStr16Key;
    mpfStr32:     Result := DoReadStr32Key;
    mpfNegFixInt: FKey := Int8(pfx);
  else
    Result := False;
  end;
end;

function TMpReader.DoReadTStamp32: Boolean;
var
  d: DWord;
  b: Byte;
begin
  if not DoReadByte(b) then exit(False);
  if TMpExtType(b) <> -1 then exit(False);
  Result := DoReadDWord(d);
  if Result then begin
    FTStamp := TMpTimeStamp.Make(Int64(d), 0);
    FCurrToken := mtkTStamp;
    TermDone;
  end;
end;

function TMpReader.DoReadTStamp64: Boolean;
var
  q: QWord;
  b: Byte;
begin
  if not DoReadByte(b) then exit(False);
  if TMpExtType(b) <> -1 then exit(False);
  Result := DoReadQWord(q);
  if Result then begin
    FTStamp := TMpTimeStamp.Make(Int64(q and $3ffffffff), DWord(q shr 34));
    FCurrToken := mtkTStamp;
    TermDone;
  end;
end;

function TMpReader.DoReadTStamp96: Boolean;
var
  q: QWord;
  d: DWord;
  b: Byte;
begin
  if not DoReadByte(b) then exit(False);
  if TMpExtType(b) <> -1 then exit(False);
  if not DoReadDWord(d) then exit(False);
  Result := DoReadQWord(q);
  if Result then begin
    FTStamp := TMpTimeStamp.Make(Int64(q), d);
    FCurrToken := mtkTStamp;
    TermDone;
  end;
end;

function TMpReader.DoReadExt8: Boolean;
var
  Len: Byte;
begin
  if not DoReadByte(Len) then exit(False);
  if ReadMode then begin
    if Len = 12 then
      if (FBufPos < FBufSize) and (TMpExtType(FBuffer[FBufPos]) = -1) then exit(DoReadTStamp96);
    Result := DoReadBytes(Succ(SizeInt(Len)), FBytes);
  end else
    Result := DoSkipBytes(Succ(SizeInt(Len)));
  if Result then begin
    FCurrToken := mtkExt;
    TermDone;
  end;
end;

function TMpReader.DoReadExt16: Boolean;
var
  Len: Word;
begin
  if not DoReadWord(Len) then exit(False);
  Result := DoReadExt(Succ(SizeInt(Len)));
end;

function TMpReader.DoReadExt32: Boolean;
var
  Len: DWord;
begin
  if not DoReadDWord(Len) then exit(False);
  {$IFNDEF CPU64}if Len > System.High(SizeInt) then exit(False);{$ENDIF}
  Result := DoReadExt(Succ(SizeInt(Len)));
end;

function TMpReader.DoReadFloat32: Boolean;
begin
  if ReadMode then
    Result := DoReadSingle(FSingle)
  else
    Result := DoSkipSingle;
  if Result then begin
    FCurrToken := mtkSingle;
    TermDone;
  end;
end;

function TMpReader.DoReadFloat64: Boolean;
begin
  if ReadMode then
    Result := DoReadDouble(FDouble)
  else
    Result := DoSkipDouble;
  if Result then begin
    FCurrToken := mtkDouble;
    TermDone;
  end;
end;

function TMpReader.DoReadUInt8: Boolean;
var
  b: Byte;
begin
  if ReadMode then begin
    Result := DoReadByte(b);
    FInt := b;
  end else
    Result := DoSkipByte;
  if Result then begin
    FCurrToken := mtkInt;
    TermDone;
  end;
end;

function TMpReader.DoReadUInt16: Boolean;
var
  w: Word;
begin
  if ReadMode then begin
    Result := DoReadWord(w);
    FInt := w;
  end else
    Result := DoSkipWord;
  if Result then begin
    FCurrToken := mtkInt;
    TermDone;
  end;
end;

function TMpReader.DoReadUInt32: Boolean;
var
  d: DWord;
begin
  if ReadMode then begin
    Result := DoReadDWord(d);
    FInt := d;
  end else
    Result := DoSkipDWord;
  if Result then begin
    FCurrToken := mtkInt;
    TermDone;
  end;
end;

function TMpReader.DoReadUInt64: Boolean;
var
  q: QWord;
begin
  if ReadMode then begin
    Result := DoReadQWord(q);
    FInt := Int64(q);
  end else
    Result := DoSkipWord;
  if Result then begin
    FCurrToken := mtkInt;
    TermDone;
  end;
end;

function TMpReader.DoReadInt8: Boolean;
var
  b: Byte;
begin
  if ReadMode then begin
    Result := DoReadByte(b);
    FInt := Int8(b);
  end else
    Result := DoSkipByte;
  if Result then begin
    FCurrToken := mtkInt;
    TermDone;
  end;
end;

function TMpReader.DoReadInt16: Boolean;
var
  w: Word;
begin
  if ReadMode then begin
    Result := DoReadWord(w);
    FInt := Int16(w);
  end else
    Result := DoSkipWord;
  if Result then begin
    FCurrToken := mtkInt;
    TermDone;
  end;
end;

function TMpReader.DoReadInt32: Boolean;
var
  d: DWord;
begin
  if ReadMode then begin
    Result := DoReadDWord(d);
    FInt := Int32(d);
  end else
    Result := DoSkipDWord;
  if Result then begin
    FCurrToken := mtkInt;
    TermDone;
  end;
end;

function TMpReader.DoReadInt64: Boolean;
var
  q: QWord;
begin
  if ReadMode then begin
    Result := DoReadQWord(q);
    FInt := Int64(q);
  end else
    Result := DoSkipWord;
  if Result then begin
    FCurrToken := mtkInt;
    TermDone;
  end;
end;

function TMpReader.DoReadFixExt1: Boolean;
begin
  if ReadMode then
    Result := DoReadBytes(2, FBytes)
  else
    Result := DoSkipBytes(2);
  if Result then begin
    FCurrToken := mtkExt;
    TermDone;
  end;
end;

function TMpReader.DoReadFixExt2: Boolean;
begin
  if ReadMode then
    Result := DoReadBytes(3, FBytes)
  else
    Result := DoSkipBytes(3);
  if Result then begin
    FCurrToken := mtkExt;
    TermDone;
  end;
end;

function TMpReader.DoReadFixExt4: Boolean;
begin
  if ReadMode then begin
    if (FBufPos < FBufSize) and (TMpExtType(FBuffer[FBufPos]) = -1) then exit(DoReadTStamp32);
    Result := DoReadBytes(5, FBytes);
  end else
    Result := DoSkipBytes(5);
  if Result then begin
    FCurrToken := mtkExt;
    TermDone;
  end;
end;

function TMpReader.DoReadFixExt8: Boolean;
begin
  if ReadMode then begin
    if (FBufPos < FBufSize) and (TMpExtType(FBuffer[FBufPos]) = -1) then exit(DoReadTStamp64);
    Result := DoReadBytes(9, FBytes);
  end else
    Result := DoSkipBytes(9);
  if Result then begin
    FCurrToken := mtkExt;
    TermDone;
  end;
end;

function TMpReader.DoReadFixExt16: Boolean;
begin
  if ReadMode then
    Result := DoReadBytes(17, FBytes)
  else
    Result := DoSkipBytes(17);
  if Result then begin
    FCurrToken := mtkExt;
    TermDone;
  end;
end;

function TMpReader.DoReadStr8: Boolean;
var
  Len: Byte;
begin
  if not DoReadByte(Len) then exit(False);
  Result := DoReadStr(SizeInt(Len));
end;

function TMpReader.DoReadStr16: Boolean;
var
  Len: Word;
begin
  if not DoReadWord(Len) then exit(False);
  Result := DoReadStr(SizeInt(Len));
end;

function TMpReader.DoReadStr32: Boolean;
var
  Len: DWord;
begin
  if not DoReadDWord(Len) then exit(False);
  {$IFNDEF CPU64}if Len > System.High(SizeInt) then exit(False);{$ENDIF}
  Result := DoReadStr(SizeInt(Len));
end;

function TMpReader.DoBeginArray16: Boolean;
var
  Len: Word;
begin
  if not DoReadWord(Len) then exit(False);
  Result := DoBeginArray(SizeInt(Len));
end;

function TMpReader.DoBeginArray32: Boolean;
var
  Len: DWord;
begin
  if not DoReadDWord(Len) then exit(False);
  {$IFNDEF CPU64}if Len > System.High(SizeInt) then exit(False);{$ENDIF}
  Result := DoBeginArray(SizeInt(Len));
end;

function TMpReader.DoBeginMap16: Boolean;
var
  Len: Word;
begin
  if not DoReadWord(Len) then exit(False);
  Result := DoBeginMap(SizeInt(Len));
end;

function TMpReader.DoBeginMap32: Boolean;
var
  Len: DWord;
begin
  if not DoReadDWord(Len) then exit(False);
  {$IFNDEF CPU64}if Len > System.High(SizeInt) then exit(False);{$ENDIF}
  Result := DoBeginMap(SizeInt(Len));
end;

function TMpReader.DoRead: Boolean;
const
{$PUSH}{$J-}
  END_TOKEN_ARRAY: array[mskArray..mskMap] of TMpTokenKind = (mtkArrayEnd, mtkMapEnd);
{$POP}
var
  pfx: Byte;
begin
  if FStack[StackTop].Count = 0 then
    if FCurrToken <> END_TOKEN_ARRAY[StructKind] then begin
      FCurrToken := END_TOKEN_ARRAY[StructKind];
      exit(True);
    end else
      if FCurrToken = END_TOKEN_ARRAY[StructKind] then begin
        FCurrToken := mtkNone;
        PopStack;
        exit(DoRead());
      end;
  if (StructKind = mskMap) and not DoReadKey() then exit(False);
  if not DoReadByte(pfx) then exit(False);
  Result := True;
  case FORMAT_TBL[pfx] of
    mpfFixInt:    DoReadFixInt(pfx);
    mpfFixMap:    Result := DoBeginMap(SizeInt(pfx and $0f));
    mpfFixArray:  Result := DoBeginArray(SizeInt(pfx and $0f));
    mpfFixStr:    Result := DoReadStr(pfx and $1f);
    mpfNil:       DoReadNil;
    mpfUnused:    Result := False;
    mpfFalse:     DoReadBool(False);
    mpfTrue:      DoReadBool(True);
    mpfBin8:      Result := DoReadBin8;
    mpfBin16:     Result := DoReadBin16;
    mpfBin32:     Result := DoReadBin32;
    mpfExt8:      Result := DoReadExt8;
    mpfExt16:     Result := DoReadExt16;
    mpfExt32:     Result := DoReadExt32;
    mpfFloat32:   Result := DoReadFloat32;
    mpfFloat64:   Result := DoReadFloat64;
    mpfUInt8:     Result := DoReadUInt8;
    mpfUInt16:    Result := DoReadUInt16;
    mpfUInt32:    Result := DoReadUInt32;
    mpfUInt64:    Result := DoReadUInt64;
    mpfInt8:      Result := DoReadInt8;
    mpfInt16:     Result := DoReadInt16;
    mpfInt32:     Result := DoReadInt32;
    mpfInt64:     Result := DoReadInt64;
    mpfFixExt1:   Result := DoReadFixExt1;
    mpfFixExt2:   Result := DoReadFixExt2;
    mpfFixExt4:   Result := DoReadFixExt4;
    mpfFixExt8:   Result := DoReadFixExt8;
    mpfFixExt16:  Result := DoReadFixExt16;
    mpfStr8:      Result := DoReadStr8;
    mpfStr16:     Result := DoReadStr16;
    mpfStr32:     Result := DoReadStr32;
    mpfArray16:   Result := DoBeginArray16;
    mpfArray32:   Result := DoBeginArray32;
    mpfMap16:     Result := DoBeginMap16;
    mpfMap32:     Result := DoBeginMap32;
    mpfNegFixInt: DoReadNegFixInt(pfx);
  end;
end;

class function TMpReader.DoReadDom(aReader: TMpReader; out aNode: TMpDomNode;
  aDuplicates: TMpDuplicates): Boolean;

  function DoReadNode(aNode: TMpDomNode): Boolean; forward;

  function DoReadArray(aNode: TMpDomNode): Boolean;
  var
    I: SizeInt;
  begin
    if aReader.TokenKind <> mtkArrayBegin then exit(False);
  {$PUSH}{$Q+}{$R+}
    I := aReader.StructUnread;
  {$POP}
    aNode.AsArray.EnsureCapacity(I);
    while I <> 0 do begin
      if not aReader.Read then exit(False);
      if not DoReadNode(aNode.AddNode) then exit(False);
      Dec(I);
    end;
    Result := aReader.Read and (aReader.TokenKind = mtkArrayEnd);
  end;

  function DoReadMap(aNode: TMpDomNode): Boolean;
  var
    I: SizeInt;
    n: TMpDomNode;
  begin
    if aReader.TokenKind <> mtkMapBegin then exit(False);
  {$PUSH}{$Q+}{$R+}
    I := aReader.StructUnread;
  {$POP}
    aNode.AsMap.EnsureCapacity(I);
    while I <> 0 do begin
      if not aReader.Read then exit(False);
      case aDuplicates of
        mduAccept:  n := aNode.AddNode(aReader.KeyValue);
        mduRewrite: n := aNode[aReader.KeyValue].AsNil;
      else // mduIgnore
        if not aNode.TryAddNode(aReader.KeyValue, n) then begin
          if aReader.TokenKind in START_TOKENS then begin
            aReader.Skip;
            if aReader.ReadState = mrsError then exit(False);
          end;
          continue;
        end;
      end;
      if not DoReadNode(n) then exit(False);
      Dec(I);
    end;
    Result := aReader.Read and (aReader.TokenKind = mtkMapEnd);
  end;

  function DoReadNode(aNode: TMpDomNode): Boolean;
  begin
    case aReader.TokenKind of
      mtkArrayBegin: if not DoReadArray(aNode) then exit(False);
      mtkMapBegin:   if not DoReadMap(aNode) then exit(False);
      mtkNil:        aNode.AsNil;
      mtkBool:       aNode.AsBoolean := aReader.AsBoolean;
      mtkInt:        aNode.AsInteger := aReader.AsInt;
      mtkSingle:     aNode.AsSingle := aReader.AsSingle;
      mtkDouble:     aNode.AsDouble := aReader.AsDouble;
      mtkString:     aNode.AsString := aReader.AsString;
      mtkBin:        aNode.AsBinary := aReader.AsBinary;
      mtkTStamp:     aNode.AsTimeStamp := aReader.AsTimeStamp;
      mtkExt:        aNode.AsExtention := aReader.AsExtention;
    else
      exit(False);
    end;
    Result := True;
  end;

begin
  aNode := TMpDomNode.Create;
  try
    try
      if (aReader.ReadState = mrsStart) and not aReader.Read then exit(False);
      Result := DoReadNode(aNode) and (aReader.ReadState <> mrsError);
    except
      Result := False;
    end;
  finally
    if not Result then FreeAndNil(aNode);
  end;
end;

class function TMpReader.DoFind(aReader: TMpReader; const aKey: TMpVariant): Boolean;
var
  Len, Idx: Int64;
begin
  if not(aReader.TokenKind in START_TOKENS) then exit(False);
  Len := aReader.StructUnread;
  if aReader.TokenKind = mtkArrayBegin then begin
    if aKey.Kind = mvkInt then begin
      Idx := aKey.AsInt;
      if QWord(Idx) >= QWord(Len) then exit(False);
    end else
      exit(False);
    if Idx = 0 then begin
      if not aReader.Read then exit(False);
    end else
      while Idx <> 0 do begin
        if not aReader.Read then exit(False);
        if aReader.TokenKind in START_TOKENS then begin
          aReader.Skip;
          if aReader.ReadState = mrsError then exit(False);
        end;
      Dec(Idx);
    end;
  end else begin // mtkMapBegin
    Idx := 0;
    while Idx < Len do begin
      if not aReader.Read then exit(False);
      if aReader.KeyValue = aKey then break;
      if aReader.TokenKind in START_TOKENS then begin
        aReader.Skip;
        if aReader.ReadState = mrsError then exit(False);
      end;
      Inc(Idx);
    end;
    if Idx = Len then exit(False);
  end;
  Result := True;
end;

class function TMpReader.DoFindPath(aReader: TMpReader; const aPath: array of TMpVariant): Boolean;
var
  I: SizeInt;
begin
  for I := 0 to System.High(aPath) do begin
    if not((aReader.TokenKind in START_TOKENS) or aReader.Read) then exit(False);
    if not DoFind(aReader, aPath[I]) then exit(False);
  end;
  Result := True;
end;

class function TMpReader.DoFindPath(aReader: TMpReader; const aPath: array of string): Boolean;
var
  I, Idx: SizeInt;
  Len, J: Int64;
begin
  for I := 0 to System.High(aPath) do begin
    if not((aReader.TokenKind in START_TOKENS) or aReader.Read) then exit(False);
    if not(aReader.TokenKind in START_TOKENS) then exit(False);
    Len := aReader.StructUnread;
    if aReader.TokenKind = mtkArrayBegin then begin
      if not IsNonNegativeInt(aPath[I], Idx) then exit(False);
      if Idx >= Len then exit(False);
      if Idx = 0 then begin
        if not aReader.Read then exit(False);
      end else
        for Idx := Idx - 1 downto 0 do begin
          if not aReader.Read then exit(False);
          if aReader.TokenKind in START_TOKENS then begin
            aReader.Skip;
            if aReader.ReadState = mrsError then exit(False);
          end;
        end;
    end else begin // mtkMapBegin
      J := 0;
      while J < Len do begin
        if not aReader.Read then exit(False);
        if aReader.KeyValue = aPath[I] then break;
        if aReader.TokenKind in START_TOKENS then begin
          aReader.Skip;
          if aReader.ReadState = mrsError then exit(False);
        end;
        Inc(J);
      end;
      if J = Len then exit(False);
    end;
  end;
  Result := True;
end;

class function TMpReader.TryReadDom(aBuffer: PByte; aCount: SizeInt; out aNode: TMpDomNode;
  aDuplicates: TMpDuplicates; aMaxDepth: Integer): Boolean;
var
  Reader: TMpReader;
begin
  if aMaxDepth < 1 then exit(False);
  Reader := TMpReader.Create(aBuffer, aCount, aMaxDepth);
  try
    if not DoReadDom(Reader, aNode, aDuplicates) then exit(False);
    Result := Reader.Position = Reader.BufferSize;
    if Result and (Reader.ReadState < mrsEof) then
      while Reader.Read do;
    Result := Result and (Reader.ReadState = mrsEof);
  finally
    Reader.Free;
  end;
end;

constructor TMpReader.Create(p: PByte; aCount: SizeInt; aMaxDepth: Integer);
begin
  inherited Create;
  FBuffer := p;
  FBufSize := aCount;
  FReadMode := True;
  FStackTop := NULL_INDEX;
  if aMaxDepth < 0 then aMaxDepth := 0;
  System.SetLength(FStack, aMaxDepth + 1);
  TryPushStack(NULL_INDEX, mskNone);
end;

function TMpReader.Read: Boolean;
begin
  if ReadState = mrsStart then begin
    if FBufPos >= FBufSize then begin
      FState := mrsError;
      exit(False);
    end;
    FState := mrsGo;
  end else
    if ReadState > mrsGo then
      exit(False);
  Result := DoRead;
  if not Result then
    if ReadState <> mrsError then
      if (FBufPos = FBufSize) and (ReadState = mrsGo) and (Depth = 0) then
        FState := mrsEof
      else
        FState := mrsError
end;

procedure TMpReader.Skip;
var
  OldDepth: SizeInt;
  Token: TMpTokenKind;
const
{$PUSH}{$J-}
  EndsArray: array[mtkArrayBegin..mtkMapBegin] of TMpTokenKind = (mtkArrayEnd, mtkMapEnd);
{$POP}
begin
  if ReadState > mrsGo then exit;
  if TokenKind in START_TOKENS then begin
    Token := EndsArray[TokenKind];
    OldDepth := Depth;
    FReadMode := False;
    try
      repeat
        if not Read then exit;
      until (Depth = OldDepth) and (TokenKind = Token);
    finally
      FReadMode := True;
    end;
  end else
    Read;
end;

function TMpReader.CopyStruct(out aStruct: TBytes): Boolean;
var
  Len: Int64;
  StartPos: SizeInt;
  Writer: TMpWriter;
begin
  aStruct := nil;
  if ReadState = mrsError then exit(False);
  if TokenKind in START_TOKENS then begin
    Len := FStack[StackTop].Count;
    case Len of
      0..$f:      StartPos := Pred(Position);
      $10..$ffff: StartPos := Position - 3;
    else
      StartPos := Position - 5;
    end;
    Skip;
    if ReadState = mrsError then exit(False);
    System.SetLength(aStruct, Position - StartPos);
    System.Move(FBuffer[StartPos], Pointer(aStruct)^, Position - StartPos);
  end else begin
    Writer := TMpWriter.Create;
    try
      case TokenKind of
        mtkNil:    Writer.AddNil;
        mtkBool:   Writer.Add(AsBoolean);
        mtkInt:    Writer.Add(AsInt);
        mtkSingle: Writer.Add(AsSingle);
        mtkDouble: Writer.Add(AsDouble);
        mtkString: Writer.Add(AsString);
        mtkBin:    Writer.Add(AsBinary);
        mtkTStamp: Writer.Add(AsTimeStamp);
        mtkExt:    Writer.Add(AsExtention);
      else
        exit(False);
      end;
      aStruct := Writer.ToBytes;
    finally
      Writer.Free;
    end;
  end;
  Result := True;
end;

{$PUSH}{$WARN 5089 OFF : Local variable "$1" of a managed type does not seem to be initialized}
function TMpReader.FindPath(const aPath: array of TMpVariant; out aNode: TMpDomNode): Boolean;
var
  Ref: specialize TGUniqRef<TMpReader>;
  Reader: TMpReader;
begin
  Ref.Instance := TMpReader.Create(FBuffer, FBufSize, System.Length(FStack) - 1);
  Reader := Ref;
  if System.Length(aPath) = 0 then exit(DoReadDom(Reader, aNode));
  if not DoFindPath(Reader, aPath) then exit(False);
  if (Reader.TokenKind in END_TOKENS) and not Reader.Read then exit(False);
  Result := DoReadDom(Reader, aNode);
end;

function TMpReader.FindPath(const aPath: array of TMpVariant; out aBytes: TBytes): Boolean;
var
  Ref: specialize TGUniqRef<TMpReader>;
  Reader: TMpReader;
begin
  aBytes := nil;
  if System.Length(aPath) = 0 then begin
    System.SetLength(aBytes, BufferSize);
    if BufferSize = 0 then exit(False);
    System.Move(FBuffer^, Pointer(aBytes)^, BufferSize);
    exit(True);
  end;
  Ref.Instance := TMpReader.Create(FBuffer, FBufSize, System.Length(FStack) - 1);
  Reader := Ref;
  if not DoFindPath(Reader, aPath) then exit(False);
  if (Reader.TokenKind in END_TOKENS) and not Reader.Read then exit(False);
  Result := Reader.CopyStruct(aBytes);
end;

function TMpReader.FindPath(const aPath: array of string; out aNode: TMpDomNode): Boolean;
var
  Ref: specialize TGUniqRef<TMpReader>;
  Reader: TMpReader;
begin
  Ref.Instance := TMpReader.Create(FBuffer, FBufSize, System.Length(FStack) - 1);
  Reader := Ref;
  if System.Length(aPath) = 0 then exit(DoReadDom(Reader, aNode));
  if not DoFindPath(Reader, aPath) then exit(False);
  if (Reader.TokenKind in END_TOKENS) and not Reader.Read then exit(False);
  Result := DoReadDom(Reader, aNode);
end;

function TMpReader.FindPath(const aPath: array of string; out aBytes: TBytes): Boolean;
var
  Ref: specialize TGUniqRef<TMpReader>;
  Reader: TMpReader;
begin
  aBytes := nil;
  if System.Length(aPath) = 0 then begin
    System.SetLength(aBytes, BufferSize);
    if BufferSize = 0 then exit(False);
    System.Move(FBuffer^, Pointer(aBytes)^, BufferSize);
    exit(True);
  end;
  Ref.Instance := TMpReader.Create(FBuffer, FBufSize, System.Length(FStack) - 1);
  Reader := Ref;
  if not DoFindPath(Reader, aPath) then exit(False);
  if (Reader.TokenKind in END_TOKENS) and not Reader.Read then exit(False);
  Result := Reader.CopyStruct(aBytes);
end;
{$POP}

function TMpReader.Find(const aKey: TMpVariant): Boolean;
begin
  Result := DoFind(Self, aKey);
end;

end.


