{****************************************************************************
*                                                                           *
*   This file is part of the LGenerics package.                             *
*   Most common types and utils.                                            *
*                                                                           *
*   Copyright(c) 2018 A.Koverdyaev(avk)                                     *
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
unit LGUtils;

{$MODE DELPHI}
{$INLINE ON}
{$MODESWITCH NESTEDPROCVARS}

interface

uses

  SysUtils,
  typinfo,
  LGStrConst;

type

  TSortOrder            = (soAsc, soDesc);
  TRangeBound           = (rbLow, rbHigh);
  TRangeBounds          = set of TRangeBound;
  TGArray<T>            = array of T;
  TGCompare<T>          = function(constref L, R: T): SizeInt;
  TGOnCompare<T>        = function(constref L, R: T): SizeInt of object;
  TGNestCompare<T>      = function(constref L, R: T): SizeInt is nested;
  TGEqualCompare<T>     = function(constref L, R: T): Boolean;
  TGOnEqualCompare<T>   = function(constref L, R: T): Boolean of object;
  TGNestEqualCompare<T> = function(constref L, R: T): Boolean is nested;
{ predicates }
  TGTest<T>             = function(constref aValue: T): Boolean;
  TGOnTest<T>           = function(constref aValue: T): Boolean of object;
  TGNestTest<T>         = function(constref aValue: T): Boolean is nested;
{ mappings }
  TGMapFunc<X, Y>       = function(constref aValue: X): Y;
  TGOnMap<X, Y>         = function(constref aValue: X): Y of object;
  TGNestMap<X, Y>       = function(constref aValue: X): Y is nested;
{ foldings; note: accumulator on second position }
  TGFold<X, Y>          = function(constref L: X; constref R: Y): Y;
  TGOnFold<X, Y>        = function(constref L: X; constref R: Y): Y of object;
  TGNestFold<X, Y>      = function(constref L: X; constref R: Y): Y is nested;

  ELGPanic              = class(Exception);
  ELGCapacityExceed     = class(Exception);
  ELGAccessEmpty        = class(Exception);
  ELGOptional           = class(Exception);
  ELGFuture             = class(Exception);
  ELGUpdateLock         = class(Exception);
  ELGListError          = class(Exception);
  ELGMapError           = class(Exception);
  ELGTableError         = class(Exception);

const
  MAX_CONTAINER_SIZE         = Succ(SizeInt.MaxValue shr 2);
  ARRAY_INITIAL_SIZE         = 32;// * must be power of 2 *
  DEFAULT_CONTAINER_CAPACITY = 32;// * must be power of 2 *
  BOUNDS_BOTH                = TRangeBounds([rbLow, rbHigh]);
  WAIT_INFINITE              = -1;

type

  TGOptional<T> = record
  private
  var
    FValue: T;
    FAssigned: Boolean;
  class var
    CFTypeKind: System.TTypeKind;
    CFNilable: Boolean;
  const
    NilableKinds = [System.tkMethod, System.tkInterface, System.tkClass, System.tkDynArray,
                    System.tkInterfaceRaw, System.tkProcVar, System.tkClassRef, System.tkPointer];
    function GetValue: T;
    class constructor InitTypeInfo;
    class function ValueIsNil(constref aValue): Boolean; static;
    class operator Initialize(var o: TGOptional<T>); inline;
  public
    procedure Assign(constref aValue: T); inline;
    function  OrElseDefault: T; inline;
    function  OrElse(constref aValue: T): T; inline;
    function  OrElseRaise(e: ExceptClass; const aMsg: string = ''): T; inline;
    property  Assigned: Boolean read FAssigned;
    property  Value: T read GetValue;
    class property Nilable: Boolean read CFNilable;
  end;

  { TGAutoRef: the simplest way to get a class instance with limited lifetime;
      instance will be destroyed automatically when it leaves the scope;
      class T must have default constructor without parameters }
  TGAutoRef<T: class> = record
  private
    FInstance: T;
    function  GetInstance: T; inline;
    procedure SetInstance(aValue: T); inline;
    class operator Initialize(var a: TGAutoRef<T>); inline;
    class operator Finalize(var a: TGAutoRef<T>); inline;
  public
  type
    TInstance = T;
    class operator Implicit(var a: TGAutoRef<T>): T; inline;
    property Instance: T read GetInstance write SetInstance;
  end;

  TGCustomEnumerator<T> = class abstract
  protected
    function  GetCurrent: T; virtual; abstract;
  public
    function  MoveNext: Boolean; virtual; abstract;
    procedure Reset; virtual; abstract;
    property  Current: T read GetCurrent;
  end;

{$PUSH}{$INTERFACES COM}
  IExecutable = interface
  ['{896FB5A3-4993-4698-9C33-D538A3BEE876}']
    procedure Execute;
  end;

  IGCallable<T> = interface
  ['{EF605AA3-4E0F-4C52-9A75-5068D94ECDAF}']
    function Call: T;
  end;
{$POP}

{$PUSH}{$INTERFACES CORBA}
  IObjInstance = interface
  ['{B5EABEA2-FF39-4B4A-AF2B-3B8603F0C575}']
    function _GetRef: TObject;
  end;

  IGEnumerable<T> = interface(IObjInstance)
  ['{92F9FDFC-BEA4-4968-A033-7A90C05DDA60}']
    function GetEnumerator: TGCustomEnumerator<T>;
  { enumerates elements in reverse order }
    function Reverse: IGEnumerable<T>;
    function ToArray: TGArray<T>;
    function Any: Boolean; overload;
    function None: Boolean;  overload;
    function Total: SizeInt; overload;
    function FindFirst(out aValue: T): Boolean;
    function First: TGOptional<T>;
    function FindLast(out aValue: T): Boolean;
    function Last: TGOptional<T>;
    function FindMin(out aValue: T; c: TGCompare<T>): Boolean; overload;
    function FindMin(out aValue: T; c: TGOnCompare<T>): Boolean; overload;
    function FindMin(out aValue: T; c: TGNestCompare<T>): Boolean; overload;
    function Min(c: TGCompare<T>): TGOptional<T>; overload;
    function Min(c: TGOnCompare<T>): TGOptional<T>; overload;
    function Min(c: TGNestCompare<T>): TGOptional<T>; overload;
    function FindMax(out aValue: T; c: TGCompare<T>): Boolean; overload;
    function FindMax(out aValue: T; c: TGOnCompare<T>): Boolean; overload;
    function FindMax(out aValue: T; c: TGNestCompare<T>): Boolean; overload;
    function Max(c: TGCompare<T>): TGOptional<T>; overload;
    function Max(c: TGOnCompare<T>): TGOptional<T>; overload;
    function Max(c: TGNestCompare<T>): TGOptional<T>; overload;
    function Skip(aCount: SizeInt): IGEnumerable<T>;
    function Limit(aCount: SizeInt): IGEnumerable<T>;
    function Sorted(c: TGCompare<T>): IGEnumerable<T>; overload;
    function Sorted(c: TGOnCompare<T>): IGEnumerable<T>; overload;
    function Sorted(c: TGNestCompare<T>): IGEnumerable<T>; overload;
    function Select(p: TGTest<T>): IGEnumerable<T>; overload;
    function Select(p: TGOnTest<T>): IGEnumerable<T>; overload;
    function Select(p: TGNestTest<T>): IGEnumerable<T>; overload;
    function Any(p: TGTest<T>): Boolean; overload;
    function Any(p: TGOnTest<T>): Boolean; overload;
    function Any(p: TGNestTest<T>): Boolean; overload;
    function None(p: TGTest<T>): Boolean; overload;
    function None(p: TGOnTest<T>): Boolean; overload;
    function None(p: TGNestTest<T>): Boolean; overload;
    function All(p: TGTest<T>): Boolean; overload;
    function All(p: TGOnTest<T>): Boolean; overload;
    function All(p: TGNestTest<T>): Boolean; overload;
    function Total(p: TGTest<T>): SizeInt; overload;
    function Total(p: TGOnTest<T>): SizeInt; overload;
    function Total(p: TGNestTest<T>): SizeInt; overload;
    function Distinct(c: TGCompare<T>): IGEnumerable<T>; overload;
    function Distinct(c: TGOnCompare<T>): IGEnumerable<T>; overload;
    function Distinct(c: TGNestCompare<T>): IGEnumerable<T>; overload;
    function Map(f: TGMapFunc<T, T>): IGEnumerable<T>; overload;
    function Map(f: TGOnMap<T, T>): IGEnumerable<T>; overload;
    function Map(f: TGNestMap<T, T>): IGEnumerable<T>; overload;
  { left-associative linear fold }
    function Fold(f: TGFold<T, T>; constref v0: T): T; overload;
    function Fold(f: TGFold<T, T>): TGOptional<T>; overload;
    function Fold(f: TGOnFold<T, T>; constref v0: T): T; overload;
    function Fold(f: TGOnFold<T, T>): TGOptional<T>; overload;
    function Fold(f: TGNestFold<T, T>; constref v0: T): T; overload;
    function Fold(f: TGNestFold<T, T>): TGOptional<T>; overload;
  end;

  IGContainer<T> = interface(IGEnumerable<T>)
  ['{A3F04344-421D-4678-8A88-42AF65647525}']
    function  GetCount: SizeInt;
    function  GetCapacity: SizeInt;
    function  IsEmpty: Boolean;
    function  NonEmpty: Boolean;
    procedure Clear;
    procedure EnsureCapacity(aValue: SizeInt);
  { free unused memory if possible }
    procedure TrimToFit;
    property  Count: SizeInt read GetCount;
    property  Capacity: SizeInt read GetCapacity;
  end;

  IGStack<T> = interface(IGContainer<T>)
  ['{6057A96E-1953-49CE-A81A-DF5633BCB38C}']
    procedure Push(constref aValue: T);
    function  PushAll(constref a: array of T): SizeInt; overload;
    function  PushAll(e: IGEnumerable<T>): SizeInt; overload;
  { EXTRACTS element from the top of stack }
    function  Pop: T;
    function  TryPop(out aValue: T): Boolean;
    function  Peek: T;
    function  TryPeek(out aValue: T): Boolean;
  end;

  IGQueue<T> = interface(IGContainer<T>)
  ['{913AFB4A-7D2C-46D8-A4FD-DAEC1F80D6C2}']
  { puts element in the tail of the queue }
    procedure Enqueue(constref aValue: T);
    function  EnqueueAll(constref a: array of T): SizeInt; overload;
    function  EnqueueAll(e: IGEnumerable<T>): SizeInt; overload;
  { EXTRACTS element from the head of queue }
    function  Dequeue: T;
    function  TryDequeue(out aValue: T): Boolean;
  { examines element in the head of queue }
    function  Peek: T;
    function  TryPeek(out aValue: T): Boolean;
  end;

  THandle = SysUtils.THandle;

const
  INVALID_HANDLE = THandle(-1);

type

  IGPriorityQueue<T> = interface(IGQueue<T>)
  ['{39ADFF1D-018D-423B-A16A-8942B06D0A76}']
    function  Insert(constref aValue: T): THandle;
    function  PeekHead: THandle;
    function  TryPeekHead(out aValue: THandle): Boolean;
    function  Value(h: THandle): T;
    procedure Update(h: THandle; constref aValue: T);
    function  Remove(h: THandle): T;
  { only another entity can be merged, aQueue will be cleared after Merge }
    function  Merge(aQueue: IGPriorityQueue<T>): SizeInt;
  end;

  IGDeque<T> = interface(IGContainer<T>)
  ['{0D127C9E-9706-4D9A-A64C-A70844DC1F55}']
    function  GetItem(aIndex: SizeInt): T;
    procedure SetItem(aIndex: SizeInt; const aValue: T);
    procedure PushFirst(constref aValue: T);
    function  PushAllFirst(constref a: array of T): SizeInt; overload;
    function  PushAllFirst(e: IGEnumerable<T>): SizeInt; overload;
    procedure PushLast(constref aValue: T);
    function  PushAllLast(constref a: array of T): SizeInt; overload;
    function  PushAllLast(e: IGEnumerable<T>): SizeInt; overload;
  { EXTRACTS element from the head of deque }
    function  PopFirst: T;
    function  TryPopFirst(out aValue: T): Boolean;
  { EXTRACTS element from the tail of deque }
    function  PopLast: T;
    function  TryPopLast(out aValue: T): Boolean;
  { examines element in the head of deque }
    function  PeekFirst: T;
    function  TryPeekFirst(out aValue: T): Boolean;
  { examines element in the tail of deque }
    function  PeekLast: T;
    function  TryPeekLast(out aValue: T): Boolean;
  { inserts aValue into position aIndex }
    procedure Insert(aIndex: SizeInt; constref aValue: T);
    function  TryInsert(aIndex: SizeInt; constref aValue: T): Boolean;
  { extracts value from position aIndex }
    function  Extract(aIndex: SizeInt): T;
    function  TryExtract(aIndex: SizeInt; out aValue: T): Boolean;
  { deletes value in position aIndex }
    procedure Delete(aIndex: SizeInt);
    function  TryDelete(aIndex: SizeInt): Boolean;
    property  Items[aIndex: SizeInt]: T read GetItem write SetItem; default;
  end;

  IGCollection<T> = interface(IGContainer<T>)
  ['{53197613-B1FC-46BD-923A-A602D0545330}']
  { returns True if element added }
    function  Add(constref aValue: T): Boolean;
    function  Contains(constref aValue: T): Boolean;
    function  NonContains(constref aValue: T): Boolean;
    function  ContainsAny(constref a: array of T): Boolean; overload;
    function  ContainsAny(e: IGEnumerable<T>): Boolean; overload;
    function  ContainsAll(constref a: array of T): Boolean; overload;
    function  ContainsAll(e: IGEnumerable<T>): Boolean; overload;
    function  Extract(constref aValue: T): Boolean;
  { returns True if element removed }
    function  Remove(constref aValue: T): Boolean;
  { returns count of removed elements }
    function  RemoveAll(constref a: array of T): SizeInt; overload;
  { returns count of removed elements }
    function  RemoveAll(e: IGEnumerable<T>): SizeInt; overload;
  { will contain only those elements that are simultaneously contained in self and aCollection }
    procedure RetainAll(c: IGCollection<T>);
  end;

  TGMapEntry<TKey, TValue> = record
    Key: TKey;
    Value: TValue;
  end;

  IGMap<TKey, TValue> = interface{(IGContainer<TGMapEntry<TKey, TValue>>)}
  ['{67DBDBD2-D54C-4E6E-9BE6-ACDA0A40B63F}']
    function  _GetRef: TObject;
    function  GetCount: SizeInt;
    function  GetCapacity: SizeInt;
    function  GetValue(const aKey: TKey): TValue;
    function  IsEmpty: Boolean;
    function  NonEmpty: Boolean;
    procedure Clear;
    procedure EnsureCapacity(aValue: SizeInt);
  { free unused memory if possible }
    procedure TrimToFit;
  { returns True and add TEntry(aKey, aValue) only if not contains aKey }
    function  Add(constref aKey: TKey; constref aValue: TValue): Boolean;
    procedure AddOrSetValue(const aKey: TKey; const aValue: TValue);
    function  TryGetValue(constref aKey: TKey; out aValue: TValue): Boolean;
    function  GetValueDef(constref aKey: TKey; constref aDefault: TValue = Default(TValue)): TValue;
  { returns True and map aNewValue to aKey only if contains aKey, False otherwise }
    function  Replace(constref aKey: TKey; constref aNewValue: TValue): Boolean;
    function  Contains(constref aKey: TKey): Boolean;
    function  Extract(constref aKey: TKey; out v: TValue): Boolean;
    function  Remove(constref aKey: TKey): Boolean;
    procedure RetainAll(c: IGCollection<TKey>);
    function  Keys: IGEnumerable<TKey>;
    function  Values: IGEnumerable<TValue>;
    property  Count: SizeInt read GetCount;
    property  Capacity: SizeInt read GetCapacity;
  { reading will raise ELGMapError if an aKey is not present in map }
    property  Items[const aKey: TKey]: TValue read GetValue write AddOrSetValue; default;
  end;
{$POP}

  { TGNodeManager: TNode must provide read-write property NextLink: PNode }
  TGNodeManager<TNode> = class
  public
  type
    PNode = ^TNode;
  private
    FHead: PNode;
    FFreeCount: SizeInt;
    procedure Put2FreeList(aNode: PNode); inline;
    class function CreateNode: PNode; static; inline;
    property Head: PNode read FHead;
  public
    destructor Destroy; override;
    function  NewNode: PNode;
    procedure DisposeNode(aNode: PNode); inline;
    procedure FreeNode(aNode: PNode); inline;
    procedure EnsureFreeCount(aCount: SizeInt);
    procedure ClearFreeList;
    procedure Clear; inline;
    property  FreeCount: SizeInt read FFreeCount;
  end;

  { TGPageNodeManager: TNode must provide read-write property NextLink: PNode}
  TGPageNodeManager<TNode> = class
  public
  type
    PNode = ^TNode;

  private
  const
    PAGE_SIZE      = 4096;
    NODE_SIZE      = SizeOf(TNode);
    NODES_PER_PAGE = (PAGE_SIZE - SizeOf(Pointer)) div NODE_SIZE;

  type
    PPage = ^TPage;
    TPage = record
      Nodes: array[1..NODES_PER_PAGE] of TNode;
      NextPage: PPage;
    end;

  var
    FPageListHead: PPage;
    FFreeListHead: PNode;
    FFreeCount,
    FPageCount: SizeInt;
    procedure NewPage;
  public
    destructor Destroy; override;
    function  NewNode: PNode;
    procedure DisposeNode(aNode: PNode); inline;
    procedure FreeNode(aNode: PNode); inline;
    procedure EnsureFreeCount(aCount: SizeInt);
    procedure ClearFreeList;
    procedure Clear;
    property  FreeCount: SizeInt read FFreeCount;
    property  PagesAllocated: SizeInt read FPageCount;
  end;

  { TGJoinableNodeManager: TNode must provide read-write property NextLink: PNode }
  TGJoinableNodeManager<TNode> = record
  public
  type
    PNode = ^TNode;

  private
    FHead,
    FTail: PNode;
    FFreeCount: SizeInt;
    procedure Put2FreeList(aNode: PNode); inline;
    class function CreateNode: PNode; static; inline;
    class operator Finalize(var nm: TGJoinableNodeManager<TNode>);
  public
    function  NewNode: PNode;
    procedure DisposeNode(aNode: PNode); inline;
    procedure FreeNode(aNode: PNode); inline;
    procedure EnsureFreeCount(aCount: SizeInt);
    procedure ClearFreeList;
    procedure Clear; inline;
    procedure Join(var nm: TGJoinableNodeManager<TNode>);
    property  FreeCount: SizeInt read FFreeCount;
  end;

  { TGJoinablePageNodeManager: TNode must provide read-write property NextLink: PNode}
  TGJoinablePageNodeManager<TNode> = record
  public
  type
    PNode = ^TNode;

  private
  const
    PAGE_SIZE      = 4096;
    NODE_SIZE      = SizeOf(TNode);
    NODES_PER_PAGE = (PAGE_SIZE - SizeOf(Pointer)) div NODE_SIZE;

  type
    PPage = ^TPage;
    TPage = record
      Nodes: array[1..NODES_PER_PAGE] of TNode;
      NextPage: PPage;
    end;

  var
    FPageListHead,
    FPageListTail: PPage;
    FFreeListHead,
    FFreeListTail: PNode;
    FFreeCount,
    FPageCount: SizeInt;
    procedure NewPage;
    class operator Finalize(var nm: TGJoinablePageNodeManager<TNode>);
  public
    function  NewNode: PNode;
    procedure DisposeNode(aNode: PNode); inline;
    procedure FreeNode(aNode: PNode); inline;
    procedure EnsureFreeCount(aCount: SizeInt);
    procedure ClearFreeList;
    procedure Clear;
    procedure Join(var nm: TGJoinablePageNodeManager<TNode>);
    property  FreeCount: SizeInt read FFreeCount;
    property  PagesAllocated: SizeInt read FPageCount;
  end;

  { returns number of significant bits of aValue }
  function NSB(aValue: SizeUInt): SizeInt; inline;
  function IsTwoPower(aValue: SizeUInt): Boolean; inline;
  { note: if aValue > Succ(SizeInt.MaxValue shr 1) then function will return wrong result }
  function RoundUpTwoPower(aValue: SizeInt): SizeInt;

implementation
{$B-}{$COPERATORS ON}

{$PUSH}{$Q-}{$R-}
function NSB(aValue: SizeUInt): SizeInt;
begin
{$IF DEFINED(CPU64)}
  Result := Succ(ShortInt(BsrQWord(aValue)));
{$ELSEIF DEFINED(CPU32)}
  Result := Succ(ShortInt(BsrDWord(aValue)));
{$ELSE}
  Result := Succ(ShortInt(BsrWord(aValue)));
{$ENDIF}
end;

function IsTwoPower(aValue: SizeUInt): Boolean;
begin
  if aValue <> 0 then
    Result := aValue and Pred(aValue) = 0
  else
    Result := False;
end;

function RoundUpTwoPower(aValue: SizeInt): SizeInt;
const
  MAX_ROUND_VALUE = Succ(SizeInt.MaxValue shr 1);
begin
  Assert(aValue <= MAX_ROUND_VALUE, Format(SEArgumentTooBigFmt, [{$I %CURRENTROUTINE%}, aValue]));
  if aValue > 1 then
    begin
      if not LGUtils.IsTwoPower(aValue) then
        Result := SizeInt(1) shl LGUtils.NSB(aValue)
      else
        Result := aValue; // round not needed ???
    end
  else
    Result := 2;
end;
{$POP}

{ TGOptional }

function TGOptional<T>.GetValue: T;
begin
  if Assigned then
    Result := FValue
  else
    raise ELGOptional.Create(SEOptionalValueEmpty);
end;

class constructor TGOptional<T>.InitTypeInfo;
var
  p: typinfo.PTypeInfo;
begin
  p := System.TypeInfo(T);
  if p <> nil then
    begin
      CFTypeKind := p^.Kind;
      CFNilable := CFTypeKind in NilableKinds;
    end
  else
    begin
      CFNilable := False;
      CFTypeKind := System.tkUnknown;
    end;
end;

class function TGOptional<T>.ValueIsNil(constref aValue): Boolean;
begin
  if Nilable then
    case CFTypeKind of
      System.tkMethod:       Result := Pointer(aValue) = nil;
      System.tkInterface:    Result := Pointer(aValue) = nil;
      System.tkClass:        Result := TObject(aValue) = nil;
      System.tkDynArray:     Result := Pointer(aValue) = nil;
      System.tkInterfaceRaw: Result := Pointer(aValue) = nil;
      System.tkProcVar:      Result := Pointer(aValue) = nil;
      System.tkClassRef:     Result := TClass(aValue)  = nil;
      System.tkPointer:      Result := Pointer(aValue) = nil;
    else //todo: what about Variants ?
      Result := False;
    end
  else
    Result := False;
end;

class operator TGOptional<T>.Initialize(var o: TGOptional<T>);
begin
  o.FAssigned := False;
end;

procedure TGOptional<T>.Assign(constref aValue: T);
begin
  if not ValueIsNil((@aValue)^) then
    begin
      FAssigned := True;
      FValue := aValue;
    end;
end;

function TGOptional<T>.OrElseDefault: T;
begin
  if Assigned then
    Result := FValue
  else
    Result := Default(T);
end;

function TGOptional<T>.OrElse(constref aValue: T): T;
begin
  if Assigned then
    Result := FValue
  else
    Result := aValue;
end;

function TGOptional<T>.OrElseRaise(e: ExceptClass; const aMsg: string): T;
begin
  if not Assigned then
    raise e.Create(aMsg);
  Result := FValue;
end;

{ TGAutoRef<T> }

function TGAutoRef<T>.GetInstance: T;
begin
  if not Assigned(FInstance) then
    FInstance := T.Create;
  Result := FInstance;
end;

procedure TGAutoRef<T>.SetInstance(aValue: T);
begin
  FInstance.Free;
  FInstance := aValue;
end;

class operator TGAutoRef<T>.Initialize(var a: TGAutoRef<T>);
begin
  a.FInstance := Default(T);
end;

class operator TGAutoRef<T>.Finalize(var a: TGAutoRef<T>);
begin
  a.FInstance.Free;
end;

class operator TGAutoRef<T>.Implicit(var a: TGAutoRef<T>): T;
begin
  Result := a.Instance;
end;

{ TGNodeManager }

procedure TGNodeManager<TNode>.Put2FreeList(aNode: PNode);
begin
  aNode^.NextLink := Head;
  FHead := aNode;
  Inc(FFreeCount);
end;

class function TGNodeManager<TNode>.CreateNode: PNode;
begin
  System.New(Result);
  System.FillChar(Result^, SizeOf(TNode), 0);
end;

destructor TGNodeManager<TNode>.Destroy;
begin
  ClearFreeList;
  inherited;
end;

function TGNodeManager<TNode>.NewNode: PNode;
begin
  if Head <> nil then
    begin
      Result := Head;
      FHead := Result^.NextLink;
      Result^.NextLink := nil;
      Dec(FFreeCount);
    end
  else
    Result := CreateNode;
end;

procedure TGNodeManager<TNode>.DisposeNode(aNode: PNode);
begin
  System.Dispose(aNode);
end;

procedure TGNodeManager<TNode>.FreeNode(aNode: PNode);
begin
  Put2FreeList(aNode);
end;

procedure TGNodeManager<TNode>.EnsureFreeCount(aCount: SizeInt);
begin
  while aCount > FreeCount do
    Put2FreeList(CreateNode);
end;

procedure TGNodeManager<TNode>.ClearFreeList;
var
  CurrNode, NextNode: PNode;
begin
  CurrNode := Head;
  while CurrNode <> nil do
    begin
      NextNode := CurrNode^.NextLink;
      System.Dispose(CurrNode);
      CurrNode := NextNode;
      Dec(FFreeCount);
    end;
  FHead := nil;
  Assert(FFreeCount = 0, Format('Inconsistent FFreeCount value(%d)', [FFreeCount]));
end;

procedure TGNodeManager<TNode>.Clear;
begin
  ClearFreeList;
end;

{ TGPageNodeManager }

procedure TGPageNodeManager<TNode>.NewPage;
var
  CurrPage: PPage;
  LastNode: PNode;
  I: Integer;
begin
  System.New(CurrPage);
  System.FillChar(CurrPage^, SizeOf(TPage), 0);
  CurrPage^.NextPage := FPageListHead;
  FPageListHead := CurrPage;
  with CurrPage^ do
    begin
      LastNode := FFreeListHead;
      I := 1;
      while I <= NODES_PER_PAGE - 4 do
        begin
          Nodes[I  ].NextLink := LastNode;
          Nodes[I+1].NextLink := @Nodes[I  ];
          Nodes[I+2].NextLink := @Nodes[I+1];
          Nodes[I+3].NextLink := @Nodes[I+2];
          LastNode := @Nodes[I+3];
          I += 4;
        end;
      for I := I to NODES_PER_PAGE do
        begin
          Nodes[I].NextLink := LastNode;
          LastNode := @Nodes[I];
        end;
      FFreeListHead := LastNode;
    end;
  Inc(FPageCount);
  FFreeCount += NODES_PER_PAGE;
end;

destructor TGPageNodeManager<TNode>.Destroy;
begin
  Clear;
  inherited;
end;

function TGPageNodeManager<TNode>.NewNode: PNode;
begin
  if FFreeListHead = nil then
    NewPage;
  Result := FFreeListHead;
  FFreeListHead := Result^.NextLink;
  Result^.NextLink := nil;
  Dec(FFreeCount);
end;

procedure TGPageNodeManager<TNode>.DisposeNode(aNode: PNode);
begin
  FreeNode(aNode);
end;

procedure TGPageNodeManager<TNode>.FreeNode(aNode: PNode);
begin
  aNode^.NextLink := FFreeListHead;
  FFreeListHead := aNode;
  Inc(FFreeCount);
end;

procedure TGPageNodeManager<TNode>.EnsureFreeCount(aCount: SizeInt);
begin
  while FreeCount < aCount do
    NewPage;
end;

procedure TGPageNodeManager<TNode>.ClearFreeList;
begin
  // do nothing
end;

procedure TGPageNodeManager<TNode>.Clear;
var
  CurrPage, NextPage: PPage;
begin
  FFreeListHead := nil;
  CurrPage := FPageListHead;
  while CurrPage <> nil do
    begin
      NextPage := CurrPage^.NextPage;
      System.Dispose(CurrPage);
      CurrPage := NextPage;
      Dec(FPageCount);
    end;
  FPageListHead := nil;
  FFreeCount := 0;
  Assert(FPageCount = 0, Format('Inconsistent FPageCount value(%d)', [FPageCount]));
end;

{ TGJoinableNodeManager }

procedure TGJoinableNodeManager<TNode>.Put2FreeList(aNode: PNode);
begin
  aNode^.NextLink := FHead;
  FHead := aNode;
  Inc(FFreeCount);
  if FTail = nil then
    FTail := aNode;
end;

class function TGJoinableNodeManager<TNode>.CreateNode: PNode;
begin
  System.New(Result);
  System.FillChar(Result^, SizeOf(TNode), 0);
end;

class operator TGJoinableNodeManager<TNode>.Finalize(var nm: TGJoinableNodeManager<TNode>);
begin
  nm.Clear;
end;

function TGJoinableNodeManager<TNode>.NewNode: PNode;
begin
  if FHead <> nil then
    begin
      Result := FHead;
      FHead := Result^.NextLink;
      Result^.NextLink := nil;
      Dec(FFreeCount);
      if FHead = nil then
        FTail := nil;
    end
  else
    Result := CreateNode;
end;

procedure TGJoinableNodeManager<TNode>.DisposeNode(aNode: PNode);
begin
  System.Dispose(aNode);
end;

procedure TGJoinableNodeManager<TNode>.FreeNode(aNode: PNode);
begin
  Put2FreeList(aNode);
end;

procedure TGJoinableNodeManager<TNode>.EnsureFreeCount(aCount: SizeInt);
begin
  while aCount > FreeCount do
    Put2FreeList(CreateNode);
end;

procedure TGJoinableNodeManager<TNode>.ClearFreeList;
var
  CurrNode, NextNode: PNode;
begin
  CurrNode := FHead;
  while CurrNode <> nil do
    begin
      NextNode := CurrNode^.NextLink;
      System.Dispose(CurrNode);
      CurrNode := NextNode;
      Dec(FFreeCount);
    end;
  FHead := nil;
  FTail := nil;
  Assert(FFreeCount = 0, Format('Inconsistent FFreeCount value(%d)', [FFreeCount]));
end;

procedure TGJoinableNodeManager<TNode>.Clear;
begin
  ClearFreeList;
end;

procedure TGJoinableNodeManager<TNode>.Join(var nm: TGJoinableNodeManager<TNode>);
begin
  if nm.FreeCount > 0 then
    begin
      if FreeCount > 0 then
        FTail^.NextLink := nm.FHead
      else
        FHead := nm.FHead;
      FTail := nm.FTail;
      FFreeCount += nm.FreeCount;
      nm.FFreeCount := 0;
      nm.FHead := nil;
      nm.FTail := nil;
    end;
end;

{ TGJoinablePageNodeManager }

procedure TGJoinablePageNodeManager<TNode>.NewPage;
var
  CurrPage: PPage;
  LastNode: PNode;
  I: Integer;
begin
  System.New(CurrPage);
  System.FillChar(CurrPage^, SizeOf(TPage), 0);
  CurrPage^.NextPage := FPageListHead;
  FPageListHead := CurrPage;
  if FPageListTail = nil then
    FPageListTail := CurrPage;
  if FFreeListTail = nil then
    FFreeListTail := @CurrPage^.Nodes[1];
  with CurrPage^ do
    begin
      LastNode := FFreeListHead;
      I := 1;
      while I <= NODES_PER_PAGE - 4 do
        begin
          Nodes[I  ].NextLink := LastNode;
          Nodes[I+1].NextLink := @Nodes[I  ];
          Nodes[I+2].NextLink := @Nodes[I+1];
          Nodes[I+3].NextLink := @Nodes[I+2];
          LastNode := @Nodes[I+3];
          I += 4;
        end;
      for I := I to NODES_PER_PAGE do
        begin
          Nodes[I].NextLink := LastNode;
          LastNode := @Nodes[I];
        end;
      FFreeListHead := LastNode;
    end;
  Inc(FPageCount);
  FFreeCount += NODES_PER_PAGE;
end;

class operator TGJoinablePageNodeManager<TNode>.Finalize(var nm: TGJoinablePageNodeManager<TNode>);
begin
  nm.Clear;
end;

function TGJoinablePageNodeManager<TNode>.NewNode: PNode;
begin
  if FFreeListHead = nil then
    NewPage;
  Result := FFreeListHead;
  FFreeListHead := Result^.NextLink;
  Result^.NextLink := nil;
  Dec(FFreeCount);
  if FFreeListHead = nil then
    FFreeListTail := nil;
end;

procedure TGJoinablePageNodeManager<TNode>.DisposeNode(aNode: PNode);
begin
  FreeNode(aNode);
end;

procedure TGJoinablePageNodeManager<TNode>.FreeNode(aNode: PNode);
begin
  aNode^.NextLink := FFreeListHead;
  FFreeListHead := aNode;
  Inc(FFreeCount);
  if FFreeListTail = nil then
    FFreeListTail := aNode;
end;

procedure TGJoinablePageNodeManager<TNode>.EnsureFreeCount(aCount: SizeInt);
begin
  while FreeCount < aCount do
    NewPage;
end;

procedure TGJoinablePageNodeManager<TNode>.ClearFreeList;
begin
  // do nothing
end;

procedure TGJoinablePageNodeManager<TNode>.Clear;
var
  CurrPage, NextPage: PPage;
begin
  FFreeListHead := nil;
  FFreeListTail := nil;
  CurrPage := FPageListHead;
  while CurrPage <> nil do
    begin
      NextPage := CurrPage^.NextPage;
      System.Dispose(CurrPage);
      CurrPage := NextPage;
      Dec(FPageCount);
    end;
  FPageListHead := nil;
  FPageListTail := nil;
  FFreeCount := 0;
  Assert(FPageCount = 0, Format('Inconsistent FPageCount value(%d)', [FPageCount]));
end;

procedure TGJoinablePageNodeManager<TNode>.Join(var nm: TGJoinablePageNodeManager<TNode>);
begin
  if nm.PagesAllocated > 0 then
    begin
      if FPageListTail <> nil then
        FPageListTail^.NextPage := nm.FPageListHead
      else
        FPageListHead := nm.FPageListHead;
      FPageListTail := nm.FPageListTail;
      FPageCount += nm.PagesAllocated;
      nm.FPageCount := 0;
      nm.FPageListHead := nil;
      nm.FPageListTail := nil;
      if nm.FreeCount > 0 then
        begin
          if FFreeListTail <> nil then
            FFreeListTail^.NextLink := nm.FFreeListHead
          else
            FFreeListHead := nm.FFreeListHead;
          FFreeListTail := nm.FFreeListTail;
          FFreeCount += nm.FreeCount;
          nm.FFreeCount := 0;
          nm.FFreeListHead := nil;
          nm.FFreeListTail := nil;
        end;
    end;
end;

end.

