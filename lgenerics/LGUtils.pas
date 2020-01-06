{****************************************************************************
*                                                                           *
*   This file is part of the LGenerics package.                             *
*   Most common types and utils.                                            *
*                                                                           *
*   Copyright(c) 2018-2019 A.Koverdyaev(avk)                                *
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
  Math,
  typinfo,
  {%H-}Rtti,
  LGStrConst;

type

  TSortOrder            = (soAsc, soDesc);
  TRangeBound           = (rbLow, rbHigh);
  TTriLean              = (tlFalse, tlTrue, tlUnknown);
  TRangeBounds          = set of TRangeBound;
  TDummy                = packed record end;
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

  ELGCapacityExceed     = class(Exception);
  ELGAccessEmpty        = class(Exception);
  ELGOptional           = class(Exception);
  ELGFuture             = class(Exception);
  ELGUpdateLock         = class(Exception);
  ELGListError          = class(Exception);
  ELGMapError           = class(Exception);
  ELGTableError         = class(Exception);

const
  MAX_CONTAINER_SIZE         = Succ(High(SizeInt) shr 2);
  ARRAY_INITIAL_SIZE         = 32;// * must be power of 2 *
  DEFAULT_CONTAINER_CAPACITY = 32;// * must be power of 2 *
  BOUNDS_BOTH                = TRangeBounds([rbLow, rbHigh]);
  WAIT_INFINITE              = -1;
  NULL_INDEX: SizeInt        = SizeInt(-1);

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
    class operator Implicit(constref aValue: T): TGOptional<T>; inline;
    class operator Implicit(constref aOpt: TGOptional<T>): T; inline;
    class operator Explicit(constref aOpt: TGOptional<T>): T; inline;
    procedure Assign(constref aValue: T);
    function  OrElseDefault: T; inline;
    function  OrElse(constref aValue: T): T; inline;
    function  OrElseRaise(e: ExceptClass; const aMsg: string = ''): T; inline;
    property  Assigned: Boolean read FAssigned;
    property  Value: T read GetValue;
    class property Nilable: Boolean read CFNilable;
  end;

  { TGAutoRef: the easy way to get a class instance with limited lifetime;
    An instance owned by TGAutoRef will be automatically created upon first request
    and will automatically be destroyed upon leaving the scope;
    class T must provide default parameterless constructor;
    copying a record will raise EInvalidOpException }
  TGAutoRef<T: class, constructor> = record
  strict private
    FInstance: T;
    FOwnsInstance: Boolean;
    function  GetInstance: T;
    procedure SetInstance(aValue: T); inline;
    function  Release: T;
    class operator Initialize(var a: TGAutoRef<T>); inline;
    class operator Finalize(var a: TGAutoRef<T>); inline;
    class operator Copy(constref aSrc: TGAutoRef<T>; var aDst: TGAutoRef<T>);
    class operator AddRef(var a: TGAutoRef<T>); inline;
  public
  type
    TInstance = T;
    class operator Implicit(var a: TGAutoRef<T>): T; inline;
    class operator Explicit(var a: TGAutoRef<T>): T; inline;
    function  HasInstance: Boolean; inline;
  { frees the instance it owns }
    procedure Clear; inline;
  { returns the instance it owns and stops ownership }
    function  ReleaseInstance: T; inline;
  { transfers ownership of an instance to aRef;
    will raise EInvalidOpException if it does not own the instance }
    procedure OwnMove(var aRef: TGAutoRef<T>);
    property  Instance: T read GetInstance write SetInstance;
    property  OwnsInstance: Boolean read FOwnsInstance;
  end;

  { TGUniqRef: like TGAutoRef provides a class instance with a limited lifetime,
    it does not require T to have a parameterless constructor, and does not automatically
    create an instance; copying a record will raise EInvalidOpException }
  TGUniqRef<T: class> = record
  strict private
    FInstance: T;
    FOwnsInstance: Boolean;
    procedure SetInstance(aValue: T); inline;
    function  Release: T;
    class operator Initialize(var u: TGUniqRef<T>); inline;
    class operator Finalize(var u: TGUniqRef<T>); inline;
    class operator Copy(constref aSrc: TGUniqRef<T>; var aDst: TGUniqRef<T>);
    class operator AddRef(var u: TGUniqRef<T>); inline;
  public
  type
    TInstance = T;
    class operator Implicit(var u: TGUniqRef<T>): T; inline;
    class operator Explicit(var u: TGUniqRef<T>): T; inline;
    function  HasInstance: Boolean; inline;
  { frees the instance it owns }
    procedure Clear; inline;
  { returns the instance it owns and stops ownership }
    function  ReleaseInstance: T; inline;
  { transfers ownership of an instance to aRef;
    will raise EInvalidOpException if it does not own the instance }
    procedure OwnMove(var aRef: TGUniqRef<T>);
    property  Instance: T read FInstance write SetInstance;
    property  OwnsInstance: Boolean read FOwnsInstance;
  end;

  { TGSharedRefA(A - Auto create): intended to be shared a single instance by several TGSharedRefA
    entities using ARC, the instance will be automatically destroyed when the reference count becomes
    zero; to automatically create an instance, class T must provide default parameterless constructor }
  TGSharedRefA<T: class, constructor> = record
  private
    FInstance: T;
    FRefCount: PInteger;
    procedure InitInstance(aValue: T);
    function  GetInstance: T;
    function  GetRefCount: Integer; inline;
    procedure SetInstance(aValue: T);
    class operator Initialize(var s: TGSharedRefA<T>); inline;
    class operator Finalize(var s: TGSharedRefA<T>);
    class operator Copy(constref aSrc: TGSharedRefA<T>; var aDst: TGSharedRefA<T>); inline;
    class operator AddRef(var s: TGSharedRefA<T>); inline;
  public
  type
    TInstance = T;
    class operator Implicit(var s: TGSharedRefA<T>): T; inline;
    class operator Explicit(var s: TGSharedRefA<T>): T; inline;
    function  HasInstance: Boolean; inline;
    procedure Release;
    property  RefCount: Integer read GetRefCount;
    property  Instance: T read GetInstance write SetInstance;
  end;

  { TGSharedRef: like TGSharedRefA intended to be shared a single instance by several TGSharedRef
    entities using ARC; it does not require T to have a parameterless constructor, and does not
    automatically create an instance }
  TGSharedRef<T: class> = record
  private
    FInstance: T;
    FRefCount: PInteger;
    procedure InitInstance(aValue: T);
    function  GetInstance: T;
    function  GetRefCount: Integer; inline;
    procedure SetInstance(aValue: T);
    class operator Initialize(var s: TGSharedRef<T>); inline;
    class operator Finalize(var s: TGSharedRef<T>);
    class operator Copy(constref aSrc: TGSharedRef<T>; var aDst: TGSharedRef<T>); inline;
    class operator AddRef(var s: TGSharedRef<T>); inline;
  public
  type
    TInstance = T;
    class operator Implicit(var s: TGSharedRef<T>): T; inline;
    class operator Explicit(var s: TGSharedRef<T>): T; inline;
    function  HasInstance: Boolean; inline;
    procedure Release;
    property  RefCount: Integer read GetRefCount;
    property  Instance: T read GetInstance write SetInstance;
  end;

  { TGUniqPtr }
  TGUniqPtr<T> = record
  public
  type
    PValue = ^T;
  private
  class var
    CFIsManaged: Boolean;
  var
    FPtr: PValue;
    FOwnsPtr: Boolean;
    function  GetAllocated: Boolean; inline;
    function  GetPtr: PValue;
    procedure FreePtr; inline;
    function  GetValue: T; inline;
    procedure SetValue(const aValue: T); inline;
    class constructor Init;
    class operator Initialize(var u: TGUniqPtr<T>); inline;
    class operator Finalize(var u: TGUniqPtr<T>); inline;
    class operator Copy(constref aSrc: TGUniqPtr<T>; var aDst: TGUniqPtr<T>);
    class operator AddRef(var u: TGUniqPtr<T>); inline;
  public
    class property IsManaged: Boolean read CFIsManaged;
    class operator Implicit(var u: TGUniqPtr<T>): T; inline;
    class operator Explicit(var u: TGUniqPtr<T>): T; inline;
    procedure Clear;
    procedure OwnMove(var aPtr: TGUniqPtr<T>);
    property  Allocated: Boolean read GetAllocated;
    property  Ptr: PValue read GetPtr;
    property  OwnsPtr: Boolean read FOwnsPtr;
    property  Value: T read GetValue write SetValue;
  end;

  { TGCowPtr: provides an ARC pointer to data on the heap with copy-on-write semantics(if necessary) }
  TGCowPtr<T> = record
  public
  type
    PValue = ^T;
  private
  type
    TInstance = record
      RefCount: Integer;
      Value: T;
    end;
    PInstance = ^TInstance;
  class var
    CFIsManaged: Boolean;
  var
    FInstance: PInstance;
    function  NewInstance: PInstance;
    procedure ReleaseInstance;
    function  GetAllocated: Boolean; inline;
    function  GetRefCount: Integer;
    function  GetPtr: PValue;
    function  GetUniqPtr: PValue;
    function  GetValue: T; inline;
    procedure SetValue(const aValue: T); inline;
    class constructor Init;
    class operator Initialize(var cp: TGCowPtr<T>); inline;
    class operator Finalize(var cp: TGCowPtr<T>); inline;
    class operator Copy(constref aSrc: TGCowPtr<T>; var aDst: TGCowPtr<T>);
    class operator AddRef(var cp: TGCowPtr<T>); inline;
  public
    class property IsManaged: Boolean read CFIsManaged;
    class operator Implicit(var cp: TGCowPtr<T>): T; inline;
    class operator Explicit(var cp: TGCowPtr<T>): T; inline;
    procedure Release;
  { ensures that the current ref is unique }
    procedure Unique;
    property  Allocated: Boolean read GetAllocated;
    property  RefCount: Integer read GetRefCount;
  { use Ptr to read data value, or to write/modify data value if COW is not required }
    property  Ptr: PValue read GetPtr;
  { use UniqPtr to write/modify data value if COW is required }
    property  UniqPtr: PValue read GetUniqPtr;
  { SetValue always uses COW }
    property  Value: T read GetValue write SetValue;
  end;

  { TGCowDynArray: ARC dynamic array with copy-on-write semantics(if necessary) }
  TGCowDynArray<T> = record
  type
    PItem = ^T;
  private
  type
    TInstance = record
      FItems: PItem;
      FLength: SizeInt;
      FRefCount: Integer;
    end;
    PInstance = ^TInstance;
    PArray    = ^TGCowDynArray<T>;

  class var
    CFIsManaged: Boolean;
  var
    FInstance: PInstance;
    function  NewInstance: PInstance; inline;
    procedure UniqInstance;
    procedure ReallocUniq(aNewLen: SizeInt);
    procedure Realloc(aNewLen: SizeInt);
    procedure ReallocManaged(aNewLen: SizeInt);
    function  GetLength: SizeInt; inline;
    function  GetRefCount: Integer; inline;
    function  GetItem(aIndex: SizeInt): T; inline;
    function  GetPtr: PItem; inline;
    function  GetUniqPtr: PItem;
    procedure SetLen(aValue: SizeInt);
    procedure SetItem(aIndex: SizeInt; const aValue: T);
    function  GetHigh: SizeInt; inline;
    class constructor Init;
    class procedure FillItems(aFrom: PItem; aCount: SizeInt; const aValue: T); static;
    class procedure CopyItems(aSrc, aDst: PItem; aCount: SizeInt); static;
    class operator  Initialize(var a: TGCowDynArray<T>); inline;
    class operator  Finalize(var a: TGCowDynArray<T>); inline;
    class operator  Copy(constref aSrc: TGCowDynArray<T>; var aDst: TGCowDynArray<T>);
    class operator  AddRef(var a: TGCowDynArray<T>); inline;
  public
  type
    TEnumerator = record
    private
      FCurrent,
      FLast: PItem;
      function GetCurrent: T; inline;
    public
      function MoveNext: Boolean; inline;
      property Current: T read GetCurrent;
    end;

    TReverseEnumerator = record
    private
      FCurrent,
      FFirst: PItem;
      function GetCurrent: T; inline;
    public
      function MoveNext: Boolean; inline;
      property Current: T read GetCurrent;
    end;

    TReverse = record
    private
      FArray: PArray;
    public
      function GetEnumerator: TReverseEnumerator; inline;
    end;

  private
    function  GetReverseEnumerator: TReverseEnumerator;
  public
    class property ItemTypeIsManaged: Boolean read CFIsManaged;
    function  GetEnumerator: TEnumerator;
    function  Reverse: TReverse; inline;
    function  IsEmpty: Boolean; inline;
    function  NonEmpty: Boolean; inline;
    procedure Release;
  { ensures that the current instance is unique }
    procedure Unique;
  { sets length to aCount and fills array by aCount values aValue }
    procedure Fill(aCount: SizeInt; constref aValue: T);
    function  CreateCopy(aFromIndex, aCount: SizeInt): TGCowDynArray<T>;
    property  RefCount: Integer read GetRefCount;
    property  Length: SizeInt read GetLength write SetLen;
    property  High: SizeInt read GetHigh;
    property  Items[aIndex: SizeInt]: T read GetItem write SetItem; default;
    property  Ptr: PItem read GetPtr;
    property  UniqPtr: PItem read GetUniqPtr;
  end;

  TGEnumerator<T> = class abstract
  protected
    function  GetCurrent: T; virtual; abstract;
  public
    function  MoveNext: Boolean; virtual; abstract;
    procedure Reset; virtual; abstract;
    property  Current: T read GetCurrent;
  end;

{$PUSH}{$INTERFACES COM}
  ITask = interface
  ['{896FB5A3-4993-4698-9C33-D538A3BEE876}']
    procedure Execute;
  end;

  TTaskPriority = (tapLowest, tapBelowNormal, tapNormal, tapAboveNormal, tapHighest);

  IPriorityTask = interface(ITask)
  ['{24E2498F-3849-4995-9FEC-D82F90D34B1F}']
    function GetPriority: TTaskPriority;
  end;
{$POP}

{$PUSH}{$INTERFACES CORBA}
  IObjInstance = interface
  ['{B5EABEA2-FF39-4B4A-AF2B-3B8603F0C575}']
    function _GetRef: TObject;
  end;

  IGEnumerable<T> = interface(IObjInstance)
  ['{92F9FDFC-BEA4-4968-A033-7A90C05DDA60}']
    function  GetEnumerator: TGEnumerator<T>;
    procedure Discard;
  { enumerates elements in reverse order }
    function  Reverse: IGEnumerable<T>;
    function  ToArray: TGArray<T>;
    function  Any: Boolean; overload;
    function  None: Boolean;  overload;
    function  Total: SizeInt; overload;
    function  FindFirst(out aValue: T): Boolean;
    function  First: TGOptional<T>;
    function  FindLast(out aValue: T): Boolean;
    function  Last: TGOptional<T>;
    function  FindMin(out aValue: T; c: TGCompare<T>): Boolean; overload;
    function  FindMin(out aValue: T; c: TGOnCompare<T>): Boolean; overload;
    function  FindMin(out aValue: T; c: TGNestCompare<T>): Boolean; overload;
    function  Min(c: TGCompare<T>): TGOptional<T>; overload;
    function  Min(c: TGOnCompare<T>): TGOptional<T>; overload;
    function  Min(c: TGNestCompare<T>): TGOptional<T>; overload;
    function  FindMax(out aValue: T; c: TGCompare<T>): Boolean; overload;
    function  FindMax(out aValue: T; c: TGOnCompare<T>): Boolean; overload;
    function  FindMax(out aValue: T; c: TGNestCompare<T>): Boolean; overload;
    function  Max(c: TGCompare<T>): TGOptional<T>; overload;
    function  Max(c: TGOnCompare<T>): TGOptional<T>; overload;
    function  Max(c: TGNestCompare<T>): TGOptional<T>; overload;
    function  Skip(aCount: SizeInt): IGEnumerable<T>;
    function  Limit(aCount: SizeInt): IGEnumerable<T>;
    function  Sorted(c: TGCompare<T>; aStable: Boolean = False): IGEnumerable<T>; overload;
    function  Sorted(c: TGOnCompare<T>; aStable: Boolean = False): IGEnumerable<T>; overload;
    function  Sorted(c: TGNestCompare<T>; aStable: Boolean = False): IGEnumerable<T>; overload;
    function  Select(p: TGTest<T>): IGEnumerable<T>; overload;
    function  Select(p: TGOnTest<T>): IGEnumerable<T>; overload;
    function  Select(p: TGNestTest<T>): IGEnumerable<T>; overload;
    function  Any(p: TGTest<T>): Boolean; overload;
    function  Any(p: TGOnTest<T>): Boolean; overload;
    function  Any(p: TGNestTest<T>): Boolean; overload;
    function  None(p: TGTest<T>): Boolean; overload;
    function  None(p: TGOnTest<T>): Boolean; overload;
    function  None(p: TGNestTest<T>): Boolean; overload;
    function  All(p: TGTest<T>): Boolean; overload;
    function  All(p: TGOnTest<T>): Boolean; overload;
    function  All(p: TGNestTest<T>): Boolean; overload;
    function  Total(p: TGTest<T>): SizeInt; overload;
    function  Total(p: TGOnTest<T>): SizeInt; overload;
    function  Total(p: TGNestTest<T>): SizeInt; overload;
    function  Distinct(c: TGCompare<T>): IGEnumerable<T>; overload;
    function  Distinct(c: TGOnCompare<T>): IGEnumerable<T>; overload;
    function  Distinct(c: TGNestCompare<T>): IGEnumerable<T>; overload;
    function  Map(f: TGMapFunc<T, T>): IGEnumerable<T>; overload;
    function  Map(f: TGOnMap<T, T>): IGEnumerable<T>; overload;
    function  Map(f: TGNestMap<T, T>): IGEnumerable<T>; overload;
  { left-associative linear fold }
    function  Fold(f: TGFold<T, T>; constref v0: T): T; overload;
    function  Fold(f: TGFold<T, T>): TGOptional<T>; overload;
    function  Fold(f: TGOnFold<T, T>; constref v0: T): T; overload;
    function  Fold(f: TGOnFold<T, T>): TGOptional<T>; overload;
    function  Fold(f: TGNestFold<T, T>; constref v0: T): T; overload;
    function  Fold(f: TGNestFold<T, T>): TGOptional<T>; overload;
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

  THandle = type SizeUInt;

const
  INVALID_HANDLE = THandle(-1);

type

  IGPriorityQueue<T> = interface(IGQueue<T>)
  ['{39ADFF1D-018D-423B-A16A-8942B06D0A76}']
    function  Insert(constref aValue: T): THandle;
    function  PeekHandle: THandle;
    function  TryPeekHandle(out aValue: THandle): Boolean;
    function  ValueOf(h: THandle): T;
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
  { will contain only those elements that are simultaneously contained in self and c }
    procedure RetainAll(c: IGCollection<T>);
  end;

  IGReadOnlyCollection<T> = interface(IGEnumerable<T>)
  ['{D0DDB482-819A-438B-BF33-B6EF21A6A9A5}']
    function  GetCount: SizeInt;
    function  GetCapacity: SizeInt;
    function  IsEmpty: Boolean;
    function  Contains(constref aValue: T): Boolean;
    function  NonContains(constref aValue: T): Boolean;
    function  ContainsAny(constref a: array of T): Boolean; overload;
    function  ContainsAny(e: IGEnumerable<T>): Boolean; overload;
    function  ContainsAll(constref a: array of T): Boolean; overload;
    function  ContainsAll(e: IGEnumerable<T>): Boolean; overload;
    property  Count: SizeInt read GetCount;
    property  Capacity: SizeInt read GetCapacity;
  end;

  TGMapEntry<TKey, TValue> = record
    Key: TKey;
    Value: TValue;
    constructor Create(constref aKey: TKey; constref aValue: TValue);
  end;

  IGMap<TKey, TValue> = interface{(IGContainer<TGMapEntry<TKey, TValue>>)}
  ['{67DBDBD2-D54C-4E6E-9BE6-ACDA0A40B63F}']
    function  _GetRef: TObject;
    function  GetCount: SizeInt;
    function  GetCapacity: SizeInt;
    function  GetValue(const aKey: TKey): TValue;
    function  IsEmpty: Boolean;
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
    function  NonContains(constref aKey: TKey): Boolean;
    function  Extract(constref aKey: TKey; out aValue: TValue): Boolean;
    function  Remove(constref aKey: TKey): Boolean;
    procedure RetainAll(aCollection: IGCollection<TKey>);
    function  Keys: IGEnumerable<TKey>;
    function  Values: IGEnumerable<TValue>;
  { if uncomment it compiles but blocks Lazarus CodeTools }
    //function  Entries: IGEnumerable<TGMapEntry<TKey, TValue>>;
    property  Count: SizeInt read GetCount;
    property  Capacity: SizeInt read GetCapacity;
  { reading will raise exception if an aKey is not present in map }
    property  Items[const aKey: TKey]: TValue read GetValue write AddOrSetValue; default;
  end;

  IGReadOnlyMap<TKey, TValue> = interface
  ['{08561616-8E8B-4DBA-AEDB-DE14C5FA9403}']
    function  GetCount: SizeInt;
    function  GetCapacity: SizeInt;
    function  IsEmpty: Boolean;
    function  TryGetValue(constref aKey: TKey; out aValue: TValue): Boolean;
    function  GetValueDef(constref aKey: TKey; constref aDefault: TValue = Default(TValue)): TValue;
    function  Contains(constref aKey: TKey): Boolean;
    function  NonContains(constref aKey: TKey): Boolean;
    function  Keys: IGEnumerable<TKey>;
    function  Values: IGEnumerable<TValue>;
  { if uncomment it compiles but blocks Lazarus CodeTools }
    //function  Entries: IGEnumerable<TGMapEntry<TKey, TValue>>;
    property  Count: SizeInt read GetCount;
    property  Capacity: SizeInt read GetCapacity;
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
    procedure FreeNode(aNode: PNode);
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
    procedure DisposeNode(aNode: PNode);
    procedure FreeNode(aNode: PNode);
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
    procedure FreeNode(aNode: PNode);
    procedure EnsureFreeCount(aCount: SizeInt);
    procedure ClearFreeList;
    procedure Clear; inline;
    procedure Join(var nm: TGJoinableNodeManager<TNode>);
    property  FreeCount: SizeInt read FFreeCount;
  end;

  TGTuple2<T1, T2> = record
    F1: T1;
    F2: T2;
    constructor Create(constref v1: T1; constref v2: T2);
  end;

  TGTuple3<T1, T2, T3> = record
    F1: T1;
    F2: T2;
    F3: T3;
    constructor Create(constref v1: T1; constref v2: T2; constref v3: T3);
  end;

  TGTuple4<T1, T2, T3, T4> = record
    F1: T1;
    F2: T2;
    F3: T3;
    F4: T4;
    constructor Create(constref v1: T1; constref v2: T2; constref v3: T3; constref v4: T4);
  end;

  TGTuple5<T1, T2, T3, T4, T5> = record
    F1: T1;
    F2: T2;
    F3: T3;
    F4: T4;
    F5: T5;
    constructor Create(constref v1: T1; constref v2: T2; constref v3: T3; constref v4: T4; constref v5: T5);
  end;

  { TGAddMonoid uses Default(T) as identity;
    it assumes T has defined operator "+" }
  TGAddMonoid<T> = class
  private
    class function GetIdentity: T; static; inline;
  public
    class property Identity: T read GetIdentity;
    class function BinOp(const L, R: T): T; static; inline;
  end;

  { TGAddMonoidEx uses Default(T) as ZeroConst;
    it assumes T also has defined operators "=" and "*" by SizeInt }
  TGAddMonoidEx<T> = class(TGAddMonoid<T>)
  private
    class function GetZeroConst: T; static; inline;
  public
    class function AddConst(constref aValue, aConst: T; aSize: SizeInt = 1): T; static; inline;
    class function IsZeroConst(constref aValue: T): Boolean; static; inline;
    class property ZeroConst: T read GetZeroConst;
  end;

  { TGMaxMonoid uses T.MinValue as negative infinity;
    it assumes T has defined operator "<"  }
  TGMaxMonoid<T> = class
  private
    class function GetIdentity: T; static; inline;
  public
    class property Identity: T read GetIdentity;
    class function BinOp(const L, R: T): T; static; inline;
  end;

  { TGMaxMonoidEx uses Default(T) as ZeroConst;
    it assumes T has defined operators "=" and "+"  }
  TGMaxMonoidEx<T> = class(TGMaxMonoid<T>)
  private
    class function GetZeroConst: T; static; inline;
  public
    class function AddConst(constref aValue, aConst: T; aSize: SizeInt = 1): T; static; inline;
    class function IsZeroConst(constref aValue: T): Boolean; static; inline;
    class property ZeroConst: T read GetZeroConst;
  end;

  { TGMinMonoid uses T.MaxValue as infinity;
    it assumes T has defined operator "<" }
  TGMinMonoid<T> = class
  private
    class function GetIdentity: T; static; inline;
  public
    class property Identity: T read GetIdentity;
    class function BinOp(const L, R: T): T; static; inline;
  end;

  { TGMinMonoidEx uses Default(T) as ZeroConst;
    it assumes T has defined operators "=" and "+" }
  TGMinMonoidEx<T> = class(TGMinMonoid<T>)
  private
    class function GetZeroConst: T; static; inline;
  public
    class function AddConst(constref aValue, aConst: T; aSize: SizeInt = 1): T; static; inline;
    class function IsZeroConst(constref aValue: T): Boolean; static; inline;
    class property ZeroConst: T read GetZeroConst;
  end;

  { TGMaxPos uses T.MinValue as negative infinity;
    it assumes T has defined operator "<" }
  TGMaxPos<T>  = record
  private
     class function GetIdentity: TGMaxPos<T>; static; inline;
  public
    Value: T;
    Index: SizeInt;
    class property Identity: TGMaxPos<T> read GetIdentity;
    class function BinOp(const L, R: TGMaxPos<T>): TGMaxPos<T>; static; inline;
  end;

  { TGMinPos uses T.MaxValue as infinity;
    it assumes T has defined operator "<" }
  TGMinPos<T>  = record
  private
     class function GetIdentity: TGMinPos<T>; static; inline;
  public
    Value: T;
    Index: SizeInt;
    class property Identity: TGMinPos<T> read GetIdentity;
    class function BinOp(const L, R: TGMinPos<T>): TGMinPos<T>; static; inline;
  end;

{$PUSH}{$PACKRECORDS DEFAULT}
  TSpinLock = record
  strict private
  const
    CACHE_PAD_SIZE = 15;
  var
    FState: DWord;
    FCacheLinePad: array[1..CACHE_PAD_SIZE] of DWord;
    class operator Initialize(var sl: TSpinLock);
  public
    procedure Lock; inline;
    procedure LockTts;
    function  TryLock: Boolean; inline;
    procedure Unlock; inline;
  end;
{$POP}

var
  BoolRandSeed: DWord = 0;

const
{$IF DEFINED(CPU64)}
  INT_SIZE_LOG  = 6;
  INT_SIZE_MASK = 63;
{$ELSEIF DEFINED(CPU32)}
  INT_SIZE_LOG  = 5;
  INT_SIZE_MASK = 31;
{$ELSE}
  INT_SIZE_LOG  = 4;
  INT_SIZE_MASK = 15;
{$ENDIF}
  MAX_POSITIVE_POW2 = Succ(High(SizeInt) shr 1);

  { returns number of significant bits of aValue }
  function  NSB(aValue: SizeUInt): SizeInt; inline;
  function  IsTwoPower(aValue: SizeUInt): Boolean; inline;
  { warning: if aValue > MAX_POSITIVE_POW2 then function will return wrong result }
  function  RoundUpTwoPower(aValue: SizeInt): SizeInt;
  function  NextRandomBoolean: Boolean; inline;
  procedure RandomizeBoolean;
  { Bob Jenkins small noncryptographic PRNG
    http://www.burtleburtle.net/bob/rand/smallprng.html }
  procedure BJSetSeed(aSeed: DWord);
  procedure BJSetSeed64(aSeed: QWord);
  procedure BJRandomize;
  procedure BJRandomize64;
  function  BJNextRandom: DWord;
  function  BJNextRandom64: QWord;

implementation
{$B-}{$COPERATORS ON}{$POINTERMATH ON}

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
begin
  Assert(aValue <= MAX_POSITIVE_POW2, Format(SEArgumentTooBigFmt, [{$I %CURRENTROUTINE%}, aValue]));
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

function NextRandomBoolean: Boolean;
begin
  BoolRandSeed := BoolRandSeed * DWord(1103515245) + DWord(12345);
  Result := Odd(BoolRandSeed shr 16);
end;

procedure RandomizeBoolean;
begin
  BoolRandSeed := DWord(GetTickCount64);
end;

var
  p: DWord = DWord($F2346B68);
  q: DWord = DWord($43E345B9);
  r: DWord = DWord($9BDDD2D5);
  s: DWord = DWord($B78D029B);

  p64: QWord = QWord($C49205791B1F3E34);
  q64: QWord = QWord($84988390DCCAC2DA);
  r64: QWord = QWord($FECAB388259108D9);
  s64: QWord = QWord($7E7F22F098FB479C);

procedure BJSetSeed(aSeed: DWord);
var
  e: DWord;
  I: Integer;
begin
  p := $f1ea5eed;
  q := aSeed;
  r := aSeed;
  s := aSeed;
  for I := 1 to 20 do
    begin
      e := p - RolDWord(q, 23);
      p := q xor RolDWord(r, 16);
      q := r + RolDWord(s, 11);
      r := s + e;
      s := e + p;
    end;
end;

procedure BJSetSeed64(aSeed: QWord);
var
  e: QWord;
  I: Integer;
begin
  p64 := $f1ea5eed;
  q64 := aSeed;
  r64 := aSeed;
  s64 := aSeed;
  for I := 1 to 20 do
    begin
      e := p64 - RolQWord(q64, 7);
      p64 := q64 xor RolQWord(r64, 13);
      q64 := r64 + RolQWord(s64, 37);
      r64 := s64 + e;
      s64 := e + p64;
    end;
end;

procedure BJRandomize;
begin
  BJSetSeed(GetTickCount64);
end;

procedure BJRandomize64;
begin
  BJSetSeed64(GetTickCount64);
end;

function BJNextRandom: DWord;
var
  e: DWord;
begin
  e := p - RolDWord(q, 23);
  p := q xor RolDWord(r, 16);
  q := r + RolDWord(s, 11);
  r := s + e;
  s := e + p;
  Result := s;
end;

function BJNextRandom64: QWord;
var
  e: QWord;
begin
  e := p64 - RolQWord(q64, 7);
  p64 := q64 xor RolQWord(r64, 13);
  q64 := r64 + RolQWord(s64, 37);
  r64 := s64 + e;
  s64 := e + p64;
  Result := s64;
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
  case CFTypeKind of
    System.tkMethod:       exit(Pointer(aValue) = nil);
    System.tkInterface:    exit(Pointer(aValue) = nil);
    System.tkClass:        exit(TObject(aValue) = nil);
    System.tkDynArray:     exit(Pointer(aValue) = nil);
    System.tkInterfaceRaw: exit(Pointer(aValue) = nil);
    System.tkProcVar:      exit(Pointer(aValue) = nil);
    System.tkClassRef:     exit(TClass(aValue)  = nil);
    System.tkPointer:      exit(Pointer(aValue) = nil);
  else //todo: what about Variants ?
  end;
  Result := False;
end;

class operator TGOptional<T>.Initialize(var o: TGOptional<T>);
begin
  o.FAssigned := False;
end;

class operator TGOptional<T>.Implicit(constref aValue: T): TGOptional<T>;
begin
  Result.Assign(aValue);
end;

class operator TGOptional<T>.Implicit(constref aOpt: TGOptional<T>): T;
begin
  Result := aOpt.Value;
end;

class operator TGOptional<T>.Explicit(constref aOpt: TGOptional<T>): T;
begin
  Result := aOpt.Value;
end;

procedure TGOptional<T>.Assign(constref aValue: T);
begin
  if CFNilable and ValueIsNil((@aValue)^) then
    exit;
  FAssigned := True;
  FValue := aValue;
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
    begin
      FInstance := T.Create;
      FOwnsInstance := Assigned(FInstance);
    end;
  Result := FInstance;
end;

procedure TGAutoRef<T>.SetInstance(aValue: T);
begin
  if aValue <> FInstance then
    begin
      if OwnsInstance then
        FInstance.Free;
      FInstance := aValue;
      FOwnsInstance := Assigned(FInstance);
    end;
end;

function TGAutoRef<T>.Release: T;
begin
  Result := FInstance;
  FInstance := Default(T);
  FOwnsInstance := False;
end;

class operator TGAutoRef<T>.Initialize(var a: TGAutoRef<T>);
begin
  a.FInstance := Default(T);
  a.FOwnsInstance := False;
end;

class operator TGAutoRef<T>.Finalize(var a: TGAutoRef<T>);
begin
  if a.OwnsInstance then
    a.FInstance.Free;
end;

class operator TGAutoRef<T>.Copy(constref aSrc: TGAutoRef<T>; var aDst: TGAutoRef<T>);
begin
  if @aSrc <> @aDst then
    raise EInvalidOpException.Create(SECopyInadmissible);
end;

class operator TGAutoRef<T>.AddRef(var a: TGAutoRef<T>);
begin
  a.FOwnsInstance := False;
end;

class operator TGAutoRef<T>.Implicit(var a: TGAutoRef<T>): T;
begin
  Result := a.Instance;
end;

class operator TGAutoRef<T>.Explicit(var a: TGAutoRef<T>): T;
begin
  Result := a.Instance;
end;

function TGAutoRef<T>.HasInstance: Boolean;
begin
  Result := Assigned(FInstance);
end;

procedure TGAutoRef<T>.Clear;
begin
  if OwnsInstance then
    FInstance.Free;
  FInstance := Default(T);
  FOwnsInstance := False;
end;

function TGAutoRef<T>.ReleaseInstance: T;
begin
  if not OwnsInstance then
    exit(Default(T));
  Result := Release;
end;

procedure TGAutoRef<T>.OwnMove(var aRef: TGAutoRef<T>);
begin
  if not Assigned(FInstance) then
    exit;
  if OwnsInstance then
    aRef.Instance := Release
  else
    raise EInvalidOpException.Create(SEOwnRequired);
end;

procedure TGUniqRef<T>.SetInstance(aValue: T);
begin
  if aValue <> FInstance then
    begin
      if OwnsInstance then
        FInstance.Free;
      FInstance := aValue;
      FOwnsInstance := Assigned(FInstance);
    end;
end;

function TGUniqRef<T>.Release: T;
begin
  Result := FInstance;
  FInstance := Default(T);
  FOwnsInstance := False;
end;

class operator TGUniqRef<T>.Initialize(var u: TGUniqRef<T>);
begin
  u.FInstance := Default(T);
  u.FOwnsInstance := False;
end;

class operator TGUniqRef<T>.Finalize(var u: TGUniqRef<T>);
begin
  if u.OwnsInstance then
    u.FInstance.Free;
end;

class operator TGUniqRef<T>.Copy(constref aSrc: TGUniqRef<T>; var aDst: TGUniqRef<T>);
begin
  if @aSrc <> @aDst then
    raise EInvalidOpException.Create(SECopyInadmissible);
end;

class operator TGUniqRef<T>.AddRef(var u: TGUniqRef<T>);
begin
  u.FOwnsInstance := False;
end;

class operator TGUniqRef<T>.Implicit(var u: TGUniqRef<T>): T;
begin
  Result := u.Instance;
end;

class operator TGUniqRef<T>.Explicit(var u: TGUniqRef<T>): T;
begin
  Result := u.Instance;
end;

function TGUniqRef<T>.HasInstance: Boolean;
begin
  Result := Assigned(FInstance);
end;

procedure TGUniqRef<T>.Clear;
begin
  if OwnsInstance then
    FInstance.Free;
  FInstance := Default(T);
  FOwnsInstance := False;
end;

function TGUniqRef<T>.ReleaseInstance: T;
begin
  if not OwnsInstance then
    exit(Default(T));
  Result := Release;
end;

procedure TGUniqRef<T>.OwnMove(var aRef: TGUniqRef<T>);
begin
  if not Assigned(FInstance) then
    exit;
  if OwnsInstance then
    aRef.Instance := Release
  else
    raise EInvalidOpException.Create(SEOwnRequired);
end;

{ TGSharedRefA<T> }

procedure TGSharedRefA<T>.InitInstance(aValue: T);
begin
  FInstance := aValue;
  if Assigned(aValue) then
    begin
      System.New(FRefCount);
      FRefCount^ := 1;
    end;
end;

function TGSharedRefA<T>.GetInstance: T;
begin
  if FRefCount = nil then
    InitInstance(T.Create);
  Result := FInstance;
end;

function TGSharedRefA<T>.GetRefCount: Integer;
begin
  if FRefCount <> nil then
    Result := FRefCount^
  else
    Result := 0;
end;

procedure TGSharedRefA<T>.SetInstance(aValue: T);
begin
  if aValue <> FInstance then
    begin
      Release;
      InitInstance(aValue);
    end;
end;

class operator TGSharedRefA<T>.Initialize(var s: TGSharedRefA<T>);
begin
  s.FRefCount := nil;
  s.FInstance := Default(T);
end;

class operator TGSharedRefA<T>.Finalize(var s: TGSharedRefA<T>);
begin
  s.Release;
end;

class operator TGSharedRefA<T>.Copy(constref aSrc: TGSharedRefA<T>; var aDst: TGSharedRefA<T>);
begin
  if @aSrc <> @aDst then
    begin
      aDst.Release;
      if aSrc.FRefCount <> nil then
        begin
          InterLockedIncrement(aSrc.FRefCount^);
          aDst.FRefCount := aSrc.FRefCount;
          aDst.FInstance := aSrc.Instance;
        end;
    end;
end;

class operator TGSharedRefA<T>.AddRef(var s: TGSharedRefA<T>);
begin
  if s.FRefCount <> nil then
    InterLockedIncrement(s.FRefCount^);
end;

class operator TGSharedRefA<T>.Implicit(var s: TGSharedRefA<T>): T;
begin
  Result := s.Instance;
end;

class operator TGSharedRefA<T>.Explicit(var s: TGSharedRefA<T>): T;
begin
  Result := s.Instance;
end;

function TGSharedRefA<T>.HasInstance: Boolean;
begin
  Result := FRefCount <> nil;
end;

procedure TGSharedRefA<T>.Release;
begin
  if FRefCount <> nil then
    begin
      if InterlockedDecrement(FRefCount^) = 0 then
        begin
          System.Dispose(FRefCount);
          FInstance.Free;
        end;
      FRefCount := nil;
    end;
end;

{ TGSharedRef<T> }

procedure TGSharedRef<T>.InitInstance(aValue: T);
begin
  FInstance := aValue;
  if Assigned(aValue) then
    begin
      System.New(FRefCount);
      FRefCount^ := 1;
    end;
end;

function TGSharedRef<T>.GetInstance: T;
begin
  if FRefCount = nil then
    exit(Default(T));
  Result := FInstance;
end;

function TGSharedRef<T>.GetRefCount: Integer;
begin
  if FRefCount <> nil then
    Result := FRefCount^
  else
    Result := 0;
end;

procedure TGSharedRef<T>.SetInstance(aValue: T);
begin
  if aValue <> FInstance then
    begin
      Release;
      InitInstance(aValue);
    end;
end;

class operator TGSharedRef<T>.Initialize(var s: TGSharedRef<T>);
begin
  s.FRefCount := nil;
  s.FInstance := Default(T);
end;

class operator TGSharedRef<T>.Finalize(var s: TGSharedRef<T>);
begin
  s.Release;
end;

class operator TGSharedRef<T>.Copy(constref aSrc: TGSharedRef<T>; var aDst: TGSharedRef<T>);
begin
  if @aSrc <> @aDst then
    begin
      aDst.Release;
      if aSrc.FRefCount <> nil then
        begin
          InterLockedIncrement(aSrc.FRefCount^);
          aDst.FRefCount := aSrc.FRefCount;
          aDst.FInstance := aSrc.Instance;
        end;
    end;
end;

class operator TGSharedRef<T>.AddRef(var s: TGSharedRef<T>);
begin
  if s.FRefCount <> nil then
    InterLockedIncrement(s.FRefCount^);
end;

class operator TGSharedRef<T>.Implicit(var s: TGSharedRef<T>): T;
begin
  Result := s.Instance;
end;

class operator TGSharedRef<T>.Explicit(var s: TGSharedRef<T>): T;
begin
  Result := s.Instance;
end;

function TGSharedRef<T>.HasInstance: Boolean;
begin
  Result := FRefCount <> nil;
end;

procedure TGSharedRef<T>.Release;
begin
  if FRefCount <> nil then
    begin
      if InterlockedDecrement(FRefCount^) = 0 then
        begin
          System.Dispose(FRefCount);
          FInstance.Free;
        end;
      FRefCount := nil;
    end;
end;

{ TGUniqPtr }

function TGUniqPtr<T>.GetAllocated: Boolean;
begin
  Result := FPtr <> nil;
end;

function TGUniqPtr<T>.GetPtr: PValue;
begin
  if FPtr = nil then
    begin
      System.New(FPtr);
      FillChar(FPtr^, SizeOf(T), 0);
      FOwnsPtr := True;
    end;
  Result := FPtr;
end;

procedure TGUniqPtr<T>.FreePtr;
begin
  if OwnsPtr then
    begin
      if IsManaged then
        FPtr^ := Default(T);
      System.Dispose(FPtr);
    end;
end;

function TGUniqPtr<T>.GetValue: T;
begin
  Result := Ptr^;
end;

procedure TGUniqPtr<T>.SetValue(const aValue: T);
begin
  Ptr^ := aValue;
end;

class constructor TGUniqPtr<T>.Init;
begin
  CFIsManaged := Rtti.IsManaged(TypeInfo(T));
end;

class operator TGUniqPtr<T>.Initialize(var u: TGUniqPtr<T>);
begin
  u.FPtr := nil;
  u.FOwnsPtr := False;
end;

class operator TGUniqPtr<T>.Finalize(var u: TGUniqPtr<T>);
begin
  u.Clear;
end;

class operator TGUniqPtr<T>.Copy(constref aSrc: TGUniqPtr<T>; var aDst: TGUniqPtr<T>);
begin
  if @aSrc <> @aDst then
    raise EInvalidOpException.Create(SECopyInadmissible);
end;

class operator TGUniqPtr<T>.AddRef(var u: TGUniqPtr<T>);
begin
  u.FOwnsPtr := False;
end;

class operator TGUniqPtr<T>.Implicit(var u: TGUniqPtr<T>): T;
begin
  Result := u.Ptr^;
end;

class operator TGUniqPtr<T>.Explicit(var u: TGUniqPtr<T>): T;
begin
  Result := u.Ptr^;
end;

procedure TGUniqPtr<T>.Clear;
begin
  FreePtr;
  FPtr := nil;
  FOwnsPtr := False;
end;

procedure TGUniqPtr<T>.OwnMove(var aPtr: TGUniqPtr<T>);
begin
  if FPtr = nil then
    exit;
  if OwnsPtr then
    begin
      aPtr.FreePtr;
      aPtr.FPtr := FPtr;
      aPtr.FOwnsPtr := True;
      FPtr := nil;
      FOwnsPtr := False;
    end
  else
    raise EInvalidOpException.Create(SEOwnRequired);
end;

{ TGCowPtr }

function TGCowPtr<T>.NewInstance: PInstance;
begin
  System.New(FInstance);
  FInstance^.RefCount := 1;
  FillChar(FInstance^.Value, SizeOf(T), 0);
  Result := FInstance;
end;

procedure TGCowPtr<T>.ReleaseInstance;
begin
  if InterlockedDecrement(FInstance^.RefCount) = 0 then
    begin
      if IsManaged then
        FInstance^.Value := Default(T);
      System.Dispose(FInstance);
    end;
  FInstance := nil;
end;

function TGCowPtr<T>.GetAllocated: Boolean;
begin
  Result := FInstance <> nil;
end;

function TGCowPtr<T>.GetRefCount: Integer;
begin
  if FInstance <> nil then
    exit(FInstance^.RefCount);
  Result := 0;
end;

function TGCowPtr<T>.GetPtr: PValue;
begin
  if FInstance = nil then
    NewInstance;
  Result := @FInstance^.Value;
end;

function TGCowPtr<T>.GetUniqPtr: PValue;
begin
  Unique;
  Result := GetPtr;
end;

function TGCowPtr<T>.GetValue: T;
begin
  Result := GetPtr^;
end;

procedure TGCowPtr<T>.SetValue(const aValue: T);
begin
  if (FInstance <> nil) and (FInstance^.RefCount > 1) then
    ReleaseInstance;
  GetPtr^ := aValue;
end;

class constructor TGCowPtr<T>.Init;
begin
  CFIsManaged := Rtti.IsManaged(TypeInfo(T));
end;

class operator TGCowPtr<T>.Initialize(var cp: TGCowPtr<T>);
begin
  cp.FInstance := nil;
end;

class operator TGCowPtr<T>.Finalize(var cp: TGCowPtr<T>);
begin
  cp.Release;
end;

class operator TGCowPtr<T>.Copy(constref aSrc: TGCowPtr<T>; var aDst: TGCowPtr<T>);
begin
  if @aSrc <> @aDst then
    begin
      aDst.Release;
      if aSrc.FInstance <> nil then
        begin
          aDst.FInstance := aSrc.FInstance;
          InterLockedIncrement(aSrc.FInstance^.RefCount);
        end;
    end;
end;

class operator TGCowPtr<T>.AddRef(var cp: TGCowPtr<T>);
begin
  if cp.FInstance <> nil then
    InterLockedIncrement(cp.FInstance^.RefCount);
end;

class operator TGCowPtr<T>.Implicit(var cp: TGCowPtr<T>): T;
begin
  Result := cp.GetPtr^;
end;

class operator TGCowPtr<T>.Explicit(var cp: TGCowPtr<T>): T;
begin
  Result := cp.GetPtr^;
end;

procedure TGCowPtr<T>.Release;
begin
  if FInstance <> nil then
    ReleaseInstance;
end;

procedure TGCowPtr<T>.Unique;
var
  Old: PInstance;
begin
  if (FInstance <> nil) and (FInstance^.RefCount > 1) then
    begin
      Old := FInstance;
      NewInstance^.Value := Old^.Value;
      InterlockedDecrement(Old^.RefCount);
    end;
end;

{ TGCowDynArray<T>.TEnumerator }

function TGCowDynArray<T>.TEnumerator.GetCurrent: T;
begin
  Result := FCurrent^;
end;

function TGCowDynArray<T>.TEnumerator.MoveNext: Boolean;
begin
  if FCurrent < FLast then
    begin
      Inc(FCurrent);
      exit(True);
    end;
  Result := False;
end;

{ TGCowDynArray<T>.TReverseEnumerator }

function TGCowDynArray<T>.TReverseEnumerator.GetCurrent: T;
begin
  Result := FCurrent^;
end;

function TGCowDynArray<T>.TReverseEnumerator.MoveNext: Boolean;
begin
  if FCurrent > FFirst then
    begin
      Dec(FCurrent);
      exit(True);
    end;
  Result := False;
end;

{ TGCowDynArray<T>.TReverse }

function TGCowDynArray<T>.TReverse.GetEnumerator: TReverseEnumerator;
begin
  Result := FArray^.GetReverseEnumerator;
end;

{ TGCowDynArray }

function TGCowDynArray<T>.NewInstance: PInstance;
begin
  System.New(Result);
  Result^.FItems := nil;
  Result^.FLength := 0;
  Result^.FRefCount := 1;
end;

procedure TGCowDynArray<T>.UniqInstance;
var
  OldInstance: PInstance;
begin
  OldInstance := FInstance;
  FInstance := NewInstance;
  FInstance^.FLength := OldInstance^.FLength;
  if FInstance^.FLength > 0 then
    begin
      FInstance^.FItems := System.GetMem(FInstance^.FLength * SizeOf(T));
      if ItemTypeIsManaged then
        begin
          System.FillChar(FInstance^.FItems^, FInstance^.FLength * SizeOf(T), 0);
          CopyItems(OldInstance^.FItems, FInstance^.FItems, FInstance^.FLength);
        end
      else
        System.Move(OldInstance^.FItems^, FInstance^.FItems^, FInstance^.FLength * SizeOf(T));
    end;
  InterlockedDecrement(OldInstance^.FRefCount);
end;

procedure TGCowDynArray<T>.ReallocUniq(aNewLen: SizeInt);
var
  OldInstance: PInstance;
begin
  OldInstance := FInstance;
  FInstance := NewInstance;
  FInstance^.FLength := aNewLen;
  if FInstance^.FLength > 0 then
    begin
      FInstance^.FItems := System.GetMem(FInstance^.FLength * SizeOf(T));
      if ItemTypeIsManaged then
        System.FillChar(FInstance^.FItems^, FInstance^.FLength * SizeOf(T), 0);
      CopyItems(OldInstance^.FItems, FInstance^.FItems, Math.Min(FInstance^.FLength, OldInstance^.FLength));
    end;
  InterlockedDecrement(OldInstance^.FRefCount);
end;

procedure TGCowDynArray<T>.Realloc(aNewLen: SizeInt);
begin
  if ItemTypeIsManaged then
    ReallocManaged(aNewLen)
  else
    FInstance^.FItems := System.ReallocMem(FInstance^.FItems, aNewLen * SizeOf(T));
  FInstance^.FLength := aNewLen;
end;

procedure TGCowDynArray<T>.ReallocManaged(aNewLen: SizeInt);
var
  Tmp: PItem;
begin
  Tmp := System.GetMem(aNewLen * SizeOf(T));
  with FInstance^ do
    begin
      if aNewLen > FLength then
        begin
          if FLength <> 0 then
            begin
              System.Move(FItems^, Tmp^, FLength * SizeOf(T));
              System.FillChar(FItems^, FLength * SizeOf(T), 0);
            end;
          System.FillChar(Tmp[FLength], (aNewLen - FLength) * SizeOf(T), 0);
        end
      else  //aNewLen < aOldLen
        begin
          System.Move(FItems^, Tmp^, aNewLen * SizeOf(T));
          System.FillChar(FItems^, aNewLen * SizeOf(T), 0);
          FillItems(FItems + aNewLen, FLength - aNewLen, Default(T));
        end;
      System.FreeMem(FItems);
      FItems := Tmp;
    end;
end;

function TGCowDynArray<T>.GetLength: SizeInt;
begin
  if FInstance <> nil then
    exit(FInstance^.FLength);
  Result := 0;
end;

function TGCowDynArray<T>.GetRefCount: Integer;
begin
  if FInstance <> nil then
    exit(FInstance^.FRefCount);
  Result := 0;
end;

function TGCowDynArray<T>.GetItem(aIndex: SizeInt): T;
begin
  if (FInstance <> nil) and (SizeUInt(aIndex) < SizeUInt(FInstance^.FLength)) then
    Result := FInstance^.FItems[aIndex]
  else
    raise EArgumentOutOfRangeException.CreateFmt(SEIndexOutOfBoundsFmt, [aIndex]);
end;

function TGCowDynArray<T>.GetPtr: PItem;
begin
  if FInstance = nil then
    FInstance := NewInstance;
  Result := FInstance^.FItems;
end;

function TGCowDynArray<T>.GetUniqPtr: PItem;
begin
  if FInstance = nil then
    FInstance := NewInstance
  else
    if FInstance^.FRefCount > 1 then
      UniqInstance;
  Result := FInstance^.FItems;
end;

procedure TGCowDynArray<T>.SetLen(aValue: SizeInt);
begin
  if aValue <> Length then
    begin
      if aValue = 0 then
        begin
          Release;
          exit;
        end;
      if aValue > 0 then
        begin
          if FInstance = nil then
            FInstance := NewInstance;
          if FInstance^.FRefCount > 1 then
            ReallocUniq(aValue)
          else
            Realloc(aValue);
        end
      else
        raise EInvalidOpException.Create(SECantAcceptNegLen);
    end;
end;

procedure TGCowDynArray<T>.SetItem(aIndex: SizeInt; const aValue: T);
begin
  if (FInstance <> nil) and (SizeUInt(aIndex) < SizeUInt(FInstance^.FLength)) then
    begin
      if FInstance^.FRefCount > 1 then
        UniqInstance;
      FInstance^.FItems[aIndex] := aValue;
    end
  else
    raise EArgumentOutOfRangeException.CreateFmt(SEIndexOutOfBoundsFmt, [aIndex]);
end;

function TGCowDynArray<T>.GetHigh: SizeInt;
begin
  if FInstance <> nil then
    exit(Pred(FInstance^.FLength));
  Result := NULL_INDEX;
end;

class constructor TGCowDynArray<T>.Init;
begin
  CFIsManaged := IsManaged(TypeInfo(T));
end;

class procedure TGCowDynArray<T>.FillItems(aFrom: PItem; aCount: SizeInt; const aValue: T);
begin
  while aCount >= 4 do
    begin
      aFrom[0] := aValue;
      aFrom[1] := aValue;
      aFrom[2] := aValue;
      aFrom[3] := aValue;
      aFrom += 4;
      aCount -= 4;
    end;
  case aCount of
    1: aFrom[0] := aValue;
    2:
      begin
        aFrom[0] := aValue;
        aFrom[1] := aValue;
      end;
    3:
      begin
        aFrom[0] := aValue;
        aFrom[1] := aValue;
        aFrom[2] := aValue;
      end;
  else
  end;
end;

class procedure TGCowDynArray<T>.CopyItems(aSrc, aDst: PItem; aCount: SizeInt);
var
  I: SizeInt;
begin
  I := 0;
  while I <= aCount - 4 do
    begin
      aDst[I  ] := aSrc[I  ];
      aDst[I+1] := aSrc[I+1];
      aDst[I+2] := aSrc[I+2];
      aDst[I+3] := aSrc[I+3];
      I += 4;
    end;
  case aCount - I of
    1: aDst[I] := aSrc[I];
    2:
      begin
        aDst[I  ] := aSrc[I  ];
        aDst[I+1] := aSrc[I+1];
      end;
    3:
      begin
        aDst[I  ] := aSrc[I  ];
        aDst[I+1] := aSrc[I+1];
        aDst[I+2] := aSrc[I+2];
      end;
  else
  end;
end;

class operator TGCowDynArray<T>.Initialize(var a: TGCowDynArray<T>);
begin
  a.FInstance := nil;
end;

class operator TGCowDynArray<T>.Finalize(var a: TGCowDynArray<T>);
begin
  a.Release;
end;

class operator TGCowDynArray<T>.Copy(constref aSrc: TGCowDynArray<T>; var aDst: TGCowDynArray<T>);
begin
  if @aSrc <> @aDst then
    begin
      aDst.Release;
      if aSrc.FInstance <> nil then
        begin
          InterLockedIncrement(aSrc.FInstance^.FRefCount);
          aDst.FInstance := aSrc.FInstance;
        end;
    end;
end;

class operator TGCowDynArray<T>.AddRef(var a: TGCowDynArray<T>);
begin
  if a.FInstance <> nil then
    InterLockedIncrement(a.FInstance^.FRefCount);
end;

function TGCowDynArray<T>.GetReverseEnumerator: TReverseEnumerator;
begin
  if (FInstance <> nil) and (FInstance^.FItems <> nil) then
    begin
      Result.FCurrent := FInstance^.FItems + FInstance^.FLength;
      Result.FFirst := FInstance^.FItems;
      exit;
    end;
  Result.FCurrent := nil;
  Result.FFirst := nil;
end;

function TGCowDynArray<T>.GetEnumerator: TEnumerator;
begin
  if (FInstance <> nil) and (FInstance^.FItems <> nil) then
    begin
      Result.FCurrent := FInstance^.FItems - 1;
      Result.FLast := FInstance^.FItems + FInstance^.FLength - 1;
      exit;
    end;
  Result.FCurrent := nil;
  Result.FLast := nil;
end;

function TGCowDynArray<T>.Reverse: TReverse;
begin
  Result.FArray := @Self;
end;

function TGCowDynArray<T>.IsEmpty: Boolean;
begin
  if FInstance <> nil then
    exit(FInstance^.FItems = nil);
  Result := True;
end;

function TGCowDynArray<T>.NonEmpty: Boolean;
begin
  if FInstance <> nil then
    exit(FInstance^.FItems <> nil);
  Result := False;
end;

procedure TGCowDynArray<T>.Release;
begin
  if (FInstance <> nil) and (InterlockedDecrement(FInstance^.FRefCount) = 0) then
    begin
      if FInstance^.FItems <> nil then
        begin
          if ItemTypeIsManaged then
            FillItems(FInstance^.FItems, FInstance^.FLength, Default(T));
          System.FreeMem(FInstance^.FItems);
        end;
      System.Dispose(FInstance);
    end;
  FInstance := nil;
end;

procedure TGCowDynArray<T>.Unique;
begin
  if (FInstance <> nil) and (FInstance^.FRefCount > 1) then
    UniqInstance;
end;

procedure TGCowDynArray<T>.Fill(aCount: SizeInt; constref aValue: T);
begin
  Release;
  if aCount < 1 then
    exit;
  Length := aCount;
  FillItems(FInstance^.FItems, aCount, aValue);
end;

function TGCowDynArray<T>.CreateCopy(aFromIndex, aCount: SizeInt): TGCowDynArray<T>;
begin
  if aFromIndex < 0 then
    aFromIndex := 0;
  Result{%H-}.Release;
  if (aFromIndex >= Length) or (aCount < 1) then
    exit;
  aCount := Math.Min(aCount, Length - aFromIndex);
  Result.Length := aCount;
  if ItemTypeIsManaged then
    CopyItems(FInstance^.FItems + aFromIndex, Result.FInstance^.FItems, aCount)
  else
    System.Move((FInstance^.FItems + aFromIndex)^, Result.FInstance^.FItems^, aCount * SizeOf(T));
end;

{ TGMapEntry }

constructor TGMapEntry<TKey, TValue>.Create(constref aKey: TKey; constref aValue: TValue);
begin
  Key := aKey;
  Value := aValue;
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
  aNode^.NextLink := FFreeListHead;
  FFreeListHead := aNode;
  Inc(FFreeCount);
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

{ TGTuple2 }

constructor TGTuple2<T1, T2>.Create(constref v1: T1; constref v2: T2);
begin
  F1 := v1;
  F2 := v2;
end;

{ TGTuple3 }

constructor TGTuple3<T1, T2, T3>.Create(constref v1: T1; constref v2: T2; constref v3: T3);
begin
  F1 := v1;
  F2 := v2;
  F3 := v3;
end;

{ TGTuple4 }

constructor TGTuple4<T1, T2, T3, T4>.Create(constref v1: T1; constref v2: T2; constref v3: T3;
  constref v4: T4);
begin
  F1 := v1;
  F2 := v2;
  F3 := v3;
  F4 := v4;
end;

{ TGTuple5 }

constructor TGTuple5<T1, T2, T3, T4, T5>.Create(constref v1: T1; constref v2: T2; constref v3: T3;
  constref v4: T4; constref v5: T5);
begin
  F1 := v1;
  F2 := v2;
  F3 := v3;
  F4 := v4;
  F5 := v5;
end;

{ TGAddMonoid }

class function TGAddMonoid<T>.GetIdentity: T;
begin
  Result := Default(T);
end;

class function TGAddMonoid<T>.BinOp(const L, R: T): T;
begin
  Result := L + R;
end;

{ TGAddMonoidEx }

class function TGAddMonoidEx<T>.GetZeroConst: T;
begin
  Result := Default(T);
end;

class function TGAddMonoidEx<T>.AddConst(constref aValue, aConst: T; aSize: SizeInt): T;
begin
  Result := aValue + aConst * aSize;
end;

class function TGAddMonoidEx<T>.IsZeroConst(constref aValue: T): Boolean;
begin
  Result := aValue = Default(T);
end;

{ TGMaxMonoid }

class function TGMaxMonoid<T>.GetIdentity: T;
begin
  Result := T.MinValue;
end;

class function TGMaxMonoid<T>.BinOp(const L, R: T): T;
begin
  if L < R then
    Result := R
  else
    Result := L
end;

{ TGMaxMonoidEx }

class function TGMaxMonoidEx<T>.GetZeroConst: T;
begin
  Result := Default(T);
end;

class function TGMaxMonoidEx<T>.AddConst(constref aValue, aConst: T; aSize: SizeInt): T;
begin
  Result := aValue + aConst;
end;

class function TGMaxMonoidEx<T>.IsZeroConst(constref aValue: T): Boolean;
begin
  Result := aValue = Default(T);
end;

{ TGMinMonoid }

class function TGMinMonoid<T>.GetIdentity: T;
begin
  Result := T.MaxValue;
end;

class function TGMinMonoid<T>.BinOp(const L, R: T): T;
begin
  if R < L then
    Result := R
  else
    Result := L
end;

{ TGMinMonoidEx }

class function TGMinMonoidEx<T>.GetZeroConst: T;
begin
  Result := Default(T);
end;

class function TGMinMonoidEx<T>.AddConst(constref aValue, aConst: T; aSize: SizeInt): T;
begin
  Result := aValue + aConst;
end;

class function TGMinMonoidEx<T>.IsZeroConst(constref aValue: T): Boolean;
begin
  Result := aValue = Default(T);
end;

{ TGMaxPos }

class function TGMaxPos<T>.GetIdentity: TGMaxPos<T>;
begin
  Result.Value := T.MinValue;
  Result.Index := NULL_INDEX;
end;

class function TGMaxPos<T>.BinOp(const L, R: TGMaxPos<T>): TGMaxPos<T>;
begin
  if R.Value > L.Value then
    Result := R
  else
    Result := L;
end;

{ TGMinPos }

class function TGMinPos<T>.GetIdentity: TGMinPos<T>;
begin
  Result.Value := T.MaxValue;
  Result.Index := NULL_INDEX;
end;

class function TGMinPos<T>.BinOp(const L, R: TGMinPos<T>): TGMinPos<T>;
begin
  if R.Value < L.Value then
    Result := R
  else
    Result := L;
end;

{ TSpinLock }

class operator TSpinLock.Initialize(var sl: TSpinLock);
begin
  sl.FState := 0;
  Assert(SizeOf(sl.FCacheLinePad) = TSpinLock.CACHE_PAD_SIZE);//to supress hints
end;

procedure TSpinLock.Lock;
begin
  while Boolean(InterlockedExchange(FState, DWord(1))) do
    ThreadSwitch;
end;

procedure TSpinLock.LockTts;
begin
  repeat
    while Boolean(FState) do;
    if not Boolean(InterlockedExchange(FState, DWord(1))) then
      exit;
  until False;
end;

function TSpinLock.TryLock: Boolean;
begin
  Result := not Boolean(InterlockedExchange(FState, DWord(1)));
end;

procedure TSpinLock.Unlock;
begin
  InterlockedExchange(FState, DWord(0));
  //FState := 0;
  //ReadWriteBarrier;
end;

end.

