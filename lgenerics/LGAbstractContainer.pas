{****************************************************************************
*                                                                           *
*   This file is part of the LGenerics package.                             *
*   Common abstact container classes.                                       *
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
unit LGAbstractContainer;

{$MODE OBJFPC}{$H+}
{$INLINE ON}
{$MODESWITCH ADVANCEDRECORDS}
{$MODESWITCH NESTEDPROCVARS}

interface

uses

  SysUtils,
  math,
  LGUtils,
  {%H-}LGHelpers,
  LGArrayHelpers,
  LGStrConst;

type
  generic TGEnumerable<T> = class abstract(TObject, specialize IGEnumerable<T>, IObjInstance)
  public
  type
    TSpecEnumerable = specialize TGEnumerable<T>;
    TSpecEnumerator = specialize TGEnumerator<T>;
    IEnumerable     = specialize IGEnumerable<T>;
    TOptional       = specialize TGOptional<T>;
    TArray          = specialize TGArray<T>;
    TCompare        = specialize TGCompare<T>;
    TOnCompare      = specialize TGOnCompare<T>;
    TNestCompare    = specialize TGNestCompare<T>;
    TTest           = specialize TGTest<T>;
    TOnTest         = specialize TGOnTest<T>;
    TNestTest       = specialize TGNestTest<T>;
    TMapFunc        = specialize TGMapFunc<T, T>;
    TOnMap          = specialize TGOnMap<T, T>;
    TNestMap        = specialize TGNestMap<T, T>;
    TFold           = specialize TGFold<T, T>;
    TOnFold         = specialize TGOnFold<T, T>;
    TNestFold       = specialize TGNestFold<T, T>;
    TDefaults       = specialize TGDefaults<T>;
    TItem           = T;

  protected
  type
    PItem = ^T;

    function  _GetRef: TObject; inline;
    procedure Discard;
  public
    function GetEnumerator: TSpecEnumerator; virtual; abstract;
  { enumerates elements in reverse order }
    function Reverse: IEnumerable; virtual;
    function ToArray: TArray; virtual;
    function Any: Boolean;
    function None: Boolean;
    function Total: SizeInt;
    function FindFirst(out aValue: T): Boolean;
    function First: TOptional;
    function FindLast(out aValue: T): Boolean;
    function Last: TOptional;
    function FindMin(out aValue: T; c: TCompare): Boolean;
    function FindMin(out aValue: T; c: TOnCompare): Boolean;
    function FindMin(out aValue: T; c: TNestCompare): Boolean;
    function Min(c: TCompare): TOptional;
    function Min(c: TOnCompare): TOptional;
    function Min(c: TNestCompare): TOptional;
    function FindMax(out aValue: T; c: TCompare): Boolean;
    function FindMax(out aValue: T; c: TOnCompare): Boolean;
    function FindMax(out aValue: T; c: TNestCompare): Boolean;
    function Max(c: TCompare): TOptional;
    function Max(c: TOnCompare): TOptional;
    function Max(c: TNestCompare): TOptional;
    function Skip(aCount: SizeInt): IEnumerable; inline;
    function Limit(aCount: SizeInt): IEnumerable; inline;
    function Sorted(c: TCompare): IEnumerable;
    function Sorted(c: TOnCompare): IEnumerable;
    function Sorted(c: TNestCompare): IEnumerable;
    function Select(aTest: TTest): IEnumerable; inline;
    function Select(aTest: TOnTest): IEnumerable; inline;
    function Select(aTest: TNestTest): IEnumerable; inline;
    function Any(aTest: TTest): Boolean;
    function Any(aTest: TOnTest): Boolean;
    function Any(aTest: TNestTest): Boolean;
    function None(aTest: TTest): Boolean; inline;
    function None(aTest: TOnTest): Boolean; inline;
    function None(aTest: TNestTest): Boolean; inline;
    function All(aTest: TTest): Boolean;
    function All(aTest: TOnTest): Boolean;
    function All(aTest: TNestTest): Boolean;
    function Total(aTest: TTest): SizeInt;
    function Total(aTest: TOnTest): SizeInt;
    function Total(aTest: TNestTest): SizeInt;
    function Distinct(c: TCompare): IEnumerable;
    function Distinct(c: TOnCompare): IEnumerable;
    function Distinct(c: TNestCompare): IEnumerable;
    function Map(aMap: TMapFunc): IEnumerable; inline;
    function Map(aMap: TOnMap): IEnumerable; inline;
    function Map(aMap: TNestMap): IEnumerable; inline;
  { left-associative linear fold }
    function Fold(aFold: TFold; constref v0: T): T;
    function Fold(aFold: TFold): TOptional;
    function Fold(aFold: TOnFold; constref v0: T): T;
    function Fold(aFold: TOnFold): TOptional;
    function Fold(aFold: TNestFold; constref v0: T): T;
    function Fold(aFold: TNestFold): TOptional;
  end;

{$I EnumsH.inc}

  { TGAbstractContainer: container abstract ancestor class }
  generic TGAbstractContainer<T> = class abstract(specialize TGEnumerable<T>, specialize IGContainer<T>)
  public
  type
    TSpecContainer = specialize TGAbstractContainer<T>;

  protected
  type
    //to supress unnecessary refcounting
    TFake = {$IFNDEF FPC_REQUIRES_PROPER_ALIGNMENT}array[0..Pred(SizeOf(T))] of Byte{$ELSE}T{$ENDIF};
    TFakeArray = array of TFake;

    TContainerEnumerator = class(specialize TGEnumerator<T>)
    strict protected
      FOwner: TSpecContainer;
    public
      constructor Create(c: TSpecContainer);
      destructor Destroy; override;
    end;

    TContainerEnumerable = class(specialize TGAutoEnumerable<T>)
    strict protected
      FOwner: TSpecContainer;
    public
      constructor Create(c: TSpecContainer);
      destructor Destroy; override;
    end;

  strict private
    FItCounter: Integer;
    function  GetInIteration: Boolean; inline;
  protected
    procedure CapacityExceedError(aValue: SizeInt);
    procedure AccessEmptyError;
    procedure IndexOutOfBoundError(aIndex: SizeInt);
    procedure UpdateLockError;
    procedure CheckInIteration; inline;
    procedure BeginIteration;
    procedure EndIteration;
    function  GetCount: SizeInt; virtual; abstract;
    function  GetCapacity: SizeInt; virtual; abstract;
    function  DoGetEnumerator: TSpecEnumerator;  virtual; abstract;
    procedure DoClear; virtual; abstract;
    procedure DoTrimToFit; virtual; abstract;
    procedure DoEnsureCapacity(aValue: SizeInt); virtual; abstract;
    procedure CopyItems(aBuffer: PItem); virtual;
    property  InIteration: Boolean read GetInIteration;
  public
    function  GetEnumerator: TSpecEnumerator; override;
    function  ToArray: TArray; override;
    function  IsEmpty: Boolean; inline;
    function  NonEmpty: Boolean; inline;
    procedure Clear;
    procedure TrimToFit;
    procedure EnsureCapacity(aValue: SizeInt);
    property  Count: SizeInt read GetCount;
    property  Capacity: SizeInt read GetCapacity;
  end;

{$I DynBufferH.inc}

  { TGAbstractCollection: collection abstract ancestor class}
  generic TGAbstractCollection<T> = class abstract(specialize TGAbstractContainer<T>, specialize IGCollection<T>,
    specialize IGReadOnlyCollection<T>)
  public
  type
    TSpecCollection = specialize TGAbstractCollection<T>;
    ICollection     = specialize IGCollection<T>;

  protected
    function  DoAdd(constref aValue: T): Boolean; virtual; abstract;
    function  DoExtract(constref aValue: T): Boolean; virtual; abstract;
    function  DoRemoveIf(aTest: TTest): SizeInt; virtual; abstract;
    function  DoRemoveIf(aTest: TOnTest): SizeInt; virtual; abstract;
    function  DoRemoveIf(aTest: TNestTest): SizeInt; virtual; abstract;
    function  DoExtractIf(aTest: TTest): TArray; virtual; abstract;
    function  DoExtractIf(aTest: TOnTest): TArray; virtual; abstract;
    function  DoExtractIf(aTest: TNestTest): TArray; virtual; abstract;

    function  DoRemove(constref aValue: T): Boolean; virtual;
    function  DoAddAll(constref a: array of T): SizeInt; virtual; overload;
    function  DoAddAll(e: IEnumerable): SizeInt; virtual; abstract; overload;
    function  DoRemoveAll(constref a: array of T): SizeInt;
    function  DoRemoveAll(e: IEnumerable): SizeInt; virtual;
  public
  { returns True if element added }
    function  Add(constref aValue: T): Boolean;
  { returns count of added elements }
    function  AddAll(constref a: array of T): SizeInt;
  { returns count of added elements }
    function  AddAll(e: IEnumerable): SizeInt;
    function  Contains(constref aValue: T): Boolean; virtual; abstract;
    function  NonContains(constref aValue: T): Boolean;
    function  ContainsAny(constref a: array of T): Boolean;
    function  ContainsAny(e: IEnumerable): Boolean;
    function  ContainsAll(constref a: array of T): Boolean;
    function  ContainsAll(e: IEnumerable): Boolean;
  { returns True if element removed }
    function  Remove(constref aValue: T): Boolean;
  { returns count of removed elements }
    function  RemoveAll(constref a: array of T): SizeInt;
  { returns count of removed elements }
    function  RemoveAll(e: IEnumerable): SizeInt;
  { returns count of removed elements }
    function  RemoveIf(aTest: TTest): SizeInt;
    function  RemoveIf(aTest: TOnTest): SizeInt;
    function  RemoveIf(aTest: TNestTest): SizeInt;
  { returns True if element extracted }
    function  Extract(constref aValue: T): Boolean;
    function  ExtractIf(aTest: TTest): TArray;
    function  ExtractIf(aTest: TOnTest): TArray;
    function  ExtractIf(aTest: TNestTest): TArray;
  { will contain only those elements that are simultaneously contained in self and aCollection }
    procedure RetainAll(aCollection: ICollection);
    function  Clone: TSpecCollection; virtual; abstract;
  end;

  { TGThreadCollection: mutex based concurrent collection }
  generic TGThreadCollection<T> = class
  public
  type
    ICollection = specialize IGCollection<T>;

  private
    FCollection: ICollection;
    FLock: TRTLCriticalSection;
    FOwnsColl: Boolean;
  protected
    procedure Lock; inline;
  public
    constructor Create(aCollection: ICollection; aOwnsCollection: Boolean = True);
    destructor Destroy; override;
    function  LockCollection: ICollection;
    procedure Unlock; inline;
    procedure Clear;
    function  Contains(constref aValue: T): Boolean;
    function  Add(constref aValue: T): Boolean;
    function  Remove(constref aValue: T): Boolean;
    property  OwnsCollection: Boolean read FOwnsColl;
  end;

  { TGThreadRWCollection: RWLock based concurrent collection }
  generic TGThreadRWCollection<T> = class
  public
  type
    ICollection = specialize IGCollection<T>;

  private
  type
    TCollection = specialize TGAbstractCollection<T>;

  var
    FCollection: ICollection;
    FRWLock: TMultiReadExclusiveWriteSynchronizer;
    FOwnsColl: Boolean;
    function GetCapacity: SizeInt;
    function GetCount: SizeInt;
  protected
    procedure BeginRead; inline;
    procedure BeginWrite; inline;
  public
  type
    IRoCollection = specialize IGReadOnlyCollection<T>;

    constructor Create(aCollection: ICollection; aOwnsCollection: Boolean = True);
    destructor Destroy; override;
    function  ReadCollection: IRoCollection;
    procedure EndRead; inline;
    function  WriteCollection: ICollection;
    procedure EndWrite; inline;
    function  Contains(constref aValue: T): Boolean;
    function  Add(constref aValue: T): Boolean;
    function  Remove(constref aValue: T): Boolean;
    property  Count: SizeInt read GetCount;
    property  Capacity: SizeInt read GetCapacity;
    property  OwnsCollection: Boolean read FOwnsColl;
  end;

  { TGAbstractSet: set abstract ancestor class }
  generic TGAbstractSet<T> = class abstract(specialize TGAbstractCollection<T>)
  public
  type
    TSpecSet = specialize TGAbstractSet<T>;

  protected
  type
    TEntry = record
      Key: T;
    end;
    PEntry = ^TEntry;

    TExtractHelper = object
    private
      FCurrIndex: SizeInt;
      FExtracted: TArray;
    public
      procedure OnExtract(p: PEntry);
      procedure Init;
      function  Final: TArray;
    end;

    function  DoAddAll(e: IEnumerable): SizeInt; override; overload;
    procedure DoSymmetricSubtract(aSet: TSpecSet);
  public
    function  IsSuperset(aSet: TSpecSet): Boolean;
    function  IsSubset(aSet: TSpecSet): Boolean; inline;
    function  IsEqual(aSet: TSpecSet): Boolean;
    function  Intersecting(aSet: TSpecSet): Boolean; inline;
    procedure Intersect(aSet: TSpecSet);
    procedure Join(aSet: TSpecSet);
    procedure Subtract(aSet: TSpecSet);
    procedure SymmetricSubtract(aSet: TSpecSet);
  end;

  generic TGMultiSetEntry<T> = record
    Key: T;
    Count: SizeInt; //multiplicity(count of occurrences)
  end;

  { TGAbstractMultiSet: multiSet abstract ancestor class  }
  generic TGAbstractMultiSet<T> = class abstract(specialize TGAbstractCollection<T>)
  public
  type
    TEntry           = specialize TGMultiSetEntry<T>;
    TSpecMultiSet    = specialize TGAbstractMultiSet<T>;
    IEntryEnumerable = specialize IGEnumerable<TEntry>;

  protected
  type
    PEntry = ^TEntry;

    TExtractHelper = object
    private
      FCurrIndex: SizeInt;
      FExtracted: TArray;
    public
      procedure OnExtract(p: PEntry);
      procedure Init;
      function  Final: TArray;
    end;

    TIntersectHelper = object
      FSet,
      FOtherSet: TSpecMultiSet;
      function OnIntersect(p: PEntry): Boolean;
    end;

  var
    FCount: SizeInt;
    function  FindEntry(constref aKey: T): PEntry; virtual; abstract;
    //return True if aKey found, otherwise inserts entry (garbage) and return False;
    function  FindOrAdd(constref aKey: T; out p: PEntry): Boolean; virtual; abstract;
    //returns True only if e removed
    function  DoSubEntry(constref e: TEntry): Boolean; virtual; abstract;
    //returns True only if e removed
    function  DoSymmSubEntry(constref e: TEntry): Boolean; virtual; abstract;
    function  GetEntryCount: SizeInt; virtual; abstract;
    function  DoDoubleEntryCounters: SizeInt; virtual; abstract;
    function  GetDistinct: IEnumerable; virtual; abstract;  // distinct keys
    function  GetEntries: IEntryEnumerable; virtual; abstract;
    procedure DoIntersect(aSet: TSpecMultiSet); virtual; abstract;

    function  GetCount: SizeInt; override;
    procedure DoJoinEntry(constref e: TEntry);
    procedure DoAddEntry(constref e: TEntry);
    function  GetKeyCount(const aKey: T): SizeInt;
    procedure SetKeyCount(const aKey: T; aValue: SizeInt);
    procedure DoArithAdd(aSet: TSpecMultiSet);
    procedure DoArithSubtract(aSet: TSpecMultiSet);
    procedure DoSymmSubtract(aSet: TSpecMultiSet);

    function  DoAdd(constref aKey: T): Boolean; override;
    function  DoAddAll(e: IEnumerable): SizeInt; override; overload;
    function  DoRemoveAll(e: IEnumerable): SizeInt; override;
    property  ElemCount: SizeInt read FCount;
  public
    function  Contains(constref aValue: T): Boolean; override;
  { returns True if multiplicity of an any key in self is greater then or equal to
    the multiplicity of that key in aSet }
    function  IsSuperSet(aSet: TSpecMultiSet): Boolean;
  { returns True if multiplicity of an any key in aSet is greater then or equal to
    the multiplicity of that key in self }
    function  IsSubSet(aSet: TSpecMultiSet): Boolean;
  { returns True if the multiplicity of an any key in self is equal to the multiplicity of that key in aSet }
    function  IsEqual(aSet: TSpecMultiSet): Boolean;
    function  Intersecting(aSet: TSpecMultiSet): Boolean;
  { will contain only those keys that are simultaneously contained in self and in aSet;
    the multiplicity of a key becomes equal to the MINIMUM of the multiplicities of a key in self and aSet }
    procedure Intersect(aSet: TSpecMultiSet);
  { will contain all keys that are contained in self or in aSet;
    the multiplicity of a key will become equal to the MAXIMUM of the multiplicities of
    a key in self and aSet }
    procedure Join(aSet: TSpecMultiSet);
  { will contain all keys that are contained in self or in aSet;
    the multiplicity of a key will become equal to the SUM of the multiplicities of a key in self and aSet }
    procedure ArithmeticAdd(aSet: TSpecMultiSet);
  { will contain only those keys whose multiplicity is greater then the multiplicity
    of that key in aSet; the multiplicity of a key will become equal to the difference of multiplicities
    of a key in self and aSet }
    procedure ArithmeticSubtract(aSet: TSpecMultiSet);
  { will contain only those keys whose multiplicity is not equal to the multiplicity
    of that key in aSet; the multiplicity of a key will become equal to absolute value of difference
    of the multiplicities of a key in self and aSet }
    procedure SymmetricSubtract(aSet: TSpecMultiSet);
  { enumerates underlying set - distinct keys only }
    function  Distinct: IEnumerable;
    function  Entries: IEntryEnumerable;
  { returs number of distinct keys }
    property  EntryCount: SizeInt read GetEntryCount; //dimension, Count - cardinality
  { will return 0 if not contains an element aValue;
    will raise EArgumentException if one try to set negative multiplicity of a aValue }
    property  Counts[const aValue: T]: SizeInt read GetKeyCount write SetKeyCount; default;
  end;

  { TSimpleIterable }

  TSimpleIterable = class
  private
    FItCounter: Integer;
    function  GetInIteration: Boolean; inline;
  protected
    procedure CapacityExceedError(aValue: SizeInt);
    procedure UpdateLockError;
    procedure CheckInIteration; inline;
    procedure BeginIteration; inline;
    procedure EndIteration; inline;
    property  InIteration: Boolean read GetInIteration;
  end;

  TMapObjectOwns   = (moOwnsKeys, moOwnsValues);
  TMapObjOwnership = set of TMapObjectOwns;

  const
    OWNS_BOTH = [moOwnsKeys, moOwnsValues];

  type
  { TGAbstractMap: map abstract ancestor class  }
  generic TGAbstractMap<TKey, TValue> = class abstract(TSimpleIterable, specialize IGMap<TKey, TValue>,
    specialize IGReadOnlyMap<TKey, TValue>)
  {must be  generic TGAbstractMap<TKey, TValue> = class abstract(
              specialize TGContainer<specialize TGMapEntry<TKey, TValue>>), but :( ... see #0033788}
  public
  type
    TSpecMap         = specialize TGAbstractMap<TKey, TValue>;
    TEntry           = specialize TGMapEntry<TKey, TValue>;
    IKeyEnumerable   = specialize IGEnumerable<TKey>;
    IValueEnumerable = specialize IGEnumerable<TValue>;
    IEntryEnumerable = specialize IGEnumerable<TEntry>;
    TEntryArray      = specialize TGArray<TEntry>;
    TKeyArray        = specialize TGArray<TKey>;
    TKeyTest         = specialize TGTest<TKey>;
    TOnKeyTest       = specialize TGOnTest<TKey>;
    TNestKeyTest     = specialize TGNestTest<TKey>;
    TKeyOptional     = specialize TGOptional<TKey>;
    TValueOptional   = specialize TGOptional<TValue>;
    TKeyCollection   = specialize TGAbstractCollection<TKey>;
    IKeyCollection   = specialize IGCollection<TKey>;

  protected
  type
    PEntry = ^TEntry;

    TExtractHelper = object
    private
      FCurrIndex: SizeInt;
      FExtracted: TEntryArray;
    public
      procedure OnExtract(p: PEntry);
      procedure Init;
      function  Final: TEntryArray;
    end;

    TCustomKeyEnumerable = class(specialize TGAutoEnumerable<TKey>)
    protected
      FOwner: TSpecMap;
    public
      constructor Create(aMap: TSpecMap);
      destructor Destroy; override;
    end;

    TCustomValueEnumerable = class(specialize TGAutoEnumerable<TValue>)
    protected
      FOwner: TSpecMap;
    public
      constructor Create(aMap: TSpecMap);
      destructor Destroy; override;
    end;

    TCustomEntryEnumerable = class(specialize TGAutoEnumerable<TEntry>)
    protected
      FOwner: TSpecMap;
    public
      constructor Create(aMap: TSpecMap);
      destructor Destroy; override;
    end;

    function  _GetRef: TObject;
    function  GetCount: SizeInt;  virtual; abstract;
    function  GetCapacity: SizeInt; virtual; abstract;
    function  Find(constref aKey: TKey): PEntry; virtual; abstract;
    //returns True if aKey found, otherwise inserts (garbage) entry and returns False;
    function  FindOrAdd(constref aKey: TKey; out p: PEntry): Boolean; virtual; abstract;
    function  DoExtract(constref aKey: TKey; out v: TValue): Boolean; virtual; abstract;
    function  DoRemoveIf(aTest: TKeyTest): SizeInt; virtual; abstract;
    function  DoRemoveIf(aTest: TOnKeyTest): SizeInt; virtual; abstract;
    function  DoRemoveIf(aTest: TNestKeyTest): SizeInt; virtual; abstract;
    function  DoExtractIf(aTest: TKeyTest): TEntryArray; virtual; abstract;
    function  DoExtractIf(aTest: TOnKeyTest): TEntryArray; virtual; abstract;
    function  DoExtractIf(aTest: TNestKeyTest): TEntryArray; virtual; abstract;

    function  DoRemove(constref aKey: TKey): Boolean; virtual;
    procedure DoClear; virtual; abstract;
    procedure DoEnsureCapacity(aValue: SizeInt); virtual; abstract;
    procedure DoTrimToFit; virtual; abstract;
    function  GetKeys: IKeyEnumerable; virtual; abstract;
    function  GetValues: IValueEnumerable; virtual; abstract;
    function  GetEntries: IEntryEnumerable; virtual; abstract;

    function  GetValue(const aKey: TKey): TValue; inline;
    function  DoSetValue(constref aKey: TKey; constref aNewValue: TValue): Boolean; virtual;
    function  DoAdd(constref aKey: TKey; constref aValue: TValue): Boolean;
    function  DoAddOrSetValue(const aKey: TKey; const aValue: TValue): Boolean; virtual;
    function  DoAddAll(constref a: array of TEntry): SizeInt;
    function  DoAddAll(e: IEntryEnumerable): SizeInt;
    function  DoRemoveAll(constref a: array of TKey): SizeInt;
    function  DoRemoveAll(e: IKeyEnumerable): SizeInt;
  public
    function  ToArray: TEntryArray;
    function  IsEmpty: Boolean; inline;
    function  NonEmpty: Boolean; inline;
    procedure Clear;
    procedure EnsureCapacity(aValue: SizeInt);
  { free unused memory if possible }
    procedure TrimToFit;
  { returns True and aValue mapped to aKey if contains aKey, False otherwise }
    function  TryGetValue(constref aKey: TKey; out aValue: TValue): Boolean;
  { returns Value mapped to aKey or aDefault }
    function  GetValueDef(constref aKey: TKey; constref aDefault: TValue = Default(TValue)): TValue; inline;
  { returns True and add TEntry(aKey, aValue) only if not contains aKey }
    function  Add(constref aKey: TKey; constref aValue: TValue): Boolean;
  { returns True and add e only if not contains e.Key }
    function  Add(constref e: TEntry): Boolean; inline;
    procedure AddOrSetValue(const aKey: TKey; const aValue: TValue);
  { returns True if e.Key added, False otherwise }
    function  AddOrSetValue(constref e: TEntry): Boolean;
  { will add only entries which keys are absent in map }
    function  AddAll(constref a: array of TEntry): SizeInt;
    function  AddAll(e: IEntryEnumerable): SizeInt;
  { returns True and map aNewValue to aKey only if contains aKey, False otherwise }
    function  Replace(constref aKey: TKey; constref aNewValue: TValue): Boolean;
    function  Contains(constref aKey: TKey): Boolean; inline;
    function  NonContains(constref aKey: TKey): Boolean;
    function  ContainsAny(constref a: array of TKey): Boolean;
    function  ContainsAny(e: IKeyEnumerable): Boolean;
    function  ContainsAll(constref a: array of TKey): Boolean;
    function  ContainsAll(e: IKeyEnumerable): Boolean;
    function  Remove(constref aKey: TKey): Boolean;
    function  RemoveAll(constref a: array of TKey): SizeInt;
    function  RemoveAll(e: IKeyEnumerable): SizeInt;
    function  RemoveIf(aTest: TKeyTest): SizeInt;
    function  RemoveIf(aTest: TOnKeyTest): SizeInt;
    function  RemoveIf(aTest: TNestKeyTest): SizeInt;
    function  Extract(constref aKey: TKey; out v: TValue): Boolean;
    function  ExtractIf(aTest: TKeyTest): TEntryArray;
    function  ExtractIf(aTest: TOnKeyTest): TEntryArray;
    function  ExtractIf(aTest: TNestKeyTest): TEntryArray;
    procedure RetainAll({%H-}aCollection: IKeyCollection);
    function  Clone: TSpecMap; virtual; abstract;
    function  Keys: IKeyEnumerable;
    function  Values: IValueEnumerable;
    function  Entries: IEntryEnumerable;
    property  Count: SizeInt read GetCount;
    property  Capacity: SizeInt read GetCapacity;
  { reading will raise ELGMapError if an aKey is not present in map }
    property  Items[const aKey: TKey]: TValue read GetValue write AddOrSetValue; default;
  end;

  { TGThreadRWMap: RWLock based concurrent map }
  generic TGThreadRWMap<TKey, TValue> = class
  public
  type
    IMap = specialize IGMap<TKey, TValue>;

  private
  type
    TMap = specialize TGAbstractMap<TKey, TValue>;

  var
    FMap: IMap;
    FRWLock: TMultiReadExclusiveWriteSynchronizer;
    FOwnsMap: Boolean;
    function  GetCount: SizeInt;
    function  GetCapacity: SizeInt;
  protected
    procedure BeginRead; inline;
    procedure BeginWrite; inline;
  public
  type
    IRoMap = specialize IGReadOnlyMap<TKey, TValue>;

    constructor Create(aMap: IMap; aOwnsMap: Boolean = True);
    destructor Destroy; override;
    function  ReadMap: IRoMap;
    procedure EndRead; inline;
    function  WriteMap: IMap;
    procedure EndWrite; inline;
  { returns True and add TEntry(aKey, aValue) only if not contains aKey }
    function  Add(constref aKey: TKey; constref aValue: TValue): Boolean;
    procedure AddOrSetValue(const aKey: TKey; constref aValue: TValue);
    function  TryGetValue(constref aKey: TKey; out aValue: TValue): Boolean;
    function  GetValueDef(constref aKey: TKey; constref aDefault: TValue = Default(TValue)): TValue;
  { returns True and map aNewValue to aKey only if contains aKey, False otherwise }
    function  Replace(constref aKey: TKey; constref aNewValue: TValue): Boolean;
    function  Contains(constref aKey: TKey): Boolean;
    function  Extract(constref aKey: TKey; out aValue: TValue): Boolean;
    function  Remove(constref aKey: TKey): Boolean;
    property  Count: SizeInt read GetCount;
    property  Capacity: SizeInt read GetCapacity;
    property  OwnsMap: Boolean read FOwnsMap;
  end;

  { TGAbstractMultiMap: multimap abstract ancestor class }
  generic TGAbstractMultiMap<TKey, TValue> = class abstract(TSimpleIterable)
  {must be generic TGAbstractMultiMap<TKey, TValue> = class abstract(
              specialize TGContainer<specialize TGMapEntry<TKey, TValue>>), but :( ... see #0033788}
  public
  type
    TEntry           = specialize TGMapEntry<TKey, TValue>;
    IKeyEnumerable   = specialize IGEnumerable<TKey>;
    IValueEnumerable = specialize IGEnumerable<TValue>;
    IEntryEnumerable = specialize IGEnumerable<TEntry>;
    TValueArray      = specialize TGArray<TKey>;

  protected
  type
    TSpecValueEnumerator = specialize TGEnumerator<TValue>;

    TAbstractValueSet = class abstract
    protected
      function GetCount: SizeInt; virtual; abstract;
    public
      function GetEnumerator: TSpecValueEnumerator; virtual; abstract;
      function ToArray: TValueArray;
      function Contains(constref aValue: TValue): Boolean; virtual; abstract;
      function Add(constref aValue: TValue): Boolean; virtual; abstract;
      function Remove(constref aValue: TValue): Boolean; virtual; abstract;
      property Count: SizeInt read GetCount;
    end;

    TMMEntry = record
      Key: TKey;
      Values: TAbstractValueSet;
    end;
    PMMEntry = ^TMMEntry;

    TCustomValueEnumerable = class(specialize TGAutoEnumerable<TValue>)
    protected
      FOwner: TGAbstractMultiMap;
    public
      constructor Create(aMap: TGAbstractMultiMap);
      destructor Destroy; override;
    end;

    TCustomEntryEnumerable = class(specialize TGAutoEnumerable<TEntry>)
    protected
      FOwner: TGAbstractMultiMap;
    public
      constructor Create(aMap: TGAbstractMultiMap);
      destructor Destroy; override;
    end;

    TCustomValueCursor = class(specialize TGEnumCursor<TValue>)
    protected
      FOwner: TGAbstractMultiMap;
    public
      constructor Create(e: TSpecEnumerator; aMap: TGAbstractMultiMap);
      destructor Destroy; override;
    end;

  var
    FCount: SizeInt;
    function  GetKeyCount: SizeInt; virtual; abstract;
    function  GetCapacity: SizeInt; virtual; abstract;
    function  GetUniqueValues: Boolean; virtual; abstract;
    procedure DoClear; virtual; abstract;
    procedure DoEnsureCapacity(aValue: SizeInt); virtual; abstract;
    procedure DoTrimToFit; virtual; abstract;
    function  Find(constref aKey: TKey): PMMEntry; virtual; abstract;
    function  FindOrAdd(constref aKey: TKey): PMMEntry; virtual; abstract;
    function  DoRemoveKey(constref aKey: TKey): SizeInt; virtual; abstract;
    function  GetKeys: IKeyEnumerable; virtual; abstract;
    function  GetValues: IValueEnumerable; virtual; abstract;
    function  GetEntries: IEntryEnumerable; virtual; abstract;

    function  DoAdd(constref aKey: TKey; constref aValue: TValue): Boolean;
    function  DoAddAll(constref a: array of TEntry): SizeInt;
    function  DoAddAll(e: IEntryEnumerable): SizeInt;
    function  DoAddValues(constref aKey: TKey; constref a: array of TValue): SizeInt;
    function  DoAddValues(constref aKey: TKey; e: IValueEnumerable): SizeInt;
    function  DoRemove(constref aKey: TKey; constref aValue: TValue): Boolean;
    function  DoRemoveAll(constref a: array of TEntry): SizeInt;
    function  DoRemoveAll(e: IEntryEnumerable): SizeInt;
    function  DoRemoveValues(constref aKey: TKey; constref a: array of TValue): SizeInt;
    function  DoRemoveValues(constref aKey: TKey; e: IValueEnumerable): SizeInt;
    function  DoRemoveKeys(constref a: array of TKey): SizeInt;
    function  DoRemoveKeys(e: IKeyEnumerable): SizeInt;
  public
    function  IsEmpty: Boolean;
    function  NonEmpty: Boolean;
    procedure Clear;
    procedure EnsureCapacity(aValue: SizeInt);
    procedure TrimToFit;

    function  Contains(constref aKey: TKey): Boolean; inline;
    function  ContainsValue(constref aKey: TKey; constref aValue: TValue): Boolean;
  { returns True and add TEntry(aKey, aValue) only if value-collection of an aKey adds aValue }
    function  Add(constref aKey: TKey; constref aValue: TValue): Boolean;
  { returns True and add e only if value-collection of an e.Key adds e.Value }
    function  Add(constref e: TEntry): Boolean;
  { returns count of added values }
    function  AddAll(constref a: array of TEntry): SizeInt;
    function  AddAll(e: IEntryEnumerable): SizeInt;
    function  AddValues(constref aKey: TKey; constref a: array of TValue): SizeInt;
    function  AddValues(constref aKey: TKey; e: IValueEnumerable): SizeInt;
  { returns True if aKey exists and mapped to aValue; aValue will be removed(and aKey if no more mapped values) }
    function  Remove(constref aKey: TKey; constref aValue: TValue): Boolean;
    function  Remove(constref e: TEntry): Boolean;
    function  RemoveAll(constref a: array of TEntry): SizeInt;
    function  RemoveAll(e: IEntryEnumerable): SizeInt;
    function  RemoveValues(constref aKey: TKey; constref a: array of TValue): SizeInt;
    function  RemoveValues(constref aKey: TKey; e: IValueEnumerable): SizeInt;
  { if aKey exists then removes if with mapped values; returns count of removed values }
    function  RemoveKey(constref aKey: TKey): SizeInt;
  { returns count of removed values }
    function  RemoveKeys(constref a: array of TKey): SizeInt;
  { returns count of removed values }
    function  RemoveKeys(e: IKeyEnumerable): SizeInt;
  { enumerates values mapped to aKey(empty if aKey is missing) }
    function  ValuesView(const aKey: TKey): IValueEnumerable;
    function  Keys: IKeyEnumerable;
    function  Values: IValueEnumerable;
    function  Entries: IEntryEnumerable;
  { returns count of values mapped to aKey (similar as multiset)}
    function  ValueCount(constref aKey: TKey): SizeInt;
    property  UniqueValues: Boolean read GetUniqueValues;
    property  Count: SizeInt read FCount;
    property  KeyCount: SizeInt read GetKeyCount;
    property  Capacity: SizeInt read GetCapacity;
    property  Items[const aKey: TKey]: IValueEnumerable read ValuesView; default;
  end;

  { TGAbstractTable2D: table abstract ancestor class }
  generic TGAbstractTable2D<TRow, TCol, TValue> = class abstract
  public
  type

    TCellData = record
      Row:    TRow;
      Column: TCol;
      Value:  TValue;
      constructor Create(constref aRow: TRow; constref aCol: TCol; constref aValue: TValue);
    end;

    TColData = record
      Row:   TRow;
      Value: TValue;
      constructor Create(constref aRow: TRow; constref aValue: TValue);
    end;

    TRowData = record
      Column: TCol;
      Value:  TValue;
      constructor Create(constref aCol: TCol; constref aValue: TValue);
    end;

    TSpecTable2D        = TGAbstractTable2D;
    TValueArray         = array of TValue;
    IValueEnumerable    = specialize IGEnumerable<TValue>;
    IColEnumerable      = specialize IGEnumerable<TCol>;
    IRowEnumerable      = specialize IGEnumerable<TRow>;
    IRowDataEnumerable  = specialize IGEnumerable<TRowData>;
    IColDataEnumerable  = specialize IGEnumerable<TColData>;
    ICellDataEnumerable = specialize IGEnumerable<TCellData>;
    TRowDataEnumerator  = class abstract(specialize TGEnumerator<TRowData>);

{$PUSH}{$INTERFACES CORBA}
    IRowMap = interface
      function  GetCount: SizeInt;
      function  GetEnumerator: TRowDataEnumerator;
      function  IsEmpty: Boolean;
      procedure TrimToFit;
      function  Contains(constref aCol: TCol): Boolean;
      function  TryGetValue(constref aCol: TCol; out aValue: TValue): Boolean;
      function  GetValueOrDefault(const aCol: TCol): TValue;
    { returns True if not contains aCol was added, False otherwise }
      function  Add(constref aCol: TCol; constref aValue: TValue): Boolean;
      procedure AddOrSetValue(const aCol: TCol; const aValue: TValue);
      function  Remove(constref aCol: TCol): Boolean;
      property  Count: SizeInt read GetCount;
      property  Cells[const aCol: TCol]: TValue read GetValueOrDefault write AddOrSetValue; default;
    end;
{$POP}

   IRowMapEnumerable = specialize IGEnumerable<IRowMap>;

  protected
  type
    TCustomRowMap = class(IRowMap)
    protected
      function  GetCount: SizeInt; virtual; abstract;
    public
      function  GetEnumerator: TRowDataEnumerator; virtual; abstract;
      function  IsEmpty: Boolean;
      procedure TrimToFit; virtual; abstract;
      function  Contains(constref aCol: TCol): Boolean; virtual; abstract;
      function  TryGetValue(constref aCol: TCol; out aValue: TValue): Boolean; virtual; abstract;
      function  GetValueOrDefault(const aCol: TCol): TValue; inline;
    { returns True if not contains aCol was added, False otherwise }
      function  Add(constref aCol: TCol; constref aValue: TValue): Boolean; virtual; abstract;
      procedure AddOrSetValue(const aCol: TCol; const aValue: TValue); virtual; abstract;
      function  Remove(constref aCol: TCol): Boolean; virtual; abstract;
      property  Count: SizeInt read GetCount;
      property  Cells[const aCol: TCol]: TValue read GetValueOrDefault write AddOrSetValue; default;
    end;

    TRowEntry = record
      Key: TRow;
      Columns: TCustomRowMap;
    end;
    PRowEntry = ^TRowEntry;

    TAutoValueEnumerable    = class abstract(specialize TGAutoEnumerable<TValue>);
    TAutoRowDataEnumerable  = class abstract(specialize TGAutoEnumerable<TRowData>);
    TAutoColDataEnumerable  = class abstract(specialize TGAutoEnumerable<TColData>);
    TAutoCellDataEnumerable = class abstract(specialize TGAutoEnumerable<TCellData>);

  var
    FCellCount: SizeInt;
    function  GetRowCount: SizeInt; virtual; abstract;
    function  DoFindRow(constref aRow: TRow): PRowEntry; virtual; abstract;
  { returns True if row found, False otherwise }
    function  DoFindOrAddRow(constref aRow: TRow; out p: PRowEntry): Boolean; virtual; abstract;
    function  DoRemoveRow(constref aRow: TRow): SizeInt; virtual; abstract;
    function  GetColumn(const aCol: TCol): IColDataEnumerable; virtual; abstract;
    function  GetCellData: ICellDataEnumerable; virtual; abstract;
    function  GetColCount(const aRow: TRow): SizeInt;
  { aRow will be added if it is missed }
    function  GetRowMap(const aRow: TRow): IRowMap;
  { will raise exception if cell is missed }
    function  GetCell(const aRow: TRow; const aCol: TCol): TValue;
  public
    function  IsEmpty: Boolean;
    function  NonEmpty: Boolean;
    procedure Clear; virtual; abstract;
    procedure TrimToFit; virtual; abstract;
    procedure EnsureRowCapacity(aValue: SizeInt); virtual; abstract;
    function  ContainsRow(constref aRow: TRow): Boolean; inline;
    function  FindRow(constref aRow: TRow; out aMap: IRowMap): Boolean;
  { if not contains aRow then add aRow and returns True, False otherwise }
    function  AddRow(constref aRow: TRow): Boolean; inline;
    function  AddRows(constref a: array of TRow): SizeInt;
  { returns count of the columns in the removed row }
    function  RemoveRow(constref aRow: TRow): SizeInt; inline;
  { returns count of the removed cells }
    function  RemoveColumn(constref aCol: TCol): SizeInt; inline;
    function  ContainsCell(constref aRow: TRow; constref aCol: TCol): Boolean;
    function  FindCell(constref aRow: TRow; constref aCol: TCol; out aValue: TValue): Boolean;
    function  GetCellDef(constref aRow: TRow; constref aCol: TCol; aDef: TValue = Default(TValue)): TValue; inline;
    procedure AddOrSetCell(const aRow: TRow; const aCol: TCol; const aValue: TValue);
    function  AddCell(constref aRow: TRow; constref aCol: TCol; constref aValue: TValue): Boolean;
    function  AddCell(constref e: TCellData): Boolean; inline;
    function  AddCells(constref a: array of TCellData): SizeInt;
    function  RemoveCell(constref aRow: TRow; constref aCol: TCol): Boolean;

    function  Rows: IRowEnumerable; virtual; abstract;
    function  EnumRowMaps: IRowMapEnumerable; virtual; abstract;
    property  RowCount: SizeInt read GetRowCount;
    property  ColCount[const aRow: TRow]: SizeInt read GetColCount;
    property  CellCount: SizeInt read FCellCount;
    property  RowMaps[const aRow: TRow]: IRowMap read GetRowMap;
    property  Columns[const aCol: TCol]: IColDataEnumerable read GetColumn;
    property  Cells: ICellDataEnumerable read GetCellData;
  { will raise an exception if one try to read the missing cell }
    property  Items[const aRow: TRow; const aCol: TCol]: TValue read GetCell write AddOrSetCell; default;
  end;

implementation
{$B-}{$COPERATORS ON}{$POINTERMATH ON}

{ TGEnumerable }

function TGEnumerable._GetRef: TObject;
begin
  Result := Self;
end;

procedure TGEnumerable.Discard;
begin
  GetEnumerator.Free;
end;

function TGEnumerable.Reverse: IEnumerable;
begin
  Result := specialize TGArrayReverse<T>.Create(ToArray);
end;

function TGEnumerable.ToArray: TArray;
var
  I, Len: SizeInt;
begin
  Len := ARRAY_INITIAL_SIZE;
  SetLength(Result, ARRAY_INITIAL_SIZE);
  I := 0;
  with GetEnumerator do
    try
      while MoveNext do
        begin
          if I = Len then
            begin
              Len += Len;
              SetLength(Result, Len);
            end;
          Result[I] := Current;
          Inc(I);
        end;
    finally
      Free;
    end;
  SetLength(Result, I);
end;

function TGEnumerable.Any: Boolean;
begin
  with GetEnumerator do
    try
      Result :=  MoveNext;
    finally
      Free;
    end;
end;

function TGEnumerable.None: Boolean;
begin
  Result := not Any;
end;

function TGEnumerable.Total: SizeInt;
begin
  Result := 0;
  with GetEnumerator do
    try
      while MoveNext do
        Inc(Result);
    finally
      Free;
    end;
end;

function TGEnumerable.FindFirst(out aValue: T): Boolean;
var
  e: TSpecEnumerator;
begin
  e := GetEnumerator;
  try
    Result := e.MoveNext;
    if Result then
      aValue := e.Current;
  finally
    e.Free;
  end;
end;

function TGEnumerable.First: TOptional;
var
  v: T;
begin
  if FindFirst(v) then
    Result.Assign(v);
end;

function TGEnumerable.FindLast(out aValue: T): Boolean;
var
  e: TSpecEnumerator;
begin
  e := GetEnumerator;
  try
    Result := e.MoveNext;
    if Result then
      begin
        while e.MoveNext do;
        aValue := e.Current;
      end;
  finally
    e.Free;
  end;
end;

function TGEnumerable.Last: TOptional;
var
  v: T;
begin
  if FindLast(v) then
    Result.Assign(v);
end;

function TGEnumerable.FindMin(out aValue: T; c: TCompare): Boolean;
var
  v: T;
begin
  with GetEnumerator do
    try
      Result := MoveNext;
      if Result then
        begin
          aValue := Current;
          while MoveNext do
            begin
              v := Current;
              if c(v, aValue) < 0 then
                aValue := v;
            end;
        end;
    finally
      Free;
    end;
end;

function TGEnumerable.FindMin(out aValue: T; c: TOnCompare): Boolean;
var
  v: T;
begin
  with GetEnumerator do
    try
      Result := MoveNext;
      if Result then
        begin
          aValue := Current;
          while MoveNext do
            begin
              v := Current;
              if c(v, aValue) < 0 then
                aValue := v;
            end;
        end;
    finally
      Free;
    end;
end;

function TGEnumerable.FindMin(out aValue: T; c: TNestCompare): Boolean;
var
  v: T;
begin
  with GetEnumerator do
    try
      Result := MoveNext;
      if Result then
        begin
          aValue := Current;
          while MoveNext do
            begin
              v := Current;
              if c(v, aValue) < 0 then
                aValue := v;
            end;
        end;
    finally
      Free;
    end;
end;

function TGEnumerable.Min(c: TCompare): TOptional;
var
  v: T;
begin
  if FindMin(v, c) then
    Result.Assign(v);
end;

function TGEnumerable.Min(c: TOnCompare): TOptional;
var
  v: T;
begin
  if FindMin(v, c) then
    Result.Assign(v);
end;

function TGEnumerable.Min(c: TNestCompare): TOptional;
var
  v: T;
begin
  if FindMin(v, c) then
    Result.Assign(v);
end;

function TGEnumerable.FindMax(out aValue: T; c: TCompare): Boolean;
var
  v: T;
begin
  with GetEnumerator do
    try
      Result := MoveNext;
      if Result then
        begin
          aValue := Current;
          while MoveNext do
            begin
              v := Current;
              if c(aValue, v) < 0 then
                aValue := v;
            end;
        end;
    finally
      Free;
    end;
end;

function TGEnumerable.FindMax(out aValue: T; c: TOnCompare): Boolean;
var
  v: T;
begin
  with GetEnumerator do
    try
      Result := MoveNext;
      if Result then
        begin
          aValue := Current;
          while MoveNext do
            begin
              v := Current;
              if c(aValue, v) < 0 then
                aValue := v;
            end;
        end;
    finally
      Free;
    end;
end;

function TGEnumerable.FindMax(out aValue: T; c: TNestCompare): Boolean;
var
  v: T;
begin
  with GetEnumerator do
    try
      Result := MoveNext;
      if Result then
        begin
          aValue := Current;
          while MoveNext do
            begin
              v := Current;
              if c(aValue, v) < 0 then
                aValue := v;
            end;
        end;
    finally
      Free;
    end;
end;

function TGEnumerable.Max(c: TCompare): TOptional;
var
  v: T;
begin
  if FindMax(v, c) then
    Result.Assign(v);
end;

function TGEnumerable.Max(c: TOnCompare): TOptional;
var
  v: T;
begin
  if FindMax(v, c) then
    Result.Assign(v);
end;

function TGEnumerable.Max(c: TNestCompare): TOptional;
var
  v: T;
begin
  if FindMax(v, c) then
    Result.Assign(v);
end;

function TGEnumerable.Skip(aCount: SizeInt): IEnumerable;
begin
  Result := specialize TGSkipEnumerable<T>.Create(GetEnumerator, aCount);
end;

function TGEnumerable.Limit(aCount: SizeInt): IEnumerable;
begin
  Result := specialize TGLimitEnumerable<T>.Create(GetEnumerator, aCount);
end;

function TGEnumerable.Sorted(c: TCompare): IEnumerable;
var
  a: TArray;
begin
  a := ToArray;
  specialize TGRegularArrayHelper<T>.Sort(a, c);
  Result := specialize TGArrayCursor<T>.Create(a);
end;

function TGEnumerable.Sorted(c: TOnCompare): IEnumerable;
var
  a: TArray;
begin
  a := ToArray;
  specialize TGDelegatedArrayHelper<T>.Sort(a, c);
  Result := specialize TGArrayCursor<T>.Create(a);
end;

function TGEnumerable.Sorted(c: TNestCompare): IEnumerable;
var
  a: TArray;
begin
  a := ToArray;
  specialize TGNestedArrayHelper<T>.Sort(a, c);
  Result := specialize TGArrayCursor<T>.Create(a);
end;

function TGEnumerable.Select(aTest: TTest): IEnumerable;
begin
  Result := specialize TGEnumRegularFilter<T>.Create(GetEnumerator, aTest);
end;

function TGEnumerable.Select(aTest: TOnTest): IEnumerable;
begin
  Result := specialize TGEnumDelegatedFilter<T>.Create(GetEnumerator, aTest);
end;

function TGEnumerable.Select(aTest: TNestTest): IEnumerable;
begin
  Result := specialize TGEnumNestedFilter<T>.Create(GetEnumerator, aTest);
end;

function TGEnumerable.Any(aTest: TTest): Boolean;
begin
  with GetEnumerator do
    try
      while MoveNext do
        if aTest(Current) then
          exit(True);
    finally
      Free;
    end;
  Result := False;
end;

function TGEnumerable.Any(aTest: TOnTest): Boolean;
begin
  with GetEnumerator do
    try
      while MoveNext do
        if aTest(Current) then
          exit(True);
    finally
      Free;
    end;
  Result := False;
end;

function TGEnumerable.Any(aTest: TNestTest): Boolean;
begin
  with GetEnumerator do
    try
      while MoveNext do
        if aTest(Current) then
          exit(True);
    finally
      Free;
    end;
  Result := False;
end;

function TGEnumerable.None(aTest: TTest): Boolean;
begin
  Result := not Any(aTest);
end;

function TGEnumerable.None(aTest: TOnTest): Boolean;
begin
  Result := not Any(aTest);
end;

function TGEnumerable.None(aTest: TNestTest): Boolean;
begin
  Result := not Any(aTest);
end;

function TGEnumerable.All(aTest: TTest): Boolean;
begin
  with GetEnumerator do
    try
      while MoveNext do
        if not aTest(Current) then
          exit(False);
    finally
      Free;
    end;
  Result := True;
end;

function TGEnumerable.All(aTest: TOnTest): Boolean;
begin
  with GetEnumerator do
    try
      while MoveNext do
        if not aTest(Current) then
          exit(False);
    finally
      Free;
    end;
  Result := True;
end;

function TGEnumerable.All(aTest: TNestTest): Boolean;
begin
  with GetEnumerator do
    try
      while MoveNext do
        if not aTest(Current) then
          exit(False);
    finally
      Free;
    end;
  Result := True;
end;

function TGEnumerable.Total(aTest: TTest): SizeInt;
begin
  Result := 0;
  with GetEnumerator do
    try
      while MoveNext do
        Result += Ord(aTest(Current));
    finally
      Free;
    end;
end;

function TGEnumerable.Total(aTest: TOnTest): SizeInt;
begin
  Result := 0;
  with GetEnumerator do
    try
      while MoveNext do
        Result += Ord(aTest(Current));
    finally
      Free;
    end;
end;

function TGEnumerable.Total(aTest: TNestTest): SizeInt;
begin
  Result := 0;
  with GetEnumerator do
    try
      while MoveNext do
        Result += Ord(aTest(Current));
    finally
      Free;
    end;
end;

function TGEnumerable.Distinct(c: TCompare): IEnumerable;
begin
  Result := specialize TGArrayCursor<T>.Create(
    specialize TGRegularArrayHelper<T>.SelectDistinct(ToArray, c));
end;

function TGEnumerable.Distinct(c: TOnCompare): IEnumerable;
begin
  Result := specialize TGArrayCursor<T>.Create(
    specialize TGDelegatedArrayHelper<T>.SelectDistinct(ToArray, c));
end;

function TGEnumerable.Distinct(c: TNestCompare): IEnumerable;
begin
  Result := specialize TGArrayCursor<T>.Create(
    specialize TGNestedArrayHelper<T>.SelectDistinct(ToArray, c));
end;

function TGEnumerable.Map(aMap: TMapFunc): IEnumerable;
begin
  Result := specialize TGEnumRegularMap<T>.Create(GetEnumerator, aMap);
end;

function TGEnumerable.Map(aMap: TOnMap): IEnumerable;
begin
  Result := specialize TGEnumDelegatedMap<T>.Create(GetEnumerator, aMap);
end;

function TGEnumerable.Map(aMap: TNestMap): IEnumerable;
begin
  Result := specialize TGEnumNestedMap<T>.Create(GetEnumerator, aMap);
end;

function TGEnumerable.Fold(aFold: TFold; constref v0: T): T;
begin
  Result := v0;
  with GetEnumerator do
    try
      while MoveNext do
        Result := aFold(Current, Result);
    finally
      Free;
    end;
end;

function TGEnumerable.Fold(aFold: TFold): TOptional;
var
  v: T;
begin
  with GetEnumerator do
    try
      if MoveNext then
        begin
          v := Current;
          while MoveNext do
            v := aFold(Current, v);
          Result.Assign(v);
        end;
    finally
      Free;
    end;
end;

function TGEnumerable.Fold(aFold: TOnFold; constref v0: T): T;
begin
  Result := v0;
  with GetEnumerator do
    try
      while MoveNext do
        Result := aFold(Current, Result);
    finally
      Free;
    end;
end;

function TGEnumerable.Fold(aFold: TOnFold): TOptional;
var
  v: T;
begin
  with GetEnumerator do
    try
      if MoveNext then
        begin
          v := Current;
          while MoveNext do
            v := aFold(Current, v);
          Result.Assign(v);
        end;
    finally
      Free;
    end;
end;

function TGEnumerable.Fold(aFold: TNestFold; constref v0: T): T;
begin
  Result := v0;
  with GetEnumerator do
    try
      while MoveNext do
        Result := aFold(Current, Result);
    finally
      Free;
    end;
end;

function TGEnumerable.Fold(aFold: TNestFold): TOptional;
var
  v: T;
begin
  with GetEnumerator do
    try
      if MoveNext then
        begin
          v := Current;
          while MoveNext do
            v := aFold(Current, v);
          Result.Assign(v);
        end;
    finally
      Free;
    end;
end;

{$I Enums.inc}

{ TGAbstractContainer.TGContainerEnumerator }

constructor TGAbstractContainer.TContainerEnumerator.Create(c: TSpecContainer);
begin
  FOwner := c;
end;

destructor TGAbstractContainer.TContainerEnumerator.Destroy;
begin
  FOwner.EndIteration;
  inherited;
end;

{ TGAbstractContainer.TContainerEnumerable }

constructor TGAbstractContainer.TContainerEnumerable.Create(c: TSpecContainer);
begin
  inherited Create;
  FOwner := c;
end;

destructor TGAbstractContainer.TContainerEnumerable.Destroy;
begin
  FOwner.EndIteration;
  inherited;
end;

{ TGAbstractContainer }

function TGAbstractContainer.GetInIteration: Boolean;
begin
  Result := Boolean(LongBool(FItCounter));
end;

procedure TGAbstractContainer.CapacityExceedError(aValue: SizeInt);
begin
  raise ELGCapacityExceed.CreateFmt(SEClassCapacityExceedFmt, [ClassName, aValue]);
end;

procedure TGAbstractContainer.AccessEmptyError;
begin
  raise ELGAccessEmpty.CreateFmt(SEClassAccessEmptyFmt, [ClassName]);
end;

procedure TGAbstractContainer.IndexOutOfBoundError(aIndex: SizeInt);
begin
  raise ELGListError.CreateFmt(SEClassIdxOutOfBoundsFmt, [ClassName, aIndex]);
end;

procedure TGAbstractContainer.UpdateLockError;
begin
  raise ELGUpdateLock.CreateFmt(SECantUpdDuringIterFmt, [ClassName]);
end;

procedure TGAbstractContainer.CheckInIteration;
begin
  if InIteration then
    UpdateLockError;
end;

procedure TGAbstractContainer.BeginIteration;
begin
  Inc(FItCounter);
end;

procedure TGAbstractContainer.EndIteration;
begin
  Dec(FItCounter);
end;

procedure TGAbstractContainer.CopyItems(aBuffer: PItem);
begin
  with GetEnumerator do
    try
      while MoveNext do
        begin
          aBuffer^ := Current;
          Inc(aBuffer);
        end;
    finally
      Free;
    end;
end;

function TGAbstractContainer.GetEnumerator: TSpecEnumerator;
begin
  BeginIteration;
  Result := DoGetEnumerator;
end;

function TGAbstractContainer.ToArray: TArray;
var
  c: SizeInt;
begin
  c := Count;
  System.SetLength(Result, c);
  if c > 0 then
    CopyItems(@Result[0]);
end;

function TGAbstractContainer.IsEmpty: Boolean;
begin
  Result := GetCount = 0;
end;

function TGAbstractContainer.NonEmpty: Boolean;
begin
  Result := GetCount <> 0;
end;

procedure TGAbstractContainer.Clear;
begin
  CheckInIteration;
  DoClear;
end;

procedure TGAbstractContainer.TrimToFit;
begin
  CheckInIteration;
  DoTrimToFit;
end;

procedure TGAbstractContainer.EnsureCapacity(aValue: SizeInt);
begin
  CheckInIteration;
  DoEnsureCapacity(aValue);
end;

{$I DynBuffer.inc}

{ TGAbstractCollection }

function TGAbstractCollection.DoRemove(constref aValue: T): Boolean;
begin
  Result := DoExtract(aValue);
end;

function TGAbstractCollection.DoAddAll(constref a: array of T): SizeInt;
var
  v: T;
begin
  Result := 0;
  for v in a do
    Result += Ord(DoAdd(v));
end;

function TGAbstractCollection.DoRemoveAll(constref a: array of T): SizeInt;
var
  v: T;
begin
  Result := 0;
  if NonEmpty then
    for v in a do
      if DoRemove(v) then
        begin
          Inc(Result);
          if Count = 0 then
            exit;
        end;
end;

function TGAbstractCollection.DoRemoveAll(e: IEnumerable): SizeInt;
var
  o: TObject;
  v: T;
begin
  o := e._GetRef;
  if o <> Self then
    begin
      Result := 0;
      for v in e do
        if DoRemove(v) then
          begin
            Inc(Result);
            if Count = 0 then
              exit;
          end;
    end
  else
    begin
      Result := Count;
      DoClear;
    end;
end;

function TGAbstractCollection.Add(constref aValue: T): Boolean;
begin
  CheckInIteration;
  Result := DoAdd(aValue);
end;

function TGAbstractCollection.AddAll(constref a: array of T): SizeInt;
begin
  CheckInIteration;
  Result := DoAddAll(a);
end;

function TGAbstractCollection.AddAll(e: IEnumerable): SizeInt;
begin
  if not InIteration then
    Result := DoAddAll(e)
  else
    begin
      Result := 0;
      e.Discard;
      UpdateLockError;
    end;
end;

function TGAbstractCollection.NonContains(constref aValue: T): Boolean;
begin
  Result := not Contains(aValue);
end;

function TGAbstractCollection.ContainsAny(constref a: array of T): Boolean;
var
  v: T;
begin
  if NonEmpty then
    for v in a do
      if Contains(v) then
        exit(True);
  Result := False;
end;

function TGAbstractCollection.ContainsAny(e: IEnumerable): Boolean;
var
  v: T;
begin
  if e._GetRef <> Self then
    begin
      if NonEmpty then
        begin
          for v in e do
            if Contains(v) then
              exit(True);
        end
      else
        e.Discard;
      Result := False;
    end
  else
    Result := NonEmpty;
end;

function TGAbstractCollection.ContainsAll(constref a: array of T): Boolean;
var
  v: T;
begin
  for v in a do
    if not Contains(v) then
      exit(False);
  Result := True;
end;

function TGAbstractCollection.ContainsAll(e: IEnumerable): Boolean;
var
  v: T;
begin
  if e._GetRef <> Self then
    for v in e do
      if NonContains(v) then
        exit(False);
  Result := True;
end;

function TGAbstractCollection.Remove(constref aValue: T): Boolean;
begin
  CheckInIteration;
  Result := DoRemove(aValue);
end;

function TGAbstractCollection.RemoveAll(constref a: array of T): SizeInt;
begin
  CheckInIteration;
  Result := DoRemoveAll(a);
end;

function TGAbstractCollection.RemoveAll(e: IEnumerable): SizeInt;
begin
  if not InIteration then
    Result := DoRemoveAll(e)
  else
    begin
      Result := 0;
      e.Discard;
      UpdateLockError;
    end;
end;

function TGAbstractCollection.RemoveIf(aTest: TTest): SizeInt;
begin
  CheckInIteration;
  Result := DoRemoveIf(aTest);
end;

function TGAbstractCollection.RemoveIf(aTest: TOnTest): SizeInt;
begin
  CheckInIteration;
  Result := DoRemoveIf(aTest);
end;

function TGAbstractCollection.RemoveIf(aTest: TNestTest): SizeInt;
begin
  CheckInIteration;
  Result := DoRemoveIf(aTest);
end;

function TGAbstractCollection.Extract(constref aValue: T): Boolean;
begin
  CheckInIteration;
  Result := DoExtract(aValue);
end;

function TGAbstractCollection.ExtractIf(aTest: TTest): TArray;
begin
  CheckInIteration;
  Result := DoExtractIf(aTest);
end;

function TGAbstractCollection.ExtractIf(aTest: TOnTest): TArray;
begin
  CheckInIteration;
  Result := DoExtractIf(aTest);
end;

function TGAbstractCollection.ExtractIf(aTest: TNestTest): TArray;
begin
  CheckInIteration;
  Result := DoExtractIf(aTest);
end;

procedure TGAbstractCollection.RetainAll(aCollection: ICollection);
begin
  if aCollection._GetRef <> Self then
    begin
      CheckInIteration;
      DoRemoveIf(@aCollection.NonContains);
    end;
end;

{ TGThreadCollection }

procedure TGThreadCollection.Lock;
begin
  System.EnterCriticalSection(FLock);
end;

constructor TGThreadCollection.Create(aCollection: ICollection; aOwnsCollection: Boolean);
begin
  System.InitCriticalSection(FLock);
  FCollection := aCollection;
  FOwnsColl := aOwnsCollection;
end;

destructor TGThreadCollection.Destroy;
begin
  Lock;
  try
    if OwnsCollection then
      FCollection._GetRef.Free;
    FCollection := nil;
    inherited;
  finally
    UnLock;
    System.DoneCriticalSection(FLock);
  end;
end;

function TGThreadCollection.LockCollection: ICollection;
begin
  Result := FCollection;
  Lock;
end;

procedure TGThreadCollection.Unlock;
begin
  System.LeaveCriticalSection(FLock);
end;

procedure TGThreadCollection.Clear;
begin
  Lock;
  try
    FCollection.Clear;
  finally
    UnLock;
  end;
end;

function TGThreadCollection.Contains(constref aValue: T): Boolean;
begin
  Lock;
  try
    Result := FCollection.Contains(aValue);
  finally
    UnLock;
  end;
end;

function TGThreadCollection.Add(constref aValue: T): Boolean;
begin
  Lock;
  try
    Result := FCollection.Add(aValue);
  finally
    UnLock;
  end;
end;

function TGThreadCollection.Remove(constref aValue: T): Boolean;
begin
  Lock;
  try
    Result := FCollection.Remove(aValue);
  finally
    UnLock;
  end;
end;

{ TGThreadRWCollection }

function TGThreadRWCollection.GetCapacity: SizeInt;
begin
  FRWLock.BeginRead;
  try
    Result := FCollection.Capacity;
  finally
    FRWLock.EndRead;
  end;
end;

function TGThreadRWCollection.GetCount: SizeInt;
begin
  FRWLock.BeginRead;
  try
    Result := FCollection.Count;
  finally
    FRWLock.EndRead;
  end;
end;

procedure TGThreadRWCollection.BeginRead;
begin
  FRWLock.BeginRead;
end;

procedure TGThreadRWCollection.BeginWrite;
begin
  FRWLock.BeginWrite;
end;

constructor TGThreadRWCollection.Create(aCollection: ICollection; aOwnsCollection: Boolean);
begin
  FRWLock := TMultiReadExclusiveWriteSynchronizer.Create;
  FCollection := aCollection;
  FOwnsColl := aOwnsCollection;
end;

destructor TGThreadRWCollection.Destroy;
begin
  FRWLock.BeginWrite;
  try
    if OwnsCollection then
      FCollection._GetRef.Free;
    FCollection := nil;
    inherited;
  finally
    FRWLock.EndWrite;
    FRWLock.Free;
  end;
end;

function TGThreadRWCollection.ReadCollection: IRoCollection;
begin
  FRWLock.BeginRead;
  Result := TCollection(FCollection._GetRef);
end;

procedure TGThreadRWCollection.EndRead;
begin
  FRWLock.EndRead;
end;

function TGThreadRWCollection.WriteCollection: ICollection;
begin
  FRWLock.BeginWrite;
  Result := FCollection;
end;

procedure TGThreadRWCollection.EndWrite;
begin
  FRWLock.EndWrite;
end;

function TGThreadRWCollection.Contains(constref aValue: T): Boolean;
begin
  FRWLock.BeginRead;
  try
    Result := FCollection.Contains(aValue);
  finally
    FRWLock.EndRead;
  end;
end;

function TGThreadRWCollection.Add(constref aValue: T): Boolean;
begin
  FRWLock.BeginWrite;
  try
    Result := FCollection.Add(aValue);
  finally
    FRWLock.EndWrite;
  end;
end;

function TGThreadRWCollection.Remove(constref aValue: T): Boolean;
begin
  FRWLock.BeginWrite;
  try
    Result := FCollection.Remove(aValue);
  finally
    FRWLock.EndWrite;
  end;
end;

{ TGAbstractSet.TExtractHelper }

procedure TGAbstractSet.TExtractHelper.OnExtract(p: PEntry);
var
  c: SizeInt;
begin
  c := System.Length(FExtracted);
  if FCurrIndex = c then
    System.SetLength(FExtracted, c shl 1);
  FExtracted[FCurrIndex] := p^.Key;
  Inc(FCurrIndex);
end;

procedure TGAbstractSet.TExtractHelper.Init;
begin
  FCurrIndex := 0;
  System.SetLength(FExtracted, ARRAY_INITIAL_SIZE);
end;

function TGAbstractSet.TExtractHelper.Final: TArray;
begin
  System.SetLength(FExtracted, FCurrIndex);
  Result := FExtracted;
end;

{ TGAbstractSet }

function TGAbstractSet.DoAddAll(e: IEnumerable): SizeInt;
var
  v: T;
begin
  if e._GetRef <> Self then
    begin
      Result := 0;
      for v in e do
        Result += Ord(DoAdd(v));
    end
  else
    Result := 0;
end;

procedure TGAbstractSet.DoSymmetricSubtract(aSet: TSpecSet);
var
  v: T;
begin
  if aSet <> Self then
    begin
      for v in aSet do
        if not DoRemove(v) then
          DoAdd(v);
    end
  else
    Clear;
end;

function TGAbstractSet.IsSuperset(aSet: TSpecSet): Boolean;
begin
  if aSet <> Self then
    begin
      if Count >= aSet.Count then
        Result := ContainsAll(aSet)
      else
        Result := False;
    end
  else
    Result := True;
end;

function TGAbstractSet.IsSubset(aSet: TSpecSet): Boolean;
begin
  Result := aSet.IsSuperset(Self);
end;

function TGAbstractSet.IsEqual(aSet: TSpecSet): Boolean;
begin
  if aSet <> Self then
    Result := (Count = aSet.Count) and ContainsAll(aSet)
  else
    Result := True;
end;

function TGAbstractSet.Intersecting(aSet: TSpecSet): Boolean;
begin
  Result := ContainsAny(aSet);
end;

procedure TGAbstractSet.Intersect(aSet: TSpecSet);
begin
  RetainAll(aSet);
end;

procedure TGAbstractSet.Join(aSet: TSpecSet);
begin
  AddAll(aSet);
end;

procedure TGAbstractSet.Subtract(aSet: TSpecSet);
begin
  RemoveAll(aSet);
end;

procedure TGAbstractSet.SymmetricSubtract(aSet: TSpecSet);
begin
  CheckInIteration;
  DoSymmetricSubtract(aSet);
end;

{ TGAbstractMultiSet.TExtractor }

procedure TGAbstractMultiSet.TExtractHelper.OnExtract(p: PEntry);
var
  I, LastKey: SizeInt;
  Key: T;
begin
  LastKey := Pred(FCurrIndex + p^.Count);
  Key := p^.Key;
  if LastKey >= System.Length(FExtracted) then
      System.SetLength(FExtracted, RoundUpTwoPower(Succ(LastKey)));
  for I := FCurrIndex to LastKey do
    FExtracted[I] := Key;
  FCurrIndex := Succ(LastKey);
end;

procedure TGAbstractMultiSet.TExtractHelper.Init;
begin
  FCurrIndex := 0;
  System.SetLength(FExtracted, ARRAY_INITIAL_SIZE);
end;

function TGAbstractMultiSet.TExtractHelper.Final: TArray;
begin
  System.SetLength(FExtracted, FCurrIndex);
  Result := FExtracted;
end;

{ TGAbstractMultiSet.TIntersectHelper }

function TGAbstractMultiSet.TIntersectHelper.OnIntersect(p: PEntry): Boolean;
var
  c, SetCount: SizeInt;
begin
  SetCount := FOtherSet[p^.Key];
  c := p^.Count;
  if SetCount > 0 then
    begin
      Result := False;
      if SetCount < c then
        begin
          FSet.FCount -= c - SetCount;
          p^.Count := SetCount;
        end;
    end
  else
    Result := True;
end;

{ TGAbstractMultiSet }

function TGAbstractMultiSet.GetCount: SizeInt;
begin
  Result := FCount;
end;

procedure TGAbstractMultiSet.DoJoinEntry(constref e: TEntry);
var
  p: PEntry;
begin
{$PUSH}{$Q+}
  if not FindOrAdd(e.Key, p) then
    begin
      p^.Count := e.Count;
      FCount += e.Count;
    end
  else
    if e.Count > p^.Count then
      begin
        FCount += e.Count - p^.Count;
        p^.Count := e.Count;
      end;
{$POP}
end;

procedure TGAbstractMultiSet.DoAddEntry(constref e: TEntry);
var
  p: PEntry;
begin
{$PUSH}{$Q+}
  FCount += e.Count;
{$POP}
  if not FindOrAdd(e.Key, p) then
    p^.Count := e.Count
  else
    p^.Count += e.Count;
end;

function TGAbstractMultiSet.GetKeyCount(const aKey: T): SizeInt;
var
  p: PEntry;
begin
  p := FindEntry(aKey);
  if p <> nil then
    Result := p^.Count
  else
    Result := 0;
end;

procedure TGAbstractMultiSet.SetKeyCount(const aKey: T; aValue: SizeInt);
var
  p: PEntry;
  e: TEntry;
begin
  if aValue < 0 then
    raise EArgumentException.CreateFmt(SECantAcceptNegCountFmt, [ClassName]);
  CheckInIteration;
  if aValue > 0 then
    begin
{$PUSH}{$Q+}
      if FindOrAdd(aKey, p) then
        begin
          FCount += aValue - p^.Count;
          p^.Count := aValue;
        end
      else
        begin
          FCount += aValue;
          p^.Count := aValue;
        end;
{$POP}
    end
  else
    begin  // aValue = 0;
      e.Key := aKey;
      e.Count := High(SizeInt);
      DoSubEntry(e);
    end;
end;

procedure TGAbstractMultiSet.DoArithAdd(aSet: TSpecMultiSet);
var
  e: TEntry;
begin
  if aSet <> Self then
    for e in aSet.Entries do
      DoAddEntry(e)
  else
    DoDoubleEntryCounters;
end;

procedure TGAbstractMultiSet.DoArithSubtract(aSet: TSpecMultiSet);
var
  e: TEntry;
begin
  if aSet <> Self then
    for e in aSet.Entries do
      DoSubEntry(e)
  else
    Clear;
end;

procedure TGAbstractMultiSet.DoSymmSubtract(aSet: TSpecMultiSet);
var
  e: TEntry;
begin
  if aSet <> Self then
    for e in aSet.Entries do
      DoSymmSubEntry(e)
  else
    Clear;
end;

function TGAbstractMultiSet.DoAdd(constref aKey: T): Boolean;
var
  p: PEntry;
begin
{$PUSH}{$Q+}
  Inc(FCount);
{$POP}
  if FindOrAdd(aKey, p) then
    Inc(p^.Count);
  Result := True;
end;
function TGAbstractMultiSet.DoAddAll(e: IEnumerable): SizeInt;
var
  v: T;
  o: TObject;
begin
  o := e._GetRef;
  if o is TSpecMultiSet then
    begin
      Result := ElemCount;
      DoArithAdd(TSpecMultiSet(o));
      Result := ElemCount - Result;
    end
  else
    begin
      Result := 0;
      for v in e do
        Result += Ord(DoAdd(v));
    end;
end;

function TGAbstractMultiSet.DoRemoveAll(e: IEnumerable): SizeInt;
var
  o: TObject;
  v: T;
begin
  o := e._GetRef;
  if o is TSpecMultiSet then
    begin
      Result := ElemCount;
      DoArithSubtract(TSpecMultiSet(o));
      Result -= ElemCount;
    end
  else
    begin
      Result := 0;
      for v in e do
        Result += Ord(DoRemove(v));
    end;
end;

function TGAbstractMultiSet.Contains(constref aValue: T): Boolean;
begin
  Result := FindEntry(aValue) <> nil;
end;

function TGAbstractMultiSet.IsSuperSet(aSet: TSpecMultiSet): Boolean;
var
  e: TEntry;
begin
  if aSet <> Self then
    begin
      if (Count >= aSet.Count) and (EntryCount >= aSet.EntryCount) then
        begin
          for e in aSet.Entries do
            if GetKeyCount(e.Key) < e.Count then
              exit(False);
          Result := True;
        end
      else
        Result := False;
    end
  else
    Result := True;
end;

function TGAbstractMultiSet.IsSubSet(aSet: TSpecMultiSet): Boolean;
var
  e: TEntry;
begin
  if aSet <> Self then
    begin
      if (aSet.Count >= Count) and (aSet.EntryCount >= EntryCount) then
        begin
          for e in Entries do
            if aSet[e.Key] < e.Count then
              exit(False);
          Result := True;
        end
      else
        Result := False;
    end
  else
    Result := True;
end;

function TGAbstractMultiSet.IsEqual(aSet: TSpecMultiSet): Boolean;
var
  e: TEntry;
begin
  if aSet <> Self then
    begin
      if (aSet.Count = Count) and (aSet.EntryCount = EntryCount) then
        begin
          for e in Entries do
            if aSet[e.Key] <> e.Count then
              exit(False);
          Result := True;
        end
      else
        Result := False;
    end
  else
    Result := True;
end;

function TGAbstractMultiSet.Intersecting(aSet: TSpecMultiSet): Boolean;
begin
  Result := ContainsAny(aSet.Distinct);
end;

procedure TGAbstractMultiSet.Intersect(aSet: TSpecMultiSet);
begin
  if aSet <> Self then
    begin
      CheckInIteration;
      DoIntersect(aSet);
    end;
end;

procedure TGAbstractMultiSet.Join(aSet: TSpecMultiSet);
var
  e: TEntry;
begin
  if aSet <> Self then
    begin
      CheckInIteration;
      for e in aSet.Entries do
        DoJoinEntry(e);
    end;
end;

procedure TGAbstractMultiSet.ArithmeticAdd(aSet: TSpecMultiSet);
begin
  CheckInIteration;
  DoArithAdd(aSet);
end;

procedure TGAbstractMultiSet.ArithmeticSubtract(aSet: TSpecMultiSet);
begin
  CheckInIteration;
  DoArithSubtract(aSet);
end;

procedure TGAbstractMultiSet.SymmetricSubtract(aSet: TSpecMultiSet);
begin
  CheckInIteration;
  DoSymmSubtract(aSet);
end;

function TGAbstractMultiSet.Distinct: IEnumerable;
begin
  BeginIteration;
  Result := GetDistinct;
end;

function TGAbstractMultiSet.Entries: IEntryEnumerable;
begin
  BeginIteration;
  Result := GetEntries;
end;

{ TSimpleIterable }

function TSimpleIterable.GetInIteration: Boolean;
begin
  Result := Boolean(LongBool(FItCounter));
end;

procedure TSimpleIterable.CapacityExceedError(aValue: SizeInt);
begin
  raise ELGCapacityExceed.CreateFmt(SEClassCapacityExceedFmt, [ClassName, aValue]);
end;

procedure TSimpleIterable.UpdateLockError;
begin
  raise ELGUpdateLock.CreateFmt(SECantUpdDuringIterFmt, [ClassName]);
end;

procedure TSimpleIterable.CheckInIteration;
begin
  if InIteration then
    UpdateLockError;
end;

procedure TSimpleIterable.BeginIteration;
begin
  Inc(FItCounter);
end;

procedure TSimpleIterable.EndIteration;
begin
  Dec(FItCounter);
end;

{ TGAbstractMap.TExtractHelper }

procedure TGAbstractMap.TExtractHelper.OnExtract(p: PEntry);
var
  c: SizeInt;
begin
  c := System.Length(FExtracted);
  if FCurrIndex = c then
    System.SetLength(FExtracted, c shl 1);
  FExtracted[FCurrIndex] := p^;
  Inc(FCurrIndex);
end;

procedure TGAbstractMap.TExtractHelper.Init;
begin
  FCurrIndex := 0;
  System.SetLength(FExtracted, ARRAY_INITIAL_SIZE);
end;

function TGAbstractMap.TExtractHelper.Final: TEntryArray;
begin
  System.SetLength(FExtracted, FCurrIndex);
  Result := FExtracted;
end;

{ TGAbstractMap.TCustomKeyEnumerable }

constructor TGAbstractMap.TCustomKeyEnumerable.Create(aMap: TSpecMap);
begin
  inherited Create;
  FOwner := aMap;
end;

destructor TGAbstractMap.TCustomKeyEnumerable.Destroy;
begin
  FOwner.EndIteration;
  inherited;
end;

{ TGAbstractMap.TCustomValueEnumerable }

constructor TGAbstractMap.TCustomValueEnumerable.Create(aMap: TSpecMap);
begin
  inherited Create;
  FOwner := aMap;
end;

destructor TGAbstractMap.TCustomValueEnumerable.Destroy;
begin
  FOwner.EndIteration;
  inherited;
end;

{ TGAbstractMap.TCustomEntryEnumerable }

constructor TGAbstractMap.TCustomEntryEnumerable.Create(aMap: TSpecMap);
begin
  inherited Create;
  FOwner := aMap;
end;

destructor TGAbstractMap.TCustomEntryEnumerable.Destroy;
begin
  FOwner.EndIteration;
  inherited;
end;

{ TGAbstractMap }

function TGAbstractMap._GetRef: TObject;
begin
  Result := Self;
end;

function TGAbstractMap.DoRemove(constref aKey: TKey): Boolean;
var
  v: TValue;
begin
  Result := DoExtract(aKey, v);
end;

function TGAbstractMap.GetValue(const aKey: TKey): TValue;
begin
  if not TryGetValue(aKey, Result) then
    raise ELGMapError.Create(SEKeyNotFound);
end;

function TGAbstractMap.DoSetValue(constref aKey: TKey; constref aNewValue: TValue): Boolean;
var
  p: PEntry;
begin
  p := Find(aKey);
  Result := p <> nil;
  if Result then
    p^.Value := aNewValue;
end;

function TGAbstractMap.DoAdd(constref aKey: TKey; constref aValue: TValue): Boolean;
var
  p: PEntry;
begin
  Result := not FindOrAdd(aKey, p);
  if Result then
    p^.Value := aValue;
end;

function TGAbstractMap.DoAddOrSetValue(const aKey: TKey; const aValue: TValue): Boolean;
var
  p: PEntry;
begin
  Result := not FindOrAdd(aKey, p);
  p^.Value := aValue;
end;

function TGAbstractMap.DoAddAll(constref a: array of TEntry): SizeInt;
var
  e: TEntry;
begin
  Result := 0;
  for e in a do
    Result += Ord(DoAdd(e.Key, e.Value));
end;

function TGAbstractMap.DoAddAll(e: IEntryEnumerable): SizeInt;
var
  Entry: TEntry;
begin
  Result := 0;
  if e._GetRef <> Self then
    for Entry in e do
      Result += Ord(DoAdd(Entry.Key, Entry.Value));
end;

function TGAbstractMap.DoRemoveAll(constref a: array of TKey): SizeInt;
var
  k: TKey;
begin
  Result := 0;
  for k in a do
    Result += Ord(DoRemove(k));
end;

function TGAbstractMap.DoRemoveAll(e: IKeyEnumerable): SizeInt;
var
  k: TKey;
begin
  Result := 0;
  if NonEmpty then
    for k in e do
      Result += Ord(DoRemove(k))
  else
    e.Discard;
end;

function TGAbstractMap.ToArray: TEntryArray;
var
  I: Integer = 0;
  e: TEntry;
begin
  System.SetLength(Result, Count);
  for e in Entries do
    begin
      Result[I] := e;
      Inc(I);
    end;
end;

function TGAbstractMap.IsEmpty: Boolean;
begin
  Result := Count = 0;
end;

function TGAbstractMap.NonEmpty: Boolean;
begin
  Result := Count <> 0;
end;

procedure TGAbstractMap.Clear;
begin
  CheckInIteration;
  DoClear;
end;

procedure TGAbstractMap.EnsureCapacity(aValue: SizeInt);
begin
  CheckInIteration;
  DoEnsureCapacity(aValue);
end;

procedure TGAbstractMap.TrimToFit;
begin
  CheckInIteration;
  DoTrimToFit;
end;

function TGAbstractMap.TryGetValue(constref aKey: TKey; out aValue: TValue): Boolean;
var
  p: PEntry;
begin
  p := Find(aKey);
  Result := p <> nil;
  if Result then
    aValue := p^.Value;
end;

function TGAbstractMap.GetValueDef(constref aKey: TKey; constref aDefault: TValue): TValue;
begin
  if not TryGetValue(aKey, Result) then
    Result := aDefault;
end;

function TGAbstractMap.Add(constref aKey: TKey; constref aValue: TValue): Boolean;
begin
  CheckInIteration;
  Result := DoAdd(aKey, aValue);
end;

function TGAbstractMap.Add(constref e: TEntry): Boolean;
begin
  Result := Add(e.Key, e.Value);
end;

procedure TGAbstractMap.AddOrSetValue(const aKey: TKey; const aValue: TValue);
begin
  CheckInIteration;
  DoAddOrSetValue(aKey, aValue);
end;

function TGAbstractMap.AddOrSetValue(constref e: TEntry): Boolean;
begin
  CheckInIteration;
  Result := DoAddOrSetValue(e.Key, e.Value);
end;

function TGAbstractMap.AddAll(constref a: array of TEntry): SizeInt;
begin
  CheckInIteration;
  Result := DoAddAll(a);
end;

function TGAbstractMap.AddAll(e: IEntryEnumerable): SizeInt;
begin
  if not InIteration then
    Result := DoAddAll(e)
  else
    begin
      Result := 0;
      e.Any;
      UpdateLockError;
    end;
end;

function TGAbstractMap.Replace(constref aKey: TKey; constref aNewValue: TValue): Boolean;
begin
  CheckInIteration;
  Result := DoSetValue(aKey, aNewValue);
end;

function TGAbstractMap.Contains(constref aKey: TKey): Boolean;
begin
  Result := Find(aKey) <> nil;
end;

function TGAbstractMap.NonContains(constref aKey: TKey): Boolean;
begin
  Result := not Contains(aKey);
end;

function TGAbstractMap.ContainsAny(constref a: array of TKey): Boolean;
var
  k: TKey;
begin
  for k in a do
    if Contains(k) then
      exit(True);
  Result := False;
end;

function TGAbstractMap.ContainsAny(e: IKeyEnumerable): Boolean;
var
  k: TKey;
begin
  if NonEmpty then
    begin
      for k in e do
        if Contains(k) then
          exit(True);
    end
  else
    e.Discard;
  Result := False;
end;

function TGAbstractMap.ContainsAll(constref a: array of TKey): Boolean;
var
  k: TKey;
begin
  for k in a do
    if not Contains(k) then
      exit(False);
  Result := True;
end;

function TGAbstractMap.ContainsAll(e: IKeyEnumerable): Boolean;
var
  k: TKey;
begin
  for k in e do
    if not Contains(k) then
      exit(False);
  Result := True;
end;

function TGAbstractMap.Remove(constref aKey: TKey): Boolean;
begin
  CheckInIteration;
  Result := DoRemove(aKey);
end;

function TGAbstractMap.RemoveAll(constref a: array of TKey): SizeInt;
begin
  CheckInIteration;
  Result := DoRemoveAll(a);
end;

function TGAbstractMap.RemoveAll(e: IKeyEnumerable): SizeInt;
begin
  if not InIteration then
    Result := DoRemoveAll(e)
  else
    begin
      Result := 0;
      e.Discard;
      UpdateLockError;
    end;
end;

function TGAbstractMap.RemoveIf(aTest: TKeyTest): SizeInt;
begin
  CheckInIteration;
  Result := DoRemoveIf(aTest);
end;

function TGAbstractMap.RemoveIf(aTest: TOnKeyTest): SizeInt;
begin
  CheckInIteration;
  Result := DoRemoveIf(aTest);
end;

function TGAbstractMap.RemoveIf(aTest: TNestKeyTest): SizeInt;
begin
  CheckInIteration;
  Result := DoRemoveIf(aTest);
end;

function TGAbstractMap.Extract(constref aKey: TKey; out v: TValue): Boolean;
begin
  CheckInIteration;
  Result := DoExtract(aKey, v);
end;

function TGAbstractMap.ExtractIf(aTest: TKeyTest): TEntryArray;
begin
  CheckInIteration;
  Result := DoExtractIf(aTest);
end;

function TGAbstractMap.ExtractIf(aTest: TOnKeyTest): TEntryArray;
begin
  CheckInIteration;
  Result := DoExtractIf(aTest);
end;

function TGAbstractMap.ExtractIf(aTest: TNestKeyTest): TEntryArray;
begin
  CheckInIteration;
  Result := DoExtractIf(aTest);
end;

procedure TGAbstractMap.RetainAll(aCollection: IKeyCollection);
begin
  Assert(aCollection = aCollection); //to supress hints
  CheckInIteration;
  DoRemoveIf(@aCollection.NonContains);
end;


function TGAbstractMap.Keys: IKeyEnumerable;
begin
  BeginIteration;
  Result := GetKeys;
end;

function TGAbstractMap.Values: IValueEnumerable;
begin
  BeginIteration;
  Result := GetValues;
end;

function TGAbstractMap.Entries: IEntryEnumerable;
begin
  BeginIteration;
  Result := GetEntries;
end;

{ TGThreadRWMap }

function TGThreadRWMap.GetCount: SizeInt;
begin
  FRWLock.BeginRead;
  try
    Result := FMap.Count;
  finally
    FRWLock.EndRead;
  end;
end;

function TGThreadRWMap.GetCapacity: SizeInt;
begin
  FRWLock.BeginRead;
  try
    Result := FMap.Capacity;
  finally
    FRWLock.EndRead;
  end;
end;

procedure TGThreadRWMap.BeginRead;
begin
  FRWLock.BeginRead;
end;

procedure TGThreadRWMap.BeginWrite;
begin
  FRWLock.BeginWrite;
end;

constructor TGThreadRWMap.Create(aMap: IMap; aOwnsMap: Boolean);
begin
  FRWLock := TMultiReadExclusiveWriteSynchronizer.Create;
  FMap := aMap;
  FOwnsMap := aOwnsMap;
end;

destructor TGThreadRWMap.Destroy;
begin
  FRWLock.BeginWrite;
  try
    if OwnsMap then
      FMap._GetRef.Free;
    FMap := nil;
    inherited;
  finally
    FRWLock.EndWrite;
    FRWLock.Free;
  end;
end;

function TGThreadRWMap.ReadMap: IRoMap;
begin
  FRWLock.BeginRead;
  Result := TMap(FMap._GetRef);
end;

procedure TGThreadRWMap.EndRead;
begin
  FRWLock.EndRead;
end;

function TGThreadRWMap.WriteMap: IMap;
begin
  FRWLock.BeginWrite;
  Result := FMap;
end;

procedure TGThreadRWMap.EndWrite;
begin
  FRWLock.EndWrite;
end;

function TGThreadRWMap.Add(constref aKey: TKey; constref aValue: TValue): Boolean;
begin
  FRWLock.BeginWrite;
  try
    Result := FMap.Add(aKey, aValue);
  finally
    FRWLock.EndWrite;
  end;
end;

procedure TGThreadRWMap.AddOrSetValue(const aKey: TKey; constref aValue: TValue);
begin
  FRWLock.BeginWrite;
  try
    FMap.AddOrSetValue(aKey, aValue);
  finally
    FRWLock.EndWrite;
  end;
end;

function TGThreadRWMap.TryGetValue(constref aKey: TKey; out aValue: TValue): Boolean;
begin
  FRWLock.BeginRead;
  try
    Result := FMap.TryGetValue(aKey, aValue);
  finally
    FRWLock.EndRead;
  end;
end;

function TGThreadRWMap.GetValueDef(constref aKey: TKey; constref aDefault: TValue): TValue;
begin
  FRWLock.BeginRead;
  try
    Result := FMap.GetValueDef(aKey, aDefault);
  finally
    FRWLock.EndRead;
  end;
end;

function TGThreadRWMap.Replace(constref aKey: TKey; constref aNewValue: TValue): Boolean;
begin
  FRWLock.BeginWrite;
  try
    Result := FMap.Replace(aKey, aNewValue);
  finally
    FRWLock.EndWrite;
  end;
end;

function TGThreadRWMap.Contains(constref aKey: TKey): Boolean;
begin
  FRWLock.BeginRead;
  try
    Result := FMap.Contains(aKey);
  finally
    FRWLock.EndRead;
  end;
end;

function TGThreadRWMap.Extract(constref aKey: TKey; out aValue: TValue): Boolean;
begin
  FRWLock.BeginWrite;
  try
    Result := FMap.Extract(aKey, aValue);
  finally
    FRWLock.EndWrite;
  end;
end;

function TGThreadRWMap.Remove(constref aKey: TKey): Boolean;
begin
  FRWLock.BeginWrite;
  try
    Result := FMap.Remove(aKey);
  finally
    FRWLock.EndWrite;
  end;
end;

{ TGAbstractMultiMap.TAbstractValueSet }

function TGAbstractMultiMap.TAbstractValueSet.ToArray: TValueArray;
var
  I, Len: SizeInt;
begin
  Len := ARRAY_INITIAL_SIZE;
  SetLength(Result, ARRAY_INITIAL_SIZE);
  I := 0;
  with GetEnumerator do
    try
      while MoveNext do
        begin
          if I = Len then
            begin
              Len += Len;
              SetLength(Result, Len);
            end;
          Result[I] := Current;
          Inc(I);
        end;
    finally
      Free;
    end;
  SetLength(Result, I);
end;

{ TGAbstractMultiMap.TCustomValueEnumerable }

constructor TGAbstractMultiMap.TCustomValueEnumerable.Create(aMap: TGAbstractMultiMap);
begin
  inherited Create;
  FOwner := aMap;
end;

destructor TGAbstractMultiMap.TCustomValueEnumerable.Destroy;
begin
  FOwner.EndIteration;
  inherited;
end;

{ TGAbstractMultiMap.TCustomEntryEnumerable }

constructor TGAbstractMultiMap.TCustomEntryEnumerable.Create(aMap: TGAbstractMultiMap);
begin
  inherited Create;
  FOwner := aMap;
end;

destructor TGAbstractMultiMap.TCustomEntryEnumerable.Destroy;
begin
  FOwner.EndIteration;
  inherited;
end;

{ TGAbstractMultiMap.TCustomValueCursor }

constructor TGAbstractMultiMap.TCustomValueCursor.Create(e: TSpecEnumerator; aMap: TGAbstractMultiMap);
begin
  inherited Create(e);
  FOwner := aMap;
end;

destructor TGAbstractMultiMap.TCustomValueCursor.Destroy;
begin
  FOwner.EndIteration;
  inherited;
end;

{ TGAbstractMultiMap }

function TGAbstractMultiMap.DoAdd(constref aKey: TKey; constref aValue: TValue): Boolean;
var
  p: PMMEntry;
begin
  p := FindOrAdd(aKey);
  Result := p^.Values.Add(aValue);
  FCount += Ord(Result);
end;

function TGAbstractMultiMap.DoAddAll(constref a: array of TEntry): SizeInt;
var
  e: TEntry;
begin
  Result := 0;
  for e in a do
    Result += Ord(DoAdd(e.Key, e.Value));
end;

function TGAbstractMultiMap.DoAddAll(e: IEntryEnumerable): SizeInt;
var
  Entry: TEntry;
begin
  Result := 0;
  for Entry in e do
    Result += Ord(DoAdd(Entry.Key, Entry.Value));
end;

function TGAbstractMultiMap.DoAddValues(constref aKey: TKey; constref a: array of TValue): SizeInt;
var
  p: PMMEntry;
  v: TValue;
begin
  Result := 0;
  if System.Length(a) > 0 then
    begin
      p := FindOrAdd(aKey);
      for v in a do
        Result += Ord(p^.Values.Add(v));
      FCount += Result;
    end;
end;

function TGAbstractMultiMap.DoAddValues(constref aKey: TKey; e: IValueEnumerable): SizeInt;
var
  p: PMMEntry = nil;
  v: TValue;
begin
  Result := 0;
  for v in e do
    begin
      if p = nil then
        p := FindOrAdd(aKey);
      Result += Ord(p^.Values.Add(v));
    end;
  FCount += Result;
end;

function TGAbstractMultiMap.DoRemove(constref aKey: TKey; constref aValue: TValue): Boolean;
var
  p: PMMEntry;
begin
  p := Find(aKey);
  if p <> nil then
    begin
      Result := p^.Values.Remove(aValue);
      FCount -= Ord(Result);
      if p^.Values.Count = 0 then
        DoRemoveKey(aKey);
    end
  else
    Result := False;
end;

function TGAbstractMultiMap.DoRemoveAll(constref a: array of TEntry): SizeInt;
var
  e: TEntry;
begin
  Result := 0;
  for e in a do
    Result += Ord(DoRemove(e.Key, e.Value));
end;

function TGAbstractMultiMap.DoRemoveAll(e: IEntryEnumerable): SizeInt;
var
  Entry: TEntry;
begin
  Result := 0;
  for Entry in e do
    Result += Ord(DoRemove(Entry.Key, Entry.Value));
end;

function TGAbstractMultiMap.DoRemoveValues(constref aKey: TKey; constref a: array of TValue): SizeInt;
var
  p: PMMEntry;
  v: TValue;
begin
  p := Find(aKey);
  Result := 0;
  if p <> nil then
    begin
      for v in a do
        Result += Ord( p^.Values.Remove(v));
      FCount -= Result;
      if p^.Values.Count = 0 then
        DoRemoveKey(aKey);
    end;
end;

function TGAbstractMultiMap.DoRemoveValues(constref aKey: TKey; e: IValueEnumerable): SizeInt;
var
  p: PMMEntry;
  v: TValue;
begin
  p := Find(aKey);
  Result := 0;
  if p <> nil then
    begin
      for v in e do
        Result += Ord( p^.Values.Remove(v));
      FCount -= Result;
      if p^.Values.Count = 0 then
        DoRemoveKey(aKey);
    end
  else
    e.FindFirst(v);///////////////
end;

function TGAbstractMultiMap.DoRemoveKeys(constref a: array of TKey): SizeInt;
var
  k: TKey;
begin
  Result := 0;
  for k in a do
    Result += DoRemoveKey(k);
  FCount -= Result;
end;

function TGAbstractMultiMap.DoRemoveKeys(e: IKeyEnumerable): SizeInt;
var
  k: TKey;
begin
  Result := 0;
  for k in e do
    Result += DoRemoveKey(k);
  FCount -= Result;
end;

function TGAbstractMultiMap.IsEmpty: Boolean;
begin
  Result := Count = 0;
end;

function TGAbstractMultiMap.NonEmpty: Boolean;
begin
  Result := Count <> 0;
end;

procedure TGAbstractMultiMap.Clear;
begin
  CheckInIteration;
  DoClear;
  FCount := 0;
end;

procedure TGAbstractMultiMap.EnsureCapacity(aValue: SizeInt);
begin
  CheckInIteration;
  DoEnsureCapacity(aValue);
end;

procedure TGAbstractMultiMap.TrimToFit;
begin
  CheckInIteration;
  DoTrimToFit;
end;

function TGAbstractMultiMap.Contains(constref aKey: TKey): Boolean;
begin
  Result := Find(aKey) <> nil;
end;

function TGAbstractMultiMap.ContainsValue(constref aKey: TKey; constref aValue: TValue): Boolean;
var
  p: PMMEntry;
begin
  p := Find(aKey);
  if p <> nil then
    Result := p^.Values.Contains(aValue)
  else
    Result := False;
end;

function TGAbstractMultiMap.Add(constref aKey: TKey; constref aValue: TValue): Boolean;
begin
  CheckInIteration;
  Result := DoAdd(aKey, aValue);
end;

function TGAbstractMultiMap.Add(constref e: TEntry): Boolean;
begin
  Result := Add(e.Key, e.Value);
end;

function TGAbstractMultiMap.AddAll(constref a: array of TEntry): SizeInt;
begin
  CheckInIteration;
  Result := DoAddAll(a);
end;

function TGAbstractMultiMap.AddAll(e: IEntryEnumerable): SizeInt;
begin
  if not InIteration then
    Result := DoAddAll(e)
  else
    begin
      Result := 0;
      e.Any;
      UpdateLockError;
    end;
end;

function TGAbstractMultiMap.AddValues(constref aKey: TKey; constref a: array of TValue): SizeInt;
begin
  CheckInIteration;
  Result := DoAddValues(aKey, a);
end;

function TGAbstractMultiMap.AddValues(constref aKey: TKey; e: IValueEnumerable): SizeInt;
begin
  CheckInIteration;
  Result := DoAddValues(aKey, e);
end;

function TGAbstractMultiMap.Remove(constref aKey: TKey; constref aValue: TValue): Boolean;
begin
  CheckInIteration;
  Result := DoRemove(aKey, aValue);
end;

function TGAbstractMultiMap.Remove(constref e: TEntry): Boolean;
begin
  Result := Remove(e.Key, e.Value);
end;

function TGAbstractMultiMap.RemoveAll(constref a: array of TEntry): SizeInt;
begin
  CheckInIteration;
  Result := DoRemoveAll(a);
end;

function TGAbstractMultiMap.RemoveAll(e: IEntryEnumerable): SizeInt;
begin
  if not InIteration then
    Result := DoRemoveAll(e)
  else
    begin
      Result := 0;
      e.Any;
      UpdateLockError;
    end;
end;

function TGAbstractMultiMap.RemoveValues(constref aKey: TKey; constref a: array of TValue): SizeInt;
begin
  CheckInIteration;
  Result := DoRemoveValues(aKey, a);
end;

function TGAbstractMultiMap.RemoveValues(constref aKey: TKey; e: IValueEnumerable): SizeInt;
begin
  if not InIteration then
    Result := DoRemoveValues(aKey, e)
  else
    begin
      Result := 0;
      e.Any;
      UpdateLockError;
    end;
end;

function TGAbstractMultiMap.RemoveKey(constref aKey: TKey): SizeInt;
begin
  CheckInIteration;
  Result := DoRemoveKey(aKey);
  FCount -= Result;
end;

function TGAbstractMultiMap.RemoveKeys(constref a: array of TKey): SizeInt;
begin
  CheckInIteration;
  Result := DoRemoveKeys(a);
end;

function TGAbstractMultiMap.RemoveKeys(e: IKeyEnumerable): SizeInt;
begin
  if not InIteration then
    Result := DoRemoveKeys(e)
  else
    begin
      Result := 0;
      e.Any;
      UpdateLockError;
    end;
end;

function TGAbstractMultiMap.ValuesView(const aKey: TKey): IValueEnumerable;
var
  p: PMMEntry;
begin
  p := Find(aKey);
  if p <> nil then
    begin
      BeginIteration;
      Result := TCustomValueCursor.Create(p^.Values.GetEnumerator, Self);
    end
  else
    Result := specialize TGArrayCursor<TValue>.Create(nil);
end;

function TGAbstractMultiMap.Keys: IKeyEnumerable;
begin
  BeginIteration;
  Result := GetKeys;
end;

function TGAbstractMultiMap.Values: IValueEnumerable;
begin
  BeginIteration;
  Result := GetValues;
end;

function TGAbstractMultiMap.Entries: IEntryEnumerable;
begin
  BeginIteration;
  Result := GetEntries;
end;

function TGAbstractMultiMap.ValueCount(constref aKey: TKey): SizeInt;
var
  p: PMMEntry;
begin
  p := Find(aKey);
  if p <> nil then
    Result := p^.Values.Count
  else
    Result := 0;
end;

{ TGAbstractTable2D.TCellData }

constructor TGAbstractTable2D.TCellData.Create(constref aRow: TRow; constref aCol: TCol; constref aValue: TValue);
begin
  Row := aRow;
  Column := aCol;
  Value := aValue;
end;

{ TGAbstractTable2D.TColData }

constructor TGAbstractTable2D.TColData.Create(constref aRow: TRow; constref aValue: TValue);
begin
  Row := aRow;
  Value := aValue;
end;

{ TGAbstractTable2D.TRowData }

constructor TGAbstractTable2D.TRowData.Create(constref aCol: TCol; constref aValue: TValue);
begin
  Column := aCol;
  Value := aValue;
end;

{ TGAbstractTable2D.TCustomRowMap }

function TGAbstractTable2D.TCustomRowMap.IsEmpty: Boolean;
begin
  Result := Count = 0;
end;

function TGAbstractTable2D.TCustomRowMap.GetValueOrDefault(const aCol: TCol): TValue;
begin
  if not TryGetValue(aCol, Result) then
    Result := Default(TValue);
end;

{ TGAbstractTable2D }

function TGAbstractTable2D.GetColCount(const aRow: TRow): SizeInt;
var
  p: PRowEntry;
begin
  p := DoFindRow(aRow);
  if p <> nil then
    Result := p^.Columns.Count
  else
    Result := 0;
end;

function TGAbstractTable2D.GetRowMap(const aRow: TRow): IRowMap;
var
  p: PRowEntry;
begin
  DoFindOrAddRow(aRow, p);
  Result := p^.Columns;
end;

function TGAbstractTable2D.GetCell(const aRow: TRow; const aCol: TCol): TValue;
begin
  if not FindCell(aRow, aCol, Result) then
    raise ELGTableError.CreateFmt(SECellNotFoundFmt, [ClassName]);
end;

function TGAbstractTable2D.IsEmpty: Boolean;
begin
  Result := CellCount = 0;
end;

function TGAbstractTable2D.NonEmpty: Boolean;
begin
  Result := CellCount <> 0;
end;

function TGAbstractTable2D.ContainsRow(constref aRow: TRow): Boolean;
begin
  Result := DoFindRow(aRow) <> nil;
end;

function TGAbstractTable2D.FindRow(constref aRow: TRow; out aMap: IRowMap): Boolean;
var
  p: PRowEntry;
begin
  p := DoFindRow(aRow);
  Result := p <> nil;
  if Result then
    aMap := p^.Columns;
end;

function TGAbstractTable2D.AddRow(constref aRow: TRow): Boolean;
var
  p: PRowEntry;
begin
  Result := not DoFindOrAddRow(aRow, p);
end;

function TGAbstractTable2D.AddRows(constref a: array of TRow): SizeInt;
var
  r: TRow;
begin
  Result := 0;
  for r in a do
    Result += Ord(AddRow(r));
end;

function TGAbstractTable2D.RemoveRow(constref aRow: TRow): SizeInt;
begin
  Result := DoRemoveRow(aRow);
end;

function TGAbstractTable2D.RemoveColumn(constref aCol: TCol): SizeInt;
var
  Map: IRowMap;
begin
  Result := 0;
  for Map in EnumRowMaps do
    Result += Ord(Map.Remove(aCol));
end;

function TGAbstractTable2D.ContainsCell(constref aRow: TRow; constref aCol: TCol): Boolean;
var
  p: PRowEntry;
begin
  p := DoFindRow(aRow);
  if p <> nil then
    Result := p^.Columns.Contains(aCol)
  else
    Result := False;
end;

function TGAbstractTable2D.FindCell(constref aRow: TRow; constref aCol: TCol; out aValue: TValue): Boolean;
var
  p: PRowEntry;
begin
  p := DoFindRow(aRow);
  if p <> nil then
    Result := p^.Columns.TryGetValue(aCol, aValue)
  else
    Result := False;
end;

function TGAbstractTable2D.GetCellDef(constref aRow: TRow; constref aCol: TCol; aDef: TValue): TValue;
begin
  if not FindCell(aRow, aCol, Result) then
    Result := aDef;
end;

procedure TGAbstractTable2D.AddOrSetCell(const aRow: TRow; const aCol: TCol; const aValue: TValue);
var
  p: PRowEntry;
begin
  DoFindOrAddRow(aRow, p);
  p^.Columns[aCol] := aValue;
end;

function TGAbstractTable2D.AddCell(constref aRow: TRow; constref aCol: TCol; constref aValue: TValue): Boolean;
begin
  Result := not ContainsCell(aRow, aCol);
  if Result then
    AddOrSetCell(aRow, aCol, aValue);
end;

function TGAbstractTable2D.AddCell(constref e: TCellData): Boolean;
begin
  Result := AddCell(e.Row, e.Column, e.Value);
end;

function TGAbstractTable2D.AddCells(constref a: array of TCellData): SizeInt;
var
  e: TCellData;
begin
  Result := 0;
  for e in a do
    Result += Ord(AddCell(e));
end;

function TGAbstractTable2D.RemoveCell(constref aRow: TRow; constref aCol: TCol): Boolean;
var
  p: PRowEntry;
begin
  p := DoFindRow(aRow);
  if p <> nil then
    begin
      Result := p^.Columns.Remove(aCol);
      if Result and p^.Columns.IsEmpty then
        DoRemoveRow(aRow);
    end
  else
    Result := False;
end;

end.

