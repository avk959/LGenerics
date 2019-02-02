{****************************************************************************
*                                                                           *
*   This file is part of the LGenerics package.                             *
*   Generic hashset implementations.                                        *
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
unit LGHashSet;

{$MODE OBJFPC}{$H+}
{$INLINE ON}{$WARN 6058 off : }
{$MODESWITCH NESTEDPROCVARS}
{$MODESWITCH ADVANCEDRECORDS}

interface

uses

  SysUtils,
  LGUtils,
  {%H-}LGHelpers,
  LGAbstractContainer,
  LGHashTable,
  LGStrConst;

type

  {TGAbstractHashSet: common abstract ancestor hashset class }
  generic TGAbstractHashSet<T> = class abstract(specialize TGAbstractSet<T>)
  public
  type
    TAbstractHashSet = specialize TGAbstractHashSet<T>;

  protected
  type
    THashTable      = specialize TGAbstractHashTable<T, TEntry>;
    THashTableClass = class of THashTable;
    THashSetClass   = class of TAbstractHashSet;
    TSearchResult   = THashTable.TSearchResult;

    TEnumerator = class(TContainerEnumerator)
    private
      FEnum: THashTable.TEntryEnumerator;
    protected
      function  GetCurrent: T; override;
    public
      constructor Create(hs: TAbstractHashSet);
      destructor Destroy; override;
      function  MoveNext: Boolean; override;
      procedure Reset; override;
    end;

    TDistinctEnumerable = class(specialize TGEnumCursor<T>)
    protected
      FSet: TAbstractHashSet;
    public
      constructor Create(e: TSpecEnumerator; aSetClass: THashSetClass);
      destructor Destroy; override;
      function  MoveNext: Boolean; override;
      procedure Reset; override;
    end;

  var
    FTable: THashTable;
    function  GetCount: SizeInt; override;
    function  GetCapacity: SizeInt; override;
    function  GetExpandTreshold: SizeInt; inline;
    function  GetFillRatio: Single; inline;
    function  GetLoadFactor: Single; inline;
    procedure SetLoadFactor(aValue: Single); inline;
    function  DoGetEnumerator: TSpecEnumerator; override;
    procedure DoClear; override;
    procedure DoTrimToFit; override;
    procedure DoEnsureCapacity(aValue: SizeInt); override;
    function  DoAdd(constref aValue: T): Boolean; override;
    function  DoExtract(constref aValue: T): Boolean; override;
    function  DoRemoveIf(aTest: TTest): SizeInt; override;
    function  DoRemoveIf(aTest: TOnTest): SizeInt; override;
    function  DoRemoveIf(aTest: TNestTest): SizeInt; override;
    function  DoExtractIf(aTest: TTest): TArray; override;
    function  DoExtractIf(aTest: TOnTest): TArray; override;
    function  DoExtractIf(aTest: TNestTest): TArray; override;
    class function GetClass: THashSetClass; virtual; abstract;
    class function GetTableClass: THashTableClass; virtual; abstract;
  public
    class function DefaultLoadFactor: Single; inline;
    class function MaxLoadFactor: Single; inline;
    class function MinLoadFactor: Single; inline;
    class function Distinct(constref a: TArray): IEnumerable; inline;
    class function Distinct(e: IEnumerable): IEnumerable; inline;
    constructor Create;
    constructor Create(constref a: array of T);
    constructor Create(e: IEnumerable);
    constructor Create(aCapacity: SizeInt);
    constructor Create(aCapacity: SizeInt; constref a: array of T);
    constructor Create(aCapacity: SizeInt; e: IEnumerable);
    constructor Create(aLoadFactor: Single);
    constructor Create(aLoadFactor: Single; constref a: array of T);
    constructor Create(aLoadFactor: Single; e: IEnumerable);
    constructor Create(aCapacity: SizeInt; aLoadFactor: Single);
    constructor Create(aCapacity: SizeInt; aLoadFactor: Single; constref a: array of T);
    constructor Create(aCapacity: SizeInt; aLoadFactor: Single; e: IEnumerable);
    constructor CreateCopy(aSet: TAbstractHashSet);
    destructor Destroy; override;
    function  Contains(constref aValue: T): Boolean; override;
    function  Clone: TAbstractHashSet; override;
    property  LoadFactor: Single read GetLoadFactor write SetLoadFactor;
    property  FillRatio: Single read GetFillRatio;
  { The number of elements that can be written without rehashing }
    property  ExpandTreshold: SizeInt read GetExpandTreshold;
  end;

  { TGBaseHashSetLP implements open addressing hashset with linear probing;
      functor TEqRel(equality relation) must provide:
        class function HashCode([const[ref]] aValue: T): SizeInt;
        class function Equal([const[ref]] L, R: T): Boolean; }
  generic TGBaseHashSetLP<T, TEqRel> = class(specialize TGAbstractHashSet<T>)
  protected
    class function GetClass: THashSetClass; override;
    class function GetTableClass: THashTableClass; override;
  end;

  { TGHashSetLP implements open addressing hashset with linear probing;
    it assumes that type T implements TEqRel }
  generic TGHashSetLP<T> = class(specialize TGBaseHashSetLP<T, T>);

  { TGBaseHashSetLPT implements open addressing hashset with linear probing and lazy deletion }
  generic TGBaseHashSetLPT<T, TEqRel> = class(specialize TGAbstractHashSet<T>)
  private
    function GetTombstonesCount: SizeInt; inline;
  protected
  type
    THashTableLPT = specialize TGOpenAddrLPT<T, TEntry, TEqRel>;

    class function GetTableClass: THashTableClass; override;
    class function GetClass: THashSetClass; override;
  public
    procedure ClearTombstones;
    property  TombstonesCount: SizeInt read GetTombstonesCount;
  end;

  { TGHashSetLPT implements open addressing hashset with linear probing and lazy deletion;
    it assumes that type T implements TEqRel }
  generic TGHashSetLPT<T> = class(specialize TGBaseHashSetLPT<T, T>);

  { TGBaseHashSetQP implements open addressing hashset with quadratic probing(c1 = c2 = 1/2) }
  generic TGBaseHashSetQP<T, TEqRel> = class(specialize TGAbstractHashSet<T>)
  private
    function GetTombstonesCount: SizeInt; inline;
  protected
  type
    THashTableQP = specialize TGOpenAddrQP<T, TEntry, TEqRel>;

    class function GetTableClass: THashTableClass; override;
    class function GetClass: THashSetClass; override;
  public
    procedure ClearTombstones;
    property  TombstonesCount: SizeInt read GetTombstonesCount;
  end;

  { TGHashSetQP implements open addressing hashset with quadratic probing(c1 = c2 = 1/2);
    it assumes that type T implements TEqRel }
  generic TGHashSetQP<T> = class(specialize TGBaseHashSetQP<T, T>);

  { TGBaseOrderedHashSet implements node based hashset with predictable iteration order,
    which is the order in which elements were inserted into the set (insertion-order) }
  generic TGBaseOrderedHashSet<T, TEqRel> = class(specialize TGAbstractHashSet<T>)
  protected
  type
    TOrderedHashTable = specialize TGOrderedHashTable<T, TEntry, TEqRel>;
    PNode             = TOrderedHashTable.PNode;

    TReverseEnumerable = class(TContainerEnumerable)
    protected
      FEnum: TOrderedHashTable.TReverseEnumerator;
      function  GetCurrent: T; override;
    public
      constructor Create(aSet: TGBaseOrderedHashSet);
      destructor Destroy; override;
      function  MoveNext: Boolean; override;
      procedure Reset; override;
    end;

    class function GetClass: THashSetClass; override;
    class function GetTableClass: THashTableClass; override;
  public
    function Reverse: IEnumerable; override;
  end;

  { TGOrderedHashSet implements node based hashset with predictable iteration order,
    which is the order in which elements were inserted into the set(insertion-order);
    it assumes that type T implements TEqRel }
  generic TGOrderedHashSet<T> = class(specialize TGBaseOrderedHashSet<T, T>);

  { TGBaseChainHashSet implements node based hashset with singly linked list chains }
  generic TGBaseChainHashSet<T, TEqRel> = class(specialize TGAbstractHashSet<T>)
  protected
    class function GetClass: THashSetClass; override;
    class function GetTableClass: THashTableClass; override;
  end;

  { TGChainHashSet implements node based hashset with singly linked list chains;
    it assumes that type T implements TEqRel }
  generic TGChainHashSet<T> = class(specialize TGBaseChainHashSet<T, T>);

  generic TGCustomObjectHashSet<T: class> = class abstract(specialize TGAbstractHashSet<T>)
  private
    FOwnsObjects: Boolean;
  protected
    procedure EntryRemoving(p: PEntry);
    procedure DoClear; override;
    function  DoRemove(constref aValue: T): Boolean; override;
    function  DoRemoveIf(aTest: TTest): SizeInt; override;
    function  DoRemoveIf(aTest: TOnTest): SizeInt; override;
    function  DoRemoveIf(aTest: TNestTest): SizeInt; override;
  public
    constructor Create(aOwnsObjects: Boolean = True);
    constructor Create(constref a: array of T; aOwnsObjects: Boolean = True);
    constructor Create(e: IEnumerable; aOwnsObjects: Boolean = True);
    constructor Create(aCapacity: SizeInt; aOwnsObjects: Boolean = True);
    constructor Create(aCapacity: SizeInt; constref a: array of T; aOwnsObjects: Boolean = True);
    constructor Create(aCapacity: SizeInt; e: IEnumerable; aOwnsObjects: Boolean = True);
    constructor Create(aLoadFactor: Single; aOwnsObjects: Boolean = True);
    constructor Create(aLoadFactor: Single; constref a: array of T; aOwnsObjects: Boolean = True);
    constructor Create(aLoadFactor: Single; e: IEnumerable; aOwnsObjects: Boolean = True);
    constructor Create(aCapacity: SizeInt; aLoadFactor: Single; aOwnsObjects: Boolean = True);
    constructor Create(aCapacity: SizeInt; aLoadFactor: Single; constref a: array of T; aOwnsObjects: Boolean = True);
    constructor Create(aCapacity: SizeInt; aLoadFactor: Single; e: IEnumerable; aOwnsObjects: Boolean = True);
    constructor CreateCopy(aSet: TGCustomObjectHashSet);
    property  OwnsObjects: Boolean read FOwnsObjects write FOwnsObjects;
  end;

  generic TGObjectHashSetLP<T: class; TEqRel> = class(specialize TGCustomObjectHashSet<T>)
  protected
    class function GetClass: THashSetClass; override;
    class function GetTableClass: THashTableClass; override;
  public
    function  Clone: TGObjectHashSetLP; override;
  end;

 { TGObjHashSetLP assumes that type T implements TEqRel }
  generic TGObjHashSetLP<T: class> = class(specialize TGObjectHashSetLP<T, T>);

  generic TGObjectHashSetLPT<T: class; TEqRel> = class(specialize TGCustomObjectHashSet<T>)
  private
    function GetTombstonesCount: SizeInt; inline;
  protected
  type
    THashTableLPT = specialize TGOpenAddrLPT<T, TEntry, TEqRel>;

    class function GetClass: THashSetClass; override;
    class function GetTableClass: THashTableClass; override;
  public
    function  Clone: TGObjectHashSetLPT; override;
    procedure ClearTombstones; inline;
    property  TombstonesCount: SizeInt read GetTombstonesCount;
  end;

  { TGObjHashSetLPT assumes that type T implements TEqRel }
  generic TGObjHashSetLPT<T: class> = class(specialize TGObjectHashSetLPT<T, T>);

  generic TGObjectHashSetQP<T: class; TEqRel> = class(specialize TGCustomObjectHashSet<T>)
  private
    function GetTombstonesCount: SizeInt; inline;
  protected
  type
    THashTableQP = specialize TGOpenAddrQP<T, TEntry, TEqRel>;

    class function GetClass: THashSetClass; override;
    class function GetTableClass: THashTableClass; override;
  public
    function  Clone: TGObjectHashSetQP; override;
    procedure ClearTombstones; inline;
    property  TombstonesCount: SizeInt read GetTombstonesCount;
  end;

  { TGObjHashSetQP assumes that type T implements TEqRel }
  generic TGObjHashSetQP<T: class> = class(specialize TGObjectHashSetQP<T, T>);

  generic TGObjectOrderedHashSet<T: class; TEqRel> = class(specialize TGBaseOrderedHashSet<T, TEqRel>)
  private
    FOwnsObjects: Boolean;
  protected
  type
    TObjectOrderedHashSet = TGObjectOrderedHashSet;

    procedure EntryRemoving(p: PEntry);
    procedure DoClear; override;
    function  DoRemove(constref aValue: T): Boolean; override;
    function  DoRemoveIf(aTest: TTest): SizeInt; override;
    function  DoRemoveIf(aTest: TOnTest): SizeInt; override;
    function  DoRemoveIf(aTest: TNestTest): SizeInt; override;
    class function GetClass: THashSetClass; override;
    class function GetTableClass: THashTableClass; override;
  public
    constructor Create(aOwnsObjects: Boolean = True);
    constructor Create(constref a: array of T; aOwnsObjects: Boolean = True);
    constructor Create(e: IEnumerable; aOwnsObjects: Boolean = True);
    constructor Create(aCapacity: SizeInt; aOwnsObjects: Boolean = True);
    constructor Create(aCapacity: SizeInt; constref a: array of T; aOwnsObjects: Boolean = True);
    constructor Create(aCapacity: SizeInt; e: IEnumerable; aOwnsObjects: Boolean = True);
    constructor Create(aLoadFactor: Single; aOwnsObjects: Boolean = True);
    constructor Create(aLoadFactor: Single; constref a: array of T; aOwnsObjects: Boolean = True);
    constructor Create(aLoadFactor: Single; e: IEnumerable; aOwnsObjects: Boolean = True);
    constructor Create(aCapacity: SizeInt; aLoadFactor: Single; aOwnsObjects: Boolean = True);
    constructor Create(aCapacity: SizeInt; aLoadFactor: Single; constref a: array of T; aOwnsObjects: Boolean = True);
    constructor Create(aCapacity: SizeInt; aLoadFactor: Single; e: IEnumerable; aOwnsObjects: Boolean = True);
    constructor CreateCopy(aSet: TGObjectOrderedHashSet);
    function  Clone: TGObjectOrderedHashSet; override;
    property  OwnsObjects: Boolean read FOwnsObjects write FOwnsObjects;
  end;

  { TGObjOrderedHashSet assumes that type T implements TEqRel }
  generic TGObjOrderedHashSet<T: class> = class(specialize TGObjectOrderedHashSet<T, T>);

  generic TGObjectChainHashSet<T: class; TEqRel> = class(specialize TGCustomObjectHashSet<T>)
  protected
    class function GetClass: THashSetClass; override;
    class function GetTableClass: THashTableClass; override;
  public
    function Clone: TGObjectChainHashSet; override;
  end;

  { TGObjChainHashSet assumes that type T implements TEqRel }
  generic TGObjChainHashSet<T: class> = class(specialize TGObjectChainHashSet<T, T>);

  { TGLiteHashSetLP implements open addressing hashset with linear probing;
      functor TEqRel(equality relation) must provide:
        class function HashCode([const[ref]] aValue: T): SizeInt;
        class function Equal([const[ref]] L, R: T): Boolean; }
  generic TGLiteHashSetLP<T, TEqRel> = record
  private
  type
    TEntry = record
      Key: T;
    end;
    PEntry = ^TEntry;

    TTableLP = specialize TGLiteHashTableLP<T, TEntry, TEqRel>;

  public
  type
    IEnumerable = specialize IGEnumerable<T>;
    ICollection = specialize IGCollection<T>;
    TTest       = specialize TGTest<T>;
    TOnTest     = specialize TGOnTest<T>;
    TNestTest   = specialize TGNestTest<T>;
    TArray      = array of T;

    TEnumerator = record
    private
      FEnum: TTableLP.TEnumerator;
      function  GetCurrent: T; inline;
      procedure Init(constref aSet: TGLiteHashSetLP); inline;
    public
      function  MoveNext: Boolean; inline;
      procedure Reset; inline;
      property  Current: T read GetCurrent;
    end;

  private
    FTable: TTableLP;
    function  GetCapacity: SizeInt; inline;
    function  GetCount: SizeInt; inline;
    function  GetFillRatio: Single; inline;
    function  GetLoadFactor: Single; inline;
    function  GetExpandTreshold: SizeInt; inline;
    procedure SetLoadFactor(aValue: Single); inline;
  public
    class operator +(constref L, R: TGLiteHashSetLP): TGLiteHashSetLP;
    class operator -(constref L, R: TGLiteHashSetLP): TGLiteHashSetLP;
    class operator *(constref L, R: TGLiteHashSetLP): TGLiteHashSetLP;
    class operator ><(constref L, R: TGLiteHashSetLP): TGLiteHashSetLP;
    class operator =(constref L, R: TGLiteHashSetLP): Boolean; inline;
    class operator <=(constref L, R: TGLiteHashSetLP): Boolean; inline;
    function  DefaultLoadFactor: Single; inline;
    function  MaxLoadFactor: Single; inline;
    function  MinLoadFactor: Single; inline;
    function  GetEnumerator: TEnumerator; inline;
    function  ToArray: TArray;
    function  IsEmpty: Boolean; inline;
    function  NonEmpty: Boolean; inline;
    procedure Clear; inline;
    procedure TrimToFit; inline;
    procedure EnsureCapacity(aValue: SizeInt); inline;
  { returns True if element added }
    function  Add(constref aValue: T): Boolean;
  { returns count of added elements }
    function  AddAll(constref a: array of T): SizeInt;
    function  AddAll(e: IEnumerable): SizeInt;
    function  AddAll(constref aSet: TGLiteHashSetLP): SizeInt;
    function  Contains(constref aValue: T): Boolean; inline;
    function  NonContains(constref aValue: T): Boolean; inline;
    function  ContainsAny(constref a: array of T): Boolean;
    function  ContainsAny(e: IEnumerable): Boolean;
    function  ContainsAny(constref aSet: TGLiteHashSetLP): Boolean;
    function  ContainsAll(constref a: array of T): Boolean;
    function  ContainsAll(e: IEnumerable): Boolean;
    function  ContainsAll(constref aSet: TGLiteHashSetLP): Boolean;
  { returns True if element removed }
    function  Remove(constref aValue: T): Boolean; inline;
  { returns count of removed elements }
    function  RemoveAll(constref a: array of T): SizeInt;
    function  RemoveAll(e: IEnumerable): SizeInt;
    function  RemoveAll(constref aSet: TGLiteHashSetLP): SizeInt;
  { returns count of removed elements }
    function  RemoveIf(aTest: TTest): SizeInt;
    function  RemoveIf(aTest: TOnTest): SizeInt;
    function  RemoveIf(aTest: TNestTest): SizeInt;
  { returns True if element extracted }
    function  Extract(constref aValue: T): Boolean; inline;
    function  ExtractIf(aTest: TTest): TArray;
    function  ExtractIf(aTest: TOnTest): TArray;
    function  ExtractIf(aTest: TNestTest): TArray;
  { will contain only those elements that are simultaneously contained in self and aCollection/aSet }
    procedure RetainAll(aCollection: ICollection);
    procedure RetainAll(constref aSet: TGLiteHashSetLP);
    function  IsSuperset(constref aSet: TGLiteHashSetLP): Boolean; inline;
    function  IsSubset(constref aSet: TGLiteHashSetLP): Boolean; inline;
    function  IsEqual(constref aSet: TGLiteHashSetLP): Boolean;
    function  Intersecting(constref aSet: TGLiteHashSetLP): Boolean; inline;
    procedure Intersect(constref aSet: TGLiteHashSetLP); inline;
    procedure Join(constref aSet: TGLiteHashSetLP);
    procedure Subtract(constref aSet: TGLiteHashSetLP);
    procedure SymmetricSubtract(constref aSet: TGLiteHashSetLP);
    property  Count: SizeInt read GetCount;
    property  Capacity: SizeInt read GetCapacity;
    property  LoadFactor: Single read GetLoadFactor write SetLoadFactor;
    property  FillRatio: Single read GetFillRatio;
    property  ExpandTreshold: SizeInt read GetExpandTreshold;
  end;

  { TGDisjointSetUnion: see https://en.wikipedia.org/wiki/Disjoint-set_data_structure }
  generic TGDisjointSetUnion<T, TEqRel> = record
  private
  type
    TEntry = record
      Key: T;
    end;
    PEntry = ^TEntry;

    TTable = specialize TGLiteChainHashTable<T, TEntry, TEqRel>;
    PNode  = TTable.PNode;

  public
  type
    TArray = array of T;

    TEnumerator = record
    private
      FList: PNode;
      FCurrIndex,
      FLastIndex: SizeInt;
      function  GetCurrent: T; inline;
    public
      function  MoveNext: Boolean; inline;
      procedure Reset; inline;
      property  Current: T read GetCurrent;
    end;

  private
    FTable: TTable;
    FDsu: array of SizeInt;
    function  GetCapacity: SizeInt; inline;
    function  GetCount: SizeInt; inline;
    function  FindOrAdd(constref aValue: T): SizeInt;
    procedure ExpandDsu;
    function  GetItem(aIndex: SizeInt): T; inline;
    function  GetTag(aValue: SizeInt): SizeInt;
  public
    function  GetEnumerator: TEnumerator; inline;
    function  ToArray: TArray;
    function  IsEmpty: Boolean; inline;
    function  NonEmpty: Boolean; inline;
    procedure Clear; inline;
    procedure EnsureCapacity(aValue: SizeInt); inline;
    function  IndexOf(constref aValue: T): SizeInt;
    function  Contains(constref aValue: T): Boolean; inline;
    function  NonContains(constref aValue: T): Boolean; inline;
  { returns index of the added element, -1 if such element already exists }
    function  Add(constref aValue: T): SizeInt;
  { destroys subsets }
    procedure Reset;
  { values related to the same subset will have the same Tag }
    function  Tag(constref aValue: T): SizeInt;
    function  TagI(aIndex: SizeInt): SizeInt;
    function  InSameSet(constref L, R: T): Boolean; inline;
    function  InSameSetI(L, R: SizeInt): Boolean; inline;
    function  InDiffSets(constref L, R: T): Boolean; inline;
    function  InDiffSetsI(L, R: SizeInt): Boolean; inline;
  { returns True and joins L and R, if L and R related to the different subsets, False otherwise }
    function  Join(constref L, R: T): Boolean; inline;
    function  JoinI(L, R: SizeInt): Boolean;
    property  Count: SizeInt read GetCount;
    property  Capacity: SizeInt read GetCapacity;
    property  Items[aIndex: SizeInt]: T read GetItem; default;
  end;

implementation
{$B-}{$COPERATORS ON}

{ TGAbstractHashSet.TEnumerator }

function TGAbstractHashSet.TEnumerator.GetCurrent: T;
begin
  Result := FEnum.GetCurrent^.Key;
end;

constructor TGAbstractHashSet.TEnumerator.Create(hs: TAbstractHashSet);
begin
  inherited Create(hs);
  FEnum := hs.FTable.GetEnumerator;
end;

destructor TGAbstractHashSet.TEnumerator.Destroy;
begin
  FEnum.Free;
  inherited;
end;

function TGAbstractHashSet.TEnumerator.MoveNext: Boolean;
begin
  Result := FEnum.MoveNext;
end;

procedure TGAbstractHashSet.TEnumerator.Reset;
begin
  FEnum.Reset;
end;

{ TGAbstractHashSet.TDistinctEnumerable }

constructor TGAbstractHashSet.TDistinctEnumerable.Create(e: TSpecEnumerator; aSetClass: THashSetClass);
begin
  inherited Create(e);
  FSet := aSetClass.Create;
end;

destructor TGAbstractHashSet.TDistinctEnumerable.Destroy;
begin
  FSet.Free;
  inherited;
end;

function TGAbstractHashSet.TDistinctEnumerable.MoveNext: Boolean;
begin
  repeat
    if not inherited MoveNext then
      exit(False);
    Result := FSet.Add(Current);
  until Result;
end;

procedure TGAbstractHashSet.TDistinctEnumerable.Reset;
begin
  inherited;
  FSet.Clear;
end;

{ TGAbstractHashSet }

function TGAbstractHashSet.GetExpandTreshold: SizeInt;
begin
  Result := FTable.ExpandTreshold;
end;

function TGAbstractHashSet.GetCount: SizeInt;
begin
  Result := FTable.Count;
end;

function TGAbstractHashSet.GetCapacity: SizeInt;
begin
  Result := FTable.Capacity;
end;

function TGAbstractHashSet.GetFillRatio: Single;
begin
  Result := FTable.FillRatio;
end;

function TGAbstractHashSet.GetLoadFactor: Single;
begin
  Result := FTable.LoadFactor;
end;

procedure TGAbstractHashSet.SetLoadFactor(aValue: Single);
begin
  FTable.LoadFactor := aValue;
end;

function TGAbstractHashSet.DoGetEnumerator: TSpecEnumerator;
begin
  Result := TEnumerator.Create(Self);
end;

procedure TGAbstractHashSet.DoClear;
begin
  FTable.Clear;
end;

procedure TGAbstractHashSet.DoTrimToFit;
begin
  FTable.TrimToFit;
end;

procedure TGAbstractHashSet.DoEnsureCapacity(aValue: SizeInt);
begin
  FTable.EnsureCapacity(aValue);
end;

function TGAbstractHashSet.DoAdd(constref aValue: T): Boolean;
var
  p: PEntry;
  sr: TSearchResult;
begin
  Result := not FTable.FindOrAdd(aValue, p, sr);
  if Result then
    p^.Key := aValue;
end;

function TGAbstractHashSet.DoExtract(constref aValue: T): Boolean;
begin
  Result := FTable.Remove(aValue);
end;

function TGAbstractHashSet.DoRemoveIf(aTest: TTest): SizeInt;
begin
  Result := FTable.RemoveIf(aTest);
end;

function TGAbstractHashSet.DoRemoveIf(aTest: TOnTest): SizeInt;
begin
  Result := FTable.RemoveIf(aTest);
end;

function TGAbstractHashSet.DoRemoveIf(aTest: TNestTest): SizeInt;
begin
  Result := FTable.RemoveIf(aTest);
end;

function TGAbstractHashSet.DoExtractIf(aTest: TTest): TArray;
var
  e: TExtractHelper;
begin
  e.Init;
  FTable.RemoveIf(aTest, @e.OnExtract);
  Result := e.Final;
end;

function TGAbstractHashSet.DoExtractIf(aTest: TOnTest): TArray;
var
  e: TExtractHelper;
begin
  e.Init;
  FTable.RemoveIf(aTest, @e.OnExtract);
  Result := e.Final;
end;

function TGAbstractHashSet.DoExtractIf(aTest: TNestTest): TArray;
var
  e: TExtractHelper;
begin
  e.Init;
  FTable.RemoveIf(aTest, @e.OnExtract);
  Result := e.Final;
end;

class function TGAbstractHashSet.DefaultLoadFactor: Single;
begin
  Result := GetTableClass.DefaultLoadFactor;
end;

class function TGAbstractHashSet.MaxLoadFactor: Single;
begin
  Result := GetTableClass.MaxLoadFactor;
end;

class function TGAbstractHashSet.MinLoadFactor: Single;
begin
  Result := GetTableClass.MinLoadFactor;
end;

class function TGAbstractHashSet.Distinct(constref a: TArray): IEnumerable;
begin
  Result := TDistinctEnumerable.Create(specialize TGArrayEnumerator<T>.Create(a), GetClass);
end;

class function TGAbstractHashSet.Distinct(e: IEnumerable): IEnumerable;
begin
  Result := TDistinctEnumerable.Create(e.GetEnumerator, GetClass);
end;

constructor TGAbstractHashSet.Create;
begin
  FTable := GetTableClass.Create;
end;

constructor TGAbstractHashSet.Create(constref a: array of T);
begin
  FTable := GetTableClass.Create;
  DoAddAll(a);
end;

constructor TGAbstractHashSet.Create(e: IEnumerable);
var
  o: TObject;
begin
  o := e._GetRef;
  if o is TAbstractHashSet then
    CreateCopy(TAbstractHashSet(o))
  else
    begin
      if o is TSpecSet then
        Create(TSpecSet(o).Count)
      else
        Create;
      DoAddAll(e);
    end;
end;

constructor TGAbstractHashSet.Create(aCapacity: SizeInt);
begin
  FTable := GetTableClass.Create(aCapacity);
end;

constructor TGAbstractHashSet.Create(aCapacity: SizeInt; constref a: array of T);
begin
  FTable := GetTableClass.Create(aCapacity);
  DoAddAll(a);
end;

constructor TGAbstractHashSet.Create(aCapacity: SizeInt; e: IEnumerable);
begin
  FTable := GetTableClass.Create(aCapacity);
  DoAddAll(e);
end;

constructor TGAbstractHashSet.Create(aLoadFactor: Single);
begin
  FTable := GetTableClass.Create(aLoadFactor);
end;

constructor TGAbstractHashSet.Create(aLoadFactor: Single; constref a: array of T);
begin
  FTable := GetTableClass.Create(aLoadFactor);
  DoAddAll(a);
end;

constructor TGAbstractHashSet.Create(aLoadFactor: Single; e: IEnumerable);
var
  o: TObject;
begin
  o := e._GetRef;
  if o is TSpecSet then
    Create(TSpecSet(o).Count, aLoadFactor)
  else
    Create(aLoadFactor);
  DoAddAll(e);
end;

constructor TGAbstractHashSet.Create(aCapacity: SizeInt; aLoadFactor: Single);
begin
  FTable := GetTableClass.Create(aCapacity, aLoadFactor);
end;

constructor TGAbstractHashSet.Create(aCapacity: SizeInt; aLoadFactor: Single; constref a: array of T);
begin
  FTable := GetTableClass.Create(aCapacity, aLoadFactor);
  DoAddAll(a);
end;

constructor TGAbstractHashSet.Create(aCapacity: SizeInt; aLoadFactor: Single; e: IEnumerable);
begin
  FTable := GetTableClass.Create(aCapacity, aLoadFactor);
  DoAddAll(e);
end;

constructor TGAbstractHashSet.CreateCopy(aSet: TAbstractHashSet);
begin
  if aSet.GetClass = GetClass then
    FTable := aSet.FTable.Clone
  else
    begin
      FTable := GetTableClass.Create(aSet.Count);
      DoAddAll(aSet);
    end;
end;

destructor TGAbstractHashSet.Destroy;
begin
  DoClear;
  FTable.Free;
  inherited;
end;

function TGAbstractHashSet.Contains(constref aValue: T): Boolean;
var
  sr: TSearchResult;
begin
  Result := FTable.Find(aValue, sr) <> nil;
end;

function TGAbstractHashSet.Clone: TAbstractHashSet;
begin
  Result := GetClass.CreateCopy(Self);
end;

{ TGBaseHashSetLP }

class function TGBaseHashSetLP.GetClass: THashSetClass;
begin
  Result := TGBaseHashSetLP;
end;

class function TGBaseHashSetLP.GetTableClass: THashTableClass;
begin
  Result := specialize TGOpenAddrLP<T, TEntry, TEqRel>;
end;

{ TGBaseHashSetLPT }

function TGBaseHashSetLPT.GetTombstonesCount: SizeInt;
begin
  Result :=  THashTableLPT(FTable).TombstonesCount;
end;

class function TGBaseHashSetLPT.GetTableClass: THashTableClass;
begin
  Result := THashTableLPT;
end;

class function TGBaseHashSetLPT.GetClass: THashSetClass;
begin
  Result := TGBaseHashSetLPT;
end;

procedure TGBaseHashSetLPT.ClearTombstones;
begin
  THashTableLPT(FTable).ClearTombstones;
end;

{ TGBaseHashSetQP }

function TGBaseHashSetQP.GetTombstonesCount: SizeInt;
begin
  Result := THashTableQP(FTable).TombstonesCount;
end;

class function TGBaseHashSetQP.GetTableClass: THashTableClass;
begin
  Result := THashTableQP;
end;

class function TGBaseHashSetQP.GetClass: THashSetClass;
begin
  Result := TGBaseHashSetQP;
end;

procedure TGBaseHashSetQP.ClearTombstones;
begin
  THashTableQP(FTable).ClearTombstones;
end;

{ TGBaseOrderedHashSet.TReverseEnumerable }

function TGBaseOrderedHashSet.TReverseEnumerable.GetCurrent: T;
begin
  Result := FEnum.GetCurrent^.Key;
end;

constructor TGBaseOrderedHashSet.TReverseEnumerable.Create(aSet: TGBaseOrderedHashSet);
begin
  inherited Create(aSet);
  FEnum := TOrderedHashTable(aSet.FTable).GetReverseEnumerator;
end;

destructor TGBaseOrderedHashSet.TReverseEnumerable.Destroy;
begin
  FEnum.Free;
  inherited;
end;

function TGBaseOrderedHashSet.TReverseEnumerable.MoveNext: Boolean;
begin
  Result := FEnum.MoveNext;
end;

procedure TGBaseOrderedHashSet.TReverseEnumerable.Reset;
begin
  FEnum.Reset;
end;

{ TGBaseOrderedHashSet }

class function TGBaseOrderedHashSet.GetClass: THashSetClass;
begin
  Result := TGBaseOrderedHashSet;
end;

class function TGBaseOrderedHashSet.GetTableClass: THashTableClass;
begin
  Result := TOrderedHashTable;
end;

function TGBaseOrderedHashSet.Reverse: IEnumerable;
begin
  BeginIteration;
  Result := TReverseEnumerable.Create(Self);
end;

{ TGBaseChainHashSet }

class function TGBaseChainHashSet.GetClass: THashSetClass;
begin
  Result := TGBaseChainHashSet;
end;

class function TGBaseChainHashSet.GetTableClass: THashTableClass;
begin
  Result := specialize TGChainHashTable<T, TEntry, TEqRel>;
end;

{ TGCustomObjectHashSet }

procedure TGCustomObjectHashSet.EntryRemoving(p: PEntry);
begin
  p^.Key.Free;
end;

procedure TGCustomObjectHashSet.DoClear;
var
  e: PEntry;
begin
  if OwnsObjects then
    for e in FTable do
      e^.Key.Free;
  inherited;
end;

function TGCustomObjectHashSet.DoRemove(constref aValue: T): Boolean;
begin
  Result := inherited DoRemove(aValue);
  if Result and OwnsObjects then
    aValue.Free;
end;

function TGCustomObjectHashSet.DoRemoveIf(aTest: TTest): SizeInt;
begin
  if OwnsObjects then
    Result := FTable.RemoveIf(aTest, @EntryRemoving)
  else
    Result := FTable.RemoveIf(aTest);
end;

function TGCustomObjectHashSet.DoRemoveIf(aTest: TOnTest): SizeInt;
begin
  if OwnsObjects then
    Result := FTable.RemoveIf(aTest, @EntryRemoving)
  else
    Result := FTable.RemoveIf(aTest);
end;

function TGCustomObjectHashSet.DoRemoveIf(aTest: TNestTest): SizeInt;
begin
  if OwnsObjects then
    Result := FTable.RemoveIf(aTest, @EntryRemoving)
  else
    Result := FTable.RemoveIf(aTest);
end;

constructor TGCustomObjectHashSet.Create(aOwnsObjects: Boolean);
begin
  inherited Create;
  OwnsObjects := aOwnsObjects;
end;

constructor TGCustomObjectHashSet.Create(constref a: array of T; aOwnsObjects: Boolean);
begin
  inherited Create(a);
  OwnsObjects := aOwnsObjects;
end;

constructor TGCustomObjectHashSet.Create(e: IEnumerable; aOwnsObjects: Boolean);
begin
  inherited Create(e);
  OwnsObjects := aOwnsObjects;
end;

constructor TGCustomObjectHashSet.Create(aCapacity: SizeInt; aOwnsObjects: Boolean);
begin
  inherited Create(aCapacity);
  OwnsObjects := aOwnsObjects;
end;

constructor TGCustomObjectHashSet.Create(aCapacity: SizeInt; constref a: array of T; aOwnsObjects: Boolean);
begin
  inherited Create(aCapacity, a);
  OwnsObjects := aOwnsObjects;
end;

constructor TGCustomObjectHashSet.Create(aCapacity: SizeInt; e: IEnumerable; aOwnsObjects: Boolean);
begin
  inherited Create(aCapacity, e);
  OwnsObjects := aOwnsObjects;
end;

constructor TGCustomObjectHashSet.Create(aLoadFactor: Single; aOwnsObjects: Boolean);
begin
  inherited Create(aLoadFactor);
  OwnsObjects := aOwnsObjects;
end;

constructor TGCustomObjectHashSet.Create(aLoadFactor: Single; constref a: array of T; aOwnsObjects: Boolean);
begin
  inherited Create(aLoadFactor, a);
  OwnsObjects := aOwnsObjects;
end;

constructor TGCustomObjectHashSet.Create(aLoadFactor: Single; e: IEnumerable; aOwnsObjects: Boolean);
begin
  inherited Create(aLoadFactor, e);
  OwnsObjects := aOwnsObjects;
end;

constructor TGCustomObjectHashSet.Create(aCapacity: SizeInt; aLoadFactor: Single; aOwnsObjects: Boolean);
begin
  inherited Create(aCapacity, aLoadFactor);
  OwnsObjects := aOwnsObjects;
end;

constructor TGCustomObjectHashSet.Create(aCapacity: SizeInt; aLoadFactor: Single; constref a: array of T;
  aOwnsObjects: Boolean);
begin
  inherited Create(aCapacity, aLoadFactor, a);
  OwnsObjects := aOwnsObjects;
end;

constructor TGCustomObjectHashSet.Create(aCapacity: SizeInt; aLoadFactor: Single; e: IEnumerable;
  aOwnsObjects: Boolean);
begin
  inherited Create(aCapacity, aLoadFactor, e);
  OwnsObjects := aOwnsObjects;
end;

constructor TGCustomObjectHashSet.CreateCopy(aSet: TGCustomObjectHashSet);
begin
  inherited CreateCopy(aSet);
  OwnsObjects := aSet.OwnsObjects;
end;

{ TGObjectHashSetLP }

class function TGObjectHashSetLP.GetClass: THashSetClass;
begin
  Result := TGObjectHashSetLP;
end;

class function TGObjectHashSetLP.GetTableClass: THashTableClass;
begin
  Result := specialize TGOpenAddrLP<T, TEntry, TEqRel>;
end;

function TGObjectHashSetLP.Clone: TGObjectHashSetLP;
begin
  Result := TGObjectHashSetLP.CreateCopy(Self);
end;

{ TGObjectHashSetLPT }

function TGObjectHashSetLPT.GetTombstonesCount: SizeInt;
begin
  Result := THashTableLPT(FTable).TombstonesCount;
end;

class function TGObjectHashSetLPT.GetClass: THashSetClass;
begin
  Result := TGObjectHashSetLPT;
end;

class function TGObjectHashSetLPT.GetTableClass: THashTableClass;
begin
  Result := THashTableLPT;
end;

function TGObjectHashSetLPT.Clone: TGObjectHashSetLPT;
begin
  Result := TGObjectHashSetLPT.CreateCopy(Self);
end;

procedure TGObjectHashSetLPT.ClearTombstones;
begin
  THashTableLPT(FTable).ClearTombstones;
end;

{ TGObjectHashSetQP }

function TGObjectHashSetQP.GetTombstonesCount: SizeInt;
begin
  Result := THashTableQP(FTable).TombstonesCount;
end;

class function TGObjectHashSetQP.GetClass: THashSetClass;
begin
  Result := TGObjectHashSetQP;
end;

class function TGObjectHashSetQP.GetTableClass: THashTableClass;
begin
  Result := THashTableQP;
end;

function TGObjectHashSetQP.Clone: TGObjectHashSetQP;
begin
  Result := TGObjectHashSetQP.CreateCopy(Self);
end;

procedure TGObjectHashSetQP.ClearTombstones;
begin
  THashTableQP(FTable).ClearTombstones;
end;

{ TGObjectOrderedHashSet }

procedure TGObjectOrderedHashSet.EntryRemoving(p: PEntry);
begin
  p^.Key.Free;
end;

procedure TGObjectOrderedHashSet.DoClear;
var
  e: PEntry;
begin
  if OwnsObjects then
    for e in FTable do
      e^.Key.Free;
  inherited;
end;

function TGObjectOrderedHashSet.DoRemove(constref aValue: T): Boolean;
begin
  Result := inherited DoRemove(aValue);
  if Result and OwnsObjects then
    aValue.Free;
end;

function TGObjectOrderedHashSet.DoRemoveIf(aTest: TTest): SizeInt;
begin
  if OwnsObjects then
    Result := FTable.RemoveIf(aTest, @EntryRemoving)
  else
    Result := FTable.RemoveIf(aTest);
end;

function TGObjectOrderedHashSet.DoRemoveIf(aTest: TOnTest): SizeInt;
begin
  if OwnsObjects then
    Result := FTable.RemoveIf(aTest, @EntryRemoving)
  else
    Result := FTable.RemoveIf(aTest);
end;

function TGObjectOrderedHashSet.DoRemoveIf(aTest: TNestTest): SizeInt;
begin
  if OwnsObjects then
    Result := FTable.RemoveIf(aTest, @EntryRemoving)
  else
    Result := FTable.RemoveIf(aTest);
end;

class function TGObjectOrderedHashSet.GetClass: THashSetClass;
begin
  Result := TGObjectOrderedHashSet;
end;

class function TGObjectOrderedHashSet.GetTableClass: THashTableClass;
begin
  Result := TOrderedHashTable;
end;

constructor TGObjectOrderedHashSet.Create(aOwnsObjects: Boolean);
begin
  inherited Create;
  OwnsObjects := aOwnsObjects;
end;

constructor TGObjectOrderedHashSet.Create(constref a: array of T; aOwnsObjects: Boolean);
begin
  inherited Create(a);
  OwnsObjects := aOwnsObjects;
end;

constructor TGObjectOrderedHashSet.Create(e: IEnumerable; aOwnsObjects: Boolean);
begin
  inherited Create(e);
  OwnsObjects := aOwnsObjects;
end;

constructor TGObjectOrderedHashSet.Create(aCapacity: SizeInt; aOwnsObjects: Boolean);
begin
  inherited Create(aCapacity);
  OwnsObjects := aOwnsObjects;
end;

constructor TGObjectOrderedHashSet.Create(aCapacity: SizeInt; constref a: array of T; aOwnsObjects: Boolean);
begin
  inherited Create(aCapacity, a);
  OwnsObjects := aOwnsObjects;
end;

constructor TGObjectOrderedHashSet.Create(aCapacity: SizeInt; e: IEnumerable; aOwnsObjects: Boolean);
begin
  inherited Create(aCapacity, e);
  OwnsObjects := aOwnsObjects;
end;

constructor TGObjectOrderedHashSet.Create(aLoadFactor: Single; aOwnsObjects: Boolean);
begin
  inherited Create(aLoadFactor);
  OwnsObjects := aOwnsObjects;
end;

constructor TGObjectOrderedHashSet.Create(aLoadFactor: Single; constref a: array of T; aOwnsObjects: Boolean);
begin
  inherited Create(aLoadFactor, a);
  OwnsObjects := aOwnsObjects;
end;

constructor TGObjectOrderedHashSet.Create(aLoadFactor: Single; e: IEnumerable; aOwnsObjects: Boolean);
begin
  inherited Create(aLoadFactor, e);
  OwnsObjects := aOwnsObjects;
end;

constructor TGObjectOrderedHashSet.Create(aCapacity: SizeInt; aLoadFactor: Single; aOwnsObjects: Boolean);
begin
  inherited Create(aCapacity, aLoadFactor);
  OwnsObjects := aOwnsObjects;
end;

constructor TGObjectOrderedHashSet.Create(aCapacity: SizeInt; aLoadFactor: Single; constref a: array of T;
  aOwnsObjects: Boolean);
begin
  inherited Create(aCapacity, aLoadFactor, a);
  OwnsObjects := aOwnsObjects;
end;

constructor TGObjectOrderedHashSet.Create(aCapacity: SizeInt; aLoadFactor: Single; e: IEnumerable;
  aOwnsObjects: Boolean);
begin
  inherited Create(aCapacity, aLoadFactor, e);
  OwnsObjects := aOwnsObjects;
end;

constructor TGObjectOrderedHashSet.CreateCopy(aSet: TGObjectOrderedHashSet);
begin
  inherited CreateCopy(aSet);
  OwnsObjects := aSet.OwnsObjects;
end;

function TGObjectOrderedHashSet.Clone: TGObjectOrderedHashSet;
begin
  Result := TGObjectOrderedHashSet.CreateCopy(Self);
end;

{ TGObjectChainHashSet }

class function TGObjectChainHashSet.GetClass: THashSetClass;
begin
  Result := TGObjectChainHashSet;
end;

class function TGObjectChainHashSet.GetTableClass: THashTableClass;
begin
  Result := specialize TGChainHashTable<T, TEntry, TEqRel>;
end;

function TGObjectChainHashSet.Clone: TGObjectChainHashSet;
begin
  Result := TGObjectChainHashSet.CreateCopy(Self);
end;

{ TGLiteHashSetLP.TEnumerator }

function TGLiteHashSetLP.TEnumerator.GetCurrent: T;
begin
  Result := FEnum.Current^.Key;
end;

procedure TGLiteHashSetLP.TEnumerator.Init(constref aSet: TGLiteHashSetLP);
begin
  FEnum := aSet.FTable.GetEnumerator;
end;

function TGLiteHashSetLP.TEnumerator.MoveNext: Boolean;
begin
  Result := FEnum.MoveNext;
end;

procedure TGLiteHashSetLP.TEnumerator.Reset;
begin
  FEnum.Reset;
end;

{ TGLiteHashSetLP }

function TGLiteHashSetLP.GetCapacity: SizeInt;
begin
  Result := FTable.Capacity;
end;

function TGLiteHashSetLP.GetCount: SizeInt;
begin
  Result := FTable.Count;
end;

function TGLiteHashSetLP.GetFillRatio: Single;
begin
  Result := FTable.FillRatio;
end;

function TGLiteHashSetLP.GetLoadFactor: Single;
begin
  Result := FTable.LoadFactor;
end;

function TGLiteHashSetLP.GetExpandTreshold: SizeInt;
begin
  Result := FTable.ExpandTreshold;
end;

procedure TGLiteHashSetLP.SetLoadFactor(aValue: Single);
begin
  FTable.LoadFactor := aValue;
end;

class operator TGLiteHashSetLP. + (constref L, R: TGLiteHashSetLP): TGLiteHashSetLP;
begin
  if @Result = @L then
    Result.Join(R)
  else
    if @Result = @R then
      Result.Join(L)
    else
      begin
        Result := L;
        Result.Join(R);
      end;
end;

class operator TGLiteHashSetLP. - (constref L, R: TGLiteHashSetLP): TGLiteHashSetLP;
var
  tmp: TGLiteHashSetLP;
  v: T;
begin
  if @Result = @L then
    Result.Subtract(R)
  else
    if @Result = @R then
      begin
        for {%H-}v in L do
          if R{%H-}.NonContains(v) then
            tmp.Add(v);
        Result := tmp;
      end
    else
      for {%H-}v in L do
        if R{%H-}.NonContains(v) then
          Result.Add(v);
end;

class operator TGLiteHashSetLP. * (constref L, R: TGLiteHashSetLP): TGLiteHashSetLP;
begin
  if @Result = @L then
    Result.Intersect(R)
  else
    if @Result = @R then
      Result.Intersect(L)
    else
      begin
        Result := L;
        Result.Intersect(R);
      end;
end;

class operator TGLiteHashSetLP.><(constref L, R: TGLiteHashSetLP): TGLiteHashSetLP;
begin
  if @Result = @L then
    Result.SymmetricSubtract(R)
  else
    if @Result = @R then
      Result.SymmetricSubtract(L)
    else
      begin
        Result := L;
        Result.SymmetricSubtract(R);
      end;
end;

class operator TGLiteHashSetLP. = (constref L, R: TGLiteHashSetLP): Boolean;
begin
  Result := L.IsEqual(R);
end;

class operator TGLiteHashSetLP.<=(constref L, R: TGLiteHashSetLP): Boolean;
begin
  Result := L{%H-}.IsSubset(R);
end;

function TGLiteHashSetLP.DefaultLoadFactor: Single;
begin
  Result := FTable.DEFAULT_LOAD_FACTOR;
end;

function TGLiteHashSetLP.MaxLoadFactor: Single;
begin
  Result := FTable.MAX_LOAD_FACTOR;
end;

function TGLiteHashSetLP.MinLoadFactor: Single;
begin
  Result := FTable.MIN_LOAD_FACTOR;
end;

function TGLiteHashSetLP.GetEnumerator: TEnumerator;
begin
  Result.Init(Self);
end;

function TGLiteHashSetLP.ToArray: TArray;
var
  I: SizeInt = 0;
  p: PEntry;
begin
  System.SetLength(Result, Count);
  for p in FTable do
    begin
      Result[I] := p^.Key;
      Inc(I);
    end;
end;

function TGLiteHashSetLP.IsEmpty: Boolean;
begin
  Result := FTable.Count = 0;
end;

function TGLiteHashSetLP.NonEmpty: Boolean;
begin
  Result := FTable.Count <> 0;
end;

procedure TGLiteHashSetLP.Clear;
begin
  FTable.Clear;
end;

procedure TGLiteHashSetLP.TrimToFit;
begin
  FTable.TrimToFit;
end;

procedure TGLiteHashSetLP.EnsureCapacity(aValue: SizeInt);
begin
  FTable.EnsureCapacity(aValue);
end;

function TGLiteHashSetLP.Add(constref aValue: T): Boolean;
var
  p: PEntry;
  Pos: SizeInt;
begin
  Result := not FTable.FindOrAdd(aValue, p, Pos);
  if Result then
    p^.Key := aValue;
end;

function TGLiteHashSetLP.AddAll(constref a: array of T): SizeInt;
var
  v: T;
begin
  Result := 0;
  for v in a do
    Result += Ord(Add(v));
end;

function TGLiteHashSetLP.AddAll(e: IEnumerable): SizeInt;
var
  v: T;
begin
  Result := 0;
  for v in e do
    Result += Ord(Add(v));
end;

function TGLiteHashSetLP.AddAll(constref aSet: TGLiteHashSetLP): SizeInt;
var
  v: T;
begin
  if @aSet <> @Self then
    begin
      Result := 0;
      for {%H-}v in aSet do
        Result += Ord(Add(v));
    end
  else
    Result := 0;
end;

function TGLiteHashSetLP.Contains(constref aValue: T): Boolean;
var
  p: SizeInt;
begin
  Result := FTable.Find(aValue, p) <> nil;
end;

function TGLiteHashSetLP.NonContains(constref aValue: T): Boolean;
begin
  Result := not Contains(aValue);
end;

function TGLiteHashSetLP.ContainsAny(constref a: array of T): Boolean;
var
  v: T;
begin
  for v in a do
    if Contains(v) then
      exit(True);
  Result := False;
end;

function TGLiteHashSetLP.ContainsAny(e: IEnumerable): Boolean;
var
  v: T;
begin
  for v in e do
    if Contains(v) then
      exit(True);
  Result := False;
end;

function TGLiteHashSetLP.ContainsAny(constref aSet: TGLiteHashSetLP): Boolean;
var
  v: T;
begin
  if @aSet = @Self then
    exit(True);
  for {%H-}v in aSet do
    if Contains(v) then
      exit(True);
  Result := False;
end;

function TGLiteHashSetLP.ContainsAll(constref a: array of T): Boolean;
var
  v: T;
begin
  for v in a do
    if NonContains(v) then
      exit(False);
  Result := True;
end;

function TGLiteHashSetLP.ContainsAll(e: IEnumerable): Boolean;
var
  v: T;
begin
  for v in e do
    if NonContains(v) then
      exit(False);
  Result := True;
end;

function TGLiteHashSetLP.ContainsAll(constref aSet: TGLiteHashSetLP): Boolean;
var
  v: T;
begin
  if @aSet = @Self then
    exit(True);
  for {%H-}v in aSet do
    if NonContains(v) then
      exit(False);
  Result := True;
end;

function TGLiteHashSetLP.Remove(constref aValue: T): Boolean;
begin
  Result := FTable.Remove(aValue);
end;

function TGLiteHashSetLP.RemoveAll(constref a: array of T): SizeInt;
var
  v: T;
begin
  Result := 0;
  for v in a do
    Result += Ord(Remove(v));
end;

function TGLiteHashSetLP.RemoveAll(e: IEnumerable): SizeInt;
var
  v: T;
begin
  Result := 0;
  for v in e do
    Result += Ord(Remove(v));
end;

function TGLiteHashSetLP.RemoveAll(constref aSet: TGLiteHashSetLP): SizeInt;
var
  v: T;
begin
  if @aSet <> @Self then
    begin
      Result := 0;
      for {%H-}v in aSet do
        Result += Ord(Remove(v));
    end
  else
    begin
      Result := Count;
      Clear;
    end;
end;

function TGLiteHashSetLP.RemoveIf(aTest: TTest): SizeInt;
begin
  Result := 0;
  with FTable{%H-}.GetRemovableEnumerator do
    while MoveNext do
      if aTest(Current^.Key) then
        begin
          RemoveCurrent;
          Inc(Result);
        end;
end;

function TGLiteHashSetLP.RemoveIf(aTest: TOnTest): SizeInt;
begin
  Result := 0;
  with FTable{%H-}.GetRemovableEnumerator do
    while MoveNext do
      if aTest(Current^.Key) then
        begin
          RemoveCurrent;
          Inc(Result);
        end;
end;

function TGLiteHashSetLP.RemoveIf(aTest: TNestTest): SizeInt;
begin
  Result := 0;
  with FTable{%H-}.GetRemovableEnumerator do
    while MoveNext do
      if aTest(Current^.Key) then
        begin
          RemoveCurrent;
          Inc(Result);
        end;
end;

function TGLiteHashSetLP.Extract(constref aValue: T): Boolean;
begin
  Result := FTable.Remove(aValue);
end;

function TGLiteHashSetLP.ExtractIf(aTest: TTest): TArray;
var
  I: SizeInt = 0;
begin
  System.SetLength(Result, ARRAY_INITIAL_SIZE);
  with FTable{%H-}.GetRemovableEnumerator do
    while MoveNext do
      if aTest(Current^.Key) then
        begin
          if I = System.Length(Result) then
            System.SetLength(Result, I shl 1);
          Result[I] := Current^.Key;
          RemoveCurrent;
          Inc(I);
        end;
  System.SetLength(Result, I);
end;

function TGLiteHashSetLP.ExtractIf(aTest: TOnTest): TArray;
var
  I: SizeInt = 0;
begin
  System.SetLength(Result, ARRAY_INITIAL_SIZE);
  with FTable{%H-}.GetRemovableEnumerator do
    while MoveNext do
      if aTest(Current^.Key) then
        begin
          if I = System.Length(Result) then
            System.SetLength(Result, I shl 1);
          Result[I] := Current^.Key;
          RemoveCurrent;
          Inc(I);
        end;
  System.SetLength(Result, I);
end;

function TGLiteHashSetLP.ExtractIf(aTest: TNestTest): TArray;
var
  I: SizeInt = 0;
begin
  System.SetLength(Result, ARRAY_INITIAL_SIZE);
  with FTable{%H-}.GetRemovableEnumerator do
    while MoveNext do
      if aTest(Current^.Key) then
        begin
          if I = System.Length(Result) then
            System.SetLength(Result, I shl 1);
          Result[I] := Current^.Key;
          RemoveCurrent;
          Inc(I);
        end;
  System.SetLength(Result, I);
end;

procedure TGLiteHashSetLP.RetainAll(aCollection: ICollection);
begin
  with FTable{%H-}.GetRemovableEnumerator do
    while MoveNext do
      if aCollection.NonContains(Current^.Key) then
        RemoveCurrent;
end;

procedure TGLiteHashSetLP.RetainAll(constref aSet: TGLiteHashSetLP);
begin
  if @aSet <> @Self then
    with FTable{%H-}.GetRemovableEnumerator do
      while MoveNext do
        if aSet.NonContains(Current^.Key) then
          RemoveCurrent;
end;

function TGLiteHashSetLP.IsSuperset(constref aSet: TGLiteHashSetLP): Boolean;
begin
  Result := ContainsAll(aSet);
end;

function TGLiteHashSetLP.IsSubset(constref aSet: TGLiteHashSetLP): Boolean;
begin
  Result := aSet.ContainsAll(Self);
end;

function TGLiteHashSetLP.IsEqual(constref aSet: TGLiteHashSetLP): Boolean;
var
  v: T;
begin
  if @aSet <> @Self then
    begin
      if Count <> aSet.Count then
        exit(False);
      for {%H-}v in aSet do
        if NonContains(v) then
          exit(False);
      Result := True;
    end
  else
    Result := True;
end;

function TGLiteHashSetLP.Intersecting(constref aSet: TGLiteHashSetLP): Boolean;
var
  v: T;
begin
  if @aSet <> @Self then
    begin
      for {%H-}v in aSet do
        if Contains(v) then
          exit(True);
      Result := False;
    end
  else
    Result := True;
end;

procedure TGLiteHashSetLP.Intersect(constref aSet: TGLiteHashSetLP);
begin
  RetainAll(aSet);
end;

procedure TGLiteHashSetLP.Join(constref aSet: TGLiteHashSetLP);
var
  v: T;
begin
  if @aSet <> @Self then
    for {%H-}v in aSet do
      Add(v);
end;

procedure TGLiteHashSetLP.Subtract(constref aSet: TGLiteHashSetLP);
var
  v: T;
begin
  if @aSet <> @Self then
    for {%H-}v in aSet do
      Remove(v)
  else
    Clear;
end;

procedure TGLiteHashSetLP.SymmetricSubtract(constref aSet: TGLiteHashSetLP);
var
  v: T;
begin
  if @aSet <> @Self then
    begin
      for {%H-}v in aSet do
        if not Remove(v) then
          Add(v)
    end
  else
    Clear;
end;

{ TGDisjointSetUnion.TEnumerator }

function TGDisjointSetUnion.TEnumerator.GetCurrent: T;
begin
  Result := FList[FCurrIndex].Data.Key;
end;

function TGDisjointSetUnion.TEnumerator.MoveNext: Boolean;
begin
  Result := FCurrIndex < FLastIndex;
  FCurrIndex += Ord(Result);
end;

procedure TGDisjointSetUnion.TEnumerator.Reset;
begin
  FCurrIndex := -1;
end;

{ TGDisjointSetUnion }

function TGDisjointSetUnion.GetCapacity: SizeInt;
begin
  Result := FTable.Capacity;
end;

function TGDisjointSetUnion.GetCount: SizeInt;
begin
  Result := FTable.Count;
end;

function TGDisjointSetUnion.FindOrAdd(constref aValue: T): SizeInt;
var
  OldCapacity: SizeInt;
  e: PEntry;
begin
  OldCapacity := Capacity;
  if not FTable.FindOrAdd(aValue, e, Result) then
    begin
      e^.Key := aValue;
      if Capacity > OldCapacity then
        ExpandDsu;
    end;
end;

function TGDisjointSetUnion.GetEnumerator: TEnumerator;
begin
  Result.FList := FTable.NodeList;
  Result.FLastIndex := Pred(FTable.Count);
  Result.FCurrIndex := -1;
end;

function TGDisjointSetUnion.ToArray: TArray;
var
  v: T;
  I: SizeInt = 0;
begin
  System.SetLength(Result, Count);
  for v in Self do
    begin
      Result[I] := v;
      Inc(I);
    end;
end;

function TGDisjointSetUnion.IsEmpty: Boolean;
begin
  Result := FTable.Count = 0;
end;

function TGDisjointSetUnion.NonEmpty: Boolean;
begin
  Result := FTable.Count <> 0;
end;

procedure TGDisjointSetUnion.Clear;
begin
  FTable.Clear;
  FDsu := nil;
end;

procedure TGDisjointSetUnion.ExpandDsu;
var
  I, NewCapacity: SizeInt;
begin
  I := System.Length(FDsu);
  NewCapacity := Capacity;
  System.SetLength(FDsu, NewCapacity);
  for I := I to Pred(NewCapacity) do
    FDsu[I] := I;
end;

function TGDisjointSetUnion.GetItem(aIndex: SizeInt): T;
begin
  if SizeUInt(aIndex) < SizeUInt(Count) then
    Result := FTable.NodeList[aIndex].Data.Key
  else
    raise ELGListError.CreateFmt(SEIndexOutOfBoundsFmt, [aIndex]);
end;

function TGDisjointSetUnion.GetTag(aValue: SizeInt): SizeInt;
begin
  if FDsu[aValue] = aValue then
    exit(aValue);
  Result := GetTag(FDsu[aValue]);
  FDsu[aValue] := Result;
end;

procedure TGDisjointSetUnion.EnsureCapacity(aValue: SizeInt);
var
  OldCapacity: SizeInt;
begin
  OldCapacity := Capacity;
  FTable.EnsureCapacity(aValue);
  if Capacity > OldCapacity then
    ExpandDsu;
end;

function TGDisjointSetUnion.IndexOf(constref aValue: T): SizeInt;
begin
  FTable.Find(aValue, Result);
end;

function TGDisjointSetUnion.Contains(constref aValue: T): Boolean;
begin
  Result := IndexOf(aValue) >= 0;
end;

function TGDisjointSetUnion.NonContains(constref aValue: T): Boolean;
begin
  Result := IndexOf(aValue) < 0;
end;

function TGDisjointSetUnion.Add(constref aValue: T): SizeInt;
var
  OldCapacity: SizeInt;
  e: PEntry;
begin
  OldCapacity := Capacity;
  if not FTable.FindOrAdd(aValue, e, Result) then
    begin
      e^.Key := aValue;
      if Capacity > OldCapacity then
        ExpandDsu;
    end
  else
    Result := -1;
end;

procedure TGDisjointSetUnion.Reset;
var
  I: SizeInt;
begin
  for I := 0 to System.High(FDsu) do
    FDsu[I] := I;
end;

function TGDisjointSetUnion.Tag(constref aValue: T): SizeInt;
var
  I: SizeInt;
begin
  I := FindOrAdd(aValue);
  Result := GetTag(I);
end;

function TGDisjointSetUnion.TagI(aIndex: SizeInt): SizeInt;
begin
  if SizeUInt(aIndex) < SizeUInt(Count) then
    Result := GetTag(aIndex)
  else
    raise ELGListError.CreateFmt(SEIndexOutOfBoundsFmt, [aIndex]);
end;

function TGDisjointSetUnion.InSameSet(constref L, R: T): Boolean;
var
  I, J: SizeInt;
begin
  I := FindOrAdd(L);
  J := FindOrAdd(R);
  Result := GetTag(I) = GetTag(J);
end;

function TGDisjointSetUnion.InSameSetI(L, R: SizeInt): Boolean;
begin
  Result := TagI(L) = TagI(R);
end;

function TGDisjointSetUnion.InDiffSets(constref L, R: T): Boolean;
var
  I, J: SizeInt;
begin
  I := FindOrAdd(L);
  J := FindOrAdd(R);
  Result := GetTag(I) <> GetTag(J);
end;

function TGDisjointSetUnion.InDiffSetsI(L, R: SizeInt): Boolean;
begin
  Result := TagI(L) <> TagI(R);
end;

function TGDisjointSetUnion.Join(constref L, R: T): Boolean;
var
  I, J: SizeInt;
begin
  I := GetTag(FindOrAdd(L));
  J := GetTag(FindOrAdd(R));
  if I = J then
    exit(False);
  if NextRandomBoolean then
    FDsu[I] := J
  else
    FDsu[J] := I;
  Result := True;
end;

function TGDisjointSetUnion.JoinI(L, R: SizeInt): Boolean;
begin
  L := TagI(L);
  R := TagI(R);
  if L = R then
    exit(False);
  if NextRandomBoolean then
    FDsu[L] := R
  else
    FDsu[R] := L;
  Result := True;
end;

end.

