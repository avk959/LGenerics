{****************************************************************************
*                                                                           *
*   This file is part of the LGenerics package.                             *
*   Generic hashset implementations.                                        *
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
unit LGHashSet;

{$MODE OBJFPC}{$H+}
{$INLINE ON}
{$MODESWITCH NESTEDPROCVARS}
{$MODESWITCH ADVANCEDRECORDS}

interface

uses

  SysUtils,
  LGUtils,
  {%H-}LGHelpers,
  LGCustomContainer,
  LGHashTable;

type

  {TGCustomHashSet: common abstract ancestor hashset class }
  generic TGCustomHashSet<T> = class abstract(specialize TGCustomSet<T>)
  public
  type
    TCustomHashSet = specialize TGCustomHashSet<T>;

  protected
  type
    THashTable           = specialize TGCustomHashTable<T, TEntry>;
    THashTableClass      = class of THashTable;
    THashSetClass        = class of TCustomHashSet;
    TSearchResult        = THashTable.TSearchResult;

    TEnumerator = class(TContainerEnumerator)
    private
      FEnum: THashTable.TEntryEnumerator;
    protected
      function  GetCurrent: T; override;
    public
      constructor Create(hs: TCustomHashSet);
      destructor Destroy; override;
      function  MoveNext: Boolean; override;
      procedure Reset; override;
    end;

    TDistinctEnumerable = class(specialize TGEnumCursor<T>)
    protected
      FSet: TCustomHashSet;
    public
      constructor Create(e: TCustomEnumerator; aSetClass: THashSetClass);
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
    function  DoGetEnumerator: TCustomEnumerator; override;
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
    constructor CreateCopy(aSet: TCustomHashSet);
    destructor Destroy; override;
    function  Contains(constref aValue: T): Boolean; override;
    function  Clone: TCustomHashSet; override;
    property  LoadFactor: Single read GetLoadFactor write SetLoadFactor;
    property  FillRatio: Single read GetFillRatio;
  { The number of elements that can be written without rehashing }
    property  ExpandTreshold: SizeInt read GetExpandTreshold;
  end;

  { TGBaseHashSetLP implements open addressing hashset with linear probing;
      functor TEqRel(equality relation) must provide:
        class function HashCode([const[ref]] aValue: T): SizeInt;
        class function Equal([const[ref]] L, R: T): Boolean; }
  generic TGBaseHashSetLP<T, TEqRel> = class(specialize TGCustomHashSet<T>)
  protected
    class function GetClass: THashSetClass; override;
    class function GetTableClass: THashTableClass; override;
  end;

  { TGHashSetLP implements open addressing hashset with linear probing;
    it assumes that type T implements TEqRel }
  generic TGHashSetLP<T> = class(specialize TGBaseHashSetLP<T, T>);

  { TGBaseHashSetLPT implements open addressing hashset with linear probing and lazy deletion }
  generic TGBaseHashSetLPT<T, TEqRel> = class(specialize TGCustomHashSet<T>)
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
  generic TGBaseHashSetQP<T, TEqRel> = class(specialize TGCustomHashSet<T>)
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
  generic TGBaseOrderedHashSet<T, TEqRel> = class(specialize TGCustomHashSet<T>)
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
  generic TGBaseChainHashSet<T, TEqRel> = class(specialize TGCustomHashSet<T>)
  protected
    class function GetClass: THashSetClass; override;
    class function GetTableClass: THashTableClass; override;
  end;

  { TGChainHashSet implements node based hashset with singly linked list chains;
    it assumes that type T implements TEqRel }
  generic TGChainHashSet<T> = class(specialize TGBaseChainHashSet<T, T>);

  { TGCustomObjectHashSet }

  generic TGCustomObjectHashSet<T: class> = class abstract(specialize TGCustomHashSet<T>)
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

  { TGObjectHashSetLP }

  generic TGObjectHashSetLP<T: class; TEqRel> = class(specialize TGCustomObjectHashSet<T>)
  protected
    class function GetClass: THashSetClass; override;
    class function GetTableClass: THashTableClass; override;
  public
    function  Clone: TGObjectHashSetLP; override;
  end;

 { TGObjHashSetLP assumes that type T implements TEqRel }
  generic TGObjHashSetLP<T: class> = class(specialize TGObjectHashSetLP<T, T>);

  { TGObjectHashSetLPT }

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

  { TGObjectHashSetQP }

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

  { TGObjectOrderedHashSet }

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

  { TGObjectChainHashSet }

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
    class operator in(constref aValue: T; constref aSet: TGLiteHashSetLP): Boolean; inline;
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
    function  Contains(constref aValue: T): Boolean; inline;
    function  NonContains(constref aValue: T): Boolean; inline;
    function  ContainsAny(constref a: array of T): Boolean;
    function  ContainsAny(e: IEnumerable): Boolean;
    function  ContainsAll(constref a: array of T): Boolean;
    function  ContainsAll(e: IEnumerable): Boolean;
  { returns True if element removed }
    function  Remove(constref aValue: T): Boolean; inline;
  { returns count of removed elements }
    function  RemoveAll(constref a: array of T): SizeInt;
    function  RemoveAll(e: IEnumerable): SizeInt;
  { returns count of removed elements }
    function  RemoveIf(aTest: TTest): SizeInt;
    function  RemoveIf(aTest: TOnTest): SizeInt;
    function  RemoveIf(aTest: TNestTest): SizeInt;
  { returns True if element extracted }
    function  Extract(constref aValue: T): Boolean; inline;
    function  ExtractIf(aTest: TTest): TArray;
    function  ExtractIf(aTest: TOnTest): TArray;
    function  ExtractIf(aTest: TNestTest): TArray;
  { will contain only those elements that are simultaneously contained in self and aCollection }
    procedure RetainAll(aCollection: ICollection);
    function  IsSuperset(constref aSet: TGLiteHashSetLP): Boolean;
    function  IsSubset(constref aSet: TGLiteHashSetLP): Boolean; inline;
    function  IsEqual(constref aSet: TGLiteHashSetLP): Boolean;
    function  Intersecting(constref aSet: TGLiteHashSetLP): Boolean; inline;
    procedure Intersect(constref aSet: TGLiteHashSetLP);
    procedure Join(constref aSet: TGLiteHashSetLP);
    procedure Subtract(constref aSet: TGLiteHashSetLP);
    procedure SymmetricSubtract(constref aSet: TGLiteHashSetLP);
    property  Count: SizeInt read GetCount;
    property  Capacity: SizeInt read GetCapacity;
    property  LoadFactor: Single read GetLoadFactor write SetLoadFactor;
    property  FillRatio: Single read GetFillRatio;
    property  ExpandTreshold: SizeInt read GetExpandTreshold;
  end;

implementation
{$B-}{$COPERATORS ON}

{ TGCustomHashSet.TEnumerator }

function TGCustomHashSet.TEnumerator.GetCurrent: T;
begin
  Result := FEnum.GetCurrent^.Key;
end;

constructor TGCustomHashSet.TEnumerator.Create(hs: TCustomHashSet);
begin
  inherited Create(hs);
  FEnum := hs.FTable.GetEnumerator;
end;

destructor TGCustomHashSet.TEnumerator.Destroy;
begin
  FEnum.Free;
  inherited;
end;

function TGCustomHashSet.TEnumerator.MoveNext: Boolean;
begin
  Result := FEnum.MoveNext;
end;

procedure TGCustomHashSet.TEnumerator.Reset;
begin
  FEnum.Reset;
end;

{ TGCustomHashSet.TDistinctEnumerable }

constructor TGCustomHashSet.TDistinctEnumerable.Create(e: TCustomEnumerator; aSetClass: THashSetClass);
begin
  inherited Create(e);
  FSet := aSetClass.Create;
end;

destructor TGCustomHashSet.TDistinctEnumerable.Destroy;
begin
  FSet.Free;
  inherited;
end;

function TGCustomHashSet.TDistinctEnumerable.MoveNext: Boolean;
begin
  repeat
    if not inherited MoveNext then
      exit(False);
    Result := FSet.Add(Current);
  until Result;
end;

procedure TGCustomHashSet.TDistinctEnumerable.Reset;
begin
  inherited;
  FSet.Clear;
end;

{ TGCustomHashSet }

function TGCustomHashSet.GetExpandTreshold: SizeInt;
begin
  Result := FTable.ExpandTreshold;
end;

function TGCustomHashSet.GetCount: SizeInt;
begin
  Result := FTable.Count;
end;

function TGCustomHashSet.GetCapacity: SizeInt;
begin
  Result := FTable.Capacity;
end;

function TGCustomHashSet.GetFillRatio: Single;
begin
  Result := FTable.FillRatio;
end;

function TGCustomHashSet.GetLoadFactor: Single;
begin
  Result := FTable.LoadFactor;
end;

procedure TGCustomHashSet.SetLoadFactor(aValue: Single);
begin
  FTable.LoadFactor := aValue;
end;

function TGCustomHashSet.DoGetEnumerator: TCustomEnumerator;
begin
  Result := TEnumerator.Create(Self);
end;

procedure TGCustomHashSet.DoClear;
begin
  FTable.Clear;
end;

procedure TGCustomHashSet.DoTrimToFit;
begin
  FTable.TrimToFit;
end;

procedure TGCustomHashSet.DoEnsureCapacity(aValue: SizeInt);
begin
  FTable.EnsureCapacity(aValue);
end;

function TGCustomHashSet.DoAdd(constref aValue: T): Boolean;
var
  p: PEntry;
  sr: TSearchResult;
begin
  Result := not FTable.FindOrAdd(aValue, p, sr);
  if Result then
    p^.Key := aValue;
end;

function TGCustomHashSet.DoExtract(constref aValue: T): Boolean;
begin
  Result := FTable.Remove(aValue);
end;

function TGCustomHashSet.DoRemoveIf(aTest: TTest): SizeInt;
begin
  Result := FTable.RemoveIf(aTest);
end;

function TGCustomHashSet.DoRemoveIf(aTest: TOnTest): SizeInt;
begin
  Result := FTable.RemoveIf(aTest);
end;

function TGCustomHashSet.DoRemoveIf(aTest: TNestTest): SizeInt;
begin
  Result := FTable.RemoveIf(aTest);
end;

function TGCustomHashSet.DoExtractIf(aTest: TTest): TArray;
var
  e: TExtractHelper;
begin
  e.Init;
  FTable.RemoveIf(aTest, @e.OnExtract);
  Result := e.Final;
end;

function TGCustomHashSet.DoExtractIf(aTest: TOnTest): TArray;
var
  e: TExtractHelper;
begin
  e.Init;
  FTable.RemoveIf(aTest, @e.OnExtract);
  Result := e.Final;
end;

function TGCustomHashSet.DoExtractIf(aTest: TNestTest): TArray;
var
  e: TExtractHelper;
begin
  e.Init;
  FTable.RemoveIf(aTest, @e.OnExtract);
  Result := e.Final;
end;

class function TGCustomHashSet.DefaultLoadFactor: Single;
begin
  Result := GetTableClass.DefaultLoadFactor;
end;

class function TGCustomHashSet.MaxLoadFactor: Single;
begin
  Result := GetTableClass.MaxLoadFactor;
end;

class function TGCustomHashSet.MinLoadFactor: Single;
begin
  Result := GetTableClass.MinLoadFactor;
end;

class function TGCustomHashSet.Distinct(constref a: TArray): IEnumerable;
begin
  Result := TDistinctEnumerable.Create(specialize TGArrayEnumerator<T>.Create(a), GetClass);
end;

class function TGCustomHashSet.Distinct(e: IEnumerable): IEnumerable;
begin
  Result := TDistinctEnumerable.Create(e.GetEnumerator, GetClass);
end;

constructor TGCustomHashSet.Create;
begin
  FTable := GetTableClass.Create;
end;

constructor TGCustomHashSet.Create(constref a: array of T);
begin
  FTable := GetTableClass.Create;
  DoAddAll(a);
end;

constructor TGCustomHashSet.Create(e: IEnumerable);
var
  o: TObject;
begin
  o := e._GetRef;
  if o is TCustomHashSet then
    CreateCopy(TCustomHashSet(o))
  else
    begin
      if o is TCustomSet then
        Create(TCustomSet(o).Count)
      else
        Create;
      DoAddAll(e);
    end;
end;

constructor TGCustomHashSet.Create(aCapacity: SizeInt);
begin
  FTable := GetTableClass.Create(aCapacity);
end;

constructor TGCustomHashSet.Create(aCapacity: SizeInt; constref a: array of T);
begin
  FTable := GetTableClass.Create(aCapacity);
  DoAddAll(a);
end;

constructor TGCustomHashSet.Create(aCapacity: SizeInt; e: IEnumerable);
begin
  FTable := GetTableClass.Create(aCapacity);
  DoAddAll(e);
end;

constructor TGCustomHashSet.Create(aLoadFactor: Single);
begin
  FTable := GetTableClass.Create(aLoadFactor);
end;

constructor TGCustomHashSet.Create(aLoadFactor: Single; constref a: array of T);
begin
  FTable := GetTableClass.Create(aLoadFactor);
  DoAddAll(a);
end;

constructor TGCustomHashSet.Create(aLoadFactor: Single; e: IEnumerable);
var
  o: TObject;
begin
  o := e._GetRef;
  if o is TCustomSet then
    Create(TCustomSet(o).Count, aLoadFactor)
  else
    Create(aLoadFactor);
  DoAddAll(e);
end;

constructor TGCustomHashSet.Create(aCapacity: SizeInt; aLoadFactor: Single);
begin
  FTable := GetTableClass.Create(aCapacity, aLoadFactor);
end;

constructor TGCustomHashSet.Create(aCapacity: SizeInt; aLoadFactor: Single; constref a: array of T);
begin
  FTable := GetTableClass.Create(aCapacity, aLoadFactor);
  DoAddAll(a);
end;

constructor TGCustomHashSet.Create(aCapacity: SizeInt; aLoadFactor: Single; e: IEnumerable);
begin
  FTable := GetTableClass.Create(aCapacity, aLoadFactor);
  DoAddAll(e);
end;

constructor TGCustomHashSet.CreateCopy(aSet: TCustomHashSet);
begin
  if aSet.GetClass = GetClass then
    FTable := aSet.FTable.Clone
  else
    begin
      FTable := GetTableClass.Create(aSet.Count);
      DoAddAll(aSet);
    end;
end;

destructor TGCustomHashSet.Destroy;
begin
  DoClear;
  FTable.Free;
  inherited;
end;

function TGCustomHashSet.Contains(constref aValue: T): Boolean;
var
  sr: TSearchResult;
begin
  Result := FTable.Find(aValue, sr) <> nil;
end;

function TGCustomHashSet.Clone: TCustomHashSet;
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
  Tmp: TGLiteHashSetLP;
begin
  if @Result = @L then
    Result.Subtract(R)
  else
    if @Result = @R then
      begin
        Tmp := R;
        Result := L;
        Result.Subtract(Tmp)
      end
    else
      begin
        Result := L;
        Result.Subtract(R);
      end;
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
  Result := L.IsSubset(R);
end;

class operator TGLiteHashSetLP.in(constref aValue: T; constref aSet: TGLiteHashSetLP): Boolean;
begin
  Result := aSet.Contains(aValue);
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
function TGLiteHashSetLP.RemoveIf(aTest: TTest): SizeInt;
begin
  Result := 0;
  with FTable.GetRemovableEnumerator do
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
  with FTable.GetRemovableEnumerator do
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
  with FTable.GetRemovableEnumerator do
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
  with FTable.GetRemovableEnumerator do
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
  with FTable.GetRemovableEnumerator do
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
  with FTable.GetRemovableEnumerator do
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
  with FTable.GetRemovableEnumerator do
    while MoveNext do
      if aCollection.NonContains(Current^.Key) then
        RemoveCurrent;
end;

function TGLiteHashSetLP.IsSuperset(constref aSet: TGLiteHashSetLP): Boolean;
var
  v: T;
begin
  if @aSet <> @Self then
    begin
      if Count >= aSet.Count then
        begin
          for v in aSet do
            if NonContains(v) then
              exit(False);
          Result := True;
        end
      else
        Result := False;
    end
  else
    Result := True;
end;

function TGLiteHashSetLP.IsSubset(constref aSet: TGLiteHashSetLP): Boolean;
begin
  Result := aSet.IsSuperset(Self);
end;

function TGLiteHashSetLP.IsEqual(constref aSet: TGLiteHashSetLP): Boolean;
var
  v: T;
begin
  if @aSet <> @Self then
    begin
      if Count <> aSet.Count then
        exit(False);
      for v in aSet do
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
      for v in aSet do
        if Contains(v) then
          exit(True);
      Result := False;
    end
  else
    Result := True;
end;

procedure TGLiteHashSetLP.Intersect(constref aSet: TGLiteHashSetLP);
begin
  with FTable.GetRemovableEnumerator do
    while MoveNext do
      if aSet.NonContains(Current^.Key) then
        RemoveCurrent;
end;

procedure TGLiteHashSetLP.Join(constref aSet: TGLiteHashSetLP);
var
  v: T;
begin
  for v in aSet do
    Add(v);
end;

procedure TGLiteHashSetLP.Subtract(constref aSet: TGLiteHashSetLP);
var
  v: T;
begin
  for v in aSet do
    Remove(v);
end;

procedure TGLiteHashSetLP.SymmetricSubtract(constref aSet: TGLiteHashSetLP);
var
  v: T;
begin
  for v in aSet do
    if not Remove(v) then
      Add(v);
end;

end.

