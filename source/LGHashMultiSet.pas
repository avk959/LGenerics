{****************************************************************************
*                                                                           *
*   This file is part of the LGenerics package.                             *
*   Generic hash multiset implementations.                                  *
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
unit LGHashMultiSet;

{$mode objfpc}{$H+}
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

  { TGCustomHashMultiSet: common hash multiset abstract ancestor class }
  generic TGCustomHashMultiSet<T> = class abstract(specialize TGCustomMultiSet<T>)
  public
  type
    TCustomHashMultiSet = specialize TGCustomHashMultiSet<T>;

  protected
  type
    THashTable          = specialize TGCustomHashTable<T, TEntry>;
    THashTableClass     = class of THashTable;
    THashMultiSetClass  = class of TCustomHashMultiSet;
    TSearchResult       = THashTable.TSearchResult;

    TEnumerator = class(TContainerEnumerator)
    protected
      FEnum: THashTable.TEntryEnumerator;
      FCurrKeyCount: SizeInt;
      function  GetCurrent: T; override;
    public
      constructor Create(ms: TCustomHashMultiSet);
      destructor Destroy; override;
      function  MoveNext: Boolean; override;
      procedure Reset; override;
    end;

    TDistinctEnumerable = class(TContainerEnumerable)
    protected
      FEnum: THashTable.TEntryEnumerator;
      function  GetCurrent: T; override;
    public
      constructor Create(aSet: TCustomHashMultiSet);
      destructor Destroy; override;
      function  MoveNext: Boolean; override;
      procedure Reset; override;
    end;

    TEntryEnumerable = class(specialize TGAutoEnumerable<TEntry>)
    protected
      FOwner: TCustomHashMultiSet;
      FEnum: THashTable.TEntryEnumerator;
      function  GetCurrent: TEntry; override;
    public
      constructor Create(aSet: TCustomHashMultiSet);
      destructor Destroy; override;
      function  MoveNext: Boolean; override;
      procedure Reset; override;
    end;

  var
    FTable: THashTable;
    procedure EntryRemoved(p: PEntry);
    function  GetFillRatio: Single; inline;
    function  GetLoadFactor: Single; inline;
    procedure SetLoadFactor(aValue: Single); inline;
    function  GetExpandTreshold: SizeInt; inline;
    function  GetCapacity: SizeInt; override;
    function  DoGetEnumerator: TCustomEnumerator; override;
    procedure DoClear; override;
    procedure DoTrimToFit; override;
    procedure DoEnsureCapacity(aValue: SizeInt); override;
    function  FindEntry(constref aKey: T): PEntry; override;
    function  FindOrAdd(constref aKey: T; out p: PEntry): Boolean; override;
    function  DoSubEntry(constref e: TEntry): Boolean; override;
    function  DoSymmSubEntry(constref e: TEntry): Boolean; override;
    function  DoExtract(constref aKey: T): Boolean; override;
    function  GetEntryCount: SizeInt; override;
    function  DoDoubleEntryCounters: SizeInt; override;
    function  GetDistinct: IEnumerable; override;  // distinct keys
    function  GetEntries: IEntryEnumerable; override;
    procedure DoIntersect(aSet: TCustomMultiSet); override;
    function  DoRemoveIf(aTest: TTest): SizeInt; override;
    function  DoRemoveIf(aTest: TOnTest): SizeInt; override;
    function  DoRemoveIf(aTest: TNestTest): SizeInt; override;
    function  DoExtractIf(aTest: TTest): TArray; override;
    function  DoExtractIf(aTest: TOnTest): TArray; override;
    function  DoExtractIf(aTest: TNestTest): TArray; override;
    class function GetTableClass: THashTableClass; virtual; abstract;
    class function GetClass: THashMultiSetClass; virtual; abstract;
  public
    class function DefaultLoadFactor: Single; inline;
    class function MaxLoadFactor: Single; inline;
    class function MinLoadFactor: Single; inline;
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
    constructor CreateCopy(aMultiSet: TCustomHashMultiSet);
    destructor Destroy; override;
    function  Clone: TCustomHashMultiSet; override;
    property  LoadFactor: Single read GetLoadFactor write SetLoadFactor;
    property  FillRatio: Single read GetFillRatio;
  { The number of entries that can be written without rehashing }
    property  ExpandTreshold: SizeInt read GetExpandTreshold;
  end;

  { TGBaseHashMultiSetLP implements open addressing hash multiset with linear probing;
      functor TEqRel(equality relation) must provide:
        class function HashCode([const[ref]] aValue: T): SizeInt;
        class function Equal([const[ref]] L, R: T): Boolean; }
  generic TGBaseHashMultiSetLP<T, TEqRel> = class(specialize TGCustomHashMultiSet<T>)
  protected
    class function GetTableClass: THashTableClass; override;
    class function GetClass: THashMultiSetClass; override;
  end;

  { TGHashMultiSetLP implements open addressing hash multiset with linear probing;
    it assumes that type T implements TEqRel }
  generic TGHashMultiSetLP<T> = class(specialize TGBaseHashMultiSetLP<T, T>);

  { TGBaseHashMultiSetLPT implements open addressing hash multiset with linear probing and lazy deletion }
  generic TGBaseHashMultiSetLPT<T, TEqRel> = class(specialize TGCustomHashMultiSet<T>)
  private
    function GetTombstonesCount: SizeInt; inline;
  protected
  type
    THashTableLPT = specialize TGOpenAddrLPT<T, TEntry, TEqRel>;

    class function GetTableClass: THashTableClass; override;
    class function GetClass: THashMultiSetClass; override;
  public
    procedure ClearTombstones; inline;
    property  TombstonesCount: SizeInt read GetTombstonesCount;
  end;

  { TGHashMultiSetLPT implements open addressing hash multiset with linear probing and lazy deletion;
    it assumes that type T implements TEqRel }
  generic TGHashMultiSetLPT<T> = class(specialize TGBaseHashMultiSetLPT<T, T>);

  { TGBaseHashMultiSetQP implements open addressing hashmultiset with quadratic probing(c1 = c2 = 1/2) }
  generic TGBaseHashMultiSetQP<T, TEqRel> = class(specialize TGCustomHashMultiSet<T>)
  private
    function GetTombstonesCount: SizeInt; inline;
  protected
  type
    THashTableQP = specialize TGOpenAddrQP<T, TEntry, TEqRel>;

    class function GetTableClass: THashTableClass; override;
    class function GetClass: THashMultiSetClass; override;
  public
    procedure ClearTombstones; inline;
    property  TombstonesCount: SizeInt read GetTombstonesCount;
  end;

  { TGHashMultiSetQP implements open addressing hashmultiset with quadratic probing(c1 = c2 = 1/2);
    it assumes that type T implements TEqRel }
  generic TGHashMultiSetQP<T> = class(specialize TGBaseHashMultiSetQP<T, T>);

  { TGBaseChainHashMultiSet implements node based hashset with singly linked list chains }
  generic TGBaseChainHashMultiSet<T, TEqRel> = class(specialize TGCustomHashMultiSet<T>)
  protected
    class function GetTableClass: THashTableClass; override;
    class function GetClass: THashMultiSetClass; override;
  end;

  { TGChainHashMultiSet implements node based hashset with singly linked list chains;
    it assumes that type T implements TEqRel }
  generic TGChainHashMultiSet<T> = class(specialize TGBaseChainHashMultiSet<T, T>);

  { TGCustomObjectHashMultiSet }

  generic TGCustomObjectHashMultiSet<T: class> = class abstract(specialize TGCustomHashMultiSet<T>)
  private
    FOwnsObjects: Boolean;
  protected
  type
    TObjectHashMultiSetClass = class of TGCustomObjectHashMultiSet;

    function  DoSubEntry(constref e: TEntry): Boolean; override;
    function  DoSymmSubEntry(constref e: TEntry): Boolean; override;
    function  DoRemove(constref aKey: T): Boolean; override;
    procedure DoClear; override;
    procedure EntryRemoved(p: PEntry);
    procedure DoIntersect(aSet: TCustomMultiSet); override;
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
    constructor CreateCopy(aMultiSet: TGCustomObjectHashMultiSet);
    property  OwnsObjects: Boolean read FOwnsObjects write FOwnsObjects;
  end;

  generic TGObjectHashMultiSetLP<T: class; TEqRel> = class(specialize TGCustomObjectHashMultiSet<T>)
  protected
    class function GetTableClass: THashTableClass; override;
    class function GetClass: THashMultiSetClass; override;
  public
    function Clone: TGObjectHashMultiSetLP; override;
  end;

  generic TGObjHashMultiSetLP<T: class> = class(specialize TGObjectHashMultiSetLP<T, T>);

  generic TGObjectHashMultiSetLPT<T: class; TEqRel> = class(specialize TGCustomObjectHashMultiSet<T>)
  private
    function GetTombstonesCount: SizeInt; inline;
  protected
  type
    THashTableLPT = specialize TGOpenAddrLPT<T, TEntry, TEqRel>;

    class function GetTableClass: THashTableClass; override;
    class function GetClass: THashMultiSetClass; override;
  public
    function  Clone: TGObjectHashMultiSetLPT; override;
    procedure ClearTombstones; inline;
    property  TombstonesCount: SizeInt read GetTombstonesCount;
  end;

  generic TGObjHashMultiSetLPT<T: class> = class(specialize TGObjectHashMultiSetLPT<T, T>);

  generic TGObjectHashMultiSetQP<T: class; TEqRel> = class(specialize TGCustomObjectHashMultiSet<T>)
  private
    function GetTombstonesCount: SizeInt; inline;
  protected
  type
    THashTableQP = specialize TGOpenAddrQP<T, TEntry, TEqRel>;

    class function GetTableClass: THashTableClass; override;
    class function GetClass: THashMultiSetClass; override;
  public
    function  Clone: TGObjectHashMultiSetQP; override;
    procedure ClearTombstones; inline;
    property  TombstonesCount: SizeInt read GetTombstonesCount;
  end;

  generic TGObjHashMultiSetQP<T: class> = class(specialize TGObjectHashMultiSetQP<T, T>);

  generic TGObjectChainHashMultiSet<T: class; TEqRel> = class(specialize TGCustomObjectHashMultiSet<T>)
  protected
    class function GetTableClass: THashTableClass; override;
    class function GetClass: THashMultiSetClass; override;
  public
    function Clone: TGObjectChainHashMultiSet; override;
  end;

  generic TGObjChainHashMultiSet<T: class> = class(specialize TGObjectChainHashMultiSet<T, T>);

  { TGLiteHashMultiSetLP implements open addressing hash multiset with linear probing;
      functor TEqRel(equality relation) must provide:
        class function HashCode([const[ref]] aValue: T): SizeInt;
        class function Equal([const[ref]] L, R: T): Boolean; }
  generic TGLiteHashMultiSetLP<T, TEqRel> = record
  public
  type
    TEntry      = specialize TGMultiSetEntry<T>;
    IEnumerable = specialize IGEnumerable<T>;
    ICollection = specialize IGCollection<T>;
    TTest       = specialize TGTest<T>;
    TOnTest     = specialize TGOnTest<T>;
    TNestTest   = specialize TGNestTest<T>;
    TArray      = array of T;
    TEntryArray = array of TEntry;

  private
  type
    TTableLP  = specialize TGLiteHashTableLP<T, TEntry, TEqRel>;
    PMultiSet = ^TGLiteHashMultiSetLP;
    PEntry    = ^TEntry;

  public
  type
    TEnumerator = record
    private
      FEnum: TTableLP.TEnumerator;
      FCurrKeyCount: SizeInt;
      function  GetCurrent: T; inline;
      procedure Init(constref aSet: TGLiteHashMultiSetLP); inline;
    public
      function  MoveNext: Boolean; inline;
      procedure Reset; inline;
      property  Current: T read GetCurrent;
    end;

    TDistinctEnumerator = record
    private
      FEnum: TTableLP.TEnumerator;
      function  GetCurrent: T; inline;
      procedure Init(constref aSet: TGLiteHashMultiSetLP); inline;
    public
      function  MoveNext: Boolean; inline;
      procedure Reset; inline;
      property  Current: T read GetCurrent;
    end;

    TEntryEnumerator = record
    private
      FEnum: TTableLP.TEnumerator;
      function  GetCurrent: TEntry; inline;
      procedure Init(constref aSet: TGLiteHashMultiSetLP); inline;
    public
      function  MoveNext: Boolean; inline;
      procedure Reset; inline;
      property  Current: TEntry read GetCurrent;
    end;

    TDistinct = record
    private
      FMultiset: PMultiSet;
      procedure Init(aSet: PMultiSet); inline;
    public
      function GetEnumerator: TDistinctEnumerator;
    end;

    TEntries = record
    private
      FMultiset: PMultiSet;
      procedure Init(aSet: PMultiSet); inline;
    public
      function GetEnumerator: TEntryEnumerator;
    end;

  private
    FTable: TTableLP;
    FCount: SizeInt;
    function  GetCapacity: SizeInt; inline;
    function  GetEntryCount: SizeInt; inline;
    function  GetExpandTreshold: SizeInt; inline;
    function  GetFillRatio: Single; inline;
    function  GetLoadFactor: Single; inline;
    procedure SetLoadFactor(aValue: Single); inline;
    function  GetDistinctEnumerator: TDistinctEnumerator; inline;
    function  GetEntryEnumerator: TEntryEnumerator; inline;
    function  Find(constref aKey: T): PEntry; inline;
    function  FindOrAdd(constref aKey: T; out p: PEntry): Boolean;
    function  GetKeyCount(const aKey: T): SizeInt;
    procedure SetKeyCount(const aKey: T; aValue: SizeInt);
    class operator Initialize(var ms: TGLiteHashMultiSetLP);
  public
    function  DefaultLoadFactor: Single; inline;
    function  MaxLoadFactor: Single; inline;
    function  MinLoadFactor: Single; inline;
    function  GetEnumerator: TEnumerator; inline;
    function  Distinct: TDistinct; inline;
    function  Entries: TEntries; inline;
    function  ToArray: TArray;
    function  ToEntryArray: TEntryArray;
    function  IsEmpty: Boolean; inline;
    function  NonEmpty: Boolean; inline;
    procedure Clear;
    procedure TrimToFit; inline;
    procedure EnsureCapacity(aValue: SizeInt); inline;
    function  Contains(constref aValue: T): Boolean; inline;
    function  NonContains(constref aValue: T): Boolean; inline;
    function  ContainsAny(constref a: array of T): Boolean;
    function  ContainsAny(e: IEnumerable): Boolean;
    function  ContainsAny(constref aSet: TGLiteHashMultiSetLP): Boolean;
    function  ContainsAll(constref a: array of T): Boolean;
    function  ContainsAll(e: IEnumerable): Boolean;
    function  ContainsAll(constref aSet: TGLiteHashMultiSetLP): Boolean;
    procedure Add(constref aValue: T);
  { returns count of added elements }
    function  AddAll(constref a: array of T): SizeInt;
    function  AddAll(e: IEnumerable): SizeInt;
    function  AddAll(constref aSet: TGLiteHashMultiSetLP): SizeInt;
  { returns True if element removed }
    function  Remove(constref aValue: T): Boolean; inline;
    function  RemoveAll(constref a: array of T): SizeInt;
    function  RemoveAll(e: IEnumerable): SizeInt;
    function  RemoveAll(constref aSet: TGLiteHashMultiSetLP): SizeInt;
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
  { returns True if multiplicity of an any key in self is greater then or equal to
    the multiplicity of that key in aSet }
    function  IsSuperMultiSet(constref aSet: TGLiteHashMultiSetLP): Boolean; inline;
  { returns True if multiplicity of an any key in aSet is greater then or equal to
    the multiplicity of that key in self }
    function  IsSubMultiSet(constref aSet: TGLiteHashMultiSetLP): Boolean; inline;
  { returns True if the multiplicity of an any key in self is equal to the multiplicity of that key in aSet }
    function  IsEqual(constref aSet: TGLiteHashMultiSetLP): Boolean;
    function  Intersecting(constref aSet: TGLiteHashMultiSetLP): Boolean;
  { will contain only those keys that are simultaneously contained in self and in aSet;
    the multiplicity of a key becomes equal to the MINIMUM of the multiplicities of a key in self and aSet }
    procedure Intersect(constref aSet: TGLiteHashMultiSetLP);
  { will contain all keys that are contained in self or in aSet;
    the multiplicity of a key will become equal to the MAXIMUM of the multiplicities of
    a key in self and aSet }
    procedure Join(constref aSet: TGLiteHashMultiSetLP);
  { will contain all keys that are contained in self or in aSet;
    the multiplicity of a key will become equal to the SUM of the multiplicities of a key in self and aSet }
    procedure ArithmeticAdd(constref aSet: TGLiteHashMultiSetLP);
  { will contain only those keys whose multiplicity is greater then the multiplicity
    of that key in aSet; the multiplicity of a key will become equal to the difference of multiplicities
    of a key in self and aSet }
    procedure ArithmeticSubtract(constref aSet: TGLiteHashMultiSetLP);
  { will contain only those keys whose multiplicity is not equal to the multiplicity
    of that key in aSet; the multiplicity of a key will become equal to absolute value of difference
    of the multiplicities of a key in self and aSet }
    procedure SymmetricSubtract(constref aSet: TGLiteHashMultiSetLP);
    property  Count: SizeInt read FCount;
  { returs number of distinct keys }
    property  EntryCount: SizeInt read GetEntryCount; //dimension, Count - cardinality
    property  Capacity: SizeInt read GetCapacity;
    property  LoadFactor: Single read GetLoadFactor write SetLoadFactor;
    property  FillRatio: Single read GetFillRatio;
  { The number of entries that can be written without rehashing }
    property  ExpandTreshold: SizeInt read GetExpandTreshold;
  { will return 0 if not contains an element aValue;
    will raise EArgumentException if one try to set negative multiplicity of a aValue }
    property  Counts[const aValue: T]: SizeInt read GetKeyCount write SetKeyCount; default;
  end;

implementation
{$B-}{$COPERATORS ON}

{ TGCustomHashMultiSet.TEnumerator }

function TGCustomHashMultiSet.TEnumerator.GetCurrent: T;
begin
  Result := FEnum.Current^.Key;
end;

constructor TGCustomHashMultiSet.TEnumerator.Create(ms: TCustomHashMultiSet);
begin
  inherited Create(ms);
  FEnum := ms.FTable.GetEnumerator;
end;

destructor TGCustomHashMultiSet.TEnumerator.Destroy;
begin
  FEnum.Free;
  inherited;
end;

function TGCustomHashMultiSet.TEnumerator.MoveNext: Boolean;
begin
  Result := FCurrKeyCount > 0;
  FCurrKeyCount -= Ord(Result);
  if not Result then
    begin
      Result := FEnum.MoveNext;
      if Result then
        FCurrKeyCount := Pred(FEnum.Current^.Count);
    end;
end;

procedure TGCustomHashMultiSet.TEnumerator.Reset;
begin
  FEnum.Reset;
  FCurrKeyCount := 0;
end;

{ TGCustomHashMultiSet.TDistinctEnumerable }

function TGCustomHashMultiSet.TDistinctEnumerable.GetCurrent: T;
begin
  Result := FEnum.Current^.Key;
end;

constructor TGCustomHashMultiSet.TDistinctEnumerable.Create(aSet: TCustomHashMultiSet);
begin
  inherited Create(aSet);
  FEnum := aSet.FTable.GetEnumerator;
end;

destructor TGCustomHashMultiSet.TDistinctEnumerable.Destroy;
begin
  FEnum.Free;
  inherited;
end;

function TGCustomHashMultiSet.TDistinctEnumerable.MoveNext: Boolean;
begin
  Result := FEnum.MoveNext;
end;

procedure TGCustomHashMultiSet.TDistinctEnumerable.Reset;
begin
  FEnum.Reset;
end;

{ TGCustomHashMultiSet.TEntryEnumerable }

function TGCustomHashMultiSet.TEntryEnumerable.GetCurrent: TEntry;
begin
  Result := FEnum.Current^;
end;

constructor TGCustomHashMultiSet.TEntryEnumerable.Create(aSet: TCustomHashMultiSet);
begin
  inherited Create;
  FOwner := aSet;
  FEnum := aSet.FTable.GetEnumerator;
end;

destructor TGCustomHashMultiSet.TEntryEnumerable.Destroy;
begin
  FEnum.Free;
  FOwner.EndIteration;
  inherited;
end;

function TGCustomHashMultiSet.TEntryEnumerable.MoveNext: Boolean;
begin
  Result := FEnum.MoveNext;
end;

procedure TGCustomHashMultiSet.TEntryEnumerable.Reset;
begin
  FEnum.Reset;
end;

{ TGCustomHashMultiSet }

function TGCustomHashMultiSet.GetExpandTreshold: SizeInt;
begin
  Result := FTable.ExpandTreshold;
end;

procedure TGCustomHashMultiSet.EntryRemoved(p: PEntry);
begin
  FCount -= p^.Count;
end;

function TGCustomHashMultiSet.GetFillRatio: Single;
begin
  Result := FTable.FillRatio;
end;

function TGCustomHashMultiSet.GetLoadFactor: Single;
begin
  Result := FTable.LoadFactor;
end;

procedure TGCustomHashMultiSet.SetLoadFactor(aValue: Single);
begin
  FTable.LoadFactor := aValue;
end;

function TGCustomHashMultiSet.GetCapacity: SizeInt;
begin
  Result := FTable.Capacity;
end;

function TGCustomHashMultiSet.DoGetEnumerator: TCustomEnumerator;
begin
  Result := TEnumerator.Create(Self);
end;

procedure TGCustomHashMultiSet.DoClear;
begin
  FTable.Clear;
  FCount := 0;
end;

procedure TGCustomHashMultiSet.DoTrimToFit;
begin
  FTable.TrimToFit;
end;

procedure TGCustomHashMultiSet.DoEnsureCapacity(aValue: SizeInt);
begin
  FTable.EnsureCapacity(aValue);
end;

function TGCustomHashMultiSet.FindEntry(constref aKey: T): PEntry;
var
  sr: TSearchResult;
begin
  Result := FTable.Find(aKey, sr);
end;

function TGCustomHashMultiSet.FindOrAdd(constref aKey: T; out p: PEntry): Boolean;
var
  sr: TSearchResult;
begin
  Result := FTable.FindOrAdd(aKey, p, sr);
  if not Result then
    begin
      p^.Key := aKey;
      p^.Count := 1;
    end;
end;

function TGCustomHashMultiSet.DoSubEntry(constref e: TEntry): Boolean;
var
  p: PEntry;
  sr: TSearchResult;
begin
  p := FTable.Find(e.Key, sr);
  if p <> nil then
    begin
      if p^.Count > e.Count then
        begin
          FCount -= e.Count;
          p^.Count -= e.Count;
        end
      else
        begin
          FCount -= p^.Count;
          FTable.RemoveAt(sr);
          exit(True);
        end;
    end;
  Result := False;
end;

function TGCustomHashMultiSet.DoSymmSubEntry(constref e: TEntry): Boolean;
var
  sr: TSearchResult;
  p: PEntry;
begin
  if FTable.FindOrAdd(e.Key, p, sr) then
    begin
      if p^.Count > e.Count then
        begin
          FCount -= e.Count;
          p^.Count -= e.Count;
        end
      else
        if p^.Count < e.Count then
          begin
           {$PUSH}{$Q+}
            FCount -= p^.Count shl 1 - e.Count;
           {$POP}
            p^.Count := e.Count - p^.Count;
          end
        else  // counts equals
          begin
            FCount -= p^.Count;
            FTable.RemoveAt(sr);
            exit(True);
          end;
    end
  else
    begin
      p^.Key := e.Key;
      p^.Count := e.Count;
      {$PUSH}{$Q+}
      FCount += e.Count;
      {$POP}
    end;
  Result := False;
end;

function TGCustomHashMultiSet.DoExtract(constref aKey: T): Boolean;
var
  p: PEntry;
  sr: TSearchResult;
begin
  p := FTable.Find(aKey, sr);
  Result := p <> nil;
  if Result then
    begin
      Dec(p^.Count);
      Dec(FCount);
      if p^.Count = 0 then
        FTable.RemoveAt(sr);
    end;
end;

function TGCustomHashMultiSet.GetEntryCount: SizeInt;
begin
  Result := FTable.Count;
end;

function TGCustomHashMultiSet.DoDoubleEntryCounters: SizeInt;
var
  p: PEntry;
begin
  Result := ElemCount;
{$PUSH}{$Q+}
  FCount += ElemCount;
{$POP}
  with FTable.GetEnumerator do
    try
      while MoveNext do
        begin
          p := Current;
          p^.Count += p^.Count;
        end;
    finally
      Free;
    end;
end;

function TGCustomHashMultiSet.GetDistinct: IEnumerable;
begin
  Result := TDistinctEnumerable.Create(Self);
end;

function TGCustomHashMultiSet.GetEntries: IEntryEnumerable;
begin
  Result := TEntryEnumerable.Create(Self);
end;

procedure TGCustomHashMultiSet.DoIntersect(aSet: TCustomMultiSet);
var
  I{%H-}: TIntersectHelper;
begin
  I.FSet := Self;
  I.FOtherSet := aSet;
  FTable.RemoveIf(@I.OnIntersect, @EntryRemoved);
end;

function TGCustomHashMultiSet.DoRemoveIf(aTest: TTest): SizeInt;
begin
  Result := ElemCount;
  FTable.RemoveIf(aTest, @EntryRemoved);
  Result -= ElemCount;
end;

function TGCustomHashMultiSet.DoRemoveIf(aTest: TOnTest): SizeInt;
begin
  Result := ElemCount;
  FTable.RemoveIf(aTest, @EntryRemoved);
  Result -= ElemCount;
end;

function TGCustomHashMultiSet.DoRemoveIf(aTest: TNestTest): SizeInt;
begin
  Result := ElemCount;
  FTable.RemoveIf(aTest, @EntryRemoved);
  Result -= ElemCount;
end;

function TGCustomHashMultiSet.DoExtractIf(aTest: TTest): TArray;
var
  e: TExtractHelper;
begin
  e.Init;
  FTable.RemoveIf(aTest, @e.OnExtract);
  Result := e.Final;
  FCount -= System.Length(Result);
end;

function TGCustomHashMultiSet.DoExtractIf(aTest: TOnTest): TArray;
var
  e: TExtractHelper;
begin
  e.Init;
  FTable.RemoveIf(aTest, @e.OnExtract);
  Result := e.Final;
  FCount -= System.Length(Result);
end;

function TGCustomHashMultiSet.DoExtractIf(aTest: TNestTest): TArray;
var
  e: TExtractHelper;
begin
  e.Init;
  FTable.RemoveIf(aTest, @e.OnExtract);
  Result := e.Final;
  FCount -= System.Length(Result);
end;

class function TGCustomHashMultiSet.DefaultLoadFactor: Single;
begin
  Result := GetTableClass.DefaultLoadFactor;
end;

class function TGCustomHashMultiSet.MaxLoadFactor: Single;
begin
  Result := GetTableClass.MaxLoadFactor;
end;

class function TGCustomHashMultiSet.MinLoadFactor: Single;
begin
  Result := GetTableClass.MinLoadFactor;
end;

constructor TGCustomHashMultiSet.Create;
begin
  FTable := GetTableClass.Create;
end;

constructor TGCustomHashMultiSet.Create(constref a: array of T);
begin
  FTable := GetTableClass.Create;
  DoAddAll(a);
end;

constructor TGCustomHashMultiSet.Create(e: IEnumerable);
var
  o: TObject;
begin
  o := e._GetRef;
  if o is TCustomHashMultiSet then
    CreateCopy(TCustomHashMultiSet(o))
  else
    begin
      if o is TCustomMultiSet then
        Create(TCustomMultiSet(o).EntryCount)
      else
        Create;
      DoAddAll(e);
    end;
end;

constructor TGCustomHashMultiSet.Create(aCapacity: SizeInt);
begin
  FTable := GetTableClass.Create(aCapacity);
end;

constructor TGCustomHashMultiSet.Create(aCapacity: SizeInt; constref a: array of T);
begin
  FTable := GetTableClass.Create(aCapacity);
  DoAddAll(a);
end;

constructor TGCustomHashMultiSet.Create(aCapacity: SizeInt; e: IEnumerable);
begin
  FTable := GetTableClass.Create(aCapacity);
  DoAddAll(e);
end;

constructor TGCustomHashMultiSet.Create(aLoadFactor: Single);
begin
  FTable := GetTableClass.Create(aLoadFactor);
end;

constructor TGCustomHashMultiSet.Create(aLoadFactor: Single; constref a: array of T);
begin
  FTable := GetTableClass.Create(aLoadFactor);
  DoAddAll(a);
end;

constructor TGCustomHashMultiSet.Create(aLoadFactor: Single; e: IEnumerable);
var
  o: TObject;
begin
  o := e._GetRef;
  if o is TCustomMultiSet then
    Create(TCustomMultiSet(o).EntryCount, aLoadFactor)
  else
    Create(aLoadFactor);
  DoAddAll(e);
end;

constructor TGCustomHashMultiSet.Create(aCapacity: SizeInt; aLoadFactor: Single);
begin
  FTable := GetTableClass.Create(aCapacity, aLoadFactor);
end;

constructor TGCustomHashMultiSet.Create(aCapacity: SizeInt; aLoadFactor: Single; constref a: array of T);
begin
  FTable := GetTableClass.Create(aCapacity, aLoadFactor);
  DoAddAll(a);
end;

constructor TGCustomHashMultiSet.Create(aCapacity: SizeInt; aLoadFactor: Single; e: IEnumerable);
begin
  FTable := GetTableClass.Create(aCapacity, aLoadFactor);
  DoAddAll(e);
end;

constructor TGCustomHashMultiSet.CreateCopy(aMultiSet: TCustomHashMultiSet);
var
  e: TEntry;
begin
  if aMultiSet.GetClass = GetClass then
    begin
      FTable := aMultiSet.FTable.Clone;
      FCount := aMultiSet.Count;
    end
  else
    begin
      FTable := GetTableClass.Create(aMultiSet.EntryCount);
      for e in aMultiSet.Entries do
        DoAddEntry(e);
    end;
end;

destructor TGCustomHashMultiSet.Destroy;
begin
  DoClear;
  FTable.Free;
  inherited;
end;

function TGCustomHashMultiSet.Clone: TCustomHashMultiSet;
begin
  Result := GetClass.Create(Self);
end;

{ TGBaseHashMultiSetLP }

class function TGBaseHashMultiSetLP.GetTableClass: THashTableClass;
begin
  Result := specialize TGOpenAddrLP<T, TEntry, TEqRel>;
end;

class function TGBaseHashMultiSetLP.GetClass: THashMultiSetClass;
begin
  Result := TGBaseHashMultiSetLP;
end;

{ TGBaseHashMultiSetLPT }

function TGBaseHashMultiSetLPT.GetTombstonesCount: SizeInt;
begin
  Result := THashTableLPT(FTable).TombstonesCount;
end;

class function TGBaseHashMultiSetLPT.GetTableClass: THashTableClass;
begin
  Result := THashTableLPT;
end;

class function TGBaseHashMultiSetLPT.GetClass: THashMultiSetClass;
begin
  Result := TGBaseHashMultiSetLPT;
end;

procedure TGBaseHashMultiSetLPT.ClearTombstones;
begin
  THashTableLPT(FTable).ClearTombstones;
end;

{ TGBaseHashMultiSetQP }

function TGBaseHashMultiSetQP.GetTombstonesCount: SizeInt;
begin
  Result := THashTableQP(FTable).TombstonesCount;
end;

class function TGBaseHashMultiSetQP.GetTableClass: THashTableClass;
begin
  Result := THashTableQP;
end;

class function TGBaseHashMultiSetQP.GetClass: THashMultiSetClass;
begin
  Result := TGBaseHashMultiSetQP;
end;

procedure TGBaseHashMultiSetQP.ClearTombstones;
begin
  THashTableQP(FTable).ClearTombstones;
end;

{ TGBaseChainHashMultiSet }

class function TGBaseChainHashMultiSet.GetTableClass: THashTableClass;
begin
 Result := specialize TGChainHashTable<T, TEntry, TEqRel>;
end;

class function TGBaseChainHashMultiSet.GetClass: THashMultiSetClass;
begin
  Result := TGBaseChainHashMultiSet;
end;

{ TGCustomObjectHashMultiSet }

function TGCustomObjectHashMultiSet.DoSubEntry(constref e: TEntry): Boolean;
begin
  Result := inherited DoSubEntry(e);
  if Result and OwnsObjects then
    e.Key.Free;
end;

function TGCustomObjectHashMultiSet.DoSymmSubEntry(constref e: TEntry): Boolean;
begin
  Result := inherited DoSymmSubEntry(e);
  if Result and OwnsObjects then
    e.Key.Free;
end;

function TGCustomObjectHashMultiSet.DoRemove(constref aKey: T): Boolean;
var
  p: PEntry;
  ItemPos: TSearchResult;
begin
  p := FTable.Find(aKey, ItemPos);
  Result := p <> nil;
  if Result then
    begin
      Dec(p^.Count);
      Dec(FCount);
      if p^.Count = 0 then
        begin
          FTable.RemoveAt(ItemPos);
          if OwnsObjects then
            aKey.Free;
        end;
    end;
end;

procedure TGCustomObjectHashMultiSet.DoClear;
var
  p: PEntry;
begin
  if OwnsObjects then
    for p in FTable do
      p^.Key.Free;
  inherited;
end;

procedure TGCustomObjectHashMultiSet.EntryRemoved(p: PEntry);
begin
  FCount -= p^.Count;
  if OwnsObjects then
    p^.Key.Free;
end;

procedure TGCustomObjectHashMultiSet.DoIntersect(aSet: TCustomMultiSet);
var
  {%H-}I: TIntersectHelper;
begin
  I.FSet := Self;
  I.FOtherSet := aSet;
  FTable.RemoveIf(@I.OnIntersect, @EntryRemoved);
end;

function TGCustomObjectHashMultiSet.DoRemoveIf(aTest: TTest): SizeInt;
begin
  Result := ElemCount;
  FTable.RemoveIf(aTest, @EntryRemoved);
  Result -= ElemCount;
end;

function TGCustomObjectHashMultiSet.DoRemoveIf(aTest: TOnTest): SizeInt;
begin
  Result := ElemCount;
  FTable.RemoveIf(aTest, @EntryRemoved);
  Result -= ElemCount;
end;

function TGCustomObjectHashMultiSet.DoRemoveIf(aTest: TNestTest): SizeInt;
begin
  Result := ElemCount;
  FTable.RemoveIf(aTest, @EntryRemoved);
  Result -= ElemCount;
end;

constructor TGCustomObjectHashMultiSet.Create(aOwnsObjects: Boolean);
begin
  inherited Create;
  FOwnsObjects := aOwnsObjects;
end;

constructor TGCustomObjectHashMultiSet.Create(constref a: array of T; aOwnsObjects: Boolean);
begin
  inherited Create(a);
  FOwnsObjects := aOwnsObjects;
end;

constructor TGCustomObjectHashMultiSet.Create(e: IEnumerable; aOwnsObjects: Boolean);
begin
  inherited Create(e);
  FOwnsObjects := aOwnsObjects;
end;

constructor TGCustomObjectHashMultiSet.Create(aCapacity: SizeInt; aOwnsObjects: Boolean);
begin
  inherited Create(aCapacity);
  FOwnsObjects := aOwnsObjects;
end;

constructor TGCustomObjectHashMultiSet.Create(aCapacity: SizeInt; constref a: array of T; aOwnsObjects: Boolean);
begin
  inherited Create(aCapacity, a);
  FOwnsObjects := aOwnsObjects;
end;

constructor TGCustomObjectHashMultiSet.Create(aCapacity: SizeInt; e: IEnumerable; aOwnsObjects: Boolean);
begin
  inherited Create(aCapacity, e);
  FOwnsObjects := aOwnsObjects;
end;

constructor TGCustomObjectHashMultiSet.Create(aLoadFactor: Single; aOwnsObjects: Boolean);
begin
  inherited Create(aLoadFactor);
  FOwnsObjects := aOwnsObjects;
end;

constructor TGCustomObjectHashMultiSet.Create(aLoadFactor: Single; constref a: array of T; aOwnsObjects: Boolean);
begin
  inherited Create(aLoadFactor, a);
  FOwnsObjects := aOwnsObjects;
end;

constructor TGCustomObjectHashMultiSet.Create(aLoadFactor: Single; e: IEnumerable; aOwnsObjects: Boolean);
begin
  inherited Create(aLoadFactor, e);
  FOwnsObjects := aOwnsObjects;
end;

constructor TGCustomObjectHashMultiSet.Create(aCapacity: SizeInt; aLoadFactor: Single; aOwnsObjects: Boolean);
begin
  inherited Create(aCapacity, aLoadFactor);
  FOwnsObjects := aOwnsObjects;
end;

constructor TGCustomObjectHashMultiSet.Create(aCapacity: SizeInt; aLoadFactor: Single; constref a: array of T;
  aOwnsObjects: Boolean);
begin
  inherited Create(aCapacity, aLoadFactor, a);
  FOwnsObjects := aOwnsObjects;
end;

constructor TGCustomObjectHashMultiSet.Create(aCapacity: SizeInt; aLoadFactor: Single; e: IEnumerable;
  aOwnsObjects: Boolean);
begin
  inherited Create(aCapacity, aLoadFactor, e);
  FOwnsObjects := aOwnsObjects;
end;

constructor TGCustomObjectHashMultiSet.CreateCopy(aMultiSet: TGCustomObjectHashMultiSet);
begin
  inherited CreateCopy(aMultiSet);
  FOwnsObjects := aMultiSet.OwnsObjects;
end;

{ TGObjectHashMultiSetLP }

class function TGObjectHashMultiSetLP.GetTableClass: THashTableClass;
begin
  Result := specialize TGOpenAddrLP<T, TEntry, TEqRel>;
end;

class function TGObjectHashMultiSetLP.GetClass: THashMultiSetClass;
begin
  Result := TGObjectHashMultiSetLP;
end;

function TGObjectHashMultiSetLP.Clone: TGObjectHashMultiSetLP;
begin
  Result := TGObjectHashMultiSetLP.CreateCopy(Self);
end;

{ TGObjectHashMultiSetLPT }

function TGObjectHashMultiSetLPT.GetTombstonesCount: SizeInt;
begin
  Result := THashTableLPT(FTable).TombstonesCount;
end;

class function TGObjectHashMultiSetLPT.GetTableClass: THashTableClass;
begin
  Result := THashTableLPT;
end;

class function TGObjectHashMultiSetLPT.GetClass: THashMultiSetClass;
begin
  Result := TGObjectHashMultiSetLPT;
end;

function TGObjectHashMultiSetLPT.Clone: TGObjectHashMultiSetLPT;
begin
  Result := TGObjectHashMultiSetLPT.CreateCopy(Self);
end;

procedure TGObjectHashMultiSetLPT.ClearTombstones;
begin
  THashTableLPT(FTable).ClearTombstones;
end;

{ TGObjectHashMultiSetQP }

function TGObjectHashMultiSetQP.GetTombstonesCount: SizeInt;
begin
  Result := THashTableQP(FTable).TombstonesCount;
end;

class function TGObjectHashMultiSetQP.GetTableClass: THashTableClass;
begin
  Result := THashTableQP;
end;

class function TGObjectHashMultiSetQP.GetClass: THashMultiSetClass;
begin
  Result := TGObjectHashMultiSetQP;
end;

function TGObjectHashMultiSetQP.Clone: TGObjectHashMultiSetQP;
begin
  Result := TGObjectHashMultiSetQP.CreateCopy(Self);
end;

procedure TGObjectHashMultiSetQP.ClearTombstones;
begin
  THashTableQP(FTable).ClearTombstones;
end;

{ TGObjectChainHashMultiSet }

class function TGObjectChainHashMultiSet.GetTableClass: THashTableClass;
begin
  Result := specialize TGChainHashTable<T, TEntry, TEqRel>;
end;

class function TGObjectChainHashMultiSet.GetClass: THashMultiSetClass;
begin
  Result := TGObjectChainHashMultiSet;
end;

function TGObjectChainHashMultiSet.Clone: TGObjectChainHashMultiSet;
begin
  Result := TGObjectChainHashMultiSet.CreateCopy(Self);
end;

{ TGLiteHashMultiSetLP.TEnumerator }

function TGLiteHashMultiSetLP.TEnumerator.GetCurrent: T;
begin
  Result := FEnum.Current^.Key;
end;

procedure TGLiteHashMultiSetLP.TEnumerator.Init(constref aSet: TGLiteHashMultiSetLP);
begin
  FEnum := aSet.FTable.GetEnumerator;
  FCurrKeyCount := 0;
end;

function TGLiteHashMultiSetLP.TEnumerator.MoveNext: Boolean;
begin
  Result := FCurrKeyCount > 0;
  FCurrKeyCount -= Ord(Result);
  if not Result then
    begin
      Result := FEnum.MoveNext;
      if Result then
        FCurrKeyCount := Pred(FEnum.Current^.Count);
    end;
end;

procedure TGLiteHashMultiSetLP.TEnumerator.Reset;
begin
  FEnum.Reset;
  FCurrKeyCount := 0;
end;

{ TGLiteHashMultiSetLP.TDistinctEnumerator }

function TGLiteHashMultiSetLP.TDistinctEnumerator.GetCurrent: T;
begin
  Result := FEnum.Current^.Key;
end;

procedure TGLiteHashMultiSetLP.TDistinctEnumerator.Init(constref aSet: TGLiteHashMultiSetLP);
begin
  FEnum := aSet.FTable.GetEnumerator;
end;

function TGLiteHashMultiSetLP.TDistinctEnumerator.MoveNext: Boolean;
begin
  Result := FEnum.MoveNext;
end;

procedure TGLiteHashMultiSetLP.TDistinctEnumerator.Reset;
begin
  FEnum.Reset;
end;

{ TGLiteHashMultiSetLP.TEntryEnumerator }

function TGLiteHashMultiSetLP.TEntryEnumerator.GetCurrent: TEntry;
begin
  Result := FEnum.Current^;
end;

procedure TGLiteHashMultiSetLP.TEntryEnumerator.Init(constref aSet: TGLiteHashMultiSetLP);
begin
  FEnum := aSet.FTable.GetEnumerator;
end;

function TGLiteHashMultiSetLP.TEntryEnumerator.MoveNext: Boolean;
begin
  Result := FEnum.MoveNext;
end;

procedure TGLiteHashMultiSetLP.TEntryEnumerator.Reset;
begin
  FEnum.Reset;
end;

{ TGLiteHashMultiSetLP.TDistinct }

procedure TGLiteHashMultiSetLP.TDistinct.Init(aSet: PMultiSet);
begin
  FMultiset := aSet;
end;

function TGLiteHashMultiSetLP.TDistinct.GetEnumerator: TDistinctEnumerator;
begin
  FMultiset^.GetDistinctEnumerator;
end;

{ TGLiteHashMultiSetLP.TEntries }

procedure TGLiteHashMultiSetLP.TEntries.Init(aSet: PMultiSet);
begin
  FMultiset := aSet;
end;

function TGLiteHashMultiSetLP.TEntries.GetEnumerator: TEntryEnumerator;
begin
  FMultiset^.GetEntryEnumerator;
end;

{ TGLiteHashMultiSetLP }

function TGLiteHashMultiSetLP.GetEntryCount: SizeInt;
begin
  Result := FTable.Count;
end;

function TGLiteHashMultiSetLP.GetExpandTreshold: SizeInt;
begin
  Result := FTable.ExpandTreshold;
end;

function TGLiteHashMultiSetLP.GetCapacity: SizeInt;
begin
  Result := FTable.Capacity;
end;

function TGLiteHashMultiSetLP.GetFillRatio: Single;
begin
  Result := FTable.FillRatio;
end;

function TGLiteHashMultiSetLP.GetLoadFactor: Single;
begin
  Result := FTable.LoadFactor;
end;

procedure TGLiteHashMultiSetLP.SetLoadFactor(aValue: Single);
begin
  FTable.LoadFactor := aValue;
end;

function TGLiteHashMultiSetLP.GetDistinctEnumerator: TDistinctEnumerator;
begin
  Result.Init(Self);
end;

function TGLiteHashMultiSetLP.GetEntryEnumerator: TEntryEnumerator;
begin
  Result.Init(Self);
end;

function TGLiteHashMultiSetLP.Find(constref aKey: T): PEntry;
var
  Pos: SizeInt;
begin
  Result := FTable.Find(aKey, Pos);
end;

function TGLiteHashMultiSetLP.FindOrAdd(constref aKey: T; out p: PEntry): Boolean;
var
  Pos: SizeInt;
begin
  Result := FTable.FindOrAdd(aKey, p, Pos);
  if not Result then
    begin
      p^.Key := aKey;
      p^.Count := 1;
    end;
end;

function TGLiteHashMultiSetLP.GetKeyCount(const aKey: T): SizeInt;
var
  p: PEntry;
begin
  p := Find(aKey);
  if p <> nil then
    Result := p^.Count
  else
    Result := 0;
end;

procedure TGLiteHashMultiSetLP.SetKeyCount(const aKey: T; aValue: SizeInt);
var
  p: PEntry;
  Pos: SizeInt;
begin
  if aValue < 0 then
    raise EArgumentException.Create(SECantAcceptNegCount);
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
      p := FTable.Find(aKey, Pos);
      if p <> nil then
        begin
          FCount -= p^.Count;
          FTable.RemoveAt(Pos);
        end;
    end;
end;

class operator TGLiteHashMultiSetLP.Initialize(var ms: TGLiteHashMultiSetLP);
begin
  ms.FCount := 0;
end;

function TGLiteHashMultiSetLP.DefaultLoadFactor: Single;
begin
  Result := FTable.DEFAULT_LOAD_FACTOR;
end;

function TGLiteHashMultiSetLP.MaxLoadFactor: Single;
begin
  Result := FTable.MAX_LOAD_FACTOR;
end;

function TGLiteHashMultiSetLP.MinLoadFactor: Single;
begin
  Result := FTable.MIN_LOAD_FACTOR;
end;

function TGLiteHashMultiSetLP.GetEnumerator: TEnumerator;
begin
  Result.Init(Self);
end;

function TGLiteHashMultiSetLP.Distinct: TDistinct;
begin
  Result.Init(@Self);
end;

function TGLiteHashMultiSetLP.Entries: TEntries;
begin
  Result.Init(@Self);
end;

function TGLiteHashMultiSetLP.ToArray: TArray;
var
  I: SizeInt = 0;
  v: T;
begin
  System.SetLength(Result, Count);
  for v in Self do
    begin
      Result[I] := v;
      Inc(I);
    end;
end;

function TGLiteHashMultiSetLP.ToEntryArray: TEntryArray;
var
  I: SizeInt = 0;
  p: PEntry;
begin
  System.SetLength(Result, EntryCount);
  for p in FTable do
    begin
      Result[I] := p^;
      Inc(I);
    end;
end;

function TGLiteHashMultiSetLP.IsEmpty: Boolean;
begin
  Result := Count = 0;
end;

function TGLiteHashMultiSetLP.NonEmpty: Boolean;
begin
  Result := Count <> 0;
end;

procedure TGLiteHashMultiSetLP.Clear;
begin
  FTable.Clear;
  FCount := 0;
end;

procedure TGLiteHashMultiSetLP.TrimToFit;
begin
  FTable.TrimToFit;
end;

procedure TGLiteHashMultiSetLP.EnsureCapacity(aValue: SizeInt);
begin
  FTable.EnsureCapacity(aValue);
end;

function TGLiteHashMultiSetLP.Contains(constref aValue: T): Boolean;
begin
  Result := Find(aValue) <> nil;
end;

function TGLiteHashMultiSetLP.NonContains(constref aValue: T): Boolean;
begin
  Result := Find(aValue) = nil;
end;

function TGLiteHashMultiSetLP.ContainsAny(constref a: array of T): Boolean;
var
  v: T;
begin
  for v in a do
    if Contains(v) then
      exit(True);
  Result := False;
end;

function TGLiteHashMultiSetLP.ContainsAny(e: IEnumerable): Boolean;
var
  v: T;
begin
  for v in e do
    if Contains(v) then
      exit(True);
  Result := False;
end;

function TGLiteHashMultiSetLP.ContainsAny(constref aSet: TGLiteHashMultiSetLP): Boolean;
var
  v: T;
begin
  if @aSet <> @Self then
    begin
      for v in aSet.Distinct do
        if Contains(v) then
          exit(True);
      Result := False;
    end
  else
    Result := True;
end;

function TGLiteHashMultiSetLP.ContainsAll(constref a: array of T): Boolean;
var
  v: T;
begin
  for v in a do
    if NonContains(v) then
      exit(False);
  Result := True;
end;

function TGLiteHashMultiSetLP.ContainsAll(e: IEnumerable): Boolean;
var
  v: T;
begin
  for v in e do
    if NonContains(v) then
      exit(False);
  Result := True;
end;

function TGLiteHashMultiSetLP.ContainsAll(constref aSet: TGLiteHashMultiSetLP): Boolean;
var
  p: PEntry;
begin
  if @aSet = @Self then
    exit(True);
  if (Count >= aSet.Count) and (EntryCount >= aSet.EntryCount) then
    begin
      for p in aSet.FTable do
        if GetKeyCount(p^.Key) < p^.Count then
          exit(False);
      Result := True;
    end
  else
    Result := False;
end;

procedure TGLiteHashMultiSetLP.Add(constref aValue: T);
var
  p: PEntry;
begin
{$PUSH}{$Q+}
  Inc(FCount);
{$POP}
  if FindOrAdd(aValue, p) then
    Inc(p^.Count);
end;

function TGLiteHashMultiSetLP.AddAll(constref a: array of T): SizeInt;
var
  v: T;
begin
  Result := 0;
  for v in a do
    begin
      Add(v);
      Inc(Result);
    end;
end;

function TGLiteHashMultiSetLP.AddAll(e: IEnumerable): SizeInt;
var
  v: T;
begin
  Result := 0;
  for v in e do
    begin
      Add(v);
      Inc(Result);
    end;
end;

function TGLiteHashMultiSetLP.AddAll(constref aSet: TGLiteHashMultiSetLP): SizeInt;
begin
  Result := Count;
  Join(aSet);
  Result := Count - Result;
end;

function TGLiteHashMultiSetLP.Remove(constref aValue: T): Boolean;
begin
  Result := Extract(aValue);
end;

function TGLiteHashMultiSetLP.RemoveAll(constref a: array of T): SizeInt;
var
  v: T;
begin
  Result := 0;
  for v in a do
    Result += Ord(Remove(v));
end;

function TGLiteHashMultiSetLP.RemoveAll(e: IEnumerable): SizeInt;
var
  v: T;
begin
  Result := 0;
  for v in e do
    Result += Ord(Remove(v));
end;

function TGLiteHashMultiSetLP.RemoveAll(constref aSet: TGLiteHashMultiSetLP): SizeInt;
begin
  Result := Count;
  SymmetricSubtract(aSet);
  Result := Result - Count;
end;

function TGLiteHashMultiSetLP.RemoveIf(aTest: TTest): SizeInt;
var
  p: PEntry;
begin
  Result := 0;
  with FTable.GetRemovableEnumerator do
    while MoveNext do
      begin
        p := Current;
        if aTest(p^.Key) then
          begin
            FCount -= p^.Count;
            Result += p^.Count;
            RemoveCurrent;
          end
      end;
end;

function TGLiteHashMultiSetLP.RemoveIf(aTest: TOnTest): SizeInt;
var
  p: PEntry;
begin
  Result := 0;
  with FTable.GetRemovableEnumerator do
    while MoveNext do
      begin
        p := Current;
        if aTest(p^.Key) then
          begin
            FCount -= p^.Count;
            Result += p^.Count;
            RemoveCurrent;
          end
      end;
end;

function TGLiteHashMultiSetLP.RemoveIf(aTest: TNestTest): SizeInt;
var
  p: PEntry;
begin
  Result := 0;
  with FTable.GetRemovableEnumerator do
    while MoveNext do
      begin
        p := Current;
        if aTest(p^.Key) then
          begin
            FCount -= p^.Count;
            Result += p^.Count;
            RemoveCurrent;
          end
      end;
end;

function TGLiteHashMultiSetLP.Extract(constref aValue: T): Boolean;
var
  p: PEntry;
  Pos: SizeInt;
begin
  p := FTable.Find(aValue, Pos);
  Result := p <> nil;
  if Result then
    begin
      Dec(p^.Count);
      Dec(FCount);
      if p^.Count = 0 then
        FTable.RemoveAt(Pos);
    end;
end;

function TGLiteHashMultiSetLP.ExtractIf(aTest: TTest): TArray;
var
  I, Last: SizeInt;
  p: PEntry;
  v: T;
begin
  System.SetLength(Result, ARRAY_INITIAL_SIZE);
  I := 0;
  with FTable.GetRemovableEnumerator do
    while MoveNext do
      begin
        p := Current;
        if aTest(p^.Key) then
          begin
            Last := Pred(I + p^.Count);
            FCount -= p^.Count;
            v := p^.Key;
            if Last >= System.Length(Result) then
                System.SetLength(Result, RoundUpTwoPower(Succ(Last)));
            for I := I to Last do
              Result[I] := v;
            RemoveCurrent;
            I := Succ(Last);
          end;
      end;
  System.SetLength(Result, I);
end;

function TGLiteHashMultiSetLP.ExtractIf(aTest: TOnTest): TArray;
var
  I, Last: SizeInt;
  p: PEntry;
  v: T;
begin
  System.SetLength(Result, ARRAY_INITIAL_SIZE);
  I := 0;
  with FTable.GetRemovableEnumerator do
    while MoveNext do
      begin
        p := Current;
        if aTest(p^.Key) then
          begin
            Last := Pred(I + p^.Count);
            FCount -= p^.Count;
            v := p^.Key;
            if Last >= System.Length(Result) then
                System.SetLength(Result, RoundUpTwoPower(Succ(Last)));
            for I := I to Last do
              Result[I] := v;
            RemoveCurrent;
            I := Succ(Last);
          end;
      end;
  System.SetLength(Result, I);
end;

function TGLiteHashMultiSetLP.ExtractIf(aTest: TNestTest): TArray;
var
  I, Last: SizeInt;
  p: PEntry;
  v: T;
begin
  System.SetLength(Result, ARRAY_INITIAL_SIZE);
  I := 0;
  with FTable.GetRemovableEnumerator do
    while MoveNext do
      begin
        p := Current;
        if aTest(p^.Key) then
          begin
            Last := Pred(I + p^.Count);
            FCount -= p^.Count;
            v := p^.Key;
            if Last >= System.Length(Result) then
                System.SetLength(Result, RoundUpTwoPower(Succ(Last)));
            for I := I to Last do
              Result[I] := v;
            RemoveCurrent;
            I := Succ(Last);
          end;
      end;
  System.SetLength(Result, I);
end;

procedure TGLiteHashMultiSetLP.RetainAll(aCollection: ICollection);
begin
  RemoveIf(@aCollection.NonContains);
end;

function TGLiteHashMultiSetLP.IsSuperMultiSet(constref aSet: TGLiteHashMultiSetLP): Boolean;
begin
  Result := ContainsAll(aSet);
end;

function TGLiteHashMultiSetLP.IsSubMultiSet(constref aSet: TGLiteHashMultiSetLP): Boolean;
begin
  Result := aSet.ContainsAll(Self);
end;

function TGLiteHashMultiSetLP.IsEqual(constref aSet: TGLiteHashMultiSetLP): Boolean;
var
  p: PEntry;
begin
  if @aSet = @Self then
    exit(True);
  if (aSet.Count = Count) and (aSet.EntryCount = EntryCount) then
    begin
      for p in FTable do
        if aSet[p^.Key] <> p^.Count then
          exit(False);
      Result := True;
    end
  else
    Result := False;
end;

function TGLiteHashMultiSetLP.Intersecting(constref aSet: TGLiteHashMultiSetLP): Boolean;
var
  p: PEntry;
begin
  if @aSet = @Self then
    exit(True);
  for p in FTable do
    if aSet.Contains(p^.Key) then
      exit(True);
  Result := False;
end;

procedure TGLiteHashMultiSetLP.Intersect(constref aSet: TGLiteHashMultiSetLP);
var
  cnt: SizeInt;
  p: PEntry;
begin
  if @aSet <> @Self then
    with FTable.GetRemovableEnumerator do
      while MoveNext do
        begin
          p := Current;
          cnt := aSet[p^.Key];
          if cnt <> 0 then
            begin
              if cnt < p^.Count then
                begin
                  FCount -= p^.Count - cnt;
                  p^.Count := cnt;
                end;
            end
          else
            begin
              FCount -= p^.Count;
              RemoveCurrent;
            end;
        end;
end;

procedure TGLiteHashMultiSetLP.Join(constref aSet: TGLiteHashMultiSetLP);
var
  p, ps: PEntry;
begin
{$PUSH}{$Q+}
  if @aSet = @Self then
    begin
      for p in FTable do
        p^.Count += p^.Count;
        FCount += FCount;
      exit;
    end;
  for ps in aSet.FTable do
    if not FindOrAdd(ps^.Key, p) then
      begin
        p^.Count := ps^.Count;
        FCount += ps^.Count;
      end
    else
      if ps^.Count > p^.Count then
        begin
          FCount += ps^.Count - p^.Count;
          p^.Count := ps^.Count;
        end;
{$POP}
end;

procedure TGLiteHashMultiSetLP.ArithmeticAdd(constref aSet: TGLiteHashMultiSetLP);
var
  p, ps: PEntry;
begin
{$PUSH}{$Q+}
  if @aSet <> @Self then
    for ps in aSet.FTable do
      begin
        FCount += ps^.Count;
        if FindOrAdd(ps^.Key, p) then
          p^.Count += ps^.Count
        else
          begin
            p^.Key := ps^.Key;
            p^.Count := ps^.Count;
          end;
      end
  else
    begin
      FCount += FCount;
      for p in FTable do
        p^.Count += p^.Count;
    end;
{$POP}
end;

procedure TGLiteHashMultiSetLP.ArithmeticSubtract(constref aSet: TGLiteHashMultiSetLP);
var
  p, ps: PEntry;
  Pos: SizeInt;
begin
  if @aSet <> @Self then
    for ps in aSet.FTable do
      begin
        p := FTable.Find(ps^.Key, Pos);
        if p <> nil then
          begin
            if ps^.Count < p^.Count then
              begin
                FCount -= ps^.Count;
                p^.Count -= ps^.Count;
              end
            else
              begin
                FCount -= p^.Count;
                FTable.RemoveAt(Pos);
              end;
          end;
      end
  else
    Clear;
end;

procedure TGLiteHashMultiSetLP.SymmetricSubtract(constref aSet: TGLiteHashMultiSetLP);
var
  p, ps: PEntry;
  Pos: SizeInt;
begin
  if @aSet <> @Self then
    for ps in aSet.FTable do
      begin
        if FTable.FindOrAdd(ps^.Key, p, Pos) then
          begin
            if p^.Count > ps^.Count then
              begin
                FCount -= ps^.Count;
                p^.Count -= ps^.Count;
              end
            else
              if p^.Count < ps^.Count then
                begin
                 {$PUSH}{$Q+}
                  FCount -= p^.Count shl 1 - ps^.Count;
                 {$POP}
                  p^.Count := ps^.Count - p^.Count;
                end
              else  // counts equals
                begin
                  FCount -= p^.Count;
                  FTable.RemoveAt(Pos);
                end;
          end
        else
          begin
            p^.Key := ps^.Key;
            p^.Count := ps^.Count;
            {$PUSH}{$Q+}
            FCount += ps^.Count;
            {$POP}
          end;
      end
  else
    Clear;
end;

end.

