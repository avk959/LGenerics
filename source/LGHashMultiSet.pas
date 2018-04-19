{****************************************************************************
*         _     ____ _____ _   _ _____ ____  ___ ____ ____                  *
*        | |   / ___| ____| \ | | ____|  _ \|_ _/ ___/ ___|                 *
*        | |  | |  _|  _| |  \| |  _| | |_) || | |   \___ \                 *
*        | |__| |_| | |___| |\  | |___|  _ < | | |___ ___) |                *
*        |_____\____|_____|_| \_|_____|_| \_\___\____|____/                 *
*                                                                           *
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
*                   _         __     ___           __                       *
*            /\  /\/_\/\   /\/__\   / __\/\ /\  /\ \ \                      *
*           / /_/ //_\\ \ / /_\    / _\ / / \ \/  \/ /                      *
*          / __  /  _  \ V //__   / /   \ \_/ / /\  /                       *
*          \/ /_/\_/ \_/\_/\__/   \/     \___/\_\ \/                        *
*****************************************************************************}
unit LGHashMultiSet;

{$mode objfpc}{$H+}
{$INLINE ON}
{$MODESWITCH NESTEDPROCVARS}

interface

uses

  SysUtils,
  LGUtils,
  {%H-}LGHelpers,
  LGCustomContainer,
  LGHashTable;

type

  { TGCustomHashMultiSet: common abstract ancestor hash multiset class }
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
    function  GetTableSize: SizeInt; inline;
  { The capacity of the hash multiset is treated as the number of entries that can be written without rehashing }
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
    property  TableSize: SizeInt read GetTableSize;
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

  { TGObjectHashMultiSetLP }

  generic TGObjectHashMultiSetLP<T: class; TEqRel> = class(specialize TGCustomObjectHashMultiSet<T>)
  protected
    class function GetTableClass: THashTableClass; override;
    class function GetClass: THashMultiSetClass; override;
  public
    function Clone: TGObjectHashMultiSetLP; override;
  end;

  generic TGObjHashMultiSetLP<T: class> = class(specialize TGObjectHashMultiSetLP<T, T>);

  { TGObjectHashMultiSetLPT }

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

  { TGObjectHashMultiSetQP }

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

  { TGObjectChainHashMultiSet }

  generic TGObjectChainHashMultiSet<T: class; TEqRel> = class(specialize TGCustomObjectHashMultiSet<T>)
  protected
    class function GetTableClass: THashTableClass; override;
    class function GetClass: THashMultiSetClass; override;
  public
    function Clone: TGObjectChainHashMultiSet; override;
  end;

  generic TGObjChainHashMultiSet<T: class> = class(specialize TGObjectChainHashMultiSet<T, T>);

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

function TGCustomHashMultiSet.GetTableSize: SizeInt;
begin
  Result := FTable.TableSize;
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
  if not FTable.EnsureCapacity(aValue) then
    CapacityExceedError(aValue);
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
  if aMultiSet.ClassType = ClassType then
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

end.

