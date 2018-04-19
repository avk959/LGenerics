{****************************************************************************
*         _     ____ _____ _   _ _____ ____  ___ ____ ____                  *
*        | |   / ___| ____| \ | | ____|  _ \|_ _/ ___/ ___|                 *
*        | |  | |  _|  _| |  \| |  _| | |_) || | |   \___ \                 *
*        | |__| |_| | |___| |\  | |___|  _ < | | |___ ___) |                *
*        |_____\____|_____|_| \_|_____|_| \_\___\____|____/                 *
*                                                                           *
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
*                   _         __     ___           __                       *
*            /\  /\/_\/\   /\/__\   / __\/\ /\  /\ \ \                      *
*           / /_/ //_\\ \ / /_\    / _\ / / \ \/  \/ /                      *
*          / __  /  _  \ V //__   / /   \ \_/ / /\  /                       *
*          \/ /_/\_/ \_/\_/\__/   \/     \___/\_\ \/                        *
*****************************************************************************}
unit LGHashSet;

{$MODE OBJFPC}{$H+}
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
  { The capacity of the hashset is treated as the number of elements that can be written without rehashing }
    function  GetCapacity: SizeInt; override;
    function  GetFillRatio: Single; inline;
    function  GetLoadFactor: Single; inline;
    procedure SetLoadFactor(aValue: Single); inline;
    function  GetTableSize: SizeInt; inline;
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
    class function Distinct(a: TArray): IEnumerable; inline;
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
    property  TableSize: SizeInt read GetTableSize;
  end;

  { TGBaseHashSetLP implements open addressing hashset with linear probing;
      functor TEqRel(equality relation) must provide:
        class function HashCode([const[ref]] aValue: T): SizeInt;
        class function Equal([const[ref]] L, R: T): Boolean;}
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

function TGCustomHashSet.GetTableSize: SizeInt;
begin
  Result := FTable.TableSize;
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
  if not FTable.EnsureCapacity(aValue) then
    CapacityExceedError(aValue);
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

class function TGCustomHashSet.Distinct(a: TArray): IEnumerable;
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
  if aSet.ClassType = ClassType then
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

end.

