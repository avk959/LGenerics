{****************************************************************************
*         _     ____ _____ _   _ _____ ____  ___ ____ ____                  *
*        | |   / ___| ____| \ | | ____|  _ \|_ _/ ___/ ___|                 *
*        | |  | |  _|  _| |  \| |  _| | |_) || | |   \___ \                 *
*        | |__| |_| | |___| |\  | |___|  _ < | | |___ ___) |                *
*        |_____\____|_____|_| \_|_____|_| \_\___\____|____/                 *
*                                                                           *
*   This file is part of the LGenerics package.                             *
*   Generic hashmap implementations.                                        *
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
unit LGHashMap;

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

  { TGCustomHashMap: common abstract ancestor class to implement hashmap }
  generic TGCustomHashMap<TKey, TValue> = class abstract(specialize TGCustomMap<TKey, TValue>)
  protected
  type
    TCustomHashMap       = specialize TGCustomHashMap<TKey, TValue>;
    THashMapClass        = class of TCustomHashMap;
    THashTable           = specialize TGCustomHashTable<TKey, TEntry>;
    THashTableClass      = class of THashTable;
    TSearchResult        = THashTable.TSearchResult;

    TKeyEnumerable = class(TCustomKeyEnumerable)
    protected
      FEnum: THashTable.TEntryEnumerator;
      function  GetCurrent: TKey; override;
    public
      constructor Create(aMap: TCustomHashMap);
      destructor Destroy; override;
      function  MoveNext: Boolean; override;
      procedure Reset; override;
    end;

    TValueEnumerable = class(TCustomValueEnumerable)
    protected
      FEnum: THashTable.TEntryEnumerator;
      function  GetCurrent: TValue; override;
    public
      constructor Create(aMap: TCustomHashMap);
      destructor Destroy; override;
      function  MoveNext: Boolean; override;
      procedure Reset; override;
    end;

    TEntryEnumerable = class(TCustomEntryEnumerable)
    protected
      FEnum: THashTable.TEntryEnumerator;
      function  GetCurrent: TEntry; override;
    public
      constructor Create(aMap: TCustomHashMap);
      destructor Destroy; override;
      function  MoveNext: Boolean; override;
      procedure Reset; override;
    end;

  var
    FTable: THashTable;
    function  GetCount: SizeInt; override;
  { The capacity of the hashmap is treated as the number of entries that can be written without rehashing }
    function  GetCapacity: SizeInt; override;
    function  GetFillRatio: Single;
    function  GetLoadFactor: Single;
    procedure SetLoadFactor(aValue: Single); inline;
    function  GetTableSize: SizeInt; inline;
    function  Find(constref aKey: TKey): PEntry; override;
    //return True if aKey found, otherwise insert (garbage) pair and return False;
    function  FindOrAdd(constref aKey: TKey; out p: PEntry): Boolean; override;
    function  DoExtract(constref aKey: TKey; out v: TValue): Boolean; override;
    function  DoRemoveIf(aTest: TKeyTest): SizeInt; override;
    function  DoRemoveIf(aTest: TOnKeyTest): SizeInt; override;
    function  DoRemoveIf(aTest: TNestKeyTest): SizeInt; override;
    function  DoExtractIf(aTest: TKeyTest): TEntryArray; override;
    function  DoExtractIf(aTest: TOnKeyTest): TEntryArray; override;
    function  DoExtractIf(aTest: TNestKeyTest): TEntryArray; override;
    procedure DoClear; override;
    procedure DoEnsureCapacity(aValue: SizeInt); override;
    procedure DoTrimToFit; override;
    function  GetKeys: IKeyEnumerable; override;
    function  GetValues: IValueEnumerable; override;
    function  GetEntries: IEntryEnumerable; override;
    class function GetTableClass: THashTableClass; virtual; abstract;
    class function GetClass: THashMapClass; virtual; abstract;
  public
    class function DefaultLoadFactor: Single; inline;
    class function MaxLoadFactor: Single; inline;
    class function MinLoadFactor: Single; inline;
    constructor Create;
    constructor Create(constref a: array of TEntry);
    constructor Create(e: IEntryEnumerable);
    constructor Create(aCapacity: SizeInt);
    constructor Create(aCapacity: SizeInt; constref a: array of TEntry);
    constructor Create(aCapacity: SizeInt; e: IEntryEnumerable);
    constructor Create(aLoadFactor: Double);
    constructor Create(aLoadFactor: Single; constref a: array of TEntry);
    constructor Create(aLoadFactor: Single; e: IEntryEnumerable);
    constructor Create(aCapacity: SizeInt; aLoadFactor: Single);
    constructor Create(aCapacity: SizeInt; aLoadFactor: Single; constref a: array of TEntry);
    constructor Create(aCapacity: SizeInt; aLoadFactor: Single; e: IEntryEnumerable);
    constructor CreateCopy(aMap: TCustomHashMap);
    destructor Destroy; override;
    function  Clone: TCustomHashMap; override;
    property  LoadFactor: Single read GetLoadFactor write SetLoadFactor;
    property  FillRatio: Single read GetFillRatio;
    property  TableSize: SizeInt read GetTableSize;
  end;

  { TGBaseHashMapLP implements open addressing hashmap with linear probing;
      TKeyEqRel must provide:
        class function HashCode([const[ref]] aValue: TKey): SizeInt;
        class function Equal([const[ref]] L, R: TKey): Boolean; }
  generic TGBaseHashMapLP<TKey, TValue, TKeyEqRel> = class(specialize TGCustomHashMap<TKey, TValue>)
  protected
    class function GetTableClass: THashTableClass; override;
    class function GetClass: THashMapClass; override;
  end;

  { TGHashMapLP implements open addressing hashmap with linear probing;
    it assumes that type TKey implements TKeyEqRel }
  generic TGHashMapLP<TKey, TValue> = class(specialize TGBaseHashMapLP<TKey, TValue, TKey>);

  { TGBaseHashMapLPT implements open addressing hashmap with linear probing and lazy deletion }
  generic TGBaseHashMapLPT<TKey, TValue, TKeyEqRel> = class(specialize TGCustomHashMap<TKey, TValue>)
  private
    function GetTombstonesCount: SizeInt; inline;
  protected
  type
    TTableLPT = specialize TGOpenAddrLPT<TKey, TEntry, TKeyEqRel>;

    class function GetTableClass: THashTableClass; override;
    class function GetClass: THashMapClass; override;
  public
    procedure ClearTombstones; inline;
    property  TombstonesCount: SizeInt read GetTombstonesCount;
  end;

  { TGHashMapLPT implements open addressing hashmap with linear probing and lazy deletion;
    it assumes that type TKey implements TKeyEqRel }
  generic TGHashMapLPT<TKey, TValue> = class(specialize TGBaseHashMapLPT<TKey, TValue, TKey>);

  { TGBaseHashMapQP implements open addressing hashmap with quadratic probing(c1 = c2 = 1/2)}
  generic TGBaseHashMapQP<TKey, TValue, TKeyEqRel> = class(specialize TGCustomHashMap<TKey, TValue>)
  private
    function GetTombstonesCount: SizeInt; inline;
  protected
  type
    TTableQP = specialize TGOpenAddrQP<TKey, TEntry, TKeyEqRel>;

    class function GetTableClass: THashTableClass; override;
    class function GetClass: THashMapClass; override;
  public
    procedure ClearTombstones; inline;
    property  TombstonesCount: SizeInt read GetTombstonesCount;
  end;

  { TGHashMapQP implements open addressing hashmap with quadratic probing(c1 = c2 = 1/2);
    it assumes that type TKey implements TKeyEqRel }
  generic TGHashMapQP<TKey, TValue> = class(specialize TGBaseHashMapQP<TKey, TValue, TKey>);

  { TGBaseChainHashMap implements node based hashmap with singly linked list chains }
  generic TGBaseChainHashMap<TKey, TValue, TKeyEqRel> = class(specialize TGCustomHashMap<TKey, TValue>)
  protected
    class function GetTableClass: THashTableClass; override;
    class function GetClass: THashMapClass; override;
  end;

  { TGChainHashMap implements node based hashmap with singly linked list chains;
    it assumes that type TKey implements TKeyEqRel }
  generic TGChainHashMap<TKey, TValue> = class(specialize TGBaseChainHashMap<TKey, TValue, TKey>);

  { TGCustomObjectHashMap
      note: for equality comparision of (TValue as TObject) used TObjectHelper from LGHelpers }
  generic TGCustomObjectHashMap<TKey, TValue> = class abstract(specialize TGCustomHashMap<TKey, TValue>)
  private
    FOwnsKeys: Boolean;
    FOwnsValues: Boolean;
  protected
  type
    TObjectHashMapClass = class of TGCustomObjectHashMap;

    procedure EntryRemoving(p: PEntry);
    procedure SetOwnership(aOwns: TMapObjOwnership); inline;
    function  DoRemove(constref aKey: TKey): Boolean; override;
    function  DoRemoveIf(aTest: TKeyTest): SizeInt; override;
    function  DoRemoveIf(aTest: TOnKeyTest): SizeInt; override;
    function  DoRemoveIf(aTest: TNestKeyTest): SizeInt; override;
    procedure DoClear; override;
    function  DoSetValue(constref aKey: TKey; constref aNewValue: TValue): Boolean; override;
    function  DoAddOrSetValue(const aKey: TKey; const aValue: TValue): Boolean; override;
    class function GetClass: TObjectHashMapClass; reintroduce; virtual; abstract;
  public
    constructor Create(aOwns: TMapObjOwnership = OWNS_BOTH);
    constructor Create(constref a: array of TEntry; aOwns: TMapObjOwnership = OWNS_BOTH);
    constructor Create(e: IEntryEnumerable; aOwns: TMapObjOwnership = OWNS_BOTH);
    constructor Create(aCapacity: SizeInt; aOwns: TMapObjOwnership = OWNS_BOTH);
    constructor Create(aCapacity: SizeInt; constref a: array of TEntry; aOwns: TMapObjOwnership = OWNS_BOTH);
    constructor Create(aCapacity: SizeInt; e: IEntryEnumerable; aOwns: TMapObjOwnership = OWNS_BOTH);
    constructor Create(aLoadFactor: Single; aOwns: TMapObjOwnership = OWNS_BOTH);
    constructor Create(aLoadFactor: Single; constref a: array of TEntry; aOwns: TMapObjOwnership = OWNS_BOTH);
    constructor Create(aLoadFactor: Single; e: IEntryEnumerable; aOwns: TMapObjOwnership = OWNS_BOTH);
    constructor Create(aCapacity: SizeInt; aLoadFactor: Single; aOwns: TMapObjOwnership = OWNS_BOTH);
    constructor Create(aCapacity: SizeInt; aLoadFactor: Single; constref a: array of TEntry;
                       aOwns: TMapObjOwnership = OWNS_BOTH);
    constructor Create(aCapacity: SizeInt; aLoadFactor: Single; e: IEntryEnumerable;
                       aOwns: TMapObjOwnership = OWNS_BOTH);
    constructor CreateCopy(aMap: TGCustomObjectHashMap);
    property  OwnsKeys: Boolean read FOwnsKeys write FOwnsKeys;
    property  OwnsValues: Boolean read FOwnsValues write FOwnsValues;
  end;


  { TGObjectHashMapLP }

  generic TGObjectHashMapLP<TKey, TValue, TKeyEqRel> = class(specialize TGCustomObjectHashMap<TKey, TValue>)
  protected
    class function GetTableClass: THashTableClass; override;
    class function GetClass: TObjectHashMapClass; override;
  public
    function Clone: TGObjectHashMapLP; override;
  end;

  generic TGObjHashMapLP<TKey, TValue> = class(specialize TGObjectHashMapLP<TKey, TValue, TKey>);

  { TGObjectHashMapLPT }

  generic TGObjectHashMapLPT<TKey, TValue, TKeyEqRel> = class(specialize TGCustomObjectHashMap<TKey, TValue>)
  private
    function GetTombstonesCount: SizeInt; inline;
  protected
  type
    TTableLPT = specialize TGOpenAddrLPT<TKey, TEntry, TKeyEqRel>;

    class function GetTableClass: THashTableClass; override;
    class function GetClass: TObjectHashMapClass; override;
  public
    function  Clone: TGObjectHashMapLPT; override;
    procedure ClearTombstones; inline;
    property  TombstonesCount: SizeInt read GetTombstonesCount;
  end;

  generic TGObjHashMapLPT<TKey, TValue> = class(specialize TGObjectHashMapLPT<TKey, TValue, TKey>);

  { TGObjectHashMapQP }

  generic TGObjectHashMapQP<TKey, TValue, TKeyEqRel> = class(specialize TGCustomObjectHashMap<TKey, TValue>)
  private
    function GetTombstonesCount: SizeInt; inline;
  protected
  type
    TTableQP = specialize TGOpenAddrQP<TKey, TEntry, TKeyEqRel>;

    class function GetTableClass: THashTableClass; override;
    class function GetClass: TObjectHashMapClass; override;
  public
    function  Clone: TGObjectHashMapQP; override;
    procedure ClearTombstones; inline;
    property  TombstonesCount: SizeInt read GetTombstonesCount;
  end;

  generic TGObjHashMapQP<TKey, TValue> = class(specialize TGObjectHashMapQP<TKey, TValue, TKey>);

  { TGObjectChainHashMap }

  generic TGObjectChainHashMap<TKey, TValue, TKeyEqRel> = class(specialize TGCustomObjectHashMap<TKey, TValue>)
  protected
    class function GetTableClass: THashTableClass; override;
    class function GetClass: TObjectHashMapClass; override;
  public
    function Clone: TGObjectChainHashMap; override;
  end;

  generic TGObjChainHashMap<TKey, TValue> = class(specialize TGObjectChainHashMap<TKey, TValue, TKey>);

implementation
{$B-}{$COPERATORS ON}

{ TGCustomHashMap.TKeyEnumerable }

function TGCustomHashMap.TKeyEnumerable.GetCurrent: TKey;
begin
  Result := FEnum.Current^.Key;
end;

constructor TGCustomHashMap.TKeyEnumerable.Create(aMap: TCustomHashMap);
begin
  inherited Create(aMap);
  FEnum := aMap.FTable.GetEnumerator;
end;

destructor TGCustomHashMap.TKeyEnumerable.Destroy;
begin
  FEnum.Free;
  inherited;
end;

function TGCustomHashMap.TKeyEnumerable.MoveNext: Boolean;
begin
  Result := FEnum.MoveNext;
end;

procedure TGCustomHashMap.TKeyEnumerable.Reset;
begin
  FEnum.Reset;
end;

{ TGCustomHashMap.TValueEnumerable }

function TGCustomHashMap.TValueEnumerable.GetCurrent: TValue;
begin
  Result := FEnum.Current^.Value;
end;

constructor TGCustomHashMap.TValueEnumerable.Create(aMap: TCustomHashMap);
begin
  inherited Create(aMap);
  FEnum := aMap.FTable.GetEnumerator;
end;

destructor TGCustomHashMap.TValueEnumerable.Destroy;
begin
  FEnum.Free;
  inherited;
end;

function TGCustomHashMap.TValueEnumerable.MoveNext: Boolean;
begin
  Result := FEnum.MoveNext;
end;

procedure TGCustomHashMap.TValueEnumerable.Reset;
begin
  FEnum.Reset;
end;

{ TGCustomHashMap.TEntryEnumerable }

function TGCustomHashMap.TEntryEnumerable.GetCurrent: TEntry;
begin
  Result := FEnum.Current^;
end;

constructor TGCustomHashMap.TEntryEnumerable.Create(aMap: TCustomHashMap);
begin
  inherited Create(aMap);
  FEnum := aMap.FTable.GetEnumerator;
end;

destructor TGCustomHashMap.TEntryEnumerable.Destroy;
begin
  FEnum.Free;
  inherited;
end;

function TGCustomHashMap.TEntryEnumerable.MoveNext: Boolean;
begin
  Result := FEnum.MoveNext;
end;

procedure TGCustomHashMap.TEntryEnumerable.Reset;
begin
  FEnum.Reset;
end;

{ TGCustomHashMap }

function TGCustomHashMap.GetCount: SizeInt;
begin
  Result := FTable.Count;
end;

function TGCustomHashMap.GetCapacity: SizeInt;
begin
  Result := FTable.Capacity;
end;

function TGCustomHashMap.GetFillRatio: Single;
begin
  Result := FTable.FillRatio;
end;

function TGCustomHashMap.GetLoadFactor: Single;
begin
  Result := FTable.LoadFactor;
end;

procedure TGCustomHashMap.SetLoadFactor(aValue: Single);
begin
  FTable.LoadFactor := aValue;
end;

function TGCustomHashMap.GetTableSize: SizeInt;
begin
  Result := FTable.TableSize;
end;

function TGCustomHashMap.Find(constref aKey: TKey): PEntry;
var
  sr: TSearchResult;
begin
  Result := FTable.Find(aKey, sr);
end;

function TGCustomHashMap.FindOrAdd(constref aKey: TKey; out p: PEntry): Boolean;
var
  sr: TSearchResult;
begin
  Result := FTable.FindOrAdd(aKey, p, sr);
  if not Result then
    p^.Key := aKey;
end;

function TGCustomHashMap.DoExtract(constref aKey: TKey; out v: TValue): Boolean;
var
  p: PEntry;
  sr: TSearchResult;
begin
  p := FTable.Find(aKey, sr);
  Result := p <> nil;
  if Result then
    begin
      v := p^.Value;
      FTable.RemoveAt(sr);
    end;
end;

function TGCustomHashMap.DoRemoveIf(aTest: TKeyTest): SizeInt;
begin
  Result := FTable.RemoveIf(aTest);
end;

function TGCustomHashMap.DoRemoveIf(aTest: TOnKeyTest): SizeInt;
begin
  Result := FTable.RemoveIf(aTest);
end;

function TGCustomHashMap.DoRemoveIf(aTest: TNestKeyTest): SizeInt;
begin
  Result := FTable.RemoveIf(aTest);
end;

function TGCustomHashMap.DoExtractIf(aTest: TKeyTest): TEntryArray;
var
  eh: TExtractHelper;
begin
  eh.Init;
  FTable.RemoveIf(aTest, @eh.OnExtract);
  Result := eh.Final;
end;

function TGCustomHashMap.DoExtractIf(aTest: TOnKeyTest): TEntryArray;
var
  e: TExtractHelper;
begin
  e.Init;
  FTable.RemoveIf(aTest, @e.OnExtract);
  Result := e.Final;
end;

function TGCustomHashMap.DoExtractIf(aTest: TNestKeyTest): TEntryArray;
var
  e: TExtractHelper;
begin
  e.Init;
  FTable.RemoveIf(aTest, @e.OnExtract);
  Result := e.Final;
end;

procedure TGCustomHashMap.DoClear;
begin
  FTable.Clear;
end;

procedure TGCustomHashMap.DoEnsureCapacity(aValue: SizeInt);
begin
  if not FTable.EnsureCapacity(aValue) then
    CapacityExceedError(aValue);
end;

procedure TGCustomHashMap.DoTrimToFit;
begin
  FTable.TrimToFit;
end;

function TGCustomHashMap.GetKeys: IKeyEnumerable;
begin
  Result := TKeyEnumerable.Create(Self);
end;

function TGCustomHashMap.GetValues: IValueEnumerable;
begin
  Result := TValueEnumerable.Create(Self);
end;

function TGCustomHashMap.GetEntries: IEntryEnumerable;
begin
  Result := TEntryEnumerable.Create(Self);
end;

class function TGCustomHashMap.DefaultLoadFactor: Single;
begin
  Result := GetTableClass.DefaultLoadFactor;
end;

class function TGCustomHashMap.MaxLoadFactor: Single;
begin
  Result := GetTableClass.MaxLoadFactor;
end;

class function TGCustomHashMap.MinLoadFactor: Single;
begin
  Result := GetTableClass.MinLoadFactor;
end;

constructor TGCustomHashMap.Create;
begin
  FTable := GetTableClass.Create;
end;

constructor TGCustomHashMap.Create(constref a: array of TEntry);
begin
  FTable := GetTableClass.Create;
  DoAddAll(a);
end;

constructor TGCustomHashMap.Create(e: IEntryEnumerable);
begin
  FTable := GetTableClass.Create;
  DoAddAll(e);
end;

constructor TGCustomHashMap.Create(aCapacity: SizeInt);
begin
  FTable := GetTableClass.Create(aCapacity);
end;

constructor TGCustomHashMap.Create(aCapacity: SizeInt; constref a: array of TEntry);
begin
  FTable := GetTableClass.Create(aCapacity);
  DoAddAll(a);
end;

constructor TGCustomHashMap.Create(aCapacity: SizeInt; e: IEntryEnumerable);
begin
  FTable := GetTableClass.Create(aCapacity);
  DoAddAll(e);
end;

constructor TGCustomHashMap.Create(aLoadFactor: Double);
begin
  FTable := GetTableClass.Create(aLoadFactor);
end;

constructor TGCustomHashMap.Create(aLoadFactor: Single; constref a: array of TEntry);
begin
  FTable := GetTableClass.Create(aLoadFactor);
  DoAddAll(a);
end;

constructor TGCustomHashMap.Create(aLoadFactor: Single; e: IEntryEnumerable);
begin
  FTable := GetTableClass.Create(aLoadFactor);
  DoAddAll(e);
end;

constructor TGCustomHashMap.Create(aCapacity: SizeInt; aLoadFactor: Single);
begin
  FTable := GetTableClass.Create(aCapacity, aLoadFactor);
end;

constructor TGCustomHashMap.Create(aCapacity: SizeInt; aLoadFactor: Single; constref a: array of TEntry);
begin
  FTable := GetTableClass.Create(aCapacity, aLoadFactor);
  DoAddAll(a);
end;

constructor TGCustomHashMap.Create(aCapacity: SizeInt; aLoadFactor: Single; e: IEntryEnumerable);
begin
  FTable := GetTableClass.Create(aCapacity, aLoadFactor);
  DoAddAll(e);
end;

constructor TGCustomHashMap.CreateCopy(aMap: TCustomHashMap);
begin
  inherited Create;
  if aMap.ClassType = ClassType then
    FTable := aMap.FTable.Clone
  else
    begin
      FTable := GetTableClass.Create(aMap.Count);
      DoAddAll(aMap.Entries);
    end;
end;

destructor TGCustomHashMap.Destroy;
begin
  DoClear;
  FTable.Free;
  inherited;
end;

function TGCustomHashMap.Clone: TCustomHashMap;
begin
  Result := GetClass.CreateCopy(Self);
end;

{ TGBaseHashMapLP }

class function TGBaseHashMapLP.GetTableClass: THashTableClass;
begin
  Result := specialize TGOpenAddrLP<TKey, TEntry, TKeyEqRel>;
end;

class function TGBaseHashMapLP.GetClass: THashMapClass;
begin
  Result := TGBaseHashMapLP;
end;

{ TGBaseHashMapLPT }

function TGBaseHashMapLPT.GetTombstonesCount: SizeInt;
begin
  Result := TTableLPT(FTable).TombstonesCount;
end;

class function TGBaseHashMapLPT.GetTableClass: THashTableClass;
begin
  Result := TTableLPT;
end;

class function TGBaseHashMapLPT.GetClass: THashMapClass;
begin
  Result := TGBaseHashMapLPT;
end;

procedure TGBaseHashMapLPT.ClearTombstones;
begin
  TTableLPT(FTable).ClearTombstones;
end;

{ TGBaseHashMapQP }

function TGBaseHashMapQP.GetTombstonesCount: SizeInt;
begin
  Result := TTableQP(FTable).TombstonesCount;
end;

class function TGBaseHashMapQP.GetTableClass: THashTableClass;
begin
  Result := TTableQP;
end;

class function TGBaseHashMapQP.GetClass: THashMapClass;
begin
  Result := TGBaseHashMapQP;
end;

procedure TGBaseHashMapQP.ClearTombstones;
begin
  TTableQP(FTable).ClearTombstones;
end;

{ TGBaseChainHashMap }

class function TGBaseChainHashMap.GetTableClass: THashTableClass;
begin
  Result := specialize TGChainHashTable<TKey, TEntry, TKeyEqRel>;
end;

class function TGBaseChainHashMap.GetClass: THashMapClass;
begin
  Result := TGBaseChainHashMap;
end;

{ TGCustomObjectHashMap }

procedure TGCustomObjectHashMap.EntryRemoving(p: PEntry);
begin
  if OwnsKeys then
    TObject(p^.Key).Free;
  if OwnsValues then
    TObject(p^.Value).Free;
end;

procedure TGCustomObjectHashMap.SetOwnership(aOwns: TMapObjOwnership);
begin
  OwnsKeys := moOwnsKeys in aOwns;
  OwnsValues := moOwnsValues in aOwns;
end;

function TGCustomObjectHashMap.DoRemove(constref aKey: TKey): Boolean;
var
  v: TValue;
begin
  Result := DoExtract(aKey, v);
  if Result then
    begin
      if OwnsKeys then
        TObject(aKey).Free;
      if OwnsValues then
        TObject(v).Free;
    end;
end;

function TGCustomObjectHashMap.DoRemoveIf(aTest: TKeyTest): SizeInt;
begin
  Result := FTable.RemoveIf(aTest, @EntryRemoving);
end;

function TGCustomObjectHashMap.DoRemoveIf(aTest: TOnKeyTest): SizeInt;
begin
  Result := FTable.RemoveIf(aTest, @EntryRemoving);
end;

function TGCustomObjectHashMap.DoRemoveIf(aTest: TNestKeyTest): SizeInt;
begin
  Result := FTable.RemoveIf(aTest, @EntryRemoving);
end;

procedure TGCustomObjectHashMap.DoClear;
var
  p: PEntry;
begin
  if OwnsKeys or OwnsValues then
    for p in FTable do
      begin
        if OwnsKeys then
          TObject(p^.Key).Free;
        if OwnsValues then
          TObject(p^.Value).Free;
      end;
  inherited;
end;

function TGCustomObjectHashMap.DoSetValue(constref aKey: TKey; constref aNewValue: TValue): Boolean;
var
  p: PEntry;
begin
  p := Find(aKey);
  Result := p <> nil;
  if Result then
    begin
      if OwnsValues and not TObject.Equal(TObject(p^.Value), TObject(aNewValue)) then
        TObject(p^.Value).Free;
      p^.Value := aNewValue;
    end;
end;

function TGCustomObjectHashMap.DoAddOrSetValue(const aKey: TKey; const aValue: TValue): Boolean;
var
  p: PEntry;
begin
  Result := not FindOrAdd(aKey, p);
  if not Result then
    begin
      if OwnsValues and not TObject.Equal(TObject(p^.Value), TObject(aValue)) then
        TObject(p^.Value).Free;
    end;
  p^.Value := aValue;
end;

constructor TGCustomObjectHashMap.Create(aOwns: TMapObjOwnership);
begin
  inherited Create;
  SetOwnership(aOwns);
end;

constructor TGCustomObjectHashMap.Create(constref a: array of TEntry; aOwns: TMapObjOwnership);
begin
  inherited Create(a);
  SetOwnership(aOwns);
end;

constructor TGCustomObjectHashMap.Create(e: IEntryEnumerable; aOwns: TMapObjOwnership);
begin
  inherited Create(e);
  SetOwnership(aOwns);
end;

constructor TGCustomObjectHashMap.Create(aCapacity: SizeInt; aOwns: TMapObjOwnership);
begin
  inherited Create(aCapacity);
  SetOwnership(aOwns);
end;

constructor TGCustomObjectHashMap.Create(aCapacity: SizeInt; constref a: array of TEntry;
  aOwns: TMapObjOwnership);
begin
  inherited Create(aCapacity, a);
  SetOwnership(aOwns);
end;

constructor TGCustomObjectHashMap.Create(aCapacity: SizeInt; e: IEntryEnumerable; aOwns: TMapObjOwnership);
begin
  inherited Create(aCapacity, e);
  SetOwnership(aOwns);
end;

constructor TGCustomObjectHashMap.Create(aLoadFactor: Single; aOwns: TMapObjOwnership);
begin
  inherited Create(aLoadFactor);
  SetOwnership(aOwns);
end;

constructor TGCustomObjectHashMap.Create(aLoadFactor: Single; constref a: array of TEntry;
  aOwns: TMapObjOwnership);
begin
  inherited Create(aLoadFactor, a);
  SetOwnership(aOwns);
end;

constructor TGCustomObjectHashMap.Create(aLoadFactor: Single; e: IEntryEnumerable; aOwns: TMapObjOwnership);
begin
  inherited Create(aLoadFactor, e);
  SetOwnership(aOwns);
end;

constructor TGCustomObjectHashMap.Create(aCapacity: SizeInt; aLoadFactor: Single; aOwns: TMapObjOwnership);
begin
  inherited Create(aCapacity, aLoadFactor);
  SetOwnership(aOwns);
end;

constructor TGCustomObjectHashMap.Create(aCapacity: SizeInt; aLoadFactor: Single; constref a: array of TEntry;
  aOwns: TMapObjOwnership);
begin
  inherited Create(aCapacity, aLoadFactor, a);
  SetOwnership(aOwns);
end;

constructor TGCustomObjectHashMap.Create(aCapacity: SizeInt; aLoadFactor: Single; e: IEntryEnumerable;
  aOwns: TMapObjOwnership);
begin
  inherited Create(aCapacity, aLoadFactor, e);
  SetOwnership(aOwns);
end;

constructor TGCustomObjectHashMap.CreateCopy(aMap: TGCustomObjectHashMap);
begin
  inherited CreateCopy(aMap);
  OwnsKeys := aMap.OwnsKeys;
  OwnsValues := aMap.OwnsValues;
end;

{ TGObjectHashMapLP }

class function TGObjectHashMapLP.GetTableClass: THashTableClass;
begin
  Result := specialize TGOpenAddrLP<TKey, TEntry, TKeyEqRel>;
end;

class function TGObjectHashMapLP.GetClass: TObjectHashMapClass;
begin
  Result := TGObjectHashMapLP;
end;

function TGObjectHashMapLP.Clone: TGObjectHashMapLP;
begin
  Result := TGObjectHashMapLP.CreateCopy(Self);
end;

{ TGObjectHashMapLPT }

function TGObjectHashMapLPT.GetTombstonesCount: SizeInt;
begin
  Result := TTableLPT(FTable).TombstonesCount;
end;

class function TGObjectHashMapLPT.GetTableClass: THashTableClass;
begin
  Result := TTableLPT;
end;

class function TGObjectHashMapLPT.GetClass: TObjectHashMapClass;
begin
  Result := TGObjectHashMapLPT;
end;

function TGObjectHashMapLPT.Clone: TGObjectHashMapLPT;
begin
  Result := TGObjectHashMapLPT.CreateCopy(Self);
end;

procedure TGObjectHashMapLPT.ClearTombstones;
begin
  TTableLPT(FTable).ClearTombstones;
end;

{ TGObjectHashMapQP }

function TGObjectHashMapQP.GetTombstonesCount: SizeInt;
begin
  Result := TTableQP(FTable).TombstonesCount;
end;

class function TGObjectHashMapQP.GetTableClass: THashTableClass;
begin
  Result := TTableQP;
end;

class function TGObjectHashMapQP.GetClass: TObjectHashMapClass;
begin
  Result := TGObjectHashMapQP;
end;

function TGObjectHashMapQP.Clone: TGObjectHashMapQP;
begin
  Result := TGObjectHashMapQP.CreateCopy(Self);
end;

procedure TGObjectHashMapQP.ClearTombstones;
begin
  TTableQP(FTable).ClearTombstones;
end;

{ TGObjectChainHashMap }

class function TGObjectChainHashMap.GetTableClass: THashTableClass;
begin
  Result := specialize TGChainHashTable<TKey, TEntry, TKeyEqRel>;
end;

class function TGObjectChainHashMap.GetClass: TObjectHashMapClass;
begin
  Result := TGObjectChainHashMap;
end;

function TGObjectChainHashMap.Clone: TGObjectChainHashMap;
begin
  Result := TGObjectChainHashMap.CreateCopy(Self);
end;

end.

