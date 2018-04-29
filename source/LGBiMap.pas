{****************************************************************************
*                                                                           *
*   This file is part of the LGenerics package.                             *
*   Generic bijective map implementation on top of hash table.              *
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
unit LGBiMap;

{$mode objfpc}{$H+}
{$INLINE ON}

interface

uses
  SysUtils,
  LGUtils,
  {%H-}LGHelpers,
  LGCustomContainer,
  LGHashTable,
  LGStrConst;

type

{$PUSH}{$INTERFACES CORBA}
  generic IGInverseMap<TKey, TValue> = interface
  ['{12C18769-51E6-4FA8-8AC7-904B46B86D9F}']
    function  _GetRef: TObject;
    function  GetCount: SizeInt;
    function  GetCapacity: SizeInt;
    function  GetValue(const aKey: TValue): TKey;
    function  IsEmpty: Boolean;
    function  NonEmpty: Boolean;
    procedure Clear;
    procedure EnsureCapacity(aValue: SizeInt);
  { free unused memory if possible }
    procedure TrimToFit;
  { returns True and add TEntry(aKey, aValue) only if not contains aKey }
    function  Add(constref aKey: TValue; constref aValue: TKey): Boolean; overload;
    procedure AddOrSetValue(const aKey: TValue; const aValue: TKey); overload;
    function  TryGetValue(constref aKey: TValue; out aValue: TKey): Boolean;
    function  GetValueDef(constref aKey: TValue; constref aDefault: TKey = Default(TKey)): TKey;
  { returns True and map aNewValue to aKey only if contains aKey, False otherwise }
    function  Replace(constref aKey: TValue; constref aNewValue: TKey): Boolean;
    function  Contains(constref aKey: TValue): Boolean;
    function  Extract(constref aKey: TValue; out v: TKey): Boolean;
    function  Remove(constref aKey: TValue): Boolean;
    procedure RetainAll(c: specialize IGCollection<TValue>);
    function  Keys: specialize IGEnumerable<TValue>;
    function  Values: specialize IGEnumerable<TKey>;
    property  Count: SizeInt read GetCount;
    property  Capacity: SizeInt read GetCapacity;
  { reading will raise ELGMapError if an aKey is not present in map }
    property  Items[const aKey: TValue]: TKey read GetValue write AddOrSetValue; default;
  end;
{$POP}

  { TGHashBiMap implements bijective map(i.e. a one-to-one correspondence between keys and values)
     on top of hashtable;

      functor TKeyEqRel(key equality relation) must provide:
        class function HashCode([const[ref]] k: TKey): SizeInt;
        class function Equal([const[ref]] L, R: TKey): Boolean;

      functor TValueEqRel(value equality relation) must provide:
        class function HashCode([const[ref]] v: TValue): SizeInt;
        class function Equal([const[ref]] L, R: TValue): Boolean;  }
  generic TGHashBiMap<TKey, TValue, TKeyEqRel, TValueEqRel> = class(TCustomIterable,
    specialize IGMap<TKey, TValue>, specialize IGInverseMap<TKey, TValue>)
  {must be  generic TGHashBiMap<TKey, TValue> = class abstract(
              specialize TGContainer<specialize TGMapEntry<TKey, TValue>>), but :( ... }
  public
  type
    THashBiMap       = specialize TGHashBiMap<TKey, TValue, TKeyEqRel, TValueEqRel>;
    TEntry           = specialize TGMapEntry<TKey, TValue>;
    TInverseEntry    = specialize TGMapEntry<TValue, TKey>;
    IKeyEnumerable   = specialize IGEnumerable<TKey>;
    IValueEnumerable = specialize IGEnumerable<TValue>;
    IEntryEnumerable = specialize IGEnumerable<TEntry>;
    TEntryArray      = specialize TGArray<TEntry>;
    TKeyCollection   = specialize TGCustomCollection<TKey>;
    TValueCollection = specialize TGCustomCollection<TValue>;
    IKeyCollection   = specialize IGCollection<TKey>;
    IValueCollection = specialize IGCollection<TValue>;
    IInverseMap      = specialize IGInverseMap<TKey, TValue>;
    EBiMapError      = class(Exception);

  protected
  type
    PKeyNode   = ^TKeyNode;
    PValueNode = ^TValueNode;

    TKeyEntry  = record
      Key: TKey;
      ValueNode: PValueNode;
    end;
    PKeyEntry = ^TKeyEntry;

    TValEntry = record
      Key: TValue;
      KeyNode: PKeyNode;
    end;
    PValEntry = ^TValEntry;

    TKeySet            = specialize TGChainHashTable<TKey, TKeyEntry, TKeyEqRel>;
    TKeySearchResult   = TKeySet.TSearchResult;
    TKeyNode           = TKeySet.TNode;
    TValueSet          = specialize TGChainHashTable<TValue, TValEntry, TValueEqRel>;
    TValueSearchResult = TValueSet.TSearchResult;
    TValueNode         = TValueSet.TNode;

    TKeyEnumerable = class(specialize TGAutoEnumerable<TKey>)
    protected
      FOwner: TGHashBiMap;
      FEnum:  TKeySet.TEntryEnumerator;
      function GetCurrent: TKey; override;
    public
      constructor Create(aMap: TGHashBiMap);
      destructor Destroy; override;
      function MoveNext: Boolean; override;
      procedure Reset; override;
    end;

    TValueEnumerable = class(specialize TGAutoEnumerable<TValue>)
    protected
      FOwner: TGHashBiMap;
      FEnum:  TValueSet.TEntryEnumerator;
      function GetCurrent: TValue; override;
    public
      constructor Create(aMap: TGHashBiMap);
      destructor Destroy; override;
      function MoveNext: Boolean; override;
      procedure Reset; override;
    end;

    TEntryEnumerable = class(specialize TGAutoEnumerable<TEntry>)
    protected
      FOwner: TGHashBiMap;
      FEnum:  TKeySet.TEntryEnumerator;
      function GetCurrent: TEntry; override;
    public
      constructor Create(aMap: TGHashBiMap);
      destructor Destroy; override;
      function MoveNext: Boolean; override;
      procedure Reset; override;
    end;

  var
    FKeySet: TKeySet;
    FValueSet: TValueSet;
    function  _GetRef: TObject;
    function  GetCount: SizeInt; inline;
    function  GetCapacity: SizeInt; inline;
    function  GetFillRatio: Single; inline;
    function  GetLoadFactor: Single; inline;
    function  GetTableSize: SizeInt; inline;
    procedure SetLoadFactor(aValue: Single);
    procedure KeyRemoving(p: PKeyEntry);
    procedure ValueRemoving(p: PValEntry);
    procedure DoClear; virtual;
    procedure DoEnsureCapacity(aValue: SizeInt);
    procedure DoTrimToFit;
    function  DoAdd(constref aKey: TKey; constref aValue: TValue): Boolean;
    function  DoAddAll(constref a: array of TEntry): SizeInt;
    function  DoAddAll(e: IEntryEnumerable): SizeInt;
    function  TryAddOrSetValue(constref aKey: TKey; constref aValue: TValue): Boolean;
    function  TryAddOrSetKey(constref aValue: TValue; constref aKey: TKey): Boolean;
    procedure DoAddOrSetValue(constref aKey: TKey; constref aValue: TValue);
    procedure DoAddOrSetKey(constref aValue: TValue; constref aKey: TKey);
    function  DoExtractKey(constref aKey: TKey; out v: TValue): Boolean;
    function  DoRemoveKey(constref aKey: TKey): Boolean; virtual;
    function  DoRemoveKeys(constref a: array of TKey): SizeInt;
    function  DoRemoveKeys(e: IKeyEnumerable): SizeInt;
    function  DoExtractValue(constref aValue: TValue; out k: TKey): Boolean; virtual;
    function  DoRemoveValue(constref aValue: TValue): Boolean; virtual;
    function  DoRemoveValues(constref a: array of TValue): SizeInt;
    function  DoRemoveValues(e: IValueEnumerable): SizeInt;
    function  DoReplaceValue(constref aKey: TKey; constref aNewValue: TValue): Boolean; virtual;
    function  DoReplaceKey(constref aValue: TValue; constref aNewKey: TKey): Boolean; virtual;
    procedure DoRetainAll({%H-}c: IKeyCollection); virtual;
    procedure DoRetainAllVal({%H-}c: IValueCollection); virtual;
    function  GetKeys: IKeyEnumerable;
    function  GetValues: IValueEnumerable;
    function  GetEntries: IEntryEnumerable;
    function  AddInverse(constref aValue: TValue; constref aKey: TKey): Boolean;
  { returns True and add e only if keys do not contain e.Key and values do not contain e.Value }
  public
    class function DefaultLoadFactor: Single; static; inline;
    class function MaxLoadFactor: Single; static; inline;
    class function MinLoadFactor: Single; static; inline;
    constructor Create;
    constructor Create(constref a: array of TEntry);
    constructor Create(e: IEntryEnumerable);
    constructor Create(aCapacity: SizeInt);
    constructor Create(aCapacity: SizeInt; constref a: array of TEntry);
    constructor Create(aCapacity: SizeInt; e: IEntryEnumerable);
    constructor CreateCopy(aMap: TGHashBiMap);
    destructor  Destroy; override;
    function  IsEmpty: Boolean;
    function  NonEmpty: Boolean;
    procedure Clear;
    procedure EnsureCapacity(aValue: SizeInt);
    procedure TrimToFit;
    function  Contains(constref aKey: TKey): Boolean; inline;
    function  NonContains(constref aKey: TKey): Boolean; inline;
    function  ContainsValue(constref aValue: TValue): Boolean; inline;
    function  NonContainsValue(constref aValue: TValue): Boolean; inline;
  { will raise ELGMapError if not contains aKey }
    function  GetValue(const aKey: TKey): TValue; inline;
  { will raise ELGMapError if not contains aKey }
    function  GetKey(const aValue: TValue): TKey; inline;
    function  TryGetValue(constref aKey: TKey; out aValue: TValue): Boolean;
    function  TryGetKey(constref aValue: TValue; out aKey: TKey): Boolean;
    function  GetValueDef(constref aKey: TKey; constref aDefault: TValue = Default(TValue)): TValue; inline;
    function  GetKeyDef(constref aValue: TValue; constref aDefault: TKey = Default(TKey)): TKey; inline;
  { returns True and add TEntry(aKey, aValue) only if keys do not contain aKey and
    values do not contain aValue }
    function  Add(constref aKey: TKey; constref aValue: TValue): Boolean;
  { returns True and add e only if keys do not contain e.Key and values do not contain e.Value }
    function  Add(constref e: TEntry): Boolean;
  { will raise ELGMapError if contains aValue }
    procedure AddOrSetValue(const aKey: TKey; const aValue: TValue);
  { will return False if contains aValue }
    function  AddOrSetValue(constref e: TEntry): Boolean;
  { will raise ELGMapError if contains aValue }
    procedure AddOrSetKey(const aValue: TValue; const aKey: TKey);
  { will return False if contains aValue }
    function  AddOrSetKey(constref e: TInverseEntry): Boolean;
  { will add only entries which keys and values are not contained in the map }
    function  AddAll(constref a: array of TEntry): SizeInt;
    function  AddAll(e: IEntryEnumerable): SizeInt;
  { returns True if contains aKey and not contains aNewValue }
    function  Replace(constref aKey: TKey; constref aNewValue: TValue): Boolean;
  { returns True if contains aValue and not contains aNewKey }
    function  ReplaceKey(constref aValue: TValue; constref aNewKey: TKey): Boolean;
    function  Extract(constref aKey: TKey; out v: TValue): Boolean;
    function  ExtractValue(constref aValue: TValue; out k: TKey): Boolean;
  { returns True if aKey contained in the map and removes an aKey from the map }
    function  Remove(constref aKey: TKey): Boolean;
    function  RemoveAll(constref a: array of TKey): SizeInt;
    function  RemoveAll(e: IKeyEnumerable): SizeInt;
  { returns True if aValue contained in the map and removes an aValue from the map }
    function  RemoveValue(constref aValue: TValue): Boolean;
    function  RemoveValues(constref a: array of TValue): SizeInt;
    function  RemoveValues(e: IValueEnumerable): SizeInt;
    procedure RetainAll(c: IKeyCollection);
    procedure RetainAll(c: IValueCollection);
    function  Clone: THashBiMap; virtual;
    function  Keys: IKeyEnumerable;
    function  Values: IValueEnumerable;
    function  Entries: IEntryEnumerable;
  private
    function  IInverseMap.Contains      = ContainsValue;
    function  IInverseMap.NonContains   = NonContainsValue;
    function  IInverseMap.GetValue      = GetKey;
    function  IInverseMap.TryGetValue   = TryGetKey;
    function  IInverseMap.GetValueDef   = GetKeyDef;
    function  IInverseMap.Add           = AddInverse;
    procedure IInverseMap.AddOrSetValue = AddOrSetKey;
    function  IInverseMap.Replace       = ReplaceKey;
    function  IInverseMap.Extract       = ExtractValue;
    function  IInverseMap.Remove        = RemoveValue;
    function  IInverseMap.RetainAll     = RetainAll;
    function  IInverseMap.Keys          = Values;
    function  IInverseMap.Values        = Keys;
  public
    property  Count: SizeInt read GetCount;
    property  Capacity: SizeInt read GetCapacity;
    property  LoadFactor: Single read GetLoadFactor write SetLoadFactor;
    property  FillRatio: Single read GetFillRatio;
    property  TableSize: SizeInt read GetTableSize;
  { will raise ELGMapError if not contains aKey or contains aValue }
    property  Items[const aKey: TKey]: TValue read GetValue write AddOrSetValue; default;
  end;

  { TGHashBiMapK assumes that TKey implements TKeyEqRel }
  generic TGHashBiMapK<TKey, TValue, TValueEqRel> = class(
    specialize TGHashBiMap<TKey, TValue, TKey, TValueEqRel>);

  { TGHashBiMapV assumes that TValue implements TValueEqRel }
  generic TGHashBiMapV<TKey, TValue, TKeyEqRel> = class(
    specialize TGHashBiMap<TKey, TValue, TKeyEqRel, TValue>);

  { TGHashBiMap2 assumes that TKey implements TKeyEqRel and TValue implements TValueEqRel }
  generic TGHashBiMap2<TKey, TValue> = class(specialize TGHashBiMap<TKey, TValue, TKey, TValue>);

  { TGObjectHashBiMap }

  generic TGObjectHashBiMap<TKey, TValue, TKeyEqRel, TValueEqRel> = class(specialize
    TGHashBiMap<TKey, TValue, TKeyEqRel, TValueEqRel>)
  private
    FOwnsKeys: Boolean;
    FOwnsValues: Boolean;
  protected
    procedure SetOwnership(aOwns: TMapObjOwnership); inline;
    procedure KeyRemoving(p: PKeyEntry);
    procedure ValueRemoving(p: PValEntry);
    procedure DoClear; override;
    function  DoRemoveKey(constref aKey: TKey): Boolean; override;
    function  DoRemoveValue(constref aValue: TValue): Boolean; override;
    function  DoReplaceValue(constref aKey: TKey; constref aNewValue: TValue): Boolean; override;
    function  DoReplaceKey(constref aValue: TValue; constref aNewKey: TKey): Boolean; override;
    procedure DoRetainAll({%H-}c: IKeyCollection); override;
    procedure DoRetainAllVal({%H-}c: IValueCollection); override;
  public
    constructor Create(aOwns: TMapObjOwnership = OWNS_BOTH);
    constructor Create(constref a: array of TEntry; aOwns: TMapObjOwnership = OWNS_BOTH);
    constructor Create(e: IEntryEnumerable; aOwns: TMapObjOwnership = OWNS_BOTH);
    constructor Create(aCapacity: SizeInt; aOwns: TMapObjOwnership = OWNS_BOTH);
    constructor Create(aCapacity: SizeInt; constref a: array of TEntry; aOwns: TMapObjOwnership = OWNS_BOTH);
    constructor Create(aCapacity: SizeInt; e: IEntryEnumerable; aOwns: TMapObjOwnership = OWNS_BOTH);
    constructor CreateCopy(aMap: TGObjectHashBiMap);
    function  Clone: TGObjectHashBiMap; override;
    property  OwnsKeys: Boolean read FOwnsKeys write FOwnsKeys;
    property  OwnsValues: Boolean read FOwnsValues write FOwnsValues;
  end;

  { TGObjHashBiMapK assumes that TKey implements TKeyEqRel }
  generic TGObjHashBiMapK<TKey, TValue, TValueEqRel> = class(specialize
    TGObjectHashBiMap<TKey, TValue, TKey, TValueEqRel>);

  { TGObjHashBiMapV assumes that TValue implements TValueEqRel }
  generic TGObjHashBiMapV<TKey, TValue, TKeyEqRel> = class(specialize
    TGObjectHashBiMap<TKey, TValue, TKeyEqRel, TValue>);

  { TGObjHashBiMap2 assumes that TKey implements TKeyEqRel and TValue implements TValueEqRel }
  generic TGObjHashBiMap2<TKey, TValue> = class(specialize TGObjectHashBiMap<TKey, TValue, TKey, TValue>);

implementation
{$B-}{$COPERATORS ON}

{ TGHashBiMap.TKeyEnumerable }

function TGHashBiMap.TKeyEnumerable.GetCurrent: TKey;
begin
  Result := FEnum.Current^.Key;
end;

constructor TGHashBiMap.TKeyEnumerable.Create(aMap: TGHashBiMap);
begin
  inherited Create;
  FOwner := aMap;
  FEnum := aMap.FKeySet.GetEnumerator;
end;

destructor TGHashBiMap.TKeyEnumerable.Destroy;
begin
  FOwner.EndIteration;
  FEnum.Free;
  inherited;
end;

function TGHashBiMap.TKeyEnumerable.MoveNext: Boolean;
begin
  Result := FEnum.MoveNext;
end;

procedure TGHashBiMap.TKeyEnumerable.Reset;
begin
  FEnum.Reset;
end;

{ TGHashBiMap.TValueEnumerable }

function TGHashBiMap.TValueEnumerable.GetCurrent: TValue;
begin
  Result := FEnum.Current^.Key;
end;

constructor TGHashBiMap.TValueEnumerable.Create(aMap: TGHashBiMap);
begin
  inherited Create;
  FOwner := aMap;
  FEnum := aMap.FValueSet.GetEnumerator;
end;

destructor TGHashBiMap.TValueEnumerable.Destroy;
begin
  FOwner.EndIteration;
  FEnum.Free;
  inherited;
end;

function TGHashBiMap.TValueEnumerable.MoveNext: Boolean;
begin
  Result := FEnum.MoveNext;
end;

procedure TGHashBiMap.TValueEnumerable.Reset;
begin
  FEnum.Reset;
end;

{ TGHashBiMap.TEntryEnumerable }

function TGHashBiMap.TEntryEnumerable.GetCurrent: TEntry;
var
  p: PKeyEntry;
begin
  p := FEnum.Current;
  Result.Key := p^.Key;
  Result.Value := p^.ValueNode^.Data.Key;
end;

constructor TGHashBiMap.TEntryEnumerable.Create(aMap: TGHashBiMap);
begin
  inherited Create;
  FOwner := aMap;
  FEnum := aMap.FKeySet.GetEnumerator;
end;

destructor TGHashBiMap.TEntryEnumerable.Destroy;
begin
  FOwner.EndIteration;
  FEnum.Free;
  inherited;
end;

function TGHashBiMap.TEntryEnumerable.MoveNext: Boolean;
begin
  Result := FEnum.MoveNext;
end;

procedure TGHashBiMap.TEntryEnumerable.Reset;
begin
  FEnum.Reset;
end;

{ TGBiMap }

function TGHashBiMap._GetRef: TObject;
begin
  Result := Self;
end;

function TGHashBiMap.GetCount: SizeInt;
begin
  Result := FKeySet.Count;
end;

procedure TGHashBiMap.DoClear;
begin
  FKeySet.Clear;
  FValueSet.Clear;
end;

function TGHashBiMap.GetCapacity: SizeInt;
begin
  Result := FKeySet.Capacity;
end;

function TGHashBiMap.GetFillRatio: Single;
begin
  Result := FKeySet.FillRatio;
end;

function TGHashBiMap.GetLoadFactor: Single;
begin
  Result := FKeySet.LoadFactor;
end;

function TGHashBiMap.GetTableSize: SizeInt;
begin
  Result := FKeySet.TableSize;
end;

procedure TGHashBiMap.SetLoadFactor(aValue: Single);
begin
  FKeySet.LoadFactor := aValue;
  FValueSet.LoadFactor := aValue;
end;

procedure TGHashBiMap.KeyRemoving(p: PKeyEntry);
begin
  FValueSet.Remove(p^.ValueNode^.Data.Key);
end;

procedure TGHashBiMap.ValueRemoving(p: PValEntry);
begin
  FKeySet.Remove(p^.KeyNode^.Data.Key);
end;

procedure TGHashBiMap.DoEnsureCapacity(aValue: SizeInt);
begin
  FKeySet.EnsureCapacity(aValue);
  FValueSet.EnsureCapacity(aValue);
end;

procedure TGHashBiMap.DoTrimToFit;
begin
  FKeySet.TrimToFit;
  FValueSet.TrimToFit;
end;

function TGHashBiMap.DoAdd(constref aKey: TKey; constref aValue: TValue): Boolean;
var
  pk: PKeyNode;
  pv: PValueNode;
begin
  if Contains(aKey) or ContainsValue(aValue) then
    exit(False);

  pk := FKeySet.Add(aKey);
  pv := FValueSet.Add(aValue);

  pk^.Data.Key := aKey;
  pk^.Data.ValueNode := pv;
  pv^.Data.Key := aValue;
  pv^.Data.KeyNode := pk;
  Result := True;
end;

function TGHashBiMap.DoAddAll(constref a: array of TEntry): SizeInt;
var
  e: TEntry;
begin
  Result := Count;
  for e in a do
    DoAdd(e.Key, e.Value);
  Result := Count - Result;
end;

function TGHashBiMap.DoAddAll(e: IEntryEnumerable): SizeInt;
var
  Entry: TEntry;
begin
  Result := Count;
  for Entry in e do
    DoAdd(Entry.Key, Entry.Value);
  Result := Count - Result;
end;

function TGHashBiMap.TryAddOrSetValue(constref aKey: TKey; constref aValue: TValue): Boolean;
begin
  Result := not ContainsValue(aValue);
  if Result then
    if Contains(aKey) then
      DoReplaceValue(aKey, aValue)
    else
      DoAdd(aKey, aValue);
end;

function TGHashBiMap.TryAddOrSetKey(constref aValue: TValue; constref aKey: TKey): Boolean;
begin
  Result := not Contains(aKey);
  if Result then
    if ContainsValue(aValue) then
      DoReplaceKey(aValue, aKey)
    else
      DoAdd(aKey, aValue);
end;

procedure TGHashBiMap.DoAddOrSetValue(constref aKey: TKey; constref aValue: TValue);
begin
  if not TryAddOrSetValue(aKey, aValue) then
    raise ELGMapError.Create(SEValueAlreadyExist);
end;

procedure TGHashBiMap.DoAddOrSetKey(constref aValue: TValue; constref aKey: TKey);
begin
  if not TryAddOrSetKey(aValue, aKey) then
    raise ELGMapError.Create(SEKeyAlreadyExist);
end;

function TGHashBiMap.DoExtractKey(constref aKey: TKey; out v: TValue): Boolean;
var
  srk: TKeySearchResult;
  pk: PKeyEntry;
begin
  pk := FKeySet.Find(aKey, srk);
  Result := pk <> nil;
  if Result then
    begin
      v := pk^.ValueNode^.Data.Key;
      if not FValueSet.Remove(pk^.ValueNode^.Data.Key) then
        raise EBiMapError.Create(SEInternalDataInconsist);
      FKeySet.RemoveAt(srk);
    end;
end;

function TGHashBiMap.DoRemoveKey(constref aKey: TKey): Boolean;
var
  v: TValue;
begin
  Result := DoExtractKey(aKey, v);
end;

function TGHashBiMap.DoRemoveKeys(constref a: array of TKey): SizeInt;
var
  k: TKey;
begin
  Result := Count;
  for k in a do
    DoRemoveKey(k);
  Result := Result - Count;
end;

function TGHashBiMap.DoRemoveKeys(e: IKeyEnumerable): SizeInt;
var
  k: TKey;
begin
  Result := Count;
  for k in e do
    DoRemoveKey(k);
  Result := Result - Count;
end;

function TGHashBiMap.DoExtractValue(constref aValue: TValue; out k: TKey): Boolean;
var
  srv: TValueSearchResult;
  pv: PValEntry;
begin
  pv := FValueSet.Find(aValue, srv);
  Result := pv <> nil;
  if Result then
    begin
      k := pv^.KeyNode^.Data.Key;
      if not FKeySet.Remove(pv^.KeyNode^.Data.Key) then
        raise EBiMapError.Create(SEInternalDataInconsist);
      FValueSet.RemoveAt(srv);
    end;
end;

function TGHashBiMap.DoRemoveValue(constref aValue: TValue): Boolean;
var
  k: TKey;
begin
  Result := DoExtractValue(aValue, k);
end;

function TGHashBiMap.DoRemoveValues(constref a: array of TValue): SizeInt;
var
  v: TValue;
begin
  Result := Count;
  for v in a do
    DoRemoveValue(v);
  Result := Result - Count;
end;

function TGHashBiMap.DoRemoveValues(e: IValueEnumerable): SizeInt;
var
  v: TValue;
begin
  Result := Count;
  for v in e do
    DoRemoveValue(v);
  Result := Result - Count;
end;

function TGHashBiMap.DoReplaceValue(constref aKey: TKey; constref aNewValue: TValue): Boolean;
var
  srk: TKeySearchResult;
  srv: TValueSearchResult;
  pk: PKeyEntry;
  pv: PValEntry;
begin
  pk := FKeySet.Find(aKey, srk);
  Result := pk <> nil;
  if Result then
    begin
      Result := not TValueEqRel.Equal(pk^.ValueNode^.Data.Key, aNewValue);
      if Result then
        begin
          Result := not FValueSet.FindOrAdd(aNewValue, pv, srv);
          if Result then
            begin
              if not FValueSet.Remove(pk^.ValueNode^.Data.Key) then
                raise EBiMapError.Create(SEInternalDataInconsist);
              pk^.ValueNode := srv.Node;
            end;
        end;
    end;
end;

function TGHashBiMap.DoReplaceKey(constref aValue: TValue; constref aNewKey: TKey): Boolean;
var
  srk: TKeySearchResult;
  srv: TValueSearchResult;
  pk: PKeyEntry;
  pv: PValEntry;
begin
  pv := FValueSet.Find(aValue, srv);
  Result := pv <> nil;
  if Result then
    begin
      Result := not TKeyEqRel.Equal(pv^.KeyNode^.Data.Key, aNewKey);
      if Result then
        begin
          Result := not FKeySet.FindOrAdd(aNewKey, pk, srk);
          if Result then
            begin
              if not FKeySet.Remove(pv^.KeyNode^.Data.Key) then
                raise EBiMapError.Create(SEInternalDataInconsist);
              pv^.KeyNode := srk.Node;
            end;
        end;
    end;
end;

procedure TGHashBiMap.DoRetainAll(c: IKeyCollection);
begin
  FKeySet.RemoveIf(@c.NonContains, @KeyRemoving);
end;

procedure TGHashBiMap.DoRetainAllVal(c: IValueCollection);
begin
  FValueSet.RemoveIf(@c.NonContains, @ValueRemoving);
end;

function TGHashBiMap.GetKeys: IKeyEnumerable;
begin
  Result := TKeyEnumerable.Create(Self);
end;

function TGHashBiMap.GetValues: IValueEnumerable;
begin
  Result := TValueEnumerable.Create(Self);
end;

function TGHashBiMap.GetEntries: IEntryEnumerable;
begin
  Result := TEntryEnumerable.Create(Self);
end;

function TGHashBiMap.AddInverse(constref aValue: TValue; constref aKey: TKey): Boolean;
begin
  CheckInIteration;
  Result := DoAdd(aKey, aValue);
end;

class function TGHashBiMap.DefaultLoadFactor: Single;
begin
  Result := TKeySet.DefaultLoadFactor;
end;

class function TGHashBiMap.MaxLoadFactor: Single;
begin
  Result := TKeySet.MaxLoadFactor;
end;

class function TGHashBiMap.MinLoadFactor: Single;
begin
  Result := TKeySet.MinLoadFactor;
end;

constructor TGHashBiMap.Create;
begin
  FKeySet := TKeySet.Create;
  FValueSet := TValueSet.Create;
end;

constructor TGHashBiMap.Create(constref a: array of TEntry);
begin
  Create;
  DoAddAll(a);
end;

constructor TGHashBiMap.Create(e: IEntryEnumerable);
begin
  Create;
  DoAddAll(e);
end;

constructor TGHashBiMap.Create(aCapacity: SizeInt);
begin
  FKeySet := TKeySet.Create(aCapacity);
  FValueSet := TValueSet.Create(aCapacity);
end;

constructor TGHashBiMap.Create(aCapacity: SizeInt; constref a: array of TEntry);
begin
  Create(aCapacity);
  DoAddAll(a);
end;

constructor TGHashBiMap.Create(aCapacity: SizeInt; e: IEntryEnumerable);
begin
  Create(aCapacity);
  DoAddAll(e);
end;

constructor TGHashBiMap.CreateCopy(aMap: TGHashBiMap);
begin
  Create(aMap.Count);
  DoAddAll(aMap.Entries);
end;

destructor TGHashBiMap.Destroy;
begin
  DoClear;
  FKeySet.Free;
  FValueSet.Free;
  inherited;
end;

function TGHashBiMap.IsEmpty: Boolean;
begin
  Result := Count = 0;
end;

function TGHashBiMap.NonEmpty: Boolean;
begin
  Result := Count > 0;
end;

procedure TGHashBiMap.Clear;
begin
  CheckInIteration;
  DoClear;
end;

procedure TGHashBiMap.EnsureCapacity(aValue: SizeInt);
begin
  CheckInIteration;
  DoEnsureCapacity(aValue);
end;

procedure TGHashBiMap.TrimToFit;
begin
  CheckInIteration;
  DoTrimToFit;
end;

function TGHashBiMap.Contains(constref aKey: TKey): Boolean;
var
  p: TKeySearchResult;
begin
  Result := FKeySet.Find(aKey, p) <> nil;
end;

function TGHashBiMap.NonContains(constref aKey: TKey): Boolean;
begin
  Result := not Contains(aKey);
end;

function TGHashBiMap.ContainsValue(constref aValue: TValue): Boolean;
var
  p: TValueSearchResult;
begin
  Result := FValueSet.Find(aValue, p) <> nil;
end;

function TGHashBiMap.NonContainsValue(constref aValue: TValue): Boolean;
begin
  Result := not ContainsValue(aValue);
end;

function TGHashBiMap.GetValue(const aKey: TKey): TValue;
begin
  if not TryGetValue(aKey, Result) then
    raise ELGMapError.Create(SEKeyNotFound);
end;

function TGHashBiMap.GetKey(const aValue: TValue): TKey;
begin
  if not TryGetKey(aValue, Result) then
    raise ELGMapError.Create(SEValueNotFound);
end;

function TGHashBiMap.TryGetValue(constref aKey: TKey; out aValue: TValue): Boolean;
var
  p: PKeyEntry;
  sr: TKeySearchResult;
begin
  p := FKeySet.Find(aKey, sr);
  Result := p <> nil;
  if Result then
    aValue := p^.ValueNode^.Data.Key;
end;

function TGHashBiMap.TryGetKey(constref aValue: TValue; out aKey: TKey): Boolean;
var
  p: PValEntry;
  sr: TValueSearchResult;
begin
  p := FValueSet.Find(aValue, sr);
  Result := p <> nil;
  if Result then
    aKey := p^.KeyNode^.Data.Key;
end;

function TGHashBiMap.GetValueDef(constref aKey: TKey; constref aDefault: TValue): TValue;
begin
  if not TryGetValue(aKey, Result) then
    Result := aDefault;
end;

function TGHashBiMap.GetKeyDef(constref aValue: TValue; constref aDefault: TKey): TKey;
begin
  if not TryGetKey(aValue, Result) then
    Result := aDefault;
end;

function TGHashBiMap.Add(constref aKey: TKey; constref aValue: TValue): Boolean;
begin
  CheckInIteration;
  Result := DoAdd(aKey, aValue);
end;

function TGHashBiMap.Add(constref e: TEntry): Boolean;
begin
  CheckInIteration;
  Result := DoAdd(e.Key, e.Value);
end;

procedure TGHashBiMap.AddOrSetValue(const aKey: TKey; const aValue: TValue);
begin
  CheckInIteration;
  DoAddOrSetValue(aKey, aValue);
end;

function TGHashBiMap.AddOrSetValue(constref e: TEntry): Boolean;
begin
  CheckInIteration;
  Result := TryAddOrSetValue(e.Key, e.Value);
end;

procedure TGHashBiMap.AddOrSetKey(const aValue: TValue; const aKey: TKey);
begin
  CheckInIteration;
  DoAddOrSetKey(aValue, aKey);
end;

function TGHashBiMap.AddOrSetKey(constref e: TInverseEntry): Boolean;
begin
  CheckInIteration;
  Result := TryAddOrSetKey(e.Key, e.Value);
end;

function TGHashBiMap.AddAll(constref a: array of TEntry): SizeInt;
begin
  CheckInIteration;
  Result := DoAddAll(a);
end;

function TGHashBiMap.AddAll(e: IEntryEnumerable): SizeInt;
begin
  CheckInIteration;
  Result := DoAddAll(e);
end;

function TGHashBiMap.Remove(constref aKey: TKey): Boolean;
begin
  CheckInIteration;
  Result := DoRemoveKey(aKey);
end;

function TGHashBiMap.RemoveAll(constref a: array of TKey): SizeInt;
begin
  CheckInIteration;
  Result := DoRemoveKeys(a);
end;

function TGHashBiMap.RemoveAll(e: IKeyEnumerable): SizeInt;
begin
  CheckInIteration;
  Result := DoRemoveKeys(e);
end;

function TGHashBiMap.RemoveValue(constref aValue: TValue): Boolean;
begin
  CheckInIteration;
  Result := DoRemoveValue(aValue);
end;

function TGHashBiMap.RemoveValues(constref a: array of TValue): SizeInt;
begin
  CheckInIteration;
  Result := DoRemoveValues(a);
end;

function TGHashBiMap.RemoveValues(e: IValueEnumerable): SizeInt;
begin
  CheckInIteration;
  Result := DoRemoveValues(e);
end;

function TGHashBiMap.Replace(constref aKey: TKey; constref aNewValue: TValue): Boolean;
begin
  CheckInIteration;
  Result := DoReplaceValue(aKey, aNewValue);
end;

function TGHashBiMap.ReplaceKey(constref aValue: TValue; constref aNewKey: TKey): Boolean;
begin
  CheckInIteration;
  Result := DoReplaceKey(aValue, aNewKey);
end;

function TGHashBiMap.Extract(constref aKey: TKey; out v: TValue): Boolean;
begin
  CheckInIteration;
  Result := DoExtractKey(aKey, v);
end;

function TGHashBiMap.ExtractValue(constref aValue: TValue; out k: TKey): Boolean;
begin
  CheckInIteration;
  Result := DoExtractValue(aValue, k);
end;

procedure TGHashBiMap.RetainAll(c: IKeyCollection);
begin
  CheckInIteration;
  DoRetainAll(c);
end;

procedure TGHashBiMap.RetainAll(c: IValueCollection);
begin
  CheckInIteration;
  DoRetainAllVal(c);
end;

function TGHashBiMap.Clone: THashBiMap;
begin
  Result := THashBiMap.CreateCopy(Self);
end;

function TGHashBiMap.Keys: IKeyEnumerable;
begin
  BeginIteration;
  Result := GetKeys;
end;

function TGHashBiMap.Values: IValueEnumerable;
begin
  BeginIteration;
  Result := GetValues;
end;

function TGHashBiMap.Entries: IEntryEnumerable;
begin
  BeginIteration;
  Result := GetEntries;
end;

{ TGObjectHashBiMap }

procedure TGObjectHashBiMap.SetOwnership(aOwns: TMapObjOwnership);
begin
  OwnsKeys := moOwnsKeys in aOwns;
  OwnsValues := moOwnsValues in aOwns;
end;

procedure TGObjectHashBiMap.KeyRemoving(p: PKeyEntry);
var
  v: TValue;
begin
  v := p^.ValueNode^.Data.Key;
  if OwnsKeys then
    TObject(p^.Key).Free;
  FValueSet.Remove(v);
  if OwnsValues then
    TObject(v).Free;
end;

procedure TGObjectHashBiMap.ValueRemoving(p: PValEntry);
var
  k: TKey;
begin
  k := p^.KeyNode^.Data.Key;
  if OwnsKeys then
    TObject(p^.Key).Free;
  FKeySet.Remove(k);
  if OwnsValues then
    TObject(k).Free;
end;

procedure TGObjectHashBiMap.DoClear;
var
  e: PKeyEntry;
begin
  if OwnsKeys or OwnsValues then
    for e in FKeySet do
      begin
        if OwnsKeys then
          TObject(e^.Key).Free;
        if OwnsValues then
          TObject(e^.ValueNode^.Data.Key).Free;
      end;
  inherited;
end;

function TGObjectHashBiMap.DoRemoveKey(constref aKey: TKey): Boolean;
var
  v: TValue;
begin
  Result := DoExtractKey(aKey, v);
  if Result then
    begin
      if OwnsKeys then
        TObject(aKey).Free;
      if OwnsValues then
        TObject(v).Free;
    end;
end;

function TGObjectHashBiMap.DoRemoveValue(constref aValue: TValue): Boolean;
var
  k: TKey;
begin
  Result := DoExtractValue(aValue, k);
  if Result then
    begin
      if OwnsKeys then
        TObject(k).Free;
      if OwnsValues then
        TObject(aValue).Free;
    end;
end;

function TGObjectHashBiMap.DoReplaceValue(constref aKey: TKey; constref aNewValue: TValue): Boolean;
var
  srk: TKeySearchResult;
  srv: TValueSearchResult;
  pk: PKeyEntry;
  pv: PValEntry;
  v: TValue;
begin
  pk := FKeySet.Find(aKey, srk);
  Result := pk <> nil;
  if Result then
    begin
      Result := not TValueEqRel.Equal(pk^.ValueNode^.Data.Key, aNewValue);
      if Result then
        begin
          Result := not FValueSet.FindOrAdd(aNewValue, pv, srv);
          if Result then
            begin
              v := pk^.ValueNode^.Data.Key;
              if not FValueSet.Remove(v) then
                raise EBiMapError.Create(SEInternalDataInconsist);
              pk^.ValueNode := srv.Node;
              if OwnsValues then
                TObject(v).Free;
            end;
        end;
    end;
end;

function TGObjectHashBiMap.DoReplaceKey(constref aValue: TValue; constref aNewKey: TKey): Boolean;
var
  srk: TKeySearchResult;
  srv: TValueSearchResult;
  pk: PKeyEntry;
  pv: PValEntry;
  k: TKey;
begin
  pv := FValueSet.Find(aValue, srv);
  Result := pv <> nil;
  if Result then
    begin
      Result := not TKeyEqRel.Equal(pv^.KeyNode^.Data.Key, aNewKey);
      if Result then
        begin
          Result := not FKeySet.FindOrAdd(aNewKey, pk, srk);
          if Result then
            begin
              k := pv^.KeyNode^.Data.Key;
              if not FKeySet.Remove(k) then
                raise EBiMapError.Create(SEInternalDataInconsist);
              pv^.KeyNode := srk.Node;
              if OwnsKeys then
                TObject(k).Free;
            end;
        end;
    end;
end;

procedure TGObjectHashBiMap.DoRetainAll(c: IKeyCollection);
begin
  FKeySet.RemoveIf(@c.NonContains, @KeyRemoving);
end;

procedure TGObjectHashBiMap.DoRetainAllVal(c: IValueCollection);
begin
  FValueSet.RemoveIf(@c.NonContains, @ValueRemoving);
end;

constructor TGObjectHashBiMap.Create(aOwns: TMapObjOwnership);
begin
  inherited Create;
  SetOwnership(aOwns);
end;

constructor TGObjectHashBiMap.Create(constref a: array of TEntry; aOwns: TMapObjOwnership);
begin
  inherited Create(a);
  SetOwnership(aOwns);
end;

constructor TGObjectHashBiMap.Create(e: IEntryEnumerable; aOwns: TMapObjOwnership);
begin
  inherited Create(e);
  SetOwnership(aOwns);
end;

constructor TGObjectHashBiMap.Create(aCapacity: SizeInt; aOwns: TMapObjOwnership);
begin
  inherited Create(aCapacity);
  SetOwnership(aOwns);
end;

constructor TGObjectHashBiMap.Create(aCapacity: SizeInt; constref a: array of TEntry; aOwns: TMapObjOwnership);
begin
  inherited Create(aCapacity, a);
  SetOwnership(aOwns);
end;

constructor TGObjectHashBiMap.Create(aCapacity: SizeInt; e: IEntryEnumerable; aOwns: TMapObjOwnership);
begin
  inherited Create(aCapacity, e);
  SetOwnership(aOwns);
end;

constructor TGObjectHashBiMap.CreateCopy(aMap: TGObjectHashBiMap);
begin
  inherited CreateCopy(aMap);
  OwnsKeys := aMap.OwnsKeys;
  OwnsValues := aMap.OwnsValues;
end;

function TGObjectHashBiMap.Clone: TGObjectHashBiMap;
begin
  Result := TGObjectHashBiMap.CreateCopy(Self);
end;

end.

