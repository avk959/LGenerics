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
{$INLINE ON}{$WARN 6058 off : }

interface

uses
  SysUtils,
  LGUtils,
  {%H-}LGHelpers,
  LGCustomContainer,
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
              specialize TGContainer<specialize TGMapEntry<TKey, TValue>>), but :( ... see #0033788}
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

  protected
  type

    TNode  = record
      KeyHash,
      ValueHash,
      NextKey,
      NextValue: SizeInt;
      Data: TEntry;
    end;

    TNodeList  = array of TNode;
    TChainList = array of SizeInt;

  const
    NULL_INDEX  = SizeInt(-1);
    MAX_CAPACITY: SizeInt  = (MAX_CONTAINER_SIZE shr 2) div SizeOf(TNode);

  type
    TKeyEnumerable = class(specialize TGAutoEnumerable<TKey>)
    protected
      FOwner: TGHashBiMap;
      FList:  TNodeList;
      FCurrIndex,
      FLastIndex: SizeInt;
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
      FList:  TNodeList;
      FCurrIndex,
      FLastIndex: SizeInt;
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
      FList:  TNodeList;
      FCurrIndex,
      FLastIndex: SizeInt;
      function GetCurrent: TEntry; override;
    public
      constructor Create(aMap: TGHashBiMap);
      destructor Destroy; override;
      function MoveNext: Boolean; override;
      procedure Reset; override;
    end;

  var
    FNodeList: TNodeList;
    FKeyChains,
    FValueChains: TChainList;
    FCount: SizeInt;
    function  _GetRef: TObject;
    function  GetCount: SizeInt; inline;
    function  GetCapacity: SizeInt; inline;
    procedure InitialAlloc; inline;
    procedure Rehash;
    procedure Resize(aNewCapacity: SizeInt);
    procedure Expand;
    procedure RemoveFromKeyChain(aIndex: SizeInt);
    procedure RemoveFromValueChain(aIndex: SizeInt);
    procedure FixKeyChain(aOldIndex, aNewIndex: SizeInt);
    procedure FixValueChain(aOldIndex, aNewIndex: SizeInt);
    function  DoFindKey(constref aKey: TKey; aHash: SizeInt): SizeInt;
    function  DoFindValue(constref aValue: TValue; aHash: SizeInt): SizeInt;
    function  FindKey(constref aKey: TKey): SizeInt; inline;
    function  FindValue(constref aValue: TValue): SizeInt; inline;
    procedure DoAddData(constref aKey: TKey; constref aValue: TValue; aKeyHash, aValHash: SizeInt);
    procedure DoRemove(aIndex: SizeInt);
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
  { returns True and add aValue and aKey only if keys do not contain aKey and values do not contain aValue }
    function  AddInverse(constref aValue: TValue; constref aKey: TKey): Boolean;
    class constructor Init;
  public
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
    function  TryAddOrSetValue(constref e: TEntry): Boolean;
  { will raise ELGMapError if contains aValue }
    procedure AddOrSetKey(const aValue: TValue; const aKey: TKey);
  { will return False if contains aValue }
    function  TryAddOrSetKey(constref e: TInverseEntry): Boolean;
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
    property  Count: SizeInt read FCount;
    property  Capacity: SizeInt read GetCapacity;
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
  Result := FList[FCurrIndex].Data.Key;
end;

constructor TGHashBiMap.TKeyEnumerable.Create(aMap: TGHashBiMap);
begin
  inherited Create;
  FOwner := aMap;
  FList := aMap.FNodeList;
  FLastIndex := Pred(aMap.Count);
  FCurrIndex := -1;
end;

destructor TGHashBiMap.TKeyEnumerable.Destroy;
begin
  FOwner.EndIteration;
  inherited;
end;

function TGHashBiMap.TKeyEnumerable.MoveNext: Boolean;
begin
  Result := FCurrIndex < FLastIndex;
  FCurrIndex += Ord(Result);
end;

procedure TGHashBiMap.TKeyEnumerable.Reset;
begin
  FCurrIndex := -1;
end;

{ TGHashBiMap.TValueEnumerable }

function TGHashBiMap.TValueEnumerable.GetCurrent: TValue;
begin
  Result := FList[FCurrIndex].Data.Value;
end;

constructor TGHashBiMap.TValueEnumerable.Create(aMap: TGHashBiMap);
begin
  inherited Create;
  FOwner := aMap;
  FList := aMap.FNodeList;
  FLastIndex := Pred(aMap.Count);
  FCurrIndex := -1;
end;

destructor TGHashBiMap.TValueEnumerable.Destroy;
begin
  FOwner.EndIteration;
  inherited;
end;

function TGHashBiMap.TValueEnumerable.MoveNext: Boolean;
begin
  Result := FCurrIndex < FLastIndex;
  FCurrIndex += Ord(Result);
end;

procedure TGHashBiMap.TValueEnumerable.Reset;
begin
  FCurrIndex := -1;
end;

{ TGHashBiMap.TEntryEnumerable }

function TGHashBiMap.TEntryEnumerable.GetCurrent: TEntry;
begin
  Result := FList[FCurrIndex].Data;
end;

constructor TGHashBiMap.TEntryEnumerable.Create(aMap: TGHashBiMap);
begin
  inherited Create;
  FOwner := aMap;
  FList := aMap.FNodeList;
  FLastIndex := Pred(aMap.Count);
  FCurrIndex := -1;
end;

destructor TGHashBiMap.TEntryEnumerable.Destroy;
begin
  FOwner.EndIteration;
  inherited;
end;

function TGHashBiMap.TEntryEnumerable.MoveNext: Boolean;
begin
  Result := FCurrIndex < FLastIndex;
  FCurrIndex += Ord(Result);
end;

procedure TGHashBiMap.TEntryEnumerable.Reset;
begin
  FCurrIndex := -1;
end;

{ TGBiMap }

function TGHashBiMap._GetRef: TObject;
begin
  Result := Self;
end;

procedure TGHashBiMap.InitialAlloc;
begin
  System.SetLength(FNodeList, DEFAULT_CONTAINER_CAPACITY);
  System.SetLength(FKeyChains, DEFAULT_CONTAINER_CAPACITY);
  System.FillChar(FKeyChains[0], DEFAULT_CONTAINER_CAPACITY * SizeOf(SizeInt), $ff);
  System.SetLength(FValueChains, DEFAULT_CONTAINER_CAPACITY);
  System.FillChar(FValueChains[0], DEFAULT_CONTAINER_CAPACITY * SizeOf(SizeInt), $ff);
end;

procedure TGHashBiMap.Rehash;
var
  I, kInd, vInd, Mask: SizeInt;
begin
  Mask := System.High(FNodeList);
  System.FillChar(Pointer(FKeyChains)^, Succ(Mask) * SizeOf(SizeInt), $ff);
  System.FillChar(Pointer(FValueChains)^, Succ(Mask) * SizeOf(SizeInt), $ff);
  for I := 0 to Pred(Count) do
    begin
      kInd := FNodeList[I].KeyHash and Mask;
      vInd := FNodeList[I].ValueHash and Mask;
      FNodeList[I].NextKey := FKeyChains[kInd];
      FKeyChains[kInd] := I;
      FNodeList[I].NextValue := FValueChains[vInd];
      FValueChains[vInd] := I;
    end;
end;

procedure TGHashBiMap.Resize(aNewCapacity: SizeInt);
begin
  System.SetLength(FNodeList, aNewCapacity);
  System.SetLength(FKeyChains, aNewCapacity);
  System.SetLength(FValueChains, aNewCapacity);
  Rehash;
end;

procedure TGHashBiMap.Expand;
var
  OldCapacity: SizeInt;
begin
  OldCapacity := Capacity;
  if OldCapacity > 0 then
    begin
      if OldCapacity < MAX_CAPACITY then
        Resize(OldCapacity shl 1)
      else
        CapacityExceedError(OldCapacity shl 1);
    end
  else
    InitialAlloc;
end;

procedure TGHashBiMap.RemoveFromKeyChain(aIndex: SizeInt);
var
  I, Curr, Prev: SizeInt;
begin
  I := FNodeList[aIndex].KeyHash and System.High(FNodeList);
  Curr := FKeyChains[I];
  Prev := NULL_INDEX;
  while Curr <> NULL_INDEX do
    begin
      if Curr = aIndex then
        begin
          if Prev <> NULL_INDEX then
            FNodeList[Prev].NextKey := FNodeList[Curr].NextKey
          else
            FKeyChains[I] := FNodeList[Curr].NextKey;
          exit;
        end;
      Prev := Curr;
      Curr := FNodeList[Curr].NextKey;
    end;
end;

procedure TGHashBiMap.RemoveFromValueChain(aIndex: SizeInt);
var
  I, Curr, Prev: SizeInt;
begin
  I := FNodeList[aIndex].ValueHash and System.High(FNodeList);
  Curr := FValueChains[I];
  Prev := NULL_INDEX;
  while Curr <> NULL_INDEX do
    begin
      if Curr = aIndex then
        begin
          if Prev <> NULL_INDEX then
            FNodeList[Prev].NextValue := FNodeList[Curr].NextValue
          else
            FValueChains[I] := FNodeList[Curr].NextValue;
          exit;
        end;
      Prev := Curr;
      Curr := FNodeList[Curr].NextValue;
    end;
end;

procedure TGHashBiMap.FixKeyChain(aOldIndex, aNewIndex: SizeInt);
var
  I: SizeInt;
begin
  I := FNodeList[aOldIndex].KeyHash and System.High(FNodeList);
  if FKeyChains[I] <> aOldIndex then
    repeat
      if FNodeList[I].NextKey = aOldIndex then
        begin
          FNodeList[I].NextKey := aNewIndex;
          exit;
        end;
      I := FNodeList[I].NextKey;
    until I = NULL_INDEX
  else
    FKeyChains[I] := aNewIndex;
end;

procedure TGHashBiMap.FixValueChain(aOldIndex, aNewIndex: SizeInt);
var
  I: SizeInt;
begin
  I := FNodeList[aOldIndex].ValueHash and System.High(FNodeList);
  if FValueChains[I] <> aOldIndex then
    repeat
      if FNodeList[I].NextValue = aOldIndex then
        begin
          FNodeList[I].NextValue := aNewIndex;
          exit;
        end;
      I := FNodeList[I].NextValue;
    until I = NULL_INDEX
  else
    FValueChains[I] := aNewIndex
end;

function TGHashBiMap.DoFindKey(constref aKey: TKey; aHash: SizeInt): SizeInt;
begin
  Result := FKeyChains[aHash and System.High(FNodeList)];
  while Result <> NULL_INDEX do
    begin
      if (FNodeList[Result].KeyHash = aHash) and TKeyEqRel.Equal(FNodeList[Result].Data.Key, aKey) then
        exit;
      Result := FNodeList[Result].NextKey;
    end;
end;

function TGHashBiMap.DoFindValue(constref aValue: TValue; aHash: SizeInt): SizeInt;
begin
  Result := FValueChains[aHash and System.High(FNodeList)];
  while Result <> NULL_INDEX do
    begin
      if (FNodeList[Result].ValueHash = aHash) and TValueEqRel.Equal(FNodeList[Result].Data.Value, aValue) then
        exit;
      Result := FNodeList[Result].NextValue;
    end;
end;

function TGHashBiMap.FindKey(constref aKey: TKey): SizeInt;
begin
  if Count > 0 then
    Result:= DoFindKey(aKey, TKeyEqRel.HashCode(aKey))
  else
    Result := NULL_INDEX;
end;

function TGHashBiMap.FindValue(constref aValue: TValue): SizeInt;
begin
  if Count > 0 then
    Result := DoFindValue(aValue, TValueEqRel.HashCode(aValue))
  else
    Result := NULL_INDEX;
end;

procedure TGHashBiMap.DoAddData(constref aKey: TKey; constref aValue: TValue; aKeyHash, aValHash: SizeInt);
var
  kInd, vInd, Mask, I: SizeInt;
begin
  Mask := System.High(FNodeList);
  I := Count;
  kInd := aKeyHash and Mask;
  vInd := aValHash and Mask;
  FNodeList[I].Data.Key := aKey;
  FNodeList[I].Data.Value := aValue;
  FNodeList[I].KeyHash := aKeyHash;
  FNodeList[I].ValueHash := aValHash;
  FNodeList[I].NextKey := FKeyChains[kInd];
  FKeyChains[kInd] := I;
  FNodeList[I].NextValue := FValueChains[vInd];
  FValueChains[vInd] := I;
  Inc(FCount);
end;

procedure TGHashBiMap.DoRemove(aIndex: SizeInt);
begin
  RemoveFromKeyChain(aIndex);
  RemoveFromValueChain(aIndex);
  FNodeList[aIndex].Data := Default(TEntry);
  Dec(FCount);
  if aIndex < Count then
    begin
      FixKeyChain(Count, aIndex);
      FixValueChain(Count, aIndex);
      System.Move(FNodeList[Count], FNodeList[aIndex], SizeOf(TNode));
      System.FillChar(FNodeList[Count], SizeOf(TNode), 0);
    end;
end;

function TGHashBiMap.GetCount: SizeInt;
begin
  Result := FCount;
end;

function TGHashBiMap.GetCapacity: SizeInt;
begin
  Result := System.Length(FNodeList);
end;

procedure TGHashBiMap.DoClear;
begin
  FNodeList := nil;
  FKeyChains := nil;
  FValueChains := nil;
  FCount := 0;
end;

procedure TGHashBiMap.DoEnsureCapacity(aValue: SizeInt);
begin
  if aValue <= Capacity then
    exit;
  if aValue <= DEFAULT_CONTAINER_CAPACITY then
    aValue := DEFAULT_CONTAINER_CAPACITY
  else
    if aValue <= MAX_CAPACITY then
      aValue := LGUtils.RoundUpTwoPower(aValue)
    else
      CapacityExceedError(aValue);
  Resize(aValue);
end;

procedure TGHashBiMap.DoTrimToFit;
var
  NewCapacity: SizeInt;
begin
  if Count > 0 then
    begin
      NewCapacity := LGUtils.RoundUpTwoPower(Count);
      if NewCapacity < Capacity then
        Resize(NewCapacity);
    end
  else
    Clear;
end;

function TGHashBiMap.DoAdd(constref aKey: TKey; constref aValue: TValue): Boolean;
var
  kh, vh: SizeInt;
begin
  kh := TKeyEqRel.HashCode(aKey);
  vh := TValueEqRel.HashCode(aValue);
  if Count > 0 then
    begin
      if DoFindKey(aKey, kh) <> NULL_INDEX then
        exit(False);
      if DoFindValue(aValue, vh) <> NULL_INDEX then
        exit(False);
    end;
  if Count = Capacity then
    Expand;
  DoAddData(aKey, aValue, kh, vh);
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
  I: SizeInt;
begin
  I := FindKey(aKey);
  Result := I <> NULL_INDEX;
  if Result then
    begin
      v := FNodeList[I].Data.Value;
      DoRemove(I);
    end;
end;

function TGHashBiMap.DoRemoveKey(constref aKey: TKey): Boolean;
var
  Dummy: TValue;
begin
  Result := DoExtractKey(aKey, Dummy);
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
  I: SizeInt;
begin
  I := FindValue(aValue);
  Result := I <> NULL_INDEX;
  if Result then
    begin
      k := FNodeList[I].Data.Key;
      DoRemove(I);
    end;
end;

function TGHashBiMap.DoRemoveValue(constref aValue: TValue): Boolean;
var
  Dummy: TKey;
begin
  Result := DoExtractValue(aValue, Dummy);
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
  I, J, h: SizeInt;
begin
  I := FindKey(aKey);
  if I = NULL_INDEX then
    exit(False);
  if FindValue(aNewValue) <> NULL_INDEX then
    exit(False);
  if not TValueEqRel.Equal(aNewValue, FNodeList[I].Data.Value) then
    begin
      h := TValueEqRel.HashCode(aNewValue);
      RemoveFromValueChain(I);
      J := h and Pred(Capacity);
      FNodeList[I].ValueHash := h;
      FNodeList[I].Data.Value := aNewValue;
      FNodeList[I].NextValue := FValueChains[J];
      FValueChains[J] := I;
    end;
  Result := True;
end;

function TGHashBiMap.DoReplaceKey(constref aValue: TValue; constref aNewKey: TKey): Boolean;
var
  I, J, h: SizeInt;
begin
  I := FindValue(aValue);
  if I = NULL_INDEX then
    exit(False);
  if FindKey(aNewKey) <> NULL_INDEX then
    exit(False);
  if not TKeyEqRel.Equal(aNewKey, FNodeList[I].Data.Key) then
    begin
      h := TKeyEqRel.HashCode(aNewKey);
      RemoveFromKeyChain(I);
      J := h and Pred(Capacity);
      FNodeList[I].KeyHash := h;
      FNodeList[I].Data.Key := aNewKey;
      FNodeList[I].NextKey := FKeyChains[J];
      FKeyChains[J] := I;
    end;
  Result := True;
end;

procedure TGHashBiMap.DoRetainAll(c: IKeyCollection);
var
  I: SizeInt = 0;
begin
  while I < Count do
    if c.NonContains(FNodeList[I].Data.Key) then
      DoRemove(I)
    else
      Inc(I);
end;

procedure TGHashBiMap.DoRetainAllVal(c: IValueCollection);
var
  I: SizeInt = 0;
begin
  while I < Count do
    if c.NonContains(FNodeList[I].Data.Value) then
      DoRemove(I)
    else
      Inc(I);
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

class constructor TGHashBiMap.Init;
begin
{$PUSH}{$J+}
  MAX_CAPACITY := LGUtils.RoundUpTwoPower(MAX_CAPACITY);
{$POP}
end;

constructor TGHashBiMap.Create;
begin
  InitialAlloc;
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
  EnsureCapacity(aCapacity);
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
  FNodeList := System.Copy(aMap.FNodeList);
  FKeyChains := System.Copy(aMap.FKeyChains);
  FValueChains := System.Copy(aMap.FValueChains);
  FCount := aMap.Count;
end;

destructor TGHashBiMap.Destroy;
begin
  DoClear;
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
begin
  Result := FindKey(aKey) <> NULL_INDEX;
end;

function TGHashBiMap.NonContains(constref aKey: TKey): Boolean;
begin
  Result := FindKey(aKey) = NULL_INDEX;
end;

function TGHashBiMap.ContainsValue(constref aValue: TValue): Boolean;
begin
  Result := FindValue(aValue) <> NULL_INDEX;
end;

function TGHashBiMap.NonContainsValue(constref aValue: TValue): Boolean;
begin
  Result := FindValue(aValue) = NULL_INDEX;
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
  I: SizeInt;
begin
  I := FindKey(aKey);
  Result := I <> NULL_INDEX;
  if Result then
    aValue := FNodeList[I].Data.Value;
end;

function TGHashBiMap.TryGetKey(constref aValue: TValue; out aKey: TKey): Boolean;
var
  I: SizeInt;
begin
  I := FindValue(aValue);
  Result := I <> NULL_INDEX;
  if Result then
    aKey := FNodeList[I].Data.Key;
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

function TGHashBiMap.TryAddOrSetValue(constref e: TEntry): Boolean;
begin
  CheckInIteration;
  Result := TryAddOrSetValue(e.Key, e.Value);
end;

procedure TGHashBiMap.AddOrSetKey(const aValue: TValue; const aKey: TKey);
begin
  CheckInIteration;
  DoAddOrSetKey(aValue, aKey);
end;

function TGHashBiMap.TryAddOrSetKey(constref e: TInverseEntry): Boolean;
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
  if not InIteration then
    Result := DoAddAll(e)
  else
    begin
      Result := 0;
      e.Any;
      UpdateLockError;
    end;
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
  if not InIteration then
    Result := DoRemoveKeys(e)
  else
    begin
      Result := 0;
      e.Any;
      UpdateLockError;
    end;
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
  if not InIteration then
    Result := DoRemoveValues(e)
  else
    begin
      Result := 0;
      e.Any;
      UpdateLockError;
    end;
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

procedure TGObjectHashBiMap.DoClear;
var
  I: SizeInt;
begin
  if OwnsKeys or OwnsValues then
    for I := 0 to Pred(Count) do
      begin
        if OwnsKeys then
          TObject(FNodeList[I].Data.Key).Free;
        if OwnsValues then
          TObject(FNodeList[I].Data.Value).Free;
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
  I, J, h: SizeInt;
begin
  I := FindKey(aKey);
  if I = NULL_INDEX then
    exit(False);
  if FindValue(aNewValue) <> NULL_INDEX then
    exit(False);
  if not TValueEqRel.Equal(aNewValue, FNodeList[I].Data.Value) then
    begin
      h := TValueEqRel.HashCode(aNewValue);
      RemoveFromValueChain(I);
      J := h and Pred(Capacity);
      FNodeList[I].ValueHash := h;
      if OwnsValues then
        TObject(FNodeList[I].Data.Value).Free;
      FNodeList[I].Data.Value := aNewValue;
      FNodeList[I].NextValue := FValueChains[J];
      FValueChains[J] := I;
    end;
  Result := True;
end;

function TGObjectHashBiMap.DoReplaceKey(constref aValue: TValue; constref aNewKey: TKey): Boolean;
var
  I, J, h: SizeInt;
begin
  I := FindValue(aValue);
  if I = NULL_INDEX then
    exit(False);
  if FindKey(aNewKey) <> NULL_INDEX then
    exit(False);
  if not TKeyEqRel.Equal(aNewKey, FNodeList[I].Data.Key) then
    begin
      h := TKeyEqRel.HashCode(aNewKey);
      RemoveFromKeyChain(I);
      J := h and Pred(Capacity);
      FNodeList[I].KeyHash := h;
      if OwnsKeys then
        TObject(FNodeList[I].Data.Key).Free;
      FNodeList[I].Data.Key := aNewKey;
      FNodeList[I].NextKey := FKeyChains[J];
      FKeyChains[J] := I;
    end;
  Result := True;
end;

procedure TGObjectHashBiMap.DoRetainAll(c: IKeyCollection);
var
  I: SizeInt = 0;
begin
  while I < Count do
    if c.NonContains(FNodeList[I].Data.Key) then
      begin
        if OwnsKeys then
          TObject(FNodeList[I].Data.Key).Free;
        if OwnsValues then
          TObject(FNodeList[I].Data.Value).Free;
        DoRemove(I);
      end
    else
      Inc(I);
end;

procedure TGObjectHashBiMap.DoRetainAllVal(c: IValueCollection);
var
  I: SizeInt = 0;
begin
  while I < Count do
    if c.NonContains(FNodeList[I].Data.Value) then
      begin
        if OwnsKeys then
          TObject(FNodeList[I].Data.Key).Free;
        if OwnsValues then
          TObject(FNodeList[I].Data.Value).Free;
        DoRemove(I);
      end
    else
      Inc(I);
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

