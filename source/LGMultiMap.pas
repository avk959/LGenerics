{****************************************************************************
*         _     ____ _____ _   _ _____ ____  ___ ____ ____                  *
*        | |   / ___| ____| \ | | ____|  _ \|_ _/ ___/ ___|                 *
*        | |  | |  _|  _| |  \| |  _| | |_) || | |   \___ \                 *
*        | |__| |_| | |___| |\  | |___|  _ < | | |___ ___) |                *
*        |_____\____|_____|_| \_|_____|_| \_\___\____|____/                 *
*                                                                           *
*   This file is part of the LGenerics package.                             *
*   Generic multimap implementations.                                       *
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
unit LGMultiMap;

{$mode objfpc}{$H+}
{$INLINE ON}

interface

uses

  SysUtils,
  LGUtils,
  {%H-}LGHelpers,
  LGCustomContainer,
  LGHashTable,
  LGAvlTree,
  LGSortedList;

type

  { TGCustomHashMultiMap: common abstract ancestor class}
  generic TGCustomHashMultiMap<TKey, TValue, TKeyEqRel> = class abstract(specialize TGCustomMultiMap<TKey, TValue>)
  protected
  type

    THashTable    = class(specialize TGHashTableLP<TKey, TMMEntry, TKeyEqRel>);

    TKeyEnumerable = class(TCustomKeyEnumerable)
    protected
      FEnum: THashTable.TEnumerator;
      function  GetCurrent: TKey; override;
    public
      constructor Create(aMap: TGCustomHashMultiMap);
      function  MoveNext: Boolean; override;
      procedure Reset; override;
    end;

    TValueEnumerable = class(TCustomValueEnumerable)
    protected
      FValueEnum: TCustomValueEnumerator;
      FEntryEnum: THashTable.TEnumerator;
      function  GetCurrent: TValue; override;
    public
      constructor Create(aMap: TGCustomHashMultiMap);
      function  MoveNext: Boolean; override;
      procedure Reset; override;
    end;

    TEntryEnumerable = class(TCustomEntryEnumerable)
    protected
      FValueEnum: TCustomValueEnumerator;
      FEntryEnum: THashTable.TEnumerator;
      function  GetCurrent: TEntry; override;
    public
      constructor Create(aMap: TGCustomHashMultiMap);
      function  MoveNext: Boolean; override;
      procedure Reset; override;
    end;

  var
    FTable: THashTable;
    function  GetFillRatio: Single;
    function  GetLoadFactor: Single;
    function  GetTableSize: SizeInt;
    procedure SetLoadFactor(aValue: Single);
    function  GetKeyCount: SizeInt; override;
    function  GetCapacity: SizeInt; override;
    procedure DoClear; override;
    procedure DoEnsureCapacity(aValue: SizeInt); override;
    procedure DoTrimToFit; override;
    function  Find(constref aKey: TKey): PMMEntry; override;
    function  FindOrAdd(constref aKey: TKey): PMMEntry; override;
    function  DoRemoveKey(constref aKey: TKey): SizeInt; override;
    function  GetKeys: IKeyEnumerable; override;
    function  GetValues: IValueEnumerable; override;
    function  GetEntries: IEntryEnumerable; override;
    function  CreateValueSet: TCustomValueSet; virtual; abstract;
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
    property  LoadFactor: Single read GetLoadFactor write SetLoadFactor;
    property  FillRatio: Single read GetFillRatio;
    property  TableSize: SizeInt read GetTableSize;
  end;

  { TGHashMultiMap implements multimap with value collections as linear probing hashset;

      functor TKeyEqRel(key equality relation) must provide:
        class function HashCode([const[ref]] k: TKey): SizeInt;
        class function Equal([const[ref]] L, R: TKey): Boolean;

      functor TValueEqRel(value equality relation) must provide:
        class function HashCode([const[ref]] v: TValue): SizeInt;
        class function Equal([const[ref]] L, R: TValue): Boolean; }
  generic TGHashMultiMap<TKey, TValue, TKeyEqRel, TValueEqRel> = class(specialize TGCustomHashMultiMap<TKey, TValue, TKeyEqRel>)
  protected
  type
    TValueSet = class(TCustomValueSet)
    protected
    type
      TValEntry = record
        Key: TValue;
      end;
      PValEntry = ^TValEntry;

      TTable = specialize TGHashTableLP<TValue, TValEntry, TValueEqRel>;

      TEnumerator = class(TCustomValueEnumerator)
      protected
        FEnum: TTable.TEnumerator;
        function  GetCurrent: TValue; override;
      public
        constructor Create(aTable: TTable);
        function  MoveNext: Boolean; override;
        procedure Reset; override;
      end;

    const
      START_CAPACITY = 8;
      LOAD_FACTOR    = 0.65; //todo: why ???

    var
      FTable: TTable;
      function GetCount: SizeInt; override;
    public
      constructor Create;
      destructor Destroy; override;
      function  GetEnumerator: TCustomValueEnumerator; override;
      function  Contains(constref aValue: TValue): Boolean; override;
      function  Add(constref aValue: TValue): Boolean; override;
      function  Remove(constref aValue: TValue): Boolean; override;
    end;

    function GetUniqueKeyValues: Boolean; override;
    function CreateValueSet: TCustomValueSet; override;
  public
    destructor Destroy; override;
  end;

  { TGHashMultiMapK assumes that TKey implements TKeyEqRel }
  generic TGHashMultiMapK<TKey, TValue, TValueEqRel> = class(
    specialize TGHashMultiMap<TKey, TValue, TKey, TValueEqRel>);

  { TGHashMultiMapV assumes that TValue implements TValueEqRel }
  generic TGHashMultiMapV<TKey, TValue, TKeyEqRel> = class(
    specialize TGHashMultiMap<TKey, TValue, TKeyEqRel, TValue>);

  { TGHashMultiMap2 assumes that TKey implements TKeyEqRel and TValue implements TValueEqRel }
  generic TGHashMultiMap2<TKey, TValue> = class(specialize TGHashMultiMap<TKey, TValue, TKey, TValue>);

  { TGTreeMultiMap implements multimap with value collections as avl tree;

      functor TKeyEqRel(key equality relation) must provide:
        class function HashCode([const[ref]] k: TKey): SizeInt;
        class function Equal([const[ref]] L, R: TKey): Boolean;

      functor TValueCmpRel(value comparision relation) must provide:
        class function Compare([const[ref]] L, R: TValue): SizeInt; }
  generic TGTreeMultiMap<TKey, TValue, TKeyEqRel, TValueCmpRel> = class(specialize TGCustomHashMultiMap<TKey, TValue, TKeyEqRel>)
  protected
  type
    TValEntry = record
      Key: TValue;
    end;
    PValEntry = ^TValEntry;

    TNode        = specialize TGAvlTreeNode<TValEntry>;
    PNode        = ^TNode;
    TNodeManager = specialize TGPageNodeManager<TNode>;
    PNodeManager = ^TNodeManager;

    TValueSet = class(TCustomValueSet)
    protected
    type
      TTable = specialize TGAvlTree2<TValue, TValEntry, TNodeManager, TValueCmpRel>;

      TEnumerator = class(TCustomValueEnumerator)
      protected
        FEnum: TTable.TEnumerator;
        function  GetCurrent: TValue; override;
      public
        constructor Create(aTable: TTable);
        function  MoveNext: Boolean; override;
        procedure Reset; override;
      end;

    var
      FTable: TTable;
      function GetCount: SizeInt; override;
    public
      constructor Create(aNodeManager: TNodeManager);
      destructor Destroy; override;
      function  GetEnumerator: TCustomValueEnumerator; override;
      function  Contains(constref aValue: TValue): Boolean; override;
      function  Add(constref aValue: TValue): Boolean; override;
      function  Remove(constref aValue: TValue): Boolean; override;
    end;
  var
    FNodeManager: TNodeManager;
    procedure DoClear; override;
    procedure DoTrimToFit; override;
    function  GetUniqueKeyValues: Boolean; override;
    function  CreateValueSet: TCustomValueSet; override;
  public
    constructor Create;
    constructor Create(constref a: array of TEntry);
    constructor Create(e: IEntryEnumerable);
    constructor Create(aCapacity: SizeInt);
    constructor Create(aCapacity: SizeInt; constref a: array of TEntry);
    constructor Create(aCapacity: SizeInt; e: IEntryEnumerable);
    destructor  Destroy; override;
  end;

  { TGTreeMultiMapK assumes that TKey implements TKeyEqRel }
  generic TGTreeMultiMapK<TKey, TValue, TValueCmpRel> = class(
    specialize TGTreeMultiMap<TKey, TValue, TKey, TValueCmpRel>);

  { TGHashMultiMapV assumes that TValue implements TValueCmpRel }
  generic TGTreeMultiMapV<TKey, TValue, TKeyEqRel> = class(
    specialize TGTreeMultiMap<TKey, TValue, TKeyEqRel, TValue>);

  { TGTreeMultiMap2 assumes that TKey implements TKeyEqRel and TValue implements TValueCmpRel }
  generic TGTreeMultiMap2<TKey, TValue> = class(specialize TGTreeMultiMap<TKey, TValue, TKey, TValue>);

  { TGListMultiMap implements multimap with value collections as sorted list;

      functor TKeyEqRel(key equality relation) must provide:
        class function HashCode([const[ref]] k: TKey): SizeInt;
        class function Equal([const[ref]] L, R: TKey): Boolean;

      functor TValueCmpRel(value comparision relation) must provide:
        class function Compare([const[ref]] L, R: TValue): SizeInt; }
  generic TGListMultiMap<TKey, TValue, TKeyEqRel, TValueCmpRel> = class(specialize TGCustomHashMultiMap<TKey, TValue, TKeyEqRel>)
  protected
  const
    INITIAL_CAPACITY = 8;

  type
    TValueSet = class(TCustomValueSet)
    protected
    type
      TList = specialize TGSortedList2<TValue, TValueCmpRel>;

      TEnumerator = class(TCustomValueEnumerator)
      protected
        FEnum: TList.TEnumerator;
        function  GetCurrent: TValue; override;
      public
        constructor Create(aList: TList);
        function  MoveNext: Boolean; override;
        procedure Reset; override;
      end;

    var
      FList: TList;
      function GetCount: SizeInt; override;
    public
      constructor Create;
      destructor Destroy; override; final;
      function  GetEnumerator: TCustomValueEnumerator; override;
      procedure TrimToFit;
      function  Contains(constref aValue: TValue): Boolean; override;
      function  Add(constref aValue: TValue): Boolean; override;
      function  Remove(constref aValue: TValue): Boolean; override;
    end;

    procedure DoTrimToFit; override;
    function  GetUniqueKeyValues: Boolean; override;
    function  CreateValueSet: TCustomValueSet; override;
  public
     destructor Destroy; override;
  end;

  { TGListMultiMapK assumes that TKey implements TKeyEqRel }
  generic TGListMultiMapK<TKey, TValue, TValueCmpRel> = class(
    specialize TGListMultiMap<TKey, TValue, TKey, TValueCmpRel>);

  { TGListMultiMapV assumes that TValue implements TValueCmpRel }
  generic TGListMultiMapV<TKey, TValue, TKeyEqRel> = class(
    specialize TGListMultiMap<TKey, TValue, TKeyEqRel, TValue>);

  { TGListMultiMap2 assumes that TKey implements TKeyEqRel and TValue implements TValueCmpRel }
  generic TGListMultiMap2<TKey, TValue> = class(specialize TGListMultiMap<TKey, TValue, TKey, TValue>);

implementation
{$B-}{$COPERATORS ON}

{ TGCustomHashMultiMap.TKeyEnumerable }

function TGCustomHashMultiMap.TKeyEnumerable.GetCurrent: TKey;
begin
  Result := FEnum.Current^.Key;
end;

constructor TGCustomHashMultiMap.TKeyEnumerable.Create(aMap: TGCustomHashMultiMap);
begin
  inherited Create(aMap);
  FEnum := aMap.FTable.GetEnumerator;
end;

function TGCustomHashMultiMap.TKeyEnumerable.MoveNext: Boolean;
begin
  Result := FEnum.MoveNext;
end;

procedure TGCustomHashMultiMap.TKeyEnumerable.Reset;
begin
  FEnum.Reset;
end;

{ TGCustomHashMultiMap.TValueEnumerable }

function TGCustomHashMultiMap.TValueEnumerable.GetCurrent: TValue;
begin
  Result := FValueEnum.Current;
end;

constructor TGCustomHashMultiMap.TValueEnumerable.Create(aMap: TGCustomHashMultiMap);
begin
  inherited Create(aMap);
  FEntryEnum := aMap.FTable.GetEnumerator;
end;

function TGCustomHashMultiMap.TValueEnumerable.MoveNext: Boolean;
begin
  repeat
    if not Assigned(FValueEnum) then
      begin
        if not FEntryEnum.MoveNext then
          exit(False);
        FValueEnum := FEntryEnum.Current^.Values.GetEnumerator;
      end;
    Result := FValueEnum.MoveNext;
    if not Result then
      FreeAndNil(FValueEnum);
  until Result;
end;

procedure TGCustomHashMultiMap.TValueEnumerable.Reset;
begin
  FEntryEnum.Reset;
  FValueEnum := nil;
end;

{ TGCustomHashMultiMap.TEntryEnumerable }

function TGCustomHashMultiMap.TEntryEnumerable.GetCurrent: TEntry;
begin
  Result.Key := FEntryEnum.Current^.Key;
  Result.Value := FValueEnum.Current;
end;

constructor TGCustomHashMultiMap.TEntryEnumerable.Create(aMap: TGCustomHashMultiMap);
begin
  inherited Create(aMap);
  FEntryEnum := aMap.FTable.GetEnumerator;
end;

function TGCustomHashMultiMap.TEntryEnumerable.MoveNext: Boolean;
begin
  repeat
    if not Assigned(FValueEnum) then
      begin
        if not FEntryEnum.MoveNext then
          exit(False);
        FValueEnum := FEntryEnum.Current^.Values.GetEnumerator;
      end;
    Result := FValueEnum.MoveNext;
    if not Result then
      FreeAndNil(FValueEnum);
  until Result;
end;

procedure TGCustomHashMultiMap.TEntryEnumerable.Reset;
begin
  FEntryEnum.Reset;
  FValueEnum := nil;
end;

{ TGCustomHashMultiMap }

function TGCustomHashMultiMap.GetFillRatio: Single;
begin
  Result := FTable.FillRatio;
end;

function TGCustomHashMultiMap.GetLoadFactor: Single;
begin
  Result := FTable.LoadFactor;
end;

function TGCustomHashMultiMap.GetTableSize: SizeInt;
begin
  Result := FTable.TableSize;
end;

procedure TGCustomHashMultiMap.SetLoadFactor(aValue: Single);
begin
  FTable.LoadFactor := aValue;
end;

function TGCustomHashMultiMap.GetKeyCount: SizeInt;
begin
  Result := FTable.Count;
end;

function TGCustomHashMultiMap.GetCapacity: SizeInt;
begin
  Result := FTable.Capacity;
end;

procedure TGCustomHashMultiMap.DoClear;
var
  p: PMMEntry;
begin
  for p in FTable do
    p^.Values.Free;
  FTable.Clear;
end;

procedure TGCustomHashMultiMap.DoEnsureCapacity(aValue: SizeInt);
begin
  if not FTable.EnsureCapacity(aValue) then
    CapacityExceedError(aValue);
end;

procedure TGCustomHashMultiMap.DoTrimToFit;
begin
  FTable.TrimToFit;
end;

function TGCustomHashMultiMap.Find(constref aKey: TKey): PMMEntry;
var
  p: SizeInt;
begin
  Result := FTable.Find(aKey, p);
end;

function TGCustomHashMultiMap.FindOrAdd(constref aKey: TKey): PMMEntry;
var
  p: SizeInt;
begin
  if not FTable.FindOrAdd(aKey, Result, p) then
    begin
      Result^.Key := aKey;
      Result^.Values := CreateValueSet;
    end;
end;

function TGCustomHashMultiMap.DoRemoveKey(constref aKey: TKey): SizeInt;
var
  Pos: SizeInt;
  p: PMMEntry;
begin
  p := FTable.Find(aKey, Pos);
  if p <> nil then
    begin
      Result := p^.Values.Count;
      p^.Values.Free;
      FTable.RemoveAt(Pos);
    end
  else
    Result := 0;
end;

function TGCustomHashMultiMap.GetKeys: IKeyEnumerable;
begin
  Result := TKeyEnumerable.Create(Self);
end;

function TGCustomHashMultiMap.GetValues: IValueEnumerable;
begin
  Result := TValueEnumerable.Create(Self);
end;

function TGCustomHashMultiMap.GetEntries: IEntryEnumerable;
begin
  Result := TEntryEnumerable.Create(Self);
end;

class function TGCustomHashMultiMap.DefaultLoadFactor: Single;
begin
  Result := THashTable.DefaultLoadFactor;
end;

class function TGCustomHashMultiMap.MaxLoadFactor: Single;
begin
  Result := THashTable.MaxLoadFactor;
end;

class function TGCustomHashMultiMap.MinLoadFactor: Single;
begin
  Result := THashTable.MinLoadFactor;
end;

constructor TGCustomHashMultiMap.Create;
begin
  FTable := THashTable.Create;
end;

constructor TGCustomHashMultiMap.Create(constref a: array of TEntry);
begin
  FTable := THashTable.Create;
  DoAddAll(a);
end;

constructor TGCustomHashMultiMap.Create(e: IEntryEnumerable);
begin
  FTable := THashTable.Create;
  DoAddAll(e);
end;

constructor TGCustomHashMultiMap.Create(aCapacity: SizeInt);
begin
  FTable := THashTable.Create(aCapacity);
end;

constructor TGCustomHashMultiMap.Create(aCapacity: SizeInt; constref a: array of TEntry);
begin
  FTable := THashTable.Create(aCapacity);
  DoAddAll(a);
end;

constructor TGCustomHashMultiMap.Create(aCapacity: SizeInt; e: IEntryEnumerable);
begin
  FTable := THashTable.Create(aCapacity);
  DoAddAll(e);
end;

{ TGHashMultiMap.TValueSet.TEnumerator }

function TGHashMultiMap.TValueSet.TEnumerator.GetCurrent: TValue;
begin
  Result := FEnum.Current^.Key;
end;

constructor TGHashMultiMap.TValueSet.TEnumerator.Create(aTable: TTable);
begin
  FEnum := aTable.GetEnumerator;
end;

function TGHashMultiMap.TValueSet.TEnumerator.MoveNext: Boolean;
begin
  Result := FEnum.MoveNext;
end;

procedure TGHashMultiMap.TValueSet.TEnumerator.Reset;
begin
  FEnum.Reset;
end;

{ TGHashMultiMap.TValueSet }

function TGHashMultiMap.TValueSet.GetCount: SizeInt;
begin
  Result := FTable.Count;
end;

constructor TGHashMultiMap.TValueSet.Create;
begin
  FTable := TTable.Create(START_CAPACITY, LOAD_FACTOR);
end;

destructor TGHashMultiMap.TValueSet.Destroy;
begin
  FTable.Free;
  inherited;
end;

function TGHashMultiMap.TValueSet.GetEnumerator: TCustomValueEnumerator;
begin
  Result := TEnumerator.Create(Ftable);
end;

function TGHashMultiMap.TValueSet.Contains(constref aValue: TValue): Boolean;
var
  p: SizeInt;
begin
  Result := FTable.Find(aValue, p) <> nil;
end;

function TGHashMultiMap.TValueSet.Add(constref aValue: TValue): Boolean;
var
  p: PValEntry;
  Pos: SizeInt;
begin
  Result := not FTable.FindOrAdd(aValue, p, Pos);
  if Result then
    p^.Key := aValue;
end;

function TGHashMultiMap.TValueSet.Remove(constref aValue: TValue): Boolean;
begin
  Result := FTable.Remove(aValue);
  if Result then
    FTable.TrimToFit;
end;

{ TGHashMultiMap }

function TGHashMultiMap.GetUniqueKeyValues: Boolean;
begin
  Result := True;
end;

function TGHashMultiMap.CreateValueSet: TCustomValueSet;
begin
  Result := TValueSet.Create;
end;

destructor TGHashMultiMap.Destroy;
begin
  DoClear;
  FTable.Free;
  inherited;
end;

{ TGTreeMultiMap.TValueSet.TEnumerator }

function TGTreeMultiMap.TValueSet.TEnumerator.GetCurrent: TValue;
begin
  Result := FEnum.Current^.Key;
end;

constructor TGTreeMultiMap.TValueSet.TEnumerator.Create(aTable: TTable);
begin
  FEnum := aTable.GetEnumerator;
end;

function TGTreeMultiMap.TValueSet.TEnumerator.MoveNext: Boolean;
begin
  Result := FEnum.MoveNext;
end;

procedure TGTreeMultiMap.TValueSet.TEnumerator.Reset;
begin
  FEnum.Reset;
end;

{ TGTreeMultiMap.TValueSet }

function TGTreeMultiMap.TValueSet.GetCount: SizeInt;
begin
  Result := FTable.Count;
end;

constructor TGTreeMultiMap.TValueSet.Create(aNodeManager: TNodeManager);
begin
  FTable := TTable.Create(aNodeManager);
end;

destructor TGTreeMultiMap.TValueSet.Destroy;
begin
  FTable.Free;
  inherited;
end;

function TGTreeMultiMap.TValueSet.GetEnumerator: TCustomValueEnumerator;
begin
  Result := TEnumerator.Create(FTable);
end;

function TGTreeMultiMap.TValueSet.Contains(constref aValue: TValue): Boolean;
begin
  Result := FTable.Find(aValue) <> nil;
end;

function TGTreeMultiMap.TValueSet.Add(constref aValue: TValue): Boolean;
var
  p: PNode;
begin
  Result := not FTable.FindOrAdd(aValue, p);
  if Result then
    p^.Data.Key := aValue;
end;

function TGTreeMultiMap.TValueSet.Remove(constref aValue: TValue): Boolean;
begin
  Result := FTable.Remove(aValue);
end;

{ TGTreeMultiMap }

procedure TGTreeMultiMap.DoClear;
begin
  inherited DoClear;
  FNodeManager.Clear;
end;

procedure TGTreeMultiMap.DoTrimToFit;
begin
  inherited;
  if Count = 0 then
    FNodeManager.Clear;
end;

function TGTreeMultiMap.GetUniqueKeyValues: Boolean;
begin
  Result := True;
end;

function TGTreeMultiMap.CreateValueSet: TCustomValueSet;
begin
  Result := TValueSet.Create(FNodeManager);
end;

constructor TGTreeMultiMap.Create;
begin
  FTable := THashTable.Create;
  FNodeManager := TNodeManager.Create;
end;

constructor TGTreeMultiMap.Create(constref a: array of TEntry);
begin
  FTable := THashTable.Create;
  FNodeManager := TNodeManager.Create;
  DoAddAll(a);
end;

constructor TGTreeMultiMap.Create(e: IEntryEnumerable);
begin
  FTable := THashTable.Create;
  FNodeManager := TNodeManager.Create;
  DoAddAll(e);
end;

constructor TGTreeMultiMap.Create(aCapacity: SizeInt);
begin
  FTable := THashTable.Create(aCapacity);
  FNodeManager := TNodeManager.Create;
end;

constructor TGTreeMultiMap.Create(aCapacity: SizeInt; constref a: array of TEntry);
begin
  FTable := THashTable.Create(aCapacity);
  FNodeManager := TNodeManager.Create;
  DoAddAll(a);
end;

constructor TGTreeMultiMap.Create(aCapacity: SizeInt; e: IEntryEnumerable);
begin
  FTable := THashTable.Create(aCapacity);
  FNodeManager := TNodeManager.Create;
  DoAddAll(e);
end;

destructor TGTreeMultiMap.Destroy;
begin
  DoClear;
  FTable.Free;
  FNodeManager.Free;
  inherited;
end;

{ TGListMultiMap.TValueSet.TEnumerator }

function TGListMultiMap.TValueSet.TEnumerator.GetCurrent: TValue;
begin
  Result := FEnum.Current;
end;

constructor TGListMultiMap.TValueSet.TEnumerator.Create(aList: TList);
begin
  FEnum := aList.GetEnumerator;
end;

function TGListMultiMap.TValueSet.TEnumerator.MoveNext: Boolean;
begin
  Result := FEnum.MoveNext;
end;

procedure TGListMultiMap.TValueSet.TEnumerator.Reset;
begin
  FEnum.Reset;
end;

{ TGListMultiMap.TValueSet }

function TGListMultiMap.TValueSet.GetCount: SizeInt;
begin
  Result := FList.Count;
end;

constructor TGListMultiMap.TValueSet.Create;
begin
  FList := TList.Create(INITIAL_CAPACITY, True);
end;

destructor TGListMultiMap.TValueSet.Destroy;
begin
  FList.Free;
  inherited;
end;

function TGListMultiMap.TValueSet.GetEnumerator: TCustomValueEnumerator;
begin
  Result := TEnumerator.Create(FList);
end;

procedure TGListMultiMap.TValueSet.TrimToFit;
begin
  FList.TrimToFit;
end;

function TGListMultiMap.TValueSet.Contains(constref aValue: TValue): Boolean;
var
  I: SizeInt;
begin
  Result := FList.Contains(aValue);
end;

function TGListMultiMap.TValueSet.Add(constref aValue: TValue): Boolean;
begin
  Result := FList.Add(aValue);
end;

function TGListMultiMap.TValueSet.Remove(constref aValue: TValue): Boolean;
begin
  Result := FList.Remove(aValue);
  FList.TrimToFit;
end;

{ TGListMultiMap }

procedure TGListMultiMap.DoTrimToFit;
var
  p: PMMEntry;
begin
  inherited;
  for p in FTable do
    TValueSet(p^.Values).TrimToFit;
end;

function TGListMultiMap.GetUniqueKeyValues: Boolean;
begin
  Result := False;
end;

function TGListMultiMap.CreateValueSet: TCustomValueSet;
begin
  Result := TValueSet.Create;
end;

destructor TGListMultiMap.Destroy;
begin
  DoClear;
  inherited;
end;

end.

