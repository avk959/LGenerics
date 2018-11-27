{****************************************************************************
*                                                                           *
*   This file is part of the LGenerics package.                             *
*   Generic sorted map implementations on top of AVL tree.                  *
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
unit LGTreeMap;

{$mode objfpc}{$H+}
{$INLINE ON}{$WARN 6058 off : }
{$MODESWITCH NESTEDPROCVARS}

interface

uses

  SysUtils,
  LGUtils,
  {%H-}LGHelpers,
  LGAbstractContainer,
  LGAvlTree;

type

  { TGCustomTreeMap:  common tree map abstract ancestor class }
  generic TGCustomTreeMap<TKey, TValue> = class abstract(specialize TGCustomMap<TKey, TValue>)
  public
  type
    TCustomTreeMap = specialize TGCustomTreeMap<TKey, TValue>;

  protected
  type
    TTree          = specialize TGCustomAvlTree<TKey, TEntry>;
    PNode          = TTree.PNode;

    TKeyEnumerable = class(TCustomKeyEnumerable)
    protected
      FEnum: TTree.TEnumerator;
      function  GetCurrent: TKey; override;
    public
      constructor Create(aMap: TCustomTreeMap; aReverse: Boolean = False);
      destructor Destroy; override;
      function  MoveNext: Boolean; override;
      procedure Reset; override;
    end;

    TValueEnumerable = class(TCustomValueEnumerable)
    protected
      FEnum: TTree.TEnumerator;
      function  GetCurrent: TValue; override;
    public
      constructor Create(aMap: TCustomTreeMap; aReverse: Boolean = False);
      destructor Destroy; override;
      function  MoveNext: Boolean; override;
      procedure Reset; override;
    end;

    TEntryEnumerable = class(TCustomEntryEnumerable)
    protected
      FEnum: TTree.TEnumerator;
      function  GetCurrent: TEntry; override;
    public
      constructor Create(aMap: TCustomTreeMap; aReverse: Boolean = False);
      destructor Destroy; override;
      function  MoveNext: Boolean; override;
      procedure Reset; override;
    end;

    TKeyTailEnumerable = class(TCustomKeyEnumerable)
    protected
      FEnum: TTree.TEnumerator;
      function  GetCurrent: TKey; override;
    public
      constructor Create(constref aLowBound: TKey; aMap: TCustomTreeMap; aInclusive: Boolean);
      destructor Destroy; override;
      function  MoveNext: Boolean; override;
      procedure Reset; override;
    end;

  var
    FTree: TTree;
    function  GetCount: SizeInt; override;
    function  GetCapacity: SizeInt; override;
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
    function  FindNearestLT(constref aPattern: TKey; out aKey: TKey): Boolean;
    function  FindNearestLE(constref aPattern: TKey; out aKey: TKey): Boolean;
    function  FindNearestGT(constref aPattern: TKey; out aKey: TKey): Boolean;
    function  FindNearestGE(constref aPattern: TKey; out aKey: TKey): Boolean;
  public
    destructor Destroy; override;
    function ReverseKeys: IKeyEnumerable;
    function ReverseValues: IValueEnumerable;
    function ReverseEntries: IEntryEnumerable;
    function FindFirstKey(out aKey: TKey): Boolean;
    function FirstKey: TKeyOptional;
    function FindLastKey(out aKey: TKey): Boolean;
    function LastKey: TKeyOptional;
    function FindFirstValue(out aValue: TValue): Boolean;
    function FirstValue: TValueOptional;
    function FindLastValue(out aValue: TValue): Boolean;
    function LastValue: TValueOptional;
    function FindMin(out aKey: TKey): Boolean; inline;
    function Min: TKeyOptional; inline;
    function FindMax(out aKey: TKey): Boolean; inline;
    function Max: TKeyOptional; inline;
  { returns True if exists key whose value greater then or equal to aKey (depending on aInclusive) }
    function FindCeil(constref aKey: TKey; out aCeil: TKey; aInclusive: Boolean = True): Boolean;
  { returns True if exists key whose value less then aKey (or equal to aKey, depending on aInclusive) }
    function FindFloor(constref aKey: TKey; out aFloor: TKey; aInclusive: Boolean = False): Boolean;
  { enumerates keys which are strictly less than(if not aInclusive) aHighBound }
    function Head(constref aHighBound: TKey; aInclusive: Boolean = False): IKeyEnumerable; virtual; abstract;
  { enumerates keys whose are greater than or equal to aLowBound(if aInclusive) }
    function Tail(constref aLowBound: TKey; aInclusive: Boolean = True): IKeyEnumerable;
  { enumerates keys which are greater than or equal to aLowBound and strictly less than aHighBound(by default) }
    function Range(constref aLowBound, aHighBound: TKey; aIncludeBounds: TRangeBounds = [rbLow]): IKeyEnumerable;
      virtual; abstract;
  { returns sorted map whose keys are strictly less than(if not aInclusive) aHighBound }
    function HeadMap(constref aHighBound: TKey; aInclusive: Boolean = False): TCustomTreeMap; virtual; abstract;
  { returns sorted map whose keys are greater than or equal to(if aInclusive) aLowBound }
    function TailMap(constref aLowBound: TKey; aInclusive: Boolean = True): TCustomTreeMap; virtual; abstract;
  { returns sorted map whose keys are greater than or equal to aLowBound and strictly less than
    aHighBound(by default) }
    function SubMap(constref aLowBound, aHighBound: TKey; aIncludeBounds: TRangeBounds = [rbLow]): TCustomTreeMap;
      virtual; abstract;
  end;


  { TGBaseTreeMap implements sorted map;
     functor TKeyCmpRel (key comparision relation) must provide:
       class function Compare([const[ref]] L, R: TKey): SizeInt;  }
  generic TGBaseTreeMap<TKey, TValue, TKeyCmpRel> = class(specialize TGCustomTreeMap<TKey, TValue>)
  protected
  type
    TBaseTree  = specialize TGAvlTree<TKey, TEntry, TKeyCmpRel>;

    TKeyHeadEnumerable = class(TCustomKeyEnumerable)
    private
      FEnum: TTree.TEnumerator;
      FHighBound: TKey;
      FInclusive,
      FDone: Boolean;
    protected
      function  GetCurrent: TKey; override;
    public
      constructor Create(constref aHighBound: TKey; aMap: TCustomTreeMap; aInclusive: Boolean); overload;
      destructor Destroy; override;
      function  MoveNext: Boolean; override;
      procedure Reset; override;
    end;

    TKeyRangeEnumerable = class(TKeyHeadEnumerable)
      constructor Create(constref aLowBound, aHighBound: TKey; aMap: TCustomTreeMap; aBounds: TRangeBounds); overload;
    end;

    class function DoCompare(constref L, R: TKey): SizeInt; static;
  public
  type
    TComparator = specialize TGCompare<TKey>;
    class function Comparator: TComparator; static; inline;
    constructor Create;
    constructor Create(aCapacity: SizeInt);
    constructor Create(constref a: array of TEntry);
    constructor Create(e: IEntryEnumerable);
    constructor CreateCopy(aMap: TGBaseTreeMap);
    function Clone: TGBaseTreeMap; override;
    function Head(constref aHighBound: TKey; aInclusive: Boolean = False): IKeyEnumerable; override;
    function Range(constref aLowBound, aHighBound: TKey; aIncludeBounds: TRangeBounds = [rbLow]): IKeyEnumerable;
      override;
    function HeadMap(constref aHighBound: TKey; aInclusive: Boolean = False): TGBaseTreeMap; override;
    function TailMap(constref aLowBound: TKey; aInclusive: Boolean = True): TGBaseTreeMap; override;
    function SubMap(constref aLowBound, aHighBound: TKey; aIncludeBounds: TRangeBounds = [rbLow]): TGBaseTreeMap;
      override;
  end;

  { TGTreeMap implements sorted map; it assumes that type TKey implements TKeyCmpRel}
  generic TGTreeMap<TKey, TValue> = class(specialize TGBaseTreeMap<TKey, TValue, TKey>);

  { TGObjectTreeMap
    note: for equality comparision of Values used TObjectHelper from LGHelpers }
  generic TGObjectTreeMap<TKey, TValue, TKeyCmpRel> = class(specialize TGBaseTreeMap<TKey, TValue, TKeyCmpRel>)
  protected
    FOwnsKeys: Boolean;
    FOwnsValues: Boolean;
    procedure EntryRemoving(p: PEntry);
    procedure SetOwnership(aOwns: TMapObjOwnership);
    function  DoRemove(constref aKey: TKey): Boolean; override;
    function  DoRemoveIf(aTest: TKeyTest): SizeInt; override;
    function  DoRemoveIf(aTest: TOnKeyTest): SizeInt; override;
    function  DoRemoveIf(aTest: TNestKeyTest): SizeInt; override;
    procedure DoClear; override;
    function  DoSetValue(constref aKey: TKey; constref aNewValue: TValue): Boolean; override;
    function  DoAddOrSetValue(const aKey: TKey; const aValue: TValue): Boolean; override;
  public
    constructor Create(aOwns: TMapObjOwnership = OWNS_BOTH);
    constructor Create(aCapacity: SizeInt; aOwns: TMapObjOwnership = OWNS_BOTH);
    constructor Create(constref a: array of TEntry; aOwns: TMapObjOwnership = OWNS_BOTH);
    constructor Create(e: IEntryEnumerable; aOwns: TMapObjOwnership = OWNS_BOTH);
    constructor CreateCopy(aMap: TGObjectTreeMap);
    function  Clone: TGObjectTreeMap; override;
    property  OwnsKeys: Boolean read FOwnsKeys write FOwnsKeys;
    property  OwnsValues: Boolean read FOwnsValues write FOwnsValues;
  end;

  generic TGObjTreeMap<TKey, TValue> = class(specialize TGObjectTreeMap<TKey, TValue, TKey>);

  { TGComparableTreeMap implements sorted map; it assumes that type T has defined comparision operators }
  generic TGComparableTreeMap<TKey, TValue> = class(specialize TGCustomTreeMap<TKey, TValue>)
  protected
  type
    TComparableTree = specialize TGComparableAvlTree<TKey, TEntry>;

    TKeyHeadEnumerable = class(TCustomKeyEnumerable)
    protected
      FEnum: TTree.TEnumerator;
      FHighBound: TKey;
      FInclusive,
      FDone: Boolean;
      function  GetCurrent: TKey; override;
    public
      constructor Create(constref aHighBound: TKey; aMap: TCustomTreeMap; aInclusive: Boolean); overload;
      destructor Destroy; override;
      function  MoveNext: Boolean; override;
      procedure Reset; override;
    end;

    TKeyRangeEnumerable = class(TKeyHeadEnumerable)
      constructor Create(constref aLowBound, aHighBound: TKey; aMap: TCustomTreeMap; aBounds: TRangeBounds); overload;
    end;

    class function DoCompare(constref L, R: TKey): SizeInt; static;
  public
  type
    TComparator = specialize TGCompare<TKey>;
    class function Comparator: TComparator; static; inline;
    constructor Create;
    constructor Create(aCapacity: SizeInt);
    constructor Create(constref a: array of TEntry);
    constructor Create(e: IEntryEnumerable);
    constructor CreateCopy(aMap: TGComparableTreeMap);
    function Clone: TGComparableTreeMap; override;
    function Head(constref aHighBound: TKey; aInclusive: Boolean = False): IKeyEnumerable; override;
    function Range(constref aLowBound, aHighBound: TKey; aIncludeBounds: TRangeBounds = [rbLow]): IKeyEnumerable;
      override;
    function HeadMap(constref aHighBound: TKey; aInclusive: Boolean = False): TGComparableTreeMap; override;
    function TailMap(constref aLowBound: TKey; aInclusive: Boolean = True): TGComparableTreeMap; override;
    function SubMap(constref aLowBound, aHighBound: TKey; aIncludeBounds: TRangeBounds = [rbLow]): TGComparableTreeMap;
      override;
  end;

  { TGRegularTreeMap implements sorted map with regular comparator }
  generic TGRegularTreeMap<TKey, TValue> = class(specialize TGCustomTreeMap<TKey, TValue>)
  public
  type
    TComparator = specialize TGCompare<TKey>;

  protected
  type
    TRegularTree  = specialize TGRegularAvlTree<TKey, TEntry>;

    TKeyHeadEnumerable = class(TCustomKeyEnumerable)
    protected
      FEnum: TTree.TEnumerator;
      FCompare: TComparator;
      FHighBound: TKey;
      FInclusive,
      FDone: Boolean;
      function  GetCurrent: TKey; override;
    public
      constructor Create(constref aHighBound: TKey; aMap: TCustomTreeMap; aInclusive: Boolean); overload;
      destructor Destroy; override;
      function  MoveNext: Boolean; override;
      procedure Reset; override;
    end;

    TKeyRangeEnumerable = class(TKeyHeadEnumerable)
      constructor Create(constref aLowBound, aHighBound: TKey; aMap: TCustomTreeMap; aBounds: TRangeBounds); overload;
    end;

  public
    constructor Create;
    constructor Create(c: TComparator);
    constructor Create(aCapacity: SizeInt; c: TComparator);
    constructor Create(constref a: array of TEntry; c: TComparator);
    constructor Create(e: IEntryEnumerable; c: TComparator);
    constructor CreateCopy(aMap: TGRegularTreeMap);
    function Comparator: TComparator; inline;
    function Clone: TGRegularTreeMap; override;
    function Head(constref aHighBound: TKey; aInclusive: Boolean = False): IKeyEnumerable; override;
    function Range(constref aLowBound, aHighBound: TKey; aIncludeBounds: TRangeBounds = [rbLow]): IKeyEnumerable;
      override;
    function HeadMap(constref aHighBound: TKey; aInclusive: Boolean = False): TGRegularTreeMap; override;
    function TailMap(constref aLowBound: TKey; aInclusive: Boolean = True): TGRegularTreeMap; override;
    function SubMap(constref aLowBound, aHighBound: TKey; aIncludeBounds: TRangeBounds = [rbLow]): TGRegularTreeMap;
      override;
  end;

  { TGObjectRegularTreeMap
    note: for equality comparision of Values used TObjectHelper from LGHelpers }
  generic TGObjectRegularTreeMap<TKey, TValue> = class(specialize TGRegularTreeMap<TKey, TValue>)
  protected
    FOwnsKeys: Boolean;
    FOwnsValues: Boolean;
    procedure EntryRemoving(p: PEntry);
    procedure SetOwnership(aOwns: TMapObjOwnership);
    function  DoRemove(constref aKey: TKey): Boolean; override;
    function  DoRemoveIf(aTest: TKeyTest): SizeInt; override;
    function  DoRemoveIf(aTest: TOnKeyTest): SizeInt; override;
    function  DoRemoveIf(aTest: TNestKeyTest): SizeInt; override;
    procedure DoClear; override;
    function  DoSetValue(constref aKey: TKey; constref aNewValue: TValue): Boolean; override;
    function  DoAddOrSetValue(const aKey: TKey; const aValue: TValue): Boolean; override;
  public
    constructor Create(aOwns: TMapObjOwnership = OWNS_BOTH);
    constructor Create(c: TComparator; aOwns: TMapObjOwnership = OWNS_BOTH);
    constructor Create(aCapacity: SizeInt; c: TComparator; aOwns: TMapObjOwnership = OWNS_BOTH);
    constructor Create(constref a: array of TEntry; c: TComparator; aOwns: TMapObjOwnership = OWNS_BOTH);
    constructor Create(e: IEntryEnumerable; c: TComparator; aOwns: TMapObjOwnership = OWNS_BOTH);
    constructor CreateCopy(aMap: TGObjectRegularTreeMap);
    function  Clone: TGObjectRegularTreeMap; override;
    property  OwnsKeys: Boolean read FOwnsKeys write FOwnsKeys;
    property  OwnsValues: Boolean read FOwnsValues write FOwnsValues;
  end;

  { TGDelegatedTreeMap implements sorted map with delegated comparator }
  generic TGDelegatedTreeMap<TKey, TValue> = class(specialize TGCustomTreeMap<TKey, TValue>)
  public
  type
    TComparator = specialize TGOnCompare<TKey>;

  protected
  type
    TDelegatedTree  = specialize TGDelegatedAvlTree<TKey, TEntry>;

    TKeyHeadEnumerable = class(TCustomKeyEnumerable)
    protected
      FEnum: TTree.TEnumerator;
      FCompare: TComparator;
      FHighBound: TKey;
      FInclusive,
      FDone: Boolean;
      function  GetCurrent: TKey; override;
    public
      constructor Create(constref aHighBound: TKey; aMap: TCustomTreeMap; aInclusive: Boolean); overload;
      destructor Destroy; override;
      function  MoveNext: Boolean; override;
      procedure Reset; override;
    end;

    TKeyRangeEnumerable = class(TKeyHeadEnumerable)
      constructor Create(constref aLowBound, aHighBound: TKey; aMap: TCustomTreeMap; aBounds: TRangeBounds); overload;
    end;

  public
    constructor Create;
    constructor Create(c: TComparator);
    constructor Create(aCapacity: SizeInt; c: TComparator);
    constructor Create(constref a: array of TEntry; c: TComparator);
    constructor Create(e: IEntryEnumerable; c: TComparator);
    constructor CreateCopy(aMap: TGDelegatedTreeMap);
    function Comparator: TComparator;
    function Clone: TGDelegatedTreeMap; override;

    function Head(constref aHighBound: TKey; aInclusive: Boolean = False): IKeyEnumerable; override;
    function Range(constref aLowBound, aHighBound: TKey; aIncludeBounds: TRangeBounds = [rbLow]): IKeyEnumerable;
      override;
    function HeadMap(constref aHighBound: TKey; aInclusive: Boolean = False): TGDelegatedTreeMap; override;
    function TailMap(constref aLowBound: TKey; aInclusive: Boolean = True): TGDelegatedTreeMap; override;
    function SubMap(constref aLowBound, aHighBound: TKey; aIncludeBounds: TRangeBounds = [rbLow]): TGDelegatedTreeMap;
      override;
  end;

  { TGObjectDelegatedTreeMap
    note: for equality comparision of Values used TObjectHelper from LGHelpers }
  generic TGObjectDelegatedTreeMap<TKey, TValue> = class(specialize TGDelegatedTreeMap<TKey, TValue>)
  protected
    FOwnsKeys: Boolean;
    FOwnsValues: Boolean;
    procedure EntryRemoving(p: PEntry);
    procedure SetOwnership(aOwns: TMapObjOwnership);
    function  DoRemove(constref aKey: TKey): Boolean; override;
    function  DoRemoveIf(aTest: TKeyTest): SizeInt; override;
    function  DoRemoveIf(aTest: TOnKeyTest): SizeInt; override;
    function  DoRemoveIf(aTest: TNestKeyTest): SizeInt; override;
    procedure DoClear; override;
    function  DoSetValue(constref aKey: TKey; constref aNewValue: TValue): Boolean; override;
    function  DoAddOrSetValue(const aKey: TKey; const aValue: TValue): Boolean; override;
  public
    constructor Create(aOwns: TMapObjOwnership = OWNS_BOTH);
    constructor Create(c: TComparator; aOwns: TMapObjOwnership = OWNS_BOTH);
    constructor Create(aCapacity: SizeInt; c: TComparator; aOwns: TMapObjOwnership = OWNS_BOTH);
    constructor Create(constref a: array of TEntry; c: TComparator; aOwns: TMapObjOwnership = OWNS_BOTH);
    constructor Create(e: IEntryEnumerable; c: TComparator; aOwns: TMapObjOwnership = OWNS_BOTH);
    constructor CreateCopy(aMap: TGObjectDelegatedTreeMap);
    function  Clone: TGObjectDelegatedTreeMap; override;
    property  OwnsKeys: Boolean read FOwnsKeys write FOwnsKeys;
    property  OwnsValues: Boolean read FOwnsValues write FOwnsValues;
  end;

implementation
{$B-}{$COPERATORS ON}

{ TGCustomTreeMap.TKeyEnumerable }

function TGCustomTreeMap.TKeyEnumerable.GetCurrent: TKey;
begin
  Result := FEnum.Current^.Data.Key;
end;

constructor TGCustomTreeMap.TKeyEnumerable.Create(aMap: TCustomTreeMap; aReverse: Boolean);
begin
  inherited Create(aMap);
  if aReverse then
    FEnum := aMap.FTree.GetReverseEnumerator
  else
    FEnum := aMap.FTree.GetEnumerator;
end;

destructor TGCustomTreeMap.TKeyEnumerable.Destroy;
begin
  FEnum.Free;
  inherited;
end;

function TGCustomTreeMap.TKeyEnumerable.MoveNext: Boolean;
begin
  Result := FEnum.MoveNext;
end;

procedure TGCustomTreeMap.TKeyEnumerable.Reset;
begin
  FEnum.Reset;
end;

{ TGCustomTreeMap.TValueEnumerable }

function TGCustomTreeMap.TValueEnumerable.GetCurrent: TValue;
begin
  Result := FEnum.Current^.Data.Value;
end;

constructor TGCustomTreeMap.TValueEnumerable.Create(aMap: TCustomTreeMap; aReverse: Boolean);
begin
  inherited Create(aMap);
  if aReverse then
    FEnum := aMap.FTree.GetReverseEnumerator
  else
    FEnum := aMap.FTree.GetEnumerator;
end;

destructor TGCustomTreeMap.TValueEnumerable.Destroy;
begin
  FEnum.Free;
  inherited;
end;

function TGCustomTreeMap.TValueEnumerable.MoveNext: Boolean;
begin
  Result := FEnum.MoveNext;
end;

procedure TGCustomTreeMap.TValueEnumerable.Reset;
begin
  FEnum.Reset;
end;

{ TGBaseTreeMap.TEntryEnumerable }

function TGCustomTreeMap.TEntryEnumerable.GetCurrent: TEntry;
begin
  Result := FEnum.Current^.Data;
end;

constructor TGCustomTreeMap.TEntryEnumerable.Create(aMap: TCustomTreeMap; aReverse: Boolean);
begin
  inherited Create(aMap);
  if aReverse then
    FEnum := aMap.FTree.GetReverseEnumerator
  else
    FEnum := aMap.FTree.GetEnumerator;
end;

destructor TGCustomTreeMap.TEntryEnumerable.Destroy;
begin
  FEnum.Free;
  inherited;
end;

function TGCustomTreeMap.TEntryEnumerable.MoveNext: Boolean;
begin
  Result := FEnum.MoveNext;
end;

procedure TGCustomTreeMap.TEntryEnumerable.Reset;
begin
  FEnum.Reset;
end;

{ TGCustomTreeMap.TKeyTailEnumerable }

function TGCustomTreeMap.TKeyTailEnumerable.GetCurrent: TKey;
begin
  Result := FEnum.Current^.Data.Key;
end;

constructor TGCustomTreeMap.TKeyTailEnumerable.Create(constref aLowBound: TKey; aMap: TCustomTreeMap;
  aInclusive: Boolean);
begin
  inherited Create(aMap);
  FEnum := aMap.FTree.GetEnumeratorAt(aLowBound, aInclusive);
end;

destructor TGCustomTreeMap.TKeyTailEnumerable.Destroy;
begin
  FEnum.Free;
  inherited;
end;

function TGCustomTreeMap.TKeyTailEnumerable.MoveNext: Boolean;
begin
  Result := FEnum.MoveNext;
end;

procedure TGCustomTreeMap.TKeyTailEnumerable.Reset;
begin
  FEnum.Reset;
end;

{ TGCustomTreeMap }

function TGCustomTreeMap.GetCount: SizeInt;
begin
  Result := FTree.Count;
end;

function TGCustomTreeMap.GetCapacity: SizeInt;
begin
  Result := FTree.Capacity;
end;

function TGCustomTreeMap.Find(constref aKey: TKey): PEntry;
var
  Node: PNode;
begin
  Node := FTree.Find(aKey);
  if Node <> nil then
    Result := @Node^.Data
  else
    Result := nil;
end;

function TGCustomTreeMap.FindOrAdd(constref aKey: TKey; out p: PEntry): Boolean;
var
  Node: PNode;
begin
  Result := FTree.FindOrAdd(aKey, Node);
  p := @Node^.Data;
end;

function TGCustomTreeMap.DoExtract(constref aKey: TKey; out v: TValue): Boolean;
var
  Node: PNode;
begin
  Node := FTree.Find(aKey);
  Result := Node <> nil;
  if Result then
    begin
      v := Node^.Data.Value;
      FTree.RemoveNode(Node);
    end;
end;

function TGCustomTreeMap.DoRemoveIf(aTest: TKeyTest): SizeInt;
begin
  Result := FTree.RemoveIf(aTest);
end;

function TGCustomTreeMap.DoRemoveIf(aTest: TOnKeyTest): SizeInt;
begin
  Result := FTree.RemoveIf(aTest);
end;

function TGCustomTreeMap.DoRemoveIf(aTest: TNestKeyTest): SizeInt;
begin
  Result := FTree.RemoveIf(aTest);
end;

function TGCustomTreeMap.DoExtractIf(aTest: TKeyTest): TEntryArray;
var
  e: TExtractHelper;
begin
  e.Init;
  FTree.RemoveIf(aTest, @e.OnExtract);
  Result := e.Final;
end;

function TGCustomTreeMap.DoExtractIf(aTest: TOnKeyTest): TEntryArray;
var
  e: TExtractHelper;
begin
  e.Init;
  FTree.RemoveIf(aTest, @e.OnExtract);
  Result := e.Final;
end;

function TGCustomTreeMap.DoExtractIf(aTest: TNestKeyTest): TEntryArray;
var
  e: TExtractHelper;
begin
  e.Init;
  FTree.RemoveIf(aTest, @e.OnExtract);
  Result := e.Final;
end;

procedure TGCustomTreeMap.DoClear;
begin
  FTree.Clear;
end;

procedure TGCustomTreeMap.DoEnsureCapacity(aValue: SizeInt);
begin
  FTree.EnsureCapacity(aValue);
end;

procedure TGCustomTreeMap.DoTrimToFit;
begin
  FTree.TrimToFit;
end;

function TGCustomTreeMap.GetKeys: IKeyEnumerable;
begin
  Result := TKeyEnumerable.Create(Self);
end;

function TGCustomTreeMap.GetValues: IValueEnumerable;
begin
  Result := TValueEnumerable.Create(Self);
end;

function TGCustomTreeMap.GetEntries: IEntryEnumerable;
begin
  Result := TEntryEnumerable.Create(Self);
end;

function TGCustomTreeMap.FindNearestLT(constref aPattern: TKey; out aKey: TKey): Boolean;
var
  Node: PNode;
begin
  Node := FTree.FindLess(aPattern);
  Result := Node <> nil;
  if Result then
    aKey := Node^.Data.Key;
end;

function TGCustomTreeMap.FindNearestLE(constref aPattern: TKey; out aKey: TKey): Boolean;
var
  Node: PNode;
begin
  Node := FTree.FindLessOrEqual(aPattern);
  Result := Node <> nil;
  if Result then
    aKey := Node^.Data.Key;
end;

function TGCustomTreeMap.FindNearestGT(constref aPattern: TKey; out aKey: TKey): Boolean;
var
  Node: PNode;
begin
  Node := FTree.FindGreater(aPattern);
  Result := Node <> nil;
  if Result then
    aKey := Node^.Data.Key;
end;

function TGCustomTreeMap.FindNearestGE(constref aPattern: TKey; out aKey: TKey): Boolean;
var
  Node: PNode;
begin
  Node := FTree.FindGreaterOrEqual(aPattern);
  Result := Node <> nil;
  if Result then
    aKey := Node^.Data.Key;
end;

destructor TGCustomTreeMap.Destroy;
begin
  DoClear;
  FTree.Free;
  inherited;
end;

function TGCustomTreeMap.ReverseKeys: IKeyEnumerable;
begin
  BeginIteration;
  Result := TKeyEnumerable.Create(Self, True);
end;

function TGCustomTreeMap.ReverseValues: IValueEnumerable;
begin
  BeginIteration;
  Result := TValueEnumerable.Create(Self, True);
end;

function TGCustomTreeMap.ReverseEntries: IEntryEnumerable;
begin
  BeginIteration;
  Result := TEntryEnumerable.Create(Self, True);
end;

function TGCustomTreeMap.FindFirstKey(out aKey: TKey): Boolean;
var
  Node: PNode;
begin
  Node := FTree.Lowest;
  Result := Node <> nil;
  if Result then
    aKey := Node^.Data.Key;
end;

function TGCustomTreeMap.FirstKey: TKeyOptional;
var
  k: TKey;
begin
  if FindFirstKey(k) then
    Result.Assign(k);
end;

function TGCustomTreeMap.FindLastKey(out aKey: TKey): Boolean;
var
  Node: PNode;
begin
  Node := FTree.Highest;
  Result := Node <> nil;
  if Result then
    aKey := Node^.Data.Key;
end;

function TGCustomTreeMap.LastKey: TKeyOptional;
var
  k: TKey;
begin
  if FindLastKey(k) then
    Result.Assign(k);
end;

function TGCustomTreeMap.FindFirstValue(out aValue: TValue): Boolean;
var
  Node: PNode;
begin
  Node := FTree.Lowest;
  Result := Node <> nil;
  if Result then
    aValue := Node^.Data.Value;
end;

function TGCustomTreeMap.FirstValue: TValueOptional;
var
  v: TValue;
begin
  if FindFirstValue(v) then
    Result.Assign(v);
end;

function TGCustomTreeMap.FindLastValue(out aValue: TValue): Boolean;
var
  Node: PNode;
begin
  Node := FTree.Highest;
  Result := Node <> nil;
  if Result then
    aValue := Node^.Data.Value;
end;

function TGCustomTreeMap.LastValue: TValueOptional;
var
  v: TValue;
begin
  if FindLastValue(v) then
    Result.Assign(v);
end;

function TGCustomTreeMap.FindMin(out aKey: TKey): Boolean;
begin
  Result := FindFirstKey(aKey);
end;

function TGCustomTreeMap.Min: TKeyOptional;
begin
  Result := FirstKey;
end;

function TGCustomTreeMap.FindMax(out aKey: TKey): Boolean;
begin
  Result := FindLastKey(aKey);
end;

function TGCustomTreeMap.Max: TKeyOptional;
begin
  Result := LastKey;
end;

function TGCustomTreeMap.FindCeil(constref aKey: TKey; out aCeil: TKey; aInclusive: Boolean): Boolean;
begin
  if aInclusive then
    Result := FindNearestGE(aKey, aCeil)
  else
    Result := FindNearestGT(aKey, aCeil);
end;

function TGCustomTreeMap.FindFloor(constref aKey: TKey; out aFloor: TKey; aInclusive: Boolean): Boolean;
begin
  if aInclusive then
    Result := FindNearestLE(aKey, aFloor)
  else
    Result := FindNearestLT(aKey, aFloor);
end;

function TGCustomTreeMap.Tail(constref aLowBound: TKey; aInclusive: Boolean): IKeyEnumerable;
begin
  BeginIteration;
  Result := TKeyTailEnumerable.Create(aLowBound, Self, aInclusive);
end;

{ TGBaseTreeMap.TKeyHeadEnumerable }

function TGBaseTreeMap.TKeyHeadEnumerable.GetCurrent: TKey;
begin
  Result := FEnum.Current^.Data.Key;
end;

constructor TGBaseTreeMap.TKeyHeadEnumerable.Create(constref aHighBound: TKey; aMap: TCustomTreeMap;
  aInclusive: Boolean);
begin
  inherited Create(aMap);
  FEnum := aMap.FTree.GetEnumerator;
  FHighBound := aHighBound;
  FInclusive := aInclusive;
end;

destructor TGBaseTreeMap.TKeyHeadEnumerable.Destroy;
begin
  FEnum.Free;
  inherited;
end;

function TGBaseTreeMap.TKeyHeadEnumerable.MoveNext: Boolean;
begin
  if FDone or not FEnum.MoveNext then
    exit(False);
  if FInclusive then
    Result := TKeyCmpRel.Compare(FEnum.Current^.Data.Key, FHighBound) <= 0
  else
    Result := TKeyCmpRel.Compare(FEnum.Current^.Data.Key, FHighBound) < 0;
  FDone := not Result;
end;

procedure TGBaseTreeMap.TKeyHeadEnumerable.Reset;
begin
  FEnum.Reset;
  FDone := False;
end;

{ TGBaseTreeMap.TKeyRangeEnumerable }

constructor TGBaseTreeMap.TKeyRangeEnumerable.Create(constref aLowBound, aHighBound: TKey;
  aMap: TCustomTreeMap; aBounds: TRangeBounds);
begin
  inherited Create(aMap);
  FEnum := aMap.FTree.GetEnumeratorAt(aLowBound, rbLow in aBounds);
  FHighBound := aHighBound;
  FInclusive := rbHigh in aBounds;
end;

{ TGBaseTreeMap }

class function TGBaseTreeMap.DoCompare(constref L, R: TKey): SizeInt;
begin
  Result := TKeyCmpRel.Compare(L, R);
end;

class function TGBaseTreeMap.Comparator: TComparator;
begin
  Result := @DoCompare;
end;

constructor TGBaseTreeMap.Create;
begin
  FTree := TBaseTree.Create;
end;

constructor TGBaseTreeMap.Create(aCapacity: SizeInt);
begin
  FTree := TBaseTree.Create(aCapacity);
end;

constructor TGBaseTreeMap.Create(constref a: array of TEntry);
begin
  FTree := TBaseTree.Create;
  DoAddAll(a);
end;

constructor TGBaseTreeMap.Create(e: IEntryEnumerable);
begin
  FTree := TBaseTree.Create;
  DoAddAll(e);
end;

constructor TGBaseTreeMap.CreateCopy(aMap: TGBaseTreeMap);
begin
  FTree := TBaseTree(aMap.FTree).Clone;
end;

function TGBaseTreeMap.Clone: TGBaseTreeMap;
begin
  Result := TGBaseTreeMap.CreateCopy(Self);
end;

function TGBaseTreeMap.Head(constref aHighBound: TKey; aInclusive: Boolean): IKeyEnumerable;
begin
  BeginIteration;
  Result := TKeyHeadEnumerable.Create(aHighBound, Self, aInclusive);
end;

function TGBaseTreeMap.Range(constref aLowBound, aHighBound: TKey; aIncludeBounds: TRangeBounds): IKeyEnumerable;
begin
  BeginIteration;
  Result := TKeyRangeEnumerable.Create(aLowBound, aHighBound, Self, aIncludeBounds);
end;

function TGBaseTreeMap.HeadMap(constref aHighBound: TKey; aInclusive: Boolean): TGBaseTreeMap;
begin
  Result := TGBaseTreeMap.Create;
  with TKeyHeadEnumerable.Create(aHighBound, Self, aInclusive) do
    try
      while MoveNext do
        Result.Add(FEnum.Current^.Data);
    finally
      Free;
    end;
end;

function TGBaseTreeMap.TailMap(constref aLowBound: TKey; aInclusive: Boolean): TGBaseTreeMap;
begin
  Result := TGBaseTreeMap.Create;
  with TKeyTailEnumerable.Create(aLowBound, Self, aInclusive) do
    try
      while MoveNext do
        Result.Add(FEnum.Current^.Data);
    finally
      Free;
    end;
end;

function TGBaseTreeMap.SubMap(constref aLowBound, aHighBound: TKey;
  aIncludeBounds: TRangeBounds): TGBaseTreeMap;
begin
  Result := TGBaseTreeMap.Create;
  with TKeyRangeEnumerable.Create(aLowBound, aHighBound, Self, aIncludeBounds) do
    try
      while MoveNext do
        Result.Add(FEnum.Current^.Data);
    finally
      Free;
    end;
end;

{ TGObjectTreeMap }

procedure TGObjectTreeMap.EntryRemoving(p: PEntry);
begin
  if OwnsKeys then
    TObject(p^.Key).Free;
  if OwnsValues then
    TObject(p^.Value).Free;
end;

procedure TGObjectTreeMap.SetOwnership(aOwns: TMapObjOwnership);
begin
  FOwnsKeys := moOwnsKeys in aOwns;
  FOwnsValues := moOwnsValues in aOwns;
end;

function TGObjectTreeMap.DoRemove(constref aKey: TKey): Boolean;
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

function TGObjectTreeMap.DoRemoveIf(aTest: TKeyTest): SizeInt;
begin
  Result := FTree.RemoveIf(aTest, @EntryRemoving);
end;

function TGObjectTreeMap.DoRemoveIf(aTest: TOnKeyTest): SizeInt;
begin
  Result := FTree.RemoveIf(aTest, @EntryRemoving);
end;

function TGObjectTreeMap.DoRemoveIf(aTest: TNestKeyTest): SizeInt;
begin
  Result := FTree.RemoveIf(aTest, @EntryRemoving);
end;

procedure TGObjectTreeMap.DoClear;
var
  Node: PNode;
begin
  if OwnsKeys or OwnsValues then
    for Node in FTree do
      begin
        if OwnsKeys then
          TObject(Node^.Data.Key).Free;
        if OwnsValues then
          TObject(Node^.Data.Value).Free;
      end;
  inherited;
end;

function TGObjectTreeMap.DoSetValue(constref aKey: TKey; constref aNewValue: TValue): Boolean;
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

function TGObjectTreeMap.DoAddOrSetValue(const aKey: TKey; const aValue: TValue): Boolean;
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

constructor TGObjectTreeMap.Create(aOwns: TMapObjOwnership);
begin
  inherited Create;
  SetOwnership(aOwns);
end;

constructor TGObjectTreeMap.Create(aCapacity: SizeInt; aOwns: TMapObjOwnership);
begin
  inherited Create(aCapacity);
  SetOwnership(aOwns);
end;

constructor TGObjectTreeMap.Create(constref a: array of TEntry; aOwns: TMapObjOwnership);
begin
  inherited Create(a);
  SetOwnership(aOwns);
end;

constructor TGObjectTreeMap.Create(e: IEntryEnumerable; aOwns: TMapObjOwnership);
begin
  inherited Create(e);
  SetOwnership(aOwns);
end;

constructor TGObjectTreeMap.CreateCopy(aMap: TGObjectTreeMap);
begin
  inherited CreateCopy(aMap);
  OwnsKeys := aMap.OwnsKeys;
  OwnsValues := aMap.OwnsValues;
end;

function TGObjectTreeMap.Clone: TGObjectTreeMap;
begin
  Result := TGObjectTreeMap.CreateCopy(Self)
end;


{ TGComparableTreeMap.TKeyHeadEnumerable }

function TGComparableTreeMap.TKeyHeadEnumerable.GetCurrent: TKey;
begin
  Result := FEnum.Current^.Data.Key;
end;

constructor TGComparableTreeMap.TKeyHeadEnumerable.Create(constref aHighBound: TKey; aMap: TCustomTreeMap;
  aInclusive: Boolean);
begin
  inherited Create(aMap);
  FEnum := aMap.FTree.GetEnumerator;
  FHighBound := aHighBound;
  FInclusive := aInclusive;
end;

destructor TGComparableTreeMap.TKeyHeadEnumerable.Destroy;
begin
  FEnum.Free;
  inherited;
end;

function TGComparableTreeMap.TKeyHeadEnumerable.MoveNext: Boolean;
begin
  if FDone or not FEnum.MoveNext then
    exit(False);
  if FInclusive then
    Result := FEnum.Current^.Data.Key <= FHighBound
  else
    Result := FEnum.Current^.Data.Key < FHighBound;
  FDone := not Result;
end;

procedure TGComparableTreeMap.TKeyHeadEnumerable.Reset;
begin
  FEnum.Reset;
  FDone := False;
end;

{ TGComparableTreeMap.TKeyRangeEnumerable }

constructor TGComparableTreeMap.TKeyRangeEnumerable.Create(constref aLowBound, aHighBound: TKey;
  aMap: TCustomTreeMap; aBounds: TRangeBounds);
begin
  inherited Create(aMap);
  FEnum := aMap.FTree.GetEnumeratorAt(aLowBound, rbLow in aBounds);
  FHighBound := aHighBound;
  FInclusive := rbHigh in aBounds;
end;

{ TGComparableTreeMap }

class function TGComparableTreeMap.DoCompare(constref L, R: TKey): SizeInt;
begin
  if L > R then
    Result := 1
  else
    if R > L then
      Result := -1
    else
      Result := 0;
end;

class function TGComparableTreeMap.Comparator: TComparator;
begin
  Result := @DoCompare;
end;

constructor TGComparableTreeMap.Create;
begin
  FTree := TComparableTree.Create;
end;

constructor TGComparableTreeMap.Create(aCapacity: SizeInt);
begin
  FTree := TComparableTree.Create(aCapacity);
end;

constructor TGComparableTreeMap.Create(constref a: array of TEntry);
begin
  Create;
  DoAddAll(a);
end;

constructor TGComparableTreeMap.Create(e: IEntryEnumerable);
begin
  Create;
  DoAddAll(e);
end;

constructor TGComparableTreeMap.CreateCopy(aMap: TGComparableTreeMap);
begin
  inherited Create;
  FTree := TComparableTree(aMap.FTree).Clone;
end;

function TGComparableTreeMap.Clone: TGComparableTreeMap;
begin
  Result := TGComparableTreeMap.CreateCopy(Self);
end;

function TGComparableTreeMap.Head(constref aHighBound: TKey; aInclusive: Boolean): IKeyEnumerable;
begin
  BeginIteration;
  Result := TKeyHeadEnumerable.Create(aHighBound, Self, aInclusive);
end;

function TGComparableTreeMap.Range(constref aLowBound, aHighBound: TKey;
  aIncludeBounds: TRangeBounds): IKeyEnumerable;
begin
  BeginIteration;
  Result := TKeyRangeEnumerable.Create(aLowBound, aHighBound, Self, aIncludeBounds);
end;

function TGComparableTreeMap.HeadMap(constref aHighBound: TKey; aInclusive: Boolean): TGComparableTreeMap;
begin
  Result := TGComparableTreeMap.Create;
  with TKeyHeadEnumerable.Create(aHighBound, Self, aInclusive) do
    try
      while MoveNext do
        Result.Add(FEnum.Current^.Data);
      finally
        Free;
      end;
end;

function TGComparableTreeMap.TailMap(constref aLowBound: TKey; aInclusive: Boolean): TGComparableTreeMap;
begin
  Result := TGComparableTreeMap.Create;
  with TKeyTailEnumerable.Create(aLowBound, Self, aInclusive) do
    try
      while MoveNext do
        Result.Add(FEnum.Current^.Data);
    finally
      Free;
    end;
end;

function TGComparableTreeMap.SubMap(constref aLowBound, aHighBound: TKey;
  aIncludeBounds: TRangeBounds): TGComparableTreeMap;
begin
  Result := TGComparableTreeMap.Create;
  with TKeyRangeEnumerable.Create(aLowBound, aHighBound, Self, aIncludeBounds) do
    try
      while MoveNext do
        Result.Add(FEnum.Current^.Data);
    finally
      Free;
    end;
end;

{ TGRegularTreeMap.TKeyHeadEnumerable }

function TGRegularTreeMap.TKeyHeadEnumerable.GetCurrent: TKey;
begin
  Result := FEnum.Current^.Data.Key;
end;

constructor TGRegularTreeMap.TKeyHeadEnumerable.Create(constref aHighBound: TKey; aMap: TCustomTreeMap;
  aInclusive: Boolean);
begin
  inherited Create(aMap);
  FEnum := aMap.FTree.GetEnumerator;
  FCompare := TRegularTree(aMap.FTree).Comparator;
  FHighBound := aHighBound;
  FInclusive := aInclusive;
end;

destructor TGRegularTreeMap.TKeyHeadEnumerable.Destroy;
begin
  FEnum.Free;
  inherited;
end;

function TGRegularTreeMap.TKeyHeadEnumerable.MoveNext: Boolean;
begin
  if FDone or not FEnum.MoveNext then
    exit(False);
  if FInclusive then
    Result := FCompare(FEnum.Current^.Data.Key, FHighBound) <= 0
  else
    Result := FCompare(FEnum.Current^.Data.Key, FHighBound) < 0;
  FDone := not Result;
end;

procedure TGRegularTreeMap.TKeyHeadEnumerable.Reset;
begin
  FEnum.Reset;
  FDone := False;
end;

{ TGRegularTreeMap.TKeyRangeEnumerable }

constructor TGRegularTreeMap.TKeyRangeEnumerable.Create(constref aLowBound, aHighBound: TKey;
  aMap: TCustomTreeMap; aBounds: TRangeBounds);
begin
  inherited Create(aMap);
  FEnum := aMap.FTree.GetEnumeratorAt(aLowBound, rbLow in aBounds);
  FCompare := TRegularTree(aMap.FTree).Comparator;
  FHighBound := aHighBound;
  FInclusive := rbHigh in aBounds;
end;

{ TGRegularTreeMap }

function TGRegularTreeMap.Comparator: TComparator;
begin
  Result := TRegularTree(FTree).Comparator;
end;

constructor TGRegularTreeMap.Create;
begin
  FTree := TRegularTree.Create(specialize TGDefaults<TKey>.Compare);
end;

constructor TGRegularTreeMap.Create(c: TComparator);
begin
  FTree := TRegularTree.Create(c);
end;

constructor TGRegularTreeMap.Create(aCapacity: SizeInt; c: TComparator);
begin
  FTree := TRegularTree.Create(aCapacity, c);
end;

constructor TGRegularTreeMap.Create(constref a: array of TEntry; c: TComparator);
begin
  Create(c);
  DoAddAll(a);
end;

constructor TGRegularTreeMap.Create(e: IEntryEnumerable; c: TComparator);
begin
  Create(c);
  DoAddAll(e);
end;

constructor TGRegularTreeMap.CreateCopy(aMap: TGRegularTreeMap);
begin
  FTree := TRegularTree(aMap.FTree).Clone;
end;

function TGRegularTreeMap.Clone: TGRegularTreeMap;
begin
  Result := TGRegularTreeMap.CreateCopy(Self);
end;

function TGRegularTreeMap.Head(constref aHighBound: TKey; aInclusive: Boolean): IKeyEnumerable;
begin
  BeginIteration;
  Result := TKeyHeadEnumerable.Create(aHighBound, Self, aInclusive);
end;

function TGRegularTreeMap.Range(constref aLowBound, aHighBound: TKey;
  aIncludeBounds: TRangeBounds): IKeyEnumerable;
begin
  BeginIteration;
  Result := TKeyRangeEnumerable.Create(aLowBound, aHighBound, Self, aIncludeBounds);
end;

function TGRegularTreeMap.HeadMap(constref aHighBound: TKey; aInclusive: Boolean): TGRegularTreeMap;
begin
  Result := TGRegularTreeMap.Create(Comparator);
  with TKeyHeadEnumerable.Create(aHighBound, Self, aInclusive) do
    try
      while MoveNext do
        Result.Add(FEnum.Current^.Data);
    finally
      Free;
    end;
end;

function TGRegularTreeMap.TailMap(constref aLowBound: TKey; aInclusive: Boolean): TGRegularTreeMap;
begin
  Result := TGRegularTreeMap.Create(Comparator);
  with TKeyTailEnumerable.Create(aLowBound, Self, aInclusive) do
    try
      while MoveNext do
        Result.Add(FEnum.Current^.Data);
    finally
      Free;
    end;
end;

function TGRegularTreeMap.SubMap(constref aLowBound, aHighBound: TKey;
  aIncludeBounds: TRangeBounds): TGRegularTreeMap;
begin
  Result := TGRegularTreeMap.Create(Comparator);
  with TKeyRangeEnumerable.Create(aLowBound, aHighBound, Self, aIncludeBounds) do
    try
      while MoveNext do
        Result.Add(FEnum.Current^.Data);
    finally
      Free;
    end;
end;

{ TGObjectRegularTreeMap }

procedure TGObjectRegularTreeMap.EntryRemoving(p: PEntry);
begin
  if OwnsKeys then
    TObject(p^.Key).Free;
  if OwnsValues then
    TObject(p^.Value).Free;
end;

procedure TGObjectRegularTreeMap.SetOwnership(aOwns: TMapObjOwnership);
begin
  OwnsKeys := moOwnsKeys in aOwns;
  OwnsValues := moOwnsValues in aOwns;
end;

function TGObjectRegularTreeMap.DoRemove(constref aKey: TKey): Boolean;
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

function TGObjectRegularTreeMap.DoRemoveIf(aTest: TKeyTest): SizeInt;
begin
  Result := FTree.RemoveIf(aTest, @EntryRemoving);
end;

function TGObjectRegularTreeMap.DoRemoveIf(aTest: TOnKeyTest): SizeInt;
begin
  Result := FTree.RemoveIf(aTest, @EntryRemoving);
end;

function TGObjectRegularTreeMap.DoRemoveIf(aTest: TNestKeyTest): SizeInt;
begin
  Result := FTree.RemoveIf(aTest, @EntryRemoving);
end;

procedure TGObjectRegularTreeMap.DoClear;
var
  Node: PNode;
begin
  if OwnsKeys or OwnsValues then
    for Node in FTree do
      begin
        if OwnsKeys then
          TObject(Node^.Data.Key).Free;
        if OwnsValues then
          TObject(Node^.Data.Value).Free;
      end;
  inherited;
end;

function TGObjectRegularTreeMap.DoSetValue(constref aKey: TKey; constref aNewValue: TValue): Boolean;
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

function TGObjectRegularTreeMap.DoAddOrSetValue(const aKey: TKey; const aValue: TValue): Boolean;
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

constructor TGObjectRegularTreeMap.Create(aOwns: TMapObjOwnership);
begin
  inherited Create;
  SetOwnership(aOwns);
end;

constructor TGObjectRegularTreeMap.Create(c: TComparator; aOwns: TMapObjOwnership);
begin
  inherited Create(c);
  SetOwnership(aOwns);
end;

constructor TGObjectRegularTreeMap.Create(aCapacity: SizeInt; c: TComparator; aOwns: TMapObjOwnership);
begin
  inherited Create(aCapacity, c);
  SetOwnership(aOwns);
end;

constructor TGObjectRegularTreeMap.Create(constref a: array of TEntry; c: TComparator; aOwns: TMapObjOwnership);
begin
  inherited Create(a, c);
  SetOwnership(aOwns);
end;

constructor TGObjectRegularTreeMap.Create(e: IEntryEnumerable; c: TComparator; aOwns: TMapObjOwnership);
begin
  inherited Create(e, c);
  SetOwnership(aOwns);
end;

constructor TGObjectRegularTreeMap.CreateCopy(aMap: TGObjectRegularTreeMap);
begin
  inherited CreateCopy(aMap);
  OwnsKeys := aMap.OwnsKeys;
  OwnsValues := aMap.OwnsValues;
end;

function TGObjectRegularTreeMap.Clone: TGObjectRegularTreeMap;
begin
  Result := TGObjectRegularTreeMap.CreateCopy(Self);
end;

{ TGDelegatedTreeMap.TKeyHeadEnumerable }

function TGDelegatedTreeMap.TKeyHeadEnumerable.GetCurrent: TKey;
begin
  Result := FEnum.Current^.Data.Key;
end;

constructor TGDelegatedTreeMap.TKeyHeadEnumerable.Create(constref aHighBound: TKey; aMap: TCustomTreeMap;
  aInclusive: Boolean);
begin
  inherited Create(aMap);
  FEnum := aMap.FTree.GetEnumerator;
  FCompare := TDelegatedTree(aMap.FTree).Comparator;
  FHighBound := aHighBound;
  FInclusive := aInclusive;
end;

destructor TGDelegatedTreeMap.TKeyHeadEnumerable.Destroy;
begin
  FEnum.Free;
  inherited;
end;

function TGDelegatedTreeMap.TKeyHeadEnumerable.MoveNext: Boolean;
begin
  if FDone or not FEnum.MoveNext then
    exit(False);
  if FInclusive then
    Result := FCompare(FEnum.Current^.Data.Key, FHighBound) <= 0
  else
    Result := FCompare(FEnum.Current^.Data.Key, FHighBound) < 0;
  FDone := not Result;
end;

procedure TGDelegatedTreeMap.TKeyHeadEnumerable.Reset;
begin
  FEnum.Reset;
  FDone := False;
end;

{ TGDelegatedTreeMap.TKeyRangeEnumerable }

constructor TGDelegatedTreeMap.TKeyRangeEnumerable.Create(constref aLowBound, aHighBound: TKey;
  aMap: TCustomTreeMap; aBounds: TRangeBounds);
begin
  inherited Create(aMap);
  FEnum := aMap.FTree.GetEnumeratorAt(aLowBound, rbLow in aBounds);
  FCompare := TDelegatedTree(aMap.FTree).Comparator;
  FHighBound := aHighBound;
  FInclusive := rbHigh in aBounds;
end;

{ TGDelegatedTreeMap }

constructor TGDelegatedTreeMap.Create;
begin
  FTree := TDelegatedTree.Create(specialize TGDefaults<TKey>.OnCompare);
end;

constructor TGDelegatedTreeMap.Create(c: TComparator);
begin
  FTree := TDelegatedTree.Create(c);
end;

constructor TGDelegatedTreeMap.Create(aCapacity: SizeInt; c: TComparator);
begin
  FTree := TDelegatedTree.Create(aCapacity, c);
end;

constructor TGDelegatedTreeMap.Create(constref a: array of TEntry; c: TComparator);
begin
  Create(c);
  DoAddAll(a);
end;

constructor TGDelegatedTreeMap.Create(e: IEntryEnumerable; c: TComparator);
begin
  Create(c);
  DoAddAll(e);
end;

constructor TGDelegatedTreeMap.CreateCopy(aMap: TGDelegatedTreeMap);
begin
  FTree := TDelegatedTree(aMap.FTree).Clone;
end;

function TGDelegatedTreeMap.Comparator: TComparator;
begin
  Result := TDelegatedTree(FTree).Comparator;
end;

function TGDelegatedTreeMap.Clone: TGDelegatedTreeMap;
begin
  Result := TGDelegatedTreeMap.CreateCopy(Self);
end;

function TGDelegatedTreeMap.Head(constref aHighBound: TKey; aInclusive: Boolean): IKeyEnumerable;
begin
  BeginIteration;
  Result := TKeyHeadEnumerable.Create(aHighBound, Self, aInclusive);
end;

function TGDelegatedTreeMap.Range(constref aLowBound, aHighBound: TKey;
  aIncludeBounds: TRangeBounds): IKeyEnumerable;
begin
  BeginIteration;
  Result := TKeyRangeEnumerable.Create(aLowBound, aHighBound, Self, aIncludeBounds);
end;

function TGDelegatedTreeMap.HeadMap(constref aHighBound: TKey; aInclusive: Boolean): TGDelegatedTreeMap;
begin
  Result := TGDelegatedTreeMap.Create(Comparator);
  with TKeyHeadEnumerable.Create(aHighBound, Self, aInclusive) do
    try
      while MoveNext do
        Result.Add(FEnum.Current^.Data);
    finally
      Free;
    end;
end;

function TGDelegatedTreeMap.TailMap(constref aLowBound: TKey; aInclusive: Boolean): TGDelegatedTreeMap;
begin
  Result := TGDelegatedTreeMap.Create(Comparator);
  with TKeyTailEnumerable.Create(aLowBound, Self, aInclusive) do
    try
      while MoveNext do
        Result.Add(FEnum.Current^.Data);
    finally
      Free;
    end;
end;

function TGDelegatedTreeMap.SubMap(constref aLowBound, aHighBound: TKey;
  aIncludeBounds: TRangeBounds): TGDelegatedTreeMap;
var
  k: TKey;
begin
  Result := TGDelegatedTreeMap.Create(Comparator);
  with TKeyRangeEnumerable.Create(aLowBound, aHighBound, Self, aIncludeBounds) do
    try
      while MoveNext do
        Result.Add(FEnum.Current^.Data);
    finally
      Free;
    end;
end;

{ TGObjectDelegatedTreeMap }

procedure TGObjectDelegatedTreeMap.EntryRemoving(p: PEntry);
begin
  if OwnsKeys then
    TObject(p^.Key).Free;
  if OwnsValues then
    TObject(p^.Value).Free;
end;

procedure TGObjectDelegatedTreeMap.SetOwnership(aOwns: TMapObjOwnership);
begin
  OwnsKeys := moOwnsKeys in aOwns;
  OwnsValues := moOwnsValues in aOwns;
end;

function TGObjectDelegatedTreeMap.DoRemove(constref aKey: TKey): Boolean;
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

function TGObjectDelegatedTreeMap.DoRemoveIf(aTest: TKeyTest): SizeInt;
begin
  Result := FTree.RemoveIf(aTest, @EntryRemoving);
end;

function TGObjectDelegatedTreeMap.DoRemoveIf(aTest: TOnKeyTest): SizeInt;
begin
  Result := FTree.RemoveIf(aTest, @EntryRemoving);
end;

function TGObjectDelegatedTreeMap.DoRemoveIf(aTest: TNestKeyTest): SizeInt;
begin
  Result := FTree.RemoveIf(aTest, @EntryRemoving);
end;

procedure TGObjectDelegatedTreeMap.DoClear;
var
  Node: PNode;
begin
  if OwnsKeys or OwnsValues then
    for Node in FTree do
      begin
        if OwnsKeys then
          TObject(Node^.Data.Key).Free;
        if OwnsValues then
          TObject(Node^.Data.Value).Free;
      end;
  inherited;
end;

function TGObjectDelegatedTreeMap.DoSetValue(constref aKey: TKey; constref aNewValue: TValue): Boolean;
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

function TGObjectDelegatedTreeMap.DoAddOrSetValue(const aKey: TKey; const aValue: TValue): Boolean;
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

constructor TGObjectDelegatedTreeMap.Create(aOwns: TMapObjOwnership);
begin
  inherited Create;
  SetOwnership(aOwns);
end;

constructor TGObjectDelegatedTreeMap.Create(c: TComparator; aOwns: TMapObjOwnership);
begin
  inherited Create(c);
  SetOwnership(aOwns);
end;

constructor TGObjectDelegatedTreeMap.Create(aCapacity: SizeInt; c: TComparator; aOwns: TMapObjOwnership);
begin
  inherited Create(aCapacity, c);
  SetOwnership(aOwns);
end;

constructor TGObjectDelegatedTreeMap.Create(constref a: array of TEntry; c: TComparator; aOwns: TMapObjOwnership);
begin
  inherited Create(a, c);
  SetOwnership(aOwns);
end;

constructor TGObjectDelegatedTreeMap.Create(e: IEntryEnumerable; c: TComparator; aOwns: TMapObjOwnership);
begin
  inherited Create(e, c);
  SetOwnership(aOwns);
end;

constructor TGObjectDelegatedTreeMap.CreateCopy(aMap: TGObjectDelegatedTreeMap);
begin
  inherited CreateCopy(aMap);
  OwnsKeys := aMap.OwnsKeys;
  OwnsValues := aMap.OwnsValues;
end;

function TGObjectDelegatedTreeMap.Clone: TGObjectDelegatedTreeMap;
begin
  Result := TGObjectDelegatedTreeMap.CreateCopy(Self);
end;

end.

