{****************************************************************************
*                                                                           *
*   This file is part of the LGenerics package.                             *
*   Generic sorted set implementations on the top of AVL tree.              *
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
unit LGTreeSet;

{$mode objfpc}{$H+}
{$INLINE ON}
{$MODESWITCH NESTEDPROCVARS}
{$MODESWITCH ADVANCEDRECORDS}

interface

uses

  SysUtils,
  LGUtils,
  {%H-}LGHelpers,
  LGCustomContainer,
  LGAvlTree;

type

  { TGCustomTreeSet: common abstract ancestor set class }
  generic TGCustomTreeSet<T> = class abstract(specialize TGCustomSet<T>)
  public
  type
    TCustomTreeSet = specialize TGCustomTreeSet<T>;

  protected
  type
    TTree = specialize TGCustomAvlTree<T, TEntry>;
    PNode = TTree.PNode;

    TEnumerator = class(TContainerEnumerator)
    protected
      FEnum: TTree.TEnumerator;
      function  GetCurrent: T; override;
    public
      constructor Create(aSet: TCustomTreeSet);
      destructor Destroy; override;
      function  MoveNext: Boolean; override;
      procedure Reset; override;
    end;

    TReverseEnumerable = class(TContainerEnumerable)
    protected
      FEnum: TTree.TEnumerator;
      function  GetCurrent: T; override;
    public
      constructor Create(aSet: TCustomTreeSet);
      destructor Destroy; override;
      function  MoveNext: Boolean; override;
      procedure Reset; override;
    end;

    TTailEnumerable = class(TContainerEnumerable)
    protected
      FEnum: TTree.TEnumeratorAt;
      function  GetCurrent: T; override;
    public
      constructor Create(constref aLowBound: T; aSet: TCustomTreeSet; aInclusive: Boolean);
      destructor Destroy; override;
      function  MoveNext: Boolean; override;
      procedure Reset; override;
    end;

  var
    FTree: TTree;
    function  GetCount: SizeInt; override;
    function  GetCapacity: SizeInt; override;
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
    function  FindNearestLT(constref aPattern: T; out aValue: T): Boolean;
    function  FindNearestLE(constref aPattern: T; out aValue: T): Boolean;
    function  FindNearestGT(constref aPattern: T; out aValue: T): Boolean;
    function  FindNearestGE(constref aPattern: T; out aValue: T): Boolean;
  public
    destructor Destroy; override;
    function Reverse: IEnumerable; override;
    function Contains(constref aValue: T): Boolean; override;
    function FindMin(out aValue: T): Boolean;
    function FindMax(out aValue: T): Boolean;
  { returns True if exists element whose value greater then or equal to aValue (depend on aInclusive) }
    function FindCeil(constref aValue: T; out aCeil: T; aInclusive: Boolean = True): Boolean;
  { returns True if exists element whose value less then aBound (or equal to aValue depend on aInclusive) }
    function FindFloor(constref aValue: T; out aFloor: T; aInclusive: Boolean = False): Boolean;
  { enumerates values whose are strictly less than(if not aInclusive) aHighBound }
    function Head(constref aHighBound: T; aInclusive: Boolean = False): IEnumerable; virtual; abstract;
  { enumerates values whose are greater than or equal to(if aInclusive) aLowBound }
    function Tail(constref aLowBound: T; aInclusive: Boolean = True): IEnumerable;
  { enumerates values whose are greater than or equal to aLowBound and strictly less than aHighBound(by default)}
    function Range(constref aLowBound, aHighBound: T; aIncludeBounds: TRangeBounds = [rbLow]): IEnumerable;
       virtual; abstract;
  { returns sorted set whose items are strictly less than(if not aInclusive) aHighBound }
    function HeadSet(constref aHighBound: T; aInclusive: Boolean = False): TCustomTreeSet; virtual; abstract;
  { returns sorted set whose items are greater than or equal(if aInclusive) to aLowBound}
    function TailSet(constref aLowBound: T; aInclusive: Boolean = True): TCustomTreeSet; virtual; abstract;
  { returns sorted set whose items are greater than or equal to aLowBound and strictly less than
    aHighBound(by default) }
    function SubSet(constref aLowBound, aHighBound: T; aIncludeBounds: TRangeBounds = [rbLow]): TCustomTreeSet;
       virtual; abstract;
  end;

  { TGBaseTreeSet implements sorted set;
      functor TCmpRel (comparision relation) must provide:
        class function Compare([const[ref]] L, R: T): SizeInt; }
  generic TGBaseTreeSet<T, TCmpRel> = class(specialize TGCustomTreeSet<T>)
  protected
  type
    TBaseTree = specialize TGAvlTree<T, TEntry, TCmpRel>;

    THeadEnumerable = class(TContainerEnumerable)
    protected
      FEnum: TTree.TEnumerator;
      FHighBound: T;
      FInclusive,
      FDone: Boolean;
      function  GetCurrent: T; override;
    public
      constructor Create(constref aHighBound: T; aSet: TCustomTreeSet; aInclusive: Boolean); overload;
      destructor Destroy; override;
      function  MoveNext: Boolean; override;
      procedure Reset; override;
    end;

    TRangeEnumerable = class(THeadEnumerable)
      constructor Create(constref aLowBound, aHighBound: T; aSet: TCustomTreeSet; aBounds: TRangeBounds); overload;
    end;

    class function DoCompare(constref L, R: T): SizeInt; static;
  public
  type
    TComparator = TCompare;
    class function Comparator: TComparator; static; inline;
    constructor Create;
    constructor Create(aCapacity: SizeInt);
    constructor Create(constref a: array of T);
    constructor Create(e: IEnumerable);
    constructor CreateCopy(aSet: TGBaseTreeSet);
    function Clone: TGBaseTreeSet; override;
    function Head(constref aHighBound: T; aInclusive: Boolean = False): IEnumerable; override;
    function Range(constref aLowBound, aHighBound: T; aIncludeBounds: TRangeBounds = [rbLow]): IEnumerable;
      override;
    function HeadSet(constref aHighBound: T; aInclusive: Boolean = False): TGBaseTreeSet; override;
    function TailSet(constref aLowBound: T; aInclusive: Boolean = True): TGBaseTreeSet; override;
    function SubSet(constref aLowBound, aHighBound: T; aIncludeBounds: TRangeBounds = [rbLow]): TGBaseTreeSet;
      override;
  end;

  { TGTreeSet implements sorded set, it assumes that type T implements TCmpRel }
  generic TGTreeSet<T> = class(specialize TGBaseTreeSet<T, T>);

  { TGComparableTreeSet implements sorted set; it assumes that type T has defined comparision operators }
  generic TGComparableTreeSet<T> = class(specialize TGCustomTreeSet<T>)
  protected
  type
    TComparableTree = specialize TGComparableAvlTree<T, TEntry>;

    THeadEnumerable = class(TContainerEnumerable)
    private
      FEnum: TTree.TEnumerator;
      FHighBound: T;
      FInclusive,
      FDone: Boolean;
    protected
      function  GetCurrent: T; override;
    public
      constructor Create(constref aHighBound: T; aSet: TCustomTreeSet; aInclusive: Boolean); overload;
      destructor Destroy; override;
      function  MoveNext: Boolean; override;
      procedure Reset; override;
    end;

    TRangeEnumerable = class(THeadEnumerable)
      constructor Create(constref aLowBound, aHighBound: T; aSet: TCustomTreeSet; aBounds: TRangeBounds); overload;
    end;

    class function DoCompare(constref L, R: T): SizeInt; static;
  public
  type
    TComparator = TCompare;
    class function Comparator: TComparator; static; inline;
    constructor Create;
    constructor Create(aCapacity: SizeInt);
    constructor Create(constref a: array of T);
    constructor Create(e: IEnumerable);
    constructor CreateCopy(aSet: TGComparableTreeSet);
    function Clone: TGComparableTreeSet; override;
    function Head(constref aHighBound: T; aInclusive: Boolean = False): IEnumerable; override;
    function Range(constref aLowBound, aHighBound: T; aIncludeBounds: TRangeBounds = [rbLow]): IEnumerable;
      override;
    function HeadSet(constref aHighBound: T; aInclusive: Boolean = False): TGComparableTreeSet; override;
    function TailSet(constref aLowBound: T; aInclusive: Boolean = True): TGComparableTreeSet; override;
    function SubSet(constref aLowBound, aHighBound: T; aIncludeBounds: TRangeBounds = [rbLow]): TGComparableTreeSet;
      override;
  end;

  { TGObjectTreeSet }

  generic TGObjectTreeSet<T: class; TCmpRel> = class(specialize TGBaseTreeSet<T, TCmpRel>)
  private
    FOwnsObjects: Boolean;
  protected
    procedure NodeRemoved(p: PEntry);
    procedure DoClear; override;
    function  DoRemove(constref aValue: T): Boolean; override;
    function  DoRemoveIf(aTest: TTest): SizeInt; override;
    function  DoRemoveIf(aTest: TOnTest): SizeInt; override;
    function  DoRemoveIf(aTest: TNestTest): SizeInt; override;
  public
    constructor Create(aOwnsObjects: Boolean = True);
    constructor Create(aCapacity: SizeInt; aOwnsObjects: Boolean = True);
    constructor Create(constref a: array of T; aOwnsObjects: Boolean = True);
    constructor Create(e: IEnumerable; aOwnsObjects: Boolean = True);
    constructor CreateCopy(aSet: TGObjectTreeSet);
    function  Clone: TGObjectTreeSet; override;
    property  OwnsObjects: Boolean read FOwnsObjects write FOwnsObjects;
  end;

  { TGObjTreeSet assumes that type T implements TCmpRel}
  generic TGObjTreeSet<T: class> = class(specialize TGObjectTreeSet<T, T>);

  { TGRegularTreeSet implements sorted set with regular comparator }
  generic TGRegularTreeSet<T> = class(specialize TGCustomTreeSet<T>)
  protected
  type
    TRegularTree = specialize TGRegularAvlTree<T, TEntry>;

    THeadEnumerable = class(TContainerEnumerable)
    private
      FEnum: TTree.TEnumerator;
      FHighBound: T;
      FCompare: TCompare;
      FInclusive,
      FDone: Boolean;
    protected
      function  GetCurrent: T; override;
    public
      constructor Create(constref aHighBound: T; aSet: TCustomTreeSet; aInclusive: Boolean); overload;
      destructor Destroy; override;
      function  MoveNext: Boolean; override;
      procedure Reset; override;
    end;

    TRangeEnumerable = class(THeadEnumerable)
      constructor Create(constref aLowBound, aHighBound: T; aSet: TCustomTreeSet; aBounds: TRangeBounds); overload;
    end;

  public
  type
    TComparator = TCompare;
    constructor Create;
    constructor Create(c: TComparator);
    constructor Create(aCapacity: SizeInt; c: TComparator);
    constructor Create(constref a: array of T; c: TComparator);
    constructor Create(e: IEnumerable; c: TComparator);
    constructor CreateCopy(aSet: TGRegularTreeSet);
    function Comparator: TComparator; inline;
    function Clone: TGRegularTreeSet; override;
    function Head(constref aHighBound: T; aInclusive: Boolean = False): IEnumerable; override;
    function Range(constref aLowBound, aHighBound: T; aIncludeBounds: TRangeBounds = [rbLow]): IEnumerable;
      override;
    function HeadSet(constref aHighBound: T; aInclusive: Boolean = False): TGRegularTreeSet; override;
    function TailSet(constref aLowBound: T; aInclusive: Boolean = True): TGRegularTreeSet; override;
    function SubSet(constref aLowBound, aHighBound: T; aIncludeBounds: TRangeBounds = [rbLow]): TGRegularTreeSet;
      override;
  end;

  { TGObjectRegularTreeSet }

  generic TGObjectRegularTreeSet<T: class> = class(specialize TGRegularTreeSet<T>)
  private
    FOwnsObjects: Boolean;
  protected
    procedure NodeRemoved(p: PEntry);
    procedure DoClear; override;
    function  DoRemove(constref aValue: T): Boolean; override;
    function  DoRemoveIf(aTest: TTest): SizeInt; override;
    function  DoRemoveIf(aTest: TOnTest): SizeInt; override;
    function  DoRemoveIf(aTest: TNestTest): SizeInt; override;
  public
    constructor Create(aOwnsObjects: Boolean = True);
    constructor Create(c: TComparator; aOwnsObjects: Boolean = True);
    constructor Create(aCapacity: SizeInt; c: TComparator; aOwnsObjects: Boolean = True);
    constructor Create(constref a: array of T; c: TComparator; aOwnsObjects: Boolean = True);
    constructor Create(e: IEnumerable; c: TComparator; aOwnsObjects: Boolean = True);
    constructor CreateCopy(aSet: TGObjectRegularTreeSet);
    function  Clone: TGObjectRegularTreeSet; override;
    property  OwnsObjects: Boolean read FOwnsObjects write FOwnsObjects;
  end;

  { TGDelegatedTreeSet implements sorted set with delegated comparator }
  generic TGDelegatedTreeSet<T> = class(specialize TGCustomTreeSet<T>)
  protected
  type
    TDelegatedTree = specialize TGDelegatedAvlTree<T, TEntry>;

    THeadEnumerable = class(TContainerEnumerable)
    protected
      FEnum: TTree.TEnumerator;
      FHighBound: T;
      FCompare: TOnCompare;
      FInclusive,
      FDone: Boolean;
      function  GetCurrent: T; override;
    public
      constructor Create(constref aHighBound: T; aSet: TCustomTreeSet; aInclusive: Boolean); overload;
      destructor Destroy; override;
      function  MoveNext: Boolean; override;
      procedure Reset; override;
    end;

    TRangeEnumerable = class(THeadEnumerable)
      constructor Create(constref aLowBound, aHighBound: T; aSet: TCustomTreeSet; aBounds: TRangeBounds); overload;
    end;

  public
  type
    TComparator = TOnCompare;
    constructor Create;
    constructor Create(c: TComparator);
    constructor Create(aCapacity: SizeInt; c: TComparator);
    constructor Create(constref a: array of T; c: TComparator);
    constructor Create(e: IEnumerable; c: TComparator);
    constructor CreateCopy(aSet: TGDelegatedTreeSet);
    function Comparator: TComparator; inline;
    function Clone: TGDelegatedTreeSet; override;
    function Head(constref aHighBound: T; aInclusive: Boolean = False): IEnumerable; override;
    function Range(constref aLowBound, aHighBound: T; aIncludeBounds: TRangeBounds = [rbLow]): IEnumerable;
      override;
    function HeadSet(constref aHighBound: T; aInclusive: Boolean = False): TGDelegatedTreeSet; override;
    function TailSet(constref aLowBound: T; aInclusive: Boolean = True): TGDelegatedTreeSet; override;
    function SubSet(constref aLowBound, aHighBound: T; aIncludeBounds: TRangeBounds = [rbLow]): TGDelegatedTreeSet;
      override;
  end;

  { TGObjectDelegatedTreeSet }

  generic TGObjectDelegatedTreeSet<T: class> = class(specialize TGDelegatedTreeSet<T>)
  protected
    FOwnsObjects: Boolean;
    procedure NodeRemoved(p: PEntry);
    procedure DoClear; override;
    function  DoRemove(constref aValue: T): Boolean; override;
    function  DoRemoveIf(aTest: TTest): SizeInt; override;
    function  DoRemoveIf(aTest: TOnTest): SizeInt; override;
    function  DoRemoveIf(aTest: TNestTest): SizeInt; override;
  public
    constructor Create(aOwnsObjects: Boolean = True);
    constructor Create(aCompare: TOnCompare; aOwnsObjects: Boolean = True);
    constructor Create(aCapacity: SizeInt; aCompare: TOnCompare; aOwnsObjects: Boolean = True);
    constructor Create(constref a: array of T; aCompare: TOnCompare; aOwnsObjects: Boolean = True);
    constructor Create(e: IEnumerable; aCompare: TOnCompare; aOwnsObjects: Boolean = True);
    constructor CreateCopy(aSet: TGObjectDelegatedTreeSet);
    function  Clone: TGObjectDelegatedTreeSet; override;
    property  OwnsObjects: Boolean read FOwnsObjects write FOwnsObjects;
  end;

  { TGLiteTreeSet implements sorted set;
            functor TCmpRel (comparision relation) must provide:
        class function Compare([const[ref]] L, R: T): SizeInt; }
  generic TGLiteTreeSet<T, TCmpRel> = record
  private
  type
    TEntry = record
      Key: T;
    end;
    PEntry = ^TEntry;

    TTree = specialize TGLiteAvlTree<T, TEntry, TCmpRel>;

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
      FEnum: TTree.TEnumerator;
      function  GetCurrent: T; inline;
      procedure Init(constref aSet: TGLiteTreeSet); inline;
    public
      function  MoveNext: Boolean; inline;
      procedure Reset; inline;
      property  Current: T read GetCurrent;
    end;

  private
    FTree: TTree;
    function  GetCapacity: SizeInt; inline;
    function  GetCount: SizeInt; inline;
  public
    function  GetEnumerator: TEnumerator; inline;
    function  ToArray: TArray;
    function  IsEmpty: Boolean; inline;
    function  NonEmpty: Boolean; inline;
    procedure Clear; inline;
    procedure TrimToFit; inline;
    procedure EnsureCapacity(aValue: SizeInt); inline;
  { returns True if element added }
    function  Add(constref aValue: T): Boolean; inline;
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
    function  IsSuperset(constref aSet: TGLiteTreeSet): Boolean;
    function  IsSubset(constref aSet: TGLiteTreeSet): Boolean; inline;
    function  IsEqual(constref aSet: TGLiteTreeSet): Boolean;
    function  Intersecting(constref aSet: TGLiteTreeSet): Boolean; inline;
    procedure Intersect(constref aSet: TGLiteTreeSet);
    procedure Join(constref aSet: TGLiteTreeSet);
    procedure Subtract(constref aSet: TGLiteTreeSet);
    procedure SymmetricSubtract(constref aSet: TGLiteTreeSet);
    property  Count: SizeInt read GetCount;
    property  Capacity: SizeInt read GetCapacity;
  end;

implementation
{$B-}{$COPERATORS ON}

{ TGCustomTreeSet.TEnumerator }

function TGCustomTreeSet.TEnumerator.GetCurrent: T;
begin
  Result := FEnum.Current^.Data.Key;
end;

constructor TGCustomTreeSet.TEnumerator.Create(aSet: TCustomTreeSet);
begin
  inherited Create(aSet);
  FEnum := aSet.FTree.GetEnumerator;
end;

destructor TGCustomTreeSet.TEnumerator.Destroy;
begin
  FEnum.Free;
  inherited;
end;

function TGCustomTreeSet.TEnumerator.MoveNext: Boolean;
begin
  Result := FEnum.MoveNext;
end;

procedure TGCustomTreeSet.TEnumerator.Reset;
begin
  FEnum.Reset;
end;

{ TGCustomTreeSet.TReverseEnumerable }

function TGCustomTreeSet.TReverseEnumerable.GetCurrent: T;
begin
  Result := FEnum.Current^.Data.Key;
end;

constructor TGCustomTreeSet.TReverseEnumerable.Create(aSet: TCustomTreeSet);
begin
  inherited Create(aSet);
  FEnum := aSet.FTree.GetReverseEnumerator;
end;

destructor TGCustomTreeSet.TReverseEnumerable.Destroy;
begin
  FEnum.Free;
  inherited;
end;

function TGCustomTreeSet.TReverseEnumerable.MoveNext: Boolean;
begin
  Result := FEnum.MoveNext;
end;

procedure TGCustomTreeSet.TReverseEnumerable.Reset;
begin
  FEnum.Reset;
end;

{ TGCustomTreeSet.TTailEnumerable }

function TGCustomTreeSet.TTailEnumerable.GetCurrent: T;
begin
  Result := FEnum.Current^.Data.Key;
end;

constructor TGCustomTreeSet.TTailEnumerable.Create(constref aLowBound: T; aSet: TCustomTreeSet;
  aInclusive: Boolean);
begin
  inherited Create(aSet);
  FEnum := aSet.FTree.GetEnumeratorAt(aLowBound, aInclusive);
end;

destructor TGCustomTreeSet.TTailEnumerable.Destroy;
begin
  FEnum.Free;
  inherited;
end;

function TGCustomTreeSet.TTailEnumerable.MoveNext: Boolean;
begin
  Result := FEnum.MoveNext;
end;

procedure TGCustomTreeSet.TTailEnumerable.Reset;
begin
  FEnum.Reset;
end;

{ TGCustomTreeSet }

function TGCustomTreeSet.GetCount: SizeInt;
begin
  Result := FTree.Count;
end;

function TGCustomTreeSet.GetCapacity: SizeInt;
begin
  Result := FTree.Capacity;
end;

function TGCustomTreeSet.DoGetEnumerator: TCustomEnumerator;
begin
  Result := TEnumerator.Create(Self);
end;

procedure TGCustomTreeSet.DoClear;
begin
  FTree.Clear;
end;

procedure TGCustomTreeSet.DoTrimToFit;
begin
  FTree.TrimToFit;
end;

procedure TGCustomTreeSet.DoEnsureCapacity(aValue: SizeInt);
begin
  FTree.EnsureCapacity(aValue);
end;

function TGCustomTreeSet.DoAdd(constref aValue: T): Boolean;
var
  p: PNode;
begin
  Result := not FTree.FindOrAdd(aValue, p);
end;

function TGCustomTreeSet.DoExtract(constref aValue: T): Boolean;
begin
  Result := FTree.Remove(aValue);
end;

function TGCustomTreeSet.DoRemoveIf(aTest: TTest): SizeInt;
begin
  Result := FTree.RemoveIf(aTest);
end;

function TGCustomTreeSet.DoRemoveIf(aTest: TOnTest): SizeInt;
begin
  Result := FTree.RemoveIf(aTest);
end;

function TGCustomTreeSet.DoRemoveIf(aTest: TNestTest): SizeInt;
begin
  Result := FTree.RemoveIf(aTest);
end;

function TGCustomTreeSet.DoExtractIf(aTest: TTest): TArray;
var
  e: TExtractHelper;
begin
  e.Init;
  FTree.RemoveIf(aTest, @e.OnExtract);
  Result := e.Final;
end;

function TGCustomTreeSet.DoExtractIf(aTest: TOnTest): TArray;
var
  e: TExtractHelper;
begin
  e.Init;
  FTree.RemoveIf(aTest, @e.OnExtract);
  Result := e.Final;
end;

function TGCustomTreeSet.DoExtractIf(aTest: TNestTest): TArray;
var
  e: TExtractHelper;
begin
  e.Init;
  FTree.RemoveIf(aTest, @e.OnExtract);
  Result := e.Final;
end;

function TGCustomTreeSet.FindNearestLT(constref aPattern: T; out aValue: T): Boolean;
var
  Node: PNode;
begin
  Node := FTree.FindLess(aPattern);
  Result := Node <> nil;
  if Result then
    aValue := Node^.Data.Key;
end;

function TGCustomTreeSet.FindNearestLE(constref aPattern: T; out aValue: T): Boolean;
var
  Node: PNode;
begin
  Node := FTree.FindLessOrEqual(aPattern);
  Result := Node <> nil;
  if Result then
    aValue := Node^.Data.Key;
end;

function TGCustomTreeSet.FindNearestGT(constref aPattern: T; out aValue: T): Boolean;
var
  Node: PNode;
begin
  Node := FTree.FindGreater(aPattern);
  Result := Node <> nil;
  if Result then
    aValue := Node^.Data.Key;
end;

function TGCustomTreeSet.FindNearestGE(constref aPattern: T; out aValue: T): Boolean;
var
  Node: PNode;
begin
  Node := FTree.FindGreaterOrEqual(aPattern);
  Result := Node <> nil;
  if Result then
    aValue := Node^.Data.Key;
end;

destructor TGCustomTreeSet.Destroy;
begin
  DoClear;
  FTree.Free;
  inherited;
end;

function TGCustomTreeSet.Reverse: IEnumerable;
begin
  BeginIteration;
  Result := TReverseEnumerable.Create(Self);
end;

function TGCustomTreeSet.Contains(constref aValue: T): Boolean;
begin
  Result := FTree.Find(aValue) <> nil;
end;

function TGCustomTreeSet.FindMin(out aValue: T): Boolean;
var
  Node: PNode;
begin
  Node := FTree.Lowest;
  Result := Node <> nil;
  if Result then
    aValue := Node^.Data.Key;
end;

function TGCustomTreeSet.FindMax(out aValue: T): Boolean;
var
  Node: PNode;
begin
  Node := FTree.Highest;
  Result := Node <> nil;
  if Result then
    aValue := Node^.Data.Key;
end;

function TGCustomTreeSet.FindCeil(constref aValue: T; out aCeil: T; aInclusive: Boolean): Boolean;
begin
  if aInclusive then
    Result := FindNearestGE(aValue, aCeil)
  else
    Result := FindNearestGT(aValue, aCeil);
end;

function TGCustomTreeSet.FindFloor(constref aValue: T; out aFloor: T; aInclusive: Boolean): Boolean;
begin
  if aInclusive then
    Result := FindNearestLE(aValue, aFloor)
  else
    Result := FindNearestLT(aValue, aFloor);
end;

function TGCustomTreeSet.Tail(constref aLowBound: T; aInclusive: Boolean): IEnumerable;
begin
  BeginIteration;
  Result := TTailEnumerable.Create(aLowBound, Self, aInclusive);
end;

{ TGBaseTreeSet.THeadEnumerable }

function TGBaseTreeSet.THeadEnumerable.GetCurrent: T;
begin
  Result := FEnum.Current^.Data.Key;
end;

constructor TGBaseTreeSet.THeadEnumerable.Create(constref aHighBound: T; aSet: TCustomTreeSet;
  aInclusive: Boolean);
begin
  inherited Create(aSet);
  FEnum := aSet.FTree.GetEnumerator;
  FHighBound := aHighBound;
  FInclusive := aInclusive;
end;

destructor TGBaseTreeSet.THeadEnumerable.Destroy;
begin
  FEnum.Free;
  inherited;
end;

function TGBaseTreeSet.THeadEnumerable.MoveNext: Boolean;
begin
  if FDone or not FEnum.MoveNext then
    exit(False);
  if FInclusive then
    Result := TCmpRel.Compare(FEnum.Current^.Data.Key, FHighBound) <= 0
  else
    Result := TCmpRel.Compare(FEnum.Current^.Data.Key, FHighBound) < 0;
  FDone := not Result;
end;

procedure TGBaseTreeSet.THeadEnumerable.Reset;
begin
  FEnum.Reset;
  FDone := False;
end;

{ TGBaseTreeSet.TRangeEnumerable }

constructor TGBaseTreeSet.TRangeEnumerable.Create(constref aLowBound, aHighBound: T; aSet: TCustomTreeSet;
  aBounds: TRangeBounds);
begin
  inherited Create(aSet);
  FEnum := aSet.FTree.GetEnumeratorAt(aLowBound, rbLow in aBounds);
  FHighBound := aHighBound;
  FInclusive := rbHigh in aBounds;
end;

{ TGBaseTreeSet }

class function TGBaseTreeSet.DoCompare(constref L, R: T): SizeInt;
begin
  Result := TCmpRel.Compare(L, R);
end;

class function TGBaseTreeSet.Comparator: TComparator;
begin
  Result := @DoCompare;
end;

constructor TGBaseTreeSet.Create;
begin
  FTree := TBaseTree.Create;
end;

constructor TGBaseTreeSet.Create(aCapacity: SizeInt);
begin
  FTree := TBaseTree.Create(aCapacity);
end;

constructor TGBaseTreeSet.Create(constref a: array of T);
begin
  FTree := TBaseTree.Create;
  DoAddAll(a);
end;

constructor TGBaseTreeSet.Create(e: IEnumerable);
var
  o: TObject;
begin
  o := e._GetRef;
  if o is TGBaseTreeSet then
    CreateCopy(TGBaseTreeSet(o))
  else
    begin
      if o is TCustomSet then
        Create(TCustomSet(o).Count)
      else
        Create;
      DoAddAll(e);
    end;
end;

constructor TGBaseTreeSet.CreateCopy(aSet: TGBaseTreeSet);
begin
  FTree := TBaseTree(aSet.FTree).Clone;
end;

function TGBaseTreeSet.Clone: TGBaseTreeSet;
begin
  Result := TGBaseTreeSet.CreateCopy(Self);
end;

function TGBaseTreeSet.Head(constref aHighBound: T; aInclusive: Boolean): IEnumerable;
begin
  BeginIteration;
  Result := THeadEnumerable.Create(aHighBound, Self, aInclusive);
end;

function TGBaseTreeSet.Range(constref aLowBound, aHighBound: T; aIncludeBounds: TRangeBounds): IEnumerable;
begin
  BeginIteration;
  Result := TRangeEnumerable.Create(aLowBound, aHighBound, Self, aIncludeBounds);
end;

function TGBaseTreeSet.HeadSet(constref aHighBound: T; aInclusive: Boolean): TGBaseTreeSet;
var
  v: T;
begin
  Result := TGBaseTreeSet.Create;
  for v in Head(aHighBound, aInclusive) do
    Result.Add(v);
end;

function TGBaseTreeSet.TailSet(constref aLowBound: T; aInclusive: Boolean): TGBaseTreeSet;
var
  v: T;
begin
  Result := TGBaseTreeSet.Create;
  for v in Tail(aLowBound, aInclusive) do
    Result.Add(v);
end;

function TGBaseTreeSet.SubSet(constref aLowBound, aHighBound: T; aIncludeBounds: TRangeBounds): TGBaseTreeSet;
var
  v: T;
begin
    Result := TGBaseTreeSet.Create;
  for v in Range(aLowBound, aHighBound, aIncludeBounds) do
    Result.Add(v);
end;

{ TGComparableTreeSet.THeadEnumerable }

function TGComparableTreeSet.THeadEnumerable.GetCurrent: T;
begin
  Result := FEnum.Current^.Data.Key;
end;

constructor TGComparableTreeSet.THeadEnumerable.Create(constref aHighBound: T; aSet: TCustomTreeSet;
  aInclusive: Boolean);
begin
  inherited Create(aSet);
  FEnum := aSet.FTree.GetEnumerator;
  FHighBound := aHighBound;
  FInclusive := aInclusive;
end;

destructor TGComparableTreeSet.THeadEnumerable.Destroy;
begin
  FEnum.Free;
  inherited;
end;

function TGComparableTreeSet.THeadEnumerable.MoveNext: Boolean;
begin
  if FDone or not FEnum.MoveNext then
    exit(False);
  if FInclusive then
    Result := FEnum.Current^.Data.Key <= FHighBound
  else
    Result := FEnum.Current^.Data.Key < FHighBound;
  FDone := not Result;
end;

procedure TGComparableTreeSet.THeadEnumerable.Reset;
begin
  FEnum.Reset;
  FDone := False;
end;

{ TGComparableTreeSet.TRangeEnumerable }

constructor TGComparableTreeSet.TRangeEnumerable.Create(constref aLowBound, aHighBound: T;
  aSet: TCustomTreeSet; aBounds: TRangeBounds);
begin
  inherited Create(aSet);
  FEnum := aSet.FTree.GetEnumeratorAt(aLowBound, rbLow in aBounds);
  FHighBound := aHighBound;
  FInclusive := rbHigh in aBounds;
end;

{ TGComparableTreeSet }

class function TGComparableTreeSet.DoCompare(constref L, R: T): SizeInt;
begin
  if L > R then
    Result := 1
  else
    if R > L then
      Result := -1
    else
      Result := 0;
end;

class function TGComparableTreeSet.Comparator: TComparator;
begin
  Result := @DoCompare;
end;

constructor TGComparableTreeSet.Create;
begin
  FTree := TComparableTree.Create;
end;

constructor TGComparableTreeSet.Create(aCapacity: SizeInt);
begin
  FTree := TComparableTree.Create(aCapacity);
end;

constructor TGComparableTreeSet.Create(constref a: array of T);
begin
  FTree := TComparableTree.Create;
  DoAddAll(a);
end;

constructor TGComparableTreeSet.Create(e: IEnumerable);
var
  o: TObject;
begin
  o := e._GetRef;
  if o is TGComparableTreeSet then
    CreateCopy(TGComparableTreeSet(o))
  else
    begin
      if o is TCustomSet then
        Create(TCustomSet(o).Count)
      else
        Create;
      DoAddAll(e);
    end;
end;

constructor TGComparableTreeSet.CreateCopy(aSet: TGComparableTreeSet);
begin
  FTree := TComparableTree(aSet.FTree).Clone;
end;

function TGComparableTreeSet.Clone: TGComparableTreeSet;
begin
  Result := TGComparableTreeSet.CreateCopy(Self);
end;

function TGComparableTreeSet.Head(constref aHighBound: T; aInclusive: Boolean): IEnumerable;
begin
  BeginIteration;
  Result := THeadEnumerable.Create(aHighBound, Self, aInclusive);
end;

function TGComparableTreeSet.Range(constref aLowBound, aHighBound: T; aIncludeBounds: TRangeBounds): IEnumerable;
begin
  BeginIteration;
  Result := TRangeEnumerable.Create(aLowBound, aHighBound, Self, aIncludeBounds);
end;

function TGComparableTreeSet.HeadSet(constref aHighBound: T; aInclusive: Boolean): TGComparableTreeSet;
var
  v: T;
begin
  Result := TGComparableTreeSet.Create;
  for v in Head(aHighBound, aInclusive) do
    Result.Add(v);
end;

function TGComparableTreeSet.TailSet(constref aLowBound: T; aInclusive: Boolean): TGComparableTreeSet;
var
  v: T;
begin
  Result := TGComparableTreeSet.Create;
  for v in Tail(aLowBound, aInclusive) do
    Result.Add(v);
end;

function TGComparableTreeSet.SubSet(constref aLowBound, aHighBound: T;
  aIncludeBounds: TRangeBounds): TGComparableTreeSet;
var
  v: T;
begin
  Result := TGComparableTreeSet.Create;
  for v in Range(aLowBound, aHighBound, aIncludeBounds) do
    Result.Add(v);
end;

{ TGObjectTreeSet }

procedure TGObjectTreeSet.NodeRemoved(p: PEntry);
begin
  p^.Key.Free;
end;

procedure TGObjectTreeSet.DoClear;
var
  p: PNode;
begin
  if OwnsObjects then
    for p in FTree do
      p^.Data.Key.Free;
  inherited;
end;

function TGObjectTreeSet.DoRemove(constref aValue: T): Boolean;
begin
  Result := inherited DoRemove(aValue);
  if Result and OwnsObjects then
    aValue.Free;
end;

function TGObjectTreeSet.DoRemoveIf(aTest: TTest): SizeInt;
begin
  if OwnsObjects then
    Result := FTree.RemoveIf(aTest, @NodeRemoved)
  else
    Result := FTree.RemoveIf(aTest);
end;

function TGObjectTreeSet.DoRemoveIf(aTest: TOnTest): SizeInt;
begin
  if OwnsObjects then
    Result := FTree.RemoveIf(aTest, @NodeRemoved)
  else
    Result := FTree.RemoveIf(aTest);
end;

function TGObjectTreeSet.DoRemoveIf(aTest: TNestTest): SizeInt;
begin
  if OwnsObjects then
    Result := FTree.RemoveIf(aTest, @NodeRemoved)
  else
    Result := FTree.RemoveIf(aTest);
end;

constructor TGObjectTreeSet.Create(aOwnsObjects: Boolean);
begin
  inherited Create;
  FOwnsObjects := aOwnsObjects;
end;

constructor TGObjectTreeSet.Create(aCapacity: SizeInt; aOwnsObjects: Boolean);
begin
  inherited Create(aCapacity);
  FOwnsObjects := aOwnsObjects;
end;

constructor TGObjectTreeSet.Create(constref a: array of T; aOwnsObjects: Boolean);
begin
  inherited Create(a);
  FOwnsObjects := aOwnsObjects;
end;

constructor TGObjectTreeSet.Create(e: IEnumerable; aOwnsObjects: Boolean);
begin
  inherited Create(e);
  FOwnsObjects := aOwnsObjects;
end;

constructor TGObjectTreeSet.CreateCopy(aSet: TGObjectTreeSet);
begin
  FTree := TBaseTree(aSet.FTree).Clone;
  OwnsObjects := aSet.OwnsObjects;
end;

function TGObjectTreeSet.Clone: TGObjectTreeSet;
begin
  Result := TGObjectTreeSet.CreateCopy(Self);
end;

{ TGRegularTreeSet.THeadEnumerable }

function TGRegularTreeSet.THeadEnumerable.GetCurrent: T;
begin
  Result := FEnum.Current^.Data.Key;
end;

constructor TGRegularTreeSet.THeadEnumerable.Create(constref aHighBound: T; aSet: TCustomTreeSet;
  aInclusive: Boolean);
begin
  inherited Create(aSet);
  FEnum := aSet.FTree.GetEnumerator;
  FCompare := TRegularTree(aSet.FTree).Comparator;
  FHighBound := aHighBound;
  FInclusive := aInclusive;
end;

destructor TGRegularTreeSet.THeadEnumerable.Destroy;
begin
  FEnum.Free;
  inherited;
end;

function TGRegularTreeSet.THeadEnumerable.MoveNext: Boolean;
begin
  if FDone or not FEnum.MoveNext then
    exit(False);
  if FInclusive then
    Result := FCompare(FEnum.Current^.Data.Key, FHighBound) <= 0
  else
    Result := FCompare(FEnum.Current^.Data.Key, FHighBound) < 0;
  FDone := not Result;
end;

procedure TGRegularTreeSet.THeadEnumerable.Reset;
begin
  FEnum.Reset;
  FDone := False;
end;

{ TGRegularTreeSet.TRangeEnumerable }

constructor TGRegularTreeSet.TRangeEnumerable.Create(constref aLowBound, aHighBound: T; aSet: TCustomTreeSet;
  aBounds: TRangeBounds);
begin
  inherited Create(aSet);
  FEnum := aSet.FTree.GetEnumeratorAt(aLowBound, rbLow in aBounds);
  FCompare := TRegularTree(aSet.FTree).Comparator;
  FHighBound := aHighBound;
  FInclusive := rbHigh in aBounds;
end;

{ TGRegularTreeSet }

constructor TGRegularTreeSet.Create;
begin
  FTree := TRegularTree.Create(TDefaults.Compare);
end;

constructor TGRegularTreeSet.Create(c: TComparator);
begin
  FTree := TRegularTree.Create(c);
end;

constructor TGRegularTreeSet.Create(aCapacity: SizeInt; c: TComparator);
begin
  FTree := TRegularTree.Create(aCapacity, c);
end;

constructor TGRegularTreeSet.Create(constref a: array of T; c: TComparator);
begin
  FTree := TRegularTree.Create(c);
  DoAddAll(a);
end;

constructor TGRegularTreeSet.Create(e: IEnumerable; c: TComparator);
begin
  FTree := TRegularTree.Create(c);
  DoAddAll(e);
end;

constructor TGRegularTreeSet.CreateCopy(aSet: TGRegularTreeSet);
begin
  FTree := TRegularTree(aSet.FTree).Clone;
end;

function TGRegularTreeSet.Comparator: TComparator;
begin
  Result := TRegularTree(FTree).Comparator;
end;

function TGRegularTreeSet.Clone: TGRegularTreeSet;
begin
  Result := TGRegularTreeSet.Create(Self, Comparator);
end;

function TGRegularTreeSet.Head(constref aHighBound: T; aInclusive: Boolean): IEnumerable;
begin
  BeginIteration;
  Result := THeadEnumerable.Create(aHighBound, Self, aInclusive);
end;

function TGRegularTreeSet.Range(constref aLowBound, aHighBound: T; aIncludeBounds: TRangeBounds): IEnumerable;
begin
  BeginIteration;
  Result := TRangeEnumerable.Create(aLowBound, aHighBound, Self, aIncludeBounds);
end;

function TGRegularTreeSet.HeadSet(constref aHighBound: T; aInclusive: Boolean): TGRegularTreeSet;
var
  v: T;
begin
  Result := TGRegularTreeSet.Create(Comparator);
  for v in Head(aHighBound, aInclusive) do
    Result.Add(v);
end;

function TGRegularTreeSet.TailSet(constref aLowBound: T; aInclusive: Boolean): TGRegularTreeSet;
var
  v: T;
begin
  Result := TGRegularTreeSet.Create(Comparator);
  for v in Tail(aLowBound, aInclusive) do
    Result.Add(v);
end;

function TGRegularTreeSet.SubSet(constref aLowBound, aHighBound: T;
  aIncludeBounds: TRangeBounds): TGRegularTreeSet;
var
  v: T;
begin
  Result := TGRegularTreeSet.Create(Comparator);
  for v in Range(aLowBound, aHighBound, aIncludeBounds) do
    Result.Add(v);
end;

{ TGObjectRegularTreeSet }

procedure TGObjectRegularTreeSet.NodeRemoved(p: PEntry);
begin
  p^.Key.Free;
end;

procedure TGObjectRegularTreeSet.DoClear;
var
  p: PNode;
begin
  if OwnsObjects then
    for p in FTree do
      p^.Data.Key.Free;
  inherited;
end;

function TGObjectRegularTreeSet.DoRemove(constref aValue: T): Boolean;
begin
  Result := inherited DoRemove(aValue);
  if Result and OwnsObjects then
    aValue.Free;
end;

function TGObjectRegularTreeSet.DoRemoveIf(aTest: TTest): SizeInt;
begin
  if OwnsObjects then
    Result := FTree.RemoveIf(aTest, @NodeRemoved)
  else
    Result := FTree.RemoveIf(aTest);
end;

function TGObjectRegularTreeSet.DoRemoveIf(aTest: TOnTest): SizeInt;
begin
  if OwnsObjects then
    Result := FTree.RemoveIf(aTest, @NodeRemoved)
  else
    Result := FTree.RemoveIf(aTest);
end;

function TGObjectRegularTreeSet.DoRemoveIf(aTest: TNestTest): SizeInt;
begin
  if OwnsObjects then
    Result := FTree.RemoveIf(aTest, @NodeRemoved)
  else
    Result := FTree.RemoveIf(aTest);
end;

constructor TGObjectRegularTreeSet.Create(aOwnsObjects: Boolean);
begin
  inherited Create;
  FOwnsObjects := aOwnsObjects;
end;

constructor TGObjectRegularTreeSet.Create(c: TComparator; aOwnsObjects: Boolean);
begin
  inherited Create(c);
  FOwnsObjects := aOwnsObjects;
end;

constructor TGObjectRegularTreeSet.Create(aCapacity: SizeInt; c: TComparator; aOwnsObjects: Boolean);
begin
  inherited Create(aCapacity, c);
  FOwnsObjects := aOwnsObjects;
end;

constructor TGObjectRegularTreeSet.Create(constref a: array of T; c: TComparator; aOwnsObjects: Boolean);
begin
  inherited Create(a, c);
  FOwnsObjects := aOwnsObjects;
end;

constructor TGObjectRegularTreeSet.Create(e: IEnumerable; c: TComparator; aOwnsObjects: Boolean);
begin
  inherited Create(e, c);
  FOwnsObjects := aOwnsObjects;
end;

constructor TGObjectRegularTreeSet.CreateCopy(aSet: TGObjectRegularTreeSet);
begin
  FTree := TRegularTree(aSet.FTree).Clone;
  FOwnsObjects := aSet.OwnsObjects;
end;

function TGObjectRegularTreeSet.Clone: TGObjectRegularTreeSet;
begin
  Result := TGObjectRegularTreeSet.CreateCopy(Self);
end;

{ TGDelegatedTreeSet.THeadEnumerable }

function TGDelegatedTreeSet.THeadEnumerable.GetCurrent: T;
begin
  Result := FEnum.Current^.Data.Key;
end;

constructor TGDelegatedTreeSet.THeadEnumerable.Create(constref aHighBound: T; aSet: TCustomTreeSet;
  aInclusive: Boolean);
begin
  inherited Create(aSet);
  FEnum := aSet.FTree.GetEnumerator;
  FCompare := TDelegatedTree(aSet.FTree).Comparator;
  FHighBound := aHighBound;
  FInclusive := aInclusive;
end;

destructor TGDelegatedTreeSet.THeadEnumerable.Destroy;
begin
  FEnum.Free;
  inherited;
end;

function TGDelegatedTreeSet.THeadEnumerable.MoveNext: Boolean;
begin
  if FDone or not FEnum.MoveNext then
    exit(False);
  if FInclusive then
    Result := FCompare(FEnum.Current^.Data.Key, FHighBound) <= 0
  else
    Result := FCompare(FEnum.Current^.Data.Key, FHighBound) < 0;
  FDone := not Result;
end;

procedure TGDelegatedTreeSet.THeadEnumerable.Reset;
begin
  FEnum.Reset;
  FDone := False;
end;

{ TGDelegatedTreeSet.TRangeEnumerable }

constructor TGDelegatedTreeSet.TRangeEnumerable.Create(constref aLowBound, aHighBound: T; aSet: TCustomTreeSet;
  aBounds: TRangeBounds);
begin
  inherited Create(aSet);
  FEnum := aSet.FTree.GetEnumeratorAt(aLowBound, rbLow in aBounds);
  FCompare := TDelegatedTree(aSet.FTree).Comparator;
  FHighBound := aHighBound;
  FInclusive := rbHigh in aBounds;
end;

{ TGDelegatedTreeSet }

constructor TGDelegatedTreeSet.Create;
begin
  FTree := TDelegatedTree.Create(TDefaults.OnCompare);
end;

constructor TGDelegatedTreeSet.Create(c: TComparator);
begin
  FTree := TDelegatedTree.Create(c);
end;

constructor TGDelegatedTreeSet.Create(aCapacity: SizeInt; c: TComparator);
begin
  FTree := TDelegatedTree.Create(aCapacity, c);
end;

constructor TGDelegatedTreeSet.Create(constref a: array of T; c: TComparator);
begin
  FTree := TDelegatedTree.Create(c);
  DoAddAll(a);
end;

constructor TGDelegatedTreeSet.Create(e: IEnumerable; c: TComparator);
begin
  FTree := TDelegatedTree.Create(c);
  DoAddAll(e);
end;

constructor TGDelegatedTreeSet.CreateCopy(aSet: TGDelegatedTreeSet);
begin
  FTree := TDelegatedTree(aSet.FTree).Clone;
end;

function TGDelegatedTreeSet.Comparator: TComparator;
begin
  Result := TDelegatedTree(FTree).Comparator;
end;

function TGDelegatedTreeSet.Clone: TGDelegatedTreeSet;
begin
  Result := TGDelegatedTreeSet.CreateCopy(Self);
end;

function TGDelegatedTreeSet.Head(constref aHighBound: T; aInclusive: Boolean): IEnumerable;
begin
  BeginIteration;
  Result := THeadEnumerable.Create(aHighBound, Self, aInclusive);
end;

function TGDelegatedTreeSet.Range(constref aLowBound, aHighBound: T; aIncludeBounds: TRangeBounds): IEnumerable;
begin
  BeginIteration;
  Result := TRangeEnumerable.Create(aLowBound, aHighBound, Self, aIncludeBounds);
end;

function TGDelegatedTreeSet.HeadSet(constref aHighBound: T; aInclusive: Boolean): TGDelegatedTreeSet;
var
  v: T;
begin
  Result := TGDelegatedTreeSet.Create(Comparator);
  for v in Head(aHighBound, aInclusive) do
    Result.Add(v);
end;

function TGDelegatedTreeSet.TailSet(constref aLowBound: T; aInclusive: Boolean): TGDelegatedTreeSet;
var
  v: T;
begin
  Result := TGDelegatedTreeSet.Create(Comparator);
  for v in Tail(aLowBound, aInclusive) do
    Result.Add(v);
end;

function TGDelegatedTreeSet.SubSet(constref aLowBound, aHighBound: T;
  aIncludeBounds: TRangeBounds): TGDelegatedTreeSet;
var
  v: T;
begin
  Result := TGDelegatedTreeSet.Create(Comparator);
  for v in Range(aLowBound, aHighBound, aIncludeBounds) do
    Result.Add(v);
end;

{ TGObjectDelegatedTreeSet }

procedure TGObjectDelegatedTreeSet.NodeRemoved(p: PEntry);
begin
  p^.Key.Free;
end;

procedure TGObjectDelegatedTreeSet.DoClear;
var
  p: PNode;
begin
  if OwnsObjects then
    for p in FTree do
      p^.Data.Key.Free;
  inherited;
end;

function TGObjectDelegatedTreeSet.DoRemove(constref aValue: T): Boolean;
begin
  Result := inherited DoRemove(aValue);
  if Result and OwnsObjects then
    aValue.Free;
end;

function TGObjectDelegatedTreeSet.DoRemoveIf(aTest: TTest): SizeInt;
begin
  if OwnsObjects then
    Result := FTree.RemoveIf(aTest, @NodeRemoved)
  else
    Result := FTree.RemoveIf(aTest);
end;

function TGObjectDelegatedTreeSet.DoRemoveIf(aTest: TOnTest): SizeInt;
begin
  if OwnsObjects then
    Result := FTree.RemoveIf(aTest, @NodeRemoved)
  else
    Result := FTree.RemoveIf(aTest);
end;

function TGObjectDelegatedTreeSet.DoRemoveIf(aTest: TNestTest): SizeInt;
begin
  if OwnsObjects then
    Result := FTree.RemoveIf(aTest, @NodeRemoved)
  else
    Result := FTree.RemoveIf(aTest);
end;

constructor TGObjectDelegatedTreeSet.Create(aOwnsObjects: Boolean);
begin
  inherited Create;
  OwnsObjects := aOwnsObjects;
end;

constructor TGObjectDelegatedTreeSet.Create(aCompare: TOnCompare; aOwnsObjects: Boolean);
begin
  inherited Create(aCompare);
  FOwnsObjects := aOwnsObjects;
end;

constructor TGObjectDelegatedTreeSet.Create(aCapacity: SizeInt; aCompare: TOnCompare; aOwnsObjects: Boolean);
begin
  inherited Create(aCapacity, aCompare);
  FOwnsObjects := aOwnsObjects;
end;

constructor TGObjectDelegatedTreeSet.Create(constref a: array of T;
  aCompare: TOnCompare; aOwnsObjects: Boolean);
begin
  inherited Create(a, aCompare);
  FOwnsObjects := aOwnsObjects;
end;

constructor TGObjectDelegatedTreeSet.Create(e: IEnumerable;
  aCompare: TOnCompare; aOwnsObjects: Boolean);
begin
  inherited Create(e, aCompare);
  FOwnsObjects := aOwnsObjects;
end;

constructor TGObjectDelegatedTreeSet.CreateCopy(aSet: TGObjectDelegatedTreeSet);
begin
  FTree := TDelegatedTree(aSet.FTree).Clone;
  FOwnsObjects := aSet.OwnsObjects;
end;

function TGObjectDelegatedTreeSet.Clone: TGObjectDelegatedTreeSet;
begin
  Result := TGObjectDelegatedTreeSet.CreateCopy(Self);
end;

{ TGLiteTreeSet.TEnumerator }

function TGLiteTreeSet.TEnumerator.GetCurrent: T;
begin
  Result := FEnum.Current^.Key;
end;

procedure TGLiteTreeSet.TEnumerator.Init(constref aSet: TGLiteTreeSet);
begin
  FEnum := aSet.FTree.GetEnumerator;
end;

function TGLiteTreeSet.TEnumerator.MoveNext: Boolean;
begin
  Result := FEnum.MoveNext;
end;

procedure TGLiteTreeSet.TEnumerator.Reset;
begin
  FEnum.Reset;
end;

{ TGLiteTreeSet }

function TGLiteTreeSet.GetCapacity: SizeInt;
begin
  Result := FTree.Capacity;
end;

function TGLiteTreeSet.GetCount: SizeInt;
begin
  Result := FTree.Count;
end;

function TGLiteTreeSet.GetEnumerator: TEnumerator;
begin
  Result.Init(Self);
end;

function TGLiteTreeSet.ToArray: TArray;
var
  I: SizeInt = 0;
  p: PEntry;
begin
  System.SetLength(Result, Count);
  for p in FTree do
    begin
      Result[I] := p^.Key;
      Inc(I);
    end;
end;

function TGLiteTreeSet.IsEmpty: Boolean;
begin
  Result := FTree.Count = 0;
end;

function TGLiteTreeSet.NonEmpty: Boolean;
begin
  Result := FTree.Count <> 0;
end;

procedure TGLiteTreeSet.Clear;
begin
  FTree.Clear;
end;

procedure TGLiteTreeSet.TrimToFit;
begin
  FTree.TrimToFit;
end;

procedure TGLiteTreeSet.EnsureCapacity(aValue: SizeInt);
begin
  FTree.EnsureCapacity(aValue);
end;

function TGLiteTreeSet.Add(constref aValue: T): Boolean;
var
  p: PEntry;
begin
  Result := not FTree.FindOrAdd(aValue, p);
end;

function TGLiteTreeSet.AddAll(constref a: array of T): SizeInt;
var
  v: T;
begin
  Result := 0;
  for v in a do
    Result += Ord(Add(v));
end;

function TGLiteTreeSet.AddAll(e: IEnumerable): SizeInt;
var
  v: T;
begin
  Result := 0;
  for v in e do
    Result += Ord(Add(v));
end;

function TGLiteTreeSet.Contains(constref aValue: T): Boolean;
begin
  Result := FTree.Find(aValue) <> nil;
end;

function TGLiteTreeSet.NonContains(constref aValue: T): Boolean;
begin
  Result := FTree.Find(aValue) = nil;
end;

function TGLiteTreeSet.ContainsAny(constref a: array of T): Boolean;
var
  v: T;
begin
  for v in a do
    if Contains(v) then
      exit(True);
  Result := False;
end;

function TGLiteTreeSet.ContainsAny(e: IEnumerable): Boolean;
var
  v: T;
begin
  for v in e do
    if Contains(v) then
      exit(True);
  Result := False;
end;

function TGLiteTreeSet.ContainsAll(constref a: array of T): Boolean;
var
  v: T;
begin
  for v in a do
    if NonContains(v) then
      exit(False);
  Result := True;
end;

function TGLiteTreeSet.ContainsAll(e: IEnumerable): Boolean;
var
  v: T;
begin
  for v in e do
    if NonContains(v) then
      exit(False);
  Result := True;
end;

function TGLiteTreeSet.Remove(constref aValue: T): Boolean;
begin
  Result := FTree.Remove(aValue);
end;

function TGLiteTreeSet.RemoveAll(constref a: array of T): SizeInt;
var
  v: T;
begin
  Result := 0;
  for v in a do
    Result += Ord(Remove(v));
end;

function TGLiteTreeSet.RemoveAll(e: IEnumerable): SizeInt;
var
  v: T;
begin
  Result := 0;
  for v in e do
    Result += Ord(Remove(v));
end;

function TGLiteTreeSet.RemoveIf(aTest: TTest): SizeInt;
var
  List: TTree.TNodeList;
  I: SizeInt = 0;
begin
  Result := Count;
  if NonEmpty then
    begin
      List := FTree.NodeList;
      while I < FTree.Count do
        if aTest(List[I].Data.Key) then
          FTree.RemoveAt(I)
        else
          Inc(I);
    end;
  Result := Result - Count;
end;

function TGLiteTreeSet.RemoveIf(aTest: TOnTest): SizeInt;
var
  List: TTree.TNodeList;
  I: SizeInt = 0;
begin
  Result := Count;
  if NonEmpty then
    begin
      List := FTree.NodeList;
      while I < FTree.Count do
        if aTest(List[I].Data.Key) then
          FTree.RemoveAt(I)
        else
          Inc(I);
    end;
  Result := Result - Count;
end;

function TGLiteTreeSet.RemoveIf(aTest: TNestTest): SizeInt;
var
  List: TTree.TNodeList;
  I: SizeInt = 0;
begin
  Result := Count;
  if NonEmpty then
    begin
      List := FTree.NodeList;
      while I < FTree.Count do
        if aTest(List[I].Data.Key) then
          FTree.RemoveAt(I)
        else
          Inc(I);
    end;
  Result := Result - Count;
end;

function TGLiteTreeSet.Extract(constref aValue: T): Boolean;
begin
  Result := FTree.Remove(aValue);
end;

function TGLiteTreeSet.ExtractIf(aTest: TTest): TArray;
var
  List: TTree.TNodeList;
  I, J: SizeInt;
begin
  if NonEmpty then
    begin
      System.SetLength(Result, ARRAY_INITIAL_SIZE);
      List := FTree.NodeList;
      I := 0;
      J := 0;
      while I < FTree.Count do
        if aTest(List[I].Data.Key) then
          begin
            if J = System.Length(Result) then
              System.SetLength(Result, J shl 1);
            Result[J] := List[I].Data.Key;
            FTree.RemoveAt(I);
            Inc(J);
          end
        else
          Inc(I);
      System.SetLength(Result, J);
    end
  else
    Result := nil;
end;

function TGLiteTreeSet.ExtractIf(aTest: TOnTest): TArray;
var
  List: TTree.TNodeList;
  I, J: SizeInt;
begin
  if NonEmpty then
    begin
      System.SetLength(Result, ARRAY_INITIAL_SIZE);
      List := FTree.NodeList;
      I := 0;
      J := 0;
      while I < FTree.Count do
        if aTest(List[I].Data.Key) then
          begin
            if J = System.Length(Result) then
              System.SetLength(Result, J shl 1);
            Result[J] := List[I].Data.Key;
            FTree.RemoveAt(I);
            Inc(J);
          end
        else
          Inc(I);
      System.SetLength(Result, J);
    end
  else
    Result := nil;
end;

function TGLiteTreeSet.ExtractIf(aTest: TNestTest): TArray;
var
  List: TTree.TNodeList;
  I, J: SizeInt;
begin
  if NonEmpty then
    begin
      System.SetLength(Result, ARRAY_INITIAL_SIZE);
      List := FTree.NodeList;
      I := 0;
      J := 0;
      while I < FTree.Count do
        if aTest(List[I].Data.Key) then
          begin
            if J = System.Length(Result) then
              System.SetLength(Result, J shl 1);
            Result[J] := List[I].Data.Key;
            FTree.RemoveAt(I);
            Inc(J);
          end
        else
          Inc(I);
      System.SetLength(Result, J);
    end
  else
    Result := nil;
end;

procedure TGLiteTreeSet.RetainAll(aCollection: ICollection);
  function CollNonCoontains(constref aValue: T): Boolean;
  begin
    Result := aCollection.NonContains(aValue);
  end;
begin
  RemoveIf(@CollNonCoontains);
end;

function TGLiteTreeSet.IsSuperset(constref aSet: TGLiteTreeSet): Boolean;
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

function TGLiteTreeSet.IsSubset(constref aSet: TGLiteTreeSet): Boolean;
begin
  Result := aSet.IsSuperset(Self);
end;

function TGLiteTreeSet.IsEqual(constref aSet: TGLiteTreeSet): Boolean;
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

function TGLiteTreeSet.Intersecting(constref aSet: TGLiteTreeSet): Boolean;
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

procedure TGLiteTreeSet.Intersect(constref aSet: TGLiteTreeSet);
begin

end;

procedure TGLiteTreeSet.Join(constref aSet: TGLiteTreeSet);
var
  v: T;
begin
  for v in aSet do
    Add(v);
end;

procedure TGLiteTreeSet.Subtract(constref aSet: TGLiteTreeSet);
var
  v: T;
begin
  for v in aSet do
    Remove(v);
end;

procedure TGLiteTreeSet.SymmetricSubtract(constref aSet: TGLiteTreeSet);
var
  v: T;
begin
  for v in aSet do
    if not Remove(v) then
      Add(v);
end;

end.

