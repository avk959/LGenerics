{****************************************************************************
*                                                                           *
*   This file is part of the LGenerics package.                             *
*   Generic sorted list implementation.                                     *
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
unit LGSortedList;

{$mode objfpc}{$H+}
{$INLINE ON}
{$MODESWITCH ADVANCEDRECORDS}
{$MODESWITCH NESTEDPROCVARS}

interface

uses

  SysUtils,
  math,
  LGUtils,
  {%H-}LGHelpers,
  LGCustomContainer,
  LGArrayHelpers,
  LGStrConst;

type

  { TGBaseSortedList is always sorted ascending }
  generic TGBaseSortedList<T, TCmpRel> = class(specialize TGCustomCollection<T>)
  protected
  const
    // MAX_CAPACITY must be <= Succ(SizeInt.MaxValue div 2)
    MAX_CAPACITY: SizeInt = SizeInt(MAX_CONTAINER_SIZE div SizeOf(TItem));

  type
    TSortedList = specialize TGBaseSortedList<T, TCmpRel>;
    THelper     = class(specialize TGBaseArrayHelper<T, TCmpRel>);

    TEnumerator = class(TContainerEnumerator)
    private
      FItems: TArray;
      FCurrIndex,
      FLast: SizeInt;
    protected
      function  GetCurrent: T; override;
    public
      constructor Create(aList: TSortedList);
      function  MoveNext: Boolean; override;
      procedure Reset; override;
    end;

    TReverseEnumerable = class(TContainerEnumerable)
    protected
      FItems: TArray;
      FCurrIndex,
      FCount: SizeInt;
      function  GetCurrent: T; override;
    public
      constructor Create(aList: TSortedList);
      function  MoveNext: Boolean; override;
      procedure Reset; override;
    end;

    THeadEnumerable = class(TContainerEnumerable)
    protected
      FItems: TArray;
      FCurrIndex,
      FLast: SizeInt;
      function  GetCurrent: T; override;
    public
      constructor Create(aList: TSortedList; aLastIndex: SizeInt); overload;
      function  MoveNext: Boolean; override;
      procedure Reset; override;
    end;

    TTailEnumerable = class(TContainerEnumerable)
    protected
      FItems: TArray;
      FCurrIndex,
      FStart,
      FLast: SizeInt;
      function  GetCurrent: T; override;
    public
      constructor Create(aList: TSortedList; aStartIndex: SizeInt); overload;
      function  MoveNext: Boolean; override;
      procedure Reset; override;
    end;

  TRangeEnumerable = class(TTailEnumerable)
    constructor Create(aList: TSortedList; aStartIndex, aLastIndex: SizeInt); overload;
  end;

  public
  type
    TRecEnumerator = record
    private
      FItems: TArray;
      FCurrIndex,
      FLast: SizeInt;
      function  GetCurrent: T; inline;
    public
      procedure Init(aList: TSortedList);
      function  MoveNext: Boolean; inline;
      procedure Reset; inline;
      property  Current: T read GetCurrent;
    end;

  protected
  type
    TExtractHelper = object
    private
      FCurrIndex: SizeInt;
      FExtracted: TArray;
    public
      procedure Add(constref aValue: T);
      procedure Init;
      function  Final: TArray;
    end;

  var
    FItems: TArray;
    FCount: SizeInt;
    FRejectDuplicates: Boolean;
    function  GetCount: SizeInt; override;
    function  GetCapacity: SizeInt; override;
    procedure SetRejectDuplicates(aValue: Boolean);
    procedure DoClear; override;
    function  DoGetEnumerator: TCustomEnumerator; override;
    procedure DoTrimToFit; override;
    procedure DoEnsureCapacity(aValue: SizeInt); override;
    procedure CopyItems(aBuffer: PItem); override;
    function  GetItem(aIndex: SizeInt): T; inline;
    procedure SetItem(aIndex: SizeInt; const aValue: T); inline;
    procedure DoSetItem(aIndex: SizeInt; const aValue: T); virtual;
    procedure RemoveDuplicates;
    procedure InsertItem(aIndex: SizeInt; constref aValue: T);
    function  DoAdd(constref aValue: T): Boolean; override;
    function  DoInsert(constref aValue: T): SizeInt;
    function  DoRemove(constref aValue: T): Boolean; override;
    function  DoExtract(constref aValue: T): Boolean; override;
    function  DoRemoveIf(aTest: TTest): SizeInt; override;
    function  DoRemoveIf(aTest: TOnTest): SizeInt; override;
    function  DoRemoveIf(aTest: TNestTest): SizeInt; override;
    function  DoExtractIf(aTest: TTest): TArray; override;
    function  DoExtractIf(aTest: TOnTest): TArray; override;
    function  DoExtractIf(aTest: TNestTest): TArray; override;
    function  SelectDistinctArray(constref a: array of T): TArray;
    function  DoAddAll(constref a: array of T): SizeInt; override; overload;
    function  DoAddAll(e: IEnumerable): SizeInt; override; overload;
    function  IndexInRange(aIndex: SizeInt): Boolean; inline;
    procedure CheckIndexRange(aIndex: SizeInt); inline;
    function  ListCapacity: SizeInt; inline;
    function  GetReverse: IEnumerable;
    procedure Expand(aValue: SizeInt);
    procedure ItemAdding; inline;
    function  ExtractItem(aIndex: SizeInt): T;
    function  DeleteItem(aIndex: SizeInt): T; virtual;
    function  DoDeleteRange(aIndex, aCount: SizeInt): SizeInt; virtual;
    function  GetRecEnumerator: TRecEnumerator; inline; //for internal use
    function  NearestLT(constref aValue: T): SizeInt;
    function  RightmostLE(constref aValue: T): SizeInt;
    function  NearestGT(constref aValue: T): SizeInt;
    function  LeftmostGE(constref aValue: T): SizeInt;
    property  ElemCount: SizeInt read FCount;
  public
    constructor CreateEmpty;
    constructor Create;
    constructor Create(aCapacity: SizeInt);
    constructor Create(constref a: array of T);
    constructor Create(e: IEnumerable);
    constructor Create(aRejectDuplicates: Boolean);
    constructor Create(constref a: array of T; aRejectDuplicates: Boolean);
    constructor Create(e: IEnumerable; aRejectDuplicates: Boolean);
    destructor  Destroy; override;

    function  Reverse: IEnumerable; override;
    function  ToArray: TArray; override;
    function  FindMin(out aValue: T): Boolean;
    function  FindMax(out aValue: T): Boolean;
  { returns insert index, -1 if element is not inserted }
    function  Insert(constref aValue: T): SizeInt;
    function  Contains(constref aValue: T): Boolean; override;
    procedure Delete(aIndex: SizeInt);
    function  TryDelete(aIndex: SizeInt): Boolean;
    function  DeleteAll(aIndex, aCount: SizeInt): SizeInt;
    function  IndexOf(constref aValue: T): SizeInt; inline;
  { returns index of leftest occurrence of aValue, -1 if there are no such element }
    function  FirstIndexOf(constref aValue: T): SizeInt;
  { returns count of occurrences of aValue, 0 if there are no such element }
    function  CountOf(constref aValue: T): SizeInt;
  { returns index of element whose value greater then or equal to aValue (depend on aInclusive);
    returns -1 if there are no such element }
    function  IndexOfCeil(constref aValue: T; aInclusive: Boolean): SizeInt;
  { returns index of element whose value less then aBound (or equal to aValue depend on aInclusive);
    returns -1 if there are no such element }
    function  IndexOfFloor(constref aValue: T; aInclusive: Boolean): SizeInt;
  { enumerates values whose are strictly less than(if not aInclusive) aHighBound }
    function  Head(constref aHighBound: T; aInclusive: Boolean = False): IEnumerable;
  { enumerates values whose are greater than or equal to(if aInclusive) aLowBound }
    function  Tail(constref aLowBound: T; aInclusive: Boolean = True): IEnumerable;
  { enumerates values whose are greater than or equal to aLowBound and strictly less than aHighBound(by default)}
    function  Range(constref aLowBound, aHighBound: T; aIncludeBounds: TRangeBounds = [rbLow]): IEnumerable;
    function  HeadList(constref aHighBound: T; aInclusive: Boolean = False): TSortedList;
    function  TailList(constref aLowBound: T; aInclusive: Boolean = True): TSortedList;
    function  SubList(constref aLowBound, aHighBound: T; aIncludeBounds: TRangeBounds = [rbLow]): TSortedList;
    function  Clone: TSortedList; override;
    property  RejectDuplicates: Boolean read FRejectDuplicates write SetRejectDuplicates;
    property  Items[aIndex: SizeInt]: T read GetItem write SetItem; default;
  end;

  { TGSortedList assumes that type T implements TCmpRel}
  generic TGSortedList<T> = class(specialize TGBaseSortedList<T, T>);

  { TGObjectSortedList }

  generic TGObjectSortedList<T, TCmpRel> = class(specialize TGBaseSortedList<T, TCmpRel>)
  private
    FOwnsObects: Boolean;
  protected
    procedure DoClear; override;
    procedure DoSetItem(aIndex: SizeInt; const aValue: T); override;
    function  DoRemove(constref aValue: T): Boolean; override;
    function  DoRemoveIf(aTest: TTest): SizeInt; override;
    function  DoRemoveIf(aTest: TOnTest): SizeInt; override;
    function  DoRemoveIf(aTest: TNestTest): SizeInt; override;
    function  DeleteItem(aIndex: SizeInt): T; override;
    function  DoDeleteRange(aIndex, aCount: SizeInt): SizeInt; override;
  public
  type
    TObjectSortedList = specialize TGObjectSortedList<T, TCmpRel>;

    constructor CreateEmpty(aOwnsObjects: Boolean = True);
    constructor Create(aOwnsObjects: Boolean = True);
    constructor Create(aCapacity: SizeInt; aOwnsObjects: Boolean = True);
    constructor Create(constref a: array of T; aOwnsObjects: Boolean = True);
    constructor Create(e: IEnumerable; aOwnsObjects: Boolean = True);
    constructor Create(aRejectDuplicates: Boolean; aOwnsObjects: Boolean = True);
    constructor Create(constref a: array of T; aRejectDuplicates: Boolean; aOwnsObjects: Boolean = True);
    constructor Create(e: IEnumerable; aRejectDuplicates: Boolean; aOwnsObjects: Boolean = True);
    function  Clone: TObjectSortedList; override;
    property OwnsObjects: Boolean read FOwnsObects write FOwnsObects;
  end;

  { TGObjSortedList assumes that TObject type T implements TCmpRel}
  generic TGObjSortedList<T> = class(specialize TGObjectSortedList<T, T>);

  { TGSortedList2 is always sorted ascending }
  generic TGSortedList2<T, TCmpRel> = class
  private
  type
    TArray = array of T;

  public
  type
    TEnumerator = record
    private
      FList: TArray;
      FCurrIndex,
      FLastIndex: SizeInt;
      function  GetCurrent: T; inline;
    public
      procedure Init(aList: TGSortedList2); inline;
      function  MoveNext: Boolean;
      procedure Reset; inline;
      property  Current: T read GetCurrent;
    end;

  private
  const
    ITEM_SIZE = SizeOf(T);
    MAX_CAPACITY = MAX_CONTAINER_SIZE div ITEM_SIZE;

  type
    THelper = specialize TGBaseArrayHelper<T, TCmpRel>;

  var
    FItems: TArray;
    FCount: SizeInt;
    FAllowDuplicates: Boolean;
    function  GetCapacity: SizeInt; inline;
    procedure Expand(aValue: SizeInt);
    procedure ItemAdding; inline;
    procedure InsertItem(aIndex: SizeInt; constref aValue: T);
    procedure RemoveItem(aIndex: SizeInt);
    procedure CapacityExceedError(aValue: SizeInt); inline;
  public
    constructor CreateEmpty;
    constructor CreateEmpty(aAllowDuplicates: Boolean);
    constructor Create;
    constructor Create(aCapacity: SizeInt);
    constructor Create(aCapacity: SizeInt; AllowDuplicates: Boolean);
    destructor  Destroy; override;
    function  GetEnumerator: TEnumerator; inline;
    procedure Clear; inline;
    function  EnsureCapacity(aValue: SizeInt): Boolean; inline;
    procedure TrimToFit; inline;
    function  Add(constref aValue: T): Boolean;
    function  Contains(constref aValue: T): Boolean;
    function  Remove(constref aValue: T): Boolean;
    property  Count: SizeInt read FCount;
    property  Capacity: SizeInt read GetCapacity;
  { by default False }
    property  AllowDuplicates: Boolean read FAllowDuplicates;
  end;

  { TGSortedListTable is always sorted ascending }
  generic TGSortedListTable<TKey, TEntry, TCmpRel> = class
  private
  type
    TEntryList = array of TEntry;

  public
  type
    PEntry = ^TEntry;

    TEntryCmpRel = class
      class function Compare(constref L, R: TEntry): SizeInt; static; inline;
    end;

    TEnumerator = record
    private
      FList: TEntryList;
      FCurrIndex,
      FLastIndex: SizeInt;
      function  GetCurrent: PEntry; inline;
    public
      procedure Init(aTable: TGSortedListTable); inline;
      function  MoveNext: Boolean;
      procedure Reset; inline;
      property  Current: PEntry read GetCurrent;
    end;

  private
  const
    E_SIZE       = SizeOf(TEntry);
    ENTRY_SIZE   = E_SIZE or Ord(E_SIZE = 0);
    MAX_CAPACITY = MAX_CONTAINER_SIZE div ENTRY_SIZE;

  var
    FItems: TEntryList;
    FCount: SizeInt;
    FAllowDuplicates: Boolean;
    function  GetCapacity: SizeInt; inline;
    procedure Expand(aValue: SizeInt);
    procedure ItemAdding; inline;
    procedure InsertItem(aIndex: SizeInt; constref aValue: TEntry);
    procedure RemoveItem(aIndex: SizeInt);
    procedure CapacityExceedError(aValue: SizeInt); inline;
  public
    constructor CreateEmpty;
    constructor CreateEmpty(AllowDuplicates: Boolean);
    constructor Create;
    constructor Create(aCapacity: SizeInt);
    constructor Create(aCapacity: SizeInt; AllowDuplicates: Boolean);
    destructor  Destroy; override;
    function  GetEnumerator: TEnumerator; inline;
    procedure Clear; inline;
    function  EnsureCapacity(aValue: SizeInt): Boolean; inline;
    procedure TrimToFit; inline;
    function  FindOrAdd(constref aKey: TKey; out e: PEntry; out aPos: SizeInt): Boolean;
    function  Find(constref aKey: TKey; out aPos: SizeInt): PEntry;
    function  Add(constref aKey: TKey): PEntry;
    function  Remove(constref aKey: TKey): Boolean;
    procedure RemoveAt(constref aPos: SizeInt); inline;
    property  Count: SizeInt read FCount;
    property  Capacity: SizeInt read GetCapacity;
  { by default False }
    property  AllowDuplicates: Boolean read FAllowDuplicates;
  end;

implementation
{$B-}{$COPERATORS ON}

{ TGBaseSortedList.TEnumerator }

function TGBaseSortedList.TEnumerator.GetCurrent: T;
begin
  Result := FItems[FCurrIndex];
end;

constructor TGBaseSortedList.TEnumerator.Create(aList: TSortedList);
begin
  inherited Create(aList);
  FItems := aList.FItems;
  FLast := Pred(aList.ElemCount);
  FCurrIndex := -1;
end;

function TGBaseSortedList.TEnumerator.MoveNext: Boolean;
begin
  Result := FCurrIndex < FLast;
  FCurrIndex += Ord(Result);
end;

procedure TGBaseSortedList.TEnumerator.Reset;
begin
  FCurrIndex := -1;
end;

{ TGBaseSortedList.TReverseEnumerable }

function TGBaseSortedList.TReverseEnumerable.GetCurrent: T;
begin
  Result := FItems[FCurrIndex];
end;

constructor TGBaseSortedList.TReverseEnumerable.Create(aList: TSortedList);
begin
  inherited Create(aList);
  FItems := aList.FItems;
  FCount := aList.ElemCount;
  FCurrIndex := FCount;
end;

function TGBaseSortedList.TReverseEnumerable.MoveNext: Boolean;
begin
  Result := FCurrIndex > 0;
  FCurrIndex -= Ord(Result);
end;

procedure TGBaseSortedList.TReverseEnumerable.Reset;
begin
  FCurrIndex := FCount;
end;

{ TGBaseSortedList.THeadEnumerable }

function TGBaseSortedList.THeadEnumerable.GetCurrent: T;
begin
  Result := FItems[FCurrIndex];
end;

constructor TGBaseSortedList.THeadEnumerable.Create(aList: TSortedList; aLastIndex: SizeInt);
begin
  inherited Create(aList);
  FItems := aList.FItems;
  FLast := aLastIndex;
end;

function TGBaseSortedList.THeadEnumerable.MoveNext: Boolean;
begin
  Result := FCurrIndex < FLast;
  FCurrIndex += Ord(Result);
end;

procedure TGBaseSortedList.THeadEnumerable.Reset;
begin
  FCurrIndex := -1;
end;

{ TGBaseSortedList.TTailEnumerable }

function TGBaseSortedList.TTailEnumerable.GetCurrent: T;
begin
  Result := FItems[FCurrIndex];
end;

constructor TGBaseSortedList.TTailEnumerable.Create(aList: TSortedList; aStartIndex: SizeInt);
begin
  inherited Create(aList);
  FItems := aList.FItems;
  FLast := Pred(aList.ElemCount);
  FStart := Pred(aStartIndex);
  FCurrIndex := FStart;
end;

function TGBaseSortedList.TTailEnumerable.MoveNext: Boolean;
begin
  Result := FCurrIndex < FLast;
  FCurrIndex += Ord(Result);
end;

procedure TGBaseSortedList.TTailEnumerable.Reset;
begin
  FCurrIndex := FStart;
end;

{ TGBaseSortedList.TRangeEnumerable }

constructor TGBaseSortedList.TRangeEnumerable.Create(aList: TSortedList; aStartIndex, aLastIndex: SizeInt);
begin
  inherited Create(aList, aStartIndex);
  FLast := aLastIndex;
end;

{ TGBaseSortedList.TRecEnumerator }

function TGBaseSortedList.TRecEnumerator.GetCurrent: T;
begin
  Result := FItems[FCurrIndex];
end;

procedure TGBaseSortedList.TRecEnumerator.Init(aList: TSortedList);
begin
  FItems := aList.FItems;
  FLast := Pred(aList.ElemCount);
  FCurrIndex := -1;
end;

function TGBaseSortedList.TRecEnumerator.MoveNext: Boolean;
begin
  Result := FCurrIndex < FLast;
  FCurrIndex += Ord(Result);
end;

procedure TGBaseSortedList.TRecEnumerator.Reset;
begin
  FCurrIndex := -1;
end;

{ TGBaseSortedList.TExtractHelper }

procedure TGBaseSortedList.TExtractHelper.Add(constref aValue: T);
var
  c: SizeInt;
begin
  c := System.Length(FExtracted);
  if FCurrIndex = c then
    System.SetLength(FExtracted, c shl 1);
  FExtracted[FCurrIndex] := aValue;
  Inc(FCurrIndex);
end;

procedure TGBaseSortedList.TExtractHelper.Init;
begin
  FCurrIndex := 0;
  System.SetLength(FExtracted, ARRAY_INITIAL_SIZE);
end;

function TGBaseSortedList.TExtractHelper.Final: TArray;
begin
  System.SetLength(FExtracted, FCurrIndex);
  Result := FExtracted;
end;

{ TGBaseSortedList }

function TGBaseSortedList.GetCount: SizeInt;
begin
  Result := ElemCount;
end;

function TGBaseSortedList.GetCapacity: SizeInt;
begin
  Result := System.Length(FItems);
end;

procedure TGBaseSortedList.SetRejectDuplicates(aValue: Boolean);
begin
  if RejectDuplicates <> aValue then
    begin
      FRejectDuplicates := aValue;
      if RejectDuplicates then
        RemoveDuplicates;
    end;
end;

procedure TGBaseSortedList.DoClear;
begin
  FItems := nil;
  FCount := 0;
end;

function TGBaseSortedList.DoGetEnumerator: TCustomEnumerator;
begin
  Result := TEnumerator.Create(Self);
end;

procedure TGBaseSortedList.DoTrimToFit;
begin
  if ListCapacity > ElemCount then
    System.SetLength(FItems, ElemCount);
end;

procedure TGBaseSortedList.DoEnsureCapacity(aValue: SizeInt);
begin
  if aValue > ListCapacity then
    Expand(aValue);
end;

procedure TGBaseSortedList.CopyItems(aBuffer: PItem);
begin
  if ElemCount > 0 then
    THelper.CopyItems(@FItems[0], aBuffer, ElemCount);
end;

function TGBaseSortedList.GetItem(aIndex: SizeInt): T;
begin
  CheckIndexRange(aIndex);
  Result := FItems[aIndex];
end;

procedure TGBaseSortedList.SetItem(aIndex: SizeInt; const aValue: T);
begin
  CheckIndexRange(aIndex);
  DoSetItem(aIndex, aValue);
end;

procedure TGBaseSortedList.DoSetItem(aIndex: SizeInt; const aValue: T);
var
  sr: THelper.TSearchResult;
  c: SizeInt;
begin
  c := TCmpRel.Compare(aValue, FItems[aIndex]);
  if c <> 0 then
    begin
      CheckInIteration;
      if ElemCount > 1 then
        begin
          sr := THelper.BinarySearchPos(FItems[0..Pred(ElemCount)], aValue);
          if (sr.FoundIndex > -1) and RejectDuplicates then
            begin
              DeleteItem(sr.FoundIndex);
              exit;
            end;
          FItems[aIndex] := Default(T);  ///////////////
          if sr.InsertIndex > aIndex then
            System.Move(FItems[Succ(aIndex)], FItems[aIndex], sr.InsertIndex - aIndex)
          else
            System.Move(FItems[sr.InsertIndex], FItems[Succ(sr.InsertIndex)], aIndex - sr.InsertIndex);
          System.FillChar(FItems[sr.InsertIndex], SizeOf(T), 0);
          FItems[sr.InsertIndex] := aValue;
        end;
    end;
end;

procedure TGBaseSortedList.RemoveDuplicates;
var
  I, J, Hi: SizeInt;
begin
  Hi := Pred(ElemCount);
  if Hi < 1 then
    exit;
  I := 0;
  for J := 1 to Hi do
    begin
      if TCmpRel.Compare(FItems[I], FItems[J]) = 0 then
        continue;
      Inc(I);
      if J > I then
        FItems[I] := FItems[J];
    end;
  FCount := Succ(I);
  for I := ElemCount to Hi do
    FItems[I] := Default(T);
end;

procedure TGBaseSortedList.InsertItem(aIndex: SizeInt; constref aValue: T);
begin
  ItemAdding;
  if aIndex < ElemCount then
    begin
      System.Move(FItems[aIndex], FItems[Succ(aIndex)], SizeOf(T) * (ElemCount - aIndex));
      System.FillChar(FItems[aIndex], SizeOf(T), 0);
    end;
  FItems[aIndex] := aValue;
  Inc(FCount);
end;

function TGBaseSortedList.DoAdd(constref aValue: T): Boolean;
var
  sr: THelper.TSearchResult;
begin
  if ElemCount > 0 then
    begin
      sr := THelper.BinarySearchPos(FItems[0..Pred(ElemCount)], aValue);
      if (sr.FoundIndex > -1) and RejectDuplicates then
        exit(False);
      InsertItem(sr.InsertIndex, aValue);
    end
  else
    InsertItem(ElemCount, aValue);
  Result := True;
end;

function TGBaseSortedList.DoInsert(constref aValue: T): SizeInt;
var
  sr: THelper.TSearchResult;
begin
  if ElemCount > 0 then
    begin
      sr := THelper.BinarySearchPos(FItems[0..Pred(ElemCount)], aValue);
      if (sr.FoundIndex > -1) and RejectDuplicates then
        exit(-1);
      Result := sr.InsertIndex;
    end
  else
    Result := 0;
  InsertItem(Result, aValue);
end;

function TGBaseSortedList.DoRemove(constref aValue: T): Boolean;
var
  Removed: SizeInt;
begin
  Removed := IndexOf(aValue);
  Result := Removed > -1;
  if Result then
    DeleteItem(Removed);
end;

function TGBaseSortedList.DoExtract(constref aValue: T): Boolean;
var
  Extracted: SizeInt;
begin
  Extracted := IndexOf(aValue);
  Result := Extracted > -1;
  if Result then
    ExtractItem(Extracted);
end;

function TGBaseSortedList.DoRemoveIf(aTest: TTest): SizeInt;
var
  I, J: SizeInt;
begin
  Result := ElemCount;
  if Result > 0 then
    begin
      J := 0;
      for I := 0 to Pred(Result) do
        begin
          if aTest(FItems[I]) then
            continue;
          if I > J then
            FItems[J] := FItems[I];
          Inc(J);
        end;
      FCount := J;
      for I := ElemCount to Pred(Result) do
        FItems[I] := Default(T);
      Result := Result - ElemCount;
    end;
end;

function TGBaseSortedList.DoRemoveIf(aTest: TOnTest): SizeInt;
var
  I, J: SizeInt;
begin
  Result := ElemCount;
  if Result > 0 then
    begin
      J := 0;
      for I := 0 to Pred(Result) do
        begin
          if aTest(FItems[I]) then
            continue;
          if I > J then
            FItems[J] := FItems[I];
          Inc(J);
        end;
      FCount := J;
      for I := ElemCount to Pred(Result) do
        FItems[I] := Default(T);
      Result := Result - ElemCount;
    end;
end;

function TGBaseSortedList.DoRemoveIf(aTest: TNestTest): SizeInt;
var
  I, J: SizeInt;
begin
  Result := ElemCount;
  if Result > 0 then
    begin
      J := 0;
      for I := 0 to Pred(Result) do
        begin
          if aTest(FItems[I]) then
            continue;
          if I > J then
            FItems[J] := FItems[I];
          Inc(J);
        end;
      FCount := J;
      for I := ElemCount to Pred(Result) do
        FItems[I] := Default(T);
      Result := Result - ElemCount;
    end;
end;

function TGBaseSortedList.DoExtractIf(aTest: TTest): TArray;
var
  h: TExtractHelper;
  I, J, OldCount: SizeInt;
begin
  if ElemCount = 0 then
    exit(nil);
  OldCount := ElemCount;
  h.Init;
  J := 0;
  for I := 0 to Pred(OldCount) do
    begin
      if aTest(FItems[I]) then
        begin
          h.Add(FItems[I]);
          continue;
        end;
      if I > J then
        FItems[J] := FItems[I];
      Inc(J);
    end;
  FCount := J;
  for I := ElemCount to Pred(OldCount) do
    FItems[I] := Default(T);
  Result := h.Final;
end;

function TGBaseSortedList.DoExtractIf(aTest: TOnTest): TArray;
var
  h: TExtractHelper;
  I, J, OldCount: SizeInt;
begin
  if ElemCount = 0 then
    exit(nil);
  OldCount := ElemCount;
  h.Init;
  J := 0;
  for I := 0 to Pred(OldCount) do
    begin
      if aTest(FItems[I]) then
        begin
          h.Add(FItems[I]);
          continue;
        end;
      if I > J then
        FItems[J] := FItems[I];
      Inc(J);
    end;
  FCount := J;
  for I := ElemCount to Pred(OldCount) do
    FItems[I] := Default(T);
  Result := h.Final;
end;

function TGBaseSortedList.DoExtractIf(aTest: TNestTest): TArray;
var
  h: TExtractHelper;
  I, J, OldCount: SizeInt;
begin
  if ElemCount = 0 then
    exit(nil);
  OldCount := ElemCount;
  h.Init;
  J := 0;
  for I := 0 to Pred(OldCount) do
    begin
      if aTest(FItems[I]) then
        begin
          h.Add(FItems[I]);
          continue;
        end;
      if I > J then
        FItems[J] := FItems[I];
      Inc(J);
    end;
  FCount := J;
  for I := ElemCount to Pred(OldCount) do
    FItems[I] := Default(T);
  Result := h.Final;
end;

function TGBaseSortedList.SelectDistinctArray(constref a: array of T): TArray;
var
  I, J, Hi: SizeInt;
begin
  Result := THelper.SelectDistinct(a);
  if ElemCount = 0 then
    exit;
  Hi := System.High(Result);
  I := -1;
  for J := 0 to Hi do
    begin
      if IndexOf(Result[J]) > -1 then
        continue;
      Inc(I);
      if J > I then
        Result[I] := Result[J];
    end;
  System.SetLength(Result, Succ(I));
end;

function TGBaseSortedList.DoAddAll(constref a: array of T): SizeInt;
var
  OldCount: SizeInt;
  PSrc: PItem;
  da: TArray;
begin
  OldCount := ElemCount;
  if RejectDuplicates then
    begin
      da := SelectDistinctArray(a);
      Result := System.Length(da);
      if Result = 0 then
        exit;
      PSrc := @da[0];
    end
  else
    begin
      Result := System.Length(a);
      if Result = 0 then
        exit;
      PSrc := @a[0];
    end;
  DoEnsureCapacity(OldCount + Result);
  THelper.CopyItems(PSrc, @FItems[OldCount], Result);
  FCount += Result;
  if RejectDuplicates or (OldCount >= Result) then
    THelper.MergeSort(FItems[0..Pred(ElemCount)])
  else
    THelper.Sort(FItems[0..Pred(ElemCount)])
end;

function TGBaseSortedList.DoAddAll(e: IEnumerable): SizeInt;
begin
  if (e._GetRef = Self) and RejectDuplicates then
    exit(0);
  Result := DoAddAll(e.ToArray);
end;

function TGBaseSortedList.IndexInRange(aIndex: SizeInt): Boolean;
begin
  Result := (aIndex >= 0) and (aIndex < ElemCount);
end;

procedure TGBaseSortedList.CheckIndexRange(aIndex: SizeInt);
begin
  if not IndexInRange(aIndex) then
   IndexOutOfBoundError(aIndex);
end;

function TGBaseSortedList.ListCapacity: SizeInt;
begin
  Result := System.Length(FItems);
end;

function TGBaseSortedList.GetReverse: IEnumerable;
begin
  Result := TReverseEnumerable.Create(Self);
end;

procedure TGBaseSortedList.Expand(aValue: SizeInt);
begin
  //there aValue > Capacity
  if aValue <= DEFAULT_CONTAINER_CAPACITY then
    System.SetLength(FItems, DEFAULT_CONTAINER_CAPACITY)
  else
    if aValue <= MAX_CAPACITY then
      begin
        aValue := Math.Min(MAX_CAPACITY, LGUtils.RoundUpTwoPower(aValue));
        System.SetLength(FItems, aValue);
      end
    else
      CapacityExceedError(aValue);
end;

procedure TGBaseSortedList.ItemAdding;
begin
  if ElemCount = ListCapacity then
    Expand(Succ(ElemCount));
end;

function TGBaseSortedList.ExtractItem(aIndex: SizeInt): T;
begin
  Result := FItems[aIndex];
  FItems[aIndex] := Default(T);
  Dec(FCount);
  System.Move(FItems[Succ(aIndex)], FItems[aIndex], SizeOf(T) * (ElemCount - aIndex));
  System.FillChar(FItems[ElemCount], SizeOf(T), 0);
end;

function TGBaseSortedList.DeleteItem(aIndex: SizeInt): T;
begin
  Result := ExtractItem(aIndex);
end;

function TGBaseSortedList.DoDeleteRange(aIndex, aCount: SizeInt): SizeInt;
var
  I: SizeInt;
begin
  if aCount < 0 then
    aCount := 0;
  Result := Math.Min(aCount, ElemCount - aIndex);
  if Result > 0 then
    begin
      for I := aIndex to Pred(aIndex + Result) do
        FItems[I] := Default(T);
      FCount -= Result;
      System.Move(FItems[aIndex + Result], FItems[aIndex], SizeOf(T) * (ElemCount - aIndex));
      System.FillChar(FItems[ElemCount], SizeOf(T) * Result, 0);
    end;
end;

function TGBaseSortedList.GetRecEnumerator: TRecEnumerator;
begin
  Result.Init(Self);
end;

function TGBaseSortedList.NearestLT(constref aValue: T): SizeInt;
begin
  if (ElemCount = 0) or (TCmpRel.Compare(aValue, FItems[0]) <= 0) then
    exit(-1);
  if TCmpRel.Compare(aValue, FItems[Pred(ElemCount)]) > 0 then
     exit(Pred(ElemCount));
  //here such element exist in FItems and not first nor last
  Result := THelper.BinarySearchPos(FItems[0..Pred(ElemCount)], aValue).InsertIndex;
  if TCmpRel.Compare(FItems[Result], aValue) >= 0 then
    repeat
      Dec(Result)
    until TCmpRel.Compare(FItems[Result], aValue) < 0
  else // < 0
    while TCmpRel.Compare(FItems[Succ(Result)], aValue) < 0 do
      Inc(Result);
end;

function TGBaseSortedList.RightmostLE(constref aValue: T): SizeInt;
begin
  if (ElemCount = 0) or (TCmpRel.Compare(aValue, FItems[0]) < 0) then
    exit(-1);
  if TCmpRel.Compare(aValue, FItems[Pred(ElemCount)]) >= 0 then
    exit(Pred(ElemCount));
  //here such element exist in FItems and not first nor last
  Result := THelper.BinarySearchPos(FItems[0..Pred(ElemCount)], aValue).InsertIndex;
  if TCmpRel.Compare(FItems[Result], aValue) > 0 then
    repeat
      Dec(Result)
    until TCmpRel.Compare(FItems[Result], aValue) <= 0
  else // <= 0
    while TCmpRel.Compare(FItems[Succ(Result)], aValue) <= 0 do
      Inc(Result);
end;

function TGBaseSortedList.NearestGT(constref aValue: T): SizeInt;
begin
  if (ElemCount = 0) or (TCmpRel.Compare(aValue, FItems[Pred(ElemCount)]) >= 0) then
    exit(-1);
  if TCmpRel.Compare(aValue, FItems[0]) < 0 then
    exit(0);
  //here such element exist in FItems and not first nor last
  Result := THelper.BinarySearchPos(FItems[0..Pred(ElemCount)], aValue).InsertIndex;
  if TCmpRel.Compare(FItems[Result], aValue) <= 0 then
    repeat
      Inc(Result)
    until TCmpRel.Compare(FItems[Result], aValue) > 0
  else // > 0
    while TCmpRel.Compare(FItems[Pred(Result)], aValue) > 0 do
      Dec(Result);
end;

function TGBaseSortedList.LeftmostGE(constref aValue: T): SizeInt;
begin
  if (ElemCount = 0) or (TCmpRel.Compare(aValue, FItems[Pred(ElemCount)]) > 0) then
    exit(-1);
  if TCmpRel.Compare(aValue, FItems[0]) <= 0 then
    exit(0);
  //here such element exist in FItems and not first nor last
  Result := THelper.BinarySearchPos(FItems[0..Pred(ElemCount)], aValue).InsertIndex;
  if TCmpRel.Compare(FItems[Result], aValue) < 0 then
    repeat
      Inc(Result)
    until TCmpRel.Compare(FItems[Result], aValue) >= 0
  else // >=
    while TCmpRel.Compare(FItems[Pred(Result)], aValue) >= 0 do
      Dec(Result);
end;

constructor TGBaseSortedList.CreateEmpty;
begin
  inherited Create;
end;

constructor TGBaseSortedList.Create;
begin
  System.SetLength(FItems, DEFAULT_CONTAINER_CAPACITY);
end;

constructor TGBaseSortedList.Create(aCapacity: SizeInt);
begin
  if aCapacity <= MAX_CAPACITY then
    begin
      if aCapacity < 0 then
        aCapacity := 0;
      System.SetLength(FItems, aCapacity);
    end
  else
    CapacityExceedError(aCapacity);
end;

constructor TGBaseSortedList.Create(constref a: array of T);
begin
  FItems := THelper.CreateCopy(a);
  FCount := ListCapacity;
  if ElemCount > 0 then
    THelper.Sort(FItems);
end;

constructor TGBaseSortedList.Create(e: IEnumerable);
begin
  FItems := e.ToArray;
  FCount := ListCapacity;
  if ElemCount > 0 then
    THelper.Sort(FItems);
end;

constructor TGBaseSortedList.Create(aRejectDuplicates: Boolean);
begin
  Create;
  FRejectDuplicates := aRejectDuplicates;
end;

constructor TGBaseSortedList.Create(constref a: array of T; aRejectDuplicates: Boolean);
begin
  FRejectDuplicates := aRejectDuplicates;
  if RejectDuplicates then
    begin
      FItems := THelper.SelectDistinct(a);
      FCount := ListCapacity;
    end
  else
    Create(a);
end;

constructor TGBaseSortedList.Create(e: IEnumerable; aRejectDuplicates: Boolean);
begin
  FRejectDuplicates := aRejectDuplicates;
  if RejectDuplicates then
    begin
      FItems := THelper.SelectDistinct(e.ToArray);
      FCount := ListCapacity;
    end
  else
    Create(e);
end;

destructor TGBaseSortedList.Destroy;
begin
  DoClear;
  inherited;
end;

function TGBaseSortedList.Reverse: IEnumerable;
begin
  BeginIteration;
  Result := GetReverse;
end;

function TGBaseSortedList.ToArray: TArray;
begin
  Result := System.Copy(FItems, 0, ElemCount);
end;

function TGBaseSortedList.FindMin(out aValue: T): Boolean;
begin
  Result := ElemCount > 0;
  if Result then
    aValue := FItems[0];
end;

function TGBaseSortedList.FindMax(out aValue: T): Boolean;
begin
  Result := ElemCount > 0;
  if Result then
    aValue := FItems[Pred(ElemCount)];
end;

function TGBaseSortedList.Insert(constref aValue: T): SizeInt;
begin
  CheckInIteration;
  Result := DoInsert(aValue);
end;

function TGBaseSortedList.Contains(constref aValue: T): Boolean;
begin
  Result := IndexOf(aValue) > -1;
end;

procedure TGBaseSortedList.Delete(aIndex: SizeInt);
begin
  CheckInIteration;
  CheckIndexRange(aIndex);
  DeleteItem(aIndex);
end;

function TGBaseSortedList.TryDelete(aIndex: SizeInt): Boolean;
begin
  Result := not InIteration and IndexInRange(aIndex);
  if Result then
    DeleteItem(aIndex);
end;

function TGBaseSortedList.DeleteAll(aIndex, aCount: SizeInt): SizeInt;
begin
  CheckInIteration;
  CheckIndexRange(aIndex);
  Result := DoDeleteRange(aIndex, aCount);
end;

function TGBaseSortedList.IndexOf(constref aValue: T): SizeInt;
begin
  if ElemCount > 0 then
    Result := THelper.BinarySearch(FItems[0..Pred(ElemCount)], aValue)
  else
    Result := -1;
end;

function TGBaseSortedList.FirstIndexOf(constref aValue: T): SizeInt;
begin
  if ElemCount = 0 then
    exit(-1);
  Result := THelper.BinarySearch(FItems[0..Pred(ElemCount)], aValue);
  while (Result > 0) and (TCmpRel.Compare(aValue, FItems[Pred(Result)]) = 0) do
    Dec(Result);
end;

function TGBaseSortedList.CountOf(constref aValue: T): SizeInt;
var
  LastIdx, FirstIdx: SizeInt;
begin
  if ElemCount = 0 then
    exit(0);
  LastIdx := THelper.BinarySearch(FItems[0..Pred(ElemCount)], aValue);
  if LastIdx < 0 then
    exit(0);
  FirstIdx := LastIdx;
  while (FirstIdx > 0) and (TCmpRel.Compare(aValue, FItems[Pred(FirstIdx)]) = 0) do
    Dec(FirstIdx);
  while (LastIdx < Pred(ElemCount)) and (TCmpRel.Compare(aValue, FItems[Succ(LastIdx)]) = 0) do
    Inc(LastIdx);
  Result := Succ(LastIdx - FirstIdx);
end;

function TGBaseSortedList.IndexOfCeil(constref aValue: T; aInclusive: Boolean): SizeInt;
begin
  if aInclusive then
    Result := LeftmostGE(aValue)
  else
    Result := NearestGT(aValue);
end;

function TGBaseSortedList.IndexOfFloor(constref aValue: T; aInclusive: Boolean): SizeInt;
begin
  if aInclusive then
    Result := RightmostLE(aValue)
  else
    Result := NearestLT(aValue);
end;

function TGBaseSortedList.Head(constref aHighBound: T; aInclusive: Boolean): IEnumerable;
begin
  BeginIteration;
  Result := THeadEnumerable.Create(Self, IndexOfFloor(aHighBound, aInclusive));
end;

function TGBaseSortedList.Tail(constref aLowBound: T; aInclusive: Boolean): IEnumerable;
var
  StartIdx: SizeInt;
begin
  StartIdx := IndexOfCeil(ALowBound, aInclusive);
  if StartIdx < 0 then
    StartIdx := ElemCount;
  BeginIteration;
  Result := TTailEnumerable.Create(Self, StartIdx);
end;

function TGBaseSortedList.Range(constref aLowBound, aHighBound: T; aIncludeBounds: TRangeBounds): IEnumerable;
var
  StartIdx: SizeInt;
begin
  StartIdx := IndexOfCeil(ALowBound, rbLow in aIncludeBounds);
  if StartIdx < 0 then
    StartIdx := ElemCount;
  BeginIteration;
  Result := TRangeEnumerable.Create(Self, StartIdx, IndexOfFloor(aHighBound, rbHigh in aIncludeBounds));
end;

function TGBaseSortedList.HeadList(constref aHighBound: T; aInclusive: Boolean): TSortedList;
var
  HeadCount: SizeInt;
begin
  HeadCount := Succ(IndexOfFloor(aHighBound, aInclusive));
  if HeadCount = 0 then
    exit(TSortedList.CreateEmpty);
  Result := TSortedList.Create(HeadCount);
  Result.FCount := HeadCount;
  THelper.CopyItems(@FItems[0], @Result.FItems[0], HeadCount);
end;

function TGBaseSortedList.TailList(constref aLowBound: T; aInclusive: Boolean): TSortedList;
var
  StartIdx: SizeInt;
begin
  StartIdx := IndexOfCeil(ALowBound, aInclusive);
  if StartIdx < 0 then
    exit(TSortedList.CreateEmpty);
  Result := TSortedList.Create(ElemCount - StartIdx);
  Result.FCount := ElemCount - StartIdx;
  THelper.CopyItems(@FItems[StartIdx], @Result.FItems[0], ElemCount - StartIdx);
end;

function TGBaseSortedList.SubList(constref aLowBound, aHighBound: T; aIncludeBounds: TRangeBounds): TSortedList;
var
  StartIdx, LastIdx, RangeCount: SizeInt;
begin
  StartIdx := IndexOfCeil(ALowBound, rbLow in aIncludeBounds);
  if StartIdx < 0 then
    exit(TSortedList.CreateEmpty);
  LastIdx := IndexOfFloor(aHighBound, rbHigh in aIncludeBounds);
  if LastIdx < StartIdx then
    exit(TSortedList.CreateEmpty);
  RangeCount := Succ(LastIdx - StartIdx);
  Result := TSortedList.Create(RangeCount);
  Result.FCount := RangeCount;
  THelper.CopyItems(@FItems[StartIdx], @Result.FItems[0], RangeCount);
end;

function TGBaseSortedList.Clone: TSortedList;
begin
  Result := TSortedList.CreateEmpty;
  //Result.FItems := System.Copy(FItems, 0, ListCapacity);
  Result.FItems := ToArray; ///////////////
  Result.FCount := ElemCount;
  Result.FRejectDuplicates := RejectDuplicates;
end;

{ TGObjectSortedList }

procedure TGObjectSortedList.DoClear;
var
  I: SizeInt;
begin
  if OwnsObjects then
    for I := 0 to Pred(ElemCount) do
      FItems[I].Free;
  inherited;
end;

procedure TGObjectSortedList.DoSetItem(aIndex: SizeInt; const aValue: T);
var
  sr: THelper.TSearchResult;
  c: SizeInt;
begin
  c := TCmpRel.Compare(aValue, FItems[aIndex]);
  if c <> 0 then
    begin
      CheckInIteration;
      if ElemCount > 1 then
        begin
          sr := THelper.BinarySearchPos(Self.FItems[0..Pred(ElemCount)], aValue);
          if (sr.FoundIndex > -1) and RejectDuplicates then
            begin
              DeleteItem(sr.FoundIndex);
              exit;
            end;
          if OwnsObjects then
            FItems[aIndex].Free;
          if sr.InsertIndex > aIndex then
            System.Move(FItems[Succ(aIndex)], FItems[aIndex], sr.InsertIndex - aIndex)
          else
            System.Move(FItems[sr.InsertIndex], FItems[Succ(sr.InsertIndex)], aIndex - sr.InsertIndex);
          //System.FillChar(FItems[sr.InsertIndex], SizeOf(T), 0);
          FItems[sr.InsertIndex] := aValue;
        end;
    end;
end;

function TGObjectSortedList.DoRemove(constref aValue: T): Boolean;
begin
  Result := inherited DoRemove(aValue);
  if Result and OwnsObjects then
    aValue.Free;
end;

function TGObjectSortedList.DoRemoveIf(aTest: TTest): SizeInt;
var
  I, J: SizeInt;
begin
  Result := ElemCount;
  if Result > 0 then
    begin
      J := 0;
      for I := 0 to Pred(Result) do
        begin
          if aTest(FItems[I]) then
            continue;
          if I > J then
            begin
              if OwnsObjects then
                FItems[J].Free;
              FItems[J] := FItems[I];
            end;
          Inc(J);
        end;
      FCount := J;
      if OwnsObjects then
        for I := ElemCount to Pred(Result) do
          FItems[I].Free;
      Result := Result - ElemCount;
    end;
end;

function TGObjectSortedList.DoRemoveIf(aTest: TOnTest): SizeInt;
var
  I, J: SizeInt;
begin
  Result := ElemCount;
  if Result > 0 then
    begin
      J := 0;
      for I := 0 to Pred(Result) do
        begin
          if aTest(FItems[I]) then
            continue;
          if I > J then
            begin
              if OwnsObjects then
                FItems[J].Free;
              FItems[J] := FItems[I];
            end;
          Inc(J);
        end;
      FCount := J;
      if OwnsObjects then
        for I := ElemCount to Pred(Result) do
          FItems[I].Free;
      Result := Result - ElemCount;
    end;
end;

function TGObjectSortedList.DoRemoveIf(aTest: TNestTest): SizeInt;
var
  I, J: SizeInt;
begin
  Result := ElemCount;
  if Result > 0 then
    begin
      J := 0;
      for I := 0 to Pred(Result) do
        begin
          if aTest(FItems[I]) then
            continue;
          if I > J then
            begin
              if OwnsObjects then
                FItems[J].Free;
              FItems[J] := FItems[I];
            end;
          Inc(J);
        end;
      FCount := J;
      if OwnsObjects then
        for I := ElemCount to Pred(Result) do
          FItems[I].Free;
      Result := Result - ElemCount;
    end;
end;

function TGObjectSortedList.DeleteItem(aIndex: SizeInt): T;
begin
  Result := inherited DeleteItem(aIndex);
  if OwnsObjects then
    Result.Free;
end;

function TGObjectSortedList.DoDeleteRange(aIndex, aCount: SizeInt): SizeInt;
var
  I: SizeInt;
begin
  if aCount < 0 then
    aCount := 0;
  Result := Math.Min(aCount, ElemCount - aIndex);
  if Result > 0 then
    begin
      if OwnsObjects then
        for I := aIndex to Pred(aIndex + Result) do
          FItems[I].Free;
      FCount -= Result;
      System.Move(FItems[aIndex + Result], FItems[aIndex], SizeOf(T) * (ElemCount - aIndex));
      //System.FillChar(FItems[ElemCount], SizeOf(T) * Result, 0);
    end;
end;

constructor TGObjectSortedList.CreateEmpty(aOwnsObjects: Boolean);
begin
  inherited CreateEmpty;
  OwnsObjects := aOwnsObjects;
end;

constructor TGObjectSortedList.Create(aOwnsObjects: Boolean);
begin
  inherited Create;
  OwnsObjects := aOwnsObjects;
end;

constructor TGObjectSortedList.Create(aCapacity: SizeInt; aOwnsObjects: Boolean);
begin
  inherited Create(aCapacity);
  OwnsObjects := aOwnsObjects;
end;

constructor TGObjectSortedList.Create(constref a: array of T; aOwnsObjects: Boolean);
begin
  inherited Create(a);
  OwnsObjects := aOwnsObjects;
end;

constructor TGObjectSortedList.Create(e: IEnumerable; aOwnsObjects: Boolean);
begin
  inherited Create(e);
  OwnsObjects := aOwnsObjects;
end;

constructor TGObjectSortedList.Create(aRejectDuplicates: Boolean; aOwnsObjects: Boolean);
begin
  inherited Create(aRejectDuplicates);
  OwnsObjects := aOwnsObjects;
end;

constructor TGObjectSortedList.Create(constref a: array of T; aRejectDuplicates: Boolean; aOwnsObjects: Boolean);
begin
  inherited Create(a, aRejectDuplicates);
  OwnsObjects := aOwnsObjects;
end;

constructor TGObjectSortedList.Create(e: IEnumerable; aRejectDuplicates: Boolean; aOwnsObjects: Boolean);
begin
  inherited Create(e, aRejectDuplicates);
  OwnsObjects := aOwnsObjects;
end;

function TGObjectSortedList.Clone: TObjectSortedList;
begin
  Result := TObjectSortedList.CreateEmpty(OwnsObjects);
  //Result.FItems := System.Copy(FItems, 0, ListCapacity);
  Result.FItems := ToArray; ///////////////
  Result.FCount := ElemCount;
  Result.FRejectDuplicates := RejectDuplicates;
end;

{ TGSortedList2.TEnumerator }

function TGSortedList2.TEnumerator.GetCurrent: T;
begin
  Result := FList[FCurrIndex];
end;

procedure TGSortedList2.TEnumerator.Init(aList: TGSortedList2);
begin
  FList := aList.FItems;
  FLastIndex := Pred(aList.Count);
  FCurrIndex := -1;
end;

function TGSortedList2.TEnumerator.MoveNext: Boolean;
begin
  Result := FCurrIndex < FLastIndex;
  FCurrIndex += Ord(Result);
end;

procedure TGSortedList2.TEnumerator.Reset;
begin
  FCurrIndex := -1;
end;

{ TGSortedList2 }

function TGSortedList2.GetCapacity: SizeInt;
begin
  Result := System.Length(FItems);
end;

procedure TGSortedList2.Expand(aValue: SizeInt);
begin
  //there aValue > Capacity
  if aValue <= DEFAULT_CONTAINER_CAPACITY then
    System.SetLength(FItems, DEFAULT_CONTAINER_CAPACITY)
  else
    if aValue <= MAX_CAPACITY then
      begin
        aValue := Math.Min(MAX_CAPACITY, LGUtils.RoundUpTwoPower(aValue));
        System.SetLength(FItems, aValue);
      end
    else
      CapacityExceedError(aValue);
end;

procedure TGSortedList2.ItemAdding;
begin
  if Count = Capacity then
    Expand(Succ(Count));
end;

procedure TGSortedList2.InsertItem(aIndex: SizeInt; constref aValue: T);
begin
  ItemAdding;
  if aIndex < Count then
    begin
      System.Move(FItems[aIndex], FItems[Succ(aIndex)], SizeOf(T) * (Count - aIndex));
      System.FillChar(FItems[aIndex], SizeOf(T), 0);
    end;
  FItems[aIndex] := aValue;
  Inc(FCount);
end;

procedure TGSortedList2.RemoveItem(aIndex: SizeInt);
begin
  FItems[aIndex] := Default(T);
  Dec(FCount);
  System.Move(FItems[Succ(aIndex)], FItems[aIndex], SizeOf(T) * (Count - aIndex));
  System.FillChar(FItems[Count], SizeOf(T), 0);
end;

procedure TGSortedList2.CapacityExceedError(aValue: SizeInt);
begin
  raise ELGCapacityExceed.CreateFmt(SEClassCapacityExceedFmt, [ClassName, aValue]);
end;

constructor TGSortedList2.CreateEmpty;
begin
  inherited Create;
end;

constructor TGSortedList2.CreateEmpty(aAllowDuplicates: Boolean);
begin
  inherited Create;
  FAllowDuplicates := aAllowDuplicates;
end;

constructor TGSortedList2.Create;
begin
  System.SetLength(FItems, DEFAULT_CONTAINER_CAPACITY);
end;

constructor TGSortedList2.Create(aCapacity: SizeInt);
begin
  if aCapacity <= MAX_CAPACITY then
    begin
      if aCapacity < 0 then
        aCapacity := 0;
      System.SetLength(FItems, aCapacity);
    end
  else
    CapacityExceedError(aCapacity);
end;

constructor TGSortedList2.Create(aCapacity: SizeInt; AllowDuplicates: Boolean);
begin
  Create(aCapacity);
  FAllowDuplicates := AllowDuplicates;
end;

destructor TGSortedList2.Destroy;
begin
  Clear;
  inherited;
end;

function TGSortedList2.GetEnumerator: TEnumerator;
begin
  Result.Init(Self);
end;

procedure TGSortedList2.Clear;
begin
  FItems := nil;
  FCount := 0;
end;

function TGSortedList2.EnsureCapacity(aValue: SizeInt): Boolean;
begin
  try
    if aValue > Capacity then
      Expand(aValue);
    Result := True;
  except
    Result := False;
  end;
end;

procedure TGSortedList2.TrimToFit;
begin
  System.SetLength(FItems, Count);
end;

function TGSortedList2.Add(constref aValue: T): Boolean;
var
  sr: THelper.TSearchResult;
begin
  if Count > 0 then
    begin
      sr := THelper.BinarySearchPos(FItems[0..Pred(Count)], aValue);
      if (sr.FoundIndex >= 0) and not AllowDuplicates then
        exit(False);
      InsertItem(sr.InsertIndex, aValue);
    end
  else
    InsertItem(Count, aValue);
  Result := True;
end;

function TGSortedList2.Contains(constref aValue: T): Boolean;
begin
  if Count > 0 then
    Result := THelper.BinarySearch(FItems[0..Pred(Count)], aValue) >= 0
  else
    Result := False;
end;

function TGSortedList2.Remove(constref aValue: T): Boolean;
var
  RemoveIdx: SizeInt;
begin
  if Count > 0 then
    begin
      RemoveIdx := THelper.BinarySearch(FItems[0..Pred(Count)], aValue);
      Result := RemoveIdx >= 0;
      if Result then
        RemoveItem(RemoveIdx);
    end
  else
    Result := False;
end;

{ TGSortedListTable.TEntryCmpRel }

class function TGSortedListTable.TEntryCmpRel.Compare(constref L, R: TEntry): SizeInt;
begin
  Result := TCmpRel.Compare(L.Key, R.Key);
end;

{ TGSortedListTable.TEnumerator }

function TGSortedListTable.TEnumerator.GetCurrent: PEntry;
begin
  Result := @FList[FCurrIndex];
end;

procedure TGSortedListTable.TEnumerator.Init(aTable: TGSortedListTable);
begin
  FList := aTable.FItems;
  FLastIndex := Pred(aTable.Count);
  FCurrIndex := -1;
end;

function TGSortedListTable.TEnumerator.MoveNext: Boolean;
begin
  Result := FCurrIndex < FLastIndex;
  FCurrIndex += Ord(Result);
end;

procedure TGSortedListTable.TEnumerator.Reset;
begin
  FCurrIndex := -1;
end;

{ TGSortedListTable }

function TGSortedListTable.GetCapacity: SizeInt;
begin
  Result := System.Length(FItems);
end;

procedure TGSortedListTable.Expand(aValue: SizeInt);
begin
  //there aValue > Capacity
  if aValue <= DEFAULT_CONTAINER_CAPACITY then
    System.SetLength(FItems, DEFAULT_CONTAINER_CAPACITY)
  else
    if aValue <= MAX_CAPACITY then
      begin
        aValue := Math.Min(MAX_CAPACITY, LGUtils.RoundUpTwoPower(aValue));
        System.SetLength(FItems, aValue);
      end
    else
      CapacityExceedError(aValue);
end;

procedure TGSortedListTable.ItemAdding;
begin
  if Count = Capacity then
    Expand(Succ(Count));
end;

procedure TGSortedListTable.InsertItem(aIndex: SizeInt; constref aValue: TEntry);
begin
  ItemAdding;
  if aIndex < Count then
    begin
      System.Move(FItems[aIndex], FItems[Succ(aIndex)], SizeOf(TEntry) * (Count - aIndex));
      System.FillChar(FItems[aIndex], SizeOf(TEntry), 0);
    end;
  FItems[aIndex] := aValue;
  Inc(FCount);
end;

procedure TGSortedListTable.RemoveItem(aIndex: SizeInt);
begin
  FItems[aIndex] := Default(TEntry);
  Dec(FCount);
  System.Move(FItems[Succ(aIndex)], FItems[aIndex], SizeOf(TEntry) * (Count - aIndex));
  System.FillChar(FItems[Count], SizeOf(TEntry), 0);
end;

procedure TGSortedListTable.CapacityExceedError(aValue: SizeInt);
begin
  raise ELGCapacityExceed.CreateFmt(SEClassCapacityExceedFmt, [ClassName, aValue]);
end;

constructor TGSortedListTable.CreateEmpty;
begin
  inherited Create;
end;

constructor TGSortedListTable.CreateEmpty(AllowDuplicates: Boolean);
begin
  inherited Create;
  FAllowDuplicates := AllowDuplicates;
end;

constructor TGSortedListTable.Create;
begin
  System.SetLength(FItems, DEFAULT_CONTAINER_CAPACITY);
end;

constructor TGSortedListTable.Create(aCapacity: SizeInt);
begin
  if aCapacity <= MAX_CAPACITY then
    begin
      if aCapacity < 0 then
        aCapacity := 0;
      System.SetLength(FItems, aCapacity);
    end
  else
    CapacityExceedError(aCapacity);
end;

constructor TGSortedListTable.Create(aCapacity: SizeInt; AllowDuplicates: Boolean);
begin
  Create(aCapacity);
  FAllowDuplicates := AllowDuplicates;
end;

destructor TGSortedListTable.Destroy;
begin
  Clear;
  inherited;
end;

function TGSortedListTable.GetEnumerator: TEnumerator;
begin
  Result.Init(Self);
end;

procedure TGSortedListTable.Clear;
begin
  FItems := nil;
  FCount := 0;
end;

function TGSortedListTable.EnsureCapacity(aValue: SizeInt): Boolean;
begin
  try
    if aValue > Capacity then
      Expand(aValue);
    Result := True;
  except
    Result := False;
  end;
end;

procedure TGSortedListTable.TrimToFit;
begin
  System.SetLength(FItems, Count);
end;

function TGSortedListTable.FindOrAdd(constref aKey: TKey; out e: PEntry; out aPos: SizeInt): Boolean;
var
  sr: specialize TGBaseArrayHelper<TEntry, TEntryCmpRel>.TSearchResult;
  Entry: TEntry;
begin
  Entry.Key := aKey;
  if Count > 0 then
    begin
      sr := specialize TGBaseArrayHelper<TEntry, TEntryCmpRel>.BinarySearchPos(FItems[0..Pred(Count)], Entry);
      Result := sr.FoundIndex >= 0;
      if Result then
        aPos := sr.FoundIndex
      else
        begin
          aPos := sr.InsertIndex;
          InsertItem(aPos, Entry);
        end;
    end
  else
    begin
      Result := False;
      aPos := 0;
      InsertItem(aPos, Entry);
    end;
  e := @FItems[aPos];
end;

function TGSortedListTable.Find(constref aKey: TKey; out aPos: SizeInt): PEntry;
var
  e: TEntry;
begin
  e.Key := aKey;
  Result := nil;
  if Count > 0 then
    begin
      aPos := specialize TGBaseArrayHelper<TEntry, TEntryCmpRel>.BinarySearch(FItems[0..Pred(Count)], e);
      if aPos >= 0 then
        Result := @FItems[aPos];
    end;
end;

function TGSortedListTable.Add(constref aKey: TKey): PEntry;
var
  sr: specialize TGBaseArrayHelper<TEntry, TEntryCmpRel>.TSearchResult;
  Entry: TEntry;
begin
  Result := nil;
  Entry.Key := aKey;
  if Count > 0 then
    begin
      sr := specialize TGBaseArrayHelper<TEntry, TEntryCmpRel>.BinarySearchPos(FItems[0..Pred(Count)], Entry);
      if (sr.FoundIndex < 0) or AllowDuplicates then
        begin
          InsertItem(sr.InsertIndex, Entry);
          Result := @FItems[sr.InsertIndex];
        end;
    end
  else
    begin
      InsertItem(0, Entry);
      Result := @FItems[0];
    end;
end;

function TGSortedListTable.Remove(constref aKey: TKey): Boolean;
var
  e: TEntry;
  RemoveIdx: SizeInt;
begin
  e.Key := aKey;
  RemoveIdx := specialize TGBaseArrayHelper<TEntry, TEntryCmpRel>.BinarySearch(FItems[0..Pred(Count)], e);
  Result := RemoveIdx >= 0;
  if Result then
    RemoveItem(RemoveIdx);
end;

procedure TGSortedListTable.RemoveAt(constref aPos: SizeInt);
begin
  if (aPos >= 0) and (aPos < Count) then
    RemoveItem(aPos);
end;

end.

