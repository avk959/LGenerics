{****************************************************************************
*                                                                           *
*   This file is part of the LGenerics package.                             *
*   Generic deque(double ended queue) implementation.                       *
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
unit LGDeque;

{$mode objfpc}{$H+}
{$INLINE ON}{$WARN 6058 off : }
{$MODESWITCH ADVANCEDRECORDS}

interface

uses

  SysUtils,
  LGUtils,
  {%H-}LGHelpers,
  LGArrayHelpers,
  LGAbstractContainer,
  LGStrConst;

type

  generic TGDeque<T> = class(specialize TGCustomRingArrayBuffer<T>, specialize IGDeque<T>)
  protected
  const
    SIZE_CUTOFF = 64;

    procedure DoPushFirst(constref aValue: T);
    function  TailIndex: SizeInt; inline;
    function  PeekTail: T; inline;
    function  ExtractTail: T;
    function  AddArray2Head(constref a: array of T): SizeInt;
    function  AddContainer2Head(aContainer: TCustomContainer): SizeInt;
    function  AddEnum2Head(e: IEnumerable): SizeInt;
    function  InternalIndex(aIndex: SizeInt): SizeInt; inline;
    function  FastGetItem(aIndex: SizeInt): T; inline;
    procedure FastSetItem(aIndex: SizeInt; aValue: T); inline;
    procedure FastSwap(L, R: SizeInt);
    function  GetItem(aIndex: SizeInt): T; inline;
    procedure SetItem(aIndex: SizeInt; const aValue: T); virtual;
    procedure ShiftHeadRight(aToIndex: SizeInt);
    procedure ShiftHeadLeft(aFromIndex: SizeInt);
    procedure ShiftTailRight(aFromIndex: SizeInt);
    procedure ShiftTailLeft(aToIndex: SizeInt);
    procedure InsertItem(aIndex: SizeInt; aValue: T);
    function  ExtractItem(aIndex: SizeInt): T;
    function  DeleteItem(aIndex: SizeInt): T; virtual;
  public
    procedure PushFirst(constref aValue: T); inline;
    function  PushAllFirst(constref a: array of T): SizeInt;
    function  PushAllFirst(e: IEnumerable): SizeInt; inline;
    procedure PushLast(constref aValue: T); inline;
    function  PushAllLast(constref a: array of T): SizeInt;
    function  PushAllLast(e: IEnumerable): SizeInt; inline;
  { EXTRACTS element from the head of deque; will raise ELGAccessEmpty if inctance is empty;
    will raise ELGUpdateLock if instance in iteration }
    function  PopFirst: T;
    function  TryPopFirst(out aValue: T): Boolean;
  { EXTRACTS element from the tail of deque; will raise ELGAccessEmpty if inctance is empty;
    will raise ELGUpdateLock if instance in iteration }
    function  PopLast: T;
    function  TryPopLast(out aValue: T): Boolean;
  { examines element in the head of deque; will raise ELGAccessEmpty if inctance is empty }
    function  PeekFirst: T;
    function  TryPeekFirst(out aValue: T): Boolean;
  { examines element in the tail of deque; will raise ELGAccessEmpty if inctance is empty }
    function  PeekLast: T;
    function  TryPeekLast(out aValue: T): Boolean;
  { inserts aValue into position aIndex;
    will raise ELGListError if aIndex out of bounds(aIndex = Count  is allowed);
    will raise ELGUpdateLock if instance in iteration }
    procedure Insert(aIndex: SizeInt; constref aValue: T);
  { will return False if aIndex out of bounds or instance in iteration }
    function  TryInsert(aIndex: SizeInt; constref aValue: T): Boolean;
  { extracts value from position aIndex;
    will raise ELGListError if aIndex out of bounds;
    will raise ELGUpdateLock if instance in iteration }
    function  Extract(aIndex: SizeInt): T;
  { will return False if aIndex out of bounds or instance in iteration }
    function  TryExtract(aIndex: SizeInt; out aValue: T): Boolean;
  { deletes value in position aIndex;
    will raise ELGListError if aIndex out of bounds;
    will raise ELGUpdateLock if instance in iteration }
    procedure Delete(aIndex: SizeInt);
  { will return False if aIndex out of bounds or instance in iteration }
    function  TryDelete(aIndex: SizeInt): Boolean;
    property  Items[aIndex: SizeInt]: T read GetItem write SetItem; default;
  end;

  { TGObjectDeque notes:
    TGObjectDeque.PopFirst(or TGObjectDeque.TryPopFirst) and
    TGObjectDeque.PopLast(or TGObjectDeque.TryPopLast) EXTRACTS object from deque:
    one must to free this object yourself;
    for equality comparision of items uses TObjectHelper from LGHelpers  }
  generic TGObjectDeque<T: class> = class(specialize TGDeque<T>)
  private
    FOwnsObjects: Boolean;
  protected
    procedure DoClear; override;
    procedure SetItem(aIndex: SizeInt; const aValue: T); override;
    function  DeleteItem(aIndex: SizeInt): T; override;
  public
    constructor Create(aOwnsObjects: Boolean = True);
    constructor Create(aCapacity: SizeInt; aOwnsObjects: Boolean = True);
    constructor Create(constref A: array of T; aOwnsObjects: Boolean = True);
    constructor Create(e: IEnumerable; aOwnsObjects: Boolean = True);
    property  OwnsObjects: Boolean read FOwnsObjects write FOwnsObjects;
  end;

  generic TGThreadDeque<T> = class
  public
  type
    IDeque = specialize IGDeque<T>;
  private
    FDeque: IDeque;
    FLock: TRtlCriticalSection;
    procedure DoLock; inline;
  public
    constructor Create(aDeque: IDeque);
    destructor Destroy; override;
    procedure Clear;
    procedure PushFirst(constref aValue: T); inline;
    procedure PushLast(constref aValue: T); inline;
    function  TryPopFirst(out aValue: T): Boolean;
    function  TryPopLast(out aValue: T): Boolean;
    function  TryPeekFirst(out aValue: T): Boolean;
    function  TryPeekLast(out aValue: T): Boolean;
    function  Lock: IDeque;
    procedure Unlock; inline;
  end;

  generic TGLiteDeque<T> = record
  public
  type
    TBuffer     = specialize TGLiteRingDynBuffer<T>;
    TEnumerator = TBuffer.TEnumerator;
    TMutables   = TBuffer.TMutables;
    TReverse    = TBuffer.TReverse;
    TArray      = TBuffer.TArray;
    PItem       = TBuffer.PItem;

  private
  const
    SIZE_CUTOFF = 64;

  type
    TFake = {$IFNDEF FPC_REQUIRES_PROPER_ALIGNMENT}array[0..Pred(SizeOf(T))] of Byte{$ELSE}T{$ENDIF};

  var
    FBuffer: TBuffer;
    function  GetCapacity: SizeInt; inline;
    function  InternalIndex(aIndex: SizeInt): SizeInt; inline;
    function  FastGetItem(aIndex: SizeInt): T; inline;
    procedure FastSetItem(aIndex: SizeInt; constref aValue: T); inline;
    procedure FastSwap(L, R: SizeInt); inline;
    function  GetItem(aIndex: SizeInt): T; inline;
    function  GetMutable(aIndex: SizeInt): PItem; inline;
    procedure SetItem(aIndex: SizeInt; const aValue: T); inline;
    procedure ShiftHeadRight(aToIndex: SizeInt);
    procedure ShiftHeadLeft(aFromIndex: SizeInt);
    procedure ShiftTailRight(aFromIndex: SizeInt);
    procedure ShiftTailLeft(aToIndex: SizeInt);
    procedure InsertItem(aIndex: SizeInt; aValue: T);
    function  DeleteItem(aIndex: SizeInt): T; inline;
  public
    function  GetEnumerator: TEnumerator; inline;
    function  Mutables: TMutables; inline; //
    function  Reverse: TReverse; inline;
    function  ToArray: TArray; inline;
    procedure Clear; inline;
    function  IsEmpty: Boolean; inline;
    function  NonEmpty: Boolean; inline;
    procedure EnsureCapacity(aValue: SizeInt); inline;
    procedure TrimToFit; inline;
    procedure PushFirst(constref aValue: T); inline;
    procedure PushLast(constref aValue: T); inline;
  { EXTRACTS element from the head of deque; will raise ELGAccessEmpty if inctance is empty }
    function  PopFirst: T; inline;
    function  TryPopFirst(out aValue: T): Boolean; inline;
  { EXTRACTS element from the tail of deque; will raise ELGAccessEmpty if inctance is empty }
    function  PopLast: T; inline;
    function  TryPopLast(out aValue: T): Boolean; inline;
  { examines element in the head of deque; will raise ELGAccessEmpty if inctance is empty }
    function  PeekFirst: T; inline;
    function  TryPeekFirst(out aValue: T): Boolean;
  { examines element in the tail of deque; will raise ELGAccessEmpty if inctance is empty }
    function  PeekLast: T; inline;
    function  TryPeekLast(out aValue: T): Boolean; inline;
  { inserts aValue into position aIndex;
    will raise ELGListError if aIndex out of bounds(aIndex = Count  is allowed) }
    procedure Insert(aIndex: SizeInt; constref aValue: T);
  { will return False if aIndex out of bounds }
    function  TryInsert(aIndex: SizeInt; constref aValue: T): Boolean;
  { deletes and returns value from position aIndex;
    will raise ELGListError if aIndex out of bounds }
    function  Delete(aIndex: SizeInt): T;
  { will return False if aIndex out of bounds }
    function  TryDelete(aIndex: SizeInt; out aValue: T): Boolean;
    property  Count: SizeInt read FBuffer.FCount;
    property  Capacity: SizeInt read GetCapacity;
    property  Items[aIndex: SizeInt]: T read GetItem write SetItem; default;
    property  Mutable[aIndex: SizeInt]: PItem read GetMutable;
  end;

  generic TGLiteThreadDeque<T> = class
  public
  type
    TDeque = specialize TGLiteDeque<T>;
    PDeque = ^TDeque;

  private
    FDeque: TDeque;
    FLock: TRtlCriticalSection;
    procedure DoLock; inline;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure PushFirst(constref aValue: T); inline;
    procedure PushLast(constref aValue: T); inline;
    function  TryPopFirst(out aValue: T): Boolean;
    function  TryPopLast(out aValue: T): Boolean;
    function  TryPeekFirst(out aValue: T): Boolean;
    function  TryPeekLast(out aValue: T): Boolean;
    function  Lock: PDeque;
    procedure Unlock; inline;
  end;

  { TGLiteObjectDeque notes:
    TGLiteObjectDeque.PopFirst(or TGLiteObjectDeque.TryPopFirst) and
    TGLiteObjectDeque.PopLast(or TGLiteObjectDeque.TryPopLast) EXTRACTS object from deque:
    one must to free this object yourself;
    for equality comparision of items uses TObjectHelper from LGHelpers }
  generic TGLiteObjectDeque<T: class> = record
  public
  type
    TDeque      = specialize TGLiteDeque<T>;
    PDeque      = ^TDeque;
    TEnumerator = TDeque.TEnumerator;
    TReverse    = TDeque.TReverse;
    TArray      = TDeque.TArray;

  private
    FDeque: TDeque;
    FOwnsObjects: Boolean;
    procedure CheckFreeItems;
    function  GetCapacity: SizeInt; inline;
    function  GetCount: SizeInt; inline;
    function  GetItem(aIndex: SizeInt): T; inline;
    procedure SetItem(aIndex: SizeInt; const aValue: T);
    class operator Initialize(var d: TGLiteObjectDeque);
    class operator Copy(constref aSrc: TGLiteObjectDeque; var aDst: TGLiteObjectDeque);
  public
    function  InnerDeque: PDeque;
    function  GetEnumerator: TEnumerator; inline;
    function  Reverse: TReverse; inline;
    function  ToArray: TArray; inline;
    procedure Clear; inline;
    function  IsEmpty: Boolean; inline;
    function  NonEmpty: Boolean; inline;
    procedure EnsureCapacity(aValue: SizeInt); inline;
    procedure TrimToFit; inline;
    procedure PushFirst(constref aValue: T); inline;
    procedure PushLast(constref aValue: T); inline;
  { EXTRACTS element from the head of deque; will raise ELGAccessEmpty if inctance is empty }
    function  PopFirst: T; inline;
    function  TryPopFirst(out aValue: T): Boolean; inline;
  { EXTRACTS element from the tail of deque; will raise ELGAccessEmpty if inctance is empty }
    function  PopLast: T; inline;
    function  TryPopLast(out aValue: T): Boolean;
  { examines element in the head of deque; will raise ELGAccessEmpty if inctance is empty }
    function  PeekFirst: T; inline;
    function  TryPeekFirst(out aValue: T): Boolean;
  { examines element in the tail of deque; will raise ELGAccessEmpty if inctance is empty }
    function  PeekLast: T; inline;
    function  TryPeekLast(out aValue: T): Boolean; inline;
  { inserts aValue into position aIndex;
    will raise ELGListError if aIndex out of bounds(aIndex = Count  is allowed) }
    procedure Insert(aIndex: SizeInt; constref aValue: T); inline;
  { will return False if aIndex out of bounds }
    function  TryInsert(aIndex: SizeInt; constref aValue: T): Boolean; inline;
  { extracts value from position aIndex;
    will raise ELGListError if aIndex out of bounds }
    function  Extract(aIndex: SizeInt): T; inline;
  { will return False if aIndex out of bounds }
    function  TryExtract(aIndex: SizeInt; out aValue: T): Boolean; inline;
  { deletes value in position aIndex;
    will raise ELGListError if aIndex out of bounds }
    procedure Delete(aIndex: SizeInt); inline;
  { will return False if aIndex out of bounds }
    function  TryDelete(aIndex: SizeInt): Boolean; inline;
    property  Count: SizeInt read GetCount;
    property  Capacity: SizeInt read GetCapacity;
    property  Items[aIndex: SizeInt]: T read GetItem write SetItem; default;
    property  OwnsObjects: Boolean read FOwnsObjects write FOwnsObjects;
  end;

  generic TGLiteThreadObjectDeque<T: class> = class
  public
  type
    TDeque = specialize TGLiteObjectDeque<T>;
    PDeque = ^TDeque;

  private
    FDeque: TDeque;
    FLock: TRtlCriticalSection;
    procedure DoLock; inline;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure PushFirst(constref aValue: T); inline;
    procedure PushLast(constref aValue: T); inline;
    function  TryPopFirst(out aValue: T): Boolean;
    function  TryPopLast(out aValue: T): Boolean;
    function  TryPeekFirst(out aValue: T): Boolean;
    function  TryPeekLast(out aValue: T): Boolean;
    function  Lock: PDeque;
    procedure Unlock; inline;
  end;

  { TGDequeHelpUtil }

  generic TGDequeHelpUtil<T> = class
  public
  type
    TDeque           = class(specialize TGDeque<T>);
    TLiteDeque       = specialize TGLiteDeque<T>;
    TEqualityCompare = specialize TGEqualCompare<T>;
  protected
    class procedure DoReverse(d: TDeque; L, R: SizeInt); static; inline;
    class procedure DoReverse(var d: TLiteDeque; L, R: SizeInt); static; inline;
  public
    class procedure Reverse(d: TDeque); static;
    class procedure Reverse(var d: TLiteDeque); static; inline;
    class procedure RandomShuffle(d: TDeque); static;
    class procedure RandomShuffle(var d: TLiteDeque); static;
    class function  SequentSearch(d: TDeque; constref aValue: T; c: TEqualityCompare): SizeInt; static;
    class function  SequentSearch(constref d: TLiteDeque; constref aValue: T; c: TEqualityCompare): SizeInt; static;
    class function  Same(d1, d2: TDeque; c: TEqualityCompare): Boolean; static;
    class function  Same(constref d1, d2: TLiteDeque; c: TEqualityCompare): Boolean; static;
  end;

  { TGBaseDequeHelper }

  generic TGBaseDequeHelper<T, TCmpRel> = class(specialize TGDequeHelpUtil<T>)
  protected
  type
    THelper    = class(specialize TGBaseArrayHelper<T, TCmpRel>);
    TFake      = THelper.TFake;
    TSortSplit = THelper.TSortSplit;
    class function  DoBinSearch(d: TDeque; constref aValue: T): SizeInt; static;
    class function  DoBinSearch(constref d: TLiteDeque; constref aValue: T): SizeInt; static;
    class function  CountRun2Asc(d: TDeque; R: SizeInt): SizeInt;
    class function  CountRun2Asc(var d: TLiteDeque; R: SizeInt): SizeInt;
    class procedure InsertionSort(d: TDeque; L, R: SizeInt); static;
    class procedure InsertionSort(var d: TLiteDeque; L, R: SizeInt); static;
    class procedure DoHeapSort(d: TDeque; L, R: SizeInt); static;
    class procedure DoHeapSort(var d: TLiteDeque; L, R: SizeInt); static;
    class function  QSplitR(d: TDeque; L, R: SizeInt): TSortSplit; static;
    class function  QSplitR(var d: TLiteDeque; L, R: SizeInt): TSortSplit; static;
    class procedure DoQSort(d: TDeque; L, R: SizeInt); static;
    class procedure DoQSort(var d: TLiteDeque; L, R: SizeInt); static;
    class function  GetMo9Pivot(d: TDeque; L, R: SizeInt): T; static;
    class function  GetMo9Pivot(constref d: TLiteDeque; L, R: SizeInt): T; static;
    class function  QSplitMo9(d: TDeque; L, R: SizeInt): TSortSplit; static;
    class function  QSplitMo9(var d: TLiteDeque; L, R: SizeInt): TSortSplit; static;
    class procedure DoIntroSort(d: TDeque; L, R, Ttl: SizeInt); static;
    class procedure DoIntroSort(var d: TLiteDeque; L, R, Ttl: SizeInt); static;
  public
  type
    TOptional = specialize TGOptional<T>;
    class function  SequentSearch(d: TDeque; constref aValue: T): SizeInt; static;
    class function  SequentSearch(constref d: TLiteDeque; constref aValue: T): SizeInt; static;
    class function  BinarySearch(d: TDeque; constref aValue: T): SizeInt; static; inline;
    class function  BinarySearch(constref d: TLiteDeque; constref aValue: T): SizeInt; static; inline;
    class function  IndexOfMin(d: TDeque): SizeInt; static;
    class function  IndexOfMin(constref d: TLiteDeque): SizeInt; static;
    class function  IndexOfMax(d: TDeque): SizeInt; static;
    class function  IndexOfMax(constref d: TLiteDeque): SizeInt; static;
    class function  GetMin(d: TDeque): TOptional; static;
    class function  GetMin(constref d: TLiteDeque): TOptional; static;
    class function  GetMax(d: TDeque): TOptional; static;
    class function  GetMax(constref d: TLiteDeque): TOptional; static;
    class function  FindMin(d: TDeque; out aValue: T): Boolean; static;
    class function  FindMin(constref d: TLiteDeque; out aValue: T): Boolean; static;
    class function  FindMax(d: TDeque; out aValue: T): Boolean; static;
    class function  FindMax(constref d: TLiteDeque; out aValue: T): Boolean; static;
    class function  FindMinMax(d: TDeque; out aMin, aMax: T): Boolean; static;
    class function  FindMinMax(constref d: TLiteDeque; out aMin, aMax: T): Boolean; static;
    class function  FindNthSmallest(d: TDeque; N: SizeInt; out aValue: T): Boolean; static;
    class function  FindNthSmallest(constref d: TLiteDeque; N: SizeInt; out aValue: T): Boolean; static;
    class function  NthSmallest(d: TDeque; N: SizeInt): TOptional; static;
    class function  NthSmallest(constref d: TLiteDeque; N: SizeInt): TOptional; static;
    class function  NextPermutation2Asc(d: TDeque): Boolean; static;
    class function  NextPermutation2Asc(var d: TLiteDeque): Boolean; static;
    class function  NextPermutation2Desc(d: TDeque): Boolean; static;
    class function  NextPermutation2Desc(var d: TLiteDeque): Boolean; static;
    class function  IsNonDescending(d: TDeque): Boolean; static;
    class function  IsNonDescending(constref d: TLiteDeque): Boolean; static;
    class function  IsStrictAscending(d: TDeque): Boolean; static;
    class function  IsStrictAscending(constref d: TLiteDeque): Boolean; static;
    class function  IsNonAscending(d: TDeque): Boolean; static;
    class function  IsNonAscending(constref d: TLiteDeque): Boolean; static;
    class function  IsStrictDescending(d: TDeque): Boolean; static;
    class function  IsStrictDescending(constref d: TLiteDeque): Boolean; static;
    class function  Same(d1, d2: TDeque): Boolean; static;
    class function  Same(constref d1, d2: TLiteDeque): Boolean; static;
    class procedure QuickSort(d: TDeque; o: TSortOrder = soAsc); static;
    class procedure QuickSort(var d: TLiteDeque; o: TSortOrder = soAsc); static;
    class procedure IntroSort(d: TDeque; o: TSortOrder = soAsc); static;
    class procedure IntroSort(var d: TLiteDeque; o: TSortOrder = soAsc); static;
    class procedure Sort(d: TDeque; o: TSortOrder = soAsc); static; inline;
    class procedure Sort(var d: TLiteDeque; o: TSortOrder = soAsc); static; inline;
  { copies only distinct values from v }
    class function  SelectDistinct(d: TDeque): TDeque.TArray; static; inline;
    class function  SelectDistinct(constref d: TLiteDeque): TDeque.TArray; static; inline;
  end;

  { TGDequeHelper assumes that type T implements TCmpRel }
  generic TGDequeHelper<T> = class(specialize TGBaseDequeHelper<T, T>);

implementation
{$B-}{$COPERATORS ON}

{ TGDeque }

procedure TGDeque.DoPushFirst(constref aValue: T);
begin
  ItemAdding;
  Dec(FHead);
  if Head < 0 then
    FHead += System.Length(FItems);
  Inc(FCount);
  FItems[Head] := aValue;
end;

function TGDeque.TailIndex: SizeInt;
begin
  Result := InternalIndex(Pred(ElemCount));
end;

function TGDeque.PeekTail: T;
begin
  Result := FItems[TailIndex];
end;

function TGDeque.ExtractTail: T;
var
  TailPos: SizeInt;
begin
  TailPos := TailIndex;
  Dec(FCount);
  Result := FItems[TailPos];
  FItems[TailPos] := Default(T);
end;

function TGDeque.AddArray2Head(constref a: array of T): SizeInt;
var
  HeadPos, I, c: SizeInt;
begin
  Result := System.Length(a);
  if Result > 0 then
    begin
      DoEnsureCapacity(ElemCount + Result);
      c := System.Length(FItems);
      HeadPos := Head;
      for I := 0 to System.High(a) do
        begin
          Dec(HeadPos);
          if HeadPos < 0 then
            HeadPos += c;
          FItems[HeadPos] := a[I];
        end;
      FCount += Result;
      FHead := HeadPos;
    end;
end;

function TGDeque.AddContainer2Head(aContainer: TCustomContainer): SizeInt;
var
  HeadPos, c: SizeInt;
  v: T;
begin
  if aContainer <> Self then
    begin
      Result := aContainer.Count;
      if Result > 0 then
        begin
          DoEnsureCapacity(ElemCount + Result);
          c := System.Length(FItems);
          HeadPos := Head;
          for v in aContainer do
            begin
              Dec(HeadPos);
              if HeadPos < 0 then
                HeadPos += c;
              FItems[HeadPos] := v;
            end;
          FCount += Result;
          FHead := HeadPos;
        end;
    end
  else
    Result := AddArray2Head(aContainer.ToArray);
end;

function TGDeque.AddEnum2Head(e: IEnumerable): SizeInt;
var
  v: T;
begin
  Result := ElemCount;
  for v in e do
    begin
      ItemAdding;
      Dec(FHead);
      if Head < 0 then
        FHead += System.Length(FItems);
      Inc(FCount);
      FItems[Head] := v;
    end;
  Result := ElemCount - Result;
end;

function TGDeque.InternalIndex(aIndex: SizeInt): SizeInt;
begin
  Result := aIndex + Head;
  if Result >= System.Length(FItems) then
    Result -= System.Length(FItems);
end;

function TGDeque.FastGetItem(aIndex: SizeInt): T;
begin
  Result := FItems[InternalIndex(aIndex)];
end;

procedure TGDeque.FastSetItem(aIndex: SizeInt; aValue: T);
begin
  FItems[InternalIndex(aIndex)] := aValue;
end;

procedure TGDeque.FastSwap(L, R: SizeInt);
var
  c: SizeInt;
  v: TFake;
begin
  c := System.Length(FItems);
  L += Head;
  R += Head;
  if L >= c then
    L -= c;
  if R >= c then
    R -= c;
  v := TFake(FItems[L]);
  TFake(FItems[L]) := TFake(FItems[R]);
  TFake(FItems[R]) := v;
end;

function TGDeque.GetItem(aIndex: SizeInt): T;
begin
  CheckIndexRange(aIndex);
  Result := FastGetItem(aIndex);
end;

procedure TGDeque.SetItem(aIndex: SizeInt; const aValue: T);
begin
  //CheckInIteration;  ???
  CheckIndexRange(aIndex);
  FastSetItem(aIndex, aValue);
end;

procedure TGDeque.ShiftHeadRight(aToIndex: SizeInt);
var
  I, Curr, Prev, c: SizeInt;
begin
  c := System.Length(FItems);
  Curr := InternalIndex(aToIndex);
  for I := aToIndex downto 1 do
    begin
      Prev := Pred(Curr);
      if Prev < 0 then
        Prev += c;
      TFake(FItems[Curr]) := TFake(FItems[Prev]);
      Curr := Prev;
    end;
  TFake(FItems[Curr]) := Default(TFake); //clear old head slot
  Inc(FHead);
  if FHead >= c then
    FHead -= c;
end;

procedure TGDeque.ShiftHeadLeft(aFromIndex: SizeInt);
var
  I, Curr, Next, c: SizeInt;
begin
  c := System.Length(FItems);
  Dec(FHead);
  if FHead < 0 then
    FHead += c;
  Curr := Head;
  for I := 0 to Pred(aFromIndex) do
    begin
      Next := Succ(Curr);
      if Next >= c then
        Next -= c;
      TFake(FItems[Curr]) := TFake(FItems[Next]);
      Curr := Next;
    end;
  TFake(FItems[Curr]) := Default(TFake); //clear last slot
end;

procedure TGDeque.ShiftTailRight(aFromIndex: SizeInt);
var
  I, Curr, Prev, c: SizeInt;
begin
  c := System.Length(FItems);
  Curr := InternalIndex(Pred(ElemCount));  //here FCount already increased
  for I := Pred(ElemCount) downto Succ(aFromIndex) do
    begin
      Prev := Pred(Curr);
      if Prev < 0 then
        Prev += c;
      TFake(FItems[Curr]) := TFake(FItems[Prev]);
      Curr := Prev;
    end;
  TFake(FItems[Curr]) := Default(TFake); //clear old aFromIndex slot
end;

procedure TGDeque.ShiftTailLeft(aToIndex: SizeInt);
var
  I, Curr, Next, c: SizeInt;
begin
  c := System.Length(FItems);
  Curr := InternalIndex(aToIndex);
  for I := aToIndex to Pred(ElemCount) do //here FCount already decreased
    begin
      Next := Succ(Curr);
      if Next >= c then
        Next -= c;
      TFake(FItems[Curr]) := TFake(FItems[Next]);
      Curr := Next;
    end;
  TFake(FItems[Curr]) := Default(TFake); //clear last slot
end;

procedure TGDeque.InsertItem(aIndex: SizeInt; aValue: T);
begin
  if aIndex = 0 then
    DoPushFirst(aValue)
  else
    if aIndex = ElemCount then
      Append(aValue)
    else
      begin
        ItemAdding;
        Inc(FCount); ////
        if ElemCount <= SIZE_CUTOFF then
          ShiftTailRight(aIndex)
        else
          if aIndex >= Pred(ElemCount shr 1) then
            ShiftTailRight(aIndex)
          else
            ShiftHeadLeft(aIndex);
        FItems[InternalIndex(aIndex)] := aValue;
      end;
end;

function TGDeque.ExtractItem(aIndex: SizeInt): T;
var
  I: SizeInt;
begin
  if aIndex = 0 then
    Result := ExtractHead
  else
    if aIndex = Pred(ElemCount) then
      Result := ExtractTail
    else
      begin
        I := InternalIndex(aIndex);
        Result := FItems[I];
        FItems[I] := Default(T);
        Dec(FCount);  ///////
        if ElemCount <= SIZE_CUTOFF then
            ShiftTailLeft(aIndex)
        else
          if aIndex >= Pred(ElemCount shr 1) then
            ShiftTailLeft(aIndex)
          else
            ShiftHeadRight(aIndex);
      end;
end;

function TGDeque.DeleteItem(aIndex: SizeInt): T;
begin
  Result := ExtractItem(aIndex);
end;

procedure TGDeque.PushFirst(constref aValue: T);
begin
  CheckInIteration;
  DoPushFirst(aValue);
end;

function TGDeque.PushAllFirst(constref a: array of T): SizeInt;
begin
  CheckInIteration;
  Result := AddArray2Head(a);
end;

function TGDeque.PushAllFirst(e: IEnumerable): SizeInt;
var
  o: TObject;
begin
  if not InIteration then
    begin
      o := e._GetRef;
      if o is TCustomContainer then
        Result := AddContainer2Head(TCustomContainer(o))
      else
        Result := AddEnum2Head(e);
    end
  else
    begin
      Result := 0;
      e.Any;
      UpdateLockError;
    end;
end;

procedure TGDeque.PushLast(constref aValue: T);
begin
  CheckInIteration;
  Append(aValue);
end;

function TGDeque.PushAllLast(constref a: array of T): SizeInt;
begin
  CheckInIteration;
  Result := AppendArray(a);
end;

function TGDeque.PushAllLast(e: IEnumerable): SizeInt;
begin
  if not InIteration then
    Result := AppendEnumerable(e)
  else
    begin
      Result := 0;
      e.Any;
      UpdateLockError;
    end;
end;

function TGDeque.PopFirst: T;
begin
  CheckInIteration;
  CheckEmpty;
  Result := ExtractHead;
end;

function TGDeque.TryPopFirst(out aValue: T): Boolean;
begin
  Result := not InIteration and (ElemCount > 0);
  if Result then
    aValue := ExtractHead;
end;

function TGDeque.PopLast: T;
begin
  CheckInIteration;
  CheckEmpty;
  Result := ExtractTail;
end;

function TGDeque.TryPopLast(out aValue: T): Boolean;
begin
  Result := not InIteration and (ElemCount > 0);
  if Result then
    aValue := ExtractTail;
end;

function TGDeque.PeekFirst: T;
begin
  CheckEmpty;
  Result := Items[Head];
end;

function TGDeque.TryPeekFirst(out aValue: T): Boolean;
begin
  Result := ElemCount > 0;
  if Result then
    aValue := FItems[Head];
end;

function TGDeque.PeekLast: T;
begin
  CheckEmpty;
  Result := PeekTail;
end;

function TGDeque.TryPeekLast(out aValue: T): Boolean;
begin
  Result := ElemCount > 0;
  if Result then
    aValue := PeekTail;
end;

procedure TGDeque.Insert(aIndex: SizeInt; constref aValue: T);
begin
  CheckInIteration;
  CheckInsertIndexRange(aIndex);
  InsertItem(aIndex, aValue);
end;

function TGDeque.TryInsert(aIndex: SizeInt; constref aValue: T): Boolean;
begin
  Result := not InIteration and IndexInInsertRange(aIndex);
  if Result then
    InsertItem(aIndex, aValue);
end;

function TGDeque.Extract(aIndex: SizeInt): T;
begin
  CheckInIteration;
  CheckIndexRange(aIndex);
  Result := ExtractItem(aIndex);
end;

function TGDeque.TryExtract(aIndex: SizeInt; out aValue: T): Boolean;
begin
  Result := not InIteration and IndexInRange(aIndex);
  if Result then
    aValue := ExtractItem(aIndex);
end;

procedure TGDeque.Delete(aIndex: SizeInt);
begin
  CheckInIteration;
  CheckIndexRange(aIndex);
  DeleteItem(aIndex);
end;

function TGDeque.TryDelete(aIndex: SizeInt): Boolean;
begin
  Result := not InIteration and IndexInRange(aIndex);
  if Result then
    DeleteItem(aIndex);
end;

{ TGObjectDeque }

procedure TGObjectDeque.DoClear;
var
  I, CurrIdx, c: SizeInt;
begin
  if OwnsObjects and (ElemCount > 0) then
    begin
      CurrIdx := FHead;
      c := Capacity;
      for I := 1 to ElemCount do
        begin
          FItems[CurrIdx].Free;
          Inc(CurrIdx);
          if CurrIdx = c then
            CurrIdx := 0;
        end;
    end;
  inherited;
end;

procedure TGObjectDeque.SetItem(aIndex: SizeInt; const aValue: T);
var
  v: T;
begin
  //CheckInIteration;
  CheckIndexRange(aIndex);
  if OwnsObjects then
    begin
      v := FastGetItem(aIndex);
      if not TObject.Equal(v, aValue) then
        v.Free;
    end;
  FastSetItem(aIndex, aValue);
end;

function TGObjectDeque.DeleteItem(aIndex: SizeInt): T;
begin
  Result := inherited DeleteItem(aIndex);
  if OwnsObjects then
    Result.Free;
end;

constructor TGObjectDeque.Create(aOwnsObjects: Boolean);
begin
  inherited Create;
  FOwnsObjects := aOwnsObjects;
end;

constructor TGObjectDeque.Create(aCapacity: SizeInt; aOwnsObjects: Boolean);
begin
  inherited Create(aCapacity);
  FOwnsObjects := aOwnsObjects;
end;

constructor TGObjectDeque.Create(constref A: array of T; aOwnsObjects: Boolean = True);
begin
  inherited Create(A);
  FOwnsObjects := aOwnsObjects;
end;

constructor TGObjectDeque.Create(e: IEnumerable; aOwnsObjects: Boolean = True);
begin
  inherited Create(e);
  FOwnsObjects := aOwnsObjects;
end;

procedure TGThreadDeque.DoLock;
begin
  System.EnterCriticalSection(FLock);
end;

constructor TGThreadDeque.Create(aDeque: IDeque);
begin
  System.InitCriticalSection(FLock);
  FDeque := aDeque;
end;

destructor TGThreadDeque.Destroy;
begin
  DoLock;
  try
    FDeque._GetRef.Free;
    inherited;
  finally
    UnLock;
    System.DoneCriticalSection(FLock);
  end;
end;

procedure TGThreadDeque.Clear;
begin
  DoLock;
  try
    FDeque.Clear;
  finally
    UnLock;
  end;
end;

procedure TGThreadDeque.PushFirst(constref aValue: T);
begin
  DoLock;
  try
    FDeque.PushFirst(aValue);
  finally
    UnLock;
  end;
end;

procedure TGThreadDeque.PushLast(constref aValue: T);
begin
  DoLock;
  try
    FDeque.PushLast(aValue);
  finally
    UnLock;
  end;
end;

function TGThreadDeque.TryPopFirst(out aValue: T): Boolean;
begin
  DoLock;
  try
    Result := FDeque.TryPopFirst(aValue);
  finally
    UnLock;
  end;
end;

function TGThreadDeque.TryPopLast(out aValue: T): Boolean;
begin
  DoLock;
  try
    Result := FDeque.TryPopLast(aValue);
  finally
    UnLock;
  end;
end;

function TGThreadDeque.TryPeekFirst(out aValue: T): Boolean;
begin
  DoLock;
  try
    Result := FDeque.TryPeekFirst(aValue);
  finally
    UnLock;
  end;
end;

function TGThreadDeque.TryPeekLast(out aValue: T): Boolean;
begin
  DoLock;
  try
    Result := FDeque.TryPeekLast(aValue);
  finally
    UnLock;
  end;
end;

function TGThreadDeque.Lock: IDeque;
begin
  Result := FDeque;
  DoLock;
end;

procedure TGThreadDeque.Unlock;
begin
  System.LeaveCriticalSection(FLock);
end;

{ TGLiteDeque }

function TGLiteDeque.GetCapacity: SizeInt;
begin
  Result := FBuffer.Capacity;
end;

function TGLiteDeque.InternalIndex(aIndex: SizeInt): SizeInt;
begin
  Result := FBuffer.InternalIndex(aIndex);
end;

function TGLiteDeque.FastGetItem(aIndex: SizeInt): T;
begin
  Result := FBuffer.FastGetItem(aIndex);
end;

procedure TGLiteDeque.FastSetItem(aIndex: SizeInt; constref aValue: T);
begin
  FBuffer.FastSetItem(aIndex, aValue);
end;

procedure TGLiteDeque.FastSwap(L, R: SizeInt);
var
  c: SizeInt;
  v: TFake;
begin
  c := FBuffer.Capacity;
  L += FBuffer.Head;
  R += FBuffer.Head;
  if L >= c then
    L -= c;
  if R >= c then
    R -= c;
  v := TFake(FBuffer.FItems[L]);
  TFake(FBuffer.FItems[L]) := TFake(FBuffer.FItems[R]);
  TFake(FBuffer.FItems[R]) := v;
end;

function TGLiteDeque.GetItem(aIndex: SizeInt): T;
begin
  if SizeUInt(aIndex) < SizeUInt(FBuffer.Count) then
    Result := FastGetItem(aIndex)
  else
    raise ELGListError.CreateFmt(SEIndexOutOfBoundsFmt, [aIndex]);
end;

function TGLiteDeque.GetMutable(aIndex: SizeInt): PItem;
begin
  if SizeUInt(aIndex) < SizeUInt(FBuffer.Count) then
    Result := @FBuffer.FItems[FBuffer.InternalIndex(aIndex)]
  else
    raise ELGListError.CreateFmt(SEIndexOutOfBoundsFmt, [aIndex]);
end;

procedure TGLiteDeque.SetItem(aIndex: SizeInt; const aValue: T);
begin
  if SizeUInt(aIndex) < SizeUInt(FBuffer.Count) then
    FastSetItem(aIndex, aValue)
  else
    raise ELGListError.CreateFmt(SEIndexOutOfBoundsFmt, [aIndex]);
end;

procedure TGLiteDeque.ShiftHeadRight(aToIndex: SizeInt);
var
  I, Curr, Prev, c: SizeInt;
begin
  c := FBuffer.Capacity;
  Curr := InternalIndex(aToIndex);
  for I := aToIndex downto 1 do
    begin
      Prev := Pred(Curr);
      if Prev < 0 then
        Prev += c;
      TFake(FBuffer.FItems[Curr]) := TFake(FBuffer.FItems[Prev]);
      Curr := Prev;
    end;
  TFake(FBuffer.FItems[Curr]) := Default(TFake); //clear old head slot
  Inc(FBuffer.FHead);
  if FBuffer.Head >= c then
    FBuffer.FHead -= c;
end;

procedure TGLiteDeque.ShiftHeadLeft(aFromIndex: SizeInt);
var
  I, Curr, Next, c: SizeInt;
begin
  c := FBuffer.Capacity;
  Dec(FBuffer.FHead);
  if FBuffer.Head < 0 then
    FBuffer.FHead += c;
  Curr := FBuffer.Head;
  for I := 0 to Pred(aFromIndex) do
    begin
      Next := Succ(Curr);
      if Next >= c then
        Next -= c;
      TFake(FBuffer.FItems[Curr]) := TFake(FBuffer.FItems[Next]);
      Curr := Next;
    end;
  TFake(FBuffer.FItems[Curr]) := Default(TFake); //clear last slot
end;

procedure TGLiteDeque.ShiftTailRight(aFromIndex: SizeInt);
var
  I, Curr, Prev, c: SizeInt;
begin
  c := FBuffer.Capacity;
  Curr := InternalIndex(Pred(Count));  //here FBuffer.FCount already increased
  for I := Pred(Count) downto Succ(aFromIndex) do
    begin
      Prev := Pred(Curr);
      if Prev < 0 then
        Prev += c;
      TFake(FBuffer.FItems[Curr]) := TFake(FBuffer.FItems[Prev]);
      Curr := Prev;
    end;
  TFake(FBuffer.FItems[Curr]) := Default(TFake); //clear old aFromIndex slot
end;

procedure TGLiteDeque.ShiftTailLeft(aToIndex: SizeInt);
var
  I, Curr, Next, c: SizeInt;
begin
  c := FBuffer.Capacity;
  Curr := InternalIndex(aToIndex);
  for I := aToIndex to Pred(Count) do //here FBuffer.FCount already decreased
    begin
      Next := Succ(Curr);
      if Next >= c then
        Next -= c;
      TFake(FBuffer.FItems[Curr]) := TFake(FBuffer.FItems[Next]);
      Curr := Next;
    end;
  TFake(FBuffer.FItems[Curr]) := Default(TFake); //clear last slot
end;

procedure TGLiteDeque.InsertItem(aIndex: SizeInt; aValue: T);
begin
  if aIndex = 0 then
    PushFirst(aValue)
  else
    if aIndex = Count then
      FBuffer.PushLast(aValue)
    else
      begin
        FBuffer.ItemAdding;
        Inc(FBuffer.FCount); ////
        if Count <= SIZE_CUTOFF then
          ShiftTailRight(aIndex)
        else
          if aIndex >= Pred(Count shr 1) then
            ShiftTailRight(aIndex)
          else
            ShiftHeadLeft(aIndex);
        FBuffer.FItems[InternalIndex(aIndex)] := aValue;
      end;
end;

function TGLiteDeque.DeleteItem(aIndex: SizeInt): T;
var
  I: SizeInt;
begin
  if aIndex = 0 then
    Result := FBuffer.PopHead
  else
    if aIndex = Pred(Count) then
      Result := FBuffer.PopTail
    else
      begin
        I := InternalIndex(aIndex);
        Result := FBuffer.FItems[I];
        FBuffer.FItems[I] := Default(T);
        Dec(FBuffer.FCount);  ///////
        if Count <= SIZE_CUTOFF then
            ShiftTailLeft(aIndex)
        else
          if aIndex >= Pred(Count shr 1) then
            ShiftTailLeft(aIndex)
          else
            ShiftHeadRight(aIndex);
      end;
end;

function TGLiteDeque.GetEnumerator: TEnumerator;
begin
  Result := FBuffer.GetEnumerator;
end;

function TGLiteDeque.Mutables: TMutables;
begin
  Result := FBuffer.Mutables;
end;

function TGLiteDeque.Reverse: TReverse;
begin
  Result := FBuffer.Reverse;
end;

function TGLiteDeque.ToArray: TArray;
begin
  Result := FBuffer.ToArray;
end;

procedure TGLiteDeque.Clear;
begin
  FBuffer.Clear;
end;

function TGLiteDeque.IsEmpty: Boolean;
begin
  Result := FBuffer.Count = 0;
end;

function TGLiteDeque.NonEmpty: Boolean;
begin
  Result := FBuffer.Count <> 0;
end;

procedure TGLiteDeque.EnsureCapacity(aValue: SizeInt);
begin
  FBuffer.EnsureCapacity(aValue);
end;

procedure TGLiteDeque.TrimToFit;
begin
  FBuffer.TrimToFit;
end;

procedure TGLiteDeque.PushFirst(constref aValue: T);
begin
  FBuffer.PushFirst(aValue);
end;

procedure TGLiteDeque.PushLast(constref aValue: T);
begin
  FBuffer.PushLast(aValue);
end;

function TGLiteDeque.PopFirst: T;
begin
  Result := FBuffer.PopFirst;
end;

function TGLiteDeque.TryPopFirst(out aValue: T): Boolean;
begin
  Result := FBuffer.TryPopFirst(aValue);
end;

function TGLiteDeque.PopLast: T;
begin
  Result := FBuffer.PopLast;
end;

function TGLiteDeque.TryPopLast(out aValue: T): Boolean;
begin
  Result := FBuffer.TryPopLast(aValue);
end;

function TGLiteDeque.PeekFirst: T;
begin
  Result := FBuffer.PeekFirst;
end;

function TGLiteDeque.TryPeekFirst(out aValue: T): Boolean;
begin
  Result := FBuffer.TryPeekFirst(aValue);
end;

function TGLiteDeque.PeekLast: T;
begin
  Result := FBuffer.PeekLast;
end;

function TGLiteDeque.TryPeekLast(out aValue: T): Boolean;
begin
  Result := FBuffer.TryPeekLast(aValue);
end;

procedure TGLiteDeque.Insert(aIndex: SizeInt; constref aValue: T);
begin
  if SizeUInt(aIndex) <= SizeUInt(FBuffer.Count) then
    InsertItem(aIndex, aValue)
  else
    raise ELGListError.CreateFmt(SEIndexOutOfBoundsFmt, [aIndex]);
end;

function TGLiteDeque.TryInsert(aIndex: SizeInt; constref aValue: T): Boolean;
begin
  Result := SizeUInt(aIndex) <= SizeUInt(FBuffer.Count);
  if Result then
    InsertItem(aIndex, aValue);
end;

function TGLiteDeque.Delete(aIndex: SizeInt): T;
begin
  if SizeUInt(aIndex) < SizeUInt(FBuffer.Count) then
    Result := DeleteItem(aIndex)
  else
    raise ELGListError.CreateFmt(SEIndexOutOfBoundsFmt, [aIndex]);
end;

function TGLiteDeque.TryDelete(aIndex: SizeInt; out aValue: T): Boolean;
begin
  Result := SizeUInt(aIndex) < SizeUInt(FBuffer.Count);
  if Result then
    aValue := DeleteItem(aIndex);
end;

{ TGLiteThreadDeque }

procedure TGLiteThreadDeque.DoLock;
begin
  System.EnterCriticalSection(FLock);
end;

constructor TGLiteThreadDeque.Create;
begin
  System.InitCriticalSection(FLock);
end;

destructor TGLiteThreadDeque.Destroy;
begin
  DoLock;
  try
    Finalize(FDeque);
    inherited;
  finally
    UnLock;
    System.DoneCriticalSection(FLock);
  end;
end;

procedure TGLiteThreadDeque.Clear;
begin
  DoLock;
  try
    FDeque.Clear;
  finally
    UnLock;
  end;
end;

procedure TGLiteThreadDeque.PushFirst(constref aValue: T);
begin
  DoLock;
  try
    FDeque.PushFirst(aValue);
  finally
    UnLock;
  end;
end;

procedure TGLiteThreadDeque.PushLast(constref aValue: T);
begin
  DoLock;
  try
    FDeque.PushLast(aValue);
  finally
    UnLock;
  end;
end;

function TGLiteThreadDeque.TryPopFirst(out aValue: T): Boolean;
begin
  DoLock;
  try
    Result := FDeque.TryPopFirst(aValue);
  finally
    UnLock;
  end;
end;

function TGLiteThreadDeque.TryPopLast(out aValue: T): Boolean;
begin
  DoLock;
  try
    Result := FDeque.TryPopLast(aValue);
  finally
    UnLock;
  end;
end;

function TGLiteThreadDeque.TryPeekFirst(out aValue: T): Boolean;
begin
  DoLock;
  try
    Result := FDeque.TryPeekFirst(aValue);
  finally
    UnLock;
  end;
end;

function TGLiteThreadDeque.TryPeekLast(out aValue: T): Boolean;
begin
  DoLock;
  try
    Result := FDeque.TryPeekLast(aValue);
  finally
    UnLock;
  end;
end;

function TGLiteThreadDeque.Lock: PDeque;
begin
  Result := @FDeque;
  DoLock;
end;

procedure TGLiteThreadDeque.Unlock;
begin
  System.LeaveCriticalSection(FLock);
end;

{ TGLiteObjectDeque }

procedure TGLiteObjectDeque.CheckFreeItems;
var
  v: T;
begin
  if OwnsObjects then
    for v in FDeque do
      v.Free;
end;

function TGLiteObjectDeque.GetCapacity: SizeInt;
begin
  Result := FDeque.Capacity;
end;

function TGLiteObjectDeque.GetCount: SizeInt;
begin
  Result := FDeque.Count;
end;

function TGLiteObjectDeque.GetItem(aIndex: SizeInt): T;
begin
  Result := FDeque.GetItem(aIndex)
end;

procedure TGLiteObjectDeque.SetItem(aIndex: SizeInt; const aValue: T);
var
  v: T;
begin
  if OwnsObjects then
    begin
      v := FDeque.GetItem(aIndex);
      if not TObject.Equal(v, aValue) then
        v.Free;
      FDeque.FastSetItem(aIndex, aValue);
    end
  else
    FDeque.SetItem(aIndex, aValue);
end;

class operator TGLiteObjectDeque.Initialize(var d: TGLiteObjectDeque);
begin
  d.OwnsObjects := True;
end;

class operator TGLiteObjectDeque.Copy(constref aSrc: TGLiteObjectDeque; var aDst: TGLiteObjectDeque);
begin
  if @aDst = @aSrc then
    exit;
  aDst.CheckFreeItems;
  aDst.FDeque := aSrc.FDeque;
  aDst.FOwnsObjects := aSrc.OwnsObjects;
end;

function TGLiteObjectDeque.InnerDeque: PDeque;
begin
  Result := @FDeque;
end;

function TGLiteObjectDeque.GetEnumerator: TEnumerator;
begin
  Result := FDeque.GetEnumerator;
end;

function TGLiteObjectDeque.Reverse: TReverse;
begin
  Result := FDeque.Reverse;
end;

function TGLiteObjectDeque.ToArray: TArray;
begin
  Result := FDeque.ToArray;
end;

procedure TGLiteObjectDeque.Clear;
begin
  CheckFreeItems;
  FDeque.Clear;
end;

function TGLiteObjectDeque.IsEmpty: Boolean;
begin
  Result := FDeque.IsEmpty;
end;

function TGLiteObjectDeque.NonEmpty: Boolean;
begin
  Result := FDeque.NonEmpty;
end;

procedure TGLiteObjectDeque.EnsureCapacity(aValue: SizeInt);
begin
  FDeque.EnsureCapacity(aValue);
end;

procedure TGLiteObjectDeque.TrimToFit;
begin
  FDeque.TrimToFit;
end;

procedure TGLiteObjectDeque.PushFirst(constref aValue: T);
begin
  FDeque.PushFirst(aValue);
end;

procedure TGLiteObjectDeque.PushLast(constref aValue: T);
begin
  FDeque.PushLast(aValue);
end;

function TGLiteObjectDeque.PopFirst: T;
begin
  Result := FDeque.PopFirst;
end;

function TGLiteObjectDeque.TryPopFirst(out aValue: T): Boolean;
begin
  Result := FDeque.TryPopFirst(aValue);
end;

function TGLiteObjectDeque.PopLast: T;
begin
  Result := FDeque.PopLast;
end;

function TGLiteObjectDeque.TryPopLast(out aValue: T): Boolean;
begin
  Result := FDeque.TryPopLast(aValue);
end;

function TGLiteObjectDeque.PeekFirst: T;
begin
  Result := FDeque.PeekFirst;
end;

function TGLiteObjectDeque.TryPeekFirst(out aValue: T): Boolean;
begin
  Result := FDeque.TryPeekFirst(aValue);
end;

function TGLiteObjectDeque.PeekLast: T;
begin
  Result := FDeque.PeekLast;
end;

function TGLiteObjectDeque.TryPeekLast(out aValue: T): Boolean;
begin
  Result := FDeque.TryPeekLast(aValue);
end;

procedure TGLiteObjectDeque.Insert(aIndex: SizeInt; constref aValue: T);
begin
  FDeque.Insert(aIndex, aValue);
end;

function TGLiteObjectDeque.TryInsert(aIndex: SizeInt; constref aValue: T): Boolean;
begin
  Result := FDeque.TryInsert(aIndex, aValue);
end;

function TGLiteObjectDeque.Extract(aIndex: SizeInt): T;
begin
  Result := FDeque.Delete(aIndex);
end;

function TGLiteObjectDeque.TryExtract(aIndex: SizeInt; out aValue: T): Boolean;
begin
  Result := FDeque.TryDelete(aIndex, aValue);
end;

procedure TGLiteObjectDeque.Delete(aIndex: SizeInt);
var
  v: T;
begin
  v := FDeque.Delete(aIndex);
  if OwnsObjects then
    v.Free;
end;

function TGLiteObjectDeque.TryDelete(aIndex: SizeInt): Boolean;
var
  v: T;
begin
  Result := FDeque.TryDelete(aIndex, v);
  if Result and OwnsObjects then
    v.Free;
end;

{ TGLiteThreadObjectDeque }

procedure TGLiteThreadObjectDeque.DoLock;
begin
  System.EnterCriticalSection(FLock);
end;

constructor TGLiteThreadObjectDeque.Create;
begin
  System.InitCriticalSection(FLock);
end;

destructor TGLiteThreadObjectDeque.Destroy;
begin
  DoLock;
  try
    Finalize(FDeque);
    inherited;
  finally
    UnLock;
    System.DoneCriticalSection(FLock);
  end;
end;

procedure TGLiteThreadObjectDeque.Clear;
begin
  DoLock;
  try
    FDeque.Clear;
  finally
    UnLock;
  end;
end;

procedure TGLiteThreadObjectDeque.PushFirst(constref aValue: T);
begin
  DoLock;
  try
    FDeque.PushFirst(aValue);
  finally
    UnLock;
  end;
end;

procedure TGLiteThreadObjectDeque.PushLast(constref aValue: T);
begin
  DoLock;
  try
    FDeque.PushLast(aValue);
  finally
    UnLock;
  end;
end;

function TGLiteThreadObjectDeque.TryPopFirst(out aValue: T): Boolean;
begin
  DoLock;
  try
    Result := FDeque.TryPopFirst(aValue);
  finally
    UnLock;
  end;
end;

function TGLiteThreadObjectDeque.TryPopLast(out aValue: T): Boolean;
begin
  DoLock;
  try
    Result := FDeque.TryPopLast(aValue);
  finally
    UnLock;
  end;
end;

function TGLiteThreadObjectDeque.TryPeekFirst(out aValue: T): Boolean;
begin
  DoLock;
  try
    Result := FDeque.TryPeekFirst(aValue);
  finally
    UnLock;
  end;
end;

function TGLiteThreadObjectDeque.TryPeekLast(out aValue: T): Boolean;
begin
  DoLock;
  try
    Result := FDeque.TryPeekLast(aValue);
  finally
    UnLock;
  end;
end;

function TGLiteThreadObjectDeque.Lock: PDeque;
begin
  Result := @FDeque;
  DoLock;
end;

procedure TGLiteThreadObjectDeque.Unlock;
begin
  System.LeaveCriticalSection(FLock);
end;

{ TGDequeHelpUtil }

class procedure TGDequeHelpUtil.DoReverse(d: TDeque; L, R: SizeInt);
begin
  while L < R do
    begin
      d.FastSwap(L, R);
      Inc(L);
      Dec(R);
    end;
end;

class procedure TGDequeHelpUtil.DoReverse(var d: TLiteDeque; L, R: SizeInt);
begin
  while L < R do
    begin
      d.FastSwap(L, R);
      Inc(L);
      Dec(R);
    end;
end;

class procedure TGDequeHelpUtil.Reverse(d: TDeque);
var
  R: SizeInt;
begin
  R := Pred(d.ElemCount);
  if R > 0 then
    begin
      d.CheckInIteration;
      DoReverse(d, 0, R);
    end;
end;

class procedure TGDequeHelpUtil.Reverse(var d: TLiteDeque);
begin
   DoReverse(d, 0, Pred(d.Count));
end;

class procedure TGDequeHelpUtil.RandomShuffle(d: TDeque);
var
  I, J: SizeInt;
begin
  I := Pred(d.ElemCount);
  if I > 0 then
    begin
      d.CheckInIteration;
      for I := I downto 1 do
        begin
          J := Random(I);
          d.FastSwap(I, J);
        end;
    end;
end;

class procedure TGDequeHelpUtil.RandomShuffle(var d: TLiteDeque);
var
  I, J: SizeInt;
begin
  for I := Pred(d.Count) downto 1 do
    begin
      J := Random(I);
      d.FastSwap(I, J);
    end;
end;

class function TGDequeHelpUtil.SequentSearch(d: TDeque; constref aValue: T; c: TEqualityCompare): SizeInt;
begin
  for Result := 0 to Pred(d.ElemCount) do
    if c(aValue, d.FastGetItem(Result)) then
      exit;
  Result := -1;
end;

class function TGDequeHelpUtil.SequentSearch(constref d: TLiteDeque; constref aValue: T;
  c: TEqualityCompare): SizeInt;
begin
  for Result := 0 to Pred(d.Count) do
    if c(aValue, d.FastGetItem(Result)) then
      exit;
  Result := -1;
end;

class function TGDequeHelpUtil.Same(d1, d2: TDeque; c: TEqualityCompare): Boolean;
var
  I, Count: SizeInt;
begin
  Count := d1.ElemCount;
  if d2.ElemCount <> Count then
    exit(False);
  for I := 0 to Pred(Count) do
    if not c(d1.FastGetItem(I), d2.FastGetItem(I)) then
      exit(False);
  Result := True;
end;

class function TGDequeHelpUtil.Same(constref d1, d2: TLiteDeque; c: TEqualityCompare): Boolean;
var
  I, Count: SizeInt;
begin
  Count := d1.Count;
  if d2.Count <> Count then
    exit(False);
  for I := 0 to Pred(Count) do
    if not c(d1.FastGetItem(I), d2.FastGetItem(I)) then
      exit(False);
  Result := True;
end;

{ TGBaseDequeHelper }

class function TGBaseDequeHelper.DoBinSearch(d: TDeque; constref aValue: T): SizeInt;
var
  R, L, M, c: SizeInt;
begin
  //here R must be >= 0;
  R := Pred(d.ElemCount);
  Result := -1;
  L := 0;
  c := TCmpRel.Compare(d.FastGetItem(R), d.FastGetItem(0));
  if c > 0 then  //ascending
    begin
      while L < R do
        begin
          M := L + (R - L) shr 1;
          c := TCmpRel.Compare(d.FastGetItem(M), aValue);
          if c < 0 then
            L := Succ(M)
          else
            begin
              if c = 0 then
                exit(M);
              R := M;
            end;
        end;
      //here L >= R
      if TCmpRel.Compare(d.FastGetItem(R), aValue) = 0 then
        Result := R;
    end
  else
    if c < 0 then  //descending
      begin
        while L < R do
          begin
            M := L + ((R - L) shr 1);
            c := TCmpRel.Compare(d.FastGetItem(M), aValue);
            if c > 0 then
              L := Succ(M)
            else
              begin
                if c = 0 then
                  exit(M);
                R := M;
              end;
          end;
        //here L >= R
        if TCmpRel.Compare(d.FastGetItem(R), aValue) = 0 then
          Result := R;
      end
    else           //constant
      if TCmpRel.Compare(d.FastGetItem(L), aValue) = 0 then
        exit(L);
end;

class function TGBaseDequeHelper.DoBinSearch(constref d: TLiteDeque; constref aValue: T): SizeInt;
var
  R, L, M, c: SizeInt;
begin
  //here R must be >= 0;
  R := Pred(d.Count);
  Result := -1;
  L := 0;
  c := TCmpRel.Compare(d.FastGetItem(R), d.FastGetItem(0));
  if c > 0 then  //ascending
    begin
      while L < R do
        begin
          M := L + (R - L) shr 1;
          c := TCmpRel.Compare(d.FastGetItem(M), aValue);
          if c < 0 then
            L := Succ(M)
          else
            begin
              if c = 0 then
                exit(M);
              R := M;
            end;
        end;
      //here L >= R
      if TCmpRel.Compare(d.FastGetItem(R), aValue) = 0 then
        Result := R;
    end
  else
    if c < 0 then  //descending
      begin
        while L < R do
          begin
            M := L + ((R - L) shr 1);
            c := TCmpRel.Compare(d.FastGetItem(M), aValue);
            if c > 0 then
              L := Succ(M)
            else
              begin
                if c = 0 then
                  exit(M);
                R := M;
              end;
          end;
        //here L >= R
        if TCmpRel.Compare(d.FastGetItem(R), aValue) = 0 then
          Result := R;
      end
    else           //constant
      if TCmpRel.Compare(d.FastGetItem(L), aValue) = 0 then
        exit(L);
end;

class function TGBaseDequeHelper.CountRun2Asc(d: TDeque; R: SizeInt): SizeInt;
begin
  Result := 0;
  while (Result < R) and (TCmpRel.Compare(d.FastGetItem(Result), d.FastGetItem(Succ(Result))) = 0) do
    Inc(Result);
  if Result < R then
    begin
      Inc(Result);
      if TCmpRel.Compare(d.FastGetItem(Pred(Result)), d.FastGetItem(Result)) < 0 then   // ascending
        while (Result < R) and
              (TCmpRel.Compare(d.FastGetItem(Result), d.FastGetItem(Succ(Result))) <= 0) do
          Inc(Result)
      else                                                      // descending
        begin
          while (Result < R) and
                (TCmpRel.Compare(d.FastGetItem(Succ(Result)), d.FastGetItem(Result)) <= 0) do
            Inc(Result);
          DoReverse(d, 0, Result);
        end;
    end;
end;

class function TGBaseDequeHelper.CountRun2Asc(var d: TLiteDeque; R: SizeInt): SizeInt;
begin
  Result := 0;
  while (Result < R) and (TCmpRel.Compare(d.FastGetItem(Result), d.FastGetItem(Succ(Result))) = 0) do
    Inc(Result);
  if Result < R then
    begin
      Inc(Result);
      if TCmpRel.Compare(d.FastGetItem(Pred(Result)), d.FastGetItem(Result)) < 0 then   // ascending
        while (Result < R) and
              (TCmpRel.Compare(d.FastGetItem(Result), d.FastGetItem(Succ(Result))) <= 0) do
          Inc(Result)
      else                                                      // descending
        begin
          while (Result < R) and
                (TCmpRel.Compare(d.FastGetItem(Succ(Result)), d.FastGetItem(Result)) <= 0) do
            Inc(Result);
          DoReverse(d, 0, Result);
        end;
    end;
end;

class procedure TGBaseDequeHelper.InsertionSort(d: TDeque; L, R: SizeInt);
var
  I, J: SizeInt;
  v: TFake;
begin
  for I := L + 1 to R do
    begin
      v := TFake(d.FItems[d.InternalIndex(I)]);
      J := I - 1;
      while (J >= 0) and (TCmpRel.Compare(T(v), d.FItems[d.InternalIndex(J)]) < 0) do
        begin
          TFake(d.FItems[d.InternalIndex(J + 1)]) := TFake(d.FItems[d.InternalIndex(J)]);
          Dec(J);
        end;
      TFake(d.FItems[d.InternalIndex(J + 1)]) := v;
    end;
end;

class procedure TGBaseDequeHelper.InsertionSort(var d: TLiteDeque; L, R: SizeInt);
var
  I, J: SizeInt;
  v: TFake;
begin
  for I := L + 1 to R do
    begin
      v := TFake(d.FBuffer.FItems[d.InternalIndex(I)]);
      J := I - 1;
      while (J >= 0) and (TCmpRel.Compare(T(v), d.FBuffer.FItems[d.InternalIndex(J)]) < 0) do
        begin
          TFake(d.FBuffer.FItems[d.InternalIndex(J + 1)]) := TFake(d.FBuffer.FItems[d.InternalIndex(J)]);
          Dec(J);
        end;
      TFake(d.FBuffer.FItems[d.InternalIndex(J + 1)]) := v;
    end;
end;

class procedure TGBaseDequeHelper.DoHeapSort(d: TDeque; L, R: SizeInt);
var
  I, Curr, Next: SizeInt;
  v: TFake;
begin
  if R - L > THelper.HEAP_INSERT_CUTOFF then
    begin
      for I := Pred(Succ(R - L) shr 1) downto L do
        begin
          Curr := I;
          Next := Succ(I shl 1);
          v := TFake(d.FItems[d.InternalIndex(Curr)]);
          while Next <= R do
            begin
              if (Succ(Next) <= R) and (TCmpRel.Compare(
                  d.FItems[d.InternalIndex(Next)], d.FItems[d.InternalIndex(Succ(Next))]) < 0)then
                Inc(Next);
              if TCmpRel.Compare(T(v), d.FItems[d.InternalIndex(Next)]) >= 0 then
                break;
              TFake(d.FItems[d.InternalIndex(Curr)]) := TFake(d.FItems[d.InternalIndex(Next)]);
              Curr := Next;
              Next := Succ(Next shl 1);
            end;
          TFake(d.FItems[d.InternalIndex(Curr)]) := v;
        end;
      for I := R downto L + 1 do
        begin
          Curr := L;
          Next := Succ(L);
          v := TFake(d.FItems[d.InternalIndex(I)]);
          TFake(d.FItems[d.InternalIndex(I)]) := TFake(d.FItems[d.InternalIndex(L)]);
          while Next < I do
            begin
              if(Succ(Next) < I) and (TCmpRel.Compare(
                 d.FItems[d.InternalIndex(Next)], d.FItems[d.InternalIndex(Succ(Next))]) < 0) then
                Inc(Next);
              TFake(d.FItems[d.InternalIndex(Curr)]) := TFake(d.FItems[d.InternalIndex(Next)]);
              Curr := Next;
              Next := Succ(Next shl 1);
            end;
          Next := Pred(Curr) shr 1;
          while (Curr > 0) and (TCmpRel.Compare(T(v), d.FItems[d.InternalIndex(Next)]) > 0) do
            begin
              TFake(d.FItems[d.InternalIndex(Curr)]) := TFake(d.FItems[d.InternalIndex(Next)]);
              Curr := Next;
              Next := Pred(Next) shr 1;
            end;
          TFake(d.FItems[d.InternalIndex(Curr)]) := v;
        end;
    end
  else
    InsertionSort(d, L, R);
end;

class procedure TGBaseDequeHelper.DoHeapSort(var d: TLiteDeque; L, R: SizeInt);
var
  I, Curr, Next: SizeInt;
  v: TFake;
begin
  if R - L > THelper.HEAP_INSERT_CUTOFF then
    begin
      for I := Pred(Succ(R - L) shr 1) downto L do
        begin
          Curr := I;
          Next := Succ(I shl 1);
          v := TFake(d.FBuffer.FItems[d.InternalIndex(Curr)]);
          while Next <= R do
            begin
              if (Succ(Next) <= R) and (TCmpRel.Compare(
                  d.FBuffer.FItems[d.InternalIndex(Next)], d.FBuffer.FItems[d.InternalIndex(Succ(Next))]) < 0)then
                Inc(Next);
              if TCmpRel.Compare(T(v), d.FBuffer.FItems[d.InternalIndex(Next)]) >= 0 then
                break;
              TFake(d.FBuffer.FItems[d.InternalIndex(Curr)]) := TFake(d.FBuffer.FItems[d.InternalIndex(Next)]);
              Curr := Next;
              Next := Succ(Next shl 1);
            end;
          TFake(d.FBuffer.FItems[d.InternalIndex(Curr)]) := v;
        end;
      for I := R downto L + 1 do
        begin
          Curr := L;
          Next := Succ(L);
          v := TFake(d.FBuffer.FItems[d.InternalIndex(I)]);
          TFake(d.FBuffer.FItems[d.InternalIndex(I)]) := TFake(d.FBuffer.FItems[d.InternalIndex(L)]);
          while Next < I do
            begin
              if(Succ(Next) < I) and (TCmpRel.Compare(
                 d.FBuffer.FItems[d.InternalIndex(Next)], d.FBuffer.FItems[d.InternalIndex(Succ(Next))]) < 0) then
                Inc(Next);
              TFake(d.FBuffer.FItems[d.InternalIndex(Curr)]) := TFake(d.FBuffer.FItems[d.InternalIndex(Next)]);
              Curr := Next;
              Next := Succ(Next shl 1);
            end;
          Next := Pred(Curr) shr 1;
          while (Curr > 0) and (TCmpRel.Compare(T(v), d.FBuffer.FItems[d.InternalIndex(Next)]) > 0) do
            begin
              TFake(d.FBuffer.FItems[d.InternalIndex(Curr)]) := TFake(d.FBuffer.FItems[d.InternalIndex(Next)]);
              Curr := Next;
              Next := Pred(Next) shr 1;
            end;
          TFake(d.FBuffer.FItems[d.InternalIndex(Curr)]) := v;
        end;
    end
  else
    InsertionSort(d, L, R);
end;

class function TGBaseDequeHelper.QSplitR(d: TDeque; L, R: SizeInt): TSortSplit;
var
  Pivot: T;
begin
  Pivot := d.FItems[d.InternalIndex(Succ(L + Random(Pred(R - L))))];
  Dec(L);
  Inc(R);
  repeat
    repeat Inc(L) until TCmpRel.Compare(Pivot, d.FItems[d.InternalIndex(L)]) <= 0;
    repeat Dec(R) until TCmpRel.Compare(d.FItems[d.InternalIndex(R)], Pivot) <= 0;
    if L > R then break;
    d.FastSwap(L, R);
  until False;
  Result.Left := R;
  Result.Right := L;
end;

class function TGBaseDequeHelper.QSplitR(var d: TLiteDeque; L, R: SizeInt): TSortSplit;
var
  Pivot: T;
begin
  Pivot := d.FBuffer.FItems[d.InternalIndex(Succ(L + Random(Pred(R - L))))];
  Dec(L);
  Inc(R);
  repeat
    repeat Inc(L) until TCmpRel.Compare(Pivot, d.FBuffer.FItems[d.InternalIndex(L)]) <= 0;
    repeat Dec(R) until TCmpRel.Compare(d.FBuffer.FItems[d.InternalIndex(R)], Pivot) <= 0;
    if L > R then break;
    d.FastSwap(L, R);
  until False;
  Result.Left := R;
  Result.Right := L;
end;

class procedure TGBaseDequeHelper.DoQSort(d: TDeque; L, R: SizeInt);
begin
  while R - L > THelper.QUICK_INSERT_CUTOFF do
    with QSplitR(d, L, R) do
      if Left - L <= R - Right then
        begin
          DoQSort(d, L, Left);
          L := Right;
        end
      else
        begin
          DoQSort(d, Right, R);
          R := Left;
        end;
  if R - L > 0 then
    InsertionSort(d, L, R);
end;

class procedure TGBaseDequeHelper.DoQSort(var d: TLiteDeque; L, R: SizeInt);
begin
  while R - L > THelper.QUICK_INSERT_CUTOFF do
    with QSplitR(d, L, R) do
      if Left - L <= R - Right then
        begin
          DoQSort(d, L, Left);
          L := Right;
        end
      else
        begin
          DoQSort(d, Right, R);
          R := Left;
        end;
  if R - L > 0 then
    InsertionSort(d, L, R);
end;

class function TGBaseDequeHelper.GetMo9Pivot(d: TDeque; L, R: SizeInt): T;
begin
  if R - L > THelper.MEDIAN_OF9_CUTOFF then
    begin
      Result := THelper.MedianOf3(
        THelper.MedianOf3(
          d.FItems[d.InternalIndex(L)],
          d.FItems[d.InternalIndex(L + Succ(R - L) shr 3)],
          d.FItems[d.InternalIndex(L + Succ(R - L) shr 2)]),
        THelper.MedianOf3(
          d.FItems[d.InternalIndex(L + Succ(R - L) shr 1 - Succ(R - L) shr 3)],
          d.FItems[d.InternalIndex(L + Succ(R - L) shr 1)],
          d.FItems[d.InternalIndex(L + Succ(R - L) shr 1 + Succ(R - L) shr 3)]),
        THelper.MedianOf3(
          d.FItems[d.InternalIndex(R - Succ(R - L) shr 2)],
          d.FItems[d.InternalIndex(R - Succ(R - L) shr 3)],
          d.FItems[d.InternalIndex(R)]));
    end
  else
    Result := THelper.MedianOf3(
          d.FItems[d.InternalIndex(L)],
          d.FItems[d.InternalIndex(L + Succ(R - L) shr 1)],
          d.FItems[d.InternalIndex(R)]);
end;

class function TGBaseDequeHelper.GetMo9Pivot(constref d: TLiteDeque; L, R: SizeInt): T;
begin
  if R - L > THelper.MEDIAN_OF9_CUTOFF then
    begin
      Result := THelper.MedianOf3(
        THelper.MedianOf3(
          d.FBuffer.FItems[d.InternalIndex(L)],
          d.FBuffer.FItems[d.InternalIndex(L + Succ(R - L) shr 3)],
          d.FBuffer.FItems[d.InternalIndex(L + Succ(R - L) shr 2)]),
        THelper.MedianOf3(
          d.FBuffer.FItems[d.InternalIndex(L + Succ(R - L) shr 1 - Succ(R - L) shr 3)],
          d.FBuffer.FItems[d.InternalIndex(L + Succ(R - L) shr 1)],
          d.FBuffer.FItems[d.InternalIndex(L + Succ(R - L) shr 1 + Succ(R - L) shr 3)]),
        THelper.MedianOf3(
          d.FBuffer.FItems[d.InternalIndex(R - Succ(R - L) shr 2)],
          d.FBuffer.FItems[d.InternalIndex(R - Succ(R - L) shr 3)],
          d.FBuffer.FItems[d.InternalIndex(R)]));
    end
  else
    Result := THelper.MedianOf3(
          d.FBuffer.FItems[d.InternalIndex(L)],
          d.FBuffer.FItems[d.InternalIndex(L + Succ(R - L) shr 1)],
          d.FBuffer.FItems[d.InternalIndex(R)]);
end;

class function TGBaseDequeHelper.QSplitMo9(d: TDeque; L, R: SizeInt): TSortSplit;
var
  Pivot: T;
begin
  Pivot := GetMo9Pivot(d, L, R);
  Dec(L);
  Inc(R);
  repeat
    repeat Inc(L) until TCmpRel.Compare(Pivot, d.FItems[d.InternalIndex(L)]) <= 0;
    repeat Dec(R) until TCmpRel.Compare(d.FItems[d.InternalIndex(R)], Pivot) <= 0;
    if L > R then break;
    d.FastSwap(L, R);
  until False;
  Result.Left := R;
  Result.Right := L;
end;

class function TGBaseDequeHelper.QSplitMo9(var d: TLiteDeque; L, R: SizeInt): TSortSplit;
var
  Pivot: T;
begin
  Pivot := GetMo9Pivot(d, L, R);
  Dec(L);
  Inc(R);
  repeat
    repeat Inc(L) until TCmpRel.Compare(Pivot, d.FBuffer.FItems[d.InternalIndex(L)]) <= 0;
    repeat Dec(R) until TCmpRel.Compare(d.FBuffer.FItems[d.InternalIndex(R)], Pivot) <= 0;
    if L > R then break;
    d.FastSwap(L, R);
  until False;
  Result.Left := R;
  Result.Right := L;
end;

class procedure TGBaseDequeHelper.DoIntroSort(d: TDeque; L, R, Ttl: SizeInt);
begin
  if R - L > THelper.QUICK_INSERT_CUTOFF then
    if Ttl > 0 then
      with QSplitMo9(d, L, R) do
        begin
          DoIntroSort(d, L, Left, Pred(Ttl));
          DoIntroSort(d, Right, R, Pred(Ttl));
        end
    else
      DoHeapSort(d, L, R)
  else
    if R - L > 0 then
      InsertionSort(d, L, R);
end;

class procedure TGBaseDequeHelper.DoIntroSort(var d: TLiteDeque; L, R, Ttl: SizeInt);
begin
  if R - L > THelper.QUICK_INSERT_CUTOFF then
    if Ttl > 0 then
      with QSplitMo9(d, L, R) do
        begin
          DoIntroSort(d, L, Left, Pred(Ttl));
          DoIntroSort(d, Right, R, Pred(Ttl));
        end
    else
      DoHeapSort(d, L, R)
  else
    if R - L > 0 then
      InsertionSort(d, L, R);
end;

class function TGBaseDequeHelper.SequentSearch(d: TDeque; constref aValue: T): SizeInt;
begin
  for Result := 0 to Pred(d.ElemCount) do
    if TCmpRel.Compare(aValue, d.FastGetItem(Result)) = 0 then
      exit;
  Result := -1;
end;

class function TGBaseDequeHelper.SequentSearch(constref d: TLiteDeque; constref aValue: T): SizeInt;
begin
  for Result := 0 to Pred(d.Count) do
    if TCmpRel.Compare(aValue, d.FastGetItem(Result)) = 0 then
      exit;
  Result := -1;
end;

class function TGBaseDequeHelper.BinarySearch(d: TDeque; constref aValue: T): SizeInt;
begin
  if d.ElemCount > 0 then
    Result := DoBinSearch(d, aValue)
  else
    Result := -1;
end;

class function TGBaseDequeHelper.BinarySearch(constref d: TLiteDeque; constref aValue: T): SizeInt;
begin
  if d.Count > 0 then
    Result := DoBinSearch(d, aValue)
  else
    Result := -1;
end;

class function TGBaseDequeHelper.IndexOfMin(d: TDeque): SizeInt;
var
  R, I: SizeInt;
  m, v: T;
begin
  R := Pred(d.ElemCount);
  if R >= 0 then
    begin
      Result := 0;
      m := d.FastGetItem(0);
      for I := 1 to R do
        begin
          v := d.FastGetItem(I);
          if TCmpRel.Compare(v, m) < 0 then
            begin
              m := v;
              Result := I;
            end;
        end;
    end
  else
    Result := R;
end;

class function TGBaseDequeHelper.IndexOfMin(constref d: TLiteDeque): SizeInt;
var
  R, I: SizeInt;
  m, v: T;
begin
  R := Pred(d.Count);
  if R >= 0 then
    begin
      Result := 0;
      m := d.FastGetItem(0);
      for I := 1 to R do
        begin
          v := d.FastGetItem(I);
          if TCmpRel.Compare(v, m) < 0 then
            begin
              m := v;
              Result := I;
            end;
        end;
    end
  else
    Result := R;
end;

class function TGBaseDequeHelper.IndexOfMax(d: TDeque): SizeInt;
var
  R, I: SizeInt;
  m, v: T;
begin
  R := Pred(d.ElemCount);
  if R >= 0 then
    begin
      Result := 0;
      m := d.FastGetItem(0);
      for I := 1 to R do
        begin
          v := d.FastGetItem(I);
          if TCmpRel.Compare(m, v) < 0 then
            begin
              m := v;
              Result := I;
            end;
        end;
    end
  else
    Result := R;
end;

class function TGBaseDequeHelper.IndexOfMax(constref d: TLiteDeque): SizeInt;
var
  R, I: SizeInt;
  m, v: T;
begin
  R := Pred(d.Count);
  if R >= 0 then
    begin
      Result := 0;
      m := d.FastGetItem(0);
      for I := 1 to R do
        begin
          v := d.FastGetItem(I);
          if TCmpRel.Compare(m, v) < 0 then
            begin
              m := v;
              Result := I;
            end;
        end;
    end
  else
    Result := R;
end;

class function TGBaseDequeHelper.GetMin(d: TDeque): TOptional;
var
  v: T;
begin
  if FindMin(d, v) then
    Result.Assign(v);
end;

class function TGBaseDequeHelper.GetMin(constref d: TLiteDeque): TOptional;
var
  v: T;
begin
  if FindMin(d, v) then
    Result.Assign(v);
end;

class function TGBaseDequeHelper.GetMax(d: TDeque): TOptional;
var
  v: T;
begin
  if FindMax(d, v) then
    Result.Assign(v);
end;

class function TGBaseDequeHelper.GetMax(constref d: TLiteDeque): TOptional;
var
  v: T;
begin
  if FindMax(d, v) then
    Result.Assign(v);
end;

class function TGBaseDequeHelper.FindMin(d: TDeque; out aValue: T): Boolean;
var
  R, I: SizeInt;
  v: T;
begin
  R := Pred(d.ElemCount);
  Result := R >= 0;
  if Result then
    begin
      aValue := d.FastGetItem(0);
      for I := 1 to R do
        begin
          v := d.FastGetItem(I);
          if TCmpRel.Compare(v, aValue) < 0 then
            aValue := v;
        end;
    end;
end;

class function TGBaseDequeHelper.FindMin(constref d: TLiteDeque; out aValue: T): Boolean;
var
  R, I: SizeInt;
  v: T;
begin
  R := Pred(d.Count);
  Result := R >= 0;
  if Result then
    begin
      aValue := d.FastGetItem(0);
      for I := 1 to R do
        begin
          v := d.FastGetItem(I);
          if TCmpRel.Compare(v, aValue) < 0 then
            aValue := v;
        end;
    end;
end;

class function TGBaseDequeHelper.FindMax(d: TDeque; out aValue: T): Boolean;
var
  R, I: SizeInt;
  v: T;
begin
  R := Pred(d.ElemCount);
  Result := R >= 0;
  if Result then
    begin
      aValue := d.FastGetItem(0);
      for I := 1 to R do
        begin
          v := d.FastGetItem(I);
          if TCmpRel.Compare(aValue, v) < 0 then
            aValue := v;
        end;
    end;
end;

class function TGBaseDequeHelper.FindMax(constref d: TLiteDeque; out aValue: T): Boolean;
var
  R, I: SizeInt;
  v: T;
begin
  R := Pred(d.Count);
  Result := R >= 0;
  if Result then
    begin
      aValue := d.FastGetItem(0);
      for I := 1 to R do
        begin
          v := d.FastGetItem(I);
          if TCmpRel.Compare(aValue, v) < 0 then
            aValue := v;
        end;
    end;
end;

class function TGBaseDequeHelper.FindMinMax(d: TDeque; out aMin, aMax: T): Boolean;
var
  R, I: SizeInt;
  v: T;
begin
  R := Pred(d.ElemCount);
  Result := R >= 0;
  if Result then
    begin
      aMin := d.FastGetItem(0);
      aMax := aMin;
      for I := 1 to R do
        begin
          v := d.FastGetItem(I);
          if TCmpRel.Compare(aMax, v) < 0 then
            aMax := v
          else
            if TCmpRel.Compare(v, aMin) < 0 then
              aMin := v;
        end;
    end;
end;

class function TGBaseDequeHelper.FindMinMax(constref d: TLiteDeque; out aMin, aMax: T): Boolean;
var
  R, I: SizeInt;
  v: T;
begin
  R := Pred(d.Count);
  Result := R >= 0;
  if Result then
    begin
      aMin := d.FastGetItem(0);
      aMax := aMin;
      for I := 1 to R do
        begin
          v := d.FastGetItem(I);
          if TCmpRel.Compare(aMax, v) < 0 then
            aMax := v
          else
            if TCmpRel.Compare(v, aMin) < 0 then
              aMin := v;
        end;
    end;
end;

class function TGBaseDequeHelper.FindNthSmallest(d: TDeque; N: SizeInt; out aValue: T): Boolean;
begin
  Result := THelper.FindNthSmallest(d.ToArray, N, aValue);
end;

class function TGBaseDequeHelper.FindNthSmallest(constref d: TLiteDeque; N: SizeInt; out aValue: T): Boolean;
begin
  Result := THelper.FindNthSmallest(d.ToArray, N, aValue);
end;

class function TGBaseDequeHelper.NthSmallest(d: TDeque; N: SizeInt): TOptional;
var
  v: T;
begin
  if FindNthSmallest(d, N, v) then
    Result.Assign(v);
end;

class function TGBaseDequeHelper.NthSmallest(constref d: TLiteDeque; N: SizeInt): TOptional;
var
  v: T;
begin
  if FindNthSmallest(d, N, v) then
    Result.Assign(v);
end;

class function TGBaseDequeHelper.NextPermutation2Asc(d: TDeque): Boolean;
var
  I, J, R: SizeInt;
begin
  R := Pred(d.ElemCount);
  if R > 0 then
    begin
      d.CheckInIteration;
      J := -1;
      for I := Pred(R) downto 0 do
        if TCmpRel.Compare(d.FastGetItem(I), d.FastGetItem(Succ(I))) > 0 then
          begin
            J := I;
            break;
          end;
      if J < 0 then exit(False);
      for I := R downto 0 do
        if TCmpRel.Compare(d.FastGetItem(J), d.FastGetItem(I)) > 0 then
          begin
            d.FastSwap(I, J);
            break;
          end;
      DoReverse(d, Succ(J), R);
      Result := True;
    end
  else
    Result := False;
end;

class function TGBaseDequeHelper.NextPermutation2Asc(var d: TLiteDeque): Boolean;
var
  I, J, R: SizeInt;
begin
  R := Pred(d.Count);
  if R > 0 then
    begin
      J := -1;
      for I := Pred(R) downto 0 do
        if TCmpRel.Compare(d.FastGetItem(I), d.FastGetItem(Succ(I))) > 0 then
          begin
            J := I;
            break;
          end;
      if J < 0 then exit(False);
      for I := R downto 0 do
        if TCmpRel.Compare(d.FastGetItem(J), d.FastGetItem(I)) > 0 then
          begin
            d.FastSwap(I, J);
            break;
          end;
      DoReverse(d, Succ(J), R);
      Result := True;
    end
  else
    Result := False;
end;

class function TGBaseDequeHelper.NextPermutation2Desc(d: TDeque): Boolean;
var
  I, J, R: SizeInt;
begin
  R := Pred(d.ElemCount);
  if R > 0 then
    begin
      d.CheckInIteration;
      J := -1;
      for I := Pred(R) downto 0 do
        if TCmpRel.Compare(d.FastGetItem(I), d.FastGetItem(Succ(I))) < 0 then
          begin
            J := I;
            break;
          end;
      if J < 0 then exit(False);
      for I := R downto 0 do
        if TCmpRel.Compare(d.FastGetItem(J), d.FastGetItem(I)) < 0 then
          begin
            d.FastSwap(I, J);
            break;
          end;
      DoReverse(d, Succ(J), R);
      Result := True;
    end
  else
    Result := False;
end;

class function TGBaseDequeHelper.NextPermutation2Desc(var d: TLiteDeque): Boolean;
var
  I, J, R: SizeInt;
begin
  R := Pred(d.Count);
  if R > 0 then
    begin
      J := -1;
      for I := Pred(R) downto 0 do
        if TCmpRel.Compare(d.FastGetItem(I), d.FastGetItem(Succ(I))) < 0 then
          begin
            J := I;
            break;
          end;
      if J < 0 then exit(False);
      for I := R downto 0 do
        if TCmpRel.Compare(d.FastGetItem(J), d.FastGetItem(I)) < 0 then
          begin
            d.FastSwap(I, J);
            break;
          end;
      DoReverse(d, Succ(J), R);
      Result := True;
    end
  else
    Result := False;
end;

class function TGBaseDequeHelper.IsNonDescending(d: TDeque): Boolean;
var
  I: SizeInt;
begin
  for I := 0 to d.ElemCount - 2 do
    if TCmpRel.Compare(d.FastGetItem(Succ(I)), d.FastGetItem(I)) < 0 then
      exit(False);
  Result := True;
end;

class function TGBaseDequeHelper.IsNonDescending(constref d: TLiteDeque): Boolean;
var
  I: SizeInt;
begin
  for I := 0 to d.Count - 2 do
    if TCmpRel.Compare(d.FastGetItem(Succ(I)), d.FastGetItem(I)) < 0 then
      exit(False);
  Result := True;
end;

class function TGBaseDequeHelper.IsStrictAscending(d: TDeque): Boolean;
var
  I, R: SizeInt;
begin
  R := Pred(d.ElemCount);
  if R > 0 then
    begin
      for I := 0 to Pred(R) do
        if TCmpRel.Compare(d.FastGetItem(Succ(I)), d.FastGetItem(I)) <= 0 then
          exit(False);
      Result := True;
    end
  else
    Result := False;
end;

class function TGBaseDequeHelper.IsStrictAscending(constref d: TLiteDeque): Boolean;
var
  I, R: SizeInt;
begin
  R := Pred(d.Count);
  if R > 0 then
    begin
      for I := 0 to Pred(R) do
        if TCmpRel.Compare(d.FastGetItem(Succ(I)), d.FastGetItem(I)) <= 0 then
          exit(False);
      Result := True;
    end
  else
    Result := False;
end;

class function TGBaseDequeHelper.IsNonAscending(d: TDeque): Boolean;
var
  I: SizeInt;
begin
  for I := 0 to d.ElemCount - 2 do
    if TCmpRel.Compare(d.FastGetItem(I), d.FastGetItem(Succ(I))) < 0 then
      exit(False);
  Result := True;
end;

class function TGBaseDequeHelper.IsNonAscending(constref d: TLiteDeque): Boolean;
var
  I: SizeInt;
begin
  for I := 0 to d.Count - 2 do
    if TCmpRel.Compare(d.FastGetItem(I), d.FastGetItem(Succ(I))) < 0 then
      exit(False);
  Result := True;
end;

class function TGBaseDequeHelper.IsStrictDescending(d: TDeque): Boolean;
var
  I, R: SizeInt;
begin
  R := Pred(d.ElemCount);
  if R > 0 then
    begin
      for I := 0 to Pred(R) do
        if TCmpRel.Compare(d.FastGetItem(I), d.FastGetItem(Succ(I))) <= 0 then
          exit(False);
      Result := True;
    end
  else
    Result := False;
end;

class function TGBaseDequeHelper.IsStrictDescending(constref d: TLiteDeque): Boolean;
var
  I, R: SizeInt;
begin
  R := Pred(d.Count);
  if R > 0 then
    begin
      for I := 0 to Pred(R) do
        if TCmpRel.Compare(d.FastGetItem(I), d.FastGetItem(Succ(I))) <= 0 then
          exit(False);
      Result := True;
    end
  else
    Result := False;
end;

class function TGBaseDequeHelper.Same(d1, d2: TDeque): Boolean;
var
  I, c: SizeInt;
begin
  c := d1.ElemCount;
  if d2.ElemCount <> c then
    exit(False);
  for I := 0 to Pred(c) do
    if TCmpRel.Compare(d1.FastGetItem(I), d2.FastGetItem(I)) <> 0 then
      exit(False);
  Result := True;
end;

class function TGBaseDequeHelper.Same(constref d1, d2: TLiteDeque): Boolean;
var
  I, c: SizeInt;
begin
  c := d1.Count;
  if d2.Count <> c then
    exit(False);
  for I := 0 to Pred(c) do
    if TCmpRel.Compare(d1.FastGetItem(I), d2.FastGetItem(I)) <> 0 then
      exit(False);
  Result := True;
end;

class procedure TGBaseDequeHelper.QuickSort(d: TDeque; o: TSortOrder);
var
  R: SizeInt;
begin
  R := Pred(d.ElemCount);
  if R > 0 then
    begin
      d.CheckInIteration;
      if CountRun2Asc(d, R) < R then
        DoQSort(d, 0, R);
      if o = soDesc then
        DoReverse(d, 0, R);
    end;
end;

class procedure TGBaseDequeHelper.QuickSort(var d: TLiteDeque; o: TSortOrder);
var
  R: SizeInt;
begin
  R := Pred(d.Count);
  if R > 0 then
    begin
      if CountRun2Asc(d, R) < R then
        DoQSort(d, 0, R);
      if o = soDesc then
        DoReverse(d, 0, R);
    end;
end;

class procedure TGBaseDequeHelper.IntroSort(d: TDeque; o: TSortOrder);
var
  R: SizeInt;
begin
  R := Pred(d.ElemCount);
  if R > 0 then
    begin
      d.CheckInIteration;
      if CountRun2Asc(d, R) < R then
        DoIntroSort(d, 0, R, Pred(LGUtils.NSB(R + 1)) * THelper.INTRO_LOG_FACTOR);
      if o = soDesc then
        DoReverse(d, 0, R);
    end;
end;

class procedure TGBaseDequeHelper.IntroSort(var d: TLiteDeque; o: TSortOrder);
var
  R: SizeInt;
begin
  R := Pred(d.Count);
  if R > 0 then
    begin
      if CountRun2Asc(d, R) < R then
        DoIntroSort(d, 0, R, Pred(LGUtils.NSB(R + 1)) * THelper.INTRO_LOG_FACTOR);
      if o = soDesc then
        DoReverse(d, 0, R);
    end;
end;

class procedure TGBaseDequeHelper.Sort(d: TDeque; o: TSortOrder);
begin
  IntroSort(d, o);
end;

class procedure TGBaseDequeHelper.Sort(var d: TLiteDeque; o: TSortOrder);
begin
  IntroSort(d, o);
end;

class function TGBaseDequeHelper.SelectDistinct(d: TDeque): TDeque.TArray;
begin
  if d.ElemCount > 0 then
    Result := THelper.SelectDistinct(d.ToArray)
  else
    Result := nil;
end;

class function TGBaseDequeHelper.SelectDistinct(constref d: TLiteDeque): TDeque.TArray;
begin
  if d.Count > 0 then
    Result := THelper.SelectDistinct(d.ToArray)
  else
    Result := nil;
end;

end.

