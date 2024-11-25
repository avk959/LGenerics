unit LGPriorityQueueTest;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}

interface
uses
  SysUtils, fpcunit, testregistry,
  LGUtils,
  LGHelpers,
  lgAbstractContainer,
  LGPriorityQueue,
  LGArrayHelpers;

type

  { TGPriorityQueueTest }

  TGPriorityQueueTest = class(TTestCase)
  private
  type
    TRec = record
      Key,
      Value: Integer;
      class operator < (const L, R: TRec): Boolean; inline;
      constructor Create(k, v: Integer);
    end;

    TIntBaseBinHeap          = specialize TGBinHeap<Integer>;
    TIntComparbleBinHeapMax  = specialize TGComparableBinHeapMax<Integer>;
    TIntComparbleBinHeapMin  = specialize TGComparableBinHeapMin<Integer>;
    TIntRegularBinHeap       = specialize TGRegularBinHeap<Integer>;
    TIntDelegatedBinHeap     = specialize TGDelegatedBinHeap<Integer>;

    TLiteIntBinHeap          = specialize TGLiteBinHeap<Integer, Integer>;
    TLiteIntBinHeapMin       = specialize TGLiteComparableBinHeapMin<Integer>;
    TIntKSelect              = specialize TGKSelect<Integer, Integer>;
    TIntCompKSelect          = specialize TGComparableKSelect<Integer>;
    TIntRegKSelect           = specialize TGRegularKSelect<Integer>;

    TIntBasePairHeap         = specialize TGPairingHeap<Integer>;
    TIntComparblePairHeapMax = specialize TGComparablePairHeapMax<Integer>;
    TIntComparblePairHeapMin = specialize TGComparablePairHeapMin<Integer>;
    TIntRegularPairHeap      = specialize TGRegularPairHeap<Integer>;
    TIntDelegatedPairHeap    = specialize TGDelegatedPairHeap<Integer>;

    TLiteIntPairingHeap      = specialize TGLitePairingHeap<Integer, Integer>;
    TLiteIntPairHeapMin      = specialize TGLiteComparablePairHeapMin<Integer>;

    TIntHelper               = specialize TGOrdinalArrayHelper<Integer>;
    TIntArray                = TIntBaseBinHeap.TArray;
    IIntEnumerable           = specialize IGEnumerable<Integer>;
    TIntArrayCursor          = specialize TGArrayCursor<Integer>;
    THandleArray             = array of THandle;
    function LongIntCmp(const L, R: LongInt): Boolean;
    function EqualSets(const L, R: array of Integer): Boolean;
  published
    procedure BaseBinHeap;
    procedure BaseBinHeapReverse;
    procedure ComparableBinHeapMax;
    procedure ComparableBinHeapMax1;
    procedure ComparableBinHeapMaxReverse;
    procedure ComparableBinHeapMin;
    procedure ComparableBinHeapMin1;
    procedure ComparableBinHeapMinReverse;
    procedure RegularBinHeap;
    procedure DefaultRegularBinHeap;
    procedure RegularBinHeapReverse;
    procedure DelegatedBinHeap;
    procedure DefaultDelegatedBinHeap;
    procedure DelegatedBinHeapReverse;
    procedure RegularKSelectArray;
    procedure RegularKSelectEnum;

    procedure LiteBinHeap;
    procedure LiteBinHeapReverse;
    procedure LiteBinHeapAssign;
    procedure LiteBinHeapPassByValue;
    procedure LiteBinHeapAddAll;
    procedure LiteComparableBinHeap;
    procedure LiteComparableBinHeap1;
    procedure LiteComparableBinHeapReverse;
    procedure LiteComparableBinHeapAssign;
    procedure LiteComparableBinHeapPassByValue;
    procedure LiteComparableBinHeapAddAll;
    procedure KSelectArray;
    procedure KSelectEnum;
    procedure ComparableKSelectArray;
    procedure ComparableKSelectEnum;

    procedure BasePairHeapEnqueueAll;
    procedure BasePairHeapUpdate;
    procedure BasePairHeapMerge;
    procedure BasePairHeapReverse;

    procedure ComparablePairHeapMaxEnqueueAll;
    procedure ComparablePairHeapMaxUpdate;
    procedure ComparablePairHeapMaxMerge;
    procedure ComparablePairHeapMaxReverse;

    procedure ComparablePairHeapMinEnqueueAll;
    procedure ComparablePairHeapMinUpdate;
    procedure ComparablePairHeapMinMerge;
    procedure ComparablePairHeapMinReverse;

    procedure RegularPairHeapEnqueueAll;
    procedure RegularPairHeapUpdate;
    procedure RegularPairHeapMerge;
    procedure RegularPairHeapReverse;

    procedure DefaultRegularPairHeap1;
    procedure DefaultRegularPairHeap2;
    procedure DefaultRegularPairHeap3;

    procedure DelegatedPairHeapEnqueueAll;
    procedure DelegatedPairHeapUpdate;
    procedure DelegatedPairHeapMerge;
    procedure DelegatedPairHeapReverse;

    procedure DefaultDelegatedPairHeap1;
    procedure DefaultDelegatedPairHeap2;
    procedure DefaultDelegatedPairHeap3;

    procedure LitePairHeapEnqueue;
    procedure LitePairHeapUpdate;
    procedure LitePairHeapMerge;
    procedure LitePairHeapReverse;
    procedure LitePairHeapAssign;
    procedure LitePairHeapPassByValue;

    procedure LiteCompareblePairHeapEnqueue;
    procedure LiteCompareblePairHeapEnqueue1;
    procedure LiteCompareblePairHeapUpdate;
    procedure LiteCompareblePairHeapMerge;
    procedure LiteCompareblePairHeapReverse;
    procedure LiteCompareblePairHeapAssign;
    procedure LiteCompareblePairHeapPassByValue;
  end;

implementation

{ TGPriorityQueueTest.TRec }

class operator TGPriorityQueueTest.TRec.<(const L, R: TRec): Boolean;
begin
  Result := L.Key < R.Key;
end;

constructor TGPriorityQueueTest.TRec.Create(k, v: Integer);
begin
  Key := k;
  Value := v;
end;

function TGPriorityQueueTest.LongIntCmp(const L, R: LongInt): Boolean;
begin
  Result := L < R;
end;

function TGPriorityQueueTest.EqualSets(const L, R: array of Integer): Boolean;
var
  LocR: TIntArray;
  Item, I: Integer;
begin
  if Length(L) <> Length(R) then exit(False);
  LocR := TIntHelper.CreateCopy(R);
  for Item in L do begin
    I := TIntHelper.SequentSearch(LocR, Item);
    if I < 0 then exit(False);
    Delete(LocR, I, 1);
  end;
  Result := LocR = nil;
end;

procedure TGPriorityQueueTest.BaseBinHeap;
var
  q: TIntBaseBinHeap;
  I: SizeInt;
  a: TIntArray;
begin
  System.SetLength(a, 100);
  for I := 1 to System.Length(a) do
    a[I - 1] := I * 1001;
  TIntHelper.RandomShuffle(a);
  q := TIntBaseBinHeap.Create;
  try
    AssertTrue(q.IsEmpty);
    I := q.EnqueueAll(a);
    AssertTrue(I = System.Length(a));
    AssertTrue(q.Count = System.Length(a));
    I := 0;
    while q.NonEmpty do
      begin
        a[I] := q.Dequeue;
        Inc(I);
      end;
    AssertTrue(I = System.Length(a));
    AssertTrue(TIntHelper.IsStrictDescending(a));
  finally
    q.Free;
  end;
end;

procedure TGPriorityQueueTest.BaseBinHeapReverse;
var
  q: TIntBaseBinHeap;
  I: SizeInt;
  a, b: TIntArray;
begin
  System.SetLength(a, 100);
  for I := 1 to System.Length(a) do
    a[I - 1] := I * 1001;
  TIntHelper.RandomShuffle(a);
  q := TIntBaseBinHeap.Create(a);
  try
    a := TIntHelper.CreateReverseCopy(q.ToArray);
    b := q.Reverse.ToArray;
    AssertTrue(TIntHelper.Same(a, b));
  finally
    q.Free;
  end;
end;

procedure TGPriorityQueueTest.ComparableBinHeapMax;
var
  q: TIntComparbleBinHeapMax;
  I: SizeInt;
  a: TIntArray;
begin
  System.SetLength(a, 100);
  for I := 1 to System.Length(a) do
    a[I - 1] := I * 1001;
  TIntHelper.RandomShuffle(a);
  q := TIntComparbleBinHeapMax.Create;
  try
    AssertTrue(q.IsEmpty);
    I := q.EnqueueAll(a);
    AssertTrue(I = System.Length(a));
    AssertTrue(q.Count = System.Length(a));
    I := 0;
    while q.NonEmpty do
      begin
        a[I] := q.Dequeue;
        Inc(I);
      end;
    AssertTrue(I = System.Length(a));
    AssertTrue(TIntHelper.IsStrictDescending(a));
  finally
    q.Free;
  end;
end;

procedure TGPriorityQueueTest.ComparableBinHeapMax1;
var
  q: specialize TGComparableBinHeapMax<TRec>;
  I: Integer;
  a: array of TRec;
begin
  System.SetLength(a, 100);
  for I := 1 to System.Length(a) do
    a[I - 1] := TRec.Create(I * 1001, 0);
  specialize TGComparableArrayHelper<TRec>.RandomShuffle(a);
  q := specialize TGComparableBinHeapMax<TRec>.Create;
  try
    AssertTrue(q.IsEmpty);
    I := q.EnqueueAll(a);
    AssertTrue(I = System.Length(a));
    AssertTrue(q.Count = System.Length(a));
    I := 0;
    while q.NonEmpty do
      begin
        a[I] := q.Dequeue;
        Inc(I);
      end;
    AssertTrue(I = System.Length(a));
    AssertTrue(specialize TGComparableArrayHelper<TRec>.IsStrictDescending(a));
  finally
    q.Free;
  end;
end;

procedure TGPriorityQueueTest.ComparableBinHeapMaxReverse;
var
  q: TIntComparbleBinHeapMax;
  I: SizeInt;
  a, b: TIntArray;
begin
  System.SetLength(a, 100);
  for I := 1 to System.Length(a) do
    a[I - 1] := I * 1001;
  TIntHelper.RandomShuffle(a);
  q := TIntComparbleBinHeapMax.Create(a);
  try
    a := TIntHelper.CreateReverseCopy(q.ToArray);
    b := q.Reverse.ToArray;
    AssertTrue(TIntHelper.Same(a, b));
  finally
    q.Free;
  end;
end;

procedure TGPriorityQueueTest.ComparableBinHeapMin;
var
  q: TIntComparbleBinHeapMin;
  I: SizeInt;
  a: TIntArray;
begin
  System.SetLength(a, 100);
  for I := 1 to System.Length(a) do
    a[I - 1] := I * 1001;
  TIntHelper.RandomShuffle(a);
  q := TIntComparbleBinHeapMin.Create;
  try
    AssertTrue(q.IsEmpty);
    I := q.EnqueueAll(a);
    AssertTrue(I = System.Length(a));
    AssertTrue(q.Count = System.Length(a));
    I := 0;
    while q.NonEmpty do
      begin
        a[I] := q.Dequeue;
        Inc(I);
      end;
    AssertTrue(I = System.Length(a));
    AssertTrue(TIntHelper.IsStrictAscending(a));
  finally
    q.Free;
  end;
end;

procedure TGPriorityQueueTest.ComparableBinHeapMin1;
var
  q: specialize TGComparableBinHeapMin<TRec>;
  I: Integer;
  a: array of TRec;
begin
  System.SetLength(a, 100);
  for I := 1 to System.Length(a) do
    a[I - 1] := TRec.Create(I * 1001, 0);
  specialize TGComparableArrayHelper<TRec>.RandomShuffle(a);
  q := specialize TGComparableBinHeapMin<TRec>.Create;
  try
    AssertTrue(q.IsEmpty);
    I := q.EnqueueAll(a);
    AssertTrue(I = System.Length(a));
    AssertTrue(q.Count = System.Length(a));
    I := 0;
    while q.NonEmpty do
      begin
        a[I] := q.Dequeue;
        Inc(I);
      end;
    AssertTrue(I = System.Length(a));
    AssertTrue(specialize TGComparableArrayHelper<TRec>.IsStrictAscending(a));
  finally
    q.Free;
  end;
end;

procedure TGPriorityQueueTest.ComparableBinHeapMinReverse;
var
  q: TIntComparbleBinHeapMin;
  I: SizeInt;
  a, b: TIntArray;
begin
  System.SetLength(a, 100);
  for I := 1 to System.Length(a) do
    a[I - 1] := I * 1001;
  TIntHelper.RandomShuffle(a);
  q := TIntComparbleBinHeapMin.Create(a);
  try
    a := TIntHelper.CreateReverseCopy(q.ToArray);
    b := q.Reverse.ToArray;
    AssertTrue(TIntHelper.Same(a, b));
  finally
    q.Free;
  end;
end;

function CmpLongInt(const L, R: LongInt): Boolean;
begin
  Result := L < R;
end;

procedure TGPriorityQueueTest.RegularBinHeap;
var
  q: TIntRegularBinHeap;
  I: SizeInt;
  a: TIntArray;
begin
  System.SetLength(a, 100);
  for I := 1 to System.Length(a) do
    a[I - 1] := I * 1001;
  TIntHelper.RandomShuffle(a);
  q := TIntRegularBinHeap.Create(@CmpLongInt);
  try
    AssertTrue(q.IsEmpty);
    I := q.EnqueueAll(a);
    AssertTrue(I = System.Length(a));
    AssertTrue(q.Count = System.Length(a));
    I := 0;
    while q.NonEmpty do
      begin
        a[I] := q.Dequeue;
        Inc(I);
      end;
    AssertTrue(I = System.Length(a));
    AssertTrue(TIntHelper.IsStrictDescending(a));
  finally
    q.Free;
  end;
end;

procedure TGPriorityQueueTest.DefaultRegularBinHeap;
var
  q: TIntRegularBinHeap;
  I: SizeInt;
  a: TIntArray;
begin
  System.SetLength(a, 100);
  for I := 1 to System.Length(a) do
    a[I - 1] := I * 1001;
  TIntHelper.RandomShuffle(a);
  q := TIntRegularBinHeap.Create;
  try
    AssertTrue(q.IsEmpty);
    I := q.EnqueueAll(a);
    AssertTrue(I = System.Length(a));
    AssertTrue(q.Count = System.Length(a));
    I := 0;
    while q.NonEmpty do
      begin
        a[I] := q.Dequeue;
        Inc(I);
      end;
    AssertTrue(I = System.Length(a));
    AssertTrue(TIntHelper.IsStrictDescending(a));
  finally
    q.Free;
  end;
end;

procedure TGPriorityQueueTest.RegularBinHeapReverse;
var
  q: TIntRegularBinHeap;
  I: SizeInt;
  a, b: TIntArray;
begin
  System.SetLength(a, 100);
  for I := 1 to System.Length(a) do
    a[I - 1] := I * 1001;
  TIntHelper.RandomShuffle(a);
  q := TIntRegularBinHeap.Create(a, @CmpLongInt);
  try
    a := TIntHelper.CreateReverseCopy(q.ToArray);
    b := q.Reverse.ToArray;
    AssertTrue(TIntHelper.Same(a, b));
  finally
    q.Free;
  end;
end;

procedure TGPriorityQueueTest.DelegatedBinHeap;
var
  q: TIntDelegatedBinHeap;
  I: SizeInt;
  a: TIntArray;
begin
  System.SetLength(a, 100);
  for I := 1 to System.Length(a) do
    a[I - 1] := I * 1001;
  TIntHelper.RandomShuffle(a);
  q := TIntDelegatedBinHeap.Create(@LongIntCmp);
  try
    AssertTrue(q.IsEmpty);
    I := q.EnqueueAll(a);
    AssertTrue(I = System.Length(a));
    AssertTrue(q.Count = System.Length(a));
    I := 0;
    while q.NonEmpty do
      begin
        a[I] := q.Dequeue;
        Inc(I);
      end;
    AssertTrue(I = System.Length(a));
    AssertTrue(TIntHelper.IsStrictDescending(a));
  finally
    q.Free;
  end;
end;

procedure TGPriorityQueueTest.DefaultDelegatedBinHeap;
var
  q: TIntDelegatedBinHeap;
  I: SizeInt;
  a: TIntArray;
begin
  System.SetLength(a, 100);
  for I := 1 to System.Length(a) do
    a[I - 1] := I * 1001;
  TIntHelper.RandomShuffle(a);
  q := TIntDelegatedBinHeap.Create;
  try
    AssertTrue(q.IsEmpty);
    I := q.EnqueueAll(a);
    AssertTrue(I = System.Length(a));
    AssertTrue(q.Count = System.Length(a));
    I := 0;
    while q.NonEmpty do
      begin
        a[I] := q.Dequeue;
        Inc(I);
      end;
    AssertTrue(I = System.Length(a));
    AssertTrue(TIntHelper.IsStrictDescending(a));
  finally
    q.Free;
  end;
end;

procedure TGPriorityQueueTest.DelegatedBinHeapReverse;
var
  q: TIntDelegatedBinHeap;
  I: SizeInt;
  a, b: TIntArray;
begin
  System.SetLength(a, 100);
  for I := 1 to System.Length(a) do
    a[I - 1] := I * 1001;
  TIntHelper.RandomShuffle(a);
  q := TIntDelegatedBinHeap.Create(a, @LongIntCmp);
  try
    a := TIntHelper.CreateReverseCopy(q.ToArray);
    b := q.Reverse.ToArray;
    AssertTrue(TIntHelper.Same(a, b));
  finally
    q.Free;
  end;
end;

procedure TGPriorityQueueTest.RegularKSelectArray;
var
  a, r: TIntArray;
  c: specialize TGLessCompare<Integer>;
begin
  c := specialize TGDefaults<Integer>.Less;
  a := [];
  AssertTrue(TIntRegKSelect.GetKMax(a, 1, c) = nil);
  AssertTrue(TIntRegKSelect.GetKMin(a, 1, c) = nil);

  a := [1, 2];
  AssertTrue(TIntRegKSelect.GetKMax(a, -1, c) = nil);
  AssertTrue(TIntRegKSelect.GetKMin(a, -1, c) = nil);

  r := TIntRegKSelect.GetKMin(a, 3, c);
  AssertTrue(EqualSets(r, [1, 2]));

  r := TIntRegKSelect.GetKMax(a, 3, c);
  AssertTrue(EqualSets(r, [1, 2]));

  r := TIntRegKSelect.GetKMax(a, 1, c);
  AssertTrue(EqualSets(r, [2]));

  r := TIntRegKSelect.GetKMin(a, 1, c);
  AssertTrue(EqualSets(r, [1]));

  a := [1, 1, 2, 2, 3, 3, 4, 4];

  r := TIntRegKSelect.GetKMax(a, 3, c);
  AssertTrue(EqualSets(r, [3, 4, 4]));

  r := TIntRegKSelect.GetKMin(a, 3, c);
  AssertTrue(EqualSets(r, [1, 1, 2]));
end;

procedure TGPriorityQueueTest.RegularKSelectEnum;
var
  e: IIntEnumerable;
  a, r: TIntArray;
  c: specialize TGLessCompare<Integer>;
begin
  c := specialize TGDefaults<Integer>.Less;
  a := [];
  e := TIntArrayCursor.Create(a);
  AssertTrue(TIntRegKSelect.GetKMax(e, 1, c) = nil);
  e := TIntArrayCursor.Create(a);
  AssertTrue(TIntRegKSelect.GetKMin(e, 1, c) = nil);

  a := [1, 2];
  e := TIntArrayCursor.Create(a);
  AssertTrue(TIntRegKSelect.GetKMax(e, -1, c) = nil);
  e := TIntArrayCursor.Create(a);
  AssertTrue(TIntRegKSelect.GetKMin(e, -1, c) = nil);

  e := TIntArrayCursor.Create(a);
  r := TIntRegKSelect.GetKMin(e, 3, c);
  AssertTrue(EqualSets(r, [1, 2]));

  e := TIntArrayCursor.Create(a);
  r := TIntRegKSelect.GetKMax(e, 3, c);
  AssertTrue(EqualSets(r, [1, 2]));

  e := TIntArrayCursor.Create(a);
  r := TIntRegKSelect.GetKMax(e, 1, c);
  AssertTrue(EqualSets(r, [2]));

  e := TIntArrayCursor.Create(a);
  r := TIntRegKSelect.GetKMin(e, 1, c);
  AssertTrue(EqualSets(r, [1]));

  a := [1, 1, 2, 2, 3, 3, 4, 4];

  e := TIntArrayCursor.Create(a);
  r := TIntRegKSelect.GetKMax(e, 3, c);
  AssertTrue(EqualSets(r, [3, 4, 4]));

  e := TIntArrayCursor.Create(a);
  r := TIntRegKSelect.GetKMin(e, 3, c);
  AssertTrue(EqualSets(r, [1, 1, 2]));
end;

procedure TGPriorityQueueTest.LiteBinHeap;
var
  q: TLiteIntBinHeap;
  I: SizeInt;
  a: TIntArray;
begin
  System.SetLength(a, 100);
  for I := 1 to System.Length(a) do
    a[I - 1] := I * 1001;
  TIntHelper.RandomShuffle(a);
  AssertTrue({%H-}q.IsEmpty);
  for I in a do
    q.Enqueue(I);
  AssertTrue(q.Count = System.Length(a));
  I := 0;
  while q.NonEmpty do
    begin
      a[I] := q.Dequeue;
      Inc(I);
    end;
  AssertTrue(I = System.Length(a));
  AssertTrue(TIntHelper.IsStrictDescending(a));
end;

procedure TGPriorityQueueTest.LiteBinHeapReverse;
var
  q: TLiteIntBinHeap;
  I, J: SizeInt;
  a: TIntArray;
begin
  System.SetLength(a, 100);
  for I := 1 to System.Length(a) do
    a[I - 1] := I * 1001;
  TIntHelper.RandomShuffle(a);
  for I in a do
    q.Enqueue(I);
  a := TIntHelper.CreateReverseCopy(q.ToArray);
  J := 0;
  for I in q.Reverse do
    begin
      AssertTrue(I = a[J]);
      Inc(J);
    end;
end;

procedure TGPriorityQueueTest.LiteBinHeapAssign;
var
  h1, h2: TLiteIntBinHeap;
begin
  {%H-}h1.Enqueue(1);
  h1.Enqueue(2);
  h1.Enqueue(3);
  AssertTrue(h1.Count = 3);
  h2 := h1;
  AssertTrue(h2.Count = 3);
  h1.Clear;
  AssertTrue(h1.IsEmpty);
  AssertTrue(h2.Count = 3);
  AssertTrue(h2.Dequeue = 3);
  AssertTrue(h2.Dequeue = 2);
  AssertTrue(h2.Dequeue = 1);
  h2.Enqueue(4);
  h2.Enqueue(5);
  AssertTrue(h2.Count = 2);
  h1.Enqueue(6);
  AssertTrue(h1.Count = 1);
  h2 := h1;
  AssertTrue(h1.Dequeue = 6);
  AssertTrue(h1.IsEmpty);
  AssertTrue(h2.Count = 1);
  AssertTrue(h2.Dequeue = 6);
end;

procedure TGPriorityQueueTest.LiteBinHeapPassByValue;
  procedure Test(aHeap: TLiteIntBinHeap);
  begin
    aHeap.Enqueue(10);
    aHeap.Enqueue(1);
    AssertTrue(aHeap.NonEmpty);
  end;
var
  h: TLiteIntBinHeap;
begin
  {%H-}h.EnsureCapacity(10);
  AssertTrue(h.Capacity > 2);
  AssertTrue(h.IsEmpty);
  Test(h);
  AssertTrue(h.IsEmpty);
end;

procedure TGPriorityQueueTest.LiteBinHeapAddAll;
var
  q: TLiteIntBinHeap;
  I, J: Integer;
  a: TIntArray;
  e: IIntEnumerable;
const
  TestSize = 100;
begin
  a := TIntHelper.CreateRandomInRange(TestSize, -1000, 1000);
  q.EnqueueAll(a);
  AssertTrue(q.Count = TestSize);
  I := 0;
  J := 0;
  while q.TryDequeue(J) do
    begin
      a[I] := J;
      Inc(I);
    end;
  AssertTrue(TIntHelper.IsNonAscending(a));

  a := TIntHelper.CreateRandomInRange(TestSize, -2000, 2000);
  e := TIntArrayCursor.Create(a);
  q.EnqueueAll(e);
  AssertTrue(q.Count = TestSize);
  I := 0;
  J := 0;
  while q.TryDequeue(J) do
    begin
      a[I] := J;
      Inc(I);
    end;
  AssertTrue(TIntHelper.IsNonAscending(a));
end;

procedure TGPriorityQueueTest.LiteComparableBinHeap;
var
  q: TLiteIntBinHeapMin;
  I: SizeInt;
  a: TIntArray;
begin
  System.SetLength(a, 100);
  for I := 1 to System.Length(a) do
    a[I - 1] := I * 1001;
  TIntHelper.RandomShuffle(a);
  AssertTrue({%H-}q.IsEmpty);
  for I in a do
    q.Enqueue(I);
  AssertTrue(q.Count = System.Length(a));
  I := 0;
  while q.NonEmpty do
    begin
      a[I] := q.Dequeue;
      Inc(I);
    end;
  AssertTrue(I = System.Length(a));
  AssertTrue(TIntHelper.IsStrictAscending(a));
end;

procedure TGPriorityQueueTest.LiteComparableBinHeap1;
var
  q: specialize TGLiteComparableBinHeapMin<TRec>;
  I: Integer;
  a: array of TRec;
begin
  System.SetLength(a, 100);
  for I := 1 to System.Length(a) do
    a[I - 1] := TRec.Create(I * 1001, 0);
  specialize TGComparableArrayHelper<TRec>.RandomShuffle(a);
  AssertTrue({%H-}q.IsEmpty);
  for I := 0 to High(a) do
    q.Enqueue(a[I]);
  AssertTrue(q.Count = System.Length(a));
  I := 0;
  while q.NonEmpty do
    begin
      a[I] := q.Dequeue;
      Inc(I);
    end;
  AssertTrue(I = System.Length(a));
  AssertTrue(specialize TGComparableArrayHelper<TRec>.IsStrictAscending(a));
end;

procedure TGPriorityQueueTest.LiteComparableBinHeapReverse;
var
  q: TLiteIntBinHeapMin;
  I, J: SizeInt;
  a: TIntArray;
begin
  System.SetLength(a, 100);
  for I := 1 to System.Length(a) do
    a[I - 1] := I * 1001;
  TIntHelper.RandomShuffle(a);
  for I in a do
    q.Enqueue(I);
  a := TIntHelper.CreateReverseCopy(q.ToArray);
  J := 0;
  for I in q.Reverse do
    begin
      AssertTrue(I = a[J]);
      Inc(J);
    end;
end;

procedure TGPriorityQueueTest.LiteComparableBinHeapAssign;
var
  h1, h2: TLiteIntBinHeapMin;
begin
  {%H-}h1.Enqueue(1);
  h1.Enqueue(2);
  h1.Enqueue(3);
  AssertTrue(h1.Count = 3);
  h2 := h1;
  AssertTrue(h2.Count = 3);
  h1.Clear;
  AssertTrue(h1.IsEmpty);
  AssertTrue(h2.Count = 3);
  AssertTrue(h2.Dequeue = 1);
  AssertTrue(h2.Dequeue = 2);
  AssertTrue(h2.Dequeue = 3);
  h2.Enqueue(4);
  h2.Enqueue(5);
  AssertTrue(h2.Count = 2);
  h1.Enqueue(6);
  AssertTrue(h1.Count = 1);
  h2 := h1;
  AssertTrue(h1.Dequeue = 6);
  AssertTrue(h1.IsEmpty);
  AssertTrue(h2.Count = 1);
  AssertTrue(h2.Dequeue = 6);
end;

procedure TGPriorityQueueTest.LiteComparableBinHeapPassByValue;
  procedure Test(aHeap: TLiteIntBinHeapMin);
  begin
    aHeap.Enqueue(10);
    aHeap.Enqueue(1);
    AssertTrue(aHeap.NonEmpty);
  end;
var
  h: TLiteIntBinHeapMin;
begin
  {%H-}h.EnsureCapacity(10);
  AssertTrue(h.Capacity > 2);
  AssertTrue(h.IsEmpty);
  Test(h);
  AssertTrue(h.IsEmpty);
end;

procedure TGPriorityQueueTest.LiteComparableBinHeapAddAll;
var
  q: TLiteIntBinHeapMin;
  I, J: Integer;
  a: TIntArray;
  e: IIntEnumerable;
const
  TestSize = 100;
begin
  a := TIntHelper.CreateRandomInRange(TestSize, -1000, 1000);
  q.EnqueueAll(a);
  AssertTrue(q.Count = TestSize);
  I := 0;
  J := 0;
  while q.TryDequeue(J) do
    begin
      a[I] := J;
      Inc(I);
    end;
  AssertTrue(TIntHelper.IsNonDescending(a));

  a := TIntHelper.CreateRandomInRange(TestSize, -2000, 2000);
  e := TIntArrayCursor.Create(a);
  q.EnqueueAll(e);
  AssertTrue(q.Count = TestSize);
  I := 0;
  J := 0;
  while q.TryDequeue(J) do
    begin
      a[I] := J;
      Inc(I);
    end;
  AssertTrue(TIntHelper.IsNonDescending(a));
end;

procedure TGPriorityQueueTest.KSelectArray;
var
  a, r: TIntArray;
begin
  a := [];
  AssertTrue(TIntKSelect.GetKMax(a, 1) = nil);
  AssertTrue(TIntKSelect.GetKMin(a, 1) = nil);

  a := [1, 2];
  AssertTrue(TIntKSelect.GetKMax(a, -1) = nil);
  AssertTrue(TIntKSelect.GetKMin(a, -1) = nil);

  r := TIntKSelect.GetKMin(a, 3);
  AssertTrue(EqualSets(r, [1, 2]));

  r := TIntKSelect.GetKMax(a, 3);
  AssertTrue(EqualSets(r, [1, 2]));

  r := TIntKSelect.GetKMax(a, 1);
  AssertTrue(EqualSets(r, [2]));

  r := TIntKSelect.GetKMin(a, 1);
  AssertTrue(EqualSets(r, [1]));

  a := [1, 1, 2, 2, 3, 3, 4, 4];

  r := TIntKSelect.GetKMax(a, 3);
  AssertTrue(EqualSets(r, [3, 4, 4]));

  r := TIntKSelect.GetKMin(a, 3);
  AssertTrue(EqualSets(r, [1, 1, 2]));
end;

procedure TGPriorityQueueTest.KSelectEnum;
var
  e: IIntEnumerable;
  a, r: TIntArray;
begin
  a := [];
  e := TIntArrayCursor.Create(a);
  AssertTrue(TIntKSelect.GetKMax(e, 1) = nil);
  e := TIntArrayCursor.Create(a);
  AssertTrue(TIntKSelect.GetKMin(e, 1) = nil);

  a := [1, 2];
  e := TIntArrayCursor.Create(a);
  AssertTrue(TIntKSelect.GetKMax(e, -1) = nil);
  e := TIntArrayCursor.Create(a);
  AssertTrue(TIntKSelect.GetKMin(e, -1) = nil);

  e := TIntArrayCursor.Create(a);
  r := TIntKSelect.GetKMin(e, 3);
  AssertTrue(EqualSets(r, [1, 2]));

  e := TIntArrayCursor.Create(a);
  r := TIntKSelect.GetKMax(e, 3);
  AssertTrue(EqualSets(r, [1, 2]));

  e := TIntArrayCursor.Create(a);
  r := TIntKSelect.GetKMax(e, 1);
  AssertTrue(EqualSets(r, [2]));

  e := TIntArrayCursor.Create(a);
  r := TIntKSelect.GetKMin(e, 1);
  AssertTrue(EqualSets(r, [1]));

  a := [1, 1, 2, 2, 3, 3, 4, 4];

  e := TIntArrayCursor.Create(a);
  r := TIntKSelect.GetKMax(e, 3);
  AssertTrue(EqualSets(r, [3, 4, 4]));

  e := TIntArrayCursor.Create(a);
  r := TIntKSelect.GetKMin(e, 3);
  AssertTrue(EqualSets(r, [1, 1, 2]));
end;

procedure TGPriorityQueueTest.ComparableKSelectArray;
var
  a, r: TIntArray;
begin
  a := [];
  AssertTrue(TIntCompKSelect.GetKMax(a, 1) = nil);
  AssertTrue(TIntCompKSelect.GetKMin(a, 1) = nil);

  a := [1, 2];
  AssertTrue(TIntCompKSelect.GetKMax(a, -1) = nil);
  AssertTrue(TIntCompKSelect.GetKMin(a, -1) = nil);

  r := TIntCompKSelect.GetKMin(a, 3);
  AssertTrue(EqualSets(r, [1, 2]));

  r := TIntCompKSelect.GetKMax(a, 3);
  AssertTrue(EqualSets(r, [1, 2]));

  r := TIntCompKSelect.GetKMax(a, 1);
  AssertTrue(EqualSets(r, [2]));

  r := TIntCompKSelect.GetKMin(a, 1);
  AssertTrue(EqualSets(r, [1]));

  a := [1, 1, 2, 2, 3, 3, 4, 4];

  r := TIntCompKSelect.GetKMax(a, 3);
  AssertTrue(EqualSets(r, [3, 4, 4]));

  r := TIntCompKSelect.GetKMin(a, 3);
  AssertTrue(EqualSets(r, [1, 1, 2]));
end;

procedure TGPriorityQueueTest.ComparableKSelectEnum;
var
  e: IIntEnumerable;
  a, r: TIntArray;
begin
  a := [];
  e := TIntArrayCursor.Create(a);
  AssertTrue(TIntCompKSelect.GetKMax(e, 1) = nil);
  e := TIntArrayCursor.Create(a);
  AssertTrue(TIntCompKSelect.GetKMin(e, 1) = nil);

  a := [1, 2];
  e := TIntArrayCursor.Create(a);
  AssertTrue(TIntCompKSelect.GetKMax(e, -1) = nil);
  e := TIntArrayCursor.Create(a);
  AssertTrue(TIntCompKSelect.GetKMin(e, -1) = nil);

  e := TIntArrayCursor.Create(a);
  r := TIntCompKSelect.GetKMin(e, 3);
  AssertTrue(EqualSets(r, [1, 2]));

  e := TIntArrayCursor.Create(a);
  r := TIntCompKSelect.GetKMax(e, 3);
  AssertTrue(EqualSets(r, [1, 2]));

  e := TIntArrayCursor.Create(a);
  r := TIntCompKSelect.GetKMax(e, 1);
  AssertTrue(EqualSets(r, [2]));

  e := TIntArrayCursor.Create(a);
  r := TIntCompKSelect.GetKMin(e, 1);
  AssertTrue(EqualSets(r, [1]));

  a := [1, 1, 2, 2, 3, 3, 4, 4];

  e := TIntArrayCursor.Create(a);
  r := TIntCompKSelect.GetKMax(e, 3);
  AssertTrue(EqualSets(r, [3, 4, 4]));

  e := TIntArrayCursor.Create(a);
  r := TIntCompKSelect.GetKMin(e, 3);
  AssertTrue(EqualSets(r, [1, 1, 2]));
end;

procedure TGPriorityQueueTest.BasePairHeapEnqueueAll;
var
  q: TIntBasePairHeap;
  I: SizeInt;
  a: TIntArray;
begin
  System.SetLength(a, 100);
  for I := 1 to System.Length(a) do
    a[I - 1] := I * 1001;
  TIntHelper.RandomShuffle(a);
  q := TIntBasePairHeap.Create;
  try
    AssertTrue(q.IsEmpty);
    I := q.EnqueueAll(a);
    AssertTrue(I = System.Length(a));
    AssertTrue(q.Count = System.Length(a));
    I := 0;
    while q.NonEmpty do
      begin
        a[I] := q.Dequeue;
        Inc(I);
      end;
    AssertTrue(I = System.Length(a));
    AssertTrue(TIntHelper.IsStrictDescending(a));
  finally
    q.Free;
  end;
end;

procedure TGPriorityQueueTest.BasePairHeapUpdate;
var
  q: TIntBasePairHeap;
  I, J: SizeInt;
  Handle: THandle;
  a: TIntArray;
  h: THandleArray;
begin
  System.SetLength(a, 100);
  System.SetLength(h, System.Length(a));
  for I := 1 to System.Length(a) do
    a[I - 1] := I * 1001;
  TIntHelper.RandomShuffle(a);
  q := TIntBasePairHeap.Create;
  try
    J := 0;
    for I := 0 to System.High(a) do
      begin
        h[J] := q.Insert(a[I]);
        Inc(J);
      end;

    for I := 0 to System.High(a) do
      AssertTrue(q.ValueOf(h[I]) = a[I]);

    TIntHelper.RandomShuffle(a);

    for I := 0 to System.High(a) do
      {%H-}q.Update(h[I], a[I]);

    for I := 0 to System.High(a) do
      AssertTrue(q.ValueOf(h[I]) = a[I]);

    I := 0;
    while q.NonEmpty do
      begin
        Handle := q{%H-}.PeekHandle;
        a[I] := q.Remove(Handle);
        Inc(I);
      end;
    AssertTrue(I = System.Length(a));
    AssertTrue(TIntHelper.IsStrictDescending(a));
  finally
    q.Free;
  end;
end;

procedure TGPriorityQueueTest.BasePairHeapMerge;
var
  q, q1: TIntBasePairHeap;
  I, J, TestSize: SizeInt;
  a: TIntArray;
begin
  TestSize := 100;
  System.SetLength(a, TestSize);
  for I := 1 to System.Length(a) do
    a[I - 1] := I * 1001;
  q1 := TIntBasePairHeap.Create;
  q := TIntBasePairHeap.Create(a);
  try
    AssertTrue(q.Count = TestSize);
    J := Succ(TestSize);
    for I := 0 to System.High(a) do
      begin
        a[I] := J * 1001;
        Inc(J);
      end;
    AssertTrue(q1.EnqueueAll(a) = TestSize);
    AssertTrue(q1.Count = TestSize);
    AssertTrue(q.Merge(q) = 0);
    AssertTrue(q.Merge(q1) = TestSize);
    AssertTrue(q.Count = TestSize * 2);
    AssertTrue(q1.Count = 0);
    AssertTrue(q1.Capacity = 0);
    System.SetLength(a, 119);
    for I := 0 to System.High(a) do
      a[I] := q.Dequeue;
    q1.EnqueueAll(a);
    AssertTrue(q1.Merge(q) = TestSize * 2 - 119);
    AssertTrue(q1.Count = TestSize * 2);
    AssertTrue(q.Count = 0);
    AssertTrue(q.Capacity = 0);
    System.SetLength(a, q1.Count);
    I := 0;
    while q1.NonEmpty do
      begin
        a[I] := q1.Dequeue;
        Inc(I);
      end;
    AssertTrue(TIntHelper.IsStrictDescending(a));
  finally
    q.Free;
    q1.Free;
  end;
end;

procedure TGPriorityQueueTest.BasePairHeapReverse;
var
  q: TIntBasePairHeap;
  I: SizeInt;
  a, b: TIntArray;
begin
  System.SetLength(a, 100);
  for I := 1 to System.Length(a) do
    a[I - 1] := I * 1001;
  TIntHelper.RandomShuffle(a);
  q := TIntBasePairHeap.Create(a);
  try
    a := TIntHelper.CreateReverseCopy(q.ToArray);
    b := q.Reverse.ToArray;
    AssertTrue(TIntHelper.Same(a, b));
  finally
    q.Free;
  end;
end;

procedure TGPriorityQueueTest.ComparablePairHeapMaxEnqueueAll;
var
  q: TIntComparblePairHeapMax;
  I: SizeInt;
  a: TIntArray;
begin
  System.SetLength(a, 100);
  for I := 1 to System.Length(a) do
    a[I - 1] := I * 1001;
  TIntHelper.RandomShuffle(a);
  q := TIntComparblePairHeapMax.Create;
  try
    AssertTrue(q.IsEmpty);
    I := q.EnqueueAll(a);
    AssertTrue(I = System.Length(a));
    AssertTrue(q.Count = System.Length(a));
    I := 0;
    while q.NonEmpty do
      begin
        a[I] := q.Dequeue;
        Inc(I);
      end;
    AssertTrue(I = System.Length(a));
    AssertTrue(TIntHelper.IsStrictDescending(a));
  finally
    q.Free;
  end;
end;

procedure TGPriorityQueueTest.ComparablePairHeapMaxUpdate;
var
  q: TIntComparblePairHeapMax;
  I, J: SizeInt;
  Handle: THandle;
  a: TIntArray;
  h: THandleArray;
begin
  System.SetLength(a, 100);
  System.SetLength(h, System.Length(a));
  for I := 1 to System.Length(a) do
    a[I - 1] := I * 1001;
  TIntHelper.RandomShuffle(a);
  q := TIntComparblePairHeapMax.Create;
  try
    J := 0;
    for I := 0 to System.High(a) do
      begin
        h[J] := q.Insert(a[I]);
        Inc(J);
      end;

    for I := 0 to System.High(a) do
      AssertTrue(q.ValueOf(h[I]) = a[I]);

    TIntHelper.RandomShuffle(a);

    for I := 0 to System.High(a) do
      {%H-}q.Update(h[I], a[I]);

    for I := 0 to System.High(a) do
      AssertTrue(q.ValueOf(h[I]) = a[I]);

    I := 0;
    while q.NonEmpty do
      begin
        Handle := q{%H-}.PeekHandle;
        a[I] := q.Remove(Handle);
        Inc(I);
      end;
    AssertTrue(I = System.Length(a));
    AssertTrue(TIntHelper.IsStrictDescending(a));
  finally
    q.Free;
  end;
end;

procedure TGPriorityQueueTest.ComparablePairHeapMaxMerge;
var
  q, q1: TIntComparblePairHeapMax;
  I, J, TestSize: SizeInt;
  a: TIntArray;
begin
  TestSize := 100;
  System.SetLength(a, TestSize);
  for I := 1 to System.Length(a) do
    a[I - 1] := I * 1001;
  q1 := TIntComparblePairHeapMax.Create;
  q := TIntComparblePairHeapMax.Create(a);
  try
    AssertTrue(q.Count = TestSize);
    J := Succ(TestSize);
    for I := 0 to System.High(a) do
      begin
        a[I] := J * 1001;
        Inc(J);
      end;
    AssertTrue(q1.EnqueueAll(a) = TestSize);
    AssertTrue(q1.Count = TestSize);
    AssertTrue(q.Merge(q) = 0);
    AssertTrue(q.Merge(q1) = TestSize);
    AssertTrue(q.Count = TestSize * 2);
    AssertTrue(q1.Count = 0);
    AssertTrue(q1.Capacity = 0);
    System.SetLength(a, 119);
    for I := 0 to System.High(a) do
      a[I] := q.Dequeue;
    q1.EnqueueAll(a);
    AssertTrue(q1.Merge(q) = TestSize * 2 - 119);
    AssertTrue(q1.Count = TestSize * 2);
    AssertTrue(q.Count = 0);
    AssertTrue(q.Capacity = 0);
    System.SetLength(a, q1.Count);
    I := 0;
    while q1.NonEmpty do
      begin
        a[I] := q1.Dequeue;
        Inc(I);
      end;
    AssertTrue(TIntHelper.IsStrictDescending(a));
  finally
    q.Free;
    q1.Free;
  end;
end;

procedure TGPriorityQueueTest.ComparablePairHeapMaxReverse;
var
  q: TIntComparblePairHeapMax;
  I: SizeInt;
  a, b: TIntArray;
begin
  System.SetLength(a, 100);
  for I := 1 to System.Length(a) do
    a[I - 1] := I * 1001;
  TIntHelper.RandomShuffle(a);
  q := TIntComparblePairHeapMax.Create(a);
  try
    a := TIntHelper.CreateReverseCopy(q.ToArray);
    b := q.Reverse.ToArray;
    AssertTrue(TIntHelper.Same(a, b));
  finally
    q.Free;
  end;
end;

procedure TGPriorityQueueTest.ComparablePairHeapMinEnqueueAll;
var
  q: TIntComparblePairHeapMin;
  I: SizeInt;
  a: TIntArray;
begin
  System.SetLength(a, 100);
  for I := 1 to System.Length(a) do
    a[I - 1] := I * 1001;
  TIntHelper.RandomShuffle(a);
  q := TIntComparblePairHeapMin.Create;
  try
    AssertTrue(q.IsEmpty);
    I := q.EnqueueAll(a);
    AssertTrue(I = System.Length(a));
    AssertTrue(q.Count = System.Length(a));
    I := 0;
    while q.NonEmpty do
      begin
        a[I] := q.Dequeue;
        Inc(I);
      end;
    AssertTrue(I = System.Length(a));
    AssertTrue(TIntHelper.IsStrictAscending(a));
  finally
    q.Free;
  end;
end;

procedure TGPriorityQueueTest.ComparablePairHeapMinUpdate;
var
  q: TIntComparblePairHeapMin;
  I, J: SizeInt;
  Handle: THandle;
  a: TIntArray;
  h: THandleArray;
begin
  System.SetLength(a, 100);
  System.SetLength(h, System.Length(a));
  for I := 1 to System.Length(a) do
    a[I - 1] := I * 1001;
  TIntHelper.RandomShuffle(a);
  q := TIntComparblePairHeapMin.Create;
  try
    J := 0;
    for I := 0 to System.High(a) do
      begin
        h[J] := q.Insert(a[I]);
        Inc(J);
      end;

    for I := 0 to System.High(a) do
      AssertTrue(q.ValueOf(h[I]) = a[I]);

    TIntHelper.RandomShuffle(a);

    for I := 0 to System.High(a) do
      {%H-}q.Update(h[I], a[I]);

    for I := 0 to System.High(a) do
      AssertTrue(q.ValueOf(h[I]) = a[I]);

    I := 0;
    while q.NonEmpty do
      begin
        Handle := q{%H-}.PeekHandle;
        a[I] := q.Remove(Handle);
        Inc(I);
      end;
    AssertTrue(I = System.Length(a));
    AssertTrue(TIntHelper.IsStrictAscending(a));
  finally
    q.Free;
  end;
end;

procedure TGPriorityQueueTest.ComparablePairHeapMinMerge;
var
  q, q1: TIntComparblePairHeapMin;
  I, J, TestSize: SizeInt;
  a: TIntArray;
begin
  TestSize := 100;
  System.SetLength(a, TestSize);
  for I := 1 to System.Length(a) do
    a[I - 1] := I * 1001;
  q1 := TIntComparblePairHeapMin.Create;
  q := TIntComparblePairHeapMin.Create(a);
  try
    AssertTrue(q.Count = TestSize);
    J := Succ(TestSize);
    for I := 0 to System.High(a) do
      begin
        a[I] := J * 1001;
        Inc(J);
      end;
    AssertTrue(q1.EnqueueAll(a) = TestSize);
    AssertTrue(q1.Count = TestSize);
    AssertTrue(q.Merge(q) = 0);
    AssertTrue(q.Merge(q1) = TestSize);
    AssertTrue(q.Count = TestSize * 2);
    AssertTrue(q1.Count = 0);
    AssertTrue(q1.Capacity = 0);
    System.SetLength(a, 119);
    for I := 0 to System.High(a) do
      a[I] := q.Dequeue;
    q1.EnqueueAll(a);
    AssertTrue(q1.Merge(q) = TestSize * 2 - 119);
    AssertTrue(q1.Count = TestSize * 2);
    AssertTrue(q.Count = 0);
    AssertTrue(q.Capacity = 0);
    System.SetLength(a, q1.Count);
    I := 0;
    while q1.NonEmpty do
      begin
        a[I] := q1.Dequeue;
        Inc(I);
      end;
    AssertTrue(TIntHelper.IsStrictAscending(a));
  finally
    q.Free;
    q1.Free;
  end;
end;

procedure TGPriorityQueueTest.ComparablePairHeapMinReverse;
var
  q: TIntComparblePairHeapMin;
  I: SizeInt;
  a, b: TIntArray;
begin
  System.SetLength(a, 100);
  for I := 1 to System.Length(a) do
    a[I - 1] := I * 1001;
  TIntHelper.RandomShuffle(a);
  q := TIntComparblePairHeapMin.Create(a);
  try
    a := TIntHelper.CreateReverseCopy(q.ToArray);
    b := q.Reverse.ToArray;
    AssertTrue(TIntHelper.Same(a, b));
  finally
    q.Free;
  end;
end;

procedure TGPriorityQueueTest.RegularPairHeapEnqueueAll;
var
  q: TIntRegularPairHeap;
  I: SizeInt;
  a: TIntArray;
begin
  System.SetLength(a, 100);
  for I := 1 to System.Length(a) do
    a[I - 1] := I * 1001;
  TIntHelper.RandomShuffle(a);
  q := TIntRegularPairHeap.Create(@CmpLongInt);
  try
    AssertTrue(q.IsEmpty);
    I := q.EnqueueAll(a);
    AssertTrue(I = System.Length(a));
    AssertTrue(q.Count = System.Length(a));
    I := 0;
    while q.NonEmpty do
      begin
        a[I] := q.Dequeue;
        Inc(I);
      end;
    AssertTrue(I = System.Length(a));
    AssertTrue(TIntHelper.IsStrictDescending(a));
  finally
    q.Free;
  end;
end;

procedure TGPriorityQueueTest.RegularPairHeapUpdate;
var
  q: TIntRegularPairHeap;
  I, J: SizeInt;
  Handle: THandle;
  a: TIntArray;
  h: THandleArray;
begin
  System.SetLength(a, 100);
  System.SetLength(h, System.Length(a));
  for I := 1 to System.Length(a) do
    a[I - 1] := I * 1001;
  TIntHelper.RandomShuffle(a);
  q := TIntRegularPairHeap.Create(@CmpLongInt);
  try
    J := 0;
    for I := 0 to System.High(a) do
      begin
        h[J] := q.Insert(a[I]);
        Inc(J);
      end;

    for I := 0 to System.High(a) do
      AssertTrue(q.ValueOf(h[I]) = a[I]);

    TIntHelper.RandomShuffle(a);

    for I := 0 to System.High(a) do
      {%H-}q.Update(h[I], a[I]);

    for I := 0 to System.High(a) do
      AssertTrue(q.ValueOf(h[I]) = a[I]);

    I := 0;
    while q.NonEmpty do
      begin
        Handle := q{%H-}.PeekHandle;
        a[I] := q.Remove(Handle);
        Inc(I);
      end;
    AssertTrue(I = System.Length(a));
    AssertTrue(TIntHelper.IsStrictDescending(a));
  finally
    q.Free;
  end;
end;

procedure TGPriorityQueueTest.RegularPairHeapMerge;
var
  q, q1: TIntRegularPairHeap;
  I, J, TestSize: SizeInt;
  a: TIntArray;
begin
  TestSize := 100;
  System.SetLength(a, TestSize);
  for I := 1 to System.Length(a) do
    a[I - 1] := I * 1001;
  q1 := TIntRegularPairHeap.Create(@CmpLongInt);
  q := TIntRegularPairHeap.Create(a, @CmpLongInt);
  try
    AssertTrue(q.Count = TestSize);
    J := Succ(TestSize);
    for I := 0 to System.High(a) do
      begin
        a[I] := J * 1001;
        Inc(J);
      end;
    AssertTrue(q1.EnqueueAll(a) = TestSize);
    AssertTrue(q1.Count = TestSize);
    AssertTrue(q.Merge(q) = 0);
    AssertTrue(q.Merge(q1) = TestSize);
    AssertTrue(q.Count = TestSize * 2);
    AssertTrue(q1.Count = 0);
    AssertTrue(q1.Capacity = 0);
    System.SetLength(a, 119);
    for I := 0 to System.High(a) do
      a[I] := q.Dequeue;
    q1.EnqueueAll(a);
    AssertTrue(q1.Merge(q) = TestSize * 2 - 119);
    AssertTrue(q1.Count = TestSize * 2);
    AssertTrue(q.Count = 0);
    AssertTrue(q.Capacity = 0);
    System.SetLength(a, q1.Count);
    I := 0;
    while q1.NonEmpty do
      begin
        a[I] := q1.Dequeue;
        Inc(I);
      end;
    AssertTrue(TIntHelper.IsStrictDescending(a));
  finally
    q.Free;
    q1.Free;
  end;
end;

procedure TGPriorityQueueTest.RegularPairHeapReverse;
var
  q: TIntRegularPairHeap;
  I: SizeInt;
  a, b: TIntArray;
begin
  System.SetLength(a, 100);
  for I := 1 to System.Length(a) do
    a[I - 1] := I * 1001;
  TIntHelper.RandomShuffle(a);
  q := TIntRegularPairHeap.Create(a, @CmpLongInt);
  try
    a := TIntHelper.CreateReverseCopy(q.ToArray);
    b := q.Reverse.ToArray;
    AssertTrue(TIntHelper.Same(a, b));
  finally
    q.Free;
  end;
end;

procedure TGPriorityQueueTest.DefaultRegularPairHeap1;
var
  q: TIntRegularPairHeap;
  I: SizeInt;
  a: TIntArray;
begin
  System.SetLength(a, 100);
  for I := 1 to System.Length(a) do
    a[I - 1] := I * 1001;
  TIntHelper.RandomShuffle(a);
  q := TIntRegularPairHeap.Create;
  try
    AssertTrue(q.IsEmpty);
    I := q.EnqueueAll(a);
    AssertTrue(I = System.Length(a));
    AssertTrue(q.Count = System.Length(a));
    I := 0;
    while q.NonEmpty do
      begin
        a[I] := q.Dequeue;
        Inc(I);
      end;
    AssertTrue(I = System.Length(a));
    AssertTrue(TIntHelper.IsStrictDescending(a));
  finally
    q.Free;
  end;
end;

procedure TGPriorityQueueTest.DefaultRegularPairHeap2;
var
  q: TIntRegularPairHeap;
  I, J: SizeInt;
  Handle: THandle;
  a: TIntArray;
  h: THandleArray;
begin
  System.SetLength(a, 100);
  System.SetLength(h, System.Length(a));
  for I := 1 to System.Length(a) do
    a[I - 1] := I * 1001;
  TIntHelper.RandomShuffle(a);
  q := TIntRegularPairHeap.Create;
  try
    J := 0;
    for I := 0 to System.High(a) do
      begin
        h[J] := q.Insert(a[I]);
        Inc(J);
      end;

    for I := 0 to System.High(a) do
      AssertTrue(q.ValueOf(h[I]) = a[I]);

    TIntHelper.RandomShuffle(a);

    for I := 0 to System.High(a) do
      {%H-}q.Update(h[I], a[I]);

    for I := 0 to System.High(a) do
      AssertTrue(q.ValueOf(h[I]) = a[I]);

    I := 0;
    while q.NonEmpty do
      begin
        Handle := q{%H-}.PeekHandle;
        a[I] := q.Remove(Handle);
        Inc(I);
      end;
    AssertTrue(I = System.Length(a));
    AssertTrue(TIntHelper.IsStrictDescending(a));
  finally
    q.Free;
  end;
end;

procedure TGPriorityQueueTest.DefaultRegularPairHeap3;
var
  q, q1: TIntRegularPairHeap;
  I, J: SizeInt;
  a: TIntArray;
begin
  System.SetLength(a, 100);
  for I := 1 to System.Length(a) do
    a[I - 1] := I * 1001;
  q := TIntRegularPairHeap.Create(a, specialize TGDefaults<Integer>.Less);
  try
    AssertTrue(q.Count = System.Length(a));
    J := Succ(System.Length(a));
    for I := 0 to System.High(a) do
      begin
        a[I] := J * 1001;
        Inc(J);
      end;
    q1 := TIntRegularPairHeap.Create(a, @CmpLongInt);
    AssertTrue(q1.Count = System.Length(a));
    q.Merge(q1);
    q1.Free;
    AssertTrue(q.Count = System.Length(a) * 2);
    System.SetLength(a, q.Count);
    I := 0;
    while q.NonEmpty do
      begin
        a[I] := q.Dequeue;
        Inc(I);
      end;
    AssertTrue(I = System.Length(a));
    AssertTrue(TIntHelper.IsStrictDescending(a));
  finally
    q.Free;
  end;
end;

procedure TGPriorityQueueTest.DelegatedPairHeapEnqueueAll;
var
  q: TIntDelegatedPairHeap;
  I: SizeInt;
  a: TIntArray;
begin
  System.SetLength(a, 100);
  for I := 1 to System.Length(a) do
    a[I - 1] := I * 1001;
  TIntHelper.RandomShuffle(a);
  q := TIntDelegatedPairHeap.Create(@LongIntCmp);
  try
    AssertTrue(q.IsEmpty);
    I := q.EnqueueAll(a);
    AssertTrue(I = System.Length(a));
    AssertTrue(q.Count = System.Length(a));
    I := 0;
    while q.NonEmpty do
      begin
        a[I] := q.Dequeue;
        Inc(I);
      end;
    AssertTrue(I = System.Length(a));
    AssertTrue(TIntHelper.IsStrictDescending(a));
  finally
    q.Free;
  end;
end;

procedure TGPriorityQueueTest.DelegatedPairHeapUpdate;
var
  q: TIntDelegatedPairHeap;
  I, J: SizeInt;
  Handle: THandle;
  a: TIntArray;
  h: THandleArray;
begin
  System.SetLength(a, 100);
  System.SetLength(h, System.Length(a));
  for I := 1 to System.Length(a) do
    a[I - 1] := I * 1001;
  TIntHelper.RandomShuffle(a);
  q := TIntDelegatedPairHeap.Create(@LongIntCmp);
  try
    J := 0;
    for I := 0 to System.High(a) do
      begin
        h[J] := q.Insert(a[I]);
        Inc(J);
      end;

    for I := 0 to System.High(a) do
      AssertTrue(q.ValueOf(h[I]) = a[I]);

    TIntHelper.RandomShuffle(a);

    for I := 0 to System.High(a) do
      {%H-}q.Update(h[I], a[I]);

    for I := 0 to System.High(a) do
      AssertTrue(q.ValueOf(h[I]) = a[I]);

    I := 0;
    while q.NonEmpty do
      begin
        Handle := q{%H-}.PeekHandle;
        a[I] := q.Remove(Handle);
        Inc(I);
      end;
    AssertTrue(I = System.Length(a));
    AssertTrue(TIntHelper.IsStrictDescending(a));
  finally
    q.Free;
  end;
end;

procedure TGPriorityQueueTest.DelegatedPairHeapMerge;
var
  q, q1: TIntDelegatedPairHeap;
  I, J, TestSize: SizeInt;
  a: TIntArray;
begin
  TestSize := 100;
  System.SetLength(a, TestSize);
  for I := 1 to System.Length(a) do
    a[I - 1] := I * 1001;
  q1 := TIntDelegatedPairHeap.Create(@LongIntCmp);
  q := TIntDelegatedPairHeap.Create(a, @LongIntCmp);
  try
    AssertTrue(q.Count = TestSize);
    J := Succ(TestSize);
    for I := 0 to System.High(a) do
      begin
        a[I] := J * 1001;
        Inc(J);
      end;
    AssertTrue(q1.EnqueueAll(a) = TestSize);
    AssertTrue(q1.Count = TestSize);
    AssertTrue(q.Merge(q) = 0);
    AssertTrue(q.Merge(q1) = TestSize);
    AssertTrue(q.Count = TestSize * 2);
    AssertTrue(q1.Count = 0);
    AssertTrue(q1.Capacity = 0);
    System.SetLength(a, 119);
    for I := 0 to System.High(a) do
      a[I] := q.Dequeue;
    q1.EnqueueAll(a);
    AssertTrue(q1.Merge(q) = TestSize * 2 - 119);
    AssertTrue(q1.Count = TestSize * 2);
    AssertTrue(q.Count = 0);
    AssertTrue(q.Capacity = 0);
    System.SetLength(a, q1.Count);
    I := 0;
    while q1.NonEmpty do
      begin
        a[I] := q1.Dequeue;
        Inc(I);
      end;
    AssertTrue(TIntHelper.IsStrictDescending(a));
  finally
    q.Free;
    q1.Free;
  end;
end;

procedure TGPriorityQueueTest.DelegatedPairHeapReverse;
var
  q: TIntDelegatedPairHeap;
  I: SizeInt;
  a, b: TIntArray;
begin
  System.SetLength(a, 100);
  for I := 1 to System.Length(a) do
    a[I - 1] := I * 1001;
  TIntHelper.RandomShuffle(a);
  q := TIntDelegatedPairHeap.Create(a, @LongIntCmp);
  try
    a := TIntHelper.CreateReverseCopy(q.ToArray);
    b := q.Reverse.ToArray;
    AssertTrue(TIntHelper.Same(a, b));
  finally
    q.Free;
  end;
end;

procedure TGPriorityQueueTest.DefaultDelegatedPairHeap1;
var
  q: TIntDelegatedPairHeap;
  I: SizeInt;
  a: TIntArray;
begin
  System.SetLength(a, 100);
  for I := 1 to System.Length(a) do
    a[I - 1] := I * 1001;
  TIntHelper.RandomShuffle(a);
  q := TIntDelegatedPairHeap.Create;
  try
    AssertTrue(q.IsEmpty);
    I := q.EnqueueAll(a);
    AssertTrue(I = System.Length(a));
    AssertTrue(q.Count = System.Length(a));
    I := 0;
    while q.NonEmpty do
      begin
        a[I] := q.Dequeue;
        Inc(I);
      end;
    AssertTrue(I = System.Length(a));
    AssertTrue(TIntHelper.IsStrictDescending(a));
  finally
    q.Free;
  end;
end;

procedure TGPriorityQueueTest.DefaultDelegatedPairHeap2;
var
  q: TIntDelegatedPairHeap;
  I, J: SizeInt;
  Handle: THandle;
  a: TIntArray;
  h: THandleArray;
begin
  System.SetLength(a, 100);
  System.SetLength(h, System.Length(a));
  for I := 1 to System.Length(a) do
    a[I - 1] := I * 1001;
  TIntHelper.RandomShuffle(a);
  q := TIntDelegatedPairHeap.Create;
  try
    J := 0;
    for I := 0 to System.High(a) do
      begin
        h[J] := q.Insert(a[I]);
        Inc(J);
      end;

    for I := 0 to System.High(a) do
      AssertTrue(q.ValueOf(h[I]) = a[I]);

    TIntHelper.RandomShuffle(a);

    for I := 0 to System.High(a) do
      {%H-}q.Update(h[I], a[I]);

    for I := 0 to System.High(a) do
      AssertTrue(q.ValueOf(h[I]) = a[I]);

    I := 0;
    while q.NonEmpty do
      begin
        Handle := q{%H-}.PeekHandle;
        a[I] := q.Remove(Handle);
        Inc(I);
      end;
    AssertTrue(I = System.Length(a));
    AssertTrue(TIntHelper.IsStrictDescending(a));
  finally
    q.Free;
  end;
end;

procedure TGPriorityQueueTest.DefaultDelegatedPairHeap3;
var
  q, q1: TIntDelegatedPairHeap;
  I, J: SizeInt;
  a: TIntArray;
begin
  System.SetLength(a, 100);
  for I := 1 to System.Length(a) do
    a[I - 1] := I * 1001;
  q := TIntDelegatedPairHeap.Create(a, specialize TGDefaults<Integer>.OnLess);
  try
    AssertTrue(q.Count = System.Length(a));
    J := Succ(System.Length(a));
    for I := 0 to System.High(a) do
      begin
        a[I] := J * 1001;
        Inc(J);
      end;
    q1 := TIntDelegatedPairHeap.Create(a, @LongIntCmp);
    AssertTrue(q1.Count = System.Length(a));
    q.Merge(q1);
    q1.Free;
    AssertTrue(q.Count = System.Length(a) * 2);
    System.SetLength(a, q.Count);
    I := 0;
    while q.NonEmpty do
      begin
        a[I] := q.Dequeue;
        Inc(I);
      end;
    AssertTrue(I = System.Length(a));
    AssertTrue(TIntHelper.IsStrictDescending(a));
  finally
    q.Free;
  end;
end;

procedure TGPriorityQueueTest.LitePairHeapEnqueue;
var
  q: TLiteIntPairingHeap;
  I: SizeInt;
  a: TIntArray;
begin
  System.SetLength(a, 100);
  for I := 1 to System.Length(a) do
    a[I - 1] := I * 1001;
  TIntHelper.RandomShuffle(a);
  AssertTrue({%H-}q.IsEmpty);
  for I in a do
    {%H-}q.Enqueue(I);
  AssertTrue(q.Count = System.Length(a));
  I := 0;
  while q.NonEmpty do
    begin
      a[I] := q.Dequeue;
      Inc(I);
    end;
  AssertTrue(I = System.Length(a));
  AssertTrue(TIntHelper.IsStrictDescending(a));
end;

procedure TGPriorityQueueTest.LitePairHeapUpdate;
var
  q: TLiteIntPairingHeap;
  I, J: SizeInt;
  Handle: THandle;
  a: TIntArray;
  h: THandleArray;
begin
  System.SetLength(a, 100);
  System.SetLength(h, System.Length(a));
  for I := 1 to System.Length(a) do
    a[I - 1] := I * 1001;
  TIntHelper.RandomShuffle(a);
  J := 0;
  for I := 0 to System.High(a) do
    begin
      h[J] := q{%H-}.Insert(a[I]);
      Inc(J);
    end;

  for I := 0 to System.High(a) do
    AssertTrue(q.ValueOf(h[I]) = a[I]);

  TIntHelper.RandomShuffle(a);

  for I := 0 to System.High(a) do
    {%H-}q.Update(h[I], a[I]);

  for I := 0 to System.High(a) do
    AssertTrue(q.ValueOf(h[I]) = a[I]);

  I := 0;
  while q.NonEmpty do
    begin
      Handle := q{%H-}.PeekHandle;
      a[I] := q.Remove(Handle);
      Inc(I);
    end;
  AssertTrue(I = System.Length(a));
  AssertTrue(TIntHelper.IsStrictDescending(a));
end;

procedure TGPriorityQueueTest.LitePairHeapMerge;
var
  q, q1: TLiteIntPairingHeap;
  I, TestSize: SizeInt;
  a: TIntArray;
begin
  TestSize := 100;
  for I := 1 to TestSize do
    {%H-}q.Enqueue(I * 1001);
  AssertTrue(q.Count = TestSize);
  for I := Succ(TestSize) to TestSize * 2 do
    {%H-}q1.Enqueue(I * 1001);
  AssertTrue(q1.Count = TestSize);
  AssertTrue(q.Merge(q) = 0);
  AssertTrue(q.Merge(q1) = TestSize);
  AssertTrue(q.Count = TestSize * 2);
  AssertTrue(q1.Count = 0);
  for I := 1 to 119 do
    {%H-}q1.Enqueue(q.Dequeue);
  AssertTrue(q1.Merge(q) = TestSize * 2 - 119);
  AssertTrue(q1.Count = TestSize * 2);
  AssertTrue(q.Count = 0);
  System.SetLength(a, q1.Count);
  I := 0;
  while q1.NonEmpty do
    begin
      a[I] := q1.Dequeue;
      Inc(I);
    end;
  AssertTrue(TIntHelper.IsStrictDescending(a));
end;

procedure TGPriorityQueueTest.LitePairHeapReverse;
var
  q: TLiteIntPairingHeap;
  I, J: SizeInt;
  a: TIntArray;
begin
  System.SetLength(a, 100);
  for I := 1 to System.Length(a) do
    a[I - 1] := I * 1001;
  TIntHelper.RandomShuffle(a);
  for I in a do
    {%H-}q.Enqueue(I);
  a := TIntHelper.CreateReverseCopy(q.ToArray);
  J := 0;
  for I in q.Reverse do
    begin
      AssertTrue(I = a[J]);
      Inc(J);
    end;
end;

procedure TGPriorityQueueTest.LitePairHeapAssign;
var
  h1, h2: TLiteIntPairingHeap;
begin
  {%H-}h1.Enqueue(1);
  h1.Enqueue(2);
  h1.Enqueue(3);
  AssertTrue(h1.Count = 3);
  h2 := h1;
  AssertTrue(h2.Count = 3);
  h1.Clear;
  AssertTrue(h1.IsEmpty);
  AssertTrue(h2.Count = 3);
  AssertTrue(h2.Dequeue = 3);
  AssertTrue(h2.Dequeue = 2);
  AssertTrue(h2.Dequeue = 1);
  h2.Enqueue(4);
  h2.Enqueue(5);
  AssertTrue(h2.Count = 2);
  h1.Enqueue(6);
  AssertTrue(h1.Count = 1);
  h2 := h1;
  AssertTrue(h1.Dequeue = 6);
  AssertTrue(h1.IsEmpty);
  AssertTrue(h2.Count = 1);
  AssertTrue(h2.Dequeue = 6);
end;

procedure TGPriorityQueueTest.LitePairHeapPassByValue;
  procedure Test(aHeap: TLiteIntPairingHeap);
  begin
    aHeap.Enqueue(10);
    aHeap.Enqueue(11);
    aHeap.Enqueue(12);
  end;
var
  h: TLiteIntPairingHeap;
begin
  AssertTrue({%H-}h.IsEmpty);
  h.Enqueue(3);
  AssertTrue(h.Count = 1);
  Test(h);
  AssertTrue(h.Count = 1);
  AssertTrue(h.Dequeue = 3);
  AssertTrue(h.IsEmpty);
end;

procedure TGPriorityQueueTest.LiteCompareblePairHeapEnqueue;
var
  q: TLiteIntPairHeapMin;
  I: SizeInt;
  a: TIntArray;
begin
  System.SetLength(a, 100);
  for I := 1 to System.Length(a) do
    a[I - 1] := I * 1001;
  TIntHelper.RandomShuffle(a);
  AssertTrue({%H-}q.IsEmpty);
  for I in a do
    q.Enqueue(I);
  AssertTrue(q.Count = System.Length(a));
  I := 0;
  while q.NonEmpty do
    begin
      a[I] := q.Dequeue;
      Inc(I);
    end;
  AssertTrue(I = System.Length(a));
  AssertTrue(TIntHelper.IsStrictAscending(a));
end;

procedure TGPriorityQueueTest.LiteCompareblePairHeapEnqueue1;
var
  q: specialize TGLiteComparablePairHeapMin<TRec>;
  I: SizeInt;
  a: array of TRec;
begin
  System.SetLength(a, 100);
  for I := 1 to System.Length(a) do
    a[I - 1] := TRec.Create(I * 1001, 0);
  specialize TGComparableArrayHelper<TRec>.RandomShuffle(a);
  AssertTrue({%H-}q.IsEmpty);
  for I := 0 to High(a) do
    q.Enqueue(a[I]);
  AssertTrue(q.Count = System.Length(a));
  I := 0;
  while q.NonEmpty do
    begin
      a[I] := q.Dequeue;
      Inc(I);
    end;
  AssertTrue(I = System.Length(a));
  AssertTrue(specialize TGComparableArrayHelper<TRec>.IsStrictAscending(a));
end;

procedure TGPriorityQueueTest.LiteCompareblePairHeapUpdate;
var
  q: TLiteIntPairHeapMin;
  I, J: SizeInt;
  Handle: THandle;
  a: TIntArray;
  h: THandleArray;
begin
  System.SetLength(a, 100);
  System.SetLength(h, System.Length(a));
  for I := 1 to System.Length(a) do
    a[I - 1] := I * 1001;
  TIntHelper.RandomShuffle(a);
  J := 0;
  for I := 0 to System.High(a) do
    begin
      h[J] := q{%H-}.Insert(a[I]);
      Inc(J);
    end;

  for I := 0 to System.High(a) do
    AssertTrue(q.ValueOf(h[I]) = a[I]);

  TIntHelper.RandomShuffle(a);

  for I := 0 to System.High(a) do
    {%H-}q.Update(h[I], a[I]);

  for I := 0 to System.High(a) do
    AssertTrue(q.ValueOf(h[I]) = a[I]);

  I := 0;
  while q.NonEmpty do
    begin
      Handle := q{%H-}.PeekHandle;
      a[I] := q.Remove(Handle);
      Inc(I);
    end;
  AssertTrue(I = System.Length(a));
  AssertTrue(TIntHelper.IsStrictAscending(a));
end;

procedure TGPriorityQueueTest.LiteCompareblePairHeapMerge;
var
  q, q1: TLiteIntPairHeapMin;
  I, TestSize: SizeInt;
  a: TIntArray;
begin
  TestSize := 100;
  for I := 1 to TestSize do
    {%H-}q.Enqueue(I * 1001);
  AssertTrue(q.Count = TestSize);
  for I := Succ(TestSize) to TestSize * 2 do
    {%H-}q1.Enqueue(I * 1001);
  AssertTrue(q1.Count = TestSize);
  AssertTrue(q.Merge(q) = 0);
  AssertTrue(q.Merge(q1) = TestSize);
  AssertTrue(q.Count = TestSize * 2);
  AssertTrue(q1.Count = 0);
  for I := 1 to 119 do
    {%H-}q1.Enqueue(q.Dequeue);
  AssertTrue(q1.Merge(q) = TestSize * 2 - 119);
  AssertTrue(q1.Count = TestSize * 2);
  AssertTrue(q.Count = 0);
  System.SetLength(a, q1.Count);
  I := 0;
  while q1.NonEmpty do
    begin
      a[I] := q1.Dequeue;
      Inc(I);
    end;
  AssertTrue(TIntHelper.IsStrictAscending(a));
end;

procedure TGPriorityQueueTest.LiteCompareblePairHeapReverse;
var
  q: TLiteIntPairHeapMin;
  I, J: SizeInt;
  a: TIntArray;
begin
  System.SetLength(a, 100);
  for I := 1 to System.Length(a) do
    a[I - 1] := I * 1001;
  TIntHelper.RandomShuffle(a);
  for I in a do
    {%H-}q.Enqueue(I);
  a := TIntHelper.CreateReverseCopy(q.ToArray);
  J := 0;
  for I in q.Reverse do
    begin
      AssertTrue(I = a[J]);
      Inc(J);
    end;
end;

procedure TGPriorityQueueTest.LiteCompareblePairHeapAssign;
var
  h1, h2: TLiteIntPairHeapMin;
begin
  {%H-}h1.Enqueue(1);
  h1.Enqueue(2);
  h1.Enqueue(3);
  AssertTrue(h1.Count = 3);
  h2 := h1;
  AssertTrue(h2.Count = 3);
  h1.Clear;
  AssertTrue(h1.IsEmpty);
  AssertTrue(h2.Count = 3);
  AssertTrue(h2.Dequeue = 1);
  AssertTrue(h2.Dequeue = 2);
  AssertTrue(h2.Dequeue = 3);
  h2.Enqueue(4);
  h2.Enqueue(5);
  AssertTrue(h2.Count = 2);
  h1.Enqueue(6);
  AssertTrue(h1.Count = 1);
  h2 := h1;
  AssertTrue(h1.Dequeue = 6);
  AssertTrue(h1.IsEmpty);
  AssertTrue(h2.Count = 1);
  AssertTrue(h2.Dequeue = 6);
end;

procedure TGPriorityQueueTest.LiteCompareblePairHeapPassByValue;
  procedure Test(aHeap: TLiteIntPairHeapMin);
  begin
    aHeap.Enqueue(10);
    aHeap.Enqueue(11);
    aHeap.Enqueue(12);
  end;
var
  h: TLiteIntPairHeapMin;
begin
  AssertTrue({%H-}h.IsEmpty);
  h.Enqueue(3);
  AssertTrue(h.Count = 1);
  Test(h);
  AssertTrue(h.Count = 1);
  AssertTrue(h.Dequeue = 3);
  AssertTrue(h.IsEmpty);
end;

initialization
  RegisterTest(TGPriorityQueueTest);
end.

