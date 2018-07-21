unit LGPriorityQueueTest;

{$mode objfpc}{$H+}

interface
uses
  SysUtils, fpcunit, testregistry,
  LGUtils,
  LGHelpers,
  LGPriorityQueue,
  LGArrayHelpers;

type

  { TGPriorityQueueTest }

  TGPriorityQueueTest = class(TTestCase)
  private
  type
    TIntBaseBinHeap          = specialize TGBinHeap<Integer>;
    TIntComparbleBinHeapMax  = specialize TGComparableBinHeapMax<Integer>;
    TIntComparbleBinHeapMin  = specialize TGComparableBinHeapMin<Integer>;
    TIntRegularBinHeap       = specialize TGRegularBinHeap<Integer>;
    TIntDelegatedBinHeap     = specialize TGDelegatedBinHeap<Integer>;

    TLiteIntBinHeap          = specialize TGLiteBinHeap<Integer, Integer>;
    TLiteIntBinHeapMin       = specialize TGLiteComparableBinHeapMin<Integer>;

    TIntBasePairHeap         = specialize TGPairingHeap<Integer>;
    TIntComparblePairHeapMax = specialize TGComparablePairHeapMax<Integer>;
    TIntComparblePairHeapMin = specialize TGComparablePairHeapMin<Integer>;
    TIntRegularPairHeap      = specialize TGRegularPairHeap<Integer>;
    TIntDelegatedPairHeap    = specialize TGDelegatedPairHeap<Integer>;

    TLiteIntPairingHeap      = specialize TGLitePairingHeap<Integer, Integer>;
    TLiteIntPairHeapMin      = specialize TGLiteComparablePairHeapMin<Integer>;

    TIntHelper               = specialize TGComparableArrayHelper<Integer>;
    TIntArray                = TIntBaseBinHeap.TArray;
    THandleArray             = array of THandle;
    function LongIntCmp(constref L, R: LongInt): SizeInt;
  published
    procedure BaseBinHeap;
    procedure BaseBinHeapReverse;
    procedure ComparableBinHeapMax;
    procedure ComparableBinHeapMaxReverse;
    procedure ComparableBinHeapMin;
    procedure ComparableBinHeapMinReverse;
    procedure RegularBinHeap;
    procedure DefaultRegularBinHeap;
    procedure RegularBinHeapReverse;
    procedure DelegatedBinHeap;
    procedure DefaultDelegatedBinHeap;
    procedure DelegatedBinHeapReverse;

    procedure LiteBinHeap;
    procedure LiteBinHeapReverse;
    procedure LiteComparableBinHeap;
    procedure LiteComparableBinHeapReverse;

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

    procedure LiteCompareblePairHeapEnqueue;
    procedure LiteCompareblePairHeapUpdate;
    procedure LiteCompareblePairHeapMerge;
    procedure LiteCompareblePairHeapReverse;
  end;

implementation

function TGPriorityQueueTest.LongIntCmp(constref L, R: LongInt): SizeInt;
begin
{$IFDEF CPU64}
  Result := SizeInt(L) - SizeInt(R);
{$ELSE CPU64}
  if L > R then
    Result := 1
  else
    if R > L then
      Result := -1
    else
      Result := 0;
{$ENDIF CPU64}
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

function CmpLongInt(constref L, R: LongInt): SizeInt;
begin
{$IFDEF CPU64}
  Result := SizeInt(L) - SizeInt(R);
{$ELSE CPU64}
  if L > R then
    Result := 1
  else
    if R > L then
      Result := -1
    else
      Result := 0;
{$ENDIF CPU64}
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
      AssertTrue(q.Value(h[I]) = a[I]);

    TIntHelper.RandomShuffle(a);

    for I := 0 to System.High(a) do
      {%H-}q.Update(h[I], a[I]);

    for I := 0 to System.High(a) do
      AssertTrue(q.Value(h[I]) = a[I]);

    I := 0;
    while q.NonEmpty do
      begin
        Handle := q{%H-}.HPeek;
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
      AssertTrue(q.Value(h[I]) = a[I]);

    TIntHelper.RandomShuffle(a);

    for I := 0 to System.High(a) do
      {%H-}q.Update(h[I], a[I]);

    for I := 0 to System.High(a) do
      AssertTrue(q.Value(h[I]) = a[I]);

    I := 0;
    while q.NonEmpty do
      begin
        Handle := q{%H-}.HPeek;
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
      AssertTrue(q.Value(h[I]) = a[I]);

    TIntHelper.RandomShuffle(a);

    for I := 0 to System.High(a) do
      {%H-}q.Update(h[I], a[I]);

    for I := 0 to System.High(a) do
      AssertTrue(q.Value(h[I]) = a[I]);

    I := 0;
    while q.NonEmpty do
      begin
        Handle := q{%H-}.HPeek;
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
      AssertTrue(q.Value(h[I]) = a[I]);

    TIntHelper.RandomShuffle(a);

    for I := 0 to System.High(a) do
      {%H-}q.Update(h[I], a[I]);

    for I := 0 to System.High(a) do
      AssertTrue(q.Value(h[I]) = a[I]);

    I := 0;
    while q.NonEmpty do
      begin
        Handle := q{%H-}.HPeek;
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
      AssertTrue(q.Value(h[I]) = a[I]);

    TIntHelper.RandomShuffle(a);

    for I := 0 to System.High(a) do
      {%H-}q.Update(h[I], a[I]);

    for I := 0 to System.High(a) do
      AssertTrue(q.Value(h[I]) = a[I]);

    I := 0;
    while q.NonEmpty do
      begin
        Handle := q{%H-}.HPeek;
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
  q := TIntRegularPairHeap.Create(a, specialize TGDefaults<Integer>.Compare);
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
      AssertTrue(q.Value(h[I]) = a[I]);

    TIntHelper.RandomShuffle(a);

    for I := 0 to System.High(a) do
      {%H-}q.Update(h[I], a[I]);

    for I := 0 to System.High(a) do
      AssertTrue(q.Value(h[I]) = a[I]);

    I := 0;
    while q.NonEmpty do
      begin
        Handle := q{%H-}.HPeek;
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
      AssertTrue(q.Value(h[I]) = a[I]);

    TIntHelper.RandomShuffle(a);

    for I := 0 to System.High(a) do
      {%H-}q.Update(h[I], a[I]);

    for I := 0 to System.High(a) do
      AssertTrue(q.Value(h[I]) = a[I]);

    I := 0;
    while q.NonEmpty do
      begin
        Handle := q{%H-}.HPeek;
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
  q := TIntDelegatedPairHeap.Create(a, specialize TGDefaults<Integer>.OnCompare);
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
    AssertTrue(q.Value(h[I]) = a[I]);

  TIntHelper.RandomShuffle(a);

  for I := 0 to System.High(a) do
    {%H-}q.Update(h[I], a[I]);

  for I := 0 to System.High(a) do
    AssertTrue(q.Value(h[I]) = a[I]);

  I := 0;
  while q.NonEmpty do
    begin
      Handle := q{%H-}.HPeek;
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
  AssertTrue(q1.Capacity = 0);
  for I := 1 to 119 do
    {%H-}q1.Enqueue(q.Dequeue);
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
    {%H-}q.Enqueue(I);
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
    AssertTrue(q.Value(h[I]) = a[I]);

  TIntHelper.RandomShuffle(a);

  for I := 0 to System.High(a) do
    {%H-}q.Update(h[I], a[I]);

  for I := 0 to System.High(a) do
    AssertTrue(q.Value(h[I]) = a[I]);

  I := 0;
  while q.NonEmpty do
    begin
      Handle := q{%H-}.HPeek;
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
  AssertTrue(q1.Capacity = 0);
  for I := 1 to 119 do
    {%H-}q1.Enqueue(q.Dequeue);
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

initialization
  RegisterTest(TGPriorityQueueTest);
end.

