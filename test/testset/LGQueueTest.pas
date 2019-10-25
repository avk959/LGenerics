unit LGQueueTest;

{$mode objfpc}{$H+}
{$MODESWITCH NESTEDPROCVARS}

interface
uses
  SysUtils, fpcunit, testregistry,
  LGUtils,
  LGAbstractContainer,
  LGQueue,
  LGArrayHelpers;

type

  TGQueueTest = class(TTestCase)
  private
  type
    TIntQueue      = class(specialize TGQueue<Integer>);
    TStrQueue      = class(specialize TGQueue<string>);
    TAutoIntQueue  = specialize TGAutoRef<TIntQueue>;
    TAutoStrQueue  = specialize TGAutoRef<TStrQueue>;
    TIntArray      = specialize TGArray<Integer>;
    TStrArray      = specialize TGArray<string>;
    TIntEnumerable = specialize TGArrayCursor<Integer>;
    TIntHelper     = specialize TGComparableArrayHelper<Integer>;
    TObjProc       = procedure of object;

    TTestObj = class
    private
      FProc: TObjProc;
    public
      constructor Create(aProc: TObjProc);
      destructor Destroy; override;
    end;

    TCounter = object
    private
      FCount: Integer;
      procedure IncCount;
      function GetIncrement: TObjProc;
    public
      property Increment: TObjProc read GetIncrement;
      property Count: Integer read FCount;
    end;

    TObjQueue     = class(specialize TGObjectQueue<TTestObj>);
    TAutoObjQueue = specialize TGAutoRef<TObjQueue>;

  const
    IntArray21: array[1..21] of Integer =
      (1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21);
  published
    procedure CreateEmpty;
    procedure CreateCapacity1;
    procedure CreateCapacity0;
    procedure CreateCapacityNeg;
    procedure CreateCapacity1024;
    procedure CreateEmptyArray;
    procedure CreateStaticArray21;
    procedure CreateDynArray21;
    procedure CreateEnum21;

    procedure EnsureCapacity0;
    procedure EnsureCapacity11;

    procedure Enqueue;
    procedure EnqueueAllStaticArray21;
    procedure EnqueueAllDynArray21;
    procedure EnqueueAllEnum21;
    procedure EnqueueAllSelf;

    procedure DequeueEmpty;
    procedure Dequeuef21;
    procedure Dequeue64;
    procedure EnqueueDequeue;

    procedure Reverse;

    procedure TryDequeueEmpty;
    procedure TryDequeueOf21;

    procedure PeekEmpty;
    procedure PeekOf21;

    procedure TryPeekEmpty;
    procedure TryPeekOf21;

    procedure Clear;

    procedure TrimToFit;
    procedure TrimToFit_1;
    procedure TrimToFit_2;
    procedure TrimToFitStr;

    procedure EnumeratorInt;
    procedure EnumeratorStr;

    procedure ToArrayInt;
    procedure ToArrayStr;

    procedure InIteration;
    procedure IterationDone;
    procedure ObjectQueue;
  end;

  TLiteQueueTest = class(TTestCase)
  private
  type
    TIntQueue = specialize TGLiteQueue<Integer>;
    TStrQueue = specialize TGLiteQueue<string>;
    TProc     = procedure is nested;

    TTestObj = class
    private
      FProc: TProc;
    public
      constructor Create(aProc: TProc);
      destructor Destroy; override;
    end;
    TObjQueue = specialize TGLiteObjectQueue<TTestObj>;

  const
    IntArray21: array[1..21] of Integer =
      (1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21);
  published

    procedure EnsureCapacity;
    procedure Enqueue;
    procedure DequeueEmpty;
    procedure Dequeue;
    procedure Dequeue2;
    procedure Dequeue3;
    procedure Reverse;
    procedure Mutables;
    procedure TryDequeueEmpty;
    procedure TryDequeue;
    procedure PeekEmpty;
    procedure Peek;
    procedure TryPeekEmpty;
    procedure TryPeek;
    procedure Clear;
    procedure TrimToFit;
    procedure TrimToFit_1;
    procedure TrimToFit_2;
    procedure TrimToFitStr;
    procedure EnumeratorInt;
    procedure EnumeratorStr;
    procedure Assign;
    procedure PassByValue;
    procedure ToArrayInt;
    procedure ToArrayStr;
    procedure ObjectQueue;
  end;

implementation
{$B-}{$COPERATORS ON}

{ TGQueueTest.TTestObj }

constructor TGQueueTest.TTestObj.Create(aProc: TObjProc);
begin
  FProc := aProc;
end;

destructor TGQueueTest.TTestObj.Destroy;
begin
  if FProc <> nil then
    FProc;
  inherited;
end;

{ TGQueueTest.TCounter }

procedure TGQueueTest.TCounter.IncCount;
begin
  Inc(FCount);
end;

function TGQueueTest.TCounter.GetIncrement: TObjProc;
begin
  Result := @IncCount;
end;

procedure TGQueueTest.CreateEmpty;
var
  q: TAutoIntQueue;
begin
  AssertTrue({%H-}q.Instance.Count = 0);
  AssertTrue(q.Instance.Capacity = DEFAULT_CONTAINER_CAPACITY);
end;

procedure TGQueueTest.CreateCapacity1;
var
  q: TAutoIntQueue;
begin
  {%H-}q.Instance := TIntQueue.Create(1);
  AssertTrue(q.Instance.Count = 0);
  AssertTrue(q.Instance.Capacity = 1);
end;

procedure TGQueueTest.CreateCapacity0;
var
  q: TAutoIntQueue;
begin
  {%H-}q.Instance := TIntQueue.Create(0);
  AssertTrue(q.Instance.Count = 0);
  AssertTrue(q.Instance.Capacity = 0);
end;

procedure TGQueueTest.CreateCapacityNeg;
var
  q: TAutoIntQueue;
begin
  {%H-}q.Instance := TIntQueue.Create(-15);
  AssertTrue(q.Instance.Count = 0);
  AssertTrue(q.Instance.Capacity = 0);
end;

procedure TGQueueTest.CreateCapacity1024;
var
  q: TAutoIntQueue;
begin
  {%H-}q.Instance := TIntQueue.Create(1024);
  AssertTrue(q.Instance.Count = 0);
  AssertTrue(q.Instance.Capacity = 1024);
end;

procedure TGQueueTest.CreateEmptyArray;
var
  q: TAutoIntQueue;
  a: TIntArray;
begin
  {%H-}q.Instance := TIntQueue.Create(a{%H-});
  AssertTrue(q.Instance.Count = 0);
  AssertTrue(q.Instance.Capacity = DEFAULT_CONTAINER_CAPACITY);
end;

procedure TGQueueTest.CreateStaticArray21;
var
  q: TAutoIntQueue;
begin
  {%H-}q.Instance := TIntQueue.Create(IntArray21);
  AssertTrue(q.Instance.Count = 21);
  AssertTrue(q.Instance.Capacity = DEFAULT_CONTAINER_CAPACITY);
  AssertTrue(TIntHelper.Same(q.Instance.ToArray, IntArray21));
end;

procedure TGQueueTest.CreateDynArray21;
var
  q: TAutoIntQueue;
begin
  {%H-}q.Instance := TIntQueue.Create(TIntHelper.CreateCopy(IntArray21));
  AssertTrue(q.Instance.Count = 21);
  AssertTrue(q.Instance.Capacity = DEFAULT_CONTAINER_CAPACITY);
  AssertTrue(TIntHelper.Same(q.Instance.ToArray, IntArray21));
end;

procedure TGQueueTest.CreateEnum21;
var
  q: TAutoIntQueue;
begin
  {%H-}q.Instance := TIntQueue.Create(TIntEnumerable.Create(TIntHelper.CreateCopy(IntArray21)));
  AssertTrue(q.Instance.Count = 21);
  AssertTrue(q.Instance.Capacity = DEFAULT_CONTAINER_CAPACITY);
  AssertTrue(TIntHelper.Same(q.Instance.ToArray, IntArray21));
end;

procedure TGQueueTest.EnsureCapacity0;
var
  q: TAutoIntQueue;
begin
  {%H-}q.Instance := TIntQueue.Create(0);
  q.Instance.EnsureCapacity(0);
  AssertTrue(q.Instance.Capacity = 0);
end;

procedure TGQueueTest.EnsureCapacity11;
var
  q: TAutoIntQueue;
begin
  {%H-}q.Instance := TIntQueue.Create(0);
  q.Instance.EnsureCapacity(11);
  AssertTrue(q.Instance.Capacity = DEFAULT_CONTAINER_CAPACITY);
end;

procedure TGQueueTest.Enqueue;
var
  q: TAutoIntQueue;
begin
  {%H-}q.Instance.Enqueue(5);
  AssertFalse(q.Instance.IsEmpty);
  AssertTrue(q.Instance.NonEmpty);
  AssertTrue(q.Instance.Count = 1);
  q.Instance.Enqueue(10);
  AssertTrue(q.Instance.Count = 2);
  q.Instance.Enqueue(15);
  AssertTrue(q.Instance.Count = 3);
end;

procedure TGQueueTest.EnqueueAllStaticArray21;
var
  q: TAutoIntQueue;
begin
  {%H-}q.Instance.Enqueue(5);
  AssertTrue(q.Instance.EnqueueAll(IntArray21) = System.Length(IntArray21));
  AssertTrue(q.Instance.Count = 22);
end;

procedure TGQueueTest.EnqueueAllDynArray21;
var
  q: TAutoIntQueue;
begin
  {%H-}q.Instance.Enqueue(5);
  AssertTrue(q.Instance.EnqueueAll(TIntHelper.CreateCopy(IntArray21)) = System.Length(IntArray21));
  AssertTrue(q.Instance.Count = 22);
end;

procedure TGQueueTest.EnqueueAllEnum21;
var
  q: TAutoIntQueue;
begin
  {%H-}q.Instance.Enqueue(5);
  AssertTrue(q.Instance.EnqueueAll(TIntEnumerable.Create(TIntHelper.CreateCopy(IntArray21))) = System.Length(IntArray21));
  AssertTrue(q.Instance.Count = 22);
end;

procedure TGQueueTest.EnqueueAllSelf;
var
  q: TAutoIntQueue;
  I: Integer;
begin
  {%H-}q.Instance.EnqueueAll(IntArray21);
  AssertTrue(q.Instance.EnqueueAll(q.Instance) = 21);
  for I := 1 to 21 do
    AssertTrue(q{%H-}.Instance.Dequeue = I);
  for I := 1 to 21 do
    AssertTrue(q{%H-}.Instance.Dequeue = I);
  AssertTrue(q.Instance.IsEmpty);
end;

procedure TGQueueTest.DequeueEmpty;
var
  q: TAutoIntQueue;
  Raised: Boolean = False;
begin
  try
    {%H-}q.Instance.Dequeue;
  except
    on e: ELGAccessEmpty do
      Raised := True;
  end;
  AssertTrue(Raised);
end;

procedure TGQueueTest.Dequeuef21;
var
  q: TAutoIntQueue;
  I, v: Integer;
begin
  {%H-}q.Instance := TIntQueue.Create(IntArray21);
  for I := 1 to 21 do
    begin
      v := q{%H-}.Instance.Dequeue;
      AssertTrue(v = I);
    end;
  AssertTrue(q.Instance.IsEmpty);
end;

procedure TGQueueTest.Dequeue64;
var
  q: TAutoIntQueue;
  I, v: Integer;
begin
  for I := 1 to 64 do
    {%H-}q.Instance.Enqueue(I);

  for I := 1 to 40 do
    begin
      v := q{%H-}.Instance.Dequeue;
      AssertTrue(v = I);
    end;
  for I := 65 to 90 do
    q.Instance.Enqueue(I);
  for I := 41 to 90 do
    begin
      v := q{%H-}.Instance.Dequeue;
      AssertTrue(v = I);
    end;
  AssertTrue(q.Instance.IsEmpty);
end;

procedure TGQueueTest.EnqueueDequeue;
var
  q: TAutoIntQueue;
  I, v: Integer;
begin
  for I := 1 to 64 do
    {%H-}q.Instance.Enqueue(I);

  for I := 1 to 20 do
    begin
      v := q{%H-}.Instance.Dequeue;
      AssertTrue(v = I);
    end;
  for I := 65 to 100 do
    q.Instance.Enqueue(I);
  AssertTrue(q.Instance.Count = 80);
  AssertTrue(q.Instance.Capacity = 128);
  for I := 21 to 100 do
    begin
      v := q{%H-}.Instance.Dequeue;
      AssertTrue(v = I);
    end;
  AssertTrue(q.Instance.IsEmpty);
end;

procedure TGQueueTest.Reverse;
var
  q: TAutoIntQueue;
  I, v: Integer;
begin
  for I := 1 to 64 do
    q.Instance.Enqueue(I);
  q.Instance.TrimToFit;
  for I := 1 to 40 do
    begin
      v := q{%H-}.Instance.Dequeue;
      AssertTrue(v = I);
    end;
  for I := 65 to 85 do
    q.Instance.Enqueue(I);
  AssertTrue(q.Instance.Count = 45);
  q.Instance.EnsureCapacity(1022);
  v := 85;
  for I in q.Instance.Reverse do
    begin
      AssertTrue(I = v);
      Dec(v);
    end;
end;

procedure TGQueueTest.TryDequeueEmpty;
var
  q: TAutoIntQueue;
  v: Integer;
begin
  AssertFalse({%H-}q.Instance.TryDequeue(v));
end;

procedure TGQueueTest.TryDequeueOf21;
var
  q: TAutoIntQueue;
  I, v: Integer;
begin
  {%H-}q.Instance := TIntQueue.Create(IntArray21);
  for I := 1 to 21 do
    begin
      AssertTrue(q.Instance.TryDequeue(v));
      AssertTrue(v = I);
    end;
  AssertTrue(q.Instance.IsEmpty);
end;

procedure TGQueueTest.PeekEmpty;
var
  q: TAutoIntQueue;
  Raised: Boolean = False;
begin
  try
    {%H-}q.Instance.Peek;
  except
    Raised := True;
  end;
  AssertTrue(Raised);
end;

procedure TGQueueTest.PeekOf21;
var
  q: TAutoIntQueue;
  I, v: Integer;
begin
  {%H-}q.Instance := TIntQueue.Create(IntArray21);
  v := q{%H-}.Instance.Peek;
  AssertTrue(v = 1);
  v := q{%H-}.Instance.Peek;
  AssertTrue(v = 1);
  for I := 1 to 21 do
    begin
      v := q{%H-}.Instance.Peek;
      {%H-}q.Instance.Dequeue;
      AssertTrue(v = I);
    end;
end;

procedure TGQueueTest.TryPeekEmpty;
var
  q: TAutoIntQueue;
  v: Integer;
begin
  AssertFalse({%H-}q.Instance.TryPeek(v));
end;

procedure TGQueueTest.TryPeekOf21;
var
  q: TAutoIntQueue;
  I, v: Integer;
begin
  {%H-}q.Instance := TIntQueue.Create(IntArray21);
  AssertTrue(q.Instance.TryPeek(v));
  AssertTrue(v = 1);
  AssertTrue(q.Instance.TryPeek(v));
  AssertTrue(v = 1);
  for I := 1 to 21 do
    begin
      AssertTrue(q.Instance.TryPeek(v));
      AssertTrue(v = I);
      {%H-}q.Instance.Dequeue;
    end;
  AssertTrue(q.Instance.IsEmpty);
end;

procedure TGQueueTest.Clear;
var
  q: TAutoIntQueue;
begin
  {%H-}q.Instance := TIntQueue.Create(IntArray21);
  AssertTrue(q.Instance.Count = 21);
  q.Instance.Clear;
  AssertTrue(q.Instance.IsEmpty);
  AssertTrue(q.Instance.Capacity = 0);
end;

procedure TGQueueTest.TrimToFit;
var
  q: TAutoIntQueue;
  I: Integer;
begin
  {%H-}q.Instance := TIntQueue.Create(IntArray21);
  AssertTrue(q.Instance.Count = 21);
  AssertTrue(q.Instance.Capacity = DEFAULT_CONTAINER_CAPACITY);
  q.Instance.TrimToFit;
  AssertTrue(q.Instance.Count = 21);
  AssertTrue(q.Instance.Capacity = 21);

  while q.Instance.Count < 120 do
    q.Instance.Enqueue(0);
  while q.Instance.NonEmpty do
    q.Instance.Dequeue;
  q.Instance.TrimToFit;
  AssertTrue(q.Instance.Capacity = 0);
  for I := 1 to 150 do
    q.Instance.Enqueue(I); //should not crash
end;

procedure TGQueueTest.TrimToFit_1;
var
  q: TAutoIntQueue;
  I, v: Integer;
begin
  for I := 1 to 64 do
    {%H-}q.Instance.Enqueue(I);
  for I := 1 to 40 do
    begin
      v := q{%H-}.Instance.Dequeue;
      AssertTrue(v = I);
    end;
  for I := 65 to 85 do
    q.Instance.Enqueue(I);
  q.Instance.TrimToFit;
  AssertTrue(q.Instance.Count = 45);
  AssertTrue(q.Instance.Capacity = 45);
  for I := 41 to 85 do
    begin
      v := q{%H-}.Instance.Dequeue;
      AssertTrue(v = I);
    end;
  AssertTrue(q.Instance.IsEmpty);
end;

procedure TGQueueTest.TrimToFit_2;
var
  q: TAutoIntQueue;
  I, v: Integer;
begin
  for I := 1 to 64 do
    {%H-}q.Instance.Enqueue(I);
  for I := 1 to 40 do
    {%H-}q.Instance.Dequeue;
  for I := 65 to 100 do
    q.Instance.Enqueue(I);
  q.Instance.TrimToFit;
  AssertTrue(q.Instance.Count = 60);
  AssertTrue(q.Instance.Capacity = 60);
  q.Instance.EnsureCapacity(100);
  for I := 101 to 140 do
    q.Instance.Enqueue(I);
  q.Instance.TrimToFit;
  AssertTrue(q.Instance.Count = 100);
  AssertTrue(q.Instance.Capacity = 100);
  q.Instance.EnsureCapacity(110);
  for I := 41 to 140 do
    begin
      v := q{%H-}.Instance.Dequeue;
      AssertTrue(v = I);
    end;
  AssertTrue(q.Instance.IsEmpty);
end;

procedure TGQueueTest.TrimToFitStr;
var
  q: TAutoStrQueue;
  I: Integer;
  v: string;
begin
  for I := 1 to 64 do
    {%H-}q.Instance.Enqueue('string ' + I.ToString);
  for I := 1 to 40 do
    {%H-}q.Instance.Dequeue;
  for I := 65 to 100 do
    q.Instance.Enqueue('string ' + I.ToString);
  q.Instance.TrimToFit;
  AssertTrue(q.Instance.Count = 60);
  AssertTrue(q.Instance.Capacity = 60);
  q.Instance.EnsureCapacity(100);
  for I := 101 to 140 do
    q.Instance.Enqueue('string ' + I.ToString);
  q.Instance.TrimToFit;
  AssertTrue(q.Instance.Count = 100);
  AssertTrue(q.Instance.Capacity = 100);
  q.Instance.EnsureCapacity(110);
  for I := 41 to 140 do
    begin
      v := q{%H-}.Instance.Dequeue;
      AssertTrue(v = 'string ' + I.ToString);
    end;
  AssertTrue(q.Instance.IsEmpty);
end;

procedure TGQueueTest.EnumeratorInt;
var
  q: TAutoIntQueue;
  I, v: Integer;
begin
  for I := 1 to 64 do
    q.Instance.Enqueue(I);
  for I := 1 to 40 do
    {%H-}q.Instance.Dequeue;
  for I := 65 to 100 do
    q.Instance.Enqueue(I);
  q.Instance.TrimToFit;
  q.Instance.EnsureCapacity(100);
  for I := 101 to 140 do
    q.Instance.Enqueue(I);
  q.Instance.TrimToFit;
  q.Instance.EnsureCapacity(110);
  I := 41;
  for v in q.Instance do
    begin
      AssertTrue(v = I);
      Inc(I);
    end;
end;

procedure TGQueueTest.EnumeratorStr;
var
  q: TAutoStrQueue;
  I: Integer;
  v: string;
begin
  for I := 1 to 64 do
    q.Instance.Enqueue('string ' + I.ToString);
  for I := 1 to 40 do
    {%H-}q.Instance.Dequeue;
  for I := 65 to 100 do
    q.Instance.Enqueue('string ' + I.ToString);
  q.Instance.TrimToFit;
  q.Instance.EnsureCapacity(100);
  for I := 101 to 140 do
    q.Instance.Enqueue('string ' + I.ToString);
  q.Instance.TrimToFit;
  q.Instance.EnsureCapacity(110);
  I := 41;
  for v in q.Instance do
    begin
      AssertTrue(v = 'string ' + I.ToString);
      Inc(I);
    end;
end;

procedure TGQueueTest.ToArrayInt;
var
  q: TAutoIntQueue;
  I: Integer;
  a: TIntArray;
begin
  for I := 1 to 62 do
    {%H-}q.Instance.Enqueue(I);
  for I := 1 to 40 do
    {%H-}q.Instance.Dequeue;
  for I := 63 to 100 do
    q.Instance.Enqueue(I);
  q.Instance.TrimToFit;
  q.Instance.EnsureCapacity(100);
  for I := 101 to 140 do
    q.Instance.Enqueue(I);
  q.Instance.TrimToFit;
  q.Instance.EnsureCapacity(110);
  a := q.Instance.ToArray;
  for I := 41 to 140 do
    AssertTrue(a[I - 41] = I);
end;

procedure TGQueueTest.ToArrayStr;
var
  q: TAutoStrQueue;
  I: Integer;
  a: TStrArray;
begin
  for I := 1 to 61 do
    {%H-}q.Instance.Enqueue('string ' + I.ToString);
  for I := 1 to 40 do
    {%H-}q.Instance.Dequeue;
  for I := 62 to 100 do
    q.Instance.Enqueue('string ' + I.ToString);
  q.Instance.TrimToFit;
  q.Instance.EnsureCapacity(100);
  for I := 101 to 140 do
    q.Instance.Enqueue('string ' + I.ToString);
  q.Instance.TrimToFit;
  q.Instance.EnsureCapacity(110);
  a := q.Instance.ToArray;
  for I := 41 to 140 do
    AssertTrue(a[I - 41] = 'string ' + I.ToString);
end;

procedure TGQueueTest.InIteration;
  function IsEven(constref aValue: Integer): Boolean;
  begin
    Result := not Odd(aValue);
  end;
var
  s: TAutoIntQueue;
  I: Integer;
  Raised: Boolean = False;
begin
  s.Instance := TIntQueue.Create(IntArray21);
  try
    for I in s.Instance.Select(@IsEven) do
      begin
        if not Odd(I) then
        {%H-}s.Instance.Dequeue;
      end;
  except
    on e: ELGUpdateLock do
      Raised := True;
  end;
  AssertTrue(Raised);
end;

procedure TGQueueTest.IterationDone;
  function IsEven(constref aValue: Integer): Boolean;
  begin
    Result := not Odd(aValue);
  end;
  function Double(constref aValue: Integer): Integer;
  begin
    Result := aValue + aValue;
  end;
var
  q: TAutoIntQueue;
  I, J: Integer;
begin
  q.Instance := TIntQueue.Create(IntArray21);
  J := 2;
  for I in q.Instance.Select(@IsEven).Map(@Double) do
    begin
      AssertTrue(I = J * 2);
      J += 2;
    end;
  //shouldn't raise exeption
  q.Instance.Enqueue(22);
  AssertTrue(q.Instance.Count = 22);
end;

procedure TGQueueTest.ObjectQueue;
var
  Counter: TCounter;
  q: TAutoObjQueue;
  I: Integer;
begin
  Counter := Default(TCounter);
  for I := 1 to 100 do
    {%H-}q.Instance.Enqueue(TTestObj.Create(Counter.Increment));
  AssertTrue(q.Instance.Count = 100);
  q.Instance.TrimToFit;
  for I := 1 to 20 do
    q.Instance.Dequeue{%H-}.Free;
  AssertTrue(Counter.Count = 20);
  AssertTrue(q.Instance.Count = 80);
  for I := 1 to 20 do
    q.Instance.Enqueue(TTestObj.Create(Counter.Increment));
  AssertTrue(q.Instance.Count = 100);
  q.Instance := Default(TObjQueue);
  AssertTrue(Counter.Count = 120);
end;

{ TLiteQueueTest.TTestObj }

constructor TLiteQueueTest.TTestObj.Create(aProc: TProc);
begin
  FProc := aProc;
end;

destructor TLiteQueueTest.TTestObj.Destroy;
begin
  if FProc <> nil then
    FProc;
  inherited;
end;

{ TLiteQueueTest }

procedure TLiteQueueTest.EnsureCapacity;
var
  q: TIntQueue;
begin
  AssertTrue({%H-}q.Capacity = 0);
  q.EnsureCapacity(0);
  AssertTrue(q.Capacity = 0);
  q.EnsureCapacity(11);
  AssertTrue(q.Capacity = DEFAULT_CONTAINER_CAPACITY);
  q.EnsureCapacity(250);
  AssertTrue(q.Capacity = 256);
end;

procedure TLiteQueueTest.Enqueue;
var
  q: TIntQueue;
begin
  {%H-}q.Enqueue(5);
  AssertFalse(q.IsEmpty);
  AssertTrue(q.NonEmpty);
  AssertTrue(q.Count = 1);
  q.Enqueue(10);
  AssertTrue(q.Count = 2);
  q.Enqueue(15);
  AssertTrue(q.Count = 3);
end;

procedure TLiteQueueTest.DequeueEmpty;
var
  q: TIntQueue;
  Raised: Boolean = False;
begin
  try
    {%H-}q.Dequeue;
  except
    on e: ELGAccessEmpty do
      Raised := True;
  end;
  AssertTrue(Raised);
end;

procedure TLiteQueueTest.Dequeue;
var
  q: TIntQueue;
  I, J: Integer;
begin
  for I in IntArray21 do
    q.Enqueue(I);
  for I := 1 to 21 do
    begin
      J := q{%H-}.Dequeue;
      AssertTrue(J = I);
    end;
  AssertTrue(q.IsEmpty);
end;

procedure TLiteQueueTest.Dequeue2;
var
  q: TIntQueue;
  I, J: Integer;
begin
  for I := 1 to 64 do
    {%H-}q.Enqueue(I);

  for I := 1 to 40 do
    begin
      J := q{%H-}.Dequeue;
      AssertTrue(J = I);
    end;
  for I := 65 to 90 do
    q.Enqueue(I);
  for I := 41 to 90 do
    begin
      J := q{%H-}.Dequeue;
      AssertTrue(J = I);
    end;
  AssertTrue(q.IsEmpty);
end;

procedure TLiteQueueTest.Dequeue3;
var
  q: TIntQueue;
  I, v: Integer;
begin
  for I := 1 to 64 do
    {%H-}q.Enqueue(I);

  for I := 1 to 20 do
    begin
      v := q{%H-}.Dequeue;
      AssertTrue(v = I);
    end;
  for I := 65 to 100 do
    q.Enqueue(I);
  AssertTrue(q.Count = 80);
  for I := 21 to 100 do
    begin
      v := q{%H-}.Dequeue;
      AssertTrue(v = I);
    end;
  AssertTrue(q.IsEmpty);
end;

procedure TLiteQueueTest.Reverse;
var
  q: TIntQueue;
  I, J: Integer;
begin
  for I := 1 to 64 do
    q.Enqueue(I);
  q.TrimToFit;
  for I := 1 to 40 do
    begin
      J := q{%H-}.Dequeue;
      AssertTrue(J = I);
    end;
  for I := 65 to 85 do
    q.Enqueue(I);
  AssertTrue(q.Count = 45);
  q.EnsureCapacity(1022);
  J := 85;
  for {%H-}I in q.Reverse do
    begin
      AssertTrue(I = J);
      Dec(J);
    end;
end;

procedure TLiteQueueTest.Mutables;
var
  q: TIntQueue;
  I, J: Integer;
  p: TIntQueue.PItem;
begin
  for I := 1 to 50 do
    {%H-}q.Enqueue(I);
  for {%H-}p in q.Mutables do
    p^ := p^ + 10;
  J := 1;
  for {%H-}I in q do
    begin
      AssertTrue(I = J + 10);
      Inc(J);
    end;
end;

procedure TLiteQueueTest.TryDequeueEmpty;
var
  q: TIntQueue;
  I: Integer;
begin
  AssertFalse({%H-}q.TryDequeue(I){%H-});
end;

procedure TLiteQueueTest.TryDequeue;
var
  q: TIntQueue;
  I, J: Integer;
begin
  for I in IntArray21 do
    q.Enqueue(I);
  for I := 1 to 21 do
    begin
      AssertTrue(q.TryDequeue(J){%H-});
      AssertTrue(J = I);
    end;
  AssertTrue(q.IsEmpty);
end;

procedure TLiteQueueTest.PeekEmpty;
var
  q: TIntQueue;
  Raised: Boolean = False;
begin
  try
    {%H-}q.Peek;
  except
    Raised := True;
  end;
  AssertTrue(Raised);
end;

procedure TLiteQueueTest.Peek;
var
  q: TIntQueue;
  I, J: Integer;
begin
  for I in IntArray21 do
    q.Enqueue(I);
  J := q{%H-}.Peek;
  AssertTrue(J = 1);
  J := q{%H-}.Peek;
  AssertTrue(J = 1);
  for I := 1 to 21 do
    begin
      J := q{%H-}.Peek;
      {%H-}q.Dequeue;
      AssertTrue(J = I);
    end;
end;

procedure TLiteQueueTest.TryPeekEmpty;
var
  q: TIntQueue;
  I: Integer;
begin
  AssertFalse({%H-}q.TryPeek(I));
end;

procedure TLiteQueueTest.TryPeek;
var
  q: TIntQueue;
  I, J: Integer;
begin
  for I in IntArray21 do
    q.Enqueue(I);
  AssertTrue(q.TryPeek(J));
  AssertTrue(J = 1);
  AssertTrue(q.TryPeek(J));
  AssertTrue(J = 1);
  for I := 1 to 21 do
    begin
      AssertTrue(q.TryPeek(J));
      AssertTrue(J = I);
      {%H-}q.Dequeue;
    end;
  AssertTrue(q.IsEmpty);
end;

procedure TLiteQueueTest.Clear;
var
  q: TIntQueue;
  I: Integer;
begin
  for I in IntArray21 do
    q.Enqueue(I);
  AssertTrue(q.Count = 21);
  q.Clear;
  AssertTrue(q.IsEmpty);
  AssertTrue(q.Capacity = 0);
end;

procedure TLiteQueueTest.TrimToFit;
var
  q: TIntQueue;
  I: Integer;
begin
  for I in IntArray21 do
    q.Enqueue(I);
  AssertTrue(q.Count = 21);
  AssertTrue(q.Capacity = DEFAULT_CONTAINER_CAPACITY);
  q.TrimToFit;
  AssertTrue(q.Count = 21);
  AssertTrue(q.Capacity = 21);

  while q.Count < 120 do
    q.Enqueue(0);
  while q.NonEmpty do
    q.Dequeue;
  q.TrimToFit;
  AssertTrue(q.Capacity = 0);
  for I := 1 to 150 do
    q.Enqueue(I); //should not crash
end;

procedure TLiteQueueTest.TrimToFit_1;
var
  q: TIntQueue;
  I, J: Integer;
begin
  for I := 1 to 64 do
    {%H-}q.Enqueue(I);
  for I := 1 to 40 do
    begin
      J := q{%H-}.Dequeue;
      AssertTrue(J = I);
    end;
  for I := 65 to 85 do
    q.Enqueue(I);
  q.TrimToFit;
  AssertTrue(q.Count = 45);
  AssertTrue(q.Capacity = 45);
  for I := 41 to 85 do
    begin
      J := q{%H-}.Dequeue;
      AssertTrue(J = I);
    end;
  AssertTrue(q.IsEmpty);
end;

procedure TLiteQueueTest.TrimToFit_2;
var
  q: TIntQueue;
  I, J: Integer;
begin
  for I := 1 to 64 do
    {%H-}q.Enqueue(I);
  for I := 1 to 40 do
    {%H-}q.Dequeue;
  for I := 65 to 100 do
    q.Enqueue(I);
  q.TrimToFit;
  AssertTrue(q.Count = 60);
  AssertTrue(q.Capacity = 60);
  q.EnsureCapacity(100);
  for I := 101 to 140 do
    q.Enqueue(I);
  q.TrimToFit;
  AssertTrue(q.Count = 100);
  AssertTrue(q.Capacity = 100);
  q.EnsureCapacity(110);
  for I := 41 to 140 do
    begin
      J := q{%H-}.Dequeue;
      AssertTrue(J = I);
    end;
  AssertTrue(q.IsEmpty);
end;

procedure TLiteQueueTest.TrimToFitStr;
var
  q: TStrQueue;
  I: Integer;
  v: string;
begin
  for I := 1 to 64 do
    {%H-}q.Enqueue('string ' + I.ToString);
  for I := 1 to 40 do
    {%H-}q.Dequeue;
  for I := 65 to 100 do
    q.Enqueue('string ' + I.ToString);
  q.TrimToFit;
  AssertTrue(q.Count = 60);
  AssertTrue(q.Capacity = 60);
  q.EnsureCapacity(100);
  for I := 101 to 140 do
    q.Enqueue('string ' + I.ToString);
  q.TrimToFit;
  AssertTrue(q.Count = 100);
  AssertTrue(q.Capacity = 100);
  q.EnsureCapacity(110);
  for I := 41 to 140 do
    begin
      v := q{%H-}.Dequeue;
      AssertTrue(v = 'string ' + I.ToString);
    end;
  AssertTrue(q.IsEmpty);
end;

procedure TLiteQueueTest.EnumeratorInt;
var
  q: TIntQueue;
  I, J: Integer;
begin
  for I := 1 to 64 do
    q.Enqueue(I);
  for I := 1 to 40 do
    {%H-}q.Dequeue;
  for I := 65 to 100 do
    q.Enqueue(I);
  q.TrimToFit;
  q.EnsureCapacity(100);
  for I := 101 to 140 do
    q.Enqueue(I);
  q.TrimToFit;
  q.EnsureCapacity(110);
  I := 41;
  for {%H-}J in q do
    begin
      AssertTrue(J = I);
      Inc(I);
    end;
end;

procedure TLiteQueueTest.EnumeratorStr;
var
  q: TStrQueue;
  I: Integer;
  s: string;
begin
  for I := 1 to 64 do
    q.Enqueue('string ' + I.ToString);
  for I := 1 to 40 do
    {%H-}q.Dequeue;
  for I := 65 to 100 do
    q.Enqueue('string ' + I.ToString);
  q.TrimToFit;
  q.EnsureCapacity(100);
  for I := 101 to 140 do
    q.Enqueue('string ' + I.ToString);
  q.TrimToFit;
  q.EnsureCapacity(110);
  I := 41;
  for {%H-}s in q do
    begin
      AssertTrue(s = 'string ' + I.ToString);
      Inc(I);
    end;
end;

procedure TLiteQueueTest.Assign;
var
  q1, q2: TIntQueue;
begin
  {%H-}q1.Enqueue(1);
  q1.Enqueue(2);
  q1.Enqueue(3);
  AssertTrue(q1.Count = 3);
  q2 := q1;
  AssertTrue(q1.Dequeue = 1);
  AssertTrue(q1.Dequeue = 2);
  AssertTrue(q1.Dequeue = 3);

  AssertTrue(q2.Count = 3);
  AssertTrue(q1.IsEmpty);

  AssertTrue(q2.Dequeue = 1);
  AssertTrue(q2.Dequeue = 2);
  AssertTrue(q2.Dequeue = 3);
  q2.Enqueue(4);
  q2.Enqueue(5);
  AssertTrue(q2.Count = 2);
  q1.Enqueue(6);
  AssertTrue(q1.Count = 1);
  q2 := q1;
  AssertTrue(q1.Dequeue = 6);
  AssertTrue(q1.IsEmpty);
  AssertTrue(q2.Count = 1);
  AssertTrue(q2.Dequeue = 6);
end;

procedure TLiteQueueTest.PassByValue;
  procedure Test(aQueue: TIntQueue);
  begin
    aQueue.Enqueue(10);
    aQueue.Enqueue(1);
    AssertTrue(aQueue.NonEmpty);
  end;
var
  q: TIntQueue;
begin
  {%q-}q.EnsureCapacity(10);
  AssertTrue(q.Capacity > 2);
  AssertTrue(q.IsEmpty);
  Test(q);
  AssertTrue(q.IsEmpty);
end;

procedure TLiteQueueTest.ToArrayInt;
var
  q: TIntQueue;
  I: Integer;
  a: array of Integer;
begin
  for I := 1 to 62 do
    {%H-}q.Enqueue(I);
  for I := 1 to 40 do
    {%H-}q.Dequeue;
  for I := 63 to 100 do
    q.Enqueue(I);
  q.TrimToFit;
  q.EnsureCapacity(100);
  for I := 101 to 140 do
    q.Enqueue(I);
  q.TrimToFit;
  q.EnsureCapacity(110);
  a := q.ToArray;
  for I := 41 to 140 do
    AssertTrue(a[I - 41] = I);
end;

procedure TLiteQueueTest.ToArrayStr;
var
  q: TStrQueue;
  I: Integer;
  a: array of string;
begin
  for I := 1 to 61 do
    {%H-}q.Enqueue('string ' + I.ToString);
  for I := 1 to 40 do
    {%H-}q.Dequeue;
  for I := 62 to 100 do
    q.Enqueue('string ' + I.ToString);
  q.TrimToFit;
  q.EnsureCapacity(100);
  for I := 101 to 140 do
    q.Enqueue('string ' + I.ToString);
  q.TrimToFit;
  q.EnsureCapacity(110);
  a := q.ToArray;
  for I := 41 to 140 do
    AssertTrue(a[I - 41] = 'string ' + I.ToString);
end;

procedure TLiteQueueTest.ObjectQueue;
var
  Counter: Integer = 0;
  q: TObjQueue;
  I: Integer;
  procedure IncCounter;
  begin
    Inc(Counter);
  end;
begin
  for I := 1 to 100 do
    {%H-}q.Enqueue(TTestObj.Create(@IncCounter));
  AssertTrue(q.Count = 100);
  {%H-}q.TrimToFit;
  for I := 1 to 20 do
    q.Dequeue{%H-}.Free;
  AssertTrue(Counter = 20);
  AssertTrue(q.Count = 80);
  for I := 1 to 20 do
    q.Enqueue(TTestObj.Create(@IncCounter));
  AssertTrue(q.Count = 100);
  q := Default(TObjQueue);
  AssertTrue(Counter = 120);
end;

initialization
  RegisterTest(TGQueueTest);
  RegisterTest(TLiteQueueTest);
end.

