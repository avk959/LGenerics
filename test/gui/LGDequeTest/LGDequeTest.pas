unit LGDequeTest;

{$mode objfpc}{$H+}
{$MODESWITCH NESTEDPROCVARS}

interface
uses
  SysUtils, fpcunit, testregistry,
  LGUtils,
  LGAbstractContainer,
  LGDeque,
  LGArrayHelpers;

type

  TGDequeTest = class(TTestCase)
  private
  type
    TIntDeque      = class(specialize TGDeque<Integer>);
    TStrDeque      = class(specialize TGDeque<string>);
    TAutoIntDeque  = specialize TGAutoRef<TIntDeque>;
    TAutoStrDeque  = specialize TGAutoRef<TStrDeque>;
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

    TObjDeque     = class(specialize TGObjectDeque<TTestObj>);
    TAutoObjDeque = specialize TGAutoRef<TObjDeque>;
    TObjArray     = specialize TGArray<TTestObj>;

  const
    IntArray21: array[1..21] of Integer =
      (1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21);
  published
    procedure CreateEmpty;
    procedure CreateCapacity;
    procedure CreateCapacity0;
    procedure CreateCapacityNeg;
    procedure CreateCapacity1024;
    procedure CreateEmptyArray;
    procedure CreateStaticArray;
    procedure CreateDynArray;
    procedure CreateEnum;
    procedure EnsureCapacity;
    procedure EnsureCapacity_1;
    procedure PushPop;
    procedure PushPop_1;
    procedure PushAllStaticArray;
    procedure PushAllDynArray;
    procedure PushAllEnum;
    procedure PushAllSelf;
    procedure TryPopEmpty;
    procedure TryPop;
    procedure TryPeekEmpty;
    procedure TryPeek;
    procedure Insert;
    procedure InsertOutOfBounds;
    procedure InsertStr;
    procedure Extract;
    procedure ExtractStr;
    procedure Delete;
    procedure Clear;
    procedure TrimToFit;
    procedure TrimToFit_1;
    procedure TrimToFitStr;
    procedure EnumeratorInt;
    procedure EnumeratorStr;
    procedure Reverse;
    procedure ToArrayInt;
    procedure ToArrayStr;
    procedure ObjectDeque;
    procedure ObjectSetItem;
    procedure ObjectExtract;
    procedure ObjectDelete;
  end;

  TLiteDequeTest = class(TTestCase)
  private
  type
    TIntDeque = specialize TGLiteDeque<Integer>;
    TStrDeque = specialize TGLiteDeque<string>;
    TProc     = procedure is nested;

    TTestObj = class
    private
      FProc: TProc;
    public
      constructor Create(aProc: TProc);
      destructor Destroy; override;
    end;

    TObjDeque     = specialize TGLiteObjectDeque<TTestObj>;

  published
    procedure EnsureCapacity;
    procedure PushPop;
    procedure PushPop_1;
    procedure TryPopEmpty;
    procedure TryPop;
    procedure TryPeekEmpty;
    procedure TryPeek;
    procedure Insert;
    procedure InsertOutOfBounds;
    procedure InsertStr;
    procedure DeleteStr;
    procedure Delete;
    procedure Clear;
    procedure TrimToFit;
    procedure TrimToFit_1;
    procedure TrimToFitStr;
    procedure EnumeratorInt;
    procedure EnumeratorStr;
    procedure Reverse;
    procedure Mutables;
    procedure ToArray;
    procedure ObjectDeque;
    procedure ObjectSetItem;
    procedure ObjectExtract;
    procedure ObjectDelete;
  end;

implementation
{$B-}{$COPERATORS ON}

{ TGDequeTest.TTestObj }

constructor TGDequeTest.TTestObj.Create(aProc: TObjProc);
begin
  FProc := aProc;
end;

destructor TGDequeTest.TTestObj.Destroy;
begin
  if FProc <> nil then
    FProc;
  inherited;
end;

{ TGDequeTest.TCounter }

procedure TGDequeTest.TCounter.IncCount;
begin
  Inc(FCount);
end;

function TGDequeTest.TCounter.GetIncrement: TObjProc;
begin
  Result := @IncCount;
end;

procedure TGDequeTest.CreateEmpty;
var
  d: TAutoIntDeque;
begin
  AssertTrue({%H-}d.Instance.Count = 0);
  AssertTrue(d.Instance.Capacity = DEFAULT_CONTAINER_CAPACITY);
end;

procedure TGDequeTest.CreateCapacity;
var
  d: TAutoIntDeque;
begin
  {%H-}d.Instance := TIntDeque.Create(1);
  AssertTrue(d.Instance.Count = 0);
  AssertTrue(d.Instance.Capacity = 1);
end;

procedure TGDequeTest.CreateCapacity0;
var
  d: TAutoIntDeque;
begin
  {%H-}d.Instance := TIntDeque.Create(0);
  AssertTrue(d.Instance.Count = 0);
  AssertTrue(d.Instance.Capacity = 0);
end;

procedure TGDequeTest.CreateCapacityNeg;
var
  d: TAutoIntDeque;
begin
  {%H-}d.Instance := TIntDeque.Create(-7);
  AssertTrue(d.Instance.Count = 0);
  AssertTrue(d.Instance.Capacity = 0);
end;

procedure TGDequeTest.CreateCapacity1024;
var
  d: TAutoIntDeque;
begin
  {%H-}d.Instance := TIntDeque.Create(1024);
  AssertTrue(d.Instance.Count = 0);
  AssertTrue(d.Instance.Capacity = 1024);
end;

procedure TGDequeTest.CreateEmptyArray;
var
  d: TAutoIntDeque;
  a: TIntArray;
begin
  {%H-}d.Instance := TIntDeque.Create(a{%H-});
  AssertTrue(d.Instance.Count = 0);
  AssertTrue(d.Instance.Capacity = DEFAULT_CONTAINER_CAPACITY);
end;

procedure TGDequeTest.CreateStaticArray;
var
  d: TAutoIntDeque;
begin
  {%H-}d.Instance := TIntDeque.Create(IntArray21);
  AssertTrue(d.Instance.Count = 21);
  AssertTrue(d.Instance.Capacity = DEFAULT_CONTAINER_CAPACITY);
  AssertTrue(TIntHelper.Same(d.Instance.ToArray, IntArray21));
end;

procedure TGDequeTest.CreateDynArray;
var
  d: TAutoIntDeque;
begin
  {%H-}d.Instance := TIntDeque.Create(TIntHelper.CreateCopy(IntArray21));
  AssertTrue(d.Instance.Count = 21);
  AssertTrue(d.Instance.Capacity = DEFAULT_CONTAINER_CAPACITY);
  AssertTrue(TIntHelper.Same(d.Instance.ToArray, IntArray21));
end;

procedure TGDequeTest.CreateEnum;
var
  d: TAutoIntDeque;
begin
  {%H-}d.Instance := TIntDeque.Create(TIntEnumerable.Create(TIntHelper.CreateCopy(IntArray21)));
  AssertTrue(d.Instance.Count = 21);
  AssertTrue(d.Instance.Capacity = DEFAULT_CONTAINER_CAPACITY);
  AssertTrue(TIntHelper.Same(d.Instance.ToArray, IntArray21));
end;

procedure TGDequeTest.EnsureCapacity;
var
  d: TAutoIntDeque;
begin
  {%H-}d.Instance := TIntDeque.Create(0);
  d.Instance.EnsureCapacity(0);
  AssertTrue(d.Instance.Capacity = 0);
end;

procedure TGDequeTest.EnsureCapacity_1;
var
  d: TAutoIntDeque;
begin
  {%H-}d.Instance := TIntDeque.Create(0);
  d.Instance.EnsureCapacity(11);
  AssertTrue(d.Instance.Capacity = DEFAULT_CONTAINER_CAPACITY);
  d.Instance.EnsureCapacity(200);
  AssertTrue(d.Instance.Capacity = 256);
end;

procedure TGDequeTest.PushPop;
var
  d: TAutoIntDeque;
  I, v: Integer;
begin
  {%H-}d.Instance.PushLast(1);
  AssertTrue(d.Instance.NonEmpty);
  AssertTrue(d.Instance.Count = 1);
  d.Instance.PushLast(2);
  AssertTrue(d.Instance.Count = 2);
  d.Instance.PushLast(3);
  AssertTrue(d.Instance.Count = 3);
  d.Instance.PushFirst(0);
  AssertTrue(d.Instance.Count = 4);
  d.Instance.PushFirst(-1);
  AssertTrue(d.Instance.Count = 5);
  d.Instance.PushFirst(-2);
  AssertTrue(d.Instance.Count = 6);
  for I := -2 to 3 do
    begin
      v := d.Instance.PopFirst;
      AssertTrue(Format('%d expected, got %d', [I, v]), v = I);
    end;
  AssertTrue(d.Instance.IsEmpty);
end;

procedure TGDequeTest.PushPop_1;
var
  d: TAutoIntDeque;
  I, v: Integer;
begin
  {%H-}d.Instance.PushLast(1);
  d.Instance.PushLast(2);
  d.Instance.PushLast(3);
  d.Instance.PushFirst(0);
  d.Instance.PushFirst(-1);
  d.Instance.PushFirst(-2);
  AssertTrue(d.Instance.Count = 6);
  for I := 3 downto -2 do
    begin
      v := d.Instance.PopLast;
      AssertTrue(v = I);
    end;
  AssertTrue(d.Instance.IsEmpty);
end;

procedure TGDequeTest.PushAllStaticArray;
var
  d: TAutoIntDeque;
  I, v: Integer;
begin
  AssertTrue({%H-}d.Instance.PushAllLast(IntArray21) = 21);
  AssertTrue(d.Instance.Count = 21);
  AssertTrue(d.Instance.PushAllFirst(IntArray21) = 21);
  AssertTrue(d.Instance.Count = 42);
  for I := 21 downto 1 do
    begin
      v := d.Instance.PopFirst;
      AssertTrue(v = I);
    end;
  AssertTrue(d.Instance.Count = 21);
  for I := 1 to 21 do
    begin
      v := d.Instance.PopFirst;
      AssertTrue(v = I);
    end;
  AssertTrue(d.Instance.IsEmpty);
end;

procedure TGDequeTest.PushAllDynArray;
var
  d: TAutoIntDeque;
  I, v: Integer;
begin
  AssertTrue({%H-}d.Instance.PushAllLast(TIntHelper.CreateCopy(IntArray21)) = 21);
  AssertTrue(d.Instance.Count = 21);
  AssertTrue(d.Instance.PushAllFirst(TIntHelper.CreateCopy(IntArray21)) = 21);
  AssertTrue(d.Instance.Count = 42);
  for I := 21 downto 1 do
    begin
      v := d.Instance.PopFirst;
      AssertTrue(v = I);
    end;
  AssertTrue(d.Instance.Count = 21);
  for I := 1 to 21 do
    begin
      v := d.Instance.PopFirst;
      AssertTrue(v = I);
    end;
  AssertTrue(d.Instance.IsEmpty);
end;

procedure TGDequeTest.PushAllEnum;
var
  d: TAutoIntDeque;
  I, v: Integer;
begin
  AssertTrue({%H-}d.Instance.PushAllLast(TIntEnumerable.Create(TIntHelper.CreateCopy(IntArray21))) = 21);
  AssertTrue(d.Instance.Count = 21);
  AssertTrue(d.Instance.PushAllFirst(TIntEnumerable.Create(TIntHelper.CreateCopy(IntArray21))) = 21);
  AssertTrue(d.Instance.Count = 42);
  for I := 21 downto 1 do
    begin
      v := d.Instance.PopFirst;
      AssertTrue(v = I);
    end;
  AssertTrue(d.Instance.Count = 21);
  for I := 1 to 21 do
    begin
      v := d.Instance.PopFirst;
      AssertTrue(v = I);
    end;
  AssertTrue(d.Instance.IsEmpty);
end;

procedure TGDequeTest.PushAllSelf;
var
  d: TAutoIntDeque;
  I, v: Integer;
begin
  AssertTrue({%H-}d.Instance.PushAllLast(IntArray21) = 21);
  AssertTrue(d.Instance.PushAllFirst(d.Instance) = 21);
  AssertTrue(d.Instance.Count = 42);
  for I := 21 downto 1 do
    begin
      v := d.Instance.PopFirst;
      AssertTrue(v = I);
    end;
  AssertTrue(d.Instance.Count = 21);
  AssertTrue(d.Instance.PushAllLast(d.Instance) = 21);
  AssertTrue(d.Instance.Count = 42);
  for I := 1 to 21 do
    begin
      v := d.Instance.PopFirst;
      AssertTrue(v = I);
    end;
  AssertTrue(d.Instance.Count = 21);
  for I := 21 downto 1 do
    begin
      v := d.Instance.PopLast;
      AssertTrue(v = I);
    end;
  AssertTrue(d.Instance.IsEmpty);
end;

procedure TGDequeTest.TryPopEmpty;
var
  d: TAutoIntDeque;
  v: Integer;
begin
  AssertFalse({%H-}d.Instance.TryPopFirst(v));
  AssertFalse(d.Instance.TryPopLast(v));
end;

procedure TGDequeTest.TryPop;
var
  d: TAutoIntDeque;
  v: Integer;
begin
  {%H-}d.Instance.PushLast(1);
  d.Instance.PushLast(2);
  d.Instance.PushLast(3);
  AssertTrue(d.Instance.TryPopFirst(v));
  AssertTrue(v = 1);
  AssertTrue(d.Instance.TryPopLast(v));
  AssertTrue(v = 3);
end;

procedure TGDequeTest.TryPeekEmpty;
var
  d: TAutoIntDeque;
  v: Integer;
begin
  AssertFalse({%H-}d.Instance.TryPeekFirst(v));
  AssertFalse(d.Instance.TryPeekLast(v));
end;

procedure TGDequeTest.TryPeek;
var
  d: TAutoIntDeque;
  v: Integer;
begin
  {%H-}d.Instance.PushLast(1);
  d.Instance.PushLast(2);
  d.Instance.PushLast(3);
  AssertTrue(d.Instance.TryPeekFirst(v));
  AssertTrue(v = 1);
  AssertTrue(d.Instance.TryPeekLast(v));
  AssertTrue(Format('%d expected, got %d', [3, v]),v = 3);
end;

procedure TGDequeTest.Insert;
var
  d: TAutoIntDeque;
  I: Integer;
begin
  {%H-}d.Instance.Insert(0, 4);
  d.Instance.Insert(1, 5);
  d.Instance.Insert(0, 2);
  d.Instance.Insert(3, 7);
  d.Instance.Insert(3, 6);
  d.Instance.Insert(1, 3);
  d.Instance.Insert(0, 1);
  d.Instance.Insert(0, 0);
  AssertTrue(d.Instance.Count = 8);
  for I := 0 to Pred(d.Instance.Count) do
    AssertTrue(d{%H-}.Instance[I] = I);
end;

procedure TGDequeTest.InsertOutOfBounds;
var
  d: TAutoIntDeque;
  Raised: Boolean;
begin
  d.Instance.Insert(0, 12);
  AssertTrue(d{%H-}.Instance[0] = 12);
  Raised := False;
  try
    d.Instance.Insert(2, 13);
  except
    on ELGListError do
      Raised := True;
  end;
  AssertTrue(Raised);
  Raised := False;
  try
    d.Instance.Insert(-3, 13);
  except
    on ELGListError do
      Raised := True;
  end;
  AssertTrue(Raised);
end;

procedure TGDequeTest.InsertStr;
var
  d: TAutoStrDeque;
  I: Integer;
begin
  for I := 1 to 50 do
    {%H-}d.Instance.PushLast('str ' + I.ToString);
  for I := 100 downto 51 do
    d.Instance.Insert(10, 'str ' + I.ToString);
  AssertTrue(d.Instance.Count = 100);
  for I := 59 downto 10 do
    AssertTrue(d{%H-}.Instance[I] = 'str ' + (I + 41).ToString);
end;

procedure TGDequeTest.Extract;
var
  d: TAutoIntDeque;
  I: Integer = 1;
begin
  {%H-}d.Instance := TIntDeque.Create(IntArray21);
  while d.Instance.NonEmpty do
    begin
      AssertTrue(d.Instance.Extract(0) = I);
      Inc(I);
    end;
end;

procedure TGDequeTest.ExtractStr;
var
  d: TAutoStrDeque;
  I: Integer;
begin
  for I := 0 to 99 do
    {%H-}d.Instance.PushLast('str ' + I.ToString);
  for I := 10 to 60 do
    AssertTrue(d.Instance.Extract(10) = 'str ' + I.ToString);
  AssertTrue(d.Instance.Count = 49);
end;

procedure TGDequeTest.Delete;
var
  d: TAutoIntDeque;
  I: Integer;
begin
  {%H-}d.Instance := TIntDeque.Create(IntArray21);
  I := 9;
  while d.Instance.Count > 7 do
    begin
      d.Instance.Delete(7);
      if d.Instance.Count > 7 then
        AssertTrue(d{%H-}.Instance[7] = I);
      Inc(I);
    end;
  I := 2;
  while d.Instance.NonEmpty do
    begin
      d.Instance.Delete(0);
      if d.Instance.NonEmpty then
        AssertTrue(d{%H-}.Instance[0] = I);
      Inc(I);
    end;
end;

procedure TGDequeTest.Clear;
var
  d: TAutoIntDeque;
begin
  {%H-}d.Instance := TIntDeque.Create(IntArray21);
  AssertTrue(d.Instance.Count = 21);
  AssertTrue(d.Instance.Capacity <> 0);
  d.Instance.Clear;
  AssertTrue(d.Instance.IsEmpty);
  AssertTrue(d.Instance.Capacity = 0);
end;

procedure TGDequeTest.TrimToFit;
var
  d: TAutoIntDeque;
begin
  {%H-}d.Instance := TIntDeque.Create(IntArray21[1..19]);
  AssertTrue(d.Instance.Count = 19);
  AssertTrue(d.Instance.Capacity = DEFAULT_CONTAINER_CAPACITY);
  d.Instance.TrimToFit;
  AssertTrue(d.Instance.Count = 19);
  AssertTrue(d.Instance.Capacity = 19);
end;

procedure TGDequeTest.TrimToFit_1;
var
  d: TAutoIntDeque;
  I, v: Integer;
begin
  for I := 32 downto 1 do
    {%H-}d.Instance.PushFirst(I);
  for I := 33 to 64 do
    d.Instance.PushLast(I);
  for I := 1 to 10 do
    d.Instance.PopFirst;
  for I := 1 to 10 do
    d.Instance.PopLast;       //11..54
  for I := 55 to 90 do
    d.Instance.PushLast(I);
  d.Instance.TrimToFit;
  AssertTrue(d.Instance.Count = 80);
  AssertTrue(d.Instance.Capacity = 80);
  d.Instance.EnsureCapacity(300);
  for I := 10 downto 1 do
    d.Instance.PushFirst(I);
  for I := 91 to 100 do
    d.Instance.PushLast(I);
  d.Instance.TrimToFit;
  AssertTrue(d.Instance.Count = 100);
  AssertTrue(d.Instance.Capacity = 100);
  d.Instance.EnsureCapacity(300);
  for I := 1 to 100 do
    begin
      v := d.Instance.PopFirst;
      AssertTrue(v = I);
    end;
  AssertTrue(d.Instance.IsEmpty);
end;

procedure TGDequeTest.TrimToFitStr;
var
  d: TAutoStrDeque;
  I: Integer;
  v: string;
begin
  for I := 32 downto 1 do
    {%H-}d.Instance.PushFirst('string ' + I.ToString);
  for I := 33 to 64 do
    d.Instance.PushLast('string ' + I.ToString);
  for I := 1 to 10 do
    d.Instance.PopFirst;
  for I := 1 to 10 do
    d.Instance.PopLast;       //11..54
  for I := 55 to 90 do
    d.Instance.PushLast('string ' + I.ToString);
  d.Instance.TrimToFit;
  AssertTrue(d.Instance.Count = 80);
  AssertTrue(d.Instance.Capacity = 80);
  d.Instance.EnsureCapacity(300);
  for I := 10 downto 1 do
    d.Instance.PushFirst('string ' + I.ToString);
  for I := 91 to 100 do
    d.Instance.PushLast('string ' + I.ToString);
  d.Instance.TrimToFit;
  AssertTrue(d.Instance.Count = 100);
  AssertTrue(d.Instance.Capacity = 100);
  d.Instance.EnsureCapacity(300);
  for I := 1 to 100 do
    begin
      v := d.Instance.PopFirst;
      AssertTrue(v = 'string ' + I.ToString);
    end;
  AssertTrue(d.Instance.IsEmpty);
end;

procedure TGDequeTest.EnumeratorInt;
var
  d: TAutoIntDeque;
  I, v: Integer;
begin
  for I := 32 downto 1 do
    d.Instance.PushFirst(I);
  for I := 33 to 64 do
    d.Instance.PushLast(I);
  for I := 1 to 10 do
    d.Instance.PopFirst;
  for I := 1 to 10 do
    d.Instance.PopLast;       //11..54
  for I := 55 to 90 do
    d.Instance.PushLast(I);
  d.Instance.TrimToFit;
  d.Instance.EnsureCapacity(300);
  for I := 10 downto 1 do
    d.Instance.PushFirst(I);
  for I := 91 to 100 do
    d.Instance.PushLast(I);
  d.Instance.TrimToFit;
  d.Instance.EnsureCapacity(300);
  I := 1;
  for v in d.Instance do
    begin
      AssertTrue(v = I);
      Inc(I);
    end;
end;

procedure TGDequeTest.EnumeratorStr;
var
  d: TAutoStrDeque;
  I: Integer;
  v: string;
begin
  for I := 32 downto 1 do
    d.Instance.PushFirst('string ' + I.ToString);
  for I := 33 to 64 do
    d.Instance.PushLast('string ' + I.ToString);
  for I := 1 to 10 do
    d.Instance.PopFirst;
  for I := 1 to 10 do
    d.Instance.PopLast;       //11..54
  for I := 55 to 90 do
    d.Instance.PushLast('string ' + I.ToString);
  d.Instance.TrimToFit;
  d.Instance.EnsureCapacity(300);
  for I := 10 downto 1 do
    d.Instance.PushFirst('string ' + I.ToString);
  for I := 91 to 100 do
    d.Instance.PushLast('string ' + I.ToString);
  d.Instance.TrimToFit;
  d.Instance.EnsureCapacity(300);
  I := 1;
  for v in d.Instance do
    begin
      AssertTrue(v = 'string ' + I.ToString);
      Inc(I);
    end;
end;

procedure TGDequeTest.Reverse;
var
  q: TAutoIntDeque;
  I, v: Integer;
begin
  for I := 1 to 64 do
    q.Instance.PushLast(I);
  q.Instance.TrimToFit;
  for I := 1 to 40 do
    begin
      v := q{%H-}.Instance.PopFirst;
      AssertTrue(v = I);
    end;
  for I := 65 to 85 do
    q.Instance.PushLast(I);
  AssertTrue(q.Instance.Count = 45);
  q.Instance.EnsureCapacity(1022);
  v := 85;
  for I in q.Instance.Reverse do
    begin
      AssertTrue(I = v);
      Dec(v);
    end;
end;

procedure TGDequeTest.ToArrayInt;
var
  d: TAutoIntDeque;
  I: Integer;
  a: TIntArray;
begin
  for I := 32 downto 1 do
    {%H-}d.Instance.PushFirst(I);
  for I := 33 to 64 do
    d.Instance.PushLast(I);
  for I := 1 to 10 do
    d.Instance.PopFirst;
  for I := 1 to 10 do
    d.Instance.PopLast;       //11..54
  for I := 55 to 90 do
    d.Instance.PushLast(I);
  d.Instance.TrimToFit;
  d.Instance.EnsureCapacity(300);
  for I := 10 downto 1 do
    d.Instance.PushFirst(I);
  for I := 91 to 100 do
    d.Instance.PushLast(I);
  d.Instance.TrimToFit;
  d.Instance.EnsureCapacity(300);
  a := d.Instance.ToArray;
  AssertTrue(System.Length(a) = 100);
  for I := 1 to 100 do
    AssertTrue(a[I - 1] = I);
end;

procedure TGDequeTest.ToArrayStr;
var
  d: TAutoStrDeque;
  I: Integer;
  a: TStrArray;
begin
  for I := 32 downto 1 do
    {%H-}d.Instance.PushFirst('string ' + I.ToString);
  for I := 33 to 64 do
    d.Instance.PushLast('string ' + I.ToString);
  for I := 1 to 10 do
    d.Instance.PopFirst;
  for I := 1 to 10 do
    d.Instance.PopLast;       //11..54
  for I := 55 to 90 do
    d.Instance.PushLast('string ' + I.ToString);
  d.Instance.EnsureCapacity(300);
  for I := 10 downto 1 do
    d.Instance.PushFirst('string ' + I.ToString);
  for I := 91 to 100 do
    d.Instance.PushLast('string ' + I.ToString);
  d.Instance.TrimToFit;
  d.Instance.EnsureCapacity(300);
  a := d.Instance.ToArray;
  AssertTrue(System.Length(a) = 100);
  for I := 1 to 100 do
    AssertTrue(a[I - 1] = 'string ' + I.ToString);
end;

procedure TGDequeTest.ObjectDeque;
var
  Counter: TCounter;
  d: TAutoObjDeque;
  I: Integer;
begin
  Counter := Default(TCounter);
  for I := 1 to 100 do
    {%H-}d.Instance.PushFirst(TTestObj.Create(Counter.Increment));
  AssertTrue(d.Instance.Count = 100);
  d.Instance.TrimToFit;
  for I := 1 to 20 do
    d.Instance.PopFirst.Free;
  AssertTrue(Counter.Count = 20);
  AssertTrue(d.Instance.Count = 80);
  for I := 1 to 20 do
    d.Instance.PushLast(TTestObj.Create(Counter.Increment));
  AssertTrue(d.Instance.Count = 100);
  d.Instance := Default(TObjDeque);
  AssertTrue(Counter.Count = 120);
end;

procedure TGDequeTest.ObjectSetItem;
var
  Counter: TCounter;
  d: TAutoObjDeque;
  a: TObjArray;
  I, TestSize: Integer;
begin
  Counter := Default(TCounter);
  TestSize := 100;
  for I := 1 to TestSize do
    {%H-}d.Instance.Insert({%H-}d.Instance.Count, TTestObj.Create(Counter.Increment));
  AssertTrue(Counter.Count = 0);
  System.SetLength(a, TestSize);
  for I := 0 to System.High(a) do
    a[I] := TTestObj.Create(Counter.Increment);
  for I := 0 to Pred(d.Instance.Count) do
    d.Instance[I] := a[I];
  AssertTrue(Counter.Count = TestSize);
  d.Instance.Clear;
  AssertTrue(Counter.Count = TestSize * 2);
end;

procedure TGDequeTest.ObjectExtract;
var
  Counter: TCounter;
  d: TAutoObjDeque;
  a: TObjArray;
  I, TestSize: Integer;
begin
  Counter := Default(TCounter);
  TestSize := 100;
  for I := 1 to TestSize do
    {%H-}d.Instance.PushFirst(TTestObj.Create(Counter.Increment));
  AssertTrue(Counter.Count = 0);
  System.SetLength(a, TestSize);
  I := 0;
  while d.Instance.NonEmpty do
    begin
      a[I] := d.Instance.Extract(0);
      Inc(I);
    end;
  AssertTrue(Counter.Count = 0);
  for I := 0 to System.High(a) do
    a[I].Free;
  AssertTrue(Counter.Count = TestSize);
end;

procedure TGDequeTest.ObjectDelete;
var
  Counter: TCounter;
  d: TAutoObjDeque;
  I, TestSize: Integer;
begin
  Counter := Default(TCounter);
  TestSize := 100;
  for I := 1 to TestSize do
    {%H-}d.Instance.PushFirst(TTestObj.Create(Counter.Increment));
  AssertTrue(Counter.Count = 0);
  I := 0;
  while d.Instance.NonEmpty do
    d.Instance.Delete(0);
  AssertTrue(Counter.Count = TestSize);
end;

{ TLiteDequeTest.TTestObj }

constructor TLiteDequeTest.TTestObj.Create(aProc: TProc);
begin
  FProc := aProc;
end;

destructor TLiteDequeTest.TTestObj.Destroy;
begin
  if FProc <> nil then
    FProc;
  inherited;
end;

{ TLiteDequeTest }

procedure TLiteDequeTest.EnsureCapacity;
var
  d: TIntDeque;
begin
  {%H-}d.EnsureCapacity(11);
  AssertTrue(d.Capacity = DEFAULT_CONTAINER_CAPACITY);
  d.EnsureCapacity(200);
  AssertTrue(d.Capacity = 256);
end;

procedure TLiteDequeTest.PushPop;
var
  d: TIntDeque;
  I, J: Integer;
begin
  {%H-}d.PushLast(1);
  AssertTrue(d.NonEmpty);
  AssertTrue(d.Count = 1);
  d.PushLast(2);
  AssertTrue(d.Count = 2);
  d.PushLast(3);
  AssertTrue(d.Count = 3);
  d.PushFirst(0);
  AssertTrue(d.Count = 4);
  d.PushFirst(-1);
  AssertTrue(d.Count = 5);
  d.PushFirst(-2);
  AssertTrue(d.Count = 6);
  for I := -2 to 3 do
    begin
      J := d{%H-}.PopFirst;
      AssertTrue(Format('%d expected, got %d', [I, J]), J = I);
    end;
  AssertTrue(d.IsEmpty);
end;

procedure TLiteDequeTest.PushPop_1;
var
  d: TIntDeque;
  I, J: Integer;
begin
  {%H-}d.PushLast(1);
  d.PushLast(2);
  d.PushLast(3);
  d.PushFirst(0);
  d.PushFirst(-1);
  d.PushFirst(-2);
  AssertTrue(d.Count = 6);
  for I := 3 downto -2 do
    begin
      J := d{%H-}.PopLast;
      AssertTrue(J = I);
    end;
  AssertTrue(d.IsEmpty);
end;

procedure TLiteDequeTest.TryPopEmpty;
var
  d: TIntDeque;
  I: Integer;
begin
  AssertFalse({%H-}d.TryPopFirst(I){%H-});
  AssertFalse(d.TryPopLast(I){%H-});
end;

procedure TLiteDequeTest.TryPop;
var
  d: TIntDeque;
  I: Integer;
begin
  {%H-}d.PushLast(1);
  d.PushLast(2);
  d.PushLast(3);
  AssertTrue(d.TryPopFirst(I){%H-});
  AssertTrue(I = 1);
  AssertTrue(d.TryPopLast(I){%H-});
  AssertTrue(I = 3);
end;

procedure TLiteDequeTest.TryPeekEmpty;
var
  d: TIntDeque;
  I: Integer;
begin
  AssertFalse({%H-}d.TryPeekFirst(I));
  AssertFalse(d.TryPeekLast(I){%H-});
end;

procedure TLiteDequeTest.TryPeek;
var
  d: TIntDeque;
  I: Integer;
begin
  {%H-}d.PushLast(1);
  d.PushLast(2);
  d.PushLast(3);
  AssertTrue(d.TryPeekFirst(I));
  AssertTrue(I = 1);
  AssertTrue(d.TryPeekLast(I){%H-});
  AssertTrue(I = 3);
end;

procedure TLiteDequeTest.Insert;
var
  d: TIntDeque;
  I: Integer;
begin
  {%H-}d.Insert(0, 4);
  d.Insert(1, 5);
  d.Insert(0, 2);
  d.Insert(3, 7);
  d.Insert(3, 6);
  d.Insert(1, 3);
  d.Insert(0, 1);
  d.Insert(0, 0);
  AssertTrue(d.Count = 8);
  for I := 0 to Pred(d.Count) do
    AssertTrue(d{%H-}[I] = I);
end;

procedure TLiteDequeTest.InsertOutOfBounds;
var
  d: TIntDeque;
  Raised: Boolean;
begin
  d.Insert(0, 12);
  AssertTrue(d{%H-}[0] = 12);
  Raised := False;
  try
    d.Insert(2, 13);
  except
    on ELGListError do
      Raised := True;
  end;
  AssertTrue(Raised);

  Raised := False;
  try
    d.Insert(-3, 13);
  except
    on ELGListError do
      Raised := True;
  end;
  AssertTrue(Raised);
end;

procedure TLiteDequeTest.InsertStr;
var
  d: TStrDeque;
  I: Integer;
begin
  for I := 1 to 50 do
    {%H-}d.PushLast('str ' + I.ToString);
  for I := 100 downto 51 do
    d.Insert(10, 'str ' + I.ToString);
  AssertTrue(d.Count = 100);
  for I := 59 downto 10 do
    AssertTrue(d{%H-}[I] = 'str ' + (I + 41).ToString);
end;

procedure TLiteDequeTest.DeleteStr;
var
  d: TStrDeque;
  I: Integer;
begin
  for I := 0 to 99 do
    {%H-}d.PushLast('str ' + I.ToString);
  for I := 10 to 60 do
    AssertTrue(d.Delete(10) = 'str ' + I.ToString);
  AssertTrue(d.Count = 49);
end;

procedure TLiteDequeTest.Delete;
var
  d: TIntDeque;
  I: Integer;
begin
  for I in [1..21] do
    {%H-}d.PushLast(I);
  I := 1;
  while d.NonEmpty do
    begin
      AssertTrue(d.Delete(0) = I);
      Inc(I);
    end;
end;

procedure TLiteDequeTest.Clear;
var
  d: TIntDeque;
  I: Integer;
begin
  for I in [1..21] do
    {%H-}d.PushLast(I);
  AssertTrue(d.Count = 21);
  AssertTrue(d.Capacity = DEFAULT_CONTAINER_CAPACITY);
  d.Clear;
  AssertTrue(d.IsEmpty);
  AssertTrue(d.Capacity = 0);
end;

procedure TLiteDequeTest.TrimToFit;
var
  d: TIntDeque;
  I: Integer;
begin
  for I in [1..21] do
    {%H-}d.PushLast(I);
  AssertTrue(d.Count = 21);
  AssertTrue(d.Capacity = DEFAULT_CONTAINER_CAPACITY);
  d.TrimToFit;
  AssertTrue(d.Count = 21);
  AssertTrue(d.Capacity = 21);
end;

procedure TLiteDequeTest.TrimToFit_1;
var
  d: TIntDeque;
  I, J: Integer;
begin
  for I := 32 downto 1 do
    {%H-}d.PushFirst(I);
  for I := 33 to 64 do
    {%H-}d.PushLast(I);
  for I := 1 to 10 do
    {%H-}d.PopFirst;
  for I := 1 to 10 do
    {%H-}d.PopLast;
  for I := 55 to 90 do
    d.PushLast(I);
  d.TrimToFit;
  AssertTrue(d.Count = 80);
  AssertTrue(d.Capacity = 80);
  d.EnsureCapacity(300);
  for I := 10 downto 1 do
    d.PushFirst(I);
  for I := 91 to 100 do
    d.PushLast(I);
  d.TrimToFit;
  AssertTrue(d.Count = 100);
  AssertTrue(d.Capacity = 100);
  d.EnsureCapacity(300);
  for I := 1 to 100 do
    begin
      J := d{%H-}.PopFirst;
      AssertTrue(J = I);
    end;
  AssertTrue(d.IsEmpty);
end;

procedure TLiteDequeTest.TrimToFitStr;
var
  d: TStrDeque;
  I: Integer;
  s: string;
begin
  for I := 32 downto 1 do
    {%H-}d.PushFirst('string ' + I.ToString);
  for I := 33 to 64 do
    d.PushLast('string ' + I.ToString);
  for I := 1 to 10 do
    {%H-}d.PopFirst;
  for I := 1 to 10 do
    {%H-}d.PopLast;
  for I := 55 to 90 do
    d.PushLast('string ' + I.ToString);
  d.TrimToFit;
  AssertTrue(d.Count = 80);
  AssertTrue(d.Capacity = 80);
  d.EnsureCapacity(300);
  for I := 10 downto 1 do
    d.PushFirst('string ' + I.ToString);
  for I := 91 to 100 do
    d.PushLast('string ' + I.ToString);
  d.TrimToFit;
  AssertTrue(d.Count = 100);
  AssertTrue(d.Capacity = 100);
  d.EnsureCapacity(300);
  for I := 1 to 100 do
    begin
      s := d{%H-}.PopFirst;
      AssertTrue(s = 'string ' + I.ToString);
    end;
  AssertTrue(d.IsEmpty);
end;

procedure TLiteDequeTest.EnumeratorInt;
var
  d: TIntDeque;
  I, J: Integer;
begin
  for I := 32 downto 1 do
    {%H-}d.PushFirst(I);
  for I := 33 to 64 do
    d.PushLast(I);
  for I := 1 to 10 do
    {%H-}d.PopFirst;
  for I := 1 to 10 do
    {%H-}d.PopLast;
  for I := 55 to 90 do
    d.PushLast(I);
  d.TrimToFit;
  d.EnsureCapacity(300);
  for I := 10 downto 1 do
    d.PushFirst(I);
  for I := 91 to 100 do
    d.PushLast(I);
  d.TrimToFit;
  d.EnsureCapacity(300);
  I := 1;
  for {%H-}J in d do
    begin
      AssertTrue(J = I);
      Inc(I);
    end;
end;

procedure TLiteDequeTest.EnumeratorStr;
var
  d: TStrDeque;
  I: Integer;
  s: string;
begin
  for I := 32 downto 1 do
    {%H-}d.PushFirst('string ' + I.ToString);
  for I := 33 to 64 do
    d.PushLast('string ' + I.ToString);
  for I := 1 to 10 do
    {%H-}d.PopFirst;
  for I := 1 to 10 do
    {%H-}d.PopLast;
  for I := 55 to 90 do
    d.PushLast('string ' + I.ToString);
  d.TrimToFit;
  d.EnsureCapacity(300);
  for I := 10 downto 1 do
    d.PushFirst('string ' + I.ToString);
  for I := 91 to 100 do
    d.PushLast('string ' + I.ToString);
  d.TrimToFit;
  d.EnsureCapacity(300);
  I := 1;
  for {%H-}s in d do
    begin
      AssertTrue(s = 'string ' + I.ToString);
      Inc(I);
    end;
end;

procedure TLiteDequeTest.Reverse;
var
  d: TIntDeque;
  I, J: Integer;
begin
  for I := 32 downto 1 do
    {%H-}d.PushFirst(I);
  for I := 33 to 64 do
    d.PushLast(I);
  for I := 1 to 10 do
    {%H-}d.PopFirst;
  for I := 1 to 10 do
    {%H-}d.PopLast;
  for I := 55 to 90 do
    d.PushLast(I);
  d.TrimToFit;
  d.EnsureCapacity(300);
  for I := 10 downto 1 do
    d.PushFirst(I);
  for I := 91 to 100 do
    d.PushLast(I);
  d.TrimToFit;
  d.EnsureCapacity(300);
  I := 100;
  for {%H-}J in d.Reverse do
    begin
      AssertTrue(J = I);
      Dec(I);
    end;
end;

procedure TLiteDequeTest.Mutables;
var
  d: TIntDeque;
  I, J: Integer;
  p: TIntDeque.PItem;
begin
  for I := 1 to 50 do
    {%H-}d.PushLast(I);
  for {%H-}p in d.Mutables do
    p^ := p^ + 37;
  J := 1;
  for {%H-}I in d do
    begin
      AssertTrue(I = J + 37);
      Inc(J);
    end;
end;

procedure TLiteDequeTest.ToArray;
var
  d: TStrDeque;
  I: Integer;
  a: TStringArray;
begin
  for I := 32 downto 1 do
    {%H-}d.PushFirst('string ' + I.ToString);
  for I := 33 to 64 do
    d.PushLast('string ' + I.ToString);
  for I := 1 to 10 do
    {%H-}d.PopFirst;
  for I := 1 to 10 do
    {%H-}d.PopLast;
  for I := 55 to 90 do
    d.PushLast('string ' + I.ToString);
  d.EnsureCapacity(300);
  for I := 10 downto 1 do
    d.PushFirst('string ' + I.ToString);
  for I := 91 to 100 do
    d.PushLast('string ' + I.ToString);
  d.TrimToFit;
  d.EnsureCapacity(300);
  a := d.ToArray;
  AssertTrue(Length(a) = 100);
  for I := 1 to 100 do
    AssertTrue(a[I - 1] = 'string ' + I.ToString);
end;

procedure TLiteDequeTest.ObjectDeque;
var
  Counter: Integer = 0;
  d: TObjDeque;
  I: Integer;
  procedure IncCounter;
  begin
    Inc(Counter);
  end;
begin
  for I := 1 to 100 do
    {%H-}d.PushFirst(TTestObj.Create(@IncCounter));
  AssertTrue(d.Count = 100);
  for I := 1 to 20 do
    d.PopFirst{%H-}.Free;
  AssertTrue(Counter = 20);
  AssertTrue(d.Count = 80);
  for I := 1 to 20 do
    d.PushLast(TTestObj.Create(@IncCounter));
  AssertTrue(d.Count = 100);
  d := Default(TObjDeque);
  AssertTrue(Counter = 120);
end;

procedure TLiteDequeTest.ObjectSetItem;
var
  Counter: Integer = 0;
  d: TObjDeque;
  a: array of TTestObj;
  I, TestSize: Integer;
  procedure IncCounter;
  begin
    Inc(Counter);
  end;
begin
  TestSize := 100;
  for I := 1 to TestSize do
    {%H-}d.Insert({%H-}d.Count, TTestObj.Create(@IncCounter));
  AssertTrue(Counter = 0);
  System.SetLength(a, TestSize);
  for I := 0 to System.High(a) do
    a[I] := TTestObj.Create(@IncCounter);
  for I := 0 to Pred(d.Count) do
    d[I] := a[I];
  AssertTrue(Counter = TestSize);
  d.Clear;
  AssertTrue(Counter = TestSize * 2);
end;

procedure TLiteDequeTest.ObjectExtract;
var
  Counter: Integer = 0;
  d: TObjDeque;
  a: array of TTestObj;
  I, TestSize: Integer;
  procedure IncCounter;
  begin
    Inc(Counter);
  end;
begin
  TestSize := 100;
  for I := 1 to TestSize do
    {%H-}d.PushFirst(TTestObj.Create(@IncCounter));
  AssertTrue(Counter = 0);
  System.SetLength(a, TestSize);
  I := 0;
  while d.NonEmpty do
    begin
      a[I] := d.Extract(0);
      Inc(I);
    end;
  AssertTrue(Counter = 0);
  for I := 0 to System.High(a) do
    a[I].Free;
  AssertTrue(Counter = TestSize);
end;

procedure TLiteDequeTest.ObjectDelete;
var
  Counter: Integer = 0;
  d: TObjDeque;
  I, TestSize: Integer;
  procedure IncCounter;
  begin
    Inc(Counter);
  end;
begin
  TestSize := 100;
  for I := 1 to TestSize do
    {%H-}d.PushFirst(TTestObj.Create(@IncCounter));
  AssertTrue(Counter = 0);
  I := 0;
  while d.NonEmpty do
    d.Delete(0);
  AssertTrue(Counter = TestSize);
end;

initialization
  RegisterTest(TGDequeTest);
  RegisterTest(TLiteDequeTest);
end.

