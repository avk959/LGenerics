unit LGStackTest;

{$mode objfpc}{$H+}
{$MODESWITCH NESTEDPROCVARS}

interface

uses

  SysUtils, fpcunit, testregistry,
  LGUtils,
  LGCustomContainer,
  LGStack,
  LGArrayHelpers;

type

  TGStackTest = class(TTestCase)
  private
  type
    TIntStack      = class(specialize TGStack<Integer>);
    TAutoIntStack  = specialize TGAutoRef<TIntStack>;
    TIntArray      = specialize TGArray<Integer>;
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

    TObjStack     = class(specialize TGObjectStack<TTestObj>);
    TAutoObjStack = specialize TGAutoRef<TObjStack>;

  const
    IntArray21: array[1..21] of Integer =
      (1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21);
  published
    procedure CreateEmpty;
    procedure CreateCapacity0;
    procedure CreateCapacityNeg;
    procedure CreateCapacity1024;
    procedure CreateEmptyArray;
    procedure CreateStaticArray21;
    procedure CreateDynArray21;
    procedure CreateEnum21;
    procedure EnsureCapacity0;
    procedure EnsureCapacity11;
    procedure IsEmpty;
    procedure NonEmpty;
    procedure Push;
    procedure PushAllStaticArray21;
    procedure PushAllDynArray21;
    procedure PushAllEnum21;
    procedure PushAllSelf;
    procedure Reverse;
    procedure PopEmpty;
    procedure Pop21;
    procedure TryPopEmpty;
    procedure TryPop21;
    procedure PeekEmpty;
    procedure Peek21;
    procedure TryPeekEmpty;
    procedure TryPeek21;
    procedure Clear21;
    procedure TrimToFit21;
    procedure InIteration;
    procedure IterationDone;
    procedure ObjectStack;
  end;

  TLiteStackTest = class(TTestCase)
  private
  type
    TProc = procedure is nested;

    TTestObj = class
    private
      FProc: TProc;
    public
      constructor Create(aProc: TProc);
      destructor Destroy; override;
    end;

    TStack    = specialize TGLiteStack<Integer>;
    TObjStack = specialize TGLiteObjectStack<TTestObj>;

  published
    procedure EnsureCapacity;
    procedure Push;
    procedure IsEmpty;
    procedure NonEmpty;
    procedure Reverse;
    procedure Mutables;
    procedure ToArray;
    procedure PopEmpty;
    procedure Pop;
    procedure TryPopEmpty;
    procedure TryPop;
    procedure PeekEmpty;
    procedure Peek;
    procedure TryPeekEmpty;
    procedure TryPeek;
    procedure Clear;
    procedure TrimToFit;
    procedure ObjectStack;
  end;

implementation
{$B-}{$COPERATORS ON}

{ TGStackTest.TTestObj }

constructor TGStackTest.TTestObj.Create(aProc: TObjProc);
begin
  FProc := aProc;
end;

destructor TGStackTest.TTestObj.Destroy;
begin
  if FProc <> nil then
    FProc;
  inherited;
end;

{ TGStackTest.TCounter }

procedure TGStackTest.TCounter.IncCount;
begin
  Inc(FCount);
end;

function TGStackTest.TCounter.GetIncrement: TObjProc;
begin
  Result := @IncCount;
end;

procedure TGStackTest.CreateEmpty;
var
  s: TAutoIntStack;
begin
  AssertTrue({%H-}s.Instance.Count = 0);
  AssertTrue(s.Instance.Capacity = DEFAULT_CONTAINER_CAPACITY);
end;

procedure TGStackTest.CreateCapacity0;
var
  s: TAutoIntStack;
begin
  {%H-}s.Instance := TIntStack.Create(0);
  AssertTrue(s.Instance.Count = 0);
  AssertTrue(s.Instance.Capacity = 0);
end;

procedure TGStackTest.CreateCapacityNeg;
var
  s: TAutoIntStack;
begin
  {%H-}s.Instance := TIntStack.Create(-15);
  AssertTrue(s.Instance.Count = 0);
  AssertTrue(s.Instance.Capacity = 0);
end;

procedure TGStackTest.CreateCapacity1024;
var
  s: TAutoIntStack;
begin
  {%H-}s.Instance := TIntStack.Create(1024);
  AssertTrue(s.Instance.Count = 0);
  AssertTrue(s.Instance.Capacity = 1024);
end;

procedure TGStackTest.CreateEmptyArray;
var
  s: TAutoIntStack;
  a: TIntArray;
begin
  {%H-}s.Instance := TIntStack.Create(a{%H-});
  AssertTrue(s.Instance.Count = 0);
  AssertTrue(s.Instance.Capacity = DEFAULT_CONTAINER_CAPACITY);
end;

procedure TGStackTest.CreateStaticArray21;
var
  s: TAutoIntStack;
begin
  {%H-}s.Instance := TIntStack.Create(IntArray21);
  AssertTrue(s.Instance.Count = 21);
  AssertTrue(s.Instance.Capacity = DEFAULT_CONTAINER_CAPACITY);
  AssertTrue(TIntHelper.Same(s.Instance.ToArray, IntArray21));
end;

procedure TGStackTest.CreateDynArray21;
var
  s: TAutoIntStack;
begin
  {%H-}s.Instance := TIntStack.Create(TIntHelper.CreateCopy(IntArray21));
  AssertTrue(s.Instance.Count = 21);
  AssertTrue(s.Instance.Capacity = DEFAULT_CONTAINER_CAPACITY);
  AssertTrue(TIntHelper.Same(s.Instance.ToArray, IntArray21));
end;

procedure TGStackTest.CreateEnum21;
var
  s: TAutoIntStack;
begin
  {%H-}s.Instance := TIntStack.Create(TIntEnumerable.Create(TIntHelper.CreateCopy(IntArray21)));
  AssertTrue(s.Instance.Count = 21);
  AssertTrue(s.Instance.Capacity = DEFAULT_CONTAINER_CAPACITY);
  AssertTrue(TIntHelper.Same(s.Instance.ToArray, IntArray21));
end;

procedure TGStackTest.EnsureCapacity0;
var
  s: TAutoIntStack;
begin
  {%H-}s.Instance := TIntStack.Create(0);
  s.Instance.EnsureCapacity(0);
  AssertTrue(s.Instance.Capacity = 0);
end;

procedure TGStackTest.EnsureCapacity11;
var
  s: TAutoIntStack;
begin
  {%H-}s.Instance := TIntStack.Create(0);
  s.Instance.EnsureCapacity(11);
  AssertTrue(s.Instance.Capacity = DEFAULT_CONTAINER_CAPACITY);
end;

procedure TGStackTest.IsEmpty;
var
  s: TAutoIntStack;
begin
  AssertTrue({%H-}s.Instance.IsEmpty);
  s.Instance.Push(-2);
  AssertFalse(s.Instance.IsEmpty);
end;

procedure TGStackTest.NonEmpty;
var
  s: TAutoIntStack;
begin
  AssertFalse({%H-}s.Instance.NonEmpty);
  s.Instance.Push(-2);
  AssertTrue(s.Instance.NonEmpty);
end;

procedure TGStackTest.Push;
var
  s: TAutoIntStack;
begin
  {%H-}s.Instance.Push(5);
  AssertTrue(s.Instance.Count = 1);
  s.Instance.Push(10);
  AssertTrue(s.Instance.Count = 2);
  s.Instance.Push(15);
  AssertTrue(s.Instance.Count = 3);
end;

procedure TGStackTest.PushAllStaticArray21;
var
  s: TAutoIntStack;
begin
  {%H-}s.Instance.Push(5);
  AssertTrue(s.Instance.PushAll(IntArray21) = 21);
  AssertTrue(s.Instance.Count = 22);
end;

procedure TGStackTest.PushAllDynArray21;
var
  s: TAutoIntStack;
begin
  {%H-}s.Instance.Push(5);
  AssertTrue(s.Instance.PushAll(TIntHelper.CreateCopy(IntArray21)) = 21);
  AssertTrue(s.Instance.Count = 22);
end;

procedure TGStackTest.PushAllEnum21;
var
  s: TAutoIntStack;
begin
  {%H-}s.Instance.Push(5);
  AssertTrue(s.Instance.PushAll(TIntEnumerable.Create(TIntHelper.CreateCopy(IntArray21))) = 21);
  AssertTrue(s.Instance.Count = 22);
end;

procedure TGStackTest.PushAllSelf;
var
  s: TAutoIntStack;
  I: Integer;
begin
  {%H-}s.Instance.PushAll(IntArray21);
  AssertTrue(s.Instance.PushAll(s.Instance) = 21);
  for I := 21 downto 1 do
    AssertTrue(s.Instance.Pop = I);
  for I := 21 downto 1 do
    AssertTrue(s.Instance.Pop = I);
  AssertTrue(s.Instance.IsEmpty);
end;

procedure TGStackTest.Reverse;
var
  s: TAutoIntStack;
  I, J: Integer;
begin
  s.Instance.PushAll(IntArray21);
  J := 21;
  for I in s.Instance.Reverse do
    begin
      AssertTrue(I = J);
      Dec(J);
    end;
end;

procedure TGStackTest.PopEmpty;
var
  s: TAutoIntStack;
  Raised: Boolean = False;
begin
  try
    s.Instance.Pop;
  except
    on e: ELGAccessEmpty do
      Raised := True;
  end;
  AssertTrue(Raised);
end;

procedure TGStackTest.Pop21;
var
  s: TAutoIntStack;
  I, v: Integer;
begin
  {%H-}s.Instance := TIntStack.Create(IntArray21);
  for I := 21 downto 1 do
    begin
      v := s.Instance.Pop;
      AssertTrue(v = I);
    end;
  AssertTrue(s.Instance.IsEmpty);
end;

procedure TGStackTest.TryPopEmpty;
var
  s: TAutoIntStack;
  v: Integer;
begin
  AssertFalse({%H-}s.Instance.TryPop(v));
end;

procedure TGStackTest.TryPop21;
var
  s: TAutoIntStack;
  I, v: Integer;
begin
  {%H-}s.Instance := TIntStack.Create(IntArray21);
  for I := 21 downto 1 do
    begin
      AssertTrue(s.Instance.TryPop(v));
      AssertTrue(v = I);
    end;
  AssertTrue(s.Instance.IsEmpty);
end;

procedure TGStackTest.PeekEmpty;
var
  s: TAutoIntStack;
  Raised: Boolean = False;
begin
  try
    {%H-}s.Instance.Peek;
  except
    on e: ELGAccessEmpty do
      Raised := True;
  end;
  AssertTrue(Raised);
end;

procedure TGStackTest.Peek21;
var
  s: TAutoIntStack;
  I, v: Integer;
begin
  {%H-}s.Instance := TIntStack.Create(IntArray21);
  v := s{%H-}.Instance.Peek;
  AssertTrue(v = 21);
  v := s{%H-}.Instance.Peek;
  AssertTrue(v = 21);
  for I := 21 downto 1 do
    begin
      v := s{%H-}.Instance.Peek;
      s.Instance.Pop;
      AssertTrue(v = I);
    end;
end;

procedure TGStackTest.TryPeekEmpty;
var
  s: TAutoIntStack;
  v: Integer;
begin
  AssertFalse({%H-}s.Instance.TryPeek(v));
end;

procedure TGStackTest.TryPeek21;
var
  s: TAutoIntStack;
  I, v: Integer;
begin
  {%H-}s.Instance := TIntStack.Create(IntArray21);
  AssertTrue(s.Instance.TryPeek(v));
  AssertTrue(v = 21);
  AssertTrue(s.Instance.TryPeek(v));
  AssertTrue(v = 21);
  for I := 21 downto 1 do
    begin
      AssertTrue(s.Instance.TryPeek(v));
      AssertTrue(v = I);
      s.Instance.Pop;
    end;
end;

procedure TGStackTest.Clear21;
var
  s: TAutoIntStack;
begin
  {%H-}s.Instance := TIntStack.Create(IntArray21);
  AssertTrue(s.Instance.Count = 21);
  AssertTrue(s.Instance.Capacity = DEFAULT_CONTAINER_CAPACITY);
  s.Instance.Clear;
  AssertTrue(s.Instance.IsEmpty);
  AssertTrue(s.Instance.Capacity = 0);
end;

procedure TGStackTest.TrimToFit21;
var
  s: TAutoIntStack;
begin
  {%H-}s.Instance := TIntStack.Create(IntArray21);
  AssertTrue(s.Instance.Count = 21);
  AssertTrue(s.Instance.Capacity = DEFAULT_CONTAINER_CAPACITY);
  s.Instance.EnsureCapacity(255);
  AssertTrue(s.Instance.Capacity = 256);
  s.Instance.TrimToFit;
  AssertTrue(s.Instance.Count = 21);
  AssertTrue(s.Instance.Capacity = 21);
end;

procedure TGStackTest.InIteration;
  function IsEven(constref aValue: Integer): Boolean;
  begin
    Result := not Odd(aValue);
  end;
var
  s: TAutoIntStack;
  I: Integer;
  Raised: Boolean = False;
begin
  s.Instance := TIntStack.Create(IntArray21);
  try
    for I in s.Instance.Select(@IsEven) do
      begin
        if not Odd(I) then
        s.Instance.Pop;
      end;
  except
    on e: ELGUpdateLock do
      Raised := True;
  end;
  AssertTrue(Raised);
end;

procedure TGStackTest.IterationDone;
  function IsEven(constref aValue: Integer): Boolean;
  begin
    Result := not Odd(aValue);
  end;
  function Double(constref aValue: Integer): Integer;
  begin
    Result := aValue + aValue;
  end;
var
  s: TAutoIntStack;
  I, J: Integer;
begin
  s.Instance := TIntStack.Create(IntArray21);
  J := 2;
  for I in s.Instance.Select(@IsEven).Map(@Double) do
    begin
      AssertTrue(I = J * 2);
      J += 2;
    end;
  //shouldn't raise exeption
  s.Instance.Push(22);
  AssertTrue(s.Instance.Count = 22);
end;

procedure TGStackTest.ObjectStack;
var
  Counter: TCounter;
  s: TAutoObjStack;
  I: Integer;
begin
  Counter := Default(TCounter);
  for I := 1 to 100 do
    {%H-}s.Instance.Push(TTestObj.Create(Counter.Increment));
  AssertTrue(s.Instance.Count = 100);
  for I := 1 to 20 do
    s.Instance.Pop.Free;
  AssertTrue(Counter.Count = 20);
  AssertTrue(s.Instance.Count = 80);
  s.Instance := Default(TObjStack);
  AssertTrue(Counter.Count = 100);
end;

{ TLiteStackTest.TTestObj }

constructor TLiteStackTest.TTestObj.Create(aProc: TProc);
begin
  FProc := aProc;
end;

destructor TLiteStackTest.TTestObj.Destroy;
begin
  if FProc <> nil then
    FProc;
  inherited;
end;

{ TLiteStackTest }

procedure TLiteStackTest.EnsureCapacity;
var
  s: TStack;
begin
  AssertTrue({%H-}s.Capacity = 0);
  s.EnsureCapacity(0);
  AssertTrue(s.Capacity = 0);
  s.EnsureCapacity(11);
  AssertTrue(s.Capacity = DEFAULT_CONTAINER_CAPACITY);
end;

procedure TLiteStackTest.Push;
var
  s: TStack;
begin
  AssertTrue({%H-}s.Capacity = 0);
  {%H-}s.Push(5);
  AssertTrue(s.Count = 1);
  s.Push(10);
  AssertTrue(s.Count = 2);
  s.Push(15);
  AssertTrue(s.Count = 3);
end;

procedure TLiteStackTest.IsEmpty;
var
  s: TStack;
begin
  AssertTrue({%H-}s.IsEmpty);
  s.Push(3);
  AssertFalse(s.IsEmpty);
end;

procedure TLiteStackTest.NonEmpty;
var
  s: TStack;
begin
  AssertFalse({%H-}s.NonEmpty);
  s.Push(10);
  AssertTrue(s.NonEmpty);
end;

procedure TLiteStackTest.Reverse;
var
  s: TStack;
  I, J: Integer;
begin
  for I := 1 to 50 do
    {%H-}s.Push(I);
  J := 50;
  for I in s.Reverse do
    begin
      AssertTrue(I = J);
      Dec(J);
    end;
end;

procedure TLiteStackTest.Mutables;
var
  s: TStack;
  I, J: Integer;
  p: TStack.PItem;
begin
  for I := 1 to 50 do
    {%H-}s.Push(I);
  for p in s.Mutables do
    p^ := p^ + 10;
  J := 1;
  for I in s do
    begin
      AssertTrue(I = J + 10);
      Inc(J);
    end;
end;

procedure TLiteStackTest.ToArray;
var
  s: TStack;
  I, J: Integer;
  a: TStack.TArray;
begin
  AssertTrue({%H-}s.ToArray = nil);
  for I := 1 to 50 do
    s.Push(I);
  a := s.ToArray;
  AssertTrue(Length(a) = 50);
  J := 1;
  for I in s do
    begin
      AssertTrue(I = J);
      Inc(J);
    end;
end;

procedure TLiteStackTest.PopEmpty;
var
  s: TStack;
  Raised: Boolean = False;
begin
  try
    s.Pop;
  except
    on e: ELGAccessEmpty do
      Raised := True;
  end;
  AssertTrue(Raised);
end;

procedure TLiteStackTest.Pop;
var
  s: TStack;
  I, J: Integer;
begin
  for I := 1 to 50 do
    {%H-}s.Push(I);
  for I := 50 downto 1 do
    begin
      J := s.Pop;
      AssertTrue(J = I);
    end;
  AssertTrue(s.IsEmpty);
end;

procedure TLiteStackTest.TryPopEmpty;
var
  s: TStack;
  I: Integer;
begin
  AssertFalse({%H-}s.TryPop(I));
end;

procedure TLiteStackTest.TryPop;
var
  s: TStack;
  I, J: Integer;
begin
  for I := 1 to 50 do
    {%H-}s.Push(I);
  for I := 50 downto 1 do
    begin
      AssertTrue(s.TryPop(J));
      AssertTrue(J = I);
    end;
  AssertTrue(s.IsEmpty);
end;

procedure TLiteStackTest.PeekEmpty;
var
  s: TStack;
  Raised: Boolean = False;
begin
  try
    {%H-}s.Peek;
  except
    on e: ELGAccessEmpty do
      Raised := True;
  end;
  AssertTrue(Raised);
end;

procedure TLiteStackTest.Peek;
var
  s: TStack;
  I, J: Integer;
begin
  for I := 1 to 50 do
    {%H-}s.Push(I);
  J := s{%H-}.Peek;
  AssertTrue(J = 50);
  J := s{%H-}.Peek;
  AssertTrue(J = 50);
  for I := 50 downto 1 do
    begin
      J := s{%H-}.Peek;
      s.Pop;
      AssertTrue(J = I);
    end;
end;

procedure TLiteStackTest.TryPeekEmpty;
var
  s: TStack;
  J: Integer;
begin
  AssertFalse({%H-}s.TryPeek(J));
end;

procedure TLiteStackTest.TryPeek;
var
  s: TStack;
  I, J: Integer;
begin
  for I := 1 to 50 do
    {%H-}s.Push(I);
  AssertTrue(s.TryPeek(J));
  AssertTrue(J = 50);
  AssertTrue(s.TryPeek(J));
  AssertTrue(J = 50);
  for I := 50 downto 1 do
    begin
      AssertTrue(s.TryPeek(J));
      AssertTrue(J = I);
      s.Pop;
    end;
end;

procedure TLiteStackTest.Clear;
var
  s: TStack;
  I: Integer;
begin
  for I := 1 to 30 do
    {%H-}s.Push(I);
  AssertTrue(s.Count = 30);
  AssertTrue(s.Capacity = DEFAULT_CONTAINER_CAPACITY);
  s.Clear;
  AssertTrue(s.IsEmpty);
  AssertTrue(s.Capacity = 0);
end;

procedure TLiteStackTest.TrimToFit;
var
  s: TStack;
  I: Integer;
begin
  for I := 1 to 30 do
    {%H-}s.Push(I);
  AssertTrue(s.Count = 30);
  AssertTrue(s.Capacity = DEFAULT_CONTAINER_CAPACITY);
  s.EnsureCapacity(250);
  AssertTrue(s.Capacity = 256);
  s.TrimToFit;
  AssertTrue(s.Count = 30);
  AssertTrue(s.Capacity = 30);
end;

procedure TLiteStackTest.ObjectStack;
var
  Counter: Integer = 0;
  procedure IncCounter;
  begin
    Inc(Counter);
  end;
var
  s: TObjStack;
  I: Integer;
begin
  for I := 0 to 99 do
    {%H-}s.Push(TTestObj.Create(@IncCounter));
  for I := 0 to 19 do
    s.Pop{%H-}.Free;
  AssertTrue(Counter = 20);
  s := Default(TObjStack);
  AssertTrue(Counter = 100);
end;

initialization
  RegisterTest(TGStackTest);
  RegisterTest(TLiteStackTest);
end.

