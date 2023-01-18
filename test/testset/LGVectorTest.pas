unit LGVectorTest;

{$mode objfpc}{$H+}
{$MODESWITCH NESTEDPROCVARS}
{$MODESWITCH ADVANCEDRECORDS}

interface

uses

  SysUtils, fpcunit, testregistry,
  LGUtils,
  LGAbstractContainer,
  LGVector,
  LGArrayHelpers,
  LGMiscUtils;

type

  TMyRec = record
    Key: Integer;
    Data: Double;
    constructor Create(aKey: Integer; aData: Double);
    class function GetKey(const aRec: TMyRec): Integer; static; inline;
    class operator < (const L, R: TMyRec): Boolean; inline;
  end;

  TMyDblRec = record
    Key,
    Data: Double;
    constructor Create(aKey, aData: Double);
    class function GetKey(const aRec: TMyDblRec): Double; static; inline;
    class operator < (const L, R: TMyDblRec): Boolean; inline;
  end;

  { TGVectorTest }

  TGVectorTest = class(TTestCase)
  private
  type
    TIntVector     = class(specialize TGVector<Integer>);
    TStrVector     = class(specialize TGVector<string>);
    TAutoIntVector = specialize TGAutoRef<TIntVector>;
    TAutoStrVector = specialize TGAutoRef<TStrVector>;
    TIntArray      = specialize TGArray<Integer>;
    TStrArray      = specialize TGArray<string>;
    TIntEnumerable = specialize TGArrayCursor<Integer>;
    TIntHelper     = specialize TGComparableArrayHelper<Integer>;
    TStrHelper     = specialize TGComparableArrayHelper<string>;
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

    TObjVector     = specialize TGObjectVector<TTestObj>;
    TAutoObjVector = specialize TGAutoRef<TObjVector>;
    TObjArray      = specialize TGArray<TTestObj>;

  const
    IntArray21: array[1..21] of Integer =
      (1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21);
  published
    procedure IsEmpty;
    procedure NonEmpty;
    procedure CreateEmpty;
    procedure CreateCapacity0;
    procedure CreateCapacityNeg;
    procedure CreateCapacity1024;
    procedure CreateEmptyArray;
    procedure CreateEmptyEnum;
    procedure CreateStaticArray21;
    procedure CreateDynArray21;
    procedure CreateEnum21;

    procedure Clear;
    procedure TrimToFit;

    procedure EnsureCapacity0;
    procedure EnsureCapacity11;

    procedure GetItemOfEmpty;
    procedure GetItemOutOfBounds;
    procedure GetItemOutOfBounds_1;
    procedure GetItem;

    procedure SetItemOfEmpty;
    procedure SetItemOutOfBounds;
    procedure SetItemOutOfBounds_1;
    procedure SetItem;

    procedure Add;
    procedure AddAllEmptyArray;
    procedure AddAllStaticArray21;
    procedure AddAllDynArray21;
    procedure AddAllEnum21;
    procedure AddAllSelf;

    procedure Insert;
    procedure InsertOutOfBounds;
    procedure Insert1;
    procedure InsertAllStaticArray21;
    procedure InsertAllStaticArray_1;
    procedure InsertAllStaticArray_2;
    procedure InsertAllEnum;
    procedure InsertAllSelf;

    procedure ExtractFromEmpty;
    procedure ExtractFail;
    procedure Extract;

    procedure TryExtractFromEmpty;
    procedure TryExtractFail;
    procedure TryExtract;

    procedure ExtractAllFromEmpty;
    procedure ExtractAllFail;
    procedure ExtractAllFail_1;
    procedure ExtractAll;
    procedure ExtractAll_1;
    procedure ExtractAll_2;
    procedure ExtractAll_3;
    procedure ExtractAll_4;
    procedure ExtractAllStr;

    procedure TryDeleteFromEmpty;
    procedure TryDeleteFail;
    procedure TryDelete;
    procedure TryDeleteStr;

    procedure DeleteAllFromEmpty;
    procedure DeleteAllFail;
    procedure DeleteAll;
    procedure DeleteAll_1;
    procedure DeleteAll_2;
    procedure DeleteAllStr;

    procedure SplitEmpty;
    procedure SplitInt;
    procedure SplitInt_1;
    procedure SplitInt_2;
    procedure SplitStr;

    procedure Merge;
    procedure MergeStr;

    procedure InIteration;
    procedure IterationDone;
    procedure ObjectVector;

    procedure OrdHelper;
    procedure RadixHelper;
  end;

  { TGLiteVectorTest }

  TGLiteVectorTest = class(TTestCase)
  private
  type
    TIntVector = specialize TGLiteVector<Integer>;
    TStrVector = specialize TGLiteVector<string>;
    TIntArray  = specialize TGArray<Integer>;
    TStrArray  = specialize TGArray<string>;
    TIntHelper = specialize TGComparableArrayHelper<Integer>;
    TProc      = procedure is nested;
    IIntEnum   = specialize IGEnumerable<Integer>;

    TTestObj = class
    private
      FProc: TProc;
    public
      constructor Create(aProc: TProc);
      destructor Destroy; override;
    end;

    TObjVector     = specialize TGLiteObjectVector<TTestObj>;

  const
    IntArray21: array[1..21] of Integer =
      (1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21);

    function  GetEnum(aCount: Integer): IIntEnum;
    function  GetReverseEnum(aCount: Integer): IIntEnum;
  published
    procedure IsEmpty;
    procedure NonEmpty;

    procedure EnsureCapacity;

    procedure GetItemOfEmpty;
    procedure GetItemOutOfBounds;
    procedure GetItemOutOfBounds_1;
    procedure GetItem;

    procedure SetItemOfEmpty;
    procedure SetItemOutOfBounds;
    procedure SetItemOutOfBounds_1;
    procedure SetItem;

    procedure Add;
    procedure AddAllEmptyArray;
    procedure AddAllStaticArray21;
    procedure AddAllDynArray21;
    procedure AddAllSelf;

    procedure Clear;
    procedure MakeEmpty;
    procedure MakeEmptyStr;
    procedure TrimToFit;

    procedure Insert;
    procedure InsertOutOfBounds;
    procedure Insert1;

    procedure ExtractFromEmpty;
    procedure ExtractFail;
    procedure Extract;

    procedure TryExtractFromEmpty;
    procedure TryExtractFail;
    procedure TryExtract;

    procedure ExtractAllFromEmpty;
    procedure ExtractAllOutOfBounds;
    procedure ExtractAllOutOfBounds_1;
    procedure ExtractAll;
    procedure ExtractAll_1;
    procedure ExtractAll_2;
    procedure ExtractAllStr;

    procedure DeleteAllFromEmpty;
    procedure DeleteAllOutOfBounds;
    procedure DeleteAllOutOfBounds_1;
    procedure DeleteAll;
    procedure DeleteAll_1;
    procedure DeleteAll_2;
    procedure DeleteAllStr;
    procedure Swap;
    procedure Swap1;

    procedure GetEnumerable;
    procedure GetReverseEnumerable;
    procedure Mutables;
    procedure Mutable;
    procedure UncMutable;
    procedure CallByValue;

    procedure ObjectVector;

    procedure OrdHelper;
    procedure RadixHelper;
    procedure RadixHelperDouble;
  end;

  { TBoolVectorTest }

  TBoolVectorTest = class(TTestCase)
  private
  type
    TIntVector = specialize TGLiteVector<SizeInt>;

  published
    procedure Capacity;
    procedure GetBitOutOfBounds;
    procedure GetBit;
    procedure SetBitOutOfBounds;
    procedure SetBit;
    procedure PopCount;
    procedure InitRange;
    procedure GetEnumerator;
    procedure Reverse;
    procedure ToArray;
    procedure EnsureCapacity;
    procedure SetBits;
    procedure ClearBits;
    procedure ToggleBits;
    procedure IsEmpty;
    procedure NonEmpty;
    procedure SwapBits;
    procedure CopyBits;
    procedure All;
    procedure Bsf;
    procedure Bsr;
    procedure Lob;
    procedure ToggleBit;
    procedure CallByValue;
    procedure Intersecting;
    procedure IntersectionPop;
    procedure IntersectionPop1;
    procedure Contains;
    procedure JoinGain;
    procedure JoinGain1;
    procedure Join;
    procedure Join1;
    procedure Subtract;
    procedure Subtract1;
    procedure Intersect;
    procedure Intersect1;
    procedure DisjunctJoin;
    procedure TestEquals;
  end;

implementation

{$B-}{$COPERATORS ON}

{ TMyRec }

constructor TMyRec.Create(aKey: Integer; aData: Double);
begin
  Key := aKey;
  Data := aData;
end;

class function TMyRec.GetKey(const aRec: TMyRec): Integer;
begin
  Result := aRec.Key;
end;

class operator TMyRec.<(const L, R: TMyRec): Boolean;
begin
  Result := L.Key < R.Key;
end;

{ TMyDblRec }

constructor TMyDblRec.Create(aKey, aData: Double);
begin
  Key := aKey;
  Data := aData;
end;

class function TMyDblRec.GetKey(const aRec: TMyDblRec): Double;
begin
  Result := aRec.Key;
end;

class operator TMyDblRec.<(const L, R: TMyDblRec): Boolean;
begin
  Result := L.Key < R.Key;
end;

{ TGVectorTest.TTestObj }

constructor TGVectorTest.TTestObj.Create(aProc: TObjProc);
begin
  FProc := aProc;
end;

destructor TGVectorTest.TTestObj.Destroy;
begin
  if FProc <> nil then
    FProc;
  inherited;
end;

{ TGVectorTest.TCounter }

procedure TGVectorTest.TCounter.IncCount;
begin
  Inc(FCount);
end;

function TGVectorTest.TCounter.GetIncrement: TObjProc;
begin
  Result := @IncCount;
end;

procedure TGVectorTest.IsEmpty;
var
  v: TAutoIntVector;
begin
  AssertTrue({%H-}v.Instance.IsEmpty);
end;

procedure TGVectorTest.NonEmpty;
var
  v: TAutoIntVector;
begin
  AssertFalse({%H-}v.Instance.NonEmpty);
end;

procedure TGVectorTest.CreateEmpty;
var
  v: TAutoIntVector;
begin
  AssertTrue({%H-}v.Instance.Count = 0);
  AssertTrue(v.Instance.Capacity = DEFAULT_CONTAINER_CAPACITY);
end;

procedure TGVectorTest.CreateCapacity0;
var
  v: TAutoIntVector;
begin
  {%H-}v.Instance := TIntVector.Create(0);
  AssertTrue({%H-}v.Instance.Count = 0);
  AssertTrue(v.Instance.Capacity = 0);
end;

procedure TGVectorTest.CreateCapacityNeg;
var
  v: TAutoIntVector;
begin
  {%H-}v.Instance := TIntVector.Create(-7);
  AssertTrue(v.Instance.Count = 0);
  AssertTrue(v.Instance.Capacity = 0);
end;

procedure TGVectorTest.CreateCapacity1024;
var
  v: TAutoIntVector;
begin
  {%H-}v.Instance := TIntVector.Create(1024);
  AssertTrue(v.Instance.Count = 0);
  AssertTrue(v.Instance.Capacity = 1024);
end;

procedure TGVectorTest.CreateEmptyArray;
var
  v: TAutoIntVector;
  a: TIntArray;
begin
  {%H-}v.Instance := TIntVector.Create(a{%H-});
  AssertTrue(v.Instance.Count = 0);
  AssertTrue(v.Instance.Capacity = DEFAULT_CONTAINER_CAPACITY);
end;

procedure TGVectorTest.CreateEmptyEnum;
var
  v: TAutoIntVector;
  a: TIntArray;
begin
  {%H-}v.Instance := TIntVector.Create(TIntEnumerable.Create(a{%H-}));
  AssertTrue(v.Instance.Count = 0);
  AssertTrue(v.Instance.Capacity = DEFAULT_CONTAINER_CAPACITY);
end;

procedure TGVectorTest.CreateStaticArray21;
var
  v: TAutoIntVector;
  I: Integer;
begin
  {%H-}v.Instance := TIntVector.Create(IntArray21);
  AssertTrue(v.Instance.Count = 21);
  AssertTrue(v.Instance.Capacity = v.Instance.Count);
  for I := 1 to 21 do
    AssertTrue(v{%H-}.Instance[I - 1] = I);
end;

procedure TGVectorTest.CreateDynArray21;
var
  v: TAutoIntVector;
  I: Integer;
begin
  {%H-}v.Instance := TIntVector.Create(TIntHelper.CreateCopy(IntArray21));
  AssertTrue(v.Instance.Count = 21);
  AssertTrue(v.Instance.Capacity = v.Instance.Count);
  for I := 1 to 21 do
    AssertTrue(v{%H-}.Instance[I - 1] = I);
end;

procedure TGVectorTest.CreateEnum21;
var
  v: TAutoIntVector;
  I: Integer;
begin
  {%H-}v.Instance := TIntVector.Create(TIntEnumerable.Create(TIntHelper.CreateCopy(IntArray21)));
  AssertTrue(v.Instance.Count = 21);
  AssertTrue(v.Instance.Capacity = lgUtils.RoundUpTwoPower(v.Instance.Count));
  for I := 1 to 21 do
    AssertTrue(v{%H-}.Instance[I - 1] = I);
end;

procedure TGVectorTest.Clear;
var
  v: TAutoIntVector;
begin
  {%H-}v.Instance := TIntVector.Create(IntArray21);
  AssertTrue(v.Instance.Count = 21);
  AssertTrue(v.Instance.Capacity = v.Instance.Count);
  v.Instance.Clear;
  AssertTrue(v.Instance.IsEmpty);
  AssertTrue(v.Instance.Capacity = 0);
end;

procedure TGVectorTest.TrimToFit;
var
  v: TAutoIntVector;
begin
  {%H-}v.Instance := TIntVector.Create(IntArray21);
  AssertTrue(v.Instance.Count = 21);
  AssertTrue(v.Instance.Capacity = v.Instance.Count);
  v.Instance.EnsureCapacity(255);
  AssertTrue(v.Instance.Capacity = 256);
  v.Instance.TrimToFit;
  AssertTrue(v.Instance.Count = 21);
  AssertTrue(v.Instance.Capacity = 21);
end;

procedure TGVectorTest.EnsureCapacity0;
var
  v: TAutoIntVector;
begin
  {%H-}v.Instance := TIntVector.Create(0);
  v.Instance.EnsureCapacity(0);
  AssertTrue(v.Instance.Capacity = 0);
end;

procedure TGVectorTest.EnsureCapacity11;
var
  v: TAutoIntVector;
begin
  {%H-}v.Instance := TIntVector.Create(0);
  v.Instance.EnsureCapacity(11);
  AssertTrue(v.Instance.Capacity = LGUtils.RoundUpTwoPower(11));
end;

procedure TGVectorTest.GetItemOfEmpty;
var
  v: TAutoIntVector;
  {%H-}I: Integer;
  Raised: Boolean = False;
begin
  try
    I := v{%H-}.Instance[0];
  except
    on ELGListError do
      Raised := True;
  end;
  AssertTrue(Raised);
end;

procedure TGVectorTest.GetItemOutOfBounds;
var
  v: TAutoIntVector;
  {%H-}I: Integer;
  Raised: Boolean = False;
begin
  v.Instance := TIntVector.Create(IntArray21);
  try
    I := v{%H-}.Instance[-1];
  except
    on ELGListError do
      Raised := True;
  end;
  AssertTrue(Raised);
end;

procedure TGVectorTest.GetItemOutOfBounds_1;
var
  v: TAutoIntVector;
  {%H-}I: Integer;
  Raised: Boolean = False;
begin
  v.Instance := TIntVector.Create(IntArray21);
  try
    I := v{%H-}.Instance[22];
  except
    on ELGListError do
      Raised := True;
  end;
  AssertTrue(Raised);
end;

procedure TGVectorTest.GetItem;
var
  v: TAutoIntVector;
  I: Integer;
begin
  {%H-}v.Instance := TIntVector.Create(IntArray21);
  for I := 0 to 20 do
    AssertTrue(v{%H-}.Instance[I] = I + 1);
end;

procedure TGVectorTest.SetItemOfEmpty;
var
  v: TAutoIntVector;
  Raised: Boolean = False;
begin
  try
    v.Instance[0] := 10;
  except
    on ELGListError do
      Raised := True;
  end;
  AssertTrue(Raised);
end;

procedure TGVectorTest.SetItemOutOfBounds;
var
  v: TAutoIntVector;
  Raised: Boolean = False;
begin
  v.Instance := TIntVector.Create(IntArray21);
  try
    v.Instance[-1] := 7;
  except
    on ELGListError do
      Raised := True;
  end;
  AssertTrue(Raised);
end;

procedure TGVectorTest.SetItemOutOfBounds_1;
var
  v: TAutoIntVector;
  Raised: Boolean = False;
begin
  v.Instance := TIntVector.Create(IntArray21);
  try
    v.Instance[21] := 12;
  except
    on ELGListError do
      Raised := True;
  end;
  AssertTrue(Raised);
end;

procedure TGVectorTest.SetItem;
var
  v: TAutoIntVector;
  I: Integer;
begin
  {%H-}v.Instance := TIntVector.Create(IntArray21);
  for I := 0 to 20 do
    v.Instance[I] := v{%H-}.Instance[I] + 1;
  for I := 0 to 20 do
    AssertTrue(v{%H-}.Instance[I] = I + 2);
end;

procedure TGVectorTest.Add;
var
  v: TAutoIntVector;
begin
  AssertTrue({%H-}v.Instance.Add(11)= 0);
  AssertTrue(v.Instance.Count = 1);
  AssertTrue(v{%H-}.Instance[0] = 11);
  AssertTrue(v.Instance.Add(111) = 1);
  AssertTrue(v.Instance.Count = 2);
  AssertTrue(v{%H-}.Instance[1] = 111);
  AssertTrue(v.Instance.Add(1111) = 2);
  AssertTrue(v.Instance.Count = 3);
  AssertTrue(v{%H-}.Instance[2] = 1111);
end;

procedure TGVectorTest.AddAllEmptyArray;
var
  v: TAutoIntVector;
  a: TIntArray;
begin
  AssertTrue({%H-}v.Instance.AddAll(a{%H-}) = 0);
  AssertTrue(v.Instance.Count = 0);
end;

procedure TGVectorTest.AddAllStaticArray21;
var
  v: TAutoIntVector;
  I: Integer;
begin
  {%H-}v.Instance.Add(0);
  AssertTrue(v.Instance.AddAll(IntArray21) = 21);
  AssertTrue(v.Instance.Count = 22);
  for I := 0 to 21 do
    AssertTrue(v{%H-}.Instance[I] = I);
end;

procedure TGVectorTest.AddAllDynArray21;
var
  v: TAutoIntVector;
  I: Integer;
begin
  {%H-}v.Instance.Add(0);
  AssertTrue(v.Instance.AddAll(TIntHelper.CreateCopy(IntArray21)) = 21);
  AssertTrue(v.Instance.Count = 22);
  for I := 0 to 21 do
    AssertTrue(v{%H-}.Instance[I] = I);
end;

procedure TGVectorTest.AddAllEnum21;
var
  v: TAutoIntVector;
  I: Integer;
begin
  {%H-}v.Instance.Add(0);
  AssertTrue(v.Instance.AddAll(TIntEnumerable.Create(TIntHelper.CreateCopy(IntArray21))) = 21);
  AssertTrue(v.Instance.Count = 22);
  for I := 0 to 21 do
    AssertTrue(v{%H-}.Instance[I] = I);
end;

procedure TGVectorTest.AddAllSelf;
var
  v: TAutoIntVector;
  I: Integer;
begin
  {%H-}v.Instance.AddAll(IntArray21);
  AssertTrue(v.Instance.AddAll(v.Instance) = 21);
  for I := 0 to 20 do
    AssertTrue(v{%H-}.Instance[I] = IntArray21[I + 1]);
  for I := 0 to 20 do
    AssertTrue(v{%H-}.Instance[I+21] = IntArray21[I + 1]);
end;

procedure TGVectorTest.Insert;
var
  v: TAutoIntVector;
  I: Integer;
begin
  {%H-}v.Instance := TIntVector.Create(IntArray21);
  v.Instance.Insert(20, 22);
  v.Instance.Insert(20, 23);
  v.Instance.Insert(20, 24);
  AssertTrue(v.Instance.Count = 24);
  for I := 1 to 20 do
    AssertTrue(v{%H-}.Instance[I - 1] = I);
  AssertTrue(v{%H-}.Instance[20] = 24);
  AssertTrue(v{%H-}.Instance[21] = 23);
  AssertTrue(v{%H-}.Instance[22] = 22);
  AssertTrue(v{%H-}.Instance[23] = 21);
end;

procedure TGVectorTest.InsertOutOfBounds;
var
  v: TAutoIntVector;
  Raised: Boolean;
begin
  v.Instance.Insert(0, 12);
  AssertTrue(v{%H-}.Instance[0] = 12);
  Raised := False;
  try
    v.Instance.Insert(2, 13);
  except
    on ELGListError do
      Raised := True;
  end;

  AssertTrue(Raised);
  Raised := False;
  try
    v.Instance.Insert(-3, 13);
  except
    on ELGListError do
      Raised := True;
  end;
  AssertTrue(Raised);
end;

procedure TGVectorTest.Insert1;
var
  v: TAutoIntVector;
  I: Integer;
begin
  {%H-}v.Instance := TIntVector.Create(TIntArray.Create(4,5,6,7));
  v.Instance.Insert(0, 3);
  v.Instance.Insert(0, 2);
  v.Instance.Insert(0, 1);
  v.Instance.Insert(0, 0);
  AssertTrue(v.Instance.Count = 8);
  for I := 0 to 7 do
    AssertTrue(v{%H-}.Instance[I] = I);
end;

procedure TGVectorTest.InsertAllStaticArray21;
var
  v: TAutoIntVector;
  I: Integer;
begin
  {%H-}v.Instance := TIntVector.Create(TIntArray.Create(0,22));
  AssertTrue(v.Instance.InsertAll(1, IntArray21) = 21);
  AssertTrue(v.Instance.Count = 23);
  for I := 0 to 22 do
    AssertTrue(v{%H-}.Instance[I] = I);
end;

procedure TGVectorTest.InsertAllStaticArray_1;
var
  v: TAutoIntVector;
  I: Integer;
begin
  AssertTrue({%H-}v.Instance.InsertAll(0, IntArray21) = 21);
  AssertTrue(v.Instance.Count = 21);
  for I := 0 to 20 do
    AssertTrue(v{%H-}.Instance[I] = I + 1);
end;

procedure TGVectorTest.InsertAllStaticArray_2;
var
  v: TAutoIntVector;
  I: Integer;
begin
  {%H-}v.Instance := TIntVector.Create(TIntArray.Create(22));
  AssertTrue(v.Instance.InsertAll(0, IntArray21) = 21);
  AssertTrue(v.Instance.Count = 22);
  for I := 0 to 21 do
    AssertTrue(v{%H-}.Instance[I] = I + 1);
end;

procedure TGVectorTest.InsertAllEnum;
var
  v, v1: TAutoIntVector;
  I: Integer;
begin
  {%H-}v.Instance := TIntVector.Create(TIntArray.Create(0, 1, 2, 10, 11));
  {%H-}v1.Instance := TIntVector.Create(TIntArray.Create(3, 4, 5, 6, 7, 8, 9));
  AssertTrue(v.Instance.InsertAll(3, v1.Instance) = 7);
  AssertTrue(v.Instance.Count = 12);
  for I := 0 to 11 do
    AssertTrue(v{%H-}.Instance[I] = I);
end;

procedure TGVectorTest.InsertAllSelf;
var
  v: TAutoIntVector;
begin
  {%H-}v.Instance.AddAll(IntArray21);
  AssertTrue(v.Instance.InsertAll(2, v.Instance) = 21);
  AssertTrue(v.Instance.Count = 42);
  AssertTrue(v{%H-}.Instance[2] = IntArray21[1]);
  AssertTrue(v{%H-}.Instance[22] = IntArray21[21]);
  AssertTrue(v{%H-}.Instance[23] = IntArray21[3]);
  AssertTrue(v{%H-}.Instance[41] = IntArray21[21]);
end;

procedure TGVectorTest.ExtractFromEmpty;
var
  v: TAutoIntVector;
  Raised: Boolean = False;
begin
  try
    {%H-}v.Instance.Extract(0);
  except
    on ELGListError do
      Raised := True;
  end;
  AssertTrue(Raised);
end;

procedure TGVectorTest.ExtractFail;
var
  v: TAutoIntVector;
  Raised: Boolean = False;
begin
  v.Instance := TIntVector.Create([0, 1, 2]);
  try
    {%H-}v.Instance.Extract(3);
  except
    on ELGListError do
      Raised := True;
  end;
  AssertTrue(Raised);
end;

procedure TGVectorTest.Extract;
var
  v: TAutoIntVector;
  I: Integer;
begin
  {%H-}v.Instance := TIntVector.Create([0, 1, 2, 3, 4, 5, 6, 7]);
  I := v{%H-}.Instance.Extract(0);
  AssertTrue(I = 0);
  AssertTrue(v.Instance.Count = 7);
  AssertTrue(v{%H-}.Instance[0] = 1);
  I := v{%H-}.Instance.Extract(3);
  AssertTrue(I = 4);
  AssertTrue(v.Instance.Count = 6);
  AssertTrue(v{%H-}.Instance[3] = 5);
  AssertTrue(TIntHelper.Same(v.Instance.ToArray, [1, 2, 3, 5, 6, 7]));
end;

procedure TGVectorTest.TryExtractFromEmpty;
var
  v: TAutoIntVector;
  I: Integer;
begin
  AssertFalse({%H-}v.Instance.TryExtract(0, I));
end;

procedure TGVectorTest.TryExtractFail;
var
  v: TAutoIntVector;
  I: Integer;
begin
  {%H-}v.Instance := TIntVector.Create(TIntArray.Create(0, 1, 2));
  AssertFalse(v.Instance.TryExtract(3, I));
end;

procedure TGVectorTest.TryExtract;
var
  v: TAutoIntVector;
  I: Integer;
begin
  {%H-}v.Instance := TIntVector.Create([0, 1, 2, 3, 4, 5, 6, 7]);
  AssertTrue(v.Instance.TryExtract(0, I));
  AssertTrue(I = 0);
  AssertTrue(v.Instance.Count = 7);
  AssertTrue(v{%H-}.Instance[0] = 1);
  AssertTrue(v.Instance.TryExtract(3, I));
  AssertTrue(I = 4);
  AssertTrue(v.Instance.Count = 6);
  AssertTrue(v{%H-}.Instance[3] = 5);
  AssertTrue(TIntHelper.Same(v.Instance.ToArray, TIntArray.Create(1, 2, 3, 5, 6, 7)));
end;

procedure TGVectorTest.ExtractAllFromEmpty;
var
  v: TAutoIntVector;
  Raised: Boolean = False;
begin
  try
    v.Instance.ExtractAll(0, 2);
  except
    on ELGListError do
      Raised := True;
  end;
  AssertTrue(Raised);
end;

procedure TGVectorTest.ExtractAllFail;
var
  v: TAutoIntVector;
  Raised: Boolean = False;
begin
  v.Instance := TIntVector.Create(TIntArray.Create(0, 1, 2, 3, 4, 5, 6, 7));
  try
    v.Instance.ExtractAll(8, 2);
  except
    on ELGListError do
      Raised := True;
  end;
  AssertTrue(Raised);
end;

procedure TGVectorTest.ExtractAllFail_1;
var
  v: TAutoIntVector;
  Raised: Boolean = False;
begin
  v.Instance := TIntVector.Create(TIntArray.Create(0, 1, 2, 3, 4, 5, 6, 7));
  try
    v.Instance.ExtractAll(-1, 2);
  except
    on ELGListError do
      Raised := True;
  end;
  AssertTrue(Raised);
end;

procedure TGVectorTest.ExtractAll;
var
  v: TAutoIntVector;
  e: TIntArray;
  I: Integer;
begin
  {%H-}v.Instance := TIntVector.Create(IntArray21);
  e := v.Instance.ExtractAll(11, 10);
  AssertTrue(v.Instance.Count = 11);
  AssertTrue(System.Length(e) = 10);
  for I := 0 to 10 do
    AssertTrue(v{%H-}.Instance[I] = I + 1);
  for I := 0 to 9 do
    AssertTrue(e[I] = I + 12);
end;

procedure TGVectorTest.ExtractAll_1;
var
  v: TAutoIntVector;
  e: TIntArray;
  I: Integer;
begin
  {%H-}v.Instance := TIntVector.Create(IntArray21);
  e := v.Instance.ExtractAll(15, 10);
  AssertTrue(v.Instance.Count = 15);
  AssertTrue(System.Length(e) = 6);
  for I := 0 to 14 do
    AssertTrue(v{%H-}.Instance[I] = I + 1);
  for I := 0 to 5 do
    AssertTrue(e[I] = I + 16);
end;

procedure TGVectorTest.ExtractAll_2;
var
  v: TAutoIntVector;
  e: TIntArray;
  I: Integer;
begin
  {%H-}v.Instance := TIntVector.Create(IntArray21);
  e := v.Instance.ExtractAll(0, 10);
  AssertTrue(v.Instance.Count = 11);
  AssertTrue(System.Length(e) = 10);
  for I := 0 to 10 do
    AssertTrue(v{%H-}.Instance[I] = I + 11);
  for I := 0 to 9 do
    AssertTrue(e[I] = I + 1);
end;

procedure TGVectorTest.ExtractAll_3;
var
  v: TAutoIntVector;
  e: TIntArray;
  I: Integer;
begin
  {%H-}v.Instance := TIntVector.Create(IntArray21);
  e := v.Instance.ExtractAll(1, 19);
  AssertTrue(v.Instance.Count = 2);
  AssertTrue(System.Length(e) = 19);
  AssertTrue(v{%H-}.Instance[0] = 1);
  AssertTrue(v{%H-}.Instance[1] = 21);
  for I := 0 to 18 do
    AssertTrue(e[I] = I + 2);
end;

procedure TGVectorTest.ExtractAll_4;
var
  v: TAutoIntVector;
  e: TIntArray;
  I: Integer;
begin
  {%H-}v.Instance := TIntVector.Create(IntArray21);
  e := v.Instance.ExtractAll(0, 25);
  AssertTrue(v.Instance.Count = 0);
  AssertTrue(System.Length(e) = 21);
  for I := 0 to 20 do
    AssertTrue(e[I] = I + 1);
end;

procedure TGVectorTest.ExtractAllStr;
var
  v: TAutoStrVector;
  e: TStrArray;
  I: Integer;
begin
  for I := 0 to 31 do
    {%H-}v.Instance.Add('string ' + I.ToString);
  e := v.Instance.ExtractAll(0, 33);
  AssertTrue(v.Instance.Count = 0);
  AssertTrue(System.Length(e) = 32);
  for I := 0 to 31 do
    AssertTrue(e[I] = 'string ' + I.ToString);
end;

procedure TGVectorTest.TryDeleteFromEmpty;
var
  v: TAutoIntVector;
begin
  AssertFalse({%H-}v.Instance.TryDelete(0));
end;

procedure TGVectorTest.TryDeleteFail;
var
  v: TAutoIntVector;
begin
  {%H-}v.Instance := TIntVector.Create([1, 2, 3, 4, 5]);
  AssertTrue(v.Instance.Count = 5);
  AssertFalse(v.Instance.TryDelete(-1));
  AssertFalse(v.Instance.TryDelete(6));
end;

procedure TGVectorTest.TryDelete;
var
  v: TAutoIntVector;
begin
  {%H-}v.Instance := TIntVector.Create(TIntArray.Create(1,2,3,4,5));
  AssertTrue(v.Instance.TryDelete(0));
  AssertTrue(v.Instance.Count = 4);
  AssertTrue(v.Instance.TryDelete(3));
  AssertTrue(v.Instance.Count = 3);
  AssertTrue(v.Instance.TryDelete(1));
  AssertTrue(v.Instance.Count = 2);
  AssertTrue(Format('2 expected, got %d', [v{%H-}.Instance[0]]), v{%H-}.Instance[0] = 2);
  AssertTrue(Format('4 expected, got %d', [v{%H-}.Instance[1]]), v{%H-}.Instance[1] = 4);
end;

procedure TGVectorTest.TryDeleteStr;
var
  v: TAutoStrVector;
begin
  {%H-}v.Instance := TStrVector.Create(TStrArray.Create('1','2','3','4','5'));
  AssertTrue(v.Instance.TryDelete(0));
  AssertTrue(v.Instance.Count = 4);
  AssertTrue(v.Instance.TryDelete(3));
  AssertTrue(v.Instance.Count = 3);
  AssertTrue(v.Instance.TryDelete(1));
  AssertTrue(v.Instance.Count = 2);
  AssertTrue(Format('2 expected, got %s', [v{%H-}.Instance[0]]), v{%H-}.Instance[0] = '2');
  AssertTrue(Format('4 expected, got %s', [v{%H-}.Instance[1]]), v{%H-}.Instance[1] = '4');
end;

procedure TGVectorTest.DeleteAllFromEmpty;
var
  v: TAutoIntVector;
  Raised: Boolean = False;
begin
  try
    v.Instance.DeleteAll(0, 100);
  except
    on e: ELGListError do
      Raised := True;
  end;
  AssertTrue(Raised);
end;

procedure TGVectorTest.DeleteAllFail;
var
  v: TAutoIntVector;
  Raised: Boolean;
begin
  v.Instance := TIntVector.Create([1,2,3,4,5]);
  AssertTrue(v.Instance.Count = 5);
  Raised := False;
  try
    v.Instance.DeleteAll(-1, 5);
  except
    on e: ELGListError do
      Raised := True;
  end;
  AssertTrue(Raised);
  Raised := False;
  try
    v.Instance.DeleteAll(6, 5);
  except
    on e: ELGListError do
      Raised := True;
  end;
  AssertTrue(Raised);
end;

procedure TGVectorTest.DeleteAll;
var
  v: TAutoIntVector;
  I: Integer;
begin
  {%H-}v.Instance := TIntVector.Create(IntArray21);
  AssertTrue(v.Instance.DeleteAll(0, 5) = 5);
  AssertTrue(v.Instance.Count = 16);
  for I := 0 to 15 do
    AssertTrue(v{%H-}.Instance[I] = I + 6);
end;

procedure TGVectorTest.DeleteAll_1;
var
  v: TAutoIntVector;
  I: Integer;
begin
  {%H-}v.Instance := TIntVector.Create(IntArray21);
  AssertTrue(v.Instance.DeleteAll(16, 8) = 5);
  AssertTrue(v.Instance.Count = 16);
  for I := 0 to 15 do
    AssertTrue(v{%H-}.Instance[I] = I + 1);
end;

procedure TGVectorTest.DeleteAll_2;
var
  v: TAutoIntVector;
begin
  {%H-}v.Instance := TIntVector.Create(IntArray21);
  AssertTrue(v.Instance.DeleteAll(1, 19) = 19);
  AssertTrue(v.Instance.Count = 2);
  AssertTrue(v{%H-}.Instance[0] = 1);
  AssertTrue(v{%H-}.Instance[1] = 21);
end;

procedure TGVectorTest.DeleteAllStr;
var
  v: TAutoStrVector;
  I: Integer;
begin
  for I := 0 to 15 do
    {%H-}v.Instance.Add('string ' + I.ToString);
  AssertTrue(v.Instance.DeleteAll(1, 14) = 14);
  AssertTrue(v.Instance.Count = 2);
  AssertTrue(v{%H-}.Instance[0] = 'string 0');
  AssertTrue(v{%H-}.Instance[1] = 'string 15');
end;

procedure TGVectorTest.SplitEmpty;
var
  v: TAutoIntVector;
  Raised: Boolean = False;
begin
  try
    v.Instance.Split(0);
  except
    on ELGListError do
      Raised := True;
  end;
  AssertTrue(Raised);
end;

procedure TGVectorTest.SplitInt;
var
  v, e: TAutoIntVector;
  I: Integer;
begin
  {%H-}v.Instance := TIntVector.Create(IntArray21);
  {%H-}e.Instance := TIntVector(v.Instance.Split(12));
  AssertTrue(v.Instance.Count = 12);
  AssertTrue(e.Instance.Count = 9);
  for I := 0 to 11 do
    AssertTrue(v{%H-}.Instance[I] = I + 1);
  for I := 0 to 8 do
    AssertTrue(e{%H-}.Instance[I] = I + 13);
end;

procedure TGVectorTest.SplitInt_1;
var
  v, e: TAutoIntVector;
  I: Integer;
begin
  {%H-}v.Instance := TIntVector.Create(IntArray21);
  {%H-}e.Instance := TIntVector(v.Instance.Split(0));
  AssertTrue(v.Instance.Count = 0);
  AssertTrue(e.Instance.Count = 21);
  for I := 0 to 20 do
    AssertTrue(e{%H-}.Instance[I] = I + 1);
end;

procedure TGVectorTest.SplitInt_2;
var
  v: TAutoIntVector;
  Raised: Boolean = False;
begin
  v.Instance := TIntVector.Create(IntArray21);
  try
    v.Instance.Split(-1);
  except
    on ELGListError do
      Raised := True;
  end;
  AssertTrue(Raised);
end;

procedure TGVectorTest.SplitStr;
var
  v, e: TAutoStrVector;
  I: Integer;
begin
  for I := 0 to 20 do
    {%H-}v.Instance.Add('string ' + I.ToString);
  {%H-}e.Instance := TStrVector(v.Instance.Split(9));
  AssertTrue(v.Instance.Count = 9);
  AssertTrue(e.Instance.Count = 12);
  for I := 0 to 8 do
    AssertTrue(v{%H-}.Instance[I] = 'string ' + I.ToString);
  for I := 0 to 11 do
    AssertTrue(e{%H-}.Instance[I] = 'string ' + (I + 9).ToString);
end;

procedure TGVectorTest.Merge;
var
  vs, vd: TAutoIntVector;
const
  a1: TIntArray = (1,2,3);
  a2: TIntArray = (4,5,6);
  Expect: TIntArray = (1,2,3,4,5,6);
begin
  AssertTrue(vd.Instance.Merge(vs.Instance) = 0);
  AssertTrue(vd.Instance.IsEmpty);
  AssertTrue(vd.Instance.AddAll(a1) = 3);
  AssertTrue(vd.Instance.Merge(vs.Instance) = 0);
  AssertTrue(vd.Instance.Count = 3);
  AssertTrue(vs.Instance.AddAll(a2) = 3);
  AssertTrue(vs.Instance.Count = 3);
  AssertTrue(vd.Instance.Merge(vs.Instance) = 3);
  AssertTrue(vd.Instance.Count = 6);
  AssertTrue(vs.Instance.IsEmpty);
  AssertTrue(TIntHelper.Same(Expect, vd.Instance.ToArray));
end;

procedure TGVectorTest.MergeStr;
var
  vs, vd: TAutoStrVector;
const
  a1: TStrArray = ('11','22','33');
  a2: TStrArray = ('44','55','66');
  Expect: TStrArray = ('11','22','33','44','55','66');
begin
  AssertTrue(vd.Instance.Merge(vs.Instance) = 0);
  AssertTrue(vd.Instance.IsEmpty);
  AssertTrue(vd.Instance.AddAll(a1) = 3);
  AssertTrue(vd.Instance.Merge(vs.Instance) = 0);
  AssertTrue(vd.Instance.Count = 3);
  AssertTrue(vs.Instance.AddAll(a2) = 3);
  AssertTrue(vs.Instance.Count = 3);
  AssertTrue(vd.Instance.Merge(vs.Instance) = 3);
  AssertTrue(vd.Instance.Count = 6);
  AssertTrue(vs.Instance.IsEmpty);
  AssertTrue(TStrHelper.Same(Expect, vd.Instance.ToArray));
end;

procedure TGVectorTest.InIteration;
var
  v: TAutoIntVector;
  I: Integer;
  Raised: Boolean = False;
begin
  v.Instance := TIntVector.Create(IntArray21);
  try
    for I in v.Instance do
      if not Odd(I) then
        v.Instance.Add(I);
  except
    on e: ELGUpdateLock do
      Raised := True;
  end;
  AssertTrue(Raised);
end;

procedure TGVectorTest.IterationDone;
  function IsEven(const aValue: Integer): Boolean;
  begin
    Result := not Odd(aValue);
  end;
  function Double(const aValue: Integer): Integer;
  begin
    Result := aValue + aValue;
  end;
var
  v: TAutoIntVector;
  I, J: Integer;
begin
  v.Instance := TIntVector.Create(IntArray21);
  J := 2;
  for I in v.Instance.Select(@IsEven).Map(@Double) do
    begin
      AssertTrue(I = J * 2);
      J += 2;
    end;
  //shouldn't raise exeption
  v.Instance.Insert(21, 22);
  AssertTrue(v{%H-}.Instance[21] = 22);
end;

procedure TGVectorTest.ObjectVector;
var
  Counter: TCounter;
  v: TAutoObjVector;
  v1: TObjVector;
  e: TObjArray;
  I: Integer;
  o: TTestObj;
begin
  Counter := Default(TCounter);
  for I := 1 to 100 do
    {%H-}v.Instance.Add(TTestObj.Create(Counter.Increment));
  AssertTrue(v.Instance.Count = 100);
  for I := 1 to 10 do
    v.Instance.Delete(0);
  AssertTrue(Counter.Count = 10);
  AssertTrue(v.Instance.Count = 90);
  for I := 1 to 10 do
    v.Instance.Extract(0){%H-}.Free;
  AssertTrue(Counter.Count = 20);
  AssertTrue(v.Instance.Count = 80);
  v.Instance.DeleteAll(0, 10);
  AssertTrue(Counter.Count = 30);
  AssertTrue(v.Instance.Count = 70);
  for I := 1 to 10 do
    v.Instance[I] := TTestObj.Create(Counter.Increment);
  AssertTrue(Counter.Count = 40);
  AssertTrue(v.Instance.Count = 70);
  e := v.Instance.ExtractAll(40, 10);
  AssertTrue(Counter.Count = 40);
  AssertTrue(v.Instance.Count = 60);
  AssertTrue(System.Length(e) = 10);
  for o in e do
    o.Free;
  AssertTrue(Counter.Count = 50);
  v1 := v.Instance.Split(40);
  AssertTrue(v.Instance.Count = 40);
  AssertTrue(v1.Count = 20);
  v.Instance := Default(TObjVector);
  AssertTrue(Counter.Count = 90);
  v1.Free;
  AssertTrue(Counter.Count = 110);
end;

procedure TGVectorTest.OrdHelper;
type
  TOrdHelper = specialize TGOrdVectorHelper<Integer>;
  THelper    = specialize TGComparableVectorHelper<Integer>;
var
  v: TAutoIntVector;
  I: Integer;
const
  TestSize = 1000;
begin
  for I := 1 to TestSize do
    v.Instance.Add(Random(MaxInt) - MaxInt div 2);
  TOrdHelper.Sort(v.Instance);
  AssertTrue(THelper.IsNonDescending(v.Instance));
  THelper.RandomShuffle(v.Instance);
  TOrdHelper.Sort(v.Instance, soDesc);
  AssertTrue(THelper.IsNonAscending(v.Instance));
end;

procedure TGVectorTest.RadixHelper;
type
  TRadixHelper = specialize TGRadixVectorSorter<TMyRec, Integer, TMyRec>;
  THelper      = specialize TGComparableVectorHelper<TMyRec>;
  TVector      = specialize TGVector<TMyRec>;
var
  v: specialize TGAutoRef<TVector>;
  I: Integer;
const
  TestSize = 1000;
begin
  for I := 1 to TestSize do
    v.Instance.Add(TMyRec.Create(Random(MaxInt) - MaxInt div 2, Random));
  TRadixHelper.Sort(v.Instance);
  AssertTrue(THelper.IsNonDescending(v.Instance));
  THelper.RandomShuffle(v.Instance);
  TRadixHelper.Sort(v.Instance, soDesc);
  AssertTrue(THelper.IsNonAscending(v.Instance));
end;

{ TGLiteVectorTest.TTestObj }

constructor TGLiteVectorTest.TTestObj.Create(aProc: TProc);
begin
  FProc := aProc;
end;

destructor TGLiteVectorTest.TTestObj.Destroy;
begin
  if FProc <> nil then
    FProc;
  inherited;
end;

{ TGLiteVectorTest }

function TGLiteVectorTest.GetEnum(aCount: Integer): IIntEnum;
var
  v: TIntVector;
  I: Integer;
begin
  for I := 0 to Pred(aCount) do
    v.Add(I);
  Result := specialize TGWrapEnumerable<Integer, TIntVector.TEnumerator>.Construct(v.GetEnumerator);
end;

function TGLiteVectorTest.GetReverseEnum(aCount: Integer): IIntEnum;
var
  v: TIntVector;
  I: Integer;
begin
  for I := 0 to Pred(aCount) do
    v.Add(I);
  Result :=
    specialize TGWrapEnumerable<Integer, TIntVector.TReverseEnumerator>.Construct(v.GetReverseEnumerator);
end;

procedure TGLiteVectorTest.IsEmpty;
var
  v: TIntVector;
begin
  AssertTrue({%H-}v.IsEmpty);
end;

procedure TGLiteVectorTest.NonEmpty;
var
  v: TIntVector;
begin
  AssertFalse({%H-}v.NonEmpty);
end;

procedure TGLiteVectorTest.EnsureCapacity;
var
  v: TIntVector;
begin
  {%H-}v.EnsureCapacity(11);
  AssertTrue(v.Capacity = LGUtils.RoundUpTwoPower(11));
  v.EnsureCapacity(55);
  AssertTrue(v.Capacity = LGUtils.RoundUpTwoPower(55));
end;

procedure TGLiteVectorTest.GetItemOfEmpty;
var
  v: TIntVector;
  {%H-}I: Integer;
  Raised: Boolean = False;
begin
  try
    I := {%H-}v[0];
  except
    on ELGListError do
      Raised := True;
  end;
  AssertTrue(Raised);
end;

procedure TGLiteVectorTest.GetItemOutOfBounds;
var
  v: TIntVector;
  {%H-}I: Integer;
  Raised: Boolean = False;
begin
  v.AddAll(IntArray21);
  try
    I := v{%H-}[-1];
  except
    on ELGListError do
      Raised := True;
  end;
  AssertTrue(Raised);
end;

procedure TGLiteVectorTest.GetItemOutOfBounds_1;
var
  v: TIntVector;
  {%H-}I: Integer;
  Raised: Boolean = False;
begin
  v.AddAll(IntArray21);
  try
    I := v{%H-}[21];
  except
    on ELGListError do
      Raised := True;
  end;
  AssertTrue(Raised);
end;

procedure TGLiteVectorTest.GetItem;
var
  v: TIntVector;
  I: Integer;
begin
  v.AddAll(IntArray21);
  for I := 0 to 20 do
    AssertTrue(v{%H-}[I] = I + 1);
end;

procedure TGLiteVectorTest.SetItemOfEmpty;
var
  v: TIntVector;
  Raised: Boolean = False;
begin
  try
    v[0] := 10;
  except
    on ELGListError do
      Raised := True;
  end;
  AssertTrue(Raised);
end;

procedure TGLiteVectorTest.SetItemOutOfBounds;
var
  v: TIntVector;
  Raised: Boolean = False;
begin
  v.AddAll(IntArray21);
  try
    {%H-}v[-1] := 7;
  except
    on ELGListError do
      Raised := True;
  end;
  AssertTrue(Raised);
end;

procedure TGLiteVectorTest.SetItemOutOfBounds_1;
var
  v: TIntVector;
  Raised: Boolean = False;
begin
  v.AddAll(IntArray21);
  try
    v[21] := 12;
  except
    on ELGListError do
      Raised := True;
  end;
  AssertTrue(Raised);
end;

procedure TGLiteVectorTest.SetItem;
var
  v: TIntVector;
  I: Integer;
begin
  v.AddAll(IntArray21);
  for I := 0 to 20 do
    v[I] := v{%H-}[I] + 1;
  for I := 0 to 20 do
    AssertTrue(v{%H-}[I] = I + 2);
end;

procedure TGLiteVectorTest.Add;
var
  v: TIntVector;
begin
  AssertTrue({%H-}v.Add(11)= 0);
  AssertTrue(v.Count = 1);
  AssertTrue(v{%H-}[0] = 11);
  AssertTrue(v.Add(111) = 1);
  AssertTrue(v.Count = 2);
  AssertTrue(v{%H-}[1] = 111);
  AssertTrue(v.Add(1111) = 2);
  AssertTrue(v.Count = 3);
  AssertTrue(v{%H-}[2] = 1111);
end;

procedure TGLiteVectorTest.AddAllEmptyArray;
var
  v: TIntVector;
  a: TIntArray;
begin
  AssertTrue({%H-}v.AddAll(a{%H-}) = 0);
  AssertTrue(v.Count = 0);
end;

procedure TGLiteVectorTest.AddAllStaticArray21;
var
  v: TIntVector;
  I: Integer;
begin
  {%H-}v.Add(0);
  AssertTrue(v.AddAll(IntArray21) = 21);
  AssertTrue(v.Count = 22);
  for I := 0 to 21 do
    AssertTrue(v{%H-}[I] = I);
end;

procedure TGLiteVectorTest.AddAllDynArray21;
var
  v: TIntVector;
  I: Integer;
begin
  {%H-}v.Add(0);
  AssertTrue(v.AddAll(TIntHelper.CreateCopy(IntArray21)) = 21);
  AssertTrue(v.Count = 22);
  for I := 0 to 21 do
    AssertTrue(v{%H-}[I] = I);
end;

procedure TGLiteVectorTest.AddAllSelf;
var
  v: TIntVector;
  I: Integer;
begin
  {%H-}v.AddAll(IntArray21);
  AssertTrue(v.AddAll(v) = 21);
  for I := 0 to 20 do
    AssertTrue(v{%H-}[I] = IntArray21[I + 1]);
  for I := 0 to 20 do
    AssertTrue(v{%H-}[I+21] = IntArray21[I + 1]);
end;

procedure TGLiteVectorTest.Clear;
var
  v: TIntVector;
begin
  {%H-}v.AddAll(IntArray21);
  AssertTrue(v.Count = 21);
  AssertTrue(v.Capacity = lgUtils.RoundUpTwoPower(v.Count));
  v.Clear;
  AssertTrue(v.IsEmpty);
  AssertTrue(v.Capacity = 0);
end;

procedure TGLiteVectorTest.MakeEmpty;
var
  v: TIntVector;
  c: SizeInt;
begin
  AssertTrue({%H-}v.IsEmpty);
  AssertTrue(v.Capacity = 0);
  v.MakeEmpty;
  AssertTrue(v.IsEmpty);
  AssertTrue(v.Capacity = 0);
  v.AddAll(IntArray21);
  AssertTrue(v.Count = 21);
  c := v.Capacity;
  AssertTrue(c = lgUtils.RoundUpTwoPower(v.Count));
  v.MakeEmpty;
  AssertTrue(v.IsEmpty);
  AssertTrue(v.Capacity = c);
end;

procedure TGLiteVectorTest.MakeEmptyStr;
var
  v: TStrVector;
  I, cap: Integer;
begin
  AssertTrue({%H-}v.IsEmpty);
  AssertTrue(v.Capacity = 0);
  v.MakeEmpty;
  AssertTrue(v.IsEmpty);
  AssertTrue(v.Capacity = 0);
  for I in [1..50] do
    v.Add('str_' + I.ToString);
  AssertTrue(v.Count = 50);
  cap := v.Capacity;
  AssertTrue(cap <> 0);
  v.MakeEmpty;
  AssertTrue(v.IsEmpty);
  AssertTrue(v.Capacity = cap);
end;

procedure TGLiteVectorTest.TrimToFit;
var
  v: TIntVector;
begin
  {%H-}v.AddAll(IntArray21);
  AssertTrue(v.Count = 21);
  AssertTrue(v.Capacity = lgUtils.RoundUpTwoPower(v.Count));
  v.EnsureCapacity(250);
  AssertTrue(v.Capacity = 256);
  v.TrimToFit;
  AssertTrue(v.Count = 21);
  AssertTrue(v.Capacity = 21);
end;

procedure TGLiteVectorTest.Insert;
var
  v: TIntVector;
  I: Integer;
begin
  {%H-}v.AddAll(IntArray21);
  v.Insert(20, 22);
  v.Insert(20, 23);
  v.Insert(20, 24);
  AssertTrue(v.Count = 24);
  for I := 1 to 20 do
    AssertTrue(v{%H-}[I - 1] = I);
  AssertTrue(v{%H-}[20] = 24);
  AssertTrue(v{%H-}[21] = 23);
  AssertTrue(v{%H-}[22] = 22);
  AssertTrue(v{%H-}[23] = 21);
end;

procedure TGLiteVectorTest.InsertOutOfBounds;
var
  v: TIntVector;
  Raised: Boolean;
begin
  v.Insert(0, 12);
  AssertTrue(v{%H-}[0] = 12);
  Raised := False;
  try
    v.Insert(2, 13);
  except
    on ELGListError do
      Raised := True;
  end;
  AssertTrue(Raised);

  Raised := False;
  try
    v.Insert(-3, 13);
  except
    on ELGListError do
      Raised := True;
  end;
  AssertTrue(Raised);
end;

procedure TGLiteVectorTest.Insert1;
var
  v: TIntVector;
  I: Integer;
begin
  {%H-}v.AddAll([4,5,6,7]);
  v.Insert(0, 3);
  v.Insert(0, 2);
  v.Insert(0, 1);
  v.Insert(0, 0);
  AssertTrue(v.Count = 8);
  for I := 0 to 7 do
    AssertTrue(v{%H-}[I] = I);
end;

procedure TGLiteVectorTest.ExtractFromEmpty;
var
  v: TIntVector;
  Raised: Boolean = False;
begin
  try
    {%H-}v.Extract(0);
  except
    on ELGListError do
      Raised := True;
  end;
  AssertTrue(Raised);
end;

procedure TGLiteVectorTest.ExtractFail;
var
  v: TIntVector;
  Raised: Boolean = False;
begin
  v.AddAll([0, 1, 2]);
  try
    {%H-}v.Extract(3);
  except
    on ELGListError do
      Raised := True;
  end;
  AssertTrue(Raised);
end;

procedure TGLiteVectorTest.Extract;
var
  v: TIntVector;
  I: Integer;
begin
  {%H-}v.AddAll([0, 1, 2, 3, 4, 5, 6, 7]);
  I := v{%H-}.Extract(0);
  AssertTrue(I = 0);
  AssertTrue(v.Count = 7);
  AssertTrue(v{%H-}[0] = 1);
  I := v{%H-}.Extract(3);
  AssertTrue(I = 4);
  AssertTrue(v.Count = 6);
  AssertTrue(v{%H-}[3] = 5);
  AssertTrue(TIntHelper.Same(v.ToArray, [1, 2, 3, 5, 6, 7]))
end;

procedure TGLiteVectorTest.TryExtractFromEmpty;
var
  v: TIntVector;
  I: Integer;
begin
  AssertFalse({%H-}v.TryExtract(0, I));
end;

procedure TGLiteVectorTest.TryExtractFail;
var
  v: TIntVector;
  I: Integer;
begin
  {%H-}v.AddAll([0, 1, 2]);
  AssertFalse(v.TryExtract(3, I));
end;

procedure TGLiteVectorTest.TryExtract;
var
  v: TIntVector;
  I: Integer;
begin
  {%H-}v.AddAll([0, 1, 2, 3, 4, 5, 6, 7]);
  AssertTrue(v.TryExtract(0, I));
  AssertTrue(I = 0);
  AssertTrue(v.Count = 7);
  AssertTrue(v{%H-}[0] = 1);
  AssertTrue(v.TryExtract(3, I));
  AssertTrue(I = 4);
  AssertTrue(v.Count = 6);
  AssertTrue(v{%H-}[3] = 5);
  AssertTrue(TIntHelper.Same(v.ToArray, [1, 2, 3, 5, 6, 7]))
end;

procedure TGLiteVectorTest.ExtractAllFromEmpty;
var
  v: TIntVector;
  Raised: Boolean = False;
begin
  try
    v.ExtractAll(0, 2);
  except
    on ELGListError do
      Raised := True;
  end;
  AssertTrue(Raised);
end;

procedure TGLiteVectorTest.ExtractAllOutOfBounds;
var
  v: TIntVector;
  Raised: Boolean = False;
begin
  v.AddAll([0, 1, 2, 3, 4, 5, 6, 7]);
  try
    v.ExtractAll(8, 2);
  except
    on ELGListError do
      Raised := True;
  end;
  AssertTrue(Raised);
end;

procedure TGLiteVectorTest.ExtractAllOutOfBounds_1;
var
  v: TIntVector;
  Raised: Boolean = False;
begin
  v.AddAll([0, 1, 2, 3, 4, 5, 6, 7]);
  try
    v.ExtractAll(-1, 2);
  except
    on ELGListError do
      Raised := True;
  end;
  AssertTrue(Raised);
end;

procedure TGLiteVectorTest.ExtractAll;
var
  v: TIntVector;
  a: TIntArray;
  I: Integer;
begin
  {%H-}v.AddAll(IntArray21);
  a := v.ExtractAll(11, 10);
  AssertTrue(v.Count = 11);
  AssertTrue(System.Length(a) = 10);
  for I := 0 to 10 do
    AssertTrue(v{%H-}[I] = I + 1);
  for I := 0 to 9 do
    AssertTrue(a[I] = I + 12);
end;

procedure TGLiteVectorTest.ExtractAll_1;
var
  v: TIntVector;
  a: TIntArray;
  I: Integer;
begin
  {%H-}v.AddAll(IntArray21);
  a := v.ExtractAll(15, 10);
  AssertTrue(v.Count = 15);
  AssertTrue(System.Length(a) = 6);
  for I := 0 to 14 do
    AssertTrue(v{%H-}[I] = I + 1);
  for I := 0 to 5 do
    AssertTrue(a[I] = I + 16);
end;

procedure TGLiteVectorTest.ExtractAll_2;
var
  v: TIntVector;
  a: TIntArray;
  I: Integer;
begin
  {%H-}v.AddAll(IntArray21);
  a := v.ExtractAll(0, 10);
  AssertTrue(v.Count = 11);
  AssertTrue(System.Length(a) = 10);
  for I := 0 to 10 do
    AssertTrue(v{%H-}[I] = I + 11);
  for I := 0 to 9 do
    AssertTrue(a[I] = I + 1);
end;

procedure TGLiteVectorTest.ExtractAllStr;
var
  v: TStrVector;
  a: TStrArray;
  I: Integer;
begin
  for I := 0 to 31 do
    {%H-}v.Add('string ' + I.ToString);
  a := v.ExtractAll(0, 33);
  AssertTrue(v.Count = 0);
  AssertTrue(System.Length(a) = 32);
  for I := 0 to 31 do
    AssertTrue(a[I] = 'string ' + I.ToString);
end;

procedure TGLiteVectorTest.DeleteAllFromEmpty;
var
  v: TIntVector;
  Raised: Boolean = False;
begin
  try
    v.DeleteAll(0, 2);
  except
    on ELGListError do
      Raised := True;
  end;
  AssertTrue(Raised);
end;

procedure TGLiteVectorTest.DeleteAllOutOfBounds;
var
  v: TIntVector;
  Raised: Boolean = False;
begin
  v.AddAll([0, 1, 2, 3, 4, 5, 6, 7]);
  try
    v.DeleteAll(8, 2);
  except
    on ELGListError do
      Raised := True;
  end;
  AssertTrue(Raised);
end;

procedure TGLiteVectorTest.DeleteAllOutOfBounds_1;
var
  v: TIntVector;
  Raised: Boolean = False;
begin
  v.AddAll([0, 1, 2, 3, 4, 5, 6, 7]);
  try
    v.DeleteAll(-1, 2);
  except
    on ELGListError do
      Raised := True;
  end;
  AssertTrue(Raised);
end;

procedure TGLiteVectorTest.DeleteAll;
var
  v: TIntVector;
  I: Integer;
begin
  {%H-}v.AddAll(IntArray21);
  I := v.DeleteAll(11, 10);
  AssertTrue(v.Count = 11);
  AssertTrue(I = 10);
  for I := 0 to 10 do
    AssertTrue(v{%H-}[I] = I + 1);
end;

procedure TGLiteVectorTest.DeleteAll_1;
var
  v: TIntVector;
  I: Integer;
begin
  {%H-}v.AddAll(IntArray21);
  I := v.DeleteAll(15, 10);
  AssertTrue(v.Count = 15);
  AssertTrue(I = 6);
  for I := 0 to 14 do
    AssertTrue(v{%H-}[I] = I + 1);
end;

procedure TGLiteVectorTest.DeleteAll_2;
var
  v: TIntVector;
  I: Integer;
begin
  {%H-}v.AddAll(IntArray21);
  I := v.DeleteAll(0, 10);
  AssertTrue(v.Count = 11);
  AssertTrue(I = 10);
  for I := 0 to 10 do
    AssertTrue(v{%H-}[I] = I + 11);
end;

procedure TGLiteVectorTest.DeleteAllStr;
var
  v: TStrVector;
  I: Integer;
begin
  for I := 0 to 31 do
    {%H-}v.Add('string ' + I.ToString);
  I := v.DeleteAll(2, 33);
  AssertTrue(v.Count = 2);
  AssertTrue(I = 30);
end;

procedure TGLiteVectorTest.Swap;
var
  v: TStrVector;
  I: Integer;
begin
  for I := 0 to 31 do
    {%H-}v.Add('string ' + I.ToString);
  v.Swap(3, 11);
  AssertTrue(v[3] = 'string 11');
  AssertTrue(v[11] = 'string 3');
  v.Swap(11, 3);
  AssertTrue(v[3] = 'string 3');
  AssertTrue(v[11] = 'string 11');
  v.UncSwap(3, 11);
  AssertTrue(v[3] = 'string 11');
  AssertTrue(v[11] = 'string 3');
  v.UncSwap(11, 3);
  AssertTrue(v[3] = 'string 3');
  AssertTrue(v[11] = 'string 11');
end;

procedure TGLiteVectorTest.Swap1;
var
  v: TStrVector;
  I: Integer;
  Raised: Boolean = False;
begin
  for I := 0 to 11 do
    {%H-}v.Add('string ' + I.ToString);
  try
    v.Swap(-1, 11);
  except
    Raised := True;
  end;
  AssertTrue(Raised);
  Raised := False;
  try
    v.Swap(3, 12);
  except
    Raised := True;
  end;
  AssertTrue(Raised);
end;

procedure TGLiteVectorTest.GetEnumerable;
var
  I, J: Integer;
begin
  J := 0;
  for I in GetEnum(-5) do
    begin
      AssertTrue(I = J);
      Inc(J);
    end;
  AssertTrue(J = 0);
  for I in GetEnum(100) do
    begin
      AssertTrue(I = J);
      Inc(J);
    end;
  AssertTrue(J = 100);
end;

procedure TGLiteVectorTest.GetReverseEnumerable;
var
  I, J: Integer;
begin
  J := 100;
  for I in GetReverseEnum(-1) do
    begin
      Dec(J);
      AssertTrue(I = J);
    end;
  AssertTrue(J = 100);
  for I in GetReverseEnum(100) do
    begin
      Dec(J);
      AssertTrue(I = J);
    end;
  AssertTrue(J = 0);
end;

procedure TGLiteVectorTest.Mutables;
var
  v: TIntVector;
  I: Integer;
  p: TIntVector.PItem;
begin
  v.AddAll(IntArray21);
  for p in v.Mutables do
    Inc(p^);
  for I := 0 to 20 do
    AssertTrue(v{%H-}[I] = I + 2);
end;

procedure TGLiteVectorTest.Mutable;
var
  v: TIntVector;
  I: Integer;
  p: TIntVector.PItem;
begin
  v.AddAll(IntArray21);
  for I := 0 to 20 do
    begin
      p := v.Mutable[I];
      Inc(p^);
    end;
  for I := 0 to 20 do
    AssertTrue(v{%H-}[I] = I + 2);
end;

procedure TGLiteVectorTest.UncMutable;
var
  v: TStrVector;
  I: Integer;
  p: TStrVector.PItem;
begin
  for I := 0 to 31 do
    {%H-}v.Add('string ' + I.ToString);
  for I := 0 to Pred(v.Count) do
    begin
      p := v.Mutable[I];
      p^ := 'string ' + (I*2).ToString;
    end;
  for I := 0 to Pred(v.Count) do
    AssertTrue(v[I] = 'string ' + (I*2).ToString);
end;

procedure TGLiteVectorTest.CallByValue;
  procedure Test(aVector: TIntVector);
  begin
    AssertTrue(aVector.IsEmpty);
    aVector.Add(11);
    AssertTrue(aVector.NonEmpty);
  end;
  procedure Test2(aVector: TIntVector);
  begin
    aVector.Add(10);
    AssertTrue(aVector[aVector.Count - 1] = 10);
  end;
var
  v: TIntVector;
begin
  {%H-}v.EnsureCapacity(5);
  AssertTrue(v.IsEmpty);
  Test(v);
  AssertTrue(v.IsEmpty);
  v.Add(5);
  AssertTrue(v.Count = 1);
  AssertTrue(v[0] = 5);
  Test2(v);
  AssertTrue(v.Count = 1);
  AssertTrue(v[0] = 5);
end;

procedure TGLiteVectorTest.ObjectVector;
var
  Counter: Integer = 0;
  procedure IncCounter;
  begin
    Inc(Counter);
  end;
var
  v: TObjVector;
  a: array of TTestObj;
  o: TTestObj;
  I: Integer;
begin
  for I := 0 to 99 do
    {%H-}v.Add(TTestObj.Create(@IncCounter));
  for I := 0 to 19 do
    v.Delete(Pred(v.Count)){%H-};
  AssertTrue(Counter = 20);
  I := v.DeleteAll(60, 100);
  AssertTrue(I = 20);
  AssertTrue(Counter = 40);
  a := v.ExtractAll(40, 100);
  AssertTrue(Length(a) = 20);
  AssertTrue(Counter = 40);
  for o in a do
    o.Free;
  AssertTrue(Counter = 60);
  v := Default(TObjVector);
  AssertTrue(Counter = 100);
end;

procedure TGLiteVectorTest.OrdHelper;
type
  TOrdHelper = specialize TGOrdVectorHelper<Integer>;
  THelper    = specialize TGComparableVectorHelper<Integer>;
var
  v: TIntVector;
  I: Integer;
const
  TestSize = 1000;
begin
  for I := 1 to TestSize do
    v.Add(Random(MaxInt) - MaxInt div 2);
  TOrdHelper.Sort(v);
  AssertTrue(THelper.IsNonDescending(v));
  THelper.RandomShuffle(v);
  TOrdHelper.Sort(v, soDesc);
  AssertTrue(THelper.IsNonAscending(v));
end;

procedure TGLiteVectorTest.RadixHelper;
type
  TRadixHelper = specialize TGRadixVectorSorter<TMyRec, Integer, TMyRec>;
  THelper      = specialize TGComparableVectorHelper<TMyRec>;
  TVector      = specialize TGLiteVector<TMyRec>;
var
  v: TVector;
  I: Integer;
const
  TestSize = 1000;
begin
  for I := 1 to TestSize do
    v.Add(TMyRec.Create(Random(MaxInt) - MaxInt div 2, Random));
  TRadixHelper.Sort(v);
  AssertTrue(THelper.IsNonDescending(v));
  THelper.RandomShuffle(v);
  TRadixHelper.Sort(v, soDesc);
  AssertTrue(THelper.IsNonAscending(v));
end;

procedure TGLiteVectorTest.RadixHelperDouble;
type
  TRadixHelper = specialize TGRadixVectorSorter<TMyDblRec, Double, TMyDblRec>;
  THelper      = specialize TGComparableVectorHelper<TMyDblRec>;
  TVector      = specialize TGLiteVector<TMyDblRec>;
var
  v: TVector;
  I: Integer;
const
  TestSize = 1000;
begin
  for I := 1 to High(TestSize) do
    if NextRandomBoolean then
      v.Add(TMyDblRec.Create(Random * Random(1000), 100.0))
    else
      v.Add(TMyDblRec.Create(-Random * Random(1000), 100.0));
  TRadixHelper.Sort(v);
  AssertTrue(THelper.IsNonDescending(v));
  THelper.RandomShuffle(v);
  TRadixHelper.Sort(v, soDesc);
  AssertTrue(THelper.IsNonAscending(v));
end;

{ TBoolVectorTest }

procedure TBoolVectorTest.Capacity;
var
  v: TBoolVector;
begin
  AssertTrue({%H-}v.Capacity = 0);
  v.Capacity := -25;
  AssertTrue(v.Capacity = 0);
  v.Capacity := 15;
  AssertTrue(v.Capacity = BitSizeOf(SizeUInt));
  v.Capacity := 1005;
  AssertTrue(v.Capacity =
    (1005 div BitSizeOf(SizeUInt) + Ord(1005 mod BitSizeOf(SizeUInt) <> 0)) * BitSizeOf(SizeUInt));
  v.Capacity := -5;
  AssertTrue(v.Capacity = 0);
end;

procedure TBoolVectorTest.GetBitOutOfBounds;
var
  v: TBoolVector;
  {%H-}Bit, Raised: Boolean;
begin
  Raised := False;
  try
    Bit := v[0];
  except
    on ELGListError do
      Raised := True;
  end;
  AssertTrue(Raised);

  Raised := False;
  v.Capacity := 64;
  try
    Bit := v[64];
  except
    on ELGListError do
      Raised := True;
  end;
  AssertTrue(Raised);
end;

procedure TBoolVectorTest.GetBit;
var
  v: TBoolVector;
  I: SizeInt;
begin
  v.Capacity := 64;
  for I := 0 to 63 do
    AssertFalse(v[I]);
end;

procedure TBoolVectorTest.SetBitOutOfBounds;
var
  v: TBoolVector;
  Raised: Boolean;
begin
  Raised := False;
  try
    v[0] := True;
  except
    on ELGListError do
      Raised := True;
  end;
  AssertTrue(Raised);

  Raised := False;
  v.Capacity := 64;
  try
    v[64] := True;
  except
    on ELGListError do
      Raised := True;
  end;
  AssertTrue(Raised);
end;

procedure TBoolVectorTest.SetBit;
var
  v: TBoolVector;
  I: SizeInt;
begin
  v.Capacity := 64;
  for I := 0 to 63 do
    begin
      AssertFalse(v[I]);
      v[I] := True;
    end;
  AssertTrue(v.All);
end;

procedure TBoolVectorTest.PopCount;
var
  v: TBoolVector;
  I: SizeInt;
begin
  AssertTrue(v.PopCount = 0);
  v.Capacity := 64;
  AssertTrue(v.PopCount = 0);
  v[10] := True;
  AssertTrue(v.PopCount = 1);
  v[63] := True;
  AssertTrue(v.PopCount = 2);
  for I := 0 to 63 do
    v[I] := True;
  AssertTrue(v.PopCount = 64);
end;

procedure TBoolVectorTest.InitRange;
var
  v: TBoolVector;
  I: SizeInt;
begin
  v.InitRange(-10);
  AssertTrue(v.Capacity = 0);
  v.InitRange(56);
  AssertTrue(v.Capacity = 64);
  for I := 0 to 55 do
    AssertTrue(v[I]);
  for I := 56 to 63 do
    AssertFalse(v[I]);

  v.InitRange(236);
  AssertTrue(v.Capacity = 256);
  for I := 0 to 235 do
    AssertTrue(v.UncBits[I]);
  for I := 236 to 255 do
    AssertFalse(v.UncBits[I]);
end;

procedure TBoolVectorTest.GetEnumerator;
var
  v: TBoolVector;
  Data: TIntVector;
  I, Counter: SizeInt;
begin
  Counter := 0;
  for I in {%H-}v do
    Inc(Counter);
  AssertTrue(Counter = 0);
  v.Capacity := 128;
  for I in {%H-}v do
    Inc(Counter);
  AssertTrue(Counter = 0);
  Data.AddAll([5, 21, 37, 55, 67, 91, 112, 123]);
  for I in Data do
    v[I] := True;
  for I in v do
    begin
      AssertTrue(I = Data[Counter]);
      Inc(Counter);
    end;
end;

procedure TBoolVectorTest.Reverse;
var
  v: TBoolVector;
  Data: TIntVector;
  I, Counter: SizeInt;
begin
  Counter := 0;
  for I in {%H-}v.Reverse do
    Inc(Counter);
  AssertTrue(Counter = 0);
  v.Capacity := 128;
  for I in {%H-}v.Reverse do
    Inc(Counter);
  AssertTrue(Counter = 0);
  Data.AddAll([5, 21, 37, 55, 67, 91, 112, 123]);
  for I in Data do
    v[I] := True;
  for I in v.Reverse do
    begin
      AssertTrue(I = Data[Pred(Data.Count) - Counter]);
      Inc(Counter);
    end;
end;

procedure TBoolVectorTest.ToArray;
var
  v: TBoolVector;
  I: Integer;
  a, r: array of SizeInt;
begin
  a := [3, 17, 29, 44, 59, 63];
  v.Capacity := 64;
  for I in a do
    v[I] := True;
  r := v.ToArray;
  AssertTrue(specialize TGComparableArrayHelper<SizeInt>.Same(a, r));
end;

procedure TBoolVectorTest.EnsureCapacity;
var
  v: TBoolVector;
begin
  AssertTrue({%H-}v.Capacity = 0);
  v.EnsureCapacity(-5);
  AssertTrue(v.Capacity = 0);
  v.EnsureCapacity(155);
  AssertTrue(v.Capacity >= 155);
  v.EnsureCapacity(100);
  AssertTrue(v.Capacity >= 155);
end;

procedure TBoolVectorTest.SetBits;
var
  v: TBoolVector;
  I: SizeInt;
begin
  {%H-}v.SetBits;
  AssertTrue(v.Capacity = 0);
  v.Capacity := 64;
  v.SetBits;
  for I := 0 to 63 do
    AssertTrue(v[I]);
end;

procedure TBoolVectorTest.ClearBits;
var
  v: TBoolVector;
  I: SizeInt;
begin
  v.Capacity := 60;
  v.SetBits;
  for I := 0 to 59 do
    AssertTrue(v[I]);
  v.ClearBits;
  for I := 0 to 59 do
    AssertFalse(v[I]);
end;

procedure TBoolVectorTest.ToggleBits;
var
  v: TBoolVector;
  I: SizeInt;
begin
  v.Capacity := 128;
  for I := 0 to Pred(v.Capacity div 2) do
    v[I * 2] := True;
  v.ToggleBits;
  for I := 0 to Pred(v.Capacity div 2) do
    AssertFalse(v[I * 2]);
  for I := 0 to Pred(v.Capacity div 2) do
    AssertTrue(v[I * 2 + 1]);
end;

procedure TBoolVectorTest.IsEmpty;
var
  v: TBoolVector;
begin
  AssertTrue(v.IsEmpty);
  v.Capacity := 64;
  AssertTrue(v.IsEmpty);
  v[5] := True;
  AssertFalse(v.IsEmpty);
end;

procedure TBoolVectorTest.NonEmpty;
var
  v: TBoolVector;
begin
  AssertFalse(v.NonEmpty);
  v.Capacity := 64;
  AssertFalse(v.NonEmpty);
  v[5] := True;
  AssertTrue(v.NonEmpty);
end;

procedure TBoolVectorTest.SwapBits;
var
  v1, v2: TBoolVector;
begin
  v2.InitRange(120);
  v1.SwapBits(v2);
  AssertTrue(v1.PopCount = 120);
  AssertTrue(v2.IsEmpty);
  v1.ClearBits;
  v1[11] := True;
  v2.Capacity := 32;
  v2[31] := True;
  v2.SwapBits(v1);
  AssertTrue(v1[31]);
  AssertTrue(v2[11]);
end;

procedure TBoolVectorTest.CopyBits;
var
  v1, v2: TBoolVector;
begin
  v1.InitRange(124);
  v1.CopyBits(v2{%H-}, 42);
  AssertTrue(v1.PopCount = 124);
  v2.EnsureCapacity(124);
  v1.CopyBits(v2, 1);
  AssertTrue(v1.PopCount = 123);
  v1.CopyBits(v2, 42);
  AssertTrue(v1.PopCount = 82);
  v1.CopyBits(v2, 63);
  AssertTrue(v1.PopCount = 61);
  v1.CopyBits(v2, 64);
  AssertTrue(v1.PopCount = 60);
  v1.CopyBits(v2, 65);
  AssertTrue(v1.PopCount = 59);
  v1.CopyBits(v2, 123);
  AssertTrue(v1.PopCount = 1);
  v1.CopyBits(v2, 130);
  AssertTrue(v1.PopCount = 0);
  v1.Clear;
  v2.InitRange(42);
  v1.CopyBits(v2, 12);
  AssertTrue(v1.PopCount = 12);
end;

procedure TBoolVectorTest.All;
var
  v: TBoolVector;
begin
  v.InitRange(256);
  AssertTrue(v.All);
  v.Capacity := 2000;
  v.SetBits;
  AssertTrue(v.All);
  v[100] := False;
  AssertFalse(v.All);
end;

procedure TBoolVectorTest.Bsf;
var
  v: TBoolVector;
  I: Integer;
begin
  AssertTrue(v.Bsf = -1);
  v.Capacity := 120;
  AssertTrue(v.Bsf = -1);
  v[5] := True;
  v[55] := True;
  AssertTrue(v.Bsf = 5);
  for I := 0 to Pred(v.Capacity) do
    begin
      v[I] := True;
      AssertTrue(v.Bsf = I);
      v[I] := False;
    end;
end;

procedure TBoolVectorTest.Bsr;
var
  v: TBoolVector;
  I: Integer;
begin
  AssertTrue(v.Bsr = -1);
  v.Capacity := 120;
  AssertTrue(v.Bsr = -1);
  v[25] := True;
  v[75] := True;
  AssertTrue(v.Bsr = 75);
  v.ClearBits;
  for I := 0 to Pred(v.Capacity) do
    begin
      v[I] := True;
      AssertTrue(v.Bsr = I);
      v[I] := False;
    end;
end;

procedure TBoolVectorTest.Lob;
var
  v: TBoolVector;
  I: Integer;
begin
  AssertTrue(v.Lob = -1);
  v.Capacity := 120;
  AssertTrue(v.Lob = 0);
  v.SetBits;
  AssertTrue(v.Lob = -1);
  v.ClearBits;
  for I := 0 to v.Capacity - 2 do
    begin
      v[I] := True;
      AssertTrue(v.Lob = Succ(I));
    end;
  v[120] := False;
  AssertTrue(v.Lob = 120);
end;

procedure TBoolVectorTest.ToggleBit;
var
  v: TBoolVector;
begin
  v.Capacity := 120;
  AssertFalse(v.ToggleBit(110));
  AssertTrue(v[110]);
  AssertTrue(v.ToggleBit(110));
  AssertFalse(v[110]);

  AssertFalse(v.UncToggleBit(111));
  AssertTrue(v[111]);
  AssertTrue(v.UncToggleBit(111));
  AssertFalse(v[111]);
end;

procedure TBoolVectorTest.CallByValue;
  procedure Test(aVector: TBoolVector);
  begin
    aVector.EnsureCapacity(135);
    aVector[134] := True;
    AssertTrue(aVector[134]);
  end;
  procedure Test2(aVector: TBoolVector);
  begin
    aVector.SetBits;
    AssertTrue(aVector.All);
  end;
var
  v: TBoolVector;
begin
  {%H-}v.EnsureCapacity(135);
  AssertTrue(v.IsEmpty);
  Test(v);
  AssertTrue(v.IsEmpty);
  Test2(v);
  AssertTrue(v.IsEmpty);
end;

procedure TBoolVectorTest.Intersecting;
var
  v1, v2: TBoolVector;
begin
  AssertFalse(v1.Intersecting(v2{%H-}));
  AssertFalse(v2.Intersecting(v1));
  AssertFalse(v1.Intersecting(v1));
  AssertFalse(v2.Intersecting(v2));
  v1.Capacity := 120;
  v2.Capacity := 250;
  AssertFalse(v1.Intersecting(v1));
  AssertFalse(v2.Intersecting(v2));
  AssertFalse(v1.Intersecting(v2));
  AssertFalse(v2.Intersecting(v1));
  v1[10] := True;
  v2[110] := True;
  AssertFalse(v1.Intersecting(v2));
  AssertFalse(v2.Intersecting(v1));
  v1[110] := True;
  AssertTrue(v1.Intersecting(v2));
  AssertTrue(v2.Intersecting(v1));
  AssertTrue(v1.Intersecting(v1));
  AssertTrue(v2.Intersecting(v2));
end;

procedure TBoolVectorTest.IntersectionPop;
var
  v1, v2: TBoolVector;
begin
  AssertTrue(v1.IntersectionPop(v2{%H-}) = 0);
  AssertTrue(v2.IntersectionPop(v1) = 0);
  v1.Capacity := 120;
  v2.Capacity := 250;
  AssertTrue(v1.IntersectionPop(v2) = 0);
  AssertTrue(v2.IntersectionPop(v1) = 0);
  v1[10] := True;
  v2[110] := True;
  v2[125] := True;
  AssertTrue(v1.IntersectionPop(v2) = 0);
  AssertTrue(v2.IntersectionPop(v1) = 0);
  v1[110] := True;
  v2[10] := True;
  AssertTrue(v1.IntersectionPop(v2) = 2);
  AssertTrue(v2.IntersectionPop(v1) = 2);

  AssertTrue(v1.IntersectionPop(v1) = 2);
  AssertTrue(v2.IntersectionPop(v2) = 3);
end;

procedure TBoolVectorTest.IntersectionPop1;
var
  v1, v2: TBoolVector;
begin
  v1.Capacity := 160;
  v2.Capacity := 170;
  v1[11] := True;
  v1[129] := True;
  v2[11] := True;
  v2[110] := True;
  v2[169] := True;
  AssertTrue(v1.IntersectionPop(v2) = 1);
  v2[129] := True;
  AssertTrue(v1.IntersectionPop(v2) = 2);
  v1.Capacity := 192;
  v2.Capacity := 202;
  v1[168] := True;
  v2[201] := True;
  AssertTrue(v1.IntersectionPop(v2) = 2);
  v2[168] := True;
  AssertTrue(v1.IntersectionPop(v2) = 3);
  v1.Capacity := 224;
  v2.Capacity := 234;
  v1[218] := True;
  v2[233] := True;
  AssertTrue(v1.IntersectionPop(v2) = 3);
  v2[218] := True;
  AssertTrue(v1.IntersectionPop(v2) = 4);
end;

procedure TBoolVectorTest.Contains;
var
  v1, v2: TBoolVector;
begin
  AssertTrue(v1.Contains(v2{%H-}));
  AssertTrue(v2.Contains(v1));
  v1.Capacity := 120;
  v2.Capacity := 250;
  AssertTrue(v1.Contains(v2));
  AssertTrue(v2.Contains(v1));
  AssertTrue(v1.Contains(v1));
  AssertTrue(v2.Contains(v2));
  v1[10] := True;
  AssertTrue(v1.Contains(v2));
  AssertFalse(v2.Contains(v1));
  v2[10] := True;
  AssertTrue(v1.Contains(v2));
  AssertTrue(v2.Contains(v1));
  v2[110] := True;
  AssertFalse(v1.Contains(v2));
  AssertTrue(v2.Contains(v1));
end;

procedure TBoolVectorTest.JoinGain;
var
  v1, v2: TBoolVector;
begin
  AssertTrue(v1.JoinGain(v2{%H-}) = 0);
  AssertTrue(v2.JoinGain(v1) = 0);
  v1.Capacity := 120;
  v2.Capacity := 250;
  AssertTrue(v1.JoinGain(v2) = 0);
  AssertTrue(v2.JoinGain(v1) = 0);
  AssertTrue(v1.JoinGain(v1) = 0);
  AssertTrue(v2.JoinGain(v2) = 0);
  v1[10] := True;
  v2[110] := True;
  AssertTrue(v1.JoinGain(v2) = 1);
  AssertTrue(v2.JoinGain(v1) = 1);
  v2[240] := True;
  AssertTrue(v1.JoinGain(v2) = 2);
  AssertTrue(v2.JoinGain(v1) = 1);
  AssertTrue(v1.JoinGain(v1) = 0);
  AssertTrue(v2.JoinGain(v2) = 0);
  v1[110] := True;
  v2[10] := True;
  AssertTrue(v1.JoinGain(v2) = 1);
  AssertTrue(v2.JoinGain(v1) = 0);
end;

procedure TBoolVectorTest.JoinGain1;
var
  v1, v2: TBoolVector;
begin
  v1.Capacity := 160;
  v2.Capacity := 170;
  v1[11] := True;
  v1[129] := True;
  v2[11] := True;
  v2[110] := True;
  v2[169] := True;
  AssertTrue(v1.JoinGain(v2) = 2);
  AssertTrue(v2.JoinGain(v1) = 1);
  v2[129] := True;
  AssertTrue(v1.JoinGain(v2) = 2);
  AssertTrue(v2.JoinGain(v1) = 0);
  v1.Capacity := 192;
  v2.Capacity := 202;
  v1[168] := True;
  v2[201] := True;
  AssertTrue(v1.JoinGain(v2) = 3);
  AssertTrue(v2.JoinGain(v1) = 1);
  v2[168] := True;
  AssertTrue(v1.JoinGain(v2) = 3);
  AssertTrue(v2.JoinGain(v1) = 0);
  v1.Capacity := 224;
  v2.Capacity := 234;
  v1[218] := True;
  v2[233] := True;
  AssertTrue(v1.JoinGain(v2) = 4);
  AssertTrue(v2.JoinGain(v1) = 1);
  v2[218] := True;
   AssertTrue(v1.JoinGain(v2) = 4);
  AssertTrue(v2.JoinGain(v1) = 0);
end;

procedure TBoolVectorTest.Join;
var
  v1, v2: TBoolVector;
begin
  v2.Capacity := 256;
  v1.Join(v2);
  AssertTrue(v1.Capacity = 0);
  AssertTrue(v1.IsEmpty);
  v2.Join(v1);
  AssertTrue(v2.Capacity = 256);
  AssertTrue(v2.IsEmpty);
  v2[127] := True;
  v1.Join(v2);
  AssertTrue(v1.Capacity = 128);
  AssertTrue(v1[127]);
  v1[110] := True;
  v2.Join(v1);
  AssertTrue(v2.Capacity = 256);
  AssertTrue(v2[110]);
  v2[255] := True;
  v1.Join(v2);
  AssertTrue(v1.Capacity = 256);
  AssertTrue(v1[110]);
  AssertTrue(v1[255]);
end;

procedure TBoolVectorTest.Join1;
var
  v1, v2: TBoolVector;
begin
  v1.Capacity := 160;
  v2.Capacity := 170;
  v1[11] := True;
  v1[129] := True;
  v2[11] := True;
  v2[110] := True;
  v2[169] := True;
  v1.Join(v2);
  AssertTrue(v1.Capacity = v2.Capacity);
  AssertTrue(v1.PopCount = 4);
  AssertTrue(v1[11]);
  AssertTrue(v1[110]);
  AssertTrue(v1[129]);
  AssertTrue(v1[169]);
  v2.Capacity := 224;
  v2[170] := True;
  v2[221] := True;
  v1.Join(v2);
  AssertTrue(v1.Capacity = v2.Capacity);
  AssertTrue(v1.PopCount = 6);
  AssertTrue(v1[11]);
  AssertTrue(v1[110]);
  AssertTrue(v1[129]);
  AssertTrue(v1[169]);
  AssertTrue(v1[170]);
  AssertTrue(v1[221]);
end;

procedure TBoolVectorTest.Subtract;
var
  v1, v2: TBoolVector;
begin
  v1.Capacity := 128;
  v2.Capacity := 256;
  v1.Subtract(v2);
  AssertTrue(v1.Capacity = 128);
  AssertTrue(v1.IsEmpty);
  v2.Subtract(v1);
  AssertTrue(v2.Capacity = 256);
  AssertTrue(v2.IsEmpty);
  v1[110] := True;
  v2[250] := True;
  v1.Subtract(v2);
  AssertTrue(v1.PopCount = 1);
  AssertTrue(v1[110]);
  v2.Subtract(v1);
  AssertTrue(v2.PopCount = 1);
  AssertTrue(v2[250]);
  v1[115] := True;
  v2[115] := True;
  AssertTrue(v1.PopCount = 2);
  AssertTrue(v2.PopCount = 2);
  v1.Subtract(v2);
  AssertTrue(v1.PopCount = 1);
  AssertFalse(v1[115]);
end;

procedure TBoolVectorTest.Subtract1;
var
  v1, v2: TBoolVector;
begin
  v1.Capacity := 160;
  v2.Capacity := 170;
  v1[11] := True;
  v1[129] := True;
  v2[11] := True;
  v2[110] := True;
  v2[169] := True;
  v1.Subtract(v2);
  AssertTrue(v1.PopCount = 1);
  AssertTrue(v1[129]);
  v2.Subtract(v1);
  AssertTrue(v2.PopCount = 3);
  AssertTrue(v2[11]);
  AssertTrue(v2[110]);
  AssertTrue(v2[169]);
  v2[190] := True;
  v1.Capacity := 224;
  v1[11] := True;
  v1[110] := True;
  v1[129] := True;
  v1[169] := True;
  v1[190] := True;
  v2.Subtract(v1);
  AssertTrue(v2.IsEmpty);
  v1.Subtract(v1);
  AssertTrue(v1.IsEmpty);
end;

procedure TBoolVectorTest.Intersect;
var
  v1, v2: TBoolVector;
begin
  v1.Capacity := 128;
  v2.Capacity := 256;
  v1[110] := True;
  v2[250] := True;
  v1.Intersect(v2);
  AssertTrue(v1.IsEmpty);
  v1[110] := True;
  v2.Intersect(v1);
  AssertTrue(v2.IsEmpty);
  v2.SetBits;
  v2.Intersect(v1);
  AssertTrue(v2.PopCount = 1);
  AssertTrue(v2[110]);
end;

procedure TBoolVectorTest.Intersect1;
var
  v1, v2: TBoolVector;
begin
  v1.Capacity := 160;
  v2.Capacity := 170;
  v1[11] := True;
  v1[129] := True;
  v2[11] := True;
  v2[110] := True;
  v2[152] := True;
  v2[169] := True;
  v1.Intersect(v1);
  AssertTrue(v1.PopCount = 2);
  AssertTrue(v1[11]);
  AssertTrue(v1[129]);
  v1.Intersect(v2);
  AssertTrue(v1.PopCount = 1);
  AssertTrue(v1[11]);
  v1.Capacity := 180;
  v1[169] := True;
  v2.Intersect(v1);
  AssertTrue(v2.PopCount = 2);
  AssertTrue(v2[11]);
  AssertTrue(v2[169]);
end;

procedure TBoolVectorTest.DisjunctJoin;
var
  v1, v2: TBoolVector;
begin
  v1.DisjunctJoin(v2{%H-});
  AssertTrue(v1.Capacity = 0);
  v1.Capacity := 128;
  v2.Capacity := 256;
  v1[0] := True;
  v2[128] := True;
  v1.DisjunctJoin(v2);
  AssertTrue(v1.Capacity = 256);
  AssertTrue(v1.PopCount = 2);
  AssertTrue(v1[0]);
  AssertTrue(v1[128]);
  v1.DisjunctJoin(v1);
  AssertTrue(v1.IsEmpty);
  v1[1] := True;
  v1[127] := True;
  v2[127] := True;
  v1.DisjunctJoin(v2);
  AssertTrue(v1.PopCount = 2);
  AssertTrue(v1[1]);
  AssertTrue(v1[128]);

  v1.Capacity := 512;
  v1[500] := True;
  v1.DisjunctJoin(v2);
  AssertTrue(v1.PopCount = 3);
  AssertTrue(v1[1]);
  AssertTrue(v1[127]);
  AssertTrue(v1[500])
end;

procedure TBoolVectorTest.TestEquals;
var
  v1, v2: TBoolVector;
begin
  AssertTrue(v1.Equals(v1{%H-}));
  AssertTrue(v1.Equals(v2{%H-}));
  v1.Capacity := 128;
  AssertFalse(v1.Equals(v2));
  v2.Capacity := 128;
  AssertTrue(v1.Equals(v2));
  v1[100] := True;
  AssertFalse(v1.Equals(v2));
  v2[100] := True;
  AssertTrue(v1.Equals(v2));
end;

initialization

  RegisterTest(TGVectorTest);
  RegisterTest(TGLiteVectorTest);
  RegisterTest(TBoolVectorTest);

end.

