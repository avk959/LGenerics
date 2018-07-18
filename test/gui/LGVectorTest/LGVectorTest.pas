unit LGVectorTest;

{$mode objfpc}{$H+}
{$MODESWITCH NESTEDPROCVARS}

interface

uses

  SysUtils, fpcunit, testregistry,
  LGUtils,
  LGCustomContainer,
  LGVector,
  LGArrayHelpers;

type

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

    procedure InIteration;
    procedure IterationDone;
    procedure ObjectVector;
  end;

  TGLiteVectorTest = class(TTestCase)
  private
  type
    TIntVector     = specialize TGLiteVector<Integer>;
    TStrVector     = specialize TGLiteVector<string>;
    TIntArray      = specialize TGArray<Integer>;
    TStrArray      = specialize TGArray<string>;
    TIntHelper     = specialize TGComparableArrayHelper<Integer>;
    TProc          = procedure is nested;

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
    procedure ExtractAllFail;
    procedure ExtractAllFail_1;
    procedure ExtractAll;
    procedure ExtractAll_1;
    procedure ExtractAll_2;
    procedure ExtractAllStr;

    procedure Mutables;

    procedure ObjectVector;
  end;

implementation
{$B-}{$COPERATORS ON}

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
  AssertTrue(Format('21 expected, got %d', [v.Instance.Count]), v.Instance.Count = 21);
  AssertTrue(v.Instance.Capacity = DEFAULT_CONTAINER_CAPACITY);
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
  AssertTrue(v.Instance.Capacity = DEFAULT_CONTAINER_CAPACITY);
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
  AssertTrue(v.Instance.Capacity = DEFAULT_CONTAINER_CAPACITY);
  for I := 1 to 21 do
    AssertTrue(v{%H-}.Instance[I - 1] = I);
end;

procedure TGVectorTest.Clear;
var
  v: TAutoIntVector;
begin
  {%H-}v.Instance := TIntVector.Create(IntArray21);
  AssertTrue(v.Instance.Count = 21);
  AssertTrue(v.Instance.Capacity = DEFAULT_CONTAINER_CAPACITY);
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
  AssertTrue(v.Instance.Capacity = DEFAULT_CONTAINER_CAPACITY);
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
  AssertTrue(v.Instance.Capacity = DEFAULT_CONTAINER_CAPACITY);
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
  AssertTrue(TIntHelper.Same(v.Instance.ToArray, [1, 2, 3, 5, 6, 7]))
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
  AssertTrue(TIntHelper.Same(v.Instance.ToArray, TIntArray.Create(1, 2, 3, 5, 6, 7)))
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

procedure TGVectorTest.InIteration;
begin

end;

procedure TGVectorTest.IterationDone;
  function IsEven(constref aValue: Integer): Boolean;
  begin
    Result := not Odd(aValue);
  end;
  function Double(constref aValue: Integer): Integer;
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
  AssertTrue(v.Capacity = DEFAULT_CONTAINER_CAPACITY);
  v.EnsureCapacity(55);
  AssertTrue(v.Capacity = 64);
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
  AssertTrue(v.Capacity = DEFAULT_CONTAINER_CAPACITY);
  v.Clear;
  AssertTrue(v.IsEmpty);
  AssertTrue(v.Capacity = 0);
end;

procedure TGLiteVectorTest.TrimToFit;
var
  v: TIntVector;
begin
  {%H-}v.AddAll(IntArray21);
  AssertTrue(v.Count = 21);
  AssertTrue(v.Capacity = DEFAULT_CONTAINER_CAPACITY);
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

procedure TGLiteVectorTest.ExtractAllFail;
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

procedure TGLiteVectorTest.ExtractAllFail_1;
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

procedure TGLiteVectorTest.Mutables;
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

procedure TGLiteVectorTest.ObjectVector;
var
  Counter: Integer = 0;
  procedure IncCounter;
  begin
    Inc(Counter);
  end;
var
  v: TObjVector;
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
  v.Clear;
  AssertTrue(Counter = 100);
end;

initialization

  RegisterTest(TGVectorTest);
  RegisterTest(TGLiteVectorTest);

end.

