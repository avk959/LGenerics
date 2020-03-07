unit LGHashSetTest;

{$mode objfpc}{$H+}
{$MODESWITCH NESTEDPROCVARS}

interface
uses
  Classes, SysUtils, fpcunit, testregistry,
  LGUtils,
  LGAbstractContainer,
  LGHashSet,
  LGArrayHelpers;

type
  TIntHelper      = specialize TGComparableArrayHelper<Integer>;
  TIntArrayCursor = specialize TGArrayCursor<Integer>;
  TIntArray       = specialize TGArray<Integer>;
  TObjProc        = procedure of object;

  TTestObj = class
  private
    FValue: Integer;
    FProc: TObjProc;
  public
    constructor Create(aProc: TObjProc);
    constructor Create(aValue: Integer; aProc: TObjProc);
    destructor Destroy; override;
    property Value: Integer read FValue;
  end;

  TCounter = object
  private
    FCount: Integer;
    procedure IncCount;
    function  GetIncrement: TObjProc;
  public
    property  Increment: TObjProc read GetIncrement;
    property  Count: Integer read FCount;
  end;

  TObjArray      = specialize TGArray<TTestObj>;
  IIntEnumerable = specialize IGEnumerable<Integer>;

  TTestClass = class(TTestCase)
  private
    function IsEven(constref aValue: Integer): Boolean;
    function ObjIsEven(constref aObj: TTestObj): Boolean;
  end;

  THashSetLPTest = class(TTestClass)
  private
  type
    TSet        = class(specialize TGHashSetLP<Integer>);
    TAutoSet    = specialize TGAutoRef<TSet>;
    TObjSet     = specialize TGObjHashSetLP<TTestObj>;
    TAutoObjSet = specialize TGAutoRef<TObjSet>;

  published
    procedure TestCreate;
    procedure CreateArray;
    procedure CreateEnum;
    procedure CreateCapacity5;
    procedure CreateCapacity50Array11;
    procedure CreateCapacity50Enum11;
    procedure CreateLoadFactor0;
    procedure CreateLoadFactor3;
    procedure CreateLoadFactor;
    procedure CreateLoadFactor075Array11;
    procedure CreateLoadFactor075Enum11;
    procedure CreateCapacityLoadFactor;
    procedure CreateCapacityLoadFactor075Array11;
    procedure CreateCapacityLoadFactor075Enum11;

    procedure Clear;
    procedure EnsureCapacity;
    procedure TrimToFit;

    procedure Add;
    procedure Add100;

    procedure AddArray;
    procedure AddEnum;
    procedure AddSelf;

    procedure Remove;
    procedure RemoveArray;
    procedure RemoveEnum;
    procedure RemoveSelf;

    procedure RemoveIfRegular;
    procedure RemoveIfDelegated;
    procedure RemoveIfNested;

    procedure ExtractIfRegular;
    procedure ExtractIfDelegated;
    procedure ExtractIfNested;

    procedure Retain;
    procedure Retain_1;
    procedure RetainSelf;

    procedure IsSuperset;
    procedure IsSubset;
    procedure IsEqual;
    procedure Intersecting;
    procedure SymmetricSubtract;

    procedure DistinctArray;
    procedure DistinctArray_1;
    procedure DistinctEnum;
    procedure DistinctEnum_1;

    procedure ObjectSetRemove;
    procedure ObjectSetRemoveArray;
    procedure ObjectSetRemoveEnum;
    procedure ObjectSetRemoveSelf;

    procedure ObjectSetRemoveIfRegular;
    procedure ObjectSetRemoveIfDelegated;
    procedure ObjectSetRemoveIfNested;

    procedure ObjectSetExtractIfRegular;
    procedure ObjectSetExtractIfDelegated;
    procedure ObjectSetExtractIfNested;

    procedure ObjectSetRetain;
    procedure ObjectSetSymmetricSubtract;
  end;

  THashSetLPTTest = class(TTestClass)
  private
  type
    TSet        = class(specialize TGHashSetLPT<Integer>);
    TAutoSet    = specialize TGAutoRef<TSet>;
    TObjSet     = specialize TGObjHashSetLPT<TTestObj>;
    TAutoObjSet = specialize TGAutoRef<TObjSet>;

  published
    procedure TestCreate;
    procedure CreateArray;
    procedure CreateEnum;
    procedure CreateCapacity0;
    procedure CreateCapacity5;
    procedure CreateCapacity50Array11;
    procedure CreateCapacity50Enum11;
    procedure CreateLoadFactor0;
    procedure CreateLoadFactor3;
    procedure CreateLoadFactor;
    procedure CreateLoadFactor075Array11;
    procedure CreateLoadFactor075Enum11;
    procedure CreateCapacityLoadFactor;
    procedure CreateCapacityLoadFactor075Array11;
    procedure CreateCapacityLoadFactor075Enum11;
    procedure Clear;
    procedure EnsureCapacity;
    procedure TrimToFit;
    procedure Add;
    procedure Add100;
    procedure AddArray;
    procedure AddEnum;
    procedure AddSelf;
    procedure Remove;
    procedure RemoveArray;
    procedure RemoveEnum;
    procedure RemoveSelf;

    procedure Tombstones;

    procedure RemoveIfRegular;
    procedure RemoveIfDelegated;
    procedure RemoveIfNested;

    procedure ExtractIfRegular;
    procedure ExtractIfDelegated;
    procedure ExtractIfNested;

    procedure Retain;
    procedure Retain_1;
    procedure RetainSelf;

    procedure IsSuperset;
    procedure IsSubset;
    procedure IsEqual;
    procedure Intersecting;
    procedure SymmetricSubtract;

    procedure ObjectSetRemove;
    procedure ObjectSetRemoveArray;
    procedure ObjectSetRemoveEnum;
    procedure ObjectSetRemoveSelf;

    procedure ObjectSetRemoveIfRegular;
    procedure ObjectSetRemoveIfDelegated;
    procedure ObjectSetRemoveIfNested;

    procedure ObjectSetExtractIfRegular;
    procedure ObjectSetExtractIfDelegated;
    procedure ObjectSetExtractIfNested;

    procedure ObjectSetRetain;
    procedure ObjectSetSymmetricSubtract;
  end;

  THashSetQPTest = class(TTestClass)
  private
  type
    TSet        = class(specialize TGHashSetQP<Integer>);
    TAutoSet    = specialize TGAutoRef<TSet>;
    TObjSet     = specialize TGObjHashSetQP<TTestObj>;
    TAutoObjSet = specialize TGAutoRef<TObjSet>;

  published
    procedure TestCreate;
    procedure CreateArray;
    procedure CreateEnum;
    procedure CreateCapacity0;
    procedure CreateCapacity5;
    procedure CreateCapacity50Array11;
    procedure CreateCapacity50Enum11;
    procedure CreateLoadFactor0;
    procedure CreateLoadFactor3;
    procedure CreateLoadFactor;
    procedure CreateLoadFactor075Array11;
    procedure CreateLoadFactor075Enum11;
    procedure CreateCapacityLoadFactor;
    procedure CreateCapacityLoadFactor075Array11;
    procedure CreateCapacityLoadFactor075Enum11;
    procedure Clear;
    procedure EnsureCapacity;
    procedure TrimToFit;
    procedure Add;
    procedure Add100;
    procedure AddArray;
    procedure AddEnum;
    procedure AddSelf;
    procedure Remove;
    procedure RemoveArray;
    procedure RemoveEnum;
    procedure RemoveSelf;

    procedure Tombstones;

    procedure RemoveIfRegular;
    procedure RemoveIfDelegated;
    procedure RemoveIfNested;

    procedure ExtractIfRegular;
    procedure ExtractIfDelegated;
    procedure ExtractIfNested;

    procedure Retain;
    procedure Retain_1;
    procedure RetainSelf;

    procedure IsSuperset;
    procedure IsSubset;
    procedure IsEqual;
    procedure Intersecting;
    procedure SymmetricSubtract;

    procedure ObjectSetRemove;
    procedure ObjectSetRemoveArray;
    procedure ObjectSetRemoveEnum;
    procedure ObjectSetRemoveSelf;

    procedure ObjectSetRemoveIfRegular;
    procedure ObjectSetRemoveIfDelegated;
    procedure ObjectSetRemoveIfNested;

    procedure ObjectSetExtractIfRegular;
    procedure ObjectSetExtractIfDelegated;
    procedure ObjectSetExtractIfNested;

    procedure ObjectSetRetain;
    procedure ObjectSetSymmetricSubtract;
  end;

  TChainHashSetTest = class(TTestClass)
  private
  type
    TSet        = class(specialize TGChainHashSet<Integer>);
    TAutoSet    = specialize TGAutoRef<TSet>;
    TObjSet     = specialize TGObjChainHashSet<TTestObj>;
    TAutoObjSet = specialize TGAutoRef<TObjSet>;

  published
    procedure TestCreate;
    procedure CreateArray;
    procedure CreateEnum;
    procedure CreateCapacity0;
    procedure CreateCapacity5;
    procedure CreateCapacity50Array11;
    procedure CreateCapacity50Enum11;
    procedure CreateLoadFactor0;
    procedure CreateLoadFactor10;
    procedure CreateLoadFactor;
    procedure CreateLoadFactor3Array11;
    procedure CreateLoadFactor3Enum11;
    procedure CreateCapacityLoadFactor;
    procedure CreateCapacityLoadFactor3Array11;
    procedure CreateCapacityLoadFactor3Enum11;
    procedure Clear;
    procedure EnsureCapacity;
    procedure TrimToFit;
    procedure Add;
    procedure Add100;
    procedure AddArray;
    procedure AddEnum;
    procedure AddSelf;
    procedure Remove;
    procedure RemoveArray;
    procedure RemoveEnum;
    procedure RemoveSelf;

    procedure RemoveIfRegular;
    procedure RemoveIfDelegated;
    procedure RemoveIfNested;

    procedure ExtractIfRegular;
    procedure ExtractIfDelegated;
    procedure ExtractIfNested;

    procedure Retain;
    procedure Retain_1;
    procedure RetainSelf;

    procedure IsSuperset;
    procedure IsSubset;
    procedure IsEqual;
    procedure Intersecting;
    procedure SymmetricSubtract;

    procedure ObjectSetRemove;
    procedure ObjectSetRemoveArray;
    procedure ObjectSetRemoveEnum;
    procedure ObjectSetRemoveSelf;

    procedure ObjectSetRemoveIfRegular;
    procedure ObjectSetRemoveIfDelegated;
    procedure ObjectSetRemoveIfNested;

    procedure ObjectSetExtractIfRegular;
    procedure ObjectSetExtractIfDelegated;
    procedure ObjectSetExtractIfNested;

    procedure ObjectSetRetain;
    procedure ObjectSetSymmetricSubtract;
  end;

  TOrdHashSetTest = class(TTestClass)
  private
  type
    TSet        = class(specialize TGOrderedHashSet<Integer>);
    TAutoSet    = specialize TGAutoRef<TSet>;
    TObjSet     = specialize TGObjOrderedHashSet<TTestObj>;
    TAutoObjSet = specialize TGAutoRef<TObjSet>;

  published
    procedure TestCreate;
    procedure CreateArray;
    procedure CreateEnum;
    procedure CreateCapacity0;
    procedure CreateCapacity5;
    procedure CreateCapacity50Array11;
    procedure CreateCapacity50Enum11;
    procedure CreateLoadFactor0;
    procedure CreateLoadFactor10;
    procedure CreateLoadFactor;
    procedure CreateLoadFactor3Array11;
    procedure CreateLoadFactor3Enum11;
    procedure CreateCapacityLoadFactor;
    procedure CreateCapacityLoadFactor3Array11;
    procedure CreateCapacityLoadFactor3Enum11;
    procedure Clear;
    procedure EnsureCapacity;
    procedure TrimToFit;
    procedure Add;
    procedure Add100;
    procedure AddArray;
    procedure AddEnum;
    procedure AddSelf;
    procedure Remove;
    procedure RemoveArray;
    procedure RemoveEnum;
    procedure RemoveSelf;

    procedure RemoveIfRegular;
    procedure RemoveIfDelegated;
    procedure RemoveIfNested;

    procedure ExtractIfRegular;
    procedure ExtractIfDelegated;
    procedure ExtractIfNested;

    procedure Retain;
    procedure Retain_1;
    procedure RetainSelf;

    procedure IsSuperset;
    procedure IsSubset;
    procedure IsEqual;
    procedure Intersecting;
    procedure SymmetricSubtract;

    procedure Ordered;

    procedure ObjectSetRemove;
    procedure ObjectSetRemoveArray;
    procedure ObjectSetRemoveEnum;
    procedure ObjectSetRemoveSelf;

    procedure ObjectSetRemoveIfRegular;
    procedure ObjectSetRemoveIfDelegated;
    procedure ObjectSetRemoveIfNested;

    procedure ObjectSetExtractIfRegular;
    procedure ObjectSetExtractIfDelegated;
    procedure ObjectSetExtractIfNested;

    procedure ObjectSetRetain;
    procedure ObjectSetSymmetricSubtract;
  end;

  { TLiteHashSetLPTest }

  TLiteHashSetLPTest = class(TTestClass)
  private
  type
    TSetSpec = specialize TGLiteHashSetLP<Integer, Integer>;
    TSet     = TSetSpec.TSet;

  published
    procedure Add;
    procedure Add100;
    procedure AddArray;
    procedure AddEnum;
    procedure AddSet;
    procedure AddSelf;
    procedure LoadFactor;
    procedure Clear;
    procedure EnsureCapacity;
    procedure TrimToFit;
    procedure Remove;
    procedure RemoveArray;
    procedure RemoveEnum;
    procedure RemoveSet;
    procedure RemoveSelf;

    procedure RemoveIfRegular;
    procedure RemoveIfDelegated;
    procedure RemoveIfNested;

    procedure ExtractIfRegular;
    procedure ExtractIfDelegated;
    procedure ExtractIfNested;

    procedure Retain;
    procedure Retain_1;
    procedure RetainSelf;

    procedure IsSuperset;
    procedure IsSubset;
    procedure IsEqual;
    procedure Intersecting;
    procedure SymmetricSubtract;

    procedure Include;
    procedure Include1;
    procedure Exclude;
    procedure Exclude1;
    procedure Intersection;
    procedure Intersection1;
    procedure SymmetricDifference;
    procedure SymmetricDifference1;
    procedure Equality;
    procedure Contains;
    procedure TestIn;
    procedure PassByValue;
  end;

  TLiteChainHashSetTest = class(TTestClass)
  private
  type
    TSetSpec = specialize TGLiteChainHashSet<Integer, Integer>;
    TSet     = TSetSpec.TSet;

  published
    procedure Add;
    procedure Add100;
    procedure AddArray;
    procedure AddEnum;
    procedure AddSet;
    procedure AddSelf;
    procedure Clear;
    procedure EnsureCapacity;
    procedure TrimToFit;
    procedure Remove;
    procedure RemoveArray;
    procedure RemoveEnum;
    procedure RemoveSet;
    procedure RemoveSelf;

    procedure RemoveIfRegular;
    procedure RemoveIfDelegated;
    procedure RemoveIfNested;

    procedure ExtractIfRegular;
    procedure ExtractIfDelegated;
    procedure ExtractIfNested;

    procedure Retain;
    procedure Retain_1;
    procedure RetainSelf;

    procedure IsSuperset;
    procedure IsSubset;
    procedure IsEqual;
    procedure Intersecting;
    procedure SymmetricSubtract;

    procedure Include;
    procedure Include1;
    procedure Exclude;
    procedure Exclude1;
    procedure Intersection;
    procedure Intersection1;
    procedure SymmetricDifference;
    procedure SymmetricDifference1;
    procedure Equality;
    procedure Contains;
    procedure TestIn;
    procedure PassByValue;
  end;

  TLiteEquatableHashSetTest = class(TTestClass)
  private
  type
    TSetSpec = specialize TGLiteEquatableHashSet<Integer, Integer>;
    TSet     = TSetSpec.TSet;

  published
    procedure Add;
    procedure Add100;
    procedure AddArray;
    procedure AddEnum;
    procedure AddSet;
    procedure AddSelf;
    procedure Clear;
    procedure EnsureCapacity;
    procedure TrimToFit;
    procedure Remove;
    procedure RemoveArray;
    procedure RemoveEnum;
    procedure RemoveSet;
    procedure RemoveSelf;

    procedure RemoveIfRegular;
    procedure RemoveIfDelegated;
    procedure RemoveIfNested;

    procedure ExtractIfRegular;
    procedure ExtractIfDelegated;
    procedure ExtractIfNested;

    procedure Retain;
    procedure Retain_1;
    procedure RetainSelf;

    procedure IsSuperset;
    procedure IsSubset;
    procedure IsEqual;
    procedure Intersecting;
    procedure SymmetricSubtract;

    procedure Include;
    procedure Include1;
    procedure Exclude;
    procedure Exclude1;
    procedure Intersection;
    procedure Intersection1;
    procedure SymmetricDifference;
    procedure SymmetricDifference1;
    procedure Equality;
    procedure Contains;
    procedure TestIn;
    procedure PassByValue;
  end;

  TGDisjointSetUnionTest = class(TTestCase)
  private
  type
    TDsu = specialize TGDisjointSetUnion<string, string>;

  published
    procedure Add;
    procedure Clear;
    procedure EnsureCapacity;
    procedure GetEnumerator;
    procedure ToArray;
    procedure Join;
    procedure JoinI;
    procedure Tag;
    procedure TagI;
    procedure InSameSet;
    procedure InSameSetI;
    procedure InDiffSets;
    procedure InDiffSetsI;
    procedure Reset;
  end;

  THashSetFGTest = class(TTestCase)
  private
  type
    TSet    = specialize TGThreadHashSetFG<Integer>;
    TSetRef = specialize TGAutoRef<TSet>;
    TArray  = array of Integer;
    THelper = specialize TGOrdinalArrayHelper<Integer>;

    TWorker = class(TThread)
    private
      FSet: TSet;
      FData: TArray;
      FIndex: Integer;
      FFlag,
      FResult: PInteger;
    public
      constructor Create(aSet: TSet; const aData: TArray; aIndex: Integer; aFlagValue, aResult: PInteger);
    end;

    TAdder = class(TWorker)
    protected
      procedure Execute; override;
    end;

    TRemover = class(TWorker)
    protected
      procedure Execute; override;
    end;

  published
    procedure SimpleAdd;
    procedure SimpleRemove;
    procedure Add;
    procedure Remove;
  end;

implementation
{$B-}{$COPERATORS ON}{$WARNINGS OFF}

const
  IntArray10: array[1..10] of Integer = (2, 4, 6, 8, 10, 12, 14, 16, 18, 20);
  IntArray11: array[1..11] of Integer = (1, 3, 5, 7, 9, 11, 13, 15, 17, 19, 21);
  IntArray21: array[1..21] of Integer = (1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21);

function IsEvenInt(constref aValue: Integer): Boolean;
begin
  Result := not Odd(aValue);
end;

function IsEvenObj(constref aObj: TTestObj): Boolean;
begin
  Result := not Odd(aObj.Value);
end;

{ TTestObj }

constructor TTestObj.Create(aProc: TObjProc);
begin
  FProc := aProc;
end;

constructor TTestObj.Create(aValue: Integer; aProc: TObjProc);
begin
  FValue := aValue;
  FProc := aProc;
end;

destructor TTestObj.Destroy;
begin
  if FProc <> nil then
    FProc;
  inherited;
end;

{ TCounter }

procedure TCounter.IncCount;
begin
  Inc(FCount);
end;

function TCounter.GetIncrement: TObjProc;
begin
  Result := @IncCount;
end;

{ TTestClass }

function TTestClass.IsEven(constref aValue: Integer): Boolean;
begin
  Result := not Odd(aValue);
end;

function TTestClass.ObjIsEven(constref aObj: TTestObj): Boolean;
begin
  Result := not Odd(aObj.Value);
end;

procedure THashSetLPTest.TestCreate;
var
  s: TAutoSet;
begin
  AssertTrue(s.Instance.Count = 0);
  AssertTrue(s.Instance.LoadFactor = s.Instance.DefaultLoadFactor);
end;

procedure THashSetLPTest.CreateArray;
var
  s: TAutoSet;
  I: Integer;
begin
  s.Instance := TSet.Create(IntArray11);
  AssertTrue(s.Instance.Count = 11);
  AssertTrue(s.Instance.LoadFactor = s.Instance.DefaultLoadFactor);
  for I in IntArray11 do
    AssertTrue(s.Instance.Contains(I));
end;

procedure THashSetLPTest.CreateEnum;
var
  s: TAutoSet;
  I: Integer;
  e: IIntEnumerable;
begin
  e := TIntArrayCursor.Create(TIntHelper.CreateCopy(IntArray11));
  s.Instance := TSet.Create(e);
  AssertTrue(s.Instance.Count = 11);
  AssertTrue(s.Instance.LoadFactor = s.Instance.DefaultLoadFactor);
  for I in IntArray11 do
    AssertTrue(s.Instance.Contains(I));
end;

procedure THashSetLPTest.CreateCapacity5;
var
  s: TAutoSet;
begin
  s.Instance := TSet.Create(5);
  AssertTrue(s.Instance.Count = 0);
  AssertTrue(s.Instance.Capacity >= 5);
  AssertTrue(s.Instance.LoadFactor = s.Instance.DefaultLoadFactor);
end;

procedure THashSetLPTest.CreateCapacity50Array11;
var
  s: TAutoSet;
  I: Integer;
begin
  s.Instance := TSet.Create(50, IntArray11);
  AssertTrue(s.Instance.Count = 11);
  AssertTrue(s.Instance.Capacity >= 50);
  AssertTrue(s.Instance.LoadFactor = s.Instance.DefaultLoadFactor);
  for I in IntArray11 do
    AssertTrue(s.Instance.Contains(I));
end;

procedure THashSetLPTest.CreateCapacity50Enum11;
var
  s: TAutoSet;
  I: Integer;
  e: IIntEnumerable;
begin
  e := TIntArrayCursor.Create(TIntHelper.CreateCopy(IntArray11));
  s.Instance := TSet.Create(50, e);
  AssertTrue(s.Instance.Count = 11);
  AssertTrue(s.Instance.Capacity >= 50);
  AssertTrue(s.Instance.LoadFactor = s.Instance.DefaultLoadFactor);
  for I in IntArray11 do
    AssertTrue(s.Instance.Contains(I));
end;

procedure THashSetLPTest.CreateLoadFactor0;
var
  s: TAutoSet;
begin
  s.Instance := TSet.Create(0.0);
  AssertTrue(s.Instance.Count = 0);
  AssertTrue(s.Instance.LoadFactor = TSet.MinLoadFactor);
end;

procedure THashSetLPTest.CreateLoadFactor3;
var
  s: TAutoSet;
begin
  s.Instance := TSet.Create(3.0);
  AssertTrue(s.Instance.Count = 0);
  AssertTrue(s.Instance.LoadFactor = TSet.MaxLoadFactor);
end;

procedure THashSetLPTest.CreateLoadFactor;
var
  s: TAutoSet;
  lf: Double;
begin
  lf := 0.5;
  s.Instance := TSet.Create(lf);
  AssertTrue(s.Instance.Count = 0);
  AssertTrue(s.Instance.LoadFactor = lf);
end;

procedure THashSetLPTest.CreateLoadFactor075Array11;
var
  s: TAutoSet;
  I: Integer;
  lf: Single;
begin
  lf := 0.75;
  s.Instance := TSet.Create(lf, IntArray11);
  AssertTrue(s.Instance.Count = 11);
  AssertTrue(s.Instance.LoadFactor = lf);
  for I in IntArray11 do
    AssertTrue(s.Instance.Contains(I));
end;

procedure THashSetLPTest.CreateLoadFactor075Enum11;
var
  s: TAutoSet;
  I: Integer;
  e: IIntEnumerable;
  lf: Single;
begin
  lf := 0.75;
  e := TIntArrayCursor.Create(TIntHelper.CreateCopy(IntArray11));
  s.Instance := TSet.Create(lf, e);
  AssertTrue(s.Instance.Count = 11);
  AssertTrue(s.Instance.LoadFactor = lf);
  for I in IntArray11 do
    AssertTrue(s.Instance.Contains(I));
end;

procedure THashSetLPTest.CreateCapacityLoadFactor;
var
  s: TAutoSet;
  lf: Single;
begin
  lf := 0.45;
  s.Instance := TSet.Create(50);
  AssertTrue(s.Instance.Count = 0);
  s.Instance.LoadFactor := lf;
  AssertTrue(s.Instance.Capacity >= 50);
  AssertTrue(s.Instance.LoadFactor = lf);
end;

procedure THashSetLPTest.CreateCapacityLoadFactor075Array11;
var
  s: TAutoSet;
  I: Integer;
  lf: Single;
begin
  lf := 0.75;
  s.Instance := TSet.Create(11, lf, IntArray11);
  AssertTrue(s.Instance.Count = 11);
  AssertTrue(s.Instance.LoadFactor = lf);
  for I in IntArray11 do
    AssertTrue(s.Instance.Contains(I));
end;

procedure THashSetLPTest.CreateCapacityLoadFactor075Enum11;
var
  s: TAutoSet;
  I: Integer;
  e: IIntEnumerable;
  lf: Single;
begin
  lf := 0.75;
  e := TIntArrayCursor.Create(TIntHelper.CreateCopy(IntArray11));
  s.Instance := TSet.Create(11, lf, e);
  AssertTrue(s.Instance.Count = 11);
  AssertTrue(s.Instance.LoadFactor = lf);
  for I in IntArray11 do
    AssertTrue(s.Instance.Contains(I));
end;

procedure THashSetLPTest.Clear;
var
  s: TAutoSet;
begin
  s.Instance := TSet.Create(IntArray11);
  AssertTrue(s.Instance.Count = 11);
  AssertTrue(s.Instance.Capacity >= 11);
  s.Instance.Clear;
  AssertTrue(s.Instance.Count = 0);
  AssertTrue(s.Instance.Capacity = 0);
end;

procedure THashSetLPTest.EnsureCapacity;
var
  s: TAutoSet;
  c: SizeInt;
begin
  c := s.Instance.ExpandTreshold;
  s.Instance.EnsureCapacity(c + 1);
  AssertTrue(s.Instance.Capacity > c);
end;

procedure THashSetLPTest.TrimToFit;
var
  s: TAutoSet;
  c: SizeInt;
begin
  s.Instance := TSet.Create(50, IntArray11);
  c := s.Instance.Capacity;
  s.Instance.TrimToFit;
  AssertTrue(s.Instance.Capacity < c);
end;

procedure THashSetLPTest.Add;
var
  s: TAutoSet;
begin
  AssertTrue(s.Instance.Count = 0);
  AssertTrue(s.Instance.Add(1));
  AssertTrue(s.Instance.Count = 1);
  AssertTrue(s.Instance.Contains(1));
  AssertTrue(s.Instance.Add(51));
  AssertTrue(s.Instance.Count = 2);
  AssertTrue(s.Instance.Contains(51));
  AssertFalse(s.Instance.Add(1));
  AssertFalse(s.Instance.Add(51));
  AssertTrue(s.Instance.Count = 2);
  AssertTrue(s.Instance.Add(52));
  AssertTrue(s.Instance.Count = 3);
  AssertTrue(s.Instance.Contains(52));
end;

procedure THashSetLPTest.Add100;
var
  s: TAutoSet;
  I: Integer;
begin
  for I := 1 to 100 do
    AssertTrue(s.Instance.Add(I));
  AssertTrue(s.Instance.Count = 100);
  for I := 1 to 100 do
    AssertFalse(s.Instance.Add(I));
  AssertTrue(s.Instance.Count = 100);
  for I := 1 to 100 do
    AssertTrue(s.Instance.Contains(I));
end;

procedure THashSetLPTest.AddArray;
var
  s: TAutoSet;
begin
  AssertTrue(s.Instance.AddAll(IntArray21) = System.Length(IntArray21));
  AssertTrue(s.Instance.Count = System.Length(IntArray21));
  AssertTrue(s.Instance.ContainsAll(IntArray21));
end;

procedure THashSetLPTest.AddEnum;
var
  s: TAutoSet;
  e: IIntEnumerable;
begin
  e := TIntArrayCursor.Create(TIntHelper.CreateCopy(IntArray21));
  AssertTrue(s.Instance.AddAll(e) = System.Length(IntArray21));
  AssertTrue(s.Instance.Count = System.Length(IntArray21));
  AssertTrue(s.Instance.ContainsAll(IntArray21));
end;

procedure THashSetLPTest.AddSelf;
var
  s: TAutoSet;
begin
  s.Instance.AddAll(IntArray21);
  AssertTrue(s.Instance.AddAll(s.Instance) = 0);
  AssertTrue(s.Instance.Count = System.Length(IntArray21));
end;

procedure THashSetLPTest.Remove;
var
  s: TAutoSet;
begin
  s.Instance := TSet.Create(IntArray21);
  AssertTrue(s.Instance.Count = System.Length(IntArray21));
  AssertTrue(s.Instance.Remove(IntArray21[1]));
  AssertTrue(s.Instance.Count = System.Length(IntArray21) - 1);
  AssertTrue(s.Instance.Remove(IntArray21[System.High(IntArray21)]));
  AssertTrue(s.Instance.Count = System.Length(IntArray21) - 2);
  AssertFalse(s.Instance.Remove(-1));
  AssertTrue(s.Instance.Count = System.Length(IntArray21) - 2);
  AssertFalse(s.Instance.Remove(100));
  AssertTrue(s.Instance.Count = System.Length(IntArray21) - 2);
end;

procedure THashSetLPTest.RemoveArray;
var
  s: TAutoSet;
begin
  s.Instance := TSet.Create(IntArray21);
  AssertTrue(s.Instance.RemoveAll(IntArray11) = 11);
  AssertTrue(s.Instance.Count = 10);
end;

procedure THashSetLPTest.RemoveEnum;
var
  s: TAutoSet;
  e: IIntEnumerable;
begin
  e := TIntArrayCursor.Create(TIntHelper.CreateCopy(IntArray11));
  s.Instance := TSet.Create(IntArray21);
  AssertTrue(s.Instance.RemoveAll(e) = 11);
  AssertTrue(s.Instance.Count = 10);
end;

procedure THashSetLPTest.RemoveSelf;
var
  s: TAutoSet;
begin
  s.Instance := TSet.Create(IntArray21);
  AssertTrue(s.Instance.RemoveAll(s.Instance) = 21);
  AssertTrue(s.Instance.IsEmpty);
end;

procedure THashSetLPTest.RemoveIfRegular;
var
  s: TAutoSet;
begin
  s.Instance := TSet.Create(IntArray21);
  AssertTrue(s.Instance.RemoveIf(@IsEvenInt) = 10);
  AssertTrue(s.Instance.Count = 11);
  AssertTrue(s.Instance.ContainsAll(IntArray11));
end;

procedure THashSetLPTest.RemoveIfDelegated;
var
  s: TAutoSet;
begin
  s.Instance := TSet.Create(IntArray21);
  AssertTrue(s.Instance.RemoveIf(@IsEven) = 10);
  AssertTrue(s.Instance.Count = 11);
  AssertTrue(s.Instance.ContainsAll(IntArray11));
end;

procedure THashSetLPTest.RemoveIfNested;
  function IsEvenInt(constref aValue: Integer): Boolean;
  begin
    Result := not Odd(aValue);
  end;
var
  s: TAutoSet;
begin
  s.Instance := TSet.Create(IntArray21);
  AssertTrue(s.Instance.RemoveIf(@IsEvenInt) = 10);
  AssertTrue(s.Instance.Count = 11);
  AssertTrue(s.Instance.ContainsAll(IntArray11));
end;

procedure THashSetLPTest.ExtractIfRegular;
var
  s: TAutoSet;
  e: TIntArray;
begin
  s.Instance := TSet.Create(IntArray21);
  e := s.Instance.ExtractIf(@IsEvenInt);
  AssertTrue(s.Instance.Count = 11);
  AssertTrue(s.Instance.ContainsAll(IntArray11));
  AssertTrue(System.Length(e) = 10);
  TIntHelper.QuickSort(e);
  AssertTrue(TIntHelper.Same(IntArray10, e));
end;

procedure THashSetLPTest.ExtractIfDelegated;
var
  s: TAutoSet;
  e: TIntArray;
begin
  s.Instance := TSet.Create(IntArray21);
  e := s.Instance.ExtractIf(@IsEven);
  AssertTrue(s.Instance.Count = 11);
  AssertTrue(s.Instance.ContainsAll(IntArray11));
  AssertTrue(System.Length(e) = 10);
  TIntHelper.QuickSort(e);
  AssertTrue(TIntHelper.Same(IntArray10, e));
end;

procedure THashSetLPTest.ExtractIfNested;
  function IsEvenInt(constref aValue: Integer): Boolean;
  begin
    Result := not Odd(aValue);
  end;
var
  s: TAutoSet;
  e: TIntArray;
begin
  s.Instance := TSet.Create(IntArray21);
  e := s.Instance.ExtractIf(@IsEvenInt);
  AssertTrue(s.Instance.Count = 11);
  AssertTrue(s.Instance.ContainsAll(IntArray11));
  AssertTrue(System.Length(e) = 10);
  TIntHelper.QuickSort(e);
  AssertTrue(TIntHelper.Same(IntArray10, e));
end;

procedure THashSetLPTest.Retain;
var
  s, s1: TAutoSet;
begin
  s.Instance := TSet.Create(IntArray21);
  s1.Instance := TSet.Create(IntArray11);
  s.Instance.AddAll([-3, -2, -1, 0]);
  AssertTrue(s.Instance.Count = 25);
  s.Instance.RetainAll(s1.Instance);
  AssertTrue(s.Instance.Count = 11);
  AssertTrue(s.Instance.ContainsAll(s1.Instance));
end;

procedure THashSetLPTest.Retain_1;
var
  s, s1: TAutoSet;
begin
  s.Instance := TSet.Create(IntArray10);
  s1.Instance := TSet.Create(IntArray11);
  s.Instance.RetainAll(s1.Instance);
  AssertTrue(s.Instance.IsEmpty);
end;

procedure THashSetLPTest.RetainSelf;
var
  s: TAutoSet;
begin
  s.Instance := TSet.Create(IntArray21);
  AssertTrue(s.Instance.Count = 21);
  s.Instance.RetainAll(s.Instance);
  AssertTrue(s.Instance.Count = 21);
  AssertTrue(s.Instance.ContainsAll(IntArray21));
end;

procedure THashSetLPTest.IsSuperset;
var
  s, s1: TAutoSet;
begin
  s.Instance := TSet.Create(IntArray21);
  s1.Instance := TSet.Create(IntArray21);
  AssertTrue(s.Instance.IsSuperset(s1.Instance));
  AssertTrue(s1.Instance.IsSuperset(s.Instance));
  AssertTrue(s.Instance.IsSuperset(s.Instance));
  s1.Instance := TSet.Create(IntArray11);
  AssertTrue(s.Instance.IsSuperset(s1.Instance));
  AssertFalse(s1.Instance.IsSuperset(s.Instance));
end;

procedure THashSetLPTest.IsSubset;
var
  s, s1: TAutoSet;
begin
  s.Instance := TSet.Create(IntArray21);
  s1.Instance := TSet.Create(IntArray21);
  AssertTrue(s.Instance.IsSubset(s1.Instance));
  AssertTrue(s1.Instance.IsSubset(s.Instance));
  AssertTrue(s.Instance.IsSubset(s.Instance));
  s1.Instance := TSet.Create(IntArray11);
  AssertTrue(s1.Instance.IsSubset(s.Instance));
  AssertFalse(s.Instance.IsSubset(s1.Instance));
end;

procedure THashSetLPTest.IsEqual;
var
  s, s1: TAutoSet;
begin
  s.Instance := TSet.Create(IntArray21);
  s1.Instance := TSet.Create(IntArray21);
  AssertTrue(s.Instance.IsEqual(s.Instance));
  AssertTrue(s.Instance.IsEqual(s1.Instance));
  s1.Instance := TSet.Create(IntArray11);
  AssertFalse(s.Instance.IsEqual(s1.Instance));
end;

procedure THashSetLPTest.Intersecting;
var
  s, s1: TAutoSet;
begin
  s.Instance := TSet.Create(IntArray21);
  s1.Instance := TSet.Create(IntArray11);
  AssertTrue(s.Instance.Intersecting(s.Instance));
  AssertTrue(s.Instance.Intersecting(s1.Instance));
  s.Instance := TSet.Create(IntArray10);
  AssertFalse(s.Instance.Intersecting(s1.Instance));
  AssertFalse(s1.Instance.Intersecting(s.Instance));
end;

procedure THashSetLPTest.SymmetricSubtract;
var
  s, s1: TAutoSet;
begin
  s.Instance := TSet.Create(IntArray21);
  s1.Instance := TSet.Create(IntArray21);
  s.Instance.SymmetricSubtract(s1.Instance);
  AssertTrue(s.Instance.IsEmpty);
  s.Instance := TSet.Create(IntArray21);
  s1.Instance.Clear;
  s.Instance.SymmetricSubtract(s1.Instance);
  AssertTrue(s.Instance.Count = 21);
  AssertTrue(s.Instance.ContainsAll(IntArray21));
  s.Instance := TSet.Create(IntArray11);
  s1.Instance := TSet.Create(IntArray10);
  s.Instance.SymmetricSubtract(s1.Instance);
  AssertTrue(s.Instance.Count = 21);
  AssertTrue(s.Instance.ContainsAll(IntArray21));
  s.Instance := TSet.Create(IntArray11);
  s1.Instance := TSet.Create(IntArray10);
  s.Instance.AddAll([-5, -4, -3, -2, -1, 0]);
  s1.Instance.AddAll([-5, -4, -3, -2, -1, 0]);
  s.Instance.SymmetricSubtract(s1.Instance);
  AssertTrue(s.Instance.Count = 21);
  AssertTrue(s.Instance.ContainsAll(IntArray21));
end;

procedure THashSetLPTest.DistinctArray;
var
  a, a1: TIntArray;
begin
  a := TIntHelper.CreateCopy(IntArray21);
  a1 := TIntHelper.CreateMerge(IntArray21, a);
  a1 := TIntHelper.CreateMerge(a1, a1);
  AssertTrue(TIntHelper.Same(a, TSet.Distinct(a1).ToArray));
end;

procedure THashSetLPTest.DistinctArray_1;
var
  a: TIntArray;
  I, J, v, TestValue: Integer;
begin
  TestValue := 1111;
  System.SetLength(a, 100);
  for I := 0 to System.High(a) do
    a[I] := TestValue;
  J := 0;
  for v in TSet.Distinct(a) do
    begin
      I := v;
      Inc(J);
    end;
  AssertTrue(I = TestValue);
  AssertTrue(J = 1);
end;

procedure THashSetLPTest.DistinctEnum;
var
  a, a1: TIntArray;
  e: IIntEnumerable;
begin
  a := TIntHelper.CreateCopy(IntArray21);
  a1 := TIntHelper.CreateMerge(IntArray21, a);
  e := TIntArrayCursor.Create(TIntHelper.CreateMerge(a1, a1));
  AssertTrue(TIntHelper.Same(a, TSet.Distinct(e).ToArray));
end;

procedure THashSetLPTest.DistinctEnum_1;
var
  a: TIntArray;
  e: IIntEnumerable;
  I, J, v, TestValue: Integer;
begin
  TestValue := 1111;
  System.SetLength(a, 100);
  for I := 0 to System.High(a) do
    a[I] := TestValue;
  e := TIntArrayCursor.Create(a);
  J := 0;
  for v in TSet.Distinct(e) do
    begin
      I := v;
      Inc(J);
    end;
  AssertTrue(I = TestValue);
  AssertTrue(J = 1);
end;

procedure THashSetLPTest.ObjectSetRemove;
var
  s: TAutoObjSet;
  a: TObjArray;
  I, TestSize: Integer;
  Counter: TCounter;
begin
  TestSize := 36;
  Counter := Default(TCounter);
  System.SetLength(a, TestSize);
  for I := 0 to System.High(a) do
    begin
      a[I] := TTestObj.Create(Counter.Increment);
      s.Instance.Add(a[I]);
    end;
  AssertTrue(s.Instance.Count = TestSize);
  for I := 0 to System.High(a) do
    begin
      AssertTrue(s.Instance.Remove(a[I]));
      AssertTrue(s.Instance.Count = Pred(TestSize - I));
      AssertTrue(Counter.Count = Succ(I));
    end;
  AssertTrue(s.Instance.IsEmpty);
end;

procedure THashSetLPTest.ObjectSetRemoveArray;
var
  s: TAutoObjSet;
  a: TObjArray;
  I, TestSize: Integer;
  Counter: TCounter;
begin
  TestSize := 56;
  Counter := Default(TCounter);
  for I := 1 to TestSize do
    s.Instance.Add(TTestObj.Create(Counter.Increment));
  AssertTrue(s.Instance.Count = TestSize);
  a := s.Instance.ToArray;
  AssertTrue(System.Length(a) = TestSize);
  AssertTrue(s.Instance.RemoveAll(a) = TestSize);
  AssertTrue(s.Instance.IsEmpty);
  AssertTrue(Counter.Count = TestSize);
end;

procedure THashSetLPTest.ObjectSetRemoveEnum;
type
  TFakeEnum = specialize TGArrayCursor<TTestObj>;
var
  s: TAutoObjSet;
  e: TFakeEnum;
  I, TestSize: Integer;
  Counter: TCounter;
begin
  TestSize := 46;
  Counter := Default(TCounter);
  for I := 1 to TestSize do
    s.Instance.Add(TTestObj.Create(Counter.Increment));
  AssertTrue(s.Instance.Count = TestSize);
  e := TFakeEnum.Create(s.Instance.ToArray);
  AssertTrue(s.Instance.RemoveAll(e) = TestSize);
  AssertTrue(s.Instance.IsEmpty);
  AssertTrue(Counter.Count = TestSize);
end;

procedure THashSetLPTest.ObjectSetRemoveSelf;
var
  s: TAutoObjSet;
  I, TestSize: Integer;
  Counter: TCounter;
begin
  TestSize := 100;
  Counter := Default(TCounter);
  for I := 1 to TestSize do
    s.Instance.Add(TTestObj.Create(Counter.Increment));
  AssertTrue(s.Instance.Count = TestSize);
  AssertTrue(s.Instance.RemoveAll(s.Instance) = TestSize);
  AssertTrue(s.Instance.IsEmpty);
  AssertTrue(Counter.Count = TestSize);
end;

procedure THashSetLPTest.ObjectSetRemoveIfRegular;
var
  s: TAutoObjSet;
  I, TestSize: Integer;
  Counter: TCounter;
  o: TTestObj;
begin
  TestSize := 100;
  Counter := Default(TCounter);
  for I := 1 to TestSize do
    s.Instance.Add(TTestObj.Create(I, Counter.Increment));
  AssertTrue(s.Instance.Count = TestSize);
  AssertTrue(s.Instance.RemoveIf(@IsEvenObj) = TestSize div 2);
  AssertTrue(Counter.Count = TestSize div 2);
  AssertTrue(s.Instance.Count = TestSize div 2);
  for o in s.Instance do
    AssertTrue(Odd(o.Value));
  s.Instance := Default(TObjSet);
  AssertTrue(Counter.Count = TestSize);
end;

procedure THashSetLPTest.ObjectSetRemoveIfDelegated;
var
  s: TAutoObjSet;
  I, TestSize: Integer;
  Counter: TCounter;
  o: TTestObj;
begin
  TestSize := 100;
  Counter := Default(TCounter);
  for I := 1 to TestSize do
    s.Instance.Add(TTestObj.Create(I, Counter.Increment));
  AssertTrue(s.Instance.Count = TestSize);
  AssertTrue(s.Instance.RemoveIf(@ObjIsEven) = TestSize div 2);
  AssertTrue(Counter.Count = TestSize div 2);
  AssertTrue(s.Instance.Count = TestSize div 2);
  for o in s.Instance do
    AssertTrue(Odd(o.Value));
  s.Instance := Default(TObjSet);
  AssertTrue(Counter.Count = TestSize);
end;

procedure THashSetLPTest.ObjectSetRemoveIfNested;
  function IsEvenNest(constref aObj: TTestObj): Boolean;
  begin
    Result := not Odd(aObj.Value);
  end;
var
  s: TAutoObjSet;
  I, TestSize: Integer;
  Counter: TCounter;
  o: TTestObj;
begin
  TestSize := 100;
  Counter := Default(TCounter);
  for I := 1 to TestSize do
    s.Instance.Add(TTestObj.Create(I, Counter.Increment));
  AssertTrue(s.Instance.Count = TestSize);
  AssertTrue(s.Instance.RemoveIf(@IsEvenNest) = TestSize div 2);
  AssertTrue(Counter.Count = TestSize div 2);
  AssertTrue(s.Instance.Count = TestSize div 2);
  for o in s.Instance do
    AssertTrue(Odd(o.Value));
  s.Instance := Default(TObjSet);
  AssertTrue(Counter.Count = TestSize);
end;

procedure THashSetLPTest.ObjectSetExtractIfRegular;
var
  s: TAutoObjSet;
  I, TestSize: Integer;
  Counter: TCounter;
  a: TObjArray;
  o: TTestObj;
begin
  TestSize := 100;
  Counter := Default(TCounter);
  for I := 1 to TestSize do
    s.Instance.Add(TTestObj.Create(I, Counter.Increment));
  AssertTrue(s.Instance.Count = TestSize);
  a := s.Instance.ExtractIf(@IsEvenObj);
  AssertTrue(System.Length(a) = TestSize div 2);
  AssertTrue(Counter.Count = 0);
  AssertTrue(s.Instance.Count = TestSize div 2);
  for o in a do
    begin
      AssertTrue(not Odd(o.Value));
      o.Free;
    end;
  for o in s.Instance do
    AssertTrue(Odd(o.Value));
  s.Instance := Default(TObjSet);
  AssertTrue(Counter.Count = TestSize);
end;

procedure THashSetLPTest.ObjectSetExtractIfDelegated;
var
  s: TAutoObjSet;
  I, TestSize: Integer;
  Counter: TCounter;
  a: TObjArray;
  o: TTestObj;
begin
  TestSize := 100;
  Counter := Default(TCounter);
  for I := 1 to TestSize do
    s.Instance.Add(TTestObj.Create(I, Counter.Increment));
  AssertTrue(s.Instance.Count = TestSize);
  a := s.Instance.ExtractIf(@ObjIsEven);
  AssertTrue(System.Length(a) = TestSize div 2);
  AssertTrue(Counter.Count = 0);
  AssertTrue(s.Instance.Count = TestSize div 2);
  for o in a do
    begin
      AssertTrue(not Odd(o.Value));
      o.Free;
    end;
  for o in s.Instance do
    AssertTrue(Odd(o.Value));
  s.Instance := Default(TObjSet);
  AssertTrue(Counter.Count = TestSize);
end;

procedure THashSetLPTest.ObjectSetExtractIfNested;
  function IsEvenNest(constref aObj: TTestObj): Boolean;
  begin
    Result := not Odd(aObj.Value);
  end;
var
  s: TAutoObjSet;
  I, TestSize: Integer;
  Counter: TCounter;
  a: TObjArray;
  o: TTestObj;
begin
  TestSize := 100;
  Counter := Default(TCounter);
  for I := 1 to TestSize do
    s.Instance.Add(TTestObj.Create(I, Counter.Increment));
  AssertTrue(s.Instance.Count = TestSize);
  a := s.Instance.ExtractIf(@IsEvenNest);
  AssertTrue(System.Length(a) = TestSize div 2);
  AssertTrue(Counter.Count = 0);
  AssertTrue(s.Instance.Count = TestSize div 2);
  for o in a do
    begin
      AssertTrue(not Odd(o.Value));
      o.Free;
    end;
  for o in s.Instance do
    AssertTrue(Odd(o.Value));
  s.Instance := Default(TObjSet);
  AssertTrue(Counter.Count = TestSize);
end;

procedure THashSetLPTest.ObjectSetRetain;
var
  s, s1: TAutoObjSet;
  I, TestSize: Integer;
  Counter: TCounter;
  a: TObjArray;
begin
  TestSize := 100;
  Counter := Default(TCounter);
  System.SetLength(a, TestSize div 2);
  s1.Instance := TObjSet.Create(False);

  for I := 0 to System.High(a) do
    a[I] := TTestObj.Create(Counter.Increment);
  s1.Instance.AddAll(a);
  s.Instance.AddAll(a);
  for I := 1 to TestSize div 2 do
    s.Instance.Add(TTestObj.Create(Counter.Increment));
  AssertTrue(s.Instance.Count = TestSize);
  s.Instance.RetainAll(s1.Instance);
  AssertTrue(s.Instance.Count = TestSize div 2);
  AssertTrue(Counter.Count = TestSize div 2);
  AssertTrue(s.Instance.ContainsAll(s1.Instance));
  s.Instance := Default(TObjSet);
  AssertTrue(Counter.Count = TestSize);
end;

procedure THashSetLPTest.ObjectSetSymmetricSubtract;
var
  s, s1: TAutoObjSet;
  I, TestSize: Integer;
  Counter: TCounter;
  a: TObjArray;
begin
  TestSize := 100;
  Counter := Default(TCounter);
  for I := 1 to TestSize do
    s.Instance.Add(TTestObj.Create(Counter.Increment));
  for I := 1 to TestSize do
    s1.Instance.Add(TTestObj.Create(Counter.Increment));
  System.SetLength(a, TestSize);
  for I := 0 to System.High(a) do
    a[I] := TTestObj.Create(Counter.Increment);
  s.Instance.AddAll(a);
  AssertTrue(s.Instance.Count = TestSize * 2);
  s1.Instance.AddAll(a);
  AssertTrue(s1.Instance.Count = TestSize * 2);
  s1.Instance.OwnsObjects := False;
  s.Instance.SymmetricSubtract(s1.Instance);
  AssertTrue(s.Instance.Count = TestSize * 2);
  AssertTrue(Counter.Count = TestSize);
  AssertTrue(s1.Instance.Count = TestSize * 2);
  s.Instance := Default(TObjSet);
  AssertTrue(Counter.Count = TestSize * 3);
end;

procedure THashSetLPTTest.TestCreate;
var
  s: TAutoSet;
begin
  AssertTrue(s.Instance.Count = 0);
  AssertTrue(s.Instance.LoadFactor = s.Instance.DefaultLoadFactor);
end;

procedure THashSetLPTTest.CreateArray;
var
  s: TAutoSet;
  I: Integer;
begin
  s.Instance := TSet.Create(IntArray11);
  AssertTrue(s.Instance.Count = 11);
  AssertTrue(s.Instance.Capacity >= 11);
  AssertTrue(s.Instance.LoadFactor = s.Instance.DefaultLoadFactor);
  for I in IntArray11 do
    AssertTrue(s.Instance.Contains(I));
end;

procedure THashSetLPTTest.CreateEnum;
var
  s: TAutoSet;
  I: Integer;
  e: IIntEnumerable;
begin
  e := TIntArrayCursor.Create(TIntHelper.CreateCopy(IntArray11));
  s.Instance := TSet.Create(e);
  AssertTrue(s.Instance.Count = 11);
  AssertTrue(s.Instance.Capacity >= 11);
  AssertTrue(s.Instance.LoadFactor = s.Instance.DefaultLoadFactor);
  for I in IntArray11 do
    AssertTrue(s.Instance.Contains(I));
end;

procedure THashSetLPTTest.CreateCapacity0;
var
  s: TAutoSet;
begin
  s.Instance := TSet.Create(0);
  AssertTrue(s.Instance.Count = 0);
  AssertTrue(s.Instance.Capacity > 0);
  AssertTrue(s.Instance.LoadFactor = s.Instance.DefaultLoadFactor);
end;

procedure THashSetLPTTest.CreateCapacity5;
var
  s: TAutoSet;
begin
  s.Instance := TSet.Create(5);
  AssertTrue(s.Instance.Count = 0);
  AssertTrue(s.Instance.Capacity >= 5);
  AssertTrue(s.Instance.LoadFactor = s.Instance.DefaultLoadFactor);
end;

procedure THashSetLPTTest.CreateCapacity50Array11;
var
  s: TAutoSet;
  I: Integer;
begin
  s.Instance := TSet.Create(50, IntArray11);
  AssertTrue(s.Instance.Count = 11);
  AssertTrue(s.Instance.Capacity >= 50);
  AssertTrue(s.Instance.LoadFactor = s.Instance.DefaultLoadFactor);
  for I in IntArray11 do
    AssertTrue(s.Instance.Contains(I));
end;

procedure THashSetLPTTest.CreateCapacity50Enum11;
var
  s: TAutoSet;
  I: Integer;
  e: IIntEnumerable;
begin
  e := TIntArrayCursor.Create(TIntHelper.CreateCopy(IntArray11));
  s.Instance := TSet.Create(50, e);
  AssertTrue(s.Instance.Count = 11);
  AssertTrue(s.Instance.Capacity >= 50);
  AssertTrue(s.Instance.LoadFactor = s.Instance.DefaultLoadFactor);
  for I in IntArray11 do
    AssertTrue(s.Instance.Contains(I));
end;

procedure THashSetLPTTest.CreateLoadFactor0;
var
  s: TAutoSet;
begin
  s.Instance := TSet.Create(0.0);
  AssertTrue(s.Instance.Count = 0);
  AssertTrue(s.Instance.LoadFactor = TSet.MinLoadFactor);
end;

procedure THashSetLPTTest.CreateLoadFactor3;
var
  s: TAutoSet;
begin
  s.Instance := TSet.Create(3.0);
  AssertTrue(s.Instance.Count = 0);
  AssertTrue(s.Instance.LoadFactor = TSet.MaxLoadFactor);
end;

procedure THashSetLPTTest.CreateLoadFactor;
var
  s: TAutoSet;
  lf: Single;
begin
  lf := 0.5;
  s.Instance := TSet.Create(lf);
  AssertTrue(s.Instance.Count = 0);
  AssertTrue(s.Instance.LoadFactor = lf);
end;

procedure THashSetLPTTest.CreateLoadFactor075Array11;
var
  s: TAutoSet;
  I: Integer;
  lf: Single;
begin
  lf := 0.75;
  s.Instance := TSet.Create(lf, IntArray11);
  AssertTrue(s.Instance.Count = 11);
  AssertTrue(s.Instance.LoadFactor = lf);
  for I in IntArray11 do
    AssertTrue(s.Instance.Contains(I));
end;

procedure THashSetLPTTest.CreateLoadFactor075Enum11;
var
  s: TAutoSet;
  I: Integer;
  e: IIntEnumerable;
  lf: Single;
begin
  lf := 0.75;
  e := TIntArrayCursor.Create(TIntHelper.CreateCopy(IntArray11));
  s.Instance := TSet.Create(lf, e);
  AssertTrue(s.Instance.Count = 11);
  AssertTrue(s.Instance.LoadFactor = lf);
  for I in IntArray11 do
    AssertTrue(s.Instance.Contains(I));
end;

procedure THashSetLPTTest.CreateCapacityLoadFactor;
var
  s: TAutoSet;
  lf: Single;
begin
  lf := 0.45;
  s.Instance := TSet.Create(50);
  AssertTrue(s.Instance.Count = 0);
  s.Instance.LoadFactor := lf;
  AssertTrue(s.Instance.Capacity >= 50);
  AssertTrue(s.Instance.LoadFactor = lf);
end;

procedure THashSetLPTTest.CreateCapacityLoadFactor075Array11;
var
  s: TAutoSet;
  I: Integer;
  lf: Single;
begin
  lf := 0.75;
  s.Instance := TSet.Create(50, lf, IntArray11);
  AssertTrue(s.Instance.Count = 11);
  AssertTrue(s.Instance.Capacity >= 50);
  AssertTrue(s.Instance.LoadFactor = lf);
  for I in IntArray11 do
    AssertTrue(s.Instance.Contains(I));
end;

procedure THashSetLPTTest.CreateCapacityLoadFactor075Enum11;
var
  s: TAutoSet;
  I: Integer;
  e: IIntEnumerable;
  lf: Single;
begin
  lf := 0.75;
  e := TIntArrayCursor.Create(TIntHelper.CreateCopy(IntArray11));
  s.Instance := TSet.Create(50, lf, e);
  AssertTrue(s.Instance.Count = 11);
  AssertTrue(s.Instance.Capacity >= 50);
  AssertTrue(s.Instance.LoadFactor = lf);
  for I in IntArray11 do
    AssertTrue(s.Instance.Contains(I));
end;

procedure THashSetLPTTest.Clear;
var
  s: TAutoSet;
begin
  s.Instance := TSet.Create(IntArray11);
  AssertTrue(s.Instance.Count = 11);
  AssertTrue(s.Instance.Capacity > 0);
  s.Instance.Clear;
  AssertTrue(s.Instance.Count = 0);
  AssertTrue(s.Instance.Capacity = 0);
end;

procedure THashSetLPTTest.EnsureCapacity;
var
  s: TAutoSet;
  c: SizeInt;
begin
  c := s.Instance.ExpandTreshold;
  s.Instance.EnsureCapacity(c + 1);
  AssertTrue(s.Instance.Capacity > c);
end;

procedure THashSetLPTTest.TrimToFit;
var
  s: TAutoSet;
  c: SizeInt;
begin
  s.Instance := TSet.Create(0.85, IntArray11);
  c := s.Instance.Capacity;
  s.Instance.TrimToFit;
  AssertTrue(s.Instance.Capacity < c);
end;

procedure THashSetLPTTest.Add;
var
  s: TAutoSet;
begin
  AssertTrue(s.Instance.Count = 0);
  AssertTrue(s.Instance.Add(1));
  AssertTrue(s.Instance.Count = 1);
  AssertTrue(s.Instance.Contains(1));
  AssertTrue(s.Instance.Add(51));
  AssertTrue(s.Instance.Count = 2);
  AssertTrue(s.Instance.Contains(51));
  AssertFalse(s.Instance.Add(1));
  AssertFalse(s.Instance.Add(51));
  AssertTrue(s.Instance.Count = 2);
  AssertTrue(s.Instance.Add(52));
  AssertTrue(s.Instance.Count = 3);
  AssertTrue(s.Instance.Contains(52));
end;

procedure THashSetLPTTest.Add100;
var
  s: TAutoSet;
  I: Integer;
begin
  for I := 1 to 100 do
    AssertTrue(s.Instance.Add(I));
  AssertTrue(s.Instance.Count = 100);
  for I := 1 to 100 do
    AssertFalse(s.Instance.Add(I));
  AssertTrue(s.Instance.Count = 100);
  for I := 1 to 100 do
    AssertTrue(s.Instance.Contains(I));
end;

procedure THashSetLPTTest.AddArray;
var
  s: TAutoSet;
begin
  AssertTrue(s.Instance.AddAll(IntArray21) = System.Length(IntArray21));
  AssertTrue(s.Instance.Count = System.Length(IntArray21));
  AssertTrue(s.Instance.ContainsAll(IntArray21));
end;

procedure THashSetLPTTest.AddEnum;
var
  s: TAutoSet;
  e: IIntEnumerable;
begin
  e := TIntArrayCursor.Create(TIntHelper.CreateCopy(IntArray21));
  AssertTrue(s.Instance.AddAll(e) = System.Length(IntArray21));
  AssertTrue(s.Instance.Count = System.Length(IntArray21));
  AssertTrue(s.Instance.ContainsAll(IntArray21));
end;

procedure THashSetLPTTest.AddSelf;
var
  s: TAutoSet;
begin
  s.Instance.AddAll(IntArray21);
  AssertTrue(s.Instance.AddAll(s.Instance) = 0);
  AssertTrue(s.Instance.Count = System.Length(IntArray21));
end;

procedure THashSetLPTTest.Remove;
var
  s: TAutoSet;
begin
  s.Instance := TSet.Create(IntArray21);
  AssertTrue(s.Instance.Count = System.Length(IntArray21));
  AssertTrue(s.Instance.Remove(IntArray21[1]));
  AssertTrue(s.Instance.Count = System.Length(IntArray21) - 1);
  AssertTrue(s.Instance.Remove(IntArray21[System.High(IntArray21)]));
  AssertTrue(s.Instance.Count = System.Length(IntArray21) - 2);
  AssertFalse(s.Instance.Remove(-1));
  AssertTrue(s.Instance.Count = System.Length(IntArray21) - 2);
  AssertFalse(s.Instance.Remove(100));
  AssertTrue(s.Instance.Count = System.Length(IntArray21) - 2);
end;

procedure THashSetLPTTest.RemoveArray;
var
  s: TAutoSet;
begin
  AssertTrue(s.Instance.RemoveAll(IntArray11) = 0);
  s.Instance := TSet.Create(IntArray21);
  AssertTrue(s.Instance.RemoveAll(IntArray11) = 11);
  AssertTrue(s.Instance.Count = 10);
end;

procedure THashSetLPTTest.RemoveEnum;
var
  s: TAutoSet;
  e: IIntEnumerable;
begin
  AssertTrue(s.Instance.RemoveAll(TIntArrayCursor.Create(TIntHelper.CreateCopy(IntArray11))) = 0);
  e := TIntArrayCursor.Create(TIntHelper.CreateCopy(IntArray11));
  s.Instance := TSet.Create(IntArray21);
  AssertTrue(s.Instance.RemoveAll(e) = 11);
  AssertTrue(s.Instance.Count = 10);
end;

procedure THashSetLPTTest.RemoveSelf;
var
  s: TAutoSet;
begin
  s.Instance := TSet.Create(IntArray21);
  AssertTrue(s.Instance.RemoveAll(s.Instance) = 21);
  AssertTrue(s.Instance.IsEmpty);
end;

procedure THashSetLPTTest.Tombstones;
var
  s: TAutoSet;
  c: SizeInt;
begin
  s.Instance := TSet.Create(IntArray21);
  c := s.Instance.Capacity;
  AssertTrue(s.Instance.TombstonesCount = 0);
  AssertTrue(s.Instance.Remove(IntArray21[1]));
  AssertTrue(s.Instance.TombstonesCount = 1);
  AssertTrue(s.Instance.Remove(IntArray21[2]));
  AssertTrue(s.Instance.TombstonesCount = 2);
  AssertTrue(s.Instance.Add(IntArray21[1]));
  AssertTrue(s.Instance.TombstonesCount = 1);
  AssertTrue(s.Instance.Add(IntArray21[2]));
  AssertTrue(s.Instance.TombstonesCount = 0);
  AssertTrue(s.Instance.RemoveAll(IntArray21) = 21);
  AssertTrue(s.Instance.TombstonesCount = 21);
  s.Instance.ClearTombstones;
  AssertTrue(s.Instance.TombstonesCount = 0);
  AssertTrue(s.Instance.Capacity = c);
end;

procedure THashSetLPTTest.RemoveIfRegular;
var
  s: TAutoSet;
begin
  s.Instance := TSet.Create(IntArray21);
  AssertTrue(s.Instance.RemoveIf(@IsEvenInt) = 10);
  AssertTrue(s.Instance.Count = 11);
  AssertTrue(s.Instance.ContainsAll(IntArray11));
end;

procedure THashSetLPTTest.RemoveIfDelegated;
var
  s: TAutoSet;
begin
  s.Instance := TSet.Create(IntArray21);
  AssertTrue(s.Instance.RemoveIf(@IsEven) = 10);
  AssertTrue(s.Instance.Count = 11);
  AssertTrue(s.Instance.ContainsAll(IntArray11));
end;

procedure THashSetLPTTest.RemoveIfNested;
  function IsEvenInt(constref aValue: Integer): Boolean;
  begin
    Result := not Odd(aValue);
  end;
var
  s: TAutoSet;
begin
  s.Instance := TSet.Create(IntArray21);
  AssertTrue(s.Instance.RemoveIf(@IsEvenInt) = 10);
  AssertTrue(s.Instance.Count = 11);
  AssertTrue(s.Instance.ContainsAll(IntArray11));
end;

procedure THashSetLPTTest.ExtractIfRegular;
var
  s: TAutoSet;
  e: TIntArray;
begin
  s.Instance := TSet.Create(IntArray21);
  e := s.Instance.ExtractIf(@IsEvenInt);
  AssertTrue(s.Instance.Count = 11);
  AssertTrue(s.Instance.ContainsAll(IntArray11));
  AssertTrue(System.Length(e) = 10);
  TIntHelper.QuickSort(e);
  AssertTrue(TIntHelper.Same(IntArray10, e));
end;

procedure THashSetLPTTest.ExtractIfDelegated;
var
  s: TAutoSet;
  e: TIntArray;
begin
  s.Instance := TSet.Create(IntArray21);
  e := s.Instance.ExtractIf(@IsEven);
  AssertTrue(s.Instance.Count = 11);
  AssertTrue(s.Instance.ContainsAll(IntArray11));
  AssertTrue(System.Length(e) = 10);
  TIntHelper.QuickSort(e);
  AssertTrue(TIntHelper.Same(IntArray10, e));
end;

procedure THashSetLPTTest.ExtractIfNested;
  function IsEvenInt(constref aValue: Integer): Boolean;
  begin
    Result := not Odd(aValue);
  end;
var
  s: TAutoSet;
  e: TIntArray;
begin
  s.Instance := TSet.Create(IntArray21);
  e := s.Instance.ExtractIf(@IsEvenInt);
  AssertTrue(s.Instance.Count = 11);
  AssertTrue(s.Instance.ContainsAll(IntArray11));
  AssertTrue(System.Length(e) = 10);
  TIntHelper.QuickSort(e);
  AssertTrue(TIntHelper.Same(IntArray10, e));
end;

procedure THashSetLPTTest.Retain;
var
  s, s1: TAutoSet;
begin
  s.Instance := TSet.Create(IntArray21);
  s1.Instance := TSet.Create(IntArray11);
  s.Instance.AddAll([-3, -2, -1, 0]);
  AssertTrue(s.Instance.Count = 25);
  s.Instance.RetainAll(s1.Instance);
  AssertTrue(s.Instance.Count = 11);
  AssertTrue(s.Instance.ContainsAll(s1.Instance));
end;

procedure THashSetLPTTest.Retain_1;
var
  s, s1: TAutoSet;
begin
  s.Instance := TSet.Create(IntArray10);
  s1.Instance := TSet.Create(IntArray11);
  s.Instance.RetainAll(s1.Instance);
  AssertTrue(s.Instance.IsEmpty);
end;

procedure THashSetLPTTest.RetainSelf;
var
  s: TAutoSet;
begin
  s.Instance := TSet.Create(IntArray21);
  AssertTrue(s.Instance.Count = 21);
  s.Instance.RetainAll(s.Instance);
  AssertTrue(s.Instance.Count = 21);
  AssertTrue(s.Instance.ContainsAll(IntArray21));
end;

procedure THashSetLPTTest.IsSuperset;
var
  s, s1: TAutoSet;
begin
  s.Instance := TSet.Create(IntArray21);
  s1.Instance := TSet.Create(IntArray21);
  AssertTrue(s.Instance.IsSuperset(s1.Instance));
  AssertTrue(s1.Instance.IsSuperset(s.Instance));
  AssertTrue(s.Instance.IsSuperset(s.Instance));
  s1.Instance := TSet.Create(IntArray11);
  AssertTrue(s.Instance.IsSuperset(s1.Instance));
  AssertFalse(s1.Instance.IsSuperset(s.Instance));
end;

procedure THashSetLPTTest.IsSubset;
var
  s, s1: TAutoSet;
begin
  s.Instance := TSet.Create(IntArray21);
  s1.Instance := TSet.Create(IntArray21);
  AssertTrue(s.Instance.IsSubset(s1.Instance));
  AssertTrue(s1.Instance.IsSubset(s.Instance));
  AssertTrue(s.Instance.IsSubset(s.Instance));
  s1.Instance := TSet.Create(IntArray11);
  AssertTrue(s1.Instance.IsSubset(s.Instance));
  AssertFalse(s.Instance.IsSubset(s1.Instance));
end;

procedure THashSetLPTTest.IsEqual;
var
  s, s1: TAutoSet;
begin
  s.Instance := TSet.Create(IntArray21);
  s1.Instance := TSet.Create(IntArray21);
  AssertTrue(s.Instance.IsEqual(s.Instance));
  AssertTrue(s.Instance.IsEqual(s1.Instance));
  s1.Instance := TSet.Create(IntArray11);
  AssertFalse(s.Instance.IsEqual(s1.Instance));
end;

procedure THashSetLPTTest.Intersecting;
var
  s, s1: TAutoSet;
begin
  s.Instance := TSet.Create(IntArray21);
  s1.Instance := TSet.Create(IntArray11);
  AssertTrue(s.Instance.Intersecting(s.Instance));
  AssertTrue(s.Instance.Intersecting(s1.Instance));
  s.Instance := TSet.Create(IntArray10);
  AssertFalse(s.Instance.Intersecting(s1.Instance));
  AssertFalse(s1.Instance.Intersecting(s.Instance));
end;

procedure THashSetLPTTest.SymmetricSubtract;
var
  s, s1: TAutoSet;
begin
  s.Instance := TSet.Create(IntArray21);
  s1.Instance := TSet.Create(IntArray21);
  s.Instance.SymmetricSubtract(s1.Instance);
  AssertTrue(s.Instance.IsEmpty);
  s.Instance := TSet.Create(IntArray21);
  s1.Instance.Clear;
  s.Instance.SymmetricSubtract(s1.Instance);
  AssertTrue(s.Instance.Count = 21);
  AssertTrue(s.Instance.ContainsAll(IntArray21));
  s.Instance := TSet.Create(IntArray11);
  s1.Instance := TSet.Create(IntArray10);
  s.Instance.SymmetricSubtract(s1.Instance);
  AssertTrue(s.Instance.Count = 21);
  AssertTrue(s.Instance.ContainsAll(IntArray21));
  s.Instance := TSet.Create(IntArray11);
  s1.Instance := TSet.Create(IntArray10);
  s.Instance.AddAll([-5, -4, -3, -2, -1, 0]);
  s1.Instance.AddAll([-5, -4, -3, -2, -1, 0]);
  s.Instance.SymmetricSubtract(s1.Instance);
  AssertTrue(s.Instance.Count = 21);
  AssertTrue(s.Instance.ContainsAll(IntArray21));
end;

procedure THashSetLPTTest.ObjectSetRemove;
var
  s: TAutoObjSet;
  a: TObjArray;
  I, TestSize: Integer;
  Counter: TCounter;
begin
  TestSize := 36;
  Counter := Default(TCounter);
  System.SetLength(a, TestSize);
  for I := 0 to System.High(a) do
    begin
      a[I] := TTestObj.Create(Counter.Increment);
      s.Instance.Add(a[I]);
    end;
  AssertTrue(s.Instance.Count = TestSize);
  for I := 0 to System.High(a) do
    begin
      AssertTrue(s.Instance.Remove(a[I]));
      AssertTrue(s.Instance.Count = Pred(TestSize - I));
      AssertTrue(Counter.Count = Succ(I));
    end;
  AssertTrue(s.Instance.IsEmpty);
end;

procedure THashSetLPTTest.ObjectSetRemoveArray;
var
  s: TAutoObjSet;
  a: TObjArray;
  I, TestSize: Integer;
  Counter: TCounter;
begin
  TestSize := 56;
  Counter := Default(TCounter);
  for I := 1 to TestSize do
    s.Instance.Add(TTestObj.Create(Counter.Increment));
  AssertTrue(s.Instance.Count = TestSize);
  a := s.Instance.ToArray;
  AssertTrue(System.Length(a) = TestSize);
  AssertTrue(s.Instance.RemoveAll(a) = TestSize);
  AssertTrue(s.Instance.IsEmpty);
  AssertTrue(Counter.Count = TestSize);
end;

procedure THashSetLPTTest.ObjectSetRemoveEnum;
type
  TFakeEnum = specialize TGArrayCursor<TTestObj>;
var
  s: TAutoObjSet;
  e: TFakeEnum;
  I, TestSize: Integer;
  Counter: TCounter;
begin
  TestSize := 46;
  Counter := Default(TCounter);
  for I := 1 to TestSize do
    s.Instance.Add(TTestObj.Create(Counter.Increment));
  AssertTrue(s.Instance.Count = TestSize);
  e := TFakeEnum.Create(s.Instance.ToArray);
  AssertTrue(s.Instance.RemoveAll(e) = TestSize);
  AssertTrue(s.Instance.IsEmpty);
  AssertTrue(Counter.Count = TestSize);
end;

procedure THashSetLPTTest.ObjectSetRemoveSelf;
var
  s: TAutoObjSet;
  I, TestSize: Integer;
  Counter: TCounter;
begin
  TestSize := 100;
  Counter := Default(TCounter);
  for I := 1 to TestSize do
    s.Instance.Add(TTestObj.Create(Counter.Increment));
  AssertTrue(s.Instance.Count = TestSize);
  AssertTrue(s.Instance.RemoveAll(s.Instance) = TestSize);
  AssertTrue(s.Instance.IsEmpty);
  AssertTrue(Counter.Count = TestSize);
end;

procedure THashSetLPTTest.ObjectSetRemoveIfRegular;
var
  s: TAutoObjSet;
  I, TestSize: Integer;
  Counter: TCounter;
  o: TTestObj;
begin
  TestSize := 100;
  Counter := Default(TCounter);
  for I := 1 to TestSize do
    s.Instance.Add(TTestObj.Create(I, Counter.Increment));
  AssertTrue(s.Instance.Count = TestSize);
  AssertTrue(s.Instance.RemoveIf(@IsEvenObj) = TestSize div 2);
  AssertTrue(Counter.Count = TestSize div 2);
  AssertTrue(s.Instance.Count = TestSize div 2);
  for o in s.Instance do
    AssertTrue(Odd(o.Value));
  s.Instance := Default(TObjSet);
  AssertTrue(Counter.Count = TestSize);
end;

procedure THashSetLPTTest.ObjectSetRemoveIfDelegated;
var
  s: TAutoObjSet;
  I, TestSize: Integer;
  Counter: TCounter;
  o: TTestObj;
begin
  TestSize := 100;
  Counter := Default(TCounter);
  for I := 1 to TestSize do
    s.Instance.Add(TTestObj.Create(I, Counter.Increment));
  AssertTrue(s.Instance.Count = TestSize);
  AssertTrue(s.Instance.RemoveIf(@ObjIsEven) = TestSize div 2);
  AssertTrue(Counter.Count = TestSize div 2);
  AssertTrue(s.Instance.Count = TestSize div 2);
  for o in s.Instance do
    AssertTrue(Odd(o.Value));
  s.Instance := Default(TObjSet);
  AssertTrue(Counter.Count = TestSize);
end;

procedure THashSetLPTTest.ObjectSetRemoveIfNested;
  function IsEvenNest(constref aObj: TTestObj): Boolean;
  begin
    Result := not Odd(aObj.Value);
  end;
var
  s: TAutoObjSet;
  I, TestSize: Integer;
  Counter: TCounter;
  o: TTestObj;
begin
  TestSize := 100;
  Counter := Default(TCounter);
  for I := 1 to TestSize do
    s.Instance.Add(TTestObj.Create(I, Counter.Increment));
  AssertTrue(s.Instance.Count = TestSize);
  AssertTrue(s.Instance.RemoveIf(@IsEvenNest) = TestSize div 2);
  AssertTrue(Counter.Count = TestSize div 2);
  AssertTrue(s.Instance.Count = TestSize div 2);
  for o in s.Instance do
    AssertTrue(Odd(o.Value));
  s.Instance := Default(TObjSet);
  AssertTrue(Counter.Count = TestSize);
end;

procedure THashSetLPTTest.ObjectSetExtractIfRegular;
var
  s: TAutoObjSet;
  I, TestSize: Integer;
  Counter: TCounter;
  a: TObjArray;
  o: TTestObj;
begin
  TestSize := 100;
  Counter := Default(TCounter);
  for I := 1 to TestSize do
    s.Instance.Add(TTestObj.Create(I, Counter.Increment));
  AssertTrue(s.Instance.Count = TestSize);
  a := s.Instance.ExtractIf(@IsEvenObj);
  AssertTrue(System.Length(a) = TestSize div 2);
  AssertTrue(Counter.Count = 0);
  AssertTrue(s.Instance.Count = TestSize div 2);
  for o in a do
    begin
      AssertTrue(not Odd(o.Value));
      o.Free;
    end;
  for o in s.Instance do
    AssertTrue(Odd(o.Value));
  s.Instance := Default(TObjSet);
  AssertTrue(Counter.Count = TestSize);
end;

procedure THashSetLPTTest.ObjectSetExtractIfDelegated;
var
  s: TAutoObjSet;
  I, TestSize: Integer;
  Counter: TCounter;
  a: TObjArray;
  o: TTestObj;
begin
  TestSize := 100;
  Counter := Default(TCounter);
  for I := 1 to TestSize do
    s.Instance.Add(TTestObj.Create(I, Counter.Increment));
  AssertTrue(s.Instance.Count = TestSize);
  a := s.Instance.ExtractIf(@ObjIsEven);
  AssertTrue(System.Length(a) = TestSize div 2);
  AssertTrue(Counter.Count = 0);
  AssertTrue(s.Instance.Count = TestSize div 2);
  for o in a do
    begin
      AssertTrue(not Odd(o.Value));
      o.Free;
    end;
  for o in s.Instance do
    AssertTrue(Odd(o.Value));
  s.Instance := Default(TObjSet);
  AssertTrue(Counter.Count = TestSize);
end;

procedure THashSetLPTTest.ObjectSetExtractIfNested;
  function IsEvenNest(constref aObj: TTestObj): Boolean;
  begin
    Result := not Odd(aObj.Value);
  end;
var
  s: TAutoObjSet;
  I, TestSize: Integer;
  Counter: TCounter;
  a: TObjArray;
  o: TTestObj;
begin
  TestSize := 100;
  Counter := Default(TCounter);
  for I := 1 to TestSize do
    s.Instance.Add(TTestObj.Create(I, Counter.Increment));
  AssertTrue(s.Instance.Count = TestSize);
  a := s.Instance.ExtractIf(@IsEvenNest);
  AssertTrue(System.Length(a) = TestSize div 2);
  AssertTrue(Counter.Count = 0);
  AssertTrue(s.Instance.Count = TestSize div 2);
  for o in a do
    begin
      AssertTrue(not Odd(o.Value));
      o.Free;
    end;
  for o in s.Instance do
    AssertTrue(Odd(o.Value));
  s.Instance := Default(TObjSet);
  AssertTrue(Counter.Count = TestSize);
end;

procedure THashSetLPTTest.ObjectSetRetain;
var
  s, s1: TAutoObjSet;
  I, TestSize: Integer;
  Counter: TCounter;
  a: TObjArray;
begin
  TestSize := 100;
  Counter := Default(TCounter);
  System.SetLength(a, TestSize div 2);
  s1.Instance := TObjSet.Create(False);

  for I := 0 to System.High(a) do
    a[I] := TTestObj.Create(Counter.Increment);
  s1.Instance.AddAll(a);
  s.Instance.AddAll(a);
  for I := 1 to TestSize div 2 do
    s.Instance.Add(TTestObj.Create(Counter.Increment));
  AssertTrue(s.Instance.Count = TestSize);
  s.Instance.RetainAll(s1.Instance);
  AssertTrue(s.Instance.Count = TestSize div 2);
  AssertTrue(Counter.Count = TestSize div 2);
  AssertTrue(s.Instance.ContainsAll(s1.Instance));
  s.Instance := Default(TObjSet);
  AssertTrue(Counter.Count = TestSize);
end;

procedure THashSetLPTTest.ObjectSetSymmetricSubtract;
var
  s, s1: TAutoObjSet;
  I, TestSize: Integer;
  Counter: TCounter;
  a: TObjArray;
begin
  TestSize := 100;
  Counter := Default(TCounter);
  for I := 1 to TestSize do
    s.Instance.Add(TTestObj.Create(Counter.Increment));
  for I := 1 to TestSize do
    s1.Instance.Add(TTestObj.Create(Counter.Increment));
  System.SetLength(a, TestSize);
  for I := 0 to System.High(a) do
    a[I] := TTestObj.Create(Counter.Increment);
  s.Instance.AddAll(a);
  AssertTrue(s.Instance.Count = TestSize * 2);
  s1.Instance.AddAll(a);
  AssertTrue(s1.Instance.Count = TestSize * 2);
  s1.Instance.OwnsObjects := False;
  s.Instance.SymmetricSubtract(s1.Instance);
  AssertTrue(s.Instance.Count = TestSize * 2);
  AssertTrue(Counter.Count = TestSize);
  AssertTrue(s1.Instance.Count = TestSize * 2);
  s.Instance := Default(TObjSet);
  AssertTrue(Counter.Count = TestSize * 3);
end;

procedure THashSetQPTest.TestCreate;
var
  s: TAutoSet;
begin
  AssertTrue(s.Instance.Count = 0);
  AssertTrue(s.Instance.Capacity > 0);
  AssertTrue(s.Instance.LoadFactor = s.Instance.DefaultLoadFactor);
end;

procedure THashSetQPTest.CreateArray;
var
  s: TAutoSet;
  I: Integer;
begin
  s.Instance := TSet.Create(IntArray11);
  AssertTrue(s.Instance.Count = 11);
  AssertTrue(s.Instance.LoadFactor = s.Instance.DefaultLoadFactor);
  for I in IntArray11 do
    AssertTrue(s.Instance.Contains(I));
end;

procedure THashSetQPTest.CreateEnum;
var
  s: TAutoSet;
  I: Integer;
  e: IIntEnumerable;
begin
  e := TIntArrayCursor.Create(TIntHelper.CreateCopy(IntArray11));
  s.Instance := TSet.Create(e);
  AssertTrue(s.Instance.Count = 11);
  AssertTrue(s.Instance.LoadFactor = s.Instance.DefaultLoadFactor);
  for I in IntArray11 do
    AssertTrue(s.Instance.Contains(I));
end;

procedure THashSetQPTest.CreateCapacity0;
var
  s: TAutoSet;
begin
  s.Instance := TSet.Create(0);
  AssertTrue(s.Instance.Count = 0);
  AssertTrue(s.Instance.Capacity > 0);
  AssertTrue(s.Instance.LoadFactor = s.Instance.DefaultLoadFactor);
end;

procedure THashSetQPTest.CreateCapacity5;
var
  s: TAutoSet;
begin
  s.Instance := TSet.Create(5);
  AssertTrue(s.Instance.Count = 0);
  AssertTrue(s.Instance.Capacity >= 5);
  AssertTrue(s.Instance.LoadFactor = s.Instance.DefaultLoadFactor);
end;

procedure THashSetQPTest.CreateCapacity50Array11;
var
  s: TAutoSet;
  I: Integer;
begin
  s.Instance := TSet.Create(50, IntArray11);
  AssertTrue(s.Instance.Count = 11);
  AssertTrue(s.Instance.Capacity >= 50);
  AssertTrue(s.Instance.LoadFactor = s.Instance.DefaultLoadFactor);
  for I in IntArray11 do
    AssertTrue(s.Instance.Contains(I));
end;

procedure THashSetQPTest.CreateCapacity50Enum11;
var
  s: TAutoSet;
  I: Integer;
  e: IIntEnumerable;
begin
  e := TIntArrayCursor.Create(TIntHelper.CreateCopy(IntArray11));
  s.Instance := TSet.Create(50, e);
  AssertTrue(s.Instance.Count = 11);
  AssertTrue(s.Instance.Capacity >= 50);
  AssertTrue(s.Instance.LoadFactor = s.Instance.DefaultLoadFactor);
  for I in IntArray11 do
    AssertTrue(s.Instance.Contains(I));
end;

procedure THashSetQPTest.CreateLoadFactor0;
var
  s: TAutoSet;
begin
  s.Instance := TSet.Create(0.0);
  AssertTrue(s.Instance.Count = 0);
  AssertTrue(s.Instance.LoadFactor = TSet.MinLoadFactor);
end;

procedure THashSetQPTest.CreateLoadFactor3;
var
  s: TAutoSet;
begin
  s.Instance := TSet.Create(3.0);
  AssertTrue(s.Instance.Count = 0);
  AssertTrue(s.Instance.LoadFactor = TSet.MaxLoadFactor);
end;

procedure THashSetQPTest.CreateLoadFactor;
var
  s: TAutoSet;
  lf: Single;
begin
  lf := 0.5;
  s.Instance := TSet.Create(lf);
  AssertTrue(s.Instance.Count = 0);
  AssertTrue(s.Instance.LoadFactor = lf);
end;

procedure THashSetQPTest.CreateLoadFactor075Array11;
var
  s: TAutoSet;
  I: Integer;
  lf: Single;
begin
  lf := 0.75;
  s.Instance := TSet.Create(lf, IntArray11);
  AssertTrue(s.Instance.Count = 11);
  AssertTrue(s.Instance.LoadFactor = lf);
  for I in IntArray11 do
    AssertTrue(s.Instance.Contains(I));
end;

procedure THashSetQPTest.CreateLoadFactor075Enum11;
var
  s: TAutoSet;
  I: Integer;
  e: IIntEnumerable;
  lf: Single;
begin
  lf := 0.75;
  e := TIntArrayCursor.Create(TIntHelper.CreateCopy(IntArray11));
  s.Instance := TSet.Create(lf, e);
  AssertTrue(s.Instance.Count = 11);
  AssertTrue(s.Instance.LoadFactor = lf);
  for I in IntArray11 do
    AssertTrue(s.Instance.Contains(I));
end;

procedure THashSetQPTest.CreateCapacityLoadFactor;
var
  s: TAutoSet;
  lf: Single;
begin
  lf := 0.45;
  s.Instance := TSet.Create(50);
  AssertTrue(s.Instance.Count = 0);
  s.Instance.LoadFactor := lf;
  AssertTrue(s.Instance.Capacity >= 50);
  AssertTrue(s.Instance.LoadFactor = lf);
end;

procedure THashSetQPTest.CreateCapacityLoadFactor075Array11;
var
  s: TAutoSet;
  I: Integer;
  lf: Single;
begin
  lf := 0.75;
  s.Instance := TSet.Create(50, lf, IntArray11);
  AssertTrue(s.Instance.Count = 11);
  AssertTrue(s.Instance.Capacity >= 50);
  AssertTrue(s.Instance.LoadFactor = lf);
  for I in IntArray11 do
    AssertTrue(s.Instance.Contains(I));
end;

procedure THashSetQPTest.CreateCapacityLoadFactor075Enum11;
var
  s: TAutoSet;
  I: Integer;
  e: IIntEnumerable;
  lf: Single;
begin
  lf := 0.75;
  e := TIntArrayCursor.Create(TIntHelper.CreateCopy(IntArray11));
  s.Instance := TSet.Create(50, lf, e);
  AssertTrue(s.Instance.Count = 11);
  AssertTrue(s.Instance.Capacity >= 50);
  AssertTrue(s.Instance.LoadFactor = lf);
  for I in IntArray11 do
    AssertTrue(s.Instance.Contains(I));
end;

procedure THashSetQPTest.Clear;
var
  s: TAutoSet;
begin
  s.Instance := TSet.Create(IntArray11);
  AssertTrue(s.Instance.Count = 11);
  AssertTrue(s.Instance.Capacity > 0);
  s.Instance.Clear;
  AssertTrue(s.Instance.Count = 0);
  AssertTrue(s.Instance.Capacity = 0);
end;

procedure THashSetQPTest.EnsureCapacity;
var
  s: TAutoSet;
  c: SizeInt;
begin
  c := s.Instance.ExpandTreshold;
  s.Instance.EnsureCapacity(c + 1);
  AssertTrue(s.Instance.Capacity > c);
end;

procedure THashSetQPTest.TrimToFit;
var
  s: TAutoSet;
  c: SizeInt;
begin
  s.Instance := TSet.Create(0.85, IntArray11);
  c := s.Instance.Capacity;
  s.Instance.TrimToFit;
  AssertTrue(s.Instance.Capacity >= 11);
  AssertTrue(s.Instance.Capacity < c);
end;

procedure THashSetQPTest.Add;
var
  s: TAutoSet;
begin
  AssertTrue(s.Instance.Count = 0);
  AssertTrue(s.Instance.Add(1));
  AssertTrue(s.Instance.Count = 1);
  AssertTrue(s.Instance.Contains(1));
  AssertTrue(s.Instance.Add(51));
  AssertTrue(s.Instance.Count = 2);
  AssertTrue(s.Instance.Contains(51));
  AssertFalse(s.Instance.Add(1));
  AssertFalse(s.Instance.Add(51));
  AssertTrue(s.Instance.Count = 2);
  AssertTrue(s.Instance.Add(52));
  AssertTrue(s.Instance.Count = 3);
  AssertTrue(s.Instance.Contains(52));
end;

procedure THashSetQPTest.Add100;
var
  s: TAutoSet;
  I: Integer;
begin
  for I := 1 to 100 do
    AssertTrue(s.Instance.Add(I));
  AssertTrue(s.Instance.Count = 100);
  for I := 1 to 100 do
    AssertFalse(s.Instance.Add(I));
  AssertTrue(s.Instance.Count = 100);
  for I := 1 to 100 do
    AssertTrue(s.Instance.Contains(I));
end;

procedure THashSetQPTest.AddArray;
var
  s: TAutoSet;
begin
  AssertTrue(s.Instance.AddAll(IntArray21) = System.Length(IntArray21));
  AssertTrue(s.Instance.Count = System.Length(IntArray21));
  AssertTrue(s.Instance.ContainsAll(IntArray21));
end;

procedure THashSetQPTest.AddEnum;
var
  s: TAutoSet;
  e: IIntEnumerable;
begin
  e := TIntArrayCursor.Create(TIntHelper.CreateCopy(IntArray21));
  AssertTrue(s.Instance.AddAll(e) = System.Length(IntArray21));
  AssertTrue(s.Instance.Count = System.Length(IntArray21));
  AssertTrue(s.Instance.ContainsAll(IntArray21));
end;

procedure THashSetQPTest.AddSelf;
var
  s: TAutoSet;
begin
  s.Instance.AddAll(IntArray21);
  AssertTrue(s.Instance.AddAll(s.Instance) = 0);
  AssertTrue(s.Instance.Count = System.Length(IntArray21));
end;

procedure THashSetQPTest.Remove;
var
  s: TAutoSet;
begin
  s.Instance := TSet.Create(IntArray21);
  AssertTrue(s.Instance.Count = System.Length(IntArray21));
  AssertTrue(s.Instance.Remove(IntArray21[1]));
  AssertTrue(s.Instance.Count = System.Length(IntArray21) - 1);
  AssertTrue(s.Instance.Remove(IntArray21[System.High(IntArray21)]));
  AssertTrue(s.Instance.Count = System.Length(IntArray21) - 2);
  AssertFalse(s.Instance.Remove(-1));
  AssertTrue(s.Instance.Count = System.Length(IntArray21) - 2);
  AssertFalse(s.Instance.Remove(100));
  AssertTrue(s.Instance.Count = System.Length(IntArray21) - 2);
end;

procedure THashSetQPTest.RemoveArray;
var
  s: TAutoSet;
begin
  s.Instance := TSet.Create(IntArray21);
  AssertTrue(s.Instance.RemoveAll(IntArray11) = 11);
  AssertTrue(s.Instance.Count = 10);
end;

procedure THashSetQPTest.RemoveEnum;
var
  s: TAutoSet;
  e: IIntEnumerable;
begin
  e := TIntArrayCursor.Create(TIntHelper.CreateCopy(IntArray11));
  s.Instance := TSet.Create(IntArray21);
  AssertTrue(s.Instance.RemoveAll(e) = 11);
  AssertTrue(s.Instance.Count = 10);
end;

procedure THashSetQPTest.RemoveSelf;
var
  s: TAutoSet;
begin
  s.Instance := TSet.Create(IntArray21);
  AssertTrue(s.Instance.RemoveAll(s.Instance) = 21);
  AssertTrue(s.Instance.IsEmpty);
end;

procedure THashSetQPTest.Tombstones;
var
  s: TAutoSet;
  c: SizeInt;
begin
  s.Instance := TSet.Create(IntArray21);
  c := s.Instance.Capacity;
  AssertTrue(s.Instance.TombstonesCount = 0);
  AssertTrue(s.Instance.Remove(IntArray21[1]));
  AssertTrue(s.Instance.TombstonesCount = 1);
  AssertTrue(s.Instance.Remove(IntArray21[2]));
  AssertTrue(s.Instance.TombstonesCount = 2);
  AssertTrue(s.Instance.Add(IntArray21[1]));
  AssertTrue(s.Instance.TombstonesCount = 1);
  AssertTrue(s.Instance.Add(IntArray21[2]));
  AssertTrue(s.Instance.TombstonesCount = 0);
  AssertTrue(s.Instance.RemoveAll(IntArray21) = 21);
  AssertTrue(s.Instance.TombstonesCount = 21);
  s.Instance.ClearTombstones;
  AssertTrue(s.Instance.TombstonesCount = 0);
  AssertTrue(s.Instance.Capacity = c);
end;

procedure THashSetQPTest.RemoveIfRegular;
var
  s: TAutoSet;
begin
  s.Instance := TSet.Create(IntArray21);
  AssertTrue(s.Instance.RemoveIf(@IsEvenInt) = 10);
  AssertTrue(s.Instance.Count = 11);
  AssertTrue(s.Instance.ContainsAll(IntArray11));
end;

procedure THashSetQPTest.RemoveIfDelegated;
var
  s: TAutoSet;
begin
  s.Instance := TSet.Create(IntArray21);
  AssertTrue(s.Instance.RemoveIf(@IsEven) = 10);
  AssertTrue(s.Instance.Count = 11);
  AssertTrue(s.Instance.ContainsAll(IntArray11));
end;

procedure THashSetQPTest.RemoveIfNested;
  function IsEvenInt(constref aValue: Integer): Boolean;
  begin
    Result := not Odd(aValue);
  end;
var
  s: TAutoSet;
begin
  s.Instance := TSet.Create(IntArray21);
  AssertTrue(s.Instance.RemoveIf(@IsEvenInt) = 10);
  AssertTrue(s.Instance.Count = 11);
  AssertTrue(s.Instance.ContainsAll(IntArray11));
end;

procedure THashSetQPTest.ExtractIfRegular;
var
  s: TAutoSet;
  e: TIntArray;
begin
  s.Instance := TSet.Create(IntArray21);
  e := s.Instance.ExtractIf(@IsEvenInt);
  AssertTrue(s.Instance.Count = 11);
  AssertTrue(s.Instance.ContainsAll(IntArray11));
  AssertTrue(System.Length(e) = 10);
  TIntHelper.QuickSort(e);
  AssertTrue(TIntHelper.Same(IntArray10, e));
end;

procedure THashSetQPTest.ExtractIfDelegated;
var
  s: TAutoSet;
  e: TIntArray;
begin
  s.Instance := TSet.Create(IntArray21);
  e := s.Instance.ExtractIf(@IsEven);
  AssertTrue(s.Instance.Count = 11);
  AssertTrue(s.Instance.ContainsAll(IntArray11));
  AssertTrue(System.Length(e) = 10);
  TIntHelper.QuickSort(e);
  AssertTrue(TIntHelper.Same(IntArray10, e));
end;

procedure THashSetQPTest.ExtractIfNested;
  function IsEvenInt(constref aValue: Integer): Boolean;
  begin
    Result := not Odd(aValue);
  end;
var
  s: TAutoSet;
  e: TIntArray;
begin
  s.Instance := TSet.Create(IntArray21);
  e := s.Instance.ExtractIf(@IsEvenInt);
  AssertTrue(s.Instance.Count = 11);
  AssertTrue(s.Instance.ContainsAll(IntArray11));
  AssertTrue(System.Length(e) = 10);
  TIntHelper.QuickSort(e);
  AssertTrue(TIntHelper.Same(IntArray10, e));
end;

procedure THashSetQPTest.Retain;
var
  s, s1: TAutoSet;
begin
  s.Instance := TSet.Create(IntArray21);
  s1.Instance := TSet.Create(IntArray11);
  s.Instance.AddAll([-3, -2, -1, 0]);
  AssertTrue(s.Instance.Count = 25);
  s.Instance.RetainAll(s1.Instance);
  AssertTrue(s.Instance.Count = 11);
  AssertTrue(s.Instance.ContainsAll(s1.Instance));
end;

procedure THashSetQPTest.Retain_1;
var
  s, s1: TAutoSet;
begin
  s.Instance := TSet.Create(IntArray10);
  s1.Instance := TSet.Create(IntArray11);
  s.Instance.RetainAll(s1.Instance);
  AssertTrue(s.Instance.IsEmpty);
end;

procedure THashSetQPTest.RetainSelf;
var
  s: TAutoSet;
begin
  s.Instance := TSet.Create(IntArray21);
  AssertTrue(s.Instance.Count = 21);
  s.Instance.RetainAll(s.Instance);
  AssertTrue(s.Instance.Count = 21);
  AssertTrue(s.Instance.ContainsAll(IntArray21));
end;

procedure THashSetQPTest.IsSuperset;
var
  s, s1: TAutoSet;
begin
  s.Instance := TSet.Create(IntArray21);
  s1.Instance := TSet.Create(IntArray21);
  AssertTrue(s.Instance.IsSuperset(s1.Instance));
  AssertTrue(s1.Instance.IsSuperset(s.Instance));
  AssertTrue(s.Instance.IsSuperset(s.Instance));
  s1.Instance := TSet.Create(IntArray11);
  AssertTrue(s.Instance.IsSuperset(s1.Instance));
  AssertFalse(s1.Instance.IsSuperset(s.Instance));
end;

procedure THashSetQPTest.IsSubset;
var
  s, s1: TAutoSet;
begin
  s.Instance := TSet.Create(IntArray21);
  s1.Instance := TSet.Create(IntArray21);
  AssertTrue(s.Instance.IsSubset(s1.Instance));
  AssertTrue(s1.Instance.IsSubset(s.Instance));
  AssertTrue(s.Instance.IsSubset(s.Instance));
  s1.Instance := TSet.Create(IntArray11);
  AssertTrue(s1.Instance.IsSubset(s.Instance));
  AssertFalse(s.Instance.IsSubset(s1.Instance));
end;

procedure THashSetQPTest.IsEqual;
var
  s, s1: TAutoSet;
begin
  s.Instance := TSet.Create(IntArray21);
  s1.Instance := TSet.Create(IntArray21);
  AssertTrue(s.Instance.IsEqual(s.Instance));
  AssertTrue(s.Instance.IsEqual(s1.Instance));
  s1.Instance := TSet.Create(IntArray11);
  AssertFalse(s.Instance.IsEqual(s1.Instance));
end;

procedure THashSetQPTest.Intersecting;
var
  s, s1: TAutoSet;
begin
  s.Instance := TSet.Create(IntArray21);
  s1.Instance := TSet.Create(IntArray11);
  AssertTrue(s.Instance.Intersecting(s.Instance));
  AssertTrue(s.Instance.Intersecting(s1.Instance));
  s.Instance := TSet.Create(IntArray10);
  AssertFalse(s.Instance.Intersecting(s1.Instance));
  AssertFalse(s1.Instance.Intersecting(s.Instance));
end;

procedure THashSetQPTest.SymmetricSubtract;
var
  s, s1: TAutoSet;
begin
  s.Instance := TSet.Create(IntArray21);
  s1.Instance := TSet.Create(IntArray21);
  s.Instance.SymmetricSubtract(s1.Instance);
  AssertTrue(s.Instance.IsEmpty);
  s.Instance := TSet.Create(IntArray21);
  s1.Instance.Clear;
  s.Instance.SymmetricSubtract(s1.Instance);
  AssertTrue(s.Instance.Count = 21);
  AssertTrue(s.Instance.ContainsAll(IntArray21));
  s.Instance := TSet.Create(IntArray11);
  s1.Instance := TSet.Create(IntArray10);
  s.Instance.SymmetricSubtract(s1.Instance);
  AssertTrue(s.Instance.Count = 21);
  AssertTrue(s.Instance.ContainsAll(IntArray21));
  s.Instance := TSet.Create(IntArray11);
  s1.Instance := TSet.Create(IntArray10);
  s.Instance.AddAll([-5, -4, -3, -2, -1, 0]);
  s1.Instance.AddAll([-5, -4, -3, -2, -1, 0]);
  s.Instance.SymmetricSubtract(s1.Instance);
  AssertTrue(s.Instance.Count = 21);
  AssertTrue(s.Instance.ContainsAll(IntArray21));
end;

procedure THashSetQPTest.ObjectSetRemove;
var
  s: TAutoObjSet;
  a: TObjArray;
  I, TestSize: Integer;
  Counter: TCounter;
begin
  TestSize := 36;
  Counter := Default(TCounter);
  System.SetLength(a, TestSize);
  for I := 0 to System.High(a) do
    begin
      a[I] := TTestObj.Create(Counter.Increment);
      s.Instance.Add(a[I]);
    end;
  AssertTrue(s.Instance.Count = TestSize);
  for I := 0 to System.High(a) do
    begin
      AssertTrue(s.Instance.Remove(a[I]));
      AssertTrue(s.Instance.Count = Pred(TestSize - I));
      AssertTrue(Counter.Count = Succ(I));
    end;
  AssertTrue(s.Instance.IsEmpty);
end;

procedure THashSetQPTest.ObjectSetRemoveArray;
var
  s: TAutoObjSet;
  a: TObjArray;
  I, TestSize: Integer;
  Counter: TCounter;
begin
  TestSize := 56;
  Counter := Default(TCounter);
  for I := 1 to TestSize do
    s.Instance.Add(TTestObj.Create(Counter.Increment));
  AssertTrue(s.Instance.Count = TestSize);
  a := s.Instance.ToArray;
  AssertTrue(System.Length(a) = TestSize);
  AssertTrue(s.Instance.RemoveAll(a) = TestSize);
  AssertTrue(s.Instance.IsEmpty);
  AssertTrue(Counter.Count = TestSize);
end;

procedure THashSetQPTest.ObjectSetRemoveEnum;
type
  TFakeEnum = specialize TGArrayCursor<TTestObj>;
var
  s: TAutoObjSet;
  e: TFakeEnum;
  I, TestSize: Integer;
  Counter: TCounter;
begin
  TestSize := 46;
  Counter := Default(TCounter);
  for I := 1 to TestSize do
    s.Instance.Add(TTestObj.Create(Counter.Increment));
  AssertTrue(s.Instance.Count = TestSize);
  e := TFakeEnum.Create(s.Instance.ToArray);
  AssertTrue(s.Instance.RemoveAll(e) = TestSize);
  AssertTrue(s.Instance.IsEmpty);
  AssertTrue(Counter.Count = TestSize);
end;

procedure THashSetQPTest.ObjectSetRemoveSelf;
var
  s: TAutoObjSet;
  I, TestSize: Integer;
  Counter: TCounter;
begin
  TestSize := 100;
  Counter := Default(TCounter);
  for I := 1 to TestSize do
    s.Instance.Add(TTestObj.Create(Counter.Increment));
  AssertTrue(s.Instance.Count = TestSize);
  AssertTrue(s.Instance.RemoveAll(s.Instance) = TestSize);
  AssertTrue(s.Instance.IsEmpty);
  AssertTrue(Counter.Count = TestSize);
end;

procedure THashSetQPTest.ObjectSetRemoveIfRegular;
var
  s: TAutoObjSet;
  I, TestSize: Integer;
  Counter: TCounter;
  o: TTestObj;
begin
  TestSize := 100;
  Counter := Default(TCounter);
  for I := 1 to TestSize do
    s.Instance.Add(TTestObj.Create(I, Counter.Increment));
  AssertTrue(s.Instance.Count = TestSize);
  AssertTrue(s.Instance.RemoveIf(@IsEvenObj) = TestSize div 2);
  AssertTrue(Counter.Count = TestSize div 2);
  AssertTrue(s.Instance.Count = TestSize div 2);
  for o in s.Instance do
    AssertTrue(Odd(o.Value));
  s.Instance := Default(TObjSet);
  AssertTrue(Counter.Count = TestSize);
end;

procedure THashSetQPTest.ObjectSetRemoveIfDelegated;
var
  s: TAutoObjSet;
  I, TestSize: Integer;
  Counter: TCounter;
  o: TTestObj;
begin
  TestSize := 100;
  Counter := Default(TCounter);
  for I := 1 to TestSize do
    s.Instance.Add(TTestObj.Create(I, Counter.Increment));
  AssertTrue(s.Instance.Count = TestSize);
  AssertTrue(s.Instance.RemoveIf(@ObjIsEven) = TestSize div 2);
  AssertTrue(Counter.Count = TestSize div 2);
  AssertTrue(s.Instance.Count = TestSize div 2);
  for o in s.Instance do
    AssertTrue(Odd(o.Value));
  s.Instance := Default(TObjSet);
  AssertTrue(Counter.Count = TestSize);
end;

procedure THashSetQPTest.ObjectSetRemoveIfNested;
  function IsEvenNest(constref aObj: TTestObj): Boolean;
  begin
    Result := not Odd(aObj.Value);
  end;
var
  s: TAutoObjSet;
  I, TestSize: Integer;
  Counter: TCounter;
  o: TTestObj;
begin
  TestSize := 100;
  Counter := Default(TCounter);
  for I := 1 to TestSize do
    s.Instance.Add(TTestObj.Create(I, Counter.Increment));
  AssertTrue(s.Instance.Count = TestSize);
  AssertTrue(s.Instance.RemoveIf(@IsEvenNest) = TestSize div 2);
  AssertTrue(Counter.Count = TestSize div 2);
  AssertTrue(s.Instance.Count = TestSize div 2);
  for o in s.Instance do
    AssertTrue(Odd(o.Value));
  s.Instance := Default(TObjSet);
  AssertTrue(Counter.Count = TestSize);
end;

procedure THashSetQPTest.ObjectSetExtractIfRegular;
var
  s: TAutoObjSet;
  I, TestSize: Integer;
  Counter: TCounter;
  a: TObjArray;
  o: TTestObj;
begin
  TestSize := 100;
  Counter := Default(TCounter);
  for I := 1 to TestSize do
    s.Instance.Add(TTestObj.Create(I, Counter.Increment));
  AssertTrue(s.Instance.Count = TestSize);
  a := s.Instance.ExtractIf(@IsEvenObj);
  AssertTrue(System.Length(a) = TestSize div 2);
  AssertTrue(Counter.Count = 0);
  AssertTrue(s.Instance.Count = TestSize div 2);
  for o in a do
    begin
      AssertTrue(not Odd(o.Value));
      o.Free;
    end;
  for o in s.Instance do
    AssertTrue(Odd(o.Value));
  s.Instance := Default(TObjSet);
  AssertTrue(Counter.Count = TestSize);
end;

procedure THashSetQPTest.ObjectSetExtractIfDelegated;
var
  s: TAutoObjSet;
  I, TestSize: Integer;
  Counter: TCounter;
  a: TObjArray;
  o: TTestObj;
begin
  TestSize := 100;
  Counter := Default(TCounter);
  for I := 1 to TestSize do
    s.Instance.Add(TTestObj.Create(I, Counter.Increment));
  AssertTrue(s.Instance.Count = TestSize);
  a := s.Instance.ExtractIf(@ObjIsEven);
  AssertTrue(System.Length(a) = TestSize div 2);
  AssertTrue(Counter.Count = 0);
  AssertTrue(s.Instance.Count = TestSize div 2);
  for o in a do
    begin
      AssertTrue(not Odd(o.Value));
      o.Free;
    end;
  for o in s.Instance do
    AssertTrue(Odd(o.Value));
  s.Instance := Default(TObjSet);
  AssertTrue(Counter.Count = TestSize);
end;

procedure THashSetQPTest.ObjectSetExtractIfNested;
  function IsEvenNest(constref aObj: TTestObj): Boolean;
  begin
    Result := not Odd(aObj.Value);
  end;
var
  s: TAutoObjSet;
  I, TestSize: Integer;
  Counter: TCounter;
  a: TObjArray;
  o: TTestObj;
begin
  TestSize := 100;
  Counter := Default(TCounter);
  for I := 1 to TestSize do
    s.Instance.Add(TTestObj.Create(I, Counter.Increment));
  AssertTrue(s.Instance.Count = TestSize);
  a := s.Instance.ExtractIf(@IsEvenNest);
  AssertTrue(System.Length(a) = TestSize div 2);
  AssertTrue(Counter.Count = 0);
  AssertTrue(s.Instance.Count = TestSize div 2);
  for o in a do
    begin
      AssertTrue(not Odd(o.Value));
      o.Free;
    end;
  for o in s.Instance do
    AssertTrue(Odd(o.Value));
  s.Instance := Default(TObjSet);
  AssertTrue(Counter.Count = TestSize);
end;

procedure THashSetQPTest.ObjectSetRetain;
var
  s, s1: TAutoObjSet;
  I, TestSize: Integer;
  Counter: TCounter;
  a: TObjArray;
begin
  TestSize := 100;
  Counter := Default(TCounter);
  System.SetLength(a, TestSize div 2);
  s1.Instance := TObjSet.Create(False);

  for I := 0 to System.High(a) do
    a[I] := TTestObj.Create(Counter.Increment);
  s1.Instance.AddAll(a);
  s.Instance.AddAll(a);
  for I := 1 to TestSize div 2 do
    s.Instance.Add(TTestObj.Create(Counter.Increment));
  AssertTrue(s.Instance.Count = TestSize);
  s.Instance.RetainAll(s1.Instance);
  AssertTrue(s.Instance.Count = TestSize div 2);
  AssertTrue(Counter.Count = TestSize div 2);
  AssertTrue(s.Instance.ContainsAll(s1.Instance));
  s.Instance := Default(TObjSet);
  AssertTrue(Counter.Count = TestSize);
end;

procedure THashSetQPTest.ObjectSetSymmetricSubtract;
var
  s, s1: TAutoObjSet;
  I, TestSize: Integer;
  Counter: TCounter;
  a: TObjArray;
begin
  TestSize := 100;
  Counter := Default(TCounter);
  for I := 1 to TestSize do
    s.Instance.Add(TTestObj.Create(Counter.Increment));
  for I := 1 to TestSize do
    s1.Instance.Add(TTestObj.Create(Counter.Increment));
  System.SetLength(a, TestSize);
  for I := 0 to System.High(a) do
    a[I] := TTestObj.Create(Counter.Increment);
  s.Instance.AddAll(a);
  AssertTrue(s.Instance.Count = TestSize * 2);
  s1.Instance.AddAll(a);
  AssertTrue(s1.Instance.Count = TestSize * 2);
  s1.Instance.OwnsObjects := False; //////////
  s.Instance.SymmetricSubtract(s1.Instance);
  AssertTrue(s.Instance.Count = TestSize * 2);
  AssertTrue(Counter.Count = TestSize);
  AssertTrue(s1.Instance.Count = TestSize * 2);
  s.Instance := Default(TObjSet);
  AssertTrue(Counter.Count = TestSize * 3);
end;

procedure TChainHashSetTest.TestCreate;
var
  s: TAutoSet;
begin
  AssertTrue(s.Instance.Count = 0);
  AssertTrue(s.Instance.Capacity > 0);
  AssertTrue(s.Instance.LoadFactor = s.Instance.DefaultLoadFactor);
end;

procedure TChainHashSetTest.CreateArray;
var
  s: TAutoSet;
  I: Integer;
begin
  s.Instance := TSet.Create(IntArray11);
  AssertTrue(s.Instance.Count = 11);
  AssertTrue(s.Instance.LoadFactor = s.Instance.DefaultLoadFactor);
  for I in IntArray11 do
    AssertTrue(s.Instance.Contains(I));
end;

procedure TChainHashSetTest.CreateEnum;
var
  s: TAutoSet;
  I: Integer;
  e: IIntEnumerable;
begin
  e := TIntArrayCursor.Create(TIntHelper.CreateCopy(IntArray11));
  s.Instance := TSet.Create(e);
  AssertTrue(s.Instance.Count = 11);
  AssertTrue(s.Instance.LoadFactor = s.Instance.DefaultLoadFactor);
  for I in IntArray11 do
    AssertTrue(s.Instance.Contains(I));
end;

procedure TChainHashSetTest.CreateCapacity0;
var
  s: TAutoSet;
begin
  s.Instance := TSet.Create(0);
  AssertTrue(s.Instance.Count = 0);
  AssertTrue(s.Instance.Capacity > 0);
  AssertTrue(s.Instance.LoadFactor = s.Instance.DefaultLoadFactor);
end;

procedure TChainHashSetTest.CreateCapacity5;
var
  s: TAutoSet;
begin
  s.Instance := TSet.Create(5);
  AssertTrue(s.Instance.Count = 0);
  AssertTrue(s.Instance.Capacity >= 5);
  AssertTrue(s.Instance.LoadFactor = s.Instance.DefaultLoadFactor);
end;

procedure TChainHashSetTest.CreateCapacity50Array11;
var
  s: TAutoSet;
  I: Integer;
begin
  s.Instance := TSet.Create(50, IntArray11);
  AssertTrue(s.Instance.Count = 11);
  AssertTrue(s.Instance.Capacity >= 50);
  AssertTrue(s.Instance.LoadFactor = s.Instance.DefaultLoadFactor);
  for I in IntArray11 do
    AssertTrue(s.Instance.Contains(I));
end;

procedure TChainHashSetTest.CreateCapacity50Enum11;
var
  s: TAutoSet;
  I: Integer;
  e: IIntEnumerable;
begin
  e := TIntArrayCursor.Create(TIntHelper.CreateCopy(IntArray11));
  s.Instance := TSet.Create(50, e);
  AssertTrue(s.Instance.Count = 11);
  AssertTrue(s.Instance.Capacity >= 50);
  AssertTrue(s.Instance.LoadFactor = s.Instance.DefaultLoadFactor);
  for I in IntArray11 do
    AssertTrue(s.Instance.Contains(I));
end;

procedure TChainHashSetTest.CreateLoadFactor0;
var
  s: TAutoSet;
begin
  s.Instance := TSet.Create(0.0);
  AssertTrue(s.Instance.Count = 0);
  AssertTrue(s.Instance.LoadFactor = TSet.MinLoadFactor);
end;

procedure TChainHashSetTest.CreateLoadFactor10;
var
  s: TAutoSet;
begin
  s.Instance := TSet.Create(10.0);
  AssertTrue(s.Instance.Count = 0);
  AssertTrue(s.Instance.LoadFactor = TSet.MaxLoadFactor);
end;

procedure TChainHashSetTest.CreateLoadFactor;
var
  s: TAutoSet;
  lf: Single;
begin
  lf := 0.5;
  s.Instance := TSet.Create(lf);
  AssertTrue(s.Instance.Count = 0);
  AssertTrue(s.Instance.LoadFactor = lf);
end;

procedure TChainHashSetTest.CreateLoadFactor3Array11;
var
  s: TAutoSet;
  I: Integer;
  lf: Single;
begin
  lf := 3.0;
  s.Instance := TSet.Create(lf, IntArray11);
  AssertTrue(s.Instance.Count = 11);
  AssertTrue(s.Instance.LoadFactor = lf);
  for I in IntArray11 do
    AssertTrue(s.Instance.Contains(I));
end;

procedure TChainHashSetTest.CreateLoadFactor3Enum11;
var
  s: TAutoSet;
  I: Integer;
  e: IIntEnumerable;
  lf: Single;
begin
  lf := 3.0;
  e := TIntArrayCursor.Create(TIntHelper.CreateCopy(IntArray11));
  s.Instance := TSet.Create(lf, e);
  AssertTrue(s.Instance.Count = 11);
  AssertTrue(s.Instance.LoadFactor = lf);
  for I in IntArray11 do
    AssertTrue(s.Instance.Contains(I));
end;

procedure TChainHashSetTest.CreateCapacityLoadFactor;
var
  s: TAutoSet;
  lf: Single;
begin
  lf := 0.45;
  s.Instance := TSet.Create(50);
  AssertTrue(s.Instance.Count = 0);
  s.Instance.LoadFactor := lf;
  AssertTrue(s.Instance.Capacity >= 50);
  AssertTrue(s.Instance.LoadFactor = lf);
end;

procedure TChainHashSetTest.CreateCapacityLoadFactor3Array11;
var
  s: TAutoSet;
  I: Integer;
  lf: Single;
begin
  lf := 3.0;
  s.Instance := TSet.Create(50, lf, IntArray11);
  AssertTrue(s.Instance.Count = 11);
  AssertTrue(s.Instance.Capacity.ToString, s.Instance.Capacity = 32);
  AssertTrue(s.Instance.LoadFactor = lf);
  for I in IntArray11 do
    AssertTrue(s.Instance.Contains(I));
end;

procedure TChainHashSetTest.CreateCapacityLoadFactor3Enum11;
var
  s: TAutoSet;
  I: Integer;
  e: IIntEnumerable;
  lf: Single;
begin
  lf := 3.0;
  e := TIntArrayCursor.Create(TIntHelper.CreateCopy(IntArray11));
  s.Instance := TSet.Create(50, lf, e);
  AssertTrue(s.Instance.Count = 11);
  AssertTrue(s.Instance.Capacity = 32);
  AssertTrue(s.Instance.LoadFactor = lf);
  for I in IntArray11 do
    AssertTrue(s.Instance.Contains(I));
end;

procedure TChainHashSetTest.Clear;
var
  s: TAutoSet;
begin
  s.Instance := TSet.Create(IntArray11);
  AssertTrue(s.Instance.Count = 11);
  AssertTrue(s.Instance.Capacity > 0);
  s.Instance.Clear;
  AssertTrue(s.Instance.Count = 0);
  AssertTrue(s.Instance.Capacity = 0);
end;

procedure TChainHashSetTest.EnsureCapacity;
var
  s: TAutoSet;
  c: SizeInt;
begin
  c := s.Instance.ExpandTreshold;
  s.Instance.EnsureCapacity(c + 1);
  AssertTrue(s.Instance.Capacity > c);
end;

procedure TChainHashSetTest.TrimToFit;
var
  s: TAutoSet;
  c: SizeInt;
begin
  s.Instance := TSet.Create(IntArray11);
  c := s.Instance.Capacity;
  s.Instance.TrimToFit;
  AssertTrue(s.Instance.Capacity < c);
end;

procedure TChainHashSetTest.Add;
var
  s: TAutoSet;
begin
  AssertTrue(s.Instance.Count = 0);
  AssertTrue(s.Instance.Add(1));
  AssertTrue(s.Instance.Count = 1);
  AssertTrue(s.Instance.Contains(1));
  AssertTrue(s.Instance.Add(51));
  AssertTrue(s.Instance.Count = 2);
  AssertTrue(s.Instance.Contains(51));
  AssertFalse(s.Instance.Add(1));
  AssertFalse(s.Instance.Add(51));
  AssertTrue(s.Instance.Count = 2);
  AssertTrue(s.Instance.Add(52));
  AssertTrue(s.Instance.Count = 3);
  AssertTrue(s.Instance.Contains(52));
end;

procedure TChainHashSetTest.Add100;
var
  s: TAutoSet;
  I: Integer;
begin
  for I := 1 to 100 do
    AssertTrue(s.Instance.Add(I));
  AssertTrue(s.Instance.Count = 100);
  for I := 1 to 100 do
    AssertFalse(s.Instance.Add(I));
  AssertTrue(s.Instance.Count = 100);
  for I := 1 to 100 do
    AssertTrue(s.Instance.Contains(I));
end;

procedure TChainHashSetTest.AddArray;
var
  s: TAutoSet;
begin
  AssertTrue(s.Instance.AddAll(IntArray21) = System.Length(IntArray21));
  AssertTrue(s.Instance.Count = System.Length(IntArray21));
  AssertTrue(s.Instance.ContainsAll(IntArray21));
end;

procedure TChainHashSetTest.AddEnum;
var
  s: TAutoSet;
  e: IIntEnumerable;
begin
  e := TIntArrayCursor.Create(TIntHelper.CreateCopy(IntArray21));
  AssertTrue(s.Instance.AddAll(e) = System.Length(IntArray21));
  AssertTrue(s.Instance.Count = System.Length(IntArray21));
  AssertTrue(s.Instance.ContainsAll(IntArray21));
end;

procedure TChainHashSetTest.AddSelf;
var
  s: TAutoSet;
begin
  s.Instance.AddAll(IntArray21);
  AssertTrue(s.Instance.AddAll(s.Instance) = 0);
  AssertTrue(s.Instance.Count = System.Length(IntArray21));
end;

procedure TChainHashSetTest.Remove;
var
  s: TAutoSet;
begin
  s.Instance := TSet.Create(IntArray21);
  AssertTrue(s.Instance.Count = System.Length(IntArray21));
  AssertTrue(s.Instance.Remove(IntArray21[1]));
  AssertTrue(s.Instance.Count = System.Length(IntArray21) - 1);
  AssertTrue(s.Instance.Remove(IntArray21[System.High(IntArray21)]));
  AssertTrue(s.Instance.Count = System.Length(IntArray21) - 2);
  AssertFalse(s.Instance.Remove(-1));
  AssertTrue(s.Instance.Count = System.Length(IntArray21) - 2);
  AssertFalse(s.Instance.Remove(100));
  AssertTrue(s.Instance.Count = System.Length(IntArray21) - 2);
end;

procedure TChainHashSetTest.RemoveArray;
var
  s: TAutoSet;
begin
  s.Instance := TSet.Create(IntArray21);
  AssertTrue(s.Instance.RemoveAll(IntArray11) = 11);
  AssertTrue(s.Instance.Count = 10);
end;

procedure TChainHashSetTest.RemoveEnum;
var
  s: TAutoSet;
  e: IIntEnumerable;
begin
  e := TIntArrayCursor.Create(TIntHelper.CreateCopy(IntArray11));
  s.Instance := TSet.Create(IntArray21);
  AssertTrue(s.Instance.RemoveAll(e) = 11);
  AssertTrue(s.Instance.Count = 10);
end;

procedure TChainHashSetTest.RemoveSelf;
var
  s: TAutoSet;
begin
  s.Instance := TSet.Create(IntArray21);
  AssertTrue(s.Instance.RemoveAll(s.Instance) = 21);
  AssertTrue(s.Instance.IsEmpty);
end;

procedure TChainHashSetTest.RemoveIfRegular;
var
  s: TAutoSet;
begin
  s.Instance := TSet.Create(IntArray21);
  AssertTrue(s.Instance.RemoveIf(@IsEvenInt) = 10);
  AssertTrue(s.Instance.Count = 11);
  AssertTrue(s.Instance.ContainsAll(IntArray11));
end;

procedure TChainHashSetTest.RemoveIfDelegated;
var
  s: TAutoSet;
begin
  s.Instance := TSet.Create(IntArray21);
  AssertTrue(s.Instance.RemoveIf(@IsEven) = 10);
  AssertTrue(s.Instance.Count = 11);
  AssertTrue(s.Instance.ContainsAll(IntArray11));
end;

procedure TChainHashSetTest.RemoveIfNested;
  function IsEvenInt(constref aValue: Integer): Boolean;
  begin
    Result := not Odd(aValue);
  end;
var
  s: TAutoSet;
begin
  s.Instance := TSet.Create(IntArray21);
  AssertTrue(s.Instance.RemoveIf(@IsEvenInt) = 10);
  AssertTrue(s.Instance.Count = 11);
  AssertTrue(s.Instance.ContainsAll(IntArray11));
end;

procedure TChainHashSetTest.ExtractIfRegular;
var
  s: TAutoSet;
  e: TIntArray;
begin
  s.Instance := TSet.Create(IntArray21);
  e := s.Instance.ExtractIf(@IsEvenInt);
  AssertTrue(s.Instance.Count = 11);
  AssertTrue(s.Instance.ContainsAll(IntArray11));
  AssertTrue(System.Length(e) = 10);
  TIntHelper.QuickSort(e);
  AssertTrue(TIntHelper.Same(IntArray10, e));
end;

procedure TChainHashSetTest.ExtractIfDelegated;
var
  s: TAutoSet;
  e: TIntArray;
begin
  s.Instance := TSet.Create(IntArray21);
  e := s.Instance.ExtractIf(@IsEven);
  AssertTrue(s.Instance.Count = 11);
  AssertTrue(s.Instance.ContainsAll(IntArray11));
  AssertTrue(System.Length(e) = 10);
  TIntHelper.QuickSort(e);
  AssertTrue(TIntHelper.Same(IntArray10, e));
end;

procedure TChainHashSetTest.ExtractIfNested;
  function IsEvenInt(constref aValue: Integer): Boolean;
  begin
    Result := not Odd(aValue);
  end;
var
  s: TAutoSet;
  e: TIntArray;
begin
  s.Instance := TSet.Create(IntArray21);
  e := s.Instance.ExtractIf(@IsEvenInt);
  AssertTrue(s.Instance.Count = 11);
  AssertTrue(s.Instance.ContainsAll(IntArray11));
  AssertTrue(System.Length(e) = 10);
  TIntHelper.QuickSort(e);
  AssertTrue(TIntHelper.Same(IntArray10, e));
end;

procedure TChainHashSetTest.Retain;
var
  s, s1: TAutoSet;
begin
  s.Instance := TSet.Create(IntArray21);
  s1.Instance := TSet.Create(IntArray11);
  s.Instance.AddAll([-3, -2, -1, 0]);
  AssertTrue(s.Instance.Count = 25);
  s.Instance.RetainAll(s1.Instance);
  AssertTrue(s.Instance.Count = 11);
  AssertTrue(s.Instance.ContainsAll(s1.Instance));
end;

procedure TChainHashSetTest.Retain_1;
var
  s, s1: TAutoSet;
begin
  s.Instance := TSet.Create(IntArray10);
  s1.Instance := TSet.Create(IntArray11);
  s.Instance.RetainAll(s1.Instance);
  AssertTrue(s.Instance.IsEmpty);
end;

procedure TChainHashSetTest.RetainSelf;
var
  s: TAutoSet;
begin
  s.Instance := TSet.Create(IntArray21);
  AssertTrue(s.Instance.Count = 21);
  s.Instance.RetainAll(s.Instance);
  AssertTrue(s.Instance.Count = 21);
  AssertTrue(s.Instance.ContainsAll(IntArray21));
end;

procedure TChainHashSetTest.IsSuperset;
var
  s, s1: TAutoSet;
begin
  s.Instance := TSet.Create(IntArray21);
  s1.Instance := TSet.Create(IntArray21);
  AssertTrue(s.Instance.IsSuperset(s1.Instance));
  AssertTrue(s1.Instance.IsSuperset(s.Instance));
  AssertTrue(s.Instance.IsSuperset(s.Instance));
  s1.Instance := TSet.Create(IntArray11);
  AssertTrue(s.Instance.IsSuperset(s1.Instance));
  AssertFalse(s1.Instance.IsSuperset(s.Instance));
end;

procedure TChainHashSetTest.IsSubset;
var
  s, s1: TAutoSet;
begin
  s.Instance := TSet.Create(IntArray21);
  s1.Instance := TSet.Create(IntArray21);
  AssertTrue(s.Instance.IsSubset(s1.Instance));
  AssertTrue(s1.Instance.IsSubset(s.Instance));
  AssertTrue(s.Instance.IsSubset(s.Instance));
  s1.Instance := TSet.Create(IntArray11);
  AssertTrue(s1.Instance.IsSubset(s.Instance));
  AssertFalse(s.Instance.IsSubset(s1.Instance));
end;

procedure TChainHashSetTest.IsEqual;
var
  s, s1: TAutoSet;
begin
  s.Instance := TSet.Create(IntArray21);
  s1.Instance := TSet.Create(IntArray21);
  AssertTrue(s.Instance.IsEqual(s.Instance));
  AssertTrue(s.Instance.IsEqual(s1.Instance));
  s1.Instance := TSet.Create(IntArray11);
  AssertFalse(s.Instance.IsEqual(s1.Instance));
end;

procedure TChainHashSetTest.Intersecting;
var
  s, s1: TAutoSet;
begin
  s.Instance := TSet.Create(IntArray21);
  s1.Instance := TSet.Create(IntArray11);
  AssertTrue(s.Instance.Intersecting(s.Instance));
  AssertTrue(s.Instance.Intersecting(s1.Instance));
  s.Instance := TSet.Create(IntArray10);
  AssertFalse(s.Instance.Intersecting(s1.Instance));
  AssertFalse(s1.Instance.Intersecting(s.Instance));
end;

procedure TChainHashSetTest.SymmetricSubtract;
var
  s, s1: TAutoSet;
begin
  s.Instance := TSet.Create(IntArray21);
  s1.Instance := TSet.Create(IntArray21);
  s.Instance.SymmetricSubtract(s1.Instance);
  AssertTrue(s.Instance.IsEmpty);
  s.Instance := TSet.Create(IntArray21);
  s1.Instance.Clear;
  s.Instance.SymmetricSubtract(s1.Instance);
  AssertTrue(s.Instance.Count = 21);
  AssertTrue(s.Instance.ContainsAll(IntArray21));
  s.Instance := TSet.Create(IntArray11);
  s1.Instance := TSet.Create(IntArray10);
  s.Instance.SymmetricSubtract(s1.Instance);
  AssertTrue(s.Instance.Count = 21);
  AssertTrue(s.Instance.ContainsAll(IntArray21));
  s.Instance := TSet.Create(IntArray11);
  s1.Instance := TSet.Create(IntArray10);
  s.Instance.AddAll([-5, -4, -3, -2, -1, 0]);
  s1.Instance.AddAll([-5, -4, -3, -2, -1, 0]);
  s.Instance.SymmetricSubtract(s1.Instance);
  AssertTrue(s.Instance.Count = 21);
  AssertTrue(s.Instance.ContainsAll(IntArray21));
end;

procedure TChainHashSetTest.ObjectSetRemove;
var
  s: TAutoObjSet;
  a: TObjArray;
  I, TestSize: Integer;
  Counter: TCounter;
begin
  TestSize := 36;
  Counter := Default(TCounter);
  System.SetLength(a, TestSize);
  for I := 0 to System.High(a) do
    begin
      a[I] := TTestObj.Create(Counter.Increment);
      s.Instance.Add(a[I]);
    end;
  AssertTrue(s.Instance.Count = TestSize);
  for I := 0 to System.High(a) do
    begin
      AssertTrue(s.Instance.Remove(a[I]));
      AssertTrue(s.Instance.Count = Pred(TestSize - I));
      AssertTrue(Counter.Count = Succ(I));
    end;
  AssertTrue(s.Instance.IsEmpty);
end;

procedure TChainHashSetTest.ObjectSetRemoveArray;
var
  s: TAutoObjSet;
  a: TObjArray;
  I, TestSize: Integer;
  Counter: TCounter;
begin
  TestSize := 56;
  Counter := Default(TCounter);
  for I := 1 to TestSize do
    s.Instance.Add(TTestObj.Create(Counter.Increment));
  AssertTrue(s.Instance.Count = TestSize);
  a := s.Instance.ToArray;
  AssertTrue(System.Length(a) = TestSize);
  AssertTrue(s.Instance.RemoveAll(a) = TestSize);
  AssertTrue(s.Instance.IsEmpty);
  AssertTrue(Counter.Count = TestSize);
end;

procedure TChainHashSetTest.ObjectSetRemoveEnum;
type
  TFakeEnum = specialize TGArrayCursor<TTestObj>;
var
  s: TAutoObjSet;
  e: TFakeEnum;
  I, TestSize: Integer;
  Counter: TCounter;
begin
  TestSize := 46;
  Counter := Default(TCounter);
  for I := 1 to TestSize do
    s.Instance.Add(TTestObj.Create(Counter.Increment));
  AssertTrue(s.Instance.Count = TestSize);
  e := TFakeEnum.Create(s.Instance.ToArray);
  AssertTrue(s.Instance.RemoveAll(e) = TestSize);
  AssertTrue(s.Instance.IsEmpty);
  AssertTrue(Counter.Count = TestSize);
end;

procedure TChainHashSetTest.ObjectSetRemoveSelf;
var
  s: TAutoObjSet;
  I, TestSize: Integer;
  Counter: TCounter;
begin
  TestSize := 100;
  Counter := Default(TCounter);
  for I := 1 to TestSize do
    s.Instance.Add(TTestObj.Create(Counter.Increment));
  AssertTrue(s.Instance.Count = TestSize);
  AssertTrue(s.Instance.RemoveAll(s.Instance) = TestSize);
  AssertTrue(s.Instance.IsEmpty);
  AssertTrue(Counter.Count = TestSize);
end;

procedure TChainHashSetTest.ObjectSetRemoveIfRegular;
var
  s: TAutoObjSet;
  I, TestSize: Integer;
  Counter: TCounter;
  o: TTestObj;
begin
  TestSize := 100;
  Counter := Default(TCounter);
  for I := 1 to TestSize do
    s.Instance.Add(TTestObj.Create(I, Counter.Increment));
  AssertTrue(s.Instance.Count = TestSize);
  AssertTrue(s.Instance.RemoveIf(@IsEvenObj) = TestSize div 2);
  AssertTrue(Counter.Count = TestSize div 2);
  AssertTrue(s.Instance.Count = TestSize div 2);
  for o in s.Instance do
    AssertTrue(Odd(o.Value));
  s.Instance := Default(TObjSet);
  AssertTrue(Counter.Count = TestSize);
end;

procedure TChainHashSetTest.ObjectSetRemoveIfDelegated;
var
  s: TAutoObjSet;
  I, TestSize: Integer;
  Counter: TCounter;
  o: TTestObj;
begin
  TestSize := 100;
  Counter := Default(TCounter);
  for I := 1 to TestSize do
    s.Instance.Add(TTestObj.Create(I, Counter.Increment));
  AssertTrue(s.Instance.Count = TestSize);
  AssertTrue(s.Instance.RemoveIf(@ObjIsEven) = TestSize div 2);
  AssertTrue(Counter.Count = TestSize div 2);
  AssertTrue(s.Instance.Count = TestSize div 2);
  for o in s.Instance do
    AssertTrue(Odd(o.Value));
  s.Instance := Default(TObjSet);
  AssertTrue(Counter.Count = TestSize);
end;

procedure TChainHashSetTest.ObjectSetRemoveIfNested;
  function IsEvenNest(constref aObj: TTestObj): Boolean;
  begin
    Result := not Odd(aObj.Value);
  end;
var
  s: TAutoObjSet;
  I, TestSize: Integer;
  Counter: TCounter;
  o: TTestObj;
begin
  TestSize := 100;
  Counter := Default(TCounter);
  for I := 1 to TestSize do
    s.Instance.Add(TTestObj.Create(I, Counter.Increment));
  AssertTrue(s.Instance.Count = TestSize);
  AssertTrue(s.Instance.RemoveIf(@IsEvenNest) = TestSize div 2);
  AssertTrue(Counter.Count = TestSize div 2);
  AssertTrue(s.Instance.Count = TestSize div 2);
  for o in s.Instance do
    AssertTrue(Odd(o.Value));
  s.Instance := Default(TObjSet);
  AssertTrue(Counter.Count = TestSize);
end;

procedure TChainHashSetTest.ObjectSetExtractIfRegular;
var
  s: TAutoObjSet;
  I, TestSize: Integer;
  Counter: TCounter;
  a: TObjArray;
  o: TTestObj;
begin
  TestSize := 100;
  Counter := Default(TCounter);
  for I := 1 to TestSize do
    s.Instance.Add(TTestObj.Create(I, Counter.Increment));
  AssertTrue(s.Instance.Count = TestSize);
  a := s.Instance.ExtractIf(@IsEvenObj);
  AssertTrue(System.Length(a) = TestSize div 2);
  AssertTrue(Counter.Count = 0);
  AssertTrue(s.Instance.Count = TestSize div 2);
  for o in a do
    begin
      AssertTrue(not Odd(o.Value));
      o.Free;
    end;
  for o in s.Instance do
    AssertTrue(Odd(o.Value));
  s.Instance := Default(TObjSet);
  AssertTrue(Counter.Count = TestSize);
end;

procedure TChainHashSetTest.ObjectSetExtractIfDelegated;
var
  s: TAutoObjSet;
  I, TestSize: Integer;
  Counter: TCounter;
  a: TObjArray;
  o: TTestObj;
begin
  TestSize := 100;
  Counter := Default(TCounter);
  for I := 1 to TestSize do
    s.Instance.Add(TTestObj.Create(I, Counter.Increment));
  AssertTrue(s.Instance.Count = TestSize);
  a := s.Instance.ExtractIf(@ObjIsEven);
  AssertTrue(System.Length(a) = TestSize div 2);
  AssertTrue(Counter.Count = 0);
  AssertTrue(s.Instance.Count = TestSize div 2);
  for o in a do
    begin
      AssertTrue(not Odd(o.Value));
      o.Free;
    end;
  for o in s.Instance do
    AssertTrue(Odd(o.Value));
  s.Instance := Default(TObjSet);
  AssertTrue(Counter.Count = TestSize);
end;

procedure TChainHashSetTest.ObjectSetExtractIfNested;
  function IsEvenNest(constref aObj: TTestObj): Boolean;
  begin
    Result := not Odd(aObj.Value);
  end;
var
  s: TAutoObjSet;
  I, TestSize: Integer;
  Counter: TCounter;
  a: TObjArray;
  o: TTestObj;
begin
  TestSize := 100;
  Counter := Default(TCounter);
  for I := 1 to TestSize do
    s.Instance.Add(TTestObj.Create(I, Counter.Increment));
  AssertTrue(s.Instance.Count = TestSize);
  a := s.Instance.ExtractIf(@IsEvenNest);
  AssertTrue(System.Length(a) = TestSize div 2);
  AssertTrue(Counter.Count = 0);
  AssertTrue(s.Instance.Count = TestSize div 2);
  for o in a do
    begin
      AssertTrue(not Odd(o.Value));
      o.Free;
    end;
  for o in s.Instance do
    AssertTrue(Odd(o.Value));
  s.Instance := Default(TObjSet);
  AssertTrue(Counter.Count = TestSize);
end;

procedure TChainHashSetTest.ObjectSetRetain;
var
  s, s1: TAutoObjSet;
  I, TestSize: Integer;
  Counter: TCounter;
  a: TObjArray;
begin
  TestSize := 100;
  Counter := Default(TCounter);
  System.SetLength(a, TestSize div 2);
  s1.Instance := TObjSet.Create(False);

  for I := 0 to System.High(a) do
    a[I] := TTestObj.Create(Counter.Increment);
  s1.Instance.AddAll(a);
  s.Instance.AddAll(a);
  for I := 1 to TestSize div 2 do
    s.Instance.Add(TTestObj.Create(Counter.Increment));
  AssertTrue(s.Instance.Count = TestSize);
  s.Instance.RetainAll(s1.Instance);
  AssertTrue(s.Instance.Count = TestSize div 2);
  AssertTrue(Counter.Count = TestSize div 2);
  AssertTrue(s.Instance.ContainsAll(s1.Instance));
  s.Instance := Default(TObjSet);
  AssertTrue(Counter.Count = TestSize);
end;

procedure TChainHashSetTest.ObjectSetSymmetricSubtract;
var
  s, s1: TAutoObjSet;
  I, TestSize: Integer;
  Counter: TCounter;
  a: TObjArray;
begin
  TestSize := 100;
  Counter := Default(TCounter);
  for I := 1 to TestSize do
    s.Instance.Add(TTestObj.Create(Counter.Increment));
  for I := 1 to TestSize do
    s1.Instance.Add(TTestObj.Create(Counter.Increment));
  System.SetLength(a, TestSize);
  for I := 0 to System.High(a) do
    a[I] := TTestObj.Create(Counter.Increment);
  s.Instance.AddAll(a);
  AssertTrue(s.Instance.Count = TestSize * 2);
  s1.Instance.AddAll(a);
  AssertTrue(s1.Instance.Count = TestSize * 2);
  s1.Instance.OwnsObjects := False;
  s.Instance.SymmetricSubtract(s1.Instance);
  AssertTrue(s.Instance.Count = TestSize * 2);
  AssertTrue(Counter.Count = TestSize);
  AssertTrue(s1.Instance.Count = TestSize * 2);
  s.Instance := Default(TObjSet);
  AssertTrue(Counter.Count = TestSize * 3);
end;

procedure TOrdHashSetTest.TestCreate;
var
  s: TAutoSet;
begin
  AssertTrue(s.Instance.Count = 0);
  AssertTrue(s.Instance.Capacity > 0);
  AssertTrue(s.Instance.LoadFactor = s.Instance.DefaultLoadFactor);
end;

procedure TOrdHashSetTest.CreateArray;
var
  s: TAutoSet;
  I: Integer;
begin
  s.Instance := TSet.Create(IntArray11);
  AssertTrue(s.Instance.Count = 11);
  AssertTrue(s.Instance.Capacity >= 11);
  AssertTrue(s.Instance.LoadFactor = s.Instance.DefaultLoadFactor);
  for I in IntArray11 do
    AssertTrue(s.Instance.Contains(I));
end;

procedure TOrdHashSetTest.CreateEnum;
var
  s: TAutoSet;
  I: Integer;
  e: IIntEnumerable;
begin
  e := TIntArrayCursor.Create(TIntHelper.CreateCopy(IntArray11));
  s.Instance := TSet.Create(e);
  AssertTrue(s.Instance.Count = 11);
  AssertTrue(s.Instance.Capacity >= 11);
  AssertTrue(s.Instance.LoadFactor = s.Instance.DefaultLoadFactor);
  for I in IntArray11 do
    AssertTrue(s.Instance.Contains(I));
end;

procedure TOrdHashSetTest.CreateCapacity0;
var
  s: TAutoSet;
begin
  s.Instance := TSet.Create(0);
  AssertTrue(s.Instance.Count = 0);
  AssertTrue(s.Instance.Capacity >= 0);
  AssertTrue(s.Instance.LoadFactor = s.Instance.DefaultLoadFactor);
end;

procedure TOrdHashSetTest.CreateCapacity5;
var
  s: TAutoSet;
begin
  s.Instance := TSet.Create(5);
  AssertTrue(s.Instance.Count = 0);
  AssertTrue(s.Instance.Capacity >= 5);
  AssertTrue(s.Instance.LoadFactor = s.Instance.DefaultLoadFactor);
end;

procedure TOrdHashSetTest.CreateCapacity50Array11;
var
  s: TAutoSet;
  I: Integer;
begin
  s.Instance := TSet.Create(50, IntArray11);
  AssertTrue(s.Instance.Count = 11);
  AssertTrue(s.Instance.Capacity >= 50);
  AssertTrue(s.Instance.LoadFactor = s.Instance.DefaultLoadFactor);
  for I in IntArray11 do
    AssertTrue(s.Instance.Contains(I));
end;

procedure TOrdHashSetTest.CreateCapacity50Enum11;
var
  s: TAutoSet;
  I: Integer;
  e: IIntEnumerable;
begin
  e := TIntArrayCursor.Create(TIntHelper.CreateCopy(IntArray11));
  s.Instance := TSet.Create(50, e);
  AssertTrue(s.Instance.Count = 11);
  AssertTrue(s.Instance.Capacity >= 50);
  AssertTrue(s.Instance.LoadFactor = s.Instance.DefaultLoadFactor);
  for I in IntArray11 do
    AssertTrue(s.Instance.Contains(I));
end;

procedure TOrdHashSetTest.CreateLoadFactor0;
var
  s: TAutoSet;
begin
  s.Instance := TSet.Create(0.0);
  AssertTrue(s.Instance.Count = 0);
  AssertTrue(s.Instance.LoadFactor = TSet.MinLoadFactor);
end;

procedure TOrdHashSetTest.CreateLoadFactor10;
var
  s: TAutoSet;
begin
  s.Instance := TSet.Create(10.0);
  AssertTrue(s.Instance.Count = 0);
  AssertTrue(s.Instance.LoadFactor = TSet.MaxLoadFactor);
end;

procedure TOrdHashSetTest.CreateLoadFactor;
var
  s: TAutoSet;
  lf: Single;
begin
  lf := 0.5;
  s.Instance := TSet.Create(lf);
  AssertTrue(s.Instance.Count = 0);
  AssertTrue(s.Instance.LoadFactor = lf);
end;

procedure TOrdHashSetTest.CreateLoadFactor3Array11;
var
  s: TAutoSet;
  I: Integer;
  lf: Single;
begin
  lf := 3.0;
  s.Instance := TSet.Create(lf, IntArray11);
  AssertTrue(s.Instance.Count = 11);
  AssertTrue(s.Instance.Capacity >= 11);
  AssertTrue(s.Instance.LoadFactor = lf);
  for I in IntArray11 do
    AssertTrue(s.Instance.Contains(I));
end;

procedure TOrdHashSetTest.CreateLoadFactor3Enum11;
var
  s: TAutoSet;
  I: Integer;
  e: IIntEnumerable;
  lf: Single;
begin
  lf := 3.0;
  e := TIntArrayCursor.Create(TIntHelper.CreateCopy(IntArray11));
  s.Instance := TSet.Create(lf, e);
  AssertTrue(s.Instance.Count = 11);
  AssertTrue(s.Instance.Capacity >= 11);
  AssertTrue(s.Instance.LoadFactor = lf);
  for I in IntArray11 do
    AssertTrue(s.Instance.Contains(I));
end;

procedure TOrdHashSetTest.CreateCapacityLoadFactor;
var
  s: TAutoSet;
  lf: Single;
begin
  lf := 0.45;
  s.Instance := TSet.Create(50, lf);
  AssertTrue(s.Instance.Count = 0);
  AssertTrue(s.Instance.Capacity >= 50);
  AssertTrue(s.Instance.LoadFactor = lf);
end;

procedure TOrdHashSetTest.CreateCapacityLoadFactor3Array11;
var
  s: TAutoSet;
  I: Integer;
  lf: Single;
begin
  lf := 3.0;
  s.Instance := TSet.Create(50, lf, IntArray11);
  AssertTrue(s.Instance.Count = 11);
  AssertTrue(s.Instance.Capacity >= 32);
  AssertTrue(s.Instance.LoadFactor = lf);
  for I in IntArray11 do
    AssertTrue(s.Instance.Contains(I));
end;

procedure TOrdHashSetTest.CreateCapacityLoadFactor3Enum11;
var
  s: TAutoSet;
  I: Integer;
  e: IIntEnumerable;
  lf: Single;
begin
  lf := 3.0;
  e := TIntArrayCursor.Create(TIntHelper.CreateCopy(IntArray11));
  s.Instance := TSet.Create(50, lf, e);
  AssertTrue(s.Instance.Count = 11);
  AssertTrue(s.Instance.Capacity >= 32);
  AssertTrue(s.Instance.LoadFactor = lf);
  for I in IntArray11 do
    AssertTrue(s.Instance.Contains(I));
end;

procedure TOrdHashSetTest.Clear;
var
  s: TAutoSet;
begin
  s.Instance := TSet.Create(IntArray11);
  AssertTrue(s.Instance.Count = 11);
  AssertTrue(s.Instance.Capacity >= 11);
  s.Instance.Clear;
  AssertTrue(s.Instance.Count = 0);
  AssertTrue(s.Instance.Capacity = 0);
end;

procedure TOrdHashSetTest.EnsureCapacity;
var
  s: TAutoSet;
  c: SizeInt;
begin
  c := s.Instance.ExpandTreshold;
  s.Instance.EnsureCapacity(c + 1);
  AssertTrue(s.Instance.Capacity > c);
end;

procedure TOrdHashSetTest.TrimToFit;
var
  s: TAutoSet;
  c: SizeInt;
begin
  s.Instance := TSet.Create(IntArray11);
  c := s.Instance.Capacity;
  s.Instance.TrimToFit;
  AssertTrue(s.Instance.Capacity < c);
end;

procedure TOrdHashSetTest.Add;
var
  s: TAutoSet;
begin
  AssertTrue(s.Instance.Count = 0);
  AssertTrue(s.Instance.Add(1));
  AssertTrue(s.Instance.Count = 1);
  AssertTrue(s.Instance.Contains(1));
  AssertTrue(s.Instance.Add(51));
  AssertTrue(s.Instance.Count = 2);
  AssertTrue(s.Instance.Contains(51));
  AssertFalse(s.Instance.Add(1));
  AssertFalse(s.Instance.Add(51));
  AssertTrue(s.Instance.Count = 2);
  AssertTrue(s.Instance.Add(52));
  AssertTrue(s.Instance.Count = 3);
  AssertTrue(s.Instance.Contains(52));
end;

procedure TOrdHashSetTest.Add100;
var
  s: TAutoSet;
  I: Integer;
begin
  for I := 1 to 100 do
    AssertTrue(s.Instance.Add(I));
  AssertTrue(s.Instance.Count = 100);
  for I := 1 to 100 do
    AssertFalse(s.Instance.Add(I));
  AssertTrue(s.Instance.Count = 100);
  for I := 1 to 100 do
    AssertTrue(s.Instance.Contains(I));
end;

procedure TOrdHashSetTest.AddArray;
var
  s: TAutoSet;
begin
  AssertTrue(s.Instance.AddAll(IntArray21) = System.Length(IntArray21));
  AssertTrue(s.Instance.Count = System.Length(IntArray21));
  AssertTrue(s.Instance.ContainsAll(IntArray21));
end;

procedure TOrdHashSetTest.AddEnum;
var
  s: TAutoSet;
  e: IIntEnumerable;
begin
  e := TIntArrayCursor.Create(TIntHelper.CreateCopy(IntArray21));
  AssertTrue(s.Instance.AddAll(e) = System.Length(IntArray21));
  AssertTrue(s.Instance.Count = System.Length(IntArray21));
  AssertTrue(s.Instance.ContainsAll(IntArray21));
end;

procedure TOrdHashSetTest.AddSelf;
var
  s: TAutoSet;
begin
  s.Instance.AddAll(IntArray21);
  AssertTrue(s.Instance.AddAll(s.Instance) = 0);
  AssertTrue(s.Instance.Count = System.Length(IntArray21));
end;

procedure TOrdHashSetTest.Remove;
var
  s: TAutoSet;
begin
  s.Instance := TSet.Create(IntArray21);
  AssertTrue(s.Instance.Count = System.Length(IntArray21));
  AssertTrue(s.Instance.Remove(IntArray21[1]));
  AssertTrue(s.Instance.Count = System.Length(IntArray21) - 1);
  AssertTrue(s.Instance.Remove(IntArray21[System.High(IntArray21)]));
  AssertTrue(s.Instance.Count = System.Length(IntArray21) - 2);
  AssertFalse(s.Instance.Remove(-1));
  AssertTrue(s.Instance.Count = System.Length(IntArray21) - 2);
  AssertFalse(s.Instance.Remove(100));
  AssertTrue(s.Instance.Count = System.Length(IntArray21) - 2);
end;

procedure TOrdHashSetTest.RemoveArray;
var
  s: TAutoSet;
begin
  s.Instance := TSet.Create(IntArray21);
  AssertTrue(s.Instance.RemoveAll(IntArray11) = 11);
  AssertTrue(s.Instance.Count = 10);
end;

procedure TOrdHashSetTest.RemoveEnum;
var
  s: TAutoSet;
  e: IIntEnumerable;
begin
  e := TIntArrayCursor.Create(TIntHelper.CreateCopy(IntArray11));
  s.Instance := TSet.Create(IntArray21);
  AssertTrue(s.Instance.RemoveAll(e) = 11);
  AssertTrue(s.Instance.Count = 10);
end;

procedure TOrdHashSetTest.RemoveSelf;
var
  s: TAutoSet;
begin
  s.Instance := TSet.Create(IntArray21);
  AssertTrue(s.Instance.RemoveAll(s.Instance) = 21);
  AssertTrue(s.Instance.IsEmpty);
end;

procedure TOrdHashSetTest.RemoveIfRegular;
var
  s: TAutoSet;
begin
  s.Instance := TSet.Create(IntArray21);
  AssertTrue(s.Instance.RemoveIf(@IsEvenInt) = 10);
  AssertTrue(s.Instance.Count = 11);
  AssertTrue(s.Instance.ContainsAll(IntArray11));
end;

procedure TOrdHashSetTest.RemoveIfDelegated;
var
  s: TAutoSet;
begin
  s.Instance := TSet.Create(IntArray21);
  AssertTrue(s.Instance.RemoveIf(@IsEven) = 10);
  AssertTrue(s.Instance.Count = 11);
  AssertTrue(s.Instance.ContainsAll(IntArray11));
end;

procedure TOrdHashSetTest.RemoveIfNested;
  function IsEvenInt(constref aValue: Integer): Boolean;
  begin
    Result := not Odd(aValue);
  end;
var
  s: TAutoSet;
begin
  s.Instance := TSet.Create(IntArray21);
  AssertTrue(s.Instance.RemoveIf(@IsEvenInt) = 10);
  AssertTrue(s.Instance.Count = 11);
  AssertTrue(s.Instance.ContainsAll(IntArray11));
end;

procedure TOrdHashSetTest.ExtractIfRegular;
var
  s: TAutoSet;
  e: TIntArray;
  I: Integer;
begin
  s.Instance := TSet.Create(IntArray21);
  e := s.Instance.ExtractIf(@IsEvenInt);
  AssertTrue(s.Instance.Count = 11);
  AssertTrue(s.Instance.ContainsAll(IntArray11));
  AssertTrue(System.Length(e) = 10);
  TIntHelper.QuickSort(e);
  AssertTrue(TIntHelper.Same(IntArray10, e));
  AssertTrue(s.Instance.AddAll(e) = 10);
  s.Instance.Clear;
  System.SetLength(e, 100);
  for I := 0 to System.High(e) do
    e[I] := I;
  AssertTrue(s.Instance.AddAll(e) = 100);
  e := s.Instance.ExtractIf(@IsEvenInt);
  for I := 0 to System.High(e) do
    AssertTrue(IsEvenInt(e[I]));
  AssertTrue(s.Instance.AddAll(e) = 50);
end;

procedure TOrdHashSetTest.ExtractIfDelegated;
var
  s: TAutoSet;
  e: TIntArray;
begin
  s.Instance := TSet.Create(IntArray21);
  e := s.Instance.ExtractIf(@IsEven);
  AssertTrue(s.Instance.Count = 11);
  AssertTrue(s.Instance.ContainsAll(IntArray11));
  AssertTrue(System.Length(e) = 10);
  TIntHelper.QuickSort(e);
  AssertTrue(TIntHelper.Same(IntArray10, e));
end;

procedure TOrdHashSetTest.ExtractIfNested;
  function IsEvenInt(constref aValue: Integer): Boolean;
  begin
    Result := not Odd(aValue);
  end;
var
  s: TAutoSet;
  e: TIntArray;
begin
  s.Instance := TSet.Create(IntArray21);
  e := s.Instance.ExtractIf(@IsEvenInt);
  AssertTrue(s.Instance.Count = 11);
  AssertTrue(s.Instance.ContainsAll(IntArray11));
  AssertTrue(System.Length(e) = 10);
  TIntHelper.QuickSort(e);
  AssertTrue(TIntHelper.Same(IntArray10, e));
end;

procedure TOrdHashSetTest.Retain;
var
  s, s1: TAutoSet;
begin
  s.Instance := TSet.Create(IntArray21);
  s1.Instance := TSet.Create(IntArray11);
  s.Instance.AddAll([-3, -2, -1, 0]);
  AssertTrue(s.Instance.Count = 25);
  s.Instance.RetainAll(s1.Instance);
  AssertTrue(s.Instance.Count = 11);
  AssertTrue(s.Instance.ContainsAll(s1.Instance));
end;

procedure TOrdHashSetTest.Retain_1;
var
  s, s1: TAutoSet;
begin
  s.Instance := TSet.Create(IntArray10);
  s1.Instance := TSet.Create(IntArray11);
  s.Instance.RetainAll(s1.Instance);
  AssertTrue(s.Instance.IsEmpty);
end;

procedure TOrdHashSetTest.RetainSelf;
var
  s: TAutoSet;
begin
  s.Instance := TSet.Create(IntArray21);
  AssertTrue(s.Instance.Count = 21);
  s.Instance.RetainAll(s.Instance);
  AssertTrue(s.Instance.Count = 21);
  AssertTrue(s.Instance.ContainsAll(IntArray21));
end;

procedure TOrdHashSetTest.IsSuperset;
var
  s, s1: TAutoSet;
begin
  s.Instance := TSet.Create(IntArray21);
  s1.Instance := TSet.Create(IntArray21);
  AssertTrue(s.Instance.IsSuperset(s1.Instance));
  AssertTrue(s1.Instance.IsSuperset(s.Instance));
  AssertTrue(s.Instance.IsSuperset(s.Instance));
  s1.Instance := TSet.Create(IntArray11);
  AssertTrue(s.Instance.IsSuperset(s1.Instance));
  AssertFalse(s1.Instance.IsSuperset(s.Instance));
end;

procedure TOrdHashSetTest.IsSubset;
var
  s, s1: TAutoSet;
begin
  s.Instance := TSet.Create(IntArray21);
  s1.Instance := TSet.Create(IntArray21);
  AssertTrue(s.Instance.IsSubset(s1.Instance));
  AssertTrue(s1.Instance.IsSubset(s.Instance));
  AssertTrue(s.Instance.IsSubset(s.Instance));
  s1.Instance := TSet.Create(IntArray11);
  AssertTrue(s1.Instance.IsSubset(s.Instance));
  AssertFalse(s.Instance.IsSubset(s1.Instance));
end;

procedure TOrdHashSetTest.IsEqual;
var
  s, s1: TAutoSet;
begin
  s.Instance := TSet.Create(IntArray21);
  s1.Instance := TSet.Create(IntArray21);
  AssertTrue(s.Instance.IsEqual(s.Instance));
  AssertTrue(s.Instance.IsEqual(s1.Instance));
  s1.Instance := TSet.Create(IntArray11);
  AssertFalse(s.Instance.IsEqual(s1.Instance));
end;

procedure TOrdHashSetTest.Intersecting;
var
  s, s1: TAutoSet;
begin
  s.Instance := TSet.Create(IntArray21);
  s1.Instance := TSet.Create(IntArray11);
  AssertTrue(s.Instance.Intersecting(s.Instance));
  AssertTrue(s.Instance.Intersecting(s1.Instance));
  s.Instance := TSet.Create(IntArray10);
  AssertFalse(s.Instance.Intersecting(s1.Instance));
  AssertFalse(s1.Instance.Intersecting(s.Instance));
end;

procedure TOrdHashSetTest.SymmetricSubtract;
var
  s, s1: TAutoSet;
begin
  s.Instance := TSet.Create(IntArray21);
  s1.Instance := TSet.Create(IntArray21);
  s.Instance.SymmetricSubtract(s1.Instance);
  AssertTrue(s.Instance.IsEmpty);
  s.Instance := TSet.Create(IntArray21);
  s1.Instance.Clear;
  s.Instance.SymmetricSubtract(s1.Instance);
  AssertTrue(s.Instance.Count = 21);
  AssertTrue(s.Instance.ContainsAll(IntArray21));
  s.Instance := TSet.Create(IntArray11);
  s1.Instance := TSet.Create(IntArray10);
  s.Instance.SymmetricSubtract(s1.Instance);
  AssertTrue(s.Instance.Count = 21);
  AssertTrue(s.Instance.ContainsAll(IntArray21));
  s.Instance := TSet.Create(IntArray11);
  s1.Instance := TSet.Create(IntArray10);
  s.Instance.AddAll([-5, -4, -3, -2, -1, 0]);
  s1.Instance.AddAll([-5, -4, -3, -2, -1, 0]);
  s.Instance.SymmetricSubtract(s1.Instance);
  AssertTrue(s.Instance.Count = 21);
  AssertTrue(s.Instance.ContainsAll(IntArray21));
end;

procedure TOrdHashSetTest.Ordered;
var
  s: TAutoSet;
  I: SizeInt;
  a: TIntArray;
begin
  System.SetLength(a, 100);
  for I := 1 to System.Length(a) do
    a[I - 1] := I * 1001;
  TIntHelper.RandomShuffle(a);
  s.Instance := TSet.Create(a);
  AssertTrue(TIntHelper.Same(a, s.Instance.ToArray));
  TIntHelper.Reverse(a);
  AssertTrue(TIntHelper.Same(a, s.Instance.Reverse.ToArray));
end;

procedure TOrdHashSetTest.ObjectSetRemove;
var
  s: TAutoObjSet;
  a: TObjArray;
  I, TestSize: Integer;
  Counter: TCounter;
begin
  TestSize := 36;
  Counter := Default(TCounter);
  System.SetLength(a, TestSize);
  for I := 0 to System.High(a) do
    begin
      a[I] := TTestObj.Create(Counter.Increment);
      s.Instance.Add(a[I]);
    end;
  AssertTrue(s.Instance.Count = TestSize);
  for I := 0 to System.High(a) do
    begin
      AssertTrue(s.Instance.Remove(a[I]));
      AssertTrue(s.Instance.Count = Pred(TestSize - I));
      AssertTrue(Counter.Count = Succ(I));
    end;
  AssertTrue(s.Instance.IsEmpty);
end;

procedure TOrdHashSetTest.ObjectSetRemoveArray;
var
  s: TAutoObjSet;
  a: TObjArray;
  I, TestSize: Integer;
  Counter: TCounter;
begin
  TestSize := 56;
  Counter := Default(TCounter);
  for I := 1 to TestSize do
    s.Instance.Add(TTestObj.Create(Counter.Increment));
  AssertTrue(s.Instance.Count = TestSize);
  a := s.Instance.ToArray;
  AssertTrue(System.Length(a) = TestSize);
  AssertTrue(s.Instance.RemoveAll(a) = TestSize);
  AssertTrue(s.Instance.IsEmpty);
  AssertTrue(Counter.Count = TestSize);
end;

procedure TOrdHashSetTest.ObjectSetRemoveEnum;
type
  TFakeEnum = specialize TGArrayCursor<TTestObj>;
var
  s: TAutoObjSet;
  e: TFakeEnum;
  I, TestSize: Integer;
  Counter: TCounter;
begin
  TestSize := 46;
  Counter := Default(TCounter);
  for I := 1 to TestSize do
    s.Instance.Add(TTestObj.Create(Counter.Increment));
  AssertTrue(s.Instance.Count = TestSize);
  e := TFakeEnum.Create(s.Instance.ToArray);
  AssertTrue(s.Instance.RemoveAll(e) = TestSize);
  AssertTrue(s.Instance.IsEmpty);
  AssertTrue(Counter.Count = TestSize);
end;

procedure TOrdHashSetTest.ObjectSetRemoveSelf;
var
  s: TAutoObjSet;
  I, TestSize: Integer;
  Counter: TCounter;
begin
  TestSize := 100;
  Counter := Default(TCounter);
  for I := 1 to TestSize do
    s.Instance.Add(TTestObj.Create(Counter.Increment));
  AssertTrue(s.Instance.Count = TestSize);
  AssertTrue(s.Instance.RemoveAll(s.Instance) = TestSize);
  AssertTrue(s.Instance.IsEmpty);
  AssertTrue(Counter.Count = TestSize);
end;

procedure TOrdHashSetTest.ObjectSetRemoveIfRegular;
var
  s: TAutoObjSet;
  I, TestSize: Integer;
  Counter: TCounter;
  o: TTestObj;
begin
  TestSize := 100;
  Counter := Default(TCounter);
  for I := 1 to TestSize do
    s.Instance.Add(TTestObj.Create(I, Counter.Increment));
  AssertTrue(s.Instance.Count = TestSize);
  AssertTrue(s.Instance.RemoveIf(@IsEvenObj) = TestSize div 2);
  AssertTrue(Counter.Count = TestSize div 2);
  AssertTrue(s.Instance.Count = TestSize div 2);
  for o in s.Instance do
    AssertTrue(Odd(o.Value));
  s.Instance := Default(TObjSet);
  AssertTrue(Counter.Count = TestSize);
end;

procedure TOrdHashSetTest.ObjectSetRemoveIfDelegated;
var
  s: TAutoObjSet;
  I, TestSize: Integer;
  Counter: TCounter;
  o: TTestObj;
begin
  TestSize := 100;
  Counter := Default(TCounter);
  for I := 1 to TestSize do
    s.Instance.Add(TTestObj.Create(I, Counter.Increment));
  AssertTrue(s.Instance.Count = TestSize);
  AssertTrue(s.Instance.RemoveIf(@ObjIsEven) = TestSize div 2);
  AssertTrue(Counter.Count = TestSize div 2);
  AssertTrue(s.Instance.Count = TestSize div 2);
  for o in s.Instance do
    AssertTrue(Odd(o.Value));
  s.Instance := Default(TObjSet);
  AssertTrue(Counter.Count = TestSize);
end;

procedure TOrdHashSetTest.ObjectSetRemoveIfNested;
  function IsEvenNest(constref aObj: TTestObj): Boolean;
  begin
    Result := not Odd(aObj.Value);
  end;
var
  s: TAutoObjSet;
  I, TestSize: Integer;
  Counter: TCounter;
  o: TTestObj;
begin
  TestSize := 100;
  Counter := Default(TCounter);
  for I := 1 to TestSize do
    s.Instance.Add(TTestObj.Create(I, Counter.Increment));
  AssertTrue(s.Instance.Count = TestSize);
  AssertTrue(s.Instance.RemoveIf(@IsEvenNest) = TestSize div 2);
  AssertTrue(Counter.Count = TestSize div 2);
  AssertTrue(s.Instance.Count = TestSize div 2);
  for o in s.Instance do
    AssertTrue(Odd(o.Value));
  s.Instance := Default(TObjSet);
  AssertTrue(Counter.Count = TestSize);
end;

procedure TOrdHashSetTest.ObjectSetExtractIfRegular;
var
  s: TAutoObjSet;
  I, TestSize: Integer;
  Counter: TCounter;
  a: TObjArray;
  o: TTestObj;
begin
  TestSize := 100;
  Counter := Default(TCounter);
  for I := 1 to TestSize do
    s.Instance.Add(TTestObj.Create(I, Counter.Increment));
  AssertTrue(s.Instance.Count = TestSize);
  a := s.Instance.ExtractIf(@IsEvenObj);
  AssertTrue(System.Length(a) = TestSize div 2);
  AssertTrue(Counter.Count = 0);
  AssertTrue(s.Instance.Count = TestSize div 2);
  for o in a do
    begin
      AssertTrue(not Odd(o.Value));
      o.Free;
    end;
  for o in s.Instance do
    AssertTrue(Odd(o.Value));
  s.Instance := Default(TObjSet);
  AssertTrue(Counter.Count = TestSize);
end;

procedure TOrdHashSetTest.ObjectSetExtractIfDelegated;
var
  s: TAutoObjSet;
  I, TestSize: Integer;
  Counter: TCounter;
  a: TObjArray;
  o: TTestObj;
begin
  TestSize := 100;
  Counter := Default(TCounter);
  for I := 1 to TestSize do
    s.Instance.Add(TTestObj.Create(I, Counter.Increment));
  AssertTrue(s.Instance.Count = TestSize);
  a := s.Instance.ExtractIf(@ObjIsEven);
  AssertTrue(System.Length(a) = TestSize div 2);
  AssertTrue(Counter.Count = 0);
  AssertTrue(s.Instance.Count = TestSize div 2);
  for o in a do
    begin
      AssertTrue(not Odd(o.Value));
      o.Free;
    end;
  for o in s.Instance do
    AssertTrue(Odd(o.Value));
  s.Instance := Default(TObjSet);
  AssertTrue(Counter.Count = TestSize);
end;

procedure TOrdHashSetTest.ObjectSetExtractIfNested;
  function IsEvenNest(constref aObj: TTestObj): Boolean;
  begin
    Result := not Odd(aObj.Value);
  end;
var
  s: TAutoObjSet;
  I, TestSize: Integer;
  Counter: TCounter;
  a: TObjArray;
  o: TTestObj;
begin
  TestSize := 100;
  Counter := Default(TCounter);
  for I := 1 to TestSize do
    s.Instance.Add(TTestObj.Create(I, Counter.Increment));
  AssertTrue(s.Instance.Count = TestSize);
  a := s.Instance.ExtractIf(@IsEvenNest);
  AssertTrue(System.Length(a) = TestSize div 2);
  AssertTrue(Counter.Count = 0);
  AssertTrue(s.Instance.Count = TestSize div 2);
  for o in a do
    begin
      AssertTrue(not Odd(o.Value));
      o.Free;
    end;
  for o in s.Instance do
    AssertTrue(Odd(o.Value));
  s.Instance := Default(TObjSet);
  AssertTrue(Counter.Count = TestSize);
end;

procedure TOrdHashSetTest.ObjectSetRetain;
var
  s, s1: TAutoObjSet;
  I, TestSize: Integer;
  Counter: TCounter;
  a: TObjArray;
begin
  TestSize := 100;
  Counter := Default(TCounter);
  System.SetLength(a, TestSize div 2);
  s1.Instance := TObjSet.Create(False);

  for I := 0 to System.High(a) do
    a[I] := TTestObj.Create(Counter.Increment);
  s1.Instance.AddAll(a);
  s.Instance.AddAll(a);
  for I := 1 to TestSize div 2 do
    s.Instance.Add(TTestObj.Create(Counter.Increment));
  AssertTrue(s.Instance.Count = TestSize);
  s.Instance.RetainAll(s1.Instance);
  AssertTrue(s.Instance.Count = TestSize div 2);
  AssertTrue(Counter.Count = TestSize div 2);
  AssertTrue(s.Instance.ContainsAll(s1.Instance));
  s.Instance := Default(TObjSet);
  AssertTrue(Counter.Count = TestSize);
end;

procedure TOrdHashSetTest.ObjectSetSymmetricSubtract;
var
  s, s1: TAutoObjSet;
  I, TestSize: Integer;
  Counter: TCounter;
  a: TObjArray;
begin
  TestSize := 100;
  Counter := Default(TCounter);
  for I := 1 to TestSize do
    s.Instance.Add(TTestObj.Create(Counter.Increment));
  for I := 1 to TestSize do
    s1.Instance.Add(TTestObj.Create(Counter.Increment));
  System.SetLength(a, TestSize);
  for I := 0 to System.High(a) do
    a[I] := TTestObj.Create(Counter.Increment);
  s.Instance.AddAll(a);
  AssertTrue(s.Instance.Count = TestSize * 2);
  s1.Instance.AddAll(a);
  AssertTrue(s1.Instance.Count = TestSize * 2);
  s1.Instance.OwnsObjects := False;
  s.Instance.SymmetricSubtract(s1.Instance);
  AssertTrue(s.Instance.Count = TestSize * 2);
  AssertTrue(Counter.Count = TestSize);
  AssertTrue(s1.Instance.Count = TestSize * 2);
  s.Instance := Default(TObjSet);
  AssertTrue(Counter.Count = TestSize * 3);
end;

{ TLiteHashSetLPTest }

procedure TLiteHashSetLPTest.Add;
var
  s: TSet;
begin
  AssertTrue(s.Count = 0);
  AssertTrue(s.Add(1));
  AssertTrue(s.Count = 1);
  AssertTrue(s.Contains(1));
  AssertTrue(s.Add(51));
  AssertTrue(s.Count = 2);
  AssertTrue(s.Contains(51));
  AssertFalse(s.Add(1));
  AssertFalse(s.Add(51));
  AssertTrue(s.Count = 2);
  AssertTrue(s.Add(52));
  AssertTrue(s.Count = 3);
  AssertTrue(s.Contains(52));
end;

procedure TLiteHashSetLPTest.Add100;
var
  s: TSet;
  I: Integer;
begin
  for I := 1 to 100 do
    AssertTrue(s.Add(I));
  AssertTrue(s.Count = 100);
  for I := 1 to 100 do
    AssertFalse(s.Add(I));
  AssertTrue(s.Count = 100);
  for I := 1 to 100 do
    AssertTrue(s.Contains(I));
end;

procedure TLiteHashSetLPTest.AddArray;
var
  s: TSet;
begin
  AssertTrue(s.AddAll(IntArray21) = Length(IntArray21));
  AssertTrue(s.Count = Length(IntArray21));
  AssertTrue(s.ContainsAll(IntArray21));
end;

procedure TLiteHashSetLPTest.AddEnum;
var
  s: TSet;
  e: IIntEnumerable;
begin
  e := TIntArrayCursor.Create(TIntHelper.CreateCopy(IntArray21));
  AssertTrue(s.AddAll(e) = Length(IntArray21));
  AssertTrue(s.Count = Length(IntArray21));
  AssertTrue(s.ContainsAll(IntArray21));
end;

procedure TLiteHashSetLPTest.AddSet;
var
  s, s21: TSet;
begin
  AssertTrue(s.AddAll(IntArray11) = Length(IntArray11));
  AssertTrue(s.Count = Length(IntArray11));
  AssertTrue(s21.AddAll(IntArray21) = Length(IntArray21));
  AssertTrue(s21.Count = Length(IntArray21));
  AssertTrue(s.AddAll(s21) = 10);
  AssertTrue(s.Count = Length(IntArray21));
  AssertTrue(s.ContainsAll(IntArray21));
end;

procedure TLiteHashSetLPTest.AddSelf;
var
  s: TSet;
begin
  AssertTrue(s.AddAll(IntArray21) = Length(IntArray21));
  AssertTrue(s.AddAll(s) = 0);
  AssertTrue(s.Count = Length(IntArray21));
end;

procedure TLiteHashSetLPTest.LoadFactor;
var
  s: TSet;
begin
  s.AddAll(IntArray21);
  s.LoadFactor := Single(0.95);
  AssertTrue(s.LoadFactor = Single(0.9));
  AssertTrue(s.Count = Length(IntArray21));
  AssertTrue(s.LoadFactor = Single(0.9));
  s.LoadFactor := Single(0.4);
  AssertTrue(s.LoadFactor = Single(0.4));
  AssertTrue(s.Count = Length(IntArray21));
  s.LoadFactor := Single(0.2);
  AssertTrue(s.LoadFactor = Single(0.25));
  AssertTrue(s.Count = Length(IntArray21));
end;

procedure TLiteHashSetLPTest.Clear;
var
  s: TSet;
begin
  s.AddAll(IntArray11);
  AssertTrue(s.Count = 11);
  AssertTrue(s.Capacity >= 11);
  s.Clear;
  AssertTrue(s.Count = 0);
  AssertTrue(s.Capacity = 0);
end;

procedure TLiteHashSetLPTest.EnsureCapacity;
var
  s: TSet;
  c: SizeInt;
begin
  c := s.ExpandTreshold;
  s.EnsureCapacity(c + 1);
  AssertTrue(s.Capacity > c);
end;

procedure TLiteHashSetLPTest.TrimToFit;
var
  s: TSet;
  c: SizeInt;
begin
  s.AddAll(IntArray11);
  c := s.Capacity;
  s.EnsureCapacity(s.ExpandTreshold + 1);
  AssertTrue(s.Capacity > c);
  s.TrimToFit;
  AssertTrue(s.Capacity = c);
end;

procedure TLiteHashSetLPTest.Remove;
var
  s: TSet;
begin
  s.AddAll(IntArray21);
  AssertTrue(s.Count = Length(IntArray21));
  AssertTrue(s.Remove(IntArray21[1]));
  AssertTrue(s.Count = Length(IntArray21) - 1);
  AssertTrue(s.Remove(IntArray21[High(IntArray21)]));
  AssertTrue(s.Count = Length(IntArray21) - 2);
  AssertFalse(s.Remove(-1));
  AssertTrue(s.Count = Length(IntArray21) - 2);
  AssertFalse(s.Remove(100));
  AssertTrue(s.Count = Length(IntArray21) - 2);
end;

procedure TLiteHashSetLPTest.RemoveArray;
var
  s: TSet;
begin
  s.AddAll(IntArray21);
  AssertTrue(s.RemoveAll(IntArray11) = 11);
  AssertTrue(s.Count = 10);
end;

procedure TLiteHashSetLPTest.RemoveEnum;
var
  s: TSet;
  e: IIntEnumerable;
begin
  e := TIntArrayCursor.Create(TIntHelper.CreateCopy(IntArray11));
  s.AddAll(IntArray21);
  AssertTrue(s.RemoveAll(e) = 11);
  AssertTrue(s.Count = 10);
end;

procedure TLiteHashSetLPTest.RemoveSet;
var
  s, s11: TSet;
begin
  s11.AddAll(IntArray11);
  s.AddAll(IntArray21);
  AssertTrue(s.RemoveAll(s11) = 11);
  AssertTrue(s.Count = 10);
end;

procedure TLiteHashSetLPTest.RemoveSelf;
var
  s: TSet;
begin
  s.AddAll(IntArray21);
  AssertTrue(s.RemoveAll(s) = 21);
  AssertTrue(s.IsEmpty);
end;

procedure TLiteHashSetLPTest.RemoveIfRegular;
var
  s: TSet;
begin
  s.AddAll(IntArray21);
  AssertTrue(s.RemoveIf(@IsEvenInt) = 10);
  AssertTrue(s.Count = 11);
  AssertTrue(s.ContainsAll(IntArray11));
end;

procedure TLiteHashSetLPTest.RemoveIfDelegated;
var
  s: TSet;
begin
  s.AddAll(IntArray21);
  AssertTrue(s.RemoveIf(@IsEven) = 10);
  AssertTrue(s.Count = 11);
  AssertTrue(s.ContainsAll(IntArray11));
end;

procedure TLiteHashSetLPTest.RemoveIfNested;
  function IsEvenInt(constref aValue: Integer): Boolean;
  begin
    Result := not Odd(aValue);
  end;
var
  s: TSet;
begin
  s.AddAll(IntArray21);
  AssertTrue(s.RemoveIf(@IsEvenInt) = 10);
  AssertTrue(s.Count = 11);
  AssertTrue(s.ContainsAll(IntArray11));
end;

procedure TLiteHashSetLPTest.ExtractIfRegular;
var
  s: TSet;
  a: TIntArray;
begin
  s.AddAll(IntArray21);
  a := s.ExtractIf(@IsEvenInt);
  AssertTrue(s.Count = 11);
  AssertTrue(s.ContainsAll(IntArray11));
  AssertTrue(Length(a) = 10);
  TIntHelper.Sort(a);
  AssertTrue(TIntHelper.Same(IntArray10, a));
end;

procedure TLiteHashSetLPTest.ExtractIfDelegated;
var
  s: TSet;
  a: TIntArray;
begin
  s.AddAll(IntArray21);
  a := s.ExtractIf(@IsEven);
  AssertTrue(s.Count = 11);
  AssertTrue(s.ContainsAll(IntArray11));
  AssertTrue(Length(a) = 10);
  TIntHelper.Sort(a);
  AssertTrue(TIntHelper.Same(IntArray10, a));
end;

procedure TLiteHashSetLPTest.ExtractIfNested;
  function IsEvenInt(constref aValue: Integer): Boolean;
  begin
    Result := not Odd(aValue);
  end;
var
  s: TSet;
  a: TIntArray;
begin
  s.AddAll(IntArray21);
  a := s.ExtractIf(@IsEvenInt);
  AssertTrue(s.Count = 11);
  AssertTrue(s.ContainsAll(IntArray11));
  AssertTrue(Length(a) = 10);
  TIntHelper.Sort(a);
  AssertTrue(TIntHelper.Same(IntArray10, a));
end;

procedure TLiteHashSetLPTest.Retain;
var
  s, s11: TSet;
begin
  s.AddAll(IntArray21);
  s.AddAll([-3, -2, -1, 0]);
  AssertTrue(s.Count = 25);
  s11.AddAll(IntArray11);
  s.RetainAll(s11);
  AssertTrue(s.Count = 11);
  AssertTrue(s.ContainsAll(s11));
end;

procedure TLiteHashSetLPTest.Retain_1;
var
  s, s11: TSet;
begin
  s.AddAll(IntArray10);
  s11.AddAll(IntArray11);
  s.RetainAll(s11);
  AssertTrue(s.IsEmpty);
end;

procedure TLiteHashSetLPTest.RetainSelf;
var
  s: TSet;
begin
  s.AddAll(IntArray21);
  s.RetainAll(s);
  AssertTrue(s.Count = 21);
  AssertTrue(s.ContainsAll(IntArray21));
end;

procedure TLiteHashSetLPTest.IsSuperset;
var
  s, s1: TSet;
begin
  s.AddAll(IntArray21);
  s1.AddAll(IntArray21);
  AssertTrue(s.IsSuperset(s1));
  AssertTrue(s1.IsSuperset(s));
  AssertTrue(s.IsSuperset(s));
  s1.Clear;
  s1.AddAll(IntArray11);
  AssertTrue(s.IsSuperset(s1));
  AssertFalse(s1.IsSuperset(s));
end;

procedure TLiteHashSetLPTest.IsSubset;
var
  s, s1: TSet;
begin
  s.AddAll(IntArray21);
  s1.AddAll(IntArray21);
  AssertTrue(s.IsSubset(s1));
  AssertTrue(s1.IsSubset(s));
  AssertTrue(s.IsSubset(s));
  s1.Clear;
  s1.AddAll(IntArray11);
  AssertTrue(s1.IsSubset(s));
  AssertFalse(s.IsSubset(s1));
end;

procedure TLiteHashSetLPTest.IsEqual;
var
  s, s1: TSet;
begin
  s.AddAll(IntArray21);
  s1.AddAll(IntArray21);
  AssertTrue(s.IsEqual(s));
  AssertTrue(s.IsEqual(s1));
  AssertTrue(s1.IsEqual(s));
  s1.Remove(21);
  AssertFalse(s.IsEqual(s1));
  AssertFalse(s1.IsEqual(s));
end;

procedure TLiteHashSetLPTest.Intersecting;
var
  s, s11: TSet;
begin
  s.AddAll(IntArray21);
  s11.AddAll(IntArray11);
  AssertTrue(s.Intersecting(s));
  AssertTrue(s.Intersecting(s11));
  AssertTrue(s11.Intersecting(s));
  s.Clear;
  s.AddAll(IntArray10);
  AssertFalse(s.Intersecting(s11));
  AssertFalse(s11.Intersecting(s));
end;

procedure TLiteHashSetLPTest.SymmetricSubtract;
var
  s, s1: TSet;
begin
  s.AddAll(IntArray21);
  s1.AddAll(IntArray21);
  s.SymmetricSubtract(s1);
  AssertTrue(s.IsEmpty);
  s.AddAll(IntArray21);
  s1.Clear;
  s.SymmetricSubtract(s1);
  AssertTrue(s.Count = 21);
  AssertTrue(s.ContainsAll(IntArray21));
  s.Clear;
  s.AddAll(IntArray11);
  s1.AddAll(IntArray10);
  s.SymmetricSubtract(s1);
  AssertTrue(s.Count = 21);
  AssertTrue(s.ContainsAll(IntArray21));
  s.Clear;
  s.AddAll(IntArray11);
  s.AddAll([-5, -4, -3, -2, -1, 0]);
  s1.AddAll([-5, -4, -3, -2, -1, 0]);
  s.SymmetricSubtract(s1);
  AssertTrue(s.Count = 21);
  AssertTrue(s.ContainsAll(IntArray21));
end;

procedure TLiteHashSetLPTest.Include;
var
  s1, s2, s3: TSet;
begin
  s3 := s1 + s2;
  AssertTrue(s3.IsEmpty);

  AssertTrue(s1.AddAll(IntArray10) = Length(IntArray10));
  s3 := s1 + s2;
  AssertTrue(s3.Count = 10);
  AssertTrue(s3.ContainsAll(IntArray10));

  AssertTrue(s2.AddAll(IntArray11) = Length(IntArray11));
  s3 := s1 + s2;
  AssertTrue(s3.Count = 21);
  AssertTrue(s3.ContainsAll(IntArray21));
end;

procedure TLiteHashSetLPTest.Include1;
var
  s1, s2: TSet;
begin
  s1 := s1 + s2;
  AssertTrue(s1.IsEmpty);

  s2 := s1 + s2;
  AssertTrue(s2.IsEmpty);

  AssertTrue(s1.AddAll(IntArray10) = Length(IntArray10));
  s1 := s1 + s2;
  AssertTrue(s1.Count = 10);
  AssertTrue(s1.ContainsAll(IntArray10));

  s2 := s1 + s2;
  AssertTrue(s2.Count = 10);
  AssertTrue(s2.ContainsAll(IntArray10));

  s1.Clear;
  AssertTrue(s1.AddAll(IntArray11) = Length(IntArray11));
  s2 := s1 + s2;
  AssertTrue(s2.Count = 21);
  AssertTrue(s2.ContainsAll(IntArray21));
end;

procedure TLiteHashSetLPTest.Exclude;
var
  s1, s2, s3: TSet;
begin
  s3 := s1 - s2;
  AssertTrue(s3.IsEmpty);

  AssertTrue(s1.AddAll(IntArray10) = Length(IntArray10));
  s3 := s1 - s2;
  AssertTrue(s3.Count = 10);
  AssertTrue(s3.ContainsAll(IntArray10));

  AssertTrue(s2.AddAll(IntArray21) = Length(IntArray21));
  s3 := s2 - s1;
  AssertTrue(s3.Count = 11);
  AssertTrue(s3.ContainsAll(IntArray11));
end;

procedure TLiteHashSetLPTest.Exclude1;
var
  s1, s2: TSet;
begin
  s1 := s1 - s2;
  AssertTrue(s1.IsEmpty);

  s2 := s1 - s2;
  AssertTrue(s2.IsEmpty);

  AssertTrue(s1.AddAll(IntArray10) = Length(IntArray10));
  s1 := s1 - s2;
  AssertTrue(s1.Count = 10);
  AssertTrue(s1.ContainsAll(IntArray10));

  s2 := s1 - s2;
  AssertTrue(s2.Count = 10);
  AssertTrue(s2.ContainsAll(IntArray10));

  AssertTrue(s1.AddAll(IntArray11) = Length(IntArray11));
  s2 := s1 - s2;
  AssertTrue(s2.Count = 11);
  AssertTrue(s2.ContainsAll(IntArray11));
end;

procedure TLiteHashSetLPTest.Intersection;
var
  s1, s2, s3: TSet;
begin
  s3 := s1 * s2;
  AssertTrue(s3.IsEmpty);

  AssertTrue(s1.AddAll(IntArray10) = Length(IntArray10));
  s3 := s1 * s2;
  AssertTrue(s3.IsEmpty);

  AssertTrue(s2.AddAll(IntArray11) = Length(IntArray11));
  s3 := s1 * s2;
  AssertTrue(s3.IsEmpty);

  AssertTrue(s2.AddAll(IntArray10) = Length(IntArray10));
  s3 := s2 * s1;
  AssertTrue(s3.Count = 10);
  AssertTrue(s3.ContainsAll(IntArray10));
end;

procedure TLiteHashSetLPTest.Intersection1;
var
  s1, s2: TSet;
begin
  s1 := s1 * s2;
  AssertTrue(s1.IsEmpty);

  s2 := s1 * s2;
  AssertTrue(s2.IsEmpty);

  AssertTrue(s1.AddAll(IntArray10) = Length(IntArray10));
  s1 := s1 * s2;
  AssertTrue(s1.IsEmpty);

  AssertTrue(s2.AddAll(IntArray10) = Length(IntArray10));
  s2 := s1 * s2;
  AssertTrue(s2.IsEmpty);

  AssertTrue(s1.AddAll(IntArray11) = Length(IntArray11));
  AssertTrue(s2.AddAll(IntArray21) = Length(IntArray21));
  s2 := s1 * s2;
  AssertTrue(s2.Count = 11);
  AssertTrue(s2.ContainsAll(IntArray11));
end;

procedure TLiteHashSetLPTest.SymmetricDifference;
var
  s1, s2, s3: TSet;
begin
  s3 := s1 >< s2;
  AssertTrue(s3.IsEmpty);

  s1.AddAll(IntArray21);
  s2.AddAll(IntArray21);
  s3 := s1 >< s2;
  AssertTrue(s3.IsEmpty);

  s1.AddAll(IntArray21);
  s2.Clear;
  s3 := s1 >< s2;
  AssertTrue(s3.Count = 21);
  AssertTrue(s3.ContainsAll(IntArray21));

  s1.Clear;
  s1.AddAll(IntArray11);
  s2.AddAll(IntArray10);
  s3 := s1 >< s2;
  AssertTrue(s3.Count = 21);
  AssertTrue(s3.ContainsAll(IntArray21));
end;

procedure TLiteHashSetLPTest.SymmetricDifference1;
var
  s1, s2: TSet;
begin
  s1 := s1 >< s2;
  AssertTrue(s1.IsEmpty);

  s2 := s1 >< s2;
  AssertTrue(s2.IsEmpty);

  s1.AddAll(IntArray21);
  s2.AddAll(IntArray21);
  s1 := s1 >< s2;
  AssertTrue(s1.IsEmpty);

  s1.AddAll(IntArray21);
  s2.Clear;
  s2 := s1 >< s2;
  AssertTrue(s2.Count = 21);
  AssertTrue(s2.ContainsAll(IntArray21));

  s1.Clear;
  s2.Clear;
  s1.AddAll(IntArray11);
  s2.AddAll(IntArray10);
  s1 := s1 >< s2;
  AssertTrue(s1.Count = 21);
  AssertTrue(s1.ContainsAll(IntArray21));
end;

procedure TLiteHashSetLPTest.Equality;
var
  s1, s2: TSet;
begin
  AssertTrue(s1 = s1);
  AssertTrue(s2 = s2);
  AssertTrue(s1 = s2);

  s1.AddAll(IntArray21);
  s2.AddAll(IntArray21);
  AssertTrue(s2 = s2);
  AssertTrue(s2 = s1);
  AssertTrue(s1 = s2);
  AssertTrue(s1 = s1);

  s1.Remove(11);
  AssertFalse(s2 = s1);
  AssertFalse(s1 = s2);
end;

procedure TLiteHashSetLPTest.Contains;
var
  s1, s2: TSet;
begin
  AssertTrue(s1 <= s2);
  AssertTrue(s2 <= s1);

  s1.AddAll(IntArray21);
  s2.AddAll(IntArray21);

  AssertTrue(s2 <= s1);
  AssertTrue(s1 <= s2);
  AssertTrue(s2 <= s2);

  s1.Clear;
  s1.AddAll(IntArray11);
  AssertTrue(s1 <= s2);
  AssertFalse(s2 <= s1);
end;

procedure TLiteHashSetLPTest.TestIn;
var
  s: TSet;
begin
  AssertFalse(10 in s);
  s.AddAll(IntArray21);
  AssertTrue(10 in s);
  AssertFalse(0 in s);
  AssertFalse(22 in s);
end;

procedure TLiteHashSetLPTest.PassByValue;
  procedure Test(aSet: TSet);
  begin
    aSet.Add(2);
    aSet.Add(-5);
    aSet.Add(15);
    AssertTrue(aSet.NonEmpty);
  end;
var
  s: TSet;
begin
  s.EnsureCapacity(10);
  AssertTrue(s.IsEmpty);
  Test(s);
  AssertTrue(s.IsEmpty);
  s.Add(0);
  AssertFalse(s.Contains(2));
  AssertFalse(s.Contains(-5));
  AssertFalse(s.Contains(15));
end;


{ TLiteChainHashSetTest }

procedure TLiteChainHashSetTest.Add;
var
  s: TSet;
begin
  AssertTrue(s.Count = 0);
  AssertTrue(s.Add(1));
  AssertTrue(s.Count = 1);
  AssertTrue(s.Contains(1));
  AssertTrue(s.Add(51));
  AssertTrue(s.Count = 2);
  AssertTrue(s.Contains(51));
  AssertFalse(s.Add(1));
  AssertFalse(s.Add(51));
  AssertTrue(s.Count = 2);
  AssertTrue(s.Add(52));
  AssertTrue(s.Count = 3);
  AssertTrue(s.Contains(52));
end;

procedure TLiteChainHashSetTest.Add100;
var
  s: TSet;
  I: Integer;
begin
  for I := 1 to 100 do
    AssertTrue(s.Add(I));
  AssertTrue(s.Count = 100);
  for I := 1 to 100 do
    AssertFalse(s.Add(I));
  AssertTrue(s.Count = 100);
  for I := 1 to 100 do
    AssertTrue(s.Contains(I));
end;

procedure TLiteChainHashSetTest.AddArray;
var
  s: TSet;
begin
  AssertTrue(s.AddAll(IntArray21) = Length(IntArray21));
  AssertTrue(s.Count = Length(IntArray21));
  AssertTrue(s.ContainsAll(IntArray21));
end;

procedure TLiteChainHashSetTest.AddEnum;
var
  s: TSet;
  e: IIntEnumerable;
begin
  e := TIntArrayCursor.Create(TIntHelper.CreateCopy(IntArray21));
  AssertTrue(s.AddAll(e) = Length(IntArray21));
  AssertTrue(s.Count = Length(IntArray21));
  AssertTrue(s.ContainsAll(IntArray21));
end;

procedure TLiteChainHashSetTest.AddSet;
var
  s, s21: TSet;
begin
  AssertTrue(s.AddAll(IntArray11) = Length(IntArray11));
  AssertTrue(s.Count = Length(IntArray11));
  AssertTrue(s21.AddAll(IntArray21) = Length(IntArray21));
  AssertTrue(s21.Count = Length(IntArray21));
  AssertTrue(s.AddAll(s21) = 10);
  AssertTrue(s.Count = Length(IntArray21));
  AssertTrue(s.ContainsAll(IntArray21));
end;

procedure TLiteChainHashSetTest.AddSelf;
var
  s: TSet;
begin
  AssertTrue(s.AddAll(IntArray21) = Length(IntArray21));
  AssertTrue(s.AddAll(s) = 0);
  AssertTrue(s.Count = Length(IntArray21));
end;

procedure TLiteChainHashSetTest.Clear;
var
  s: TSet;
begin
  s.AddAll(IntArray11);
  AssertTrue(s.Count = 11);
  AssertTrue(s.Capacity >= 11);
  s.Clear;
  AssertTrue(s.Count = 0);
  AssertTrue(s.Capacity = 0);
end;

procedure TLiteChainHashSetTest.EnsureCapacity;
var
  s: TSet;
  c: SizeInt;
begin
  c := s.ExpandTreshold;
  s.EnsureCapacity(c + 1);
  AssertTrue(s.Capacity > c);
end;

procedure TLiteChainHashSetTest.TrimToFit;
var
  s: TSet;
  c: SizeInt;
begin
  s.AddAll(IntArray21);
  c := s.Capacity;
  s.EnsureCapacity(s.ExpandTreshold + 1);
  AssertTrue(s.Capacity > c);
  s.TrimToFit;
  AssertTrue(s.Capacity <= c);
end;

procedure TLiteChainHashSetTest.Remove;
var
  s: TSet;
begin
  s.AddAll(IntArray21);
  AssertTrue(s.Count = Length(IntArray21));
  AssertTrue(s.Remove(IntArray21[1]));
  AssertTrue(s.Count = Length(IntArray21) - 1);
  AssertTrue(s.Remove(IntArray21[High(IntArray21)]));
  AssertTrue(s.Count = Length(IntArray21) - 2);
  AssertFalse(s.Remove(-1));
  AssertTrue(s.Count = Length(IntArray21) - 2);
  AssertFalse(s.Remove(100));
  AssertTrue(s.Count = Length(IntArray21) - 2);
end;

procedure TLiteChainHashSetTest.RemoveArray;
var
  s: TSet;
begin
  s.AddAll(IntArray21);
  AssertTrue(s.RemoveAll(IntArray11) = 11);
  AssertTrue(s.Count = 10);
end;

procedure TLiteChainHashSetTest.RemoveEnum;
var
  s: TSet;
  e: IIntEnumerable;
begin
  e := TIntArrayCursor.Create(TIntHelper.CreateCopy(IntArray11));
  s.AddAll(IntArray21);
  AssertTrue(s.RemoveAll(e) = 11);
  AssertTrue(s.Count = 10);
end;

procedure TLiteChainHashSetTest.RemoveSet;
var
  s, s11: TSet;
begin
  s11.AddAll(IntArray11);
  s.AddAll(IntArray21);
  AssertTrue(s.RemoveAll(s11) = 11);
  AssertTrue(s.Count = 10);
end;

procedure TLiteChainHashSetTest.RemoveSelf;
var
  s: TSet;
begin
  s.AddAll(IntArray21);
  AssertTrue(s.RemoveAll(s) = 21);
  AssertTrue(s.IsEmpty);
end;

procedure TLiteChainHashSetTest.RemoveIfRegular;
var
  s: TSet;
begin
  s.AddAll(IntArray21);
  AssertTrue(s.RemoveIf(@IsEvenInt) = 10);
  AssertTrue(s.Count = 11);
  AssertTrue(s.ContainsAll(IntArray11));
end;

procedure TLiteChainHashSetTest.RemoveIfDelegated;
var
  s: TSet;
begin
  s.AddAll(IntArray21);
  AssertTrue(s.RemoveIf(@IsEven) = 10);
  AssertTrue(s.Count = 11);
  AssertTrue(s.ContainsAll(IntArray11));
end;

procedure TLiteChainHashSetTest.RemoveIfNested;
  function IsEvenInt(constref aValue: Integer): Boolean;
  begin
    Result := not Odd(aValue);
  end;
var
  s: TSet;
begin
  s.AddAll(IntArray21);
  AssertTrue(s.RemoveIf(@IsEvenInt) = 10);
  AssertTrue(s.Count = 11);
  AssertTrue(s.ContainsAll(IntArray11));
end;

procedure TLiteChainHashSetTest.ExtractIfRegular;
var
  s: TSet;
  a: TIntArray;
begin
  s.AddAll(IntArray21);
  a := s.ExtractIf(@IsEvenInt);
  AssertTrue(s.Count = 11);
  AssertTrue(s.ContainsAll(IntArray11));
  AssertTrue(Length(a) = 10);
  TIntHelper.Sort(a);
  AssertTrue(TIntHelper.Same(IntArray10, a));
end;

procedure TLiteChainHashSetTest.ExtractIfDelegated;
var
  s: TSet;
  a: TIntArray;
begin
  s.AddAll(IntArray21);
  a := s.ExtractIf(@IsEven);
  AssertTrue(s.Count = 11);
  AssertTrue(s.ContainsAll(IntArray11));
  AssertTrue(Length(a) = 10);
  TIntHelper.Sort(a);
  AssertTrue(TIntHelper.Same(IntArray10, a));
end;

procedure TLiteChainHashSetTest.ExtractIfNested;
  function IsEvenInt(constref aValue: Integer): Boolean;
  begin
    Result := not Odd(aValue);
  end;
var
  s: TSet;
  a: TIntArray;
begin
  s.AddAll(IntArray21);
  a := s.ExtractIf(@IsEvenInt);
  AssertTrue(s.Count = 11);
  AssertTrue(s.ContainsAll(IntArray11));
  AssertTrue(Length(a) = 10);
  TIntHelper.Sort(a);
  AssertTrue(TIntHelper.Same(IntArray10, a));
end;

procedure TLiteChainHashSetTest.Retain;
var
  s, s11: TSet;
begin
  s.AddAll(IntArray21);
  s.AddAll([-3, -2, -1, 0]);
  AssertTrue(s.Count = 25);
  s11.AddAll(IntArray11);
  s.RetainAll(s11);
  AssertTrue(s.Count = 11);
  AssertTrue(s.ContainsAll(s11));
end;

procedure TLiteChainHashSetTest.Retain_1;
var
  s, s11: TSet;
begin
  s.AddAll(IntArray10);
  s11.AddAll(IntArray11);
  s.RetainAll(s11);
  AssertTrue(s.IsEmpty);
end;

procedure TLiteChainHashSetTest.RetainSelf;
var
  s: TSet;
begin
  s.AddAll(IntArray21);
  s.RetainAll(s);
  AssertTrue(s.Count = 21);
  AssertTrue(s.ContainsAll(IntArray21));
end;

procedure TLiteChainHashSetTest.IsSuperset;
var
  s, s1: TSet;
begin
  s.AddAll(IntArray21);
  s1.AddAll(IntArray21);
  AssertTrue(s.IsSuperset(s1));
  AssertTrue(s1.IsSuperset(s));
  AssertTrue(s.IsSuperset(s));
  s1.Clear;
  s1.AddAll(IntArray11);
  AssertTrue(s.IsSuperset(s1));
  AssertFalse(s1.IsSuperset(s));
end;

procedure TLiteChainHashSetTest.IsSubset;
var
  s, s1: TSet;
begin
  s.AddAll(IntArray21);
  s1.AddAll(IntArray21);
  AssertTrue(s.IsSubset(s1));
  AssertTrue(s1.IsSubset(s));
  AssertTrue(s.IsSubset(s));
  s1.Clear;
  s1.AddAll(IntArray11);
  AssertTrue(s1.IsSubset(s));
  AssertFalse(s.IsSubset(s1));
end;

procedure TLiteChainHashSetTest.IsEqual;
var
  s, s1: TSet;
begin
  s.AddAll(IntArray21);
  s1.AddAll(IntArray21);
  AssertTrue(s.IsEqual(s));
  AssertTrue(s.IsEqual(s1));
  AssertTrue(s1.IsEqual(s));
  s1.Remove(21);
  AssertFalse(s.IsEqual(s1));
  AssertFalse(s1.IsEqual(s));
end;

procedure TLiteChainHashSetTest.Intersecting;
var
  s, s11: TSet;
begin
  s.AddAll(IntArray21);
  s11.AddAll(IntArray11);
  AssertTrue(s.Intersecting(s));
  AssertTrue(s.Intersecting(s11));
  AssertTrue(s11.Intersecting(s));
  s.Clear;
  s.AddAll(IntArray10);
  AssertFalse(s.Intersecting(s11));
  AssertFalse(s11.Intersecting(s));
end;

procedure TLiteChainHashSetTest.SymmetricSubtract;
var
  s, s1: TSet;
begin
  s.AddAll(IntArray21);
  s.SymmetricSubtract(s1);
  AssertTrue(s.Count = 21);
  AssertTrue(s.ContainsAll(IntArray21));
  s1.AddAll(IntArray21);
  s.Clear;
  s.SymmetricSubtract(s1);
  AssertTrue(s.ContainsAll(IntArray21));
  s.SymmetricSubtract(s1);
  AssertTrue(s.IsEmpty);
  s.AddAll(IntArray11);
  s1.Clear;
  s1.AddAll(IntArray10);
  s.SymmetricSubtract(s1);
  AssertTrue(s.Count = 21);
  AssertTrue(s.ContainsAll(IntArray21));
  s.Clear;
  s.AddAll(IntArray11);
  s.AddAll([-5, -4, -3, -2, -1, 0]);
  s1.AddAll([-5, -4, -3, -2, -1, 0]);
  s.SymmetricSubtract(s1);
  AssertTrue(s.Count = 21);
  AssertTrue(s.ContainsAll(IntArray21));
end;

procedure TLiteChainHashSetTest.Include;
var
  s1, s2, s3: TSet;
begin
  s3 := s1 + s2;
  AssertTrue(s3.IsEmpty);

  AssertTrue(s1.AddAll(IntArray10) = Length(IntArray10));
  s3 := s1 + s2;
  AssertTrue(s3.Count = 10);
  AssertTrue(s3.ContainsAll(IntArray10));

  AssertTrue(s2.AddAll(IntArray11) = Length(IntArray11));
  s3 := s1 + s2;
  AssertTrue(s3.Count = 21);
  AssertTrue(s3.ContainsAll(IntArray21));
end;

procedure TLiteChainHashSetTest.Include1;
var
  s1, s2: TSet;
begin
  s1 := s1 + s2;
  AssertTrue(s1.IsEmpty);

  s2 := s1 + s2;
  AssertTrue(s2.IsEmpty);

  AssertTrue(s1.AddAll(IntArray10) = Length(IntArray10));
  s1 := s1 + s2;
  AssertTrue(s1.Count = 10);
  AssertTrue(s1.ContainsAll(IntArray10));

  s2 := s1 + s2;
  AssertTrue(s2.Count = 10);
  AssertTrue(s2.ContainsAll(IntArray10));

  s1.Clear;
  AssertTrue(s1.AddAll(IntArray11) = Length(IntArray11));
  s2 := s1 + s2;
  AssertTrue(s2.Count = 21);
  AssertTrue(s2.ContainsAll(IntArray21));
end;

procedure TLiteChainHashSetTest.Exclude;
var
  s1, s2, s3: TSet;
begin
  s3 := s1 - s2;
  AssertTrue(s3.IsEmpty);

  AssertTrue(s1.AddAll(IntArray10) = Length(IntArray10));
  s3 := s1 - s2;
  AssertTrue(s3.Count = 10);
  AssertTrue(s3.ContainsAll(IntArray10));

  AssertTrue(s2.AddAll(IntArray21) = Length(IntArray21));
  s3 := s2 - s1;
  AssertTrue(s3.Count = 11);
  AssertTrue(s3.ContainsAll(IntArray11));
end;

procedure TLiteChainHashSetTest.Exclude1;
var
  s1, s2: TSet;
begin
  s1 := s1 - s2;
  AssertTrue(s1.IsEmpty);

  s2 := s1 - s2;
  AssertTrue(s2.IsEmpty);

  AssertTrue(s1.AddAll(IntArray10) = Length(IntArray10));
  s1 := s1 - s2;
  AssertTrue(s1.Count = 10);
  AssertTrue(s1.ContainsAll(IntArray10));

  s2 := s1 - s2;
  AssertTrue(s2.Count = 10);
  AssertTrue(s2.ContainsAll(IntArray10));

  AssertTrue(s1.AddAll(IntArray11) = Length(IntArray11));
  s2 := s1 - s2;
  AssertTrue(s2.Count = 11);
  AssertTrue(s2.ContainsAll(IntArray11));
end;

procedure TLiteChainHashSetTest.Intersection;
var
  s1, s2, s3: TSet;
begin
  s3 := s1 * s2;
  AssertTrue(s3.IsEmpty);

  AssertTrue(s1.AddAll(IntArray10) = Length(IntArray10));
  s3 := s1 * s2;
  AssertTrue(s3.IsEmpty);

  AssertTrue(s2.AddAll(IntArray11) = Length(IntArray11));
  s3 := s1 * s2;
  AssertTrue(s3.IsEmpty);

  AssertTrue(s2.AddAll(IntArray10) = Length(IntArray10));
  s3 := s2 * s1;
  AssertTrue(s3.Count = 10);
  AssertTrue(s3.ContainsAll(IntArray10));
end;

procedure TLiteChainHashSetTest.Intersection1;
var
  s1, s2: TSet;
begin
  s1 := s1 * s2;
  AssertTrue(s1.IsEmpty);

  s2 := s1 * s2;
  AssertTrue(s2.IsEmpty);

  AssertTrue(s1.AddAll(IntArray10) = Length(IntArray10));
  s1 := s1 * s2;
  AssertTrue(s1.IsEmpty);

  AssertTrue(s2.AddAll(IntArray10) = Length(IntArray10));
  s2 := s1 * s2;
  AssertTrue(s2.IsEmpty);

  AssertTrue(s1.AddAll(IntArray11) = Length(IntArray11));
  AssertTrue(s2.AddAll(IntArray21) = Length(IntArray21));
  s2 := s1 * s2;
  AssertTrue(s2.Count = 11);
  AssertTrue(s2.ContainsAll(IntArray11));
end;

procedure TLiteChainHashSetTest.SymmetricDifference;
var
  s1, s2, s3: TSet;
begin
  s3 := s1 >< s2;
  AssertTrue(s3.IsEmpty);

  s1.AddAll(IntArray21);
  s2.AddAll(IntArray21);
  s3 := s1 >< s2;
  AssertTrue(s3.IsEmpty);

  s1.AddAll(IntArray21);
  s2.Clear;
  s3 := s1 >< s2;
  AssertTrue(s3.Count = 21);
  AssertTrue(s3.ContainsAll(IntArray21));

  s1.Clear;
  s1.AddAll(IntArray11);
  s2.AddAll(IntArray10);
  s3 := s1 >< s2;
  AssertTrue(s3.Count = 21);
  AssertTrue(s3.ContainsAll(IntArray21));
end;

procedure TLiteChainHashSetTest.SymmetricDifference1;
var
  s1, s2: TSet;
begin
  s1 := s1 >< s2;
  AssertTrue(s1.IsEmpty);

  s2 := s1 >< s2;
  AssertTrue(s2.IsEmpty);

  s1.AddAll(IntArray21);
  s2.AddAll(IntArray21);
  s1 := s1 >< s2;
  AssertTrue(s1.IsEmpty);

  s1.AddAll(IntArray21);
  s2.Clear;
  s2 := s1 >< s2;
  AssertTrue(s2.Count = 21);
  AssertTrue(s2.ContainsAll(IntArray21));

  s1.Clear;
  s2.Clear;
  s1.AddAll(IntArray11);
  s2.AddAll(IntArray10);
  s1 := s1 >< s2;
  AssertTrue(s1.Count = 21);
  AssertTrue(s1.ContainsAll(IntArray21));
end;

procedure TLiteChainHashSetTest.Equality;
var
  s1, s2: TSet;
begin
  AssertTrue(s1 = s1);
  AssertTrue(s2 = s2);
  AssertTrue(s1 = s2);

  s1.AddAll(IntArray21);
  s2.AddAll(IntArray21);
  AssertTrue(s2 = s2);
  AssertTrue(s2 = s1);
  AssertTrue(s1 = s2);
  AssertTrue(s1 = s1);

  s1.Remove(11);
  AssertFalse(s2 = s1);
  AssertFalse(s1 = s2);
end;

procedure TLiteChainHashSetTest.Contains;
var
  s1, s2: TSet;
begin
  AssertTrue(s1 <= s2);
  AssertTrue(s2 <= s1);

  s1.AddAll(IntArray21);
  s2.AddAll(IntArray21);

  AssertTrue(s2 <= s1);
  AssertTrue(s1 <= s2);
  AssertTrue(s2 <= s2);

  s1.Clear;
  s1.AddAll(IntArray11);
  AssertTrue(s1 <= s2);
  AssertFalse(s2 <= s1);
end;

procedure TLiteChainHashSetTest.TestIn;
var
  s: TSet;
begin
  AssertFalse(10 in s);
  s.AddAll(IntArray21);
  AssertTrue(10 in s);
  AssertFalse(0 in s);
  AssertFalse(22 in s);
end;

procedure TLiteChainHashSetTest.PassByValue;
  procedure Test(aSet: TSet);
  begin
    aSet.Add(2);
    aSet.Add(-5);
    aSet.Add(15);
    AssertTrue(aSet.NonEmpty);
  end;
var
  s: TSet;
begin
  s.EnsureCapacity(10);
  AssertTrue(s.IsEmpty);
  Test(s);
  AssertTrue(s.IsEmpty);
  s.Add(0);
  AssertFalse(s.Contains(2));
  AssertFalse(s.Contains(-5));
  AssertFalse(s.Contains(15));
end;

{ TLiteEquatableHashSetTest }

procedure TLiteEquatableHashSetTest.Add;
var
  s: TSet;
begin
  AssertTrue(s.Count = 0);
  AssertTrue(s.Add(1));
  AssertTrue(s.Count = 1);
  AssertTrue(s.Contains(1));
  AssertTrue(s.Add(51));
  AssertTrue(s.Count = 2);
  AssertTrue(s.Contains(51));
  AssertFalse(s.Add(1));
  AssertFalse(s.Add(51));
  AssertTrue(s.Count = 2);
  AssertTrue(s.Add(52));
  AssertTrue(s.Count = 3);
  AssertTrue(s.Contains(52));
end;

procedure TLiteEquatableHashSetTest.Add100;
var
  s: TSet;
  I: Integer;
begin
  for I := 1 to 100 do
    AssertTrue(s.Add(I));
  AssertTrue(s.Count = 100);
  for I := 1 to 100 do
    AssertFalse(s.Add(I));
  AssertTrue(s.Count = 100);
  for I := 1 to 100 do
    AssertTrue(s.Contains(I));
end;

procedure TLiteEquatableHashSetTest.AddArray;
var
  s: TSet;
begin
  AssertTrue(s.AddAll(IntArray21) = Length(IntArray21));
  AssertTrue(s.Count = Length(IntArray21));
  AssertTrue(s.ContainsAll(IntArray21));
end;

procedure TLiteEquatableHashSetTest.AddEnum;
var
  s: TSet;
  e: IIntEnumerable;
begin
  e := TIntArrayCursor.Create(TIntHelper.CreateCopy(IntArray21));
  AssertTrue(s.AddAll(e) = Length(IntArray21));
  AssertTrue(s.Count = Length(IntArray21));
  AssertTrue(s.ContainsAll(IntArray21));
end;

procedure TLiteEquatableHashSetTest.AddSet;
var
  s, s21: TSet;
begin
  AssertTrue(s.AddAll(IntArray11) = Length(IntArray11));
  AssertTrue(s.Count = Length(IntArray11));
  AssertTrue(s21.AddAll(IntArray21) = Length(IntArray21));
  AssertTrue(s21.Count = Length(IntArray21));
  AssertTrue(s.AddAll(s21) = 10);
  AssertTrue(s.Count = Length(IntArray21));
  AssertTrue(s.ContainsAll(IntArray21));
end;

procedure TLiteEquatableHashSetTest.AddSelf;
var
  s: TSet;
begin
  AssertTrue(s.AddAll(IntArray21) = Length(IntArray21));
  AssertTrue(s.AddAll(s) = 0);
  AssertTrue(s.Count = Length(IntArray21));
end;

procedure TLiteEquatableHashSetTest.Clear;
var
  s: TSet;
begin
  s.AddAll(IntArray11);
  AssertTrue(s.Count = 11);
  AssertTrue(s.Capacity >= 11);
  s.Clear;
  AssertTrue(s.Count = 0);
  AssertTrue(s.Capacity = 0);
end;

procedure TLiteEquatableHashSetTest.EnsureCapacity;
var
  s: TSet;
  c: SizeInt;
begin
  c := s.ExpandTreshold;
  s.EnsureCapacity(c + 1);
  AssertTrue(s.Capacity > c);
end;

procedure TLiteEquatableHashSetTest.TrimToFit;
var
  s: TSet;
  c: SizeInt;
begin
  s.AddAll(IntArray21);
  c := s.Capacity;
  s.EnsureCapacity(s.ExpandTreshold + 1);
  AssertTrue(s.Capacity > c);
  s.TrimToFit;
  AssertTrue(s.Capacity <= c);
end;

procedure TLiteEquatableHashSetTest.Remove;
var
  s: TSet;
begin
  s.AddAll(IntArray21);
  AssertTrue(s.Count = Length(IntArray21));
  AssertTrue(s.Remove(IntArray21[1]));
  AssertTrue(s.Count = Length(IntArray21) - 1);
  AssertTrue(s.Remove(IntArray21[High(IntArray21)]));
  AssertTrue(s.Count = Length(IntArray21) - 2);
  AssertFalse(s.Remove(-1));
  AssertTrue(s.Count = Length(IntArray21) - 2);
  AssertFalse(s.Remove(100));
  AssertTrue(s.Count = Length(IntArray21) - 2);
end;

procedure TLiteEquatableHashSetTest.RemoveArray;
var
  s: TSet;
begin
  s.AddAll(IntArray21);
  AssertTrue(s.RemoveAll(IntArray11) = 11);
  AssertTrue(s.Count = 10);
end;

procedure TLiteEquatableHashSetTest.RemoveEnum;
var
  s: TSet;
  e: IIntEnumerable;
begin
  e := TIntArrayCursor.Create(TIntHelper.CreateCopy(IntArray11));
  s.AddAll(IntArray21);
  AssertTrue(s.RemoveAll(e) = 11);
  AssertTrue(s.Count = 10);
end;

procedure TLiteEquatableHashSetTest.RemoveSet;
var
  s, s11: TSet;
begin
  s11.AddAll(IntArray11);
  s.AddAll(IntArray21);
  AssertTrue(s.RemoveAll(s11) = 11);
  AssertTrue(s.Count = 10);
end;

procedure TLiteEquatableHashSetTest.RemoveSelf;
var
  s: TSet;
begin
  s.AddAll(IntArray21);
  AssertTrue(s.RemoveAll(s) = 21);
  AssertTrue(s.IsEmpty);
end;

procedure TLiteEquatableHashSetTest.RemoveIfRegular;
var
  s: TSet;
begin
  s.AddAll(IntArray21);
  AssertTrue(s.RemoveIf(@IsEvenInt) = 10);
  AssertTrue(s.Count = 11);
  AssertTrue(s.ContainsAll(IntArray11));
end;

procedure TLiteEquatableHashSetTest.RemoveIfDelegated;
var
  s: TSet;
begin
  s.AddAll(IntArray21);
  AssertTrue(s.RemoveIf(@IsEven) = 10);
  AssertTrue(s.Count = 11);
  AssertTrue(s.ContainsAll(IntArray11));
end;

procedure TLiteEquatableHashSetTest.RemoveIfNested;
  function IsEvenInt(constref aValue: Integer): Boolean;
  begin
    Result := not Odd(aValue);
  end;
var
  s: TSet;
begin
  s.AddAll(IntArray21);
  AssertTrue(s.RemoveIf(@IsEvenInt) = 10);
  AssertTrue(s.Count = 11);
  AssertTrue(s.ContainsAll(IntArray11));
end;

procedure TLiteEquatableHashSetTest.ExtractIfRegular;
var
  s: TSet;
  a: TIntArray;
begin
  s.AddAll(IntArray21);
  a := s.ExtractIf(@IsEvenInt);
  AssertTrue(s.Count = 11);
  AssertTrue(s.ContainsAll(IntArray11));
  AssertTrue(Length(a) = 10);
  TIntHelper.Sort(a);
  AssertTrue(TIntHelper.Same(IntArray10, a));
end;

procedure TLiteEquatableHashSetTest.ExtractIfDelegated;
var
  s: TSet;
  a: TIntArray;
begin
  s.AddAll(IntArray21);
  a := s.ExtractIf(@IsEven);
  AssertTrue(s.Count = 11);
  AssertTrue(s.ContainsAll(IntArray11));
  AssertTrue(Length(a) = 10);
  TIntHelper.Sort(a);
  AssertTrue(TIntHelper.Same(IntArray10, a));
end;

procedure TLiteEquatableHashSetTest.ExtractIfNested;
  function IsEvenInt(constref aValue: Integer): Boolean;
  begin
    Result := not Odd(aValue);
  end;
var
  s: TSet;
  a: TIntArray;
begin
  s.AddAll(IntArray21);
  a := s.ExtractIf(@IsEvenInt);
  AssertTrue(s.Count = 11);
  AssertTrue(s.ContainsAll(IntArray11));
  AssertTrue(Length(a) = 10);
  TIntHelper.Sort(a);
  AssertTrue(TIntHelper.Same(IntArray10, a));
end;

procedure TLiteEquatableHashSetTest.Retain;
var
  s, s11: TSet;
begin
  s.AddAll(IntArray21);
  s.AddAll([-3, -2, -1, 0]);
  AssertTrue(s.Count = 25);
  s11.AddAll(IntArray11);
  s.RetainAll(s11);
  AssertTrue(s.Count = 11);
  AssertTrue(s.ContainsAll(s11));
end;

procedure TLiteEquatableHashSetTest.Retain_1;
var
  s, s11: TSet;
begin
  s.AddAll(IntArray10);
  s11.AddAll(IntArray11);
  s.RetainAll(s11);
  AssertTrue(s.IsEmpty);
end;

procedure TLiteEquatableHashSetTest.RetainSelf;
var
  s: TSet;
begin
  s.AddAll(IntArray21);
  s.RetainAll(s);
  AssertTrue(s.Count = 21);
  AssertTrue(s.ContainsAll(IntArray21));
end;

procedure TLiteEquatableHashSetTest.IsSuperset;
var
  s, s1: TSet;
begin
  s.AddAll(IntArray21);
  s1.AddAll(IntArray21);
  AssertTrue(s.IsSuperset(s1));
  AssertTrue(s1.IsSuperset(s));
  AssertTrue(s.IsSuperset(s));
  s1.Clear;
  s1.AddAll(IntArray11);
  AssertTrue(s.IsSuperset(s1));
  AssertFalse(s1.IsSuperset(s));
end;

procedure TLiteEquatableHashSetTest.IsSubset;
var
  s, s1: TSet;
begin
  s.AddAll(IntArray21);
  s1.AddAll(IntArray21);
  AssertTrue(s.IsSubset(s1));
  AssertTrue(s1.IsSubset(s));
  AssertTrue(s.IsSubset(s));
  s1.Clear;
  s1.AddAll(IntArray11);
  AssertTrue(s1.IsSubset(s));
  AssertFalse(s.IsSubset(s1));
end;

procedure TLiteEquatableHashSetTest.IsEqual;
var
  s, s1: TSet;
begin
  s.AddAll(IntArray21);
  s1.AddAll(IntArray21);
  AssertTrue(s.IsEqual(s));
  AssertTrue(s.IsEqual(s1));
  AssertTrue(s1.IsEqual(s));
  s1.Remove(21);
  AssertFalse(s.IsEqual(s1));
  AssertFalse(s1.IsEqual(s));
end;

procedure TLiteEquatableHashSetTest.Intersecting;
var
  s, s11: TSet;
begin
  s.AddAll(IntArray21);
  s11.AddAll(IntArray11);
  AssertTrue(s.Intersecting(s));
  AssertTrue(s.Intersecting(s11));
  AssertTrue(s11.Intersecting(s));
  s.Clear;
  s.AddAll(IntArray10);
  AssertFalse(s.Intersecting(s11));
  AssertFalse(s11.Intersecting(s));
end;

procedure TLiteEquatableHashSetTest.SymmetricSubtract;
var
  s, s1: TSet;
begin
  s.AddAll(IntArray21);
  s.SymmetricSubtract(s1);
  AssertTrue(s.Count = 21);
  AssertTrue(s.ContainsAll(IntArray21));
  s1.AddAll(IntArray21);
  s.Clear;
  s.SymmetricSubtract(s1);
  AssertTrue(s.ContainsAll(IntArray21));
  s.SymmetricSubtract(s1);
  AssertTrue(s.IsEmpty);
  s.AddAll(IntArray11);
  s1.Clear;
  s1.AddAll(IntArray10);
  s.SymmetricSubtract(s1);
  AssertTrue(s.Count = 21);
  AssertTrue(s.ContainsAll(IntArray21));
  s.Clear;
  s.AddAll(IntArray11);
  s.AddAll([-5, -4, -3, -2, -1, 0]);
  s1.AddAll([-5, -4, -3, -2, -1, 0]);
  s.SymmetricSubtract(s1);
  AssertTrue(s.Count = 21);
  AssertTrue(s.ContainsAll(IntArray21));
end;

procedure TLiteEquatableHashSetTest.Include;
var
  s1, s2, s3: TSet;
begin
  s3 := s1 + s2;
  AssertTrue(s3.IsEmpty);

  AssertTrue(s1.AddAll(IntArray10) = Length(IntArray10));
  s3 := s1 + s2;
  AssertTrue(s3.Count = 10);
  AssertTrue(s3.ContainsAll(IntArray10));

  AssertTrue(s2.AddAll(IntArray11) = Length(IntArray11));
  s3 := s1 + s2;
  AssertTrue(s3.Count = 21);
  AssertTrue(s3.ContainsAll(IntArray21));
end;

procedure TLiteEquatableHashSetTest.Include1;
var
  s1, s2: TSet;
begin
  s1 := s1 + s2;
  AssertTrue(s1.IsEmpty);

  s2 := s1 + s2;
  AssertTrue(s2.IsEmpty);

  AssertTrue(s1.AddAll(IntArray10) = Length(IntArray10));
  s1 := s1 + s2;
  AssertTrue(s1.Count = 10);
  AssertTrue(s1.ContainsAll(IntArray10));

  s2 := s1 + s2;
  AssertTrue(s2.Count = 10);
  AssertTrue(s2.ContainsAll(IntArray10));

  s1.Clear;
  AssertTrue(s1.AddAll(IntArray11) = Length(IntArray11));
  s2 := s1 + s2;
  AssertTrue(s2.Count = 21);
  AssertTrue(s2.ContainsAll(IntArray21));
end;

procedure TLiteEquatableHashSetTest.Exclude;
var
  s1, s2, s3: TSet;
begin
  s3 := s1 - s2;
  AssertTrue(s3.IsEmpty);

  AssertTrue(s1.AddAll(IntArray10) = Length(IntArray10));
  s3 := s1 - s2;
  AssertTrue(s3.Count = 10);
  AssertTrue(s3.ContainsAll(IntArray10));

  AssertTrue(s2.AddAll(IntArray21) = Length(IntArray21));
  s3 := s2 - s1;
  AssertTrue(s3.Count = 11);
  AssertTrue(s3.ContainsAll(IntArray11));
end;

procedure TLiteEquatableHashSetTest.Exclude1;
var
  s1, s2: TSet;
begin
  s1 := s1 - s2;
  AssertTrue(s1.IsEmpty);

  s2 := s1 - s2;
  AssertTrue(s2.IsEmpty);

  AssertTrue(s1.AddAll(IntArray10) = Length(IntArray10));
  s1 := s1 - s2;
  AssertTrue(s1.Count = 10);
  AssertTrue(s1.ContainsAll(IntArray10));

  s2 := s1 - s2;
  AssertTrue(s2.Count = 10);
  AssertTrue(s2.ContainsAll(IntArray10));

  AssertTrue(s1.AddAll(IntArray11) = Length(IntArray11));
  s2 := s1 - s2;
  AssertTrue(s2.Count = 11);
  AssertTrue(s2.ContainsAll(IntArray11));
end;

procedure TLiteEquatableHashSetTest.Intersection;
var
  s1, s2, s3: TSet;
begin
  s3 := s1 * s2;
  AssertTrue(s3.IsEmpty);

  AssertTrue(s1.AddAll(IntArray10) = Length(IntArray10));
  s3 := s1 * s2;
  AssertTrue(s3.IsEmpty);

  AssertTrue(s2.AddAll(IntArray11) = Length(IntArray11));
  s3 := s1 * s2;
  AssertTrue(s3.IsEmpty);

  AssertTrue(s2.AddAll(IntArray10) = Length(IntArray10));
  s3 := s2 * s1;
  AssertTrue(s3.Count = 10);
  AssertTrue(s3.ContainsAll(IntArray10));
end;

procedure TLiteEquatableHashSetTest.Intersection1;
var
  s1, s2: TSet;
begin
  s1 := s1 * s2;
  AssertTrue(s1.IsEmpty);

  s2 := s1 * s2;
  AssertTrue(s2.IsEmpty);

  AssertTrue(s1.AddAll(IntArray10) = Length(IntArray10));
  s1 := s1 * s2;
  AssertTrue(s1.IsEmpty);

  AssertTrue(s2.AddAll(IntArray10) = Length(IntArray10));
  s2 := s1 * s2;
  AssertTrue(s2.IsEmpty);

  AssertTrue(s1.AddAll(IntArray11) = Length(IntArray11));
  AssertTrue(s2.AddAll(IntArray21) = Length(IntArray21));
  s2 := s1 * s2;
  AssertTrue(s2.Count = 11);
  AssertTrue(s2.ContainsAll(IntArray11));
end;

procedure TLiteEquatableHashSetTest.SymmetricDifference;
var
  s1, s2, s3: TSet;
begin
  s3 := s1 >< s2;
  AssertTrue(s3.IsEmpty);

  s1.AddAll(IntArray21);
  s2.AddAll(IntArray21);
  s3 := s1 >< s2;
  AssertTrue(s3.IsEmpty);

  s1.AddAll(IntArray21);
  s2.Clear;
  s3 := s1 >< s2;
  AssertTrue(s3.Count = 21);
  AssertTrue(s3.ContainsAll(IntArray21));

  s1.Clear;
  s1.AddAll(IntArray11);
  s2.AddAll(IntArray10);
  s3 := s1 >< s2;
  AssertTrue(s3.Count = 21);
  AssertTrue(s3.ContainsAll(IntArray21));
end;

procedure TLiteEquatableHashSetTest.SymmetricDifference1;
var
  s1, s2: TSet;
begin
  s1 := s1 >< s2;
  AssertTrue(s1.IsEmpty);

  s2 := s1 >< s2;
  AssertTrue(s2.IsEmpty);

  s1.AddAll(IntArray21);
  s2.AddAll(IntArray21);
  s1 := s1 >< s2;
  AssertTrue(s1.IsEmpty);

  s1.AddAll(IntArray21);
  s2.Clear;
  s2 := s1 >< s2;
  AssertTrue(s2.Count = 21);
  AssertTrue(s2.ContainsAll(IntArray21));

  s1.Clear;
  s2.Clear;
  s1.AddAll(IntArray11);
  s2.AddAll(IntArray10);
  s1 := s1 >< s2;
  AssertTrue(s1.Count = 21);
  AssertTrue(s1.ContainsAll(IntArray21));
end;

procedure TLiteEquatableHashSetTest.Equality;
var
  s1, s2: TSet;
begin
  AssertTrue(s1 = s1);
  AssertTrue(s2 = s2);
  AssertTrue(s1 = s2);

  s1.AddAll(IntArray21);
  s2.AddAll(IntArray21);
  AssertTrue(s2 = s2);
  AssertTrue(s2 = s1);
  AssertTrue(s1 = s2);
  AssertTrue(s1 = s1);

  s1.Remove(11);
  AssertFalse(s2 = s1);
  AssertFalse(s1 = s2);
end;

procedure TLiteEquatableHashSetTest.Contains;
var
  s1, s2: TSet;
begin
  AssertTrue(s1 <= s2);
  AssertTrue(s2 <= s1);

  s1.AddAll(IntArray21);
  s2.AddAll(IntArray21);

  AssertTrue(s2 <= s1);
  AssertTrue(s1 <= s2);
  AssertTrue(s2 <= s2);

  s1.Clear;
  s1.AddAll(IntArray11);
  AssertTrue(s1 <= s2);
  AssertFalse(s2 <= s1);
end;

procedure TLiteEquatableHashSetTest.TestIn;
var
  s: TSet;
begin
  AssertFalse(10 in s);
  s.AddAll(IntArray21);
  AssertTrue(10 in s);
  AssertFalse(0 in s);
  AssertFalse(22 in s);
end;

procedure TLiteEquatableHashSetTest.PassByValue;
  procedure Test(aSet: TSet);
  begin
    aSet.Add(2);
    aSet.Add(-5);
    aSet.Add(15);
    AssertTrue(aSet.NonEmpty);
  end;
var
  s: TSet;
begin
  s.EnsureCapacity(10);
  AssertTrue(s.IsEmpty);
  Test(s);
  AssertTrue(s.IsEmpty);
  s.Add(0);
  AssertFalse(s.Contains(2));
  AssertFalse(s.Contains(-5));
  AssertFalse(s.Contains(15));
end;

{ TGDisjointSetUnionTest }

procedure TGDisjointSetUnionTest.Add;
var
  d: TDsu;
begin
  AssertTrue(d.Count = 0);
  AssertTrue(d.Add('str0') = 0);
  AssertTrue(d.Count = 1);
  AssertTrue(d.Contains('str0'));
  AssertTrue(d.Add('str1') = 1);
  AssertTrue(d.Count = 2);
  AssertTrue(d.Contains('str1'));
  AssertTrue('index = ' + d.Add('str0').ToString, d.Add('str0') = -1);
  AssertTrue(d.Add('str1') = -1);
  AssertTrue(d.Count = 2);
  AssertTrue(d.Add('str2') = 2);
  AssertTrue(d.Count = 3);
  AssertTrue(d.Contains('str2'));
end;

procedure TGDisjointSetUnionTest.Clear;
var
  d: TDsu;
  I: SizeInt;
begin
  for I in [1..50] do
    d.Add('str' + I.ToString);
  AssertTrue(d.Count = 50);
  AssertTrue(d.Capacity >= 50);
  d.Clear;
  AssertTrue(d.Count = 0);
  AssertTrue(d.Capacity = 0);
end;

procedure TGDisjointSetUnionTest.EnsureCapacity;
var
  d: TDsu;
begin
  AssertTrue(d.Capacity = 0);
  d.EnsureCapacity(100);
  AssertTrue(d.Capacity >= 100);
end;

procedure TGDisjointSetUnionTest.GetEnumerator;
var
  d: TDsu;
  I: SizeInt;
  s: string;
begin
  for I in [1..50] do
    d.Add('str' + I.ToString);
  I := 1;
  for s in d do
    begin
      AssertTrue(s = 'str' + I.ToString);
      Inc(I);
    end;
end;

procedure TGDisjointSetUnionTest.ToArray;
var
  d: TDsu;
  I: SizeInt;
  s: string;
  a: TStringArray;
begin
  for I in [1..50] do
    d.Add('str' + I.ToString);
  a := d.ToArray;
  I := 1;
  for s in a do
    begin
      AssertTrue(s = 'str' + I.ToString);
      Inc(I);
    end;
end;

procedure TGDisjointSetUnionTest.Join;
var
  d: TDsu;
begin
  AssertFalse(d.Join('str0', 'str0'));
  AssertTrue(d.Join('str0', 'str1'));
  AssertTrue(d.Contains('str1'));
  AssertTrue(d.Count = 2);
  AssertFalse(d.Join('str0', 'str1'));
  AssertTrue(d.Add('str2') = 2);
  AssertTrue(d.Join('str0', 'str2'));
  AssertTrue(d.Count = 3);
  AssertTrue(d.Contains('str2'));
end;

procedure TGDisjointSetUnionTest.JoinI;
var
  d: TDsu;
  I, J: SizeInt;
begin
  I := d.Add('str0');
  AssertFalse(d.JoinI(I, I));
  J := d.Add('str1');
  AssertTrue(d.JoinI(I, J));
  AssertFalse(d.JoinI(I, J));
  J := d.Add('str2');
  AssertTrue(d.JoinI(I, J));
  AssertFalse(d.JoinI(I, J));
end;

procedure TGDisjointSetUnionTest.Tag;
var
  d: TDsu;
  I, J: SizeInt;
begin
  I := d.Add('str0');
  AssertTrue(d.Tag('str0') = I);
  J := d.Add('str1');
  AssertTrue(d.Tag('str1') = J);
  AssertTrue(d.Join('str0', 'str1'));
  I := d.Tag('str0');
  J := d.Tag('str1');
  AssertTrue(I = J);
end;

procedure TGDisjointSetUnionTest.TagI;
var
  d: TDsu;
  I, J: SizeInt;
begin
  I := d.Add('str0');
  AssertTrue(d.TagI(I) = I);
  J := d.Add('str1');
  AssertTrue(d.TagI(J) = J);
  AssertTrue(d.JoinI(I, J));
  I := d.TagI(I);
  J := d.TagI(J);
  AssertTrue(I = J);
end;

procedure TGDisjointSetUnionTest.InSameSet;
var
  d: TDsu;
begin
  AssertFalse(d.InSameSet('str0', 'str1'));
  AssertTrue(d.Join('str0', 'str1'));
  AssertTrue(d.InSameSet('str0', 'str1'));
  AssertFalse(d.InSameSet('str0', 'str2'));
  AssertTrue(d.Join('str1', 'str2'));
  AssertTrue(d.InSameSet('str0', 'str2'));
end;

procedure TGDisjointSetUnionTest.InSameSetI;
var
  d: TDsu;
  I, J: SizeInt;
begin
  I := d.Add('str0');
  J := d.Add('str1');
  AssertFalse(d.InSameSetI(I, J));
  AssertTrue(d.JoinI(I, J));
  AssertTrue(d.InSameSetI(I, J));
end;

procedure TGDisjointSetUnionTest.InDiffSets;
var
  d: TDsu;
begin
  AssertTrue(d.InDiffSets('str0', 'str1'));
  AssertTrue(d.Join('str0', 'str1'));
  AssertFalse(d.InDiffSets('str0', 'str1'));
  AssertTrue(d.InDiffSets('str0', 'str2'));
  AssertTrue(d.Join('str1', 'str2'));
  AssertFalse(d.InDiffSets('str0', 'str2'));
end;

procedure TGDisjointSetUnionTest.InDiffSetsI;
var
  d: TDsu;
  I, J: SizeInt;
begin
  I := d.Add('str0');
  J := d.Add('str1');
  AssertTrue(d.InDiffSetsI(I, J));
  AssertTrue(d.JoinI(I, J));
  AssertFalse(d.InDiffSetsI(I, J));
end;

procedure TGDisjointSetUnionTest.Reset;
var
  d: TDsu;
begin
  AssertTrue(d.Join('str0', 'str1'));
  AssertTrue(d.Join('str1', 'str2'));
  AssertTrue(d.InSameSet('str0', 'str1'));
  AssertTrue(d.InSameSet('str1', 'str2'));
  AssertTrue(d.InSameSet('str0', 'str2'));
  d.Reset;
  AssertFalse(d.InSameSet('str0', 'str1'));
  AssertFalse(d.InSameSet('str1', 'str2'));
  AssertFalse(d.InSameSet('str0', 'str2'));
end;

{ THashSetFGTest.TWorker }

constructor THashSetFGTest.TWorker.Create(aSet: TSet; const aData: TArray; aIndex: Integer; aFlagValue,
  aResult: PInteger);
begin
  inherited Create(True);
  FreeOnTerminate := True;
  FSet := aSet;
  FData := aData;
  FIndex := aIndex;
  FFlag := aFlagValue;
  FResult := aResult;
end;

{ THashSetFGTest.TAdder }

procedure THashSetFGTest.TAdder.Execute;
var
  I: Integer;
  Added: Integer = 0;
begin
  for I in FData do
    Added += Ord(FSet.Add(I));
  FResult[FIndex] := Added;
  InterlockedIncrement(FFlag^);
  Terminate;
end;

{ THashSetFGTest.TRemover }

procedure THashSetFGTest.TRemover.Execute;
var
  I: Integer;
  Removed: Integer = 0;
begin
  for I in FData do
    Removed += Ord(FSet.Remove(I));
  FResult[FIndex] := Removed;
  InterlockedIncrement(FFlag^);
  Terminate;
end;

{ THashSetFGTest }

procedure THashSetFGTest.SimpleAdd;
var
  s: TSet;
  ref: TSetRef;
begin
  s := {%H-}ref;
  AssertTrue(s.Count = 0);

  AssertTrue(s.Add(-1));
  AssertTrue(s.Count = 1);
  AssertTrue(s.Contains(-1));

  AssertTrue(s.Add(0));
  AssertTrue(s.Count = 2);
  AssertTrue(s.Contains(0));

  AssertTrue(s.Add(1));
  AssertTrue(s.Count = 3);
  AssertTrue(s.Contains(1));

  AssertFalse(s.Add(0));
  AssertTrue(s.Count = 3);

  AssertFalse(s.Add(-1));
  AssertTrue(s.Count = 3);
end;

procedure THashSetFGTest.SimpleRemove;
var
  s: TSet;
  ref: TSetRef;
  I: Integer;
begin
  s := {%H-}ref;
  AssertTrue(s.Count = 0);

  for I := 0 to 99 do
    AssertTrue(s.Add(I));
  AssertTrue(s.Count = 100);

  for I := 100 to 150 do
    AssertFalse(s.Remove(I));
  AssertTrue(s.Count = 100);

  for I := 0 to 99 do
    AssertTrue(s.Remove(I));
  AssertTrue(s.Count = 0);
end;

procedure THashSetFGTest.Add;
const
  TestSize    = 100000;
  ThreadCount = 4;
  function CreateTestArray: TArray;
  begin
    Result := THelper.CreateRange(1, TestSize);
    THelper.RandomShuffle(Result);
  end;
var
  s: TSet;
  TestArray: array[0..Pred(ThreadCount)] of TArray;
  ThreadArray: array[0..Pred(ThreadCount)] of TAdder;
  Results: array[0..Pred(ThreadCount)] of Integer;
  I, Total: Integer;
  Finished: Integer = 0;
begin
  for I := 0 to Pred(ThreadCount) do
    TestArray[I] := CreateTestArray;
  s := TSet.Create;
  for I := 0 to Pred(ThreadCount) do
    ThreadArray[I] := TAdder.Create(s, TestArray[I], I, @Finished, @Results[0]);
  for I := 0 to Pred(ThreadCount) do
    ThreadArray[I].Start;
  while Finished < ThreadCount do
    Sleep(50);
  AssertTrue(s.Count = TestSize);
  s.Free;
  Total := 0;
  for I in Results do
    Total += I;
  AssertTrue(Total = TestSize);
end;

procedure THashSetFGTest.Remove;
const
  TestSize    = 100000;
  ThreadCount = 4;
  function CreateTestArray: TArray;
  begin
    Result := THelper.CreateRange(1, TestSize);
    THelper.RandomShuffle(Result);
  end;
var
  s: TSet;
  TestArray: array[0..Pred(ThreadCount)] of TArray;
  ThreadArray: array[0..Pred(ThreadCount)] of TRemover;
  Results: array[0..Pred(ThreadCount)] of Integer;
  I, Total: Integer;
  Finished: Integer = 0;
begin
  s := TSet.Create(TestSize);
  for I := 1 to TestSize do
    s.Add(I);
  for I := 0 to Pred(ThreadCount) do
    TestArray[I] := CreateTestArray;
  for I := 0 to Pred(ThreadCount) do
    ThreadArray[I] := TRemover.Create(s, TestArray[I], I, @Finished, @Results[0]);
  for I := 0 to Pred(ThreadCount) do
    ThreadArray[I].Start;
  while Finished < ThreadCount do
    Sleep(50);
  AssertTrue(s.Count = 0);
  s.Free;
  Total := 0;
  for I in Results do
    Total += I;
  AssertTrue(Total = TestSize);
end;

initialization
  RegisterTest(THashSetLPTest);
  RegisterTest(THashSetLPTTest);
  RegisterTest(THashSetQPTest);
  RegisterTest(TChainHashSetTest);
  RegisterTest(TOrdHashSetTest);
  RegisterTest(TLiteHashSetLPTest);
  RegisterTest(TLiteChainHashSetTest);
  RegisterTest(TLiteEquatableHashSetTest);
  RegisterTest(TGDisjointSetUnionTest);
  RegisterTest(THashSetFGTest);
end.

