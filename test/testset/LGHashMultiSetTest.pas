unit LGHashMultiSetTest;

{$mode objfpc}{$H+}
{$MODESWITCH NESTEDPROCVARS}

interface
uses
  Classes, SysUtils, fpcunit, testregistry,
  LGUtils,
  LGAbstractContainer,
  LGHashMultiSet,
  LGHashSet,
  LGArrayHelpers;

type

  TIntHelper      = specialize TGComparableArrayHelper<Integer>;
  TIntArrayCursor = specialize TGArrayCursor<Integer>;
  IIntEnumerable  = specialize IGEnumerable<Integer>;
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

  TObjArray = specialize TGArray<TTestObj>;

  THashMultisetLPTest = class(TTestCase)
  private
  type
    TMultiSet        = class(specialize TGHashMultiSetLP<Integer>);
    TAutoMultiSet    = specialize TGAutoRef<TMultiSet>;
    TObjMultiSet     = specialize TGObjHashMultiSetLP<TTestObj>;
    TAutoObjMultiSet = specialize TGAutoRef<TObjMultiSet>;

    function IsEven(const aValue: Integer): Boolean;
    function ObjIsEven(const aObj: TTestObj): Boolean;
  published
    procedure TestCreate;
    procedure CreateArray11;
    procedure CreateEnum11;
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
    procedure Intersect;
    procedure Join;
    procedure ArithmeticAdd;
    procedure ArithmeticSubtract;
    procedure SymmetricSubtract;

    procedure ObjectMultiSetRemove;
    procedure ObjectMultiSetRemoveArray;
    procedure ObjectMultiSetRemoveEnum;
    procedure ObjectMultiSetRemoveSelf;

    procedure ObjectMultiSetRemoveIfRegular;
    procedure ObjectMultiSetRemoveIfDelegated;
    procedure ObjectMultiSetRemoveIfNested;

    procedure ObjectMultiSetExtractIfRegular;
    procedure ObjectMultiSetExtractIfDelegated;
    procedure ObjectMultiSetExtractIfNested;

    procedure ObjectMultiSetRetain;

    procedure ObjectMultiSetIntersect;
    procedure ObjectMultiSetArithmeticSubtract;
    procedure ObjectMultiSetSymmetricSubtract;
  end;

  THashMultisetLPTTest = class(TTestCase)
  private
  type
    TMultiSet        = class(specialize TGHashMultiSetLPT<Integer>);
    TAutoMultiSet    = specialize TGAutoRef<TMultiSet>;
    TObjMultiSet     = specialize TGObjHashMultiSetLPT<TTestObj>;
    TAutoObjMultiSet = specialize TGAutoRef<TObjMultiSet>;

    function IsEven(const aValue: Integer): Boolean;
    function ObjIsEven(const aObj: TTestObj): Boolean;
  published
    procedure TestCreate;
    procedure CreateArray11;
    procedure CreateEnum11;
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
    procedure Intersect;
    procedure Join;
    procedure ArithmeticAdd;
    procedure ArithmeticSubtract;
    procedure SymmetricSubtract;

    procedure ObjectMultiSetRemove;
    procedure ObjectMultiSetRemoveArray;
    procedure ObjectMultiSetRemoveEnum;
    procedure ObjectMultiSetRemoveSelf;

    procedure ObjectMultiSetRemoveIfRegular;
    procedure ObjectMultiSetRemoveIfDelegated;
    procedure ObjectMultiSetRemoveIfNested;

    procedure ObjectMultiSetExtractIfRegular;
    procedure ObjectMultiSetExtractIfDelegated;
    procedure ObjectMultiSetExtractIfNested;

    procedure ObjectMultiSetRetain;

    procedure ObjectMultiSetIntersect;
    procedure ObjectMultiSetArithmeticSubtract;
    procedure ObjectMultiSetSymmetricSubtract;
  end;

  THashMultisetQPTest = class(TTestCase)
  private
  type
    TMultiSet        = class(specialize TGHashMultiSetQP<Integer>);
    TAutoMultiSet    = specialize TGAutoRef<TMultiSet>;
    TObjMultiSet     = specialize TGObjHashMultiSetQP<TTestObj>;
    TAutoObjMultiSet = specialize TGAutoRef<TObjMultiSet>;

    function IsEven(const aValue: Integer): Boolean;
    function ObjIsEven(const aObj: TTestObj): Boolean;
  published
    procedure TestCreate;
    procedure CreateArray11;
    procedure CreateEnum11;
    procedure CreateCapacity0;
    procedure CreateCapacity5;
    procedure CreateCapacity50Array11;
    procedure CreateCapacity50Enum11;
    procedure CreateLoadFactor0;
    procedure CreateLoadFactor3;
    procedure CreateLoadFactor;
    procedure CreateLoadFactor075Array21;
    procedure CreateLoadFactor075Enum21;
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
    procedure Intersect;
    procedure Join;
    procedure ArithmeticAdd;
    procedure ArithmeticSubtract;
    procedure SymmetricSubtract;

    procedure ObjectMultiSetRemove;
    procedure ObjectMultiSetRemoveArray;
    procedure ObjectMultiSetRemoveEnum;
    procedure ObjectMultiSetRemoveSelf;

    procedure ObjectMultiSetRemoveIfRegular;
    procedure ObjectMultiSetRemoveIfDelegated;
    procedure ObjectMultiSetRemoveIfNested;

    procedure ObjectMultiSetExtractIfRegular;
    procedure ObjectMultiSetExtractIfDelegated;
    procedure ObjectMultiSetExtractIfNested;

    procedure ObjectMultiSetRetain;

    procedure ObjectMultiSetIntersect;
    procedure ObjectMultiSetArithmeticSubtract;
    procedure ObjectMultiSetSymmetricSubtract;
  end;

  TChainHashMultisetTest = class(TTestCase)
  private
  type
    TMultiSet        = class(specialize TGChainHashMultiSet<Integer>);
    TAutoMultiSet    = specialize TGAutoRef<TMultiSet>;
    TObjMultiSet     = specialize TGObjChainHashMultiSet<TTestObj>;
    TAutoObjMultiSet = specialize TGAutoRef<TObjMultiSet>;

    function IsEven(const aValue: Integer): Boolean;
    function ObjIsEven(const aObj: TTestObj): Boolean;
  published
    procedure TestCreate;
    procedure CreateArray11;
    procedure CreateEnum11;
    procedure CreateCapacity0;
    procedure CreateCapacity5;
    procedure CreateCapacity50Array11;
    procedure CreateCapacity50Enum11;
    procedure CreateLoadFactor0;
    procedure CreateLoadFactor3;
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

    procedure TestRemove;
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
    procedure Intersect;
    procedure Join;
    procedure ArithmeticAdd;
    procedure ArithmeticSubtract;
    procedure SymmetricSubtract;

    procedure ObjectMultiSetRemove;
    procedure ObjectMultiSetRemoveArray;
    procedure ObjectMultiSetRemoveEnum;
    procedure ObjectMultiSetRemoveSelf;

    procedure ObjectMultiSetRemoveIfRegular;
    procedure ObjectMultiSetRemoveIfDelegated;
    procedure ObjectMultiSetRemoveIfNested;

    procedure ObjectMultiSetExtractIfRegular;
    procedure ObjectMultiSetExtractIfDelegated;
    procedure ObjectMultiSetExtractIfNested;

    procedure ObjectMultiSetRetain;

    procedure ObjectMultiSetIntersect;
    procedure ObjectMultiSetArithmeticSubtract;
    procedure ObjectMultiSetSymmetricSubtract;
  end;

  TLiteHashMultisetLPTest = class(TTestCase)
  private
  type
    TMultiSetSpec = specialize TGLiteHashMultiSetLP<Integer, Integer>;
    TMultiSet     = TMultiSetSpec.TMultiSet;

    function IsEven(const aValue: Integer): Boolean;
  published
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

    procedure IsSuperset;
    procedure IsSubset;
    procedure IsEqual;
    procedure Intersecting;
    procedure Intersect;
    procedure Join;
    procedure ArithmeticAdd;
    procedure ArithmeticSubtract;
    procedure SymmetricSubtract;
    procedure PassByValue;
  end;

  TLiteChainHashMultisetTest = class(TTestCase)
  private
  type
    TMultiSetSpec = specialize TGLiteChainHashMultiSet<Integer, Integer>;
    TMultiSet     = TMultiSetSpec.TMultiSet;

    function IsEven(const aValue: Integer): Boolean;
  published
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

    procedure IsSuperset;
    procedure IsSubset;
    procedure IsEqual;
    procedure Intersecting;
    procedure Intersect;
    procedure Join;
    procedure ArithmeticAdd;
    procedure ArithmeticSubtract;
    procedure SymmetricSubtract;
    procedure PassByValue;
  end;

  TLiteEquatableHashMultisetTest = class(TTestCase)
  private
  type
    TMultiSetSpec = specialize TGLiteEquatableHashMultiSet<Integer, Integer>;
    TMultiSet     = TMultiSetSpec.TMultiSet;

    function IsEven(const aValue: Integer): Boolean;
  published
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

    procedure IsSuperset;
    procedure IsSubset;
    procedure IsEqual;
    procedure Intersecting;
    procedure Intersect;
    procedure Join;
    procedure ArithmeticAdd;
    procedure ArithmeticSubtract;
    procedure SymmetricSubtract;
    procedure PassByValue;
  end;

  TThreadFGHashMultiSetTest = class(TTestCase)
  private
  type
    TSet    = specialize TGThreadHashMultiSetFG<Integer>;
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

function IsEvenInt(const aValue: Integer): Boolean;
begin
  Result := not Odd(aValue);
end;

function IsEvenObj(const aObj: TTestObj): Boolean;
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

function THashMultisetLPTest.IsEven(const aValue: Integer): Boolean;
begin
  Result := not Odd(aValue);
end;

function THashMultisetLPTest.ObjIsEven(const aObj: TTestObj): Boolean;
begin
  Result := not Odd(aObj.Value);
end;

procedure THashMultisetLPTest.TestCreate;
var
  ms: TAutoMultiSet;
begin
  AssertTrue(ms.Instance.Count = 0);
  AssertTrue(ms.Instance.LoadFactor = TMultiSet.DefaultLoadFactor);
end;

procedure THashMultisetLPTest.CreateArray11;
var
  ms: TAutoMultiSet;
  I: Integer;
begin
  ms.Instance := TMultiSet.Create(IntArray11);
  AssertTrue(ms.Instance.Count = 11);
  AssertTrue(ms.Instance.LoadFactor = TMultiSet.DefaultLoadFactor);
  for I in IntArray11 do
    AssertTrue(ms.Instance.Contains(I));
end;

procedure THashMultisetLPTest.CreateEnum11;
var
  ms: TAutoMultiSet;
  I: Integer;
  e: IIntEnumerable;
begin
  e := TIntArrayCursor.Create(TIntHelper.CreateCopy(IntArray11));
  ms.Instance := TMultiSet.Create(e);
  AssertTrue(ms.Instance.Count = 11);
  AssertTrue(ms.Instance.LoadFactor = TMultiSet.DefaultLoadFactor);
  for I in IntArray11 do
    AssertTrue(ms.Instance.Contains(I));
end;

procedure THashMultisetLPTest.CreateCapacity0;
var
  ms: TAutoMultiSet;
begin
  ms.Instance := TMultiSet.Create(0);
  AssertTrue(ms.Instance.Count = 0);
  AssertTrue(ms.Instance.LoadFactor = TMultiSet.DefaultLoadFactor);
end;

procedure THashMultisetLPTest.CreateCapacity5;
var
  ms: TAutoMultiSet;
begin
  ms.Instance := TMultiSet.Create(5);
  AssertTrue(ms.Instance.Count = 0);
  AssertTrue(ms.Instance.Capacity >= 5);
  AssertTrue(ms.Instance.LoadFactor = TMultiSet.DefaultLoadFactor);
end;

procedure THashMultisetLPTest.CreateCapacity50Array11;
var
  ms: TAutoMultiSet;
  I: Integer;
begin
  ms.Instance := TMultiSet.Create(50, IntArray11);
  AssertTrue(ms.Instance.Count = 11);
  AssertTrue(ms.Instance.Capacity >= 50);
  AssertTrue(ms.Instance.LoadFactor = TMultiSet.DefaultLoadFactor);
  for I in IntArray11 do
    AssertTrue(ms.Instance.Contains(I));
end;

procedure THashMultisetLPTest.CreateCapacity50Enum11;
var
  ms: TAutoMultiSet;
  I: Integer;
  e: IIntEnumerable;
begin
  e := TIntArrayCursor.Create(TIntHelper.CreateCopy(IntArray11));
  ms.Instance := TMultiSet.Create(50, e);
  AssertTrue(ms.Instance.Count = 11);
  AssertTrue(ms.Instance.Capacity >= 50);
  AssertTrue(ms.Instance.LoadFactor = TMultiSet.DefaultLoadFactor);
  for I in IntArray11 do
    AssertTrue(ms.Instance.Contains(I));
end;

procedure THashMultisetLPTest.CreateLoadFactor0;
var
  ms: TAutoMultiSet;
begin
  ms.Instance := TMultiSet.Create(0.0);
  AssertTrue(ms.Instance.Count = 0);
  AssertTrue(ms.Instance.LoadFactor = TMultiSet.MinLoadFactor);
end;

procedure THashMultisetLPTest.CreateLoadFactor3;
var
  ms: TAutoMultiSet;
begin
  ms.Instance := TMultiSet.Create(3.0);
  AssertTrue(ms.Instance.Count = 0);
  AssertTrue(ms.Instance.LoadFactor = TMultiSet.MaxLoadFactor);
end;

procedure THashMultisetLPTest.CreateLoadFactor;
var
  ms: TAutoMultiSet;
  lf: Double;
begin
  lf := 0.5;
  ms.Instance := TMultiSet.Create(lf);
  AssertTrue(ms.Instance.Count = 0);
  AssertTrue(ms.Instance.LoadFactor = lf);
end;

procedure THashMultisetLPTest.CreateLoadFactor075Array11;
var
  ms: TAutoMultiSet;
  I: Integer;
  lf: Single;
begin
  lf := 0.75;
  ms.Instance := TMultiSet.Create(lf, IntArray11);
  AssertTrue(ms.Instance.Count = 11);
  AssertTrue(ms.Instance.Capacity >= 11);
  AssertTrue(ms.Instance.LoadFactor = lf);
  for I in IntArray11 do
    AssertTrue(ms.Instance.Contains(I));
end;

procedure THashMultisetLPTest.CreateLoadFactor075Enum11;
var
  ms: TAutoMultiSet;
  I: Integer;
  e: IIntEnumerable;
  lf: Single;
begin
  lf := 0.75;
  e := TIntArrayCursor.Create(TIntHelper.CreateCopy(IntArray11));
  ms.Instance := TMultiSet.Create(lf, e);
  AssertTrue(ms.Instance.Count = 11);
  AssertTrue(ms.Instance.Capacity >= 11);
  AssertTrue(ms.Instance.LoadFactor = lf);
  for I in IntArray11 do
    AssertTrue(ms.Instance.Contains(I));
end;

procedure THashMultisetLPTest.CreateCapacityLoadFactor;
var
  ms: TAutoMultiSet;
  lf: Single;
begin
  lf := 0.45;
  ms.Instance := TMultiSet.Create(50, lf);
  AssertTrue(ms.Instance.Count = 0);
  AssertTrue(ms.Instance.Capacity >= 50);
  AssertTrue(ms.Instance.LoadFactor = lf);
end;

procedure THashMultisetLPTest.CreateCapacityLoadFactor075Array11;
var
  ms: TAutoMultiSet;
  I: Integer;
  lf: Single;
begin
  lf := 0.75;
  ms.Instance := TMultiSet.Create(50, lf, IntArray11);
  AssertTrue(ms.Instance.Count = 11);
  AssertTrue(ms.Instance.Capacity >= 50);
  AssertTrue(ms.Instance.LoadFactor = lf);
  for I in IntArray11 do
    AssertTrue(ms.Instance.Contains(I));
end;

procedure THashMultisetLPTest.CreateCapacityLoadFactor075Enum11;
var
  ms: TAutoMultiSet;
  I: Integer;
  e: IIntEnumerable;
  lf: Single;
begin
  lf := 0.75;
  e := TIntArrayCursor.Create(TIntHelper.CreateCopy(IntArray11));
  ms.Instance := TMultiSet.Create(50, lf, e);
  AssertTrue(ms.Instance.Count = 11);
  AssertTrue(ms.Instance.Capacity >= 50);
  AssertTrue(ms.Instance.LoadFactor = lf);
  for I in IntArray11 do
    AssertTrue(ms.Instance.Contains(I));
end;

procedure THashMultisetLPTest.Clear;
var
  ms: TAutoMultiSet;
begin
  ms.Instance := TMultiSet.Create(IntArray11);
  AssertTrue(ms.Instance.Count = 11);
  AssertTrue(ms.Instance.Capacity >= 11);
  ms.Instance.Clear;
  AssertTrue(ms.Instance.Count = 0);
  AssertTrue(ms.Instance.Capacity = 0);
end;

procedure THashMultisetLPTest.EnsureCapacity;
var
  ms: TAutoMultiSet;
  c: SizeInt;
begin
  c := ms.Instance.ExpandTreshold;
  ms.Instance.EnsureCapacity(c + 100);
  AssertTrue(ms.Instance.ExpandTreshold >= c + 100);
end;

procedure THashMultisetLPTest.TrimToFit;
var
  ms: TAutoMultiSet;
  c: SizeInt;
begin
  ms.Instance := TMultiSet.Create(0.8, IntArray11);
  ms.Instance.EnsureCapacity(40);
  c := ms.Instance.Capacity;
  ms.Instance.TrimToFit;
  AssertTrue(ms.Instance.Capacity < c);
end;

procedure THashMultisetLPTest.Add;
var
  ms: TAutoMultiSet;
begin
  AssertTrue(ms.Instance.Count = 0);
  AssertTrue(ms.Instance.Add(1));
  AssertTrue(ms.Instance.Count = 1);
  AssertTrue(ms.Instance.Contains(1));
  AssertTrue(ms.Instance.Add(51));
  AssertTrue(ms.Instance.Contains(51));
  AssertTrue(ms.Instance.Count = 2);
  AssertTrue(ms.Instance.EntryCount = 2);
  AssertTrue(ms.Instance.Add(1));
  AssertTrue(ms.Instance.Add(51));
  AssertTrue(ms.Instance.Count = 4);
  AssertTrue(ms.Instance.EntryCount = 2);
  AssertTrue(ms.Instance.Add(52));
  AssertTrue(ms.Instance.Contains(52));
  AssertTrue(ms.Instance.Count = 5);
  AssertTrue(ms.Instance.EntryCount = 3);
end;

procedure THashMultisetLPTest.Add100;
var
  ms: TAutoMultiSet;
  I: Integer;
begin
  for I := 1 to 100 do
    AssertTrue(ms.Instance.Add(I));
  AssertTrue(ms.Instance.Count = 100);
  for I := 1 to 100 do
    AssertTrue(ms.Instance.Add(I));
  AssertTrue(ms.Instance.Count = 200);
  AssertTrue(ms.Instance.EntryCount = 100);
  for I := 1 to 100 do
    AssertTrue(ms.Instance.Contains(I));
  ms.Instance[10] := 52;
  AssertTrue(ms.Instance.Count = 250);
  ms.Instance[10] := 12;
  AssertTrue(ms.Instance.Count = 210);
end;

procedure THashMultisetLPTest.AddArray;
var
  ms: TAutoMultiSet;
begin
  AssertTrue(ms.Instance.AddAll(IntArray21) = System.Length(IntArray21));
  AssertTrue(ms.Instance.Count = System.Length(IntArray21));
  AssertTrue(ms.Instance.EntryCount = System.Length(IntArray21));
  AssertTrue(ms.Instance.ContainsAll(IntArray21));
  AssertTrue(ms.Instance.AddAll(IntArray21) = System.Length(IntArray21));
  AssertTrue(ms.Instance.Count = System.Length(IntArray21) * 2);
  AssertTrue(ms.Instance.EntryCount = System.Length(IntArray21));
  AssertTrue(ms.Instance.ContainsAll(IntArray21));
end;

procedure THashMultisetLPTest.AddEnum;
var
  ms: TAutoMultiSet;
  e: IIntEnumerable;
begin
  e := TIntArrayCursor.Create(TIntHelper.CreateCopy(IntArray21));
  AssertTrue(ms.Instance.AddAll(e) = System.Length(IntArray21));
  AssertTrue(ms.Instance.Count = System.Length(IntArray21));
  AssertTrue(ms.Instance.EntryCount = System.Length(IntArray21));
  AssertTrue(ms.Instance.ContainsAll(IntArray21));
  e := TIntArrayCursor.Create(TIntHelper.CreateCopy(IntArray21));
  AssertTrue(ms.Instance.AddAll(e) = System.Length(IntArray21));
  AssertTrue(ms.Instance.Count = System.Length(IntArray21) * 2);
  AssertTrue(ms.Instance.EntryCount = System.Length(IntArray21));
  AssertTrue(ms.Instance.ContainsAll(IntArray21));
end;

procedure THashMultisetLPTest.AddSelf;
var
  ms: TAutoMultiSet;
begin
  ms.Instance.AddAll(IntArray21);
  AssertTrue(ms.Instance.AddAll(ms.Instance) = System.Length(IntArray21));
  AssertTrue(ms.Instance.Count = System.Length(IntArray21) * 2);
  AssertTrue(ms.Instance.EntryCount = System.Length(IntArray21));
end;

procedure THashMultisetLPTest.Remove;
var
  ms: TAutoMultiSet;
  c: SizeInt;
begin
  c := System.Length(IntArray21) * 2;
  ms.Instance := TMultiSet.Create(IntArray21);
  ms.Instance.AddAll(IntArray21);
  AssertTrue(ms.Instance.Count = c);
  AssertTrue(ms.Instance.EntryCount = System.Length(IntArray21));
  AssertTrue(ms.Instance.Remove(IntArray21[1]));
  AssertTrue(ms.Instance.Count = c - 1);
  AssertTrue(ms.Instance.EntryCount = System.Length(IntArray21));
  AssertTrue(ms.Instance.Remove(IntArray21[System.High(IntArray21)]));
  AssertTrue(ms.Instance.Count = c - 2);
  AssertTrue(ms.Instance.EntryCount = System.Length(IntArray21));
  AssertTrue(ms.Instance.Remove(IntArray21[1]));
  AssertTrue(ms.Instance.Count = c - 3);
  AssertTrue(ms.Instance.EntryCount = System.Length(IntArray21) - 1);
  AssertFalse(ms.Instance.Remove(-1));
  AssertTrue(ms.Instance.Count = c - 3);
  AssertFalse(ms.Instance.Remove(100));
  AssertTrue(ms.Instance.Count = c - 3);
end;

procedure THashMultisetLPTest.RemoveArray;
var
  ms: TAutoMultiSet;
begin
  ms.Instance := TMultiSet.Create(IntArray21);
  ms.Instance.AddAll(IntArray21);
  AssertTrue(ms.Instance.RemoveAll(IntArray11) = 11);
  AssertTrue(ms.Instance.Count = 31);
  AssertTrue(ms.Instance.EntryCount = 21);
  AssertTrue(ms.Instance.RemoveAll(IntArray10) = 10);
  AssertTrue(ms.Instance.Count = 21);
  AssertTrue(ms.Instance.EntryCount = 21);
end;

procedure THashMultisetLPTest.RemoveEnum;
var
  ms: TAutoMultiSet;
  e: IIntEnumerable;
begin
  ms.Instance := TMultiSet.Create(IntArray21);
  ms.Instance.AddAll(IntArray21);
  e := TIntArrayCursor.Create(TIntHelper.CreateCopy(IntArray11));
  AssertTrue(ms.Instance.RemoveAll(e) = 11);
  AssertTrue(ms.Instance.Count = 31);
  AssertTrue(ms.Instance.EntryCount = 21);
  e := TIntArrayCursor.Create(TIntHelper.CreateCopy(IntArray10));
  AssertTrue(ms.Instance.RemoveAll(e) = 10);
  AssertTrue(ms.Instance.Count = 21);
  AssertTrue(ms.Instance.EntryCount = 21);
end;

procedure THashMultisetLPTest.RemoveSelf;
var
  ms: TAutoMultiSet;
begin
  ms.Instance := TMultiSet.Create(IntArray21);
  ms.Instance.AddAll(IntArray21);
  AssertTrue(ms.Instance.RemoveAll(ms.Instance) = 42);
  AssertTrue(ms.Instance.IsEmpty);
end;

procedure THashMultisetLPTest.RemoveIfRegular;
var
  ms: TAutoMultiSet;
  I: Integer;
begin
  ms.Instance := TMultiSet.Create(IntArray21);
  ms.Instance.AddAll(IntArray21);
  AssertTrue(ms.Instance.RemoveIf(@IsEvenInt) = 20);
  AssertTrue(ms.Instance.Count = 22);
  AssertTrue(ms.Instance.ContainsAll(IntArray11));
  for I in IntArray11 do
    AssertTrue(ms.Instance[I] = 2);
end;

procedure THashMultisetLPTest.RemoveIfDelegated;
var
  ms: TAutoMultiSet;
  I: Integer;
begin
  ms.Instance := TMultiSet.Create(IntArray21);
  ms.Instance.AddAll(IntArray21);
  AssertTrue(ms.Instance.RemoveIf(@IsEven) = 20);
  AssertTrue(ms.Instance.Count = 22);
  AssertTrue(ms.Instance.ContainsAll(IntArray11));
  for I in IntArray11 do
    AssertTrue(ms.Instance[I] = 2);
end;

procedure THashMultisetLPTest.RemoveIfNested;
  function IsEvenInt(const aValue: Integer): Boolean;
  begin
    Result := not Odd(aValue);
  end;
var
  ms: TAutoMultiSet;
  I: Integer;
begin
  ms.Instance := TMultiSet.Create(IntArray21);
  ms.Instance.AddAll(IntArray21);
  AssertTrue(ms.Instance.RemoveIf(@IsEvenInt) = 20);
  AssertTrue(ms.Instance.Count = 22);
  AssertTrue(ms.Instance.ContainsAll(IntArray11));
  for I in IntArray11 do
    AssertTrue(ms.Instance[I] = 2);
end;

procedure THashMultisetLPTest.ExtractIfRegular;
var
  ms, ms1: TAutoMultiSet;
  e: TIntArray;
  I: Integer;
begin
  ms.Instance := TMultiSet.Create(IntArray21);
  ms.Instance.AddAll(IntArray21);
  e := ms.Instance.ExtractIf(@IsEvenInt);
  AssertTrue(ms.Instance.Count = 22);
  AssertTrue(ms.Instance.ContainsAll(IntArray11));
  AssertTrue(System.Length(e) = 20);
  ms1.Instance := TMultiSet.Create(e);
  for I in IntArray11 do
    AssertTrue(ms.Instance[I] = 2);
  for I in IntArray10 do
    AssertTrue(ms1.Instance[I] = 2);
end;

procedure THashMultisetLPTest.ExtractIfDelegated;
var
  ms, ms1: TAutoMultiSet;
  e: TIntArray;
  I: Integer;
begin
  ms.Instance := TMultiSet.Create(IntArray21);
  ms.Instance.AddAll(IntArray21);
  e := ms.Instance.ExtractIf(@IsEven);
  AssertTrue(ms.Instance.Count = 22);
  AssertTrue(ms.Instance.ContainsAll(IntArray11));
  AssertTrue(System.Length(e) = 20);
  ms1.Instance := TMultiSet.Create(e);
  for I in IntArray11 do
    AssertTrue(ms.Instance[I] = 2);
  for I in IntArray10 do
    AssertTrue(ms1.Instance[I] = 2);
end;

procedure THashMultisetLPTest.ExtractIfNested;
var
  ms, ms1: TAutoMultiSet;
  e: TIntArray;
  I: Integer;
  function IsEvenInt(const aValue: Integer): Boolean;
  begin
    Result := not Odd(aValue);
  end;
begin
  ms.Instance := TMultiSet.Create(IntArray21);
  ms.Instance.AddAll(IntArray21);
  e := ms.Instance.ExtractIf(@IsEvenInt);
  AssertTrue(ms.Instance.Count = 22);
  AssertTrue(ms.Instance.ContainsAll(IntArray11));
  AssertTrue(System.Length(e) = 20);
  ms1.Instance := TMultiSet.Create(e);
  for I in IntArray11 do
    AssertTrue(ms.Instance[I] = 2);
  for I in IntArray10 do
    AssertTrue(ms1.Instance[I] = 2);
end;

procedure THashMultisetLPTest.Retain;
var
  ms, ms1: TAutoMultiSet;
  I: Integer;
begin
  ms.Instance := TMultiSet.Create(IntArray21);
  ms.Instance.AddAll(IntArray21);
  ms1.Instance := TMultiSet.Create(IntArray11);
  AssertTrue(ms.Instance.Count = 42);
  ms.Instance.RetainAll(ms1.Instance);
  AssertTrue(ms.Instance.Count = 22);
  AssertTrue(ms.Instance.ContainsAll(ms1.Instance));
  for I in IntArray11 do
    AssertTrue(ms.Instance[I] = 2);
end;

procedure THashMultisetLPTest.Retain_1;
var
  ms, ms1: TAutoMultiSet;
begin
  ms.Instance := TMultiSet.Create(IntArray10);
  ms.Instance.AddAll(IntArray10);
  AssertTrue(ms.Instance.Count = 20);
  ms1.Instance := TMultiSet.Create(IntArray11);
  ms.Instance.RetainAll(ms1.Instance);
  AssertTrue(ms.Instance.IsEmpty);
end;

procedure THashMultisetLPTest.RetainSelf;
var
  ms: TAutoMultiSet;
begin
  ms.Instance := TMultiSet.Create(IntArray21);
  ms.Instance.AddAll(IntArray21);
  AssertTrue(ms.Instance.Count = 42);
  ms.Instance.RetainAll(ms.Instance);
  AssertTrue(ms.Instance.Count = 42);
  AssertTrue(ms.Instance.ContainsAll(IntArray21));
end;

procedure THashMultisetLPTest.IsSuperset;
var
  ms, ms1: TAutoMultiSet;
begin
  ms.Instance := TMultiSet.Create(IntArray21);
  ms.Instance.AddAll(IntArray21);
  ms1.Instance := TMultiSet.Create(ms.Instance);
  AssertTrue(ms.Instance.IsSuperSet(ms1.Instance));
  AssertTrue(ms1.Instance.IsSuperSet(ms.Instance));
  AssertTrue(ms.Instance.IsSuperSet(ms.Instance));
  ms1.Instance.Remove(IntArray21[1]);
  AssertTrue(ms.Instance.IsSuperSet(ms1.Instance));
  AssertFalse(ms1.Instance.IsSuperSet(ms.Instance));
  ms1.Instance.Clear;
  AssertTrue(ms.Instance.IsSuperSet(ms1.Instance));
  AssertFalse(ms1.Instance.IsSuperSet(ms.Instance));
  ms.Instance.Clear;
  AssertTrue(ms.Instance.IsSuperSet(ms1.Instance));
  AssertTrue(ms1.Instance.IsSuperSet(ms.Instance));
end;

procedure THashMultisetLPTest.IsSubset;
var
  ms, ms1: TAutoMultiSet;
begin
  ms.Instance := TMultiSet.Create(IntArray21);
  ms.Instance.AddAll(IntArray21);
  ms1.Instance := TMultiSet.Create(ms.Instance);
  AssertTrue(ms.Instance.IsSubSet(ms1.Instance));
  AssertTrue(ms1.Instance.IsSubSet(ms.Instance));
  AssertTrue(ms.Instance.IsSubSet(ms.Instance));
  ms1.Instance.Remove(IntArray21[1]);
  AssertFalse(ms.Instance.IsSubSet(ms1.Instance));
  AssertTrue(ms1.Instance.IsSubSet(ms.Instance));
  ms1.Instance.Clear;
  AssertFalse(ms.Instance.IsSubSet(ms1.Instance));
  AssertTrue(ms1.Instance.IsSubSet(ms.Instance));
  ms.Instance.Clear;
  AssertTrue(ms1.Instance.IsSubSet(ms.Instance));
  AssertTrue(ms.Instance.IsSubSet(ms1.Instance));
end;

procedure THashMultisetLPTest.IsEqual;
var
  ms, ms1: TAutoMultiSet;
begin
  ms.Instance := TMultiSet.Create(IntArray21);
  ms.Instance.AddAll(IntArray21);
  ms1.Instance := TMultiSet.Create(ms.Instance);
  AssertTrue(ms.Instance.IsEqual(ms1.Instance));
  AssertTrue(ms1.Instance.IsEqual(ms.Instance));
  AssertTrue(ms.Instance.IsEqual(ms.Instance));
  ms1.Instance.Remove(IntArray21[1]);
  AssertFalse(ms.Instance.IsEqual(ms1.Instance));
  AssertFalse(ms1.Instance.IsEqual(ms.Instance));
  ms1.Instance.Clear;
  ms.Instance.Clear;
  AssertTrue(ms.Instance.IsEqual(ms1.Instance));
  AssertTrue(ms1.Instance.IsEqual(ms.Instance));
end;

procedure THashMultisetLPTest.Intersecting;
var
  ms, ms1: TAutoMultiSet;
begin
  ms.Instance := TMultiSet.Create(IntArray21);
  ms1.Instance := TMultiSet.Create(IntArray11);
  AssertTrue(ms.Instance.Intersecting(ms.Instance));
  AssertTrue(ms.Instance.Intersecting(ms1.Instance));
  ms.Instance := TMultiSet.Create(IntArray10);
  AssertFalse(ms.Instance.Intersecting(ms1.Instance));
  AssertFalse(ms1.Instance.Intersecting(ms.Instance));
end;

procedure THashMultisetLPTest.Intersect;
var
  ms, ms1: TAutoMultiSet;
  I: Integer;
begin
  ms.Instance := TMultiSet.Create(IntArray21);
  ms.Instance.AddAll(IntArray21);
  AssertTrue(ms.Instance.Count = 42);
  ms1.Instance := TMultiSet.Create(IntArray11);
  ms1.Instance.AddAll([-4, -3, -2, -1, 0]);
  ms.Instance.Intersect(ms1.Instance);
  AssertTrue(ms.Instance.Count = 11);
  for I in IntArray11 do
    AssertTrue(ms.Instance[I] = 1);
end;

procedure THashMultisetLPTest.Join;
var
  ms, ms1: TAutoMultiSet;
  I: Integer;
begin
  ms1.Instance := TMultiSet.Create(IntArray11);
  ms.Instance.Join(ms1.Instance);
  AssertTrue(ms.Instance.IsEqual(ms1.Instance));
  ms1.Instance := TMultiSet.Create(IntArray10);
  ms1.Instance.AddAll(ms1.Instance);
  ms.Instance.Join(ms1.Instance);
  for I in IntArray11 do
    AssertTrue(ms.Instance[I] = 1);
  for I in IntArray10 do
    AssertTrue(ms.Instance[I] = 2);
end;

procedure THashMultisetLPTest.ArithmeticAdd;
var
  ms, ms1: TAutoMultiSet;
  I: Integer;
begin
  ms1.Instance := TMultiSet.Create(IntArray11);
  ms.Instance.ArithmeticAdd(ms1.Instance);
  AssertTrue(ms.Instance.IsEqual(ms1.Instance));
  ms.Instance.AddAll(IntArray10);
  ms.Instance.ArithmeticAdd(ms1.Instance);
  for I in IntArray11 do
    AssertTrue(ms.Instance[I] = 2);
  for I in IntArray10 do
    AssertTrue(ms.Instance[I] = 1);
  ms.Instance.ArithmeticAdd(ms.Instance);
  for I in IntArray11 do
    AssertTrue(ms.Instance[I] = 4);
  for I in IntArray10 do
    AssertTrue(ms.Instance[I] = 2);
end;

procedure THashMultisetLPTest.ArithmeticSubtract;
var
  ms, ms1: TAutoMultiSet;
begin
  ms1.Instance := TMultiSet.Create(IntArray11);
  ms.Instance.ArithmeticSubtract(ms1.Instance);
  AssertTrue(ms.Instance.IsEmpty);
  ms.Instance.AddAll(IntArray10);
  ms.Instance.ArithmeticSubtract(ms1.Instance);
  AssertTrue(ms.Instance.Count = 10);
  AssertTrue(ms.Instance.ContainsAll(IntArray10));
  ms.Instance.AddAll(IntArray11);
  AssertTrue(ms.Instance.ContainsAll(IntArray21));
  ms.Instance.ArithmeticSubtract(ms1.Instance);
  AssertTrue(ms.Instance.Count = 10);
  AssertTrue(ms.Instance.ContainsAll(IntArray10));
  ms1.Instance.AddAll(IntArray10);
  ms.Instance.ArithmeticSubtract(ms1.Instance);
  AssertTrue(ms.Instance.IsEmpty);
end;

procedure THashMultisetLPTest.SymmetricSubtract;
var
  ms, ms1: TAutoMultiSet;
begin
  ms1.Instance := TMultiSet.Create(IntArray11);
  ms.Instance.SymmetricSubtract(ms1.Instance);
  AssertTrue(ms.Instance.Count = 11);
  AssertTrue(ms.Instance.ContainsAll(IntArray11));
  ms.Instance.AddAll(IntArray10);
  ms.Instance.SymmetricSubtract(ms1.Instance);
  AssertTrue(ms.Instance.Count = 10);
  AssertTrue(ms.Instance.ContainsAll(IntArray10));
  ms.Instance.AddAll(IntArray11);
  ms.Instance.AddAll(IntArray21);
  AssertTrue(ms.Instance.Count = 42);
  ms1.Instance.AddAll(IntArray10);
  ms.Instance.SymmetricSubtract(ms1.Instance);
  AssertTrue(ms.Instance.Count = 21);
  AssertTrue(ms.Instance.ContainsAll(IntArray21));
  ms.Instance.SymmetricSubtract(ms1.Instance);
  AssertTrue(ms.Instance.IsEmpty);
end;

procedure THashMultisetLPTest.ObjectMultiSetRemove;
var
  ms: TAutoObjMultiSet;
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
      ms.Instance.Add(a[I]);
    end;
  AssertTrue(ms.Instance.Count = TestSize);
  ms.Instance.AddAll(a);
  AssertTrue(ms.Instance.Count = TestSize * 2);
  for I := 0 to System.High(a) do
    AssertTrue(ms.Instance.Remove(a[I]));
  AssertTrue(ms.Instance.Count = TestSize);
  AssertTrue(Counter.Count = 0);
  for I := 0 to System.High(a) do
    begin
      AssertTrue(ms.Instance.Remove(a[I]));
      AssertTrue(ms.Instance.Count = Pred(TestSize - I));
      AssertTrue(Counter.Count = Succ(I));
    end;
  AssertTrue(ms.Instance.IsEmpty);
  AssertTrue(Counter.Count = TestSize);
end;

procedure THashMultisetLPTest.ObjectMultiSetRemoveArray;
var
  ms: TAutoObjMultiSet;
  a: TObjArray;
  I, TestSize: Integer;
  Counter: TCounter;
begin
  TestSize := 56;
  Counter := Default(TCounter);
  for I := 1 to TestSize do
    ms.Instance.Add(TTestObj.Create(Counter.Increment));
  AssertTrue(ms.Instance.Count = TestSize);
  a := ms.Instance.ToArray;
  AssertTrue(System.Length(a) = TestSize);
  ms.Instance.AddAll(a);
  AssertTrue(ms.Instance.Count = TestSize * 2);
  AssertTrue(ms.Instance.RemoveAll(a) = TestSize);
  AssertTrue(Counter.Count = 0);
  AssertTrue(ms.Instance.RemoveAll(a) = TestSize);
  AssertTrue(ms.Instance.IsEmpty);
  AssertTrue(Counter.Count = TestSize);
end;

procedure THashMultisetLPTest.ObjectMultiSetRemoveEnum;
type
  TFakeEnum = specialize TGArrayCursor<TTestObj>;
var
  ms: TAutoObjMultiSet;
  a: TObjArray;
  e: TFakeEnum;
  I, TestSize: Integer;
  Counter: TCounter;
begin
  TestSize := 52;
  Counter := Default(TCounter);
  for I := 1 to TestSize do
    ms.Instance.Add(TTestObj.Create(Counter.Increment));
  AssertTrue(ms.Instance.Count = TestSize);
  a := ms.Instance.ToArray;
  AssertTrue(System.Length(a) = TestSize);
  ms.Instance.AddAll(a);
  AssertTrue(ms.Instance.Count = TestSize * 2);
  e := TFakeEnum.Create(a);
  AssertTrue(ms.Instance.RemoveAll(e) = TestSize);
  AssertTrue(Counter.Count = 0);
  e := TFakeEnum.Create(a);
  AssertTrue(ms.Instance.RemoveAll(e) = TestSize);
  AssertTrue(ms.Instance.IsEmpty);
  AssertTrue(Counter.Count = TestSize);
end;

procedure THashMultisetLPTest.ObjectMultiSetRemoveSelf;
var
  ms: TAutoObjMultiSet;
  I, TestSize: Integer;
  Counter: TCounter;
begin
  TestSize := 100;
  Counter := Default(TCounter);
  for I := 1 to TestSize do
    ms.Instance.Add(TTestObj.Create(Counter.Increment));
  ms.Instance.AddAll(ms.Instance);
  AssertTrue(ms.Instance.Count = TestSize * 2);
  AssertTrue(ms.Instance.RemoveAll(ms.Instance) = TestSize * 2);
  AssertTrue(ms.Instance.IsEmpty);
  AssertTrue(Counter.Count = TestSize);
end;

procedure THashMultisetLPTest.ObjectMultiSetRemoveIfRegular;
var
  ms: TAutoObjMultiSet;
  I, TestSize: Integer;
  Counter: TCounter;
  o: TTestObj;
begin
  TestSize := 100;
  Counter := Default(TCounter);
  for I := 1 to TestSize do
    ms.Instance.Add(TTestObj.Create(I, Counter.Increment));
  ms.Instance.AddAll(ms.Instance);
  AssertTrue(ms.Instance.Count = TestSize * 2);
  AssertTrue(ms.Instance.RemoveIf(@IsEvenObj) = TestSize);
  AssertTrue(Counter.Count = TestSize div 2);
  AssertTrue(ms.Instance.Count = TestSize);
  for o in ms.Instance do
    AssertTrue(Odd(o.Value));
  ms.Instance := Default(TObjMultiSet);
  AssertTrue(Counter.Count = TestSize);
end;

procedure THashMultisetLPTest.ObjectMultiSetRemoveIfDelegated;
var
  ms: TAutoObjMultiSet;
  I, TestSize: Integer;
  Counter: TCounter;
  o: TTestObj;
begin
  TestSize := 100;
  Counter := Default(TCounter);
  for I := 1 to TestSize do
    ms.Instance.Add(TTestObj.Create(I, Counter.Increment));
  ms.Instance.AddAll(ms.Instance);
  AssertTrue(ms.Instance.Count = TestSize * 2);
  AssertTrue(ms.Instance.RemoveIf(@ObjIsEven) = TestSize);
  AssertTrue(Counter.Count = TestSize div 2);
  AssertTrue(ms.Instance.Count = TestSize);
  for o in ms.Instance do
    AssertTrue(Odd(o.Value));
  ms.Instance := Default(TObjMultiSet);
  AssertTrue(Counter.Count = TestSize);
end;

procedure THashMultisetLPTest.ObjectMultiSetRemoveIfNested;
  function IsEvenNest(const aObj: TTestObj): Boolean;
  begin
    Result := not Odd(aObj.Value);
  end;
var
  ms: TAutoObjMultiSet;
  I, TestSize: Integer;
  Counter: TCounter;
  o: TTestObj;
begin
  TestSize := 100;
  Counter := Default(TCounter);
  for I := 1 to TestSize do
    ms.Instance.Add(TTestObj.Create(I, Counter.Increment));
  ms.Instance.AddAll(ms.Instance);
  AssertTrue(ms.Instance.Count = TestSize * 2);
  AssertTrue(ms.Instance.RemoveIf(@IsEvenNest) = TestSize);
  AssertTrue(Counter.Count = TestSize div 2);
  AssertTrue(ms.Instance.Count = TestSize);
  for o in ms.Instance do
    AssertTrue(Odd(o.Value));
  ms.Instance := Default(TObjMultiSet);
  AssertTrue(Counter.Count = TestSize);
end;

procedure THashMultisetLPTest.ObjectMultiSetExtractIfRegular;
var
  ms: TAutoObjMultiSet;
  I, TestSize: Integer;
  Counter: TCounter;
  a: TObjArray;
  o: TTestObj;
begin
  TestSize := 100;
  Counter := Default(TCounter);
  for I := 1 to TestSize do
    ms.Instance.Add(TTestObj.Create(I, Counter.Increment));
  ms.Instance.AddAll(ms.Instance);
  AssertTrue(ms.Instance.Count = TestSize * 2);
  a := ms.Instance.ExtractIf(@IsEvenObj);
  AssertTrue(System.Length(a) = TestSize);
  AssertTrue(Counter.Count = 0);
  AssertTrue(ms.Instance.Count = TestSize);
  for o in ms.Instance do
    AssertTrue(Odd(o.Value));
  for o in a do
    AssertTrue(not Odd(o.Value));
  ms.Instance.Clear;
  AssertTrue(Counter.Count = TestSize div 2);
  ms.Instance.AddAll(a);
  ms.Instance.Clear;
  AssertTrue(Counter.Count = TestSize);
end;

procedure THashMultisetLPTest.ObjectMultiSetExtractIfDelegated;
var
  ms: TAutoObjMultiSet;
  I, TestSize: Integer;
  Counter: TCounter;
  a: TObjArray;
  o: TTestObj;
begin
  TestSize := 100;
  Counter := Default(TCounter);
  for I := 1 to TestSize do
    ms.Instance.Add(TTestObj.Create(I, Counter.Increment));
  ms.Instance.AddAll(ms.Instance);
  AssertTrue(ms.Instance.Count = TestSize * 2);
  a := ms.Instance.ExtractIf(@ObjIsEven);
  AssertTrue(System.Length(a) = TestSize);
  AssertTrue(Counter.Count = 0);
  AssertTrue(ms.Instance.Count = TestSize);
  for o in ms.Instance do
    AssertTrue(Odd(o.Value));
  for o in a do
    AssertTrue(not Odd(o.Value));
  ms.Instance.Clear;
  AssertTrue(Counter.Count = TestSize div 2);
  ms.Instance.AddAll(a);
  ms.Instance.Clear;
  AssertTrue(Counter.Count = TestSize);
end;

procedure THashMultisetLPTest.ObjectMultiSetExtractIfNested;
  function IsEvenNest(const aObj: TTestObj): Boolean;
  begin
    Result := not Odd(aObj.Value);
  end;
var
  ms: TAutoObjMultiSet;
  I, TestSize: Integer;
  Counter: TCounter;
  a: TObjArray;
  o: TTestObj;
begin
  TestSize := 100;
  Counter := Default(TCounter);
  for I := 1 to TestSize do
    ms.Instance.Add(TTestObj.Create(I, Counter.Increment));
  ms.Instance.AddAll(ms.Instance);
  AssertTrue(ms.Instance.Count = TestSize * 2);
  a := ms.Instance.ExtractIf(@IsEvenNest);
  AssertTrue(System.Length(a) = TestSize);
  AssertTrue(Counter.Count = 0);
  AssertTrue(ms.Instance.Count = TestSize);
  for o in ms.Instance do
    AssertTrue(Odd(o.Value));
  for o in a do
    AssertTrue(not Odd(o.Value));
  ms.Instance.Clear;
  AssertTrue(Counter.Count = TestSize div 2);
  ms.Instance.AddAll(a);
  ms.Instance.Clear;
  AssertTrue(Counter.Count = TestSize);
end;

procedure THashMultisetLPTest.ObjectMultiSetRetain;
type
  TObjSet     = specialize TGObjHashSetLP<TTestObj>;
  TAutoObjSet = specialize TGAutoRef<TObjSet>;
var
  ms: TAutoObjMultiSet;
  s1: TAutoObjSet;
  I, TestSize: Integer;
  Counter: TCounter;
  a: TObjArray;
begin
  TestSize := 100;
  Counter := Default(TCounter);
  System.SetLength(a, TestSize);
  s1.Instance := TObjSet.Create(False);

  for I := 0 to System.High(a) do
    a[I] := TTestObj.Create(Counter.Increment);
  s1.Instance.AddAll(a);
  ms.Instance.AddAll(a);
  ms.Instance.AddAll(a);
  for I := 1 to TestSize do
    ms.Instance.Add(TTestObj.Create(Counter.Increment));
  AssertTrue(ms.Instance.Count = TestSize * 3);
  ms.Instance.RetainAll(s1.Instance);
  AssertTrue(ms.Instance.Count = TestSize * 2);
  AssertTrue(Counter.Count = TestSize);
  AssertTrue(ms.Instance.ContainsAll(s1.Instance));
  ms.Instance := Default(TObjMultiSet);
  AssertTrue(Counter.Count = TestSize * 2);
end;

procedure THashMultisetLPTest.ObjectMultiSetIntersect;
var
  ms, ms1: TAutoObjMultiSet;
  I, TestSize: Integer;
  Counter: TCounter;
  a: TObjArray;
  o: TTestObj;
begin
  TestSize := 50;
  Counter := Default(TCounter);
  System.SetLength(a, TestSize);
  for I := 0 to System.High(a) do
    a[I] := TTestObj.Create(Counter.Increment);

  ms.Instance.AddAll(a);
  ms.Instance.AddAll(a);

  for o in a do
    AssertTrue(ms.Instance[o] = 2);

  for I := 1 to TestSize do
    ms.Instance.Add(TTestObj.Create(Counter.Increment));

  AssertTrue(Counter.Count = 0);
  AssertTrue(ms.Instance.Count = TestSize * 3);

  ms1.Instance.OwnsObjects := False;
  AssertTrue(ms1.Instance.AddAll(a) = TestSize);

  ms.Instance.Intersect(ms1.Instance);

  AssertTrue(ms.Instance.Count = TestSize);
  AssertTrue(Counter.Count = TestSize);

  for o in a do
    AssertTrue(ms.Instance[o] = 1);
end;

procedure THashMultisetLPTest.ObjectMultiSetArithmeticSubtract;
var
  ms, ms1: TAutoObjMultiSet;
  I, TestSize: Integer;
  Counter: TCounter;
  a: TObjArray;
  o: TTestObj;
begin
  TestSize := 55;
  Counter := Default(TCounter);
  System.SetLength(a, TestSize);
  for I := 0 to System.High(a) do
    a[I] := TTestObj.Create(Counter.Increment);

  ms.Instance.AddAll(a);
  ms.Instance.AddAll(a);
  ms.Instance.AddAll(a);

  ms1.Instance.OwnsObjects := False;

  ms1.Instance.AddAll(a);

  for I := 1 to TestSize do
    ms.Instance.Add(TTestObj.Create(Counter.Increment));

  AssertTrue(Counter.Count = 0);
  AssertTrue(ms.Instance.Count = TestSize * 4);
  for o in a do
    AssertTrue(ms.Instance[o] = 3);

  ms.Instance.ArithmeticSubtract(ms1.Instance);
  AssertTrue(ms.Instance.Count = TestSize * 3);
  for o in a do
    AssertTrue(ms.Instance[o] = 2);

  ms.Instance.ArithmeticSubtract(ms1.Instance);
  AssertTrue(ms.Instance.Count = TestSize * 2);
  for o in a do
    AssertTrue(ms.Instance[o] = 1);

  ms.Instance.ArithmeticSubtract(ms1.Instance);
  AssertTrue(ms.Instance.Count = TestSize);

  AssertTrue(Counter.Count = TestSize);
end;

procedure THashMultisetLPTest.ObjectMultiSetSymmetricSubtract;
var
  ms, ms1: TAutoObjMultiSet;
  I, TestSize: Integer;
  Counter: TCounter;
begin
  TestSize := 65;
  Counter := Default(TCounter);

  for I := 1 to TestSize do
    ms.Instance.Add(TTestObj.Create(Counter.Increment));
  ms.Instance.AddAll(ms.Instance);
  ms.Instance.OwnsObjects := False;

  for I := 1 to TestSize do
    ms1.Instance.Add(TTestObj.Create(Counter.Increment));
  ms1.Instance.OwnsObjects := False;

  AssertTrue(Counter.Count = 0);
  AssertTrue(ms.Instance.Count = TestSize * 2);

  ms.Instance.SymmetricSubtract(ms1.Instance);
  AssertTrue(ms.Instance.Count = TestSize * 3);

  ms1.Instance.SymmetricSubtract(ms.Instance);
  AssertTrue(ms1.Instance.Count = TestSize * 2);

  ms.Instance.SymmetricSubtract(ms1.Instance);
  AssertTrue(ms.Instance.Count = TestSize);

  ms.Instance.ArithmeticSubtract(ms1.Instance);
  AssertTrue(ms.Instance.Count = TestSize);

  AssertTrue(Counter.Count = 0);

  ms.Instance.OwnsObjects := True;
  ms1.Instance.OwnsObjects := True;

  ms.Instance := Default(TObjMultiSet);
  ms1.Instance := Default(TObjMultiSet);

  AssertTrue(Counter.Count = TestSize * 2);
end;


function THashMultisetLPTTest.IsEven(const aValue: Integer): Boolean;
begin
  Result := not Odd(aValue);
end;

function THashMultisetLPTTest.ObjIsEven(const aObj: TTestObj): Boolean;
begin
  Result := not Odd(aObj.Value);
end;

procedure THashMultisetLPTTest.TestCreate;
var
  ms: TAutoMultiSet;
begin
  AssertTrue(ms.Instance.Count = 0);
  AssertTrue(ms.Instance.LoadFactor = TMultiSet.DefaultLoadFactor);
end;

procedure THashMultisetLPTTest.CreateArray11;
var
  ms: TAutoMultiSet;
  I: Integer;
begin
  ms.Instance := TMultiSet.Create(IntArray11);
  AssertTrue(ms.Instance.Count = 11);
  AssertTrue(ms.Instance.Capacity >= 11);
  AssertTrue(ms.Instance.LoadFactor = TMultiSet.DefaultLoadFactor);
  for I in IntArray11 do
    AssertTrue(ms.Instance.Contains(I));
end;

procedure THashMultisetLPTTest.CreateEnum11;
var
  ms: TAutoMultiSet;
  I: Integer;
  e: IIntEnumerable;
begin
  e := TIntArrayCursor.Create(TIntHelper.CreateCopy(IntArray11));
  ms.Instance := TMultiSet.Create(e);
  AssertTrue(ms.Instance.Count = 11);
  AssertTrue(ms.Instance.Capacity >= 11);
  AssertTrue(ms.Instance.LoadFactor = TMultiSet.DefaultLoadFactor);
  for I in IntArray11 do
    AssertTrue(ms.Instance.Contains(I));
end;

procedure THashMultisetLPTTest.CreateCapacity0;
var
  ms: TAutoMultiSet;
begin
  ms.Instance := TMultiSet.Create(0);
  AssertTrue(ms.Instance.Count = 0);
  AssertTrue(ms.Instance.Capacity > 0);
  AssertTrue(ms.Instance.LoadFactor = TMultiSet.DefaultLoadFactor);
end;

procedure THashMultisetLPTTest.CreateCapacity5;
var
  ms: TAutoMultiSet;
begin
  ms.Instance := TMultiSet.Create(5);
  AssertTrue(ms.Instance.Count = 0);
  AssertTrue(ms.Instance.Capacity >= 5);
  AssertTrue(ms.Instance.LoadFactor = TMultiSet.DefaultLoadFactor);
end;

procedure THashMultisetLPTTest.CreateCapacity50Array11;
var
  ms: TAutoMultiSet;
  I: Integer;
begin
  ms.Instance := TMultiSet.Create(50, IntArray11);
  AssertTrue(ms.Instance.Count = 11);
  AssertTrue(ms.Instance.Capacity >= 50);
  AssertTrue(ms.Instance.LoadFactor = TMultiSet.DefaultLoadFactor);
  for I in IntArray11 do
    AssertTrue(ms.Instance.Contains(I));
end;

procedure THashMultisetLPTTest.CreateCapacity50Enum11;
var
  ms: TAutoMultiSet;
  I: Integer;
  e: IIntEnumerable;
begin
  e := TIntArrayCursor.Create(TIntHelper.CreateCopy(IntArray11));
  ms.Instance := TMultiSet.Create(50, e);
  AssertTrue(ms.Instance.Count = 11);
  AssertTrue(ms.Instance.Capacity >= 50);
  AssertTrue(ms.Instance.LoadFactor = TMultiSet.DefaultLoadFactor);
  for I in IntArray11 do
    AssertTrue(ms.Instance.Contains(I));
end;

procedure THashMultisetLPTTest.CreateLoadFactor0;
var
  ms: TAutoMultiSet;
begin
  ms.Instance := TMultiSet.Create(0.0);
  AssertTrue(ms.Instance.Count = 0);
  AssertTrue(ms.Instance.LoadFactor = TMultiSet.MinLoadFactor);
end;

procedure THashMultisetLPTTest.CreateLoadFactor3;
var
  ms: TAutoMultiSet;
begin
  ms.Instance := TMultiSet.Create(3.0);
  AssertTrue(ms.Instance.Count = 0);
  AssertTrue(ms.Instance.LoadFactor = TMultiSet.MaxLoadFactor);
end;

procedure THashMultisetLPTTest.CreateLoadFactor;
var
  ms: TAutoMultiSet;
  lf: Double;
begin
  lf := 0.5;
  ms.Instance := TMultiSet.Create(lf);
  AssertTrue(ms.Instance.Count = 0);
  AssertTrue(ms.Instance.LoadFactor = lf);
end;

procedure THashMultisetLPTTest.CreateLoadFactor075Array11;
var
  ms: TAutoMultiSet;
  I: Integer;
  lf: Single;
begin
  lf := 0.75;
  ms.Instance := TMultiSet.Create(lf, IntArray11);
  AssertTrue(ms.Instance.Count = 11);
  AssertTrue(ms.Instance.Capacity >= 11);
  AssertTrue(ms.Instance.LoadFactor = lf);
  for I in IntArray11 do
    AssertTrue(ms.Instance.Contains(I));
end;

procedure THashMultisetLPTTest.CreateLoadFactor075Enum11;
var
  ms: TAutoMultiSet;
  I: Integer;
  e: IIntEnumerable;
  lf: Single;
begin
  lf := 0.75;
  e := TIntArrayCursor.Create(TIntHelper.CreateCopy(IntArray11));
  ms.Instance := TMultiSet.Create(lf, e);
  AssertTrue(ms.Instance.Count = 11);
  AssertTrue(ms.Instance.Capacity >= 11);
  AssertTrue(ms.Instance.LoadFactor = lf);
  for I in IntArray11 do
    AssertTrue(ms.Instance.Contains(I));
end;

procedure THashMultisetLPTTest.CreateCapacityLoadFactor;
var
  ms: TAutoMultiSet;
  lf: Single;
begin
  lf := 0.45;
  ms.Instance := TMultiSet.Create(50, lf);
  AssertTrue(ms.Instance.Count = 0);
  AssertTrue(ms.Instance.Capacity >= 50);
  AssertTrue(ms.Instance.LoadFactor = lf);
end;

procedure THashMultisetLPTTest.CreateCapacityLoadFactor075Array11;
var
  ms: TAutoMultiSet;
  I: Integer;
  lf: Single;
begin
  lf := 0.75;
  ms.Instance := TMultiSet.Create(50, lf, IntArray11);
  AssertTrue(ms.Instance.Count = 11);
  AssertTrue(ms.Instance.Capacity >= 50);
  AssertTrue(ms.Instance.LoadFactor = lf);
  for I in IntArray11 do
    AssertTrue(ms.Instance.Contains(I));
end;

procedure THashMultisetLPTTest.CreateCapacityLoadFactor075Enum11;
var
  ms: TAutoMultiSet;
  I: Integer;
  e: IIntEnumerable;
  lf: Single;
begin
  lf := 0.75;
  e := TIntArrayCursor.Create(TIntHelper.CreateCopy(IntArray11));
  ms.Instance := TMultiSet.Create(50, lf, e);
  AssertTrue(ms.Instance.Count = 11);
  AssertTrue(ms.Instance.Capacity >= 50);
  AssertTrue(ms.Instance.LoadFactor = lf);
  for I in IntArray11 do
    AssertTrue(ms.Instance.Contains(I));
end;

procedure THashMultisetLPTTest.Clear;
var
  ms: TAutoMultiSet;
begin
  ms.Instance := TMultiSet.Create(IntArray11);
  AssertTrue(ms.Instance.Count = 11);
  AssertTrue(ms.Instance.Capacity >= 11);
  ms.Instance.Clear;
  AssertTrue(ms.Instance.Count = 0);
  AssertTrue(ms.Instance.Capacity = 0);
end;

procedure THashMultisetLPTTest.EnsureCapacity;
var
  ms: TAutoMultiSet;
  c: SizeInt;
begin
  c := ms.Instance.ExpandTreshold;
  ms.Instance.EnsureCapacity(c + 100);
  AssertTrue(ms.Instance.ExpandTreshold >= c + 100);
end;

procedure THashMultisetLPTTest.TrimToFit;
var
  ms: TAutoMultiSet;
  c: SizeInt;
begin
  ms.Instance := TMultiSet.Create(0.8, IntArray11);
  ms.Instance.EnsureCapacity(40);
  c := ms.Instance.Capacity;
  ms.Instance.TrimToFit;
  AssertTrue(ms.Instance.Capacity < c);
end;

procedure THashMultisetLPTTest.Add;
var
  ms: TAutoMultiSet;
begin
  AssertTrue(ms.Instance.Count = 0);
  AssertTrue(ms.Instance.Add(1));
  AssertTrue(ms.Instance.Count = 1);
  AssertTrue(ms.Instance.Contains(1));
  AssertTrue(ms.Instance.Add(51));
  AssertTrue(ms.Instance.Contains(51));
  AssertTrue(ms.Instance.Count = 2);
  AssertTrue(ms.Instance.EntryCount = 2);
  AssertTrue(ms.Instance.Add(1));
  AssertTrue(ms.Instance.Add(51));
  AssertTrue(ms.Instance.Count = 4);
  AssertTrue(ms.Instance.EntryCount = 2);
  AssertTrue(ms.Instance.Add(52));
  AssertTrue(ms.Instance.Contains(52));
  AssertTrue(ms.Instance.Count = 5);
  AssertTrue(ms.Instance.EntryCount = 3);
  ms.Instance[101] := 10;
  AssertTrue(ms.Instance.Count = 15);
  AssertTrue(ms.Instance.EntryCount = 4);
end;

procedure THashMultisetLPTTest.Add100;
var
  ms: TAutoMultiSet;
  I: Integer;
begin
  for I := 1 to 100 do
    AssertTrue(ms.Instance.Add(I));
  AssertTrue(ms.Instance.Count = 100);
  for I := 1 to 100 do
    AssertTrue(ms.Instance.Add(I));
  AssertTrue(ms.Instance.Count = 200);
  AssertTrue(ms.Instance.EntryCount = 100);
  for I := 1 to 100 do
    AssertTrue(ms.Instance.Contains(I));
  ms.Instance[15] := 52;
  AssertTrue(ms.Instance.Count = 250);
  ms.Instance[15] := 12;
  AssertTrue(ms.Instance.Count = 210);
end;

procedure THashMultisetLPTTest.AddArray;
var
  ms: TAutoMultiSet;
begin
  AssertTrue(ms.Instance.AddAll(IntArray21) = System.Length(IntArray21));
  AssertTrue(ms.Instance.Count = System.Length(IntArray21));
  AssertTrue(ms.Instance.EntryCount = System.Length(IntArray21));
  AssertTrue(ms.Instance.ContainsAll(IntArray21));
  AssertTrue(ms.Instance.AddAll(IntArray21) = System.Length(IntArray21));
  AssertTrue(ms.Instance.Count = System.Length(IntArray21) * 2);
  AssertTrue(ms.Instance.EntryCount = System.Length(IntArray21));
  AssertTrue(ms.Instance.ContainsAll(IntArray21));
end;

procedure THashMultisetLPTTest.AddEnum;
var
  ms: TAutoMultiSet;
  e: IIntEnumerable;
begin
  e := TIntArrayCursor.Create(TIntHelper.CreateCopy(IntArray21));
  AssertTrue(ms.Instance.AddAll(e) = System.Length(IntArray21));
  AssertTrue(ms.Instance.Count = System.Length(IntArray21));
  AssertTrue(ms.Instance.EntryCount = System.Length(IntArray21));
  AssertTrue(ms.Instance.ContainsAll(IntArray21));
  e := TIntArrayCursor.Create(TIntHelper.CreateCopy(IntArray21));
  AssertTrue(ms.Instance.AddAll(e) = System.Length(IntArray21));
  AssertTrue(ms.Instance.Count = System.Length(IntArray21) * 2);
  AssertTrue(ms.Instance.EntryCount = System.Length(IntArray21));
  AssertTrue(ms.Instance.ContainsAll(IntArray21));
end;

procedure THashMultisetLPTTest.AddSelf;
var
  ms: TAutoMultiSet;
begin
  ms.Instance.AddAll(IntArray21);
  AssertTrue(ms.Instance.AddAll(ms.Instance) = System.Length(IntArray21));
  AssertTrue(ms.Instance.Count = System.Length(IntArray21) * 2);
  AssertTrue(ms.Instance.EntryCount = System.Length(IntArray21));
end;

procedure THashMultisetLPTTest.Remove;
var
  ms: TAutoMultiSet;
  c: SizeInt;
begin
  c := System.Length(IntArray21) * 2;
  ms.Instance := TMultiSet.Create(IntArray21);
  ms.Instance.AddAll(IntArray21);
  AssertTrue(ms.Instance.Count = c);
  AssertTrue(ms.Instance.EntryCount = System.Length(IntArray21));
  AssertTrue(ms.Instance.Remove(IntArray21[1]));
  AssertTrue(ms.Instance.Count = c - 1);
  AssertTrue(ms.Instance.EntryCount = System.Length(IntArray21));
  AssertTrue(ms.Instance.Remove(IntArray21[System.High(IntArray21)]));
  AssertTrue(ms.Instance.Count = c - 2);
  AssertTrue(ms.Instance.EntryCount = System.Length(IntArray21));
  AssertTrue(ms.Instance.Remove(IntArray21[1]));
  AssertTrue(ms.Instance.Count = c - 3);
  AssertTrue(ms.Instance.EntryCount = System.Length(IntArray21) - 1);
  AssertFalse(ms.Instance.Remove(-1));
  AssertTrue(ms.Instance.Count = c - 3);
  AssertFalse(ms.Instance.Remove(100));
  AssertTrue(ms.Instance.Count = c - 3);
end;

procedure THashMultisetLPTTest.RemoveArray;
var
  ms: TAutoMultiSet;
begin
  ms.Instance := TMultiSet.Create(IntArray21);
  ms.Instance.AddAll(IntArray21);
  AssertTrue(ms.Instance.RemoveAll(IntArray11) = 11);
  AssertTrue(ms.Instance.Count = 31);
  AssertTrue(ms.Instance.EntryCount = 21);
  AssertTrue(ms.Instance.RemoveAll(IntArray10) = 10);
  AssertTrue(ms.Instance.Count = 21);
  AssertTrue(ms.Instance.EntryCount = 21);
end;

procedure THashMultisetLPTTest.RemoveEnum;
var
  ms: TAutoMultiSet;
  e: IIntEnumerable;
begin
  ms.Instance := TMultiSet.Create(IntArray21);
  ms.Instance.AddAll(IntArray21);
  e := TIntArrayCursor.Create(TIntHelper.CreateCopy(IntArray11));
  AssertTrue(ms.Instance.RemoveAll(e) = 11);
  AssertTrue(ms.Instance.Count = 31);
  AssertTrue(ms.Instance.EntryCount = 21);
  e := TIntArrayCursor.Create(TIntHelper.CreateCopy(IntArray10));
  AssertTrue(ms.Instance.RemoveAll(e) = 10);
  AssertTrue(ms.Instance.Count = 21);
  AssertTrue(ms.Instance.EntryCount = 21);
end;

procedure THashMultisetLPTTest.RemoveSelf;
var
  ms: TAutoMultiSet;
begin
  ms.Instance := TMultiSet.Create(IntArray21);
  ms.Instance.AddAll(IntArray21);
  AssertTrue(ms.Instance.RemoveAll(ms.Instance) = 42);
  AssertTrue(ms.Instance.IsEmpty);
end;

procedure THashMultisetLPTTest.RemoveIfRegular;
var
  ms: TAutoMultiSet;
  I: Integer;
begin
  ms.Instance := TMultiSet.Create(IntArray21);
  ms.Instance.AddAll(IntArray21);
  AssertTrue(ms.Instance.RemoveIf(@IsEvenInt) = 20);
  AssertTrue(ms.Instance.Count = 22);
  AssertTrue(ms.Instance.ContainsAll(IntArray11));
  for I in IntArray11 do
    AssertTrue(ms.Instance[I] = 2);
end;

procedure THashMultisetLPTTest.RemoveIfDelegated;
var
  ms: TAutoMultiSet;
  I: Integer;
begin
  ms.Instance := TMultiSet.Create(IntArray21);
  ms.Instance.AddAll(IntArray21);
  AssertTrue(ms.Instance.RemoveIf(@IsEven) = 20);
  AssertTrue(ms.Instance.Count = 22);
  AssertTrue(ms.Instance.ContainsAll(IntArray11));
  for I in IntArray11 do
    AssertTrue(ms.Instance[I] = 2);
end;

procedure THashMultisetLPTTest.RemoveIfNested;
  function IsEvenInt(const aValue: Integer): Boolean;
  begin
    Result := not Odd(aValue);
  end;
var
  ms: TAutoMultiSet;
  I: Integer;
begin
  ms.Instance := TMultiSet.Create(IntArray21);
  ms.Instance.AddAll(IntArray21);
  AssertTrue(ms.Instance.RemoveIf(@IsEvenInt) = 20);
  AssertTrue(ms.Instance.Count = 22);
  AssertTrue(ms.Instance.ContainsAll(IntArray11));
  for I in IntArray11 do
    AssertTrue(ms.Instance[I] = 2);
end;

procedure THashMultisetLPTTest.ExtractIfRegular;
var
  ms, ms1: TAutoMultiSet;
  e: TIntArray;
  I: Integer;
begin
  ms.Instance := TMultiSet.Create(IntArray21);
  ms.Instance.AddAll(IntArray21);
  e := ms.Instance.ExtractIf(@IsEvenInt);
  AssertTrue(ms.Instance.Count = 22);
  AssertTrue(ms.Instance.ContainsAll(IntArray11));
  AssertTrue(System.Length(e) = 20);
  ms1.Instance := TMultiSet.Create(e);
  for I in IntArray11 do
    AssertTrue(ms.Instance[I] = 2);
  for I in IntArray10 do
    AssertTrue(ms1.Instance[I] = 2);
end;

procedure THashMultisetLPTTest.ExtractIfDelegated;
var
  ms, ms1: TAutoMultiSet;
  e: TIntArray;
  I: Integer;
begin
  ms.Instance := TMultiSet.Create(IntArray21);
  ms.Instance.AddAll(IntArray21);
  e := ms.Instance.ExtractIf(@IsEven);
  AssertTrue(ms.Instance.Count = 22);
  AssertTrue(ms.Instance.ContainsAll(IntArray11));
  AssertTrue(System.Length(e) = 20);
  ms1.Instance := TMultiSet.Create(e);
  for I in IntArray11 do
    AssertTrue(ms.Instance[I] = 2);
  for I in IntArray10 do
    AssertTrue(ms1.Instance[I] = 2);
end;

procedure THashMultisetLPTTest.ExtractIfNested;
  function IsEvenInt(const aValue: Integer): Boolean;
  begin
    Result := not Odd(aValue);
  end;
var
  ms, ms1: TAutoMultiSet;
  e: TIntArray;
  I: Integer;
begin
  ms.Instance := TMultiSet.Create(IntArray21);
  ms.Instance.AddAll(IntArray21);
  e := ms.Instance.ExtractIf(@IsEvenInt);
  AssertTrue(ms.Instance.Count = 22);
  AssertTrue(ms.Instance.ContainsAll(IntArray11));
  AssertTrue(System.Length(e) = 20);
  ms1.Instance := TMultiSet.Create(e);
  for I in IntArray11 do
    AssertTrue(ms.Instance[I] = 2);
  for I in IntArray10 do
    AssertTrue(ms1.Instance[I] = 2);
end;

procedure THashMultisetLPTTest.Retain;
var
  ms, ms1: TAutoMultiSet;
  I: Integer;
begin
  ms.Instance := TMultiSet.Create(IntArray21);
  ms.Instance.AddAll(IntArray21);
  ms1.Instance := TMultiSet.Create(IntArray11);
  AssertTrue(ms.Instance.Count = 42);
  ms.Instance.RetainAll(ms1.Instance);
  AssertTrue(ms.Instance.Count = 22);
  AssertTrue(ms.Instance.ContainsAll(ms1.Instance));
  for I in IntArray11 do
    AssertTrue(ms.Instance[I] = 2);
end;

procedure THashMultisetLPTTest.Retain_1;
var
  ms, ms1: TAutoMultiSet;
begin
  ms.Instance := TMultiSet.Create(IntArray10);
  ms.Instance.AddAll(IntArray10);
  AssertTrue(ms.Instance.Count = 20);
  ms1.Instance := TMultiSet.Create(IntArray11);
  ms.Instance.RetainAll(ms1.Instance);
  AssertTrue(ms.Instance.IsEmpty);
end;

procedure THashMultisetLPTTest.RetainSelf;
var
  ms: TAutoMultiSet;
begin
  ms.Instance := TMultiSet.Create(IntArray21);
  ms.Instance.AddAll(IntArray21);
  AssertTrue(ms.Instance.Count = 42);
  ms.Instance.RetainAll(ms.Instance);
  AssertTrue(ms.Instance.Count = 42);
  AssertTrue(ms.Instance.ContainsAll(IntArray21));
end;

procedure THashMultisetLPTTest.IsSuperset;
var
  ms, ms1: TAutoMultiSet;
begin
  ms.Instance := TMultiSet.Create(IntArray21);
  ms.Instance.AddAll(IntArray21);
  ms1.Instance := TMultiSet.Create(ms.Instance);
  AssertTrue(ms.Instance.IsSuperSet(ms1.Instance));
  AssertTrue(ms1.Instance.IsSuperSet(ms.Instance));
  AssertTrue(ms.Instance.IsSuperSet(ms.Instance));
  ms1.Instance.Remove(IntArray21[1]);
  AssertTrue(ms.Instance.IsSuperSet(ms1.Instance));
  AssertFalse(ms1.Instance.IsSuperSet(ms.Instance));
  ms1.Instance.Clear;
  AssertTrue(ms.Instance.IsSuperSet(ms1.Instance));
  AssertFalse(ms1.Instance.IsSuperSet(ms.Instance));
  ms.Instance.Clear;
  AssertTrue(ms.Instance.IsSuperSet(ms1.Instance));
  AssertTrue(ms1.Instance.IsSuperSet(ms.Instance));
end;

procedure THashMultisetLPTTest.IsSubset;
var
  ms, ms1: TAutoMultiSet;
begin
  ms.Instance := TMultiSet.Create(IntArray21);
  ms.Instance.AddAll(IntArray21);
  ms1.Instance := TMultiSet.Create(ms.Instance);
  AssertTrue(ms.Instance.IsSubSet(ms1.Instance));
  AssertTrue(ms1.Instance.IsSubSet(ms.Instance));
  AssertTrue(ms.Instance.IsSubSet(ms.Instance));
  ms1.Instance.Remove(IntArray21[1]);
  AssertFalse(ms.Instance.IsSubSet(ms1.Instance));
  AssertTrue(ms1.Instance.IsSubSet(ms.Instance));
  ms1.Instance.Clear;
  AssertFalse(ms.Instance.IsSubSet(ms1.Instance));
  AssertTrue(ms1.Instance.IsSubSet(ms.Instance));
  ms.Instance.Clear;
  AssertTrue(ms1.Instance.IsSubSet(ms.Instance));
  AssertTrue(ms.Instance.IsSubSet(ms1.Instance));
end;

procedure THashMultisetLPTTest.IsEqual;
var
  ms, ms1: TAutoMultiSet;
begin
  ms.Instance := TMultiSet.Create(IntArray21);
  ms.Instance.AddAll(IntArray21);
  ms1.Instance := TMultiSet.Create(ms.Instance);
  AssertTrue(ms.Instance.IsEqual(ms1.Instance));
  AssertTrue(ms1.Instance.IsEqual(ms.Instance));
  AssertTrue(ms.Instance.IsEqual(ms.Instance));
  ms1.Instance.Remove(IntArray21[1]);
  AssertFalse(ms.Instance.IsEqual(ms1.Instance));
  AssertFalse(ms1.Instance.IsEqual(ms.Instance));
  ms1.Instance.Clear;
  ms.Instance.Clear;
  AssertTrue(ms.Instance.IsEqual(ms1.Instance));
  AssertTrue(ms1.Instance.IsEqual(ms.Instance));
end;

procedure THashMultisetLPTTest.Intersecting;
var
  ms, ms1: TAutoMultiSet;
begin
  ms.Instance := TMultiSet.Create(IntArray21);
  ms1.Instance := TMultiSet.Create(IntArray11);
  AssertTrue(ms.Instance.Intersecting(ms.Instance));
  AssertTrue(ms.Instance.Intersecting(ms1.Instance));
  ms.Instance := TMultiSet.Create(IntArray10);
  AssertFalse(ms.Instance.Intersecting(ms1.Instance));
  AssertFalse(ms1.Instance.Intersecting(ms.Instance));
end;

procedure THashMultisetLPTTest.Intersect;
var
  ms, ms1: TAutoMultiSet;
  I: Integer;
begin
  ms.Instance := TMultiSet.Create(IntArray21);
  ms.Instance.AddAll(IntArray21);
  AssertTrue(ms.Instance.Count = 42);
  ms1.Instance := TMultiSet.Create(IntArray11);
  ms1.Instance.AddAll([-4, -3, -2, -1, 0]);
  ms.Instance.Intersect(ms1.Instance);
  AssertTrue(ms.Instance.Count = 11);
  for I in IntArray11 do
    AssertTrue(ms.Instance[I] = 1);
end;

procedure THashMultisetLPTTest.Join;
var
  ms, ms1: TAutoMultiSet;
  I: Integer;
begin
  ms1.Instance := TMultiSet.Create(IntArray11);
  ms.Instance.Join(ms1.Instance);
  AssertTrue(ms.Instance.IsEqual(ms1.Instance));
  ms1.Instance := TMultiSet.Create(IntArray10);
  ms1.Instance.AddAll(ms1.Instance);
  ms.Instance.Join(ms1.Instance);
  for I in IntArray11 do
    AssertTrue(ms.Instance[I] = 1);
  for I in IntArray10 do
    AssertTrue(ms.Instance[I] = 2);
end;

procedure THashMultisetLPTTest.ArithmeticAdd;
var
  ms, ms1: TAutoMultiSet;
  I: Integer;
begin
  ms1.Instance := TMultiSet.Create(IntArray11);
  ms.Instance.ArithmeticAdd(ms1.Instance);
  AssertTrue(ms.Instance.IsEqual(ms1.Instance));
  ms.Instance.AddAll(IntArray10);
  ms.Instance.ArithmeticAdd(ms1.Instance);
  for I in IntArray11 do
    AssertTrue(ms.Instance[I] = 2);
  for I in IntArray10 do
    AssertTrue(ms.Instance[I] = 1);
  ms.Instance.ArithmeticAdd(ms.Instance);
  for I in IntArray11 do
    AssertTrue(ms.Instance[I] = 4);
  for I in IntArray10 do
    AssertTrue(ms.Instance[I] = 2);
end;

procedure THashMultisetLPTTest.ArithmeticSubtract;
var
  ms, ms1: TAutoMultiSet;
begin
  ms1.Instance := TMultiSet.Create(IntArray11);
  ms.Instance.ArithmeticSubtract(ms1.Instance);
  AssertTrue(ms.Instance.IsEmpty);
  ms.Instance.AddAll(IntArray10);
  ms.Instance.ArithmeticSubtract(ms1.Instance);
  AssertTrue(ms.Instance.Count = 10);
  AssertTrue(ms.Instance.ContainsAll(IntArray10));
  ms.Instance.AddAll(IntArray11);
  AssertTrue(ms.Instance.ContainsAll(IntArray21));
  ms.Instance.ArithmeticSubtract(ms1.Instance);
  AssertTrue(ms.Instance.Count = 10);
  AssertTrue(ms.Instance.ContainsAll(IntArray10));
  ms1.Instance.AddAll(IntArray10);
  ms.Instance.ArithmeticSubtract(ms1.Instance);
  AssertTrue(ms.Instance.IsEmpty);
end;

procedure THashMultisetLPTTest.SymmetricSubtract;
var
  ms, ms1: TAutoMultiSet;
begin
  ms1.Instance := TMultiSet.Create(IntArray11);
  ms.Instance.SymmetricSubtract(ms1.Instance);
  AssertTrue(ms.Instance.Count = 11);
  AssertTrue(ms.Instance.ContainsAll(IntArray11));
  ms.Instance.AddAll(IntArray10);
  ms.Instance.SymmetricSubtract(ms1.Instance);
  AssertTrue(ms.Instance.Count = 10);
  AssertTrue(ms.Instance.ContainsAll(IntArray10));
  ms.Instance.AddAll(IntArray11);
  ms.Instance.AddAll(IntArray21);
  AssertTrue(ms.Instance.Count = 42);
  ms1.Instance.AddAll(IntArray10);
  ms.Instance.SymmetricSubtract(ms1.Instance);
  AssertTrue(ms.Instance.Count = 21);
  AssertTrue(ms.Instance.ContainsAll(IntArray21));
  ms.Instance.SymmetricSubtract(ms1.Instance);
  AssertTrue(ms.Instance.IsEmpty);
end;

procedure THashMultisetLPTTest.ObjectMultiSetRemove;
var
  ms: TAutoObjMultiSet;
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
      ms.Instance.Add(a[I]);
    end;
  AssertTrue(ms.Instance.Count = TestSize);
  ms.Instance.AddAll(a);
  AssertTrue(ms.Instance.Count = TestSize * 2);
  for I := 0 to System.High(a) do
    AssertTrue(ms.Instance.Remove(a[I]));
  AssertTrue(ms.Instance.Count = TestSize);
  AssertTrue(Counter.Count = 0);
  for I := 0 to System.High(a) do
    begin
      AssertTrue(ms.Instance.Remove(a[I]));
      AssertTrue(ms.Instance.Count = Pred(TestSize - I));
      AssertTrue(Counter.Count = Succ(I));
    end;
  AssertTrue(ms.Instance.IsEmpty);
  AssertTrue(Counter.Count = TestSize);
end;

procedure THashMultisetLPTTest.ObjectMultiSetRemoveArray;
var
  ms: TAutoObjMultiSet;
  a: TObjArray;
  I, TestSize: Integer;
  Counter: TCounter;
begin
  TestSize := 56;
  Counter := Default(TCounter);
  for I := 1 to TestSize do
    ms.Instance.Add(TTestObj.Create(Counter.Increment));
  AssertTrue(ms.Instance.Count = TestSize);
  a := ms.Instance.ToArray;
  AssertTrue(System.Length(a) = TestSize);
  ms.Instance.AddAll(a);
  AssertTrue(ms.Instance.Count = TestSize * 2);
  AssertTrue(ms.Instance.RemoveAll(a) = TestSize);
  AssertTrue(Counter.Count = 0);
  AssertTrue(ms.Instance.RemoveAll(a) = TestSize);
  AssertTrue(ms.Instance.IsEmpty);
  AssertTrue(Counter.Count = TestSize);
end;

procedure THashMultisetLPTTest.ObjectMultiSetRemoveEnum;
type
  TFakeEnum = specialize TGArrayCursor<TTestObj>;
var
  ms: TAutoObjMultiSet;
  a: TObjArray;
  e: TFakeEnum;
  I, TestSize: Integer;
  Counter: TCounter;
begin
  TestSize := 52;
  Counter := Default(TCounter);
  for I := 1 to TestSize do
    ms.Instance.Add(TTestObj.Create(Counter.Increment));
  AssertTrue(ms.Instance.Count = TestSize);
  a := ms.Instance.ToArray;
  AssertTrue(System.Length(a) = TestSize);
  ms.Instance.AddAll(a);
  AssertTrue(ms.Instance.Count = TestSize * 2);
  e := TFakeEnum.Create(a);
  AssertTrue(ms.Instance.RemoveAll(e) = TestSize);
  AssertTrue(Counter.Count = 0);
  e := TFakeEnum.Create(a);
  AssertTrue(ms.Instance.RemoveAll(e) = TestSize);
  AssertTrue(ms.Instance.IsEmpty);
  AssertTrue(Counter.Count = TestSize);
end;

procedure THashMultisetLPTTest.ObjectMultiSetRemoveSelf;
var
  ms: TAutoObjMultiSet;
  I, TestSize: Integer;
  Counter: TCounter;
begin
  TestSize := 100;
  Counter := Default(TCounter);
  for I := 1 to TestSize do
    ms.Instance.Add(TTestObj.Create(Counter.Increment));
  ms.Instance.AddAll(ms.Instance);
  AssertTrue(ms.Instance.Count = TestSize * 2);
  AssertTrue(ms.Instance.RemoveAll(ms.Instance) = TestSize * 2);
  AssertTrue(ms.Instance.IsEmpty);
  AssertTrue(Counter.Count = TestSize);
end;

procedure THashMultisetLPTTest.ObjectMultiSetRemoveIfRegular;
var
  ms: TAutoObjMultiSet;
  I, TestSize: Integer;
  Counter: TCounter;
  o: TTestObj;
begin
  TestSize := 100;
  Counter := Default(TCounter);
  for I := 1 to TestSize do
    ms.Instance.Add(TTestObj.Create(I, Counter.Increment));
  ms.Instance.AddAll(ms.Instance);
  AssertTrue(ms.Instance.Count = TestSize * 2);
  AssertTrue(ms.Instance.RemoveIf(@IsEvenObj) = TestSize);
  AssertTrue(Counter.Count = TestSize div 2);
  AssertTrue(ms.Instance.Count = TestSize);
  for o in ms.Instance do
    AssertTrue(Odd(o.Value));
  ms.Instance := Default(TObjMultiSet);
  AssertTrue(Counter.Count = TestSize);
end;

procedure THashMultisetLPTTest.ObjectMultiSetRemoveIfDelegated;
var
  ms: TAutoObjMultiSet;
  I, TestSize: Integer;
  Counter: TCounter;
  o: TTestObj;
begin
  TestSize := 100;
  Counter := Default(TCounter);
  for I := 1 to TestSize do
    ms.Instance.Add(TTestObj.Create(I, Counter.Increment));
  ms.Instance.AddAll(ms.Instance);
  AssertTrue(ms.Instance.Count = TestSize * 2);
  AssertTrue(ms.Instance.RemoveIf(@ObjIsEven) = TestSize);
  AssertTrue(Counter.Count = TestSize div 2);
  AssertTrue(ms.Instance.Count = TestSize);
  for o in ms.Instance do
    AssertTrue(Odd(o.Value));
  ms.Instance := Default(TObjMultiSet);
  AssertTrue(Counter.Count = TestSize);
end;

procedure THashMultisetLPTTest.ObjectMultiSetRemoveIfNested;
  function IsEvenNest(const aObj: TTestObj): Boolean;
  begin
    Result := not Odd(aObj.Value);
  end;
var
  ms: TAutoObjMultiSet;
  I, TestSize: Integer;
  Counter: TCounter;
  o: TTestObj;
begin
  TestSize := 100;
  Counter := Default(TCounter);
  for I := 1 to TestSize do
    ms.Instance.Add(TTestObj.Create(I, Counter.Increment));
  ms.Instance.AddAll(ms.Instance);
  AssertTrue(ms.Instance.Count = TestSize * 2);
  AssertTrue(ms.Instance.RemoveIf(@IsEvenNest) = TestSize);
  AssertTrue(Counter.Count = TestSize div 2);
  AssertTrue(ms.Instance.Count = TestSize);
  for o in ms.Instance do
    AssertTrue(Odd(o.Value));
  ms.Instance := Default(TObjMultiSet);
  AssertTrue(Counter.Count = TestSize);
end;

procedure THashMultisetLPTTest.ObjectMultiSetExtractIfRegular;
var
  ms: TAutoObjMultiSet;
  I, TestSize: Integer;
  Counter: TCounter;
  a: TObjArray;
  o: TTestObj;
begin
  TestSize := 100;
  Counter := Default(TCounter);
  for I := 1 to TestSize do
    ms.Instance.Add(TTestObj.Create(I, Counter.Increment));
  ms.Instance.AddAll(ms.Instance);
  AssertTrue(ms.Instance.Count = TestSize * 2);
  a := ms.Instance.ExtractIf(@IsEvenObj);
  AssertTrue(System.Length(a) = TestSize);
  AssertTrue(Counter.Count = 0);
  AssertTrue(ms.Instance.Count = TestSize);
  for o in ms.Instance do
    AssertTrue(Odd(o.Value));
  for o in a do
    AssertTrue(not Odd(o.Value));
  ms.Instance.Clear;
  AssertTrue(Counter.Count = TestSize div 2);
  ms.Instance.AddAll(a);
  ms.Instance.Clear;
  AssertTrue(Counter.Count = TestSize);
end;

procedure THashMultisetLPTTest.ObjectMultiSetExtractIfDelegated;
var
  ms: TAutoObjMultiSet;
  I, TestSize: Integer;
  Counter: TCounter;
  a: TObjArray;
  o: TTestObj;
begin
  TestSize := 100;
  Counter := Default(TCounter);
  for I := 1 to TestSize do
    ms.Instance.Add(TTestObj.Create(I, Counter.Increment));
  ms.Instance.AddAll(ms.Instance);
  AssertTrue(ms.Instance.Count = TestSize * 2);
  a := ms.Instance.ExtractIf(@ObjIsEven);
  AssertTrue(System.Length(a) = TestSize);
  AssertTrue(Counter.Count = 0);
  AssertTrue(ms.Instance.Count = TestSize);
  for o in ms.Instance do
    AssertTrue(Odd(o.Value));
  for o in a do
    AssertTrue(not Odd(o.Value));
  ms.Instance.Clear;
  AssertTrue(Counter.Count = TestSize div 2);
  ms.Instance.AddAll(a);
  ms.Instance.Clear;
  AssertTrue(Counter.Count = TestSize);
end;

procedure THashMultisetLPTTest.ObjectMultiSetExtractIfNested;
  function IsEvenNest(const aObj: TTestObj): Boolean;
  begin
    Result := not Odd(aObj.Value);
  end;
var
  ms: TAutoObjMultiSet;
  I, TestSize: Integer;
  Counter: TCounter;
  a: TObjArray;
  o: TTestObj;
begin
  TestSize := 100;
  Counter := Default(TCounter);
  for I := 1 to TestSize do
    ms.Instance.Add(TTestObj.Create(I, Counter.Increment));
  ms.Instance.AddAll(ms.Instance);
  AssertTrue(ms.Instance.Count = TestSize * 2);
  a := ms.Instance.ExtractIf(@IsEvenNest);
  AssertTrue(System.Length(a) = TestSize);
  AssertTrue(Counter.Count = 0);
  AssertTrue(ms.Instance.Count = TestSize);
  for o in ms.Instance do
    AssertTrue(Odd(o.Value));
  for o in a do
    AssertTrue(not Odd(o.Value));
  ms.Instance.Clear;
  AssertTrue(Counter.Count = TestSize div 2);
  ms.Instance.AddAll(a);
  ms.Instance.Clear;
  AssertTrue(Counter.Count = TestSize);
end;

procedure THashMultisetLPTTest.ObjectMultiSetRetain;
type
  TObjSet     = specialize TGObjHashSetLP<TTestObj>;
  TAutoObjSet = specialize TGAutoRef<TObjSet>;
var
  ms: TAutoObjMultiSet;
  s1: TAutoObjSet;
  I, TestSize: Integer;
  Counter: TCounter;
  a: TObjArray;
begin
  TestSize := 100;
  Counter := Default(TCounter);
  System.SetLength(a, TestSize);
  s1.Instance := TObjSet.Create(False);

  for I := 0 to System.High(a) do
    a[I] := TTestObj.Create(Counter.Increment);
  s1.Instance.AddAll(a);
  ms.Instance.AddAll(a);
  ms.Instance.AddAll(a);
  for I := 1 to TestSize do
    ms.Instance.Add(TTestObj.Create(Counter.Increment));
  AssertTrue(ms.Instance.Count = TestSize * 3);
  ms.Instance.RetainAll(s1.Instance);
  AssertTrue(ms.Instance.Count = TestSize * 2);
  AssertTrue(Counter.Count = TestSize);
  AssertTrue(ms.Instance.ContainsAll(s1.Instance));
  ms.Instance := Default(TObjMultiSet);
  AssertTrue(Counter.Count = TestSize * 2);
end;

procedure THashMultisetLPTTest.ObjectMultiSetIntersect;
var
  ms, ms1: TAutoObjMultiSet;
  I, TestSize: Integer;
  Counter: TCounter;
  a: TObjArray;
  o: TTestObj;
begin
  TestSize := 50;
  Counter := Default(TCounter);
  System.SetLength(a, TestSize);
  for I := 0 to System.High(a) do
    a[I] := TTestObj.Create(Counter.Increment);

  ms.Instance.AddAll(a);
  ms.Instance.AddAll(a);

  for o in a do
    AssertTrue(ms.Instance[o] = 2);

  for I := 1 to TestSize do
    ms.Instance.Add(TTestObj.Create(Counter.Increment));

  AssertTrue(Counter.Count = 0);
  AssertTrue(ms.Instance.Count = TestSize * 3);

  ms1.Instance.OwnsObjects := False;
  AssertTrue(ms1.Instance.AddAll(a) = TestSize);

  ms.Instance.Intersect(ms1.Instance);

  AssertTrue(ms.Instance.Count = TestSize);
  AssertTrue(Counter.Count = TestSize);

  for o in a do
    AssertTrue(ms.Instance[o] = 1);
end;

procedure THashMultisetLPTTest.ObjectMultiSetArithmeticSubtract;
var
  ms, ms1: TAutoObjMultiSet;
  I, TestSize: Integer;
  Counter: TCounter;
  a: TObjArray;
  o: TTestObj;
begin
  TestSize := 55;
  Counter := Default(TCounter);
  System.SetLength(a, TestSize);
  for I := 0 to System.High(a) do
    a[I] := TTestObj.Create(Counter.Increment);

  ms.Instance.AddAll(a);
  ms.Instance.AddAll(a);
  ms.Instance.AddAll(a);

  ms1.Instance.OwnsObjects := False;

  ms1.Instance.AddAll(a);

  for I := 1 to TestSize do
    ms.Instance.Add(TTestObj.Create(Counter.Increment));

  AssertTrue(Counter.Count = 0);
  AssertTrue(ms.Instance.Count = TestSize * 4);
  for o in a do
    AssertTrue(ms.Instance[o] = 3);

  ms.Instance.ArithmeticSubtract(ms1.Instance);
  AssertTrue(ms.Instance.Count = TestSize * 3);
  for o in a do
    AssertTrue(ms.Instance[o] = 2);

  ms.Instance.ArithmeticSubtract(ms1.Instance);
  AssertTrue(ms.Instance.Count = TestSize * 2);
  for o in a do
    AssertTrue(ms.Instance[o] = 1);

  ms.Instance.ArithmeticSubtract(ms1.Instance);
  AssertTrue(ms.Instance.Count = TestSize);

  AssertTrue(Counter.Count = TestSize);
end;

procedure THashMultisetLPTTest.ObjectMultiSetSymmetricSubtract;
var
  ms, ms1: TAutoObjMultiSet;
  I, TestSize: Integer;
  Counter: TCounter;
begin
  TestSize := 65;
  Counter := Default(TCounter);

  for I := 1 to TestSize do
    ms.Instance.Add(TTestObj.Create(Counter.Increment));
  ms.Instance.AddAll(ms.Instance);
  ms.Instance.OwnsObjects := False;

  for I := 1 to TestSize do
    ms1.Instance.Add(TTestObj.Create(Counter.Increment));
  ms1.Instance.OwnsObjects := False;

  AssertTrue(Counter.Count = 0);
  AssertTrue(ms.Instance.Count = TestSize * 2);

  ms.Instance.SymmetricSubtract(ms1.Instance);
  AssertTrue(ms.Instance.Count = TestSize * 3);

  ms1.Instance.SymmetricSubtract(ms.Instance);
  AssertTrue(ms1.Instance.Count = TestSize * 2);

  ms.Instance.SymmetricSubtract(ms1.Instance);
  AssertTrue(ms.Instance.Count = TestSize);

  ms.Instance.ArithmeticSubtract(ms1.Instance);
  AssertTrue(ms.Instance.Count = TestSize);

  AssertTrue(Counter.Count = 0);

  ms.Instance.OwnsObjects := True;
  ms1.Instance.OwnsObjects := True;

  ms.Instance := Default(TObjMultiSet);
  ms1.Instance := Default(TObjMultiSet);

  AssertTrue(Counter.Count = TestSize * 2);
end;


function THashMultisetQPTest.IsEven(const aValue: Integer): Boolean;
begin
  Result := not Odd(aValue);
end;

function THashMultisetQPTest.ObjIsEven(const aObj: TTestObj): Boolean;
begin
  Result := not Odd(aObj.Value);
end;

procedure THashMultisetQPTest.TestCreate;
var
  ms: TAutoMultiSet;
begin
  AssertTrue(ms.Instance.Count = 0);
  AssertTrue(ms.Instance.LoadFactor = TMultiSet.DefaultLoadFactor);
end;

procedure THashMultisetQPTest.CreateArray11;
var
  ms: TAutoMultiSet;
  I: Integer;
begin
  ms.Instance := TMultiSet.Create(IntArray11);
  AssertTrue(ms.Instance.Count = 11);
  AssertTrue(ms.Instance.Capacity >= 11);
  AssertTrue(ms.Instance.LoadFactor = TMultiSet.DefaultLoadFactor);
  for I in IntArray11 do
    AssertTrue(ms.Instance.Contains(I));
end;

procedure THashMultisetQPTest.CreateEnum11;
var
  ms: TAutoMultiSet;
  I: Integer;
  e: IIntEnumerable;
begin
  e := TIntArrayCursor.Create(TIntHelper.CreateCopy(IntArray11));
  ms.Instance := TMultiSet.Create(e);
  AssertTrue(ms.Instance.Count = 11);
  AssertTrue(ms.Instance.Capacity >= 11);
  AssertTrue(ms.Instance.LoadFactor = TMultiSet.DefaultLoadFactor);
  for I in IntArray11 do
    AssertTrue(ms.Instance.Contains(I));
end;

procedure THashMultisetQPTest.CreateCapacity0;
var
  ms: TAutoMultiSet;
begin
  ms.Instance := TMultiSet.Create(0);
  AssertTrue(ms.Instance.Count = 0);
  AssertTrue(ms.Instance.Capacity > 0);
  AssertTrue(ms.Instance.LoadFactor = TMultiSet.DefaultLoadFactor);
end;

procedure THashMultisetQPTest.CreateCapacity5;
var
  ms: TAutoMultiSet;
begin
  ms.Instance := TMultiSet.Create(5);
  AssertTrue(ms.Instance.Count = 0);
  AssertTrue(ms.Instance.Capacity >= 5);
  AssertTrue(ms.Instance.LoadFactor = TMultiSet.DefaultLoadFactor);
end;

procedure THashMultisetQPTest.CreateCapacity50Array11;
var
  ms: TAutoMultiSet;
  I: Integer;
begin
  ms.Instance := TMultiSet.Create(50, IntArray11);
  AssertTrue(ms.Instance.Count = 11);
  AssertTrue(ms.Instance.Capacity >= 50);
  AssertTrue(ms.Instance.LoadFactor = TMultiSet.DefaultLoadFactor);
  for I in IntArray11 do
    AssertTrue(ms.Instance.Contains(I));
end;

procedure THashMultisetQPTest.CreateCapacity50Enum11;
var
  ms: TAutoMultiSet;
  I: Integer;
  e: IIntEnumerable;
begin
  e := TIntArrayCursor.Create(TIntHelper.CreateCopy(IntArray11));
  ms.Instance := TMultiSet.Create(50, e);
  AssertTrue(ms.Instance.Count = 11);
  AssertTrue(ms.Instance.Capacity >= 50);
  AssertTrue(ms.Instance.LoadFactor = TMultiSet.DefaultLoadFactor);
  for I in IntArray11 do
    AssertTrue(ms.Instance.Contains(I));
end;

procedure THashMultisetQPTest.CreateLoadFactor0;
var
  ms: TAutoMultiSet;
begin
  ms.Instance := TMultiSet.Create(0.0);
  AssertTrue(ms.Instance.Count = 0);
  AssertTrue(ms.Instance.LoadFactor = TMultiSet.MinLoadFactor);
end;

procedure THashMultisetQPTest.CreateLoadFactor3;
var
  ms: TAutoMultiSet;
begin
  ms.Instance := TMultiSet.Create(3.0);
  AssertTrue(ms.Instance.Count = 0);
  AssertTrue(ms.Instance.LoadFactor = TMultiSet.MaxLoadFactor);
end;

procedure THashMultisetQPTest.CreateLoadFactor;
var
  ms: TAutoMultiSet;
  lf: Double;
begin
  lf := 0.5;
  ms.Instance := TMultiSet.Create(lf);
  AssertTrue(ms.Instance.Count = 0);
  AssertTrue(ms.Instance.LoadFactor = lf);
end;

procedure THashMultisetQPTest.CreateLoadFactor075Array21;
var
  ms: TAutoMultiSet;
  I: Integer;
  lf: Single;
begin
  lf := 0.75;
  ms.Instance := TMultiSet.Create(lf, IntArray21);
  AssertTrue(ms.Instance.Count = 21);
  AssertTrue(ms.Instance.Capacity >= 21);
  AssertTrue(ms.Instance.LoadFactor = lf);
  for I in IntArray11 do
    AssertTrue(ms.Instance.Contains(I));
end;

procedure THashMultisetQPTest.CreateLoadFactor075Enum21;
var
  ms: TAutoMultiSet;
  I: Integer;
  e: IIntEnumerable;
  lf: Single;
begin
  lf := 0.75;
  e := TIntArrayCursor.Create(TIntHelper.CreateCopy(IntArray21));
  ms.Instance := TMultiSet.Create(lf, e);
  AssertTrue(ms.Instance.Count = 21);
  AssertTrue(ms.Instance.Capacity >= 21);
  AssertTrue(ms.Instance.LoadFactor = lf);
  for I in IntArray11 do
    AssertTrue(ms.Instance.Contains(I));
end;

procedure THashMultisetQPTest.CreateCapacityLoadFactor;
var
  ms: TAutoMultiSet;
  lf: Single;
begin
  lf := 0.45;
  ms.Instance := TMultiSet.Create(50, lf);
  AssertTrue(ms.Instance.Count = 0);
  AssertTrue(ms.Instance.Capacity >= 50);
  AssertTrue(ms.Instance.LoadFactor = lf);
end;

procedure THashMultisetQPTest.CreateCapacityLoadFactor075Array11;
var
  ms: TAutoMultiSet;
  I: Integer;
  lf: Single;
begin
  lf := 0.75;
  ms.Instance := TMultiSet.Create(50, lf, IntArray11);
  AssertTrue(ms.Instance.Count = 11);
  AssertTrue(ms.Instance.Capacity >= 50);
  AssertTrue(ms.Instance.LoadFactor = lf);
  for I in IntArray11 do
    AssertTrue(ms.Instance.Contains(I));
end;

procedure THashMultisetQPTest.CreateCapacityLoadFactor075Enum11;
var
  ms: TAutoMultiSet;
  I: Integer;
  e: IIntEnumerable;
  lf: Single;
begin
  lf := 0.75;
  e := TIntArrayCursor.Create(TIntHelper.CreateCopy(IntArray11));
  ms.Instance := TMultiSet.Create(50, lf, e);
  AssertTrue(ms.Instance.Count = 11);
  AssertTrue(ms.Instance.Capacity >= 50);
  AssertTrue(ms.Instance.LoadFactor = lf);
  for I in IntArray11 do
    AssertTrue(ms.Instance.Contains(I));
end;

procedure THashMultisetQPTest.Clear;
var
  ms: TAutoMultiSet;
begin
  ms.Instance := TMultiSet.Create(IntArray11);
  AssertTrue(ms.Instance.Count = 11);
  AssertTrue(ms.Instance.Capacity >= 11);
  ms.Instance.Clear;
  AssertTrue(ms.Instance.Count = 0);
  AssertTrue(ms.Instance.Capacity = 0);
end;

procedure THashMultisetQPTest.EnsureCapacity;
var
  ms: TAutoMultiSet;
  c: SizeInt;
begin
  c := ms.Instance.ExpandTreshold;
  ms.Instance.EnsureCapacity(c + 100);
  AssertTrue(ms.Instance.ExpandTreshold >= c + 100);
end;

procedure THashMultisetQPTest.TrimToFit;
var
  ms: TAutoMultiSet;
  c: SizeInt;
begin
  ms.Instance := TMultiSet.Create(0.8, IntArray11);
  ms.Instance.EnsureCapacity(40);
  c := ms.Instance.Capacity;
  ms.Instance.TrimToFit;
  AssertTrue(ms.Instance.Capacity < c);
end;

procedure THashMultisetQPTest.Add;
var
  ms: TAutoMultiSet;
begin
  AssertTrue(ms.Instance.Count = 0);
  AssertTrue(ms.Instance.Add(1));
  AssertTrue(ms.Instance.Count = 1);
  AssertTrue(ms.Instance.Contains(1));
  AssertTrue(ms.Instance.Add(51));
  AssertTrue(ms.Instance.Contains(51));
  AssertTrue(ms.Instance.Count = 2);
  AssertTrue(ms.Instance.EntryCount = 2);
  AssertTrue(ms.Instance.Add(1));
  AssertTrue(ms.Instance.Add(51));
  AssertTrue(ms.Instance.Count = 4);
  AssertTrue(ms.Instance.EntryCount = 2);
  AssertTrue(ms.Instance.Add(52));
  AssertTrue(ms.Instance.Contains(52));
  AssertTrue(ms.Instance.Count = 5);
  AssertTrue(ms.Instance.EntryCount = 3);
end;

procedure THashMultisetQPTest.Add100;
var
  ms: TAutoMultiSet;
  I: Integer;
begin
  for I := 1 to 100 do
    AssertTrue(ms.Instance.Add(I));
  AssertTrue(ms.Instance.Count = 100);
  for I := 1 to 100 do
    AssertTrue(ms.Instance.Add(I));
  AssertTrue(ms.Instance.Count = 200);
  AssertTrue(ms.Instance.EntryCount = 100);
  for I := 1 to 100 do
    AssertTrue(ms.Instance.Contains(I));
  ms.Instance[21] := 52;
  AssertTrue(ms.Instance.Count = 250);
  ms.Instance[21] := 12;
  AssertTrue(ms.Instance.Count = 210);
end;

procedure THashMultisetQPTest.AddArray;
var
  ms: TAutoMultiSet;
begin
  AssertTrue(ms.Instance.AddAll(IntArray21) = System.Length(IntArray21));
  AssertTrue(ms.Instance.Count = System.Length(IntArray21));
  AssertTrue(ms.Instance.EntryCount = System.Length(IntArray21));
  AssertTrue(ms.Instance.ContainsAll(IntArray21));
  AssertTrue(ms.Instance.AddAll(IntArray21) = System.Length(IntArray21));
  AssertTrue(ms.Instance.Count = System.Length(IntArray21) * 2);
  AssertTrue(ms.Instance.EntryCount = System.Length(IntArray21));
  AssertTrue(ms.Instance.ContainsAll(IntArray21));
end;

procedure THashMultisetQPTest.AddEnum;
var
  ms: TAutoMultiSet;
  e: IIntEnumerable;
begin
  e := TIntArrayCursor.Create(TIntHelper.CreateCopy(IntArray21));
  AssertTrue(ms.Instance.AddAll(e) = System.Length(IntArray21));
  AssertTrue(ms.Instance.Count = System.Length(IntArray21));
  AssertTrue(ms.Instance.EntryCount = System.Length(IntArray21));
  AssertTrue(ms.Instance.ContainsAll(IntArray21));
  e := TIntArrayCursor.Create(TIntHelper.CreateCopy(IntArray21));
  AssertTrue(ms.Instance.AddAll(e) = System.Length(IntArray21));
  AssertTrue(ms.Instance.Count = System.Length(IntArray21) * 2);
  AssertTrue(ms.Instance.EntryCount = System.Length(IntArray21));
  AssertTrue(ms.Instance.ContainsAll(IntArray21));
end;

procedure THashMultisetQPTest.AddSelf;
var
  ms: TAutoMultiSet;
begin
  ms.Instance.AddAll(IntArray21);
  AssertTrue(ms.Instance.AddAll(ms.Instance) = System.Length(IntArray21));
  AssertTrue(ms.Instance.Count = System.Length(IntArray21) * 2);
  AssertTrue(ms.Instance.EntryCount = System.Length(IntArray21));
end;

procedure THashMultisetQPTest.Remove;
var
  ms: TAutoMultiSet;
  c: SizeInt;
begin
  c := System.Length(IntArray21) * 2;
  ms.Instance := TMultiSet.Create(IntArray21);
  ms.Instance.AddAll(IntArray21);
  AssertTrue(ms.Instance.Count = c);
  AssertTrue(ms.Instance.EntryCount = System.Length(IntArray21));
  AssertTrue(ms.Instance.Remove(IntArray21[1]));
  AssertTrue(ms.Instance.Count = c - 1);
  AssertTrue(ms.Instance.EntryCount = System.Length(IntArray21));
  AssertTrue(ms.Instance.Remove(IntArray21[System.High(IntArray21)]));
  AssertTrue(ms.Instance.Count = c - 2);
  AssertTrue(ms.Instance.EntryCount = System.Length(IntArray21));
  AssertTrue(ms.Instance.Remove(IntArray21[1]));
  AssertTrue(ms.Instance.Count = c - 3);
  AssertTrue(ms.Instance.EntryCount = System.Length(IntArray21) - 1);
  AssertFalse(ms.Instance.Remove(-1));
  AssertTrue(ms.Instance.Count = c - 3);
  AssertFalse(ms.Instance.Remove(100));
  AssertTrue(ms.Instance.Count = c - 3);
end;

procedure THashMultisetQPTest.RemoveArray;
var
  ms: TAutoMultiSet;
begin
  ms.Instance := TMultiSet.Create(IntArray21);
  ms.Instance.AddAll(IntArray21);
  AssertTrue(ms.Instance.RemoveAll(IntArray11) = 11);
  AssertTrue(ms.Instance.Count = 31);
  AssertTrue(ms.Instance.EntryCount = 21);
  AssertTrue(ms.Instance.RemoveAll(IntArray10) = 10);
  AssertTrue(ms.Instance.Count = 21);
  AssertTrue(ms.Instance.EntryCount = 21);
end;

procedure THashMultisetQPTest.RemoveEnum;
var
  ms: TAutoMultiSet;
  e: IIntEnumerable;
begin
  ms.Instance := TMultiSet.Create(IntArray21);
  ms.Instance.AddAll(IntArray21);
  e := TIntArrayCursor.Create(TIntHelper.CreateCopy(IntArray11));
  AssertTrue(ms.Instance.RemoveAll(e) = 11);
  AssertTrue(ms.Instance.Count = 31);
  AssertTrue(ms.Instance.EntryCount = 21);
  e := TIntArrayCursor.Create(TIntHelper.CreateCopy(IntArray10));
  AssertTrue(ms.Instance.RemoveAll(e) = 10);
  AssertTrue(ms.Instance.Count = 21);
  AssertTrue(ms.Instance.EntryCount = 21);
end;

procedure THashMultisetQPTest.RemoveSelf;
var
  ms: TAutoMultiSet;
begin
  ms.Instance := TMultiSet.Create(IntArray21);
  ms.Instance.AddAll(IntArray21);
  AssertTrue(ms.Instance.RemoveAll(ms.Instance) = 42);
  AssertTrue(ms.Instance.IsEmpty);
end;

procedure THashMultisetQPTest.RemoveIfRegular;
var
  ms: TAutoMultiSet;
  I: Integer;
begin
  ms.Instance := TMultiSet.Create(IntArray21);
  ms.Instance.AddAll(IntArray21);
  AssertTrue(ms.Instance.RemoveIf(@IsEvenInt) = 20);
  AssertTrue(ms.Instance.Count = 22);
  AssertTrue(ms.Instance.ContainsAll(IntArray11));
  for I in IntArray11 do
    AssertTrue(ms.Instance[I] = 2);
end;

procedure THashMultisetQPTest.RemoveIfDelegated;
var
  ms: TAutoMultiSet;
  I: Integer;
begin
  ms.Instance := TMultiSet.Create(IntArray21);
  ms.Instance.AddAll(IntArray21);
  AssertTrue(ms.Instance.RemoveIf(@IsEven) = 20);
  AssertTrue(ms.Instance.Count = 22);
  AssertTrue(ms.Instance.ContainsAll(IntArray11));
  for I in IntArray11 do
    AssertTrue(ms.Instance[I] = 2);
end;

procedure THashMultisetQPTest.RemoveIfNested;
  function IsEvenInt(const aValue: Integer): Boolean;
  begin
    Result := not Odd(aValue);
  end;
var
  ms: TAutoMultiSet;
  I: Integer;
begin
  ms.Instance := TMultiSet.Create(IntArray21);
  ms.Instance.AddAll(IntArray21);
  AssertTrue(ms.Instance.RemoveIf(@IsEvenInt) = 20);
  AssertTrue(ms.Instance.Count = 22);
  AssertTrue(ms.Instance.ContainsAll(IntArray11));
  for I in IntArray11 do
    AssertTrue(ms.Instance[I] = 2);
end;

procedure THashMultisetQPTest.ExtractIfRegular;
var
  ms, ms1: TAutoMultiSet;
  e: TIntArray;
  I: Integer;
begin
  ms.Instance := TMultiSet.Create(IntArray21);
  ms.Instance.AddAll(IntArray21);
  e := ms.Instance.ExtractIf(@IsEvenInt);
  AssertTrue(ms.Instance.Count = 22);
  AssertTrue(ms.Instance.ContainsAll(IntArray11));
  AssertTrue(System.Length(e) = 20);
  ms1.Instance := TMultiSet.Create(e);
  for I in IntArray11 do
    AssertTrue(ms.Instance[I] = 2);
  for I in IntArray10 do
    AssertTrue(ms1.Instance[I] = 2);
end;

procedure THashMultisetQPTest.ExtractIfDelegated;
var
  ms, ms1: TAutoMultiSet;
  e: TIntArray;
  I: Integer;
begin
  ms.Instance := TMultiSet.Create(IntArray21);
  ms.Instance.AddAll(IntArray21);
  e := ms.Instance.ExtractIf(@IsEven);
  AssertTrue(ms.Instance.Count = 22);
  AssertTrue(ms.Instance.ContainsAll(IntArray11));
  AssertTrue(System.Length(e) = 20);
  ms1.Instance := TMultiSet.Create(e);
  for I in IntArray11 do
    AssertTrue(ms.Instance[I] = 2);
  for I in IntArray10 do
    AssertTrue(ms1.Instance[I] = 2);
end;

procedure THashMultisetQPTest.ExtractIfNested;
  function IsEvenInt(const aValue: Integer): Boolean;
  begin
    Result := not Odd(aValue);
  end;
var
  ms, ms1: TAutoMultiSet;
  e: TIntArray;
  I: Integer;
begin
  ms.Instance := TMultiSet.Create(IntArray21);
  ms.Instance.AddAll(IntArray21);
  e := ms.Instance.ExtractIf(@IsEvenInt);
  AssertTrue(ms.Instance.Count = 22);
  AssertTrue(ms.Instance.ContainsAll(IntArray11));
  AssertTrue(System.Length(e) = 20);
  ms1.Instance := TMultiSet.Create(e);
  for I in IntArray11 do
    AssertTrue(ms.Instance[I] = 2);
  for I in IntArray10 do
    AssertTrue(ms1.Instance[I] = 2);
end;

procedure THashMultisetQPTest.Retain;
var
  ms, ms1: TAutoMultiSet;
  I: Integer;
begin
  ms.Instance := TMultiSet.Create(IntArray21);
  ms.Instance.AddAll(IntArray21);
  ms1.Instance := TMultiSet.Create(IntArray11);
  AssertTrue(ms.Instance.Count = 42);
  ms.Instance.RetainAll(ms1.Instance);
  AssertTrue(ms.Instance.Count = 22);
  AssertTrue(ms.Instance.ContainsAll(ms1.Instance));
  for I in IntArray11 do
    AssertTrue(ms.Instance[I] = 2);
end;

procedure THashMultisetQPTest.Retain_1;
var
  ms, ms1: TAutoMultiSet;
begin
  ms.Instance := TMultiSet.Create(IntArray10);
  ms.Instance.AddAll(IntArray10);
  AssertTrue(ms.Instance.Count = 20);
  ms1.Instance := TMultiSet.Create(IntArray11);
  ms.Instance.RetainAll(ms1.Instance);
  AssertTrue(ms.Instance.IsEmpty);
end;

procedure THashMultisetQPTest.RetainSelf;
var
  ms: TAutoMultiSet;
begin
  ms.Instance := TMultiSet.Create(IntArray21);
  ms.Instance.AddAll(IntArray21);
  AssertTrue(ms.Instance.Count = 42);
  ms.Instance.RetainAll(ms.Instance);
  AssertTrue(ms.Instance.Count = 42);
  AssertTrue(ms.Instance.ContainsAll(IntArray21));
end;

procedure THashMultisetQPTest.IsSuperset;
var
  ms, ms1: TAutoMultiSet;
begin
  ms.Instance := TMultiSet.Create(IntArray21);
  ms.Instance.AddAll(IntArray21);
  ms1.Instance := TMultiSet.Create(ms.Instance);
  AssertTrue(ms.Instance.IsSuperSet(ms1.Instance));
  AssertTrue(ms1.Instance.IsSuperSet(ms.Instance));
  AssertTrue(ms.Instance.IsSuperSet(ms.Instance));
  ms1.Instance.Remove(IntArray21[1]);
  AssertTrue(ms.Instance.IsSuperSet(ms1.Instance));
  AssertFalse(ms1.Instance.IsSuperSet(ms.Instance));
  ms1.Instance.Clear;
  AssertTrue(ms.Instance.IsSuperSet(ms1.Instance));
  AssertFalse(ms1.Instance.IsSuperSet(ms.Instance));
  ms.Instance.Clear;
  AssertTrue(ms.Instance.IsSuperSet(ms1.Instance));
  AssertTrue(ms1.Instance.IsSuperSet(ms.Instance));
end;

procedure THashMultisetQPTest.IsSubset;
var
  ms, ms1: TAutoMultiSet;
begin
  ms.Instance := TMultiSet.Create(IntArray21);
  ms.Instance.AddAll(IntArray21);
  ms1.Instance := TMultiSet.Create(ms.Instance);
  AssertTrue(ms.Instance.IsSubSet(ms1.Instance));
  AssertTrue(ms1.Instance.IsSubSet(ms.Instance));
  AssertTrue(ms.Instance.IsSubSet(ms.Instance));
  ms1.Instance.Remove(IntArray21[1]);
  AssertFalse(ms.Instance.IsSubSet(ms1.Instance));
  AssertTrue(ms1.Instance.IsSubSet(ms.Instance));
  ms1.Instance.Clear;
  AssertFalse(ms.Instance.IsSubSet(ms1.Instance));
  AssertTrue(ms1.Instance.IsSubSet(ms.Instance));
  ms.Instance.Clear;
  AssertTrue(ms1.Instance.IsSubSet(ms.Instance));
  AssertTrue(ms.Instance.IsSubSet(ms1.Instance));
end;

procedure THashMultisetQPTest.IsEqual;
var
  ms, ms1: TAutoMultiSet;
begin
  ms.Instance := TMultiSet.Create(IntArray21);
  ms.Instance.AddAll(IntArray21);
  ms1.Instance := TMultiSet.Create(ms.Instance);
  AssertTrue(ms.Instance.IsEqual(ms1.Instance));
  AssertTrue(ms1.Instance.IsEqual(ms.Instance));
  AssertTrue(ms.Instance.IsEqual(ms.Instance));
  ms1.Instance.Remove(IntArray21[1]);
  AssertFalse(ms.Instance.IsEqual(ms1.Instance));
  AssertFalse(ms1.Instance.IsEqual(ms.Instance));
  ms1.Instance.Clear;
  ms.Instance.Clear;
  AssertTrue(ms.Instance.IsEqual(ms1.Instance));
  AssertTrue(ms1.Instance.IsEqual(ms.Instance));
end;

procedure THashMultisetQPTest.Intersecting;
var
  ms, ms1: TAutoMultiSet;
begin
  ms.Instance := TMultiSet.Create(IntArray21);
  ms1.Instance := TMultiSet.Create(IntArray11);
  AssertTrue(ms.Instance.Intersecting(ms.Instance));
  AssertTrue(ms.Instance.Intersecting(ms1.Instance));
  ms.Instance := TMultiSet.Create(IntArray10);
  AssertFalse(ms.Instance.Intersecting(ms1.Instance));
  AssertFalse(ms1.Instance.Intersecting(ms.Instance));
end;

procedure THashMultisetQPTest.Intersect;
var
  ms, ms1: TAutoMultiSet;
  I: Integer;
begin
  ms.Instance := TMultiSet.Create(IntArray21);
  ms.Instance.AddAll(IntArray21);
  AssertTrue(ms.Instance.Count = 42);
  ms1.Instance := TMultiSet.Create(IntArray11);
  ms1.Instance.AddAll([-4, -3, -2, -1, 0]);
  ms.Instance.Intersect(ms1.Instance);
  AssertTrue(ms.Instance.Count = 11);
  for I in IntArray11 do
    AssertTrue(ms.Instance[I] = 1);
end;

procedure THashMultisetQPTest.Join;
var
  ms, ms1: TAutoMultiSet;
  I: Integer;
begin
  ms1.Instance := TMultiSet.Create(IntArray11);
  ms.Instance.Join(ms1.Instance);
  AssertTrue(ms.Instance.IsEqual(ms1.Instance));
  ms1.Instance := TMultiSet.Create(IntArray10);
  ms1.Instance.AddAll(ms1.Instance);
  ms.Instance.Join(ms1.Instance);
  for I in IntArray11 do
    AssertTrue(ms.Instance[I] = 1);
  for I in IntArray10 do
    AssertTrue(ms.Instance[I] = 2);
end;

procedure THashMultisetQPTest.ArithmeticAdd;
var
  ms, ms1: TAutoMultiSet;
  I: Integer;
begin
  ms1.Instance := TMultiSet.Create(IntArray11);
  ms.Instance.ArithmeticAdd(ms1.Instance);
  AssertTrue(ms.Instance.IsEqual(ms1.Instance));
  ms.Instance.AddAll(IntArray10);
  ms.Instance.ArithmeticAdd(ms1.Instance);
  for I in IntArray11 do
    AssertTrue(ms.Instance[I] = 2);
  for I in IntArray10 do
    AssertTrue(ms.Instance[I] = 1);
  ms.Instance.ArithmeticAdd(ms.Instance);
  for I in IntArray11 do
    AssertTrue(ms.Instance[I] = 4);
  for I in IntArray10 do
    AssertTrue(ms.Instance[I] = 2);
end;

procedure THashMultisetQPTest.ArithmeticSubtract;
var
  ms, ms1: TAutoMultiSet;
begin
  ms1.Instance := TMultiSet.Create(IntArray11);
  ms.Instance.ArithmeticSubtract(ms1.Instance);
  AssertTrue(ms.Instance.IsEmpty);
  ms.Instance.AddAll(IntArray10);
  ms.Instance.ArithmeticSubtract(ms1.Instance);
  AssertTrue(ms.Instance.Count = 10);
  AssertTrue(ms.Instance.ContainsAll(IntArray10));
  ms.Instance.AddAll(IntArray11);
  AssertTrue(ms.Instance.ContainsAll(IntArray21));
  ms.Instance.ArithmeticSubtract(ms1.Instance);
  AssertTrue(ms.Instance.Count = 10);
  AssertTrue(ms.Instance.ContainsAll(IntArray10));
  ms1.Instance.AddAll(IntArray10);
  ms.Instance.ArithmeticSubtract(ms1.Instance);
  AssertTrue(ms.Instance.IsEmpty);
end;

procedure THashMultisetQPTest.SymmetricSubtract;
var
  ms, ms1: TAutoMultiSet;
begin
  ms1.Instance := TMultiSet.Create(IntArray11);
  ms.Instance.SymmetricSubtract(ms1.Instance);
  AssertTrue(ms.Instance.Count = 11);
  AssertTrue(ms.Instance.ContainsAll(IntArray11));
  ms.Instance.AddAll(IntArray10);
  ms.Instance.SymmetricSubtract(ms1.Instance);
  AssertTrue(ms.Instance.Count = 10);
  AssertTrue(ms.Instance.ContainsAll(IntArray10));
  ms.Instance.AddAll(IntArray11);
  ms.Instance.AddAll(IntArray21);
  AssertTrue(ms.Instance.Count = 42);
  ms1.Instance.AddAll(IntArray10);
  ms.Instance.SymmetricSubtract(ms1.Instance);
  AssertTrue(ms.Instance.Count = 21);
  AssertTrue(ms.Instance.ContainsAll(IntArray21));
  ms.Instance.SymmetricSubtract(ms1.Instance);
  AssertTrue(ms.Instance.IsEmpty);
end;

procedure THashMultisetQPTest.ObjectMultiSetRemove;
var
  ms: TAutoObjMultiSet;
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
      ms.Instance.Add(a[I]);
    end;
  AssertTrue(ms.Instance.Count = TestSize);
  ms.Instance.AddAll(a);
  AssertTrue(ms.Instance.Count = TestSize * 2);
  for I := 0 to System.High(a) do
    AssertTrue(ms.Instance.Remove(a[I]));
  AssertTrue(ms.Instance.Count = TestSize);
  AssertTrue(Counter.Count = 0);
  for I := 0 to System.High(a) do
    begin
      AssertTrue(ms.Instance.Remove(a[I]));
      AssertTrue(ms.Instance.Count = Pred(TestSize - I));
      AssertTrue(Counter.Count = Succ(I));
    end;
  AssertTrue(ms.Instance.IsEmpty);
  AssertTrue(Counter.Count = TestSize);
end;

procedure THashMultisetQPTest.ObjectMultiSetRemoveArray;
var
  ms: TAutoObjMultiSet;
  a: TObjArray;
  I, TestSize: Integer;
  Counter: TCounter;
begin
  TestSize := 56;
  Counter := Default(TCounter);
  for I := 1 to TestSize do
    ms.Instance.Add(TTestObj.Create(Counter.Increment));
  AssertTrue(ms.Instance.Count = TestSize);
  a := ms.Instance.ToArray;
  AssertTrue(System.Length(a) = TestSize);
  ms.Instance.AddAll(a);
  AssertTrue(ms.Instance.Count = TestSize * 2);
  AssertTrue(ms.Instance.RemoveAll(a) = TestSize);
  AssertTrue(Counter.Count = 0);
  AssertTrue(ms.Instance.RemoveAll(a) = TestSize);
  AssertTrue(ms.Instance.IsEmpty);
  AssertTrue(Counter.Count = TestSize);
end;

procedure THashMultisetQPTest.ObjectMultiSetRemoveEnum;
type
  TFakeEnum = specialize TGArrayCursor<TTestObj>;
var
  ms: TAutoObjMultiSet;
  a: TObjArray;
  e: TFakeEnum;
  I, TestSize: Integer;
  Counter: TCounter;
begin
  TestSize := 52;
  Counter := Default(TCounter);
  for I := 1 to TestSize do
    ms.Instance.Add(TTestObj.Create(Counter.Increment));
  AssertTrue(ms.Instance.Count = TestSize);
  a := ms.Instance.ToArray;
  AssertTrue(System.Length(a) = TestSize);
  ms.Instance.AddAll(a);
  AssertTrue(ms.Instance.Count = TestSize * 2);
  e := TFakeEnum.Create(a);
  AssertTrue(ms.Instance.RemoveAll(e) = TestSize);
  AssertTrue(Counter.Count = 0);
  e := TFakeEnum.Create(a);
  AssertTrue(ms.Instance.RemoveAll(e) = TestSize);
  AssertTrue(ms.Instance.IsEmpty);
  AssertTrue(Counter.Count = TestSize);
end;

procedure THashMultisetQPTest.ObjectMultiSetRemoveSelf;
var
  ms: TAutoObjMultiSet;
  I, TestSize: Integer;
  Counter: TCounter;
begin
  TestSize := 100;
  Counter := Default(TCounter);
  for I := 1 to TestSize do
    ms.Instance.Add(TTestObj.Create(Counter.Increment));
  ms.Instance.AddAll(ms.Instance);
  AssertTrue(ms.Instance.Count = TestSize * 2);
  AssertTrue(ms.Instance.RemoveAll(ms.Instance) = TestSize * 2);
  AssertTrue(ms.Instance.IsEmpty);
  AssertTrue(Counter.Count = TestSize);
end;

procedure THashMultisetQPTest.ObjectMultiSetRemoveIfRegular;
var
  ms: TAutoObjMultiSet;
  I, TestSize: Integer;
  Counter: TCounter;
  o: TTestObj;
begin
  TestSize := 100;
  Counter := Default(TCounter);
  for I := 1 to TestSize do
    ms.Instance.Add(TTestObj.Create(I, Counter.Increment));
  ms.Instance.AddAll(ms.Instance);
  AssertTrue(ms.Instance.Count = TestSize * 2);
  AssertTrue(ms.Instance.RemoveIf(@IsEvenObj) = TestSize);
  AssertTrue(Counter.Count = TestSize div 2);
  AssertTrue(ms.Instance.Count = TestSize);
  for o in ms.Instance do
    AssertTrue(Odd(o.Value));
  ms.Instance := Default(TObjMultiSet);
  AssertTrue(Counter.Count = TestSize);
end;

procedure THashMultisetQPTest.ObjectMultiSetRemoveIfDelegated;
var
  ms: TAutoObjMultiSet;
  I, TestSize: Integer;
  Counter: TCounter;
  o: TTestObj;
begin
  TestSize := 100;
  Counter := Default(TCounter);
  for I := 1 to TestSize do
    ms.Instance.Add(TTestObj.Create(I, Counter.Increment));
  ms.Instance.AddAll(ms.Instance);
  AssertTrue(ms.Instance.Count = TestSize * 2);
  AssertTrue(ms.Instance.RemoveIf(@ObjIsEven) = TestSize);
  AssertTrue(Counter.Count = TestSize div 2);
  AssertTrue(ms.Instance.Count = TestSize);
  for o in ms.Instance do
    AssertTrue(Odd(o.Value));
  ms.Instance := Default(TObjMultiSet);
  AssertTrue(Counter.Count = TestSize);
end;

procedure THashMultisetQPTest.ObjectMultiSetRemoveIfNested;
  function IsEvenNest(const aObj: TTestObj): Boolean;
  begin
    Result := not Odd(aObj.Value);
  end;
var
  ms: TAutoObjMultiSet;
  I, TestSize: Integer;
  Counter: TCounter;
  o: TTestObj;
begin
  TestSize := 100;
  Counter := Default(TCounter);
  for I := 1 to TestSize do
    ms.Instance.Add(TTestObj.Create(I, Counter.Increment));
  ms.Instance.AddAll(ms.Instance);
  AssertTrue(ms.Instance.Count = TestSize * 2);
  AssertTrue(ms.Instance.RemoveIf(@IsEvenNest) = TestSize);
  AssertTrue(Counter.Count = TestSize div 2);
  AssertTrue(ms.Instance.Count = TestSize);
  for o in ms.Instance do
    AssertTrue(Odd(o.Value));
  ms.Instance := Default(TObjMultiSet);
  AssertTrue(Counter.Count = TestSize);
end;

procedure THashMultisetQPTest.ObjectMultiSetExtractIfRegular;
var
  ms: TAutoObjMultiSet;
  I, TestSize: Integer;
  Counter: TCounter;
  a: TObjArray;
  o: TTestObj;
begin
  TestSize := 100;
  Counter := Default(TCounter);
  for I := 1 to TestSize do
    ms.Instance.Add(TTestObj.Create(I, Counter.Increment));
  ms.Instance.AddAll(ms.Instance);
  AssertTrue(ms.Instance.Count = TestSize * 2);
  a := ms.Instance.ExtractIf(@IsEvenObj);
  AssertTrue(System.Length(a) = TestSize);
  AssertTrue(Counter.Count = 0);
  AssertTrue(ms.Instance.Count = TestSize);
  for o in ms.Instance do
    AssertTrue(Odd(o.Value));
  for o in a do
    AssertTrue(not Odd(o.Value));
  ms.Instance.Clear;
  AssertTrue(Counter.Count = TestSize div 2);
  ms.Instance.AddAll(a);
  ms.Instance.Clear;
  AssertTrue(Counter.Count = TestSize);
end;

procedure THashMultisetQPTest.ObjectMultiSetExtractIfDelegated;
var
  ms: TAutoObjMultiSet;
  I, TestSize: Integer;
  Counter: TCounter;
  a: TObjArray;
  o: TTestObj;
begin
  TestSize := 100;
  Counter := Default(TCounter);
  for I := 1 to TestSize do
    ms.Instance.Add(TTestObj.Create(I, Counter.Increment));
  ms.Instance.AddAll(ms.Instance);
  AssertTrue(ms.Instance.Count = TestSize * 2);
  a := ms.Instance.ExtractIf(@ObjIsEven);
  AssertTrue(System.Length(a) = TestSize);
  AssertTrue(Counter.Count = 0);
  AssertTrue(ms.Instance.Count = TestSize);
  for o in ms.Instance do
    AssertTrue(Odd(o.Value));
  for o in a do
    AssertTrue(not Odd(o.Value));
  ms.Instance.Clear;
  AssertTrue(Counter.Count = TestSize div 2);
  ms.Instance.AddAll(a);
  ms.Instance.Clear;
  AssertTrue(Counter.Count = TestSize);
end;

procedure THashMultisetQPTest.ObjectMultiSetExtractIfNested;
  function IsEvenNest(const aObj: TTestObj): Boolean;
  begin
    Result := not Odd(aObj.Value);
  end;
var
  ms: TAutoObjMultiSet;
  I, TestSize: Integer;
  Counter: TCounter;
  a: TObjArray;
  o: TTestObj;
begin
  TestSize := 100;
  Counter := Default(TCounter);
  for I := 1 to TestSize do
    ms.Instance.Add(TTestObj.Create(I, Counter.Increment));
  ms.Instance.AddAll(ms.Instance);
  AssertTrue(ms.Instance.Count = TestSize * 2);
  a := ms.Instance.ExtractIf(@IsEvenNest);
  AssertTrue(System.Length(a) = TestSize);
  AssertTrue(Counter.Count = 0);
  AssertTrue(ms.Instance.Count = TestSize);
  for o in ms.Instance do
    AssertTrue(Odd(o.Value));
  for o in a do
    AssertTrue(not Odd(o.Value));
  ms.Instance.Clear;
  AssertTrue(Counter.Count = TestSize div 2);
  ms.Instance.AddAll(a);
  ms.Instance.Clear;
  AssertTrue(Counter.Count = TestSize);
end;

procedure THashMultisetQPTest.ObjectMultiSetRetain;
type
  TObjSet     = specialize TGObjHashSetLP<TTestObj>;
  TAutoObjSet = specialize TGAutoRef<TObjSet>;
var
  ms: TAutoObjMultiSet;
  s1: TAutoObjSet;
  I, TestSize: Integer;
  Counter: TCounter;
  a: TObjArray;
begin
  TestSize := 100;
  Counter := Default(TCounter);
  System.SetLength(a, TestSize);
  s1.Instance := TObjSet.Create(False);

  for I := 0 to System.High(a) do
    a[I] := TTestObj.Create(Counter.Increment);
  s1.Instance.AddAll(a);
  ms.Instance.AddAll(a);
  ms.Instance.AddAll(a);
  for I := 1 to TestSize do
    ms.Instance.Add(TTestObj.Create(Counter.Increment));
  AssertTrue(ms.Instance.Count = TestSize * 3);
  ms.Instance.RetainAll(s1.Instance);
  AssertTrue(ms.Instance.Count = TestSize * 2);
  AssertTrue(Counter.Count = TestSize);
  AssertTrue(ms.Instance.ContainsAll(s1.Instance));
  ms.Instance := Default(TObjMultiSet);
  AssertTrue(Counter.Count = TestSize * 2);
end;

procedure THashMultisetQPTest.ObjectMultiSetIntersect;
var
  ms, ms1: TAutoObjMultiSet;
  I, TestSize: Integer;
  Counter: TCounter;
  a: TObjArray;
  o: TTestObj;
begin
  TestSize := 50;
  Counter := Default(TCounter);
  System.SetLength(a, TestSize);
  for I := 0 to System.High(a) do
    a[I] := TTestObj.Create(Counter.Increment);

  ms.Instance.AddAll(a);
  ms.Instance.AddAll(a);

  for o in a do
    AssertTrue(ms.Instance[o] = 2);

  for I := 1 to TestSize do
    ms.Instance.Add(TTestObj.Create(Counter.Increment));

  AssertTrue(Counter.Count = 0);
  AssertTrue(ms.Instance.Count = TestSize * 3);

  ms1.Instance.OwnsObjects := False;
  AssertTrue(ms1.Instance.AddAll(a) = TestSize);

  ms.Instance.Intersect(ms1.Instance);

  AssertTrue(ms.Instance.Count = TestSize);
  AssertTrue(Counter.Count = TestSize);

  for o in a do
    AssertTrue(ms.Instance[o] = 1);
end;

procedure THashMultisetQPTest.ObjectMultiSetArithmeticSubtract;
var
  ms, ms1: TAutoObjMultiSet;
  I, TestSize: Integer;
  Counter: TCounter;
  a: TObjArray;
  o: TTestObj;
begin
  TestSize := 55;
  Counter := Default(TCounter);
  System.SetLength(a, TestSize);
  for I := 0 to System.High(a) do
    a[I] := TTestObj.Create(Counter.Increment);

  ms.Instance.AddAll(a);
  ms.Instance.AddAll(a);
  ms.Instance.AddAll(a);

  ms1.Instance.OwnsObjects := False;

  ms1.Instance.AddAll(a);

  for I := 1 to TestSize do
    ms.Instance.Add(TTestObj.Create(Counter.Increment));

  AssertTrue(Counter.Count = 0);
  AssertTrue(ms.Instance.Count = TestSize * 4);
  for o in a do
    AssertTrue(ms.Instance[o] = 3);

  ms.Instance.ArithmeticSubtract(ms1.Instance);
  AssertTrue(ms.Instance.Count = TestSize * 3);
  for o in a do
    AssertTrue(ms.Instance[o] = 2);

  ms.Instance.ArithmeticSubtract(ms1.Instance);
  AssertTrue(ms.Instance.Count = TestSize * 2);
  for o in a do
    AssertTrue(ms.Instance[o] = 1);

  ms.Instance.ArithmeticSubtract(ms1.Instance);
  AssertTrue(ms.Instance.Count = TestSize);

  AssertTrue(Counter.Count = TestSize);
end;

procedure THashMultisetQPTest.ObjectMultiSetSymmetricSubtract;
var
  ms, ms1: TAutoObjMultiSet;
  I, TestSize: Integer;
  Counter: TCounter;
begin
  TestSize := 65;
  Counter := Default(TCounter);

  for I := 1 to TestSize do
    ms.Instance.Add(TTestObj.Create(Counter.Increment));
  ms.Instance.AddAll(ms.Instance);
  ms.Instance.OwnsObjects := False;

  for I := 1 to TestSize do
    ms1.Instance.Add(TTestObj.Create(Counter.Increment));
  ms1.Instance.OwnsObjects := False;

  AssertTrue(Counter.Count = 0);
  AssertTrue(ms.Instance.Count = TestSize * 2);

  ms.Instance.SymmetricSubtract(ms1.Instance);
  AssertTrue(ms.Instance.Count = TestSize * 3);

  ms1.Instance.SymmetricSubtract(ms.Instance);
  AssertTrue(ms1.Instance.Count = TestSize * 2);

  ms.Instance.SymmetricSubtract(ms1.Instance);
  AssertTrue(ms.Instance.Count = TestSize);

  ms.Instance.ArithmeticSubtract(ms1.Instance);
  AssertTrue(ms.Instance.Count = TestSize);

  AssertTrue(Counter.Count = 0);

  ms.Instance.OwnsObjects := True;
  ms1.Instance.OwnsObjects := True;

  ms.Instance := Default(TObjMultiSet);
  ms1.Instance := Default(TObjMultiSet);

  AssertTrue(Counter.Count = TestSize * 2);
end;


function TChainHashMultisetTest.IsEven(const aValue: Integer): Boolean;
begin
  Result := not Odd(aValue);
end;

function TChainHashMultisetTest.ObjIsEven(const aObj: TTestObj): Boolean;
begin
  Result := not Odd(aObj.Value);
end;

procedure TChainHashMultisetTest.TestCreate;
var
  ms: TAutoMultiSet;
begin
  AssertTrue(ms.Instance.Count = 0);
  AssertTrue(ms.Instance.Capacity > 0);
  AssertTrue(ms.Instance.LoadFactor = TMultiSet.DefaultLoadFactor);
end;

procedure TChainHashMultisetTest.CreateArray11;
var
  ms: TAutoMultiSet;
  I: Integer;
begin
  ms.Instance := TMultiSet.Create(IntArray11);
  AssertTrue(ms.Instance.Count = 11);
  AssertTrue(ms.Instance.Capacity >= 11);
  AssertTrue(ms.Instance.LoadFactor = TMultiSet.DefaultLoadFactor);
  for I in IntArray11 do
    AssertTrue(ms.Instance.Contains(I));
end;

procedure TChainHashMultisetTest.CreateEnum11;
var
  ms: TAutoMultiSet;
  I: Integer;
  e: IIntEnumerable;
begin
  e := TIntArrayCursor.Create(TIntHelper.CreateCopy(IntArray11));
  ms.Instance := TMultiSet.Create(e);
  AssertTrue(ms.Instance.Count = 11);
  AssertTrue(ms.Instance.Capacity >= 11);
  AssertTrue(ms.Instance.LoadFactor = TMultiSet.DefaultLoadFactor);
  for I in IntArray11 do
    AssertTrue(ms.Instance.Contains(I));
end;

procedure TChainHashMultisetTest.CreateCapacity0;
var
  ms: TAutoMultiSet;
begin
  ms.Instance := TMultiSet.Create(0);
  AssertTrue(ms.Instance.Count = 0);
  AssertTrue(ms.Instance.Capacity > 0);
  AssertTrue(ms.Instance.LoadFactor = TMultiSet.DefaultLoadFactor);
end;

procedure TChainHashMultisetTest.CreateCapacity5;
var
  ms: TAutoMultiSet;
begin
  ms.Instance := TMultiSet.Create(5);
  AssertTrue(ms.Instance.Count = 0);
  AssertTrue(ms.Instance.Capacity >= 5);
  AssertTrue(ms.Instance.LoadFactor = TMultiSet.DefaultLoadFactor);
end;

procedure TChainHashMultisetTest.CreateCapacity50Array11;
var
  ms: TAutoMultiSet;
  I: Integer;
begin
  ms.Instance := TMultiSet.Create(50, IntArray11);
  AssertTrue(ms.Instance.Count = 11);
  AssertTrue(ms.Instance.Capacity >= 50);
  AssertTrue(ms.Instance.LoadFactor = TMultiSet.DefaultLoadFactor);
  for I in IntArray11 do
    AssertTrue(ms.Instance.Contains(I));
end;

procedure TChainHashMultisetTest.CreateCapacity50Enum11;
var
  ms: TAutoMultiSet;
  I: Integer;
  e: IIntEnumerable;
begin
  e := TIntArrayCursor.Create(TIntHelper.CreateCopy(IntArray11));
  ms.Instance := TMultiSet.Create(50, e);
  AssertTrue(ms.Instance.Count = 11);
  AssertTrue(ms.Instance.Capacity >= 50);
  AssertTrue(ms.Instance.LoadFactor = TMultiSet.DefaultLoadFactor);
  for I in IntArray11 do
    AssertTrue(ms.Instance.Contains(I));
end;

procedure TChainHashMultisetTest.CreateLoadFactor0;
var
  ms: TAutoMultiSet;
begin
  ms.Instance := TMultiSet.Create(0.0);
  AssertTrue(ms.Instance.Count = 0);
  AssertTrue(ms.Instance.LoadFactor = TMultiSet.MinLoadFactor);
end;

procedure TChainHashMultisetTest.CreateLoadFactor3;
var
  ms: TAutoMultiSet;
  lf: Single;
begin
  lf := 3.0;
  ms.Instance := TMultiSet.Create(lf);
  AssertTrue(ms.Instance.Count = 0);
  AssertTrue(ms.Instance.LoadFactor = lf);
end;

procedure TChainHashMultisetTest.CreateLoadFactor;
var
  ms: TAutoMultiSet;
  lf: Double;
begin
  lf := 0.5;
  ms.Instance := TMultiSet.Create(lf);
  AssertTrue(ms.Instance.Count = 0);
  AssertTrue(ms.Instance.LoadFactor = lf);
end;

procedure TChainHashMultisetTest.CreateLoadFactor3Array11;
var
  ms: TAutoMultiSet;
  I: Integer;
  lf: Single;
begin
  lf := 3.0;
  ms.Instance := TMultiSet.Create(lf, IntArray11);
  AssertTrue(ms.Instance.Count = 11);
  AssertTrue(ms.Instance.Capacity >= 11);
  AssertTrue(ms.Instance.LoadFactor = lf);
  for I in IntArray11 do
    AssertTrue(ms.Instance.Contains(I));
end;

procedure TChainHashMultisetTest.CreateLoadFactor3Enum11;
var
  ms: TAutoMultiSet;
  I: Integer;
  e: IIntEnumerable;
  lf: Single;
begin
  lf := 3.0;
  e := TIntArrayCursor.Create(TIntHelper.CreateCopy(IntArray11));
  ms.Instance := TMultiSet.Create(lf, e);
  AssertTrue(ms.Instance.Count = 11);
  AssertTrue(ms.Instance.Capacity >= 11);
  AssertTrue(ms.Instance.LoadFactor = lf);
  for I in IntArray11 do
    AssertTrue(ms.Instance.Contains(I));
end;

procedure TChainHashMultisetTest.CreateCapacityLoadFactor;
var
  ms: TAutoMultiSet;
  lf: Single;
begin
  lf := 0.45;
  ms.Instance := TMultiSet.Create(50, lf);
  AssertTrue(ms.Instance.Count = 0);
  AssertTrue(ms.Instance.Capacity >= 50);
  AssertTrue(ms.Instance.LoadFactor = lf);
end;

procedure TChainHashMultisetTest.CreateCapacityLoadFactor3Array11;
var
  ms: TAutoMultiSet;
  I: Integer;
  lf: Single;
begin
  lf := 3.0;
  ms.Instance := TMultiSet.Create(50, lf, IntArray11);
  AssertTrue(ms.Instance.Count = 11);
  AssertTrue(ms.Instance.Capacity = 32);
  AssertTrue(ms.Instance.LoadFactor = lf);
  for I in IntArray11 do
    AssertTrue(ms.Instance.Contains(I));
end;

procedure TChainHashMultisetTest.CreateCapacityLoadFactor3Enum11;
var
  ms: TAutoMultiSet;
  I: Integer;
  e: IIntEnumerable;
  lf: Single;
begin
  lf := 3.0;
  e := TIntArrayCursor.Create(TIntHelper.CreateCopy(IntArray11));
  ms.Instance := TMultiSet.Create(50, lf, e);
  AssertTrue(ms.Instance.Count = 11);
  AssertTrue(ms.Instance.Capacity = 32);
  AssertTrue(ms.Instance.LoadFactor = lf);
  for I in IntArray11 do
    AssertTrue(ms.Instance.Contains(I));
end;

procedure TChainHashMultisetTest.Clear;
var
  ms: TAutoMultiSet;
begin
  ms.Instance := TMultiSet.Create(IntArray11);
  AssertTrue(ms.Instance.Count = 11);
  AssertTrue(ms.Instance.Capacity >= 11);
  ms.Instance.Clear;
  AssertTrue(ms.Instance.Count = 0);
  AssertTrue(ms.Instance.Capacity = 0);
end;

procedure TChainHashMultisetTest.EnsureCapacity;
var
  ms: TAutoMultiSet;
  c: SizeInt;
begin
  c := ms.Instance.ExpandTreshold;
  ms.Instance.EnsureCapacity(c + 100);
  AssertTrue(ms.Instance.ExpandTreshold >= c + 100);
end;

procedure TChainHashMultisetTest.TrimToFit;
var
  ms: TAutoMultiSet;
  c: SizeInt;
begin
  ms.Instance := TMultiSet.Create(0.8, IntArray11);
  ms.Instance.EnsureCapacity(40);
  c := ms.Instance.Capacity;
  ms.Instance.TrimToFit;
  AssertTrue(ms.Instance.Capacity < c);
end;

procedure TChainHashMultisetTest.Add;
var
  ms: TAutoMultiSet;
begin
  AssertTrue(ms.Instance.Count = 0);
  AssertTrue(ms.Instance.Add(1));
  AssertTrue(ms.Instance.Count = 1);
  AssertTrue(ms.Instance.Contains(1));
  AssertTrue(ms.Instance.Add(51));
  AssertTrue(ms.Instance.Contains(51));
  AssertTrue(ms.Instance.Count = 2);
  AssertTrue(ms.Instance.EntryCount = 2);
  AssertTrue(ms.Instance.Add(1));
  AssertTrue(ms.Instance.Add(51));
  AssertTrue(ms.Instance.Count = 4);
  AssertTrue(ms.Instance.EntryCount = 2);
  AssertTrue(ms.Instance.Add(52));
  AssertTrue(ms.Instance.Contains(52));
  AssertTrue(ms.Instance.Count = 5);
  AssertTrue(ms.Instance.EntryCount = 3);
  ms.Instance[101] := 10;
  AssertTrue(ms.Instance.Count = 15);
  AssertTrue(ms.Instance.EntryCount = 4);
end;

procedure TChainHashMultisetTest.Add100;
var
  ms: TAutoMultiSet;
  I: Integer;
begin
  for I := 1 to 100 do
    AssertTrue(ms.Instance.Add(I));
  AssertTrue(ms.Instance.Count = 100);
  for I := 1 to 100 do
    AssertTrue(ms.Instance.Add(I));
  AssertTrue(ms.Instance.Count = 200);
  AssertTrue(ms.Instance.EntryCount = 100);
  for I := 1 to 100 do
    AssertTrue(ms.Instance.Contains(I));
  ms.Instance[40] := 52;
  AssertTrue(ms.Instance.Count = 250);
  ms.Instance[40] := 12;
  AssertTrue(ms.Instance.Count = 210);
end;

procedure TChainHashMultisetTest.AddArray;
var
  ms: TAutoMultiSet;
begin
  AssertTrue(ms.Instance.AddAll(IntArray21) = System.Length(IntArray21));
  AssertTrue(ms.Instance.Count = System.Length(IntArray21));
  AssertTrue(ms.Instance.EntryCount = System.Length(IntArray21));
  AssertTrue(ms.Instance.ContainsAll(IntArray21));
  AssertTrue(ms.Instance.AddAll(IntArray21) = System.Length(IntArray21));
  AssertTrue(ms.Instance.Count = System.Length(IntArray21) * 2);
  AssertTrue(ms.Instance.EntryCount = System.Length(IntArray21));
  AssertTrue(ms.Instance.ContainsAll(IntArray21));
end;

procedure TChainHashMultisetTest.AddEnum;
var
  ms: TAutoMultiSet;
  e: IIntEnumerable;
begin
  e := TIntArrayCursor.Create(TIntHelper.CreateCopy(IntArray21));
  AssertTrue(ms.Instance.AddAll(e) = System.Length(IntArray21));
  AssertTrue(ms.Instance.Count = System.Length(IntArray21));
  AssertTrue(ms.Instance.EntryCount = System.Length(IntArray21));
  AssertTrue(ms.Instance.ContainsAll(IntArray21));
  e := TIntArrayCursor.Create(TIntHelper.CreateCopy(IntArray21));
  AssertTrue(ms.Instance.AddAll(e) = System.Length(IntArray21));
  AssertTrue(ms.Instance.Count = System.Length(IntArray21) * 2);
  AssertTrue(ms.Instance.EntryCount = System.Length(IntArray21));
  AssertTrue(ms.Instance.ContainsAll(IntArray21));
end;

procedure TChainHashMultisetTest.AddSelf;
var
  ms: TAutoMultiSet;
begin
  ms.Instance.AddAll(IntArray21);
  AssertTrue(ms.Instance.AddAll(ms.Instance) = System.Length(IntArray21));
  AssertTrue(ms.Instance.Count = System.Length(IntArray21) * 2);
  AssertTrue(ms.Instance.EntryCount = System.Length(IntArray21));
end;

procedure TChainHashMultisetTest.TestRemove;
var
  ms: TAutoMultiSet;
  c: SizeInt;
begin
  c := System.Length(IntArray21) * 2;
  ms.Instance := TMultiSet.Create(IntArray21);
  ms.Instance.AddAll(IntArray21);
  AssertTrue(ms.Instance.Count = c);
  AssertTrue(ms.Instance.EntryCount = System.Length(IntArray21));
  AssertTrue(ms.Instance.Remove(IntArray21[1]));
  AssertTrue(ms.Instance.Count = c - 1);
  AssertTrue(ms.Instance.EntryCount = System.Length(IntArray21));
  AssertTrue(ms.Instance.Remove(IntArray21[System.High(IntArray21)]));
  AssertTrue(ms.Instance.Count = c - 2);
  AssertTrue(ms.Instance.EntryCount = System.Length(IntArray21));
  AssertTrue(ms.Instance.Remove(IntArray21[1]));
  AssertTrue(ms.Instance.Count = c - 3);
  AssertTrue(ms.Instance.EntryCount = System.Length(IntArray21) - 1);
  AssertFalse(ms.Instance.Remove(-1));
  AssertTrue(ms.Instance.Count = c - 3);
  AssertFalse(ms.Instance.Remove(100));
  AssertTrue(ms.Instance.Count = c - 3);
end;

procedure TChainHashMultisetTest.RemoveArray;
var
  ms: TAutoMultiSet;
begin
  ms.Instance := TMultiSet.Create(IntArray21);
  ms.Instance.AddAll(IntArray21);
  AssertTrue(ms.Instance.RemoveAll(IntArray11) = 11);
  AssertTrue(ms.Instance.Count = 31);
  AssertTrue(ms.Instance.EntryCount = 21);
  AssertTrue(ms.Instance.RemoveAll(IntArray10) = 10);
  AssertTrue(ms.Instance.Count = 21);
  AssertTrue(ms.Instance.EntryCount = 21);
end;

procedure TChainHashMultisetTest.RemoveEnum;
var
  ms: TAutoMultiSet;
  e: IIntEnumerable;
begin
  ms.Instance := TMultiSet.Create(IntArray21);
  ms.Instance.AddAll(IntArray21);
  e := TIntArrayCursor.Create(TIntHelper.CreateCopy(IntArray11));
  AssertTrue(ms.Instance.RemoveAll(e) = 11);
  AssertTrue(ms.Instance.Count = 31);
  AssertTrue(ms.Instance.EntryCount = 21);
  e := TIntArrayCursor.Create(TIntHelper.CreateCopy(IntArray10));
  AssertTrue(ms.Instance.RemoveAll(e) = 10);
  AssertTrue(ms.Instance.Count = 21);
  AssertTrue(ms.Instance.EntryCount = 21);
end;

procedure TChainHashMultisetTest.RemoveSelf;
var
  ms: TAutoMultiSet;
begin
  ms.Instance := TMultiSet.Create(IntArray21);
  ms.Instance.AddAll(IntArray21);
  AssertTrue(ms.Instance.RemoveAll(ms.Instance) = 42);
  AssertTrue(ms.Instance.IsEmpty);
end;

procedure TChainHashMultisetTest.RemoveIfRegular;
var
  ms: TAutoMultiSet;
  I: Integer;
begin
  ms.Instance := TMultiSet.Create(IntArray21);
  ms.Instance.AddAll(IntArray21);
  AssertTrue(ms.Instance.RemoveIf(@IsEvenInt) = 20);
  AssertTrue(ms.Instance.Count = 22);
  AssertTrue(ms.Instance.ContainsAll(IntArray11));
  for I in IntArray11 do
    AssertTrue(ms.Instance[I] = 2);
end;

procedure TChainHashMultisetTest.RemoveIfDelegated;
var
  ms: TAutoMultiSet;
  I: Integer;
begin
  ms.Instance := TMultiSet.Create(IntArray21);
  ms.Instance.AddAll(IntArray21);
  AssertTrue(ms.Instance.RemoveIf(@IsEven) = 20);
  AssertTrue(ms.Instance.Count = 22);
  AssertTrue(ms.Instance.ContainsAll(IntArray11));
  for I in IntArray11 do
    AssertTrue(ms.Instance[I] = 2);
end;

procedure TChainHashMultisetTest.RemoveIfNested;
  function IsEvenInt(const aValue: Integer): Boolean;
  begin
    Result := not Odd(aValue);
  end;
var
  ms: TAutoMultiSet;
  I: Integer;
begin
  ms.Instance := TMultiSet.Create(IntArray21);
  ms.Instance.AddAll(IntArray21);
  AssertTrue(ms.Instance.RemoveIf(@IsEvenInt) = 20);
  AssertTrue(ms.Instance.Count = 22);
  AssertTrue(ms.Instance.ContainsAll(IntArray11));
  for I in IntArray11 do
    AssertTrue(ms.Instance[I] = 2);
end;

procedure TChainHashMultisetTest.ExtractIfRegular;
var
  ms, ms1: TAutoMultiSet;
  e: TIntArray;
  I: Integer;
begin
  ms.Instance := TMultiSet.Create(IntArray21);
  ms.Instance.AddAll(IntArray21);
  e := ms.Instance.ExtractIf(@IsEvenInt);
  AssertTrue(ms.Instance.Count = 22);
  AssertTrue(ms.Instance.ContainsAll(IntArray11));
  AssertTrue(System.Length(e) = 20);
  ms1.Instance := TMultiSet.Create(e);
  for I in IntArray11 do
    AssertTrue(ms.Instance[I] = 2);
  for I in IntArray10 do
    AssertTrue(ms1.Instance[I] = 2);
end;

procedure TChainHashMultisetTest.ExtractIfDelegated;
var
  ms, ms1: TAutoMultiSet;
  e: TIntArray;
  I: Integer;
begin
  ms.Instance := TMultiSet.Create(IntArray21);
  ms.Instance.AddAll(IntArray21);
  e := ms.Instance.ExtractIf(@IsEven);
  AssertTrue(ms.Instance.Count = 22);
  AssertTrue(ms.Instance.ContainsAll(IntArray11));
  AssertTrue(System.Length(e) = 20);
  ms1.Instance := TMultiSet.Create(e);
  for I in IntArray11 do
    AssertTrue(ms.Instance[I] = 2);
  for I in IntArray10 do
    AssertTrue(ms1.Instance[I] = 2);
end;

procedure TChainHashMultisetTest.ExtractIfNested;
  function IsEvenInt(const aValue: Integer): Boolean;
  begin
    Result := not Odd(aValue);
  end;
var
  ms, ms1: TAutoMultiSet;
  e: TIntArray;
  I: Integer;
begin
  ms.Instance := TMultiSet.Create(IntArray21);
  ms.Instance.AddAll(IntArray21);
  e := ms.Instance.ExtractIf(@IsEvenInt);
  AssertTrue(ms.Instance.Count = 22);
  AssertTrue(ms.Instance.ContainsAll(IntArray11));
  AssertTrue(System.Length(e) = 20);
  ms1.Instance := TMultiSet.Create(e);
  for I in IntArray11 do
    AssertTrue(ms.Instance[I] = 2);
  for I in IntArray10 do
    AssertTrue(ms1.Instance[I] = 2);
end;

procedure TChainHashMultisetTest.Retain;
var
  ms, ms1: TAutoMultiSet;
  I: Integer;
begin
  ms.Instance := TMultiSet.Create(IntArray21);
  ms.Instance.AddAll(IntArray21);
  ms1.Instance := TMultiSet.Create(IntArray11);
  AssertTrue(ms.Instance.Count = 42);
  ms.Instance.RetainAll(ms1.Instance);
  AssertTrue(ms.Instance.Count = 22);
  AssertTrue(ms.Instance.ContainsAll(ms1.Instance));
  for I in IntArray11 do
    AssertTrue(ms.Instance[I] = 2);
end;

procedure TChainHashMultisetTest.Retain_1;
var
  ms, ms1: TAutoMultiSet;
begin
  ms.Instance := TMultiSet.Create(IntArray10);
  ms.Instance.AddAll(IntArray10);
  AssertTrue(ms.Instance.Count = 20);
  ms1.Instance := TMultiSet.Create(IntArray11);
  ms.Instance.RetainAll(ms1.Instance);
  AssertTrue(ms.Instance.IsEmpty);
end;

procedure TChainHashMultisetTest.RetainSelf;
var
  ms: TAutoMultiSet;
begin
  ms.Instance := TMultiSet.Create(IntArray21);
  ms.Instance.AddAll(IntArray21);
  AssertTrue(ms.Instance.Count = 42);
  ms.Instance.RetainAll(ms.Instance);
  AssertTrue(ms.Instance.Count = 42);
  AssertTrue(ms.Instance.ContainsAll(IntArray21));
end;

procedure TChainHashMultisetTest.IsSuperset;
var
  ms, ms1: TAutoMultiSet;
begin
  ms.Instance := TMultiSet.Create(IntArray21);
  ms.Instance.AddAll(IntArray21);
  ms1.Instance := TMultiSet.Create(ms.Instance);
  AssertTrue(ms.Instance.IsSuperSet(ms1.Instance));
  AssertTrue(ms1.Instance.IsSuperSet(ms.Instance));
  AssertTrue(ms.Instance.IsSuperSet(ms.Instance));
  ms1.Instance.Remove(IntArray21[1]);
  AssertTrue(ms.Instance.IsSuperSet(ms1.Instance));
  AssertFalse(ms1.Instance.IsSuperSet(ms.Instance));
  ms1.Instance.Clear;
  AssertTrue(ms.Instance.IsSuperSet(ms1.Instance));
  AssertFalse(ms1.Instance.IsSuperSet(ms.Instance));
  ms.Instance.Clear;
  AssertTrue(ms.Instance.IsSuperSet(ms1.Instance));
  AssertTrue(ms1.Instance.IsSuperSet(ms.Instance));
end;

procedure TChainHashMultisetTest.IsSubset;
var
  ms, ms1: TAutoMultiSet;
begin
  ms.Instance := TMultiSet.Create(IntArray21);
  ms.Instance.AddAll(IntArray21);
  ms1.Instance := TMultiSet.Create(ms.Instance);
  AssertTrue(ms.Instance.IsSubSet(ms1.Instance));
  AssertTrue(ms1.Instance.IsSubSet(ms.Instance));
  AssertTrue(ms.Instance.IsSubSet(ms.Instance));
  ms1.Instance.Remove(IntArray21[1]);
  AssertFalse(ms.Instance.IsSubSet(ms1.Instance));
  AssertTrue(ms1.Instance.IsSubSet(ms.Instance));
  ms1.Instance.Clear;
  AssertFalse(ms.Instance.IsSubSet(ms1.Instance));
  AssertTrue(ms1.Instance.IsSubSet(ms.Instance));
  ms.Instance.Clear;
  AssertTrue(ms1.Instance.IsSubSet(ms.Instance));
  AssertTrue(ms.Instance.IsSubSet(ms1.Instance));
end;

procedure TChainHashMultisetTest.IsEqual;
var
  ms, ms1: TAutoMultiSet;
begin
  ms.Instance := TMultiSet.Create(IntArray21);
  ms.Instance.AddAll(IntArray21);
  ms1.Instance := TMultiSet.Create(ms.Instance);
  AssertTrue(ms.Instance.IsEqual(ms1.Instance));
  AssertTrue(ms1.Instance.IsEqual(ms.Instance));
  AssertTrue(ms.Instance.IsEqual(ms.Instance));
  ms1.Instance.Remove(IntArray21[1]);
  AssertFalse(ms.Instance.IsEqual(ms1.Instance));
  AssertFalse(ms1.Instance.IsEqual(ms.Instance));
  ms1.Instance.Clear;
  ms.Instance.Clear;
  AssertTrue(ms.Instance.IsEqual(ms1.Instance));
  AssertTrue(ms1.Instance.IsEqual(ms.Instance));
end;

procedure TChainHashMultisetTest.Intersecting;
var
  ms, ms1: TAutoMultiSet;
begin
  ms.Instance := TMultiSet.Create(IntArray21);
  ms1.Instance := TMultiSet.Create(IntArray11);
  AssertTrue(ms.Instance.Intersecting(ms.Instance));
  AssertTrue(ms.Instance.Intersecting(ms1.Instance));
  ms.Instance := TMultiSet.Create(IntArray10);
  AssertFalse(ms.Instance.Intersecting(ms1.Instance));
  AssertFalse(ms1.Instance.Intersecting(ms.Instance));
end;

procedure TChainHashMultisetTest.Intersect;
var
  ms, ms1: TAutoMultiSet;
  I: Integer;
begin
  ms.Instance := TMultiSet.Create(IntArray21);
  ms.Instance.AddAll(IntArray21);
  AssertTrue(ms.Instance.Count = 42);
  ms1.Instance := TMultiSet.Create(IntArray11);
  ms1.Instance.AddAll([-4, -3, -2, -1, 0]);
  ms.Instance.Intersect(ms1.Instance);
  AssertTrue(ms.Instance.Count = 11);
  for I in IntArray11 do
    AssertTrue(ms.Instance[I] = 1);
end;

procedure TChainHashMultisetTest.Join;
var
  ms, ms1: TAutoMultiSet;
  I: Integer;
begin
  ms1.Instance := TMultiSet.Create(IntArray11);
  ms.Instance.Join(ms1.Instance);
  AssertTrue(ms.Instance.IsEqual(ms1.Instance));
  ms1.Instance := TMultiSet.Create(IntArray10);
  ms1.Instance.AddAll(ms1.Instance);
  ms.Instance.Join(ms1.Instance);
  for I in IntArray11 do
    AssertTrue(ms.Instance[I] = 1);
  for I in IntArray10 do
    AssertTrue(ms.Instance[I] = 2);
end;

procedure TChainHashMultisetTest.ArithmeticAdd;
var
  ms, ms1: TAutoMultiSet;
  I: Integer;
begin
  ms1.Instance := TMultiSet.Create(IntArray11);
  ms.Instance.ArithmeticAdd(ms1.Instance);
  AssertTrue(ms.Instance.IsEqual(ms1.Instance));
  ms.Instance.AddAll(IntArray10);
  ms.Instance.ArithmeticAdd(ms1.Instance);
  for I in IntArray11 do
    AssertTrue(ms.Instance[I] = 2);
  for I in IntArray10 do
    AssertTrue(ms.Instance[I] = 1);
  ms.Instance.ArithmeticAdd(ms.Instance);
  for I in IntArray11 do
    AssertTrue(ms.Instance[I] = 4);
  for I in IntArray10 do
    AssertTrue(ms.Instance[I] = 2);
end;

procedure TChainHashMultisetTest.ArithmeticSubtract;
var
  ms, ms1: TAutoMultiSet;
begin
  ms1.Instance := TMultiSet.Create(IntArray11);
  ms.Instance.ArithmeticSubtract(ms1.Instance);
  AssertTrue(ms.Instance.IsEmpty);
  ms.Instance.AddAll(IntArray10);
  ms.Instance.ArithmeticSubtract(ms1.Instance);
  AssertTrue(ms.Instance.Count = 10);
  AssertTrue(ms.Instance.ContainsAll(IntArray10));
  ms.Instance.AddAll(IntArray11);
  AssertTrue(ms.Instance.ContainsAll(IntArray21));
  ms.Instance.ArithmeticSubtract(ms1.Instance);
  AssertTrue(ms.Instance.Count = 10);
  AssertTrue(ms.Instance.ContainsAll(IntArray10));
  ms1.Instance.AddAll(IntArray10);
  ms.Instance.ArithmeticSubtract(ms1.Instance);
  AssertTrue(ms.Instance.IsEmpty);
end;

procedure TChainHashMultisetTest.SymmetricSubtract;
var
  ms, ms1: TAutoMultiSet;
begin
  ms1.Instance := TMultiSet.Create(IntArray11);
  ms.Instance.SymmetricSubtract(ms1.Instance);
  AssertTrue(ms.Instance.Count = 11);
  AssertTrue(ms.Instance.ContainsAll(IntArray11));
  ms.Instance.AddAll(IntArray10);
  ms.Instance.SymmetricSubtract(ms1.Instance);
  AssertTrue(ms.Instance.Count = 10);
  AssertTrue(ms.Instance.ContainsAll(IntArray10));
  ms.Instance.AddAll(IntArray11);
  ms.Instance.AddAll(IntArray21);
  AssertTrue(ms.Instance.Count = 42);
  ms1.Instance.AddAll(IntArray10);
  ms.Instance.SymmetricSubtract(ms1.Instance);
  AssertTrue(ms.Instance.Count = 21);
  AssertTrue(ms.Instance.ContainsAll(IntArray21));
  ms.Instance.SymmetricSubtract(ms1.Instance);
  AssertTrue(ms.Instance.IsEmpty);
end;

procedure TChainHashMultisetTest.ObjectMultiSetRemove;
var
  ms: TAutoObjMultiSet;
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
      ms.Instance.Add(a[I]);
    end;
  AssertTrue(ms.Instance.Count = TestSize);
  ms.Instance.AddAll(a);
  AssertTrue(ms.Instance.Count = TestSize * 2);
  for I := 0 to System.High(a) do
    AssertTrue(ms.Instance.Remove(a[I]));
  AssertTrue(ms.Instance.Count = TestSize);
  AssertTrue(Counter.Count = 0);
  for I := 0 to System.High(a) do
    begin
      AssertTrue(ms.Instance.Remove(a[I]));
      AssertTrue(ms.Instance.Count = Pred(TestSize - I));
      AssertTrue(Counter.Count = Succ(I));
    end;
  AssertTrue(ms.Instance.IsEmpty);
  AssertTrue(Counter.Count = TestSize);
end;

procedure TChainHashMultisetTest.ObjectMultiSetRemoveArray;
var
  ms: TAutoObjMultiSet;
  a: TObjArray;
  I, TestSize: Integer;
  Counter: TCounter;
begin
  TestSize := 56;
  Counter := Default(TCounter);
  for I := 1 to TestSize do
    ms.Instance.Add(TTestObj.Create(Counter.Increment));
  AssertTrue(ms.Instance.Count = TestSize);
  a := ms.Instance.ToArray;
  AssertTrue(System.Length(a) = TestSize);
  ms.Instance.AddAll(a);
  AssertTrue(ms.Instance.Count = TestSize * 2);
  AssertTrue(ms.Instance.RemoveAll(a) = TestSize);
  AssertTrue(Counter.Count = 0);
  AssertTrue(ms.Instance.RemoveAll(a) = TestSize);
  AssertTrue(ms.Instance.IsEmpty);
  AssertTrue(Counter.Count = TestSize);
end;

procedure TChainHashMultisetTest.ObjectMultiSetRemoveEnum;
type
  TFakeEnum = specialize TGArrayCursor<TTestObj>;
var
  ms: TAutoObjMultiSet;
  a: TObjArray;
  e: TFakeEnum;
  I, TestSize: Integer;
  Counter: TCounter;
begin
  TestSize := 52;
  Counter := Default(TCounter);
  for I := 1 to TestSize do
    ms.Instance.Add(TTestObj.Create(Counter.Increment));
  AssertTrue(ms.Instance.Count = TestSize);
  a := ms.Instance.ToArray;
  AssertTrue(System.Length(a) = TestSize);
  ms.Instance.AddAll(a);
  AssertTrue(ms.Instance.Count = TestSize * 2);
  e := TFakeEnum.Create(a);
  AssertTrue(ms.Instance.RemoveAll(e) = TestSize);
  AssertTrue(Counter.Count = 0);
  e := TFakeEnum.Create(a);
  AssertTrue(ms.Instance.RemoveAll(e) = TestSize);
  AssertTrue(ms.Instance.IsEmpty);
  AssertTrue(Counter.Count = TestSize);
end;

procedure TChainHashMultisetTest.ObjectMultiSetRemoveSelf;
var
  ms: TAutoObjMultiSet;
  I, TestSize: Integer;
  Counter: TCounter;
begin
  TestSize := 100;
  Counter := Default(TCounter);
  for I := 1 to TestSize do
    ms.Instance.Add(TTestObj.Create(Counter.Increment));
  ms.Instance.AddAll(ms.Instance);
  AssertTrue(ms.Instance.Count = TestSize * 2);
  AssertTrue(ms.Instance.RemoveAll(ms.Instance) = TestSize * 2);
  AssertTrue(ms.Instance.IsEmpty);
  AssertTrue(Counter.Count = TestSize);
end;

procedure TChainHashMultisetTest.ObjectMultiSetRemoveIfRegular;
var
  ms: TAutoObjMultiSet;
  I, TestSize: Integer;
  Counter: TCounter;
  o: TTestObj;
begin
  TestSize := 100;
  Counter := Default(TCounter);
  for I := 1 to TestSize do
    ms.Instance.Add(TTestObj.Create(I, Counter.Increment));
  ms.Instance.AddAll(ms.Instance);
  AssertTrue(ms.Instance.Count = TestSize * 2);
  AssertTrue(ms.Instance.RemoveIf(@IsEvenObj) = TestSize);
  AssertTrue(Counter.Count = TestSize div 2);
  AssertTrue(ms.Instance.Count = TestSize);
  for o in ms.Instance do
    AssertTrue(Odd(o.Value));
  ms.Instance := Default(TObjMultiSet);
  AssertTrue(Counter.Count = TestSize);
end;

procedure TChainHashMultisetTest.ObjectMultiSetRemoveIfDelegated;
var
  ms: TAutoObjMultiSet;
  I, TestSize: Integer;
  Counter: TCounter;
  o: TTestObj;
begin
  TestSize := 100;
  Counter := Default(TCounter);
  for I := 1 to TestSize do
    ms.Instance.Add(TTestObj.Create(I, Counter.Increment));
  ms.Instance.AddAll(ms.Instance);
  AssertTrue(ms.Instance.Count = TestSize * 2);
  AssertTrue(ms.Instance.RemoveIf(@ObjIsEven) = TestSize);
  AssertTrue(Counter.Count = TestSize div 2);
  AssertTrue(ms.Instance.Count = TestSize);
  for o in ms.Instance do
    AssertTrue(Odd(o.Value));
  ms.Instance := Default(TObjMultiSet);
  AssertTrue(Counter.Count = TestSize);
end;

procedure TChainHashMultisetTest.ObjectMultiSetRemoveIfNested;
  function IsEvenNest(const aObj: TTestObj): Boolean;
  begin
    Result := not Odd(aObj.Value);
  end;
var
  ms: TAutoObjMultiSet;
  I, TestSize: Integer;
  Counter: TCounter;
  o: TTestObj;
begin
  TestSize := 100;
  Counter := Default(TCounter);
  for I := 1 to TestSize do
    ms.Instance.Add(TTestObj.Create(I, Counter.Increment));
  ms.Instance.AddAll(ms.Instance);
  AssertTrue(ms.Instance.Count = TestSize * 2);
  AssertTrue(ms.Instance.RemoveIf(@IsEvenNest) = TestSize);
  AssertTrue(Counter.Count = TestSize div 2);
  AssertTrue(ms.Instance.Count = TestSize);
  for o in ms.Instance do
    AssertTrue(Odd(o.Value));
  ms.Instance := Default(TObjMultiSet);
  AssertTrue(Counter.Count = TestSize);
end;

procedure TChainHashMultisetTest.ObjectMultiSetExtractIfRegular;
var
  ms: TAutoObjMultiSet;
  I, TestSize: Integer;
  Counter: TCounter;
  a: TObjArray;
  o: TTestObj;
begin
  TestSize := 100;
  Counter := Default(TCounter);
  for I := 1 to TestSize do
    ms.Instance.Add(TTestObj.Create(I, Counter.Increment));
  ms.Instance.AddAll(ms.Instance);
  AssertTrue(ms.Instance.Count = TestSize * 2);
  a := ms.Instance.ExtractIf(@IsEvenObj);
  AssertTrue(System.Length(a) = TestSize);
  AssertTrue(Counter.Count = 0);
  AssertTrue(ms.Instance.Count = TestSize);
  for o in ms.Instance do
    AssertTrue(Odd(o.Value));
  for o in a do
    AssertTrue(not Odd(o.Value));
  ms.Instance.Clear;
  AssertTrue(Counter.Count = TestSize div 2);
  ms.Instance.AddAll(a);
  ms.Instance.Clear;
  AssertTrue(Counter.Count = TestSize);
end;

procedure TChainHashMultisetTest.ObjectMultiSetExtractIfDelegated;
var
  ms: TAutoObjMultiSet;
  I, TestSize: Integer;
  Counter: TCounter;
  a: TObjArray;
  o: TTestObj;
begin
  TestSize := 100;
  Counter := Default(TCounter);
  for I := 1 to TestSize do
    ms.Instance.Add(TTestObj.Create(I, Counter.Increment));
  ms.Instance.AddAll(ms.Instance);
  AssertTrue(ms.Instance.Count = TestSize * 2);
  a := ms.Instance.ExtractIf(@ObjIsEven);
  AssertTrue(System.Length(a) = TestSize);
  AssertTrue(Counter.Count = 0);
  AssertTrue(ms.Instance.Count = TestSize);
  for o in ms.Instance do
    AssertTrue(Odd(o.Value));
  for o in a do
    AssertTrue(not Odd(o.Value));
  ms.Instance.Clear;
  AssertTrue(Counter.Count = TestSize div 2);
  ms.Instance.AddAll(a);
  ms.Instance.Clear;
  AssertTrue(Counter.Count = TestSize);
end;

procedure TChainHashMultisetTest.ObjectMultiSetExtractIfNested;
  function IsEvenNest(const aObj: TTestObj): Boolean;
  begin
    Result := not Odd(aObj.Value);
  end;
var
  ms: TAutoObjMultiSet;
  I, TestSize: Integer;
  Counter: TCounter;
  a: TObjArray;
  o: TTestObj;
begin
  TestSize := 100;
  Counter := Default(TCounter);
  for I := 1 to TestSize do
    ms.Instance.Add(TTestObj.Create(I, Counter.Increment));
  ms.Instance.AddAll(ms.Instance);
  AssertTrue(ms.Instance.Count = TestSize * 2);
  a := ms.Instance.ExtractIf(@IsEvenNest);
  AssertTrue(System.Length(a) = TestSize);
  AssertTrue(Counter.Count = 0);
  AssertTrue(ms.Instance.Count = TestSize);
  for o in ms.Instance do
    AssertTrue(Odd(o.Value));
  for o in a do
    AssertTrue(not Odd(o.Value));
  ms.Instance.Clear;
  AssertTrue(Counter.Count = TestSize div 2);
  ms.Instance.AddAll(a);
  ms.Instance.Clear;
  AssertTrue(Counter.Count = TestSize);
end;

procedure TChainHashMultisetTest.ObjectMultiSetRetain;
type
  TObjSet     = specialize TGObjHashSetLP<TTestObj>;
  TAutoObjSet = specialize TGAutoRef<TObjSet>;
var
  ms: TAutoObjMultiSet;
  s1: TAutoObjSet;
  I, TestSize: Integer;
  Counter: TCounter;
  a: TObjArray;
begin
  TestSize := 100;
  Counter := Default(TCounter);
  System.SetLength(a, TestSize);
  s1.Instance := TObjSet.Create(False);

  for I := 0 to System.High(a) do
    a[I] := TTestObj.Create(Counter.Increment);
  s1.Instance.AddAll(a);
  ms.Instance.AddAll(a);
  ms.Instance.AddAll(a);
  for I := 1 to TestSize do
    ms.Instance.Add(TTestObj.Create(Counter.Increment));
  AssertTrue(ms.Instance.Count = TestSize * 3);
  ms.Instance.RetainAll(s1.Instance);
  AssertTrue(ms.Instance.Count = TestSize * 2);
  AssertTrue(Counter.Count = TestSize);
  AssertTrue(ms.Instance.ContainsAll(s1.Instance));
  ms.Instance := Default(TObjMultiSet);
  AssertTrue(Counter.Count = TestSize * 2);
end;

procedure TChainHashMultisetTest.ObjectMultiSetIntersect;
var
  ms, ms1: TAutoObjMultiSet;
  I, TestSize: Integer;
  Counter: TCounter;
  a: TObjArray;
  o: TTestObj;
begin
  TestSize := 50;
  Counter := Default(TCounter);
  System.SetLength(a, TestSize);
  for I := 0 to System.High(a) do
    a[I] := TTestObj.Create(Counter.Increment);

  ms.Instance.AddAll(a);
  ms.Instance.AddAll(a);

  for o in a do
    AssertTrue(ms.Instance[o] = 2);

  for I := 1 to TestSize do
    ms.Instance.Add(TTestObj.Create(Counter.Increment));

  AssertTrue(Counter.Count = 0);
  AssertTrue(ms.Instance.Count = TestSize * 3);

  ms1.Instance.OwnsObjects := False;
  AssertTrue(ms1.Instance.AddAll(a) = TestSize);

  ms.Instance.Intersect(ms1.Instance);

  AssertTrue(ms.Instance.Count = TestSize);
  AssertTrue(Counter.Count = TestSize);

  for o in a do
    AssertTrue(ms.Instance[o] = 1);
end;

procedure TChainHashMultisetTest.ObjectMultiSetArithmeticSubtract;
var
  ms, ms1: TAutoObjMultiSet;
  I, TestSize: Integer;
  Counter: TCounter;
  a: TObjArray;
  o: TTestObj;
begin
  TestSize := 55;
  Counter := Default(TCounter);
  System.SetLength(a, TestSize);
  for I := 0 to System.High(a) do
    a[I] := TTestObj.Create(Counter.Increment);

  ms.Instance.AddAll(a);
  ms.Instance.AddAll(a);
  ms.Instance.AddAll(a);

  ms1.Instance.OwnsObjects := False;

  ms1.Instance.AddAll(a);

  for I := 1 to TestSize do
    ms.Instance.Add(TTestObj.Create(Counter.Increment));

  AssertTrue(Counter.Count = 0);
  AssertTrue(ms.Instance.Count = TestSize * 4);
  for o in a do
    AssertTrue(ms.Instance[o] = 3);

  ms.Instance.ArithmeticSubtract(ms1.Instance);
  AssertTrue(ms.Instance.Count = TestSize * 3);
  for o in a do
    AssertTrue(ms.Instance[o] = 2);

  ms.Instance.ArithmeticSubtract(ms1.Instance);
  AssertTrue(ms.Instance.Count = TestSize * 2);
  for o in a do
    AssertTrue(ms.Instance[o] = 1);

  ms.Instance.ArithmeticSubtract(ms1.Instance);
  AssertTrue(ms.Instance.Count = TestSize);

  AssertTrue(Counter.Count = TestSize);
end;

procedure TChainHashMultisetTest.ObjectMultiSetSymmetricSubtract;
var
  ms, ms1: TAutoObjMultiSet;
  I, TestSize: Integer;
  Counter: TCounter;
begin
  TestSize := 65;
  Counter := Default(TCounter);

  for I := 1 to TestSize do
    ms.Instance.Add(TTestObj.Create(Counter.Increment));
  ms.Instance.AddAll(ms.Instance);
  ms.Instance.OwnsObjects := False;

  for I := 1 to TestSize do
    ms1.Instance.Add(TTestObj.Create(Counter.Increment));
  ms1.Instance.OwnsObjects := False;

  AssertTrue(Counter.Count = 0);
  AssertTrue(ms.Instance.Count = TestSize * 2);

  ms.Instance.SymmetricSubtract(ms1.Instance);
  AssertTrue(ms.Instance.Count = TestSize * 3);

  ms1.Instance.SymmetricSubtract(ms.Instance);
  AssertTrue(ms1.Instance.Count = TestSize * 2);

  ms.Instance.SymmetricSubtract(ms1.Instance);
  AssertTrue(ms.Instance.Count = TestSize);

  ms.Instance.ArithmeticSubtract(ms1.Instance);
  AssertTrue(ms.Instance.Count = TestSize);

  AssertTrue(Counter.Count = 0);

  ms.Instance.OwnsObjects := True;
  ms1.Instance.OwnsObjects := True;

  ms.Instance := Default(TObjMultiSet);
  ms1.Instance := Default(TObjMultiSet);

  AssertTrue(Counter.Count = TestSize * 2);
end;

{ TLiteHashMultisetLPTest }

function TLiteHashMultisetLPTest.IsEven(const aValue: Integer): Boolean;
begin
  Result := not Odd(aValue);
end;

procedure TLiteHashMultisetLPTest.Clear;
var
  ms: TMultiSet;
begin
  ms.AddAll(IntArray11);
  AssertTrue(ms.Count = 11);
  AssertTrue(ms.Capacity >= 11);
  ms.Clear;
  AssertTrue(ms.Count = 0);
  AssertTrue(ms.Capacity = 0);
end;

procedure TLiteHashMultisetLPTest.EnsureCapacity;
var
  ms: TMultiSet;
  c: SizeInt;
begin
  c := ms.ExpandTreshold;
  ms.EnsureCapacity(c + 100);
  AssertTrue(ms.ExpandTreshold >= c + 100);
end;

procedure TLiteHashMultisetLPTest.TrimToFit;
var
  ms: TMultiSet;
begin
  ms.AddAll(IntArray11);
  AssertTrue(ms.Capacity = 32);
  ms.LoadFactor := 0.8;
  ms.TrimToFit;
  AssertTrue(ms.Capacity = 16);
end;

procedure TLiteHashMultisetLPTest.Add;
var
  ms: TMultiSet;
begin
  AssertTrue(ms.Count = 0);
  ms.Add(1);
  AssertTrue(ms.Count = 1);
  AssertTrue(ms.Contains(1));
  ms.Add(51);
  AssertTrue(ms.Contains(51));
  AssertTrue(ms.Count = 2);
  AssertTrue(ms.EntryCount = 2);
  ms.Add(1);
  ms.Add(51);
  AssertTrue(ms.Count = 4);
  AssertTrue(ms.EntryCount = 2);
  AssertTrue(ms[1] = 2);
  AssertTrue(ms[51] = 2);
  ms.Add(52);
  AssertTrue(ms.Contains(52));
  AssertTrue(ms.Count = 5);
  AssertTrue(ms.EntryCount = 3);
  AssertTrue(ms[52] = 1);
end;

procedure TLiteHashMultisetLPTest.Add100;
var
  ms: TMultiSet;
  I: Integer;
begin
  for I := 1 to 100 do
    ms.Add(I);
  AssertTrue(ms.Count = 100);
  for I := 1 to 100 do
    ms.Add(I);
  AssertTrue(ms.Count = 200);
  AssertTrue(ms.EntryCount = 100);
  for I := 1 to 100 do
    AssertTrue(ms[I] = 2);
  ms[10] := 52;
  AssertTrue(ms.Count = 250);
  ms[10] := 12;
  AssertTrue(ms.Count = 210);
end;

procedure TLiteHashMultisetLPTest.AddArray;
var
  ms: TMultiSet;
  I: Integer;
begin
  AssertTrue(ms.AddAll(IntArray21) = Length(IntArray21));
  AssertTrue(ms.Count = Length(IntArray21));
  AssertTrue(ms.EntryCount = Length(IntArray21));
  AssertTrue(ms.ContainsAll(IntArray21));
  AssertTrue(ms.AddAll(IntArray21) = Length(IntArray21));
  AssertTrue(ms.Count = Length(IntArray21) * 2);
  AssertTrue(ms.EntryCount = Length(IntArray21));
  for I in IntArray21 do
    AssertTrue(ms[I] = 2);
end;

procedure TLiteHashMultisetLPTest.AddEnum;
var
  ms: TMultiSet;
  I: Integer;
begin
  AssertTrue(ms.AddAll(TIntArrayCursor.Create(TIntHelper.CreateCopy(IntArray21))) = Length(IntArray21));
  AssertTrue(ms.Count = Length(IntArray21));
  AssertTrue(ms.EntryCount = Length(IntArray21));
  AssertTrue(ms.ContainsAll(IntArray21));
  AssertTrue(ms.AddAll(TIntArrayCursor.Create(TIntHelper.CreateCopy(IntArray21))) = Length(IntArray21));
  AssertTrue(ms.Count = Length(IntArray21) * 2);
  AssertTrue(ms.EntryCount = Length(IntArray21));
  for I in IntArray21 do
    AssertTrue(ms[I] = 2);
end;

procedure TLiteHashMultisetLPTest.AddSelf;
var
  ms: TMultiSet;
  I: Integer;
begin
  ms.AddAll(IntArray21);
  AssertTrue(ms.AddAll(ms) = Length(IntArray21));
  AssertTrue(ms.Count = Length(IntArray21) * 2);
  for I in IntArray21 do
    AssertTrue(ms[I] = 2);
end;

procedure TLiteHashMultisetLPTest.Remove;
var
  ms: TMultiSet;
  Len, Len2: SizeInt;
begin
  Len := Length(IntArray21);
  Len2 := Len * 2;
  ms.AddAll(IntArray21);
  ms.AddAll(IntArray21);
  AssertTrue(ms.Count = Len2);
  AssertTrue(ms.EntryCount = Len);
  AssertTrue(ms.Remove(IntArray21[1]));
  AssertTrue(ms.Count = Len2 - 1);
  AssertTrue(ms.EntryCount = Len);
  AssertTrue(ms.Remove(IntArray21[High(IntArray21)]));
  AssertTrue(ms.Count = Len2 - 2);
  AssertTrue(ms.EntryCount = Len);
  AssertTrue(ms.Remove(IntArray21[1]));
  AssertTrue(ms.Count = Len2 - 3);
  AssertTrue(ms.EntryCount = Len - 1);
  AssertFalse(ms.Remove(-1));
  AssertTrue(ms.Count = Len2 - 3);
  AssertFalse(ms.Remove(100));
  AssertTrue(ms.Count = Len2 - 3);
end;

procedure TLiteHashMultisetLPTest.RemoveArray;
var
  ms: TMultiSet;
begin
  ms.AddAll(IntArray21);
  ms.AddAll(IntArray21);
  AssertTrue(ms.Count = 42);
  AssertTrue(ms.RemoveAll(IntArray11) = 11);
  AssertTrue(ms.Count = 31);
  AssertTrue(ms.EntryCount = 21);
  AssertTrue(ms.RemoveAll(IntArray10) = 10);
  AssertTrue(ms.Count = 21);
  AssertTrue(ms.EntryCount = 21);
end;

procedure TLiteHashMultisetLPTest.RemoveEnum;
var
  ms: TMultiSet;
begin
  ms.AddAll(IntArray21);
  ms.AddAll(IntArray21);
  AssertTrue(ms.Count = 42);
  AssertTrue(ms.RemoveAll(TIntArrayCursor.Create(TIntHelper.CreateCopy(IntArray11))) = 11);
  AssertTrue(ms.Count = 31);
  AssertTrue(ms.EntryCount = 21);
  AssertTrue(ms.RemoveAll(TIntArrayCursor.Create(TIntHelper.CreateCopy(IntArray10))) = 10);
  AssertTrue(ms.Count = 21);
  AssertTrue(ms.EntryCount = 21);
end;

procedure TLiteHashMultisetLPTest.RemoveSelf;
var
  ms: TMultiSet;
begin
  ms.AddAll(IntArray21);
  ms.AddAll(IntArray21);
  AssertTrue(ms.RemoveAll(ms) = 42);
  AssertTrue(ms.IsEmpty);
end;

procedure TLiteHashMultisetLPTest.RemoveIfRegular;
var
  ms: TMultiSet;
  I: Integer;
begin
  ms.AddAll(IntArray21);
  ms.AddAll(IntArray21);
  AssertTrue(ms.RemoveIf(@IsEvenInt) = 20);
  AssertTrue(ms.Count = 22);
  AssertTrue(ms.ContainsAll(IntArray11));
  for I in IntArray11 do
    AssertTrue(ms[I] = 2);
end;

procedure TLiteHashMultisetLPTest.RemoveIfDelegated;
var
  ms: TMultiSet;
  I: Integer;
begin
  ms.AddAll(IntArray21);
  ms.AddAll(IntArray21);
  AssertTrue(ms.RemoveIf(@IsEven) = 20);
  AssertTrue(ms.Count = 22);
  AssertTrue(ms.ContainsAll(IntArray11));
  for I in IntArray11 do
    AssertTrue(ms[I] = 2);
end;

procedure TLiteHashMultisetLPTest.RemoveIfNested;
  function IsEvenInt(const aValue: Integer): Boolean;
  begin
    Result := not Odd(aValue);
  end;
var
  ms: TMultiSet;
  I: Integer;
begin
  ms.AddAll(IntArray21);
  ms.AddAll(IntArray21);
  AssertTrue(ms.RemoveIf(@IsEvenInt) = 20);
  AssertTrue(ms.Count = 22);
  AssertTrue(ms.ContainsAll(IntArray11));
  for I in IntArray11 do
    AssertTrue(ms[I] = 2);
end;

procedure TLiteHashMultisetLPTest.ExtractIfRegular;
var
  ms, ms1: TMultiSet;
  e: TIntArray;
  I: Integer;
begin
  ms.AddAll(IntArray21);
  ms.AddAll(IntArray21);
  e := ms.ExtractIf(@IsEvenInt);
  AssertTrue(ms.Count = 22);
  AssertTrue(ms.ContainsAll(IntArray11));
  for I in IntArray11 do
    AssertTrue(ms[I] = 2);
  AssertTrue(Length(e) = 20);
  ms1.AddAll(e);
  for I in IntArray10 do
    AssertTrue(ms1[I] = 2);
end;

procedure TLiteHashMultisetLPTest.ExtractIfDelegated;
var
  ms, ms1: TMultiSet;
  e: TIntArray;
  I: Integer;
begin
  ms.AddAll(IntArray21);
  ms.AddAll(IntArray21);
  e := ms.ExtractIf(@IsEven);
  AssertTrue(ms.Count = 22);
  AssertTrue(ms.ContainsAll(IntArray11));
  for I in IntArray11 do
    AssertTrue(ms[I] = 2);
  AssertTrue(Length(e) = 20);
  ms1.AddAll(e);
  for I in IntArray10 do
    AssertTrue(ms1[I] = 2);
end;

procedure TLiteHashMultisetLPTest.ExtractIfNested;
var
  ms, ms1: TMultiSet;
  e: TIntArray;
  I: Integer;
  function IsEvenInt(const aValue: Integer): Boolean;
  begin
    Result := not Odd(aValue);
  end;
begin
  ms.AddAll(IntArray21);
  ms.AddAll(IntArray21);
  e := ms.ExtractIf(@IsEvenInt);
  AssertTrue(ms.Count = 22);
  AssertTrue(ms.ContainsAll(IntArray11));
  for I in IntArray11 do
    AssertTrue(ms[I] = 2);
  AssertTrue(Length(e) = 20);
  ms1.AddAll(e);
  for I in IntArray10 do
    AssertTrue(ms1[I] = 2);
end;

procedure TLiteHashMultisetLPTest.Retain;
var
  ms: TMultiSet;
  s: specialize TGHashMultiSetLP<Integer>;
  I: Integer;
begin
  ms.AddAll(IntArray21);
  ms.AddAll(IntArray21);
  AssertTrue(ms.Count = 42);
  s := specialize TGHashMultiSetLP<Integer>.Create(IntArray11);
  try
    ms.RetainAll(s);
  finally
    s.Free;
  end;
  AssertTrue(ms.Count = 22);
  AssertTrue(ms.ContainsAll(IntArray11));
  for I in IntArray11 do
    AssertTrue(ms[I] = 2);
end;

procedure TLiteHashMultisetLPTest.Retain_1;
var
  ms: TMultiSet;
  s: specialize TGHashMultiSetLP<Integer>;
begin
  ms.AddAll(IntArray10);
  ms.AddAll(IntArray10);
  AssertTrue(ms.Count = 20);
  s := specialize TGHashMultiSetLP<Integer>.Create(IntArray11);
  try
    ms.RetainAll(s);
  finally
    s.Free;
  end;
  AssertTrue(ms.IsEmpty);
end;

procedure TLiteHashMultisetLPTest.IsSuperset;
var
  ms, ms1: TMultiSet;
begin
  ms.AddAll(IntArray21);
  ms.AddAll(IntArray21);
  ms1.AddAll(ms);
  AssertTrue(ms.IsSuperSet(ms1));
  AssertTrue(ms1.IsSuperSet(ms));
  AssertTrue(ms.IsSuperSet(ms));
  ms1.Remove(IntArray21[1]);
  AssertTrue(ms.IsSuperSet(ms1));
  AssertFalse(ms1.IsSuperSet(ms));
  ms1.Clear;
  AssertTrue(ms.IsSuperSet(ms1));
  AssertFalse(ms1.IsSuperSet(ms));
  ms.Clear;
  AssertTrue(ms.IsSuperSet(ms1));
  AssertTrue(ms1.IsSuperSet(ms));
end;

procedure TLiteHashMultisetLPTest.IsSubset;
var
  ms, ms1: TMultiSet;
begin
  ms.AddAll(IntArray21);
  ms.AddAll(IntArray21);
  ms1.AddAll(ms);
  AssertTrue(ms.IsSubSet(ms1));
  AssertTrue(ms1.IsSubSet(ms));
  AssertTrue(ms.IsSubSet(ms));
  ms1.Remove(IntArray21[1]);
  AssertFalse(ms.IsSubSet(ms1));
  AssertTrue(ms1.IsSubSet(ms));
  ms1.Clear;
  AssertFalse(ms.IsSubSet(ms1));
  AssertTrue(ms1.IsSubSet(ms));
  ms.Clear;
  AssertTrue(ms1.IsSubSet(ms));
  AssertTrue(ms.IsSubSet(ms1));
end;

procedure TLiteHashMultisetLPTest.IsEqual;
var
  ms, ms1: TMultiSet;
begin
  ms.AddAll(IntArray21);
  ms.AddAll(IntArray21);
  ms1.AddAll(ms);
  AssertTrue(ms.IsEqual(ms1));
  AssertTrue(ms1.IsEqual(ms));
  AssertTrue(ms.IsEqual(ms));
  ms1.Remove(IntArray21[1]);
  AssertFalse(ms.IsEqual(ms1));
  AssertFalse(ms1.IsEqual(ms));
  ms.Remove(IntArray21[1]);
  AssertTrue(ms1.IsEqual(ms));
  AssertTrue(ms.IsEqual(ms));
  ms1.Clear;
  ms.Clear;
  AssertTrue(ms.IsEqual(ms1));
  AssertTrue(ms1.IsEqual(ms));
end;

procedure TLiteHashMultisetLPTest.Intersecting;
var
  ms, ms1: TMultiSet;
begin
  ms.AddAll(IntArray21);
  ms1.AddAll(IntArray11);
  AssertTrue(ms.Intersecting(ms));
  AssertTrue(ms.Intersecting(ms1));
  ms.RemoveAll(IntArray11);
  AssertFalse(ms.Intersecting(ms1));
  AssertFalse(ms1.Intersecting(ms));
end;

procedure TLiteHashMultisetLPTest.Intersect;
var
  ms, ms1: TMultiSet;
  I: Integer;
begin
  ms.AddAll(IntArray21);
  ms.AddAll(IntArray21);
  AssertTrue(ms.Count = 42);
  ms1.AddAll(IntArray11);
  ms1.AddAll([-4, -3, -2, -1, 0]);
  ms.Intersect(ms1);
  AssertTrue(ms.Count = 11);
  for I in IntArray11 do
    AssertTrue(ms[I] = 1);
end;

procedure TLiteHashMultisetLPTest.Join;
var
  ms, ms1: TMultiSet;
  I: Integer;
begin
  ms1.AddAll(IntArray11);
  ms.Join(ms1);
  AssertTrue(ms.IsEqual(ms1));
  ms1.Clear;
  ms1.AddAll(IntArray10);
  ms1.AddAll(ms1);
  ms.Join(ms1);
  for I in IntArray11 do
    AssertTrue(ms[I] = 1);
  for I in IntArray10 do
    AssertTrue(ms[I] = 2);
end;

procedure TLiteHashMultisetLPTest.ArithmeticAdd;
var
  ms, ms1: TMultiSet;
  I: Integer;
begin
  ms1.AddAll(IntArray11);
  ms.ArithmeticAdd(ms1);
  AssertTrue(ms.IsEqual(ms1));
  ms.AddAll(IntArray10);
  ms.ArithmeticAdd(ms1);
  for I in IntArray11 do
    AssertTrue(ms[I] = 2);
  for I in IntArray10 do
    AssertTrue(ms[I] = 1);
  ms.ArithmeticAdd(ms);
  for I in IntArray11 do
    AssertTrue(ms[I] = 4);
  for I in IntArray10 do
    AssertTrue(ms[I] = 2);
end;

procedure TLiteHashMultisetLPTest.ArithmeticSubtract;
var
  ms, ms1: TMultiSet;
begin
  ms1.AddAll(IntArray11);
  ms.ArithmeticSubtract(ms1);
  AssertTrue(ms.IsEmpty);
  ms.AddAll(IntArray10);
  ms.ArithmeticSubtract(ms1);
  AssertTrue(ms.Count = 10);
  AssertTrue(ms.ContainsAll(IntArray10));
  ms.AddAll(IntArray11);
  AssertTrue(ms.ContainsAll(IntArray21));
  ms.ArithmeticSubtract(ms1);
  AssertTrue(ms.Count = 10);
  AssertTrue(ms.ContainsAll(IntArray10));
  ms1.AddAll(IntArray10);
  ms.ArithmeticSubtract(ms1);
  AssertTrue(ms.IsEmpty);
end;

procedure TLiteHashMultisetLPTest.SymmetricSubtract;
var
  ms, ms1: TMultiSet;
begin
  ms1.AddAll(IntArray11);
  ms.SymmetricSubtract(ms1);
  AssertTrue(ms.Count = 11);
  AssertTrue(ms.ContainsAll(IntArray11));
  ms.AddAll(IntArray10);
  ms.SymmetricSubtract(ms1);
  AssertTrue(ms.Count = 10);
  AssertTrue(ms.ContainsAll(IntArray10));
  ms.AddAll(IntArray11);
  ms.AddAll(IntArray21);
  AssertTrue(ms.Count = 42);
  ms1.AddAll(IntArray10);
  ms.SymmetricSubtract(ms1);
  AssertTrue(ms.Count = 21);
  AssertTrue(ms.ContainsAll(IntArray21));
  ms.SymmetricSubtract(ms1);
  AssertTrue(ms.IsEmpty);
end;

procedure TLiteHashMultisetLPTest.PassByValue;
  procedure Test(aSet: TMultiSet);
  begin
    aSet.Add(5);
    aSet.Add(10);
    aSet.Add(15);
    AssertTrue(aSet.NonEmpty);
  end;
var
  ms: TMultiSet;
begin
  ms.EnsureCapacity(10);
  AssertTrue(ms.IsEmpty);
  Test(ms);
  AssertTrue(ms.IsEmpty);
  AssertTrue(ms.NonContains(5));
  AssertTrue(ms.NonContains(10));
  AssertTrue(ms.NonContains(15));
end;


{ TLiteChainHashMultisetTest }

function TLiteChainHashMultisetTest.IsEven(const aValue: Integer): Boolean;
begin
  Result := not Odd(aValue);
end;

procedure TLiteChainHashMultisetTest.Clear;
var
  ms: TMultiSet;
begin
  ms.AddAll(IntArray11);
  AssertTrue(ms.Count = 11);
  AssertTrue(ms.Capacity >= 11);
  ms.Clear;
  AssertTrue(ms.Count = 0);
  AssertTrue(ms.Capacity = 0);
end;

procedure TLiteChainHashMultisetTest.EnsureCapacity;
var
  ms: TMultiSet;
  c: SizeInt;
begin
  c := ms.ExpandTreshold;
  ms.EnsureCapacity(c + 100);
  AssertTrue(ms.ExpandTreshold >= c + 100);
end;

procedure TLiteChainHashMultisetTest.TrimToFit;
var
  ms: TMultiSet;
  c: Integer;
begin
  ms.AddAll(IntArray11);
  ms.EnsureCapacity(40);
  c := ms.Capacity;
  ms.TrimToFit;
  AssertTrue(ms.Capacity < c);
end;

procedure TLiteChainHashMultisetTest.Add;
var
  ms: TMultiSet;
begin
  AssertTrue(ms.Count = 0);
  ms.Add(1);
  AssertTrue(ms.Count = 1);
  AssertTrue(ms.Contains(1));
  ms.Add(51);
  AssertTrue(ms.Contains(51));
  AssertTrue(ms.Count = 2);
  AssertTrue(ms.EntryCount = 2);
  ms.Add(1);
  ms.Add(51);
  AssertTrue(ms.Count = 4);
  AssertTrue(ms.EntryCount = 2);
  AssertTrue(ms[1] = 2);
  AssertTrue(ms[51] = 2);
  ms.Add(52);
  AssertTrue(ms.Contains(52));
  AssertTrue(ms.Count = 5);
  AssertTrue(ms.EntryCount = 3);
  AssertTrue(ms[52] = 1);
end;

procedure TLiteChainHashMultisetTest.Add100;
var
  ms: TMultiSet;
  I: Integer;
begin
  for I := 1 to 100 do
    ms.Add(I);
  AssertTrue(ms.Count = 100);
  for I := 1 to 100 do
    ms.Add(I);
  AssertTrue(ms.Count = 200);
  AssertTrue(ms.EntryCount = 100);
  for I := 1 to 100 do
    AssertTrue(ms[I] = 2);
  ms[10] := 52;
  AssertTrue(ms.Count = 250);
  ms[10] := 12;
  AssertTrue(ms.Count = 210);
end;

procedure TLiteChainHashMultisetTest.AddArray;
var
  ms: TMultiSet;
  I: Integer;
begin
  AssertTrue(ms.AddAll(IntArray21) = Length(IntArray21));
  AssertTrue(ms.Count = Length(IntArray21));
  AssertTrue(ms.EntryCount = Length(IntArray21));
  AssertTrue(ms.ContainsAll(IntArray21));
  AssertTrue(ms.AddAll(IntArray21) = Length(IntArray21));
  AssertTrue(ms.Count = Length(IntArray21) * 2);
  AssertTrue(ms.EntryCount = Length(IntArray21));
  for I in IntArray21 do
    AssertTrue(ms[I] = 2);
end;

procedure TLiteChainHashMultisetTest.AddEnum;
var
  ms: TMultiSet;
  I: Integer;
begin
  AssertTrue(ms.AddAll(TIntArrayCursor.Create(TIntHelper.CreateCopy(IntArray21))) = Length(IntArray21));
  AssertTrue(ms.Count = Length(IntArray21));
  AssertTrue(ms.EntryCount = Length(IntArray21));
  AssertTrue(ms.ContainsAll(IntArray21));
  AssertTrue(ms.AddAll(TIntArrayCursor.Create(TIntHelper.CreateCopy(IntArray21))) = Length(IntArray21));
  AssertTrue(ms.Count = Length(IntArray21) * 2);
  AssertTrue(ms.EntryCount = Length(IntArray21));
  for I in IntArray21 do
    AssertTrue(ms[I] = 2);
end;

procedure TLiteChainHashMultisetTest.AddSelf;
var
  ms: TMultiSet;
  I: Integer;
begin
  ms.AddAll(IntArray21);
  AssertTrue(ms.AddAll(ms) = Length(IntArray21));
  AssertTrue(ms.Count = Length(IntArray21) * 2);
  for I in IntArray21 do
    AssertTrue(ms[I] = 2);
end;

procedure TLiteChainHashMultisetTest.Remove;
var
  ms: TMultiSet;
  Len, Len2: SizeInt;
begin
  Len := Length(IntArray21);
  Len2 := Len * 2;
  ms.AddAll(IntArray21);
  ms.AddAll(IntArray21);
  AssertTrue(ms.Count = Len2);
  AssertTrue(ms.EntryCount = Len);
  AssertTrue(ms.Remove(IntArray21[1]));
  AssertTrue(ms.Count = Len2 - 1);
  AssertTrue(ms.EntryCount = Len);
  AssertTrue(ms.Remove(IntArray21[High(IntArray21)]));
  AssertTrue(ms.Count = Len2 - 2);
  AssertTrue(ms.EntryCount = Len);
  AssertTrue(ms.Remove(IntArray21[1]));
  AssertTrue(ms.Count = Len2 - 3);
  AssertTrue(ms.EntryCount = Len - 1);
  AssertFalse(ms.Remove(-1));
  AssertTrue(ms.Count = Len2 - 3);
  AssertFalse(ms.Remove(100));
  AssertTrue(ms.Count = Len2 - 3);
end;

procedure TLiteChainHashMultisetTest.RemoveArray;
var
  ms: TMultiSet;
begin
  ms.AddAll(IntArray21);
  ms.AddAll(IntArray21);
  AssertTrue(ms.Count = 42);
  AssertTrue(ms.RemoveAll(IntArray11) = 11);
  AssertTrue(ms.Count = 31);
  AssertTrue(ms.EntryCount = 21);
  AssertTrue(ms.RemoveAll(IntArray10) = 10);
  AssertTrue(ms.Count = 21);
  AssertTrue(ms.EntryCount = 21);
end;

procedure TLiteChainHashMultisetTest.RemoveEnum;
var
  ms: TMultiSet;
begin
  ms.AddAll(IntArray21);
  ms.AddAll(IntArray21);
  AssertTrue(ms.Count = 42);
  AssertTrue(ms.RemoveAll(TIntArrayCursor.Create(TIntHelper.CreateCopy(IntArray11))) = 11);
  AssertTrue(ms.Count = 31);
  AssertTrue(ms.EntryCount = 21);
  AssertTrue(ms.RemoveAll(TIntArrayCursor.Create(TIntHelper.CreateCopy(IntArray10))) = 10);
  AssertTrue(ms.Count = 21);
  AssertTrue(ms.EntryCount = 21);
end;

procedure TLiteChainHashMultisetTest.RemoveSelf;
var
  ms: TMultiSet;
begin
  ms.AddAll(IntArray21);
  ms.AddAll(IntArray21);
  AssertTrue(ms.RemoveAll(ms) = 42);
  AssertTrue(ms.IsEmpty);
end;

procedure TLiteChainHashMultisetTest.RemoveIfRegular;
var
  ms: TMultiSet;
  I: Integer;
begin
  ms.AddAll(IntArray21);
  ms.AddAll(IntArray21);
  AssertTrue(ms.RemoveIf(@IsEvenInt) = 20);
  AssertTrue(ms.Count = 22);
  AssertTrue(ms.ContainsAll(IntArray11));
  for I in IntArray11 do
    AssertTrue(ms[I] = 2);
end;

procedure TLiteChainHashMultisetTest.RemoveIfDelegated;
var
  ms: TMultiSet;
  I: Integer;
begin
  ms.AddAll(IntArray21);
  ms.AddAll(IntArray21);
  AssertTrue(ms.RemoveIf(@IsEven) = 20);
  AssertTrue(ms.Count = 22);
  AssertTrue(ms.ContainsAll(IntArray11));
  for I in IntArray11 do
    AssertTrue(ms[I] = 2);
end;

procedure TLiteChainHashMultisetTest.RemoveIfNested;
  function IsEvenInt(const aValue: Integer): Boolean;
  begin
    Result := not Odd(aValue);
  end;
var
  ms: TMultiSet;
  I: Integer;
begin
  ms.AddAll(IntArray21);
  ms.AddAll(IntArray21);
  AssertTrue(ms.RemoveIf(@IsEvenInt) = 20);
  AssertTrue(ms.Count = 22);
  AssertTrue(ms.ContainsAll(IntArray11));
  for I in IntArray11 do
    AssertTrue(ms[I] = 2);
end;

procedure TLiteChainHashMultisetTest.ExtractIfRegular;
var
  ms, ms1: TMultiSet;
  e: TIntArray;
  I: Integer;
begin
  ms.AddAll(IntArray21);
  ms.AddAll(IntArray21);
  e := ms.ExtractIf(@IsEvenInt);
  AssertTrue(ms.Count = 22);
  AssertTrue(ms.ContainsAll(IntArray11));
  for I in IntArray11 do
    AssertTrue(ms[I] = 2);
  AssertTrue(Length(e) = 20);
  ms1.AddAll(e);
  for I in IntArray10 do
    AssertTrue(ms1[I] = 2);
end;

procedure TLiteChainHashMultisetTest.ExtractIfDelegated;
var
  ms, ms1: TMultiSet;
  e: TIntArray;
  I: Integer;
begin
  ms.AddAll(IntArray21);
  ms.AddAll(IntArray21);
  e := ms.ExtractIf(@IsEven);
  AssertTrue(ms.Count = 22);
  AssertTrue(ms.ContainsAll(IntArray11));
  for I in IntArray11 do
    AssertTrue(ms[I] = 2);
  AssertTrue(Length(e) = 20);
  ms1.AddAll(e);
  for I in IntArray10 do
    AssertTrue(ms1[I] = 2);
end;

procedure TLiteChainHashMultisetTest.ExtractIfNested;
var
  ms, ms1: TMultiSet;
  e: TIntArray;
  I: Integer;
  function IsEvenInt(const aValue: Integer): Boolean;
  begin
    Result := not Odd(aValue);
  end;
begin
  ms.AddAll(IntArray21);
  ms.AddAll(IntArray21);
  e := ms.ExtractIf(@IsEvenInt);
  AssertTrue(ms.Count = 22);
  AssertTrue(ms.ContainsAll(IntArray11));
  for I in IntArray11 do
    AssertTrue(ms[I] = 2);
  AssertTrue(Length(e) = 20);
  ms1.AddAll(e);
  for I in IntArray10 do
    AssertTrue(ms1[I] = 2);
end;

procedure TLiteChainHashMultisetTest.Retain;
var
  ms: TMultiSet;
  s: specialize TGHashMultiSetLP<Integer>;
  I: Integer;
begin
  ms.AddAll(IntArray21);
  ms.AddAll(IntArray21);
  AssertTrue(ms.Count = 42);
  s := specialize TGHashMultiSetLP<Integer>.Create(IntArray11);
  try
    ms.RetainAll(s);
  finally
    s.Free;
  end;
  AssertTrue(ms.Count = 22);
  AssertTrue(ms.ContainsAll(IntArray11));
  for I in IntArray11 do
    AssertTrue(ms[I] = 2);
end;

procedure TLiteChainHashMultisetTest.Retain_1;
var
  ms: TMultiSet;
  s: specialize TGHashMultiSetLP<Integer>;
begin
  ms.AddAll(IntArray10);
  ms.AddAll(IntArray10);
  AssertTrue(ms.Count = 20);
  s := specialize TGHashMultiSetLP<Integer>.Create(IntArray11);
  try
    ms.RetainAll(s);
  finally
    s.Free;
  end;
  AssertTrue(ms.IsEmpty);
end;

procedure TLiteChainHashMultisetTest.IsSuperset;
var
  ms, ms1: TMultiSet;
begin
  ms.AddAll(IntArray21);
  ms.AddAll(IntArray21);
  ms1.AddAll(ms);
  AssertTrue(ms.IsSuperSet(ms1));
  AssertTrue(ms1.IsSuperSet(ms));
  AssertTrue(ms.IsSuperSet(ms));
  ms1.Remove(IntArray21[1]);
  AssertTrue(ms.IsSuperSet(ms1));
  AssertFalse(ms1.IsSuperSet(ms));
  ms1.Clear;
  AssertTrue(ms.IsSuperSet(ms1));
  AssertFalse(ms1.IsSuperSet(ms));
  ms.Clear;
  AssertTrue(ms.IsSuperSet(ms1));
  AssertTrue(ms1.IsSuperSet(ms));
end;

procedure TLiteChainHashMultisetTest.IsSubset;
var
  ms, ms1: TMultiSet;
begin
  ms.AddAll(IntArray21);
  ms.AddAll(IntArray21);
  ms1.AddAll(ms);
  AssertTrue(ms.IsSubSet(ms1));
  AssertTrue(ms1.IsSubSet(ms));
  AssertTrue(ms.IsSubSet(ms));
  ms1.Remove(IntArray21[1]);
  AssertFalse(ms.IsSubSet(ms1));
  AssertTrue(ms1.IsSubSet(ms));
  ms1.Clear;
  AssertFalse(ms.IsSubSet(ms1));
  AssertTrue(ms1.IsSubSet(ms));
  ms.Clear;
  AssertTrue(ms1.IsSubSet(ms));
  AssertTrue(ms.IsSubSet(ms1));
end;

procedure TLiteChainHashMultisetTest.IsEqual;
var
  ms, ms1: TMultiSet;
begin
  ms.AddAll(IntArray21);
  ms.AddAll(IntArray21);
  ms1.AddAll(ms);
  AssertTrue(ms.IsEqual(ms1));
  AssertTrue(ms1.IsEqual(ms));
  AssertTrue(ms.IsEqual(ms));
  ms1.Remove(IntArray21[1]);
  AssertFalse(ms.IsEqual(ms1));
  AssertFalse(ms1.IsEqual(ms));
  ms.Remove(IntArray21[1]);
  AssertTrue(ms1.IsEqual(ms));
  AssertTrue(ms.IsEqual(ms));
  ms1.Clear;
  ms.Clear;
  AssertTrue(ms.IsEqual(ms1));
  AssertTrue(ms1.IsEqual(ms));
end;

procedure TLiteChainHashMultisetTest.Intersecting;
var
  ms, ms1: TMultiSet;
begin
  ms.AddAll(IntArray21);
  ms1.AddAll(IntArray11);
  AssertTrue(ms.Intersecting(ms));
  AssertTrue(ms.Intersecting(ms1));
  ms.RemoveAll(IntArray11);
  AssertFalse(ms.Intersecting(ms1));
  AssertFalse(ms1.Intersecting(ms));
end;

procedure TLiteChainHashMultisetTest.Intersect;
var
  ms, ms1: TMultiSet;
  I: Integer;
begin
  ms.AddAll(IntArray21);
  ms.AddAll(IntArray21);
  AssertTrue(ms.Count = 42);
  ms1.AddAll(IntArray11);
  ms1.AddAll([-4, -3, -2, -1, 0]);
  ms.Intersect(ms1);
  AssertTrue(ms.Count = 11);
  for I in IntArray11 do
    AssertTrue(ms[I] = 1);
end;

procedure TLiteChainHashMultisetTest.Join;
var
  ms, ms1: TMultiSet;
  I: Integer;
begin
  ms1.AddAll(IntArray11);
  ms.Join(ms1);
  AssertTrue(ms.IsEqual(ms1));
  ms1.Clear;
  ms1.AddAll(IntArray10);
  ms1.AddAll(ms1);
  ms.Join(ms1);
  for I in IntArray11 do
    AssertTrue(ms[I] = 1);
  for I in IntArray10 do
    AssertTrue(ms[I] = 2);
end;

procedure TLiteChainHashMultisetTest.ArithmeticAdd;
var
  ms, ms1: TMultiSet;
  I: Integer;
begin
  ms1.AddAll(IntArray11);
  ms.ArithmeticAdd(ms1);
  AssertTrue(ms.IsEqual(ms1));
  ms.AddAll(IntArray10);
  ms.ArithmeticAdd(ms1);
  for I in IntArray11 do
    AssertTrue(ms[I] = 2);
  for I in IntArray10 do
    AssertTrue(ms[I] = 1);
  ms.ArithmeticAdd(ms);
  for I in IntArray11 do
    AssertTrue(ms[I] = 4);
  for I in IntArray10 do
    AssertTrue(ms[I] = 2);
end;

procedure TLiteChainHashMultisetTest.ArithmeticSubtract;
var
  ms, ms1: TMultiSet;
begin
  ms1.AddAll(IntArray11);
  ms.ArithmeticSubtract(ms1);
  AssertTrue(ms.IsEmpty);
  ms.AddAll(IntArray10);
  ms.ArithmeticSubtract(ms1);
  AssertTrue(ms.Count = 10);
  AssertTrue(ms.ContainsAll(IntArray10));
  ms.AddAll(IntArray11);
  AssertTrue(ms.ContainsAll(IntArray21));
  ms.ArithmeticSubtract(ms1);
  AssertTrue(ms.Count = 10);
  AssertTrue(ms.ContainsAll(IntArray10));
  ms1.AddAll(IntArray10);
  ms.ArithmeticSubtract(ms1);
  AssertTrue(ms.IsEmpty);
end;

procedure TLiteChainHashMultisetTest.SymmetricSubtract;
var
  ms, ms1: TMultiSet;
begin
  ms1.AddAll(IntArray11);
  ms.SymmetricSubtract(ms1);
  AssertTrue(ms.Count = 11);
  AssertTrue(ms.ContainsAll(IntArray11));
  ms.AddAll(IntArray10);
  ms.SymmetricSubtract(ms1);
  AssertTrue(ms.Count = 10);
  AssertTrue(ms.ContainsAll(IntArray10));
  ms.AddAll(IntArray11);
  ms.AddAll(IntArray21);
  AssertTrue(ms.Count = 42);
  ms1.AddAll(IntArray10);
  ms.SymmetricSubtract(ms1);
  AssertTrue(ms.Count = 21);
  AssertTrue(ms.ContainsAll(IntArray21));
  ms.SymmetricSubtract(ms1);
  AssertTrue(ms.IsEmpty);
end;

procedure TLiteChainHashMultisetTest.PassByValue;
  procedure Test(aSet: TMultiSet);
  begin
    aSet.Add(5);
    aSet.Add(10);
    aSet.Add(15);
    AssertTrue(aSet.NonEmpty);
  end;
var
  ms: TMultiSet;
begin
  ms.EnsureCapacity(10);
  AssertTrue(ms.IsEmpty);
  Test(ms);
  AssertTrue(ms.IsEmpty);
  AssertTrue(ms.NonContains(5));
  AssertTrue(ms.NonContains(10));
  AssertTrue(ms.NonContains(15));
end;


{ TLiteEquatableHashMultisetTest }

function TLiteEquatableHashMultisetTest.IsEven(const aValue: Integer): Boolean;
begin
  Result := not Odd(aValue);
end;

procedure TLiteEquatableHashMultisetTest.Clear;
var
  ms: TMultiSet;
begin
  ms.AddAll(IntArray11);
  AssertTrue(ms.Count = 11);
  AssertTrue(ms.Capacity >= 11);
  ms.Clear;
  AssertTrue(ms.Count = 0);
  AssertTrue(ms.Capacity = 0);
end;

procedure TLiteEquatableHashMultisetTest.EnsureCapacity;
var
  ms: TMultiSet;
  c: SizeInt;
begin
  c := ms.ExpandTreshold;
  ms.EnsureCapacity(c + 100);
  AssertTrue(ms.ExpandTreshold >= c + 100);
end;

procedure TLiteEquatableHashMultisetTest.TrimToFit;
var
  ms: TMultiSet;
begin
  ms.AddAll(IntArray11);
  ms.EnsureCapacity(50);
  AssertTrue(ms.Capacity = 128);
  ms.TrimToFit;
  AssertTrue(ms.Capacity = 32);
end;

procedure TLiteEquatableHashMultisetTest.Add;
var
  ms: TMultiSet;
begin
  AssertTrue(ms.Count = 0);
  ms.Add(1);
  AssertTrue(ms.Count = 1);
  AssertTrue(ms.Contains(1));
  ms.Add(51);
  AssertTrue(ms.Contains(51));
  AssertTrue(ms.Count = 2);
  AssertTrue(ms.EntryCount = 2);
  ms.Add(1);
  ms.Add(51);
  AssertTrue(ms.Count = 4);
  AssertTrue(ms.EntryCount = 2);
  AssertTrue(ms[1] = 2);
  AssertTrue(ms[51] = 2);
  ms.Add(52);
  AssertTrue(ms.Contains(52));
  AssertTrue(ms.Count = 5);
  AssertTrue(ms.EntryCount = 3);
  AssertTrue(ms[52] = 1);
end;

procedure TLiteEquatableHashMultisetTest.Add100;
var
  ms: TMultiSet;
  I: Integer;
begin
  for I := 1 to 100 do
    ms.Add(I);
  AssertTrue(ms.Count = 100);
  for I := 1 to 100 do
    ms.Add(I);
  AssertTrue(ms.Count = 200);
  AssertTrue(ms.EntryCount = 100);
  for I := 1 to 100 do
    AssertTrue(ms[I] = 2);
  ms[10] := 52;
  AssertTrue(ms.Count = 250);
  ms[10] := 12;
  AssertTrue(ms.Count = 210);
end;

procedure TLiteEquatableHashMultisetTest.AddArray;
var
  ms: TMultiSet;
  I: Integer;
begin
  AssertTrue(ms.AddAll(IntArray21) = Length(IntArray21));
  AssertTrue(ms.Count = Length(IntArray21));
  AssertTrue(ms.EntryCount = Length(IntArray21));
  AssertTrue(ms.ContainsAll(IntArray21));
  AssertTrue(ms.AddAll(IntArray21) = Length(IntArray21));
  AssertTrue(ms.Count = Length(IntArray21) * 2);
  AssertTrue(ms.EntryCount = Length(IntArray21));
  for I in IntArray21 do
    AssertTrue(ms[I] = 2);
end;

procedure TLiteEquatableHashMultisetTest.AddEnum;
var
  ms: TMultiSet;
  I: Integer;
begin
  AssertTrue(ms.AddAll(TIntArrayCursor.Create(TIntHelper.CreateCopy(IntArray21))) = Length(IntArray21));
  AssertTrue(ms.Count = Length(IntArray21));
  AssertTrue(ms.EntryCount = Length(IntArray21));
  AssertTrue(ms.ContainsAll(IntArray21));
  AssertTrue(ms.AddAll(TIntArrayCursor.Create(TIntHelper.CreateCopy(IntArray21))) = Length(IntArray21));
  AssertTrue(ms.Count = Length(IntArray21) * 2);
  AssertTrue(ms.EntryCount = Length(IntArray21));
  for I in IntArray21 do
    AssertTrue(ms[I] = 2);
end;

procedure TLiteEquatableHashMultisetTest.AddSelf;
var
  ms: TMultiSet;
  I: Integer;
begin
  ms.AddAll(IntArray21);
  AssertTrue(ms.AddAll(ms) = Length(IntArray21));
  AssertTrue(ms.Count = Length(IntArray21) * 2);
  for I in IntArray21 do
    AssertTrue(ms[I] = 2);
end;

procedure TLiteEquatableHashMultisetTest.Remove;
var
  ms: TMultiSet;
  Len, Len2: SizeInt;
begin
  Len := Length(IntArray21);
  Len2 := Len * 2;
  ms.AddAll(IntArray21);
  ms.AddAll(IntArray21);
  AssertTrue(ms.Count = Len2);
  AssertTrue(ms.EntryCount = Len);
  AssertTrue(ms.Remove(IntArray21[1]));
  AssertTrue(ms.Count = Len2 - 1);
  AssertTrue(ms.EntryCount = Len);
  AssertTrue(ms.Remove(IntArray21[High(IntArray21)]));
  AssertTrue(ms.Count = Len2 - 2);
  AssertTrue(ms.EntryCount = Len);
  AssertTrue(ms.Remove(IntArray21[1]));
  AssertTrue(ms.Count = Len2 - 3);
  AssertTrue(ms.EntryCount = Len - 1);
  AssertFalse(ms.Remove(-1));
  AssertTrue(ms.Count = Len2 - 3);
  AssertFalse(ms.Remove(100));
  AssertTrue(ms.Count = Len2 - 3);
end;

procedure TLiteEquatableHashMultisetTest.RemoveArray;
var
  ms: TMultiSet;
begin
  ms.AddAll(IntArray21);
  ms.AddAll(IntArray21);
  AssertTrue(ms.Count = 42);
  AssertTrue(ms.RemoveAll(IntArray11) = 11);
  AssertTrue(ms.Count = 31);
  AssertTrue(ms.EntryCount = 21);
  AssertTrue(ms.RemoveAll(IntArray10) = 10);
  AssertTrue(ms.Count = 21);
  AssertTrue(ms.EntryCount = 21);
end;

procedure TLiteEquatableHashMultisetTest.RemoveEnum;
var
  ms: TMultiSet;
begin
  ms.AddAll(IntArray21);
  ms.AddAll(IntArray21);
  AssertTrue(ms.Count = 42);
  AssertTrue(ms.RemoveAll(TIntArrayCursor.Create(TIntHelper.CreateCopy(IntArray11))) = 11);
  AssertTrue(ms.Count = 31);
  AssertTrue(ms.EntryCount = 21);
  AssertTrue(ms.RemoveAll(TIntArrayCursor.Create(TIntHelper.CreateCopy(IntArray10))) = 10);
  AssertTrue(ms.Count = 21);
  AssertTrue(ms.EntryCount = 21);
end;

procedure TLiteEquatableHashMultisetTest.RemoveSelf;
var
  ms: TMultiSet;
begin
  ms.AddAll(IntArray21);
  ms.AddAll(IntArray21);
  AssertTrue(ms.RemoveAll(ms) = 42);
  AssertTrue(ms.IsEmpty);
end;

procedure TLiteEquatableHashMultisetTest.RemoveIfRegular;
var
  ms: TMultiSet;
  I: Integer;
begin
  ms.AddAll(IntArray21);
  ms.AddAll(IntArray21);
  AssertTrue(ms.RemoveIf(@IsEvenInt) = 20);
  AssertTrue(ms.Count = 22);
  AssertTrue(ms.ContainsAll(IntArray11));
  for I in IntArray11 do
    AssertTrue(ms[I] = 2);
end;

procedure TLiteEquatableHashMultisetTest.RemoveIfDelegated;
var
  ms: TMultiSet;
  I: Integer;
begin
  ms.AddAll(IntArray21);
  ms.AddAll(IntArray21);
  AssertTrue(ms.RemoveIf(@IsEven) = 20);
  AssertTrue(ms.Count = 22);
  AssertTrue(ms.ContainsAll(IntArray11));
  for I in IntArray11 do
    AssertTrue(ms[I] = 2);
end;

procedure TLiteEquatableHashMultisetTest.RemoveIfNested;
  function IsEvenInt(const aValue: Integer): Boolean;
  begin
    Result := not Odd(aValue);
  end;
var
  ms: TMultiSet;
  I: Integer;
begin
  ms.AddAll(IntArray21);
  ms.AddAll(IntArray21);
  AssertTrue(ms.RemoveIf(@IsEvenInt) = 20);
  AssertTrue(ms.Count = 22);
  AssertTrue(ms.ContainsAll(IntArray11));
  for I in IntArray11 do
    AssertTrue(ms[I] = 2);
end;

procedure TLiteEquatableHashMultisetTest.ExtractIfRegular;
var
  ms, ms1: TMultiSet;
  e: TIntArray;
  I: Integer;
begin
  ms.AddAll(IntArray21);
  ms.AddAll(IntArray21);
  e := ms.ExtractIf(@IsEvenInt);
  AssertTrue(ms.Count = 22);
  AssertTrue(ms.ContainsAll(IntArray11));
  for I in IntArray11 do
    AssertTrue(ms[I] = 2);
  AssertTrue(Length(e) = 20);
  ms1.AddAll(e);
  for I in IntArray10 do
    AssertTrue(ms1[I] = 2);
end;

procedure TLiteEquatableHashMultisetTest.ExtractIfDelegated;
var
  ms, ms1: TMultiSet;
  e: TIntArray;
  I: Integer;
begin
  ms.AddAll(IntArray21);
  ms.AddAll(IntArray21);
  e := ms.ExtractIf(@IsEven);
  AssertTrue(ms.Count = 22);
  AssertTrue(ms.ContainsAll(IntArray11));
  for I in IntArray11 do
    AssertTrue(ms[I] = 2);
  AssertTrue(Length(e) = 20);
  ms1.AddAll(e);
  for I in IntArray10 do
    AssertTrue(ms1[I] = 2);
end;

procedure TLiteEquatableHashMultisetTest.ExtractIfNested;
var
  ms, ms1: TMultiSet;
  e: TIntArray;
  I: Integer;
  function IsEvenInt(const aValue: Integer): Boolean;
  begin
    Result := not Odd(aValue);
  end;
begin
  ms.AddAll(IntArray21);
  ms.AddAll(IntArray21);
  e := ms.ExtractIf(@IsEvenInt);
  AssertTrue(ms.Count = 22);
  AssertTrue(ms.ContainsAll(IntArray11));
  for I in IntArray11 do
    AssertTrue(ms[I] = 2);
  AssertTrue(Length(e) = 20);
  ms1.AddAll(e);
  for I in IntArray10 do
    AssertTrue(ms1[I] = 2);
end;

procedure TLiteEquatableHashMultisetTest.Retain;
var
  ms: TMultiSet;
  s: specialize TGHashMultiSetLP<Integer>;
  I: Integer;
begin
  ms.AddAll(IntArray21);
  ms.AddAll(IntArray21);
  AssertTrue(ms.Count = 42);
  s := specialize TGHashMultiSetLP<Integer>.Create(IntArray11);
  try
    ms.RetainAll(s);
  finally
    s.Free;
  end;
  AssertTrue(ms.Count = 22);
  AssertTrue(ms.ContainsAll(IntArray11));
  for I in IntArray11 do
    AssertTrue(ms[I] = 2);
end;

procedure TLiteEquatableHashMultisetTest.Retain_1;
var
  ms: TMultiSet;
  s: specialize TGHashMultiSetLP<Integer>;
begin
  ms.AddAll(IntArray10);
  ms.AddAll(IntArray10);
  AssertTrue(ms.Count = 20);
  s := specialize TGHashMultiSetLP<Integer>.Create(IntArray11);
  try
    ms.RetainAll(s);
  finally
    s.Free;
  end;
  AssertTrue(ms.IsEmpty);
end;

procedure TLiteEquatableHashMultisetTest.IsSuperset;
var
  ms, ms1: TMultiSet;
begin
  ms.AddAll(IntArray21);
  ms.AddAll(IntArray21);
  ms1.AddAll(ms);
  AssertTrue(ms.IsSuperSet(ms1));
  AssertTrue(ms1.IsSuperSet(ms));
  AssertTrue(ms.IsSuperSet(ms));
  ms1.Remove(IntArray21[1]);
  AssertTrue(ms.IsSuperSet(ms1));
  AssertFalse(ms1.IsSuperSet(ms));
  ms1.Clear;
  AssertTrue(ms.IsSuperSet(ms1));
  AssertFalse(ms1.IsSuperSet(ms));
  ms.Clear;
  AssertTrue(ms.IsSuperSet(ms1));
  AssertTrue(ms1.IsSuperSet(ms));
end;

procedure TLiteEquatableHashMultisetTest.IsSubset;
var
  ms, ms1: TMultiSet;
begin
  ms.AddAll(IntArray21);
  ms.AddAll(IntArray21);
  ms1.AddAll(ms);
  AssertTrue(ms.IsSubSet(ms1));
  AssertTrue(ms1.IsSubSet(ms));
  AssertTrue(ms.IsSubSet(ms));
  ms1.Remove(IntArray21[1]);
  AssertFalse(ms.IsSubSet(ms1));
  AssertTrue(ms1.IsSubSet(ms));
  ms1.Clear;
  AssertFalse(ms.IsSubSet(ms1));
  AssertTrue(ms1.IsSubSet(ms));
  ms.Clear;
  AssertTrue(ms1.IsSubSet(ms));
  AssertTrue(ms.IsSubSet(ms1));
end;

procedure TLiteEquatableHashMultisetTest.IsEqual;
var
  ms, ms1: TMultiSet;
begin
  ms.AddAll(IntArray21);
  ms.AddAll(IntArray21);
  ms1.AddAll(ms);
  AssertTrue(ms.IsEqual(ms1));
  AssertTrue(ms1.IsEqual(ms));
  AssertTrue(ms.IsEqual(ms));
  ms1.Remove(IntArray21[1]);
  AssertFalse(ms.IsEqual(ms1));
  AssertFalse(ms1.IsEqual(ms));
  ms.Remove(IntArray21[1]);
  AssertTrue(ms1.IsEqual(ms));
  AssertTrue(ms.IsEqual(ms));
  ms1.Clear;
  ms.Clear;
  AssertTrue(ms.IsEqual(ms1));
  AssertTrue(ms1.IsEqual(ms));
end;

procedure TLiteEquatableHashMultisetTest.Intersecting;
var
  ms, ms1: TMultiSet;
begin
  ms.AddAll(IntArray21);
  ms1.AddAll(IntArray11);
  AssertTrue(ms.Intersecting(ms));
  AssertTrue(ms.Intersecting(ms1));
  ms.RemoveAll(IntArray11);
  AssertFalse(ms.Intersecting(ms1));
  AssertFalse(ms1.Intersecting(ms));
end;

procedure TLiteEquatableHashMultisetTest.Intersect;
var
  ms, ms1: TMultiSet;
  I: Integer;
begin
  ms.AddAll(IntArray21);
  ms.AddAll(IntArray21);
  AssertTrue(ms.Count = 42);
  ms1.AddAll(IntArray11);
  ms1.AddAll([-4, -3, -2, -1, 0]);
  ms.Intersect(ms1);
  AssertTrue(ms.Count = 11);
  for I in IntArray11 do
    AssertTrue(ms[I] = 1);
end;

procedure TLiteEquatableHashMultisetTest.Join;
var
  ms, ms1: TMultiSet;
  I: Integer;
begin
  ms1.AddAll(IntArray11);
  ms.Join(ms1);
  AssertTrue(ms.IsEqual(ms1));
  ms1.Clear;
  ms1.AddAll(IntArray10);
  ms1.AddAll(ms1);
  ms.Join(ms1);
  for I in IntArray11 do
    AssertTrue(ms[I] = 1);
  for I in IntArray10 do
    AssertTrue(ms[I] = 2);
end;

procedure TLiteEquatableHashMultisetTest.ArithmeticAdd;
var
  ms, ms1: TMultiSet;
  I: Integer;
begin
  ms1.AddAll(IntArray11);
  ms.ArithmeticAdd(ms1);
  AssertTrue(ms.IsEqual(ms1));
  ms.AddAll(IntArray10);
  ms.ArithmeticAdd(ms1);
  for I in IntArray11 do
    AssertTrue(ms[I] = 2);
  for I in IntArray10 do
    AssertTrue(ms[I] = 1);
  ms.ArithmeticAdd(ms);
  for I in IntArray11 do
    AssertTrue(ms[I] = 4);
  for I in IntArray10 do
    AssertTrue(ms[I] = 2);
end;

procedure TLiteEquatableHashMultisetTest.ArithmeticSubtract;
var
  ms, ms1: TMultiSet;
begin
  ms1.AddAll(IntArray11);
  ms.ArithmeticSubtract(ms1);
  AssertTrue(ms.IsEmpty);
  ms.AddAll(IntArray10);
  ms.ArithmeticSubtract(ms1);
  AssertTrue(ms.Count = 10);
  AssertTrue(ms.ContainsAll(IntArray10));
  ms.AddAll(IntArray11);
  AssertTrue(ms.ContainsAll(IntArray21));
  ms.ArithmeticSubtract(ms1);
  AssertTrue(ms.Count = 10);
  AssertTrue(ms.ContainsAll(IntArray10));
  ms1.AddAll(IntArray10);
  ms.ArithmeticSubtract(ms1);
  AssertTrue(ms.IsEmpty);
end;

procedure TLiteEquatableHashMultisetTest.SymmetricSubtract;
var
  ms, ms1: TMultiSet;
begin
  ms1.AddAll(IntArray11);
  ms.SymmetricSubtract(ms1);
  AssertTrue(ms.Count = 11);
  AssertTrue(ms.ContainsAll(IntArray11));
  ms.AddAll(IntArray10);
  ms.SymmetricSubtract(ms1);
  AssertTrue(ms.Count = 10);
  AssertTrue(ms.ContainsAll(IntArray10));
  ms.AddAll(IntArray11);
  ms.AddAll(IntArray21);
  AssertTrue(ms.Count = 42);
  ms1.AddAll(IntArray10);
  ms.SymmetricSubtract(ms1);
  AssertTrue(ms.Count = 21);
  AssertTrue(ms.ContainsAll(IntArray21));
  ms.SymmetricSubtract(ms1);
  AssertTrue(ms.IsEmpty);
end;

procedure TLiteEquatableHashMultisetTest.PassByValue;
  procedure Test(aSet: TMultiSet);
  begin
    aSet.Add(5);
    aSet.Add(10);
    aSet.Add(15);
    AssertTrue(aSet.NonEmpty);
  end;
var
  ms: TMultiSet;
begin
  ms.EnsureCapacity(10);
  AssertTrue(ms.IsEmpty);
  Test(ms);
  AssertTrue(ms.IsEmpty);
  AssertTrue(ms.NonContains(5));
  AssertTrue(ms.NonContains(10));
  AssertTrue(ms.NonContains(15));
end;

{ TThreadFGHashMultiSetTest.TWorker }

constructor TThreadFGHashMultiSetTest.TWorker.Create(aSet: TSet; const aData: TArray; aIndex: Integer; aFlagValue,
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

{ TThreadFGHashMultiSetTest.TAdder }

procedure TThreadFGHashMultiSetTest.TAdder.Execute;
var
  I: Integer;
  Added: Integer = 0;
begin
  for I in FData do
    begin
      FSet.Add(I);
      Inc(Added);
    end;
  FResult[FIndex] := Added;
  InterlockedIncrement(FFlag^);
  Terminate;
end;

{ TThreadFGHashMultiSetTest.TRemover }

procedure TThreadFGHashMultiSetTest.TRemover.Execute;
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

{ TThreadFGHashMultiSetTest }

procedure TThreadFGHashMultiSetTest.SimpleAdd;
var
  ref: TSetRef;
  I: Integer;
begin
  for I := 1 to 100 do
    ref.Instance.Add(I);
  AssertTrue(ref.Instance.Count = 100);
  AssertTrue(ref.Instance.EntryCount = 100);
  for I := 1 to 100 do
    ref.Instance.Add(I);
  AssertTrue(ref.Instance.Count = 200);
  AssertTrue(ref.Instance.EntryCount = 100);
  for I := 1 to 100 do
    ref.Instance.Add(I);
  AssertTrue(ref.Instance.Count = 300);
  AssertTrue(ref.Instance.EntryCount = 100);
  for I := 1 to 100 do
    AssertTrue(ref.Instance.Contains(I));
  for I := 1 to 100 do
    AssertTrue(ref.Instance[I] = 3);
end;

procedure TThreadFGHashMultiSetTest.SimpleRemove;
var
  ref: TSetRef;
  I, J: Integer;
begin
  for I := 1 to 3 do
    for J := 1 to 100 do
      ref.Instance.Add(J);
  AssertTrue(ref.Instance.Count = 300);
  AssertTrue(ref.Instance.EntryCount = 100);
  for I := 1 to 100 do
    AssertTrue(ref.Instance.Remove(I));
  AssertTrue(ref.Instance.Count = 200);
  AssertTrue(ref.Instance.EntryCount = 100);
  for I := 1 to 100 do
    AssertTrue(ref.Instance.Remove(I));
  AssertTrue(ref.Instance.Count = 100);
  AssertTrue(ref.Instance.EntryCount = 100);
  for I := 1 to 100 do
    AssertTrue(ref.Instance.Remove(I));
  AssertTrue(ref.Instance.Count = 0);
  AssertTrue(ref.Instance.EntryCount = 0);
end;

procedure TThreadFGHashMultiSetTest.Add;
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
  AssertTrue(s.Count = TestSize * ThreadCount);
  AssertTrue(s.EntryCount = TestSize);
  s.Free;
  Total := 0;
  for I in Results do
    Total += I;
  AssertTrue(Total = TestSize * ThreadCount);
end;

procedure TThreadFGHashMultiSetTest.Remove;
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
  AssertTrue(s.EntryCount = 0);
  s.Free;
  Total := 0;
  for I in Results do
    Total += I;
  AssertTrue(Total = TestSize * 2);
end;

initialization
  RegisterTest(THashMultisetLPTest);
  RegisterTest(THashMultisetLPTTest);
  RegisterTest(THashMultisetQPTest);
  RegisterTest(TChainHashMultisetTest);
  RegisterTest(TLiteHashMultisetLPTest);
  RegisterTest(TLiteChainHashMultisetTest);
  RegisterTest(TLiteEquatableHashMultisetTest);
  RegisterTest(TThreadFGHashMultiSetTest);
end.

