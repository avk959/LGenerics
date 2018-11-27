unit LGTreeSetTest;

{$mode objfpc}{$H+}
{$MODESWITCH NESTEDPROCVARS}

interface
uses
  SysUtils, fpcunit, testregistry,
  LGUtils,
  LGAbstractContainer,
  LGHelpers,
  LGTreeSet,
  LGArrayHelpers;

type

  TIntHelper      = specialize TGComparableArrayHelper<Integer>;
  TIntArrayCursor = specialize TGArrayCursor<Integer>;
  IEnumerable     = specialize IGEnumerable<Integer>;
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
    function GetIncrement: TObjProc;
  public
    property Increment: TObjProc read GetIncrement;
    property Count: Integer read FCount;
  end;

  TObjArray      = specialize TGArray<TTestObj>;

  TTreeSetTest = class(TTestCase)
  private
  type
    TSet        = class(specialize TGTreeSet<Integer>);
    TAutoSet    = specialize TGAutoRef<TSet>;
    TObjSet     = specialize TGObjTreeSet<TTestObj>;
    TAutoObjSet = specialize TGAutoRef<TObjSet>;

    function IsEven(constref aValue: Integer): Boolean;
    function ObjIsEven(constref aObj: TTestObj): Boolean;
  published
    procedure TestCreate;
    procedure CreateArray;
    procedure CreateEnum;
    procedure CreateCapacity0;
    procedure CreateCapacity5;
    procedure Clear;
    procedure EnsureCapacity;
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

    procedure RetainAll;
    procedure RetainAll_1;
    procedure RetainAllSelf;

    procedure IsSuperset;
    procedure IsSubset;
    procedure IsEqual;
    procedure Intersecting;
    procedure SymmetricSubtract;

    procedure Order;
    procedure Order_1;

    procedure Head;
    procedure Tail;
    procedure Range;

    procedure FindFloor;
    procedure FindCeil;

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

  TComparableTreeSetTest = class(TTestCase)
  private
  type
    TSet        = class(specialize TGComparableTreeSet<Integer>);
    TAutoSet    = specialize TGAutoRef<TSet>;

    function IsEven(constref aValue: Integer): Boolean;
    function ObjIsEven(constref aObj: TTestObj): Boolean;
  published
    procedure TestCreate;
    procedure CreateArray;
    procedure CreateEnum;
    procedure CreateCapacity0;
    procedure CreateCapacity5;
    procedure Clear;
    procedure EnsureCapacity;
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

    procedure RetainAll;
    procedure RetainAll_1;
    procedure RetainAllSelf;

    procedure IsSuperset;
    procedure IsSubset;
    procedure IsEqual;
    procedure Intersecting;
    procedure SymmetricSubtract;

    procedure Order;
    procedure Order_1;

    procedure Head;
    procedure Tail;
    procedure Range;

    procedure FindFloor;
    procedure FindCeil;
  end;

  TRegularTreeSetTest = class(TTestCase)
  private
  type
    TSet        = class(specialize TGRegularTreeSet<Integer>);
    TAutoSet    = specialize TGAutoRef<TSet>;
    TObjSet     = specialize TGObjectRegularTreeSet<TTestObj>;
    TAutoObjSet = specialize TGAutoRef<TObjSet>;

    function IsEven(constref aValue: Integer): Boolean;
    function ObjIsEven(constref aObj: TTestObj): Boolean;
  published
    procedure TestCreate;
    procedure CreateArray;
    procedure CreateEnum;
    procedure CreateCapacity0;
    procedure CreateCapacity5;
    procedure Clear;
    procedure EnsureCapacity;
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

    procedure RetainAll;
    procedure RetainAll_1;
    procedure RetainAllSelf;

    procedure IsSuperset;
    procedure IsSubset;
    procedure IsEqual;
    procedure Intersecting;
    procedure SymmetricSubtract;

    procedure Order;
    procedure Order_1;

    procedure Head;
    procedure Tail;
    procedure Range;

    procedure FindFloor;
    procedure FindCeil;

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

  TDelegatedTreeSetTest = class(TTestCase)
  private
  type
    TSet        = class(specialize TGDelegatedTreeSet<Integer>);
    TAutoSet    = specialize TGAutoRef<TSet>;
    TObjSet     = specialize TGObjectDelegatedTreeSet<TTestObj>;
    TAutoObjSet = specialize TGAutoRef<TObjSet>;

    function IsEven(constref aValue: Integer): Boolean;
    function ObjIsEven(constref aObj: TTestObj): Boolean;
  published
    procedure TestCreate;
    procedure CreateArray;
    procedure CreateEnum;
    procedure CreateCapacity0;
    procedure CreateCapacity5;
    procedure Clear;
    procedure EnsureCapacity;
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

    procedure RetainAll;
    procedure RetainAll_1;
    procedure RetainAllSelf;

    procedure IsSuperset;
    procedure IsSubset;
    procedure IsEqual;
    procedure Intersecting;
    procedure SymmetricSubtract;

    procedure Order;
    procedure Order_1;

    procedure Head;
    procedure Tail;
    procedure Range;

    procedure FindFloor;
    procedure FindCeil;

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

  TLiteTreeSetTest = class(TTestCase)
  private
  type
    TSet = specialize TGLiteTreeSet<Integer, Integer>;

    function IsEven(constref aValue: Integer): Boolean;
    function ObjIsEven(constref aObj: TTestObj): Boolean;
  published
    procedure Clear;
    procedure EnsureCapacity;
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

    procedure RetainAll;
    procedure RetainAll_1;
    procedure RetainAllSelf;

    procedure IsSuperset;
    procedure IsSubset;
    procedure IsEqual;
    procedure Intersecting;
    procedure SymmetricSubtract;

    procedure Order;
    procedure Order_1;

    procedure Head;
    procedure Tail;
    procedure Range;

    procedure FindFloor;
    procedure FindCeil;
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

function TTreeSetTest.IsEven(constref aValue: Integer): Boolean;
begin
  Result := not Odd(aValue);
end;

function TTreeSetTest.ObjIsEven(constref aObj: TTestObj): Boolean;
begin
  Result := not Odd(aObj.Value);
end;

procedure TTreeSetTest.TestCreate;
var
  s: TAutoSet;
begin
  AssertTrue(s.Instance.Count = 0);
  AssertTrue(s.Instance.Capacity >= DEFAULT_CONTAINER_CAPACITY);
end;

procedure TTreeSetTest.CreateArray;
var
  s: TAutoSet;
  I: Integer;
begin
  s.Instance := TSet.Create(IntArray11);
  AssertTrue(s.Instance.Count = 11);
  AssertTrue(s.Instance.Capacity >= DEFAULT_CONTAINER_CAPACITY);
  for I in IntArray11 do
    AssertTrue(s.Instance.Contains(I));
end;

procedure TTreeSetTest.CreateEnum;
var
  s: TAutoSet;
  I: Integer;
  e: IEnumerable;
begin
  e := TIntArrayCursor.Create(TIntHelper.CreateCopy(IntArray11));
  s.Instance := TSet.Create(e);
  AssertTrue(s.Instance.Count = 11);
  for I in IntArray11 do
    AssertTrue(s.Instance.Contains(I));
end;

procedure TTreeSetTest.CreateCapacity0;
var
  s: TAutoSet;
begin
  s.Instance := TSet.Create(0);
  AssertTrue(s.Instance.Count = 0);
  AssertTrue(s.Instance.Capacity = 0);
end;

procedure TTreeSetTest.CreateCapacity5;
var
  s: TAutoSet;
begin
  s.Instance := TSet.Create(5);
  AssertTrue(s.Instance.Count = 0);
  AssertTrue(s.Instance.Capacity >= 5);
end;

procedure TTreeSetTest.Clear;
var
  s: TAutoSet;
begin
  s.Instance := TSet.Create(IntArray21);
  AssertTrue(s.Instance.Count = 21);
  AssertTrue(s.Instance.Capacity >= 21);
  s.Instance.Clear;
  AssertTrue(s.Instance.Count = 0);
  AssertTrue(s.Instance.Capacity = 0);
end;

procedure TTreeSetTest.EnsureCapacity;
var
  s: TAutoSet;
begin
  s.Instance.Clear;
  AssertTrue(s.Instance.Capacity = 0);
  s.Instance.EnsureCapacity(100);
  AssertTrue(s.Instance.Capacity >= 100);
end;

procedure TTreeSetTest.Add;
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

procedure TTreeSetTest.Add100;
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

procedure TTreeSetTest.AddArray;
var
  s: TAutoSet;
begin
  AssertTrue(s.Instance.AddAll(IntArray21) = System.Length(IntArray21));
  AssertTrue(s.Instance.Count = System.Length(IntArray21));
  AssertTrue(s.Instance.ContainsAll(IntArray21));
end;

procedure TTreeSetTest.AddEnum;
var
  s: TAutoSet;
  e: IEnumerable;
begin
  e := TIntArrayCursor.Create(TIntHelper.CreateCopy(IntArray21));
  AssertTrue(s.Instance.AddAll(e) = System.Length(IntArray21));
  AssertTrue(s.Instance.Count = System.Length(IntArray21));
  AssertTrue(s.Instance.ContainsAll(IntArray21));
end;

procedure TTreeSetTest.AddSelf;
var
  s: TAutoSet;
begin
  s.Instance.AddAll(IntArray21);
  AssertTrue(s.Instance.AddAll(s.Instance) = 0);
  AssertTrue(s.Instance.Count = System.Length(IntArray21));
end;

procedure TTreeSetTest.Remove;
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

procedure TTreeSetTest.RemoveArray;
var
  s: TAutoSet;
begin
  s.Instance := TSet.Create(IntArray21);
  AssertTrue(s.Instance.RemoveAll(IntArray11) = 11);
  AssertTrue(s.Instance.Count = 10);
end;

procedure TTreeSetTest.RemoveEnum;
var
  s: TAutoSet;
  e: IEnumerable;
begin
  e := TIntArrayCursor.Create(TIntHelper.CreateCopy(IntArray11));
  s.Instance := TSet.Create(IntArray21);
  AssertTrue(s.Instance.RemoveAll(e) = 11);
  AssertTrue(s.Instance.Count = 10);
end;

procedure TTreeSetTest.RemoveSelf;
var
  s: TAutoSet;
begin
  s.Instance := TSet.Create(IntArray21);
  AssertTrue(s.Instance.RemoveAll(s.Instance) = 21);
  AssertTrue(s.Instance.IsEmpty);
end;

procedure TTreeSetTest.RemoveIfRegular;
var
  s: TAutoSet;
begin
  s.Instance := TSet.Create(IntArray21);
  AssertTrue(s.Instance.RemoveIf(@IsEvenInt) = 10);
  AssertTrue(s.Instance.Count = 11);
  AssertTrue(s.Instance.ContainsAll(IntArray11));
end;

procedure TTreeSetTest.RemoveIfDelegated;
var
  s: TAutoSet;
begin
  s.Instance := TSet.Create(IntArray21);
  AssertTrue(s.Instance.RemoveIf(@IsEven) = 10);
  AssertTrue(s.Instance.Count = 11);
  AssertTrue(s.Instance.ContainsAll(IntArray11));
end;

procedure TTreeSetTest.RemoveIfNested;
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

procedure TTreeSetTest.ExtractIfRegular;
var
  s: TAutoSet;
  e: TIntArray;
begin
  s.Instance := TSet.Create(IntArray21);
  e := s.Instance.ExtractIf(@IsEvenInt);
  AssertTrue(s.Instance.Count = 11);
  AssertTrue(s.Instance.ContainsAll(IntArray11));
  AssertTrue(System.Length(e) = 10);
  AssertTrue(TIntHelper.Same(IntArray10, e));
end;

procedure TTreeSetTest.ExtractIfDelegated;
var
  s: TAutoSet;
  e: TIntArray;
begin
  s.Instance := TSet.Create(IntArray21);
  e := s.Instance.ExtractIf(@IsEven);
  AssertTrue(s.Instance.Count = 11);
  AssertTrue(s.Instance.ContainsAll(IntArray11));
  AssertTrue(System.Length(e) = 10);
  AssertTrue(TIntHelper.Same(IntArray10, e));
end;

procedure TTreeSetTest.ExtractIfNested;
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

procedure TTreeSetTest.RetainAll;
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

procedure TTreeSetTest.RetainAll_1;
var
  s, s1: TAutoSet;
begin
  s.Instance := TSet.Create(IntArray10);
  s1.Instance := TSet.Create(IntArray11);
  s.Instance.RetainAll(s1.Instance);
  AssertTrue(s.Instance.IsEmpty);
end;

procedure TTreeSetTest.RetainAllSelf;
var
  s: TAutoSet;
begin
  s.Instance := TSet.Create(IntArray21);
  AssertTrue(s.Instance.Count = 21);
  s.Instance.RetainAll(s.Instance);
  AssertTrue(s.Instance.Count = 21);
  AssertTrue(s.Instance.ContainsAll(IntArray21));
end;

procedure TTreeSetTest.IsSuperset;
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

procedure TTreeSetTest.IsSubset;
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

procedure TTreeSetTest.IsEqual;
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

procedure TTreeSetTest.Intersecting;
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

procedure TTreeSetTest.SymmetricSubtract;
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

procedure TTreeSetTest.Order;
var
  s: TAutoSet;
begin
  s.Instance := TSet.Create(TIntHelper.CreateRandomShuffle(IntArray21));
  AssertTrue(TIntHelper.Same(IntArray21, s.Instance.ToArray));
  AssertTrue(TIntHelper.Same(s.Instance.Reverse.ToArray, TIntHelper.CreateReverseCopy(IntArray21)));
end;

procedure TTreeSetTest.Order_1;
var
  s: TAutoSet;
  a: TIntArray;
  I: Integer;
begin
  System.SetLength(a, 100);
  for I := 0 to System.High(a) do
    a[I] := I;
  TIntHelper.RandomShuffle(a);
  s.Instance := TSet.Create(a);
  AssertTrue(s.Instance.FindFirst(I));
  AssertTrue(I = 0);
  AssertTrue(s.Instance.FindLast(I));
  AssertTrue(I = 99);
  AssertTrue(s.Instance.FindMin(I));
  AssertTrue(I = 0);
  AssertTrue(s.Instance.FindMax(I));
  AssertTrue(I = 99);
end;

procedure TTreeSetTest.Head;
var
  s: TAutoSet;
  a: TIntArray;
  I, v: Integer;
begin
  System.SetLength(a, 100);
  for I := 0 to System.High(a) do
    a[I] := I;
  TIntHelper.RandomShuffle(a);
  s.Instance := TSet.Create(a);
  AssertTrue(s.Instance.Head(0).None);
  I := 0;
  for v in s.Instance.Head(80) do
    begin
      AssertTrue(I = v);
      Inc(I);
    end;
  AssertTrue(I = 80);
  I := 0;
  for v in s.Instance.Head(80, True) do
    begin
      AssertTrue(I = v);
      Inc(I);
    end;
  AssertTrue(I = 81);
end;

procedure TTreeSetTest.Tail;
var
  s: TAutoSet;
  a: TIntArray;
  I, v: Integer;
begin
  System.SetLength(a, 100);
  for I := 0 to System.High(a) do
    a[I] := I;
  TIntHelper.RandomShuffle(a);
  s.Instance := TSet.Create(a);
  AssertTrue(s.Instance.Tail(100).None);
  I := 30;
  for v in s.Instance.Tail(30) do
    begin
      AssertTrue(I = v);
      Inc(I);
    end;
  AssertTrue(I = 100);
end;

procedure TTreeSetTest.Range;
var
  s: TAutoSet;
  a: TIntArray;
  I, v: Integer;
begin
  System.SetLength(a, 100);
  for I := 0 to System.High(a) do
    a[I] := I;
  TIntHelper.RandomShuffle(a);
  s.Instance := TSet.Create(a);
  AssertTrue(s.Instance.Range(10, 10).None);
  AssertTrue(s.Instance.Range(100, 100).None);
  AssertTrue(s.Instance.Range(0, 0).None);
  I := 10;
  for v in s.Instance.Range(10, 90) do
    begin
      AssertTrue(I = v);
      Inc(I);
    end;
  AssertTrue(I = 90);
end;

procedure TTreeSetTest.FindFloor;
var
  s: TAutoSet;
  I: Integer;
begin
  for I := 1 to 16 do
    s.Instance.Add(I * 3);

  AssertFalse(s.Instance.FindFloor(2, I));
  AssertFalse(s.Instance.FindFloor(3, I));
  AssertTrue(s.Instance.FindFloor(3, I, True));
  AssertTrue(I = 3);
  AssertTrue(s.Instance.FindFloor(9, I));
  AssertTrue(I = 6);
  AssertTrue(s.Instance.FindFloor(9, I, True));
  AssertTrue(I = 9);
  AssertTrue(s.Instance.FindFloor(13, I));
  AssertTrue(I = 12);
  AssertTrue(s.Instance.FindFloor(22, I));
  AssertTrue(I = 21);
  AssertTrue(s.Instance.FindFloor(27, I));
  AssertTrue(I = 24);
  AssertTrue(s.Instance.FindFloor(27, I, True));
  AssertTrue(I = 27);
end;

procedure TTreeSetTest.FindCeil;
var
  s: TAutoSet;
  I: Integer;
begin
  for I := 1 to 16 do
    s.Instance.Add(I * 3);
  AssertTrue(s.Instance.FindCeil(48, I));
  AssertTrue(I = 48);
  AssertFalse(s.Instance.FindCeil(48, I, False));
  AssertTrue(s.Instance.FindCeil(9, I));
  AssertTrue(I = 9);
  AssertTrue(s.Instance.FindCeil(9, I, False));
  AssertTrue(I = 12);
  AssertTrue(s.Instance.FindCeil(13, I));
  AssertTrue(I = 15);
  AssertTrue(s.Instance.FindCeil(22, I));
  AssertTrue(I = 24);
  AssertTrue(s.Instance.FindCeil(27, I));
  AssertTrue(I = 27);
  AssertTrue(s.Instance.FindCeil(27, I, False));
  AssertTrue(I = 30);
end;

procedure TTreeSetTest.ObjectSetRemove;
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

procedure TTreeSetTest.ObjectSetRemoveArray;
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

procedure TTreeSetTest.ObjectSetRemoveEnum;
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

procedure TTreeSetTest.ObjectSetRemoveSelf;
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

procedure TTreeSetTest.ObjectSetRemoveIfRegular;
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

procedure TTreeSetTest.ObjectSetRemoveIfDelegated;
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

procedure TTreeSetTest.ObjectSetRemoveIfNested;
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

procedure TTreeSetTest.ObjectSetExtractIfRegular;
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

procedure TTreeSetTest.ObjectSetExtractIfDelegated;
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

procedure TTreeSetTest.ObjectSetExtractIfNested;
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

procedure TTreeSetTest.ObjectSetRetain;
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

procedure TTreeSetTest.ObjectSetSymmetricSubtract;
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


function TComparableTreeSetTest.IsEven(constref aValue: Integer): Boolean;
begin
  Result := not Odd(aValue);
end;

function TComparableTreeSetTest.ObjIsEven(constref aObj: TTestObj): Boolean;
begin
   Result := not Odd(aObj.Value);
end;

procedure TComparableTreeSetTest.TestCreate;
var
  s: TAutoSet;
begin
  AssertTrue(s.Instance.Count = 0);
  AssertTrue(s.Instance.Capacity >= DEFAULT_CONTAINER_CAPACITY);
end;

procedure TComparableTreeSetTest.CreateArray;
var
  s: TAutoSet;
  I: Integer;
begin
  s.Instance := TSet.Create(IntArray11);
  AssertTrue(s.Instance.Count = 11);
  AssertTrue(s.Instance.Capacity >= DEFAULT_CONTAINER_CAPACITY);
  for I in IntArray11 do
    AssertTrue(s.Instance.Contains(I));
end;

procedure TComparableTreeSetTest.CreateEnum;
var
  s: TAutoSet;
  I: Integer;
  e: IEnumerable;
begin
  e := TIntArrayCursor.Create(TIntHelper.CreateCopy(IntArray11));
  s.Instance := TSet.Create(e);
  AssertTrue(s.Instance.Count = 11);
  AssertTrue(s.Instance.Capacity >= DEFAULT_CONTAINER_CAPACITY);
  for I in IntArray11 do
    AssertTrue(s.Instance.Contains(I));
end;

procedure TComparableTreeSetTest.CreateCapacity0;
var
  s: TAutoSet;
begin
  s.Instance := TSet.Create(0);
  AssertTrue(s.Instance.Count = 0);
  AssertTrue(s.Instance.Capacity = 0);
end;

procedure TComparableTreeSetTest.CreateCapacity5;
var
  s: TAutoSet;
begin
  s.Instance := TSet.Create(5);
  AssertTrue(s.Instance.Count = 0);
  AssertTrue(s.Instance.Capacity >= 5);
end;

procedure TComparableTreeSetTest.Clear;
var
  s: TAutoSet;
begin
  s.Instance := TSet.Create(IntArray21);
  AssertTrue(s.Instance.Count = 21);
  AssertTrue(s.Instance.Capacity >= 21);
  s.Instance.Clear;
  AssertTrue(s.Instance.Count = 0);
  AssertTrue(s.Instance.Capacity = 0);
end;

procedure TComparableTreeSetTest.EnsureCapacity;
var
  s: TAutoSet;
begin
  s.Instance.Clear;
  AssertTrue(s.Instance.Capacity = 0);
  s.Instance.EnsureCapacity(100);
  AssertTrue(s.Instance.Capacity >= 100);
end;

procedure TComparableTreeSetTest.Add;
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

procedure TComparableTreeSetTest.Add100;
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

procedure TComparableTreeSetTest.AddArray;
var
  s: TAutoSet;
begin
  AssertTrue(s.Instance.AddAll(IntArray21) = System.Length(IntArray21));
  AssertTrue(s.Instance.Count = System.Length(IntArray21));
  AssertTrue(s.Instance.ContainsAll(IntArray21));
end;

procedure TComparableTreeSetTest.AddEnum;
var
  s: TAutoSet;
  e: IEnumerable;
begin
  e := TIntArrayCursor.Create(TIntHelper.CreateCopy(IntArray21));
  AssertTrue(s.Instance.AddAll(e) = System.Length(IntArray21));
  AssertTrue(s.Instance.Count = System.Length(IntArray21));
  AssertTrue(s.Instance.ContainsAll(IntArray21));
end;

procedure TComparableTreeSetTest.AddSelf;
var
  s: TAutoSet;
begin
  s.Instance.AddAll(IntArray21);
  AssertTrue(s.Instance.AddAll(s.Instance) = 0);
  AssertTrue(s.Instance.Count = System.Length(IntArray21));
end;

procedure TComparableTreeSetTest.Remove;
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

procedure TComparableTreeSetTest.RemoveArray;
var
  s: TAutoSet;
begin
  s.Instance := TSet.Create(IntArray21);
  AssertTrue(s.Instance.RemoveAll(IntArray11) = 11);
  AssertTrue(s.Instance.Count = 10);
end;

procedure TComparableTreeSetTest.RemoveEnum;
var
  s: TAutoSet;
  e: IEnumerable;
begin
  e := TIntArrayCursor.Create(TIntHelper.CreateCopy(IntArray11));
  s.Instance := TSet.Create(IntArray21);
  AssertTrue(s.Instance.RemoveAll(e) = 11);
  AssertTrue(s.Instance.Count = 10);
end;

procedure TComparableTreeSetTest.RemoveSelf;
var
  s: TAutoSet;
begin
  s.Instance := TSet.Create(IntArray21);
  AssertTrue(s.Instance.RemoveAll(s.Instance) = 21);
  AssertTrue(s.Instance.IsEmpty);
end;

procedure TComparableTreeSetTest.RemoveIfRegular;
var
  s: TAutoSet;
begin
  s.Instance := TSet.Create(IntArray21);
  AssertTrue(s.Instance.RemoveIf(@IsEvenInt) = 10);
  AssertTrue(s.Instance.Count = 11);
  AssertTrue(s.Instance.ContainsAll(IntArray11));
end;

procedure TComparableTreeSetTest.RemoveIfDelegated;
var
  s: TAutoSet;
begin
  s.Instance := TSet.Create(IntArray21);
  AssertTrue(s.Instance.RemoveIf(@IsEven) = 10);
  AssertTrue(s.Instance.Count = 11);
  AssertTrue(s.Instance.ContainsAll(IntArray11));
end;

procedure TComparableTreeSetTest.RemoveIfNested;
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

procedure TComparableTreeSetTest.ExtractIfRegular;
var
  s: TAutoSet;
  e: TIntArray;
begin
  s.Instance := TSet.Create(IntArray21);
  e := s.Instance.ExtractIf(@IsEvenInt);
  AssertTrue(s.Instance.Count = 11);
  AssertTrue(s.Instance.ContainsAll(IntArray11));
  AssertTrue(System.Length(e) = 10);
  AssertTrue(TIntHelper.Same(IntArray10, e));
end;

procedure TComparableTreeSetTest.ExtractIfDelegated;
var
  s: TAutoSet;
  e: TIntArray;
begin
  s.Instance := TSet.Create(IntArray21);
  e := s.Instance.ExtractIf(@IsEven);
  AssertTrue(s.Instance.Count = 11);
  AssertTrue(s.Instance.ContainsAll(IntArray11));
  AssertTrue(System.Length(e) = 10);
  AssertTrue(TIntHelper.Same(IntArray10, e));
end;

procedure TComparableTreeSetTest.ExtractIfNested;
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

procedure TComparableTreeSetTest.RetainAll;
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

procedure TComparableTreeSetTest.RetainAll_1;
var
  s, s1: TAutoSet;
begin
  s.Instance := TSet.Create(IntArray10);
  s1.Instance := TSet.Create(IntArray11);
  s.Instance.RetainAll(s1.Instance);
  AssertTrue(s.Instance.IsEmpty);
end;

procedure TComparableTreeSetTest.RetainAllSelf;
var
  s: TAutoSet;
begin
  s.Instance := TSet.Create(IntArray21);
  AssertTrue(s.Instance.Count = 21);
  s.Instance.RetainAll(s.Instance);
  AssertTrue(s.Instance.Count = 21);
  AssertTrue(s.Instance.ContainsAll(IntArray21));
end;

procedure TComparableTreeSetTest.IsSuperset;
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

procedure TComparableTreeSetTest.IsSubset;
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

procedure TComparableTreeSetTest.IsEqual;
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

procedure TComparableTreeSetTest.Intersecting;
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

procedure TComparableTreeSetTest.SymmetricSubtract;
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

procedure TComparableTreeSetTest.Order;
var
  s: TAutoSet;
begin
  s.Instance := TSet.Create(TIntHelper.CreateRandomShuffle(IntArray21));
  AssertTrue(TIntHelper.Same(IntArray21, s.Instance.ToArray));
  AssertTrue(TIntHelper.Same(s.Instance.Reverse.ToArray, TIntHelper.CreateReverseCopy(IntArray21)));
end;

procedure TComparableTreeSetTest.Order_1;
var
  s: TAutoSet;
  a: TIntArray;
  I: Integer;
begin
  System.SetLength(a, 100);
  for I := 0 to System.High(a) do
    a[I] := I;
  TIntHelper.RandomShuffle(a);
  s.Instance := TSet.Create(a);
  AssertTrue(s.Instance.FindFirst(I));
  AssertTrue(I = 0);
  AssertTrue(s.Instance.FindLast(I));
  AssertTrue(I = 99);
  AssertTrue(s.Instance.FindMin(I));
  AssertTrue(I = 0);
  AssertTrue(s.Instance.FindMax(I));
  AssertTrue(I = 99);
end;

procedure TComparableTreeSetTest.Head;
var
  s: TAutoSet;
  a: TIntArray;
  I, v: Integer;
begin
  System.SetLength(a, 100);
  for I := 0 to System.High(a) do
    a[I] := I;
  TIntHelper.RandomShuffle(a);
  s.Instance := TSet.Create(a);
  AssertTrue(s.Instance.Head(0).None);
  I := 0;
  for v in s.Instance.Head(80) do
    begin
      AssertTrue(I = v);
      Inc(I);
    end;
  AssertTrue(I = 80);
  I := 0;
  for v in s.Instance.Head(80, True) do
    begin
      AssertTrue(I = v);
      Inc(I);
    end;
  AssertTrue(I = 81);
end;

procedure TComparableTreeSetTest.Tail;
var
  s: TAutoSet;
  a: TIntArray;
  I, v: Integer;
begin
  System.SetLength(a, 100);
  for I := 0 to System.High(a) do
    a[I] := I;
  TIntHelper.RandomShuffle(a);
  s.Instance := TSet.Create(a);
  AssertTrue(s.Instance.Tail(100).None);
  I := 30;
  for v in s.Instance.Tail(30) do
    begin
      AssertTrue(I = v);
      Inc(I);
    end;
  AssertTrue(I = 100);
end;

procedure TComparableTreeSetTest.Range;
var
  s: TAutoSet;
  a: TIntArray;
  I, v: Integer;
begin
  System.SetLength(a, 100);
  for I := 0 to System.High(a) do
    a[I] := I;
  TIntHelper.RandomShuffle(a);
  s.Instance := TSet.Create(a);
  AssertTrue(s.Instance.Range(10, 10).None);
  AssertTrue(s.Instance.Range(100, 100).None);
  AssertTrue(s.Instance.Range(0, 0).None);
  I := 10;
  for v in s.Instance.Range(10, 90) do
    begin
      AssertTrue(I = v);
      Inc(I);
    end;
  AssertTrue(I = 90);
end;

procedure TComparableTreeSetTest.FindFloor;
var
  s: TAutoSet;
  I: Integer;
begin
  for I := 1 to 16 do
    s.Instance.Add(I * 3);

  AssertFalse(s.Instance.FindFloor(2, I));
  AssertFalse(s.Instance.FindFloor(3, I));
  AssertTrue(s.Instance.FindFloor(3, I, True));
  AssertTrue(I = 3);
  AssertTrue(s.Instance.FindFloor(9, I));
  AssertTrue(I = 6);
  AssertTrue(s.Instance.FindFloor(9, I, True));
  AssertTrue(I = 9);
  AssertTrue(s.Instance.FindFloor(13, I));
  AssertTrue(I = 12);
  AssertTrue(s.Instance.FindFloor(22, I));
  AssertTrue(I = 21);
  AssertTrue(s.Instance.FindFloor(27, I));
  AssertTrue(I = 24);
  AssertTrue(s.Instance.FindFloor(27, I, True));
  AssertTrue(I = 27);
end;

procedure TComparableTreeSetTest.FindCeil;
var
  s: TAutoSet;
  I: Integer;
begin
  for I := 1 to 16 do
    s.Instance.Add(I * 3);
  AssertTrue(s.Instance.FindCeil(48, I));
  AssertTrue(I = 48);
  AssertFalse(s.Instance.FindCeil(48, I, False));
  AssertTrue(s.Instance.FindCeil(9, I));
  AssertTrue(I = 9);
  AssertTrue(s.Instance.FindCeil(9, I, False));
  AssertTrue(I = 12);
  AssertTrue(s.Instance.FindCeil(13, I));
  AssertTrue(I = 15);
  AssertTrue(s.Instance.FindCeil(22, I));
  AssertTrue(I = 24);
  AssertTrue(s.Instance.FindCeil(27, I));
  AssertTrue(I = 27);
  AssertTrue(s.Instance.FindCeil(27, I, False));
  AssertTrue(I = 30);
end;

function TRegularTreeSetTest.IsEven(constref aValue: Integer): Boolean;
begin
  Result := not Odd(aValue);
end;

function TRegularTreeSetTest.ObjIsEven(constref aObj: TTestObj): Boolean;
begin
   Result := not Odd(aObj.Value);
end;

procedure TRegularTreeSetTest.TestCreate;
var
  s: TAutoSet;
begin
  AssertTrue(s.Instance.Count = 0);
  AssertTrue(s.Instance.Capacity >= DEFAULT_CONTAINER_CAPACITY);
end;

procedure TRegularTreeSetTest.CreateArray;
var
  s: TAutoSet;
  I: Integer;
begin
  s.Instance := TSet.Create(IntArray11, specialize TGDefaults<Integer>.Compare);
  AssertTrue(s.Instance.Count = 11);
  for I in IntArray11 do
    AssertTrue(s.Instance.Contains(I));
end;

procedure TRegularTreeSetTest.CreateEnum;
var
  s: TAutoSet;
  I: Integer;
  e: IEnumerable;
begin
  e := TIntArrayCursor.Create(TIntHelper.CreateCopy(IntArray11));
  s.Instance := TSet.Create(e, specialize TGDefaults<Integer>.Compare);
  AssertTrue(s.Instance.Count = 11);
  for I in IntArray11 do
    AssertTrue(s.Instance.Contains(I));
end;

procedure TRegularTreeSetTest.CreateCapacity0;
var
  s: TAutoSet;
begin
  s.Instance := TSet.Create(0, specialize TGDefaults<Integer>.Compare);
  AssertTrue(s.Instance.Count = 0);
  AssertTrue(s.Instance.Capacity = 0);
end;

procedure TRegularTreeSetTest.CreateCapacity5;
var
  s: TAutoSet;
begin
  s.Instance := TSet.Create(5, specialize TGDefaults<Integer>.Compare);
  AssertTrue(s.Instance.Count = 0);
  AssertTrue(s.Instance.Capacity >= 5);
end;

procedure TRegularTreeSetTest.Clear;
var
  s: TAutoSet;
begin
  s.Instance := TSet.Create(IntArray21, specialize TGDefaults<Integer>.Compare);
  AssertTrue(s.Instance.Count = 21);
  AssertTrue(s.Instance.Capacity >= 21);
  s.Instance.Clear;
  AssertTrue(s.Instance.Count = 0);
  AssertTrue(s.Instance.Capacity = 0);
end;

procedure TRegularTreeSetTest.EnsureCapacity;
var
  s: TAutoSet;
begin
  s.Instance.Clear;
  AssertTrue(s.Instance.Capacity = 0);
  s.Instance.EnsureCapacity(100);
  AssertTrue(s.Instance.Capacity >= 100);
end;

procedure TRegularTreeSetTest.Add;
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

procedure TRegularTreeSetTest.Add100;
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

procedure TRegularTreeSetTest.AddArray;
var
  s: TAutoSet;
begin
  AssertTrue(s.Instance.AddAll(IntArray21) = System.Length(IntArray21));
  AssertTrue(s.Instance.Count = System.Length(IntArray21));
  AssertTrue(s.Instance.ContainsAll(IntArray21));
end;

procedure TRegularTreeSetTest.AddEnum;
var
  s: TAutoSet;
  e: IEnumerable;
begin
  e := TIntArrayCursor.Create(TIntHelper.CreateCopy(IntArray21));
  AssertTrue(s.Instance.AddAll(e) = System.Length(IntArray21));
  AssertTrue(s.Instance.Count = System.Length(IntArray21));
  AssertTrue(s.Instance.ContainsAll(IntArray21));
end;

procedure TRegularTreeSetTest.AddSelf;
var
  s: TAutoSet;
begin
  s.Instance.AddAll(IntArray21);
  AssertTrue(s.Instance.AddAll(s.Instance) = 0);
  AssertTrue(s.Instance.Count = System.Length(IntArray21));
end;

procedure TRegularTreeSetTest.Remove;
var
  s: TAutoSet;
begin
  s.Instance := TSet.Create(IntArray21, specialize TGDefaults<Integer>.Compare);
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

procedure TRegularTreeSetTest.RemoveArray;
var
  s: TAutoSet;
begin
  s.Instance := TSet.Create(IntArray21, specialize TGDefaults<Integer>.Compare);
  AssertTrue(s.Instance.RemoveAll(IntArray11) = 11);
  AssertTrue(s.Instance.Count = 10);
end;

procedure TRegularTreeSetTest.RemoveEnum;
var
  s: TAutoSet;
  e: IEnumerable;
begin
  e := TIntArrayCursor.Create(TIntHelper.CreateCopy(IntArray11));
  s.Instance := TSet.Create(IntArray21, specialize TGDefaults<Integer>.Compare);
  AssertTrue(s.Instance.RemoveAll(e) = 11);
  AssertTrue(s.Instance.Count = 10);
end;

procedure TRegularTreeSetTest.RemoveSelf;
var
  s: TAutoSet;
begin
  s.Instance := TSet.Create(IntArray21, specialize TGDefaults<Integer>.Compare);
  AssertTrue(s.Instance.RemoveAll(s.Instance) = 21);
  AssertTrue(s.Instance.IsEmpty);
end;

procedure TRegularTreeSetTest.RemoveIfRegular;
var
  s: TAutoSet;
begin
  s.Instance := TSet.Create(IntArray21, specialize TGDefaults<Integer>.Compare);
  AssertTrue(s.Instance.RemoveIf(@IsEvenInt) = 10);
  AssertTrue(s.Instance.Count = 11);
  AssertTrue(s.Instance.ContainsAll(IntArray11));
end;

procedure TRegularTreeSetTest.RemoveIfDelegated;
var
  s: TAutoSet;
begin
  s.Instance := TSet.Create(IntArray21, specialize TGDefaults<Integer>.Compare);
  AssertTrue(s.Instance.RemoveIf(@IsEven) = 10);
  AssertTrue(s.Instance.Count = 11);
  AssertTrue(s.Instance.ContainsAll(IntArray11));
end;

procedure TRegularTreeSetTest.RemoveIfNested;
  function IsEvenInt(constref aValue: Integer): Boolean;
  begin
    Result := not Odd(aValue);
  end;
var
  s: TAutoSet;
begin
  s.Instance := TSet.Create(IntArray21, specialize TGDefaults<Integer>.Compare);
  AssertTrue(s.Instance.RemoveIf(@IsEvenInt) = 10);
  AssertTrue(s.Instance.Count = 11);
  AssertTrue(s.Instance.ContainsAll(IntArray11));
end;

procedure TRegularTreeSetTest.ExtractIfRegular;
var
  s: TAutoSet;
  e: TIntArray;
begin
  s.Instance := TSet.Create(IntArray21, specialize TGDefaults<Integer>.Compare);
  e := s.Instance.ExtractIf(@IsEvenInt);
  AssertTrue(s.Instance.Count = 11);
  AssertTrue(s.Instance.ContainsAll(IntArray11));
  AssertTrue(System.Length(e) = 10);
  AssertTrue(TIntHelper.Same(IntArray10, e));
end;

procedure TRegularTreeSetTest.ExtractIfDelegated;
var
  s: TAutoSet;
  e: TIntArray;
begin
  s.Instance := TSet.Create(IntArray21, specialize TGDefaults<Integer>.Compare);
  e := s.Instance.ExtractIf(@IsEven);
  AssertTrue(s.Instance.Count = 11);
  AssertTrue(s.Instance.ContainsAll(IntArray11));
  AssertTrue(System.Length(e) = 10);
  AssertTrue(TIntHelper.Same(IntArray10, e));
end;

procedure TRegularTreeSetTest.ExtractIfNested;
  function IsEvenInt(constref aValue: Integer): Boolean;
  begin
    Result := not Odd(aValue);
  end;
var
  s: TAutoSet;
  e: TIntArray;
begin
  s.Instance := TSet.Create(IntArray21, specialize TGDefaults<Integer>.Compare);
  e := s.Instance.ExtractIf(@IsEvenInt);
  AssertTrue(s.Instance.Count = 11);
  AssertTrue(s.Instance.ContainsAll(IntArray11));
  AssertTrue(System.Length(e) = 10);
  TIntHelper.QuickSort(e);
  AssertTrue(TIntHelper.Same(IntArray10, e));
end;

procedure TRegularTreeSetTest.RetainAll;
var
  s, s1: TAutoSet;
begin
  s.Instance := TSet.Create(IntArray21, specialize TGDefaults<Integer>.Compare);
  s1.Instance := TSet.Create(IntArray11, specialize TGDefaults<Integer>.Compare);
  s.Instance.AddAll([-3, -2, -1, 0]);
  AssertTrue(s.Instance.Count = 25);
  s.Instance.RetainAll(s1.Instance);
  AssertTrue(s.Instance.Count = 11);
  AssertTrue(s.Instance.ContainsAll(s1.Instance));
end;

procedure TRegularTreeSetTest.RetainAll_1;
var
  s, s1: TAutoSet;
begin
  s.Instance := TSet.Create(IntArray10, specialize TGDefaults<Integer>.Compare);
  s1.Instance := TSet.Create(IntArray11, specialize TGDefaults<Integer>.Compare);
  s.Instance.RetainAll(s1.Instance);
  AssertTrue(s.Instance.IsEmpty);
end;

procedure TRegularTreeSetTest.RetainAllSelf;
var
  s: TAutoSet;
begin
  s.Instance := TSet.Create(IntArray21, specialize TGDefaults<Integer>.Compare);
  AssertTrue(s.Instance.Count = 21);
  s.Instance.RetainAll(s.Instance);
  AssertTrue(s.Instance.Count = 21);
  AssertTrue(s.Instance.ContainsAll(IntArray21));
end;

procedure TRegularTreeSetTest.IsSuperset;
var
  s, s1: TAutoSet;
begin
  s.Instance := TSet.Create(IntArray21, specialize TGDefaults<Integer>.Compare);
  s1.Instance := TSet.Create(IntArray21, specialize TGDefaults<Integer>.Compare);
  AssertTrue(s.Instance.IsSuperset(s1.Instance));
  AssertTrue(s1.Instance.IsSuperset(s.Instance));
  AssertTrue(s.Instance.IsSuperset(s.Instance));
  s1.Instance := TSet.Create(IntArray11, specialize TGDefaults<Integer>.Compare);
  AssertTrue(s.Instance.IsSuperset(s1.Instance));
  AssertFalse(s1.Instance.IsSuperset(s.Instance));
end;

procedure TRegularTreeSetTest.IsSubset;
var
  s, s1: TAutoSet;
begin
  s.Instance := TSet.Create(IntArray21, specialize TGDefaults<Integer>.Compare);
  s1.Instance := TSet.Create(IntArray21, specialize TGDefaults<Integer>.Compare);
  AssertTrue(s.Instance.IsSubset(s1.Instance));
  AssertTrue(s1.Instance.IsSubset(s.Instance));
  AssertTrue(s.Instance.IsSubset(s.Instance));
  s1.Instance := TSet.Create(IntArray11, specialize TGDefaults<Integer>.Compare);
  AssertTrue(s1.Instance.IsSubset(s.Instance));
  AssertFalse(s.Instance.IsSubset(s1.Instance));
end;

procedure TRegularTreeSetTest.IsEqual;
var
  s, s1: TAutoSet;
begin
  s.Instance := TSet.Create(IntArray21, specialize TGDefaults<Integer>.Compare);
  s1.Instance := TSet.Create(IntArray21, specialize TGDefaults<Integer>.Compare);
  AssertTrue(s.Instance.IsEqual(s.Instance));
  AssertTrue(s.Instance.IsEqual(s1.Instance));
  s1.Instance := TSet.Create(IntArray11, specialize TGDefaults<Integer>.Compare);
  AssertFalse(s.Instance.IsEqual(s1.Instance));
end;

procedure TRegularTreeSetTest.Intersecting;
var
  s, s1: TAutoSet;
begin
  s.Instance := TSet.Create(IntArray21, specialize TGDefaults<Integer>.Compare);
  s1.Instance := TSet.Create(IntArray11, specialize TGDefaults<Integer>.Compare);
  AssertTrue(s.Instance.Intersecting(s.Instance));
  AssertTrue(s.Instance.Intersecting(s1.Instance));
  s.Instance := TSet.Create(IntArray10, specialize TGDefaults<Integer>.Compare);
  AssertFalse(s.Instance.Intersecting(s1.Instance));
  AssertFalse(s1.Instance.Intersecting(s.Instance));
end;

procedure TRegularTreeSetTest.SymmetricSubtract;
var
  s, s1: TAutoSet;
begin
  s.Instance := TSet.Create(IntArray21, specialize TGDefaults<Integer>.Compare);
  s1.Instance := TSet.Create(IntArray21, specialize TGDefaults<Integer>.Compare);
  s.Instance.SymmetricSubtract(s1.Instance);
  AssertTrue(s.Instance.IsEmpty);
  s.Instance := TSet.Create(IntArray21, specialize TGDefaults<Integer>.Compare);
  s1.Instance.Clear;
  s.Instance.SymmetricSubtract(s1.Instance);
  AssertTrue(s.Instance.Count = 21);
  AssertTrue(s.Instance.ContainsAll(IntArray21));
  s.Instance := TSet.Create(IntArray11, specialize TGDefaults<Integer>.Compare);
  s1.Instance := TSet.Create(IntArray10, specialize TGDefaults<Integer>.Compare);
  s.Instance.SymmetricSubtract(s1.Instance);
  AssertTrue(s.Instance.Count = 21);
  AssertTrue(s.Instance.ContainsAll(IntArray21));
  s.Instance := TSet.Create(IntArray11, specialize TGDefaults<Integer>.Compare);
  s1.Instance := TSet.Create(IntArray10, specialize TGDefaults<Integer>.Compare);
  s.Instance.AddAll([-5, -4, -3, -2, -1, 0]);
  s1.Instance.AddAll([-5, -4, -3, -2, -1, 0]);
  s.Instance.SymmetricSubtract(s1.Instance);
  AssertTrue(s.Instance.Count = 21);
  AssertTrue(s.Instance.ContainsAll(IntArray21));
end;

procedure TRegularTreeSetTest.Order;
var
  s: TAutoSet;
begin
  s.Instance := TSet.Create(TIntHelper.CreateRandomShuffle(IntArray21), specialize TGDefaults<Integer>.Compare);
  AssertTrue(TIntHelper.Same(IntArray21, s.Instance.ToArray));
  AssertTrue(TIntHelper.Same(s.Instance.Reverse.ToArray, TIntHelper.CreateReverseCopy(IntArray21)));
end;

procedure TRegularTreeSetTest.Order_1;
var
  s: TAutoSet;
  a: TIntArray;
  I: Integer;
begin
  System.SetLength(a, 100);
  for I := 0 to System.High(a) do
    a[I] := I;
  TIntHelper.RandomShuffle(a);
  s.Instance := TSet.Create(a, specialize TGDefaults<Integer>.Compare);
  AssertTrue(s.Instance.FindFirst(I));
  AssertTrue(I = 0);
  AssertTrue(s.Instance.FindLast(I));
  AssertTrue(I = 99);
  AssertTrue(s.Instance.FindMin(I));
  AssertTrue(I = 0);
  AssertTrue(s.Instance.FindMax(I));
  AssertTrue(I = 99);
end;

procedure TRegularTreeSetTest.Head;
var
  s: TAutoSet;
  a: TIntArray;
  I, v: Integer;
begin
  System.SetLength(a, 100);
  for I := 0 to System.High(a) do
    a[I] := I;
  TIntHelper.RandomShuffle(a);
  s.Instance := TSet.Create(a, specialize TGDefaults<Integer>.Compare);
  AssertTrue(s.Instance.Head(0).None);
  I := 0;
  for v in s.Instance.Head(80) do
    begin
      AssertTrue(I = v);
      Inc(I);
    end;
  AssertTrue(I = 80);
  I := 0;
  for v in s.Instance.Head(80, True) do
    begin
      AssertTrue(I = v);
      Inc(I);
    end;
  AssertTrue(I = 81);
end;

procedure TRegularTreeSetTest.Tail;
var
  s: TAutoSet;
  a: TIntArray;
  I, v: Integer;
begin
  System.SetLength(a, 100);
  for I := 0 to System.High(a) do
    a[I] := I;
  TIntHelper.RandomShuffle(a);
  s.Instance := TSet.Create(a, specialize TGDefaults<Integer>.Compare);
  AssertTrue(s.Instance.Tail(100).None);
  I := 30;
  for v in s.Instance.Tail(30) do
    begin
      AssertTrue(I = v);
      Inc(I);
    end;
  AssertTrue(I = 100);
end;

procedure TRegularTreeSetTest.Range;
var
  s: TAutoSet;
  a: TIntArray;
  I, v: Integer;
begin
  System.SetLength(a, 100);
  for I := 0 to System.High(a) do
    a[I] := I;
  TIntHelper.RandomShuffle(a);
  s.Instance := TSet.Create(a, specialize TGDefaults<Integer>.Compare);
  AssertTrue(s.Instance.Range(10, 10).None);
  AssertTrue(s.Instance.Range(100, 100).None);
  AssertTrue(s.Instance.Range(0, 0).None);
  I := 10;
  for v in s.Instance.Range(10, 90) do
    begin
      AssertTrue(I = v);
      Inc(I);
    end;
  AssertTrue(I = 90);
end;

procedure TRegularTreeSetTest.FindFloor;
var
  s: TAutoSet;
  I: Integer;
begin
  for I := 1 to 16 do
    s.Instance.Add(I * 3);

  AssertFalse(s.Instance.FindFloor(2, I));
  AssertFalse(s.Instance.FindFloor(3, I));
  AssertTrue(s.Instance.FindFloor(3, I, True));
  AssertTrue(I = 3);
  AssertTrue(s.Instance.FindFloor(9, I));
  AssertTrue(I = 6);
  AssertTrue(s.Instance.FindFloor(9, I, True));
  AssertTrue(I = 9);
  AssertTrue(s.Instance.FindFloor(13, I));
  AssertTrue(I = 12);
  AssertTrue(s.Instance.FindFloor(22, I));
  AssertTrue(I = 21);
  AssertTrue(s.Instance.FindFloor(27, I));
  AssertTrue(I = 24);
  AssertTrue(s.Instance.FindFloor(27, I, True));
  AssertTrue(I = 27);
end;

procedure TRegularTreeSetTest.FindCeil;
var
  s: TAutoSet;
  I: Integer;
begin
  for I := 1 to 16 do
    s.Instance.Add(I * 3);
  AssertTrue(s.Instance.FindCeil(48, I));
  AssertTrue(I = 48);
  AssertFalse(s.Instance.FindCeil(48, I, False));
  AssertTrue(s.Instance.FindCeil(9, I));
  AssertTrue(I = 9);
  AssertTrue(s.Instance.FindCeil(9, I, False));
  AssertTrue(I = 12);
  AssertTrue(s.Instance.FindCeil(13, I));
  AssertTrue(I = 15);
  AssertTrue(s.Instance.FindCeil(22, I));
  AssertTrue(I = 24);
  AssertTrue(s.Instance.FindCeil(27, I));
  AssertTrue(I = 27);
  AssertTrue(s.Instance.FindCeil(27, I, False));
  AssertTrue(I = 30);
end;

procedure TRegularTreeSetTest.ObjectSetRemove;
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

procedure TRegularTreeSetTest.ObjectSetRemoveArray;
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

procedure TRegularTreeSetTest.ObjectSetRemoveEnum;
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

procedure TRegularTreeSetTest.ObjectSetRemoveSelf;
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

procedure TRegularTreeSetTest.ObjectSetRemoveIfRegular;
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

procedure TRegularTreeSetTest.ObjectSetRemoveIfDelegated;
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

procedure TRegularTreeSetTest.ObjectSetRemoveIfNested;
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

procedure TRegularTreeSetTest.ObjectSetExtractIfRegular;
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

procedure TRegularTreeSetTest.ObjectSetExtractIfDelegated;
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

procedure TRegularTreeSetTest.ObjectSetExtractIfNested;
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

procedure TRegularTreeSetTest.ObjectSetRetain;
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

procedure TRegularTreeSetTest.ObjectSetSymmetricSubtract;
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


function TDelegatedTreeSetTest.IsEven(constref aValue: Integer): Boolean;
begin
  Result := not Odd(aValue);
end;

function TDelegatedTreeSetTest.ObjIsEven(constref aObj: TTestObj): Boolean;
begin
   Result := not Odd(aObj.Value);
end;

procedure TDelegatedTreeSetTest.TestCreate;
var
  s: TAutoSet;
begin
  AssertTrue(s.Instance.Count = 0);
  AssertTrue(s.Instance.Capacity >= DEFAULT_CONTAINER_CAPACITY);
end;

procedure TDelegatedTreeSetTest.CreateArray;
var
  s: TAutoSet;
  I: Integer;
begin
  s.Instance := TSet.Create(IntArray11, specialize TGDefaults<Integer>.OnCompare);
  AssertTrue(s.Instance.Count = 11);
  for I in IntArray11 do
    AssertTrue(s.Instance.Contains(I));
end;

procedure TDelegatedTreeSetTest.CreateEnum;
var
  s: TAutoSet;
  I: Integer;
  e: IEnumerable;
begin
  e := TIntArrayCursor.Create(TIntHelper.CreateCopy(IntArray11));
  s.Instance := TSet.Create(e, specialize TGDefaults<Integer>.OnCompare);
  AssertTrue(s.Instance.Count = 11);
  for I in IntArray11 do
    AssertTrue(s.Instance.Contains(I));
end;

procedure TDelegatedTreeSetTest.CreateCapacity0;
var
  s: TAutoSet;
begin
  s.Instance := TSet.Create(0, specialize TGDefaults<Integer>.OnCompare);
  AssertTrue(s.Instance.Count = 0);
  AssertTrue(s.Instance.Capacity = 0);
end;

procedure TDelegatedTreeSetTest.CreateCapacity5;
var
  s: TAutoSet;
begin
  s.Instance := TSet.Create(5, specialize TGDefaults<Integer>.OnCompare);
  AssertTrue(s.Instance.Count = 0);
  AssertTrue(s.Instance.Capacity >= 5);
end;

procedure TDelegatedTreeSetTest.Clear;
var
  s: TAutoSet;
begin
  s.Instance := TSet.Create(IntArray21, specialize TGDefaults<Integer>.OnCompare);
  AssertTrue(s.Instance.Count = 21);
  AssertTrue(s.Instance.Capacity >= 21);
  s.Instance.Clear;
  AssertTrue(s.Instance.Count = 0);
  AssertTrue(s.Instance.Capacity = 0);
end;

procedure TDelegatedTreeSetTest.EnsureCapacity;
var
  s: TAutoSet;
begin
  s.Instance.Clear;
  s.Instance.EnsureCapacity(100);
  AssertTrue(s.Instance.Capacity >= 100);
end;

procedure TDelegatedTreeSetTest.Add;
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

procedure TDelegatedTreeSetTest.Add100;
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

procedure TDelegatedTreeSetTest.AddArray;
var
  s: TAutoSet;
begin
  AssertTrue(s.Instance.AddAll(IntArray21) = System.Length(IntArray21));
  AssertTrue(s.Instance.Count = System.Length(IntArray21));
  AssertTrue(s.Instance.ContainsAll(IntArray21));
end;

procedure TDelegatedTreeSetTest.AddEnum;
var
  s: TAutoSet;
  e: IEnumerable;
begin
  e := TIntArrayCursor.Create(TIntHelper.CreateCopy(IntArray21));
  AssertTrue(s.Instance.AddAll(e) = System.Length(IntArray21));
  AssertTrue(s.Instance.Count = System.Length(IntArray21));
  AssertTrue(s.Instance.ContainsAll(IntArray21));
end;

procedure TDelegatedTreeSetTest.AddSelf;
var
  s: TAutoSet;
begin
  s.Instance.AddAll(IntArray21);
  AssertTrue(s.Instance.AddAll(s.Instance) = 0);
  AssertTrue(s.Instance.Count = System.Length(IntArray21));
end;

procedure TDelegatedTreeSetTest.Remove;
var
  s: TAutoSet;
begin
  s.Instance := TSet.Create(IntArray21, specialize TGDefaults<Integer>.OnCompare);
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

procedure TDelegatedTreeSetTest.RemoveArray;
var
  s: TAutoSet;
begin
  s.Instance := TSet.Create(IntArray21, specialize TGDefaults<Integer>.OnCompare);
  AssertTrue(s.Instance.RemoveAll(IntArray11) = 11);
  AssertTrue(s.Instance.Count = 10);
end;

procedure TDelegatedTreeSetTest.RemoveEnum;
var
  s: TAutoSet;
  e: IEnumerable;
begin
  e := TIntArrayCursor.Create(TIntHelper.CreateCopy(IntArray11));
  s.Instance := TSet.Create(IntArray21, specialize TGDefaults<Integer>.OnCompare);
  AssertTrue(s.Instance.RemoveAll(e) = 11);
  AssertTrue(s.Instance.Count = 10);
end;

procedure TDelegatedTreeSetTest.RemoveSelf;
var
  s: TAutoSet;
begin
  s.Instance := TSet.Create(IntArray21, specialize TGDefaults<Integer>.OnCompare);
  AssertTrue(s.Instance.RemoveAll(s.Instance) = 21);
  AssertTrue(s.Instance.IsEmpty);
end;

procedure TDelegatedTreeSetTest.RemoveIfRegular;
var
  s: TAutoSet;
begin
  s.Instance := TSet.Create(IntArray21, specialize TGDefaults<Integer>.OnCompare);
  AssertTrue(s.Instance.RemoveIf(@IsEvenInt) = 10);
  AssertTrue(s.Instance.Count = 11);
  AssertTrue(s.Instance.ContainsAll(IntArray11));
end;

procedure TDelegatedTreeSetTest.RemoveIfDelegated;
var
  s: TAutoSet;
begin
  s.Instance := TSet.Create(IntArray21, specialize TGDefaults<Integer>.OnCompare);
  AssertTrue(s.Instance.RemoveIf(@IsEven) = 10);
  AssertTrue(s.Instance.Count = 11);
  AssertTrue(s.Instance.ContainsAll(IntArray11));
end;

procedure TDelegatedTreeSetTest.RemoveIfNested;
  function IsEvenInt(constref aValue: Integer): Boolean;
  begin
    Result := not Odd(aValue);
  end;
var
  s: TAutoSet;
begin
  s.Instance := TSet.Create(IntArray21, specialize TGDefaults<Integer>.OnCompare);
  AssertTrue(s.Instance.RemoveIf(@IsEvenInt) = 10);
  AssertTrue(s.Instance.Count = 11);
  AssertTrue(s.Instance.ContainsAll(IntArray11));
end;

procedure TDelegatedTreeSetTest.ExtractIfRegular;
var
  s: TAutoSet;
  e: TIntArray;
begin
  s.Instance := TSet.Create(IntArray21, specialize TGDefaults<Integer>.OnCompare);
  e := s.Instance.ExtractIf(@IsEvenInt);
  AssertTrue(s.Instance.Count = 11);
  AssertTrue(s.Instance.ContainsAll(IntArray11));
  AssertTrue(System.Length(e) = 10);
  AssertTrue(TIntHelper.Same(IntArray10, e));
end;

procedure TDelegatedTreeSetTest.ExtractIfDelegated;
var
  s: TAutoSet;
  e: TIntArray;
begin
  s.Instance := TSet.Create(IntArray21, specialize TGDefaults<Integer>.OnCompare);
  e := s.Instance.ExtractIf(@IsEven);
  AssertTrue(s.Instance.Count = 11);
  AssertTrue(s.Instance.ContainsAll(IntArray11));
  AssertTrue(System.Length(e) = 10);
  AssertTrue(TIntHelper.Same(IntArray10, e));
end;

procedure TDelegatedTreeSetTest.ExtractIfNested;
  function IsEvenInt(constref aValue: Integer): Boolean;
  begin
    Result := not Odd(aValue);
  end;
var
  s: TAutoSet;
  e: TIntArray;
begin
  s.Instance := TSet.Create(IntArray21, specialize TGDefaults<Integer>.OnCompare);
  e := s.Instance.ExtractIf(@IsEvenInt);
  AssertTrue(s.Instance.Count = 11);
  AssertTrue(s.Instance.ContainsAll(IntArray11));
  AssertTrue(System.Length(e) = 10);
  TIntHelper.QuickSort(e);
  AssertTrue(TIntHelper.Same(IntArray10, e));
end;

procedure TDelegatedTreeSetTest.RetainAll;
var
  s, s1: TAutoSet;
begin
  s.Instance := TSet.Create(IntArray21, specialize TGDefaults<Integer>.OnCompare);
  s1.Instance := TSet.Create(IntArray11, specialize TGDefaults<Integer>.OnCompare);
  s.Instance.AddAll([-3, -2, -1, 0]);
  AssertTrue(s.Instance.Count = 25);
  s.Instance.RetainAll(s1.Instance);
  AssertTrue(s.Instance.Count = 11);
  AssertTrue(s.Instance.ContainsAll(s1.Instance));
end;

procedure TDelegatedTreeSetTest.RetainAll_1;
var
  s, s1: TAutoSet;
begin
  s.Instance := TSet.Create(IntArray10, specialize TGDefaults<Integer>.OnCompare);
  s1.Instance := TSet.Create(IntArray11, specialize TGDefaults<Integer>.OnCompare);
  s.Instance.RetainAll(s1.Instance);
  AssertTrue(s.Instance.IsEmpty);
end;

procedure TDelegatedTreeSetTest.RetainAllSelf;
var
  s: TAutoSet;
begin
  s.Instance := TSet.Create(IntArray21, specialize TGDefaults<Integer>.OnCompare);
  AssertTrue(s.Instance.Count = 21);
  s.Instance.RetainAll(s.Instance);
  AssertTrue(s.Instance.Count = 21);
  AssertTrue(s.Instance.ContainsAll(IntArray21));
end;

procedure TDelegatedTreeSetTest.IsSuperset;
var
  s, s1: TAutoSet;
begin
  s.Instance := TSet.Create(IntArray21, specialize TGDefaults<Integer>.OnCompare);
  s1.Instance := TSet.Create(IntArray21, specialize TGDefaults<Integer>.OnCompare);
  AssertTrue(s.Instance.IsSuperset(s1.Instance));
  AssertTrue(s1.Instance.IsSuperset(s.Instance));
  AssertTrue(s.Instance.IsSuperset(s.Instance));
  s1.Instance := TSet.Create(IntArray11, specialize TGDefaults<Integer>.OnCompare);
  AssertTrue(s.Instance.IsSuperset(s1.Instance));
  AssertFalse(s1.Instance.IsSuperset(s.Instance));
end;

procedure TDelegatedTreeSetTest.IsSubset;
var
  s, s1: TAutoSet;
begin
  s.Instance := TSet.Create(IntArray21, specialize TGDefaults<Integer>.OnCompare);
  s1.Instance := TSet.Create(IntArray21, specialize TGDefaults<Integer>.OnCompare);
  AssertTrue(s.Instance.IsSubset(s1.Instance));
  AssertTrue(s1.Instance.IsSubset(s.Instance));
  AssertTrue(s.Instance.IsSubset(s.Instance));
  s1.Instance := TSet.Create(IntArray11, specialize TGDefaults<Integer>.OnCompare);
  AssertTrue(s1.Instance.IsSubset(s.Instance));
  AssertFalse(s.Instance.IsSubset(s1.Instance));
end;

procedure TDelegatedTreeSetTest.IsEqual;
var
  s, s1: TAutoSet;
begin
  s.Instance := TSet.Create(IntArray21, specialize TGDefaults<Integer>.OnCompare);
  s1.Instance := TSet.Create(IntArray21, specialize TGDefaults<Integer>.OnCompare);
  AssertTrue(s.Instance.IsEqual(s.Instance));
  AssertTrue(s.Instance.IsEqual(s1.Instance));
  s1.Instance := TSet.Create(IntArray11, specialize TGDefaults<Integer>.OnCompare);
  AssertFalse(s.Instance.IsEqual(s1.Instance));
end;

procedure TDelegatedTreeSetTest.Intersecting;
var
  s, s1: TAutoSet;
begin
  s.Instance := TSet.Create(IntArray21, specialize TGDefaults<Integer>.OnCompare);
  s1.Instance := TSet.Create(IntArray11, specialize TGDefaults<Integer>.OnCompare);
  AssertTrue(s.Instance.Intersecting(s.Instance));
  AssertTrue(s.Instance.Intersecting(s1.Instance));
  s.Instance := TSet.Create(IntArray10, specialize TGDefaults<Integer>.OnCompare);
  AssertFalse(s.Instance.Intersecting(s1.Instance));
  AssertFalse(s1.Instance.Intersecting(s.Instance));
end;

procedure TDelegatedTreeSetTest.SymmetricSubtract;
var
  s, s1: TAutoSet;
begin
  s.Instance := TSet.Create(IntArray21, specialize TGDefaults<Integer>.OnCompare);
  s1.Instance := TSet.Create(IntArray21, specialize TGDefaults<Integer>.OnCompare);
  s.Instance.SymmetricSubtract(s1.Instance);
  AssertTrue(s.Instance.IsEmpty);
  s.Instance := TSet.Create(IntArray21, specialize TGDefaults<Integer>.OnCompare);
  s1.Instance.Clear;
  s.Instance.SymmetricSubtract(s1.Instance);
  AssertTrue(s.Instance.Count = 21);
  AssertTrue(s.Instance.ContainsAll(IntArray21));
  s.Instance := TSet.Create(IntArray11, specialize TGDefaults<Integer>.OnCompare);
  s1.Instance := TSet.Create(IntArray10, specialize TGDefaults<Integer>.OnCompare);
  s.Instance.SymmetricSubtract(s1.Instance);
  AssertTrue(s.Instance.Count = 21);
  AssertTrue(s.Instance.ContainsAll(IntArray21));
  s.Instance := TSet.Create(IntArray11, specialize TGDefaults<Integer>.OnCompare);
  s1.Instance := TSet.Create(IntArray10, specialize TGDefaults<Integer>.OnCompare);
  s.Instance.AddAll([-5, -4, -3, -2, -1, 0]);
  s1.Instance.AddAll([-5, -4, -3, -2, -1, 0]);
  s.Instance.SymmetricSubtract(s1.Instance);
  AssertTrue(s.Instance.Count = 21);
  AssertTrue(s.Instance.ContainsAll(IntArray21));
end;

procedure TDelegatedTreeSetTest.Order;
var
  s: TAutoSet;
begin
  s.Instance := TSet.Create(TIntHelper.CreateRandomShuffle(IntArray21), specialize TGDefaults<Integer>.OnCompare);
  AssertTrue(TIntHelper.Same(IntArray21, s.Instance.ToArray));
  AssertTrue(TIntHelper.Same(s.Instance.Reverse.ToArray, TIntHelper.CreateReverseCopy(IntArray21)));
end;

procedure TDelegatedTreeSetTest.Order_1;
var
  s: TAutoSet;
  a: TIntArray;
  I: Integer;
begin
  System.SetLength(a, 100);
  for I := 0 to System.High(a) do
    a[I] := I;
  TIntHelper.RandomShuffle(a);
  s.Instance := TSet.Create(a, specialize TGDefaults<Integer>.OnCompare);
  AssertTrue(s.Instance.FindFirst(I));
  AssertTrue(I = 0);
  AssertTrue(s.Instance.FindLast(I));
  AssertTrue(I = 99);
  AssertTrue(s.Instance.FindMin(I));
  AssertTrue(I = 0);
  AssertTrue(s.Instance.FindMax(I));
  AssertTrue(I = 99);
end;

procedure TDelegatedTreeSetTest.Head;
var
  s: TAutoSet;
  a: TIntArray;
  I, v: Integer;
begin
  System.SetLength(a, 100);
  for I := 0 to System.High(a) do
    a[I] := I;
  TIntHelper.RandomShuffle(a);
  s.Instance := TSet.Create(a, specialize TGDefaults<Integer>.OnCompare);
  AssertTrue(s.Instance.Head(0).None);
  I := 0;
  for v in s.Instance.Head(80) do
    begin
      AssertTrue(I = v);
      Inc(I);
    end;
  AssertTrue(I = 80);
  I := 0;
  for v in s.Instance.Head(80, True) do
    begin
      AssertTrue(I = v);
      Inc(I);
    end;
  AssertTrue(I = 81);
end;

procedure TDelegatedTreeSetTest.Tail;
var
  s: TAutoSet;
  a: TIntArray;
  I, v: Integer;
begin
  System.SetLength(a, 100);
  for I := 0 to System.High(a) do
    a[I] := I;
  TIntHelper.RandomShuffle(a);
  s.Instance := TSet.Create(a, specialize TGDefaults<Integer>.OnCompare);
  AssertTrue(s.Instance.Tail(100).None);
  I := 30;
  for v in s.Instance.Tail(30) do
    begin
      AssertTrue(I = v);
      Inc(I);
    end;
  AssertTrue(I = 100);
end;

procedure TDelegatedTreeSetTest.Range;
var
  s: TAutoSet;
  a: TIntArray;
  I, v: Integer;
begin
  System.SetLength(a, 100);
  for I := 0 to System.High(a) do
    a[I] := I;
  TIntHelper.RandomShuffle(a);
  s.Instance := TSet.Create(a, specialize TGDefaults<Integer>.OnCompare);
  AssertTrue(s.Instance.Range(10, 10).None);
  AssertTrue(s.Instance.Range(100, 100).None);
  AssertTrue(s.Instance.Range(0, 0).None);
  I := 10;
  for v in s.Instance.Range(10, 90) do
    begin
      AssertTrue(I = v);
      Inc(I);
    end;
  AssertTrue(I = 90);
end;

procedure TDelegatedTreeSetTest.FindFloor;
var
  s: TAutoSet;
  I: Integer;
begin
  for I := 1 to 16 do
    s.Instance.Add(I * 3);

  AssertFalse(s.Instance.FindFloor(2, I));
  AssertFalse(s.Instance.FindFloor(3, I));
  AssertTrue(s.Instance.FindFloor(3, I, True));
  AssertTrue(I = 3);
  AssertTrue(s.Instance.FindFloor(9, I));
  AssertTrue(I = 6);
  AssertTrue(s.Instance.FindFloor(9, I, True));
  AssertTrue(I = 9);
  AssertTrue(s.Instance.FindFloor(13, I));
  AssertTrue(I = 12);
  AssertTrue(s.Instance.FindFloor(22, I));
  AssertTrue(I = 21);
  AssertTrue(s.Instance.FindFloor(27, I));
  AssertTrue(I = 24);
  AssertTrue(s.Instance.FindFloor(27, I, True));
  AssertTrue(I = 27);
end;

procedure TDelegatedTreeSetTest.FindCeil;
var
  s: TAutoSet;
  I: Integer;
begin
  for I := 1 to 16 do
    s.Instance.Add(I * 3);
  AssertTrue(s.Instance.FindCeil(48, I));
  AssertTrue(I = 48);
  AssertFalse(s.Instance.FindCeil(48, I, False));
  AssertTrue(s.Instance.FindCeil(9, I));
  AssertTrue(I = 9);
  AssertTrue(s.Instance.FindCeil(9, I, False));
  AssertTrue(I = 12);
  AssertTrue(s.Instance.FindCeil(13, I));
  AssertTrue(I = 15);
  AssertTrue(s.Instance.FindCeil(22, I));
  AssertTrue(I = 24);
  AssertTrue(s.Instance.FindCeil(27, I));
  AssertTrue(I = 27);
  AssertTrue(s.Instance.FindCeil(27, I, False));
  AssertTrue(I = 30);
end;

procedure TDelegatedTreeSetTest.ObjectSetRemove;
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

procedure TDelegatedTreeSetTest.ObjectSetRemoveArray;
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

procedure TDelegatedTreeSetTest.ObjectSetRemoveEnum;
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

procedure TDelegatedTreeSetTest.ObjectSetRemoveSelf;
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

procedure TDelegatedTreeSetTest.ObjectSetRemoveIfRegular;
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

procedure TDelegatedTreeSetTest.ObjectSetRemoveIfDelegated;
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

procedure TDelegatedTreeSetTest.ObjectSetRemoveIfNested;
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

procedure TDelegatedTreeSetTest.ObjectSetExtractIfRegular;
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

procedure TDelegatedTreeSetTest.ObjectSetExtractIfDelegated;
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

procedure TDelegatedTreeSetTest.ObjectSetExtractIfNested;
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

procedure TDelegatedTreeSetTest.ObjectSetRetain;
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

procedure TDelegatedTreeSetTest.ObjectSetSymmetricSubtract;
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

{ TLiteTreeSetTest }

function TLiteTreeSetTest.IsEven(constref aValue: Integer): Boolean;
begin
  Result := not Odd(aValue);
end;

function TLiteTreeSetTest.ObjIsEven(constref aObj: TTestObj): Boolean;
begin
  Result := not Odd(aObj.Value);
end;

procedure TLiteTreeSetTest.Clear;
var
  s: TSet;
begin
  AssertTrue(s.AddAll(IntArray21) = 21);
  AssertTrue(s.Count = 21);
  AssertTrue(s.Capacity > 21);
  s.Clear;
  AssertTrue(s.Count = 0);
  AssertTrue(s.Capacity = 0);
end;

procedure TLiteTreeSetTest.EnsureCapacity;
var
  s: TSet;
begin
  AssertTrue(s.Capacity = 0);
  s.EnsureCapacity(100);
  AssertTrue(s.Capacity >= 100);
end;

procedure TLiteTreeSetTest.Add;
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

procedure TLiteTreeSetTest.Add100;
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

procedure TLiteTreeSetTest.AddArray;
var
  s: TSet;
begin
  AssertTrue(s.AddAll(IntArray21) = Length(IntArray21));
  AssertTrue(s.Count = Length(IntArray21));
  AssertTrue(s.ContainsAll(IntArray21));
end;

procedure TLiteTreeSetTest.AddEnum;
var
  s: TSet;
  e: IEnumerable;
begin
  e := TIntArrayCursor.Create(TIntHelper.CreateCopy(IntArray21));
  AssertTrue(s.AddAll(e) = Length(IntArray21));
  AssertTrue(s.Count = Length(IntArray21));
  AssertTrue(s.ContainsAll(IntArray21));
end;

procedure TLiteTreeSetTest.AddSelf;
var
  s: TSet;
begin
  s.AddAll(IntArray21);
  AssertTrue(s.AddAll(s) = 0);
  AssertTrue(s.Count = Length(IntArray21));
end;

procedure TLiteTreeSetTest.Remove;
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

procedure TLiteTreeSetTest.RemoveArray;
var
  s: TSet;
begin
  s.AddAll(IntArray21);
  AssertTrue(s.RemoveAll(IntArray11) = 11);
  AssertTrue(s.Count = 10);
end;

procedure TLiteTreeSetTest.RemoveEnum;
var
  s: TSet;
  e: IEnumerable;
begin
  e := TIntArrayCursor.Create(TIntHelper.CreateCopy(IntArray11));
  s.AddAll(IntArray21);
  AssertTrue(s.RemoveAll(e) = 11);
  AssertTrue(s.Count = 10);
end;

procedure TLiteTreeSetTest.RemoveSelf;
var
  s: TSet;
begin
  s.AddAll(IntArray21);
  AssertTrue(s.RemoveAll(s) = 21);
  AssertTrue(s.IsEmpty);
end;

procedure TLiteTreeSetTest.RemoveIfRegular;
var
  s: TSet;
begin
  s.AddAll(IntArray21);
  AssertTrue(s.RemoveIf(@IsEvenInt) = 10);
  AssertTrue(s.Count = 11);
  AssertTrue(s.ContainsAll(IntArray11));
end;

procedure TLiteTreeSetTest.RemoveIfDelegated;
var
  s: TSet;
begin
  s.AddAll(IntArray21);
  AssertTrue(s.RemoveIf(@IsEven) = 10);
  AssertTrue(s.Count = 11);
  AssertTrue(s.ContainsAll(IntArray11));
end;

procedure TLiteTreeSetTest.RemoveIfNested;
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

procedure TLiteTreeSetTest.ExtractIfRegular;
var
  s: TSet;
  e: TIntArray;
begin
  s.AddAll(IntArray21);
  e := s.ExtractIf(@IsEvenInt);
  AssertTrue(s.Count = 11);
  AssertTrue(s.ContainsAll(IntArray11));
  AssertTrue(Length(e) = 10);
  TIntHelper.Sort(e);
  AssertTrue(TIntHelper.Same(IntArray10, e));
end;

procedure TLiteTreeSetTest.ExtractIfDelegated;
var
  s: TSet;
  e: TIntArray;
begin
  s.AddAll(IntArray21);
  e := s.ExtractIf(@IsEven);
  AssertTrue(s.Count = 11);
  AssertTrue(s.ContainsAll(IntArray11));
  AssertTrue(Length(e) = 10);
  TIntHelper.Sort(e);
  AssertTrue(TIntHelper.Same(IntArray10, e));
end;

procedure TLiteTreeSetTest.ExtractIfNested;
  function IsEvenInt(constref aValue: Integer): Boolean;
  begin
    Result := not Odd(aValue);
  end;
var
  s: TSet;
  e: TIntArray;
begin
  s.AddAll(IntArray21);
  e := s.ExtractIf(@IsEvenInt);
  AssertTrue(s.Count = 11);
  AssertTrue(s.ContainsAll(IntArray11));
  AssertTrue(Length(e) = 10);
  TIntHelper.Sort(e);
  AssertTrue(TIntHelper.Same(IntArray10, e));
end;

procedure TLiteTreeSetTest.RetainAll;
var
  s, s11: TSet;
begin
  s.AddAll(IntArray21);
  s11.AddAll(IntArray11);
  s.AddAll([-3, -2, -1, 0]);
  AssertTrue(s.Count = 25);
  s.RetainAll(s11);
  AssertTrue(s.Count = 11);
  AssertTrue(s.ContainsAll(s11));
end;

procedure TLiteTreeSetTest.RetainAll_1;
var
  s, s1: TSet;
begin
  s.AddAll(IntArray10);
  s1.AddAll(IntArray11);
  s.RetainAll(s1);
  AssertTrue(s.IsEmpty);
end;

procedure TLiteTreeSetTest.RetainAllSelf;
var
  s: TSet;
begin
  s.AddAll(IntArray21);
  AssertTrue(s.Count = 21);
  s.RetainAll(s);
  AssertTrue(s.Count = 21);
  AssertTrue(s.ContainsAll(IntArray21));
end;

procedure TLiteTreeSetTest.IsSuperset;
var
  s, s1: TSet;
begin
  s.AddAll(IntArray21);
  s1.AddAll(IntArray21);
  AssertTrue(s.IsSuperset(s1));
  AssertTrue(s1.IsSuperset(s));
  AssertTrue(s.IsSuperset(s));
  s1.RemoveAll(IntArray10);
  AssertTrue(s.IsSuperset(s1));
  AssertFalse(s1.IsSuperset(s));
end;

procedure TLiteTreeSetTest.IsSubset;
var
  s, s1: TSet;
begin
  s.AddAll(IntArray21);
  s1.AddAll(IntArray21);
  AssertTrue(s.IsSubset(s1));
  AssertTrue(s1.IsSubset(s));
  AssertTrue(s.IsSubset(s));
  s1.RemoveAll(IntArray10);
  AssertTrue(s1.IsSubset(s));
  AssertFalse(s.IsSubset(s1));
end;

procedure TLiteTreeSetTest.IsEqual;
var
  s, s1: TSet;
begin
  s.AddAll(IntArray21);
  s1.AddAll(IntArray21);
  AssertTrue(s.IsEqual(s));
  AssertTrue(s.IsEqual(s1));
  s1.RemoveAll(IntArray10);
  AssertFalse(s.IsEqual(s1));
  AssertFalse(s1.IsEqual(s));
end;

procedure TLiteTreeSetTest.Intersecting;
var
  s, s1: TSet;
begin
  s.AddAll(IntArray21);
  s1.AddAll(IntArray11);
  AssertTrue(s.Intersecting(s));
  AssertTrue(s.Intersecting(s1));
  s.RemoveAll(IntArray11);
  AssertFalse(s.Intersecting(s1));
  AssertFalse(s1.Intersecting(s));
end;

procedure TLiteTreeSetTest.SymmetricSubtract;
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
  s.RemoveAll(IntArray10);
  s1.AddAll(IntArray10);
  s.SymmetricSubtract(s1);
  AssertTrue(s.Count = 21);
  AssertTrue(s.ContainsAll(IntArray21));
  s.RemoveAll(IntArray11);
  s1.Clear;
  s1.AddAll(IntArray11);
  s.AddAll([-5, -4, -3, -2, -1, 0]);
  s1.AddAll([-5, -4, -3, -2, -1, 0]);
  s.SymmetricSubtract(s1);
  AssertTrue(s.Count = 21);
  AssertTrue(s.ContainsAll(IntArray21));
end;

procedure TLiteTreeSetTest.Order;
var
  s: TSet;
  I, J: Integer;
  a: TIntArray;
begin
  s.AddAll(TIntHelper.CreateRandomShuffle(IntArray21));
  AssertTrue(TIntHelper.Same(IntArray21, s.ToArray));
  a := TIntHelper.CreateReverseCopy(IntArray21);
  J := 0;
  for {%H-}I in s.Reverse do
    begin
      AssertTrue(a[J] = I);
      Inc(J);
    end;
end;

procedure TLiteTreeSetTest.Order_1;
var
  s: TSet;
  a: TIntArray;
  I: Integer;
begin
  SetLength(a, 100);
  for I := 0 to System.High(a) do
    a[I] := I;
  TIntHelper.RandomShuffle(a);
  s.AddAll(a);
  AssertTrue(s.FindMin(I));
  AssertTrue(I = 0);
  AssertTrue(s.FindMax(I));
  AssertTrue(I = 99);
end;

procedure TLiteTreeSetTest.Head;
var
  s: TSet;
  a: TIntArray;
  I, J: Integer;
begin
  SetLength(a, 100);
  for I := 0 to High(a) do
    a[I] := I;
  TIntHelper.RandomShuffle(a);
  s.AddAll(a);
  I := 0;
  for J in s.Head(0) do
    Inc(I);
  AssertTrue(I = 0);
  I := 0;
  for J in s.Head(80) do
    begin
      AssertTrue(I = J);
      Inc(I);
    end;
  AssertTrue(I = 80);
  I := 0;
  for J in s.Head(80, True) do
    begin
      AssertTrue(I = J);
      Inc(I);
    end;
  AssertTrue(I = 81);
end;

procedure TLiteTreeSetTest.Tail;
var
  s: TSet;
  a: TIntArray;
  I, J: Integer;
begin
  SetLength(a, 100);
  for I := 0 to High(a) do
    a[I] := I;
  TIntHelper.RandomShuffle(a);
  s.AddAll(a);
  I := 0;
  for J in s.Tail(99) do
    Inc(I);
  AssertTrue(I = 1);
  I := 0;
  for J in s.Tail(99, False) do
    Inc(I);
  AssertTrue(I = 0);
  I := 30;
  for J in s.Tail(30) do
    begin
      AssertTrue(I = J);
      Inc(I);
    end;
  AssertTrue(I = 100);
end;

procedure TLiteTreeSetTest.Range;
var
  s: TSet;
  a: TIntArray;
  I, J: Integer;
begin
  SetLength(a, 100);
  for I := 0 to High(a) do
    a[I] := I;
  TIntHelper.RandomShuffle(a);
  s.AddAll(a);
  I := 0;
  for J in s.Range(10, 10) do
    Inc(I);
  AssertTrue(I = 0);
  I := 0;
  for J in s.Range(10, 10, BOUNDS_BOTH) do
    Inc(I);
  AssertTrue(I = 1);
  I := 10;
  for J in s.Range(10, 90) do
    begin
      AssertTrue(I = J);
      Inc(I);
    end;
  AssertTrue(I = 90);
  I := 10;
  for J in s.Range(10, 90, BOUNDS_BOTH) do
    begin
      AssertTrue(I = J);
      Inc(I);
    end;
  AssertTrue(I = 91);
end;

procedure TLiteTreeSetTest.FindFloor;
var
  s: TSet;
  I: Integer;
begin
  for I := 1 to 16 do
    s.Add(I * 3);
  AssertFalse(s.FindFloor(2, I));
  AssertFalse(s.FindFloor(3, I));
  AssertTrue(s.FindFloor(3, I, True));
  AssertTrue(I = 3);
  AssertTrue(s.FindFloor(9, I));
  AssertTrue(I = 6);
  AssertTrue(s.FindFloor(9, I, True));
  AssertTrue(I = 9);
  AssertTrue(s.FindFloor(13, I));
  AssertTrue(I = 12);
  AssertTrue(s.FindFloor(22, I));
  AssertTrue(I = 21);
  AssertTrue(s.FindFloor(27, I));
  AssertTrue(I = 24);
  AssertTrue(s.FindFloor(27, I, True));
  AssertTrue(I = 27);
end;

procedure TLiteTreeSetTest.FindCeil;
var
  s: TSet;
  I: Integer;
begin
  for I := 1 to 16 do
    s.Add(I * 3);
  AssertTrue(s.FindCeil(48, I));
  AssertTrue(I = 48);
  AssertFalse(s.FindCeil(48, I, False));
  AssertTrue(s.FindCeil(9, I));
  AssertTrue(I = 9);
  AssertTrue(s.FindCeil(9, I, False));
  AssertTrue(I = 12);
  AssertTrue(s.FindCeil(13, I));
  AssertTrue(I = 15);
  AssertTrue(s.FindCeil(22, I));
  AssertTrue(I = 24);
  AssertTrue(s.FindCeil(27, I));
  AssertTrue(I = 27);
  AssertTrue(s.FindCeil(27, I, False));
  AssertTrue(I = 30);
end;

initialization
  RegisterTest(TTreeSetTest);
  RegisterTest(TComparableTreeSetTest);
  RegisterTest(TRegularTreeSetTest);
  RegisterTest(TDelegatedTreeSetTest);
  RegisterTest(TLiteTreeSetTest);
end.

