unit LGTreeMultisetTest;

{$mode objfpc}{$H+}
{$MODESWITCH NESTEDPROCVARS}

interface
uses
  SysUtils, fpcunit, testregistry,
  LGUtils,
  LGAbstractContainer,
  LGTreeMultiSet,
  LGHashSet,
  LGHelpers,
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
    function  GetIncrement: TObjProc;
  public
    property  Increment: TObjProc read GetIncrement;
    property  Count: Integer read FCount;
  end;

  TObjArray = specialize TGArray<TTestObj>;

  TBaseTreeMultisetTest = class(TTestCase)
  private
  type
    TMultiSet        = class(specialize TGTreeMultiSet<Integer>);
    TAutoMultiSet    = specialize TGAutoRef<TMultiSet>;
    TObjMultiSet     = specialize TGObjTreeMultiSet<TTestObj>;
    TAutoObjMultiSet = specialize TGAutoRef<TObjMultiSet>;

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
    procedure EnsureCapacity_1;

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

    procedure IsSuperMultiset;
    procedure IsSubMultiset;
    procedure IsEqual;
    procedure Intersecting;
    procedure Intersect;
    procedure Join;
    procedure ArithmeticAdd;
    procedure ArithmeticSubtract;
    procedure SymmetricSubtract;

    procedure Head;
    procedure Tail;
    procedure Range;
    procedure FindFloor;
    procedure FindCeil;

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

  TComparableTreeMultisetTest = class(TTestCase)
  private
  type
    TMultiSet        = class(specialize TGComparableTreeMultiSet<Integer>);
    TAutoMultiSet    = specialize TGAutoRef<TMultiSet>;

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
    procedure EnsureCapacity_1;

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

    procedure IsSuperMultiset;
    procedure IsSubMultiset;
    procedure IsEqual;
    procedure Intersecting;
    procedure Intersect;
    procedure Join;
    procedure ArithmeticAdd;
    procedure ArithmeticSubtract;
    procedure SymmetricSubtract;

    procedure Head;
    procedure Tail;
    procedure Range;
    procedure FindFloor;
    procedure FindCeil;
  end;

  TRegularTreeMultisetTest = class(TTestCase)
  private
  type
    TMultiSet        = class(specialize TGRegularTreeMultiSet<Integer>);
    TAutoMultiSet    = specialize TGAutoRef<TMultiSet>;

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
    procedure EnsureCapacity_1;

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

    procedure IsSuperMultiset;
    procedure IsSubMultiset;
    procedure IsEqual;
    procedure Intersecting;
    procedure Intersect;
    procedure Join;
    procedure ArithmeticAdd;
    procedure ArithmeticSubtract;
    procedure SymmetricSubtract;

    procedure Head;
    procedure Tail;
    procedure Range;
    procedure FindFloor;
    procedure FindCeil;
  end;

  TDelegatedTreeMultisetTest = class(TTestCase)
  private
  type
    TMultiSet        = class(specialize TGDelegatedTreeMultiSet<Integer>);
    TAutoMultiSet    = specialize TGAutoRef<TMultiSet>;

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
    procedure EnsureCapacity_1;

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

    procedure IsSuperMultiset;
    procedure IsSubMultiset;
    procedure IsEqual;
    procedure Intersecting;
    procedure Intersect;
    procedure Join;
    procedure ArithmeticAdd;
    procedure ArithmeticSubtract;
    procedure SymmetricSubtract;

    procedure Head;
    procedure Tail;
    procedure Range;
    procedure FindFloor;
    procedure FindCeil;
  end;

implementation
{$B-}{$COPERATORS ON}

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

function TBaseTreeMultisetTest.IsEven(constref aValue: Integer): Boolean;
begin
  Result := not Odd(aValue);
end;

function TBaseTreeMultisetTest.ObjIsEven(constref aObj: TTestObj): Boolean;
begin
  Result := not Odd(aObj.Value);
end;

procedure TBaseTreeMultisetTest.TestCreate;
var
  ms: TAutoMultiSet;
begin
  AssertTrue(ms.Instance.Count = 0);
  AssertTrue(ms.Instance.Capacity >= DEFAULT_CONTAINER_CAPACITY);
end;

procedure TBaseTreeMultisetTest.CreateArray;
var
  ms: TAutoMultiSet;
  I: Integer;
begin
  ms.Instance := TMultiSet.Create(IntArray11);
  AssertTrue(ms.Instance.Count = 11);
  AssertTrue(ms.Instance.Capacity >= 11);
  for I in IntArray11 do
    AssertTrue(ms.Instance.Contains(I));
end;

procedure TBaseTreeMultisetTest.CreateEnum;
var
  ms: TAutoMultiSet;
  I: Integer;
  e: IEnumerable;
begin
  e := TIntArrayCursor.Create(TIntHelper.CreateCopy(IntArray11));
  ms.Instance := TMultiSet.Create(e);
  AssertTrue(ms.Instance.Count = 11);
  for I in IntArray11 do
    AssertTrue(ms.Instance.Contains(I));
end;

procedure TBaseTreeMultisetTest.CreateCapacity0;
var
  ms: TAutoMultiSet;
begin
  ms.Instance := TMultiSet.Create(0);
  AssertTrue(ms.Instance.Count = 0);
  AssertTrue(ms.Instance.Capacity = 0);
end;

procedure TBaseTreeMultisetTest.CreateCapacity5;
var
  ms: TAutoMultiSet;
begin
  ms.Instance := TMultiSet.Create(5);
  AssertTrue(ms.Instance.Count = 0);
  AssertTrue(ms.Instance.Capacity >= 5);
end;

procedure TBaseTreeMultisetTest.Clear;
var
  ms: TAutoMultiSet;
begin
  ms.Instance := TMultiSet.Create(IntArray11);
  AssertTrue(ms.Instance.Count = 11);
  AssertTrue(ms.Instance.Capacity >= DEFAULT_CONTAINER_CAPACITY);
  ms.Instance.Clear;
  AssertTrue(ms.Instance.Count = 0);
  AssertTrue(ms.Instance.Capacity = 0);
end;

procedure TBaseTreeMultisetTest.EnsureCapacity;
var
  ms: TAutoMultiSet;
  c: SizeInt;
begin
  c := ms.Instance.Capacity;
  ms.Instance.EnsureCapacity(c - 1);
  AssertTrue(ms.Instance.Capacity = c);
end;

procedure TBaseTreeMultisetTest.EnsureCapacity_1;
var
  ms: TAutoMultiSet;
  c: SizeInt;
begin
  c := ms.Instance.Capacity;
  ms.Instance.EnsureCapacity(c + 1);
  AssertTrue(ms.Instance.Capacity > c);
end;

procedure TBaseTreeMultisetTest.Add;
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

procedure TBaseTreeMultisetTest.Add100;
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

procedure TBaseTreeMultisetTest.AddArray;
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

procedure TBaseTreeMultisetTest.AddEnum;
var
  ms: TAutoMultiSet;
  e: IEnumerable;
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

procedure TBaseTreeMultisetTest.AddSelf;
var
  ms: TAutoMultiSet;
begin
  ms.Instance.AddAll(IntArray21);
  AssertTrue(ms.Instance.AddAll(ms.Instance) = System.Length(IntArray21));
  AssertTrue(ms.Instance.Count = System.Length(IntArray21) * 2);
  AssertTrue(ms.Instance.EntryCount = System.Length(IntArray21));
end;

procedure TBaseTreeMultisetTest.Remove;
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

procedure TBaseTreeMultisetTest.RemoveArray;
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

procedure TBaseTreeMultisetTest.RemoveEnum;
var
  ms: TAutoMultiSet;
  e: IEnumerable;
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

procedure TBaseTreeMultisetTest.RemoveSelf;
var
  ms: TAutoMultiSet;
begin
  ms.Instance := TMultiSet.Create(IntArray21);
  ms.Instance.AddAll(IntArray21);
  AssertTrue(ms.Instance.RemoveAll(ms.Instance) = 42);
  AssertTrue(ms.Instance.IsEmpty);
end;

procedure TBaseTreeMultisetTest.RemoveIfRegular;
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

procedure TBaseTreeMultisetTest.RemoveIfDelegated;
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

procedure TBaseTreeMultisetTest.RemoveIfNested;
  function IsEvenInt(constref aValue: Integer): Boolean;
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

procedure TBaseTreeMultisetTest.ExtractIfRegular;
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

procedure TBaseTreeMultisetTest.ExtractIfDelegated;
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

procedure TBaseTreeMultisetTest.ExtractIfNested;
  function IsEvenInt(constref aValue: Integer): Boolean;
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

procedure TBaseTreeMultisetTest.Retain;
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

procedure TBaseTreeMultisetTest.Retain_1;
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

procedure TBaseTreeMultisetTest.RetainSelf;
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

procedure TBaseTreeMultisetTest.IsSuperMultiset;
var
  ms, ms1: TAutoMultiSet;
begin
  ms.Instance := TMultiSet.Create(IntArray21);
  ms.Instance.AddAll(IntArray21);
  ms1.Instance := TMultiSet.Create(ms.Instance);
  AssertTrue(ms.Instance.IsSuperMultiset(ms1.Instance));
  AssertTrue(ms1.Instance.IsSuperMultiset(ms.Instance));
  AssertTrue(ms.Instance.IsSuperMultiset(ms.Instance));
  ms1.Instance.Remove(IntArray21[1]);
  AssertTrue(ms.Instance.IsSuperMultiset(ms1.Instance));
  AssertFalse(ms1.Instance.IsSuperMultiset(ms.Instance));
  ms1.Instance.Clear;
  AssertTrue(ms.Instance.IsSuperMultiset(ms1.Instance));
  AssertFalse(ms1.Instance.IsSuperMultiset(ms.Instance));
  ms.Instance.Clear;
  AssertTrue(ms.Instance.IsSuperMultiset(ms1.Instance));
  AssertTrue(ms1.Instance.IsSuperMultiset(ms.Instance));
end;

procedure TBaseTreeMultisetTest.IsSubMultiset;
var
  ms, ms1: TAutoMultiSet;
begin
  ms.Instance := TMultiSet.Create(IntArray21);
  ms.Instance.AddAll(IntArray21);
  ms1.Instance := TMultiSet.Create(ms.Instance);
  AssertTrue(ms.Instance.IsSubMultiset(ms1.Instance));
  AssertTrue(ms1.Instance.IsSubMultiset(ms.Instance));
  AssertTrue(ms.Instance.IsSubMultiset(ms.Instance));
  ms1.Instance.Remove(IntArray21[1]);
  AssertFalse(ms.Instance.IsSubMultiset(ms1.Instance));
  AssertTrue(ms1.Instance.IsSubMultiset(ms.Instance));
  ms1.Instance.Clear;
  AssertFalse(ms.Instance.IsSubMultiset(ms1.Instance));
  AssertTrue(ms1.Instance.IsSubMultiset(ms.Instance));
  ms.Instance.Clear;
  AssertTrue(ms1.Instance.IsSubMultiset(ms.Instance));
  AssertTrue(ms.Instance.IsSubMultiset(ms1.Instance));
end;

procedure TBaseTreeMultisetTest.IsEqual;
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

procedure TBaseTreeMultisetTest.Intersecting;
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

procedure TBaseTreeMultisetTest.Intersect;
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

procedure TBaseTreeMultisetTest.Join;
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

procedure TBaseTreeMultisetTest.ArithmeticAdd;
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

procedure TBaseTreeMultisetTest.ArithmeticSubtract;
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

procedure TBaseTreeMultisetTest.SymmetricSubtract;
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

procedure TBaseTreeMultisetTest.Head;
var
  ms: TAutoMultiSet;
  a: TIntArray;
  I: Integer;
begin
  System.SetLength(a{%H-}, 100);
  for I := 0 to System.High(a) do
    a[I] := I;
  TIntHelper.RandomShuffle(a);
  ms.Instance := TMultiSet.Create(a);
  AssertTrue(ms.Instance.Head(0).None);
  AssertTrue(ms.Instance.Head(0, True).Total = 1);
  AssertTrue(ms.Instance.Head(80).Total = 80);
  AssertTrue(ms.Instance.Head(80, True).Total = 81);
end;

procedure TBaseTreeMultisetTest.Tail;
var
  ms: TAutoMultiSet;
  a: TIntArray;
  I: Integer;
begin
  System.SetLength(a{%H-}, 100);
  for I := 0 to System.High(a) do
    a[I] := I;
  TIntHelper.RandomShuffle(a);
  ms.Instance := TMultiSet.Create(a);
  AssertTrue(ms.Instance.Tail(99).Total = 1);
  AssertTrue(ms.Instance.Tail(99, False).None);
  AssertTrue(ms.Instance.Tail(30).Total = 70);
  AssertTrue(ms.Instance.Tail(30, False).Total = 69);
end;

procedure TBaseTreeMultisetTest.Range;
var
  ms: TAutoMultiSet;
  a: TIntArray;
  I, J: Integer;
begin
  System.SetLength(a{%H-}, 100);
  for I := 0 to System.High(a) do
    a[I] := I;
  TIntHelper.RandomShuffle(a);
  ms.Instance := TMultiSet.Create(a);
  AssertTrue(ms.Instance.Range(10, 10).None);
  AssertTrue(ms.Instance.Range(100, 100).None);
  AssertTrue(ms.Instance.Range(0, 0).None);
  I := 10;
  for J in ms.Instance.Range(10, 90) do
    begin
      AssertTrue(I = J);
      Inc(I);
    end;
  AssertTrue(I = 90);
end;

procedure TBaseTreeMultisetTest.FindFloor;
var
  ms: TAutoMultiSet;
  I: Integer;
begin
  for I := 1 to 16 do
    ms.Instance.Add(I * 3);
  AssertFalse(ms.Instance.FindFloor(2, I));
  AssertFalse(ms.Instance.FindFloor(3, I));
  AssertTrue(ms.Instance.FindFloor(3, I, True));
  AssertTrue(I = 3);
  AssertTrue(ms.Instance.FindFloor(9, I));
  AssertTrue(I = 6);
  AssertTrue(ms.Instance.FindFloor(9, I, True));
  AssertTrue(I = 9);
  AssertTrue(ms.Instance.FindFloor(13, I));
  AssertTrue(I = 12);
  AssertTrue(ms.Instance.FindFloor(22, I));
  AssertTrue(I = 21);
  AssertTrue(ms.Instance.FindFloor(27, I));
  AssertTrue(I = 24);
  AssertTrue(ms.Instance.FindFloor(27, I, True));
  AssertTrue(I = 27);
end;

procedure TBaseTreeMultisetTest.FindCeil;
var
  ms: TAutoMultiSet;
  I: Integer;
begin
  for I := 1 to 16 do
    ms.Instance.Add(I * 3);
  AssertTrue(ms.Instance.FindCeil(48, I));
  AssertTrue(I = 48);
  AssertFalse(ms.Instance.FindCeil(48, I, False));
  AssertTrue(ms.Instance.FindCeil(9, I));
  AssertTrue(I = 9);
  AssertTrue(ms.Instance.FindCeil(9, I, False));
  AssertTrue(I = 12);
  AssertTrue(ms.Instance.FindCeil(13, I));
  AssertTrue(I = 15);
  AssertTrue(ms.Instance.FindCeil(22, I));
  AssertTrue(I = 24);
  AssertTrue(ms.Instance.FindCeil(27, I));
  AssertTrue(I = 27);
  AssertTrue(ms.Instance.FindCeil(27, I, False));
  AssertTrue(I = 30);
end;

procedure TBaseTreeMultisetTest.ObjectMultiSetRemove;
var
  ms: TAutoObjMultiSet;
  a: TObjArray;
  I, TestSize: Integer;
  Counter: TCounter;
begin
  TestSize := 36;
  Counter := Default(TCounter);
  System.SetLength(a{%H-}, TestSize);
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

procedure TBaseTreeMultisetTest.ObjectMultiSetRemoveArray;
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

procedure TBaseTreeMultisetTest.ObjectMultiSetRemoveEnum;
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

procedure TBaseTreeMultisetTest.ObjectMultiSetRemoveSelf;
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

procedure TBaseTreeMultisetTest.ObjectMultiSetRemoveIfRegular;
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

procedure TBaseTreeMultisetTest.ObjectMultiSetRemoveIfDelegated;
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

procedure TBaseTreeMultisetTest.ObjectMultiSetRemoveIfNested;
  function IsEvenNest(constref aObj: TTestObj): Boolean;
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

procedure TBaseTreeMultisetTest.ObjectMultiSetExtractIfRegular;
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

procedure TBaseTreeMultisetTest.ObjectMultiSetExtractIfDelegated;
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

procedure TBaseTreeMultisetTest.ObjectMultiSetExtractIfNested;
  function IsEvenNest(constref aObj: TTestObj): Boolean;
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

procedure TBaseTreeMultisetTest.ObjectMultiSetRetain;
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
  System.SetLength(a{%H-}, TestSize);
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

procedure TBaseTreeMultisetTest.ObjectMultiSetIntersect;
var
  ms, ms1: TAutoObjMultiSet;
  I, TestSize: Integer;
  Counter: TCounter;
  a: TObjArray;
  o: TTestObj;
begin
  TestSize := 50;
  Counter := Default(TCounter);
  System.SetLength(a{%H-}, TestSize);
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

procedure TBaseTreeMultisetTest.ObjectMultiSetArithmeticSubtract;
var
  ms, ms1: TAutoObjMultiSet;
  I, TestSize: Integer;
  Counter: TCounter;
  a: TObjArray;
  o: TTestObj;
begin
  TestSize := 55;
  Counter := Default(TCounter);
  System.SetLength(a{%H-}, TestSize);
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

procedure TBaseTreeMultisetTest.ObjectMultiSetSymmetricSubtract;
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

function TComparableTreeMultisetTest.IsEven(constref aValue: Integer): Boolean;
begin
  Result := not Odd(aValue);
end;

function TComparableTreeMultisetTest.ObjIsEven(constref aObj: TTestObj): Boolean;
begin
  Result := not Odd(aObj.Value);
end;

procedure TComparableTreeMultisetTest.TestCreate;
var
  ms: TAutoMultiSet;
begin
  AssertTrue(ms.Instance.Count = 0);
  AssertTrue(ms.Instance.Capacity >= DEFAULT_CONTAINER_CAPACITY);
end;

procedure TComparableTreeMultisetTest.CreateArray;
var
  ms: TAutoMultiSet;
  I: Integer;
begin
  ms.Instance := TMultiSet.Create(IntArray11);
  AssertTrue(ms.Instance.Count = 11);
  AssertTrue(ms.Instance.Capacity >= 11);
  for I in IntArray11 do
    AssertTrue(ms.Instance.Contains(I));
end;

procedure TComparableTreeMultisetTest.CreateEnum;
var
  ms: TAutoMultiSet;
  I: Integer;
  e: IEnumerable;
begin
  e := TIntArrayCursor.Create(TIntHelper.CreateCopy(IntArray11));
  ms.Instance := TMultiSet.Create(e);
  AssertTrue(ms.Instance.Count = 11);
  for I in IntArray11 do
    AssertTrue(ms.Instance.Contains(I));
end;

procedure TComparableTreeMultisetTest.CreateCapacity0;
var
  ms: TAutoMultiSet;
begin
  ms.Instance := TMultiSet.Create(0);
  AssertTrue(ms.Instance.Count = 0);
  AssertTrue(ms.Instance.Capacity = 0);
end;

procedure TComparableTreeMultisetTest.CreateCapacity5;
var
  ms: TAutoMultiSet;
begin
  ms.Instance := TMultiSet.Create(5);
  AssertTrue(ms.Instance.Count = 0);
  AssertTrue(ms.Instance.Capacity >= 5);
end;

procedure TComparableTreeMultisetTest.Clear;
var
  ms: TAutoMultiSet;
begin
  ms.Instance := TMultiSet.Create(IntArray11);
  AssertTrue(ms.Instance.Count = 11);
  AssertTrue(ms.Instance.Capacity >= DEFAULT_CONTAINER_CAPACITY);
  ms.Instance.Clear;
  AssertTrue(ms.Instance.Count = 0);
  AssertTrue(ms.Instance.Capacity = 0);
end;

procedure TComparableTreeMultisetTest.EnsureCapacity;
var
  ms: TAutoMultiSet;
  c: SizeInt;
begin
  c := ms.Instance.Capacity;
  ms.Instance.EnsureCapacity(c - 1);
  AssertTrue(ms.Instance.Capacity = c);
end;

procedure TComparableTreeMultisetTest.EnsureCapacity_1;
var
  ms: TAutoMultiSet;
  c: SizeInt;
begin
  c := ms.Instance.Capacity;
  ms.Instance.EnsureCapacity(c + 1);
  AssertTrue(ms.Instance.Capacity > c);
end;

procedure TComparableTreeMultisetTest.Add;
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

procedure TComparableTreeMultisetTest.Add100;
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
  ms.Instance[20] := 52;
  AssertTrue(ms.Instance.Count = 250);
  ms.Instance[20] := 12;
  AssertTrue(ms.Instance.Count = 210);
end;

procedure TComparableTreeMultisetTest.AddArray;
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

procedure TComparableTreeMultisetTest.AddEnum;
var
  ms: TAutoMultiSet;
  e: IEnumerable;
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

procedure TComparableTreeMultisetTest.AddSelf;
var
  ms: TAutoMultiSet;
begin
  ms.Instance.AddAll(IntArray21);
  AssertTrue(ms.Instance.AddAll(ms.Instance) = System.Length(IntArray21));
  AssertTrue(ms.Instance.Count = System.Length(IntArray21) * 2);
  AssertTrue(ms.Instance.EntryCount = System.Length(IntArray21));
end;

procedure TComparableTreeMultisetTest.TestRemove;
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

procedure TComparableTreeMultisetTest.RemoveArray;
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

procedure TComparableTreeMultisetTest.RemoveEnum;
var
  ms: TAutoMultiSet;
  e: IEnumerable;
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

procedure TComparableTreeMultisetTest.RemoveSelf;
var
  ms: TAutoMultiSet;
begin
  ms.Instance := TMultiSet.Create(IntArray21);
  ms.Instance.AddAll(IntArray21);
  AssertTrue(ms.Instance.RemoveAll(ms.Instance) = 42);
  AssertTrue(ms.Instance.IsEmpty);
end;

procedure TComparableTreeMultisetTest.RemoveIfRegular;
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

procedure TComparableTreeMultisetTest.RemoveIfDelegated;
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

procedure TComparableTreeMultisetTest.RemoveIfNested;
  function IsEvenInt(constref aValue: Integer): Boolean;
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

procedure TComparableTreeMultisetTest.ExtractIfRegular;
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

procedure TComparableTreeMultisetTest.ExtractIfDelegated;
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

procedure TComparableTreeMultisetTest.ExtractIfNested;
  function IsEvenInt(constref aValue: Integer): Boolean;
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

procedure TComparableTreeMultisetTest.Retain;
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

procedure TComparableTreeMultisetTest.Retain_1;
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

procedure TComparableTreeMultisetTest.RetainSelf;
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

procedure TComparableTreeMultisetTest.IsSuperMultiset;
var
  ms, ms1: TAutoMultiSet;
begin
  ms.Instance := TMultiSet.Create(IntArray21);
  ms.Instance.AddAll(IntArray21);
  ms1.Instance := TMultiSet.Create(ms.Instance);
  AssertTrue(ms.Instance.IsSuperMultiset(ms1.Instance));
  AssertTrue(ms1.Instance.IsSuperMultiset(ms.Instance));
  AssertTrue(ms.Instance.IsSuperMultiset(ms.Instance));
  ms1.Instance.Remove(IntArray21[1]);
  AssertTrue(ms.Instance.IsSuperMultiset(ms1.Instance));
  AssertFalse(ms1.Instance.IsSuperMultiset(ms.Instance));
  ms1.Instance.Clear;
  AssertTrue(ms.Instance.IsSuperMultiset(ms1.Instance));
  AssertFalse(ms1.Instance.IsSuperMultiset(ms.Instance));
  ms.Instance.Clear;
  AssertTrue(ms.Instance.IsSuperMultiset(ms1.Instance));
  AssertTrue(ms1.Instance.IsSuperMultiset(ms.Instance));
end;

procedure TComparableTreeMultisetTest.IsSubMultiset;
var
  ms, ms1: TAutoMultiSet;
begin
  ms.Instance := TMultiSet.Create(IntArray21);
  ms.Instance.AddAll(IntArray21);
  ms1.Instance := TMultiSet.Create(ms.Instance);
  AssertTrue(ms.Instance.IsSubMultiset(ms1.Instance));
  AssertTrue(ms1.Instance.IsSubMultiset(ms.Instance));
  AssertTrue(ms.Instance.IsSubMultiset(ms.Instance));
  ms1.Instance.Remove(IntArray21[1]);
  AssertFalse(ms.Instance.IsSubMultiset(ms1.Instance));
  AssertTrue(ms1.Instance.IsSubMultiset(ms.Instance));
  ms1.Instance.Clear;
  AssertFalse(ms.Instance.IsSubMultiset(ms1.Instance));
  AssertTrue(ms1.Instance.IsSubMultiset(ms.Instance));
  ms.Instance.Clear;
  AssertTrue(ms1.Instance.IsSubMultiset(ms.Instance));
  AssertTrue(ms.Instance.IsSubMultiset(ms1.Instance));
end;

procedure TComparableTreeMultisetTest.IsEqual;
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

procedure TComparableTreeMultisetTest.Intersecting;
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

procedure TComparableTreeMultisetTest.Intersect;
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

procedure TComparableTreeMultisetTest.Join;
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

procedure TComparableTreeMultisetTest.ArithmeticAdd;
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

procedure TComparableTreeMultisetTest.ArithmeticSubtract;
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

procedure TComparableTreeMultisetTest.SymmetricSubtract;
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

procedure TComparableTreeMultisetTest.Head;
var
  ms: TAutoMultiSet;
  a: TIntArray;
  I: Integer;
begin
  System.SetLength(a{%H-}, 100);
  for I := 0 to System.High(a) do
    a[I] := I;
  TIntHelper.RandomShuffle(a);
  ms.Instance := TMultiSet.Create(a);
  AssertTrue(ms.Instance.Head(0).None);
  AssertTrue(ms.Instance.Head(0, True).Total = 1);
  AssertTrue(ms.Instance.Head(80).Total = 80);
  AssertTrue(ms.Instance.Head(80, True).Total = 81);
end;

procedure TComparableTreeMultisetTest.Tail;
var
  ms: TAutoMultiSet;
  a: TIntArray;
  I: Integer;
begin
  System.SetLength(a{%H-}, 100);
  for I := 0 to System.High(a) do
    a[I] := I;
  TIntHelper.RandomShuffle(a);
  ms.Instance := TMultiSet.Create(a);
  AssertTrue(ms.Instance.Tail(99).Total = 1);
  AssertTrue(ms.Instance.Tail(99, False).None);
  AssertTrue(ms.Instance.Tail(30).Total = 70);
  AssertTrue(ms.Instance.Tail(30, False).Total = 69);
end;

procedure TComparableTreeMultisetTest.Range;
var
  ms: TAutoMultiSet;
  a: TIntArray;
  I, J: Integer;
begin
  System.SetLength(a{%H-}, 100);
  for I := 0 to System.High(a) do
    a[I] := I;
  TIntHelper.RandomShuffle(a);
  ms.Instance := TMultiSet.Create(a);
  AssertTrue(ms.Instance.Range(10, 10).None);
  AssertTrue(ms.Instance.Range(100, 100).None);
  AssertTrue(ms.Instance.Range(0, 0).None);
  I := 10;
  for J in ms.Instance.Range(10, 90) do
    begin
      AssertTrue(I = J);
      Inc(I);
    end;
  AssertTrue(I = 90);
end;

procedure TComparableTreeMultisetTest.FindFloor;
var
  ms: TAutoMultiSet;
  I: Integer;
begin
  for I := 1 to 16 do
    ms.Instance.Add(I * 3);
  AssertFalse(ms.Instance.FindFloor(2, I));
  AssertFalse(ms.Instance.FindFloor(3, I));
  AssertTrue(ms.Instance.FindFloor(3, I, True));
  AssertTrue(I = 3);
  AssertTrue(ms.Instance.FindFloor(9, I));
  AssertTrue(I = 6);
  AssertTrue(ms.Instance.FindFloor(9, I, True));
  AssertTrue(I = 9);
  AssertTrue(ms.Instance.FindFloor(13, I));
  AssertTrue(I = 12);
  AssertTrue(ms.Instance.FindFloor(22, I));
  AssertTrue(I = 21);
  AssertTrue(ms.Instance.FindFloor(27, I));
  AssertTrue(I = 24);
  AssertTrue(ms.Instance.FindFloor(27, I, True));
  AssertTrue(I = 27);
end;

procedure TComparableTreeMultisetTest.FindCeil;
var
  ms: TAutoMultiSet;
  I: Integer;
begin
  for I := 1 to 16 do
    ms.Instance.Add(I * 3);
  AssertTrue(ms.Instance.FindCeil(48, I));
  AssertTrue(I = 48);
  AssertFalse(ms.Instance.FindCeil(48, I, False));
  AssertTrue(ms.Instance.FindCeil(9, I));
  AssertTrue(I = 9);
  AssertTrue(ms.Instance.FindCeil(9, I, False));
  AssertTrue(I = 12);
  AssertTrue(ms.Instance.FindCeil(13, I));
  AssertTrue(I = 15);
  AssertTrue(ms.Instance.FindCeil(22, I));
  AssertTrue(I = 24);
  AssertTrue(ms.Instance.FindCeil(27, I));
  AssertTrue(I = 27);
  AssertTrue(ms.Instance.FindCeil(27, I, False));
  AssertTrue(I = 30);
end;

function TRegularTreeMultisetTest.IsEven(constref aValue: Integer): Boolean;
begin
  Result := not Odd(aValue);
end;

function TRegularTreeMultisetTest.ObjIsEven(constref aObj: TTestObj): Boolean;
begin
  Result := not Odd(aObj.Value);
end;

procedure TRegularTreeMultisetTest.TestCreate;
var
  ms: TAutoMultiSet;
begin
  AssertTrue(ms.Instance.Count = 0);
  AssertTrue(ms.Instance.Capacity >= DEFAULT_CONTAINER_CAPACITY);
end;

procedure TRegularTreeMultisetTest.CreateArray;
var
  ms: TAutoMultiSet;
  I: Integer;
begin
  ms.Instance := TMultiSet.Create(IntArray11, specialize TGDefaults<Integer>.Compare);
  AssertTrue(ms.Instance.Count = 11);
  AssertTrue(ms.Instance.Capacity >= 11);
  for I in IntArray11 do
    AssertTrue(ms.Instance.Contains(I));
end;

procedure TRegularTreeMultisetTest.CreateEnum;
var
  ms: TAutoMultiSet;
  I: Integer;
  e: IEnumerable;
begin
  e := TIntArrayCursor.Create(TIntHelper.CreateCopy(IntArray11));
  ms.Instance := TMultiSet.Create(e, specialize TGDefaults<Integer>.Compare);
  AssertTrue(ms.Instance.Count = 11);
  for I in IntArray11 do
    AssertTrue(ms.Instance.Contains(I));
end;

procedure TRegularTreeMultisetTest.CreateCapacity0;
var
  ms: TAutoMultiSet;
begin
  ms.Instance := TMultiSet.Create(0, specialize TGDefaults<Integer>.Compare);
  AssertTrue(ms.Instance.Count = 0);
  AssertTrue(ms.Instance.Capacity = 0);
end;

procedure TRegularTreeMultisetTest.CreateCapacity5;
var
  ms: TAutoMultiSet;
begin
  ms.Instance := TMultiSet.Create(5, specialize TGDefaults<Integer>.Compare);
  AssertTrue(ms.Instance.Count = 0);
  AssertTrue(ms.Instance.Capacity >= 5);
end;

procedure TRegularTreeMultisetTest.Clear;
var
  ms: TAutoMultiSet;
begin
  ms.Instance := TMultiSet.Create(IntArray11, specialize TGDefaults<Integer>.Compare);
  AssertTrue(ms.Instance.Count = 11);
  AssertTrue(ms.Instance.Capacity >= DEFAULT_CONTAINER_CAPACITY);
  ms.Instance.Clear;
  AssertTrue(ms.Instance.Count = 0);
  AssertTrue(ms.Instance.Capacity = 0);
end;

procedure TRegularTreeMultisetTest.EnsureCapacity;
var
  ms: TAutoMultiSet;
  c: SizeInt;
begin
  c := ms.Instance.Capacity;
  ms.Instance.EnsureCapacity(c - 1);
  AssertTrue(ms.Instance.Capacity = c);
end;

procedure TRegularTreeMultisetTest.EnsureCapacity_1;
var
  ms: TAutoMultiSet;
  c: SizeInt;
begin
  c := ms.Instance.Capacity;
  ms.Instance.EnsureCapacity(c + 1);
  AssertTrue(ms.Instance.Capacity > c);
end;

procedure TRegularTreeMultisetTest.Add;
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

procedure TRegularTreeMultisetTest.Add100;
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
  ms.Instance[18] := 52;
  AssertTrue(ms.Instance.Count = 250);
  ms.Instance[18] := 12;
  AssertTrue(ms.Instance.Count = 210);
end;

procedure TRegularTreeMultisetTest.AddArray;
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

procedure TRegularTreeMultisetTest.AddEnum;
var
  ms: TAutoMultiSet;
  e: IEnumerable;
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

procedure TRegularTreeMultisetTest.AddSelf;
var
  ms: TAutoMultiSet;
begin
  ms.Instance.AddAll(IntArray21);
  AssertTrue(ms.Instance.AddAll(ms.Instance) = System.Length(IntArray21));
  AssertTrue(ms.Instance.Count = System.Length(IntArray21) * 2);
  AssertTrue(ms.Instance.EntryCount = System.Length(IntArray21));
end;

procedure TRegularTreeMultisetTest.Remove;
var
  ms: TAutoMultiSet;
  c: SizeInt;
begin
  c := System.Length(IntArray21) * 2;
  ms.Instance := TMultiSet.Create(IntArray21, specialize TGDefaults<Integer>.Compare);
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

procedure TRegularTreeMultisetTest.RemoveArray;
var
  ms: TAutoMultiSet;
begin
  ms.Instance := TMultiSet.Create(IntArray21, specialize TGDefaults<Integer>.Compare);
  ms.Instance.AddAll(IntArray21);
  AssertTrue(ms.Instance.RemoveAll(IntArray11) = 11);
  AssertTrue(ms.Instance.Count = 31);
  AssertTrue(ms.Instance.EntryCount = 21);
  AssertTrue(ms.Instance.RemoveAll(IntArray10) = 10);
  AssertTrue(ms.Instance.Count = 21);
  AssertTrue(ms.Instance.EntryCount = 21);
end;

procedure TRegularTreeMultisetTest.RemoveEnum;
var
  ms: TAutoMultiSet;
  e: IEnumerable;
begin
  ms.Instance := TMultiSet.Create(IntArray21, specialize TGDefaults<Integer>.Compare);
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

procedure TRegularTreeMultisetTest.RemoveSelf;
var
  ms: TAutoMultiSet;
begin
  ms.Instance := TMultiSet.Create(IntArray21, specialize TGDefaults<Integer>.Compare);
  ms.Instance.AddAll(IntArray21);
  AssertTrue(ms.Instance.RemoveAll(ms.Instance) = 42);
  AssertTrue(ms.Instance.IsEmpty);
end;

procedure TRegularTreeMultisetTest.RemoveIfRegular;
var
  ms: TAutoMultiSet;
  I: Integer;
begin
  ms.Instance := TMultiSet.Create(IntArray21, specialize TGDefaults<Integer>.Compare);
  ms.Instance.AddAll(IntArray21);
  AssertTrue(ms.Instance.RemoveIf(@IsEvenInt) = 20);
  AssertTrue(ms.Instance.Count = 22);
  AssertTrue(ms.Instance.ContainsAll(IntArray11));
  for I in IntArray11 do
    AssertTrue(ms.Instance[I] = 2);
end;

procedure TRegularTreeMultisetTest.RemoveIfDelegated;
var
  ms: TAutoMultiSet;
  I: Integer;
begin
  ms.Instance := TMultiSet.Create(IntArray21, specialize TGDefaults<Integer>.Compare);
  ms.Instance.AddAll(IntArray21);
  AssertTrue(ms.Instance.RemoveIf(@IsEven) = 20);
  AssertTrue(ms.Instance.Count = 22);
  AssertTrue(ms.Instance.ContainsAll(IntArray11));
  for I in IntArray11 do
    AssertTrue(ms.Instance[I] = 2);
end;

procedure TRegularTreeMultisetTest.RemoveIfNested;
  function IsEvenInt(constref aValue: Integer): Boolean;
  begin
    Result := not Odd(aValue);
  end;
var
  ms: TAutoMultiSet;
  I: Integer;
begin
  ms.Instance := TMultiSet.Create(IntArray21, specialize TGDefaults<Integer>.Compare);
  ms.Instance.AddAll(IntArray21);
  AssertTrue(ms.Instance.RemoveIf(@IsEvenInt) = 20);
  AssertTrue(ms.Instance.Count = 22);
  AssertTrue(ms.Instance.ContainsAll(IntArray11));
  for I in IntArray11 do
    AssertTrue(ms.Instance[I] = 2);
end;

procedure TRegularTreeMultisetTest.ExtractIfRegular;
var
  ms, ms1: TAutoMultiSet;
  e: TIntArray;
  I: Integer;
begin
  ms.Instance := TMultiSet.Create(IntArray21, specialize TGDefaults<Integer>.Compare);
  ms.Instance.AddAll(IntArray21);
  e := ms.Instance.ExtractIf(@IsEvenInt);
  AssertTrue(ms.Instance.Count = 22);
  AssertTrue(ms.Instance.ContainsAll(IntArray11));
  AssertTrue(System.Length(e) = 20);
  ms1.Instance := TMultiSet.Create(e, specialize TGDefaults<Integer>.Compare);
  for I in IntArray11 do
    AssertTrue(ms.Instance[I] = 2);
  for I in IntArray10 do
    AssertTrue(ms1.Instance[I] = 2);
end;

procedure TRegularTreeMultisetTest.ExtractIfDelegated;
var
  ms, ms1: TAutoMultiSet;
  e: TIntArray;
  I: Integer;
begin
  ms.Instance := TMultiSet.Create(IntArray21, specialize TGDefaults<Integer>.Compare);
  ms.Instance.AddAll(IntArray21);
  e := ms.Instance.ExtractIf(@IsEven);
  AssertTrue(ms.Instance.Count = 22);
  AssertTrue(ms.Instance.ContainsAll(IntArray11));
  AssertTrue(System.Length(e) = 20);
  ms1.Instance := TMultiSet.Create(e, specialize TGDefaults<Integer>.Compare);
  for I in IntArray11 do
    AssertTrue(ms.Instance[I] = 2);
  for I in IntArray10 do
    AssertTrue(ms1.Instance[I] = 2);
end;

procedure TRegularTreeMultisetTest.ExtractIfNested;
  function IsEvenInt(constref aValue: Integer): Boolean;
  begin
    Result := not Odd(aValue);
  end;
var
  ms, ms1: TAutoMultiSet;
  e: TIntArray;
  I: Integer;
begin
  ms.Instance := TMultiSet.Create(IntArray21, specialize TGDefaults<Integer>.Compare);
  ms.Instance.AddAll(IntArray21);
  e := ms.Instance.ExtractIf(@IsEvenInt);
  AssertTrue(ms.Instance.Count = 22);
  AssertTrue(ms.Instance.ContainsAll(IntArray11));
  AssertTrue(System.Length(e) = 20);
  ms1.Instance := TMultiSet.Create(e, specialize TGDefaults<Integer>.Compare);
  for I in IntArray11 do
    AssertTrue(ms.Instance[I] = 2);
  for I in IntArray10 do
    AssertTrue(ms1.Instance[I] = 2);
end;

procedure TRegularTreeMultisetTest.Retain;
var
  ms, ms1: TAutoMultiSet;
  I: Integer;
begin
  ms.Instance := TMultiSet.Create(IntArray21, specialize TGDefaults<Integer>.Compare);
  ms.Instance.AddAll(IntArray21);
  ms1.Instance := TMultiSet.Create(IntArray11, specialize TGDefaults<Integer>.Compare);
  AssertTrue(ms.Instance.Count = 42);
  ms.Instance.RetainAll(ms1.Instance);
  AssertTrue(ms.Instance.Count = 22);
  AssertTrue(ms.Instance.ContainsAll(ms1.Instance));
  for I in IntArray11 do
    AssertTrue(ms.Instance[I] = 2);
end;

procedure TRegularTreeMultisetTest.Retain_1;
var
  ms, ms1: TAutoMultiSet;
begin
  ms.Instance := TMultiSet.Create(IntArray10, specialize TGDefaults<Integer>.Compare);
  ms.Instance.AddAll(IntArray10);
  AssertTrue(ms.Instance.Count = 20);
  ms1.Instance := TMultiSet.Create(IntArray11, specialize TGDefaults<Integer>.Compare);
  ms.Instance.RetainAll(ms1.Instance);
  AssertTrue(ms.Instance.IsEmpty);
end;

procedure TRegularTreeMultisetTest.RetainSelf;
var
  ms: TAutoMultiSet;
begin
  ms.Instance := TMultiSet.Create(IntArray21, specialize TGDefaults<Integer>.Compare);
  ms.Instance.AddAll(IntArray21);
  AssertTrue(ms.Instance.Count = 42);
  ms.Instance.RetainAll(ms.Instance);
  AssertTrue(ms.Instance.Count = 42);
  AssertTrue(ms.Instance.ContainsAll(IntArray21));
end;

procedure TRegularTreeMultisetTest.IsSuperMultiset;
var
  ms, ms1: TAutoMultiSet;
begin
  ms.Instance := TMultiSet.Create(IntArray21, specialize TGDefaults<Integer>.Compare);
  ms.Instance.AddAll(IntArray21);
  ms1.Instance := TMultiSet.Create(ms.Instance, specialize TGDefaults<Integer>.Compare);
  AssertTrue(ms.Instance.IsSuperMultiset(ms1.Instance));
  AssertTrue(ms1.Instance.IsSuperMultiset(ms.Instance));
  AssertTrue(ms.Instance.IsSuperMultiset(ms.Instance));
  ms1.Instance.Remove(IntArray21[1]);
  AssertTrue(ms.Instance.IsSuperMultiset(ms1.Instance));
  AssertFalse(ms1.Instance.IsSuperMultiset(ms.Instance));
  ms1.Instance.Clear;
  AssertTrue(ms.Instance.IsSuperMultiset(ms1.Instance));
  AssertFalse(ms1.Instance.IsSuperMultiset(ms.Instance));
  ms.Instance.Clear;
  AssertTrue(ms.Instance.IsSuperMultiset(ms1.Instance));
  AssertTrue(ms1.Instance.IsSuperMultiset(ms.Instance));
end;

procedure TRegularTreeMultisetTest.IsSubMultiset;
var
  ms, ms1: TAutoMultiSet;
begin
  ms.Instance := TMultiSet.Create(IntArray21, specialize TGDefaults<Integer>.Compare);
  ms.Instance.AddAll(IntArray21);
  ms1.Instance := TMultiSet.Create(ms.Instance, specialize TGDefaults<Integer>.Compare);
  AssertTrue(ms.Instance.IsSubMultiset(ms1.Instance));
  AssertTrue(ms1.Instance.IsSubMultiset(ms.Instance));
  AssertTrue(ms.Instance.IsSubMultiset(ms.Instance));
  ms1.Instance.Remove(IntArray21[1]);
  AssertFalse(ms.Instance.IsSubMultiset(ms1.Instance));
  AssertTrue(ms1.Instance.IsSubMultiset(ms.Instance));
  ms1.Instance.Clear;
  AssertFalse(ms.Instance.IsSubMultiset(ms1.Instance));
  AssertTrue(ms1.Instance.IsSubMultiset(ms.Instance));
  ms.Instance.Clear;
  AssertTrue(ms1.Instance.IsSubMultiset(ms.Instance));
  AssertTrue(ms.Instance.IsSubMultiset(ms1.Instance));
end;

procedure TRegularTreeMultisetTest.IsEqual;
var
  ms, ms1: TAutoMultiSet;
begin
  ms.Instance := TMultiSet.Create(IntArray21, specialize TGDefaults<Integer>.Compare);
  ms.Instance.AddAll(IntArray21);
  ms1.Instance := TMultiSet.Create(ms.Instance, specialize TGDefaults<Integer>.Compare);
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

procedure TRegularTreeMultisetTest.Intersecting;
var
  ms, ms1: TAutoMultiSet;
begin
  ms.Instance := TMultiSet.Create(IntArray21, specialize TGDefaults<Integer>.Compare);
  ms1.Instance := TMultiSet.Create(IntArray11, specialize TGDefaults<Integer>.Compare);
  AssertTrue(ms.Instance.Intersecting(ms.Instance));
  AssertTrue(ms.Instance.Intersecting(ms1.Instance));
  ms.Instance := TMultiSet.Create(IntArray10, specialize TGDefaults<Integer>.Compare);
  AssertFalse(ms.Instance.Intersecting(ms1.Instance));
  AssertFalse(ms1.Instance.Intersecting(ms.Instance));
end;

procedure TRegularTreeMultisetTest.Intersect;
var
  ms, ms1: TAutoMultiSet;
  I: Integer;
begin
  ms.Instance := TMultiSet.Create(IntArray21, specialize TGDefaults<Integer>.Compare);
  ms.Instance.AddAll(IntArray21);
  AssertTrue(ms.Instance.Count = 42);
  ms1.Instance := TMultiSet.Create(IntArray11, specialize TGDefaults<Integer>.Compare);
  ms1.Instance.AddAll([-4, -3, -2, -1, 0]);
  ms.Instance.Intersect(ms1.Instance);
  AssertTrue(ms.Instance.Count = 11);
  for I in IntArray11 do
    AssertTrue(ms.Instance[I] = 1);
end;

procedure TRegularTreeMultisetTest.Join;
var
  ms, ms1: TAutoMultiSet;
  I: Integer;
begin
  ms1.Instance := TMultiSet.Create(IntArray11, specialize TGDefaults<Integer>.Compare);
  ms.Instance.Join(ms1.Instance);
  AssertTrue(ms.Instance.IsEqual(ms1.Instance));

  ms1.Instance := TMultiSet.Create(IntArray10, specialize TGDefaults<Integer>.Compare);
  ms1.Instance.AddAll(ms1.Instance);

  ms.Instance.Join(ms1.Instance);
  for I in IntArray11 do
    AssertTrue(ms.Instance[I] = 1);
  for I in IntArray10 do
    AssertTrue(ms.Instance[I] = 2);
end;

procedure TRegularTreeMultisetTest.ArithmeticAdd;
var
  ms, ms1: TAutoMultiSet;
  I: Integer;
begin
  ms1.Instance := TMultiSet.Create(IntArray11, specialize TGDefaults<Integer>.Compare);
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

procedure TRegularTreeMultisetTest.ArithmeticSubtract;
var
  ms, ms1: TAutoMultiSet;
begin
  ms1.Instance := TMultiSet.Create(IntArray11, specialize TGDefaults<Integer>.Compare);
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

procedure TRegularTreeMultisetTest.SymmetricSubtract;
var
  ms, ms1: TAutoMultiSet;
begin
  ms1.Instance := TMultiSet.Create(IntArray11, specialize TGDefaults<Integer>.Compare);
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

procedure TRegularTreeMultisetTest.Head;
var
  ms: TAutoMultiSet;
  a: TIntArray;
  I: Integer;
begin
  System.SetLength(a{%H-}, 100);
  for I := 0 to System.High(a) do
    a[I] := I;
  TIntHelper.RandomShuffle(a);
  ms.Instance := TMultiSet.Create(a, specialize TGDefaults<Integer>.Compare);
  AssertTrue(ms.Instance.Head(0).None);
  AssertTrue(ms.Instance.Head(0, True).Total = 1);
  AssertTrue(ms.Instance.Head(80).Total = 80);
  AssertTrue(ms.Instance.Head(80, True).Total = 81);
end;

procedure TRegularTreeMultisetTest.Tail;
var
  ms: TAutoMultiSet;
  a: TIntArray;
  I: Integer;
begin
  System.SetLength(a{%H-}, 100);
  for I := 0 to System.High(a) do
    a[I] := I;
  TIntHelper.RandomShuffle(a);
  ms.Instance := TMultiSet.Create(a, specialize TGDefaults<Integer>.Compare);
  AssertTrue(ms.Instance.Tail(99).Total = 1);
  AssertTrue(ms.Instance.Tail(99, False).None);
  AssertTrue(ms.Instance.Tail(30).Total = 70);
  AssertTrue(ms.Instance.Tail(30, False).Total = 69);
end;

procedure TRegularTreeMultisetTest.Range;
var
  ms: TAutoMultiSet;
  a: TIntArray;
  I, J: Integer;
begin
  System.SetLength(a{%H-}, 100);
  for I := 0 to System.High(a) do
    a[I] := I;
  TIntHelper.RandomShuffle(a);
  ms.Instance := TMultiSet.Create(a, specialize TGDefaults<Integer>.Compare);
  AssertTrue(ms.Instance.Range(10, 10).None);
  AssertTrue(ms.Instance.Range(100, 100).None);
  AssertTrue(ms.Instance.Range(0, 0).None);
  I := 10;
  for J in ms.Instance.Range(10, 90) do
    begin
      AssertTrue(I = J);
      Inc(I);
    end;
  AssertTrue(I = 90);
end;

procedure TRegularTreeMultisetTest.FindFloor;
var
  ms: TAutoMultiSet;
  I: Integer;
begin
  for I := 1 to 16 do
    ms.Instance.Add(I * 3);
  AssertFalse(ms.Instance.FindFloor(2, I));
  AssertFalse(ms.Instance.FindFloor(3, I));
  AssertTrue(ms.Instance.FindFloor(3, I, True));
  AssertTrue(I = 3);
  AssertTrue(ms.Instance.FindFloor(9, I));
  AssertTrue(I = 6);
  AssertTrue(ms.Instance.FindFloor(9, I, True));
  AssertTrue(I = 9);
  AssertTrue(ms.Instance.FindFloor(13, I));
  AssertTrue(I = 12);
  AssertTrue(ms.Instance.FindFloor(22, I));
  AssertTrue(I = 21);
  AssertTrue(ms.Instance.FindFloor(27, I));
  AssertTrue(I = 24);
  AssertTrue(ms.Instance.FindFloor(27, I, True));
  AssertTrue(I = 27);
end;

procedure TRegularTreeMultisetTest.FindCeil;
var
  ms: TAutoMultiSet;
  I: Integer;
begin
  for I := 1 to 16 do
    ms.Instance.Add(I * 3);
  AssertTrue(ms.Instance.FindCeil(48, I));
  AssertTrue(I = 48);
  AssertFalse(ms.Instance.FindCeil(48, I, False));
  AssertTrue(ms.Instance.FindCeil(9, I));
  AssertTrue(I = 9);
  AssertTrue(ms.Instance.FindCeil(9, I, False));
  AssertTrue(I = 12);
  AssertTrue(ms.Instance.FindCeil(13, I));
  AssertTrue(I = 15);
  AssertTrue(ms.Instance.FindCeil(22, I));
  AssertTrue(I = 24);
  AssertTrue(ms.Instance.FindCeil(27, I));
  AssertTrue(I = 27);
  AssertTrue(ms.Instance.FindCeil(27, I, False));
  AssertTrue(I = 30);
end;

function TDelegatedTreeMultisetTest.IsEven(constref aValue: Integer): Boolean;
begin
  Result := not Odd(aValue);
end;

function TDelegatedTreeMultisetTest.ObjIsEven(constref aObj: TTestObj): Boolean;
begin
  Result := not Odd(aObj.Value);
end;

procedure TDelegatedTreeMultisetTest.TestCreate;
var
  ms: TAutoMultiSet;
begin
  AssertTrue(ms.Instance.Count = 0);
  AssertTrue(ms.Instance.Capacity >= DEFAULT_CONTAINER_CAPACITY);
end;

procedure TDelegatedTreeMultisetTest.CreateArray;
var
  ms: TAutoMultiSet;
  I: Integer;
begin
  ms.Instance := TMultiSet.Create(IntArray11, specialize TGDefaults<Integer>.OnCompare);
  AssertTrue(ms.Instance.Count = 11);
  AssertTrue(ms.Instance.Capacity >= 11);
  for I in IntArray11 do
    AssertTrue(ms.Instance.Contains(I));
end;

procedure TDelegatedTreeMultisetTest.CreateEnum;
var
  ms: TAutoMultiSet;
  I: Integer;
  e: IEnumerable;
begin
  e := TIntArrayCursor.Create(TIntHelper.CreateCopy(IntArray11));
  ms.Instance := TMultiSet.Create(e, specialize TGDefaults<Integer>.OnCompare);
  AssertTrue(ms.Instance.Count = 11);
  for I in IntArray11 do
    AssertTrue(ms.Instance.Contains(I));
end;

procedure TDelegatedTreeMultisetTest.CreateCapacity0;
var
  ms: TAutoMultiSet;
begin
  ms.Instance := TMultiSet.Create(0, specialize TGDefaults<Integer>.OnCompare);
  AssertTrue(ms.Instance.Count = 0);
  AssertTrue(ms.Instance.Capacity = 0);
end;

procedure TDelegatedTreeMultisetTest.CreateCapacity5;
var
  ms: TAutoMultiSet;
begin
  ms.Instance := TMultiSet.Create(5, specialize TGDefaults<Integer>.OnCompare);
  AssertTrue(ms.Instance.Count = 0);
  AssertTrue(ms.Instance.Capacity >= 5);
end;

procedure TDelegatedTreeMultisetTest.Clear;
var
  ms: TAutoMultiSet;
begin
  ms.Instance := TMultiSet.Create(IntArray11, specialize TGDefaults<Integer>.OnCompare);
  AssertTrue(ms.Instance.Count = 11);
  AssertTrue(ms.Instance.Capacity >= DEFAULT_CONTAINER_CAPACITY);
  ms.Instance.Clear;
  AssertTrue(ms.Instance.Count = 0);
  AssertTrue(ms.Instance.Capacity = 0);
end;

procedure TDelegatedTreeMultisetTest.EnsureCapacity;
var
  ms: TAutoMultiSet;
  c: SizeInt;
begin
  c := ms.Instance.Capacity;
  ms.Instance.EnsureCapacity(c - 1);
  AssertTrue(ms.Instance.Capacity = c);
end;

procedure TDelegatedTreeMultisetTest.EnsureCapacity_1;
var
  ms: TAutoMultiSet;
  c: SizeInt;
begin
  c := ms.Instance.Capacity;
  ms.Instance.EnsureCapacity(c + 1);
  AssertTrue(ms.Instance.Capacity > c);
end;

procedure TDelegatedTreeMultisetTest.Add;
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

procedure TDelegatedTreeMultisetTest.Add100;
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
  ms.Instance[50] := 52;
  AssertTrue(ms.Instance.Count = 250);
  ms.Instance[50] := 12;
  AssertTrue(ms.Instance.Count = 210);
end;

procedure TDelegatedTreeMultisetTest.AddArray;
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

procedure TDelegatedTreeMultisetTest.AddEnum;
var
  ms: TAutoMultiSet;
  e: IEnumerable;
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

procedure TDelegatedTreeMultisetTest.AddSelf;
var
  ms: TAutoMultiSet;
begin
  ms.Instance.AddAll(IntArray21);
  AssertTrue(ms.Instance.AddAll(ms.Instance) = System.Length(IntArray21));
  AssertTrue(ms.Instance.Count = System.Length(IntArray21) * 2);
  AssertTrue(ms.Instance.EntryCount = System.Length(IntArray21));
end;

procedure TDelegatedTreeMultisetTest.Remove;
var
  ms: TAutoMultiSet;
  c: SizeInt;
begin
  c := System.Length(IntArray21) * 2;
  ms.Instance := TMultiSet.Create(IntArray21, specialize TGDefaults<Integer>.OnCompare);
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

procedure TDelegatedTreeMultisetTest.RemoveArray;
var
  ms: TAutoMultiSet;
begin
  ms.Instance := TMultiSet.Create(IntArray21, specialize TGDefaults<Integer>.OnCompare);
  ms.Instance.AddAll(IntArray21);
  AssertTrue(ms.Instance.RemoveAll(IntArray11) = 11);
  AssertTrue(ms.Instance.Count = 31);
  AssertTrue(ms.Instance.EntryCount = 21);
  AssertTrue(ms.Instance.RemoveAll(IntArray10) = 10);
  AssertTrue(ms.Instance.Count = 21);
  AssertTrue(ms.Instance.EntryCount = 21);
end;

procedure TDelegatedTreeMultisetTest.RemoveEnum;
var
  ms: TAutoMultiSet;
  e: IEnumerable;
begin
  ms.Instance := TMultiSet.Create(IntArray21, specialize TGDefaults<Integer>.OnCompare);
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

procedure TDelegatedTreeMultisetTest.RemoveSelf;
var
  ms: TAutoMultiSet;
begin
  ms.Instance := TMultiSet.Create(IntArray21, specialize TGDefaults<Integer>.OnCompare);
  ms.Instance.AddAll(IntArray21);
  AssertTrue(ms.Instance.RemoveAll(ms.Instance) = 42);
  AssertTrue(ms.Instance.IsEmpty);
end;

procedure TDelegatedTreeMultisetTest.RemoveIfRegular;
var
  ms: TAutoMultiSet;
  I: Integer;
begin
  ms.Instance := TMultiSet.Create(IntArray21, specialize TGDefaults<Integer>.OnCompare);
  ms.Instance.AddAll(IntArray21);
  AssertTrue(ms.Instance.RemoveIf(@IsEvenInt) = 20);
  AssertTrue(ms.Instance.Count = 22);
  AssertTrue(ms.Instance.ContainsAll(IntArray11));
  for I in IntArray11 do
    AssertTrue(ms.Instance[I] = 2);
end;

procedure TDelegatedTreeMultisetTest.RemoveIfDelegated;
var
  ms: TAutoMultiSet;
  I: Integer;
begin
  ms.Instance := TMultiSet.Create(IntArray21, specialize TGDefaults<Integer>.OnCompare);
  ms.Instance.AddAll(IntArray21);
  AssertTrue(ms.Instance.RemoveIf(@IsEven) = 20);
  AssertTrue(ms.Instance.Count = 22);
  AssertTrue(ms.Instance.ContainsAll(IntArray11));
  for I in IntArray11 do
    AssertTrue(ms.Instance[I] = 2);
end;

procedure TDelegatedTreeMultisetTest.RemoveIfNested;
  function IsEvenInt(constref aValue: Integer): Boolean;
  begin
    Result := not Odd(aValue);
  end;
var
  ms: TAutoMultiSet;
  I: Integer;
begin
  ms.Instance := TMultiSet.Create(IntArray21, specialize TGDefaults<Integer>.OnCompare);
  ms.Instance.AddAll(IntArray21);
  AssertTrue(ms.Instance.RemoveIf(@IsEvenInt) = 20);
  AssertTrue(ms.Instance.Count = 22);
  AssertTrue(ms.Instance.ContainsAll(IntArray11));
  for I in IntArray11 do
    AssertTrue(ms.Instance[I] = 2);
end;

procedure TDelegatedTreeMultisetTest.ExtractIfRegular;
var
  ms, ms1: TAutoMultiSet;
  e: TIntArray;
  I: Integer;
begin
  ms.Instance := TMultiSet.Create(IntArray21, specialize TGDefaults<Integer>.OnCompare);
  ms.Instance.AddAll(IntArray21);
  e := ms.Instance.ExtractIf(@IsEvenInt);
  AssertTrue(ms.Instance.Count = 22);
  AssertTrue(ms.Instance.ContainsAll(IntArray11));
  AssertTrue(System.Length(e) = 20);
  ms1.Instance := TMultiSet.Create(e, specialize TGDefaults<Integer>.OnCompare);
  for I in IntArray11 do
    AssertTrue(ms.Instance[I] = 2);
  for I in IntArray10 do
    AssertTrue(ms1.Instance[I] = 2);
end;

procedure TDelegatedTreeMultisetTest.ExtractIfDelegated;
var
  ms, ms1: TAutoMultiSet;
  e: TIntArray;
  I: Integer;
begin
  ms.Instance := TMultiSet.Create(IntArray21, specialize TGDefaults<Integer>.OnCompare);
  ms.Instance.AddAll(IntArray21);
  e := ms.Instance.ExtractIf(@IsEven);
  AssertTrue(ms.Instance.Count = 22);
  AssertTrue(ms.Instance.ContainsAll(IntArray11));
  AssertTrue(System.Length(e) = 20);
  ms1.Instance := TMultiSet.Create(e, specialize TGDefaults<Integer>.OnCompare);
  for I in IntArray11 do
    AssertTrue(ms.Instance[I] = 2);
  for I in IntArray10 do
    AssertTrue(ms1.Instance[I] = 2);
end;

procedure TDelegatedTreeMultisetTest.ExtractIfNested;
  function IsEvenInt(constref aValue: Integer): Boolean;
  begin
    Result := not Odd(aValue);
  end;
var
  ms, ms1: TAutoMultiSet;
  e: TIntArray;
  I: Integer;
begin
  ms.Instance := TMultiSet.Create(IntArray21, specialize TGDefaults<Integer>.OnCompare);
  ms.Instance.AddAll(IntArray21);
  e := ms.Instance.ExtractIf(@IsEvenInt);
  AssertTrue(ms.Instance.Count = 22);
  AssertTrue(ms.Instance.ContainsAll(IntArray11));
  AssertTrue(System.Length(e) = 20);
  ms1.Instance := TMultiSet.Create(e, specialize TGDefaults<Integer>.OnCompare);
  for I in IntArray11 do
    AssertTrue(ms.Instance[I] = 2);
  for I in IntArray10 do
    AssertTrue(ms1.Instance[I] = 2);
end;

procedure TDelegatedTreeMultisetTest.Retain;
var
  ms, ms1: TAutoMultiSet;
  I: Integer;
begin
  ms.Instance := TMultiSet.Create(IntArray21, specialize TGDefaults<Integer>.OnCompare);
  ms.Instance.AddAll(IntArray21);
  ms1.Instance := TMultiSet.Create(IntArray11, specialize TGDefaults<Integer>.OnCompare);
  AssertTrue(ms.Instance.Count = 42);
  ms.Instance.RetainAll(ms1.Instance);
  AssertTrue(ms.Instance.Count = 22);
  AssertTrue(ms.Instance.ContainsAll(ms1.Instance));
  for I in IntArray11 do
    AssertTrue(ms.Instance[I] = 2);
end;

procedure TDelegatedTreeMultisetTest.Retain_1;
var
  ms, ms1: TAutoMultiSet;
begin
  ms.Instance := TMultiSet.Create(IntArray10, specialize TGDefaults<Integer>.OnCompare);
  ms.Instance.AddAll(IntArray10);
  AssertTrue(ms.Instance.Count = 20);
  ms1.Instance := TMultiSet.Create(IntArray11, specialize TGDefaults<Integer>.OnCompare);
  ms.Instance.RetainAll(ms1.Instance);
  AssertTrue(ms.Instance.IsEmpty);
end;

procedure TDelegatedTreeMultisetTest.RetainSelf;
var
  ms: TAutoMultiSet;
begin
  ms.Instance := TMultiSet.Create(IntArray21, specialize TGDefaults<Integer>.OnCompare);
  ms.Instance.AddAll(IntArray21);
  AssertTrue(ms.Instance.Count = 42);
  ms.Instance.RetainAll(ms.Instance);
  AssertTrue(ms.Instance.Count = 42);
  AssertTrue(ms.Instance.ContainsAll(IntArray21));
end;

procedure TDelegatedTreeMultisetTest.IsSuperMultiset;
var
  ms, ms1: TAutoMultiSet;
begin
  ms.Instance := TMultiSet.Create(IntArray21, specialize TGDefaults<Integer>.OnCompare);
  ms.Instance.AddAll(IntArray21);
  ms1.Instance := TMultiSet.Create(ms.Instance, specialize TGDefaults<Integer>.OnCompare);
  AssertTrue(ms.Instance.IsSuperMultiset(ms1.Instance));
  AssertTrue(ms1.Instance.IsSuperMultiset(ms.Instance));
  AssertTrue(ms.Instance.IsSuperMultiset(ms.Instance));
  ms1.Instance.Remove(IntArray21[1]);
  AssertTrue(ms.Instance.IsSuperMultiset(ms1.Instance));
  AssertFalse(ms1.Instance.IsSuperMultiset(ms.Instance));
  ms1.Instance.Clear;
  AssertTrue(ms.Instance.IsSuperMultiset(ms1.Instance));
  AssertFalse(ms1.Instance.IsSuperMultiset(ms.Instance));
  ms.Instance.Clear;
  AssertTrue(ms.Instance.IsSuperMultiset(ms1.Instance));
  AssertTrue(ms1.Instance.IsSuperMultiset(ms.Instance));
end;

procedure TDelegatedTreeMultisetTest.IsSubMultiset;
var
  ms, ms1: TAutoMultiSet;
begin
  ms.Instance := TMultiSet.Create(IntArray21, specialize TGDefaults<Integer>.OnCompare);
  ms.Instance.AddAll(IntArray21);
  ms1.Instance := TMultiSet.Create(ms.Instance, specialize TGDefaults<Integer>.OnCompare);
  AssertTrue(ms.Instance.IsSubMultiset(ms1.Instance));
  AssertTrue(ms1.Instance.IsSubMultiset(ms.Instance));
  AssertTrue(ms.Instance.IsSubMultiset(ms.Instance));
  ms1.Instance.Remove(IntArray21[1]);
  AssertFalse(ms.Instance.IsSubMultiset(ms1.Instance));
  AssertTrue(ms1.Instance.IsSubMultiset(ms.Instance));
  ms1.Instance.Clear;
  AssertFalse(ms.Instance.IsSubMultiset(ms1.Instance));
  AssertTrue(ms1.Instance.IsSubMultiset(ms.Instance));
  ms.Instance.Clear;
  AssertTrue(ms1.Instance.IsSubMultiset(ms.Instance));
  AssertTrue(ms.Instance.IsSubMultiset(ms1.Instance));
end;

procedure TDelegatedTreeMultisetTest.IsEqual;
var
  ms, ms1: TAutoMultiSet;
begin
  ms.Instance := TMultiSet.Create(IntArray21, specialize TGDefaults<Integer>.OnCompare);
  ms.Instance.AddAll(IntArray21);
  ms1.Instance := TMultiSet.Create(ms.Instance, specialize TGDefaults<Integer>.OnCompare);
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

procedure TDelegatedTreeMultisetTest.Intersecting;
var
  ms, ms1: TAutoMultiSet;
begin
  ms.Instance := TMultiSet.Create(IntArray21, specialize TGDefaults<Integer>.OnCompare);
  ms1.Instance := TMultiSet.Create(IntArray11, specialize TGDefaults<Integer>.OnCompare);
  AssertTrue(ms.Instance.Intersecting(ms.Instance));
  AssertTrue(ms.Instance.Intersecting(ms1.Instance));
  ms.Instance := TMultiSet.Create(IntArray10, specialize TGDefaults<Integer>.OnCompare);
  AssertFalse(ms.Instance.Intersecting(ms1.Instance));
  AssertFalse(ms1.Instance.Intersecting(ms.Instance));
end;

procedure TDelegatedTreeMultisetTest.Intersect;
var
  ms, ms1: TAutoMultiSet;
  I: Integer;
begin
  ms.Instance := TMultiSet.Create(IntArray21, specialize TGDefaults<Integer>.OnCompare);
  ms.Instance.AddAll(IntArray21);
  AssertTrue(ms.Instance.Count = 42);
  ms1.Instance := TMultiSet.Create(IntArray11, specialize TGDefaults<Integer>.OnCompare);
  ms1.Instance.AddAll([-4, -3, -2, -1, 0]);
  ms.Instance.Intersect(ms1.Instance);
  AssertTrue(ms.Instance.Count = 11);
  for I in IntArray11 do
    AssertTrue(ms.Instance[I] = 1);
end;

procedure TDelegatedTreeMultisetTest.Join;
var
  ms, ms1: TAutoMultiSet;
  I: Integer;
begin
  ms1.Instance := TMultiSet.Create(IntArray11, specialize TGDefaults<Integer>.OnCompare);
  ms.Instance.Join(ms1.Instance);
  AssertTrue(ms.Instance.IsEqual(ms1.Instance));

  ms1.Instance := TMultiSet.Create(IntArray10, specialize TGDefaults<Integer>.OnCompare);
  ms1.Instance.AddAll(ms1.Instance);

  ms.Instance.Join(ms1.Instance);
  for I in IntArray11 do
    AssertTrue(ms.Instance[I] = 1);
  for I in IntArray10 do
    AssertTrue(ms.Instance[I] = 2);
end;

procedure TDelegatedTreeMultisetTest.ArithmeticAdd;
var
  ms, ms1: TAutoMultiSet;
  I: Integer;
begin
  ms1.Instance := TMultiSet.Create(IntArray11, specialize TGDefaults<Integer>.OnCompare);
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

procedure TDelegatedTreeMultisetTest.ArithmeticSubtract;
var
  ms, ms1: TAutoMultiSet;
begin
  ms1.Instance := TMultiSet.Create(IntArray11, specialize TGDefaults<Integer>.OnCompare);
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

procedure TDelegatedTreeMultisetTest.SymmetricSubtract;
var
  ms, ms1: TAutoMultiSet;
begin
  ms1.Instance := TMultiSet.Create(IntArray11, specialize TGDefaults<Integer>.OnCompare);
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

procedure TDelegatedTreeMultisetTest.Head;
var
  ms: TAutoMultiSet;
  a: TIntArray;
  I: Integer;
begin
  System.SetLength(a{%H-}, 100);
  for I := 0 to System.High(a) do
    a[I] := I;
  TIntHelper.RandomShuffle(a);
  ms.Instance := TMultiSet.Create(a, specialize TGDefaults<Integer>.OnCompare);
  AssertTrue(ms.Instance.Head(0).None);
  AssertTrue(ms.Instance.Head(0, True).Total = 1);
  AssertTrue(ms.Instance.Head(80).Total = 80);
  AssertTrue(ms.Instance.Head(80, True).Total = 81);
end;

procedure TDelegatedTreeMultisetTest.Tail;
var
  ms: TAutoMultiSet;
  a: TIntArray;
  I: Integer;
begin
  System.SetLength(a{%H-}, 100);
  for I := 0 to System.High(a) do
    a[I] := I;
  TIntHelper.RandomShuffle(a);
  ms.Instance := TMultiSet.Create(a, specialize TGDefaults<Integer>.OnCompare);
  AssertTrue(ms.Instance.Tail(99).Total = 1);
  AssertTrue(ms.Instance.Tail(99, False).None);
  AssertTrue(ms.Instance.Tail(30).Total = 70);
  AssertTrue(ms.Instance.Tail(30, False).Total = 69);
end;

procedure TDelegatedTreeMultisetTest.Range;
var
  ms: TAutoMultiSet;
  a: TIntArray;
  I, J: Integer;
begin
  System.SetLength(a{%H-}, 100);
  for I := 0 to System.High(a) do
    a[I] := I;
  TIntHelper.RandomShuffle(a);
  ms.Instance := TMultiSet.Create(a, specialize TGDefaults<Integer>.OnCompare);
  AssertTrue(ms.Instance.Range(10, 10).None);
  AssertTrue(ms.Instance.Range(100, 100).None);
  AssertTrue(ms.Instance.Range(0, 0).None);
  I := 10;
  for J in ms.Instance.Range(10, 90) do
    begin
      AssertTrue(I = J);
      Inc(I);
    end;
  AssertTrue(I = 90);
end;

procedure TDelegatedTreeMultisetTest.FindFloor;
var
  ms: TAutoMultiSet;
  I: Integer;
begin
  for I := 1 to 16 do
    ms.Instance.Add(I * 3);
  AssertFalse(ms.Instance.FindFloor(2, I));
  AssertFalse(ms.Instance.FindFloor(3, I));
  AssertTrue(ms.Instance.FindFloor(3, I, True));
  AssertTrue(I = 3);
  AssertTrue(ms.Instance.FindFloor(9, I));
  AssertTrue(I = 6);
  AssertTrue(ms.Instance.FindFloor(9, I, True));
  AssertTrue(I = 9);
  AssertTrue(ms.Instance.FindFloor(13, I));
  AssertTrue(I = 12);
  AssertTrue(ms.Instance.FindFloor(22, I));
  AssertTrue(I = 21);
  AssertTrue(ms.Instance.FindFloor(27, I));
  AssertTrue(I = 24);
  AssertTrue(ms.Instance.FindFloor(27, I, True));
  AssertTrue(I = 27);
end;

procedure TDelegatedTreeMultisetTest.FindCeil;
var
  ms: TAutoMultiSet;
  I: Integer;
begin
  for I := 1 to 16 do
    ms.Instance.Add(I * 3);
  AssertTrue(ms.Instance.FindCeil(48, I));
  AssertTrue(I = 48);
  AssertFalse(ms.Instance.FindCeil(48, I, False));
  AssertTrue(ms.Instance.FindCeil(9, I));
  AssertTrue(I = 9);
  AssertTrue(ms.Instance.FindCeil(9, I, False));
  AssertTrue(I = 12);
  AssertTrue(ms.Instance.FindCeil(13, I));
  AssertTrue(I = 15);
  AssertTrue(ms.Instance.FindCeil(22, I));
  AssertTrue(I = 24);
  AssertTrue(ms.Instance.FindCeil(27, I));
  AssertTrue(I = 27);
  AssertTrue(ms.Instance.FindCeil(27, I, False));
  AssertTrue(I = 30);
end;

initialization
  RegisterTest(TBaseTreeMultisetTest);
  RegisterTest(TComparableTreeMultisetTest);
  RegisterTest(TRegularTreeMultisetTest);
  RegisterTest(TDelegatedTreeMultisetTest);
end.

