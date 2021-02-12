unit LGListTest;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}
{$modeswitch nestedprocvars}

interface

uses
  SysUtils, fpcunit, testregistry,
  LGUtils,
  LGAbstractContainer,
  LGHelpers,
  LGList,
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

  TObjArray  = specialize TGArray<TTestObj>;

  { TGSortedListTest }

  TGSortedListTest = class(TTestCase)
  private
  type
    TIntList  = class(specialize TGSortedList<Integer>);
    TAutoList = specialize TGAutoRef<TIntList>;

    function IsEven(const aValue: Integer): Boolean;
    function ObjIsEven(const aObj: TTestObj): Boolean;
  published
    procedure TestCreate;
    procedure CreateCapacity;
    procedure CreateArray;
    procedure CreateEnum;
    procedure Clear;
    procedure EnsureCapacity;
    procedure TrimToFit;
    procedure Add;
    procedure AddArray;
    procedure AddEnum;
    procedure RejectDuplicates;
    procedure IndexOf;
    procedure IndexOf1;
    procedure CountOf;
    procedure Remove;
    procedure RemoveArray;
    procedure RemoveEnum;
    procedure RemoveSelf;
    procedure Insert;
    procedure Delete;
    procedure DeleteOutOfBounds;
    procedure DeleteAll;
    procedure TryDelete;
    procedure RemoveIfRegular;
    procedure RemoveIfDelegated;
    procedure RemoveIfNested;
    procedure ExtractIfRegular;
    procedure ExtractIfDelegated;
    procedure ExtractIfNested;
    procedure RetainAll;
    procedure RetainAll_1;
    procedure RetainAllSelf;
    procedure Head;
    procedure Head1;
    procedure Tail;
    procedure Tail1;
    procedure Range;
    procedure Range1;
    procedure FindFloor;
    procedure FindFloor1;
    procedure FindCeil;
    procedure FindCeil1;
  end;

  { TLiteSortedListTest }

  TLiteSortedListTest = class(TTestCase)
  private
  type
    TIntList = specialize TGLiteSortedList<Integer, Integer>;

  published
    procedure Add;
    procedure Clear;
    procedure EnsureCapacity;
    procedure TrimToFit;
    procedure RejectDuplicates;
    procedure IndexOf;
    procedure IndexOf1;
    procedure CountOf;
    procedure Remove;
    procedure Insert;
    procedure Delete;
    procedure DeleteOutOfBounds;
    procedure TryDelete;
    procedure Head;
    procedure Head1;
    procedure Tail;
    procedure Tail1;
    procedure Range;
    procedure Range1;
    procedure FindFloor;
    procedure FindFloor1;
    procedure FindCeil;
    procedure FindCeil1;
    procedure PassByValue;
  end;

  TRec = record
    Key,
    Value: Integer;
    class operator < (const L, R: TRec): Boolean;
    constructor Create(k, v: Integer);
  end;

  { TLiteComparableSortedListTest }

  TLiteComparableSortedListTest = class(TTestCase)
  private
  type
    TRecList = specialize TGLiteComparableSortedList<TRec>;
    TIntList = specialize TGLiteComparableSortedList<Integer>;

  published
    procedure Add;
    procedure AddRec;
    procedure Clear;
    procedure EnsureCapacity;
    procedure TrimToFit;
    procedure RejectDuplicates;
    procedure IndexOf;
    procedure IndexOf1;
    procedure CountOf;
    procedure Remove;
    procedure Insert;
    procedure Delete;
    procedure DeleteOutOfBounds;
    procedure TryDelete;
    procedure Head;
    procedure Head1;
    procedure Tail;
    procedure Tail1;
    procedure Range;
    procedure Range1;
    procedure FindFloor;
    procedure FindFloor1;
    procedure FindCeil;
    procedure FindCeil1;
    procedure PassByValue;
  end;

  { TLiteHashListTest }

  TLiteHashListTest = class(TTestCase)
  private
  type
    TIntList = specialize TGLiteHashList<Integer, Integer>;
    TStrData    = record
      Value: string;
      constructor Create(const aValue: string);
      class function HashCode(const aValue: TStrData): SizeInt; static;
      class function Equal(const L, R: TStrData): Boolean; static;
    end;

  published
    procedure Add;
    procedure AddArray;
    procedure AddEnum;
    procedure AddUniq;
    procedure AddUniqArray;
    procedure AddUniqEnum;
    procedure Clear;
    procedure EnsureCapacity;
    procedure TrimToFit;
    procedure GetEnumerator;
    procedure ToArray;
    procedure Reverse;
    procedure IndexOf;
    procedure CountOf;
    procedure Remove;
    procedure Remove_1;
    procedure Remove_2;
    procedure Insert;
    procedure Delete;
    procedure DeleteOutOfBounds;
    procedure PassByValue;
  end;

  { TLiteHashList2Test }

  TLiteHashList2Test = class(TTestCase)
  private
  type
    TEntry       = specialize TGMapEntry<Integer, Integer>;
    TIntList     = specialize TGLiteHashList2<Integer, TEntry, Integer>;
    TEntryCursor = specialize TGArrayCursor<TEntry>;

    TStrEntry    = record
      Value: string;
      constructor Create(const aValue: string);
      property Key: string read Value;
    end;

    TIntEntry    = record
      Key: Integer;
      Value: string;
      constructor Create(aKey: Integer; const aValue: string);
    end;

  published
    procedure Add;
    procedure AddArray;
    procedure AddEnum;
    procedure AddUniq;
    procedure AddUniqArray;
    procedure Clear;
    procedure MakeEmpty;
    procedure EnsureCapacity;
    procedure GetEnumerator;
    procedure ToArray;
    procedure Reverse;
    procedure IdenticalKeys;
    procedure IndexOf;
    procedure CountOf;
    procedure Remove;
    procedure ContainsUniq;
    procedure FindUniq;
    procedure PassByValue;
  end;

implementation
{$B-}{$COPERATORS ON}{$WARNINGS OFF}

const
  IntArray10: array[1..10] of Integer = (2, 4, 6, 8, 10, 12, 14, 16, 18, 20);
  IntArray11: array[1..11] of Integer = (1, 3, 5, 7, 9, 11, 13, 15, 17, 19, 21);
  IntArray15: array[1..15] of Integer = (1, 1, 2, 2, 3, 3, 4, 4, 4, 5, 5, 6, 6, 7, 7);
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

{ TGSortedListTest }

function TGSortedListTest.IsEven(const aValue: Integer): Boolean;
begin
  Result := not Odd(aValue);
end;

function TGSortedListTest.ObjIsEven(const aObj: TTestObj): Boolean;
begin
  Result := not Odd(aObj.Value);
end;

procedure TGSortedListTest.TestCreate;
var
  lst: TAutoList;
begin
  AssertTrue(lst.Instance.Count = 0);
  AssertTrue(lst.Instance.Capacity >= DEFAULT_CONTAINER_CAPACITY);
end;

procedure TGSortedListTest.CreateCapacity;
var
  lst: TAutoList;
begin
  lst.Instance := TIntList.Create(0);
  AssertTrue(lst.Instance.Count = 0);
  AssertTrue(lst.Instance.Capacity = 0);
  lst.Instance := TIntList.Create(22);
  AssertTrue(lst.Instance.Count = 0);
  AssertTrue(lst.Instance.Capacity = 22);
end;

procedure TGSortedListTest.CreateArray;
var
  lst: TAutoList;
  I: Integer;
begin
  lst.Instance := TIntList.Create(IntArray11);
  AssertTrue(lst.Instance.Count = 11);
  AssertTrue(lst.Instance.Capacity = 11);
  for I in IntArray11 do
    AssertTrue(lst.Instance.Contains(I));
  lst.Instance := TIntList.Create(IntArray15, True);
  AssertTrue(lst.Instance.Count = 7);
  AssertTrue(lst.Instance.Capacity = 7);
  for I in IntArray15 do
    AssertTrue(lst.Instance.IndexOf(I) >= 0);
end;

procedure TGSortedListTest.CreateEnum;
var
  lst: TAutoList;
  I: Integer;
begin
  lst.Instance := TIntList.Create(TIntArrayCursor.Create(TIntHelper.CreateCopy(IntArray11)));
  AssertTrue(lst.Instance.Count = 11);
  AssertTrue(lst.Instance.Capacity = 11);
  for I in IntArray11 do
    AssertTrue(lst.Instance.Contains(I));
  lst.Instance := TIntList.Create(TIntArrayCursor.Create(TIntHelper.CreateCopy(IntArray15)), True);
  AssertTrue(lst.Instance.Count = 7);
  AssertTrue(lst.Instance.Capacity = 7);
  for I in IntArray15 do
    AssertTrue(lst.Instance.IndexOf(I) >= 0);
end;

procedure TGSortedListTest.Clear;
var
  lst: TAutoList;
begin
  lst.Instance := TIntList.Create(IntArray21);
  AssertTrue(lst.Instance.Count = 21);
  AssertTrue(lst.Instance.Capacity = 21);
  lst.Instance.Clear;
  AssertTrue(lst.Instance.Count = 0);
  AssertTrue(lst.Instance.Capacity = 0);
end;

procedure TGSortedListTest.EnsureCapacity;
var
  lst: TAutoList;
begin
  lst.Instance.Clear;
  AssertTrue(lst.Instance.Capacity = 0);
  lst.Instance.EnsureCapacity(100);
  AssertTrue(lst.Instance.Capacity >= 100);
end;

procedure TGSortedListTest.TrimToFit;
var
  lst: TAutoList;
begin
  {%H-}lst.Instance := TIntList.Create(IntArray21);
  AssertTrue(lst.Instance.Count = 21);
  lst.Instance.EnsureCapacity(255);
  AssertTrue(lst.Instance.Capacity = 256);
  lst.Instance.TrimToFit;
  AssertTrue(lst.Instance.Count = 21);
  AssertTrue(lst.Instance.Capacity = 21);
end;

procedure TGSortedListTest.Add;
var
  lst: TAutoList;
  I: Integer;
begin
  AssertTrue(lst.Instance.Count = 0);
  AssertTrue(lst.Instance.Add(1));
  AssertTrue(lst.Instance.Count = 1);
  AssertTrue(lst.Instance.Contains(1));
  AssertTrue(lst.Instance.Add(51));
  AssertTrue(lst.Instance.Count = 2);
  AssertTrue(lst.Instance.Contains(51));
  AssertTrue(lst.Instance.Add(52));
  AssertTrue(lst.Instance.Count = 3);
  AssertTrue(lst.Instance.Contains(52));
  for I := 1 to 100 do
    AssertTrue(lst.Instance.Add(I));
  AssertTrue(lst.Instance.Count = 103);
  for I := 1 to 100 do
    AssertTrue(lst.Instance.Contains(I));
end;

procedure TGSortedListTest.AddArray;
var
  lst: TAutoList;
begin
  AssertTrue(lst.Instance.AddAll(IntArray21) = Length(IntArray21));
  AssertTrue(lst.Instance.Count = Length(IntArray21));
  AssertTrue(lst.Instance.ContainsAll(IntArray21));
end;

procedure TGSortedListTest.AddEnum;
var
  lst: TAutoList;
begin
  AssertTrue(lst.Instance.AddAll(TIntArrayCursor.Create(TIntHelper.CreateCopy(IntArray21))) = Length(IntArray21));
  AssertTrue(lst.Instance.Count = Length(IntArray21));
  AssertTrue(lst.Instance.ContainsAll(IntArray21));
end;

procedure TGSortedListTest.RejectDuplicates;
var
  lst: TAutoList;
  I, J: Integer;
begin
  AssertTrue(lst.Instance.AddAll(IntArray21) = Length(IntArray21));
  AssertTrue(lst.Instance.Count = Length(IntArray21));
  AssertTrue(lst.Instance.AddAll(IntArray21) = Length(IntArray21));
  AssertTrue(lst.Instance.Count = Length(IntArray21) * 2);
  lst.Instance.RejectDuplicates := True;
  AssertTrue(lst.Instance.Count = Length(IntArray21));
  J := 1;
  for I in lst.Instance do
    begin
      AssertTrue(I = IntArray21[J]);
      Inc(J);
    end;
end;

procedure TGSortedListTest.IndexOf;
var
  lst: TAutoList;
  I: Integer;
begin
  AssertTrue(lst.Instance.AddAll(IntArray21) = Length(IntArray21));
  for I in IntArray21 do
    AssertTrue(lst.Instance.IndexOf(I) = I - 1);

  AssertTrue(lst.Instance.IndexOf(0) = -1);
  AssertTrue(lst.Instance.IndexOf(22) = -1);
end;

procedure TGSortedListTest.IndexOf1;
var
  lst: TAutoList;
  I: Integer;
begin
  lst.Instance.AddAll(IntArray21);
  lst.Instance.AddAll(IntArray21);
  lst.Instance.AddAll(IntArray21);
  AssertTrue(lst.Instance.Count = Length(IntArray21) * 3);
  for I in IntArray21 do
    AssertTrue(lst.Instance.IndexOf(I) = (I - 1) * 3);
  AssertTrue(lst.Instance.IndexOf(0) = -1);
  AssertTrue(lst.Instance.IndexOf(22) = -1);
end;

procedure TGSortedListTest.CountOf;
var
  lst: TAutoList;
  I: Integer;
begin
  lst.Instance.AddAll(IntArray11);
  lst.Instance.AddAll(IntArray11);
  lst.Instance.AddAll(IntArray11);
  AssertTrue(lst.Instance.Count = Length(IntArray11) * 3);
  for I in IntArray11 do
    AssertTrue(lst.Instance.CountOf(I) = 3);
  AssertTrue(lst.Instance.CountOf(0) = 0);
  AssertTrue(lst.Instance.CountOf(12) = 0);
end;

procedure TGSortedListTest.Remove;
var
  lst: TAutoList;
begin
  lst.Instance := TIntList.Create(IntArray21);
  AssertTrue(lst.Instance.Count = Length(IntArray21));
  AssertTrue(lst.Instance.Remove(IntArray21[1]));
  AssertTrue(lst.Instance.Count = Length(IntArray21) - 1);
  AssertTrue(lst.Instance.Remove(IntArray21[High(IntArray21)]));
  AssertTrue(lst.Instance.Count = Length(IntArray21) - 2);
  AssertFalse(lst.Instance.Remove(-1));
  AssertTrue(lst.Instance.Count = Length(IntArray21) - 2);
  AssertFalse(lst.Instance.Remove(100));
  AssertTrue(lst.Instance.Count = Length(IntArray21) - 2);
  AssertTrue(lst.Instance.Remove(IntArray21[2]));
  AssertTrue(lst.Instance.Count = Length(IntArray21) - 3);
  AssertTrue(lst.Instance.Remove(IntArray21[High(IntArray21) - 1]));
  AssertTrue(lst.Instance.Count = Length(IntArray21) - 4);
end;

procedure TGSortedListTest.RemoveArray;
var
  lst: TAutoList;
begin
  lst.Instance := TIntList.Create(IntArray21);
  AssertTrue(lst.Instance.RemoveAll(IntArray11) = 11);
  AssertTrue(lst.Instance.Count = 10);
  AssertTrue(lst.Instance.RemoveAll(IntArray10) = 10);
  AssertTrue(lst.Instance.IsEmpty);
end;

procedure TGSortedListTest.RemoveEnum;
var
  lst: TAutoList;
begin
  lst.Instance := TIntList.Create(IntArray21);
  AssertTrue(lst.Instance.RemoveAll(TIntArrayCursor.Create(TIntHelper.CreateCopy(IntArray11))) = 11);
  AssertTrue(lst.Instance.Count = 10);
  AssertTrue(lst.Instance.RemoveAll(TIntArrayCursor.Create(TIntHelper.CreateCopy(IntArray10))) = 10);
  AssertTrue(lst.Instance.IsEmpty);
end;

procedure TGSortedListTest.RemoveSelf;
var
  lst: TAutoList;
begin
  lst.Instance := TIntList.Create(IntArray21);
  AssertTrue(lst.Instance.RemoveAll(lst.Instance) = 21);
  AssertTrue(lst.Instance.IsEmpty);
end;

procedure TGSortedListTest.Insert;
var
  lst: TAutoList;
  I: Integer;
begin
  for I := 1 to 21 do
    AssertTrue(lst.Instance.Insert(I) = I - 1);
  AssertTrue(lst.Instance.Count = 21);
  AssertTrue(lst.Instance.Insert(20) >= 0);
  AssertTrue(lst.Instance.Insert(20) >= 0);
  AssertTrue(lst.Instance.Insert(20) >= 0);
  AssertTrue(lst.Instance.Count = 24);
  lst.Instance.RejectDuplicates := True;
  AssertTrue(lst.Instance.Count = 21);
  AssertTrue(lst.Instance.Insert(19) < 0);
  AssertTrue(lst.Instance.Insert(20) < 0);
  AssertTrue(lst.Instance.Insert(21) < 0);
  AssertTrue(lst.Instance.Count = 21);
end;

procedure TGSortedListTest.Delete;
var
  lst: TAutoList;
begin
  lst.Instance := TIntList.Create([0, 1, 2, 3, 4, 5, 6, 7]);
  lst.Instance.Delete(0);
  AssertTrue(lst.Instance.Count = 7);
  AssertTrue(lst{%H-}.Instance[0] = 1);
  lst.Instance.Delete(3);
  AssertTrue(lst.Instance.Count = 6);
  AssertTrue(lst{%H-}.Instance[3] = 5);
  AssertTrue(TIntHelper.Same(lst.Instance.ToArray, [1, 2, 3, 5, 6, 7]));
end;

procedure TGSortedListTest.DeleteOutOfBounds;
var
  lst: TAutoList;
  Raised: Boolean;
begin
  Raised := False;
  try
    lst.Instance.Delete(0);
  except
    on ELGListError do
      Raised := True;
  end;
  AssertTrue(Raised);

  lst.Instance.Insert(12);
  AssertTrue(lst{%H-}.Instance[0] = 12);
  Raised := False;
  try
    lst.Instance.Delete(1);
  except
    on ELGListError do
      Raised := True;
  end;
  AssertTrue(Raised);

  Raised := False;
  try
    lst.Instance.Delete(-1);
  except
    on ELGListError do
      Raised := True;
  end;
  AssertTrue(Raised);
end;

procedure TGSortedListTest.DeleteAll;
var
  lst: TAutoList;
  I: Integer;
begin
  lst.Instance := TIntList.Create(IntArray21);
  AssertTrue(lst.Instance.DeleteAll(0, 5) = 5);
  AssertTrue(lst.Instance.Count = 16);
  for I := 0 to 15 do
    AssertTrue(lst{%H-}.Instance[I] = I + 6);
  lst.Instance := TIntList.Create(IntArray21);
  AssertTrue(lst.Instance.DeleteAll(16, 8) = 5);
  AssertTrue(lst.Instance.Count = 16);
  for I := 0 to 15 do
    AssertTrue(lst{%H-}.Instance[I] = I + 1);
  lst.Instance := TIntList.Create(IntArray21);
  AssertTrue(lst.Instance.DeleteAll(1, 19) = 19);
  AssertTrue(lst.Instance.Count = 2);
  AssertTrue(lst{%H-}.Instance[0] = 1);
  AssertTrue(lst{%H-}.Instance[1] = 21);
end;

procedure TGSortedListTest.TryDelete;
var
  lst: TAutoList;
begin
  AssertFalse(lst.Instance.TryDelete(0));
  lst.Instance := TIntList.Create([1, 2, 3, 4, 5]);
  AssertTrue(lst.Instance.Count = 5);
  AssertFalse(lst.Instance.TryDelete(-1));
  AssertFalse(lst.Instance.TryDelete(6));
  AssertTrue(lst.Instance.TryDelete(0));
  AssertTrue(lst.Instance.Count = 4);
  AssertTrue(lst.Instance.TryDelete(3));
  AssertTrue(lst.Instance.Count = 3);
  AssertTrue(lst.Instance.TryDelete(1));
  AssertTrue(lst.Instance.Count = 2);
end;

procedure TGSortedListTest.RemoveIfRegular;
var
  lst: TAutoList;
begin
  lst.Instance := TIntList.Create(IntArray21);
  AssertTrue(lst.Instance.RemoveIf(@IsEvenInt) = 10);
  AssertTrue(lst.Instance.Count = 11);
  AssertTrue(lst.Instance.ContainsAll(IntArray11));
end;

procedure TGSortedListTest.RemoveIfDelegated;
var
  lst: TAutoList;
begin
  lst.Instance := TIntList.Create(IntArray21);
  AssertTrue(lst.Instance.RemoveIf(@IsEven) = 10);
  AssertTrue(lst.Instance.Count = 11);
  AssertTrue(lst.Instance.ContainsAll(IntArray11));
end;

procedure TGSortedListTest.RemoveIfNested;
var
  lst: TAutoList;
  function IsEvenInt(const aValue: Integer): Boolean;
  begin
    Result := not Odd(aValue);
  end;
begin
  lst.Instance := TIntList.Create(IntArray21);
  AssertTrue(lst.Instance.RemoveIf(@IsEvenInt) = 10);
  AssertTrue(lst.Instance.Count = 11);
  AssertTrue(lst.Instance.ContainsAll(IntArray11));
end;

procedure TGSortedListTest.ExtractIfRegular;
var
  lst: TAutoList;
  e: TIntArray;
begin
  lst.Instance := TIntList.Create(IntArray21);
  e := lst.Instance.ExtractIf(@IsEvenInt);
  AssertTrue(lst.Instance.Count = 11);
  AssertTrue(lst.Instance.ContainsAll(IntArray11));
  AssertTrue(System.Length(e) = 10);
  AssertTrue(TIntHelper.Same(IntArray10, e));
end;

procedure TGSortedListTest.ExtractIfDelegated;
var
  lst: TAutoList;
  e: TIntArray;
begin
  lst.Instance := TIntList.Create(IntArray21);
  e := lst.Instance.ExtractIf(@IsEven);
  AssertTrue(lst.Instance.Count = 11);
  AssertTrue(lst.Instance.ContainsAll(IntArray11));
  AssertTrue(System.Length(e) = 10);
  AssertTrue(TIntHelper.Same(IntArray10, e));
end;

procedure TGSortedListTest.ExtractIfNested;
var
  lst: TAutoList;
  e: TIntArray;
  function IsEvenInt(const aValue: Integer): Boolean;
  begin
    Result := not Odd(aValue);
  end;
begin
  lst.Instance := TIntList.Create(IntArray21);
  e := lst.Instance.ExtractIf(@IsEvenInt);
  AssertTrue(lst.Instance.Count = 11);
  AssertTrue(lst.Instance.ContainsAll(IntArray11));
  AssertTrue(System.Length(e) = 10);
  AssertTrue(TIntHelper.Same(IntArray10, e));
end;

procedure TGSortedListTest.RetainAll;
var
  lst, s1: TAutoList;
begin
  lst.Instance := TIntList.Create(IntArray21);
  s1.Instance := TIntList.Create(IntArray11);
  lst.Instance.AddAll([-3, -2, -1, 0]);
  AssertTrue(lst.Instance.Count = 25);
  lst.Instance.RetainAll(s1.Instance);
  AssertTrue(lst.Instance.Count = 11);
  AssertTrue(lst.Instance.ContainsAll(s1.Instance));
end;

procedure TGSortedListTest.RetainAll_1;
var
  lst, s1: TAutoList;
begin
  lst.Instance := TIntList.Create(IntArray10);
  s1.Instance := TIntList.Create(IntArray11);
  lst.Instance.RetainAll(s1.Instance);
  AssertTrue(lst.Instance.IsEmpty);
end;

procedure TGSortedListTest.RetainAllSelf;
var
  lst: TAutoList;
begin
  lst.Instance := TIntList.Create(IntArray21);
  AssertTrue(lst.Instance.Count = 21);
  lst.Instance.RetainAll(lst.Instance);
  AssertTrue(lst.Instance.Count = 21);
  AssertTrue(lst.Instance.ContainsAll(IntArray21));
end;

procedure TGSortedListTest.Head;
var
  lst: TAutoList;
  a: TIntArray;
  I, J: Integer;
begin
  System.SetLength(a, 100);
  for I := 0 to System.High(a) do
    a[I] := I;
  TIntHelper.RandomShuffle(a);
  lst.Instance := TIntList.Create(a);
  AssertTrue(lst.Instance.Head(0).None);
  AssertTrue(lst.Instance.Head(0, True).Total = 1);
  I := 0;
  for J in lst.Instance.Head(80) do
    begin
      AssertTrue(I = J);
      Inc(I);
    end;
  AssertTrue(I = 80);
  I := 0;
  for J in lst.Instance.Head(80, True) do
    begin
      AssertTrue(I = J);
      Inc(I);
    end;
  AssertTrue(I = 81);
end;

procedure TGSortedListTest.Head1;
var
  lst: TAutoList;
  I, J: Integer;
begin
  AssertTrue(lst.Instance.Head(5).None);
  for I := 0 to 2 do
    for J := 1 to 8 do
      lst.Instance.Add(J);
  AssertTrue(lst.Instance.Count = 24);
  AssertTrue(lst.Instance.Head(0).None);
  AssertTrue(lst.Instance.Head(1).None);
  AssertTrue(lst.Instance.Head(1, True).Total = 3);
  AssertTrue(lst.Instance.Head(5).Total = 12);
  AssertTrue(lst.Instance.Head(5, True).Total = 15);
  AssertTrue(lst.Instance.Head(8).Total = 21);
  AssertTrue(lst.Instance.Head(8, True).Total = 24);
  AssertTrue(lst.Instance.Head(9).Total = 24);
end;

procedure TGSortedListTest.Tail;
var
  lst: TAutoList;
  a: TIntArray;
  I, J: Integer;
begin
  AssertTrue(lst.Instance.Tail(9).None);
  System.SetLength(a, 100);
  for I := 0 to System.High(a) do
    a[I] := I;
  TIntHelper.RandomShuffle(a);
  lst.Instance := TIntList.Create(a);
  AssertTrue(lst.Instance.Tail(99).Total = 1);
  AssertTrue(lst.Instance.Tail(99, False).None);
  I := 30;
  for J in lst.Instance.Tail(30) do
    begin
      AssertTrue(I = J);
      Inc(I);
    end;
  AssertTrue(I = 100);
  AssertTrue(lst.Instance.Tail(30, False).Total = 69);
end;

procedure TGSortedListTest.Tail1;
var
  lst: TAutoList;
  I, J: Integer;
begin
  for I := 0 to 2 do
    for J := 1 to 8 do
      lst.Instance.Add(J);
  AssertTrue(lst.Instance.Count = 24);
  AssertTrue(lst.Instance.Tail(9).None);
  AssertTrue(lst.Instance.Tail(8).Total = 3);
  AssertTrue(lst.Instance.Tail(8, False).None);
  AssertTrue(lst.Instance.Tail(5).Total = 12);
  AssertTrue(lst.Instance.Tail(5, False).Total = 9);
  AssertTrue(lst.Instance.Tail(1, False).Total = 21);
  AssertTrue(lst.Instance.Tail(1).Total = 24);
  AssertTrue(lst.Instance.Tail(0, True).Total = 24);
end;

procedure TGSortedListTest.Range;
var
  lst: TAutoList;
  a: TIntArray;
  I, J: Integer;
begin
  AssertTrue(lst.Instance.Range(1, 5).None);
  System.SetLength(a, 100);
  for I := 0 to System.High(a) do
    a[I] := I;
  TIntHelper.RandomShuffle(a);
  lst.Instance := TIntList.Create(a);
  AssertTrue(lst.Instance.Range(10, 10).None);
  AssertTrue(lst.Instance.Range(10, 10, BOUNDS_BOTH).Total = 1);
  AssertTrue(lst.Instance.Range(99, 99, []).None);
  AssertTrue(lst.Instance.Range(0, 0).None);
  AssertTrue(lst.Instance.Range(0, 0, BOUNDS_BOTH).Total = 1);
  I := 10;
  for J in lst.Instance.Range(10, 90) do
    begin
      AssertTrue(I = J);
      Inc(I);
    end;
  AssertTrue(I = 90);
  AssertTrue(lst.Instance.Range(10, 90, BOUNDS_BOTH).Total = 81);
end;

procedure TGSortedListTest.Range1;
var
  lst: TAutoList;
  I, J: Integer;
begin
  for I := 0 to 2 do
    for J := 1 to 8 do
      lst.Instance.Add(J);
  AssertTrue(lst.Instance.Count = 24);
  AssertTrue(lst.Instance.Range(9, 10).None);
  AssertTrue(lst.Instance.Range(8, 8, BOUNDS_BOTH).Total = 3);
  AssertTrue(lst.Instance.Range(8, 8).None);
  AssertTrue(lst.Instance.Range(5, 7).Total = 6);
  AssertTrue(lst.Instance.Range(5, 7, BOUNDS_BOTH).Total = 9);
  AssertTrue(lst.Instance.Range(1, 8).Total = 21);
  AssertTrue(lst.Instance.Range(1, 8, BOUNDS_BOTH).Total = 24);
  AssertTrue(lst.Instance.Range(0, 9).Total = 24);
end;

procedure TGSortedListTest.FindFloor;
var
  lst: TAutoList;
  I: Integer;
begin
  for I := 0 to 15 do
    lst.Instance.Add(I);
  AssertTrue(lst.Instance.IndexOfFloor(-2) = -1);
  AssertTrue(lst.Instance.IndexOfFloor(0) = -1);
  AssertTrue(lst.Instance.IndexOfFloor(0, True) = 0);
  AssertTrue(lst.Instance.IndexOfFloor(3) = 2);
  AssertTrue(lst.Instance.IndexOfFloor(3, True) = 3);
  AssertTrue(lst.Instance.IndexOfFloor(15) = 14);
  AssertTrue(lst.Instance.IndexOfFloor(15, True) = 15);
  AssertTrue(lst.Instance.IndexOfFloor(20) = 15);
end;

procedure TGSortedListTest.FindFloor1;
var
  lst: TAutoList;
  I: Integer;
begin
  for I := 0 to 15 do
    lst.Instance.Add(I);
  lst.Instance.AddAll(lst.Instance);
  lst.Instance.AddAll(lst.Instance);
  AssertTrue(lst.Instance.Count = 64);
  AssertTrue(lst.Instance.IndexOfFloor(-2) = -1);
  AssertTrue(lst.Instance.IndexOfFloor(0) = -1);
  AssertTrue(lst.Instance.IndexOfFloor(0, True) = 3);
  AssertTrue(lst.Instance.IndexOfFloor(3) = Pred(3*4));
  AssertTrue(lst.Instance.IndexOfFloor(3, True) = Pred(4*4));
  AssertTrue(lst.Instance.IndexOfFloor(15) = Pred(15*4));
  AssertTrue(lst.Instance.IndexOfFloor(15, True) = Pred(16*4));
  AssertTrue(lst.Instance.IndexOfFloor(20) = 63);
end;

procedure TGSortedListTest.FindCeil;
var
  lst: TAutoList;
  I: Integer;
begin
  for I := 0 to 15 do
    lst.Instance.Add(I);
  AssertTrue(lst.Instance.IndexOfCeil(15) = 15);
  AssertTrue(lst.Instance.IndexOfCeil(15, False) = -1);
  AssertTrue(lst.Instance.IndexOfCeil(0) = 0);
  AssertTrue(lst.Instance.IndexOfCeil(0, False) = 1);
  AssertTrue(lst.Instance.IndexOfCeil(10) = 10);
  AssertTrue(lst.Instance.IndexOfCeil(10, False) = 11);
  AssertTrue(lst.Instance.IndexOfCeil(-1) = 0);
end;

procedure TGSortedListTest.FindCeil1;
var
  lst: TAutoList;
  I: Integer;
begin
  for I := 0 to 15 do
    lst.Instance.Add(I);
  lst.Instance.AddAll(lst.Instance);
  lst.Instance.AddAll(lst.Instance);
  AssertTrue(lst.Instance.Count = 64);
  AssertTrue(lst.Instance.IndexOfCeil(15) = 15*4);
  AssertTrue(lst.Instance.IndexOfCeil(15, False) = -1);
  AssertTrue(lst.Instance.IndexOfCeil(0) = 0);
  AssertTrue(lst.Instance.IndexOfCeil(0, False) = 1*4);
  AssertTrue(lst.Instance.IndexOfCeil(10) = 10*4);
  AssertTrue(lst.Instance.IndexOfCeil(10, False) = 11*4);
  AssertTrue(lst.Instance.IndexOfCeil(-1) = 0);
end;

{ TLiteSortedListTest }

procedure TLiteSortedListTest.Add;
var
  lst: TIntList;
  I: Integer;
begin
  AssertTrue(lst.Count = 0);
  AssertTrue(lst.Add(1));
  AssertTrue(lst.Count = 1);
  AssertTrue(lst.Contains(1));
  AssertTrue(lst.Add(51));
  AssertTrue(lst.Count = 2);
  AssertTrue(lst.Contains(51));
  AssertTrue(lst.Add(52));
  AssertTrue(lst.Count = 3);
  AssertTrue(lst.Contains(52));
  for I := 1 to 100 do
    AssertTrue(lst.Add(I));
  AssertTrue(lst.Count = 103);
  for I := 1 to 100 do
    AssertTrue(lst.Contains(I));
end;

procedure TLiteSortedListTest.Clear;
var
  lst: TIntList;
  I: Integer;
begin
  for I in IntArray21 do
    lst.Add(I);
  AssertTrue(lst.Count = 21);
  AssertTrue(lst.Capacity >= 21);
  lst.Clear;
  AssertTrue(lst.Count = 0);
  AssertTrue(lst.Capacity = 0);
end;

procedure TLiteSortedListTest.EnsureCapacity;
var
  lst: TIntList;
begin
  AssertTrue(lst.Capacity = 0);
  lst.EnsureCapacity(100);
  AssertTrue(lst.Capacity >= 100);
end;

procedure TLiteSortedListTest.TrimToFit;
var
  lst: TIntList;
  I: Integer;
begin
  for I in IntArray21 do
    lst.Add(I);
  AssertTrue(lst.Count = 21);
  lst.EnsureCapacity(255);
  AssertTrue(lst.Capacity = 256);
  lst.TrimToFit;
  AssertTrue(lst.Count = 21);
  AssertTrue(lst.Capacity = 21);
end;

procedure TLiteSortedListTest.RejectDuplicates;
var
  lst: TIntList;
  I, J: Integer;
begin
  for I in IntArray21 do
    lst.Add(I);
  for I in IntArray21 do
    lst.Add(I);
  AssertTrue(lst.Count = Length(IntArray21) * 2);
  lst.RejectDuplicates := True;
  AssertTrue(lst.Count = Length(IntArray21));
  J := 1;
  for I in lst do
    begin
      AssertTrue(I = IntArray21[J]);
      Inc(J);
    end;
end;

procedure TLiteSortedListTest.IndexOf;
var
  lst: TIntList;
  I: Integer;
begin
  for I in IntArray21 do
    lst.Add(I);
  for I in IntArray21 do
    AssertTrue(lst.IndexOf(I) = I - 1);

  AssertTrue(lst.IndexOf(0) = -1);
  AssertTrue(lst.IndexOf(22) = -1);
end;

procedure TLiteSortedListTest.IndexOf1;
var
  lst: TIntList;
  I: Integer;
begin
  for I in IntArray21 do
    begin
      lst.Add(I);
      lst.Add(I);
      lst.Add(I);
    end;
  AssertTrue(lst.Count = Length(IntArray21) * 3);
  for I in IntArray21 do
    AssertTrue(lst.IndexOf(I) = (I - 1) * 3);
  AssertTrue(lst.IndexOf(0) = -1);
  AssertTrue(lst.IndexOf(22) = -1);
end;

procedure TLiteSortedListTest.CountOf;
var
  lst: TIntList;
  I: Integer;
begin
  for I in IntArray11 do
    begin
      lst.Add(I);
      lst.Add(I);
      lst.Add(I);
    end;
  AssertTrue(lst.Count = Length(IntArray11) * 3);
  for I in IntArray11 do
    AssertTrue(lst.CountOf(I) = 3);
  AssertTrue(lst.CountOf(0) = 0);
  AssertTrue(lst.CountOf(12) = 0);
end;

procedure TLiteSortedListTest.Remove;
var
  lst: TIntList;
  I: Integer;
begin
  for I in IntArray21 do
    lst.Add(I);
  AssertTrue(lst.Count = Length(IntArray21));
  AssertTrue(lst.Remove(IntArray21[1]));
  AssertTrue(lst.Count = Length(IntArray21) - 1);
  AssertTrue(lst.Remove(IntArray21[High(IntArray21)]));
  AssertTrue(lst.Count = Length(IntArray21) - 2);
  AssertFalse(lst.Remove(-1));
  AssertTrue(lst.Count = Length(IntArray21) - 2);
  AssertFalse(lst.Remove(100));
  AssertTrue(lst.Count = Length(IntArray21) - 2);
  AssertTrue(lst.Remove(IntArray21[2]));
  AssertTrue(lst.Count = Length(IntArray21) - 3);
  AssertTrue(lst.Remove(IntArray21[High(IntArray21) - 1]));
  AssertTrue(lst.Count = Length(IntArray21) - 4);
end;

procedure TLiteSortedListTest.Insert;
var
  lst: TIntList;
  I: Integer;
begin
  for I := 1 to 21 do
    AssertTrue(lst.Insert(I) = I - 1);
  AssertTrue(lst.Count = 21);
  AssertTrue(lst.Insert(20) >= 0);
  AssertTrue(lst.Insert(20) >= 0);
  AssertTrue(lst.Insert(20) >= 0);
  AssertTrue(lst.Count = 24);
  lst.RejectDuplicates := True;
  AssertTrue(lst.Count = 21);
  AssertTrue(lst.Insert(19) < 0);
  AssertTrue(lst.Insert(20) < 0);
  AssertTrue(lst.Insert(21) < 0);
  AssertTrue(lst.Count = 21);
end;

procedure TLiteSortedListTest.Delete;
var
  lst: TIntList;
  I: Integer;
begin
  for I := 0 to 7 do
    lst.Add(I);
  AssertTrue(lst.Count = 8);
  lst.Delete(0);
  AssertTrue(lst.Count = 7);
  AssertTrue(lst[0] = 1);
  lst.Delete(3);
  AssertTrue(lst.Count = 6);
  AssertTrue(lst[3] = 5);
  AssertTrue(TIntHelper.Same(lst.ToArray, [1, 2, 3, 5, 6, 7]));
end;

procedure TLiteSortedListTest.DeleteOutOfBounds;
var
  lst: TIntList;
  Raised: Boolean;
begin
  Raised := False;
  try
    lst.Delete(0);
  except
    on ELGListError do
      Raised := True;
  end;
  AssertTrue(Raised);

  lst.Insert(12);
  AssertTrue(lst[0] = 12);
  Raised := False;
  try
    lst.Delete(1);
  except
    on ELGListError do
      Raised := True;
  end;

  AssertTrue(Raised);
  Raised := False;
  try
    lst.Delete(-1);
  except
    on ELGListError do
      Raised := True;
  end;
  AssertTrue(Raised);
end;

procedure TLiteSortedListTest.TryDelete;
var
  lst: TIntList;
  I: Integer;
begin
  AssertFalse(lst.TryDelete(0));
  for I := 1 to 5 do
    lst.Add(I);
  AssertTrue(lst.Count = 5);
  AssertFalse(lst.TryDelete(-1));
  AssertFalse(lst.TryDelete(6));
  AssertTrue(lst.TryDelete(0));
  AssertTrue(lst.Count = 4);
  AssertTrue(lst.TryDelete(3));
  AssertTrue(lst.Count = 3);
  AssertTrue(lst.TryDelete(1));
  AssertTrue(lst.Count = 2);
end;

procedure TLiteSortedListTest.Head;
var
  lst: TIntList;
  a: TIntArray;
  I, J: Integer;
begin
  System.SetLength(a, 100);
  for I := 0 to System.High(a) do
    a[I] := I;
  TIntHelper.RandomShuffle(a);
  for I in a do
    lst.Add(I);
  I := 0;
  for J in lst{%H-}.Head(0) do
    begin
      AssertTrue(I = J);
      Inc(I);
    end;
  AssertTrue(I = 0);
  I := 0;
  for J in lst{%H-}.Head(0, True) do
    begin
      AssertTrue(I = J);
      Inc(I);
    end;
  AssertTrue(I = 1);
  I := 0;
  for J in lst{%H-}.Head(80) do
    begin
      AssertTrue(I = J);
      Inc(I);
    end;
  AssertTrue(I = 80);
  I := 0;
  for J in lst{%H-}.Head(80, True) do
    begin
      AssertTrue(I = J);
      Inc(I);
    end;
  AssertTrue(I = 81);
end;

procedure TLiteSortedListTest.Head1;
var
  lst: TIntList;
  I, J: Integer;
begin
  J := 0;
  for I in lst.Head(5) do
    Inc(J);
  AssertTrue(J = 0);

  for I := 0 to 2 do
    for J := 1 to 8 do
      lst.Add(J);
  AssertTrue(lst.Count = 24);

  J := 0;
  for I in lst.Head(0) do
    Inc(J);
  AssertTrue(J = 0);

  for I in lst.Head(1) do
    Inc(J);
  AssertTrue(J = 0);

  for I in lst.Head(1, True) do
    Inc(J);
  AssertTrue(J = 3);

  J := 0;
  for I in lst.Head(5) do
    begin
     AssertTrue(I < 5);
     Inc(J);
    end;
  AssertTrue(J = 12);

  J := 0;
  for I in lst.Head(5, True) do
    begin
     AssertTrue(I <= 5);
     Inc(J);
    end;
  AssertTrue(J = 15);

  J := 0;
  for I in lst.Head(8) do
    begin
     AssertTrue(I < 8);
     Inc(J);
    end;
  AssertTrue(J = 21);

  J := 0;
  for I in lst.Head(8, True) do
    begin
     AssertTrue(I <= 8);
     Inc(J);
    end;
  AssertTrue(J = 24);
end;

procedure TLiteSortedListTest.Tail;
var
  lst: TIntList;
  a: TIntArray;
  I, J: Integer;
begin
  System.SetLength(a, 100);
  for I := 0 to System.High(a) do
    a[I] := I;
  TIntHelper.RandomShuffle(a);
  for I in a do
    lst.Add(I);
  I := 0;
  for J in lst.Tail(99) do
    Inc(I);
  AssertTrue(I = 1);
  I := 0;
  for J in lst.Tail(99, False) do
    Inc(I);
  AssertTrue(I = 0);
  I := 30;
  for J in lst.Tail(30) do
    begin
      AssertTrue(I = J);
      Inc(I);
    end;
  AssertTrue(I = 100);
end;

procedure TLiteSortedListTest.Tail1;
var
  lst: TIntList;
  I, J: Integer;
begin
  J := 0;
  for I in lst.Tail(0) do
    Inc(J);
  AssertTrue(J = 0);

  for I := 0 to 2 do
    for J := 1 to 8 do
      lst.Add(J);
  AssertTrue(lst.Count = 24);

  J := 0;
  for I in lst.Tail(9) do
    Inc(J);
  AssertTrue(J = 0);

  for I in lst.Tail(8, False) do
    Inc(J);
  AssertTrue(J = 0);

  for I in lst.Tail(8) do
    Inc(J);
  AssertTrue(J = 3);

  J := 0;
  for I in lst.Tail(5) do
    begin
      AssertTrue(I >= 5);
      Inc(J);
    end;
  AssertTrue(J = 12);

  J := 0;
  for I in lst.Tail(5, False) do
    begin
      AssertTrue(I > 5);
      Inc(J);
    end;
  AssertTrue(J = 9);

  J := 0;
  for I in lst.Tail(1) do
    begin
      AssertTrue(I >= 1);
      Inc(J);
    end;
  AssertTrue(J = 24);

  J := 0;
  for I in lst.Tail(1, False) do
    begin
      AssertTrue(I > 1);
      Inc(J);
    end;
  AssertTrue(J = 21);

  J := 0;
  for I in lst.Tail(0, False) do
    Inc(J);
  AssertTrue(J = 24);
end;

procedure TLiteSortedListTest.Range;
var
  lst: TIntList;
  a: TIntArray;
  I, J: Integer;
begin
  System.SetLength(a, 100);
  for I := 0 to System.High(a) do
    a[I] := I;
  TIntHelper.RandomShuffle(a);
  for I in a do
    lst.Add(I);
  I := 0;
  for J in lst{%H-}.Range(10, 10) do
    Inc(I);
  AssertTrue(I = 0);
  I := 0;
  for J in lst{%H-}.Range(10, 10, BOUNDS_BOTH) do
    Inc(I);
  AssertTrue(I = 1);
  I := 0;
  for J in lst{%H-}.Range(99, 99, []) do
    Inc(I);
  AssertTrue(I = 0);

  I := 10;
  for J in lst{%H-}.Range(10, 90) do
    begin
      AssertTrue(I = J);
      Inc(I);
    end;
  AssertTrue(I = 90);
end;

procedure TLiteSortedListTest.Range1;
var
  lst: TIntList;
  I, J: Integer;
begin
  J := 0;
  for I in lst.Range(0, 5) do
    Inc(J);
  AssertTrue(J = 0);

  for I := 0 to 2 do
    for J := 1 to 8 do
      lst.Add(J);
  AssertTrue(lst.Count = 24);

  J := 0;
  for I in lst.Range(9, 10) do
    Inc(J);
  AssertTrue(J = 0);

  for I in lst.Range(8, 8) do
    Inc(J);
  AssertTrue(J = 0);

  for I in lst.Range(8, 8, BOUNDS_BOTH) do
    begin
      AssertTrue(I = 8);
      Inc(J);
    end;
  AssertTrue(J = 3);

  J := 0;
  for I in lst.Range(5, 7) do
    begin
      AssertTrue((I >= 5) and (I < 7));
      Inc(J);
    end;
  AssertTrue(J = 6);

  J := 0;
  for I in lst.Range(5, 7, BOUNDS_BOTH) do
    begin
      AssertTrue((I >= 5) and (I <= 7));
      Inc(J);
    end;
  AssertTrue(J = 9);

  J := 0;
  for I in lst.Range(1, 8) do
    begin
      AssertTrue((I >= 1) and (I < 8));
      Inc(J);
    end;
  AssertTrue(J = 21);

  J := 0;
  for I in lst.Range(1, 8, BOUNDS_BOTH) do
    begin
      AssertTrue((I >= 1) and (I <= 8));
      Inc(J);
    end;
  AssertTrue(J = 24);

  J := 0;
  for I in lst.Range(0, 9) do
    Inc(J);
  AssertTrue(J = 24);
end;

procedure TLiteSortedListTest.FindFloor;
var
  lst: TIntList;
  I: Integer;
begin
  for I := 0 to 15 do
    lst.Add(I);
  AssertTrue(lst.IndexOfFloor(-2) = -1);
  AssertTrue(lst.IndexOfFloor(3) = 2);
  AssertTrue(lst.IndexOfFloor(3, True) = 3);
  AssertTrue(lst.IndexOfFloor(15) = 14);
  AssertTrue(lst.IndexOfFloor(15, True) = 15);
  AssertTrue(lst.IndexOfFloor(20) = 15);
end;

procedure TLiteSortedListTest.FindFloor1;
var
  lst: TIntList;
  I, J: Integer;
begin
  for J in [1..4] do
    for I := 0 to 15 do
      lst.Add(I);
  AssertTrue(lst.Count = 64);
  AssertTrue(lst.IndexOfFloor(-2) = -1);
  AssertTrue(lst.IndexOfFloor(0) = -1);
  AssertTrue(lst.IndexOfFloor(0, True) = 3);
  AssertTrue(lst.IndexOfFloor(3) = Pred(3*4));
  AssertTrue(lst.IndexOfFloor(3, True) = Pred(4*4));
  AssertTrue(lst.IndexOfFloor(15) = Pred(15*4));
  AssertTrue(lst.IndexOfFloor(15, True) = Pred(16*4));
  AssertTrue(lst.IndexOfFloor(20) = 63);
end;

procedure TLiteSortedListTest.FindCeil;
var
  lst: TIntList;
  I: Integer;
begin
  for I := 0 to 15 do
    lst.Add(I);
  AssertTrue(lst.IndexOfCeil(15) = 15);
  AssertTrue(lst.IndexOfCeil(15, False) = -1);
  AssertTrue(lst.IndexOfCeil(0) = 0);
  AssertTrue(lst.IndexOfCeil(0, False) = 1);
  AssertTrue(lst.IndexOfCeil(10) = 10);
  AssertTrue(lst.IndexOfCeil(10, False) = 11);
end;

procedure TLiteSortedListTest.FindCeil1;
var
  lst: TIntList;
  I, J: Integer;
begin
  for J in [1..4] do
    for I := 0 to 15 do
      lst.Add(I);
  AssertTrue(lst.Count = 64);
  AssertTrue(lst.IndexOfCeil(15) = 15*4);
  AssertTrue(lst.IndexOfCeil(15, False) = -1);
  AssertTrue(lst.IndexOfCeil(0) = 0);
  AssertTrue(lst.IndexOfCeil(0, False) = 1*4);
  AssertTrue(lst.IndexOfCeil(10) = 10*4);
  AssertTrue(lst.IndexOfCeil(10, False) = 11*4);
  AssertTrue(lst.IndexOfCeil(-1) = 0);
end;

procedure TLiteSortedListTest.PassByValue;
  procedure Test(aList: TIntList);
  begin
    aList.Add(6);
    aList.Add(2);
    AssertTrue(aList.NonEmpty);
  end;
var
  lst: TIntList;
begin
  lst.EnsureCapacity(12);
  AssertTrue(lst.IsEmpty);
  Test(lst);
  AssertTrue(lst.IsEmpty);
  lst.Add(7);
  Test(lst);
  AssertTrue(lst[0] = 7);
end;

{ TLiteComparableSortedListTest.TRec }

class operator TRec.<(const L, R: TRec): Boolean;
begin
  Result := L.Key < R.Key;
end;

constructor TRec.Create(k, v: Integer);
begin
  Key := k;
  Value := v;
end;

{ TLiteComparableSortedListTest }

procedure TLiteComparableSortedListTest.Add;
var
  lst: TIntList;
  I: Integer;
begin
  AssertTrue(lst.Count = 0);
  AssertTrue(lst.Add(1));
  AssertTrue(lst.Count = 1);
  AssertTrue(lst.Contains(1));
  AssertTrue(lst.Add(51));
  AssertTrue(lst.Count = 2);
  AssertTrue(lst.Contains(51));
  AssertTrue(lst.Add(52));
  AssertTrue(lst.Count = 3);
  AssertTrue(lst.Contains(52));
  for I := 1 to 100 do
    AssertTrue(lst.Add(I));
  AssertTrue(lst.Count = 103);
  for I := 1 to 100 do
    AssertTrue(lst.Contains(I));
end;

procedure TLiteComparableSortedListTest.AddRec;
var
  lst: TRecList;
  I: Integer;
  r: TRec;
const
  TestSize = 100;
begin
  lst.RejectDuplicates := True;
  for I := 1 to TestSize do
    lst.Add(TRec.Create(I, 0));
  AssertTrue(lst.Count = 100);
  AssertFalse(lst.Add(TRec.Create(TestSize, 0)));
  I := 0;
  for r in lst.Range(TRec.Create(10, 0), TRec.Create(30, 0)) do
    Inc(I);
  AssertTrue(I = 20);
end;

procedure TLiteComparableSortedListTest.Clear;
var
  lst: TIntList;
  I: Integer;
begin
  for I in IntArray21 do
    lst.Add(I);
  AssertTrue(lst.Count = 21);
  AssertTrue(lst.Capacity >= 21);
  lst.Clear;
  AssertTrue(lst.Count = 0);
  AssertTrue(lst.Capacity = 0);
end;

procedure TLiteComparableSortedListTest.EnsureCapacity;
var
  lst: TIntList;
begin
  AssertTrue(lst.Capacity = 0);
  lst.EnsureCapacity(100);
  AssertTrue(lst.Capacity >= 100);
end;

procedure TLiteComparableSortedListTest.TrimToFit;
var
  lst: TIntList;
  I: Integer;
begin
  for I in IntArray21 do
    lst.Add(I);
  AssertTrue(lst.Count = 21);
  lst.EnsureCapacity(255);
  AssertTrue(lst.Capacity = 256);
  lst.TrimToFit;
  AssertTrue(lst.Count = 21);
  AssertTrue(lst.Capacity = 21);
end;

procedure TLiteComparableSortedListTest.RejectDuplicates;
var
  lst: TIntList;
  I, J: Integer;
begin
  for I in IntArray21 do
    lst.Add(I);
  for I in IntArray21 do
    lst.Add(I);
  AssertTrue(lst.Count = Length(IntArray21) * 2);
  lst.RejectDuplicates := True;
  AssertTrue(lst.Count = Length(IntArray21));
  J := 1;
  for I in lst do
    begin
      AssertTrue(I = IntArray21[J]);
      Inc(J);
    end;
end;

procedure TLiteComparableSortedListTest.IndexOf;
var
  lst: TIntList;
  I: Integer;
begin
  for I in IntArray21 do
    lst.Add(I);
  for I in IntArray21 do
    AssertTrue(lst.IndexOf(I) = I - 1);

  AssertTrue(lst.IndexOf(0) = -1);
  AssertTrue(lst.IndexOf(22) = -1);
end;

procedure TLiteComparableSortedListTest.IndexOf1;
var
  lst: TIntList;
  I: Integer;
begin
  for I in IntArray21 do
    begin
      lst.Add(I);
      lst.Add(I);
      lst.Add(I);
    end;
  AssertTrue(lst.Count = Length(IntArray21) * 3);
  for I in IntArray21 do
    AssertTrue(lst.IndexOf(I) = (I - 1) * 3);
  AssertTrue(lst.IndexOf(0) = -1);
  AssertTrue(lst.IndexOf(22) = -1);
end;

procedure TLiteComparableSortedListTest.CountOf;
var
  lst: TIntList;
  I: Integer;
begin
  for I in IntArray11 do
    begin
      lst.Add(I);
      lst.Add(I);
      lst.Add(I);
    end;
  AssertTrue(lst.Count = Length(IntArray11) * 3);
  for I in IntArray11 do
    AssertTrue(lst.CountOf(I) = 3);
  AssertTrue(lst.CountOf(0) = 0);
  AssertTrue(lst.CountOf(12) = 0);
end;

procedure TLiteComparableSortedListTest.Remove;
var
  lst: TIntList;
  I: Integer;
begin
  for I in IntArray21 do
    lst.Add(I);
  AssertTrue(lst.Count = Length(IntArray21));
  AssertTrue(lst.Remove(IntArray21[1]));
  AssertTrue(lst.Count = Length(IntArray21) - 1);
  AssertTrue(lst.Remove(IntArray21[High(IntArray21)]));
  AssertTrue(lst.Count = Length(IntArray21) - 2);
  AssertFalse(lst.Remove(-1));
  AssertTrue(lst.Count = Length(IntArray21) - 2);
  AssertFalse(lst.Remove(100));
  AssertTrue(lst.Count = Length(IntArray21) - 2);
  AssertTrue(lst.Remove(IntArray21[2]));
  AssertTrue(lst.Count = Length(IntArray21) - 3);
  AssertTrue(lst.Remove(IntArray21[High(IntArray21) - 1]));
  AssertTrue(lst.Count = Length(IntArray21) - 4);
end;

procedure TLiteComparableSortedListTest.Insert;
var
  lst: TIntList;
  I: Integer;
begin
  for I := 1 to 21 do
    AssertTrue(lst.Insert(I) = I - 1);
  AssertTrue(lst.Count = 21);
  AssertTrue(lst.Insert(20) >= 0);
  AssertTrue(lst.Insert(20) >= 0);
  AssertTrue(lst.Insert(20) >= 0);
  AssertTrue(lst.Count = 24);
  lst.RejectDuplicates := True;
  AssertTrue(lst.Count = 21);
  AssertTrue(lst.Insert(19) < 0);
  AssertTrue(lst.Insert(20) < 0);
  AssertTrue(lst.Insert(21) < 0);
  AssertTrue(lst.Count = 21);
end;

procedure TLiteComparableSortedListTest.Delete;
var
  lst: TIntList;
  I: Integer;
begin
  for I := 0 to 7 do
    lst.Add(I);
  AssertTrue(lst.Count = 8);
  lst.Delete(0);
  AssertTrue(lst.Count = 7);
  AssertTrue(lst[0] = 1);
  lst.Delete(3);
  AssertTrue(lst.Count = 6);
  AssertTrue(lst[3] = 5);
  AssertTrue(TIntHelper.Same(lst.ToArray, [1, 2, 3, 5, 6, 7]));
end;

procedure TLiteComparableSortedListTest.DeleteOutOfBounds;
var
  lst: TIntList;
  Raised: Boolean;
begin
  Raised := False;
  try
    lst.Delete(0);
  except
    on ELGListError do
      Raised := True;
  end;
  AssertTrue(Raised);

  lst.Insert(12);
  AssertTrue(lst[0] = 12);
  Raised := False;
  try
    lst.Delete(1);
  except
    on ELGListError do
      Raised := True;
  end;

  AssertTrue(Raised);
  Raised := False;
  try
    lst.Delete(-1);
  except
    on ELGListError do
      Raised := True;
  end;
  AssertTrue(Raised);
end;

procedure TLiteComparableSortedListTest.TryDelete;
var
  lst: TIntList;
  I: Integer;
begin
  AssertFalse(lst.TryDelete(0));
  for I := 1 to 5 do
    lst.Add(I);
  AssertTrue(lst.Count = 5);
  AssertFalse(lst.TryDelete(-1));
  AssertFalse(lst.TryDelete(6));
  AssertTrue(lst.TryDelete(0));
  AssertTrue(lst.Count = 4);
  AssertTrue(lst.TryDelete(3));
  AssertTrue(lst.Count = 3);
  AssertTrue(lst.TryDelete(1));
  AssertTrue(lst.Count = 2);
end;

procedure TLiteComparableSortedListTest.Head;
var
  lst: TIntList;
  a: TIntArray;
  I, J: Integer;
begin
  System.SetLength(a, 100);
  for I := 0 to System.High(a) do
    a[I] := I;
  TIntHelper.RandomShuffle(a);
  for I in a do
    lst.Add(I);
  I := 0;
  for J in lst{%H-}.Head(0) do
    begin
      AssertTrue(I = J);
      Inc(I);
    end;
  AssertTrue(I = 0);
  I := 0;
  for J in lst{%H-}.Head(0, True) do
    begin
      AssertTrue(I = J);
      Inc(I);
    end;
  AssertTrue(I = 1);
  I := 0;
  for J in lst{%H-}.Head(80) do
    begin
      AssertTrue(I = J);
      Inc(I);
    end;
  AssertTrue(I = 80);
  I := 0;
  for J in lst{%H-}.Head(80, True) do
    begin
      AssertTrue(I = J);
      Inc(I);
    end;
  AssertTrue(I = 81);
end;

procedure TLiteComparableSortedListTest.Head1;
var
  lst: TIntList;
  I, J: Integer;
begin
  J := 0;
  for I in lst.Head(5) do
    Inc(J);
  AssertTrue(J = 0);

  for I := 0 to 2 do
    for J := 1 to 8 do
      lst.Add(J);
  AssertTrue(lst.Count = 24);

  J := 0;
  for I in lst.Head(0) do
    Inc(J);
  AssertTrue(J = 0);

  for I in lst.Head(1) do
    Inc(J);
  AssertTrue(J = 0);

  for I in lst.Head(1, True) do
    Inc(J);
  AssertTrue(J = 3);

  J := 0;
  for I in lst.Head(5) do
    begin
     AssertTrue(I < 5);
     Inc(J);
    end;
  AssertTrue(J = 12);

  J := 0;
  for I in lst.Head(5, True) do
    begin
     AssertTrue(I <= 5);
     Inc(J);
    end;
  AssertTrue(J = 15);

  J := 0;
  for I in lst.Head(8) do
    begin
     AssertTrue(I < 8);
     Inc(J);
    end;
  AssertTrue(J = 21);

  J := 0;
  for I in lst.Head(8, True) do
    begin
     AssertTrue(I <= 8);
     Inc(J);
    end;
  AssertTrue(J = 24);
end;

procedure TLiteComparableSortedListTest.Tail;
var
  lst: TIntList;
  a: TIntArray;
  I, J: Integer;
begin
  System.SetLength(a, 100);
  for I := 0 to System.High(a) do
    a[I] := I;
  TIntHelper.RandomShuffle(a);
  for I in a do
    lst.Add(I);
  I := 0;
  for J in lst.Tail(99) do
    Inc(I);
  AssertTrue(I = 1);
  I := 0;
  for J in lst.Tail(99, False) do
    Inc(I);
  AssertTrue(I = 0);
  I := 30;
  for J in lst.Tail(30) do
    begin
      AssertTrue(I = J);
      Inc(I);
    end;
  AssertTrue(I = 100);
end;

procedure TLiteComparableSortedListTest.Tail1;
var
  lst: TIntList;
  I, J: Integer;
begin
  J := 0;
  for I in lst.Tail(0) do
    Inc(J);
  AssertTrue(J = 0);

  for I := 0 to 2 do
    for J := 1 to 8 do
      lst.Add(J);
  AssertTrue(lst.Count = 24);

  J := 0;
  for I in lst.Tail(9) do
    Inc(J);
  AssertTrue(J = 0);

  for I in lst.Tail(8, False) do
    Inc(J);
  AssertTrue(J = 0);

  for I in lst.Tail(8) do
    Inc(J);
  AssertTrue(J = 3);

  J := 0;
  for I in lst.Tail(5) do
    begin
      AssertTrue(I >= 5);
      Inc(J);
    end;
  AssertTrue(J = 12);

  J := 0;
  for I in lst.Tail(5, False) do
    begin
      AssertTrue(I > 5);
      Inc(J);
    end;
  AssertTrue(J = 9);

  J := 0;
  for I in lst.Tail(1) do
    begin
      AssertTrue(I >= 1);
      Inc(J);
    end;
  AssertTrue(J = 24);

  J := 0;
  for I in lst.Tail(1, False) do
    begin
      AssertTrue(I > 1);
      Inc(J);
    end;
  AssertTrue(J = 21);

  J := 0;
  for I in lst.Tail(0, False) do
    Inc(J);
  AssertTrue(J = 24);
end;

procedure TLiteComparableSortedListTest.Range;
var
  lst: TIntList;
  a: TIntArray;
  I, J: Integer;
begin
  System.SetLength(a, 100);
  for I := 0 to System.High(a) do
    a[I] := I;
  TIntHelper.RandomShuffle(a);
  for I in a do
    lst.Add(I);
  I := 0;
  for J in lst{%H-}.Range(10, 10) do
    Inc(I);
  AssertTrue(I = 0);
  I := 0;
  for J in lst{%H-}.Range(10, 10, BOUNDS_BOTH) do
    Inc(I);
  AssertTrue(I = 1);
  I := 0;
  for J in lst{%H-}.Range(99, 99, []) do
    Inc(I);
  AssertTrue(I = 0);

  I := 10;
  for J in lst{%H-}.Range(10, 90) do
    begin
      AssertTrue(I = J);
      Inc(I);
    end;
  AssertTrue(I = 90);
end;

procedure TLiteComparableSortedListTest.Range1;
var
  lst: TIntList;
  I, J: Integer;
begin
  J := 0;
  for I in lst.Range(0, 5) do
    Inc(J);
  AssertTrue(J = 0);

  for I := 0 to 2 do
    for J := 1 to 8 do
      lst.Add(J);
  AssertTrue(lst.Count = 24);

  J := 0;
  for I in lst.Range(9, 10) do
    Inc(J);
  AssertTrue(J = 0);

  for I in lst.Range(8, 8) do
    Inc(J);
  AssertTrue(J = 0);

  for I in lst.Range(8, 8, BOUNDS_BOTH) do
    begin
      AssertTrue(I = 8);
      Inc(J);
    end;
  AssertTrue(J = 3);

  J := 0;
  for I in lst.Range(5, 7) do
    begin
      AssertTrue((I >= 5) and (I < 7));
      Inc(J);
    end;
  AssertTrue(J = 6);

  J := 0;
  for I in lst.Range(5, 7, BOUNDS_BOTH) do
    begin
      AssertTrue((I >= 5) and (I <= 7));
      Inc(J);
    end;
  AssertTrue(J = 9);

  J := 0;
  for I in lst.Range(1, 8) do
    begin
      AssertTrue((I >= 1) and (I < 8));
      Inc(J);
    end;
  AssertTrue(J = 21);

  J := 0;
  for I in lst.Range(1, 8, BOUNDS_BOTH) do
    begin
      AssertTrue((I >= 1) and (I <= 8));
      Inc(J);
    end;
  AssertTrue(J = 24);

  J := 0;
  for I in lst.Range(0, 9) do
    Inc(J);
  AssertTrue(J = 24);
end;

procedure TLiteComparableSortedListTest.FindFloor;
var
  lst: TIntList;
  I: Integer;
begin
  for I := 0 to 15 do
    lst.Add(I);
  AssertTrue(lst.IndexOfFloor(-2) = -1);
  AssertTrue(lst.IndexOfFloor(3) = 2);
  AssertTrue(lst.IndexOfFloor(3, True) = 3);
  AssertTrue(lst.IndexOfFloor(15) = 14);
  AssertTrue(lst.IndexOfFloor(15, True) = 15);
  AssertTrue(lst.IndexOfFloor(20) = 15);
end;

procedure TLiteComparableSortedListTest.FindFloor1;
var
  lst: TIntList;
  I, J: Integer;
begin
  for J in [1..4] do
    for I := 0 to 15 do
      lst.Add(I);
  AssertTrue(lst.Count = 64);
  AssertTrue(lst.IndexOfFloor(-2) = -1);
  AssertTrue(lst.IndexOfFloor(0) = -1);
  AssertTrue(lst.IndexOfFloor(0, True) = 3);
  AssertTrue(lst.IndexOfFloor(3) = Pred(3*4));
  AssertTrue(lst.IndexOfFloor(3, True) = Pred(4*4));
  AssertTrue(lst.IndexOfFloor(15) = Pred(15*4));
  AssertTrue(lst.IndexOfFloor(15, True) = Pred(16*4));
  AssertTrue(lst.IndexOfFloor(20) = 63);
end;

procedure TLiteComparableSortedListTest.FindCeil;
var
  lst: TIntList;
  I: Integer;
begin
  for I := 0 to 15 do
    lst.Add(I);
  AssertTrue(lst.IndexOfCeil(15) = 15);
  AssertTrue(lst.IndexOfCeil(15, False) = -1);
  AssertTrue(lst.IndexOfCeil(0) = 0);
  AssertTrue(lst.IndexOfCeil(0, False) = 1);
  AssertTrue(lst.IndexOfCeil(10) = 10);
  AssertTrue(lst.IndexOfCeil(10, False) = 11);
end;

procedure TLiteComparableSortedListTest.FindCeil1;
var
  lst: TIntList;
  I, J: Integer;
begin
  for J in [1..4] do
    for I := 0 to 15 do
      lst.Add(I);
  AssertTrue(lst.Count = 64);
  AssertTrue(lst.IndexOfCeil(15) = 15*4);
  AssertTrue(lst.IndexOfCeil(15, False) = -1);
  AssertTrue(lst.IndexOfCeil(0) = 0);
  AssertTrue(lst.IndexOfCeil(0, False) = 1*4);
  AssertTrue(lst.IndexOfCeil(10) = 10*4);
  AssertTrue(lst.IndexOfCeil(10, False) = 11*4);
  AssertTrue(lst.IndexOfCeil(-1) = 0);
end;

procedure TLiteComparableSortedListTest.PassByValue;
  procedure Test(aList: TIntList);
  begin
    aList.Add(6);
    aList.Add(2);
    AssertTrue(aList.NonEmpty);
  end;
var
  lst: TIntList;
begin
  lst.EnsureCapacity(12);
  AssertTrue(lst.Capacity > 2);
  AssertTrue(lst.IsEmpty);
  Test(lst);
  AssertTrue(lst.IsEmpty);
  lst.Add(7);
  Test(lst);
  AssertTrue(lst[0] = 7);
end;

{ TLiteHashListTest.TStrData }

constructor TLiteHashListTest.TStrData.Create(const aValue: string);
begin
  Value := aValue;
end;

class function TLiteHashListTest.TStrData.HashCode(const aValue: TStrData): SizeInt;
begin
  Result := string.HashCode(aValue.Value);
end;

class function TLiteHashListTest.TStrData.Equal(const L, R: TStrData): Boolean;
begin
  Result := string.Equal(L.Value, R.Value);
end;

{ TLiteHashListTest }

procedure TLiteHashListTest.Add;
var
  lst: TIntList;
  I: Integer;
begin
  AssertTrue(lst.Count = 0);
  AssertTrue(lst.Add(1) = 0);
  AssertTrue(lst.Count = 1);
  AssertTrue(lst.Contains(1));
  AssertTrue(lst.Add(51) = 1);
  AssertTrue(lst.Count = 2);
  AssertTrue(lst.Contains(51));
  AssertTrue(lst.Add(52) = 2);
  AssertTrue(lst.Count = 3);
  AssertTrue(lst.Contains(52));
  for I := 1 to 100 do
    AssertTrue(lst.Add(I) = I + 2);
  AssertTrue(lst.Count = 103);
  for I := 1 to 100 do
    AssertTrue(lst.Contains(I));
end;

procedure TLiteHashListTest.AddArray;
var
  lst: TIntList;
  I: Integer;
begin
  AssertTrue(lst.AddAll(IntArray21) = Length(IntArray21));
  AssertTrue(lst.Count = Length(IntArray21));
  for I in IntArray21 do
    AssertTrue(lst.Contains(I));
end;

procedure TLiteHashListTest.AddEnum;
var
  lst: TIntList;
  I: Integer;
begin
  AssertTrue(lst.AddAll(TIntArrayCursor.Create(TIntHelper.CreateCopy(IntArray21))) = Length(IntArray21));
  AssertTrue(lst.Count = Length(IntArray21));
  for I in IntArray21 do
    AssertTrue(lst.Contains(I));
end;

procedure TLiteHashListTest.AddUniq;
var
  lst: TIntList;
begin
  AssertTrue(lst.Count = 0);
  AssertTrue(lst.AddUniq(1));
  AssertTrue(lst.Count = 1);
  AssertTrue(lst.Contains(1));
  AssertFalse(lst.AddUniq(1));
  AssertTrue(lst.AddUniq(51));
  AssertTrue(lst.Count = 2);
  AssertTrue(lst.Contains(51));
  AssertFalse(lst.AddUniq(51));
  AssertTrue(lst.AddUniq(52));
  AssertTrue(lst.Count = 3);
  AssertTrue(lst.Contains(52));
  AssertFalse(lst.AddUniq(52));
end;

procedure TLiteHashListTest.AddUniqArray;
var
  lst: TIntList;
begin
  AssertTrue(lst.AddAllUniq(IntArray11) = Length(IntArray11));
  AssertTrue(lst.AddAllUniq(IntArray10) = Length(IntArray10));
  AssertTrue(lst.AddAllUniq(IntArray11) = 0);
  AssertTrue(lst.AddAllUniq(IntArray10) = 0);
  AssertTrue(lst.AddAllUniq(IntArray21) = 0);
end;

procedure TLiteHashListTest.AddUniqEnum;
var
  lst: TIntList;
begin
  AssertTrue(lst.AddAllUniq(TIntArrayCursor.Create(TIntHelper.CreateCopy(IntArray11))) = Length(IntArray11));
  AssertTrue(lst.AddAllUniq(TIntArrayCursor.Create(TIntHelper.CreateCopy(IntArray10))) = Length(IntArray10));
  AssertTrue(lst.AddAllUniq(TIntArrayCursor.Create(TIntHelper.CreateCopy(IntArray11))) = 0);
  AssertTrue(lst.AddAllUniq(TIntArrayCursor.Create(TIntHelper.CreateCopy(IntArray10))) = 0);
  AssertTrue(lst.AddAllUniq(TIntArrayCursor.Create(TIntHelper.CreateCopy(IntArray21))) = 0);
end;

procedure TLiteHashListTest.Clear;
var
  lst: TIntList;
begin
  lst.AddAll(IntArray21);
  AssertTrue(lst.Count = 21);
  AssertTrue(lst.Capacity >= 21);
  lst.Clear;
  AssertTrue(lst.Count = 0);
  AssertTrue(lst.Capacity = 0);
end;

procedure TLiteHashListTest.EnsureCapacity;
var
  lst: TIntList;
begin
  AssertTrue(lst.Capacity = 0);
  lst.EnsureCapacity(100);
  AssertTrue(lst.Capacity >= 100);
end;

procedure TLiteHashListTest.TrimToFit;
var
  lst: TIntList;
begin
  lst.AddAll(IntArray21);
  AssertTrue(lst.Count = 21);
  lst.EnsureCapacity(255);
  AssertTrue(lst.Capacity = 256);
  lst.TrimToFit;
  AssertTrue(lst.Count = 21);
  AssertTrue(lst.Capacity = lgUtils.RoundUpTwoPower(lst.Count));
end;

procedure TLiteHashListTest.GetEnumerator;
var
  lst: TIntList;
  I, J: Integer;
begin
  lst.AddAll(IntArray21);
  J := 1;
  for I in lst do
    begin
      AssertTrue(I = IntArray21[J]);
      Inc(J);
    end;
end;

procedure TLiteHashListTest.ToArray;
var
  lst: TIntList;
  a: TIntArray;
begin
  lst.AddAll(IntArray21);
  a := lst.ToArray;
  AssertTrue(TIntHelper.Same(a, IntArray21));
end;

procedure TLiteHashListTest.Reverse;
var
  lst: TIntList;
  I, J: Integer;
begin
  lst.AddAll(IntArray21);
  J := 21;
  for I in lst.Reverse do
    begin
      AssertTrue(I = IntArray21[J]);
      Dec(J);
    end;
  AssertTrue(J = 0);
end;

procedure TLiteHashListTest.IndexOf;
var
  lst: TIntList;
  I: Integer;
begin
  lst.AddAll(IntArray21);
  for I in IntArray21 do
    AssertTrue(lst.IndexOf(I) = I - 1);
  AssertTrue(lst.IndexOf(0) = -1);
  AssertTrue(lst.IndexOf(22) = -1);
end;

procedure TLiteHashListTest.CountOf;
var
  lst: TIntList;
  I: Integer;
begin
  lst.AddAll(IntArray11);
  lst.AddAll(IntArray11);
  lst.AddAll(IntArray11);
  AssertTrue(lst.Count = Length(IntArray11) * 3);
  for I in IntArray11 do
    AssertTrue(lst.CountOf(I) = 3);
  AssertTrue(lst.CountOf(0) = 0);
  AssertTrue(lst.CountOf(12) = 0);
end;

procedure TLiteHashListTest.Remove;
var
  lst: TIntList;
begin
  lst.AddAll(IntArray21);
  AssertTrue(lst.Count = Length(IntArray21));
  AssertTrue(lst.Remove(IntArray21[1]));
  AssertTrue(lst.Count = Length(IntArray21) - 1);
  AssertTrue(lst.Remove(IntArray21[High(IntArray21)]));
  AssertTrue(lst.Count = Length(IntArray21) - 2);
  AssertFalse(lst.Remove(-1));
  AssertTrue(lst.Count = Length(IntArray21) - 2);
  AssertFalse(lst.Remove(100));
  AssertTrue(lst.Count = Length(IntArray21) - 2);
  AssertTrue(lst.Remove(IntArray21[2]));
  AssertTrue(lst.Count = Length(IntArray21) - 3);
  AssertTrue(lst.Remove(IntArray21[High(IntArray21) - 1]));
  AssertTrue(lst.Count = Length(IntArray21) - 4);
end;

procedure TLiteHashListTest.Remove_1;
var
  lst: TIntList;
  I, TestSize: Integer;
begin
  TestSize := 257;
  for I := 0 to Pred(TestSize) do
    lst.Add(I);
  AssertTrue(lst.Count = TestSize);
  for I := 0 to Pred(TestSize) do
    AssertTrue(lst.Contains(I));
  for I := 0 to Pred(TestSize) do
    AssertTrue(lst.Remove(I));
  AssertTrue(lst.IsEmpty);
end;

procedure TLiteHashListTest.Remove_2;
var
  lst: specialize TGLiteHashList<TStrData, TStrData>;
  I: Integer;
begin
  for I := 1 to 100 do
    lst.Add(TStrData.Create('key ' + I.ToString));
  AssertTrue(lst.Count = 100);
  for I := 100 downto 1 do
    AssertTrue(lst.Remove(TStrData.Create('key ' + I.ToString)));
  AssertTrue(lst.IsEmpty);
  for I := 1 to 100 do
    lst.Add(TStrData.Create('key ' + I.ToString));
  AssertTrue(lst.Count = 100);
  for I := 1 to 100 do
    AssertTrue(lst.Remove(TStrData.Create('key ' + I.ToString)));
  AssertTrue(lst.IsEmpty);
end;

procedure TLiteHashListTest.Insert;
var
  lst: TIntList;
  I: Integer;
begin
  lst.AddAll(IntArray21);
  lst.Insert(20, 22);
  lst.Insert(20, 23);
  lst.Insert(20, 24);
  AssertTrue(lst.Count = 24);
  for I := 1 to 20 do
    AssertTrue(lst[I - 1] = I);
  AssertTrue(lst[20] = 24);
  AssertTrue(lst[21] = 23);
  AssertTrue(lst[22] = 22);
  AssertTrue(lst[23] = 21);
end;

procedure TLiteHashListTest.Delete;
var
  lst: TIntList;
begin
  lst.AddAll([0, 1, 2, 3, 4, 5, 6, 7]);
  lst.Delete(0);
  AssertTrue(lst.Count = 7);
  AssertTrue(lst[0] = 1);
  lst.Delete(3);
  AssertTrue(lst.Count = 6);
  AssertTrue(lst[3] = 5);
  AssertTrue(TIntHelper.Same(lst.ToArray, [1, 2, 3, 5, 6, 7]));
end;

procedure TLiteHashListTest.DeleteOutOfBounds;
var
  lst: TIntList;
  Raised: Boolean;
begin
  Raised := False;
  try
    lst.Delete(0);
  except
    on ELGListError do
      Raised := True;
  end;
  AssertTrue(Raised);

  lst.Add(12);
  AssertTrue(lst[0] = 12);
  Raised := False;
  try
    lst.Delete(1);
  except
    on ELGListError do
      Raised := True;
  end;
  AssertTrue(Raised);

  Raised := False;
  try
    lst.Delete(-1);
  except
    on ELGListError do
      Raised := True;
  end;
  AssertTrue(Raised);
end;

procedure TLiteHashListTest.PassByValue;
  procedure Test(aList: TIntList);
  begin
    aList.Add(6);
    aList.Add(2);
    AssertTrue(aList.NonEmpty);
  end;
var
  lst: TIntList;
begin
  lst.EnsureCapacity(12);
  AssertTrue(lst.Capacity > 2);
  AssertTrue(lst.IsEmpty);
  Test(lst);
  AssertTrue(lst.IsEmpty);
  lst.Add(7);
  AssertTrue(lst.IndexOf(2) = -1);
end;

{ TLiteHashList2Test.TStrEntry }

constructor TLiteHashList2Test.TStrEntry.Create(const aValue: string);
begin
  Value := aValue;
end;

{ TLiteHashList2Test.TIntEntry }

constructor TLiteHashList2Test.TIntEntry.Create(aKey: Integer; const aValue: string);
begin
  Key := aKey;
  Value := aValue;
end;

{ TLiteHashList2Test }

procedure TLiteHashList2Test.Add;
var
  lst: TIntList;
  I: Integer;
begin
  AssertTrue(lst.Count = 0);
  AssertTrue(lst.Add(TEntry.Create(1, 1)) = 0);
  AssertTrue(lst.Count = 1);
  AssertTrue(lst.Contains(1));
  AssertTrue(lst.Add(TEntry.Create(51, 2)) = 1);
  AssertTrue(lst.Count = 2);
  AssertTrue(lst.Contains(51));
  AssertTrue(lst.Add(TEntry.Create(51, 3)) = 2);
  AssertTrue(lst.Count = 3);
  AssertTrue(lst.Contains(51));
  for I := 1 to 100 do
    AssertTrue(lst.Add(TEntry.Create(I, 42)) = I + 2);
  AssertTrue(lst.Count = 103);
  for I := 1 to 100 do
    AssertTrue(lst.Contains(I));
end;

procedure TLiteHashList2Test.AddArray;
var
  lst: TIntList;
  I: Integer;
  a: array of TEntry = nil;
  e: TEntry;
begin
  SetLength(a, 42);
  for I := 0 to High(a) do
    a[I] := TEntry.Create(I, I);
  AssertTrue(lst.AddAll(a) = Length(a));
  AssertTrue(lst.Count = Length(a));
  for e in a do
    AssertTrue(lst.Contains(e.Key));
end;

procedure TLiteHashList2Test.AddEnum;
var
  lst: TIntList;
  I: Integer;
  a: array of TEntry = nil;
  e: TEntry;
begin
  SetLength(a, 42);
  for I := 0 to High(a) do
    a[I] := TEntry.Create(I, I);
  AssertTrue(lst.AddAll(TEntryCursor.Create(a)) = Length(a));
  AssertTrue(lst.Count = Length(a));
  for e in a do
    AssertTrue(lst.Contains(e.Key));
end;

procedure TLiteHashList2Test.AddUniq;
var
  lst: TIntList;
  I: Integer;
begin
  lst.Add(TEntry.Create(42, 0));
  AssertTrue(lst.Count = 1);
  for I := 1 to 42 do
    AssertFalse(lst.AddUniq(TEntry.Create(42, I)));
  AssertTrue(lst.Count = 1);
end;

procedure TLiteHashList2Test.AddUniqArray;
var
  lst: TIntList;
  I: Integer;
  a: array of TEntry = nil;
begin
  SetLength(a, 42);
  for I := 0 to High(a) do
    a[I] := TEntry.Create(I, I);
  AssertTrue(lst.AddAllUniq(a) = Length(a));
  AssertTrue(lst.Count = Length(a));
  AssertTrue(lst.AddAllUniq(a) = 0);
  AssertTrue(lst.Count = Length(a));
end;

procedure TLiteHashList2Test.Clear;
var
  lst: TIntList;
  I: Integer;
begin
  AssertTrue(lst.Count = 0);
  AssertTrue(lst.Capacity = 0);
  lst.Clear;
  AssertTrue(lst.Count = 0);
  for I := 1 to 42 do
    lst.Add(TEntry.Create(I, I));
  AssertTrue(lst.Count = 42);
  AssertTrue(lst.Capacity > 0);
  lst.Clear;
  AssertTrue(lst.Count = 0);
  AssertTrue(lst.Capacity = 0);
end;

procedure TLiteHashList2Test.MakeEmpty;
var
  lst: TIntList;
  I, c: Integer;
begin
  for I := 1 to 42 do
    lst.Add(TEntry.Create(I, I));
  AssertTrue(lst.Count = 42);
  c := lst.Capacity;
  AssertTrue(c > 0);
  lst.MakeEmpty;
  AssertTrue(lst.Count = 0);
  AssertTrue(lst.Capacity = c);
  for I := 43 to 84 do
    lst.Add(TEntry.Create(I, I));
  AssertTrue(lst.Count = 42);
  AssertTrue(lst.Capacity = c);
end;

procedure TLiteHashList2Test.EnsureCapacity;
var
  lst: TIntList;
begin
  AssertTrue(lst.Capacity = 0);
  lst.EnsureCapacity(3);
  AssertTrue(lst.Capacity = 4);
  lst.EnsureCapacity(42);
  AssertTrue(lst.Capacity = 64);
  lst.EnsureCapacity(63);
  AssertTrue(lst.Capacity = 64);
end;

procedure TLiteHashList2Test.GetEnumerator;
var
  lst: TIntList;
  I, J: Integer;
  a: array of TEntry = nil;
  e: TEntry;
begin
  J := 0;
  for e in lst do
    Inc(J);
  AssertTrue(J = 0);
  SetLength(a, 42);
  for I := 0 to High(a) do
    a[I] := TEntry.Create(I, I);
  lst.AddAll(a);
  for e in lst do
    begin
      AssertTrue((e.Key = a[J].Key) and (e.Value = a[J].Value));
      Inc(J);
    end;
  AssertTrue(J = lst.Count);
end;

procedure TLiteHashList2Test.ToArray;
var
  lst: TIntList;
  I: Integer;
  a, r: array of TEntry;
begin
  r := lst.ToArray;
  AssertTrue(r = nil);
  SetLength(a, 42);
  for I := 0 to High(a) do
    a[I] := TEntry.Create(I, I);
  lst.AddAll(a);
  r := lst.ToArray;
  AssertTrue(Length(r) = Length(a));
  for I := 0 to High(a) do
    AssertTrue((r[I].Key = a[I].Key) and (r[I].Value = a[I].Value));
end;

procedure TLiteHashList2Test.Reverse;
var
  lst: TIntList;
  I: Integer;
  a: array of TEntry;
  e: TEntry;
begin
  I := 0;
  for e in lst.Reverse do
    Inc(I);
  AssertTrue(I = 0);
  SetLength(a, 42);
  for I := 0 to High(a) do
    a[I] := TEntry.Create(I, I);
  lst.AddAll(a);
  I := lst.Count - 1;
  for e in lst.Reverse do
    begin
      AssertTrue((e.Key = a[I].Key) and (e.Value = a[I].Value));
      Dec(I);
    end;
  AssertTrue(I = -1);
end;

procedure TLiteHashList2Test.IdenticalKeys;
var
  lst: TIntList;
  I: Integer;
  e: TEntry;
  s1, s2: set of 1..42;
begin
  I := 0;
  for e in lst.EqualKeys(1) do
    Inc(I);
  AssertTrue(I = 0);
  s1 := [];
  for I := 1 to 42 do
    begin
      lst.Add(TEntry.Create(1, I));
      Include(s1, I);
    end;
  AssertTrue(s1 = [1..42]);
  s2 := [];
  for I := 1 to 42 do
    begin
      lst.Add(TEntry.Create(2, I));
      Include(s2, I);
    end;
  AssertTrue(s2 = [1..42]);
  I := 0;
  for e in lst.EqualKeys(1) do
    begin
      AssertTrue(e.Key = 1);
      Exclude(s1, e.Value);
      Inc(I);
    end;
  AssertTrue(I = 42);
  AssertTrue(s1 = []);
  I := 0;
  for e in lst.EqualKeys(2) do
    begin
      AssertTrue(e.Key = 2);
      Exclude(s2, e.Value);
      Inc(I);
    end;
  AssertTrue(I = 42);
  AssertTrue(s2 = []);
end;

procedure TLiteHashList2Test.IndexOf;
var
  lst: TIntList;
  I, J: Integer;
  e: TEntry;
begin
  I := lst.IndexOf(1);
  AssertTrue(I = -1);
  for I := 1 to 42 do
    lst.Add(TEntry.Create(I, I));
  for I := 1 to 42 do
    begin
      J := lst.IndexOf(I);
      AssertTrue(J = I-1);
      e := lst[J];
      AssertTrue((e.Key = I)and(e.Value = I));
    end;
end;

procedure TLiteHashList2Test.CountOf;
var
  lst: TIntList;
  I: Integer;
begin
  for I := 1 to 42 do
    lst.Add(TEntry.Create(I, I));
  for I := 1 to 42 do
   AssertTrue(lst.CountOf(I) = 1);
  for I := 1 to 42 do
    lst.Add(TEntry.Create(I, I));
  for I := 1 to 42 do
   AssertTrue(lst.CountOf(I) = 2);
  for I := 1 to 42 do
    lst.Add(TEntry.Create(I, I));
  for I := 1 to 42 do
   AssertTrue(lst.CountOf(I) = 3);
end;

procedure TLiteHashList2Test.Remove;
var
  lst: specialize TGLiteHashList2<string, TStrEntry, string>;
  e: TStrEntry;
  I: Integer;
begin
  for I := 1 to 100 do
    lst.Add(TStrEntry.Create('key ' + I.ToString));
  AssertTrue(lst.Count = 100);
  for I := 100 downto 1 do
    AssertTrue(lst.Remove('key ' + I.ToString, e));
  AssertTrue(lst.IsEmpty);
  for I := 1 to 100 do
    lst.Add(TStrEntry.Create('key ' + I.ToString));
  AssertTrue(lst.Count = 100);
  for I := 1 to 100 do
    AssertTrue(lst.Remove('key ' + I.ToString, e));
  AssertTrue(lst.IsEmpty);
end;

procedure TLiteHashList2Test.ContainsUniq;
var
  lst: specialize TGLiteHashList2<Integer, TIntEntry, Integer>;
  I: Integer;
begin
  AssertFalse(lst.ContainsUniq(0));
  for I := 1 to 100 do
    lst.Add(TIntEntry.Create(I, 'item ' + I.ToString));
  AssertTrue(lst.Count = 100);
  for I := 1 to 100 do
    AssertTrue(lst.ContainsUniq(I));
  AssertFalse(lst.ContainsUniq(0));
  AssertFalse(lst.ContainsUniq(101));
  for I := 1 to 100 do
    lst.Add(TIntEntry.Create(I, 'item ' + I.ToString));
  for I := 1 to 100 do
    AssertFalse(lst.ContainsUniq(I));
end;

procedure TLiteHashList2Test.FindUniq;
var
  lst: specialize TGLiteHashList2<Integer, TIntEntry, Integer>;
  p: ^TIntEntry;
  I: Integer;
begin
  AssertTrue(lst.FindUniq(0) = nil);
  for I := 1 to 100 do
    lst.Add(TIntEntry.Create(I, 'item ' + I.ToString));
  for I := 1 to 100 do
    begin
      p := lst.FindUniq(I);
      AssertTrue(p <> nil);
      AssertTrue((p^.Key = I) and (p^.Value = 'item ' + I.ToString));
    end;
  for I := 1 to 100 do
    lst.Add(TIntEntry.Create(I, 'item ' + I.ToString));
  for I := 1 to 100 do
    AssertTrue(lst.FindUniq(I) = nil);
end;

procedure TLiteHashList2Test.PassByValue;
  procedure Test(aList: TIntList);
  begin
    aList.EnsureCapacity(20);
    aList.Add(TEntry.Create(6, 6));
    aList.Add(TEntry.Create(2, 2));
    AssertTrue(aList.NonEmpty);
    AssertTrue(aList.Capacity = 32);
  end;
var
  lst: TIntList;
begin
  lst.EnsureCapacity(10);
  AssertTrue(lst.Capacity = 16);
  AssertTrue(lst.IsEmpty);
  lst.Add(TEntry.Create(5, 5));
  lst.Add(TEntry.Create(7, 7));
  Test(lst);
  AssertTrue(lst.Count = 2);
  AssertTrue(lst.IndexOf(2) = -1);
  AssertTrue(lst.IndexOf(6) = -1);
  AssertTrue(lst.IndexOf(5) <> -1);
  AssertTrue(lst.IndexOf(7) <> -1);
end;

initialization
  RegisterTest(TGSortedListTest);
  RegisterTest(TLiteSortedListTest);
  RegisterTest(TLiteComparableSortedListTest);
  RegisterTest(TLiteHashListTest);
  RegisterTest(TLiteHashList2Test);
end.

