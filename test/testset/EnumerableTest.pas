unit EnumerableTest;

{$mode objfpc}{$H+}
{$MODESWITCH NESTEDPROCVARS}

interface

uses

  SysUtils, fpcunit, testregistry,
  LGUtils,
  LGAbstractContainer,
  LGArrayHelpers;

type

  TPair            = record Key, Value: Integer end;
  TPairArray       = array of TPair;
  TPairArrayCursor = specialize TGArrayCursor<TPair>;
  IPairEnumerable  = specialize IGEnumerable<TPair>;

  { TEnumerableTest }

  TEnumerableTest = class(TTestCase)
  private
  type

    TIntEnumerable  = specialize TGEnumerable<Integer>;
    TIntArrayCursor = specialize TGArrayCursor<Integer>;
    IIntEnumerable  = TIntEnumerable.IEnumerable;
    TIntHelper      = specialize TGComparableArrayHelper<Integer>;
    TIntArray21     = array[1..21] of Integer;
    TIntArray10     = array[1..10] of Integer;
    TIntArray11     = array[1..11] of Integer;
    TIntOptional    = TIntEnumerable.TOptional;

  const

    IntStrictInc21: TIntArray21 = (
      1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21);
    IntStrictDec21: TIntArray21 = (
      21, 20, 19, 18, 17, 16, 15, 14, 13, 12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1);
    IntEvenInc10: TIntArray10   = (2, 4, 6, 8, 10, 12, 14, 16, 18, 20);
    IntOddInc11: TIntArray11    = (1, 3, 5, 7, 9, 11, 13, 15, 17, 19, 21);
    IntMul4Inc21: TIntArray21   = (
      4, 8, 12, 16, 20, 24, 28, 32, 36, 40, 44, 48, 52, 56, 60, 64, 68, 72, 76, 80, 84);
  var
    FCounter: Integer;
    function DoIntCmp(constref L, R: Integer): Boolean;
    function GetIsEven(constref aValue: Integer): Boolean;
    function DoMulBy4(constref aValue: Integer): Integer;
    function DoAddSquare(constref X, Y: Integer): Integer;
    function PairCmp(constref L, R: TPair): Boolean;
    function CreatePairArray(aKeyCount, aPairCount: Integer): TPairArray;
    procedure DoIncCounter(constref aValue: Integer);
    function LessThen15(constref aValue: Integer): Boolean;
  published
    procedure EmptyEnum_ToArray;
    procedure Enum21_ToArray;

    procedure EmptyEnum_Any;
    procedure Enum21_Any;

    procedure EmptyEnum_None;
    procedure Enum21_None;

    procedure EmptyEnum_Total;
    procedure Enum21_Total;

    procedure EmptyEnum_FindFirst;
    procedure Enum21_FindFirst;

    procedure EmptyEnum_First;
    procedure Enum21_First;

    procedure EmptyEnum_FindLast;
    procedure Enum21_FindLast;

    procedure EmptyEnum_Last;
    procedure Enum21_Last;

    procedure EmptyEnum_FindMinRegular;
    procedure Enum21_FindMinRegular;

    procedure EmptyEnum_FindMinDelegated;
    procedure Enum21_FindMinDelegated;

    procedure EmptyEnum_FindMinNested;
    procedure Enum21_FindMinNested;

    procedure EmptyEnum_MinRegular;
    procedure Enum21_MinRegular;

    procedure EmptyEnum_MinDelegated;
    procedure Enum21_MinDelegated;

    procedure EmptyEnum_MinNested;
    procedure Enum21_MinNested;

    procedure EmptyEnum_FindMaxRegular;
    procedure Enum21_FindMaxRegular;

    procedure EmptyEnum_FindMaxDelegated;
    procedure Enum21_FindMaxDelegated;

    procedure EmptyEnum_FindMaxNested;
    procedure Enum21_FindMaxNested;

    procedure EmptyEnum_MaxRegular;
    procedure Enum21_MaxRegular;

    procedure EmptyEnum_MaxDelegated;
    procedure Enum21_MaxDelegated;

    procedure EmptyEnum_MaxNested;
    procedure Enum21_MaxNested;

    procedure Reverse;

    procedure EmptyEnum_Skip0;
    procedure Enum21_Skip0;
    procedure Enum21_Skip3;
    procedure Enum21_SkipNeg;
    procedure Enum21_SkipAll;

    procedure EmptyEnum_Limit1;
    procedure Enum21_Limit0;
    procedure Enum21_Limit5;
    procedure Enum21_Limit23;

    procedure EmptyEnum_SortedRegular;
    procedure Enum21_SortedRegular;
    procedure Enum21_SortedRegularDesc;

    procedure EmptyEnum_SortedDelegated;
    procedure Enum21_SortedDelegated;
    procedure Enum21_SortedDelegatedDesc;

    procedure EmptyEnum_SortedNested;
    procedure Enum21_SortedNested;
    procedure Enum21_SortedNestedDesc;

    procedure EmptyEnum_SelectRegular;
    procedure Enum21_SelectRegular;
    procedure Enum11_SelectRegular;

    procedure EmptyEnum_SelectDelegated;
    procedure Enum21_SelectDelegated;
    procedure Enum11_SelectDelegated;

    procedure EmptyEnum_SelectNested;
    procedure Enum21_SelectNested;
    procedure Enum11_SelectNested;

    procedure EmptyEnum_AnyRegular;
    procedure Enum21_AnyRegular;
    procedure Enum11_AnyRegular;

    procedure EmptyEnum_AnyDelegated;
    procedure Enum21_AnyDelegated;
    procedure Enum11_AnyDelegated;

    procedure EmptyEnum_AnyNested;
    procedure Enum21_AnyNested;
    procedure Enum11_AnyNested;

    procedure EmptyEnum_NoneRegular;
    procedure Enum21_NoneRegular;
    procedure Enum11_NoneRegular;

    procedure EmptyEnum_NoneDelegated;
    procedure Enum21_NoneDelegated;
    procedure Enum11_NoneDelegated;

    procedure EmptyEnum_NoneNested;
    procedure Enum21_NoneNested;
    procedure Enum11_NoneNested;

    procedure EmptyEnum_AllRegular;
    procedure Enum21_AllRegular;
    procedure Enum10_AllRegular;

    procedure EmptyEnum_AllDelegated;
    procedure Enum21_AllDelegated;
    procedure Enum10_AllDelegated;

    procedure EmptyEnum_AllNested;
    procedure Enum21_AllNested;
    procedure Enum10_AllNested;

    procedure EmptyEnum_TotalRegular;
    procedure Enum21_TotalRegular;
    procedure Enum11_TotalRegular;

    procedure EmptyEnum_TotalDelegated;
    procedure Enum21_TotalDelegated;
    procedure Enum11_TotalDelegated;

    procedure EmptyEnum_TotalNested;
    procedure Enum21_TotalNested;
    procedure Enum11_TotalNested;

    procedure EmptyEnum_MapRegular;
    procedure Enum21_MapRegular;
    procedure Enum10_MapRegular;

    procedure EmptyEnum_MapDelegated;
    procedure Enum21_MapDelegated;
    procedure Enum10_MapDelegated;

    procedure EmptyEnum_MapNested;
    procedure Enum21_MapNested;
    procedure Enum10_MapNested;

    procedure EmptyEnum_FoldRegularWithV0;
    procedure Enum21_FoldRegularWithV0;
    procedure EmptyEnum_FoldRegular;
    procedure Enum21_FoldRegular;

    procedure EmptyEnum_FoldDelegatedWithV0;
    procedure Enum21_FoldDelegatedWithV0;
    procedure EmptyEnum_FoldDelegated;
    procedure Enum21_FoldDelegated;

    procedure EmptyEnum_FoldNestedWithV0;
    procedure Enum21_FoldNestedWithV0;
    procedure EmptyEnum_FoldNested;
    procedure Enum21_FoldNested;

    procedure SortedRegular;
    procedure SortedDelegated;
    procedure SortedNested;

    procedure ForEachRegular;
    procedure ForEachDelegated;
    procedure ForEachNested;

    procedure TakeWhileRegular;
    procedure TakeWhileDelegated;
    procedure TakeWhileNested;

    procedure SkipWhileRegular;
    procedure SkipWhileDelegated;
    procedure SkipWhileNested;
  end;

implementation
{$B-}{$COPERATORS ON}

var
  Counter: Integer;

function IntCmp(constref L, R: Integer): Boolean;
begin
  Result := L < R;
end;

function IsEven(constref aValue: Integer): Boolean;
begin
  Result := not Odd(aValue);
end;

function MulBy4(constref aValue: Integer): Integer;
begin
  Result := aValue * 4;
end;

function AddSquare(constref X, Y: Integer): Integer;
begin
  Result := X * X + Y;
end;

function PairCompare(constref L, R: TPair): Boolean;
begin
  Result := L.Key < R.Key;
end;

procedure IncCounter(constref aValue: Integer);
begin
  assert(aValue = aValue);
  Inc(Counter);
end;

function LessThen15(constref aValue: Integer): Boolean;
begin
  Result := aValue < 15;
end;

function TEnumerableTest.DoIntCmp(constref L, R: Integer): Boolean;
begin
  Result := L < R;
end;

function TEnumerableTest.GetIsEven(constref aValue: Integer): Boolean;
begin
  Result := not Odd(aValue);
end;

function TEnumerableTest.DoMulBy4(constref aValue: Integer): Integer;
begin
  Result := aValue * 4;
end;

function TEnumerableTest.DoAddSquare(constref X, Y: Integer): Integer;
begin
  Result := X * X + Y;
end;

function TEnumerableTest.PairCmp(constref L, R: TPair): Boolean;
begin
  Result := L.Key < R.Key;
end;

function TEnumerableTest.CreatePairArray(aKeyCount, aPairCount: Integer): TPairArray;
var
  Counter: array of Integer;
  I, Key: Integer;
begin
  SetLength(Result, aPairCount);
  SetLength(Counter, aKeyCount);
  Key := 0;
  for I := 0 to Pred(aKeyCount) do
    begin
      Counter[I] := Key;
      Key += aKeyCount;
    end;
  for I := 0 to System.High(Result) do
    begin
      Key := Random(aKeyCount);
      Result[I].Key := Key;
      Result[I].Value := Counter[Key];
      Inc(Counter[Key]);
    end;
end;

procedure TEnumerableTest.DoIncCounter(constref aValue: Integer);
begin
  assert(aValue = aValue);
  Inc(FCounter);
end;

function TEnumerableTest.LessThen15(constref aValue: Integer): Boolean;
begin
  Result := aValue < 15;
end;

procedure TEnumerableTest.EmptyEnum_ToArray;
var
  e: IIntEnumerable;
begin
  e := TIntArrayCursor.Create(nil);
  AssertTrue(e.ToArray = nil);
end;

procedure TEnumerableTest.Enum21_ToArray;
var
  e: IIntEnumerable;
begin
  e := TIntArrayCursor.Create(TIntHelper.CreateCopy(IntStrictInc21));
  AssertTrue(TIntHelper.Same(e.ToArray, IntStrictInc21));
end;

procedure TEnumerableTest.EmptyEnum_Any;
var
  e: IIntEnumerable;
begin
  e := TIntArrayCursor.Create(nil);
  AssertFalse(e.Any);
end;

procedure TEnumerableTest.Enum21_Any;
var
  e: IIntEnumerable;
begin
  e := TIntArrayCursor.Create(TIntHelper.CreateCopy(IntStrictInc21));
  AssertTrue(e.Any);
end;

procedure TEnumerableTest.EmptyEnum_None;
var
  e: IIntEnumerable;
begin
  e := TIntArrayCursor.Create(nil);
  AssertTrue(e.None);
end;

procedure TEnumerableTest.Enum21_None;
var
  e: IIntEnumerable;
begin
  e := TIntArrayCursor.Create(TIntHelper.CreateCopy(IntStrictInc21));
  AssertFalse(e.None);
end;

procedure TEnumerableTest.EmptyEnum_Total;
var
  e: IIntEnumerable;
begin
  e := TIntArrayCursor.Create(nil);
  AssertTrue(e.Total = 0);
end;

procedure TEnumerableTest.Enum21_Total;
var
  e: IIntEnumerable;
begin
  e := TIntArrayCursor.Create(TIntHelper.CreateCopy(IntStrictInc21));
  AssertTrue(e.Total = 21);
end;

procedure TEnumerableTest.EmptyEnum_FindFirst;
var
  e: IIntEnumerable;
  v: Integer;
begin
  e := TIntArrayCursor.Create(nil);
  AssertFalse(e.FindFirst(v));
end;

procedure TEnumerableTest.Enum21_FindFirst;
var
  e: IIntEnumerable;
  v: Integer;
begin
  e := TIntArrayCursor.Create(TIntHelper.CreateCopy(IntStrictInc21));
  AssertTrue(e.FindFirst(v));
  AssertTrue(v = 1);
end;

procedure TEnumerableTest.EmptyEnum_First;
var
  e: IIntEnumerable;
begin
  e := TIntArrayCursor.Create(nil);
  AssertFalse(e.First.Assigned);
end;

procedure TEnumerableTest.Enum21_First;
var
  e: IIntEnumerable;
  o: TIntOptional;
begin
  e := TIntArrayCursor.Create(TIntHelper.CreateCopy(IntStrictInc21));
  o := e.First;
  AssertTrue(o.Assigned);
  AssertTrue(o.Value = 1);
end;

procedure TEnumerableTest.EmptyEnum_FindLast;
var
  e: IIntEnumerable;
  v: Integer;
begin
  e := TIntArrayCursor.Create(nil);
  AssertFalse(e.FindLast(v));
end;

procedure TEnumerableTest.Enum21_FindLast;
var
  e: IIntEnumerable;
  v: Integer;
begin
  e := TIntArrayCursor.Create(TIntHelper.CreateCopy(IntStrictInc21));
  AssertTrue(e.FindLast(v));
  AssertTrue(v = 21);
end;

procedure TEnumerableTest.EmptyEnum_Last;
var
  e: IIntEnumerable;
begin
  e := TIntArrayCursor.Create(nil);
  AssertFalse(e.Last.Assigned);
end;

procedure TEnumerableTest.Enum21_Last;
var
  e: IIntEnumerable;
  o: TIntOptional;
begin
  e := TIntArrayCursor.Create(TIntHelper.CreateCopy(IntStrictInc21));
  o := e.Last;
  AssertTrue(o.Assigned);
  AssertTrue(o.Value = 21);
end;

procedure TEnumerableTest.EmptyEnum_FindMinRegular;
var
  e: IIntEnumerable;
  v: Integer;
begin
  e := TIntArrayCursor.Create(nil);
  AssertFalse(e.FindMin(v, @IntCmp));
end;

procedure TEnumerableTest.Enum21_FindMinRegular;
var
  e: IIntEnumerable;
  v: Integer;
begin
  e := TIntArrayCursor.Create(TIntHelper.CreateRandomShuffle(IntStrictInc21));
  AssertTrue(e.FindMin(v, @IntCmp));
  AssertTrue(v = 1);
end;

procedure TEnumerableTest.EmptyEnum_FindMinDelegated;
var
  e: IIntEnumerable;
  v: Integer;
begin
  e := TIntArrayCursor.Create(nil);
  AssertFalse(e.FindMin(v, @DoIntCmp));
end;

procedure TEnumerableTest.Enum21_FindMinDelegated;
var
  e: IIntEnumerable;
  v: Integer;
begin
  e := TIntArrayCursor.Create(TIntHelper.CreateRandomShuffle(IntStrictInc21));
  AssertTrue(e.FindMin(v, @DoIntCmp));
  AssertTrue(v = 1);
end;

procedure TEnumerableTest.EmptyEnum_FindMinNested;
  function IntComp(constref L, R: Integer): Boolean;
  begin
    Result := L < R;
  end;
var
  e: IIntEnumerable;
  v: Integer;
begin
  e := TIntArrayCursor.Create(nil);
  AssertFalse(e.FindMin(v, @IntComp));
end;

procedure TEnumerableTest.Enum21_FindMinNested;
  function IntComp(constref L, R: Integer): Boolean;
  begin
    Result := L < R;
  end;
var
  e: IIntEnumerable;
  v: Integer;
begin
  e := TIntArrayCursor.Create(TIntHelper.CreateRandomShuffle(IntStrictInc21));
  AssertTrue(e.FindMin(v, @IntComp));
  AssertTrue(v = 1);
end;

procedure TEnumerableTest.EmptyEnum_MinRegular;
var
  e: IIntEnumerable;
begin
  e := TIntArrayCursor.Create(nil);
  AssertFalse(e.Min(@IntCmp).Assigned);
end;

procedure TEnumerableTest.Enum21_MinRegular;
var
  e: IIntEnumerable;
  o: TIntOptional;
begin
  e := TIntArrayCursor.Create(TIntHelper.CreateRandomShuffle(IntStrictInc21));
  o := e.Min(@IntCmp);
  AssertTrue(o.Assigned);
  AssertTrue(o.Value = 1);
end;

procedure TEnumerableTest.EmptyEnum_MinDelegated;
var
  e: IIntEnumerable;
begin
  e := TIntArrayCursor.Create(nil);
  AssertFalse(e.Min(@DoIntCmp).Assigned);
end;

procedure TEnumerableTest.Enum21_MinDelegated;
var
  e: IIntEnumerable;
  o: TIntOptional;
begin
  e := TIntArrayCursor.Create(TIntHelper.CreateRandomShuffle(IntStrictInc21));
  o := e.Min(@DoIntCmp);
  AssertTrue(o.Assigned);
  AssertTrue(o.Value = 1);
end;

procedure TEnumerableTest.EmptyEnum_MinNested;
  function IntComp(constref L, R: Integer): SizeInt;
  begin
  {$IFDEF CPU64}
    Result := SizeInt(L) - SizeInt(R);
  {$ELSE CPU64}
    if L > R then
      Result := 1
    else
      if R > L then
        Result := -1
      else
        Result := 0;
  {$ENDIF CPU64}
  end;
var
  e: IIntEnumerable;
begin
  e := TIntArrayCursor.Create(nil);
  AssertFalse(e.Min(@DoIntCmp).Assigned);
end;

procedure TEnumerableTest.Enum21_MinNested;
  function IntComp(constref L, R: Integer): Boolean;
  begin
    Result := L < R;
  end;
var
  e: IIntEnumerable;
  o: TIntOptional;
begin
  e := TIntArrayCursor.Create(TIntHelper.CreateRandomShuffle(IntStrictInc21));
  o := e.Min(@IntComp);
  AssertTrue(o.Assigned);
  AssertTrue(o.Value = 1);
end;

procedure TEnumerableTest.EmptyEnum_FindMaxRegular;
var
  e: IIntEnumerable;
  v: Integer;
begin
  e := TIntArrayCursor.Create(nil);
  AssertFalse(e.FindMax(v, @IntCmp));
end;

procedure TEnumerableTest.Enum21_FindMaxRegular;
var
  e: IIntEnumerable;
  v: Integer;
begin
  e := TIntArrayCursor.Create(TIntHelper.CreateRandomShuffle(IntStrictInc21));
  AssertTrue(e.FindMax(v, @IntCmp));
  AssertTrue(v = 21);
end;

procedure TEnumerableTest.EmptyEnum_FindMaxDelegated;
var
  e: IIntEnumerable;
  v: Integer;
begin
  e := TIntArrayCursor.Create(nil);
  AssertFalse(e.FindMax(v, @DoIntCmp));
end;

procedure TEnumerableTest.Enum21_FindMaxDelegated;
var
  e: IIntEnumerable;
  v: Integer;
begin
  e := TIntArrayCursor.Create(TIntHelper.CreateRandomShuffle(IntStrictInc21));
  AssertTrue(e.FindMax(v, @DoIntCmp));
  AssertTrue(v = 21);
end;

procedure TEnumerableTest.EmptyEnum_FindMaxNested;
  function IntComp(constref L, R: Integer): Boolean;
  begin
    Result := L < R;
  end;
var
  e: IIntEnumerable;
  v: Integer;
begin
  e := TIntArrayCursor.Create(nil);
  AssertFalse(e.FindMax(v, @IntComp));
end;

procedure TEnumerableTest.Enum21_FindMaxNested;
  function IntComp(constref L, R: Integer): Boolean;
  begin
    Result := L < R;
  end;
var
  e: IIntEnumerable;
  v: Integer;
begin
  e := TIntArrayCursor.Create(TIntHelper.CreateRandomShuffle(IntStrictInc21));
  AssertTrue(e.FindMax(v, @IntComp));
  AssertTrue(v = 21);
end;

procedure TEnumerableTest.EmptyEnum_MaxRegular;
var
  e: IIntEnumerable;
begin
  e := TIntArrayCursor.Create(nil);
  AssertFalse(e.Max(@IntCmp).Assigned);
end;

procedure TEnumerableTest.Enum21_MaxRegular;
var
  e: IIntEnumerable;
  o: TIntOptional;
begin
  e := TIntArrayCursor.Create(TIntHelper.CreateRandomShuffle(IntStrictInc21));
  o := e.Max(@IntCmp);
  AssertTrue(o.Assigned);
  AssertTrue(o.Value = 21);
end;

procedure TEnumerableTest.EmptyEnum_MaxDelegated;
var
  e: IIntEnumerable;
begin
  e := TIntArrayCursor.Create(nil);
  AssertFalse(e.Max(@DoIntCmp).Assigned);
end;

procedure TEnumerableTest.Enum21_MaxDelegated;
var
  e: IIntEnumerable;
  o: TIntOptional;
begin
  e := TIntArrayCursor.Create(TIntHelper.CreateRandomShuffle(IntStrictInc21));
  o := e.Max(@DoIntCmp);
  AssertTrue(o.Assigned);
  AssertTrue(o.Value = 21);
end;

procedure TEnumerableTest.EmptyEnum_MaxNested;
  function IntComp(constref L, R: Integer): SizeInt;
  begin
  {$IFDEF CPU64}
    Result := SizeInt(L) - SizeInt(R);
  {$ELSE CPU64}
    if L > R then
      Result := 1
    else
      if R > L then
        Result := -1
      else
        Result := 0;
  {$ENDIF CPU64}
  end;
var
  e: IIntEnumerable;
begin
  e := TIntArrayCursor.Create(nil);
  AssertFalse(e.Max(@DoIntCmp).Assigned);
end;

procedure TEnumerableTest.Enum21_MaxNested;
  function IntComp(constref L, R: Integer): Boolean;
  begin
    Result := L < R;
  end;
var
  e: IIntEnumerable;
  o: TIntOptional;
begin
  e := TIntArrayCursor.Create(TIntHelper.CreateRandomShuffle(IntStrictInc21));
  o := e.Max(@IntComp);
  AssertTrue(o.Assigned);
  AssertTrue(o.Value = 21);
end;

procedure TEnumerableTest.Reverse;
var
  e: IIntEnumerable;
  I, J : Integer;
begin
  e := TIntArrayCursor.Create(TIntHelper.CreateCopy(IntStrictInc21));
  J := 21;
  for I in e.Reverse do
    begin
      AssertTrue(I = J);
      Dec(J);
    end;
end;

procedure TEnumerableTest.EmptyEnum_Skip0;
var
  e: IIntEnumerable;
begin
  e := TIntArrayCursor.Create(nil);
  AssertTrue(e.Skip(0).None);
end;

procedure TEnumerableTest.Enum21_Skip0;
var
  e: IIntEnumerable;
begin
  e := TIntArrayCursor.Create(TIntHelper.CreateCopy(IntStrictInc21));
  AssertTrue(e.Skip(0).Total = 21);
  e := TIntArrayCursor.Create(TIntHelper.CreateCopy(IntStrictInc21));
  AssertTrue(TIntHelper.Same(e.Skip(0).ToArray,  IntStrictInc21));
end;

procedure TEnumerableTest.Enum21_Skip3;
var
  e: IIntEnumerable;
begin
  e := TIntArrayCursor.Create(TIntHelper.CreateCopy(IntStrictInc21));
  AssertTrue(e.Skip(3).Total = 18);
  e := TIntArrayCursor.Create(TIntHelper.CreateCopy(IntStrictInc21));
  AssertTrue(TIntHelper.Same(e.Skip(3).ToArray,  IntStrictInc21[4..21]));
end;

procedure TEnumerableTest.Enum21_SkipNeg;
var
  e: IIntEnumerable;
begin
  e := TIntArrayCursor.Create(TIntHelper.CreateCopy(IntStrictInc21));
  AssertTrue(e.Skip(-15).Total = 21);
  e := TIntArrayCursor.Create(TIntHelper.CreateCopy(IntStrictInc21));
  AssertTrue(TIntHelper.Same(e.Skip(-3).ToArray,  IntStrictInc21));
end;

procedure TEnumerableTest.Enum21_SkipAll;
var
  e: IIntEnumerable;
begin
  e := TIntArrayCursor.Create(TIntHelper.CreateCopy(IntStrictInc21));
  AssertTrue(e.Skip(21).None);
end;

procedure TEnumerableTest.EmptyEnum_Limit1;
var
  e: IIntEnumerable;
begin
  e := TIntArrayCursor.Create(nil);
  AssertTrue(e.Limit(1).None);
end;

procedure TEnumerableTest.Enum21_Limit0;
var
  e: IIntEnumerable;
begin
  e := TIntArrayCursor.Create(TIntHelper.CreateCopy(IntStrictInc21));
  AssertTrue(e.Limit(0).None);
end;

procedure TEnumerableTest.Enum21_Limit5;
var
  e: IIntEnumerable;
begin
  e := TIntArrayCursor.Create(TIntHelper.CreateCopy(IntStrictInc21));
  AssertTrue(e.Limit(5).Total = 5);
  e := TIntArrayCursor.Create(TIntHelper.CreateCopy(IntStrictInc21));
  AssertTrue(TIntHelper.Same(e.Limit(5).ToArray, IntStrictInc21[1..5]));
end;

procedure TEnumerableTest.Enum21_Limit23;
var
  e: IIntEnumerable;
begin
  e := TIntArrayCursor.Create(TIntHelper.CreateCopy(IntStrictInc21));
  AssertTrue(e.Limit(23).Total = 21);
  e := TIntArrayCursor.Create(TIntHelper.CreateCopy(IntStrictInc21));
  AssertTrue(TIntHelper.Same(e.Limit(23).ToArray, IntStrictInc21));
end;

procedure TEnumerableTest.EmptyEnum_SortedRegular;
var
  e: IIntEnumerable;
begin
  //shouldn't crash
  e := TIntArrayCursor.Create(nil);
  AssertTrue(e.Sorted(@IntCmp).None);
end;

procedure TEnumerableTest.Enum21_SortedRegular;
var
  e: IIntEnumerable;
begin
  e := TIntArrayCursor.Create(TIntHelper.CreateRandomShuffle(IntStrictInc21));
  AssertTrue(TIntHelper.Same(e.Sorted(@IntCmp).ToArray, IntStrictInc21));
end;

procedure TEnumerableTest.Enum21_SortedRegularDesc;
var
  e: IIntEnumerable;
begin
  e := TIntArrayCursor.Create(TIntHelper.CreateRandomShuffle(IntStrictInc21));
  AssertTrue(TIntHelper.Same(e.Sorted(@IntCmp).Reverse.ToArray, IntStrictDec21));
end;

procedure TEnumerableTest.EmptyEnum_SortedDelegated;
var
  e: IIntEnumerable;
begin
  //shouldn't crash
  e := TIntArrayCursor.Create(nil);
  AssertTrue(e.Sorted(@DoIntCmp).None);
end;

procedure TEnumerableTest.Enum21_SortedDelegated;
var
  e: IIntEnumerable;
begin
  e := TIntArrayCursor.Create(TIntHelper.CreateRandomShuffle(IntStrictInc21));
  AssertTrue(TIntHelper.Same(e.Sorted(@DoIntCmp).ToArray, IntStrictInc21));
end;

procedure TEnumerableTest.Enum21_SortedDelegatedDesc;
var
  e: IIntEnumerable;
begin
  e := TIntArrayCursor.Create(TIntHelper.CreateRandomShuffle(IntStrictInc21));
  AssertTrue(TIntHelper.Same(e.Sorted(@DoIntCmp).Reverse.ToArray, IntStrictDec21));
end;

procedure TEnumerableTest.EmptyEnum_SortedNested;
  function IntComp(constref L, R: Integer): Boolean;
  begin
    Result := L < R;
  end;
var
  e: IIntEnumerable;
begin
  e := TIntArrayCursor.Create(nil);
  AssertTrue(e.Sorted(@IntComp).None);
end;

procedure TEnumerableTest.Enum21_SortedNested;
  function IntComp(constref L, R: Integer): Boolean;
  begin
    Result := L < R;
  end;
var
  e: IIntEnumerable;
begin
  e := TIntArrayCursor.Create(TIntHelper.CreateRandomShuffle(IntStrictInc21));
  AssertTrue(TIntHelper.Same(e.Sorted(@IntComp).ToArray, IntStrictInc21));
end;

procedure TEnumerableTest.Enum21_SortedNestedDesc;
  function IntComp(constref L, R: Integer): Boolean;
  begin
    Result := L < R;
  end;
var
  e: IIntEnumerable;
begin
  e := TIntArrayCursor.Create(TIntHelper.CreateRandomShuffle(IntStrictInc21));
  AssertTrue(TIntHelper.Same(e.Sorted(@IntComp).Reverse.ToArray, IntStrictDec21));
end;

procedure TEnumerableTest.EmptyEnum_SelectRegular;
var
  e: IIntEnumerable;
begin
  e := TIntArrayCursor.Create(nil);
  AssertTrue(e.Select(@IsEven).None);
end;

procedure TEnumerableTest.Enum21_SelectRegular;
var
  e: IIntEnumerable;
begin
  e := TIntArrayCursor.Create(TIntHelper.CreateCopy(IntStrictInc21));
  AssertTrue(e.Select(@IsEven).Total = 10);
  e := TIntArrayCursor.Create(TIntHelper.CreateCopy(IntStrictInc21));
  AssertTrue(TIntHelper.Same(e.Select(@IsEven).ToArray, IntEvenInc10));
end;

procedure TEnumerableTest.Enum11_SelectRegular;
var
  e: IIntEnumerable;
begin
  e := TIntArrayCursor.Create(TIntHelper.CreateCopy(IntOddInc11));
  AssertTrue(e.Select(@IsEven).None);
end;

procedure TEnumerableTest.EmptyEnum_SelectDelegated;
var
  e: IIntEnumerable;
begin
  e := TIntArrayCursor.Create(nil);
  AssertTrue(e.Select(@GetIsEven).None);
end;

procedure TEnumerableTest.Enum21_SelectDelegated;
var
  e: IIntEnumerable;
begin
  e := TIntArrayCursor.Create(TIntHelper.CreateCopy(IntStrictInc21));
  AssertTrue(e.Select(@IsEven).Total = 10);
  e := TIntArrayCursor.Create(TIntHelper.CreateCopy(IntStrictInc21));
  AssertTrue(TIntHelper.Same(e.Select(@GetIsEven).ToArray, IntEvenInc10));
end;

procedure TEnumerableTest.Enum11_SelectDelegated;
var
  e: IIntEnumerable;
begin
  e := TIntArrayCursor.Create(TIntHelper.CreateCopy(IntOddInc11));
  AssertTrue(e.Select(@GetIsEven).None);
end;

procedure TEnumerableTest.EmptyEnum_SelectNested;
  function IsEven(constref aValue: Integer): Boolean;
  begin
    Result := not Odd(aValue);
  end;
var
  e: IIntEnumerable;
begin
  e := TIntArrayCursor.Create(nil);
  AssertTrue(e.Select(@IsEven).None);
end;

procedure TEnumerableTest.Enum21_SelectNested;
  function IsEven(constref aValue: Integer): Boolean;
  begin
    Result := not Odd(aValue);
  end;
var
  e: IIntEnumerable;
begin
  e := TIntArrayCursor.Create(TIntHelper.CreateCopy(IntStrictInc21));
  AssertTrue(e.Select(@IsEven).Total = 10);
  e := TIntArrayCursor.Create(TIntHelper.CreateCopy(IntStrictInc21));
  AssertTrue(TIntHelper.Same(e.Select(@IsEven).ToArray, IntEvenInc10));
end;

procedure TEnumerableTest.Enum11_SelectNested;
  function IsEven(constref aValue: Integer): Boolean;
  begin
    Result := not Odd(aValue);
  end;
var
  e: IIntEnumerable;
begin
  e := TIntArrayCursor.Create(TIntHelper.CreateCopy(IntOddInc11));
  AssertTrue(e.Select(@IsEven).None);
end;

procedure TEnumerableTest.EmptyEnum_AnyRegular;
var
  e: IIntEnumerable;
begin
  e := TIntArrayCursor.Create(nil);
  AssertFalse(e.Any(@IsEven));
end;

procedure TEnumerableTest.Enum21_AnyRegular;
var
  e: IIntEnumerable;
begin
  e := TIntArrayCursor.Create(TIntHelper.CreateCopy(IntStrictInc21));
  AssertTrue(e.Any(@IsEven));
end;

procedure TEnumerableTest.Enum11_AnyRegular;
var
  e: IIntEnumerable;
begin
  e := TIntArrayCursor.Create(TIntHelper.CreateCopy(IntOddInc11));
  AssertFalse(e.Any(@IsEven));
end;

procedure TEnumerableTest.EmptyEnum_AnyDelegated;
var
  e: IIntEnumerable;
begin
  e := TIntArrayCursor.Create(nil);
  AssertFalse(e.Any(@GetIsEven));
end;

procedure TEnumerableTest.Enum21_AnyDelegated;
var
  e: IIntEnumerable;
begin
  e := TIntArrayCursor.Create(TIntHelper.CreateCopy(IntStrictInc21));
  AssertTrue(e.Any(@GetIsEven));
end;

procedure TEnumerableTest.Enum11_AnyDelegated;
var
  e: IIntEnumerable;
begin
  e := TIntArrayCursor.Create(TIntHelper.CreateCopy(IntOddInc11));
  AssertFalse(e.Any(@GetIsEven));
end;

procedure TEnumerableTest.EmptyEnum_AnyNested;
  function IsEven(constref aValue: Integer): Boolean;
  begin
    Result := not Odd(aValue);
  end;
var
  e: IIntEnumerable;
begin
  e := TIntArrayCursor.Create(nil);
  AssertFalse(e.Any(@IsEven));
end;

procedure TEnumerableTest.Enum21_AnyNested;
  function IsEven(constref aValue: Integer): Boolean;
  begin
    Result := not Odd(aValue);
  end;
var
  e: IIntEnumerable;
begin
  e := TIntArrayCursor.Create(TIntHelper.CreateCopy(IntStrictInc21));
  AssertTrue(e.Any(@IsEven));
end;

procedure TEnumerableTest.Enum11_AnyNested;
  function IsEven(constref aValue: Integer): Boolean;
  begin
    Result := not Odd(aValue);
  end;
var
  e: IIntEnumerable;
begin
  e := TIntArrayCursor.Create(TIntHelper.CreateCopy(IntOddInc11));
  AssertFalse(e.Any(@IsEven));
end;

procedure TEnumerableTest.EmptyEnum_NoneRegular;
var
  e: IIntEnumerable;
begin
  e := TIntArrayCursor.Create(nil);
  AssertTrue(e.None(@IsEven));
end;

procedure TEnumerableTest.Enum21_NoneRegular;
var
  e: IIntEnumerable;
begin
  e := TIntArrayCursor.Create(TIntHelper.CreateCopy(IntStrictInc21));
  AssertFalse(e.None(@IsEven));
end;

procedure TEnumerableTest.Enum11_NoneRegular; // IntStrictInc21
var
  e: IIntEnumerable;
begin
  e := TIntArrayCursor.Create(TIntHelper.CreateCopy(IntOddInc11));
  AssertTrue(e.None(@IsEven));
end;

procedure TEnumerableTest.EmptyEnum_NoneDelegated;
var
  e: IIntEnumerable;
begin
  e := TIntArrayCursor.Create(nil);
  AssertTrue(e.None(@GetIsEven));
end;

procedure TEnumerableTest.Enum21_NoneDelegated;
var
  e: IIntEnumerable;
begin
  e := TIntArrayCursor.Create(TIntHelper.CreateCopy(IntStrictInc21));
  AssertFalse(e.None(@GetIsEven));
end;

procedure TEnumerableTest.Enum11_NoneDelegated;
var
  e: IIntEnumerable;
begin
  e := TIntArrayCursor.Create(TIntHelper.CreateCopy(IntOddInc11));
  AssertTrue(e.None(@GetIsEven));
end;

procedure TEnumerableTest.EmptyEnum_NoneNested;
  function IsEven(constref aValue: Integer): Boolean;
  begin
    Result := not Odd(aValue);
  end;
var
  e: IIntEnumerable;
begin
  e := TIntArrayCursor.Create(nil);
  AssertTrue(e.None(@IsEven));
end;

procedure TEnumerableTest.Enum21_NoneNested;
  function IsEven(constref aValue: Integer): Boolean;
  begin
    Result := not Odd(aValue);
  end;
var
  e: IIntEnumerable;
begin
  e := TIntArrayCursor.Create(TIntHelper.CreateCopy(IntStrictInc21));
  AssertFalse(e.None(@IsEven));
end;

procedure TEnumerableTest.Enum11_NoneNested;
  function IsEven(constref aValue: Integer): Boolean;
  begin
    Result := not Odd(aValue);
  end;
var
  e: IIntEnumerable;
begin
  e := TIntArrayCursor.Create(TIntHelper.CreateCopy(IntOddInc11));
  AssertTrue(e.None(@IsEven));
end;

procedure TEnumerableTest.EmptyEnum_AllRegular;
var
  e: IIntEnumerable;
begin
  e := TIntArrayCursor.Create(nil);
  AssertTrue(e.All(@IsEven));
end;

procedure TEnumerableTest.Enum21_AllRegular;
var
  e: IIntEnumerable;
begin
  e := TIntArrayCursor.Create(TIntHelper.CreateCopy(IntStrictInc21));
  AssertFalse(e.All(@IsEven));
end;

procedure TEnumerableTest.Enum10_AllRegular;
var
  e: IIntEnumerable;
begin
  e := TIntArrayCursor.Create(TIntHelper.CreateCopy(IntEvenInc10));
  AssertTrue(e.All(@IsEven));
end;

procedure TEnumerableTest.EmptyEnum_AllDelegated;
var
  e: IIntEnumerable;
begin
  e := TIntArrayCursor.Create(nil);
  AssertTrue(e.All(@GetIsEven));
end;

procedure TEnumerableTest.Enum21_AllDelegated;
var
  e: IIntEnumerable;
begin
  e := TIntArrayCursor.Create(TIntHelper.CreateCopy(IntStrictInc21));
  AssertFalse(e.All(@GetIsEven));
end;

procedure TEnumerableTest.Enum10_AllDelegated;
var
  e: IIntEnumerable;
begin
  e := TIntArrayCursor.Create(TIntHelper.CreateCopy(IntEvenInc10));
  AssertTrue(e.All(@GetIsEven));
end;

procedure TEnumerableTest.EmptyEnum_AllNested;
  function IsEven(constref aValue: Integer): Boolean;
  begin
    Result := not Odd(aValue);
  end;
var
  e: IIntEnumerable;
begin
  e := TIntArrayCursor.Create(nil);
  AssertTrue(e.All(@IsEven));
end;

procedure TEnumerableTest.Enum21_AllNested;
  function IsEven(constref aValue: Integer): Boolean;
  begin
    Result := not Odd(aValue);
  end;
var
  e: IIntEnumerable;
begin
  e := TIntArrayCursor.Create(TIntHelper.CreateCopy(IntStrictInc21));
  AssertFalse(e.All(@IsEven));
end;

procedure TEnumerableTest.Enum10_AllNested;
  function IsEven(constref aValue: Integer): Boolean;
  begin
    Result := not Odd(aValue);
  end;
var
  e: IIntEnumerable;
begin
  e := TIntArrayCursor.Create(TIntHelper.CreateCopy(IntEvenInc10));
  AssertTrue(e.All(@IsEven));
end;

procedure TEnumerableTest.EmptyEnum_TotalRegular;
var
  e: IIntEnumerable;
begin
  e := TIntArrayCursor.Create(nil);
  AssertTrue(e.Total(@IsEven) = 0);
end;

procedure TEnumerableTest.Enum21_TotalRegular;
var
  e: IIntEnumerable;
begin
  e := TIntArrayCursor.Create(TIntHelper.CreateCopy(IntStrictInc21));
  AssertTrue(e.Total(@IsEven) = 10);
end;

procedure TEnumerableTest.Enum11_TotalRegular;
var
  e: IIntEnumerable;
begin
  e := TIntArrayCursor.Create(TIntHelper.CreateCopy(IntOddInc11));
  AssertTrue(e.Total(@IsEven) = 0);
end;

procedure TEnumerableTest.EmptyEnum_TotalDelegated;
var
  e: IIntEnumerable;
begin
  e := TIntArrayCursor.Create(nil);
  AssertTrue(e.Total(@GetIsEven) = 0);
end;

procedure TEnumerableTest.Enum21_TotalDelegated;
var
  e: IIntEnumerable;
begin
  e := TIntArrayCursor.Create(TIntHelper.CreateCopy(IntStrictInc21));
  AssertTrue(e.Total(@GetIsEven) = 10);
end;

procedure TEnumerableTest.Enum11_TotalDelegated;
var
  e: IIntEnumerable;
begin
  e := TIntArrayCursor.Create(TIntHelper.CreateCopy(IntOddInc11));
  AssertTrue(e.Total(@GetIsEven) = 0);
end;

procedure TEnumerableTest.EmptyEnum_TotalNested;
  function IsEven(constref aValue: Integer): Boolean;
  begin
    Result := not Odd(aValue);
  end;
var
  e: IIntEnumerable;
begin
  e := TIntArrayCursor.Create(nil);
  AssertTrue(e.Total(@IsEven) = 0);
end;

procedure TEnumerableTest.Enum21_TotalNested;
  function IsEven(constref aValue: Integer): Boolean;
  begin
    Result := not Odd(aValue);
  end;
var
  e: IIntEnumerable;
begin
  e := TIntArrayCursor.Create(TIntHelper.CreateCopy(IntStrictInc21));
  AssertTrue(e.Total(@IsEven) = 10);
end;

procedure TEnumerableTest.Enum11_TotalNested;
  function IsEven(constref aValue: Integer): Boolean;
  begin
    Result := not Odd(aValue);
  end;
var
  e: IIntEnumerable;
begin
  e := TIntArrayCursor.Create(TIntHelper.CreateCopy(IntOddInc11));
  AssertTrue(e.Total(@IsEven) = 0);
end;

procedure TEnumerableTest.EmptyEnum_MapRegular;
var
  e: IIntEnumerable;
begin
  e := TIntArrayCursor.Create(nil);
  AssertTrue(e.Map(@MulBy4).None);
end;

procedure TEnumerableTest.Enum21_MapRegular;
var
  e: IIntEnumerable;
begin
  e := TIntArrayCursor.Create(TIntHelper.CreateCopy(IntStrictInc21));
  AssertTrue(TIntHelper.Same(e.Map(@MulBy4).ToArray, IntMul4Inc21));
end;

procedure TEnumerableTest.Enum10_MapRegular;
var
  e: IIntEnumerable;
begin
  e := TIntArrayCursor.Create(TIntHelper.CreateCopy(IntStrictInc21[3..13]));
  AssertTrue(TIntHelper.Same(e.Map(@MulBy4).ToArray, IntMul4Inc21[3..13]));
end;

procedure TEnumerableTest.EmptyEnum_MapDelegated;
var
  e: IIntEnumerable;
begin
  e := TIntArrayCursor.Create(nil);
  AssertTrue(e.Map(@DoMulBy4).None);
end;

procedure TEnumerableTest.Enum21_MapDelegated;
var
  e: IIntEnumerable;
begin
  e := TIntArrayCursor.Create(TIntHelper.CreateCopy(IntStrictInc21));
  AssertTrue(TIntHelper.Same(e.Map(@DoMulBy4).ToArray, IntMul4Inc21));
end;

procedure TEnumerableTest.Enum10_MapDelegated;
var
  e: IIntEnumerable;
begin
  e := TIntArrayCursor.Create(TIntHelper.CreateCopy(IntStrictInc21[3..13]));
  AssertTrue(TIntHelper.Same(e.Map(@DoMulBy4).ToArray, IntMul4Inc21[3..13]));
end;

procedure TEnumerableTest.EmptyEnum_MapNested;
  function MulBy4(constref aValue: Integer): Integer;
  begin
    Result := aValue * 4;
  end;
var
  e: IIntEnumerable;
begin
  e := TIntArrayCursor.Create(nil);
  AssertTrue(e.Map(@MulBy4).None);
end;

procedure TEnumerableTest.Enum21_MapNested;
  function MulBy4(constref aValue: Integer): Integer;
  begin
    Result := aValue * 4;
  end;
var
  e: IIntEnumerable;
begin
  e := TIntArrayCursor.Create(TIntHelper.CreateCopy(IntStrictInc21));
  AssertTrue(TIntHelper.Same(e.Map(@MulBy4).ToArray, IntMul4Inc21));
end;

procedure TEnumerableTest.Enum10_MapNested;
  function MulBy4(constref aValue: Integer): Integer;
  begin
    Result := aValue * 4;
  end;
var
  e: IIntEnumerable;
begin
  e := TIntArrayCursor.Create(TIntHelper.CreateCopy(IntStrictInc21[3..13]));
  AssertTrue(TIntHelper.Same(e.Map(@MulBy4).ToArray, IntMul4Inc21[3..13]));
end;

procedure TEnumerableTest.EmptyEnum_FoldRegularWithV0;
var
  e: IIntEnumerable;
begin
  e := TIntArrayCursor.Create(nil);
  AssertTrue(e.Fold(@AddSquare, -1) = -1);
end;

procedure TEnumerableTest.Enum21_FoldRegularWithV0;
var
  e: IIntEnumerable;
begin
  e := TIntArrayCursor.Create(TIntHelper.CreateCopy(IntStrictInc21));
  AssertTrue(e.Fold(@AddSquare, 0) = 3311); //sum of squares 1..21
end;

procedure TEnumerableTest.EmptyEnum_FoldRegular;
var
  e: IIntEnumerable;
begin
  e := TIntArrayCursor.Create(nil);
  AssertFalse(e.Fold(@AddSquare).Assigned);
end;

procedure TEnumerableTest.Enum21_FoldRegular;
var
  e: IIntEnumerable;
  o: TIntOptional;
begin
  e := TIntArrayCursor.Create(TIntHelper.CreateCopy(IntStrictInc21));
  o := e.Fold(@AddSquare);
  AssertTrue(o.Assigned);
  AssertTrue(o.Value = 3311);
end;

procedure TEnumerableTest.EmptyEnum_FoldDelegatedWithV0;
var
  e: IIntEnumerable;
begin
  e := TIntArrayCursor.Create(nil);
  AssertTrue(e.Fold(@DoAddSquare, -10) = -10);
end;

procedure TEnumerableTest.Enum21_FoldDelegatedWithV0;
var
  e: IIntEnumerable;
begin
  e := TIntArrayCursor.Create(TIntHelper.CreateCopy(IntStrictInc21));
  AssertTrue(e.Fold(@DoAddSquare, 0) = 3311); //sum of squares 1..21
end;

procedure TEnumerableTest.EmptyEnum_FoldDelegated;
var
  e: IIntEnumerable;
begin
  e := TIntArrayCursor.Create(nil);
  AssertFalse(e.Fold(@DoAddSquare).Assigned);
end;

procedure TEnumerableTest.Enum21_FoldDelegated;
var
  e: IIntEnumerable;
  o: TIntOptional;
begin
  e := TIntArrayCursor.Create(TIntHelper.CreateCopy(IntStrictInc21));
  o := e.Fold(@DoAddSquare);
  AssertTrue(o.Assigned);
  AssertTrue(o.Value = 3311);
end;

procedure TEnumerableTest.EmptyEnum_FoldNestedWithV0;
  function AddSquare(constref X, Y: Integer): Integer;
  begin
    Result := X * X + Y;
  end;
var
  e: IIntEnumerable;
begin
  e := TIntArrayCursor.Create(nil);
  AssertTrue(e.Fold(@AddSquare, -3) = -3);
end;

procedure TEnumerableTest.Enum21_FoldNestedWithV0;
  function AddSquare(constref X, Y: Integer): Integer;
  begin
    Result := X * X + Y;
  end;
var
  e: IIntEnumerable;
begin
  e := TIntArrayCursor.Create(TIntHelper.CreateCopy(IntStrictInc21));
  AssertTrue(e.Fold(@AddSquare, 0) = 3311); //sum of squares 1..21
end;

procedure TEnumerableTest.EmptyEnum_FoldNested;
  function AddSquare(constref X, Y: Integer): Integer;
  begin
    Result := X * X + Y;
  end;
var
  e: IIntEnumerable;
begin
  e := TIntArrayCursor.Create(nil);
  AssertFalse(e.Fold(@AddSquare).Assigned);
end;

procedure TEnumerableTest.Enum21_FoldNested;
  function AddSquare(constref X, Y: Integer): Integer;
  begin
    Result := X * X + Y;
  end;
var
  e: IIntEnumerable;
  o: TIntOptional;
begin
  e := TIntArrayCursor.Create(TIntHelper.CreateCopy(IntStrictInc21));
  o := e.Fold(@AddSquare);
  AssertTrue(o.Assigned);
  AssertTrue(o.Value = 3311);
end;

procedure TEnumerableTest.SortedRegular;
type
  THelper     = specialize TGRegularArrayHelper<TPair>;
  TNestHelper = specialize TGNestedArrayHelper<TPair>;
  function ValCmp(constref L, R: TPair): Boolean;
  begin
    Result := L.Value < R.Value;
  end;
var
  e: IPairEnumerable;
  a: array of TPair;
const
  KeyCount = 55;
  TestSize = 1000;
begin
  e := TPairArrayCursor.Create(CreatePairArray(KeyCount, TestSize));
  a := e.Sorted(@PairCompare).ToArray;
  AssertTrue(THelper.IsNonDescending(a, @PairCompare));
  AssertFalse(TNestHelper.IsStrictAscending(a, @ValCmp));
  a := IPairEnumerable(TPairArrayCursor.Create(CreatePairArray(KeyCount, TestSize)))
      .Sorted(@PairCompare, True)
      .ToArray;
  AssertTrue(THelper.IsNonDescending(a, @PairCompare));
  AssertTrue(TNestHelper.IsStrictAscending(a, @ValCmp));
end;

procedure TEnumerableTest.SortedDelegated;
type
  THelper     = specialize TGDelegatedArrayHelper<TPair>;
  TNestHelper = specialize TGNestedArrayHelper<TPair>;
  function ValCmp(constref L, R: TPair): Boolean;
  begin
    Result := L.Value < R.Value;
  end;
var
  e: IPairEnumerable;
  a: array of TPair;
const
  KeyCount = 55;
  TestSize = 1000;
begin
  e := TPairArrayCursor.Create(CreatePairArray(KeyCount, TestSize));
  a := e.Sorted(@PairCompare).ToArray;
  AssertTrue(THelper.IsNonDescending(a, @PairCmp));
  AssertFalse(TNestHelper.IsStrictAscending(a, @ValCmp));
  a := IPairEnumerable(TPairArrayCursor.Create(CreatePairArray(KeyCount, TestSize)))
      .Sorted(@PairCompare, True)
      .ToArray;
  AssertTrue(THelper.IsNonDescending(a, @PairCmp));
  AssertTrue(TNestHelper.IsStrictAscending(a, @ValCmp));
end;

procedure TEnumerableTest.SortedNested;
type
  THelper = specialize TGNestedArrayHelper<TPair>;
  function KeyCmp(constref L, R: TPair): Boolean;
  begin
    Result := L.Key < R.Key;
  end;
  function ValCmp(constref L, R: TPair): Boolean;
  begin
    Result := L.Value < R.Value;
  end;
var
  e: IPairEnumerable;
  a: array of TPair;
const
  KeyCount = 55;
  TestSize = 1000;
begin
  e := TPairArrayCursor.Create(CreatePairArray(KeyCount, TestSize));
  a := e.Sorted(@PairCompare).ToArray;
  AssertTrue(THelper.IsNonDescending(a, @KeyCmp));
  AssertFalse(THelper.IsStrictAscending(a, @ValCmp));
  a := IPairEnumerable(TPairArrayCursor.Create(CreatePairArray(KeyCount, TestSize)))
      .Sorted(@PairCompare, True)
      .ToArray;
  AssertTrue(THelper.IsNonDescending(a, @KeyCmp));
  AssertTrue(THelper.IsStrictAscending(a, @ValCmp));
end;

procedure TEnumerableTest.ForEachRegular;
var
  e: IIntEnumerable;
begin
  Counter := 0;
  e := TIntArrayCursor.Create([]);
  e.ForEach(@IncCounter);
  AssertTrue(Counter = 0);
  e := TIntArrayCursor.Create(TIntHelper.CreateCopy(IntStrictDec21));
  e.ForEach(@IncCounter);
  AssertTrue(Counter = 21);
end;

procedure TEnumerableTest.ForEachDelegated;
var
  e: IIntEnumerable;
begin
  FCounter := 0;
  e := TIntArrayCursor.Create([]);
  e.ForEach(@DoIncCounter);
  AssertTrue(FCounter = 0);
  e := TIntArrayCursor.Create(TIntHelper.CreateCopy(IntStrictDec21));
  e.ForEach(@DoIncCounter);
  AssertTrue(FCounter = 21);
end;

procedure TEnumerableTest.ForEachNested;
var
  Counter: Integer = 0;
  procedure IncCounter(constref v: Integer);begin assert(v=v); Inc(Counter);end;
var
  e: IIntEnumerable;
begin
  e := TIntArrayCursor.Create([]);
  e.ForEach(@IncCounter);
  AssertTrue(Counter = 0);
  e := TIntArrayCursor.Create(TIntHelper.CreateCopy(IntStrictDec21));
  e.ForEach(@IncCounter);
  AssertTrue(Counter = 21);
end;

procedure TEnumerableTest.TakeWhileRegular;
var
  e: IIntEnumerable;
  a: array of Integer;
begin
  e := TIntArrayCursor.Create([]);
  AssertTrue(e.TakeWhile(@LessThen15).None);
  e := TIntArrayCursor.Create(TIntHelper.CreateCopy(IntStrictInc21));
  a := e.TakeWhile(@LessThen15).ToArray;
  AssertTrue(TIntHelper.Same(a, [1,2,3,4,5,6,7,8,9,10,11,12,13,14]));
  e := TIntArrayCursor.Create([1, 4, 10, 15, 11, 9, 3, 5]);
  a := e.TakeWhile(@LessThen15).ToArray;
  AssertTrue(TIntHelper.Same(a, [1, 4, 10]));
  e := TIntArrayCursor.Create([15, 1, 4, 10, 15, 11, 9, 3, 5]);
  AssertTrue(e.TakeWhile(@LessThen15).None);
end;

procedure TEnumerableTest.TakeWhileDelegated;
var
  e: IIntEnumerable;
  a: array of Integer;
begin
  e := TIntArrayCursor.Create([]);
  AssertTrue(e.TakeWhile(@Self.LessThen15).None);
  e := TIntArrayCursor.Create(TIntHelper.CreateCopy(IntStrictInc21));
  a := e.TakeWhile(@Self.LessThen15).ToArray;
  AssertTrue(TIntHelper.Same(a, [1,2,3,4,5,6,7,8,9,10,11,12,13,14]));
  e := TIntArrayCursor.Create([1, 4, 10, 15, 11, 9, 3, 5]);
  a := e.TakeWhile(@Self.LessThen15).ToArray;
  AssertTrue(TIntHelper.Same(a, [1, 4, 10]));
  e := TIntArrayCursor.Create([15, 1, 4, 10, 15, 11, 9, 3, 5]);
  AssertTrue(e.TakeWhile(@Self.LessThen15).None);
end;

procedure TEnumerableTest.TakeWhileNested;
var
  e: IIntEnumerable;
  a: array of Integer;
  function IsLessThen15(constref v: Integer): Boolean; begin Result := v < 15 end;
begin
  e := TIntArrayCursor.Create([]);
  AssertTrue(e.TakeWhile(@IsLessThen15).None);
  e := TIntArrayCursor.Create(TIntHelper.CreateCopy(IntStrictInc21));
  a := e.TakeWhile(@IsLessThen15).ToArray;
  AssertTrue(TIntHelper.Same(a, [1,2,3,4,5,6,7,8,9,10,11,12,13,14]));
  e := TIntArrayCursor.Create([1, 4, 10, 15, 11, 9, 3, 5]);
  a := e.TakeWhile(@IsLessThen15).ToArray;
  AssertTrue(TIntHelper.Same(a, [1, 4, 10]));
  e := TIntArrayCursor.Create([15, 1, 4, 10, 15, 11, 9, 3, 5]);
  AssertTrue(e.TakeWhile(@IsLessThen15).None);
end;

procedure TEnumerableTest.SkipWhileRegular;
var
  e: IIntEnumerable;
  a: array of Integer;
begin
  e := TIntArrayCursor.Create([]);
  AssertTrue(e.SkipWhile(@LessThen15).None);
  e := TIntArrayCursor.Create(TIntHelper.CreateCopy(IntStrictInc21));
  a := e.SkipWhile(@LessThen15).ToArray;
  AssertTrue(TIntHelper.Same(a, [15, 16, 17, 18, 19, 20, 21]));
  e := TIntArrayCursor.Create([1, 4, 10, 15, 11, 9, 3, 5]);
  a := e.SkipWhile(@LessThen15).ToArray;
  AssertTrue(TIntHelper.Same(a, [15, 11, 9, 3, 5]));
  e := TIntArrayCursor.Create([14, 1, 4, 10, 13, 11, 9, 3, 5]);
  AssertTrue(e.SkipWhile(@LessThen15).None);
end;

procedure TEnumerableTest.SkipWhileDelegated;
var
  e: IIntEnumerable;
  a: array of Integer;
begin
  e := TIntArrayCursor.Create([]);
  AssertTrue(e.SkipWhile(@Self.LessThen15).None);
  e := TIntArrayCursor.Create(TIntHelper.CreateCopy(IntStrictInc21));
  a := e.SkipWhile(@Self.LessThen15).ToArray;
  AssertTrue(TIntHelper.Same(a, [15, 16, 17, 18, 19, 20, 21]));
  e := TIntArrayCursor.Create([1, 4, 10, 15, 11, 9, 3, 5]);
  a := e.SkipWhile(@Self.LessThen15).ToArray;
  AssertTrue(TIntHelper.Same(a, [15, 11, 9, 3, 5]));
  e := TIntArrayCursor.Create([14, 1, 4, 10, 13, 11, 9, 3, 5]);
  AssertTrue(e.SkipWhile(@Self.LessThen15).None);
end;

procedure TEnumerableTest.SkipWhileNested;
var
  e: IIntEnumerable;
  a: array of Integer;
  function IsLessThen15(constref v: Integer): Boolean; begin Result := v < 15 end;
begin
  e := TIntArrayCursor.Create([]);
  AssertTrue(e.SkipWhile(@IsLessThen15).None);
  e := TIntArrayCursor.Create(TIntHelper.CreateCopy(IntStrictInc21));
  a := e.SkipWhile(@IsLessThen15).ToArray;
  AssertTrue(TIntHelper.Same(a, [15, 16, 17, 18, 19, 20, 21]));
  e := TIntArrayCursor.Create([1, 4, 10, 15, 11, 9, 3, 5]);
  a := e.SkipWhile(@IsLessThen15).ToArray;
  AssertTrue(TIntHelper.Same(a, [15, 11, 9, 3, 5]));
  e := TIntArrayCursor.Create([14, 1, 4, 10, 13, 11, 9, 3, 5]);
  AssertTrue(e.SkipWhile(@IsLessThen15).None);
end;


initialization

  RegisterTest(TEnumerableTest);

end.

