unit LGRegularArrayHelperTest;

{$mode objfpc}{$H+}

interface

uses

  SysUtils, fpcunit, testregistry,
  LGUtils,
  LGArrayHelpers,
  LGMiscUtils;

type

  TIntPair    = record
    Data,
    Index: Integer;
  end;

  { TRegularArrayHelperTest }

  TRegularArrayHelperTest = class(TTestCase)
  private
  type

    TIntHelper  = specialize TGRegularArrayHelper<Integer>;
    TIntTimSort = specialize TGRegularTimSort<Integer>;
    THackHelper = class(TIntHelper);
    TIntArray   = specialize TGArray<Integer>;

    TIntPairs    = specialize TGArray<TIntPair>;

    TPairIdxCmp  = class
      class function Less(const L, R: TIntPair): Boolean; static;
    end;

    TPairDataHelper = specialize TGRegularArrayHelper<TIntPair>;
    TPairTimSort    = specialize TGRegularTimSort<TIntPair>;

    TPairIdxHelper  = specialize TGBaseArrayHelper<TIntPair, TPairIdxCmp>;

  published
    procedure SequentSearchEmpty;
    procedure SequentSearchStaticSuccess1;
    procedure SequentSearchStaticUnsuccess1;
    procedure SequentSearchDynSuccess1;
    procedure SequentSearchDynUnsuccess1;
    procedure SequentSearchStaticSuccess10;
    procedure SequentSearchStaticUnsuccess10;

    procedure BinarySearchEmpty;
    procedure BinarySearchStaticSuccess1;
    procedure BinarySearchStaticUnsuccess1;
    procedure BinarySearchStaticSuccess2;
    procedure BinarySearchStaticUnsuccess2;
    procedure BinarySearchStaticSuccess10Uniq;
    procedure BinarySearchStaticUnsuccess10Uniq;
    procedure BinarySearchStaticSuccess20;
    procedure BinarySearchStaticSuccess20_1;
    procedure BinarySearchStaticSuccess20_2;
    procedure BinarySearchStaticSuccess20_3;
    procedure BinarySearchStaticUnsuccessEqual10;
    procedure BinarySearchStaticSuccessEqual10;

    procedure BinarySearchStaticUnsuccess20;
    procedure BinarySearchStaticUnsuccess20_1;
    procedure BinarySearchDynSuccess1;
    procedure BinarySearchDynUnsuccess1;
    procedure BinarySearchDynSuccess2;
    procedure BinarySearchDynUnsuccess2;
    procedure BinarySearchDynSuccess10Uniq;
    procedure BinarySearchDynUnsuccess10Uniq;
    procedure BinarySearchDynSuccess20;
    procedure BinarySearchDynSuccess20_1;
    procedure BinarySearchDynUnsuccess20;
    procedure BinarySearchDynUnsuccess20_1;
    procedure BinarySearchDynSuccess20_2;
    procedure BinarySearchDynSuccess20_3;
    procedure BinarySearchDynUnsuccessEqual10;
    procedure BinarySearchDynSuccessEqual10;

    procedure BinarySearchPos;
    procedure BinarySearchPos1;
    procedure BinarySearchPos2;
    procedure BinarySearchPos3;

    procedure IndexOfMinEmpty;
    procedure IndexOfMinStatic1;
    procedure IndexOfMinStatic2;
    procedure IndexOfMinStatic10;
    procedure IndexOfMinDyn1;
    procedure IndexOfMinDyn2;
    procedure IndexOfMinDyn10;

    procedure IndexOfMaxEmpty;
    procedure IndexOfMaxStatic1;
    procedure IndexOfMaxStatic2;
    procedure IndexOfMaxStatic10;
    procedure IndexOfMaxDyn1;
    procedure IndexOfMaxDyn2;
    procedure IndexOfMaxDyn10;

    procedure GetMinOfEmpty;
    procedure GetMinOfStatic1;
    procedure GetMinOfStatic2;
    procedure GetMinOfStatic10;
    procedure GetMinOfDyn1;
    procedure GetMinOfDyn2;
    procedure GetMinOfDyn10;

    procedure GetMaxOfEmpty;
    procedure GetMaxOfStatic1;
    procedure GetMaxOfStatic2;
    procedure GetMaxOfStatic10;
    procedure GetMaxOfDyn1;
    procedure GetMaxOfDyn2;
    procedure GetMaxOfDyn10;

    procedure FindMinOfEmpty;
    procedure FindMinOfStatic1;
    procedure FindMinOfStatic2;
    procedure FindMinOfStatic10;
    procedure FindMinOfDyn1;
    procedure FindMinOfDyn2;
    procedure FindMinOfDyn10;

    procedure FindMaxOfEmpty;
    procedure FindMaxOfStatic1;
    procedure FindMaxOfStatic2;
    procedure FindMaxOfStatic10;
    procedure FindMaxOfDyn1;
    procedure FindMaxOfDyn2;
    procedure FindMaxOfDyn10;

    procedure FindMinMaxOfEmpty;
    procedure FindMinMaxOfStatic1;
    procedure FindMinMaxOfStatic2;
    procedure FindMinMaxOfStatic10;
    procedure FindMinMaxOfDyn1;
    procedure FindMinMaxOfDyn2;
    procedure FindMinMaxOfDyn10;

    procedure FindNthSmallestOfEmpty;
    procedure FindNthSmallestOfStatic1;
    procedure FindNthSmallestOfStatic1_1;
    procedure FindNthSmallestOfStatic1_2;
    procedure FindNthSmallestOfStatic21;
    procedure FindNthSmallestOfStatic21_1;
    procedure FindNthSmallestOfStatic21_2;
    procedure FindNthSmallestOfStatic21_3;
    procedure FindNthSmallestOfStatic21_4;
    procedure FindNthSmallestOfStatic21_5;
    procedure FindNthSmallestOfStatic21_6;
    procedure FindNthSmallestOfStatic21_7;
    procedure FindNthSmallestOfStatic21_8;
    procedure FindNthSmallestOfDyn1;
    procedure FindNthSmallestOfDyn1_1;
    procedure FindNthSmallestOfDyn1_2;
    procedure FindNthSmallestOfDyn21;
    procedure FindNthSmallestOfDyn21_1;
    procedure FindNthSmallestOfDyn21_2;
    procedure FindNthSmallestOfDyn21_3;
    procedure FindNthSmallestOfDyn21_4;
    procedure FindNthSmallestOfDyn21_5;
    procedure FindNthSmallestOfDyn21_6;
    procedure FindNthSmallestOfDyn21_7;
    procedure FindNthSmallestOfDyn21_8;

    procedure NthSmallestOfEmpty;
    procedure NthSmallestOfStatic1;
    procedure NthSmallestOfStatic1_1;
    procedure NthSmallestOfStatic1_2;
    procedure NthSmallestOfStatic21;
    procedure NthSmallestOfStatic21_1;
    procedure NthSmallestOfStatic21_2;
    procedure NthSmallestOfStatic21_3;
    procedure NthSmallestOfStatic21_4;
    procedure NthSmallestOfStatic21_5;
    procedure NthSmallestOfStatic21_6;
    procedure NthSmallestOfStatic21_7;
    procedure NthSmallestOfStatic21_8;
    procedure NthSmallestOfDyn1;
    procedure NthSmallestOfDyn1_1;
    procedure NthSmallestOfDyn1_2;
    procedure NthSmallestOfDyn21;
    procedure NthSmallestOfDyn21_1;
    procedure NthSmallestOfDyn21_2;
    procedure NthSmallestOfDyn21_3;
    procedure NthSmallestOfDyn21_4;
    procedure NthSmallestOfDyn21_5;
    procedure NthSmallestOfDyn21_6;
    procedure NthSmallestOfDyn21_7;
    procedure NthSmallestOfDyn21_8;

    procedure FindNthSmallestNDOfStatic21;
    procedure FindNthSmallestNDOfStatic21_1;
    procedure FindNthSmallestNDOfDyn21;
    procedure FindNthSmallestNDOfDyn21_1;

    procedure NthSmallestNDOfStatic21;
    procedure NthSmallestNDOfStatic21_1;
    procedure NthSmallestNDOfDyn21;
    procedure NthSmallestNDOfDyn21_1;

    procedure NextPermutation2AscOfEmpty;
    procedure NextPermutation2AscOfStatic1;
    procedure NextPermutation2AscOfStatic2Asc;
    procedure NextPermutation2AscOfStatic2Desc;
    procedure NextPermutation2AscOfStatic10Equal;
    procedure NextPermutation2AscOfStatic3;
    procedure NextPermutation2AscOfStatic4;
    procedure NextPermutation2AscOfDyn1;
    procedure NextPermutation2AscOfDyn2Asc;
    procedure NextPermutation2AscOfDyn2Desc;
    procedure NextPermutation2AscOfDyn10Equal;
    procedure NextPermutation2AscOfDyn3;
    procedure NextPermutation2AscOfDyn4;

    procedure NextPermutation2DescOfEmpty;
    procedure NextPermutation2DescOfStatic1;
    procedure NextPermutation2DescOfStatic2Asc;
    procedure NextPermutation2DescOfStatic2Desc;
    procedure NextPermutation2DescOfStatic10Equal;
    procedure NextPermutation2DescOfStatic3;
    procedure NextPermutation2DescOfStatic4;
    procedure NextPermutation2DescOfDyn1;
    procedure NextPermutation2DescOfDyn2Asc;
    procedure NextPermutation2DescOfDyn2Desc;
    procedure NextPermutation2DescOfDyn10Equal;
    procedure NextPermutation2DescOfDyn3;
    procedure NextPermutation2DescOfDyn4;

    procedure IsNonDescendingOfEmpty;
    procedure IsNonDescendingOfStatic1;
    procedure IsNonDescendingOfStatic10Equal;
    procedure IsNonDescendingOfStatic21Success;
    procedure IsNonDescendingOfStatic21Success_1;
    procedure IsNonDescendingOfStatic21Unsuccess;
    procedure IsNonDescendingOfStatic21Unsuccess_1;
    procedure IsNonDescendingOfDyn1;
    procedure IsNonDescendingOfDyn10Equal;
    procedure IsNonDescendingOfDyn21Success;
    procedure IsNonDescendingOfDyn21Success_1;
    procedure IsNonDescendingOfDyn21Unsuccess;
    procedure IsNonDescendingOfDyn21Unsuccess_1;

    procedure IsStrictAscendingOfEmpty;
    procedure IsStrictAscendingOfStatic1;
    procedure IsStrictAscendingOfStatic10Equal;
    procedure IsStrictAscendingOfStatic21Success;
    procedure IsStrictAscendingOfStatic21Unsuccess;
    procedure IsStrictAscendingOfDyn1;
    procedure IsStrictAscendingOfDyn10Equal;
    procedure IsStrictAscendingOfDyn21Success;
    procedure IsStrictAscendingOfDyn21Unsuccess;

    procedure IsNonAscendingOfEmpty;
    procedure IsNonAscendingOfStatic1;
    procedure IsNonAscendingOfStatic10Equal;
    procedure IsNonAscendingOfStatic21Success;
    procedure IsNonAscendingOfStatic21Success_1;
    procedure IsNonAscendingOfStatic21Unsuccess;
    procedure IsNonAscendingOfStatic21Unsuccess_1;
    procedure IsNonAscendingOfDyn1;
    procedure IsNonAscendingOfDyn10Equal;
    procedure IsNonAscendingOfDyn21Success;
    procedure IsNonAscendingOfDyn21Success_1;
    procedure IsNonAscendingOfDyn21Unsuccess;
    procedure IsNonAscendingOfDyn21Unsuccess_1;

    procedure IsStrictDescendingOfEmpty;
    procedure IsStrictDescendingOfStatic1;
    procedure IsStrictDescendingOfStatic10Equal;
    procedure IsStrictDescendingOfStatic21Success;
    procedure IsStrictDescendingOfStatic21Unsuccess;
    procedure IsStrictDescendingOfDyn1;
    procedure IsStrictDescendingOfDyn10Equal;
    procedure IsStrictDescendingOfDyn21Success;
    procedure IsStrictDescendingOfDyn21Unsuccess;

    procedure InversionCount;
    procedure InversionCountND;
    procedure Lis;

    procedure SameOfEmpty;
    procedure SameOfStatic1Success;
    procedure SameOfStatic1Unsuccess;
    procedure SameOfStatic2Success;
    procedure SameOfStatic2Unsuccess;
    procedure SameOfStatic21Success;
    procedure SameOfStatic21Unsuccess;
    procedure SameOfDyn1Success;
    procedure SameOfDyn1Unsuccess;
    procedure SameOfDyn2Success;
    procedure SameOfDyn2Unsuccess;
    procedure SameOfDyn21Success;
    procedure SameOfDyn21Unsuccess;

    procedure QuickSortOfEmpty;
    procedure QuickSortAscOfStatic3Asc;
    procedure QuickSortAscOfStatic3Desc;
    procedure QuickSortDescOfStatic3Desc;
    procedure QuickSortDescOfStatic3Asc;
    procedure QuickSortAscOfStatic377;
    procedure QuickSortDescOfStatic377;
    procedure QuickSortAscOfStatic377Random;
    procedure QuickSortAscOfStatic377RandomZeroes;
    procedure QuickSortDescOfStatic377Random;
    procedure QuickSortDescOfStatic377RandomZeroes;
    procedure QuickSortAscOfDyn3Asc;
    procedure QuickSortAscOfDyn3Desc;
    procedure QuickSortDescOfDyn3Desc;
    procedure QuickSortDescOfDyn3Asc;
    procedure QuickSortAscOfDyn377;
    procedure QuickSortDescOfDyn377;
    procedure QuickSortAscOfDyn377Random;
    procedure QuickSortAscOfDyn377RandomZeroes;
    procedure QuickSortDescOfDyn377Random;
    procedure QuickSortDescOfDyn377RandomZeroes;

    procedure HeapSortOfStatic277;
    procedure HeapSortOfStatic277Random;
    procedure HeapSortOfStatic277RandomZeroes;
    procedure HeapSortOfDyn277;
    procedure HeapSortOfDyn277Random;
    procedure HeapSortOfDyn277RandomZeroes;

    procedure IntroSortOfEmpty;
    procedure IntroSortAscOfStatic3Asc;
    procedure IntroSortAscOfStatic3Desc;
    procedure IntroSortDescOfStatic3Desc;
    procedure IntroSortDescOfStatic3Asc;
    procedure IntroSortAscOfStatic577;
    procedure IntroSortDescOfStatic577;
    procedure IntroSortAscOfStatic577Random;
    procedure IntroSortAscOfStatic577RandomZeroes;
    procedure IntroSortDescOfStatic577Random;
    procedure IntroSortDescOfStatic577RandomZeroes;
    procedure IntroSortAscOfDyn3Asc;
    procedure IntroSortAscOfDyn3Desc;
    procedure IntroSortDescOfDyn3Desc;
    procedure IntroSortDescOfDyn3Asc;
    procedure IntroSortAscOfDyn577;
    procedure IntroSortDescOfDyn577;
    procedure IntroSortAscOfDyn577Random;
    procedure IntroSortAscOfDyn577RandomZeroes;
    procedure IntroSortDescOfDyn577Random;
    procedure IntroSortDescOfDyn577RandomZeroes;

    procedure DualPivotQuickSortOfEmpty;
    procedure DualPivotQuickSortAscOfStatic3Asc;
    procedure DualPivotQuickSortAscOfStatic3Desc;
    procedure DualPivotQuickSortDescOfStatic3Desc;
    procedure DualPivotQuickSortDescOfStatic3Asc;
    procedure DualPivotQuickSortAscOfStatic577;
    procedure DualPivotQuickSortDescOfStatic577;
    procedure DualPivotQuickSortAscOfStatic577Random;
    procedure DualPivotQuickSortAscOfStatic577RandomZeroes;
    procedure DualPivotQuickSortDescOfStatic577Random;
    procedure DualPivotQuickSortDescOfStatic577RandomZeroes;
    procedure DualPivotQuickSortAscOfDyn3Asc;
    procedure DualPivotQuickSortAscOfDyn3Desc;
    procedure DualPivotQuickSortDescOfDyn3Desc;
    procedure DualPivotQuickSortDescOfDyn3Asc;
    procedure DualPivotQuickSortAscOfDyn577;
    procedure DualPivotQuickSortDescOfDyn577;
    procedure DualPivotQuickSortAscOfDyn577Random;
    procedure DualPivotQuickSortAscOfDyn577RandomZeroes;
    procedure DualPivotQuickSortDescOfDyn577Random;
    procedure DualPivotQuickSortDescOfDyn577RandomZeroes;

    procedure MergeSortOfEmpty;
    procedure MergeSortAscOfStatic3Asc;
    procedure MergeSortAscOfStatic3Desc;
    procedure MergeSortDescOfStatic3Desc;
    procedure MergeSortDescOfStatic3Asc;
    procedure MergeSortAscOfStatic577;
    procedure MergeSortDescOfStatic577;
    procedure MergeSortAscOfStatic577Random;
    procedure MergeSortAscOfStatic577RandomZeroes;
    procedure MergeSortDescOfStatic577Random;
    procedure MergeSortDescOfStatic577RandomZeroes;
    procedure MergeSortAscOfDyn3Asc;
    procedure MergeSortAscOfDyn3Desc;
    procedure MergeSortDescOfDyn3Desc;
    procedure MergeSortDescOfDyn3Asc;
    procedure MergeSortAscOfDyn577;
    procedure MergeSortDescOfDyn577;
    procedure MergeSortAscOfDyn577Random;
    procedure MergeSortAscOfDyn577RandomZeroes;
    procedure MergeSortDescOfDyn577Random;
    procedure MergeSortDescOfDyn577RandomZeroes;
    procedure MergeSortAscOfStatic877Stable;
    procedure MergeSortDescOfStatic877Stable;
    procedure MergeSortAscOfDyn877Stable;
    procedure MergeSortDescOfDyn877Stable;

    procedure PDQSortTest;
    procedure TimSortTest;
    procedure TimSortAscStableTest;
    procedure TimSortDescStableTest;
  end;

implementation
{$B-}{$COPERATORS ON}

type

  TIntArray21 = array[0..20] of Integer;
  TIntArray10 = array[1..10] of Integer;

const

  IntSrc10: array[2..11] of Integer = (10, -15, 117, 21, -199, 1, 13, 0, -29, -1);

  IntSrc7: array[0..6] of Integer = (101, 0, -11, -27, 127, 1, 0);

  IntSorted10Uniq: array[1..10] of Integer = (-22, -1, 0, 3, 14, 51, 69, 177, 281, 593);

  IntEqual10: TIntArray10 = (9, 9, 9, 9, 9, 9, 9, 9, 9, 9);

  IntSorted20: array[1..20] of Integer = (-5, -5, -3, 0, 0, 2, 2, 5, 7, 7, 8, 10, 14, 17, 17, 28, 28, 39, 40, 40);

  InOrderSrc21: TIntArray21 = (0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20);

  InOrderDblSrc21: TIntArray21 = (0, 0, 1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6, 7, 7, 8, 8, 9, 9, 10);

function IntCmp(const L, R: Integer): Boolean;
begin
  Result := L < R;
end;


function PairCmp(const L, R: TIntPair): Boolean;
begin
  Result := L.Data < R.Data;
end;

{ TRegularArrayHelperTest.TPairIdxCmp }

class function TRegularArrayHelperTest.TPairIdxCmp.Less(const L, R: TIntPair): Boolean;
begin
  Result := L.Index < R.Index;
end;

{ TRegularArrayHelperTest }

procedure TRegularArrayHelperTest.SequentSearchEmpty;
var
  a: TIntArray;
begin
  AssertTrue(TIntHelper.SequentSearch(a{%H-}, 13, @IntCmp) = -1);
end;

procedure TRegularArrayHelperTest.SequentSearchStaticSuccess1;
begin
  AssertTrue(TIntHelper.SequentSearch(IntSrc10[3..3], -15, @IntCmp) = 0);
end;

procedure TRegularArrayHelperTest.SequentSearchStaticUnsuccess1;
begin
  AssertTrue(TIntHelper.SequentSearch(IntSrc10[3..3], 10, @IntCmp) = -1);
end;

procedure TRegularArrayHelperTest.SequentSearchDynSuccess1;
begin
  AssertTrue(TIntHelper.SequentSearch(TIntArray.Create(-15), -15, @IntCmp) = 0);
end;

procedure TRegularArrayHelperTest.SequentSearchDynUnsuccess1;
begin
  AssertTrue(TIntHelper.SequentSearch(TIntArray.Create(-15), 10, @IntCmp) = -1);
end;

procedure TRegularArrayHelperTest.SequentSearchStaticSuccess10;
begin
  AssertTrue(TIntHelper.SequentSearch(IntSrc10, -1, @IntCmp) = 9);
end;

procedure TRegularArrayHelperTest.SequentSearchStaticUnsuccess10;
begin
  AssertTrue(TIntHelper.SequentSearch(IntSrc10, 11, @IntCmp) = -1);
end;

procedure TRegularArrayHelperTest.BinarySearchEmpty;
var
  a: TIntArray;
begin
  AssertTrue(TIntHelper.BinarySearch(a{%H-}, 13, @IntCmp) = -1);
end;

procedure TRegularArrayHelperTest.BinarySearchStaticSuccess1;
begin
  AssertTrue(TIntHelper.BinarySearch(IntSrc10[3..3], -15, @IntCmp) = 0);
end;

procedure TRegularArrayHelperTest.BinarySearchStaticUnsuccess1;
begin
  AssertTrue(TIntHelper.BinarySearch(IntSrc10[3..3], 10, @IntCmp) = -1);
end;

procedure TRegularArrayHelperTest.BinarySearchStaticSuccess2;
begin
  AssertTrue(TIntHelper.BinarySearch(IntSorted10Uniq[3..4], 3, @IntCmp) = 1);
end;

procedure TRegularArrayHelperTest.BinarySearchStaticUnsuccess2;
begin
  AssertTrue(TIntHelper.BinarySearch(IntSorted10Uniq[3..4], 2, @IntCmp) = -1);
end;

procedure TRegularArrayHelperTest.BinarySearchStaticSuccess10Uniq;
begin
  AssertTrue(TIntHelper.BinarySearch(IntSorted10Uniq, 14, @IntCmp) = 4);
end;

procedure TRegularArrayHelperTest.BinarySearchStaticUnsuccess10Uniq;
begin
  AssertTrue(TIntHelper.BinarySearch(IntSorted10Uniq, 11, @IntCmp) = -1);
end;

procedure TRegularArrayHelperTest.BinarySearchStaticSuccess20;
var
  I: SizeInt;
begin
  I := TIntHelper.BinarySearch(IntSorted20, 2, @IntCmp);
  AssertTrue((I = 5) or (I = 6));
end;

procedure TRegularArrayHelperTest.BinarySearchStaticSuccess20_1;
begin
  AssertTrue(TIntHelper.BinarySearch(IntSorted20, 8, @IntCmp) = 10);
end;

procedure TRegularArrayHelperTest.BinarySearchStaticSuccess20_2;
begin
  AssertTrue(TIntHelper.BinarySearch(IntSorted20, -5, @IntCmp) = 0);
end;

procedure TRegularArrayHelperTest.BinarySearchStaticSuccess20_3;
begin
  AssertTrue(TIntHelper.BinarySearch(IntSorted20, 17, @IntCmp) = 13);
end;

procedure TRegularArrayHelperTest.BinarySearchStaticUnsuccessEqual10;
begin
  AssertTrue(TIntHelper.BinarySearch(IntEqual10, 5, @IntCmp) = -1);
end;

procedure TRegularArrayHelperTest.BinarySearchStaticSuccessEqual10;
begin
  AssertTrue(TIntHelper.BinarySearch(IntEqual10, 9, @IntCmp) = 0);
end;

procedure TRegularArrayHelperTest.BinarySearchStaticUnsuccess20;
begin
  AssertTrue(TIntHelper.BinarySearch(IntSorted20, -8, @IntCmp) = -1);
end;

procedure TRegularArrayHelperTest.BinarySearchStaticUnsuccess20_1;
begin
  AssertTrue(TIntHelper.BinarySearch(IntSorted20, 9, @IntCmp) = -1);
end;

procedure TRegularArrayHelperTest.BinarySearchDynSuccess1;
begin
  AssertTrue(TIntHelper.BinarySearch(TIntHelper.CreateCopy(IntSrc10[3..3]), -15, @IntCmp) = 0);
end;

procedure TRegularArrayHelperTest.BinarySearchDynUnsuccess1;
begin
  AssertTrue(TIntHelper.BinarySearch(TIntHelper.CreateCopy(IntSrc10[3..3]), 10, @IntCmp) = -1);
end;

procedure TRegularArrayHelperTest.BinarySearchDynSuccess2;
begin
  AssertTrue(TIntHelper.BinarySearch(TIntHelper.CreateCopy(IntSorted10Uniq[3..4]), 3, @IntCmp) = 1);
end;

procedure TRegularArrayHelperTest.BinarySearchDynUnsuccess2;
begin
  AssertTrue(TIntHelper.BinarySearch(TIntHelper.CreateCopy(IntSorted10Uniq[3..4]), 2, @IntCmp) = -1);
end;

procedure TRegularArrayHelperTest.BinarySearchDynSuccess10Uniq;
begin
  AssertTrue(TIntHelper.BinarySearch(TIntHelper.CreateCopy(IntSorted10Uniq), 281, @IntCmp) = 8);
end;

procedure TRegularArrayHelperTest.BinarySearchDynUnsuccess10Uniq;
begin
  AssertTrue(TIntHelper.BinarySearch(TIntHelper.CreateCopy(IntSorted10Uniq), 15, @IntCmp) = -1);
end;

procedure TRegularArrayHelperTest.BinarySearchDynSuccess20;
var
  I: SizeInt;
begin
  I := TIntHelper.BinarySearch(TIntHelper.CreateCopy(IntSorted20), 2, @IntCmp);
  AssertTrue((I = 5) or (I = 6));
end;

procedure TRegularArrayHelperTest.BinarySearchDynSuccess20_1;
begin
  AssertTrue(TIntHelper.BinarySearch(TIntHelper.CreateCopy(IntSorted20), 8, @IntCmp) = 10);
end;

procedure TRegularArrayHelperTest.BinarySearchDynUnsuccess20;
begin
  AssertTrue(TIntHelper.BinarySearch(TIntHelper.CreateCopy(IntSorted20), -8, @IntCmp) = -1);
end;

procedure TRegularArrayHelperTest.BinarySearchDynUnsuccess20_1;
begin
  AssertTrue(TIntHelper.BinarySearch(TIntHelper.CreateCopy(IntSorted20), 9, @IntCmp) = -1);
end;

procedure TRegularArrayHelperTest.BinarySearchDynSuccess20_2;
begin
  AssertTrue(TIntHelper.BinarySearch(TIntHelper.CreateCopy(IntSorted20), -5, @IntCmp) = 0);
end;

procedure TRegularArrayHelperTest.BinarySearchDynSuccess20_3;
begin
  AssertTrue(TIntHelper.BinarySearch(TIntHelper.CreateCopy(IntSorted20), 17, @IntCmp) = 13);
end;

procedure TRegularArrayHelperTest.BinarySearchDynUnsuccessEqual10;
begin
  AssertTrue(TIntHelper.BinarySearch(TIntHelper.CreateCopy(IntEqual10), 5, @IntCmp) = -1);
end;

procedure TRegularArrayHelperTest.BinarySearchDynSuccessEqual10;
begin
  AssertTrue(TIntHelper.BinarySearch(TIntHelper.CreateCopy(IntEqual10), 9, @IntCmp) = 0);
end;

procedure TRegularArrayHelperTest.BinarySearchPos;
var
  sr: TSearchResult;
begin
  sr := TIntHelper.BinarySearchPos([], 0, @IntCmp);
  AssertTrue(sr.FoundIndex = -1);
  AssertTrue(sr.InsertIndex = 0);
  sr := TIntHelper.BinarySearchPos([1], 0, @IntCmp);
  AssertTrue(sr.FoundIndex = -1);
  AssertTrue(sr.InsertIndex = 0);
  sr := TIntHelper.BinarySearchPos([1], 2, @IntCmp);
  AssertTrue(sr.FoundIndex = -1);
  AssertTrue(sr.InsertIndex = 1);
  sr := TIntHelper.BinarySearchPos([1], 1, @IntCmp);
  AssertTrue(sr.FoundIndex = 0);
  AssertTrue(sr.InsertIndex = 1);
end;

procedure TRegularArrayHelperTest.BinarySearchPos1;
var
  sr: TSearchResult;
begin
  sr := TIntHelper.BinarySearchPos(IntEqual10, 5, @IntCmp);
  AssertTrue(sr.FoundIndex = -1);
  AssertTrue(sr.InsertIndex = 0);
  sr := TIntHelper.BinarySearchPos(IntEqual10, 10, @IntCmp);
  AssertTrue(sr.FoundIndex = -1);
  AssertTrue(sr.InsertIndex = 10);
  sr := TIntHelper.BinarySearchPos(IntEqual10, 9, @IntCmp);
  AssertTrue(sr.FoundIndex = 9);
  AssertTrue(sr.InsertIndex = 10);
end;

procedure TRegularArrayHelperTest.BinarySearchPos2;
var
  sr: TSearchResult;
begin
  sr := TIntHelper.BinarySearchPos(IntSorted20, -6, @IntCmp);
  AssertTrue(sr.FoundIndex = -1);
  AssertTrue(sr.InsertIndex = 0);
  sr := TIntHelper.BinarySearchPos(IntSorted20, 41, @IntCmp);
  AssertTrue(sr.FoundIndex = -1);
  AssertTrue(sr.InsertIndex = 20);
  sr := TIntHelper.BinarySearchPos(IntSorted20, -5, @IntCmp);
  AssertTrue(sr.FoundIndex = 1);
  AssertTrue(sr.InsertIndex = 2);
  sr := TIntHelper.BinarySearchPos(IntSorted20, 4, @IntCmp);
  AssertTrue(sr.FoundIndex = -1);
  AssertTrue(sr.InsertIndex = 7);
end;

procedure TRegularArrayHelperTest.BinarySearchPos3;
var
  sr: TSearchResult;
  a: array of Integer;
begin
  a := TIntHelper.CreateReverseCopy(IntSorted20);
  sr := TIntHelper.BinarySearchPos(a, -6, @IntCmp);
  AssertTrue(sr.FoundIndex = -1);
  AssertTrue(sr.InsertIndex = 20);
  sr := TIntHelper.BinarySearchPos(a, 41, @IntCmp);
  AssertTrue(sr.FoundIndex = -1);
  AssertTrue(sr.InsertIndex = 0);
  sr := TIntHelper.BinarySearchPos(a, -5, @IntCmp);
  AssertTrue(sr.FoundIndex = 19);
  AssertTrue(sr.InsertIndex = 20);
  sr := TIntHelper.BinarySearchPos(a, 4, @IntCmp);
  AssertTrue(sr.FoundIndex = -1);
  AssertTrue(sr.InsertIndex = 13);
end;

procedure TRegularArrayHelperTest.IndexOfMinEmpty;
var
  a: TIntArray;
begin
  AssertTrue(TIntHelper.IndexOfMin(a{%H-}, @IntCmp) = -1);
end;

procedure TRegularArrayHelperTest.IndexOfMinStatic1;
begin
  AssertTrue(TIntHelper.IndexOfMin(IntSrc10[3..3], @IntCmp) = 0);
end;

procedure TRegularArrayHelperTest.IndexOfMinStatic2;
begin
  AssertTrue(TIntHelper.IndexOfMin(IntSrc10[3..4], @IntCmp) = 0);
end;

procedure TRegularArrayHelperTest.IndexOfMinStatic10;
begin
  AssertTrue(TIntHelper.IndexOfMin(IntSrc10, @IntCmp) = 4);
end;

procedure TRegularArrayHelperTest.IndexOfMinDyn1;
begin
  AssertTrue(TIntHelper.IndexOfMin(TIntHelper.CreateCopy(IntSrc10[3..3]), @IntCmp) = 0);
end;

procedure TRegularArrayHelperTest.IndexOfMinDyn2;
begin
  AssertTrue(TIntHelper.IndexOfMin(TIntHelper.CreateCopy(IntSrc10[3..4]), @IntCmp) = 0);
end;

procedure TRegularArrayHelperTest.IndexOfMinDyn10;
begin
  AssertTrue(TIntHelper.IndexOfMin(TIntHelper.CreateCopy(IntSrc10), @IntCmp) = 4);
end;

procedure TRegularArrayHelperTest.IndexOfMaxEmpty;
var
  a: TIntArray;
begin
  AssertTrue(TIntHelper.IndexOfMax(a{%H-}, @IntCmp) = -1);
end;

procedure TRegularArrayHelperTest.IndexOfMaxStatic1;
begin
  AssertTrue(TIntHelper.IndexOfMax(IntSrc10[3..3], @IntCmp) = 0);
end;

procedure TRegularArrayHelperTest.IndexOfMaxStatic2;
begin
  AssertTrue(TIntHelper.IndexOfMax(IntSrc10[3..4], @IntCmp) = 1);
end;

procedure TRegularArrayHelperTest.IndexOfMaxStatic10;
begin
  AssertTrue(TIntHelper.IndexOfMax(IntSrc10, @IntCmp) = 2);
end;

procedure TRegularArrayHelperTest.IndexOfMaxDyn1;
begin
  AssertTrue(TIntHelper.IndexOfMax(TIntHelper.CreateCopy(IntSrc10[3..3]), @IntCmp) = 0);
end;

procedure TRegularArrayHelperTest.IndexOfMaxDyn2;
begin
  AssertTrue(TIntHelper.IndexOfMax(TIntHelper.CreateCopy(IntSrc10[3..4]), @IntCmp) = 1);
end;

procedure TRegularArrayHelperTest.IndexOfMaxDyn10;
begin
  AssertTrue(TIntHelper.IndexOfMax(TIntHelper.CreateCopy(IntSrc10), @IntCmp) = 2);
end;

procedure TRegularArrayHelperTest.GetMinOfEmpty;
var
  a: TIntArray;
begin
  AssertFalse(TIntHelper.GetMin(a{%H-}, @IntCmp).Assigned);
end;

procedure TRegularArrayHelperTest.GetMinOfStatic1;
begin
  AssertTrue(TIntHelper.GetMin(IntSrc10[5..5], @IntCmp).Value = 21);
end;

procedure TRegularArrayHelperTest.GetMinOfStatic2;
begin
  AssertTrue(TIntHelper.GetMin(IntSrc10[7..8], @IntCmp).Value = 1);
end;

procedure TRegularArrayHelperTest.GetMinOfStatic10;
begin
  AssertTrue(TIntHelper.GetMin(IntSrc10, @IntCmp).Value = -199);
end;

procedure TRegularArrayHelperTest.GetMinOfDyn1;
begin
  AssertTrue(TIntHelper.GetMin(TIntHelper.CreateCopy(IntSrc10[5..5]), @IntCmp).Value = 21);
end;

procedure TRegularArrayHelperTest.GetMinOfDyn2;
begin
  AssertTrue(TIntHelper.GetMin(TIntHelper.CreateCopy(IntSrc10[7..8]), @IntCmp).Value = 1);
end;

procedure TRegularArrayHelperTest.GetMinOfDyn10;
begin
  AssertTrue(TIntHelper.GetMin(TIntHelper.CreateCopy(IntSrc10), @IntCmp).Value = -199);
end;

procedure TRegularArrayHelperTest.GetMaxOfEmpty;
var
  a: TIntArray;
begin
  AssertFalse(TIntHelper.GetMax(a{%H-}, @IntCmp).Assigned);
end;

procedure TRegularArrayHelperTest.GetMaxOfStatic1;
begin
  AssertTrue(TIntHelper.GetMax(IntSrc10[9..9], @IntCmp).Value = 0);
end;

procedure TRegularArrayHelperTest.GetMaxOfStatic2;
begin
  AssertTrue(TIntHelper.GetMax(IntSrc10[7..8], @IntCmp).Value = 13);
end;

procedure TRegularArrayHelperTest.GetMaxOfStatic10;
begin
  AssertTrue(TIntHelper.GetMax(IntSrc10, @IntCmp).Value = 117);
end;

procedure TRegularArrayHelperTest.GetMaxOfDyn1;
begin
  AssertTrue(TIntHelper.GetMax(TIntHelper.CreateCopy(IntSrc10[9..9]), @IntCmp).Value = 0);
end;

procedure TRegularArrayHelperTest.GetMaxOfDyn2;
begin
  AssertTrue(TIntHelper.GetMax(TIntHelper.CreateCopy(IntSrc10[7..8]), @IntCmp).Value = 13);
end;

procedure TRegularArrayHelperTest.GetMaxOfDyn10;
begin
  AssertTrue(TIntHelper.GetMax(TIntHelper.CreateCopy(IntSrc10), @IntCmp).Value = 117);
end;

procedure TRegularArrayHelperTest.FindMinOfEmpty;
var
  a: TIntArray;
  m: Integer;
begin
  AssertFalse(TIntHelper.FindMin(a{%H-}, m, @IntCmp));
end;

procedure TRegularArrayHelperTest.FindMinOfStatic1;
var
  m: Integer;
begin
  AssertTrue(TIntHelper.FindMin(IntSrc10[7..7], m, @IntCmp));
  AssertTrue(m = 1);
end;

procedure TRegularArrayHelperTest.FindMinOfStatic2;
var
  m: Integer;
begin
  AssertTrue(TIntHelper.FindMin(IntSrc10[2..3], m, @IntCmp));
  AssertTrue(m = -15);
end;

procedure TRegularArrayHelperTest.FindMinOfStatic10;
var
  m: Integer;
begin
  AssertTrue(TIntHelper.FindMin(IntSrc10, m, @IntCmp));
  AssertTrue(m = -199);
end;

procedure TRegularArrayHelperTest.FindMinOfDyn1;
var
  m: Integer;
begin
  AssertTrue(TIntHelper.FindMin(TIntHelper.CreateCopy(IntSrc10[7..7]), m, @IntCmp));
  AssertTrue(m = 1);
end;

procedure TRegularArrayHelperTest.FindMinOfDyn2;
var
  m: Integer;
begin
  AssertTrue(TIntHelper.FindMin(TIntHelper.CreateCopy(IntSrc10[2..3]), m, @IntCmp));
  AssertTrue(m = -15);
end;

procedure TRegularArrayHelperTest.FindMinOfDyn10;
var
  m: Integer;
begin
  AssertTrue(TIntHelper.FindMin(TIntHelper.CreateCopy(IntSrc10), m, @IntCmp));
  AssertTrue(m = -199);
end;

procedure TRegularArrayHelperTest.FindMaxOfEmpty;
var
  a: TIntArray;
  m: Integer;
begin
  AssertFalse(TIntHelper.FindMax(a{%H-}, m, @IntCmp));
end;

procedure TRegularArrayHelperTest.FindMaxOfStatic1;
var
  m: Integer;
begin
  AssertTrue(TIntHelper.FindMax(IntSrc10[7..7], m, @IntCmp));
  AssertTrue(m = 1);
end;

procedure TRegularArrayHelperTest.FindMaxOfStatic2;
var
  m: Integer;
begin
  AssertTrue(TIntHelper.FindMax(IntSrc10[2..3], m, @IntCmp));
  AssertTrue(m = 10);
end;

procedure TRegularArrayHelperTest.FindMaxOfStatic10;
var
  m: Integer;
begin
  AssertTrue(TIntHelper.FindMax(IntSrc10, m, @IntCmp));
  AssertTrue(m = 117);
end;

procedure TRegularArrayHelperTest.FindMaxOfDyn1;
var
  m: Integer;
begin
  AssertTrue(TIntHelper.FindMax(TIntHelper.CreateCopy(IntSrc10[7..7]), m, @IntCmp));
  AssertTrue(m = 1);
end;

procedure TRegularArrayHelperTest.FindMaxOfDyn2;
var
  m: Integer;
begin
  AssertTrue(TIntHelper.FindMax(TIntHelper.CreateCopy(IntSrc10[2..3]), m, @IntCmp));
  AssertTrue(m = 10);
end;

procedure TRegularArrayHelperTest.FindMaxOfDyn10;
var
  m: Integer;
begin
  AssertTrue(TIntHelper.FindMax(TIntHelper.CreateCopy(IntSrc10), m, @IntCmp));
  AssertTrue(m = 117);
end;

procedure TRegularArrayHelperTest.FindMinMaxOfEmpty;
var
  a: TIntArray;
  mn, mx: Integer;
begin
  AssertFalse(TIntHelper.FindMinMax(a{%H-}, mn, mx, @IntCmp));
end;

procedure TRegularArrayHelperTest.FindMinMaxOfStatic1;
var
  mn, mx: Integer;
begin
  AssertTrue(TIntHelper.FindMinMax(IntSrc10[7..7], mn, mx, @IntCmp));
  AssertTrue(mn = 1);
  AssertTrue(mx = 1);
end;

procedure TRegularArrayHelperTest.FindMinMaxOfStatic2;
var
  mn, mx: Integer;
begin
  AssertTrue(TIntHelper.FindMinMax(IntSrc10[2..3], mn, mx, @IntCmp));
  AssertTrue(mn = -15);
  AssertTrue(mx = 10);
end;

procedure TRegularArrayHelperTest.FindMinMaxOfStatic10;
var
  mn, mx: Integer;
begin
  AssertTrue(TIntHelper.FindMinMax(IntSrc10, mn, mx, @IntCmp));
  AssertTrue(mn = -199);
  AssertTrue(mx = 117);
end;

procedure TRegularArrayHelperTest.FindMinMaxOfDyn1;
var
  mn, mx: Integer;
begin
  AssertTrue(TIntHelper.FindMinMax(TIntHelper.CreateCopy(IntSrc10[7..7]), mn, mx, @IntCmp));
  AssertTrue(mn = 1);
  AssertTrue(mx = 1);
end;

procedure TRegularArrayHelperTest.FindMinMaxOfDyn2;
var
  mn, mx: Integer;
begin
  AssertTrue(TIntHelper.FindMinMax(TIntHelper.CreateCopy(IntSrc10[2..3]), mn, mx, @IntCmp));
  AssertTrue(mn = -15);
  AssertTrue(mx = 10);
end;

procedure TRegularArrayHelperTest.FindMinMaxOfDyn10;
var
  mn, mx: Integer;
begin
  AssertTrue(TIntHelper.FindMinMax(TIntHelper.CreateCopy(IntSrc10), mn, mx, @IntCmp));
  AssertTrue(mn = -199);
  AssertTrue(mx = 117);
end;

procedure TRegularArrayHelperTest.FindNthSmallestOfEmpty;
var
  a: TIntArray;
  v: Integer;
begin
  AssertFalse(TIntHelper.FindNthSmallest(a{%H-}, 1, v, @IntCmp));
end;

procedure TRegularArrayHelperTest.FindNthSmallestOfStatic1;
var
  v: Integer;
begin
  AssertTrue(TIntHelper.FindNthSmallest(InOrderSrc21[3..3], 0, v, @IntCmp));
  AssertTrue(v = 3);
end;

procedure TRegularArrayHelperTest.FindNthSmallestOfStatic1_1;
var
  v: Integer;
begin
  AssertTrue(TIntHelper.FindNthSmallest(InOrderSrc21[5..5], -1, v, @IntCmp));
  AssertTrue(v = 5);
end;

procedure TRegularArrayHelperTest.FindNthSmallestOfStatic1_2;
var
  v: Integer;
begin
  AssertTrue(TIntHelper.FindNthSmallest(InOrderSrc21[2..2], 2, v, @IntCmp));
  AssertTrue(v = 2);
end;

procedure TRegularArrayHelperTest.FindNthSmallestOfStatic21;
var
  v: Integer;
  a: TIntArray21;
begin
  a := InOrderSrc21;
  TIntHelper.RandomShuffle(a);
  AssertTrue(TIntHelper.FindNthSmallest(a, 0, v, @IntCmp));
  AssertTrue(v = 0);
end;

procedure TRegularArrayHelperTest.FindNthSmallestOfStatic21_1;
var
  v: Integer;
  a: TIntArray21;
begin
  a := InOrderSrc21;
  TIntHelper.RandomShuffle(a);
  AssertTrue(TIntHelper.FindNthSmallest(a, -15, v, @IntCmp));
  AssertTrue(v = 0);
end;

procedure TRegularArrayHelperTest.FindNthSmallestOfStatic21_2;
var
  v: Integer;
  a: TIntArray21;
begin
  a := InOrderSrc21;
  TIntHelper.RandomShuffle(a);
  AssertTrue(TIntHelper.FindNthSmallest(a, 20, v, @IntCmp));
  AssertTrue(v = 20);
end;

procedure TRegularArrayHelperTest.FindNthSmallestOfStatic21_3;
var
  v: Integer;
  a: TIntArray21;
begin
  a := InOrderSrc21;
  TIntHelper.RandomShuffle(a);
  AssertTrue(TIntHelper.FindNthSmallest(a, 22, v, @IntCmp));
  AssertTrue(v = 20);
end;

procedure TRegularArrayHelperTest.FindNthSmallestOfStatic21_4;
var
  v: Integer;
  a: TIntArray21;
begin
  a := InOrderSrc21;
  TIntHelper.RandomShuffle(a);
  AssertTrue(TIntHelper.FindNthSmallest(a, 11, v, @IntCmp));
  AssertTrue(v = 11);
end;

procedure TRegularArrayHelperTest.FindNthSmallestOfStatic21_5;
var
  v: Integer;
  a: TIntArray21;
begin
  a := InOrderSrc21;
  TIntHelper.RandomShuffle(a);
  AssertTrue(TIntHelper.FindNthSmallest(a, 1, v, @IntCmp));
  AssertTrue(v = 1);
end;

procedure TRegularArrayHelperTest.FindNthSmallestOfStatic21_6;
var
  v: Integer;
  a: TIntArray21;
begin
  a := InOrderSrc21;
  TIntHelper.RandomShuffle(a);
  AssertTrue(TIntHelper.FindNthSmallest(a, 19, v, @IntCmp));
  AssertTrue(v = 19);
end;

procedure TRegularArrayHelperTest.FindNthSmallestOfStatic21_7;
var
  v: Integer;
  a: TIntArray21;
begin
  a := InOrderDblSrc21;
  TIntHelper.RandomShuffle(a);
  AssertTrue(TIntHelper.FindNthSmallest(a, 10, v, @IntCmp));
  AssertTrue(v = 5);
end;

procedure TRegularArrayHelperTest.FindNthSmallestOfStatic21_8;
var
  v: Integer;
  a: TIntArray21;
begin
  a := InOrderDblSrc21;
  TIntHelper.RandomShuffle(a);
  AssertTrue(TIntHelper.FindNthSmallest(a, 11, v, @IntCmp));
  AssertTrue(v = 5);
end;

procedure TRegularArrayHelperTest.FindNthSmallestOfDyn1;
var
  v: Integer;
begin
  AssertTrue(TIntHelper.FindNthSmallest(TIntHelper.CreateCopy(InOrderSrc21[6..6]), 0, v, @IntCmp));
  AssertTrue(v = 6);
end;

procedure TRegularArrayHelperTest.FindNthSmallestOfDyn1_1;
var
  v: Integer;
begin
  AssertTrue(TIntHelper.FindNthSmallest(TIntHelper.CreateCopy(InOrderSrc21[6..6]), -3, v, @IntCmp));
  AssertTrue(v = 6);
end;

procedure TRegularArrayHelperTest.FindNthSmallestOfDyn1_2;
var
  v: Integer;
begin
  AssertTrue(TIntHelper.FindNthSmallest(TIntHelper.CreateCopy(InOrderSrc21[6..6]), 4, v, @IntCmp));
  AssertTrue(v = 6);
end;

procedure TRegularArrayHelperTest.FindNthSmallestOfDyn21;
var
  v: Integer;
  a: TIntArray;
begin
  a := TIntHelper.CreateRandomShuffle(InOrderSrc21);
  AssertTrue(TIntHelper.FindNthSmallest(a, 0, v, @IntCmp));
  AssertTrue(v = 0);
end;

procedure TRegularArrayHelperTest.FindNthSmallestOfDyn21_1;
var
  v: Integer;
  a: TIntArray;
begin
  a := TIntHelper.CreateRandomShuffle(InOrderSrc21);
  AssertTrue(TIntHelper.FindNthSmallest(a, -3, v, @IntCmp));
  AssertTrue(v = 0);
end;

procedure TRegularArrayHelperTest.FindNthSmallestOfDyn21_2;
var
  v: Integer;
  a: TIntArray;
begin
  a := TIntHelper.CreateRandomShuffle(InOrderSrc21);
  AssertTrue(TIntHelper.FindNthSmallest(a, 20, v, @IntCmp));
  AssertTrue(v = 20);
end;

procedure TRegularArrayHelperTest.FindNthSmallestOfDyn21_3;
var
  v: Integer;
  a: TIntArray;
begin
  a := TIntHelper.CreateRandomShuffle(InOrderSrc21);
  AssertTrue(TIntHelper.FindNthSmallest(a, 22, v, @IntCmp));
  AssertTrue(v = 20);
end;

procedure TRegularArrayHelperTest.FindNthSmallestOfDyn21_4;
var
  v: Integer;
  a: TIntArray;
begin
  a := TIntHelper.CreateRandomShuffle(InOrderSrc21);
  AssertTrue(TIntHelper.FindNthSmallest(a, 14, v, @IntCmp));
  AssertTrue(v = 14);
end;

procedure TRegularArrayHelperTest.FindNthSmallestOfDyn21_5;
var
  v: Integer;
  a: TIntArray;
begin
  a := TIntHelper.CreateRandomShuffle(InOrderSrc21);
  AssertTrue(TIntHelper.FindNthSmallest(a, 1, v, @IntCmp));
  AssertTrue(v = 1);
end;

procedure TRegularArrayHelperTest.FindNthSmallestOfDyn21_6;
var
  v: Integer;
  a: TIntArray;
begin
  a := TIntHelper.CreateRandomShuffle(InOrderSrc21);
  AssertTrue(TIntHelper.FindNthSmallest(a, 19, v, @IntCmp));
  AssertTrue(v = 19);
end;

procedure TRegularArrayHelperTest.FindNthSmallestOfDyn21_7;
var
  v: Integer;
  a: TIntArray;
begin
  a := TIntHelper.CreateRandomShuffle(InOrderDblSrc21);
  AssertTrue(TIntHelper.FindNthSmallest(a, 14, v, @IntCmp));
  AssertTrue(v = 7);
end;

procedure TRegularArrayHelperTest.FindNthSmallestOfDyn21_8;
var
  v: Integer;
  a: TIntArray;
begin
  a := TIntHelper.CreateRandomShuffle(InOrderDblSrc21);
  AssertTrue(TIntHelper.FindNthSmallest(a, 15, v, @IntCmp));
  AssertTrue(v = 7);
end;

procedure TRegularArrayHelperTest.NthSmallestOfEmpty;
var
  a: TIntArray;
begin
  AssertFalse(TIntHelper.NthSmallest(a{%H-}, 1, @IntCmp).Assigned);
end;

procedure TRegularArrayHelperTest.NthSmallestOfStatic1;
begin
  AssertTrue(TIntHelper.NthSmallest(InOrderSrc21[3..3], 0, @IntCmp).Value = 3);
end;

procedure TRegularArrayHelperTest.NthSmallestOfStatic1_1;
begin
  AssertTrue(TIntHelper.NthSmallest(InOrderSrc21[5..5], -1, @IntCmp).Value = 5);
end;

procedure TRegularArrayHelperTest.NthSmallestOfStatic1_2;
begin
  AssertTrue(TIntHelper.NthSmallest(InOrderSrc21[2..2], 2, @IntCmp).Value = 2);
end;

procedure TRegularArrayHelperTest.NthSmallestOfStatic21;
var
  a: TIntArray21;
begin
  a := InOrderSrc21;
  TIntHelper.RandomShuffle(a);
  AssertTrue(TIntHelper.NthSmallest(a, 0, @IntCmp).Value = 0);
end;

procedure TRegularArrayHelperTest.NthSmallestOfStatic21_1;
var
  a: TIntArray21;
begin
  a := InOrderSrc21;
  TIntHelper.RandomShuffle(a);
  AssertTrue(TIntHelper.NthSmallest(a, -15, @IntCmp).Value = 0);
end;

procedure TRegularArrayHelperTest.NthSmallestOfStatic21_2;
var
  a: TIntArray21;
begin
  a := InOrderSrc21;
  TIntHelper.RandomShuffle(a);
  AssertTrue(TIntHelper.NthSmallest(a, 20, @IntCmp).Value = 20);
end;

procedure TRegularArrayHelperTest.NthSmallestOfStatic21_3;
var
  a: TIntArray21;
begin
  a := InOrderSrc21;
  TIntHelper.RandomShuffle(a);
  AssertTrue(TIntHelper.NthSmallest(a, 22, @IntCmp).Value = 20);
end;

procedure TRegularArrayHelperTest.NthSmallestOfStatic21_4;
var
  a: TIntArray21;
begin
  a := InOrderSrc21;
  TIntHelper.RandomShuffle(a);
  AssertTrue(TIntHelper.NthSmallest(a, 11, @IntCmp).Value = 11);
end;

procedure TRegularArrayHelperTest.NthSmallestOfStatic21_5;
var
  a: TIntArray21;
begin
  a := InOrderSrc21;
  TIntHelper.RandomShuffle(a);
  AssertTrue(TIntHelper.NthSmallest(a, 1, @IntCmp).Value = 1);
end;

procedure TRegularArrayHelperTest.NthSmallestOfStatic21_6;
var
  a: TIntArray21;
begin
  a := InOrderSrc21;
  TIntHelper.RandomShuffle(a);
  AssertTrue(TIntHelper.NthSmallest(a, 19, @IntCmp).Value = 19);
end;

procedure TRegularArrayHelperTest.NthSmallestOfStatic21_7;
var
  a: TIntArray21;
begin
  a := InOrderDblSrc21;
  TIntHelper.RandomShuffle(a);
  AssertTrue(TIntHelper.NthSmallest(a, 10, @IntCmp).Value = 5);
end;

procedure TRegularArrayHelperTest.NthSmallestOfStatic21_8;
var
  a: TIntArray21;
begin
  a := InOrderDblSrc21;
  TIntHelper.RandomShuffle(a);
  AssertTrue(TIntHelper.NthSmallest(a, 11, @IntCmp).Value = 5);
end;

procedure TRegularArrayHelperTest.NthSmallestOfDyn1;
begin
  AssertTrue(TIntHelper.NthSmallest(TIntHelper.CreateCopy(InOrderSrc21[6..6]), 0, @IntCmp).Value = 6);
end;

procedure TRegularArrayHelperTest.NthSmallestOfDyn1_1;
begin
  AssertTrue(TIntHelper.NthSmallest(TIntHelper.CreateCopy(InOrderSrc21[6..6]), -3, @IntCmp).Value = 6);
end;

procedure TRegularArrayHelperTest.NthSmallestOfDyn1_2;
begin
  AssertTrue(TIntHelper.NthSmallest(TIntHelper.CreateCopy(InOrderSrc21[6..6]), 4, @IntCmp).Value = 6);
end;

procedure TRegularArrayHelperTest.NthSmallestOfDyn21;
var
  a: TIntArray;
begin
  a := TIntHelper.CreateRandomShuffle(InOrderSrc21);
  AssertTrue(TIntHelper.NthSmallest(a, 0, @IntCmp).Value = 0);
end;

procedure TRegularArrayHelperTest.NthSmallestOfDyn21_1;
var
  a: TIntArray;
begin
  a := TIntHelper.CreateRandomShuffle(InOrderSrc21);
  AssertTrue(TIntHelper.NthSmallest(a, -3, @IntCmp).Value = 0);
end;

procedure TRegularArrayHelperTest.NthSmallestOfDyn21_2;
var
  a: TIntArray;
begin
  a := TIntHelper.CreateRandomShuffle(InOrderSrc21);
  AssertTrue(TIntHelper.NthSmallest(a, 20, @IntCmp).Value = 20);
end;

procedure TRegularArrayHelperTest.NthSmallestOfDyn21_3;
var
  a: TIntArray;
begin
  a := TIntHelper.CreateRandomShuffle(InOrderSrc21);
  AssertTrue(TIntHelper.NthSmallest(a, 23, @IntCmp).Value = 20);
end;

procedure TRegularArrayHelperTest.NthSmallestOfDyn21_4;
var
  a: TIntArray;
begin
  a := TIntHelper.CreateRandomShuffle(InOrderSrc21);
  AssertTrue(TIntHelper.NthSmallest(a, 14, @IntCmp).Value = 14);
end;

procedure TRegularArrayHelperTest.NthSmallestOfDyn21_5;
var
  a: TIntArray;
begin
  a := TIntHelper.CreateRandomShuffle(InOrderSrc21);
  AssertTrue(TIntHelper.NthSmallest(a, 1, @IntCmp).Value = 1);
end;

procedure TRegularArrayHelperTest.NthSmallestOfDyn21_6;
var
  a: TIntArray;
begin
  a := TIntHelper.CreateRandomShuffle(InOrderSrc21);
  AssertTrue(TIntHelper.NthSmallest(a, 19, @IntCmp).Value = 19);
end;

procedure TRegularArrayHelperTest.NthSmallestOfDyn21_7;
var
  a: TIntArray;
begin
  a := TIntHelper.CreateRandomShuffle(InOrderDblSrc21);
  AssertTrue(TIntHelper.NthSmallest(a, 14, @IntCmp).Value = 7);
end;

procedure TRegularArrayHelperTest.NthSmallestOfDyn21_8;
var
  a: TIntArray;
begin
  a := TIntHelper.CreateRandomShuffle(InOrderDblSrc21);
  AssertTrue(TIntHelper.NthSmallest(a, 15, @IntCmp).Value = 7);
end;

procedure TRegularArrayHelperTest.FindNthSmallestNDOfStatic21;
var
  a, b: TIntArray21;
  v: Integer;
begin
  a := InOrderSrc21;
  TIntHelper.RandomShuffle(a);
  b := a;
  AssertTrue(TIntHelper.FindNthSmallestND(a, 8, v, @IntCmp));
  AssertTrue(v = 8);
  AssertTrue(TIntHelper.Same(a, b, @IntCmp));
end;

procedure TRegularArrayHelperTest.FindNthSmallestNDOfStatic21_1;
var
  a, b: TIntArray21;
  v: Integer;
begin
  a := InOrderSrc21;
  TIntHelper.RandomShuffle(a);
  b := a;
  AssertTrue(TIntHelper.FindNthSmallestND(a, 11, v, @IntCmp));
  AssertTrue(v = 11);
  AssertTrue(TIntHelper.Same(a, b, @IntCmp));
end;

procedure TRegularArrayHelperTest.FindNthSmallestNDOfDyn21;
var
  a, b: TIntArray;
  v: Integer;
begin
  a := TIntHelper.CreateRandomShuffle(InOrderSrc21);
  b := TIntHelper.CreateCopy(a);
  AssertTrue(TIntHelper.FindNthSmallestND(a, 7, v, @IntCmp));
  AssertTrue(v = 7);
  AssertTrue(TIntHelper.Same(a, b, @IntCmp));
end;

procedure TRegularArrayHelperTest.FindNthSmallestNDOfDyn21_1;
var
  a, b: TIntArray;
  v: Integer;
begin
  a := TIntHelper.CreateRandomShuffle(InOrderSrc21);
  b := TIntHelper.CreateCopy(a);
  AssertTrue(TIntHelper.FindNthSmallestND(a, 12, v, @IntCmp));
  AssertTrue(v = 12);
  AssertTrue(TIntHelper.Same(a, b, @IntCmp));
end;

procedure TRegularArrayHelperTest.NthSmallestNDOfStatic21;
var
  a, b: TIntArray21;
begin
  a := InOrderSrc21;
  TIntHelper.RandomShuffle(a);
  b := a;
  AssertTrue(TIntHelper.NthSmallestND(a, 9, @IntCmp).Value = 9);
  AssertTrue(TIntHelper.Same(a, b, @IntCmp));
end;

procedure TRegularArrayHelperTest.NthSmallestNDOfStatic21_1;
var
  a, b: TIntArray21;
begin
  a := InOrderSrc21;
  TIntHelper.RandomShuffle(a);
  b := a;
  AssertTrue(TIntHelper.NthSmallestND(a, 13, @IntCmp).Value = 13);
  AssertTrue(TIntHelper.Same(a, b, @IntCmp));
end;

procedure TRegularArrayHelperTest.NthSmallestNDOfDyn21;
var
  a, b: TIntArray;
begin
  a := TIntHelper.CreateRandomShuffle(InOrderSrc21);
  b := TIntHelper.CreateCopy(a);
  AssertTrue(TIntHelper.NthSmallestND(a, 5, @IntCmp).Value = 5);
  AssertTrue(TIntHelper.Same(a, b, @IntCmp));
end;

procedure TRegularArrayHelperTest.NthSmallestNDOfDyn21_1;
var
  a, b: TIntArray;
begin
  a := TIntHelper.CreateRandomShuffle(InOrderSrc21);
  b := TIntHelper.CreateCopy(a);
  AssertTrue(TIntHelper.NthSmallestND(a, 15, @IntCmp).Value = 15);
  AssertTrue(TIntHelper.Same(a, b, @IntCmp));
end;

procedure TRegularArrayHelperTest.NextPermutation2AscOfEmpty;
var
  a: TIntArray;
begin
  AssertFalse(TIntHelper.NextPermutation2Asc(a{%H-}, @IntCmp));
end;

procedure TRegularArrayHelperTest.NextPermutation2AscOfStatic1;
var
  a: array[0..0] of Integer = (10);
begin
  AssertFalse(TIntHelper.NextPermutation2Asc(a, @IntCmp));
end;

procedure TRegularArrayHelperTest.NextPermutation2AscOfStatic2Asc;
var
  a: array[0..1] of Integer = (5, 8);
begin
  AssertFalse(TIntHelper.NextPermutation2Asc(a, @IntCmp));
end;

procedure TRegularArrayHelperTest.NextPermutation2AscOfStatic2Desc;
const
  c: array[0..1] of Integer = (3, 11);
var
  a: array[0..1] of Integer = (11, 3);
begin
  AssertTrue(TIntHelper.NextPermutation2Asc(a, @IntCmp));
  AssertTrue(TIntHelper.Same(a, c, @IntCmp));
end;

procedure TRegularArrayHelperTest.NextPermutation2AscOfStatic10Equal;
var
  a: TIntArray10;
begin
  a := IntEqual10;
  AssertFalse(TIntHelper.NextPermutation2Asc(a, @IntCmp));
end;

procedure TRegularArrayHelperTest.NextPermutation2AscOfStatic3;
type
  TInt3 = array[1..3] of Integer;
const
  c1: TInt3 = (2, 1, 0);
  c2: TInt3 = (2, 0, 1);
  c3: TInt3 = (1, 2, 0);
  c4: TInt3 = (1, 0, 2);
  c5: TInt3 = (0, 2, 1);
  c6: TInt3 = (0, 1, 2);
var
  a: TInt3;
begin
  a := c1;
  AssertTrue(TIntHelper.NextPermutation2Asc(a, @IntCmp));
  AssertTrue(TIntHelper.Same(a, c2, @IntCmp));
  AssertTrue(TIntHelper.NextPermutation2Asc(a, @IntCmp));
  AssertTrue(TIntHelper.Same(a, c3, @IntCmp));
  AssertTrue(TIntHelper.NextPermutation2Asc(a, @IntCmp));
  AssertTrue(TIntHelper.Same(a, c4, @IntCmp));
  AssertTrue(TIntHelper.NextPermutation2Asc(a, @IntCmp));
  AssertTrue(TIntHelper.Same(a, c5, @IntCmp));
  AssertTrue(TIntHelper.NextPermutation2Asc(a, @IntCmp));
  AssertTrue(TIntHelper.Same(a, c6, @IntCmp));
  AssertFalse(TIntHelper.NextPermutation2Asc(a, @IntCmp));
end;

procedure TRegularArrayHelperTest.NextPermutation2AscOfStatic4;
type
  TInt4 = array[0..3] of Integer;
const
  c0: TInt4 = (3, 2, 1, 0);
  c1: TInt4 = (0, 1, 2, 3);
var
  a: TInt4;
  PermCount: SizeInt = 1;
begin
  a := c0;
  while TIntHelper.NextPermutation2Asc(a, @IntCmp) do
    Inc(PermCount);
  AssertTrue(TIntHelper.Same(a, c1, @IntCmp));
  AssertTrue(PermCount = 24);
end;

procedure TRegularArrayHelperTest.NextPermutation2AscOfDyn1;
begin
  AssertFalse(TIntHelper.NextPermutation2Asc(TIntArray.Create(25), @IntCmp));
end;

procedure TRegularArrayHelperTest.NextPermutation2AscOfDyn2Asc;
begin
  AssertFalse(TIntHelper.NextPermutation2Asc(TIntArray.Create(13, 17), @IntCmp));
end;

procedure TRegularArrayHelperTest.NextPermutation2AscOfDyn2Desc;
begin
  AssertTrue(TIntHelper.NextPermutation2Asc(TIntArray.Create(21, 11), @IntCmp));
end;

procedure TRegularArrayHelperTest.NextPermutation2AscOfDyn10Equal;
begin
  AssertFalse(TIntHelper.NextPermutation2Asc(TIntHelper.CreateCopy(IntEqual10), @IntCmp));
end;

procedure TRegularArrayHelperTest.NextPermutation2AscOfDyn3;
type
  TInt3 = array[1..3] of Integer;
const
  c1: TInt3 = (2, 1, 0);
  c2: TInt3 = (2, 0, 1);
  c3: TInt3 = (1, 2, 0);
  c4: TInt3 = (1, 0, 2);
  c5: TInt3 = (0, 2, 1);
  c6: TInt3 = (0, 1, 2);
var
  a: TIntArray;
begin
  a := TIntHelper.CreateCopy(c1);
  AssertTrue(TIntHelper.NextPermutation2Asc(a, @IntCmp));
  AssertTrue(TIntHelper.Same(a, c2, @IntCmp));
  AssertTrue(TIntHelper.NextPermutation2Asc(a, @IntCmp));
  AssertTrue(TIntHelper.Same(a, c3, @IntCmp));
  AssertTrue(TIntHelper.NextPermutation2Asc(a, @IntCmp));
  AssertTrue(TIntHelper.Same(a, c4, @IntCmp));
  AssertTrue(TIntHelper.NextPermutation2Asc(a, @IntCmp));
  AssertTrue(TIntHelper.Same(a, c5, @IntCmp));
  AssertTrue(TIntHelper.NextPermutation2Asc(a, @IntCmp));
  AssertTrue(TIntHelper.Same(a, c6, @IntCmp));
  AssertFalse(TIntHelper.NextPermutation2Asc(a, @IntCmp));
end;

procedure TRegularArrayHelperTest.NextPermutation2AscOfDyn4;
type
  TInt4 = array[0..3] of Integer;
const
  c0: TInt4 = (3, 2, 1, 0);
  c1: TInt4 = (0, 1, 2, 3);
var
  a: TIntArray;
  PermCount: SizeInt = 1;
begin
  a := TIntHelper.CreateCopy(c0);
  while TIntHelper.NextPermutation2Asc(a, @IntCmp) do
    Inc(PermCount);
  AssertTrue(TIntHelper.Same(a, c1, @IntCmp));
  AssertTrue(PermCount = 24);
end;

procedure TRegularArrayHelperTest.NextPermutation2DescOfEmpty;
var
  a: TIntArray;
begin
  AssertFalse(TIntHelper.NextPermutation2Desc(a{%H-}, @IntCmp));
end;

procedure TRegularArrayHelperTest.NextPermutation2DescOfStatic1;
var
  a: array[0..0] of Integer = (10);
begin
  AssertFalse(TIntHelper.NextPermutation2Desc(a, @IntCmp));
end;

procedure TRegularArrayHelperTest.NextPermutation2DescOfStatic2Asc;
const
  c: array[0..1] of Integer = (8, 5);
var
  a: array[0..1] of Integer = (5, 8);
begin
  AssertTrue(TIntHelper.NextPermutation2Desc(a, @IntCmp));
  AssertTrue(TIntHelper.Same(a, c, @IntCmp));
end;

procedure TRegularArrayHelperTest.NextPermutation2DescOfStatic2Desc;
var
  a: array[0..1] of Integer = (11, 3);
begin
  AssertFalse(TIntHelper.NextPermutation2Desc(a, @IntCmp));
end;

procedure TRegularArrayHelperTest.NextPermutation2DescOfStatic10Equal;
var
  a: TIntArray10;
begin
  a := IntEqual10;
  AssertFalse(TIntHelper.NextPermutation2Desc(a, @IntCmp));
end;

procedure TRegularArrayHelperTest.NextPermutation2DescOfStatic3;
type
  TInt3 = array[1..3] of Integer;
const
  c1: TInt3 = (0, 1, 2);
  c2: TInt3 = (0, 2, 1);
  c3: TInt3 = (1, 0, 2);
  c4: TInt3 = (1, 2, 0);
  c5: TInt3 = (2, 0, 1);
  c6: TInt3 = (2, 1, 0);
var
  a: TInt3;
begin
  a := c1;
  AssertTrue(TIntHelper.NextPermutation2Desc(a, @IntCmp));
  AssertTrue(TIntHelper.Same(a, c2, @IntCmp));
  AssertTrue(TIntHelper.NextPermutation2Desc(a, @IntCmp));
  AssertTrue(TIntHelper.Same(a, c3, @IntCmp));
  AssertTrue(TIntHelper.NextPermutation2Desc(a, @IntCmp));
  AssertTrue(TIntHelper.Same(a, c4, @IntCmp));
  AssertTrue(TIntHelper.NextPermutation2Desc(a, @IntCmp));
  AssertTrue(TIntHelper.Same(a, c5, @IntCmp));
  AssertTrue(TIntHelper.NextPermutation2Desc(a, @IntCmp));
  AssertTrue(TIntHelper.Same(a, c6, @IntCmp));
  AssertFalse(TIntHelper.NextPermutation2Desc(a, @IntCmp));
end;

procedure TRegularArrayHelperTest.NextPermutation2DescOfStatic4;
type
  TInt4 = array[0..3] of Integer;
const
  c0: TInt4 = (0, 1, 2, 3);
  c1: TInt4 = (3, 2, 1, 0);
var
  a: TInt4;
  PermCount: SizeInt = 1;
begin
  a := c0;
  while TIntHelper.NextPermutation2Desc(a, @IntCmp) do
    Inc(PermCount);
  AssertTrue(TIntHelper.Same(a, c1, @IntCmp));
  AssertTrue(PermCount = 24);
end;

procedure TRegularArrayHelperTest.NextPermutation2DescOfDyn1;
begin
  AssertFalse(TIntHelper.NextPermutation2Desc(TIntArray.Create(10), @IntCmp));
end;

procedure TRegularArrayHelperTest.NextPermutation2DescOfDyn2Asc;
const
  c: array[0..1] of Integer = (8, 5);
var
  a: TIntArray;
begin
  a := TIntArray.Create(5, 8);
  AssertTrue(TIntHelper.NextPermutation2Desc(a, @IntCmp));
  AssertTrue(TIntHelper.Same(a, c, @IntCmp));
end;

procedure TRegularArrayHelperTest.NextPermutation2DescOfDyn2Desc;
begin
  AssertFalse(TIntHelper.NextPermutation2Desc(TIntArray.Create(11, 3), @IntCmp));
end;

procedure TRegularArrayHelperTest.NextPermutation2DescOfDyn10Equal;
begin
  AssertFalse(TIntHelper.NextPermutation2Desc(TIntHelper.CreateCopy(IntEqual10), @IntCmp));
end;

procedure TRegularArrayHelperTest.NextPermutation2DescOfDyn3;
type
  TInt3 = array[1..3] of Integer;
const
  c1: TInt3 = (0, 1, 2);
  c2: TInt3 = (0, 2, 1);
  c3: TInt3 = (1, 0, 2);
  c4: TInt3 = (1, 2, 0);
  c5: TInt3 = (2, 0, 1);
  c6: TInt3 = (2, 1, 0);
var
  a: TIntArray;
begin
  a := TIntHelper.CreateCopy(c1);
  AssertTrue(TIntHelper.NextPermutation2Desc(a, @IntCmp));
  AssertTrue(TIntHelper.Same(a, c2, @IntCmp));
  AssertTrue(TIntHelper.NextPermutation2Desc(a, @IntCmp));
  AssertTrue(TIntHelper.Same(a, c3, @IntCmp));
  AssertTrue(TIntHelper.NextPermutation2Desc(a, @IntCmp));
  AssertTrue(TIntHelper.Same(a, c4, @IntCmp));
  AssertTrue(TIntHelper.NextPermutation2Desc(a, @IntCmp));
  AssertTrue(TIntHelper.Same(a, c5, @IntCmp));
  AssertTrue(TIntHelper.NextPermutation2Desc(a, @IntCmp));
  AssertTrue(TIntHelper.Same(a, c6, @IntCmp));
  AssertFalse(TIntHelper.NextPermutation2Desc(a, @IntCmp));
end;

procedure TRegularArrayHelperTest.NextPermutation2DescOfDyn4;
type
  TInt4 = array[0..3] of Integer;
const
  c0: TInt4 = (0, 1, 2, 3);
  c1: TInt4 = (3, 2, 1, 0);
var
  a: TIntArray;
  PermCount: SizeInt = 1;
begin
  a := TIntHelper.CreateCopy(c0);
  while TIntHelper.NextPermutation2Desc(a, @IntCmp) do
    Inc(PermCount);
  AssertTrue(TIntHelper.Same(a, c1, @IntCmp));
  AssertTrue(PermCount = 24);
end;

procedure TRegularArrayHelperTest.IsNonDescendingOfEmpty;
var
  a: TIntArray;
begin
  AssertTrue(TIntHelper.IsNonDescending(a{%H-}, @IntCmp));
end;

procedure TRegularArrayHelperTest.IsNonDescendingOfStatic1;
begin
  AssertTrue(TIntHelper.IsNonDescending(IntSrc10[4..4], @IntCmp));
end;

procedure TRegularArrayHelperTest.IsNonDescendingOfStatic10Equal;
begin
  AssertTrue(TIntHelper.IsNonDescending(IntEqual10, @IntCmp));
end;

procedure TRegularArrayHelperTest.IsNonDescendingOfStatic21Success;
begin
  AssertTrue(TIntHelper.IsNonDescending(InOrderSrc21, @IntCmp));
end;

procedure TRegularArrayHelperTest.IsNonDescendingOfStatic21Success_1;
begin
  AssertTrue(TIntHelper.IsNonDescending(InOrderDblSrc21, @IntCmp));
end;

procedure TRegularArrayHelperTest.IsNonDescendingOfStatic21Unsuccess;
var
  a: TIntArray21;
begin
  a := InOrderSrc21;
  TIntHelper.Reverse(a[4..5]);
  AssertFalse(TIntHelper.IsNonDescending(a, @IntCmp));
end;

procedure TRegularArrayHelperTest.IsNonDescendingOfStatic21Unsuccess_1;
var
  a: TIntArray21;
begin
  a := InOrderDblSrc21;
  TIntHelper.Reverse(a[5..7]);
  AssertFalse(TIntHelper.IsNonDescending(a, @IntCmp));
end;

procedure TRegularArrayHelperTest.IsNonDescendingOfDyn1;
begin
  AssertTrue(TIntHelper.IsNonDescending(TIntArray.Create(111), @IntCmp));
end;

procedure TRegularArrayHelperTest.IsNonDescendingOfDyn10Equal;
begin
  AssertTrue(TIntHelper.IsNonDescending(TIntHelper.CreateCopy(IntEqual10), @IntCmp));
end;

procedure TRegularArrayHelperTest.IsNonDescendingOfDyn21Success;
begin
  AssertTrue(TIntHelper.IsNonDescending(TIntHelper.CreateCopy(InOrderSrc21), @IntCmp));
end;

procedure TRegularArrayHelperTest.IsNonDescendingOfDyn21Success_1;
begin
  AssertTrue(TIntHelper.IsNonDescending(TIntHelper.CreateCopy(InOrderDblSrc21), @IntCmp));
end;

procedure TRegularArrayHelperTest.IsNonDescendingOfDyn21Unsuccess;
var
  a: TIntArray;
begin
  a := TIntHelper.CreateCopy(InOrderSrc21);
  TIntHelper.Reverse(a[5..6]);
  AssertFalse(TIntHelper.IsNonDescending(a, @IntCmp));
end;

procedure TRegularArrayHelperTest.IsNonDescendingOfDyn21Unsuccess_1;
var
  a: TIntArray;
begin
  a := TIntHelper.CreateCopy(InOrderDblSrc21);
  TIntHelper.Reverse(a[6..8]);
  AssertFalse(TIntHelper.IsNonDescending(a, @IntCmp));
end;

procedure TRegularArrayHelperTest.IsStrictAscendingOfEmpty;
var
  a: TIntArray;
begin
  AssertFalse(TIntHelper.IsStrictAscending(a{%H-}, @IntCmp));
end;

procedure TRegularArrayHelperTest.IsStrictAscendingOfStatic1;
begin
  AssertFalse(TIntHelper.IsStrictAscending(IntSrc10[3..3], @IntCmp));
end;

procedure TRegularArrayHelperTest.IsStrictAscendingOfStatic10Equal;
begin
  AssertFalse(TIntHelper.IsStrictAscending(IntEqual10, @IntCmp));
end;

procedure TRegularArrayHelperTest.IsStrictAscendingOfStatic21Success;
begin
  AssertTrue(TIntHelper.IsStrictAscending(InOrderSrc21, @IntCmp));
end;

procedure TRegularArrayHelperTest.IsStrictAscendingOfStatic21Unsuccess;
begin
  AssertFalse(TIntHelper.IsStrictAscending(InOrderDblSrc21, @IntCmp));
end;

procedure TRegularArrayHelperTest.IsStrictAscendingOfDyn1;
begin
  AssertFalse(TIntHelper.IsStrictAscending(TIntArray.Create(14), @IntCmp));
end;

procedure TRegularArrayHelperTest.IsStrictAscendingOfDyn10Equal;
begin
  AssertFalse(TIntHelper.IsStrictAscending(TIntHelper.CreateCopy(IntEqual10), @IntCmp));
end;

procedure TRegularArrayHelperTest.IsStrictAscendingOfDyn21Success;
begin
   AssertTrue(TIntHelper.IsStrictAscending(TIntHelper.CreateCopy(InOrderSrc21), @IntCmp));
end;

procedure TRegularArrayHelperTest.IsStrictAscendingOfDyn21Unsuccess;
begin
  AssertFalse(TIntHelper.IsStrictAscending(TIntHelper.CreateCopy(InOrderDblSrc21), @IntCmp));
end;

procedure TRegularArrayHelperTest.IsNonAscendingOfEmpty;
var
  a: TIntArray;
begin
  AssertTrue(TIntHelper.IsNonAscending(a{%H-}, @IntCmp));
end;

procedure TRegularArrayHelperTest.IsNonAscendingOfStatic1;
begin
  AssertTrue(TIntHelper.IsNonAscending(IntSrc10[9..9], @IntCmp));
end;

procedure TRegularArrayHelperTest.IsNonAscendingOfStatic10Equal;
begin
  AssertTrue(TIntHelper.IsNonAscending(IntEqual10, @IntCmp));
end;

procedure TRegularArrayHelperTest.IsNonAscendingOfStatic21Success;
var
  a: TIntArray21;
begin
  a := InOrderSrc21;
  TIntHelper.Reverse(a);
  AssertTrue(TIntHelper.IsNonAscending(a, @IntCmp));
end;

procedure TRegularArrayHelperTest.IsNonAscendingOfStatic21Success_1;
var
  a: TIntArray21;
begin
  a := InOrderDblSrc21;
  TIntHelper.Reverse(a);
  AssertTrue(TIntHelper.IsNonAscending(a, @IntCmp));
end;

procedure TRegularArrayHelperTest.IsNonAscendingOfStatic21Unsuccess;
var
  a: TIntArray21;
begin
  a := InOrderSrc21;
  AssertFalse(TIntHelper.IsNonAscending(a, @IntCmp));
end;

procedure TRegularArrayHelperTest.IsNonAscendingOfStatic21Unsuccess_1;
var
  a: TIntArray21;
begin
  a := InOrderDblSrc21;
  AssertFalse(TIntHelper.IsNonAscending(a, @IntCmp));
end;

procedure TRegularArrayHelperTest.IsNonAscendingOfDyn1;
begin
  AssertTrue(TIntHelper.IsNonAscending(TIntArray.Create(21), @IntCmp));
end;

procedure TRegularArrayHelperTest.IsNonAscendingOfDyn10Equal;
begin
  AssertTrue(TIntHelper.IsNonAscending(TIntHelper.CreateCopy(IntEqual10), @IntCmp));
end;

procedure TRegularArrayHelperTest.IsNonAscendingOfDyn21Success;
begin
  AssertTrue(TIntHelper.IsNonAscending(TIntHelper.CreateReverseCopy(InOrderSrc21), @IntCmp));
end;

procedure TRegularArrayHelperTest.IsNonAscendingOfDyn21Success_1;
begin
  AssertTrue(TIntHelper.IsNonAscending(TIntHelper.CreateReverseCopy(InOrderDblSrc21), @IntCmp));
end;

procedure TRegularArrayHelperTest.IsNonAscendingOfDyn21Unsuccess;
begin
  AssertFalse(TIntHelper.IsNonAscending(TIntHelper.CreateCopy(InOrderSrc21), @IntCmp));
end;

procedure TRegularArrayHelperTest.IsNonAscendingOfDyn21Unsuccess_1;
begin
  AssertFalse(TIntHelper.IsNonAscending(TIntHelper.CreateCopy(InOrderDblSrc21), @IntCmp));
end;

procedure TRegularArrayHelperTest.IsStrictDescendingOfEmpty;
var
  a: TIntArray;
begin
  AssertFalse(TIntHelper.IsStrictDescending(a{%H-}, @IntCmp));
end;

procedure TRegularArrayHelperTest.IsStrictDescendingOfStatic1;
begin
  AssertFalse(TIntHelper.IsStrictDescending(IntSrc10[2..2], @IntCmp));
end;

procedure TRegularArrayHelperTest.IsStrictDescendingOfStatic10Equal;
begin
  AssertFalse(TIntHelper.IsStrictDescending(IntEqual10, @IntCmp));
end;

procedure TRegularArrayHelperTest.IsStrictDescendingOfStatic21Success;
var
  a: TIntArray21;
begin
  a := InOrderSrc21;
  TIntHelper.Reverse(a);
  AssertTrue(TIntHelper.IsStrictDescending(a, @IntCmp));
end;

procedure TRegularArrayHelperTest.IsStrictDescendingOfStatic21Unsuccess;
var
  a: TIntArray21;
begin
  a := InOrderDblSrc21;
  TIntHelper.Reverse(a);
  AssertFalse(TIntHelper.IsStrictDescending(a, @IntCmp));
end;

procedure TRegularArrayHelperTest.IsStrictDescendingOfDyn1;
begin
  AssertFalse(TIntHelper.IsStrictDescending(TIntArray.Create(37), @IntCmp));
end;

procedure TRegularArrayHelperTest.IsStrictDescendingOfDyn10Equal;
begin
  AssertFalse(TIntHelper.IsStrictDescending(TIntHelper.CreateCopy(IntEqual10), @IntCmp));
end;

procedure TRegularArrayHelperTest.IsStrictDescendingOfDyn21Success;
begin
  AssertTrue(TIntHelper.IsStrictDescending(TIntHelper.CreateReverseCopy(InOrderSrc21), @IntCmp));
end;

procedure TRegularArrayHelperTest.IsStrictDescendingOfDyn21Unsuccess;
begin
  AssertFalse(TIntHelper.IsStrictDescending(TIntHelper.CreateReverseCopy(InOrderDblSrc21), @IntCmp));
end;

procedure TRegularArrayHelperTest.InversionCount;
const
  BigSize = 1000;
var
  a: TIntArray = nil;
  I: Integer;
begin
  AssertTrue(TIntHelper.InversionCount(a, @IntCmp) = 0);
  a := [5];
  AssertTrue(TIntHelper.InversionCount(a, @IntCmp) = 0);
  a := [5, 9];
  AssertTrue(TIntHelper.InversionCount(a, @IntCmp) = 0);
  a := [9, 5];
  AssertTrue(TIntHelper.InversionCount(a, @IntCmp) = 1);
  a := [9, 5, 2];
  AssertTrue(TIntHelper.InversionCount(a, @IntCmp) = 3);
  AssertTrue(TIntHelper.IsNonDescending(a, @IntCmp));
  SetLength(a, BigSize);
  for I := 0 to High(a) do
    a[I] := I;
  AssertTrue(TIntHelper.InversionCount(a, @IntCmp) = 0);
  TIntHelper.Reverse(a);
  AssertTrue(TIntHelper.InversionCount(a, @IntCmp) = BigSize*Pred(BigSize) div 2);
  AssertTrue(TIntHelper.IsStrictAscending(a, @IntCmp));
end;

procedure TRegularArrayHelperTest.InversionCountND;
const
  BigSize = 1000;
var
  a: TIntArray = nil;
  b: TIntArray;
  I: Integer;
begin
  SetLength(a, BigSize);
  for I := 0 to High(a) do
    a[I] := BigSize - I;
  b := TIntHelper.CreateCopy(a);
  AssertTrue(TIntHelper.InversionCountND(b, @IntCmp) = BigSize*Pred(BigSize) div 2);
  AssertTrue(TIntHelper.Same(a, b, @IntCmp));
end;

procedure TRegularArrayHelperTest.Lis;
begin
  AssertTrue(TIntHelper.Lis([], @IntCmp) = nil);
  AssertTrue(TIntHelper.Lis([1], @IntCmp) = nil);
  AssertTrue(TIntHelper.Lis([1, 1], @IntCmp) = nil);
  AssertTrue(TIntHelper.Lis([2, 1], @IntCmp) = nil);
  AssertTrue(TIntHelper.Lis([5, 5, 5, 4], @IntCmp) = nil);
  AssertTrue(TIntHelper.Lis([13, 10, 8, 7, 3, 2], @IntCmp) = nil);
  AssertTrue(TSizeIntHelper.Same(TIntHelper.Lis([1, 2], @IntCmp), [0, 1]));
  AssertTrue(TSizeIntHelper.Same(TIntHelper.Lis([1, -1, 2, 4, 3, 2], @IntCmp), [1, 2, 4]));
  AssertTrue(TSizeIntHelper.Same(TIntHelper.Lis([1, -1, 2, 4, 3, 2, 2], @IntCmp), [1, 2, 4]));
  AssertTrue(TSizeIntHelper.Same(TIntHelper.Lis(
    [0, 8, 4, 12, 2, 10, 6, 14, 1, 9, 5, 13, 3, 11, 7, 15], @IntCmp),
    //0, 2, 6, 9, 11, 15
    [0, 4, 6, 9, 13, 15]));
end;

procedure TRegularArrayHelperTest.SameOfEmpty;
var
  a, b: TIntArray;
begin
  AssertTrue(TIntHelper.Same(a{%H-}, b{%H-}, @IntCmp));
end;

procedure TRegularArrayHelperTest.SameOfStatic1Success;
begin
  AssertTrue(TIntHelper.Same(IntSrc10[9..9], IntSrc7[1..1], @IntCmp));
end;

procedure TRegularArrayHelperTest.SameOfStatic1Unsuccess;
begin
  AssertFalse(TIntHelper.Same(IntSrc10[8..8], IntSrc7[3..3], @IntCmp));
end;

procedure TRegularArrayHelperTest.SameOfStatic2Success;
begin
  AssertTrue(TIntHelper.Same(InOrderSrc21[0..1], InOrderDblSrc21[1..2], @IntCmp));
end;

procedure TRegularArrayHelperTest.SameOfStatic2Unsuccess;
begin
  AssertFalse(TIntHelper.Same(InOrderSrc21[1..2], InOrderDblSrc21[1..2], @IntCmp));
end;

procedure TRegularArrayHelperTest.SameOfStatic21Success;
var
  a: TIntArray21;
begin
  a := InOrderSrc21;
  AssertTrue(TIntHelper.Same(InOrderSrc21, a, @IntCmp));
end;

procedure TRegularArrayHelperTest.SameOfStatic21Unsuccess;
var
  a: TIntArray21;
begin
  a := InOrderSrc21;
  AssertFalse(TIntHelper.Same(InOrderSrc21[1..20], a, @IntCmp));
end;

procedure TRegularArrayHelperTest.SameOfDyn1Success;
begin
  AssertTrue(TIntHelper.Same(TIntArray.Create(12), TIntArray.Create(12), @IntCmp));
end;

procedure TRegularArrayHelperTest.SameOfDyn1Unsuccess;
begin
  AssertFalse(TIntHelper.Same(TIntArray.Create(14), TIntArray.Create(15), @IntCmp));
end;

procedure TRegularArrayHelperTest.SameOfDyn2Success;
begin
  AssertTrue(TIntHelper.Same(TIntArray.Create(11, 12), TIntArray.Create(11, 12), @IntCmp));
end;

procedure TRegularArrayHelperTest.SameOfDyn2Unsuccess;
begin
  AssertFalse(TIntHelper.Same(TIntArray.Create(11, 14), TIntArray.Create(11, 15), @IntCmp));
end;

procedure TRegularArrayHelperTest.SameOfDyn21Success;
var
  a, b: TIntArray;
begin
  a := TIntHelper.CreateCopy(InOrderSrc21);
  b := TIntHelper.CreateCopy(InOrderSrc21);
  AssertTrue(TIntHelper.Same(a, b, @IntCmp));
end;

procedure TRegularArrayHelperTest.SameOfDyn21Unsuccess; //IntSorted10Uniq
var
  a, b: TIntArray;
begin
  a := TIntHelper.CreateCopy(InOrderSrc21);
  TIntHelper.Reverse(a[2..3]);
  b := TIntHelper.CreateCopy(InOrderSrc21);
  AssertFalse(TIntHelper.Same(a, b, @IntCmp));
end;

procedure TRegularArrayHelperTest.QuickSortOfEmpty;
var
  a: TIntArray;
begin
  //should not crash
  TIntHelper.QuickSort(a{%H-}, @IntCmp);
  AssertTrue(a = nil);
end;

procedure TRegularArrayHelperTest.QuickSortAscOfStatic3Asc;
const
  c: array[1..3] of Integer = (9, 13, 13);
var
  a: array[0..2] of Integer = (9, 13, 13);
begin
  TIntHelper.QuickSort(a, @IntCmp);
  AssertTrue(TIntHelper.Same(a, c, @IntCmp));
end;

procedure TRegularArrayHelperTest.QuickSortAscOfStatic3Desc;
const
  c: array[1..3] of Integer = (9, 13, 13);
var
  a: array[0..2] of Integer = (13, 13, 9);
begin
  TIntHelper.QuickSort(a, @IntCmp);
  AssertTrue(TIntHelper.Same(a, c, @IntCmp));
end;

procedure TRegularArrayHelperTest.QuickSortDescOfStatic3Desc;
const
  c: array[1..3] of Integer = (27, 11, 5);
var
  a: array[0..2] of Integer = (27, 11, 5);
begin
  TIntHelper.QuickSort(a, @IntCmp, soDesc);
  AssertTrue(TIntHelper.Same(a, c, @IntCmp));
end;

procedure TRegularArrayHelperTest.QuickSortDescOfStatic3Asc;
const
  c: array[1..3] of Integer = (27, 11, 5);
var
  a: array[0..2] of Integer = (11, 5, 27);
begin
  TIntHelper.QuickSort(a, @IntCmp, soDesc);
  AssertTrue(TIntHelper.Same(a, c, @IntCmp));
end;

procedure TRegularArrayHelperTest.QuickSortAscOfStatic377;
var
  I: Integer;
  a, b: array[1..377] of Integer;
begin
  for I := 1 to 377 do
    b[I] := I;
  a := b;
  TIntHelper.RandomShuffle(a);
  TIntHelper.QuickSort(a, @IntCmp);
  AssertTrue(TIntHelper.Same(a, b, @IntCmp));
end;

procedure TRegularArrayHelperTest.QuickSortDescOfStatic377;
var
  I: Integer;
  a, b: array[1..377] of Integer;
begin
  for I := 1 to 377 do
    b[I] := 378 - I;
  a := b;
  TIntHelper.RandomShuffle(a);
  TIntHelper.QuickSort(a, @IntCmp, soDesc);
  AssertTrue(TIntHelper.Same(a, b, @IntCmp));
end;

procedure TRegularArrayHelperTest.QuickSortAscOfStatic377Random;
var
  I: Integer;
  a: array[1..377] of Integer;
begin
  for I := 1 to 377 do
    a[I] := Random(Integer.MaxValue);
  TIntHelper.QuickSort(a, @IntCmp);
  AssertTrue(TIntHelper.IsNonDescending(a, @IntCmp));
end;

procedure TRegularArrayHelperTest.QuickSortAscOfStatic377RandomZeroes;
var
  I: Integer;
  a: array[1..377] of Integer;
begin
  for I := 1 to 377 do
    a[I] := Random(2);
  TIntHelper.QuickSort(a, @IntCmp);
  AssertTrue(TIntHelper.IsNonDescending(a, @IntCmp));
end;

procedure TRegularArrayHelperTest.QuickSortDescOfStatic377Random;
var
  I: Integer;
  a: array[1..377] of Integer;
begin
  for I := 1 to 377 do
    a[I] := Random(Integer.MaxValue);
  TIntHelper.QuickSort(a, @IntCmp, soDesc);
  AssertTrue(TIntHelper.IsNonAscending(a, @IntCmp));
end;

procedure TRegularArrayHelperTest.QuickSortDescOfStatic377RandomZeroes;
var
  I: Integer;
  a: array[1..377] of Integer;
begin
  for I := 1 to 377 do
    a[I] := Random(2);
  TIntHelper.QuickSort(a, @IntCmp, soDesc);
  AssertTrue(TIntHelper.IsNonAscending(a, @IntCmp));
end;

procedure TRegularArrayHelperTest.QuickSortAscOfDyn3Asc;
const
  c: array[1..3] of Integer = (11, 19, 22);
var
  a: TIntArray;
begin
  a := TIntArray.Create(11, 19, 22);
  TIntHelper.QuickSort(a, @IntCmp);
  AssertTrue(TIntHelper.Same(a, c, @IntCmp));
end;

procedure TRegularArrayHelperTest.QuickSortAscOfDyn3Desc;
const
  c: array[1..3] of Integer = (11, 19, 22);
var
  a: TIntArray;
begin
  a := TIntArray.Create(22, 11, 19);
  TIntHelper.QuickSort(a, @IntCmp);
  AssertTrue(TIntHelper.Same(a, c, @IntCmp));
end;

procedure TRegularArrayHelperTest.QuickSortDescOfDyn3Desc;
const
  c: array[1..3] of Integer = (31, 14, 3);
var
  a: TIntArray;
begin
  a := TIntArray.Create(31, 14, 3);
  TIntHelper.QuickSort(a, @IntCmp, soDesc);
  AssertTrue(TIntHelper.Same(a, c, @IntCmp));
end;

procedure TRegularArrayHelperTest.QuickSortDescOfDyn3Asc;
const
  c: array[1..3] of Integer = (31, 14, 3);
var
  a: TIntArray;
begin
  a := TIntArray.Create(14, 3, 31);
  TIntHelper.QuickSort(a, @IntCmp, soDesc);
  AssertTrue(TIntHelper.Same(a, c, @IntCmp));
end;

procedure TRegularArrayHelperTest.QuickSortAscOfDyn377;
var
  I: Integer;
  b: array[1..377] of Integer;
  a: TIntArray;
begin
  for I := 1 to 377 do
    b[I] := I;
  a := TIntHelper.CreateRandomShuffle(b);
  TIntHelper.QuickSort(a, @IntCmp);
  AssertTrue(TIntHelper.Same(a, b, @IntCmp));
end;

procedure TRegularArrayHelperTest.QuickSortDescOfDyn377;
var
  I: Integer;
  b: array[1..377] of Integer;
  a: TIntArray;
begin
  for I := 1 to 377 do
    b[I] := 378 - I;
  a := TIntHelper.CreateRandomShuffle(b);
  TIntHelper.QuickSort(a, @IntCmp, soDesc);
  AssertTrue(TIntHelper.Same(a, b, @IntCmp));
end;

procedure TRegularArrayHelperTest.QuickSortAscOfDyn377Random;
var
  I: Integer;
  a: TIntArray;
begin
  System.SetLength(a, 377);
  for I := 0 to System.High(a) do
    a[I] := Random(Integer.MaxValue);
  TIntHelper.QuickSort(a, @IntCmp);
  AssertTrue(TIntHelper.IsNonDescending(a, @IntCmp));
end;

procedure TRegularArrayHelperTest.QuickSortAscOfDyn377RandomZeroes;
var
  I: Integer;
  a: TIntArray;
begin
  System.SetLength(a, 377);
  for I := 0 to System.High(a) do
    a[I] := Random(2);
  TIntHelper.QuickSort(a, @IntCmp);
  AssertTrue(TIntHelper.IsNonDescending(a, @IntCmp));
end;

procedure TRegularArrayHelperTest.QuickSortDescOfDyn377Random;
var
  I: Integer;
  a: TIntArray;
begin
  System.SetLength(a, 377);
  for I := 0 to System.High(a) do
    a[I] := Random(Integer.MaxValue);
  TIntHelper.QuickSort(a, @IntCmp, soDesc);
  AssertTrue(TIntHelper.IsNonAscending(a, @IntCmp));
end;

procedure TRegularArrayHelperTest.QuickSortDescOfDyn377RandomZeroes;
var
  I: Integer;
  a: TIntArray;
begin
  System.SetLength(a, 377);
  for I := 0 to System.High(a) do
    a[I] := Random(2);
  TIntHelper.QuickSort(a, @IntCmp, soDesc);
  AssertTrue(TIntHelper.IsNonAscending(a, @IntCmp));
end;

procedure TRegularArrayHelperTest.HeapSortOfStatic277;
var
  I: Integer;
  a, b: array[1..277] of Integer;
begin
  for I := 1 to 277 do
    b[I] := I;
  a := b;
  TIntHelper.RandomShuffle(a);
  THackHelper.DoHeapSort(@a[1], 276, @IntCmp);
  AssertTrue(TIntHelper.Same(a, b, @IntCmp));
end;

procedure TRegularArrayHelperTest.HeapSortOfStatic277Random;
var
  I: Integer;
  a: array[1..277] of Integer;
begin
  for I := 1 to 277 do
    a[I] := Random(Integer.MaxValue);
  THackHelper.DoHeapSort(@a[1], 276, @IntCmp);
  AssertTrue(TIntHelper.IsNonDescending(a, @IntCmp));
end;

procedure TRegularArrayHelperTest.HeapSortOfStatic277RandomZeroes;
var
  I: Integer;
  a: array[1..277] of Integer;
begin
  for I := 1 to 277 do
    a[I] := Random(2);
  THackHelper.DoHeapSort(@a[1], 276, @IntCmp);
  AssertTrue(TIntHelper.IsNonDescending(a, @IntCmp));
end;

procedure TRegularArrayHelperTest.HeapSortOfDyn277;
var
  I: Integer;
  b: array[1..277] of Integer;
  a: TIntArray;
begin
  for I := 1 to 277 do
    b[I] := I;
  a := TIntHelper.CreateRandomShuffle(b);
  THackHelper.DoHeapSort(@a[0], 276, @IntCmp);
  AssertTrue(TIntHelper.Same(a, b, @IntCmp));
end;

procedure TRegularArrayHelperTest.HeapSortOfDyn277Random;
var
  I: Integer;
  a: TIntArray;
begin
  System.SetLength(a, 277);
  for I := 0 to System.High(a) do
    a[I] := Random(Integer.MaxValue);
  THackHelper.DoHeapSort(@a[0], 276, @IntCmp);
  AssertTrue(TIntHelper.IsNonDescending(a, @IntCmp));
end;

procedure TRegularArrayHelperTest.HeapSortOfDyn277RandomZeroes;
var
  I: Integer;
  a: TIntArray;
begin
  System.SetLength(a, 277);
  for I := 0 to System.High(a) do
    a[I] := Random(2);
  THackHelper.DoHeapSort(@a[0], 276, @IntCmp);
  AssertTrue(TIntHelper.IsNonDescending(a, @IntCmp));
end;

procedure TRegularArrayHelperTest.IntroSortOfEmpty;
var
  a: TIntArray;
begin
  //should not crash
  TIntHelper.IntroSort(a{%H-}, @IntCmp);
  AssertTrue(a = nil);
end;

procedure TRegularArrayHelperTest.IntroSortAscOfStatic3Asc;
const
  c: array[1..3] of Integer = (9, 13, 13);
var
  a: array[0..2] of Integer = (9, 13, 13);
begin
  TIntHelper.IntroSort(a, @IntCmp);
  AssertTrue(TIntHelper.Same(a, c, @IntCmp));
end;

procedure TRegularArrayHelperTest.IntroSortAscOfStatic3Desc;
const
  c: array[1..3] of Integer = (9, 13, 13);
var
  a: array[0..2] of Integer = (13, 13, 9);
begin
  TIntHelper.IntroSort(a, @IntCmp);
  AssertTrue(TIntHelper.Same(a, c, @IntCmp));
end;

procedure TRegularArrayHelperTest.IntroSortDescOfStatic3Desc;
const
  c: array[1..3] of Integer = (27, 11, 5);
var
  a: array[0..2] of Integer = (27, 11, 5);
begin
  TIntHelper.IntroSort(a, @IntCmp, soDesc);
  AssertTrue(TIntHelper.Same(a, c, @IntCmp));
end;

procedure TRegularArrayHelperTest.IntroSortDescOfStatic3Asc;
const
  c: array[1..3] of Integer = (27, 11, 5);
var
  a: array[0..2] of Integer = (11, 5, 27);
begin
  TIntHelper.IntroSort(a, @IntCmp, soDesc);
  AssertTrue(TIntHelper.Same(a, c, @IntCmp));
end;

procedure TRegularArrayHelperTest.IntroSortAscOfStatic577;
var
  I: Integer;
  a, b: array[1..577] of Integer;
begin
  for I := 1 to 577 do
    b[I] := I;
  a := b;
  TIntHelper.RandomShuffle(a);
  TIntHelper.IntroSort(a, @IntCmp);
  AssertTrue(TIntHelper.Same(a, b, @IntCmp));
end;

procedure TRegularArrayHelperTest.IntroSortDescOfStatic577;
var
  I: Integer;
  a, b: array[1..577] of Integer;
begin
  for I := 1 to 577 do
    b[I] := 578 - I;
  a := b;
  TIntHelper.RandomShuffle(a);
  TIntHelper.IntroSort(a, @IntCmp, soDesc);
  AssertTrue(TIntHelper.Same(a, b, @IntCmp));
end;

procedure TRegularArrayHelperTest.IntroSortAscOfStatic577Random;
var
  I: Integer;
  a: array[1..577] of Integer;
begin
  for I := 1 to 577 do
    a[I] := Random(Integer.MaxValue);
  TIntHelper.IntroSort(a, @IntCmp);
  AssertTrue(TIntHelper.IsNonDescending(a, @IntCmp));
end;

procedure TRegularArrayHelperTest.IntroSortAscOfStatic577RandomZeroes;
var
  I: Integer;
  a: array[1..577] of Integer;
begin
  for I := 1 to 577 do
    a[I] := Random(2);
  TIntHelper.IntroSort(a, @IntCmp);
  AssertTrue(TIntHelper.IsNonDescending(a, @IntCmp));
end;

procedure TRegularArrayHelperTest.IntroSortDescOfStatic577Random;
var
  I: Integer;
  a: array[1..577] of Integer;
begin
  for I := 1 to 577 do
    a[I] := Random(Integer.MaxValue);
  TIntHelper.IntroSort(a, @IntCmp, soDesc);
  AssertTrue(TIntHelper.IsNonAscending(a, @IntCmp));
end;

procedure TRegularArrayHelperTest.IntroSortDescOfStatic577RandomZeroes;
var
  I: Integer;
  a: array[1..577] of Integer;
begin
  for I := 1 to 577 do
    a[I] := Random(2);
  TIntHelper.IntroSort(a, @IntCmp, soDesc);
  AssertTrue(TIntHelper.IsNonAscending(a, @IntCmp));
end;

procedure TRegularArrayHelperTest.IntroSortAscOfDyn3Asc;
const
  c: array[1..3] of Integer = (11, 19, 22);
var
  a: TIntArray;
begin
  a := TIntArray.Create(11, 19, 22);
  TIntHelper.IntroSort(a, @IntCmp);
  AssertTrue(TIntHelper.Same(a, c, @IntCmp));
end;

procedure TRegularArrayHelperTest.IntroSortAscOfDyn3Desc;
const
  c: array[1..3] of Integer = (11, 19, 22);
var
  a: TIntArray;
begin
  a := TIntArray.Create(22, 11, 19);
  TIntHelper.IntroSort(a, @IntCmp);
  AssertTrue(TIntHelper.Same(a, c, @IntCmp));
end;

procedure TRegularArrayHelperTest.IntroSortDescOfDyn3Desc;
const
  c: array[1..3] of Integer = (31, 14, 3);
var
  a: TIntArray;
begin
  a := TIntArray.Create(31, 14, 3);
  TIntHelper.IntroSort(a, @IntCmp, soDesc);
  AssertTrue(TIntHelper.Same(a, c, @IntCmp));
end;

procedure TRegularArrayHelperTest.IntroSortDescOfDyn3Asc;
const
  c: array[1..3] of Integer = (31, 14, 3);
var
  a: TIntArray;
begin
  a := TIntArray.Create(14, 3, 31);
  TIntHelper.IntroSort(a, @IntCmp, soDesc);
  AssertTrue(TIntHelper.Same(a, c, @IntCmp));
end;

procedure TRegularArrayHelperTest.IntroSortAscOfDyn577;
var
  I: Integer;
  b: array[1..577] of Integer;
  a: TIntArray;
begin
  for I := 1 to 577 do
    b[I] := I;
  a := TIntHelper.CreateRandomShuffle(b);
  TIntHelper.IntroSort(a, @IntCmp);
  AssertTrue(TIntHelper.Same(a, b, @IntCmp));
end;

procedure TRegularArrayHelperTest.IntroSortDescOfDyn577;
var
  I: Integer;
  b: array[1..577] of Integer;
  a: TIntArray;
begin
  for I := 1 to 577 do
    b[I] := 578 - I;
  a := TIntHelper.CreateRandomShuffle(b);
  TIntHelper.IntroSort(a, @IntCmp, soDesc);
  AssertTrue(TIntHelper.Same(a, b, @IntCmp));
end;

procedure TRegularArrayHelperTest.IntroSortAscOfDyn577Random;
var
  I: Integer;
  a: TIntArray;
begin
  System.SetLength(a, 577);
  for I := 0 to System.High(a) do
    a[I] := Random(Integer.MaxValue);
  TIntHelper.IntroSort(a, @IntCmp);
  AssertTrue(TIntHelper.IsNonDescending(a, @IntCmp));
end;

procedure TRegularArrayHelperTest.IntroSortAscOfDyn577RandomZeroes;
var
  I: Integer;
  a: TIntArray;
begin
  System.SetLength(a, 577);
  for I := 0 to System.High(a) do
    a[I] := Random(2);
  TIntHelper.IntroSort(a, @IntCmp);
  AssertTrue(TIntHelper.IsNonDescending(a, @IntCmp));
end;

procedure TRegularArrayHelperTest.IntroSortDescOfDyn577Random;
var
  I: Integer;
  a: TIntArray;
begin
  System.SetLength(a, 577);
  for I := 0 to System.High(a) do
    a[I] := Random(Integer.MaxValue);
  TIntHelper.IntroSort(a, @IntCmp, soDesc);
  AssertTrue(TIntHelper.IsNonAscending(a, @IntCmp));
end;

procedure TRegularArrayHelperTest.IntroSortDescOfDyn577RandomZeroes;
var
  I: Integer;
  a: TIntArray;
begin
  System.SetLength(a, 577);
  for I := 0 to System.High(a) do
    a[I] := Random(2);
  TIntHelper.IntroSort(a, @IntCmp, soDesc);
  AssertTrue(TIntHelper.IsNonAscending(a, @IntCmp));
end;

procedure TRegularArrayHelperTest.DualPivotQuickSortOfEmpty;
var
  a: TIntArray;
begin
  //should not crash
  TIntHelper.DualPivotQuickSort(a{%H-}, @IntCmp);
  AssertTrue(a = nil);
end;

procedure TRegularArrayHelperTest.DualPivotQuickSortAscOfStatic3Asc;
const
  c: array[1..3] of Integer = (9, 13, 13);
var
  a: array[0..2] of Integer = (9, 13, 13);
begin
  TIntHelper.DualPivotQuickSort(a, @IntCmp);
  AssertTrue(TIntHelper.Same(a, c, @IntCmp));
end;

procedure TRegularArrayHelperTest.DualPivotQuickSortAscOfStatic3Desc;
const
  c: array[1..3] of Integer = (9, 13, 13);
var
  a: array[0..2] of Integer = (13, 13, 9);
begin
  TIntHelper.DualPivotQuickSort(a, @IntCmp);
  AssertTrue(TIntHelper.Same(a, c, @IntCmp));
end;

procedure TRegularArrayHelperTest.DualPivotQuickSortDescOfStatic3Desc;
const
  c: array[1..3] of Integer = (27, 11, 5);
var
  a: array[0..2] of Integer = (27, 11, 5);
begin
  TIntHelper.DualPivotQuickSort(a, @IntCmp, soDesc);
  AssertTrue(TIntHelper.Same(a, c, @IntCmp));
end;

procedure TRegularArrayHelperTest.DualPivotQuickSortDescOfStatic3Asc;
const
  c: array[1..3] of Integer = (27, 11, 5);
var
  a: array[0..2] of Integer = (11, 5, 27);
begin
  TIntHelper.DualPivotQuickSort(a, @IntCmp, soDesc);
  AssertTrue(TIntHelper.Same(a, c, @IntCmp));
end;

procedure TRegularArrayHelperTest.DualPivotQuickSortAscOfStatic577;
var
  I: Integer;
  a, b: array[1..577] of Integer;
begin
  for I := 1 to 577 do
    b[I] := I;
  a := b;
  TIntHelper.RandomShuffle(a);
  TIntHelper.DualPivotQuickSort(a, @IntCmp);
  AssertTrue(TIntHelper.Same(a, b, @IntCmp));
end;

procedure TRegularArrayHelperTest.DualPivotQuickSortDescOfStatic577;
var
  I: Integer;
  a, b: array[1..577] of Integer;
begin
  for I := 1 to 577 do
    b[I] := 578 - I;
  a := b;
  TIntHelper.RandomShuffle(a);
  TIntHelper.DualPivotQuickSort(a, @IntCmp, soDesc);
  AssertTrue(TIntHelper.Same(a, b, @IntCmp));
end;

procedure TRegularArrayHelperTest.DualPivotQuickSortAscOfStatic577Random;
var
  I: Integer;
  a: array[1..577] of Integer;
begin
  for I := 1 to 577 do
    a[I] := Random(Integer.MaxValue);
  TIntHelper.DualPivotQuickSort(a, @IntCmp);
  AssertTrue(TIntHelper.IsNonDescending(a, @IntCmp));
end;

procedure TRegularArrayHelperTest.DualPivotQuickSortAscOfStatic577RandomZeroes;
var
  I: Integer;
  a: array[1..577] of Integer;
begin
  for I := 1 to 577 do
    a[I] := Random(2);
  TIntHelper.DualPivotQuickSort(a, @IntCmp);
  AssertTrue(TIntHelper.IsNonDescending(a, @IntCmp));
end;

procedure TRegularArrayHelperTest.DualPivotQuickSortDescOfStatic577Random;
var
  I: Integer;
  a: array[1..577] of Integer;
begin
  for I := 1 to 577 do
    a[I] := Random(Integer.MaxValue);
  TIntHelper.DualPivotQuickSort(a, @IntCmp, soDesc);
  AssertTrue(TIntHelper.IsNonAscending(a, @IntCmp));
end;

procedure TRegularArrayHelperTest.DualPivotQuickSortDescOfStatic577RandomZeroes;
var
  I: Integer;
  a: array[1..577] of Integer;
begin
  for I := 1 to 577 do
    a[I] := Random(2);
  TIntHelper.DualPivotQuickSort(a, @IntCmp, soDesc);
  AssertTrue(TIntHelper.IsNonAscending(a, @IntCmp));
end;

procedure TRegularArrayHelperTest.DualPivotQuickSortAscOfDyn3Asc;
const
  c: array[1..3] of Integer = (11, 19, 22);
var
  a: TIntArray;
begin
  a := TIntArray.Create(11, 19, 22);
  TIntHelper.DualPivotQuickSort(a, @IntCmp);
  AssertTrue(TIntHelper.Same(a, c, @IntCmp));
end;

procedure TRegularArrayHelperTest.DualPivotQuickSortAscOfDyn3Desc;
const
  c: array[1..3] of Integer = (11, 19, 22);
var
  a: TIntArray;
begin
  a := TIntArray.Create(22, 11, 19);
  TIntHelper.DualPivotQuickSort(a, @IntCmp);
  AssertTrue(TIntHelper.Same(a, c, @IntCmp));
end;

procedure TRegularArrayHelperTest.DualPivotQuickSortDescOfDyn3Desc;
const
  c: array[1..3] of Integer = (31, 14, 3);
var
  a: TIntArray;
begin
  a := TIntArray.Create(31, 14, 3);
  TIntHelper.DualPivotQuickSort(a, @IntCmp, soDesc);
  AssertTrue(TIntHelper.Same(a, c, @IntCmp));
end;

procedure TRegularArrayHelperTest.DualPivotQuickSortDescOfDyn3Asc;
const
  c: array[1..3] of Integer = (31, 14, 3);
var
  a: TIntArray;
begin
  a := TIntArray.Create(14, 3, 31);
  TIntHelper.DualPivotQuickSort(a, @IntCmp, soDesc);
  AssertTrue(TIntHelper.Same(a, c, @IntCmp));
end;

procedure TRegularArrayHelperTest.DualPivotQuickSortAscOfDyn577;
var
  I: Integer;
  b: array[1..577] of Integer;
  a: TIntArray;
begin
  for I := 1 to 577 do
    b[I] := I;
  a := TIntHelper.CreateRandomShuffle(b);
  TIntHelper.DualPivotQuickSort(a, @IntCmp);
  AssertTrue(TIntHelper.Same(a, b, @IntCmp));
end;

procedure TRegularArrayHelperTest.DualPivotQuickSortDescOfDyn577;
var
  I: Integer;
  b: array[1..577] of Integer;
  a: TIntArray;
begin
  for I := 1 to 577 do
    b[I] := 578 - I;
  a := TIntHelper.CreateRandomShuffle(b);
  TIntHelper.DualPivotQuickSort(a, @IntCmp, soDesc);
  AssertTrue(TIntHelper.Same(a, b, @IntCmp));
end;

procedure TRegularArrayHelperTest.DualPivotQuickSortAscOfDyn577Random;
var
  I: Integer;
  a: TIntArray;
begin
  System.SetLength(a, 577);
  for I := 0 to System.High(a) do
    a[I] := Random(Integer.MaxValue);
  TIntHelper.DualPivotQuickSort(a, @IntCmp);
  AssertTrue(TIntHelper.IsNonDescending(a, @IntCmp));
end;

procedure TRegularArrayHelperTest.DualPivotQuickSortAscOfDyn577RandomZeroes;
var
  I: Integer;
  a: TIntArray;
begin
  System.SetLength(a, 577);
  for I := 0 to System.High(a) do
    a[I] := Random(2);
  TIntHelper.DualPivotQuickSort(a, @IntCmp);
  AssertTrue(TIntHelper.IsNonDescending(a, @IntCmp));
end;

procedure TRegularArrayHelperTest.DualPivotQuickSortDescOfDyn577Random;
var
  I: Integer;
  a: TIntArray;
begin
  System.SetLength(a, 577);
  for I := 0 to System.High(a) do
    a[I] := Random(Integer.MaxValue);
  TIntHelper.DualPivotQuickSort(a, @IntCmp, soDesc);
  AssertTrue(TIntHelper.IsNonAscending(a, @IntCmp));
end;

procedure TRegularArrayHelperTest.DualPivotQuickSortDescOfDyn577RandomZeroes;
var
  I: Integer;
  a: TIntArray;
begin
  System.SetLength(a, 577);
  for I := 0 to System.High(a) do
    a[I] := Random(2);
  TIntHelper.DualPivotQuickSort(a, @IntCmp, soDesc);
  AssertTrue(TIntHelper.IsNonAscending(a, @IntCmp));
end;

procedure TRegularArrayHelperTest.MergeSortOfEmpty;
var
  a: TIntArray;
begin
  //should not crash
  TIntHelper.MergeSort(a{%H-}, @IntCmp);
  AssertTrue(a = nil);
end;

procedure TRegularArrayHelperTest.MergeSortAscOfStatic3Asc;
const
  c: array[1..3] of Integer = (9, 13, 13);
var
  a: array[0..2] of Integer = (9, 13, 13);
begin
  TIntHelper.MergeSort(a, @IntCmp);
  AssertTrue(TIntHelper.Same(a, c, @IntCmp));
end;

procedure TRegularArrayHelperTest.MergeSortAscOfStatic3Desc;
const
  c: array[1..3] of Integer = (9, 13, 13);
var
  a: array[0..2] of Integer = (13, 13, 9);
begin
  TIntHelper.MergeSort(a, @IntCmp);
  AssertTrue(TIntHelper.Same(a, c, @IntCmp));
end;

procedure TRegularArrayHelperTest.MergeSortDescOfStatic3Desc;
const
  c: array[1..3] of Integer = (27, 11, 5);
var
  a: array[0..2] of Integer = (27, 11, 5);
begin
  TIntHelper.MergeSort(a, @IntCmp, soDesc);
  AssertTrue(TIntHelper.Same(a, c, @IntCmp));
end;

procedure TRegularArrayHelperTest.MergeSortDescOfStatic3Asc;
const
  c: array[1..3] of Integer = (27, 11, 5);
var
  a: array[0..2] of Integer = (11, 5, 27);
begin
  TIntHelper.MergeSort(a, @IntCmp, soDesc);
  AssertTrue(TIntHelper.Same(a, c, @IntCmp));
end;

procedure TRegularArrayHelperTest.MergeSortAscOfStatic577;
var
  I: Integer;
  a, b: array[1..577] of Integer;
begin
  for I := 1 to 577 do
    b[I] := I;
  a := b;
  TIntHelper.RandomShuffle(a);
  TIntHelper.MergeSort(a, @IntCmp);
  AssertTrue(TIntHelper.Same(a, b, @IntCmp));
end;

procedure TRegularArrayHelperTest.MergeSortDescOfStatic577;
var
  I: Integer;
  a, b: array[1..577] of Integer;
begin
  for I := 1 to 577 do
    b[I] := 578 - I;
  a := b;
  TIntHelper.RandomShuffle(a);
  TIntHelper.MergeSort(a, @IntCmp, soDesc);
  AssertTrue(TIntHelper.Same(a, b, @IntCmp));
end;

procedure TRegularArrayHelperTest.MergeSortAscOfStatic577Random;
var
  I: Integer;
  a: array[1..577] of Integer;
begin
  for I := 1 to 577 do
    a[I] := Random(Integer.MaxValue);
  TIntHelper.MergeSort(a, @IntCmp);
  AssertTrue(TIntHelper.IsNonDescending(a, @IntCmp));
end;

procedure TRegularArrayHelperTest.MergeSortAscOfStatic577RandomZeroes;
var
  I: Integer;
  a: array[1..577] of Integer;
begin
  for I := 1 to 577 do
    a[I] := Random(2);
  TIntHelper.MergeSort(a, @IntCmp);
  AssertTrue(TIntHelper.IsNonDescending(a, @IntCmp));
end;

procedure TRegularArrayHelperTest.MergeSortDescOfStatic577Random;
var
  I: Integer;
  a: array[1..577] of Integer;
begin
  for I := 1 to 577 do
    a[I] := Random(Integer.MaxValue);
  TIntHelper.MergeSort(a, @IntCmp, soDesc);
  AssertTrue(TIntHelper.IsNonAscending(a, @IntCmp));
end;

procedure TRegularArrayHelperTest.MergeSortDescOfStatic577RandomZeroes;
var
  I: Integer;
  a: array[1..577] of Integer;
begin
  for I := 1 to 577 do
    a[I] := Random(2);
  TIntHelper.MergeSort(a, @IntCmp, soDesc);
  AssertTrue(TIntHelper.IsNonAscending(a, @IntCmp));
end;

procedure TRegularArrayHelperTest.MergeSortAscOfDyn3Asc;
const
  c: array[1..3] of Integer = (11, 19, 22);
var
  a: TIntArray;
begin
  a := TIntArray.Create(11, 19, 22);
  TIntHelper.MergeSort(a, @IntCmp);
  AssertTrue(TIntHelper.Same(a, c, @IntCmp));
end;

procedure TRegularArrayHelperTest.MergeSortAscOfDyn3Desc;
const
  c: array[1..3] of Integer = (11, 19, 22);
var
  a: TIntArray;
begin
  a := TIntArray.Create(22, 11, 19);
  TIntHelper.MergeSort(a, @IntCmp);
  AssertTrue(TIntHelper.Same(a, c, @IntCmp));
end;

procedure TRegularArrayHelperTest.MergeSortDescOfDyn3Desc;
const
  c: array[1..3] of Integer = (31, 14, 3);
var
  a: TIntArray;
begin
  a := TIntArray.Create(31, 14, 3);
  TIntHelper.MergeSort(a, @IntCmp, soDesc);
  AssertTrue(TIntHelper.Same(a, c, @IntCmp));
end;

procedure TRegularArrayHelperTest.MergeSortDescOfDyn3Asc;
const
  c: array[1..3] of Integer = (31, 14, 3);
var
  a: TIntArray;
begin
  a := TIntArray.Create(14, 3, 31);
  TIntHelper.MergeSort(a, @IntCmp, soDesc);
  AssertTrue(TIntHelper.Same(a, c, @IntCmp));
end;

procedure TRegularArrayHelperTest.MergeSortAscOfDyn577;
var
  I: Integer;
  b: array[1..577] of Integer;
  a: TIntArray;
begin
  for I := 1 to 577 do
    b[I] := I;
  a := TIntHelper.CreateRandomShuffle(b);
  TIntHelper.MergeSort(a, @IntCmp);
  AssertTrue(TIntHelper.Same(a, b, @IntCmp));
end;

procedure TRegularArrayHelperTest.MergeSortDescOfDyn577;
var
  I: Integer;
  b: array[1..577] of Integer;
  a: TIntArray;
begin
  for I := 1 to 577 do
    b[I] := 578 - I;
  a := TIntHelper.CreateRandomShuffle(b);
  TIntHelper.MergeSort(a, @IntCmp, soDesc);
  AssertTrue(TIntHelper.Same(a, b, @IntCmp));
end;

procedure TRegularArrayHelperTest.MergeSortAscOfDyn577Random;
var
  I: Integer;
  a: TIntArray;
begin
  System.SetLength(a, 577);
  for I := 0 to System.High(a) do
    a[I] := Random(Integer.MaxValue);
  TIntHelper.MergeSort(a, @IntCmp);
  AssertTrue(TIntHelper.IsNonDescending(a, @IntCmp));
end;

procedure TRegularArrayHelperTest.MergeSortAscOfDyn577RandomZeroes;
var
  I: Integer;
  a: TIntArray;
begin
  System.SetLength(a, 577);
  for I := 0 to System.High(a) do
    a[I] := Random(2);
  TIntHelper.MergeSort(a, @IntCmp);
  AssertTrue(TIntHelper.IsNonDescending(a, @IntCmp));
end;

procedure TRegularArrayHelperTest.MergeSortDescOfDyn577Random;
var
  I: Integer;
  a: TIntArray;
begin
  System.SetLength(a, 577);
  for I := 0 to System.High(a) do
    a[I] := Random(Integer.MaxValue);
  TIntHelper.MergeSort(a, @IntCmp, soDesc);
  AssertTrue(TIntHelper.IsNonAscending(a, @IntCmp));
end;

procedure TRegularArrayHelperTest.MergeSortDescOfDyn577RandomZeroes;
var
  I: Integer;
  a: TIntArray;
begin
  System.SetLength(a, 577);
  for I := 0 to System.High(a) do
    a[I] := Random(2);
  TIntHelper.MergeSort(a, @IntCmp, soDesc);
  AssertTrue(TIntHelper.IsNonAscending(a, @IntCmp));
end;

procedure TRegularArrayHelperTest.MergeSortAscOfStatic877Stable;
const
  ValCount = 42;
  ItemsCount = 877;
type
  TValCounter = array[0..Pred(ValCount)] of Integer;
var
  ValCounter: TValCounter;
  a: array[1..ItemsCount] of TIntPair;
  I, J, v: Integer;
begin
  ValCounter := Default(TValCounter);
  for I := 1 to ItemsCount do
    begin
      v := Random(ValCount);
      a[I].Data := v;
      a[I].Index := ValCounter[v];
      Inc(ValCounter[v]);
    end;
  // pairs with same .Data have strictly increasing .Index values;
  // after stable sorting, this order should be preserved
  TPairDataHelper.MergeSort(a, @PairCmp);
  AssertTrue(TPairDataHelper.IsNonDescending(a, @PairCmp));
  I := 1;
  J := 1;
  v := 0;
  repeat
    while (J <= ItemsCount) and (a[J].Data = v) do
      Inc(J);
    AssertTrue(ValCounter[v] = J - I);
    if I < Pred(J) then //current range may be empty or has single element - it is randomness :)
      AssertTrue(TPairIdxHelper.IsStrictAscending(a[I..Pred(J)]))
    else
      AssertTrue(TPairIdxHelper.IsNonDescending(a[I..Pred(J)]));
    Inc(v);
    I := J;
  until v = ValCount;
end;

procedure TRegularArrayHelperTest.MergeSortDescOfStatic877Stable;
const
  ValCount = 42;
  ItemsCount = 877;
type
  TValCounter = array[0..Pred(ValCount)] of Integer;
var
  ValCounter: TValCounter;
  a: array[1..ItemsCount] of TIntPair;
  I, J, v: Integer;
begin
  ValCounter := Default(TValCounter);
  for I := 1 to ItemsCount do
    begin
      v := Random(ValCount);
      a[I].Data := v;
      a[I].Index := ValCounter[v];
      Inc(ValCounter[v]);
    end;
  // pairs with same .Data have strictly increasing .Index values;
  // after stable sorting, this order should be preserved
  TPairDataHelper.MergeSort(a, @PairCmp, soDesc);
  AssertTrue(TPairDataHelper.IsNonAscending(a, @PairCmp));
  I := 1;
  J := 1;
  v := Pred(ValCount);
  repeat
    while (J <= ItemsCount) and (a[J].Data = v) do
      Inc(J);
    AssertTrue(ValCounter[v] = J - I);
    if I < Pred(J) then //current range may be empty or has single element - it is randomness :)
      AssertTrue(TPairIdxHelper.IsStrictAscending(a[I..Pred(J)]))
    else
      AssertTrue(TPairIdxHelper.IsNonDescending(a[I..Pred(J)]));
    Dec(v);
    I := J;
  until v < 0;
end;

procedure TRegularArrayHelperTest.MergeSortAscOfDyn877Stable;
const
  ValCount = 42;
  ItemsCount = 877;
type
  TValCounter = array[0..Pred(ValCount)] of Integer;
var
  ValCounter: TValCounter;
  a: TIntPairs;
  I, J, v: Integer;
begin
  ValCounter := Default(TValCounter);
  System.SetLength(a, ItemsCount);
  for I := 0 to System.High(a) do
    begin
      v := Random(ValCount);
      a[I].Data := v;
      a[I].Index := ValCounter[v];
      Inc(ValCounter[v]);
    end;
  // pairs with same .Data have strictly increasing .Index values;
  // after stable sorting, this order should be preserved
  TPairDataHelper.MergeSort(a, @PairCmp);
  AssertTrue(TPairDataHelper.IsNonDescending(a, @PairCmp));
  I := 0;
  J := 0;
  v := 0;
  repeat
    while (J < ItemsCount) and (a[J].Data = v) do
      Inc(J);
    AssertTrue(ValCounter[v] = J - I);
    if I < Pred(J) then //current range may be empty or has single element - it is randomness :)
      AssertTrue(TPairIdxHelper.IsStrictAscending(a[I..Pred(J)]))
    else
      AssertTrue(TPairIdxHelper.IsNonDescending(a[I..Pred(J)]));
    Inc(v);
    I := J;
  until v = ValCount;
end;

procedure TRegularArrayHelperTest.MergeSortDescOfDyn877Stable;
const
  ValCount = 42;
  ItemsCount = 877;
type
  TValCounter = array[0..Pred(ValCount)] of Integer;
var
  ValCounter: TValCounter;
  a: TIntPairs;
  I, J, v: Integer;
begin
  ValCounter := Default(TValCounter);
  System.SetLength(a, ItemsCount);
  for I := 0 to System.High(a) do
    begin
      v := Random(ValCount);
      a[I].Data := v;
      a[I].Index := ValCounter[v];
      Inc(ValCounter[v]);
    end;
  // pairs with same .Data have strictly increasing .Index values;
  // after stable sorting, this order should be preserved
  TPairDataHelper.MergeSort(a, @PairCmp, soDesc);
  AssertTrue(TPairDataHelper.IsNonAscending(a, @PairCmp));
  I := 0;
  J := 0;
  v := Pred(ValCount);
  repeat
    while (J < ItemsCount) and (a[J].Data = v) do
      Inc(J);
    AssertTrue(ValCounter[v] = J - I);
    if I < Pred(J) then //current range may be empty or has single element - it is randomness :)
      AssertTrue(TPairIdxHelper.IsStrictAscending(a[I..Pred(J)]))
    else
      AssertTrue(TPairIdxHelper.IsNonDescending(a[I..Pred(J)]));
    Dec(v);
    I := J;
  until v < 0;
end;

procedure TRegularArrayHelperTest.PDQSortTest;
var
  a, b: TIntArray;
  I: Integer;
const
  TestSize = 1000;
begin
  TIntHelper.PDQSort(a{%H-}, @IntCmp);
  AssertTrue(a = nil);
  a := [9, 13, 13];
  TIntHelper.PDQSort(a, @IntCmp);
  AssertTrue(TIntHelper.Same(a, [9, 13, 13], @IntCmp));
  a := [13, 11, 5];
  TIntHelper.PDQSort(a, @IntCmp);
  AssertTrue(TIntHelper.Same(a, [5, 11, 13], @IntCmp));
  SetLength(b, TestSize);
  for I := 0 to Pred(TestSize) do
    b[I] := I;
  a := TIntHelper.CreateRandomShuffle(b);
  TIntHelper.PDQSort(a, @IntCmp);
  AssertTrue(TIntHelper.Same(a, b, @IntCmp));
  a := TIntHelper.CreateRandomShuffle(b);
  TIntHelper.Reverse(b);
  TIntHelper.PDQSort(a, @IntCmp, soDesc);
  AssertTrue(TIntHelper.Same(a, b, @IntCmp));
end;

procedure TRegularArrayHelperTest.TimSortTest;
var
  a, b: TIntArray;
  I: Integer;
const
  TestSize = 1000;
begin
  TIntTimSort.Sort(a{%H-}, @IntCmp);
  AssertTrue(a = nil);
  a := [9, 13, 13];
  TIntTimSort.Sort(a, @IntCmp);
  AssertTrue(TIntHelper.Same(a, [9, 13, 13], @IntCmp));
  a := [13, 11, 5];
  TIntTimSort.Sort(a, @IntCmp);
  AssertTrue(TIntHelper.Same(a, [5, 11, 13], @IntCmp));
  SetLength(b, TestSize);
  for I := 0 to Pred(TestSize) do
    b[I] := I;
  a := TIntHelper.CreateRandomShuffle(b);
  TIntTimSort.Sort(a, @IntCmp);
  AssertTrue(TIntHelper.Same(a, b, @IntCmp));
  a := TIntHelper.CreateRandomShuffle(b);
  TIntHelper.Reverse(b);
  TIntTimSort.Sort(a, @IntCmp, soDesc);
  AssertTrue(TIntHelper.Same(a, b, @IntCmp));
end;

procedure TRegularArrayHelperTest.TimSortAscStableTest;
const
  ValCount = 65;
  TestSize = 1000;
type
  TCounter = array[0..Pred(ValCount)] of Integer;
var
  Counter: TCounter;
  a: TIntPairs;
  I, J, v: Integer;
begin
  Counter := Default(TCounter);
  System.SetLength(a, TestSize);
  for I := 0 to System.High(a) do
    begin
      v := Random(ValCount);
      a[I].Data := v;
      a[I].Index := Counter[v];
      Inc(Counter[v]);
    end;
  // pairs with same .Data have strictly increasing .Index values;
  // after stable sorting, this order should be preserved
  TPairTimSort.Sort(a, @PairCmp);
  AssertTrue(TPairDataHelper.IsNonDescending(a, @PairCmp));
  I := 0;
  J := 0;
  v := 0;
  repeat
    while (J < TestSize) and (a[J].Data = v) do
      Inc(J);
    AssertTrue(Counter[v] = J - I);
    if I < Pred(J) then //current range may be empty or has single element - it is randomness :)
      AssertTrue(TPairIdxHelper.IsStrictAscending(a[I..Pred(J)]))
    else
      AssertTrue(TPairIdxHelper.IsNonDescending(a[I..Pred(J)]));
    Inc(v);
    I := J;
  until v = ValCount;
end;

procedure TRegularArrayHelperTest.TimSortDescStableTest;
const
  ValCount = 55;
  TestSize = 1000;
type
  TCounter = array[0..Pred(ValCount)] of Integer;
var
  Counter: TCounter;
  a: TIntPairs;
  I, J, v: Integer;
begin
  Counter := Default(TCounter);
  System.SetLength(a, TestSize);
  for I := 0 to System.High(a) do
    begin
      v := Random(ValCount);
      a[I].Data := v;
      a[I].Index := Counter[v];
      Inc(Counter[v]);
    end;
  // pairs with same .Data have strictly increasing .Index values;
  // after stable sorting, this order should be preserved
  TPairTimSort.Sort(a, @PairCmp, soDesc);
  AssertTrue(TPairDataHelper.IsNonAscending(a, @PairCmp));
  I := 0;
  J := 0;
  v := Pred(ValCount);
  repeat
    while (J < TestSize) and (a[J].Data = v) do
      Inc(J);
    AssertTrue(Counter[v] = J - I);
    if I < Pred(J) then //current range may be empty or has single element - it is randomness :)
      AssertTrue(TPairIdxHelper.IsStrictAscending(a[I..Pred(J)]))
    else
      AssertTrue(TPairIdxHelper.IsNonDescending(a[I..Pred(J)]));
    Dec(v);
    I := J;
  until v < 0;
end;



initialization

  RegisterTest(TRegularArrayHelperTest);

end.

