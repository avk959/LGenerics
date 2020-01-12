unit LGBaseArrayHelperTest;

{$mode objfpc}{$H+}

interface

uses

  SysUtils, fpcunit, testregistry,
  LGUtils,
  LGArrayHelpers,
  LGMiscUtils,
  LGDeque;

type

  { TBaseArrayHelperTest }

  TBaseArrayHelperTest = class(TTestCase)
  private
  type
    TIntCmpRel = class
      class function Less(L, R: Integer): Boolean; static;
    end;

    TIntHelper  = specialize TGBaseArrayHelper<Integer, TIntCmpRel>;
    TIntTimSort = specialize TGBaseTimSort<Integer, TIntCmpRel>;
    THackHelper = class(TIntHelper);
    TIntArray   = specialize TGArray<Integer>;

    TIntPair    = record
      Data,
      Index: Integer;
    end;

    TIntPairs    = specialize TGArray<TIntPair>;

    TPairDataCmp = class
      class function Less(const L, R: TIntPair): Boolean; static;
    end;

    TPairIdxCmp  = class
      class function Less(const L, R: TIntPair): Boolean; static;
    end;

    TPairDataHelper = specialize TGBaseArrayHelper<TIntPair, TPairDataCmp>;
    TPairTimSort    = specialize TGBaseTimSort<TIntPair, TPairDataCmp>;

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
    procedure BinarySearchStaticSuccessDesc20;

    procedure BinarySearchPosStaticSuccess20;
    procedure BinarySearchPosStaticSuccess20_1;
    procedure BinarySearchPosDynSuccess20_2;
    procedure BinarySearchPosStaticSuccess20_3;
    procedure BinarySearchPosStaticSuccess20_4;
    procedure BinarySearchPosStaticSuccess20_5;
    procedure BinarySearchPosStaticSuccessEqual10;

    procedure BinarySearchPosDynEmpty;
    procedure BinarySearchPosDynSingle;
    procedure BinarySearchPosDynSingle_1;
    procedure BinarySearchPosDynSingle_2;
    procedure BinarySearchPosStaticUnsuccess20;
    procedure BinarySearchPosStaticUnsuccess20_1;
    procedure BinarySearchPosStaticUnsuccess20_2;
    procedure BinarySearchPosStaticUnsuccess20_3;
    procedure BinarySearchPosStaticUnsuccess20_4;
    procedure BinarySearchPosStaticUnsuccess20_5;
    procedure BinarySearchPosStaticUnuccessEqual10;
    procedure BinarySearchPosStaticUnuccessEqual10_1;

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

  { TBaseIndexedHelperTest }

  TBaseIndexedHelperTest = class(TTestCase)
  private
  type
    TIntDeque  = specialize TGLiteDeque<Integer>;
    TDeqHelper = specialize TGIndexedHelper<Integer, TIntDeque>;
    THelper    = specialize TGArrayHelper<Integer>;
  published
    procedure CreateCopy;
    procedure Reverse;
    procedure Reverse2;
    procedure SequentSearch;
    procedure IndexOfMin;
    procedure IndexOfMax;
    procedure Sort;
    procedure Sort2;
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

  IntDescSorted20: array[1..20] of Integer = (
    55, 15, 13, 10, 10, 2, 2, -5, -7, -7, -8, -10, -14, -17, -17, -28, -28, -39, -40, -40);

  InOrderSrc21: TIntArray21 = (0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20);

  InOrderDblSrc21: TIntArray21 = (0, 0, 1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6, 7, 7, 8, 8, 9, 9, 10);

{ TBaseArrayHelperTest.TIntCmpRel }

class function TBaseArrayHelperTest.TIntCmpRel.Less(L, R: Integer): Boolean;
begin
  Result := L < R;
end;

{ TBaseArrayHelperTest.TPairDataCmp }

class function TBaseArrayHelperTest.TPairDataCmp.Less(const L, R: TIntPair): Boolean;
begin
  Result := L.Data < R.Data;
end;

{ TBaseArrayHelperTest.TPairIdxCmp }

class function TBaseArrayHelperTest.TPairIdxCmp.Less(const L, R: TIntPair): Boolean;
begin
  Result := L.Index < R.Index;
end;

{ TBaseArrayHelperTest }

procedure TBaseArrayHelperTest.SequentSearchEmpty;
var
  a: TIntArray;
begin
  AssertTrue(TIntHelper.SequentSearch(a{%H-}, 13) = -1);
end;

procedure TBaseArrayHelperTest.SequentSearchStaticSuccess1;
begin
  AssertTrue(TIntHelper.SequentSearch(IntSrc10[3..3], -15) = 0);
end;

procedure TBaseArrayHelperTest.SequentSearchStaticUnsuccess1;
begin
  AssertTrue(TIntHelper.SequentSearch(IntSrc10[3..3], 10) = -1);
end;

procedure TBaseArrayHelperTest.SequentSearchDynSuccess1;
begin
  AssertTrue(TIntHelper.SequentSearch(TIntArray.Create(-15), -15) = 0);
end;

procedure TBaseArrayHelperTest.SequentSearchDynUnsuccess1;
begin
  AssertTrue(TIntHelper.SequentSearch(TIntArray.Create(-15), 10) = -1);
end;

procedure TBaseArrayHelperTest.SequentSearchStaticSuccess10;
begin
  AssertTrue(TIntHelper.SequentSearch(IntSrc10, -1) = 9);
end;

procedure TBaseArrayHelperTest.SequentSearchStaticUnsuccess10;
begin
  AssertTrue(TIntHelper.SequentSearch(IntSrc10, 11) = -1);
end;

procedure TBaseArrayHelperTest.BinarySearchEmpty;
var
  a: TIntArray;
begin
  AssertTrue(TIntHelper.BinarySearch(a{%H-}, 13) = -1);
end;

procedure TBaseArrayHelperTest.BinarySearchStaticSuccess1;
begin
  AssertTrue(TIntHelper.BinarySearch(IntSrc10[3..3], -15) = 0);
end;

procedure TBaseArrayHelperTest.BinarySearchStaticUnsuccess1;
begin
  AssertTrue(TIntHelper.BinarySearch(IntSrc10[3..3], 10) = -1);
end;

procedure TBaseArrayHelperTest.BinarySearchStaticSuccess2;
begin
  AssertTrue(TIntHelper.BinarySearch(IntSorted10Uniq[3..4], 3) = 1);
end;

procedure TBaseArrayHelperTest.BinarySearchStaticUnsuccess2;
begin
  AssertTrue(TIntHelper.BinarySearch(IntSorted10Uniq[3..4], 2) = -1);
end;

procedure TBaseArrayHelperTest.BinarySearchStaticSuccess10Uniq;
begin
  AssertTrue(TIntHelper.BinarySearch(IntSorted10Uniq, 14) = 4);
end;

procedure TBaseArrayHelperTest.BinarySearchStaticUnsuccess10Uniq;
begin
  AssertTrue(TIntHelper.BinarySearch(IntSorted10Uniq, 11) = -1);
end;

procedure TBaseArrayHelperTest.BinarySearchStaticSuccess20;
var
  I: SizeInt;
begin
  I := TIntHelper.BinarySearch(InOrderSrc21, 20);
  AssertTrue(I = 20);
end;

procedure TBaseArrayHelperTest.BinarySearchStaticSuccess20_1;
begin
  AssertTrue(TIntHelper.BinarySearch(IntSorted20, 8) = 10);
end;

procedure TBaseArrayHelperTest.BinarySearchStaticSuccess20_2;
begin
  AssertTrue(TIntHelper.BinarySearch(IntSorted20, -5) = 0);
end;

procedure TBaseArrayHelperTest.BinarySearchStaticSuccess20_3;
begin
  AssertTrue(TIntHelper.BinarySearch(IntSorted20, 17) = 13);
end;

procedure TBaseArrayHelperTest.BinarySearchStaticUnsuccessEqual10;
begin
  AssertTrue(TIntHelper.BinarySearch(IntEqual10, 5) = -1);
end;

procedure TBaseArrayHelperTest.BinarySearchStaticSuccessEqual10;
begin
  AssertTrue(TIntHelper.BinarySearch(IntEqual10, 9) = 0);
end;

procedure TBaseArrayHelperTest.BinarySearchStaticSuccessDesc20;
begin
  AssertTrue(TIntHelper.BinarySearch(IntDescSorted20, 2) = 5);
  AssertTrue(TIntHelper.BinarySearch(IntDescSorted20, -17) = 13);
  AssertTrue(TIntHelper.BinarySearch(IntDescSorted20, -40) = 18);
end;

procedure TBaseArrayHelperTest.BinarySearchPosStaticSuccess20;
var
  sr: TSearchResult;
begin
  sr := TIntHelper.BinarySearchPos(IntSorted20, 28);
  AssertTrue(sr.FoundIndex = 16);
  AssertTrue(sr.InsertIndex = Succ(sr.FoundIndex));
end;

procedure TBaseArrayHelperTest.BinarySearchPosStaticSuccess20_1;
var
  sr: TSearchResult;
begin
  sr := TIntHelper.BinarySearchPos(InOrderSrc21, 20);
  AssertTrue(sr.FoundIndex = 20);
  AssertTrue(sr.InsertIndex = Succ(sr.FoundIndex));
end;

procedure TBaseArrayHelperTest.BinarySearchPosDynSuccess20_2;
var
  a: TIntArray;
  sr: TSearchResult;
begin
  a := TIntHelper.CreateReverseCopy(InOrderSrc21);
  sr := TIntHelper.BinarySearchPos(a, 20);
  AssertTrue(sr.FoundIndex = 0);
  AssertTrue(sr.InsertIndex = Succ(sr.FoundIndex));
end;

procedure TBaseArrayHelperTest.BinarySearchPosStaticSuccess20_3;
var
  sr: TSearchResult;
begin
  sr := TIntHelper.BinarySearchPos(IntDescSorted20, 10);
  AssertTrue(sr.FoundIndex = 4);
  AssertTrue(sr.InsertIndex = Succ(sr.FoundIndex));
end;

procedure TBaseArrayHelperTest.BinarySearchPosStaticSuccess20_4;
var
  sr: TSearchResult;
begin
  sr := TIntHelper.BinarySearchPos(IntDescSorted20, 2);
  AssertTrue(sr.FoundIndex = 6);
  AssertTrue(sr.InsertIndex = Succ(sr.FoundIndex));
end;

procedure TBaseArrayHelperTest.BinarySearchPosStaticSuccess20_5;
var
  sr: TSearchResult;
begin
  sr := TIntHelper.BinarySearchPos(IntDescSorted20, -17);
  AssertTrue(sr.FoundIndex = 14);
  AssertTrue(sr.InsertIndex = Succ(sr.FoundIndex));
end;

procedure TBaseArrayHelperTest.BinarySearchPosStaticSuccessEqual10;
var
  sr: TSearchResult;
begin
  sr := TIntHelper.BinarySearchPos(IntEqual10, 9);
  AssertTrue(sr.FoundIndex = 9);
  AssertTrue(sr.InsertIndex = Succ(sr.FoundIndex));
end;

procedure TBaseArrayHelperTest.BinarySearchPosDynEmpty;
var
  sr: TSearchResult;
begin
  sr := TIntHelper.BinarySearchPos([], 7);
  AssertTrue(sr.FoundIndex = -1);
  AssertTrue(sr.InsertIndex = 0);
end;

procedure TBaseArrayHelperTest.BinarySearchPosDynSingle;
var
  sr: TSearchResult;
begin
  sr := TIntHelper.BinarySearchPos([3], 7);
  AssertTrue(sr.FoundIndex = -1);
  AssertTrue(sr.InsertIndex = 1);
end;

procedure TBaseArrayHelperTest.BinarySearchPosDynSingle_1;
var
  sr: TSearchResult;
begin
  sr := TIntHelper.BinarySearchPos([3], 0);
  AssertTrue(sr.FoundIndex = -1);
  AssertTrue(sr.InsertIndex = 0);
end;

procedure TBaseArrayHelperTest.BinarySearchPosDynSingle_2;
var
  sr: TSearchResult;
begin
  sr := TIntHelper.BinarySearchPos([3], 3);
  AssertTrue(sr.FoundIndex = 0);
  AssertTrue(sr.InsertIndex = 1);
end;

procedure TBaseArrayHelperTest.BinarySearchPosStaticUnsuccess20;
var
  sr: TSearchResult;
begin
  sr := TIntHelper.BinarySearchPos(IntSorted20, 41);
  AssertTrue(sr.FoundIndex = -1);
  AssertTrue(sr.InsertIndex = 20);
end;

procedure TBaseArrayHelperTest.BinarySearchPosStaticUnsuccess20_1;
var
  sr: TSearchResult;
begin
  sr := TIntHelper.BinarySearchPos(InOrderSrc21, -1);
  AssertTrue(sr.FoundIndex = -1);
  AssertTrue(sr.InsertIndex = 0);
end;

procedure TBaseArrayHelperTest.BinarySearchPosStaticUnsuccess20_2;
var
  a: TIntArray;
  sr: TSearchResult;
begin
  a := TIntHelper.CreateReverseCopy(InOrderSrc21);
  sr := TIntHelper.BinarySearchPos(a, 21);
  AssertTrue(sr.FoundIndex = -1);
  AssertTrue(sr.InsertIndex = 0);
end;

procedure TBaseArrayHelperTest.BinarySearchPosStaticUnsuccess20_3;
var
  sr: TSearchResult;
begin
  sr := TIntHelper.BinarySearchPos(IntDescSorted20, 17);
  AssertTrue(sr.FoundIndex = -1);
  AssertTrue(sr.InsertIndex = 1);
end;

procedure TBaseArrayHelperTest.BinarySearchPosStaticUnsuccess20_4;
var
  sr: TSearchResult;
begin
  sr := TIntHelper.BinarySearchPos(IntDescSorted20, 60);
  AssertTrue(sr.FoundIndex = -1);
  AssertTrue(sr.InsertIndex = 0);
end;

procedure TBaseArrayHelperTest.BinarySearchPosStaticUnsuccess20_5;
var
  sr: TSearchResult;
begin
  sr := TIntHelper.BinarySearchPos(IntDescSorted20, -41);
  AssertTrue(sr.FoundIndex = -1);
  AssertTrue(sr.InsertIndex = 20);
end;

procedure TBaseArrayHelperTest.BinarySearchPosStaticUnuccessEqual10;
var
  sr: TSearchResult;
begin
  sr := TIntHelper.BinarySearchPos(IntEqual10, 5);
  AssertTrue(sr.FoundIndex = -1);
  AssertTrue(sr.InsertIndex = 0);
end;

procedure TBaseArrayHelperTest.BinarySearchPosStaticUnuccessEqual10_1;
var
  sr: TSearchResult;
begin
  sr := TIntHelper.BinarySearchPos(IntEqual10, 11);
  AssertTrue(sr.FoundIndex = -1);
  AssertTrue(sr.InsertIndex = 10);
end;

procedure TBaseArrayHelperTest.BinarySearchStaticUnsuccess20;
begin
  AssertTrue(TIntHelper.BinarySearch(IntSorted20, -8) = -1);
end;

procedure TBaseArrayHelperTest.BinarySearchStaticUnsuccess20_1;
begin
  AssertTrue(TIntHelper.BinarySearch(IntSorted20, 9) = -1);
end;

procedure TBaseArrayHelperTest.BinarySearchDynSuccess1;
begin
  AssertTrue(TIntHelper.BinarySearch(TIntHelper.CreateCopy(IntSrc10[3..3]), -15) = 0);
end;

procedure TBaseArrayHelperTest.BinarySearchDynUnsuccess1;
begin
  AssertTrue(TIntHelper.BinarySearch(TIntHelper.CreateCopy(IntSrc10[3..3]), 10) = -1);
end;

procedure TBaseArrayHelperTest.BinarySearchDynSuccess2;
begin
  AssertTrue(TIntHelper.BinarySearch(TIntHelper.CreateCopy(IntSorted10Uniq[3..4]), 3) = 1);
end;

procedure TBaseArrayHelperTest.BinarySearchDynUnsuccess2;
begin
  AssertTrue(TIntHelper.BinarySearch(TIntHelper.CreateCopy(IntSorted10Uniq[3..4]), 2) = -1);
end;

procedure TBaseArrayHelperTest.BinarySearchDynSuccess10Uniq;
begin
  AssertTrue(TIntHelper.BinarySearch(TIntHelper.CreateCopy(IntSorted10Uniq), 281) = 8);
end;

procedure TBaseArrayHelperTest.BinarySearchDynUnsuccess10Uniq;
begin
  AssertTrue(TIntHelper.BinarySearch(TIntHelper.CreateCopy(IntSorted10Uniq), 15) = -1);
end;

procedure TBaseArrayHelperTest.BinarySearchDynSuccess20;
var
  I: SizeInt;
begin
  I := TIntHelper.BinarySearch(TIntHelper.CreateCopy(IntSorted20), 2);
  AssertTrue((I = 5) or (I = 6));
end;

procedure TBaseArrayHelperTest.BinarySearchDynSuccess20_1;
begin
  AssertTrue(TIntHelper.BinarySearch(TIntHelper.CreateCopy(IntSorted20), 8) = 10);
end;

procedure TBaseArrayHelperTest.BinarySearchDynUnsuccess20;
begin
  AssertTrue(TIntHelper.BinarySearch(TIntHelper.CreateCopy(IntSorted20), -8) = -1);
end;

procedure TBaseArrayHelperTest.BinarySearchDynUnsuccess20_1;
begin
  AssertTrue(TIntHelper.BinarySearch(TIntHelper.CreateCopy(IntSorted20), 9) = -1);
end;

procedure TBaseArrayHelperTest.BinarySearchDynSuccess20_2;
begin
  AssertTrue(TIntHelper.BinarySearch(TIntHelper.CreateCopy(IntSorted20), -5) = 0);
end;

procedure TBaseArrayHelperTest.BinarySearchDynSuccess20_3;
begin
  AssertTrue(TIntHelper.BinarySearch(TIntHelper.CreateCopy(IntSorted20), 17) = 13);
end;

procedure TBaseArrayHelperTest.BinarySearchDynUnsuccessEqual10;
begin
  AssertTrue(TIntHelper.BinarySearch(TIntHelper.CreateCopy(IntEqual10), 5) = -1);
end;

procedure TBaseArrayHelperTest.BinarySearchDynSuccessEqual10;
begin
  AssertTrue(TIntHelper.BinarySearch(TIntHelper.CreateCopy(IntEqual10), 9) = 0);
end;

procedure TBaseArrayHelperTest.IndexOfMinEmpty;
var
  a: TIntArray;
begin
  AssertTrue(TIntHelper.IndexOfMin(a{%H-}) = -1);
end;

procedure TBaseArrayHelperTest.IndexOfMinStatic1;
begin
  AssertTrue(TIntHelper.IndexOfMin(IntSrc10[3..3]) = 0);
end;

procedure TBaseArrayHelperTest.IndexOfMinStatic2;
begin
  AssertTrue(TIntHelper.IndexOfMin(IntSrc10[3..4]) = 0);
end;

procedure TBaseArrayHelperTest.IndexOfMinStatic10;
begin
  AssertTrue(TIntHelper.IndexOfMin(IntSrc10) = 4);
end;

procedure TBaseArrayHelperTest.IndexOfMinDyn1;
begin
  AssertTrue(TIntHelper.IndexOfMin(TIntHelper.CreateCopy(IntSrc10[3..3])) = 0);
end;

procedure TBaseArrayHelperTest.IndexOfMinDyn2;
begin
  AssertTrue(TIntHelper.IndexOfMin(TIntHelper.CreateCopy(IntSrc10[3..4])) = 0);
end;

procedure TBaseArrayHelperTest.IndexOfMinDyn10;
begin
  AssertTrue(TIntHelper.IndexOfMin(TIntHelper.CreateCopy(IntSrc10)) = 4);
end;

procedure TBaseArrayHelperTest.IndexOfMaxEmpty;
var
  a: TIntArray;
begin
  AssertTrue(TIntHelper.IndexOfMax(a{%H-}) = -1);
end;

procedure TBaseArrayHelperTest.IndexOfMaxStatic1;
begin
  AssertTrue(TIntHelper.IndexOfMax(IntSrc10[3..3]) = 0);
end;

procedure TBaseArrayHelperTest.IndexOfMaxStatic2;
begin
  AssertTrue(TIntHelper.IndexOfMax(IntSrc10[3..4]) = 1);
end;

procedure TBaseArrayHelperTest.IndexOfMaxStatic10;
begin
  AssertTrue(TIntHelper.IndexOfMax(IntSrc10) = 2);
end;

procedure TBaseArrayHelperTest.IndexOfMaxDyn1;
begin
  AssertTrue(TIntHelper.IndexOfMax(TIntHelper.CreateCopy(IntSrc10[3..3])) = 0);
end;

procedure TBaseArrayHelperTest.IndexOfMaxDyn2;
begin
  AssertTrue(TIntHelper.IndexOfMax(TIntHelper.CreateCopy(IntSrc10[3..4])) = 1);
end;

procedure TBaseArrayHelperTest.IndexOfMaxDyn10;
begin
  AssertTrue(TIntHelper.IndexOfMax(TIntHelper.CreateCopy(IntSrc10)) = 2);
end;

procedure TBaseArrayHelperTest.GetMinOfEmpty;
var
  a: TIntArray;
begin
  AssertFalse(TIntHelper.GetMin(a{%H-}).Assigned);
end;

procedure TBaseArrayHelperTest.GetMinOfStatic1;
begin
  AssertTrue(TIntHelper.GetMin(IntSrc10[5..5]).Value = 21);
end;

procedure TBaseArrayHelperTest.GetMinOfStatic2;
begin
  AssertTrue(TIntHelper.GetMin(IntSrc10[7..8]).Value = 1);
end;

procedure TBaseArrayHelperTest.GetMinOfStatic10;
begin
  AssertTrue(TIntHelper.GetMin(IntSrc10).Value = -199);
end;

procedure TBaseArrayHelperTest.GetMinOfDyn1;
begin
  AssertTrue(TIntHelper.GetMin(TIntHelper.CreateCopy(IntSrc10[5..5])).Value = 21);
end;

procedure TBaseArrayHelperTest.GetMinOfDyn2;
begin
  AssertTrue(TIntHelper.GetMin(TIntHelper.CreateCopy(IntSrc10[7..8])).Value = 1);
end;

procedure TBaseArrayHelperTest.GetMinOfDyn10;
begin
  AssertTrue(TIntHelper.GetMin(TIntHelper.CreateCopy(IntSrc10)).Value = -199);
end;

procedure TBaseArrayHelperTest.GetMaxOfEmpty;
var
  a: TIntArray;
begin
  AssertFalse(TIntHelper.GetMax(a{%H-}).Assigned);
end;

procedure TBaseArrayHelperTest.GetMaxOfStatic1;
begin
  AssertTrue(TIntHelper.GetMax(IntSrc10[9..9]).Value = 0);
end;

procedure TBaseArrayHelperTest.GetMaxOfStatic2;
begin
  AssertTrue(TIntHelper.GetMax(IntSrc10[7..8]).Value = 13);
end;

procedure TBaseArrayHelperTest.GetMaxOfStatic10;
begin
  AssertTrue(TIntHelper.GetMax(IntSrc10).Value = 117);
end;

procedure TBaseArrayHelperTest.GetMaxOfDyn1;
begin
  AssertTrue(TIntHelper.GetMax(TIntHelper.CreateCopy(IntSrc10[9..9])).Value = 0);
end;

procedure TBaseArrayHelperTest.GetMaxOfDyn2;
begin
  AssertTrue(TIntHelper.GetMax(TIntHelper.CreateCopy(IntSrc10[7..8])).Value = 13);
end;

procedure TBaseArrayHelperTest.GetMaxOfDyn10;
begin
  AssertTrue(TIntHelper.GetMax(TIntHelper.CreateCopy(IntSrc10)).Value = 117);
end;

procedure TBaseArrayHelperTest.FindMinOfEmpty;
var
  a: TIntArray;
  m: Integer;
begin
  AssertFalse(TIntHelper.FindMin(a{%H-}, m));
end;

procedure TBaseArrayHelperTest.FindMinOfStatic1;
var
  m: Integer;
begin
  AssertTrue(TIntHelper.FindMin(IntSrc10[7..7], m));
  AssertTrue(m = 1);
end;

procedure TBaseArrayHelperTest.FindMinOfStatic2;
var
  m: Integer;
begin
  AssertTrue(TIntHelper.FindMin(IntSrc10[2..3], m));
  AssertTrue(m = -15);
end;

procedure TBaseArrayHelperTest.FindMinOfStatic10;
var
  m: Integer;
begin
  AssertTrue(TIntHelper.FindMin(IntSrc10, m));
  AssertTrue(m = -199);
end;

procedure TBaseArrayHelperTest.FindMinOfDyn1;
var
  m: Integer;
begin
  AssertTrue(TIntHelper.FindMin(TIntHelper.CreateCopy(IntSrc10[7..7]), m));
  AssertTrue(m = 1);
end;

procedure TBaseArrayHelperTest.FindMinOfDyn2;
var
  m: Integer;
begin
  AssertTrue(TIntHelper.FindMin(TIntHelper.CreateCopy(IntSrc10[2..3]), m));
  AssertTrue(m = -15);
end;

procedure TBaseArrayHelperTest.FindMinOfDyn10;
var
  m: Integer;
begin
  AssertTrue(TIntHelper.FindMin(TIntHelper.CreateCopy(IntSrc10), m));
  AssertTrue(m = -199);
end;

procedure TBaseArrayHelperTest.FindMaxOfEmpty;
var
  a: TIntArray;
  m: Integer;
begin
  AssertFalse(TIntHelper.FindMax(a{%H-}, m));
end;

procedure TBaseArrayHelperTest.FindMaxOfStatic1;
var
  m: Integer;
begin
  AssertTrue(TIntHelper.FindMax(IntSrc10[7..7], m));
  AssertTrue(m = 1);
end;

procedure TBaseArrayHelperTest.FindMaxOfStatic2;
var
  m: Integer;
begin
  AssertTrue(TIntHelper.FindMax(IntSrc10[2..3], m));
  AssertTrue(m = 10);
end;

procedure TBaseArrayHelperTest.FindMaxOfStatic10;
var
  m: Integer;
begin
  AssertTrue(TIntHelper.FindMax(IntSrc10, m));
  AssertTrue(m = 117);
end;

procedure TBaseArrayHelperTest.FindMaxOfDyn1;
var
  m: Integer;
begin
  AssertTrue(TIntHelper.FindMax(TIntHelper.CreateCopy(IntSrc10[7..7]), m));
  AssertTrue(m = 1);
end;

procedure TBaseArrayHelperTest.FindMaxOfDyn2;
var
  m: Integer;
begin
  AssertTrue(TIntHelper.FindMax(TIntHelper.CreateCopy(IntSrc10[2..3]), m));
  AssertTrue(m = 10);
end;

procedure TBaseArrayHelperTest.FindMaxOfDyn10;
var
  m: Integer;
begin
  AssertTrue(TIntHelper.FindMax(TIntHelper.CreateCopy(IntSrc10), m));
  AssertTrue(m = 117);
end;

procedure TBaseArrayHelperTest.FindMinMaxOfEmpty;
var
  a: TIntArray;
  mn, mx: Integer;
begin
  AssertFalse(TIntHelper.FindMinMax(a{%H-}, mn, mx));
end;

procedure TBaseArrayHelperTest.FindMinMaxOfStatic1;
var
  mn, mx: Integer;
begin
  AssertTrue(TIntHelper.FindMinMax(IntSrc10[7..7], mn, mx));
  AssertTrue(mn = 1);
  AssertTrue(mx = 1);
end;

procedure TBaseArrayHelperTest.FindMinMaxOfStatic2;
var
  mn, mx: Integer;
begin
  AssertTrue(TIntHelper.FindMinMax(IntSrc10[2..3], mn, mx));
  AssertTrue(mn = -15);
  AssertTrue(mx = 10);
end;

procedure TBaseArrayHelperTest.FindMinMaxOfStatic10;
var
  mn, mx: Integer;
begin
  AssertTrue(TIntHelper.FindMinMax(IntSrc10, mn, mx));
  AssertTrue(mn = -199);
  AssertTrue(mx = 117);
end;

procedure TBaseArrayHelperTest.FindMinMaxOfDyn1;
var
  mn, mx: Integer;
begin
  AssertTrue(TIntHelper.FindMinMax(TIntHelper.CreateCopy(IntSrc10[7..7]), mn, mx));
  AssertTrue(mn = 1);
  AssertTrue(mx = 1);
end;

procedure TBaseArrayHelperTest.FindMinMaxOfDyn2;
var
  mn, mx: Integer;
begin
  AssertTrue(TIntHelper.FindMinMax(TIntHelper.CreateCopy(IntSrc10[2..3]), mn, mx));
  AssertTrue(mn = -15);
  AssertTrue(mx = 10);
end;

procedure TBaseArrayHelperTest.FindMinMaxOfDyn10;
var
  mn, mx: Integer;
begin
  AssertTrue(TIntHelper.FindMinMax(TIntHelper.CreateCopy(IntSrc10), mn, mx));
  AssertTrue(mn = -199);
  AssertTrue(mx = 117);
end;

procedure TBaseArrayHelperTest.FindNthSmallestOfEmpty;
var
  a: TIntArray;
  v: Integer;
begin
  AssertFalse(TIntHelper.FindNthSmallest(a{%H-}, 1, v));
end;

procedure TBaseArrayHelperTest.FindNthSmallestOfStatic1;
var
  v: Integer;
begin
  AssertTrue(TIntHelper.FindNthSmallest(InOrderSrc21[3..3], 0, v));
  AssertTrue(v = 3);
end;

procedure TBaseArrayHelperTest.FindNthSmallestOfStatic1_1;
var
  v: Integer;
begin
  AssertTrue(TIntHelper.FindNthSmallest(InOrderSrc21[5..5], -1, v));
  AssertTrue(v = 5);
end;

procedure TBaseArrayHelperTest.FindNthSmallestOfStatic1_2;
var
  v: Integer;
begin
  AssertTrue(TIntHelper.FindNthSmallest(InOrderSrc21[2..2], 2, v));
  AssertTrue(v = 2);
end;

procedure TBaseArrayHelperTest.FindNthSmallestOfStatic21;
var
  v: Integer;
  a: TIntArray21;
begin
  a := InOrderSrc21;
  TIntHelper.RandomShuffle(a);
  AssertTrue(TIntHelper.FindNthSmallest(a, 0, v));
  AssertTrue(v = 0);
end;

procedure TBaseArrayHelperTest.FindNthSmallestOfStatic21_1;
var
  v: Integer;
  a: TIntArray21;
begin
  a := InOrderSrc21;
  TIntHelper.RandomShuffle(a);
  AssertTrue(TIntHelper.FindNthSmallest(a, -15, v));
  AssertTrue(v = 0);
end;

procedure TBaseArrayHelperTest.FindNthSmallestOfStatic21_2;
var
  v: Integer;
  a: TIntArray21;
begin
  a := InOrderSrc21;
  TIntHelper.RandomShuffle(a);
  AssertTrue(TIntHelper.FindNthSmallest(a, 20, v));
  AssertTrue(v = 20);
end;

procedure TBaseArrayHelperTest.FindNthSmallestOfStatic21_3;
var
  v: Integer;
  a: TIntArray21;
begin
  a := InOrderSrc21;
  TIntHelper.RandomShuffle(a);
  AssertTrue(TIntHelper.FindNthSmallest(a, 22, v));
  AssertTrue(v = 20);
end;

procedure TBaseArrayHelperTest.FindNthSmallestOfStatic21_4;
var
  v: Integer;
  a: TIntArray21;
begin
  a := InOrderSrc21;
  TIntHelper.RandomShuffle(a);
  AssertTrue(TIntHelper.FindNthSmallest(a, 11, v));
  AssertTrue(v = 11);
end;

procedure TBaseArrayHelperTest.FindNthSmallestOfStatic21_5;
var
  v: Integer;
  a: TIntArray21;
begin
  a := InOrderSrc21;
  TIntHelper.RandomShuffle(a);
  AssertTrue(TIntHelper.FindNthSmallest(a, 1, v));
  AssertTrue(v = 1);
end;

procedure TBaseArrayHelperTest.FindNthSmallestOfStatic21_6;
var
  v: Integer;
  a: TIntArray21;
begin
  a := InOrderSrc21;
  TIntHelper.RandomShuffle(a);
  AssertTrue(TIntHelper.FindNthSmallest(a, 19, v));
  AssertTrue(v = 19);
end;

procedure TBaseArrayHelperTest.FindNthSmallestOfStatic21_7;
var
  v: Integer;
  a: TIntArray21;
begin
  a := InOrderDblSrc21;
  TIntHelper.RandomShuffle(a);
  AssertTrue(TIntHelper.FindNthSmallest(a, 10, v));
  AssertTrue(v = 5);
end;

procedure TBaseArrayHelperTest.FindNthSmallestOfStatic21_8;
var
  v: Integer;
  a: TIntArray21;
begin
  a := InOrderDblSrc21;
  TIntHelper.RandomShuffle(a);
  AssertTrue(TIntHelper.FindNthSmallest(a, 11, v));
  AssertTrue(v = 5);
end;

procedure TBaseArrayHelperTest.FindNthSmallestOfDyn1;
var
  v: Integer;
begin
  AssertTrue(TIntHelper.FindNthSmallest(TIntHelper.CreateCopy(InOrderSrc21[6..6]), 0, v));
  AssertTrue(v = 6);
end;

procedure TBaseArrayHelperTest.FindNthSmallestOfDyn1_1;
var
  v: Integer;
begin
  AssertTrue(TIntHelper.FindNthSmallest(TIntHelper.CreateCopy(InOrderSrc21[6..6]), -3, v));
  AssertTrue(v = 6);
end;

procedure TBaseArrayHelperTest.FindNthSmallestOfDyn1_2;
var
  v: Integer;
begin
  AssertTrue(TIntHelper.FindNthSmallest(TIntHelper.CreateCopy(InOrderSrc21[6..6]), 4, v));
  AssertTrue(v = 6);
end;

procedure TBaseArrayHelperTest.FindNthSmallestOfDyn21;
var
  v: Integer;
  a: TIntArray;
begin
  a := TIntHelper.CreateRandomShuffle(InOrderSrc21);
  AssertTrue(TIntHelper.FindNthSmallest(a, 0, v));
  AssertTrue(v = 0);
end;

procedure TBaseArrayHelperTest.FindNthSmallestOfDyn21_1;
var
  v: Integer;
  a: TIntArray;
begin
  a := TIntHelper.CreateRandomShuffle(InOrderSrc21);
  AssertTrue(TIntHelper.FindNthSmallest(a, -3, v));
  AssertTrue(v = 0);
end;

procedure TBaseArrayHelperTest.FindNthSmallestOfDyn21_2;
var
  v: Integer;
  a: TIntArray;
begin
  a := TIntHelper.CreateRandomShuffle(InOrderSrc21);
  AssertTrue(TIntHelper.FindNthSmallest(a, 20, v));
  AssertTrue(v = 20);
end;

procedure TBaseArrayHelperTest.FindNthSmallestOfDyn21_3;
var
  v: Integer;
  a: TIntArray;
begin
  a := TIntHelper.CreateRandomShuffle(InOrderSrc21);
  AssertTrue(TIntHelper.FindNthSmallest(a, 22, v));
  AssertTrue(v = 20);
end;

procedure TBaseArrayHelperTest.FindNthSmallestOfDyn21_4;
var
  v: Integer;
  a: TIntArray;
begin
  a := TIntHelper.CreateRandomShuffle(InOrderSrc21);
  AssertTrue(TIntHelper.FindNthSmallest(a, 14, v));
  AssertTrue(v = 14);
end;

procedure TBaseArrayHelperTest.FindNthSmallestOfDyn21_5;
var
  v: Integer;
  a: TIntArray;
begin
  a := TIntHelper.CreateRandomShuffle(InOrderSrc21);
  AssertTrue(TIntHelper.FindNthSmallest(a, 1, v));
  AssertTrue(v = 1);
end;

procedure TBaseArrayHelperTest.FindNthSmallestOfDyn21_6;
var
  v: Integer;
  a: TIntArray;
begin
  a := TIntHelper.CreateRandomShuffle(InOrderSrc21);
  AssertTrue(TIntHelper.FindNthSmallest(a, 19, v));
  AssertTrue(v = 19);
end;

procedure TBaseArrayHelperTest.FindNthSmallestOfDyn21_7;
var
  v: Integer;
  a: TIntArray;
begin
  a := TIntHelper.CreateRandomShuffle(InOrderDblSrc21);
  AssertTrue(TIntHelper.FindNthSmallest(a, 14, v));
  AssertTrue(v = 7);
end;

procedure TBaseArrayHelperTest.FindNthSmallestOfDyn21_8;
var
  v: Integer;
  a: TIntArray;
begin
  a := TIntHelper.CreateRandomShuffle(InOrderDblSrc21);
  AssertTrue(TIntHelper.FindNthSmallest(a, 15, v));
  AssertTrue(v = 7);
end;

procedure TBaseArrayHelperTest.NthSmallestOfEmpty;
var
  a: TIntArray;
begin
  AssertFalse(TIntHelper.NthSmallest(a{%H-}, 1).Assigned);
end;

procedure TBaseArrayHelperTest.NthSmallestOfStatic1;
begin
  AssertTrue(TIntHelper.NthSmallest(InOrderSrc21[3..3], 0).Value = 3);
end;

procedure TBaseArrayHelperTest.NthSmallestOfStatic1_1;
begin
  AssertTrue(TIntHelper.NthSmallest(InOrderSrc21[5..5], -1).Value = 5);
end;

procedure TBaseArrayHelperTest.NthSmallestOfStatic1_2;
begin
  AssertTrue(TIntHelper.NthSmallest(InOrderSrc21[2..2], 2).Value = 2);
end;

procedure TBaseArrayHelperTest.NthSmallestOfStatic21;
var
  a: TIntArray21;
begin
  a := InOrderSrc21;
  TIntHelper.RandomShuffle(a);
  AssertTrue(TIntHelper.NthSmallest(a, 0).Value = 0);
end;

procedure TBaseArrayHelperTest.NthSmallestOfStatic21_1;
var
  a: TIntArray21;
begin
  a := InOrderSrc21;
  TIntHelper.RandomShuffle(a);
  AssertTrue(TIntHelper.NthSmallest(a, -15).Value = 0);
end;

procedure TBaseArrayHelperTest.NthSmallestOfStatic21_2;
var
  a: TIntArray21;
begin
  a := InOrderSrc21;
  TIntHelper.RandomShuffle(a);
  AssertTrue(TIntHelper.NthSmallest(a, 20).Value = 20);
end;

procedure TBaseArrayHelperTest.NthSmallestOfStatic21_3;
var
  a: TIntArray21;
begin
  a := InOrderSrc21;
  TIntHelper.RandomShuffle(a);
  AssertTrue(TIntHelper.NthSmallest(a, 22).Value = 20);
end;

procedure TBaseArrayHelperTest.NthSmallestOfStatic21_4;
var
  a: TIntArray21;
begin
  a := InOrderSrc21;
  TIntHelper.RandomShuffle(a);
  AssertTrue(TIntHelper.NthSmallest(a, 11).Value = 11);
end;

procedure TBaseArrayHelperTest.NthSmallestOfStatic21_5;
var
  a: TIntArray21;
begin
  a := InOrderSrc21;
  TIntHelper.RandomShuffle(a);
  AssertTrue(TIntHelper.NthSmallest(a, 1).Value = 1);
end;

procedure TBaseArrayHelperTest.NthSmallestOfStatic21_6;
var
  a: TIntArray21;
begin
  a := InOrderSrc21;
  TIntHelper.RandomShuffle(a);
  AssertTrue(TIntHelper.NthSmallest(a, 19).Value = 19);
end;

procedure TBaseArrayHelperTest.NthSmallestOfStatic21_7;
var
  a: TIntArray21;
begin
  a := InOrderDblSrc21;
  TIntHelper.RandomShuffle(a);
  AssertTrue(TIntHelper.NthSmallest(a, 10).Value = 5);
end;

procedure TBaseArrayHelperTest.NthSmallestOfStatic21_8;
var
  a: TIntArray21;
begin
  a := InOrderDblSrc21;
  TIntHelper.RandomShuffle(a);
  AssertTrue(TIntHelper.NthSmallest(a, 11).Value = 5);
end;

procedure TBaseArrayHelperTest.NthSmallestOfDyn1;
begin
  AssertTrue(TIntHelper.NthSmallest(TIntHelper.CreateCopy(InOrderSrc21[6..6]), 0).Value = 6);
end;

procedure TBaseArrayHelperTest.NthSmallestOfDyn1_1;
begin
  AssertTrue(TIntHelper.NthSmallest(TIntHelper.CreateCopy(InOrderSrc21[6..6]), -3).Value = 6);
end;

procedure TBaseArrayHelperTest.NthSmallestOfDyn1_2;
begin
  AssertTrue(TIntHelper.NthSmallest(TIntHelper.CreateCopy(InOrderSrc21[6..6]), 4).Value = 6);
end;

procedure TBaseArrayHelperTest.NthSmallestOfDyn21;
var
  a: TIntArray;
begin
  a := TIntHelper.CreateRandomShuffle(InOrderSrc21);
  AssertTrue(TIntHelper.NthSmallest(a, 0).Value = 0);
end;

procedure TBaseArrayHelperTest.NthSmallestOfDyn21_1;
var
  a: TIntArray;
begin
  a := TIntHelper.CreateRandomShuffle(InOrderSrc21);
  AssertTrue(TIntHelper.NthSmallest(a, -3).Value = 0);
end;

procedure TBaseArrayHelperTest.NthSmallestOfDyn21_2;
var
  a: TIntArray;
begin
  a := TIntHelper.CreateRandomShuffle(InOrderSrc21);
  AssertTrue(TIntHelper.NthSmallest(a, 20).Value = 20);
end;

procedure TBaseArrayHelperTest.NthSmallestOfDyn21_3;
var
  a: TIntArray;
begin
  a := TIntHelper.CreateRandomShuffle(InOrderSrc21);
  AssertTrue(TIntHelper.NthSmallest(a, 23).Value = 20);
end;

procedure TBaseArrayHelperTest.NthSmallestOfDyn21_4;
var
  a: TIntArray;
begin
  a := TIntHelper.CreateRandomShuffle(InOrderSrc21);
  AssertTrue(TIntHelper.NthSmallest(a, 14).Value = 14);
end;

procedure TBaseArrayHelperTest.NthSmallestOfDyn21_5;
var
  a: TIntArray;
begin
  a := TIntHelper.CreateRandomShuffle(InOrderSrc21);
  AssertTrue(TIntHelper.NthSmallest(a, 1).Value = 1);
end;

procedure TBaseArrayHelperTest.NthSmallestOfDyn21_6;
var
  a: TIntArray;
begin
  a := TIntHelper.CreateRandomShuffle(InOrderSrc21);
  AssertTrue(TIntHelper.NthSmallest(a, 19).Value = 19);
end;

procedure TBaseArrayHelperTest.NthSmallestOfDyn21_7;
var
  a: TIntArray;
begin
  a := TIntHelper.CreateRandomShuffle(InOrderDblSrc21);
  AssertTrue(TIntHelper.NthSmallest(a, 14).Value = 7);
end;

procedure TBaseArrayHelperTest.NthSmallestOfDyn21_8;
var
  a: TIntArray;
begin
  a := TIntHelper.CreateRandomShuffle(InOrderDblSrc21);
  AssertTrue(TIntHelper.NthSmallest(a, 15).Value = 7);
end;

procedure TBaseArrayHelperTest.FindNthSmallestNDOfStatic21;
var
  a, b: TIntArray21;
  v: Integer;
begin
  a := InOrderSrc21;
  TIntHelper.RandomShuffle(a);
  b := a;
  AssertTrue(TIntHelper.FindNthSmallestND(a, 8, v));
  AssertTrue(v = 8);
  AssertTrue(TIntHelper.Same(a, b));
end;

procedure TBaseArrayHelperTest.FindNthSmallestNDOfStatic21_1;
var
  a, b: TIntArray21;
  v: Integer;
begin
  a := InOrderSrc21;
  TIntHelper.RandomShuffle(a);
  b := a;
  AssertTrue(TIntHelper.FindNthSmallestND(a, 11, v));
  AssertTrue(v = 11);
  AssertTrue(TIntHelper.Same(a, b));
end;

procedure TBaseArrayHelperTest.FindNthSmallestNDOfDyn21;
var
  a, b: TIntArray;
  v: Integer;
begin
  a := TIntHelper.CreateRandomShuffle(InOrderSrc21);
  b := TIntHelper.CreateCopy(a);
  AssertTrue(TIntHelper.FindNthSmallestND(a, 7, v));
  AssertTrue(v = 7);
  AssertTrue(TIntHelper.Same(a, b));
end;

procedure TBaseArrayHelperTest.FindNthSmallestNDOfDyn21_1;
var
  a, b: TIntArray;
  v: Integer;
begin
  a := TIntHelper.CreateRandomShuffle(InOrderSrc21);
  b := TIntHelper.CreateCopy(a);
  AssertTrue(TIntHelper.FindNthSmallestND(a, 12, v));
  AssertTrue(v = 12);
  AssertTrue(TIntHelper.Same(a, b));
end;

procedure TBaseArrayHelperTest.NthSmallestNDOfStatic21;
var
  a, b: TIntArray21;
begin
  a := InOrderSrc21;
  TIntHelper.RandomShuffle(a);
  b := a;
  AssertTrue(TIntHelper.NthSmallestND(a, 9).Value = 9);
  AssertTrue(TIntHelper.Same(a, b));
end;

procedure TBaseArrayHelperTest.NthSmallestNDOfStatic21_1;
var
  a, b: TIntArray21;
begin
  a := InOrderSrc21;
  TIntHelper.RandomShuffle(a);
  b := a;
  AssertTrue(TIntHelper.NthSmallestND(a, 13).Value = 13);
  AssertTrue(TIntHelper.Same(a, b));
end;

procedure TBaseArrayHelperTest.NthSmallestNDOfDyn21;
var
  a, b: TIntArray;
begin
  a := TIntHelper.CreateRandomShuffle(InOrderSrc21);
  b := TIntHelper.CreateCopy(a);
  AssertTrue(TIntHelper.NthSmallestND(a, 5).Value = 5);
  AssertTrue(TIntHelper.Same(a, b));
end;

procedure TBaseArrayHelperTest.NthSmallestNDOfDyn21_1;
var
  a, b: TIntArray;
begin
  a := TIntHelper.CreateRandomShuffle(InOrderSrc21);
  b := TIntHelper.CreateCopy(a);
  AssertTrue(TIntHelper.NthSmallestND(a, 15).Value = 15);
  AssertTrue(TIntHelper.Same(a, b));
end;

procedure TBaseArrayHelperTest.NextPermutation2AscOfEmpty;
var
  a: TIntArray;
begin
  AssertFalse(TIntHelper.NextPermutation2Asc(a{%H-}));
end;

procedure TBaseArrayHelperTest.NextPermutation2AscOfStatic1;
var
  a: array[0..0] of Integer = (10);
begin
  AssertFalse(TIntHelper.NextPermutation2Asc(a));
end;

procedure TBaseArrayHelperTest.NextPermutation2AscOfStatic2Asc;
var
  a: array[0..1] of Integer = (5, 8);
begin
  AssertFalse(TIntHelper.NextPermutation2Asc(a));
end;

procedure TBaseArrayHelperTest.NextPermutation2AscOfStatic2Desc;
const
  c: array[0..1] of Integer = (3, 11);
var
  a: array[0..1] of Integer = (11, 3);
begin
  AssertTrue(TIntHelper.NextPermutation2Asc(a));
  AssertTrue(TIntHelper.Same(a, c));
end;

procedure TBaseArrayHelperTest.NextPermutation2AscOfStatic10Equal;
var
  a: TIntArray10;
begin
  a := IntEqual10;
  AssertFalse(TIntHelper.NextPermutation2Asc(a));
end;

procedure TBaseArrayHelperTest.NextPermutation2AscOfStatic3;
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
  AssertTrue(TIntHelper.NextPermutation2Asc(a));
  AssertTrue(TIntHelper.Same(a, c2));
  AssertTrue(TIntHelper.NextPermutation2Asc(a));
  AssertTrue(TIntHelper.Same(a, c3));
  AssertTrue(TIntHelper.NextPermutation2Asc(a));
  AssertTrue(TIntHelper.Same(a, c4));
  AssertTrue(TIntHelper.NextPermutation2Asc(a));
  AssertTrue(TIntHelper.Same(a, c5));
  AssertTrue(TIntHelper.NextPermutation2Asc(a));
  AssertTrue(TIntHelper.Same(a, c6));
  AssertFalse(TIntHelper.NextPermutation2Asc(a));
end;

procedure TBaseArrayHelperTest.NextPermutation2AscOfStatic4;
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
  while TIntHelper.NextPermutation2Asc(a) do
    Inc(PermCount);
  AssertTrue(TIntHelper.Same(a, c1));
  AssertTrue(PermCount = 24);
end;

procedure TBaseArrayHelperTest.NextPermutation2AscOfDyn1;
begin
  AssertFalse(TIntHelper.NextPermutation2Asc(TIntArray.Create(25)));
end;

procedure TBaseArrayHelperTest.NextPermutation2AscOfDyn2Asc;
begin
  AssertFalse(TIntHelper.NextPermutation2Asc(TIntArray.Create(13, 17)));
end;

procedure TBaseArrayHelperTest.NextPermutation2AscOfDyn2Desc;
begin
  AssertTrue(TIntHelper.NextPermutation2Asc(TIntArray.Create(21, 11)));
end;

procedure TBaseArrayHelperTest.NextPermutation2AscOfDyn10Equal;
begin
  AssertFalse(TIntHelper.NextPermutation2Asc(TIntHelper.CreateCopy(IntEqual10)));
end;

procedure TBaseArrayHelperTest.NextPermutation2AscOfDyn3;
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
  AssertTrue(TIntHelper.NextPermutation2Asc(a));
  AssertTrue(TIntHelper.Same(a, c2));
  AssertTrue(TIntHelper.NextPermutation2Asc(a));
  AssertTrue(TIntHelper.Same(a, c3));
  AssertTrue(TIntHelper.NextPermutation2Asc(a));
  AssertTrue(TIntHelper.Same(a, c4));
  AssertTrue(TIntHelper.NextPermutation2Asc(a));
  AssertTrue(TIntHelper.Same(a, c5));
  AssertTrue(TIntHelper.NextPermutation2Asc(a));
  AssertTrue(TIntHelper.Same(a, c6));
  AssertFalse(TIntHelper.NextPermutation2Asc(a));
end;

procedure TBaseArrayHelperTest.NextPermutation2AscOfDyn4;
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
  while TIntHelper.NextPermutation2Asc(a) do
    Inc(PermCount);
  AssertTrue(TIntHelper.Same(a, c1));
  AssertTrue(PermCount = 24);
end;

procedure TBaseArrayHelperTest.NextPermutation2DescOfEmpty;
var
  a: TIntArray;
begin
  AssertFalse(TIntHelper.NextPermutation2Desc(a{%H-}));
end;

procedure TBaseArrayHelperTest.NextPermutation2DescOfStatic1;
var
  a: array[0..0] of Integer = (10);
begin
  AssertFalse(TIntHelper.NextPermutation2Desc(a));
end;

procedure TBaseArrayHelperTest.NextPermutation2DescOfStatic2Asc;
const
  c: array[0..1] of Integer = (8, 5);
var
  a: array[0..1] of Integer = (5, 8);
begin
  AssertTrue(TIntHelper.NextPermutation2Desc(a));
  AssertTrue(TIntHelper.Same(a, c));
end;

procedure TBaseArrayHelperTest.NextPermutation2DescOfStatic2Desc;
var
  a: array[0..1] of Integer = (11, 3);
begin
  AssertFalse(TIntHelper.NextPermutation2Desc(a));
end;

procedure TBaseArrayHelperTest.NextPermutation2DescOfStatic10Equal;
var
  a: TIntArray10;
begin
  a := IntEqual10;
  AssertFalse(TIntHelper.NextPermutation2Desc(a));
end;

procedure TBaseArrayHelperTest.NextPermutation2DescOfStatic3;
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
  AssertTrue(TIntHelper.NextPermutation2Desc(a));
  AssertTrue(TIntHelper.Same(a, c2));
  AssertTrue(TIntHelper.NextPermutation2Desc(a));
  AssertTrue(TIntHelper.Same(a, c3));
  AssertTrue(TIntHelper.NextPermutation2Desc(a));
  AssertTrue(TIntHelper.Same(a, c4));
  AssertTrue(TIntHelper.NextPermutation2Desc(a));
  AssertTrue(TIntHelper.Same(a, c5));
  AssertTrue(TIntHelper.NextPermutation2Desc(a));
  AssertTrue(TIntHelper.Same(a, c6));
  AssertFalse(TIntHelper.NextPermutation2Desc(a));
end;

procedure TBaseArrayHelperTest.NextPermutation2DescOfStatic4;
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
  while TIntHelper.NextPermutation2Desc(a) do
    Inc(PermCount);
  AssertTrue(TIntHelper.Same(a, c1));
  AssertTrue(PermCount = 24);
end;

procedure TBaseArrayHelperTest.NextPermutation2DescOfDyn1;
begin
  AssertFalse(TIntHelper.NextPermutation2Desc(TIntArray.Create(10)));
end;

procedure TBaseArrayHelperTest.NextPermutation2DescOfDyn2Asc;
const
  c: array[0..1] of Integer = (8, 5);
var
  a: TIntArray;
begin
  a := TIntArray.Create(5, 8);
  AssertTrue(TIntHelper.NextPermutation2Desc(a));
  AssertTrue(TIntHelper.Same(a, c));
end;

procedure TBaseArrayHelperTest.NextPermutation2DescOfDyn2Desc;
begin
  AssertFalse(TIntHelper.NextPermutation2Desc(TIntArray.Create(11, 3)));
end;

procedure TBaseArrayHelperTest.NextPermutation2DescOfDyn10Equal;
begin
  AssertFalse(TIntHelper.NextPermutation2Desc(TIntHelper.CreateCopy(IntEqual10)));
end;

procedure TBaseArrayHelperTest.NextPermutation2DescOfDyn3;
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
  AssertTrue(TIntHelper.NextPermutation2Desc(a));
  AssertTrue(TIntHelper.Same(a, c2));
  AssertTrue(TIntHelper.NextPermutation2Desc(a));
  AssertTrue(TIntHelper.Same(a, c3));
  AssertTrue(TIntHelper.NextPermutation2Desc(a));
  AssertTrue(TIntHelper.Same(a, c4));
  AssertTrue(TIntHelper.NextPermutation2Desc(a));
  AssertTrue(TIntHelper.Same(a, c5));
  AssertTrue(TIntHelper.NextPermutation2Desc(a));
  AssertTrue(TIntHelper.Same(a, c6));
  AssertFalse(TIntHelper.NextPermutation2Desc(a));
end;

procedure TBaseArrayHelperTest.NextPermutation2DescOfDyn4;
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
  while TIntHelper.NextPermutation2Desc(a) do
    Inc(PermCount);
  AssertTrue(TIntHelper.Same(a, c1));
  AssertTrue(PermCount = 24);
end;

procedure TBaseArrayHelperTest.IsNonDescendingOfEmpty;
var
  a: TIntArray;
begin
  AssertTrue(TIntHelper.IsNonDescending(a{%H-}));
end;

procedure TBaseArrayHelperTest.IsNonDescendingOfStatic1;
begin
  AssertTrue(TIntHelper.IsNonDescending(IntSrc10[4..4]));
end;

procedure TBaseArrayHelperTest.IsNonDescendingOfStatic10Equal;
begin
  AssertTrue(TIntHelper.IsNonDescending(IntEqual10));
end;

procedure TBaseArrayHelperTest.IsNonDescendingOfStatic21Success;
begin
  AssertTrue(TIntHelper.IsNonDescending(InOrderSrc21));
end;

procedure TBaseArrayHelperTest.IsNonDescendingOfStatic21Success_1;
begin
  AssertTrue(TIntHelper.IsNonDescending(InOrderDblSrc21));
end;

procedure TBaseArrayHelperTest.IsNonDescendingOfStatic21Unsuccess;
var
  a: TIntArray21;
begin
  a := InOrderSrc21;
  TIntHelper.Reverse(a[4..5]);
  AssertFalse(TIntHelper.IsNonDescending(a));
end;

procedure TBaseArrayHelperTest.IsNonDescendingOfStatic21Unsuccess_1;
var
  a: TIntArray21;
begin
  a := InOrderDblSrc21;
  TIntHelper.Reverse(a[5..7]);
  AssertFalse(TIntHelper.IsNonDescending(a));
end;

procedure TBaseArrayHelperTest.IsNonDescendingOfDyn1;
begin
  AssertTrue(TIntHelper.IsNonDescending(TIntArray.Create(111)));
end;

procedure TBaseArrayHelperTest.IsNonDescendingOfDyn10Equal;
begin
  AssertTrue(TIntHelper.IsNonDescending(TIntHelper.CreateCopy(IntEqual10)));
end;

procedure TBaseArrayHelperTest.IsNonDescendingOfDyn21Success;
begin
  AssertTrue(TIntHelper.IsNonDescending(TIntHelper.CreateCopy(InOrderSrc21)));
end;

procedure TBaseArrayHelperTest.IsNonDescendingOfDyn21Success_1;
begin
  AssertTrue(TIntHelper.IsNonDescending(TIntHelper.CreateCopy(InOrderDblSrc21)));
end;

procedure TBaseArrayHelperTest.IsNonDescendingOfDyn21Unsuccess;
var
  a: TIntArray;
begin
  a := TIntHelper.CreateCopy(InOrderSrc21);
  TIntHelper.Reverse(a[5..6]);
  AssertFalse(TIntHelper.IsNonDescending(a));
end;

procedure TBaseArrayHelperTest.IsNonDescendingOfDyn21Unsuccess_1;
var
  a: TIntArray;
begin
  a := TIntHelper.CreateCopy(InOrderDblSrc21);
  TIntHelper.Reverse(a[6..8]);
  AssertFalse(TIntHelper.IsNonDescending(a));
end;

procedure TBaseArrayHelperTest.IsStrictAscendingOfEmpty;
var
  a: TIntArray;
begin
  AssertFalse(TIntHelper.IsStrictAscending(a{%H-}));
end;

procedure TBaseArrayHelperTest.IsStrictAscendingOfStatic1;
begin
  AssertFalse(TIntHelper.IsStrictAscending(IntSrc10[3..3]));
end;

procedure TBaseArrayHelperTest.IsStrictAscendingOfStatic10Equal;
begin
  AssertFalse(TIntHelper.IsStrictAscending(IntEqual10));
end;

procedure TBaseArrayHelperTest.IsStrictAscendingOfStatic21Success;
begin
  AssertTrue(TIntHelper.IsStrictAscending(InOrderSrc21));
end;

procedure TBaseArrayHelperTest.IsStrictAscendingOfStatic21Unsuccess;
begin
  AssertFalse(TIntHelper.IsStrictAscending(InOrderDblSrc21));
end;

procedure TBaseArrayHelperTest.IsStrictAscendingOfDyn1;
begin
  AssertFalse(TIntHelper.IsStrictAscending(TIntArray.Create(14)));
end;

procedure TBaseArrayHelperTest.IsStrictAscendingOfDyn10Equal;
begin
  AssertFalse(TIntHelper.IsStrictAscending(TIntHelper.CreateCopy(IntEqual10)));
end;

procedure TBaseArrayHelperTest.IsStrictAscendingOfDyn21Success;
begin
   AssertTrue(TIntHelper.IsStrictAscending(TIntHelper.CreateCopy(InOrderSrc21)));
end;

procedure TBaseArrayHelperTest.IsStrictAscendingOfDyn21Unsuccess;
begin
  AssertFalse(TIntHelper.IsStrictAscending(TIntHelper.CreateCopy(InOrderDblSrc21)));
end;

procedure TBaseArrayHelperTest.IsNonAscendingOfEmpty;
var
  a: TIntArray;
begin
  AssertTrue(TIntHelper.IsNonAscending(a{%H-}));
end;

procedure TBaseArrayHelperTest.IsNonAscendingOfStatic1;
begin
  AssertTrue(TIntHelper.IsNonAscending(IntSrc10[9..9]));
end;

procedure TBaseArrayHelperTest.IsNonAscendingOfStatic10Equal;
begin
  AssertTrue(TIntHelper.IsNonAscending(IntEqual10));
end;

procedure TBaseArrayHelperTest.IsNonAscendingOfStatic21Success;
var
  a: TIntArray21;
begin
  a := InOrderSrc21;
  TIntHelper.Reverse(a);
  AssertTrue(TIntHelper.IsNonAscending(a));
end;

procedure TBaseArrayHelperTest.IsNonAscendingOfStatic21Success_1;
var
  a: TIntArray21;
begin
  a := InOrderDblSrc21;
  TIntHelper.Reverse(a);
  AssertTrue(TIntHelper.IsNonAscending(a));
end;

procedure TBaseArrayHelperTest.IsNonAscendingOfStatic21Unsuccess;
var
  a: TIntArray21;
begin
  a := InOrderSrc21;
  AssertFalse(TIntHelper.IsNonAscending(a));
end;

procedure TBaseArrayHelperTest.IsNonAscendingOfStatic21Unsuccess_1;
var
  a: TIntArray21;
begin
  a := InOrderDblSrc21;
  AssertFalse(TIntHelper.IsNonAscending(a));
end;

procedure TBaseArrayHelperTest.IsNonAscendingOfDyn1;
begin
  AssertTrue(TIntHelper.IsNonAscending(TIntArray.Create(21)));
end;

procedure TBaseArrayHelperTest.IsNonAscendingOfDyn10Equal;
begin
  AssertTrue(TIntHelper.IsNonAscending(TIntHelper.CreateCopy(IntEqual10)));
end;

procedure TBaseArrayHelperTest.IsNonAscendingOfDyn21Success;
begin
  AssertTrue(TIntHelper.IsNonAscending(TIntHelper.CreateReverseCopy(InOrderSrc21)));
end;

procedure TBaseArrayHelperTest.IsNonAscendingOfDyn21Success_1;
begin
  AssertTrue(TIntHelper.IsNonAscending(TIntHelper.CreateReverseCopy(InOrderDblSrc21)));
end;

procedure TBaseArrayHelperTest.IsNonAscendingOfDyn21Unsuccess;
begin
  AssertFalse(TIntHelper.IsNonAscending(TIntHelper.CreateCopy(InOrderSrc21)));
end;

procedure TBaseArrayHelperTest.IsNonAscendingOfDyn21Unsuccess_1;
begin
  AssertFalse(TIntHelper.IsNonAscending(TIntHelper.CreateCopy(InOrderDblSrc21)));
end;

procedure TBaseArrayHelperTest.IsStrictDescendingOfEmpty;
var
  a: TIntArray;
begin
  AssertFalse(TIntHelper.IsStrictDescending(a{%H-}));
end;

procedure TBaseArrayHelperTest.IsStrictDescendingOfStatic1;
begin
  AssertFalse(TIntHelper.IsStrictDescending(IntSrc10[2..2]));
end;

procedure TBaseArrayHelperTest.IsStrictDescendingOfStatic10Equal;
begin
  AssertFalse(TIntHelper.IsStrictDescending(IntEqual10));
end;

procedure TBaseArrayHelperTest.IsStrictDescendingOfStatic21Success;
var
  a: TIntArray21;
begin
  a := InOrderSrc21;
  TIntHelper.Reverse(a);
  AssertTrue(TIntHelper.IsStrictDescending(a));
end;

procedure TBaseArrayHelperTest.IsStrictDescendingOfStatic21Unsuccess;
var
  a: TIntArray21;
begin
  a := InOrderDblSrc21;
  TIntHelper.Reverse(a);
  AssertFalse(TIntHelper.IsStrictDescending(a));
end;

procedure TBaseArrayHelperTest.IsStrictDescendingOfDyn1;
begin
  AssertFalse(TIntHelper.IsStrictDescending(TIntArray.Create(37)));
end;

procedure TBaseArrayHelperTest.IsStrictDescendingOfDyn10Equal;
begin
  AssertFalse(TIntHelper.IsStrictDescending(TIntHelper.CreateCopy(IntEqual10)));
end;

procedure TBaseArrayHelperTest.IsStrictDescendingOfDyn21Success;
begin
  AssertTrue(TIntHelper.IsStrictDescending(TIntHelper.CreateReverseCopy(InOrderSrc21)));
end;

procedure TBaseArrayHelperTest.IsStrictDescendingOfDyn21Unsuccess;
begin
  AssertFalse(TIntHelper.IsStrictDescending(TIntHelper.CreateReverseCopy(InOrderDblSrc21)));
end;

procedure TBaseArrayHelperTest.InversionCount;
const
  BigSize = 1000;
var
  a: TIntArray = nil;
  I: Integer;
begin
  AssertTrue(TIntHelper.InversionCount(a) = 0);
  a := [5];
  AssertTrue(TIntHelper.InversionCount(a) = 0);
  a := [5, 9];
  AssertTrue(TIntHelper.InversionCount(a) = 0);
  a := [9, 5];
  AssertTrue(TIntHelper.InversionCount(a) = 1);
  a := [9, 5, 2];
  AssertTrue(TIntHelper.InversionCount(a) = 3);
  AssertTrue(TIntHelper.IsNonDescending(a));
  SetLength(a, BigSize);
  for I := 0 to High(a) do
    a[I] := I;
  AssertTrue(TIntHelper.InversionCount(a) = 0);
  TIntHelper.Reverse(a);
  AssertTrue(TIntHelper.InversionCount(a) = BigSize*Pred(BigSize) div 2);
  AssertTrue(TIntHelper.IsStrictAscending(a));
end;

procedure TBaseArrayHelperTest.InversionCountND;
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
  AssertTrue(TIntHelper.InversionCountND(b) = BigSize*Pred(BigSize) div 2);
  AssertTrue(TIntHelper.Same(a, b));
end;

procedure TBaseArrayHelperTest.SameOfEmpty;
var
  a, b: TIntArray;
begin
  AssertTrue(TIntHelper.Same(a{%H-}, b{%H-}));
end;

procedure TBaseArrayHelperTest.SameOfStatic1Success;
begin
  AssertTrue(TIntHelper.Same(IntSrc10[9..9], IntSrc7[1..1]));
end;

procedure TBaseArrayHelperTest.SameOfStatic1Unsuccess;
begin
  AssertFalse(TIntHelper.Same(IntSrc10[8..8], IntSrc7[3..3]));
end;

procedure TBaseArrayHelperTest.SameOfStatic2Success;
begin
  AssertTrue(TIntHelper.Same(InOrderSrc21[0..1], InOrderDblSrc21[1..2]));
end;

procedure TBaseArrayHelperTest.SameOfStatic2Unsuccess;
begin
  AssertFalse(TIntHelper.Same(InOrderSrc21[1..2], InOrderDblSrc21[1..2]));
end;

procedure TBaseArrayHelperTest.SameOfStatic21Success;
var
  a: TIntArray21;
begin
  a := InOrderSrc21;
  AssertTrue(TIntHelper.Same(InOrderSrc21, a));
end;

procedure TBaseArrayHelperTest.SameOfStatic21Unsuccess;
var
  a: TIntArray21;
begin
  a := InOrderSrc21;
  AssertFalse(TIntHelper.Same(InOrderSrc21[1..20], a));
end;

procedure TBaseArrayHelperTest.SameOfDyn1Success;
begin
  AssertTrue(TIntHelper.Same(TIntArray.Create(12), TIntArray.Create(12)));
end;

procedure TBaseArrayHelperTest.SameOfDyn1Unsuccess;
begin
  AssertFalse(TIntHelper.Same(TIntArray.Create(14), TIntArray.Create(15)));
end;

procedure TBaseArrayHelperTest.SameOfDyn2Success;
begin
  AssertTrue(TIntHelper.Same(TIntArray.Create(11, 12), TIntArray.Create(11, 12)));
end;

procedure TBaseArrayHelperTest.SameOfDyn2Unsuccess;
begin
  AssertFalse(TIntHelper.Same(TIntArray.Create(11, 14), TIntArray.Create(11, 15)));
end;

procedure TBaseArrayHelperTest.SameOfDyn21Success;
var
  a, b: TIntArray;
begin
  a := TIntHelper.CreateCopy(InOrderSrc21);
  b := TIntHelper.CreateCopy(InOrderSrc21);
  AssertTrue(TIntHelper.Same(a, b));
end;

procedure TBaseArrayHelperTest.SameOfDyn21Unsuccess; //IntSorted10Uniq
var
  a, b: TIntArray;
begin
  a := TIntHelper.CreateCopy(InOrderSrc21);
  TIntHelper.Reverse(a[2..3]);
  b := TIntHelper.CreateCopy(InOrderSrc21);
  AssertFalse(TIntHelper.Same(a, b));
end;

procedure TBaseArrayHelperTest.QuickSortOfEmpty;
var
  a: TIntArray;
begin
  //should not crash
  TIntHelper.QuickSort(a{%H-});
  AssertTrue(a = nil);
end;

procedure TBaseArrayHelperTest.QuickSortAscOfStatic3Asc;
const
  c: array[1..3] of Integer = (9, 13, 13);
var
  a: array[0..2] of Integer = (9, 13, 13);
begin
  TIntHelper.QuickSort(a);
  AssertTrue(TIntHelper.Same(a, c));
end;

procedure TBaseArrayHelperTest.QuickSortAscOfStatic3Desc;
const
  c: array[1..3] of Integer = (9, 13, 13);
var
  a: array[0..2] of Integer = (13, 13, 9);
begin
  TIntHelper.QuickSort(a);
  AssertTrue(TIntHelper.Same(a, c));
end;

procedure TBaseArrayHelperTest.QuickSortDescOfStatic3Desc;
const
  c: array[1..3] of Integer = (27, 11, 5);
var
  a: array[0..2] of Integer = (27, 11, 5);
begin
  TIntHelper.QuickSort(a, soDesc);
  AssertTrue(TIntHelper.Same(a, c));
end;

procedure TBaseArrayHelperTest.QuickSortDescOfStatic3Asc;
const
  c: array[1..3] of Integer = (27, 11, 5);
var
  a: array[0..2] of Integer = (11, 5, 27);
begin
  TIntHelper.QuickSort(a, soDesc);
  AssertTrue(TIntHelper.Same(a, c));
end;

procedure TBaseArrayHelperTest.QuickSortAscOfStatic377;
var
  I: Integer;
  a, b: array[1..377] of Integer;
begin
  for I := 1 to 377 do
    b[I] := I;
  a := b;
  TIntHelper.RandomShuffle(a);
  TIntHelper.QuickSort(a);
  AssertTrue(TIntHelper.Same(a, b));
end;

procedure TBaseArrayHelperTest.QuickSortDescOfStatic377;
var
  I: Integer;
  a, b: array[1..377] of Integer;
begin
  for I := 1 to 377 do
    b[I] := 378 - I;
  a := b;
  TIntHelper.RandomShuffle(a);
  TIntHelper.QuickSort(a, soDesc);
  AssertTrue(TIntHelper.Same(a, b));
end;

procedure TBaseArrayHelperTest.QuickSortAscOfStatic377Random;
var
  I: Integer;
  a: array[1..377] of Integer;
begin
  for I := 1 to 377 do
    a[I] := Random(Integer.MaxValue);
  TIntHelper.QuickSort(a);
  AssertTrue(TIntHelper.IsNonDescending(a));
end;

procedure TBaseArrayHelperTest.QuickSortAscOfStatic377RandomZeroes;
var
  I: Integer;
  a: array[1..377] of Integer;
begin
  for I := 1 to 377 do
    a[I] := Random(2);
  TIntHelper.QuickSort(a);
  AssertTrue(TIntHelper.IsNonDescending(a));
end;

procedure TBaseArrayHelperTest.QuickSortDescOfStatic377Random;
var
  I: Integer;
  a: array[1..377] of Integer;
begin
  for I := 1 to 377 do
    a[I] := Random(Integer.MaxValue);
  TIntHelper.QuickSort(a, soDesc);
  AssertTrue(TIntHelper.IsNonAscending(a));
end;

procedure TBaseArrayHelperTest.QuickSortDescOfStatic377RandomZeroes;
var
  I: Integer;
  a: array[1..377] of Integer;
begin
  for I := 1 to 377 do
    a[I] := Random(2);
  TIntHelper.QuickSort(a, soDesc);
  AssertTrue(TIntHelper.IsNonAscending(a));
end;

procedure TBaseArrayHelperTest.QuickSortAscOfDyn3Asc;
const
  c: array[1..3] of Integer = (11, 19, 22);
var
  a: TIntArray;
begin
  a := TIntArray.Create(11, 19, 22);
  TIntHelper.QuickSort(a);
  AssertTrue(TIntHelper.Same(a, c));
end;

procedure TBaseArrayHelperTest.QuickSortAscOfDyn3Desc;
const
  c: array[1..3] of Integer = (11, 19, 22);
var
  a: TIntArray;
begin
  a := TIntArray.Create(22, 11, 19);
  TIntHelper.QuickSort(a);
  AssertTrue(TIntHelper.Same(a, c));
end;

procedure TBaseArrayHelperTest.QuickSortDescOfDyn3Desc;
const
  c: array[1..3] of Integer = (31, 14, 3);
var
  a: TIntArray;
begin
  a := TIntArray.Create(31, 14, 3);
  TIntHelper.QuickSort(a, soDesc);
  AssertTrue(TIntHelper.Same(a, c));
end;

procedure TBaseArrayHelperTest.QuickSortDescOfDyn3Asc;
const
  c: array[1..3] of Integer = (31, 14, 3);
var
  a: TIntArray;
begin
  a := TIntArray.Create(14, 3, 31);
  TIntHelper.QuickSort(a, soDesc);
  AssertTrue(TIntHelper.Same(a, c));
end;

procedure TBaseArrayHelperTest.QuickSortAscOfDyn377;
var
  I: Integer;
  b: array[1..377] of Integer;
  a: TIntArray;
begin
  for I := 1 to 377 do
    b[I] := I;
  a := TIntHelper.CreateRandomShuffle(b);
  TIntHelper.QuickSort(a);
  AssertTrue(TIntHelper.Same(a, b));
end;

procedure TBaseArrayHelperTest.QuickSortDescOfDyn377;
var
  I: Integer;
  b: array[1..377] of Integer;
  a: TIntArray;
begin
  for I := 1 to 377 do
    b[I] := 378 - I;
  a := TIntHelper.CreateRandomShuffle(b);
  TIntHelper.QuickSort(a, soDesc);
  AssertTrue(TIntHelper.Same(a, b));
end;

procedure TBaseArrayHelperTest.QuickSortAscOfDyn377Random;
var
  I: Integer;
  a: TIntArray;
begin
  System.SetLength(a, 377);
  for I := 0 to System.High(a) do
    a[I] := Random(Integer.MaxValue);
  TIntHelper.QuickSort(a);
  AssertTrue(TIntHelper.IsNonDescending(a));
end;

procedure TBaseArrayHelperTest.QuickSortAscOfDyn377RandomZeroes;
var
  I: Integer;
  a: TIntArray;
begin
  System.SetLength(a, 377);
  for I := 0 to System.High(a) do
    a[I] := Random(2);
  TIntHelper.QuickSort(a);
  AssertTrue(TIntHelper.IsNonDescending(a));
end;

procedure TBaseArrayHelperTest.QuickSortDescOfDyn377Random;
var
  I: Integer;
  a: TIntArray;
begin
  System.SetLength(a, 377);
  for I := 0 to System.High(a) do
    a[I] := Random(Integer.MaxValue);
  TIntHelper.QuickSort(a, soDesc);
  AssertTrue(TIntHelper.IsNonAscending(a));
end;

procedure TBaseArrayHelperTest.QuickSortDescOfDyn377RandomZeroes;
var
  I: Integer;
  a: TIntArray;
begin
  System.SetLength(a, 377);
  for I := 0 to System.High(a) do
    a[I] := Random(2);
  TIntHelper.QuickSort(a, soDesc);
  AssertTrue(TIntHelper.IsNonAscending(a));
end;

procedure TBaseArrayHelperTest.HeapSortOfStatic277;
var
  I: Integer;
  a, b: array[1..277] of Integer;
begin
  for I := 1 to 277 do
    b[I] := I;
  a := b;
  TIntHelper.RandomShuffle(a);
  THackHelper.DoHeapSort(@a[1], 276);
  AssertTrue(TIntHelper.Same(a, b));
end;

procedure TBaseArrayHelperTest.HeapSortOfStatic277Random;
var
  I: Integer;
  a: array[1..277] of Integer;
begin
  for I := 1 to 277 do
    a[I] := Random(Integer.MaxValue);
  THackHelper.DoHeapSort(@a[1], 276);
  AssertTrue(TIntHelper.IsNonDescending(a));
end;

procedure TBaseArrayHelperTest.HeapSortOfStatic277RandomZeroes;
var
  I: Integer;
  a: array[1..277] of Integer;
begin
  for I := 1 to 277 do
    a[I] := Random(2);
  THackHelper.DoHeapSort(@a[1], 276);
  AssertTrue(TIntHelper.IsNonDescending(a));
end;

procedure TBaseArrayHelperTest.HeapSortOfDyn277;
var
  I: Integer;
  b: array[1..277] of Integer;
  a: TIntArray;
begin
  for I := 1 to 277 do
    b[I] := I;
  a := TIntHelper.CreateRandomShuffle(b);
  THackHelper.DoHeapSort(@a[0], 276);
  AssertTrue(TIntHelper.Same(a, b));
end;

procedure TBaseArrayHelperTest.HeapSortOfDyn277Random;
var
  I: Integer;
  a: TIntArray;
begin
  System.SetLength(a, 277);
  for I := 0 to System.High(a) do
    a[I] := Random(Integer.MaxValue);
  THackHelper.DoHeapSort(@a[0], 276);
  AssertTrue(TIntHelper.IsNonDescending(a));
end;

procedure TBaseArrayHelperTest.HeapSortOfDyn277RandomZeroes;
var
  I: Integer;
  a: TIntArray;
begin
  System.SetLength(a, 277);
  for I := 0 to System.High(a) do
    a[I] := Random(2);
  THackHelper.DoHeapSort(@a[0], 276);
  AssertTrue(TIntHelper.IsNonDescending(a));
end;

procedure TBaseArrayHelperTest.IntroSortOfEmpty;
var
  a: TIntArray;
begin
  //should not crash
  TIntHelper.IntroSort(a{%H-});
  AssertTrue(a = nil);
end;

procedure TBaseArrayHelperTest.IntroSortAscOfStatic3Asc;
const
  c: array[1..3] of Integer = (9, 13, 13);
var
  a: array[0..2] of Integer = (9, 13, 13);
begin
  TIntHelper.IntroSort(a);
  AssertTrue(TIntHelper.Same(a, c));
end;

procedure TBaseArrayHelperTest.IntroSortAscOfStatic3Desc;
const
  c: array[1..3] of Integer = (9, 13, 13);
var
  a: array[0..2] of Integer = (13, 13, 9);
begin
  TIntHelper.IntroSort(a);
  AssertTrue(TIntHelper.Same(a, c));
end;

procedure TBaseArrayHelperTest.IntroSortDescOfStatic3Desc;
const
  c: array[1..3] of Integer = (27, 11, 5);
var
  a: array[0..2] of Integer = (27, 11, 5);
begin
  TIntHelper.IntroSort(a, soDesc);
  AssertTrue(TIntHelper.Same(a, c));
end;

procedure TBaseArrayHelperTest.IntroSortDescOfStatic3Asc;
const
  c: array[1..3] of Integer = (27, 11, 5);
var
  a: array[0..2] of Integer = (11, 5, 27);
begin
  TIntHelper.IntroSort(a, soDesc);
  AssertTrue(TIntHelper.Same(a, c));
end;

procedure TBaseArrayHelperTest.IntroSortAscOfStatic577;
var
  I: Integer;
  a, b: array[1..577] of Integer;
begin
  for I := 1 to 577 do
    b[I] := I;
  a := b;
  TIntHelper.RandomShuffle(a);
  TIntHelper.IntroSort(a);
  AssertTrue(TIntHelper.Same(a, b));
end;

procedure TBaseArrayHelperTest.IntroSortDescOfStatic577;
var
  I: Integer;
  a, b: array[1..577] of Integer;
begin
  for I := 1 to 577 do
    b[I] := 578 - I;
  a := b;
  TIntHelper.RandomShuffle(a);
  TIntHelper.IntroSort(a, soDesc);
  AssertTrue(TIntHelper.Same(a, b));
end;

procedure TBaseArrayHelperTest.IntroSortAscOfStatic577Random;
var
  I: Integer;
  a: array[1..577] of Integer;
begin
  for I := 1 to 577 do
    a[I] := Random(Integer.MaxValue);
  TIntHelper.IntroSort(a);
  AssertTrue(TIntHelper.IsNonDescending(a));
end;

procedure TBaseArrayHelperTest.IntroSortAscOfStatic577RandomZeroes;
var
  I: Integer;
  a: array[1..577] of Integer;
begin
  for I := 1 to 577 do
    a[I] := Random(2);
  TIntHelper.IntroSort(a);
  AssertTrue(TIntHelper.IsNonDescending(a));
end;

procedure TBaseArrayHelperTest.IntroSortDescOfStatic577Random;
var
  I: Integer;
  a: array[1..577] of Integer;
begin
  for I := 1 to 577 do
    a[I] := Random(Integer.MaxValue);
  TIntHelper.IntroSort(a, soDesc);
  AssertTrue(TIntHelper.IsNonAscending(a));
end;

procedure TBaseArrayHelperTest.IntroSortDescOfStatic577RandomZeroes;
var
  I: Integer;
  a: array[1..577] of Integer;
begin
  for I := 1 to 577 do
    a[I] := Random(2);
  TIntHelper.IntroSort(a, soDesc);
  AssertTrue(TIntHelper.IsNonAscending(a));
end;

procedure TBaseArrayHelperTest.IntroSortAscOfDyn3Asc;
const
  c: array[1..3] of Integer = (11, 19, 22);
var
  a: TIntArray;
begin
  a := TIntArray.Create(11, 19, 22);
  TIntHelper.IntroSort(a);
  AssertTrue(TIntHelper.Same(a, c));
end;

procedure TBaseArrayHelperTest.IntroSortAscOfDyn3Desc;
const
  c: array[1..3] of Integer = (11, 19, 22);
var
  a: TIntArray;
begin
  a := TIntArray.Create(22, 11, 19);
  TIntHelper.IntroSort(a);
  AssertTrue(TIntHelper.Same(a, c));
end;

procedure TBaseArrayHelperTest.IntroSortDescOfDyn3Desc;
const
  c: array[1..3] of Integer = (31, 14, 3);
var
  a: TIntArray;
begin
  a := TIntArray.Create(31, 14, 3);
  TIntHelper.IntroSort(a, soDesc);
  AssertTrue(TIntHelper.Same(a, c));
end;

procedure TBaseArrayHelperTest.IntroSortDescOfDyn3Asc;
const
  c: array[1..3] of Integer = (31, 14, 3);
var
  a: TIntArray;
begin
  a := TIntArray.Create(14, 3, 31);
  TIntHelper.IntroSort(a, soDesc);
  AssertTrue(TIntHelper.Same(a, c));
end;

procedure TBaseArrayHelperTest.IntroSortAscOfDyn577;
var
  I: Integer;
  b: array[1..577] of Integer;
  a: TIntArray;
begin
  for I := 1 to 577 do
    b[I] := I;
  a := TIntHelper.CreateRandomShuffle(b);
  TIntHelper.IntroSort(a);
  AssertTrue(TIntHelper.Same(a, b));
end;

procedure TBaseArrayHelperTest.IntroSortDescOfDyn577;
var
  I: Integer;
  b: array[1..577] of Integer;
  a: TIntArray;
begin
  for I := 1 to 577 do
    b[I] := 578 - I;
  a := TIntHelper.CreateRandomShuffle(b);
  TIntHelper.IntroSort(a, soDesc);
  AssertTrue(TIntHelper.Same(a, b));
end;

procedure TBaseArrayHelperTest.IntroSortAscOfDyn577Random;
var
  I: Integer;
  a: TIntArray;
begin
  System.SetLength(a, 577);
  for I := 0 to System.High(a) do
    a[I] := Random(Integer.MaxValue);
  TIntHelper.IntroSort(a);
  AssertTrue(TIntHelper.IsNonDescending(a));
end;

procedure TBaseArrayHelperTest.IntroSortAscOfDyn577RandomZeroes;
var
  I: Integer;
  a: TIntArray;
begin
  System.SetLength(a, 577);
  for I := 0 to System.High(a) do
    a[I] := Random(2);
  TIntHelper.IntroSort(a);
  AssertTrue(TIntHelper.IsNonDescending(a));
end;

procedure TBaseArrayHelperTest.IntroSortDescOfDyn577Random;
var
  I: Integer;
  a: TIntArray;
begin
  System.SetLength(a, 577);
  for I := 0 to System.High(a) do
    a[I] := Random(Integer.MaxValue);
  TIntHelper.IntroSort(a, soDesc);
  AssertTrue(TIntHelper.IsNonAscending(a));
end;

procedure TBaseArrayHelperTest.IntroSortDescOfDyn577RandomZeroes;
var
  I: Integer;
  a: TIntArray;
begin
  System.SetLength(a, 577);
  for I := 0 to System.High(a) do
    a[I] := Random(2);
  TIntHelper.IntroSort(a, soDesc);
  AssertTrue(TIntHelper.IsNonAscending(a));
end;

procedure TBaseArrayHelperTest.DualPivotQuickSortOfEmpty;
var
  a: TIntArray;
begin
  //should not crash
  TIntHelper.DualPivotQuickSort(a{%H-});
  AssertTrue(a = nil);
end;

procedure TBaseArrayHelperTest.DualPivotQuickSortAscOfStatic3Asc;
const
  c: array[1..3] of Integer = (9, 13, 13);
var
  a: array[0..2] of Integer = (9, 13, 13);
begin
  TIntHelper.DualPivotQuickSort(a);
  AssertTrue(TIntHelper.Same(a, c));
end;

procedure TBaseArrayHelperTest.DualPivotQuickSortAscOfStatic3Desc;
const
  c: array[1..3] of Integer = (9, 13, 13);
var
  a: array[0..2] of Integer = (13, 13, 9);
begin
  TIntHelper.DualPivotQuickSort(a);
  AssertTrue(TIntHelper.Same(a, c));
end;

procedure TBaseArrayHelperTest.DualPivotQuickSortDescOfStatic3Desc;
const
  c: array[1..3] of Integer = (27, 11, 5);
var
  a: array[0..2] of Integer = (27, 11, 5);
begin
  TIntHelper.DualPivotQuickSort(a, soDesc);
  AssertTrue(TIntHelper.Same(a, c));
end;

procedure TBaseArrayHelperTest.DualPivotQuickSortDescOfStatic3Asc;
const
  c: array[1..3] of Integer = (27, 11, 5);
var
  a: array[0..2] of Integer = (11, 5, 27);
begin
  TIntHelper.DualPivotQuickSort(a, soDesc);
  AssertTrue(TIntHelper.Same(a, c));
end;

procedure TBaseArrayHelperTest.DualPivotQuickSortAscOfStatic577;
var
  I: Integer;
  a, b: array[1..577] of Integer;
begin
  for I := 1 to 577 do
    b[I] := I;
  a := b;
  TIntHelper.RandomShuffle(a);
  TIntHelper.DualPivotQuickSort(a);
  AssertTrue(TIntHelper.Same(a, b));
end;

procedure TBaseArrayHelperTest.DualPivotQuickSortDescOfStatic577;
var
  I: Integer;
  a, b: array[1..577] of Integer;
begin
  for I := 1 to 577 do
    b[I] := 578 - I;
  a := b;
  TIntHelper.RandomShuffle(a);
  TIntHelper.DualPivotQuickSort(a, soDesc);
  AssertTrue(TIntHelper.Same(a, b));
end;

procedure TBaseArrayHelperTest.DualPivotQuickSortAscOfStatic577Random;
var
  I: Integer;
  a: array[1..577] of Integer;
begin
  for I := 1 to 577 do
    a[I] := Random(Integer.MaxValue);
  TIntHelper.DualPivotQuickSort(a);
  AssertTrue(TIntHelper.IsNonDescending(a));
end;

procedure TBaseArrayHelperTest.DualPivotQuickSortAscOfStatic577RandomZeroes;
var
  I: Integer;
  a: array[1..577] of Integer;
begin
  for I := 1 to 577 do
    a[I] := Random(2);
  TIntHelper.DualPivotQuickSort(a);
  AssertTrue(TIntHelper.IsNonDescending(a));
end;

procedure TBaseArrayHelperTest.DualPivotQuickSortDescOfStatic577Random;
var
  I: Integer;
  a: array[1..577] of Integer;
begin
  for I := 1 to 577 do
    a[I] := Random(Integer.MaxValue);
  TIntHelper.DualPivotQuickSort(a, soDesc);
  AssertTrue(TIntHelper.IsNonAscending(a));
end;

procedure TBaseArrayHelperTest.DualPivotQuickSortDescOfStatic577RandomZeroes;
var
  I: Integer;
  a: array[1..577] of Integer;
begin
  for I := 1 to 577 do
    a[I] := Random(2);
  TIntHelper.DualPivotQuickSort(a, soDesc);
  AssertTrue(TIntHelper.IsNonAscending(a));
end;

procedure TBaseArrayHelperTest.DualPivotQuickSortAscOfDyn3Asc;
const
  c: array[1..3] of Integer = (11, 19, 22);
var
  a: TIntArray;
begin
  a := TIntArray.Create(11, 19, 22);
  TIntHelper.DualPivotQuickSort(a);
  AssertTrue(TIntHelper.Same(a, c));
end;

procedure TBaseArrayHelperTest.DualPivotQuickSortAscOfDyn3Desc;
const
  c: array[1..3] of Integer = (11, 19, 22);
var
  a: TIntArray;
begin
  a := TIntArray.Create(22, 11, 19);
  TIntHelper.DualPivotQuickSort(a);
  AssertTrue(TIntHelper.Same(a, c));
end;

procedure TBaseArrayHelperTest.DualPivotQuickSortDescOfDyn3Desc;
const
  c: array[1..3] of Integer = (31, 14, 3);
var
  a: TIntArray;
begin
  a := TIntArray.Create(31, 14, 3);
  TIntHelper.DualPivotQuickSort(a, soDesc);
  AssertTrue(TIntHelper.Same(a, c));
end;

procedure TBaseArrayHelperTest.DualPivotQuickSortDescOfDyn3Asc;
const
  c: array[1..3] of Integer = (31, 14, 3);
var
  a: TIntArray;
begin
  a := TIntArray.Create(14, 3, 31);
  TIntHelper.DualPivotQuickSort(a, soDesc);
  AssertTrue(TIntHelper.Same(a, c));
end;

procedure TBaseArrayHelperTest.DualPivotQuickSortAscOfDyn577;
var
  I: Integer;
  b: array[1..577] of Integer;
  a: TIntArray;
begin
  for I := 1 to 577 do
    b[I] := I;
  a := TIntHelper.CreateRandomShuffle(b);
  TIntHelper.DualPivotQuickSort(a);
  AssertTrue(TIntHelper.Same(a, b));
end;

procedure TBaseArrayHelperTest.DualPivotQuickSortDescOfDyn577;
var
  I: Integer;
  b: array[1..577] of Integer;
  a: TIntArray;
begin
  for I := 1 to 577 do
    b[I] := 578 - I;
  a := TIntHelper.CreateRandomShuffle(b);
  TIntHelper.DualPivotQuickSort(a, soDesc);
  AssertTrue(TIntHelper.Same(a, b));
end;

procedure TBaseArrayHelperTest.DualPivotQuickSortAscOfDyn577Random;
var
  I: Integer;
  a: TIntArray;
begin
  System.SetLength(a, 577);
  for I := 0 to System.High(a) do
    a[I] := Random(Integer.MaxValue);
  TIntHelper.DualPivotQuickSort(a);
  AssertTrue(TIntHelper.IsNonDescending(a));
end;

procedure TBaseArrayHelperTest.DualPivotQuickSortAscOfDyn577RandomZeroes;
var
  I: Integer;
  a: TIntArray;
begin
  System.SetLength(a, 577);
  for I := 0 to System.High(a) do
    a[I] := Random(2);
  TIntHelper.DualPivotQuickSort(a);
  AssertTrue(TIntHelper.IsNonDescending(a));
end;

procedure TBaseArrayHelperTest.DualPivotQuickSortDescOfDyn577Random;
var
  I: Integer;
  a: TIntArray;
begin
  System.SetLength(a, 577);
  for I := 0 to System.High(a) do
    a[I] := Random(Integer.MaxValue);
  TIntHelper.DualPivotQuickSort(a, soDesc);
  AssertTrue(TIntHelper.IsNonAscending(a));
end;

procedure TBaseArrayHelperTest.DualPivotQuickSortDescOfDyn577RandomZeroes;
var
  I: Integer;
  a: TIntArray;
begin
  System.SetLength(a, 577);
  for I := 0 to System.High(a) do
    a[I] := Random(2);
  TIntHelper.DualPivotQuickSort(a, soDesc);
  AssertTrue(TIntHelper.IsNonAscending(a));
end;

procedure TBaseArrayHelperTest.MergeSortOfEmpty;
var
  a: TIntArray;
begin
  //should not crash
  TIntHelper.MergeSort(a{%H-});
  AssertTrue(a = nil);
end;

procedure TBaseArrayHelperTest.MergeSortAscOfStatic3Asc;
const
  c: array[1..3] of Integer = (9, 13, 13);
var
  a: array[0..2] of Integer = (9, 13, 13);
begin
  TIntHelper.MergeSort(a);
  AssertTrue(TIntHelper.Same(a, c));
end;

procedure TBaseArrayHelperTest.MergeSortAscOfStatic3Desc;
const
  c: array[1..3] of Integer = (9, 13, 13);
var
  a: array[0..2] of Integer = (13, 13, 9);
begin
  TIntHelper.MergeSort(a);
  AssertTrue(TIntHelper.Same(a, c));
end;

procedure TBaseArrayHelperTest.MergeSortDescOfStatic3Desc;
const
  c: array[1..3] of Integer = (27, 11, 5);
var
  a: array[0..2] of Integer = (27, 11, 5);
begin
  TIntHelper.MergeSort(a, soDesc);
  AssertTrue(TIntHelper.Same(a, c));
end;

procedure TBaseArrayHelperTest.MergeSortDescOfStatic3Asc;
const
  c: array[1..3] of Integer = (27, 11, 5);
var
  a: array[0..2] of Integer = (11, 5, 27);
begin
  TIntHelper.MergeSort(a, soDesc);
  AssertTrue(TIntHelper.Same(a, c));
end;

procedure TBaseArrayHelperTest.MergeSortAscOfStatic577;
var
  I: Integer;
  a, b: array[1..577] of Integer;
begin
  for I := 1 to 577 do
    b[I] := I;
  a := b;
  TIntHelper.RandomShuffle(a);
  TIntHelper.MergeSort(a);
  AssertTrue(TIntHelper.Same(a, b));
end;

procedure TBaseArrayHelperTest.MergeSortDescOfStatic577;
var
  I: Integer;
  a, b: array[1..577] of Integer;
begin
  for I := 1 to 577 do
    b[I] := 578 - I;
  a := b;
  TIntHelper.RandomShuffle(a);
  TIntHelper.MergeSort(a, soDesc);
  AssertTrue(TIntHelper.Same(a, b));
end;

procedure TBaseArrayHelperTest.MergeSortAscOfStatic577Random;
var
  I: Integer;
  a: array[1..577] of Integer;
begin
  for I := 1 to 577 do
    a[I] := Random(Integer.MaxValue);
  TIntHelper.MergeSort(a);
  AssertTrue(TIntHelper.IsNonDescending(a));
end;

procedure TBaseArrayHelperTest.MergeSortAscOfStatic577RandomZeroes;
var
  I: Integer;
  a: array[1..577] of Integer;
begin
  for I := 1 to 577 do
    a[I] := Random(2);
  TIntHelper.MergeSort(a);
  AssertTrue(TIntHelper.IsNonDescending(a));
end;

procedure TBaseArrayHelperTest.MergeSortDescOfStatic577Random;
var
  I: Integer;
  a: array[1..577] of Integer;
begin
  for I := 1 to 577 do
    a[I] := Random(Integer.MaxValue);
  TIntHelper.MergeSort(a, soDesc);
  AssertTrue(TIntHelper.IsNonAscending(a));
end;

procedure TBaseArrayHelperTest.MergeSortDescOfStatic577RandomZeroes;
var
  I: Integer;
  a: array[1..577] of Integer;
begin
  for I := 1 to 577 do
    a[I] := Random(2);
  TIntHelper.MergeSort(a, soDesc);
  AssertTrue(TIntHelper.IsNonAscending(a));
end;

procedure TBaseArrayHelperTest.MergeSortAscOfDyn3Asc;
const
  c: array[1..3] of Integer = (11, 19, 22);
var
  a: TIntArray;
begin
  a := TIntArray.Create(11, 19, 22);
  TIntHelper.MergeSort(a);
  AssertTrue(TIntHelper.Same(a, c));
end;

procedure TBaseArrayHelperTest.MergeSortAscOfDyn3Desc;
const
  c: array[1..3] of Integer = (11, 19, 22);
var
  a: TIntArray;
begin
  a := TIntArray.Create(22, 11, 19);
  TIntHelper.MergeSort(a);
  AssertTrue(TIntHelper.Same(a, c));
end;

procedure TBaseArrayHelperTest.MergeSortDescOfDyn3Desc;
const
  c: array[1..3] of Integer = (31, 14, 3);
var
  a: TIntArray;
begin
  a := TIntArray.Create(31, 14, 3);
  TIntHelper.MergeSort(a, soDesc);
  AssertTrue(TIntHelper.Same(a, c));
end;

procedure TBaseArrayHelperTest.MergeSortDescOfDyn3Asc;
const
  c: array[1..3] of Integer = (31, 14, 3);
var
  a: TIntArray;
begin
  a := TIntArray.Create(14, 3, 31);
  TIntHelper.MergeSort(a, soDesc);
  AssertTrue(TIntHelper.Same(a, c));
end;

procedure TBaseArrayHelperTest.MergeSortAscOfDyn577;
var
  I: Integer;
  b: array[1..577] of Integer;
  a: TIntArray;
begin
  for I := 1 to 577 do
    b[I] := I;
  a := TIntHelper.CreateRandomShuffle(b);
  TIntHelper.MergeSort(a);
  AssertTrue(TIntHelper.Same(a, b));
end;

procedure TBaseArrayHelperTest.MergeSortDescOfDyn577;
var
  I: Integer;
  b: array[1..577] of Integer;
  a: TIntArray;
begin
  for I := 1 to 577 do
    b[I] := 578 - I;
  a := TIntHelper.CreateRandomShuffle(b);
  TIntHelper.MergeSort(a, soDesc);
  AssertTrue(TIntHelper.Same(a, b));
end;

procedure TBaseArrayHelperTest.MergeSortAscOfDyn577Random;
var
  I: Integer;
  a: TIntArray;
begin
  System.SetLength(a, 577);
  for I := 0 to System.High(a) do
    a[I] := Random(Integer.MaxValue);
  TIntHelper.MergeSort(a);
  AssertTrue(TIntHelper.IsNonDescending(a));
end;

procedure TBaseArrayHelperTest.MergeSortAscOfDyn577RandomZeroes;
var
  I: Integer;
  a: TIntArray;
begin
  System.SetLength(a, 577);
  for I := 0 to System.High(a) do
    a[I] := Random(2);
  TIntHelper.MergeSort(a);
  AssertTrue(TIntHelper.IsNonDescending(a));
end;

procedure TBaseArrayHelperTest.MergeSortDescOfDyn577Random;
var
  I: Integer;
  a: TIntArray;
begin
  System.SetLength(a, 577);
  for I := 0 to System.High(a) do
    a[I] := Random(Integer.MaxValue);
  TIntHelper.MergeSort(a, soDesc);
  AssertTrue(TIntHelper.IsNonAscending(a));
end;

procedure TBaseArrayHelperTest.MergeSortDescOfDyn577RandomZeroes;
var
  I: Integer;
  a: TIntArray;
begin
  System.SetLength(a, 577);
  for I := 0 to System.High(a) do
    a[I] := Random(2);
  TIntHelper.MergeSort(a, soDesc);
  AssertTrue(TIntHelper.IsNonAscending(a));
end;

procedure TBaseArrayHelperTest.MergeSortAscOfStatic877Stable;
const
  ValCount = 41;
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
  TPairDataHelper.MergeSort(a);
  AssertTrue(TPairDataHelper.IsNonDescending(a));
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

procedure TBaseArrayHelperTest.MergeSortDescOfStatic877Stable;
const
  ValCount = 41;
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
  TPairDataHelper.MergeSort(a, soDesc);
  AssertTrue(TPairDataHelper.IsNonAscending(a));
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

procedure TBaseArrayHelperTest.MergeSortAscOfDyn877Stable;
const
  ValCount = 41;
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
  TPairDataHelper.MergeSort(a);
  AssertTrue(TPairDataHelper.IsNonDescending(a));
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

procedure TBaseArrayHelperTest.MergeSortDescOfDyn877Stable;
const
  ValCount = 41;
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
  TPairDataHelper.MergeSort(a, soDesc);
  AssertTrue(TPairDataHelper.IsNonAscending(a));
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

procedure TBaseArrayHelperTest.PDQSortTest;
var
  a, b: TIntArray;
  I: Integer;
const
  TestSize = 1000;
begin
  TIntHelper.PDQSort(a{%H-});
  AssertTrue(a = nil);
  a := [9, 13, 13];
  TIntHelper.PDQSort(a);
  AssertTrue(TIntHelper.Same(a, [9, 13, 13]));
  a := [13, 11, 5];
  TIntHelper.PDQSort(a);
  AssertTrue(TIntHelper.Same(a, [5, 11, 13]));
  SetLength(b, TestSize);
  for I := 0 to Pred(TestSize) do
    b[I] := I;
  a := TIntHelper.CreateRandomShuffle(b);
  TIntHelper.PDQSort(a);
  AssertTrue(TIntHelper.Same(a, b));
  a := TIntHelper.CreateRandomShuffle(b);
  TIntHelper.Reverse(b);
  TIntHelper.PDQSort(a, soDesc);
  AssertTrue(TIntHelper.Same(a, b));
end;

procedure TBaseArrayHelperTest.TimSortTest;
var
  a, b: TIntArray;
  I: Integer;
const
  TestSize = 1000;
begin
  TIntTimSort.Sort(a{%H-});
  AssertTrue(a = nil);
  a := [9, 13, 13];
  TIntTimSort.Sort(a);
  AssertTrue(TIntHelper.Same(a, [9, 13, 13]));
  a := [13, 11, 5];
  TIntTimSort.Sort(a);
  AssertTrue(TIntHelper.Same(a, [5, 11, 13]));
  SetLength(b, TestSize);
  for I := 0 to Pred(TestSize) do
    b[I] := I;
  a := TIntHelper.CreateRandomShuffle(b);
  TIntTimSort.Sort(a);
  AssertTrue(TIntHelper.Same(a, b));
  a := TIntHelper.CreateRandomShuffle(b);
  TIntHelper.Reverse(b);
  TIntTimSort.Sort(a, soDesc);
  AssertTrue(TIntHelper.Same(a, b));
end;

procedure TBaseArrayHelperTest.TimSortAscStableTest;
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
  TPairTimSort.Sort(a);
  AssertTrue(TPairDataHelper.IsNonDescending(a));
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

procedure TBaseArrayHelperTest.TimSortDescStableTest;
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
  TPairTimSort.Sort(a, soDesc);
  AssertTrue(TPairDataHelper.IsNonAscending(a));
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

{ TBaseIndexedHelperTest }

procedure TBaseIndexedHelperTest.CreateCopy;
const
  TestSize = 100;
var
  d: TIntDeque;
  a: array of Integer;
  I: Integer;
begin
  AssertTrue(TDeqHelper.CreateCopy(d{%H-}, 0, 1) = nil);
  for I := 1 to TestSize do
    d.PushLast(I);
  for I := 1 to TestSize do
    d.PushFirst(I + TestSize);
  a := TDeqHelper.CreateCopy(d, TestSize, TestSize);
  AssertTrue(Length(a) = TestSize);
  for I := 0 to High(a) do
    AssertTrue(a[I] = I + 1);
end;

procedure TBaseIndexedHelperTest.Reverse;
const
  TestSize = 100;
var
  d: TIntDeque;
  a: array of Integer;
  I: Integer;
begin
  for I := 1 to TestSize do
    d.PushLast(I);
  for I := 1 to TestSize do
    d.PushFirst(I + TestSize);
  a := d.ToArray;
  TDeqHelper.Reverse(d);
  THelper.Reverse(a);
  AssertTrue(THelper.Same(a, d.ToArray));
end;

procedure TBaseIndexedHelperTest.Reverse2;
const
  TestSize = 100;
var
  d: TIntDeque;
  a: array of Integer;
  I: Integer;
  Raised: Boolean = False;
begin
  try
    TDeqHelper.Reverse(d, 0, 1);
  except
    Raised := True;
  end;
  AssertTrue(Raised);
  for I := 1 to TestSize do
    d.PushLast(I);
  for I := 1 to TestSize do
    d.PushFirst(I + TestSize);
  a := d.ToArray;
  TDeqHelper.Reverse(d, TestSize div 2, Pred(TestSize + TestSize div 2));
  THelper.Reverse(a[TestSize div 2..Pred(TestSize + TestSize div 2)]);
  AssertTrue(THelper.Same(a, d.ToArray));
end;

procedure TBaseIndexedHelperTest.SequentSearch;
const
  TestSize = 100;
var
  d: TIntDeque;
  I: Integer;
begin
  AssertTrue(TDeqHelper.SequentSearch(d{%H-}, TestSize) = -1);
  for I := 1 to TestSize do
    d.PushLast(I);
  for I := 1 to TestSize do
    d.PushFirst(I + TestSize);
  AssertTrue(TDeqHelper.SequentSearch(d, TestSize) = d.Count - 1);
  AssertTrue(TDeqHelper.SequentSearch(d, TestSize*2) = 0);
  AssertTrue(TDeqHelper.SequentSearch(d, 1) = TestSize);
end;

procedure TBaseIndexedHelperTest.IndexOfMin;
const
  TestSize = 100;
var
  d: TIntDeque;
  I: Integer;
begin
  AssertTrue(TDeqHelper.IndexOfMin(d{%H-}) = -1);
  for I := 1 to TestSize do
    d.PushLast(I);
  for I := 1 to TestSize do
    d.PushFirst(I + TestSize);
  AssertTrue(TDeqHelper.IndexOfMin(d) = TestSize);
  TDeqHelper.Reverse(d);
  AssertTrue(TDeqHelper.IndexOfMin(d) = TestSize - 1);
end;

procedure TBaseIndexedHelperTest.IndexOfMax;
const
  TestSize = 100;
var
  d: TIntDeque;
  I: Integer;
begin
  AssertTrue(TDeqHelper.IndexOfMax(d{%H-}) = -1);
  for I := 1 to TestSize do
    d.PushLast(I);
  for I := 1 to TestSize do
    d.PushFirst(I + TestSize);
  AssertTrue(TDeqHelper.IndexOfMax(d) = 0);
  TDeqHelper.Reverse(d);
  AssertTrue(TDeqHelper.IndexOfMax(d) = d.Count - 1);
end;

procedure TBaseIndexedHelperTest.Sort;
const
  TestSize = 1000;
var
  d: TIntDeque;
  a: array of Integer;
  I: Integer;
begin
  for I := 1 to TestSize do
    d.PushLast(I);
  TDeqHelper.RandomShuffle(d);
  TDeqHelper.Sort(d);
  AssertTrue(TDeqHelper.IsStrictAscending(d));
  a := d.ToArray;
  AssertTrue(THelper.IsStrictAscending(a));
  TDeqHelper.RandomShuffle(d);
  TDeqHelper.Sort(d, soDesc);
  AssertTrue(TDeqHelper.IsStrictDescending(d));
  a := d.ToArray;
  AssertTrue(THelper.IsStrictDescending(a));
end;

procedure TBaseIndexedHelperTest.Sort2;
const
  TestSize = 1000;
var
  d: TIntDeque;
  a: array of Integer;
  I: Integer;
begin
  for I := 1 to TestSize do
    d.PushLast(I);
  TDeqHelper.RandomShuffle(d);
  TDeqHelper.Sort(d, 10, TestSize - 10);
  a := TDeqHelper.CreateCopy(d, 10, Succ(TestSize - 10 - 10));
  AssertTrue(THelper.IsStrictAscending(a));
  TDeqHelper.RandomShuffle(d);
  TDeqHelper.Sort(d, 10, TestSize - 10, soDesc);
  a := TDeqHelper.CreateCopy(d, 10, Succ(TestSize - 10 - 10));
  AssertTrue(THelper.IsStrictDescending(a));
end;

initialization

  RegisterTest(TBaseArrayHelperTest);
  RegisterTest(TBaseIndexedHelperTest);

end.

