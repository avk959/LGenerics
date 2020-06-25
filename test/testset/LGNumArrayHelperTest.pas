unit LGNumArrayHelperTest;

{$mode objfpc}{$H+}

interface

uses

  SysUtils, fpcunit, testregistry,
  LGUtils,
  LGArrayHelpers;

type

  { TNumArrayHelperTest }

  TNumArrayHelperTest = class(TTestCase)
  private
  type

    TIntHelper  = specialize TGSimpleArrayHelper<Integer>;
    THackHelper = class(TIntHelper);
    TIntArray   = specialize TGArray<Integer>;

  published
    procedure ReverseEmpty;
    procedure ReverseStatic1;
    procedure ReverseDyn1;
    procedure ReverseStatic2;
    procedure ReverseDyn2;
    procedure ReverseStatic10;
    procedure ReverseDyn10;

    procedure RotateIllegal;
    procedure RotateStatic;
    procedure RotateDyn;

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
    procedure BinarySearchStaticUnsuccess20;
    procedure BinarySearchStaticUnsuccess20_1;
    procedure BinarySearchStaticSuccess20_2;
    procedure BinarySearchStaticSuccess20_3;
    procedure BinarySearchStaticUnsuccessEqual10;
    procedure BinarySearchStaticSuccessEqual10;

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

    procedure PDQSortTest;
  end;

  { TOrdArrayHelperTest }

  TOrdArrayHelperTest = class(TTestCase)
  private
  type
    THelper   = specialize TGOrdinalArrayHelper<Integer>;
    TIntArray = THelper.TArray;
  published
    procedure CreateRange;
    procedure SortAsc;
    procedure SortDesc;
    procedure SortAsc577;
    procedure SortDesc577;
{$IFDEF CPU64}
    procedure SortInt64Asc;
    procedure SortInt64Desc;
{$ENDIF CPU64}
  end;


implementation
{$B-}{$COPERATORS ON}

type

  TIntArray21 = array[0..20] of Integer;
  TIntArray10 = array[1..10] of Integer;

const

  IntSrc10: array[2..11] of Integer = (10, -15, 117, 21, -199, 1, 13, 0, -29, -1);
  ReverseIntSrc10: array[2..11] of Integer = (-1, -29, 0, 13, 1, -199, 21, 117, -15, 10);

  IntSrc7: array[0..6] of Integer = (101, 0, -11, -27, 127, 1, 0);

  IntSorted10Uniq: array[1..10] of Integer = (-22, -1, 0, 3, 14, 51, 69, 177, 281, 593);

  IntEqual10: TIntArray10 = (9, 9, 9, 9, 9, 9, 9, 9, 9, 9);

  IntSorted20: array[1..20] of Integer = (-5, -5, -3, 0, 0, 2, 2, 5, 7, 7, 8, 10, 14, 17, 17, 28, 28, 39, 40, 40);

  InOrderSrc21: TIntArray21 = (0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20);

  InOrderDblSrc21: TIntArray21 = (0, 0, 1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6, 7, 7, 8, 8, 9, 9, 10);

{ TNumArrayHelperTest }

procedure TNumArrayHelperTest.ReverseEmpty;
var
  a: TIntArray = nil;
begin
  TIntHelper.Reverse(a);
  AssertTrue(a = nil);
end;

procedure TNumArrayHelperTest.ReverseStatic1;
type
  TArray = array[0..0] of Integer;
const
  a: TArray = (10);
var
  b: TArray;
begin
  b := a;
  TIntHelper.Reverse(b);
  AssertTrue(TIntHelper.Same(b, a));
end;

procedure TNumArrayHelperTest.ReverseDyn1;
const
  a: array[0..0] of Integer = (10);
var
  b: TIntArray;
begin
  b := TIntHelper.CreateCopy(a);
  TIntHelper.Reverse(b);
  AssertTrue(TIntHelper.Same(b, a));
end;

procedure TNumArrayHelperTest.ReverseStatic2;
type
  TArray = array[0..1] of Integer;
const
  r: TArray = (-30, 10);
var
  a: TArray = (10, -30);
begin
  TIntHelper.Reverse(a);
  AssertTrue(TIntHelper.Same(r, a));
end;

procedure TNumArrayHelperTest.ReverseDyn2;
const
  r: array[0..1] of Integer = (-30, 10);
var
  a: TIntArray;
begin
  a := TIntArray.Create(10, -30);
  TIntHelper.Reverse(a);
  AssertTrue(TIntHelper.Same(r, a));
end;

procedure TNumArrayHelperTest.ReverseStatic10;
var
  a: array[2..11] of Integer;
begin
  a := IntSrc10;
  TIntHelper.Reverse(a);
  AssertTrue(TIntHelper.Same(ReverseIntSrc10, a));
end;

procedure TNumArrayHelperTest.ReverseDyn10;
var
  a: TIntArray;
begin
  a := TIntHelper.CreateCopy(IntSrc10);
  TIntHelper.Reverse(a);
  AssertTrue(TIntHelper.Same(ReverseIntSrc10, a));
end;

procedure TNumArrayHelperTest.RotateIllegal;
type
  TIntArray10 = array[1..10] of Integer;
const
  Sample: TIntArray10 = (0, 1, 2, 3, 4, 5, 6, 7, 8, 9);
var
  a: TIntArray10;
begin
  a := Sample;
  TIntHelper.RotateLeft(a, 0);
  AssertTrue(TIntHelper.Same(a, Sample));
  TIntHelper.RotateLeft(a, 10);
  AssertTrue(TIntHelper.Same(a, Sample));
  TIntHelper.RotateLeft(a, -10);
  AssertTrue(TIntHelper.Same(a, Sample));
  TIntHelper.RotateRight(a, 10);
  AssertTrue(TIntHelper.Same(a, Sample));
  TIntHelper.RotateRight(a, -10);
  AssertTrue(TIntHelper.Same(a, Sample));
end;

procedure TNumArrayHelperTest.RotateStatic;
type
  TIntArray10 = array[1..10] of Integer;
const
  Sample: TIntArray10 = (0, 1, 2, 3, 4, 5, 6, 7, 8, 9);
  RolSample: TIntArray10 = (4, 5, 6, 7, 8, 9, 0, 1, 2, 3);
  RorSample: TIntArray10 = (6, 7, 8, 9, 0, 1, 2, 3, 4, 5);
var
  a: TIntArray10;
begin
  a := Sample;
  TIntHelper.RotateLeft(a, 4);
  AssertTrue(TIntHelper.Same(a, RolSample));
  a := Sample;
  TIntHelper.RotateLeft(a, -4);
  AssertTrue(TIntHelper.Same(a, RorSample));
  a := Sample;
  TIntHelper.RotateRight(a, 4);
  AssertTrue(TIntHelper.Same(a, RorSample));
  a := Sample;
  TIntHelper.RotateRight(a, -4);
  AssertTrue(TIntHelper.Same(a, RolSample));
  TIntHelper.RotateLeft(a, -4);
  AssertTrue(TIntHelper.Same(a, Sample));
end;

procedure TNumArrayHelperTest.RotateDyn;
type
  TIntArray10 = array[1..10] of Integer;
const
  Sample: TIntArray10 = (0, 1, 2, 3, 4, 5, 6, 7, 8, 9);
  RolSample: TIntArray10 = (4, 5, 6, 7, 8, 9, 0, 1, 2, 3);
  RorSample: TIntArray10 = (6, 7, 8, 9, 0, 1, 2, 3, 4, 5);
var
  a: TIntArray;
begin
  a := TIntHelper.CreateCopy(Sample);
  TIntHelper.RotateLeft(a, 4);
  AssertTrue(TIntHelper.Same(a, RolSample));
  a := TIntHelper.CreateCopy(Sample);
  TIntHelper.RotateLeft(a, -4);
  AssertTrue(TIntHelper.Same(a, RorSample));
  a := TIntHelper.CreateCopy(Sample);
  TIntHelper.RotateRight(a, 4);
  AssertTrue(TIntHelper.Same(a, RorSample));
  a := TIntHelper.CreateCopy(Sample);
  TIntHelper.RotateRight(a, -4);
  AssertTrue(TIntHelper.Same(a, RolSample));
  TIntHelper.RotateLeft(a, -4);
  AssertTrue(TIntHelper.Same(a, Sample));
end;

procedure TNumArrayHelperTest.SequentSearchEmpty;
var
  a: TIntArray = nil;
begin
  AssertTrue(TIntHelper.SequentSearch(a, 13) = -1);
end;

procedure TNumArrayHelperTest.SequentSearchStaticSuccess1;
begin
  AssertTrue(TIntHelper.SequentSearch(IntSrc10[3..3], -15) = 0);
end;

procedure TNumArrayHelperTest.SequentSearchStaticUnsuccess1;
begin
  AssertTrue(TIntHelper.SequentSearch(IntSrc10[3..3], 10) = -1);
end;

procedure TNumArrayHelperTest.SequentSearchDynSuccess1;
begin
  AssertTrue(TIntHelper.SequentSearch(TIntArray.Create(-15), -15) = 0);
end;

procedure TNumArrayHelperTest.SequentSearchDynUnsuccess1;
begin
  AssertTrue(TIntHelper.SequentSearch(TIntArray.Create(-15), 10) = -1);
end;

procedure TNumArrayHelperTest.SequentSearchStaticSuccess10;
begin
  AssertTrue(TIntHelper.SequentSearch(IntSrc10, -1) = 9);
end;

procedure TNumArrayHelperTest.SequentSearchStaticUnsuccess10;
begin
  AssertTrue(TIntHelper.SequentSearch(IntSrc10, 11) = -1);
end;

procedure TNumArrayHelperTest.BinarySearchEmpty;
var
  a: TIntArray;
begin
  AssertTrue(TIntHelper.BinarySearch(a{%H-}, 13) = -1);
end;

procedure TNumArrayHelperTest.BinarySearchStaticSuccess1;
begin
  AssertTrue(TIntHelper.BinarySearch(IntSrc10[3..3], -15) = 0);
end;

procedure TNumArrayHelperTest.BinarySearchStaticUnsuccess1;
begin
  AssertTrue(TIntHelper.BinarySearch(IntSrc10[3..3], 10) = -1);
end;

procedure TNumArrayHelperTest.BinarySearchStaticSuccess2;
begin
  AssertTrue(TIntHelper.BinarySearch(IntSorted10Uniq[3..4], 3) = 1);
end;

procedure TNumArrayHelperTest.BinarySearchStaticUnsuccess2;
begin
  AssertTrue(TIntHelper.BinarySearch(IntSorted10Uniq[3..4], 2) = -1);
end;

procedure TNumArrayHelperTest.BinarySearchStaticSuccess10Uniq;
begin
  AssertTrue(TIntHelper.BinarySearch(IntSorted10Uniq, 14) = 4);
end;

procedure TNumArrayHelperTest.BinarySearchStaticUnsuccess10Uniq;
begin
  AssertTrue(TIntHelper.BinarySearch(IntSorted10Uniq, 11) = -1);
end;

procedure TNumArrayHelperTest.BinarySearchStaticSuccess20;
var
  I: SizeInt;
begin
  I := TIntHelper.BinarySearch(IntSorted20, 2);
  AssertTrue((I = 5) or (I = 6));
end;

procedure TNumArrayHelperTest.BinarySearchStaticSuccess20_1;
begin
  AssertTrue(TIntHelper.BinarySearch(IntSorted20, 8) = 10);
end;

procedure TNumArrayHelperTest.BinarySearchStaticUnsuccess20;
begin
  AssertTrue(TIntHelper.BinarySearch(IntSorted20, -8) = -1);
end;

procedure TNumArrayHelperTest.BinarySearchStaticUnsuccess20_1;
begin
  AssertTrue(TIntHelper.BinarySearch(IntSorted20, 9) = -1);
end;

procedure TNumArrayHelperTest.BinarySearchStaticSuccess20_2;
begin
  AssertTrue(TIntHelper.BinarySearch(IntSorted20, -5) = 0);
end;

procedure TNumArrayHelperTest.BinarySearchStaticSuccess20_3;
begin
  AssertTrue(TIntHelper.BinarySearch(IntSorted20, 17) = 13);
end;

procedure TNumArrayHelperTest.BinarySearchStaticUnsuccessEqual10;
begin
  AssertTrue(TIntHelper.BinarySearch(IntEqual10, 5) = -1);
end;

procedure TNumArrayHelperTest.BinarySearchStaticSuccessEqual10;
begin
  AssertTrue(TIntHelper.BinarySearch(IntEqual10, 9) = 0);
end;

procedure TNumArrayHelperTest.BinarySearchDynSuccess1;
begin
  AssertTrue(TIntHelper.BinarySearch(TIntHelper.CreateCopy(IntSrc10[3..3]), -15) = 0);
end;

procedure TNumArrayHelperTest.BinarySearchDynUnsuccess1;
begin
  AssertTrue(TIntHelper.BinarySearch(TIntHelper.CreateCopy(IntSrc10[3..3]), 10) = -1);
end;

procedure TNumArrayHelperTest.BinarySearchDynSuccess2;
begin
  AssertTrue(TIntHelper.BinarySearch(TIntHelper.CreateCopy(IntSorted10Uniq[3..4]), 3) = 1);
end;

procedure TNumArrayHelperTest.BinarySearchDynUnsuccess2;
begin
  AssertTrue(TIntHelper.BinarySearch(TIntHelper.CreateCopy(IntSorted10Uniq[3..4]), 2) = -1);
end;

procedure TNumArrayHelperTest.BinarySearchDynSuccess10Uniq;
begin
  AssertTrue(TIntHelper.BinarySearch(TIntHelper.CreateCopy(IntSorted10Uniq), 281) = 8);
end;

procedure TNumArrayHelperTest.BinarySearchDynUnsuccess10Uniq;
begin
  AssertTrue(TIntHelper.BinarySearch(TIntHelper.CreateCopy(IntSorted10Uniq), 15) = -1);
end;

procedure TNumArrayHelperTest.BinarySearchDynSuccess20;
var
  I: SizeInt;
begin
  I := TIntHelper.BinarySearch(TIntHelper.CreateCopy(IntSorted20), 2);
  AssertTrue((I = 5) or (I = 6));
end;

procedure TNumArrayHelperTest.BinarySearchDynSuccess20_1;
begin
  AssertTrue(TIntHelper.BinarySearch(TIntHelper.CreateCopy(IntSorted20), 8) = 10);
end;

procedure TNumArrayHelperTest.BinarySearchDynUnsuccess20;
begin
  AssertTrue(TIntHelper.BinarySearch(TIntHelper.CreateCopy(IntSorted20), -8) = -1);
end;

procedure TNumArrayHelperTest.BinarySearchDynUnsuccess20_1;
begin
  AssertTrue(TIntHelper.BinarySearch(TIntHelper.CreateCopy(IntSorted20), 9) = -1);
end;

procedure TNumArrayHelperTest.BinarySearchDynSuccess20_2;
begin
  AssertTrue(TIntHelper.BinarySearch(TIntHelper.CreateCopy(IntSorted20), -5) = 0);
end;

procedure TNumArrayHelperTest.BinarySearchDynSuccess20_3;
begin
  AssertTrue(TIntHelper.BinarySearch(TIntHelper.CreateCopy(IntSorted20), 17) = 13);
end;

procedure TNumArrayHelperTest.BinarySearchDynUnsuccessEqual10;
begin
  AssertTrue(TIntHelper.BinarySearch(TIntHelper.CreateCopy(IntEqual10), 5) = -1);
end;

procedure TNumArrayHelperTest.BinarySearchDynSuccessEqual10;
begin
  AssertTrue(TIntHelper.BinarySearch(TIntHelper.CreateCopy(IntEqual10), 9) = 0);
end;

procedure TNumArrayHelperTest.BinarySearchPos;
var
  sr: TSearchResult;
begin
  sr := TIntHelper.BinarySearchPos([], 0);
  AssertTrue(sr.FoundIndex = -1);
  AssertTrue(sr.InsertIndex = 0);
  sr := TIntHelper.BinarySearchPos([1], 0);
  AssertTrue(sr.FoundIndex = -1);
  AssertTrue(sr.InsertIndex = 0);
  sr := TIntHelper.BinarySearchPos([1], 2);
  AssertTrue(sr.FoundIndex = -1);
  AssertTrue(sr.InsertIndex = 1);
  sr := TIntHelper.BinarySearchPos([1], 1);
  AssertTrue(sr.FoundIndex = 0);
  AssertTrue(sr.InsertIndex = 1);
end;

procedure TNumArrayHelperTest.BinarySearchPos1;
var
  sr: TSearchResult;
begin
  sr := TIntHelper.BinarySearchPos(IntEqual10, 5);
  AssertTrue(sr.FoundIndex = -1);
  AssertTrue(sr.InsertIndex = 0);
  sr := TIntHelper.BinarySearchPos(IntEqual10, 10);
  AssertTrue(sr.FoundIndex = -1);
  AssertTrue(sr.InsertIndex = 10);
  sr := TIntHelper.BinarySearchPos(IntEqual10, 9);
  AssertTrue(sr.FoundIndex = 9);
  AssertTrue(sr.InsertIndex = 10);
end;

procedure TNumArrayHelperTest.BinarySearchPos2;
var
  sr: TSearchResult;
begin
  sr := TIntHelper.BinarySearchPos(IntSorted20, -6);
  AssertTrue(sr.FoundIndex = -1);
  AssertTrue(sr.InsertIndex = 0);
  sr := TIntHelper.BinarySearchPos(IntSorted20, 41);
  AssertTrue(sr.FoundIndex = -1);
  AssertTrue(sr.InsertIndex = 20);
  sr := TIntHelper.BinarySearchPos(IntSorted20, -5);
  AssertTrue(sr.FoundIndex = 1);
  AssertTrue(sr.InsertIndex = 2);
  sr := TIntHelper.BinarySearchPos(IntSorted20, 4);
  AssertTrue(sr.FoundIndex = -1);
  AssertTrue(sr.InsertIndex = 7);
end;

procedure TNumArrayHelperTest.BinarySearchPos3;
var
  sr: TSearchResult;
  a: array of Integer;
begin
  a := TIntHelper.CreateReverseCopy(IntSorted20);
  sr := TIntHelper.BinarySearchPos(a, -6);
  AssertTrue(sr.FoundIndex = -1);
  AssertTrue(sr.InsertIndex = 20);
  sr := TIntHelper.BinarySearchPos(a, 41);
  AssertTrue(sr.FoundIndex = -1);
  AssertTrue(sr.InsertIndex = 0);
  sr := TIntHelper.BinarySearchPos(a, -5);
  AssertTrue(sr.FoundIndex = 19);
  AssertTrue(sr.InsertIndex = 20);
  sr := TIntHelper.BinarySearchPos(a, 4);
  AssertTrue(sr.FoundIndex = -1);
  AssertTrue(sr.InsertIndex = 13);
end;

procedure TNumArrayHelperTest.IndexOfMinEmpty;
var
  a: TIntArray;
begin
  AssertTrue(TIntHelper.IndexOfMin(a{%H-}) = -1);
end;

procedure TNumArrayHelperTest.IndexOfMinStatic1;
begin
  AssertTrue(TIntHelper.IndexOfMin(IntSrc10[3..3]) = 0);
end;

procedure TNumArrayHelperTest.IndexOfMinStatic2;
begin
  AssertTrue(TIntHelper.IndexOfMin(IntSrc10[3..4]) = 0);
end;

procedure TNumArrayHelperTest.IndexOfMinStatic10;
begin
  AssertTrue(TIntHelper.IndexOfMin(IntSrc10) = 4);
end;

procedure TNumArrayHelperTest.IndexOfMinDyn1;
begin
  AssertTrue(TIntHelper.IndexOfMin(TIntHelper.CreateCopy(IntSrc10[3..3])) = 0);
end;

procedure TNumArrayHelperTest.IndexOfMinDyn2;
begin
  AssertTrue(TIntHelper.IndexOfMin(TIntHelper.CreateCopy(IntSrc10[3..4])) = 0);
end;

procedure TNumArrayHelperTest.IndexOfMinDyn10;
begin
  AssertTrue(TIntHelper.IndexOfMin(TIntHelper.CreateCopy(IntSrc10)) = 4);
end;

procedure TNumArrayHelperTest.IndexOfMaxEmpty;
var
  a: TIntArray;
begin
  AssertTrue(TIntHelper.IndexOfMax(a{%H-}) = -1);
end;

procedure TNumArrayHelperTest.IndexOfMaxStatic1;
begin
  AssertTrue(TIntHelper.IndexOfMax(IntSrc10[3..3]) = 0);
end;

procedure TNumArrayHelperTest.IndexOfMaxStatic2;
begin
  AssertTrue(TIntHelper.IndexOfMax(IntSrc10[3..4]) = 1);
end;

procedure TNumArrayHelperTest.IndexOfMaxStatic10;
begin
  AssertTrue(TIntHelper.IndexOfMax(IntSrc10) = 2);
end;

procedure TNumArrayHelperTest.IndexOfMaxDyn1;
begin
  AssertTrue(TIntHelper.IndexOfMax(TIntHelper.CreateCopy(IntSrc10[3..3])) = 0);
end;

procedure TNumArrayHelperTest.IndexOfMaxDyn2;
begin
  AssertTrue(TIntHelper.IndexOfMax(TIntHelper.CreateCopy(IntSrc10[3..4])) = 1);
end;

procedure TNumArrayHelperTest.IndexOfMaxDyn10;
begin
  AssertTrue(TIntHelper.IndexOfMax(TIntHelper.CreateCopy(IntSrc10)) = 2);
end;

procedure TNumArrayHelperTest.GetMinOfEmpty;
var
  a: TIntArray;
begin
  AssertFalse(TIntHelper.GetMin(a{%H-}).Assigned);
end;

procedure TNumArrayHelperTest.GetMinOfStatic1;
begin
  AssertTrue(TIntHelper.GetMin(IntSrc10[5..5]).Value = 21);
end;

procedure TNumArrayHelperTest.GetMinOfStatic2;
begin
  AssertTrue(TIntHelper.GetMin(IntSrc10[7..8]).Value = 1);
end;

procedure TNumArrayHelperTest.GetMinOfStatic10;
begin
  AssertTrue(TIntHelper.GetMin(IntSrc10).Value = -199);
end;

procedure TNumArrayHelperTest.GetMinOfDyn1;
begin
  AssertTrue(TIntHelper.GetMin(TIntHelper.CreateCopy(IntSrc10[5..5])).Value = 21);
end;

procedure TNumArrayHelperTest.GetMinOfDyn2;
begin
  AssertTrue(TIntHelper.GetMin(TIntHelper.CreateCopy(IntSrc10[7..8])).Value = 1);
end;

procedure TNumArrayHelperTest.GetMinOfDyn10;
begin
  AssertTrue(TIntHelper.GetMin(TIntHelper.CreateCopy(IntSrc10)).Value = -199);
end;

procedure TNumArrayHelperTest.GetMaxOfEmpty;
var
  a: TIntArray;
begin
  AssertFalse(TIntHelper.GetMax(a{%H-}).Assigned);
end;

procedure TNumArrayHelperTest.GetMaxOfStatic1;
begin
  AssertTrue(TIntHelper.GetMax(IntSrc10[9..9]).Value = 0);
end;

procedure TNumArrayHelperTest.GetMaxOfStatic2;
begin
  AssertTrue(TIntHelper.GetMax(IntSrc10[7..8]).Value = 13);
end;

procedure TNumArrayHelperTest.GetMaxOfStatic10;
begin
  AssertTrue(TIntHelper.GetMax(IntSrc10).Value = 117);
end;

procedure TNumArrayHelperTest.GetMaxOfDyn1;
begin
  AssertTrue(TIntHelper.GetMax(TIntHelper.CreateCopy(IntSrc10[9..9])).Value = 0);
end;

procedure TNumArrayHelperTest.GetMaxOfDyn2;
begin
  AssertTrue(TIntHelper.GetMax(TIntHelper.CreateCopy(IntSrc10[7..8])).Value = 13);
end;

procedure TNumArrayHelperTest.GetMaxOfDyn10;
begin
  AssertTrue(TIntHelper.GetMax(TIntHelper.CreateCopy(IntSrc10)).Value = 117);
end;

procedure TNumArrayHelperTest.FindMinOfEmpty;
var
  a: TIntArray;
  m: Integer;
begin
  AssertFalse(TIntHelper.FindMin(a{%H-}, m));
end;

procedure TNumArrayHelperTest.FindMinOfStatic1;
var
  m: Integer;
begin
  AssertTrue(TIntHelper.FindMin(IntSrc10[7..7], m));
  AssertTrue(m = 1);
end;

procedure TNumArrayHelperTest.FindMinOfStatic2;
var
  m: Integer;
begin
  AssertTrue(TIntHelper.FindMin(IntSrc10[2..3], m));
  AssertTrue(m = -15);
end;

procedure TNumArrayHelperTest.FindMinOfStatic10;
var
  m: Integer;
begin
  AssertTrue(TIntHelper.FindMin(IntSrc10, m));
  AssertTrue(m = -199);
end;

procedure TNumArrayHelperTest.FindMinOfDyn1;
var
  m: Integer;
begin
  AssertTrue(TIntHelper.FindMin(TIntHelper.CreateCopy(IntSrc10[7..7]), m));
  AssertTrue(m = 1);
end;

procedure TNumArrayHelperTest.FindMinOfDyn2;
var
  m: Integer;
begin
  AssertTrue(TIntHelper.FindMin(TIntHelper.CreateCopy(IntSrc10[2..3]), m));
  AssertTrue(m = -15);
end;

procedure TNumArrayHelperTest.FindMinOfDyn10;
var
  m: Integer;
begin
  AssertTrue(TIntHelper.FindMin(TIntHelper.CreateCopy(IntSrc10), m));
  AssertTrue(m = -199);
end;

procedure TNumArrayHelperTest.FindMaxOfEmpty;
var
  a: TIntArray;
  m: Integer;
begin
  AssertFalse(TIntHelper.FindMax(a{%H-}, m));
end;

procedure TNumArrayHelperTest.FindMaxOfStatic1;
var
  m: Integer;
begin
  AssertTrue(TIntHelper.FindMax(IntSrc10[7..7], m));
  AssertTrue(m = 1);
end;

procedure TNumArrayHelperTest.FindMaxOfStatic2;
var
  m: Integer;
begin
  AssertTrue(TIntHelper.FindMax(IntSrc10[2..3], m));
  AssertTrue(m = 10);
end;

procedure TNumArrayHelperTest.FindMaxOfStatic10;
var
  m: Integer;
begin
  AssertTrue(TIntHelper.FindMax(IntSrc10, m));
  AssertTrue(m = 117);
end;

procedure TNumArrayHelperTest.FindMaxOfDyn1;
var
  m: Integer;
begin
  AssertTrue(TIntHelper.FindMax(TIntHelper.CreateCopy(IntSrc10[7..7]), m));
  AssertTrue(m = 1);
end;

procedure TNumArrayHelperTest.FindMaxOfDyn2;
var
  m: Integer;
begin
  AssertTrue(TIntHelper.FindMax(TIntHelper.CreateCopy(IntSrc10[2..3]), m));
  AssertTrue(m = 10);
end;

procedure TNumArrayHelperTest.FindMaxOfDyn10;
var
  m: Integer;
begin
  AssertTrue(TIntHelper.FindMax(TIntHelper.CreateCopy(IntSrc10), m));
  AssertTrue(m = 117);
end;

procedure TNumArrayHelperTest.FindMinMaxOfEmpty;
var
  a: TIntArray;
  mn, mx: Integer;
begin
  AssertFalse(TIntHelper.FindMinMax(a{%H-}, mn, mx));
end;

procedure TNumArrayHelperTest.FindMinMaxOfStatic1;
var
  mn, mx: Integer;
begin
  AssertTrue(TIntHelper.FindMinMax(IntSrc10[7..7], mn, mx));
  AssertTrue(mn = 1);
  AssertTrue(mx = 1);
end;

procedure TNumArrayHelperTest.FindMinMaxOfStatic2;
var
  mn, mx: Integer;
begin
  AssertTrue(TIntHelper.FindMinMax(IntSrc10[2..3], mn, mx));
  AssertTrue(mn = -15);
  AssertTrue(mx = 10);
end;

procedure TNumArrayHelperTest.FindMinMaxOfStatic10;
var
  mn, mx: Integer;
begin
  AssertTrue(TIntHelper.FindMinMax(IntSrc10, mn, mx));
  AssertTrue(mn = -199);
  AssertTrue(mx = 117);
end;

procedure TNumArrayHelperTest.FindMinMaxOfDyn1;
var
  mn, mx: Integer;
begin
  AssertTrue(TIntHelper.FindMinMax(TIntHelper.CreateCopy(IntSrc10[7..7]), mn, mx));
  AssertTrue(mn = 1);
  AssertTrue(mx = 1);
end;

procedure TNumArrayHelperTest.FindMinMaxOfDyn2;
var
  mn, mx: Integer;
begin
  AssertTrue(TIntHelper.FindMinMax(TIntHelper.CreateCopy(IntSrc10[2..3]), mn, mx));
  AssertTrue(mn = -15);
  AssertTrue(mx = 10);
end;

procedure TNumArrayHelperTest.FindMinMaxOfDyn10;
var
  mn, mx: Integer;
begin
  AssertTrue(TIntHelper.FindMinMax(TIntHelper.CreateCopy(IntSrc10), mn, mx));
  AssertTrue(mn = -199);
  AssertTrue(mx = 117);
end;

procedure TNumArrayHelperTest.FindNthSmallestOfEmpty;
var
  a: TIntArray;
  v: Integer;
begin
  AssertFalse(TIntHelper.FindNthSmallest(a{%H-}, 1, v));
end;

procedure TNumArrayHelperTest.FindNthSmallestOfStatic1;
var
  v: Integer;
begin
  AssertTrue(TIntHelper.FindNthSmallest(InOrderSrc21[3..3], 0, v));
  AssertTrue(v = 3);
end;

procedure TNumArrayHelperTest.FindNthSmallestOfStatic1_1;
var
  v: Integer;
begin
  AssertTrue(TIntHelper.FindNthSmallest(InOrderSrc21[5..5], -1, v));
  AssertTrue(v = 5);
end;

procedure TNumArrayHelperTest.FindNthSmallestOfStatic1_2;
var
  v: Integer;
begin
  AssertTrue(TIntHelper.FindNthSmallest(InOrderSrc21[2..2], 2, v));
  AssertTrue(v = 2);
end;

procedure TNumArrayHelperTest.FindNthSmallestOfStatic21;
var
  v: Integer;
  a: TIntArray21;
begin
  a := InOrderSrc21;
  TIntHelper.RandomShuffle(a);
  AssertTrue(TIntHelper.FindNthSmallest(a, 0, v));
  AssertTrue(v = 0);
end;

procedure TNumArrayHelperTest.FindNthSmallestOfStatic21_1;
var
  v: Integer;
  a: TIntArray21;
begin
  a := InOrderSrc21;
  TIntHelper.RandomShuffle(a);
  AssertTrue(TIntHelper.FindNthSmallest(a, -15, v));
  AssertTrue(v = 0);
end;

procedure TNumArrayHelperTest.FindNthSmallestOfStatic21_2;
var
  v: Integer;
  a: TIntArray21;
begin
  a := InOrderSrc21;
  TIntHelper.RandomShuffle(a);
  AssertTrue(TIntHelper.FindNthSmallest(a, 20, v));
  AssertTrue(v = 20);
end;

procedure TNumArrayHelperTest.FindNthSmallestOfStatic21_3;
var
  v: Integer;
  a: TIntArray21;
begin
  a := InOrderSrc21;
  TIntHelper.RandomShuffle(a);
  AssertTrue(TIntHelper.FindNthSmallest(a, 22, v));
  AssertTrue(v = 20);
end;

procedure TNumArrayHelperTest.FindNthSmallestOfStatic21_4;
var
  v: Integer;
  a: TIntArray21;
begin
  a := InOrderSrc21;
  TIntHelper.RandomShuffle(a);
  AssertTrue(TIntHelper.FindNthSmallest(a, 11, v));
  AssertTrue(v = 11);
end;

procedure TNumArrayHelperTest.FindNthSmallestOfStatic21_5;
var
  v: Integer;
  a: TIntArray21;
begin
  a := InOrderSrc21;
  TIntHelper.RandomShuffle(a);
  AssertTrue(TIntHelper.FindNthSmallest(a, 1, v));
  AssertTrue(v = 1);
end;

procedure TNumArrayHelperTest.FindNthSmallestOfStatic21_6;
var
  v: Integer;
  a: TIntArray21;
begin
  a := InOrderSrc21;
  TIntHelper.RandomShuffle(a);
  AssertTrue(TIntHelper.FindNthSmallest(a, 19, v));
  AssertTrue(v = 19);
end;

procedure TNumArrayHelperTest.FindNthSmallestOfStatic21_7;
var
  v: Integer;
  a: TIntArray21;
begin
  a := InOrderDblSrc21;
  TIntHelper.RandomShuffle(a);
  AssertTrue(TIntHelper.FindNthSmallest(a, 10, v));
  AssertTrue(v = 5);
end;

procedure TNumArrayHelperTest.FindNthSmallestOfStatic21_8;
var
  v: Integer;
  a: TIntArray21;
begin
  a := InOrderDblSrc21;
  TIntHelper.RandomShuffle(a);
  AssertTrue(TIntHelper.FindNthSmallest(a, 11, v));
  AssertTrue(v = 5);
end;

procedure TNumArrayHelperTest.FindNthSmallestOfDyn1;
var
  v: Integer;
begin
  AssertTrue(TIntHelper.FindNthSmallest(TIntHelper.CreateCopy(InOrderSrc21[6..6]), 0, v));
  AssertTrue(v = 6);
end;

procedure TNumArrayHelperTest.FindNthSmallestOfDyn1_1;
var
  v: Integer;
begin
  AssertTrue(TIntHelper.FindNthSmallest(TIntHelper.CreateCopy(InOrderSrc21[6..6]), -3, v));
  AssertTrue(v = 6);
end;

procedure TNumArrayHelperTest.FindNthSmallestOfDyn1_2;
var
  v: Integer;
begin
  AssertTrue(TIntHelper.FindNthSmallest(TIntHelper.CreateCopy(InOrderSrc21[6..6]), 4, v));
  AssertTrue(v = 6);
end;

procedure TNumArrayHelperTest.FindNthSmallestOfDyn21;
var
  v: Integer;
  a: TIntArray;
begin
  a := TIntHelper.CreateRandomShuffle(InOrderSrc21);
  AssertTrue(TIntHelper.FindNthSmallest(a, 0, v));
  AssertTrue(v = 0);
end;

procedure TNumArrayHelperTest.FindNthSmallestOfDyn21_1;
var
  v: Integer;
  a: TIntArray;
begin
  a := TIntHelper.CreateRandomShuffle(InOrderSrc21);
  AssertTrue(TIntHelper.FindNthSmallest(a, -3, v));
  AssertTrue(v = 0);
end;

procedure TNumArrayHelperTest.FindNthSmallestOfDyn21_2;
var
  v: Integer;
  a: TIntArray;
begin
  a := TIntHelper.CreateRandomShuffle(InOrderSrc21);
  AssertTrue(TIntHelper.FindNthSmallest(a, 20, v));
  AssertTrue(v = 20);
end;

procedure TNumArrayHelperTest.FindNthSmallestOfDyn21_3;
var
  v: Integer;
  a: TIntArray;
begin
  a := TIntHelper.CreateRandomShuffle(InOrderSrc21);
  AssertTrue(TIntHelper.FindNthSmallest(a, 22, v));
  AssertTrue(v = 20);
end;

procedure TNumArrayHelperTest.FindNthSmallestOfDyn21_4;
var
  v: Integer;
  a: TIntArray;
begin
  a := TIntHelper.CreateRandomShuffle(InOrderSrc21);
  AssertTrue(TIntHelper.FindNthSmallest(a, 14, v));
  AssertTrue(v = 14);
end;

procedure TNumArrayHelperTest.FindNthSmallestOfDyn21_5;
var
  v: Integer;
  a: TIntArray;
begin
  a := TIntHelper.CreateRandomShuffle(InOrderSrc21);
  AssertTrue(TIntHelper.FindNthSmallest(a, 1, v));
  AssertTrue(v = 1);
end;

procedure TNumArrayHelperTest.FindNthSmallestOfDyn21_6;
var
  v: Integer;
  a: TIntArray;
begin
  a := TIntHelper.CreateRandomShuffle(InOrderSrc21);
  AssertTrue(TIntHelper.FindNthSmallest(a, 19, v));
  AssertTrue(v = 19);
end;

procedure TNumArrayHelperTest.FindNthSmallestOfDyn21_7;
var
  v: Integer;
  a: TIntArray;
begin
  a := TIntHelper.CreateRandomShuffle(InOrderDblSrc21);
  AssertTrue(TIntHelper.FindNthSmallest(a, 14, v));
  AssertTrue(v = 7);
end;

procedure TNumArrayHelperTest.FindNthSmallestOfDyn21_8;
var
  v: Integer;
  a: TIntArray;
begin
  a := TIntHelper.CreateRandomShuffle(InOrderDblSrc21);
  AssertTrue(TIntHelper.FindNthSmallest(a, 15, v));
  AssertTrue(v = 7);
end;

procedure TNumArrayHelperTest.NthSmallestOfEmpty;
var
  a: TIntArray;
begin
  AssertFalse(TIntHelper.NthSmallest(a{%H-}, 1).Assigned);
end;

procedure TNumArrayHelperTest.NthSmallestOfStatic1;
begin
  AssertTrue(TIntHelper.NthSmallest(InOrderSrc21[3..3], 0).Value = 3);
end;

procedure TNumArrayHelperTest.NthSmallestOfStatic1_1;
begin
  AssertTrue(TIntHelper.NthSmallest(InOrderSrc21[5..5], -1).Value = 5);
end;

procedure TNumArrayHelperTest.NthSmallestOfStatic1_2;
begin
  AssertTrue(TIntHelper.NthSmallest(InOrderSrc21[2..2], 2).Value = 2);
end;

procedure TNumArrayHelperTest.NthSmallestOfStatic21;
var
  a: TIntArray21;
begin
  a := InOrderSrc21;
  TIntHelper.RandomShuffle(a);
  AssertTrue(TIntHelper.NthSmallest(a, 0).Value = 0);
end;

procedure TNumArrayHelperTest.NthSmallestOfStatic21_1;
var
  a: TIntArray21;
begin
  a := InOrderSrc21;
  TIntHelper.RandomShuffle(a);
  AssertTrue(TIntHelper.NthSmallest(a, -15).Value = 0);
end;

procedure TNumArrayHelperTest.NthSmallestOfStatic21_2;
var
  a: TIntArray21;
begin
  a := InOrderSrc21;
  TIntHelper.RandomShuffle(a);
  AssertTrue(TIntHelper.NthSmallest(a, 20).Value = 20);
end;

procedure TNumArrayHelperTest.NthSmallestOfStatic21_3;
var
  a: TIntArray21;
begin
  a := InOrderSrc21;
  TIntHelper.RandomShuffle(a);
  AssertTrue(TIntHelper.NthSmallest(a, 22).Value = 20);
end;

procedure TNumArrayHelperTest.NthSmallestOfStatic21_4;
var
  a: TIntArray21;
begin
  a := InOrderSrc21;
  TIntHelper.RandomShuffle(a);
  AssertTrue(TIntHelper.NthSmallest(a, 11).Value = 11);
end;

procedure TNumArrayHelperTest.NthSmallestOfStatic21_5;
var
  a: TIntArray21;
begin
  a := InOrderSrc21;
  TIntHelper.RandomShuffle(a);
  AssertTrue(TIntHelper.NthSmallest(a, 1).Value = 1);
end;

procedure TNumArrayHelperTest.NthSmallestOfStatic21_6;
var
  a: TIntArray21;
begin
  a := InOrderSrc21;
  TIntHelper.RandomShuffle(a);
  AssertTrue(TIntHelper.NthSmallest(a, 19).Value = 19);
end;

procedure TNumArrayHelperTest.NthSmallestOfStatic21_7;
var
  a: TIntArray21;
begin
  a := InOrderDblSrc21;
  TIntHelper.RandomShuffle(a);
  AssertTrue(TIntHelper.NthSmallest(a, 10).Value = 5);
end;

procedure TNumArrayHelperTest.NthSmallestOfStatic21_8;
var
  a: TIntArray21;
begin
  a := InOrderDblSrc21;
  TIntHelper.RandomShuffle(a);
  AssertTrue(TIntHelper.NthSmallest(a, 11).Value = 5);
end;

procedure TNumArrayHelperTest.NthSmallestOfDyn1;
begin
  AssertTrue(TIntHelper.NthSmallest(TIntHelper.CreateCopy(InOrderSrc21[6..6]), 0).Value = 6);
end;

procedure TNumArrayHelperTest.NthSmallestOfDyn1_1;
begin
  AssertTrue(TIntHelper.NthSmallest(TIntHelper.CreateCopy(InOrderSrc21[6..6]), -3).Value = 6);
end;

procedure TNumArrayHelperTest.NthSmallestOfDyn1_2;
begin
  AssertTrue(TIntHelper.NthSmallest(TIntHelper.CreateCopy(InOrderSrc21[6..6]), 4).Value = 6);
end;

procedure TNumArrayHelperTest.NthSmallestOfDyn21;
var
  a: TIntArray;
begin
  a := TIntHelper.CreateRandomShuffle(InOrderSrc21);
  AssertTrue(TIntHelper.NthSmallest(a, 0).Value = 0);
end;

procedure TNumArrayHelperTest.NthSmallestOfDyn21_1;
var
  a: TIntArray;
begin
  a := TIntHelper.CreateRandomShuffle(InOrderSrc21);
  AssertTrue(TIntHelper.NthSmallest(a, -3).Value = 0);
end;

procedure TNumArrayHelperTest.NthSmallestOfDyn21_2;
var
  a: TIntArray;
begin
  a := TIntHelper.CreateRandomShuffle(InOrderSrc21);
  AssertTrue(TIntHelper.NthSmallest(a, 20).Value = 20);
end;

procedure TNumArrayHelperTest.NthSmallestOfDyn21_3;
var
  a: TIntArray;
begin
  a := TIntHelper.CreateRandomShuffle(InOrderSrc21);
  AssertTrue(TIntHelper.NthSmallest(a, 23).Value = 20);
end;

procedure TNumArrayHelperTest.NthSmallestOfDyn21_4;
var
  a: TIntArray;
begin
  a := TIntHelper.CreateRandomShuffle(InOrderSrc21);
  AssertTrue(TIntHelper.NthSmallest(a, 14).Value = 14);
end;

procedure TNumArrayHelperTest.NthSmallestOfDyn21_5;
var
  a: TIntArray;
begin
  a := TIntHelper.CreateRandomShuffle(InOrderSrc21);
  AssertTrue(TIntHelper.NthSmallest(a, 1).Value = 1);
end;

procedure TNumArrayHelperTest.NthSmallestOfDyn21_6;
var
  a: TIntArray;
begin
  a := TIntHelper.CreateRandomShuffle(InOrderSrc21);
  AssertTrue(TIntHelper.NthSmallest(a, 19).Value = 19);
end;

procedure TNumArrayHelperTest.NthSmallestOfDyn21_7;
var
  a: TIntArray;
begin
  a := TIntHelper.CreateRandomShuffle(InOrderDblSrc21);
  AssertTrue(TIntHelper.NthSmallest(a, 14).Value = 7);
end;

procedure TNumArrayHelperTest.NthSmallestOfDyn21_8;
var
  a: TIntArray;
begin
  a := TIntHelper.CreateRandomShuffle(InOrderDblSrc21);
  AssertTrue(TIntHelper.NthSmallest(a, 15).Value = 7);
end;

procedure TNumArrayHelperTest.FindNthSmallestNDOfStatic21;
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

procedure TNumArrayHelperTest.FindNthSmallestNDOfStatic21_1;
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

procedure TNumArrayHelperTest.FindNthSmallestNDOfDyn21;
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

procedure TNumArrayHelperTest.FindNthSmallestNDOfDyn21_1;
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

procedure TNumArrayHelperTest.NthSmallestNDOfStatic21;
var
  a, b: TIntArray21;
begin
  a := InOrderSrc21;
  TIntHelper.RandomShuffle(a);
  b := a;
  AssertTrue(TIntHelper.NthSmallestND(a, 9).Value = 9);
  AssertTrue(TIntHelper.Same(a, b));
end;

procedure TNumArrayHelperTest.NthSmallestNDOfStatic21_1;
var
  a, b: TIntArray21;
begin
  a := InOrderSrc21;
  TIntHelper.RandomShuffle(a);
  b := a;
  AssertTrue(TIntHelper.NthSmallestND(a, 13).Value = 13);
  AssertTrue(TIntHelper.Same(a, b));
end;

procedure TNumArrayHelperTest.NthSmallestNDOfDyn21;
var
  a, b: TIntArray;
begin
  a := TIntHelper.CreateRandomShuffle(InOrderSrc21);
  b := TIntHelper.CreateCopy(a);
  AssertTrue(TIntHelper.NthSmallestND(a, 5).Value = 5);
  AssertTrue(TIntHelper.Same(a, b));
end;

procedure TNumArrayHelperTest.NthSmallestNDOfDyn21_1;
var
  a, b: TIntArray;
begin
  a := TIntHelper.CreateRandomShuffle(InOrderSrc21);
  b := TIntHelper.CreateCopy(a);
  AssertTrue(TIntHelper.NthSmallestND(a, 15).Value = 15);
  AssertTrue(TIntHelper.Same(a, b));
end;

procedure TNumArrayHelperTest.NextPermutation2AscOfEmpty;
var
  a: TIntArray;
begin
  AssertFalse(TIntHelper.NextPermutation2Asc(a{%H-}));
end;

procedure TNumArrayHelperTest.NextPermutation2AscOfStatic1;
var
  a: array[0..0] of Integer = (10);
begin
  AssertFalse(TIntHelper.NextPermutation2Asc(a));
end;

procedure TNumArrayHelperTest.NextPermutation2AscOfStatic2Asc;
var
  a: array[0..1] of Integer = (5, 8);
begin
  AssertFalse(TIntHelper.NextPermutation2Asc(a));
end;

procedure TNumArrayHelperTest.NextPermutation2AscOfStatic2Desc;
const
  c: array[0..1] of Integer = (3, 11);
var
  a: array[0..1] of Integer = (11, 3);
begin
  AssertTrue(TIntHelper.NextPermutation2Asc(a));
  AssertTrue(TIntHelper.Same(a, c));
end;

procedure TNumArrayHelperTest.NextPermutation2AscOfStatic10Equal;
var
  a: TIntArray10;
begin
  a := IntEqual10;
  AssertFalse(TIntHelper.NextPermutation2Asc(a));
end;

procedure TNumArrayHelperTest.NextPermutation2AscOfStatic3;
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

procedure TNumArrayHelperTest.NextPermutation2AscOfStatic4;
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

procedure TNumArrayHelperTest.NextPermutation2AscOfDyn1;
begin
  AssertFalse(TIntHelper.NextPermutation2Asc(TIntArray.Create(25)));
end;

procedure TNumArrayHelperTest.NextPermutation2AscOfDyn2Asc;
begin
  AssertFalse(TIntHelper.NextPermutation2Asc(TIntArray.Create(13, 17)));
end;

procedure TNumArrayHelperTest.NextPermutation2AscOfDyn2Desc;
begin
  AssertTrue(TIntHelper.NextPermutation2Asc(TIntArray.Create(21, 11)));
end;

procedure TNumArrayHelperTest.NextPermutation2AscOfDyn10Equal;
begin
  AssertFalse(TIntHelper.NextPermutation2Asc(TIntHelper.CreateCopy(IntEqual10)));
end;

procedure TNumArrayHelperTest.NextPermutation2AscOfDyn3;
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

procedure TNumArrayHelperTest.NextPermutation2AscOfDyn4;
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

procedure TNumArrayHelperTest.NextPermutation2DescOfEmpty;
var
  a: TIntArray;
begin
  AssertFalse(TIntHelper.NextPermutation2Desc(a{%H-}));
end;

procedure TNumArrayHelperTest.NextPermutation2DescOfStatic1;
var
  a: array[0..0] of Integer = (10);
begin
  AssertFalse(TIntHelper.NextPermutation2Desc(a));
end;

procedure TNumArrayHelperTest.NextPermutation2DescOfStatic2Asc;
const
  c: array[0..1] of Integer = (8, 5);
var
  a: array[0..1] of Integer = (5, 8);
begin
  AssertTrue(TIntHelper.NextPermutation2Desc(a));
  AssertTrue(TIntHelper.Same(a, c));
end;

procedure TNumArrayHelperTest.NextPermutation2DescOfStatic2Desc;
var
  a: array[0..1] of Integer = (11, 3);
begin
  AssertFalse(TIntHelper.NextPermutation2Desc(a));
end;

procedure TNumArrayHelperTest.NextPermutation2DescOfStatic10Equal;
var
  a: TIntArray10;
begin
  a := IntEqual10;
  AssertFalse(TIntHelper.NextPermutation2Desc(a));
end;

procedure TNumArrayHelperTest.NextPermutation2DescOfStatic3;
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

procedure TNumArrayHelperTest.NextPermutation2DescOfStatic4;
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

procedure TNumArrayHelperTest.NextPermutation2DescOfDyn1;
begin
  AssertFalse(TIntHelper.NextPermutation2Desc(TIntArray.Create(10)));
end;

procedure TNumArrayHelperTest.NextPermutation2DescOfDyn2Asc;
const
  c: array[0..1] of Integer = (8, 5);
var
  a: TIntArray;
begin
  a := TIntArray.Create(5, 8);
  AssertTrue(TIntHelper.NextPermutation2Desc(a));
  AssertTrue(TIntHelper.Same(a, c));
end;

procedure TNumArrayHelperTest.NextPermutation2DescOfDyn2Desc;
begin
  AssertFalse(TIntHelper.NextPermutation2Desc(TIntArray.Create(11, 3)));
end;

procedure TNumArrayHelperTest.NextPermutation2DescOfDyn10Equal;
begin
  AssertFalse(TIntHelper.NextPermutation2Desc(TIntHelper.CreateCopy(IntEqual10)));
end;

procedure TNumArrayHelperTest.NextPermutation2DescOfDyn3;
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

procedure TNumArrayHelperTest.NextPermutation2DescOfDyn4;
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

procedure TNumArrayHelperTest.IsNonDescendingOfEmpty;
var
  a: TIntArray;
begin
  AssertTrue(TIntHelper.IsNonDescending(a{%H-}));
end;

procedure TNumArrayHelperTest.IsNonDescendingOfStatic1;
begin
  AssertTrue(TIntHelper.IsNonDescending(IntSrc10[4..4]));
end;

procedure TNumArrayHelperTest.IsNonDescendingOfStatic10Equal;
begin
  AssertTrue(TIntHelper.IsNonDescending(IntEqual10));
end;

procedure TNumArrayHelperTest.IsNonDescendingOfStatic21Success;
begin
  AssertTrue(TIntHelper.IsNonDescending(InOrderSrc21));
end;

procedure TNumArrayHelperTest.IsNonDescendingOfStatic21Success_1;
begin
  AssertTrue(TIntHelper.IsNonDescending(InOrderDblSrc21));
end;

procedure TNumArrayHelperTest.IsNonDescendingOfStatic21Unsuccess;
var
  a: TIntArray21;
begin
  a := InOrderSrc21;
  TIntHelper.Reverse(a[4..5]);
  AssertFalse(TIntHelper.IsNonDescending(a));
end;

procedure TNumArrayHelperTest.IsNonDescendingOfStatic21Unsuccess_1;
var
  a: TIntArray21;
begin
  a := InOrderDblSrc21;
  TIntHelper.Reverse(a[5..7]);
  AssertFalse(TIntHelper.IsNonDescending(a));
end;

procedure TNumArrayHelperTest.IsNonDescendingOfDyn1;
begin
  AssertTrue(TIntHelper.IsNonDescending(TIntArray.Create(111)));
end;

procedure TNumArrayHelperTest.IsNonDescendingOfDyn10Equal;
begin
  AssertTrue(TIntHelper.IsNonDescending(TIntHelper.CreateCopy(IntEqual10)));
end;

procedure TNumArrayHelperTest.IsNonDescendingOfDyn21Success;
begin
  AssertTrue(TIntHelper.IsNonDescending(TIntHelper.CreateCopy(InOrderSrc21)));
end;

procedure TNumArrayHelperTest.IsNonDescendingOfDyn21Success_1;
begin
  AssertTrue(TIntHelper.IsNonDescending(TIntHelper.CreateCopy(InOrderDblSrc21)));
end;

procedure TNumArrayHelperTest.IsNonDescendingOfDyn21Unsuccess;
var
  a: TIntArray;
begin
  a := TIntHelper.CreateCopy(InOrderSrc21);
  TIntHelper.Reverse(a[5..6]);
  AssertFalse(TIntHelper.IsNonDescending(a));
end;

procedure TNumArrayHelperTest.IsNonDescendingOfDyn21Unsuccess_1;
var
  a: TIntArray;
begin
  a := TIntHelper.CreateCopy(InOrderDblSrc21);
  TIntHelper.Reverse(a[6..8]);
  AssertFalse(TIntHelper.IsNonDescending(a));
end;

procedure TNumArrayHelperTest.IsStrictAscendingOfEmpty;
var
  a: TIntArray;
begin
  AssertFalse(TIntHelper.IsStrictAscending(a{%H-}));
end;

procedure TNumArrayHelperTest.IsStrictAscendingOfStatic1;
begin
  AssertFalse(TIntHelper.IsStrictAscending(IntSrc10[3..3]));
end;

procedure TNumArrayHelperTest.IsStrictAscendingOfStatic10Equal;
begin
  AssertFalse(TIntHelper.IsStrictAscending(IntEqual10));
end;

procedure TNumArrayHelperTest.IsStrictAscendingOfStatic21Success;
begin
  AssertTrue(TIntHelper.IsStrictAscending(InOrderSrc21));
end;

procedure TNumArrayHelperTest.IsStrictAscendingOfStatic21Unsuccess;
begin
  AssertFalse(TIntHelper.IsStrictAscending(InOrderDblSrc21));
end;

procedure TNumArrayHelperTest.IsStrictAscendingOfDyn1;
begin
  AssertFalse(TIntHelper.IsStrictAscending(TIntArray.Create(14)));
end;

procedure TNumArrayHelperTest.IsStrictAscendingOfDyn10Equal;
begin
  AssertFalse(TIntHelper.IsStrictAscending(TIntHelper.CreateCopy(IntEqual10)));
end;

procedure TNumArrayHelperTest.IsStrictAscendingOfDyn21Success;
begin
   AssertTrue(TIntHelper.IsStrictAscending(TIntHelper.CreateCopy(InOrderSrc21)));
end;

procedure TNumArrayHelperTest.IsStrictAscendingOfDyn21Unsuccess;
begin
  AssertFalse(TIntHelper.IsStrictAscending(TIntHelper.CreateCopy(InOrderDblSrc21)));
end;

procedure TNumArrayHelperTest.IsNonAscendingOfEmpty;
var
  a: TIntArray;
begin
  AssertTrue(TIntHelper.IsNonAscending(a{%H-}));
end;

procedure TNumArrayHelperTest.IsNonAscendingOfStatic1;
begin
  AssertTrue(TIntHelper.IsNonAscending(IntSrc10[9..9]));
end;

procedure TNumArrayHelperTest.IsNonAscendingOfStatic10Equal;
begin
  AssertTrue(TIntHelper.IsNonAscending(IntEqual10));
end;

procedure TNumArrayHelperTest.IsNonAscendingOfStatic21Success;
var
  a: TIntArray21;
begin
  a := InOrderSrc21;
  TIntHelper.Reverse(a);
  AssertTrue(TIntHelper.IsNonAscending(a));
end;

procedure TNumArrayHelperTest.IsNonAscendingOfStatic21Success_1;
var
  a: TIntArray21;
begin
  a := InOrderDblSrc21;
  TIntHelper.Reverse(a);
  AssertTrue(TIntHelper.IsNonAscending(a));
end;

procedure TNumArrayHelperTest.IsNonAscendingOfStatic21Unsuccess;
var
  a: TIntArray21;
begin
  a := InOrderSrc21;
  AssertFalse(TIntHelper.IsNonAscending(a));
end;

procedure TNumArrayHelperTest.IsNonAscendingOfStatic21Unsuccess_1;
var
  a: TIntArray21;
begin
  a := InOrderDblSrc21;
  AssertFalse(TIntHelper.IsNonAscending(a));
end;

procedure TNumArrayHelperTest.IsNonAscendingOfDyn1;
begin
  AssertTrue(TIntHelper.IsNonAscending(TIntArray.Create(21)));
end;

procedure TNumArrayHelperTest.IsNonAscendingOfDyn10Equal;
begin
  AssertTrue(TIntHelper.IsNonAscending(TIntHelper.CreateCopy(IntEqual10)));
end;

procedure TNumArrayHelperTest.IsNonAscendingOfDyn21Success;
begin
  AssertTrue(TIntHelper.IsNonAscending(TIntHelper.CreateReverseCopy(InOrderSrc21)));
end;

procedure TNumArrayHelperTest.IsNonAscendingOfDyn21Success_1;
begin
  AssertTrue(TIntHelper.IsNonAscending(TIntHelper.CreateReverseCopy(InOrderDblSrc21)));
end;

procedure TNumArrayHelperTest.IsNonAscendingOfDyn21Unsuccess;
begin
  AssertFalse(TIntHelper.IsNonAscending(TIntHelper.CreateCopy(InOrderSrc21)));
end;

procedure TNumArrayHelperTest.IsNonAscendingOfDyn21Unsuccess_1;
begin
  AssertFalse(TIntHelper.IsNonAscending(TIntHelper.CreateCopy(InOrderDblSrc21)));
end;

procedure TNumArrayHelperTest.IsStrictDescendingOfEmpty;
var
  a: TIntArray;
begin
  AssertFalse(TIntHelper.IsStrictDescending(a{%H-}));
end;

procedure TNumArrayHelperTest.IsStrictDescendingOfStatic1;
begin
  AssertFalse(TIntHelper.IsStrictDescending(IntSrc10[2..2]));
end;

procedure TNumArrayHelperTest.IsStrictDescendingOfStatic10Equal;
begin
  AssertFalse(TIntHelper.IsStrictDescending(IntEqual10));
end;

procedure TNumArrayHelperTest.IsStrictDescendingOfStatic21Success;
var
  a: TIntArray21;
begin
  a := InOrderSrc21;
  TIntHelper.Reverse(a);
  AssertTrue(TIntHelper.IsStrictDescending(a));
end;

procedure TNumArrayHelperTest.IsStrictDescendingOfStatic21Unsuccess;
var
  a: TIntArray21;
begin
  a := InOrderDblSrc21;
  TIntHelper.Reverse(a);
  AssertFalse(TIntHelper.IsStrictDescending(a));
end;

procedure TNumArrayHelperTest.IsStrictDescendingOfDyn1;
begin
  AssertFalse(TIntHelper.IsStrictDescending(TIntArray.Create(37)));
end;

procedure TNumArrayHelperTest.IsStrictDescendingOfDyn10Equal;
begin
  AssertFalse(TIntHelper.IsStrictDescending(TIntHelper.CreateCopy(IntEqual10)));
end;

procedure TNumArrayHelperTest.IsStrictDescendingOfDyn21Success;
begin
  AssertTrue(TIntHelper.IsStrictDescending(TIntHelper.CreateReverseCopy(InOrderSrc21)));
end;

procedure TNumArrayHelperTest.IsStrictDescendingOfDyn21Unsuccess;
begin
  AssertFalse(TIntHelper.IsStrictDescending(TIntHelper.CreateReverseCopy(InOrderDblSrc21)));
end;

procedure TNumArrayHelperTest.InversionCount;
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

procedure TNumArrayHelperTest.InversionCountND;
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

procedure TNumArrayHelperTest.SameOfEmpty;
var
  a, b: TIntArray;
begin
  AssertTrue(TIntHelper.Same(a{%H-}, b{%H-}));
end;

procedure TNumArrayHelperTest.SameOfStatic1Success;
begin
  AssertTrue(TIntHelper.Same(IntSrc10[9..9], IntSrc7[1..1]));
end;

procedure TNumArrayHelperTest.SameOfStatic1Unsuccess;
begin
  AssertFalse(TIntHelper.Same(IntSrc10[8..8], IntSrc7[3..3]));
end;

procedure TNumArrayHelperTest.SameOfStatic2Success;
begin
  AssertTrue(TIntHelper.Same(InOrderSrc21[0..1], InOrderDblSrc21[1..2]));
end;

procedure TNumArrayHelperTest.SameOfStatic2Unsuccess;
begin
  AssertFalse(TIntHelper.Same(InOrderSrc21[1..2], InOrderDblSrc21[1..2]));
end;

procedure TNumArrayHelperTest.SameOfStatic21Success;
var
  a: TIntArray21;
begin
  a := InOrderSrc21;
  AssertTrue(TIntHelper.Same(InOrderSrc21, a));
end;

procedure TNumArrayHelperTest.SameOfStatic21Unsuccess;
var
  a: TIntArray21;
begin
  a := InOrderSrc21;
  AssertFalse(TIntHelper.Same(InOrderSrc21[1..20], a));
end;

procedure TNumArrayHelperTest.SameOfDyn1Success;
begin
  AssertTrue(TIntHelper.Same(TIntArray.Create(12), TIntArray.Create(12)));
end;

procedure TNumArrayHelperTest.SameOfDyn1Unsuccess;
begin
  AssertFalse(TIntHelper.Same(TIntArray.Create(14), TIntArray.Create(15)));
end;

procedure TNumArrayHelperTest.SameOfDyn2Success;
begin
  AssertTrue(TIntHelper.Same(TIntArray.Create(11, 12), TIntArray.Create(11, 12)));
end;

procedure TNumArrayHelperTest.SameOfDyn2Unsuccess;
begin
  AssertFalse(TIntHelper.Same(TIntArray.Create(11, 14), TIntArray.Create(11, 15)));
end;

procedure TNumArrayHelperTest.SameOfDyn21Success;
var
  a, b: TIntArray;
begin
  a := TIntHelper.CreateCopy(InOrderSrc21);
  b := TIntHelper.CreateCopy(InOrderSrc21);
  AssertTrue(TIntHelper.Same(a, b));
end;

procedure TNumArrayHelperTest.SameOfDyn21Unsuccess; //IntSorted10Uniq
var
  a, b: TIntArray;
begin
  a := TIntHelper.CreateCopy(InOrderSrc21);
  TIntHelper.Reverse(a[2..3]);
  b := TIntHelper.CreateCopy(InOrderSrc21);
  AssertFalse(TIntHelper.Same(a, b));
end;

procedure TNumArrayHelperTest.QuickSortOfEmpty;
var
  a: TIntArray;
begin
  //should not crash
  TIntHelper.QuickSort(a{%H-});
  AssertTrue(a = nil);
end;

procedure TNumArrayHelperTest.QuickSortAscOfStatic3Asc;
const
  c: array[1..3] of Integer = (9, 13, 13);
var
  a: array[0..2] of Integer = (9, 13, 13);
begin
  TIntHelper.QuickSort(a);
  AssertTrue(TIntHelper.Same(a, c));
end;

procedure TNumArrayHelperTest.QuickSortAscOfStatic3Desc;
const
  c: array[1..3] of Integer = (9, 13, 13);
var
  a: array[0..2] of Integer = (13, 13, 9);
begin
  TIntHelper.QuickSort(a);
  AssertTrue(TIntHelper.Same(a, c));
end;

procedure TNumArrayHelperTest.QuickSortDescOfStatic3Desc;
const
  c: array[1..3] of Integer = (27, 11, 5);
var
  a: array[0..2] of Integer = (27, 11, 5);
begin
  TIntHelper.QuickSort(a, soDesc);
  AssertTrue(TIntHelper.Same(a, c));
end;

procedure TNumArrayHelperTest.QuickSortDescOfStatic3Asc;
const
  c: array[1..3] of Integer = (27, 11, 5);
var
  a: array[0..2] of Integer = (11, 5, 27);
begin
  TIntHelper.QuickSort(a, soDesc);
  AssertTrue(TIntHelper.Same(a, c));
end;

procedure TNumArrayHelperTest.QuickSortAscOfStatic377;
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

procedure TNumArrayHelperTest.QuickSortDescOfStatic377;
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

procedure TNumArrayHelperTest.QuickSortAscOfStatic377Random;
var
  I: Integer;
  a: array[1..377] of Integer;
begin
  for I := 1 to 377 do
    a[I] := Random(Integer.MaxValue);
  TIntHelper.QuickSort(a);
  AssertTrue(TIntHelper.IsNonDescending(a));
end;

procedure TNumArrayHelperTest.QuickSortAscOfStatic377RandomZeroes;
var
  I: Integer;
  a: array[1..377] of Integer;
begin
  for I := 1 to 377 do
    a[I] := Random(2);
  TIntHelper.QuickSort(a);
  AssertTrue(TIntHelper.IsNonDescending(a));
end;

procedure TNumArrayHelperTest.QuickSortDescOfStatic377Random;
var
  I: Integer;
  a: array[1..377] of Integer;
begin
  for I := 1 to 377 do
    a[I] := Random(Integer.MaxValue);
  TIntHelper.QuickSort(a, soDesc);
  AssertTrue(TIntHelper.IsNonAscending(a));
end;

procedure TNumArrayHelperTest.QuickSortDescOfStatic377RandomZeroes;
var
  I: Integer;
  a: array[1..377] of Integer;
begin
  for I := 1 to 377 do
    a[I] := Random(2);
  TIntHelper.QuickSort(a, soDesc);
  AssertTrue(TIntHelper.IsNonAscending(a));
end;

procedure TNumArrayHelperTest.QuickSortAscOfDyn3Asc;
const
  c: array[1..3] of Integer = (11, 19, 22);
var
  a: TIntArray;
begin
  a := TIntArray.Create(11, 19, 22);
  TIntHelper.QuickSort(a);
  AssertTrue(TIntHelper.Same(a, c));
end;

procedure TNumArrayHelperTest.QuickSortAscOfDyn3Desc;
const
  c: array[1..3] of Integer = (11, 19, 22);
var
  a: TIntArray;
begin
  a := TIntArray.Create(22, 11, 19);
  TIntHelper.QuickSort(a);
  AssertTrue(TIntHelper.Same(a, c));
end;

procedure TNumArrayHelperTest.QuickSortDescOfDyn3Desc;
const
  c: array[1..3] of Integer = (31, 14, 3);
var
  a: TIntArray;
begin
  a := TIntArray.Create(31, 14, 3);
  TIntHelper.QuickSort(a, soDesc);
  AssertTrue(TIntHelper.Same(a, c));
end;

procedure TNumArrayHelperTest.QuickSortDescOfDyn3Asc;
const
  c: array[1..3] of Integer = (31, 14, 3);
var
  a: TIntArray;
begin
  a := TIntArray.Create(14, 3, 31);
  TIntHelper.QuickSort(a, soDesc);
  AssertTrue(TIntHelper.Same(a, c));
end;

procedure TNumArrayHelperTest.QuickSortAscOfDyn377;
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

procedure TNumArrayHelperTest.QuickSortDescOfDyn377;
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

procedure TNumArrayHelperTest.QuickSortAscOfDyn377Random;
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

procedure TNumArrayHelperTest.QuickSortAscOfDyn377RandomZeroes;
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

procedure TNumArrayHelperTest.QuickSortDescOfDyn377Random;
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

procedure TNumArrayHelperTest.QuickSortDescOfDyn377RandomZeroes;
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

procedure TNumArrayHelperTest.HeapSortOfStatic277;
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

procedure TNumArrayHelperTest.HeapSortOfStatic277Random;
var
  I: Integer;
  a: array[1..277] of Integer;
begin
  for I := 1 to 277 do
    a[I] := Random(Integer.MaxValue);
  THackHelper.DoHeapSort(@a[1], 276);
  AssertTrue(TIntHelper.IsNonDescending(a));
end;

procedure TNumArrayHelperTest.HeapSortOfStatic277RandomZeroes;
var
  I: Integer;
  a: array[1..277] of Integer;
begin
  for I := 1 to 277 do
    a[I] := Random(2);
  THackHelper.DoHeapSort(@a[1], 276);
  AssertTrue(TIntHelper.IsNonDescending(a));
end;

procedure TNumArrayHelperTest.HeapSortOfDyn277;
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

procedure TNumArrayHelperTest.HeapSortOfDyn277Random;
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

procedure TNumArrayHelperTest.HeapSortOfDyn277RandomZeroes;
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

procedure TNumArrayHelperTest.IntroSortOfEmpty;
var
  a: TIntArray;
begin
  //should not crash
  TIntHelper.IntroSort(a{%H-});
  AssertTrue(a = nil);
end;

procedure TNumArrayHelperTest.IntroSortAscOfStatic3Asc;
const
  c: array[1..3] of Integer = (9, 13, 13);
var
  a: array[0..2] of Integer = (9, 13, 13);
begin
  TIntHelper.IntroSort(a);
  AssertTrue(TIntHelper.Same(a, c));
end;

procedure TNumArrayHelperTest.IntroSortAscOfStatic3Desc;
const
  c: array[1..3] of Integer = (9, 13, 13);
var
  a: array[0..2] of Integer = (13, 13, 9);
begin
  TIntHelper.IntroSort(a);
  AssertTrue(TIntHelper.Same(a, c));
end;

procedure TNumArrayHelperTest.IntroSortDescOfStatic3Desc;
const
  c: array[1..3] of Integer = (27, 11, 5);
var
  a: array[0..2] of Integer = (27, 11, 5);
begin
  TIntHelper.IntroSort(a, soDesc);
  AssertTrue(TIntHelper.Same(a, c));
end;

procedure TNumArrayHelperTest.IntroSortDescOfStatic3Asc;
const
  c: array[1..3] of Integer = (27, 11, 5);
var
  a: array[0..2] of Integer = (11, 5, 27);
begin
  TIntHelper.IntroSort(a, soDesc);
  AssertTrue(TIntHelper.Same(a, c));
end;

procedure TNumArrayHelperTest.IntroSortAscOfStatic577;
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

procedure TNumArrayHelperTest.IntroSortDescOfStatic577;
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

procedure TNumArrayHelperTest.IntroSortAscOfStatic577Random;
var
  I: Integer;
  a: array[1..577] of Integer;
begin
  for I := 1 to 577 do
    a[I] := Random(Integer.MaxValue);
  TIntHelper.IntroSort(a);
  AssertTrue(TIntHelper.IsNonDescending(a));
end;

procedure TNumArrayHelperTest.IntroSortAscOfStatic577RandomZeroes;
var
  I: Integer;
  a: array[1..577] of Integer;
begin
  for I := 1 to 577 do
    a[I] := Random(2);
  TIntHelper.IntroSort(a);
  AssertTrue(TIntHelper.IsNonDescending(a));
end;

procedure TNumArrayHelperTest.IntroSortDescOfStatic577Random;
var
  I: Integer;
  a: array[1..577] of Integer;
begin
  for I := 1 to 577 do
    a[I] := Random(Integer.MaxValue);
  TIntHelper.IntroSort(a, soDesc);
  AssertTrue(TIntHelper.IsNonAscending(a));
end;

procedure TNumArrayHelperTest.IntroSortDescOfStatic577RandomZeroes;
var
  I: Integer;
  a: array[1..577] of Integer;
begin
  for I := 1 to 577 do
    a[I] := Random(2);
  TIntHelper.IntroSort(a, soDesc);
  AssertTrue(TIntHelper.IsNonAscending(a));
end;

procedure TNumArrayHelperTest.IntroSortAscOfDyn3Asc;
const
  c: array[1..3] of Integer = (11, 19, 22);
var
  a: TIntArray;
begin
  a := TIntArray.Create(11, 19, 22);
  TIntHelper.IntroSort(a);
  AssertTrue(TIntHelper.Same(a, c));
end;

procedure TNumArrayHelperTest.IntroSortAscOfDyn3Desc;
const
  c: array[1..3] of Integer = (11, 19, 22);
var
  a: TIntArray;
begin
  a := TIntArray.Create(22, 11, 19);
  TIntHelper.IntroSort(a);
  AssertTrue(TIntHelper.Same(a, c));
end;

procedure TNumArrayHelperTest.IntroSortDescOfDyn3Desc;
const
  c: array[1..3] of Integer = (31, 14, 3);
var
  a: TIntArray;
begin
  a := TIntArray.Create(31, 14, 3);
  TIntHelper.IntroSort(a, soDesc);
  AssertTrue(TIntHelper.Same(a, c));
end;

procedure TNumArrayHelperTest.IntroSortDescOfDyn3Asc;
const
  c: array[1..3] of Integer = (31, 14, 3);
var
  a: TIntArray;
begin
  a := TIntArray.Create(14, 3, 31);
  TIntHelper.IntroSort(a, soDesc);
  AssertTrue(TIntHelper.Same(a, c));
end;

procedure TNumArrayHelperTest.IntroSortAscOfDyn577;
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

procedure TNumArrayHelperTest.IntroSortDescOfDyn577;
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

procedure TNumArrayHelperTest.IntroSortAscOfDyn577Random;
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

procedure TNumArrayHelperTest.IntroSortAscOfDyn577RandomZeroes;
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

procedure TNumArrayHelperTest.IntroSortDescOfDyn577Random;
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

procedure TNumArrayHelperTest.IntroSortDescOfDyn577RandomZeroes;
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

procedure TNumArrayHelperTest.DualPivotQuickSortOfEmpty;
var
  a: TIntArray;
begin
  //should not crash
  TIntHelper.DualPivotQuickSort(a{%H-});
  AssertTrue(a = nil);
end;

procedure TNumArrayHelperTest.DualPivotQuickSortAscOfStatic3Asc;
const
  c: array[1..3] of Integer = (9, 13, 13);
var
  a: array[0..2] of Integer = (9, 13, 13);
begin
  TIntHelper.DualPivotQuickSort(a);
  AssertTrue(TIntHelper.Same(a, c));
end;

procedure TNumArrayHelperTest.DualPivotQuickSortAscOfStatic3Desc;
const
  c: array[1..3] of Integer = (9, 13, 13);
var
  a: array[0..2] of Integer = (13, 13, 9);
begin
  TIntHelper.DualPivotQuickSort(a);
  AssertTrue(TIntHelper.Same(a, c));
end;

procedure TNumArrayHelperTest.DualPivotQuickSortDescOfStatic3Desc;
const
  c: array[1..3] of Integer = (27, 11, 5);
var
  a: array[0..2] of Integer = (27, 11, 5);
begin
  TIntHelper.DualPivotQuickSort(a, soDesc);
  AssertTrue(TIntHelper.Same(a, c));
end;

procedure TNumArrayHelperTest.DualPivotQuickSortDescOfStatic3Asc;
const
  c: array[1..3] of Integer = (27, 11, 5);
var
  a: array[0..2] of Integer = (11, 5, 27);
begin
  TIntHelper.DualPivotQuickSort(a, soDesc);
  AssertTrue(TIntHelper.Same(a, c));
end;

procedure TNumArrayHelperTest.DualPivotQuickSortAscOfStatic577;
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

procedure TNumArrayHelperTest.DualPivotQuickSortDescOfStatic577;
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

procedure TNumArrayHelperTest.DualPivotQuickSortAscOfStatic577Random;
var
  I: Integer;
  a: array[1..577] of Integer;
begin
  for I := 1 to 577 do
    a[I] := Random(Integer.MaxValue);
  TIntHelper.DualPivotQuickSort(a);
  AssertTrue(TIntHelper.IsNonDescending(a));
end;

procedure TNumArrayHelperTest.DualPivotQuickSortAscOfStatic577RandomZeroes;
var
  I: Integer;
  a: array[1..577] of Integer;
begin
  for I := 1 to 577 do
    a[I] := Random(2);
  TIntHelper.DualPivotQuickSort(a);
  AssertTrue(TIntHelper.IsNonDescending(a));
end;

procedure TNumArrayHelperTest.DualPivotQuickSortDescOfStatic577Random;
var
  I: Integer;
  a: array[1..577] of Integer;
begin
  for I := 1 to 577 do
    a[I] := Random(Integer.MaxValue);
  TIntHelper.DualPivotQuickSort(a, soDesc);
  AssertTrue(TIntHelper.IsNonAscending(a));
end;

procedure TNumArrayHelperTest.DualPivotQuickSortDescOfStatic577RandomZeroes;
var
  I: Integer;
  a: array[1..577] of Integer;
begin
  for I := 1 to 577 do
    a[I] := Random(2);
  TIntHelper.DualPivotQuickSort(a, soDesc);
  AssertTrue(TIntHelper.IsNonAscending(a));
end;

procedure TNumArrayHelperTest.DualPivotQuickSortAscOfDyn3Asc;
const
  c: array[1..3] of Integer = (11, 19, 22);
var
  a: TIntArray;
begin
  a := TIntArray.Create(11, 19, 22);
  TIntHelper.DualPivotQuickSort(a);
  AssertTrue(TIntHelper.Same(a, c));
end;

procedure TNumArrayHelperTest.DualPivotQuickSortAscOfDyn3Desc;
const
  c: array[1..3] of Integer = (11, 19, 22);
var
  a: TIntArray;
begin
  a := TIntArray.Create(22, 11, 19);
  TIntHelper.DualPivotQuickSort(a);
  AssertTrue(TIntHelper.Same(a, c));
end;

procedure TNumArrayHelperTest.DualPivotQuickSortDescOfDyn3Desc;
const
  c: array[1..3] of Integer = (31, 14, 3);
var
  a: TIntArray;
begin
  a := TIntArray.Create(31, 14, 3);
  TIntHelper.DualPivotQuickSort(a, soDesc);
  AssertTrue(TIntHelper.Same(a, c));
end;

procedure TNumArrayHelperTest.DualPivotQuickSortDescOfDyn3Asc;
const
  c: array[1..3] of Integer = (31, 14, 3);
var
  a: TIntArray;
begin
  a := TIntArray.Create(14, 3, 31);
  TIntHelper.DualPivotQuickSort(a, soDesc);
  AssertTrue(TIntHelper.Same(a, c));
end;

procedure TNumArrayHelperTest.DualPivotQuickSortAscOfDyn577;
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

procedure TNumArrayHelperTest.DualPivotQuickSortDescOfDyn577;
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

procedure TNumArrayHelperTest.DualPivotQuickSortAscOfDyn577Random;
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

procedure TNumArrayHelperTest.DualPivotQuickSortAscOfDyn577RandomZeroes;
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

procedure TNumArrayHelperTest.DualPivotQuickSortDescOfDyn577Random;
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

procedure TNumArrayHelperTest.DualPivotQuickSortDescOfDyn577RandomZeroes;
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

procedure TNumArrayHelperTest.PDQSortTest;
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

{ TOrdArrayHelperTest }

procedure TOrdArrayHelperTest.CreateRange;
var
  I, J: Integer;
  a: TIntArray;
begin
  a := THelper.CreateRange(0, -1);
  AssertTrue(a = nil);
  a := THelper.CreateRange(0, 0);
  AssertTrue(System.Length(a) = 1);
  J := -10;
  I := 50;
  a := THelper.CreateRange(J, I);
  AssertTrue(System.Length(a) = Succ(I - J));
  for I in a do
    begin
      AssertTrue(I = J);
      Inc(J);
    end;
end;

procedure TOrdArrayHelperTest.SortAsc;
var
  a, b: TIntArray;
begin
  a := THelper.CreateRange(0, 1000);
  b := THelper.CreateCopy(a);
  THelper.RandomShuffle(b);
  AssertFalse(THelper.Same(a, b));
  THelper.Sort(b);
  AssertTrue(THelper.Same(a, b));
end;

procedure TOrdArrayHelperTest.SortDesc;
var
  a, b: TIntArray;
begin
  a := THelper.CreateRange(0, 1000);
  THelper.Reverse(a);
  b := THelper.CreateCopy(a);
  THelper.RandomShuffle(b);
  AssertFalse(THelper.Same(a, b));
  THelper.Sort(b, soDesc);
  AssertTrue(THelper.Same(a, b));
end;

procedure TOrdArrayHelperTest.SortAsc577;
var
  I: Integer;
  a: TIntArray;
begin
  System.SetLength(a, 577);
  for I := 0 to System.High(a) do
    a[I] := Random(10000);
  THelper.Sort(a);
  AssertTrue(THelper.IsNonDescending(a));
end;

procedure TOrdArrayHelperTest.SortDesc577;
var
  I: Integer;
  a: TIntArray;
begin
  System.SetLength(a, 577);
  for I := 0 to System.High(a) do
    a[I] := Random(10000);
  THelper.Sort(a, soDesc);
  AssertTrue(THelper.IsNonAscending(a));
end;
{$IFDEF CPU64}
procedure TOrdArrayHelperTest.SortInt64Asc;
type
  TInt64Helper = specialize TGOrdinalArrayHelper<Int64>;
var
  a, a1, b: array of Int64;
  I: Integer;
begin
  System.SetLength(a, 1000);
  for I := 0 to System.High(a) do
    a[I] := High(Int64) - Random(10000);
  a1 := Copy(a);
  TInt64Helper.QuickSort(a1);
  b := TInt64Helper.Sorted(a);
  AssertFalse(TInt64Helper.Same(a, b));
  AssertTrue(TInt64Helper.Same(a1, b));
  a[0] := High(Int64);
  a[999] := Low(Int64);
  a1 := Copy(a);
  TInt64Helper.QuickSort(a1);
  b := TInt64Helper.Sorted(a);
  AssertFalse(TInt64Helper.Same(a, b));
  AssertTrue(TInt64Helper.Same(a1, b));
end;

procedure TOrdArrayHelperTest.SortInt64Desc;
type
  TInt64Helper = specialize TGOrdinalArrayHelper<Int64>;
var
  a, a1, b: TInt64Helper.TArray;
  I: Integer;
begin
  System.SetLength(a, 1000);
  for I := 0 to System.High(a) do
    a[I] := High(Int64) - Random(10000);
  a1 := Copy(a);
  TInt64Helper.QuickSort(a1, soDesc);
  b := TInt64Helper.Sorted(a, soDesc);
  AssertFalse(TInt64Helper.Same(a, b));
  AssertTrue(TInt64Helper.Same(a1, b));
  a[0] := High(Int64);
  a[999] := Low(Int64);
  a1 := Copy(a);
  TInt64Helper.QuickSort(a1, soDesc);
  b := TInt64Helper.Sorted(a, soDesc);
  AssertFalse(TInt64Helper.Same(a, b));
  AssertTrue(TInt64Helper.Same(a1, b));
end;
{$ENDIF CPU64}

initialization

  RegisterTest(TNumArrayHelperTest);
  RegisterTest(TOrdArrayHelperTest);

end.

