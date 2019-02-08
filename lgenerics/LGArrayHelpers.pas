{****************************************************************************
*                                                                           *
*   This file is part of the LGenerics package.                             *
*   Generic helper utils for arrays.                                        *
*                                                                           *
*   Copyright(c) 2018-2019 A.Koverdyaev(avk)                                *
*                                                                           *
*   This code is free software; you can redistribute it and/or modify it    *
*   under the terms of the Apache License, Version 2.0;                     *
*   You may obtain a copy of the License at                                 *
*     http://www.apache.org/licenses/LICENSE-2.0.                           *
*                                                                           *
*  Unless required by applicable law or agreed to in writing, software      *
*  distributed under the License is distributed on an "AS IS" BASIS,        *
*  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. *
*  See the License for the specific language governing permissions and      *
*  limitations under the License.                                           *
*                                                                           *
*****************************************************************************}
unit LGArrayHelpers;

{$mode objfpc}{$H+}
{$INLINE ON}
{$MODESWITCH ADVANCEDRECORDS}
{$MODESWITCH NESTEDPROCVARS}

interface

uses

  SysUtils, math,
  LGUtils,
  {%H-}LGHelpers;

type

  TSortOrder   = LGUtils.TSortOrder;
  {.$DEFINE FPC_REQUIRES_PROPER_ALIGNMENT  :for test purpose only}

  { TGArrayHelpUtil }
  generic TGArrayHelpUtil<T> = class
  public
  type
    TItem             = T;
    PItem             = ^T;
    TArray            = specialize TGArray<T>;
    TOptional         = specialize TGOptional<T>;
    TEqualCompare     = specialize TGEqualCompare<T>;
    TOnEqualCompare   = specialize TGOnEqualCompare<T>;
    TNestEqualCompare = specialize TGNestEqualCompare<T>;
    TTest             = specialize TGTest<T>;
    TOnTest           = specialize TGOnTest<T>;
    TNestTest         = specialize TGNestTest<T>;
    TFold             = specialize TGFold<T, T>;
    TOnFold           = specialize TGOnFold<T, T>;
    TNestFold         = specialize TGNestFold<T, T>;

    TSearchResult = record
      FoundIndex,
      InsertIndex: SizeInt;
    end;

  protected
  const
    HEAP_INSERT_CUTOFF  = 63;
    QUICK_INSERT_CUTOFF = 31;
    MEDIAN_OF9_CUTOFF   = 511;
    DPQ_INSERT_CUTOFF   = 47;
    INTRO_LOG_FACTOR    = 2;
  type
    //to supress unnecessary refcounting
    TFake      = {$IFNDEF FPC_REQUIRES_PROPER_ALIGNMENT}array[0..Pred(SizeOf(T))] of Byte{$ELSE}T{$ENDIF};
    TFakeArray = array of TFake;
    TSortSplit = record
      Left,
      Right: SizeInt;
    end;

    TMergeSortBase = object
    protected
    const
      MERGE_STACK_INIT_SIZE  = 16;
      MERGE_BUFFER_INIT_SIZE = 64;
      MIN_MERGE_POW          = 5;
      MIN_MERGE_LEN          = SizeInt(1) shl MIN_MERGE_POW;
    type
      TRun       = record
        Base, Count: SizeInt;
      end;
      TRunArray  = array of TRun;
    var
      FData: PItem; // pointer to data array
      FBuffer: TFakeArray;
      FStack: TRunArray;
      FStackSize: SizeInt;
      procedure PushRun(aBase, aCount: SizeInt);
      function  EnsureBufferCapacity(aSize: SizeInt): PItem;
      procedure Init(A: PItem);
      class function  MinRunLen(aTotalSize: SizeInt): SizeInt; static;
    end;

    class procedure CopyItems(aSrc, aDst: PItem; aCount: SizeInt); static;
    class procedure DoReverse(p: PItem; R: SizeInt); static;
    //class procedure DoSwap(p: PItem; L, R: SizeInt); static; inline;
  public
    class function  CreateCopy(constref A: array of T): TArray; static;
    class function  CreateReverseCopy(constref A: array of T): TArray; static;
    class function  CreateMerge(constref L, R: array of T): TArray; static;
    class function  CreateRandomShuffle(constref A: array of T): TArray; static;
    class procedure Fill(var A: array of T; constref aValue: T); static;
    class function  Append(var A: TArray; constref aValue: T): SizeInt; static;
  { if aSrc <> aDst appends aSrc to aDst, sets aSrc to nil and returns count of merged elements,
    otherwise returns 0}
    class function  Merge(var aDst, aSrc: TArray): SizeInt; static;
  { returns array of elements of A starting at aIndex(0-based) to High(A);
    length of A becomes aIndex;
    if A = nil or aIndex > High(A), then Result is empty }
    class function  Split(var A: TArray; aIndex: SizeInt): TArray; static;
  { extracts array of aCount elements of A starting at aIndex(0-based);
    if aIndex > High(A) or aCount < 1, then Result is empty;
    if aCount > length A then Result is truncated }
    class function  Extract(var A: TArray; aIndex, aCount: SizeInt): TArray; static;
    class procedure Reverse(var A: array of T); static;
  { cyclic shift of array elements by aDist positions to the left;
    the case if Abs(aDist) > Length(A) is ignored }
    class procedure RotateLeft(var A: array of T; aDist: SizeInt); static;
  { cyclic shift of array elements by aDist positions to the right;
    the case if Abs(aDist) > Length(A) is ignored }
    class procedure RotateRight(var A: array of T; aDist: SizeInt); static;
    class procedure RandomShuffle(var A: array of T); static;
  { returns 0-based position of aValue in array A, -1 if not found }
    class function  SequentSearch(constref A: array of T; constref aValue: T; c: TEqualCompare): SizeInt;
                    static;
    class function  SequentSearch(constref A: array of T; constref aValue: T; c: TOnEqualCompare): SizeInt;
                    static;
    class function  SequentSearch(constref A: array of T; constref aValue: T; c: TNestEqualCompare): SizeInt;
                    static;
  { returns True if both A and B are identical sequence of elements }
    class function  Same(constref A, B: array of T; c: TEqualCompare): Boolean; static;
    class function  Same(constref A, B: array of T; c: TOnEqualCompare): Boolean; static;
    class function  Same(constref A, B: array of T; c: TNestEqualCompare): Boolean; static;
    class function  Select(constref A: array of T; aTest: TTest): TArray;
    class function  Select(constref A: array of T; aTest: TOnTest): TArray;
    class function  Select(constref A: array of T; aTest: TNestTest): TArray;
    { left-associative linear fold }
    class function  FoldL(constref A: array of T; aFold: TFold; constref v0: T): T; static;
    class function  FoldL(constref A: array of T; aFold: TFold): TOptional; static;
    class function  FoldL(constref A: array of T; aFold: TOnFold; constref v0: T): T; static;
    class function  FoldL(constref A: array of T; aFold: TOnFold): TOptional; static;
    class function  FoldL(constref A: array of T; aFold: TNestFold; constref v0: T): T; static;
    class function  FoldL(constref A: array of T; aFold: TNestFold): TOptional; static;
  { right-associative linear fold }
    class function  FoldR(constref A: array of T; aFold: TFold; constref v0: T): T; static;
    class function  FoldR(constref A: array of T; aFold: TFold): TOptional; static;
    class function  FoldR(constref A: array of T; aFold: TOnFold; constref v0: T): T; static;
    class function  FoldR(constref A: array of T; aFold: TOnFold): TOptional; static;
    class function  FoldR(constref A: array of T; aFold: TNestFold; constref v0: T): T; static;
    class function  FoldR(constref A: array of T; aFold: TNestFold): TOptional; static;
  end;

  { TGBaseArrayHelper
      functor TCmpRel(comparision relation) must provide:
        class function Compare([const[ref]] L, R: T): SizeInt }
  generic TGBaseArrayHelper<T, TCmpRel> = class(specialize TGArrayHelpUtil<T>)
  protected
  type

    TMergeSort = object(TMergeSortBase)
    private
      procedure CollapseA;
      procedure CollapseD;
      procedure ForceCollapseA;
      procedure ForceCollapseD;
      procedure MergeAtA(aIndex: SizeInt);
      procedure MergeAtD(aIndex: SizeInt);
      procedure MergeLoA(From, CountLo, CountHi: SizeInt);
      procedure MergeLoD(From, CountLo, CountHi: SizeInt);
      procedure MergeHiA(From, CountLo, CountHi: SizeInt);
      procedure MergeHiD(From, CountLo, CountHi: SizeInt);
      class procedure InsertSortA(A: PItem; R, At: SizeInt); static;
      class procedure InsertSortD(A: PItem; R, At: SizeInt); static;
      class function  CountRunAsc(A: PItem; R: SizeInt): SizeInt; static;
      class function  CountRunDesc(A: PItem; R: SizeInt): SizeInt; static;
    public
      class procedure SortAsc(A: PItem; R: SizeInt); static;
      class procedure SortDesc(A: PItem; R: SizeInt); static;
    end;

    class function  CountRun2Asc(A: PItem; R: SizeInt): SizeInt; static;
    class procedure InsertionSort(A: PItem; R: SizeInt); static;
    class function  DoBinSearch(A: PItem; R: SizeInt; constref aValue: T): SizeInt; static;
    class function  DoBinSearchPosA(A: PItem; R: SizeInt; constref aValue: T): TSearchResult; static;
    class function  DoBinSearchPosD(A: PItem; R: SizeInt; constref aValue: T): TSearchResult; static;
    class function  DoBinSearchPos(A: PItem; R: SizeInt; constref aValue: T): TSearchResult; static;
    class procedure DoHeapSort(A: PItem; R: SizeInt); static;
    class function  QSplitR(A: PItem; R: SizeInt): TSortSplit; static;
    class procedure DoQSort(A: PItem; R: SizeInt); static;
    class function  MedianOf3(constref v1, v2, v3: T): T; static;
    class function  QSplitMo9(A: PItem; R: SizeInt): TSortSplit; static;
    class procedure DoIntroSort(A: PItem; R, Ttl: SizeInt); static;
    class function  DPQSplit(A: PItem; R: SizeInt): TSortSplit; static;
    class procedure DoDPQSort(A: PItem; R: SizeInt); static;
  { QuickSelect with random pivot, does not checks indexes }
    class function  QSelectR(A: PItem; R, N: SizeInt): T; static;
  public
  { returns 0-based position of aValue in array A, -1 if not found }
    class function  SequentSearch(constref A: array of T; constref aValue: T): SizeInt; static;
  { returns 0-based position of aValue in SORTED array A, -1 if not found }
    class function  BinarySearch(constref A: array of T; constref aValue: T): SizeInt; static;
  { returns 0-based position of aValue in SORTED array A in Result.FoundIndex(-1 if not found);
    returns position for insertion in Result.InsertIndex(-1 if array is empty);
    in case of array of single element or constant array assumes ascending state }
    class function  BinarySearchPos(constref A: array of T; constref aValue: T): TSearchResult; static;
  { returns 0-based position of minimal value in A, -1 if A is empty }
    class function  IndexOfMin(constref A: array of T): SizeInt; static;
  { returns 0-based position of maximal value in A, -1 if A is empty }
    class function  IndexOfMax(constref A: array of T): SizeInt; static;
  { returns smallest element of A in TOptional.Value if A is nonempty }
    class function  GetMin(constref A: array of T): TOptional; static;
  { returns greatest element of A in TOptional.Value if A is nonempty }
    class function  GetMax(constref A: array of T): TOptional; static;
  { returns True and smallest element of A in aValue if A is nonempty, False otherwise }
    class function  FindMin(constref A: array of T; out aValue: T): Boolean; static;
  { returns True and greatest element of A in aValue if A is nonempty, False otherwise }
    class function  FindMax(constref A: array of T; out aValue: T): Boolean; static;
  { returns True, smallest element of A in aMin and greatest element of A in aMax, if A is nonempty,
    False otherwise }
    class function  FindMinMax(constref A: array of T; out aMin, aMax: T): Boolean; static;
  { returns True and A's Nth order statistic(0-based) in aValue if A is nonempty, False otherwise;
    if N < 0 then N sets to 0; if N > High(A) then N sets to High(A);
    is destuctive: changes order of elements in A }
    class function  FindNthSmallest(var A: array of T; N: SizeInt; out aValue: T): Boolean; static;
  { returns A's Nth order statistic(0-based) in TOptional.Value if A is nonempty;
    if N < 0 then N sets to 0; if N > High(A) then N sets to High(A);
    is destuctive: changes order of elements in A }
    class function  NthSmallest(var A: array of T; N: SizeInt): TOptional; static;
  { returns True and A's Nth order statistic(0-based) in aValue if A is nonempty, False otherwise;
    if N < 0 then N sets to 0; if N > High(A) then N sets to High(A);
    is nondestuctive: creates temp copy of A }
    class function  FindNthSmallestND(constref A: array of T; N: SizeInt; out aValue: T): Boolean; static;
  { returns A's Nth order statistic(0-based) in TOptional.Value if A is nonempty;
    if N < 0 then N sets to 0; if N > High(A) then N sets to High(A);
    is nondestuctive: creates temp copy of A }
    class function  NthSmallestND(constref A: array of T; N: SizeInt): TOptional; static;
  { returns True if permutation towards nondescending state of A has done, False otherwise }
    class function  NextPermutation2Asc(var A: array of T): Boolean; static;
  { returns True if permutation towards nonascending state of A has done, False otherwise }
    class function  NextPermutation2Desc(var A: array of T): Boolean; static;
  { note: an empty array or single element array is always nondescending }
    class function  IsNonDescending(constref A: array of T): Boolean; static;
  { note: an empty array or single element array is never strict ascending }
    class function  IsStrictAscending(constref A: array of T): Boolean; static;
  { note: an empty array or single element array is always nonascending }
    class function  IsNonAscending(constref A: array of T): Boolean; static;
  { note: an empty array or single element array is never strict descending}
    class function  IsStrictDescending(constref A: array of T): Boolean; static;
  { returns True if both A and B are identical sequence of elements }
    class function  Same(constref A, B: array of T): Boolean; static;
  { slightly modified optimized quicksort with random pivot selection }
    class procedure QuickSort(var A: array of T; o: TSortOrder = soAsc); static;
  { slightly modified Introsort with pseudo-median-of-9 pivot selection }
    class procedure IntroSort(var A: array of T; o: TSortOrder = soAsc); static;
  { slightly modified V.Yaroslavskiy proposed the dual pivot Quicksort algorithm with random pivot selection }
    class procedure DualPivotQuickSort(var A: array of T; o: TSortOrder = soAsc); static;
  { stable, adaptive mergesort inspired by Java Timsort }
    class procedure MergeSort(var A: array of T; o: TSortOrder = soAsc); static;
  { default sort algorithm, currently it is IntroSort}
    class procedure Sort(var A: array of T; o: TSortOrder = soAsc); static;
    class function  Sorted(constref A: array of T; o: TSortOrder = soAsc): TArray; static;
  { copies only distinct values from A }
    class function  SelectDistinct(constref A: array of T): TArray; static;
  end;

  { TGArrayHelper assumes that type T implements TCmpRel }
  generic TGArrayHelper<T> = class(specialize TGBaseArrayHelper<T, T>);

  {TGComparableArrayHelper assumes that type T defines comparision operators }
  generic TGComparableArrayHelper<T> = class(specialize TGArrayHelpUtil<T>)
  protected
  type
    TMergeSort = object(TMergeSortBase)
    private
      procedure CollapseA;
      procedure CollapseD;
      procedure ForceCollapseA;
      procedure ForceCollapseD;
      procedure MergeAtA(aIndex: SizeInt);
      procedure MergeAtD(aIndex: SizeInt);
      procedure MergeLoA(From, CountLo, CountHi: SizeInt);
      procedure MergeLoD(From, CountLo, CountHi: SizeInt);
      procedure MergeHiA(From, CountLo, CountHi: SizeInt);
      procedure MergeHiD(From, CountLo, CountHi: SizeInt);
      class procedure InsertSortA(A: PItem; R, At: SizeInt); static;
      class procedure InsertSortD(A: PItem; R, At: SizeInt); static;
      class function  CountRunAsc(A: PItem; R: SizeInt): SizeInt; static;
      class function  CountRunDesc(A: PItem; R: SizeInt): SizeInt; static;
    public
      class procedure SortAsc(A: PItem; R: SizeInt); static;
      class procedure SortDesc(A: PItem; R: SizeInt); static;
    end;

    class function  CountRun2Asc(A: PItem; R: SizeInt): SizeInt; static;
    class procedure InsertionSort(A: PItem; R: SizeInt); static;
    class function  DoBinSearch(A: PItem; R: SizeInt; constref aValue: T): SizeInt; static;
    class function  DoBinSearchPosA(A: PItem; R: SizeInt; constref aValue: T): TSearchResult; static;
    class function  DoBinSearchPosD(A: PItem; R: SizeInt; constref aValue: T): TSearchResult; static;
    class function  DoBinSearchPos(A: PItem; R: SizeInt; constref aValue: T): TSearchResult; static;
    class procedure DoHeapSort(A: PItem; R: SizeInt); static;
    class function  QSplitR(A: PItem; R: SizeInt): TSortSplit; static;
    class procedure DoQSort(A: PItem; R: SizeInt); static;
    class function  MedianOf3(constref v1, v2, v3: T): T; static; inline;
    class function  QSplitMo9(A: PItem; R: SizeInt): TSortSplit; static;
    class procedure DoIntroSort(A: PItem; R, Ttl: SizeInt); static;
    class function  DPQSplit(A: PItem; R: SizeInt): TSortSplit; static;
    class procedure DoDPQSort(A: PItem; R: SizeInt); static;
  { QuickSelect with random pivot, does not checks indexes }
    class function  QSelectR(A: PItem; R, N: SizeInt): T; static;
  public
  { returns 0-based position of aValue in array A, -1 if not found }
    class function  SequentSearch(constref A: array of T; constref aValue: T): SizeInt; static;
  { returns 0-based position of aValue in SORTED array A, -1 if not found }
    class function  BinarySearch(constref A: array of T; constref aValue: T): SizeInt; static;
  { returns 0-based position of aValue in SORTED array A in Result.FoundIndex(-1 if not found);
    returns position for insertion in Result.InsertIndex(-1 if array is empty);
    in case of array of single element or constant array assumes ascending state }
    class function  BinarySearchPos(constref A: array of T; constref aValue: T): TSearchResult; static;
  { returns 0-based position of minimal value in A, -1 if A is empty }
    class function  IndexOfMin(constref A: array of T): SizeInt; static;
  { returns 0-based position of maximal value in A, -1 if A is empty }
    class function  IndexOfMax(constref A: array of T): SizeInt; static;
  { returns smallest element of A in TOptional.Value if A is nonempty }
    class function  GetMin(constref A: array of T): TOptional; static;
  { returns greatest element of A in TOptional.Value if A is nonempty }
    class function  GetMax(constref A: array of T): TOptional; static;
  { retursn True and smallest element of A in aValue if A is nonempty, False otherwise }
    class function  FindMin(constref A: array of T; out aValue: T): Boolean; static;
  { returns True and  greatest element of A in aValue if A is nonempty, False otherwise }
    class function  FindMax(constref A: array of T; out aValue: T): Boolean; static;
  { returns True, smallest element of A in aMin and greatest element of A in aMax,
    if A is nonempty, False otherwise }
    class function  FindMinMax(constref A: array of T; out aMin, aMax: T): Boolean; static;
  { returns True and A's Nth order statistic(0-based) in aValue if A is nonempty, False otherwise;
    if N < 0 then N sets to 0; if N > High(A) then N sets to High(A);
    is destuctive: changes order of elements in A }
    class function  FindNthSmallest(var A: array of T; N: SizeInt; out aValue: T): Boolean; static;
  { returns A's Nth order statistic(0-based) in TOptional.Value if A is nonempty;
    if N < 0 then N sets to 0; if N > High(A) then N sets to High(A);
    is destuctive: changes order of elements in A }
    class function  NthSmallest(var A: array of T; N: SizeInt): TOptional; static;
  { returns True and A's Nth order statistic(0-based) in aValue if A is nonempty, False otherwise;
    if N < 0 then N sets to 0; if N > High(A) then N sets to High(A);
    is nondestuctive: creates temp copy of A }
    class function  FindNthSmallestND(constref A: array of T; N: SizeInt; out aValue: T): Boolean; static;
  { returns A's Nth order statistic(0-based) in TOptional.Value if A is nonempty;
    if N < 0 then N sets to 0; if N > High(A) then N sets to High(A);
    is nondestuctive: creates temp copy of A }
    class function  NthSmallestND(constref A: array of T; N: SizeInt): TOptional; static;
  { returns True if permutation towards nondescending state of A has done, False otherwise }
    class function  NextPermutation2Asc(var A: array of T): Boolean; static;
  { returns True if permutation towards nonascending state of A has done, False otherwise }
    class function  NextPermutation2Desc(var A: array of T): Boolean; static;
  { note: an empty array or single element array is always nondescending }
    class function  IsNonDescending(constref A: array of T): Boolean; static;
  { note: an empty array or single element array is never strict ascending }
    class function  IsStrictAscending(constref A: array of T): Boolean; static;
  { note: an empty array or single element array is always nonascending }
    class function  IsNonAscending(constref A: array of T): Boolean; static;
  { note: an empty array or single element array is never strict descending}
    class function  IsStrictDescending(constref A: array of T): Boolean; static;
  { returns True if both A and B are identical sequence of elements }
    class function  Same(constref A, B: array of T): Boolean; static;
  { slightly modified optimized quicksort with random pivot selection }
    class procedure QuickSort(var A: array of T; o: TSortOrder = soAsc); static;
  { slightly modified Introsort with pseudo-median-of-9 pivot selection }
    class procedure IntroSort(var A: array of T; o: TSortOrder = soAsc); static;
  { slightly modified V.Yaroslavskiy proposed dual pivot Quicksort algorithm with random pivot selection }
    class procedure DualPivotQuickSort(var A: array of T; o: TSortOrder = soAsc); static;
  { stable, adaptive mergesort inspired by Java Timsort }
    class procedure MergeSort(var A: array of T; o: TSortOrder = soAsc); static;
  { default sort algorithm, currently it is IntroSort }
    class procedure Sort(var A: array of T; o: TSortOrder = soAsc); static;
    class function  Sorted(constref A: array of T; o: TSortOrder = soAsc): TArray; static;
  { copies only distinct values from A }
    class function  SelectDistinct(constref A: array of T): TArray; static;
  end;

  { TGRegularArrayHelper: with regular comparator}
  generic TGRegularArrayHelper<T> = class(specialize TGArrayHelpUtil<T>)
  public
  type
    TCompare = specialize TGCompare<T>;

  protected
  type

    TMergeSort = object(TMergeSortBase)
    private
      FCompare: TCompare;
      procedure Init(A: PItem; c: TCompare);
      procedure CollapseA;
      procedure CollapseD;
      procedure ForceCollapseA;
      procedure ForceCollapseD;
      procedure MergeAtA(aIndex: SizeInt);
      procedure MergeAtD(aIndex: SizeInt);
      procedure MergeLoA(From, CountLo, CountHi: SizeInt);
      procedure MergeLoD(From, CountLo, CountHi: SizeInt);
      procedure MergeHiA(From, CountLo, CountHi: SizeInt);
      procedure MergeHiD(From, CountLo, CountHi: SizeInt);
      class procedure InsertSortA(A: PItem; R, At: SizeInt; c: TCompare); static;
      class procedure InsertSortD(A: PItem; R, At: SizeInt; c: TCompare); static;
      class function  CountRunAsc(A: PItem; R: SizeInt; c: TCompare): SizeInt; static;
      class function  CountRunDesc(A: PItem; R: SizeInt; c: TCompare): SizeInt; static;
    public
      class procedure SortAsc(A: PItem; R: SizeInt; c: TCompare); static;
      class procedure SortDesc(A: PItem; R: SizeInt; c: TCompare); static;
    end;

    class function  CountRun2Asc(A: PItem; R: SizeInt; c: TCompare): SizeInt; static;
    class procedure InsertionSort(A: PItem; R: SizeInt; c: TCompare); static;
    class function  DoBinSearch(A: PItem; R: SizeInt; constref aValue: T; c: TCompare): SizeInt; static;
    class function  DoBinSearchPosA(A: PItem; R: SizeInt; constref aValue: T; c: TCompare): TSearchResult;
                    static;
    class function  DoBinSearchPosD(A: PItem; R: SizeInt; constref aValue: T; c: TCompare): TSearchResult;
                    static;
    class function  DoBinSearchPos(A: PItem; R: SizeInt; constref aValue: T; c: TCompare): TSearchResult;
                    static;
    class procedure DoHeapSort(A: PItem; R: SizeInt; c: TCompare); static;
    class function  QSplitR(A: PItem; R: SizeInt; c: TCompare): TSortSplit; static;
    class procedure DoQSort(A: PItem; R: SizeInt; c: TCompare); static;
    class function  MedianOf3(constref v1, v2, v3: T; c: TCompare): T; static; inline;
    class function  QSplitMo9(A: PItem; R: SizeInt; c: TCompare): TSortSplit; static;
    class procedure DoIntroSort(A: PItem; R, Ttl: SizeInt; c: TCompare); static;
    class function  DPQSplit(A: PItem; R: SizeInt; c: TCompare): TSortSplit; static;
    class procedure DoDPQSort(A: PItem; R: SizeInt; c: TCompare); static;
  { QuickSelect with random pivot, does not checks indexes }
    class function  QSelectR(A: PItem; R, N: SizeInt; c: TCompare): T; static;
  public
  { returns 0-based position of aValue in array A, -1 if not found }
    class function  SequentSearch(constref A: array of T; constref aValue: T; c: TCompare): SizeInt; static;
  { returns 0-based position of aValue in SORTED array A, -1 if not found }
    class function  BinarySearch(constref A: array of T; constref aValue: T; c: TCompare): SizeInt; static;
  { returns 0-based position of aValue in SORTED array A in Result.FoundIndex(-1 if not found);
    returns position for insertion in Result.InsertIndex(-1 if array is empty);
    in case of array of single element or constant array assumes ascending state }
    class function  BinarySearchPos(constref A: array of T; constref aValue: T; c: TCompare): TSearchResult;
                    static;
  { returns 0-based position of minimal value in A, -1 if A is empty }
    class function  IndexOfMin(constref A: array of T; c: TCompare): SizeInt; static;
  { returns 0-based position of maximal value in A, -1 if A is empty }
    class function  IndexOfMax(constref A: array of T; c: TCompare): SizeInt; static;
  { returns smallest element of A in TOptional.Value if A <> nil }
    class function  GetMin(constref A: array of T; c: TCompare): TOptional; static;
  { returns greatest element of A in TOptional.Value if A is nonempty }
    class function  GetMax(constref A: array of T; c: TCompare): TOptional; static;
  { returns True and smallest element of A in aValue if A is nonempty, False otherwise }
    class function  FindMin(constref A: array of T; out aValue: T; c: TCompare): Boolean; static;
  { returns True and  greatest element of A in aValue if A is nonempty, False otherwise }
    class function  FindMax(constref A: array of T; out aValue: T; c: TCompare): Boolean; static;
  { returns True, smallest element of A in aMin and greatest element of A in aMax,
    if A is nonempty, False otherwise }
    class function  FindMinMax(constref A: array of T; out aMin, aMax: T; c: TCompare): Boolean; static;
  { returns True and A's Nth order statistic(0-based) in aValue if A is nonempty, False otherwise;
    if N < 0 then N sets to 0; if N > High(A) then N sets to High(A);
    is destuctive: changes order of elements in A }
    class function  FindNthSmallest(var A: array of T; N: SizeInt; out aValue: T; c: TCompare): Boolean; static;
  { returns A's Nth order statistic(0-based) in TOptional.Value if A is nonempty;
    if N < 0 then N sets to 0; if N > High(A) then N sets to High(A);
    is destuctive: changes order of elements in A }
    class function  NthSmallest(var A: array of T; N: SizeInt; c: TCompare): TOptional; static;
  { returns True and A's Nth order statistic(0-based) in aValue if A is nonempty, False otherwise;
    if N < 0 then N sets to 0; if N > High(A) then N sets to High(A);
    is nondestuctive: creates temp copy of A }
    class function  FindNthSmallestND(constref A: array of T; N: SizeInt; out aValue: T; c: TCompare): Boolean; static;
  { returns A's Nth order statistic(0-based) in TOptional.Value if A is nonempty;
    if N < 0 then N sets to 0; if N > High(A) then N sets to High(A);
    is nondestuctive: creates temp copy of A }
    class function  NthSmallestND(constref A: array of T; N: SizeInt; c: TCompare): TOptional; static;
  { returns True if permutation towards nondescending state of A has done, False otherwise }
    class function  NextPermutation2Asc(var A: array of T; c: TCompare): Boolean; static;
  { returns True if permutation towards nonascending state of A has done, False otherwise }
    class function  NextPermutation2Desc(var A: array of T; c: TCompare): Boolean; static;
  { note: an empty array or single element array is always nondescending }
    class function  IsNonDescending(constref A: array of T; c: TCompare): Boolean; static;
  { note: an empty array or single element array is never strict ascending }
    class function  IsStrictAscending(constref A: array of T; c: TCompare): Boolean; static;
  { note: an empty array or single element array is always nonascending }
    class function  IsNonAscending(constref A: array of T; c: TCompare): Boolean; static;
  { note: an empty array or single element array is never strict descending}
    class function  IsStrictDescending(constref A: array of T; c: TCompare): Boolean; static;
  { returns True if both A and B are identical sequence of elements }
    class function  Same(constref A, B: array of T; c: TCompare): Boolean; static;
  { slightly modified optimized quicksort with random pivot selection }
    class procedure QuickSort(var A: array of T; c: TCompare; o: TSortOrder = soAsc); static;
  { slightly modified Introsort with pseudo-median-of-9 pivot selection }
    class procedure IntroSort(var A: array of T; c: TCompare; o: TSortOrder = soAsc); static;
  { slightly modified V.Yaroslavskiy proposed the dual pivot Quicksort algorithm with random pivot selection }
    class procedure DualPivotQuickSort(var A: array of T; c: TCompare; o: TSortOrder = soAsc); static;
  { stable, adaptive mergesort inspired by Java Timsort }
    class procedure MergeSort(var A: array of T; c: TCompare; o: TSortOrder = soAsc); static;
  { default sort algorithm, currently it is IntroSort }
    class procedure Sort(var A: array of T; c: TCompare; o: TSortOrder = soAsc); static;
    class function  Sorted(constref A: array of T; c: TCompare; o: TSortOrder = soAsc): TArray; static;
  { copies only distinct values from A }
    class function  SelectDistinct(constref A: array of T; c: TCompare): TArray;
                    static;
  end;

  { TGDelegatedArrayHelper: with delegated comparator}
  generic TGDelegatedArrayHelper<T> = class(specialize TGArrayHelpUtil<T>)
  public
  type
    TOnCompare = specialize TGOnCompare<T>;

  protected
  type

    TMergeSort = object(TMergeSortBase)
    protected
      FCompare: TOnCompare;
      procedure Init(A: PItem; c: TOnCompare);
      procedure CollapseA;
      procedure CollapseD;
      procedure ForceCollapseA;
      procedure ForceCollapseD;
      procedure MergeAtA(aIndex: SizeInt);
      procedure MergeAtD(aIndex: SizeInt);
      procedure MergeLoA(From, CountLo, CountHi: SizeInt);
      procedure MergeLoD(From, CountLo, CountHi: SizeInt);
      procedure MergeHiA(From, CountLo, CountHi: SizeInt);
      procedure MergeHiD(From, CountLo, CountHi: SizeInt);
      class procedure InsertSortA(A: PItem; R, At: SizeInt; c: TOnCompare); static;
      class procedure InsertSortD(A: PItem; R, At: SizeInt; c: TOnCompare); static;
      class function  CountRunAsc(A: PItem; R: SizeInt; c: TOnCompare): SizeInt; static;
      class function  CountRunDesc(A: PItem; R: SizeInt; c: TOnCompare): SizeInt; static;
    public
      class procedure SortAsc(A: PItem; R: SizeInt; c: TOnCompare); static;
      class procedure SortDesc(A: PItem; R: SizeInt; c: TOnCompare); static;
    end;

    class function  CountRun2Asc(A: PItem; R: SizeInt; c: TOnCompare): SizeInt; static;
    class procedure InsertionSort(A: PItem; R: SizeInt; c: TOnCompare); static;
    class function  DoBinSearch(A: PItem; R: SizeInt; constref aValue: T; c: TOnCompare): SizeInt; static;
    class function  DoBinSearchPosA(A: PItem; R: SizeInt; constref aValue: T; c: TOnCompare): TSearchResult;
                    static;
    class function  DoBinSearchPosD(A: PItem; R: SizeInt; constref aValue: T; c: TOnCompare): TSearchResult;
                    static;
    class function  DoBinSearchPos(A: PItem; R: SizeInt; constref aValue: T; c: TOnCompare): TSearchResult;
                    static;
    class procedure DoHeapSort(A: PItem; R: SizeInt; c: TOnCompare); static;
    class function  QSplitR(A: PItem; R: SizeInt; c: TOnCompare): TSortSplit; static;
    class procedure DoQSort(A: PItem; R: SizeInt; c: TOnCompare); static;
    class function  MedianOf3(constref v1, v2, v3: T; c: TOnCompare): T; static; inline;
    class function  QSplitMo9(A: PItem; R: SizeInt; c: TOnCompare): TSortSplit; static;
    class procedure DoIntroSort(A: PItem; R, Ttl: SizeInt; c: TOnCompare); static;
    class function  DPQSplit(A: PItem; R: SizeInt; c: TOnCompare): TSortSplit; static;
    class procedure DoDPQSort(A: PItem; R: SizeInt; c: TOnCompare); static;
  { QuickSelect with random pivot, does not checks indexes }
    class function  QSelectR(A: PItem; R, N: SizeInt; c: TOnCompare): T; static;
  public
  { returns 0-based position of aValue in array A, -1 if not found }
    class function  SequentSearch(constref A: array of T; constref aValue: T; c: TOnCompare): SizeInt; static;
  { returns 0-based position of aValue in SORTED array A, -1 if not found }
    class function  BinarySearch(constref A: array of T; constref aValue: T; c: TOnCompare): SizeInt; static;
  { returns 0-based position of aValue in SORTED array A in Result.FoundIndex(-1 if not found);
    returns position for insertion in Result.InsertIndex(-1 if array is empty);
    in case of array of single element or constant array assumes ascending state }
    class function  BinarySearchPos(constref A: array of T; constref aValue: T; c: TOnCompare): TSearchResult;
                    static;
  { returns 0-based position of minimal value in A, -1 if A is empty }
    class function  IndexOfMin(constref A: array of T; c: TOnCompare): SizeInt; static;
  { returns 0-based position of maximal value in A, -1 if A is empty }
    class function  IndexOfMax(constref A: array of T; c: TOnCompare): SizeInt; static;
  { returns smallest element of A in TOptional.Value if A is nonempty }
    class function  GetMin(constref A: array of T; c: TOnCompare): TOptional; static;
  { returns greatest element of A in TOptional.Value if A is nonempty }
    class function  GetMax(constref A: array of T; c: TOnCompare): TOptional; static;
  { returns True and smallest element of A in aValue if A is nonempty, False otherwise }
    class function  FindMin(constref A: array of T; out aValue: T; c: TOnCompare): Boolean; static;
  { returns True and  greatest element of A in aValue if A is nonempty, False otherwise }
    class function  FindMax(constref A: array of T; out aValue: T; c: TOnCompare): Boolean; static;
  { returns True, smallest element of A in aMin and greatest element of A in aMax,
    if A is nonempty, False otherwise }
    class function  FindMinMax(constref A: array of T; out aMin, aMax: T; c: TOnCompare): Boolean; static;
  { returns True and A's Nth order statistic(0-based) in aValue if A is nonempty, False otherwise;
    if N < 0 then N sets to 0; if N > High(A) then N sets to High(A);
    is destuctive: changes order of elements in A }
    class function  FindNthSmallest(var A: array of T; N: SizeInt; out aValue: T; c: TOnCompare): Boolean; static;
  { returns A's Nth order statistic(0-based) in TOptional.Value if A is nonempty;
    if N < 0 then N sets to 0; if N > High(A) then N sets to High(A);
    is destuctive: changes order of elements in A }
    class function  NthSmallest(var A: array of T; N: SizeInt; c: TOnCompare): TOptional; static;
  { returns True and A's Nth order statistic(0-based) in aValue if A is nonempty, False otherwise;
    if N < 0 then N sets to 0; if N > High(A) then N sets to High(A);
    is nondestuctive: creates temp copy of A }
    class function  FindNthSmallestND(constref A: array of T;N: SizeInt; out aValue: T; c: TOnCompare): Boolean; static;
  { returns A's Nth order statistic(0-based) in TOptional.Value if A is nonempty;
    if N < 0 then N sets to 0; if N > High(A) then N sets to High(A);
    is nondestuctive: creates temp copy of A }
    class function  NthSmallestND(constref A: array of T; N: SizeInt; c: TOnCompare): TOptional; static;
  { returns True if permutation towards nondescending state of A has done, False otherwise }
    class function  NextPermutation2Asc(var A: array of T; c: TOnCompare): Boolean; static;
  { returns True if permutation towards nonascending state of A has done, False otherwise }
    class function  NextPermutation2Desc(var A: array of T; c: TOnCompare): Boolean; static;
  { note: an empty array or single element array is always nondescending }
    class function  IsNonDescending(constref A: array of T; c: TOnCompare): Boolean; static;
  { note: an empty array or single element array is never strict ascending }
    class function  IsStrictAscending(constref A: array of T; c: TOnCompare): Boolean; static;
  { note: an empty array or single element array is always nonascending }
    class function  IsNonAscending(constref A: array of T; c: TOnCompare): Boolean; static;
  { note: an empty array or single element array is never strict descending}
    class function  IsStrictDescending(constref A: array of T; c: TOnCompare): Boolean; static;
  { returns True if both A and B are identical sequence of elements }
    class function  Same(constref A, B: array of T; c: TOnCompare): Boolean; static;
  { slightly modified optimized quicksort with random pivot selection }
    class procedure QuickSort(var A: array of T; c: TOnCompare; o: TSortOrder = soAsc); static;
  { slightly modified Introsort with pseudo-median-of-9 pivot selection }
    class procedure IntroSort(var A: array of T; c: TOnCompare; o: TSortOrder = soAsc); static;
  { slightly modified V.Yaroslavskiy proposed the dual pivot Quicksort algorithm with random pivot selection }
    class procedure DualPivotQuickSort(var A: array of T; c: TOnCompare; o: TSortOrder = soAsc); static;
  { stable, adaptive mergesort inspired by Java Timsort }
    class procedure MergeSort(var A: array of T; c: TOnCompare; o: TSortOrder = soAsc); static;
  { default sort algorithm, currently it is IntroSort }
    class procedure Sort(var A: array of T; c: TOnCompare; o: TSortOrder = soAsc); static;
    class function  Sorted(constref A: array of T; c: TOnCompare; o: TSortOrder = soAsc): TArray; static;
  { copies only distinct values from A }
    class function  SelectDistinct(constref A: array of T; c: TOnCompare): TArray;
                    static;
  end;

  { TGNestedArrayHelper: with nested comparator}
  generic TGNestedArrayHelper<T> = class(specialize TGArrayHelpUtil<T>)
  public
  type
    TNestCompare = specialize TGNestCompare<T>;

  protected
  type

    TMergeSort = object(TMergeSortBase)
    protected
      FCompare: TNestCompare;
      procedure Init(A: PItem; c: TNestCompare);
      procedure CollapseA;
      procedure CollapseD;
      procedure ForceCollapseA;
      procedure ForceCollapseD;
      procedure MergeAtA(aIndex: SizeInt);
      procedure MergeAtD(aIndex: SizeInt);
      procedure MergeLoA(From, CountLo, CountHi: SizeInt);
      procedure MergeLoD(From, CountLo, CountHi: SizeInt);
      procedure MergeHiA(From, CountLo, CountHi: SizeInt);
      procedure MergeHiD(From, CountLo, CountHi: SizeInt);
      class procedure InsertSortA(A: PItem; R, At: SizeInt; c: TNestCompare); static;
      class procedure InsertSortD(A: PItem; R, At: SizeInt; c: TNestCompare); static;
      class function  CountRunAsc(A: PItem; R: SizeInt; c: TNestCompare): SizeInt; static;
      class function  CountRunDesc(A: PItem; R: SizeInt; c: TNestCompare): SizeInt; static;
    public
      class procedure SortAsc(A: PItem; R: SizeInt; c: TNestCompare); static;
      class procedure SortDesc(A: PItem; R: SizeInt; c: TNestCompare); static;
    end;

    class function  CountRun2Asc(A: PItem; R: SizeInt; c: TNestCompare): SizeInt; static;
    class procedure InsertionSort(A: PItem; R: SizeInt; c: TNestCompare); static;
    class function  DoBinSearch(A: PItem; R: SizeInt; constref aValue: T; c: TNestCompare): SizeInt; static;
    class function  DoBinSearchPosA(A: PItem; R: SizeInt; constref aValue: T; c: TNestCompare): TSearchResult;
                    static;
    class function  DoBinSearchPosD(A: PItem; R: SizeInt; constref aValue: T; c: TNestCompare): TSearchResult;
                    static;
    class function  DoBinSearchPos(A: PItem; R: SizeInt; constref aValue: T; c: TNestCompare): TSearchResult;
                    static;
    class procedure DoHeapSort(A: PItem; R: SizeInt; c: TNestCompare); static;
    class function  QSplitR(A: PItem; R: SizeInt; c: TNestCompare): TSortSplit; static;
    class procedure DoQSort(A: PItem; R: SizeInt; c: TNestCompare); static;
    class function  MedianOf3(constref v1, v2, v3: T; c: TNestCompare): T; static; inline;
    class function  QSplitMo9(A: PItem; R: SizeInt; c: TNestCompare): TSortSplit; static;
    class procedure DoIntroSort(A: PItem; R, Ttl: SizeInt; c: TNestCompare); static;
    class function  DPQSplit(A: PItem; R: SizeInt; c: TNestCompare): TSortSplit; static;
    class procedure DoDPQSort(A: PItem; R: SizeInt; c: TNestCompare); static;
  { QuickSelect with random pivot, does not checks indexes }
    class function  QSelectR(A: PItem; R, N: SizeInt; c: TNestCompare): T; static;
  public
  { returns 0-based position of aValue in array A, -1 if not found }
    class function  SequentSearch(constref A: array of T; constref aValue: T; c: TNestCompare): SizeInt; static;
  { returns 0-based position of aValue in SORTED array A, -1 if not found }
    class function  BinarySearch(constref A: array of T; constref aValue: T; c: TNestCompare): SizeInt; static;
  { returns 0-based position of aValue in SORTED array A in Result.FoundIndex(-1 if not found);
    returns position for insertion in Result.InsertIndex(-1 if array is empty);
    in case of array of single element or constant array assumes ascending state }
    class function  BinarySearchPos(constref A: array of T; constref aValue: T; c: TNestCompare): TSearchResult;
                    static;
  { returns 0-based position of minimal value in A, -1 if A is empty }
    class function  IndexOfMin(constref A: array of T; c: TNestCompare): SizeInt; static;
  { returns 0-based position of maximal value in A, -1 if A is empty }
    class function  IndexOfMax(constref A: array of T; c: TNestCompare): SizeInt; static;
  { returns smallest element of A in TOptional.Value if A is nonempty }
    class function  GetMin(constref A: array of T; c: TNestCompare): TOptional; static;
  { returns greatest element of A in TOptional.Value if A is nonempty }
    class function  GetMax(constref A: array of T; c: TNestCompare): TOptional; static;
  { returns True and smallest element of A in aValue if A is nonempty, False otherwise }
    class function  FindMin(constref A: array of T; out aValue: T; c: TNestCompare): Boolean; static;
  { returns True and  greatest element of A in aValue if A is nonempty, False otherwise }
    class function  FindMax(constref A: array of T; out aValue: T; c: TNestCompare): Boolean; static;
  { returns True, smallest element of A in aMin and greatest element of A in aMax,
    if A is nonempty, False otherwise }
    class function  FindMinMax(constref A: array of T; out aMin, aMax: T; c: TNestCompare): Boolean; static;
  { returns True and A's Nth order statistic(0-based) in aValue if A is nonempty, False otherwise;
    if N < 0 then N sets to 0; if N > High(A) then N sets to High(A);
    is destuctive: changes order of elements in A }
    class function  FindNthSmallest(var A: array of T; N: SizeInt; out aValue: T; c: TNestCompare): Boolean; static;
  { returns A's Nth order statistic(0-based) in TOptional.Value if A is nonempty;
    if N < 0 then N sets to 0; if N > High(A) then N sets to High(A);
    is destuctive: changes order of elements in A }
    class function  NthSmallest(var A: array of T; N: SizeInt; c: TNestCompare): TOptional; static;
  { returns True and A's Nth order statistic(0-based) in aValue if A is nonempty, False otherwise;
    if N < 0 then N sets to 0; if N > High(A) then N sets to High(A);
    is nondestuctive: creates temp copy of A }
    class function  FindNthSmallestND(constref A: array of T;N: SizeInt; out aValue: T; c: TNestCompare): Boolean; static;
  { returns A's Nth order statistic(0-based) in TOptional.Value if A is nonempty;
    if N < 0 then N sets to 0; if N > High(A) then N sets to High(A);
    is nondestuctive: creates temp copy of A }
    class function  NthSmallestND(constref A: array of T; N: SizeInt; c: TNestCompare): TOptional; static;
  { returns True if permutation towards nondescending state of A has done, False otherwise }
    class function  NextPermutation2Asc(var A: array of T; c: TNestCompare): Boolean; static;
  { returns True if permutation towards nonascending state of A has done, False otherwise }
    class function  NextPermutation2Desc(var A: array of T; c: TNestCompare): Boolean; static;
  { note: an empty array or single element array is always nondescending }
    class function  IsNonDescending(constref A: array of T; c: TNestCompare): Boolean; static;
  { note: an empty array or single element array is never strict ascending }
    class function  IsStrictAscending(constref A: array of T; c: TNestCompare): Boolean; static;
  { note: an empty array or single element array is always nonascending }
    class function  IsNonAscending(constref A: array of T; c: TNestCompare): Boolean; static;
  { note: an empty array or single element array is never strict descending}
    class function  IsStrictDescending(constref A: array of T; c: TNestCompare): Boolean; static;
  { returns True if both A and B are identical sequence of elements }
    class function  Same(constref A, B: array of T; c: TNestCompare): Boolean; static;
  { slightly modified optimized quicksort with random pivot selection }
    class procedure QuickSort(var A: array of T; c: TNestCompare; o: TSortOrder = soAsc); static;
  { slightly modified Introsort with pseudo-median-of-9 pivot selection }
    class procedure IntroSort(var A: array of T; c: TNestCompare; o: TSortOrder = soAsc); static;
  { slightly modified V.Yaroslavskiy proposed the dual pivot Quicksort algorithm with random pivot selection }
    class procedure DualPivotQuickSort(var A: array of T; c: TNestCompare; o: TSortOrder = soAsc); static;
  { stable, adaptive mergesort inspired by Java Timsort }
    class procedure MergeSort(var A: array of T; c: TNestCompare; o: TSortOrder = soAsc); static;
  { default sort algorithm, currently it is IntroSort }
    class procedure Sort(var A: array of T; c: TNestCompare; o: TSortOrder = soAsc); static;
    class function  Sorted(constref A: array of T; c: TNestCompare; o: TSortOrder = soAsc): TArray; static;
  { copies only distinct values from A }
    class function  SelectDistinct(constref A: array of T; c: TNestCompare): TArray;
                    static;
  end;

  { TGSimpleArrayHelper: for simple types only }
  generic TGSimpleArrayHelper<T> = class(specialize TGArrayHelpUtil<T>)
  private
    class function  CountRun2Asc(var A: array of T; L, R: SizeInt): SizeInt; static;
    class procedure InsertionSort(var A: array of T; L, R: SizeInt); static;
    class function  DoBinSearch(A: PItem; R: SizeInt; constref aValue: T): SizeInt; static;
    class function  DoBinSearchPosA(A: PItem; R: SizeInt; constref aValue: T): TSearchResult; static;
    class function  DoBinSearchPosD(A: PItem; R: SizeInt; constref aValue: T): TSearchResult; static;
    class function  DoBinSearchPos(A: PItem; R: SizeInt; constref aValue: T): TSearchResult; static;
    class procedure DoHeapSort(A: PItem; R: SizeInt); static;
    class function  QSplitR(var A: array of T; L, R: SizeInt): TSortSplit; static;
    class procedure DoQSort(var A: array of T; L, R: SizeInt); static;
    class function  MedianOf3(const v1, v2, v3: T): T; static; inline;
    class function  GetMo9Pivot(constref A: array of T; L, R: SizeInt): T; static;
    class function  QSplitMo9(var A: array of T; L, R: SizeInt): TSortSplit; static;
    class procedure DoIntroSort(var A: array of T; L, R, Ttl: SizeInt); static;
    class function  DPQSplit(var A: array of T; L, R: SizeInt): TSortSplit; static;
    class procedure DoDPQSort(var A: array of T; L, R: SizeInt); static;
    class procedure DoSwap(p: PItem; L, R: SizeInt); static; inline;
    class procedure DoReverse(var A: array of T; L, R: SizeInt); static;
  { QuickSelect with random pivot, does not checks indexes }
    class function  QSelectR(var A: array of T; N: SizeInt): T; static;
  public
    class procedure Reverse(var A: array of T); static;
  { cyclic shift of array elements by aDist positions to the left;
    the case if Abs(aDist) > Length(A) is ignored }
    class procedure RotateLeft(var A: array of T; aDist: SizeInt); static;
  { cyclic shift of array elements by aDist positions to the right;
    the case if Abs(aDist) > Length(A) is ignored }
    class procedure RotateRight(var A: array of T; aDist: SizeInt); static;
  { returns 0-based position of aValue in array A, -1 if not found }
    class function  SequentSearch(constref A: array of T; constref aValue: T): SizeInt; static;
  { returns 0-based position of aValue in SORTED array A, -1 if not found }
    class function  BinarySearch(constref A: array of T; constref aValue: T): SizeInt; static;
  { returns 0-based position of aValue in SORTED array A in Result.FoundIndex(-1 if not found);
    returns position for insertion in Result.InsertIndex(-1 if array is empty);
    in case of array of single element or constant array assumes ascending state }
    class function  BinarySearchPos(constref A: array of T; constref aValue: T): TSearchResult; static;
  { returns 0-based position of minimal value in A, -1 if A is empty }
    class function  IndexOfMin(constref A: array of T): SizeInt; static;
  { returns 0-based position of maximal value in A, -1 if A is empty }
    class function  IndexOfMax(constref A: array of T): SizeInt; static;
  { returns smallest element of A in TOptional.Value if A is nonempty }
    class function  GetMin(constref A: array of T): TOptional; static;
  { returns greatest element of A in TOptional.Value if A is nonempty }
    class function  GetMax(constref A: array of T): TOptional; static;
  { returns True and smallest element of A in aValue if A is nonempty, False otherwise }
    class function  FindMin(constref A: array of T; out aValue: T): Boolean; static;
  { returns True and  greatest element of A in aValue if A is nonempty, False otherwise }
    class function  FindMax(constref A: array of T; out aValue: T): Boolean; static;
  { returns True, smallest element of A in aMin and greatest element of A in aMax,
    if A is nonempty, False otherwise }
    class function  FindMinMax(constref A: array of T; out aMin, aMax: T): Boolean; static;
  { returns True and A's Nth order statistic(0-based) in aValue if A is nonempty, False otherwise;
    if N < 0 then N sets to 0; if N > High(A) then N sets to High(A);
    destuctive: changes order of elements in A }
    class function  FindNthSmallest(var  A: array of T; N: SizeInt; out aValue: T): Boolean; static;
  { returns A's Nth order statistic(0-based) in TOptional.Value if A is nonempty;
    if N < 0 then N sets to 0; if N > High(A) then N sets to High(A);
    is destuctive: changes order of elements in A }
    class function  NthSmallest(var  A: array of T; N: SizeInt): TOptional; static;
  { returns True and A's Nth order statistic(0-based) in aValue if A is nonempty, False otherwise;
    if N < 0 then N sets to 0; if N > High(A) then N sets to High(A);
    is nondestuctive: creates temp copy of A }
    class function  FindNthSmallestND(constref  A: array of T; N: SizeInt; out aValue: T): Boolean; static;
  { returns A's Nth order statistic(0-based) in TOptional.Value if A is nonempty;
    if N < 0 then N sets to 0; if N > High(A) then N sets to High(A);
    is nondestuctive: creates temp copy of A }
    class function  NthSmallestND(constref  A: array of T; N: SizeInt): TOptional; static;
  { returns True if permutation towards nondescending state of A has done, False otherwise }
    class function  NextPermutation2Asc(var A: array of T): Boolean; static;
  { returns True if permutation towards nonascending state of A has done, False otherwise }
    class function  NextPermutation2Desc(var A: array of T): Boolean; static;
  { note: an empty array or single element array is always nondescending }
    class function  IsNonDescending(constref A: array of T): Boolean; static;
  { note: an empty array or single element array is never strict ascending }
    class function  IsStrictAscending(constref A: array of T): Boolean; static;
  { note: an empty array or single element array is always nonascending }
    class function  IsNonAscending(constref A: array of T): Boolean; static;
  { note: an empty array or single element array is never strict descending}
    class function  IsStrictDescending(constref A: array of T): Boolean; static;
  { returns True if both A and B are identical sequence of elements }
    class function  Same(constref A, B: array of T): Boolean; static;
  { slightly modified optimized quicksort with random pivot selection }
    class procedure QuickSort(var A: array of T; o: TSortOrder = soAsc); static;
  { slightly modified Introsort with pseudo-median-of-9 pivot selection }
    class procedure IntroSort(var A: array of T; o: TSortOrder = soAsc); static;
  { slightly modified V.Yaroslavskiy proposed the dual pivot Quicksort algorithm with random pivot selection }
    class procedure DualPivotQuickSort(var A: array of T; o: TSortOrder = soAsc); static;
  { default sort algorithm, currently it is IntroSort }
    class procedure Sort(var A: array of T; o: TSortOrder = soAsc); static;
    class function  Sorted(constref A: array of T; o: TSortOrder = soAsc): TArray; static;
  { copies only distinct values from A }
    class function  SelectDistinct(constref A: array of T): TArray; static;
  end;

  { TGNumArrayHelper: for numeric types only }
  generic TGNumArrayHelper<T> = class(specialize TGSimpleArrayHelper<T>)
  private
  type
    TRange = record
    strict private
      FCurrent,
      FLast,
      FStep: T;
      FInLoop: Boolean;
    public
      constructor Create(aFirst, aLast, aStep: T);
      function GetEnumerator: TRange; inline;
      function MoveNext: Boolean; inline;
      property Current: T read FCurrent;
    end;

    TDownRange = record
    strict private
      FCurrent,
      FLast,
      FStep: T;
      FInLoop: Boolean;
    public
      constructor Create(aFirst, aLast, aStep: T);
      function GetEnumerator: TDownRange; inline;
      function MoveNext: Boolean; inline;
      property Current: T read FCurrent;
    end;
  public
  { loop from aFrom to aTo with step aStep;
    if aStep > T(0) then iteration count = Max(0, Int((aTo - aFrom + aStep)/aStep)),
    otherwise 0 }
    class function Range(aFrom, aTo: T; aStep: T = T(1)): TRange; static; inline;
  { loop from aFrom down to aDownTo with step aStep;
    if aStep > T(0) then iteration count = Max(0, Int((aFrom - aDownTo + aStep)/aStep)),
    otherwise 0 }
    class function DownRange(aFrom, aDownTo: T; aStep: T = T(1)): TDownRange; static; inline;
  end;

  { TGOrdinalArrayHelper: for ordinal numeric types only }
  generic TGOrdinalArrayHelper<T> = class(specialize TGNumArrayHelper<T>)
  private
  type
    TMonotonicity = (moAsc, moDesc, moConst, moNone);

  const
  {$IFDEF CPU16}
    COUNTSORT_CUTOFF = $7fff;
  {$ELSE CPU16}
    COUNTSORT_CUTOFF = $400000; //todo: ???
  {$ENDIF CPU16}
    class procedure CountSort(var A: array of T; aMinValue, aMaxValue: T); static;
    class function  Scan(var A: array of T; out aMinValue, aMaxValue: T): TMonotonicity; static;
  public
    class function CreateRange(aFrom, aTo: T): TArray; static;
  { will use counting sort if possible }
    class procedure Sort(var A: array of T; aOrder: TSortOrder = soAsc); static;
    class function  Sorted(constref A: array of T; o: TSortOrder = soAsc): TArray; static;
  end;

implementation
{$B-}{$COPERATORS ON}{$POINTERMATH ON}{$GOTO ON}

{ TGArrayHelpUtil.TMergeSortBase }

procedure TGArrayHelpUtil.TMergeSortBase.PushRun(aBase, aCount: SizeInt);
var
  I: SizeInt;
begin
  I := FStackSize;
  Inc(FStackSize);
  if System.Length(FStack) = I then
    System.SetLength(FStack, I * 2);
  FStack[I].Base := aBase;
  FStack[I].Count := aCount;
end;

function TGArrayHelpUtil.TMergeSortBase.EnsureBufferCapacity(aSize: SizeInt): PItem;
begin
  if aSize > System.Length(FBuffer) then
    System.SetLength(FBuffer, LGUtils.RoundUpTwoPower(aSize));
  Result := Pointer(FBuffer);
end;

procedure TGArrayHelpUtil.TMergeSortBase.Init(A: PItem);
begin
  FData := A;
  FStackSize := 0;
  if System.Length(FBuffer) < MERGE_BUFFER_INIT_SIZE then
    System.SetLength(FBuffer, MERGE_BUFFER_INIT_SIZE);
  if System.Length(FStack) < MERGE_STACK_INIT_SIZE then
    System.SetLength(FStack, MERGE_STACK_INIT_SIZE);
end;

class function TGArrayHelpUtil.TMergeSortBase.MinRunLen(aTotalSize: SizeInt): SizeInt;
var
  s: SizeInt;
begin
  if aTotalSize > MIN_MERGE_LEN then
    begin
      s := LGUtils.NSB(aTotalSize) - MIN_MERGE_POW;
      Result := aTotalSize shr s;
      Result += Ord((Result shl s xor aTotalSize) <> 0);
    end
  else
    Result := aTotalSize;
end;

{ TGArrayHelpUtil }

class procedure TGArrayHelpUtil.CopyItems(aSrc, aDst: PItem; aCount: SizeInt);
begin
  if (aDst <> aSrc) and (aCount > 0) then  //else nothing to do
    if aDst > aSrc then // forward copy
      begin
        aSrc += aCount - 1;
        aDst += aCount - 1;
        while aCount >= 4 do
          begin
            aDst[ 0] := aSrc[ 0];
            aDst[-1] := aSrc[-1];
            aDst[-2] := aSrc[-2];
            aDst[-3] := aSrc[-3];
            aSrc -= 4;
            aDst -= 4;
            aCount -= 4;
          end;
        case aCount of
          1: aDst[0] := aSrc[0];
          2:
            begin
              aDst[ 0] := aSrc[ 0];
              aDst[-1] := aSrc[-1];
            end;
          3:
            begin
              aDst[ 0] := aSrc[ 0];
              aDst[-1] := aSrc[-1];
              aDst[-2] := aSrc[-2];
            end;
        end;
      end
    else              // backward copy
      begin
        while aCount >= 4 do
          begin
            aDst[0] := aSrc[0];
            aDst[1] := aSrc[1];
            aDst[2] := aSrc[2];
            aDst[3] := aSrc[3];
            aSrc += 4;
            aDst += 4;
            aCount -= 4;
          end;
        case aCount of
          1: aDst[0] := aSrc[0];
          2:
            begin
              aDst[0] := aSrc[0];
              aDst[1] := aSrc[1];
            end;
          3:
            begin
              aDst[0] := aSrc[0];
              aDst[1] := aSrc[1];
              aDst[2] := aSrc[2];
            end;
        end;
      end;
end;

class procedure TGArrayHelpUtil.DoReverse(p: PItem; R: SizeInt);
var
  v0, v1, v2, v3: TFake;
  L: SizeInt;
begin
  L := 0;
  while R - L >= 7 do
    begin
      v0 := TFake(p[L    ]);
      v1 := TFake(p[L + 1]);
      v2 := TFake(p[L + 2]);
      v3 := TFake(p[L + 3]);
      TFake(p[L    ]) := TFake(p[R    ]);
      TFake(p[L + 1]) := TFake(p[R - 1]);
      TFake(p[L + 2]) := TFake(p[R - 2]);
      TFake(p[L + 3]) := TFake(p[R - 3]);
      TFake(p[R    ]) := v0;
      TFake(p[R - 1]) := v1;
      TFake(p[R - 2]) := v2;
      TFake(p[R - 3]) := v3;
      L += 4;
      R -= 4;
    end;
  case R - L of
    1..2:
      begin
        v0 := TFake(p[L]);
        TFake(p[L]) := TFake(p[R]);
        TFake(p[R]) := v0;
      end;
    3..4:
      begin
        v0 := TFake(p[L    ]);
        v1 := TFake(p[L + 1]);
        TFake(p[L    ]) := TFake(p[R    ]);
        TFake(p[L + 1]) := TFake(p[R - 1]);
        TFake(p[R    ]) := v0;
        TFake(p[R - 1]) := v1;
      end;
    5..6:
      begin
        v0 := TFake(p[L    ]);
        v1 := TFake(p[L + 1]);
        v2 := TFake(p[L + 2]);
        TFake(p[L    ]) := TFake(p[R    ]);
        TFake(p[L + 1]) := TFake(p[R - 1]);
        TFake(p[L + 2]) := TFake(p[R - 2]);
        TFake(p[R    ]) := v0;
        TFake(p[R - 1]) := v1;
        TFake(p[R - 2]) := v2;
      end;
  end;
end;

//class procedure TGArrayHelpUtil.DoSwap(p: PItem; L, R: SizeInt);
//var
//  v: TFake;
//begin
//  v := TFake(p[L]);
//  TFake(p[L]) := TFake(p[R]);
//  TFake(p[R]) := v;
//end;

class function TGArrayHelpUtil.CreateCopy(constref A: array of T): TArray;
begin
  System.SetLength(Result, System.Length(A));
  if System.Length(Result) > 0 then
    CopyItems(@A[0], PItem(Result), System.Length(Result));
end;

class function TGArrayHelpUtil.CreateReverseCopy(constref A: array of T): TArray;
var
  L, R: SizeInt;
begin
  R := System.High(A);
  System.SetLength(Result, Succ(R));
  if R >= 0 then
    begin
      L := 0;
      while R >= 3 do
        begin
          Result[R    ] := A[L    ];
          Result[R - 1] := A[L + 1];
          Result[R - 2] := A[L + 2];
          Result[R - 3] := A[L + 3];
          L += 4;
          R -= 4;
        end;
      case R of
        0: Result[R] := A[L];
        1:
          begin
            Result[R    ] := A[L    ];
            Result[R - 1] := A[L + 1];
          end;
        2:
          begin
            Result[R    ] := A[L    ];
            Result[R - 1] := A[L + 1];
            Result[R - 2] := A[L + 2];
          end;
      end;
    end;
end;

class function TGArrayHelpUtil.CreateMerge(constref L, R: array of T): TArray;
var
  LenL, LenR: SizeInt;
begin
  LenL := System.Length(L);
  LenR := System.Length(R);
  System.SetLength(Result, LenL + LenR);
  if LenL > 0 then
    CopyItems(@L[0], @Result[0], LenL);
  if LenR > 0 then
    CopyItems(@R[0], @Result[LenL], LenR);
end;

class function TGArrayHelpUtil.CreateRandomShuffle(constref A: array of T): TArray;
begin
  Result := CreateCopy(A);
  RandomShuffle(Result);
end;

class procedure TGArrayHelpUtil.Fill(var A: array of T; constref aValue: T);
var
  I: SizeInt;
begin
  for I := 0 to System.High(A) do
    A[I] := aValue;
end;

class function TGArrayHelpUtil.Append(var A: TArray; constref aValue: T): SizeInt;
begin
  Result := System.Length(A);
  System.SetLength(A, Succ(Result));
  A[Result] := aValue;
end;

class function TGArrayHelpUtil.Merge(var aDst, aSrc: TArray): SizeInt;
var
  DstLen: SizeInt;
begin
  if aSrc <> aDst then
    begin
      Result := System.Length(aSrc);
      if Result > 0 then
        begin
          if aDst <> aSrc then
            begin
              DstLen := System.Length(aDst);
              System.SetLength(aDst, DstLen + Result);
              System.Move(aSrc[0], aDst[DstLen], SizeOf(T) * Result);
              System.FillChar(aSrc[0], SizeOf(T) * Result, 0);
              System.SetLength(aSrc, 0);
            end
          else
            begin
              System.SetLength(aDst, Result shl 1);
              CopyItems(@aSrc[0], @aDst[Result], Result);
            end;
        end;
    end
  else
    Result := 0;
end;

class function TGArrayHelpUtil.Split(var A: TArray; aIndex: SizeInt): TArray;
var
  ALen, RLen: SizeInt;
begin
  if A = nil then
    exit(nil);
  if aIndex < 0 then
    aIndex := 0;
  ALen := System.Length(A);
  if aIndex < ALen then
    begin
      RLen := ALen - aIndex;
      System.SetLength(Result, RLen);
      System.Move(A[aIndex], Result[0], SizeOf(T) * RLen);
      System.FillChar(A[aIndex], SizeOf(T) * RLen, 0);
      System.SetLength(A, aIndex);
    end
  else
    Result := nil;
end;

class function TGArrayHelpUtil.Extract(var A: TArray; aIndex, aCount: SizeInt): TArray;
var
  Len: SizeInt;
begin
  if (A = nil) or (aCount < 1) then
    exit(nil);
  if aIndex < 0 then
    aIndex := 0;
  Len := System.Length(A);
  if aIndex < Len then
    begin
      aCount := Math.Min(aCount, Len - aIndex);
      System.SetLength(Result, aCount);
      if aCount > 0 then
        begin
          System.Move(A[aIndex], Result[0], SizeOf(T) * aCount);
          Len -= aCount;
          System.Move(A[aIndex + aCount], A[aIndex], SizeOf(T) * (Len - aIndex));
          System.FillChar(A[Len], SizeOf(T) * aCount, 0);
          System.SetLength(A, Len);
        end;
    end
  else
    Result := nil;
end;

class procedure TGArrayHelpUtil.Reverse(var A: array of T);
begin
  if System.High(A) > 0 then
    DoReverse(@A[0], System.High(A));
end;

class procedure TGArrayHelpUtil.RotateLeft(var A: array of T; aDist: SizeInt);
var
  Len: SizeInt;
begin
  if (aDist = 0) or (Abs(aDist) >= System.Length(A)) then
    exit;
  Len := System.Length(A);
  if aDist < 0 then
    aDist += Len;
  DoReverse(@A[0], Pred(aDist));
  DoReverse(@A[aDist], Pred(Len - aDist));
  DoReverse(@A[0], Pred(Len));
end;

class procedure TGArrayHelpUtil.RotateRight(var A: array of T; aDist: SizeInt);
begin
  if (aDist = 0) or (Abs(aDist) >= System.Length(A)) then
    exit;
  if aDist > 0 then
    RotateLeft(A, System.Length(A) - aDist)
  else
    RotateLeft(A, -aDist);
end;


class procedure TGArrayHelpUtil.RandomShuffle(var A: array of T);
var
  I, J: SizeInt;
  v: TFake;
begin
  for I := System.High(A) downto 1 do
    begin
      J := Random(I);
      v := TFake(A[I]);
      TFake(A[I]) := TFake(A[J]);
      TFake(A[J]) := v;
    end;
end;

class function TGArrayHelpUtil.SequentSearch(constref A: array of T; constref aValue: T;
  c: TEqualCompare): SizeInt;
begin
  for Result := 0 to System.High(A) do
    if c(aValue, A[Result]) then
      exit;
  Result := -1;
end;

class function TGArrayHelpUtil.SequentSearch(constref A: array of T; constref aValue: T;
  c: TOnEqualCompare): SizeInt;
begin
  for Result := 0 to System.High(A) do
    if c(aValue, A[Result]) then
      exit;
  Result := -1;
end;

class function TGArrayHelpUtil.SequentSearch(constref A: array of T; constref aValue: T;
  c: TNestEqualCompare): SizeInt;
begin
  for Result := 0 to System.High(A) do
    if c(aValue, A[Result]) then
      exit;
  Result := -1;
end;

class function TGArrayHelpUtil.Same(constref A, B: array of T; c: TEqualCompare): Boolean;
var
  R, I: SizeInt;
begin
  R := System.High(A);
  if System.High(B) <> R then
    exit(False);
  for I := 0 to R do
    if not c(A[I], B[I]) then
      exit(False);
  Result := True;
end;

class function TGArrayHelpUtil.Same(constref A, B: array of T; c: TOnEqualCompare): Boolean;
var
  R, I: SizeInt;
begin
  R := System.High(A);
  if System.High(B) <> R then
    exit(False);
  for I := 0 to R do
    if not c(A[I], B[I]) then
      exit(False);
  Result := True;
end;

class function TGArrayHelpUtil.Same(constref A, B: array of T; c: TNestEqualCompare): Boolean;
var
  R, I: SizeInt;
begin
  R := System.High(A);
  if System.High(B) <> R then
    exit(False);
  for I := 0 to R do
    if not c(A[I], B[I]) then
      exit(False);
  Result := True;
end;

class function TGArrayHelpUtil.Select(constref A: array of T; aTest: TTest): TArray;
var
  I, Len: SizeInt;
  v: T;
begin
  Len := ARRAY_INITIAL_SIZE;
  SetLength(Result, ARRAY_INITIAL_SIZE);
  I := 0;
  for v in A do
    if aTest(v) then
      begin
        if I = Len then
          begin
            Len += Len;
            SetLength(Result, Len);
          end;
        Result[I] := v;
        Inc(I);
      end;
  SetLength(Result, I);
end;

class function TGArrayHelpUtil.Select(constref A: array of T; aTest: TOnTest): TArray;
var
  I, Len: SizeInt;
  v: T;
begin
  Len := ARRAY_INITIAL_SIZE;
  SetLength(Result, ARRAY_INITIAL_SIZE);
  I := 0;
  for v in A do
    if aTest(v) then
      begin
        if I = Len then
          begin
            Len += Len;
            SetLength(Result, Len);
          end;
        Result[I] := v;
        Inc(I);
      end;
  SetLength(Result, I);
end;

class function TGArrayHelpUtil.Select(constref A: array of T; aTest: TNestTest): TArray;
var
  I, Len: SizeInt;
  v: T;
begin
  Len := ARRAY_INITIAL_SIZE;
  SetLength(Result, ARRAY_INITIAL_SIZE);
  I := 0;
  for v in A do
    if aTest(v) then
      begin
        if I = Len then
          begin
            Len += Len;
            SetLength(Result, Len);
          end;
        Result[I] := v;
        Inc(I);
      end;
  SetLength(Result, I);
end;

class function TGArrayHelpUtil.FoldL(constref A: array of T; aFold: TFold; constref v0: T): T;
var
  v: T;
begin
  Result := v0;
  for v in A do
    Result := aFold(v, Result);
end;

class function TGArrayHelpUtil.FoldL(constref A: array of T; aFold: TFold): TOptional;
var
  I, Last: SizeInt;
  v: T;
begin
  Last := System.High(A);
  if Last >= 0 then
    begin
      v := A[0];
      for I := 1 to Last do
        v := aFold(A[I], v);
      Result.Assign(v);
    end;
end;

class function TGArrayHelpUtil.FoldL(constref A: array of T; aFold: TOnFold; constref v0: T): T;
var
  v: T;
begin
  Result := v0;
  for v in A do
    Result := aFold(v, Result);
end;

class function TGArrayHelpUtil.FoldL(constref A: array of T; aFold: TOnFold): TOptional;
var
  I, Last: SizeInt;
  v: T;
begin
  Last := System.High(A);
  if Last >= 0 then
    begin
      v := A[0];
      for I := 1 to Last do
        v := aFold(A[I], v);
      Result.Assign(v);
    end;
end;

class function TGArrayHelpUtil.FoldL(constref A: array of T; aFold: TNestFold; constref v0: T): T;
var
  v: T;
begin
  Result := v0;
  for v in A do
    Result := aFold(v, Result);
end;

class function TGArrayHelpUtil.FoldL(constref A: array of T; aFold: TNestFold): TOptional;
var
  I, Last: SizeInt;
  v: T;
begin
  Last := System.High(A);
  if Last >= 0 then
    begin
      v := A[0];
      for I := 1 to Last do
        v := aFold(A[I], v);
      Result.Assign(v);
    end;
end;

class function TGArrayHelpUtil.FoldR(constref A: array of T; aFold: TFold; constref v0: T): T;
var
  I: SizeInt;
begin
  Result := v0;
  for I := System.High(A) downto 0 do
    Result := aFold(A[I], Result);
end;

class function TGArrayHelpUtil.FoldR(constref A: array of T; aFold: TFold): TOptional;
var
  I, Last: SizeInt;
  v: T;
begin
  Last := System.High(A);
  if Last >= 0 then
    begin
      v := A[Last];
      for I := Pred(Last) downto 0 do
        v := aFold(A[I], v);
      Result.Assign(v);
    end;
end;

class function TGArrayHelpUtil.FoldR(constref A: array of T; aFold: TOnFold; constref v0: T): T;
var
  I: SizeInt;
begin
  Result := v0;
  for I := System.High(A) downto 0 do
    Result := aFold(A[I], Result);
end;

class function TGArrayHelpUtil.FoldR(constref A: array of T; aFold: TOnFold): TOptional;
var
  I, Last: SizeInt;
  v: T;
begin
  Last := System.High(A);
  if Last >= 0 then
    begin
      v := A[Last];
      for I := Pred(Last) downto 0 do
        v := aFold(A[I], v);
      Result.Assign(v);
    end;
end;

class function TGArrayHelpUtil.FoldR(constref A: array of T; aFold: TNestFold; constref v0: T): T;
var
  I: SizeInt;
begin
  Result := v0;
  for I := System.High(A) downto 0 do
    Result := aFold(A[I], Result);
end;

class function TGArrayHelpUtil.FoldR(constref A: array of T; aFold: TNestFold): TOptional;
var
  I, Last: SizeInt;
  v: T;
begin
  Last := System.High(A);
  if Last >= 0 then
    begin
      v := A[Last];
      for I := Pred(Last) downto 0 do
        v := aFold(A[I], v);
      Result.Assign(v);
    end;
end;

{ TGBaseArrayHelper.TMergeSort }

procedure TGBaseArrayHelper.TMergeSort.CollapseA;
var
  I: SizeInt;
begin
  while FStackSize > 1 do
    begin
      I := FStackSize - 2;
      if (I > 0) and (FStack[I - 1].Count <= FStack[I].Count + FStack[I + 1].Count) then
        begin
          if FStack[I - 1].Count < FStack[I + 1].Count then
            Dec(I);
        end
      else
        if FStack[I].Count > FStack[I + 1].Count then
          break;
      MergeAtA(I);
    end;
end;

procedure TGBaseArrayHelper.TMergeSort.CollapseD;
var
  I: SizeInt;
begin
  while FStackSize > 1 do
    begin
      I := FStackSize - 2;
      if (I > 0) and (FStack[I - 1].Count <= FStack[I].Count + FStack[I + 1].Count) then
        begin
          if FStack[I - 1].Count < FStack[I + 1].Count then
            Dec(I);
        end
      else
        if FStack[I].Count > FStack[I + 1].Count then
          break;
      MergeAtD(I);
    end;
end;

procedure TGBaseArrayHelper.TMergeSort.ForceCollapseA;
var
  I: SizeInt;
begin
  while FStackSize > 1 do
    begin
      I := FStackSize - 2;
      if (I > 0) and (FStack[I - 1].Count < FStack[I + 1].Count) then
        MergeAtA(I - 1)
      else
        MergeAtA(I);
    end;
end;

procedure TGBaseArrayHelper.TMergeSort.ForceCollapseD;
var
  I: SizeInt;
begin
  while FStackSize > 1 do
    begin
      I := FStackSize - 2;
      if (I > 0) and (FStack[I - 1].Count < FStack[I + 1].Count) then
        MergeAtD(I - 1)
      else
        MergeAtD(I);
    end;
end;

procedure TGBaseArrayHelper.TMergeSort.MergeAtA(aIndex: SizeInt);
var
  Base, CountLo, CountHi: SizeInt;
begin
  Base := FStack[aIndex].Base;
  CountLo := FStack[aIndex].Count;
  CountHi := FStack[aIndex + 1].Count;
  FStack[aIndex].Count := CountLo + CountHi;
  if aIndex = FStackSize - 3 then
    begin
      FStack[aIndex + 1].Base := FStack[aIndex + 2].Base;
      FStack[aIndex + 1].Count := FStack[aIndex + 2].Count;
    end;
  Dec(FStackSize);
  if CountLo <= CountHi then
    MergeLoA(Base, CountLo, CountHi)
  else
    MergeHiA(Base, CountLo, CountHi);
end;

procedure TGBaseArrayHelper.TMergeSort.MergeAtD(aIndex: SizeInt);
var
  Base, CountLo, CountHi: SizeInt;
begin
  Base := FStack[aIndex].Base;
  CountLo := FStack[aIndex].Count;
  CountHi := FStack[aIndex + 1].Count;
  FStack[aIndex].Count := CountLo + CountHi;
  if aIndex = FStackSize - 3 then
    begin
      FStack[aIndex + 1].Base := FStack[aIndex + 2].Base;
      FStack[aIndex + 1].Count := FStack[aIndex + 2].Count;
    end;
  Dec(FStackSize);
  if CountLo <= CountHi then
    MergeLoD(Base, CountLo, CountHi)
  else
    MergeHiD(Base, CountLo, CountHi);
end;

procedure TGBaseArrayHelper.TMergeSort.MergeLoA(From, CountLo, CountHi: SizeInt);
var
  pLo,           // position in low part   (in buffer)
  pHi,           // position in high part  (in data array)
  pDst: SizeInt; // current CreateMerge position (in data array)
  LocA: PItem;   // local pointer to data array
  LocB: PItem;   // local pointer to buffer
begin
  LocA := FData;
  if TCmpRel.Compare(FData[Pred(From + CountLo)], FData[From + CountLo]) > 0 then
    begin
      LocB := EnsureBufferCapacity(CountLo);
    {$IFNDEF FPC_REQUIRES_PROPER_ALIGNMENT}
      System.Move(LocA[From], LocB[0], CountLo * SizeOf(T));
    {$ELSE}
      CopyItems(@LocA[From], LocB, CountLo);
    {$ENDIF}
      if TCmpRel.Compare(LocA[Pred(From + CountLo + CountHi)], LocA[From]) >= 0 then
        begin
          pLo := 0;
          pHi := From + CountLo;
          pDst := From;
          CountHi := Pred(From + CountLo + CountHi);
          repeat
            if TCmpRel.Compare(LocB[pLo], LocA[pHi]) <= 0 then
              begin
                TFake(LocA[pDst]) := TFake(LocB[pLo]);
                Inc(pLo);
              end
            else
              begin
                TFake(LocA[pDst]) := TFake(LocA[pHi]);
                Inc(pHi);
              end;
            Inc(pDst);
          until (pLo >= CountLo) or (pHi > CountHi);
          if pLo < CountLo then
          {$IFNDEF FPC_REQUIRES_PROPER_ALIGNMENT}
            System.Move(LocB[pLo], LocA[pDst], (CountLo - pLo) * SizeOf(T)); ///
          {$ELSE}
            CopyItems(@LocB[pLo], @LocA[pDst], CountLo - pLo);
          {$ENDIF}
        end
      else
        begin
        {$IFNDEF FPC_REQUIRES_PROPER_ALIGNMENT}
          System.Move(LocA[From + CountLo], LocA[From], CountHi * SizeOf(T));///
          System.Move(LocB[0], LocA[From + CountHi], CountLo * SizeOf(T));   ///
        {$ELSE}
          CopyItems(@LocA[From + CountLo], @LocA[From], CountHi);
          CopyItems(LocB, @LocA[From + CountHi], CountLo);
        {$ENDIF}
        end;
    end;
end;

procedure TGBaseArrayHelper.TMergeSort.MergeLoD(From, CountLo, CountHi: SizeInt);
var
  pLo,           // position in low part   (in buffer)
  pHi,           // position in high part  (in data array)
  pDst: SizeInt; // current CreateMerge position (in data array)
  LocA: PItem;   // local pointer to data array
  LocB: PItem;   // local pointer to buffer
begin
  LocA := FData;
  if TCmpRel.Compare(FData[Pred(From + CountLo)], FData[From + CountLo]) < 0 then
    begin
      LocB := EnsureBufferCapacity(CountLo);
    {$IFNDEF FPC_REQUIRES_PROPER_ALIGNMENT}
      System.Move(LocA[From], LocB[0], CountLo * SizeOf(T)); ///
    {$ELSE}
      CopyItems(@LocA[From], LocB, CountLo);
    {$ENDIF}
      if TCmpRel.Compare(LocA[Pred(From + CountLo + CountHi)], LocA[From]) <= 0 then
        begin
          pLo := 0;
          pHi := From + CountLo;
          pDst := From;
          CountHi := Pred(From + CountLo + CountHi);
          repeat
            if TCmpRel.Compare(LocB[pLo], LocA[pHi]) >= 0 then
              begin
                TFake(LocA[pDst]) := TFake(LocB[pLo]);
                Inc(pLo);
              end
            else
              begin
                TFake(LocA[pDst]) := TFake(LocA[pHi]);
                Inc(pHi);
              end;
            Inc(pDst);
          until (pLo >= CountLo) or (pHi > CountHi);
          if pLo < CountLo then
          {$IFNDEF FPC_REQUIRES_PROPER_ALIGNMENT}
            System.Move(LocB[pLo], LocA[pDst], (CountLo - pLo) * SizeOf(T));///
          {$ELSE}
            CopyItems(@LocB[pLo], @LocA[pDst], CountLo - pLo);
          {$ENDIF}
        end
      else
        begin
        {$IFNDEF FPC_REQUIRES_PROPER_ALIGNMENT}
          System.Move(LocA[From + CountLo], LocA[From], CountHi * SizeOf(T));///
          System.Move(LocB[0], LocA[From + CountHi], CountLo * SizeOf(T));   ///
        {$ELSE}
          CopyItems(@LocA[From + CountLo], @LocA[From], CountHi);
          CopyItems(LocB, @LocA[From + CountHi], CountLo);
        {$ENDIF}
        end;
    end;
end;

procedure TGBaseArrayHelper.TMergeSort.MergeHiA(From, CountLo, CountHi: SizeInt);
var
  pLo,           // position in low part   (in data array)
  pHi,           // position in high part  (in buffer)
  pDst: SizeInt; // current CreateMerge position (in data array)
  LocA: PItem;   // local pointer to data array
  LocB: PItem;   // local pointer to buffer
begin
  LocA := FData;
  if TCmpRel.Compare(FData[Pred(From + CountLo)], FData[From + CountLo]) > 0 then
    begin
      LocB := EnsureBufferCapacity(CountHi);
    {$IFNDEF FPC_REQUIRES_PROPER_ALIGNMENT}
      System.Move(LocA[From + CountLo], LocB[0], CountHi * SizeOf(T));///
    {$ELSE}
      CopyItems(@LocA[From + CountLo], LocB, CountHi);
    {$ENDIF}
      if TCmpRel.Compare(LocA[Pred(From + CountLo + CountHi)], LocA[From]) >= 0 then
        begin
          pLo := Pred(From + CountLo);
          pHi := CountHi - 1;
          pDst := Pred(From + CountLo + CountHi);
          repeat
            if TCmpRel.Compare(LocA[pLo], LocB[pHi]) > 0 then
              begin
                TFake(LocA[pDst]) := TFake(LocA[pLo]);
                Dec(pLo);
              end
            else
              begin
                TFake(LocA[pDst]) := TFake(LocB[pHi]);
                Dec(pHi);
              end;
            Dec(pDst);
          until (pLo < From) or (pHi < 0);
          if pHi >= 0 then
          {$IFNDEF FPC_REQUIRES_PROPER_ALIGNMENT}
            System.Move(LocB[0], LocA[From], Succ(pHi) * SizeOf(T)); ///
          {$ELSE}
            CopyItems(LocB, @LocA[From], Succ(pHi));
          {$ENDIF}
        end
      else
        begin
        {$IFNDEF FPC_REQUIRES_PROPER_ALIGNMENT}
          System.Move(LocA[From], LocA[From + CountHi], CountLo * SizeOf(T)); ///
          System.Move(LocB[0], LocA[From], CountHi * SizeOf(T));  ///
        {$ELSE}
          CopyItems(@LocA[From], @LocA[From + CountHi], CountLo);
          CopyItems(LocB, @LocA[From], CountHi);
        {$ENDIF}
        end;
    end;
end;

procedure TGBaseArrayHelper.TMergeSort.MergeHiD(From, CountLo, CountHi: SizeInt);
var
  pLo,           // position in low part   (in data array)
  pHi,           // position in high part  (in buffer)
  pDst: SizeInt; // current CreateMerge position (in data array)
  LocA: PItem;   // local pointer to data array
  LocB: PItem;   // local pointer to buffer
begin
  LocA := FData;
  if TCmpRel.Compare(FData[Pred(From + CountLo)], FData[From + CountLo]) < 0 then
    begin
      LocB := EnsureBufferCapacity(CountHi);
    {$IFNDEF FPC_REQUIRES_PROPER_ALIGNMENT}
      System.Move(LocA[From + CountLo], LocB[0], CountHi * SizeOf(T)); ///
    {$ELSE}
      CopyItems(@LocA[From + CountLo], LocB, CountHi);
    {$ENDIF}
      if TCmpRel.Compare(LocA[Pred(From + CountLo + CountHi)], LocA[From]) <= 0 then
        begin
          pLo := Pred(From + CountLo);
          pHi := CountHi - 1;
          pDst := Pred(From + CountLo + CountHi);
          repeat
            if TCmpRel.Compare(LocA[pLo], LocB[pHi]) < 0 then
              begin
                TFake(LocA[pDst]) := TFake(LocA[pLo]);
                Dec(pLo);
              end
            else
              begin
                TFake(LocA[pDst]) := TFake(LocB[pHi]);
                Dec(pHi);
              end;
            Dec(pDst);
          until (pLo < From) or (pHi < 0);
          if pHi >= 0 then
          {$IFNDEF FPC_REQUIRES_PROPER_ALIGNMENT}
            System.Move(LocB[0], LocA[From], Succ(pHi) * SizeOf(T)); ///
          {$ELSE}
            CopyItems(LocB, @LocA[From], Succ(pHi));
          {$ENDIF}
        end
      else
        begin
        {$IFNDEF FPC_REQUIRES_PROPER_ALIGNMENT}
          System.Move(LocA[From], LocA[From + CountHi], CountLo * SizeOf(T)); ///
          System.Move(LocB[0], LocA[From], CountHi * SizeOf(T));              ///
        {$ELSE}
          CopyItems(@LocA[From], @LocA[From + CountHi], CountLo);
          CopyItems(LocB, @LocA[From], CountHi);
        {$ENDIF}
        end;
    end;
end;

class procedure TGBaseArrayHelper.TMergeSort.InsertSortA(A: PItem; R, At: SizeInt);
var
  I, J: SizeInt;
  v: TFake;
begin
  if At < 1 then
    At := 1;
  for I := At to R do
    begin
      v := TFake(A[I]);
      J := I - 1;
      while (J >= 0) and (TCmpRel.Compare(A[J], T(v)) > 0) do
        begin
          TFake(A[J + 1]) := TFake(A[J]);
          Dec(J);
        end;
      TFake(A[J + 1]) := v;
    end;
end;

class procedure TGBaseArrayHelper.TMergeSort.InsertSortD(A: PItem; R, At: SizeInt);
var
  I, J: SizeInt;
  v: TFake;
begin
  if At < 1 then
    At := 1;
  for I := At to R do
    begin
      v := TFake(A[I]);
      J := I - 1;
      while (J >= 0) and (TCmpRel.Compare(A[J], T(v)) < 0) do
        begin
          TFake(A[J + 1]) := TFake(A[J]);
          Dec(J);
        end;
      TFake(A[J + 1]) := v;
    end;
end;

class function TGBaseArrayHelper.TMergeSort.CountRunAsc(A: PItem; R: SizeInt): SizeInt;
begin
  if R > 0 then
    begin
      Result := 1;
      if TCmpRel.Compare(A[0], A[1]) <= 0 then  // ascending
        while (Result < R) and (TCmpRel.Compare(A[Result], A[Succ(Result)]) <= 0) do
          Inc(Result)
      else                                      // descending
        begin
          while (Result < R) and (TCmpRel.Compare(A[Result], A[Succ(Result)]) > 0) do
            Inc(Result);
          DoReverse(A, Result);
        end;
    end
  else
    Result := 0;
end;

class function TGBaseArrayHelper.TMergeSort.CountRunDesc(A: PItem; R: SizeInt): SizeInt;
begin
  if R > 0 then
    begin
      Result := 1;
      if TCmpRel.Compare(A[0], A[1]) >= 0 then  // descending
        while (Result < R) and (TCmpRel.Compare(A[Result], A[Succ(Result)]) >= 0) do
          Inc(Result)
      else                                      // ascending
        begin
          while (Result < R) and (TCmpRel.Compare(A[Result], A[Succ(Result)]) < 0) do
            Inc(Result);
          DoReverse(A, Result);
        end;
    end
  else
    Result := 0;
end;

class procedure TGBaseArrayHelper.TMergeSort.SortAsc(A: PItem; R: SizeInt);
var
  RunLen, MinLen, Len, L: SizeInt;
  ms: TMergeSort;
begin
  if R + 1 > MIN_MERGE_LEN then
    begin
      ms.Init(A);
      L := 0;
      MinLen := MinRunLen(R - L + 1);
       repeat
         RunLen := Succ(CountRunAsc(@A[L], R - L));
         if RunLen < MinLen then
           begin
             Len := Math.Min(Succ(R - L), MinLen);
             InsertSortA(@A[L], Pred(Len), RunLen);
             RunLen := Len;
           end;
         ms.PushRun(L, RunLen);
         L += RunLen;
         ms.CollapseA;
       until L > R;
       ms.ForceCollapseA;
    end
  else
    InsertSortA(A, R, Succ(CountRunAsc(A, R)));
end;

class procedure TGBaseArrayHelper.TMergeSort.SortDesc(A: PItem; R: SizeInt);
var
  RunLen, MinLen, Len, L: SizeInt;
  ms: TMergeSort;
begin
  if R + 1 > MIN_MERGE_LEN then
    begin
      ms.Init(A);
      L := 0;
      MinLen := MinRunLen(R - L + 1);
       repeat
         RunLen := Succ(CountRunDesc(@A[L], R - L));
         if RunLen < MinLen then
           begin
             Len := Math.Min(Succ(R - L), MinLen);
             InsertSortD(@A[L], Pred(Len), RunLen);
             RunLen := Len;
           end;
         ms.PushRun(L, RunLen);
         L += RunLen;
         ms.CollapseD;
       until L > R;
       ms.ForceCollapseD;
    end
  else
    InsertSortD(A, R, Succ(CountRunDesc(A, R)));
end;


{ TGBaseArrayHelper }

class function TGBaseArrayHelper.CountRun2Asc(A: PItem; R: SizeInt): SizeInt;
begin
  Result := 0;
  while (Result < R) and (TCmpRel.Compare(A[Result], A[Succ(Result)]) = 0) do
    Inc(Result);
  if Result < R then
    begin
      Inc(Result);
      if TCmpRel.Compare(A[Pred(Result)], A[Result]) < 0 then   // ascending
        while (Result < R) and (TCmpRel.Compare(A[Result], A[Succ(Result)]) <= 0) do
          Inc(Result)
      else                                                      // descending
        begin
          while (Result < R) and (TCmpRel.Compare(A[Succ(Result)], A[Result]) <= 0) do
            Inc(Result);
          DoReverse(A, Result);
        end;
    end;
end;

class procedure TGBaseArrayHelper.InsertionSort(A: PItem; R: SizeInt);
var
  I, J: SizeInt;
  v: TFake;
begin
  for I := 1 to R do
    begin
      v := TFake(A[I]);
      J := I - 1;
      while (J >= 0) and (TCmpRel.Compare(T(v), A[J]) < 0) do
        begin
          TFake(A[J + 1]) := TFake(A[J]);
          Dec(J);
        end;
      TFake(A[J + 1]) := v;
    end;
end;

class function TGBaseArrayHelper.DoBinSearch(A: PItem; R: SizeInt; constref aValue: T): SizeInt;
var
  L, M, c: SizeInt;
begin
  //here R must be >= 0;
  Result := -1;
  L := 0;
  c := TCmpRel.Compare(A[R], A[0]);
  if c > 0 then  //ascending
    begin
      while L < R do
        begin
          M := L + (R - L) shr 1;
          c := TCmpRel.Compare(A[M], aValue);
          if c < 0 then
            L := Succ(M)
          else
            begin
              if c = 0 then
                exit(M);
              R := M;
            end;
        end;
      //here L >= R
      if TCmpRel.Compare(A[R], aValue) = 0 then
        Result := R;
    end
  else
    if c < 0 then  //descending
      begin
        while L < R do
          begin
            M := L + ((R - L) shr 1);
            c := TCmpRel.Compare(A[M], aValue);
            if c > 0 then
              L := Succ(M)
            else
              begin
                if c = 0 then
                  exit(M);
                R := M;
              end;
          end;
        //here L >= R
        if TCmpRel.Compare(A[R], aValue) = 0 then
          Result := R;
      end
    else           //constant
      if TCmpRel.Compare(A[L], aValue) = 0 then
        exit(L);
end;

class function TGBaseArrayHelper.DoBinSearchPosA(A: PItem; R: SizeInt; constref aValue: T): TSearchResult;
var
  L, M, Cmp: SizeInt;
begin
  //here exists at least 2 ascending elements;
  Result.FoundIndex := -1;
  L := 0;
  while L < R do
    begin
      M := L + (R - L) shr 1;
      Cmp := TCmpRel.Compare(A[M], aValue);
      if Cmp < 0 then
        L := Succ(M)
      else
        begin
          if Cmp = 0 then
            begin
              Result.FoundIndex := M;
              Result.InsertIndex := Succ(M);
              exit;
            end;
          R := M;
        end;
    end;
  //here L >= R
  Cmp := TCmpRel.Compare(aValue, A[R]);
  if Cmp < 0 then
    Result.InsertIndex := R
  else
    begin
      Result.InsertIndex := Succ(R);
      if Cmp = 0 then
        Result.FoundIndex := R;
    end;
end;

class function TGBaseArrayHelper.DoBinSearchPosD(A: PItem; R: SizeInt; constref aValue: T): TSearchResult;
var
  L, M, Cmp: SizeInt;
begin
  //here exists at least 2 descending elements
  Result.FoundIndex := -1;
  L := 0;
  while L < R do
    begin
      M := L + (R - L) shr 1;
      Cmp := TCmpRel.Compare(A[M], aValue);
      if Cmp > 0 then
        L := Succ(M)
      else
        begin
          if Cmp = 0 then
            begin
              Result.FoundIndex := M;
              Result.InsertIndex := Succ(M);
              exit;
            end;
          R := M;
        end;
    end;
  //here L >= R
  Cmp := TCmpRel.Compare(aValue, A[R]);
  if Cmp > 0 then
    Result.InsertIndex := R
  else
    begin
      Result.InsertIndex := Succ(R);
      if Cmp = 0 then
        Result.FoundIndex := R;
    end;
end;

class function TGBaseArrayHelper.DoBinSearchPos(A: PItem; R: SizeInt; constref aValue: T): TSearchResult;
var
  Cmp: SizeInt;
begin
  //here R must be >= 0;
  Cmp := TCmpRel.Compare(A[R], A[0]);
  if Cmp > 0 then   //ascending
    Result := DoBinSearchPosA(A, R, aValue)
  else
    if Cmp < 0 then //descending
      Result := DoBinSearchPosD(A, R, aValue)
    else            //constant
      begin
        Cmp := TCmpRel.Compare(aValue, A[R]);
        Result.FoundIndex := -1;
        if Cmp < 0 then
           Result.InsertIndex := 0
        else
          begin
            Result.InsertIndex := Succ(R);
            if Cmp = 0 then
              Result.FoundIndex := 0;
          end;
      end;
end;

class procedure TGBaseArrayHelper.DoHeapSort(A: PItem; R: SizeInt);
var
  I, Curr, Next: SizeInt;
  v: TFake;
begin
  if R > HEAP_INSERT_CUTOFF then
    begin
      for I := Pred(Succ(R) shr 1) downto 0 do
        begin
          Curr := I;
          Next := Succ(I shl 1);
          v := TFake(A[Curr]);
          while Next <= R do
            begin
              if(Succ(Next) <= R) and (TCmpRel.Compare(A[Next], A[Succ(Next)]) < 0)then
                Inc(Next);
              if TCmpRel.Compare(T(v), A[Next]) >= 0 then
                break;
              TFake(A[Curr]) := TFake(A[Next]);
              Curr := Next;
              Next := Succ(Next shl 1);
            end;
          TFake(A[Curr]) := v;
        end;
      for I := R downto 1 do
        begin
          Curr := 0;
          Next := 1;
          v := TFake(A[I]);
          TFake(A[I]) := TFake(A[0]);
          while Next < I do
            begin
              if(Succ(Next) < I) and (TCmpRel.Compare(A[Next], A[Succ(Next)]) < 0) then
                Inc(Next);
              TFake(A[Curr]) := TFake(A[Next]);
              Curr := Next;
              Next := Succ(Next shl 1);
            end;
          Next := Pred(Curr) shr 1;
          while (Curr > 0) and (TCmpRel.Compare(T(v), A[Next]) > 0) do
            begin
              TFake(A[Curr]) := TFake(A[Next]);
              Curr := Next;
              Next := Pred(Next) shr 1;
            end;
          TFake(A[Curr]) := v;
        end;
    end
  else
    InsertionSort(A, R);
end;

class function TGBaseArrayHelper.QSplitR(A: PItem; R: SizeInt): TSortSplit;
var
  Pivot: T;
  v: TFake;
  pL, pR: SizeInt;
begin
  Pivot := A[Succ(Random(Pred(R)))];
  pL := -1;
  pR := Succ(R);
  repeat
    repeat Inc(pL) until TCmpRel.Compare(A[pL], Pivot) >= 0;
    repeat Dec(pR) until TCmpRel.Compare(A[pR], Pivot) <= 0;
    if pL > pR then break;
    v := TFake(A[pL]);
    TFake(A[pL]) := TFake(A[pR]);
    TFake(A[pR]) := v;
  until False;
  Result.Left := pR;
  Result.Right := pL;
end;

class procedure TGBaseArrayHelper.DoQSort(A: PItem; R: SizeInt);
begin
  while R > QUICK_INSERT_CUTOFF do
    with QSplitR(A, R) do
      if Left <= R - Right then
        begin
          DoQSort(A, Left);
          A := @A[Right];
          R -= Right;
        end
      else
        begin
          DoQSort(@A[Right], R - Right);
          R := Left;
        end;
  if R > 0 then
    InsertionSort(A, R);
end;

class function TGBaseArrayHelper.MedianOf3(constref v1, v2, v3: T): T;
begin
  Result := v2;
  if TCmpRel.Compare(v1, Result) < 0 then
    begin
      if TCmpRel.Compare(v3, Result) < 0 then
        begin
          if TCmpRel.Compare(v1, v3) < 0 then
            Result := v3
          else
            Result := v1;
        end;
    end
  else { v1 >= Result }
    begin
      if TCmpRel.Compare(v3, Result) > 0 then
        begin
          if TCmpRel.Compare(v1, v3) > 0 then
            Result := v3
          else
            Result := v1;
        end;
    end;
end;

class function TGBaseArrayHelper.QSplitMo9(A: PItem; R: SizeInt): TSortSplit;
var
  Pivot: T;
  v: TFake;
  pL, pR: SizeInt;
begin
  if R > MEDIAN_OF9_CUTOFF then
    Pivot := MedianOf3(
      MedianOf3(A[0],
                A[Succ(R) shr 3],
                A[Succ(R) shr 2]),
      MedianOf3(A[Succ(R) shr 1 - Succ(R) shr 3],
                A[Succ(R) shr 1],
                A[Succ(R) shr 1 + Succ(R) shr 3]),
      MedianOf3(A[R - Succ(R) shr 2],
                A[R - Succ(R) shr 3],
                A[R]))
  else
    Pivot := MedianOf3(A[0], A[Succ(R) shr 1], A[R]);
  pL := -1;
  pR := Succ(R);
  repeat
    repeat Inc(pL) until  TCmpRel.Compare(A[pL], Pivot) >= 0;
    repeat Dec(pR) until  TCmpRel.Compare(A[pR], Pivot) <= 0;
    if pL > pR then break;
    v := TFake(A[pL]);
    TFake(A[pL]) := TFake(A[pR]);
    TFake(A[pR]) := v;
  until False;
  Result.Left := pR;
  Result.Right := pL;
end;

class procedure TGBaseArrayHelper.DoIntroSort(A: PItem; R, Ttl: SizeInt);
begin
  if R > QUICK_INSERT_CUTOFF then
    if Ttl > 0 then
      with QSplitMo9(A, R) do
        begin
          DoIntroSort(A, Left, Pred(Ttl));
          DoIntroSort(@A[Right], R - Right, Pred(Ttl));
        end
    else
      DoHeapSort(A, R)
  else
    if R > 0 then
      InsertionSort(A, R);
end;

class function TGBaseArrayHelper.DPQSplit(A: PItem; R: SizeInt): TSortSplit;
var
  v, Pivot1, Pivot2: TFake;
  pL, pR, I: SizeInt;
begin
  pL := Succ(Random(Pred(R shr 1)));
  pR := Pred(R - Random(Pred(R shr 1)));

  if TCmpRel.Compare(A[pL], A[pR]) <= 0 then
    begin
      Pivot1 := TFake(A[pL]);
      TFake(A[pL]) := TFake(A[0]);
      Pivot2 := TFake(A[pR]);
      TFake(A[pR]) := TFake(A[R]);
    end
  else
    begin
      Pivot2 := TFake(A[pL]);
      TFake(A[pL]) := TFake(A[R]);
      Pivot1 := TFake(A[pR]);
      TFake(A[pR]) := TFake(A[0]);
    end;

  pL := 1;
  I  := 1;
  pR := Pred(R);
  while I <= pR do
    begin
      v := TFake(A[I]);
      if TCmpRel.Compare(T(v), T(Pivot1)) < 0 then
        begin
          TFake(A[I]) := TFake(A[pL]);
          TFake(A[pL]) := v;
          Inc(pL);
        end
      else
        if TCmpRel.Compare(T(v), T(Pivot2)) > 0 then
          begin
            while (pR >= I) and (TCmpRel.Compare(A[pR], T(Pivot2)) > 0) do
              Dec(pR);
            if pR < I then
              break;
            if TCmpRel.Compare(A[pR], T(Pivot1)) < 0 then
              begin
                TFake(A[I]) := TFake(A[pL]);
                TFake(A[pL]) := TFake(A[pR]);
                Inc(pL);
              end
            else
              TFake(A[I]) := TFake(A[pR]);
            TFake(A[pR]) := v;
            Dec(pR);
          end;
      Inc(I);
    end;

  TFake(A[0]) := TFake(A[pL - 1]);
  TFake(A[pL - 1]) := Pivot1;
  TFake(A[R]) := TFake(A[pR + 1]);
  TFake(A[pR + 1]) := Pivot2;

  Result.Left := pL - 1;
  Result.Right := pR + 1;
end;

class procedure TGBaseArrayHelper.DoDPQSort(A: PItem; R: SizeInt);
begin
  if R > DPQ_INSERT_CUTOFF then
    with DPQSplit(A, R) do
      begin
        DoDPQSort(A, Left - 1);
        DoDPQSort(@A[Right + 1], R - Right - 1);
        if TCmpRel.Compare(A[Left], A[Right]) <> 0 then
          DoDPQSort(@A[Left + 1], Right - Left - 2);
      end
  else
    if R > 0 then
      InsertionSort(A, R);
end;

class function TGBaseArrayHelper.QSelectR(A: PItem; R, N: SizeInt): T;
var
  v: TFake;
  Pivot: T;
  L, pL, pR: SizeInt;
begin
  L := 0;
  while L < Pred(R) do
    begin
      Pivot := A[L + Random(Succ(R - L))];
      pL := Pred(L);
      pR := Succ(R);
      repeat
        repeat Inc(pL) until TCmpRel.Compare(A[pL], Pivot) >= 0;
        repeat Dec(pR) until TCmpRel.Compare(A[pR], Pivot) <= 0;
        if pL >= pR then break;
        v := TFake(A[pL]);
        TFake(A[pL]) := TFake(A[pR]);
        TFake(A[pR]) := v;
      until False;
      if pL = pR then
        begin
          if pL > L then
            Dec(pR)
          else
            if pL < R then
              Inc(pL);
        end;
      if pR < N then L := pL;
      if pL > N then R := pR;
    end;
  if (L < R) and (TCmpRel.Compare(A[L], A[R]) > 0) then
    begin
      v := TFake(A[L]);
      TFake(A[L]) := TFake(A[R]);
      TFake(A[R]) := v;
    end;
  Result := A[N];
end;

class function TGBaseArrayHelper.SequentSearch(constref A: array of T; constref aValue: T): SizeInt;
begin
  for Result := 0 to System.High(A) do
    if TCmpRel.Compare(aValue, A[Result]) = 0 then
      exit;
  Result := -1;
end;

class function TGBaseArrayHelper.BinarySearch(constref A: array of T; constref aValue: T): SizeInt;
begin
  Result := High(A);
  if Result >= 0 then
    Result := DoBinSearch(@A[0], Result, aValue);
end;

class function TGBaseArrayHelper.BinarySearchPos(constref A: array of T; constref aValue: T): TSearchResult;
var
  hi: SizeInt;
begin
  hi := High(A);
  if hi >= 0 then
    Result := DoBinSearchPos(@A[0], hi, aValue)
  else
    begin
      Result.FoundIndex := hi;
      Result.InsertIndex := hi;
    end;
end;

class function TGBaseArrayHelper.IndexOfMin(constref A: array of T): SizeInt;
var
  R, I: SizeInt;
  m: T;
begin
  R := System.High(A);
  if R >= 0 then
    begin
      Result := 0;
      m := A[0];
      for I := 1 to R do
        if TCmpRel.Compare(A[I], m) < 0 then
          begin
            m := A[I];
            Result := I;
          end;
    end
  else
    Result := R;
end;

class function TGBaseArrayHelper.IndexOfMax(constref A: array of T): SizeInt;
var
  R, I: SizeInt;
  m: T;
begin
  R := System.High(A);
  if R >= 0 then
    begin
      Result := 0;
      m := A[0];
      for I := 1 to R do
        if TCmpRel.Compare(m, A[I]) < 0 then
          begin
            m := A[I];
            Result := I;
          end;
    end
  else
    Result := R;
end;

class function TGBaseArrayHelper.GetMin(constref A: array of T): TOptional;
var
  v: T;
begin
  if FindMin(A, v) then
    Result.Assign(v);
end;

class function TGBaseArrayHelper.GetMax(constref A: array of T): TOptional;
var
  v: T;
begin
  if FindMax(A, v) then
    Result.Assign(v);
end;

class function TGBaseArrayHelper.FindMin(constref A: array of T; out aValue: T): Boolean;
var
  R, I: SizeInt;
begin
  R := System.High(A);
  Result := R >= 0;
  if Result then
    begin
      aValue := A[0];
      for I := 1 to R do
        if TCmpRel.Compare(A[I], aValue) < 0 then
          aValue := A[I];
    end;
end;

class function TGBaseArrayHelper.FindMax(constref A: array of T; out aValue: T): Boolean;
var
  R, I: SizeInt;
begin
  R := System.High(A);
  Result := R >= 0;
  if Result then
    begin
      aValue := A[0];
      for I := 1 to R do
        if TCmpRel.Compare(aValue, A[I]) < 0 then
          aValue := A[I];
    end;
end;

class function TGBaseArrayHelper.FindMinMax(constref A: array of T; out aMin, aMax: T): Boolean;
var
  R, I: SizeInt;
begin
  R := System.High(A);
  Result := R >= 0;
  if Result then
    begin
      aMin := A[0];
      aMax := A[0];
      for I := 1 to R do
        if TCmpRel.Compare(aMax, A[I]) < 0 then
          aMax := A[I]
        else
          if TCmpRel.Compare(A[I], aMin) < 0 then
            aMin := A[I];
    end;
end;

class function TGBaseArrayHelper.FindNthSmallest(var A: array of T; N: SizeInt; out aValue: T): Boolean;
var
  R: SizeInt;
begin
  R := System.High(A);
  if R < 0 then
    exit(False);
  if N <= 0 then
    exit(FindMin(A, aValue));
  if N >= R then
    exit(FindMax(A, aValue));
  aValue := QSelectR(@A[0], R, N);
  Result := True;
end;

class function TGBaseArrayHelper.NthSmallest(var A: array of T; N: SizeInt): TOptional;
var
  v: T;
begin
  if FindNthSmallest(A, N, v) then
    Result.Assign(v);
end;

class function TGBaseArrayHelper.FindNthSmallestND(constref A: array of T; N: SizeInt; out aValue: T): Boolean;
begin
  Result := FindNthSmallest(CreateCopy(A), N, aValue);
end;

class function TGBaseArrayHelper.NthSmallestND(constref A: array of T; N: SizeInt): TOptional;
var
  v: T;
begin
  if FindNthSmallestND(A, N, v) then
    Result.Assign(v);
end;

class function TGBaseArrayHelper.NextPermutation2Asc(var A: array of T): Boolean;
var
  I, J, R: SizeInt;
  v: TFake;
begin
  R := System.High(A);
  J := -1;
  for I := Pred(R) downto 0 do
    if TCmpRel.Compare(A[I], A[Succ(I)]) > 0 then
      begin
        J := I;
        break;
      end;
  if J < 0 then exit(False);
  for I := R downto 0 do
    if TCmpRel.Compare(A[J], A[I]) > 0 then
      begin
        v := TFake(A[I]);
        TFake(A[I]) := TFake(A[J]);
        TFake(A[J]) := v;
        break;
      end;
  DoReverse(@A[Succ(J)], R - Succ(J));
  Result := True;
end;

class function TGBaseArrayHelper.NextPermutation2Desc(var A: array of T): Boolean;
var
  I, J, R: SizeInt;
  v: TFake;
begin
  R := System.High(A);
  J := -1;
  for I := Pred(R) downto 0 do
    if TCmpRel.Compare(A[I], A[Succ(I)]) < 0 then
      begin
        J := I;
        break;
      end;
  if J < 0 then exit(False);
  for I := R downto 0 do
    if TCmpRel.Compare(A[J], A[I]) < 0 then
      begin
        v := TFake(A[I]);
        TFake(A[I]) := TFake(A[J]);
        TFake(A[J]) := v;
        break;
      end;
  DoReverse(@A[Succ(J)], R - Succ(J));
  Result := True;
end;

class function TGBaseArrayHelper.IsNonDescending(constref A: array of T): Boolean;
var
  I: SizeInt;
begin
  for I := 0 to Pred(System.High(A)) do
    if TCmpRel.Compare(A[Succ(I)], A[I]) < 0 then
      exit(False);
  Result := True;
end;

class function TGBaseArrayHelper.IsStrictAscending(constref A: array of T): Boolean;
var
  I, R: SizeInt;
begin
  R := System.High(A);
  if R > 0 then
    begin
      for I := 0 to Pred(R) do
        if TCmpRel.Compare(A[Succ(I)], A[I]) <= 0 then
          exit(False);
      Result := True;
    end
  else
    Result := False;
end;

class function TGBaseArrayHelper.IsNonAscending(constref A: array of T): Boolean;
var
  I: SizeInt;
begin
  for I := 0 to Pred(System.High(A)) do
    if TCmpRel.Compare(A[I], A[Succ(I)]) < 0 then
      exit(False);
  Result := True;
end;

class function TGBaseArrayHelper.IsStrictDescending(constref A: array of T): Boolean;
var
  I, R: SizeInt;
begin
  R := System.High(A);
  if R > 0 then
    begin
      for I := 0 to Pred(R) do
        if TCmpRel.Compare(A[I], A[Succ(I)]) <= 0 then
          exit(False);
      Result := True;
    end
  else
    Result := False;
end;

class function TGBaseArrayHelper.Same(constref A, B: array of T): Boolean;
var
  R, I: SizeInt;
begin
  R := System.High(A);
  if System.High(B) <> R then
    exit(False);
  for I := 0 to R do
    if TCmpRel.Compare(A[I], B[I]) <> 0 then
      exit(False);
  Result := True;
end;

class procedure TGBaseArrayHelper.QuickSort(var A: array of T; o: TSortOrder);
var
  R: SizeInt;
begin
  R := System.High(A);
  if R > 0 then
    if CountRun2Asc(@A[0], R) < R then
      begin
        DoQSort(@A[0], R);
        if o = soDesc then
          Reverse(A);
      end
    else
      if (o = soDesc) and (TCmpRel.Compare(A[0], A[R]) <> 0) then
        Reverse(A);
end;

class procedure TGBaseArrayHelper.IntroSort(var A: array of T; o: TSortOrder);
var
  R: SizeInt;
begin
  R := System.High(A);
  if R > 0 then
    if CountRun2Asc(@A[0], R) < R then
      begin
        DoIntroSort(@A[0], R, Pred(LGUtils.NSB(R + 1)) * INTRO_LOG_FACTOR);
        if o = soDesc then
          Reverse(A);
      end
    else
      if (o = soDesc) and (TCmpRel.Compare(A[0], A[R]) <> 0) then
        Reverse(A);
end;

class procedure TGBaseArrayHelper.DualPivotQuickSort(var A: array of T; o: TSortOrder);
var
  R: SizeInt;
begin
  R := System.High(A);
  if R > 0 then
    if CountRun2Asc(@A[0], R) < R then
      begin
        DoDPQSort(@A[0], R);
        if o = soDesc then
          Reverse(A);
      end
    else
      if (o = soDesc) and (TCmpRel.Compare(A[0], A[R]) <> 0) then
        Reverse(A);
end;

class procedure TGBaseArrayHelper.MergeSort(var A: array of T; o: TSortOrder);
var
  R: SizeInt;
begin
  R := High(A);
  if R > 0 then
    if o = soAsc then
      TMergeSort.SortAsc(@A[0], R)
    else
      TMergeSort.SortDesc(@A[0], R);
end;

class procedure TGBaseArrayHelper.Sort(var A: array of T; o: TSortOrder);
begin
  IntroSort(A, o);
end;

class function TGBaseArrayHelper.Sorted(constref A: array of T; o: TSortOrder): TArray;
begin
  Result := CreateCopy(A);
  Sort(Result, o);
end;

class function TGBaseArrayHelper.SelectDistinct(constref A: array of T): TArray;
var
  I, J, Hi: SizeInt;
begin
  Result := Sorted(A);
  Hi := System.High(Result);
  if Hi < 1 then
    exit;
  I := 0;
  for J := 1 to Hi do
    begin
      if TCmpRel.Compare(Result[I], Result[J]) = 0 then
        continue;
      Inc(I);
      if J > I then
        Result[I] := Result[J];
    end;
  System.SetLength(Result, Succ(I));
end;

{ TGComparableArrayHelper.TMergeSort }

procedure TGComparableArrayHelper.TMergeSort.CollapseA;
var
  I: SizeInt;
begin
  while FStackSize > 1 do
    begin
      I := FStackSize - 2;
      if (I > 0) and (FStack[I - 1].Count <= FStack[I].Count + FStack[I + 1].Count) then
        begin
          if FStack[I - 1].Count < FStack[I + 1].Count then
            Dec(I);
        end
      else
        if FStack[I].Count > FStack[I + 1].Count then
          break;
      MergeAtA(I);
    end;
end;

procedure TGComparableArrayHelper.TMergeSort.CollapseD;
var
  I: SizeInt;
begin
  while FStackSize > 1 do
    begin
      I := FStackSize - 2;
      if (I > 0) and (FStack[I - 1].Count <= FStack[I].Count + FStack[I + 1].Count) then
        begin
          if FStack[I - 1].Count < FStack[I + 1].Count then
            Dec(I);
        end
      else
        if FStack[I].Count > FStack[I + 1].Count then
          break;
      MergeAtD(I);
    end;
end;

procedure TGComparableArrayHelper.TMergeSort.ForceCollapseA;
var
  I: SizeInt;
begin
  while FStackSize > 1 do
    begin
      I := FStackSize - 2;
      if (I > 0) and (FStack[I - 1].Count < FStack[I + 1].Count) then
        MergeAtA(I - 1)
      else
        MergeAtA(I);
    end;
end;

procedure TGComparableArrayHelper.TMergeSort.ForceCollapseD;
var
  I: SizeInt;
begin
  while FStackSize > 1 do
    begin
      I := FStackSize - 2;
      if (I > 0) and (FStack[I - 1].Count < FStack[I + 1].Count) then
        MergeAtD(I - 1)
      else
        MergeAtD(I);
    end;
end;

procedure TGComparableArrayHelper.TMergeSort.MergeAtA(aIndex: SizeInt);
var
  Base, CountLo, CountHi: SizeInt;
begin
  Base := FStack[aIndex].Base;
  CountLo := FStack[aIndex].Count;
  CountHi := FStack[aIndex + 1].Count;
  FStack[aIndex].Count := CountLo + CountHi;
  if aIndex = FStackSize - 3 then
    begin
      FStack[aIndex + 1].Base := FStack[aIndex + 2].Base;
      FStack[aIndex + 1].Count := FStack[aIndex + 2].Count;
    end;
  Dec(FStackSize);
  if CountLo <= CountHi then
    MergeLoA(Base, CountLo, CountHi)
  else
    MergeHiA(Base, CountLo, CountHi);
end;

procedure TGComparableArrayHelper.TMergeSort.MergeAtD(aIndex: SizeInt);
var
  Base, CountLo, CountHi: SizeInt;
begin
  Base := FStack[aIndex].Base;
  CountLo := FStack[aIndex].Count;
  CountHi := FStack[aIndex + 1].Count;
  FStack[aIndex].Count := CountLo + CountHi;
  if aIndex = FStackSize - 3 then
    begin
      FStack[aIndex + 1].Base := FStack[aIndex + 2].Base;
      FStack[aIndex + 1].Count := FStack[aIndex + 2].Count;
    end;
  Dec(FStackSize);
  if CountLo <= CountHi then
    MergeLoD(Base, CountLo, CountHi)
  else
    MergeHiD(Base, CountLo, CountHi);
end;

procedure TGComparableArrayHelper.TMergeSort.MergeLoA(From, CountLo, CountHi: SizeInt);
var
  pLo,           // position in low part   (in buffer)
  pHi,           // position in high part  (in data array)
  pDst: SizeInt; // current CreateMerge position (in data array)
  LocA: PItem;   // local pointer to data array
  LocB: PItem;   // local pointer to buffer
begin
  LocA := FData;
  if FData[Pred(From + CountLo)] > FData[From + CountLo] then
    begin
      LocB := EnsureBufferCapacity(CountLo);
      {$IFNDEF FPC_REQUIRES_PROPER_ALIGNMENT}
      System.Move(LocA[From], LocB[0], CountLo * SizeOf(T));
      {$ELSE}
      CopyItems(@LocA[From], LocB, CountLo);
      {$ENDIF}
      if LocA[Pred(From + CountLo + CountHi)] >= LocA[From] then
        begin
          pLo := 0;
          pHi := From + CountLo;
          pDst := From;
          CountHi := Pred(From + CountLo + CountHi);
          repeat
            if LocB[pLo] <= LocA[pHi] then
              begin
                TFake(LocA[pDst]) := TFake(LocB[pLo]);
                Inc(pLo);
              end
            else
              begin
                TFake(LocA[pDst]) := TFake(LocA[pHi]);
                Inc(pHi);
              end;
            Inc(pDst);
          until (pLo >= CountLo) or (pHi > CountHi);
          if pLo < CountLo then
            {$IFNDEF FPC_REQUIRES_PROPER_ALIGNMENT}
            System.Move(LocB[pLo], LocA[pDst], (CountLo - pLo) * SizeOf(T)); ///
            {$ELSE}
            CopyItems(@LocB[pLo], @LocA[pDst], CountLo - pLo);
            {$ENDIF}
        end
      else
        begin
          {$IFNDEF FPC_REQUIRES_PROPER_ALIGNMENT}
          System.Move(LocA[From + CountLo], LocA[From], CountHi * SizeOf(T));///
          System.Move(LocB[0], LocA[From + CountHi], CountLo * SizeOf(T));   ///
          {$ELSE}
          CopyItems(@LocA[From + CountLo], @LocA[From], CountHi);
          CopyItems(LocB, @LocA[From + CountHi], CountLo);
          {$ENDIF}
        end;
    end;
end;

procedure TGComparableArrayHelper.TMergeSort.MergeLoD(From, CountLo, CountHi: SizeInt);
var
  pLo,           // position in low part   (in buffer)
  pHi,           // position in high part  (in data array)
  pDst: SizeInt; // current CreateMerge position (in data array)
  LocA: PItem;    // local pointer to data array
  LocB: PItem;    // local pointer to buffer
begin
  LocA := FData;
  if FData[Pred(From + CountLo)] < FData[From + CountLo] then
    begin
      LocB := EnsureBufferCapacity(CountLo);
      {$IFNDEF FPC_REQUIRES_PROPER_ALIGNMENT}
      System.Move(LocA[From], LocB[0], CountLo * SizeOf(T)); ///
      {$ELSE}
      CopyItems(@LocA[From], LocB, CountLo);
      {$ENDIF}
      if LocA[Pred(From + CountLo + CountHi)] <= LocA[From] then
        begin
          pLo := 0;
          pHi := From + CountLo;
          pDst := From;
          CountHi := Pred(From + CountLo + CountHi);
          repeat
            if LocB[pLo] >= LocA[pHi] then
              begin
                TFake(LocA[pDst]) := TFake(LocB[pLo]);
                Inc(pLo);
              end
            else
              begin
                TFake(LocA[pDst]) := TFake(LocA[pHi]);
                Inc(pHi);
              end;
            Inc(pDst);
          until (pLo >= CountLo) or (pHi > CountHi);
          if pLo < CountLo then
            {$IFNDEF FPC_REQUIRES_PROPER_ALIGNMENT}
            System.Move(LocB[pLo], LocA[pDst], (CountLo - pLo) * SizeOf(T));///
            {$ELSE}
            CopyItems(@LocB[pLo], @LocA[pDst], CountLo - pLo);
            {$ENDIF}
        end
      else
        begin
          {$IFNDEF FPC_REQUIRES_PROPER_ALIGNMENT}
          System.Move(LocA[From + CountLo], LocA[From], CountHi * SizeOf(T));///
          System.Move(LocB[0], LocA[From + CountHi], CountLo * SizeOf(T));   ///
          {$ELSE}
          CopyItems(@LocA[From + CountLo], @LocA[From], CountHi);
          CopyItems(LocB, @LocA[From + CountHi], CountLo);
          {$ENDIF}
        end;
    end;
end;

procedure TGComparableArrayHelper.TMergeSort.MergeHiA(From, CountLo, CountHi: SizeInt);
var
  pLo,           // position in low part   (in data array)
  pHi,           // position in high part  (in buffer)
  pDst: SizeInt; // current CreateMerge position (in data array)
  LocA: PItem;   // local pointer to data array
  LocB: PItem;   // local pointer to buffer
begin
  LocA := FData;
  if FData[Pred(From + CountLo)] > FData[From + CountLo] then
    begin
      LocB := EnsureBufferCapacity(CountHi);
      {$IFNDEF FPC_REQUIRES_PROPER_ALIGNMENT}
      System.Move(LocA[From + CountLo], LocB[0], CountHi * SizeOf(T));///
      {$ELSE}
      CopyItems(@LocA[From + CountLo], LocB, CountHi);
      {$ENDIF}
      if LocA[Pred(From + CountLo + CountHi)] >= LocA[From] then
        begin
          pLo := Pred(From + CountLo);
          pHi := CountHi - 1;
          pDst := Pred(From + CountLo + CountHi);
          repeat
            if LocA[pLo] > LocB[pHi] then
              begin
                TFake(LocA[pDst]) := TFake(LocA[pLo]);
                Dec(pLo);
              end
            else
              begin
                TFake(LocA[pDst]) := TFake(LocB[pHi]);
                Dec(pHi);
              end;
            Dec(pDst);
          until (pLo < From) or (pHi < 0);
          if pHi >= 0 then
            {$IFNDEF FPC_REQUIRES_PROPER_ALIGNMENT}
            System.Move(LocB[0], LocA[From], Succ(pHi) * SizeOf(T)); ///
            {$ELSE}
            CopyItems(LocB, @LocA[From], Succ(pHi));
            {$ENDIF}
        end
      else
        begin
          {$IFNDEF FPC_REQUIRES_PROPER_ALIGNMENT}
          System.Move(LocA[From], LocA[From + CountHi], CountLo * SizeOf(T)); ///
          System.Move(LocB[0], LocA[From], CountHi * SizeOf(T));  ///
          {$ELSE}
          CopyItems(@LocA[From], @LocA[From + CountHi], CountLo);
          CopyItems(LocB, @LocA[From], CountHi);
          {$ENDIF}
        end;
    end;
end;

procedure TGComparableArrayHelper.TMergeSort.MergeHiD(From, CountLo, CountHi: SizeInt);
var
  pLo,           // position in low part   (in data array)
  pHi,           // position in high part  (in buffer)
  pDst: SizeInt; // current CreateMerge position (in data array)
  LocA: PItem;   // local pointer to data array
  LocB: PItem;   // local pointer to buffer
begin
  LocA := FData;
  if FData[Pred(From + CountLo)] < FData[From + CountLo] then
    begin
      LocB := EnsureBufferCapacity(CountHi);
      {$IFNDEF FPC_REQUIRES_PROPER_ALIGNMENT}
      System.Move(LocA[From + CountLo], LocB[0], CountHi * SizeOf(T)); ///
      {$ELSE}
      CopyItems(@LocA[From + CountLo], LocB, CountHi);
      {$ENDIF}
      if LocA[Pred(From + CountLo + CountHi)] <= LocA[From] then
        begin
          pLo := Pred(From + CountLo);
          pHi := CountHi - 1;
          pDst := Pred(From + CountLo + CountHi);
          repeat
            if LocA[pLo] < LocB[pHi] then
              begin
                TFake(LocA[pDst]) := TFake(LocA[pLo]);
                Dec(pLo);
              end
            else
              begin
                TFake(LocA[pDst]) := TFake(LocB[pHi]);
                Dec(pHi);
              end;
            Dec(pDst);
          until (pLo < From) or (pHi < 0);
          if pHi >= 0 then
            {$IFNDEF FPC_REQUIRES_PROPER_ALIGNMENT}
            System.Move(LocB[0], LocA[From], Succ(pHi) * SizeOf(T)); ///
            {$ELSE}
            CopyItems(LocB, @LocA[From], Succ(pHi));
            {$ENDIF}
        end
      else
        begin
          {$IFNDEF FPC_REQUIRES_PROPER_ALIGNMENT}
          System.Move(LocA[From], LocA[From + CountHi], CountLo * SizeOf(T)); ///
          System.Move(LocB[0], LocA[From], CountHi * SizeOf(T));              ///
          {$ELSE}
          CopyItems(@LocA[From], @LocA[From + CountHi], CountLo);
          CopyItems(LocB, @LocA[From], CountHi);
          {$ENDIF}
        end;
    end;
end;

class procedure TGComparableArrayHelper.TMergeSort.InsertSortA(A: PItem; R, At: SizeInt);
var
  I, J: SizeInt;
  v: TFake;
begin
  if At < 1 then
    At := 1;
  for I := At to R do
    begin
      v := TFake(A[I]);
      J := I - 1;
      while (J >= 0) and (A[J] > T(v)) do
        begin
          TFake(A[J + 1]) := TFake(A[J]);
          Dec(J);
        end;
      TFake(A[J + 1]) := v;
    end;
end;

class procedure TGComparableArrayHelper.TMergeSort.InsertSortD(A: PItem; R, At: SizeInt);
var
  I, J: SizeInt;
  v: TFake;
begin
  if At < 1 then
    At := 1;
  for I := At to R do
    begin
      v := TFake(A[I]);
      J := I - 1;
      while (J >= 0) and (A[J] < T(v)) do
        begin
          TFake(A[J + 1]) := TFake(A[J]);
          Dec(J);
        end;
      TFake(A[J + 1]) := v;
    end;
end;

class function TGComparableArrayHelper.TMergeSort.CountRunAsc(A: PItem; R: SizeInt): SizeInt;
begin
  if R > 0 then
    begin
      Result := 1;
      if A[0] <= A[1] then  // ascending
        while (Result < R) and (A[Result] <= A[Succ(Result)]) do
          Inc(Result)
      else                 // descending
        begin
          while (Result < R) and (A[Result] > A[Succ(Result)]) do
            Inc(Result);
          DoReverse(A, Result);
        end;
    end
  else
    Result := 0;
end;

class function TGComparableArrayHelper.TMergeSort.CountRunDesc(A: PItem; R: SizeInt): SizeInt;
begin
  if R > 0 then
    begin
      Result := 1;
      if A[0] >= A[1] then  // descending
        while (Result < R) and (A[Result] >= A[Succ(Result)]) do
          Inc(Result)
      else                  // ascending
        begin
          while (Result < R) and (A[Result] < A[Succ(Result)]) do
            Inc(Result);
          DoReverse(A, Result);
        end;
    end
  else
    Result := 0;
end;

class procedure TGComparableArrayHelper.TMergeSort.SortAsc(A: PItem; R: SizeInt);
var
  RunLen, MinLen, Len, L: SizeInt;
  ms: TMergeSort;
begin
  if R + 1 > MIN_MERGE_LEN then
    begin
      ms.Init(A);
      L := 0;
      MinLen := MinRunLen(R - L + 1);
       repeat
         RunLen := Succ(CountRunAsc(@A[L], R - L));
         if RunLen < MinLen then
           begin
             Len := Math.Min(Succ(R - L), MinLen);
             InsertSortA(@A[L], Pred(Len), RunLen);
             RunLen := Len;
           end;
         ms.PushRun(L, RunLen);
         L += RunLen;
         ms.CollapseA;
       until L > R;
       ms.ForceCollapseA;
    end
  else
    InsertSortA(A, R, Succ(CountRunAsc(A, R)));
end;

class procedure TGComparableArrayHelper.TMergeSort.SortDesc(A: PItem; R: SizeInt);
var
  RunLen, MinLen, Len, L: SizeInt;
  ms: TMergeSort;
begin
  if R + 1 > MIN_MERGE_LEN then
    begin
      ms.Init(A);
      L := 0;
      MinLen := MinRunLen(R - L + 1);
       repeat
         RunLen := Succ(CountRunDesc(@A[L], R - L));
         if RunLen < MinLen then
           begin
             Len := Math.Min(Succ(R - L), MinLen);
             InsertSortD(@A[L], Pred(Len), RunLen);
             RunLen := Len;
           end;
         ms.PushRun(L, RunLen);
         L += RunLen;
         ms.CollapseD;
       until L > R;
       ms.ForceCollapseD;
    end
  else
    InsertSortD(A, R, Succ(CountRunDesc(A, R)));
end;


{ TGComparableArrayHelper }

class function TGComparableArrayHelper.CountRun2Asc(A: PItem; R: SizeInt): SizeInt;
begin
  Result := 0;
  while (Result < R) and (A[Result] = A[Succ(Result)]) do
    Inc(Result);
  if Result < R then
    begin
      Inc(Result);
      if A[Pred(Result)] < A[Result] then   // ascending
        while (Result < R) and (A[Result] <= A[Succ(Result)]) do
          Inc(Result)
      else                                  // descending
        begin
          while (Result < R) and (A[Succ(Result)] <= A[Result]) do
            Inc(Result);
          DoReverse(A, Result);
        end;
    end;
end;

class procedure TGComparableArrayHelper.InsertionSort(A: PItem; R: SizeInt);
var
  I, J: SizeInt;
  v: TFake;
begin
  for I := 1 to R do
    begin
      v := TFake(A[I]);
      J := I - 1;
      while (J >= 0) and (T(v) < A[J]) do
        begin
          TFake(A[J + 1]) := TFake(A[J]);
          Dec(J);
        end;
      TFake(A[J + 1]) := v;
    end;
end;

class function TGComparableArrayHelper.DoBinSearch(A: PItem; R: SizeInt; constref aValue: T): SizeInt;
var
  L, M: SizeInt;
begin
  //here R must be >= 0;
  Result := -1;
  L := 0;
  if A[R] > A[L] then  //ascending
    begin
      while L < R do
        begin
          M := L + (R - L) shr 1;
          if A[M] < aValue then
            L := Succ(M)
          else
            begin
              if A[M] = aValue then
                exit(M);
              R := M;
            end;
        end;
      //here L >= R
      if A[R] = aValue then
        Result := R;
    end
  else
    if A[R] < A[L] then  //descending
      begin
        while L < R do
          begin
            M := L + ((R - L) shr 1);
            if A[M] > aValue then
              L := Succ(M)
            else
              begin
                if A[M] = aValue then
                  exit(M);
                R := M;
              end;
          end;
        //here L >= R
        if A[R] = aValue then
          Result := R;
      end
    else              //constant
      if A[L] = aValue then
        Result := L;
end;

class function TGComparableArrayHelper.DoBinSearchPosA(A: PItem; R: SizeInt; constref aValue: T): TSearchResult;
var
  L, M: SizeInt;
begin
  //here exists at least 2 ascending elements;
  Result.FoundIndex := -1;
  L := 0;
  while L < R do
    begin
      M := L + (R - L) shr 1;
      if A[M] < aValue then
        L := Succ(M)
      else
        begin
          if A[M] = aValue then
            begin
              Result.FoundIndex := M;
              Result.InsertIndex := Succ(M);
              exit;
            end;
          R := M;
        end;
    end;
  //here L >= R
  if aValue < A[R] then
    Result.InsertIndex := R
  else
    begin
      Result.InsertIndex := Succ(R);
      if aValue = A[R] then
        Result.FoundIndex := R;
    end;
end;

class function TGComparableArrayHelper.DoBinSearchPosD(A: PItem; R: SizeInt; constref aValue: T): TSearchResult;
var
  L, M: SizeInt;
begin
  //here exists at least 2 descending elements
  Result.FoundIndex := -1;
  L := 0;
  while L < R do
    begin
      M := L + (R - L) shr 1;
      if A[M] > aValue then
        L := Succ(M)
      else
        begin
          if A[M] = aValue then
            begin
              Result.FoundIndex := M;
              Result.InsertIndex := Succ(M);
              exit;
            end;
          R := M;
        end;
    end;
  //here L >= R
  if aValue > A[R] then
    Result.InsertIndex := R
  else
    begin
      Result.InsertIndex := Succ(R);
      if aValue = A[R] then
        Result.FoundIndex := R;
    end;
end;

class function TGComparableArrayHelper.DoBinSearchPos(A: PItem; R: SizeInt; constref aValue: T): TSearchResult;
begin
  //here R must be >= 0;
  if A[0] < A[R] then   //ascending
    Result := DoBinSearchPosA(A, R, aValue)
  else
    if A[0] > A[R] then //descending
      Result := DoBinSearchPosD(A, R, aValue)
    else                //constant
      begin
        Result.FoundIndex := -1;
        if A[R] > aValue then
           Result.InsertIndex := 0
        else
          begin
            Result.InsertIndex := Succ(R);
            if A[R] = aValue then
              Result.FoundIndex := 0;
          end;
      end;
end;

class procedure TGComparableArrayHelper.DoHeapSort(A: PItem; R: SizeInt);
var
  I, Curr, Next: SizeInt;
  v: TFake;
begin
  if R > HEAP_INSERT_CUTOFF then
    begin
      for I := Pred(Succ(R) shr 1) downto 0 do
        begin
          Curr := I;
          Next := Succ(I shl 1);
          v := TFake(A[Curr]);
          while Next <= R do
            begin
              if(Succ(Next) <= R) and (A[Next] < A[Succ(Next)])then
                Inc(Next);
              if A[Next] <= T(v) then
                break;
              TFake(A[Curr]) := TFake(A[Next]);
              Curr := Next;
              Next := Succ(Next shl 1);
            end;
          TFake(A[Curr]) := v;
        end;
      for I := R downto 1 do
        begin
          Curr := 0;
          Next := 1;
          v := TFake(A[I]);
          TFake(A[I]) := TFake(A[0]);
          while Next < I do
            begin
              if(Succ(Next) < I) and (A[Next] < A[Succ(Next)]) then
                Inc(Next);
              TFake(A[Curr]) := TFake(A[Next]);
              Curr := Next;
              Next := Succ(Next shl 1);
            end;
          Next := Pred(Curr) shr 1;
          while (Curr > 0) and (T(v) > A[Next]) do
            begin
              TFake(A[Curr]) := TFake(A[Next]);
              Curr := Next;
              Next := Pred(Next) shr 1;
            end;
          TFake(A[Curr]) := v;
        end;
    end
  else
    InsertionSort(A, R);
end;

class function TGComparableArrayHelper.QSplitR(A: PItem; R: SizeInt): TSortSplit;
var
  Pivot: T;
  v: TFake;
  pL, pR: SizeInt;
begin
  Pivot := A[Succ(Random(Pred(R)))]; //shouldn't be first or last
  pL := -1;
  pR := Succ(R);
  repeat
    repeat Inc(pL) until A[pL] >= Pivot;
    repeat Dec(pR) until A[pR] <= Pivot;
    if pL > pR then break;
    v := TFake(A[pL]);
    TFake(A[pL]) := TFake(A[pR]);
    TFake(A[pR]) := v;
  until False;
  Result.Left := pR;
  Result.Right := pL;
end;

class procedure TGComparableArrayHelper.DoQSort(A: PItem; R: SizeInt);
begin
  while R > QUICK_INSERT_CUTOFF do
    with QSplitR(A, R) do
      if Left <= R - Right then
        begin
          DoQSort(A, Left);
          A := @A[Right];
          R -= Right;
        end
      else
        begin
          DoQSort(@A[Right], R - Right);
          R := Left;
        end;
  if R > 0 then
    InsertionSort(A, R);
end;

class function TGComparableArrayHelper.MedianOf3(constref v1, v2, v3: T): T;
begin
  Result := v2;
  if v1 < Result then
    begin
      if v3 < Result then
        begin
          if v1 < v3 then
            Result := v3
          else
            Result := v1;
        end;
    end
  else { v1 >= Result }
    begin
      if v3 > Result then
        begin
          if v1 > v3 then
            Result := v3
          else
            Result := v1;
        end;
    end;
end;

class function TGComparableArrayHelper.QSplitMo9(A: PItem; R: SizeInt): TSortSplit;
var
  Pivot: T;
  v: TFake;
  pL, pR: SizeInt;
begin
  if R > MEDIAN_OF9_CUTOFF then
    Pivot := MedianOf3(
      MedianOf3(A[0],
                A[Succ(R) shr 3],
                A[Succ(R) shr 2]),
      MedianOf3(A[Succ(R) shr 1 - Succ(R) shr 3],
                A[Succ(R) shr 1],
                A[Succ(R) shr 1 + Succ(R) shr 3]),
      MedianOf3(A[R - Succ(R) shr 2],
                A[R - Succ(R) shr 3],
                A[R]))
  else
    Pivot := MedianOf3(A[0], A[Succ(R) shr 1], A[R]);
  pL := -1;
  pR := Succ(R);
  repeat
    repeat Inc(pL) until A[pL] >= Pivot;
    repeat Dec(pR) until A[pR] <= Pivot;
    if pL > pR then break;
    v := TFake(A[pL]);
    TFake(A[pL]) := TFake(A[pR]);
    TFake(A[pR]) := v;
  until False;
  Result.Left := pR;
  Result.Right := pL;
end;

class procedure TGComparableArrayHelper.DoIntroSort(A: PItem; R, Ttl: SizeInt);
begin
  if R > QUICK_INSERT_CUTOFF then
    if Ttl > 0 then
      with QSplitMo9(A, R) do
        begin
          DoIntroSort(A, Left, Pred(Ttl));
          DoIntroSort(@A[Right], R - Right, Pred(Ttl));
        end
    else
      DoHeapSort(A, R)
  else
    if R > 0 then
      InsertionSort(A, R);
end;

class function TGComparableArrayHelper.DPQSplit(A: PItem; R: SizeInt): TSortSplit;
var
  v, Pivot1, Pivot2: TFake;
  pL, pR, I: SizeInt;
begin
  pL := Succ(Random(Pred(R shr 1)));
  pR := Pred(R - Random(Pred(R shr 1)));

  if A[pL] <= A[pR] then
    begin
      Pivot1 := TFake(A[pL]);
      TFake(A[pL]) := TFake(A[0]);
      Pivot2 := TFake(A[pR]);
      TFake(A[pR]) := TFake(A[R]);
    end
  else
    begin
      Pivot2 := TFake(A[pL]);
      TFake(A[pL]) := TFake(A[R]);
      Pivot1 := TFake(A[pR]);
      TFake(A[pR]) := TFake(A[0]);
    end;

  pL := 1;
  I  := 1;
  pR := Pred(R);
  while I <= pR do
    begin
      v := TFake(A[I]);
      if T(v) < T(Pivot1) then
        begin
          TFake(A[I]) := TFake(A[pL]);
          TFake(A[pL]) := v;
          Inc(pL);
        end
      else
        if T(Pivot2) < T(v) then
          begin
            while (pR >= I) and (T(Pivot2) < A[pR]) do
              Dec(pR);
            if pR < I then
              break;
            if A[pR] < T(Pivot1) then
              begin
                TFake(A[I]) := TFake(A[pL]);
                TFake(A[pL]) := TFake(A[pR]);
                Inc(pL);
              end
            else
              TFake(A[I]) := TFake(A[pR]);
            TFake(A[pR]) := v;
            Dec(pR);
          end;
      Inc(I);
    end;

  TFake(A[0]) := TFake(A[pL - 1]);
  TFake(A[pL - 1]) := Pivot1;
  TFake(A[R]) := TFake(A[pR + 1]);
  TFake(A[pR + 1]) := Pivot2;

  Result.Left := pL - 1;
  Result.Right := pR + 1;
end;

class procedure TGComparableArrayHelper.DoDPQSort(A: PItem; R: SizeInt);
begin
  if R > DPQ_INSERT_CUTOFF then
    with DPQSplit(A, R) do
      begin
        DoDPQSort(A, Left - 1);
        DoDPQSort(@A[Right + 1], R - Right - 1);
        if A[Left] <> A[Right] then
          DoDPQSort(@A[Left + 1], Right - Left - 2);
      end
  else
    if R > 0 then
      InsertionSort(A, R);
end;

class function TGComparableArrayHelper.QSelectR(A: PItem; R, N: SizeInt): T;
var
  v: TFake;
  Pivot: T;
  L, pL, pR: SizeInt;
begin
  L := 0;
  while L < Pred(R) do
    begin
      Pivot := A[L + Random(Succ(R - L))];
      pL := Pred(L);
      pR := Succ(R);
      repeat
        repeat Inc(pL) until A[pL] >= Pivot;
        repeat Dec(pR) until A[pR] <= Pivot;
        if pL >= pR then break;
        v := TFake(A[pL]);
        TFake(A[pL]) := TFake(A[pR]);
        TFake(A[pR]) := v;
      until False;
      if pL = pR then
        begin
          if pL > L then
            Dec(pR)
          else
            if pL < R then
              Inc(pL);
        end;
      if pR < N then L := pL;
      if pL > N then R := pR;
    end;
  if (L < R) and (A[L] > A[R]) then
    begin
      v := TFake(A[L]);
      TFake(A[L]) := TFake(A[R]);
      TFake(A[R]) := v;
    end;
  Result := A[N];
end;

class function TGComparableArrayHelper.SequentSearch(constref A: array of T; constref aValue: T): SizeInt;
begin
  for Result := 0 to System.High(A) do
    if aValue = A[Result] then
      exit;
  Result := -1;
end;

class function TGComparableArrayHelper.BinarySearch(constref A: array of T; constref aValue: T): SizeInt;
begin
  Result := System.High(A);
  if Result >= 0 then
    Result := DoBinSearch(@A[0], Result, aValue);
end;

class function TGComparableArrayHelper.BinarySearchPos(constref A: array of T; constref aValue: T): TSearchResult;
var
  hi: SizeInt;
begin
  hi := High(A);
  if hi >= 0 then
    Result := DoBinSearchPos(@A[0], hi, aValue)
  else
    begin
      Result.FoundIndex := hi;
      Result.InsertIndex := hi;
    end;
end;

class function TGComparableArrayHelper.IndexOfMin(constref A: array of T): SizeInt;
var
  R, I: SizeInt;
  v: T;
begin
  R := System.High(A);
  if R >= 0 then
    begin
      Result := 0;
      v := A[0];
      for I := 1 to R do
        if v > A[I] then
          begin
            v := A[I];
            Result := I;
          end;
    end
  else
    Result := R;
end;

class function TGComparableArrayHelper.IndexOfMax(constref A: array of T): SizeInt;
var
  R, I: SizeInt;
  v: T;
begin
  R := System.High(A);
  if R >= 0 then
    begin
      Result := 0;
      v := A[0];
      for I := 1 to R do
        if v < A[I] then
          begin
            v := A[I];
            Result := I;
          end;
    end
  else
    Result := R;
end;

class function TGComparableArrayHelper.GetMin(constref A: array of T): TOptional;
var
  v: T;
begin
  if FindMin(A, v) then
    Result.Assign(v);
end;

class function TGComparableArrayHelper.GetMax(constref A: array of T): TOptional;
var
  v: T;
begin
  if FindMax(A, v) then
    Result.Assign(v);
end;

class function TGComparableArrayHelper.FindMin(constref A: array of T; out aValue: T): Boolean;
var
  R, I: SizeInt;
begin
  R := System.High(A);
  Result := R >= 0;
  if Result then
    begin
      aValue := A[0];
      for I := 1 to R do
        if aValue > A[I] then
          aValue := A[I];
    end;
end;

class function TGComparableArrayHelper.FindMax(constref A: array of T; out aValue: T): Boolean;
var
  R, I: SizeInt;
begin
  R := System.High(A);
  Result := R >= 0;
  if Result then
    begin
      aValue := A[0];
      for I := 1 to R do
        if aValue < A[I] then
          aValue := A[I];
    end;
end;

class function TGComparableArrayHelper.FindMinMax(constref A: array of T; out aMin, aMax: T): Boolean;
var
  R, I: SizeInt;
begin
  R := System.High(A);
  Result := R >= 0;
  if Result then
    begin
      aMin := A[0];
      aMax := A[0];
      for I := 1 to R do
        if A[I] > aMax then
          aMax := A[I]
        else
          if A[I] < aMin then
            aMin := A[I];
    end;
end;

class function TGComparableArrayHelper.FindNthSmallest(var A: array of T; N: SizeInt; out aValue: T): Boolean;
var
  R: SizeInt;
begin
  R := System.High(A);
  if R < 0 then
    exit(False);
  if N <= 0 then
    exit(FindMin(A, aValue));
  if N >= R then
    exit(FindMax(A, aValue));
  aValue := QSelectR(@A[0], R, N);
  Result := True;
end;

class function TGComparableArrayHelper.NthSmallest(var A: array of T; N: SizeInt): TOptional;
var
  v: T;
begin
  if FindNthSmallest(A, N, v) then
    Result.Assign(v);
end;

class function TGComparableArrayHelper.FindNthSmallestND(constref A: array of T; N: SizeInt;
  out aValue: T): Boolean;
begin
  Result := FindNthSmallest(CreateCopy(A), N, aValue);
end;

class function TGComparableArrayHelper.NthSmallestND(constref A: array of T; N: SizeInt): TOptional;
var
  v: T;
begin
  if FindNthSmallestND(A, N, v) then
    Result.Assign(v);
end;

class function TGComparableArrayHelper.NextPermutation2Asc(var A: array of T): Boolean;
var
  I, J, R: SizeInt;
  v: TFake;
begin
  R := System.High(A);
  J := -1;
  for I := Pred(R) downto 0 do
    if A[I] > A[Succ(I)] then
      begin
        J := I;
        break;
      end;
  if J < 0 then exit(False);
  for I := R downto 0 do
    if A[J] > A[I] then
      begin
        v := TFake(A[I]);
        TFake(A[I]) := TFake(A[J]);
        TFake(A[J]) := v;
        break;
      end;
  DoReverse(@A[Succ(J)], R - Succ(J));
  Result := True;
end;

class function TGComparableArrayHelper.NextPermutation2Desc(var A: array of T): Boolean;
var
  I, J, R: SizeInt;
  v: TFake;
begin
  R := System.High(A);
  J := -1;
  for I := Pred(R) downto 0 do
    if A[I] < A[Succ(I)] then
      begin
        J := I;
        break;
      end;
  if J < 0 then exit(False);
  for I := R downto 0 do
    if A[J] < A[I] then
      begin
        v := TFake(A[I]);
        TFake(A[I]) := TFake(A[J]);
        TFake(A[J]) := v;
        break;
      end;
  DoReverse(@A[Succ(J)], R - Succ(J));
  Result := True;
end;

class function TGComparableArrayHelper.IsNonDescending(constref A: array of T): Boolean;
var
  I: SizeInt;
begin
  for I := 0 to Pred(System.High(A)) do
    if A[I] > A[Succ(I)] then
      exit(False);
  Result := True;
end;

class function TGComparableArrayHelper.IsStrictAscending(constref A: array of T): Boolean;
var
  I, R: SizeInt;
begin
  R := System.High(A);
  if R > 0 then
    begin
      for I := 1 to R do
        if A[Pred(I)] >= A[I] then
          exit(False);
      Result := True;
    end
  else
    Result := False;
end;

class function TGComparableArrayHelper.IsNonAscending(constref A: array of T): Boolean;
var
  I: SizeInt;
begin
  for I := 0 to Pred(System.High(A)) do
    if A[I] < A[Succ(I)] then
      exit(False);
  Result := True;
end;

class function TGComparableArrayHelper.IsStrictDescending(constref A: array of T): Boolean;
var
  I, R: SizeInt;
begin
  R := System.High(A);
  if R > 0 then
    begin
      for I := 1 to R do
        if A[Pred(I)] <= A[I] then
          exit(False);
      Result := True;
    end
  else
    Result := False;
end;

class function TGComparableArrayHelper.Same(constref A, B: array of T): Boolean;
var
  R, I: SizeInt;
begin
  R := System.High(A);
  if System.High(B) <> R then
    exit(False);
  for I := 0 to R do
    if A[I] <> B[I] then
      exit(False);
  Result := True;
end;

class procedure TGComparableArrayHelper.QuickSort(var A: array of T; o: TSortOrder);
var
  R: SizeInt;
begin
  R := System.High(A);
  if R > 0 then
    if CountRun2Asc(@A[0], R) < R then
      begin
        DoQSort(@A[0], R);
        if o = soDesc then
          Reverse(A);
      end
    else
      if (o = soDesc) and (A[0] <> A[R]) then
        Reverse(A);
end;

class procedure TGComparableArrayHelper.IntroSort(var A: array of T; o: TSortOrder);
var
  R: SizeInt;
begin
  R := System.High(A);
  if R > 0 then
    if CountRun2Asc(@A[0], R) < R then
      begin
        DoIntroSort(@A[0], R, Pred(LGUtils.NSB(R + 1)) * INTRO_LOG_FACTOR);
        if o = soDesc then
          Reverse(A);
      end
    else
      if (o = soDesc) and (A[0] <> A[R]) then
        Reverse(A);
end;

class procedure TGComparableArrayHelper.DualPivotQuickSort(var A: array of T; o: TSortOrder);
var
  R: SizeInt;
begin
  R := System.High(A);
  if R > 0 then
    if CountRun2Asc(@A[0], R) < R then
      begin
        DoDPQSort(@A[0], R);
        if o = soDesc then
          Reverse(A);
      end
    else
      if (o = soDesc) and (A[0] <> A[R]) then
        Reverse(A);
end;

class procedure TGComparableArrayHelper.MergeSort(var A: array of T; o: TSortOrder);
var
  R: SizeInt;
begin
  R := High(A);
  if R > 0 then
    if o = soAsc then
      TMergeSort.SortAsc(@A[0], R)
    else
      TMergeSort.SortDesc(@A[0], R);
end;

class procedure TGComparableArrayHelper.Sort(var A: array of T; o: TSortOrder);
begin
  IntroSort(A, o);
end;

class function TGComparableArrayHelper.Sorted(constref A: array of T; o: TSortOrder): TArray;
begin
  Result := CreateCopy(A);
  Sort(Result, o);
end;

class function TGComparableArrayHelper.SelectDistinct(constref A: array of T): TArray;
var
  I, J, Hi: SizeInt;
begin
  Result := Sorted(A);
  Hi := System.High(Result);
  if Hi < 1 then
    exit;
  I := 0;
  for J := 1 to Hi do
    begin
      if Result[I] = Result[J] then
        continue;
      Inc(I);
      if J > I then
        Result[I] := Result[J];
    end;
  System.SetLength(Result, Succ(I));
end;

{ TGRegularArrayHelper.TMergeSort }

procedure TGRegularArrayHelper.TMergeSort.Init(A: PItem; c: TCompare);
begin
  inherited Init(A);
  FCompare := c;
end;

procedure TGRegularArrayHelper.TMergeSort.CollapseA;
var
  I: SizeInt;
begin
  while FStackSize > 1 do
    begin
      I := FStackSize - 2;
      if (I > 0) and (FStack[I - 1].Count <= FStack[I].Count + FStack[I + 1].Count) then
        begin
          if FStack[I - 1].Count < FStack[I + 1].Count then
            Dec(I);
        end
      else
        if FStack[I].Count > FStack[I + 1].Count then
          break;
      MergeAtA(I);
    end;
end;

procedure TGRegularArrayHelper.TMergeSort.CollapseD;
var
  I: SizeInt;
begin
  while FStackSize > 1 do
    begin
      I := FStackSize - 2;
      if (I > 0) and (FStack[I - 1].Count <= FStack[I].Count + FStack[I + 1].Count) then
        begin
          if FStack[I - 1].Count < FStack[I + 1].Count then
            Dec(I);
        end
      else
        if FStack[I].Count > FStack[I + 1].Count then
          break;
      MergeAtD(I);
    end;
end;

procedure TGRegularArrayHelper.TMergeSort.ForceCollapseA;
var
  I: SizeInt;
begin
  while FStackSize > 1 do
    begin
      I := FStackSize - 2;
      if (I > 0) and (FStack[I - 1].Count < FStack[I + 1].Count) then
        MergeAtA(I - 1)
      else
        MergeAtA(I);
    end;
end;

procedure TGRegularArrayHelper.TMergeSort.ForceCollapseD;
var
  I: SizeInt;
begin
  while FStackSize > 1 do
    begin
      I := FStackSize - 2;
      if (I > 0) and (FStack[I - 1].Count < FStack[I + 1].Count) then
        MergeAtD(I - 1)
      else
        MergeAtD(I);
    end;
end;

procedure TGRegularArrayHelper.TMergeSort.MergeAtA(aIndex: SizeInt);
var
  Base, CountLo, CountHi: SizeInt;
begin
  Base := FStack[aIndex].Base;
  CountLo := FStack[aIndex].Count;
  CountHi := FStack[aIndex + 1].Count;
  FStack[aIndex].Count := CountLo + CountHi;
  if aIndex = FStackSize - 3 then
    begin
      FStack[aIndex + 1].Base := FStack[aIndex + 2].Base;
      FStack[aIndex + 1].Count := FStack[aIndex + 2].Count;
    end;
  Dec(FStackSize);
  if CountLo <= CountHi then
    MergeLoA(Base, CountLo, CountHi)
  else
    MergeHiA(Base, CountLo, CountHi);
end;

procedure TGRegularArrayHelper.TMergeSort.MergeAtD(aIndex: SizeInt);
var
  Base, CountLo, CountHi: SizeInt;
begin
  Base := FStack[aIndex].Base;
  CountLo := FStack[aIndex].Count;
  CountHi := FStack[aIndex + 1].Count;
  FStack[aIndex].Count := CountLo + CountHi;
  if aIndex = FStackSize - 3 then
    begin
      FStack[aIndex + 1].Base := FStack[aIndex + 2].Base;
      FStack[aIndex + 1].Count := FStack[aIndex + 2].Count;
    end;
  Dec(FStackSize);
  if CountLo <= CountHi then
    MergeLoD(Base, CountLo, CountHi)
  else
    MergeHiD(Base, CountLo, CountHi);
end;

procedure TGRegularArrayHelper.TMergeSort.MergeLoA(From, CountLo, CountHi: SizeInt);
var
  pLo,           // position in low part   (in buffer)
  pHi,           // position in high part  (in data array)
  pDst: SizeInt; // current CreateMerge position (in data array)
  LocA: PItem;   // local pointer to data array
  LocB: PItem;   // local pointer to buffer
  c: TCompare;
begin
  LocA := FData;
  c := FCompare;
  if c(FData[Pred(From + CountLo)], FData[From + CountLo]) > 0 then
    begin
      LocB := EnsureBufferCapacity(CountLo);
      {$IFNDEF FPC_REQUIRES_PROPER_ALIGNMENT}
      System.Move(LocA[From], LocB[0], CountLo * SizeOf(T));
      {$ELSE}
      CopyItems(@LocA[From], LocB, CountLo);
      {$ENDIF}
      if c(LocA[Pred(From + CountLo + CountHi)], LocA[From]) >= 0 then
        begin
          pLo := 0;
          pHi := From + CountLo;
          pDst := From;
          CountHi := Pred(From + CountLo + CountHi);
          repeat
            if c(LocB[pLo], LocA[pHi]) <= 0 then
              begin
                TFake(LocA[pDst]) := TFake(LocB[pLo]);
                Inc(pLo);
              end
            else
              begin
                TFake(LocA[pDst]) := TFake(LocA[pHi]);
                Inc(pHi);
              end;
            Inc(pDst);
          until (pLo >= CountLo) or (pHi > CountHi);
          if pLo < CountLo then
            {$IFNDEF FPC_REQUIRES_PROPER_ALIGNMENT}
            System.Move(LocB[pLo], LocA[pDst], (CountLo - pLo) * SizeOf(T));
            {$ELSE}
            CopyItems(@LocB[pLo], @LocA[pDst], CountLo - pLo);
            {$ENDIF}
        end
      else
        begin
          {$IFNDEF FPC_REQUIRES_PROPER_ALIGNMENT}
          System.Move(LocA[From + CountLo], LocA[From], CountHi * SizeOf(T));
          System.Move(LocB[0], LocA[From + CountHi], CountLo * SizeOf(T));
          {$ELSE}
          CopyItems(@LocA[From + CountLo], @LocA[From], CountHi);
          CopyItems(LocB, @LocA[From + CountHi], CountLo);
          {$ENDIF}
        end;
    end;
end;

procedure TGRegularArrayHelper.TMergeSort.MergeLoD(From, CountLo, CountHi: SizeInt);
var
  pLo,           // position in low part   (in buffer)
  pHi,           // position in high part  (in data array)
  pDst: SizeInt; // current CreateMerge position (in data array)
  LocA: PItem;   // local pointer to data array
  LocB: PItem;   // local pointer to buffer
  c: TCompare;
begin
  LocA := FData;
  c := FCompare;
  if c(FData[Pred(From + CountLo)], FData[From + CountLo]) < 0 then
    begin
      LocB := EnsureBufferCapacity(CountLo);
      {$IFNDEF FPC_REQUIRES_PROPER_ALIGNMENT}
      System.Move(LocA[From], LocB[0], CountLo * SizeOf(T));
      {$ELSE}
      CopyItems(@LocA[From], LocB, CountLo);
      {$ENDIF}
      if c(LocA[Pred(From + CountLo + CountHi)], LocA[From]) <= 0 then
        begin
          pLo := 0;
          pHi := From + CountLo;
          pDst := From;
          CountHi := Pred(From + CountLo + CountHi);
          repeat
            if c(LocB[pLo], LocA[pHi]) >= 0 then
              begin
                TFake(LocA[pDst]) := TFake(LocB[pLo]);
                Inc(pLo);
              end
            else
              begin
                TFake(LocA[pDst]) := TFake(LocA[pHi]);
                Inc(pHi);
              end;
            Inc(pDst);
          until (pLo >= CountLo) or (pHi > CountHi);
          if pLo < CountLo then
            {$IFNDEF FPC_REQUIRES_PROPER_ALIGNMENT}
            System.Move(LocB[pLo], LocA[pDst], (CountLo - pLo) * SizeOf(T));
            {$ELSE}
            CopyItems(@LocB[pLo], @LocA[pDst], CountLo - pLo);
            {$ENDIF}
        end
      else
        begin
          {$IFNDEF FPC_REQUIRES_PROPER_ALIGNMENT}
          System.Move(LocA[From + CountLo], LocA[From], CountHi * SizeOf(T));
          System.Move(LocB[0], LocA[From + CountHi], CountLo * SizeOf(T));
          {$ELSE}
          CopyItems(@LocA[From + CountLo], @LocA[From], CountHi);
          CopyItems(LocB, @LocA[From + CountHi], CountLo);
          {$ENDIF}
        end;
    end;
end;

procedure TGRegularArrayHelper.TMergeSort.MergeHiA(From, CountLo, CountHi: SizeInt);
var
  pLo,           // position in low part   (in data array)
  pHi,           // position in high part  (in buffer)
  pDst: SizeInt; // current CreateMerge position (in data array)
  LocA: PItem;   // local pointer to data array
  LocB: PItem;   // local pointer to buffer
  c: TCompare;
begin
  LocA := FData;
  c := FCompare;
  if c(FData[Pred(From + CountLo)], FData[From + CountLo]) > 0 then
    begin
      LocB := EnsureBufferCapacity(CountHi);
      {$IFNDEF FPC_REQUIRES_PROPER_ALIGNMENT}
      System.Move(LocA[From + CountLo], LocB[0], CountHi * SizeOf(T));
      {$ELSE}
      CopyItems(@LocA[From + CountLo], LocB, CountHi);
      {$ENDIF}
      if c(LocA[Pred(From + CountLo + CountHi)], LocA[From]) >= 0 then
        begin
          pLo := Pred(From + CountLo);
          pHi := CountHi - 1;
          pDst := Pred(From + CountLo + CountHi);
          repeat
            if c(LocA[pLo], LocB[pHi]) > 0 then
              begin
                TFake(LocA[pDst]) := TFake(LocA[pLo]);
                Dec(pLo);
              end
            else
              begin
                TFake(LocA[pDst]) := TFake(LocB[pHi]);
                Dec(pHi);
              end;
            Dec(pDst);
          until (pLo < From) or (pHi < 0);
          if pHi >= 0 then
            {$IFNDEF FPC_REQUIRES_PROPER_ALIGNMENT}
            System.Move(LocB[0], LocA[From], Succ(pHi) * SizeOf(T));
            {$ELSE}
            CopyItems(LocB, @LocA[From], Succ(pHi));
            {$ENDIF}
        end
      else
        begin
          {$IFNDEF FPC_REQUIRES_PROPER_ALIGNMENT}
          System.Move(LocA[From], LocA[From + CountHi], CountLo * SizeOf(T));
          System.Move(LocB[0], LocA[From], CountHi * SizeOf(T));
          {$ELSE}
          CopyItems(@LocA[From], @LocA[From + CountHi], CountLo);
          CopyItems(LocB, @LocA[From], CountHi);
          {$ENDIF}
        end;
    end;
end;

procedure TGRegularArrayHelper.TMergeSort.MergeHiD(From, CountLo, CountHi: SizeInt);
var
  pLo,           // position in low part   (in data array)
  pHi,           // position in high part  (in buffer)
  pDst: SizeInt; // current CreateMerge position (in data array)
  LocA: PItem;   // local pointer to data array
  LocB: PItem;   // local pointer to buffer
  c: TCompare;
begin
  LocA := FData;
  c := FCompare;
  if c(FData[Pred(From + CountLo)], FData[From + CountLo]) < 0 then
    begin
      LocB := EnsureBufferCapacity(CountHi);
      {$IFNDEF FPC_REQUIRES_PROPER_ALIGNMENT}
      System.Move(LocA[From + CountLo], LocB[0], CountHi * SizeOf(T));
      {$ELSE}
      CopyItems(@LocA[From + CountLo], LocB, CountHi);
      {$ENDIF}
      if c(LocA[Pred(From + CountLo + CountHi)], LocA[From]) <= 0 then
        begin
          pLo := Pred(From + CountLo);
          pHi := CountHi - 1;
          pDst := Pred(From + CountLo + CountHi);
          repeat
            if c(LocA[pLo], LocB[pHi]) < 0 then
              begin
                TFake(LocA[pDst]) := TFake(LocA[pLo]);
                Dec(pLo);
              end
            else
              begin
                TFake(LocA[pDst]) := TFake(LocB[pHi]);
                Dec(pHi);
              end;
            Dec(pDst);
          until (pLo < From) or (pHi < 0);
          if pHi >= 0 then
            {$IFNDEF FPC_REQUIRES_PROPER_ALIGNMENT}
            System.Move(LocB[0], LocA[From], Succ(pHi) * SizeOf(T));
            {$ELSE}
            CopyItems(LocB, @LocA[From], Succ(pHi));
            {$ENDIF}
        end
      else
        begin
          {$IFNDEF FPC_REQUIRES_PROPER_ALIGNMENT}
          System.Move(LocA[From], LocA[From + CountHi], CountLo * SizeOf(T));
          System.Move(LocB[0], LocA[From], CountHi * SizeOf(T));
          {$ELSE}
          CopyItems(@LocA[From], @LocA[From + CountHi], CountLo);
          CopyItems(LocB, @LocA[From], CountHi);
          {$ENDIF}
        end;
    end;
end;

class procedure TGRegularArrayHelper.TMergeSort.InsertSortA(A: PItem; R, At: SizeInt; c: TCompare);
var
  I, J: SizeInt;
  v: TFake;
begin
  if At < 1 then
    At := 1;
  for I := At to R do
    begin
      v := TFake(A[I]);
      J := I - 1;
      while (J >= 0) and (c(A[J], T(v)) > 0) do
        begin
          TFake(A[J + 1]) := TFake(A[J]);
          Dec(J);
        end;
      TFake(A[J + 1]) := v;
    end;
end;

class procedure TGRegularArrayHelper.TMergeSort.InsertSortD(A: PItem; R, At: SizeInt; c: TCompare);
var
  I, J: SizeInt;
  v: TFake;
begin
  if At < 1 then
    At := 1;
  for I := At to R do
    begin
      v := TFake(A[I]);
      J := I - 1;
      while (J >= 0) and (c(A[J], T(v)) < 0) do
        begin
          TFake(A[J + 1]) := TFake(A[J]);
          Dec(J);
        end;
      TFake(A[J + 1]) := v;
    end;
end;

class function TGRegularArrayHelper.TMergeSort.CountRunAsc(A: PItem; R: SizeInt; c: TCompare): SizeInt;
begin
  if R > 0 then
    begin
      Result := 1;
      if c(A[0], A[1]) <= 0 then  // ascending
        while (Result < R) and (c(A[Result], A[Succ(Result)]) <= 0) do
          Inc(Result)
      else                        // descending
        begin
          while (Result < R) and (c(A[Result], A[Succ(Result)]) > 0) do
            Inc(Result);
          DoReverse(A, Result);
        end;
    end
  else
    Result := 0;
end;

class function TGRegularArrayHelper.TMergeSort.CountRunDesc(A: PItem; R: SizeInt; c: TCompare): SizeInt;
begin
  if R > 0 then
    begin
      Result := 1;
      if c(A[0], A[1]) >= 0 then  // descending
        while (Result < R) and (c(A[Result], A[Succ(Result)]) >= 0) do
          Inc(Result)
      else                        // ascending
        begin
          while (Result < R) and (c(A[Result], A[Succ(Result)]) < 0) do
            Inc(Result);
          DoReverse(A, Result);
        end;
    end
  else
    Result := 0;
end;

class procedure TGRegularArrayHelper.TMergeSort.SortAsc(A: PItem; R: SizeInt; c: TCompare);
var
  RunLen, MinLen, Len, L: SizeInt;
  ms: TMergeSort;
begin
  if R + 1 > MIN_MERGE_LEN shl 1 then
    begin
      ms.Init(A, c);
      MinLen := MinRunLen(R + 1);
      L := 0;
      repeat
        RunLen := Succ(CountRunAsc(@A[L], R - L, c));
        if RunLen < MinLen then
          begin
            Len := Math.Min(Succ(R - L), MinLen);
            InsertSortA(@A[L], Pred(Len), RunLen, c);
            RunLen := Len;
          end;
        ms.PushRun(L, RunLen);
        L += RunLen;
        ms.CollapseA;
      until L > R;
      ms.ForceCollapseA;
    end
  else
    InsertSortA(A, R, Succ(CountRunAsc(A, R, c)), c);
end;

class procedure TGRegularArrayHelper.TMergeSort.SortDesc(A: PItem; R: SizeInt; c: TCompare);
var
  RunLen, MinLen, Len, L: SizeInt;
  ms: TMergeSort;
begin
  if R + 1 > MIN_MERGE_LEN shl 1 then
    begin
      ms.Init(A, c);
      MinLen := MinRunLen(R + 1);
      L := 0;
      repeat
        RunLen := Succ(CountRunDesc(@A[L], R - L, c));
        if RunLen < MinLen then
          begin
            Len := Math.Min(Succ(R - L), MinLen);
            InsertSortD(@A[L], Pred(Len), RunLen, c);
            RunLen := Len;
          end;
        ms.PushRun(L, RunLen);
        L += RunLen;
        ms.CollapseD;
      until L > R;
      ms.ForceCollapseD;
    end
  else
    InsertSortD(A, R, Succ(CountRunDesc(A, R, c)), c);
end;

{ TGRegularArrayHelper }

class function TGRegularArrayHelper.CountRun2Asc(A: PItem; R: SizeInt; c: TCompare): SizeInt;
begin
  Result := 0;
  while (Result < R) and (c(A[Result], A[Succ(Result)]) = 0) do
    Inc(Result);
  if Result < R then
    begin
      Inc(Result);
      if c(A[Pred(Result)], A[Result]) < 0 then   // ascending
        while (Result < R) and (c(A[Result], A[Succ(Result)]) <= 0) do
          Inc(Result)
      else                                        // descending
        begin
          while (Result < R) and (c(A[Result], A[Succ(Result)]) >= 0) do
            Inc(Result);
          DoReverse(A, Result);
        end;
    end;
end;

class procedure TGRegularArrayHelper.InsertionSort(A: PItem; R: SizeInt; c: TCompare);
var
  I, J: SizeInt;
  v: TFake;
begin
  for I := 1 to R do
    begin
      v := TFake(A[I]);
      J := I - 1;
      while (J >= 0) and (c(A[J], T(v)) > 0) do
        begin
          TFake(A[J + 1]) := TFake(A[J]);
          Dec(J);
        end;
      TFake(A[J + 1]) := v;
    end;
end;

class function TGRegularArrayHelper.DoBinSearch(A: PItem; R: SizeInt; constref aValue: T; c: TCompare): SizeInt;
var
  L, M, Cmp: SizeInt;
begin
  //here R must be >= 0;
  Result := -1;
  L := 0;
  Cmp := c(A[R], A[0]);
  if Cmp > 0 then  //ascending
    begin
      while L < R do
        begin
          M := L + (R - L) shr 1;
          Cmp := c(A[M], aValue);
          if Cmp < 0 then
            L := Succ(M)
          else
            begin
              if Cmp = 0 then
                exit(M);
              R := M;
            end;
        end;
      //here L >= R
      if c(A[R], aValue) = 0 then
        Result := R;
    end
  else
    if Cmp < 0 then  //descending
      begin
        while L < R do
          begin
            M := L + ((R - L) shr 1);
            Cmp := c(A[M], aValue);
            if Cmp > 0 then
              L := Succ(M)
            else
              begin
                if Cmp = 0 then
                  exit(M);
                R := M;
              end;
          end;
        //here L >= R
        if c(A[R], aValue) = 0 then
          Result := R;
      end
    else              //constant
      if c(A[L], aValue) = 0 then
        exit(L);
end;

class function TGRegularArrayHelper.DoBinSearchPosA(A: PItem; R: SizeInt; constref aValue: T;
  c: TCompare): TSearchResult;
var
  L, M, Cmp: SizeInt;
begin
  //here exists at least 2 ascending elements;
  Result.FoundIndex := -1;
  L := 0;
  while L < R do
    begin
      M := L + (R - L) shr 1;
      Cmp := c(A[M], aValue);
      if Cmp < 0 then
        L := Succ(M)
      else
        begin
          if Cmp = 0 then
            begin
              Result.FoundIndex := M;
              Result.InsertIndex := Succ(M);
              exit;
            end;
          R := M;
        end;
    end;
  //here L >= R
  Cmp := c(aValue, A[R]);
  if Cmp < 0 then
    Result.InsertIndex := R
  else
    begin
      Result.InsertIndex := Succ(R);
      if Cmp = 0 then
        Result.FoundIndex := R;
    end;
end;

class function TGRegularArrayHelper.DoBinSearchPosD(A: PItem; R: SizeInt; constref aValue: T;
  c: TCompare): TSearchResult;
var
  L, M, Cmp: SizeInt;
begin
  //here exists at least 2 descending elements
  Result.FoundIndex := -1;
  L := 0;
  while L < R do
    begin
      M := L + (R - L) shr 1;
      Cmp := c(A[M], aValue);
      if Cmp > 0 then
        L := Succ(M)
      else
        begin
          if Cmp = 0 then
            begin
              Result.FoundIndex := M;
              Result.InsertIndex := Succ(M);
              exit;
            end;
          R := M;
        end;
    end;
  //here L >= R
  Cmp := c(aValue, A[R]);
  if Cmp > 0 then
    Result.InsertIndex := R
  else
    begin
      Result.InsertIndex := Succ(R);
      if Cmp = 0 then
        Result.FoundIndex := R;
    end;
end;

class function TGRegularArrayHelper.DoBinSearchPos(A: PItem; R: SizeInt; constref aValue: T;
  c: TCompare): TSearchResult;
var
  Cmp: SizeInt;
begin
  //here R must be >= 0;
  Cmp := c(A[R], A[0]);
  if Cmp > 0 then   //ascending
    Result := DoBinSearchPosA(A, R, aValue, c)
  else
    if Cmp < 0 then //descending
      Result := DoBinSearchPosD(A, R, aValue, c)
    else            //constant
      begin
        Cmp := c(aValue, A[R]);
        Result.FoundIndex := -1;
        if Cmp < 0 then
           Result.InsertIndex := 0
        else
          begin
            Result.InsertIndex := Succ(R);
            if Cmp = 0 then
              Result.FoundIndex := 0;
          end;
      end;
end;

class procedure TGRegularArrayHelper.DoHeapSort(A: PItem; R: SizeInt; c: TCompare);
var
  I, Curr, Next: SizeInt;
  v: TFake;
begin
  if R > HEAP_INSERT_CUTOFF then
    begin
      for I := Pred(Succ(R) shr 1) downto 0 do
        begin
          Curr := I;
          Next := Succ(I shl 1);
          v := TFake(A[Curr]);
          while Next <= R do
            begin
              if(Succ(Next) <= R) and (c(A[Next], A[Succ(Next)]) < 0)then
                Inc(Next);
              if c(T(v), A[Next]) >= 0 then
                break;
              TFake(A[Curr]) := TFake(A[Next]);
              Curr := Next;
              Next := Succ(Next shl 1);
            end;
          TFake(A[Curr]) := v;
        end;
      for I := R downto 1 do
        begin
          Curr := 0;
          Next := 1;
          v := TFake(A[I]);
          TFake(A[I]) := TFake(A[0]);
          while Next < I do
            begin
              if(Succ(Next) < I) and (c(A[Next], A[Succ(Next)]) < 0) then
                Inc(Next);
              TFake(A[Curr]) := TFake(A[Next]);
              Curr := Next;
              Next := Succ(Next shl 1);
            end;
          Next := Pred(Curr) shr 1;
          while (Curr > 0) and (c(T(v), A[Next]) > 0) do
            begin
              TFake(A[Curr]) := TFake(A[Next]);
              Curr := Next;
              Next := Pred(Next) shr 1;
            end;
          TFake(A[Curr]) := v;
        end;
    end
  else
    InsertionSort(A, R, c);
end;

class function TGRegularArrayHelper.QSplitR(A: PItem; R: SizeInt; c: TCompare): TSortSplit;
var
  Pivot: T;
  v: TFake;
  pL, pR: SizeInt;
begin
  Pivot := A[Succ(Random(Pred(R)))]; //shouldn't be first or last
  pL := -1;
  pR := Succ(R);
  repeat
    repeat Inc(pL) until c(A[pL], Pivot) >= 0;
    repeat Dec(pR) until c(A[pR], Pivot) <= 0;
    if pL > pR then break;
    v := TFake(A[pL]);
    TFake(A[pL]) := TFake(A[pR]);
    TFake(A[pR]) := v;
  until False;
  Result.Left := pR;
  Result.Right := pL;
end;

class procedure TGRegularArrayHelper.DoQSort(A: PItem; R: SizeInt; c: TCompare);
begin
  while R > QUICK_INSERT_CUTOFF do
    with QSplitR(A, R, c) do
      if Left <= R - Right then
        begin
          DoQSort(A, Left, c);
          A := @A[Right];
          R -= Right;
        end
      else
        begin
          DoQSort(@A[Right], R - Right, c);
          R := Left;
        end;
  if R > 0 then
    InsertionSort(A, R, c);
end;

class function TGRegularArrayHelper.MedianOf3(constref v1, v2, v3: T; c: TCompare): T;
begin
  Result := v2;
  if c(v1, Result) < 0 then
    begin
      if c(v3, Result) < 0 then
        begin
          if c(v1, v3) < 0 then
            Result := v3
          else
            Result := v1;
        end;
    end
  else { v1 >= Result }
    begin
      if c(v3, Result) > 0 then
        begin
          if c(v1, v3) > 0 then
            Result := v3
          else
            Result := v1;
        end;
    end;
end;

class function TGRegularArrayHelper.QSplitMo9(A: PItem; R: SizeInt; c: TCompare): TSortSplit;
var
  Pivot: T;
  v: TFake;
  pL, pR: SizeInt;
begin
  if R > MEDIAN_OF9_CUTOFF then
    Pivot := MedianOf3(
      MedianOf3(A[0],
                A[Succ(R) shr 3],
                A[Succ(R) shr 2], c),
      MedianOf3(A[Succ(R) shr 1 - Succ(R) shr 3],
                A[Succ(R) shr 1],
                A[Succ(R) shr 1 + Succ(R) shr 3], c),
      MedianOf3(A[R - Succ(R) shr 2],
                A[R - Succ(R) shr 3],
                A[R], c), c)
  else
    Pivot := MedianOf3(A[0], A[Succ(R) shr 1], A[R], c);
  pL := -1;
  pR := Succ(R);
  repeat
    repeat Inc(pL) until c(A[pL], Pivot) >= 0;
    repeat Dec(pR) until c(A[pR], Pivot) <= 0;
    if pL > pR then break;
    v := TFake(A[pL]);
    TFake(A[pL]) := TFake(A[pR]);
    TFake(A[pR]) := v;
  until False;
  Result.Left := pR;
  Result.Right := pL;
end;

class procedure TGRegularArrayHelper.DoIntroSort(A: PItem; R, Ttl: SizeInt; c: TCompare);
begin
  if R > QUICK_INSERT_CUTOFF then
    if Ttl > 0 then
      with QSplitMo9(A, R, c) do
        begin
          DoIntroSort(A, Left, Pred(Ttl), c);
          DoIntroSort(@A[Right], R - Right, Pred(Ttl), c);
        end
    else
      DoHeapSort(A, R, c)
  else
    if R > 0 then
      InsertionSort(A, R, c);
end;

class function TGRegularArrayHelper.DPQSplit(A: PItem; R: SizeInt; c: TCompare): TSortSplit;
var
  v, Pivot1, Pivot2: TFake;
  pL, pR, I: SizeInt;
begin
  pL := Succ(Random(Pred(R shr 1)));
  pR := Pred(R - Random(Pred(R shr 1)));

  if c(A[pL], A[pR]) <= 0 then
    begin
      Pivot1 := TFake(A[pL]);
      TFake(A[pL]) := TFake(A[0]);
      Pivot2 := TFake(A[pR]);
      TFake(A[pR]) := TFake(A[R]);
    end
  else
    begin
      Pivot2 := TFake(A[pL]);
      TFake(A[pL]) := TFake(A[R]);
      Pivot1 := TFake(A[pR]);
      TFake(A[pR]) := TFake(A[0]);
    end;

  pL := 1;
  I  := 1;
  pR := Pred(R);
  while I <= pR do
    begin
      v := TFake(A[I]);
      if c(T(v), T(Pivot1)) < 0 then
        begin
          TFake(A[I]) := TFake(A[pL]);
          TFake(A[pL]) := v;
          Inc(pL);
        end
      else
        if c(T(v), T(Pivot2)) > 0 then
          begin
            while (pR >= I) and (c(A[pR], T(Pivot2)) > 0) do
              Dec(pR);
            if pR < I then
              break;
            if c(A[pR], T(Pivot1)) < 0 then
              begin
                TFake(A[I]) := TFake(A[pL]);
                TFake(A[pL]) := TFake(A[pR]);
                Inc(pL);
              end
            else
              TFake(A[I]) := TFake(A[pR]);
            TFake(A[pR]) := v;
            Dec(pR);
          end;
      Inc(I);
    end;
  TFake(A[0]) := TFake(A[pL - 1]);
  TFake(A[pL - 1]) := Pivot1;
  TFake(A[R]) := TFake(A[pR + 1]);
  TFake(A[pR + 1]) := Pivot2;

  Result.Left := pL - 1;
  Result.Right := pR + 1;
end;

class procedure TGRegularArrayHelper.DoDPQSort(A: PItem; R: SizeInt; c: TCompare);
begin
  if R > DPQ_INSERT_CUTOFF then
    with DPQSplit(A, R, c) do
      begin
        DoDPQSort(A, Left - 1, c);
        DoDPQSort(@A[Right + 1], R - Right - 1, c);
        if c(A[Left], A[Right]) <> 0 then
          DoDPQSort(@A[Left + 1], Right - Left - 2, c);
      end
  else
    if R > 0 then
      InsertionSort(A, R, c);
end;

class function TGRegularArrayHelper.QSelectR(A: PItem; R, N: SizeInt; c: TCompare): T;
var
  v: TFake;
  Pivot: T;
  L, pL, pR: SizeInt;
begin
  L := 0;
  while L < Pred(R) do
    begin
      Pivot := A[L + Random(Succ(R - L))];
      pL := Pred(L);
      pR := Succ(R);
      repeat
        repeat Inc(pL) until c(A[pL], Pivot) >= 0;
        repeat Dec(pR) until c(A[pR], Pivot) <= 0;
        if pL >= pR then break;
        v := TFake(A[pL]);
        TFake(A[pL]) := TFake(A[pR]);
        TFake(A[pR]) := v;
      until False;
      if pL = pR then
        begin
          if pL > L then
            Dec(pR)
          else
            if pL < R then
              Inc(pL);
        end;
      if pR < N then L := pL;
      if pL > N then R := pR;
    end;
  if (L < R) and (c(A[L], A[R]) > 0) then
    begin
      v := TFake(A[L]);
      TFake(A[L]) := TFake(A[R]);
      TFake(A[R]) := v;
    end;
  Result := A[N];
end;

class function TGRegularArrayHelper.SequentSearch(constref A: array of T; constref aValue: T;
  c: TCompare): SizeInt;
begin
  for Result := 0 to System.High(A) do
    if c(aValue, A[Result]) = 0 then
      exit;
  Result := -1;
end;

class function TGRegularArrayHelper.BinarySearch(constref A: array of T; constref aValue: T;
  c: TCompare): SizeInt;
begin
  Result := High(A);
  if Result >= 0 then
    Result := DoBinSearch(@A[0], Result, aValue, c);
end;

class function TGRegularArrayHelper.BinarySearchPos(constref A: array of T; constref aValue: T;
  c: TCompare): TSearchResult;
var
  hi: SizeInt;
begin
  hi := High(A);
  if hi >= 0 then
    Result := DoBinSearchPos(@A[0], hi, aValue, c)
  else
    begin
      Result.FoundIndex := hi;
      Result.InsertIndex := hi;
    end;
end;

class function TGRegularArrayHelper.IndexOfMin(constref A: array of T; c: TCompare): SizeInt;
var
  R, I: SizeInt;
  v: T;
begin
  R := System.High(A);
  if R >= 0 then
    begin
      Result := 0;
      v := A[0];
      for I := 1 to R do
        if c(A[I], v) < 0 then
          begin
            v := A[I];
            Result := I;
          end;
    end
  else
    Result := R;
end;

class function TGRegularArrayHelper.IndexOfMax(constref A: array of T; c: TCompare): SizeInt;
var
  R, I: SizeInt;
  v: T;
begin
  R := System.High(A);
  if R >= 0 then
    begin
      Result := 0;
      v := A[0];
      for I := 1 to R do
        if c(v, A[I]) < 0 then
          begin
            v := A[I];
            Result := I;
          end;
    end
  else
    Result := R;
end;

class function TGRegularArrayHelper.GetMin(constref A: array of T; c: TCompare): TOptional;
var
  v: T;
begin
  if FindMin(A, v, c) then
    Result.Assign(v);
end;

class function TGRegularArrayHelper.GetMax(constref A: array of T; c: TCompare): TOptional;
var
  v: T;
begin
  if FindMax(A, v, c) then
    Result.Assign(v);
end;

class function TGRegularArrayHelper.FindMin(constref A: array of T; out aValue: T; c: TCompare): Boolean;
var
  R, I: SizeInt;
begin
  R := System.High(A);
  Result := R >= 0;
  if Result then
    begin
      aValue := A[0];
      for I := 1 to R do
        if c(A[I], aValue) < 0 then
          aValue := A[I];
    end;
end;

class function TGRegularArrayHelper.FindMax(constref A: array of T; out aValue: T; c: TCompare): Boolean;
var
  R, I: SizeInt;
begin
  R := System.High(A);
  Result := R >= 0;
  if Result then
    begin
      aValue := A[0];
      for I := 1 to R do
        if c(aValue, A[I]) < 0 then
          aValue := A[I];
    end;
end;

class function TGRegularArrayHelper.FindMinMax(constref A: array of T; out aMin, aMax: T; c: TCompare): Boolean;
var
  R, I: SizeInt;
begin
  R := System.High(A);
  Result := R >= 0;
  if Result then
    begin
      aMin := A[0];
      aMax := A[0];
      for I := 1 to R do
        if c(aMax, A[I]) < 0 then
          aMax := A[I]
        else
          if c(A[I], aMin) < 0 then
            aMin := A[I];
    end;
end;

class function TGRegularArrayHelper.FindNthSmallest(var A: array of T; N: SizeInt; out aValue: T;
  c: TCompare): Boolean;
var
  R: SizeInt;
begin
  R := System.High(A);
  if R < 0 then
    exit(False);
  if N <= 0 then
    exit(FindMin(A, aValue, c));
  if N >= R then
    exit(FindMax(A, aValue, c));
  aValue := QSelectR(@A[0], R, N, c);
  Result := True;
end;

class function TGRegularArrayHelper.NthSmallest(var A: array of T; N: SizeInt; c: TCompare): TOptional;
var
  v: T;
begin
  if FindNthSmallest(A, N, v, c) then
    Result.Assign(v);
end;

class function TGRegularArrayHelper.FindNthSmallestND(constref A: array of T; N: SizeInt; out aValue: T;
  c: TCompare): Boolean;
begin
  Result := FindNthSmallest(CreateCopy(A), N, aValue, c);
end;

class function TGRegularArrayHelper.NthSmallestND(constref A: array of T; N: SizeInt; c: TCompare): TOptional;
var
  v: T;
begin
  if FindNthSmallestND(A, N, v, c) then
    Result.Assign(v);
end;

class function TGRegularArrayHelper.NextPermutation2Asc(var A: array of T; c: TCompare): Boolean;
var
  I, J, R: SizeInt;
  v: TFake;
begin
  R := System.High(A);
  J := -1;
  for I := Pred(R) downto 0 do
    if c(A[I], A[Succ(I)]) > 0 then
      begin
        J := I;
        break;
      end;
  if J < 0 then exit(False);
  for I := R downto 0 do
    if c(A[J], A[I]) > 0 then
      begin
        v := TFake(A[I]);
        TFake(A[I]) := TFake(A[J]);
        TFake(A[J]) := v;
        break;
      end;
  DoReverse(@A[Succ(J)], R - Succ(J));
  Result := True;
end;

class function TGRegularArrayHelper.NextPermutation2Desc(var A: array of T; c: TCompare): Boolean;
var
  I, J, R: SizeInt;
  v: TFake;
begin
  R := System.High(A);
  J := -1;
  for I := Pred(R) downto 0 do
    if c(A[I], A[Succ(I)]) < 0 then
      begin
        J := I;
        break;
      end;
  if J < 0 then exit(False);
  for I := R downto 0 do
    if c(A[J], A[I]) < 0 then
      begin
        v := TFake(A[I]);
        TFake(A[I]) := TFake(A[J]);
        TFake(A[J]) := v;
        break;
      end;
  DoReverse(@A[Succ(J)], R - Succ(J));
  Result := True;
end;

class function TGRegularArrayHelper.IsNonDescending(constref A: array of T; c: TCompare): Boolean;
var
  I: SizeInt;
begin
  for I := 0 to Pred(System.High(A)) do
    if c(A[I], A[Succ(I)]) > 0 then
      exit(False);
  Result := True;
end;

class function TGRegularArrayHelper.IsStrictAscending(constref A: array of T; c: TCompare): Boolean;
var
  I, R: SizeInt;
begin
  R := System.High(A);
  if R > 0 then
    begin
      for I := 1 to R do
        if c(A[Pred(I)], A[I]) >= 0 then
          exit(False);
      Result := True;
    end
  else
    Result := False;
end;

class function TGRegularArrayHelper.IsNonAscending(constref A: array of T; c: TCompare): Boolean;
var
  I: SizeInt;
begin
  for I := 0 to Pred(System.High(A)) do
    if c(A[I], A[Succ(I)]) < 0 then
      exit(False);
  Result := True;
end;

class function TGRegularArrayHelper.IsStrictDescending(constref A: array of T; c: TCompare): Boolean;
var
  I, R: SizeInt;
begin
  R := System.High(A);
  if R > 0 then
    begin
      for I := 1 to R do
        if c(A[Pred(I)], A[I]) <= 0 then
          exit(False);
      Result := True;
    end
  else
    Result := False;
end;

class function TGRegularArrayHelper.Same(constref A, B: array of T; c: TCompare): Boolean;
var
  R, I: SizeInt;
begin
  R := System.High(A);
  if System.High(B) <> R then
    exit(False);
  for I := 0 to R do
    if c(A[I], B[I]) <> 0 then
      exit(False);
  Result := True;
end;

class procedure TGRegularArrayHelper.QuickSort(var A: array of T; c: TCompare; o: TSortOrder);
var
  R: SizeInt;
begin
  R := System.High(A);
  if R > 0 then
    if CountRun2Asc(@A[0], R, c) < R then
      begin
        DoQSort(@A[0], R, c);
        if o = soDesc then
          Reverse(A);
      end
    else
      if (o = soDesc) and (c(A[0], A[R]) <> 0) then
        Reverse(A);
end;

class procedure TGRegularArrayHelper.IntroSort(var A: array of T; c: TCompare; o: TSortOrder);
var
  R: SizeInt;
begin
  R := System.High(A);
  if R > 0 then
    if CountRun2Asc(@A[0], R, c) < R then
      begin
        DoIntroSort(@A[0], R, Pred(LGUtils.NSB(R + 1)) * INTRO_LOG_FACTOR, c);
        if o = soDesc then
          Reverse(A);
      end
    else
      if (o = soDesc) and (c(A[0], A[R]) <> 0) then
        Reverse(A);
end;

class procedure TGRegularArrayHelper.DualPivotQuickSort(var A: array of T; c: TCompare; o: TSortOrder);
var
  R: SizeInt;
begin
  R := System.High(A);
  if R > 0 then
    if CountRun2Asc(@A[0], R, c) < R then
      begin
        DoDPQSort(@A[0], R, c);
        if o = soDesc then
          Reverse(A);
      end
    else
      if (o = soDesc) and (c(A[0], A[R]) <> 0) then
        Reverse(A);
end;

class procedure TGRegularArrayHelper.MergeSort(var A: array of T; c: TCompare; o: TSortOrder);
var
  R: SizeInt;
begin
  R := High(A);
  if R > 0 then
    if o = soAsc then
      TMergeSort.SortAsc(@A[0], R, c)
    else
      TMergeSort.SortDesc(@A[0], R, c);
end;

class procedure TGRegularArrayHelper.Sort(var A: array of T; c: TCompare; o: TSortOrder);
begin
  IntroSort(A, c, o);
end;

class function TGRegularArrayHelper.Sorted(constref A: array of T; c: TCompare; o: TSortOrder): TArray;
begin
  Result := CreateCopy(A);
  Sort(Result, c, o);
end;

class function TGRegularArrayHelper.SelectDistinct(constref A: array of T; c: TCompare): TArray;
var
  I, J, Hi: SizeInt;
begin
  Result := Sorted(A, c);
  Hi := System.High(Result);
  if Hi < 1 then
    exit;
  I := 0;
  for J := 1 to Hi do
    begin
      if c(Result[I], Result[J]) = 0 then
        continue;
      Inc(I);
      if J > I then
        Result[I] := Result[J];
    end;
  System.SetLength(Result, Succ(I));
end;

{ TGDelegatedArrayHelper.TMergeSort }

procedure TGDelegatedArrayHelper.TMergeSort.Init(A: PItem; c: TOnCompare);
begin
  inherited Init(A);
  FCompare := c;
end;

procedure TGDelegatedArrayHelper.TMergeSort.CollapseA;
var
  I: SizeInt;
begin
  while FStackSize > 1 do
    begin
      I := FStackSize - 2;
      if (I > 0) and (FStack[I - 1].Count <= FStack[I].Count + FStack[I + 1].Count) then
        begin
          if FStack[I - 1].Count < FStack[I + 1].Count then
            Dec(I);
        end
      else
        if FStack[I].Count > FStack[I + 1].Count then
          break;
      MergeAtA(I);
    end;
end;

procedure TGDelegatedArrayHelper.TMergeSort.CollapseD;
var
  I: SizeInt;
begin
  while FStackSize > 1 do
    begin
      I := FStackSize - 2;
      if (I > 0) and (FStack[I - 1].Count <= FStack[I].Count + FStack[I + 1].Count) then
        begin
          if FStack[I - 1].Count < FStack[I + 1].Count then
            Dec(I);
        end
      else
        if FStack[I].Count > FStack[I + 1].Count then
          break;
      MergeAtD(I);
    end;
end;

procedure TGDelegatedArrayHelper.TMergeSort.ForceCollapseA;
var
  I: SizeInt;
begin
  while FStackSize > 1 do
    begin
      I := FStackSize - 2;
      if (I > 0) and (FStack[I - 1].Count < FStack[I + 1].Count) then
        MergeAtA(I - 1)
      else
        MergeAtA(I);
    end;
end;

procedure TGDelegatedArrayHelper.TMergeSort.ForceCollapseD;
var
  I: SizeInt;
begin
  while FStackSize > 1 do
    begin
      I := FStackSize - 2;
      if (I > 0) and (FStack[I - 1].Count < FStack[I + 1].Count) then
        MergeAtD(I - 1)
      else
        MergeAtD(I);
    end;
end;

procedure TGDelegatedArrayHelper.TMergeSort.MergeAtA(aIndex: SizeInt);
var
  Base, CountLo, CountHi: SizeInt;
begin
  Base := FStack[aIndex].Base;
  CountLo := FStack[aIndex].Count;
  CountHi := FStack[aIndex + 1].Count;
  FStack[aIndex].Count := CountLo + CountHi;
  if aIndex = FStackSize - 3 then
    begin
      FStack[aIndex + 1].Base := FStack[aIndex + 2].Base;
      FStack[aIndex + 1].Count := FStack[aIndex + 2].Count;
    end;
  Dec(FStackSize);
  if CountLo <= CountHi then
    MergeLoA(Base, CountLo, CountHi)
  else
    MergeHiA(Base, CountLo, CountHi);
end;

procedure TGDelegatedArrayHelper.TMergeSort.MergeAtD(aIndex: SizeInt);
var
  Base, CountLo, CountHi: SizeInt;
begin
  Base := FStack[aIndex].Base;
  CountLo := FStack[aIndex].Count;
  CountHi := FStack[aIndex + 1].Count;
  FStack[aIndex].Count := CountLo + CountHi;
  if aIndex = FStackSize - 3 then
    begin
      FStack[aIndex + 1].Base := FStack[aIndex + 2].Base;
      FStack[aIndex + 1].Count := FStack[aIndex + 2].Count;
    end;
  Dec(FStackSize);
  if CountLo <= CountHi then
    MergeLoD(Base, CountLo, CountHi)
  else
    MergeHiD(Base, CountLo, CountHi);
end;

procedure TGDelegatedArrayHelper.TMergeSort.MergeLoA(From, CountLo, CountHi: SizeInt);
var
  pLo,           // position in low part   (in buffer)
  pHi,           // position in high part  (in data array)
  pDst: SizeInt; // current CreateMerge position (in data array)
  LocA: PItem;   // local pointer to data array
  LocB: PItem;   // local pointer to buffer
  c: TOnCompare;
begin
  LocA := FData;
  c := FCompare;
  if c(FData[Pred(From + CountLo)], FData[From + CountLo]) > 0 then
    begin
      LocB := EnsureBufferCapacity(CountLo);
      {$IFNDEF FPC_REQUIRES_PROPER_ALIGNMENT}
      System.Move(LocA[From], LocB[0], CountLo * SizeOf(T));
      {$ELSE}
      CopyItems(@LocA[From], LocB, CountLo);
      {$ENDIF}
      if c(LocA[Pred(From + CountLo + CountHi)], LocA[From]) >= 0 then
        begin
          pLo := 0;
          pHi := From + CountLo;
          pDst := From;
          CountHi := Pred(From + CountLo + CountHi);
          repeat
            if c(LocB[pLo], LocA[pHi]) <= 0 then
              begin
                TFake(LocA[pDst]) := TFake(LocB[pLo]);
                Inc(pLo);
              end
            else
              begin
                TFake(LocA[pDst]) := TFake(LocA[pHi]);
                Inc(pHi);
              end;
            Inc(pDst);
          until (pLo >= CountLo) or (pHi > CountHi);
          if pLo < CountLo then
            {$IFNDEF FPC_REQUIRES_PROPER_ALIGNMENT}
            System.Move(LocB[pLo], LocA[pDst], (CountLo - pLo) * SizeOf(T));
            {$ELSE}
            CopyItems(@LocB[pLo], @LocA[pDst], CountLo - pLo);
            {$ENDIF}
        end
      else
        begin
          {$IFNDEF FPC_REQUIRES_PROPER_ALIGNMENT}
          System.Move(LocA[From + CountLo], LocA[From], CountHi * SizeOf(T));
          System.Move(LocB[0], LocA[From + CountHi], CountLo * SizeOf(T));
          {$ELSE}
          CopyItems(@LocA[From + CountLo], @LocA[From], CountHi);
          CopyItems(LocB, @LocA[From + CountHi], CountLo);
          {$ENDIF}
        end;
    end;
end;

procedure TGDelegatedArrayHelper.TMergeSort.MergeLoD(From, CountLo, CountHi: SizeInt);
var
  pLo,           // position in low part   (in buffer)
  pHi,           // position in high part  (in data array)
  pDst: SizeInt; // current CreateMerge position (in data array)
  LocA: PItem;   // local pointer to data array
  LocB: PItem;   // local pointer to buffer
  c: TOnCompare;
begin
  LocA := FData;
  c := FCompare;
  if c(FData[Pred(From + CountLo)], FData[From + CountLo]) < 0 then
    begin
      LocB := EnsureBufferCapacity(CountLo);
      {$IFNDEF FPC_REQUIRES_PROPER_ALIGNMENT}
      System.Move(LocA[From], LocB[0], CountLo * SizeOf(T));
      {$ELSE}
      CopyItems(@LocA[From], LocB, CountLo);
      {$ENDIF}
      if c(LocA[Pred(From + CountLo + CountHi)], LocA[From]) <= 0 then
        begin
          pLo := 0;
          pHi := From + CountLo;
          pDst := From;
          CountHi := Pred(From + CountLo + CountHi);
          repeat
            if c(LocB[pLo], LocA[pHi]) >= 0 then
              begin
                TFake(LocA[pDst]) := TFake(LocB[pLo]);
                Inc(pLo);
              end
            else
              begin
                TFake(LocA[pDst]) := TFake(LocA[pHi]);
                Inc(pHi);
              end;
            Inc(pDst);
          until (pLo >= CountLo) or (pHi > CountHi);
          if pLo < CountLo then
            {$IFNDEF FPC_REQUIRES_PROPER_ALIGNMENT}
            System.Move(LocB[pLo], LocA[pDst], (CountLo - pLo) * SizeOf(T));
            {$ELSE}
            CopyItems(@LocB[pLo], @LocA[pDst], CountLo - pLo);
            {$ENDIF}
        end
      else
        begin
          {$IFNDEF FPC_REQUIRES_PROPER_ALIGNMENT}
          System.Move(LocA[From + CountLo], LocA[From], CountHi * SizeOf(T));
          System.Move(LocB[0], LocA[From + CountHi], CountLo * SizeOf(T));
          {$ELSE}
          CopyItems(@LocA[From + CountLo], @LocA[From], CountHi);
          CopyItems(LocB, @LocA[From + CountHi], CountLo);
          {$ENDIF}
        end;
    end;
end;

procedure TGDelegatedArrayHelper.TMergeSort.MergeHiA(From, CountLo, CountHi: SizeInt);
var
  pLo,           // position in low part   (in data array)
  pHi,           // position in high part  (in buffer)
  pDst: SizeInt; // current CreateMerge position (in data array)
  LocA: PItem;   // local pointer to data array
  LocB: PItem;   // local pointer to buffer
  c: TOnCompare;
begin
  LocA := FData;
  c := FCompare;
  if c(FData[Pred(From + CountLo)], FData[From + CountLo]) > 0 then
    begin
      LocB := EnsureBufferCapacity(CountHi);
      {$IFNDEF FPC_REQUIRES_PROPER_ALIGNMENT}
      System.Move(LocA[From + CountLo], LocB[0], CountHi * SizeOf(T));
      {$ELSE}
      CopyItems(@LocA[From + CountLo], LocB, CountHi);
      {$ENDIF}
      if c(LocA[Pred(From + CountLo + CountHi)], LocA[From]) >= 0 then
        begin
          pLo := Pred(From + CountLo);
          pHi := CountHi - 1;
          pDst := Pred(From + CountLo + CountHi);
          repeat
            if c(LocA[pLo], LocB[pHi]) > 0 then
              begin
                TFake(LocA[pDst]) := TFake(LocA[pLo]);
                Dec(pLo);
              end
            else
              begin
                TFake(LocA[pDst]) := TFake(LocB[pHi]);
                Dec(pHi);
              end;
            Dec(pDst);
          until (pLo < From) or (pHi < 0);
          if pHi >= 0 then
            {$IFNDEF FPC_REQUIRES_PROPER_ALIGNMENT}
            System.Move(LocB[0], LocA[From], Succ(pHi) * SizeOf(T));
            {$ELSE}
            CopyItems(LocB, @LocA[From], Succ(pHi));
            {$ENDIF}
        end
      else
        begin
          {$IFNDEF FPC_REQUIRES_PROPER_ALIGNMENT}
          System.Move(LocA[From], LocA[From + CountHi], CountLo * SizeOf(T));
          System.Move(LocB[0], LocA[From], CountHi * SizeOf(T));
          {$ELSE}
          CopyItems(@LocA[From], @LocA[From + CountHi], CountLo);
          CopyItems(LocB, @LocA[From], CountHi);
          {$ENDIF}
        end;
    end;
end;

procedure TGDelegatedArrayHelper.TMergeSort.MergeHiD(From, CountLo, CountHi: SizeInt);
var
  pLo,           // position in low part   (in data array)
  pHi,           // position in high part  (in buffer)
  pDst: SizeInt; // current CreateMerge position (in data array)
  LocA: PItem;   // local pointer to data array
  LocB: PItem;   // local pointer to buffer
  c: TOnCompare;
begin
  LocA := FData;
  c := FCompare;
  if c(FData[Pred(From + CountLo)], FData[From + CountLo]) < 0 then
    begin
      LocB := EnsureBufferCapacity(CountHi);
      {$IFNDEF FPC_REQUIRES_PROPER_ALIGNMENT}
      System.Move(LocA[From + CountLo], LocB[0], CountHi * SizeOf(T));
      {$ELSE}
      CopyItems(@LocA[From + CountLo], LocB, CountHi);
      {$ENDIF}
      if c(LocA[Pred(From + CountLo + CountHi)], LocA[From]) <= 0 then
        begin
          pLo := Pred(From + CountLo);
          pHi := CountHi - 1;
          pDst := Pred(From + CountLo + CountHi);
          repeat
            if c(LocA[pLo], LocB[pHi]) < 0 then
              begin
                TFake(LocA[pDst]) := TFake(LocA[pLo]);
                Dec(pLo);
              end
            else
              begin
                TFake(LocA[pDst]) := TFake(LocB[pHi]);
                Dec(pHi);
              end;
            Dec(pDst);
          until (pLo < From) or (pHi < 0);
          if pHi >= 0 then
            {$IFNDEF FPC_REQUIRES_PROPER_ALIGNMENT}
            System.Move(LocB[0], LocA[From], Succ(pHi) * SizeOf(T));
            {$ELSE}
            CopyItems(LocB, @LocA[From], Succ(pHi));
            {$ENDIF}
        end
      else
        begin
          {$IFNDEF FPC_REQUIRES_PROPER_ALIGNMENT}
          System.Move(LocA[From], LocA[From + CountHi], CountLo * SizeOf(T));
          System.Move(LocB[0], LocA[From], CountHi * SizeOf(T));
          {$ELSE}
          CopyItems(@LocA[From], @LocA[From + CountHi], CountLo);
          CopyItems(LocB, @LocA[From], CountHi);
          {$ENDIF}
        end;
    end;
end;

class procedure TGDelegatedArrayHelper.TMergeSort.InsertSortA(A: PItem; R, At: SizeInt; c: TOnCompare);
var
  I, J: SizeInt;
  v: TFake;
begin
  if At < 1 then
    At := 1;
  for I := At to R do
    begin
      v := TFake(A[I]);
      J := I - 1;
      while (J >= 0) and (c(A[J], T(v)) > 0) do
        begin
          TFake(A[J + 1]) := TFake(A[J]);
          Dec(J);
        end;
      TFake(A[J + 1]) := v;
    end;
end;

class procedure TGDelegatedArrayHelper.TMergeSort.InsertSortD(A: PItem; R, At: SizeInt; c: TOnCompare);
var
  I, J: SizeInt;
  v: TFake;
begin
  if At < 1 then
    At := 1;
  for I := At to R do
    begin
      v := TFake(A[I]);
      J := I - 1;
      while (J >= 0) and (c(A[J], T(v)) < 0) do
        begin
          TFake(A[J + 1]) := TFake(A[J]);
          Dec(J);
        end;
      TFake(A[J + 1]) := v;
    end;
end;

class function TGDelegatedArrayHelper.TMergeSort.CountRunAsc(A: PItem; R: SizeInt; c: TOnCompare): SizeInt;
begin
  if R > 0 then
    begin
      Result := 1;
      if c(A[0], A[1]) <= 0 then  // ascending
        while (Result < R) and (c(A[Result], A[Succ(Result)]) <= 0) do
          Inc(Result)
      else                        // descending
        begin
          while (Result < R) and (c(A[Result], A[Succ(Result)]) > 0) do
            Inc(Result);
          DoReverse(A, Result);
        end;
    end
  else
    Result := 0;
end;

class function TGDelegatedArrayHelper.TMergeSort.CountRunDesc(A: PItem; R: SizeInt; c: TOnCompare): SizeInt;
begin
  if R > 0 then
    begin
      Result := 1;
      if c(A[0], A[1]) >= 0 then  // descending
        while (Result < R) and (c(A[Result], A[Succ(Result)]) >= 0) do
          Inc(Result)
      else                        // ascending
        begin
          while (Result < R) and (c(A[Result], A[Succ(Result)]) < 0) do
            Inc(Result);
          DoReverse(A, Result);
        end;
    end
  else
    Result := 0;
end;

class procedure TGDelegatedArrayHelper.TMergeSort.SortAsc(A: PItem; R: SizeInt; c: TOnCompare);
var
  RunLen, MinLen, Len, L: SizeInt;
  ms: TMergeSort;
begin
  if R + 1 > MIN_MERGE_LEN shl 1 then
    begin
      ms.Init(A, c);
      MinLen := MinRunLen(R + 1);
      L := 0;
      repeat
        RunLen := Succ(CountRunAsc(@A[L], R - L, c));
        if RunLen < MinLen then
          begin
            Len := Math.Min(Succ(R - L), MinLen);
            InsertSortA(@A[L], Pred(Len), RunLen, c);
            RunLen := Len;
          end;
        ms.PushRun(L, RunLen);
        L += RunLen;
        ms.CollapseA;
      until L > R;
      ms.ForceCollapseA;
    end
  else
    InsertSortA(A, R, Succ(CountRunAsc(A, R, c)), c);
end;

class procedure TGDelegatedArrayHelper.TMergeSort.SortDesc(A: PItem; R: SizeInt; c: TOnCompare);
var
  RunLen, MinLen, Len, L: SizeInt;
  ms: TMergeSort;
begin
  if R + 1 > MIN_MERGE_LEN shl 1 then
    begin
      ms.Init(A, c);
      MinLen := MinRunLen(R + 1);
      L := 0;
      repeat
        RunLen := Succ(CountRunDesc(@A[L], R - L, c));
        if RunLen < MinLen then
          begin
            Len := Math.Min(Succ(R - L), MinLen);
            InsertSortD(@A[L], Pred(Len), RunLen, c);
            RunLen := Len;
          end;
        ms.PushRun(L, RunLen);
        L += RunLen;
        ms.CollapseD;
      until L > R;
      ms.ForceCollapseD;
    end
  else
    InsertSortD(A, R, Succ(CountRunDesc(A, R, c)), c);
end;

{ TGDelegatedArrayHelper }

class function TGDelegatedArrayHelper.CountRun2Asc(A: PItem; R: SizeInt; c: TOnCompare): SizeInt;
begin
  Result := 0;
  while (Result < R) and (c(A[Result], A[Succ(Result)]) = 0) do
    Inc(Result);
  if Result < R then
    begin
      Inc(Result);
      if c(A[Pred(Result)], A[Result]) < 0 then   // ascending
        while (Result < R) and (c(A[Result], A[Succ(Result)]) <= 0) do
          Inc(Result)
      else                                        // descending
        begin
          while (Result < R) and (c(A[Result], A[Succ(Result)]) >= 0) do
            Inc(Result);
          DoReverse(A, Result);
        end;
    end;
end;

class procedure TGDelegatedArrayHelper.InsertionSort(A: PItem; R: SizeInt; c: TOnCompare);
var
  I, J: SizeInt;
  v: TFake;
begin
  for I := 1 to R do
    begin
      v := TFake(A[I]);
      J := I - 1;
      while (J >= 0) and (c(A[J], T(v)) > 0) do
        begin
          TFake(A[J + 1]) := TFake(A[J]);
          Dec(J);
        end;
      TFake(A[J + 1]) := v;
    end;
end;

class function TGDelegatedArrayHelper.DoBinSearch(A: PItem; R: SizeInt; constref aValue: T;
  c: TOnCompare): SizeInt;
var
  L, M, Cmp: SizeInt;
begin
  //here R must be >= 0;
  Result := -1;
  L := 0;
  Cmp := c(A[R], A[0]);
  if Cmp > 0 then  //ascending
    begin
      while L < R do
        begin
          M := L + (R - L) shr 1;
          Cmp := c(A[M], aValue);
          if Cmp < 0 then
            L := Succ(M)
          else
            begin
              if Cmp = 0 then
                exit(M);
              R := M;
            end;
        end;
      //here L >= R
      if c(A[R], aValue) = 0 then
        Result := R;
    end
  else
    if Cmp < 0 then  //descending
      begin
        while L < R do
          begin
            M := L + ((R - L) shr 1);
            Cmp := c(A[M], aValue);
            if Cmp > 0 then
              L := Succ(M)
            else
              begin
                if Cmp = 0 then
                  exit(M);
                R := M;
              end;
          end;
        //here L >= R
        if c(A[R], aValue) = 0 then
          Result := R;
      end
    else              //constant
      if c(A[L], aValue) = 0 then
        exit(L);
end;

class function TGDelegatedArrayHelper.DoBinSearchPosA(A: PItem; R: SizeInt; constref aValue: T;
  c: TOnCompare): TSearchResult;
var
  L, M, Cmp: SizeInt;
begin
  //here exists at least 2 ascending elements;
  Result.FoundIndex := -1;
  L := 0;
  while L < R do
    begin
      M := L + (R - L) shr 1;
      Cmp := c(A[M], aValue);
      if Cmp < 0 then
        L := Succ(M)
      else
        begin
          if Cmp = 0 then
            begin
              Result.FoundIndex := M;
              Result.InsertIndex := Succ(M);
              exit;
            end;
          R := M;
        end;
    end;
  //here L >= R
  Cmp := c(aValue, A[R]);
  if Cmp < 0 then
    Result.InsertIndex := R
  else
    begin
      Result.InsertIndex := Succ(R);
      if Cmp = 0 then
        Result.FoundIndex := R;
    end;
end;

class function TGDelegatedArrayHelper.DoBinSearchPosD(A: PItem; R: SizeInt; constref aValue: T;
  c: TOnCompare): TSearchResult;
var
  L, M, Cmp: SizeInt;
begin
  //here exists at least 2 descending elements
  Result.FoundIndex := -1;
  L := 0;
  while L < R do
    begin
      M := L + (R - L) shr 1;
      Cmp := c(A[M], aValue);
      if Cmp > 0 then
        L := Succ(M)
      else
        begin
          if Cmp = 0 then
            begin
              Result.FoundIndex := M;
              Result.InsertIndex := Succ(M);
              exit;
            end;
          R := M;
        end;
    end;
  //here L >= R
  Cmp := c(aValue, A[R]);
  if Cmp > 0 then
    Result.InsertIndex := R
  else
    begin
      Result.InsertIndex := Succ(R);
      if Cmp = 0 then
        Result.FoundIndex := R;
    end;
end;

class function TGDelegatedArrayHelper.DoBinSearchPos(A: PItem; R: SizeInt; constref aValue: T;
  c: TOnCompare): TSearchResult;
var
  Cmp: SizeInt;
begin
  //here R must be >= 0;
  Cmp := c(A[R], A[0]);
  if Cmp > 0 then   //ascending
    Result := DoBinSearchPosA(A, R, aValue, c)
  else
    if Cmp < 0 then //descending
      Result := DoBinSearchPosD(A, R, aValue, c)
    else            //constant
      begin
        Cmp := c(aValue, A[R]);
        Result.FoundIndex := -1;
        if Cmp < 0 then
           Result.InsertIndex := 0
        else
          begin
            Result.InsertIndex := Succ(R);
            if Cmp = 0 then
              Result.FoundIndex := 0;
          end;
      end;
end;

class procedure TGDelegatedArrayHelper.DoHeapSort(A: PItem; R: SizeInt; c: TOnCompare);
var
  I, Curr, Next: SizeInt;
  v: TFake;
begin
  if R > HEAP_INSERT_CUTOFF then
    begin
      for I := Pred(Succ(R) shr 1) downto 0 do
        begin
          Curr := I;
          Next := Succ(I shl 1);
          v := TFake(A[Curr]);
          while Next <= R do
            begin
              if(Succ(Next) <= R) and (c(A[Next], A[Succ(Next)]) < 0)then
                Inc(Next);
              if c(T(v), A[Next]) >= 0 then
                break;
              TFake(A[Curr]) := TFake(A[Next]);
              Curr := Next;
              Next := Succ(Next shl 1);
            end;
          TFake(A[Curr]) := v;
        end;
      for I := R downto 1 do
        begin
          Curr := 0;
          Next := 1;
          v := TFake(A[I]);
          TFake(A[I]) := TFake(A[0]);
          while Next < I do
            begin
              if(Succ(Next) < I) and (c(A[Next], A[Succ(Next)]) < 0) then
                Inc(Next);
              TFake(A[Curr]) := TFake(A[Next]);
              Curr := Next;
              Next := Succ(Next shl 1);
            end;
          Next := Pred(Curr) shr 1;
          while (Curr > 0) and (c(T(v), A[Next]) > 0) do
            begin
              TFake(A[Curr]) := TFake(A[Next]);
              Curr := Next;
              Next := Pred(Next) shr 1;
            end;
          TFake(A[Curr]) := v;
        end;
    end
  else
    InsertionSort(A, R, c);
end;

class function TGDelegatedArrayHelper.QSplitR(A: PItem; R: SizeInt; c: TOnCompare): TSortSplit;
var
  Pivot: T;
  v: TFake;
  pL, pR: SizeInt;
begin
  Pivot := A[Succ(Random(Pred(R)))]; //shouldn't be first or last
  pL := -1;
  pR := Succ(R);
  repeat
    repeat Inc(pL) until c(A[pL], Pivot) >= 0;
    repeat Dec(pR) until c(A[pR], Pivot) <= 0;
    if pL > pR then break;
    v := TFake(A[pL]);
    TFake(A[pL]) := TFake(A[pR]);
    TFake(A[pR]) := v;
  until False;
  Result.Left := pR;
  Result.Right := pL;
end;

class procedure TGDelegatedArrayHelper.DoQSort(A: PItem; R: SizeInt; c: TOnCompare);
begin
  while R > QUICK_INSERT_CUTOFF do
    with QSplitR(A, R, c) do
      if Left <= R - Right then
        begin
          DoQSort(A, Left, c);
          A := @A[Right];
          R -= Right;
        end
      else
        begin
          DoQSort(@A[Right], R - Right, c);
          R := Left;
        end;
  if R > 0 then
    InsertionSort(A, R, c);
end;

class function TGDelegatedArrayHelper.MedianOf3(constref v1, v2, v3: T; c: TOnCompare): T;
begin
  Result := v2;
  if c(v1, Result) < 0 then
    begin
      if c(v3, Result) < 0 then
        begin
          if c(v1, v3) < 0 then
            Result := v3
          else
            Result := v1;
        end;
    end
  else { v1 >= Result }
    begin
      if c(v3, Result) > 0 then
        begin
          if c(v1, v3) > 0 then
            Result := v3
          else
            Result := v1;
        end;
    end;
end;

class function TGDelegatedArrayHelper.QSplitMo9(A: PItem; R: SizeInt; c: TOnCompare): TSortSplit;
var
  Pivot: T;
  v: TFake;
  pL, pR: SizeInt;
begin
  if R > MEDIAN_OF9_CUTOFF then
    Pivot := MedianOf3(
      MedianOf3(A[0],
                A[Succ(R) shr 3],
                A[Succ(R) shr 2], c),
      MedianOf3(A[Succ(R) shr 1 - Succ(R) shr 3],
                A[Succ(R) shr 1],
                A[Succ(R) shr 1 + Succ(R) shr 3], c),
      MedianOf3(A[R - Succ(R) shr 2],
                A[R - Succ(R) shr 3],
                A[R], c), c)
  else
    Pivot := MedianOf3(A[0], A[Succ(R) shr 1], A[R], c);
  pL := -1;
  pR := Succ(R);
  repeat
    repeat Inc(pL) until c(A[pL], Pivot) >= 0;
    repeat Dec(pR) until c(A[pR], Pivot) <= 0;
    if pL > pR then break;
    v := TFake(A[pL]);
    TFake(A[pL]) := TFake(A[pR]);
    TFake(A[pR]) := v;
  until False;
  Result.Left := pR;
  Result.Right := pL;
end;

class procedure TGDelegatedArrayHelper.DoIntroSort(A: PItem; R, Ttl: SizeInt; c: TOnCompare);
begin
  if R > QUICK_INSERT_CUTOFF then
    if Ttl > 0 then
      with QSplitMo9(A, R, c) do
        begin
          DoIntroSort(A, Left, Pred(Ttl), c);
          DoIntroSort(@A[Right], R - Right, Pred(Ttl), c);
        end
    else
      DoHeapSort(A, R, c)
  else
    if R > 0 then
      InsertionSort(A, R, c);
end;

class function TGDelegatedArrayHelper.DPQSplit(A: PItem; R: SizeInt; c: TOnCompare): TSortSplit;
var
  v, Pivot1, Pivot2: TFake;
  pL, pR, I: SizeInt;
begin
  pL := Succ(Random(Pred(R shr 1)));
  pR := Pred(R - Random(Pred(R shr 1)));

  if c(A[pL], A[pR]) <= 0 then
    begin
      Pivot1 := TFake(A[pL]);
      TFake(A[pL]) := TFake(A[0]);
      Pivot2 := TFake(A[pR]);
      TFake(A[pR]) := TFake(A[R]);
    end
  else
    begin
      Pivot2 := TFake(A[pL]);
      TFake(A[pL]) := TFake(A[R]);
      Pivot1 := TFake(A[pR]);
      TFake(A[pR]) := TFake(A[0]);
    end;

  pL := 1;
  I  := 1;
  pR := Pred(R);
  while I <= pR do
    begin
      v := TFake(A[I]);
      if c(T(v), T(Pivot1)) < 0 then
        begin
          TFake(A[I]) := TFake(A[pL]);
          TFake(A[pL]) := v;
          Inc(pL);
        end
      else
        if c(T(v), T(Pivot2)) > 0 then
          begin
            while (pR >= I) and (c(A[pR], T(Pivot2)) > 0) do
              Dec(pR);
            if pR < I then
              break;
            if c(A[pR], T(Pivot1)) < 0 then
              begin
                TFake(A[I]) := TFake(A[pL]);
                TFake(A[pL]) := TFake(A[pR]);
                Inc(pL);
              end
            else
              TFake(A[I]) := TFake(A[pR]);
            TFake(A[pR]) := v;
            Dec(pR);
          end;
      Inc(I);
    end;
  TFake(A[0]) := TFake(A[pL - 1]);
  TFake(A[pL - 1]) := Pivot1;
  TFake(A[R]) := TFake(A[pR + 1]);
  TFake(A[pR + 1]) := Pivot2;

  Result.Left := pL - 1;
  Result.Right := pR + 1;
end;

class procedure TGDelegatedArrayHelper.DoDPQSort(A: PItem; R: SizeInt; c: TOnCompare);
begin
  if R > DPQ_INSERT_CUTOFF then
    with DPQSplit(A, R, c) do
      begin
        DoDPQSort(A, Left - 1, c);
        DoDPQSort(@A[Right + 1], R - Right - 1, c);
        if c(A[Left], A[Right]) <> 0 then
          DoDPQSort(@A[Left + 1], Right - Left - 2, c);
      end
  else
    if R > 0 then
      InsertionSort(A, R, c);
end;

class function TGDelegatedArrayHelper.QSelectR(A: PItem; R, N: SizeInt; c: TOnCompare): T;
var
  v: TFake;
  Pivot: T;
  L, pL, pR: SizeInt;
begin
  L := 0;
  while L < Pred(R) do
    begin
      Pivot := A[L + Random(Succ(R - L))];
      pL := Pred(L);
      pR := Succ(R);
      repeat
        repeat Inc(pL) until c(A[pL], Pivot) >= 0;
        repeat Dec(pR) until c(A[pR], Pivot) <= 0;
        if pL >= pR then break;
        v := TFake(A[pL]);
        TFake(A[pL]) := TFake(A[pR]);
        TFake(A[pR]) := v;
      until False;
      if pL = pR then
        begin
          if pL > L then
            Dec(pR)
          else
            if pL < R then
              Inc(pL);
        end;
      if pR < N then L := pL;
      if pL > N then R := pR;
    end;
  if (L < R) and (c(A[L], A[R]) > 0) then
    begin
      v := TFake(A[L]);
      TFake(A[L]) := TFake(A[R]);
      TFake(A[R]) := v;
    end;
  Result := A[N];
end;

class function TGDelegatedArrayHelper.SequentSearch(constref A: array of T; constref aValue: T;
  c: TOnCompare): SizeInt;
begin
  for Result := 0 to System.High(A) do
    if c(aValue, A[Result]) = 0 then
      exit;
  Result := -1;
end;

class function TGDelegatedArrayHelper.BinarySearch(constref A: array of T; constref aValue: T;
  c: TOnCompare): SizeInt;
begin
  Result := High(A);
  if Result >= 0 then
    Result := DoBinSearch(@A[0], Result, aValue, c);
end;

class function TGDelegatedArrayHelper.BinarySearchPos(constref A: array of T; constref aValue: T;
  c: TOnCompare): TSearchResult;
var
  hi: SizeInt;
begin
  hi := High(A);
  if hi >= 0 then
    Result := DoBinSearchPos(@A[0], hi, aValue, c)
  else
    begin
      Result.FoundIndex := hi;
      Result.InsertIndex := hi;
    end;
end;

class function TGDelegatedArrayHelper.IndexOfMin(constref A: array of T; c: TOnCompare): SizeInt;
var
  R, I: SizeInt;
  v: T;
begin
  R := System.High(A);
  if R >= 0 then
    begin
      Result := 0;
      v := A[0];
      for I := 1 to R do
        if c(A[I], v) < 0 then
          begin
            v := A[I];
            Result := I;
          end;
    end
  else
    Result := R;
end;

class function TGDelegatedArrayHelper.IndexOfMax(constref A: array of T; c: TOnCompare): SizeInt;
var
  R, I: SizeInt;
  v: T;
begin
  R := System.High(A);
  if R >= 0 then
    begin
      Result := 0;
      v := A[0];
      for I := 1 to R do
        if c(v, A[I]) < 0 then
          begin
            v := A[I];
            Result := I;
          end;
    end
  else
    Result := R;
end;

class function TGDelegatedArrayHelper.GetMin(constref A: array of T; c: TOnCompare): TOptional;
var
  v: T;
begin
  if FindMin(A, v, c) then
    Result.Assign(v);
end;

class function TGDelegatedArrayHelper.GetMax(constref A: array of T; c: TOnCompare): TOptional;
var
  v: T;
begin
  if FindMax(A, v, c) then
    Result.Assign(v);
end;

class function TGDelegatedArrayHelper.FindMin(constref A: array of T; out aValue: T; c: TOnCompare): Boolean;
var
  R, I: SizeInt;
begin
  R := System.High(A);
  Result := R >= 0;
  if Result then
    begin
      aValue := A[0];
      for I := 1 to R do
        if c(A[I], aValue) < 0 then
          aValue := A[I];
    end;
end;

class function TGDelegatedArrayHelper.FindMax(constref A: array of T; out aValue: T; c: TOnCompare): Boolean;
var
  R, I: SizeInt;
begin
  R := System.High(A);
  Result := R >= 0;
  if Result then
    begin
      aValue := A[0];
      for I := 1 to R do
        if c(aValue, A[I]) < 0 then
          aValue := A[I];
    end;
end;

class function TGDelegatedArrayHelper.FindMinMax(constref A: array of T; out aMin, aMax: T;
  c: TOnCompare): Boolean;
var
  R, I: SizeInt;
begin
  R := System.High(A);
  Result := R >= 0;
  if Result then
    begin
      aMin := A[0];
      aMax := A[0];
      for I := 1 to R do
        if c(aMax, A[I]) < 0 then
          aMax := A[I]
        else
          if c(A[I], aMin) < 0 then
            aMin := A[I];
    end;
end;

class function TGDelegatedArrayHelper.FindNthSmallest(var A: array of T; N: SizeInt; out aValue: T;
  c: TOnCompare): Boolean;
var
  R: SizeInt;
begin
  R := System.High(A);
  if R < 0 then
    exit(False);
  if N <= 0 then
    exit(FindMin(A, aValue, c));
  if N >= R then
    exit(FindMax(A, aValue, c));
  aValue := QSelectR(@A[0], R, N, c);
  Result := True;
end;

class function TGDelegatedArrayHelper.NthSmallest(var A: array of T; N: SizeInt; c: TOnCompare): TOptional;
var
  v: T;
begin
  if FindNthSmallest(A, N, v, c) then
    Result.Assign(v);
end;

class function TGDelegatedArrayHelper.FindNthSmallestND(constref A: array of T;
  N: SizeInt; out aValue: T; c: TOnCompare): Boolean;
begin
  Result := FindNthSmallest(CreateCopy(A), N, aValue, c);
end;

class function TGDelegatedArrayHelper.NthSmallestND(constref A: array of T;N: SizeInt; c: TOnCompare): TOptional;
var
  v: T;
begin
  if FindNthSmallestND(A, N, v, c) then
    Result.Assign(v);
end;

class function TGDelegatedArrayHelper.NextPermutation2Asc(var A: array of T; c: TOnCompare): Boolean;
var
  I, J, R: SizeInt;
  v: TFake;
begin
  R := System.High(A);
  J := -1;
  for I := Pred(R) downto 0 do
    if c(A[I], A[Succ(I)]) > 0 then
      begin
        J := I;
        break;
      end;
  if J < 0 then exit(False);
  for I := R downto 0 do
    if c(A[J], A[I]) > 0 then
      begin
        v := TFake(A[I]);
        TFake(A[I]) := TFake(A[J]);
        TFake(A[J]) := v;
        break;
      end;
  DoReverse(@A[Succ(J)], R - Succ(J));
  Result := True;
end;

class function TGDelegatedArrayHelper.NextPermutation2Desc(var A: array of T; c: TOnCompare): Boolean;
var
  I, J, R: SizeInt;
  v: TFake;
begin
  R := System.High(A);
  J := -1;
  for I := Pred(R) downto 0 do
    if c(A[I], A[Succ(I)]) < 0 then
      begin
        J := I;
        break;
      end;
  if J < 0 then exit(False);
  for I := R downto 0 do
    if c(A[J], A[I]) < 0 then
      begin
        v := TFake(A[I]);
        TFake(A[I]) := TFake(A[J]);
        TFake(A[J]) := v;
        break;
      end;
  DoReverse(@A[Succ(J)], R - Succ(J));
  Result := True;
end;

class function TGDelegatedArrayHelper.IsNonDescending(constref A: array of T; c: TOnCompare): Boolean;
var
  I: SizeInt;
begin
  for I := 0 to Pred(System.High(A)) do
    if c(A[I], A[Succ(I)]) > 0 then
      exit(False);
  Result := True;
end;

class function TGDelegatedArrayHelper.IsStrictAscending(constref A: array of T; c: TOnCompare): Boolean;
var
  I, R: SizeInt;
begin
  R := System.High(A);
  if R > 0 then
    begin
      for I := 1 to R do
        if c(A[Pred(I)], A[I]) >= 0 then
          exit(False);
      Result := True;
    end
  else
    Result := False;
end;

class function TGDelegatedArrayHelper.IsNonAscending(constref A: array of T; c: TOnCompare): Boolean;
var
  I: SizeInt;
begin
  for I := 0 to Pred(System.High(A)) do
    if c(A[I], A[Succ(I)]) < 0 then
      exit(False);
  Result := True;
end;

class function TGDelegatedArrayHelper.IsStrictDescending(constref A: array of T; c: TOnCompare): Boolean;
var
  I, R: SizeInt;
begin
  R := System.High(A);
  if R > 0 then
    begin
      for I := 1 to R do
        if c(A[Pred(I)], A[I]) <= 0 then
          exit(False);
      Result := True;
    end
  else
    Result := False;
end;

class function TGDelegatedArrayHelper.Same(constref A, B: array of T; c: TOnCompare): Boolean;
var
  R, I: SizeInt;
begin
  R := System.High(A);
  if System.High(B) <> R then
    exit(False);
  for I := 0 to R do
    if c(A[I], B[I]) <> 0 then
      exit(False);
  Result := True;
end;

class procedure TGDelegatedArrayHelper.QuickSort(var A: array of T; c: TOnCompare; o: TSortOrder);
var
  R: SizeInt;
begin
  R := System.High(A);
  if R > 0 then
    if CountRun2Asc(@A[0], R, c) < R then
      begin
        DoQSort(@A[0], R, c);
        if o = soDesc then
          Reverse(A);
      end
    else
      if (o = soDesc) and (c(A[0], A[R]) <> 0) then
        Reverse(A);
end;

class procedure TGDelegatedArrayHelper.IntroSort(var A: array of T; c: TOnCompare; o: TSortOrder);
var
  R: SizeInt;
begin
  R := System.High(A);
  if R > 0 then
    if CountRun2Asc(@A[0], R, c) < R then
      begin
        DoIntroSort(@A[0], R, Pred(LGUtils.NSB(R + 1)) * INTRO_LOG_FACTOR, c);
        if o = soDesc then
          Reverse(A);
      end
    else
      if (o = soDesc) and (c(A[0], A[R]) <> 0) then
        Reverse(A);
end;

class procedure TGDelegatedArrayHelper.DualPivotQuickSort(var A: array of T; c: TOnCompare; o: TSortOrder);
var
  R: SizeInt;
begin
  R := System.High(A);
  if R > 0 then
    if CountRun2Asc(@A[0], R, c) < R then
      begin
        DoDPQSort(@A[0], R, c);
        if o = soDesc then
          Reverse(A);
      end
    else
      if (o = soDesc) and (c(A[0], A[R]) <> 0) then
        Reverse(A);
end;

class procedure TGDelegatedArrayHelper.MergeSort(var A: array of T; c: TOnCompare; o: TSortOrder);
var
  R: SizeInt;
begin
  R := High(A);
  if R > 0 then
    if o = soAsc then
      TMergeSort.SortAsc(@A[0], R, c)
    else
      TMergeSort.SortDesc(@A[0], R, c);
end;

class procedure TGDelegatedArrayHelper.Sort(var A: array of T; c: TOnCompare; o: TSortOrder);
begin
  IntroSort(A, c, o);
end;

class function TGDelegatedArrayHelper.Sorted(constref A: array of T; c: TOnCompare; o: TSortOrder): TArray;
begin
  Result := CreateCopy(A);
  Sort(Result, c, o);
end;

class function TGDelegatedArrayHelper.SelectDistinct(constref A: array of T; c: TOnCompare): TArray;
var
  I, J, Hi: SizeInt;
begin
  Result := Sorted(A, c);
  Hi := System.High(Result);
  if Hi < 1 then
    exit;
  I := 0;
  for J := 1 to Hi do
    begin
      if c(Result[I], Result[J]) = 0 then
        continue;
      Inc(I);
      if J > I then
        Result[I] := Result[J];
    end;
  System.SetLength(Result, Succ(I));
end;

{ TGNestedArrayHelper.TMergeSort }

procedure TGNestedArrayHelper.TMergeSort.Init(A: PItem; c: TNestCompare);
begin
  inherited Init(A);
  FCompare := c;
end;

procedure TGNestedArrayHelper.TMergeSort.CollapseA;
var
  I: SizeInt;
begin
  while FStackSize > 1 do
    begin
      I := FStackSize - 2;
      if (I > 0) and (FStack[I - 1].Count <= FStack[I].Count + FStack[I + 1].Count) then
        begin
          if FStack[I - 1].Count < FStack[I + 1].Count then
            Dec(I);
        end
      else
        if FStack[I].Count > FStack[I + 1].Count then
          break;
      MergeAtA(I);
    end;
end;

procedure TGNestedArrayHelper.TMergeSort.CollapseD;
var
  I: SizeInt;
begin
  while FStackSize > 1 do
    begin
      I := FStackSize - 2;
      if (I > 0) and (FStack[I - 1].Count <= FStack[I].Count + FStack[I + 1].Count) then
        begin
          if FStack[I - 1].Count < FStack[I + 1].Count then
            Dec(I);
        end
      else
        if FStack[I].Count > FStack[I + 1].Count then
          break;
      MergeAtD(I);
    end;
end;

procedure TGNestedArrayHelper.TMergeSort.ForceCollapseA;
var
  I: SizeInt;
begin
  while FStackSize > 1 do
    begin
      I := FStackSize - 2;
      if (I > 0) and (FStack[I - 1].Count < FStack[I + 1].Count) then
        MergeAtA(I - 1)
      else
        MergeAtA(I);
    end;
end;

procedure TGNestedArrayHelper.TMergeSort.ForceCollapseD;
var
  I: SizeInt;
begin
  while FStackSize > 1 do
    begin
      I := FStackSize - 2;
      if (I > 0) and (FStack[I - 1].Count < FStack[I + 1].Count) then
        MergeAtD(I - 1)
      else
        MergeAtD(I);
    end;
end;

procedure TGNestedArrayHelper.TMergeSort.MergeAtA(aIndex: SizeInt);
var
  Base, CountLo, CountHi: SizeInt;
begin
  Base := FStack[aIndex].Base;
  CountLo := FStack[aIndex].Count;
  CountHi := FStack[aIndex + 1].Count;
  FStack[aIndex].Count := CountLo + CountHi;
  if aIndex = FStackSize - 3 then
    begin
      FStack[aIndex + 1].Base := FStack[aIndex + 2].Base;
      FStack[aIndex + 1].Count := FStack[aIndex + 2].Count;
    end;
  Dec(FStackSize);
  if CountLo <= CountHi then
    MergeLoA(Base, CountLo, CountHi)
  else
    MergeHiA(Base, CountLo, CountHi);
end;

procedure TGNestedArrayHelper.TMergeSort.MergeAtD(aIndex: SizeInt);
var
  Base, CountLo, CountHi: SizeInt;
begin
  Base := FStack[aIndex].Base;
  CountLo := FStack[aIndex].Count;
  CountHi := FStack[aIndex + 1].Count;
  FStack[aIndex].Count := CountLo + CountHi;
  if aIndex = FStackSize - 3 then
    begin
      FStack[aIndex + 1].Base := FStack[aIndex + 2].Base;
      FStack[aIndex + 1].Count := FStack[aIndex + 2].Count;
    end;
  Dec(FStackSize);
  if CountLo <= CountHi then
    MergeLoD(Base, CountLo, CountHi)
  else
    MergeHiD(Base, CountLo, CountHi);
end;

procedure TGNestedArrayHelper.TMergeSort.MergeLoA(From, CountLo, CountHi: SizeInt);
var
  pLo,           // position in low part   (in buffer)
  pHi,           // position in high part  (in data array)
  pDst: SizeInt; // current CreateMerge position (in data array)
  LocA: PItem;   // local pointer to data array
  LocB: PItem;   // local pointer to buffer
  c: TNestCompare;
begin
  LocA := FData;
  c := FCompare;
  if c(FData[Pred(From + CountLo)], FData[From + CountLo]) > 0 then
    begin
      LocB := EnsureBufferCapacity(CountLo);
      {$IFNDEF FPC_REQUIRES_PROPER_ALIGNMENT}
      System.Move(LocA[From], LocB[0], CountLo * SizeOf(T));
      {$ELSE}
      CopyItems(@LocA[From], LocB, CountLo);
      {$ENDIF}
      if c(LocA[Pred(From + CountLo + CountHi)], LocA[From]) >= 0 then
        begin
          pLo := 0;
          pHi := From + CountLo;
          pDst := From;
          CountHi := Pred(From + CountLo + CountHi);
          repeat
            if c(LocB[pLo], LocA[pHi]) <= 0 then
              begin
                TFake(LocA[pDst]) := TFake(LocB[pLo]);
                Inc(pLo);
              end
            else
              begin
                TFake(LocA[pDst]) := TFake(LocA[pHi]);
                Inc(pHi);
              end;
            Inc(pDst);
          until (pLo >= CountLo) or (pHi > CountHi);
          if pLo < CountLo then
            {$IFNDEF FPC_REQUIRES_PROPER_ALIGNMENT}
            System.Move(LocB[pLo], LocA[pDst], (CountLo - pLo) * SizeOf(T));
            {$ELSE}
            CopyItems(@LocB[pLo], @LocA[pDst], CountLo - pLo);
            {$ENDIF}
        end
      else
        begin
          {$IFNDEF FPC_REQUIRES_PROPER_ALIGNMENT}
          System.Move(LocA[From + CountLo], LocA[From], CountHi * SizeOf(T));
          System.Move(LocB[0], LocA[From + CountHi], CountLo * SizeOf(T));
          {$ELSE}
          CopyItems(@LocA[From + CountLo], @LocA[From], CountHi);
          CopyItems(LocB, @LocA[From + CountHi], CountLo);
          {$ENDIF}
        end;
    end;
end;

procedure TGNestedArrayHelper.TMergeSort.MergeLoD(From, CountLo, CountHi: SizeInt);
var
  pLo,           // position in low part   (in buffer)
  pHi,           // position in high part  (in data array)
  pDst: SizeInt; // current CreateMerge position (in data array)
  LocA: PItem;   // local pointer to data array
  LocB: PItem;   // local pointer to buffer
  c: TNestCompare;
begin
  LocA := FData;
  c := FCompare;
  if c(FData[Pred(From + CountLo)], FData[From + CountLo]) < 0 then
    begin
      LocB := EnsureBufferCapacity(CountLo);
      {$IFNDEF FPC_REQUIRES_PROPER_ALIGNMENT}
      System.Move(LocA[From], LocB[0], CountLo * SizeOf(T));
      {$ELSE}
      CopyItems(@LocA[From], LocB, CountLo);
      {$ENDIF}
      if c(LocA[Pred(From + CountLo + CountHi)], LocA[From]) <= 0 then
        begin
          pLo := 0;
          pHi := From + CountLo;
          pDst := From;
          CountHi := Pred(From + CountLo + CountHi);
          repeat
            if c(LocB[pLo], LocA[pHi]) >= 0 then
              begin
                TFake(LocA[pDst]) := TFake(LocB[pLo]);
                Inc(pLo);
              end
            else
              begin
                TFake(LocA[pDst]) := TFake(LocA[pHi]);
                Inc(pHi);
              end;
            Inc(pDst);
          until (pLo >= CountLo) or (pHi > CountHi);
          if pLo < CountLo then
            {$IFNDEF FPC_REQUIRES_PROPER_ALIGNMENT}
            System.Move(LocB[pLo], LocA[pDst], (CountLo - pLo) * SizeOf(T));
            {$ELSE}
            CopyItems(@LocB[pLo], @LocA[pDst], CountLo - pLo);
            {$ENDIF}
        end
      else
        begin
          {$IFNDEF FPC_REQUIRES_PROPER_ALIGNMENT}
          System.Move(LocA[From + CountLo], LocA[From], CountHi * SizeOf(T));
          System.Move(LocB[0], LocA[From + CountHi], CountLo * SizeOf(T));
          {$ELSE}
          CopyItems(@LocA[From + CountLo], @LocA[From], CountHi);
          CopyItems(LocB, @LocA[From + CountHi], CountLo);
          {$ENDIF}
        end;
    end;
end;

procedure TGNestedArrayHelper.TMergeSort.MergeHiA(From, CountLo, CountHi: SizeInt);
var
  pLo,           // position in low part   (in data array)
  pHi,           // position in high part  (in buffer)
  pDst: SizeInt; // current CreateMerge position (in data array)
  LocA: PItem;   // local pointer to data array
  LocB: PItem;   // local pointer to buffer
  c: TNestCompare;
begin
  LocA := FData;
  c := FCompare;
  if c(FData[Pred(From + CountLo)], FData[From + CountLo]) > 0 then
    begin
      LocB := EnsureBufferCapacity(CountHi);
      {$IFNDEF FPC_REQUIRES_PROPER_ALIGNMENT}
      System.Move(LocA[From + CountLo], LocB[0], CountHi * SizeOf(T));
      {$ELSE}
      CopyItems(@LocA[From + CountLo], LocB, CountHi);
      {$ENDIF}
      if c(LocA[Pred(From + CountLo + CountHi)], LocA[From]) >= 0 then
        begin
          pLo := Pred(From + CountLo);
          pHi := CountHi - 1;
          pDst := Pred(From + CountLo + CountHi);
          repeat
            if c(LocA[pLo], LocB[pHi]) > 0 then
              begin
                TFake(LocA[pDst]) := TFake(LocA[pLo]);
                Dec(pLo);
              end
            else
              begin
                TFake(LocA[pDst]) := TFake(LocB[pHi]);
                Dec(pHi);
              end;
            Dec(pDst);
          until (pLo < From) or (pHi < 0);
          if pHi >= 0 then
            {$IFNDEF FPC_REQUIRES_PROPER_ALIGNMENT}
            System.Move(LocB[0], LocA[From], Succ(pHi) * SizeOf(T));
            {$ELSE}
            CopyItems(LocB, @LocA[From], Succ(pHi));
            {$ENDIF}
        end
      else
        begin
          {$IFNDEF FPC_REQUIRES_PROPER_ALIGNMENT}
          System.Move(LocA[From], LocA[From + CountHi], CountLo * SizeOf(T));
          System.Move(LocB[0], LocA[From], CountHi * SizeOf(T));
          {$ELSE}
          CopyItems(@LocA[From], @LocA[From + CountHi], CountLo);
          CopyItems(LocB, @LocA[From], CountHi);
          {$ENDIF}
        end;
    end;
end;

procedure TGNestedArrayHelper.TMergeSort.MergeHiD(From, CountLo, CountHi: SizeInt);
var
  pLo,           // position in low part   (in data array)
  pHi,           // position in high part  (in buffer)
  pDst: SizeInt; // current CreateMerge position (in data array)
  LocA: PItem;   // local pointer to data array
  LocB: PItem;   // local pointer to buffer
  c: TNestCompare;
begin
  LocA := FData;
  c := FCompare;
  if c(FData[Pred(From + CountLo)], FData[From + CountLo]) < 0 then
    begin
      LocB := EnsureBufferCapacity(CountHi);
      {$IFNDEF FPC_REQUIRES_PROPER_ALIGNMENT}
      System.Move(LocA[From + CountLo], LocB[0], CountHi * SizeOf(T));
      {$ELSE}
      CopyItems(@LocA[From + CountLo], LocB, CountHi);
      {$ENDIF}
      if c(LocA[Pred(From + CountLo + CountHi)], LocA[From]) <= 0 then
        begin
          pLo := Pred(From + CountLo);
          pHi := CountHi - 1;
          pDst := Pred(From + CountLo + CountHi);
          repeat
            if c(LocA[pLo], LocB[pHi]) < 0 then
              begin
                TFake(LocA[pDst]) := TFake(LocA[pLo]);
                Dec(pLo);
              end
            else
              begin
                TFake(LocA[pDst]) := TFake(LocB[pHi]);
                Dec(pHi);
              end;
            Dec(pDst);
          until (pLo < From) or (pHi < 0);
          if pHi >= 0 then
            {$IFNDEF FPC_REQUIRES_PROPER_ALIGNMENT}
            System.Move(LocB[0], LocA[From], Succ(pHi) * SizeOf(T));
            {$ELSE}
            CopyItems(LocB, @LocA[From], Succ(pHi));
            {$ENDIF}
        end
      else
        begin
          {$IFNDEF FPC_REQUIRES_PROPER_ALIGNMENT}
          System.Move(LocA[From], LocA[From + CountHi], CountLo * SizeOf(T));
          System.Move(LocB[0], LocA[From], CountHi * SizeOf(T));
          {$ELSE}
          CopyItems(@LocA[From], @LocA[From + CountHi], CountLo);
          CopyItems(LocB, @LocA[From], CountHi);
          {$ENDIF}
        end;
    end;
end;

class procedure TGNestedArrayHelper.TMergeSort.InsertSortA(A: PItem; R, At: SizeInt; c: TNestCompare);
var
  I, J: SizeInt;
  v: TFake;
begin
  if At < 1 then
    At := 1;
  for I := At to R do
    begin
      v := TFake(A[I]);
      J := I - 1;
      while (J >= 0) and (c(A[J], T(v)) > 0) do
        begin
          TFake(A[J + 1]) := TFake(A[J]);
          Dec(J);
        end;
      TFake(A[J + 1]) := v;
    end;
end;

class procedure TGNestedArrayHelper.TMergeSort.InsertSortD(A: PItem; R, At: SizeInt; c: TNestCompare);
var
  I, J: SizeInt;
  v: TFake;
begin
  if At < 1 then
    At := 1;
  for I := At to R do
    begin
      v := TFake(A[I]);
      J := I - 1;
      while (J >= 0) and (c(A[J], T(v)) < 0) do
        begin
          TFake(A[J + 1]) := TFake(A[J]);
          Dec(J);
        end;
      TFake(A[J + 1]) := v;
    end;
end;

class function TGNestedArrayHelper.TMergeSort.CountRunAsc(A: PItem; R: SizeInt; c: TNestCompare): SizeInt;
begin
  if R > 0 then
    begin
      Result := 1;
      if c(A[0], A[1]) <= 0 then  // ascending
        while (Result < R) and (c(A[Result], A[Succ(Result)]) <= 0) do
          Inc(Result)
      else                        // descending
        begin
          while (Result < R) and (c(A[Result], A[Succ(Result)]) > 0) do
            Inc(Result);
          DoReverse(A, Result);
        end;
    end
  else
    Result := 0;
end;

class function TGNestedArrayHelper.TMergeSort.CountRunDesc(A: PItem; R: SizeInt; c: TNestCompare): SizeInt;
begin
  if R > 0 then
    begin
      Result := 1;
      if c(A[0], A[1]) >= 0 then  // descending
        while (Result < R) and (c(A[Result], A[Succ(Result)]) >= 0) do
          Inc(Result)
      else                        // ascending
        begin
          while (Result < R) and (c(A[Result], A[Succ(Result)]) < 0) do
            Inc(Result);
          DoReverse(A, Result);
        end;
    end
  else
    Result := 0;
end;

class procedure TGNestedArrayHelper.TMergeSort.SortAsc(A: PItem; R: SizeInt; c: TNestCompare);
var
  RunLen, MinLen, Len, L: SizeInt;
  ms: TMergeSort;
begin
  if R + 1 > MIN_MERGE_LEN shl 1 then
    begin
      ms.Init(A, c);
      MinLen := MinRunLen(R + 1);
      L := 0;
      repeat
        RunLen := Succ(CountRunAsc(@A[L], R - L, c));
        if RunLen < MinLen then
          begin
            Len := Math.Min(Succ(R - L), MinLen);
            InsertSortA(@A[L], Pred(Len), RunLen, c);
            RunLen := Len;
          end;
        ms.PushRun(L, RunLen);
        L += RunLen;
        ms.CollapseA;
      until L > R;
      ms.ForceCollapseA;
    end
  else
    InsertSortA(A, R, Succ(CountRunAsc(A, R, c)), c);
end;

class procedure TGNestedArrayHelper.TMergeSort.SortDesc(A: PItem; R: SizeInt; c: TNestCompare);
var
  RunLen, MinLen, Len, L: SizeInt;
  ms: TMergeSort;
begin
  if R + 1 > MIN_MERGE_LEN shl 1 then
    begin
      ms.Init(A, c);
      MinLen := MinRunLen(R + 1);
      L := 0;
      repeat
        RunLen := Succ(CountRunDesc(@A[L], R - L, c));
        if RunLen < MinLen then
          begin
            Len := Math.Min(Succ(R - L), MinLen);
            InsertSortD(@A[L], Pred(Len), RunLen, c);
            RunLen := Len;
          end;
        ms.PushRun(L, RunLen);
        L += RunLen;
        ms.CollapseD;
      until L > R;
      ms.ForceCollapseD;
    end
  else
    InsertSortD(A, R, Succ(CountRunDesc(A, R, c)), c);
end;

{ TGNestedArrayHelper }

class function TGNestedArrayHelper.CountRun2Asc(A: PItem; R: SizeInt; c: TNestCompare): SizeInt;
begin
  Result := 0;
  while (Result < R) and (c(A[Result], A[Succ(Result)]) = 0) do
    Inc(Result);
  if Result < R then
    begin
      Inc(Result);
      if c(A[Pred(Result)], A[Result]) < 0 then   // ascending
        while (Result < R) and (c(A[Result], A[Succ(Result)]) <= 0) do
          Inc(Result)
      else                                        // descending
        begin
          while (Result < R) and (c(A[Result], A[Succ(Result)]) >= 0) do
            Inc(Result);
          DoReverse(A, Result);
        end;
    end;
end;

class procedure TGNestedArrayHelper.InsertionSort(A: PItem; R: SizeInt; c: TNestCompare);
var
  I, J: SizeInt;
  v: TFake;
begin
  for I := 1 to R do
    begin
      v := TFake(A[I]);
      J := I - 1;
      while (J >= 0) and (c(A[J], T(v)) > 0) do
        begin
          TFake(A[J + 1]) := TFake(A[J]);
          Dec(J);
        end;
      TFake(A[J + 1]) := v;
    end;
end;

class function TGNestedArrayHelper.DoBinSearch(A: PItem; R: SizeInt; constref aValue: T;
  c: TNestCompare): SizeInt;
var
  L, M, Cmp: SizeInt;
begin
  //here R must be >= 0;
  Result := -1;
  L := 0;
  Cmp := c(A[R], A[L]);
  if Cmp > 0 then  //ascending
    begin
      while L < R do
        begin
          M := L + (R - L) shr 1;
          Cmp := c(A[M], aValue);
          if Cmp < 0 then
            L := Succ(M)
          else
            begin
              if Cmp = 0 then
                exit(M);
              R := M;
            end;
        end;
      //here L >= R
      if c(A[R], aValue) = 0 then
        Result := R;
    end
  else
    if Cmp < 0 then  //descending
      begin
        while L < R do
          begin
            M := L + ((R - L) shr 1);
            Cmp := c(A[M], aValue);
            if Cmp > 0 then
              L := Succ(M)
            else
              begin
                if Cmp = 0 then
                  exit(M);
                R := M;
              end;
          end;
        //here L >= R
        if c(A[R], aValue) = 0 then
          Result := R;
      end
    else              //constant
      if c(A[L], aValue) = 0 then
        exit(L);
end;

class function TGNestedArrayHelper.DoBinSearchPosA(A: PItem; R: SizeInt; constref aValue: T;
  c: TNestCompare): TSearchResult;
var
  L, M, Cmp: SizeInt;
begin
  //here exists at least 2 ascending elements;
  Result.FoundIndex := -1;
  L := 0;
  while L < R do
    begin
      M := L + (R - L) shr 1;
      Cmp := c(A[M], aValue);
      if Cmp < 0 then
        L := Succ(M)
      else
        begin
          if Cmp = 0 then
            begin
              Result.FoundIndex := M;
              Result.InsertIndex := Succ(M);
              exit;
            end;
          R := M;
        end;
    end;
  //here L >= R
  Cmp := c(aValue, A[R]);
  if Cmp < 0 then
    Result.InsertIndex := R
  else
    begin
      Result.InsertIndex := Succ(R);
      if Cmp = 0 then
        Result.FoundIndex := R;
    end;
end;

class function TGNestedArrayHelper.DoBinSearchPosD(A: PItem; R: SizeInt; constref aValue: T;
  c: TNestCompare): TSearchResult;
var
  L, M, Cmp: SizeInt;
begin
  //here exists at least 2 descending elements
  Result.FoundIndex := -1;
  L := 0;
  while L < R do
    begin
      M := L + (R - L) shr 1;
      Cmp := c(A[M], aValue);
      if Cmp > 0 then
        L := Succ(M)
      else
        begin
          if Cmp = 0 then
            begin
              Result.FoundIndex := M;
              Result.InsertIndex := Succ(M);
              exit;
            end;
          R := M;
        end;
    end;
  //here L >= R
  Cmp := c(aValue, A[R]);
  if Cmp > 0 then
    Result.InsertIndex := R
  else
    begin
      Result.InsertIndex := Succ(R);
      if Cmp = 0 then
        Result.FoundIndex := R;
    end;
end;

class function TGNestedArrayHelper.DoBinSearchPos(A: PItem; R: SizeInt; constref aValue: T;
  c: TNestCompare): TSearchResult;
var
  Cmp: SizeInt;
begin
  //here R must be >= 0;
  Cmp := c(A[R], A[0]);
  if Cmp > 0 then   //ascending
    Result := DoBinSearchPosA(A, R, aValue, c)
  else
    if Cmp < 0 then //descending
      Result := DoBinSearchPosD(A, R, aValue, c)
    else            //constant
      begin
        Cmp := c(aValue, A[R]);
        Result.FoundIndex := -1;
        if Cmp < 0 then
           Result.InsertIndex := 0
        else
          begin
            Result.InsertIndex := Succ(R);
            if Cmp = 0 then
              Result.FoundIndex := 0;
          end;
      end;
end;

class procedure TGNestedArrayHelper.DoHeapSort(A: PItem; R: SizeInt; c: TNestCompare);
var
  I, Curr, Next: SizeInt;
  v: TFake;
begin
  if R > HEAP_INSERT_CUTOFF then
    begin
      for I := Pred(Succ(R) shr 1) downto 0 do
        begin
          Curr := I;
          Next := Succ(I shl 1);
          v := TFake(A[Curr]);
          while Next <= R do
            begin
              if(Succ(Next) <= R) and (c(A[Next], A[Succ(Next)]) < 0)then
                Inc(Next);
              if c(T(v), A[Next]) >= 0 then
                break;
              TFake(A[Curr]) := TFake(A[Next]);
              Curr := Next;
              Next := Succ(Next shl 1);
            end;
          TFake(A[Curr]) := v;
        end;
      for I := R downto 1 do
        begin
          Curr := 0;
          Next := 1;
          v := TFake(A[I]);
          TFake(A[I]) := TFake(A[0]);
          while Next < I do
            begin
              if(Succ(Next) < I) and (c(A[Next], A[Succ(Next)]) < 0) then
                Inc(Next);
              TFake(A[Curr]) := TFake(A[Next]);
              Curr := Next;
              Next := Succ(Next shl 1);
            end;
          Next := Pred(Curr) shr 1;
          while (Curr > 0) and (c(T(v), A[Next]) > 0) do
            begin
              TFake(A[Curr]) := TFake(A[Next]);
              Curr := Next;
              Next := Pred(Next) shr 1;
            end;
          TFake(A[Curr]) := v;
        end;
    end
  else
    InsertionSort(A, R, c);
end;

class function TGNestedArrayHelper.QSplitR(A: PItem; R: SizeInt; c: TNestCompare): TSortSplit;
var
  Pivot: T;
  v: TFake;
  pL, pR: SizeInt;
begin
  Pivot := A[Succ(Random(Pred(R)))]; //shouldn't be first or last
  pL := -1;
  pR := Succ(R);
  repeat
    repeat Inc(pL) until c(A[pL], Pivot) >= 0;
    repeat Dec(pR) until c(A[pR], Pivot) <= 0;
    if pL > pR then break;
    v := TFake(A[pL]);
    TFake(A[pL]) := TFake(A[pR]);
    TFake(A[pR]) := v;
  until False;
  Result.Left := pR;
  Result.Right := pL;
end;

class procedure TGNestedArrayHelper.DoQSort(A: PItem; R: SizeInt; c: TNestCompare);
begin
  while R > QUICK_INSERT_CUTOFF do
    with QSplitR(A, R, c) do
      if Left <= R - Right then
        begin
          DoQSort(A, Left, c);
          A := @A[Right];
          R -= Right;
        end
      else
        begin
          DoQSort(@A[Right], R - Right, c);
          R := Left;
        end;
  if R > 0 then
    InsertionSort(A, R, c);
end;

class function TGNestedArrayHelper.MedianOf3(constref v1, v2, v3: T; c: TNestCompare): T;
begin
  Result := v2;
  if c(v1, Result) < 0 then
    begin
      if c(v3, Result) < 0 then
        begin
          if c(v1, v3) < 0 then
            Result := v3
          else
            Result := v1;
        end;
    end
  else { v1 >= Result }
    begin
      if c(v3, Result) > 0 then
        begin
          if c(v1, v3) > 0 then
            Result := v3
          else
            Result := v1;
        end;
    end;
end;

class function TGNestedArrayHelper.QSplitMo9(A: PItem; R: SizeInt; c: TNestCompare): TSortSplit;
var
  Pivot: T;
  v: TFake;
  pL, pR: SizeInt;
begin
  if R > MEDIAN_OF9_CUTOFF then
    Pivot := MedianOf3(
      MedianOf3(A[0],
                A[Succ(R) shr 3],
                A[Succ(R) shr 2], c),
      MedianOf3(A[Succ(R) shr 1 - Succ(R) shr 3],
                A[Succ(R) shr 1], A[Succ(R) shr 1 + Succ(R) shr 3], c),
      MedianOf3(A[R - Succ(R) shr 2],
                A[R - Succ(R) shr 3],
                A[R], c), c)
  else
    Pivot := MedianOf3(A[0], A[Succ(R) shr 1], A[R], c);
  pL := -1;
  pR := Succ(R);
  repeat
    repeat Inc(pL) until c(A[pL], Pivot) >= 0;
    repeat Dec(pR) until c(A[pR], Pivot) <= 0;
    if pL > pR then break;
    v := TFake(A[pL]);
    TFake(A[pL]) := TFake(A[pR]);
    TFake(A[pR]) := v;
  until False;
  Result.Left := pR;
  Result.Right := pL;
end;

class procedure TGNestedArrayHelper.DoIntroSort(A: PItem; R, Ttl: SizeInt; c: TNestCompare);
begin
  if R > QUICK_INSERT_CUTOFF then
    if Ttl > 0 then
      with QSplitMo9(A, R, c) do
        begin
          DoIntroSort(A, Left, Pred(Ttl), c);
          DoIntroSort(@A[Right], R - Right, Pred(Ttl), c);
        end
    else
      DoHeapSort(A, R, c)
  else
    if R > 0 then
      InsertionSort(A, R, c);
end;

class function TGNestedArrayHelper.DPQSplit(A: PItem; R: SizeInt; c: TNestCompare): TSortSplit;
var
  v, Pivot1, Pivot2: TFake;
  pL, pR, I: SizeInt;
begin
  pL := Succ(Random(Pred(R shr 1)));
  pR := Pred(R - Random(Pred(R shr 1)));

  if c(A[pL], A[pR]) <= 0 then
    begin
      Pivot1 := TFake(A[pL]);
      TFake(A[pL]) := TFake(A[0]);
      Pivot2 := TFake(A[pR]);
      TFake(A[pR]) := TFake(A[R]);
    end
  else
    begin
      Pivot2 := TFake(A[pL]);
      TFake(A[pL]) := TFake(A[R]);
      Pivot1 := TFake(A[pR]);
      TFake(A[pR]) := TFake(A[0]);
    end;

  pL := 1;
  I  := 1;
  pR := Pred(R);
  while I <= pR do
    begin
      v := TFake(A[I]);
      if c(T(v), T(Pivot1)) < 0 then
        begin
          TFake(A[I]) := TFake(A[pL]);
          TFake(A[pL]) := v;
          Inc(pL);
        end
      else
        if c(T(v), T(Pivot2)) > 0 then
          begin
            while (pR >= I) and (c(A[pR], T(Pivot2)) > 0) do
              Dec(pR);
            if pR < I then
              break;
            if c(A[pR], T(Pivot1)) < 0 then
              begin
                TFake(A[I]) := TFake(A[pL]);
                TFake(A[pL]) := TFake(A[pR]);
                Inc(pL);
              end
            else
              TFake(A[I]) := TFake(A[pR]);
            TFake(A[pR]) := v;
            Dec(pR);
          end;
      Inc(I);
    end;
  TFake(A[0]) := TFake(A[pL - 1]);
  TFake(A[pL - 1]) := Pivot1;
  TFake(A[R]) := TFake(A[pR + 1]);
  TFake(A[pR + 1]) := Pivot2;

  Result.Left := pL - 1;
  Result.Right := pR + 1;
end;

class procedure TGNestedArrayHelper.DoDPQSort(A: PItem; R: SizeInt; c: TNestCompare);
begin
  if R > DPQ_INSERT_CUTOFF then
    with DPQSplit(A, R, c) do
      begin
        DoDPQSort(A, Left - 1, c);
        DoDPQSort(@A[Right + 1], R - Right - 1, c);
        if c(A[Left], A[Right]) <> 0 then
          DoDPQSort(@A[Left + 1], Right - Left - 2, c);
      end
  else
    if R > 0 then
      InsertionSort(A, R, c);
end;

class function TGNestedArrayHelper.QSelectR(A: PItem; R, N: SizeInt; c: TNestCompare): T;
var
  v: TFake;
  Pivot: T;
  L, pL, pR: SizeInt;
begin
  L := 0;
  while L < Pred(R) do
    begin
      Pivot := A[L + Random(Succ(R - L))];
      pL := Pred(L);
      pR := Succ(R);
      repeat
        repeat Inc(pL) until c(A[pL], Pivot) >= 0;
        repeat Dec(pR) until c(A[pR], Pivot) <= 0;
        if pL >= pR then break;
        v := TFake(A[pL]);
        TFake(A[pL]) := TFake(A[pR]);
        TFake(A[pR]) := v;
      until False;
      if pL = pR then
        begin
          if pL > L then
            Dec(pR)
          else
            if pL < R then
              Inc(pL);
        end;
      if pR < N then L := pL;
      if pL > N then R := pR;
    end;
  if (L < R) and (c(A[L], A[R]) > 0) then
    begin
      v := TFake(A[L]);
      TFake(A[L]) := TFake(A[R]);
      TFake(A[R]) := v;
    end;
  Result := A[N];
end;

class function TGNestedArrayHelper.SequentSearch(constref A: array of T; constref aValue: T;
  c: TNestCompare): SizeInt;
begin
  for Result := 0 to System.High(A) do
    if c(aValue, A[Result]) = 0 then
      exit;
  Result := -1;
end;

class function TGNestedArrayHelper.BinarySearch(constref A: array of T; constref aValue: T;
  c: TNestCompare): SizeInt;
begin
  Result := High(A);
  if Result >= 0 then
    Result := DoBinSearch(@A[0], Result, aValue, c);
end;

class function TGNestedArrayHelper.BinarySearchPos(constref A: array of T; constref aValue: T;
  c: TNestCompare): TSearchResult;
var
  hi: SizeInt;
begin
  hi := High(A);
  if hi >= 0 then
    Result := DoBinSearchPos(@A[0], hi, aValue, c)
  else
    begin
      Result.FoundIndex := hi;
      Result.InsertIndex := hi;
    end;
end;

class function TGNestedArrayHelper.IndexOfMin(constref A: array of T; c: TNestCompare): SizeInt;
var
  R, I: SizeInt;
  v: T;
begin
  R := System.High(A);
  if R >= 0 then
    begin
      Result := 0;
      v := A[0];
      for I := 1 to R do
        if c(A[I], v) < 0 then
          begin
            v := A[I];
            Result := I;
          end;
    end
  else
    Result := R;
end;

class function TGNestedArrayHelper.IndexOfMax(constref A: array of T; c: TNestCompare): SizeInt;
var
  R, I: SizeInt;
  v: T;
begin
  R := System.High(A);
  if R >= 0 then
    begin
      Result := 0;
      v := A[0];
      for I := 1 to R do
        if c(v, A[I]) < 0 then
          begin
            v := A[I];
            Result := I;
          end;
    end
  else
    Result := R;
end;

class function TGNestedArrayHelper.GetMin(constref A: array of T; c: TNestCompare): TOptional;
var
  v: T;
begin
  if FindMin(A, v, c) then
    Result.Assign(v);
end;

class function TGNestedArrayHelper.GetMax(constref A: array of T; c: TNestCompare): TOptional;
var
  v: T;
begin
  if FindMax(A, v, c) then
    Result.Assign(v);
end;

class function TGNestedArrayHelper.FindMin(constref A: array of T; out aValue: T; c: TNestCompare): Boolean;
var
  R, I: SizeInt;
begin
  R := System.High(A);
  Result := R >= 0;
  if Result then
    begin
      aValue := A[0];
      for I := 1 to R do
        if c(A[I], aValue) < 0 then
          aValue := A[I];
    end;
end;

class function TGNestedArrayHelper.FindMax(constref A: array of T; out aValue: T; c: TNestCompare): Boolean;
var
  R, I: SizeInt;
begin
  R := System.High(A);
  Result := R >= 0;
  if Result then
    begin
      aValue := A[0];
      for I := 1 to R do
        if c(aValue, A[I]) < 0 then
          aValue := A[I];
    end;
end;

class function TGNestedArrayHelper.FindMinMax(constref A: array of T; out aMin, aMax: T;
  c: TNestCompare): Boolean;
var
  R, I: SizeInt;
begin
  R := System.High(A);
  Result := R >= 0;
  if Result then
    begin
      aMin := A[0];
      aMax := A[0];
      for I := 1 to R do
        if c(aMax, A[I]) < 0 then
          aMax := A[I]
        else
          if c(A[I], aMin) < 0 then
            aMin := A[I];
    end;
end;

class function TGNestedArrayHelper.FindNthSmallest(var A: array of T; N: SizeInt; out aValue: T;
  c: TNestCompare): Boolean;
var
  R: SizeInt;
begin
  R := System.High(A);
  if R < 0 then
    exit(False);
  if N <= 0 then
    exit(FindMin(A, aValue, c));
  if N >= R then
    exit(FindMax(A, aValue, c));
  aValue := QSelectR(@A[0], R, N, c);
  Result := True;
end;

class function TGNestedArrayHelper.NthSmallest(var A: array of T; N: SizeInt; c: TNestCompare): TOptional;
var
  v: T;
begin
  if FindNthSmallest(A, N, v, c) then
    Result.Assign(v);
end;

class function TGNestedArrayHelper.FindNthSmallestND(constref A: array of T;
  N: SizeInt; out aValue: T; c: TNestCompare): Boolean;
begin
  Result := FindNthSmallest(CreateCopy(A), N, aValue, c);
end;

class function TGNestedArrayHelper.NthSmallestND(constref A: array of T;N: SizeInt; c: TNestCompare): TOptional;
var
  v: T;
begin
  if FindNthSmallestND(A, N, v, c) then
    Result.Assign(v);
end;

class function TGNestedArrayHelper.NextPermutation2Asc(var A: array of T; c: TNestCompare): Boolean;
var
  I, J, R: SizeInt;
  v: TFake;
begin
  R := System.High(A);
  J := -1;
  for I := Pred(R) downto 0 do
    if c(A[I], A[Succ(I)]) > 0 then
      begin
        J := I;
        break;
      end;
  if J < 0 then exit(False);
  for I := R downto 0 do
    if c(A[J], A[I]) > 0 then
      begin
        v := TFake(A[I]);
        TFake(A[I]) := TFake(A[J]);
        TFake(A[J]) := v;
        break;
      end;
  DoReverse(@A[Succ(J)], R - Succ(J));
  Result := True;
end;

class function TGNestedArrayHelper.NextPermutation2Desc(var A: array of T; c: TNestCompare): Boolean;
var
  I, J, R: SizeInt;
  v: TFake;
begin
  R := System.High(A);
  J := -1;
  for I := Pred(R) downto 0 do
    if c(A[I], A[Succ(I)]) < 0 then
      begin
        J := I;
        break;
      end;
  if J < 0 then exit(False);
  for I := R downto 0 do
    if c(A[J], A[I]) < 0 then
      begin
        v := TFake(A[I]);
        TFake(A[I]) := TFake(A[J]);
        TFake(A[J]) := v;
        break;
      end;
  DoReverse(@A[Succ(J)], R - Succ(J));
  Result := True;
end;

class function TGNestedArrayHelper.IsNonDescending(constref A: array of T; c: TNestCompare): Boolean;
var
  I: SizeInt;
begin
  for I := 0 to Pred(System.High(A)) do
    if c(A[I], A[Succ(I)]) > 0 then
      exit(False);
  Result := True;
end;

class function TGNestedArrayHelper.IsStrictAscending(constref A: array of T; c: TNestCompare): Boolean;
var
  I, R: SizeInt;
begin
  R := System.High(A);
  if R > 0 then
    begin
      for I := 1 to R do
        if c(A[Pred(I)], A[I]) >= 0 then
          exit(False);
      Result := True;
    end
  else
    Result := False;
end;

class function TGNestedArrayHelper.IsNonAscending(constref A: array of T; c: TNestCompare): Boolean;
var
  I: SizeInt;
begin
  for I := 0 to Pred(System.High(A)) do
    if c(A[I], A[Succ(I)]) < 0 then
      exit(False);
  Result := True;
end;

class function TGNestedArrayHelper.IsStrictDescending(constref A: array of T; c: TNestCompare): Boolean;
var
  I, R: SizeInt;
begin
  R := System.High(A);
  if R > 0 then
    begin
      for I := 1 to R do
        if c(A[Pred(I)], A[I]) <= 0 then
          exit(False);
      Result := True;
    end
  else
    Result := False;
end;

class function TGNestedArrayHelper.Same(constref A, B: array of T; c: TNestCompare): Boolean;
var
  R, I: SizeInt;
begin
  R := System.High(A);
  if System.High(B) <> R then
    exit(False);
  for I := 0 to R do
    if c(A[I], B[I]) <> 0 then
      exit(False);
  Result := True;
end;

class procedure TGNestedArrayHelper.QuickSort(var A: array of T; c: TNestCompare; o: TSortOrder);
var
  R: SizeInt;
begin
  R := System.High(A);
  if R > 0 then
    if CountRun2Asc(@A[0], R, c) < R then
      begin
        DoQSort(@A[0], R, c);
        if o = soDesc then
          Reverse(A);
      end
    else
      if (o = soDesc) and (c(A[0], A[R]) <> 0) then
        Reverse(A);
end;

class procedure TGNestedArrayHelper.IntroSort(var A: array of T; c: TNestCompare; o: TSortOrder);
var
  R: SizeInt;
begin
  R := System.High(A);
  if R > 0 then
    if CountRun2Asc(@A[0], R, c) < R then
      begin
        DoIntroSort(@A[0], R, Pred(LGUtils.NSB(R + 1)) * INTRO_LOG_FACTOR, c);
        if o = soDesc then
          Reverse(A);
      end
    else
      if (o = soDesc) and (c(A[0], A[R]) <> 0) then
        Reverse(A);
end;

class procedure TGNestedArrayHelper.DualPivotQuickSort(var A: array of T; c: TNestCompare; o: TSortOrder);
var
  R: SizeInt;
begin
  R := System.High(A);
  if R > 0 then
    if CountRun2Asc(@A[0], R, c) < R then
      begin
        DoDPQSort(@A[0], R, c);
        if o = soDesc then
          Reverse(A);
      end
    else
      if (o = soDesc) and (c(A[0], A[R]) <> 0) then
        Reverse(A);
end;

class procedure TGNestedArrayHelper.MergeSort(var A: array of T; c: TNestCompare; o: TSortOrder);
var
  R: SizeInt;
begin
  R := High(A);
  if R > 0 then
    if o = soAsc then
      TMergeSort.SortAsc(@A[0], R, c)
    else
      TMergeSort.SortDesc(@A[0], R, c);
end;

class procedure TGNestedArrayHelper.Sort(var A: array of T; c: TNestCompare; o: TSortOrder);
begin
  IntroSort(A, c, o);
end;

class function TGNestedArrayHelper.Sorted(constref A: array of T; c: TNestCompare; o: TSortOrder): TArray;
begin
  Result := CreateCopy(A);
  Sort(Result, c, o);
end;

class function TGNestedArrayHelper.SelectDistinct(constref A: array of T; c: TNestCompare): TArray;
var
  I, J, Hi: SizeInt;
begin
  Result := Sorted(A, c);
  Hi := System.High(Result);
  if Hi < 1 then
    exit;
  I := 0;
  for J := 1 to Hi do
    begin
      if c(Result[I], Result[J]) = 0 then
        continue;
      Inc(I);
      if J > I then
        Result[I] := Result[J];
    end;
  System.SetLength(Result, Succ(I));
end;

{ TGSimpleArrayHelper }

class function TGSimpleArrayHelper.CountRun2Asc(var A: array of T; L, R: SizeInt): SizeInt;
begin
  Result := L;
  while (Result < R) and (A[Result] = A[Succ(Result)]) do
    Inc(Result);
  if Result < R then
    begin
      Inc(Result);
      if A[Pred(Result)] < A[Result] then  // ascending
        while (Result < R) and (A[Result] <= A[Succ(Result)]) do
          Inc(Result)
      else                                 // descending
        begin
          while (Result < R) and (A[Result] >= A[Succ(Result)]) do
            Inc(Result);
          Reverse(A[0..Result]);
        end;
    end;
end;

class procedure TGSimpleArrayHelper.InsertionSort(var A: array of T; L, R: SizeInt);
var
  I, J: SizeInt;
  v: T;
begin
  for I := L + 1 to R do
    begin
      v := A[I];
      J := I - 1;
      while (J >= 0) and (A[J] > v) do
        begin
          A[J + 1] := A[J];
          Dec(J);
        end;
      A[J + 1] := v;
    end;
end;

class function TGSimpleArrayHelper.DoBinSearch(A: PItem; R: SizeInt; constref aValue: T): SizeInt;
var
  L, M: SizeInt;
begin
  //here R must be >= 0;
  Result := -1;
  L := 0;
  if A[R] > A[L] then  //ascending
    begin
      while L < R do
        begin
          M := L + (R - L) shr 1;
          if A[M] < aValue then
            L := Succ(M)
          else
            begin
              if A[M] = aValue then
                exit(M);
              R := M;
            end;
        end;
      //here L >= R
      if A[R] = aValue then
        Result := R;
    end
  else
    if A[R] < A[L] then  //descending
      begin
        while L < R do
          begin
            M := L + ((R - L) shr 1);
            if A[M] > aValue then
              L := Succ(M)
            else
              begin
                if A[M] = aValue then
                  exit(M);
                R := M;
              end;
          end;
        //here L >= R
        if A[R] = aValue then
          Result := R;
      end
    else              //constant
      if A[L] = aValue then
        exit(L);
end;

class function TGSimpleArrayHelper.DoBinSearchPosA(A: PItem; R: SizeInt; constref aValue: T): TSearchResult;
var
  L, M: SizeInt;
begin
  //here exists at least 2 ascending elements;
  Result.FoundIndex := -1;
  L := 0;
  while L < R do
    begin
      M := L + (R - L) shr 1;
      if A[M] < aValue then
        L := Succ(M)
      else
        begin
          if A[M] = aValue then
            begin
              Result.FoundIndex := M;
              Result.InsertIndex := Succ(M);
              exit;
            end;
          R := M;
        end;
    end;
  //here L >= R
  if aValue < A[R] then
    Result.InsertIndex := R
  else
    begin
      Result.InsertIndex := Succ(R);
      if aValue = A[R] then
        Result.FoundIndex := R;
    end;
end;

class function TGSimpleArrayHelper.DoBinSearchPosD(A: PItem; R: SizeInt; constref aValue: T): TSearchResult;
var
  L, M: SizeInt;
begin
  //here exists at least 2 descending elements
  Result.FoundIndex := -1;
  L := 0;
  while L < R do
    begin
      M := L + (R - L) shr 1;
      if A[M] > aValue then
        L := Succ(M)
      else
        begin
          if A[M] = aValue then
            begin
              Result.FoundIndex := M;
              Result.InsertIndex := Succ(M);
              exit;
            end;
          R := M;
        end;
    end;
  //here L >= R
  if aValue > A[R] then
    Result.InsertIndex := R
  else
    begin
      Result.InsertIndex := Succ(R);
      if aValue = A[R] then
        Result.FoundIndex := R;
    end;
end;

class function TGSimpleArrayHelper.DoBinSearchPos(A: PItem; R: SizeInt; constref aValue: T): TSearchResult;
begin
  //here R must be >= 0;
  if A[0] < A[R] then   //ascending
    Result := DoBinSearchPosA(A, R, aValue)
  else
    if A[0] > A[R] then //descending
      Result := DoBinSearchPosD(A, R, aValue)
    else                //constant
      begin
        Result.FoundIndex := -1;
        if A[R] > aValue then
           Result.InsertIndex := 0
        else
          begin
            Result.InsertIndex := Succ(R);
            if A[R] = aValue then
              Result.FoundIndex := 0;
          end;
      end;
end;

class procedure TGSimpleArrayHelper.DoHeapSort(A: PItem; R: SizeInt);
var
  I, Curr, Next: SizeInt;
  v: T;
begin
  if R > 0 then
    begin
      for I := Pred(Succ(R) shr 1) downto 0 do
        begin
          Curr := I;
          Next := Succ(I shl 1);
          v := A[Curr];
          while Next <= R do
            begin
              if(Succ(Next) <= R) and (A[Next] < A[Succ(Next)])then
                Inc(Next);
              if v >= A[Next] then
                break;
              A[Curr] := A[Next];
              Curr := Next;
              Next := Succ(Next shl 1);
            end;
          A[Curr] := v;
        end;
      for I := R downto 1 do
        begin
          Curr := 0;
          Next := 1;
          v := A[I];
          A[I] := A[0];
          while Next < I do
            begin
              if(Succ(Next) < I) and (A[Next] < A[Succ(Next)]) then
                Inc(Next);
              A[Curr] := A[Next];
              Curr := Next;
              Next := Succ(Next shl 1);
            end;
          Next := Pred(Curr) shr 1;
          while (Curr > 0) and (v > A[Next]) do
            begin
              A[Curr] := A[Next];
              Curr := Next;
              Next := Pred(Next) shr 1;
            end;
          A[Curr] := v;
        end;
    end;
end;

class function TGSimpleArrayHelper.QSplitR(var A: array of T; L, R: SizeInt): TSortSplit;
var
  v, Pivot: T;
begin
  Pivot := A[Succ(L + Random(Pred(R - L)))]; //shouldn't be first or last
  Dec(L);
  Inc(R);
  repeat
    repeat Inc(L); until A[L] >= Pivot;
    repeat Dec(R); until A[R] <= Pivot;
    //if L > R then
    //  break;
    v := A[L];
    A[L] := A[R];
    A[R] := v;
  //until False;
  until L > R;
  v := A[L];
  A[L] := A[R];
  A[R] := v;

  Result.Left := R;
  Result.Right := L;
end;

class procedure TGSimpleArrayHelper.DoQSort(var A: array of T; L, R: SizeInt);
begin
  while R - L > QUICK_INSERT_CUTOFF do
    with QSplitR(A, L, R) do
      if Left - L <= R - Right then
        begin
          DoQSort(A, L, Left);
          L := Right;
        end
      else
        begin
          DoQSort(A, Right, R);
          R := Left;
        end;
  if R - L > 0 then
    InsertionSort(A, L, R);
end;

class function TGSimpleArrayHelper.MedianOf3(const v1, v2, v3: T): T;
begin
  Result := v2;
  if v1 < Result then
    begin
      if v3 < Result then
        begin
          if v1 < v3 then
            Result := v3
          else
            Result := v1;
        end;
    end
  else { v1 >= Result }
    begin
      if v3 > Result then
        begin
          if v1 > v3 then
            Result := v3
          else
            Result := v1;
        end;
    end;
end;

class function TGSimpleArrayHelper.GetMo9Pivot(constref A: array of T; L, R: SizeInt): T;
begin
  if R - L > MEDIAN_OF9_CUTOFF then
    begin
      Result := MedianOf3(
      MedianOf3(A[L], A[L + Succ(R - L) shr 3], A[L + Succ(R - L) shr 2]),
      MedianOf3(A[L + Succ(R - L) shr 1 - Succ(R - L) shr 3], A[L + Succ(R - L) shr 1],
                A[L + Succ(R - L) shr 1 + Succ(R - L) shr 3]),
      MedianOf3(A[R - Succ(R - L) shr 2], A[R - Succ(R - L) shr 3], A[R]));
    end
  else
    Result := MedianOf3(A[L], A[L + Succ(R - L) shr 1], A[R]);
end;

class function TGSimpleArrayHelper.QSplitMo9(var A: array of T; L, R: SizeInt): TSortSplit;
var
  v, Pivot: T;
begin
  Pivot := GetMo9Pivot(A, L, R);
  Dec(L);
  Inc(R);
  repeat
    repeat
      Inc(L);
    until A[L] >= Pivot;
    repeat
      Dec(R);
    until A[R] <= Pivot;
    if L > R then
      break;
    v := A[L];
    A[L] := A[R];
    A[R] := v;
  until False;
  Result.Left := R;
  Result.Right := L;
end;

class procedure TGSimpleArrayHelper.DoIntroSort(var A: array of T; L, R, Ttl: SizeInt);
begin
  if R - L > QUICK_INSERT_CUTOFF then
    if Ttl > 0 then
      with QSplitMo9(A, L, R) do
        begin
          DoIntroSort(A, L, Left, Pred(Ttl));
          DoIntroSort(A, Right, R, Pred(Ttl));
        end
    else
      DoHeapSort(@A[L], R - L)
  else
    if R - L > 0 then
      InsertionSort(A, L, R);
end;

class function TGSimpleArrayHelper.DPQSplit(var A: array of T; L, R: SizeInt): TSortSplit;
var
  v, Pivot1, Pivot2: T;
  pL, pR, I: SizeInt;
label
  EndLoop;
begin
  pL := Succ(L + Random(Pred((R - L) shr 1)));
  pR := Pred(R - Random(Pred((R - L) shr 1)));

  if A[pL] <= A[pR] then
    begin
      Pivot1 := A[pL];
      A[pL] := A[L];
      Pivot2 := A[pR];
      A[pR] := A[R];
    end
  else
    begin
      Pivot2 := A[pL];
      A[pL] := A[R];
      Pivot1 := A[pR];
      A[pR] := A[L];
    end;

  pL := Succ(L);
  I  := Succ(L);
  pR := Pred(R);
  while I <= pR do
    begin
      v := A[I];
      if v < Pivot1 then
        begin
          A[I] := A[pL];
          A[pL] := v;
          Inc(pL);
        end
      else
        if v > Pivot2 then
          begin
            while A[pR] > Pivot2 do
              begin
                Dec(pR);
                if pR < I then
                  goto EndLoop;
              end;
            if A[pR] < Pivot1 then
              begin
                A[I] := A[pL];
                A[pL] := A[pR];
                Inc(pL);
              end
            else
              A[I] := A[pR];
            A[pR] := v;
            Dec(pR);
          end;
      Inc(I);
    end;

EndLoop:

  A[L] := A[pL - 1];
  A[pL - 1] := Pivot1;
  A[R] := A[pR + 1];
  A[pR + 1] := Pivot2;

  Result.Left := pL - 1;
  Result.Right := pR + 1;
end;

class procedure TGSimpleArrayHelper.DoDPQSort(var A: array of T; L, R: SizeInt);
begin
  if R - L > DPQ_INSERT_CUTOFF then
    with DPQSplit(A, L, R) do
      begin
        DoDPQSort(A, L, Left - 1);
        DoDPQSort(A, Right + 1, R);
        if A[Left] <> A[Right] then
          DoDPQSort(A, Left + 1, Right - 1);
      end
  else
    if R - L > 0 then
      InsertionSort(A, L, R);
end;

class procedure TGSimpleArrayHelper.DoSwap(p: PItem; L, R: SizeInt);
var
  v: T;
begin
  v := p[L];
  p[L] := p[R];
  p[R] := v;
end;

class procedure TGSimpleArrayHelper.DoReverse(var A: array of T; L, R: SizeInt);
var
  v0, v1, v2, v3: T;
begin
  while R - L >= 7 do
    begin
      v0 := A[L    ];
      v1 := A[L + 1];
      v2 := A[L + 2];
      v3 := A[L + 3];
      A[L    ] := A[R    ];
      A[L + 1] := A[R - 1];
      A[L + 2] := A[R - 2];
      A[L + 3] := A[R - 3];
      A[R    ] := v0;
      A[R - 1] := v1;
      A[R - 2] := v2;
      A[R - 3] := v3;
      L += 4;
      R -= 4;
    end;
  case R - L of
    1..2:
      begin
        v0 := A[L];
        A[L] := A[R];
        A[R] := v0;
      end;
    3..4:
      begin
        v0 := A[L    ];
        v1 := A[L + 1];
        A[L    ] := A[R    ];
        A[L + 1] := A[R - 1];
        A[R    ] := v0;
        A[R - 1] := v1;
      end;
    5..6:
      begin
        v0 := A[L    ];
        v1 := A[L + 1];
        v2 := A[L + 2];
        A[L    ] := A[R    ];
        A[L + 1] := A[R - 1];
        A[L + 2] := A[R - 2];
        A[R    ] := v0;
        A[R - 1] := v1;
        A[R - 2] := v2;
      end;
  end;
end;

class function TGSimpleArrayHelper.QSelectR(var A: array of T; N: SizeInt): T;
var
  L, R, pL, pR: SizeInt;
  v, Pivot: T;
begin
  R := System.High(A);
  L := 0;
  while L < Pred(R) do
    begin
      Pivot := A[L + Random(Succ(R - L))];
      pL := Pred(L);
      pR := Succ(R);
      repeat
        repeat Inc(pL) until A[pL] >= Pivot;
        repeat Dec(pR) until A[pR] <= Pivot;
        if pL >= pR then break;
        v := A[pL];
        A[pL] := A[pR];
        A[pR] := v;
      until False;
      if pL = pR then
        begin
          if pL > L then
            Dec(pR)
          else
            if pL < R then
              Inc(pL);
        end;
      if pR < N then L := pL;
      if pL > N then R := pR;
    end;
  if (L < R) and (A[L] > A[R]) then
    begin
      v := A[L];
      A[L] := A[R];
      A[R] := v;
    end;
  Result := A[N];
end;

class procedure TGSimpleArrayHelper.Reverse(var A: array of T);
var
  R: SizeInt;
begin
  R := System.High(A);
  if R > 0 then
    DoReverse(A, 0, R);
end;

class procedure TGSimpleArrayHelper.RotateLeft(var A: array of T; aDist: SizeInt);
var
  Len: SizeInt;
begin
  if (aDist = 0) or (Abs(aDist) >= System.Length(A)) then
    exit;
  Len := System.Length(A);
  if aDist < 0 then
    aDist += Len;
  DoReverse(A, 0, Pred(aDist));
  DoReverse(A, aDist, Pred(Len));
  DoReverse(A, 0, Pred(Len));
end;

class procedure TGSimpleArrayHelper.RotateRight(var A: array of T; aDist: SizeInt);
begin
  if (aDist = 0) or (Abs(aDist) >= System.Length(A)) then
    exit;
  if aDist > 0 then
    RotateLeft(A, System.Length(A) - aDist)
  else
    RotateLeft(A, -aDist)
end;

class function TGSimpleArrayHelper.SequentSearch(constref A: array of T; constref aValue: T): SizeInt;
begin
  for Result := 0 to System.High(A) do
    if aValue = A[Result] then
      exit;
  Result := -1;
end;

class function TGSimpleArrayHelper.BinarySearch(constref A: array of T; constref aValue: T): SizeInt;
begin
  Result := High(A);
  if Result >= 0 then
    Result := DoBinSearch(@A[0], Result, aValue);
end;

class function TGSimpleArrayHelper.BinarySearchPos(constref A: array of T; constref aValue: T): TSearchResult;
var
  hi: SizeInt;
begin
  hi := High(A);
  if hi >= 0 then
    Result := DoBinSearchPos(@A[0], hi, aValue)
  else
    begin
      Result.FoundIndex := hi;
      Result.InsertIndex := hi;
    end;
end;

class function TGSimpleArrayHelper.IndexOfMin(constref A: array of T): SizeInt;
var
  R, I: SizeInt;
  v: T;
begin
  R := System.High(A);
  if R >= 0 then
    begin
      Result := 0;
      v := A[0];
      for I := 1 to R do
        if v > A[I] then
          begin
            v := A[I];
            Result := I;
          end;
    end
  else
    Result := R;
end;

class function TGSimpleArrayHelper.IndexOfMax(constref A: array of T): SizeInt;
var
  R, I: SizeInt;
  v: T;
begin
  R := System.High(A);
  if R >= 0 then
    begin
      Result := 0;
      v := A[0];
      for I := 1 to R do
        if v < A[I] then
          begin
            v := A[I];
            Result := I;
          end;
    end
  else
    Result := R;
end;

class function TGSimpleArrayHelper.GetMin(constref A: array of T): TOptional;
var
  v: T;
begin
  if FindMin(A, v) then
    Result.Assign(v);
end;

class function TGSimpleArrayHelper.GetMax(constref A: array of T): TOptional;
var
  v: T;
begin
  if FindMax(A, v) then
    Result.Assign(v);
end;

class function TGSimpleArrayHelper.FindMin(constref A: array of T; out aValue: T): Boolean;
var
  R, I: SizeInt;
begin
  R := System.High(A);
  Result := R >= 0;
  if Result then
    begin
      aValue := A[0];
      for I := 1 to R do
        if aValue > A[I] then
          aValue := A[I];
    end;
end;

class function TGSimpleArrayHelper.FindMax(constref A: array of T; out aValue: T): Boolean;
var
  R, I: SizeInt;
begin
  R := System.High(A);
  Result := R >= 0;
  if Result then
    begin
      aValue := A[0];
      for I := 1 to R do
        if aValue < A[I] then
          aValue := A[I];
    end;
end;

class function TGSimpleArrayHelper.FindMinMax(constref A: array of T; out aMin, aMax: T): Boolean;
var
  R, I: SizeInt;
begin
  R := System.High(A);
  Result := R >= 0;
  if Result then
    begin
      aMin := A[0];
      aMax := A[0];
      for I := 1 to R do
        if A[I] > aMax then
          aMax := A[I]
        else
          if A[I] < aMin then
            aMin := A[I];
    end;
end;

class function TGSimpleArrayHelper.FindNthSmallest(var A: array of T; N: SizeInt; out aValue: T): Boolean;
var
  R: SizeInt;
begin
  R := System.High(A);
  if R < 0 then
    exit(False);
  if N <= 0 then
    exit(FindMin(A, aValue));
  if N >= R then
    exit(FindMax(A, aValue));
  aValue := QSelectR(A, N);
  Result := True;
end;

class function TGSimpleArrayHelper.NthSmallest(var A: array of T; N: SizeInt): TOptional;
var
  v: T;
begin
  if FindNthSmallest(A, N, v) then
    Result.Assign(v);
end;

class function TGSimpleArrayHelper.FindNthSmallestND(constref A: array of T; N: SizeInt; out aValue: T): Boolean;
begin
  Result := FindNthSmallest(CreateCopy(A), N, aValue);
end;

class function TGSimpleArrayHelper.NthSmallestND(constref A: array of T; N: SizeInt): TOptional;
var
  v: T;
begin
  if FindNthSmallestND(A, N, v) then
    Result.Assign(v);
end;

class function TGSimpleArrayHelper.NextPermutation2Asc(var A: array of T): Boolean;
var
  I, J, R: SizeInt;
begin
  R := System.High(A);
  J := -1;
  for I := Pred(R) downto 0 do
    if A[I] > A[Succ(I)] then
      begin
        J := I;
        break;
      end;
  if J < 0 then exit(False);
  for I := R downto 0 do
    if A[J] > A[I] then
      begin
        DoSwap(@A[0], I, J);
        break;
      end;
  DoReverse(A, Succ(J), R);
  Result := True;
end;

class function TGSimpleArrayHelper.NextPermutation2Desc(var A: array of T): Boolean;
var
  I, J, R: SizeInt;
begin
  R := System.High(A);
  J := -1;
  for I := Pred(R) downto 0 do
    if A[I] < A[Succ(I)] then
      begin
        J := I;
        break;
      end;
  if J < 0 then exit(False);
  for I := R downto 0 do
    if A[J] < A[I] then
      begin
        DoSwap(@A[0], I, J);
        break;
      end;
  DoReverse(A, Succ(J), R);
  Result := True;
end;

class function TGSimpleArrayHelper.IsNonDescending(constref A: array of T): Boolean;
var
  I: SizeInt;
begin
  for I := 0 to Pred(System.High(A)) do
    if A[I] > A[Succ(I)] then
      exit(False);
  Result := True;
end;

class function TGSimpleArrayHelper.IsStrictAscending(constref A: array of T): Boolean;
var
  I, R: SizeInt;
begin
  R := System.High(A);
  if R > 0 then
    begin
      for I := 1 to R do
        if A[Pred(I)] >= A[I] then
          exit(False);
      Result := True;
    end
  else
    Result := False;
end;

class function TGSimpleArrayHelper.IsNonAscending(constref A: array of T): Boolean;
var
  I: SizeInt;
begin
  for I := 0 to Pred(System.High(A)) do
    if A[I] < A[Succ(I)] then
      exit(False);
  Result := True;
end;

class function TGSimpleArrayHelper.IsStrictDescending(constref A: array of T): Boolean;
var
  I, R: SizeInt;
begin
  R := System.High(A);
  if R > 0 then
    begin
      for I := 1 to R do
        if A[Pred(I)] <= A[I] then
          exit(False);
      Result := True;
    end
  else
    Result := False;
end;

class function TGSimpleArrayHelper.Same(constref A, B: array of T): Boolean;
var
  R, I: SizeInt;
begin
  R := System.High(A);
  if System.High(B) <> R then
    exit(False);
  for I := 0 to R do
    if A[I] <> B[I] then
      exit(False);
  Result := True;
end;

class procedure TGSimpleArrayHelper.QuickSort(var A: array of T; o: TSortOrder);
var
  R: SizeInt;
begin
  R := System.High(A);
  if R > 0 then
    if CountRun2Asc(A, 0, R) < R then
      begin
        DoQSort(A, 0, R);
        if o = soDesc then
          Reverse(A);
      end
    else
      if (o = soDesc) and (A[0] <> A[R]) then
        Reverse(A);
end;

class procedure TGSimpleArrayHelper.IntroSort(var A: array of T; o: TSortOrder);
var
  R: SizeInt;
begin
  R := System.High(A);
  if R > 0 then
    if CountRun2Asc(A, 0, R) < R then
      begin
        DoIntroSort(A, 0, R, Pred(LGUtils.NSB(R + 1)) * INTRO_LOG_FACTOR);
        if o = soDesc then
          Reverse(A);
      end
    else
      if (o = soDesc) and (A[0] <> A[R]) then
        Reverse(A);
end;

class procedure TGSimpleArrayHelper.DualPivotQuickSort(var A: array of T; o: TSortOrder);
var
  R: SizeInt;
begin
  R := System.High(A);
  if R > 0 then
    if CountRun2Asc(A, 0, R) < R then
      begin
        DoDPQSort(A, 0, R);
        if o = soDesc then
          Reverse(A);
      end
    else
      if (o = soDesc) and (A[0] <> A[R]) then
        Reverse(A);
end;

class procedure TGSimpleArrayHelper.Sort(var A: array of T; o: TSortOrder);
begin
  IntroSort(A, o);
end;

class function TGSimpleArrayHelper.Sorted(constref A: array of T; o: TSortOrder): TArray;
begin
  Result := CreateCopy(A);
  Sort(Result, o);
end;

class function TGSimpleArrayHelper.SelectDistinct(constref A: array of T): TArray;
var
  I, J, Hi: SizeInt;
begin
  Result := Sorted(A);
  Hi := System.High(Result);
  if Hi < 1 then
    exit;
  I := 0;
  for J := 1 to Hi do
    begin
      if Result[I] = Result[J] then
        continue;
      Inc(I);
      if J > I then
        Result[I] := Result[J];
    end;
  System.SetLength(Result, Succ(I));
end;

{ TGNumArrayHelper.TRange }

constructor TGNumArrayHelper.TRange.Create(aFirst, aLast, aStep: T);
begin
  FCurrent := aFirst;
  FLast := aLast;
  FStep := aStep;
  FInLoop := False;
end;

function TGNumArrayHelper.TRange.GetEnumerator: TRange;
begin
  Result := Self;
end;

function TGNumArrayHelper.TRange.MoveNext: Boolean;
begin
  if FInLoop then
    begin
      Result := FLast - FCurrent >= FStep;
      if Result then
        FCurrent += FStep;
    end
  else
    begin
      Result := (FCurrent <= FLast) and (FStep > T(0));
      FInLoop := True;
    end;
end;

{ TGNumArrayHelper.TDownRange }

constructor TGNumArrayHelper.TDownRange.Create(aFirst, aLast, aStep: T);
begin
  FCurrent := aFirst;
  FLast := aLast;
  FStep := aStep;
  FInLoop := False;
end;

function TGNumArrayHelper.TDownRange.GetEnumerator: TDownRange;
begin
  Result := Self;
end;

function TGNumArrayHelper.TDownRange.MoveNext: Boolean;
begin
  if FInLoop then
    begin
      Result := FCurrent - FLast >= FStep;
      if Result then
        FCurrent -= FStep;
    end
  else
    begin
      Result := (FCurrent >= FLast) and (FStep > T(0));
      FInLoop := True;
    end;
end;

class function TGNumArrayHelper.Range(aFrom, aTo: T; aStep: T): TRange;
begin
  Result := TRange.Create(aFrom, aTo, aStep);
end;

class function TGNumArrayHelper.DownRange(aFrom, aDownTo: T; aStep: T): TDownRange;
begin
  Result := TDownRange.Create(aFrom, aDownTo, aStep);
end;

{ TGOrdinalArrayHelper }

class procedure TGOrdinalArrayHelper.CountSort(var A: array of T; aMinValue, aMaxValue: T);
var
  I, J: SizeInt;
  v: T;
  Counts: array of SizeInt;
begin
  System.SetLength(Counts, Succ(aMaxValue - aMinValue));
  System.FillChar(Counts[0], Succ(aMaxValue - aMinValue) * SizeOf(SizeInt), 0);

  for J := 0 to System.High(A) do
    Inc(Counts[A[J] - aMinValue]);

  J := High(A);
  for I := aMaxValue - aMinValue downto 0 do
    if Counts[I] > 0 then
      begin
        v := T(I + aMinValue);
        repeat
          A[J] := v;
          Dec(Counts[I]);
          Dec(J);
        until Counts[I] = 0;
      end;
end;

class function TGOrdinalArrayHelper.Scan(var A: array of T; out aMinValue, aMaxValue: T): TMonotonicity;
var
  I, R: SizeInt;
begin
  R := System.High(A);
  Result := moConst;
  I := 0;
  aMinValue := A[0];
  aMaxValue := A[0];
  while (I < R) and (A[I] = A[Succ(I)]) do
    Inc(I);
  if I < R then
    begin
      Inc(I);
      if A[Pred(I)] < A[I] then  // ascending
        begin
          Result := moAsc;
          while (I < R) and (A[I] <= A[Succ(I)]) do
            begin
              if A[I] > aMaxValue then
                aMaxValue := A[I];
              Inc(I);
            end;
        end
      else                      // descending
        begin
          Result := moDesc;
          while (I < R) and (A[I] >= A[Succ(I)]) do
            begin
              if A[I] < aMinValue then
                aMinValue := A[I];
              Inc(I);
            end;
        end;
    end;
  if I < R then
    begin
      Result := moNone;
      repeat
        if A[I] < aMinValue then
          aMinValue := A[I]
        else
          if A[I] > aMaxValue then
            aMaxValue := A[I];
        Inc(I);
      until I > R;
    end;
end;

class function TGOrdinalArrayHelper.CreateRange(aFrom, aTo: T): TArray;
var
  I: T;
  J: SizeInt = 0;
begin
  System.SetLength(Result, Succ(aTo - aFrom));
  for I := aFrom to aTo do
    begin
      Result[J] := I;
      Inc(J);
    end;
end;

class procedure TGOrdinalArrayHelper.Sort(var A: array of T; aOrder: TSortOrder);
var
  R: SizeInt;
  vMin, vMax: T;
  Mono: TMonotonicity;
  Len: Int64;
begin
  R := System.High(A);
  if R > 0 then
    begin
      Mono := Scan(A, vMin, vMax);
      if Mono < moNone then
        begin
          if (Mono <> moConst) and (Ord(Mono) <> Ord(aOrder)) then
            Reverse(A);
        end
      else
        begin
          Len := Int64(vMax) - Int64(vMin);
          if (Len <= COUNTSORT_CUTOFF) and (Len shr 3 <= Succ(R)) then //todo: any tuning needed
            CountSort(A, vMin, vMax)
          else
            DoIntroSort(A, 0, R, Pred(LGUtils.NSB(R + 1)) * INTRO_LOG_FACTOR);
          if aOrder = soDesc then
            Reverse(A);
        end;
    end;
end;

class function TGOrdinalArrayHelper.Sorted(constref A: array of T; o: TSortOrder): TArray;
begin
  Result := CreateCopy(A);
  Sort(Result, o);
end;

end.

