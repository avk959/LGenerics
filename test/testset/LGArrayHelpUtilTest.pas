unit LGArrayHelpUtilTest;

{$mode objfpc}{$H+}
{$MODESWITCH NESTEDPROCVARS}

interface

uses

  SysUtils, fpcunit, testregistry,
  LGUtils,
  LGArrayHelpers;

type

  { TArrayHelpUtilTest }

  TArrayHelpUtilTest = class(TTestCase)
  private
  type

    TIntHelper    = specialize TGArrayHelpUtil<Integer>;
    TStrHelper    = specialize TGArrayHelpUtil<string>;
    TCompHelper   = specialize TGComparableArrayHelper<Integer>;
    TCmpStrHelper = specialize TGComparableArrayHelper<string>;
    TIntArray     = specialize TGArray<Integer>;

  const
    ReverseIntSrc10: array[2..11] of Integer = (-1, -29, 0, 13, 1, -199, 21, 117, -15, 10);
    MergedIntSrc: array[1..14] of Integer    = (101, 0, -11, -27, 127, 1, 0, 10, -15, 117, 21, -199, 1, 13);
  published
    procedure SwapItems;
    procedure SwapItems1;
    procedure CreateCopyOfEmpty;
    procedure CreateCopyOfStatic10;
    procedure CreateCopyOfDyn10;
    procedure CreateCopyOfStatic1;
    procedure CreateCopyOfStatic7;
    procedure CreateCopyOfDyn7;

    procedure CreateReverseCopyOfEmpty;
    procedure CreateReverseCopyOfStatic1;
    procedure CreateReverseCopyOfStatic10;
    procedure CreateReverseCopyOfDyn10;
    procedure CreateReverseCopyOfStatic7;
    procedure CreateReverseCopyOfDyn7;

    procedure CreateMergeCopyOfBothEmpty;
    procedure CreateMergeCopyOfLEmptyRStatic7;
    procedure CreateMergeCopyOfLStatic7REmpty;
    procedure CreateMergeCopyOfLEmptyRDyn7;
    procedure CreateMergeCopyOfLDyn7REmpty;
    procedure CreateMergeCopyOfLStatic7RStatic7;
    procedure CreateMergeCopyOfLStatic7RDyn7;
    procedure CreateMergeCopyOfLDyn7RStatic7;
    procedure CreateMergeCopyOfLDyn7RDyn7;

    procedure Resize;
    procedure CreateAndFill;
    procedure Fill;

    procedure AppendToEmpty;
    procedure Append;
    procedure CopyFrom;

    procedure MergeOfBothEmpty;
    procedure MergeOfDstEmptySrc7;
    procedure MergeOfDst7SrcEmpty;
    procedure MergeOfDst7Src7;
    procedure MergeOfSelf;

    procedure ConcatEmpty;
    procedure ConcatSomeEmpty;
    procedure Concat1;
    procedure Concat3;
    procedure Concat4;
    procedure Concat5;
    procedure ConcatStr;

    procedure SplitEmpty;
    procedure SplitWrongIndex;
    procedure SplitNegIndex;
    procedure Split;
    procedure SplitFromFirst;
    procedure SplitFromLast;

    procedure ExtractEmpty;
    procedure ExtractWrongIndex;
    procedure ExtractZeroCount;
    procedure Extract5;
    procedure Extract12;
    procedure ExtractFromFirst;
    procedure ExtractFromLast;

    procedure ReverseEmpty;
    procedure ReverseStatic1;
    procedure ReverseDyn1;
    procedure ReverseStatic2;
    procedure ReverseDyn2;
    procedure ReverseStatic10;
    procedure ReverseDyn10;

    procedure ReverseOrder;

    procedure RotateIllegal;
    procedure RotateStatic;
    procedure RotateDyn;

    procedure RandomShuffleEmpty;
    procedure RandomShuffleStatic1;
    procedure RandomShuffleDyn1;
    procedure RandomShuffleDyn10;
    procedure RandomShuffleStatic10;
  end;

  { TSegmentTreeTest }

  TSegmentTreeTest = class(TTestCase)
  private
  type
    TIntAdd     = specialize TGAddMonoid<Integer>;
    TIntMax     = specialize TGMaxMonoid<Integer>;
    TIntMin     = specialize TGMinMonoid<Integer>;

    TIntAddTree = specialize TGSegmentTree<Integer, TIntAdd>;
    TIntMaxTree = specialize TGSegmentTree<Integer, TIntMax>;
    TIntMinTree = specialize TGSegmentTree<Integer, TIntMin>;

    TMaxPos     = specialize TGMaxPos<Integer>;
    TMaxPosTree = specialize TGSegmentTree<TMaxPos, TMaxPos>;

    TMinPos     = specialize TGMinPos<Integer>;
    TMinPosTree = specialize TGSegmentTree<TMinPos, TMinPos>;

  published
    procedure CreateTree;
    procedure Items;
    procedure AddQuery;
    procedure MaxQuery;
    procedure MaxQuery2;
    procedure MinQuery;
    procedure MinQuery2;
  end;

implementation
{$B-}{$COPERATORS ON}

const

  IntSrc10: array[2..11] of Integer = (10, -15, 117, 21, -199, 1, 13, 0, -29, -1);

  IntSrc7: array[0..6] of Integer = (101, 0, -11, -27, 127, 1, 0);

{ TArrayHelpUtilTest }

procedure TArrayHelpUtilTest.SwapItems;
var
  a: TIntArray = nil;
  Raised: Boolean = False;
begin
  a := [1, 2, 3];
  try
    TIntHelper.SwapItems(a, 0, 3);
  except
    Raised := True;
  end;
  AssertTrue(Raised);
  Raised := False;

  try
    TIntHelper.SwapItems(a, 4, 1);
  except
    Raised := True;
  end;
  AssertTrue(Raised);
end;

procedure TArrayHelpUtilTest.SwapItems1;
var
  a: TIntArray = nil;
begin
  a := [1, 2, 3, 4, 5];
  TIntHelper.SwapItems(a, 0, 4);
  AssertTrue(a[0] = 5);
  AssertTrue(a[4] = 1);
  TIntHelper.SwapItems(a, 1, 3);
  AssertTrue(a[1] = 4);
  AssertTrue(a[3] = 2);
end;

procedure TArrayHelpUtilTest.CreateCopyOfEmpty;
var
  a, src: TIntArray;
begin
  src := nil;
  a := TIntHelper.CreateCopy(src);
  AssertTrue(a = nil);
end;

procedure TArrayHelpUtilTest.CreateCopyOfStatic10;
var
  a: TIntArray;
begin
  a := TIntHelper.CreateCopy(IntSrc10);
  AssertTrue(TCompHelper.Same(IntSrc10, a));
end;

procedure TArrayHelpUtilTest.CreateCopyOfDyn10;
var
  a, src: TIntArray;
begin
  src := TIntArray.Create(10, -15, 117, 21, -199, 1, 13, 0, -29, -1);
  a := TIntHelper.CreateCopy(src);
  AssertTrue(TCompHelper.Same(src, a));
end;

procedure TArrayHelpUtilTest.CreateCopyOfStatic1;
var
  a: TIntArray;
begin
  a := TIntHelper.CreateCopy(IntSrc10[3..3]);
  AssertTrue(TCompHelper.Same(IntSrc10[3..3], a));
end;

procedure TArrayHelpUtilTest.CreateCopyOfStatic7;
var
  a: TIntArray;
begin
  a := TIntHelper.CreateCopy(IntSrc10[4..10]);
  AssertTrue(TCompHelper.Same(IntSrc10[4..10], a));
end;

procedure TArrayHelpUtilTest.CreateCopyOfDyn7;
var
  a, src: TIntArray;
begin
  src := TIntArray.Create(10, -15, 117, 21, -199, 1, 13, 0, -29, -1);
  a := TIntHelper.CreateCopy(src[2..8]);
  AssertTrue(TCompHelper.Same(src[2..8], a));
end;

procedure TArrayHelpUtilTest.CreateReverseCopyOfEmpty;
var
  a: TIntArray;
begin
  AssertTrue(TIntHelper.CreateReverseCopy(a{%H-}) = nil);
end;

procedure TArrayHelpUtilTest.CreateReverseCopyOfStatic1;
begin
  AssertTrue(TCompHelper.Same(IntSrc10[3..3], TIntHelper.CreateReverseCopy(IntSrc10[3..3])));
end;

procedure TArrayHelpUtilTest.CreateReverseCopyOfStatic10;
begin
  AssertTrue(TCompHelper.Same(ReverseIntSrc10, TIntHelper.CreateReverseCopy(IntSrc10)));
end;

procedure TArrayHelpUtilTest.CreateReverseCopyOfDyn10;
var
  a: TIntArray;
begin
  a := TIntHelper.CreateReverseCopy(TIntHelper.CreateCopy(IntSrc10));
  AssertTrue(TCompHelper.Same(ReverseIntSrc10, a));
end;

procedure TArrayHelpUtilTest.CreateReverseCopyOfStatic7;
var
  a: TIntArray;
begin
  a := TIntHelper.CreateReverseCopy(IntSrc10[4..10]);
  AssertTrue(TCompHelper.Same(ReverseIntSrc10[3..9], a));
end;

procedure TArrayHelpUtilTest.CreateReverseCopyOfDyn7;
var
  a: TIntArray;
begin
  a := TIntHelper.CreateReverseCopy(TIntHelper.CreateCopy(IntSrc10[4..10]));
  AssertTrue(TCompHelper.Same(ReverseIntSrc10[3..9], a));
end;

procedure TArrayHelpUtilTest.CreateMergeCopyOfBothEmpty;
var
  a, L, R: TIntArray;
begin
  a := TIntHelper.CreateMerge(L{%H-}, R{%H-});
  AssertTrue(a = nil);
end;

procedure TArrayHelpUtilTest.CreateMergeCopyOfLEmptyRStatic7;
var
  a, L: TIntArray;
begin
  a := TIntHelper.CreateMerge(L{%H-}, IntSrc7);
  AssertTrue(TCompHelper.Same(IntSrc7, a));
end;

procedure TArrayHelpUtilTest.CreateMergeCopyOfLStatic7REmpty;
var
  a, R: TIntArray;
begin
  a := TIntHelper.CreateMerge(IntSrc7, R{%H-});
  AssertTrue(TCompHelper.Same(IntSrc7, a));
end;

procedure TArrayHelpUtilTest.CreateMergeCopyOfLEmptyRDyn7;
var
  a, L: TIntArray;
begin
  a := TIntHelper.CreateMerge(L{%H-}, TIntHelper.CreateCopy(IntSrc7));
  AssertTrue(TCompHelper.Same(IntSrc7, a));
end;

procedure TArrayHelpUtilTest.CreateMergeCopyOfLDyn7REmpty;
var
  a, R: TIntArray;
begin
  a := TIntHelper.CreateMerge(TIntHelper.CreateCopy(IntSrc7), R{%H-});
  AssertTrue(TCompHelper.Same(IntSrc7, a));
end;

procedure TArrayHelpUtilTest.CreateMergeCopyOfLStatic7RStatic7;
var
  a: TIntArray;
begin
  a := TIntHelper.CreateMerge(IntSrc7, IntSrc10[2..8]);
  AssertTrue(TCompHelper.Same(MergedIntSrc, a));
end;

procedure TArrayHelpUtilTest.CreateMergeCopyOfLStatic7RDyn7;
var
  a: TIntArray;
begin
  a := TIntHelper.CreateMerge(IntSrc7, TIntHelper.CreateCopy(IntSrc10[2..8]));
  AssertTrue(TCompHelper.Same(MergedIntSrc, a));
end;

procedure TArrayHelpUtilTest.CreateMergeCopyOfLDyn7RStatic7;
var
  a: TIntArray;
begin
  a := TIntHelper.CreateMerge(TIntHelper.CreateCopy(IntSrc7), IntSrc10[2..8]);
  AssertTrue(TCompHelper.Same(MergedIntSrc, a));
end;

procedure TArrayHelpUtilTest.CreateMergeCopyOfLDyn7RDyn7;
var
  a: TIntArray;
begin
  a := TIntHelper.CreateMerge(TIntHelper.CreateCopy(IntSrc7), TIntHelper.CreateCopy(IntSrc10[2..8]));
  AssertTrue(TCompHelper.Same(MergedIntSrc, a));
end;

procedure TArrayHelpUtilTest.Resize;
var
  a: TIntArray = nil;
  b: TIntArray;
begin
  b := TIntHelper.Resize(a, 0);
  AssertTrue(b = nil);
  b := TIntHelper.Resize(a, 125);
  AssertTrue(Pointer(b) = Pointer(a));
  b := TIntHelper.Resize(a, 225);
  AssertTrue(Pointer(b) = Pointer(a));
  b := TIntHelper.Resize(a, 0);
  AssertTrue(b = nil);
end;

procedure TArrayHelpUtilTest.CreateAndFill;
var
  a: TIntArray;
  I: Integer;
begin
  a := TIntHelper.CreateAndFill(5, -5);
  AssertTrue(a = nil);
  a := TIntHelper.CreateAndFill(5, 0);
  AssertTrue(a = nil);
  a := TIntHelper.CreateAndFill(5, 1);
  AssertTrue(Length(a) = 1);
  AssertTrue(a[0] = 5);
  a := TIntHelper.CreateAndFill(-5, 11);
  AssertTrue(Length(a) = 11);
  for I := 0 to 10 do
    AssertTrue(a[I] = -5);
end;

procedure TArrayHelpUtilTest.Fill;
var
  a: TIntArray = nil;
  I: Integer;
begin
  TIntHelper.Fill(a, -3);
  AssertTrue(a = nil);
  SetLength(a, 100);
  TIntHelper.Fill(a, -3);
  for I := 0 to High(a) do
    AssertTrue(a[I] = -3);
end;

procedure TArrayHelpUtilTest.AppendToEmpty;
var
  a: TIntArray;
begin
  TIntHelper.Append(a{%H-}, 10);
  AssertTrue(System.Length(a) = 1);
  AssertTrue(a[0] = 10);
end;

procedure TArrayHelpUtilTest.Append;
var
  a: TIntArray;
  I: Integer;
begin
  a := TIntArray.Create(0,1,2,3);
  TIntHelper.Append(a, 4);
  TIntHelper.Append(a, 5);
  TIntHelper.Append(a, 6);
  TIntHelper.Append(a, 7);
  AssertTrue(System.Length(a) = 8);
  for I := 0 to 7 do
    AssertTrue(a[I] = I);
end;

procedure TArrayHelpUtilTest.CopyFrom;
  function Eq(const L, R: Integer): Boolean; begin Eq := L = R end;
var
  a, b: TIntArray;
  I, cnt: Integer;
begin
  a := nil;
  b := TIntArray.Create(0,1,2,3,4,5);
  cnt := TIntHelper.CopyFrom(b, a);
  AssertTrue(a = nil);
  AssertTrue(cnt = 0);

  a := TIntArray.Create(0,1,2,3,4,5);
  b := nil;
  TIntHelper.CopyFrom(b, a);
  AssertTrue(Length(a) = 6);
  for I := 0 to High(a) do
    AssertTrue(a[I] = I);
  AssertTrue(cnt = 0);

  b := [6, 7, 8];
  cnt := TIntHelper.CopyFrom(b, a[0..3]);
  AssertTrue(Length(a) = 6);
  AssertTrue(cnt = 3);
  for I := 0 to High(b) do
    AssertTrue(a[I] = b[I]);
  for I := Length(b) to High(a) do
    AssertTrue(a[I] = I);

  b := [0,1,2,8,9];
  cnt := TIntHelper.CopyFrom(b[2..4], a[0..2]);
  AssertTrue(Length(a) = 6);
  AssertTrue(cnt = 3);
  AssertTrue(TIntHelper.Same(b[2..4], a[0..2], @Eq));
  for I := 3 to High(a) do
    AssertTrue(a[I] = I);

  cnt := TIntHelper.CopyFrom(a[4..6], a[0..2]);
  AssertTrue(Length(a) = 6);
  AssertTrue(cnt = 3);
  AssertTrue(TIntHelper.Same(a[4..6], a[0..2], @Eq));
  for I := 3 to High(a) do
    AssertTrue(a[I] = I);

  a := TIntArray.Create(0,1,2,3,4,5);
  cnt := TIntHelper.CopyFrom(a[0..2], a[4..6]);
  AssertTrue(Length(a) = 6);
  AssertTrue(cnt = 3);
  AssertTrue(TIntHelper.Same(a[4..6], a[0..2], @Eq));
  for I := 0 to 3 do
    AssertTrue(a[I] = I);
end;

procedure TArrayHelpUtilTest.MergeOfBothEmpty;
var
  Dst, Src: TIntArray;
  MergedCount: SizeInt;
begin
  MergedCount := TIntHelper.Merge(Dst{%H-}, Src{%H-});
  AssertTrue(MergedCount = 0);
  AssertTrue(Dst = nil);
  AssertTrue(Src = nil);
end;

procedure TArrayHelpUtilTest.MergeOfDstEmptySrc7;
var
  Dst, Src: TIntArray;
begin
  Src := TIntHelper.CreateCopy(IntSrc7);
  AssertTrue( TIntHelper.Merge(Dst{%H-}, Src) = 7);
  AssertTrue(TCompHelper.Same(IntSrc7, Dst));
  AssertTrue(Src = nil);
end;

procedure TArrayHelpUtilTest.MergeOfDst7SrcEmpty;
var
  Dst, Src: TIntArray;
begin
  Dst := TIntHelper.CreateCopy(IntSrc7);
  AssertTrue( TIntHelper.Merge(Dst, Src{%H-}) = 0);
  AssertTrue(TCompHelper.Same(IntSrc7, Dst));
  AssertTrue(Src = nil);
end;

procedure TArrayHelpUtilTest.MergeOfDst7Src7;
var
  Dst, Src: TIntArray;
begin
  Dst := TIntHelper.CreateCopy(IntSrc7);
  Src := TIntHelper.CreateCopy(IntSrc10[2..8]);
  AssertTrue(TIntHelper.Merge(Dst, Src) = 7);
  AssertTrue(TCompHelper.Same(MergedIntSrc, Dst));
  AssertTrue(Src = nil);
end;

procedure TArrayHelpUtilTest.MergeOfSelf;//
var
  a: TIntArray;
begin
  a := TIntHelper.CreateCopy(IntSrc10);
  AssertTrue(TIntHelper.Merge(a, a) = 0);
  AssertTrue(System.Length(a) = 10);
end;

procedure TArrayHelpUtilTest.ConcatEmpty;
var
  a: array of TIntArray = nil;
  r: TIntArray;
begin
  r := TIntHelper.Concat(a);
  AssertTrue(r = nil);
end;

procedure TArrayHelpUtilTest.ConcatSomeEmpty;
var
  r: TIntArray;
begin
  r := TIntHelper.Concat([nil, nil, nil, nil]);
  AssertTrue(r = nil);
end;

procedure TArrayHelpUtilTest.Concat1;
var
  r: TIntArray;
begin
  r := TIntHelper.Concat([[1,2,3,4,5,6]]);
  AssertTrue(Length(r) = 6);
  AssertTrue(TCompHelper.Same(r, [1,2,3,4,5,6]));
end;

procedure TArrayHelpUtilTest.Concat3;
var
  r: TIntArray;
begin
  r := TIntHelper.Concat([nil, [1,2,3], [4,5,6]]);
  AssertTrue(Length(r) = 6);
  AssertTrue(TCompHelper.Same(r, [1,2,3,4,5,6]));
end;

procedure TArrayHelpUtilTest.Concat4;
var
  r: TIntArray;
begin
  r := TIntHelper.Concat([nil, [1,2,3], [4,5,6], nil]);
  AssertTrue(Length(r) = 6);
  AssertTrue(TCompHelper.Same(r, [1,2,3,4,5,6]));
end;

procedure TArrayHelpUtilTest.Concat5;
var
  r: TIntArray;
begin
  r := TIntHelper.Concat([nil, [1,2,3], nil, [4,5,6], nil]);
  AssertTrue(Length(r) = 6);
  AssertTrue(TCompHelper.Same(r, [1,2,3,4,5,6]));
end;

procedure TArrayHelpUtilTest.ConcatStr;
var
  r: array of string;
begin
  r := TStrHelper.Concat([nil, ['aaa','bbb','ccc'], nil, ['ddd','eee','ggg'], nil]);
  AssertTrue(Length(r) = 6);
  AssertTrue(TCmpStrHelper.Same(r, ['aaa','bbb','ccc','ddd','eee','ggg']));
end;

procedure TArrayHelpUtilTest.SplitEmpty;
var
  a, r: TIntArray;
begin
  r := TIntHelper.Split(a{%H-}, 0);
  AssertTrue(r = nil);
end;

procedure TArrayHelpUtilTest.SplitWrongIndex;
var
  a, r: TIntArray;
begin
  a := TIntHelper.CreateCopy(IntSrc7);
  r := TIntHelper.Split(a, 7);
  AssertTrue(r = nil);
end;

procedure TArrayHelpUtilTest.SplitNegIndex;
var
  a, r: TIntArray;
begin
  a := TIntHelper.CreateCopy(IntSrc7);
  r := TIntHelper.Split(a, -1);
  AssertTrue(a = nil);
  AssertTrue(TCompHelper.Same(IntSrc7, r));
end;

procedure TArrayHelpUtilTest.Split;
var
  a, b: TIntArray;
begin
  a := TIntHelper.CreateCopy(IntSrc10);
  b := TIntHelper.Split(a, 6);
  AssertTrue(TCompHelper.Same(IntSrc10[2..7], a));
  AssertTrue(TCompHelper.Same(IntSrc10[8..11], b));
end;

procedure TArrayHelpUtilTest.SplitFromFirst;
var
  a, b: TIntArray;
begin
  a := TIntHelper.CreateCopy(IntSrc10);
  b := TIntHelper.Split(a, 0);
  AssertTrue(a = nil);
  AssertTrue(TCompHelper.Same(IntSrc10, b));
end;

procedure TArrayHelpUtilTest.SplitFromLast;
var
  a, b: TIntArray;
begin
  a := TIntHelper.CreateCopy(IntSrc10);
  b := TIntHelper.Split(a, 9);
  AssertTrue(System.Length(a) = 9);
  AssertTrue(TCompHelper.Same(IntSrc10[2..10], a));
  AssertTrue(TCompHelper.Same(IntSrc10[11..11], b));
end;

procedure TArrayHelpUtilTest.ExtractEmpty;
var
  a, r: TIntArray;
begin
  r := TIntHelper.Extract(a{%H-}, 0, 0);
  AssertTrue(r = nil);
end;

procedure TArrayHelpUtilTest.ExtractWrongIndex;
var
  a, r: TIntArray;
begin
  a := TIntHelper.CreateCopy(IntSrc7);
  r := TIntHelper.Extract(a, 7, 5);
  AssertTrue(r = nil);
end;

procedure TArrayHelpUtilTest.ExtractZeroCount;
var
  a, b: TIntArray;
begin
  a := TIntHelper.CreateCopy(IntSrc10);
  b := TIntHelper.Extract(a, 6, 0);
  AssertTrue(TCompHelper.Same(IntSrc10, a));
  AssertTrue(b = nil);
end;

procedure TArrayHelpUtilTest.Extract5;
const
  t: array[0..4] of Integer = (10, -15, 117, -29, -1);
  r: array[0..4] of Integer = (21, -199, 1, 13, 0);
var
  a, b: TIntArray;
begin
  a := TIntHelper.CreateCopy(IntSrc10);
  b := TIntHelper.Extract(a, 3, 5);
  AssertTrue(System.Length(a) = 5);
  AssertTrue(TCompHelper.Same(t, a));
  AssertTrue(TCompHelper.Same(r, b));
end;

procedure TArrayHelpUtilTest.Extract12;
var
  a, b: TIntArray;
begin
  a := TIntHelper.CreateCopy(IntSrc10);
  b := TIntHelper.Extract(a, 3, 12);
  AssertTrue(System.Length(a) = 3);
  AssertTrue(TCompHelper.Same(IntSrc10[2..4], a));
  AssertTrue(TCompHelper.Same(IntSrc10[5..11], b));
end;

procedure TArrayHelpUtilTest.ExtractFromFirst;
var
  a, b: TIntArray;
begin
  a := TIntHelper.CreateCopy(IntSrc10);
  b := TIntHelper.Extract(a, 0, 24);
  AssertTrue(a = nil);
  AssertTrue(TCompHelper.Same(IntSrc10, b));
end;

procedure TArrayHelpUtilTest.ExtractFromLast;
var
  a, b: TIntArray;
begin
  a := TIntHelper.CreateCopy(IntSrc10);
  b := TIntHelper.Extract(a, 9, 12);
  AssertTrue(TCompHelper.Same(IntSrc10[2..10], a));
  AssertTrue(TCompHelper.Same(IntSrc10[11..11], b));
end;

procedure TArrayHelpUtilTest.ReverseEmpty;
var
  a: TIntArray;
begin
  TIntHelper.Reverse(a{%H-});
  AssertTrue(a = nil);
end;

procedure TArrayHelpUtilTest.ReverseStatic1;
type
  TArray = array[0..0] of Integer;
const
  a: TArray = (10);
var
  b: TArray;
begin
  b := a;
  TIntHelper.Reverse(b);
  AssertTrue(TCompHelper.Same(b, a));
end;

procedure TArrayHelpUtilTest.ReverseDyn1;
const
  a: array[0..0] of Integer = (10);
var
  b: TIntArray;
begin
  b := TIntHelper.CreateCopy(a);
  TIntHelper.Reverse(b);
  AssertTrue(TCompHelper.Same(b, a));
end;

procedure TArrayHelpUtilTest.ReverseStatic2;
type
  TArray = array[0..1] of Integer;
const
  r: TArray = (-30, 10);
var
  a: TArray = (10, -30);
begin
  TIntHelper.Reverse(a);
  AssertTrue(TCompHelper.Same(r, a));
end;

procedure TArrayHelpUtilTest.ReverseDyn2;
const
  r: array[0..1] of Integer = (-30, 10);
var
  a: TIntArray;
begin
  a := TIntArray.Create(10, -30);
  TIntHelper.Reverse(a);
  AssertTrue(TCompHelper.Same(r, a));
end;

procedure TArrayHelpUtilTest.ReverseStatic10;
var
  a: array[2..11] of Integer;
begin
  a := IntSrc10;
  TIntHelper.Reverse(a);
  AssertTrue(TCompHelper.Same(ReverseIntSrc10, a));
end;

procedure TArrayHelpUtilTest.ReverseDyn10;
var
  a: TIntArray;
begin
  a := TIntHelper.CreateCopy(IntSrc10);
  TIntHelper.Reverse(a);
  AssertTrue(TCompHelper.Same(ReverseIntSrc10, a));
end;

procedure TArrayHelpUtilTest.ReverseOrder;
var
  a: TIntArray = nil;
  b: array[-1..5] of Integer = (-1, 0, 1, 2, 3, 4, 5);
  I, J, Count: Integer;
begin
  Count := 0;
  for I in TIntHelper.ReverseOrder(a) do
    Inc(Count);
  AssertTrue(Count = 0);

  J := High(b);
  for I in TIntHelper.ReverseOrder(b) do
    begin
      AssertTrue(I = b[J]);
      Dec(J);
      Inc(Count);
    end;
  AssertTrue(Count = Length(b));

  a := [0, 1, 2, 3, 4, 5];
  Count := 0;
  J := High(a);
  for I in TIntHelper.ReverseOrder(a) do
    begin
      AssertTrue(I = a[J]);
      Dec(J);
      Inc(Count);
    end;
  AssertTrue(Count = Length(a));
end;

procedure TArrayHelpUtilTest.RotateIllegal;
type
  TIntArray10 = array[1..10] of Integer;
const
  Sample: TIntArray10 = (0, 1, 2, 3, 4, 5, 6, 7, 8, 9);
var
  a: TIntArray10;
begin
  a := Sample;
  TCompHelper.RotateLeft(a, 0);
  AssertTrue(TCompHelper.Same(a, Sample));
  TCompHelper.RotateLeft(a, 10);
  AssertTrue(TCompHelper.Same(a, Sample));
  TCompHelper.RotateLeft(a, -10);
  AssertTrue(TCompHelper.Same(a, Sample));
  TCompHelper.RotateRight(a, 10);
  AssertTrue(TCompHelper.Same(a, Sample));
  TCompHelper.RotateRight(a, -10);
  AssertTrue(TCompHelper.Same(a, Sample));
end;

procedure TArrayHelpUtilTest.RotateStatic;
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
  TCompHelper.RotateLeft(a, 4);
  AssertTrue(TCompHelper.Same(a, RolSample));
  a := Sample;
  TCompHelper.RotateLeft(a, -4);
  AssertTrue(TCompHelper.Same(a, RorSample));
  a := Sample;
  TCompHelper.RotateRight(a, 4);
  AssertTrue(TCompHelper.Same(a, RorSample));
  a := Sample;
  TCompHelper.RotateRight(a, -4);
  AssertTrue(TCompHelper.Same(a, RolSample));
  TCompHelper.RotateLeft(a, -4);
  AssertTrue(TCompHelper.Same(a, Sample));
end;

procedure TArrayHelpUtilTest.RotateDyn;
type
  TIntArray10 = array[1..10] of Integer;
const
  Sample: TIntArray10 = (0, 1, 2, 3, 4, 5, 6, 7, 8, 9);
  RolSample: TIntArray10 = (4, 5, 6, 7, 8, 9, 0, 1, 2, 3);
  RorSample: TIntArray10 = (6, 7, 8, 9, 0, 1, 2, 3, 4, 5);
var
  a: TIntArray;
begin
  a := TCompHelper.CreateCopy(Sample);
  TCompHelper.RotateLeft(a, 4);
  AssertTrue(TCompHelper.Same(a, RolSample));
  a := TCompHelper.CreateCopy(Sample);
  TCompHelper.RotateLeft(a, -4);
  AssertTrue(TCompHelper.Same(a, RorSample));
  a := TCompHelper.CreateCopy(Sample);
  TCompHelper.RotateRight(a, 4);
  AssertTrue(TCompHelper.Same(a, RorSample));
  a := TCompHelper.CreateCopy(Sample);
  TCompHelper.RotateRight(a, -4);
  AssertTrue(TCompHelper.Same(a, RolSample));
  TCompHelper.RotateLeft(a, -4);
  AssertTrue(TCompHelper.Same(a, Sample));
end;

procedure TArrayHelpUtilTest.RandomShuffleEmpty;
var
  a: TIntArray;
begin
  TIntHelper.RandomShuffle(a{%H-});
  AssertTrue(a = nil);
end;

procedure TArrayHelpUtilTest.RandomShuffleStatic1;
type
  TArray = array[0..0] of Integer;
const
  r: TArray = (10);
var
  a: TArray;
begin
  a := r;
  TIntHelper.RandomShuffle(a);
  AssertTrue(TCompHelper.Same(r, a));
end;

procedure TArrayHelpUtilTest.RandomShuffleDyn1;
const
  r: array[0..0] of Integer = (10);
var
  a: TIntArray;
begin
  a := TIntHelper.CreateCopy(r);
  TIntHelper.RandomShuffle(a);
  AssertTrue(TCompHelper.Same(r, a));
end;

procedure TArrayHelpUtilTest.RandomShuffleStatic10;
var
  a: array[2..11] of Integer;
  v, I: Integer;
  AllPresent: Boolean = True;
  Found: Boolean;
begin
  a := IntSrc10;
  TIntHelper.RandomShuffle(a);
  for v in IntSrc10 do
    begin
      for I := 2 to 11 do
        begin
          Found := a[I] = v;
          if Found then
            break;
        end;
      if not Found then
        begin
          AllPresent := False;
          break;
        end;
    end;
  AssertTrue(AllPresent);
end;

procedure TArrayHelpUtilTest.RandomShuffleDyn10;
var
  a: TIntArray;
  v, I: Integer;
  AllPresent: Boolean = True;
  Found: Boolean;
begin
  a := TIntHelper.CreateCopy(IntSrc10);
  TIntHelper.RandomShuffle(a);
  for v in IntSrc10 do
    begin
      Found := False;
      for I := 0 to System.High(a) do
        begin
          Found := a[I] = v;
          if Found then
            break;
        end;
      if not Found then
        begin
          AllPresent := False;
          break;
        end;
    end;
  AssertTrue(AllPresent);
end;

{ TSegmentTreeTest }

procedure TSegmentTreeTest.CreateTree;
const
  TestSize = 1000;
var
  Tree: TIntAddTree;
  a: array of Integer;
  I: Integer;
begin
  AssertTrue(TIntAddTree.Identity = 0);
  AssertTrue(TIntMaxTree.Identity = Low(Integer));
  AssertTrue(TIntMinTree.Identity = High(Integer));
  SetLength(a, TestSize);
  for I := 0 to High(a) do
    a[I] := Succ(Random(TestSize));
  Tree := TIntAddTree.Create(a);
  AssertTrue(Tree.Count = Length(a));
  for I := 0 to High(a) do
    AssertTrue(a[I] = Tree[I]);
  for I := 0 to Pred(Tree.Count) do
    Tree[I] := Tree[I] + I;
  for I := 0 to High(a) do
    AssertTrue(Tree[I] = a[I] + I);
end;

procedure TSegmentTreeTest.Items;
var
  Tree: TIntAddTree;
  {%H-}I: Integer;
  Raised: Boolean = False;
begin
  Tree := TIntAddTree.Create([2, 3]);
  try
    I := Tree[-1];
  except
    Raised := True;
  end;
  AssertTrue(Raised);

  Raised := False;
  try
    I := Tree[2];
  except
    Raised := True;
  end;
  AssertTrue(Raised);
end;

procedure TSegmentTreeTest.AddQuery;
const
  TestSize = 1000;
var
  Tree: TIntAddTree;
  a: array of Integer;
  I, J, L, R, sum: Integer;
begin
  SetLength(a, TestSize);
  for I := 0 to High(a) do
    a[I] := Succ(Random(TestSize));
  Tree := TIntAddTree.Create(a);
  for I := 1 to TestSize do
    begin
      AssertTrue(Tree.RangeQuery(I - 1, I - 1) = a[I - 1]);
      L := Random(TestSize div 2);
      R := TestSize div 2 + Random(TestSize div 2);
      sum := 0;
      for J := L to R do
        sum += a[J];
      AssertTrue(Tree.RangeQuery(L, R) = sum);
      AssertTrue(Tree.RangeQuery(R, L) = 0);
    end;

  for I := 0 to High(a) do
    begin
      J := Random(TestSize);
      a[I] += J;
      Tree[I] := Tree[I] + J;
    end;
  for I := 1 to TestSize do
    begin
      AssertTrue(Tree.RangeQuery(I - 1, I - 1) = a[I - 1]);
      L := Random(TestSize div 2);
      R := TestSize div 2 + Random(TestSize div 2);
      sum := 0;
      for J := L to R do
        sum += a[J];
      AssertTrue(Tree.RangeQuery(L, R) = sum);
      AssertTrue(Tree.RangeQuery(R, L) = 0);
    end;
end;

procedure TSegmentTreeTest.MaxQuery;
const
  TestSize = 1000;
var
  Tree: TIntMaxTree;
  a: array of Integer;
  I, J, L, R, max: Integer;
begin
  SetLength(a, TestSize);
  for I := 0 to High(a) do
    a[I] := Succ(Random(TestSize));
  Tree := TIntMaxTree.Create(a);
  for I := 1 to TestSize do
    begin
      AssertTrue(Tree.RangeQuery(I - 1, I - 1) = a[I - 1]);
      L := Random(TestSize div 2);
      R := TestSize div 2 + Random(TestSize div 2);
      max := Low(Integer);
      for J := L to R do
        if a[J] > max then
          max := a[J];
      AssertTrue(Tree.RangeQuery(L, R) = max);
      AssertTrue(Tree.RangeQuery(R, L) = Low(Integer));
    end;

  for I := 0 to High(a) do
    begin
      J := Random(TestSize);
      a[I] += J;
      Tree[I] := Tree[I] + J;
    end;
  for I := 1 to TestSize do
    begin
      AssertTrue(Tree.RangeQuery(I - 1, I - 1) = a[I - 1]);
      L := Random(TestSize div 2);
      R := TestSize div 2 + Random(TestSize div 2);
      max := Low(Integer);
      for J := L to R do
        if a[J] > max then
          max := a[J];
      AssertTrue(Tree.RangeQuery(L, R) = max);
      AssertTrue(Tree.RangeQuery(R, L) = Low(Integer));
    end;
end;

procedure TSegmentTreeTest.MaxQuery2;
const
  TestSize = 1000;
var
  Tree: TMaxPosTree;
  a: array of Integer;
  I, J, L, R, max, maxpos: Integer;
  mPos: TMaxPos;
  procedure GetElem(aIndex: SizeInt; out aElem: TMaxPos);
  begin
    aElem.Value := a[aIndex];
    aElem.Index := aIndex;
  end;
begin
  SetLength(a, TestSize);
  for I := 0 to High(a) do
    a[I] := Succ(Random(TestSize));
  Tree := TMaxPosTree.Create(TestSize, @GetElem);
  for I := 1 to TestSize do
    begin
      AssertTrue(Tree.RangeQuery(I - 1, I - 1).Value = a[I - 1]);
      L := Random(TestSize div 2);
      R := TestSize div 2 + Random(TestSize div 2);
      max := Low(Integer);
      maxpos := -1;
      for J := L to R do
        if a[J] > max then
          begin
            max := a[J];
            maxpos := J;
          end;
      mPos := Tree.RangeQuery(L, R);
      AssertTrue(mPos.Value = max);
      AssertTrue(mPos.Index = maxpos);
    end;

  for I := 0 to High(a) do
    begin
      J := Random(TestSize);
      a[I] += J;
      mPos := Tree[I];
      mPos.Value += J;
      Tree[I] := mPos;
    end;
  for I := 1 to TestSize do
    begin
      AssertTrue(Tree.RangeQuery(I - 1, I - 1).Value = a[I - 1]);
      L := Random(TestSize div 2);
      R := TestSize div 2 + Random(TestSize div 2);
      max := Low(Integer);
      maxpos := -1;
      for J := L to R do
        if a[J] > max then
          begin
            max := a[J];
            maxpos := J;
          end;
      mPos := Tree.RangeQuery(L, R);
      AssertTrue(mPos.Value = max);
      AssertTrue(mPos.Index = maxpos);
    end;
end;

procedure TSegmentTreeTest.MinQuery;
const
  TestSize = 1000;
var
  Tree: TIntMinTree;
  a: array of Integer;
  I, J, L, R, min: Integer;
begin
  SetLength(a, TestSize);
  for I := 0 to High(a) do
    a[I] := Succ(Random(TestSize));
  Tree := TIntMinTree.Create(a);
  for I := 1 to TestSize do
    begin
      AssertTrue(Tree.RangeQuery(I - 1, I - 1) = a[I - 1]);
      L := Random(TestSize div 2);
      R := TestSize div 2 + Random(TestSize div 2);
      min := High(Integer);
      for J := L to R do
        if a[J] < min then
          min := a[J];
      AssertTrue(Tree.RangeQuery(L, R) = min);
      AssertTrue(Tree.RangeQuery(R, L) = High(Integer));
    end;

  for I := 0 to High(a) do
    begin
      J := Random(TestSize);
      a[I] += J;
      Tree[I] := Tree[I] + J;
    end;
  for I := 1 to TestSize do
    begin
      AssertTrue(Tree.RangeQuery(I - 1, I - 1) = a[I - 1]);
      L := Random(TestSize div 2);
      R := TestSize div 2 + Random(TestSize div 2);
      min := High(Integer);
      for J := L to R do
        if a[J] < min then
          min := a[J];
      AssertTrue(Tree.RangeQuery(L, R) = min);
      AssertTrue(Tree.RangeQuery(R, L) = High(Integer));
    end;
end;

procedure TSegmentTreeTest.MinQuery2;
const
  TestSize = 1000;
var
  Tree: TMinPosTree;
  a: array of Integer;
  I, J, L, R, min, minpos: Integer;
  mPos: TMinPos;
  procedure GetElem(aIndex: SizeInt; out aElem: TMinPos);
  begin
    aElem.Value := a[aIndex];
    aElem.Index := aIndex;
  end;
begin
  SetLength(a, TestSize);
  for I := 0 to High(a) do
    a[I] := Succ(Random(TestSize));
  Tree := TMinPosTree.Create(TestSize, @GetElem);
  for I := 1 to TestSize do
    begin
      AssertTrue(Tree.RangeQuery(I - 1, I - 1).Value = a[I - 1]);
      L := Random(TestSize div 2);
      R := TestSize div 2 + Random(TestSize div 2);
      min := High(Integer);
      minpos := -1;
      for J := L to R do
        if a[J] < min then
          begin
            min := a[J];
            minpos := J;
          end;
      mPos := Tree.RangeQuery(L, R);
      AssertTrue(mPos.Value = min);
      AssertTrue(mPos.Index = minpos);
    end;

  for I := 0 to High(a) do
    begin
      J := Random(TestSize);
      a[I] += J;
      mPos := Tree[I];
      mPos.Value += J;
      Tree[I] := mPos;
    end;
  for I := 1 to TestSize do
    begin
      AssertTrue(Tree.RangeQuery(I - 1, I - 1).Value = a[I - 1]);
      L := Random(TestSize div 2);
      R := TestSize div 2 + Random(TestSize div 2);
      min := High(Integer);
      minpos := -1;
      for J := L to R do
        if a[J] < min then
          begin
            min := a[J];
            minpos := J;
          end;
      mPos := Tree.RangeQuery(L, R);
      AssertTrue(mPos.Value = min);
      AssertTrue(mPos.Index = minpos);
    end;
end;

initialization

  RegisterTest(TArrayHelpUtilTest);
  RegisterTest(TSegmentTreeTest);

end.

