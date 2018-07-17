unit LGArrayHelpUtilTest;

{$mode objfpc}{$H+}

interface

uses

  SysUtils, fpcunit, testregistry,
  LGUtils,
  LGArrayHelpers;

type

  TArrayHelpUtilTest = class(TTestCase)
  private
  type

    TIntHelper  = specialize TGArrayHelpUtil<Integer>;
    TCompHelper = specialize TGComparableArrayHelper<Integer>;
    TIntArray   = specialize TGArray<Integer>;

  const
    ReverseIntSrc10: array[2..11] of Integer = (-1, -29, 0, 13, 1, -199, 21, 117, -15, 10);
    MergedIntSrc: array[1..14] of Integer    = (101, 0, -11, -27, 127, 1, 0, 10, -15, 117, 21, -199, 1, 13);
  published
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

    procedure AppendToEmpty;
    procedure Append;

    procedure MergeOfBothEmpty;
    procedure MergeOfDstEmptySrc7;
    procedure MergeOfDst7SrcEmpty;
    procedure MergeOfDst7Src7;
    procedure MergeOfSelf;

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

    procedure RandomShuffleEmpty;
    procedure RandomShuffleStatic1;
    procedure RandomShuffleDyn1;
    procedure RandomShuffleDyn10;
    procedure RandomShuffleStatic10;
  end;

implementation
{$B-}{$COPERATORS ON}

const

  IntSrc10: array[2..11] of Integer = (10, -15, 117, 21, -199, 1, 13, 0, -29, -1);

  IntSrc7: array[0..6] of Integer = (101, 0, -11, -27, 127, 1, 0);

{ TArrayHelpUtilTest }

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
  AssertTrue(TIntHelper.CreateReverseCopy(a) = nil);
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
  a := TIntHelper.CreateMerge(L, R);
  AssertTrue(a = nil);
end;

procedure TArrayHelpUtilTest.CreateMergeCopyOfLEmptyRStatic7;
var
  a, L: TIntArray;
begin
  a := TIntHelper.CreateMerge(L, IntSrc7);
  AssertTrue(TCompHelper.Same(IntSrc7, a));
end;

procedure TArrayHelpUtilTest.CreateMergeCopyOfLStatic7REmpty;
var
  a, R: TIntArray;
begin
  a := TIntHelper.CreateMerge(IntSrc7, R);
  AssertTrue(TCompHelper.Same(IntSrc7, a));
end;

procedure TArrayHelpUtilTest.CreateMergeCopyOfLEmptyRDyn7;
var
  a, L: TIntArray;
begin
  a := TIntHelper.CreateMerge(L, TIntHelper.CreateCopy(IntSrc7));
  AssertTrue(TCompHelper.Same(IntSrc7, a));
end;

procedure TArrayHelpUtilTest.CreateMergeCopyOfLDyn7REmpty;
var
  a, R: TIntArray;
begin
  a := TIntHelper.CreateMerge(TIntHelper.CreateCopy(IntSrc7), R);
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

procedure TArrayHelpUtilTest.AppendToEmpty;
var
  a: TIntArray;
begin
  TIntHelper.Append(10, a);
  AssertTrue(System.Length(a) = 1);
  AssertTrue(a[0] = 10);
end;

procedure TArrayHelpUtilTest.Append;
var
  a: TIntArray;
  I: Integer;
begin
  a := TIntArray.Create(0,1,2,3);
  TIntHelper.Append(4, a);
  TIntHelper.Append(5, a);
  TIntHelper.Append(6, a);
  TIntHelper.Append(7, a);
  AssertTrue(System.Length(a) = 8);
  for I := 0 to 7 do
    AssertTrue(a[I] = I);
end;

procedure TArrayHelpUtilTest.MergeOfBothEmpty;
var
  Dst, Src: TIntArray;
  MergedCount: SizeInt;
begin
  MergedCount := TIntHelper.Merge(Dst, Src);
  AssertTrue(MergedCount = 0);
  AssertTrue(Dst = nil);
  AssertTrue(Src = nil);
end;

procedure TArrayHelpUtilTest.MergeOfDstEmptySrc7;
var
  Dst, Src: TIntArray;
begin
  Src := TIntHelper.CreateCopy(IntSrc7);
  AssertTrue( TIntHelper.Merge(Dst, Src) = 7);
  AssertTrue(TCompHelper.Same(IntSrc7, Dst));
  AssertTrue(Src = nil);
end;

procedure TArrayHelpUtilTest.MergeOfDst7SrcEmpty;
var
  Dst, Src: TIntArray;
begin
  Dst := TIntHelper.CreateCopy(IntSrc7);
  AssertTrue( TIntHelper.Merge(Dst, Src) = 0);
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

procedure TArrayHelpUtilTest.SplitEmpty;
var
  a, r: TIntArray;
begin
  r := TIntHelper.Split(a, 0);
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
  r := TIntHelper.Extract(a, 0, 0);
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
  TIntHelper.Reverse(a);
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

procedure TArrayHelpUtilTest.RandomShuffleEmpty;
var
  a: TIntArray;
begin
  TIntHelper.RandomShuffle(a);
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



initialization

  RegisterTest(TArrayHelpUtilTest);
end.

