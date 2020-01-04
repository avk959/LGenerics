unit Samples;

{$mode objfpc}{$H+}
{$inline on}
{$modeswitch typehelpers}
{$modeswitch advancedrecords}

interface

uses
  SysUtils, TypInfo, math, LGArrayHelpers, LGMiscUtils, garrayutils, gutil,
  Generics.Collections, Generics.Defaults, {%H-}LGHelpers, PasPDQSort, epiktimer;

type

  TSortAlgo     = (saQuickSort, saIntroSort, saDualPivotQuickSort, saPDQSort, saMergeSort, saTimSort,
                   saFclSort,{ fcl-stl TOrderingArrayUtils.Sort }
                   saGCSort, { Generics.Collections TArrayHelper.Sort }
                   saPasPDQSort { Akira1364's Pascal translation of PDQSort from
                                  https://github.com/Akira13641/PasPDQSort });
  TSampleSize   = (ss1K, ss2K, ss5K, ss10K, ss20K, ss50K, ss01M, ss02M, ss05M, ss1M, ss2M, ss5M, ss10M);
  TSampleClass  = (scRandomUInt, scRandomUInt64, scRandomSingle, scRandomDouble, scRandomStr8,
                   scRandomVec4, scKLimited, scKEqualTeeth, scKEvenTeeth, scKSharpTeeth, scKDistance,
                   scKExchange);

  TVec4 = record
    X, Y, Z, W: Double;
    class function CreateRandom(aRange: Integer): TVec4; static; inline;
    class function Compare(const L, R: TVec4): Integer; static; inline;
  end;

  TUIntSample   = array of DWord;
  TUInt64Sample = array of QWord;
  TSingleSample = array of Single;
  TDoubleSample = array of Double;
  TStr8Sample   = array of string;
  TVec4Sample   = array of TVec4;

  { TSortAlgoHelper }

  TSortAlgoHelper = type helper for TSortAlgo
    function GetName: string;
  end;

  { TSampleSizeHelper }

  TSampleSizeHelper = type helper for TSampleSize
    function ToInt: SizeInt;
    function ToString: string;
  end;

  { TSampleClassHelper }

  TSampleClassHelper = type helper for TSampleClass
  private
    function  GenUIntSample(aSize: TSampleSize; K: SizeInt): TUIntSample;
    procedure GenRandomUInt32(var a: TUIntSample);
  { only k values in random order; aka Few Unique for small k }
    procedure GenKLimited(var a: TUIntSample; K: DWord);
  { aka sawtooth; simple sorted if k = 1; }
    procedure GenKEqualTeeth(var a: TUIntSample; K: DWord);
  { aka organ-pipe if k = 2; simple reversely sorted if k = 1 }
    procedure GenKEvenTeeth(var a: TUIntSample; K: DWord);
  { simple reversely sorted if k = 1 }
    procedure GenKSharpTeeth(var a: TUIntSample; K: DWord);
  { aka  Nearly Sorted for small K; simple sorted if k = 1 }
    procedure GenKDistance(var a: TUIntSample; K: DWord);
    procedure GenKExchange(var a: TUIntSample; K: DWord);
    function  GenUInt64Sample(aSize: TSampleSize): TUInt64Sample;
    function  GenSingleSample(aSize: TSampleSize): TSingleSample;
    function  GenDoubleSample(aSize: TSampleSize): TDoubleSample;
    function  GenStr8Sample(aSize: TSampleSize): TStr8Sample;
    function  GenVec4Sample(aSize: TSampleSize): TVec4Sample;
    function  RunUInt32Test(Algo: TSortAlgo; aSize: TSampleSize; K: SizeInt): Double;
    function  RunSingleTest(Algo: TSortAlgo; aSize: TSampleSize): Double;
    function  RunDoubleTest(Algo: TSortAlgo; aSize: TSampleSize): Double;
    function  RunUInt64Test(Algo: TSortAlgo; aSize: TSampleSize): Double;
    function  RunStr8Test(Algo: TSortAlgo; aSize: TSampleSize): Double;
    function  RunVec4Test(Algo: TSortAlgo; aSize: TSampleSize): Double;
  public
    function GetName: string;
    function RunTest(Algo: TSortAlgo; aSize: TSampleSize; K: SizeInt): Double;
  end;

var

  Timer: TEpikTimer;

implementation
{$B-}{$COPERATORS ON}

const
  DWordBound: Int64 = Int64(DWord.MaxValue) + 1; //Int64(1) shl 32


function GenRandomGuid: TGuid;
type
  TDWords4 = packed record
    D1, D2, D3, D4: DWord;
  end;
var
  dR: TDWords4 absolute Result;
begin
  dR.D1 := DWord(Random(DWordBound));
  dR.D2 := DWord(Random(DWordBound));
  dR.D3 := DWord(Random(DWordBound));
  dR.D4 := DWord(Random(DWordBound));
end;

function GenRandomString(aLength: Integer): string;
var
  I: Integer;
begin
  SetLength(Result, aLength);
  for I := 1 to aLength do
    Result[I] := Char(Random(95) + 32);
end;

class function TVec4.CreateRandom(aRange: Integer): TVec4;
begin
  with Result do begin
    X := Succ(Random(aRange));
    Y := Succ(Random(aRange));
    Z := Succ(Random(aRange));
    W := Succ(Random(aRange));
  end;
end;

class function TVec4.Compare(const L, R: TVec4): Integer;
begin
  Result := Ord(L.X + L.Y + L.Z + L.W > R.X + R.Y + R.Z + R.W) -
            Ord(L.X + L.Y + L.Z + L.W < R.X + R.Y + R.Z + R.W);
end;

{ TSortAlgoHelper }

function TSortAlgoHelper.GetName: string;
begin
  WriteStr(Result, Self);
  Result := Copy(Result, 3, Length(Result) - 2);
end;

{ TSampleSizeHelper }

function TSampleSizeHelper.ToInt: SizeInt;
begin
  case Self of
    ss1K:  Result := 1000;
    ss2K:  Result := 2000;
    ss5K:  Result := 5000;
    ss10K: Result := 10000;
    ss20K: Result := 20000;
    ss50K: Result := 50000;
    ss01M: Result := 100000;
    ss02M: Result := 200000;
    ss05M: Result := 500000;
    ss1M:  Result := 1000000;
    ss2M:  Result := 2000000;
    ss5M:  Result := 5000000;
  else //ss10M
    Result := 10000000;
  end;
end;

function TSampleSizeHelper.ToString: string;
begin
  Result := IntToStr(ToInt);
end;

{ TSampleClassHelper }

function TSampleClassHelper.GenUIntSample(aSize: TSampleSize; K: SizeInt): TUIntSample;
begin
  System.SetLength(Result, aSize.ToInt);
  case Self of
    scRandomUInt:  GenRandomUInt32(Result);
    scKLimited:    GenKLimited(Result, K);
    scKEqualTeeth: GenKEqualTeeth(Result, K);
    scKEvenTeeth:  GenKEvenTeeth(Result, K);
    scKSharpTeeth: GenKSharpTeeth(Result, K);
    scKDistance:   GenKDistance(Result, K);
  else //  scKExchange:
    GenKExchange(Result, K);
  end;
end;

procedure TSampleClassHelper.GenRandomUInt32(var a: TUIntSample);
var
  I: SizeInt;
begin
  for I := 0 to System.High(a) do
    a[I] := Random(DWordBound);
end;

procedure TSampleClassHelper.GenKLimited(var a: TUIntSample; K: DWord);
var
  I: SizeInt;
  r: DWord;
begin
  // only k values in random order; aka Few Unique for small k;
  r := DWord.MaxValue div K;
  for I := 0 to System.High(a) do
    a[I] := DWord(Random(Integer(K))) * r;
end;

procedure TSampleClassHelper.GenKEqualTeeth(var a: TUIntSample; K: DWord);
var
  I: SizeInt;
  r, n: DWord;
begin
{ aka sawtooth; simple sorted if k = 1;
        *       *       *       *
      *       *       *       *
    *       *       *       *
  *       *       *       *      ..... k teeth total,  }

  n := DWord(System.Length(a)) div K;
  r := DWord.MaxValue div n;
  for I := 0 to System.High(a) do
    a[I] := (DWord(I) mod n) * r;
end;

procedure TSampleClassHelper.GenKEvenTeeth(var a: TUIntSample; K: DWord);
var
  I: SizeInt;
  r, n: DWord;
begin
{ aka organ-pipe if k = 2; simple reversely sorted if k = 1;
  *           *           *
    *       *   *       *
      *   *       *   *
        *           *      ..... k teeth total}
  n := DWord(System.Length(a)) div K;
  r := DWord.MaxValue div n;
  for I := 0 to System.High(a) do
    if Odd(DWord(I) div n) then
      a[I] := (DWord(I) mod n) * r
    else
      a[I] := (n - DWord(I) mod n) * r;
end;

procedure TSampleClassHelper.GenKSharpTeeth(var a: TUIntSample; K: DWord);
type
  THelper = specialize TGComparableArrayHelper<DWord>;
var
  I, c, n, l, h: SizeInt;
  r, J: DWord;
begin
{ simple reversely sorted if k = 1;
                          *
                        *
                      *

               *
                 *
                   *

            *
          *
        *
 *
   *
     *                         ..... k teeth total}

  c := System.Length(a);
  if c > 0 then
    begin
      r := DWord.MaxValue div DWord(c);
      for I := 0 to System.High(a) do
        a[I] := DWord(I) * r;
      if K >= 1 then
        begin
          n := c div SizeInt(K);
          l := 0;
          h := Pred(n);
          for J := 1 to K do
            begin
              if Odd(J) then
                THelper.Reverse(a[l..h]);
              l += n;
              h += n;
            end;
          if (l < Pred(c)) and not Odd(K) then
            THelper.Reverse(a[l..Pred(c)]);
        end
    end;
end;

procedure TSampleClassHelper.GenKDistance(var a: TUIntSample; K: DWord);
type
  THelper = specialize TGComparableArrayHelper<DWord>;
var
  I, J, c, n, l, h: SizeInt;
  r: DWord;
begin
  { for each a[I] in a, the distance from its initial position to its final position is
   bound by K; aka  Nearly Sorted for small K }
  c := System.Length(a);
  if c > 0 then
    begin
      r := DWord.MaxValue div DWord(c);
      for I := 0 to System.High(a) do
        a[I] := DWord(I) * r;
      if K > 1 then
        begin
          n := c div SizeInt(K);
          l := 0;
          h := Pred(SizeInt(K));
          for J := 1 to n do
            begin
              THelper.RandomShuffle(a[l..h]);
              l += SizeInt(K);
              h += SizeInt(K);
            end;
          if l < Pred(c) then
            THelper.RandomShuffle(a[l..Pred(c)]);
        end
    end;
end;

procedure TSampleClassHelper.GenKExchange(var a: TUIntSample; K: DWord);
var
  I, J, L, c: SizeInt;
  r: DWord;
begin
  { an array A is said to be a k-exchange array if it becomes sorted after no more
    than exchanges of k pairs elements in A }
  c := System.Length(a);
  r := DWord.MaxValue div DWord(c);
  for I := 0 to System.High(a) do
    a[I] := DWord(I) * r;
  for I := 1 to SizeInt(K) do
    begin
      J := Random(c);
      L := Random(c);
      r := a[J];
      a[J] := a[L];
      a[L] := r;
    end;
end;

function TSampleClassHelper.GenUInt64Sample(aSize: TSampleSize): TUInt64Sample;
var
  I: SizeInt;
begin
  System.SetLength(Result, aSize.ToInt);
  for I := 0 to System.High(Result) do
    Result[I] := QWord(Random(High(Int64)));
end;

function TSampleClassHelper.GenSingleSample(aSize: TSampleSize): TSingleSample;
var
  I: SizeInt;
begin
  System.SetLength(Result, aSize.ToInt);
  for I := 0 to System.High(Result) do
    Result[I] := (Random - 0.5) * 2.0e35;
end;

function TSampleClassHelper.GenDoubleSample(aSize: TSampleSize): TDoubleSample;
var
  I: SizeInt;
begin
  System.SetLength(Result, aSize.ToInt);
  for I := 0 to System.High(Result) do
    Result[I] := (Random - 0.5) * 2.0e300;
end;

function RandomString(aLen: Integer): string;
var
  I: Integer;
begin
  SetLength(Result, aLen);
  for I := 1 to aLen do
    Result[I] := Char(Random(95) + 32);
end;

function TSampleClassHelper.GenStr8Sample(aSize: TSampleSize): TStr8Sample;
var
  I: SizeInt;
begin
  System.SetLength(Result, aSize.ToInt);
  for I := 0 to System.High(Result) do
    Result[I] := RandomString(8);
end;

function TSampleClassHelper.GenVec4Sample(aSize: TSampleSize): TVec4Sample;
var
  I: SizeInt;
begin
  System.SetLength(Result, aSize.ToInt);
  for I := 0 to System.High(Result) do
    Result[I] := TVec4.CreateRandom(200000000);
end;

function DwLess(constref L, R: DWord): Boolean;
begin
  Result := L < R;
end;

function TSampleClassHelper.RunUInt32Test(Algo: TSortAlgo; aSize: TSampleSize; K: SizeInt): Double;
type
  TDWordLess = specialize TLess<DWord>;
  THelper    = specialize TGComparableArrayHelper<DWord>;
  TTimSort   = specialize TGComparableTimSort<DWord>;
  TFclUtils  = specialize TOrderingArrayUtils<TUIntSample, DWord, TDWordLess>;
  TGcHelper  = specialize TArrayHelper<DWord>;
  TPDQSort   = specialize TPDQSorter<DWord>;
var
  Data: TUIntSample;
begin
  Data := GenUIntSample(aSize, K);
  Timer.Clear;
  Timer.Start;
  case Algo of
    saQuickSort:          THelper.QuickSort(Data);
    saIntroSort:          THelper.IntroSort(Data);
    saDualPivotQuickSort: THelper.DualPivotQuickSort(Data);
    saPDQSort:            THelper.PDQSort(Data);
    saMergeSort:          THelper.MergeSort(Data);
    saTimSort:            TTimSort.Sort(Data);
    saFclSort:            TFclUtils.Sort(Data, System.Length(Data));
    saGCSort:             TGcHelper.Sort(Data);
    saPasPDQSort:         TPDQSort.Sort(Data, @DwLess);
  end;
  Timer.Stop;
  Result := Timer.Elapsed;
end;

function SingleLess(constref L, R: Single): Boolean;
begin
  Result := L < R;
end;

function TSampleClassHelper.RunSingleTest(Algo: TSortAlgo; aSize: TSampleSize): Double;
type
  TSingleLess = specialize TLess<Single>;
  THelper     = specialize TGComparableArrayHelper<Single>;
  TTimSort    = specialize TGComparableTimSort<Single>;
  TFclUtils   = specialize TOrderingArrayUtils<TSingleSample, Single, TSingleLess>;
  TGcHelper   = specialize TArrayHelper<Single>;
  TPDQSort    = specialize TPDQSorter<Single>;
var
  Data: TSingleSample;
begin
  Data := GenSingleSample(aSize);
  Timer.Clear;
  Timer.Start;
  case Algo of
    saQuickSort:          THelper.QuickSort(Data);
    saIntroSort:          THelper.IntroSort(Data);
    saDualPivotQuickSort: THelper.DualPivotQuickSort(Data);
    saPDQSort:            THelper.PDQSort(Data);
    saMergeSort:          THelper.MergeSort(Data);
    saTimSort:            TTimSort.Sort(Data);
    saFclSort:            TFclUtils.Sort(Data, System.Length(Data));
    saGCSort:             TGcHelper.Sort(Data);
    saPasPDQSort:         TPDQSort.Sort(Data, @SingleLess);
  end;
  Timer.Stop;
  Result := Timer.Elapsed;
end;

function DblLess(constref L, R: Double): Boolean;
begin
  Result := L < R;
end;

function TSampleClassHelper.RunDoubleTest(Algo: TSortAlgo; aSize: TSampleSize): Double;
type
  TDoubleLess = specialize TLess<Double>;
  THelper     = specialize TGComparableArrayHelper<Double>;
  TTimSort    = specialize TGComparableTimSort<Double>;
  TFclUtils   = specialize TOrderingArrayUtils<TDoubleSample, Double, TDoubleLess>;
  TGcHelper   = specialize TArrayHelper<Double>;
  TPDQSort    = specialize TPDQSorter<Double>;
var
  Data: TDoubleSample;
begin
  Data := GenDoubleSample(aSize);
  Timer.Clear;
  Timer.Start;
  case Algo of
    saQuickSort:          THelper.QuickSort(Data);
    saIntroSort:          THelper.IntroSort(Data);
    saDualPivotQuickSort: THelper.DualPivotQuickSort(Data);
    saPDQSort:            THelper.PDQSort(Data);
    saMergeSort:          THelper.MergeSort(Data);
    saTimSort:            TTimSort.Sort(Data);
    saFclSort:            TFclUtils.Sort(Data, System.Length(Data));
    saGCSort:             TGcHelper.Sort(Data);
    saPasPDQSort:         TPDQSort.Sort(Data, @DblLess);
  end;
  Timer.Stop;
  Result := Timer.Elapsed;
end;

function QwLess(constref L, R: QWord): Boolean;
begin
  Result := L < R;
end;

function TSampleClassHelper.RunUInt64Test(Algo: TSortAlgo; aSize: TSampleSize): Double;
type
  TQWordLess = specialize TLess<QWord>;
  THelper    = specialize TGComparableArrayHelper<QWord>;
  TTimSort   = specialize TGComparableTimSort<QWord>;
  TFclUtils  = specialize TOrderingArrayUtils<TUInt64Sample, QWord, TQWordLess>;
  TGcHelper  = specialize TArrayHelper<QWord>;
  TPDQSort   = specialize TPDQSorter<QWord>;
var
  Data: TUInt64Sample;
begin
  Data := GenUInt64Sample(aSize);
  Timer.Clear;
  Timer.Start;
  case Algo of
    saQuickSort:          THelper.QuickSort(Data);
    saIntroSort:          THelper.IntroSort(Data);
    saDualPivotQuickSort: THelper.DualPivotQuickSort(Data);
    saPDQSort:            THelper.PDQSort(Data);
    saMergeSort:          THelper.MergeSort(Data);
    saTimSort:            TTimSort.Sort(Data);
    saFclSort:            TFclUtils.Sort(Data, System.Length(Data));
    saGCSort:             TGcHelper.Sort(Data);
    saPasPDQSort:         TPDQSort.Sort(Data, @QwLess);
  end;
  Timer.Stop;
  Result := Timer.Elapsed;
end;

function StrLess(constref L, R: string): Boolean;
begin
  Result := L < R;
end;

function StrCmp(constref L, R: string): Integer;
begin
  if L > R then
    Result := 1
  else
    if R > L then
      Result := -1
    else
      Result := 0;
end;

function TSampleClassHelper.RunStr8Test(Algo: TSortAlgo; aSize: TSampleSize): Double;
type
  TStrLess   = specialize TLess<string>;
  THelper    = specialize TGComparableArrayHelper<string>;
  TTimSort   = specialize TGComparableTimSort<string>;
  TFclUtils  = specialize TOrderingArrayUtils<TStr8Sample, string, TStrLess>;
  TGcHelper  = specialize TArrayHelper<string>;
  TPDQSort   = specialize TPDQSorter<string>;
var
  Data: TStr8Sample;
begin
  Data := GenStr8Sample(aSize);
  Timer.Clear;
  Timer.Start;
  case Algo of
    saQuickSort:          THelper.QuickSort(Data);
    saIntroSort:          THelper.IntroSort(Data);
    saDualPivotQuickSort: THelper.DualPivotQuickSort(Data);
    saPDQSort:            THelper.PDQSort(Data);
    saMergeSort:          THelper.MergeSort(Data);
    saTimSort:            TTimSort.Sort(Data);
    saFclSort:            TFclUtils.Sort(Data, System.Length(Data));
    saGCSort:             TGcHelper.Sort(Data, specialize TComparer<string>.Construct(@StrCmp));
    saPasPDQSort:         TPDQSort.Sort(Data, @StrLess);
  end;
  Timer.Stop;
  Result := Timer.Elapsed;
end;

function Vec4Order(constref A, B: TVec4): Int32;
var
  sA, sB: Double;
begin
  with A do
    sA := X + Y + Z + W;
  with B do
    sB := X + Y + Z + W;
  Result := Ord(sA > sB) - Ord(sA < sB);
end;

function Vec4Less(constref A, B: TVec4): Boolean;
begin
  Result := A.X + A.Y + A.Z + A.W < B.X + B.Y + B.Z + B.W;
end;

type
  TVec4Less = record
    class function c(const A, B: TVec4): Boolean; static;
  end;

class function TVec4Less.c(const A, B: TVec4): Boolean;
begin
  Result :=  A.X + A.Y + A.Z + A.W < B.X + B.Y + B.Z + B.W;
end;


function TSampleClassHelper.RunVec4Test(Algo: TSortAlgo; aSize: TSampleSize): Double;
type
  THelper    = specialize TGArrayHelper<TVec4>;
  TTimSort   = specialize TGTimSort<TVec4>;
  TFclUtils  = specialize TOrderingArrayUtils<TVec4Sample, TVec4, TVec4Less>;
  TGcHelper  = specialize TArrayHelper<TVec4>;
  TPDQSort   = specialize TPDQSorter<TVec4>;
var
  Data: TVec4Sample;
begin
  Data := GenVec4Sample(aSize);
  Timer.Clear;
  Timer.Start;
  case Algo of
    saQuickSort:          THelper.QuickSort(Data);
    saIntroSort:          THelper.IntroSort(Data);
    saDualPivotQuickSort: THelper.DualPivotQuickSort(Data);
    saPDQSort:            THelper.PDQSort(Data);
    saMergeSort:          THelper.MergeSort(Data);
    saTimSort:            TTimSort.Sort(Data);
    saFclSort:            TFclUtils.Sort(Data, System.Length(Data));
    saGCSort:             TGcHelper.Sort(Data, specialize TComparer<TVec4>.Construct(@Vec4Order));
    saPasPDQSort:         TPDQSort.Sort(Data, @Vec4Less);
  end;
  Timer.Stop;
  Result := Timer.Elapsed;
end;

function TSampleClassHelper.GetName: string;
begin
  case Self of
    scRandomUInt:    Result := 'random UInt32';
    scRandomUInt64:  Result := 'random UInt64';
    scRandomSingle:  Result := 'random Single';
    scRandomDouble:  Result := 'random Double';
    scRandomStr8:    Result := 'random 8-byte string';
    scRandomVec4:    Result := 'random 4D vector';
    scKLimited:      Result := 'K-limited';
    scKEqualTeeth:   Result := 'K-equal teeth';
    scKEvenTeeth:    Result := 'K-even teeth';
    scKSharpTeeth:   Result := 'K-sharp teeth';
    scKDistance:     Result := 'K-distance';
  else //scKExchange
    Result := 'K-exchange';
  end;
end;

function TSampleClassHelper.RunTest(Algo: TSortAlgo; aSize: TSampleSize; K: SizeInt): Double;
begin
  case Self of
    scRandomUInt64:  Result := RunUInt64Test(Algo, aSize);
    scRandomSingle:  Result := RunSingleTest(Algo, aSize);
    scRandomDouble:  Result := RunDoubleTest(Algo, aSize);
    scRandomStr8:    Result := RunStr8Test(Algo, aSize);
    scRandomVec4:    Result := RunVec4Test(Algo, aSize);
  else
    K := Math.Min(K, aSize.ToInt);
    Result := RunUInt32Test(Algo, aSize, K);
  end;
end;

initialization
  Timer := TEpikTimer.Create(nil);
  Timer.CorrelateTimebases;
finalization
  Timer.Free;
end.

