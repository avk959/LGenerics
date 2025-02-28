unit lgSeqUtilsTest;

{$mode objfpc}{$H+}{$warn 6058 off}{$modeswitch nestedprocvars}

interface

uses
  LazUtf8, Classes, SysUtils, Math, fpcunit, testregistry,
  lgUtils,
  lgHashSet,
  lgSeqUtils,
  lgHash;

type

  { TTestUnicodeUtils }

  TTestUnicodeUtils = class(TTestCase)
  private
  const
    DblOne  = Double(1.0);
    DblZero = Double(0.0);

  published
    procedure ValidateDfa;
    procedure Validate;

    procedure Utf8ToUcs4;
    procedure TestUtf8StrToCpSeq;
    procedure SubSequenceUtf16;
    procedure EditDistUtf8;
    procedure LevenshteinDistMbrUtf8;
    procedure LevenshteinDistMbrBoundedUtf8;
    procedure LevenshteinDistMbrDynUtf8;
    procedure LevenshteinDistMyersUtf8;
    procedure LevenshteinDistMyersDQUtf8;
    procedure LevenshteinDistMyersLongUtf8;
    procedure LevenshteinDistMyersBoundedUtf8;
    procedure LevenshteinDistMyersDQBoundedUtf8;
    procedure LevenshteinDistMyersLongBoundedUtf8;
    procedure LevenshteinDistMyersLongDynUtf8;
    procedure LcsDistWmUtf8;
    procedure LcsDistWmBoundedUtf8;
    procedure LcsDistWmDynUtf8;
    procedure DumDistMbrUtf8;
    procedure DumDistMbrBoundedUtf8;
    procedure DumDistMbrDynUtf8;
    procedure LcsGusUtf8Test;
    procedure LcsKRUtf8Test;
    procedure LcsMyersUtf8Test;
    procedure SimRatioUtf8Test;
    procedure SimRatioExUtf8Test;
    procedure FuzzySearchEdp;
    procedure Utf8HashTextTest;
    procedure Utf8HashText64Test;
    procedure Utf8SameTextTest;

    procedure Utf16ToUcs4;
    procedure SubSequenceUtf8;
    procedure EditDistUtf16;
    procedure LevenshteinDistMbrUtf16;
    procedure LevenshteinDistMbrBoundedUtf16;
    procedure LevenshteinDistMbrDynUtf16;
    procedure LevenshteinDistMyersUtf16;
    procedure LevenshteinDistMyersDQUtf16;
    procedure LevenshteinDistMyersLongUtf16;
    procedure LevenshteinDistMyersBoundedUtf16;
    procedure LevenshteinDistMyersDQBoundedUtf16;
    procedure LevenshteinDistMyersLongBoundedUtf16;
    procedure LevenshteinDistMyersLongDynUtf16;
    procedure LcsDistWmUtf16;
    procedure LcsDistWmBoundedUtf16;
    procedure LcsDistWmDynUtf16;
    procedure DumDistMbrUtf16;
    procedure DumDistMbrBoundedUtf16;
    procedure DumDistMbrDynUtf16;
    procedure LcsGusUtf16Test;
    procedure LcsKRUtf16Test;
    procedure LcsMyersUtf16Test;
    procedure SimRatioUtf16Test;
    procedure SimRatioExUtf16Test;

    procedure StrReplaceUtf8Test;
    procedure StrReplaceUtf8OwwTest;
    procedure StrReplaceUtf8CITest;
    procedure StrReplaceUtf8CiOwwTest;

    procedure ACStrReplaceTest;
    procedure ACStrReplaceCITest;
    procedure ACStrReplaceListTest;
  end;

  { TTestSeqUtils }

  TTestSeqUtils = class(TTestCase)
  private
  type
    TIntSeqUtil  = specialize TGSeqUtil<Integer, Integer>;
    TLcsEdit     = TIntSeqUtil.TLcsEdit;
    TSeqEdit     = TIntSeqUtil.TSeqEdit;
    TSeqLcsPatch = TIntSeqUtil.TSeqLcsPatch;
    TSeqPatch    = TIntSeqUtil.TSeqPatch;
    TPatchStat   = TIntSeqUtil.TSeqPatchStat;
    TIntArray    = array of Integer;
    function ApplyLcsPatch(const aSrc: array of Integer; const aPatch: TSeqLcsPatch): TIntArray;
    function ApplyPatch(const aSrc: array of Integer; const aPatch: TSeqPatch): TIntArray;
  published
    procedure IsPrefixTest;
    procedure IsSuffixTest;
    procedure PrefixLenTest;
    procedure SuffixLenTest;
    procedure SameTest;
    procedure IsPermutationTest;
    procedure TestDiff;
    procedure TestLcsPatch;
    procedure TestPatch;
  end;

  { TTestFuzzySearchBitap }

  TTestFuzzySearchBitap = class(TTestCase)
  published
    procedure TestCreate;
    procedure TestNextMatch;
    procedure TestNextMatchOww;
    procedure TestNextMatchCI;
    procedure TestNextMatchCIOww;
    procedure TestFindMatches;
    procedure TestFindMatchesOww;
    procedure TestFindMatchesCI;
    procedure TestFindMatchesCIOww;
    procedure TestEnumerator;
    procedure TestEnumeratorOww;
    procedure TestEnumeratorCI;
    procedure TestEnumeratorCIOww;
  end;

  { TACSearchFsmTest }

  TACSearchFsmTest = class(TTestCase)
  private
  type
    TStrSetType = specialize TGLiteChainHashSet<string, string>;
    TStrSet     = TStrSetType.TSet;
    TMatch      = lgUtils.TIndexMatch;
  var
    FMatchCount: Integer;
    function OnMatch(const m: TMatch): Boolean;
    function OnMatchFirst(const m: TMatch): Boolean;
  published
    procedure TestCreate;
    procedure TestSearchDelegatedDfa;
    procedure TestSearchDelegatedNfa;
    procedure TestSearchDelegatedDfaCI;
    procedure TestSearchDelegatedNfaCI;
    procedure TestSearchNestedDfa;
    procedure TestSearchNestedNfa;
    procedure TestSearchNestedDfaCI;
    procedure TestSearchNestedNfaCI;
    procedure TestIndexOfPatternDfa;
    procedure TestIndexOfPatternNfa;
    procedure TestIndexOfPatternDfaCI;
    procedure TestIndexOfPatternNfaCI;
    procedure TestFirstMatchDfa;
    procedure TestFirstMatchDfaOww;
    procedure TestFirstMatchDfaCI;
    procedure TestFirstMatchDfaCIOww;
    procedure TestFirstMatchNfa;
    procedure TestFirstMatchNfaOww;
    procedure TestFirstMatchNfaCI;
    procedure TestFirstMatchNfaCIOww;
    procedure TestFindMatches;
    procedure TestFindMatchesOww;
    procedure TestContainsMatchDfa;
    procedure TestContainsMatchNfa;
    procedure TestContainsMatchDfaCI;
    procedure TestContainsMatchNfaCI;
  end;

  { TACPersistFsmTest }

  TACPersistFsmTest = class(TTestCase)
    procedure TestCreation;
    procedure TestStreaming;
  end;

  { TKmpSearchCITest }

  TKmpSearchCITest = class(TTestCase)
    procedure TestCreation;
    procedure TestNextMatch;
    procedure TestNextMatchOww;
    procedure TestFindMatches;
    procedure TestFindMatchesOww;
  end;

  { TKmpSearchTest }

  TKmpSearchTest = class(TTestCase)
    procedure TestNextMatch;
    procedure TestNextMatchOww;
    procedure TestFindMatches;
    procedure TestFindMatchesOww;
  end;

implementation

const
  Ascii: rawbytestring = #1#2#3#4#5#6#7#8#9#10#11#12#13#14#15#16#17#0;
  NotAscii: rawbytestring = #128#129#130#131#132#133#134#135#136#137#140;

const
  WellFormed: array of string = (
    'Унылая пора! Очей очарованье!',
    'Sad time! Oh charm!',
    'Кызганыч вакыт! Эх, сөйкемлелек!',
    'เวลาเศร้า! โอ้เสน่ห์! ',
    '悲しい時間！ おお魅力！',
    'ದುಃಖದ ಸಮಯ! ಓ ಮೋಡಿ! ',
    '悲傷的時刻！ 哦魅力！',
    'Kədərli vaxt! Ey cazibədarlıq!',
    'სამწუხარო დრო! ოჰ ხიბლი!',
    'दुखद समय! ओह आकर्षण!'
  );

  MalFormed: array of rawbytestring = (
    #$c3#$28,
    #$a0#$a1,
    #$e2#$28#$a1,
    #$e2#$82#$28,
    #$f0#$28#$8c#$bc,
    #$f0#$90#$28#$bc,
    #$f0#$28#$8c#$28,
    #$c0#$9f,
    #$f5#$ff#$ff#$ff,
    #$ed#$a0#$81,
    #$f8#$90#$80#$80#$80,
    #$c2#$7F,
    #$ce,
    #$ce#$ba#$e1,
    #$ce#$ba#$e1#$bd,
    #$ce#$ba#$e1#$bd#$b9#$cf,
    #$ce#$ba#$e1#$bd#$b9#$cf#$83#$ce,
    #$ce#$ba#$e1#$bd#$b9#$cf#$83#$ce#$bc#$ce,
    #$df,
    #$ef#$bf,

    #$c0#$af,
    #$e0#$80#$af,
    #$f0#$80#$80#$af,
    #$fc#$80#$80#$80#$80#$af,

    #$c1#$bf,
    #$e0#$9f#$bf,
    #$f0#$8f#$bf#$bf,
    #$f8#$87#$bf#$bf#$bf,
    #$fc#$83#$bf#$bf#$bf#$bf,

    #$c0#$80,
    #$e0#$80#$80,
    #$f0#$80#$80#$80,
    #$f8#$80#$80#$80#$80,
    #$fc#$80#$80#$80#$80#$80,

    #$ed#$a0#$80,
    #$ed#$ad#$bf,
    #$ed#$ae#$80,
    #$ed#$af#$bf,
    #$ed#$b0#$80,
    #$ed#$be#$80,
    #$ed#$bf#$bf,

    #$ed#$a0#$80#$ed#$b0#$80,
    #$ed#$a0#$80#$ed#$bf#$bf,
    #$ed#$ad#$bf#$ed#$b0#$80,
    #$ed#$ad#$bf#$ed#$bf#$bf,
    #$ed#$ae#$80#$ed#$b0#$80,
    #$ed#$ae#$80#$ed#$bf#$bf,
    #$ed#$af#$bf#$ed#$b0#$80,
    #$ed#$af#$bf#$ed#$bf#$bf
  );

procedure TTestUnicodeUtils.ValidateDfa;
var
  s: string;
begin
  AssertTrue(Utf8ValidateDfa(Ascii));
  AssertFalse(Utf8ValidateDfa(NotAscii));

  for s in WellFormed do
    AssertTrue(Utf8ValidateDfa(s));

  for s in MalFormed do
    AssertFalse(Utf8ValidateDfa(s));
end;

procedure TTestUnicodeUtils.Validate;
var
  s: string;
begin
  AssertTrue(Utf8Validate(Ascii));
  AssertFalse(Utf8Validate(NotAscii));

  for s in WellFormed do
    AssertTrue(Utf8Validate(s));

  for s in MalFormed do
    AssertFalse(Utf8Validate(s));
end;

procedure TTestUnicodeUtils.Utf8ToUcs4;
const
  s1:  string = 'Ну, здравствуйте, здравствуйте. Je vois que je vous fais peur, садитесь и рассказывайте';
  s2:  string = 'Dieu, quelle virulente sortie![4] – отвечал, нисколько не смутясь такою встречей';
  s3:  string = 'Вы не видали еще, – или: – вы не знакомы с ma tante? – говорила Анна Павловна';
  s4:  string = 'Il va se faire tuer. Dites-moi, pourquoi cette vilaine guerre, – сказала она князю Василию';
  s5:  string = 'Да, я слышал про его план вечного мира, и это очень интересно, но едва ли возможно…';
  s6:  string = 'шепнула Анна Павловна одному. – Le vicomte est un parfait conteur, – проговорила она другому';
  s7:  string = 'Княжна облокотила свою открытую полную руку на столик и не нашла нужным что-либо сказать';
  s8:  string = 'Le charmant Hippolyte поражал своим необыкновенным сходством с сестрою-красавицею';
  s9:  string = 'Ce n’est pas une histoire de revenants? – сказал он, усевшись подле княгини и торопливо пристроив к глазам свой лорнет';
  s10: string = '悲しい時間！ おお魅力！';
begin
  AssertTrue('empty string', Ucs4SeqToUtf8(Utf8ToUcs4Seq('')) = '');
  AssertTrue('s1', Ucs4SeqToUtf8(Utf8ToUcs4Seq(s1)) = s1);
  AssertTrue('s2', Ucs4SeqToUtf8(Utf8ToUcs4Seq(s2)) = s2);
  AssertTrue('s3', Ucs4SeqToUtf8(Utf8ToUcs4Seq(s3)) = s3);
  AssertTrue('s4', Ucs4SeqToUtf8(Utf8ToUcs4Seq(s4)) = s4);
  AssertTrue('s5', Ucs4SeqToUtf8(Utf8ToUcs4Seq(s5)) = s5);
  AssertTrue('s6', Ucs4SeqToUtf8(Utf8ToUcs4Seq(s6)) = s6);
  AssertTrue('s7', Ucs4SeqToUtf8(Utf8ToUcs4Seq(s7)) = s7);
  AssertTrue('s8', Ucs4SeqToUtf8(Utf8ToUcs4Seq(s8)) = s8);
  AssertTrue('s9', Ucs4SeqToUtf8(Utf8ToUcs4Seq(s9)) = s9);
  AssertTrue('s10', Ucs4SeqToUtf8(Utf8ToUcs4Seq(s10)) = s10);
end;

procedure TTestUnicodeUtils.TestUtf8StrToCpSeq;
const
  s1:  string = 'Ну, здравствуйте, здравствуйте. Je vois que je vous fais peur, садитесь и рассказывайте';
  s2:  string = 'Dieu, quelle virulente sortie![4] – отвечал, нисколько не смутясь такою встречей';
  s3:  string = 'Вы не видали еще, – или: – вы не знакомы с ma tante? – говорила Анна Павловна';
  s4:  string = 'Il va se faire tuer. Dites-moi, pourquoi cette vilaine guerre, – сказала она князю Василию';
  s5:  string = 'Да, я слышал про его план вечного мира, и это очень интересно, но едва ли возможно…';
  s6:  string = 'шепнула Анна Павловна одному. – Le vicomte est un parfait conteur, – проговорила она другому';
  s7:  string = 'Княжна облокотила свою открытую полную руку на столик и не нашла нужным что-либо сказать';
  s8:  string = 'Le charmant Hippolyte поражал своим необыкновенным сходством с сестрою-красавицею';
  s9:  string = 'Ce n’est pas une histoire de revenants? – сказал он, усевшись подле княгини и торопливо пристроив к глазам свой лорнет';
  s10: string = '悲しい時間！ おお魅力！';
begin
  AssertTrue('empty string', CpSeqToUtf8Str(Utf8StrToCpSeq('')) = '');
  AssertTrue('s1', CpSeqToUtf8Str(Utf8StrToCpSeq(s1)) = s1);
  AssertTrue('s2', CpSeqToUtf8Str(Utf8StrToCpSeq(s2)) = s2);
  AssertTrue('s3', CpSeqToUtf8Str(Utf8StrToCpSeq(s3)) = s3);
  AssertTrue('s4', CpSeqToUtf8Str(Utf8StrToCpSeq(s4)) = s4);
  AssertTrue('s5', CpSeqToUtf8Str(Utf8StrToCpSeq(s5)) = s5);
  AssertTrue('s6', CpSeqToUtf8Str(Utf8StrToCpSeq(s6)) = s6);
  AssertTrue('s7', CpSeqToUtf8Str(Utf8StrToCpSeq(s7)) = s7);
  AssertTrue('s8', CpSeqToUtf8Str(Utf8StrToCpSeq(s8)) = s8);
  AssertTrue('s9', CpSeqToUtf8Str(Utf8StrToCpSeq(s9)) = s9);
  AssertTrue('s10', CpSeqToUtf8Str(Utf8StrToCpSeq(s10)) = s10);
end;

procedure TTestUnicodeUtils.SubSequenceUtf16;
var
  Ucs4Seq: TUcs4Seq;
  s: unicodestring;
begin
  s := '';
  AssertTrue(IsSubSequenceUtf16(s, ''));
  AssertFalse(IsSubSequenceUtf16(s, 'a'));
  s := 'a';
  AssertTrue(IsSubSequenceUtf16(s, ''));
  Ucs4Seq := [$41, $10331, $43, $10333, $45, $10334, $47, $10336]; //ascii + Gothic letters
  s := Ucs4SeqToUtf16(Ucs4Seq);
  AssertTrue(IsSubSequenceUtf16(s, Copy(s, 2, Length(s))));
  AssertFalse(IsSubSequenceUtf16(s, Copy(s, 3, Length(s))));
  AssertTrue(IsSubSequenceUtf16(s, Copy(s, 4, Length(s))));
  AssertTrue(IsSubSequenceUtf16(s, Copy(s, 5, Length(s))));
  AssertFalse(IsSubSequenceUtf16(s, Copy(s, 6, Length(s))));
end;

procedure TTestUnicodeUtils.EditDistUtf8;
var
  s1, s2: string;
  sec: TSeqEditCost;
begin
  s1 := '';
  s2 := 'привет';
  sec := TSeqEditCost.Default;
  AssertTrue(EditDistanceUtf8(s1, s1, sec) = 0);
  AssertTrue(EditDistanceUtf8(s2, s2, sec) = 0);
  AssertTrue(EditDistanceUtf8(s1, s2, sec) = 6);
  AssertTrue(EditDistanceUtf8(s2, s1, sec) = 6);

  s1 := 'аб';
  s2 := 'аа';
  AssertTrue(EditDistanceUtf8(s1, s2, sec) = 1);
  AssertTrue(EditDistanceUtf8(s2, s1, sec) = 1);

  s1 := 'аб';
  s2 := 'ба';
  AssertTrue(EditDistanceUtf8(s1, s2, sec) = 2);
  AssertTrue(EditDistanceUtf8(s2, s1, sec) = 2);

  s1 := 'аб';
  s2 := 'ааа';
  AssertTrue(EditDistanceUtf8(s1, s2, sec) = 2);
  AssertTrue(EditDistanceUtf8(s2, s1, sec) = 2);

  s1 := 'а';
  s2 := 'ббб';
  AssertTrue(EditDistanceUtf8(s1, s2, sec) = 3);
  AssertTrue(EditDistanceUtf8(s2, s1, sec) = 3);

  s1 := 'аабабаб';
  s2 := 'аббаа';
  AssertTrue(EditDistanceUtf8(s1, s2, sec) = 3);
  AssertTrue(EditDistanceUtf8(s2, s1, sec) = 3);

  s1 := 'нелли';
  s2 := 'елли';
  AssertTrue(EditDistanceUtf8(s1, s2, sec) = 1);
  AssertTrue(EditDistanceUtf8(s2, s1, sec) = 1);

  s1 := 'нелли';
  s2 := 'еллия';
  AssertTrue(EditDistanceUtf8(s1, s2, sec) = 2);
  AssertTrue(EditDistanceUtf8(s2, s1, sec) = 2);

  s1 := 'мостик';
  s2 := 'костик';
  AssertTrue(EditDistanceUtf8(s1, s2, sec) = 1);
  AssertTrue(EditDistanceUtf8(s2, s1, sec) = 1);

  s1 := 'мостик';
  s2 := 'костяка';
  AssertTrue(EditDistanceUtf8(s1, s2, sec) = 3);
  AssertTrue(EditDistanceUtf8(s2, s1, sec) = 3);

  s1 := 'дистанция';
  s2 := 'дисперсия';
  AssertTrue(EditDistanceUtf8(s1, s2, sec) = 4);
  AssertTrue(EditDistanceUtf8(s2, s1, sec) = 4);

  s1 := 'левенштейн';
  s2 := 'франкенштейн';
  AssertTrue(EditDistanceUtf8(s1, s2, sec) = 5);
  AssertTrue(EditDistanceUtf8(s2, s1, sec) = 5);

  s1 := 'ааааааа';
  s2 := 'ааааааа';
  AssertTrue(EditDistanceUtf8(s1, s2, sec) = 0);

  s1 := 'ааааааа';
  s2 := 'бббббббб';
  AssertTrue(EditDistanceUtf8(s1, s2, sec) = 8);
  AssertTrue(EditDistanceUtf8(s2, s1, sec) = 8);

  s1 := 'аааббаааа';
  s2 := 'ааааааа';
  AssertTrue(EditDistanceUtf8(s1, s2, sec) = 2);
  AssertTrue(EditDistanceUtf8(s2, s1, sec) = 2);

  s1 := 'а';
  s2 := 'б';
  AssertTrue(EditDistanceUtf8(s1, s2, sec) = 1);
  AssertTrue(EditDistanceUtf8(s2, s1, sec) = 1);

  s1 := '一天，在寒冷的冬天，我从森林里走出来。';
  s2 := '一，在寒冷的冬天，我从森林里走来。';
  AssertTrue(EditDistanceUtf8(s1, s2, sec) = 2);
  AssertTrue(EditDistanceUtf8(s2, s1, sec) = 2);
end;

procedure TTestUnicodeUtils.LevenshteinDistMbrUtf8;
var
  s1, s2: string;
begin
  s1 := '';
  s2 := 'привет';
  AssertTrue(LevDistanceMbrUtf8(s1, s1) = 0);
  AssertTrue(LevDistanceMbrUtf8(s2, s2) = 0);
  AssertTrue(LevDistanceMbrUtf8(s1, s2) = 6);
  AssertTrue(LevDistanceMbrUtf8(s2, s1) = 6);

  s1 := 'аб';
  s2 := 'аа';
  AssertTrue(LevDistanceMbrUtf8(s1, s2) = 1);
  AssertTrue(LevDistanceMbrUtf8(s2, s1) = 1);

  s1 := 'аб';
  s2 := 'ба';
  AssertTrue(LevDistanceMbrUtf8(s1, s2) = 2);
  AssertTrue(LevDistanceMbrUtf8(s2, s1) = 2);

  s1 := 'аб';
  s2 := 'ааа';
  AssertTrue(LevDistanceMbrUtf8(s1, s2) = 2);
  AssertTrue(LevDistanceMbrUtf8(s2, s1) = 2);

  s1 := 'а';
  s2 := 'ббб';
  AssertTrue(LevDistanceMbrUtf8(s1, s2) = 3);
  AssertTrue(LevDistanceMbrUtf8(s2, s1) = 3);

  s1 := 'аабабаб';
  s2 := 'аббаа';
  AssertTrue(LevDistanceMbrUtf8(s1, s2) = 3);
  AssertTrue(LevDistanceMbrUtf8(s2, s1) = 3);

  s1 := 'нелли';
  s2 := 'елли';
  AssertTrue(LevDistanceMbrUtf8(s1, s2) = 1);
  AssertTrue(LevDistanceMbrUtf8(s2, s1) = 1);

  s1 := 'нелли';
  s2 := 'еллия';
  AssertTrue(LevDistanceMbrUtf8(s1, s2) = 2);
  AssertTrue(LevDistanceMbrUtf8(s2, s1) = 2);

  s1 := 'мостик';
  s2 := 'костик';
  AssertTrue(LevDistanceMbrUtf8(s1, s2) = 1);
  AssertTrue(LevDistanceMbrUtf8(s2, s1) = 1);

  s1 := 'мостик';
  s2 := 'костяка';
  AssertTrue(LevDistanceMbrUtf8(s1, s2) = 3);
  AssertTrue(LevDistanceMbrUtf8(s2, s1) = 3);

  s1 := 'дистанция';
  s2 := 'дисперсия';
  AssertTrue(LevDistanceMbrUtf8(s1, s2) = 4);
  AssertTrue(LevDistanceMbrUtf8(s2, s1) = 4);

  s1 := 'левенштейн';
  s2 := 'франкенштейн';
  AssertTrue(LevDistanceMbrUtf8(s1, s2) = 5);
  AssertTrue(LevDistanceMbrUtf8(s2, s1) = 5);

  s1 := 'ааааааа';
  s2 := 'ааааааа';
  AssertTrue(LevDistanceMbrUtf8(s1, s2) = 0);

  s1 := 'ааааааа';
  s2 := 'бббббббб';
  AssertTrue(LevDistanceMbrUtf8(s1, s2) = 8);
  AssertTrue(LevDistanceMbrUtf8(s2, s1) = 8);

  s1 := 'аааббаааа';
  s2 := 'ааааааа';
  AssertTrue(LevDistanceMbrUtf8(s1, s2) = 2);
  AssertTrue(LevDistanceMbrUtf8(s2, s1) = 2);

  s1 := 'а';
  s2 := 'б';
  AssertTrue(LevDistanceMbrUtf8(s1, s2) = 1);
  AssertTrue(LevDistanceMbrUtf8(s2, s1) = 1);

  s1 := '一天，在寒冷的冬天，我从森林里走出来。';
  s2 := '一，在寒冷的冬天，我从森林里走来。';
  AssertTrue(LevDistanceMbrUtf8(s1, s2) = 2);
  AssertTrue(LevDistanceMbrUtf8(s2, s1) = 2);
end;

procedure TTestUnicodeUtils.LevenshteinDistMbrBoundedUtf8;
var
  s1, s2: string;
begin
  s1 := '';
  s2 := 'привет';
  AssertTrue(LevDistanceMbrUtf8(s1, s2) = 6);
  AssertTrue(LevDistanceMbrUtf8(s2, s1, 5) = -1);

  s1 := 'аб';
  s2 := 'аа';
  AssertTrue(LevDistanceMbrUtf8(s1, s2) = 1);
  AssertTrue(LevDistanceMbrUtf8(s1, s2, 0) = -1);

  s1 := 'аб';
  s2 := 'ба';
  AssertTrue(LevDistanceMbrUtf8(s1, s2) = 2);
  AssertTrue(LevDistanceMbrUtf8(s1, s2, 1) = -1);

  s1 := 'аб';
  s2 := 'ааа';
  AssertTrue(LevDistanceMbrUtf8(s1, s2) = 2);
  AssertTrue(LevDistanceMbrUtf8(s1, s2, 1) = -1);

  s1 := 'а';
  s2 := 'ббб';
  AssertTrue(LevDistanceMbrUtf8(s1, s2) = 3);
  AssertTrue(LevDistanceMbrUtf8(s1, s2, 2) = -1);

  s1 := 'аабабаб';
  s2 := 'аббаа';
  AssertTrue(LevDistanceMbrUtf8(s1, s2) = 3);
  AssertTrue(LevDistanceMbrUtf8(s1, s2, 2) = -1);

  s1 := 'нелли';
  s2 := 'елли';
  AssertTrue(LevDistanceMbrUtf8(s1, s2) = 1);
  AssertTrue(LevDistanceMbrUtf8(s1, s2, 0) = -1);

  s1 := 'нелли';
  s2 := 'еллия';
  AssertTrue(LevDistanceMbrUtf8(s1, s2) = 2);
  AssertTrue(LevDistanceMbrUtf8(s1, s2, 1) = -1);

  s1 := 'мостик';
  s2 := 'костик';
  AssertTrue(LevDistanceMbrUtf8(s1, s2) = 1);
  AssertTrue(LevDistanceMbrUtf8(s1, s2, 0) = -1);

  s1 := 'мостик';
  s2 := 'костяка';
  AssertTrue(LevDistanceMbrUtf8(s1, s2) = 3);
  AssertTrue(LevDistanceMbrUtf8(s1, s2, 2) = -1);

  s1 := 'дистанция';
  s2 := 'дисперсия';
  AssertTrue(LevDistanceMbrUtf8(s1, s2) = 4);
  AssertTrue(LevDistanceMbrUtf8(s1, s2, 3) = -1);

  s1 := 'левенштейн';
  s2 := 'франкенштейн';
  AssertTrue(LevDistanceMbrUtf8(s1, s2) = 5);
  AssertTrue(LevDistanceMbrUtf8(s1, s2, 4) = -1);

  s1 := 'ааааааа';
  s2 := 'бббббббб';
  AssertTrue(LevDistanceMbrUtf8(s1, s2) = 8);
  AssertTrue(LevDistanceMbrUtf8(s1, s2, 7) = -1);

  s1 := 'аааббаааа';
  s2 := 'ааааааа';
  AssertTrue(LevDistanceMbrUtf8(s1, s2) = 2);
  AssertTrue(LevDistanceMbrUtf8(s1, s2, 1) = -1);

  s1 := 'а';
  s2 := 'б';
  AssertTrue(LevDistanceMbrUtf8(s1, s2) = 1);
  AssertTrue(LevDistanceMbrUtf8(s1, s2, 0) = -1);

  s1 := '一天，在寒冷的冬天，我从森林里走出来。';
  s2 := '一，在寒冷的冬天，我从森林里走来。';
  AssertTrue(LevDistanceMbrUtf8(s1, s2) = 2);
  AssertTrue(LevDistanceMbrUtf8(s2, s1, 1) = -1);
end;

procedure TTestUnicodeUtils.LevenshteinDistMbrDynUtf8;
var
  s1, s2: string;
begin
  s1 := '';
  s2 := 'привет';
  AssertTrue(LevDistanceMbrUtf8(s1, s1, -1) = 0);
  AssertTrue(LevDistanceMbrUtf8(s2, s2, -1) = 0);
  AssertTrue(LevDistanceMbrUtf8(s1, s2, -1) = 6);
  AssertTrue(LevDistanceMbrUtf8(s2, s1, -1) = 6);

  s1 := 'аб';
  s2 := 'аа';
  AssertTrue(LevDistanceMbrUtf8(s1, s2, -1) = 1);
  AssertTrue(LevDistanceMbrUtf8(s2, s1, -1) = 1);

  s1 := 'аб';
  s2 := 'ба';
  AssertTrue(LevDistanceMbrUtf8(s1, s2, -1) = 2);
  AssertTrue(LevDistanceMbrUtf8(s2, s1, -1) = 2);

  s1 := 'аб';
  s2 := 'ааа';
  AssertTrue(LevDistanceMbrUtf8(s1, s2, -1) = 2);
  AssertTrue(LevDistanceMbrUtf8(s2, s1, -1) = 2);

  s1 := 'а';
  s2 := 'ббб';
  AssertTrue(LevDistanceMbrUtf8(s1, s2, -1) = 3);
  AssertTrue(LevDistanceMbrUtf8(s2, s1, -1) = 3);

  s1 := 'аабабаб';
  s2 := 'аббаа';
  AssertTrue(LevDistanceMbrUtf8(s1, s2, -1) = 3);
  AssertTrue(LevDistanceMbrUtf8(s2, s1, -1) = 3);

  s1 := 'нелли';
  s2 := 'елли';
  AssertTrue(LevDistanceMbrUtf8(s1, s2, -1) = 1);
  AssertTrue(LevDistanceMbrUtf8(s2, s1, -1) = 1);

  s1 := 'нелли';
  s2 := 'еллия';
  AssertTrue(LevDistanceMbrUtf8(s1, s2, -1) = 2);
  AssertTrue(LevDistanceMbrUtf8(s2, s1, -1) = 2);

  s1 := 'мостик';
  s2 := 'костик';
  AssertTrue(LevDistanceMbrUtf8(s1, s2, -1) = 1);
  AssertTrue(LevDistanceMbrUtf8(s2, s1, -1) = 1);

  s1 := 'мостик';
  s2 := 'костяка';
  AssertTrue(LevDistanceMbrUtf8(s1, s2, -1) = 3);
  AssertTrue(LevDistanceMbrUtf8(s2, s1, -1) = 3);

  s1 := 'дистанция';
  s2 := 'дисперсия';
  AssertTrue(LevDistanceMbrUtf8(s1, s2, -1) = 4);
  AssertTrue(LevDistanceMbrUtf8(s2, s1, -1) = 4);

  s1 := 'левенштейн';
  s2 := 'франкенштейн';
  AssertTrue(LevDistanceMbrUtf8(s1, s2, -1) = 5);
  AssertTrue(LevDistanceMbrUtf8(s2, s1, -1) = 5);

  s1 := 'ааааааа';
  s2 := 'ааааааа';
  AssertTrue(LevDistanceMbrUtf8(s1, s2, -1) = 0);

  s1 := 'ааааааа';
  s2 := 'бббббббб';
  AssertTrue(LevDistanceMbrUtf8(s1, s2, -1) = 8);
  AssertTrue(LevDistanceMbrUtf8(s2, s1, -1) = 8);

  s1 := 'аааббаааа';
  s2 := 'ааааааа';
  AssertTrue(LevDistanceMbrUtf8(s1, s2, -1) = 2);
  AssertTrue(LevDistanceMbrUtf8(s2, s1, -1) = 2);

  s1 := 'а';
  s2 := 'б';
  AssertTrue(LevDistanceMbrUtf8(s1, s2, -1) = 1);
  AssertTrue(LevDistanceMbrUtf8(s2, s1, -1) = 1);

  s1 := '一天，在寒冷的冬天，我从森林里走出来。';
  s2 := '一，在寒冷的冬天，我从森林里走来。';
  AssertTrue(LevDistanceMbrUtf8(s1, s2, -1) = 2);
  AssertTrue(LevDistanceMbrUtf8(s2, s1, -1) = 2);
end;

procedure TTestUnicodeUtils.LevenshteinDistMyersUtf8;
var
  s1, s2: string;
begin
  s1 := '';
  s2 := 'привет';
  AssertTrue(LevDistanceMyersUtf8(s1, s1) = 0);
  AssertTrue(LevDistanceMyersUtf8(s2, s2) = 0);
  AssertTrue(LevDistanceMyersUtf8(s1, s2) = 6);
  AssertTrue(LevDistanceMyersUtf8(s2, s1) = 6);

  s1 := 'аб';
  s2 := 'аа';
  AssertTrue(LevDistanceMyersUtf8(s1, s2) = 1);
  AssertTrue(LevDistanceMyersUtf8(s2, s1) = 1);

  s1 := 'аб';
  s2 := 'ба';
  AssertTrue(LevDistanceMyersUtf8(s1, s2) = 2);
  AssertTrue(LevDistanceMyersUtf8(s2, s1) = 2);

  s1 := 'аб';
  s2 := 'ааа';
  AssertTrue(LevDistanceMyersUtf8(s1, s2) = 2);
  AssertTrue(LevDistanceMyersUtf8(s2, s1) = 2);

  s1 := 'а';
  s2 := 'ббб';
  AssertTrue(LevDistanceMyersUtf8(s1, s2) = 3);
  AssertTrue(LevDistanceMyersUtf8(s2, s1) = 3);

  s1 := 'аабабаб';
  s2 := 'аббаа';
  AssertTrue(LevDistanceMyersUtf8(s1, s2) = 3);
  AssertTrue(LevDistanceMyersUtf8(s2, s1) = 3);

  s1 := 'нелли';
  s2 := 'елли';
  AssertTrue(LevDistanceMyersUtf8(s1, s2) = 1);
  AssertTrue(LevDistanceMyersUtf8(s2, s1) = 1);

  s1 := 'нелли';
  s2 := 'еллия';
  AssertTrue(LevDistanceMyersUtf8(s1, s2) = 2);
  AssertTrue(LevDistanceMyersUtf8(s2, s1) = 2);

  s1 := 'мостик';
  s2 := 'костик';
  AssertTrue(LevDistanceMyersUtf8(s1, s2) = 1);
  AssertTrue(LevDistanceMyersUtf8(s2, s1) = 1);

  s1 := 'мостик';
  s2 := 'костяка';
  AssertTrue(LevDistanceMyersUtf8(s1, s2) = 3);
  AssertTrue(LevDistanceMyersUtf8(s2, s1) = 3);

  s1 := 'дистанция';
  s2 := 'дисперсия';
  AssertTrue(LevDistanceMyersUtf8(s1, s2) = 4);
  AssertTrue(LevDistanceMyersUtf8(s2, s1) = 4);

  s1 := 'левенштейн';
  s2 := 'франкенштейн';
  AssertTrue(LevDistanceMyersUtf8(s1, s2) = 5);
  AssertTrue(LevDistanceMyersUtf8(s2, s1) = 5);

  s1 := 'ааааааа';
  s2 := 'ааааааа';
  AssertTrue(LevDistanceMyersUtf8(s1, s2) = 0);

  s1 := 'ааааааа';
  s2 := 'бббббббб';
  AssertTrue(LevDistanceMyersUtf8(s1, s2) = 8);
  AssertTrue(LevDistanceMyersUtf8(s2, s1) = 8);

  s1 := 'аааббаааа';
  s2 := 'ааааааа';
  AssertTrue(LevDistanceMyersUtf8(s1, s2) = 2);
  AssertTrue(LevDistanceMyersUtf8(s2, s1) = 2);

  s1 := 'а';
  s2 := 'б';
  AssertTrue(LevDistanceMyersUtf8(s1, s2) = 1);
  AssertTrue(LevDistanceMyersUtf8(s2, s1) = 1);

  s1 := '一天，在寒冷的冬天，我从森林里走出来。';
  s2 := '一，在寒冷的冬天，我从森林里走来。';
  AssertTrue(LevDistanceMyersUtf8(s1, s2) = 2);
  AssertTrue(LevDistanceMyersUtf8(s2, s1) = 2);
end;

procedure TTestUnicodeUtils.LevenshteinDistMyersDQUtf8;
const
  s1:  string = 'Ну, здравствуйте, здравствуйте. Je vois que je vous fais peur, садитесь и рассказывайте';
  s2:  string = 'Dieu, quelle virulente sortie! – отвечал, нисколько не смутясь такою встречей';
  s3:  string = 'Вы не видали еще, – или: – вы не знакомы с ma tante? – говорила Анна Павловна';
  s4:  string = 'Il va se faire tuer. Dites-moi, pourquoi cette vilaine guerre, – сказала она князю Василию';
  s5:  string = 'Да, я слышал про его план вечного мира, и это очень интересно, но едва ли возможно…';
  s6:  string = 'шепнула Анна Павловна одному. – Le vicomte est un parfait conteur, – проговорила она другому';
  s7:  string = 'Княжна облокотила свою открытую полную руку на столик и не нашла нужным что-либо сказать';
  s8:  string = 'Le charmant Hippolyte поражал своим необыкновенным сходством с сестрою-красавицею';
  s9:  string = 'Ce n’est pas une histoire de revenants? – сказал он, усевшись подле княгини и торопливо пристроив к глазам свой лорнет';
  s10: string = 'сказал князь Ипполит таким тоном, что видно было, – он сказал эти слова, а потом уже понял, что они значили';
  s11: string = 'Средство – европейское равновесие и droit des gens, – говорил аббат ';
  s12: string = 'Образуйте мне этого медведя, – сказал он. – Вот он месяц живет у меня, и в первый раз я его вижу в свете';
  s13: string = 'Пожилая дама носила имя княгини Друбецкой, одной из лучших фамилий России, но она была бедна';
  s14: string = 'Но влияние в свете есть капитал, который надо беречь, чтоб он не исчез. Князь Василий знал это';
begin
  AssertTrue(LevDistanceMyersUtf8(s1, s2) = EditDistanceUtf8(s1, s2, TSeqEditCost.Default));
  AssertTrue(LevDistanceMyersUtf8(s3, s4) = EditDistanceUtf8(s3, s4, TSeqEditCost.Default));
  AssertTrue(LevDistanceMyersUtf8(s5, s6) = EditDistanceUtf8(s5, s6, TSeqEditCost.Default));
  AssertTrue(LevDistanceMyersUtf8(s7, s8) = EditDistanceUtf8(s7, s8, TSeqEditCost.Default));
  AssertTrue(LevDistanceMyersUtf8(s9, s10) = EditDistanceUtf8(s9, s10, TSeqEditCost.Default));
  AssertTrue(LevDistanceMyersUtf8(s11, s12) = EditDistanceUtf8(s11, s12, TSeqEditCost.Default));
  AssertTrue(LevDistanceMyersUtf8(s13, s14) = EditDistanceUtf8(s13, s14, TSeqEditCost.Default));
end;

procedure TTestUnicodeUtils.LevenshteinDistMyersLongUtf8;
const
  s1:  string = 'Этого не обещаю. Вы знаете, как осаждают Кутузова с тех пор, как он назначен главнокомандующим. Он мне сам говорил, что все московские барыни сговорились отдать ему всех своих детей в адъютанты';
  s2:  string = 'Нет, обещайте, обещайте, Basile, – сказала вслед ему Анна Михайловна, с улыбкой молодой кокетки, которая когда-то, должно быть, была ей свойственна, а теперь так не шла к ее истощенному лицу.';
  s3:  string = 'Les souverains? Je ne parle pas de la Russie, – сказал виконт учтиво и безнадежно. – Les souverains, madame? Qu’ont ils fait pour Louis XVI, pour la reine, pour madame Elisabeth? Rien, – продолжал он, одушевляясь.';
  s4:  string = 'И он, презрительно вздохнув, опять переменил положение. Князь Ипполит, долго смотревший в лорнет на виконта, вдруг при этих словах повернулся всем телом к маленькой княгине и, попросив у нее иголку, стал показывать ей, рисуя иголкой на столе, герб Конде.';
  s5:  string = 'Ежели еще год Бонапарте останется на престоле Франции, – продолжал виконт начатый разговор, с видом человека, не слушающего других, но в деле, лучше всех ему известном, следящего только за ходом своих мыслей, – то дела пойдут слишком далеко.';
  s6:  string = 'Не успели еще Анна Павловна и другие улыбкой оценить этих слов виконта, как Пьер опять ворвался в разговор, и Анна Павловна, хотя и предчувствовавшая, что он скажет что-нибудь неприличное, уже не могла остановить его.';
  s7:  string = '– Свобода и равенство, – презрительно сказал виконт, как будто решившийся, наконец, серьезно доказать этому юноше всю глупость его речей, – всё громкие слова, которые уже давно компрометировались.';
  s8:  string = '– Mais, mon cher monsieur Pierre, – сказала Анна Павловна, – как же вы объясняете великого человека, который мог казнить герцога, наконец просто человека, без суда и без вины?';
  s9:  string = '– Как вы хотите, чтоб он всем отвечал вдруг? – сказал князь Андрей. – Притом надо в поступках государственного человека различать поступки частного лица, полководца или императора.';
  s10: string = 'Тут князь Ипполит фыркнул и захохотал гораздо прежде своих слушателей, что произвело невыгодное для рассказчика впечатление. Однако многие, и в том числе пожилая дама и Анна Павловна, улыбнулись.';
  s11: string = 'Пьер был неуклюж. Толстый, выше обыкновенного роста, широкий, с огромными красными руками, он, как говорится, не умел войти в салон и еще менее умел из него выйти, то есть перед выходом сказать что-нибудь особенно приятное.';
  s12: string = 'Два лакея, один княгинин, другой его, дожидаясь, когда они кончат говорить, стояли с шалью и рединготом и слушали их, непонятный им, французский говор с такими лицами, как будто они понимали, что говорится, но не хотели показывать этого.';
  s13: string = 'Князь Ипполит торопливо надел свой редингот, который у него, по-новому, был длиннее пяток, и, путаясь в нем, побежал на крыльцо за княгиней, которую лакей подсаживал в карету.';
  s14: string = '– Ты не понимаешь, отчего я это говорю, – продолжал он. – Ведь это целая история жизни. Ты говоришь, Бонапарте и его карьера, – сказал он, хотя Пьер и не говорил про Бонапарте.';
begin
  AssertTrue(LevDistanceMyersUtf8(s1, s2) = EditDistanceUtf8(s1, s2, TSeqEditCost.Default));
  AssertTrue(LevDistanceMyersUtf8(s3, s4) = EditDistanceUtf8(s3, s4, TSeqEditCost.Default));
  AssertTrue(LevDistanceMyersUtf8(s5, s6) = EditDistanceUtf8(s5, s6, TSeqEditCost.Default));
  AssertTrue(LevDistanceMyersUtf8(s7, s8) = EditDistanceUtf8(s7, s8, TSeqEditCost.Default));
  AssertTrue(LevDistanceMyersUtf8(s9, s10) = EditDistanceUtf8(s9, s10, TSeqEditCost.Default));
  AssertTrue(LevDistanceMyersUtf8(s11, s12) = EditDistanceUtf8(s11, s12, TSeqEditCost.Default));
  AssertTrue(LevDistanceMyersUtf8(s13, s14) = EditDistanceUtf8(s13, s14, TSeqEditCost.Default));
end;

procedure TTestUnicodeUtils.LevenshteinDistMyersBoundedUtf8;
var
  s1, s2: string;
begin
  s1 := '';
  s2 := 'привет';
  AssertTrue(LevDistanceMyersUtf8(s1, s2) = 6);
  AssertTrue(LevDistanceMyersUtf8(s2, s1, 5) = -1);

  s1 := 'аб';
  s2 := 'аа';
  AssertTrue(LevDistanceMyersUtf8(s1, s2) = 1);
  AssertTrue(LevDistanceMyersUtf8(s1, s2, 0) = -1);

  s1 := 'аб';
  s2 := 'ба';
  AssertTrue(LevDistanceMyersUtf8(s1, s2) = 2);
  AssertTrue(LevDistanceMyersUtf8(s1, s2, 1) = -1);

  s1 := 'аб';
  s2 := 'ааа';
  AssertTrue(LevDistanceMyersUtf8(s1, s2) = 2);
  AssertTrue(LevDistanceMyersUtf8(s1, s2, 1) = -1);

  s1 := 'а';
  s2 := 'ббб';
  AssertTrue(LevDistanceMyersUtf8(s1, s2) = 3);
  AssertTrue(LevDistanceMyersUtf8(s1, s2, 2) = -1);

  s1 := 'аабабаб';
  s2 := 'аббаа';
  AssertTrue(LevDistanceMyersUtf8(s1, s2) = 3);
  AssertTrue(LevDistanceMyersUtf8(s1, s2, 2) = -1);

  s1 := 'нелли';
  s2 := 'елли';
  AssertTrue(LevDistanceMyersUtf8(s1, s2) = 1);
  AssertTrue(LevDistanceMyersUtf8(s1, s2, 0) = -1);

  s1 := 'нелли';
  s2 := 'еллия';
  AssertTrue(LevDistanceMyersUtf8(s1, s2) = 2);
  AssertTrue(LevDistanceMyersUtf8(s1, s2, 1) = -1);

  s1 := 'мостик';
  s2 := 'костик';
  AssertTrue(LevDistanceMyersUtf8(s1, s2) = 1);
  AssertTrue(LevDistanceMyersUtf8(s1, s2, 0) = -1);

  s1 := 'мостик';
  s2 := 'костяка';
  AssertTrue(LevDistanceMyersUtf8(s1, s2) = 3);
  AssertTrue(LevDistanceMyersUtf8(s1, s2, 2) = -1);

  s1 := 'дистанция';
  s2 := 'дисперсия';
  AssertTrue(LevDistanceMyersUtf8(s1, s2) = 4);
  AssertTrue(LevDistanceMyersUtf8(s1, s2, 3) = -1);

  s1 := 'левенштейн';
  s2 := 'франкенштейн';
  AssertTrue(LevDistanceMyersUtf8(s1, s2) = 5);
  AssertTrue(LevDistanceMyersUtf8(s1, s2, 4) = -1);

  s1 := 'ааааааа';
  s2 := 'бббббббб';
  AssertTrue(LevDistanceMyersUtf8(s1, s2) = 8);
  AssertTrue(LevDistanceMyersUtf8(s1, s2, 7) = -1);

  s1 := 'аааббаааа';
  s2 := 'ааааааа';
  AssertTrue(LevDistanceMyersUtf8(s1, s2) = 2);
  AssertTrue(LevDistanceMyersUtf8(s1, s2, 1) = -1);

  s1 := 'а';
  s2 := 'б';
  AssertTrue(LevDistanceMyersUtf8(s1, s2) = 1);
  AssertTrue(LevDistanceMyersUtf8(s1, s2, 0) = -1);

  s1 := '一天，在寒冷的冬天，我从森林里走出来。';
  s2 := '一，在寒冷的冬天，我从森林里走来。';
  AssertTrue(LevDistanceMyersUtf8(s1, s2) = 2);
  AssertTrue(LevDistanceMyersUtf8(s2, s1, 1) = -1);
end;

procedure TTestUnicodeUtils.LevenshteinDistMyersDQBoundedUtf8;
const
  s1:  string = 'Ну, здравствуйте, здравствуйте. Je vois que je vous fais peur, садитесь и рассказывайте';
  s2:  string = 'Dieu, quelle virulente sortie![4] – отвечал, нисколько не смутясь такою встречей';
  s3:  string = 'Вы не видали еще, – или: – вы не знакомы с ma tante? – говорила Анна Павловна';
  s4:  string = 'Il va se faire tuer. Dites-moi, pourquoi cette vilaine guerre, – сказала она князю Василию';
  s5:  string = 'Да, я слышал про его план вечного мира, и это очень интересно, но едва ли возможно…';
  s6:  string = 'шепнула Анна Павловна одному. – Le vicomte est un parfait conteur, – проговорила она другому';
  s7:  string = 'Княжна облокотила свою открытую полную руку на столик и не нашла нужным что-либо сказать';
  s8:  string = 'Le charmant Hippolyte поражал своим необыкновенным сходством с сестрою-красавицею';
  s9:  string = 'Ce n’est pas une histoire de revenants? – сказал он, усевшись подле княгини и торопливо пристроив к глазам свой лорнет';
  s10: string = 'сказал князь Ипполит таким тоном, что видно было, – он сказал эти слова, а потом уже понял, что они значили';
  s11: string = 'Средство – европейское равновесие и droit des gens, – говорил аббат ';
  s12: string = 'Образуйте мне этого медведя, – сказал он. – Вот он месяц живет у меня, и в первый раз я его вижу в свете';
  s13: string = 'Пожилая дама носила имя княгини Друбецкой, одной из лучших фамилий России, но она была бедна';
  s14: string = 'Но влияние в свете есть капитал, который надо беречь, чтоб он не исчез. Князь Василий знал это';
begin
  AssertTrue(LevDistanceMyersUtf8(s1, s2, Pred(EditDistanceUtf8(s1, s2, TSeqEditCost.Default))) = -1);
  AssertTrue(LevDistanceMyersUtf8(s3, s4, Pred(EditDistanceUtf8(s3, s4, TSeqEditCost.Default))) = -1);
  AssertTrue(LevDistanceMyersUtf8(s5, s6, Pred(EditDistanceUtf8(s5, s6, TSeqEditCost.Default))) = -1);
  AssertTrue(LevDistanceMyersUtf8(s7, s8, Pred(EditDistanceUtf8(s7, s8, TSeqEditCost.Default))) = -1);
  AssertTrue(LevDistanceMyersUtf8(s9, s10, Pred(EditDistanceUtf8(s9, s10, TSeqEditCost.Default))) = -1);
  AssertTrue(LevDistanceMyersUtf8(s11, s12, Pred(EditDistanceUtf8(s11, s12, TSeqEditCost.Default))) = -1);
  AssertTrue(LevDistanceMyersUtf8(s13, s14, Pred(EditDistanceUtf8(s13, s14, TSeqEditCost.Default))) = -1);
end;

procedure TTestUnicodeUtils.LevenshteinDistMyersLongBoundedUtf8;
const
  s1:  string = 'Этого не обещаю. Вы знаете, как осаждают Кутузова с тех пор, как он назначен главнокомандующим. Он мне сам говорил, что все московские барыни сговорились отдать ему всех своих детей в адъютанты';
  s2:  string = 'Нет, обещайте, обещайте, Basile, – сказала вслед ему Анна Михайловна, с улыбкой молодой кокетки, которая когда-то, должно быть, была ей свойственна, а теперь так не шла к ее истощенному лицу.';
  s3:  string = 'Les souverains? Je ne parle pas de la Russie, – сказал виконт учтиво и безнадежно. – Les souverains, madame? Qu’ont ils fait pour Louis XVI, pour la reine, pour madame Elisabeth? Rien, – продолжал он, одушевляясь.';
  s4:  string = 'И он, презрительно вздохнув, опять переменил положение. Князь Ипполит, долго смотревший в лорнет на виконта, вдруг при этих словах повернулся всем телом к маленькой княгине и, попросив у нее иголку, стал показывать ей, рисуя иголкой на столе, герб Конде.';
  s5:  string = 'Ежели еще год Бонапарте останется на престоле Франции, – продолжал виконт начатый разговор, с видом человека, не слушающего других, но в деле, лучше всех ему известном, следящего только за ходом своих мыслей, – то дела пойдут слишком далеко.';
  s6:  string = 'Не успели еще Анна Павловна и другие улыбкой оценить этих слов виконта, как Пьер опять ворвался в разговор, и Анна Павловна, хотя и предчувствовавшая, что он скажет что-нибудь неприличное, уже не могла остановить его.';
  s7:  string = '– Свобода и равенство, – презрительно сказал виконт, как будто решившийся, наконец, серьезно доказать этому юноше всю глупость его речей, – всё громкие слова, которые уже давно компрометировались.';
  s8:  string = '– Mais, mon cher monsieur Pierre, – сказала Анна Павловна, – как же вы объясняете великого человека, который мог казнить герцога, наконец просто человека, без суда и без вины?';
  s9:  string = '– Как вы хотите, чтоб он всем отвечал вдруг? – сказал князь Андрей. – Притом надо в поступках государственного человека различать поступки частного лица, полководца или императора.';
  s10: string = 'Тут князь Ипполит фыркнул и захохотал гораздо прежде своих слушателей, что произвело невыгодное для рассказчика впечатление. Однако многие, и в том числе пожилая дама и Анна Павловна, улыбнулись.';
  s11: string = 'Пьер был неуклюж. Толстый, выше обыкновенного роста, широкий, с огромными красными руками, он, как говорится, не умел войти в салон и еще менее умел из него выйти, то есть перед выходом сказать что-нибудь особенно приятное.';
  s12: string = 'Два лакея, один княгинин, другой его, дожидаясь, когда они кончат говорить, стояли с шалью и рединготом и слушали их, непонятный им, французский говор с такими лицами, как будто они понимали, что говорится, но не хотели показывать этого.';
  s13: string = 'Князь Ипполит торопливо надел свой редингот, который у него, по-новому, был длиннее пяток, и, путаясь в нем, побежал на крыльцо за княгиней, которую лакей подсаживал в карету.';
  s14: string = '– Ты не понимаешь, отчего я это говорю, – продолжал он. – Ведь это целая история жизни. Ты говоришь, Бонапарте и его карьера, – сказал он, хотя Пьер и не говорил про Бонапарте.';
begin
  AssertTrue(LevDistanceMyersUtf8(s1, s2, Pred(EditDistanceUtf8(s1, s2, TSeqEditCost.Default))) = -1);
  AssertTrue(LevDistanceMyersUtf8(s3, s4, Pred(EditDistanceUtf8(s3, s4, TSeqEditCost.Default))) = -1);
  AssertTrue(LevDistanceMyersUtf8(s5, s6, Pred(EditDistanceUtf8(s5, s6, TSeqEditCost.Default))) = -1);
  AssertTrue(LevDistanceMyersUtf8(s7, s8, Pred(EditDistanceUtf8(s7, s8, TSeqEditCost.Default))) = -1);
  AssertTrue(LevDistanceMyersUtf8(s9, s10, Pred(EditDistanceUtf8(s9, s10, TSeqEditCost.Default))) = -1);
  AssertTrue(LevDistanceMyersUtf8(s11, s12, Pred(EditDistanceUtf8(s11, s12, TSeqEditCost.Default))) = -1);
  AssertTrue(LevDistanceMyersUtf8(s13, s14, Pred(EditDistanceUtf8(s13, s14, TSeqEditCost.Default))) = -1);
end;

procedure TTestUnicodeUtils.LevenshteinDistMyersLongDynUtf8;
const
  s1:  string = 'Этого не обещаю. Вы знаете, как осаждают Кутузова с тех пор, как он назначен главнокомандующим. Он мне сам говорил, что все московские барыни сговорились отдать ему всех своих детей в адъютанты';
  s2:  string = 'Нет, обещайте, обещайте, Basile, – сказала вслед ему Анна Михайловна, с улыбкой молодой кокетки, которая когда-то, должно быть, была ей свойственна, а теперь так не шла к ее истощенному лицу.';
  s3:  string = 'Les souverains? Je ne parle pas de la Russie, – сказал виконт учтиво и безнадежно. – Les souverains, madame? Qu’ont ils fait pour Louis XVI, pour la reine, pour madame Elisabeth? Rien, – продолжал он, одушевляясь.';
  s4:  string = 'И он, презрительно вздохнув, опять переменил положение. Князь Ипполит, долго смотревший в лорнет на виконта, вдруг при этих словах повернулся всем телом к маленькой княгине и, попросив у нее иголку, стал показывать ей, рисуя иголкой на столе, герб Конде.';
  s5:  string = 'Ежели еще год Бонапарте останется на престоле Франции, – продолжал виконт начатый разговор, с видом человека, не слушающего других, но в деле, лучше всех ему известном, следящего только за ходом своих мыслей, – то дела пойдут слишком далеко.';
  s6:  string = 'Не успели еще Анна Павловна и другие улыбкой оценить этих слов виконта, как Пьер опять ворвался в разговор, и Анна Павловна, хотя и предчувствовавшая, что он скажет что-нибудь неприличное, уже не могла остановить его.';
  s7:  string = '– Свобода и равенство, – презрительно сказал виконт, как будто решившийся, наконец, серьезно доказать этому юноше всю глупость его речей, – всё громкие слова, которые уже давно компрометировались.';
  s8:  string = '– Mais, mon cher monsieur Pierre, – сказала Анна Павловна, – как же вы объясняете великого человека, который мог казнить герцога, наконец просто человека, без суда и без вины?';
  s9:  string = '– Как вы хотите, чтоб он всем отвечал вдруг? – сказал князь Андрей. – Притом надо в поступках государственного человека различать поступки частного лица, полководца или императора.';
  s10: string = 'Тут князь Ипполит фыркнул и захохотал гораздо прежде своих слушателей, что произвело невыгодное для рассказчика впечатление. Однако многие, и в том числе пожилая дама и Анна Павловна, улыбнулись.';
  s11: string = 'Пьер был неуклюж. Толстый, выше обыкновенного роста, широкий, с огромными красными руками, он, как говорится, не умел войти в салон и еще менее умел из него выйти, то есть перед выходом сказать что-нибудь особенно приятное.';
  s12: string = 'Два лакея, один княгинин, другой его, дожидаясь, когда они кончат говорить, стояли с шалью и рединготом и слушали их, непонятный им, французский говор с такими лицами, как будто они понимали, что говорится, но не хотели показывать этого.';
  s13: string = 'Князь Ипполит торопливо надел свой редингот, который у него, по-новому, был длиннее пяток, и, путаясь в нем, побежал на крыльцо за княгиней, которую лакей подсаживал в карету.';
  s14: string = '– Ты не понимаешь, отчего я это говорю, – продолжал он. – Ведь это целая история жизни. Ты говоришь, Бонапарте и его карьера, – сказал он, хотя Пьер и не говорил про Бонапарте.';
begin
  AssertTrue(LevDistanceMyersUtf8(s1, s2, -1) = EditDistanceUtf8(s1, s2, TSeqEditCost.Default));
  AssertTrue(LevDistanceMyersUtf8(s3, s4, -1) = EditDistanceUtf8(s3, s4, TSeqEditCost.Default));
  AssertTrue(LevDistanceMyersUtf8(s5, s6, -1) = EditDistanceUtf8(s5, s6, TSeqEditCost.Default));
  AssertTrue(LevDistanceMyersUtf8(s7, s8, -1) = EditDistanceUtf8(s7, s8, TSeqEditCost.Default));
  AssertTrue(LevDistanceMyersUtf8(s9, s10, -1) = EditDistanceUtf8(s9, s10, TSeqEditCost.Default));
  AssertTrue(LevDistanceMyersUtf8(s11, s12, -1) = EditDistanceUtf8(s11, s12, TSeqEditCost.Default));
  AssertTrue(LevDistanceMyersUtf8(s13, s14, -1) = EditDistanceUtf8(s13, s14, TSeqEditCost.Default));
end;

procedure TTestUnicodeUtils.LcsDistWmUtf8;
var
  s1, s2: string;
begin
  s1 := '';
  s2 := 'привет';
  AssertTrue(LcsDistanceWmUtf8(s1, s1) = 0);
  AssertTrue(LcsDistanceWmUtf8(s2, s2) = 0);
  AssertTrue(LcsDistanceWmUtf8(s1, s2) = 6);
  AssertTrue(LcsDistanceWmUtf8(s2, s1) = 6);

  s1 := 'аб';
  s2 := 'аа';
  AssertTrue(LcsDistanceWmUtf8(s1, s2) = 2);
  AssertTrue(LcsDistanceWmUtf8(s2, s1) = 2);

  s1 := 'аб';
  s2 := 'ба';
  AssertTrue(LcsDistanceWmUtf8(s1, s2) = 2);
  AssertTrue(LcsDistanceWmUtf8(s2, s1) = 2);

  s1 := 'аб';
  s2 := 'ааа';
  AssertTrue(LcsDistanceWmUtf8(s1, s2) = 3);
  AssertTrue(LcsDistanceWmUtf8(s2, s1) = 3);

  s1 := 'а';
  s2 := 'ббб';
  AssertTrue(LcsDistanceWmUtf8(s1, s2) = 4);
  AssertTrue(LcsDistanceWmUtf8(s2, s1) = 4);

  s1 := 'аабабаб';
  s2 := 'аббаа';
  AssertTrue(LcsDistanceWmUtf8(s1, s2) = 4);
  AssertTrue(LcsDistanceWmUtf8(s2, s1) = 4);

  s1 := 'нелли';
  s2 := 'елли';
  AssertTrue(LcsDistanceWmUtf8(s1, s2) = 1);
  AssertTrue(LcsDistanceWmUtf8(s2, s1) = 1);

  s1 := 'нелли';
  s2 := 'еллия';
  AssertTrue(LcsDistanceWmUtf8(s1, s2) = 2);
  AssertTrue(LcsDistanceWmUtf8(s2, s1) = 2);

  s1 := 'мостик';
  s2 := 'костик';
  AssertTrue(LcsDistanceWmUtf8(s1, s2) = 2);
  AssertTrue(LcsDistanceWmUtf8(s2, s1) = 2);

  s1 := 'мостик';
  s2 := 'костяка';
  AssertTrue(LcsDistanceWmUtf8(s1, s2) = 5);
  AssertTrue(LcsDistanceWmUtf8(s2, s1) = 5);

  s1 := 'дистанция';
  s2 := 'дисперсия';
  AssertTrue(LcsDistanceWmUtf8(s1, s2) = 8);
  AssertTrue(LcsDistanceWmUtf8(s2, s1) = 8);

  s1 := 'левенштейн';
  s2 := 'франкенштейн';
  AssertTrue(LcsDistanceWmUtf8(s1, s2) = 8);
  AssertTrue(LcsDistanceWmUtf8(s2, s1) = 8);

  s1 := 'ааааааа';
  s2 := 'ааааааа';
  AssertTrue(LcsDistanceWmUtf8(s1, s2) = 0);

  s1 := 'ааааааа';
  s2 := 'бббббббб';
  AssertTrue(LcsDistanceWmUtf8(s1, s2) = 15);
  AssertTrue(LcsDistanceWmUtf8(s2, s1) = 15);

  s1 := 'аааббаааа';
  s2 := 'ааааааа';
  AssertTrue(LcsDistanceWmUtf8(s1, s2) = 2);
  AssertTrue(LcsDistanceWmUtf8(s2, s1) = 2);

  s1 := 'а';
  s2 := 'б';
  AssertTrue(LcsDistanceWmUtf8(s1, s2) = 2);
  AssertTrue(LcsDistanceWmUtf8(s2, s1) = 2);

  s1 := '一天，在寒冷的冬天，我从森林里走出来。';
  s2 := '一，在寒冷的冬天，我从森林里走来。';
  AssertTrue(LcsDistanceWmUtf8(s1, s2) = 2);
  AssertTrue(LcsDistanceWmUtf8(s2, s1) = 2);
end;

procedure TTestUnicodeUtils.LcsDistWmBoundedUtf8;
var
  s1, s2: string;
begin
  s1 := '';
  s2 := 'привет';
  AssertTrue(LcsDistanceWmUtf8(s1, s2) = 6);
  AssertTrue(LcsDistanceWmUtf8(s1, s2, 6) = 6);
  AssertTrue(LcsDistanceWmUtf8(s1, s2, 5) = -1);

  s1 := 'аб';
  s2 := 'аа';
  AssertTrue(LcsDistanceWmUtf8(s1, s2) = 2);
  AssertTrue(LcsDistanceWmUtf8(s1, s2, 3) = 2);
  AssertTrue(LcsDistanceWmUtf8(s1, s2, 2) = 2);
  AssertTrue(LcsDistanceWmUtf8(s1, s2, 1) = -1);

  s1 := 'аб';
  s2 := 'ба';
  AssertTrue(LcsDistanceWmUtf8(s1, s2) = 2);
  AssertTrue(LcsDistanceWmUtf8(s1, s2, 3) = 2);
  AssertTrue(LcsDistanceWmUtf8(s1, s2, 2) = 2);
  AssertTrue(LcsDistanceWmUtf8(s1, s2, 1) = -1);

  s1 := 'аб';
  s2 := 'ааа';
  AssertTrue(LcsDistanceWmUtf8(s1, s2) = 3);
  AssertTrue(LcsDistanceWmUtf8(s1, s2, 4) = 3);
  AssertTrue(LcsDistanceWmUtf8(s1, s2, 3) = 3);
  AssertTrue(LcsDistanceWmUtf8(s1, s2, 2) = -1);

  s1 := 'а';
  s2 := 'ббб';
  AssertTrue(LcsDistanceWmUtf8(s1, s2) = 4);
  AssertTrue(LcsDistanceWmUtf8(s1, s2, 4) = 4);
  AssertTrue(LcsDistanceWmUtf8(s1, s2, 3) = -1);

  s1 := 'аабабаб';
  s2 := 'аббаа';
  AssertTrue(LcsDistanceWmUtf8(s1, s2) = 4);
  AssertTrue(LcsDistanceWmUtf8(s1, s2, 4) = 4);
  AssertTrue(LcsDistanceWmUtf8(s1, s2, 3) = -1);

  s1 := 'нелли';
  s2 := 'елли';
  AssertTrue(LcsDistanceWmUtf8(s1, s2) = 1);
  AssertTrue(LcsDistanceWmUtf8(s1, s2, 1) = 1);
  AssertTrue(LcsDistanceWmUtf8(s1, s2, 0) = -1);

  s1 := 'нелли';
  s2 := 'еллия';
  AssertTrue(LcsDistanceWmUtf8(s1, s2) = 2);
  AssertTrue(LcsDistanceWmUtf8(s1, s2, 2) = 2);
  AssertTrue(LcsDistanceWmUtf8(s1, s2, 1) = -1);

  s1 := 'мостик';
  s2 := 'костик';
  AssertTrue(LcsDistanceWmUtf8(s1, s2) = 2);
  AssertTrue(LcsDistanceWmUtf8(s1, s2, 2) = 2);
  AssertTrue(LcsDistanceWmUtf8(s1, s2, 1) = -1);

  s1 := 'мостик';
  s2 := 'костяка';
  AssertTrue(LcsDistanceWmUtf8(s1, s2) = 5);
  AssertTrue(LcsDistanceWmUtf8(s1, s2, 5) = 5);
  AssertTrue(LcsDistanceWmUtf8(s1, s2, 4) = -1);

  s1 := 'дистанция';
  s2 := 'дисперсия';
  AssertTrue(LcsDistanceWmUtf8(s1, s2) = 8);
  AssertTrue(LcsDistanceWmUtf8(s1, s2, 8) = 8);
  AssertTrue(LcsDistanceWmUtf8(s1, s2, 7) = -1);

  s1 := 'левенштейн';
  s2 := 'франкенштейн';
  AssertTrue(LcsDistanceWmUtf8(s1, s2) = 8);
  AssertTrue(LcsDistanceWmUtf8(s1, s2, 7) = -1);

  s1 := 'ааааааа';
  s2 := 'бббббббб';
  AssertTrue(LcsDistanceWmUtf8(s1, s2) = 15);
  AssertTrue(LcsDistanceWmUtf8(s1, s2, 15) = 15);
  AssertTrue(LcsDistanceWmUtf8(s1, s2, 14) = -1);

  s1 := 'аааббаааа';
  s2 := 'ааааааа';
  AssertTrue(LcsDistanceWmUtf8(s1, s2) = 2);
  AssertTrue(LcsDistanceWmUtf8(s1, s2, 2) = 2);
  AssertTrue(LcsDistanceWmUtf8(s1, s2, 1) = -1);

  s1 := 'а';
  s2 := 'б';
  AssertTrue(LcsDistanceWmUtf8(s1, s2) = 2);
  AssertTrue(LcsDistanceWmUtf8(s1, s2, 2) = 2);
  AssertTrue(LcsDistanceWmUtf8(s1, s2, 1) = -1);

  s1 := '一天，在寒冷的冬天，我从森林里走出来。';
  s2 := '一，在寒冷的冬天，我从森林里走来。';
  AssertTrue(LcsDistanceWmUtf8(s1, s2) = 2);
  AssertTrue(LcsDistanceWmUtf8(s1, s2, 2) = 2);
  AssertTrue(LcsDistanceWmUtf8(s2, s1, 1) = -1);
end;

procedure TTestUnicodeUtils.LcsDistWmDynUtf8;
var
  s1, s2: string;
begin
  s1 := '';
  s2 := 'привет';
  AssertTrue(LcsDistanceWmUtf8(s1, s1, -1) = 0);
  AssertTrue(LcsDistanceWmUtf8(s2, s2, -1) = 0);
  AssertTrue(LcsDistanceWmUtf8(s1, s2, -1) = 6);
  AssertTrue(LcsDistanceWmUtf8(s2, s1, -1) = 6);

  s1 := 'аб';
  s2 := 'аа';
  AssertTrue(LcsDistanceWmUtf8(s1, s2, -1) = 2);
  AssertTrue(LcsDistanceWmUtf8(s2, s1, -1) = 2);

  s1 := 'аб';
  s2 := 'ба';
  AssertTrue(LcsDistanceWmUtf8(s1, s2, -1) = 2);
  AssertTrue(LcsDistanceWmUtf8(s2, s1, -1) = 2);

  s1 := 'аб';
  s2 := 'ааа';
  AssertTrue(LcsDistanceWmUtf8(s1, s2, -1) = 3);
  AssertTrue(LcsDistanceWmUtf8(s2, s1, -1) = 3);

  s1 := 'а';
  s2 := 'ббб';
  AssertTrue(LcsDistanceWmUtf8(s1, s2, -1) = 4);
  AssertTrue(LcsDistanceWmUtf8(s2, s1, -1) = 4);

  s1 := 'аабабаб';
  s2 := 'аббаа';
  AssertTrue(LcsDistanceWmUtf8(s1, s2, -1) = 4);
  AssertTrue(LcsDistanceWmUtf8(s2, s1, -1) = 4);

  s1 := 'нелли';
  s2 := 'елли';
  AssertTrue(LcsDistanceWmUtf8(s1, s2, -1) = 1);
  AssertTrue(LcsDistanceWmUtf8(s2, s1, -1) = 1);

  s1 := 'нелли';
  s2 := 'еллия';
  AssertTrue(LcsDistanceWmUtf8(s1, s2, -1) = 2);
  AssertTrue(LcsDistanceWmUtf8(s2, s1, -1) = 2);

  s1 := 'мостик';
  s2 := 'костик';
  AssertTrue(LcsDistanceWmUtf8(s1, s2, -1) = 2);
  AssertTrue(LcsDistanceWmUtf8(s2, s1, -1) = 2);

  s1 := 'мостик';
  s2 := 'костяка';
  AssertTrue(LcsDistanceWmUtf8(s1, s2, -1) = 5);
  AssertTrue(LcsDistanceWmUtf8(s2, s1, -1) = 5);

  s1 := 'дистанция';
  s2 := 'дисперсия';
  AssertTrue(LcsDistanceWmUtf8(s1, s2, -1) = 8);
  AssertTrue(LcsDistanceWmUtf8(s2, s1, -1) = 8);

  s1 := 'левенштейн';
  s2 := 'франкенштейн';
  AssertTrue(LcsDistanceWmUtf8(s1, s2, -1) = 8);
  AssertTrue(LcsDistanceWmUtf8(s2, s1, -1) = 8);

  s1 := 'ааааааа';
  s2 := 'ааааааа';
  AssertTrue(LcsDistanceWmUtf8(s1, s2, -1) = 0);

  s1 := 'ааааааа';
  s2 := 'бббббббб';
  AssertTrue(LcsDistanceWmUtf8(s1, s2, -1) = 15);
  AssertTrue(LcsDistanceWmUtf8(s2, s1, -1) = 15);

  s1 := 'аааббаааа';
  s2 := 'ааааааа';
  AssertTrue(LcsDistanceWmUtf8(s1, s2, -1) = 2);
  AssertTrue(LcsDistanceWmUtf8(s2, s1, -1) = 2);

  s1 := 'а';
  s2 := 'б';
  AssertTrue(LcsDistanceWmUtf8(s1, s2, -1) = 2);
  AssertTrue(LcsDistanceWmUtf8(s2, s1, -1) = 2);

  s1 := '一天，在寒冷的冬天，我从森林里走出来。';
  s2 := '一，在寒冷的冬天，我从森林里走来。';
  AssertTrue(LcsDistanceWmUtf8(s1, s2, -1) = 2);
  AssertTrue(LcsDistanceWmUtf8(s2, s1, -1) = 2);
end;

procedure TTestUnicodeUtils.DumDistMbrUtf8;
var
  s1, s2: string;
begin
  s1 := '';
  s2 := 'привет';
  AssertTrue(DumDistanceMbrUtf8(s1, s1) = 0);
  AssertTrue(DumDistanceMbrUtf8(s2, s2) = 0);
  AssertTrue(DumDistanceMbrUtf8(s1, s2) = 6);
  AssertTrue(DumDistanceMbrUtf8(s2, s1) = 6);

  s1 := 'аб';
  s2 := 'аа';
  AssertTrue(DumDistanceMbrUtf8(s1, s2) = 1);
  AssertTrue(DumDistanceMbrUtf8(s2, s1) = 1);

  s1 := 'аб';
  s2 := 'ба';
  AssertTrue(DumDistanceMbrUtf8(s1, s2) = 1);
  AssertTrue(DumDistanceMbrUtf8(s2, s1) = 1);

  s1 := 'аc';
  s2 := 'cba';
  AssertTrue(DumDistanceMbrUtf8(s1, s2) = 3);
  AssertTrue(DumDistanceMbrUtf8(s2, s1) = 3);

  s1 := 'а';
  s2 := 'ббб';
  AssertTrue(DumDistanceMbrUtf8(s1, s2) = 3);
  AssertTrue(DumDistanceMbrUtf8(s2, s1) = 3);

  s1 := 'аабабаб';
  s2 := 'аббаа';
  AssertTrue(DumDistanceMbrUtf8(s1, s2) = 3);
  AssertTrue(DumDistanceMbrUtf8(s2, s1) = 3);

  s1 := 'нелли';
  s2 := 'елли';
  AssertTrue(DumDistanceMbrUtf8(s1, s2) = 1);
  AssertTrue(DumDistanceMbrUtf8(s2, s1) = 1);

  s1 := 'нелли';
  s2 := 'елил';
  AssertTrue(DumDistanceMbrUtf8(s1, s2) = 2);
  AssertTrue(DumDistanceMbrUtf8(s2, s1) = 2);

  s1 := 'мостки';
  s2 := 'костик';
  AssertTrue(DumDistanceMbrUtf8(s1, s2) = 2);
  AssertTrue(DumDistanceMbrUtf8(s2, s1) = 2);

  s1 := 'шапито';
  s2 := 'ашипот';
  AssertTrue(DumDistanceMbrUtf8(s1, s2) = 3);
  AssertTrue(DumDistanceMbrUtf8(s2, s1) = 3);

  s1 := 'дистанция';
  s2 := 'дисперсия';
  AssertTrue(LevDistanceMbrUtf8(s1, s2) = 4);
  AssertTrue(LevDistanceMbrUtf8(s2, s1) = 4);

  s1 := 'левенштейн';
  s2 := 'франкенштейн';
  AssertTrue(DumDistanceMbrUtf8(s1, s2) = 5);
  AssertTrue(DumDistanceMbrUtf8(s2, s1) = 5);

  s1 := 'ааааааа';
  s2 := 'ааааааа';
  AssertTrue(DumDistanceMbrUtf8(s1, s2) = 0);

  s1 := 'ааааааа';
  s2 := 'бббббббб';
  AssertTrue(DumDistanceMbrUtf8(s1, s2) = 8);
  AssertTrue(DumDistanceMbrUtf8(s2, s1) = 8);

  s1 := 'аааббаааа';
  s2 := 'ааааааа';
  AssertTrue(DumDistanceMbrUtf8(s1, s2) = 2);
  AssertTrue(DumDistanceMbrUtf8(s2, s1) = 2);

  s1 := 'tests';
  s2 := 'tset';
  AssertTrue(DumDistanceMbrUtf8(s1, s2) = 2);
  AssertTrue(DumDistanceMbrUtf8(s2, s1) = 2);

  s1 := '一天，在寒冷的冬天，我从森林里走出来。';
  s2 := '一，在寒冷的冬天，我从森林里走来。';
  AssertTrue(DumDistanceMbrUtf8(s1, s2) = 2);
  AssertTrue(DumDistanceMbrUtf8(s2, s1) = 2);
end;

procedure TTestUnicodeUtils.DumDistMbrBoundedUtf8;
var
  s1, s2: string;
begin
  s1 := '';
  s2 := 'привет';
  AssertTrue(DumDistanceMbrUtf8(s1, s1, 1) = 0);
  AssertTrue(DumDistanceMbrUtf8(s2, s2, 1) = 0);
  AssertTrue(DumDistanceMbrUtf8(s1, s2, 5) = -1);
  AssertTrue(DumDistanceMbrUtf8(s2, s1, 5) = -1);

  s1 := 'аб';
  s2 := 'аа';
  AssertTrue(DumDistanceMbrUtf8(s1, s2, 0) = -1);
  AssertTrue(DumDistanceMbrUtf8(s2, s1, 0) = -1);

  s1 := 'аб';
  s2 := 'ба';
  AssertTrue(DumDistanceMbrUtf8(s1, s2, 0) = -1);
  AssertTrue(DumDistanceMbrUtf8(s2, s1, 0) = -1);

  s1 := 'аc';
  s2 := 'cba';
  AssertTrue(DumDistanceMbrUtf8(s1, s2, 2) = -1);
  AssertTrue(DumDistanceMbrUtf8(s2, s1, 2) = -1);

  s1 := 'а';
  s2 := 'ббб';
  AssertTrue(DumDistanceMbrUtf8(s1, s2, 2) = -1);
  AssertTrue(DumDistanceMbrUtf8(s2, s1, 2) = -1);

  s1 := 'аабабаб';
  s2 := 'аббаа';
  AssertTrue(DumDistanceMbrUtf8(s1, s2, 2) = -1);
  AssertTrue(DumDistanceMbrUtf8(s2, s1, 2) = -1);

  s1 := 'нелли';
  s2 := 'елли';
  AssertTrue(DumDistanceMbrUtf8(s1, s2, 0) = -1);
  AssertTrue(DumDistanceMbrUtf8(s2, s1, 0) = -1);

  s1 := 'нелли';
  s2 := 'елил';
  AssertTrue(DumDistanceMbrUtf8(s1, s2, 1) = -1);
  AssertTrue(DumDistanceMbrUtf8(s2, s1, 1) = -1);

  s1 := 'мостки';
  s2 := 'костик';
  AssertTrue(DumDistanceMbrUtf8(s1, s2, 1) = -1);
  AssertTrue(DumDistanceMbrUtf8(s2, s1, 1) = -1);

  s1 := 'шапито';
  s2 := 'ашипот';
  AssertTrue(DumDistanceMbrUtf8(s1, s2, 2) = -1);
  AssertTrue(DumDistanceMbrUtf8(s2, s1, 2) = -1);

  s1 := 'дистанция';
  s2 := 'дисперсия';
  AssertTrue(LevDistanceMbrUtf8(s1, s2, 3) = -1);
  AssertTrue(LevDistanceMbrUtf8(s2, s1, 3) = -1);

  s1 := 'левенштейн';
  s2 := 'франкенштейн';
  AssertTrue(DumDistanceMbrUtf8(s1, s2, 4) = -1);
  AssertTrue(DumDistanceMbrUtf8(s2, s1, 4) = -1);

  s1 := 'ааааааа';
  s2 := 'бббббббб';
  AssertTrue(DumDistanceMbrUtf8(s1, s2, 7) = -1);
  AssertTrue(DumDistanceMbrUtf8(s2, s1, 7) = -1);

  s1 := 'аааббаааа';
  s2 := 'ааааааа';
  AssertTrue(DumDistanceMbrUtf8(s1, s2, 1) = -1);
  AssertTrue(DumDistanceMbrUtf8(s2, s1, 1) = -1);

  s1 := 'tests';
  s2 := 'tset';
  AssertTrue(DumDistanceMbrUtf8(s1, s2, 1) = -1);
  AssertTrue(DumDistanceMbrUtf8(s2, s1, 1) = -1);

  s1 := '一天，在寒冷的冬天，我从森林里走出来。';
  s2 := '一，在寒冷的冬天，我从森林里走来。';
  AssertTrue(DumDistanceMbrUtf8(s1, s2, 1) = -1);
  AssertTrue(DumDistanceMbrUtf8(s2, s1, 1) = -1);
end;

procedure TTestUnicodeUtils.DumDistMbrDynUtf8;
var
  s1, s2: string;
begin
  s1 := '';
  s2 := 'привет';
  AssertTrue(DumDistanceMbrUtf8(s1, s1, -1) = 0);
  AssertTrue(DumDistanceMbrUtf8(s2, s2, -1) = 0);
  AssertTrue(DumDistanceMbrUtf8(s1, s2, -1) = 6);
  AssertTrue(DumDistanceMbrUtf8(s2, s1, -1) = 6);

  s1 := 'аб';
  s2 := 'аа';
  AssertTrue(DumDistanceMbrUtf8(s1, s2, -1) = 1);
  AssertTrue(DumDistanceMbrUtf8(s2, s1, -1) = 1);

  s1 := 'аб';
  s2 := 'ба';
  AssertTrue(DumDistanceMbrUtf8(s1, s2, -1) = 1);
  AssertTrue(DumDistanceMbrUtf8(s2, s1, -1) = 1);

  s1 := 'аc';
  s2 := 'cba';
  AssertTrue(DumDistanceMbrUtf8(s1, s2, -1) = 3);
  AssertTrue(DumDistanceMbrUtf8(s2, s1, -1) = 3);

  s1 := 'а';
  s2 := 'ббб';
  AssertTrue(DumDistanceMbrUtf8(s1, s2, -1) = 3);
  AssertTrue(DumDistanceMbrUtf8(s2, s1, -1) = 3);

  s1 := 'аабабаб';
  s2 := 'аббаа';
  AssertTrue(DumDistanceMbrUtf8(s1, s2, -1) = 3);
  AssertTrue(DumDistanceMbrUtf8(s2, s1, -1) = 3);

  s1 := 'нелли';
  s2 := 'елли';
  AssertTrue(DumDistanceMbrUtf8(s1, s2, -1) = 1);
  AssertTrue(DumDistanceMbrUtf8(s2, s1, -1) = 1);

  s1 := 'нелли';
  s2 := 'елил';
  AssertTrue(DumDistanceMbrUtf8(s1, s2, -1) = 2);
  AssertTrue(DumDistanceMbrUtf8(s2, s1, -1) = 2);

  s1 := 'мостки';
  s2 := 'костик';
  AssertTrue(DumDistanceMbrUtf8(s1, s2, -1) = 2);
  AssertTrue(DumDistanceMbrUtf8(s2, s1, -1) = 2);

  s1 := 'шапито';
  s2 := 'ашипот';
  AssertTrue(DumDistanceMbrUtf8(s1, s2, -1) = 3);
  AssertTrue(DumDistanceMbrUtf8(s2, s1, -1) = 3);

  s1 := 'дистанция';
  s2 := 'дисперсия';
  AssertTrue(LevDistanceMbrUtf8(s1, s2, -1) = 4);
  AssertTrue(LevDistanceMbrUtf8(s2, s1, -1) = 4);

  s1 := 'левенштейн';
  s2 := 'франкенштейн';
  AssertTrue(DumDistanceMbrUtf8(s1, s2, -1) = 5);
  AssertTrue(DumDistanceMbrUtf8(s2, s1, -1) = 5);

  s1 := 'ааааааа';
  s2 := 'ааааааа';
  AssertTrue(DumDistanceMbrUtf8(s1, s2, -1) = 0);

  s1 := 'ааааааа';
  s2 := 'бббббббб';
  AssertTrue(DumDistanceMbrUtf8(s1, s2, -1) = 8);
  AssertTrue(DumDistanceMbrUtf8(s2, s1, -1) = 8);

  s1 := 'аааббаааа';
  s2 := 'ааааааа';
  AssertTrue(DumDistanceMbrUtf8(s1, s2, -1) = 2);
  AssertTrue(DumDistanceMbrUtf8(s2, s1, -1) = 2);

  s1 := 'tests';
  s2 := 'tset';
  AssertTrue(DumDistanceMbrUtf8(s1, s2, -1) = 2);
  AssertTrue(DumDistanceMbrUtf8(s2, s1, -1) = 2);

  s1 := '一天，在寒冷的冬天，我从森林里走出来。';
  s2 := '一，在寒冷的冬天，我从森林里走来。';
  AssertTrue(DumDistanceMbrUtf8(s1, s2, -1) = 2);
  AssertTrue(DumDistanceMbrUtf8(s2, s1, -1) = 2);
end;

procedure TTestUnicodeUtils.LcsGusUtf8Test;
var
  s1: string = '';
  s2: string = '';
  s: string;
begin
  AssertTrue(LcsGusUtf8(s1, s2) = '');

  s1 := 'аа';
  AssertTrue(LcsGusUtf8(s1, s2) = '');

  s2 := 'аб';
  s := 'а';
  AssertTrue(LcsGusUtf8(s1, s2) = s);

  s1 := 'тесть';
  s2 := 'это просто тест';
  s := 'тест';
  AssertTrue(LcsGusUtf8(s1, s2) = s);

  s1 := '1234';
  s2 := '1224533324';
  s := '1234';
  AssertTrue(LcsGusUtf8(s1, s2) = s);

  s1 := '01236789';
  s2 := '01234445556789';
  s := '01236789';
  AssertTrue(LcsGusUtf8(s1, s2) = s);
end;

procedure TTestUnicodeUtils.LcsKRUtf8Test;
var
  s1: string = '';
  s2: string = '';
  s: string;
begin
  AssertTrue(LcsKRUtf8(s1, s2) = '');

  s1 := 'аа';
  AssertTrue(LcsKRUtf8(s1, s2) = '');

  s2 := 'аб';
  s := 'а';
  AssertTrue(LcsKRUtf8(s1, s2) = s);

  s1 := 'тесть';
  s2 := 'это просто тест';
  s := 'тест';
  AssertTrue(LcsKRUtf8(s1, s2) = s);

  s1 := '1234';
  s2 := '1224533324';
  s := '1234';
  AssertTrue(LcsKRUtf8(s1, s2) = s);

  s1 := '01236789';
  s2 := '01234445556789';
  s := '01236789';
  AssertTrue(LcsKRUtf8(s1, s2) = s);
end;

procedure TTestUnicodeUtils.LcsMyersUtf8Test;
var
  s1: string = '';
  s2: string = '';
  s: string;
begin
  AssertTrue(LcsMyersUtf8(s1, s2) = '');

  s1 := 'аа';
  AssertTrue(LcsMyersUtf8(s1, s2) = '');

  s2 := 'аб';
  s := 'а';
  AssertTrue(LcsMyersUtf8(s1, s2) = s);

  s1 := 'тесть';
  s2 := 'это просто тест';
  s := 'тест';
  AssertTrue(LcsMyersUtf8(s1, s2) = s);

  s1 := '1234';
  s2 := '1224533324';
  s := '1234';
  AssertTrue(LcsMyersUtf8(s1, s2) = s);

  s1 := '01236789';
  s2 := '01234445556789';
  s := '01236789';
  AssertTrue(LcsMyersUtf8(s1, s2) = s);
end;

procedure TTestUnicodeUtils.SimRatioUtf8Test;
var
  s1, s2: string;
begin
  s1 := '';
  s2 := '';
  AssertTrue(SimRatioUtf8(s1, s2) = DblOne);

  s2 := 'ббб';
  AssertTrue(SimRatioUtf8(s1, s2) = DblZero);
  AssertTrue(SimRatioUtf8(s2, s1) = DblZero);

  s1 := 'ааа';
  AssertTrue(SimRatioUtf8(s1, s2) = DblZero);
  AssertTrue(SimRatioUtf8(s2, s1) = DblZero);

  s1 := 'ббб';
  AssertTrue(SimRatioUtf8(s1, s2) = DblOne);
  AssertTrue(SimRatioUtf8(s2, s1) = DblOne);
end;

procedure TTestUnicodeUtils.SimRatioExUtf8Test;
begin
  AssertTrue(SimRatioExUtf8('', '', [' ']) = DblOne);
  AssertTrue(SimRatioExUtf8('aaa', '', [' ']) = DblZero);
  AssertTrue(SimRatioExUtf8('', 'bbb', [' ']) = DblZero);
  AssertTrue(SimRatioExUtf8('Hello world', ' Hello world ', [' ']) = DblOne);
  AssertFalse(SimRatioExUtf8('Hello world', ' hello world ', [' ']) = DblOne);
  AssertTrue(SimRatioExUtf8('Hello world', ' hello world ', [' '], smSimple, [soIgnoreCase]) = DblOne);
  AssertTrue(SimRatioExUtf8('Hello world', 'another Hello world ', [' '], smSimple, [soPartial]) = DblOne);
  AssertTrue(SimRatioExUtf8('Hello, world!', ' hello world ', [' ',',','!'], smSimple, [soIgnoreCase]) = DblOne);
  AssertTrue(SimRatioExUtf8('World hello', ' Hello world ', [' '], smTokenSort, [soIgnoreCase]) = DblOne);
  AssertTrue(SimRatioExUtf8('World hello', ' Hello world, hello', [' ',','], smTokenSet, [soIgnoreCase]) = DblOne);
  AssertTrue(SimRatioExUtf8('World hello', ' Hello another world, hello', [' ',','], smTokenSet, [soIgnoreCase, soPartial]) = DblOne);
  AssertTrue(SimRatioExUtf8('fuzzy was a bear', 'fuzzy fuzzy fuzzy bear', [' '], smTokenSetEx) = DblOne);
end;

procedure TTestUnicodeUtils.FuzzySearchEdp;
var
  p, p1: string;
  I, J, Len, k: Integer;
  m: TFuzzySearchEdp;
const
  Text: string = 'The Levenshtein distance can also be computed between two longer strings, but the cost to compute it, which is roughly proportional to the product of the two string lengths, makes this impractical';
begin
  p := '';
  k := 0;
  J := 0;
  m := TFuzzySearchEdp.Create(p);
  for I in m.Matches(Text, k) do
    Inc(J);
  AssertTrue(J = 0);

  p := 'can';
  k := 3;
  m := TFuzzySearchEdp.Create(p);
  for I in m.Matches(Text, k) do
    Inc(J);
  AssertTrue(J = 0);

  p := 'Levenhstein';
  k := 2;
  m := TFuzzySearchEdp.Create(p);
  for I in m.Matches(Text, k) do
    begin
      Len := Min(Utf8Length(p), I);
      p1 := Utf8Copy(Text, Succ(I - Len), Len);
      AssertTrue(LevDistanceMbrUtf8(p, p1) <= k);
      Inc(J);
    end;
  AssertTrue(J = 1);

  p := 'strung';
  k := 1;
  J := 0;
  m := TFuzzySearchEdp.Create(p);
  for I in m.Matches(Text, k) do
    begin
      Len := Min(Utf8Length(p), I);
      p1 := Utf8Copy(Text, Succ(I - Len), Len);
      AssertTrue(LevDistanceMbrUtf8(p, p1) <= k);
      Inc(J);
    end;
  AssertTrue(J = 2);

  p := 'lung';
  k := 1;
  J := 0;
  m := TFuzzySearchEdp.Create(p);
  for I in m.Matches(Text, k) do
    begin
      Len := Min(Utf8Length(p), I);
      p1 := Utf8Copy(Text, Succ(I - Len), Len);
      AssertTrue(LevDistanceMbrUtf8(p, p1) <= k);
      Inc(J);
    end;
  AssertTrue(J = 2);
end;

procedure TTestUnicodeUtils.Utf8HashTextTest;
var
  s1, s2, s3, s4: string;
  h: TBufHash32;
begin
  s1 := 'Привет, мир!';
  s2 := 'Привет, Мир!';
  s3 := 'Привет,'#$e2#$80#$84'мир!';
  s4 := 'Привет,'#$e2#$80#$84'Мир!';
  h := @TxxHash32LE.HashBuf;

  AssertFalse(Utf8HashText(s1, h) = Utf8HashText(s2, h));
  AssertFalse(Utf8HashText(s1, h) = Utf8HashText(s3, h));
  AssertFalse(Utf8HashText(s1, h) = Utf8HashText(s4, h));

  AssertTrue(Utf8HashText(s1, h, [scoIgnoreCase]) = Utf8HashText(s2, h, [scoIgnoreCase]));
  AssertTrue(Utf8HashText(s1, h, [scoIgnoreWS]) = Utf8HashText(s3, h, [scoIgnoreWS]));
  AssertTrue(Utf8HashText(s1, h, [scoIgnoreWS, scoIgnoreCase]) = Utf8HashText(s4, h, [scoIgnoreWS, scoIgnoreCase]));
end;

procedure TTestUnicodeUtils.Utf8HashText64Test;
var
  s1, s2, s3, s4: string;
  h: TBufHash64;
begin
  s1 := 'Привет, мир!';
  s2 := 'Привет, Мир!';
  s3 := 'Привет,'#$e2#$80#$84'мир!';
  s4 := 'Привет,'#$e2#$80#$84'Мир!';
  h := @TxxHash64LE.HashBuf;

  AssertFalse(Utf8HashText64(s1, h) = Utf8HashText64(s2, h));
  AssertFalse(Utf8HashText64(s1, h) = Utf8HashText64(s3, h));
  AssertFalse(Utf8HashText64(s1, h) = Utf8HashText64(s4, h));

  AssertTrue(Utf8HashText64(s1, h, [scoIgnoreCase]) = Utf8HashText64(s2, h, [scoIgnoreCase]));
  AssertTrue(Utf8HashText64(s1, h, [scoIgnoreWS]) = Utf8HashText64(s3, h, [scoIgnoreWS]));
  AssertTrue(Utf8HashText64(s1, h, [scoIgnoreWS, scoIgnoreCase]) = Utf8HashText64(s4, h, [scoIgnoreWS, scoIgnoreCase]));
end;

procedure TTestUnicodeUtils.Utf8SameTextTest;
var
  s1, s2, s3, s4: string;
begin
  AssertTrue(Utf8SameText('', '', [scoIgnoreWS, scoIgnoreCase]));
  AssertFalse(Utf8SameText('', ' '));
  AssertTrue(Utf8SameText('', ' ', [scoIgnoreWS]));

  s1 := 'Привет, мир!';
  s2 := 'Привет, Мир!';
  s3 := 'Привет,'#$e2#$80#$84'мир!';
  s4 := 'Привет,'#$e2#$80#$84'Мир!';

  AssertFalse(Utf8SameText(s1, s2));
  AssertFalse(Utf8SameText(s1, s3));
  AssertFalse(Utf8SameText(s1, s4));

  AssertTrue(Utf8SameText(s1, s2, [scoIgnoreCase]));
  AssertTrue(Utf8SameText(s1, s3, [scoIgnoreWS]));
  AssertTrue(Utf8SameText(s1, s4, [scoIgnoreWS, scoIgnoreCase]));
end;

{$WARN 4104 OFF}
procedure TTestUnicodeUtils.Utf16ToUcs4;
const
  s1:  string = 'Ну, здравствуйте, здравствуйте. Je vois que je vous fais peur, садитесь и рассказывайте';
  s2:  string = 'Dieu, quelle virulente sortie![4] – отвечал, нисколько не смутясь такою встречей';
  s3:  string = 'Вы не видали еще, – или: – вы не знакомы с ma tante? – говорила Анна Павловна';
  s4:  string = 'Il va se faire tuer. Dites-moi, pourquoi cette vilaine guerre, – сказала она князю Василию';
  s5:  string = 'Да, я слышал про его план вечного мира, и это очень интересно, но едва ли возможно…';
  s6:  string = 'шепнула Анна Павловна одному. – Le vicomte est un parfait conteur, – проговорила она другому';
  s7:  string = 'Княжна облокотила свою открытую полную руку на столик и не нашла нужным что-либо сказать';
  s8:  string = 'Le charmant Hippolyte поражал своим необыкновенным сходством с сестрою-красавицею';
  s9:  string = 'Ce n’est pas une histoire de revenants? – сказал он, усевшись подле княгини и торопливо пристроив к глазам свой лорнет';
  s10: string = '悲しい時間！ おお魅力！';
begin
  AssertTrue('empty string', Ucs4SeqToUtf16(Utf16ToUcs4Seq('')) = '');
  AssertTrue('s1', Ucs4SeqToUtf16(Utf16ToUcs4Seq(s1)) = s1);
  AssertTrue('s2', Ucs4SeqToUtf16(Utf16ToUcs4Seq(s2)) = s2);
  AssertTrue('s3', Ucs4SeqToUtf16(Utf16ToUcs4Seq(s3)) = s3);
  AssertTrue('s4', Ucs4SeqToUtf16(Utf16ToUcs4Seq(s4)) = s4);
  AssertTrue('s5', Ucs4SeqToUtf16(Utf16ToUcs4Seq(s5)) = s5);
  AssertTrue('s6', Ucs4SeqToUtf16(Utf16ToUcs4Seq(s6)) = s6);
  AssertTrue('s7', Ucs4SeqToUtf16(Utf16ToUcs4Seq(s7)) = s7);
  AssertTrue('s8', Ucs4SeqToUtf16(Utf16ToUcs4Seq(s8)) = s8);
  AssertTrue('s9', Ucs4SeqToUtf16(Utf16ToUcs4Seq(s9)) = s9);
  AssertTrue('s10', Ucs4SeqToUtf16(Utf16ToUcs4Seq(s10)) = s10);
end;

procedure TTestUnicodeUtils.SubSequenceUtf8;
var
  Ucs4Seq: TUcs4Seq;
  s: string;
begin
  s := '';
  AssertTrue(IsSubSequenceUtf8(s, ''));
  AssertFalse(IsSubSequenceUtf8(s, 'a'));
  s := 'a';
  AssertTrue(IsSubSequenceUtf8(s, ''));
  Ucs4Seq := [$41, $10331, $43, $10333, $45, $10334, $47, $10336]; //ascii + Gothic letters
  s := Ucs4SeqToUtf8(Ucs4Seq);
  AssertTrue(IsSubSequenceUtf8(s, Copy(s, 2, Length(s))));
  AssertFalse(IsSubSequenceUtf8(s, Copy(s, 3, Length(s))));
  AssertFalse(IsSubSequenceUtf8(s, Copy(s, 4, Length(s))));
  AssertTrue(IsSubSequenceUtf8(s, Utf8Copy(s, 3, 4)));
  AssertTrue(IsSubSequenceUtf8(s, Copy(s, 6, Length(s))));
  AssertTrue(IsSubSequenceUtf8(s, Copy(s, 7, Length(s))));
  AssertFalse(IsSubSequenceUtf8(s, Copy(s, 8, Length(s))));
end;

procedure TTestUnicodeUtils.EditDistUtf16;
var
  s1, s2: string;
begin
  s1 := '';
  s2 := 'привет';
  AssertTrue(EditDistanceUtf16(s1, s1, TSeqEditCost.Default) = 0);
  AssertTrue(EditDistanceUtf16(s2, s2, TSeqEditCost.Default) = 0);
  AssertTrue(EditDistanceUtf16(s1, s2, TSeqEditCost.Default) = 6);
  AssertTrue(EditDistanceUtf16(s2, s1, TSeqEditCost.Default) = 6);

  s1 := 'аб';
  s2 := 'аа';
  AssertTrue(EditDistanceUtf16(s1, s2, TSeqEditCost.Default) = 1);
  AssertTrue(EditDistanceUtf16(s2, s1, TSeqEditCost.Default) = 1);

  s1 := 'аб';
  s2 := 'ба';
  AssertTrue(EditDistanceUtf16(s1, s2, TSeqEditCost.Default) = 2);
  AssertTrue(EditDistanceUtf16(s2, s1, TSeqEditCost.Default) = 2);

  s1 := 'аб';
  s2 := 'ааа';
  AssertTrue(EditDistanceUtf16(s1, s2, TSeqEditCost.Default) = 2);
  AssertTrue(EditDistanceUtf16(s2, s1, TSeqEditCost.Default) = 2);

  s1 := 'а';
  s2 := 'ббб';
  AssertTrue(EditDistanceUtf16(s1, s2, TSeqEditCost.Default) = 3);
  AssertTrue(EditDistanceUtf16(s2, s1, TSeqEditCost.Default) = 3);

  s1 := 'аабабаб';
  s2 := 'аббаа';
  AssertTrue(EditDistanceUtf16(s1, s2, TSeqEditCost.Default) = 3);
  AssertTrue(EditDistanceUtf16(s2, s1, TSeqEditCost.Default) = 3);

  s1 := 'нелли';
  s2 := 'елли';
  AssertTrue(EditDistanceUtf16(s1, s2, TSeqEditCost.Default) = 1);
  AssertTrue(EditDistanceUtf16(s2, s1, TSeqEditCost.Default) = 1);

  s1 := 'нелли';
  s2 := 'еллия';
  AssertTrue(EditDistanceUtf16(s1, s2, TSeqEditCost.Default) = 2);
  AssertTrue(EditDistanceUtf16(s2, s1, TSeqEditCost.Default) = 2);

  s1 := 'мостик';
  s2 := 'костик';
  AssertTrue(EditDistanceUtf16(s1, s2, TSeqEditCost.Default) = 1);
  AssertTrue(EditDistanceUtf16(s2, s1, TSeqEditCost.Default) = 1);

  s1 := 'мостик';
  s2 := 'костяка';
  AssertTrue(EditDistanceUtf16(s1, s2, TSeqEditCost.Default) = 3);
  AssertTrue(EditDistanceUtf16(s2, s1, TSeqEditCost.Default) = 3);

  s1 := 'дистанция';
  s2 := 'дисперсия';
  AssertTrue(EditDistanceUtf16(s1, s2, TSeqEditCost.Default) = 4);
  AssertTrue(EditDistanceUtf16(s2, s1, TSeqEditCost.Default) = 4);

  s1 := 'левенштейн';
  s2 := 'франкенштейн';
  AssertTrue(EditDistanceUtf16(s1, s2, TSeqEditCost.Default) = 5);
  AssertTrue(EditDistanceUtf16(s2, s1, TSeqEditCost.Default) = 5);

  s1 := 'ааааааа';
  s2 := 'ааааааа';
  AssertTrue(EditDistanceUtf16(s1, s2, TSeqEditCost.Default) = 0);

  s1 := 'ааааааа';
  s2 := 'бббббббб';
  AssertTrue(EditDistanceUtf16(s1, s2, TSeqEditCost.Default) = 8);
  AssertTrue(EditDistanceUtf16(s2, s1, TSeqEditCost.Default) = 8);

  s1 := 'аааббаааа';
  s2 := 'ааааааа';
  AssertTrue(EditDistanceUtf16(s1, s2, TSeqEditCost.Default) = 2);
  AssertTrue(EditDistanceUtf16(s2, s1, TSeqEditCost.Default) = 2);

  s1 := 'а';
  s2 := 'б';
  AssertTrue(EditDistanceUtf16(s1, s2, TSeqEditCost.Default) = 1);
  AssertTrue(EditDistanceUtf16(s2, s1, TSeqEditCost.Default) = 1);

  s1 := '一天，在寒冷的冬天，我从森林里走出来。';
  s2 := '一，在寒冷的冬天，我从森林里走来。';
  AssertTrue(EditDistanceUtf16(s1, s2, TSeqEditCost.Default) = 2);
  AssertTrue(EditDistanceUtf16(s2, s1, TSeqEditCost.Default) = 2);
end;

procedure TTestUnicodeUtils.LevenshteinDistMbrUtf16;
var
  s1, s2: string;
begin
  s1 := '';
  s2 := 'привет';
  AssertTrue(LevDistanceMbrUtf16(s1, s1) = 0);
  AssertTrue(LevDistanceMbrUtf16(s2, s2) = 0);
  AssertTrue(LevDistanceMbrUtf16(s1, s2) = 6);
  AssertTrue(LevDistanceMbrUtf16(s2, s1) = 6);

  s1 := 'аб';
  s2 := 'аа';
  AssertTrue(LevDistanceMbrUtf16(s1, s2) = 1);
  AssertTrue(LevDistanceMbrUtf16(s2, s1) = 1);

  s1 := 'аб';
  s2 := 'ба';
  AssertTrue(LevDistanceMbrUtf16(s1, s2) = 2);
  AssertTrue(LevDistanceMbrUtf16(s2, s1) = 2);

  s1 := 'аб';
  s2 := 'ааа';
  AssertTrue(LevDistanceMbrUtf16(s1, s2) = 2);
  AssertTrue(LevDistanceMbrUtf16(s2, s1) = 2);

  s1 := 'а';
  s2 := 'ббб';
  AssertTrue(LevDistanceMbrUtf16(s1, s2) = 3);
  AssertTrue(LevDistanceMbrUtf16(s2, s1) = 3);

  s1 := 'аабабаб';
  s2 := 'аббаа';
  AssertTrue(LevDistanceMbrUtf16(s1, s2) = 3);
  AssertTrue(LevDistanceMbrUtf16(s2, s1) = 3);

  s1 := 'нелли';
  s2 := 'елли';
  AssertTrue(LevDistanceMbrUtf16(s1, s2) = 1);
  AssertTrue(LevDistanceMbrUtf16(s2, s1) = 1);

  s1 := 'нелли';
  s2 := 'еллия';
  AssertTrue(LevDistanceMbrUtf16(s1, s2) = 2);
  AssertTrue(LevDistanceMbrUtf16(s2, s1) = 2);

  s1 := 'мостик';
  s2 := 'костик';
  AssertTrue(LevDistanceMbrUtf16(s1, s2) = 1);
  AssertTrue(LevDistanceMbrUtf16(s2, s1) = 1);

  s1 := 'мостик';
  s2 := 'костяка';
  AssertTrue(LevDistanceMbrUtf16(s1, s2) = 3);
  AssertTrue(LevDistanceMbrUtf16(s2, s1) = 3);

  s1 := 'дистанция';
  s2 := 'дисперсия';
  AssertTrue(LevDistanceMbrUtf16(s1, s2) = 4);
  AssertTrue(LevDistanceMbrUtf16(s2, s1) = 4);

  s1 := 'левенштейн';
  s2 := 'франкенштейн';
  AssertTrue(LevDistanceMbrUtf16(s1, s2) = 5);
  AssertTrue(LevDistanceMbrUtf16(s2, s1) = 5);

  s1 := 'ааааааа';
  s2 := 'ааааааа';
  AssertTrue(LevDistanceMbrUtf16(s1, s2) = 0);

  s1 := 'ааааааа';
  s2 := 'бббббббб';
  AssertTrue(LevDistanceMbrUtf16(s1, s2) = 8);
  AssertTrue(LevDistanceMbrUtf16(s2, s1) = 8);

  s1 := 'аааббаааа';
  s2 := 'ааааааа';
  AssertTrue(LevDistanceMbrUtf16(s1, s2) = 2);
  AssertTrue(LevDistanceMbrUtf16(s2, s1) = 2);

  s1 := 'а';
  s2 := 'б';
  AssertTrue(LevDistanceMbrUtf16(s1, s2) = 1);
  AssertTrue(LevDistanceMbrUtf16(s2, s1) = 1);

  s1 := '一天，在寒冷的冬天，我从森林里走出来。';
  s2 := '一，在寒冷的冬天，我从森林里走来。';
  AssertTrue(LevDistanceMbrUtf16(s1, s2) = 2);
  AssertTrue(LevDistanceMbrUtf16(s2, s1) = 2);
end;

procedure TTestUnicodeUtils.LevenshteinDistMbrBoundedUtf16;
var
  s1, s2: string;
begin
  s1 := '';
  s2 := 'привет';
  AssertTrue(LevDistanceMbrUtf16(s1, s2) = 6);
  AssertTrue(LevDistanceMbrUtf16(s2, s1, 5) = -1);

  s1 := 'аб';
  s2 := 'аа';
  AssertTrue(LevDistanceMbrUtf16(s1, s2) = 1);
  AssertTrue(LevDistanceMbrUtf16(s1, s2, 0) = -1);

  s1 := 'аб';
  s2 := 'ба';
  AssertTrue(LevDistanceMbrUtf16(s1, s2) = 2);
  AssertTrue(LevDistanceMbrUtf16(s1, s2, 1) = -1);

  s1 := 'аб';
  s2 := 'ааа';
  AssertTrue(LevDistanceMbrUtf16(s1, s2) = 2);
  AssertTrue(LevDistanceMbrUtf16(s1, s2, 1) = -1);

  s1 := 'а';
  s2 := 'ббб';
  AssertTrue(LevDistanceMbrUtf16(s1, s2) = 3);
  AssertTrue(LevDistanceMbrUtf16(s1, s2, 2) = -1);

  s1 := 'аабабаб';
  s2 := 'аббаа';
  AssertTrue(LevDistanceMbrUtf16(s1, s2) = 3);
  AssertTrue(LevDistanceMbrUtf16(s1, s2, 2) = -1);

  s1 := 'нелли';
  s2 := 'елли';
  AssertTrue(LevDistanceMbrUtf16(s1, s2) = 1);
  AssertTrue(LevDistanceMbrUtf16(s1, s2, 0) = -1);

  s1 := 'нелли';
  s2 := 'еллия';
  AssertTrue(LevDistanceMbrUtf16(s1, s2) = 2);
  AssertTrue(LevDistanceMbrUtf16(s1, s2, 1) = -1);

  s1 := 'мостик';
  s2 := 'костик';
  AssertTrue(LevDistanceMbrUtf16(s1, s2) = 1);
  AssertTrue(LevDistanceMbrUtf16(s1, s2, 0) = -1);

  s1 := 'мостик';
  s2 := 'костяка';
  AssertTrue(LevDistanceMbrUtf16(s1, s2) = 3);
  AssertTrue(LevDistanceMbrUtf16(s1, s2, 2) = -1);

  s1 := 'дистанция';
  s2 := 'дисперсия';
  AssertTrue(LevDistanceMbrUtf16(s1, s2) = 4);
  AssertTrue(LevDistanceMbrUtf16(s1, s2, 3) = -1);

  s1 := 'левенштейн';
  s2 := 'франкенштейн';
  AssertTrue(LevDistanceMbrUtf16(s1, s2) = 5);
  AssertTrue(LevDistanceMbrUtf16(s1, s2, 4) = -1);

  s1 := 'ааааааа';
  s2 := 'бббббббб';
  AssertTrue(LevDistanceMbrUtf16(s1, s2) = 8);
  AssertTrue(LevDistanceMbrUtf16(s1, s2, 7) = -1);

  s1 := 'аааббаааа';
  s2 := 'ааааааа';
  AssertTrue(LevDistanceMbrUtf16(s1, s2) = 2);
  AssertTrue(LevDistanceMbrUtf16(s1, s2, 1) = -1);

  s1 := 'а';
  s2 := 'б';
  AssertTrue(LevDistanceMbrUtf16(s1, s2) = 1);
  AssertTrue(LevDistanceMbrUtf16(s1, s2, 0) = -1);

  s1 := '一天，在寒冷的冬天，我从森林里走出来。';
  s2 := '一，在寒冷的冬天，我从森林里走来。';
  AssertTrue(LevDistanceMbrUtf16(s1, s2) = 2);
  AssertTrue(LevDistanceMbrUtf16(s2, s1, 1) = -1);
end;

procedure TTestUnicodeUtils.LevenshteinDistMbrDynUtf16;
var
  s1, s2: string;
begin
  s1 := '';
  s2 := 'привет';
  AssertTrue(LevDistanceMbrUtf16(s1, s1, -1) = 0);
  AssertTrue(LevDistanceMbrUtf16(s2, s2, -1) = 0);
  AssertTrue(LevDistanceMbrUtf16(s1, s2, -1) = 6);
  AssertTrue(LevDistanceMbrUtf16(s2, s1, -1) = 6);

  s1 := 'аб';
  s2 := 'аа';
  AssertTrue(LevDistanceMbrUtf16(s1, s2, -1) = 1);
  AssertTrue(LevDistanceMbrUtf16(s2, s1, -1) = 1);

  s1 := 'аб';
  s2 := 'ба';
  AssertTrue(LevDistanceMbrUtf16(s1, s2, -1) = 2);
  AssertTrue(LevDistanceMbrUtf16(s2, s1, -1) = 2);

  s1 := 'аб';
  s2 := 'ааа';
  AssertTrue(LevDistanceMbrUtf16(s1, s2, -1) = 2);
  AssertTrue(LevDistanceMbrUtf16(s2, s1, -1) = 2);

  s1 := 'а';
  s2 := 'ббб';
  AssertTrue(LevDistanceMbrUtf16(s1, s2, -1) = 3);
  AssertTrue(LevDistanceMbrUtf16(s2, s1, -1) = 3);

  s1 := 'аабабаб';
  s2 := 'аббаа';
  AssertTrue(LevDistanceMbrUtf16(s1, s2, -1) = 3);
  AssertTrue(LevDistanceMbrUtf16(s2, s1, -1) = 3);

  s1 := 'нелли';
  s2 := 'елли';
  AssertTrue(LevDistanceMbrUtf16(s1, s2, -1) = 1);
  AssertTrue(LevDistanceMbrUtf16(s2, s1, -1) = 1);

  s1 := 'нелли';
  s2 := 'еллия';
  AssertTrue(LevDistanceMbrUtf16(s1, s2, -1) = 2);
  AssertTrue(LevDistanceMbrUtf16(s2, s1, -1) = 2);

  s1 := 'мостик';
  s2 := 'костик';
  AssertTrue(LevDistanceMbrUtf16(s1, s2, -1) = 1);
  AssertTrue(LevDistanceMbrUtf16(s2, s1, -1) = 1);

  s1 := 'мостик';
  s2 := 'костяка';
  AssertTrue(LevDistanceMbrUtf16(s1, s2, -1) = 3);
  AssertTrue(LevDistanceMbrUtf16(s2, s1, -1) = 3);

  s1 := 'дистанция';
  s2 := 'дисперсия';
  AssertTrue(LevDistanceMbrUtf16(s1, s2, -1) = 4);
  AssertTrue(LevDistanceMbrUtf16(s2, s1, -1) = 4);

  s1 := 'левенштейн';
  s2 := 'франкенштейн';
  AssertTrue(LevDistanceMbrUtf16(s1, s2, -1) = 5);
  AssertTrue(LevDistanceMbrUtf16(s2, s1, -1) = 5);

  s1 := 'ааааааа';
  s2 := 'ааааааа';
  AssertTrue(LevDistanceMbrUtf16(s1, s2, -1) = 0);

  s1 := 'ааааааа';
  s2 := 'бббббббб';
  AssertTrue(LevDistanceMbrUtf16(s1, s2, -1) = 8);
  AssertTrue(LevDistanceMbrUtf16(s2, s1, -1) = 8);

  s1 := 'аааббаааа';
  s2 := 'ааааааа';
  AssertTrue(LevDistanceMbrUtf16(s1, s2, -1) = 2);
  AssertTrue(LevDistanceMbrUtf16(s2, s1, -1) = 2);

  s1 := 'а';
  s2 := 'б';
  AssertTrue(LevDistanceMbrUtf16(s1, s2, -1) = 1);
  AssertTrue(LevDistanceMbrUtf16(s2, s1, -1) = 1);

  s1 := '一天，在寒冷的冬天，我从森林里走出来。';
  s2 := '一，在寒冷的冬天，我从森林里走来。';
  AssertTrue(LevDistanceMbrUtf16(s1, s2, -1) = 2);
  AssertTrue(LevDistanceMbrUtf16(s2, s1, -1) = 2);
end;

procedure TTestUnicodeUtils.LevenshteinDistMyersUtf16;
var
  s1, s2: string;
begin
  s1 := '';
  s2 := 'привет';
  AssertTrue(LevDistanceMyersUtf16(s1, s1) = 0);
  AssertTrue(LevDistanceMyersUtf16(s2, s2) = 0);
  AssertTrue(LevDistanceMyersUtf16(s1, s2) = 6);
  AssertTrue(LevDistanceMyersUtf16(s2, s1) = 6);

  s1 := 'аб';
  s2 := 'аа';
  AssertTrue(LevDistanceMyersUtf16(s1, s2) = 1);
  AssertTrue(LevDistanceMyersUtf16(s2, s1) = 1);

  s1 := 'аб';
  s2 := 'ба';
  AssertTrue(LevDistanceMyersUtf16(s1, s2) = 2);
  AssertTrue(LevDistanceMyersUtf16(s2, s1) = 2);

  s1 := 'аб';
  s2 := 'ааа';
  AssertTrue(LevDistanceMyersUtf16(s1, s2) = 2);
  AssertTrue(LevDistanceMyersUtf16(s2, s1) = 2);

  s1 := 'а';
  s2 := 'ббб';
  AssertTrue(LevDistanceMyersUtf16(s1, s2) = 3);
  AssertTrue(LevDistanceMyersUtf16(s2, s1) = 3);

  s1 := 'аабабаб';
  s2 := 'аббаа';
  AssertTrue(LevDistanceMyersUtf16(s1, s2) = 3);
  AssertTrue(LevDistanceMyersUtf16(s2, s1) = 3);

  s1 := 'нелли';
  s2 := 'елли';
  AssertTrue(LevDistanceMyersUtf16(s1, s2) = 1);
  AssertTrue(LevDistanceMyersUtf16(s2, s1) = 1);

  s1 := 'нелли';
  s2 := 'еллия';
  AssertTrue(LevDistanceMyersUtf16(s1, s2) = 2);
  AssertTrue(LevDistanceMyersUtf16(s2, s1) = 2);

  s1 := 'мостик';
  s2 := 'костик';
  AssertTrue(LevDistanceMyersUtf16(s1, s2) = 1);
  AssertTrue(LevDistanceMyersUtf16(s2, s1) = 1);

  s1 := 'мостик';
  s2 := 'костяка';
  AssertTrue(LevDistanceMyersUtf16(s1, s2) = 3);
  AssertTrue(LevDistanceMyersUtf16(s2, s1) = 3);

  s1 := 'дистанция';
  s2 := 'дисперсия';
  AssertTrue(LevDistanceMyersUtf16(s1, s2) = 4);
  AssertTrue(LevDistanceMyersUtf16(s2, s1) = 4);

  s1 := 'левенштейн';
  s2 := 'франкенштейн';
  AssertTrue(LevDistanceMyersUtf16(s1, s2) = 5);
  AssertTrue(LevDistanceMyersUtf16(s2, s1) = 5);

  s1 := 'ааааааа';
  s2 := 'ааааааа';
  AssertTrue(LevDistanceMyersUtf16(s1, s2) = 0);

  s1 := 'ааааааа';
  s2 := 'бббббббб';
  AssertTrue(LevDistanceMyersUtf16(s1, s2) = 8);
  AssertTrue(LevDistanceMyersUtf16(s2, s1) = 8);

  s1 := 'аааббаааа';
  s2 := 'ааааааа';
  AssertTrue(LevDistanceMyersUtf16(s1, s2) = 2);
  AssertTrue(LevDistanceMyersUtf16(s2, s1) = 2);

  s1 := 'а';
  s2 := 'б';
  AssertTrue(LevDistanceMyersUtf16(s1, s2) = 1);
  AssertTrue(LevDistanceMyersUtf16(s2, s1) = 1);

  s1 := '一天，在寒冷的冬天，我从森林里走出来。';
  s2 := '一，在寒冷的冬天，我从森林里走来。';
  AssertTrue(LevDistanceMyersUtf16(s1, s2) = 2);
  AssertTrue(LevDistanceMyersUtf16(s2, s1) = 2);
end;

procedure TTestUnicodeUtils.LevenshteinDistMyersDQUtf16;
const
  s1:  string = 'Ну, здравствуйте, здравствуйте. Je vois que je vous fais peur, садитесь и рассказывайте';
  s2:  string = 'Dieu, quelle virulente sortie! – отвечал, нисколько не смутясь такою встречей';
  s3:  string = 'Вы не видали еще, – или: – вы не знакомы с ma tante? – говорила Анна Павловна';
  s4:  string = 'Il va se faire tuer. Dites-moi, pourquoi cette vilaine guerre, – сказала она князю Василию';
  s5:  string = 'Да, я слышал про его план вечного мира, и это очень интересно, но едва ли возможно…';
  s6:  string = 'шепнула Анна Павловна одному. – Le vicomte est un parfait conteur, – проговорила она другому';
  s7:  string = 'Княжна облокотила свою открытую полную руку на столик и не нашла нужным что-либо сказать';
  s8:  string = 'Le charmant Hippolyte поражал своим необыкновенным сходством с сестрою-красавицею';
  s9:  string = 'Ce n’est pas une histoire de revenants? – сказал он, усевшись подле княгини и торопливо пристроив к глазам свой лорнет';
  s10: string = 'сказал князь Ипполит таким тоном, что видно было, – он сказал эти слова, а потом уже понял, что они значили';
  s11: string = 'Средство – европейское равновесие и droit des gens, – говорил аббат ';
  s12: string = 'Образуйте мне этого медведя, – сказал он. – Вот он месяц живет у меня, и в первый раз я его вижу в свете';
  s13: string = 'Пожилая дама носила имя княгини Друбецкой, одной из лучших фамилий России, но она была бедна';
  s14: string = 'Но влияние в свете есть капитал, который надо беречь, чтоб он не исчез. Князь Василий знал это';
begin
  AssertTrue(LevDistanceMyersUtf16(s1, s2) = EditDistanceUtf8(s1, s2, TSeqEditCost.Default));
  AssertTrue(LevDistanceMyersUtf16(s3, s4) = EditDistanceUtf8(s3, s4, TSeqEditCost.Default));
  AssertTrue(LevDistanceMyersUtf16(s5, s6) = EditDistanceUtf8(s5, s6, TSeqEditCost.Default));
  AssertTrue(LevDistanceMyersUtf16(s7, s8) = EditDistanceUtf8(s7, s8, TSeqEditCost.Default));
  AssertTrue(LevDistanceMyersUtf16(s9, s10) = EditDistanceUtf8(s9, s10, TSeqEditCost.Default));
  AssertTrue(LevDistanceMyersUtf16(s11, s12) = EditDistanceUtf8(s11, s12, TSeqEditCost.Default));
  AssertTrue(LevDistanceMyersUtf16(s13, s14) = EditDistanceUtf8(s13, s14, TSeqEditCost.Default));
end;

procedure TTestUnicodeUtils.LevenshteinDistMyersLongUtf16;
const
  s1:  string = 'Этого не обещаю. Вы знаете, как осаждают Кутузова с тех пор, как он назначен главнокомандующим. Он мне сам говорил, что все московские барыни сговорились отдать ему всех своих детей в адъютанты';
  s2:  string = 'Нет, обещайте, обещайте, Basile, – сказала вслед ему Анна Михайловна, с улыбкой молодой кокетки, которая когда-то, должно быть, была ей свойственна, а теперь так не шла к ее истощенному лицу.';
  s3:  string = 'Les souverains? Je ne parle pas de la Russie, – сказал виконт учтиво и безнадежно. – Les souverains, madame? Qu’ont ils fait pour Louis XVI, pour la reine, pour madame Elisabeth? Rien, – продолжал он, одушевляясь.';
  s4:  string = 'И он, презрительно вздохнув, опять переменил положение. Князь Ипполит, долго смотревший в лорнет на виконта, вдруг при этих словах повернулся всем телом к маленькой княгине и, попросив у нее иголку, стал показывать ей, рисуя иголкой на столе, герб Конде.';
  s5:  string = 'Ежели еще год Бонапарте останется на престоле Франции, – продолжал виконт начатый разговор, с видом человека, не слушающего других, но в деле, лучше всех ему известном, следящего только за ходом своих мыслей, – то дела пойдут слишком далеко.';
  s6:  string = 'Не успели еще Анна Павловна и другие улыбкой оценить этих слов виконта, как Пьер опять ворвался в разговор, и Анна Павловна, хотя и предчувствовавшая, что он скажет что-нибудь неприличное, уже не могла остановить его.';
  s7:  string = '– Свобода и равенство, – презрительно сказал виконт, как будто решившийся, наконец, серьезно доказать этому юноше всю глупость его речей, – всё громкие слова, которые уже давно компрометировались.';
  s8:  string = '– Mais, mon cher monsieur Pierre, – сказала Анна Павловна, – как же вы объясняете великого человека, который мог казнить герцога, наконец просто человека, без суда и без вины?';
  s9:  string = '– Как вы хотите, чтоб он всем отвечал вдруг? – сказал князь Андрей. – Притом надо в поступках государственного человека различать поступки частного лица, полководца или императора.';
  s10: string = 'Тут князь Ипполит фыркнул и захохотал гораздо прежде своих слушателей, что произвело невыгодное для рассказчика впечатление. Однако многие, и в том числе пожилая дама и Анна Павловна, улыбнулись.';
  s11: string = 'Пьер был неуклюж. Толстый, выше обыкновенного роста, широкий, с огромными красными руками, он, как говорится, не умел войти в салон и еще менее умел из него выйти, то есть перед выходом сказать что-нибудь особенно приятное.';
  s12: string = 'Два лакея, один княгинин, другой его, дожидаясь, когда они кончат говорить, стояли с шалью и рединготом и слушали их, непонятный им, французский говор с такими лицами, как будто они понимали, что говорится, но не хотели показывать этого.';
  s13: string = 'Князь Ипполит торопливо надел свой редингот, который у него, по-новому, был длиннее пяток, и, путаясь в нем, побежал на крыльцо за княгиней, которую лакей подсаживал в карету.';
  s14: string = '– Ты не понимаешь, отчего я это говорю, – продолжал он. – Ведь это целая история жизни. Ты говоришь, Бонапарте и его карьера, – сказал он, хотя Пьер и не говорил про Бонапарте.';
begin
  AssertTrue(LevDistanceMyersUtf16(s1, s2) = EditDistanceUtf8(s1, s2, TSeqEditCost.Default));
  AssertTrue(LevDistanceMyersUtf16(s3, s4) = EditDistanceUtf8(s3, s4, TSeqEditCost.Default));
  AssertTrue(LevDistanceMyersUtf16(s5, s6) = EditDistanceUtf8(s5, s6, TSeqEditCost.Default));
  AssertTrue(LevDistanceMyersUtf16(s7, s8) = EditDistanceUtf8(s7, s8, TSeqEditCost.Default));
  AssertTrue(LevDistanceMyersUtf16(s9, s10) = EditDistanceUtf8(s9, s10, TSeqEditCost.Default));
  AssertTrue(LevDistanceMyersUtf16(s11, s12) = EditDistanceUtf8(s11, s12, TSeqEditCost.Default));
  AssertTrue(LevDistanceMyersUtf16(s13, s14) = EditDistanceUtf8(s13, s14, TSeqEditCost.Default));
end;

procedure TTestUnicodeUtils.LevenshteinDistMyersBoundedUtf16;
var
  s1, s2: string;
begin
  s1 := '';
  s2 := 'привет';
  AssertTrue(LevDistanceMyersUtf16(s1, s2) = 6);
  AssertTrue(LevDistanceMyersUtf16(s2, s1, 5) = -1);

  s1 := 'аб';
  s2 := 'аа';
  AssertTrue(LevDistanceMyersUtf16(s1, s2) = 1);
  AssertTrue(LevDistanceMyersUtf16(s1, s2, 0) = -1);

  s1 := 'аб';
  s2 := 'ба';
  AssertTrue(LevDistanceMyersUtf16(s1, s2) = 2);
  AssertTrue(LevDistanceMyersUtf16(s1, s2, 1) = -1);

  s1 := 'аб';
  s2 := 'ааа';
  AssertTrue(LevDistanceMyersUtf16(s1, s2) = 2);
  AssertTrue(LevDistanceMyersUtf16(s1, s2, 1) = -1);

  s1 := 'а';
  s2 := 'ббб';
  AssertTrue(LevDistanceMyersUtf16(s1, s2) = 3);
  AssertTrue(LevDistanceMyersUtf16(s1, s2, 2) = -1);

  s1 := 'аабабаб';
  s2 := 'аббаа';
  AssertTrue(LevDistanceMyersUtf16(s1, s2) = 3);
  AssertTrue(LevDistanceMyersUtf16(s1, s2, 2) = -1);

  s1 := 'нелли';
  s2 := 'елли';
  AssertTrue(LevDistanceMyersUtf16(s1, s2) = 1);
  AssertTrue(LevDistanceMyersUtf16(s1, s2, 0) = -1);

  s1 := 'нелли';
  s2 := 'еллия';
  AssertTrue(LevDistanceMyersUtf16(s1, s2) = 2);
  AssertTrue(LevDistanceMyersUtf16(s1, s2, 1) = -1);

  s1 := 'мостик';
  s2 := 'костик';
  AssertTrue(LevDistanceMyersUtf16(s1, s2) = 1);
  AssertTrue(LevDistanceMyersUtf16(s1, s2, 0) = -1);

  s1 := 'мостик';
  s2 := 'костяка';
  AssertTrue(LevDistanceMyersUtf16(s1, s2) = 3);
  AssertTrue(LevDistanceMyersUtf16(s1, s2, 2) = -1);

  s1 := 'дистанция';
  s2 := 'дисперсия';
  AssertTrue(LevDistanceMyersUtf16(s1, s2) = 4);
  AssertTrue(LevDistanceMyersUtf16(s1, s2, 3) = -1);

  s1 := 'левенштейн';
  s2 := 'франкенштейн';
  AssertTrue(LevDistanceMyersUtf16(s1, s2) = 5);
  AssertTrue(LevDistanceMyersUtf16(s1, s2, 4) = -1);

  s1 := 'ааааааа';
  s2 := 'бббббббб';
  AssertTrue(LevDistanceMyersUtf16(s1, s2) = 8);
  AssertTrue(LevDistanceMyersUtf16(s1, s2, 7) = -1);

  s1 := 'аааббаааа';
  s2 := 'ааааааа';
  AssertTrue(LevDistanceMyersUtf16(s1, s2) = 2);
  AssertTrue(LevDistanceMyersUtf16(s1, s2, 1) = -1);

  s1 := 'а';
  s2 := 'б';
  AssertTrue(LevDistanceMyersUtf16(s1, s2) = 1);
  AssertTrue(LevDistanceMyersUtf16(s1, s2, 0) = -1);

  s1 := '一天，在寒冷的冬天，我从森林里走出来。';
  s2 := '一，在寒冷的冬天，我从森林里走来。';
  AssertTrue(LevDistanceMyersUtf16(s1, s2) = 2);
  AssertTrue(LevDistanceMyersUtf16(s2, s1, 1) = -1);
end;

procedure TTestUnicodeUtils.LevenshteinDistMyersDQBoundedUtf16;
const
  s1:  string = 'Ну, здравствуйте, здравствуйте. Je vois que je vous fais peur, садитесь и рассказывайте';
  s2:  string = 'Dieu, quelle virulente sortie![4] – отвечал, нисколько не смутясь такою встречей';
  s3:  string = 'Вы не видали еще, – или: – вы не знакомы с ma tante? – говорила Анна Павловна';
  s4:  string = 'Il va se faire tuer. Dites-moi, pourquoi cette vilaine guerre, – сказала она князю Василию';
  s5:  string = 'Да, я слышал про его план вечного мира, и это очень интересно, но едва ли возможно…';
  s6:  string = 'шепнула Анна Павловна одному. – Le vicomte est un parfait conteur, – проговорила она другому';
  s7:  string = 'Княжна облокотила свою открытую полную руку на столик и не нашла нужным что-либо сказать';
  s8:  string = 'Le charmant Hippolyte поражал своим необыкновенным сходством с сестрою-красавицею';
  s9:  string = 'Ce n’est pas une histoire de revenants? – сказал он, усевшись подле княгини и торопливо пристроив к глазам свой лорнет';
  s10: string = 'сказал князь Ипполит таким тоном, что видно было, – он сказал эти слова, а потом уже понял, что они значили';
  s11: string = 'Средство – европейское равновесие и droit des gens, – говорил аббат ';
  s12: string = 'Образуйте мне этого медведя, – сказал он. – Вот он месяц живет у меня, и в первый раз я его вижу в свете';
  s13: string = 'Пожилая дама носила имя княгини Друбецкой, одной из лучших фамилий России, но она была бедна';
  s14: string = 'Но влияние в свете есть капитал, который надо беречь, чтоб он не исчез. Князь Василий знал это';
begin
  AssertTrue(LevDistanceMyersUtf16(s1, s2, Pred(EditDistanceUtf8(s1, s2, TSeqEditCost.Default))) = -1);
  AssertTrue(LevDistanceMyersUtf16(s3, s4, Pred(EditDistanceUtf8(s3, s4, TSeqEditCost.Default))) = -1);
  AssertTrue(LevDistanceMyersUtf16(s5, s6, Pred(EditDistanceUtf8(s5, s6, TSeqEditCost.Default))) = -1);
  AssertTrue(LevDistanceMyersUtf16(s7, s8, Pred(EditDistanceUtf8(s7, s8, TSeqEditCost.Default))) = -1);
  AssertTrue(LevDistanceMyersUtf16(s9, s10, Pred(EditDistanceUtf8(s9, s10, TSeqEditCost.Default))) = -1);
  AssertTrue(LevDistanceMyersUtf16(s11, s12, Pred(EditDistanceUtf8(s11, s12, TSeqEditCost.Default))) = -1);
  AssertTrue(LevDistanceMyersUtf16(s13, s14, Pred(EditDistanceUtf8(s13, s14, TSeqEditCost.Default))) = -1);
end;

procedure TTestUnicodeUtils.LevenshteinDistMyersLongBoundedUtf16;
const
  s1:  string = 'Этого не обещаю. Вы знаете, как осаждают Кутузова с тех пор, как он назначен главнокомандующим. Он мне сам говорил, что все московские барыни сговорились отдать ему всех своих детей в адъютанты';
  s2:  string = 'Нет, обещайте, обещайте, Basile, – сказала вслед ему Анна Михайловна, с улыбкой молодой кокетки, которая когда-то, должно быть, была ей свойственна, а теперь так не шла к ее истощенному лицу.';
  s3:  string = 'Les souverains? Je ne parle pas de la Russie, – сказал виконт учтиво и безнадежно. – Les souverains, madame? Qu’ont ils fait pour Louis XVI, pour la reine, pour madame Elisabeth? Rien, – продолжал он, одушевляясь.';
  s4:  string = 'И он, презрительно вздохнув, опять переменил положение. Князь Ипполит, долго смотревший в лорнет на виконта, вдруг при этих словах повернулся всем телом к маленькой княгине и, попросив у нее иголку, стал показывать ей, рисуя иголкой на столе, герб Конде.';
  s5:  string = 'Ежели еще год Бонапарте останется на престоле Франции, – продолжал виконт начатый разговор, с видом человека, не слушающего других, но в деле, лучше всех ему известном, следящего только за ходом своих мыслей, – то дела пойдут слишком далеко.';
  s6:  string = 'Не успели еще Анна Павловна и другие улыбкой оценить этих слов виконта, как Пьер опять ворвался в разговор, и Анна Павловна, хотя и предчувствовавшая, что он скажет что-нибудь неприличное, уже не могла остановить его.';
  s7:  string = '– Свобода и равенство, – презрительно сказал виконт, как будто решившийся, наконец, серьезно доказать этому юноше всю глупость его речей, – всё громкие слова, которые уже давно компрометировались.';
  s8:  string = '– Mais, mon cher monsieur Pierre, – сказала Анна Павловна, – как же вы объясняете великого человека, который мог казнить герцога, наконец просто человека, без суда и без вины?';
  s9:  string = '– Как вы хотите, чтоб он всем отвечал вдруг? – сказал князь Андрей. – Притом надо в поступках государственного человека различать поступки частного лица, полководца или императора.';
  s10: string = 'Тут князь Ипполит фыркнул и захохотал гораздо прежде своих слушателей, что произвело невыгодное для рассказчика впечатление. Однако многие, и в том числе пожилая дама и Анна Павловна, улыбнулись.';
  s11: string = 'Пьер был неуклюж. Толстый, выше обыкновенного роста, широкий, с огромными красными руками, он, как говорится, не умел войти в салон и еще менее умел из него выйти, то есть перед выходом сказать что-нибудь особенно приятное.';
  s12: string = 'Два лакея, один княгинин, другой его, дожидаясь, когда они кончат говорить, стояли с шалью и рединготом и слушали их, непонятный им, французский говор с такими лицами, как будто они понимали, что говорится, но не хотели показывать этого.';
  s13: string = 'Князь Ипполит торопливо надел свой редингот, который у него, по-новому, был длиннее пяток, и, путаясь в нем, побежал на крыльцо за княгиней, которую лакей подсаживал в карету.';
  s14: string = '– Ты не понимаешь, отчего я это говорю, – продолжал он. – Ведь это целая история жизни. Ты говоришь, Бонапарте и его карьера, – сказал он, хотя Пьер и не говорил про Бонапарте.';
begin
  AssertTrue(LevDistanceMyersUtf16(s1, s2, Pred(EditDistanceUtf16(s1, s2, TSeqEditCost.Default))) = -1);
  AssertTrue(LevDistanceMyersUtf16(s3, s4, Pred(EditDistanceUtf16(s3, s4, TSeqEditCost.Default))) = -1);
  AssertTrue(LevDistanceMyersUtf16(s5, s6, Pred(EditDistanceUtf16(s5, s6, TSeqEditCost.Default))) = -1);
  AssertTrue(LevDistanceMyersUtf16(s7, s8, Pred(EditDistanceUtf16(s7, s8, TSeqEditCost.Default))) = -1);
  AssertTrue(LevDistanceMyersUtf16(s9, s10, Pred(EditDistanceUtf16(s9, s10, TSeqEditCost.Default))) = -1);
  AssertTrue(LevDistanceMyersUtf16(s11, s12, Pred(EditDistanceUtf16(s11, s12, TSeqEditCost.Default))) = -1);
  AssertTrue(LevDistanceMyersUtf16(s13, s14, Pred(EditDistanceUtf16(s13, s14, TSeqEditCost.Default))) = -1);
end;

procedure TTestUnicodeUtils.LevenshteinDistMyersLongDynUtf16;
const
  s1:  string = 'Этого не обещаю. Вы знаете, как осаждают Кутузова с тех пор, как он назначен главнокомандующим. Он мне сам говорил, что все московские барыни сговорились отдать ему всех своих детей в адъютанты';
  s2:  string = 'Нет, обещайте, обещайте, Basile, – сказала вслед ему Анна Михайловна, с улыбкой молодой кокетки, которая когда-то, должно быть, была ей свойственна, а теперь так не шла к ее истощенному лицу.';
  s3:  string = 'Les souverains? Je ne parle pas de la Russie, – сказал виконт учтиво и безнадежно. – Les souverains, madame? Qu’ont ils fait pour Louis XVI, pour la reine, pour madame Elisabeth? Rien, – продолжал он, одушевляясь.';
  s4:  string = 'И он, презрительно вздохнув, опять переменил положение. Князь Ипполит, долго смотревший в лорнет на виконта, вдруг при этих словах повернулся всем телом к маленькой княгине и, попросив у нее иголку, стал показывать ей, рисуя иголкой на столе, герб Конде.';
  s5:  string = 'Ежели еще год Бонапарте останется на престоле Франции, – продолжал виконт начатый разговор, с видом человека, не слушающего других, но в деле, лучше всех ему известном, следящего только за ходом своих мыслей, – то дела пойдут слишком далеко.';
  s6:  string = 'Не успели еще Анна Павловна и другие улыбкой оценить этих слов виконта, как Пьер опять ворвался в разговор, и Анна Павловна, хотя и предчувствовавшая, что он скажет что-нибудь неприличное, уже не могла остановить его.';
  s7:  string = '– Свобода и равенство, – презрительно сказал виконт, как будто решившийся, наконец, серьезно доказать этому юноше всю глупость его речей, – всё громкие слова, которые уже давно компрометировались.';
  s8:  string = '– Mais, mon cher monsieur Pierre, – сказала Анна Павловна, – как же вы объясняете великого человека, который мог казнить герцога, наконец просто человека, без суда и без вины?';
  s9:  string = '– Как вы хотите, чтоб он всем отвечал вдруг? – сказал князь Андрей. – Притом надо в поступках государственного человека различать поступки частного лица, полководца или императора.';
  s10: string = 'Тут князь Ипполит фыркнул и захохотал гораздо прежде своих слушателей, что произвело невыгодное для рассказчика впечатление. Однако многие, и в том числе пожилая дама и Анна Павловна, улыбнулись.';
  s11: string = 'Пьер был неуклюж. Толстый, выше обыкновенного роста, широкий, с огромными красными руками, он, как говорится, не умел войти в салон и еще менее умел из него выйти, то есть перед выходом сказать что-нибудь особенно приятное.';
  s12: string = 'Два лакея, один княгинин, другой его, дожидаясь, когда они кончат говорить, стояли с шалью и рединготом и слушали их, непонятный им, французский говор с такими лицами, как будто они понимали, что говорится, но не хотели показывать этого.';
  s13: string = 'Князь Ипполит торопливо надел свой редингот, который у него, по-новому, был длиннее пяток, и, путаясь в нем, побежал на крыльцо за княгиней, которую лакей подсаживал в карету.';
  s14: string = '– Ты не понимаешь, отчего я это говорю, – продолжал он. – Ведь это целая история жизни. Ты говоришь, Бонапарте и его карьера, – сказал он, хотя Пьер и не говорил про Бонапарте.';
begin
  AssertTrue(LevDistanceMyersUtf16(s1, s2, -1) = EditDistanceUtf16(s1, s2, TSeqEditCost.Default));
  AssertTrue(LevDistanceMyersUtf16(s3, s4, -1) = EditDistanceUtf16(s3, s4, TSeqEditCost.Default));
  AssertTrue(LevDistanceMyersUtf16(s5, s6, -1) = EditDistanceUtf16(s5, s6, TSeqEditCost.Default));
  AssertTrue(LevDistanceMyersUtf16(s7, s8, -1) = EditDistanceUtf16(s7, s8, TSeqEditCost.Default));
  AssertTrue(LevDistanceMyersUtf16(s9, s10, -1) = EditDistanceUtf16(s9, s10, TSeqEditCost.Default));
  AssertTrue(LevDistanceMyersUtf16(s11, s12, -1) = EditDistanceUtf16(s11, s12, TSeqEditCost.Default));
  AssertTrue(LevDistanceMyersUtf16(s13, s14, -1) = EditDistanceUtf16(s13, s14, TSeqEditCost.Default));
end;

procedure TTestUnicodeUtils.LcsDistWmUtf16;
var
  s1, s2: string;
begin
  s1 := '';
  s2 := 'привет';
  AssertTrue(LcsDistanceWmUtf16(s1, s1) = 0);
  AssertTrue(LcsDistanceWmUtf16(s2, s2) = 0);
  AssertTrue(LcsDistanceWmUtf16(s1, s2) = 6);
  AssertTrue(LcsDistanceWmUtf16(s2, s1) = 6);

  s1 := 'аб';
  s2 := 'аа';
  AssertTrue(LcsDistanceWmUtf16(s1, s2) = 2);
  AssertTrue(LcsDistanceWmUtf16(s2, s1) = 2);

  s1 := 'аб';
  s2 := 'ба';
  AssertTrue(LcsDistanceWmUtf16(s1, s2) = 2);
  AssertTrue(LcsDistanceWmUtf16(s2, s1) = 2);

  s1 := 'аб';
  s2 := 'ааа';
  AssertTrue(LcsDistanceWmUtf16(s1, s2) = 3);
  AssertTrue(LcsDistanceWmUtf16(s2, s1) = 3);

  s1 := 'а';
  s2 := 'ббб';
  AssertTrue(LcsDistanceWmUtf16(s1, s2) = 4);
  AssertTrue(LcsDistanceWmUtf16(s2, s1) = 4);

  s1 := 'аабабаб';
  s2 := 'аббаа';
  AssertTrue(LcsDistanceWmUtf16(s1, s2) = 4);
  AssertTrue(LcsDistanceWmUtf16(s2, s1) = 4);

  s1 := 'нелли';
  s2 := 'елли';
  AssertTrue(LcsDistanceWmUtf16(s1, s2) = 1);
  AssertTrue(LcsDistanceWmUtf16(s2, s1) = 1);

  s1 := 'нелли';
  s2 := 'еллия';
  AssertTrue(LcsDistanceWmUtf16(s1, s2) = 2);
  AssertTrue(LcsDistanceWmUtf16(s2, s1) = 2);

  s1 := 'мостик';
  s2 := 'костик';
  AssertTrue(LcsDistanceWmUtf16(s1, s2) = 2);
  AssertTrue(LcsDistanceWmUtf16(s2, s1) = 2);

  s1 := 'мостик';
  s2 := 'костяка';
  AssertTrue(LcsDistanceWmUtf16(s1, s2) = 5);
  AssertTrue(LcsDistanceWmUtf16(s2, s1) = 5);

  s1 := 'дистанция';
  s2 := 'дисперсия';
  AssertTrue(LcsDistanceWmUtf16(s1, s2) = 8);
  AssertTrue(LcsDistanceWmUtf16(s2, s1) = 8);

  s1 := 'левенштейн';
  s2 := 'франкенштейн';
  AssertTrue(LcsDistanceWmUtf16(s1, s2) = 8);
  AssertTrue(LcsDistanceWmUtf16(s2, s1) = 8);

  s1 := 'ааааааа';
  s2 := 'ааааааа';
  AssertTrue(LcsDistanceWmUtf16(s1, s2) = 0);

  s1 := 'ааааааа';
  s2 := 'бббббббб';
  AssertTrue(LcsDistanceWmUtf16(s1, s2) = 15);
  AssertTrue(LcsDistanceWmUtf16(s2, s1) = 15);

  s1 := 'аааббаааа';
  s2 := 'ааааааа';
  AssertTrue(LcsDistanceWmUtf16(s1, s2) = 2);
  AssertTrue(LcsDistanceWmUtf16(s2, s1) = 2);

  s1 := 'а';
  s2 := 'б';
  AssertTrue(LcsDistanceWmUtf16(s1, s2) = 2);
  AssertTrue(LcsDistanceWmUtf16(s2, s1) = 2);

  s1 := '一天，在寒冷的冬天，我从森林里走出来。';
  s2 := '一，在寒冷的冬天，我从森林里走来。';
  AssertTrue(LcsDistanceWmUtf16(s1, s2) = 2);
  AssertTrue(LcsDistanceWmUtf16(s2, s1) = 2);
end;

procedure TTestUnicodeUtils.LcsDistWmBoundedUtf16;
var
  s1, s2: string;
begin
  s1 := '';
  s2 := 'привет';
  AssertTrue(LcsDistanceWmUtf16(s1, s2) = 6);
  AssertTrue(LcsDistanceWmUtf16(s1, s2, 6) = 6);
  AssertTrue(LcsDistanceWmUtf16(s1, s2, 5) = -1);

  s1 := 'аб';
  s2 := 'аа';
  AssertTrue(LcsDistanceWmUtf16(s1, s2) = 2);
  AssertTrue(LcsDistanceWmUtf16(s1, s2, 2) = 2);
  AssertTrue(LcsDistanceWmUtf16(s1, s2, 1) = -1);

  s1 := 'аб';
  s2 := 'ба';
  AssertTrue(LcsDistanceWmUtf16(s1, s2) = 2);
  AssertTrue(LcsDistanceWmUtf16(s1, s2, 1) = -1);

  s1 := 'аб';
  s2 := 'ааа';
  AssertTrue(LcsDistanceWmUtf16(s1, s2) = 3);
  AssertTrue(LcsDistanceWmUtf16(s1, s2, 3) = 3);
  AssertTrue(LcsDistanceWmUtf16(s1, s2, 2) = -1);

  s1 := 'а';
  s2 := 'ббб';
  AssertTrue(LcsDistanceWmUtf16(s1, s2) = 4);
  AssertTrue(LcsDistanceWmUtf16(s1, s2, 3) = -1);

  s1 := 'аабабаб';
  s2 := 'аббаа';
  AssertTrue(LcsDistanceWmUtf16(s1, s2) = 4);
  AssertTrue(LcsDistanceWmUtf16(s1, s2, 4) = 4);
  AssertTrue(LcsDistanceWmUtf16(s1, s2, 3) = -1);

  s1 := 'нелли';
  s2 := 'елли';
  AssertTrue(LcsDistanceWmUtf16(s1, s2) = 1);
  AssertTrue(LcsDistanceWmUtf16(s1, s2, 1) = 1);
  AssertTrue(LcsDistanceWmUtf16(s1, s2, 0) = -1);

  s1 := 'нелли';
  s2 := 'еллия';
  AssertTrue(LcsDistanceWmUtf16(s1, s2) = 2);
  AssertTrue(LcsDistanceWmUtf16(s1, s2, 2) = 2);
  AssertTrue(LcsDistanceWmUtf16(s1, s2, 1) = -1);

  s1 := 'мостик';
  s2 := 'костик';
  AssertTrue(LcsDistanceWmUtf16(s1, s2) = 2);
  AssertTrue(LcsDistanceWmUtf16(s1, s2, 2) = 2);
  AssertTrue(LcsDistanceWmUtf16(s1, s2, 1) = -1);

  s1 := 'мостик';
  s2 := 'костяка';
  AssertTrue(LcsDistanceWmUtf16(s1, s2) = 5);
  AssertTrue(LcsDistanceWmUtf16(s1, s2, 5) = 5);
  AssertTrue(LcsDistanceWmUtf16(s1, s2, 4) = -1);

  s1 := 'дистанция';
  s2 := 'дисперсия';
  AssertTrue(LcsDistanceWmUtf16(s1, s2) = 8);
  AssertTrue(LcsDistanceWmUtf16(s1, s2, 8) = 8);
  AssertTrue(LcsDistanceWmUtf16(s1, s2, 7) = -1);

  s1 := 'левенштейн';
  s2 := 'франкенштейн';
  AssertTrue(LcsDistanceWmUtf16(s1, s2) = 8);
  AssertTrue(LcsDistanceWmUtf16(s1, s2, 8) = 8);
  AssertTrue(LcsDistanceWmUtf16(s1, s2, 7) = -1);

  s1 := 'ааааааа';
  s2 := 'бббббббб';
  AssertTrue(LcsDistanceWmUtf16(s1, s2) = 15);
  AssertTrue(LcsDistanceWmUtf16(s1, s2, 15) = 15);
  AssertTrue(LcsDistanceWmUtf16(s1, s2, 14) = -1);

  s1 := 'аааббаааа';
  s2 := 'ааааааа';
  AssertTrue(LcsDistanceWmUtf16(s1, s2) = 2);
  AssertTrue(LcsDistanceWmUtf16(s1, s2, 2) = 2);
  AssertTrue(LcsDistanceWmUtf16(s1, s2, 1) = -1);

  s1 := 'а';
  s2 := 'б';
  AssertTrue(LcsDistanceWmUtf16(s1, s2) = 2);
  AssertTrue(LcsDistanceWmUtf16(s1, s2, 2) = 2);
  AssertTrue(LcsDistanceWmUtf16(s1, s2, 1) = -1);

  s1 := '一天，在寒冷的冬天，我从森林里走出来。';
  s2 := '一，在寒冷的冬天，我从森林里走来。';
  AssertTrue(LcsDistanceWmUtf16(s1, s2) = 2);
  AssertTrue(LcsDistanceWmUtf16(s1, s2, 2) = 2);
  AssertTrue(LcsDistanceWmUtf16(s2, s1, 1) = -1);
end;

procedure TTestUnicodeUtils.LcsDistWmDynUtf16;
var
  s1, s2: string;
begin
  s1 := '';
  s2 := 'привет';
  AssertTrue(LcsDistanceWmUtf16(s1, s1, -1) = 0);
  AssertTrue(LcsDistanceWmUtf16(s2, s2, -1) = 0);
  AssertTrue(LcsDistanceWmUtf16(s1, s2, -1) = 6);
  AssertTrue(LcsDistanceWmUtf16(s2, s1, -1) = 6);

  s1 := 'аб';
  s2 := 'аа';
  AssertTrue(LcsDistanceWmUtf16(s1, s2, -1) = 2);
  AssertTrue(LcsDistanceWmUtf16(s2, s1, -1) = 2);

  s1 := 'аб';
  s2 := 'ба';
  AssertTrue(LcsDistanceWmUtf16(s1, s2, -1) = 2);
  AssertTrue(LcsDistanceWmUtf16(s2, s1, -1) = 2);

  s1 := 'аб';
  s2 := 'ааа';
  AssertTrue(LcsDistanceWmUtf16(s1, s2, -1) = 3);
  AssertTrue(LcsDistanceWmUtf16(s2, s1, -1) = 3);

  s1 := 'а';
  s2 := 'ббб';
  AssertTrue(LcsDistanceWmUtf16(s1, s2, -1) = 4);
  AssertTrue(LcsDistanceWmUtf16(s2, s1, -1) = 4);

  s1 := 'аабабаб';
  s2 := 'аббаа';
  AssertTrue(LcsDistanceWmUtf16(s1, s2, -1) = 4);
  AssertTrue(LcsDistanceWmUtf16(s2, s1, -1) = 4);

  s1 := 'нелли';
  s2 := 'елли';
  AssertTrue(LcsDistanceWmUtf16(s1, s2, -1) = 1);
  AssertTrue(LcsDistanceWmUtf16(s2, s1, -1) = 1);

  s1 := 'нелли';
  s2 := 'еллия';
  AssertTrue(LcsDistanceWmUtf16(s1, s2, -1) = 2);
  AssertTrue(LcsDistanceWmUtf16(s2, s1, -1) = 2);

  s1 := 'мостик';
  s2 := 'костик';
  AssertTrue(LcsDistanceWmUtf16(s1, s2, -1) = 2);
  AssertTrue(LcsDistanceWmUtf16(s2, s1, -1) = 2);

  s1 := 'мостик';
  s2 := 'костяка';
  AssertTrue(LcsDistanceWmUtf16(s1, s2, -1) = 5);
  AssertTrue(LcsDistanceWmUtf16(s2, s1, -1) = 5);

  s1 := 'дистанция';
  s2 := 'дисперсия';
  AssertTrue(LcsDistanceWmUtf16(s1, s2, -1) = 8);
  AssertTrue(LcsDistanceWmUtf16(s2, s1, -1) = 8);

  s1 := 'левенштейн';
  s2 := 'франкенштейн';
  AssertTrue(LcsDistanceWmUtf16(s1, s2, -1) = 8);
  AssertTrue(LcsDistanceWmUtf16(s2, s1, -1) = 8);

  s1 := 'ааааааа';
  s2 := 'ааааааа';
  AssertTrue(LcsDistanceWmUtf16(s1, s2, -1) = 0);

  s1 := 'ааааааа';
  s2 := 'бббббббб';
  AssertTrue(LcsDistanceWmUtf16(s1, s2, -1) = 15);
  AssertTrue(LcsDistanceWmUtf16(s2, s1, -1) = 15);

  s1 := 'аааббаааа';
  s2 := 'ааааааа';
  AssertTrue(LcsDistanceWmUtf16(s1, s2, -1) = 2);
  AssertTrue(LcsDistanceWmUtf16(s2, s1, -1) = 2);

  s1 := 'а';
  s2 := 'б';
  AssertTrue(LcsDistanceWmUtf16(s1, s2, -1) = 2);
  AssertTrue(LcsDistanceWmUtf16(s2, s1, -1) = 2);

  s1 := '一天，在寒冷的冬天，我从森林里走出来。';
  s2 := '一，在寒冷的冬天，我从森林里走来。';
  AssertTrue(LcsDistanceWmUtf16(s1, s2, -1) = 2);
  AssertTrue(LcsDistanceWmUtf16(s2, s1, -1) = 2);
end;

procedure TTestUnicodeUtils.DumDistMbrUtf16;
var
  s1, s2: string;
begin
  s1 := '';
  s2 := 'привет';
  AssertTrue(DumDistanceMbrUtf16(s1, s1) = 0);
  AssertTrue(DumDistanceMbrUtf16(s2, s2) = 0);
  AssertTrue(DumDistanceMbrUtf16(s1, s2) = 6);
  AssertTrue(DumDistanceMbrUtf16(s2, s1) = 6);

  s1 := 'аб';
  s2 := 'аа';
  AssertTrue(DumDistanceMbrUtf16(s1, s2) = 1);
  AssertTrue(DumDistanceMbrUtf16(s2, s1) = 1);

  s1 := 'аб';
  s2 := 'ба';
  AssertTrue(DumDistanceMbrUtf16(s1, s2) = 1);
  AssertTrue(DumDistanceMbrUtf16(s2, s1) = 1);

  s1 := 'аc';
  s2 := 'cba';
  AssertTrue(DumDistanceMbrUtf16(s1, s2) = 3);
  AssertTrue(DumDistanceMbrUtf16(s2, s1) = 3);

  s1 := 'а';
  s2 := 'ббб';
  AssertTrue(DumDistanceMbrUtf16(s1, s2) = 3);
  AssertTrue(DumDistanceMbrUtf16(s2, s1) = 3);

  s1 := 'аабабаб';
  s2 := 'аббаа';
  AssertTrue(DumDistanceMbrUtf16(s1, s2) = 3);
  AssertTrue(DumDistanceMbrUtf16(s2, s1) = 3);

  s1 := 'нелли';
  s2 := 'елли';
  AssertTrue(DumDistanceMbrUtf16(s1, s2) = 1);
  AssertTrue(DumDistanceMbrUtf16(s2, s1) = 1);

  s1 := 'нелли';
  s2 := 'елил';
  AssertTrue(DumDistanceMbrUtf16(s1, s2) = 2);
  AssertTrue(DumDistanceMbrUtf16(s2, s1) = 2);

  s1 := 'мостки';
  s2 := 'костик';
  AssertTrue(DumDistanceMbrUtf16(s1, s2) = 2);
  AssertTrue(DumDistanceMbrUtf16(s2, s1) = 2);

  s1 := 'шапито';
  s2 := 'ашипот';
  AssertTrue(DumDistanceMbrUtf16(s1, s2) = 3);
  AssertTrue(DumDistanceMbrUtf16(s2, s1) = 3);

  s1 := 'дистанция';
  s2 := 'дисперсия';
  AssertTrue(DumDistanceMbrUtf16(s1, s2) = 4);
  AssertTrue(DumDistanceMbrUtf16(s2, s1) = 4);

  s1 := 'левенштейн';
  s2 := 'франкенштейн';
  AssertTrue(DumDistanceMbrUtf16(s1, s2) = 5);
  AssertTrue(DumDistanceMbrUtf16(s2, s1) = 5);

  s1 := 'ааааааа';
  s2 := 'ааааааа';
  AssertTrue(DumDistanceMbrUtf16(s1, s2) = 0);

  s1 := 'ааааааа';
  s2 := 'бббббббб';
  AssertTrue(DumDistanceMbrUtf16(s1, s2) = 8);
  AssertTrue(DumDistanceMbrUtf16(s2, s1) = 8);

  s1 := 'аааббаааа';
  s2 := 'ааааааа';
  AssertTrue(DumDistanceMbrUtf16(s1, s2) = 2);
  AssertTrue(DumDistanceMbrUtf16(s2, s1) = 2);

  s1 := 'tests';
  s2 := 'tset';
  AssertTrue(DumDistanceMbrUtf16(s1, s2) = 2);
  AssertTrue(DumDistanceMbrUtf16(s2, s1) = 2);

  s1 := '一天，在寒冷的冬天，我从森林里走出来。';
  s2 := '一，在寒冷的冬天，我从森林里走来。';
  AssertTrue(DumDistanceMbrUtf16(s1, s2) = 2);
  AssertTrue(DumDistanceMbrUtf16(s2, s1) = 2);
end;

procedure TTestUnicodeUtils.DumDistMbrBoundedUtf16;
var
  s1, s2: string;
begin
  s1 := '';
  s2 := 'привет';
  AssertTrue(DumDistanceMbrUtf16(s1, s1, 1) = 0);
  AssertTrue(DumDistanceMbrUtf16(s2, s2, 1) = 0);
  AssertTrue(DumDistanceMbrUtf16(s1, s2, 5) = -1);
  AssertTrue(DumDistanceMbrUtf16(s2, s1, 5) = -1);

  s1 := 'аб';
  s2 := 'аа';
  AssertTrue(DumDistanceMbrUtf16(s1, s2, 0) = -1);
  AssertTrue(DumDistanceMbrUtf16(s2, s1, 0) = -1);

  s1 := 'аб';
  s2 := 'ба';
  AssertTrue(DumDistanceMbrUtf16(s1, s2, 0) = -1);
  AssertTrue(DumDistanceMbrUtf16(s2, s1, 0) = -1);

  s1 := 'аc';
  s2 := 'cba';
  AssertTrue(DumDistanceMbrUtf16(s1, s2, 2) = -1);
  AssertTrue(DumDistanceMbrUtf16(s2, s1, 2) = -1);

  s1 := 'а';
  s2 := 'ббб';
  AssertTrue(DumDistanceMbrUtf16(s1, s2, 2) = -1);
  AssertTrue(DumDistanceMbrUtf16(s2, s1, 2) = -1);

  s1 := 'аабабаб';
  s2 := 'аббаа';
  AssertTrue(DumDistanceMbrUtf16(s1, s2, 2) = -1);
  AssertTrue(DumDistanceMbrUtf16(s2, s1, 2) = -1);

  s1 := 'нелли';
  s2 := 'елли';
  AssertTrue(DumDistanceMbrUtf16(s1, s2, 0) = -1);
  AssertTrue(DumDistanceMbrUtf16(s2, s1, 0) = -1);

  s1 := 'нелли';
  s2 := 'елил';
  AssertTrue(DumDistanceMbrUtf16(s1, s2, 1) = -1);
  AssertTrue(DumDistanceMbrUtf16(s2, s1, 1) = -1);

  s1 := 'мостки';
  s2 := 'костик';
  AssertTrue(DumDistanceMbrUtf16(s1, s2, 1) = -1);
  AssertTrue(DumDistanceMbrUtf16(s2, s1, 1) = -1);

  s1 := 'шапито';
  s2 := 'ашипот';
  AssertTrue(DumDistanceMbrUtf16(s1, s2, 2) = -1);
  AssertTrue(DumDistanceMbrUtf16(s2, s1, 2) = -1);

  s1 := 'дистанция';
  s2 := 'дисперсия';
  AssertTrue(DumDistanceMbrUtf16(s1, s2, 3) = -1);
  AssertTrue(DumDistanceMbrUtf16(s2, s1, 3) = -1);

  s1 := 'левенштейн';
  s2 := 'франкенштейн';
  AssertTrue(DumDistanceMbrUtf16(s1, s2, 4) = -1);
  AssertTrue(DumDistanceMbrUtf16(s2, s1, 4) = -1);

  s1 := 'ааааааа';
  s2 := 'бббббббб';
  AssertTrue(DumDistanceMbrUtf16(s1, s2, 7) = -1);
  AssertTrue(DumDistanceMbrUtf16(s2, s1, 7) = -1);

  s1 := 'аааббаааа';
  s2 := 'ааааааа';
  AssertTrue(DumDistanceMbrUtf16(s1, s2, 1) = -1);
  AssertTrue(DumDistanceMbrUtf16(s2, s1, 1) = -1);

  s1 := 'tests';
  s2 := 'tset';
  AssertTrue(DumDistanceMbrUtf16(s1, s2, 1) = -1);
  AssertTrue(DumDistanceMbrUtf16(s2, s1, 1) = -1);

  s1 := '一天，在寒冷的冬天，我从森林里走出来。';
  s2 := '一，在寒冷的冬天，我从森林里走来。';
  AssertTrue(DumDistanceMbrUtf16(s1, s2, 1) = -1);
  AssertTrue(DumDistanceMbrUtf16(s2, s1, 1) = -1);
end;

procedure TTestUnicodeUtils.DumDistMbrDynUtf16;
var
  s1, s2: string;
begin
  s1 := '';
  s2 := 'привет';
  AssertTrue(DumDistanceMbrUtf16(s1, s1, -1) = 0);
  AssertTrue(DumDistanceMbrUtf16(s2, s2, -1) = 0);
  AssertTrue(DumDistanceMbrUtf16(s1, s2, -1) = 6);
  AssertTrue(DumDistanceMbrUtf16(s2, s1, -1) = 6);

  s1 := 'аб';
  s2 := 'аа';
  AssertTrue(DumDistanceMbrUtf16(s1, s2, -1) = 1);
  AssertTrue(DumDistanceMbrUtf16(s2, s1, -1) = 1);

  s1 := 'аб';
  s2 := 'ба';
  AssertTrue(DumDistanceMbrUtf16(s1, s2, -1) = 1);
  AssertTrue(DumDistanceMbrUtf16(s2, s1, -1) = 1);

  s1 := 'аc';
  s2 := 'cba';
  AssertTrue(DumDistanceMbrUtf16(s1, s2, -1) = 3);
  AssertTrue(DumDistanceMbrUtf16(s2, s1, -1) = 3);

  s1 := 'а';
  s2 := 'ббб';
  AssertTrue(DumDistanceMbrUtf16(s1, s2, -1) = 3);
  AssertTrue(DumDistanceMbrUtf16(s2, s1, -1) = 3);

  s1 := 'аабабаб';
  s2 := 'аббаа';
  AssertTrue(DumDistanceMbrUtf16(s1, s2, -1) = 3);
  AssertTrue(DumDistanceMbrUtf16(s2, s1, -1) = 3);

  s1 := 'нелли';
  s2 := 'елли';
  AssertTrue(DumDistanceMbrUtf16(s1, s2, -1) = 1);
  AssertTrue(DumDistanceMbrUtf16(s2, s1, -1) = 1);

  s1 := 'нелли';
  s2 := 'елил';
  AssertTrue(DumDistanceMbrUtf16(s1, s2, -1) = 2);
  AssertTrue(DumDistanceMbrUtf16(s2, s1, -1) = 2);

  s1 := 'мостки';
  s2 := 'костик';
  AssertTrue(DumDistanceMbrUtf16(s1, s2, -1) = 2);
  AssertTrue(DumDistanceMbrUtf16(s2, s1, -1) = 2);

  s1 := 'шапито';
  s2 := 'ашипот';
  AssertTrue(DumDistanceMbrUtf16(s1, s2, -1) = 3);
  AssertTrue(DumDistanceMbrUtf16(s2, s1, -1) = 3);

  s1 := 'дистанция';
  s2 := 'дисперсия';
  AssertTrue(DumDistanceMbrUtf16(s1, s2, -1) = 4);
  AssertTrue(DumDistanceMbrUtf16(s2, s1, -1) = 4);

  s1 := 'левенштейн';
  s2 := 'франкенштейн';
  AssertTrue(DumDistanceMbrUtf16(s1, s2, -1) = 5);
  AssertTrue(DumDistanceMbrUtf16(s2, s1, -1) = 5);

  s1 := 'ааааааа';
  s2 := 'ааааааа';
  AssertTrue(DumDistanceMbrUtf16(s1, s2, -1) = 0);

  s1 := 'ааааааа';
  s2 := 'бббббббб';
  AssertTrue(DumDistanceMbrUtf16(s1, s2, -1) = 8);
  AssertTrue(DumDistanceMbrUtf16(s2, s1, -1) = 8);

  s1 := 'аааббаааа';
  s2 := 'ааааааа';
  AssertTrue(DumDistanceMbrUtf16(s1, s2, -1) = 2);
  AssertTrue(DumDistanceMbrUtf16(s2, s1, -1) = 2);

  s1 := 'tests';
  s2 := 'tset';
  AssertTrue(DumDistanceMbrUtf16(s1, s2, -1) = 2);
  AssertTrue(DumDistanceMbrUtf16(s2, s1, -1) = 2);

  s1 := '一天，在寒冷的冬天，我从森林里走出来。';
  s2 := '一，在寒冷的冬天，我从森林里走来。';
  AssertTrue(DumDistanceMbrUtf16(s1, s2, -1) = 2);
  AssertTrue(DumDistanceMbrUtf16(s2, s1, -1) = 2);
end;

procedure TTestUnicodeUtils.LcsGusUtf16Test;
var
  s1: string = '';
  s2: string = '';
  s: string;
begin
  AssertTrue(LcsGusUtf16(s1, s2) = '');

  s1 := 'аа';
  AssertTrue(LcsGusUtf16(s1, s2) = '');

  s2 := 'аб';
  s := 'а';
  AssertTrue(LcsGusUtf16(s1, s2) = s);

  s1 := 'тесть';
  s2 := 'это просто тест';
  s := 'тест';
  AssertTrue(LcsGusUtf16(s1, s2) = s);

  s1 := '1234';
  s2 := '1224533324';
  s := '1234';
  AssertTrue(LcsGusUtf16(s1, s2) = s);

  s1 := '01236789';
  s2 := '01234445556789';
  s := '01236789';
  AssertTrue(LcsGusUtf16(s1, s2) = s);
end;

procedure TTestUnicodeUtils.LcsKRUtf16Test;
var
  s1: string = '';
  s2: string = '';
  s: string;
begin
  AssertTrue(LcsKRUtf16(s1, s2) = '');

  s1 := 'аа';
  AssertTrue(LcsKRUtf16(s1, s2) = '');

  s2 := 'аб';
  s := 'а';
  AssertTrue(LcsKRUtf16(s1, s2) = s);

  s1 := 'тесть';
  s2 := 'это просто тест';
  s := 'тест';
  AssertTrue(LcsKRUtf16(s1, s2) = s);

  s1 := '1234';
  s2 := '1224533324';
  s := '1234';
  AssertTrue(LcsKRUtf16(s1, s2) = s);

  s1 := '01236789';
  s2 := '01234445556789';
  s := '01236789';
  AssertTrue(LcsKRUtf16(s1, s2) = s);
end;

procedure TTestUnicodeUtils.LcsMyersUtf16Test;
var
  s1: string = '';
  s2: string = '';
  s: string;
begin
  AssertTrue(LcsMyersUtf16(s1, s2) = '');

  s1 := 'аа';
  AssertTrue(LcsMyersUtf16(s1, s2) = '');

  s2 := 'аб';
  s := 'а';
  AssertTrue(LcsMyersUtf16(s1, s2) = s);

  s1 := 'тесть';
  s2 := 'это просто тест';
  s := 'тест';
  AssertTrue(LcsMyersUtf16(s1, s2) = s);

  s1 := '1234';
  s2 := '1224533324';
  s := '1234';
  AssertTrue(LcsMyersUtf16(s1, s2) = s);

  s1 := '01236789';
  s2 := '01234445556789';
  s := '01236789';
  AssertTrue(LcsMyersUtf16(s1, s2) = s);
end;

procedure TTestUnicodeUtils.SimRatioUtf16Test;
var
  s1, s2: string;
begin
  s1 := '';
  s2 := '';
  AssertTrue(SimRatioUtf16(s1, s2) = DblOne);

  s2 := 'ббб';
  AssertTrue(SimRatioUtf16(s1, s2) = DblZero);
  AssertTrue(SimRatioUtf16(s2, s1) = DblZero);

  s1 := 'ааа';
  AssertTrue(SimRatioUtf16(s1, s2) = DblZero);
  AssertTrue(SimRatioUtf16(s2, s1) = DblZero);

  s1 := 'ббб';
  AssertTrue(SimRatioUtf16(s1, s2) = DblOne);
  AssertTrue(SimRatioUtf16(s2, s1) = DblOne);
end;

procedure TTestUnicodeUtils.SimRatioExUtf16Test;
begin
  AssertTrue(SimRatioExUtf16('', '', [' ']) = DblOne);
  AssertTrue(SimRatioExUtf16('aaa', '', [' ']) = DblZero);
  AssertTrue(SimRatioExUtf16('', 'bbb', [' ']) = DblZero);
  AssertTrue(SimRatioExUtf16('Hello world', ' Hello world ', [' ']) = DblOne);
  AssertFalse(SimRatioExUtf16('Hello world', ' hello world ', [' ']) = DblOne);
  AssertTrue(SimRatioExUtf16('Hello world', ' hello world ', [' '], smSimple, [soIgnoreCase]) = DblOne);
  AssertTrue(SimRatioExUtf16('Hello world', 'another Hello world ', [' '], smSimple, [soPartial]) = DblOne);
  AssertTrue(SimRatioExUtf16('Hello, world!', ' hello world ', [' ',',','!'], smSimple, [soIgnoreCase]) = DblOne);
  AssertTrue(SimRatioExUtf16('World hello', ' Hello world ', [' '], smTokenSort, [soIgnoreCase]) = DblOne);
  AssertTrue(SimRatioExUtf16('World hello', ' Hello world, hello', [' ',','], smTokenSet, [soIgnoreCase]) = DblOne);
  AssertTrue(SimRatioExUtf16('World hello', ' Hello another world, hello', [' ',','], smTokenSet, [soIgnoreCase, soPartial]) = DblOne);
  AssertTrue(SimRatioExUtf16('fuzzy was a bear', 'fuzzy fuzzy fuzzy bear', [' '], smTokenSetEx) = DblOne);
end;

procedure TTestUnicodeUtils.StrReplaceUtf8Test;
var
  Txt, Key, Subst, s: string;
  Cnt: SizeInt;
const
  Key1   = '2335';
  Key2   = '2345';
  Key3   = 'ABCD42';
  Key4   = 'ABCDEF';
  Key5   = '1234567890ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz_!@#$%^&*()-=+|\/":;<>?987654321LKJIHGFEDCBA';
  Subst1 = 'FEDCBA42';

  Txt1 = '12345678901234567890';
  Txt2 =
    '1234567890ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz_!@#$%^&*()-=+|\/":;<>?987654321LKJIHGFEDCBA'+
    '1234567890ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz_!@#$%^&*()-=+|\/":;<>?987654321LKJIHGFEDCBA'+
    '1234567890ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz_!@#$%^&*()-=+|\/":;<>?987654321LKJIHGFEDCBA'+
    '1234567890ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz_!@#$%^&*()-=+|\/":;<>?987654321LKJIHGFEDCBA'+
    '1234567890ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz_!@#$%^&*()-=+|\/":;<>?987654321LKJIHGFEDCBA';

  Ans4 =
    '1234567890FEDCBA42GHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz_!@#$%^&*()-=+|\/":;<>?987654321LKJIHGFEDCBA'+
    '1234567890ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz_!@#$%^&*()-=+|\/":;<>?987654321LKJIHGFEDCBA'+
    '1234567890ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz_!@#$%^&*()-=+|\/":;<>?987654321LKJIHGFEDCBA'+
    '1234567890ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz_!@#$%^&*()-=+|\/":;<>?987654321LKJIHGFEDCBA'+
    '1234567890ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz_!@#$%^&*()-=+|\/":;<>?987654321LKJIHGFEDCBA';
  Ans4a =
    '1234567890FEDCBA42GHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz_!@#$%^&*()-=+|\/":;<>?987654321LKJIHGFEDCBA'+
    '1234567890FEDCBA42GHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz_!@#$%^&*()-=+|\/":;<>?987654321LKJIHGFEDCBA'+
    '1234567890FEDCBA42GHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz_!@#$%^&*()-=+|\/":;<>?987654321LKJIHGFEDCBA'+
    '1234567890FEDCBA42GHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz_!@#$%^&*()-=+|\/":;<>?987654321LKJIHGFEDCBA'+
    '1234567890FEDCBA42GHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz_!@#$%^&*()-=+|\/":;<>?987654321LKJIHGFEDCBA';
begin
  Key := '';
  Txt := '';
  Subst := '';
  Cnt := 42;
  AssertTrue(StrReplaceUtf8(Txt, Key, Subst, Cnt) = '');
  AssertTrue(Cnt = 0);

  Cnt := 42;
  Txt := Txt1;
  AssertTrue(StrReplaceUtf8(Txt, Key, Subst, Cnt) = Txt);
  AssertTrue(Cnt = 0);

  Cnt := 42;
  Key := Key1;
  AssertTrue(StrReplaceUtf8(Txt, Key, Subst, Cnt) = Txt);
  AssertTrue(Cnt = 0);

  Key := Key2;
  AssertTrue(StrReplaceUtf8(Txt, Key, Subst, Cnt) = '1678901234567890');
  AssertTrue(Cnt = 1);

  AssertTrue(StrReplaceUtf8(Txt, Key, Subst, Cnt, [sroReplaceAll]) = '167890167890');
  AssertTrue(Cnt = 2);

  Subst := 'abcde';
  AssertTrue(StrReplaceUtf8(Txt, Key, Subst, Cnt) = '1abcde678901234567890');
  AssertTrue(Cnt = 1);

  AssertTrue(StrReplaceUtf8(Txt, Key, Subst, Cnt, [sroReplaceAll]) = '1abcde678901abcde67890');
  AssertTrue(Cnt = 2);

  Txt := Txt2;
  Key := Key3;
  Subst := Subst1;
  AssertTrue(StrReplaceUtf8(Txt, Key, Subst, Cnt, [sroReplaceAll]) = Txt);
  AssertTrue(Cnt = 0);

  Key := Key4;
  AssertTrue(StrReplaceUtf8(Txt, Key, Subst, Cnt) = Ans4);
  AssertTrue(Cnt = 1);

  AssertTrue(StrReplaceUtf8(Txt, Key, Subst, Cnt, [sroReplaceAll]) = Ans4a);
  AssertTrue(Cnt = 5);

  Key := Key5;
  Subst := '';
  Cnt := 0;
  s := StrReplaceUtf8(Txt, Key, Subst, Cnt, [sroReplaceAll]);
  AssertTrue(s, s = '');
  AssertTrue(Cnt.ToString, Cnt = 5);
end;

procedure TTestUnicodeUtils.StrReplaceUtf8OwwTest;
var
  Txt, Key, Subst: string;
  Cnt: SizeInt;
const
  OptsOww    = [sroOnlyWholeWords];
  OptsOwwAll = [sroReplaceAll, sroOnlyWholeWords];
begin
  Cnt := 42;
  Key := '';
  Txt := '';
  Subst := '';
  AssertTrue(StrReplaceUtf8(Txt, Key, Subst, Cnt, OptsOww) = '');
  AssertTrue(Cnt = 0);

  Txt := '123123123123123123123';
  Cnt := 42;
  AssertTrue(StrReplaceUtf8(Txt, Key, Subst, Cnt, OptsOww) = Txt);
  AssertTrue(Cnt = 0);

  Key := '123';
  Cnt := 42;
  AssertTrue(StrReplaceUtf8(Txt, Key, Subst, Cnt, OptsOww) = Txt);
  AssertTrue(Cnt = 0);

  Txt := '123123-123-123123-123-123123';
  AssertTrue(StrReplaceUtf8(Txt, Key, Subst, Cnt, OptsOww) = '123123--123123-123-123123');
  AssertTrue(Cnt = 1);

  AssertTrue(StrReplaceUtf8(Txt, Key, Subst, Cnt, OptsOwwAll) = '123123--123123--123123');
  AssertTrue(Cnt = 2);

  Txt := '123-123123123-123';
  AssertTrue(StrReplaceUtf8(Txt, Key, Subst, Cnt, OptsOww) = '-123123123-123');
  AssertTrue(Cnt = 1);

  AssertTrue(StrReplaceUtf8(Txt, Key, Subst, Cnt, OptsOwwAll) = '-123123123-');
  AssertTrue(Cnt = 2);

  Key := '123-123123123-123';
  AssertTrue(StrReplaceUtf8(Txt, Key, Subst, Cnt, OptsOww) = '');
  AssertTrue(Cnt = 1);

  Subst := 'abcd';
  Cnt := 0;
  AssertTrue(StrReplaceUtf8(Txt, Key, Subst, Cnt, OptsOww) = 'abcd');
  AssertTrue(Cnt = 1);

  Key := '123';
  AssertTrue(StrReplaceUtf8(Txt, Key, Subst, Cnt, OptsOwwAll) = 'abcd-123123123-abcd');
  AssertTrue(Cnt = 2);
end;

procedure TTestUnicodeUtils.StrReplaceUtf8CITest;
var
  Txt, Key, Subst: string;
  Cnt: SizeInt;
const
  OptsCI    = [sroIgnoreCase];
  OptsCiAll = [sroReplaceAll, sroIgnoreCase];
begin
  Cnt := 42;
  Key := '';
  Txt := '';
  Subst := '';
  AssertTrue(StrReplaceUtf8(Txt, Key, Subst, Cnt, OptsCI) = '');
  AssertTrue(Cnt = 0);

  Cnt := 42;
  AssertTrue(StrReplaceUtf8(Txt, Key, Subst, Cnt, OptsCiAll) = '');
  AssertTrue(Cnt = 0);

  Txt := 'нечто что что-то чтобы почтой';
  Cnt := 42;
  AssertTrue(StrReplaceUtf8(Txt, Key, Subst, Cnt, OptsCI) = Txt);
  AssertTrue(Cnt = 0);

  Key := 'ЧТО';
  Subst := '?';
  AssertTrue(StrReplaceUtf8(Txt, Key, Subst, Cnt, OptsCI) = 'не? что что-то чтобы почтой');
  AssertTrue(Cnt = 1);

  AssertTrue(StrReplaceUtf8(Txt, Key, Subst, Cnt, OptsCiAll) = 'не? ? ?-то ?бы по?й');
  AssertTrue(Cnt = 5);

  Txt := 'Ɽнечто Ɫчто Ɽчто-то Ɑчтобы Ɫпочтой';
  Subst := 'What';
  AssertTrue(StrReplaceUtf8(Txt, Key, Subst, Cnt, OptsCI) = 'ⱤнеWhat Ɫчто Ɽчто-то Ɑчтобы Ɫпочтой');
  AssertTrue(Cnt = 1);

  AssertTrue(StrReplaceUtf8(Txt, Key, Subst, Cnt, OptsCiAll) = 'ⱤнеWhat ⱢWhat ⱤWhat-то ⱭWhatбы ⱢпоWhatй');
  AssertTrue(Cnt = 5);

 Txt :=  'ＬＡＺＡＲＵＳ это среда быстрой разработки приложений';
 Key := 'ＬａｚａｒｕＳ';
 Subst := 'Лазарь';

 AssertTrue(StrReplaceUtf8(Txt, Key, Subst, Cnt, OptsCI) = 'Лазарь это среда быстрой разработки приложений');
 AssertTrue(Cnt = 1);

 Cnt := 0;
 AssertTrue(StrReplaceUtf8(Txt, Key, Subst, Cnt, OptsCiAll) = 'Лазарь это среда быстрой разработки приложений');
 AssertTrue(Cnt = 1);
end;

procedure TTestUnicodeUtils.StrReplaceUtf8CiOwwTest;
var
  Txt, Key, Subst: string;
  Cnt: SizeInt;
const
  OptsCiOww    = [sroIgnoreCase, sroOnlyWholeWords];
  OptsCiOwwAll = [sroReplaceAll, sroIgnoreCase, sroOnlyWholeWords];
begin
  Cnt := 42;
  Key := '';
  Txt := '';
  Subst := '';
  AssertTrue(StrReplaceUtf8(Txt, Key, Subst, Cnt, OptsCiOww) = '');
  AssertTrue(Cnt = 0);

  Cnt := 42;
  AssertTrue(StrReplaceUtf8(Txt, Key, Subst, Cnt, OptsCiOwwAll) = '');
  AssertTrue(Cnt = 0);

  Txt := 'нечто что что-то чтобы почтой';
  Cnt := 42;
  AssertTrue(StrReplaceUtf8(Txt, Key, Subst, Cnt, OptsCiOww) = Txt);
  AssertTrue(Cnt = 0);

  Key := 'ЧТО';
  Subst := '?';
  AssertTrue(StrReplaceUtf8(Txt, Key, Subst, Cnt, OptsCiOww) = 'нечто ? что-то чтобы почтой');
  AssertTrue(Cnt = 1);

  AssertTrue(StrReplaceUtf8(Txt, Key, Subst, Cnt, OptsCiOwwAll) = 'нечто ? ?-то чтобы почтой');
  AssertTrue(Cnt = 2);

  Txt := 'Ɽнечто Ɑчтобы Ɫпочтой Ɫ что Ɽ что-то';
  Subst := 'What';
  AssertTrue(StrReplaceUtf8(Txt, Key, Subst, Cnt, OptsCiOww) = 'Ɽнечто Ɑчтобы Ɫпочтой Ɫ What Ɽ что-то');
  AssertTrue(Cnt = 1);

  AssertTrue(StrReplaceUtf8(Txt, Key, Subst, Cnt, OptsCiOwwAll) = 'Ɽнечто Ɑчтобы Ɫпочтой Ɫ What Ɽ What-то');
  AssertTrue(Cnt = 2);

  Txt :=  'ＬＡＺＡＲＵＳэто среда быстрой разработки приложений';
  Key := 'ＬａｚａｒｕＳ';
  Subst := 'Лазарь';

  AssertTrue(StrReplaceUtf8(Txt, Key, Subst, Cnt, OptsCiOww) = Txt);
  AssertTrue(Cnt = 0);

  Cnt := 42;
  AssertTrue(StrReplaceUtf8(Txt, Key, Subst, Cnt, OptsCiOwwAll) = Txt);
  AssertTrue(Cnt = 0);

  Txt :=  'ＬＡＺＡＲＵＳ это среда быстрой разработки приложений';
  AssertTrue(StrReplaceUtf8(Txt, Key, Subst, Cnt, OptsCiOww) = 'Лазарь это среда быстрой разработки приложений');
  AssertTrue(Cnt = 1);

  Cnt := 0;
  AssertTrue(StrReplaceUtf8(Txt, Key, Subst, Cnt, OptsCiOwwAll) = 'Лазарь это среда быстрой разработки приложений');
  AssertTrue(Cnt = 1);
end;

procedure TTestUnicodeUtils.ACStrReplaceTest;
var
  Src, Res: string;
  Keys, Subs: TStringArray;
  RepCount: SizeInt;
begin
  Keys := ['сам','сом','ход','самый','сомнительный','ходок','са','со','хо'];
  Subs := ['00','11','22','33','44','55','66','77','88'];
  Src := '';
  Res := ACStrReplace(Src, Keys, Subs);
  AssertTrue(Res = '');

  Src := 'быстрым движением возвел кверху глаза';
  Res := ACStrReplace(Src, Keys, Subs, RepCount);
  AssertTrue(RepCount = 0);
  AssertTrue(Res = Src);

  Src := 'самый сомнительный ходок сам по себе не сом, но ход';
  Res := ACStrReplace(Src, Keys, Subs, RepCount);
  AssertTrue(RepCount = 6);
  AssertTrue(Res = '00ый 11нительный 22ок 00 по себе не 11, но 22');

  Res := ACStrReplace(Src, Keys, Subs, [], ohmLeftmostLongest);
  AssertTrue(Res = '33 44 55 00 по себе не 11, но 22');

  Res := ACStrReplace(Src, Keys, Subs, [], ohmLeftmostShortest);
  AssertTrue(Res = '66мый 77мнительный 88док 66м по себе не 77м, но 88д');

  Res := ACStrReplace(Src, Keys, Subs, [srfOnlyWholeWords]);
  AssertTrue(Res = '33 44 55 00 по себе не 11, но 22');

  Res := ACStrReplace(Src, Keys, [], RepCount);
  AssertTrue(RepCount = 6);
  AssertTrue(Res = 'ый нительный ок  по себе не , но ');
end;

procedure TTestUnicodeUtils.ACStrReplaceCITest;
var
  Src, Res: string;
  Keys, Subs: TStringArray;
  RepCount: SizeInt;
begin
  Keys := ['САМ','СОМ','ХОД','САМЫЙ','СОМНИТЕЛЬНЫЙ','ХОДОК','СА','СО','ХО'];
  Subs := ['00','11','22','33','44','55','66','77','88'];
  Src := '';
  Res := ACStrReplace(Src, Keys, Subs);
  AssertTrue(Res = '');

  Src := 'быстрым движением возвел кверху глаза';
  Res := ACStrReplace(Src, Keys, Subs, RepCount, []);
  AssertTrue(RepCount = 0);
  AssertTrue(Res = Src);

  Src := 'İ самый сомнительный ходок сам по себе не сом, но ход';
  Res := ACStrReplace(Src, Keys, Subs, RepCount, [srfIgnoreCase]);
  AssertTrue(RepCount = 6);
  AssertTrue(Res = 'İ 00ый 11нительный 22ок 00 по себе не 11, но 22');

  Res := ACStrReplace(Src, Keys, Subs, [srfIgnoreCase], ohmLeftmostLongest);
  AssertTrue(Res = 'İ 33 44 55 00 по себе не 11, но 22');

  Res := ACStrReplace(Src, Keys, Subs, [srfIgnoreCase], ohmLeftmostShortest);
  AssertTrue(Res = 'İ 66мый 77мнительный 88док 66м по себе не 77м, но 88д');

  Res := ACStrReplace(Src, Keys, Subs, [srfOnlyWholeWords, srfIgnoreCase]);
  AssertTrue(Res = 'İ 33 44 55 00 по себе не 11, но 22');

  Res := ACStrReplace(Src, Keys, [], RepCount, [srfIgnoreCase]);
  AssertTrue(RepCount = 6);
  AssertTrue(Res = 'İ ый нительный ок  по себе не , но ');
end;

procedure TTestUnicodeUtils.ACStrReplaceListTest;
var
  Src, Res, Keys, Subs: TStringArray;
  RepCount: SizeInt;
begin
  Src := nil;
  Keys := ['ascot','entru','remonstrative'];
  Subs := [];
  Res := ACStrReplaceList(Src, Keys, Subs, RepCount);
  AssertTrue(RepCount = 0);
  AssertTrue(Res = nil);
  Src := [
    'momenta noncontrastable ascots nonsubstantive jumbly epicoelian',
    'knobstick unfathomed entrust equivale beautifiers',
    'incommunicado incautelous menticide remonstratively pullen'];
  Res := ACStrReplaceList(Src, Keys, Subs, RepCount);
  AssertTrue(RepCount = 3);
  AssertTrue(Length(Res) = 3);
  AssertTrue(Res[0] = 'momenta noncontrastable s nonsubstantive jumbly epicoelian');
  AssertTrue(Res[1] = 'knobstick unfathomed st equivale beautifiers');
  AssertTrue(Res[2] = 'incommunicado incautelous menticide ly pullen');

  Res := ACStrReplaceList(Src, Keys, Subs, RepCount, [srfOnlyWholeWords]);
  AssertTrue(RepCount = 0);
  AssertTrue(Length(Res) = 3);
  AssertTrue(Res[0] = Src[0]);
  AssertTrue(Res[1] = Src[1]);
  AssertTrue(Res[2] = Src[2]);
end;

{ TTestSeqUtils }

function TTestSeqUtils.ApplyLcsPatch(const aSrc: array of Integer; const aPatch: TSeqLcsPatch): TIntArray;
begin
  if not TIntSeqUtil.ApplyLcsPatch(aSrc, aPatch, Result) then
    raise Exception.Create('Failed apply LcsPatch');
end;

function TTestSeqUtils.ApplyPatch(const aSrc: array of Integer; const aPatch: TSeqPatch): TIntArray;
begin
  if not TIntSeqUtil.ApplyPatch(aSrc, aPatch, Result) then
    raise Exception.Create('Failed apply patch');
end;

procedure TTestSeqUtils.IsPrefixTest;
var
  a: TIntArray = nil;
  b: TIntArray = nil;
begin
  AssertFalse(TIntSeqUtil.IsPrefix(a, b));

  b := [1,2,3];
  AssertFalse(TIntSeqUtil.IsPrefix(a, b));

  a := [1,2,3,4];
  AssertFalse(TIntSeqUtil.IsPrefix(a, b));

  a := [1,2,4];
  AssertFalse(TIntSeqUtil.IsPrefix(a, b));

  a := [1,2,3];
  AssertTrue(TIntSeqUtil.IsPrefix(a, b));

  a := [1,2];
  AssertTrue(TIntSeqUtil.IsPrefix(a, b));

  a := [1,2,3,4];
  AssertTrue(TIntSeqUtil.IsPrefix(a, a));
  AssertTrue(TIntSeqUtil.IsPrefix(a[0..2], a));
end;

procedure TTestSeqUtils.IsSuffixTest;
var
  a: TIntArray = nil;
  b: TIntArray = nil;
begin
  AssertFalse(TIntSeqUtil.IsSuffix(a, b));

  b := [1,2,3];
  AssertFalse(TIntSeqUtil.IsSuffix(a, b));

  a := [0,1,2,3];
  AssertFalse(TIntSeqUtil.IsSuffix(a, b));

  a := [1,0,3];
  AssertFalse(TIntSeqUtil.IsSuffix(a, b));

  a := [1,2,3];
  AssertTrue(TIntSeqUtil.IsSuffix(a, b));

  a := [2,3];
  AssertTrue(TIntSeqUtil.IsSuffix(a, b));

  a := [0,1,2,3];
  AssertTrue(TIntSeqUtil.IsSuffix(a, a));
  AssertTrue(TIntSeqUtil.IsSuffix(a[1..3], a));
end;

procedure TTestSeqUtils.PrefixLenTest;
var
  a: TIntArray = nil;
  b: TIntArray = nil;
begin
  AssertTrue(TIntSeqUtil.CommonPrefixLen(a, b) = 0);

  b := [1,2,3];
  AssertTrue(TIntSeqUtil.CommonPrefixLen(a, b) = 0);

  a := [1,2,3,4];
  AssertTrue(TIntSeqUtil.CommonPrefixLen(a, b) = 3);

  a := [1,2,4];
  AssertTrue(TIntSeqUtil.CommonPrefixLen(a, b) = 2);

  a := [1,2,3,4];
  AssertTrue(TIntSeqUtil.CommonPrefixLen(a, a) = 4);
  AssertTrue(TIntSeqUtil.CommonPrefixLen(a[0..2], a) = 3);
end;

procedure TTestSeqUtils.SuffixLenTest;
var
  a: TIntArray = nil;
  b: TIntArray = nil;
begin
  AssertTrue(TIntSeqUtil.CommonSuffixLen(a, b) = 0);

  b := [1,2,3];
  AssertTrue(TIntSeqUtil.CommonSuffixLen(a, b) = 0);

  a := [0,1,2,3];
  AssertTrue(TIntSeqUtil.CommonSuffixLen(a, b) = 3);

  a := [1,0,3];
  AssertTrue(TIntSeqUtil.CommonSuffixLen(a, b) = 1);

  a := [0,1,2,3];
  AssertTrue(TIntSeqUtil.CommonSuffixLen(a, a) = 4);
  AssertTrue(TIntSeqUtil.CommonSuffixLen(a[1..3], a) = 3);
end;

procedure TTestSeqUtils.SameTest;
var
  a, b: TIntArray;
begin
  a := nil;
  b := nil;
  AssertTrue(TIntSeqUtil.Same(a, b));

  a := [42];
  AssertFalse(TIntSeqUtil.Same(a, b));

  b := [42];
  AssertTrue(TIntSeqUtil.Same(a, b));

  a := [42, 1001];
  b := [42, 1001, 42];
  AssertFalse(TIntSeqUtil.Same(a, b));

  a := [42, 1001, 42];
  AssertTrue(TIntSeqUtil.Same(a, b));
end;

procedure TTestSeqUtils.IsPermutationTest;
var
  a, b: TIntArray;
begin
  a := nil;
  b := nil;
  AssertTrue(TIntSeqUtil.IsPermutation(a, b));

  a := [42];
  AssertFalse(TIntSeqUtil.IsPermutation(a, b));

  b := [42];
  AssertTrue(TIntSeqUtil.IsPermutation(a, b));

  a := [42, 1001];
  b := [7, 1001, 42];
  AssertFalse(TIntSeqUtil.IsPermutation(a, b));

  a := [1001, 42, 7];
  AssertTrue(TIntSeqUtil.IsPermutation(a, b));

  b := [1001, 7, 42];
  AssertTrue(TIntSeqUtil.IsPermutation(a, b));

  a := [1001, 42, 7, 7];
  b := [7, 1001, 7, 42];
  AssertTrue(TIntSeqUtil.IsPermutation(a, b));
end;

procedure TTestSeqUtils.TestDiff;
var
  a: TIntArray = nil;
  b: TIntArray = nil;
  d: TIntSeqUtil.TDiff;
  I: Integer;
begin
  d := TIntSeqUtil.Diff(a, b);
  AssertTrue(d.SourceChanges = nil);
  AssertTrue(d.TargetChanges = nil);

  a := [1, 2, 3];
  d := TIntSeqUtil.Diff(a, b);
  AssertTrue(Length(d.SourceChanges) = Length(a));
  AssertTrue(d.TargetChanges = nil);
  for I := 0 to High(d.SourceChanges) do
    AssertTrue(d.SourceChanges[I]);

  b := [1, 2, 3];
  a := nil;
  d := TIntSeqUtil.Diff(a, b);
  AssertTrue(Length(d.TargetChanges) = Length(b));
  AssertTrue(d.SourceChanges = nil);
  for I := 0 to High(d.TargetChanges) do
    AssertTrue(d.TargetChanges[I]);

  a := [4, 5, 6];
  d := TIntSeqUtil.Diff(a, b);

  AssertTrue(Length(d.SourceChanges) = Length(a));
  AssertTrue(Length(d.TargetChanges) = Length(b));
  for I := 0 to High(d.SourceChanges) do
    AssertTrue(d.SourceChanges[I]);
  for I := 0 to High(d.TargetChanges) do
    AssertTrue(d.TargetChanges[I]);

  a := [3,3,3,4,3,3,3,4,3,5,7];
  b := [4,3,3,3,3,3,3,3,5];
  d := TIntSeqUtil.Diff(a, b);
  AssertTrue(Length(d.SourceChanges) = Length(a));
  AssertTrue(Length(d.TargetChanges) = Length(b));
  for I := 0 to High(a) do
    if I in [3,7,10] then
      AssertTrue(d.SourceChanges[I])
    else
      AssertFalse(d.SourceChanges[I]);
  for I := 0 to High(b) do
    if I = 0 then
      AssertTrue(d.TargetChanges[I])
    else
      AssertFalse(d.TargetChanges[I]);
end;

procedure TTestSeqUtils.TestLcsPatch;
var
  a: TIntArray = nil;
  b: TIntArray = nil;
  p: TSeqLcsPatch;
  e: TLcsEdit;
begin
  p := TIntSeqUtil.MakeLcsPatch(a, b);
  AssertTrue(p = nil);

  b := [1,2,3,4,5];
  p := TIntSeqUtil.MakeLcsPatch(a, b);
  AssertTrue(Length(p) = Length(b));
  for e in p do begin
    AssertTrue(e.Operation = seoInsert);
    AssertTrue(e.Value = b[e.TargetIndex]);
  end;
  AssertTrue(TIntSeqUtil.Same(ApplyLcsPatch(a, p), b));

  a := [1,2,3,4,5];
  p := TIntSeqUtil.MakeLcsPatch(a, b);
  AssertTrue(p = nil);

  b := [0,1,2,42,3,4,5,6,7];
  p := TIntSeqUtil.MakeLcsPatch(a, b);
  AssertTrue(Length(p) = TIntSeqUtil.LcsDistanceWM(a, b));
  for e in p do
    if e.Operation = seoDelete then
      AssertTrue(e.Value = a[e.SourceIndex])
    else
      AssertTrue(e.Value = b[e.TargetIndex]);
  AssertTrue(TIntSeqUtil.Same(ApplyLcsPatch(a, p), b));

  b := nil;
  p := TIntSeqUtil.MakeLcsPatch(a, b);
  AssertTrue(Length(p) = Length(a));
  for e in p do begin
    AssertTrue(e.Operation = seoDelete);
    AssertTrue(e.Value = a[e.SourceIndex]);
  end;
  AssertTrue(TIntSeqUtil.Same(ApplyLcsPatch(a, p), b));
end;

procedure TTestSeqUtils.TestPatch;
  procedure FillStat(var s: TPatchStat; aValue: SizeInt);
  begin
    s[seoMatch] := aValue;
    s[seoDelete] := aValue;
    s[seoInsert] := aValue;
    s[seoReplace] := aValue;
  end;
var
  a: TIntArray = nil;
  b: TIntArray = nil;
  p: TSeqPatch;
  Stat: TPatchStat;
  e: TSeqEdit;
  o: TIntSeqUtil.TSeqEditOp;
begin
  FillStat(Stat, 42);
  p := TIntSeqUtil.MakePatch(a, b, Stat);
  AssertTrue(p = nil);
  for o in TIntSeqUtil.TSeqEditOp do
    AssertTrue(Stat[o] = 0);

  b := [1,2,3,4,5];
  FillStat(Stat, 42);
  p := TIntSeqUtil.MakePatch(a, b, Stat);
  AssertTrue(Length(p) = Length(b));
  for o in TIntSeqUtil.TSeqEditOp do
    if o = seoInsert then
      AssertTrue(Stat[o] = Length(b))
    else
      AssertTrue(Stat[o] = 0);
  for e in p do begin
    AssertTrue(e.Operation = seoInsert);
    AssertTrue(e.TargetValue = b[e.TargetIndex]);
  end;
  AssertTrue(TIntSeqUtil.Same(ApplyPatch(a, p), b));

  a := [1,2,3,4,5];
  FillStat(Stat, 42);
  p := TIntSeqUtil.MakePatch(a, b, Stat);
  AssertTrue(p = nil);
  for o in TIntSeqUtil.TSeqEditOp do
    if o = seoMatch then
      AssertTrue(Stat[o] = Length(b))
    else
      AssertTrue(Stat[o] = 0);

  b := [7,8,9,4,5,6,7];
  FillStat(Stat, 42);
  p := TIntSeqUtil.MakePatch(a, b, Stat);
  AssertTrue(Length(p) = TIntSeqUtil.LevDistanceMBR(a, b));
  for e in p do begin
    Dec(Stat[e.Operation]);
    case e.Operation of
      seoDelete:  AssertTrue(e.SourceValue = a[e.SourceIndex]);
      seoInsert:  AssertTrue(e.TargetValue = b[e.TargetIndex]);
      seoReplace: AssertTrue((e.SourceValue = a[e.SourceIndex]) and (e.TargetValue = b[e.TargetIndex]));
    else
    end;
  end;
  for o in TIntSeqUtil.TSeqEditOp do
    if o <> seoMatch then
      AssertTrue(Stat[o] = 0);
  AssertTrue(TIntSeqUtil.Same(ApplyPatch(a, p), b));

  b := nil;
  FillStat(Stat, 42);
  p := TIntSeqUtil.MakePatch(a, b, Stat);
  AssertTrue(Length(p) = Length(a));
  for o in TIntSeqUtil.TSeqEditOp do
    if o = seoDelete then
      AssertTrue(Stat[o] = Length(a))
    else
      AssertTrue(Stat[o] = 0);
  for e in p do begin
    AssertTrue(e.Operation = seoDelete);
    AssertTrue(e.SourceValue = a[e.SourceIndex]);
  end;
  AssertTrue(TIntSeqUtil.Same(ApplyPatch(a, p), b));
end;

{ TTestFuzzySearchBitap }

procedure TTestFuzzySearchBitap.TestCreate;
var
  fsb: TFuzzySearchBitap;
begin
  fsb.Init('');
  AssertFalse(fsb.Initialized);
  AssertTrue(fsb.Length = 0);
  AssertFalse(fsb.CaseInsensitive);

  fsb := TFuzzySearchBitap.Create('');
  AssertFalse(fsb.Initialized);
  AssertTrue(fsb.Length = 0);
  AssertFalse(fsb.CaseInsensitive);

  fsb.Init('a');
  AssertTrue(fsb.Initialized);
  AssertTrue(fsb.Length = 1);
  AssertFalse(fsb.CaseInsensitive);

  fsb := TFuzzySearchBitap.Create('a');
  AssertTrue(fsb.Initialized);
  AssertTrue(fsb.Length = 1);
  AssertFalse(fsb.CaseInsensitive);

  fsb.Init('a', True);
  AssertTrue(fsb.Initialized);
  AssertTrue(fsb.Length = 1);
  AssertTrue(fsb.CaseInsensitive);

  fsb := TFuzzySearchBitap.Create('a', True);
  AssertTrue(fsb.Initialized);
  AssertTrue(fsb.Length = 1);
  AssertTrue(fsb.CaseInsensitive);

  fsb.Init(Utf8StringOfChar('ц', fsb.MAX_PATTERN_CP));
  AssertTrue(fsb.Initialized);
  AssertTrue(fsb.Length = fsb.MAX_PATTERN_CP);
  AssertFalse(fsb.CaseInsensitive);

  fsb := TFuzzySearchBitap.Create(Utf8StringOfChar('ц', fsb.MAX_PATTERN_CP));
  AssertTrue(fsb.Initialized);
  AssertTrue(fsb.Length = fsb.MAX_PATTERN_CP);
  AssertFalse(fsb.CaseInsensitive);

  fsb.Init(Utf8StringOfChar('ц', fsb.MAX_PATTERN_CP + 1));
  AssertFalse(fsb.Initialized);
  AssertTrue(fsb.Length = 0);
  AssertFalse(fsb.CaseInsensitive);

  fsb := TFuzzySearchBitap.Create(Utf8StringOfChar('ц', fsb.MAX_PATTERN_CP + 1));
  AssertFalse(fsb.Initialized);
  AssertTrue(fsb.Length = 0);
  AssertFalse(fsb.CaseInsensitive);
end;

procedure TTestFuzzySearchBitap.TestNextMatch;
var
  fsb: TFuzzySearchBitap;
  Text, Pattern: string;
  m: TMatch;
begin
  Pattern := 'a';
  fsb.Init(Pattern);

  Text := '';
  m := fsb.NextMatch(Text, 1);
  AssertTrue((m.Offset = 0) and (m.Length = 0));

  Text := 'b';
  m := fsb.NextMatch(Text, 1, 0);
  AssertTrue((m.Offset = 1) and (m.Length = 1));

  m := fsb.NextMatch(Text, 1, 2);
  AssertTrue((m.Offset = 0) and (m.Length = 0));

  m := fsb.NextMatch(Text, fsb.MAX_PATTERN_CP + 1, 1);
  AssertTrue((m.Offset = 0) and (m.Length = 0));

  Pattern := 'abc';
  fsb.Init(Pattern);

  Text := 'aaac';
  m := fsb.NextMatch(Text, 1);
  AssertTrue((m.Offset = 2) and (m.Length = 3));

  Pattern := 'различно';
  fsb.Init(Pattern);
  Text := 'very, очень различна, type, тип различен, mesure, мера различия, has, имеет различие, entirely, совсем безлично';

  m := fsb.NextMatch(Text, 0);
  AssertTrue((m.Offset = 0) and (m.Length = 0));

  m := fsb.NextMatch(Text, 1);
  AssertTrue((m.Offset <> 0) and (m.Length <> 0));
  AssertTrue(Copy(Text, m.Offset, m.Length) = 'различна');

  m := fsb.NextMatch(Text, 1, m.Offset + m.Length);
  AssertTrue((m.Offset = 0) and (m.Length = 0));

  m := fsb.NextMatch(Text, 2);
  AssertTrue((m.Offset <> 0) and (m.Length <> 0));
  AssertTrue(Copy(Text, m.Offset, m.Length) = 'различна');

  m := fsb.NextMatch(Text, 2, m.Offset + m.Length);
  AssertTrue((m.Offset <> 0) and (m.Length <> 0));
  AssertTrue(Copy(Text, m.Offset, m.Length) = 'различен');

  m := fsb.NextMatch(Text, 2, m.Offset + m.Length);
  AssertTrue((m.Offset <> 0) and (m.Length <> 0));
  AssertTrue(Copy(Text, m.Offset, m.Length) = 'различия');

  m := fsb.NextMatch(Text, 2, m.Offset + m.Length);
  AssertTrue((m.Offset <> 0) and (m.Length <> 0));
  AssertTrue(Copy(Text, m.Offset, m.Length) = 'различие');

  m := fsb.NextMatch(Text, 2, m.Offset + m.Length);
  AssertTrue((m.Offset <> 0) and (m.Length <> 0));
  AssertTrue(Copy(Text, m.Offset, m.Length) = 'безлично');
end;

procedure TTestFuzzySearchBitap.TestNextMatchOww;
var
  fsb: TFuzzySearchBitap;
  Text, Pattern: string;
  m: TMatch;
begin
  Text := '113!12345.322.1223.133+223-323';
  Pattern := '123';
  fsb.Init(Pattern, False, True);

  m := fsb.NextMatch(Text, 0);
  AssertTrue((m.Offset = 0) and (m.Length = 0));

  m := fsb.NextMatch(Text, 1);
  AssertTrue((m.Offset = 1) and (m.Length = 3));

  m := fsb.NextMatch(Text, 1, m.Offset + m.Length);
  AssertTrue((m.Offset = 20) and (m.Length = 3));

  m := fsb.NextMatch(Text, 1, m.Offset + m.Length);
  AssertTrue((m.Offset = 24) and (m.Length = 3));

  m := fsb.NextMatch(Text, 1, m.Offset + m.Length);
  AssertTrue((m.Offset = 28) and (m.Length = 3));
end;

procedure TTestFuzzySearchBitap.TestNextMatchCI;
var
  fsb: TFuzzySearchBitap;
  Text, Pattern: string;
  m: TMatch;
begin
  Pattern := 'различно';
  fsb.Init(Pattern, True);
  Text := 'very, очень различНа, type, тип рАзЛичен, mesure, мера РаЗличия, has, имеет разлиЧие, entirely, совсем безличНО';

  m := fsb.NextMatch(Text, 0);
  AssertTrue((m.Offset = 0) and (m.Length = 0));

  m := fsb.NextMatch(Text, 1);
  AssertTrue((m.Offset <> 0) and (m.Length <> 0));
  AssertTrue(Copy(Text, m.Offset, m.Length) = 'различНа');

  m := fsb.NextMatch(Text, 1, m.Offset + m.Length);
  AssertTrue((m.Offset = 0) and (m.Length = 0));

  m := fsb.NextMatch(Text, 2);
  AssertTrue((m.Offset <> 0) and (m.Length <> 0));
  AssertTrue(Copy(Text, m.Offset, m.Length) = 'различНа');

  m := fsb.NextMatch(Text, 2, m.Offset + m.Length);
  AssertTrue((m.Offset <> 0) and (m.Length <> 0));
  AssertTrue(Copy(Text, m.Offset, m.Length) = 'рАзЛичен');

  m := fsb.NextMatch(Text, 2, m.Offset + m.Length);
  AssertTrue((m.Offset <> 0) and (m.Length <> 0));
  AssertTrue(Copy(Text, m.Offset, m.Length) = 'РаЗличия');

  m := fsb.NextMatch(Text, 2, m.Offset + m.Length);
  AssertTrue((m.Offset <> 0) and (m.Length <> 0));
  AssertTrue(Copy(Text, m.Offset, m.Length) = 'разлиЧие');

  m := fsb.NextMatch(Text, 2, m.Offset + m.Length);
  AssertTrue((m.Offset <> 0) and (m.Length <> 0));
  AssertTrue(Copy(Text, m.Offset, m.Length) = 'безличНО');

  m := fsb.NextMatch(Text, 2, m.Offset + m.Length);
  AssertTrue((m.Offset = 0) and (m.Length = 0));
end;

procedure TTestFuzzySearchBitap.TestNextMatchCIOww;
var
  fsb: TFuzzySearchBitap;
  Text, Pattern: string;
  m: TMatch;
begin
  Text := 'чисто перечислять вычислять веслом весло маслом масло числом числа несло';
  Pattern := 'ЧИСЛО';
  fsb.Init(Pattern, True, True);

  m := fsb.NextMatch(Text, 0);
  AssertTrue((m.Offset = 0) and (m.Length = 0));

  m := fsb.NextMatch(Text, 1);
  AssertTrue(m.Offset = 1);
  AssertTrue(Copy(Text, m.Offset, m.Length) = 'чисто');

  m := fsb.NextMatch(Text, 2, m.Offset + m.Length);
  AssertTrue((m.Offset <> 0) and (m.Length <> 0));
  AssertTrue(Copy(Text, m.Offset, m.Length) = 'весло');

  m := fsb.NextMatch(Text, 2, m.Offset + m.Length);
  AssertTrue((m.Offset <> 0) and (m.Length <> 0));
  AssertTrue(Copy(Text, m.Offset, m.Length) = 'масло');

  m := fsb.NextMatch(Text, 2, m.Offset + m.Length);
  AssertTrue((m.Offset <> 0) and (m.Length <> 0));
  AssertTrue(Copy(Text, m.Offset, m.Length) = 'числа');

  m := fsb.NextMatch(Text, 2, m.Offset + m.Length);
  AssertTrue((m.Offset <> 0) and (m.Length <> 0));
  AssertTrue(Copy(Text, m.Offset, m.Length) = 'несло');

  m := fsb.NextMatch(Text, 2, m.Offset + m.Length);
  AssertTrue((m.Offset = 0) and (m.Length = 0));
end;

procedure TTestFuzzySearchBitap.TestFindMatches;
var
  fsb: TFuzzySearchBitap;
  Text, Pattern: string;
  ma: array of TMatch;
begin
  Pattern := 'различно';
  fsb.Init(Pattern);
  Text := 'very, очень различна, type, тип различен, mesure, мера различия, has, имеет различие, entirely, совсем безлично';

  ma := fsb.FindMatches(Text, 0);
  AssertTrue(ma = nil);

  ma := fsb.FindMatches(Text, 1);
  AssertTrue(Length(ma) = 1);
  AssertTrue(Copy(Text, ma[0].Offset, ma[0].Length) = 'различна');

  ma := fsb.FindMatches(Text, 2);
  AssertTrue(Length(ma) = 5);
  AssertTrue(Copy(Text, ma[0].Offset, ma[0].Length) = 'различна');
  AssertTrue(Copy(Text, ma[1].Offset, ma[1].Length) = 'различен');
  AssertTrue(Copy(Text, ma[2].Offset, ma[2].Length) = 'различия');
  AssertTrue(Copy(Text, ma[3].Offset, ma[3].Length) = 'различие');
  AssertTrue(Copy(Text, ma[4].Offset, ma[4].Length) = 'безлично');

  ma := fsb.FindMatches(Text, 2, ma[1].Offset + ma[1].Length, 1);
  AssertTrue(Length(ma) = 1);
  AssertTrue(Copy(Text, ma[0].Offset, ma[0].Length) = 'различия');
end;

procedure TTestFuzzySearchBitap.TestFindMatchesOww;
var
  fsb: TFuzzySearchBitap;
  Text, Pattern: string;
  ma: array of TMatch;
begin
  Text := 'чисто перечислять вычислять веслом весло маслом масло числом числа несло';
  Pattern := 'число';
  fsb.Init(Pattern, False, True);

  ma := fsb.FindMatches(Text, 0);
  AssertTrue(ma = nil);

  ma := fsb.FindMatches(Text, 1);
  AssertTrue(Length(ma) = 2);
  AssertTrue(Copy(Text, ma[0].Offset, ma[0].Length) = 'чисто');
  AssertTrue(Copy(Text, ma[1].Offset, ma[1].Length) = 'числа');

  ma := fsb.FindMatches(Text, 2);
  AssertTrue(Length(ma) = 5);
  AssertTrue(Copy(Text, ma[0].Offset, ma[0].Length) = 'чисто');
  AssertTrue(Copy(Text, ma[1].Offset, ma[1].Length) = 'весло');
  AssertTrue(Copy(Text, ma[2].Offset, ma[2].Length) = 'масло');
  AssertTrue(Copy(Text, ma[3].Offset, ma[3].Length) = 'числа');
  AssertTrue(Copy(Text, ma[4].Offset, ma[4].Length) = 'несло');
end;

procedure TTestFuzzySearchBitap.TestFindMatchesCI;
var
  fsb: TFuzzySearchBitap;
  Text, Pattern: string;
  ma: array of TMatch;
begin
  Pattern := 'различно';
  fsb.Init(Pattern, True);
  Text := 'very, очень различНа, type, тип рАзЛичен, mesure, мера РаЗличия, has, имеет разлиЧие, entirely, совсем безличНО';

  ma := fsb.FindMatches(Text, 0);
  AssertTrue(ma = nil);

  ma := fsb.FindMatches(Text, 1);
  AssertTrue(Length(ma) = 1);
  AssertTrue(Copy(Text, ma[0].Offset, ma[0].Length) = 'различНа');

  ma := fsb.FindMatches(Text, 2);
  AssertTrue(Length(ma) = 5);
  AssertTrue(Copy(Text, ma[0].Offset, ma[0].Length) = 'различНа');
  AssertTrue(Copy(Text, ma[1].Offset, ma[1].Length) = 'рАзЛичен');
  AssertTrue(Copy(Text, ma[2].Offset, ma[2].Length) = 'РаЗличия');
  AssertTrue(Copy(Text, ma[3].Offset, ma[3].Length) = 'разлиЧие');
  AssertTrue(Copy(Text, ma[4].Offset, ma[4].Length) = 'безличНО');

  ma := fsb.FindMatches(Text, 2, ma[1].Offset + ma[1].Length, 1);
  AssertTrue(Length(ma) = 1);
  AssertTrue(Copy(Text, ma[0].Offset, ma[0].Length) = 'РаЗличия');
end;

procedure TTestFuzzySearchBitap.TestFindMatchesCIOww;
var
  fsb: TFuzzySearchBitap;
  Text, Pattern: string;
  ma: array of TMatch;
begin
  Text := 'чисто перечислять вычислять веслом весло маслом масло числом числа несло';
  Pattern := 'ЧИСЛО';
  fsb.Init(Pattern, True, True);

  ma := fsb.FindMatches(Text, 0);
  AssertTrue(ma = nil);

  ma := fsb.FindMatches(Text, 1);
  AssertTrue(Length(ma) = 2);
  AssertTrue(Copy(Text, ma[0].Offset, ma[0].Length) = 'чисто');
  AssertTrue(Copy(Text, ma[1].Offset, ma[1].Length) = 'числа');

  ma := fsb.FindMatches(Text, 2);
  AssertTrue(Length(ma) = 5);
  AssertTrue(Copy(Text, ma[0].Offset, ma[0].Length) = 'чисто');
  AssertTrue(Copy(Text, ma[1].Offset, ma[1].Length) = 'весло');
  AssertTrue(Copy(Text, ma[2].Offset, ma[2].Length) = 'масло');
  AssertTrue(Copy(Text, ma[3].Offset, ma[3].Length) = 'числа');
  AssertTrue(Copy(Text, ma[4].Offset, ma[4].Length) = 'несло');
end;

procedure TTestFuzzySearchBitap.TestEnumerator;
var
  fsb: TFuzzySearchBitap;
  Text, Pattern: string;
  m: TMatch;
  ans: TStringArray;
  I, TestPos: Integer;
begin
  Pattern := 'различно';
  fsb.Init(Pattern);
  Text := 'very, очень различна, type, тип различен, mesure, мера различия, has, имеет различие, entirely, совсем безлично';

  I := 0;
  for m in fsb.Matches(Text, 0) do
    Inc(I);
  AssertTrue(I = 0);

  ans := ['различна', 'различен', 'различия', 'различие', 'безлично'];

  for m in fsb.Matches(Text, 1) do begin
    AssertTrue(Copy(Text, m.Offset, m.Length) = ans[I]);
    Inc(I);
  end;
  AssertTrue(I = 1);

  I := 0;
  TestPos := 1;
  for m in fsb.Matches(Text, 2) do begin
    AssertTrue(Copy(Text, m.Offset, m.Length) = ans[I]);
    if I = 2 then
      TestPos := m.Offset + m.Length;
    Inc(I);
  end;
  AssertTrue(I = 5);

  I := 3;
  for m in fsb.Matches(Text, 2, TestPos) do begin
    AssertTrue(Copy(Text, m.Offset, m.Length) = ans[I]);
    Inc(I);
  end;
  AssertTrue(I = 5);
end;

procedure TTestFuzzySearchBitap.TestEnumeratorOww;
var
  fsb: TFuzzySearchBitap;
  Text, Pattern: string;
  m: TMatch;
  ans: TStringArray;
  I: Integer;
begin
  Text := 'чисто перечислять вычислять веслом весло маслом масло числом числа несло';
  Pattern := 'число';
  fsb.Init(Pattern, False, True);

  I := 0;
  for m in fsb.Matches(Text, 0) do
    Inc(I);
  AssertTrue(I = 0);

  ans := ['чисто', 'числа'];
  I := 0;
  for m in fsb.Matches(Text, 1) do begin
    AssertTrue(Copy(Text, m.Offset, m.Length) = ans[I]);
    Inc(I);
  end;

  ans := ['чисто', 'весло', 'масло', 'числа', 'несло'];
  I := 0;
  for m in fsb.Matches(Text, 2) do begin
    AssertTrue(Copy(Text, m.Offset, m.Length) = ans[I]);
    Inc(I);
  end;
end;

procedure TTestFuzzySearchBitap.TestEnumeratorCI;
var
  fsb: TFuzzySearchBitap;
  Text, Pattern: string;
  m: TMatch;
  ans: TStringArray;
  I, TestPos: Integer;
begin
  Pattern := 'различно';
  fsb.Init(Pattern, True);
  Text := 'very, очень различНа, type, тип рАзЛичен, mesure, мера РаЗличия, has, имеет разлиЧие, entirely, совсем безличНО';

  I := 0;
  for m in fsb.Matches(Text, 0) do
    Inc(I);
  AssertTrue(I = 0);

  ans := ['различНа', 'рАзЛичен', 'РаЗличия', 'разлиЧие', 'безличНО'];

  for m in fsb.Matches(Text, 1) do begin
    AssertTrue(Copy(Text, m.Offset, m.Length) = ans[I]);
    Inc(I);
  end;
  AssertTrue(I = 1);

  I := 0;
  TestPos := 1;
  for m in fsb.Matches(Text, 2) do begin
    AssertTrue(Copy(Text, m.Offset, m.Length) = ans[I]);
    if I = 2 then
      TestPos := m.Offset + m.Length;
    Inc(I);
  end;
  AssertTrue(I = 5);

  I := 3;
  for m in fsb.Matches(Text, 2, TestPos) do begin
    AssertTrue(Copy(Text, m.Offset, m.Length) = ans[I]);
    Inc(I);
  end;
  AssertTrue(I = 5);
end;

procedure TTestFuzzySearchBitap.TestEnumeratorCIOww;
var
  fsb: TFuzzySearchBitap;
  Text, Pattern: string;
  m: TMatch;
  ans: TStringArray;
  I: Integer;
begin
  Text := 'чисто перечислять вычислять веслом весло маслом масло числом числа несло';
  Pattern := 'ЧИСЛО';
  fsb.Init(Pattern, True, True);

  I := 0;
  for m in fsb.Matches(Text, 0) do
    Inc(I);
  AssertTrue(I = 0);

  ans := ['чисто', 'числа'];
  I := 0;
  for m in fsb.Matches(Text, 1) do begin
    AssertTrue(Copy(Text, m.Offset, m.Length) = ans[I]);
    Inc(I);
  end;

  ans := ['чисто', 'весло', 'масло', 'числа', 'несло'];
  I := 0;
  for m in fsb.Matches(Text, 2) do begin
    AssertTrue(Copy(Text, m.Offset, m.Length) = ans[I]);
    Inc(I);
  end;
end;

{ TACSearchFsmTest }

function TACSearchFsmTest.OnMatch(const m: TMatch): Boolean;
begin
  Inc(FMatchCount);
  Result := True;
end;

function TACSearchFsmTest.OnMatchFirst(const m: TMatch): Boolean;
begin
  Inc(FMatchCount);
  Result := False;
end;

procedure TACSearchFsmTest.TestCreate;
var
  ac: IACSearchFsmUtf8;
begin
  ac := CreateAcFsmUtf8([]);
  AssertTrue(ac.PatternCount = 0);
  AssertFalse(ac.IsMatch('a'));

  ac := CreateAcFsmUtf8(['', '']);
  AssertTrue(ac.PatternCount = 0);

  ac := CreateAcFsmUtf8(['', ''], False, True);
  AssertTrue(ac.PatternCount = 0);

  ac := CreateAcFsmUtf8(['a','b']);
  AssertFalse(ac.CaseInsensitive);
  AssertTrue(ac.PatternCount = 2);
  AssertTrue(ac.IsMatch('a'));
  AssertTrue(ac.IsMatch('b'));

  ac := CreateAcFsmUtf8(['a','b'], True);
  AssertTrue(ac.CaseInsensitive);
  AssertTrue(ac.PatternCount = 2);
  AssertTrue(ac.IsMatch('A'));
  AssertTrue(ac.IsMatch('B'));

  ac := CreateAcFsmUtf8(['a','b'], False, True);
  AssertFalse(ac.CaseInsensitive);
  AssertTrue(ac.PatternCount = 2);
  AssertTrue(ac.IsMatch('a'));
  AssertTrue(ac.IsMatch('b'));

  ac := CreateAcFsmUtf8(['a','b'], True, True);
  AssertTrue(ac.CaseInsensitive);
  AssertTrue(ac.PatternCount = 2);
  AssertTrue(ac.IsMatch('A'));
  AssertTrue(ac.IsMatch('B'));
end;

procedure TACSearchFsmTest.TestSearchDelegatedDfa;
var
  ac: IACSearchFsmUtf8;
  a: TStringArray;
  s: string;
begin
  a := ['her', 'is', 'his', 'she', 'hi', 'he', 'i', 'hero'];
  ac := CreateAcFsmUtf8(a);
  s := 'hishero';
  FMatchCount := 0;
  ac.Search(s, @OnMatch);
  AssertTrue(FMatchCount = Length(a));

  FMatchCount := 0;
  ac.Search(s, @OnMatchFirst);
  AssertTrue(FMatchCount = 1);
end;

procedure TACSearchFsmTest.TestSearchDelegatedNfa;
var
  ac: IACSearchFsmUtf8;
  a: TStringArray;
  s: string;
begin
  a := ['her', 'is', 'his', 'she', 'hi', 'he', 'i', 'hero'];
  ac := CreateAcFsmUtf8(a, False, True);
  s := 'hishero';
  FMatchCount := 0;
  ac.Search(s, @OnMatch);
  AssertTrue(FMatchCount = Length(a));

  FMatchCount := 0;
  ac.Search(s, @OnMatchFirst);
  AssertTrue(FMatchCount = 1);
end;

procedure TACSearchFsmTest.TestSearchDelegatedDfaCI;
var
  ac: IACSearchFsmUtf8;
  a: TStringArray;
  s: string;
begin
  a := ['her', 'is', 'his', 'she', 'hi', 'he', 'i', 'hero'];
  ac := CreateAcFsmUtf8(a, True);
  s := 'HISHERO';
  FMatchCount := 0;
  ac.Search(s, @OnMatch);
  AssertTrue(FMatchCount = Length(a));

  FMatchCount := 0;
  ac.Search(s, @OnMatchFirst);
  AssertTrue(FMatchCount = 1);
end;

procedure TACSearchFsmTest.TestSearchDelegatedNfaCI;
var
  ac: IACSearchFsmUtf8;
  a: TStringArray;
  s: string;
begin
  a := ['her', 'is', 'his', 'she', 'hi', 'he', 'i', 'hero'];
  ac := CreateAcFsmUtf8(a, True, True);
  s := 'HISHERO';
  FMatchCount := 0;
  ac.Search(s, @OnMatch);
  AssertTrue(FMatchCount = Length(a));

  FMatchCount := 0;
  ac.Search(s, @OnMatchFirst);
  AssertTrue(FMatchCount = 1);
end;

procedure TACSearchFsmTest.TestSearchNestedDfa;
var
  MatchCount: Integer = 0;
  function NestMatch(const m: TMatch): Boolean;
  begin
    Inc(MatchCount);
    Result := True;
  end;
  function FirstMatch(const m: TMatch): Boolean;
  begin
    Inc(MatchCount);
    Result := False;
  end;
var
  s: string;
  ss: TStrSet;
  function TestMatch(const m: TMatch): Boolean;
  begin
    ss.Remove(Copy(s, m.Offset, m.Length));
    Result := True;
  end;
var
  ac: IACSearchFsmUtf8;
  a: TStringArray;
begin
  a := ['her', 'is', 'his', 'she', 'hi', 'he', 'i', 'hero'];
  ac := CreateAcFsmUtf8(a);
  s := 'hishero';

  ac.Search(s, @NestMatch);
  AssertTrue(MatchCount = Length(a));

  MatchCount := 0;
  ac.Search(s, @FirstMatch);
  AssertTrue(MatchCount = 1);

  ss.AddAll(a);
  AssertTrue(ss.Count = Length(a));
  ac.Search(s, @TestMatch);
  AssertTrue(ss.IsEmpty);
end;

procedure TACSearchFsmTest.TestSearchNestedNfa;
var
  MatchCount: Integer = 0;
  function NestMatch(const m: TMatch): Boolean;
  begin
    Inc(MatchCount);
    Result := True;
  end;
  function FirstMatch(const m: TMatch): Boolean;
  begin
    Inc(MatchCount);
    Result := False;
  end;
var
  s: string;
  ss: TStrSet;
  function TestMatch(const m: TMatch): Boolean;
  begin
    ss.Remove(Copy(s, m.Offset, m.Length));
    Result := True;
  end;
var
  ac: IACSearchFsmUtf8;
  a: TStringArray;
begin
  a := ['her', 'is', 'his', 'she', 'hi', 'he', 'i', 'hero'];
  ac := CreateAcFsmUtf8(a, False, True);
  s := 'hishero';

  ac.Search(s, @NestMatch);
  AssertTrue(MatchCount = Length(a));

  MatchCount := 0;
  ac.Search(s, @FirstMatch);
  AssertTrue(MatchCount = 1);

  ss.AddAll(a);
  AssertTrue(ss.Count = Length(a));
  ac.Search(s, @TestMatch);
  AssertTrue(ss.IsEmpty);
end;

procedure TACSearchFsmTest.TestSearchNestedDfaCI;
var
  MatchCount: Integer = 0;
  function NestMatch(const m: TMatch): Boolean;
  begin
    Inc(MatchCount);
    Result := True;
  end;
  function FirstMatch(const m: TMatch): Boolean;
  begin
    Inc(MatchCount);
    Result := False;
  end;
var
  s: string;
  ss: TStrSet;
  function TestMatch(const m: TMatch): Boolean;
  begin
    ss.Remove(Copy(s, m.Offset, m.Length));
    Result := True;
  end;
var
  ac: IACSearchFsmUtf8;
  a: TStringArray;
  I: Integer;
begin
  a := ['HER', 'İS', 'HİS', 'SHE', 'Hİ', 'HE', 'İ', 'HERO'];
  ac := CreateAcFsmUtf8(a, True);
  s := 'hishero';

  ac.Search(s, @NestMatch);
  AssertTrue(MatchCount = Length(a));

  MatchCount := 0;
  ac.Search(s, @FirstMatch);
  AssertTrue(MatchCount = 1);

  for I := 0 to High(a) do
    ss.Add(Utf8ToLower(a[I]));
  AssertTrue(ss.Count = Length(a));
  ac.Search(s, @TestMatch);
  AssertTrue(ss.IsEmpty);
end;

procedure TACSearchFsmTest.TestSearchNestedNfaCI;
var
  MatchCount: Integer = 0;
  function NestMatch(const m: TMatch): Boolean;
  begin
    Inc(MatchCount);
    Result := True;
  end;
  function FirstMatch(const m: TMatch): Boolean;
  begin
    Inc(MatchCount);
    Result := False;
  end;
var
  s: string;
  ss: TStrSet;
  function TestMatch(const m: TMatch): Boolean;
  begin
    ss.Remove(Copy(s, m.Offset, m.Length));
    Result := True;
  end;
var
  ac: IACSearchFsmUtf8;
  a: TStringArray;
  I: Integer;
begin
  a := ['HER', 'İS', 'HİS', 'SHE', 'Hİ', 'HE', 'İ', 'HERO'];
  ac := CreateAcFsmUtf8(a, True, True);
  s := 'hishero';

  ac.Search(s, @NestMatch);
  AssertTrue(MatchCount = Length(a));

  MatchCount := 0;
  ac.Search(s, @FirstMatch);
  AssertTrue(MatchCount = 1);

  for I := 0 to High(a) do
    ss.Add(Utf8ToLower(a[I]));
  AssertTrue(ss.Count = Length(a));
  ac.Search(s, @TestMatch);
  AssertTrue(ss.IsEmpty);
end;

procedure TACSearchFsmTest.TestIndexOfPatternDfa;
var
  ac: IACSearchFsmUtf8;
  a: TStringArray;
  I: SizeInt;
  s: string;
begin
  a := ['не','один','дина','вы','на'];
  ac := CreateAcFsmUtf8(a);
  s := '';
  AssertTrue(ac.IndexOfPattern(s) = -1);
  s := 'оди';
  AssertTrue(ac.IndexOfPattern(s) = -1);
  for I := 0 to High(a) do
    AssertTrue(ac.IndexOfPattern(a[I]) = I);
  s := 'неодинаковый';
  for I := 0 to High(a) do
    begin
      AssertTrue(ac.IndexOfPattern(s, Pos(a[I], s), Length(a[I])) = I);
      AssertTrue(ac.IndexOfPattern(s, Pos(a[I], s), Length(a[I])-1) = -1);
    end;

  a := ['aa','baa','bcaa'];
  ac := CreateAcFsmUtf8(a);
  AssertTrue(ac.IndexOfPattern('baaa') = -1);
end;

procedure TACSearchFsmTest.TestIndexOfPatternNfa;
var
  ac: IACSearchFsmUtf8;
  a: TStringArray;
  I: SizeInt;
  s: string;
begin
  a := ['не','один','дина','вы','на'];
  ac := CreateAcFsmUtf8(a, False, True);
  s := '';
  AssertTrue(ac.IndexOfPattern(s) = -1);
  s := 'оди';
  AssertTrue(ac.IndexOfPattern(s) = -1);
  for I := 0 to High(a) do
    AssertTrue(ac.IndexOfPattern(a[I]) = I);
  s := 'неодинаковый';
  for I := 0 to High(a) do
    begin
      AssertTrue(ac.IndexOfPattern(s, Pos(a[I], s), Length(a[I])) = I);
      AssertTrue(ac.IndexOfPattern(s, Pos(a[I], s), Length(a[I])-1) = -1);
    end;

  a := ['aa','baa','bcaa'];
  ac := CreateAcFsmUtf8(a, False, True);
  AssertTrue(ac.IndexOfPattern('baaa') = -1);
end;

procedure TACSearchFsmTest.TestIndexOfPatternDfaCI;
var
  ac: IACSearchFsmUtf8;
  a, b: TStringArray;
  I: SizeInt;
  s: string;
begin
  a := ['не','один','дина','вы','на'];
  b := ['НЕ','ОДИН','ДИНА','ВЫ','НА'];
  ac := CreateAcFsmUtf8(a, True);
  s := '';
  AssertTrue(ac.IndexOfPattern(s) = -1);
  s := 'оди';
  AssertTrue(ac.IndexOfPattern(s) = -1);
  for I := 0 to High(b) do
    AssertTrue(ac.IndexOfPattern(b[I]) = I);
  s := 'НЕОДИНАКОВЫЙ';
  for I := 0 to High(b) do
    begin
      AssertTrue(ac.IndexOfPattern(s, Pos(b[I], s), Length(b[I])) = I);
      AssertTrue(ac.IndexOfPattern(s, Pos(b[I], s), Length(b[I])-1) = -1);
    end;
  a := ['aa','baa','bcaa'];
  ac := CreateAcFsmUtf8(a);
  AssertTrue(ac.IndexOfPattern('BAAA') = -1);
end;

procedure TACSearchFsmTest.TestIndexOfPatternNfaCI;
var
  ac: IACSearchFsmUtf8;
  a, b: TStringArray;
  I: SizeInt;
  s: string;
begin
  a := ['не','один','дина','вы','на'];
  b := ['НЕ','ОДИН','ДИНА','ВЫ','НА'];
  ac := CreateAcFsmUtf8(a, True, True);
  s := '';
  AssertTrue(ac.IndexOfPattern(s) = -1);
  s := 'оди';
  AssertTrue(ac.IndexOfPattern(s) = -1);
  for I := 0 to High(b) do
    AssertTrue(ac.IndexOfPattern(b[I]) = I);
  s := 'НЕОДИНАКОВЫЙ';
  for I := 0 to High(b) do
    begin
      AssertTrue(ac.IndexOfPattern(s, Pos(b[I], s), Length(b[I])) = I);
      AssertTrue(ac.IndexOfPattern(s, Pos(b[I], s), Length(b[I])-1) = -1);
    end;
  a := ['aa','baa','bcaa'];
  ac := CreateAcFsmUtf8(a);
  AssertTrue(ac.IndexOfPattern('BAAA') = -1);
end;

procedure TACSearchFsmTest.TestFirstMatchDfa;
var
  ac: IACSearchFsmUtf8;
  a: TStringArray;
  s: string;
  m: TMatch;
  Ofs: SizeInt;
begin
  a := ['одина','один','одинак','ако','ак','аков','овы','ов','овый','ков','ко','ковый'];
  ac := CreateAcFsmUtf8(a);
  s := 'НЕОДИНАКОВЫЙ';
  m := ac.FirstMatch(s);
  AssertTrue((m.Offset = 0) and (m.Length = 0) and (m.Index = -1));
  s := 'неодинаковый';
  m := ac.FirstMatch(s);
  AssertTrue((m.Index = 1) and (Copy(s, m.Offset, m.Length) = a[1]));
  m := ac.FirstMatch(s, smmDefault, m.Offset + m.Length);
  AssertTrue((m.Index = 4) and (Copy(s, m.Offset, m.Length) = a[4]));

  m := ac.FirstMatch(s, smmNonOverlapping);
  AssertTrue((m.Index = 1) and (Copy(s, m.Offset, m.Length) = a[1]));
  Ofs := m.Offset + m.Length;
  m := ac.FirstMatch(s, smmNonOverlapping, Ofs);
  AssertTrue((m.Index = 4) and (Copy(s, m.Offset, m.Length) = a[4]));
  m := ac.FirstMatch(s, smmLeftmostFirst, Ofs);
  AssertTrue((m.Index = 3) and (Copy(s, m.Offset, m.Length) = a[3]));
  m := ac.FirstMatch(s, smmLeftmostLongest, Ofs);
  AssertTrue((m.Index = 5) and (Copy(s, m.Offset, m.Length) = a[5]));
  m := ac.FirstMatch(s, smmLeftmostShortest, Ofs);
  AssertTrue((m.Index = 4) and (Copy(s, m.Offset, m.Length) = a[4]));

  m := ac.FirstMatch(s, smmLeftmostFirst);
  AssertTrue((m.Index = 0) and (Copy(s, m.Offset, m.Length) = a[0]));
  Ofs := m.Offset + m.Length;
  m := ac.FirstMatch(s, smmNonOverlapping, Ofs);
  AssertTrue((m.Index = 10) and (Copy(s, m.Offset, m.Length) = a[10]));
  m := ac.FirstMatch(s, smmLeftmostFirst, Ofs);
  AssertTrue((m.Index = 9) and (Copy(s, m.Offset, m.Length) = a[9]));
  m := ac.FirstMatch(s, smmLeftmostLongest, Ofs);
  AssertTrue((m.Index = 11) and (Copy(s, m.Offset, m.Length) = a[11]));
  m := ac.FirstMatch(s, smmLeftmostShortest, Ofs);
  AssertTrue((m.Index = 10) and (Copy(s, m.Offset, m.Length) = a[10]));

  m := ac.FirstMatch(s, smmLeftmostLongest);
  AssertTrue((m.Index = 2) and (Copy(s, m.Offset, m.Length) = a[2]));
  Ofs := m.Offset + m.Length;
  m := ac.FirstMatch(s, smmNonOverlapping, Ofs);
  AssertTrue((m.Index = 7) and (Copy(s, m.Offset, m.Length) = a[7]));
  m := ac.FirstMatch(s, smmLeftmostFirst, Ofs);
  AssertTrue((m.Index = 6) and (Copy(s, m.Offset, m.Length) = a[6]));
  m := ac.FirstMatch(s, smmLeftmostLongest, Ofs);
  AssertTrue((m.Index = 8) and (Copy(s, m.Offset, m.Length) = a[8]));
  m := ac.FirstMatch(s, smmLeftmostShortest, Ofs);
  AssertTrue((m.Index = 7) and (Copy(s, m.Offset, m.Length) = a[7]));

  m := ac.FirstMatch(s, smmLeftmostShortest);
  AssertTrue((m.Index = 1) and (Copy(s, m.Offset, m.Length) = a[1]));
  Ofs := m.Offset + m.Length;
  m := ac.FirstMatch(s, smmNonOverlapping, Ofs);
  AssertTrue((m.Index = 4) and (Copy(s, m.Offset, m.Length) = a[4]));
  m := ac.FirstMatch(s, smmLeftmostFirst, Ofs);
  AssertTrue((m.Index = 3) and (Copy(s, m.Offset, m.Length) = a[3]));
  m := ac.FirstMatch(s, smmLeftmostLongest, Ofs);
  AssertTrue((m.Index = 5) and (Copy(s, m.Offset, m.Length) = a[5]));
  m := ac.FirstMatch(s, smmLeftmostShortest, Ofs);
  AssertTrue((m.Index = 4) and (Copy(s, m.Offset, m.Length) = a[4]));
end;

procedure TACSearchFsmTest.TestFirstMatchDfaOww;
var
  ac: IACSearchFsmUtf8;
  a: TStringArray;
  s: string;
  m: TMatch;
  Ofs: SizeInt;
begin
  a := [
    'считает','читается','первый раз','первый','первый раз не','раз не','раз','считается',
    'раз не считается','не считается', 'не'
  ];
  ac := CreateAcFsmUtf8(a);
  ac.OnlyWholeWords := True;
  s := 'первый раз не считается';

  m := ac.FirstMatch(s);
  AssertTrue((m.Index = 3) and (Copy(s, m.Offset, m.Length) = a[3]));
  m := ac.FirstMatch(s, smmDefault, m.Offset + m.Length);
  AssertTrue((m.Index = 6) and (Copy(s, m.Offset, m.Length) = a[6]));
  m := ac.FirstMatch(s, smmDefault, m.Offset + m.Length);
  AssertTrue((m.Index = 10) and (Copy(s, m.Offset, m.Length) = a[10]));

  m := ac.FirstMatch(s, smmNonOverlapping);
  AssertTrue((m.Index = 3) and (Copy(s, m.Offset, m.Length) = a[3]));
  Ofs := m.Offset + m.Length;
  m := ac.FirstMatch(s, smmNonOverlapping, Ofs);
  AssertTrue((m.Index = 6) and (Copy(s, m.Offset, m.Length) = a[6]));
  m := ac.FirstMatch(s, smmLeftmostFirst, Ofs);
  AssertTrue((m.Index = 5) and (Copy(s, m.Offset, m.Length) = a[5]));
  m := ac.FirstMatch(s, smmLeftmostLongest, Ofs);
  AssertTrue((m.Index = 8) and (Copy(s, m.Offset, m.Length) = a[8]));
  m := ac.FirstMatch(s, smmLeftmostShortest, Ofs);
  AssertTrue((m.Index = 6) and (Copy(s, m.Offset, m.Length) = a[6]));

  m := ac.FirstMatch(s, smmLeftmostFirst);
  AssertTrue((m.Index = 2) and (Copy(s, m.Offset, m.Length) = a[2]));
  Ofs := m.Offset + m.Length;
  m := ac.FirstMatch(s, smmNonOverlapping, Ofs);
  AssertTrue((m.Index = 10) and (Copy(s, m.Offset, m.Length) = a[10]));
  m := ac.FirstMatch(s, smmLeftmostFirst, Ofs);
  AssertTrue((m.Index = 9) and (Copy(s, m.Offset, m.Length) = a[9]));
  m := ac.FirstMatch(s, smmLeftmostLongest, Ofs);
  AssertTrue((m.Index = 9) and (Copy(s, m.Offset, m.Length) = a[9]));
  m := ac.FirstMatch(s, smmLeftmostShortest, Ofs);
  AssertTrue((m.Index = 10) and (Copy(s, m.Offset, m.Length) = a[10]));

  m := ac.FirstMatch(s, smmLeftmostLongest);
  AssertTrue((m.Index = 4) and (Copy(s, m.Offset, m.Length) = a[4]));
  Ofs := m.Offset + m.Length;
  m := ac.FirstMatch(s, smmNonOverlapping, Ofs);
  AssertTrue((m.Index = 7) and (Copy(s, m.Offset, m.Length) = a[7]));
  m := ac.FirstMatch(s, smmLeftmostFirst, Ofs);
  AssertTrue((m.Index = 7) and (Copy(s, m.Offset, m.Length) = a[7]));
  m := ac.FirstMatch(s, smmLeftmostLongest, Ofs);
  AssertTrue((m.Index = 7) and (Copy(s, m.Offset, m.Length) = a[7]));
  m := ac.FirstMatch(s, smmLeftmostShortest, Ofs);
  AssertTrue((m.Index = 7) and (Copy(s, m.Offset, m.Length) = a[7]));

  m := ac.FirstMatch(s, smmLeftmostShortest);
  AssertTrue((m.Index = 3) and (Copy(s, m.Offset, m.Length) = a[3]));
  Ofs := m.Offset + m.Length;
  m := ac.FirstMatch(s, smmNonOverlapping, Ofs);
  AssertTrue((m.Index = 6) and (Copy(s, m.Offset, m.Length) = a[6]));
  m := ac.FirstMatch(s, smmLeftmostFirst, Ofs);
  AssertTrue((m.Index = 5) and (Copy(s, m.Offset, m.Length) = a[5]));
  m := ac.FirstMatch(s, smmLeftmostLongest, Ofs);
  AssertTrue((m.Index = 8) and (Copy(s, m.Offset, m.Length) = a[8]));
  m := ac.FirstMatch(s, smmLeftmostShortest, Ofs);
  AssertTrue((m.Index = 6) and (Copy(s, m.Offset, m.Length) = a[6]));
end;

procedure TACSearchFsmTest.TestFirstMatchDfaCI;
var
  ac: IACSearchFsmUtf8;
  a: TStringArray;
  s: string;
  m: TMatch;
  Ofs: SizeInt;
begin
  a := ['одина','один','одинак','ако','ак','аков','овы','ов','овый','ков','ко','ковый'];
  ac := CreateAcFsmUtf8(a, True);
  s := 'НЕОДИНАКОВЫЙ';
  m := ac.FirstMatch(s);
  AssertTrue((m.Index = 1) and AnsiSameText(Copy(s, m.Offset, m.Length), a[1]));
  m := ac.FirstMatch(s, smmDefault, m.Offset + m.Length);
  AssertTrue((m.Index = 4) and AnsiSameText(Copy(s, m.Offset, m.Length), a[4]));

  m := ac.FirstMatch(s, smmNonOverlapping);
  AssertTrue((m.Index = 1) and AnsiSameText(Copy(s, m.Offset, m.Length), a[1]));
  Ofs := m.Offset + m.Length;
  m := ac.FirstMatch(s, smmNonOverlapping, Ofs);
  AssertTrue((m.Index = 4) and AnsiSameText(Copy(s, m.Offset, m.Length), a[4]));
  m := ac.FirstMatch(s, smmLeftmostFirst, Ofs);
  AssertTrue((m.Index = 3) and AnsiSameText(Copy(s, m.Offset, m.Length), a[3]));
  m := ac.FirstMatch(s, smmLeftmostLongest, Ofs);
  AssertTrue((m.Index = 5) and AnsiSameText(Copy(s, m.Offset, m.Length), a[5]));
  m := ac.FirstMatch(s, smmLeftmostShortest, Ofs);
  AssertTrue((m.Index = 4) and AnsiSameText(Copy(s, m.Offset, m.Length), a[4]));

  m := ac.FirstMatch(s, smmLeftmostFirst);
  AssertTrue((m.Index = 0) and AnsiSameText(Copy(s, m.Offset, m.Length), a[0]));
  Ofs := m.Offset + m.Length;
  m := ac.FirstMatch(s, smmNonOverlapping, Ofs);
  AssertTrue((m.Index = 10) and AnsiSameText(Copy(s, m.Offset, m.Length), a[10]));
  m := ac.FirstMatch(s, smmLeftmostFirst, Ofs);
  AssertTrue((m.Index = 9) and AnsiSameText(Copy(s, m.Offset, m.Length), a[9]));
  m := ac.FirstMatch(s, smmLeftmostLongest, Ofs);
  AssertTrue((m.Index = 11) and AnsiSameText(Copy(s, m.Offset, m.Length), a[11]));
  m := ac.FirstMatch(s, smmLeftmostShortest, Ofs);
  AssertTrue((m.Index = 10) and AnsiSameText(Copy(s, m.Offset, m.Length), a[10]));

  m := ac.FirstMatch(s, smmLeftmostLongest);
  AssertTrue((m.Index = 2) and AnsiSameText(Copy(s, m.Offset, m.Length), a[2]));
  Ofs := m.Offset + m.Length;
  m := ac.FirstMatch(s, smmNonOverlapping, Ofs);
  AssertTrue((m.Index = 7) and AnsiSameText(Copy(s, m.Offset, m.Length), a[7]));
  m := ac.FirstMatch(s, smmLeftmostFirst, Ofs);
  AssertTrue((m.Index = 6) and AnsiSameText(Copy(s, m.Offset, m.Length), a[6]));
  m := ac.FirstMatch(s, smmLeftmostLongest, Ofs);
  AssertTrue((m.Index = 8) and AnsiSameText(Copy(s, m.Offset, m.Length), a[8]));
  m := ac.FirstMatch(s, smmLeftmostShortest, Ofs);
  AssertTrue((m.Index = 7) and AnsiSameText(Copy(s, m.Offset, m.Length), a[7]));

  m := ac.FirstMatch(s, smmLeftmostShortest);
  AssertTrue((m.Index = 1) and AnsiSameText(Copy(s, m.Offset, m.Length), a[1]));
  Ofs := m.Offset + m.Length;
  m := ac.FirstMatch(s, smmNonOverlapping, Ofs);
  AssertTrue((m.Index = 4) and AnsiSameText(Copy(s, m.Offset, m.Length), a[4]));
  m := ac.FirstMatch(s, smmLeftmostFirst, Ofs);
  AssertTrue((m.Index = 3) and AnsiSameText(Copy(s, m.Offset, m.Length), a[3]));
  m := ac.FirstMatch(s, smmLeftmostLongest, Ofs);
  AssertTrue((m.Index = 5) and AnsiSameText(Copy(s, m.Offset, m.Length), a[5]));
  m := ac.FirstMatch(s, smmLeftmostShortest, Ofs);
  AssertTrue((m.Index = 4) and AnsiSameText(Copy(s, m.Offset, m.Length), a[4]));
end;

procedure TACSearchFsmTest.TestFirstMatchDfaCIOww;
var
  ac: IACSearchFsmUtf8;
  a: TStringArray;
  s: string;
  m: TMatch;
  Ofs: SizeInt;
begin
  a := [
    'считает','читается','первый раз','первый','первый раз не','раз не','раз','считается',
    'раз не считается','не считается', 'не'
  ];
  ac := CreateAcFsmUtf8(a, True);
  ac.OnlyWholeWords := True;
  s := 'ПЕРВЫЙ РАЗ НЕ СЧИТАЕТСЯ';

  m := ac.FirstMatch(s);
  AssertTrue((m.Index = 3) and AnsiSameText(Copy(s, m.Offset, m.Length), a[3]));
  m := ac.FirstMatch(s, smmDefault, m.Offset + m.Length);
  AssertTrue((m.Index = 6) and AnsiSameText(Copy(s, m.Offset, m.Length), a[6]));
  m := ac.FirstMatch(s, smmDefault, m.Offset + m.Length);
  AssertTrue((m.Index = 10) and AnsiSameText(Copy(s, m.Offset, m.Length), a[10]));

  m := ac.FirstMatch(s, smmNonOverlapping);
  AssertTrue((m.Index = 3) and AnsiSameText(Copy(s, m.Offset, m.Length), a[3]));
  Ofs := m.Offset + m.Length;
  m := ac.FirstMatch(s, smmNonOverlapping, Ofs);
  AssertTrue((m.Index = 6) and AnsiSameText(Copy(s, m.Offset, m.Length), a[6]));
  m := ac.FirstMatch(s, smmLeftmostFirst, Ofs);
  AssertTrue((m.Index = 5) and AnsiSameText(Copy(s, m.Offset, m.Length), a[5]));
  m := ac.FirstMatch(s, smmLeftmostLongest, Ofs);
  AssertTrue((m.Index = 8) and AnsiSameText(Copy(s, m.Offset, m.Length), a[8]));
  m := ac.FirstMatch(s, smmLeftmostShortest, Ofs);
  AssertTrue((m.Index = 6) and AnsiSameText(Copy(s, m.Offset, m.Length), a[6]));

  m := ac.FirstMatch(s, smmLeftmostFirst);
  AssertTrue((m.Index = 2) and AnsiSameText(Copy(s, m.Offset, m.Length), a[2]));
  Ofs := m.Offset + m.Length;
  m := ac.FirstMatch(s, smmNonOverlapping, Ofs);
  AssertTrue((m.Index = 10) and AnsiSameText(Copy(s, m.Offset, m.Length), a[10]));
  m := ac.FirstMatch(s, smmLeftmostFirst, Ofs);
  AssertTrue((m.Index = 9) and AnsiSameText(Copy(s, m.Offset, m.Length), a[9]));
  m := ac.FirstMatch(s, smmLeftmostLongest, Ofs);
  AssertTrue((m.Index = 9) and AnsiSameText(Copy(s, m.Offset, m.Length), a[9]));
  m := ac.FirstMatch(s, smmLeftmostShortest, Ofs);
  AssertTrue((m.Index = 10) and AnsiSameText(Copy(s, m.Offset, m.Length), a[10]));

  m := ac.FirstMatch(s, smmLeftmostLongest);
  AssertTrue((m.Index = 4) and AnsiSameText(Copy(s, m.Offset, m.Length), a[4]));
  Ofs := m.Offset + m.Length;
  m := ac.FirstMatch(s, smmNonOverlapping, Ofs);
  AssertTrue((m.Index = 7) and AnsiSameText(Copy(s, m.Offset, m.Length), a[7]));
  m := ac.FirstMatch(s, smmLeftmostFirst, Ofs);
  AssertTrue((m.Index = 7) and AnsiSameText(Copy(s, m.Offset, m.Length), a[7]));
  m := ac.FirstMatch(s, smmLeftmostLongest, Ofs);
  AssertTrue((m.Index = 7) and AnsiSameText(Copy(s, m.Offset, m.Length), a[7]));
  m := ac.FirstMatch(s, smmLeftmostShortest, Ofs);
  AssertTrue((m.Index = 7) and AnsiSameText(Copy(s, m.Offset, m.Length), a[7]));

  m := ac.FirstMatch(s, smmLeftmostShortest);
  AssertTrue((m.Index = 3) and AnsiSameText(Copy(s, m.Offset, m.Length), a[3]));
  Ofs := m.Offset + m.Length;
  m := ac.FirstMatch(s, smmNonOverlapping, Ofs);
  AssertTrue((m.Index = 6) and AnsiSameText(Copy(s, m.Offset, m.Length), a[6]));
  m := ac.FirstMatch(s, smmLeftmostFirst, Ofs);
  AssertTrue((m.Index = 5) and AnsiSameText(Copy(s, m.Offset, m.Length), a[5]));
  m := ac.FirstMatch(s, smmLeftmostLongest, Ofs);
  AssertTrue((m.Index = 8) and AnsiSameText(Copy(s, m.Offset, m.Length), a[8]));
  m := ac.FirstMatch(s, smmLeftmostShortest, Ofs);
  AssertTrue((m.Index = 6) and AnsiSameText(Copy(s, m.Offset, m.Length), a[6]));
end;

procedure TACSearchFsmTest.TestFirstMatchNfa;
var
  ac: IACSearchFsmUtf8;
  a: TStringArray;
  s: string;
  m: TMatch;
  Ofs: SizeInt;
begin
  a := ['одина','один','одинак','ако','ак','аков','овы','ов','овый','ков','ко','ковый'];
  ac := CreateAcFsmUtf8(a, False, True);
  s := 'НЕОДИНАКОВЫЙ';
  m := ac.FirstMatch(s);
  AssertTrue((m.Offset = 0) and (m.Length = 0) and (m.Index = -1));
  s := 'неодинаковый';
  m := ac.FirstMatch(s);
  AssertTrue((m.Index = 1) and (Copy(s, m.Offset, m.Length) = a[1]));
  m := ac.FirstMatch(s, smmDefault, m.Offset + m.Length);
  AssertTrue((m.Index = 4) and (Copy(s, m.Offset, m.Length) = a[4]));

  m := ac.FirstMatch(s, smmNonOverlapping);
  AssertTrue((m.Index = 1) and (Copy(s, m.Offset, m.Length) = a[1]));
  Ofs := m.Offset + m.Length;
  m := ac.FirstMatch(s, smmNonOverlapping, Ofs);
  AssertTrue((m.Index = 4) and (Copy(s, m.Offset, m.Length) = a[4]));
  m := ac.FirstMatch(s, smmLeftmostFirst, Ofs);
  AssertTrue((m.Index = 3) and (Copy(s, m.Offset, m.Length) = a[3]));
  m := ac.FirstMatch(s, smmLeftmostLongest, Ofs);
  AssertTrue((m.Index = 5) and (Copy(s, m.Offset, m.Length) = a[5]));
  m := ac.FirstMatch(s, smmLeftmostShortest, Ofs);
  AssertTrue((m.Index = 4) and (Copy(s, m.Offset, m.Length) = a[4]));

  m := ac.FirstMatch(s, smmLeftmostFirst);
  AssertTrue((m.Index = 0) and (Copy(s, m.Offset, m.Length) = a[0]));
  Ofs := m.Offset + m.Length;
  m := ac.FirstMatch(s, smmNonOverlapping, Ofs);
  AssertTrue((m.Index = 10) and (Copy(s, m.Offset, m.Length) = a[10]));
  m := ac.FirstMatch(s, smmLeftmostFirst, Ofs);
  AssertTrue((m.Index = 9) and (Copy(s, m.Offset, m.Length) = a[9]));
  m := ac.FirstMatch(s, smmLeftmostLongest, Ofs);
  AssertTrue((m.Index = 11) and (Copy(s, m.Offset, m.Length) = a[11]));
  m := ac.FirstMatch(s, smmLeftmostShortest, Ofs);
  AssertTrue((m.Index = 10) and (Copy(s, m.Offset, m.Length) = a[10]));

  m := ac.FirstMatch(s, smmLeftmostLongest);
  AssertTrue((m.Index = 2) and (Copy(s, m.Offset, m.Length) = a[2]));
  Ofs := m.Offset + m.Length;
  m := ac.FirstMatch(s, smmNonOverlapping, Ofs);
  AssertTrue((m.Index = 7) and (Copy(s, m.Offset, m.Length) = a[7]));
  m := ac.FirstMatch(s, smmLeftmostFirst, Ofs);
  AssertTrue((m.Index = 6) and (Copy(s, m.Offset, m.Length) = a[6]));
  m := ac.FirstMatch(s, smmLeftmostLongest, Ofs);
  AssertTrue((m.Index = 8) and (Copy(s, m.Offset, m.Length) = a[8]));
  m := ac.FirstMatch(s, smmLeftmostShortest, Ofs);
  AssertTrue((m.Index = 7) and (Copy(s, m.Offset, m.Length) = a[7]));

  m := ac.FirstMatch(s, smmLeftmostShortest);
  AssertTrue((m.Index = 1) and (Copy(s, m.Offset, m.Length) = a[1]));
  Ofs := m.Offset + m.Length;
  m := ac.FirstMatch(s, smmNonOverlapping, Ofs);
  AssertTrue((m.Index = 4) and (Copy(s, m.Offset, m.Length) = a[4]));
  m := ac.FirstMatch(s, smmLeftmostFirst, Ofs);
  AssertTrue((m.Index = 3) and (Copy(s, m.Offset, m.Length) = a[3]));
  m := ac.FirstMatch(s, smmLeftmostLongest, Ofs);
  AssertTrue((m.Index = 5) and (Copy(s, m.Offset, m.Length) = a[5]));
  m := ac.FirstMatch(s, smmLeftmostShortest, Ofs);
  AssertTrue((m.Index = 4) and (Copy(s, m.Offset, m.Length) = a[4]));
end;

procedure TACSearchFsmTest.TestFirstMatchNfaOww;
var
  ac: IACSearchFsmUtf8;
  a: TStringArray;
  s: string;
  m: TMatch;
  Ofs: SizeInt;
begin
  a := [
    'считает','читается','первый раз','первый','первый раз не','раз не','раз','считается',
    'раз не считается','не считается', 'не'
  ];
  ac := CreateAcFsmUtf8(a, False, True);
  ac.OnlyWholeWords := True;
  s := 'первый раз не считается';

  m := ac.FirstMatch(s);
  AssertTrue((m.Index = 3) and (Copy(s, m.Offset, m.Length) = a[3]));
  m := ac.FirstMatch(s, smmDefault, m.Offset + m.Length);
  AssertTrue((m.Index = 6) and (Copy(s, m.Offset, m.Length) = a[6]));
  m := ac.FirstMatch(s, smmDefault, m.Offset + m.Length);
  AssertTrue((m.Index = 10) and (Copy(s, m.Offset, m.Length) = a[10]));

  m := ac.FirstMatch(s, smmNonOverlapping);
  AssertTrue((m.Index = 3) and (Copy(s, m.Offset, m.Length) = a[3]));
  Ofs := m.Offset + m.Length;
  m := ac.FirstMatch(s, smmNonOverlapping, Ofs);
  AssertTrue((m.Index = 6) and (Copy(s, m.Offset, m.Length) = a[6]));
  m := ac.FirstMatch(s, smmLeftmostFirst, Ofs);
  AssertTrue((m.Index = 5) and (Copy(s, m.Offset, m.Length) = a[5]));
  m := ac.FirstMatch(s, smmLeftmostLongest, Ofs);
  AssertTrue((m.Index = 8) and (Copy(s, m.Offset, m.Length) = a[8]));
  m := ac.FirstMatch(s, smmLeftmostShortest, Ofs);
  AssertTrue((m.Index = 6) and (Copy(s, m.Offset, m.Length) = a[6]));

  m := ac.FirstMatch(s, smmLeftmostFirst);
  AssertTrue((m.Index = 2) and (Copy(s, m.Offset, m.Length) = a[2]));
  Ofs := m.Offset + m.Length;
  m := ac.FirstMatch(s, smmNonOverlapping, Ofs);
  AssertTrue((m.Index = 10) and (Copy(s, m.Offset, m.Length) = a[10]));
  m := ac.FirstMatch(s, smmLeftmostFirst, Ofs);
  AssertTrue((m.Index = 9) and (Copy(s, m.Offset, m.Length) = a[9]));
  m := ac.FirstMatch(s, smmLeftmostLongest, Ofs);
  AssertTrue((m.Index = 9) and (Copy(s, m.Offset, m.Length) = a[9]));
  m := ac.FirstMatch(s, smmLeftmostShortest, Ofs);
  AssertTrue((m.Index = 10) and (Copy(s, m.Offset, m.Length) = a[10]));

  m := ac.FirstMatch(s, smmLeftmostLongest);
  AssertTrue((m.Index = 4) and (Copy(s, m.Offset, m.Length) = a[4]));
  Ofs := m.Offset + m.Length;
  m := ac.FirstMatch(s, smmNonOverlapping, Ofs);
  AssertTrue((m.Index = 7) and (Copy(s, m.Offset, m.Length) = a[7]));
  m := ac.FirstMatch(s, smmLeftmostFirst, Ofs);
  AssertTrue((m.Index = 7) and (Copy(s, m.Offset, m.Length) = a[7]));
  m := ac.FirstMatch(s, smmLeftmostLongest, Ofs);
  AssertTrue((m.Index = 7) and (Copy(s, m.Offset, m.Length) = a[7]));
  m := ac.FirstMatch(s, smmLeftmostShortest, Ofs);
  AssertTrue((m.Index = 7) and (Copy(s, m.Offset, m.Length) = a[7]));

  m := ac.FirstMatch(s, smmLeftmostShortest);
  AssertTrue((m.Index = 3) and (Copy(s, m.Offset, m.Length) = a[3]));
  Ofs := m.Offset + m.Length;
  m := ac.FirstMatch(s, smmNonOverlapping, Ofs);
  AssertTrue((m.Index = 6) and (Copy(s, m.Offset, m.Length) = a[6]));
  m := ac.FirstMatch(s, smmLeftmostFirst, Ofs);
  AssertTrue((m.Index = 5) and (Copy(s, m.Offset, m.Length) = a[5]));
  m := ac.FirstMatch(s, smmLeftmostLongest, Ofs);
  AssertTrue((m.Index = 8) and (Copy(s, m.Offset, m.Length) = a[8]));
  m := ac.FirstMatch(s, smmLeftmostShortest, Ofs);
  AssertTrue((m.Index = 6) and (Copy(s, m.Offset, m.Length) = a[6]));
end;

procedure TACSearchFsmTest.TestFirstMatchNfaCI;
var
  ac: IACSearchFsmUtf8;
  a: TStringArray;
  s: string;
  m: TMatch;
  Ofs: SizeInt;
begin
  a := ['одина','один','одинак','ако','ак','аков','овы','ов','овый','ков','ко','ковый'];
  ac := CreateAcFsmUtf8(a, True, True);
  s := 'НЕОДИНАКОВЫЙ';
  m := ac.FirstMatch(s);
  AssertTrue((m.Index = 1) and AnsiSameText(Copy(s, m.Offset, m.Length), a[1]));
  m := ac.FirstMatch(s, smmDefault, m.Offset + m.Length);
  AssertTrue((m.Index = 4) and AnsiSameText(Copy(s, m.Offset, m.Length), a[4]));

  m := ac.FirstMatch(s, smmNonOverlapping);
  AssertTrue((m.Index = 1) and AnsiSameText(Copy(s, m.Offset, m.Length), a[1]));
  Ofs := m.Offset + m.Length;
  m := ac.FirstMatch(s, smmNonOverlapping, Ofs);
  AssertTrue((m.Index = 4) and AnsiSameText(Copy(s, m.Offset, m.Length), a[4]));
  m := ac.FirstMatch(s, smmLeftmostFirst, Ofs);
  AssertTrue((m.Index = 3) and AnsiSameText(Copy(s, m.Offset, m.Length), a[3]));
  m := ac.FirstMatch(s, smmLeftmostLongest, Ofs);
  AssertTrue((m.Index = 5) and AnsiSameText(Copy(s, m.Offset, m.Length), a[5]));
  m := ac.FirstMatch(s, smmLeftmostShortest, Ofs);
  AssertTrue((m.Index = 4) and AnsiSameText(Copy(s, m.Offset, m.Length), a[4]));

  m := ac.FirstMatch(s, smmLeftmostFirst);
  AssertTrue((m.Index = 0) and AnsiSameText(Copy(s, m.Offset, m.Length), a[0]));
  Ofs := m.Offset + m.Length;
  m := ac.FirstMatch(s, smmNonOverlapping, Ofs);
  AssertTrue((m.Index = 10) and AnsiSameText(Copy(s, m.Offset, m.Length), a[10]));
  m := ac.FirstMatch(s, smmLeftmostFirst, Ofs);
  AssertTrue((m.Index = 9) and AnsiSameText(Copy(s, m.Offset, m.Length), a[9]));
  m := ac.FirstMatch(s, smmLeftmostLongest, Ofs);
  AssertTrue((m.Index = 11) and AnsiSameText(Copy(s, m.Offset, m.Length), a[11]));
  m := ac.FirstMatch(s, smmLeftmostShortest, Ofs);
  AssertTrue((m.Index = 10) and AnsiSameText(Copy(s, m.Offset, m.Length), a[10]));

  m := ac.FirstMatch(s, smmLeftmostLongest);
  AssertTrue((m.Index = 2) and AnsiSameText(Copy(s, m.Offset, m.Length), a[2]));
  Ofs := m.Offset + m.Length;
  m := ac.FirstMatch(s, smmNonOverlapping, Ofs);
  AssertTrue((m.Index = 7) and AnsiSameText(Copy(s, m.Offset, m.Length), a[7]));
  m := ac.FirstMatch(s, smmLeftmostFirst, Ofs);
  AssertTrue((m.Index = 6) and AnsiSameText(Copy(s, m.Offset, m.Length), a[6]));
  m := ac.FirstMatch(s, smmLeftmostLongest, Ofs);
  AssertTrue((m.Index = 8) and AnsiSameText(Copy(s, m.Offset, m.Length), a[8]));
  m := ac.FirstMatch(s, smmLeftmostShortest, Ofs);
  AssertTrue((m.Index = 7) and AnsiSameText(Copy(s, m.Offset, m.Length), a[7]));

  m := ac.FirstMatch(s, smmLeftmostShortest);
  AssertTrue((m.Index = 1) and AnsiSameText(Copy(s, m.Offset, m.Length), a[1]));
  Ofs := m.Offset + m.Length;
  m := ac.FirstMatch(s, smmNonOverlapping, Ofs);
  AssertTrue((m.Index = 4) and AnsiSameText(Copy(s, m.Offset, m.Length), a[4]));
  m := ac.FirstMatch(s, smmLeftmostFirst, Ofs);
  AssertTrue((m.Index = 3) and AnsiSameText(Copy(s, m.Offset, m.Length), a[3]));
  m := ac.FirstMatch(s, smmLeftmostLongest, Ofs);
  AssertTrue((m.Index = 5) and AnsiSameText(Copy(s, m.Offset, m.Length), a[5]));
  m := ac.FirstMatch(s, smmLeftmostShortest, Ofs);
  AssertTrue((m.Index = 4) and AnsiSameText(Copy(s, m.Offset, m.Length), a[4]));
end;

procedure TACSearchFsmTest.TestFirstMatchNfaCIOww;
var
  ac: IACSearchFsmUtf8;
  a: TStringArray;
  s: string;
  m: TMatch;
  Ofs: SizeInt;
begin
  a := [
    'считает','читается','первый раз','первый','первый раз не','раз не','раз','считается',
    'раз не считается','не считается', 'не'
  ];
  ac := CreateAcFsmUtf8(a, True, True);
  ac.OnlyWholeWords := True;
  s := 'ПЕРВЫЙ РАЗ НЕ СЧИТАЕТСЯ';

  m := ac.FirstMatch(s);
  AssertTrue((m.Index = 3) and AnsiSameText(Copy(s, m.Offset, m.Length), a[3]));
  m := ac.FirstMatch(s, smmDefault, m.Offset + m.Length);
  AssertTrue((m.Index = 6) and AnsiSameText(Copy(s, m.Offset, m.Length), a[6]));
  m := ac.FirstMatch(s, smmDefault, m.Offset + m.Length);
  AssertTrue((m.Index = 10) and AnsiSameText(Copy(s, m.Offset, m.Length), a[10]));

  m := ac.FirstMatch(s, smmNonOverlapping);
  AssertTrue((m.Index = 3) and AnsiSameText(Copy(s, m.Offset, m.Length), a[3]));
  Ofs := m.Offset + m.Length;
  m := ac.FirstMatch(s, smmNonOverlapping, Ofs);
  AssertTrue((m.Index = 6) and AnsiSameText(Copy(s, m.Offset, m.Length), a[6]));
  m := ac.FirstMatch(s, smmLeftmostFirst, Ofs);
  AssertTrue((m.Index = 5) and AnsiSameText(Copy(s, m.Offset, m.Length), a[5]));
  m := ac.FirstMatch(s, smmLeftmostLongest, Ofs);
  AssertTrue((m.Index = 8) and AnsiSameText(Copy(s, m.Offset, m.Length), a[8]));
  m := ac.FirstMatch(s, smmLeftmostShortest, Ofs);
  AssertTrue((m.Index = 6) and AnsiSameText(Copy(s, m.Offset, m.Length), a[6]));

  m := ac.FirstMatch(s, smmLeftmostFirst);
  AssertTrue((m.Index = 2) and AnsiSameText(Copy(s, m.Offset, m.Length), a[2]));
  Ofs := m.Offset + m.Length;
  m := ac.FirstMatch(s, smmNonOverlapping, Ofs);
  AssertTrue((m.Index = 10) and AnsiSameText(Copy(s, m.Offset, m.Length), a[10]));
  m := ac.FirstMatch(s, smmLeftmostFirst, Ofs);
  AssertTrue((m.Index = 9) and AnsiSameText(Copy(s, m.Offset, m.Length), a[9]));
  m := ac.FirstMatch(s, smmLeftmostLongest, Ofs);
  AssertTrue((m.Index = 9) and AnsiSameText(Copy(s, m.Offset, m.Length), a[9]));
  m := ac.FirstMatch(s, smmLeftmostShortest, Ofs);
  AssertTrue((m.Index = 10) and AnsiSameText(Copy(s, m.Offset, m.Length), a[10]));

  m := ac.FirstMatch(s, smmLeftmostLongest);
  AssertTrue((m.Index = 4) and AnsiSameText(Copy(s, m.Offset, m.Length), a[4]));
  Ofs := m.Offset + m.Length;
  m := ac.FirstMatch(s, smmNonOverlapping, Ofs);
  AssertTrue((m.Index = 7) and AnsiSameText(Copy(s, m.Offset, m.Length), a[7]));
  m := ac.FirstMatch(s, smmLeftmostFirst, Ofs);
  AssertTrue((m.Index = 7) and AnsiSameText(Copy(s, m.Offset, m.Length), a[7]));
  m := ac.FirstMatch(s, smmLeftmostLongest, Ofs);
  AssertTrue((m.Index = 7) and AnsiSameText(Copy(s, m.Offset, m.Length), a[7]));
  m := ac.FirstMatch(s, smmLeftmostShortest, Ofs);
  AssertTrue((m.Index = 7) and AnsiSameText(Copy(s, m.Offset, m.Length), a[7]));

  m := ac.FirstMatch(s, smmLeftmostShortest);
  AssertTrue((m.Index = 3) and AnsiSameText(Copy(s, m.Offset, m.Length), a[3]));
  Ofs := m.Offset + m.Length;
  m := ac.FirstMatch(s, smmNonOverlapping, Ofs);
  AssertTrue((m.Index = 6) and AnsiSameText(Copy(s, m.Offset, m.Length), a[6]));
  m := ac.FirstMatch(s, smmLeftmostFirst, Ofs);
  AssertTrue((m.Index = 5) and AnsiSameText(Copy(s, m.Offset, m.Length), a[5]));
  m := ac.FirstMatch(s, smmLeftmostLongest, Ofs);
  AssertTrue((m.Index = 8) and AnsiSameText(Copy(s, m.Offset, m.Length), a[8]));
  m := ac.FirstMatch(s, smmLeftmostShortest, Ofs);
  AssertTrue((m.Index = 6) and AnsiSameText(Copy(s, m.Offset, m.Length), a[6]));
end;

procedure TACSearchFsmTest.TestFindMatches;
var
  ac: IACSearchFsmUtf8;
  s: string;
  a: TStringArray;
  m: array of TMatch;
begin
  s := '012abcdefgh';
  a := ['0124','234','12345', '1234567','1234','23','890123','6789012', '5678901'];
  ac := CreateAcFsmUtf8(a);
  m := ac.FindMatches(s);
  AssertTrue(m = nil);

  s := '01234567890123456789';
  m := ac.FindMatches(s);
  AssertTrue(Length(m) = 13);

  m := ac.FindMatches(s, smmNonOverlapping);
  AssertTrue(Length(m) = 3);
  AssertTrue(Copy(s, m[0].Offset, m[0].Length) = '23');
  AssertTrue(Copy(s, m[1].Offset, m[1].Length) = '5678901');
  AssertTrue(Copy(s, m[0].Offset, m[2].Length) = '23');

  m := ac.FindMatches(s, smmLeftmostFirst);
  AssertTrue(Length(m) = 2);
  AssertTrue(Copy(s, m[0].Offset, m[0].Length) = '12345');
  AssertTrue(Copy(s, m[1].Offset, m[1].Length) = '6789012');

  m := ac.FindMatches(s, smmLeftmostLongest);
  AssertTrue(Length(m) = 2);
  AssertTrue(Copy(s, m[0].Offset, m[0].Length) = '1234567');
  AssertTrue(Copy(s, m[1].Offset, m[1].Length) = '890123');

  m := ac.FindMatches(s, smmLeftmostShortest);
  AssertTrue(Length(m) = 3);
  AssertTrue(Copy(s, m[0].Offset, m[0].Length) = '1234');
  AssertTrue(Copy(s, m[1].Offset, m[1].Length) = '5678901');
  AssertTrue(Copy(s, m[2].Offset, m[2].Length) = '23');
end;

procedure TACSearchFsmTest.TestFindMatchesOww;
var
  ac: IACSearchFsmUtf8;
  s, s2: string;
  a: TStringArray;
  m: array of TMatch;
  I: Integer;
begin
  a := ['самый','сомнительный','поход','сам','сом','ход'];
  s := 'самый сомнительный поход';

  ac := CreateAcFsmUtf8(a);

  m := ac.FindMatches(s);
  AssertTrue(Length(m) = 6);
  ac.OnlyWholeWords := True;

  m := ac.FindMatches(s);
  AssertTrue(Length(m) = 3);
  for I := 0 to High(m) do
    AssertTrue(m[I].Index = I);

  m := ac.FindMatches(s, smmDefault, 11);
  AssertTrue(Length(m) = 2);
  for I := 0 to High(m) do
    AssertTrue(m[I].Index = I+1);

  s2 := StringReplace(s, ' ', 'о', []);
  m := ac.FindMatches(s2, smmDefault, 13);
  AssertTrue(Length(m) = 1);
  AssertTrue(m[0].Index = 2);

  ac := CreateAcFsmUtf8(a, False, True);

  m := ac.FindMatches(s);
  AssertTrue(Length(m) = 6);

  ac.OnlyWholeWords := True;
  m := ac.FindMatches(s);
  AssertTrue(Length(m) = 3);
  for I := 0 to High(m) do
    AssertTrue(m[I].Index = I);

  m := ac.FindMatches(s, smmDefault, 11);
  AssertTrue(Length(m) = 2);
  for I := 0 to High(m) do
    AssertTrue(m[I].Index = I+1);

  s2 := StringReplace(s, ' ', 'о', []);
  m := ac.FindMatches(s2, smmDefault, 13);
  AssertTrue(Length(m) = 1);
  AssertTrue(m[0].Index = 2);
end;

procedure TACSearchFsmTest.TestContainsMatchDfa;
var
  ac: IACSearchFsmUtf8;
begin
  ac := CreateAcFsmUtf8(['aa','bb']);
  AssertFalse(ac.ContainsMatch('abcde'));
  AssertFalse(ac.ContainsMatch('ababcde'));
  AssertTrue(ac.ContainsMatch('bcdaae'));
  AssertTrue(ac.ContainsMatch('acdbbe'));
end;

procedure TACSearchFsmTest.TestContainsMatchNfa;
var
  ac: IACSearchFsmUtf8;
begin
  ac := CreateAcFsmUtf8(['aa','bb'], False, True);
  AssertFalse(ac.ContainsMatch('abcde'));
  AssertFalse(ac.ContainsMatch('ababcde'));
  AssertTrue(ac.ContainsMatch('bcdaae'));
  AssertTrue(ac.ContainsMatch('acdbbe'));
end;

procedure TACSearchFsmTest.TestContainsMatchDfaCI;
var
  ac: IACSearchFsmUtf8;
begin
  ac := CreateAcFsmUtf8(['aA','Bb'], True);
  AssertFalse(ac.ContainsMatch('abcde'));
  AssertFalse(ac.ContainsMatch('ababcde'));
  AssertTrue(ac.ContainsMatch('bcdaae'));
  AssertTrue(ac.ContainsMatch('acdbbe'));
end;

procedure TACSearchFsmTest.TestContainsMatchNfaCI;
var
  ac: IACSearchFsmUtf8;
begin
  ac := CreateAcFsmUtf8(['aA','Bb'], True, True);
  AssertFalse(ac.ContainsMatch('abcde'));
  AssertFalse(ac.ContainsMatch('ababcde'));
  AssertTrue(ac.ContainsMatch('bcdaae'));
  AssertTrue(ac.ContainsMatch('acdbbe'));
end;

{ TACPersistFsmTest }

procedure TACPersistFsmTest.TestCreation;
var
  a: IACPersistFsmUtf8;
begin
  a := NewAcFsmUtf8([]);
  AssertTrue(a <> nil);
  AssertTrue(a.PatternListSize = 0);
  AssertTrue(a.GetPatternList = nil);
  AssertFalse(a.Fsm.CaseInsensitive);
  AssertTrue(a.Fsm.StateCount = 1);
  AssertTrue(a.Fsm.PatternCount = 0);
  AssertTrue(a.Fsm.AlphabetSize = 0);

  a := NewAcFsmUtf8([], True);
  AssertTrue(a.Fsm.CaseInsensitive);

  a := NewAcFsmUtf8(['раз', 'раз', 'два', 'два', 'три', 'три']);
  AssertTrue(a.PatternListSize = 6);
  AssertTrue(Length(a.GetPatternList) = 6);
  AssertFalse(a.Fsm.CaseInsensitive);
  AssertTrue(a.Fsm.StateCount = 10);
  AssertTrue(a.Fsm.PatternCount = 3);
  AssertTrue(a.Fsm.AlphabetSize = 7);
  AssertTrue(a.Fsm.IsMatch('раз'));
  AssertTrue(a.Fsm.IsMatch('два'));
  AssertTrue(a.Fsm.IsMatch('три'));
end;

procedure TACPersistFsmTest.TestStreaming;
var
  ms: specialize TGAutoRef<TMemoryStream>;
  a: IACPersistFsmUtf8;
begin
  a := NewAcFsmUtf8([]);
  a.SaveToStream(ms.Instance);
  a := nil;
  ms.Instance.Position := 0;

  a := LoadAcFsmUtf8(ms.Instance);
  AssertTrue(a.PatternListSize = 0);
  AssertTrue(a.GetPatternList = nil);
  AssertFalse(a.Fsm.CaseInsensitive);
  AssertTrue(a.Fsm.StateCount = 1);
  AssertTrue(a.Fsm.PatternCount = 0);
  AssertTrue(a.Fsm.AlphabetSize = 0);

  ms.Instance.Clear;
  a := NewAcFsmUtf8(['раз', 'раз', 'два', 'два', 'три', 'три'], True);
  a.SaveToStream(ms.Instance);
  a := nil;
  ms.Instance.Position := 0;

  a := LoadAcFsmUtf8(ms.Instance);
  AssertTrue(a.PatternListSize = 6);
  AssertTrue(Length(a.GetPatternList) = 6);
  AssertTrue(a.Fsm.CaseInsensitive);
  AssertTrue(a.Fsm.StateCount = 10);
  AssertTrue(a.Fsm.PatternCount = 3);
  AssertTrue(a.Fsm.AlphabetSize = 7);
  AssertTrue(a.Fsm.IsMatch('раз'));
  AssertTrue(a.Fsm.IsMatch('два'));
  AssertTrue(a.Fsm.IsMatch('три'));
end;

{ TKmpSearchCITest }

procedure TKmpSearchCITest.TestCreation;
var
  Kmp: TKmpSearchCI;
begin
  AssertFalse({%H-}Kmp.Initialized);
  Kmp.Init('');
  AssertFalse(Kmp.Initialized);
  Kmp.Init(NotAscii);
  AssertFalse(Kmp.Initialized);
  Kmp.Init(Ascii);
  AssertTrue(Kmp.Initialized);
  Kmp.Init(Ascii, True);
  AssertTrue(Kmp.OnlyWholeWords);

  Kmp := TKmpSearchCI.Create('');
  AssertFalse(Kmp.Initialized);
  Kmp := TKmpSearchCI.Create(NotAscii);
  AssertFalse(Kmp.Initialized);
  Kmp := TKmpSearchCI.Create(Ascii);
  AssertTrue(Kmp.Initialized);
  Kmp := TKmpSearchCI.Create(Ascii, True);
  AssertTrue(Kmp.OnlyWholeWords);
end;

procedure TKmpSearchCITest.TestNextMatch;
var
  Kmp: TKmpSearchCI;
  Txt, Key: string;
  m: TMatch;
begin
  Key := '2025';
  Kmp.Init(Key);
  Txt := '1234567890123456789';
  AssertTrue(Kmp.NextMatch(Txt).Offset = 0);

  Txt := '1234567890123420256789';
  m := Kmp.NextMatch(Txt);
  AssertTrue(m.Offset <> 0);
  AssertTrue(Copy(Txt, m.Offset, m.Length) = Key);

  m := Kmp.NextMatch(Txt, 15, 4);
  AssertTrue(m.Offset <> 0);
  AssertTrue(Copy(Txt, m.Offset, m.Length) = Key);

  m := Kmp.NextMatch(Txt, 16, 4);
  AssertTrue(m.Offset = 0);

  Key := '2025ГоД';
  Kmp.Init(Key);
  AssertTrue(Kmp.NextMatch(Txt).Offset = 0);

  Txt := 'просто строка 22020220252025г2025го2025гОд6789';
  m := Kmp.NextMatch(Txt);
  AssertTrue(m.Offset <> 0);
  AssertTrue(Utf8ToLower(Copy(Txt, m.Offset, m.Length)) = Utf8ToLower(Key));

  Key := 'Г';
  Kmp.Init(Key);
  m := Kmp.NextMatch(Txt);
  AssertTrue(m.Offset <> 0);
  AssertTrue(Utf8ToLower(Copy(Txt, m.Offset, m.Length)) = Utf8ToLower(Key));
end;

procedure TKmpSearchCITest.TestNextMatchOww;
var
  Kmp: TKmpSearchCI;
  Txt, Key: string;
  m: TMatch;
begin
  Key := '2025Г';
  Kmp.Init(Key, True);
  Txt := '123456789012342025г6789';
  AssertTrue(Kmp.NextMatch(Txt).Offset = 0);

  Txt := '2025г_12345678901234-2025г_6789_2025г';
  AssertTrue(Kmp.NextMatch(Txt).Offset = 0);

  Txt := '12345678901234-2025г-6789';
  m := Kmp.NextMatch(Txt);
  AssertTrue(m.Offset <> 0);
  AssertTrue(Utf8ToLower(Copy(Txt, m.Offset, m.Length)) = Utf8ToLower(Key));

  Txt := '2025г 1234567890123456789';
  m := Kmp.NextMatch(Txt);
  AssertTrue(m.Offset <> 0);
  AssertTrue(Utf8ToLower(Copy(Txt, m.Offset, m.Length)) = Utf8ToLower(Key));

  Txt := '1234567890123456789-2025г';
  m := Kmp.NextMatch(Txt);
  AssertTrue(m.Offset <> 0);
  AssertTrue(Utf8ToLower(Copy(Txt, m.Offset, m.Length)) = Utf8ToLower(Key));

  Txt := '12345678901234-2025г-6789';
  m := Kmp.NextMatch(Txt);
  AssertTrue(m.Offset <> 0);
  AssertTrue(Utf8ToLower(Copy(Txt, m.Offset, m.Length)) = Utf8ToLower(Key));

  Key := 'Г';
  Kmp.Init(Key, True);
  AssertTrue(Kmp.NextMatch(Txt).Offset = 0);

  Txt := '12345678901234-2025.г-6789';
  m := Kmp.NextMatch(Txt);
  AssertTrue(m.Offset <> 0);
  AssertTrue(Utf8ToLower(Copy(Txt, m.Offset, m.Length)) = Utf8ToLower(Key));
end;

procedure TKmpSearchCITest.TestFindMatches;
var
  Kmp: TKmpSearchCI;
  Txt, Key: string;
  ma: array of TMatch;
  m: TMatch;
begin
  Key := 'аБеГд';
  Txt := 'абВГДАбвгДАБВгдабВГДабвгд';
  Kmp.Init(Key);
  ma := Kmp.FindMatches(Txt);
  AssertTrue(ma = nil);

  Key := 'аБвГд';
  Kmp.Init(Key);
  ma := Kmp.FindMatches(Txt);
  AssertTrue(Length(ma) = 5);
  for m in ma do
    AssertTrue(Utf8ToLower(Copy(Txt, m.Offset, m.Length)) = Utf8ToLower(Key));

  Key := 'абвгАБВГ';
  Txt := 'абВГАбвгАБВгабВГабВгабВГ';
  Kmp.Init(Key);
  ma := Kmp.FindMatches(Txt);
  AssertTrue(Length(ma) = 3);
  for m in ma do
    AssertTrue(Utf8ToLower(Copy(Txt, m.Offset, m.Length)) = Utf8ToLower(Key));

  ma := Kmp.FindMatches(Txt, True);
  AssertTrue(Length(ma) = 5);
  for m in ma do
    AssertTrue(Utf8ToLower(Copy(Txt, m.Offset, m.Length)) = Utf8ToLower(Key));
end;

procedure TKmpSearchCITest.TestFindMatchesOww;
var
  Kmp: TKmpSearchCI;
  Txt, Key: string;
  ma: array of TMatch;
  m: TMatch;
begin
  Txt := 'абВГДАбвгДАБВгдабВГДабвгд';
  Key := 'аБвГд';
  Kmp.Init(Key, True);
  ma := Kmp.FindMatches(Txt);
  AssertTrue(ma = nil);

  Txt := 'абВГД.АбвгД,АБВгд абВГД*абвгд';
  ma := Kmp.FindMatches(Txt);
  AssertTrue(Length(ma) = 5);
  for m in ma do
    AssertTrue(Utf8ToLower(Copy(Txt, m.Offset, m.Length)) = Utf8ToLower(Key));

  Txt := 'абВГД.АбвгД.АБВгд.абВГД.абвгд';
  Key := 'аБвГд.АбВгД';
  Kmp.Init(Key, True);
  ma := Kmp.FindMatches(Txt);
  AssertTrue(Length(ma) = 2);
  for m in ma do
    AssertTrue(Utf8ToLower(Copy(Txt, m.Offset, m.Length)) = Utf8ToLower(Key));

  ma := Kmp.FindMatches(Txt, True);
  AssertTrue(Length(ma) = 4);
  for m in ma do
    AssertTrue(Utf8ToLower(Copy(Txt, m.Offset, m.Length)) = Utf8ToLower(Key));
end;

{ TKmpSearchTest }

procedure TKmpSearchTest.TestNextMatch;
var
  Kmp: TKmpSearch;
  Txt, Key: string;
  Ofs: Integer;
begin
  Key := '';
  Txt := '';
  Kmp.Init(Key);
  AssertTrue(Kmp.NextMatch(Txt) = 0);

  Txt := '1234567890123420256789';
  AssertTrue(Kmp.NextMatch(Txt) = 0);

  Key := '2025';
  Kmp.Init(Key);
  Ofs := Kmp.NextMatch(Txt);
  AssertTrue(Ofs <> 0);
  AssertTrue(Copy(Txt, Ofs, Length(Key)) = Key);

  AssertTrue(Kmp.NextMatch(Txt, 1, 17) = 0);
end;

procedure TKmpSearchTest.TestNextMatchOww;
var
  Kmp: TKmpSearch;
  Txt, Key: string;
  Ofs: Integer;
begin
  Key := '';
  Txt := '';
  Kmp.Init(Key, True);
  AssertTrue(Kmp.NextMatch(Txt) = 0);

  Txt := '1234567890123420256789';
  AssertTrue(Kmp.NextMatch(Txt) = 0);

  Key := '2025';
  Kmp.Init(Key, True);
  AssertTrue(Kmp.NextMatch(Txt) = 0);

  Txt := '12345678901234 20256789';
  AssertTrue(Kmp.NextMatch(Txt) = 0);

  Txt := '123456789012342025 6789';
  AssertTrue(Kmp.NextMatch(Txt) = 0);

  Txt := '2025-234567890123456789';
  Ofs := Kmp.NextMatch(Txt);
  AssertTrue(Ofs <> 0);
  AssertTrue(Copy(Txt, Ofs, Length(Key)) = Key);

  Txt := '1234567890123456789.2025';
  Ofs := Kmp.NextMatch(Txt);
  AssertTrue(Ofs <> 0);
  AssertTrue(Copy(Txt, Ofs, Length(Key)) = Key);

  Txt := '12345678901234!2025?6789';
  Ofs := Kmp.NextMatch(Txt);
  AssertTrue(Ofs <> 0);
  AssertTrue(Copy(Txt, Ofs, Length(Key)) = Key);

  AssertTrue(Kmp.NextMatch(Txt, 1, 18) = 0);
end;

procedure TKmpSearchTest.TestFindMatches;
var
  Kmp: TKmpSearch;
  Txt, Key: string;
  a: array of SizeInt;
  Ofs: SizeInt;
begin
  Key := '12335';
  Txt := '1234512345123451234512345';
  Kmp.Init(Key);
  a := Kmp.FindMatches(Txt);
  AssertTrue(a = nil);

  Key := '12345';
  Kmp.Init(Key);
  a := Kmp.FindMatches(Txt);
  AssertTrue(Length(a) = 5);
  for Ofs in a do
    AssertTrue(Copy(Txt, Ofs, Length(Key)) = Key);

  Key := '123123';
  Txt := '123123123123123123';
  Kmp.Init(Key);
  a := Kmp.FindMatches(Txt);
  AssertTrue(Length(a) = 3);
  for Ofs in a do
    AssertTrue(Copy(Txt, Ofs, Length(Key)) = Key);

  a := Kmp.FindMatches(Txt, True);
  AssertTrue(Length(a) = 5);
  for Ofs in a do
    AssertTrue(Copy(Txt, Ofs, Length(Key)) = Key);
end;

procedure TKmpSearchTest.TestFindMatchesOww;
var
  Kmp: TKmpSearch;
  Txt, Key: string;
  a: array of SizeInt;
  Ofs: SizeInt;
begin
  Key := '12345';
  Txt := '1234512345123451234512345';
  Kmp.Init(Key, True);
  a := Kmp.FindMatches(Txt);
  AssertTrue(a = nil);

  Txt := '12345.12345.12345.12345.12345';
  a := Kmp.FindMatches(Txt);
  AssertTrue(Length(a) = 5);
  for Ofs in a do
    AssertTrue(Copy(Txt, Ofs, Length(Key)) = Key);

  Key := '123.123';
  Txt := '123.123.123.123.123.123';
  Kmp.Init(Key, True);
  a := Kmp.FindMatches(Txt);
  AssertTrue(Length(a) = 3);
  for Ofs in a do
    AssertTrue(Copy(Txt, Ofs, Length(Key)) = Key);

  a := Kmp.FindMatches(Txt, True);
  AssertTrue(Length(a) = 5);
  for Ofs in a do
    AssertTrue(Copy(Txt, Ofs, Length(Key)) = Key);
end;


initialization

  RegisterTest(TTestUnicodeUtils);
  RegisterTest(TTestSeqUtils);
  RegisterTest(TTestFuzzySearchBitap);
  RegisterTest(TACSearchFsmTest);
  RegisterTest(TACPersistFsmTest);
  RegisterTest(TKmpSearchCITest);
  RegisterTest(TKmpSearchTest);

end.

