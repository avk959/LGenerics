unit lgSeqUtilsTest;

{$mode objfpc}{$H+}{$warn 6058 off}{$modeswitch nestedprocvars}

interface

uses
  LazUtf8, Classes, SysUtils, Math, fpcunit, testregistry,
  lgUtils,
  lgHashSet,
  lgSeqUtils;

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
    procedure SubSequenceUtf16;
    procedure LevenshteinDistUtf8;
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
    procedure LcsDistMyersUtf8;
    procedure LcsDistMyersBoundedUtf8;
    procedure LcsDistMyersDynUtf8;
    procedure DumDistMbrUtf8;
    procedure DumDistMbrBoundedUtf8;
    procedure DumDistMbrDynUtf8;
    procedure LcsGusUtf8Test;
    procedure LcsKRUtf8Test;
    procedure LcsMyersUtf8Test;
    procedure SimRatioUtf8Test;
    procedure SimRatioExUtf8Test;
    procedure FuzzySearchEdp;

    procedure Utf16ToUcs4;
    procedure SubSequenceUtf8;
    procedure LevenshteinDistUtf16;
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
    procedure LcsDistMyersUtf16;
    procedure LcsDistMyersBoundedUtf16;
    procedure LcsDistMyersDynUtf16;
    procedure DumDistMbrUtf16;
    procedure DumDistMbrBoundedUtf16;
    procedure DumDistMbrDynUtf16;
    procedure LcsGusUtf16Test;
    procedure LcsKRUtf16Test;
    procedure LcsMyersUtf16Test;
    procedure SimRatioUtf16Test;
    procedure SimRatioExUtf16Test;
  end;

  { TTestSeqUtils }

  TTestSeqUtils = class(TTestCase)
  private
  type
    TIntSeqUtil = specialize TGSeqUtil<Integer, Integer>;
    TIntArray   = array of Integer;
  published
    procedure IsPrefixTest;
    procedure IsSuffixTest;
    procedure PrefixLenTest;
    procedure SuffixLenTest;
    procedure TestDiff;
    procedure SameTest;
    procedure IsPermutationTest;
  end;

  { TTestFuzzySearchBitap }

  TTestFuzzySearchBitap = class(TTestCase)
  published
    procedure TestCreate;
    procedure TestNextMatch;
    procedure TestNextMatchCI;
    procedure TestFindMatches;
    procedure TestFindMatchesCI;
    procedure TestEnumerator;
    procedure TestEnumeratorCI;
  end;

  { TACSearchFsmTest }

  TACSearchFsmTest = class(TTestCase)
  private
  type
    TStrSetType = specialize TGLiteChainHashSet<string, string>;
    TStrSet     = TStrSetType.TSet;
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
    procedure TestContainsMatchDfa;
    procedure TestContainsMatchNfa;
    procedure TestContainsMatchDfaCI;
    procedure TestContainsMatchNfaCI;
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

procedure TTestUnicodeUtils.LevenshteinDistUtf8;
var
  s1, s2: string;
begin
  s1 := '';
  s2 := 'привет';
  AssertTrue(LevDistanceUtf8(s1, s1) = 0);
  AssertTrue(LevDistanceUtf8(s2, s2) = 0);
  AssertTrue(LevDistanceUtf8(s1, s2) = 6);
  AssertTrue(LevDistanceUtf8(s2, s1) = 6);

  s1 := 'аб';
  s2 := 'аа';
  AssertTrue(LevDistanceUtf8(s1, s2) = 1);
  AssertTrue(LevDistanceUtf8(s2, s1) = 1);

  s1 := 'аб';
  s2 := 'ба';
  AssertTrue(LevDistanceUtf8(s1, s2) = 2);
  AssertTrue(LevDistanceUtf8(s2, s1) = 2);

  s1 := 'аб';
  s2 := 'ааа';
  AssertTrue(LevDistanceUtf8(s1, s2) = 2);
  AssertTrue(LevDistanceUtf8(s2, s1) = 2);

  s1 := 'а';
  s2 := 'ббб';
  AssertTrue(LevDistanceUtf8(s1, s2) = 3);
  AssertTrue(LevDistanceUtf8(s2, s1) = 3);

  s1 := 'аабабаб';
  s2 := 'аббаа';
  AssertTrue(LevDistanceUtf8(s1, s2) = 3);
  AssertTrue(LevDistanceUtf8(s2, s1) = 3);

  s1 := 'нелли';
  s2 := 'елли';
  AssertTrue(LevDistanceUtf8(s1, s2) = 1);
  AssertTrue(LevDistanceUtf8(s2, s1) = 1);

  s1 := 'нелли';
  s2 := 'еллия';
  AssertTrue(LevDistanceUtf8(s1, s2) = 2);
  AssertTrue(LevDistanceUtf8(s2, s1) = 2);

  s1 := 'мостик';
  s2 := 'костик';
  AssertTrue(LevDistanceUtf8(s1, s2) = 1);
  AssertTrue(LevDistanceUtf8(s2, s1) = 1);

  s1 := 'мостик';
  s2 := 'костяка';
  AssertTrue(LevDistanceUtf8(s1, s2) = 3);
  AssertTrue(LevDistanceUtf8(s2, s1) = 3);

  s1 := 'дистанция';
  s2 := 'дисперсия';
  AssertTrue(LevDistanceUtf8(s1, s2) = 4);
  AssertTrue(LevDistanceUtf8(s2, s1) = 4);

  s1 := 'левенштейн';
  s2 := 'франкенштейн';
  AssertTrue(LevDistanceUtf8(s1, s2) = 5);
  AssertTrue(LevDistanceUtf8(s2, s1) = 5);

  s1 := 'ааааааа';
  s2 := 'ааааааа';
  AssertTrue(LevDistanceUtf8(s1, s2) = 0);

  s1 := 'ааааааа';
  s2 := 'бббббббб';
  AssertTrue(LevDistanceUtf8(s1, s2) = 8);
  AssertTrue(LevDistanceUtf8(s2, s1) = 8);

  s1 := 'аааббаааа';
  s2 := 'ааааааа';
  AssertTrue(LevDistanceUtf8(s1, s2) = 2);
  AssertTrue(LevDistanceUtf8(s2, s1) = 2);

  s1 := 'а';
  s2 := 'б';
  AssertTrue(LevDistanceUtf8(s1, s2) = 1);
  AssertTrue(LevDistanceUtf8(s2, s1) = 1);

  s1 := '一天，在寒冷的冬天，我从森林里走出来。';
  s2 := '一，在寒冷的冬天，我从森林里走来。';
  AssertTrue(LevDistanceUtf8(s1, s2) = 2);
  AssertTrue(LevDistanceUtf8(s2, s1) = 2);
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
  AssertTrue(LevDistanceMyersUtf8(s1, s2) = LevDistanceUtf8(s1, s2));
  AssertTrue(LevDistanceMyersUtf8(s3, s4) = LevDistanceUtf8(s3, s4));
  AssertTrue(LevDistanceMyersUtf8(s5, s6) = LevDistanceUtf8(s5, s6));
  AssertTrue(LevDistanceMyersUtf8(s7, s8) = LevDistanceUtf8(s7, s8));
  AssertTrue(LevDistanceMyersUtf8(s9, s10) = LevDistanceUtf8(s9, s10));
  AssertTrue(LevDistanceMyersUtf8(s11, s12) = LevDistanceUtf8(s11, s12));
  AssertTrue(LevDistanceMyersUtf8(s13, s14) = LevDistanceUtf8(s13, s14));
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
  AssertTrue(LevDistanceMyersUtf8(s1, s2) = LevDistanceUtf8(s1, s2));
  AssertTrue(LevDistanceMyersUtf8(s3, s4) = LevDistanceUtf8(s3, s4));
  AssertTrue(LevDistanceMyersUtf8(s5, s6) = LevDistanceUtf8(s5, s6));
  AssertTrue(LevDistanceMyersUtf8(s7, s8) = LevDistanceUtf8(s7, s8));
  AssertTrue(LevDistanceMyersUtf8(s9, s10) = LevDistanceUtf8(s9, s10));
  AssertTrue(LevDistanceMyersUtf8(s11, s12) = LevDistanceUtf8(s11, s12));
  AssertTrue(LevDistanceMyersUtf8(s13, s14) = LevDistanceUtf8(s13, s14));
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
  AssertTrue(LevDistanceMyersUtf8(s1, s2, Pred(LevDistanceUtf8(s1, s2))) = -1);
  AssertTrue(LevDistanceMyersUtf8(s3, s4, Pred(LevDistanceUtf8(s3, s4))) = -1);
  AssertTrue(LevDistanceMyersUtf8(s5, s6, Pred(LevDistanceUtf8(s5, s6))) = -1);
  AssertTrue(LevDistanceMyersUtf8(s7, s8, Pred(LevDistanceUtf8(s7, s8))) = -1);
  AssertTrue(LevDistanceMyersUtf8(s9, s10, Pred(LevDistanceUtf8(s9, s10))) = -1);
  AssertTrue(LevDistanceMyersUtf8(s11, s12, Pred(LevDistanceUtf8(s11, s12))) = -1);
  AssertTrue(LevDistanceMyersUtf8(s13, s14, Pred(LevDistanceUtf8(s13, s14))) = -1);
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
  AssertTrue(LevDistanceMyersUtf8(s1, s2, Pred(LevDistanceUtf8(s1, s2))) = -1);
  AssertTrue(LevDistanceMyersUtf8(s3, s4, Pred(LevDistanceUtf8(s3, s4))) = -1);
  AssertTrue(LevDistanceMyersUtf8(s5, s6, Pred(LevDistanceUtf8(s5, s6))) = -1);
  AssertTrue(LevDistanceMyersUtf8(s7, s8, Pred(LevDistanceUtf8(s7, s8))) = -1);
  AssertTrue(LevDistanceMyersUtf8(s9, s10, Pred(LevDistanceUtf8(s9, s10))) = -1);
  AssertTrue(LevDistanceMyersUtf8(s11, s12, Pred(LevDistanceUtf8(s11, s12))) = -1);
  AssertTrue(LevDistanceMyersUtf8(s13, s14, Pred(LevDistanceUtf8(s13, s14))) = -1);
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
  AssertTrue(LevDistanceMyersUtf8(s1, s2, -1) = LevDistanceUtf8(s1, s2));
  AssertTrue(LevDistanceMyersUtf8(s3, s4, -1) = LevDistanceUtf8(s3, s4));
  AssertTrue(LevDistanceMyersUtf8(s5, s6, -1) = LevDistanceUtf8(s5, s6));
  AssertTrue(LevDistanceMyersUtf8(s7, s8, -1) = LevDistanceUtf8(s7, s8));
  AssertTrue(LevDistanceMyersUtf8(s9, s10, -1) = LevDistanceUtf8(s9, s10));
  AssertTrue(LevDistanceMyersUtf8(s11, s12, -1) = LevDistanceUtf8(s11, s12));
  AssertTrue(LevDistanceMyersUtf8(s13, s14, -1) = LevDistanceUtf8(s13, s14));
end;

procedure TTestUnicodeUtils.LcsDistMyersUtf8;
var
  s1, s2: string;
begin
  s1 := '';
  s2 := 'привет';
  AssertTrue(LcsDistanceMyersUtf8(s1, s1) = 0);
  AssertTrue(LcsDistanceMyersUtf8(s2, s2) = 0);
  AssertTrue(LcsDistanceMyersUtf8(s1, s2) = 6);
  AssertTrue(LcsDistanceMyersUtf8(s2, s1) = 6);

  s1 := 'аб';
  s2 := 'аа';
  AssertTrue(LcsDistanceMyersUtf8(s1, s2) = 2);
  AssertTrue(LcsDistanceMyersUtf8(s2, s1) = 2);

  s1 := 'аб';
  s2 := 'ба';
  AssertTrue(LcsDistanceMyersUtf8(s1, s2) = 2);
  AssertTrue(LcsDistanceMyersUtf8(s2, s1) = 2);

  s1 := 'аб';
  s2 := 'ааа';
  AssertTrue(LcsDistanceMyersUtf8(s1, s2) = 3);
  AssertTrue(LcsDistanceMyersUtf8(s2, s1) = 3);

  s1 := 'а';
  s2 := 'ббб';
  AssertTrue(LcsDistanceMyersUtf8(s1, s2) = 4);
  AssertTrue(LcsDistanceMyersUtf8(s2, s1) = 4);

  s1 := 'аабабаб';
  s2 := 'аббаа';
  AssertTrue(LcsDistanceMyersUtf8(s1, s2) = 4);
  AssertTrue(LcsDistanceMyersUtf8(s2, s1) = 4);

  s1 := 'нелли';
  s2 := 'елли';
  AssertTrue(LcsDistanceMyersUtf8(s1, s2) = 1);
  AssertTrue(LcsDistanceMyersUtf8(s2, s1) = 1);

  s1 := 'нелли';
  s2 := 'еллия';
  AssertTrue(LcsDistanceMyersUtf8(s1, s2) = 2);
  AssertTrue(LcsDistanceMyersUtf8(s2, s1) = 2);

  s1 := 'мостик';
  s2 := 'костик';
  AssertTrue(LcsDistanceMyersUtf8(s1, s2) = 2);
  AssertTrue(LcsDistanceMyersUtf8(s2, s1) = 2);

  s1 := 'мостик';
  s2 := 'костяка';
  AssertTrue(LcsDistanceMyersUtf8(s1, s2) = 5);
  AssertTrue(LcsDistanceMyersUtf8(s2, s1) = 5);

  s1 := 'дистанция';
  s2 := 'дисперсия';
  AssertTrue(LcsDistanceMyersUtf8(s1, s2) = 8);
  AssertTrue(LcsDistanceMyersUtf8(s2, s1) = 8);

  s1 := 'левенштейн';
  s2 := 'франкенштейн';
  AssertTrue(LcsDistanceMyersUtf8(s1, s2) = 8);
  AssertTrue(LcsDistanceMyersUtf8(s2, s1) = 8);

  s1 := 'ааааааа';
  s2 := 'ааааааа';
  AssertTrue(LcsDistanceMyersUtf8(s1, s2) = 0);

  s1 := 'ааааааа';
  s2 := 'бббббббб';
  AssertTrue(LcsDistanceMyersUtf8(s1, s2) = 15);
  AssertTrue(LcsDistanceMyersUtf8(s2, s1) = 15);

  s1 := 'аааббаааа';
  s2 := 'ааааааа';
  AssertTrue(LcsDistanceMyersUtf8(s1, s2) = 2);
  AssertTrue(LcsDistanceMyersUtf8(s2, s1) = 2);

  s1 := 'а';
  s2 := 'б';
  AssertTrue(LcsDistanceMyersUtf8(s1, s2) = 2);
  AssertTrue(LcsDistanceMyersUtf8(s2, s1) = 2);

  s1 := '一天，在寒冷的冬天，我从森林里走出来。';
  s2 := '一，在寒冷的冬天，我从森林里走来。';
  AssertTrue(LcsDistanceMyersUtf8(s1, s2) = 2);
  AssertTrue(LcsDistanceMyersUtf8(s2, s1) = 2);
end;

procedure TTestUnicodeUtils.LcsDistMyersBoundedUtf8;
var
  s1, s2: string;
begin
  s1 := '';
  s2 := 'привет';
  AssertTrue(LcsDistanceMyersUtf8(s1, s2) = 6);
  AssertTrue(LcsDistanceMyersUtf8(s1, s2, 5) = -1);

  s1 := 'аб';
  s2 := 'аа';
  AssertTrue(LcsDistanceMyersUtf8(s1, s2) = 2);
  AssertTrue(LcsDistanceMyersUtf8(s1, s2, 1) = -1);

  s1 := 'аб';
  s2 := 'ба';
  AssertTrue(LcsDistanceMyersUtf8(s1, s2) = 2);
  AssertTrue(LcsDistanceMyersUtf8(s1, s2, 1) = -1);

  s1 := 'аб';
  s2 := 'ааа';
  AssertTrue(LcsDistanceMyersUtf8(s1, s2) = 3);
  AssertTrue(LcsDistanceMyersUtf8(s1, s2, 2) = -1);

  s1 := 'а';
  s2 := 'ббб';
  AssertTrue(LcsDistanceMyersUtf8(s1, s2) = 4);
  AssertTrue(LcsDistanceMyersUtf8(s1, s2, 3) = -1);

  s1 := 'аабабаб';
  s2 := 'аббаа';
  AssertTrue(LcsDistanceMyersUtf8(s1, s2) = 4);
  AssertTrue(LcsDistanceMyersUtf8(s1, s2, 3) = -1);

  s1 := 'нелли';
  s2 := 'елли';
  AssertTrue(LcsDistanceMyersUtf8(s1, s2) = 1);
  AssertTrue(LcsDistanceMyersUtf8(s1, s2, 0) = -1);

  s1 := 'нелли';
  s2 := 'еллия';
  AssertTrue(LcsDistanceMyersUtf8(s1, s2) = 2);
  AssertTrue(LcsDistanceMyersUtf8(s1, s2, 1) = -1);

  s1 := 'мостик';
  s2 := 'костик';
  AssertTrue(LcsDistanceMyersUtf8(s1, s2) = 2);
  AssertTrue(LcsDistanceMyersUtf8(s1, s2, 1) = -1);

  s1 := 'мостик';
  s2 := 'костяка';
  AssertTrue(LcsDistanceMyersUtf8(s1, s2) = 5);
  AssertTrue(LcsDistanceMyersUtf8(s1, s2, 4) = -1);

  s1 := 'дистанция';
  s2 := 'дисперсия';
  AssertTrue(LcsDistanceMyersUtf8(s1, s2) = 8);
  AssertTrue(LcsDistanceMyersUtf8(s1, s2, 7) = -1);

  s1 := 'левенштейн';
  s2 := 'франкенштейн';
  AssertTrue(LcsDistanceMyersUtf8(s1, s2) = 8);
  AssertTrue(LcsDistanceMyersUtf8(s1, s2, 7) = -1);

  s1 := 'ааааааа';
  s2 := 'бббббббб';
  AssertTrue(LcsDistanceMyersUtf8(s1, s2) = 15);
  AssertTrue(LcsDistanceMyersUtf8(s1, s2, 14) = -1);

  s1 := 'аааббаааа';
  s2 := 'ааааааа';
  AssertTrue(LcsDistanceMyersUtf8(s1, s2) = 2);
  AssertTrue(LcsDistanceMyersUtf8(s1, s2, 1) = -1);

  s1 := 'а';
  s2 := 'б';
  AssertTrue(LcsDistanceMyersUtf8(s1, s2) = 2);
  AssertTrue(LcsDistanceMyersUtf8(s1, s2, 1) = -1);

  s1 := '一天，在寒冷的冬天，我从森林里走出来。';
  s2 := '一，在寒冷的冬天，我从森林里走来。';
  AssertTrue(LcsDistanceMyersUtf8(s1, s2) = 2);
  AssertTrue(LcsDistanceMyersUtf8(s2, s1, 1) = -1);
end;

procedure TTestUnicodeUtils.LcsDistMyersDynUtf8;
var
  s1, s2: string;
begin
  s1 := '';
  s2 := 'привет';
  AssertTrue(LcsDistanceMyersUtf8(s1, s1, -1) = 0);
  AssertTrue(LcsDistanceMyersUtf8(s2, s2, -1) = 0);
  AssertTrue(LcsDistanceMyersUtf8(s1, s2, -1) = 6);
  AssertTrue(LcsDistanceMyersUtf8(s2, s1, -1) = 6);

  s1 := 'аб';
  s2 := 'аа';
  AssertTrue(LcsDistanceMyersUtf8(s1, s2, -1) = 2);
  AssertTrue(LcsDistanceMyersUtf8(s2, s1, -1) = 2);

  s1 := 'аб';
  s2 := 'ба';
  AssertTrue(LcsDistanceMyersUtf8(s1, s2, -1) = 2);
  AssertTrue(LcsDistanceMyersUtf8(s2, s1, -1) = 2);

  s1 := 'аб';
  s2 := 'ааа';
  AssertTrue(LcsDistanceMyersUtf8(s1, s2, -1) = 3);
  AssertTrue(LcsDistanceMyersUtf8(s2, s1, -1) = 3);

  s1 := 'а';
  s2 := 'ббб';
  AssertTrue(LcsDistanceMyersUtf8(s1, s2, -1) = 4);
  AssertTrue(LcsDistanceMyersUtf8(s2, s1, -1) = 4);

  s1 := 'аабабаб';
  s2 := 'аббаа';
  AssertTrue(LcsDistanceMyersUtf8(s1, s2, -1) = 4);
  AssertTrue(LcsDistanceMyersUtf8(s2, s1, -1) = 4);

  s1 := 'нелли';
  s2 := 'елли';
  AssertTrue(LcsDistanceMyersUtf8(s1, s2, -1) = 1);
  AssertTrue(LcsDistanceMyersUtf8(s2, s1, -1) = 1);

  s1 := 'нелли';
  s2 := 'еллия';
  AssertTrue(LcsDistanceMyersUtf8(s1, s2, -1) = 2);
  AssertTrue(LcsDistanceMyersUtf8(s2, s1, -1) = 2);

  s1 := 'мостик';
  s2 := 'костик';
  AssertTrue(LcsDistanceMyersUtf8(s1, s2, -1) = 2);
  AssertTrue(LcsDistanceMyersUtf8(s2, s1, -1) = 2);

  s1 := 'мостик';
  s2 := 'костяка';
  AssertTrue(LcsDistanceMyersUtf8(s1, s2, -1) = 5);
  AssertTrue(LcsDistanceMyersUtf8(s2, s1, -1) = 5);

  s1 := 'дистанция';
  s2 := 'дисперсия';
  AssertTrue(LcsDistanceMyersUtf8(s1, s2, -1) = 8);
  AssertTrue(LcsDistanceMyersUtf8(s2, s1, -1) = 8);

  s1 := 'левенштейн';
  s2 := 'франкенштейн';
  AssertTrue(LcsDistanceMyersUtf8(s1, s2, -1) = 8);
  AssertTrue(LcsDistanceMyersUtf8(s2, s1, -1) = 8);

  s1 := 'ааааааа';
  s2 := 'ааааааа';
  AssertTrue(LcsDistanceMyersUtf8(s1, s2, -1) = 0);

  s1 := 'ааааааа';
  s2 := 'бббббббб';
  AssertTrue(LcsDistanceMyersUtf8(s1, s2, -1) = 15);
  AssertTrue(LcsDistanceMyersUtf8(s2, s1, -1) = 15);

  s1 := 'аааббаааа';
  s2 := 'ааааааа';
  AssertTrue(LcsDistanceMyersUtf8(s1, s2, -1) = 2);
  AssertTrue(LcsDistanceMyersUtf8(s2, s1, -1) = 2);

  s1 := 'а';
  s2 := 'б';
  AssertTrue(LcsDistanceMyersUtf8(s1, s2, -1) = 2);
  AssertTrue(LcsDistanceMyersUtf8(s2, s1, -1) = 2);

  s1 := '一天，在寒冷的冬天，我从森林里走出来。';
  s2 := '一，在寒冷的冬天，我从森林里走来。';
  AssertTrue(LcsDistanceMyersUtf8(s1, s2, -1) = 2);
  AssertTrue(LcsDistanceMyersUtf8(s2, s1, -1) = 2);
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

procedure TTestUnicodeUtils.LevenshteinDistUtf16;
var
  s1, s2: string;
begin
  s1 := '';
  s2 := 'привет';
  AssertTrue(LevDistanceUtf16(s1, s1) = 0);
  AssertTrue(LevDistanceUtf16(s2, s2) = 0);
  AssertTrue(LevDistanceUtf16(s1, s2) = 6);
  AssertTrue(LevDistanceUtf16(s2, s1) = 6);

  s1 := 'аб';
  s2 := 'аа';
  AssertTrue(LevDistanceUtf16(s1, s2) = 1);
  AssertTrue(LevDistanceUtf16(s2, s1) = 1);

  s1 := 'аб';
  s2 := 'ба';
  AssertTrue(LevDistanceUtf16(s1, s2) = 2);
  AssertTrue(LevDistanceUtf16(s2, s1) = 2);

  s1 := 'аб';
  s2 := 'ааа';
  AssertTrue(LevDistanceUtf16(s1, s2) = 2);
  AssertTrue(LevDistanceUtf16(s2, s1) = 2);

  s1 := 'а';
  s2 := 'ббб';
  AssertTrue(LevDistanceUtf16(s1, s2) = 3);
  AssertTrue(LevDistanceUtf16(s2, s1) = 3);

  s1 := 'аабабаб';
  s2 := 'аббаа';
  AssertTrue(LevDistanceUtf16(s1, s2) = 3);
  AssertTrue(LevDistanceUtf16(s2, s1) = 3);

  s1 := 'нелли';
  s2 := 'елли';
  AssertTrue(LevDistanceUtf16(s1, s2) = 1);
  AssertTrue(LevDistanceUtf16(s2, s1) = 1);

  s1 := 'нелли';
  s2 := 'еллия';
  AssertTrue(LevDistanceUtf16(s1, s2) = 2);
  AssertTrue(LevDistanceUtf16(s2, s1) = 2);

  s1 := 'мостик';
  s2 := 'костик';
  AssertTrue(LevDistanceUtf16(s1, s2) = 1);
  AssertTrue(LevDistanceUtf16(s2, s1) = 1);

  s1 := 'мостик';
  s2 := 'костяка';
  AssertTrue(LevDistanceUtf16(s1, s2) = 3);
  AssertTrue(LevDistanceUtf16(s2, s1) = 3);

  s1 := 'дистанция';
  s2 := 'дисперсия';
  AssertTrue(LevDistanceUtf16(s1, s2) = 4);
  AssertTrue(LevDistanceUtf16(s2, s1) = 4);

  s1 := 'левенштейн';
  s2 := 'франкенштейн';
  AssertTrue(LevDistanceUtf16(s1, s2) = 5);
  AssertTrue(LevDistanceUtf16(s2, s1) = 5);

  s1 := 'ааааааа';
  s2 := 'ааааааа';
  AssertTrue(LevDistanceUtf16(s1, s2) = 0);

  s1 := 'ааааааа';
  s2 := 'бббббббб';
  AssertTrue(LevDistanceUtf16(s1, s2) = 8);
  AssertTrue(LevDistanceUtf16(s2, s1) = 8);

  s1 := 'аааббаааа';
  s2 := 'ааааааа';
  AssertTrue(LevDistanceUtf16(s1, s2) = 2);
  AssertTrue(LevDistanceUtf16(s2, s1) = 2);

  s1 := 'а';
  s2 := 'б';
  AssertTrue(LevDistanceUtf16(s1, s2) = 1);
  AssertTrue(LevDistanceUtf16(s2, s1) = 1);

  s1 := '一天，在寒冷的冬天，我从森林里走出来。';
  s2 := '一，在寒冷的冬天，我从森林里走来。';
  AssertTrue(LevDistanceUtf16(s1, s2) = 2);
  AssertTrue(LevDistanceUtf16(s2, s1) = 2);
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
  AssertTrue(LevDistanceMyersUtf16(s1, s2) = LevDistanceUtf8(s1, s2));
  AssertTrue(LevDistanceMyersUtf16(s3, s4) = LevDistanceUtf8(s3, s4));
  AssertTrue(LevDistanceMyersUtf16(s5, s6) = LevDistanceUtf8(s5, s6));
  AssertTrue(LevDistanceMyersUtf16(s7, s8) = LevDistanceUtf8(s7, s8));
  AssertTrue(LevDistanceMyersUtf16(s9, s10) = LevDistanceUtf8(s9, s10));
  AssertTrue(LevDistanceMyersUtf16(s11, s12) = LevDistanceUtf8(s11, s12));
  AssertTrue(LevDistanceMyersUtf16(s13, s14) = LevDistanceUtf8(s13, s14));
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
  AssertTrue(LevDistanceMyersUtf16(s1, s2) = LevDistanceUtf8(s1, s2));
  AssertTrue(LevDistanceMyersUtf16(s3, s4) = LevDistanceUtf8(s3, s4));
  AssertTrue(LevDistanceMyersUtf16(s5, s6) = LevDistanceUtf8(s5, s6));
  AssertTrue(LevDistanceMyersUtf16(s7, s8) = LevDistanceUtf8(s7, s8));
  AssertTrue(LevDistanceMyersUtf16(s9, s10) = LevDistanceUtf8(s9, s10));
  AssertTrue(LevDistanceMyersUtf16(s11, s12) = LevDistanceUtf8(s11, s12));
  AssertTrue(LevDistanceMyersUtf16(s13, s14) = LevDistanceUtf8(s13, s14));
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
  AssertTrue(LevDistanceMyersUtf16(s1, s2, Pred(LevDistanceUtf8(s1, s2))) = -1);
  AssertTrue(LevDistanceMyersUtf16(s3, s4, Pred(LevDistanceUtf8(s3, s4))) = -1);
  AssertTrue(LevDistanceMyersUtf16(s5, s6, Pred(LevDistanceUtf8(s5, s6))) = -1);
  AssertTrue(LevDistanceMyersUtf16(s7, s8, Pred(LevDistanceUtf8(s7, s8))) = -1);
  AssertTrue(LevDistanceMyersUtf16(s9, s10, Pred(LevDistanceUtf8(s9, s10))) = -1);
  AssertTrue(LevDistanceMyersUtf16(s11, s12, Pred(LevDistanceUtf8(s11, s12))) = -1);
  AssertTrue(LevDistanceMyersUtf16(s13, s14, Pred(LevDistanceUtf8(s13, s14))) = -1);
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
  AssertTrue(LevDistanceMyersUtf16(s1, s2, Pred(LevDistanceUtf16(s1, s2))) = -1);
  AssertTrue(LevDistanceMyersUtf16(s3, s4, Pred(LevDistanceUtf16(s3, s4))) = -1);
  AssertTrue(LevDistanceMyersUtf16(s5, s6, Pred(LevDistanceUtf16(s5, s6))) = -1);
  AssertTrue(LevDistanceMyersUtf16(s7, s8, Pred(LevDistanceUtf16(s7, s8))) = -1);
  AssertTrue(LevDistanceMyersUtf16(s9, s10, Pred(LevDistanceUtf16(s9, s10))) = -1);
  AssertTrue(LevDistanceMyersUtf16(s11, s12, Pred(LevDistanceUtf16(s11, s12))) = -1);
  AssertTrue(LevDistanceMyersUtf16(s13, s14, Pred(LevDistanceUtf16(s13, s14))) = -1);
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
  AssertTrue(LevDistanceMyersUtf16(s1, s2, -1) = LevDistanceUtf16(s1, s2));
  AssertTrue(LevDistanceMyersUtf16(s3, s4, -1) = LevDistanceUtf16(s3, s4));
  AssertTrue(LevDistanceMyersUtf16(s5, s6, -1) = LevDistanceUtf16(s5, s6));
  AssertTrue(LevDistanceMyersUtf16(s7, s8, -1) = LevDistanceUtf16(s7, s8));
  AssertTrue(LevDistanceMyersUtf16(s9, s10, -1) = LevDistanceUtf16(s9, s10));
  AssertTrue(LevDistanceMyersUtf16(s11, s12, -1) = LevDistanceUtf16(s11, s12));
  AssertTrue(LevDistanceMyersUtf16(s13, s14, -1) = LevDistanceUtf16(s13, s14));
end;

procedure TTestUnicodeUtils.LcsDistMyersUtf16;
var
  s1, s2: string;
begin
  s1 := '';
  s2 := 'привет';
  AssertTrue(LcsDistanceMyersUtf16(s1, s1) = 0);
  AssertTrue(LcsDistanceMyersUtf16(s2, s2) = 0);
  AssertTrue(LcsDistanceMyersUtf16(s1, s2) = 6);
  AssertTrue(LcsDistanceMyersUtf16(s2, s1) = 6);

  s1 := 'аб';
  s2 := 'аа';
  AssertTrue(LcsDistanceMyersUtf16(s1, s2) = 2);
  AssertTrue(LcsDistanceMyersUtf16(s2, s1) = 2);

  s1 := 'аб';
  s2 := 'ба';
  AssertTrue(LcsDistanceMyersUtf16(s1, s2) = 2);
  AssertTrue(LcsDistanceMyersUtf16(s2, s1) = 2);

  s1 := 'аб';
  s2 := 'ааа';
  AssertTrue(LcsDistanceMyersUtf16(s1, s2) = 3);
  AssertTrue(LcsDistanceMyersUtf16(s2, s1) = 3);

  s1 := 'а';
  s2 := 'ббб';
  AssertTrue(LcsDistanceMyersUtf16(s1, s2) = 4);
  AssertTrue(LcsDistanceMyersUtf16(s2, s1) = 4);

  s1 := 'аабабаб';
  s2 := 'аббаа';
  AssertTrue(LcsDistanceMyersUtf16(s1, s2) = 4);
  AssertTrue(LcsDistanceMyersUtf16(s2, s1) = 4);

  s1 := 'нелли';
  s2 := 'елли';
  AssertTrue(LcsDistanceMyersUtf16(s1, s2) = 1);
  AssertTrue(LcsDistanceMyersUtf16(s2, s1) = 1);

  s1 := 'нелли';
  s2 := 'еллия';
  AssertTrue(LcsDistanceMyersUtf16(s1, s2) = 2);
  AssertTrue(LcsDistanceMyersUtf16(s2, s1) = 2);

  s1 := 'мостик';
  s2 := 'костик';
  AssertTrue(LcsDistanceMyersUtf16(s1, s2) = 2);
  AssertTrue(LcsDistanceMyersUtf16(s2, s1) = 2);

  s1 := 'мостик';
  s2 := 'костяка';
  AssertTrue(LcsDistanceMyersUtf16(s1, s2) = 5);
  AssertTrue(LcsDistanceMyersUtf16(s2, s1) = 5);

  s1 := 'дистанция';
  s2 := 'дисперсия';
  AssertTrue(LcsDistanceMyersUtf16(s1, s2) = 8);
  AssertTrue(LcsDistanceMyersUtf16(s2, s1) = 8);

  s1 := 'левенштейн';
  s2 := 'франкенштейн';
  AssertTrue(LcsDistanceMyersUtf16(s1, s2) = 8);
  AssertTrue(LcsDistanceMyersUtf16(s2, s1) = 8);

  s1 := 'ааааааа';
  s2 := 'ааааааа';
  AssertTrue(LcsDistanceMyersUtf16(s1, s2) = 0);

  s1 := 'ааааааа';
  s2 := 'бббббббб';
  AssertTrue(LcsDistanceMyersUtf16(s1, s2) = 15);
  AssertTrue(LcsDistanceMyersUtf16(s2, s1) = 15);

  s1 := 'аааббаааа';
  s2 := 'ааааааа';
  AssertTrue(LcsDistanceMyersUtf16(s1, s2) = 2);
  AssertTrue(LcsDistanceMyersUtf16(s2, s1) = 2);

  s1 := 'а';
  s2 := 'б';
  AssertTrue(LcsDistanceMyersUtf16(s1, s2) = 2);
  AssertTrue(LcsDistanceMyersUtf16(s2, s1) = 2);

  s1 := '一天，在寒冷的冬天，我从森林里走出来。';
  s2 := '一，在寒冷的冬天，我从森林里走来。';
  AssertTrue(LcsDistanceMyersUtf16(s1, s2) = 2);
  AssertTrue(LcsDistanceMyersUtf16(s2, s1) = 2);
end;

procedure TTestUnicodeUtils.LcsDistMyersBoundedUtf16;
var
  s1, s2: string;
begin
  s1 := '';
  s2 := 'привет';
  AssertTrue(LcsDistanceMyersUtf16(s1, s2) = 6);
  AssertTrue(LcsDistanceMyersUtf16(s1, s2, 5) = -1);

  s1 := 'аб';
  s2 := 'аа';
  AssertTrue(LcsDistanceMyersUtf16(s1, s2) = 2);
  AssertTrue(LcsDistanceMyersUtf16(s1, s2, 1) = -1);

  s1 := 'аб';
  s2 := 'ба';
  AssertTrue(LcsDistanceMyersUtf16(s1, s2) = 2);
  AssertTrue(LcsDistanceMyersUtf16(s1, s2, 1) = -1);

  s1 := 'аб';
  s2 := 'ааа';
  AssertTrue(LcsDistanceMyersUtf16(s1, s2) = 3);
  AssertTrue(LcsDistanceMyersUtf16(s1, s2, 2) = -1);

  s1 := 'а';
  s2 := 'ббб';
  AssertTrue(LcsDistanceMyersUtf16(s1, s2) = 4);
  AssertTrue(LcsDistanceMyersUtf16(s1, s2, 3) = -1);

  s1 := 'аабабаб';
  s2 := 'аббаа';
  AssertTrue(LcsDistanceMyersUtf16(s1, s2) = 4);
  AssertTrue(LcsDistanceMyersUtf16(s1, s2, 3) = -1);

  s1 := 'нелли';
  s2 := 'елли';
  AssertTrue(LcsDistanceMyersUtf16(s1, s2) = 1);
  AssertTrue(LcsDistanceMyersUtf16(s1, s2, 0) = -1);

  s1 := 'нелли';
  s2 := 'еллия';
  AssertTrue(LcsDistanceMyersUtf16(s1, s2) = 2);
  AssertTrue(LcsDistanceMyersUtf16(s1, s2, 1) = -1);

  s1 := 'мостик';
  s2 := 'костик';
  AssertTrue(LcsDistanceMyersUtf16(s1, s2) = 2);
  AssertTrue(LcsDistanceMyersUtf16(s1, s2, 1) = -1);

  s1 := 'мостик';
  s2 := 'костяка';
  AssertTrue(LcsDistanceMyersUtf16(s1, s2) = 5);
  AssertTrue(LcsDistanceMyersUtf16(s1, s2, 4) = -1);

  s1 := 'дистанция';
  s2 := 'дисперсия';
  AssertTrue(LcsDistanceMyersUtf16(s1, s2) = 8);
  AssertTrue(LcsDistanceMyersUtf16(s1, s2, 7) = -1);

  s1 := 'левенштейн';
  s2 := 'франкенштейн';
  AssertTrue(LcsDistanceMyersUtf16(s1, s2) = 8);
  AssertTrue(LcsDistanceMyersUtf16(s1, s2, 7) = -1);

  s1 := 'ааааааа';
  s2 := 'бббббббб';
  AssertTrue(LcsDistanceMyersUtf16(s1, s2) = 15);
  AssertTrue(LcsDistanceMyersUtf16(s1, s2, 14) = -1);

  s1 := 'аааббаааа';
  s2 := 'ааааааа';
  AssertTrue(LcsDistanceMyersUtf16(s1, s2) = 2);
  AssertTrue(LcsDistanceMyersUtf16(s1, s2, 1) = -1);

  s1 := 'а';
  s2 := 'б';
  AssertTrue(LcsDistanceMyersUtf16(s1, s2) = 2);
  AssertTrue(LcsDistanceMyersUtf16(s1, s2, 1) = -1);

  s1 := '一天，在寒冷的冬天，我从森林里走出来。';
  s2 := '一，在寒冷的冬天，我从森林里走来。';
  AssertTrue(LcsDistanceMyersUtf16(s1, s2) = 2);
  AssertTrue(LcsDistanceMyersUtf16(s2, s1, 1) = -1);
end;

procedure TTestUnicodeUtils.LcsDistMyersDynUtf16;
var
  s1, s2: string;
begin
  s1 := '';
  s2 := 'привет';
  AssertTrue(LcsDistanceMyersUtf16(s1, s1, -1) = 0);
  AssertTrue(LcsDistanceMyersUtf16(s2, s2, -1) = 0);
  AssertTrue(LcsDistanceMyersUtf16(s1, s2, -1) = 6);
  AssertTrue(LcsDistanceMyersUtf16(s2, s1, -1) = 6);

  s1 := 'аб';
  s2 := 'аа';
  AssertTrue(LcsDistanceMyersUtf16(s1, s2, -1) = 2);
  AssertTrue(LcsDistanceMyersUtf16(s2, s1, -1) = 2);

  s1 := 'аб';
  s2 := 'ба';
  AssertTrue(LcsDistanceMyersUtf16(s1, s2, -1) = 2);
  AssertTrue(LcsDistanceMyersUtf16(s2, s1, -1) = 2);

  s1 := 'аб';
  s2 := 'ааа';
  AssertTrue(LcsDistanceMyersUtf16(s1, s2, -1) = 3);
  AssertTrue(LcsDistanceMyersUtf16(s2, s1, -1) = 3);

  s1 := 'а';
  s2 := 'ббб';
  AssertTrue(LcsDistanceMyersUtf16(s1, s2, -1) = 4);
  AssertTrue(LcsDistanceMyersUtf16(s2, s1, -1) = 4);

  s1 := 'аабабаб';
  s2 := 'аббаа';
  AssertTrue(LcsDistanceMyersUtf16(s1, s2, -1) = 4);
  AssertTrue(LcsDistanceMyersUtf16(s2, s1, -1) = 4);

  s1 := 'нелли';
  s2 := 'елли';
  AssertTrue(LcsDistanceMyersUtf16(s1, s2, -1) = 1);
  AssertTrue(LcsDistanceMyersUtf16(s2, s1, -1) = 1);

  s1 := 'нелли';
  s2 := 'еллия';
  AssertTrue(LcsDistanceMyersUtf16(s1, s2, -1) = 2);
  AssertTrue(LcsDistanceMyersUtf16(s2, s1, -1) = 2);

  s1 := 'мостик';
  s2 := 'костик';
  AssertTrue(LcsDistanceMyersUtf16(s1, s2, -1) = 2);
  AssertTrue(LcsDistanceMyersUtf16(s2, s1, -1) = 2);

  s1 := 'мостик';
  s2 := 'костяка';
  AssertTrue(LcsDistanceMyersUtf16(s1, s2, -1) = 5);
  AssertTrue(LcsDistanceMyersUtf16(s2, s1, -1) = 5);

  s1 := 'дистанция';
  s2 := 'дисперсия';
  AssertTrue(LcsDistanceMyersUtf16(s1, s2, -1) = 8);
  AssertTrue(LcsDistanceMyersUtf16(s2, s1, -1) = 8);

  s1 := 'левенштейн';
  s2 := 'франкенштейн';
  AssertTrue(LcsDistanceMyersUtf16(s1, s2, -1) = 8);
  AssertTrue(LcsDistanceMyersUtf16(s2, s1, -1) = 8);

  s1 := 'ааааааа';
  s2 := 'ааааааа';
  AssertTrue(LcsDistanceMyersUtf16(s1, s2, -1) = 0);

  s1 := 'ааааааа';
  s2 := 'бббббббб';
  AssertTrue(LcsDistanceMyersUtf16(s1, s2, -1) = 15);
  AssertTrue(LcsDistanceMyersUtf16(s2, s1, -1) = 15);

  s1 := 'аааббаааа';
  s2 := 'ааааааа';
  AssertTrue(LcsDistanceMyersUtf16(s1, s2, -1) = 2);
  AssertTrue(LcsDistanceMyersUtf16(s2, s1, -1) = 2);

  s1 := 'а';
  s2 := 'б';
  AssertTrue(LcsDistanceMyersUtf16(s1, s2, -1) = 2);
  AssertTrue(LcsDistanceMyersUtf16(s2, s1, -1) = 2);

  s1 := '一天，在寒冷的冬天，我从森林里走出来。';
  s2 := '一，在寒冷的冬天，我从森林里走来。';
  AssertTrue(LcsDistanceMyersUtf16(s1, s2, -1) = 2);
  AssertTrue(LcsDistanceMyersUtf16(s2, s1, -1) = 2);
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

{ TTestSeqUtils }

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
  ac: IACSearchUtf8;
begin
  ac := CreateACSearchFsm([]);
  AssertTrue(ac.PatternCount = 0);
  AssertFalse(ac.ContainsPattern('a'));

  ac := CreateACSearchFsm(['a','b']);
  AssertFalse(ac.CaseInsensitive);
  AssertTrue(ac.PatternCount = 2);
  AssertTrue(ac.ContainsPattern('a'));
  AssertTrue(ac.ContainsPattern('b'));

  ac := CreateACSearchFsm(['a','b'], True);
  AssertTrue(ac.CaseInsensitive);
  AssertTrue(ac.PatternCount = 2);
  AssertTrue(ac.ContainsPattern('A'));
  AssertTrue(ac.ContainsPattern('B'));

  ac := CreateACSearchFsm(['a','b'], False, True);
  AssertFalse(ac.CaseInsensitive);
  AssertTrue(ac.PatternCount = 2);
  AssertTrue(ac.ContainsPattern('a'));
  AssertTrue(ac.ContainsPattern('b'));

  ac := CreateACSearchFsm(['a','b'], True, True);
  AssertTrue(ac.CaseInsensitive);
  AssertTrue(ac.PatternCount = 2);
  AssertTrue(ac.ContainsPattern('A'));
  AssertTrue(ac.ContainsPattern('B'));
end;

procedure TACSearchFsmTest.TestSearchDelegatedDfa;
var
  ac: IACSearchUtf8;
  a: TStringArray;
  s: string;
begin
  a := ['her', 'is', 'his', 'she', 'hi', 'he', 'i', 'hero'];
  ac := CreateACSearchFsm(a);
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
  ac: IACSearchUtf8;
  a: TStringArray;
  s: string;
begin
  a := ['her', 'is', 'his', 'she', 'hi', 'he', 'i', 'hero'];
  ac := CreateACSearchFsm(a, False, True);
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
  ac: IACSearchUtf8;
  a: TStringArray;
  s: string;
begin
  a := ['her', 'is', 'his', 'she', 'hi', 'he', 'i', 'hero'];
  ac := CreateACSearchFsm(a, True);
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
  ac: IACSearchUtf8;
  a: TStringArray;
  s: string;
begin
  a := ['her', 'is', 'his', 'she', 'hi', 'he', 'i', 'hero'];
  ac := CreateACSearchFsm(a, True, True);
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
  ac: IACSearchUtf8;
  a: TStringArray;
begin
  a := ['her', 'is', 'his', 'she', 'hi', 'he', 'i', 'hero'];
  ac := CreateACSearchFsm(a);
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
  ac: IACSearchUtf8;
  a: TStringArray;
begin
  a := ['her', 'is', 'his', 'she', 'hi', 'he', 'i', 'hero'];
  ac := CreateACSearchFsm(a, False, True);
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
  ac: IACSearchUtf8;
  a: TStringArray;
  I: Integer;
begin
  a := ['HER', 'İS', 'HİS', 'SHE', 'Hİ', 'HE', 'İ', 'HERO'];
  ac := CreateACSearchFsm(a, True);
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
  ac: IACSearchUtf8;
  a: TStringArray;
  I: Integer;
begin
  a := ['HER', 'İS', 'HİS', 'SHE', 'Hİ', 'HE', 'İ', 'HERO'];
  ac := CreateACSearchFsm(a, True, True);
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

procedure TACSearchFsmTest.TestContainsMatchDfa;
var
  ac: IACSearchUtf8;
begin
  ac := CreateACSearchFsm(['aa','bb']);
  AssertFalse(ac.ContainsMatch('abcde'));
  AssertFalse(ac.ContainsMatch('ababcde'));
  AssertTrue(ac.ContainsMatch('bcdaae'));
  AssertTrue(ac.ContainsMatch('acdbbe'));
end;

procedure TACSearchFsmTest.TestContainsMatchNfa;
var
  ac: IACSearchUtf8;
begin
  ac := CreateACSearchFsm(['aa','bb'], False, True);
  AssertFalse(ac.ContainsMatch('abcde'));
  AssertFalse(ac.ContainsMatch('ababcde'));
  AssertTrue(ac.ContainsMatch('bcdaae'));
  AssertTrue(ac.ContainsMatch('acdbbe'));
end;

procedure TACSearchFsmTest.TestContainsMatchDfaCI;
var
  ac: IACSearchUtf8;
begin
  ac := CreateACSearchFsm(['aA','Bb'], True);
  AssertFalse(ac.ContainsMatch('abcde'));
  AssertFalse(ac.ContainsMatch('ababcde'));
  AssertTrue(ac.ContainsMatch('bcdaae'));
  AssertTrue(ac.ContainsMatch('acdbbe'));
end;

procedure TACSearchFsmTest.TestContainsMatchNfaCI;
var
  ac: IACSearchUtf8;
begin
  ac := CreateACSearchFsm(['aA','Bb'], True, True);
  AssertFalse(ac.ContainsMatch('abcde'));
  AssertFalse(ac.ContainsMatch('ababcde'));
  AssertTrue(ac.ContainsMatch('bcdaae'));
  AssertTrue(ac.ContainsMatch('acdbbe'));
end;


initialization

  RegisterTest(TTestUnicodeUtils);
  RegisterTest(TTestSeqUtils);
  RegisterTest(TTestFuzzySearchBitap);
  RegisterTest(TACSearchFsmTest);

end.

