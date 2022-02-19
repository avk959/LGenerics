unit lgSeqUtilsTest;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, lgSeqUtils;

type

  { TTestUtf8Utils }

  TTestUtf8Utils= class(TTestCase)
  published
    procedure ValidateDfa;
    procedure Validate;
    procedure LevenshteinDist;
    procedure LevenshteinDistMbr;
    procedure LevenshteinDistMbrBounded;
    procedure LcsDistMyers;
    procedure LcsDistMyersBounded;
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

procedure TTestUtf8Utils.ValidateDfa;
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

procedure TTestUtf8Utils.Validate;
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

procedure TTestUtf8Utils.LevenshteinDist;
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

procedure TTestUtf8Utils.LevenshteinDistMbr;
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

procedure TTestUtf8Utils.LevenshteinDistMbrBounded;
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

procedure TTestUtf8Utils.LcsDistMyers;
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

procedure TTestUtf8Utils.LcsDistMyersBounded;
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



initialization

  RegisterTest(TTestUtf8Utils);

end.

