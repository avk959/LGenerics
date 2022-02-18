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



initialization

  RegisterTest(TTestUtf8Utils);

end.

