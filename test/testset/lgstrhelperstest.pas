unit LGStrHelpersTest;

{$mode objfpc}{$H+}{$modeswitch advancedrecords}{$modeswitch nestedprocvars}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, Math,
  lgUtils,
  lgArrayHelpers,
  lgHash,
  lgHashSet,
  lgStrHelpers;

type
  THelper    = specialize TGComparableArrayHelper<SizeInt>;
  string1251 = type ansistring(1251);

  { TBmSearchTest }

  TBmSearchTest = class(TTestCase)
  published
    procedure FindMatches;
    procedure FindMatchesBytes;
    procedure Matches;
    procedure MatchesBytes;
    procedure NextMatch;
    procedure NextMatchBytes;
  end;

  { TBmhrSearchTest }

  TBmhrSearchTest = class(TTestCase)
  published
    procedure FindMatches;
    procedure FindMatchesBytes;
    procedure Matches;
    procedure MatchesBytes;
    procedure NextMatch;
    procedure NextMatchBytes;
  end;

  { TBmSearchCITest }
  TBmSearchCITest = class(TTestCase)
  published
    procedure FindMatches;
    procedure FindMatches1251;
    procedure Matches;
    procedure NextMatch;
  end;

 { TFunTest }

 TFunTest = class(TTestCase)
 published
   procedure LevenshteinDist;
   procedure LevenshteinDist2;
   procedure LevenshteinDistMbr;
   procedure LevenshteinDistMbr2;
   procedure LevenshteinDistMbrBounded;
   procedure LevenshteinDistMbrDyn;
   procedure LevenshteinDistMyersD;
   procedure LevenshteinDistMyersD2;
   procedure LevenshteinDistMyersQ;
   procedure LevenshteinDistMyersDQ;
   procedure LevenshteinDistMyersLong;
   procedure LevenshteinDistMyersDBounded;
   procedure LevenshteinDistMyersQBounded;
   procedure LevenshteinDistMyersDQBounded;
   procedure LevenshteinDistMyersLongBounded;
   procedure LevenshteinDistMyersDBounded2;
   procedure LevenshteinDistMyersQBounded2;
   procedure LevenshteinDistMyersDQBounded2;
   procedure LevenshteinDistMyersLongBounded2;
   procedure LcsGusTest;
   procedure LcsKRTest;
   procedure LcsMyersTest;
   procedure SimRatioTest;
   procedure SimRatioLevExTest;
   procedure SimRatioLevExTest1251;
   procedure LcsDistMyers;
   procedure LcsDistMyers2;
   procedure LcsDistMyersBounded;
   procedure LcsDistMyersDyn;
   procedure DamerauDistMbr;
   procedure DamerauDistMbrBounded;
   procedure DamerauDistMbrDyn;
 end;

 { TACSearchFsmTest }

 TACSearchFsmTest = class(TTestCase)
 private
 type
   TACSearch = specialize TGUniqRef<TACSearchFsm>;
   TMatch    = TACSearchFsm.TMatch;

   TRbsHasher = record
     class function Equal(const L, R: rawbytestring): Boolean; static;
     class function HashCode(const s: rawbytestring): SizeInt; static;
   end;

   TStrSetType = specialize TGLiteChainHashSet<rawbytestring, TRbsHasher>;
   TStrSet     = TStrSetType.TSet;
   TRbsHelper  = specialize TGComparableArrayHelper<rawbytestring>;
 var
   FMatchCount: Integer;
   function OnMatch(const m: TMatch): Boolean;
   function OnMatchFirst(const m: TMatch): Boolean;
 published
   procedure TestCreate;
   procedure TestSearchDelegated;
   procedure TestSearchNested;
   procedure TestContainsMatch;
   procedure TestIndexOfPattern;
   procedure TestFirstMatch;
   procedure TestFindMatches;
 end;

implementation

procedure TBmSearchTest.FindMatches;
var
  m: TBmSearch;
  a: array of SizeInt = nil;
const
  Text1 = 'adcenfak';
  Text2 = 'abcbbaab';
  Text3 = 'abcabcabcabcabcabcab';
begin
  m := TBmSearch.Create('');
  a := m.FindMatches(Text1);
  AssertTrue(a = nil);
  m := TBmSearch.Create('b');
  a := m.FindMatches('');
  AssertTrue(a = nil);
  a := m.FindMatches(Text1);
  AssertTrue(a = nil);
  a := m.FindMatches(Text2);
  AssertTrue(THelper.Same(a, [2, 4, 5, 8]));
  m := TBmSearch.Create('ab');
  a := m.FindMatches(Text1);
  AssertTrue(a = nil);
  a := m.FindMatches(Text2);
  AssertTrue(THelper.Same(a, [1, 7]));
  m := TBmSearch.Create('bba');
  a := m.FindMatches(Text2);
  AssertTrue(THelper.Same(a, [4]));
  m := TBmSearch.Create('abcab');
  a := m.FindMatches(Text3);
  AssertTrue(THelper.Same(a, [1, 4, 7, 10, 13, 16]));
end;

procedure TBmSearchTest.FindMatchesBytes;
var
  m: TBmSearch;
  a: array of SizeInt = nil;
const
  Seq1: array of Byte = (1,2,3,4,5,6);
  Seq2: array of Byte = (2,7,7,5,2,7);
  Seq3: array of Byte = (1,2,3,1,2,3,1,2,3,1,2,3,1,2,3,1,2,3,1,2);
begin
  m := TBmSearch.Create([]);
  a := m.FindMatches(Seq1);
  AssertTrue(a = nil);
  m := TBmSearch.Create([7]);
  a := m.FindMatches([]);
  AssertTrue(a = nil);
  a := m.FindMatches(Seq1);
  AssertTrue(a = nil);
  a := m.FindMatches(Seq2);
  AssertTrue(THelper.Same(a, [1, 2, 5]));
  m := TBmSearch.Create([2, 7]);
  a := m.FindMatches(Seq1);
  AssertTrue(a = nil);
  a := m.FindMatches(Seq2);
  AssertTrue(THelper.Same(a, [0, 4]));
  m := TBmSearch.Create([5,2,7]);
  a := m.FindMatches(Seq2);
  AssertTrue(THelper.Same(a, [3]));
  m := TBmSearch.Create([1,2,3,1,2]);
  a := m.FindMatches(Seq3);
  AssertTrue(THelper.Same(a, [0, 3, 6, 9, 12, 15]));
end;

procedure TBmSearchTest.Matches;
var
  m: TBmSearch;
  I, Count: SizeInt;
  s: set of Byte;
const
  Text = 'abcabcabcabcabcabcab';
begin
  m := TBmSearch.Create('');
  Count := 0;
  for I in m.Matches('') do
    Inc(Count);
  AssertTrue(Count = 0);
  for I in m.Matches(Text) do
    Inc(Count);
  AssertTrue(Count = 0);
  m := TBmSearch.Create('abcab');
  s := [1, 4, 7, 10, 13, 16];
  for I in m.Matches(Text) do
    begin
      AssertTrue(Byte(I) in s);
      Inc(Count);
    end;
  AssertTrue(Count = 6);
end;

procedure TBmSearchTest.MatchesBytes;
var
  m: TBmSearch;
  I, Count: SizeInt;
  s: set of Byte;
const
  Seq: array of Byte = (1,2,3,1,2,3,1,2,3,1,2,3,1,2,3,1,2,3,1,2);
begin
  m := TBmSearch.Create([]);
  Count := 0;
  for I in m.Matches([]) do
    Inc(Count);
  AssertTrue(Count = 0);
  for I in m.Matches(Seq) do
    Inc(Count);
  AssertTrue(Count = 0);
  m := TBmSearch.Create([1,2,3,1,2]);
  s := [0, 3, 6, 9, 12, 15];
  for I in m.Matches(Seq) do
    begin
      AssertTrue(Byte(I) in s);
      Inc(Count);
    end;
  AssertTrue(Count = 6);
end;

procedure TBmSearchTest.NextMatch;
var
  m: TBmSearch;
  I, Count: SizeInt;
  s: set of Byte;
const
  Text = 'abcabcabcabcabcabcab';
begin
  m := TBmSearch.Create('');
  Count := 0;
  I := 1;
  repeat
    I := m.NextMatch('', I);
    if I <> 0 then
      begin
        Inc(Count);
        Inc(I);
      end;
  until I = 0;
  AssertTrue(Count = 0);

  I := 1;
  repeat
    I := m.NextMatch(Text, I);
    if I <> 0 then
      begin
        Inc(Count);
        Inc(I);
      end;
  until I = 0;
  AssertTrue(Count = 0);

  m := TBmSearch.Create('abcab');
  s := [1, 4, 7, 10, 13, 16];
  I := 1;
  repeat
    I := m.NextMatch(Text, I);
    if I <> 0 then
      begin
        AssertTrue(Byte(I) in s);
        Inc(Count);
        Inc(I);
      end;
  until I = 0;
  AssertTrue(Count = 6);
end;

procedure TBmSearchTest.NextMatchBytes;
var
  m: TBmSearch;
  I, Count: SizeInt;
  s: set of Byte;
const
  Seq: array of Byte = (1,2,3,1,2,3,1,2,3,1,2,3,1,2,3,1,2,3,1,2);
begin
  m := TBmSearch.Create([]);
  Count := 0;
  I := 0;
  repeat
    I := m.NextMatch([], I);
    if I <> -1 then
      begin
        Inc(Count);
        Inc(I);
      end;
  until I = -1;
  AssertTrue(Count = 0);

  I := 0;
  repeat
    I := m.NextMatch(Seq, I);
    if I <> -1 then
      begin
        Inc(Count);
        Inc(I);
      end;
  until I = -1;
  AssertTrue(Count = 0);

  m := TBmSearch.Create([1,2,3,1,2]);
  s := [0, 3, 6, 9, 12, 15];
  I := 0;
  repeat
    I := m.NextMatch(Seq, I);
    if I <> -1 then
      begin
        AssertTrue(Byte(I) in s);
        Inc(Count);
        Inc(I);
      end;
  until I = -1;
  AssertTrue(Count = 6);
end;

{ TBmhrSearchTest }

procedure TBmhrSearchTest.FindMatches;
var
  m: TBmhrSearch;
  a: array of SizeInt = nil;
const
  Text1 = 'adcenfak';
  Text2 = 'abcbbaab';
  Text3 = 'abcabcabcabcabcabcab';
begin
  m := TBmhrSearch.Create('');
  a := m.FindMatches(Text1);
  AssertTrue(a = nil);
  m := TBmhrSearch.Create('b');
  a := m.FindMatches('');
  AssertTrue(a = nil);
  a := m.FindMatches(Text1);
  AssertTrue(a = nil);
  a := m.FindMatches(Text2);
  AssertTrue(THelper.Same(a, [2, 4, 5, 8]));
  m := TBmhrSearch.Create('ab');
  a := m.FindMatches(Text1);
  AssertTrue(a = nil);
  a := m.FindMatches(Text2);
  AssertTrue(THelper.Same(a, [1, 7]));
  m := TBmhrSearch.Create('bba');
  a := m.FindMatches(Text2);
  AssertTrue(THelper.Same(a, [4]));
  m := TBmhrSearch.Create('abcab');
  a := m.FindMatches(Text3);
  AssertTrue(THelper.Same(a, [1, 4, 7, 10, 13, 16]));
end;

procedure TBmhrSearchTest.FindMatchesBytes;
var
  m: TBmhrSearch;
  a: array of SizeInt = nil;
const
  Seq1: array of Byte = (1,2,3,4,5,6);
  Seq2: array of Byte = (2,7,7,5,2,7);
  Seq3: array of Byte = (1,2,3,1,2,3,1,2,3,1,2,3,1,2,3,1,2,3,1,2);
begin
  m := TBmhrSearch.Create([]);
  a := m.FindMatches(Seq1);
  AssertTrue(a = nil);
  m := TBmhrSearch.Create([7]);
  a := m.FindMatches([]);
  AssertTrue(a = nil);
  a := m.FindMatches(Seq1);
  AssertTrue(a = nil);
  a := m.FindMatches(Seq2);
  AssertTrue(THelper.Same(a, [1, 2, 5]));
  m := TBmhrSearch.Create([2, 7]);
  a := m.FindMatches(Seq1);
  AssertTrue(a = nil);
  a := m.FindMatches(Seq2);
  AssertTrue(THelper.Same(a, [0, 4]));
  m := TBmhrSearch.Create([5,2,7]);
  a := m.FindMatches(Seq2);
  AssertTrue(THelper.Same(a, [3]));
  m := TBmhrSearch.Create([1,2,3,1,2]);
  a := m.FindMatches(Seq3);
  AssertTrue(THelper.Same(a, [0, 3, 6, 9, 12, 15]));
end;

procedure TBmhrSearchTest.Matches;
var
  m: TBmhrSearch;
  I, Count: SizeInt;
  s: set of Byte;
const
  Text = 'abcabcabcabcabcabcab';
begin
  m := TBmhrSearch.Create('');
  Count := 0;
  for I in m.Matches('') do
    Inc(Count);
  AssertTrue(Count = 0);
  for I in m.Matches(Text) do
    Inc(Count);
  AssertTrue(Count = 0);
  m := TBmhrSearch.Create('abcab');
  s := [1, 4, 7, 10, 13, 16];
  for I in m.Matches(Text) do
    begin
      AssertTrue(Byte(I) in s);
      Inc(Count);
    end;
  AssertTrue(Count = 6);
end;

procedure TBmhrSearchTest.MatchesBytes;
var
  m: TBmhrSearch;
  I, Count: SizeInt;
  s: set of Byte;
const
  Seq: array of Byte = (1,2,3,1,2,3,1,2,3,1,2,3,1,2,3,1,2,3,1,2);
begin
  m := TBmhrSearch.Create([]);
  Count := 0;
  for I in m.Matches([]) do
    Inc(Count);
  AssertTrue(Count = 0);
  for I in m.Matches(Seq) do
    Inc(Count);
  AssertTrue(Count = 0);
  m := TBmhrSearch.Create([1,2,3,1,2]);
  s := [0, 3, 6, 9, 12, 15];
  for I in m.Matches(Seq) do
    begin
      AssertTrue(Byte(I) in s);
      Inc(Count);
    end;
  AssertTrue(Count = 6);
end;

procedure TBmhrSearchTest.NextMatch;
var
  m: TBmhrSearch;
  I, Count: SizeInt;
  s: set of Byte;
const
  Text = 'abcabcabcabcabcabcab';
begin
  m := TBmhrSearch.Create('');
  Count := 0;
  I := 1;
  repeat
    I := m.NextMatch('', I);
    if I <> 0 then
      begin
        Inc(Count);
        Inc(I);
      end;
  until I = 0;
  AssertTrue(Count = 0);

  I := 1;
  repeat
    I := m.NextMatch(Text, I);
    if I <> 0 then
      begin
        Inc(Count);
        Inc(I);
      end;
  until I = 0;
  AssertTrue(Count = 0);

  m := TBmhrSearch.Create('abcab');
  s := [1, 4, 7, 10, 13, 16];
  I := 1;
  repeat
    I := m.NextMatch(Text, I);
    if I <> 0 then
      begin
        AssertTrue(Byte(I) in s);
        Inc(Count);
        Inc(I);
      end;
  until I = 0;
  AssertTrue(Count = 6);
end;

procedure TBmhrSearchTest.NextMatchBytes;
var
  m: TBmhrSearch;
  I, Count: SizeInt;
  s: set of Byte;
const
  Seq: array of Byte = (1,2,3,1,2,3,1,2,3,1,2,3,1,2,3,1,2,3,1,2);
begin
  m := TBmhrSearch.Create([]);
  Count := 0;
  I := 0;
  repeat
    I := m.NextMatch([], I);
    if I <> -1 then
      begin
        Inc(Count);
        Inc(I);
      end;
  until I = -1;
  AssertTrue(Count = 0);

  I := 0;
  repeat
    I := m.NextMatch(Seq, I);
    if I <> -1 then
      begin
        Inc(Count);
        Inc(I);
      end;
  until I = -1;
  AssertTrue(Count = 0);

  m := TBmhrSearch.Create([1,2,3,1,2]);
  s := [0, 3, 6, 9, 12, 15];
  I := 0;
  repeat
    I := m.NextMatch(Seq, I);
    if I <> -1 then
      begin
        AssertTrue(Byte(I) in s);
        Inc(Count);
        Inc(I);
      end;
  until I = -1;
  AssertTrue(Count = 6);
end;

{ TBmSearchCITest }

procedure TBmSearchCITest.FindMatches;
var
  m: TBmSearchCI;
  a: array of SizeInt = nil;
const
  Text1 = 'adcenfak';
  Text2 = 'abcbbaab';
  Text3 = 'abcabcabcabcabcabcab';
begin
  m := TBmSearchCI.Create('');
  a := m.FindMatches(Text1);
  AssertTrue(a = nil);
  m := TBmSearchCI.Create('B');
  a := m.FindMatches('');
  AssertTrue(a = nil);
  a := m.FindMatches(Text1);
  AssertTrue(a = nil);
  a := m.FindMatches(Text2);
  AssertTrue(THelper.Same(a, [2, 4, 5, 8]));
  m := TBmSearchCI.Create('Ab');
  a := m.FindMatches(Text1);
  AssertTrue(a = nil);
  a := m.FindMatches(Text2);
  AssertTrue(THelper.Same(a, [1, 7]));
  m := TBmSearchCI.Create('BBA');
  a := m.FindMatches(Text2);
  AssertTrue(THelper.Same(a, [4]));
  m := TBmSearchCI.Create('aBCAb');
  a := m.FindMatches(Text3);
  AssertTrue(THelper.Same(a, [1, 4, 7, 10, 13, 16]));
end;

const
  Win1251LoMap: array[Char] of Char = (
    #$00,#$01,#$02,#$03,#$04,#$05,#$06,#$07,#$08,#$09,#$0a,#$0b,#$0c,#$0d,#$0e,#$0f,
    #$10,#$11,#$12,#$13,#$14,#$15,#$16,#$17,#$18,#$19,#$1a,#$1b,#$1c,#$1d,#$1e,#$1f,
    #$20,#$21,#$22,#$23,#$24,#$25,#$26,#$27,#$28,#$29,#$2a,#$2b,#$2c,#$2d,#$2e,#$2f,
    #$30,#$31,#$32,#$33,#$34,#$35,#$36,#$37,#$38,#$39,#$3a,#$3b,#$3c,#$3d,#$3e,#$3f,
  // 64    A    B    C    D    E    F    G    H    I    J    K    L    M   N     O
    #$40,#$61,#$62,#$63,#$64,#$65,#$66,#$67,#$68,#$69,#$6a,#$6b,#$6c,#$6d,#$6e,#$6f,
  //  P    Q    R    S    T    U    V    W    X    Y    Z
    #$70,#$71,#$72,#$73,#$74,#$75,#$76,#$77,#$78,#$79,#$7a,#$5b,#$5c,#$5d,#$5e,#$5f,
  //       a    b    c    d    e    f    g    h    i    j    k    l    m    n    o
    #$60,#$61,#$62,#$63,#$64,#$65,#$66,#$67,#$68,#$69,#$6a,#$6b,#$6c,#$6d,#$6e,#$6f,
  //  p    q    r    s    t    u    v    w    x    y    z
    #$70,#$71,#$72,#$73,#$74,#$75,#$76,#$77,#$78,#$79,#$7a,#$7b,#$7c,#$7d,#$7e,#$7f,
    #$90,#$83,#$82,#$83,#$84,#$85,#$86,#$87,#$88,#$89,#$9a,#$8b,#$9c,#$9d,#$9e,#$9f,
    #$90,#$91,#$92,#$93,#$94,#$95,#$96,#$97,#$98,#$99,#$9a,#$9b,#$9c,#$9d,#$9e,#$9f,
  //                                          Ё
    #$a0,#$a2,#$a2,#$a3,#$a4,#$b4,#$a6,#$a7,#$b8,#$a9,#$aa,#$ab,#$ac,#$ad,#$ae,#$bf,
  //                                          ё
    #$b0,#$b1,#$b2,#$b3,#$b4,#$b5,#$b6,#$b7,#$b8,#$b9,#$ba,#$bb,#$bc,#$bd,#$be,#$bf,
  //  А    Б    В    Г    Д    Е    Ж    З    И    Й    К    Л    М    Н    О    П
    #$e0,#$e1,#$e2,#$e3,#$e4,#$e5,#$e6,#$e7,#$e8,#$e9,#$ea,#$eb,#$ec,#$ed,#$ee,#$ef,
  //  Р    С    Т    У    Ф    Х    Ц    Ч    Ш    Щ    Ъ    Ы    Ь    Э    Ю    Я
    #$f0,#$f1,#$f2,#$f3,#$f4,#$f5,#$f6,#$f7,#$f8,#$f9,#$fa,#$fb,#$fc,#$fd,#$fe,#$ff,
  //  а    б    в    г    д    е    ж    з    и    й    к    л    м    н    о    п
    #$e0,#$e1,#$e2,#$e3,#$e4,#$e5,#$e6,#$e7,#$e8,#$e9,#$ea,#$eb,#$ec,#$ed,#$ee,#$ef,
  //  р    с    т    у    ф    х    ц    ч    ш    щ    ъ    ы    ь    э    ю    я
    #$f0,#$f1,#$f2,#$f3,#$f4,#$f5,#$f6,#$f7,#$f8,#$f9,#$fa,#$fb,#$fc,#$fd,#$fe,#$ff);

function ToLower1251(c: Char): Char;
begin
  case c of
    #$a8:       Result := #$b8;
    #$c0..#$df: Result := Chr(Ord(c) + 32);
  else
    Result := LowerCase(c);
  end;
end;

procedure TBmSearchCITest.FindMatches1251;
var
  m: TBmSearchCI;
  I: SizeInt;
  a: array of SizeInt = nil;
  Text, Pattern, Pattern2: string1251;
  Tbl: TCaseMapTable;
const
  Text1: string = 'Мой дядя самых честных правил, Когда не в шутку занемог, Он уважать себя заставил И лучше выдумать не мог. Его пример другим наука; Но, боже мой, какая скука...';
  Pat1: string = 'МоГ';
  Pat2: string = 'кОгдА';
begin
  Text := Text1;
  Pattern := Pat1;
  Pattern2 := Pat2;
  AssertTrue(Length(Text) = 160);
  AssertTrue(Length(Pattern) = 3);
  AssertTrue(Length(Pattern2) = 5);

  m := TBmSearchCI.Create(Pattern, @ToLower1251);
  a := m.FindMatches(Text);
  AssertTrue(Length(a) = 2);
  AssertTrue(a[0] = 53);
  AssertTrue(a[1] = 103);

  m.Update(Pattern2);
  a := m.FindMatches(Text);
  AssertTrue(Length(a) = 1);
  AssertTrue(a[0] = 32);

  for I := 0 to High(Tbl) do
    Tbl[I] := Ord(Win1251LoMap[Chr(I)]);

  m := TBmSearchCI.Create(Pattern, Tbl);
  a := m.FindMatches(Text);
  AssertTrue(Length(a) = 2);
  AssertTrue(a[0] = 53);
  AssertTrue(a[1] = 103);

  m.Update(Pattern2);
  a := m.FindMatches(Text);
  AssertTrue(Length(a) = 1);
  AssertTrue(a[0] = 32);
end;

procedure TBmSearchCITest.Matches;
var
  m: TBmSearchCI;
  I, Count: SizeInt;
  s: set of Byte;
const
  Text = 'abcabcabcabcabcabcab';
begin
  m := TBmSearchCI.Create('');
  Count := 0;
  for I in m.Matches('') do
    Inc(Count);
  AssertTrue(Count = 0);
  for I in m.Matches(Text) do
    Inc(Count);
  AssertTrue(Count = 0);
  m := TBmSearchCI.Create('ABCAB');
  s := [1, 4, 7, 10, 13, 16];
  for I in m.Matches(Text) do
    begin
      AssertTrue(Byte(I) in s);
      Inc(Count);
    end;
  AssertTrue(Count = 6);
end;

procedure TBmSearchCITest.NextMatch;
var
  m: TBmSearchCI;
  I, Count: SizeInt;
  s: set of Byte;
const
  Text = 'aBcabcABCABcabcabcaB';
begin
  m := TBmSearchCI.Create('');
  Count := 0;
  I := 1;
  repeat
    I := m.NextMatch('', I);
    if I <> 0 then
      begin
        Inc(Count);
        Inc(I);
      end;
  until I = 0;
  AssertTrue(Count = 0);

  I := 1;
  repeat
    I := m.NextMatch(Text, I);
    if I <> 0 then
      begin
        Inc(Count);
        Inc(I);
      end;
  until I = 0;
  AssertTrue(Count = 0);

  m := TBmSearchCI.Create('AbCab');
  s := [1, 4, 7, 10, 13, 16];
  I := 1;
  repeat
    I := m.NextMatch(Text, I);
    if I <> 0 then
      begin
        AssertTrue(Byte(I) in s);
        Inc(Count);
        Inc(I);
      end;
  until I = 0;
  AssertTrue(Count = 6);
end;

{ the Wagner–Fischer dynamic programming algorithm }
function LevDistWF(const L, R: rawbytestring): SizeInt;
var
  D: array of array of SizeInt;
  I, J: SizeInt;
begin
  SetLength(D, Succ(Length(L)), Succ(Length(R)));
  for I := 1 to Length(L) do
    D[I, 0] := I;
  for I := 1 to Length(R) do
    D[0, I] := I;
  for J := 1 to Length(R) do
    for I := 1 to Length(L) do
      if L[I] = R[J] then
        D[I, J] := MinOf3(D[I-1, J] + 1, D[I, J-1] + 1, D[I-1, J-1])
      else
        D[I, J] := MinOf3(D[I-1, J] + 1, D[I, J-1] + 1, D[I-1, J-1] + 1);
  Result := D[Length(L), Length(R)];
end;

{ simple dynamic programming algorithm }
function LcsDp(const L, R: rawbytestring): rawbytestring;
var
  D: array of array of SizeInt;
  I, J: SizeInt;
  LcsLen: SizeInt = 0;
  Lcs: rawbytestring = '';
  pLcs: PChar;
begin

  SetLength(D, Succ(Length(L)), Succ(Length(R)));

  for I := 1 to Length(L) do
    for J := 1 to Length(R) do
      if L[I] = R[J] then
        D[I, J] := Succ(D[I-1, J-1])
      else
        D[I, J] := Max(D[I, J-1], D[I-1, J]);

  SetLength(Lcs, D[Length(L), Length(R)]);
  pLcs := Pointer(Lcs);

  I := Length(L);
  J := Length(R);
  while (I <> 0) and (J <> 0) do
    if L[I] = R[J] then
      begin
        pLcs[LcsLen] := L[I];
        Inc(LcsLen);
        Dec(I);
        Dec(J);
      end
    else
      if D[I, J-1] > D[I-1, J] then
        Dec(J)
      else
        Dec(I);

  specialize TGSimpleArrayHelper<Char>.Reverse(pLcs[0..Pred(LcsLen)]);
  Result := Lcs;
end;

const
  TestWords: array of rawbytestring = (
    'cetology', 'conchotome',
    'trimetallic', 'upladder',
    'chromatophil', 'archimandrite',
    'overrife', 'recurvant',
    'sasanqua', 'spondylosis',
    'torpescence', 'coappearance',
    'starcher', 'strongback',
    'interception', 'flirtatiousness',
    'fathomable', 'boulder',
    'charioted', 'montgolfier',
    'pharyngoscopy', 'trifoliate',
    'endogenesis', 'letterin',
    'turgidly', 'undoubtingness',
    'pyrheliometry', 'doctrine',
    'apophantic', 'mediglacial',
    'transcorporate', 'stentorious',
    'ingenerately', 'breathseller',
    'viviparousness', 'secluding',
    'benefice', 'gonadic',
    'scripture', 'internalization',
    'bivariate', 'eatanswill',
    'martinet', 'dummered',
    'pretense', 'expurgate',
    'teredinidae', 'strial',
    'photoma', 'bellyland',
    'verbalize', 'deforcement',
    'didynamic', 'Monumbo',
    'galliard', 'conchotome',
    'swashway', 'coupleteer',
    'miscurvature', 'brokenly',
    'preconfer', 'sliptopped',
    'lenitude', 'graupel',
    'pyralidan', 'unverminous',
    'pentaglossal', 'matrimonial',
    'uncasketed', 'cowbane',
    'extemporaneity', 'bedamp',
    'syriologist', 'esophagogastroscopy',
    'inaffectation', 'overassess',
    'dareful', 'demihag',
    'commence', 'bilaminate',
    'petaurine', 'hyperdolichocephaly',
    'restproof', 'unqueenly',
    'blattodea', 'retreative',
    'unframably', 'tentwort',
    'torotoro', 'peroxy',
    'overpatriotic', 'metaplasis',
    'eradicant', 'soever',
    'cradlesong', 'unbleached',
    'attache', 'nonsubject',
    'unparadise', 'morpheme',
    'hierocratic', 'trophothylax',
    'battlemented', 'arachin',
    'lacrym', 'nonevil',
    'unmutilated', 'overemptiness',
    'tenaille', 'accommodative',
    'hygieist', 'overfloat',
    'caulicule', 'prebaptismal',
    'subdeacon', 'chrysalidian',
    'photomechanical', 'microrhabdus',
    'platymyoid', 'hyraceum',
    'preomission', 'bicellular',
    'southernism', 'Semnopithecinae',
    'mangel', 'acidproof',
    'coryphaenoid', 'tidiable',
    'transisthmian', 'Tarkani',
    'subterjacent', 'betanglement',
    'keelfat', 'overbrood',
    'inseparable', 'jhool',
    'gudesake', 'untilled',
    'centetid', 'hypopteral',
    'chirotherian', 'biscuitmaking',
    'wapper', 'hypergol',
    'yawn', 'sluggardness',
    'sterrinck', 'swanny',
    'obtriangular', 'karyogamic',
    'odontexesis', 'aspergillum',
    'exonerative', 'fluorography',
    'shudder', 'spyism',
    'stachyose', 'transvaal',
    'trapfall', 'tillable',
    'plasmocytoma', 'international',
    'ungashed', 'cylindrenchyma',
    'despairer', 'Madreporaria',
    'subsample', 'odoom',
    'scuffer', 'anilinophilous',
    'clamshell', 'lyceal',
    'overtruthfully', 'emeraude',
    'plagiocephalism', 'atechnical',
    'niota', 'mouthbreeder',
    'transiliac', 'extrasacerdotal',
    'almsgiving', 'dittographic',
    'nonmillionaire', 'recorder',
    'pteridophyte', 'Stikine',
    'brahmanism', 'overgrain',
    'histoplasmin', 'chainwale',
    'chaperone', 'unargumentative',
    'stooker', 'phyllodium',
    'sir', 'alliteratively',
    'clinocephalic', 'tutenag',
    'compendency', 'unpartial',
    'uteritis', 'manstealer',
    'logometrical', 'nitrosococcus',
    'threadless', 'psilosophy',
    'ephraimitish', 'polystylar',
    'metatatic', 'zosteriform',
    'Phidian', 'Urocerata',
    'celestina', 'plausibly',
    'polonaise', 'wheelbird',
    'carnalness', 'refocillate',
    'pidjajap', 'shush',
    'gools', 'anticipatable',
    'kitcat', 'oversolemnly',
    'exedent', 'soundful',
    'cauterization', 'oculated',
    'haemonchiasis', 'ethmopalatine',
    'lammas', 'scales',
    'unforgiver', 'encyclopedial',
    'unlousy', 'accumbent',
    'lunare', 'chemisette',
    'dinitro', 'knavery',
    'Dipodomyinae', 'counterweighted',
    'repandousness', 'nonneutral',
    'heterotelic', 'prickleback',
    'pukateine', 'demerol',
    'mokihana', 'inderivative',
    'alytarch', 'solenostomous',
    'plumist', 'antipodagron',
    'mylonitic', 'candlewasting',
    'fractionating', 'equinus',
    'hyaloiditis', 'poriferal',
    'geikia', 'coracobrachialis',
    'ammonite', 'slanderer',
    'outskirmish', 'whacker',
    'subregion', 'marcgraviaceae',
    'alfur', 'givey',
    'denitrification', 'unamplified',
    'pulvilliform', 'disguisedness',
    'incandescent', 'lumbosacral',
    'mohock', 'theodosia',
    'encave', 'pituitous',
    'resawyer', 'peaceman',
    'acroparalysis', 'unalterability',
    'birdcall', 'leglet',
    'lienogastric', 'callosum',
    'dianisidin', 'tonant',
    'controversialist', 'geognosis',
    'yuckel', 'hexarchy',
    'unferried', 'antistrophal',
    'plainness', 'pilgrimwise',
    'traduce', 'venerable',
    'treasonably', 'coronaled',
    'uncircumlocutory', 'oxytone',
    'bifer', 'nunlike',
    'beshrew', 'preinspire',
    'blindstory', 'cerebralization',
    'eudaemonist', 'antigalactagogue',
    'pyromucyl', 'overbandy',
    'wheatear', 'vacanthearted',
    'uprip', 'cerebellopontile',
    'wifelike', 'isophasal',
    'frisca', 'axillae',
    'wrainstaff', 'beneaped',
    'sclerodermi', 'overcovetous',
    'dusty', 'underthrob',
    'noncorrosive', 'abbreviate',
    'octans', 'endurance',
    'quinquefid', 'athlete',
    'jossakeed', 'clochette',
    'resistively', 'ulerythema',
    'undiscernible', 'infusionist',
    'cerebropathy', 'hyperglycorrhachia',
    'diorthosis', 'karwar',
    'lutao', 'myelosyringosis',
    'unincreased', 'termless',
    'linenman', 'sacristan',
    'interatomic', 'limitary',
    'thunderous', 'sulphogallic',
    'enantiomorphously', 'strophiolated',
    'rubbishly', 'semihumorously',
    'evangelicism', 'thapsia',
    'majorate', 'seagirt',
    'imponderous', 'fortuitously',
    'caravel', 'deuteranomal',
    'pantomimist', 'puzzleheadedly',
    'flatterer', 'mikie',
    'unbelieve', 'preconfiguration',
    'trochaic', 'anoestrus',
    'unstaveable', 'treaclewort',
    'zigzaggedly', 'whereat',
    'dailiness', 'lophiostomate',
    'rhinoptera', 'divinatory',
    'Rosellinia', 'fingerling',
    'glyptotherium', 'hexaploid',
    'sagebrush', 'erasure',
    'isonephelic', 'stepgrandson',
    'intermorainic', 'Rhinophis',
    'yellowtop', 'twitterboned',
    'uninitiate', 'fleeter',
    'keratitis', 'wrestlerlike',
    'Eciton', 'neillia',
    'indeficiently', 'debouch',
    'daktylon', 'oftness',
    'chemicomineralogical', 'nonferrous',
    'cocamama', 'tetraspheric',
    'parel', 'wailer',
    'adjustive', 'tannyl',
    'interramification', 'pyche',
    'antivibrating', 'primordium',
    'indemnification', 'semicynical',
    'suprasensuous', 'toyfulness',
    'counterslope', 'misfortuner',
    'sophy', 'athericera',
    'exhaustively', 'meliaceae',
    'calceolate', 'Giansar',
    'mastologist', 'bilker',
    'harttite', 'garishness',
    'diffidation', 'Peggy',
    'eristic', 'saltfat',
    'unenterable', 'photospectroscopical',
    'amminolytic', 'nabber',
    'unministerial', 'polytrichia',
    'redelegation', 'producibleness',
    'unpalatial', 'vower',
    'unwrinkled', 'unfrosted',
    'assort', 'semiextinct',
    'metamorphoses', 'middlingish',
    'interorbitally', 'speechment',
    'semeia', 'uniplicate',
    'protoreptilian', 'unoverflowing',
    'eve', 'myxospongiae',
    'hypapophysis', 'certificate',
    'irremissibly', 'grossulaceous',
    'precritical', 'squaliform',
    'gorgonacean', 'overheave',
    'apathy', 'minion',
    'etypical', 'paleolithoid',
    'alulet', 'philanthropic',
    'trammer', 'aerophotography',
    'photodissociation', 'armament',
    'amphimictic', 'hendecagonal',
    'enough', 'micrometrically',
    'pare', 'cervicoaxillary',
    'redevelop', 'equidominant',
    'nighted', 'slogan',
    'cometographical', 'veinage',
    'inopercular', 'demitone',
    'reflection', 'binate',
    'insection', 'gaskin',
    'sulbasutra', 'neurepithelium',
    'nabalitic', 'sachem',
    'rigger', 'Kolis',
    'phaeophore', 'stotterel',
    'kidling', 'Heterodonta',
    'begem', 'bibliopoly',
    'switchbacker', 'fairtime',
    'bounded', 'pentametrist',
    'brigand', 'dickey',
    'troggin', 'nonimitative',
    'distantly', 'metrist',
    'bitterly', 'ribonucleic',
    'leiophyllum', 'disadventure',
    'ternstroemiaceae', 'cystoradiography',
    'lithectomy', 'yardman',
    'erection', 'carbonization',
    'unhomologous', 'Tagaur',
    'regimentation', 'submammary',
    'kittenishness', 'Phaeodaria',
    'forcipiform', 'unculture',
    'festuca', 'bourtree',
    'miskenning', 'instanter',
    'merino', 'staphylinid',
    'overtariff', 'leeky',
    'unabidingness', 'Heinrich',
    'chromocyte', 'hemophile',
    'bewrayment', 'alterity',
    'unispinose', 'hart',
    'reimbursement', 'plumeous',
    'Embioptera', 'swan',
    'urography', 'leguan',
    'montane', 'spong',
    'stilboestrol', 'astragalar',
    'vindicate', 'bullishness',
    'hepatoid', 'phototelegraph',
    'thalamifloral', 'eaglestone',
    'psychagogue', 'woodhorse',
    'tubicen', 'machiavellist',
    'eurybenthic', 'hoodful',
    'foreskirt', 'seadrome',
    'unrefuting', 'nutseed',
    'elatedness', 'poimenics',
    'goodeniaceous', 'expulse',
    'whyever', 'nomography',
    'coascend', 'pelias',
    'uromelanin', 'postmuscular',
    'scandalmongery', 'carbohydrate',
    'typhloalbuminuria', 'insistingly',
    'circumintestinal', 'misedit',
    'indigotic', 'unode',
    'rathely', 'domestication',
    'fluffy', 'tooter',
    'pupilloscope', 'necrologic',
    'gata', 'unoccluded',
    'spleuchan', 'helminthous',
    'plecopterous', 'foreconsider',
    'archturncoat', 'coagulin',
    'bowdlerism', 'sulphurous',
    'howlingly', 'profectional',
    'Lumbricidae', 'spider',
    'alphabetics', 'whaleboat',
    'Carapache', 'adipate',
    'kissy', 'screechily',
    'theoastrological', 'wisewoman',
    'myelomeningitis', 'underadjustment',
    'transference', 'inhume',
    'remagnetization', 'schmelz',
    'pherecratian', 'revirescence',
    'bookery', 'megotalc',
    'opercled', 'pipkin',
    'algebraization', 'peerling',
    'endocrinopathic', 'ostensibly',
    'assamites', 'busybody',
    'faffy', 'unrepiqued',
    'unigenous', 'cryable',
    'organing', 'pulegol',
    'hammochrysos', 'tillerman',
    'admix', 'correlate',
    'penance', 'Silvia',
    'incredulity', 'nimshi',
    'methodist', 'bowleggedness',
    'robotry', 'encephalomeningitis',
    'redsear', 'soldierly',
    'regreet', 'amadou',
    'pemican', 'unnebulous',
    'chloralization', 'septemia',
    'fullery', 'redock',
    'neath', 'liverleaf',
    'impletion', 'agnoetae',
    'umbones', 'victualless',
    'hypogonation', 'antilysis',
    'paranuclear', 'podostomatous',
    'syncytial', 'renotice',
    'calcification', 'dollishness',
    'Cynthiidae', 'Clementina',
    'plantar', 'palicourea',
    'columelliform', 'bindingness',
    'sackful', 'firebote',
    'garlicwort', 'epithalline',
    'conformableness', 'velamentous',
    'utfangethef', 'crocused',
    'insurmountability', 'granary',
    'stand', 'shrinelet',
    'canadite', 'pseudoerysipelas',
    'ayous', 'adjustment',
    'countercouchant', 'pneumatophilosophy',
    'toromona', 'denaturize',
    'packmanship', 'sanitation',
    'drawshave', 'giggling',
    'organism', 'synchronological',
    'absinthial', 'madidans',
    'paintable', 'spermoblastic',
    'perivaginal', 'wharve',
    'paralytical', 'facinorousness',
    'criminality', 'Amphirhina',
    'bedcap', 'strawboard',
    'idioplasmic', 'Simonian',
    'exaggerate', 'culicid',
    'overbrow', 'precancellation',
    'hyposyllogistic', 'terpsichoreal',
    'meconophagist', 'fluviatile',
    'charlatanically', 'unregenerateness',
    'incombustibleness', 'infrapubian',
    'monobasic', 'uneasy',
    'commelinaceae', 'remigial',
    'psychotic', 'asphodel',
    'paraphrasian', 'lawyership',
    'wined', 'uncherishing',
    'hives', 'compagination',
    'macroanalysis', 'eldritch',
    'appearanced', 'anthropogeography',
    'scoffing', 'mechitaristican',
    'garboil', 'bricking',
    'unwrinkleable', 'overrooted',
    'gimleteyed', 'forecited',
    'Polab', 'coadore',
    'recultivation', 'postgeniture',
    'nonisobaric', 'diopsis',
    'archminister', 'satelles',
    'outbred', 'ciliospinal',
    'stablekeeper', 'rubstone',
    'sightlily', 'trilocular',
    'bakula', 'sniggle',
    'petrify', 'geodiferous',
    'Hun', 'underhand',
    'androgynary', 'Japanize',
    'unreprievably', 'mesmerizer',
    'hoplomachist', 'ommatophore',
    'saccharometric', 'dedan',
    'epactal', 'hubshi',
    'dislocator', 'magnetic',
    'thirsting', 'unforewarnedness',
    'giantlike', 'verificative',
    'maximed', 'botryoidally',
    'northerner', 'wraithy',
    'quisle', 'caretta',
    'frenchy', 'chlorochromic',
    'yelk', 'downweighted',
    'homelessness', 'lithontriptic',
    'natuary', 'reapplaud',
    'tapaculo', 'floatman',
    'superdevotion', 'cella',
    'revilement', 'adscriptive',
    'reshun', 'idioplasmatic',
    'anthropophuism', 'flirtatious',
    'Benjamite', 'southeastern',
    'suggestress', 'fascisticize',
    'eccrisis', 'outfort',
    'subsistingly', 'tophetize',
    'pseudonavicular', 'reswim',
    'ocelli', 'appreciational',
    'chromatopathia', 'hoarstone',
    'vaucheria', 'supersupremacy',
    'evangelical', 'glia',
    'Dendropogon', 'strangulable',
    'insnare', 'flightful',
    'adsignify', 'eudiometry',
    'unhelpableness', 'posturist',
    'thargelion', 'adstipulator',
    'bearableness', 'tactful',
    'deuteronomic', 'apathetical',
    'scrollwise', 'microlevel');

{ TFunTest }

procedure TFunTest.LevenshteinDist;
begin
  AssertTrue(LevDistWF('', 'hello') = 5);
  AssertTrue(LevDistance('', 'hello') = 5);
  AssertTrue(LevDistance('hello', '') = 5);
  AssertTrue(LevDistance('hello', 'hello') = 0);
  AssertTrue(LevDistWF('ab', 'aa') = 1);
  AssertTrue(LevDistance('ab', 'aa') = 1);
  AssertTrue(LevDistance('aa', 'ab') = 1);
  AssertTrue(LevDistance('ab', 'ba') = 2);
  AssertTrue(LevDistance('ba', 'ab') = 2);
  AssertTrue(LevDistWF('ab', 'aaa') = 2);
  AssertTrue(LevDistance('ab', 'aaa') = 2);
  AssertTrue(LevDistance('a', 'bbb') = 3);
  AssertTrue(LevDistWF('aababab','abbaa') = 3);
  AssertTrue(LevDistance('aababab','abbaa') = 3);
  AssertTrue(LevDistWF('helli', 'elli') = 1);
  AssertTrue(LevDistance('helli', 'elli') = 1);
  AssertTrue(LevDistWF('ellia', 'helli') = 2);
  AssertTrue(LevDistance('ellia', 'helli') = 2);
  AssertTrue(LevDistance('helli', 'ellia') = 2);
  AssertTrue(LevDistWF('kitten', 'sitten') = 1);
  AssertTrue(LevDistance('kitten', 'sitten') = 1);
  AssertTrue(LevDistance('sitten', 'kitten') = 1);
  AssertTrue(LevDistWF('kitten', 'sitting') = 3);
  AssertTrue(LevDistance('kitten', 'sitting') = 3);
  AssertTrue(LevDistWF('distance', 'difference') = 5);
  AssertTrue(LevDistance('distance', 'difference') = 5);
  AssertTrue(LevDistWF('levenshtein', 'frankenstein') = 6);
  AssertTrue(LevDistance('levenshtein', 'frankenstein') = 6);
  AssertTrue(LevDistWF('aaaaaaa', 'aaaaaaa') = 0);
  AssertTrue(LevDistance('aaaaaaa', 'aaaaaaa') = 0);
  AssertTrue(LevDistWF('aaaaaaa', 'bbbbbbbb') = 8);
  AssertTrue(LevDistance('aaaaaaa', 'bbbbbbbb') = 8);
  AssertTrue(LevDistWF('aaabbaaaa', 'aaaaaaa') = 2);
  AssertTrue(LevDistance('aaabbaaaa', 'aaaaaaa') = 2);
  AssertTrue(LevDistance('a', 'a') = 0);
  AssertTrue(LevDistance('a', 'b') = 1);
end;

procedure TFunTest.LevenshteinDist2;
var
  I: Integer = 0;
  d1, d2: Integer;
begin
  while I < High(TestWords) do
    begin
      d1 := LevDistWF(TestWords[I], TestWords[I+1]);
      d2 := LevDistance(TestWords[I], TestWords[I+1]);
      AssertTrue(d1 = d2);
      I += 2;
    end;
end;

procedure TFunTest.LevenshteinDistMbr;
begin
  AssertTrue(LevDistanceMbr('', 'hello') = 5);
  AssertTrue(LevDistanceMbr('hello', '') = 5);
  AssertTrue(LevDistanceMbr('hello', 'hello') = 0);
  AssertTrue(LevDistanceMbr('ab', 'aa') = 1);
  AssertTrue(LevDistanceMbr('aa', 'ab') = 1);
  AssertTrue(LevDistanceMbr('ab', 'ba') = 2);
  AssertTrue(LevDistanceMbr('ba', 'ab') = 2);
  AssertTrue(LevDistanceMbr('ab', 'aaa') = 2);
  AssertTrue(LevDistanceMbr('a', 'bbb') = 3);
  AssertTrue(LevDistanceMbr('aababab','abbaa') = 3);
  AssertTrue(LevDistanceMbr('helli', 'elli') = 1);
  AssertTrue(LevDistanceMbr('ellia', 'helli') = 2);
  AssertTrue(LevDistanceMbr('helli', 'ellia') = 2);
  AssertTrue(LevDistanceMbr('kitten', 'sitten') = 1);
  AssertTrue(LevDistanceMbr('sitten', 'kitten') = 1);
  AssertTrue(LevDistanceMbr('kitten', 'sitting') = 3);
  AssertTrue(LevDistanceMbr('distance', 'difference') = 5);
  AssertTrue(LevDistanceMbr('levenshtein', 'frankenstein') = 6);
  AssertTrue(LevDistanceMbr('aaaaaaa', 'aaaaaaa') = 0);
  AssertTrue(LevDistanceMbr('aaaaaaa', 'bbbbbbbb') = 8);
  AssertTrue(LevDistanceMbr('aaabbaaaa', 'aaaaaaa') = 2);
  AssertTrue(LevDistanceMbr('a', 'a') = 0);
  AssertTrue(LevDistanceMbr('a', 'b') = 1);
end;

procedure TFunTest.LevenshteinDistMbr2;
var
  I: Integer = 0;
  d1, d2: Integer;
begin
  while I < High(TestWords) do
    begin
      d1 := LevDistWF(TestWords[I], TestWords[I+1]);
      d2 := LevDistanceMbr(TestWords[I], TestWords[I+1]);
      AssertTrue(d1 = d2);
      I += 2;
    end;
end;

procedure TFunTest.LevenshteinDistMbrBounded;
begin
  AssertTrue(LevDistanceMbr('', 'hello', 5) = 5);
  AssertTrue(LevDistanceMbr('', 'hello', 4) = -1);
  AssertTrue(LevDistanceMbr('ab', 'ba', 2) = 2);
  AssertTrue(LevDistanceMbr('ba', 'ab', 1) = -1);
  AssertTrue(LevDistanceMbr('ab', 'aaa', 2) = 2);
  AssertTrue(LevDistanceMbr('ab', 'aaa', 1) = -1);
  AssertTrue(LevDistanceMbr('a', 'bbb', 3) = 3);
  AssertTrue(LevDistanceMbr('a', 'bbb', 2) = -1);
  AssertTrue(LevDistanceMbr('aababab','abbaa', 3) = 3);
  AssertTrue(LevDistanceMbr('aababab','abbaa', 2) = -1);
  AssertTrue(LevDistanceMbr('helli', 'ellia', 2) = 2);
  AssertTrue(LevDistanceMbr('helli', 'ellia', 1) = -1);
  AssertTrue(LevDistanceMbr('kitten', 'sitting', 3) = 3);
  AssertTrue(LevDistanceMbr('kitten', 'sitting', 2) = -1);
  AssertTrue(LevDistanceMbr('distance', 'difference', 5) = 5);
  AssertTrue(LevDistanceMbr('distance', 'difference', 4) = -1);
  AssertTrue(LevDistanceMbr('levenshtein', 'frankenstein', 6) = 6);
  AssertTrue(LevDistanceMbr('levenshtein', 'frankenstein', 5) = -1);
  AssertTrue(LevDistanceMbr('aaaaaaa', 'aaaaaaa', 1) = 0);
  AssertTrue(LevDistanceMbr('aaaaaaa', 'bbbbbbbb', 8) = 8);
  AssertTrue(LevDistanceMbr('aaaaaaa', 'bbbbbbbb', 7) = -1);
end;

procedure TFunTest.LevenshteinDistMbrDyn;
begin
  AssertTrue(LevDistanceMbr('', 'hello', -1) = 5);
  AssertTrue(LevDistanceMbr('ab', 'ba', -1) = 2);
  AssertTrue(LevDistanceMbr('ab', 'aaa', -1) = 2);
  AssertTrue(LevDistanceMbr('a', 'bbb', -1) = 3);
  AssertTrue(LevDistanceMbr('aababab','abbaa', -1) = 3);
  AssertTrue(LevDistanceMbr('helli', 'ellia', -1) = 2);
  AssertTrue(LevDistanceMbr('kitten', 'sitting', -1) = 3);
  AssertTrue(LevDistanceMbr('distance', 'difference', -1) = 5);
  AssertTrue(LevDistanceMbr('levenshtein', 'frankenstein', -1) = 6);
  AssertTrue(LevDistanceMbr('aaaaaaa', 'aaaaaaa', -1) = 0);
  AssertTrue(LevDistanceMbr('aaaaaaa', 'bbbbbbbb', -1) = 8);
end;

procedure TFunTest.LevenshteinDistMyersD;
begin
  AssertTrue(LevDistanceMyers('', 'hello') = 5);
  AssertTrue(LevDistanceMyers('hello', '') = 5);
  AssertTrue(LevDistanceMyers('hello', 'hello') = 0);
  AssertTrue(LevDistanceMyers('ab', 'aa') = 1);
  AssertTrue(LevDistanceMyers('aa', 'ab') = 1);
  AssertTrue(LevDistanceMyers('ab', 'ba') = 2);
  AssertTrue(LevDistanceMyers('ba', 'ab') = 2);
  AssertTrue(LevDistanceMyers('ab', 'aaa') = 2);
  AssertTrue(LevDistanceMyers('a', 'bbb') = 3);
  AssertTrue(LevDistanceMyers('aababab','abbaa') = 3);
  AssertTrue(LevDistanceMyers('helli', 'elli') = 1);
  AssertTrue(LevDistanceMyers('ellia', 'helli') = 2);
  AssertTrue(LevDistanceMyers('helli', 'ellia') = 2);
  AssertTrue(LevDistanceMyers('kitten', 'sitten') = 1);
  AssertTrue(LevDistanceMyers('sitten', 'kitten') = 1);
  AssertTrue(LevDistanceMyers('kitten', 'sitting') = 3);
  AssertTrue(LevDistanceMyers('distance', 'difference') = 5);
  AssertTrue(LevDistanceMyers('levenshtein', 'frankenstein') = 6);
  AssertTrue(LevDistanceMyers('aaaaaaa', 'aaaaaaa') = 0);
  AssertTrue(LevDistanceMyers('aaaaaaa', 'bbbbbbbb') = 8);
  AssertTrue(LevDistanceMyers('aaabbaaaa', 'aaaaaaa') = 2);
  AssertTrue(LevDistanceMyers('a', 'a') = 0);
  AssertTrue(LevDistanceMyers('a', 'b') = 1);
end;

procedure TFunTest.LevenshteinDistMyersD2;
var
  I: Integer = 0;
begin
  while I < High(TestWords) do
    begin
      AssertTrue(LevDistanceMyers(TestWords[I], TestWords[I+1]) = LevDistWF(TestWords[I], TestWords[I+1]));
      I += 2;
    end;
end;

procedure TFunTest.LevenshteinDistMyersQ;
const
  s1  = 'Please my friend wait outside of the house';
  s2  = 'I want to buy a onesie… but know it won''t suit me';
  s3  = 'I will never be this young again. Ever. Oh damn…';
  s4  = 'I really want to go to work, but I am too sick to drive';
  s5  = 'The shooter says goodbye to his love';
  s6  = 'He didn''t want to go to the dentist, yet he went anyway';
  s7  = 'He ran out of money, so he had to stop playing poker';
  s8  = 'I am counting my calories, yet I really want dessert';
  s9  = 'It was getting dark, and we weren''t there yet';
  s10 = 'They got there early, and they got really good seats';
  s11 = 'The quick brown fox jumps over the lazy dog';
  s12 = 'I checked to make sure that he was still alive';
  s13 = 'She only paints with bold colors; she does not like pastels';
  s14 = 'A song can make or ruin a person''s day if they let it get to them';
begin
  AssertTrue(LevDistanceMyers(s1, s2) = LevDistWF(s1, s2));
  AssertTrue(LevDistanceMyers(s3, s4) = LevDistWF(s3, s4));
  AssertTrue(LevDistanceMyers(s5, s6) = LevDistWF(s5, s6));
  AssertTrue(LevDistanceMyers(s7, s8) = LevDistWF(s7, s8));
  AssertTrue(LevDistanceMyers(s9, s10) = LevDistWF(s9, s10));
  AssertTrue(LevDistanceMyers(s11, s12) = LevDistWF(s11, s12));
  AssertTrue(LevDistanceMyers(s13, s14) = LevDistWF(s13, s14));
end;

procedure TFunTest.LevenshteinDistMyersDQ;
const
  s1  = 'Please my friend wait outside of the house. Yeah, I think it''s a good environment for learning English';
  s2  = 'I want to buy a onesie… but know it won''t suit me. I often see the time 11:11 or 12:34 on clocks';
  s3  = 'I will never be this young again. Ever. Oh damn… Cats are good pets, for they are clean and are not noisy';
  s4  = 'I really want to go to work, but I am too sick to drive. She always speaks to him in a loud voice';
  s5  = 'The shooter says goodbye to his love. The body may perhaps compensates for the loss of a true metaphysics';
  s6  = 'He didn''t want to go to the dentist, yet he went anyway. The memory we used to share is no longer coherent';
  s7  = 'He ran out of money, so he had to stop playing poker. The waves were crashing on the shore; it was a lovely sight';
  s8  = 'I am counting my calories, yet I really want dessert. Check back tomorrow; I will see if the book has arrived';
  s9  = 'It was getting dark, and we weren''t there yet. Everyone was busy, so I went to the movie alone';
  s10 = 'They got there early, and they got really good seats. I really want to go to work, but I am too sick to drive';
  s11 = 'The quick brown fox jumps over the lazy dog. The old apple revels in its authority';
  s12 = 'I checked to make sure that he was still alive. Lets all be unique together until we realise we are all the same';
  s13 = 'She only paints with bold colors; she does not like pastels. She was too short to see over the fence';
  s14 = 'A song can make or ruin a person''s day if they let it get to them. We need to rent a room for our party';
begin
  AssertTrue(LevDistanceMyers(s1, s2) = LevDistWF(s1, s2));
  AssertTrue(LevDistanceMyers(s3, s4) = LevDistWF(s3, s4));
  AssertTrue(LevDistanceMyers(s5, s6) = LevDistWF(s5, s6));
  AssertTrue(LevDistanceMyers(s7, s8) = LevDistWF(s7, s8));
  AssertTrue(LevDistanceMyers(s9, s10) = LevDistWF(s9, s10));
  AssertTrue(LevDistanceMyers(s11, s12) = LevDistWF(s11, s12));
  AssertTrue(LevDistanceMyers(s13, s14) = LevDistWF(s13, s14));
end;

procedure TFunTest.LevenshteinDistMyersLong;
const
  s1  = 'Please my friend wait outside of the house. Yeah, I think it''s a good environment for learning English. So how do controls work??';
  s2  = 'I want to buy a onesie… but know it won''t suit me. I often see the time 11:11 or 12:34 on clocks. Also watch out for the Kylix articles';
  s3  = 'I will never be this young again. Ever. Oh damn… Cats are good pets, for they are clean and are not noisy. Why exact about Delphi?';
  s4  = 'I really want to go to work, but I am too sick to drive. She always speaks to him in a loud voice. First and fore most this is not a tutorial';
  s5  = 'The shooter says goodbye to his love. The body may perhaps compensates for the loss of a true metaphysics. My next article will be more informative with respect to programming';
  s6  = 'He didn''t want to go to the dentist, yet he went anyway. The memory we used to share is no longer coherent. eatures appear to be missing – like the help hints for the tool bar';
  s7  = 'He ran out of money, so he had to stop playing poker. The waves were crashing on the shore; it was a lovely sight. It''s not much trouble because I don''t shut off my computer very often';
  s8  = 'I am counting my calories, yet I really want dessert. Check back tomorrow; I will see if the book has arrived. You can then open a terminal window and type either of the names';
  s9  = 'It was getting dark, and we weren''t there yet. Everyone was busy, so I went to the movie alone. I am sure that someone out in the world knows what was needed but I sort of shotgun the process!';
  s10 = 'They got there early, and they got really good seats. I really want to go to work, but I am too sick to drive. I ran into many issues that I believe would have stopped others dead in their tracks';
  s11 = 'The quick brown fox jumps over the lazy dog. The old apple revels in its authority. In fact I think most of the other Linux distributions by default do not install the development libraries at all';
  s12 = 'I checked to make sure that he was still alive. Lets all be unique together until we realise we are all the same. The Levenshtein distance is a string metric for measuring the difference between two sequences';
  s13 = 'She only paints with bold colors; she does not like pastels. She was too short to see over the fence. My Mum tries to be cool by saying that she likes all the same things that I do';
  s14 = 'A song can make or ruin a person''s day if they let it get to them. We need to rent a room for our party. When I was little I had a car door slammed shut on my hand. I still remember it quite vividly';
begin
  AssertTrue(LevDistanceMyers(s1, s2) = LevDistWF(s1, s2));
  AssertTrue(LevDistanceMyers(s3, s4) = LevDistWF(s3, s4));
  AssertTrue(LevDistanceMyers(s5, s6) = LevDistWF(s5, s6));
  AssertTrue(LevDistanceMyers(s7, s8) = LevDistWF(s7, s8));
  AssertTrue(LevDistanceMyers(s9, s10) = LevDistWF(s9, s10));
  AssertTrue(LevDistanceMyers(s11, s12) = LevDistWF(s11, s12));
  AssertTrue(LevDistanceMyers(s13, s14) = LevDistWF(s13, s14));
end;

procedure TFunTest.LevenshteinDistMyersDBounded;
begin
  AssertTrue(LevDistanceMyers('cetology', '_cetol_ogy_', 4) = 3);
  AssertTrue(LevDistanceMyers('cetology', '_cetol_ogy_', 3) = 3);
  AssertTrue(LevDistanceMyers('cetology', '_cetol_ogy_', 2) = -1);
  AssertTrue(LevDistanceMyers('recurvant', '_rec_urv_ant_', 5) = 4);
  AssertTrue(LevDistanceMyers('recurvant', '_rec_urv_ant_', 4) = 4);
  AssertTrue(LevDistanceMyers('recurvant', '_rec_urv_ant_', 3) = -1);
end;

procedure TFunTest.LevenshteinDistMyersQBounded;
const
  s1  = 'Please my friend wait outside of the house';
  s2  = '_Please my friend wait_outside of the house_';
  s3  = 'I want to buy a onesie… but know it won''t suit me';
  s4  = '_I want to_buy a onesie… but_know it_won''t suit me_';
begin
  AssertTrue(LevDistanceMyers(s1, s2, 4) = 3);
  AssertTrue(LevDistanceMyers(s1, s2, 3) = 3);
  AssertTrue(LevDistanceMyers(s1, s2, 2) = -1);
  AssertTrue(LevDistanceMyers(s3, s4, 6) = 5);
  AssertTrue(LevDistanceMyers(s3, s4, 5) = 5);
  AssertTrue(LevDistanceMyers(s3, s4, 4) = -1);
end;

procedure TFunTest.LevenshteinDistMyersDQBounded;
const
  s1  = 'I really want to go to work, but I am too sick to drive. She always speaks to him in a loud voice';
  s2  = '_I really want to go to work, but I am too sick to drive._She always speaks to him in a loud voice_';
  s3  = 'The shooter says goodbye to his love. The body may perhaps compensates for the loss of a true metaphysics';
  s4  = '_The shooter says goodbye_to his love. The body may perhaps_compensates_for the loss of a true metaphysics_';
begin
  AssertTrue(LevDistanceMyers(s1, s2, 4) = 3);
  AssertTrue(LevDistanceMyers(s1, s2, 3) = 3);
  AssertTrue(LevDistanceMyers(s1, s2, 2) = -1);
  AssertTrue(LevDistanceMyers(s3, s4, 6) = 5);
  AssertTrue(LevDistanceMyers(s3, s4, 5) = 5);
  AssertTrue(LevDistanceMyers(s3, s4, 4) = -1);
end;

procedure TFunTest.LevenshteinDistMyersLongBounded;
const
  s1  = 'Please my friend wait outside of the house. Yeah, I think it''s a good environment for learning English. So how do controls work??';
  s2  = '_Please my friend wait outside of the house. Yeah, I think it''s a good environment for learning English._So how do controls work???';
  s3  = 'It was getting dark, and we weren''t there yet. Everyone was busy, so I went to the movie alone. I am sure that someone out in the world knows what was needed but I sort of shotgun the process!';
  s4  = '_It was getting dark, and we weren''t there yet. Everyone was busy, so_I went to the movie alone._I am sure that someone_out in the world knows what was needed but I sort of shotgun the process!!';
begin
  AssertTrue(LevDistanceMyers(s1, s2, 4) = 3);
  AssertTrue(LevDistanceMyers(s1, s2, 3) = 3);
  AssertTrue(LevDistanceMyers(s1, s2, 2) = -1);
  AssertTrue(LevDistanceMyers(s3, s4, 6) = 5);
  AssertTrue(LevDistanceMyers(s3, s4, 5) = 5);
  AssertTrue(LevDistanceMyers(s3, s4, 4) = -1);
end;

procedure TFunTest.LevenshteinDistMyersDBounded2;
begin
  AssertTrue(LevDistanceMyers('cetology', '_cetol_ogy_', -2) = 3);
  AssertTrue(LevDistanceMyers('recurvant', '_rec_urv_ant_', -1) = 4);
end;

procedure TFunTest.LevenshteinDistMyersQBounded2;
const
  s1  = 'Please my friend wait outside of the house';
  s2  = '_Please my friend wait_outside of the house_';
  s3  = 'I want to buy a onesie… but know it won''t suit me';
  s4  = '_I want to_buy a onesie… but_know it_won''t suit me_';
begin
  AssertTrue(LevDistanceMyers(s1, s2, -2) = 3);
  AssertTrue(LevDistanceMyers(s3, s4, -1) = 5);
end;

procedure TFunTest.LevenshteinDistMyersDQBounded2;
const
  s1  = 'I really want to go to work, but I am too sick to drive. She always speaks to him in a loud voice';
  s2  = '_I really want to go to work, but I am too sick to drive._She always speaks to him in a loud voice_';
  s3  = 'The shooter says goodbye to his love. The body may perhaps compensates for the loss of a true metaphysics';
  s4  = '_The shooter says goodbye_to his love. The body may perhaps_compensates_for the loss of a true metaphysics_';
begin
  AssertTrue(LevDistanceMyers(s1, s2, -4) = 3);
  AssertTrue(LevDistanceMyers(s3, s4, -5) = 5);
end;

procedure TFunTest.LevenshteinDistMyersLongBounded2;
const
  s1  = 'Please my friend wait outside of the house. Yeah, I think it''s a good environment for learning English. So how do controls work??';
  s2  = '_Please my friend wait outside of the house. Yeah, I think it''s a good environment for learning English._So how do controls work???';
  s3  = 'It was getting dark, and we weren''t there yet. Everyone was busy, so I went to the movie alone. I am sure that someone out in the world knows what was needed but I sort of shotgun the process!';
  s4  = '_It was getting dark, and we weren''t there yet. Everyone was busy, so_I went to the movie alone._I am sure that someone_out in the world knows what was needed but I sort of shotgun the process!!';
begin
  AssertTrue(LevDistanceMyers(s1, s2, -4) = 3);
  AssertTrue(LevDistanceMyers(s3, s4, -6) = 5);
end;

procedure TFunTest.LcsGusTest;
var
  s1: string = '';
  s2: string = '';
  Lcs, LcsG: string;
  I: Integer = 0;
begin
  AssertTrue(LcsDp(s1, s2) = '');
  AssertTrue(LcsGus(s1, s2) = '');

  s1 := 'aa';
  AssertTrue(LcsDp(s1, s2) = '');
  AssertTrue(LcsGus(s1, s2) = '');

  AssertTrue(LcsGus('a', 'a') = 'a');
  AssertTrue(LcsGus('a', 'b') = '');

  s1 := 'thisisatest';
  s2 := 'testing123testing';
  AssertTrue(LcsDp(s1, s2) = 'tsitest');
  AssertTrue(LcsGus(s1, s2) = 'tsitest');

  s1 := '1234';
  s2 := '1224533324';
  AssertTrue(LcsDp(s1, s2) = '1234');
  AssertTrue(LcsGus(s1, s2) = '1234');

  s1 := '01236789';
  s2 := '01234445556789';
  AssertTrue(LcsDp(s1, s2) = '01236789');
  AssertTrue(LcsGus(s1, s2) = '01236789');

  while I < High(TestWords) do
    begin
      Lcs := LcsDp(TestWords[I], TestWords[I+1]);
      LcsG := LcsGus(TestWords[I], TestWords[I+1]);
      if Lcs <> LcsG then
        begin
          AssertTrue(Length(Lcs) = Length(LcsG));
          AssertTrue(IsSubSequence(TestWords[I], LcsG));
          AssertTrue(IsSubSequence(TestWords[I+1], LcsG));
        end;
      I += 2;
    end;
end;

procedure TFunTest.LcsKRTest;
var
  s1: string = '';
  s2: string = '';
  Lcs, LcsK: string;
  I: Integer = 0;
begin
  AssertTrue(LcsDp(s1, s2) = '');
  AssertTrue(LcsKR(s1, s2) = '');

  s1 := 'aa';
  AssertTrue(LcsDp(s1, s2) = '');
  AssertTrue(LcsKR(s1, s2) = '');

  AssertTrue(LcsKR('a', 'a') = 'a');
  AssertTrue(LcsKR('a', 'b') = '');

  s1 := 'thisisatest';
  s2 := 'testing123testing';
  AssertTrue(LcsDp(s1, s2) = 'tsitest');
  AssertTrue(LcsKR(s1, s2) = 'tsitest');

  s1 := '1234';
  s2 := '1224533324';
  AssertTrue(LcsDp(s1, s2) = '1234');
  AssertTrue(LcsKR(s1, s2) = '1234');

  s1 := '01236789';
  s2 := '01234445556789';
  AssertTrue(LcsDp(s1, s2) = '01236789');
  AssertTrue(LcsKR(s1, s2) = '01236789');

  while I < High(TestWords) do
    begin
      Lcs := LcsDp(TestWords[I], TestWords[I+1]);
      LcsK := LcsKR(TestWords[I], TestWords[I+1]);
      if Lcs <> LcsK then
        begin
          AssertTrue(Length(Lcs) = Length(LcsK));
          AssertTrue(IsSubSequence(TestWords[I], LcsK));
          AssertTrue(IsSubSequence(TestWords[I+1], LcsK));
        end;
      I += 2;
    end;
end;

procedure TFunTest.LcsMyersTest;
var
  s1: string = '';
  s2: string = '';
  Lcs, LcsM: string;
  I: Integer = 0;
begin
  AssertTrue(LcsDp(s1, s2) = '');
  AssertTrue(LcsMyers(s1, s2) = '');

  s1 := 'aa';
  AssertTrue(LcsDp(s1, s2) = '');
  AssertTrue(LcsMyers(s1, s2) = '');

  AssertTrue(LcsKR('a', 'a') = 'a');
  AssertTrue(LcsMyers('a', 'b') = '');

  s1 := 'thisisatest';
  s2 := 'testing123testing';
  AssertTrue(LcsDp(s1, s2) = 'tsitest');
  AssertTrue(LcsMyers(s1, s2) = 'tsitest');

  s1 := '1234';
  s2 := '1224533324';
  AssertTrue(LcsDp(s1, s2) = '1234');
  AssertTrue(LcsMyers(s1, s2) = '1234');

  s1 := '01236789';
  s2 := '01234445556789';
  AssertTrue(LcsDp(s1, s2) = '01236789');
  AssertTrue(LcsMyers(s1, s2) = '01236789');

  while I < High(TestWords) do
    begin
      Lcs := LcsDp(TestWords[I], TestWords[I+1]);
      LcsM := LcsMyers(TestWords[I], TestWords[I+1]);
      if Lcs <> LcsM then
        begin
          AssertTrue(Length(Lcs) = Length(LcsM));
          AssertTrue(IsSubSequence(TestWords[I], LcsM));
          AssertTrue(IsSubSequence(TestWords[I+1], LcsM));
        end;
      I += 2;
    end;
end;

const
  DblOne  = Double(1.0);
  DblZero = Double(0.0);

procedure TFunTest.SimRatioTest;
begin
  AssertTrue(SimRatio('', '') = DblOne);
  AssertTrue(SimRatio('aaa', '') = DblZero);
  AssertTrue(SimRatio('', 'bbb') = DblZero);
  AssertTrue(SimRatio('aaa', 'bbb') = DblZero);
  AssertTrue(SimRatio('abc', 'abc') = DblOne);
end;

procedure TFunTest.SimRatioLevExTest;
begin
  AssertTrue(SimRatioEx('', '') = DblOne);
  AssertTrue(SimRatioEx('aaa', '') = DblZero);
  AssertTrue(SimRatioEx('', 'bbb') = DblZero);
  AssertTrue(SimRatioEx('Hello world', ' Hello world ') = DblOne);
  AssertFalse(SimRatioEx('Hello world', ' hello world ') = DblOne);
  AssertTrue(SimRatioEx('Hello world', ' hello world ', smSimple, [' '], [soIgnoreCase]) = DblOne);
  AssertTrue(SimRatioEx('Hello world', 'another Hello world ', smSimple, [' '], [soPartial]) = DblOne);
  AssertTrue(SimRatioEx('Hello, world!', ' hello world ', smSimple, [' ',',','!'], [soIgnoreCase]) = DblOne);
  AssertTrue(SimRatioEx('World hello', ' Hello world ', smTokenSort, [' '], [soIgnoreCase]) = DblOne);
  AssertTrue(SimRatioEx('World hello', ' Hello world, hello', smTokenSet, [' ',','], [soIgnoreCase]) = DblOne);
  AssertTrue(SimRatioEx('World hello', ' Hello another world, hello', smTokenSet, [' ',','], [soIgnoreCase, soPartial]) = DblOne);
  AssertTrue(SimRatioEx('fuzzy was a bear', 'fuzzy fuzzy fuzzy bear', smTokenSetEx) = DblOne);
end;

const
  Win1251CollRu: array[Char] of Byte = (
    $00, $01, $02, $03, $04, $05, $06, $07, $08, $09, $0a, $0b, $0c, $0d, $0e, $0f,
    $10, $11, $12, $13, $14, $15, $16, $17, $18, $19, $1a, $1b, $1c, $1d, $1e, $1f,
    $20, $21, $22, $23, $24, $25, $26, $27, $28, $29, $2a, $2b, $2c, $2d, $2e, $2f,
    $30, $31, $32, $33, $34, $35, $36, $37, $38, $39, $3a, $3b, $3c, $3d, $3e, $3f,
  //64    A    B    C    D    E    F    G    H    I    J    K    L    M   N     O
    $40, $41, $42, $43, $44, $45, $46, $47, $48, $49, $4a, $4b, $4c, $4d, $4e, $4f,
  // P    Q    R    S    T    U    V    W    X    Y    Z
    $50, $51, $52, $53, $54, $55, $56, $57, $58, $59, $5a, $5b, $5c, $5d, $5e, $5f,
  //      a    b    c    d    e    f    g    h    i    j    k    l    m    n    o
    $60, $61, $62, $63, $64, $65, $66, $67, $68, $69, $6a, $6b, $6c, $6d, $6e, $6f,
  // p    q    r    s    t    u    v    w    x    y    z
    $70, $71, $72, $73, $74, $75, $76, $77, $78, $79, $7a, $7b, $7c, $7d, $7e, $7f,
    $80, $81, $82, $83, $84, $85, $86, $87, $88, $89, $8a, $8b, $8c, $8d, $8e, $8f,
    $90, $91, $92, $93, $94, $95, $96, $97, $98, $99, $9a, $9b, $9c, $9d, $9e, $9f,
  //                                          Ё
    $a0, $a1, $a2, $a3, $a4, $a5, $a6, $a7, $c4, $a8, $a9, $aa, $ab, $ac, $ad, $ae,
  //                                          ё
    $af, $b0, $b1, $b2, $b3, $b4, $b5, $b6, $e5, $b7, $b8, $b9, $ba, $bb, $bc, $bd,
  // А    Б    В    Г    Д    Е    Ж    З    И    Й    К    Л    М    Н    О    П
    $be, $bf, $c0, $c1, $c2, $c3, $c5, $c6, $c7, $c8, $c9, $ca, $cb, $cc, $cd, $ce,
  // Р    С    Т    У    Ф    Х    Ц    Ч    Ш    Щ    Ъ    Ы    Ь    Э    Ю    Я
    $cf, $d0, $d1, $d2, $d3, $d4, $d5, $d6, $d7, $d8, $d9, $da, $db, $dc, $dd, $de,
  // а    б    в    г    д    е    ж    з    и    й    к    л    м    н    о    п
    $df, $e0, $e1, $e2, $e3, $e4, $e6, $e7, $e8, $e9, $ea, $eb, $ec, $ed, $ee, $ef,
  // р    с    т    у    ф    х    ц    ч    ш    щ    ъ    ы    ь    э    ю    я
    $f0, $f1, $f2, $f3, $f4, $f5, $f6, $f7, $f8, $f9, $fa, $fb, $fc, $fd, $fe, $ff);

function Less1251(const L, R: array of Char): Boolean;
var
  I, c: SizeInt;
begin
  for I := 0 to Math.Min(High(L), High(R)) do
    begin
      c := SizeInt(Win1251CollRu[L[I]]) - SizeInt(Win1251CollRu[R[I]]);
      if c <> 0 then exit(c < 0);
    end;
  Result := Length(L) < Length(R);
end;

function LoCase1251(const s: rawbytestring): rawbytestring;
var
  I: SizeInt;
begin
  SetLength(Result, Length(s));
  for I := 1 to Length(s) do
    Result[I] := Win1251LoMap[s[I]];
end;

procedure TFunTest.SimRatioLevExTest1251;
var
  R: Double;
  Pat1, Pat2: string1251;
const
  s1: string = 'В зале стояла нарядная настоящая душистая елка до самого потолка';
  s2: string = 'До Ёлка зАле НАСТОЯЩАЯ потолка в дуШИСтая стояла наряДНАЯ самого';
begin
  Pat1 := s1;
  Pat2 := s2;
  R := SimRatioEx(Pat1, Pat2, smTokenSort, [' '], [soIgnoreCase]);
  AssertTrue(R.ToString, R < 0.45);
  R := SimRatioEx(Pat1, Pat2, smTokenSort, [' '], [soIgnoreCase], 0, sdaDefault, @LoCase1251);
  AssertTrue(R.ToString, R > 0.8);
  R := SimRatioEx(Pat1, Pat2, smTokenSort, [' '], [soIgnoreCase], 0, sdaDefault, @LoCase1251, @Less1251);
  AssertTrue(R.ToString, R > 0.98);
end;

function LcsDistDp(const L, R: rawbytestring): SizeInt;
var
  D: array of array of SizeInt;
  I, J: SizeInt;
begin
  SetLength(D, Succ(Length(L)), Succ(Length(R)));
  for I := 1 to Length(L) do
    D[I, 0] := I;
  for I := 1 to Length(R) do
    D[0, I] := I;
  for J := 1 to Length(R) do
    for I := 1 to Length(L) do
      if L[I] = R[J] then
        D[I, J] := D[I-1, J-1]
      else
        D[I, J] := MinOf3(D[I-1, J] + 1, D[I, J-1] + 1, D[I-1, J-1] + 2);
  Result := D[Length(L), Length(R)];
end;

procedure TFunTest.LcsDistMyers;
begin
  AssertTrue(LcsDistDp('', 'hello') = 5);
  AssertTrue(LcsDistanceMyers('', 'hello') = 5);
  AssertTrue(LcsDistanceMyers('hello', '') = 5);

  AssertTrue(LcsDistanceMyers('hello', 'hello') = 0);

  AssertTrue(LcsDistDp('ab', 'aa') = 2);
  AssertTrue(LcsDistanceMyers('ab', 'aa') = 2);
  AssertTrue(LcsDistanceMyers('aa', 'ab') = 2);

  AssertTrue(LcsDistDp('ab', 'ba') = 2);
  AssertTrue(LcsDistanceMyers('ab', 'ba') = 2);
  AssertTrue(LcsDistanceMyers('ba', 'ab') = 2);

  AssertTrue(LcsDistDp('ab', 'aaa') = 3);
  AssertTrue(LcsDistanceMyers('ab', 'aaa') = 3);
  AssertTrue(LcsDistanceMyers('aaa', 'ab') = 3);

  AssertTrue(LcsDistDp('a', 'bbb') = 4);
  AssertTrue(LcsDistanceMyers('a', 'bbb') = 4);
  AssertTrue(LcsDistanceMyers('bbb', 'a') = 4);

  AssertTrue(LcsDistDp('aababab','abbaa') = 4);
  AssertTrue(LcsDistanceMyers('aababab','abbaa') = 4);
  AssertTrue(LcsDistanceMyers('abbaa', 'aababab') = 4);

  AssertTrue(LcsDistDp('helli', 'elli') = 1);
  AssertTrue(LcsDistanceMyers('helli', 'elli') = 1);
  AssertTrue(LcsDistanceMyers('elli', 'helli') = 1);

  AssertTrue(LcsDistDp('ellia', 'helli') = 2);
  AssertTrue(LcsDistanceMyers('ellia', 'helli') = 2);
  AssertTrue(LcsDistanceMyers('helli', 'ellia') = 2);

  AssertTrue(LcsDistDp('kitten', 'sitten') = 2);
  AssertTrue(LcsDistanceMyers('kitten', 'sitten') = 2);
  AssertTrue(LcsDistanceMyers('sitten', 'kitten') = 2);

  AssertTrue(LcsDistDp('kitten', 'sitting') = 5);
  AssertTrue(LcsDistanceMyers('kitten', 'sitting') = 5);
  AssertTrue(LcsDistanceMyers('sitting', 'kitten') = 5);

  AssertTrue(LcsDistDp('distance', 'difference') = 8);
  AssertTrue(LcsDistanceMyers('distance', 'difference') = 8);
  AssertTrue(LcsDistanceMyers('difference', 'distance') = 8);

  AssertTrue(LcsDistDp('levenshtein', 'frankenstein') = 9);
  AssertTrue(LcsDistanceMyers('levenshtein', 'frankenstein') = 9);
  AssertTrue(LcsDistanceMyers('frankenstein', 'levenshtein') = 9);


  AssertTrue(LcsDistDp('aaaaaaa', 'aaaaaaa') = 0);
  AssertTrue(LcsDistanceMyers('aaaaaaa', 'aaaaaaa') = 0);

  AssertTrue(LcsDistDp('aaaaaaa', 'bbbbbbbb') = 15);
  AssertTrue(LcsDistanceMyers('aaaaaaa', 'bbbbbbbb') = 15);
  AssertTrue(LcsDistanceMyers('bbbbbbbb', 'aaaaaaa') = 15);

  AssertTrue(LcsDistDp('aaabbaaaa', 'aaaaaaa') = 2);
  AssertTrue(LcsDistanceMyers('aaabbaaaa', 'aaaaaaa') = 2);
  AssertTrue(LcsDistanceMyers('aaaaaaa', 'aaabbaaaa') = 2);

  AssertTrue(LcsDistanceMyers('b', 'a') = 2);
  AssertTrue(LcsDistanceMyers('a', 'b') = 2);
end;

procedure TFunTest.LcsDistMyers2;
var
  I: Integer = 0;
  d1, d2, d3: Integer;
begin
  while I < High(TestWords) do
    begin
      d1 := LcsDistDp(TestWords[I], TestWords[I+1]);
      d2 := LcsDistanceMyers(TestWords[I], TestWords[I+1]);
      d3 :=
        Length(TestWords[I]) + Length(TestWords[I+1]) - 2 * Length(LcsKR(TestWords[I], TestWords[I+1]));
      AssertTrue((d1 = d2) and (d2 = d3));
      I += 2;
    end;
end;

procedure TFunTest.LcsDistMyersBounded;
begin
  AssertTrue(LcsDistanceMyers('', 'hello', 5) = 5);
  AssertTrue(LcsDistanceMyers('', 'hello', 4) = -1);
  AssertTrue(LcsDistanceMyers('ab', 'ba', 2) = 2);
  AssertTrue(LcsDistanceMyers('ba', 'ab', 1) = -1);
  AssertTrue(LcsDistanceMyers('ab', 'aaa', 3) = 3);
  AssertTrue(LcsDistanceMyers('ab', 'aaa', 2) = -1);
  AssertTrue(LcsDistanceMyers('a', 'bbb', 4) = 4);
  AssertTrue(LcsDistanceMyers('a', 'bbb', 3) = -1);
  AssertTrue(LcsDistanceMyers('aababab','abbaa', 4) = 4);
  AssertTrue(LcsDistanceMyers('aababab','abbaa', 3) = -1);
  AssertTrue(LcsDistanceMyers('helli', 'ellia', 2) = 2);
  AssertTrue(LcsDistanceMyers('helli', 'ellia', 1) = -1);
  AssertTrue(LcsDistanceMyers('kitten', 'sitting', 5) = 5);
  AssertTrue(LcsDistanceMyers('kitten', 'sitting', 4) = -1);
  AssertTrue(LcsDistanceMyers('distance', 'difference', 8) = 8);
  AssertTrue(LcsDistanceMyers('distance', 'difference', 7) = -1);
  AssertTrue(LcsDistanceMyers('levenshtein', 'frankenstein', 9) = 9);
  AssertTrue(LcsDistanceMyers('levenshtein', 'frankenstein', 8) = -1);
  AssertTrue(LcsDistanceMyers('aaaaaaa', 'aaaaaaa', 1) = 0);
  AssertTrue(LcsDistanceMyers('aaaaaaa', 'bbbbbbbb', 15) = 15);
  AssertTrue(LcsDistanceMyers('aaaaaaa', 'bbbbbbbb', 14) = -1);
end;

procedure TFunTest.LcsDistMyersDyn;
begin
  AssertTrue(LcsDistanceMyers('', 'hello', -1) = 5);
  AssertTrue(LcsDistanceMyers('hello', '', -1) = 5);

  AssertTrue(LcsDistanceMyers('hello', 'hello', -1) = 0);

  AssertTrue(LcsDistanceMyers('ab', 'aa', -1) = 2);
  AssertTrue(LcsDistanceMyers('aa', 'ab', -1) = 2);

  AssertTrue(LcsDistanceMyers('ab', 'ba', -1) = 2);
  AssertTrue(LcsDistanceMyers('ba', 'ab', -1) = 2);

  AssertTrue(LcsDistanceMyers('ab', 'aaa', -1) = 3);
  AssertTrue(LcsDistanceMyers('aaa', 'ab', -1) = 3);

  AssertTrue(LcsDistanceMyers('a', 'bbb', -1) = 4);
  AssertTrue(LcsDistanceMyers('bbb', 'a', -1) = 4);

  AssertTrue(LcsDistanceMyers('aababab','abbaa', -1) = 4);
  AssertTrue(LcsDistanceMyers('abbaa', 'aababab', -1) = 4);

  AssertTrue(LcsDistanceMyers('helli', 'elli', -1) = 1);
  AssertTrue(LcsDistanceMyers('elli', 'helli', -1) = 1);

  AssertTrue(LcsDistanceMyers('ellia', 'helli', -1) = 2);
  AssertTrue(LcsDistanceMyers('helli', 'ellia', -1) = 2);

  AssertTrue(LcsDistanceMyers('kitten', 'sitten', -1) = 2);
  AssertTrue(LcsDistanceMyers('sitten', 'kitten', -1) = 2);

  AssertTrue(LcsDistanceMyers('kitten', 'sitting', -1) = 5);
  AssertTrue(LcsDistanceMyers('sitting', 'kitten', -1) = 5);

  AssertTrue(LcsDistanceMyers('distance', 'difference', -1) = 8);
  AssertTrue(LcsDistanceMyers('difference', 'distance', -1) = 8);

  AssertTrue(LcsDistanceMyers('levenshtein', 'frankenstein', -1) = 9);
  AssertTrue(LcsDistanceMyers('frankenstein', 'levenshtein', -1) = 9);

  AssertTrue(LcsDistanceMyers('aaaaaaa', 'bbbbbbbb', -1) = 15);
  AssertTrue(LcsDistanceMyers('bbbbbbbb', 'aaaaaaa', -1) = 15);

  AssertTrue(LcsDistanceMyers('aaabbaaaa', 'aaaaaaa', -1) = 2);
  AssertTrue(LcsDistanceMyers('aaaaaaa', 'aaabbaaaa', -1) = 2);

  AssertTrue(LcsDistanceMyers('b', 'a', -1) = 2);
  AssertTrue(LcsDistanceMyers('a', 'b', -1) = 2);
end;

procedure TFunTest.DamerauDistMbr;
begin
  AssertTrue(DumDistanceMbr('', 'hello') = 5);
  AssertTrue(DumDistanceMbr('hello', '') = 5);
  AssertTrue(DumDistanceMbr('hello', 'hello') = 0);
  AssertTrue(DumDistanceMbr('ab', 'aa') = 1);
  AssertTrue(DumDistanceMbr('aa', 'ab') = 1);
  AssertTrue(DumDistanceMbr('ab', 'ba') = 1);
  AssertTrue(DumDistanceMbr('ba', 'ab') = 1);
  AssertTrue(DumDistanceMbr('ab', 'aaa') = 2);
  AssertTrue(DumDistanceMbr('a', 'bbb') = 3);
  AssertTrue(DumDistanceMbr('ac', 'cba') = 3);
  AssertTrue(DumDistanceMbr('cba', 'ac') = 3);
  AssertTrue(DumDistanceMbr('aababab','abbaa') = 3);
  AssertTrue(DumDistanceMbr('helli', 'elli') = 1);
  AssertTrue(DumDistanceMbr('ehlli', 'helli') = 1);
  AssertTrue(DumDistanceMbr('helli', 'ehlli') = 1);
  AssertTrue(DumDistanceMbr('kitten', 'sitten') = 1);
  AssertTrue(DumDistanceMbr('sitten', 'kitten') = 1);
  AssertTrue(DumDistanceMbr('kitten', 'sitting') = 3);
  AssertTrue(DumDistanceMbr('kitten', 'ktitne') = 2);
  AssertTrue(DumDistanceMbr('ktitne', 'kitten') = 2);
  AssertTrue(DumDistanceMbr('distance', 'difference') = 5);
  AssertTrue(DumDistanceMbr('levenshtein', 'frankenstein') = 6);
  AssertTrue(DumDistanceMbr('aaaaaaa', 'aaaaaaa') = 0);
  AssertTrue(DumDistanceMbr('aaaaaaa', 'bbbbbbbb') = 8);
  AssertTrue(DumDistanceMbr('aaabbaaaa', 'aaaaaaa') = 2);
  AssertTrue(DumDistanceMbr('a', 'a') = 0);
  AssertTrue(DumDistanceMbr('a', 'b') = 1);
end;

procedure TFunTest.DamerauDistMbrBounded;
begin
  AssertTrue(DumDistanceMbr('', 'hello', 5) = 5);
  AssertTrue(DumDistanceMbr('', 'hello', 4) = -1);
  AssertTrue(DumDistanceMbr('hello', 'hello', 1) = 0);
  AssertTrue(DumDistanceMbr('ab', 'aa', 1) = 1);
  AssertTrue(DumDistanceMbr('ab', 'ba', 1) = 1);
  AssertTrue(DumDistanceMbr('ab', 'ba', 0) = -1);
  AssertTrue(DumDistanceMbr('ab', 'aaa', 2) = 2);
  AssertTrue(DumDistanceMbr('ab', 'aaa', 1) = -1);
  AssertTrue(DumDistanceMbr('a', 'bbb', 2) = -1);
  AssertTrue(DumDistanceMbr('ac', 'cba', 3) = 3);
  AssertTrue(DumDistanceMbr('ac', 'cba', 2) = -1);
  AssertTrue(DumDistanceMbr('aababab','abbaa', 3) = 3);
  AssertTrue(DumDistanceMbr('aababab','abbaa', 2) = -1);
  AssertTrue(DumDistanceMbr('helli', 'elli', 1) = 1);
  AssertTrue(DumDistanceMbr('helli', 'elli', 0) = -1);
  AssertTrue(DumDistanceMbr('ehlli', 'helli', 1) = 1);
  AssertTrue(DumDistanceMbr('ehlli', 'helli', 0) = -1);
  AssertTrue(DumDistanceMbr('kitten', 'sitten', 1) = 1);
  AssertTrue(DumDistanceMbr('kitten', 'sitten', 0) = -1);
  AssertTrue(DumDistanceMbr('kitten', 'sitting', 3) = 3);
  AssertTrue(DumDistanceMbr('kitten', 'sitting', 2) = -1);
  AssertTrue(DumDistanceMbr('kitten', 'ktitne', 2) = 2);
  AssertTrue(DumDistanceMbr('kitten', 'ktitne', 1) = -1);
  AssertTrue(DumDistanceMbr('distance', 'difference', 5) = 5);
  AssertTrue(DumDistanceMbr('distance', 'difference', 4) = -1);
  AssertTrue(DumDistanceMbr('levenshtein', 'frankenstein', 6) = 6);
  AssertTrue(DumDistanceMbr('levenshtein', 'frankenstein', 5) = -1);
end;

procedure TFunTest.DamerauDistMbrDyn;
begin
  AssertTrue(DumDistanceMbr('', 'hello', -1) = 5);
  AssertTrue(DumDistanceMbr('hello', '', -1) = 5);
  AssertTrue(DumDistanceMbr('hello', 'hello', -1) = 0);
  AssertTrue(DumDistanceMbr('ab', 'aa', -1) = 1);
  AssertTrue(DumDistanceMbr('aa', 'ab', -1) = 1);
  AssertTrue(DumDistanceMbr('ab', 'ba', -1) = 1);
  AssertTrue(DumDistanceMbr('ba', 'ab', -1) = 1);
  AssertTrue(DumDistanceMbr('ab', 'aaa', -1) = 2);
  AssertTrue(DumDistanceMbr('a', 'bbb', -1) = 3);
  AssertTrue(DumDistanceMbr('ac', 'cba', -1) = 3);
  AssertTrue(DumDistanceMbr('cba', 'ac', -1) = 3);
  AssertTrue(DumDistanceMbr('aababab','abbaa', -1) = 3);
  AssertTrue(DumDistanceMbr('helli', 'elli', -1) = 1);
  AssertTrue(DumDistanceMbr('ehlli', 'helli', -1) = 1);
  AssertTrue(DumDistanceMbr('helli', 'ehlli', -1) = 1);
  AssertTrue(DumDistanceMbr('kitten', 'sitten', -1) = 1);
  AssertTrue(DumDistanceMbr('sitten', 'kitten', -1) = 1);
  AssertTrue(DumDistanceMbr('kitten', 'sitting', -1) = 3);
  AssertTrue(DumDistanceMbr('kitten', 'ktitne', -1) = 2);
  AssertTrue(DumDistanceMbr('ktitne', 'kitten', -1) = 2);
  AssertTrue(DumDistanceMbr('distance', 'difference', -1) = 5);
  AssertTrue(DumDistanceMbr('levenshtein', 'frankenstein', -1) = 6);
  AssertTrue(DumDistanceMbr('aaaaaaa', 'aaaaaaa', -1) = 0);
  AssertTrue(DumDistanceMbr('aaaaaaa', 'bbbbbbbb', -1) = 8);
  AssertTrue(DumDistanceMbr('aaabbaaaa', 'aaaaaaa', -1) = 2);
  AssertTrue(DumDistanceMbr('a', 'a', -1) = 0);
  AssertTrue(DumDistanceMbr('a', 'b', -1) = 1);
end;

{ TACSearchFsmTest.TRbsHasher }

class function TACSearchFsmTest.TRbsHasher.Equal(const L, R: rawbytestring): Boolean;
begin
  Result := L = R;
end;

class function TACSearchFsmTest.TRbsHasher.HashCode(const s: rawbytestring): SizeInt;
begin
  Result := TxxHash32LE.HashStr(s);
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
  ac: TACSearch;
begin
  {%H-}ac.Instance := TACSearchFsm.Create([]);
  AssertTrue(ac.Instance.AlphabetSize = 0);
  AssertTrue(ac.Instance.NodeCount = 1);
  AssertTrue(ac.Instance.PatternCount = 0);
  AssertFalse(ac.Instance.IsMatch('a'));

  ac.Instance := TACSearchFsm.Create(['', '']);
  AssertTrue(ac.Instance.NodeCount = 1);
  AssertTrue(ac.Instance.PatternCount = 0);

  ac.Instance := TACSearchFsm.Create(['a','b', 'a', 'b']);
  AssertTrue(ac.Instance.AlphabetSize <> 0);
  AssertTrue(ac.Instance.NodeCount = 3);
  AssertTrue(ac.Instance.PatternCount = 2);
  AssertTrue(ac.Instance.IsMatch('a'));
  AssertTrue(ac.Instance.IsMatch('b'));
end;

procedure TACSearchFsmTest.TestSearchDelegated;
var
  ac: TACSearch;
  s: rawbytestring;
  a: TStringArray;
begin
  a := ['her', 'is', 'his', 'she', 'hi', 'he', 'i', 'hero'];
  {%H-}ac.Instance := TACSearchFsm.Create(a);
  s := 'hishero';
  FMatchCount := 0;
  ac.Instance.Search(s, @OnMatch);
  AssertTrue(FMatchCount = Length(a));

  FMatchCount := 0;
  ac.Instance.Search(s, @OnMatchFirst);
  AssertTrue(FMatchCount = 1);
end;

procedure TACSearchFsmTest.TestSearchNested;
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
  s: rawbytestring;
  ss: TStrSet;
  function TestMatch(const m: TMatch): Boolean;
  begin
    ss.Remove(Copy(s, m.Offset, m.Length));
    Result := True;
  end;
var
  ac: TACSearch;
  a: TStringArray;
begin
  a := ['her', 'is', 'his', 'she', 'hi', 'he', 'i', 'hero'];
  {%H-}ac.Instance := TACSearchFsm.Create(a);
  s := 'hishero';
  ac.Instance.Search(s, @NestMatch);
  AssertTrue(MatchCount = Length(a));

  MatchCount := 0;
  ac.Instance.Search(s, @FirstMatch);
  AssertTrue(MatchCount = 1);

  ss.AddAll(a);
  AssertTrue(ss.Count = Length(a));
  ac.Instance.Search(s, @TestMatch);
  AssertTrue(ss.IsEmpty);
end;

procedure TACSearchFsmTest.TestContainsMatch;
var
  ac: TACSearch;
begin
  {%H-}ac.Instance := TACSearchFsm.Create(['aa','bb']);
  AssertFalse(ac.Instance.ContainsMatch('abcde'));
  AssertFalse(ac.Instance.ContainsMatch('ababcde'));
  AssertTrue(ac.Instance.ContainsMatch('aabcde'));
  AssertTrue(ac.Instance.ContainsMatch('abbcde'));
end;

procedure TACSearchFsmTest.TestIndexOfPattern;
var
  ac: TACSearch;
begin
  {%H-}ac.Instance := TACSearchFsm.Create([]);
  AssertTrue(ac.Instance.IndexOfPattern('a') = -1);
  AssertTrue(ac.Instance.IndexOfPattern('b') = -1);
  ac.Instance := TACSearchFsm.Create(['aa','ab','ac']);
  AssertTrue(ac.Instance.IndexOfPattern('a') = -1);
  AssertTrue(ac.Instance.IndexOfPattern('aa') = 0);
  AssertTrue(ac.Instance.IndexOfPattern('ab') = 1);
  AssertTrue(ac.Instance.IndexOfPattern('ac') = 2);
end;

procedure TACSearchFsmTest.TestFirstMatch;
var
  ac: TACSearch;
  s: string;
  m: TMatch;
begin
  s := '012abcdefgh';
  {%H-}ac.Instance := TACSearchFsm.Create(['0124','234','12345', '1234567','1234','23','890123']);
  m := ac.Instance.FirstMatch(s);
  AssertTrue((m.Offset = 0) and (m.Length = 0) and (m.Index = -1));
  s := '01234567890123456789';
  m := ac.Instance.FirstMatch(s);
  AssertTrue((m.Index = 5) and (Copy(s, m.Offset, m.Length) = '23'));
  m := ac.Instance.FirstMatch(s, smmLeftmostFirst);
  AssertTrue((m.Index = 2) and (Copy(s, m.Offset, m.Length) = '12345'));
  m := ac.Instance.FirstMatch(s, smmLeftmostLongest);
  AssertTrue((m.Index = 3) and (Copy(s, m.Offset, m.Length) = '1234567'));
  m := ac.Instance.FirstMatch(s, smmLeftmostShortest);
  AssertTrue((m.Index = 4) and (Copy(s, m.Offset, m.Length) = '1234'));
end;

procedure TACSearchFsmTest.TestFindMatches;
var
  ac: TACSearch;
  s: string;
  a: TStringArray;
  m: array of TMatch;
  I: Integer;
begin
  s := '012abcdefgh';
  a := ['0124','234','12345', '1234567','1234','23','890123','6789012', '5678901'];
  {%H-}ac.Instance := TACSearchFsm.Create(a);
  m := ac.Instance.FindMatches(s);
  AssertTrue(m = nil);

  s := '01234567890123456789';
  m := ac.Instance.FindMatches(s);
  AssertTrue(Length(m) = 13);

  m := ac.Instance.FindMatches(s, smmLeftmostFirst);
  AssertTrue(Length(m) = 2);
  AssertTrue(Copy(s, m[0].Offset, m[0].Length) = '12345');
  AssertTrue(Copy(s, m[1].Offset, m[1].Length) = '6789012');

  m := ac.Instance.FindMatches(s, smmLeftmostLongest);
  AssertTrue(Length(m) = 2);
  AssertTrue(Copy(s, m[0].Offset, m[0].Length) = '1234567');
  AssertTrue(Copy(s, m[1].Offset, m[1].Length) = '890123');

  m := ac.Instance.FindMatches(s, smmLeftmostShortest);
  AssertTrue(Length(m) = 3);
  AssertTrue(Copy(s, m[0].Offset, m[0].Length) = '1234');
  AssertTrue(Copy(s, m[1].Offset, m[1].Length) = '5678901');
  AssertTrue(Copy(s, m[2].Offset, m[2].Length) = '23');
end;

initialization

  RegisterTest(TBmSearchTest);
  RegisterTest(TBmhrSearchTest);
  RegisterTest(TBmSearchCITest);
  RegisterTest(TFunTest);
  RegisterTest(TACSearchFsmTest);

end.

