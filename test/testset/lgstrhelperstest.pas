unit LGStrHelpersTest;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  lgArrayHelpers,
  lgStrHelpers;

type
  THelper = specialize TGComparableArrayHelper<SizeInt>;

  { TBmSearchTest }

  TBmSearchTest = class(TTestCase)
  private

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
  private

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
  private

  published
    procedure FindMatches;
    procedure Matches;
    procedure NextMatch;
  end;

 { TFunTest }

 TFunTest = class(TTestCase)
 private
 published
   procedure LevenshteinDist;
   procedure LevenshteinDistMbr;
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

{ TFunTest }

procedure TFunTest.LevenshteinDist;
begin
  AssertTrue(LevDistance('', 'hello') = 5);
  AssertTrue(LevDistance('hello', '') = 5);
  AssertTrue(LevDistance('hello', 'hello') = 0);
  AssertTrue(LevDistance('ab', 'aa') = 1);
  AssertTrue(LevDistance('aa', 'ab') = 1);
  AssertTrue(LevDistance('ab', 'ba') = 2);
  AssertTrue(LevDistance('ba', 'ab') = 2);
  AssertTrue(LevDistance('ab', 'aaa') = 2);
  AssertTrue(LevDistance('a', 'bbb') = 3);
  AssertTrue(LevDistance('aababab','abbaa') = 3);
  AssertTrue(LevDistance('helli', 'elli') = 1);
  AssertTrue(LevDistance('ellia', 'helli') = 2);
  AssertTrue(LevDistance('helli', 'ellia') = 2);
  AssertTrue(LevDistance('kitten', 'sitten') = 1);
  AssertTrue(LevDistance('sitten', 'kitten') = 1);
  AssertTrue(LevDistance('kitten', 'sitting') = 3);
  AssertTrue(LevDistance('distance', 'difference') = 5);
  AssertTrue(LevDistance('levenshtein', 'frankenstein') = 6);
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
end;

initialization

  RegisterTest(TBmSearchTest);
  RegisterTest(TBmhrSearchTest);
  RegisterTest(TBmSearchCITest);
  RegisterTest(TFunTest);

end.

