unit LGStrHelpersTest;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, Math,
  lgUtils,
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
   procedure LevenshteinDist2;
   procedure LevenshteinDistMbr;
   procedure LevenshteinDistMbr2;
   procedure LevenshteinDistMbrLimit;
   procedure LcsGusTest;
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

procedure TFunTest.LevenshteinDistMbrLimit;
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
  s1 := 'thisisatest';
  s2 := 'testing123testing';
  AssertTrue(LcsDp(s1, s2) = 'tsitest');
  AssertTrue(LcsGus(s1, s2) = 'tsitest');
  s1 := '1234';
  s2 := '1224533324';
  AssertTrue(LcsDp(s1, s2) = '1234');
  AssertTrue(LcsGus(s1, s2) = '1234');
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

initialization

  RegisterTest(TBmSearchTest);
  RegisterTest(TBmhrSearchTest);
  RegisterTest(TBmSearchCITest);
  RegisterTest(TFunTest);

end.

