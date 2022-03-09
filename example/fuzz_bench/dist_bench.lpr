program dist_bench;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNUX}cmem,{$ENDIF}
  LazUtf8, SysUtils, Math, lgUtils, lgStrHelpers, lgSeqUtils;

function LevenshteinDistUtf8(const L, R : string): SizeInt;
var
  D: array of array of Integer = nil;
  LenL, LenR, I, J, PtLen: Integer;
  c: Cardinal;
  pL, pR: PChar;
begin
  LenL := UTF8Length(L);
  LenR := UTF8Length(R);
  SetLength(D, LenL+1, LenR+1);

  for I := 1 to LenR do D[0, I] := I;
  for I := 1 to LenL do D[I, 0] := I;

  pL := Pointer(L);
  for I := 1 to LenL do begin
    c := UTF8CodepointToUnicode(pL, PtLen);
    Inc(pL, PtLen);
    pR := Pointer(R);
    for J := 1 to LenR do begin
      if UTF8CodepointToUnicode(pR, PtLen) = c then
        D[I, J] := D[I-1, J-1] //match
      else
        D[I, J] :=
          Math.Min(D[I-1, J],  //deletion
          Math.Min(D[I, J-1],  //insertion
                   D[I-1, J-1] //substitution
          )) + 1;
      Inc(pR, PtLen);
    end;
  end;

  Result := D[LenL, LenR];
end;

{ from Rosetta Code }
function LevenshteinDistance(const s, t: rawbytestring): SizeInt;
var
  d: array of array of integer;
  i, j, n, m: integer;
begin
  n := length(t);
  m := length(s);
  setlength(d, m+1, n+1);

  for i := 0 to m do
    d[i,0] := i;
  for j := 0 to n do
    d[0,j] := j;
  for j := 1 to n do
    for i := 1 to m do
      if s[i] = t[j] then
        d[i,j] := d[i-1,j-1]
      else
        d[i,j] := min(d[i-1,j] + 1, min(d[i,j-1] + 1, d[i-1,j-1] + 1));
  LevenshteinDistance := d[m,n];
end;


type
  TDataSet = record
    Data: TStringArray;
    Name: string;
    TotalLen,
    AvgLen,
    AvgDist: Integer;
  end;

  TSbDistFun = function(const L, R: rawbytestring): SizeInt;
  TDistFun   = function(const L, R: string): SizeInt;
  TSbTestFun = record
    Fun: TSbDistFun;
    Name: string;
  end;
  TTestFun   = record
    Fun: TDistFun;
    Name: string;
  end;


const
  RepCount = 500;
  TryCount = 5;
{ the datasets were created by randomly selecting words from a large dictionary }
  TestSet: array of TDataSet = (
    (Data: ({$I small_big.inc});   Name: 'Short/big_dist';    TotalLen: 87047; AvgLen:  87; AvgDist: 76),
    (Data: ({$I small_small.inc}); Name: 'Short/small_dist';  TotalLen: 85881; AvgLen:  86; AvgDist: 10),
    (Data: ({$I mid_big.inc});     Name: 'Medium/big_dist';   TotalLen: 26007; AvgLen: 260; AvgDist: 221),
    (Data: ({$I mid_small.inc});   Name: 'Medium/small_dist'; TotalLen: 51790; AvgLen: 259; AvgDist: 27),
    (Data: ({$I big_big.inc});     Name: 'Long/big_dist';     TotalLen: 41776; AvgLen: 836; AvgDist: 695),
    (Data: ({$I big_small.inc});   Name: 'Long/small_dist';   TotalLen: 81311; AvgLen: 813; AvgDist: 63));

  SbFuns: array of TSbTestFun = (
    (Fun: @LcsDistanceMyers;    Name: 'LcsDistanceMyers'),
    (Fun: @LevDistanceMbr;      Name: 'LevDistanceMbr'),
    (Fun: @LevDistanceMyers;    Name: 'LevDistanceMyers'),
    (Fun: @LevenshteinDistance; Name: 'Reference_DP'));

  UniFuns: array of TTestFun = (
    (Fun: @LcsDistanceMyersUtf8; Name: 'LcsDistanceMyersUtf8'),
    (Fun: @LevDistanceMbrUtf8;   Name: 'LevDistanceMbrUtf8'),
    (Fun: @LevDistanceMyersUtf8; Name: 'LevDistanceMyersUtf8'),
    (Fun: @LevenshteinDistUtf8;  Name: 'Reference_DP'));

procedure RunSb;
var
  I, J, K: Integer;
  d: SizeInt;
  Start, Score, BestScore: QWord;
  CurrFun: TSbTestFun;
  CurrSet: TDataSet;
begin
  for CurrSet in TestSet do begin
    for CurrFun in SbFuns do begin
      BestScore := High(QWord);
      for K := 1 to TryCount do begin
        Start := GetTickCount64;
        for J := 1 to RepCount do begin
          I := 0;
          while I < High(CurrSet.Data) do begin
            d := CurrFun.Fun(CurrSet.Data[I], CurrSet.Data[I+1]);
            Inc(I, 2);
          end;
        end;
        Score := GetTickCount64 - Start;
        if Score < BestScore then
          BestScore := Score;
      end;
      WriteLn(StdErr, CurrSet.Name, ' ', CurrFun.Name, ' ', BestScore);
      WriteLn(CurrSet.Name, ' ', CurrFun.Name, ' ', BestScore);
    end;
  end;
end;

procedure RunUni;
var
  I, J, K: Integer;
  d: SizeInt;
  Start, Score, BestScore: QWord;
  CurrFun: TTestFun;
  CurrSet: TDataSet;
begin
  for CurrSet in TestSet do begin
    for CurrFun in UniFuns do begin
      BestScore := High(QWord);
      for K := 1 to TryCount do begin
        Start := GetTickCount64;
        for J := 1 to RepCount do begin
          I := 0;
          while I < High(CurrSet.Data) do begin
            d := CurrFun.Fun(CurrSet.Data[I], CurrSet.Data[I+1]);
            Inc(I, 2);
          end;
        end;
        Score := GetTickCount64 - Start;
        if Score < BestScore then
          BestScore := Score;
      end;
      WriteLn(StdErr, CurrSet.Name, ' ', CurrFun.Name, ' ', BestScore);
      WriteLn(CurrSet.Name, ' ', CurrFun.Name, ' ', BestScore);
    end;
  end;
end;

begin
  RunSb;
  WriteLn;
  RunUni;
end.

