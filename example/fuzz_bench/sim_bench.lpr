program sim_bench;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNUX}cmem,{$ENDIF}
  LazUtf8, SysUtils, lgUtils, lgStrHelpers, lgSeqUtils;

type
  TDataSet = record
    Data: TStringArray;
    Name: string;
    TotalLen,
    AvgLen,
    AvgDist: Integer;
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

procedure Run;
var
  I, J, K: Integer;
  s: Double;
  Start, Score, BestScore: QWord;
  CurrSet: TDataSet;
begin
  for CurrSet in TestSet do begin
    BestScore := High(QWord);
    for K := 1 to TryCount do begin
      Start := GetTickCount64;
      for J := 1 to RepCount do begin
        I := 0;
        while I < High(CurrSet.Data) do begin
          s := SimRatioEx(CurrSet.Data[I], CurrSet.Data[I+1], smTokenSetEx, [' ']);
          Inc(I, 2);
        end;
      end;
      Score := GetTickCount64 - Start;
      if Score < BestScore then
        BestScore := Score;
    end;
    WriteLn(StdErr, CurrSet.Name, ' ', BestScore);
    WriteLn(CurrSet.Name, ' ', BestScore);
  end;
end;

procedure RunUni;
var
  I, J, K: Integer;
  s: Double;
  Start, Score, BestScore: QWord;
  CurrSet: TDataSet;
begin
  for CurrSet in TestSet do begin
    BestScore := High(QWord);
    for K := 1 to TryCount do begin
      Start := GetTickCount64;
      for J := 1 to RepCount do begin
        I := 0;
        while I < High(CurrSet.Data) do begin
          s := SimRatioExUtf8(CurrSet.Data[I], CurrSet.Data[I+1], [' '], smTokenSetEx);
          Inc(I, 2);
        end;
      end;
      Score := GetTickCount64 - Start;
      if Score < BestScore then
        BestScore := Score;
    end;
    WriteLn(StdErr, CurrSet.Name, ' ', BestScore);
    WriteLn(CurrSet.Name, ' ', BestScore);
  end;
end;

begin
  Run;
  WriteLn;
  RunUni;
end.

