program lcs_bench;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNUX}cmem,{$ENDIF}
  LazUtf8, SysUtils, Math, lgStrHelpers, lgSeqUtils;

function LcsDP(const L, R: rawbytestring): rawbytestring;
var
  D: array of array of SizeInt;
  I, J: SizeInt;
  pR: PChar;
begin
  SetLength(D, Succ(Length(L)), Succ(Length(R)));

  for I := 1 to Length(L) do
    for J := 1 to Length(R) do
      if L[I] = R[J] then
        D[I, J] := Succ(D[I-1, J-1])
      else
        D[I, J] := Max(D[I, J-1], D[I-1, J]);

  SetLength(Result, D[Length(L), Length(R)]);
  pR := @Result[Length(Result)];

  I := Length(L);
  J := Length(R);
  while (I > 0) and (J > 0) do
    if L[I] = R[J] then
      begin
        pR^ := L[I];
        Dec(pR);
        Dec(I);
        Dec(J);
      end
    else
      if D[I, J-1] > D[I-1, J] then
        Dec(J)
      else
        Dec(I);
end;

function LcsDPUtf8(const L, R: string): string;
var
  D: array of array of SizeInt;
  LenL, LenR, I, J, PtLen: Integer;
  c: Cardinal;
  pL, pR: PChar;
  sb: TStringBuilder;
begin
  LenL := UTF8Length(L);
  LenR := UTF8Length(R);
  SetLength(D, LenL+1, LenR+1);

  pL := Pointer(L);
  for I := 1 to LenL do begin
    c := UTF8CodepointToUnicode(pL, PtLen);
    Inc(pL, PtLen);
    pR := Pointer(R);
    for J := 1 to LenR do begin
      if UTF8CodepointToUnicode(pR, PtLen) = c then
        D[I, J] := Succ(D[I-1, J-1])
      else
        D[I, J] := Max(D[I, J-1], D[I-1, J]);
      Inc(pR, PtLen);
    end;
  end;

  sb := TStringBuilder.Create;
  try
    I := LenL;
    J := LenR;
    while (I > 0) and (J > 0) do
      if L[I] = R[J] then
        begin
          sb.Append(Utf8Copy(L, I, 1));
          Dec(I);
          Dec(J);
        end
      else
        if D[I, J-1] > D[I-1, J] then
          Dec(J)
        else
          Dec(I);
    Result := Utf8ReverseString(sb.ToString);
  finally
    sb.Free;
  end;
end;

type
  TDataSet = record
    Data: TStringArray;
    Name: string;
    TotalLen,
    AvgLen,
    AvgDist: Integer;
  end;

  TLcsFun  = function(const L, R: rawbytestring): rawbytestring;
  TTestFun = record
    Fun: TLcsFun;
    Name: string;
  end;

  TLcsFunUni  = function(const L, R: string): string;
  TTestFunUni = record
    Fun: TLcsFunUni;
    Name: string;
  end;


const
  RepCount = 100;
  TryCount = 5;
{ the datasets were created by randomly selecting words from a large dictionary }
  TestSet: array of TDataSet = (
    (Data: ({$I small_big.inc});   Name: 'Short/big_dist';    TotalLen: 87047; AvgLen:  88; AvgDist: 76),
    (Data: ({$I small_small.inc}); Name: 'Short/small_dist';  TotalLen: 85881; AvgLen:  86; AvgDist: 10),
    (Data: ({$I mid_big.inc});     Name: 'Medium/big_dist';   TotalLen: 26007; AvgLen: 261; AvgDist: 221),
    (Data: ({$I mid_small.inc});   Name: 'Medium/small_dist'; TotalLen: 51790; AvgLen: 259; AvgDist: 27),
    (Data: ({$I big_big.inc});     Name: 'Long/big_dist';     TotalLen: 41776; AvgLen: 836; AvgDist: 695),
    (Data: ({$I big_small.inc});   Name: 'Long/small_dist';   TotalLen: 81311; AvgLen: 814; AvgDist: 63));

  Funs: array of TTestFun = (
    (Fun: @LcsGus;   Name: 'LcsGus'),
    (Fun: @LcsKr;    Name: 'LcsKr'),
    (Fun: @LcsMyers; Name: 'LcsMyers'),
    (Fun: @LcsDP;    Name: 'Reference_DP'));

  UniFuns: array of TTestFunUni = (
    (Fun: @LcsGusUtf8;   Name: 'LcsGusUtf8'),
    (Fun: @LcsKrUtf8;    Name: 'LcsKrUtf8'),
    (Fun: @LcsMyersUtf8; Name: 'LcsMyersUtf8'),
    (Fun: @LcsDPUtf8;    Name: 'Reference_DP'));

procedure Run;
var
  I, J, K: Integer;
  Lcs: string;
  Start, Score, BestScore: QWord;
  CurrFun: TTestFun;
  CurrSet: TDataSet;
begin
  for CurrSet in TestSet do begin
    for CurrFun in Funs do begin
      BestScore := High(QWord);
      for K := 1 to TryCount do begin
        Start := GetTickCount64;
        for J := 1 to RepCount do begin
          I := 0;
          while I < High(CurrSet.Data) do begin
            Lcs := CurrFun.Fun(CurrSet.Data[I], CurrSet.Data[I+1]);
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
  Lcs: string;
  Start, Score, BestScore: QWord;
  CurrFun: TTestFunUni;
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
            Lcs := CurrFun.Fun(CurrSet.Data[I], CurrSet.Data[I+1]);
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
  Run;
  WriteLn;
  RunUni;
end.

