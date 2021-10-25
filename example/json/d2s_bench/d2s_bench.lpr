program d2s_bench;
{$mode objfpc}{$H+}
uses

  SysUtils, DateUtils, lgUtils, lgJson, mormot.core.text, PasDblStrUtils;

type
  TD2sProc   = procedure(aValue: Double; out s: shortstring);
  TD2sFunc   = function(aValue: Double): string;
  TBenchProc = specialize TGTuple2<string, TD2sProc>;
  TBenchFunc = specialize TGTuple2<string, TD2sFunc>;
  TDblArray  = array of Double;

procedure D2sM2(aValue: Double; out s: shortstring);
begin
  DoubleToShort(s, aValue);
end;

procedure D2sStr(aValue: Double; out s: shortstring);
begin
  Str(aValue, s);
end;

procedure D2sF2T(aValue: Double; out s: shortstring);
begin
  SetLength(s, FloatToText(@s[1], aValue, ffGeneral, 16, 0));
end;

procedure D2s(aValue: Double; out s: shortstring);
begin
  Double2Str(aValue, s);
end;

function D2sM2(aValue: Double): string;
begin
  Result := DoubleToStr(aValue);
end;

function D2sStr2(aValue: Double): string;
begin
  Result := '';
  Str(aValue, Result);
end;

function D2sF2S(aValue: Double): string;
begin
  Result := FloatToStr(aValue);
end;

function D2sPDSU(aValue: Double): string;
begin
  Result := RyuDoubleToString(aValue, False);
end;

function D2s(aValue: Double): string;
begin
  Result := Double2Str(aValue);
end;

function  NextRandomDouble: Double;
var
  q: QWord;
  d: Double absolute q;
const
  InfExp = QWord($7ff0000000000000);
begin
  repeat
    q := BJNextRandom64;
  until q and InfExp <> InfExp;
  Result := d;
end;

const
  Procs: array of TBenchProc = (
    (F1: 'DoubleToShort '; F2: @D2sM2),
    (F1: 'Str           '; F2: @D2sStr),
    (F1: 'FloatToText   '; F2: @D2sF2T),
    (F1: 'Double2Str    '; F2: @D2s));

  Funcs: array of TBenchFunc = (
    (F1: 'DoubleToStr       '; F2: @D2sM2),
    (F1: 'Str               '; F2: @D2sStr2),
    (F1: 'FloatToStr        '; F2: @D2sF2S),
    (F1: 'RyuDoubleToString '; F2: @D2sPDSU),
    (F1: 'Double2Str        '; F2: @D2s));

  TryCount = 5;
  TestSize = 1000*1000*10;

function GenTestData: TDblArray;
var
  I: Integer;
begin
  Result := nil;
  BJSetSeed64(2021);
  SetLength(Result, TestSize);
  for I := 0 to High(Result) do
    Result[I] := NextRandomDouble;
end;

procedure RunProcs(const aData: TDblArray);
var
  s: shortstring;
  Proc: TBenchProc;
  I, J, Score, BestScore: Integer;
  Start: TTime;
begin
  for Proc in Procs do
    begin
      BestScore := MaxInt;
      for I := 1 to TryCount do
        begin
          Start := Time;
          for J := 0 to High(aData) do
            Proc.F2(aData[J], s);
          Score := MillisecondsBetween(Time, Start);
          if Score < BestScore then
            BestScore := Score;
          if I < TryCount then
            Sleep(2000);
        end;
       WriteLn(StdErr, Proc.F1, ' ', BestScore);
       WriteLn(Proc.F1, ' ', BestScore);
       Sleep(10000);
    end;
end;

procedure RunFuncs(const aData: TDblArray);
var
  s: string;
  Fun: TBenchFunc;
  I, J, Score, BestScore: Integer;
  Start: TTime;
begin
  for Fun in Funcs do
    begin
      BestScore := MaxInt;
      for I := 1 to TryCount do
        begin
          Start := Time;
          for J := 0 to High(aData) do
            s := Fun.F2(aData[J]);
          Score := MillisecondsBetween(Time, Start);
          if Score < BestScore then
            BestScore := Score;
          if I < TryCount then
            Sleep(2000);
        end;
       WriteLn(StdErr, Fun.F1, ' ', BestScore);
       WriteLn(Fun.F1, ' ', BestScore);
       if Fun.F1 <> Funcs[High(Funcs)].F1 then
         Sleep(10000);
    end;
end;

var
  Data: TDblArray;

begin
  Data := GenTestData;
  Sleep(2000);
  RunProcs(Data);
  RunFuncs(Data);
end.


