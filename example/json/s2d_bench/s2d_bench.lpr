program s2d_bench;

{$mode objfpc}{$H+}

uses

  classes, SysUtils, DateUtils, lgUtils, lgJson, PasDblStrUtils;

type
  TS2dFunc   = function(const s: string; out d: Double): Boolean;
  TBenchFunc = specialize TGTuple2<string, TS2dFunc>;

function S2dVal(const s: string; out d: Double): Boolean;
var
  c: Integer;
begin
  Val(s, d, c);
  Result := c = 0;
end;

function S2dPDSU(const s: string; out d: Double): Boolean;
begin
  d := ConvertStringToDouble(s, rmNearest, @Result);
end;

const
  Funcs: array of TBenchFunc = (
    (F1: 'Val                   '; F2: @S2dVal),
    (F1: 'ConvertStringToDouble '; F2: @S2dPDSU),
    (F1: 'TryStr2Double         '; F2: @TryStr2Double));

  TryCount = 5;

procedure Run(const aFileName: string);
var
  Loader: specialize TGAutoRef<TStringList>;
  Lines: TStringArray = nil;
  d: Double;
  Func: TBenchFunc;
  I, J, K, Score, BestScore, Rejected: Integer;
  Start: TTime;
const
  RepCount = 4;
begin
  Loader.Instance.LoadFromFile(aFileName);
  WriteLn(aFileName);
  WriteLn('read ', Loader.Instance.Count, ' lines');
  Lines := Loader.Instance.ToStringArray;
  Loader.Instance.Clear;
  J := 0;
  for I := 0 to High(Lines) do
    J += Length(Lines[I]);
  WriteLn('total size ', J, ' bytes');
  Sleep(2000);
  for Func in Funcs do
    begin
      BestScore := MaxInt;
      for I := 1 to TryCount do
        begin
          Rejected := 0;
          Start := Time;
          for J := 1 to RepCount do
            for K := 0 to High(Lines) do
              if not Func.F2(Lines[K], d) then
                Inc(Rejected);
          Score := MillisecondsBetween(Time, Start);
          if Score < BestScore then
            BestScore := Score;
          if I < TryCount then
            Sleep(2000);
        end;
       WriteLn(StdErr, Func.F1, ' ', BestScore);
       WriteLn(Func.F1, ' ', BestScore);
       WriteLn(Func.F1, ' rejected: ', Rejected);
       if Func.F1 <> Funcs[High(Funcs)].F1 then
         Sleep(10000);
    end;
end;

begin
  Run('uniform.txt');
  Sleep(10000);
  Run('citylots.txt');
end.

