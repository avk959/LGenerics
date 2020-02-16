program map_bench;

{$mode objfpc}{$H+}
{.$define use_gcontnrs}

uses
  SysUtils, bench_util, hashlist_bench, gcdict_bench, lg_bench,
  {$ifdef use_gcontnrs}gcontnrs_bench,{$endif}
  hashtable_bench, ghashmap_bench;

var
  BenchBy: TBenchBy = bbMedian;
  Seed: Integer = DEFAULT_SEED;
  MapSize: Integer = DEFAULT_SIZE;
  Interval: Integer = DEFAULT_INTERVAL;

procedure ReadSettings;
var
  Tmp: Integer;
begin
  if ParamCount <> 4 then exit;
  //BenchBy
  if TryStrToInt(ParamStr(1), Tmp) and (Tmp in [0..1]) then
    BenchBy := TBenchBy(Tmp);
  //Seed
  if TryStrToInt(ParamStr(2), Tmp) then
    Seed := Tmp;
  //MapSize
  if TryStrToInt(ParamStr(3), Tmp) and (Tmp > 0) then
    MapSize := Tmp;
  //Interval
  if TryStrToInt(ParamStr(4), Tmp) and (Tmp > 0) then
    Interval := Tmp;
end;

begin
  ReadSettings;
  CheckCpuSpeed;
  RunGcDict(Seed, MapSize, Interval, BenchBy);
  RunHashMap(Seed, MapSize, Interval, BenchBy);
  RunHashTable(Seed, MapSize, Interval, BenchBy);
  RunHashList(Seed, MapSize, Interval, BenchBy);
{$ifdef use_gcontnrs}
  RunGenHashMap(Seed, MapSize, Interval, BenchBy);
{$endif}
  RunHashMapLP(Seed, MapSize, Interval, BenchBy);
  RunLiteHashMapLP(Seed, MapSize, Interval, BenchBy);
  RunChainHashMap(Seed, MapSize, Interval, BenchBy);
end.

