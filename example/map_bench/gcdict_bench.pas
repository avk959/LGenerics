unit gcdict_bench;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, DateUtils, Generics.Collections, bench_util;

  procedure RunGcDict(aSeed, aSize, aInterval: Integer; aBenchBy: TBenchBy);

implementation
{$B-}{$coperators on}{$macro on}
{$I RunBenchBody.inc}

type
  TMap = specialize TDictionary<string, Integer>;

procedure DoRunGcDict(aSeed, aSize, aKeySize, aInterval: Integer; aBenchBy: TBenchBy);
{$define MapTypeMacro := TMap}
{$define MapNameMacro := 'TDictionary '}
{$define MapCreateMacro := Map := TMap.Create}
{$define MapCleanupMacro := Map.Clear}
{$define AddOrSetValueMacro := Map.AddOrSetValue(Key, Value)}
{$define MapEnsureCapacityMacro := Map.Clear; Map.Capacity := Trunc(aSize/Map.MaxLoadFactor)+1}
{$define AddOrSetValue2Macro := Map.AddOrSetValue(Key, Value)}
{$define MapContainsMacro := Map.ContainsKey(a[I].Key)}
{$define MapClearMacro := Map.Clear}
{$define MapRemoveMacro := Map.Remove(a[I].Key)}
{$define MapCountMacro := Map.Count}
{$define MapFreeMacro := Map.Free}
RunBodyMacro
end;

procedure RunGcDict(aSeed, aSize, aInterval: Integer; aBenchBy: TBenchBy);
var
  KeySize: TKeySize;
begin
  for KeySize in TKeySize do
    DoRunGcDict(aSeed, aSize, KeySizes[KeySize], aInterval, aBenchBy);
end;

end.

