unit lg_bench;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, DateUtils, LGHashMap, LGHash, bench_util;

  procedure RunHashMapLP(aSeed, aSize, aInterval: Integer; aBenchBy: TBenchBy);
  procedure RunLiteHashMapLP(aSeed, aSize, aInterval: Integer; aBenchBy: TBenchBy);
  procedure RunChainHashMap(aSeed, aSize, aInterval: Integer; aBenchBy: TBenchBy);

implementation
{$B-}{$coperators on}{$macro on}
{$I RunBenchBody.inc}

type
  TChainMap = specialize TGChainHashMap<string, Integer>;
  TMap      = specialize TGHashMapLP<string, Integer>;
  TLiteMap  = specialize TGLiteHashMapLP<string, Integer, string>;

procedure DoRunHashMapLP(aSeed, aSize, aKeySize, aInterval: Integer; aBenchBy: TBenchBy);
{$define MapTypeMacro := TMap}
{$define MapNameMacro := 'THashMapLP '}
{$define MapCreateMacro := Map := TMap.Create}
{$define MapCleanupMacro :=  Map.Clear}
{$define AddOrSetValueMacro := Map.AddOrSetValue(a[I])}
{$define MapEnsureCapacityMacro := Map.Clear; Map.EnsureCapacity(aSize)}
{$define AddOrSetValue2Macro := Map.AddOrSetValue(a[I])}
{$define MapContainsMacro := Map.Contains(a[I].Key)}
{$define MapClearMacro := Map.Clear}
{$define MapRemoveMacro := Map.Remove(a[I].Key)}
{$define MapCountMacro := Map.Count}
{$define MapFreeMacro := Map.Free}
RunBodyMacro
end;

procedure DoRunLiteHashMap(aSeed, aSize, aKeySize, aInterval: Integer; aBenchBy: TBenchBy);
{$define MapTypeMacro := TLiteMap}
{$define MapNameMacro := 'TLiteHashMapLP '}
{$define MapCreateMacro := }
{$define MapCleanupMacro :=  Map.Clear}
{$define MapFreeMacro := }
RunBodyMacro
end;

procedure DoRunChainHashMap(aSeed, aSize, aKeySize, aInterval: Integer; aBenchBy: TBenchBy);
{$define MapTypeMacro := TChainMap}
{$define MapNameMacro := 'TChainHashMap '}
{$define MapCreateMacro := Map := TChainMap.Create}
{$define MapCleanupMacro :=  Map.Clear}
{$define MapFreeMacro := Map.Free}
RunBodyMacro
end;

procedure RunHashMapLP(aSeed, aSize, aInterval: Integer; aBenchBy: TBenchBy);
var
  KeySize: TKeySize;
begin
  for KeySize in TKeySize do
    DoRunHashMapLP(aSeed, aSize, KeySizes[KeySize], aInterval, aBenchBy);
end;

procedure RunLiteHashMapLP(aSeed, aSize, aInterval: Integer; aBenchBy: TBenchBy);
var
  KeySize: TKeySize;
begin
  for KeySize in TKeySize do
    DoRunLiteHashMap(aSeed, aSize, KeySizes[KeySize], aInterval, aBenchBy);
end;

procedure RunChainHashMap(aSeed, aSize, aInterval: Integer; aBenchBy: TBenchBy);
var
  KeySize: TKeySize;
begin
  for KeySize in TKeySize do
    DoRunChainHashMap(aSeed, aSize, KeySizes[KeySize], aInterval, aBenchBy);
end;

end.

