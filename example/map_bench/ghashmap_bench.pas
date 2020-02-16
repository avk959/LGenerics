unit ghashmap_bench;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, DateUtils, ghashmap, LGHash, bench_util;

  procedure RunHashMap(aSeed, aSize, aInterval: Integer; aBenchBy: TBenchBy);

implementation
{$B-}{$coperators on}{$macro on}
{$I RunBenchBody.inc}

type
  THash = class
    class function Hash(const aKey: string; n: SizeUInt): SizeUInt; static; inline;
  end;
  TMap = specialize THashMap<string, Integer, THash>;

class function THash.Hash(const aKey: string; n: SizeUInt): SizeUInt;
begin
  Result := SizeUInt(TxxHash32LE.HashStr(aKey)) and (n-1);
end;

procedure DoRunHashMap(aSeed, aSize, aKeySize, aInterval: Integer; aBenchBy: TBenchBy);
{$define MapTypeMacro := TMap}
{$define MapNameMacro := 'THashMap '}
{$define MapCreateMacro := Map := nil}
{$define MapCleanupMacro :=  Map.Free; Map := TMap.Create}
{$define AddOrSetValueMacro := Map.Insert(Key, Value)}
{$define MapEnsureCapacityMacro := Map.Free; Map := TMap.Create(aSize)}
{$define AddOrSetValue2Macro := Map.Insert(Key, Value)}
{$define MapContainsMacro := Map.Contains(a[I].Key)}
{$define MapClearMacro := Map.Free; Map := TMap.Create}
{$define MapRemoveMacro := Map.Delete(a[I].Key)}
{$define MapCountMacro := Map.Size}
{$define MapFreeMacro := Map.Free}
RunBodyMacro
end;

procedure RunHashMap(aSeed, aSize, aInterval: Integer; aBenchBy: TBenchBy);
var
  KeySize: TKeySize;
begin
  for KeySize in TKeySize do
    DoRunHashMap(aSeed, aSize, KeySizes[KeySize], aInterval, aBenchBy);
end;

end.

