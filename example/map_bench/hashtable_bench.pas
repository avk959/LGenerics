unit hashtable_bench;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, DateUtils, contnrs, bench_util;

  procedure RunHashTable(aSeed, aSize, aInterval: Integer; aBenchBy: TBenchBy);

implementation
{$B-}{$coperators on}{$macro on}
{$I RunBenchBody.inc}

procedure DoRunHashTable(aSeed, aSize, aKeySize, aInterval: Integer; aBenchBy: TBenchBy);
{$define MapTypeMacro := TFPDataHashTable}
{$define MapNameMacro := 'TFPDataHashTable '}
{$define MapCreateMacro := Map := nil}
{$define MapCleanupMacro := Map.Free; Map := TFPDataHashTable.Create}
{$define AddOrSetValueMacro :=
  begin
    Map[Key] := Pointer(Value);
    if Map.Count >= Map.HashTableSize then
      Map.HashTableSize := Map.Count * 2;
  end
}
{$define MapEnsureCapacityMacro := Map.Free; Map := TFPDataHashTable.CreateWith(aSize, @RSHash)}
{$define AddOrSetValue2Macro := Map[Key] := Pointer(Value)}
{$define MapContainsMacro := Map.Find(a[I].Key) <> nil}
{$define MapClearMacro := Map.Clear}
{$define MapRemoveMacro := Map.Delete(a[I].Key)}
{$define MapCountMacro := Map.Count}
{$define MapFreeMacro := Map.Free}
RunBodyMacro
end;

procedure RunHashTable(aSeed, aSize, aInterval: Integer; aBenchBy: TBenchBy);
var
  KeySize: TKeySize;
begin
  for KeySize in TKeySize do
    DoRunHashTable(aSeed, aSize, KeySizes[KeySize], aInterval, aBenchBy);
end;

end.

