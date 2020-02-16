unit hashlist_bench;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, DateUtils, contnrs, LGUtils, LGArrayHelpers, bench_util;

  procedure RunHashList(aSeed, aSize, aInterval: Integer; aBenchBy: TBenchBy);


implementation
{$B-}{$coperators on}{$macro on}
{$I RunBenchBody.inc}

type
  TMapEntry    = specialize TGMapEntry<shortstring, Integer>;
  TEntryHelper = specialize TGArrayHelpUtil<TMapEntry>;
  TEntryArray  = array of TMapEntry;

  TMap  = class(TFPHashList)
    procedure AddOrSetValue(const aKey: shortstring; aValue: Integer);
  end;

{ TStrHashList }

procedure TMap.AddOrSetValue(const aKey: shortstring; aValue: Integer);
var
  Found: Integer;
begin
  Found := FindIndexOf(aKey);
  if Found >= 0 then
    Items[Found] := Pointer(aValue)
  else
    Add(aKey, Pointer(aValue));
end;

function RandomString(aLength: Integer): shortstring;
begin
  Result := bench_util.RandomString(aLength);
end;

procedure DoRunHashList(aSeed, aSize, aKeySize, aInterval: Integer; aBenchBy: TBenchBy);
{$define MapTypeMacro := TMap}
{$define MapNameMacro := 'TFPHashList '}
{$define MapCreateMacro := Map := TMap.Create}
{$define MapCleanupMacro := Map.Clear}
{$define AddOrSetValueMacro := Map.AddOrSetValue(Key, Value)}
{$define MapEnsureCapacityMacro := Map.Clear; Map.Capacity := aSize}
{$define AddOrSetValue2Macro := Map.AddOrSetValue(Key, Value)}
{$define MapContainsMacro := Map.FindIndexOf(a[I].Key) >= 0}
{$define MapClearMacro := Map.Clear}
{$define MapRemoveMacro := begin Fail := True; break end}
{$define MapCountMacro := Map.Count}
{$define MapFreeMacro := Map.Free}
RunBodyMacro
end;

procedure RunHashList(aSeed, aSize, aInterval: Integer; aBenchBy: TBenchBy);
var
  KeySize: TKeySize;
begin
  for KeySize in TKeySize do
    DoRunHashList(aSeed, aSize, KeySizes[KeySize], aInterval, aBenchBy);
end;

end.

