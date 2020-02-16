unit gcontnrs_bench;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, DateUtils, GContnrs, bench_util;

  procedure RunGenHashMap(aSeed, aSize, aInterval: Integer; aBenchBy: TBenchBy);

implementation
{$B-}{$coperators on}{$macro on}
{$I RunBenchBody.inc}

type

  TMap = class(specialize TGenHashMap<string, Integer>)
  private
    function  GetHash(const aValue: string): Integer;
    function  KeysEqual(const L, R: string) : Boolean;
  public
    procedure AfterConstruction; override;
  end;


function TMap.GetHash(const aValue: string): Integer;
begin
  Result := HashString(aValue);
end;

function TMap.KeysEqual(const L, R: string): Boolean;
begin
  Result := L = R;
end;

procedure TMap.AfterConstruction;
begin
  inherited;
  OnHashKey := @GetHash;
  OnKeysEqual := @KeysEqual;
end;

procedure DoRunGenHashMap(aSeed, aSize, aKeySize, aInterval: Integer; aBenchBy: TBenchBy);
{$define MapTypeMacro := TMap}
{$define MapNameMacro := 'TGenHashMap '}
{$define MapCreateMacro := Map := nil}
{$define MapCleanupMacro :=  Map.Free; Map := TMap.Create }
{$define AddOrSetValueMacro := Map.Include(Key, Value)}
{$define MapEnsureCapacityMacro := Map.Free; Map := TMap.Create(aSize)}
{$define AddOrSetValue2Macro := Map.Include(Key, Value)}
{$define MapContainsMacro := Map.Contains(a[I].Key)}
{$define MapClearMacro := Map.Clear}
{$define MapRemoveMacro := Map.Exclude(a[I].Key)}
{$define MapCountMacro := Map.Size}
{$define MapFreeMacro := Map.Free}
RunBodyMacro
end;

procedure RunGenHashMap(aSeed, aSize, aInterval: Integer; aBenchBy: TBenchBy);
var
  KeySize: TKeySize;
begin
  for KeySize in TKeySize do
    DoRunGenHashMap(aSeed, aSize, KeySizes[KeySize], aInterval, aBenchBy);
end;

end.

