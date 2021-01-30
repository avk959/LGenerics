unit LGMultimapTest;

{$mode objfpc}{$H+}

interface
uses
  SysUtils, fpcunit, testregistry,
  LGUtils,
  LGAbstractContainer,
  LGMultiMap,
  LGArrayHelpers;

type
  TStrEntry  = specialize TGMapEntry<string, string>;
  TCursor    = specialize TGArrayCursor<TStrEntry>;
  TStrCursor = specialize TGArrayCursor<string>;
  THelper    = specialize TGArrayHelpUtil<TStrEntry>;
  TStrHelper = specialize TGComparableArrayHelper<string>;
  TStrArray  = specialize TGArray<string>;

  TGHashMultiMapTest = class(TTestCase)
  private
  type
    TMultiMap     = class(specialize TGHashMultiMap2<string, string>);
    TAutoMultiMap = specialize TGAutoRef<TMultiMap>;

  published
    procedure TestCreate;
    procedure CreateArray;
    procedure CreateEnum;
    procedure CreateCapacity;
    procedure CreateCapacityArray;
    procedure CreateCapacityEnum;
    procedure Clear;
    procedure EnsureCapacity;
    procedure TrimToFit;
    procedure Add;
    procedure AddArray;
    procedure AddEnum;
    procedure AddValuesArray;
    procedure AddValuesEnum;
    procedure Remove;
    procedure RemoveAll;
    procedure RemoveKey;
    procedure RemoveKeys;
    procedure RemoveValues;
    procedure Items;
    procedure InIteration;
  end;

  TGTreeMultiMapTest = class(TTestCase)
  private
  type
    TMultiMap     = class(specialize TGTreeMultiMap2<string, string>);
    TAutoMultiMap = specialize TGAutoRef<TMultiMap>;

  published
    procedure TestCreate;
    procedure CreateArray;
    procedure CreateEnum;
    procedure CreateCapacity;
    procedure CreateCapacityArray;
    procedure CreateCapacityEnum;
    procedure Clear;
    procedure EnsureCapacity;
    procedure TrimToFit;
    procedure Add;
    procedure AddArray;
    procedure AddEnum;
    procedure AddValuesArray;
    procedure AddValuesEnum;
    procedure Remove;
    procedure RemoveAll;
    procedure RemoveKey;
    procedure RemoveKeys;
    procedure RemoveValues;
    procedure Items;
    procedure InIteration;
  end;

  TGListMultiMapTest = class(TTestCase)
  private
  type
    TMultiMap     = class(specialize TGListMultiMap2<string, string>);
    TAutoMultiMap = specialize TGAutoRef<TMultiMap>;

  published
    procedure TestCreate;
    procedure CreateArray;
    procedure CreateEnum;
    procedure CreateCapacity;
    procedure CreateCapacityArray;
    procedure CreateCapacityEnum;
    procedure Clear;
    procedure EnsureCapacity;
    procedure TrimToFit;
    procedure Add;
    procedure AddArray;
    procedure AddEnum;
    procedure AddValuesArray;
    procedure AddValuesEnum;
    procedure Remove;
    procedure RemoveAll;
    procedure RemoveKey;
    procedure RemoveKeys;
    procedure RemoveValues;
    procedure Items;
    procedure InIteration;
  end;

  TGLiteMultiMapTest = class(TTestCase)
  private
  type
    TMultiMap = specialize TGLiteHashMultiMap<string, string, string>;

  published
    procedure Clear;
    procedure EnsureCapacity;
    procedure TrimToFit;
    procedure Add;
    procedure AddArray;
    procedure AddEnum;
    procedure AddValuesArray;
    procedure AddValuesEnum;
    procedure Remove;
    procedure Items;
  end;

implementation
{$B-}{$COPERATORS ON}{$WARNINGS OFF}

const
  Array12: array[1..12] of TStrEntry =
    ((Key: 'fish'; Value: 'pike'), (Key: 'fish'; Value: 'crucian'), (Key: 'fish'; Value: 'ruff'),
     (Key: 'bird'; Value: 'sparrow'), (Key: 'bird'; Value: 'swift'), (Key: 'bird'; Value: 'raven'),
     (Key: 'reptile'; Value: 'turtle'), (Key: 'reptile'; Value: 'alligator'), (Key: 'reptile'; Value: 'snake'),
     (Key: 'insect'; Value: 'ant'), (Key: 'insect'; Value: 'fly'), (Key: 'insect'; Value: 'hornet'));

  ArrayKeys:   array[1..4] of string = ('fish','bird','reptile','insect');
  ArrayFish3:  array[1..3] of string = ('pike', 'crucian', 'ruff');
  ArrayFish4:  array[1..4] of string = ('shark', 'perch', 'tuna', 'burbot');
  ArrayPlant4: array[1..4] of string = ('nettle', 'cedar', 'wheat', 'peach');

function EntriesEqual(const L, R: TStrEntry): Boolean;
begin
  Result := (L.Key = R.Key) and (L.Value = R.Value)
end;

function ArrayContainsEntry(const a: array of TStrEntry; const e: TStrEntry): Boolean;
begin
  Result := THelper.SequentSearch(a, e, @EntriesEqual) >= 0;
end;

{ TGHashMultiMapTest }

procedure TGHashMultiMapTest.TestCreate;
var
  m: TAutoMultiMap;
begin
  AssertTrue(m.Instance.Count = 0);
  AssertTrue(m.Instance.LoadFactor = m.Instance.DefaultLoadFactor);
end;

procedure TGHashMultiMapTest.CreateArray;
var
  m: TAutoMultiMap;
  e: TStrEntry;
  s: string;
begin
  m.Instance := TMultiMap.Create(Array12);
  AssertTrue(m.Instance.Count = 12);
  AssertTrue(m.Instance.KeyCount = 4);
  for s in m.Instance.Keys do
    AssertTrue(m.Instance.ValueCount(s) = 3);
  for e in Array12 do
    begin
      AssertTrue(m.Instance.Contains(e.Key));
      AssertTrue(m.Instance.ContainsValue(e.Key, e.Value));
    end;
  for e in m.Instance.Entries do
    AssertTrue(ArrayContainsEntry(Array12, e));
end;

procedure TGHashMultiMapTest.CreateEnum;
var
  m: TAutoMultiMap;
  e: TStrEntry;
  s: string;
begin
  m.Instance := TMultiMap.Create(TCursor.Create(THelper.CreateCopy(Array12)));
  AssertTrue(m.Instance.Count = 12);
  AssertTrue(m.Instance.KeyCount = 4);
  for s in m.Instance.Keys do
    AssertTrue(m.Instance.ValueCount(s) = 3);
  for e in Array12 do
    begin
      AssertTrue(m.Instance.Contains(e.Key));
      AssertTrue(m.Instance.ContainsValue(e.Key, e.Value));
    end;
  for e in m.Instance.Entries do
    AssertTrue(ArrayContainsEntry(Array12, e));
end;

procedure TGHashMultiMapTest.CreateCapacity;
var
  m: TAutoMultiMap;
begin
  m.Instance := TMultiMap.Create(45);
  AssertTrue(m.Instance.Count = 0);
  AssertTrue(m.Instance.Capacity >= 45);
  AssertTrue(m.Instance.LoadFactor = m.Instance.DefaultLoadFactor);
end;

procedure TGHashMultiMapTest.CreateCapacityArray;
var
  m: TAutoMultiMap;
  e: TStrEntry;
  s: string;
begin
  m.Instance := TMultiMap.Create(50, Array12);
  AssertTrue(m.Instance.Count = 12);
  AssertTrue(m.Instance.KeyCount = 4);
  AssertTrue(m.Instance.Capacity >= 50);
  AssertTrue(m.Instance.LoadFactor = m.Instance.DefaultLoadFactor);
  for s in m.Instance.Keys do
    AssertTrue(m.Instance.ValueCount(s) = 3);
  for e in Array12 do
    begin
      AssertTrue(m.Instance.Contains(e.Key));
      AssertTrue(m.Instance.ContainsValue(e.Key, e.Value));
    end;
  for e in m.Instance.Entries do
    AssertTrue(ArrayContainsEntry(Array12, e));
end;

procedure TGHashMultiMapTest.CreateCapacityEnum;
var
  m: TAutoMultiMap;
  e: TStrEntry;
  s: string;
begin
  m.Instance := TMultiMap.Create(50, TCursor.Create(THelper.CreateCopy(Array12)));
  AssertTrue(m.Instance.Count = 12);
  AssertTrue(m.Instance.KeyCount = 4);
  AssertTrue(m.Instance.Capacity >= 50);
  AssertTrue(m.Instance.LoadFactor = m.Instance.DefaultLoadFactor);
  for s in m.Instance.Keys do
    AssertTrue(m.Instance.ValueCount(s) = 3);
  for e in Array12 do
    begin
      AssertTrue(m.Instance.Contains(e.Key));
      AssertTrue(m.Instance.ContainsValue(e.Key, e.Value));
    end;
  for e in m.Instance.Entries do
    AssertTrue(ArrayContainsEntry(Array12, e));
end;

procedure TGHashMultiMapTest.Clear;
var
  m: TAutoMultiMap;
begin
  m.Instance := TMultiMap.Create(Array12);
  AssertTrue(m.Instance.Count = 12);
  AssertTrue(m.Instance.KeyCount = 4);
  AssertTrue(m.Instance.Capacity >= 4);
  m.Instance.Clear;
  AssertTrue(m.Instance.Count = 0);
  AssertTrue(m.Instance.KeyCount = 0);
  AssertTrue(m.Instance.Capacity = 0);
end;

procedure TGHashMultiMapTest.EnsureCapacity;
var
  m: TAutoMultiMap;
  c: SizeInt;
begin
  c := m.Instance.ExpandTreshold;
  m.Instance.EnsureCapacity(c + 1);
  AssertTrue(m.Instance.ExpandTreshold > c);
end;

procedure TGHashMultiMapTest.TrimToFit;
var
  m: TAutoMultiMap;
  c: SizeInt;
begin
  m.Instance := TMultiMap.Create(Array12);
  c := m.Instance.Capacity;
  m.Instance.EnsureCapacity(c+1);
  AssertTrue(m.Instance.Capacity > c);
  m.Instance.TrimToFit;
  AssertTrue(m.Instance.Capacity = 8);
end;

procedure TGHashMultiMapTest.Add;
var
  m: TAutoMultiMap;
begin
  AssertTrue(m.Instance.Count = 0);
  AssertTrue(m.Instance.Add('key1', 'value1'));
  AssertTrue(m.Instance.Count = 1);
  AssertTrue(m.Instance.KeyCount = 1);
  AssertTrue(m.Instance.Contains('key1'));
  AssertTrue(m.Instance.ContainsValue('key1', 'value1'));
  AssertFalse(m.Instance.Add('key1', 'value1'));
  AssertTrue(m.Instance.Add(TStrEntry.Create('key1', 'value2')));
  AssertTrue(m.Instance.Count = 2);
  AssertTrue(m.Instance.KeyCount = 1);
  AssertTrue(m.Instance.ContainsValue('key1', 'value2'));
  AssertTrue(m.Instance.Add('key2', 'value1'));
  AssertTrue(m.Instance.Count = 3);
  AssertTrue(m.Instance.KeyCount = 2);
  AssertFalse(m.Instance.Add('key2', 'value1'));
  AssertTrue(m.Instance.Add(TStrEntry.Create('key2', 'value2')));
  AssertTrue(m.Instance.Count = 4);
  AssertTrue(m.Instance.KeyCount = 2);
end;

procedure TGHashMultiMapTest.AddArray;
var
  m: TAutoMultiMap;
  e: TStrEntry;
  s: string;
begin
  AssertTrue(m.Instance.AddAll(Array12) = Length(Array12));
  AssertTrue(m.Instance.Count = 12);
  AssertTrue(m.Instance.KeyCount = 4);
  for s in m.Instance.Keys do
    AssertTrue(m.Instance.ValueCount(s) = 3);
  for e in Array12 do
    begin
      AssertTrue(m.Instance.Contains(e.Key));
      AssertTrue(m.Instance.ContainsValue(e.Key, e.Value));
    end;
  for e in m.Instance.Entries do
    AssertTrue(ArrayContainsEntry(Array12, e));
  AssertTrue(m.Instance.AddAll(Array12) = 0);
end;

procedure TGHashMultiMapTest.AddEnum;
var
  m: TAutoMultiMap;
  e: TStrEntry;
  s: string;
begin
  AssertTrue(m.Instance.AddAll(TCursor.Create(THelper.CreateCopy(Array12))) = Length(Array12));
  AssertTrue(m.Instance.Count = 12);
  AssertTrue(m.Instance.KeyCount = 4);
  for s in m.Instance.Keys do
    AssertTrue(m.Instance.ValueCount(s) = 3);
  for e in Array12 do
    begin
      AssertTrue(m.Instance.Contains(e.Key));
      AssertTrue(m.Instance.ContainsValue(e.Key, e.Value));
    end;
  for e in m.Instance.Entries do
    AssertTrue(ArrayContainsEntry(Array12, e));
  AssertTrue(m.Instance.AddAll(TCursor.Create(THelper.CreateCopy(Array12))) = 0);
end;

procedure TGHashMultiMapTest.AddValuesArray;
var
  m: TAutoMultiMap;
  s: string;
begin
  m.Instance.AddAll(Array12);
  AssertTrue(m.Instance.AddValues('fish', ArrayFish4) = 4);
  AssertTrue(m.Instance.Count = 16);
  AssertTrue(m.Instance.KeyCount = 4);
  for s in ArrayFish4 do
    AssertTrue(m.Instance.ContainsValue('fish', s));
  AssertTrue(m.Instance.AddValues('fish', ArrayFish4) = 0);
  AssertTrue(m.Instance.AddValues('plant', ArrayPlant4) = 4);
  AssertTrue(m.Instance.Count = 20);
  AssertTrue(m.Instance.KeyCount = 5);
  for s in ArrayPlant4 do
    AssertTrue(m.Instance.ContainsValue('plant', s));
  AssertTrue(m.Instance.AddValues('plant', ArrayPlant4) = 0);
end;

procedure TGHashMultiMapTest.AddValuesEnum;
var
  m: TAutoMultiMap;
  s: string;
begin
  m.Instance.AddAll(Array12);
  AssertTrue(m.Instance.AddValues('fish', TStrCursor.Create(TStrHelper.CreateCopy(ArrayFish4))) = 4);
  AssertTrue(m.Instance.Count = 16);
  AssertTrue(m.Instance.KeyCount = 4);
  for s in ArrayFish4 do
    AssertTrue(m.Instance.ContainsValue('fish', s));
  AssertTrue(m.Instance.AddValues('fish', TStrCursor.Create(TStrHelper.CreateCopy(ArrayFish4))) = 0);
  AssertTrue(m.Instance.AddValues('plant', TStrCursor.Create(TStrHelper.CreateCopy(ArrayPlant4))) = 4);
  AssertTrue(m.Instance.Count = 20);
  AssertTrue(m.Instance.KeyCount = 5);
  for s in ArrayPlant4 do
    AssertTrue(m.Instance.ContainsValue('plant', s));
  AssertTrue(m.Instance.AddValues('plant', TStrCursor.Create(TStrHelper.CreateCopy(ArrayPlant4))) = 0);
end;

procedure TGHashMultiMapTest.Remove;
var
  m: TAutoMultiMap;
begin
  m.Instance.AddAll(Array12);
  AssertTrue(m.Instance.Remove('fish', 'pike'));
  AssertTrue(m.Instance.Remove('bird', 'swift'));
  AssertTrue(m.Instance.Remove('reptile', 'snake'));
  AssertTrue(m.Instance.Remove('insect', 'ant'));
  AssertTrue(m.Instance.KeyCount = 4);
  AssertTrue(m.Instance.Count = 8);
  AssertTrue(m.Instance.Remove('fish', 'crucian'));
  AssertTrue(m.Instance.Remove('fish', 'ruff'));
  AssertTrue(m.Instance.KeyCount = 3);
  AssertTrue(m.Instance.Count = 6);
end;

procedure TGHashMultiMapTest.RemoveAll;
var
  m: TAutoMultiMap;
begin
  AssertTrue(m.Instance.RemoveAll(Array12) = 0);
  m.Instance.AddAll(Array12);
  AssertTrue(m.Instance.RemoveAll(Array12) = 12);
  AssertTrue(m.Instance.IsEmpty);
  m.Instance.AddAll(Array12);
  AssertTrue(m.Instance.RemoveAll(TCursor.Create(THelper.CreateCopy(Array12))) = 12);
  AssertTrue(m.Instance.IsEmpty);
end;

procedure TGHashMultiMapTest.RemoveKey;
var
  m: TAutoMultiMap;
begin
  AssertTrue(m.Instance.RemoveKey('fish') = 0);
  m.Instance.AddAll(Array12);
  AssertTrue(m.Instance.RemoveKey('plant') = 0);
  AssertTrue(m.Instance.RemoveKey('fish') = 3);
  AssertTrue(m.Instance.RemoveKey('bird') = 3);
  AssertTrue(m.Instance.RemoveKey('reptile') = 3);
  AssertTrue(m.Instance.RemoveKey('insect') = 3);
  AssertTrue(m.Instance.IsEmpty);
end;

procedure TGHashMultiMapTest.RemoveKeys;
var
  m: TAutoMultiMap;
begin
  AssertTrue(m.Instance.RemoveKeys(ArrayKeys) = 0);
  m.Instance.AddAll(Array12);
  AssertTrue(m.Instance.RemoveKeys(ArrayKeys[1..2]) = 6);
  AssertTrue(m.Instance.KeyCount = 2);
  AssertTrue(m.Instance.Count = 6);
  AssertTrue(m.Instance.RemoveKeys(ArrayKeys) = 6);
  AssertTrue(m.Instance.IsEmpty);
end;

procedure TGHashMultiMapTest.RemoveValues;
var
  m: TAutoMultiMap;
begin
  AssertTrue(m.Instance.RemoveValues('fish', ArrayFish3) = 0);
  m.Instance.AddAll(Array12);
  AssertTrue(m.Instance.RemoveValues('bird', ArrayFish3) = 0);
  AssertTrue(m.Instance.RemoveValues('fish', ArrayFish3) = 3);
  AssertTrue(m.Instance.KeyCount = 3);
  AssertTrue(m.Instance.Count = 9);
  AssertTrue(m.Instance.AddAll(Array12) = 3);
  AssertTrue(m.Instance.RemoveValues('fish', TStrCursor.Create(TStrHelper.CreateCopy(ArrayFish3))) = 3);
  AssertTrue(m.Instance.Count = 9);
  AssertTrue(m.Instance.KeyCount = 3);
end;

procedure TGHashMultiMapTest.Items;
var
  m: TAutoMultiMap;
  k, v: string;
  I: Integer = 0;
begin
  for k in m.Instance.Keys do
    for v in m.Instance[k] do
      Inc(I);
  AssertTrue(I = 0);
  m.Instance.AddAll(Array12);
  I := 0;
  for k in m.Instance.Keys do
    for v in m.Instance[k] do
      begin
        AssertTrue(m.Instance.ContainsValue(k, v));
        Inc(I);
      end;
  AssertTrue(I = 12);
end;

procedure TGHashMultiMapTest.InIteration;
var
  m: TAutoMultiMap;
  Raised: Boolean = False;
  se: TStrEntry;
begin
  m.Instance.AddAll(Array12);
  try
    for se in m.Instance.Entries do
      m.Instance.Add(se);
  except
    on e: ELGUpdateLock do
      Raised := True;
  end;
  AssertTrue(Raised);
end;

{ TGTreeMultiMapTest }

procedure TGTreeMultiMapTest.TestCreate;
var
  m: TAutoMultiMap;
begin
  AssertTrue(m.Instance.Count = 0);
  AssertTrue(m.Instance.LoadFactor = m.Instance.DefaultLoadFactor);
end;

procedure TGTreeMultiMapTest.CreateArray;
var
  m: TAutoMultiMap;
  e: TStrEntry;
  s: string;
begin
  m.Instance := TMultiMap.Create(Array12);
  AssertTrue(m.Instance.Count = 12);
  AssertTrue(m.Instance.KeyCount = 4);
  for s in m.Instance.Keys do
    AssertTrue(m.Instance.ValueCount(s) = 3);
  for e in Array12 do
    begin
      AssertTrue(m.Instance.Contains(e.Key));
      AssertTrue(m.Instance.ContainsValue(e.Key, e.Value));
    end;
  for e in m.Instance.Entries do
    AssertTrue(ArrayContainsEntry(Array12, e));
end;

procedure TGTreeMultiMapTest.CreateEnum;
var
  m: TAutoMultiMap;
  e: TStrEntry;
  s: string;
begin
  m.Instance := TMultiMap.Create(TCursor.Create(THelper.CreateCopy(Array12)));
  AssertTrue(m.Instance.Count = 12);
  AssertTrue(m.Instance.KeyCount = 4);
  for s in m.Instance.Keys do
    AssertTrue(m.Instance.ValueCount(s) = 3);
  for e in Array12 do
    begin
      AssertTrue(m.Instance.Contains(e.Key));
      AssertTrue(m.Instance.ContainsValue(e.Key, e.Value));
    end;
  for e in m.Instance.Entries do
    AssertTrue(ArrayContainsEntry(Array12, e));
end;

procedure TGTreeMultiMapTest.CreateCapacity;
var
  m: TAutoMultiMap;
begin
  m.Instance := TMultiMap.Create(45);
  AssertTrue(m.Instance.Count = 0);
  AssertTrue(m.Instance.Capacity >= 45);
  AssertTrue(m.Instance.LoadFactor = m.Instance.DefaultLoadFactor);
end;

procedure TGTreeMultiMapTest.CreateCapacityArray;
var
  m: TAutoMultiMap;
  e: TStrEntry;
  s: string;
begin
  m.Instance := TMultiMap.Create(50, Array12);
  AssertTrue(m.Instance.Count = 12);
  AssertTrue(m.Instance.KeyCount = 4);
  AssertTrue(m.Instance.Capacity >= 50);
  AssertTrue(m.Instance.LoadFactor = m.Instance.DefaultLoadFactor);
  for s in m.Instance.Keys do
    AssertTrue(m.Instance.ValueCount(s) = 3);
  for e in Array12 do
    begin
      AssertTrue(m.Instance.Contains(e.Key));
      AssertTrue(m.Instance.ContainsValue(e.Key, e.Value));
    end;
  for e in m.Instance.Entries do
    AssertTrue(ArrayContainsEntry(Array12, e));
end;

procedure TGTreeMultiMapTest.CreateCapacityEnum;
var
  m: TAutoMultiMap;
  e: TStrEntry;
  s: string;
begin
  m.Instance := TMultiMap.Create(50, TCursor.Create(THelper.CreateCopy(Array12)));
  AssertTrue(m.Instance.Count = 12);
  AssertTrue(m.Instance.KeyCount = 4);
  AssertTrue(m.Instance.Capacity >= 50);
  AssertTrue(m.Instance.LoadFactor = m.Instance.DefaultLoadFactor);
  for s in m.Instance.Keys do
    AssertTrue(m.Instance.ValueCount(s) = 3);
  for e in Array12 do
    begin
      AssertTrue(m.Instance.Contains(e.Key));
      AssertTrue(m.Instance.ContainsValue(e.Key, e.Value));
    end;
  for e in m.Instance.Entries do
    AssertTrue(ArrayContainsEntry(Array12, e));
end;

procedure TGTreeMultiMapTest.Clear;
var
  m: TAutoMultiMap;
begin
  m.Instance := TMultiMap.Create(Array12);
  AssertTrue(m.Instance.Count = 12);
  AssertTrue(m.Instance.KeyCount = 4);
  AssertTrue(m.Instance.Capacity >= 4);
  m.Instance.Clear;
  AssertTrue(m.Instance.Count = 0);
  AssertTrue(m.Instance.KeyCount = 0);
  AssertTrue(m.Instance.Capacity = 0);
end;

procedure TGTreeMultiMapTest.EnsureCapacity;
var
  m: TAutoMultiMap;
  c: SizeInt;
begin
  c := m.Instance.ExpandTreshold;
  m.Instance.EnsureCapacity(c + 1);
  AssertTrue(m.Instance.ExpandTreshold > c);
end;

procedure TGTreeMultiMapTest.TrimToFit;
var
  m: TAutoMultiMap;
  c: SizeInt;
begin
  m.Instance := TMultiMap.Create(Array12);
  c := m.Instance.Capacity;
  m.Instance.EnsureCapacity(c+1);
  AssertTrue(m.Instance.Capacity > c);
  m.Instance.TrimToFit;
  AssertTrue(m.Instance.Capacity = 8);
end;

procedure TGTreeMultiMapTest.Add;
var
  m: TAutoMultiMap;
begin
  AssertTrue(m.Instance.Count = 0);
  AssertTrue(m.Instance.Add('key1', 'value1'));
  AssertTrue(m.Instance.Count = 1);
  AssertTrue(m.Instance.KeyCount = 1);
  AssertTrue(m.Instance.Contains('key1'));
  AssertTrue(m.Instance.ContainsValue('key1', 'value1'));
  AssertFalse(m.Instance.Add('key1', 'value1'));
  AssertTrue(m.Instance.Add(TStrEntry.Create('key1', 'value2')));
  AssertTrue(m.Instance.Count = 2);
  AssertTrue(m.Instance.KeyCount = 1);
  AssertTrue(m.Instance.ContainsValue('key1', 'value2'));
  AssertTrue(m.Instance.Add('key2', 'value1'));
  AssertTrue(m.Instance.Count = 3);
  AssertTrue(m.Instance.KeyCount = 2);
  AssertFalse(m.Instance.Add('key2', 'value1'));
  AssertTrue(m.Instance.Add(TStrEntry.Create('key2', 'value2')));
  AssertTrue(m.Instance.Count = 4);
  AssertTrue(m.Instance.KeyCount = 2);
end;

procedure TGTreeMultiMapTest.AddArray;
var
  m: TAutoMultiMap;
  e: TStrEntry;
  s: string;
begin
  AssertTrue(m.Instance.AddAll(Array12) = Length(Array12));
  AssertTrue(m.Instance.Count = 12);
  AssertTrue(m.Instance.KeyCount = 4);
  for s in m.Instance.Keys do
    AssertTrue(m.Instance.ValueCount(s) = 3);
  for e in Array12 do
    begin
      AssertTrue(m.Instance.Contains(e.Key));
      AssertTrue(m.Instance.ContainsValue(e.Key, e.Value));
    end;
  for e in m.Instance.Entries do
    AssertTrue(ArrayContainsEntry(Array12, e));
  AssertTrue(m.Instance.AddAll(Array12) = 0);
end;

procedure TGTreeMultiMapTest.AddEnum;
var
  m: TAutoMultiMap;
  e: TStrEntry;
  s: string;
begin
  AssertTrue(m.Instance.AddAll(TCursor.Create(THelper.CreateCopy(Array12))) = Length(Array12));
  AssertTrue(m.Instance.Count = 12);
  AssertTrue(m.Instance.KeyCount = 4);
  for s in m.Instance.Keys do
    AssertTrue(m.Instance.ValueCount(s) = 3);
  for e in Array12 do
    begin
      AssertTrue(m.Instance.Contains(e.Key));
      AssertTrue(m.Instance.ContainsValue(e.Key, e.Value));
    end;
  for e in m.Instance.Entries do
    AssertTrue(ArrayContainsEntry(Array12, e));
  AssertTrue(m.Instance.AddAll(TCursor.Create(THelper.CreateCopy(Array12))) = 0);
end;

procedure TGTreeMultiMapTest.AddValuesArray;
var
  m: TAutoMultiMap;
  s: string;
begin
  m.Instance.AddAll(Array12);
  AssertTrue(m.Instance.AddValues('fish', ArrayFish4) = 4);
  AssertTrue(m.Instance.Count = 16);
  AssertTrue(m.Instance.KeyCount = 4);
  for s in ArrayFish4 do
    AssertTrue(m.Instance.ContainsValue('fish', s));
  AssertTrue(m.Instance.AddValues('fish', ArrayFish4) = 0);
  AssertTrue(m.Instance.AddValues('plant', ArrayPlant4) = 4);
  AssertTrue(m.Instance.Count = 20);
  AssertTrue(m.Instance.KeyCount = 5);
  for s in ArrayPlant4 do
    AssertTrue(m.Instance.ContainsValue('plant', s));
  AssertTrue(m.Instance.AddValues('plant', ArrayPlant4) = 0);
end;

procedure TGTreeMultiMapTest.AddValuesEnum;
var
  m: TAutoMultiMap;
  s: string;
begin
  m.Instance.AddAll(Array12);
  AssertTrue(m.Instance.AddValues('fish', TStrCursor.Create(TStrHelper.CreateCopy(ArrayFish4))) = 4);
  AssertTrue(m.Instance.Count = 16);
  AssertTrue(m.Instance.KeyCount = 4);
  for s in ArrayFish4 do
    AssertTrue(m.Instance.ContainsValue('fish', s));
  AssertTrue(m.Instance.AddValues('fish', TStrCursor.Create(TStrHelper.CreateCopy(ArrayFish4))) = 0);
  AssertTrue(m.Instance.AddValues('plant', TStrCursor.Create(TStrHelper.CreateCopy(ArrayPlant4))) = 4);
  AssertTrue(m.Instance.Count = 20);
  AssertTrue(m.Instance.KeyCount = 5);
  for s in ArrayPlant4 do
    AssertTrue(m.Instance.ContainsValue('plant', s));
  AssertTrue(m.Instance.AddValues('plant', TStrCursor.Create(TStrHelper.CreateCopy(ArrayPlant4))) = 0);
end;

procedure TGTreeMultiMapTest.Remove;
var
  m: TAutoMultiMap;
begin
  m.Instance.AddAll(Array12);
  AssertTrue(m.Instance.Remove('fish', 'pike'));
  AssertTrue(m.Instance.Remove('bird', 'swift'));
  AssertTrue(m.Instance.Remove('reptile', 'snake'));
  AssertTrue(m.Instance.Remove('insect', 'ant'));
  AssertTrue(m.Instance.KeyCount = 4);
  AssertTrue(m.Instance.Count = 8);
  AssertTrue(m.Instance.Remove('fish', 'crucian'));
  AssertTrue(m.Instance.Remove('fish', 'ruff'));
  AssertTrue(m.Instance.KeyCount = 3);
  AssertTrue(m.Instance.Count = 6);
end;

procedure TGTreeMultiMapTest.RemoveAll;
var
  m: TAutoMultiMap;
begin
  AssertTrue(m.Instance.RemoveAll(Array12) = 0);
  m.Instance.AddAll(Array12);
  AssertTrue(m.Instance.RemoveAll(Array12) = 12);
  AssertTrue(m.Instance.IsEmpty);
  m.Instance.AddAll(Array12);
  AssertTrue(m.Instance.RemoveAll(TCursor.Create(THelper.CreateCopy(Array12))) = 12);
  AssertTrue(m.Instance.IsEmpty);
end;

procedure TGTreeMultiMapTest.RemoveKey;
var
  m: TAutoMultiMap;
begin
  AssertTrue(m.Instance.RemoveKey('fish') = 0);
  m.Instance.AddAll(Array12);
  AssertTrue(m.Instance.RemoveKey('plant') = 0);
  AssertTrue(m.Instance.RemoveKey('fish') = 3);
  AssertTrue(m.Instance.RemoveKey('bird') = 3);
  AssertTrue(m.Instance.RemoveKey('reptile') = 3);
  AssertTrue(m.Instance.RemoveKey('insect') = 3);
  AssertTrue(m.Instance.IsEmpty);
end;

procedure TGTreeMultiMapTest.RemoveKeys;
var
  m: TAutoMultiMap;
begin
  AssertTrue(m.Instance.RemoveKeys(ArrayKeys) = 0);
  m.Instance.AddAll(Array12);
  AssertTrue(m.Instance.RemoveKeys(ArrayKeys[1..2]) = 6);
  AssertTrue(m.Instance.KeyCount = 2);
  AssertTrue(m.Instance.Count = 6);
  AssertTrue(m.Instance.RemoveKeys(ArrayKeys) = 6);
  AssertTrue(m.Instance.IsEmpty);
end;

procedure TGTreeMultiMapTest.RemoveValues;
var
  m: TAutoMultiMap;
begin
  AssertTrue(m.Instance.RemoveValues('fish', ArrayFish3) = 0);
  m.Instance.AddAll(Array12);
  AssertTrue(m.Instance.RemoveValues('bird', ArrayFish3) = 0);
  AssertTrue(m.Instance.RemoveValues('fish', ArrayFish3) = 3);
  AssertTrue(m.Instance.KeyCount = 3);
  AssertTrue(m.Instance.Count = 9);
  AssertTrue(m.Instance.AddAll(Array12) = 3);
  AssertTrue(m.Instance.RemoveValues('fish', TStrCursor.Create(TStrHelper.CreateCopy(ArrayFish3))) = 3);
  AssertTrue(m.Instance.Count = 9);
  AssertTrue(m.Instance.KeyCount = 3);
end;

procedure TGTreeMultiMapTest.Items;
var
  m: TAutoMultiMap;
  k, v: string;
  I: Integer = 0;
begin
  for k in m.Instance.Keys do
    for v in m.Instance[k] do
      Inc(I);
  AssertTrue(I = 0);
  m.Instance.AddAll(Array12);
  I := 0;
  for k in m.Instance.Keys do
    for v in m.Instance[k] do
      begin
        AssertTrue(m.Instance.ContainsValue(k, v));
        Inc(I);
      end;
  AssertTrue(I = 12);
end;

procedure TGTreeMultiMapTest.InIteration;
var
  m: TAutoMultiMap;
  Raised: Boolean = False;
  se: TStrEntry;
begin
  m.Instance.AddAll(Array12);
  try
    for se in m.Instance.Entries do
      m.Instance.Add(se);
  except
    on e: ELGUpdateLock do
      Raised := True;
  end;
  AssertTrue(Raised);
end;

{ TGListMultiMapTest }

procedure TGListMultiMapTest.TestCreate;
var
  m: TAutoMultiMap;
begin
  AssertTrue(m.Instance.Count = 0);
  AssertTrue(m.Instance.LoadFactor = m.Instance.DefaultLoadFactor);
end;

procedure TGListMultiMapTest.CreateArray;
var
  m: TAutoMultiMap;
  e: TStrEntry;
  s: string;
begin
  m.Instance := TMultiMap.Create(Array12);
  AssertTrue(m.Instance.Count = 12);
  AssertTrue(m.Instance.KeyCount = 4);
  for s in m.Instance.Keys do
    AssertTrue(m.Instance.ValueCount(s) = 3);
  for e in Array12 do
    begin
      AssertTrue(m.Instance.Contains(e.Key));
      AssertTrue(m.Instance.ContainsValue(e.Key, e.Value));
    end;
  for e in m.Instance.Entries do
    AssertTrue(ArrayContainsEntry(Array12, e));
end;

procedure TGListMultiMapTest.CreateEnum;
var
  m: TAutoMultiMap;
  e: TStrEntry;
  s: string;
begin
  m.Instance := TMultiMap.Create(TCursor.Create(THelper.CreateCopy(Array12)));
  AssertTrue(m.Instance.Count = 12);
  AssertTrue(m.Instance.KeyCount = 4);
  for s in m.Instance.Keys do
    AssertTrue(m.Instance.ValueCount(s) = 3);
  for e in Array12 do
    begin
      AssertTrue(m.Instance.Contains(e.Key));
      AssertTrue(m.Instance.ContainsValue(e.Key, e.Value));
    end;
  for e in m.Instance.Entries do
    AssertTrue(ArrayContainsEntry(Array12, e));
end;

procedure TGListMultiMapTest.CreateCapacity;
var
  m: TAutoMultiMap;
begin
  m.Instance := TMultiMap.Create(45);
  AssertTrue(m.Instance.Count = 0);
  AssertTrue(m.Instance.Capacity >= 45);
  AssertTrue(m.Instance.LoadFactor = m.Instance.DefaultLoadFactor);
end;

procedure TGListMultiMapTest.CreateCapacityArray;
var
  m: TAutoMultiMap;
  e: TStrEntry;
  s: string;
begin
  m.Instance := TMultiMap.Create(50, Array12);
  AssertTrue(m.Instance.Count = 12);
  AssertTrue(m.Instance.KeyCount = 4);
  AssertTrue(m.Instance.Capacity >= 50);
  AssertTrue(m.Instance.LoadFactor = m.Instance.DefaultLoadFactor);
  for s in m.Instance.Keys do
    AssertTrue(m.Instance.ValueCount(s) = 3);
  for e in Array12 do
    begin
      AssertTrue(m.Instance.Contains(e.Key));
      AssertTrue(m.Instance.ContainsValue(e.Key, e.Value));
    end;
  for e in m.Instance.Entries do
    AssertTrue(ArrayContainsEntry(Array12, e));
end;

procedure TGListMultiMapTest.CreateCapacityEnum;
var
  m: TAutoMultiMap;
  e: TStrEntry;
  s: string;
begin
  m.Instance := TMultiMap.Create(50, TCursor.Create(THelper.CreateCopy(Array12)));
  AssertTrue(m.Instance.Count = 12);
  AssertTrue(m.Instance.KeyCount = 4);
  AssertTrue(m.Instance.Capacity >= 50);
  AssertTrue(m.Instance.LoadFactor = m.Instance.DefaultLoadFactor);
  for s in m.Instance.Keys do
    AssertTrue(m.Instance.ValueCount(s) = 3);
  for e in Array12 do
    begin
      AssertTrue(m.Instance.Contains(e.Key));
      AssertTrue(m.Instance.ContainsValue(e.Key, e.Value));
    end;
  for e in m.Instance.Entries do
    AssertTrue(ArrayContainsEntry(Array12, e));
end;

procedure TGListMultiMapTest.Clear;
var
  m: TAutoMultiMap;
begin
  m.Instance := TMultiMap.Create(Array12);
  AssertTrue(m.Instance.Count = 12);
  AssertTrue(m.Instance.KeyCount = 4);
  AssertTrue(m.Instance.Capacity >= 4);
  m.Instance.Clear;
  AssertTrue(m.Instance.Count = 0);
  AssertTrue(m.Instance.KeyCount = 0);
  AssertTrue(m.Instance.Capacity = 0);
end;

procedure TGListMultiMapTest.EnsureCapacity;
var
  m: TAutoMultiMap;
  c: SizeInt;
begin
  c := m.Instance.ExpandTreshold;
  m.Instance.EnsureCapacity(c + 1);
  AssertTrue(m.Instance.ExpandTreshold > c);
end;

procedure TGListMultiMapTest.TrimToFit;
var
  m: TAutoMultiMap;
  c: SizeInt;
begin
  m.Instance := TMultiMap.Create(Array12);
  c := m.Instance.Capacity;
  m.Instance.EnsureCapacity(c+1);
  AssertTrue(m.Instance.Capacity > c);
  m.Instance.TrimToFit;
  AssertTrue(m.Instance.Capacity = 8);
end;

procedure TGListMultiMapTest.Add;
var
  m: TAutoMultiMap;
begin
  AssertTrue(m.Instance.Count = 0);
  AssertTrue(m.Instance.Add('key1', 'value1'));
  AssertTrue(m.Instance.Count = 1);
  AssertTrue(m.Instance.KeyCount = 1);
  AssertTrue(m.Instance.Contains('key1'));
  AssertTrue(m.Instance.ContainsValue('key1', 'value1'));
  AssertTrue(m.Instance.Add('key1', 'value1'));
  AssertTrue(m.Instance.Count = 2);
  AssertTrue(m.Instance.KeyCount = 1);
  AssertTrue(m.Instance.Add(TStrEntry.Create('key1', 'value2')));
  AssertTrue(m.Instance.Count = 3);
  AssertTrue(m.Instance.KeyCount = 1);
  AssertTrue(m.Instance.ContainsValue('key1', 'value2'));
  AssertTrue(m.Instance.Add('key2', 'value1'));
  AssertTrue(m.Instance.Count = 4);
  AssertTrue(m.Instance.KeyCount = 2);
  AssertTrue(m.Instance.Add('key2', 'value1'));
  AssertTrue(m.Instance.Count = 5);
  AssertTrue(m.Instance.KeyCount = 2);
  AssertTrue(m.Instance.Add(TStrEntry.Create('key2', 'value2')));
  AssertTrue(m.Instance.Count = 6);
  AssertTrue(m.Instance.KeyCount = 2);
end;

procedure TGListMultiMapTest.AddArray;
var
  m: TAutoMultiMap;
  e: TStrEntry;
  s: string;
begin
  AssertTrue(m.Instance.AddAll(Array12) = Length(Array12));
  AssertTrue(m.Instance.Count = 12);
  AssertTrue(m.Instance.KeyCount = 4);
  for s in m.Instance.Keys do
    AssertTrue(m.Instance.ValueCount(s) = 3);
  for e in Array12 do
    begin
      AssertTrue(m.Instance.Contains(e.Key));
      AssertTrue(m.Instance.ContainsValue(e.Key, e.Value));
    end;
  for e in m.Instance.Entries do
    AssertTrue(ArrayContainsEntry(Array12, e));
  AssertTrue(m.Instance.AddAll(Array12) = Length(Array12));
  AssertTrue(m.Instance.Count = Length(Array12) * 2);
  AssertTrue(m.Instance.KeyCount = 4);
end;

procedure TGListMultiMapTest.AddEnum;
var
  m: TAutoMultiMap;
  e: TStrEntry;
  s: string;
begin
  AssertTrue(m.Instance.AddAll(TCursor.Create(THelper.CreateCopy(Array12))) = Length(Array12));
  AssertTrue(m.Instance.Count = 12);
  AssertTrue(m.Instance.KeyCount = 4);
  for s in m.Instance.Keys do
    AssertTrue(m.Instance.ValueCount(s) = 3);
  for e in Array12 do
    begin
      AssertTrue(m.Instance.Contains(e.Key));
      AssertTrue(m.Instance.ContainsValue(e.Key, e.Value));
    end;
  for e in m.Instance.Entries do
    AssertTrue(ArrayContainsEntry(Array12, e));
  AssertTrue(m.Instance.AddAll(TCursor.Create(THelper.CreateCopy(Array12))) = Length(Array12));
  AssertTrue(m.Instance.Count = Length(Array12) * 2);
  AssertTrue(m.Instance.KeyCount = 4);
end;

procedure TGListMultiMapTest.AddValuesArray;
var
  m: TAutoMultiMap;
  s: string;
begin
  m.Instance.AddAll(Array12);
  AssertTrue(m.Instance.AddValues('fish', ArrayFish4) = 4);
  AssertTrue(m.Instance.Count = 16);
  AssertTrue(m.Instance.KeyCount = 4);
  for s in ArrayFish4 do
    AssertTrue(m.Instance.ContainsValue('fish', s));
  AssertTrue(m.Instance.AddValues('plant', ArrayPlant4) = 4);
  AssertTrue(m.Instance.Count = 20);
  AssertTrue(m.Instance.KeyCount = 5);
  for s in ArrayPlant4 do
    AssertTrue(m.Instance.ContainsValue('plant', s));
  AssertTrue(m.Instance.AddValues('fish', ArrayFish4) = 4);
  AssertTrue(m.Instance.AddValues('plant', ArrayPlant4) = 4);
  AssertTrue(m.Instance.Count = 28);
  AssertTrue(m.Instance.KeyCount = 5);
end;

procedure TGListMultiMapTest.AddValuesEnum;
var
  m: TAutoMultiMap;
  s: string;
begin
  m.Instance.AddAll(Array12);
  AssertTrue(m.Instance.AddValues('fish', TStrCursor.Create(TStrHelper.CreateCopy(ArrayFish4))) = 4);
  AssertTrue(m.Instance.Count = 16);
  AssertTrue(m.Instance.KeyCount = 4);
  for s in ArrayFish4 do
    AssertTrue(m.Instance.ContainsValue('fish', s));
  AssertTrue(m.Instance.AddValues('plant', TStrCursor.Create(TStrHelper.CreateCopy(ArrayPlant4))) = 4);
  AssertTrue(m.Instance.Count = 20);
  AssertTrue(m.Instance.KeyCount = 5);
  for s in ArrayPlant4 do
    AssertTrue(m.Instance.ContainsValue('plant', s));
  AssertTrue(m.Instance.AddValues('fish', TStrCursor.Create(TStrHelper.CreateCopy(ArrayFish4))) = 4);
  AssertTrue(m.Instance.AddValues('plant', TStrCursor.Create(TStrHelper.CreateCopy(ArrayPlant4))) = 4);
  AssertTrue(m.Instance.Count = 28);
  AssertTrue(m.Instance.KeyCount = 5);
end;

procedure TGListMultiMapTest.Remove;
var
  m: TAutoMultiMap;
begin
  m.Instance.AddAll(Array12);
  AssertTrue(m.Instance.Remove('fish', 'pike'));
  AssertTrue(m.Instance.Remove('bird', 'swift'));
  AssertTrue(m.Instance.Remove('reptile', 'snake'));
  AssertTrue(m.Instance.Remove('insect', 'ant'));
  AssertTrue(m.Instance.KeyCount = 4);
  AssertTrue(m.Instance.Count = 8);
  AssertTrue(m.Instance.Remove('fish', 'crucian'));
  AssertTrue(m.Instance.Remove('fish', 'ruff'));
  AssertTrue(m.Instance.KeyCount = 3);
  AssertTrue(m.Instance.Count = 6);
end;

procedure TGListMultiMapTest.RemoveAll;
var
  m: TAutoMultiMap;
begin
  AssertTrue(m.Instance.RemoveAll(Array12) = 0);
  m.Instance.AddAll(Array12);
  AssertTrue(m.Instance.RemoveAll(Array12) = 12);
  AssertTrue(m.Instance.IsEmpty);
  m.Instance.AddAll(Array12);
  AssertTrue(m.Instance.RemoveAll(TCursor.Create(THelper.CreateCopy(Array12))) = 12);
  AssertTrue(m.Instance.IsEmpty);
end;

procedure TGListMultiMapTest.RemoveKey;
var
  m: TAutoMultiMap;
begin
  AssertTrue(m.Instance.RemoveKey('fish') = 0);
  m.Instance.AddAll(Array12);
  AssertTrue(m.Instance.RemoveKey('plant') = 0);
  AssertTrue(m.Instance.RemoveKey('fish') = 3);
  AssertTrue(m.Instance.RemoveKey('bird') = 3);
  AssertTrue(m.Instance.RemoveKey('reptile') = 3);
  AssertTrue(m.Instance.RemoveKey('insect') = 3);
  AssertTrue(m.Instance.IsEmpty);
end;

procedure TGListMultiMapTest.RemoveKeys;
var
  m: TAutoMultiMap;
begin
  AssertTrue(m.Instance.RemoveKeys(ArrayKeys) = 0);
  m.Instance.AddAll(Array12);
  AssertTrue(m.Instance.RemoveKeys(ArrayKeys[1..2]) = 6);
  AssertTrue(m.Instance.KeyCount = 2);
  AssertTrue(m.Instance.Count = 6);
  AssertTrue(m.Instance.RemoveKeys(ArrayKeys) = 6);
  AssertTrue(m.Instance.IsEmpty);
end;

procedure TGListMultiMapTest.RemoveValues;
var
  m: TAutoMultiMap;
begin
  AssertTrue(m.Instance.RemoveValues('fish', ArrayFish3) = 0);
  m.Instance.AddAll(Array12);
  AssertTrue(m.Instance.RemoveValues('bird', ArrayFish3) = 0);
  AssertTrue(m.Instance.RemoveValues('fish', ArrayFish3) = 3);
  AssertTrue(m.Instance.KeyCount = 3);
  AssertTrue(m.Instance.Count = 9);
  AssertTrue(m.Instance.RemoveValues('fish', TStrCursor.Create(TStrHelper.CreateCopy(ArrayFish3))) = 0);
  AssertTrue(m.Instance.Count = 9);
  AssertTrue(m.Instance.KeyCount = 3);
end;

procedure TGListMultiMapTest.Items;
var
  m: TAutoMultiMap;
  k, v: string;
  I: Integer = 0;
begin
  for k in m.Instance.Keys do
    for v in m.Instance[k] do
      Inc(I);
  AssertTrue(I = 0);
  m.Instance.AddAll(Array12);
  I := 0;
  for k in m.Instance.Keys do
    for v in m.Instance[k] do
      begin
        AssertTrue(m.Instance.ContainsValue(k, v));
        Inc(I);
      end;
  AssertTrue(I = 12);
end;

procedure TGListMultiMapTest.InIteration;
var
  m: TAutoMultiMap;
  Raised: Boolean = False;
  se: TStrEntry;
begin
  m.Instance.AddAll(Array12);
  try
    for se in m.Instance.Entries do
      m.Instance.Add(se);
  except
    on e: ELGUpdateLock do
      Raised := True;
  end;
  AssertTrue(Raised);
end;

{ TGLiteMultiMapTest }

procedure TGLiteMultiMapTest.Clear;
var
  m: TMultiMap;
begin
  m.AddAll(Array12);
  AssertTrue(m.Count = 12);
  AssertTrue(m.Capacity >= 12);
  m.Clear;
  AssertTrue(m.Count = 0);
  AssertTrue(m.Capacity = 0);
end;

procedure TGLiteMultiMapTest.EnsureCapacity;
var
  m: TMultiMap;
  c: SizeInt;
begin
  c := m.Capacity;
  m.EnsureCapacity(c + 100);
  AssertTrue(m.Capacity >= c + 100);
end;

procedure TGLiteMultiMapTest.TrimToFit;
var
  m: TMultiMap;
  c: SizeInt;
begin
  m.AddAll(Array12);
  m.EnsureCapacity(40);
  c := m.Capacity;
  m.TrimToFit;
  AssertTrue(m.Capacity < c);
  AssertTrue(m.Capacity = 16);
end;

procedure TGLiteMultiMapTest.Add;
var
  m: TMultiMap;
begin
  AssertTrue(m.Count = 0);
  m.Add('key1', 'value1');
  AssertTrue(m.Count = 1);
  AssertTrue(m.Contains('key1'){%H-});
  m.Add('key1', 'value1');
  AssertTrue(m.Count = 2);
  AssertTrue(m.ValueCount('key1') = 2);
  m.Add(TStrEntry.Create('key1', 'value2'));
  AssertTrue(m.Count = 3);
  AssertTrue(m.ValueCount('key1') = 3);
  m.Add('key2', 'value1');
  AssertTrue(m.Count = 4);
  m.Add('key2', 'value1');
  AssertTrue(m.Count = 5);
  m.Add(TStrEntry.Create('key2', 'value2'));
  AssertTrue(m.Count = 6);
  AssertTrue(m.ValueCount('key2') = 3);
end;

procedure TGLiteMultiMapTest.AddArray;
var
  m: TMultiMap;
  e: TStrEntry;
  s: string;
begin
  AssertTrue(m.AddAll(Array12) = Length(Array12));
  AssertTrue(m.Count = 12);
  for s in m.Keys do
    AssertTrue(m.ValueCount(s) = 3);
  for e in Array12 do
    AssertTrue(m.Contains(e.Key){%H-});
  AssertTrue(m.AddAll(Array12) = Length(Array12));
  AssertTrue(m.Count = Length(Array12) * 2);
end;

procedure TGLiteMultiMapTest.AddEnum;
var
  m: TMultiMap;
  e: TStrEntry;
  s: string;
begin
  AssertTrue(m.AddAll(TCursor.Create(THelper.CreateCopy(Array12))) = Length(Array12));
  AssertTrue(m.Count = 12);
  for s in m.Keys do
    AssertTrue(m.ValueCount(s) = 3);
  for e in Array12 do
    AssertTrue(m.Contains(e.Key){%H-});
  AssertTrue(m.AddAll(TCursor.Create(THelper.CreateCopy(Array12))) = Length(Array12));
  AssertTrue(m.Count = Length(Array12) * 2);
end;

procedure TGLiteMultiMapTest.AddValuesArray;
var
  m: TMultiMap;
begin
  m.AddAll(Array12);
  AssertTrue(m.AddValues('fish', ArrayFish4) = 4);
  AssertTrue(m.Count = 16);
  AssertTrue(m.AddValues('plant', ArrayPlant4) = 4);
  AssertTrue(m.Count = 20);
  AssertTrue(m.AddValues('fish', ArrayFish4) = 4);
  AssertTrue(m.AddValues('plant', ArrayPlant4) = 4);
  AssertTrue(m.Count = 28);
end;

procedure TGLiteMultiMapTest.AddValuesEnum;
var
  m: TMultiMap;
begin
  m.AddAll(Array12);
  AssertTrue(m.AddValues('fish', TStrCursor.Create(TStrHelper.CreateCopy(ArrayFish4))) = 4);
  AssertTrue(m.Count = 16);
  AssertTrue(m.AddValues('plant', TStrCursor.Create(TStrHelper.CreateCopy(ArrayPlant4))) = 4);
  AssertTrue(m.Count = 20);
  AssertTrue(m.AddValues('fish', TStrCursor.Create(TStrHelper.CreateCopy(ArrayFish4))) = 4);
  AssertTrue(m.AddValues('plant', TStrCursor.Create(TStrHelper.CreateCopy(ArrayPlant4))) = 4);
  AssertTrue(m.Count = 28);
end;

procedure TGLiteMultiMapTest.Remove;
var
  m: TMultiMap;
begin
  m.AddAll(Array12);
  AssertTrue(m.Remove('fish'));
  AssertTrue(m.Remove('bird'));
  AssertFalse(m.Remove('plant'));
  AssertTrue(m.Remove('reptile'));
  AssertTrue(m.Remove('insect'));
  AssertTrue(m.Count = 8);
  AssertTrue(m.Remove('fish'));
  AssertTrue(m.Remove('fish'));
  AssertTrue(m.Count = 6);
end;

procedure TGLiteMultiMapTest.Items;
var
  m: TMultiMap;
  k, v: string;
  I: Integer = 0;
begin
  for k in m.Keys do
    for v in m[k] do
      Inc(I);
  AssertTrue(I = 0);
  m.AddAll(Array12);
  I := 0;
  for k in ArrayKeys do
    for v in m[k] do
      Inc(I);
  AssertTrue(I = 12);
end;

initialization
  RegisterTest(TGHashMultiMapTest);
  RegisterTest(TGTreeMultiMapTest);
  RegisterTest(TGListMultiMapTest);
  RegisterTest(TGLiteMultiMapTest);
end.

