unit LGBiMapTest;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, fpcunit, testregistry,
  LGUtils,
  LGAbstractContainer,
  LGBiMap,
  LGHashSet,
  LGArrayHelpers;

type
  TEntry = specialize TGMapEntry<string, Integer>;

  { THashBiMapTest }

  THashBiMapTest = class(TTestCase)
  private
  type
    TMap        = specialize TGHashBiMap2<string, Integer>;
    TAutoMap    = specialize TGAutoRef<TMap>;
    TCursor     = specialize TGArrayCursor<TEntry>;
    TStrCursor  = specialize TGArrayCursor<string>;
    THelper     = specialize TGArrayHelpUtil<TEntry>;
    TEntryArray = array of TEntry;
    IInverseMap = TMap.IInverseMap;
    TStrSet     = specialize TGHashSetLP<string>;
    TAutoStrSet = specialize TGAutoRef<TStrSet>;
    TIntSet     = specialize TGHashSetLP<Integer>;
    TAutoIntSet = specialize TGAutoRef<TIntSet>;
    TProc       = procedure of object;

    TTestObj = class
    private
      FProc: TProc;
    public
      constructor Create(aProc: TProc);
      destructor Destroy; override;
    end;

    TCounter = object
    private
      FCount: Integer;
      procedure IncCount;
      function  GetIncrement: TProc;
    public
      property  Increment: TProc read GetIncrement;
      property  Count: Integer read FCount;
    end;

    TObjMap     = specialize TGObjHashBiMap2<TTestObj, string>;
    TAutoObjMap = specialize TGAutoRef<TObjMap>;
    TObjArray   = specialize TGArray<TTestObj>;
    TObjSet     = specialize TGHashSetLP<TTestObj>;
    TAutoObjSet = specialize TGAutoRef<TObjSet>;

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
    procedure InverseAdd;
    procedure AddArray;
    procedure AddEnum;
    procedure Replace;
    procedure InverseReplace;
    procedure Remove;
    procedure Remove_1;
    procedure InverseRemove;
    procedure InverseRemove_1;
    procedure RemoveArray;
    procedure RemoveEnum;
    procedure Retain;
    procedure InverseRetain;
    procedure GetValue;
    procedure InverseGetValue;
    procedure GetValue_1;
    procedure InverseGetValue_1;
    procedure AddOrSetValue;
    procedure InverseAddOrSetValue;
    procedure AddOrSetValue_1;
    procedure InverseAddOrSetValue_1;
    procedure InIteration;
    procedure ObjectMapClear;
    procedure ObjectMapRemove;
    procedure ObjectMapRetain;
  end;

implementation
{$B-}{$COPERATORS ON}{$WARNINGS OFF}

const
  Array12: array[0..11] of TEntry =
    ((Key: 'key0'; Value: 0), (Key: 'key1'; Value: 1), (Key: 'key2'; Value: 2), (Key: 'key3'; Value: 3),
     (Key: 'key4'; Value: 4), (Key: 'key5'; Value: 5), (Key: 'key6'; Value: 6), (Key: 'key7'; Value: 7),
     (Key: 'key8'; Value: 8), (Key: 'key9'; Value: 9), (Key: 'key10'; Value: 10), (Key: 'key11'; Value: 11));

{ THashBiMapTest.TTestObj }

constructor THashBiMapTest.TTestObj.Create(aProc: TProc);
begin
  FProc := aProc;
end;

destructor THashBiMapTest.TTestObj.Destroy;
begin
  if FProc <> nil then
    FProc;
  inherited;
end;

{ THashBiMapTest.TCounter }

procedure THashBiMapTest.TCounter.IncCount;
begin
  Inc(FCount);
end;

function THashBiMapTest.TCounter.GetIncrement: TProc;
begin
  Result := @IncCount;
end;

{ THashBiMapTest }

procedure THashBiMapTest.TestCreate;
var
  m: TAutoMap;
begin
  AssertTrue(m.Instance.Count = 0);
  AssertTrue(m.Instance.Capacity = DEFAULT_CONTAINER_CAPACITY);
end;

procedure THashBiMapTest.CreateArray;
var
  m: TAutoMap;
  e: TEntry;
  a: TEntryArray;
begin
  m.Instance := TMap.Create(a);
  AssertTrue(m.Instance.Count = 0);
  m.Instance := TMap.Create(Array12);
  AssertTrue(m.Instance.Count = Length(Array12));
  for e in Array12 do
    begin
      AssertTrue(m.Instance.Contains(e.Key){%H-});
      AssertTrue(m.Instance[e.Key] = e.Value);
    end;
end;

procedure THashBiMapTest.CreateEnum;
var
  m: TAutoMap;
  e: TEntry;
begin
  m.Instance := TMap.Create(TCursor.Create(THelper.CreateCopy(Array12)));
  AssertTrue(m.Instance.Count = Length(Array12));
  for e in Array12 do
    begin
      AssertTrue(m.Instance.Contains(e.Key){%H-});
      AssertTrue(m.Instance[e.Key] = e.Value);
    end;
end;

procedure THashBiMapTest.CreateCapacity;
var
  m: TAutoMap;
begin
  m.Instance := TMap.Create(45);
  AssertTrue(m.Instance.Count = 0);
  AssertTrue(m.Instance.Capacity >= 45);
end;

procedure THashBiMapTest.CreateCapacityArray;
var
  m: TAutoMap;
  e: TEntry;
begin
  m.Instance := TMap.Create(65, Array12);
  AssertTrue(m.Instance.Count = Length(Array12));
  AssertTrue(m.Instance.Capacity >= 65);
  for e in Array12 do
    begin
      AssertTrue(m.Instance.Contains(e.Key){%H-});
      AssertTrue(m.Instance[e.Key] = e.Value);
    end;
end;

procedure THashBiMapTest.CreateCapacityEnum;
var
  m: TAutoMap;
  e: TEntry;
begin
  m.Instance := TMap.Create(65, TCursor.Create(THelper.CreateCopy(Array12)));
  AssertTrue(m.Instance.Count = Length(Array12));
  AssertTrue(m.Instance.Capacity >= 65);
  for e in Array12 do
    begin
      AssertTrue(m.Instance.Contains(e.Key){%H-});
      AssertTrue(m.Instance[e.Key] = e.Value);
    end;
end;

procedure THashBiMapTest.Clear;
var
  m: TAutoMap;
begin
  m.Instance := TMap.Create(Array12);
  AssertTrue(m.Instance.Count = Length(Array12));
  AssertTrue(m.Instance.Capacity >= Length(Array12));
  m.Instance.Clear;
  AssertTrue(m.Instance.Count = 0);
  AssertTrue(m.Instance.Capacity = 0);
end;

procedure THashBiMapTest.EnsureCapacity;
var
  m: TAutoMap;
begin
  m.Instance.EnsureCapacity(100);
  AssertTrue(m.Instance.Capacity >= 100);
end;

procedure THashBiMapTest.TrimToFit;
var
  m: TAutoMap;
  c: SizeInt;
begin
  m.Instance := TMap.Create(Array12);
  m.Instance.EnsureCapacity(40);
  c := m.Instance.Capacity;
  m.Instance.TrimToFit;
  AssertTrue(m.Instance.Capacity < c);
  AssertTrue(m.Instance.Capacity = 16);
end;

procedure THashBiMapTest.Add;
var
  m: TAutoMap;
begin
  AssertTrue(m.Instance.Count = 0);
  AssertTrue(m.Instance.Add('key1', 1));
  AssertTrue(m.Instance.Count = 1);
  AssertTrue(m.Instance.Contains('key1'){%H-});
  AssertTrue(m.Instance['key1'] = 1);
  AssertFalse(m.Instance.Add('key1', 1));
  AssertTrue(m.Instance.Add(TEntry.Create('key2', 2)));
  AssertTrue(m.Instance.Count = 2);
  AssertTrue(m.Instance.Contains('key2'){%H-});
  AssertTrue(m.Instance['key2'] = 2);
  AssertFalse(m.Instance.Add('key2', 2));
  AssertTrue(m.Instance.Count = 2);
end;

procedure THashBiMapTest.InverseAdd;
var
  m: TAutoMap;
  InvMap: IInverseMap;
begin
  InvMap := m.Instance;
  AssertTrue(InvMap.Count = 0);
  AssertTrue(InvMap.Add(1, 'key1'));
  AssertTrue(InvMap.Count = 1);
  AssertTrue(InvMap.Contains(1){%H-});
  AssertTrue(InvMap[1] = 'key1');
  AssertFalse(InvMap.Add(1, 'key1'));
  AssertTrue(InvMap.Add(2, 'key2'));
  AssertTrue(InvMap.Count = 2);
  AssertTrue(InvMap.Contains(2){%H-});
  AssertTrue(InvMap[2] = 'key2');
  AssertFalse(InvMap.Add(2, 'key2'));
  AssertTrue(InvMap.Count = 2);
end;

procedure THashBiMapTest.AddArray;
var
  m: TAutoMap;
  e: TEntry;
  a: TEntryArray;
  k: string;
begin
  AssertTrue(m.Instance.AddAll(a) = 0);
  AssertTrue(m.Instance.Count = 0);
  AssertTrue(m.Instance.AddAll(Array12) = Length(Array12));
  AssertTrue(m.Instance.Count = Length(Array12));
  for e in Array12 do
    begin
      AssertTrue(m.Instance.Contains(e.Key){%H-});
      AssertTrue(m.Instance[e.Key] = e.Value);
      AssertTrue(m.Instance.ContainsValue(e.Value){%H-});
      AssertTrue(m.Instance.TryGetKey(e.Value, k));
      AssertTrue(k = e.Key);
    end;
end;

procedure THashBiMapTest.AddEnum;
var
  m: TAutoMap;
  e: TEntry;
  a: TEntryArray;
begin
  AssertTrue(m.Instance.AddAll(TCursor.Create(a)) = 0);
  AssertTrue(m.Instance.Count = 0);
  AssertTrue(m.Instance.AddAll(TCursor.Create(THelper.CreateCopy(Array12))) = Length(Array12));
  AssertTrue(m.Instance.Count = Length(Array12));
  for e in Array12 do
    begin
      AssertTrue(m.Instance.Contains(e.Key){%H-});
      AssertTrue(m.Instance[e.Key] = e.Value);
    end;
end;

procedure THashBiMapTest.Replace;
var
  m: TAutoMap;
  e: TEntry;
begin
  m.Instance := TMap.Create(Array12);
  for e in Array12 do
    AssertFalse(m.Instance.Replace(e.Key, e.Value));
  for e in Array12 do
    AssertTrue(m.Instance.Replace(e.Key, e.Value + 12));
  for e in Array12 do
    AssertTrue(m.Instance[e.Key] = e.Value + 12);
  AssertTrue(m.Instance.Count = Length(Array12));
end;

procedure THashBiMapTest.InverseReplace;
var
  m: TAutoMap;
  e: TEntry;
  InvMap: IInverseMap;
begin
  m.Instance := TMap.Create(Array12);
  InvMap := m.Instance;
  for e in Array12 do
    AssertFalse(InvMap.Replace(e.Value, e.Key));
  for e in Array12 do
    AssertTrue(InvMap.Replace(e.Value, e.Key + '_'));
  for e in Array12 do
    AssertTrue(InvMap[e.Value] = e.Key + '_');
  AssertTrue(InvMap.Count = Length(Array12));
end;

procedure THashBiMapTest.Remove;
var
  m: TAutoMap;
  e: TEntry;
begin
  m.Instance := TMap.Create(Array12);
  AssertFalse(m.Instance.Remove('key'));
  AssertFalse(m.Instance.Remove('key12'));
  AssertTrue(m.Instance.Count = Length(Array12));
  for e in Array12 do
    AssertTrue(m.Instance.Remove(e.Key));
  AssertTrue(m.Instance.IsEmpty);
end;

procedure THashBiMapTest.Remove_1;
var
  m: TAutoMap;
  I, TestSize: Integer;
begin
  TestSize := 257;
  for I := 0 to Pred(TestSize) do
    AssertTrue(m.Instance.Add('key' + I.ToString, I));
  AssertTrue(m.Instance.Count = TestSize);
  for I := 0 to Pred(TestSize) do
    AssertTrue(m.Instance.Remove('key' + I.ToString));
  AssertTrue(m.Instance.IsEmpty);
end;

procedure THashBiMapTest.InverseRemove;
var
  m: TAutoMap;
  e: TEntry;
  InvMap: IInverseMap;
begin
  m.Instance := TMap.Create(Array12);
  InvMap := m.Instance;
  AssertFalse(InvMap.Remove(-1));
  AssertFalse(InvMap.Remove(12));
  AssertTrue(InvMap.Count = Length(Array12));
  for e in Array12 do
    AssertTrue(InvMap.Remove(e.Value));
  AssertTrue(InvMap.Count.ToString, InvMap.IsEmpty);
end;

procedure THashBiMapTest.InverseRemove_1;
var
  m: TAutoMap;
  I, TestSize: Integer;
  InvMap: IInverseMap;
begin
  TestSize := 257;
  InvMap := m.Instance;
  for I := 0 to Pred(TestSize) do
    AssertTrue(InvMap.Add(I, 'key' + I.ToString));
  AssertTrue(InvMap.Count = TestSize);
  for I := 0 to Pred(TestSize) do
    AssertTrue(InvMap.Remove(I));
  AssertTrue(InvMap.IsEmpty);
end;

procedure THashBiMapTest.RemoveArray;
var
  m: TAutoMap;
  a: array of string;
  e: TEntry;
  I: Integer = 0;
begin
  AssertTrue(m.Instance.AddAll(Array12) = Length(Array12));
  AssertTrue(m.Instance.Count = Length(Array12));
  AssertTrue(m.Instance.RemoveAll(a) = 0);
  AssertTrue(m.Instance.Count = Length(Array12));
  SetLength(a, Length(Array12));
  for e in Array12 do
    begin
      a[I] := e.Key;
      Inc(I);
    end;
  AssertTrue(m.Instance.RemoveAll(a) = Length(Array12));
  AssertTrue(m.Instance.IsEmpty);
end;

procedure THashBiMapTest.RemoveEnum;
var
  m: TAutoMap;
  a: array of string;
  e: TEntry;
  I: Integer = 0;
begin
  AssertTrue(m.Instance.AddAll(Array12) = Length(Array12));
  AssertTrue(m.Instance.Count = Length(Array12));
  AssertTrue(m.Instance.RemoveAll(TStrCursor.Create(a)) = 0);
  AssertTrue(m.Instance.Count = Length(Array12));
  SetLength(a, Length(Array12));
  for e in Array12 do
    begin
      a[I] := e.Key;
      Inc(I);
    end;
  AssertTrue(m.Instance.RemoveAll(TStrCursor.Create(a)) = Length(Array12));
  AssertTrue(m.Instance.IsEmpty);
end;

procedure THashBiMapTest.Retain;
var
  m: TAutoMap;
  s: TAutoStrSet;
  e: TEntry;
begin
  m.Instance.AddAll(Array12);
  AssertTrue(m.Instance.Count = Length(Array12));
  m.Instance.RetainAll(s.Instance);
  AssertTrue(m.Instance.IsEmpty);
  m.Instance.AddAll(Array12);
  for e in Array12 do
    s.Instance.Add(e.Key);
  m.Instance.RetainAll(s.Instance);
  AssertTrue(m.Instance.Count = Length(Array12));
  for e in Array12 do
    if Odd(e.Value) then
      s.Instance.Remove(e.Key);
  m.Instance.RetainAll(s.Instance);
  AssertTrue(m.Instance.Count = Length(Array12) div 2);
  for e in Array12 do
    if Odd(e.Value) then
      AssertFalse(m.Instance.Contains(e.Key){%H-})
    else
      AssertTrue(m.Instance.Contains(e.Key){%H-});
end;

procedure THashBiMapTest.InverseRetain;
var
  m: TAutoMap;
  s: TAutoIntSet;
  e: TEntry;
  InvMap: IInverseMap;
begin
  m.Instance.AddAll(Array12);
  AssertTrue(m.Instance.Count = Length(Array12));
  InvMap := m.Instance;
  InvMap.RetainAll(s.Instance);
  AssertTrue(InvMap.IsEmpty);
  m.Instance.AddAll(Array12);
  for e in Array12 do
    s.Instance.Add(e.Value);
  InvMap.RetainAll(s.Instance);
  AssertTrue(InvMap.Count = Length(Array12));
  for e in Array12 do
    if Odd(e.Value) then
      s.Instance.Remove(e.Value);
  InvMap.RetainAll(s.Instance);
  AssertTrue(InvMap.Count = Length(Array12) div 2);
  for e in Array12 do
    if Odd(e.Value) then
      AssertFalse(InvMap.Contains(e.Value){%H-})
    else
      AssertTrue(InvMap.Contains(e.Value){%H-});
end;

procedure THashBiMapTest.GetValue;
var
  m: TAutoMap;
  Raised: Boolean = False;
begin
  try
    m.Instance.GetValue('key0');
  except
    on e: ELGMapError do
      Raised := True;
  end;
  AssertTrue(Raised);

  m.Instance.AddAll(Array12);
  Raised := False;
  try
    m.Instance.GetValue('key12');
  except
    on e: ELGMapError do
      Raised := True;
  end;
  AssertTrue(Raised);
end;

procedure THashBiMapTest.InverseGetValue;
var
  m: TAutoMap;
  InvMap: IInverseMap;
  Raised: Boolean = False;
begin
  InvMap := m.Instance;
  try
    InvMap.GetValue(0);
  except
    on e: ELGMapError do
      Raised := True;
  end;
  AssertTrue(Raised);

  m.Instance.AddAll(Array12);
  Raised := False;
  try
    InvMap.GetValue(12);
  except
    on e: ELGMapError do
      Raised := True;
  end;
  AssertTrue(Raised);
end;

procedure THashBiMapTest.GetValue_1;
var
  m: TAutoMap;
  e: TEntry;
begin
  m.Instance.AddAll(Array12);
  for e in Array12 do
    AssertTrue(m.Instance.GetValue(e.Key) = e.Value);
end;

procedure THashBiMapTest.InverseGetValue_1;
var
  m: TAutoMap;
  e: TEntry;
  InvMap: IInverseMap;
begin
  InvMap := m.Instance;
  m.Instance.AddAll(Array12);
  for e in Array12 do
    AssertTrue(InvMap.GetValue(e.Value) = e.Key);
end;

procedure THashBiMapTest.AddOrSetValue;
var
  m: TAutoMap;
  Raised: Boolean = False;
begin
  m.Instance.AddOrSetValue('key', 1);
  AssertTrue(m.Instance.Count = 1);
  AssertTrue(m.Instance['key'] = 1);
  try
    m.Instance.AddOrSetValue('key', 1);
  except
    on e: ELGMapError do
      Raised := True;
  end;
  AssertTrue(Raised);
end;

procedure THashBiMapTest.InverseAddOrSetValue;
var
  m: TAutoMap;
  Raised: Boolean = False;
  InvMap: IInverseMap;
begin
  InvMap := m.Instance;
  InvMap.AddOrSetValue(1, 'key');
  AssertTrue(InvMap.Count = 1);
  AssertTrue(InvMap[1] = 'key');
  try
    InvMap.AddOrSetValue(1, 'key');
  except
    on e: ELGMapError do
      Raised := True;
  end;
  AssertTrue(Raised);
end;

procedure THashBiMapTest.AddOrSetValue_1;
var
  m: TAutoMap;
  e: TEntry;
begin
  for e in Array12 do
    m.Instance.AddOrSetValue(e.Key, e.Value);
  for e in Array12 do
    AssertTrue(m.Instance[e.Key] = e.Value);
  for e in Array12 do
    m.Instance.AddOrSetValue(e.Key, e.Value + Length(Array12));
  for e in Array12 do
    AssertTrue(m.Instance[e.Key] = e.Value + Length(Array12));
end;

procedure THashBiMapTest.InverseAddOrSetValue_1;
var
  m: TAutoMap;
  e: TEntry;
  InvMap: IInverseMap;
begin
  InvMap := m.Instance;
  for e in Array12 do
    InvMap.AddOrSetValue(e.Value, e.Key);
  for e in Array12 do
    AssertTrue(InvMap[e.Value] = e.Key);
  for e in Array12 do
    InvMap.AddOrSetValue(e.Value, e.Key + '_');
  for e in Array12 do
    AssertTrue(InvMap[e.Value] = e.Key + '_');
end;

procedure THashBiMapTest.InIteration;
var
  m: TAutoMap;
  Raised: Boolean = False;
begin
  AssertTrue(m.Instance.AddAll(Array12) = Length(Array12));
  try
    m.Instance.RemoveAll(m.Instance.Keys);
  except
    on e: ELGUpdateLock do
      Raised := True;
  end;
  AssertTrue(Raised);
end;

procedure THashBiMapTest.ObjectMapClear;
var
  m: TAutoObjMap;
  a: TObjArray;
  I, TestSize: Integer;
  Counter: TCounter;
begin
  TestSize := 330;
  Counter := Default(TCounter);
  m.Instance := TObjMap.Create([moOwnsKeys]);
  for I := 1 to TestSize do
    AssertTrue(m.Instance.Add(TTestObj.Create(Counter.Increment), 'val' + I.ToString));
  AssertTrue(m.Instance.Count = TestSize);
  m.Instance.Clear;
  AssertTrue(m.Instance.IsEmpty);
  AssertTrue(Counter.Count = TestSize);

  System.SetLength(a, TestSize);
  m.Instance.OwnsKeys := False;
  Counter := Default(TCounter);
  for I := 0 to System.High(a) do
    begin
      a[I] := TTestObj.Create(Counter.Increment);
      m.Instance.Add(a[I], 'val' + I.ToString);
    end;
  m.Instance.Clear;
  AssertTrue(m.Instance.IsEmpty);
  AssertTrue(Counter.Count = 0);
  for I := 0 to System.High(a) do
    a[I].Free;
  AssertTrue(Counter.Count = TestSize);
end;

procedure THashBiMapTest.ObjectMapRemove;
var
  m: TAutoObjMap;
  a: TObjArray;
  I, TestSize: Integer;
  Counter: TCounter;
begin
  TestSize := 320;
  Counter := Default(TCounter);
  System.SetLength(a, TestSize);
  m.Instance := TObjMap.Create([moOwnsKeys]);
  for I := 0 to System.High(a) do
    begin
      a[I] := TTestObj.Create(Counter.Increment);
      AssertTrue(m.Instance.Add(a[I], 'val' + I.ToString));
    end;
  AssertTrue(m.Instance.Count = TestSize);
  for I := 0 to System.High(a) do
    begin
      AssertTrue(m.Instance.Remove(a[I]));
      AssertTrue(m.Instance.Count = Pred(TestSize - I));
      AssertTrue(Counter.Count = Succ(I));
    end;
  AssertTrue(m.Instance.IsEmpty);

  m.Instance.OwnsKeys := False;
  Counter := Default(TCounter);
  for I := 0 to System.High(a) do
    begin
      a[I] := TTestObj.Create(Counter.Increment);
      m.Instance.Add(a[I], 'val' + I.ToString);
    end;
  for I := 0 to System.High(a) do
    AssertTrue(m.Instance.Remove(a[I]));
  AssertTrue(m.Instance.IsEmpty);
  AssertTrue(Counter.Count = 0);
  for I := 0 to System.High(a) do
    a[I].Free;
  AssertTrue(Counter.Count = TestSize);
end;

procedure THashBiMapTest.ObjectMapRetain;
var
  m: TAutoObjMap;
  a: TObjArray;
  I, TestSize: Integer;
  Counter: TCounter;
  s: TAutoObjSet;
  o: TTestObj;
begin
  TestSize := 350;
  Counter := Default(TCounter);
  m.Instance := TObjMap.Create([moOwnsKeys]);
  for I := 1 to TestSize do
    AssertTrue(m.Instance.Add(TTestObj.Create(Counter.Increment), 'val' + I.ToString));
  m.Instance.RetainAll(s.Instance);
  AssertTrue(m.Instance.IsEmpty);
  AssertTrue(Counter.Count = TestSize);

  Counter := Default(TCounter);
  System.SetLength(a, TestSize);
  for I := 0 to System.High(a) do
    begin
      a[I] := TTestObj.Create(Counter.Increment);
      m.Instance.Add(a[I], 'val' + I.ToString);
      s.Instance.Add(a[I]);
    end;
  m.Instance.RetainAll(s.Instance);
  AssertTrue(m.Instance.Count = TestSize);
  AssertTrue(Counter.Count = 0);

  for I := 0 to Pred(TestSize div 2) do
    s.Instance.Remove(a[I]);
  AssertTrue(Counter.Count = 0);
  m.Instance.RetainAll(s.Instance);
  AssertTrue(m.Instance.Count = TestSize div 2);
  AssertTrue(Counter.Count = TestSize div 2);
  for o in s.Instance do
    AssertTrue(m.Instance.Contains(o){%H-});
end;

initialization
  RegisterTest(THashBiMapTest);
end.

