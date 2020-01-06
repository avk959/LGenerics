unit LGRbTreeTest;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, fpcunit, testregistry,
  LGUtils,
  LGArrayHelpers,
  LGRbTree;

type

  { TLiteRbTreeTest }

  TLiteRbTreeTest = class(TTestCase)
  private
  type
    TIntTree = specialize TGLiteRbTree<Integer, TDummy, Integer>;
    THelper  = specialize TGOrdinalArrayHelper<Integer>;
  published
    procedure IsEmpty;
    procedure Add;
    procedure GetEnumerator;
    procedure GetReverseEnumerator;
    procedure GetEnumeratorAt;
    procedure ToArray;
    procedure Clear;
    procedure FindOrAdd;
    procedure Find;
    procedure Remove;
  end;

  TLiteComparableRbTreeTest = class(TTestCase)
  private
  type
    TIntTree = specialize TGLiteComparableRbTree<Integer, TDummy>;
    THelper  = specialize TGOrdinalArrayHelper<Integer>;
  published
    procedure IsEmpty;
    procedure Add;
    procedure GetEnumerator;
    procedure GetReverseEnumerator;
    procedure GetEnumeratorAt;
    procedure ToArray;
    procedure Clear;
    procedure FindOrAdd;
    procedure Find;
    procedure Remove;
  end;

implementation
{$B-}{$COPERATORS ON}

procedure TLiteRbTreeTest.IsEmpty;
var
  Tree: TIntTree;
begin
  AssertTrue({%H-}Tree.IsEmpty);
  Tree.Add(1);
  AssertFalse(Tree.IsEmpty);
  AssertTrue(Tree.Count = 1);
  Tree.Add(2);
  AssertTrue(Tree.Count = 2);
end;

procedure TLiteRbTreeTest.Add;
const
  TestSize = 1000;
var
  Tree: TIntTree;
  p: TIntTree.PNode;
  I: Integer;
begin
  AssertTrue({%H-}Tree.IsEmpty);
  p := Tree.Add(1);
  AssertTrue(p <> nil);
  AssertTrue(p^.Key = 1);
  AssertTrue(Tree.Count = 1);
  AssertTrue(Tree.Add(1) = nil);
  AssertTrue(Tree.Count = 1);
  p := Tree.Add(11);
  AssertTrue(p <> nil);
  AssertTrue(p^.Key = 11);
  AssertTrue(Tree.Count = 2);

  for I in THelper.CreateRandomRangePermutation(1, TestSize) do
    Tree.Add(I);
  AssertTrue(Tree.Count = TestSize);
  AssertTrue(Tree.CheckState = rsConsistent);
end;

procedure TLiteRbTreeTest.GetEnumerator;
const
  TestSize = 1000;
var
  Tree: TIntTree;
  a: array of Integer;
  p: TIntTree.PNode;
  I: Integer;
begin
  I := 0;
  for p in Tree do
    Inc(I);
  AssertTrue(I = 0);

  a := THelper.CreateRandomRangePermutation(1, TestSize);
  for I in a do
    Tree.Add(I);
  THelper.Sort(a);
  I := 0;
  for p in Tree do
    begin
      AssertTrue(p^.Key = a[I]);
      Inc(I);
    end;
  AssertTrue(I = TestSize);
end;

procedure TLiteRbTreeTest.GetReverseEnumerator;
const
  TestSize = 1000;
var
  Tree: TIntTree;
  a: array of Integer;
  e: TIntTree.TReverseEnumerator;
  p: TIntTree.PNode;
  I: Integer;
begin
  I := 0;
  e := Tree.GetReverseEnumerator;
  while e.MoveNext do
    Inc(I);
  AssertTrue(I = 0);

  a := THelper.CreateRandomRangePermutation(1, TestSize);
  for I in a do
    Tree.Add(I);
  THelper.Sort(a);
  I := TestSize;
  e := Tree.GetReverseEnumerator;
  while e.MoveNext do
    begin
      p := e.Current;
      Dec(I);
      AssertTrue(p^.Key = a[I]);
    end;
  AssertTrue(I = 0);
end;

procedure TLiteRbTreeTest.GetEnumeratorAt;
const
  TestSize = 1000;
var
  Tree: TIntTree;
  e: TIntTree.TEnumerator;
  p: TIntTree.PNode;
  I: Integer;
begin
  I := 0;
  e := Tree.GetEnumeratorAt(3, False);
  while e.MoveNext do
    Inc(I);
  AssertTrue(I = 0);

  for I := 1 to TestSize do
    Tree.Add(I);

  I := Succ(TestSize div 2);
  e := Tree.GetEnumeratorAt(TestSize div 2, False);
  while e.MoveNext do
    begin
      p := e.Current;
      AssertTrue(p^.Key = I);
      Inc(I);
    end;
  AssertTrue(I = TestSize+1);

  I := TestSize div 2;
  e := Tree.GetEnumeratorAt(TestSize div 2, True);
  while e.MoveNext do
    begin
      p := e.Current;
      AssertTrue(p^.Key = I);
      Inc(I);
    end;
  AssertTrue(I = TestSize+1);
end;

procedure TLiteRbTreeTest.ToArray;
const
  TestSize = 1000;
var
  Tree: TIntTree;
  a: TIntTree.TEntryArray;
  I: Integer;
begin
  a := Tree.ToArray;
  AssertTrue(a = nil);

  for I := TestSize downto 1 do
    Tree.Add(I);
  a := Tree.ToArray;
  AssertTrue(Length(a) = TestSize);
  for I := 0 to High(a) do
    AssertTrue(a[I].Key = Succ(I));
end;

procedure TLiteRbTreeTest.Clear;
const
  TestSize = 100;
var
  Tree: TIntTree;
  I: Integer;
begin
  AssertTrue({%H-}Tree.IsEmpty);
  Tree.Clear;
  AssertTrue(Tree.IsEmpty);
  for I := 1 to TestSize do
    Tree.Add(I);
  AssertTrue(Tree.Count = TestSize);
  Tree.Clear;
  AssertTrue(Tree.IsEmpty);
end;

procedure TLiteRbTreeTest.FindOrAdd;
const
  TestSize = 1000;
var
  Tree: TIntTree;
  I: Integer;
  p: TIntTree.PNode;
begin
  for I := 1 to TestSize do
    AssertFalse(Tree.FindOrAdd(I, p));
  AssertTrue(Tree.Count = TestSize);
  for I := 1 to TestSize do
    AssertTrue(Tree.FindOrAdd(I, p));
  AssertTrue(Tree.Count = TestSize);
end;

procedure TLiteRbTreeTest.Find;
const
  TestSize = 1000;
var
  Tree: TIntTree;
  I: Integer;
  p: TIntTree.PNode;
begin
  for I := 1 to TestSize do
    begin
      AssertTrue(Tree.Find(I) = nil);
      Tree.Add(I);
    end;
  AssertTrue(Tree.Count = TestSize);
  for I := 1 to TestSize do
    begin
      p := Tree.Find(I);
      AssertTrue(p <> nil);
      AssertTrue(p^.Key = I);
    end;
end;

procedure TLiteRbTreeTest.Remove;
const
  TestSize = 1000;
var
  Tree: TIntTree;
  I: Integer;
begin
  for I := 1 to TestSize do
    begin
      AssertFalse(Tree.Remove(I));
      Tree.Add(I);
    end;
  AssertTrue(Tree.Count = TestSize);
  for I := 1 to TestSize do
    AssertTrue(Tree.Remove(I));
  AssertTrue(Tree.IsEmpty);
end;

{ TLiteComparableRbTreeTest }

procedure TLiteComparableRbTreeTest.IsEmpty;
var
  Tree: TIntTree;
begin
  AssertTrue({%H-}Tree.IsEmpty);
  Tree.Add(1);
  AssertFalse(Tree.IsEmpty);
  AssertTrue(Tree.Count = 1);
  Tree.Add(2);
  AssertTrue(Tree.Count = 2);
end;

procedure TLiteComparableRbTreeTest.Add;
const
  TestSize = 1000;
var
  Tree: TIntTree;
  p: TIntTree.PNode;
  I: Integer;
begin
  AssertTrue({%H-}Tree.IsEmpty);
  p := Tree.Add(1);
  AssertTrue(p <> nil);
  AssertTrue(p^.Key = 1);
  AssertTrue(Tree.Count = 1);
  AssertTrue(Tree.Add(1) = nil);
  AssertTrue(Tree.Count = 1);
  p := Tree.Add(11);
  AssertTrue(p <> nil);
  AssertTrue(p^.Key = 11);
  AssertTrue(Tree.Count = 2);

  for I in THelper.CreateRandomRangePermutation(1, TestSize) do
    Tree.Add(I);
  AssertTrue(Tree.Count = TestSize);
  AssertTrue(Tree.CheckState = rsConsistent);
end;

procedure TLiteComparableRbTreeTest.GetEnumerator;
const
  TestSize = 1000;
var
  Tree: TIntTree;
  a: array of Integer;
  p: TIntTree.PNode;
  I: Integer;
begin
  I := 0;
  for p in Tree do
    Inc(I);
  AssertTrue(I = 0);

  a := THelper.CreateRandomRangePermutation(1, TestSize);
  for I in a do
    Tree.Add(I);
  THelper.Sort(a);
  I := 0;
  for p in Tree do
    begin
      AssertTrue(p^.Key = a[I]);
      Inc(I);
    end;
  AssertTrue(I = TestSize);
end;

procedure TLiteComparableRbTreeTest.GetReverseEnumerator;
const
  TestSize = 1000;
var
  Tree: TIntTree;
  a: array of Integer;
  e: TIntTree.TReverseEnumerator;
  p: TIntTree.PNode;
  I: Integer;
begin
  I := 0;
  e := Tree.GetReverseEnumerator;
  while e.MoveNext do
    Inc(I);
  AssertTrue(I = 0);

  a := THelper.CreateRandomRangePermutation(1, TestSize);
  for I in a do
    Tree.Add(I);
  THelper.Sort(a);
  I := TestSize;
  e := Tree.GetReverseEnumerator;
  while e.MoveNext do
    begin
      p := e.Current;
      Dec(I);
      AssertTrue(p^.Key = a[I]);
    end;
  AssertTrue(I = 0);
end;

procedure TLiteComparableRbTreeTest.GetEnumeratorAt;
const
  TestSize = 1000;
var
  Tree: TIntTree;
  e: TIntTree.TEnumerator;
  p: TIntTree.PNode;
  I: Integer;
begin
  I := 0;
  e := Tree.GetEnumeratorAt(3, False);
  while e.MoveNext do
    Inc(I);
  AssertTrue(I = 0);

  for I := 1 to TestSize do
    Tree.Add(I);

  I := Succ(TestSize div 2);
  e := Tree.GetEnumeratorAt(TestSize div 2, False);
  while e.MoveNext do
    begin
      p := e.Current;
      AssertTrue(p^.Key = I);
      Inc(I);
    end;
  AssertTrue(I = TestSize+1);

  I := TestSize div 2;
  e := Tree.GetEnumeratorAt(TestSize div 2, True);
  while e.MoveNext do
    begin
      p := e.Current;
      AssertTrue(p^.Key = I);
      Inc(I);
    end;
  AssertTrue(I = TestSize+1);
end;

procedure TLiteComparableRbTreeTest.ToArray;
const
  TestSize = 1000;
var
  Tree: TIntTree;
  a: TIntTree.TEntryArray;
  I: Integer;
begin
  a := Tree.ToArray;
  AssertTrue(a = nil);

  for I := TestSize downto 1 do
    Tree.Add(I);
  a := Tree.ToArray;
  AssertTrue(Length(a) = TestSize);
  for I := 0 to High(a) do
    AssertTrue(a[I].Key = Succ(I));
end;

procedure TLiteComparableRbTreeTest.Clear;
const
  TestSize = 100;
var
  Tree: TIntTree;
  I: Integer;
begin
  AssertTrue({%H-}Tree.IsEmpty);
  Tree.Clear;
  AssertTrue(Tree.IsEmpty);
  for I := 1 to TestSize do
    Tree.Add(I);
  AssertTrue(Tree.Count = TestSize);
  Tree.Clear;
  AssertTrue(Tree.IsEmpty);
end;

procedure TLiteComparableRbTreeTest.FindOrAdd;
const
  TestSize = 1000;
var
  Tree: TIntTree;
  I: Integer;
  p: TIntTree.PNode;
begin
  for I := 1 to TestSize do
    AssertFalse(Tree.FindOrAdd(I, p));
  AssertTrue(Tree.Count = TestSize);
  for I := 1 to TestSize do
    AssertTrue(Tree.FindOrAdd(I, p));
  AssertTrue(Tree.Count = TestSize);
end;

procedure TLiteComparableRbTreeTest.Find;
const
  TestSize = 1000;
var
  Tree: TIntTree;
  I: Integer;
  p: TIntTree.PNode;
begin
  for I := 1 to TestSize do
    begin
      AssertTrue(Tree.Find(I) = nil);
      Tree.Add(I);
    end;
  AssertTrue(Tree.Count = TestSize);
  for I := 1 to TestSize do
    begin
      p := Tree.Find(I);
      AssertTrue(p <> nil);
      AssertTrue(p^.Key = I);
    end;
end;

procedure TLiteComparableRbTreeTest.Remove;
const
  TestSize = 1000;
var
  Tree: TIntTree;
  I: Integer;
begin
  for I := 1 to TestSize do
    begin
      AssertFalse(Tree.Remove(I));
      Tree.Add(I);
    end;
  AssertTrue(Tree.Count = TestSize);
  for I := 1 to TestSize do
    AssertTrue(Tree.Remove(I));
  AssertTrue(Tree.IsEmpty);
end;


initialization

  RegisterTest(TLiteRbTreeTest);
  RegisterTest(TLiteComparableRbTreeTest);

end.

