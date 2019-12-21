unit LGTreapTest;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, fpcunit, testregistry,
  LGUtils,
  LGArrayHelpers,
  LGTreap;

type

  { TLiteTreapTest }

  TLiteTreapTest = class(TTestCase)
  private
  type
    TIntTreap = specialize TGLiteTreap<Integer, TEmptyData, Integer>;
    TStrTreap = specialize TGLiteTreap<string, TEmptyData, string>;
  published
    procedure IsEmpty;
    procedure Add;
    procedure AddStr;
    procedure Clear;
    procedure Remove;
    procedure RemoveStr;
    procedure Find;
    procedure FindStr;
    procedure CountOf;
    procedure CountOfStr;
    procedure Split;
    procedure SplitStr;
    procedure ToArray;
    procedure ToArrayStr;
  end;

  { TLiteIdxTreapTest }

  TLiteIdxTreapTest = class(TTestCase)
  private
  type
    TIntTreap = specialize TGLiteIdxTreap<Integer, TEmptyData, Integer>;
    TStrTreap = specialize TGLiteIdxTreap<string, TEmptyData, string>;
  published
    procedure IsEmpty;
    procedure Add;
    procedure AddStr;
    procedure Clear;
    procedure Remove;
    procedure RemoveStr;
    procedure Find;
    procedure FindStr;
    procedure CountOf;
    procedure CountOfStr;
    procedure Split;
    procedure SplitStr;
    procedure IndexOf;
    procedure IndexOfStr;
    procedure Items;
    procedure ToArray;
    procedure ToArrayStr;
  end;

  { TLiteSegmentTreapTest }

  TLiteSegmentTreapTest = class(TTestCase)
  private
  type
    TAddMonoid = specialize TGAddMonoid<Integer>;
    TIntTreap  = specialize TGLiteSegmentTreap<Integer, Integer, Integer, TAddMonoid>;
  published
    procedure IsEmpty;
    procedure Add;
    procedure Clear;
    procedure Contains;
    procedure Remove;
    procedure Find;
    procedure FindLess;
    procedure FindLessOrEqual;
    procedure FindGreater;
    procedure FindGreaterOrEqual;
    procedure Split;
    procedure IndexOf;
    procedure ToArray;
    procedure RangeQueryI;
    procedure RangeQuery;
    procedure HeadQueryI;
    procedure HeadQuery;
    procedure TailQueryI;
    procedure TailQuery;
  end;

  { TLiteImplicitTreapTest }

  TLiteImplicitTreapTest = class(TTestCase)
  private
  type
    TIntTreap = specialize TGLiteImplicitTreap<Integer>;
    THelper   = specialize TGOrdinalArrayHelper<Integer>;

  published
    procedure IsEmpty;
    procedure Add;
    procedure Clear;
    procedure Insert;
    procedure Delete;
    procedure Split;
    procedure Split1;
    procedure Merge;
    procedure ToArray;
    procedure RotateLeft;
    procedure RotateRight;
    procedure Items;
  end;

  { TLiteImplSegmentTreapTest }

  TLiteImplSegmentTreapTest = class(TTestCase)
  private
  type
    TAddMonoid = specialize TGAddMonoid<Integer>;
    TIntTreap  = specialize TGLiteImplSegmentTreap<Integer, TAddMonoid>;
    THelper    = specialize TGOrdinalArrayHelper<Integer>;

  published
    procedure IsEmpty;
    procedure Add;
    procedure Clear;
    procedure Insert;
    procedure Delete;
    procedure Split;
    procedure Split1;
    procedure Merge;
    procedure ToArray;
    procedure RotateLeft;
    procedure RotateRight;
    procedure RangeQuery;
    procedure HeadQuery;
    procedure TailQuery;
    procedure Items;
  end;

  { TLiteImplicitSegTreapTest }

  TLiteImplicitSegTreapTest = class(TTestCase)
  private
  type
    TAddMonoid = specialize TGAddMonoidEx<Integer>;
    TIntTreap  = specialize TGLiteImplicitSegTreap<Integer, TAddMonoid>;
    THelper    = specialize TGOrdinalArrayHelper<Integer>;

  published
    procedure Add;
    procedure Clear;
    procedure Insert;
    procedure Delete;
    procedure DeleteRange;
    procedure Split;
    procedure SplitRange;
    procedure Merge;
    procedure ToArray;
    procedure RotateLeft;
    procedure RotateRight;
    procedure Reverse;
    procedure RangeUpdate;
    procedure RangeQuery;
    procedure HeadQuery;
    procedure TailQuery;
    procedure Items;
  end;

implementation
{$B-}{$COPERATORS ON}

procedure TLiteTreapTest.IsEmpty;
var
  Treap: TIntTreap;
begin
  AssertTrue({%H-}Treap.Root = nil);
  AssertTrue(Treap.IsEmpty);
  Treap.Add(10);
  AssertFalse(Treap.Root = nil);
  AssertFalse(Treap.IsEmpty);
end;

procedure TLiteTreapTest.Add;
var
  Treap: TIntTreap;
  p: TIntTreap.PNode;
  I: Integer;
begin
  AssertTrue(Treap.Count = 0);
  for I in [1..16] do
    begin
      p := Treap.Add(I);
      AssertTrue(p <> nil);
      AssertTrue(p^.Key = I);
    end;
  AssertTrue(Treap.Count = 16);

  for I in [1..16] do
    begin
      p := Treap.Add(I);
      AssertTrue(p <> nil);
      AssertTrue(p^.Key = I);
    end;
  AssertTrue(Treap.Count = 32);
end;

procedure TLiteTreapTest.AddStr;
var
  Treap: TStrTreap;
  p: TStrTreap.PNode;
  I: Integer;
  s: string;
begin
  AssertTrue(Treap.Count = 0);
  for I in [1..16] do
    begin
      s := 'string ' + I.ToString;
      p := Treap.Add(s);
      AssertTrue(p <> nil);
      AssertTrue(p^.Key = s);
    end;
  AssertTrue(Treap.Count = 16);

  for I in [1..16] do
    begin
      s := 'string ' + I.ToString;
      p := Treap.Add(s);
      AssertTrue(p <> nil);
      AssertTrue(p^.Key = s);
    end;
  AssertTrue(Treap.Count = 32);
end;

procedure TLiteTreapTest.Clear;
var
  Treap: TIntTreap;
  I: Integer;
begin
  for I in [1..16] do
    Treap.Add(I);
  AssertTrue(Treap.Count = 16);
  Treap.Clear;
  AssertTrue(Treap.Count = 0);
  for I in [1..16] do
    Treap.Add(I);
  AssertTrue(Treap.Count = 16);
end;

procedure TLiteTreapTest.Remove;
var
  Treap: TIntTreap;
  I: Integer;
begin
  AssertFalse(Treap.Remove(0));
  AssertTrue(Treap.Count = 0);
  for I in [1..16] do
    Treap.Add(I);
  for I in [1..16] do
    Treap.Add(I);
  AssertTrue(Treap.Count = 32);
  AssertFalse(Treap.Remove(0));
  AssertFalse(Treap.Remove(17));
  for I in [1..16] do
    AssertTrue(Treap.Remove(I));
  AssertTrue(Treap.Count = 16);
  for I in [1..16] do
    AssertTrue(Treap.Remove(I));
  AssertTrue(Treap.IsEmpty);
end;

procedure TLiteTreapTest.RemoveStr;
var
  Treap: TStrTreap;
  I: Integer;
begin
  AssertFalse(Treap.Remove('string'));
  AssertTrue(Treap.Count = 0);
  for I in [1..16] do
    Treap.Add('string ' + I.ToString);
  for I in [1..16] do
    Treap.Add('string ' + I.ToString);
  AssertFalse(Treap.Remove('string 0'));
  AssertFalse(Treap.Remove('string 17'));
  AssertTrue(Treap.Count = 32);
  for I in [1..16] do
    AssertTrue(Treap.Remove('string ' + I.ToString));
  AssertTrue(Treap.Count = 16);
  for I in [1..16] do
    AssertTrue(Treap.Remove('string ' + I.ToString));
  AssertTrue(Treap.IsEmpty);
end;

procedure TLiteTreapTest.Find;
var
  Treap: TIntTreap;
  p: TIntTreap.PNode;
  I: Integer;
begin
  for I in [1..16] do
    Treap.Add(I);
  AssertTrue(Treap.Find(0) = nil);
  for I in [1..16] do
    begin
      p := Treap.Find(I);
      AssertTrue(p <> nil);
      AssertTrue(p^.Key = I);
    end;
  for I in [17..32] do
    AssertTrue(Treap.Find(I) = nil);
end;

procedure TLiteTreapTest.FindStr;
var
  Treap: TStrTreap;
  p: TStrTreap.PNode;
  I: Integer;
  s: string;
begin
  for I in [1..16] do
    Treap.Add('string ' + I.ToString);
  AssertTrue(Treap.Find('string 0') = nil);
  for I in [1..16] do
    begin
      s := 'string ' + I.ToString;
      p := Treap.Find(s);
      AssertTrue(p <> nil);
      AssertTrue(p^.Key = s);
    end;
  for I in [17..32] do
    AssertTrue(Treap.Find('string ' + I.ToString) = nil);
end;

procedure TLiteTreapTest.CountOf;
var
  Treap: TIntTreap;
  I: Integer;
begin
  for I in [1..16] do
    AssertTrue(Treap.CountOf(I) = 0);
  for I in [1..16] do
    begin
      Treap.Add(I);
      Treap.Add(I);
      Treap.Add(I);
    end;
  AssertTrue(Treap.Count = 48);
  AssertTrue(Treap.CountOf(0) = 0);
  for I in [1..16] do
    AssertTrue(Treap.CountOf(I) = 3);
  AssertTrue(Treap.CountOf(17) = 0);
end;

procedure TLiteTreapTest.CountOfStr;
var
  Treap: TStrTreap;
  I: Integer;
  s: string;
begin
  for I in [1..16] do
    AssertTrue(Treap.CountOf('string ' + I.ToString) = 0);
  for I in [1..16] do
    begin
      s := 'string ' + I.ToString;
      Treap.Add(s);
      Treap.Add(s);
      Treap.Add(s);
    end;
  AssertTrue(Treap.CountOf('string') = 0);
  AssertTrue(Treap.Count = 48);
  for I in [1..16] do
    AssertTrue(Treap.CountOf('string ' + I.ToString) = 3);
  AssertTrue(Treap.CountOf('string 17') = 0);
end;

procedure TLiteTreapTest.Split;
var
  Treap, Treap2, Treap3: TIntTreap;
  I: Integer;
begin
  Treap.Split(1, Treap2);
  AssertTrue(Treap2.IsEmpty);

  for I in [1..16] do
    begin
      Treap.Add(I);
      Treap.Add(I);
    end;
  AssertTrue(Treap.Count = 32);

  Treap.Split(17, Treap2);
  AssertTrue(Treap2.IsEmpty);

  Treap.Split(9, Treap2);
  AssertTrue(Treap.Count = 16);
  AssertTrue(Treap2.Count = 16);
  for I in [1..8] do
    AssertTrue(Treap.CountOf(I) = 2);
  for I in [9..16] do
    AssertTrue(Treap2.CountOf(I) = 2);

  Treap2.Clear;
  Treap.Split(0, Treap2);
  AssertTrue(Treap.IsEmpty);
  AssertTrue(Treap2.Count = 16);

  for I in [1..16] do
    begin
      Treap.Add(I);
      Treap.Add(I);
    end;

  TIntTreap.Split(9, Treap, Treap2, Treap3);
  AssertTrue(Treap.Count = 0);
  AssertTrue(Treap2.Count = 16);
  AssertTrue(Treap3.Count = 16);
  for I in [1..8] do
    AssertTrue(Treap2.CountOf(I) = 2);
  for I in [9..16] do
    AssertTrue(Treap3.CountOf(I) = 2);
end;

procedure TLiteTreapTest.SplitStr;
var
  Treap, Treap2, Treap3: TStrTreap;
  I: Integer;
  s: string;
begin
  for I in [11..26] do
    begin
      s := I.ToString;
      Treap.Add(s);
      Treap.Add(s);
    end;
  AssertTrue(Treap.Count = 32);

  Treap.Split('27', Treap2);
  AssertTrue(Treap2.IsEmpty);

  Treap.Split('19', Treap2);
  AssertTrue(Treap.Count = 16);
  AssertTrue(Treap2.Count = 16);
  for I in [11..18] do
    AssertTrue(Treap.CountOf(I.ToString) = 2);
  for I in [19..26] do
    AssertTrue(Treap2.CountOf(I.ToString) = 2);

  Treap2.Clear;
  Treap.Split('10', Treap2);
  AssertTrue(Treap.IsEmpty);
  AssertTrue(Treap2.Count = 16);

  for I in [11..26] do
    begin
      s := I.ToString;
      Treap.Add(s);
      Treap.Add(s);
    end;

  Treap.Split('19', Treap, Treap2, Treap3);
  AssertTrue(Treap.Count = 0);
  AssertTrue(Treap2.Count = 16);
  AssertTrue(Treap3.Count = 16);
  for I in [11..18] do
    AssertTrue(Treap2.CountOf(I.ToString) = 2);
  for I in [19..26] do
    AssertTrue(Treap3.CountOf(I.ToString) = 2);
end;

procedure TLiteTreapTest.ToArray;
var
  Treap: TIntTreap;
  a: array of TIntTreap.TEntry;
  I: Integer;
begin
  a := Treap.ToArray;
  AssertTrue(Length(a) = 0);
  for I in [1..16] do
    Treap.Add(I);
  a := Treap.ToArray;
  AssertTrue(Length(a) = 16);
  for I in [1..16] do
    AssertTrue(a[I-1].Key = I);
end;

procedure TLiteTreapTest.ToArrayStr;
var
  Treap: TStrTreap;
  a: array of TStrTreap.TEntry;
  I: Integer;
begin
  a := Treap.ToArray;
  AssertTrue(Length(a) = 0);
  for I in [10..26] do
    Treap.Add(I.ToString);
  a := Treap.ToArray;
  AssertTrue(Length(a) = 17);
  for I in [10..26] do
    AssertTrue(a[I-10].Key = I.ToString);
end;

{ TLiteIdxTreapTest }

procedure TLiteIdxTreapTest.IsEmpty;
var
  Treap: TIntTreap;
begin
  AssertTrue({%H-}Treap.Root = nil);
  AssertTrue(Treap.IsEmpty);
  Treap.Add(1);
  AssertFalse(Treap.Root = nil);
  AssertFalse(Treap.IsEmpty);
end;

procedure TLiteIdxTreapTest.Add;
var
  Treap: TIntTreap;
  p: TIntTreap.PNode;
  I: Integer;
begin
  AssertTrue({%H-}Treap.Count = 0);
  for I in [1..16] do
    begin
      p := Treap.Add(I);
      AssertTrue(p <> nil);
      AssertTrue(p^.Key = I);
    end;
  AssertTrue(Treap.Count = 16);

  for I in [1..16] do
    begin
      p := Treap.Add(I);
      AssertTrue(p <> nil);
      AssertTrue(p^.Key = I);
    end;
  AssertTrue(Treap.Count = 32);
end;

procedure TLiteIdxTreapTest.AddStr;
var
  Treap: TStrTreap;
  p: TStrTreap.PNode;
  I: Integer;
  s: string;
begin
  AssertTrue({%H-}Treap.Count = 0);
  for I in [1..16] do
    begin
      s := 'string ' + I.ToString;
      p := Treap.Add(s);
      AssertTrue(p <> nil);
      AssertTrue(p^.Key = s);
    end;
  AssertTrue(Treap.Count = 16);

  for I in [1..16] do
    begin
      s := 'string ' + I.ToString;
      p := Treap.Add(s);
      AssertTrue(p <> nil);
      AssertTrue(p^.Key = s);
    end;
  AssertTrue(Treap.Count = 32);
end;

procedure TLiteIdxTreapTest.Clear;
var
  Treap: TIntTreap;
  I: Integer;
begin
  for I in [1..16] do
    Treap.Add(I);
  AssertTrue(Treap.Count = 16);
  Treap.Clear;
  AssertTrue(Treap.Count = 0);
  for I in [1..16] do
    Treap.Add(I);
  AssertTrue(Treap.Count = 16);
end;

procedure TLiteIdxTreapTest.Remove;
var
  Treap: TIntTreap;
  I: Integer;
begin
  AssertFalse(Treap.Remove(0));
  AssertTrue(Treap.Count = 0);
  for I in [1..16] do
    Treap.Add(I);
  for I in [1..16] do
    Treap.Add(I);
  AssertTrue(Treap.Count = 32);
  AssertFalse(Treap.Remove(0));
  AssertFalse(Treap.Remove(17));
  for I in [1..16] do
    AssertTrue(Treap.Remove(I));
  AssertTrue(Treap.Count = 16);
  for I in [1..16] do
    AssertTrue(Treap.Remove(I));
  AssertTrue(Treap.IsEmpty);
end;

procedure TLiteIdxTreapTest.RemoveStr;
var
  Treap: TStrTreap;
  I: Integer;
begin
  AssertFalse(Treap.Remove('string'));
  AssertTrue(Treap.Count = 0);
  for I in [1..16] do
    Treap.Add('string ' + I.ToString);
  for I in [1..16] do
    Treap.Add('string ' + I.ToString);
  AssertFalse(Treap.Remove('string 0'));
  AssertFalse(Treap.Remove('string 17'));
  AssertTrue(Treap.Count = 32);
  for I in [1..16] do
    AssertTrue(Treap.Remove('string ' + I.ToString));
  AssertTrue(Treap.Count = 16);
  for I in [1..16] do
    AssertTrue(Treap.Remove('string ' + I.ToString));
  AssertTrue(Treap.IsEmpty);
end;

procedure TLiteIdxTreapTest.Find;
var
  Treap: TIntTreap;
  p: TIntTreap.PNode;
  I: Integer;
begin
  for I in [1..16] do
    Treap.Add(I);
  AssertTrue(Treap.Find(0) = nil);
  for I in [1..16] do
    begin
      p := Treap.Find(I);
      AssertTrue(p <> nil);
      AssertTrue(p^.Key = I);
    end;
  for I in [17..32] do
    AssertTrue(Treap.Find(I) = nil);
end;

procedure TLiteIdxTreapTest.FindStr;
var
  Treap: TStrTreap;
  p: TStrTreap.PNode;
  I: Integer;
  s: string;
begin
  for I in [1..16] do
    Treap.Add('string ' + I.ToString);
  AssertTrue(Treap.Find('string 0') = nil);
  for I in [1..16] do
    begin
      s := 'string ' + I.ToString;
      p := Treap.Find(s);
      AssertTrue(p <> nil);
      AssertTrue(p^.Key = s);
    end;
  for I in [17..32] do
    AssertTrue(Treap.Find('string ' + I.ToString) = nil);
end;

procedure TLiteIdxTreapTest.CountOf;
var
  Treap: TIntTreap;
  I: Integer;
begin
  for I in [1..16] do
    AssertTrue(Treap.CountOf(I) = 0);
  for I in [1..16] do
    begin
      Treap.Add(I);
      Treap.Add(I);
      Treap.Add(I);
    end;
  AssertTrue(Treap.Count = 48);
  AssertTrue(Treap.CountOf(0) = 0);
  for I in [1..16] do
    AssertTrue(Treap.CountOf(I) = 3);
  AssertTrue(Treap.CountOf(17) = 0);
end;

procedure TLiteIdxTreapTest.CountOfStr;
var
  Treap: TStrTreap;
  I: Integer;
  s: string;
begin
  for I in [1..16] do
    AssertTrue(Treap.CountOf('string ' + I.ToString) = 0);
  for I in [1..16] do
    begin
      s := 'string ' + I.ToString;
      Treap.Add(s);
      Treap.Add(s);
      Treap.Add(s);
    end;
  AssertTrue(Treap.CountOf('string') = 0);
  AssertTrue(Treap.Count = 48);
  for I in [1..16] do
    AssertTrue(Treap.CountOf('string ' + I.ToString) = 3);
  AssertTrue(Treap.CountOf('string 17') = 0);
end;

procedure TLiteIdxTreapTest.Split;
var
  Treap, Treap2, Treap3: TIntTreap;
  I: Integer;
begin
  Treap.Split(1, Treap2);
  AssertTrue(Treap2.IsEmpty);

  for I in [1..16] do
    begin
      Treap.Add(I);
      Treap.Add(I);
    end;
  AssertTrue(Treap.Count = 32);

  Treap.Split(17, Treap2);
  AssertTrue(Treap2.IsEmpty);

  Treap.Split(9, Treap2);
  AssertTrue(Treap.Count = 16);
  AssertTrue(Treap2.Count = 16);
  for I in [1..8] do
    AssertTrue(Treap.CountOf(I) = 2);
  for I in [9..16] do
    AssertTrue(Treap2.CountOf(I) = 2);

  Treap2.Clear;
  Treap.Split(0, Treap2);
  AssertTrue(Treap.IsEmpty);
  AssertTrue(Treap2.Count = 16);

  for I in [1..16] do
    begin
      Treap.Add(I);
      Treap.Add(I);
    end;

  TIntTreap.Split(9, Treap, Treap2, Treap3);
  AssertTrue(Treap.Count = 0);
  AssertTrue(Treap2.Count = 16);
  AssertTrue(Treap3.Count = 16);
  for I in [1..8] do
    AssertTrue(Treap2.CountOf(I) = 2);
  for I in [9..16] do
    AssertTrue(Treap3.CountOf(I) = 2);
end;

procedure TLiteIdxTreapTest.SplitStr;
var
  Treap, Treap2, Treap3: TStrTreap;
  I: Integer;
  s: string;
begin
  for I in [11..26] do
    begin
      s := I.ToString;
      Treap.Add(s);
      Treap.Add(s);
    end;
  AssertTrue(Treap.Count = 32);

  Treap.Split('27', Treap2);
  AssertTrue(Treap2.IsEmpty);

  Treap.Split('19', Treap2);
  AssertTrue(Treap.Count = 16);
  AssertTrue(Treap2.Count = 16);
  for I in [11..18] do
    AssertTrue(Treap.CountOf(I.ToString) = 2);
  for I in [19..26] do
    AssertTrue(Treap2.CountOf(I.ToString) = 2);

  Treap2.Clear;
  Treap.Split('10', Treap2);
  AssertTrue(Treap.IsEmpty);
  AssertTrue(Treap2.Count = 16);

  for I in [11..26] do
    begin
      s := I.ToString;
      Treap.Add(s);
      Treap.Add(s);
    end;

  Treap.Split('19', Treap, Treap2, Treap3);
  AssertTrue(Treap.Count = 0);
  AssertTrue(Treap2.Count = 16);
  AssertTrue(Treap3.Count = 16);
  for I in [11..18] do
    AssertTrue(Treap2.CountOf(I.ToString) = 2);
  for I in [19..26] do
    AssertTrue(Treap3.CountOf(I.ToString) = 2);
end;

procedure TLiteIdxTreapTest.IndexOf;
var
  Treap: TIntTreap;
  I: Integer;
begin
  for I in [1..16] do
    AssertTrue({%H-}Treap.IndexOf(I) = -1);
  for I in [1..16] do
    Treap.Add(I);
  AssertTrue(Treap.Count = 16);
  AssertTrue(Treap.IndexOf(0) = -1);
  for I in [1..16] do
    AssertTrue(Treap.IndexOf(I) = Pred(I));
  AssertTrue(Treap.IndexOf(17) = -1);
end;

procedure TLiteIdxTreapTest.IndexOfStr;
var
  Treap: TStrTreap;
  I: Integer;
begin
  for I in [11..26] do
    AssertTrue({%H-}Treap.IndexOf(I.ToString) = -1);
  for I in [11..26] do
    Treap.Add(I.ToString);
  AssertTrue(Treap.Count = 16);
  AssertTrue(Treap.IndexOf('10') = -1);
  for I in [11..26] do
    AssertTrue(Treap.IndexOf(I.ToString) = (I - 11));
  AssertTrue(Treap.IndexOf('27') = -1);
end;

procedure TLiteIdxTreapTest.Items;
var
  Treap: TIntTreap;
  I: Integer;
  p: TIntTreap.PNode;
begin
  for I in [0..17] do
    AssertTrue({%H-}Treap[Pred(I)] = nil);
  for I in [1..16] do
    Treap.Add(I);
  AssertTrue(Treap.Count = 16);
  AssertTrue(Treap[-1] = nil);
  for I in [1..16] do
    begin
      p := Treap[Pred(I)];
      AssertTrue(p <> nil);
      AssertTrue(p^.Key = I);
    end;
  AssertTrue(Treap[16] = nil);
end;

procedure TLiteIdxTreapTest.ToArray;
var
  Treap: TIntTreap;
  a: array of TIntTreap.TEntry;
  I: Integer;
begin
  a := Treap.ToArray;
  AssertTrue(Length(a) = 0);
  for I in [1..16] do
    Treap.Add(I);
  a := Treap.ToArray;
  AssertTrue(Length(a) = 16);
  for I in [1..16] do
    AssertTrue(a[I-1].Key = I);
end;

procedure TLiteIdxTreapTest.ToArrayStr;
var
  Treap: TStrTreap;
  a: array of TStrTreap.TEntry;
  I: Integer;
begin
  a := Treap.ToArray;
  AssertTrue(Length(a) = 0);
  for I in [10..26] do
    Treap.Add(I.ToString);
  a := Treap.ToArray;
  AssertTrue(Length(a) = 17);
  for I in [10..26] do
    AssertTrue(a[I-10].Key = I.ToString);
end;

{ TLiteSegmentTreapTest }

procedure TLiteSegmentTreapTest.IsEmpty;
var
  Treap: TIntTreap;
begin
  AssertTrue({%H-}Treap.IsEmpty);
  Treap.Add(1, 1);
  AssertFalse(Treap.IsEmpty);
end;

procedure TLiteSegmentTreapTest.Add;
var
  Treap: TIntTreap;
  I: Integer;
begin
  AssertTrue({%H-}Treap.Count = 0);
  for I in [1..16] do
    AssertTrue(Treap.Add(I, I));

  for I in [1..16] do
    AssertFalse(Treap.Add(I, I + 100));

  AssertTrue(Treap.Count = 16);
end;

procedure TLiteSegmentTreapTest.Clear;
var
  Treap: TIntTreap;
  I: Integer;
begin
  for I in [1..16] do
    Treap.Add(I, I);
  AssertTrue(Treap.Count = 16);
  Treap.Clear;
  AssertTrue(Treap.Count = 0);
  for I in [1..16] do
    Treap.Add(I, I);
  AssertTrue(Treap.Count = 16);
end;

procedure TLiteSegmentTreapTest.Contains;
var
  Treap: TIntTreap;
  I: Integer;
begin
  AssertTrue({%H-}Treap.Count = 0);
  for I in [11..42] do
    AssertTrue(Treap.Add(I, I));
  AssertTrue(Treap.Count = 32);

  for I in [1..10] do
    AssertFalse(Treap.Contains(I));
  for I in [43..53] do
    AssertFalse(Treap.Contains(I));

  for I in [11..42] do
    AssertTrue(Treap.Contains(I));
end;

procedure TLiteSegmentTreapTest.Remove;
var
  Treap: TIntTreap;
  I, J: Integer;
begin
  AssertFalse(Treap.Remove(0));
  AssertTrue(Treap.Count = 0);
  for I in [1..16] do
    Treap.Add(I, I);

  AssertTrue(Treap.Count = 16);
  AssertFalse(Treap.Remove(0));
  AssertFalse(Treap.Remove(17));
  for I in [1..16] do
    AssertTrue(Treap.Remove(I));
  AssertTrue(Treap.IsEmpty);

  for I in [1..16] do
    Treap.Add(I, I + 1);
  AssertTrue(Treap.Count = 16);
  for I in [1..16] do
    begin
      AssertTrue(Treap.Remove(I, J));
      AssertTrue(J = I + 1);
    end;
  AssertTrue(Treap.IsEmpty);
end;

procedure TLiteSegmentTreapTest.Find;
var
  Treap: TIntTreap;
  I, J: Integer;
begin
  for I in [1..16] do
    Treap.Add(I, I);
  AssertFalse(Treap.Find(0, J));
  for I in [1..16] do
    begin
      AssertTrue(Treap.Find(I, J));
      AssertTrue(J = I);
    end;
  for I in [17..32] do
    AssertFalse(Treap.Find(I, J));
end;

procedure TLiteSegmentTreapTest.FindLess;
const
  TestSize = 100;
var
  Treap: TIntTreap;
  I, J: Integer;
begin
  for I in [1..TestSize] do
    Treap.Add(I, I);
  for I in [2..TestSize] do
    begin
      AssertTrue(Treap.FindLess(I, J));
      AssertTrue(J = I - 1);
    end;
end;

procedure TLiteSegmentTreapTest.FindLessOrEqual;
const
  TestSize = 100;
var
  Treap: TIntTreap;
  I, J: Integer;
begin
  for I in [1..TestSize] do
    Treap.Add(I, I);
  for I in [1..TestSize] do
    begin
      AssertTrue(Treap.FindLessOrEqual(I, J));
      AssertTrue(J = I);
    end;
end;

procedure TLiteSegmentTreapTest.FindGreater;
const
  TestSize = 100;
var
  Treap: TIntTreap;
  I, J: Integer;
begin
  for I in [1..TestSize] do
    Treap.Add(I, I);
  for I in [1..TestSize-1] do
    begin
      AssertTrue(Treap.FindGreater(I, J));
      AssertTrue(J = I + 1);
    end;
end;

procedure TLiteSegmentTreapTest.FindGreaterOrEqual;
const
  TestSize = 100;
var
  Treap: TIntTreap;
  I, J: Integer;
begin
  for I in [1..TestSize] do
    Treap.Add(I, I);
  for I in [1..TestSize] do
    begin
      AssertTrue(Treap.FindGreaterOrEqual(I, J));
      AssertTrue(J = I);
    end;
end;

procedure TLiteSegmentTreapTest.Split;
var
  Treap, Treap2, Treap3: TIntTreap;
  I: Integer;
begin
  Treap.Split(1, Treap2);
  AssertTrue(Treap2.IsEmpty);

  for I in [1..32] do
    Treap.Add(I, I);
  AssertTrue(Treap.Count = 32);

  Treap.Split(33, Treap2);
  AssertTrue(Treap2.IsEmpty);

  Treap.Split(17, Treap2);
  AssertTrue(Treap.Count = 16);
  AssertTrue(Treap2.Count = 16);
  for I in [1..16] do
    AssertTrue(Treap.Contains(I));
  for I in [17..32] do
    AssertTrue(Treap2.Contains(I));

  Treap2.Clear;
  Treap.Split(0, Treap2);
  AssertTrue(Treap.IsEmpty);
  AssertTrue(Treap2.Count = 16);

  for I in [1..32] do
    Treap.Add(I, I);

  Treap.Split(17, Treap, Treap2, Treap3);
  AssertTrue(Treap.Count = 0);
  AssertTrue(Treap2.Count = 16);
  AssertTrue(Treap3.Count = 16);
  for I in [1..16] do
    AssertTrue(Treap2.Contains(I));
  for I in [17..32] do
    AssertTrue(Treap3.Contains(I));
end;

procedure TLiteSegmentTreapTest.IndexOf;
var
  Treap: TIntTreap;
  I: Integer;
begin
  for I in [1..16] do
    AssertTrue({%H-}Treap.IndexOf(I) = -1);
  for I in [1..16] do
    Treap.Add(I, I);
  AssertTrue(Treap.Count = 16);
  AssertTrue(Treap.IndexOf(0) = -1);
  for I in [1..16] do
    AssertTrue(Treap.IndexOf(I) = Pred(I));
  AssertTrue(Treap.IndexOf(17) = -1);
end;

procedure TLiteSegmentTreapTest.ToArray;
var
  Treap: TIntTreap;
  a: array of TIntTreap.TEntry;
  I: Integer;
begin
  a := Treap.ToArray;
  AssertTrue(Length(a) = 0);
  for I in [1..16] do
    Treap.Add(I, I + 10);
  a := Treap.ToArray;
  AssertTrue(Length(a) = Treap.Count);
  for I in [1..16] do
    begin
      AssertTrue(a[I-1].Key = I);
      AssertTrue(a[I-1].Value = I + 10);
    end;
end;

procedure TLiteSegmentTreapTest.RangeQueryI;
const
  TestSize = 1000;
var
  Treap: TIntTreap;
  a: array of Integer;
  I, J, L, R, sum: Integer;
begin
  SetLength(a, TestSize);
  for I := 0 to High(a) do
    a[I] := Succ(Random(TestSize));
  for I := 0 to Pred(TestSize) do
    Treap.Add(I, a[I]);

  sum := 0;
  for J := 0 to Pred(TestSize) do
    begin
      sum += a[J];
      AssertTrue(Treap.RangeQueryI(0, J) = sum);
    end;

  for I := 0 to Pred(TestSize) do
    begin
      AssertTrue(Treap.RangeQueryI(I, I) = a[I]);
      L := Random(TestSize div 2);
      R := TestSize div 2 + Random(TestSize div 2);
      sum := 0;
      for J := L to R do
        sum += a[J];
      AssertTrue(Treap.RangeQueryI(L, R) = sum);
      AssertTrue(Treap.RangeQueryI(R, L) = 0);
    end;
end;

procedure TLiteSegmentTreapTest.RangeQuery;
const
  TestSize = 1000;
var
  Treap: TIntTreap;
  a: array of Integer;
  I, J, L, R, sum: Integer;
  cnt: SizeInt;
begin
  SetLength(a, TestSize);
  for I := 0 to High(a) do
    a[I] := Succ(Random(TestSize));
  for I := 0 to Pred(TestSize) do
    Treap.Add(I, a[I]);

  for I := 0 to Pred(TestSize) do
    begin
      AssertTrue(Treap.RangeQuery(I, I+1) = a[I]);
      L := Random(TestSize div 2);
      R := TestSize div 2 + Random(TestSize div 2);
      sum := 0;
      for J := L to R - 1 do
        sum += a[J];
      AssertTrue(Treap.RangeQuery(L, R) = sum);
      AssertTrue(Treap.RangeQuery(R, L) = 0);
      Treap.RangeQuery(L, R, cnt);
      AssertTrue(cnt = R - L);
    end;
end;

procedure TLiteSegmentTreapTest.HeadQueryI;
const
  TestSize = 1000;
var
  Treap: TIntTreap;
  a: array of Integer;
  I, J, sum: Integer;
begin
  SetLength(a, TestSize);
  for I := 0 to High(a) do
    a[I] := Succ(Random(TestSize));
  for I := 0 to Pred(TestSize) do
    Treap.Add(I, a[I]);

  for I := 0 to Pred(TestSize) do
    begin
      sum := 0;
      for J := 0 to I do
        sum += a[J];
      AssertTrue(Treap.HeadQueryI(I) = sum);
    end;
end;

procedure TLiteSegmentTreapTest.HeadQuery;
const
  TestSize = 1000;
var
  Treap: TIntTreap;
  a: array of Integer;
  I, J, sum: Integer;
  cnt: SizeInt;
begin
  SetLength(a, TestSize);
  for I := 0 to High(a) do
    a[I] := Succ(Random(TestSize));
  for I := 0 to Pred(TestSize) do
    Treap.Add(I, a[I]);

  for I := 0 to Pred(TestSize) do
    begin
      sum := 0;
      for J := 0 to Pred(I) do
        sum += a[J];
      AssertTrue(Treap.HeadQuery(I) = sum);
      Treap.HeadQuery(I, cnt);
      AssertTrue(cnt = I);
    end;
end;

procedure TLiteSegmentTreapTest.TailQueryI;
const
  TestSize = 1000;
var
  Treap: TIntTreap;
  a: array of Integer;
  I, J, sum: Integer;
begin
  SetLength(a, TestSize);
  for I := 0 to High(a) do
    a[I] := Succ(Random(TestSize));
  for I := 0 to Pred(TestSize) do
    Treap.Add(I, a[I]);

  for I := 0 to Pred(TestSize) do
    begin
      sum := 0;
      for J := I to Pred(TestSize) do
        sum += a[J];
      AssertTrue(Treap.TailQueryI(I) = sum);
    end;
end;

procedure TLiteSegmentTreapTest.TailQuery;
const
  TestSize = 1000;
var
  Treap: TIntTreap;
  a: array of Integer;
  I, J, sum: Integer;
  cnt: SizeInt;
begin
  SetLength(a, TestSize);
  for I := 0 to High(a) do
    a[I] := Succ(Random(TestSize));
  for I := 0 to Pred(TestSize) do
    Treap.Add(I, a[I]);

  for I := 0 to Pred(TestSize) do
    begin
      sum := 0;
      for J := I to Pred(TestSize) do
        sum += a[J];
      AssertTrue(Treap.TailQuery(I) = sum);
      Treap.TailQuery(I, cnt);
      AssertTrue(cnt = TestSize - I);
    end;
end;

{ TLiteImplicitTreapTest }

procedure TLiteImplicitTreapTest.IsEmpty;
var
  Treap: TIntTreap;
begin
  AssertTrue({%H-}Treap.IsEmpty);
  Treap.Add(1);
  AssertFalse(Treap.IsEmpty);
end;

procedure TLiteImplicitTreapTest.Add;
var
  Treap: TIntTreap;
  I: Integer;
begin
  AssertTrue({%H-}Treap.Count = 0);
  for I in [1..16] do
    AssertTrue(Treap.Add(I) = I - 1);

  for I in [1..16] do
    AssertTrue(Treap[I-1] = I);

  for I in [1..16] do
    AssertTrue(Treap.Add(I) = I + 15);

  for I in [16..31] do
    AssertTrue(Treap[I] = I - 15);

  AssertTrue(Treap.Count = 32);
end;

procedure TLiteImplicitTreapTest.Clear;
var
  Treap: TIntTreap;
  I: Integer;
begin
  for I in [1..16] do
    Treap.Add(I);
  AssertTrue(Treap.Count = 16);
  Treap.Clear;
  AssertTrue(Treap.Count = 0);
  for I in [1..16] do
    Treap.Add(I);
  AssertTrue(Treap.Count = 16);
end;

procedure TLiteImplicitTreapTest.Insert;
const
  TestSize = 100;
var
  Treap, Treap1: TIntTreap;
  I: Integer;
begin
  for I := 1 to TestSize do
    Treap.Insert(0, I);
  for I := 0 to Pred(TestSize) do
    AssertTrue(Treap[I] = TestSize - I);

  Treap.Clear;
  for I := 1 to TestSize do
    Treap.Insert(Treap.Count, I);
  for I := 0 to Pred(TestSize) do
    AssertTrue(Treap[I] = I + 1);

  for I := TestSize + 1 to TestSize * 2 do
    Treap1.Insert({%H-}Treap1.Count, I);
  Treap1.Insert(0, Treap);
  AssertTrue(Treap1.Count = TestSize * 2);
  for I := 1 to Treap1.Count do
    AssertTrue(Treap1[I-1] = I);
end;

procedure TLiteImplicitTreapTest.Delete;
const
  TestSize = 100;
var
  Treap: TIntTreap;
  I: Integer;
begin
  for I := 0 to Pred(TestSize) do
    Treap.Add(I);
  AssertTrue(Treap.Delete(Pred(TestSize)) = Pred(TestSize));
  I := 0;
  while not Treap.IsEmpty do
    begin
      AssertTrue(Treap.Delete(0) = I);
      Inc(I);
    end;
  AssertTrue(I = Pred(TestSize));
end;

procedure TLiteImplicitTreapTest.Split;
const
  TestSize = 100;
var
  Treap, Treap2: TIntTreap;
  I: Integer;
begin
  for I in [1..TestSize] do
    Treap.Add(I);
  AssertTrue(Treap.Count = TestSize);

  Treap.Split(TestSize div 2, Treap2);
  AssertTrue(Treap.Count = TestSize div 2);
  AssertTrue(Treap2.Count = TestSize div 2);

  Treap2.Clear;
  Treap.Split(0, Treap2);
  AssertTrue(Treap.IsEmpty);
  AssertTrue(Treap2.Count = TestSize div 2);
end;

procedure TLiteImplicitTreapTest.Split1;
const
  TestSize = 1000;
var
  Treap, Treap2: TIntTreap;
  I, J, Size: Integer;
begin
  for I := 1 to TestSize do
    Treap.Add(I);
  AssertTrue(Treap.Count = TestSize);

  J := TestSize div 4;
  Size := TestSize div 2;
  Treap.Split(J, Size, Treap2);
  AssertTrue(Treap.Count = Size);
  AssertTrue(Treap2.Count = Size);
  for I := 0 to Pred(J) do
    AssertTrue(Treap[I] = Succ(I));
  for I := J to Pred(Treap.Count) do
    AssertTrue(Treap[I] = Succ(I) + Size);
  for I := 0 to Pred(Treap2.Count) do
    AssertTrue(Treap2[I] = Succ(I) + J );

  Treap.Insert(J, Treap2);
  for I := 0 to Pred(Treap.Count) do
    AssertTrue(Treap[I] = Succ(I));
  AssertTrue(Treap2.IsEmpty);
end;

procedure TLiteImplicitTreapTest.Merge;
const
  TestSize = 100;
var
  Treap, Treap1: TIntTreap;
  I: Integer;
begin
  for I := 1 to TestSize do
    Treap1.Add(I);
  AssertTrue({%H-}Treap.IsEmpty);
  Treap.Merge(Treap1);
  AssertTrue(Treap1.IsEmpty);
  AssertTrue(Treap.Count = TestSize);

  for I := TestSize + 1 to TestSize * 2 do
    Treap1.Add(I);
  Treap.Merge(Treap1);
  AssertTrue(Treap1.IsEmpty);
  AssertTrue(Treap.Count = TestSize * 2);
  for I := 1 to Treap.Count do
    AssertTrue(Treap[I-1] = I);
end;

procedure TLiteImplicitTreapTest.ToArray;
const
  TestSize = 100;
var
  Treap: TIntTreap;
  I: Integer;
  a: array of Integer;
begin
  a := Treap.ToArray;
  AssertTrue(a = nil);
  for I := 1 to TestSize do
    Treap.Add(I);
  a := Treap.ToArray;
  for I := 1 to TestSize do
    AssertTrue(a[I-1] = I);
end;

procedure TLiteImplicitTreapTest.RotateLeft;
const
  TestSize = 100;
var
  Treap: TIntTreap;
  I: Integer;
  a: array of Integer;
begin
  SetLength(a, TestSize);
  for I := 1 to TestSize do
    begin
      Treap.Add(I);
      a[I-1] := I;
    end;

  Treap.RotateLeft(0);
  for I := 0 to Pred(TestSize) do
    AssertTrue(Treap[I] = a[I]);

  Treap.RotateLeft(TestSize);
  for I := 0 to Pred(TestSize) do
    AssertTrue(Treap[I] = a[I]);

  Treap.RotateLeft(TestSize+10);
  for I := 0 to Pred(TestSize) do
    AssertTrue(Treap[I] = a[I]);

  Treap.RotateLeft(TestSize div 3);
  THelper.RotateLeft(a, TestSize div 3);
  for I := 0 to Pred(TestSize) do
    AssertTrue(Treap[I] = a[I]);

  Treap.RotateLeft(-(TestSize div 4));
  THelper.RotateLeft(a, -(TestSize div 4));
  for I := 0 to Pred(TestSize) do
    AssertTrue(Treap[I] = a[I]);
end;

procedure TLiteImplicitTreapTest.RotateRight;
const
  TestSize = 100;
var
  Treap: TIntTreap;
  I: Integer;
  a: array of Integer;
begin
  SetLength(a, TestSize);
  for I := 1 to TestSize do
    begin
      Treap.Add(I);
      a[I-1] := I;
    end;

  Treap.RotateRight(0);
  for I := 0 to Pred(TestSize) do
    AssertTrue(Treap[I] = a[I]);

  Treap.RotateRight(TestSize);
  for I := 0 to Pred(TestSize) do
    AssertTrue(Treap[I] = a[I]);

  Treap.RotateRight(TestSize+10);
  for I := 0 to Pred(TestSize) do
    AssertTrue(Treap[I] = a[I]);

  Treap.RotateRight(TestSize div 3);
  THelper.RotateRight(a, TestSize div 3);
  for I := 0 to Pred(TestSize) do
    AssertTrue(Treap[I] = a[I]);

  Treap.RotateRight(-(TestSize div 4));
  THelper.RotateRight(a, -(TestSize div 4));
  for I := 0 to Pred(TestSize) do
    AssertTrue(Treap[I] = a[I]);
end;

procedure TLiteImplicitTreapTest.Items;
const
  TestSize = 100;
var
  Treap: TIntTreap;
  I: Integer;
  a: array of Integer;
begin
  SetLength(a, TestSize);
  for I := 1 to TestSize do
    begin
      Treap.Add(I);
      a[I-1] := I;
    end;
  for I := 0 to Pred(TestSize) do
    AssertTrue(Treap[I] = a[I]);

  for I := 0 to Pred(TestSize) do
    begin
      Treap[I] := Treap[I] + Succ(I);
      a[I] += Succ(I);
    end;
  for I := 0 to Pred(TestSize) do
    AssertTrue(Treap[I] = a[I]);
end;

{ TLiteImplSegmentTreapTest }

procedure TLiteImplSegmentTreapTest.IsEmpty;
var
  Treap: TIntTreap;
begin
  AssertTrue({%H-}Treap.IsEmpty);
  Treap.Add(1);
  AssertFalse(Treap.IsEmpty);
end;

procedure TLiteImplSegmentTreapTest.Add;
var
  Treap: TIntTreap;
  I: Integer;
begin
  AssertTrue({%H-}Treap.Count = 0);
  for I in [1..16] do
    AssertTrue(Treap.Add(I) = I - 1);

  for I in [1..16] do
    AssertTrue(Treap[I-1] = I);

  for I in [1..16] do
    AssertTrue(Treap.Add(I) = I + 15);

  for I in [16..31] do
    AssertTrue(Treap[I] = I - 15);

  AssertTrue(Treap.Count = 32);
end;

procedure TLiteImplSegmentTreapTest.Clear;
var
  Treap: TIntTreap;
  I: Integer;
begin
  for I in [1..16] do
    Treap.Add(I);
  AssertTrue(Treap.Count = 16);
  Treap.Clear;
  AssertTrue(Treap.Count = 0);
  for I in [1..16] do
    Treap.Add(I);
  AssertTrue(Treap.Count = 16);
end;

procedure TLiteImplSegmentTreapTest.Insert;
const
  TestSize = 100;
var
  Treap, Treap1: TIntTreap;
  I: Integer;
begin
  for I := 1 to TestSize do
    Treap.Insert(0, I);
  for I := 0 to Pred(TestSize) do
    AssertTrue(Treap[I] = TestSize - I);

  Treap.Clear;
  for I := 1 to TestSize do
    Treap.Insert(Treap.Count, I);
  for I := 0 to Pred(TestSize) do
    AssertTrue(Treap[I] = I + 1);

  for I := TestSize + 1 to TestSize * 2 do
    Treap1.Insert({%H-}Treap1.Count, I);
  Treap1.Insert(0, Treap);
  AssertTrue(Treap1.Count = TestSize * 2);
  for I := 1 to Treap1.Count do
    AssertTrue(Treap1[I-1] = I);
end;

procedure TLiteImplSegmentTreapTest.Delete;
const
  TestSize = 100;
var
  Treap: TIntTreap;
  I: Integer;
begin
  for I := 0 to Pred(TestSize) do
    Treap.Add(I);
  AssertTrue(Treap.Delete(Pred(TestSize)) = Pred(TestSize));
  I := 0;
  while not Treap.IsEmpty do
    begin
      AssertTrue(Treap.Delete(0) = I);
      Inc(I);
    end;
  AssertTrue(I = Pred(TestSize));
end;

procedure TLiteImplSegmentTreapTest.Split;
const
  TestSize = 100;
var
  Treap, Treap2: TIntTreap;
  I: Integer;
begin
  for I in [1..TestSize] do
    Treap.Add(I);
  AssertTrue(Treap.Count = TestSize);

  Treap.Split(TestSize div 2, Treap2);
  AssertTrue(Treap.Count = TestSize div 2);
  AssertTrue(Treap2.Count = TestSize div 2);

  Treap2.Clear;
  Treap.Split(0, Treap2);
  AssertTrue(Treap.IsEmpty);
  AssertTrue(Treap2.Count = TestSize div 2);
end;

procedure TLiteImplSegmentTreapTest.Split1;
const
  TestSize = 1000;
var
  Treap, Treap2: TIntTreap;
  I, J, Size: Integer;
begin
  for I := 1 to TestSize do
    Treap.Add(I);
  AssertTrue(Treap.Count = TestSize);

  J := TestSize div 4;
  Size := TestSize div 2;
  Treap.Split(J, Size, Treap2);
  AssertTrue(Treap.Count = Size);
  AssertTrue(Treap2.Count = Size);
  for I := 0 to Pred(J) do
    AssertTrue(Treap[I] = Succ(I));
  for I := J to Pred(Treap.Count) do
    AssertTrue(Treap[I] = Succ(I) + Size);
  for I := 0 to Pred(Treap2.Count) do
    AssertTrue(Treap2[I] = Succ(I) + J );

  Treap.Insert(J, Treap2);
  for I := 0 to Pred(Treap.Count) do
    AssertTrue(Treap[I] = Succ(I));
  AssertTrue(Treap2.IsEmpty);
end;

procedure TLiteImplSegmentTreapTest.Merge;
const
  TestSize = 100;
var
  Treap, Treap1: TIntTreap;
  I: Integer;
begin
  for I := 1 to TestSize do
    Treap1.Add(I);
  AssertTrue({%H-}Treap.IsEmpty);
  Treap.Merge(Treap1);
  AssertTrue(Treap1.IsEmpty);
  AssertTrue(Treap.Count = TestSize);

  for I := TestSize + 1 to TestSize * 2 do
    Treap1.Add(I);
  Treap.Merge(Treap1);
  AssertTrue(Treap1.IsEmpty);
  AssertTrue(Treap.Count = TestSize * 2);
  for I := 1 to Treap.Count do
    AssertTrue(Treap[I-1] = I);
end;

procedure TLiteImplSegmentTreapTest.ToArray;
const
  TestSize = 100;
var
  Treap: TIntTreap;
  I: Integer;
  a: array of Integer;
begin
  a := Treap.ToArray;
  AssertTrue(a = nil);
  for I := 1 to TestSize do
    Treap.Add(I);
  a := Treap.ToArray;
  AssertTrue(Length(a) = TestSize);
  for I := 1 to TestSize do
    AssertTrue(a[I-1] = I);
end;

procedure TLiteImplSegmentTreapTest.RotateLeft;
const
  TestSize = 100;
var
  Treap: TIntTreap;
  I: Integer;
  a: array of Integer;
begin
  SetLength(a, TestSize);
  for I := 1 to TestSize do
    begin
      Treap.Add(I);
      a[I-1] := I;
    end;

  Treap.RotateLeft(0);
  for I := 0 to Pred(TestSize) do
    AssertTrue(Treap[I] = a[I]);

  Treap.RotateLeft(TestSize);
  for I := 0 to Pred(TestSize) do
    AssertTrue(Treap[I] = a[I]);

  Treap.RotateLeft(TestSize+10);
  for I := 0 to Pred(TestSize) do
    AssertTrue(Treap[I] = a[I]);

  Treap.RotateLeft(TestSize div 3);
  THelper.RotateLeft(a, TestSize div 3);
  for I := 0 to Pred(TestSize) do
    AssertTrue(Treap[I] = a[I]);

  Treap.RotateLeft(-(TestSize div 4));
  THelper.RotateLeft(a, -(TestSize div 4));
  for I := 0 to Pred(TestSize) do
    AssertTrue(Treap[I] = a[I]);
end;

procedure TLiteImplSegmentTreapTest.RotateRight;
const
  TestSize = 100;
var
  Treap: TIntTreap;
  I: Integer;
  a: array of Integer;
begin
  SetLength(a, TestSize);
  for I := 1 to TestSize do
    begin
      Treap.Add(I);
      a[I-1] := I;
    end;

  Treap.RotateRight(0);
  for I := 0 to Pred(TestSize) do
    AssertTrue(Treap[I] = a[I]);

  Treap.RotateRight(TestSize);
  for I := 0 to Pred(TestSize) do
    AssertTrue(Treap[I] = a[I]);

  Treap.RotateRight(TestSize+10);
  for I := 0 to Pred(TestSize) do
    AssertTrue(Treap[I] = a[I]);

  Treap.RotateRight(TestSize div 3);
  THelper.RotateRight(a, TestSize div 3);
  for I := 0 to Pred(TestSize) do
    AssertTrue(Treap[I] = a[I]);

  Treap.RotateRight(-(TestSize div 4));
  THelper.RotateRight(a, -(TestSize div 4));
  for I := 0 to Pred(TestSize) do
    AssertTrue(Treap[I] = a[I]);
end;

procedure TLiteImplSegmentTreapTest.RangeQuery;
const
  TestSize = 1000;
var
  Treap: TIntTreap;
  a: array of Integer;
  I, J, L, R, sum: Integer;
begin
  SetLength(a, TestSize);
  for I := 0 to Pred(TestSize) do
    begin
      J := Succ(Random(TestSize));
      a[I] := J;
      Treap.Add(J);
    end;
  for I := 0 to Pred(TestSize) do
    AssertTrue(Treap[I] = a[I]);

  sum := 0;
  for J := 0 to Pred(TestSize) do
    begin
      sum += a[J];
      AssertTrue(Treap.RangeQuery(0, J) = sum);
    end;

  for I := 0 to Pred(TestSize) do
    begin
      AssertTrue(Treap.RangeQuery(I, I) = a[I]);
      L := Random(TestSize div 2);
      R := TestSize div 2 + Random(TestSize div 2);
      sum := 0;
      for J := L to R do
        sum += a[J];
      AssertTrue(Treap.RangeQuery(L, R) = sum);
      AssertTrue(Treap.RangeQuery(R, L) = 0);
    end;
end;

procedure TLiteImplSegmentTreapTest.HeadQuery;
const
  TestSize = 1000;
var
  Treap: TIntTreap;
  a: array of Integer;
  I, J, sum: Integer;
begin
  SetLength(a, TestSize);
  for I := 0 to High(a) do
    begin
      J := Succ(Random(TestSize));
      a[I] := J;
      Treap.Add(J);
    end;

  for I := 0 to Pred(TestSize) do
    begin
      sum := 0;
      for J := 0 to I do
        sum += a[J];
      AssertTrue(Treap.HeadQuery(I) = sum);
    end;
end;

procedure TLiteImplSegmentTreapTest.TailQuery;
const
  TestSize = 1000;
var
  Treap: TIntTreap;
  a: array of Integer;
  I, J, sum: Integer;
begin
  SetLength(a, TestSize);
  for I := 0 to High(a) do
    begin
      J := Succ(Random(TestSize));
      a[I] := J;
      Treap.Add(J);
    end;

  for I := 0 to Pred(TestSize) do
    begin
      sum := 0;
      for J := I to Pred(TestSize) do
        sum += a[J];
      AssertTrue(Treap.TailQuery(I) = sum);
    end;
end;

procedure TLiteImplSegmentTreapTest.Items;
const
  TestSize = 1000;
var
  Treap: TIntTreap;
  a: array of Integer;
  I, J, sum: Integer;
begin
  SetLength(a, TestSize);
  for I := 0 to High(a) do
    begin
      J := Succ(Random(TestSize));
      a[I] := J;
      Treap.Add(J);
    end;

  sum := 0;
  for J := 0 to Pred(TestSize) do
    begin
      sum += a[J];
      AssertTrue(Treap.RangeQuery(0, J) = sum);
    end;

  for I := 0 to Pred(TestSize) do
    begin
      J := Random(TestSize);
      a[I] += J;
      Treap[I] := Treap[I] + J;
    end;
  sum := 0;
  for J := 0 to Pred(TestSize) do
    begin
      sum += a[J];
      AssertTrue(Treap.RangeQuery(0, J) = sum);
    end;

  System.Delete(a, TestSize div 2, 1);
  System.Delete(a, TestSize div 3, 1);
  Treap.Delete(TestSize div 2);
  Treap.Delete(TestSize div 3);

  sum := 0;
  for J := 0 to High(a) do
    begin
      sum += a[J];
      AssertTrue(Treap.RangeQuery(0, J) = sum);
    end;
end;

{ TLiteImplicitSegTreapTest }

procedure TLiteImplicitSegTreapTest.Add;
const
  TestSize = 100;
var
  Treap: TIntTreap;
  I: Integer;
begin
  AssertTrue({%H-}Treap.Count = 0);
  for I in [1..TestSize] do
    AssertTrue(Treap.Add(I) = I - 1);

  for I in [1..TestSize] do
    AssertTrue(Treap[I-1] = I);

  for I in [1..TestSize] do
    AssertTrue(Treap.Add(I) = I + Pred(TestSize));

  for I in [TestSize..Pred(TestSize * 2)] do
    AssertTrue(Treap[I] = I - Pred(TestSize));

  AssertTrue(Treap.Count = TestSize * 2);
end;

procedure TLiteImplicitSegTreapTest.Clear;
var
  Treap: TIntTreap;
  I: Integer;
begin
  for I in [1..16] do
    Treap.Add(I);
  AssertTrue(Treap.Count = 16);
  Treap.Clear;
  AssertTrue(Treap.Count = 0);
  for I in [1..16] do
    Treap.Add(I);
  AssertTrue(Treap.Count = 16);
end;

procedure TLiteImplicitSegTreapTest.Insert;
const
  TestSize = 100;
var
  Treap, Treap1: TIntTreap;
  I: Integer;
begin
  for I := 1 to TestSize do
    Treap.Insert(0, I);
  for I := 0 to Pred(TestSize) do
    AssertTrue(Treap[I] = TestSize - I);

  Treap.Clear;
  for I := 1 to TestSize do
    Treap.Insert(Treap.Count, I);
  for I := 0 to Pred(TestSize) do
    AssertTrue(Treap[I] = I + 1);

  for I := TestSize + 1 to TestSize * 2 do
    Treap1.Insert({%H-}Treap1.Count, I);
  Treap1.Insert(0, Treap);
  AssertTrue(Treap1.Count = TestSize * 2);
  for I := 1 to Treap1.Count do
    AssertTrue(Treap1[I-1] = I);
end;

procedure TLiteImplicitSegTreapTest.Delete;
const
  TestSize = 100;
var
  Treap: TIntTreap;
  I: Integer;
begin
  for I := 0 to Pred(TestSize) do
    Treap.Add(I);
  AssertTrue(Treap.Delete(Pred(TestSize)) = Pred(TestSize));
  I := 0;
  while not Treap.IsEmpty do
    begin
      AssertTrue(Treap.Delete(0) = I);
      Inc(I);
    end;
  AssertTrue(I = Pred(TestSize));
end;

procedure TLiteImplicitSegTreapTest.DeleteRange;
const
  TestSize = 100;
var
  Treap: TIntTreap;
  I: Integer;
begin
  for I := 0 to Pred(TestSize) do
    Treap.Add(I);
  AssertTrue(Treap.Count = TestSize);
  AssertTrue(Treap.Delete(Treap.Count div 2, TestSize) = TestSize div 2);
  AssertTrue(Treap.Count = TestSize div 2);
  AssertTrue(Treap.Delete(0, TestSize div 4) = TestSize div 4);
  AssertTrue(Treap.Count = TestSize div 4);
  AssertTrue(Treap.Delete(0, TestSize) = TestSize div 4);
  AssertTrue(Treap.IsEmpty);
end;

procedure TLiteImplicitSegTreapTest.Split;
const
  TestSize = 100;
var
  Treap, Treap2: TIntTreap;
  I: Integer;
begin
  for I in [1..TestSize] do
    Treap.Add(I);
  AssertTrue(Treap.Count = TestSize);

  Treap.Split(TestSize div 2, Treap2);
  AssertTrue(Treap.Count = TestSize div 2);
  AssertTrue(Treap2.Count = TestSize div 2);

  Treap2.Clear;
  Treap.Split(0, Treap2);
  AssertTrue(Treap.IsEmpty);
  AssertTrue(Treap2.Count = TestSize div 2);
end;

procedure TLiteImplicitSegTreapTest.SplitRange;
const
  TestSize = 1000;
var
  Treap, Treap2: TIntTreap;
  I, J, Size: Integer;
begin
  for I := 1 to TestSize do
    Treap.Add(I);
  AssertTrue(Treap.Count = TestSize);

  J := TestSize div 4;
  Size := TestSize div 2;
  Treap.Split(J, Size, Treap2);
  AssertTrue(Treap.Count = Size);
  AssertTrue(Treap2.Count = Size);
  for I := 0 to Pred(J) do
    AssertTrue(Treap[I] = Succ(I));
  for I := J to Pred(Treap.Count) do
    AssertTrue(Treap[I] = Succ(I) + Size);
  for I := 0 to Pred(Treap2.Count) do
    AssertTrue(Treap2[I] = Succ(I) + J );

  Treap.Insert(J, Treap2);
  for I := 0 to Pred(Treap.Count) do
    AssertTrue(Treap[I] = Succ(I));
  AssertTrue(Treap2.IsEmpty);
end;

procedure TLiteImplicitSegTreapTest.Merge;
const
  TestSize = 100;
var
  Treap, Treap1: TIntTreap;
  I: Integer;
begin
  for I := 1 to TestSize do
    Treap1.Add(I);
  AssertTrue({%H-}Treap.IsEmpty);
  Treap.Merge(Treap1);
  AssertTrue(Treap1.IsEmpty);
  AssertTrue(Treap.Count = TestSize);

  for I := TestSize + 1 to TestSize * 2 do
    Treap1.Add(I);
  Treap.Merge(Treap1);
  AssertTrue(Treap1.IsEmpty);
  AssertTrue(Treap.Count = TestSize * 2);
  for I := 1 to Treap.Count do
    AssertTrue(Treap[I-1] = I);
end;

procedure TLiteImplicitSegTreapTest.ToArray;
const
  TestSize = 100;
var
  Treap: TIntTreap;
  I: Integer;
  a: array of Integer;
begin
  a := Treap.ToArray;
  AssertTrue(a = nil);
  for I := 1 to TestSize do
    Treap.Add(I);
  a := Treap.ToArray;
  AssertTrue(Length(a) = TestSize);
  for I := 1 to TestSize do
    AssertTrue(a[I-1] = I);
end;

procedure TLiteImplicitSegTreapTest.RotateLeft;
const
  TestSize = 100;
var
  Treap: TIntTreap;
  I: Integer;
  a: array of Integer;
begin
  SetLength(a, TestSize);
  for I := 1 to TestSize do
    begin
      Treap.Add(I);
      a[I-1] := I;
    end;

  Treap.RotateLeft(0);
  for I := 0 to Pred(TestSize) do
    AssertTrue(Treap[I] = a[I]);

  Treap.RotateLeft(TestSize);
  for I := 0 to Pred(TestSize) do
    AssertTrue(Treap[I] = a[I]);

  Treap.RotateLeft(TestSize+10);
  for I := 0 to Pred(TestSize) do
    AssertTrue(Treap[I] = a[I]);

  Treap.RotateLeft(TestSize div 3);
  THelper.RotateLeft(a, TestSize div 3);
  for I := 0 to Pred(TestSize) do
    AssertTrue(Treap[I] = a[I]);

  Treap.RotateLeft(-(TestSize div 4));
  THelper.RotateLeft(a, -(TestSize div 4));
  for I := 0 to Pred(TestSize) do
    AssertTrue(Treap[I] = a[I]);
end;

procedure TLiteImplicitSegTreapTest.RotateRight;
const
  TestSize = 100;
var
  Treap: TIntTreap;
  I: Integer;
  a: array of Integer;
begin
  SetLength(a, TestSize);
  for I := 1 to TestSize do
    begin
      Treap.Add(I);
      a[I-1] := I;
    end;

  Treap.RotateRight(0);
  for I := 0 to Pred(TestSize) do
    AssertTrue(Treap[I] = a[I]);

  Treap.RotateRight(TestSize);
  for I := 0 to Pred(TestSize) do
    AssertTrue(Treap[I] = a[I]);

  Treap.RotateRight(TestSize+10);
  for I := 0 to Pred(TestSize) do
    AssertTrue(Treap[I] = a[I]);

  Treap.RotateRight(TestSize div 3);
  THelper.RotateRight(a, TestSize div 3);
  for I := 0 to Pred(TestSize) do
    AssertTrue(Treap[I] = a[I]);

  Treap.RotateRight(-(TestSize div 4));
  THelper.RotateRight(a, -(TestSize div 4));
  for I := 0 to Pred(TestSize) do
    AssertTrue(Treap[I] = a[I]);
end;

procedure TLiteImplicitSegTreapTest.Reverse;
const
  TestSize = 100;
var
  Treap: TIntTreap;
  I: Integer;
  a: array of Integer;
begin
  SetLength(a, TestSize);
  for I := 1 to TestSize do
    begin
      Treap.Add(I);
      a[I-1] := I;
    end;
  Treap.Reverse(TestSize div 3, TestSize div 3);
  THelper.Reverse(a[(TestSize div 3)..(TestSize div 3 + Pred(TestSize div 3))]);
  for I := 0 to Pred(TestSize) do
    AssertTrue(Treap[I] = a[I]);

  Treap.Reverse;
  THelper.Reverse(a);
  for I := 0 to Pred(TestSize) do
    AssertTrue(Treap[I] = a[I]);
end;

procedure TLiteImplicitSegTreapTest.RangeUpdate;
const
  TestSize = 100;
var
  Treap: TIntTreap;
  I, L, R: Integer;
  a: array of Integer;
begin
  SetLength(a, TestSize);
  for I := 1 to TestSize do
    begin
      Treap.Add(I);
      a[I-1] := I;
    end;

  L := TestSize div 3;
  R := L + TestSize div 2;

  Treap.RangeUpdate(L, R, 0);
  for I := 0 to Pred(TestSize) do
    AssertTrue(Treap[I] = a[I]);

  for I := L to R do
    a[I] += 100;
  Treap.RangeUpdate(L, R, 100);
  for I := 0 to Pred(TestSize) do
    AssertTrue(Treap[I] = a[I]);

  for I := 0 to Pred(TestSize) do
    a[I] -= 200;
  Treap.RangeUpdate(0, Pred(TestSize), -200);
  for I := 0 to Pred(TestSize) do
    AssertTrue(Treap[I] = a[I]);
end;

procedure TLiteImplicitSegTreapTest.RangeQuery;
const
  TestSize = 1000;
var
  Treap: TIntTreap;
  a: array of Integer;
  I, J, L, R, sum: Integer;
begin
  SetLength(a, TestSize);
  for I := 0 to Pred(TestSize) do
    begin
      J := Succ(Random(TestSize));
      a[I] := J;
      Treap.Add(J);
    end;
  for I := 0 to Pred(TestSize) do
    AssertTrue(Treap[I] = a[I]);

  sum := 0;
  for J := 0 to Pred(TestSize) do
    begin
      sum += a[J];
      AssertTrue(Treap.RangeQuery(0, J) = sum);
    end;

  for I := 0 to Pred(TestSize) do
    begin
      AssertTrue(Treap.RangeQuery(I, I) = a[I]);
      L := Random(TestSize div 2);
      R := TestSize div 2 + Random(TestSize div 2);
      sum := 0;
      for J := L to R do
        begin
          a[J] += 1;
          sum += a[J];
        end;
      Treap.RangeUpdate(L, R, 1);
      AssertTrue(Treap.RangeQuery(L, R) = sum);
      AssertTrue(Treap.RangeQuery(R, L) = 0);
    end;
end;

procedure TLiteImplicitSegTreapTest.HeadQuery;
const
  TestSize = 1000;
var
  Treap: TIntTreap;
  a: array of Integer;
  I, J, sum: Integer;
begin
  SetLength(a, TestSize);
  for I := 0 to High(a) do
    begin
      J := Succ(Random(TestSize));
      a[I] := J;
      Treap.Add(J);
    end;

  for I := 0 to Pred(TestSize) do
    begin
      sum := 0;
      for J := 0 to I do
        begin
          a[J] += 2;
          sum += a[J];
        end;
      Treap.RangeUpdate(0, I, 2);
      AssertTrue(Treap.HeadQuery(I) = sum);
    end;
end;

procedure TLiteImplicitSegTreapTest.TailQuery;
const
  TestSize = 1000;
var
  Treap: TIntTreap;
  a: array of Integer;
  I, J, sum: Integer;
begin
  SetLength(a, TestSize);
  for I := 0 to High(a) do
    begin
      J := Succ(Random(TestSize));
      a[I] := J;
      Treap.Add(J);
    end;

  for I := 0 to Pred(TestSize) do
    begin
      sum := 0;
      for J := I to Pred(TestSize) do
        begin
          a[J] += 2;
          sum += a[J];
        end;
      Treap.RangeUpdate(I, Pred(TestSize), 2);
      AssertTrue(Treap.TailQuery(I) = sum);
    end;
end;

procedure TLiteImplicitSegTreapTest.Items;
const
  TestSize = 1000;
var
  Treap: TIntTreap;
  a: array of Integer;
  I, J: Integer;
begin
  SetLength(a, TestSize);
  for I := 0 to High(a) do
    begin
      J := Succ(Random(TestSize));
      a[I] := J;
      Treap.Add(J);
    end;
  for I := 0 to Pred(TestSize) do
    AssertTrue(Treap[I] = a[I]);

  for I := 0 to Pred(TestSize) do
    begin
      J := Succ(Random(TestSize));
      a[I] += J;
      Treap[I] := Treap[I] + J;
    end;

  for I := 0 to Pred(TestSize) do
    AssertTrue(Treap[I] = a[I]);
end;

initialization

  RegisterTest(TLiteTreapTest);
  RegisterTest(TLiteIdxTreapTest);
  RegisterTest(TLiteSegmentTreapTest);
  RegisterTest(TLiteImplicitTreapTest);
  RegisterTest(TLiteImplSegmentTreapTest);
  RegisterTest(TLiteImplicitSegTreapTest);

end.

