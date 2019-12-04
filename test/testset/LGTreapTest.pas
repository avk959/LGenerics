unit LGTreapTest;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, fpcunit, testregistry,
  LGUtils,
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
    procedure Remove;
    procedure RemoveStr;
    procedure Find;
    procedure FindStr;
    procedure CountOf;
    procedure CountOfStr;
    procedure Split;
    procedure SplitStr;
  end;

implementation

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

procedure TLiteTreapTest.Remove;
var
  Treap: TIntTreap;
  I: Integer;
begin
  AssertTrue(Treap.Count = 0);
  for I in [1..16] do
    Treap.Add(I);
  for I in [1..16] do
    Treap.Add(I);
  AssertTrue(Treap.Count = 32);
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
  AssertTrue(Treap.Count = 0);
  for I in [1..16] do
    Treap.Add('string ' + I.ToString);
  for I in [1..16] do
    Treap.Add('string ' + I.ToString);
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
  for I in [1..16] do
    begin
      p := Treap.Find(I);
      AssertTrue(p <> nil);
      AssertTrue(p^.Key = I);
    end;
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
  for I in [1..16] do
    begin
      s := 'string ' + I.ToString;
      p := Treap.Find(s);
      AssertTrue(p <> nil);
      AssertTrue(p^.Key = s);
    end;
end;

procedure TLiteTreapTest.CountOf;
var
  Treap: TIntTreap;
  I: Integer;
begin
  for I in [1..16] do
    begin
      Treap.Add(I);
      Treap.Add(I);
      Treap.Add(I);
    end;
  AssertTrue(Treap.CountOf(0) = 0);
  AssertTrue(Treap.Count = 48);
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



initialization

  RegisterTest(TLiteTreapTest);

end.
