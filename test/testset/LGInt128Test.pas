unit LGInt128Test;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, fpcunit, testregistry,
  LGInt128;

type

  { TUInt128Test }

  TUInt128Test = class(TTestCase)
  published
    procedure Lo_Hi;
    procedure IsZero;
    procedure SetZero;
    procedure Negate;
    procedure IsOdd;
    procedure PopCount;
    procedure IsTwoPower;
    procedure BitLength;
    procedure IncValue;
  end;

implementation

procedure TUInt128Test.Lo_Hi;
var
  I: TUInt128;
begin
  {%H-}I.DWords[0] := $11111111;
  I.DWords[1] := $22222222;
  I.DWords[2] := $33333333;
  I.DWords[3] := $44444444;
  AssertTrue(I.Lo = $2222222211111111);
  AssertTrue(I.Hi = $4444444433333333);
end;

procedure TUInt128Test.IsZero;
var
  I: TUInt128;
  J: Integer;
begin
  I := Default(TUInt128);
  for J := 0 to Pred(SizeOf(I)) do
    begin
      AssertTrue(I.IsZero);
      I.Bytes[J] := 1;
      AssertFalse(I.IsZero);
      I.Bytes[J] := 0;
    end;
end;

procedure TUInt128Test.SetZero;
var
  I: TUInt128;
  J: Integer;
begin
  FillChar(I{%H-}, SizeOf(I), $ff);
  AssertFalse(I.IsZero);
  I.SetZero;
  for J := 0 to 3 do
    AssertTrue(I.DWords[J] = 0);
  AssertTrue(I.IsZero);
end;

procedure TUInt128Test.Negate;
var
  I: TUInt128;
  J: Integer;
begin
  I := Default(TUInt128);
  I.Negate;
  for J := 0 to 3 do
    AssertTrue(I.DWords[J] = 0);

  I.DWords[0] := 1;
  I.Negate;
  for J := 0 to 3 do
    AssertTrue(I.DWords[J] = High(DWord));
  I.Negate;
  AssertTrue(I.DWords[0] = 1);
  for J := 1 to 3 do
    AssertTrue(I.DWords[J] = 0);

  I.DWords[0] := 0;
  I.DWords[1] := 5;
  I.Negate;
  AssertTrue(I.DWords[0] = 0);
  AssertTrue(I.DWords[1] = DWord(-5));
  for J := 2 to 3 do
    AssertTrue(I.DWords[J] = High(DWord));
  I.Negate;
  AssertTrue(I.DWords[0] = 0);
  AssertTrue(I.DWords[1] = 5);
  for J := 2 to 3 do
    AssertTrue(I.DWords[J] = 0);

  I.DWords[1] := 0;
  I.DWords[3] := $80000000;
  I.Negate;
  for J := 0 to 2 do
    AssertTrue(I.DWords[J] = 0);
  AssertTrue(I.DWords[3] = $80000000);
  I.Negate;
  for J := 0 to 2 do
    AssertTrue(I.DWords[J] = 0);
  AssertTrue(I.DWords[3] = $80000000);
end;

procedure TUInt128Test.IsOdd;
var
  I: TUInt128;
begin
  I := Default(TUInt128);
  AssertFalse(I.IsOdd);
  I.DWords[0] := 5;
  AssertTrue(I.IsOdd);
  I.DWords[0] := 8;
  AssertFalse(I.IsOdd);
end;

procedure TUInt128Test.PopCount;
var
  I: TUInt128;
  J: Integer;
begin
  I := I.MaxValue;
  AssertTrue(I.PopCount = 128);
  I := Default(TUInt128);
  AssertTrue(I.PopCount = 0);
  for J := 0 to Pred(SizeOf(I)) do
    begin
      I.Bytes[J] := $ff;
      AssertTrue(I.PopCount = 8);
      I.Bytes[J] := 0;
    end;
end;

procedure TUInt128Test.IsTwoPower;
var
  I: TUInt128;
  J: Integer;
begin
  I := Default(TUInt128);
  AssertFalse(I.IsTwoPower);
  for J := 0 to Pred(BitSizeOf(I)) do
    begin
      I.Bits[J] := True;
      AssertTrue(I.IsTwoPower);
      I.Bits[J] := False;
    end;
  I.DWords[3] := $c0000000;
  AssertFalse(I.IsTwoPower);
end;

procedure TUInt128Test.BitLength;
var
  I: TUInt128;
  J: Integer;
begin
  I := Default(TUInt128);
  AssertTrue(I.BitLength = 0);
  for J := 0 to Pred(BitSizeOf(I)) do
    begin
      I.Bits[J] := True;
      AssertTrue(I.BitLength = Succ(J));
      I.Bits[J] := False;
    end;
end;

procedure TUInt128Test.IncValue;
var
  I: TUInt128;
  J: Integer;
begin
  I := Default(TUInt128);
  Inc(I);
  AssertTrue(I.DWords[0] = 1);
  for J := 1 to 3 do
    AssertTrue(I.DWords[J] = 0);
  Inc(I);
  AssertTrue(I.DWords[0] = 2);
  for J := 1 to 3 do
    AssertTrue(I.DWords[J] = 0);

  I := I.Encode(High(QWord), 0);
  Inc(I);
  for J := 0 to 1 do
    AssertTrue(I.DWords[J] = 0);
  AssertTrue(I.DWords[2] = 1);
  AssertTrue(I.DWords[3] = 0);
  I := I.MaxValue;
  Inc(I);
  AssertTrue(I.IsZero);
end;



initialization

  RegisterTest(TUInt128Test);

end.

