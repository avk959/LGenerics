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
    procedure ToStringTest;
    procedure ToHexString;
    procedure TryParse;
    procedure CompareValue;
    procedure CompareDWord;
    procedure AssignDWord;
    procedure AssignString;
    procedure AsString;
    procedure IncValue;
    procedure DecValue;
    procedure ShiftLeft;
    procedure ShiftRight;
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

procedure TUInt128Test.ToStringTest;
var
  I: TUInt128;
begin
  I := Default(TUInt128);
  AssertTrue(I.ToString = '0');

  I := I.Encode(100, 0);
  AssertTrue(I.ToString = '100');

  I := I.Encode(High(QWord), 0);
  AssertTrue(I.ToString = '18446744073709551615');
  I := I.MaxValue;
  AssertTrue(I.ToString = '340282366920938463463374607431768211455');
end;

procedure TUInt128Test.ToHexString;
var
  I: TUInt128;
begin
  I := Default(TUInt128);
  AssertTrue(I.ToHexString = '00000000000000000000000000000000');
  AssertTrue(I.ToHexString(True) = '$00000000000000000000000000000000');
  AssertTrue(I.ToHexString(1) = '0');
  AssertTrue(I.ToHexString(1, True) = '$0');
  AssertTrue(I.ToHexString(7) = '0000000');
  AssertTrue(I.ToHexString(40) = '0000000000000000000000000000000000000000');

  I := I.Encode(High(QWord), 0);
  AssertTrue(I.ToHexString = '0000000000000000FFFFFFFFFFFFFFFF');
  AssertTrue(I.ToHexString(1) = 'FFFFFFFFFFFFFFFF');
  AssertTrue(I.ToHexString(20) = '0000FFFFFFFFFFFFFFFF');
  AssertTrue(I.ToHexString(22, True) = '$000000FFFFFFFFFFFFFFFF');

  I := I.MaxValue;
  AssertTrue(I.ToHexString = 'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF');
  AssertTrue(I.ToHexString(True) = '$FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF');
  AssertTrue(I.ToHexString(22) = 'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF');
  AssertTrue(I.ToHexString(42, True) = '$0000000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF');
end;

procedure TUInt128Test.TryParse;
var
  I: TUInt128;
begin
  AssertFalse(TUInt128.TryParse('', I));
  AssertFalse(TUInt128.TryParse('0x', I));
  AssertFalse(TUInt128.TryParse('x', I));
  AssertFalse(TUInt128.TryParse('-', I));
  AssertFalse(TUInt128.TryParse('0-', I));

  AssertTrue(TUInt128.TryParse('0', I));
  AssertTrue(I.IsZero);

  AssertTrue(TUInt128.TryParse('110', I));
  AssertTrue(I.ToString = '110');

  AssertTrue(TUInt128.TryParse('x11f', I));
  AssertTrue(I.ToHexString(1) = '11F');
  AssertTrue(TUInt128.TryParse('0x11f', I));
  AssertTrue(I.ToHexString(1) = '11F');
  AssertTrue(TUInt128.TryParse('0X11f', I));
  AssertTrue(I.ToHexString(1) = '11F');


  AssertTrue(TUInt128.TryParse('18446744073709551615', I));
  AssertTrue(I.ToString = '18446744073709551615');

  AssertTrue(TUInt128.TryParse('$ffffffffffffffff', I));
  AssertTrue(I.ToHexString(1) = 'FFFFFFFFFFFFFFFF');

  AssertTrue(TUInt128.TryParse('340282366920938463463374607431768211455', I));
  AssertTrue(I.ToString = '340282366920938463463374607431768211455');
end;

procedure TUInt128Test.CompareValue;
var
  I, J: TUInt128;
begin
  I := Default(TUInt128);
  J := Default(TUInt128);
  AssertTrue(I = J);
  AssertTrue(I >= J);
  AssertFalse(I > J);
  AssertTrue(I <= J);
  AssertFalse(I < J);

  I := TUInt128.Encode(1, 0);
  AssertFalse(I = J);
  AssertTrue(I >= J);
  AssertTrue(I > J);
  AssertFalse(I <= J);
  AssertFalse(I < J);

  J := TUInt128.Encode(0, 1);
  AssertFalse(I = J);
  AssertFalse(I >= J);
  AssertFalse(I > J);
  AssertTrue(I <= J);
  AssertTrue(I < J);

  I := TUInt128.Encode(0, 1);
  AssertTrue(I = J);
  AssertTrue(I >= J);
  AssertFalse(I > J);
  AssertTrue(I <= J);
  AssertFalse(I < J);
end;

procedure TUInt128Test.CompareDWord;
var
  I: TUInt128;
  d: DWord;
begin
  I := Default(TUInt128);
  d := 0;
  AssertTrue(I = d);
  AssertTrue(I >= d);
  AssertFalse(I > d);
  AssertTrue(I <= d);
  AssertFalse(I < d);

  I := TUInt128.Encode(1, 0);
  AssertFalse(I = d);
  AssertTrue(I >= d);
  AssertTrue(I > d);
  AssertFalse(I <= d);
  AssertFalse(I < d);

  d := d.MaxValue;
  AssertFalse(I = d);
  AssertFalse(I >= d);
  AssertFalse(I > d);
  AssertTrue(I <= d);
  AssertTrue(I < d);

  I := TUInt128.Encode(0, 1);
  AssertFalse(I = d);
  AssertTrue(I >= d);
  AssertTrue(I > d);
  AssertFalse(I <= d);
  AssertFalse(I < d);
end;

procedure TUInt128Test.AssignDWord;
var
  I: TUInt128;
  d: DWord;
begin
  d := 0;
  I := d;
  AssertTrue(I.IsZero);

  d := 100001;
  I := d;
  AssertTrue(I.ToString = '100001');

  d := DWord.MaxValue;
  I := d;
  AssertTrue(I.ToString = d.MaxValue.ToString);
end;

procedure TUInt128Test.AssignString;
var
  I: TUInt128;
begin
  I := '0';
  AssertTrue(I.IsZero);

  I := '10001';
  AssertTrue(I.ToString = '10001');

  I := '0X11f';
  AssertTrue(I.ToHexString(1) = '11F');


  I := '340282366920938463463374607431768211122';
  AssertTrue(I.ToString = '340282366920938463463374607431768211122');
end;

procedure TUInt128Test.AsString;
var
  I: TUInt128;
  s: string;
begin
  I := Default(TUInt128);
  s := I;
  AssertTrue(s = '0');

  I := I.Encode(10001, 0);
  s := I;
  AssertTrue(s = '10001');

  I := I.MaxValue;
  s := I;
  AssertTrue(s = '340282366920938463463374607431768211455');
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

procedure TUInt128Test.DecValue;
var
  I: TUInt128;
begin
  I := Default(TUInt128);
  Dec(I);
  AssertTrue(I = I.MaxValue);

  I := I.Encode(1, 0);
  Dec(I);
  AssertTrue(I.IsZero);

  I := I.Encode(0, 1);
  Dec(I);
  AssertTrue(I.Hi = 0);
  AssertTrue(I.Lo = High(QWord));
end;

procedure TUInt128Test.ShiftLeft;
var
  I, Expect: TUInt128;
  J: Integer;
begin
  I := Default(TUInt128);
  for J := 1 to Pred(BitSizeOf(I)) do
    AssertTrue(I shl J = 0);
  I := 1;
  AssertTrue(I shl 0 = 1);
  AssertTrue(I shl BitSizeOf(I) = 1);
  Expect := 0;
  for J := 0 to Pred(BitSizeOf(I)) do
    begin
      Expect.Bits[J] := True;
      AssertTrue(I shl J = Expect);
      Expect.Bits[J] := False;
    end;
end;

procedure TUInt128Test.ShiftRight;
var
  I, Expect: TUInt128;
  J: Integer;
begin
  I := TUInt128(1) shl 127;
  AssertTrue(I shr 0 = I);
  AssertTrue(I shr BitSizeOf(I) = I);
  Expect := 0;
  for J := 0 to Pred(BitSizeOf(I)) do
    begin
      Expect.Bits[Pred(BitSizeOf(I)) - J] := True;
      AssertTrue(I shr J = Expect);
      Expect.Bits[Pred(BitSizeOf(I)) - J] := False;
    end;
end;



initialization

  RegisterTest(TUInt128Test);

end.

