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
    procedure SetZero;
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

procedure TUInt128Test.SetZero;
var
  I: TUInt128;
  J: Integer;
begin
  FillChar(I{%H-}, SizeOf(I), $ff);
  I.SetZero;
  for J := 0 to 3 do
    AssertTrue(I.DWords[J] = 0);
end;



initialization

  RegisterTest(TUInt128Test);

end.

