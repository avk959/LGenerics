unit LGUtilsTest;

{$mode objfpc}{$H+}
{$MODESWITCH NESTEDPROCVARS}

interface

uses

  Classes, SysUtils, fpcunit, testregistry,
  LGUtils,
  LGVector,
  LGMiscUtils;

type

  TCommonFunctionTest = class(TTestCase)
  published
    procedure NSBOfZero;
    procedure NSBOfAll;
    procedure NSBOfAll_1;

    procedure IsTwoPowerOfZero;
    procedure IsTwoPowerOfOne;
    procedure IsTwoPowerOfTwoPow;
    procedure IsTwoPowerOfNonTwoPow;

    procedure RoundUp2TwoPowerOfZero;
    procedure RoundUp2TwoPowerOfOne;
    procedure RoundUp2TwoPowerOfNeg;
    procedure RoundUp2TwoPowerAll;
  end;

  TAutoRefTest = class(TTestCase)
  private
  type
    TProc = procedure is nested;
    TTestClass = class
    private
      FProc: TProc;
    public
      destructor Destroy; override;
      property OnDestroy: TProc read FProc write FProc;
    end;
  published
    procedure Instance;
    procedure Assign;
    procedure Destruction;
  end;

  TGOptionalTest = class(TTestCase)
  private
  type
    TOptional        = specialize TGOptional<Integer>;
    TStrOptional     = specialize TGOptional<string>;
    TIntfOptional    = specialize TGOptional<IInterface>;
    IIntEnumerable   = specialize IGEnumerable<Integer>;
    TIntfRawOptional = specialize TGOptional<IIntEnumerable>;
    TIntArray        = specialize TGArray<Integer>;
    TArrayOptional   = specialize TGOptional<TIntArray>;
    TIntVector       = specialize TGVector<Integer>;
    TAutoVector      = specialize TGAutoRef<TIntVector>;
    EmptyError       = class(Exception);
  published
    procedure AssignInteger;
    procedure AssignString;
    procedure AssignEmptyString;
    procedure AssignInterface;
    procedure AssignNilInterface;
    procedure AssignInterfaceRaw;
    procedure AssignNilInterfaceRaw;
    procedure AssignArray;
    procedure AssignNilArray;
    procedure OrElseDefaultInt;
    procedure OrElseDefaultStr;
    procedure OrElseInt;
    procedure OrElseStr;
    procedure OrElseRaiseInt;
    procedure OrElseRaiseStr;
  end;

implementation
{$B-}{$COPERATORS ON}

procedure TCommonFunctionTest.NSBOfZero;
begin
  AssertTrue(NSB(0) = 0);
end;

procedure TCommonFunctionTest.NSBOfAll;
var
  I: SizeInt;
begin
  for I := 0 to Pred(BitSizeOf(SizeUInt)) do
    AssertTrue(NSB(SizeInt(1) shl I) = Succ(I));
end;

procedure TCommonFunctionTest.NSBOfAll_1;
var
  I, v: SizeInt;
begin
  v := 0;
  for I := 0 to Pred(BitSizeOf(SizeUInt)) do
    begin
      v += SizeInt(1) shl I;
      AssertTrue(NSB(v) = Succ(I));
    end;
end;

procedure TCommonFunctionTest.IsTwoPowerOfZero;
begin
  AssertFalse(IsTwoPower(0));
end;

procedure TCommonFunctionTest.IsTwoPowerOfOne;
begin
  AssertTrue(IsTwoPower(1));
end;

procedure TCommonFunctionTest.IsTwoPowerOfTwoPow;
var
  I: SizeInt;
begin
  for I := 0 to Pred(BitSizeOf(SizeInt)) do
    AssertTrue(IsTwoPower(SizeUInt(1) shl I));
end;

procedure TCommonFunctionTest.IsTwoPowerOfNonTwoPow;
var
  I: SizeInt;
begin
  for I := 2 to Pred(BitSizeOf(SizeInt)) do
    AssertFalse(IsTwoPower((SizeUInt(1) shl I) + 1));
end;

procedure TCommonFunctionTest.RoundUp2TwoPowerOfZero;
begin
  AssertTrue(RoundUpTwoPower(0) = 2);
end;

procedure TCommonFunctionTest.RoundUp2TwoPowerOfOne;
begin
  AssertTrue(RoundUpTwoPower(1) = 2);
end;

procedure TCommonFunctionTest.RoundUp2TwoPowerOfNeg;
begin
  AssertTrue(RoundUpTwoPower(-25) = 2);
end;

procedure TCommonFunctionTest.RoundUp2TwoPowerAll;
var
  I, v, TwoPow: SizeInt;
begin
  v := 0;
  for I := 0 to BitSizeOf(SizeInt) - 3 do
    begin
      v += SizeInt(1) shl I;
      TwoPow := SizeInt(1) shl Succ(I);
      AssertTrue(RoundUpTwoPower(v) = TwoPow);
    end;
end;

{ TAutoRefTest.TTestClass }

destructor TAutoRefTest.TTestClass.Destroy;
begin
  if Assigned(OnDestroy) then
    OnDestroy;
  inherited;
end;

{ TAutoRefTest }

procedure TAutoRefTest.Instance;
var
  Ref: specialize TGAutoRef<TStringList>;
  List: TStringList = nil;
begin
  List := {%H-}Ref.Instance;
  AssertTrue(Assigned(List));
  AssertTrue(List.Count = 0);
end;

procedure TAutoRefTest.Assign;
var
  Ref: specialize TGAutoRef<TStringList>;
  List: TStringList = nil;
begin
  List := {%H-}Ref;
  AssertTrue(Assigned(List));
  AssertTrue(List.Count = 0);
end;

procedure TAutoRefTest.Destruction;
var
  Ref: specialize TGAutoRef<TTestClass>;
  Inst: TTestClass = nil;
  Counter: Integer = 0;
  procedure IncCounter;
  begin
    Inc(Counter);
  end;
  procedure Test;
  var
    Ref: specialize TGAutoRef<TTestClass>;
    Inst: TTestClass = nil;
  begin
    Inst := {%H-}Ref;
    Inst.OnDestroy := @IncCounter;
  end;
begin
  Inst := {%H-}Ref;
  Inst.OnDestroy := @IncCounter;
  AssertTrue(Counter = 0);
  Ref.Instance := Default(TTestClass);
  AssertTrue(Counter = 1);
  Test;
  AssertTrue(Counter = 2);
end;

{ TGOptionalTest }

procedure TGOptionalTest.AssignInteger;
var
  o: TOptional;
begin
  AssertFalse(o.Nilable);
  AssertFalse({%H-}o.Assigned);
  o := 115;
  AssertTrue(o.Assigned);
  AssertTrue(o.Value = 115);
  o := 0;
  AssertTrue(o.Value = 0);
end;

procedure TGOptionalTest.AssignString;
var
  o: TStrOptional;
begin
  AssertFalse(o.Nilable);
  AssertFalse({%H-}o.Assigned);
  o := 'test string';
  AssertTrue(o.Assigned);
  AssertTrue(o.Value = 'test string');
end;

procedure TGOptionalTest.AssignEmptyString;
var
  o: TStrOptional;
begin
  AssertFalse({%H-}o.Assigned);
  o := '';
  AssertTrue(o.Assigned);
end;

procedure TGOptionalTest.AssignInterface;
var
  o: TIntfOptional;
begin
  AssertTrue('Expected Nilable, but got not', o.Nilable);
  AssertFalse({%H-}o.Assigned);
  o := TInterfacedObject.Create;
  AssertTrue(o.Assigned);
end;

procedure TGOptionalTest.AssignNilInterface;
var
  o: TIntfOptional;
begin
  AssertFalse({%H-}o.Assigned);
  o := IInterface(nil);
  AssertFalse(o.Assigned);
end;

procedure TGOptionalTest.AssignInterfaceRaw;
var
  o: TIntfRawOptional;
  v: TAutoVector;
begin
  AssertTrue(o.Nilable);
  AssertFalse({%H-}o.Assigned);
  o := {%H-}v.Instance;
  AssertTrue(o.Assigned);
end;

procedure TGOptionalTest.AssignNilInterfaceRaw;
var
  o: TIntfRawOptional;
begin
  AssertFalse({%H-}o.Assigned);
  o := IIntEnumerable(nil);
  AssertFalse(o.Assigned);
end;

procedure TGOptionalTest.AssignArray;
var
  o: TArrayOptional;
begin
  AssertTrue(o.Nilable);
  AssertFalse({%H-}o.Assigned);
  o := TIntArray([0, 11, 27]);
  AssertTrue(o.Assigned);
end;

procedure TGOptionalTest.AssignNilArray;
var
  o: TArrayOptional;
begin
  o := nil;
  AssertFalse(o.Assigned);
end;

procedure TGOptionalTest.OrElseDefaultInt;
var
  o: TOptional;
begin
  AssertFalse({%H-}o.Assigned);
  AssertTrue(o.OrElseDefault = 0);
end;

procedure TGOptionalTest.OrElseDefaultStr;
var
  o: TStrOptional;
begin
  AssertFalse({%H-}o.Assigned);
  AssertTrue(o.OrElseDefault = '');
end;

procedure TGOptionalTest.OrElseInt;
var
  o: TOptional;
begin
  AssertFalse({%H-}o.Assigned);
  AssertTrue(o.OrElse(121) = 121);
end;

procedure TGOptionalTest.OrElseStr;
var
  o: TStrOptional;
begin
  AssertFalse({%H-}o.Assigned);
  AssertTrue(o.OrElse('another test string') = 'another test string');
end;

procedure TGOptionalTest.OrElseRaiseInt;
var
  o: TOptional;
  Raised: Boolean = False;
begin
  try
    o.OrElseRaise(EmptyError);
  except
    on e: EmptyError do
      Raised := True;
  end;
  AssertTrue(Raised);
end;

procedure TGOptionalTest.OrElseRaiseStr;
var
  o: TStrOptional;
  Raised: Boolean = False;
begin
  try
    o.OrElseRaise(EmptyError);
  except
    on e: EmptyError do
      Raised := True;
  end;
  AssertTrue(Raised);
end;

initialization

  RegisterTest(TCommonFunctionTest);
  RegisterTest(TAutoRefTest);
  RegisterTest(TGOptionalTest);

end.

