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

  { TAutoRefTest }

  TAutoRefTest = class(TTestCase)
  private
  type

    TProc = procedure is nested;
    TTestClass = class
    private
      FProc: TProc;
      FValue: Integer;
    public
      destructor Destroy; override;
      property Value: Integer read FValue write FValue;
      property OnDestroy: TProc read FProc write FProc;
    end;
    TTestRef = specialize TGAutoRef<TTestClass>;

  var
    FTestRef: TTestRef;
    procedure CallByValue(aRef: TTestRef);
    procedure CallByValue2(aRef: TTestRef);
    procedure CallAsVar(var aRef: TTestRef; aValue: Integer);
    procedure CallAsOut(out aRef: TTestRef; aProc: TProc; aValue: Integer);
    procedure CallAsConst(const aRef: TTestRef);
    procedure CallAsConstref(constref aRef: TTestRef);
    procedure RefOwnMoveFail(aRef1: TTestRef; out aRef2: TTestRef);
    procedure RefOwnMove(var aRef1: TTestRef; out aRef2: TTestRef);
  published
    procedure Instance;
    procedure Implicit;
    procedure Explicit;
    procedure Copy;
    procedure PassByValue;
    procedure PassAsVar;
    procedure PassAsOut;
    procedure PassAsConst;
    procedure PassAsConstref;
    procedure HasInstance;
    procedure OwnMove;
    procedure OwnMoveFail;
    procedure Destruction;
    procedure Destruction1; //related to #0034772
  end;

  TUniqRefTest = class(TTestCase)
  private
  type
    TProc = procedure is nested;
    TTestClass = class
    private
      FProc: TProc;
    public
      constructor Create(aProc: TProc);
      destructor Destroy; override;
    end;
  published
    procedure Instance;
    procedure Implicit;
    procedure Explicit;
    procedure Copy;
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

procedure TAutoRefTest.CallByValue(aRef: TTestRef);
begin
  AssertFalse(aRef.OwnsInstance);
end;

procedure TAutoRefTest.CallByValue2(aRef: TTestRef);
var
  Counter: Integer = 0;
  procedure IncCounter;
  begin
    Inc(Counter);
  end;
begin
  AssertFalse(aRef.OwnsInstance);
  aRef.Instance := TTestClass.Create;
  TTestClass(aRef).OnDestroy := @IncCounter;
  AssertTrue(aRef.OwnsInstance);
  aRef.Instance := Default(TTestClass);
  AssertTrue(Counter = 1);
end;

procedure TAutoRefTest.CallAsVar(var aRef: TTestRef; aValue: Integer);
begin
  AssertTrue(aRef.OwnsInstance);
  TTestClass(aRef).Value := aValue;
end;

procedure TAutoRefTest.CallAsOut(out aRef: TTestRef; aProc: TProc; aValue: Integer);
begin
  aRef.Instance := TTestClass.Create;
  TTestClass(aRef).OnDestroy := aProc;
  TTestClass(aRef).Value := aValue;
end;

procedure TAutoRefTest.CallAsConst(const aRef: TTestRef);
begin
  AssertTrue(aRef.OwnsInstance);
end;

procedure TAutoRefTest.CallAsConstref(constref aRef: TTestRef);
begin
  AssertTrue(aRef.OwnsInstance);
end;

procedure TAutoRefTest.RefOwnMoveFail(aRef1: TTestRef; out aRef2: TTestRef);
begin
  aRef1.OwnMove(aRef2);
end;

procedure TAutoRefTest.RefOwnMove(var aRef1: TTestRef; out aRef2: TTestRef);
begin
  aRef1.OwnMove(aRef2);
end;

procedure TAutoRefTest.Instance;
var
  Ref: specialize TGAutoRef<TStringList>;
  List: TStringList = nil;
begin
  List := {%H-}Ref.Instance;
  AssertTrue(Assigned(List));
  AssertTrue(List.Count = 0);
end;

procedure TAutoRefTest.Implicit;
var
  Ref: specialize TGAutoRef<TStringList>;
  List: TStringList = nil;
begin
  List := {%H-}Ref;
  AssertTrue(Assigned(List));
  AssertTrue(List.Count = 0);
end;

procedure TAutoRefTest.Explicit;
var
  Ref: specialize TGAutoRef<TStringList>;
begin
  AssertTrue(TStringList(Ref).Count = 0);
  AssertTrue(TStringList(Ref).Add('line') = 0);
  AssertTrue(TStringList(Ref).Count = 1);
  AssertTrue(TStringList(Ref)[0] = 'line');
end;

procedure TAutoRefTest.Copy;
var
  Ref1, Ref2: specialize TGAutoRef<TStringList>;
  Rased: Boolean = False;
begin
  AssertTrue(TStringList(Ref1).Count = 0);
  try
    Ref2 := Ref1;
  except
    on e: EInvalidOpException do
      Rased := True;
  end;
  AssertTrue(Rased);
end;

procedure TAutoRefTest.PassByValue;
var
  Counter: Integer = 0;
  procedure IncCounter;
  begin
    Inc(Counter);
  end;
var
  Ref: specialize TGAutoRef<TTestClass>;
begin
  TTestClass(Ref).OnDestroy := @IncCounter;

  CallByValue(Ref);

  AssertTrue(Counter = 0);

  CallByValue2(Ref);

  AssertTrue(Counter = 0);
  Ref.Instance := nil;
  AssertTrue(Counter = 1);
end;

procedure TAutoRefTest.PassAsVar;
var
  Counter: Integer = 0;
  procedure IncCounter;
  begin
    Inc(Counter);
  end;
var
  Ref: specialize TGAutoRef<TTestClass>;
begin
  TTestClass(Ref).OnDestroy := @IncCounter;
  AssertTrue(TTestClass(Ref).Value = 0);

  CallAsVar(Ref, 15);

  AssertTrue(TTestClass(Ref).Value = 15);
  AssertTrue(Counter = 0);
  Ref.Instance := nil;
  AssertTrue(Counter = 1);
end;

procedure TAutoRefTest.PassAsOut;
var
  Counter1: Integer = 0;
  Counter2: Integer = 0;
  procedure IncCounter1;
  begin
    Inc(Counter1);
  end;
  procedure IncCounter2;
  begin
    Inc(Counter2);
  end;
var
  Ref: specialize TGAutoRef<TTestClass>;
begin
  TTestClass(Ref).OnDestroy := @IncCounter1;
  AssertTrue(TTestClass(Ref).Value = 0);

  CallAsOut(Ref, @IncCounter2, 25);

  AssertTrue(Counter1 = 1);
  AssertTrue(Counter2 = 0);
  AssertTrue(TTestClass(Ref).Value = 25);
  Ref.Instance := nil;
  AssertTrue(Counter1 = 1);
  AssertTrue(Counter2 = 1);
end;

procedure TAutoRefTest.PassAsConst;
var
  Counter: Integer = 0;
  procedure IncCounter;
  begin
    Inc(Counter);
  end;
var
  Ref: specialize TGAutoRef<TTestClass>;
begin
  TTestClass(Ref).OnDestroy := @IncCounter;

  CallAsConst(Ref);

  AssertTrue(Counter = 0);
  Ref.Instance := nil;
  AssertTrue(Counter = 1);
end;

procedure TAutoRefTest.PassAsConstref;
var
  Counter: Integer = 0;
  procedure IncCounter;
  begin
    Inc(Counter);
  end;
var
  Ref: specialize TGAutoRef<TTestClass>;
begin
  TTestClass(Ref).OnDestroy := @IncCounter;

  CallAsConstref(Ref);

  AssertTrue(Counter = 0);
  Ref.Instance := nil;
  AssertTrue(Counter = 1);
end;

procedure TAutoRefTest.HasInstance;
var
  Ref: specialize TGAutoRef<TStringList>;
begin
  AssertFalse(Ref.HasInstance);
  AssertTrue(TStringList(Ref).Count = 0);
  AssertTrue(Ref.HasInstance);
end;

procedure TAutoRefTest.OwnMove;
var
  Counter: Integer = 0;
  procedure IncCounter;
  begin
    Inc(Counter);
  end;
var
  Ref: specialize TGAutoRef<TTestClass>;
begin
  TTestClass(Ref).OnDestroy := @IncCounter;

  RefOwnMove(Ref, FTestRef);

  AssertFalse(Ref.HasInstance);
  TTestClass(Ref).OnDestroy := @IncCounter;
  AssertTrue(Counter = 0);
  Ref.Instance := nil;
  AssertTrue(Counter = 1);

  AssertTrue(FTestRef.HasInstance);
  FTestRef.Instance := nil;
  AssertTrue(Counter = 2);
end;

procedure TAutoRefTest.OwnMoveFail;
var
  Ref: specialize TGAutoRef<TTestClass>;
  Raised: Boolean = False;
begin
  TTestClass(Ref).Value := 2;

  try
    RefOwnMoveFail(Ref, FTestRef);
  except
    Raised := True;
  end;
  AssertTrue(Raised);
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

procedure TAutoRefTest.Destruction1;
var
  IsAssigned: Boolean = False;
  procedure Test;
  var
    Ref: specialize TGAutoRef<TObject>;
    Inst: TObject = nil;
  begin
    Inst := {%H-}Ref;
    try
      IsAssigned := Assigned(Inst);
      exit;
    finally
    end;
  end;
var
  Raised: Boolean = False;
begin
  try
    Test;
  except
    Raised := True;
  end;
  AssertTrue(IsAssigned);
  AssertFalse(Raised);
end;

{ TUniqRefTest.TTestClass }

constructor TUniqRefTest.TTestClass.Create(aProc: TProc);
begin
  FProc := aProc;
end;

destructor TUniqRefTest.TTestClass.Destroy;
begin
  if FProc <> nil then
    FProc();
  inherited;
end;

{ TUniqRefTest }

procedure TUniqRefTest.Instance;
begin

end;

procedure TUniqRefTest.Implicit;
begin

end;

procedure TUniqRefTest.Explicit;
begin

end;

procedure TUniqRefTest.Copy;
begin

end;

procedure TUniqRefTest.Destruction;
begin

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
  RegisterTest(TUniqRefTest);
  RegisterTest(TGOptionalTest);

end.

