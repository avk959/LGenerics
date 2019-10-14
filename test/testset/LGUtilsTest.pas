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
    procedure CallByValue(aRef: TTestRef; aValue: Integer);
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
    procedure ReleaseInstance;
    procedure OwnMove;
    procedure OwnMoveFail;
    procedure Destruction;
    procedure Destruction1; //related to #0034772
  end;

  { TUniqRefTest }

  TUniqRefTest = class(TTestCase)
  private
  type

    TProc = procedure is nested;
    TTestClass = class
    private
      FProc: TProc;
      FValue: Integer;
    public
      constructor Create(aProc: TProc);
      destructor Destroy; override;
      property Value: Integer read FValue write FValue;
      property OnDestroy: TProc read FProc write FProc;

    end;
    TTestRef = specialize TGUniqRef<TTestClass>;

  var
    FTestRef: TTestRef;
    procedure CallByValue(aRef: TTestRef; aValue: Integer);
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
    procedure ReleaseInstance;
    procedure OwnMove;
    procedure OwnMoveFail;
  end;

  { TSharedRefATest }

  TSharedRefATest = class(TTestCase)
  private
  type

    TProc = procedure of object;
    TTestClass = class
    private
      FProc: TProc;
      FValue: Integer;
    public
      destructor Destroy; override;
      property Value: Integer read FValue write FValue;
      property OnDestroy: TProc read FProc write FProc;

    end;
    TTestRef = specialize TGSharedRefA<TTestClass>;

  var
    FTestRef: TTestRef;
    FCounter: Integer;
    procedure IncCounter;
    procedure CallByValue(aRef: TTestRef; aValue: Integer);
    procedure CallByValue2(aRef: TTestRef);
    procedure CallByValue3(aRef: TTestRef; aValue: Integer);
    procedure CallAsVar(var aRef: TTestRef; aValue: Integer);
    procedure CallAsOut(out aRef: TTestRef; aValue: Integer);
    procedure CallAsConst(const aRef: TTestRef);
    procedure CallAsConstref(constref aRef: TTestRef);
  published
    procedure Instance;
    procedure Implicit;
    procedure Explicit;
    procedure CopyRefCount;
    procedure PassByValue;
    procedure PassAsVar;
    procedure PassAsOut;
    procedure PassAsConst;
    procedure PassAsConstref;
    procedure HasInstance;
    procedure Release;
  end;

  { TSharedRefTest }

  TSharedRefTest = class(TTestCase)
  private
  type

    TProc = procedure of object;
    TTestClass = class
    private
      FProc: TProc;
      FValue: Integer;
    public
      constructor Create(aProc: TProc);
      destructor Destroy; override;
      property Value: Integer read FValue write FValue;
      property OnDestroy: TProc read FProc write FProc;

    end;
    TTestRef = specialize TGSharedRef<TTestClass>;

  var
    FTestRef: TTestRef;
    FCounter: Integer;
    procedure IncCounter;
    procedure CallByValue(aRef: TTestRef; aValue: Integer);
    procedure CallByValue2(aRef: TTestRef);
    procedure CallByValue3(aRef: TTestRef; aValue: Integer);
    procedure CallAsVar(var aRef: TTestRef; aValue: Integer);
    procedure CallAsOut(out aRef: TTestRef; aValue: Integer);
    procedure CallAsConst(const aRef: TTestRef);
    procedure CallAsConstref(constref aRef: TTestRef);
  published
    procedure Instance;
    procedure Implicit;
    procedure Explicit;
    procedure CopyRefCount;
    procedure PassByValue;
    procedure PassAsVar;
    procedure PassAsOut;
    procedure PassAsConst;
    procedure PassAsConstref;
    procedure HasInstance;
    procedure Release;
  end;

  { TCowPtrTest }

  TCowPtrTest = class(TTestCase)
  private
  type
    TTestColor = (tcBlack, tcRed, tcYellow, tcGreen, tcBlue, tcWhite);
    TTestRec = record
      Name: string[50];
      Color: TTestColor;
      Count: Integer;
    end;

    TTestPtr = specialize TGCowPtr<TTestRec>;
  const
    CONST_REC: TTestRec = (Name: 'CONST_REC'; Color: tcWhite; Count: 1);
  var
    FTestPtr: TTestPtr;
    procedure CallByValue(aPtr: TTestPtr; aColor: TTestColor; aCount: Integer);
    procedure CallByValue2(aSrc: TTestPtr; aCount: Integer; out aDst: TTestPtr);
    procedure CallAsVar(var aPtr: TTestPtr; aColor: TTestColor; aCount: Integer);
  published
    procedure Allocated;
    procedure Implicit;
    procedure CopyRefCount;
    procedure ReadPtr;
    procedure UniqPtr;
    procedure PassByValue;
    procedure PassAsVar;
    procedure Release;
    procedure Unique;
    procedure Value;
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

procedure TAutoRefTest.CallByValue(aRef: TTestRef; aValue: Integer);
begin
  AssertFalse(aRef.OwnsInstance);
  TTestClass(aRef).Value := aValue;
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
  AssertFalse({%H-}aRef.HasInstance);
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
  aRef1.OwnMove(aRef2{%H-});
end;

procedure TAutoRefTest.RefOwnMove(var aRef1: TTestRef; out aRef2: TTestRef);
begin
  aRef1.OwnMove(aRef2{%H-});
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

  CallByValue(Ref, -5);

  AssertTrue(Counter = 0);
  AssertTrue(TTestClass(Ref).Value = -5);

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
  AssertFalse({%H-}Ref.HasInstance);
  AssertTrue(TStringList(Ref).Count = 0);
  AssertTrue(Ref.HasInstance);
  Ref.Instance := nil;
  AssertFalse(Ref.HasInstance);
end;

procedure TAutoRefTest.ReleaseInstance;
var
  Ref: specialize TGAutoRef<TStringList>;
  List: TStringList;
begin
  TStringList(Ref).Add('line');
  AssertTrue(TStringList(Ref).Count = 1);
  AssertTrue(Ref.HasInstance);
  List := Ref.ReleaseInstance;
  AssertFalse(Ref.HasInstance);
  AssertTrue(List.Count = 1);
  AssertTrue(List[0] = 'line');
  List.Free;
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
  AssertTrue(Counter = 0);

  AssertTrue(FTestRef.HasInstance);
  FTestRef.Instance := nil;
  AssertTrue(Counter = 1);
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
  AssertTrue(TTestClass(Ref).Value = 2);
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

procedure TUniqRefTest.CallByValue(aRef: TTestRef; aValue: Integer);
begin
  AssertFalse(aRef.OwnsInstance);
  TTestClass(aRef).Value := aValue;
end;

procedure TUniqRefTest.CallByValue2(aRef: TTestRef);
var
  Counter: Integer = 0;
  procedure IncCounter;
  begin
    Inc(Counter);
  end;
begin
  AssertFalse(aRef.OwnsInstance);
  aRef.Instance := TTestClass.Create(@IncCounter);
  AssertTrue(aRef.OwnsInstance);
  aRef.Instance := Default(TTestClass);
  AssertTrue(Counter = 1);
end;

procedure TUniqRefTest.CallAsVar(var aRef: TTestRef; aValue: Integer);
begin
  AssertTrue(aRef.OwnsInstance);
  TTestClass(aRef).Value := aValue;
end;

procedure TUniqRefTest.CallAsOut(out aRef: TTestRef; aProc: TProc; aValue: Integer);
begin
  AssertFalse({%H-}aRef.HasInstance);
  aRef.Instance := TTestClass.Create(aProc);
  TTestClass(aRef).Value := aValue;
end;

procedure TUniqRefTest.CallAsConst(const aRef: TTestRef);
begin
  AssertTrue(aRef.OwnsInstance);
end;

procedure TUniqRefTest.CallAsConstref(constref aRef: TTestRef);
begin
  AssertTrue(aRef.OwnsInstance);
end;

procedure TUniqRefTest.RefOwnMoveFail(aRef1: TTestRef; out aRef2: TTestRef);
begin
  aRef1.OwnMove(aRef2{%H-});
end;

procedure TUniqRefTest.RefOwnMove(var aRef1: TTestRef; out aRef2: TTestRef);
begin
  aRef1.OwnMove(aRef2{%H-});
end;

procedure TUniqRefTest.Instance;
var
  Ref: specialize TGUniqRef<TStringList>;
begin
  AssertFalse(Assigned({%H-}Ref.Instance));
  Ref.Instance := TStringList.Create;
  AssertTrue(Assigned(Ref.Instance));
  AssertTrue(Ref.Instance.Count = 0);
end;

procedure TUniqRefTest.Implicit;
var
  Ref: specialize TGUniqRef<TStringList>;
  List: TStringList = nil;
begin
  List := {%H-}Ref;
  AssertFalse(Assigned(List));
  Ref.Instance := TStringList.Create;
  List := Ref;
  AssertTrue(List.Count = 0);
end;

procedure TUniqRefTest.Explicit;
var
  Ref: specialize TGUniqRef<TStringList>;
begin
  AssertFalse(Assigned({%H-}Ref.Instance));
  Ref.Instance := TStringList.Create;
  AssertTrue(TStringList(Ref).Count = 0);
  AssertTrue(TStringList(Ref).Add('line') = 0);
  AssertTrue(TStringList(Ref).Count = 1);
  AssertTrue(TStringList(Ref)[0] = 'line');
end;

procedure TUniqRefTest.Copy;
var
  Ref1, Ref2: specialize TGUniqRef<TStringList>;
  Rased: Boolean = False;
begin
  Ref1.Instance := TStringList.Create;
  AssertTrue(TStringList(Ref1).Count = 0);
  try
    Ref2 := Ref1;
  except
    on e: EInvalidOpException do
      Rased := True;
  end;
  AssertTrue(Rased);
end;

procedure TUniqRefTest.PassByValue;
var
  Counter: Integer = 0;
  procedure IncCounter;
  begin
    Inc(Counter);
  end;
var
  Ref: specialize TGUniqRef<TTestClass>;
begin
  {%H-}Ref.Instance := TTestClass.Create(@IncCounter);

  CallByValue(Ref, -5);

  AssertTrue(Counter = 0);
  AssertTrue(TTestClass(Ref).Value = -5);

  CallByValue2(Ref);

  AssertTrue(Counter = 0);
  Ref.Instance := nil;
  AssertTrue(Counter = 1);
end;

procedure TUniqRefTest.PassAsVar;
var
  Counter: Integer = 0;
  procedure IncCounter;
  begin
    Inc(Counter);
  end;
var
  Ref: specialize TGUniqRef<TTestClass>;
begin
  {%H-}Ref.Instance := TTestClass.Create(@IncCounter);
  AssertTrue(TTestClass(Ref).Value = 0);

  CallAsVar(Ref, 15);

  AssertTrue(TTestClass(Ref).Value = 15);
  AssertTrue(Counter = 0);
  Ref.Instance := nil;
  AssertTrue(Counter = 1);
end;

procedure TUniqRefTest.PassAsOut;
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
  Ref: specialize TGUniqRef<TTestClass>;
begin
  {%H-}Ref.Instance := TTestClass.Create(@IncCounter1);
  AssertTrue(TTestClass(Ref).Value = 0);

  CallAsOut(Ref, @IncCounter2, 25);

  AssertTrue(Counter1 = 1);
  AssertTrue(Counter2 = 0);
  AssertTrue(TTestClass(Ref).Value = 25);
  Ref.Instance := nil;
  AssertTrue(Counter1 = 1);
  AssertTrue(Counter2 = 1);
end;

procedure TUniqRefTest.PassAsConst;
var
  Counter: Integer = 0;
  procedure IncCounter;
  begin
    Inc(Counter);
  end;
var
  Ref: specialize TGUniqRef<TTestClass>;
begin
  {%H-}Ref.Instance := TTestClass.Create(@IncCounter);

  CallAsConst(Ref);

  AssertTrue(Counter = 0);
  Ref.Instance := nil;
  AssertTrue(Counter = 1);
end;

procedure TUniqRefTest.PassAsConstref;
var
  Counter: Integer = 0;
  procedure IncCounter;
  begin
    Inc(Counter);
  end;
var
  Ref: specialize TGUniqRef<TTestClass>;
begin
  {%H-}Ref.Instance := TTestClass.Create(@IncCounter);

  CallAsConstref(Ref);

  AssertTrue(Counter = 0);
  Ref.Instance := nil;
  AssertTrue(Counter = 1);
end;

procedure TUniqRefTest.HasInstance;
var
  Ref: specialize TGUniqRef<TStringList>;
begin
  AssertFalse({%H-}Ref.HasInstance);
  Ref.Instance := TStringList.Create;
  AssertTrue(Ref.HasInstance);
  Ref.Instance := nil;
  AssertFalse(Ref.HasInstance);
end;

procedure TUniqRefTest.ReleaseInstance;
var
  Ref: specialize TGUniqRef<TStringList>;
  List: TStringList;
begin
  {%H-}Ref.Instance := TStringList.Create;
  TStringList(Ref).Add('line');
  AssertTrue(TStringList(Ref).Count = 1);
  AssertTrue(Ref.HasInstance);
  List := Ref.ReleaseInstance;
  AssertFalse(Ref.HasInstance);
  AssertTrue(List.Count = 1);
  AssertTrue(List[0] = 'line');
  List.Free;
end;

procedure TUniqRefTest.OwnMove;
var
  Counter: Integer = 0;
  procedure IncCounter;
  begin
    Inc(Counter);
  end;
var
  Ref: specialize TGUniqRef<TTestClass>;
begin
  {%H-}Ref.Instance := TTestClass.Create(@IncCounter);

  RefOwnMove(Ref, FTestRef);

  AssertFalse(Ref.HasInstance);
  AssertTrue(Counter = 0);

  AssertTrue(FTestRef.HasInstance);
  FTestRef.Instance := nil;
  AssertTrue(Counter = 1);
end;

procedure TUniqRefTest.OwnMoveFail;
var
  Ref: specialize TGUniqRef<TTestClass>;
  Raised: Boolean = False;
begin
  {%H-}Ref.Instance := TTestClass.Create(nil);
  TTestClass(Ref).Value := 2;

  try
    RefOwnMoveFail(Ref, FTestRef);
  except
    Raised := True;
  end;
  AssertTrue(Raised);
  AssertTrue(TTestClass(Ref).Value = 2);
end;

{ TSharedRefATest.TTestClass }

destructor TSharedRefATest.TTestClass.Destroy;
begin
  if FProc <> nil then
    FProc;
  inherited;
end;

{ TSharedRefATest }

procedure TSharedRefATest.IncCounter;
begin
  Inc(FCounter);
end;

procedure TSharedRefATest.CallByValue(aRef: TTestRef; aValue: Integer);
begin
  AssertTrue(aRef.RefCount = 2);
  TTestClass(aRef).Value := aValue;
end;

procedure TSharedRefATest.CallByValue2(aRef: TTestRef);
begin
  FTestRef := aRef;
  AssertTrue(aRef.RefCount = 3);
end;

procedure TSharedRefATest.CallByValue3(aRef: TTestRef; aValue: Integer);
begin
  aRef.Instance := TTestClass.Create;
  TTestClass(aRef).Value := aValue;
end;

procedure TSharedRefATest.CallAsVar(var aRef: TTestRef; aValue: Integer);
begin
  AssertTrue(aRef.RefCount = 1);
  aRef.Instance := TTestClass.Create;
  TTestClass(aRef).OnDestroy := @IncCounter;
  TTestClass(aRef).Value := aValue;
end;

procedure TSharedRefATest.CallAsOut(out aRef: TTestRef; aValue: Integer);
begin
  AssertTrue({%H-}aRef.RefCount = 0);
  TTestClass(aRef).OnDestroy := @IncCounter;
  TTestClass(aRef).Value := aValue;
end;

procedure TSharedRefATest.CallAsConst(const aRef: TTestRef);
begin
  AssertTrue(aRef.RefCount = 1);
end;

procedure TSharedRefATest.CallAsConstref(constref aRef: TTestRef);
begin
  AssertTrue(aRef.RefCount = 1);
end;

procedure TSharedRefATest.Instance;
var
  Ref: specialize TGSharedRefA<TStringList>;
begin
  AssertTrue(Assigned({%H-}Ref.Instance));
  AssertTrue(Ref.Instance.Count = 0);
end;

procedure TSharedRefATest.Implicit;
var
  Ref: specialize TGSharedRefA<TStringList>;
  List: TStringList = nil;
begin
  List := {%H-}Ref;
  AssertTrue(Assigned(List));
  AssertTrue(List.Count = 0);
end;

procedure TSharedRefATest.Explicit;
var
  Ref: specialize TGSharedRefA<TStringList>;
begin
  AssertTrue(TStringList(Ref).Count = 0);
  AssertTrue(TStringList(Ref).Add('line') = 0);
  AssertTrue(TStringList(Ref).Count = 1);
  AssertTrue(TStringList(Ref)[0] = 'line');
end;

procedure TSharedRefATest.CopyRefCount;
var
  Ref, Ref2: specialize TGSharedRefA<TTestClass>;
begin
  FCounter := 0;
  AssertTrue({%H-}Ref.RefCount = 0);
  Ref2 := Ref;
  AssertTrue(Ref2.RefCount = 0);
  AssertFalse(Ref2.HasInstance);

  AssertTrue(Assigned(Ref.Instance));
  TTestClass(Ref).OnDestroy := @IncCounter;
  AssertTrue(Ref.RefCount = 1);

  Ref2 := Ref;
  AssertTrue(Ref.RefCount = 2);
  AssertTrue(Ref2.RefCount = 2);

  FTestRef := Ref2;
  AssertTrue(Ref.RefCount = 3);
  AssertTrue(Ref2.RefCount = 3);
  AssertTrue(FTestRef.RefCount = 3);

  FTestRef.Instance := nil;
  AssertTrue(Ref.RefCount = 2);
  AssertTrue(Ref2.RefCount = 2);
  AssertTrue(FTestRef.RefCount = 0);
  AssertTrue(FCounter = 0);

  Ref2.Instance := nil;
  AssertTrue(Ref.RefCount = 1);
  AssertTrue(Ref2.RefCount = 0);
  AssertTrue(FCounter = 0);

  Ref.Instance := nil;
  AssertTrue(Ref.RefCount = 0);
  AssertTrue(FCounter = 1);
end;

procedure TSharedRefATest.PassByValue;
var
  Ref: specialize TGSharedRefA<TTestClass>;
begin
  TTestClass(Ref).Value := -5;

  CallByValue(Ref, 5);

  AssertTrue(Ref.RefCount = 1);
  AssertTrue(TTestClass(Ref).Value = 5);
  FCounter := 0;
  TTestClass(Ref).OnDestroy := @IncCounter;

  CallByValue2(Ref);

  AssertTrue(FCounter = 0);
  AssertTrue(Ref.RefCount = 2);
  AssertTrue(TTestClass(Ref).Value = 5);
  FTestRef.Instance := nil;
  AssertTrue(Ref.RefCount = 1);

  CallByValue3(Ref, 15);

  AssertTrue(Ref.RefCount = 1);
  AssertTrue(FCounter = 0);
  AssertTrue(TTestClass(Ref).Value = 5);
end;

procedure TSharedRefATest.PassAsVar;
var
  Ref: specialize TGSharedRefA<TTestClass>;
begin
  FCounter := 0;
  TTestClass(Ref).OnDestroy := @IncCounter;

  CallAsVar(Ref, 111);

  AssertTrue(FCounter = 1);
  AssertTrue(TTestClass(Ref).Value = 111);
end;

procedure TSharedRefATest.PassAsOut;
var
  Ref: specialize TGSharedRefA<TTestClass>;
begin
  FCounter := 0;
  TTestClass(Ref).OnDestroy := @IncCounter;

  CallAsOut(Ref, 111);

  AssertTrue(FCounter = 1);
  AssertTrue(Ref.RefCount = 1);
  AssertTrue(TTestClass(Ref).Value = 111);
end;

procedure TSharedRefATest.PassAsConst;
var
  Ref: specialize TGSharedRefA<TTestClass>;
begin
  FCounter := 0;
  TTestClass(Ref).OnDestroy := @IncCounter;

  CallAsConst(Ref);

  AssertTrue(FCounter = 0);
end;

procedure TSharedRefATest.PassAsConstref;
var
  Ref: specialize TGSharedRefA<TTestClass>;
begin
  FCounter := 0;
  TTestClass(Ref).OnDestroy := @IncCounter;

  CallAsConstref(Ref);

  AssertTrue(FCounter = 0);
end;

procedure TSharedRefATest.HasInstance;
var
  Ref: specialize TGSharedRefA<TStringList>;
begin
  AssertFalse({%H-}Ref.HasInstance);
  AssertTrue(TStringList(Ref).Count = 0);
  AssertTrue(Ref.HasInstance);
  Ref.Instance := nil;
  AssertFalse(Ref.HasInstance);
end;

procedure TSharedRefATest.Release;
var
  Ref, Ref2: specialize TGSharedRefA<TTestClass>;
begin
  FCounter := 0;
  AssertTrue({%H-}Ref.RefCount = 0);
  Ref.Release;
  AssertTrue({%H-}Ref.RefCount = 0);

  TTestClass(Ref).OnDestroy := @IncCounter;
  AssertTrue(Ref.RefCount = 1);
  AssertTrue({%H-}Ref2.RefCount = 0);
  Ref2 := Ref;
  AssertTrue(Ref.RefCount = 2);
  AssertTrue(Ref2.RefCount = 2);
  FTestRef := Ref2;
  AssertTrue(Ref.RefCount = 3);
  AssertTrue(Ref2.RefCount = 3);
  AssertTrue(FTestRef.RefCount = 3);
  FTestRef.Release;
  AssertTrue(Ref.RefCount = 2);
  AssertTrue(Ref2.RefCount = 2);
  AssertTrue(FTestRef.RefCount = 0);
  AssertTrue(FCounter = 0);
  Ref2.Release;
  AssertTrue(Ref.RefCount = 1);
  AssertTrue(Ref2.RefCount = 0);
  AssertTrue(FCounter = 0);
  Ref.Release;
  AssertTrue(Ref.RefCount = 0);
  AssertTrue(FCounter = 1);
end;

{ TSharedRefTest.TTestClass }

constructor TSharedRefTest.TTestClass.Create(aProc: TProc);
begin
  FProc := aProc;
end;

destructor TSharedRefTest.TTestClass.Destroy;
begin
  if FProc <> nil then
    FProc;
  inherited;
end;

{ TSharedRefTest }

procedure TSharedRefTest.IncCounter;
begin
  Inc(FCounter);
end;

procedure TSharedRefTest.CallByValue(aRef: TTestRef; aValue: Integer);
begin
  AssertTrue(aRef.RefCount = 2);
  TTestClass(aRef).Value := aValue;
end;

procedure TSharedRefTest.CallByValue2(aRef: TTestRef);
begin
  FTestRef := aRef;
  AssertTrue(aRef.RefCount = 3);
end;

procedure TSharedRefTest.CallByValue3(aRef: TTestRef; aValue: Integer);
begin
  aRef.Instance := TTestClass.Create(nil);
  TTestClass(aRef).Value := aValue;
end;

procedure TSharedRefTest.CallAsVar(var aRef: TTestRef; aValue: Integer);
begin
  AssertTrue(aRef.RefCount = 1);
  aRef.Instance := TTestClass.Create(@IncCounter);
  TTestClass(aRef).Value := aValue;
end;

procedure TSharedRefTest.CallAsOut(out aRef: TTestRef; aValue: Integer);
begin
  AssertTrue({%H-}aRef.RefCount = 0);
  aRef.Instance := TTestClass.Create(@IncCounter);
  TTestClass(aRef).Value := aValue;
end;

procedure TSharedRefTest.CallAsConst(const aRef: TTestRef);
begin
  AssertTrue(aRef.RefCount = 1);
end;

procedure TSharedRefTest.CallAsConstref(constref aRef: TTestRef);
begin
  AssertTrue(aRef.RefCount = 1);
end;

procedure TSharedRefTest.Instance;
var
  Ref: specialize TGSharedRef<TStringList>;
begin
  AssertFalse(Assigned({%H-}Ref.Instance));
  Ref.Instance := TStringList.Create;
  AssertTrue(Assigned(Ref.Instance));
  AssertTrue(Ref.Instance.Count = 0);
end;

procedure TSharedRefTest.Implicit;
var
  Ref: specialize TGSharedRef<TStringList>;
  List: TStringList = nil;
begin
  List := {%H-}Ref;
  AssertFalse(Assigned(List));
  Ref.Instance := TStringList.Create;
  List := Ref;
  AssertTrue(List.Count = 0);
end;

procedure TSharedRefTest.Explicit;
var
  Ref: specialize TGSharedRef<TStringList>;
begin
  AssertFalse(Assigned({%H-}Ref.Instance));
  Ref.Instance := TStringList.Create;
  AssertTrue(TStringList(Ref).Count = 0);
  AssertTrue(TStringList(Ref).Add('line') = 0);
  AssertTrue(TStringList(Ref).Count = 1);
  AssertTrue(TStringList(Ref)[0] = 'line');
end;

procedure TSharedRefTest.CopyRefCount;
var
  Ref, Ref2: specialize TGSharedRef<TTestClass>;
begin
  FCounter := 0;
  AssertTrue({%H-}Ref.RefCount = 0);
  Ref2 := Ref;
  AssertTrue(Ref2.RefCount = 0);
  AssertFalse(Ref2.HasInstance);

  Ref.Instance := TTestClass.Create(@IncCounter);
  AssertTrue(Assigned(Ref.Instance));
  AssertTrue(Ref.RefCount = 1);

  Ref2 := Ref;
  AssertTrue(Ref.RefCount = 2);
  AssertTrue(Ref2.RefCount = 2);

  FTestRef := Ref2;
  AssertTrue(Ref.RefCount = 3);
  AssertTrue(Ref2.RefCount = 3);
  AssertTrue(FTestRef.RefCount = 3);

  FTestRef.Instance := nil;
  AssertTrue(Ref.RefCount = 2);
  AssertTrue(Ref2.RefCount = 2);
  AssertTrue(FTestRef.RefCount = 0);
  AssertTrue(FCounter = 0);

  Ref2.Instance := nil;
  AssertTrue(Ref.RefCount = 1);
  AssertTrue(Ref2.RefCount = 0);
  AssertTrue(FCounter = 0);

  Ref.Instance := nil;
  AssertTrue(Ref.RefCount = 0);
  AssertTrue(FCounter = 1);
end;

procedure TSharedRefTest.PassByValue;
var
  Ref: specialize TGSharedRef<TTestClass>;
begin
  FCounter := 0;
  Ref.Instance := TTestClass.Create(@IncCounter);
  TTestClass(Ref).Value := -5;

  CallByValue(Ref, 5);

  AssertTrue(Ref.RefCount = 1);
  AssertTrue(FCounter = 0);
  AssertTrue(TTestClass(Ref).Value = 5);

  CallByValue2(Ref);

  AssertTrue(FCounter = 0);
  AssertTrue(Ref.RefCount = 2);
  AssertTrue(TTestClass(Ref).Value = 5);

  FTestRef.Instance := nil;
  AssertTrue(Ref.RefCount = 1);

  CallByValue3(Ref, 15);

  AssertTrue(Ref.RefCount = 1);
  AssertTrue(FCounter = 0);
  AssertTrue(TTestClass(Ref).Value = 5);
end;

procedure TSharedRefTest.PassAsVar;
var
  Ref: specialize TGSharedRef<TTestClass>;
begin
  FCounter := 0;
  Ref.Instance := TTestClass.Create(@IncCounter);

  CallAsVar(Ref, 111);

  AssertTrue(FCounter = 1);
  AssertTrue(TTestClass(Ref).Value = 111);
end;

procedure TSharedRefTest.PassAsOut;
var
  Ref: specialize TGSharedRef<TTestClass>;
begin
  FCounter := 0;
  Ref.Instance := TTestClass.Create(@IncCounter);

  CallAsOut(Ref, 111);

  AssertTrue(FCounter = 1);
  AssertTrue(Ref.RefCount = 1);
  AssertTrue(TTestClass(Ref).Value = 111);
end;

procedure TSharedRefTest.PassAsConst;
var
  Ref: specialize TGSharedRef<TTestClass>;
begin
  FCounter := 0;
  Ref.Instance := TTestClass.Create(@IncCounter);

  CallAsConst(Ref);

  AssertTrue(FCounter = 0);
end;

procedure TSharedRefTest.PassAsConstref;
var
  Ref: specialize TGSharedRef<TTestClass>;
begin
  FCounter := 0;
  Ref.Instance := TTestClass.Create(@IncCounter);

  CallAsConstref(Ref);

  AssertTrue(FCounter = 0);
end;

procedure TSharedRefTest.HasInstance;
var
  Ref: specialize TGSharedRef<TStringList>;
begin
  AssertFalse({%H-}Ref.HasInstance);

  Ref.Instance := TStringList.Create;
  AssertTrue(Ref.HasInstance);
  AssertTrue(TStringList(Ref).Count = 0);

  Ref.Instance := nil;
  AssertFalse(Ref.HasInstance);
end;

procedure TSharedRefTest.Release;
var
  Ref, Ref2: specialize TGSharedRef<TTestClass>;
begin
  FCounter := 0;
  AssertTrue({%H-}Ref.RefCount = 0);
  Ref.Release;
  AssertTrue({%H-}Ref.RefCount = 0);

  Ref.Instance := TTestClass.Create(@IncCounter);
  AssertTrue(Ref.RefCount = 1);
  AssertTrue({%H-}Ref2.RefCount = 0);

  Ref2 := Ref;
  AssertTrue(Ref.RefCount = 2);
  AssertTrue(Ref2.RefCount = 2);

  FTestRef := Ref2;
  AssertTrue(Ref.RefCount = 3);
  AssertTrue(Ref2.RefCount = 3);
  AssertTrue(FTestRef.RefCount = 3);

  FTestRef.Release;
  AssertTrue(Ref.RefCount = 2);
  AssertTrue(Ref2.RefCount = 2);
  AssertTrue(FTestRef.RefCount = 0);
  AssertTrue(FCounter = 0);

  Ref2.Release;
  AssertTrue(Ref.RefCount = 1);
  AssertTrue(Ref2.RefCount = 0);
  AssertTrue(FCounter = 0);

  Ref.Release;
  AssertTrue(Ref.RefCount = 0);
  AssertTrue(FCounter = 1);
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

{ TCowPtrTest }

procedure TCowPtrTest.CallByValue(aPtr: TTestPtr; aColor: TTestColor; aCount: Integer);
var
  p: TTestPtr.PValue;
begin
  AssertTrue(aPtr.RefCount > 1);
  p := aPtr.UniqPtr;
  p^.Name := '';
  p^.Color := aColor;
  p^.Count := aCount;
end;

procedure TCowPtrTest.CallByValue2(aSrc: TTestPtr; aCount: Integer; out aDst: TTestPtr);
begin
  AssertTrue(aSrc.RefCount > 1);
  AssertTrue(aDst.RefCount = 0);
  aSrc.UniqPtr^.Count := aCount;
  aDst := aSrc;
end;

procedure TCowPtrTest.CallAsVar(var aPtr: TTestPtr; aColor: TTestColor; aCount: Integer);
var
  p: TTestPtr.PValue;
begin
  AssertTrue(aPtr.RefCount = 1);
  p := aPtr.UniqPtr;
  p^.Name := '';
  p^.Color := aColor;
  p^.Count := aCount;
end;

procedure TCowPtrTest.Allocated;
var
  v: TTestPtr;
  p: TTestPtr.PValue = nil;
begin
  AssertFalse({%H-}v.Allocated);
  p := v.Ptr;
  AssertTrue(v.Allocated);
  AssertTrue(p <> nil);
end;

procedure TCowPtrTest.Implicit;
var
  v: TTestPtr;
  Rec: TTestRec;
  procedure Test(const aRec: TTestRec);
  begin
    AssertTrue(aRec.Name = v.Ptr^.Name);
    AssertTrue(aRec.Color = v.Ptr^.Color);
    AssertTrue(aRec.Count = v.Ptr^.Count);
  end;
begin
  Rec := CONST_REC;
  AssertTrue(Rec.Name = CONST_REC.Name);
  AssertTrue(Rec.Color = CONST_REC.Color);
  AssertTrue(Rec.Count = CONST_REC.Count);
  Rec := {%H-}v;
  AssertTrue(v.Allocated);
  AssertTrue(Rec.Name = '');
  AssertTrue(Rec.Color = tcBlack);
  AssertTrue(Rec.Count = 0);
  v.Value := CONST_REC;
  Test(v);
end;

procedure TCowPtrTest.CopyRefCount;
var
  v, v2: TTestPtr;
begin
  AssertTrue({%H-}v.RefCount = 0);
  v2 := v;
  AssertTrue(v2.RefCount = 0);
  AssertFalse(v2.Allocated);

  v.Value := CONST_REC;
  AssertTrue(v.Allocated);
  AssertTrue(v.RefCount = 1);

  v2 := v;
  AssertTrue(v.RefCount = 2);
  AssertTrue(v2.RefCount = 2);

  FTestPtr := v2;
  AssertTrue(v.RefCount = 3);
  AssertTrue(v2.RefCount = 3);
  AssertTrue(FTestPtr.RefCount = 3);
end;

procedure TCowPtrTest.ReadPtr;
var
  v, v2: TTestPtr;
begin
  {%H-}v.Value := CONST_REC;
  v2 := v;
  AssertTrue(v.RefCount = 2);
  AssertTrue(v2.RefCount = 2);
  v.Ptr^.Name := 'New name';
  AssertTrue(v.RefCount = 2);
  AssertTrue(v2.RefCount = 2);
  AssertTrue(v2.Ptr^.Name = 'New name');
end;

procedure TCowPtrTest.UniqPtr;
var
  v, v2: TTestPtr;
begin
  v.UniqPtr^ := CONST_REC;
  v2 := v;
  AssertTrue(v.RefCount = 2);
  AssertTrue(v2.RefCount = 2);
  v.UniqPtr^.Name := 'New name';
  AssertTrue(v.RefCount = 1);
  AssertTrue(v2.RefCount = 1);
  AssertTrue(v.Ptr^.Color = CONST_REC.Color);
  AssertTrue(v.Ptr^.Count = CONST_REC.Count);
  AssertTrue(v2.Ptr^.Name = CONST_REC.Name);
end;

procedure TCowPtrTest.PassByValue;
var
  v, v2: TTestPtr;
begin
  {%H-}v.Value := CONST_REC;

  CallByValue(v, tcBlue, 3);

  AssertTrue(v.RefCount = 1);
  AssertTrue(v.Ptr^.Name = CONST_REC.Name);
  AssertTrue(v.Ptr^.Color = CONST_REC.Color);
  AssertTrue(v.Ptr^.Count = CONST_REC.Count);

  CallByValue2(v, 3, v2);

  AssertTrue(v.RefCount = 1);
  AssertTrue(v2.RefCount = 1);
  AssertTrue(v.Ptr^.Name = CONST_REC.Name);
  AssertTrue(v.Ptr^.Color = CONST_REC.Color);
  AssertTrue(v.Ptr^.Count = CONST_REC.Count);
  AssertTrue(v2.Ptr^.Name = CONST_REC.Name);
  AssertTrue(v2.Ptr^.Color = CONST_REC.Color);
  AssertTrue(v2.Ptr^.Count = 3);
end;

procedure TCowPtrTest.PassAsVar;
var
  v: TTestPtr;
begin
  v.UniqPtr^ := CONST_REC;

  CallAsVar(v, tcRed, 7);
  AssertTrue(v.RefCount = 1);
  AssertTrue(v.Ptr^.Name = '');
  AssertTrue(v.Ptr^.Color = tcRed);
  AssertTrue(v.Ptr^.Count = 7);
end;

procedure TCowPtrTest.Release;
var
  v, v2: TTestPtr;
begin
  AssertTrue({%H-}v.RefCount = 0);
  v.Release;
  AssertTrue({%H-}v.RefCount = 0);

  v.UniqPtr^ := CONST_REC;
  AssertTrue(v.RefCount = 1);
  AssertTrue({%H-}v2.RefCount = 0);

  v2 := v;
  AssertTrue(v.RefCount = 2);
  AssertTrue(v2.RefCount = 2);

  FTestPtr := v2;
  AssertTrue(v.RefCount = 3);
  AssertTrue(v2.RefCount = 3);
  AssertTrue(FTestPtr.RefCount = 3);

  FTestPtr.Release;
  AssertTrue(v.RefCount = 2);
  AssertTrue(v2.RefCount = 2);
  AssertTrue(FTestPtr.RefCount = 0);

  v2.Release;
  AssertTrue(v.RefCount = 1);
  AssertTrue(v2.RefCount = 0);

  v.Release;
  AssertTrue(v.RefCount = 0);
end;

procedure TCowPtrTest.Unique;
var
  v, v2: TTestPtr;
begin
  {%H-}v.Value := CONST_REC;
  AssertTrue(v.RefCount = 1);
  AssertTrue({%H-}v2.RefCount = 0);

  v2 := v;
  AssertTrue(v.RefCount = 2);
  AssertTrue(v2.RefCount = 2);

  FTestPtr := v2;
  AssertTrue(v.RefCount = 3);
  AssertTrue(v2.RefCount = 3);
  AssertTrue(FTestPtr.RefCount = 3);

  v.Unique;
  AssertTrue(v.RefCount = 1);
  AssertTrue(v2.RefCount = 2);
  AssertTrue(FTestPtr.RefCount = 2);

  v.Ptr^.Name := 'New name';
  v2.Unique;
  AssertTrue(v.RefCount = 1);
  AssertTrue(v2.RefCount = 1);
  AssertTrue(FTestPtr.RefCount = 1);

  v2.Ptr^.Color := tcRed;
  FTestPtr.Ptr^.Count := 7;

  AssertTrue(v.Ptr^.Name = 'New name');
  AssertTrue(v.Ptr^.Color = CONST_REC.Color);
  AssertTrue(v.Ptr^.Count = CONST_REC.Count);

  AssertTrue(v2.Ptr^.Name = CONST_REC.Name);
  AssertTrue(v2.Ptr^.Color = tcRed);
  AssertTrue(v2.Ptr^.Count = CONST_REC.Count);

  AssertTrue(FTestPtr.Ptr^.Name = CONST_REC.Name);
  AssertTrue(FTestPtr.Ptr^.Color = CONST_REC.Color);
  AssertTrue(FTestPtr.Ptr^.Count = 7);
end;

procedure TCowPtrTest.Value;
var
  v, v2: TTestPtr;
  Rec: TTestRec;
begin
  Rec := CONST_REC;
  {%H-}v.Value := Rec;
  AssertTrue(v.Ptr^.Name = CONST_REC.Name);
  AssertTrue(v.Ptr^.Color = CONST_REC.Color);
  AssertTrue(v.Ptr^.Count = CONST_REC.Count);

  v2 := v;
  Rec := v.Value;
  AssertTrue(Rec.Name = CONST_REC.Name);
  AssertTrue(Rec.Color = CONST_REC.Color);
  AssertTrue(Rec.Count = CONST_REC.Count);

  Rec.Name := 'New name';
  Rec.Color := tcGreen;
  Rec.Count := 5;
  v2.Value := Rec;
  AssertTrue(v.Ptr^.Name = CONST_REC.Name);
  AssertTrue(v.Ptr^.Color = CONST_REC.Color);
  AssertTrue(v.Ptr^.Count = CONST_REC.Count);
  AssertTrue(v2.Ptr^.Name = 'New name');
  AssertTrue(v2.Ptr^.Color = tcGreen);
  AssertTrue(v2.Ptr^.Count = 5);
end;

initialization

  RegisterTest(TCommonFunctionTest);
  RegisterTest(TAutoRefTest);
  RegisterTest(TUniqRefTest);
  RegisterTest(TSharedRefATest);
  RegisterTest(TSharedRefTest);
  RegisterTest(TCowPtrTest);
  RegisterTest(TGOptionalTest);

end.

