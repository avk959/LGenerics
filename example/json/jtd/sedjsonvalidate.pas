unit sedJsonValidate;

{$MODE OBJFPC}{$H+}{$MODESWITCH ADVANCEDRECORDS}

interface

uses
  SysUtils;

type

  TValidateResult = record
    IsValid: Boolean;
    CaretX,
    CaretY: Integer;
    Message: string;
    class function Valid: TValidateResult; static;
    constructor Make(const aMessage: string);
    constructor Make(const aMessage: string; X, Y: Integer);
  end;

  function ValidateJson(const s: string; aSkipBom: Boolean = False; aMaxDepth: Integer = 511): TValidateResult;

implementation
{$B-}{$J-}{$COPERATORS ON}{$POINTERMATH ON}{$MACRO ON}{$WARN 2005 OFF}

uses
  lgUtils, sedStrConsts;

type
  TParseMode = (pmNone, pmKey, pmArray, pmObject);
  PParseMode = ^TParseMode;
  TOpenArray = record
    Data: Pointer;
    Size: Integer;
    constructor Make(aData: Pointer; aSize: Integer);
  end;

const
  Space  = Integer( 0); //  space
  White  = Integer( 1); //  other whitespace
  LCurBr = Integer( 2); //  {
  RCurBr = Integer( 3); //  }
  LSqrBr = Integer( 4); //  [
  RSqrBr = Integer( 5); //  ]
  Colon  = Integer( 6); //  :
  Comma  = Integer( 7); //  ,
  Quote  = Integer( 8); //  "
  BSlash = Integer( 9); //  \
  Slash  = Integer(10); //  /
  Plus   = Integer(11); //  +
  Minus  = Integer(12); //  -
  Point  = Integer(13); //  .
  Zero   = Integer(14); //  0
  Digit  = Integer(15); //  123456789
  LowerA = Integer(16); //  a
  LowerB = Integer(17); //  b
  LowerC = Integer(18); //  c
  LowerD = Integer(19); //  d
  LowerE = Integer(20); //  e
  LowerF = Integer(21); //  f
  LowerL = Integer(22); //  l
  LowerN = Integer(23); //  n
  LowerR = Integer(24); //  r
  LowerS = Integer(25); //  s
  LowerT = Integer(26); //  t
  LowerU = Integer(27); //  u
  ABCDF  = Integer(28); //  ABCDF
  UpperE = Integer(29); //  E
  Etc    = Integer(30); //  everything else

  SymClassTable: array[0..127] of Integer = (
    -1,    -1,     -1,     -1,     -1,     -1,     -1,     -1,
    -1,    White,  White,  -1,     -1,     White,  -1,     -1,
    -1,    -1,     -1,     -1,     -1,     -1,     -1,     -1,
    -1,    -1,     -1,     -1,     -1,     -1,     -1,     -1,

    Space, Etc,    Quote,  Etc,    Etc,    Etc,    Etc,    Etc,
    Etc,   Etc,    Etc,    Plus,   Comma,  Minus,  Point,  Slash,
    Zero,  Digit,  Digit,  Digit,  Digit,  Digit,  Digit,  Digit,
    Digit, Digit,  Colon,  Etc,    Etc,    Etc,    Etc,    Etc,

    Etc,   ABCDF,  ABCDF,  ABCDF,  ABCDF,  UpperE, ABCDF,  Etc,
    Etc,   Etc,    Etc,    Etc,    Etc,    Etc,    Etc,    Etc,
    Etc,   Etc,    Etc,    Etc,    Etc,    Etc,    Etc,    Etc,
    Etc,   Etc,    Etc,    LSqrBr, BSlash, RSqrBr, Etc,    Etc,

    Etc,   LowerA, LowerB, LowerC, LowerD, LowerE, LowerF, Etc,
    Etc,   Etc,    Etc,    Etc,    LowerL, Etc,    LowerN, Etc,
    Etc,   Etc,    LowerR, LowerS, LowerT, LowerU, Etc,    Etc,
    Etc,   Etc,    Etc,    LCurBr, Etc,    RCurBr, Etc,    Etc
  );

  __ = Integer(-1);// error
  GO = Integer( 0);// start
  OK = Integer( 1);// ok
  OB = Integer( 2);// object
  KE = Integer( 3);// key
  CO = Integer( 4);// colon
  VA = Integer( 5);// value
  AR = Integer( 6);// array
  ST = Integer( 7);// string
  ES = Integer( 8);// escape
  U1 = Integer( 9);// u1
  U2 = Integer(10);// u2
  U3 = Integer(11);// u3
  U4 = Integer(12);// u4
  MI = Integer(13);// minus
  ZE = Integer(14);// zero
  IR = Integer(15);// integer
  FR = Integer(16);// fraction
  FS = Integer(17);// fraction
  E1 = Integer(18);// e
  E2 = Integer(19);// ex
  E3 = Integer(20);// exp
  T1 = Integer(21);// tr
  T2 = Integer(22);// tru
  T3 = Integer(23);// true
  F1 = Integer(24);// fa
  F2 = Integer(25);// fal
  F3 = Integer(26);// fals
  F4 = Integer(27);// false
  N1 = Integer(28);// nu
  N2 = Integer(29);// nul
  N3 = Integer(30);// null

  StateTransitions: array[GO..N3, Space..Etc] of Integer = (
{
  The state transition table takes the current state and the current symbol,
  and returns either a new state or an action. An action is represented as a
  negative number. A JSON text is accepted if at the end of the text the
  state is OK and if the mode is pmNone.

             white                                      1-9                                   ABCDF  etc
         space |  {  }  [  ]  :  ,  "  \  /  +  -  .  0  |  a  b  c  d  e  f  l  n  r  s  t  u  |  E  | }
{start  GO}(GO,GO,-6,__,-5,__,__,__,ST,__,__,__,MI,__,ZE,IR,__,__,__,__,__,F1,__,N1,__,__,T1,__,__,__,__),
{ok     OK}(OK,OK,__,-8,__,-7,__,-3,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__),
{object OB}(OB,OB,__,-9,__,__,__,__,ST,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__),
{key    KE}(KE,KE,__,__,__,__,__,__,ST,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__),
{colon  CO}(CO,CO,__,__,__,__,-2,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__),
{value  VA}(VA,VA,-6,__,-5,__,__,__,ST,__,__,__,MI,__,ZE,IR,__,__,__,__,__,F1,__,N1,__,__,T1,__,__,__,__),
{array  AR}(AR,AR,-6,__,-5,-7,__,__,ST,__,__,__,MI,__,ZE,IR,__,__,__,__,__,F1,__,N1,__,__,T1,__,__,__,__),
{string ST}(ST,__,ST,ST,ST,ST,ST,ST,-4,ES,ST,ST,ST,ST,ST,ST,ST,ST,ST,ST,ST,ST,ST,ST,ST,ST,ST,ST,ST,ST,ST),
{escape ES}(__,__,__,__,__,__,__,__,ST,ST,ST,__,__,__,__,__,__,ST,__,__,__,ST,__,ST,ST,__,ST,U1,__,__,__),
{u1     U1}(__,__,__,__,__,__,__,__,__,__,__,__,__,__,U2,U2,U2,U2,U2,U2,U2,U2,__,__,__,__,__,__,U2,U2,__),
{u2     U2}(__,__,__,__,__,__,__,__,__,__,__,__,__,__,U3,U3,U3,U3,U3,U3,U3,U3,__,__,__,__,__,__,U3,U3,__),
{u3     U3}(__,__,__,__,__,__,__,__,__,__,__,__,__,__,U4,U4,U4,U4,U4,U4,U4,U4,__,__,__,__,__,__,U4,U4,__),
{u4     U4}(__,__,__,__,__,__,__,__,__,__,__,__,__,__,ST,ST,ST,ST,ST,ST,ST,ST,__,__,__,__,__,__,ST,ST,__),
{minus  MI}(__,__,__,__,__,__,__,__,__,__,__,__,__,__,ZE,IR,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__),
{zero   ZE}(OK,OK,__,-8,__,-7,__,-3,__,__,__,__,__,FR,__,__,__,__,__,__,E1,__,__,__,__,__,__,__,__,E1,__),
{int    IR}(OK,OK,__,-8,__,-7,__,-3,__,__,__,__,__,FR,IR,IR,__,__,__,__,E1,__,__,__,__,__,__,__,__,E1,__),
{frac   FR}(__,__,__,__,__,__,__,__,__,__,__,__,__,__,FS,FS,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__),
{fracs  FS}(OK,OK,__,-8,__,-7,__,-3,__,__,__,__,__,__,FS,FS,__,__,__,__,E1,__,__,__,__,__,__,__,__,E1,__),
{e      E1}(__,__,__,__,__,__,__,__,__,__,__,E2,E2,__,E3,E3,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__),
{ex     E2}(__,__,__,__,__,__,__,__,__,__,__,__,__,__,E3,E3,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__),
{exp    E3}(OK,OK,__,-8,__,-7,__,-3,__,__,__,__,__,__,E3,E3,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__),
{t      T1}(__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,T2,__,__,__,__,__,__),
{tr     T2}(__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,T3,__,__,__),
{tru    T3}(__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,OK,__,__,__,__,__,__,__,__,__,__),
{f      F1}(__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,F2,__,__,__,__,__,__,__,__,__,__,__,__,__,__),
{fa     F2}(__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,F3,__,__,__,__,__,__,__,__),
{fal    F3}(__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,F4,__,__,__,__,__),
{fals   F4}(__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,OK,__,__,__,__,__,__,__,__,__,__),
{n      N1}(__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,N2,__,__,__),
{nu     N2}(__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,N3,__,__,__,__,__,__,__,__),
{nul    N3}(__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,OK,__,__,__,__,__,__,__,__)
  );

{$DEFINE ErrorMacro := begin Success := False; break end}
function ValidateBuf(aBuf: PByte; aSize: SizeInt; const aStack: TOpenArray): TValidateResult;
var
  Stack: PParseMode;
  I: SizeInt;
  NextState, NextClass, StackHigh, Row, Col: Integer;
  State: Integer = GO;
  sTop: Integer = 0;
  Curr: Byte;
  Success: Boolean = True;
  Msg: string;
begin
  Stack := aStack.Data;
  StackHigh := Pred(aStack.Size);
  Stack[0] := pmNone;
  Row := 1;
  Col := 1;
  I := 0;
  while I < aSize do
    begin
      Curr := aBuf[I];
      if Curr < 128 then begin
        NextClass := SymClassTable[Curr];
        if NextClass = __ then
          exit(TValidateResult.Make(Format(SEIllegalSymbolFmt, [Curr]), Col, Row));
      end else
        NextClass := Etc;
      NextState := StateTransitions[State, NextClass];
      if NextState >= 0 then
        State := NextState
      else
        case NextState of
          -9:
            begin
              if Stack[sTop] <> pmKey then ErrorMacro;
              Dec(sTop);
              State := OK;
            end;
          -8:
            begin
              if Stack[sTop] <> pmObject then ErrorMacro;
              Dec(sTop);
              State := OK;
            end;
          -7:
            begin
              if Stack[sTop] <> pmArray then ErrorMacro;
              Dec(sTop);
              State := OK;
            end;
          -6:
            begin
              Inc(sTop);
              if sTop > StackHigh then ErrorMacro;
              Stack[sTop] := pmKey;
              State := OB;
            end;
          -5:
            begin
              Inc(sTop);
              if sTop > StackHigh then ErrorMacro;
              Stack[sTop] := pmArray;
              State := AR;
            end;
          -4:
            case Stack[sTop] of
              pmKey:                     State := CO;
              pmNone, pmArray, pmObject: State := OK;
            end;
          -3:
            case Stack[sTop] of
              pmObject:
                begin
                  Stack[sTop] := pmKey;
                  State := KE;
                end;
              pmArray: State := VA;
            else
              ErrorMacro;
            end;
          -2:
            begin
              if Stack[sTop] <> pmKey then ErrorMacro;
              Stack[sTop] := pmObject;
              State := VA;
            end;
        else
          ErrorMacro;
        end;
      case Curr of
        10:
          if aBuf[I-1] <> 13 then begin
            Inc(Row); Col := 1;
          end;
        13:
          begin
            Inc(Row); Col := 1;
          end;
      else
        Inc(Col);
      end;
      Inc(I);
    end;
  if Success then
    Success := ((State = OK) or (State in [ZE, IR, FS, E3])) and (sTop = 0) and (Stack[0] = pmNone);
  if Success then
    exit(TValidateResult.Valid);

  if sTop > StackHigh then
    exit(TValidateResult.Make(SEInternStackOverflow, Col, Row));

  if State = Go then
    Msg := SEUnexpectEof
  else
    Msg := Format(SEUnexpectSymbolFmt, [Row, Col, Char(aBuf[I]), aBuf[I]]);
  Result := TValidateResult.Make(Msg, Col, Row);
end;

function ValidateJson(const s: string; aSkipBom: Boolean; aMaxDepth: Integer): TValidateResult;
var
  Stack: array[0..511] of TParseMode;
  DynStack: array of TParseMode = nil;
  Buf: PByte;
  Size: SizeInt;
  bk: TBomKind;
begin
  if aMaxDepth < 1 then exit(TValidateResult.Make(SEInternStackOverflow));
  Buf := Pointer(s);
  Size := System.Length(s);
  if Size < 1 then exit(TValidateResult.Make('Empty source'));
  if aSkipBom then
    begin
      bk := DetectBom(Buf, Size);
      case bk of
        bkNone: ;
        bkUtf8:
          begin
            Buf += UTF8_BOM_LEN;
            Size -= UTF8_BOM_LEN;
          end;
      else
        exit(TValidateResult.Make(Format(SEUnexpectBomFmt, [ENCODING_NAMES[bk]])));
      end;
    end;
  if aMaxDepth <= Length(Stack) then
    Result := ValidateBuf(Buf, Size, TOpenArray.Make(@Stack[0], aMaxDepth + 1))
  else
    begin
      System.SetLength(DynStack, aMaxDepth + 1);
      Result := ValidateBuf(Buf, Size, TOpenArray.Make(Pointer(DynStack), aMaxDepth + 1));
    end;
end;

{ TOpenArray }

constructor TOpenArray.Make(aData: Pointer; aSize: Integer);
begin
  Data := aData;
  Size := aSize;
end;

{ TValidateResult }

class function TValidateResult.Valid: TValidateResult;
begin
  Result.IsValid := True;
  Result.CaretX := -1;
  Result.CaretY := -1;
  Result.Message := '';
end;

constructor TValidateResult.Make(const aMessage: string);
begin
  IsValid := False;
  CaretX := -1;
  CaretY := -1;
  Message := aMessage;
end;

constructor TValidateResult.Make(const aMessage: string; X, Y: Integer);
begin
  IsValid := False;
  CaretX := X;
  CaretY := Y;
  Message := aMessage;
end;

end.

