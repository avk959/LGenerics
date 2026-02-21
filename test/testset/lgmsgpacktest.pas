unit LGMsgPackTest;

{$MODE OBJFPC}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, LgUtils, LgMsgPack;

type

  { TTestMpWriter }

  TTestMpWriter = class(TTestCase)
  private
  type
    TWriter = specialize TGAutoRef<TMpWriter>;
  published
    procedure TestWriteNil;
    procedure TestWriteBool;
    procedure TestWriteInt;
    procedure TestWriteStr;
    procedure TestWriteBin;
    procedure TestWriteTs;
    procedure TestWriteExt;
    procedure TestWriteArray;
    procedure TestWriteMap;
  end;

  { TTestMpReader }

  TTestMpReader = class(TTestCase)
  private
  type
    TReader = specialize TGUniqRef<TMpReader>;
  published
    procedure TestReadNil;
    procedure TestReadBool;
    procedure TestReadInt;
    procedure TestReadStr;
    procedure TestReadBin;
    procedure TestReadTs;
    procedure TestReadExt;
    procedure TestReadArray;
    procedure TestReadMap;
  end;

  { TTestMpDomNode }

  TTestMpDomNode = class(TTestCase)
  private
  type
    TMpNode = specialize TGAutoRef<TMpDomNode>;
  published
    procedure TestMake;
  end;


  function FillBytes(aValue: Byte; aCount: Integer): TBytes;
  function SameBytes(const L, R: TBytes): Boolean;

type
  TMpNumTest = record
    Num: Int64;
    Blob: TBytes;
  end;

  TMpExtTest = record
    ExtType: TMpExtType;
    Data,
    Blob: TBytes;
  end;

const
  NUM_TESTS: array of TMpNumTest = (
    (Num:                0; Blob: ($00)),
    (Num:              127; Blob: ($7f)),
    (Num:              128; Blob: ($cc,$80)),
    (Num:              255; Blob: ($cc,$ff)),
    (Num:              256; Blob: ($cd,$01,$00)),
    (Num:            65535; Blob: ($cd,$ff,$ff)),
    (Num:            65536; Blob: ($ce,$00,$01,$00,$00)),
    (Num:       2147483647; Blob: ($ce,$7f,$ff,$ff,$ff)),
    (Num:       2147483648; Blob: ($ce,$80,$00,$00,$00)),
    (Num:       4294967295; Blob: ($ce,$ff,$ff,$ff,$ff)),
    (Num:       4294967296; Blob: ($cf,$00,$00,$00,$01,$00,$00,$00,$00)),
    (Num:  281474976710656; Blob: ($cf,$00,$01,$00,$00,$00,$00,$00,$00)),
    (Num:               -1; Blob: ($ff)),
    (Num:              -32; Blob: ($e0)),
    (Num:              -33; Blob: ($d0,$df)),
    (Num:             -128; Blob: ($d0,$80)),
    (Num:             -129; Blob: ($d1,$ff,$7f)),
    (Num:             -256; Blob: ($d1,$ff,$00)),
    (Num:           -32768; Blob: ($d1,$80,$00)),
    (Num:           -32769; Blob: ($d2,$ff,$ff,$7f,$ff)),
    (Num:           -65536; Blob: ($d2,$ff,$ff,$00,$00)),
    (Num:      -2147483648; Blob: ($d2,$80,$00,$00,$00)),
    (Num:      -2147483649; Blob: ($d3,$ff,$ff,$ff,$ff,$7f,$ff,$ff,$ff)),
    (Num:      -4294967296; Blob: ($d3,$ff,$ff,$ff,$ff,$00,$00,$00,$00)),
    (Num: -281474976710656; Blob: ($d3,$ff,$ff,$00,$00,$00,$00,$00,$00))
  );

  EXT_TESTS: array of TMpExtTest = (
    (ExtType: 1; Data: ($10); Blob: ($d4,$01,$10)),
    (ExtType: 2; Data: ($20,$21); Blob: ($d5,$02,$20,$21)),
    (ExtType: 3; Data: ($30,$31,$32,$33); Blob: ($d6,$03,$30,$31,$32,$33)),
    (ExtType: 4; Data: ($40,$41,$42,$43,$44,$45,$46,$47); Blob: ($d7,$04,$40,$41,$42,$43,$44,$45,$46,$47)),
    (ExtType: 5; Data: ($50,$51,$52,$53,$54,$55,$56,$57,$58,$59,$5a,$5b,$5c,$5d,$5e,$5f); Blob: ($d8,$05,$50,$51,$52,$53,$54,$55,$56,$57,$58,$59,$5a,$5b,$5c,$5d,$5e,$5f)),
    (ExtType: 6; Data: nil; Blob: ($c7,$00,$06)),
    (ExtType: 7; Data: ($70,$71,$72); Blob: ($c7,$03,$07,$70,$71,$72)),
    (ExtType: 8; Data: ($80,$81,$82,$83,$85,$86,$87); Blob: ($c7,$07,$08,$80,$81,$82,$83,$85,$86,$87))
  );

implementation
{$WARN 5089 OFF : Local variable "$1" of a managed type does not seem to be initialized}

function FillBytes(aValue: Byte; aCount: Integer): TBytes;
begin
  SetLength(Result, aCount);
  FillByte(Pointer(Result)^, aCount, aValue);
end;

function SameBytes(const L, R: TBytes): Boolean;
begin
  if Pointer(L) = Pointer(R) then exit(True);
  if Length(L) <> Length(R) then exit(False);
  Result := CompareByte(L[0], R[0], Length(L)) = 0;
end;

procedure TTestMpWriter.TestWriteNil;
var
  Writer: TWriter;
const
  Ans: TBytes = ($c0);
begin
  Writer.Instance.AddNil;
  AssertTrue(SameBytes(Writer.Instance.ToBytes, Ans));
end;

procedure TTestMpWriter.TestWriteBool;
var
  Writer: TWriter;
const
  AnsFalse: TBytes = ($c2);
  AnsTrue: TBytes  = ($c3);
begin
  Writer.Instance.Add(False);
  AssertTrue(SameBytes(Writer.Instance.ToBytes, AnsFalse));
  Writer.Instance.Reset;
  Writer.Instance.Add(True);
  AssertTrue(SameBytes(Writer.Instance.ToBytes, AnsTrue));
end;

procedure TTestMpWriter.TestWriteInt;
var
  Writer: TWriter;
  I: Integer;
begin
  for I := 0 to High(NUM_TESTS) do begin
    Writer.Instance.Reset;
    Writer.Instance.Add(NUM_TESTS[I].Num);
    AssertTrue(I.ToString, SameBytes(Writer.Instance.ToBytes, NUM_TESTS[I].Blob));
  end;
end;

procedure TTestMpWriter.TestWriteStr;
var
  Writer: TWriter;
const
  s0 = '';
  s1 = 'a';
  s2 = 'bbbbbbbb';
  s3 = 'ccccccccccccccccccccccccccccccc';
  s4 = 'cccccccccccccccccccccccccccccccd';
  s5 = 'почему небо синее?';

  Ans0: TBytes = ($a0);
  Ans1: TBytes = ($a1,$61);
  Ans2: TBytes = ($a8,$62,$62,$62,$62,$62,$62,$62,$62);
  Ans3: TBytes = ($bf,$63,$63,$63,$63,$63,$63,$63,$63,$63,$63,$63,$63,$63,$63,$63,$63,$63,$63,$63,$63,$63,$63,$63,$63,$63,$63,$63,$63,$63,$63,$63);
  Ans4: TBytes = ($d9,$20,$63,$63,$63,$63,$63,$63,$63,$63,$63,$63,$63,$63,$63,$63,$63,$63,$63,$63,$63,$63,$63,$63,$63,$63,$63,$63,$63,$63,$63,$63,$63,$64);
  Ans5: TBytes = ($d9,$21,$d0,$bf,$d0,$be,$d1,$87,$d0,$b5,$d0,$bc,$d1,$83,$20,$d0,$bd,$d0,$b5,$d0,$b1,$d0,$be,$20,$d1,$81,$d0,$b8,$d0,$bd,$d0,$b5,$d0,$b5,$3f);
begin
  Writer.Instance.Add(s0);
  AssertTrue('0', SameBytes(Writer.Instance.ToBytes, Ans0));

  Writer.Instance.Reset;
  Writer.Instance.Add(s1);
  AssertTrue('1', SameBytes(Writer.Instance.ToBytes, Ans1));

  Writer.Instance.Reset;
  Writer.Instance.Add(s2);
  AssertTrue('2', SameBytes(Writer.Instance.ToBytes, Ans2));

  Writer.Instance.Reset;
  Writer.Instance.Add(s3);
  AssertTrue('3', SameBytes(Writer.Instance.ToBytes, Ans3));

  Writer.Instance.Reset;
  Writer.Instance.Add(s4);
  AssertTrue('4', SameBytes(Writer.Instance.ToBytes, Ans4));

  Writer.Instance.Reset;
  Writer.Instance.Add(s5);
  AssertTrue('5', SameBytes(Writer.Instance.ToBytes, Ans5));
end;

procedure TTestMpWriter.TestWriteBin;
var
  Writer: TWriter;
  ans: TBytes;
const
  b0: TBytes = nil;
  b1: TBytes = ($01);
  b2: TBytes = ($00,$00);
  b3: TBytes = ($00,$00,$00,$00,$00,$00,$00);

  Ans0: TBytes = ($c4,$00);
  Ans1: TBytes = ($c4,$01,$01);
  Ans2: TBytes = ($c4,$02,$00,$00);
  Ans3: TBytes = ($c4,$07,$00,$00,$00,$00,$00,$00,$00);
begin
  Writer.Instance.Add(b0);
  AssertTrue('0', SameBytes(Writer.Instance.ToBytes, Ans0));

  Writer.Instance.Reset;
  Writer.Instance.Add(b1);
  AssertTrue('1', SameBytes(Writer.Instance.ToBytes, Ans1));

  Writer.Instance.Reset;
  Writer.Instance.Add(b2);
  AssertTrue('2', SameBytes(Writer.Instance.ToBytes, Ans2));

  Writer.Instance.Reset;
  Writer.Instance.Add(b3);
  AssertTrue('3', SameBytes(Writer.Instance.ToBytes, Ans3));

  Writer.Instance.Reset;
  Writer.Instance.Add(FillBytes(42, 256));
  ans := FillBytes(42, 256);
  Insert([$c5, $01, $00], ans, 0);
  AssertTrue('4', SameBytes(Writer.Instance.ToBytes, ans));

  Writer.Instance.Reset;
  Writer.Instance.Add(FillBytes(101, 65536));
  ans := FillBytes(101, 65536);
  Insert([$c6, $00, $01, $00, $00], ans, 0);
  AssertTrue('5', SameBytes(Writer.Instance.ToBytes, ans));
end;

procedure TTestMpWriter.TestWriteTs;
var
  Writer: TWriter;
  ts: TMpTimeStamp;
begin
  ts := TMpTimeStamp.Make($ffff0000, 0);
  Writer.Instance.Add(ts);
  AssertTrue('0', SameBytes(Writer.Instance.ToBytes, [$d6,$ff,$ff,$ff,$00,$00]));

  ts := TMpTimeStamp.Make($3ffff0000, $ffff);
  Writer.Instance.Reset;
  Writer.Instance.Add(ts);
  AssertTrue('1', SameBytes(Writer.Instance.ToBytes, [$d7,$ff,$00,$03,$ff,$ff,$ff,$ff,$00,$00]));

  ts := TMpTimeStamp.Make($7ffff0000, 0);
  Writer.Instance.Reset;
  Writer.Instance.Add(ts);
  AssertTrue('2', SameBytes(Writer.Instance.ToBytes, [$c7,$0c,$ff,$00,$00,$00,$00,$00,$00,$00,$07,$ff,$ff,$00,$00]));

  ts := TMpTimeStamp.Make($7ffff0000, $ffff);
  Writer.Instance.Reset;
  Writer.Instance.Add(ts);
  AssertTrue('3', SameBytes(Writer.Instance.ToBytes, [$c7,$0c,$ff,$00,$00,$ff,$ff,$00,$00,$00,$07,$ff,$ff,$00,$00]));
end;

procedure TTestMpWriter.TestWriteExt;
var
  Writer: TWriter;
  I: Integer;
begin
  for I := 0 to High(EXT_TESTS) do begin
    Writer.Instance.Reset;
    Writer.Instance.AddExt(EXT_TESTS[I].ExtType, EXT_TESTS[I].Data);
    AssertTrue(I.ToString, SameBytes(Writer.Instance.ToBytes, EXT_TESTS[I].Blob));
  end;
end;

procedure TTestMpWriter.TestWriteArray;
var
  Writer: TWriter;
begin
  Writer.Instance.BeginArray(0);
  AssertTrue('0', SameBytes(Writer.Instance.ToBytes, [$90]));

  Writer.Instance.Reset;
  Writer.Instance.BeginArray(2);
  Writer.Instance.Add(Int64(1));
  Writer.Instance.Add(Int64(2));
  AssertTrue('1', SameBytes(Writer.Instance.ToBytes, [$92,$01,$02]));

  Writer.Instance.Reset;
  Writer.Instance.BeginArray(1);
  Writer.Instance.BeginArray(0);
  AssertTrue('2', SameBytes(Writer.Instance.ToBytes, [$91,$90]));
end;

procedure TTestMpWriter.TestWriteMap;
var
  Writer: TWriter;
begin
  Writer.Instance.BeginMap(0);
  AssertTrue('0', SameBytes(Writer.Instance.ToBytes, [$80]));

  Writer.Instance.Reset;
  Writer.Instance.BeginMap(1);
  Writer.Instance.Add('a');
  Writer.Instance.BeginMap(0);
  AssertTrue('1', SameBytes(Writer.Instance.ToBytes, [$81,$a1,$61,$80]));
end;

{ TTestMpReader }

procedure TTestMpReader.TestReadNil;
var
  Reader: TReader;
const
  Buffer: TBytes = ($c0);
begin
  Reader.Instance := TMpReader.Create(Pointer(Buffer), Length(Buffer), 2);
  AssertTrue(Reader.Instance.Read);
  AssertTrue(Reader.Instance.TokenKind = mtkNil);
  AssertTrue(Reader.Instance.ReadState = mrsGo);
  AssertFalse(Reader.Instance.Read);
  AssertTrue(Reader.Instance.ReadState = mrsEof);
end;

procedure TTestMpReader.TestReadBool;
var
  Reader: TReader;
const
  BufFalse: TBytes = ($c2);
  BufTrue: TBytes = ($c3);
begin
  Reader.Instance := TMpReader.Create(Pointer(BufFalse), Length(BufFalse), 2);
  AssertTrue(Reader.Instance.Read);
  AssertTrue(Reader.Instance.TokenKind = mtkBool);
  AssertFalse(Reader.Instance.AsBoolean);
  AssertTrue(Reader.Instance.ReadState = mrsGo);
  AssertFalse(Reader.Instance.Read);
  AssertTrue(Reader.Instance.ReadState = mrsEof);

  Reader.Instance := TMpReader.Create(Pointer(BufTrue), Length(BufTrue), 2);
  AssertTrue(Reader.Instance.Read);
  AssertTrue(Reader.Instance.TokenKind = mtkBool);
  AssertTrue(Reader.Instance.AsBoolean);
  AssertTrue(Reader.Instance.ReadState = mrsGo);
  AssertFalse(Reader.Instance.Read);
  AssertTrue(Reader.Instance.ReadState = mrsEof);
end;

procedure TTestMpReader.TestReadInt;
var
  Reader: TReader;
  I: Integer;
begin
  for I := 0 to High(NUM_TESTS) do begin
    Reader.Instance := TMpReader.Create(Pointer(NUM_TESTS[I].Blob), Length(NUM_TESTS[I].Blob), 2);
    AssertTrue(Reader.Instance.Read);
    AssertTrue(I.ToString, Reader.Instance.TokenKind = mtkInt);
    AssertTrue(Reader.Instance.AsInt = NUM_TESTS[I].Num);
    AssertTrue(Reader.Instance.ReadState = mrsGo);
    AssertFalse(Reader.Instance.Read);
    AssertTrue(Reader.Instance.ReadState = mrsEof);
  end;
end;

procedure TTestMpReader.TestReadStr;
var
  Reader: TReader;
const
  s0: TBytes = ($a0);
  s1: TBytes = ($a1,$61);
  s2: TBytes = ($a8,$62,$62,$62,$62,$62,$62,$62,$62);
  s3: TBytes = ($bf,$63,$63,$63,$63,$63,$63,$63,$63,$63,$63,$63,$63,$63,$63,$63,$63,$63,$63,$63,$63,$63,$63,$63,$63,$63,$63,$63,$63,$63,$63,$63);
  s4: TBytes = ($d9,$20,$63,$63,$63,$63,$63,$63,$63,$63,$63,$63,$63,$63,$63,$63,$63,$63,$63,$63,$63,$63,$63,$63,$63,$63,$63,$63,$63,$63,$63,$63,$63,$64);
  s5: TBytes = ($d9,$21,$d0,$bf,$d0,$be,$d1,$87,$d0,$b5,$d0,$bc,$d1,$83,$20,$d0,$bd,$d0,$b5,$d0,$b1,$d0,$be,$20,$d1,$81,$d0,$b8,$d0,$bd,$d0,$b5,$d0,$b5,$3f);

  Ans0 = '';
  Ans1 = 'a';
  Ans2 = 'bbbbbbbb';
  Ans3 = 'ccccccccccccccccccccccccccccccc';
  Ans4 = 'cccccccccccccccccccccccccccccccd';
  Ans5 = 'почему небо синее?';
begin
  Reader.Instance := TMpReader.Create(Pointer(s0), Length(s0), 2);
  AssertTrue(Reader.Instance.Read);
  AssertTrue('0', Reader.Instance.TokenKind = mtkString);
  AssertTrue(Reader.Instance.AsString = Ans0);
  AssertTrue(Reader.Instance.ReadState = mrsGo);
  AssertFalse(Reader.Instance.Read);
  AssertTrue(Reader.Instance.ReadState = mrsEof);

  Reader.Instance := TMpReader.Create(Pointer(s1), Length(s1), 2);
  AssertTrue(Reader.Instance.Read);
  AssertTrue('1', Reader.Instance.TokenKind = mtkString);
  AssertTrue(Reader.Instance.AsString = Ans1);
  AssertTrue(Reader.Instance.ReadState = mrsGo);
  AssertFalse(Reader.Instance.Read);
  AssertTrue(Reader.Instance.ReadState = mrsEof);

  Reader.Instance := TMpReader.Create(Pointer(s2), Length(s2), 2);
  AssertTrue(Reader.Instance.Read);
  AssertTrue('2', Reader.Instance.TokenKind = mtkString);
  AssertTrue(Reader.Instance.AsString = Ans2);
  AssertTrue(Reader.Instance.ReadState = mrsGo);
  AssertFalse(Reader.Instance.Read);
  AssertTrue(Reader.Instance.ReadState = mrsEof);

  Reader.Instance := TMpReader.Create(Pointer(s3), Length(s3), 2);
  AssertTrue(Reader.Instance.Read);
  AssertTrue('3', Reader.Instance.TokenKind = mtkString);
  AssertTrue(Reader.Instance.AsString = Ans3);
  AssertTrue(Reader.Instance.ReadState = mrsGo);
  AssertFalse(Reader.Instance.Read);
  AssertTrue(Reader.Instance.ReadState = mrsEof);

  Reader.Instance := TMpReader.Create(Pointer(s4), Length(s4), 2);
  AssertTrue(Reader.Instance.Read);
  AssertTrue('4', Reader.Instance.TokenKind = mtkString);
  AssertTrue(Reader.Instance.AsString = Ans4);
  AssertTrue(Reader.Instance.ReadState = mrsGo);
  AssertFalse(Reader.Instance.Read);
  AssertTrue(Reader.Instance.ReadState = mrsEof);

  Reader.Instance := TMpReader.Create(Pointer(s5), Length(s5), 2);
  AssertTrue(Reader.Instance.Read);
  AssertTrue('5', Reader.Instance.TokenKind = mtkString);
  AssertTrue(Reader.Instance.AsString = Ans5);
  AssertTrue(Reader.Instance.ReadState = mrsGo);
  AssertFalse(Reader.Instance.Read);
  AssertTrue(Reader.Instance.ReadState = mrsEof);
end;

procedure TTestMpReader.TestReadBin;
var
  Reader: TReader;
const
  b0: TBytes = ($c4,$00);
  b1: TBytes = ($c4,$01,$01);
  b2: TBytes = ($c4,$02,$00,$00);
  b3: TBytes = ($c4,$07,$00,$00,$00,$00,$00,$00,$00);

  Ans0: TBytes = nil;
  Ans1: TBytes = ($01);
  Ans2: TBytes = ($00,$00);
  Ans3: TBytes = ($00,$00,$00,$00,$00,$00,$00);
begin
  Reader.Instance := TMpReader.Create(Pointer(b0), Length(b0), 2);
  AssertTrue(Reader.Instance.Read);
  AssertTrue('0', Reader.Instance.TokenKind = mtkBin);
  AssertTrue(Reader.Instance.AsBinary = Ans0);
  AssertTrue(Reader.Instance.ReadState = mrsGo);
  AssertFalse(Reader.Instance.Read);
  AssertTrue(Reader.Instance.ReadState = mrsEof);

  Reader.Instance := TMpReader.Create(Pointer(b1), Length(b1), 2);
  AssertTrue(Reader.Instance.Read);
  AssertTrue('1', Reader.Instance.TokenKind = mtkBin);
  AssertTrue(SameBytes(Reader.Instance.AsBinary, Ans1));
  AssertTrue(Reader.Instance.ReadState = mrsGo);
  AssertFalse(Reader.Instance.Read);
  AssertTrue(Reader.Instance.ReadState = mrsEof);

  Reader.Instance := TMpReader.Create(Pointer(b2), Length(b2), 2);
  AssertTrue(Reader.Instance.Read);
  AssertTrue('2', Reader.Instance.TokenKind = mtkBin);
  AssertTrue(SameBytes(Reader.Instance.AsBinary, Ans2));
  AssertTrue(Reader.Instance.ReadState = mrsGo);
  AssertFalse(Reader.Instance.Read);
  AssertTrue(Reader.Instance.ReadState = mrsEof);

  Reader.Instance := TMpReader.Create(Pointer(b3), Length(b3), 2);
  AssertTrue(Reader.Instance.Read);
  AssertTrue('3', Reader.Instance.TokenKind = mtkBin);
  AssertTrue(SameBytes(Reader.Instance.AsBinary, Ans3));
  AssertTrue(Reader.Instance.ReadState = mrsGo);
  AssertFalse(Reader.Instance.Read);
  AssertTrue(Reader.Instance.ReadState = mrsEof);
end;

procedure TTestMpReader.TestReadTs;
var
  Reader: TReader;
  b:TBytes;
begin
  b := [$d6,$ff,$ff,$ff,$00,$00];
  Reader.Instance := TMpReader.Create(Pointer(b), Length(b), 2);
  AssertTrue(Reader.Instance.Read);
  AssertTrue('0', Reader.Instance.TokenKind = mtkTStamp);
  AssertTrue(Reader.Instance.AsTimeStamp = TMpTimeStamp.Make($ffff0000, 0));
  AssertTrue(Reader.Instance.ReadState = mrsGo);
  AssertFalse(Reader.Instance.Read);
  AssertTrue(Reader.Instance.ReadState = mrsEof);

  b := [$d7,$ff,$00,$03,$ff,$ff,$ff,$ff,$00,$00];
  Reader.Instance := TMpReader.Create(Pointer(b), Length(b), 2);
  AssertTrue(Reader.Instance.Read);
  AssertTrue('1', Reader.Instance.TokenKind = mtkTStamp);
  AssertTrue(Reader.Instance.AsTimeStamp = TMpTimeStamp.Make($3ffff0000, $ffff));
  AssertTrue(Reader.Instance.ReadState = mrsGo);
  AssertFalse(Reader.Instance.Read);
  AssertTrue(Reader.Instance.ReadState = mrsEof);

  b := [$c7,$0c,$ff,$00,$00,$00,$00,$00,$00,$00,$07,$ff,$ff,$00,$00];
  Reader.Instance := TMpReader.Create(Pointer(b), Length(b), 2);
  AssertTrue(Reader.Instance.Read);
  AssertTrue('2', Reader.Instance.TokenKind = mtkTStamp);
  AssertTrue(Reader.Instance.AsTimeStamp = TMpTimeStamp.Make($7ffff0000, 0));
  AssertTrue(Reader.Instance.ReadState = mrsGo);
  AssertFalse(Reader.Instance.Read);
  AssertTrue(Reader.Instance.ReadState = mrsEof);

  b := [$c7,$0c,$ff,$00,$00,$ff,$ff,$00,$00,$00,$07,$ff,$ff,$00,$00];
  Reader.Instance := TMpReader.Create(Pointer(b), Length(b), 2);
  AssertTrue(Reader.Instance.Read);
  AssertTrue('3', Reader.Instance.TokenKind = mtkTStamp);
  AssertTrue(Reader.Instance.AsTimeStamp = TMpTimeStamp.Make($7ffff0000, $ffff));
  AssertTrue(Reader.Instance.ReadState = mrsGo);
  AssertFalse(Reader.Instance.Read);
  AssertTrue(Reader.Instance.ReadState = mrsEof);
end;

procedure TTestMpReader.TestReadExt;
var
  Reader: TReader;
  I: Integer;
  b: TBytes;
begin
  for I := 0 to High(EXT_TESTS) do begin
    Reader.Instance := TMpReader.Create(Pointer(EXT_TESTS[I].Blob), Length(EXT_TESTS[I].Blob), 2);
    AssertTrue(Reader.Instance.Read);
    AssertTrue(I.ToString, Reader.Instance.TokenKind = mtkExt);
    b := Reader.Instance.AsExtention;
    AssertTrue(TMpExtType(b[0]) = EXT_TESTS[I].ExtType);
    AssertTrue(SameBytes(Copy(b, 1, Length(b)), EXT_TESTS[I].Data));
    AssertTrue(Reader.Instance.ReadState = mrsGo);
    AssertFalse(Reader.Instance.Read);
    AssertTrue(Reader.Instance.ReadState = mrsEof);
  end;
end;

procedure TTestMpReader.TestReadArray;
var
  Reader: TReader;
  b: TBytes;
begin
  b := [$90];
  Reader.Instance := TMpReader.Create(Pointer(b), Length(b), 2);
  AssertTrue(Reader.Instance.Read);
  AssertTrue('0', Reader.Instance.TokenKind = mtkArrayBegin);
  AssertTrue(Reader.Instance.StructUnread = 0);
  AssertTrue(Reader.Instance.Read);
  AssertTrue('1', Reader.Instance.TokenKind = mtkArrayEnd);
  AssertTrue(Reader.Instance.ReadState = mrsGo);
  AssertFalse(Reader.Instance.Read);
  AssertTrue(Reader.Instance.ReadState = mrsEof);

  b := [$91,$90];
  Reader.Instance := TMpReader.Create(Pointer(b), Length(b), 2);
  AssertTrue(Reader.Instance.Read);
  AssertTrue('2', Reader.Instance.TokenKind = mtkArrayBegin);
  AssertTrue(Reader.Instance.StructUnread = 1);
  AssertTrue(Reader.Instance.Read);
  AssertTrue('3', Reader.Instance.TokenKind = mtkArrayBegin);
  AssertTrue(Reader.Instance.StructUnread = 0);
  AssertTrue(Reader.Instance.Read);
  AssertTrue('4', Reader.Instance.TokenKind = mtkArrayEnd);
  AssertTrue(Reader.Instance.Read);
  AssertTrue('5', Reader.Instance.TokenKind = mtkArrayEnd);
  AssertTrue(Reader.Instance.ReadState = mrsGo);
  AssertFalse(Reader.Instance.Read);
  AssertTrue(Reader.Instance.ReadState = mrsEof);
end;

procedure TTestMpReader.TestReadMap;
var
  Reader: TReader;
  b: TBytes;
begin
  b := [$80];
  Reader.Instance := TMpReader.Create(Pointer(b), Length(b), 2);
  AssertTrue(Reader.Instance.Read);
  AssertTrue('0', Reader.Instance.TokenKind = mtkMapBegin);
  AssertTrue(Reader.Instance.StructUnread = 0);
  AssertTrue(Reader.Instance.Read);
  AssertTrue('1', Reader.Instance.TokenKind = mtkMapEnd);
  AssertTrue(Reader.Instance.ReadState = mrsGo);
  AssertFalse(Reader.Instance.Read);
  AssertTrue(Reader.Instance.ReadState = mrsEof);

  b := [$81,$a1,$61,$80];
  Reader.Instance := TMpReader.Create(Pointer(b), Length(b), 2);
  AssertTrue(Reader.Instance.Read);
  AssertTrue('2', Reader.Instance.TokenKind = mtkMapBegin);
  AssertTrue(Reader.Instance.StructUnread = 1);
  AssertTrue(Reader.Instance.Read);
  AssertTrue('3', Reader.Instance.TokenKind = mtkMapBegin);
  AssertTrue('4', Reader.Instance.KeyValue = 'a');
  AssertTrue(Reader.Instance.StructUnread = 0);
  AssertTrue(Reader.Instance.Read);
  AssertTrue('5', Reader.Instance.TokenKind = mtkMapEnd);
  AssertTrue(Reader.Instance.Read);
  AssertTrue('6', Reader.Instance.TokenKind = mtkMapEnd);
  AssertTrue(Reader.Instance.ReadState = mrsGo);
  AssertFalse(Reader.Instance.Read);
  AssertTrue(Reader.Instance.ReadState = mrsEof);
end;

{ TTestMpDomNode }

procedure TTestMpDomNode.TestMake;
var
  Node: TMpNode;
const
  f = Single(12.55);
  d = Double(2.718281828459045);
  s = 'string';
  b: TBytes = ($01,$02,$03,$04,$05);
begin
  Node.Instance := TMpDomNode.MakeNil;
  AssertTrue(Node.Instance.IsNil);

  Node.Instance := TMpDomNode.Make(True);
  AssertTrue(Node.Instance.IsBoolean);
  AssertTrue(Node.Instance.AsBoolean);

  Node.Instance := TMpDomNode.Make(False);
  AssertTrue(Node.Instance.IsBoolean);
  AssertFalse(Node.Instance.AsBoolean);

  Node.Instance := TMpDomNode.MakeI(42);
  AssertTrue(Node.Instance.IsInteger);
  AssertTrue(Node.Instance.AsInteger = 42);

  Node.Instance := TMpDomNode.MakeS(f);
  AssertTrue(Node.Instance.IsSingle);
  AssertTrue(Node.Instance.AsSingle = f);

  Node.Instance := TMpDomNode.MakeD(d);
  AssertTrue(Node.Instance.IsDouble);
  AssertTrue(Node.Instance.AsDouble = d);

  Node.Instance := TMpDomNode.Make(s);
  AssertTrue(Node.Instance.IsString);
  AssertTrue(Node.Instance.AsString = s);

  Node.Instance := TMpDomNode.MakeB(b);
  AssertTrue(Node.Instance.IsBinary);
  AssertTrue(SameBytes(Node.Instance.AsBinary, b));
end;



initialization

  RegisterTest(TTestMpWriter);
  RegisterTest(TTestMpReader);
  RegisterTest(TTestMpDomNode);

end.

