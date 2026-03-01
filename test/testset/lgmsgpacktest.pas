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

  { TTestMpStreamWriter }

  TTestMpStreamWriter = class(TTestCase)
  private
  type
    TWriter = specialize TGUniqRef<TMpStreamWriter>;
  published
    procedure TestWriteDom;
  end;

  { TTestMpReader }

  TTestMpReader = class(TTestCase)
  private
  type
    TReader = specialize TGUniqRef<TMpReader>;
    TMpNode = specialize TGAutoRef<TMpDomNode>;
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
    procedure TestFindPath;
    procedure TestFindPathBin;
    procedure TestFindPathStr;
    procedure TestFindPathStrBin;
  end;

  { TTestMpDomNode }

  TTestMpDomNode = class(TTestCase)
  private
  type
    TMpNode = specialize TGAutoRef<TMpDomNode>;
  published
    procedure TestMake;
    procedure ArrayAdd;
    procedure ArrayInsert;
    procedure ArrayDelete;
    procedure ArrayExtract;
    procedure MapAdd;
    procedure MapTryAdd;
    procedure MapDelete;
    procedure MapExtract;
    procedure MapExtractKey;
    procedure MapRemove;
    procedure MapIndexOf;
    procedure MapCountOf;
    procedure DomTryParseArray;
    procedure DomTryParseMap;
    procedure TestEqualTo;
    procedure ArrayEnumerator;
    procedure MapEnumerator;
    procedure MapPairs;
    procedure MapEqualKeys;
    procedure ArrayFindItem;
    procedure MapFindItem;
    procedure MapFindUniq;
    procedure TestFindPath;
    procedure TestFindPathStr;
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

{ TTestMpStreamWriter }

procedure TTestMpStreamWriter.TestWriteDom;
var
  Writer: TWriter;
  Stream: specialize TGAutoRef<TMemoryStream>;
  n1, n2: specialize TGAutoRef<TMpDomNode>;
begin
  n1.Instance.AddI(42).AddNil.Add('abc').AddNil;
  n1.Instance.Items[1].AddI(1, 1001).AddNil('key').Add(2, False);
  n1.Instance.Items[1].Items[1].Add('str', 'value').AddB([1,2,3,4], [2,3,4,5]).AddI(1, 77);

  Writer.Instance := TMpStreamWriter.Create(Stream.Instance);
  AssertFalse(n2.Instance.TryParse(Stream.Instance));

  Writer.Instance.WriteDom(n1.Instance);
  Stream.Instance.Position := 0;

  AssertTrue(n2.Instance.TryParse(Stream.Instance));
  AssertTrue(n2.Instance.EqualTo(n1.Instance));
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

procedure TTestMpReader.TestFindPath;
var
  Reader: TReader;
  Node: TMpNode;
  n: TMpDomNode;
  b: TBytes;
  Path: TMpVarArray;
begin
  Node.Instance.AddI(42).AddNil.Add('abc').AddNil;
  n := Node.Instance.Items[1].AddI(1, 1001).AddNil('key').Add(2, False);
  n.Items[1].Add('str', 'value').AddB([1,2,3,4], [2,3,4,5]).AddI(1, 77);
  b := Node.Instance.AsMsgPack;

  Reader.Instance := TMpReader.Create(Pointer(b), Length(b), 3);

  Path := [];
  AssertTrue(Reader.Instance.FindPath(Path, n));
  AssertTrue(n.EqualTo(Node.Instance));
  n.Free;

  Path := [0];
  AssertTrue(Reader.Instance.FindPath(Path, n));
  AssertTrue(n.EqualTo(Node.Instance.Items[0]));
  n.Free;

  Path := [1];
  AssertTrue(Reader.Instance.FindPath(Path, n));
  AssertTrue(n.EqualTo(Node.Instance.Items[1]));
  n.Free;

  Path := [2];
  AssertTrue(Reader.Instance.FindPath(Path, n));
  AssertTrue(n.EqualTo(Node.Instance.Items[2]));
  n.Free;

  Path := [3];
  AssertTrue(Reader.Instance.FindPath(Path, n));
  AssertTrue(n.EqualTo(Node.Instance.Items[3]));
  n.Free;

  Path := [4];
  AssertFalse(Reader.Instance.FindPath(Path, n));
  AssertTrue(n = nil);

  Path := [1,1];
  AssertTrue(Reader.Instance.FindPath(Path, n));
  AssertTrue(n.EqualTo(Node.Instance.Items[1][1]));
  n.Free;

  Path := [1,'key'];
  AssertTrue(Reader.Instance.FindPath(Path, n));
  AssertTrue(n.EqualTo(Node.Instance.Items[1]['key']));
  n.Free;

  Path := [1,2];
  AssertTrue(Reader.Instance.FindPath(Path, n));
  AssertTrue(n.EqualTo(Node.Instance.Items[1][2]));
  n.Free;

  Path := [1,3];
  AssertFalse(Reader.Instance.FindPath(Path, n));
  AssertTrue(n = nil);

  Path := [1,'key','str'];
  AssertTrue(Reader.Instance.FindPath(Path, n));
  AssertTrue(n.EqualTo(Node.Instance.Items[1]['key']['str']));
  n.Free;

  Path := [1,'key',[1,2,3,4]];
  AssertTrue(Reader.Instance.FindPath(Path, n));
  AssertTrue(n.EqualTo(Node.Instance.Items[1]['key'][[1,2,3,4]]));
  n.Free;

  Path := [1,'key',1];
  AssertTrue(Reader.Instance.FindPath(Path, n));
  AssertTrue(n.EqualTo(Node.Instance.Items[1]['key'][1]));
  n.Free;
end;

procedure TTestMpReader.TestFindPathBin;
var
  Reader: TReader;
  Node: TMpNode;
  n: TMpDomNode;
  b, Found: TBytes;
  Path: TMpVarArray;
begin
  Node.Instance.AddI(42).AddNil.Add('abc').AddNil;
  n := Node.Instance.Items[1].AddI(1, 1001).AddNil('key').Add(2, False);
  n.Items[1].Add('str', 'value').AddB([1,2,3,4], [2,3,4,5]).AddI(1, 77);
  b := Node.Instance.AsMsgPack;

  Reader.Instance := TMpReader.Create(Pointer(b), Length(b), 3);

  Path := [];
  AssertTrue(Reader.Instance.FindPath(Path, Found));
  AssertTrue(SameBytes(Found, Node.Instance.AsMsgPack));

  Path := [0];
  AssertTrue(Reader.Instance.FindPath(Path, Found));
  AssertTrue(SameBytes(Found, Node.Instance.Items[0].AsMsgPack));

  Path := [1];
  AssertTrue(Reader.Instance.FindPath(Path, Found));
  AssertTrue(SameBytes(Found, Node.Instance.Items[1].AsMsgPack));

  Path := [2];
  AssertTrue(Reader.Instance.FindPath(Path, Found));
  AssertTrue(SameBytes(Found, Node.Instance.Items[2].AsMsgPack));

  Path := [3];
  AssertTrue(Reader.Instance.FindPath(Path, Found));
  AssertTrue(SameBytes(Found, Node.Instance.Items[3].AsMsgPack));

  Path := [4];
  AssertFalse(Reader.Instance.FindPath(Path, Found));
  AssertTrue(Found = nil);

  Path := [1,1];
  AssertTrue(Reader.Instance.FindPath(Path, Found));
  AssertTrue(SameBytes(Found, Node.Instance.Items[1][1].AsMsgPack));

  Path := [1,'key'];
  AssertTrue(Reader.Instance.FindPath(Path, Found));
  AssertTrue(SameBytes(Found, Node.Instance.Items[1]['key'].AsMsgPack));

  Path := [1,2];
  AssertTrue(Reader.Instance.FindPath(Path, Found));
  AssertTrue(SameBytes(Found, Node.Instance.Items[1][2].AsMsgPack));

  Path := [1,3];
  AssertFalse(Reader.Instance.FindPath(Path, Found));
  AssertTrue(Found = nil);

  Path := [1,'key','str'];
  AssertTrue(Reader.Instance.FindPath(Path, Found));
  AssertTrue(SameBytes(Found, Node.Instance.Items[1]['key']['str'].AsMsgPack));

  Path := [1,'key',[1,2,3,4]];
  AssertTrue(Reader.Instance.FindPath(Path, Found));
  AssertTrue(SameBytes(Found, Node.Instance.Items[1]['key'][[1,2,3,4]].AsMsgPack));

  Path := [1,'key',1];
  AssertTrue(Reader.Instance.FindPath(Path, Found));
  AssertTrue(SameBytes(Found, Node.Instance.Items[1]['key'][1].AsMsgPack));
end;

procedure TTestMpReader.TestFindPathStr;
var
  Reader: TReader;
  Node: TMpNode;
  n: TMpDomNode;
  b: TBytes;
  Path: TStringArray;
begin
  Node.Instance.AddI(42).AddNil.Add('abc').AddNil;
  n := Node.Instance.Items[1].AddI('11', 1001).AddNil('key').Add('22', False);
  n.Items[1].Add('str', 'value').AddB('123', [2,3,4,5]).AddI('345', 77);
  b := Node.Instance.AsMsgPack;

  Reader.Instance := TMpReader.Create(Pointer(b), Length(b), 3);

  Path := [];
  AssertTrue(Reader.Instance.FindPathStr(Path, n));
  AssertTrue(n.EqualTo(Node.Instance));
  n.Free;

  Path := ['0'];
  AssertTrue(Reader.Instance.FindPathStr(Path, n));
  AssertTrue(n.EqualTo(Node.Instance.Items[0]));
  n.Free;

  Path := ['1'];
  AssertTrue(Reader.Instance.FindPathStr(Path, n));
  AssertTrue(n.EqualTo(Node.Instance.Items[1]));
  n.Free;

  Path := ['2'];
  AssertTrue(Reader.Instance.FindPathStr(Path, n));
  AssertTrue(n.EqualTo(Node.Instance.Items[2]));
  n.Free;

  Path := ['3'];
  AssertTrue(Reader.Instance.FindPathStr(Path, n));
  AssertTrue(n.EqualTo(Node.Instance.Items[3]));
  n.Free;

  Path := ['4'];
  AssertFalse(Reader.Instance.FindPathStr(Path, n));
  AssertTrue(n = nil);

  Path := ['1','11'];
  AssertTrue(Reader.Instance.FindPathStr(Path, n));
  AssertTrue(n.EqualTo(Node.Instance.Items[1]['11']));
  n.Free;

  Path := ['1','key'];
  AssertTrue(Reader.Instance.FindPathStr(Path, n));
  AssertTrue(n.EqualTo(Node.Instance.Items[1]['key']));
  n.Free;

  Path := ['1','22'];
  AssertTrue(Reader.Instance.FindPathStr(Path, n));
  AssertTrue(n.EqualTo(Node.Instance.Items[1]['22']));
  n.Free;

  Path := ['1','3'];
  AssertFalse(Reader.Instance.FindPathStr(Path, n));
  AssertTrue(n = nil);

  Path := ['1','key','str'];
  AssertTrue(Reader.Instance.FindPathStr(Path, n));
  AssertTrue(n.EqualTo(Node.Instance.Items[1]['key']['str']));
  n.Free;

  Path := ['1','key','123'];
  AssertTrue(Reader.Instance.FindPathStr(Path, n));
  AssertTrue(n.EqualTo(Node.Instance.Items[1]['key']['123']));
  n.Free;

  Path := ['1','key','345'];
  AssertTrue(Reader.Instance.FindPathStr(Path, n));
  AssertTrue(n.EqualTo(Node.Instance.Items[1]['key']['345']));
  n.Free;
end;

procedure TTestMpReader.TestFindPathStrBin;
var
  Reader: TReader;
  Node: TMpNode;
  n: TMpDomNode;
  b, Found: TBytes;
  Path: TStringArray;
begin
  Node.Instance.AddI(42).AddNil.Add('abc').AddNil;
  n := Node.Instance.Items[1].AddI('11', 1001).AddNil('key').Add('22', False);
  n.Items[1].Add('str', 'value').AddB('123', [2,3,4,5]).AddI('345', 77);
  b := Node.Instance.AsMsgPack;

  Reader.Instance := TMpReader.Create(Pointer(b), Length(b), 3);

  Path := [];
  AssertTrue(Reader.Instance.FindPathStr(Path, Found));
  AssertTrue(SameBytes(Found, Node.Instance.AsMsgPack));

  Path := ['0'];
  AssertTrue(Reader.Instance.FindPathStr(Path, Found));
  AssertTrue(SameBytes(Found, Node.Instance.Items[0].AsMsgPack));

  Path := ['1'];
  AssertTrue(Reader.Instance.FindPathStr(Path, Found));
  AssertTrue(SameBytes(Found, Node.Instance.Items[1].AsMsgPack));

  Path := ['2'];
  AssertTrue(Reader.Instance.FindPathStr(Path, Found));
  AssertTrue(SameBytes(Found, Node.Instance.Items[2].AsMsgPack));

  Path := ['3'];
  AssertTrue(Reader.Instance.FindPathStr(Path, Found));
  AssertTrue(SameBytes(Found, Node.Instance.Items[3].AsMsgPack));

  Path := ['4'];
  AssertFalse(Reader.Instance.FindPathStr(Path, Found));
  AssertTrue(Found = nil);

  Path := ['1','11'];
  AssertTrue(Reader.Instance.FindPathStr(Path, Found));
  AssertTrue(SameBytes(Found, Node.Instance.Items[1]['11'].AsMsgPack));

  Path := ['1','key'];
  AssertTrue(Reader.Instance.FindPathStr(Path, Found));
  AssertTrue(SameBytes(Found, Node.Instance.Items[1]['key'].AsMsgPack));

  Path := ['1','22'];
  AssertTrue(Reader.Instance.FindPathStr(Path, Found));
  AssertTrue(SameBytes(Found, Node.Instance.Items[1]['22'].AsMsgPack));

  Path := ['1','3'];
  AssertFalse(Reader.Instance.FindPathStr(Path, Found));
  AssertTrue(Found = nil);

  Path := ['1','key','str'];
  AssertTrue(Reader.Instance.FindPathStr(Path, Found));
  AssertTrue(SameBytes(Found, Node.Instance.Items[1]['key']['str'].AsMsgPack));

  Path := ['1','key','123'];
  AssertTrue(Reader.Instance.FindPathStr(Path, Found));
  AssertTrue(SameBytes(Found, Node.Instance.Items[1]['key']['123'].AsMsgPack));

  Path := ['1','key','345'];
  AssertTrue(Reader.Instance.FindPathStr(Path, Found));
  AssertTrue(SameBytes(Found, Node.Instance.Items[1]['key']['345'].AsMsgPack));
end;

{ TTestMpDomNode }

procedure TTestMpDomNode.TestMake;
var
  Node: TMpNode;
  ts: TMpTimeStamp;
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

  ts := TMpTimeStamp.Make(100000000, 100000000);
  Node.Instance := TMpDomNode.Make(ts);
  AssertTrue(Node.Instance.IsTimeStamp);
  AssertTrue(Node.Instance.AsTimeStamp = ts);
end;

procedure TTestMpDomNode.ArrayAdd;
var
  Node: TMpNode;
  n: TMpDomNode;
const
  i = 1001;
  f = Single(2.71);
  d = Double(3.1415926);
  s = 'value';
  b: TBytes = ($21,$22,$23,$24,$25);
begin
  AssertTrue(Node.Instance.IsNil);
  AssertTrue(Node.Instance.Count = 0);

  Node.Instance.AddNil;
  AssertTrue(Node.Instance.IsArray);
  AssertTrue(Node.Instance.Count = 1);
  AssertTrue(Node.Instance.Items[0].IsNil);

  Node.Instance.Add(True);
  AssertTrue(Node.Instance.IsArray);
  AssertTrue(Node.Instance.Count = 2);
  AssertTrue(Node.Instance.Items[1].IsBoolean);
  AssertTrue(Node.Instance.Items[1].AsBoolean);

  Node.Instance.AddI(i);
  AssertTrue(Node.Instance.IsArray);
  AssertTrue(Node.Instance.Count = 3);
  AssertTrue(Node.Instance.Items[2].IsInteger);
  AssertTrue(Node.Instance.Items[2].AsInteger = i);

  Node.Instance.AddS(f);
  AssertTrue(Node.Instance.IsArray);
  AssertTrue(Node.Instance.Count = 4);
  AssertTrue(Node.Instance.Items[3].IsSingle);
  AssertTrue(Node.Instance.Items[3].AsSingle = f);

  Node.Instance.AddD(d);
  AssertTrue(Node.Instance.IsArray);
  AssertTrue(Node.Instance.Count = 5);
  AssertTrue(Node.Instance.Items[4].IsDouble);
  AssertTrue(Node.Instance.Items[4].AsDouble = d);

  Node.Instance.Add(s);
  AssertTrue(Node.Instance.IsArray);
  AssertTrue(Node.Instance.Count = 6);
  AssertTrue(Node.Instance.Items[5].IsString);
  AssertTrue(Node.Instance.Items[5].AsString = s);

  Node.Instance.Add(b);
  AssertTrue(Node.Instance.IsArray);
  AssertTrue(Node.Instance.Count = 7);
  AssertTrue(Node.Instance.Items[6].IsBinary);
  AssertTrue(SameBytes(Node.Instance.Items[6].AsBinary, b));

  n := Node.Instance.AddNode;
  AssertTrue(n.IsNil);
  AssertTrue(Node.Instance.IsArray);
  AssertTrue(Node.Instance.Count = 8);
  AssertTrue(Node.Instance.Items[7].IsNil);
end;

procedure TTestMpDomNode.ArrayInsert;
var
  Node: TMpNode;
  I: SizeInt;
begin
  AssertTrue(Node.Instance.Count = 0);

  I := Node.Instance.InsertNil(-2);
  AssertTrue(Node.Instance.Count = 1);
  AssertTrue(I = 0);
  AssertTrue(Node.Instance.Items[I].IsNil);

  I := Node.Instance.Insert(5, 'key');
  AssertTrue(Node.Instance.Count = 2);
  AssertTrue(I = 1);
  AssertTrue(Node.Instance.Items[I].IsString);

  I := Node.Instance.InsertI(1, 42);
  AssertTrue(Node.Instance.Count = 3);
  AssertTrue(I = 1);
  AssertTrue(Node.Instance.Items[I].IsInteger);

  I := Node.Instance.Insert(0, False);
  AssertTrue(Node.Instance.Count = 4);
  AssertTrue(I = 0);
  AssertTrue(Node.Instance.Items[I].IsBoolean);

  I := Node.Instance.Insert(3, True);
  AssertTrue(Node.Instance.Count = 5);
  AssertTrue(I = 3);
  AssertTrue(Node.Instance.Items[I].IsBoolean);
end;

procedure TTestMpDomNode.ArrayDelete;
var
  Node: TMpNode;
begin
  AssertFalse(Node.Instance.Delete(0));

  Node.Instance.Add(False).Add('abcdef').Add([1,2,3,4,5,6,7]).AddI(77);
  AssertTrue(Node.Instance.IsArray);
  AssertTrue(Node.Instance.Count = 4);

  AssertFalse(Node.Instance.Delete(-1));
  AssertFalse(Node.Instance.Delete(4));

  AssertTrue(Node.Instance.Delete(0));
  AssertTrue(Node.Instance.Count = 3);
  AssertTrue(Node.Instance.Items[0].IsString);

  AssertTrue(Node.Instance.Delete(1));
  AssertTrue(Node.Instance.Count = 2);
  AssertTrue(Node.Instance.Items[1].IsInteger);

  AssertTrue(Node.Instance.Delete(0));
  AssertTrue(Node.Instance.Count = 1);
  AssertTrue(Node.Instance.Items[0].IsInteger);

  AssertTrue(Node.Instance.Delete(0));
  AssertTrue(Node.Instance.Count = 0);
end;

procedure TTestMpDomNode.ArrayExtract;
var
  Node: TMpNode;
  n: TMpDomNode;
begin
  AssertFalse(Node.Instance.Extract(0, n));

  Node.Instance.Add(False).Add('abcdef').Add([1,2,3,4,5,6,7]).AddI(77);
  AssertTrue(Node.Instance.Count = 4);
  AssertTrue(Node.Instance.IsArray);

  AssertFalse(Node.Instance.Extract(-1, n));
  AssertFalse(Node.Instance.Extract(4, n));

  AssertTrue(Node.Instance.Extract(0, n));
  AssertTrue(Node.Instance.Count = 3);
  AssertTrue(Node.Instance.Items[0].IsString);
  AssertTrue(n.IsBoolean);
  AssertFalse(n.AsBoolean);
  n.Free;

  AssertTrue(Node.Instance.Extract(1, n));
  AssertTrue(Node.Instance.Count = 2);
  AssertTrue(Node.Instance.Items[1].IsInteger);
  AssertTrue(n.IsBinary);
  n.Free;

  AssertTrue(Node.Instance.Extract(0, n));
  AssertTrue(Node.Instance.Count = 1);
  AssertTrue(Node.Instance.Items[0].IsInteger);
  AssertTrue(n.IsString);
  n.Free;

  AssertTrue(Node.Instance.Extract(0, n));
  AssertTrue(Node.Instance.Count = 0);
  AssertTrue(n.IsInteger);
  n.Free;
end;

procedure TTestMpDomNode.MapAdd;
var
  Node: TMpNode;
  n: TMpDomNode;
  k: TMpVariant;
const
  i = 1001;
  f = Single(2.71);
  d = Double(3.1415926);
  s = 'value';
  b: TBytes = ($21,$22,$23,$24,$25);
begin
  AssertTrue(Node.Instance.IsNil);
  AssertTrue(Node.Instance.Count = 0);

  Node.Instance.AddNil(1);
  AssertTrue(Node.Instance.IsMap);
  AssertTrue(Node.Instance.Count = 1);
  AssertTrue(Node.Instance.Items[0].IsNil);
  AssertTrue(Node.Instance.FindKey(0, k));
  AssertTrue(k = 1);

  Node.Instance.Add(2, True);
  AssertTrue(Node.Instance.IsMap);
  AssertTrue(Node.Instance.Count = 2);
  AssertTrue(Node.Instance.Items[1].IsBoolean);
  AssertTrue(Node.Instance.Items[1].AsBoolean);
  AssertTrue(Node.Instance.FindKey(1, k));
  AssertTrue(k = 2);

  Node.Instance.AddI(3, i);
  AssertTrue(Node.Instance.IsMap);
  AssertTrue(Node.Instance.Count = 3);
  AssertTrue(Node.Instance.Items[2].IsInteger);
  AssertTrue(Node.Instance.Items[2].AsInteger = i);
  AssertTrue(Node.Instance.FindKey(2, k));
  AssertTrue(k = 3);

  Node.Instance.AddS(4, f);
  AssertTrue(Node.Instance.IsMap);
  AssertTrue(Node.Instance.Count = 4);
  AssertTrue(Node.Instance.Items[3].IsSingle);
  AssertTrue(Node.Instance.Items[3].AsSingle = f);
  AssertTrue(Node.Instance.FindKey(3, k));
  AssertTrue(k = 4);

  Node.Instance.AddD(5, d);
  AssertTrue(Node.Instance.IsMap);
  AssertTrue(Node.Instance.Count = 5);
  AssertTrue(Node.Instance.Items[4].IsDouble);
  AssertTrue(Node.Instance.Items[4].AsDouble = d);
  AssertTrue(Node.Instance.FindKey(4, k));
  AssertTrue(k = 5);

  Node.Instance.Add(6, s);
  AssertTrue(Node.Instance.IsMap);
  AssertTrue(Node.Instance.Count = 6);
  AssertTrue(Node.Instance.Items[5].IsString);
  AssertTrue(Node.Instance.Items[5].AsString = s);
  AssertTrue(Node.Instance.FindKey(5, k));
  AssertTrue(k = 6);

  Node.Instance.Add(7, b);
  AssertTrue(Node.Instance.IsMap);
  AssertTrue(Node.Instance.Count = 7);
  AssertTrue(Node.Instance.Items[6].IsBinary);
  AssertTrue(SameBytes(Node.Instance.Items[6].AsBinary, b));
  AssertTrue(Node.Instance.FindKey(6, k));
  AssertTrue(k = 7);

  n := Node.Instance.AddNode(8);
  AssertTrue(n.IsNil);
  AssertTrue(Node.Instance.IsMap);
  AssertTrue(Node.Instance.Count = 8);
  AssertTrue(Node.Instance.Items[7].IsNil);
  AssertTrue(Node.Instance.FindKey(7, k));
  AssertTrue(k = 8);
end;

procedure TTestMpDomNode.MapTryAdd;
var
  Node: TMpNode;
  n: TMpDomNode;
const
  i = 1001;
  f = Single(2.71);
  d = Double(3.1415926);
  s = 'value';
  b: TBytes = ($21,$22,$23,$24,$25);
begin
  AssertTrue(Node.Instance.IsNil);
  AssertTrue(Node.Instance.Count = 0);

  AssertTrue(Node.Instance.TryAddNil(1));
  AssertTrue(Node.Instance.IsMap);
  AssertTrue(Node.Instance.Count = 1);
  AssertTrue(Node.Instance[1].IsNil);

  AssertFalse(Node.Instance.TryAdd(1, True));
  AssertTrue(Node.Instance.TryAdd(2, True));
  AssertTrue(Node.Instance.IsMap);
  AssertTrue(Node.Instance.Count = 2);
  AssertTrue(Node.Instance[2].IsBoolean);
  AssertTrue(Node.Instance[2].AsBoolean);

  AssertFalse(Node.Instance.TryAdd(2, True));
  AssertTrue(Node.Instance.TryAddI(3, i));
  AssertTrue(Node.Instance.IsMap);
  AssertTrue(Node.Instance.Count = 3);
  AssertTrue(Node.Instance[3].IsInteger);
  AssertTrue(Node.Instance[3].AsInteger = i);

  AssertFalse(Node.Instance.TryAdd(3, True));
  AssertTrue(Node.Instance.TryAddS(4, f));
  AssertTrue(Node.Instance.IsMap);
  AssertTrue(Node.Instance.Count = 4);
  AssertTrue(Node.Instance[4].IsSingle);
  AssertTrue(Node.Instance[4].AsSingle = f);

  AssertFalse(Node.Instance.TryAdd(4, True));
  AssertTrue(Node.Instance.TryAddD(5, d));
  AssertTrue(Node.Instance.IsMap);
  AssertTrue(Node.Instance.Count = 5);
  AssertTrue(Node.Instance[5].IsDouble);
  AssertTrue(Node.Instance[5].AsDouble = d);

  AssertFalse(Node.Instance.TryAdd(5, True));
  AssertTrue(Node.Instance.TryAdd(6, s));
  AssertTrue(Node.Instance.IsMap);
  AssertTrue(Node.Instance.Count = 6);
  AssertTrue(Node.Instance[6].IsString);
  AssertTrue(Node.Instance[6].AsString = s);

  AssertFalse(Node.Instance.TryAdd(6, True));
  AssertTrue(Node.Instance.TryAdd(7, b));
  AssertTrue(Node.Instance.IsMap);
  AssertTrue(Node.Instance.Count = 7);
  AssertTrue(Node.Instance[7].IsBinary);
  AssertTrue(SameBytes(Node.Instance[7].AsBinary, b));

  AssertFalse(Node.Instance.TryAddNode(7, n));
  AssertTrue(Node.Instance.TryAddNode(8, n));
  AssertTrue(n.IsNil);
  AssertTrue(Node.Instance.IsMap);
  AssertTrue(Node.Instance.Count = 8);
  AssertTrue(Node.Instance[8].IsNil);
end;

procedure TTestMpDomNode.MapDelete;
var
  Node: TMpNode;
begin
  AssertFalse(Node.Instance.Delete(0));

  Node.Instance.Add(1, False).Add(2, 'abcdef').Add(3, [1,2,3,4,5,6,7]).AddI(4, 77);
  AssertTrue(Node.Instance.Count = 4);
  AssertTrue(Node.Instance.IsMap);

  AssertFalse(Node.Instance.Delete(-1));
  AssertFalse(Node.Instance.Delete(4));

  AssertTrue(Node.Instance.Delete(0));
  AssertTrue(Node.Instance.Count = 3);
  AssertTrue(Node.Instance.Items[0].IsString);

  AssertTrue(Node.Instance.Delete(1));
  AssertTrue(Node.Instance.Count = 2);
  AssertTrue(Node.Instance.Items[1].IsInteger);

  AssertTrue(Node.Instance.Delete(0));
  AssertTrue(Node.Instance.Count = 1);
  AssertTrue(Node.Instance.Items[0].IsInteger);

  AssertTrue(Node.Instance.Delete(0));
  AssertTrue(Node.Instance.Count = 0);
end;

procedure TTestMpDomNode.MapExtract;
var
  Node: TMpNode;
  n: TMpDomNode;
begin
  AssertFalse(Node.Instance.Extract(0, n));

  Node.Instance.Add(1, False).Add(2, 'abcdef').Add(3, [1,2,3,4,5,6,7]).AddI(4, 42);
  AssertTrue(Node.Instance.Count = 4);
  AssertTrue(Node.Instance.IsMap);

  AssertFalse(Node.Instance.Extract(-1, n));
  AssertFalse(Node.Instance.Extract(4, n));

  AssertTrue(Node.Instance.Extract(0, n));
  AssertTrue(Node.Instance.Count = 3);
  AssertTrue(Node.Instance.Items[0].IsString);
  AssertTrue(n.IsBoolean);
  AssertFalse(n.AsBoolean);
  n.Free;

  AssertTrue(Node.Instance.Extract(1, n));
  AssertTrue(Node.Instance.Count = 2);
  AssertTrue(Node.Instance.Items[1].IsInteger);
  AssertTrue(n.IsBinary);
  n.Free;

  AssertTrue(Node.Instance.Extract(0, n));
  AssertTrue(Node.Instance.Count = 1);
  AssertTrue(Node.Instance.Items[0].IsInteger);
  AssertTrue(n.IsString);
  n.Free;

  AssertTrue(Node.Instance.Extract(0, n));
  AssertTrue(Node.Instance.Count = 0);
  AssertTrue(n.IsInteger);
  n.Free;
end;

procedure TTestMpDomNode.MapExtractKey;
var
  Node: TMpNode;
  n: TMpDomNode;
begin
  AssertFalse(Node.Instance.ExtractKey(1, n));

  Node.Instance.Add(1, False).Add(2, 'abcdef').Add(3, [1,2,3,4,5,6,7]).AddI(4, 42).Add(4, 'key');
  AssertTrue(Node.Instance.Count = 5);
  AssertTrue(Node.Instance.IsMap);

  AssertFalse(Node.Instance.ExtractKey(0, n));
  AssertFalse(Node.Instance.ExtractKey(6, n));

  AssertTrue(Node.Instance.ExtractKey(3, n));
  AssertTrue(Node.Instance.Count = 4);
  AssertTrue(n.IsBinary);
  n.Free;

  AssertTrue(Node.Instance.ExtractKey(1, n));
  AssertTrue(Node.Instance.Count = 3);
  AssertTrue(n.IsBoolean);
  n.Free;

  AssertTrue(Node.Instance.ExtractKey(4, n));
  AssertTrue(Node.Instance.Count = 2);
  AssertTrue(n.IsString);
  n.Free;

  AssertTrue(Node.Instance.ExtractKey(2, n));
  AssertTrue(Node.Instance.Count = 1);
  AssertTrue(n.IsString);
  n.Free;

  AssertTrue(Node.Instance.ExtractKey(4, n));
  AssertTrue(Node.Instance.Count = 0);
  AssertTrue(n.IsInteger);
  n.Free;
end;

procedure TTestMpDomNode.MapRemove;
var
  Node: TMpNode;
begin
  AssertFalse(Node.Instance.Remove(1));

  Node.Instance.Add(1, False).Add(2, 'abcdef').Add(3, [1,2,3,4,5,6,7]).AddI(4, 42).Add(4, 'key');
  AssertTrue(Node.Instance.Count = 5);
  AssertTrue(Node.Instance.IsMap);

  AssertFalse(Node.Instance.Remove(0));
  AssertFalse(Node.Instance.Remove(6));

  AssertTrue(Node.Instance.Remove(3));
  AssertTrue(Node.Instance.Count = 4);

  AssertTrue(Node.Instance.Remove(1));
  AssertTrue(Node.Instance.Count = 3);

  AssertTrue(Node.Instance.Remove(4));
  AssertTrue(Node.Instance.Count = 2);

  AssertTrue(Node.Instance.Remove(2));
  AssertTrue(Node.Instance.Count = 1);

  AssertTrue(Node.Instance.Remove(4));
  AssertTrue(Node.Instance.Count = 0);

  AssertFalse(Node.Instance.Remove(4));
end;

procedure TTestMpDomNode.MapIndexOf;
var
  Node: TMpNode;
  I, Idx: SizeInt;
begin
  Node.Instance.Add('abc').Add(False);
  AssertTrue(Node.Instance.IsArray);
  AssertTrue(Node.Instance.IndexOf(0) < 0);
  AssertTrue(Node.Instance.IndexOf(1) < 0);

  Node.Instance.AddI(1, 42).AddNil(2).Add(3, True).Add(4, 'string').AddB(5, [1,2,3,4,5])
               .Add(6, TMpTimeStamp.Make(1000,1000)).AddD(7, 3.1415926);
  AssertTrue(Node.Instance.IsMap);
  AssertTrue(Node.Instance.Count = 7);
  for I := 1 to Node.Instance.Count do begin
    Idx := Node.Instance.IndexOf(I);
    AssertTrue(Idx >= 0);
    AssertTrue(Node.Instance.Pairs[Idx].Key = I);
  end;
end;

procedure TTestMpDomNode.MapCountOf;
var
  Node: TMpNode;
begin
  Node.Instance.AddI(1, 42).AddNil(1).Add(2, True).Add(3, 'string').AddB(3, [1,2,3,4,5])
               .Add(3, TMpTimeStamp.Make(1000,1000)).AddD(5, 3.1415926);
  AssertTrue(Node.Instance.IsMap);
  AssertTrue(Node.Instance.Count = 7);

  AssertTrue(Node.Instance.CountOf(0) = 0);
  AssertTrue(Node.Instance.CountOf(1) = 2);
  AssertTrue(Node.Instance.CountOf(2) = 1);
  AssertTrue(Node.Instance.CountOf(3) = 3);
  AssertTrue(Node.Instance.CountOf(4) = 0);
  AssertTrue(Node.Instance.CountOf(5) = 1);
end;

procedure TTestMpDomNode.DomTryParseArray;
var
  Node: TMpNode;
  n: TMpDomNode;
  b: TBytes;
begin
  b := [$90];
  AssertTrue(Node.Instance.TryParse(b));
  AssertTrue(Node.Instance.IsArray);
  AssertTrue(Node.Instance.Count = 0);

  b := [$91,$91,$90];
  AssertTrue(Node.Instance.TryParse(b));
  AssertTrue(Node.Instance.IsArray);
  AssertTrue(Node.Instance.Count = 1);
  n := Node.Instance.Items[0];
  AssertTrue(n.IsArray);
  AssertTrue(n.Count = 1);
  n := n.Items[0];
  AssertTrue(n.IsArray);
  AssertTrue(n.Count = 0);

  b := [$91,$91,$90,$90];
  AssertFalse(Node.Instance.TryParse(b));
end;

procedure TTestMpDomNode.DomTryParseMap;
var
  Node: TMpNode;
  n: TMpDomNode;
  b: TBytes;
  k: TMpVariant;
begin
  b := [$80];
  AssertTrue(Node.Instance.TryParse(b));
  AssertTrue(Node.Instance.IsMap);
  AssertTrue(Node.Instance.Count = 0);

  b := [$81,$a1,$61,$81,$a1,$62,$80];
  AssertTrue(Node.Instance.TryParse(b));
  AssertTrue(Node.Instance.IsMap);
  AssertTrue(Node.Instance.Count = 1);
  n := Node.Instance.Items[0];
  k := Node.Instance.Pairs[0].Key;
  AssertTrue(k = 'a');
  AssertTrue(n.IsMap);
  AssertTrue(n.Count = 1);
  k := n.Pairs[0].Key;
  n := n.Items[0];
  AssertTrue(k = 'b');
  AssertTrue(n.IsMap);
  AssertTrue(n.Count = 0);

  b := [$81,$a1,$61,$81,$a1,$62,$80,$80];
  AssertFalse(Node.Instance.TryParse(b));
end;

procedure TTestMpDomNode.TestEqualTo;
var
  Node, Other: TMpNode;
begin
  AssertTrue(TMpDomNode(nil).EqualTo(TMpDomNode(nil)));

  AssertTrue(Node.Instance.EqualTo(Other.Instance));

  Node.Instance.AddI(42).AddNil.Add(True).Add('string').AddB([1,2,3,4,5])
  .Add(TMpTimeStamp.Make(1000,1000)).AddNode
  .AddI(1, 1001).AddNil(2).Add(3, False).Add(4, 'key').AddB(5, [6,7,8,9])
  .Add(6, TMpTimeStamp.Make(10000,10000));

  Other.Instance := Node.Instance.Clone;
  AssertTrue(Node.Instance.EqualTo(Other.Instance));

  Other.Instance.Items[Other.Instance.Count - 1][3].AsBoolean := True;
  AssertFalse(Node.Instance.EqualTo(Other.Instance));

  Other.Instance.Items[Other.Instance.Count - 1][3].AsBoolean := False;
  AssertTrue(Node.Instance.EqualTo(Other.Instance));

  Inc(Other.Instance.Items[Other.Instance.Count - 1][5].AsBinary[2]);
  AssertFalse(Node.Instance.EqualTo(Other.Instance));

  Dec(Other.Instance.Items[Other.Instance.Count - 1][5].AsBinary[2]);
  AssertTrue(Node.Instance.EqualTo(Other.Instance));

  Node.Instance.Items[Other.Instance.Count - 1].Add(7, 'value');
  Other.Instance.Items[Other.Instance.Count - 1].Add(6, 'value');
  AssertFalse(Node.Instance.EqualTo(Other.Instance));
end;

procedure TTestMpDomNode.ArrayEnumerator;
var
  Node: TMpNode;
  n: TMpDomNode;
  I: SizeInt;
begin
  I := 0;
  for n in Node.Instance do
    Inc(I);
  AssertTrue(I = 0);

  Node.Instance := TMpDomNode.Make('abcd');
  for n in Node.Instance do
    Inc(I);
  AssertTrue(I = 0);

  Node.Instance.AddI(42).AddNil.Add(True).Add('string').AddB([1,2,3,4,5]).Add(TMpTimeStamp.Make(1000,1000));
  AssertTrue(Node.Instance.IsArray);
  AssertTrue(Node.Instance.Count = 6);
  for n in Node.Instance do begin
    AssertTrue(n.EqualTo(Node.Instance.Items[I]));
    Inc(I);
  end;
  AssertTrue(Node.Instance.Count = I);
end;

procedure TTestMpDomNode.MapEnumerator;
var
  Node: TMpNode;
  n: TMpDomNode;
  I: SizeInt;
begin

  Node.Instance.AddI(1, 42).AddNil(2).Add(3, True).Add(4, 'string').AddB(5,[1,2,3,4,5])
               .Add(6, TMpTimeStamp.Make(1000,1000));
  AssertTrue(Node.Instance.IsMap);
  AssertTrue(Node.Instance.Count = 6);
  I := 0;
  for n in Node.Instance do begin
    AssertTrue(n.EqualTo(Node.Instance.Items[I]));
    Inc(I);
  end;
  AssertTrue(Node.Instance.Count = I);
end;

procedure TTestMpDomNode.MapPairs;
var
  Node: TMpNode;
  I: SizeInt;
  p: TMpDomNode.TPair;
begin
  Node.Instance.AddI(1, 42).AddNil(2).Add(3, True).Add(4, 'string').AddB(5, [1,2,3,4,5])
               .Add(6, TMpTimeStamp.Make(1000,1000));
  AssertTrue(Node.Instance.IsMap);
  AssertTrue(Node.Instance.Count = 6);
  I := 0;
  for p in Node.Instance.PairEnumerator do begin
    AssertTrue(p.Key = I + 1);
    AssertTrue(p.Value.EqualTo(Node.Instance.Items[I]));
    Inc(I);
  end;
  AssertTrue(Node.Instance.Count = I);
end;

procedure TTestMpDomNode.MapEqualKeys;
var
  Node: TMpNode;
  p: TMpDomNode.TPair;
  I: SizeInt;
begin
  Node.Instance.AddI(1, 42).AddNil(1).Add(2, True).Add(3, 'string').AddB(3, [1,2,3,4,5])
               .Add(3, TMpTimeStamp.Make(1000,1000)).AddD(5, 3.1415926);
  AssertTrue(Node.Instance.IsMap);
  AssertTrue(Node.Instance.Count = 7);

  I := 0;
  for p in Node.Instance.EqualKeys(0) do
    Inc(I);
  AssertTrue(I = 0);
  for p in Node.Instance.EqualKeys(4) do
    Inc(I);
  AssertTrue(I = 0);

  for p in Node.Instance.EqualKeys(1) do begin
    AssertTrue(p.Key = 1);
    Inc(I);
  end;
  AssertTrue(I = 2);

  I := 0;
  for p in Node.Instance.EqualKeys(2) do begin
    AssertTrue(p.Key = 2);
    Inc(I);
  end;
  AssertTrue(I = 1);

  I := 0;
  for p in Node.Instance.EqualKeys(3) do begin
    AssertTrue(p.Key = 3);
    Inc(I);
  end;
  AssertTrue(I = 3);

  I := 0;
  for p in Node.Instance.EqualKeys(5) do begin
    AssertTrue(p.Key = 5);
    Inc(I);
  end;
  AssertTrue(I = 1);
end;

procedure TTestMpDomNode.ArrayFindItem;
var
  Node: TMpNode;
  n: TMpDomNode;
begin
  n := Node.Instance;
  AssertFalse(Node.Instance.FindItem(0, n));
  AssertTrue(n = nil);

  Node.Instance.AsString := 'abcd';
  n := Node.Instance;
  AssertFalse(Node.Instance.FindItem(0, n));
  AssertTrue(n = nil);

  Node.Instance.AddI(42).AddNil.Add(True).Add('string').AddB([1,2,3,4,5]);

  AssertFalse(Node.Instance.FindItem(-1, n));
  AssertFalse(Node.Instance.FindItem(5, n));

  AssertTrue(Node.Instance.FindItem(0, n));
  AssertTrue(n.IsInteger and (n.AsInteger = 42));

  AssertTrue(Node.Instance.FindItem(1, n));
  AssertTrue(n.IsNil);

  AssertTrue(Node.Instance.FindItem(2, n));
  AssertTrue(n.IsBoolean and n.AsBoolean);

  AssertTrue(Node.Instance.FindItem(3, n));
  AssertTrue(n.IsString and (n.AsString = 'string'));

  AssertTrue(Node.Instance.FindItem(4, n));
  AssertTrue(n.IsBinary and SameBytes(n.AsBinary, [1,2,3,4,5]));
end;

procedure TTestMpDomNode.MapFindItem;
var
  Node: TMpNode;
  n: TMpDomNode;
begin
  Node.Instance.AddI(1, 42).AddNil(2).Add(3, True).Add(4, 'string').AddB(5,[1,2,3,4,5]);

  AssertFalse(Node.Instance.FindItem(-1, n));
  AssertFalse(Node.Instance.FindItem(5, n));

  AssertTrue(Node.Instance.FindItem(0, n));
  AssertTrue(n.IsInteger and (n.AsInteger = 42));

  AssertTrue(Node.Instance.FindItem(1, n));
  AssertTrue(n.IsNil);

  AssertTrue(Node.Instance.FindItem(2, n));
  AssertTrue(n.IsBoolean and n.AsBoolean);

  AssertTrue(Node.Instance.FindItem(3, n));
  AssertTrue(n.IsString and (n.AsString = 'string'));

  AssertTrue(Node.Instance.FindItem(4, n));
  AssertTrue(n.IsBinary and SameBytes(n.AsBinary, [1,2,3,4,5]));
end;

procedure TTestMpDomNode.MapFindUniq;
var
  Node: TMpNode;
  n: TMpDomNode;
begin
  Node.Instance.AddI(1, 42).AddNil(2).Add(3, True).Add(4, 'string').AddB(5,[1,2,3]).AddNil(3).AddI(5, 1001);

  n := Node.Instance;
  AssertFalse(Node.Instance.FindUniq(0, n));
  AssertTrue(n = nil);
  AssertFalse(Node.Instance.FindUniq(5, n));

  AssertTrue(Node.Instance.FindUniq(1, n));
  AssertTrue(n.IsInteger and (n.AsInteger = 42));

  AssertTrue(Node.Instance.FindUniq(2, n));
  AssertTrue(n.IsNil);

  AssertFalse(Node.Instance.FindUniq(3, n));
  AssertTrue(n = nil);

  AssertTrue(Node.Instance.FindUniq(4, n));
  AssertTrue(n.IsString and (n.AsString = 'string'));

  AssertFalse(Node.Instance.FindUniq(5, n));
  AssertTrue(n = nil);
end;

procedure TTestMpDomNode.TestFindPath;
var
  Node: TMpNode;
  n: TMpDomNode;
  Path, Found: TMpVarArray;
begin
  Node.Instance.AddI(42).AddNil.Add('abc').AddNil;
  n := Node.Instance.Items[1].AddI(1, 1001).AddNil('key').Add(2, False);
  n.Items[1].Add('str', 'value').AddB([1,2,3,4], [2,3,4,5]).AddI(1, 77);
  AssertTrue(Node.Instance.IsArray);
  AssertTrue(Node.Instance.Count = 4);

  Path := [];
  AssertTrue(Node.Instance.FindPath(Path, n));
  AssertTrue(n = Node.Instance);

  Path := [0];
  AssertTrue(Node.Instance.FindPath(Path, n));
  AssertTrue(n = Node.Instance.Items[0]);

  Path := [1];
  AssertTrue(Node.Instance.FindPath(Path, n));
  AssertTrue(n = Node.Instance.Items[1]);

  Path := [2];
  AssertTrue(Node.Instance.FindPath(Path, n));
  AssertTrue(n = Node.Instance.Items[2]);

  Path := [3];
  AssertTrue(Node.Instance.FindPath(Path, n));
  AssertTrue(n = Node.Instance.Items[3]);

  Path := [4];
  AssertFalse(Node.Instance.FindPath(Path, n));
  AssertTrue(n = nil);

  Path := [1,'key'];
  AssertTrue(Node.Instance.FindPath(Path, n));
  AssertTrue(n = Node.Instance.Items[1]['key']);
  AssertTrue(Node.Instance.TryGetPath(n, Found));
  AssertTrue(TMpVariant.ArrayEqual(Path, Found));

  Path := [1,'key','str'];
  AssertTrue(Node.Instance.FindPath(Path, n));
  AssertTrue(n = Node.Instance.Items[1]['key']['str']);
  AssertTrue(Node.Instance.TryGetPath(n, Found));
  AssertTrue(TMpVariant.ArrayEqual(Path, Found));

  Path := [1,'key',[1,2,3,4]];
  AssertTrue(Node.Instance.FindPath(Path, n));
  AssertTrue(n = Node.Instance.Items[1]['key'][[1,2,3,4]]);
  AssertTrue(Node.Instance.TryGetPath(n, Found));
  AssertTrue(TMpVariant.ArrayEqual(Path, Found));
end;

procedure TTestMpDomNode.TestFindPathStr;
var
  Node: TMpNode;
  n: TMpDomNode;
  Path: TStringArray;
begin
  Node.Instance.AddI(42).AddNil.Add('abc').AddNil;
  n := Node.Instance.Items[1].AddI(1, 1001).AddNil('key').Add(2, False);
  n.Items[1].Add('str', 'value').AddB([1,2,3,4], [2,3,4,5]).AddI(1, 77);
  AssertTrue(Node.Instance.IsArray);
  AssertTrue(Node.Instance.Count = 4);

  Path := [];
  AssertTrue(Node.Instance.FindPathStr(Path, n));
  AssertTrue(n = Node.Instance);

  Path := ['0'];
  AssertTrue(Node.Instance.FindPathStr(Path, n));
  AssertTrue(n = Node.Instance.Items[0]);

  Path := ['1'];
  AssertTrue(Node.Instance.FindPathStr(Path, n));
  AssertTrue(n = Node.Instance.Items[1]);

  Path := ['2'];
  AssertTrue(Node.Instance.FindPathStr(Path, n));
  AssertTrue(n = Node.Instance.Items[2]);

  Path := ['3'];
  AssertTrue(Node.Instance.FindPathStr(Path, n));
  AssertTrue(n = Node.Instance.Items[3]);

  Path := ['4'];
  AssertFalse(Node.Instance.FindPathStr(Path, n));
  AssertTrue(n = nil);

  Path := ['1','key'];
  AssertTrue(Node.Instance.FindPathStr(Path, n));
  AssertTrue(n = Node.Instance.Items[1]['key']);

  Path := ['1','key','str'];
  AssertTrue(Node.Instance.FindPathStr(Path, n));
  AssertTrue(n = Node.Instance.Items[1]['key']['str']);
end;



initialization

  RegisterTest(TTestMpWriter);
  RegisterTest(TTestMpStreamWriter);
  RegisterTest(TTestMpReader);
  RegisterTest(TTestMpDomNode);

end.

