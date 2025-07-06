unit LGHashTest;

{$mode objfpc}{$H+}

interface

uses

  Classes, SysUtils, fpcunit, testregistry,
  LGHash;

type

  TxxHash32Test = class(TTestCase)
  private
  const
    UnexpFmt = 'Unexpected TxxHash32LE result($%x)';

  published
    procedure HashBuf0Seed0;
    procedure HashBuf1Seed0;
    procedure HashBuf2Seed0;
    procedure HashBuf3Seed0;
    procedure HashBuf4Seed0;
    procedure HashBuf5Seed0;
    procedure HashBuf8Seed0;
    procedure HashBuf12Seed0;
    procedure HashBuf15Seed0;
    procedure HashBuf16Seed0;
    procedure HashBuf17Seed0;
    procedure HashBuf21Seed0;
    procedure HashBuf25Seed0;
    procedure HashBuf31Seed0;

    procedure HashBuf0Seed1000001;
    procedure HashBuf1Seed1000001;
    procedure HashBuf2Seed1000001;
    procedure HashBuf3Seed1000001;
    procedure HashBuf4Seed1000001;
    procedure HashBuf5Seed1000001;
    procedure HashBuf8Seed1000001;
    procedure HashBuf12Seed1000001;
    procedure HashBuf15Seed1000001;
    procedure HashBuf16Seed1000001;
    procedure HashBuf17Seed1000001;
    procedure HashBuf21Seed1000001;
    procedure HashBuf25Seed1000001;
    procedure HashBuf31Seed1000001;

    procedure HashWord0Seed0;
    procedure HashWordSeed0;
    procedure HashWord0Seed1000001;
    procedure HashWordSeed1000001;

    procedure HashDWord0Seed0;
    procedure HashDWordSeed0;
    procedure HashDWord0Seed1000001;
    procedure HashDWordSeed1000001;

    procedure HashQWord0Seed0;
    procedure HashQWordSeed0;
    procedure HashQWord0Seed1000001;
    procedure HashQWordSeed1000001;

    procedure HashGuid0Seed0;
    procedure HashGuidSeed0;
    procedure HashGuid0Seed1000001;
    procedure HashGuidSeed1000001;

    procedure Incremental;
  end;

  TxxHash64Test = class(TTestCase)
  private
  const
    UnexpFmt = 'Unexpected TxxHash64LE result($%x)';

  published
    procedure HashBuf0Seed0;
    procedure HashBuf1Seed0;
    procedure HashBuf2Seed0;
    procedure HashBuf3Seed0;
    procedure HashBuf4Seed0;
    procedure HashBuf5Seed0;
    procedure HashBuf8Seed0;
    procedure HashBuf12Seed0;
    procedure HashBuf15Seed0;
    procedure HashBuf16Seed0;
    procedure HashBuf17Seed0;
    procedure HashBuf21Seed0;
    procedure HashBuf25Seed0;
    procedure HashBuf31Seed0;
    procedure HashBuf32Seed0;
    procedure HashBuf35Seed0;
    procedure HashBuf36Seed0;
    procedure HashBuf39Seed0;
    procedure HashBuf40Seed0;
    procedure HashBuf41Seed0;

    procedure HashBuf0Seed1000001;
    procedure HashBuf1Seed1000001;
    procedure HashBuf2Seed1000001;
    procedure HashBuf3Seed1000001;
    procedure HashBuf4Seed1000001;
    procedure HashBuf5Seed1000001;
    procedure HashBuf8Seed1000001;
    procedure HashBuf12Seed1000001;
    procedure HashBuf15Seed1000001;
    procedure HashBuf16Seed1000001;
    procedure HashBuf17Seed1000001;
    procedure HashBuf21Seed1000001;
    procedure HashBuf25Seed1000001;
    procedure HashBuf31Seed1000001;
    procedure HashBuf32Seed1000001;
    procedure HashBuf35Seed1000001;
    procedure HashBuf36Seed1000001;
    procedure HashBuf39Seed1000001;
    procedure HashBuf40Seed1000001;
    procedure HashBuf41Seed1000001;

    procedure HashWord0Seed0;
    procedure HashWordSeed0;
    procedure HashWord0Seed1000001;
    procedure HashWordSeed1000001;

    procedure HashDWord0Seed0;
    procedure HashDWordSeed0;
    procedure HashDWord0Seed1000001;
    procedure HashDWordSeed1000001;

    procedure HashQWord0Seed0;
    procedure HashQWordSeed0;
    procedure HashQWord0Seed1000001;
    procedure HashQWordSeed1000001;

    procedure HashGuid0Seed0;
    procedure HashGuidSeed0;
    procedure HashGuid0Seed1000001;
    procedure HashGuidSeed1000001;

    procedure Incremental;
  end;

  TMurmur2Test = class(TTestCase)
  private
  const
    UnexpFmt = 'Unexpected TMurmur2LE result($%x)';

  published
    procedure HashBuf0Seed0;
    procedure HashBuf1Seed0;
    procedure HashBuf2Seed0;
    procedure HashBuf3Seed0;
    procedure HashBuf4Seed0;
    procedure HashBuf5Seed0;
    procedure HashBuf8Seed0;
    procedure HashBuf12Seed0;
    procedure HashBuf15Seed0;
    procedure HashBuf16Seed0;
    procedure HashBuf17Seed0;
    procedure HashBuf21Seed0;
    procedure HashBuf25Seed0;
    procedure HashBuf31Seed0;

    procedure HashBuf0Seed1000001;
    procedure HashBuf1Seed1000001;
    procedure HashBuf2Seed1000001;
    procedure HashBuf3Seed1000001;
    procedure HashBuf4Seed1000001;
    procedure HashBuf5Seed1000001;
    procedure HashBuf8Seed1000001;
    procedure HashBuf12Seed1000001;
    procedure HashBuf15Seed1000001;
    procedure HashBuf16Seed1000001;
    procedure HashBuf17Seed1000001;
    procedure HashBuf21Seed1000001;
    procedure HashBuf25Seed1000001;
    procedure HashBuf31Seed1000001;

    procedure HashWord0Seed0;
    procedure HashWordSeed0;
    procedure HashWord0Seed1000001;
    procedure HashWordSeed1000001;

    procedure HashDWord0Seed0;
    procedure HashDWordSeed0;
    procedure HashDWord0Seed1000001;
    procedure HashDWordSeed1000001;

    procedure HashQWord0Seed0;
    procedure HashQWordSeed0;
    procedure HashQWord0Seed1000001;
    procedure HashQWordSeed1000001;

    procedure HashGuid0Seed0;
    procedure HashGuidSeed0;
    procedure HashGuid0Seed1000001;
    procedure HashGuidSeed1000001;
  end;

  TMurmur2ATest = class(TTestCase)
  private
  const
    UnexpFmt = 'Unexpected TMurmur2aLE result($%x)';

  published
    procedure HashBuf0Seed0;
    procedure HashBuf1Seed0;
    procedure HashBuf2Seed0;
    procedure HashBuf3Seed0;
    procedure HashBuf4Seed0;
    procedure HashBuf5Seed0;
    procedure HashBuf8Seed0;
    procedure HashBuf12Seed0;
    procedure HashBuf15Seed0;
    procedure HashBuf16Seed0;
    procedure HashBuf17Seed0;
    procedure HashBuf21Seed0;
    procedure HashBuf25Seed0;
    procedure HashBuf31Seed0;

    procedure HashBuf0Seed1000001;
    procedure HashBuf1Seed1000001;
    procedure HashBuf2Seed1000001;
    procedure HashBuf3Seed1000001;
    procedure HashBuf4Seed1000001;
    procedure HashBuf5Seed1000001;
    procedure HashBuf8Seed1000001;
    procedure HashBuf12Seed1000001;
    procedure HashBuf15Seed1000001;
    procedure HashBuf16Seed1000001;
    procedure HashBuf17Seed1000001;
    procedure HashBuf21Seed1000001;
    procedure HashBuf25Seed1000001;
    procedure HashBuf31Seed1000001;

    procedure HashWord0Seed0;
    procedure HashWordSeed0;
    procedure HashWord0Seed1000001;
    procedure HashWordSeed1000001;

    procedure HashDWord0Seed0;
    procedure HashDWordSeed0;
    procedure HashDWord0Seed1000001;
    procedure HashDWordSeed1000001;

    procedure HashQWord0Seed0;
    procedure HashQWordSeed0;
    procedure HashQWord0Seed1000001;
    procedure HashQWordSeed1000001;

    procedure HashGuid0Seed0;
    procedure HashGuidSeed0;
    procedure HashGuid0Seed1000001;
    procedure HashGuidSeed1000001;
  end;

  TMurmur3Test = class(TTestCase)
  private
  const
    UnexpFmt = 'Unexpected TMurmur3LE result($%x)';

  published
    procedure HashBuf0Seed0;
    procedure HashBuf1Seed0;
    procedure HashBuf2Seed0;
    procedure HashBuf3Seed0;
    procedure HashBuf4Seed0;
    procedure HashBuf5Seed0;
    procedure HashBuf8Seed0;
    procedure HashBuf12Seed0;
    procedure HashBuf15Seed0;
    procedure HashBuf16Seed0;
    procedure HashBuf17Seed0;
    procedure HashBuf21Seed0;
    procedure HashBuf25Seed0;
    procedure HashBuf31Seed0;

    procedure HashBuf0Seed1000001;
    procedure HashBuf1Seed1000001;
    procedure HashBuf2Seed1000001;
    procedure HashBuf3Seed1000001;
    procedure HashBuf4Seed1000001;
    procedure HashBuf5Seed1000001;
    procedure HashBuf8Seed1000001;
    procedure HashBuf12Seed1000001;
    procedure HashBuf15Seed1000001;
    procedure HashBuf16Seed1000001;
    procedure HashBuf17Seed1000001;
    procedure HashBuf21Seed1000001;
    procedure HashBuf25Seed1000001;
    procedure HashBuf31Seed1000001;

    procedure HashWord0Seed0;
    procedure HashWordSeed0;
    procedure HashWord0Seed1000001;
    procedure HashWordSeed1000001;

    procedure HashDWord0Seed0;
    procedure HashDWordSeed0;
    procedure HashDWord0Seed1000001;
    procedure HashDWordSeed1000001;

    procedure HashQWord0Seed0;
    procedure HashQWordSeed0;
    procedure HashQWord0Seed1000001;
    procedure HashQWordSeed1000001;

    procedure HashGuid0Seed0;
    procedure HashGuidSeed0;
    procedure HashGuid0Seed1000001;
    procedure HashGuidSeed1000001;

    procedure Incremental;
  end;


  TMurmur64aTest = class(TTestCase)
  private
  const
    UnexpFmt = 'Unexpected TMurmur64aLE result($%x)';

  published
    procedure HashBuf0Seed0;
    procedure HashBuf1Seed0;
    procedure HashBuf2Seed0;
    procedure HashBuf3Seed0;
    procedure HashBuf4Seed0;
    procedure HashBuf5Seed0;
    procedure HashBuf8Seed0;
    procedure HashBuf12Seed0;
    procedure HashBuf15Seed0;
    procedure HashBuf16Seed0;
    procedure HashBuf17Seed0;
    procedure HashBuf21Seed0;
    procedure HashBuf25Seed0;
    procedure HashBuf31Seed0;
    procedure HashBuf32Seed0;
    procedure HashBuf35Seed0;
    procedure HashBuf36Seed0;
    procedure HashBuf39Seed0;
    procedure HashBuf40Seed0;
    procedure HashBuf41Seed0;

    procedure HashBuf0Seed1000001;
    procedure HashBuf1Seed1000001;
    procedure HashBuf2Seed1000001;
    procedure HashBuf3Seed1000001;
    procedure HashBuf4Seed1000001;
    procedure HashBuf5Seed1000001;
    procedure HashBuf8Seed1000001;
    procedure HashBuf12Seed1000001;
    procedure HashBuf15Seed1000001;
    procedure HashBuf16Seed1000001;
    procedure HashBuf17Seed1000001;
    procedure HashBuf21Seed1000001;
    procedure HashBuf25Seed1000001;
    procedure HashBuf31Seed1000001;
    procedure HashBuf32Seed1000001;
    procedure HashBuf35Seed1000001;
    procedure HashBuf36Seed1000001;
    procedure HashBuf39Seed1000001;
    procedure HashBuf40Seed1000001;
    procedure HashBuf41Seed1000001;

    procedure HashWord0Seed0;
    procedure HashWordSeed0;
    procedure HashWord0Seed1000001;
    procedure HashWordSeed1000001;

    procedure HashDWord0Seed0;
    procedure HashDWordSeed0;
    procedure HashDWord0Seed1000001;
    procedure HashDWordSeed1000001;

    procedure HashQWord0Seed0;
    procedure HashQWordSeed0;
    procedure HashQWord0Seed1000001;
    procedure HashQWordSeed1000001;

    procedure HashGuid0Seed0;
    procedure HashGuidSeed0;
    procedure HashGuid0Seed1000001;
    procedure HashGuidSeed1000001;
  end;


implementation
{$B-}{$COPERATORS ON}

function RandomString(aLen: Integer): string;
const
  CsSize = 64;
  CharSet: array[0..CsSize-1] of AnsiChar = '1234567890ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz_$';
var
  p: PAnsiChar;
begin
  SetLength(Result, aLen);
  p := Pointer(Result);
  while aLen > 0 do
    begin
      p^ := CharSet[Random(CsSize)];
      Inc(p);
      Dec(aLen);
    end;
end;

procedure TxxHash32Test.HashBuf0Seed0;
var
  h: DWord;
  s: ansistring;
const
  hash = $02CC5D05;
begin
  s := '';
  h := TxxHash32LE.HashBuf(PAnsiChar(s), System.Length(s));
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TxxHash32Test.HashBuf1Seed0;
var
  h: DWord;
  s: string;
const
  hash = $550D7456;
begin
  s := 'a';
  h := TxxHash32LE.HashBuf(Pointer(s), System.Length(s));
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TxxHash32Test.HashBuf2Seed0;
var
  h: DWord;
  s: string;
const
  hash = $4999FC53;
begin
  s := 'ab';
  h := TxxHash32LE.HashBuf(Pointer(s), System.Length(s));
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TxxHash32Test.HashBuf3Seed0;
var
  h: DWord;
  s: string;
const
  hash = $32D153FF;
begin
  s := 'abc';
  h := TxxHash32LE.HashBuf(Pointer(s), System.Length(s));
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TxxHash32Test.HashBuf4Seed0;
var
  h: DWord;
  s: string;
const
  hash = $A3643705;
begin
  s := 'abcd';
  h := TxxHash32LE.HashBuf(Pointer(s), System.Length(s));
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TxxHash32Test.HashBuf5Seed0;
var
  h: DWord;
  s: string;
const
  hash = $25FEF410;
begin
  s := 'abcdb';
  h := TxxHash32LE.HashBuf(Pointer(s), System.Length(s));
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TxxHash32Test.HashBuf8Seed0;
var
  h: DWord;
  s: string;
const
  hash = $C2212455;
begin
  s := 'abcdbcde';
  h := TxxHash32LE.HashBuf(Pointer(s), System.Length(s));
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TxxHash32Test.HashBuf12Seed0;
var
  h: DWord;
  s: string;
const
  hash = $B87F108E;
begin
  s := 'abcdbcdecdef';
  h := TxxHash32LE.HashBuf(Pointer(s), System.Length(s));
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TxxHash32Test.HashBuf15Seed0;
var
  h: DWord;
  s: string;
const
  hash = $5FE92951;
begin
  s := 'abcdbcdecdefdef';
  h := TxxHash32LE.HashBuf(Pointer(s), System.Length(s));
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TxxHash32Test.HashBuf16Seed0;
var
  h: DWord;
  s: string;
const
  hash = $8D448C00;
begin
  s := 'abcdbcdecdefdefg';
  h := TxxHash32LE.HashBuf(Pointer(s), System.Length(s));
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TxxHash32Test.HashBuf17Seed0;
var
  h: DWord;
  s: string;
const
  hash = $905AF0F9;
begin
  s := 'abcdbcdecdefdefge';
  h := TxxHash32LE.HashBuf(Pointer(s), System.Length(s));
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TxxHash32Test.HashBuf21Seed0;
var
  h: DWord;
  s: string;
const
  hash = $5E611CEE;
begin
  s := 'abcdbcdecdefdefgefghf';
  h := TxxHash32LE.HashBuf(Pointer(s), System.Length(s));
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TxxHash32Test.HashBuf25Seed0;
var
  h: DWord;
  s: string;
const
  hash = $3946382F;
begin
  s := 'abcdbcdecdefdefgefghfghig';
  h := TxxHash32LE.HashBuf(Pointer(s), System.Length(s));
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TxxHash32Test.HashBuf31Seed0;
var
  h: DWord;
  s: string;
const
  hash = $0F448874;
begin
  s := 'abcdbcdecdefdefgefghfghighijhij';
  h := TxxHash32LE.HashBuf(Pointer(s), System.Length(s));
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TxxHash32Test.HashBuf0Seed1000001;
var
  h: DWord;
  s: string;
const
  seed = 1000001;
  hash = $256EE7A6;
begin
  s := '';
  h := TxxHash32LE.HashBuf(Pointer(s), System.Length(s), seed);
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TxxHash32Test.HashBuf1Seed1000001;
var
  h: DWord;
  s: string;
const
  seed = 1000001;
  hash = $FB53763A;
begin
  s := 'a';
  h := TxxHash32LE.HashBuf(Pointer(s), System.Length(s), seed);
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TxxHash32Test.HashBuf2Seed1000001;
var
  h: DWord;
  s: string;
const
  seed = 1000001;
  hash = $4FA0728C;
begin
  s := 'ab';
  h := TxxHash32LE.HashBuf(Pointer(s), System.Length(s), seed);
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TxxHash32Test.HashBuf3Seed1000001;
var
  h: DWord;
  s: string;
const
  seed = 1000001;
  hash = $FB9506AD;
begin
  s := 'abc';
  h := TxxHash32LE.HashBuf(Pointer(s), System.Length(s), seed);
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TxxHash32Test.HashBuf4Seed1000001;
var
  h: DWord;
  s: string;
const
  seed = 1000001;
  hash = $BBA1E1FC;
begin
  s := 'abcd';
  h := TxxHash32LE.HashBuf(Pointer(s), System.Length(s), seed);
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TxxHash32Test.HashBuf5Seed1000001;
var
  h: DWord;
  s: string;
const
  seed = 1000001;
  hash = $1CA4FAB5;
begin
  s := 'abcdb';
  h := TxxHash32LE.HashBuf(Pointer(s), System.Length(s), seed);
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TxxHash32Test.HashBuf8Seed1000001;
var
  h: DWord;
  s: string;
const
  seed = 1000001;
  hash = $16C642B7;
begin
  s := 'abcdbcde';
  h := TxxHash32LE.HashBuf(Pointer(s), System.Length(s), seed);
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TxxHash32Test.HashBuf12Seed1000001;
var
  h: DWord;
  s: string;
const
  seed = 1000001;
  hash = $10AD24B8;
begin
  s := 'abcdbcdecdef';
  h := TxxHash32LE.HashBuf(Pointer(s), System.Length(s), seed);
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TxxHash32Test.HashBuf15Seed1000001;
var
  h: DWord;
  s: string;
const
  seed = 1000001;
  hash = $F2D90C0D;
begin
  s := 'abcdbcdecdefdef';
  h := TxxHash32LE.HashBuf(Pointer(s), System.Length(s), seed);
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TxxHash32Test.HashBuf16Seed1000001;
var
  h: DWord;
  s: string;
const
  seed = 1000001;
  hash = $E6F70F6E;
begin
  s := 'abcdbcdecdefdefg';
  h := TxxHash32LE.HashBuf(Pointer(s), System.Length(s), seed);
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TxxHash32Test.HashBuf17Seed1000001;
var
  h: DWord;
  s: string;
const
  seed = 1000001;
  hash = $248FF85D;
begin
  s := 'abcdbcdecdefdefge';
  h := TxxHash32LE.HashBuf(Pointer(s), System.Length(s), seed);
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TxxHash32Test.HashBuf21Seed1000001;
var
  h: DWord;
  s: string;
const
  seed = 1000001;
  hash = $E6EA3B8C;
begin
  s := 'abcdbcdecdefdefgefghf';
  h := TxxHash32LE.HashBuf(Pointer(s), System.Length(s), seed);
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TxxHash32Test.HashBuf25Seed1000001;
var
  h: DWord;
  s: string;
const
  seed = 1000001;
  hash = $18EFE9AE;
begin
  s := 'abcdbcdecdefdefgefghfghig';
  h := TxxHash32LE.HashBuf(Pointer(s), System.Length(s), seed);
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TxxHash32Test.HashBuf31Seed1000001;
var
  h: DWord;
  s: string;
const
  seed = 1000001;
  hash = $31B8DB2E;
begin
  s := 'abcdbcdecdefdefgefghfghighijhij';
  h := TxxHash32LE.HashBuf(Pointer(s), System.Length(s), seed);
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TxxHash32Test.HashWord0Seed0;
var
  h: DWord;
  w: Word;
const
  seed = 0;
  hash = $B5AA6AF5;
begin
  w := 0;
  h := TxxHash32LE.HashWord(w, seed);
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
  h := TxxHash32LE.HashWord(w);
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TxxHash32Test.HashWordSeed0;
var
  h: DWord;
  w: Word;
const
  seed = 0;
  hash = $7E26B838;
begin
  w := $2F63;
  h := TxxHash32LE.HashWord(w, seed);
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
  h := TxxHash32LE.HashWord(w);
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TxxHash32Test.HashWord0Seed1000001;
var
  h: DWord;
  w: Word;
const
  seed = 1000001;
  hash = $81A1D423;
begin
  w := 0;
  h := TxxHash32LE.HashWord(w, seed);
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TxxHash32Test.HashWordSeed1000001;
var
  h: DWord;
  w: Word;
const
  seed = 1000001;
  hash = $7319C7F8;
begin
  w := $2F63;
  h := TxxHash32LE.HashWord(w, seed);
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TxxHash32Test.HashDWord0Seed0;
var
  h: DWord;
  d: DWord;
const
  seed = 0;
  hash = $08D6D969;
begin
  d := 0;
  h := TxxHash32LE.HashDWord(d, seed);
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
  h := TxxHash32LE.HashDWord(d);
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TxxHash32Test.HashDWordSeed0;
var
  h: DWord;
  d: DWord;
const
  seed = 0;
  hash = $53DA062D;
begin
  d := $484F120C;
  h := TxxHash32LE.HashDWord(d, seed);
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
  h := TxxHash32LE.HashDWord(d);
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TxxHash32Test.HashDWord0Seed1000001;
var
  h: DWord;
  d: DWord;
const
  seed = 1000001;
  hash = $EF637702;
begin
  d := 0;
  h := TxxHash32LE.HashDWord(d, seed);
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TxxHash32Test.HashDWordSeed1000001;
var
  h: DWord;
  d: DWord;
const
  seed = 1000001;
  hash = $E64BEEEB;
begin
  d := $484F120C;
  h := TxxHash32LE.HashDWord(d, seed);
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TxxHash32Test.HashQWord0Seed0;
var
  h: DWord;
  q: QWord;
const
  seed = 0;
  hash = $DEB39513;
begin
  q := 0;
  h := TxxHash32LE.HashQWord(q, seed);
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
  h := TxxHash32LE.HashQWord(q);
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TxxHash32Test.HashQWordSeed0;
var
  h: DWord;
  q: QWord;
const
  seed = 0;
  hash = $7101F312;
begin
  q := $044F5885223A2B;
  h := TxxHash32LE.HashQWord(q, seed);
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
  h := TxxHash32LE.HashQWord(q);
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TxxHash32Test.HashQWord0Seed1000001;
var
  h: DWord;
  q: QWord;
const
  seed = 1000001;
  hash = $2C90FC90;
begin
  q := 0;
  h := TxxHash32LE.HashQWord(q, seed);
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TxxHash32Test.HashQWordSeed1000001;
var
  h: DWord;
  q: QWord;
const
  seed = 1000001;
  hash = $2A57FC36;
begin
  q := $044F5885223A2B;
  h := TxxHash32LE.HashQWord(q, seed);
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TxxHash32Test.HashGuid0Seed0;
var
  h: DWord;
  g: TGuid;
const
  seed = 0;
  hash = $8E022B3A;
begin
  g := StringToGuid('{00000000-0000-0000-0000-000000000000}');
  h := TxxHash32LE.HashGuid(g, seed);
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
  h := TxxHash32LE.HashGuid(g);
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TxxHash32Test.HashGuidSeed0;
var
  h: DWord;
  g: TGuid;
const
  seed = 0;
  hash = $7CBC8805;
begin
  g := StringToGuid('{8D60E51C-0FEA-4D45-9644-4C4AFC4FC22F}');
  h := TxxHash32LE.HashGuid(g, seed);
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
  h := TxxHash32LE.HashGuid(g);
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TxxHash32Test.HashGuid0Seed1000001;
var
  h: DWord;
  g: TGuid;
const
  seed = 1000001;
  hash = $4B4C33FB;
begin
  g := StringToGuid('{00000000-0000-0000-0000-000000000000}');
  h := TxxHash32LE.HashGuid(g, seed);
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TxxHash32Test.HashGuidSeed1000001;
var
  h: DWord;
  g: TGuid;
const
  seed = 1000001;
  hash = $372157CA;
begin
  g := StringToGuid('{8D60E51C-0FEA-4D45-9644-4C4AFC4FC22F}');
  h := TxxHash32LE.HashGuid(g, seed);
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TxxHash32Test.Incremental;
const
  MIN_LEN     = 2;
  LEN_RANGE   = 13;
  MIN_COUNT   = 2;
  COUNT_RANGE = 10;
  TEST_SIZE   = 1000;
var
  h: TIncXxHash32LE;
  a: TStringArray;
  s: string;
  I, J: Integer;
  Seed: DWord;
begin
  for I := 1 to TEST_SIZE do
    begin
      Seed := DWord(Random(MaxInt));
      SetLength(a, Random(COUNT_RANGE) + MIN_COUNT);
      for J := 0 to High(a) do
        a[J] := RandomString(Random(LEN_RANGE) + MIN_LEN);
      h.Reset(Seed);
      for s in a do
        h.Add(Pointer(s)^, Length(s));
      AssertTrue(h.Hash = TxxHash32LE.HashStr(string.Join('', a), Seed));
    end;
end;

procedure TxxHash64Test.HashBuf0Seed0;
var
  h: QWord;
  s: ansistring;
const
  seed = 0;
  hash: QWord = QWord($EF46DB3751D8E999);
begin
  s := '';
  h := TxxHash64LE.HashBuf(PAnsiChar(s), System.Length(s), seed);
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TxxHash64Test.HashBuf1Seed0;
var
  h: QWord;
  s: ansistring;
const
  seed = 0;
  hash: QWord = QWord($D24EC4F1A98C6E5B);
begin
  s := 'a';
  h := TxxHash64LE.HashBuf(PAnsiChar(s), System.Length(s), seed);
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TxxHash64Test.HashBuf2Seed0;
var
  h: QWord;
  s: ansistring;
const
  seed = 0;
  hash: QWord = QWord($65F708CA92D04A61);
begin
  s := 'ab';
  h := TxxHash64LE.HashBuf(PAnsiChar(s), System.Length(s), seed);
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TxxHash64Test.HashBuf3Seed0;
var
  h: QWord;
  s: ansistring;
const
  seed = 0;
  hash: QWord = QWord($44BC2CF5AD770999);
begin
  s := 'abc';
  h := TxxHash64LE.HashBuf(PAnsiChar(s), System.Length(s), seed);
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TxxHash64Test.HashBuf4Seed0;
var
  h: QWord;
  s: ansistring;
const
  seed = 0;
  hash: QWord = QWord($DE0327B0D25D92CC);
begin
  s := 'abcd';
  h := TxxHash64LE.HashBuf(PAnsiChar(s), System.Length(s), seed);
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TxxHash64Test.HashBuf5Seed0;
var
  h: QWord;
  s: ansistring;
const
  seed = 0;
  hash: QWord = QWord($6278A44A4B70C190);
begin
  s := 'abcdb';
  h := TxxHash64LE.HashBuf(PAnsiChar(s), System.Length(s), seed);
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TxxHash64Test.HashBuf8Seed0;
var
  h: QWord;
  s: ansistring;
const
  seed = 0;
  hash: QWord = QWord($119734EF51A67B76);
begin
  s := 'abcdbcde';
  h := TxxHash64LE.HashBuf(PAnsiChar(s), System.Length(s), seed);
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TxxHash64Test.HashBuf12Seed0;
var
  h: QWord;
  s: ansistring;
const
  seed = 0;
  hash: QWord = QWord($CAE0FD1AC1C1489D);
begin
  s := 'abcdbcdecdef';
  h := TxxHash64LE.HashBuf(PAnsiChar(s), System.Length(s), seed);
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TxxHash64Test.HashBuf15Seed0;
var
  h: QWord;
  s: ansistring;
const
  seed = 0;
  hash: QWord = QWord($F8C9A4D9F0CB7C16);
begin
  s := 'abcdbcdecdefdef';
  h := TxxHash64LE.HashBuf(PAnsiChar(s), System.Length(s), seed);
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TxxHash64Test.HashBuf16Seed0;
var
  h: QWord;
  s: ansistring;
const
  seed = 0;
  hash: QWord = QWord($11B91B9EC2B207E5);
begin
  s := 'abcdbcdecdefdefg';
  h := TxxHash64LE.HashBuf(PAnsiChar(s), System.Length(s), seed);
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TxxHash64Test.HashBuf17Seed0;
var
  h: QWord;
  s: ansistring;
const
  seed = 0;
  hash: QWord = QWord($3185CB85B105097E);
begin
  s := 'abcdbcdecdefdefge';
  h := TxxHash64LE.HashBuf(PAnsiChar(s), System.Length(s), seed);
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TxxHash64Test.HashBuf21Seed0;
var
  h: QWord;
  s: ansistring;
const
  seed = 0;
  hash: QWord = QWord($FFCA110AD4DCCD3C);
begin
  s := 'abcdbcdecdefdefgefghf';
  h := TxxHash64LE.HashBuf(PAnsiChar(s), System.Length(s), seed);
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TxxHash64Test.HashBuf25Seed0;
var
  h: QWord;
  s: ansistring;
const
  seed = 0;
  hash: QWord = QWord($DEB387A5DEF4C776);
begin
  s := 'abcdbcdecdefdefgefghfghig';
  h := TxxHash64LE.HashBuf(PAnsiChar(s), System.Length(s), seed);
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TxxHash64Test.HashBuf31Seed0;
var
  h: QWord;
  s: ansistring;
const
  seed = 0;
  hash: QWord = QWord($941DA75B496F8208);
begin
  s := 'abcdbcdecdefdefgefghfghighijhij';
  h := TxxHash64LE.HashBuf(PAnsiChar(s), System.Length(s), seed);
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TxxHash64Test.HashBuf32Seed0;
var
  h: QWord;
  s: ansistring;
const
  seed = 0;
  hash: QWord = QWord($F35AB6D9F7ECDF06);
begin
  s := 'abcdbcdecdefdefgefghfghighijhijk';
  h := TxxHash64LE.HashBuf(PAnsiChar(s), System.Length(s), seed);
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TxxHash64Test.HashBuf35Seed0;
var
  h: QWord;
  s: ansistring;
const
  seed = 0;
  hash: QWord = QWord($8CED90A902E51AB0);
begin
  s := 'abcdbcdecdefdefgefghfghighijhijkijk';
  h := TxxHash64LE.HashBuf(PAnsiChar(s), System.Length(s), seed);
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TxxHash64Test.HashBuf36Seed0;
var
  h: QWord;
  s: ansistring;
const
  seed = 0;
  hash: QWord = QWord($B53CCF42376AD771);
begin
  s := 'abcdbcdecdefdefgefghfghighijhijkijkl';
  h := TxxHash64LE.HashBuf(PAnsiChar(s), System.Length(s), seed);
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TxxHash64Test.HashBuf39Seed0;
var
  h: QWord;
  s: ansistring;
const
  seed = 0;
  hash: QWord = QWord($134EDF29A5DD429D);
begin
  s := 'abcdbcdecdefdefgefghfghighijhijkijkljkl';
  h := TxxHash64LE.HashBuf(PAnsiChar(s), System.Length(s), seed);
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TxxHash64Test.HashBuf40Seed0;
var
  h: QWord;
  s: ansistring;
const
  seed = 0;
  hash: QWord = QWord($5DE8B076AD09B0C5);
begin
  s := 'abcdbcdecdefdefgefghfghighijhijkijkljklm';
  h := TxxHash64LE.HashBuf(PAnsiChar(s), System.Length(s), seed);
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TxxHash64Test.HashBuf41Seed0;
var
  h: QWord;
  s: ansistring;
const
  seed = 0;
  hash: QWord = QWord($710472A8EEBF5118);
begin
  s := 'abcdbcdecdefdefgefghfghighijhijkijkljklmk';
  h := TxxHash64LE.HashBuf(PAnsiChar(s), System.Length(s), seed);
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TxxHash64Test.HashBuf0Seed1000001;
var
  h: QWord;
  s: ansistring;
const
  seed = 1000001;
  hash: QWord = QWord($BE60AAA9B78ECCA6);
begin
  s := '';
  h := TxxHash64LE.HashBuf(PAnsiChar(s), System.Length(s), seed);
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TxxHash64Test.HashBuf1Seed1000001;
var
  h: QWord;
  s: ansistring;
const
  seed = 1000001;
  hash: QWord = QWord($A5A0A4182639F283);
begin
  s := 'a';
  h := TxxHash64LE.HashBuf(PAnsiChar(s), System.Length(s), seed);
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TxxHash64Test.HashBuf2Seed1000001;
var
  h: QWord;
  s: ansistring;
const
  seed = 1000001;
  hash: QWord = QWord($45478FD10F96E07B);
begin
  s := 'ab';
  h := TxxHash64LE.HashBuf(PAnsiChar(s), System.Length(s), seed);
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TxxHash64Test.HashBuf3Seed1000001;
var
  h: QWord;
  s: ansistring;
const
  seed = 1000001;
  hash: QWord = QWord($6AF08B0352AED077);
begin
  s := 'abc';
  h := TxxHash64LE.HashBuf(PAnsiChar(s), System.Length(s), seed);
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TxxHash64Test.HashBuf4Seed1000001;
var
  h: QWord;
  s: ansistring;
const
  seed = 1000001;
  hash: QWord = QWord($53B79852583B2E2E);
begin
  s := 'abcd';
  h := TxxHash64LE.HashBuf(PAnsiChar(s), System.Length(s), seed);
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TxxHash64Test.HashBuf5Seed1000001;
var
  h: QWord;
  s: ansistring;
const
  seed = 1000001;
  hash: QWord = QWord($E861DC2F28ED8B38);
begin
  s := 'abcdb';
  h := TxxHash64LE.HashBuf(PAnsiChar(s), System.Length(s), seed);
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TxxHash64Test.HashBuf8Seed1000001;
var
  h: QWord;
  s: ansistring;
const
  seed = 1000001;
  hash: QWord = QWord($16EE8B3BAB9CABEF);
begin
  s := 'abcdbcde';
  h := TxxHash64LE.HashBuf(PAnsiChar(s), System.Length(s), seed);
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TxxHash64Test.HashBuf12Seed1000001;
var
  h: QWord;
  s: ansistring;
const
  seed = 1000001;
  hash: QWord = QWord($239934F6F17E2708);
begin
  s := 'abcdbcdecdef';
  h := TxxHash64LE.HashBuf(PAnsiChar(s), System.Length(s), seed);
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TxxHash64Test.HashBuf15Seed1000001;
var
  h: QWord;
  s: ansistring;
const
  seed = 1000001;
  hash: QWord = QWord($8F49C92E50384201);
begin
  s := 'abcdbcdecdefdef';
  h := TxxHash64LE.HashBuf(PAnsiChar(s), System.Length(s), seed);
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TxxHash64Test.HashBuf16Seed1000001;
var
  h: QWord;
  s: ansistring;
const
  seed = 1000001;
  hash: QWord = QWord($1699425BD87C36F4);
begin
  s := 'abcdbcdecdefdefg';
  h := TxxHash64LE.HashBuf(PAnsiChar(s), System.Length(s), seed);
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TxxHash64Test.HashBuf17Seed1000001;
var
  h: QWord;
  s: ansistring;
const
  seed = 1000001;
  hash: QWord = QWord($823E73FFD1786E3A);
begin
  s := 'abcdbcdecdefdefge';
  h := TxxHash64LE.HashBuf(PAnsiChar(s), System.Length(s), seed);
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TxxHash64Test.HashBuf21Seed1000001;
var
  h: QWord;
  s: ansistring;
const
  seed = 1000001;
  hash: QWord = QWord($1F29C995222FDC9A);
begin
  s := 'abcdbcdecdefdefgefghf';
  h := TxxHash64LE.HashBuf(PAnsiChar(s), System.Length(s), seed);
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TxxHash64Test.HashBuf25Seed1000001;
var
  h: QWord;
  s: ansistring;
const
  seed = 1000001;
  hash: QWord = QWord($C483D186EA72DE8E);
begin
  s := 'abcdbcdecdefdefgefghfghig';
  h := TxxHash64LE.HashBuf(PAnsiChar(s), System.Length(s), seed);
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TxxHash64Test.HashBuf31Seed1000001;
var
  h: QWord;
  s: ansistring;
const
  seed = 1000001;
  hash: QWord = QWord($97E0035EDC062184);
begin
  s := 'abcdbcdecdefdefgefghfghighijhij';
  h := TxxHash64LE.HashBuf(PAnsiChar(s), System.Length(s), seed);
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TxxHash64Test.HashBuf32Seed1000001;
var
  h: QWord;
  s: ansistring;
const
  seed = 1000001;
  hash: QWord = QWord($2421C55768CE3EEC);
begin
  s := 'abcdbcdecdefdefgefghfghighijhijk';
  h := TxxHash64LE.HashBuf(PAnsiChar(s), System.Length(s), seed);
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TxxHash64Test.HashBuf35Seed1000001;
var
  h: QWord;
  s: ansistring;
const
  seed = 1000001;
  hash: QWord = QWord($35D7E8397592EDF4);
begin
  s := 'abcdbcdecdefdefgefghfghighijhijkijk';
  h := TxxHash64LE.HashBuf(PAnsiChar(s), System.Length(s), seed);
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TxxHash64Test.HashBuf36Seed1000001;
var
  h: QWord;
  s: ansistring;
const
  seed = 1000001;
  hash: QWord = QWord($54CF3B8E0F2DD963);
begin
  s := 'abcdbcdecdefdefgefghfghighijhijkijkl';
  h := TxxHash64LE.HashBuf(PAnsiChar(s), System.Length(s), seed);
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TxxHash64Test.HashBuf39Seed1000001;
var
  h: QWord;
  s: ansistring;
const
  seed = 1000001;
  hash: QWord = QWord($64D416C21BD81D4F);
begin
  s := 'abcdbcdecdefdefgefghfghighijhijkijkljkl';
  h := TxxHash64LE.HashBuf(PAnsiChar(s), System.Length(s), seed);
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TxxHash64Test.HashBuf40Seed1000001;
var
  h: QWord;
  s: ansistring;
const
  seed = 1000001;
  hash: QWord = QWord($43AA9351F5853414);
begin
  s := 'abcdbcdecdefdefgefghfghighijhijkijkljklm';
  h := TxxHash64LE.HashBuf(PAnsiChar(s), System.Length(s), seed);
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TxxHash64Test.HashBuf41Seed1000001;
var
  h: QWord;
  s: ansistring;
const
  seed = 1000001;
  hash: QWord = QWord($F67EE414D9DC7229);
begin
  s := 'abcdbcdecdefdefgefghfghighijhijkijkljklmk';
  h := TxxHash64LE.HashBuf(PAnsiChar(s), System.Length(s), seed);
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TxxHash64Test.HashWord0Seed0;
var
  h: QWord;
  w: Word;
const
  seed = 0;
  hash: QWord = QWord($9AABA41FFA2DA101);
begin
  w := 0;
  h := TxxHash64LE.HashWord(w, seed);
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TxxHash64Test.HashWordSeed0;
var
  h: QWord;
  w: Word;
const
  seed = 0;
  hash: QWord = QWord($285EA3B47F528F99);
begin
  w := $2F63;
  h := TxxHash64LE.HashWord(w, seed);
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TxxHash64Test.HashWord0Seed1000001;
var
  h: QWord;
  w: Word;
const
  seed = 1000001;
  hash: QWord = QWord($13A1EBC31CA63B83);
begin
  w := 0;
  h := TxxHash64LE.HashWord(w, seed);
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TxxHash64Test.HashWordSeed1000001;
var
  h: QWord;
  w: Word;
const
  seed = 1000001;
  hash: QWord = QWord($D9E4A945A260FFFB);
begin
  w := $2F63;
  h := TxxHash64LE.HashWord(w, seed);
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TxxHash64Test.HashDWord0Seed0;
var
  h: QWord;
  d: DWord;
const
  seed = 0;
  hash: QWord = QWord($3AEFA6FD5CF2DEB4);
begin
  d := 0;
  h := TxxHash64LE.HashDWord(d, seed);
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TxxHash64Test.HashDWordSeed0;
var
  h: QWord;
  d: DWord;
const
  seed = 0;
  hash: QWord = QWord($64D1CDBBCF0CF2F3);
begin
  d := $484F120C;
  h := TxxHash64LE.HashDWord(d, seed);
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TxxHash64Test.HashDWord0Seed1000001;
var
  h: QWord;
  d: DWord;
const
  seed = 1000001;
  hash: QWord = QWord($AA2E7654CC6D3EA5);
begin
  d := 0;
  h := TxxHash64LE.HashDWord(d, seed);
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TxxHash64Test.HashDWordSeed1000001;
var
  h: QWord;
  d: DWord;
const
  seed = 1000001;
  hash: QWord = QWord($56F737125701A42D);
begin
  d := $484F120C;
  h := TxxHash64LE.HashDWord(d, seed);
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TxxHash64Test.HashQWord0Seed0;
var
  h: QWord;
  q: QWord;
const
  seed = 0;
  hash: QWord = QWord($34C96ACDCADB1BBB);
begin
  q := 0;
  h := TxxHash64LE.HashQWord(q, seed);
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TxxHash64Test.HashQWordSeed0;
var
  h: QWord;
  q: QWord;
const
  seed = 0;
  hash: QWord = QWord($0AC227EC4CD60AD5);
begin
  q := $00044F5885223A2B;
  h := TxxHash64LE.HashQWord(q, seed);
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TxxHash64Test.HashQWord0Seed1000001;
var
  h: QWord;
  q: QWord;
const
  seed = 1000001;
  hash: QWord = QWord($4B31AF9B1A94C53F);
begin
  q := 0;
  h := TxxHash64LE.HashQWord(q, seed);
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TxxHash64Test.HashQWordSeed1000001;
var
  h: QWord;
  q: QWord;
const
  seed = 1000001;
  hash: QWord = QWord($9A023ABCAB274DDF);
begin
  q := $00044F5885223A2B;
  h := TxxHash64LE.HashQWord(q, seed);
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TxxHash64Test.HashGuid0Seed0;
var
  h: QWord;
  g: TGuid;
const
  seed = 0;
  hash: QWord = QWord($AF09F71516247C32);
begin
  g := StringToGuid('{00000000-0000-0000-0000-000000000000}');
  h := TxxHash64LE.HashGuid(g, seed);
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TxxHash64Test.HashGuidSeed0;
var
  h: QWord;
  g: TGuid;
const
  seed = 0;
  hash: QWord = QWord($C0F8FD94418E7997);
begin
  g := StringToGuid('{8D60E51C-0FEA-4D45-9644-4C4AFC4FC22F}');
  h := TxxHash64LE.HashGuid(g, seed);
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TxxHash64Test.HashGuid0Seed1000001;
var
  h: QWord;
  g: TGuid;
const
  seed = 1000001;
  hash: QWord = QWord($ACE99E87E1C14DD3);
begin
  g := StringToGuid('{00000000-0000-0000-0000-000000000000}');
  h := TxxHash64LE.HashGuid(g, seed);
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TxxHash64Test.HashGuidSeed1000001;
var
  h: QWord;
  g: TGuid;
const
  seed = 1000001;
  hash: QWord = QWord($ECF6DF25D7DC8928);
begin
  g := StringToGuid('{8D60E51C-0FEA-4D45-9644-4C4AFC4FC22F}');
  h := TxxHash64LE.HashGuid(g, seed);
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TxxHash64Test.Incremental;
const
  MIN_LEN     = 2;
  LEN_RANGE   = 13;
  MIN_COUNT   = 2;
  COUNT_RANGE = 10;
  TEST_SIZE   = 1000;
var
  h: TIncXxHash64LE;
  a: TStringArray;
  s: string;
  I, J: Integer;
  Seed: QWord;
begin
  for I := 1 to TEST_SIZE do
    begin
      Seed := QWord(Random(MaxInt));
      SetLength(a, Random(COUNT_RANGE) + MIN_COUNT);
      for J := 0 to High(a) do
        a[J] := RandomString(Random(LEN_RANGE) + MIN_LEN);
      h.Reset(Seed);
      for s in a do
        h.Add(Pointer(s)^, Length(s));
      AssertTrue(h.Hash = TxxHash64LE.HashStr(string.Join('', a), Seed));
    end;
end;

{ TMurmur2Test }

procedure TMurmur2Test.HashBuf0Seed0;
var
  h: DWord;
  s: ansistring;
const
  hash = 0;
begin
  s := '';
  h := TMurmur2LE.HashBuf(Pointer(s), System.Length(s));
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TMurmur2Test.HashBuf1Seed0;
var
  h: DWord;
  s: ansistring;
const
  hash = $92685F5E;
begin
  s := 'a';
  h := TMurmur2LE.HashBuf(Pointer(s), System.Length(s));
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TMurmur2Test.HashBuf2Seed0;
var
  h: DWord;
  s: ansistring;
const
  hash = $1AA14063;
begin
  s := 'ab';
  h := TMurmur2LE.HashBuf(Pointer(s), System.Length(s));
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TMurmur2Test.HashBuf3Seed0;
var
  h: DWord;
  s: ansistring;
const
  hash = $13577C9B;
begin
  s := 'abc';
  h := TMurmur2LE.HashBuf(Pointer(s), System.Length(s));
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TMurmur2Test.HashBuf4Seed0;
var
  h: DWord;
  s: ansistring;
const
  hash = $26873021;
begin
  s := 'abcd';
  h := TMurmur2LE.HashBuf(Pointer(s), System.Length(s));
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TMurmur2Test.HashBuf5Seed0;
var
  h: DWord;
  s: ansistring;
const
  hash = $A9EEA631;
begin
  s := 'abcdb';
  h := TMurmur2LE.HashBuf(Pointer(s), System.Length(s));
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TMurmur2Test.HashBuf8Seed0;
var
  h: DWord;
  s: ansistring;
const
  hash = $4E1F9EFF;
begin
  s := 'abcdbcde';
  h := TMurmur2LE.HashBuf(Pointer(s), System.Length(s));
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TMurmur2Test.HashBuf12Seed0;
var
  h: DWord;
  s: ansistring;
const
  hash = $977A18EB;
begin
  s := 'abcdbcdecdef';
  h := TMurmur2LE.HashBuf(Pointer(s), System.Length(s));
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TMurmur2Test.HashBuf15Seed0;
var
  h: DWord;
  s: ansistring;
const
  hash = $1D3C7681;
begin
  s := 'abcdbcdecdefdef';
  h := TMurmur2LE.HashBuf(Pointer(s), System.Length(s));
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TMurmur2Test.HashBuf16Seed0;
var
  h: DWord;
  s: ansistring;
const
  hash = $00C3F35E;
begin
  s := 'abcdbcdecdefdefg';
  h := TMurmur2LE.HashBuf(Pointer(s), System.Length(s));
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TMurmur2Test.HashBuf17Seed0;
var
  h: DWord;
  s: ansistring;
const
  hash = $295E234B;
begin
  s := 'abcdbcdecdefdefge';
  h := TMurmur2LE.HashBuf(Pointer(s), System.Length(s));
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TMurmur2Test.HashBuf21Seed0;
var
  h: DWord;
  s: ansistring;
const
  hash = $C1EF9BAC;
begin
  s := 'abcdbcdecdefdefgefghf';
  h := TMurmur2LE.HashBuf(Pointer(s), System.Length(s));
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TMurmur2Test.HashBuf25Seed0;
var
  h: DWord;
  s: ansistring;
const
  hash = $50B7A5D4;
begin
  s := 'abcdbcdecdefdefgefghfghig';
  h := TMurmur2LE.HashBuf(Pointer(s), System.Length(s));
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TMurmur2Test.HashBuf31Seed0;
var
  h: DWord;
  s: ansistring;
const
  hash = $109584D9;
begin
  s := 'abcdbcdecdefdefgefghfghighijhij';
  h := TMurmur2LE.HashBuf(Pointer(s), System.Length(s));
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TMurmur2Test.HashBuf0Seed1000001;
var
  h: DWord;
  s: string;
const
  seed = 1000001;
  hash = $F755D1FF;
begin
  s := '';
  h := TMurmur2LE.HashBuf(Pointer(s), System.Length(s), seed);
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TMurmur2Test.HashBuf1Seed1000001;
var
  h: DWord;
  s: string;
const
  seed = 1000001;
  hash = $CA992F04;
begin
  s := 'a';
  h := TMurmur2LE.HashBuf(Pointer(s), System.Length(s), seed);
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TMurmur2Test.HashBuf2Seed1000001;
var
  h: DWord;
  s: string;
const
  seed = 1000001;
  hash = $FC3F6D56;
begin
  s := 'ab';
  h := TMurmur2LE.HashBuf(Pointer(s), System.Length(s), seed);
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TMurmur2Test.HashBuf3Seed1000001;
var
  h: DWord;
  s: string;
const
  seed = 1000001;
  hash = $39A9831C;
begin
  s := 'abc';
  h := TMurmur2LE.HashBuf(Pointer(s), System.Length(s), seed);
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TMurmur2Test.HashBuf4Seed1000001;
var
  h: DWord;
  s: string;
const
  seed = 1000001;
  hash = $FCD5AF55;
begin
  s := 'abcd';
  h := TMurmur2LE.HashBuf(Pointer(s), System.Length(s), seed);
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TMurmur2Test.HashBuf5Seed1000001;
var
  h: DWord;
  s: string;
const
  seed = 1000001;
  hash = $57761F5B;
begin
  s := 'abcdb';
  h := TMurmur2LE.HashBuf(Pointer(s), System.Length(s), seed);
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TMurmur2Test.HashBuf8Seed1000001;
var
  h: DWord;
  s: string;
const
  seed = 1000001;
  hash = $365CFCA9;
begin
  s := 'abcdbcde';
  h := TMurmur2LE.HashBuf(Pointer(s), System.Length(s), seed);
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TMurmur2Test.HashBuf12Seed1000001;
var
  h: DWord;
  s: string;
const
  seed = 1000001;
  hash = $743BCAA1;
begin
  s := 'abcdbcdecdef';
  h := TMurmur2LE.HashBuf(Pointer(s), System.Length(s), seed);
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TMurmur2Test.HashBuf15Seed1000001;
var
  h: DWord;
  s: string;
const
  seed = 1000001;
  hash = $D044489F;
begin
  s := 'abcdbcdecdefdef';
  h := TMurmur2LE.HashBuf(Pointer(s), System.Length(s), seed);
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TMurmur2Test.HashBuf16Seed1000001;
var
  h: DWord;
  s: string;
const
  seed = 1000001;
  hash = $D9488D5C;
begin
  s := 'abcdbcdecdefdefg';
  h := TMurmur2LE.HashBuf(Pointer(s), System.Length(s), seed);
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TMurmur2Test.HashBuf17Seed1000001;
var
  h: DWord;
  s: string;
const
  seed = 1000001;
  hash = $93EBF7D8;
begin
  s := 'abcdbcdecdefdefge';
  h := TMurmur2LE.HashBuf(Pointer(s), System.Length(s), seed);
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TMurmur2Test.HashBuf21Seed1000001;
var
  h: DWord;
  s: string;
const
  seed = 1000001;
  hash = $918BF0D1;
begin
  s := 'abcdbcdecdefdefgefghf';
  h := TMurmur2LE.HashBuf(Pointer(s), System.Length(s), seed);
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TMurmur2Test.HashBuf25Seed1000001;
var
  h: DWord;
  s: string;
const
  seed = 1000001;
  hash = $97BFAC97;
begin
  s := 'abcdbcdecdefdefgefghfghig';
  h := TMurmur2LE.HashBuf(Pointer(s), System.Length(s), seed);
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TMurmur2Test.HashBuf31Seed1000001;
var
  h: DWord;
  s: string;
const
  seed = 1000001;
  hash = $86D833F0;
begin
  s := 'abcdbcdecdefdefgefghfghighijhij';
  h := TMurmur2LE.HashBuf(Pointer(s), System.Length(s), seed);
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TMurmur2Test.HashWord0Seed0;
var
  h: DWord;
  w: Word;
const
  seed = 0;
  hash = $D29EDD7A;
begin
  w := 0;
  h := TMurmur2LE.HashWord(w, seed);
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
  h := TMurmur2LE.HashWord(w);
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TMurmur2Test.HashWordSeed0;
var
  h: DWord;
  w: Word;
const
  seed = 0;
  hash = $4140A587;
begin
  w := $2F63;
  h := TMurmur2LE.HashWord(w, seed);
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
  h := TMurmur2LE.HashWord(w);
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TMurmur2Test.HashWord0Seed1000001;
var
  h: DWord;
  w: Word;
const
  seed = 1000001;
  hash = $DB45A597;
begin
  w := 0;
  h := TMurmur2LE.HashWord(w, seed);
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TMurmur2Test.HashWordSeed1000001;
var
  h: DWord;
  w: Word;
const
  seed = 1000001;
  hash = $AA0D3BB1;
begin
  w := $2F63;
  h := TMurmur2LE.HashWord(w, seed);
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TMurmur2Test.HashDWord0Seed0;
var
  h: DWord;
  d: DWord;
const
  seed = 0;
  hash = $B469B2CC;
begin
  d := 0;
  h := TMurmur2LE.HashDWord(d, seed);
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
  h := TMurmur2LE.HashDWord(d);
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TMurmur2Test.HashDWordSeed0;
var
  h: DWord;
  d: DWord;
const
  seed = 0;
  hash = $BADC8B51;
begin
  d := $484F120C;
  h := TMurmur2LE.HashDWord(d, seed);
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
  h := TMurmur2LE.HashDWord(d);
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TMurmur2Test.HashDWord0Seed1000001;
var
  h: DWord;
  d: DWord;
const
  seed = 1000001;
  hash = $3A5C71B7;
begin
  d := 0;
  h := TMurmur2LE.HashDWord(d, seed);
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TMurmur2Test.HashDWordSeed1000001;
var
  h: DWord;
  d: DWord;
const
  seed = 1000001;
  hash = $422A864C;
begin
  d := $484F120C;
  h := TMurmur2LE.HashDWord(d, seed);
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TMurmur2Test.HashQWord0Seed0;
var
  h: DWord;
  q: QWord;
const
  seed = 0;
  hash = $93B132BC;
begin
  q := 0;
  h := TMurmur2LE.HashQWord(q, seed);
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
  h := TMurmur2LE.HashQWord(q);
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TMurmur2Test.HashQWordSeed0;
var
  h: DWord;
  q: QWord;
const
  seed = 0;
  hash = $EBBA610F;
begin
  q := $044F5885223A2B;
  h := TMurmur2LE.HashQWord(q, seed);
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
  h := TMurmur2LE.HashQWord(q);
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TMurmur2Test.HashQWord0Seed1000001;
var
  h: DWord;
  q: QWord;
const
  seed = 1000001;
  hash = $BBC62D28;
begin
  q := 0;
  h := TMurmur2LE.HashQWord(q, seed);
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TMurmur2Test.HashQWordSeed1000001;
var
  h: DWord;
  q: QWord;
const
  seed = 1000001;
  hash = $5048E868;
begin
  q := $044F5885223A2B;
  h := TMurmur2LE.HashQWord(q, seed);
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TMurmur2Test.HashGuid0Seed0;
var
  h: DWord;
  g: TGuid;
const
  seed = 0;
  hash = $2018C55B;
begin
  g := StringToGuid('{00000000-0000-0000-0000-000000000000}');
  h := TMurmur2LE.HashGuid(g, seed);
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
  h := TMurmur2LE.HashGuid(g);
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TMurmur2Test.HashGuidSeed0;
var
  h: DWord;
  g: TGuid;
const
  seed = 0;
  hash = $566B7774;
begin
  g := StringToGuid('{8D60E51C-0FEA-4D45-9644-4C4AFC4FC22F}');
  h := TMurmur2LE.HashGuid(g, seed);
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
  h := TMurmur2LE.HashGuid(g);
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TMurmur2Test.HashGuid0Seed1000001;
var
  h: DWord;
  g: TGuid;
const
  seed = 1000001;
  hash = $284EBEF0;
begin
  g := StringToGuid('{00000000-0000-0000-0000-000000000000}');
  h := TMurmur2LE.HashGuid(g, seed);
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TMurmur2Test.HashGuidSeed1000001;
var
  h: DWord;
  g: TGuid;
const
  seed = 1000001;
  hash = $3930A2C1;
begin
  g := StringToGuid('{8D60E51C-0FEA-4D45-9644-4C4AFC4FC22F}');
  h := TMurmur2LE.HashGuid(g, seed);
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

{ TMurmur2ATest }

procedure TMurmur2ATest.HashBuf0Seed0;
var
  h: DWord;
  s: ansistring;
const
  hash = $00000000;
begin
  s := '';
  h := TMurmur2aLE.HashBuf(Pointer(s), System.Length(s));
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TMurmur2ATest.HashBuf1Seed0;
var
  h: DWord;
  s: ansistring;
const
  hash = $0803888B;
begin
  s := 'a';
  h := TMurmur2aLE.HashBuf(Pointer(s), System.Length(s));
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TMurmur2ATest.HashBuf2Seed0;
var
  h: DWord;
  s: ansistring;
const
  hash = $618515AF;
begin
  s := 'ab';
  h := TMurmur2aLE.HashBuf(PAnsiChar(s), System.Length(s));
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TMurmur2ATest.HashBuf3Seed0;
var
  h: DWord;
  s: ansistring;
const
  hash = $11589F67;
begin
  s := 'abc';
  h := TMurmur2aLE.HashBuf(Pointer(s), System.Length(s));
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TMurmur2ATest.HashBuf4Seed0;
var
  h: DWord;
  s: ansistring;
const
  hash = $5C193C47;
begin
  s := 'abcd';
  h := TMurmur2aLE.HashBuf(PAnsiChar(s), System.Length(s));
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TMurmur2ATest.HashBuf5Seed0;
var
  h: DWord;
  s: ansistring;
const
  hash = $45B0A26E;
begin
  s := 'abcdb';
  h := TMurmur2aLE.HashBuf(Pointer(s), System.Length(s));
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TMurmur2ATest.HashBuf8Seed0;
var
  h: DWord;
  s: ansistring;
const
  hash = $47A56DB9;
begin
  s := 'abcdbcde';
  h := TMurmur2aLE.HashBuf(Pointer(s), System.Length(s));
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TMurmur2ATest.HashBuf12Seed0;
var
  h: DWord;
  s: ansistring;
const
  hash = $C69D9F64;
begin
  s := 'abcdbcdecdef';
  h := TMurmur2aLE.HashBuf(Pointer(s), System.Length(s));
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TMurmur2ATest.HashBuf15Seed0;
var
  h: DWord;
  s: ansistring;
const
  hash = $1E63C7F2;
begin
  s := 'abcdbcdecdefdef';
  h := TMurmur2aLE.HashBuf(Pointer(s), System.Length(s));
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TMurmur2ATest.HashBuf16Seed0;
var
  h: DWord;
  s: ansistring;
const
  hash = $F9DE2CB8;
begin
  s := 'abcdbcdecdefdefg';
  h := TMurmur2aLE.HashBuf(Pointer(s), System.Length(s));
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TMurmur2ATest.HashBuf17Seed0;
var
  h: DWord;
  s: ansistring;
const
  hash = $6508B70C;
begin
  s := 'abcdbcdecdefdefge';
  h := TMurmur2aLE.HashBuf(Pointer(s), System.Length(s));
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TMurmur2ATest.HashBuf21Seed0;
var
  h: DWord;
  s: ansistring;
const
  hash = $3BAB71ED;
begin
  s := 'abcdbcdecdefdefgefghf';
  h := TMurmur2aLE.HashBuf(Pointer(s), System.Length(s));
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TMurmur2ATest.HashBuf25Seed0;
var
  h: DWord;
  s: ansistring;
const
  hash = $F603AF4F;
begin
  s := 'abcdbcdecdefdefgefghfghig';
  h := TMurmur2aLE.HashBuf(Pointer(s), System.Length(s));
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TMurmur2ATest.HashBuf31Seed0;
var
  h: DWord;
  s: ansistring;
const
  hash = $956FA78E;
begin
  s := 'abcdbcdecdefdefgefghfghighijhij';
  h := TMurmur2aLE.HashBuf(Pointer(s), System.Length(s));
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TMurmur2ATest.HashBuf0Seed1000001;
var
  h: DWord;
  s: ansistring;
const
  seed = 1000001;
  hash = $AEE79377;
begin
  s := '';
  h := TMurmur2aLE.HashBuf(Pointer(s), System.Length(s), seed);
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TMurmur2ATest.HashBuf1Seed1000001;
var
  h: DWord;
  s: ansistring;
const
  seed = 1000001;
  hash = $1BAD92A4;
begin
  s := 'a';
  h := TMurmur2aLE.HashBuf(Pointer(s), System.Length(s), seed);
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TMurmur2ATest.HashBuf2Seed1000001;
var
  h: DWord;
  s: ansistring;
const
  seed = 1000001;
  hash = $57FB4FA7;
begin
  s := 'ab';
  h := TMurmur2aLE.HashBuf(Pointer(s), System.Length(s), seed);
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TMurmur2ATest.HashBuf3Seed1000001;
var
  h: DWord;
  s: ansistring;
const
  seed = 1000001;
  hash = $30DDCDB6;
begin
  s := 'abc';
  h := TMurmur2aLE.HashBuf(Pointer(s), System.Length(s), seed);
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TMurmur2ATest.HashBuf4Seed1000001;
var
  h: DWord;
  s: ansistring;
const
  seed = 1000001;
  hash = $9ECFC889;
begin
  s := 'abcd';
  h := TMurmur2aLE.HashBuf(Pointer(s), System.Length(s), seed);
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TMurmur2ATest.HashBuf5Seed1000001;
var
  h: DWord;
  s: ansistring;
const
  seed = 1000001;
  hash = $8CCFB4F4;
begin
  s := 'abcdb';
  h := TMurmur2aLE.HashBuf(Pointer(s), System.Length(s), seed);
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TMurmur2ATest.HashBuf8Seed1000001;
var
  h: DWord;
  s: ansistring;
const
  seed = 1000001;
  hash = $A34F5C4B;
begin
  s := 'abcdbcde';
  h := TMurmur2aLE.HashBuf(Pointer(s), System.Length(s), seed);
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TMurmur2ATest.HashBuf12Seed1000001;
var
  h: DWord;
  s: ansistring;
const
  seed = 1000001;
  hash = $23D34E26;
begin
  s := 'abcdbcdecdef';
  h := TMurmur2aLE.HashBuf(Pointer(s), System.Length(s), seed);
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TMurmur2ATest.HashBuf15Seed1000001;
var
  h: DWord;
  s: ansistring;
const
  seed = 1000001;
  hash = $4E20B149;
begin
  s := 'abcdbcdecdefdef';
  h := TMurmur2aLE.HashBuf(Pointer(s), System.Length(s), seed);
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TMurmur2ATest.HashBuf16Seed1000001;
var
  h: DWord;
  s: ansistring;
const
  seed = 1000001;
  hash = $EA7342D7;
begin
  s := 'abcdbcdecdefdefg';
  h := TMurmur2aLE.HashBuf(Pointer(s), System.Length(s), seed);
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TMurmur2ATest.HashBuf17Seed1000001;
var
  h: DWord;
  s: ansistring;
const
  seed = 1000001;
  hash = $511CC4AD;
begin
  s := 'abcdbcdecdefdefge';
  h := TMurmur2aLE.HashBuf(Pointer(s), System.Length(s), seed);
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TMurmur2ATest.HashBuf21Seed1000001;
var
  h: DWord;
  s: ansistring;
const
  seed = 1000001;
  hash = $B5F96729;
begin
  s := 'abcdbcdecdefdefgefghf';
  h := TMurmur2aLE.HashBuf(Pointer(s), System.Length(s), seed);
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TMurmur2ATest.HashBuf25Seed1000001;
var
  h: DWord;
  s: ansistring;
const
  seed = 1000001;
  hash = $6CE03247;
begin
  s := 'abcdbcdecdefdefgefghfghig';
  h := TMurmur2aLE.HashBuf(Pointer(s), System.Length(s), seed);
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TMurmur2ATest.HashBuf31Seed1000001;
var
  h: DWord;
  s: ansistring;
const
  seed = 1000001;
  hash = $55D5F10C;
begin
  s := 'abcdbcdecdefdefgefghfghighijhij';
  h := TMurmur2aLE.HashBuf(Pointer(s), System.Length(s), seed);
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TMurmur2ATest.HashWord0Seed0;
var
  h: DWord;
  w: Word;
const
  seed = 0;
  hash = $AB332279;
begin
  w := 0;
  h := TMurmur2aLE.HashWord(w, seed);
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
  h := TMurmur2aLE.HashWord(w);
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TMurmur2ATest.HashWordSeed0;
var
  h: DWord;
  w: Word;
const
  seed = 0;
  hash = $6EB42EB7;
begin
  w := $2F63;
  h := TMurmur2aLE.HashWord(w, seed);
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
  h := TMurmur2aLE.HashWord(w);
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TMurmur2ATest.HashWord0Seed1000001;
var
  h: DWord;
  w: Word;
const
  seed = 1000001;
  hash = $170E064B;
begin
  w := 0;
  h := TMurmur2aLE.HashWord(w, seed);
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TMurmur2ATest.HashWordSeed1000001;
var
  h: DWord;
  w: Word;
const
  seed = 1000001;
  hash = $3454D956;
begin
  w := $2F63;
  h := TMurmur2aLE.HashWord(w, seed);
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TMurmur2ATest.HashDWord0Seed0;
var
  h: DWord;
  d: DWord;
const
  seed = 0;
  hash = $24A83904;
begin
  d := 0;
  h := TMurmur2aLE.HashDWord(d, seed);
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
  h := TMurmur2aLE.HashDWord(d);
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TMurmur2ATest.HashDWordSeed0;
var
  h: DWord;
  d: DWord;
const
  seed = 0;
  hash = $2EC7C475;
begin
  d := $484F120C;
  h := TMurmur2aLE.HashDWord(d, seed);
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
  h := TMurmur2aLE.HashDWord(d);
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TMurmur2ATest.HashDWord0Seed1000001;
var
  h: DWord;
  d: DWord;
const
  seed = 1000001;
  hash = $DF199652;
begin
  d := 0;
  h := TMurmur2aLE.HashDWord(d, seed);
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TMurmur2ATest.HashDWordSeed1000001;
var
  h: DWord;
  d: DWord;
const
  seed = 1000001;
  hash = $0D087B7A;
begin
  d := $484F120C;
  h := TMurmur2aLE.HashDWord(d, seed);
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TMurmur2ATest.HashQWord0Seed0;
var
  h: DWord;
  q: QWord;
const
  seed = 0;
  hash = $587A7AC8;
begin
  q := 0;
  h := TMurmur2aLE.HashQWord(q, seed);
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
  h := TMurmur2aLE.HashQWord(q);
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TMurmur2ATest.HashQWordSeed0;
var
  h: DWord;
  q: QWord;
const
  seed = 0;
  hash = $3CA884E5;
begin
  q := $044F5885223A2B;
  h := TMurmur2aLE.HashQWord(q, seed);
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
  h := TMurmur2aLE.HashQWord(q);
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TMurmur2ATest.HashQWord0Seed1000001;
var
  h: DWord;
  q: QWord;
const
  seed = 1000001;
  hash = $97F3B144;
begin
  q := 0;
  h := TMurmur2aLE.HashQWord(q, seed);
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TMurmur2ATest.HashQWordSeed1000001;
var
  h: DWord;
  q: QWord;
const
  seed = 1000001;
  hash = $53FF735E;
begin
  q := $044F5885223A2B;
  h := TMurmur2aLE.HashQWord(q, seed);
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TMurmur2ATest.HashGuid0Seed0;
var
  h: DWord;
  g: TGuid;
const
  seed = 0;
  hash = $C97BB883;
begin
  g := StringToGuid('{00000000-0000-0000-0000-000000000000}');
  h := TMurmur2aLE.HashGuid(g, seed);
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
  h := TMurmur2aLE.HashGuid(g);
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TMurmur2ATest.HashGuidSeed0;
var
  h: DWord;
  g: TGuid;
const
  seed = 0;
  hash = $8EB07608;
begin
  g := StringToGuid('{8D60E51C-0FEA-4D45-9644-4C4AFC4FC22F}');
  h := TMurmur2aLE.HashGuid(g, seed);
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
  h := TMurmur2aLE.HashGuid(g);
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TMurmur2ATest.HashGuid0Seed1000001;
var
  h: DWord;
  g: TGuid;
const
  seed = 1000001;
  hash = $8EB4948D;
begin
  g := StringToGuid('{00000000-0000-0000-0000-000000000000}');
  h := TMurmur2aLE.HashGuid(g, seed);
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TMurmur2ATest.HashGuidSeed1000001;
var
  h: DWord;
  g: TGuid;
const
  seed = 1000001;
  hash = $9889CCD6;
begin
  g := StringToGuid('{8D60E51C-0FEA-4D45-9644-4C4AFC4FC22F}');
  h := TMurmur2aLE.HashGuid(g, seed);
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

{ TMurmur3Test }

procedure TMurmur3Test.HashBuf0Seed0;
var
  h: DWord;
  s: ansistring;
const
  hash = $00000000;
begin
  s := '';
  h := TMurmur3LE.HashBuf(Pointer(s), System.Length(s));
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TMurmur3Test.HashBuf1Seed0;
var
  h: DWord;
  s: ansistring;
const
  hash = $3C2569B2;
begin
  s := 'a';
  h := TMurmur3LE.HashBuf(Pointer(s), System.Length(s));
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TMurmur3Test.HashBuf2Seed0;
var
  h: DWord;
  s: ansistring;
const
  hash = $9BBFD75F;
begin
  s := 'ab';
  h := TMurmur3LE.HashBuf(Pointer(s), System.Length(s));
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TMurmur3Test.HashBuf3Seed0;
var
  h: DWord;
  s: ansistring;
const
  hash = $B3DD93FA;
begin
  s := 'abc';
  h := TMurmur3LE.HashBuf(Pointer(s), System.Length(s));
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TMurmur3Test.HashBuf4Seed0;
var
  h: DWord;
  s: ansistring;
const
  hash = $43ED676A;
begin
  s := 'abcd';
  h := TMurmur3LE.HashBuf(Pointer(s), System.Length(s));
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TMurmur3Test.HashBuf5Seed0;
var
  h: DWord;
  s: ansistring;
const
  hash = $67AFB202;
begin
  s := 'abcdb';
  h := TMurmur3LE.HashBuf(Pointer(s), System.Length(s));
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TMurmur3Test.HashBuf8Seed0;
var
  h: DWord;
  s: ansistring;
const
  hash = $CAE0FDE2;
begin
  s := 'abcdbcde';
  h := TMurmur3LE.HashBuf(Pointer(s), System.Length(s));
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TMurmur3Test.HashBuf12Seed0;
var
  h: DWord;
  s: ansistring;
const
  hash = $0528BF34;
begin
  s := 'abcdbcdecdef';
  h := TMurmur3LE.HashBuf(Pointer(s), System.Length(s));
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TMurmur3Test.HashBuf15Seed0;
var
  h: DWord;
  s: ansistring;
const
  hash = $014D6EBE;
begin
  s := 'abcdbcdecdefdef';
  h := TMurmur3LE.HashBuf(Pointer(s), System.Length(s));
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TMurmur3Test.HashBuf16Seed0;
var
  h: DWord;
  s: ansistring;
const
  hash = $1EAC71F1;
begin
  s := 'abcdbcdecdefdefg';
  h := TMurmur3LE.HashBuf(Pointer(s), System.Length(s));
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TMurmur3Test.HashBuf17Seed0;
var
  h: DWord;
  s: ansistring;
const
  hash = $6A92CB80;
begin
  s := 'abcdbcdecdefdefge';
  h := TMurmur3LE.HashBuf(Pointer(s), System.Length(s));
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TMurmur3Test.HashBuf21Seed0;
var
  h: DWord;
  s: ansistring;
const
  hash = $1399D090;
begin
  s := 'abcdbcdecdefdefgefghf';
  h := TMurmur3LE.HashBuf(Pointer(s), System.Length(s));
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TMurmur3Test.HashBuf25Seed0;
var
  h: DWord;
  s: ansistring;
const
  hash = $89481C90;
begin
  s := 'abcdbcdecdefdefgefghfghig';
  h := TMurmur3LE.HashBuf(Pointer(s), System.Length(s));
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TMurmur3Test.HashBuf31Seed0;
var
  h: DWord;
  s: ansistring;
const
  hash = $FED7D92C;
begin
  s := 'abcdbcdecdefdefgefghfghighijhij';
  h := TMurmur3LE.HashBuf(Pointer(s), System.Length(s));
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TMurmur3Test.HashBuf0Seed1000001;
var
  h: DWord;
  s: ansistring;
const
  seed = 1000001;
  hash = $BBE298BA;
begin
  s := '';
  h := TMurmur3LE.HashBuf(Pointer(s), System.Length(s), seed);
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TMurmur3Test.HashBuf1Seed1000001;
var
  h: DWord;
  s: ansistring;
const
  seed = 1000001;
  hash = $A401C4F9;
begin
  s := 'a';
  h := TMurmur3LE.HashBuf(Pointer(s), System.Length(s), seed);
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TMurmur3Test.HashBuf2Seed1000001;
var
  h: DWord;
  s: ansistring;
const
  seed = 1000001;
  hash = $F23D57FF;
begin
  s := 'ab';
  h := TMurmur3LE.HashBuf(Pointer(s), System.Length(s), seed);
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TMurmur3Test.HashBuf3Seed1000001;
var
  h: DWord;
  s: ansistring;
const
  seed = 1000001;
  hash = $4785D127;
begin
  s := 'abc';
  h := TMurmur3LE.HashBuf(Pointer(s), System.Length(s), seed);
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TMurmur3Test.HashBuf4Seed1000001;
var
  h: DWord;
  s: ansistring;
const
  seed = 1000001;
  hash = $6CF9C034;
begin
  s := 'abcd';
  h := TMurmur3LE.HashBuf(Pointer(s), System.Length(s), seed);
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TMurmur3Test.HashBuf5Seed1000001;
var
  h: DWord;
  s: ansistring;
const
  seed = 1000001;
  hash = $142556C5;
begin
  s := 'abcdb';
  h := TMurmur3LE.HashBuf(Pointer(s), System.Length(s), seed);
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TMurmur3Test.HashBuf8Seed1000001;
var
  h: DWord;
  s: ansistring;
const
  seed = 1000001;
  hash = $3A9C35C6;
begin
  s := 'abcdbcde';
  h := TMurmur3LE.HashBuf(Pointer(s), System.Length(s), seed);
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TMurmur3Test.HashBuf12Seed1000001;
var
  h: DWord;
  s: ansistring;
const
  seed = 1000001;
  hash = $8724722F;
begin
  s := 'abcdbcdecdef';
  h := TMurmur3LE.HashBuf(Pointer(s), System.Length(s), seed);
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TMurmur3Test.HashBuf15Seed1000001;
var
  h: DWord;
  s: ansistring;
const
  seed = 1000001;
  hash = $CB962C6C;
begin
  s := 'abcdbcdecdefdef';
  h := TMurmur3LE.HashBuf(Pointer(s), System.Length(s), seed);
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TMurmur3Test.HashBuf16Seed1000001;
var
  h: DWord;
  s: ansistring;
const
  seed = 1000001;
  hash = $E68A108E;
begin
  s := 'abcdbcdecdefdefg';
  h := TMurmur3LE.HashBuf(Pointer(s), System.Length(s), seed);
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TMurmur3Test.HashBuf17Seed1000001;
var
  h: DWord;
  s: ansistring;
const
  seed = 1000001;
  hash = $4CA19843;
begin
  s := 'abcdbcdecdefdefge';
  h := TMurmur3LE.HashBuf(Pointer(s), System.Length(s), seed);
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TMurmur3Test.HashBuf21Seed1000001;
var
  h: DWord;
  s: ansistring;
const
  seed = 1000001;
  hash = $DE60A5D2;
begin
  s := 'abcdbcdecdefdefgefghf';
  h := TMurmur3LE.HashBuf(Pointer(s), System.Length(s), seed);
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TMurmur3Test.HashBuf25Seed1000001;
var
  h: DWord;
  s: ansistring;
const
  seed = 1000001;
  hash = $455193F3;
begin
  s := 'abcdbcdecdefdefgefghfghig';
  h := TMurmur3LE.HashBuf(Pointer(s), System.Length(s), seed);
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TMurmur3Test.HashBuf31Seed1000001;
var
  h: DWord;
  s: ansistring;
const
  seed = 1000001;
  hash = $456449C1;
begin
  s := 'abcdbcdecdefdefgefghfghighijhij';
  h := TMurmur3LE.HashBuf(Pointer(s), System.Length(s), seed);
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TMurmur3Test.HashWord0Seed0;
var
  h: DWord;
  w: Word;
const
  seed = 0;
  hash = $30F4C306;
begin
  w := 0;
  h := TMurmur3LE.HashWord(w, seed);
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
  h := TMurmur3LE.HashWord(w);
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TMurmur3Test.HashWordSeed0;
var
  h: DWord;
  w: Word;
const
  seed = 0;
  hash = $309F4CD2;
begin
  w := $2F63;
  h := TMurmur3LE.HashWord(w, seed);
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
  h := TMurmur3LE.HashWord(w);
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TMurmur3Test.HashWord0Seed1000001;
var
  h: DWord;
  w: Word;
const
  seed = 1000001;
  hash = $664C2E59;
begin
  w := 0;
  h := TMurmur3LE.HashWord(w, seed);
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TMurmur3Test.HashWordSeed1000001;
var
  h: DWord;
  w: Word;
const
  seed = 1000001;
  hash = $3C3C9E4D;
begin
  w := $2F63;
  h := TMurmur3LE.HashWord(w, seed);
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TMurmur3Test.HashDWord0Seed0;
var
  h: DWord;
  d: DWord;
const
  seed = 0;
  hash = $2362F9DE;
begin
  d := 0;
  h := TMurmur3LE.HashDWord(d, seed);
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
  h := TMurmur3LE.HashDWord(d);
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TMurmur3Test.HashDWordSeed0;
var
  h: DWord;
  d: DWord;
const
  seed = 0;
  hash = $35415238;
begin
  d := $484F120C;
  h := TMurmur3LE.HashDWord(d, seed);
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
  h := TMurmur3LE.HashDWord(d);
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TMurmur3Test.HashDWord0Seed1000001;
var
  h: DWord;
  d: DWord;
const
  seed = 1000001;
  hash = $06380A1A;
begin
  d := 0;
  h := TMurmur3LE.HashDWord(d, seed);
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TMurmur3Test.HashDWordSeed1000001;
var
  h: DWord;
  d: DWord;
const
  seed = 1000001;
  hash = $DF84B23E;
begin
  d := $484F120C;
  h := TMurmur3LE.HashDWord(d, seed);
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TMurmur3Test.HashQWord0Seed0;
var
  h: DWord;
  q: QWord;
const
  seed = 0;
  hash = $63852AFC;
begin
  q := 0;
  h := TMurmur3LE.HashQWord(q, seed);
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
  h := TMurmur3LE.HashQWord(q);
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TMurmur3Test.HashQWordSeed0;
var
  h: DWord;
  q: QWord;
const
  seed = 0;
  hash = $A3785B1C;
begin
  q := $044F5885223A2B;
  h := TMurmur3LE.HashQWord(q, seed);
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
  h := TMurmur3LE.HashQWord(q);
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TMurmur3Test.HashQWord0Seed1000001;
var
  h: DWord;
  q: QWord;
const
  seed = 1000001;
  hash = $24A47ED6;
begin
  q := 0;
  h := TMurmur3LE.HashQWord(q, seed);
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TMurmur3Test.HashQWordSeed1000001;
var
  h: DWord;
  q: QWord;
const
  seed = 1000001;
  hash = $AD404E09;
begin
  q := $044F5885223A2B;
  h := TMurmur3LE.HashQWord(q, seed);
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TMurmur3Test.HashGuid0Seed0;
var
  h: DWord;
  g: TGuid;
const
  seed = 0;
  hash = $8134CDF8;
begin
  g := StringToGuid('{00000000-0000-0000-0000-000000000000}');
  h := TMurmur3LE.HashGuid(g, seed);
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
  h := TMurmur3LE.HashGuid(g);
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TMurmur3Test.HashGuidSeed0;
var
  h: DWord;
  g: TGuid;
const
  seed = 0;
  hash = $2FF4B34A;
begin
  g := StringToGuid('{8D60E51C-0FEA-4D45-9644-4C4AFC4FC22F}');
  h := TMurmur3LE.HashGuid(g, seed);
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
  h := TMurmur3LE.HashGuid(g);
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TMurmur3Test.HashGuid0Seed1000001;
var
  h: DWord;
  g: TGuid;
const
  seed = 1000001;
  hash = $88AADD74;
begin
  g := StringToGuid('{00000000-0000-0000-0000-000000000000}');
  h := TMurmur3LE.HashGuid(g, seed);
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TMurmur3Test.HashGuidSeed1000001;
var
  h: DWord;
  g: TGuid;
const
  seed = 1000001;
  hash = $75CA6B02;
begin
  g := StringToGuid('{8D60E51C-0FEA-4D45-9644-4C4AFC4FC22F}');
  h := TMurmur3LE.HashGuid(g, seed);
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TMurmur3Test.Incremental;
const
  MIN_LEN     = 2;
  LEN_RANGE   = 13;
  MIN_COUNT   = 2;
  COUNT_RANGE = 10;
  TEST_SIZE   = 1000;
var
  h: TIncMurmur3LE;
  a: TStringArray;
  s: string;
  I, J: Integer;
  Seed: DWord;
begin
  for I := 1 to TEST_SIZE do
    begin
      Seed := DWord(Random(MaxInt));
      SetLength(a, Random(COUNT_RANGE) + MIN_COUNT);
      for J := 0 to High(a) do
        a[J] := RandomString(Random(LEN_RANGE) + MIN_LEN);
      h.Reset(Seed);
      for s in a do
        h.Add(Pointer(s)^, Length(s));
      AssertTrue(h.Hash = TMurmur3LE.HashStr(string.Join('', a), Seed));
    end;
end;

{ TMurmur64aTest }

procedure TMurmur64aTest.HashBuf0Seed0;
var
  h: QWord;
  s: ansistring;
const
  seed = 0;
  hash: QWord = QWord($0000000000000000);
begin
  s := '';
  h := TMurmur64aLE.HashBuf(Pointer(s), System.Length(s), seed);
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TMurmur64aTest.HashBuf1Seed0;
var
  h: QWord;
  s: ansistring;
const
  seed = 0;
  hash: QWord = QWord($071717D2D36B6B11);
begin
  s := 'a';
  h := TMurmur64aLE.HashBuf(Pointer(s), System.Length(s), seed);
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TMurmur64aTest.HashBuf2Seed0;
var
  h: QWord;
  s: ansistring;
const
  seed = 0;
  hash: QWord = QWord($62BE85B2FE53D1F8);
begin
  s := 'ab';
  h := TMurmur64aLE.HashBuf(Pointer(s), System.Length(s), seed);
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TMurmur64aTest.HashBuf3Seed0;
var
  h: QWord;
  s: ansistring;
const
  seed = 0;
  hash: QWord = QWord($9CC9C33498A95EFB);
begin
  s := 'abc';
  h := TMurmur64aLE.HashBuf(Pointer(s), System.Length(s), seed);
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TMurmur64aTest.HashBuf4Seed0;
var
  h: QWord;
  s: ansistring;
const
  seed = 0;
  hash: QWord = QWord($EC1044C45CC5097A);
begin
  s := 'abcd';
  h := TMurmur64aLE.HashBuf(Pointer(s), System.Length(s), seed);
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TMurmur64aTest.HashBuf5Seed0;
var
  h: QWord;
  s: ansistring;
const
  seed = 0;
  hash: QWord = QWord($BD1F53E64A0F4F68);
begin
  s := 'abcdb';
  h := TMurmur64aLE.HashBuf(Pointer(s), System.Length(s), seed);
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TMurmur64aTest.HashBuf8Seed0;
var
  h: QWord;
  s: ansistring;
const
  seed = 0;
  hash: QWord = QWord($A9EF962A27D6DEB7);
begin
  s := 'abcdbcde';
  h := TMurmur64aLE.HashBuf(Pointer(s), System.Length(s), seed);
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TMurmur64aTest.HashBuf12Seed0;
var
  h: QWord;
  s: ansistring;
const
  seed = 0;
  hash: QWord = QWord($36D79846E30F8699);
begin
  s := 'abcdbcdecdef';
  h := TMurmur64aLE.HashBuf(Pointer(s), System.Length(s), seed);
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TMurmur64aTest.HashBuf15Seed0;
var
  h: QWord;
  s: ansistring;
const
  seed = 0;
  hash: QWord = QWord($0A7945813DFEC848);
begin
  s := 'abcdbcdecdefdef';
  h := TMurmur64aLE.HashBuf(Pointer(s), System.Length(s), seed);
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TMurmur64aTest.HashBuf16Seed0;
var
  h: QWord;
  s: ansistring;
const
  seed = 0;
  hash: QWord = QWord($64699D31FB6F9122);
begin
  s := 'abcdbcdecdefdefg';
  h := TMurmur64aLE.HashBuf(Pointer(s), System.Length(s), seed);
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TMurmur64aTest.HashBuf17Seed0;
var
  h: QWord;
  s: ansistring;
const
  seed = 0;
  hash: QWord = QWord($48E49BA93F660D43);
begin
  s := 'abcdbcdecdefdefge';
  h := TMurmur64aLE.HashBuf(Pointer(s), System.Length(s), seed);
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TMurmur64aTest.HashBuf21Seed0;
var
  h: QWord;
  s: ansistring;
const
  seed = 0;
  hash: QWord = QWord($5002843D301D1487);
begin
  s := 'abcdbcdecdefdefgefghf';
  h := TMurmur64aLE.HashBuf(Pointer(s), System.Length(s), seed);
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TMurmur64aTest.HashBuf25Seed0;
var
  h: QWord;
  s: ansistring;
const
  seed = 0;
  hash: QWord = QWord($7081BD44112D36C4);
begin
  s := 'abcdbcdecdefdefgefghfghig';
  h := TMurmur64aLE.HashBuf(Pointer(s), System.Length(s), seed);
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TMurmur64aTest.HashBuf31Seed0;
var
  h: QWord;
  s: ansistring;
const
  seed = 0;
  hash: QWord = QWord($84D5B260C7D854CB);
begin
  s := 'abcdbcdecdefdefgefghfghighijhij';
  h := TMurmur64aLE.HashBuf(Pointer(s), System.Length(s), seed);
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TMurmur64aTest.HashBuf32Seed0;
var
  h: QWord;
  s: ansistring;
const
  seed = 0;
  hash: QWord = QWord($DCCF3AD120ADF16C);
begin
  s := 'abcdbcdecdefdefgefghfghighijhijk';
  h := TMurmur64aLE.HashBuf(Pointer(s), System.Length(s), seed);
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TMurmur64aTest.HashBuf35Seed0;
var
  h: QWord;
  s: ansistring;
const
  seed = 0;
  hash: QWord = QWord($6F468AF4649649F7);
begin
  s := 'abcdbcdecdefdefgefghfghighijhijkijk';
  h := TMurmur64aLE.HashBuf(Pointer(s), System.Length(s), seed);
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TMurmur64aTest.HashBuf36Seed0;
var
  h: QWord;
  s: ansistring;
const
  seed = 0;
  hash: QWord = QWord($17F6D8C0D83703F7);
begin
  s := 'abcdbcdecdefdefgefghfghighijhijkijkl';
  h := TMurmur64aLE.HashBuf(Pointer(s), System.Length(s), seed);
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TMurmur64aTest.HashBuf39Seed0;
var
  h: QWord;
  s: ansistring;
const
  seed = 0;
  hash: QWord = QWord($1BD5592BCBB3DCD4);
begin
  s := 'abcdbcdecdefdefgefghfghighijhijkijkljkl';
  h := TMurmur64aLE.HashBuf(Pointer(s), System.Length(s), seed);
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TMurmur64aTest.HashBuf40Seed0;
var
  h: QWord;
  s: ansistring;
const
  seed = 0;
  hash: QWord = QWord($4B082AE27884AB3B);
begin
  s := 'abcdbcdecdefdefgefghfghighijhijkijkljklm';
  h := TMurmur64aLE.HashBuf(Pointer(s), System.Length(s), seed);
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TMurmur64aTest.HashBuf41Seed0;
var
  h: QWord;
  s: ansistring;
const
  seed = 0;
  hash: QWord = QWord($43BCE4A96BF65FCB);
begin
  s := 'abcdbcdecdefdefgefghfghighijhijkijkljklmk';
  h := TMurmur64aLE.HashBuf(Pointer(s), System.Length(s), seed);
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TMurmur64aTest.HashBuf0Seed1000001;
var
  h: QWord;
  s: ansistring;
const
  seed = 1000001;
  hash: QWord = QWord($B4A4EB631E3ED19C);
begin
  s := '';
  h := TMurmur64aLE.HashBuf(Pointer(s), System.Length(s), seed);
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TMurmur64aTest.HashBuf1Seed1000001;
var
  h: QWord;
  s: ansistring;
const
  seed = 1000001;
  hash: QWord = QWord($84653ED625AE132F);
begin
  s := 'a';
  h := TMurmur64aLE.HashBuf(Pointer(s), System.Length(s), seed);
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TMurmur64aTest.HashBuf2Seed1000001;
var
  h: QWord;
  s: ansistring;
const
  seed = 1000001;
  hash: QWord = QWord($A3E6A9FB2A65EB56);
begin
  s := 'ab';
  h := TMurmur64aLE.HashBuf(Pointer(s), System.Length(s), seed);
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TMurmur64aTest.HashBuf3Seed1000001;
var
  h: QWord;
  s: ansistring;
const
  seed = 1000001;
  hash: QWord = QWord($E3A4A68104685AAE);
begin
  s := 'abc';
  h := TMurmur64aLE.HashBuf(Pointer(s), System.Length(s), seed);
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TMurmur64aTest.HashBuf4Seed1000001;
var
  h: QWord;
  s: ansistring;
const
  seed = 1000001;
  hash: QWord = QWord($A19B40ADE3CE39D7);
begin
  s := 'abcd';
  h := TMurmur64aLE.HashBuf(Pointer(s), System.Length(s), seed);
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TMurmur64aTest.HashBuf5Seed1000001;
var
  h: QWord;
  s: ansistring;
const
  seed = 1000001;
  hash: QWord = QWord($F3A11FF1B169B7C0);
begin
  s := 'abcdb';
  h := TMurmur64aLE.HashBuf(Pointer(s), System.Length(s), seed);
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TMurmur64aTest.HashBuf8Seed1000001;
var
  h: QWord;
  s: ansistring;
const
  seed = 1000001;
  hash: QWord = QWord($B7837BD92AAAE67A);
begin
  s := 'abcdbcde';
  h := TMurmur64aLE.HashBuf(Pointer(s), System.Length(s), seed);
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TMurmur64aTest.HashBuf12Seed1000001;
var
  h: QWord;
  s: ansistring;
const
  seed = 1000001;
  hash: QWord = QWord($4008D2139CD77DBE);
begin
  s := 'abcdbcdecdef';
  h := TMurmur64aLE.HashBuf(Pointer(s), System.Length(s), seed);
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TMurmur64aTest.HashBuf15Seed1000001;
var
  h: QWord;
  s: ansistring;
const
  seed = 1000001;
  hash: QWord = QWord($5D03648B1CB50CC3);
begin
  s := 'abcdbcdecdefdef';
  h := TMurmur64aLE.HashBuf(Pointer(s), System.Length(s), seed);
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TMurmur64aTest.HashBuf16Seed1000001;
var
  h: QWord;
  s: ansistring;
const
  seed = 1000001;
  hash: QWord = QWord($D434511FD60F48F2);
begin
  s := 'abcdbcdecdefdefg';
  h := TMurmur64aLE.HashBuf(Pointer(s), System.Length(s), seed);
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TMurmur64aTest.HashBuf17Seed1000001;
var
  h: QWord;
  s: ansistring;
const
  seed = 1000001;
  hash: QWord = QWord($CF75AD1D00D7280B);
begin
  s := 'abcdbcdecdefdefge';
  h := TMurmur64aLE.HashBuf(Pointer(s), System.Length(s), seed);
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TMurmur64aTest.HashBuf21Seed1000001;
var
  h: QWord;
  s: ansistring;
const
  seed = 1000001;
  hash: QWord = QWord($5E8344E7B8A51EED);
begin
  s := 'abcdbcdecdefdefgefghf';
  h := TMurmur64aLE.HashBuf(Pointer(s), System.Length(s), seed);
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TMurmur64aTest.HashBuf25Seed1000001;
var
  h: QWord;
  s: ansistring;
const
  seed = 1000001;
  hash: QWord = QWord($9CDBC613744AF96E);
begin
  s := 'abcdbcdecdefdefgefghfghig';
  h := TMurmur64aLE.HashBuf(Pointer(s), System.Length(s), seed);
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TMurmur64aTest.HashBuf31Seed1000001;
var
  h: QWord;
  s: ansistring;
const
  seed = 1000001;
  hash: QWord = QWord($9DD91A2F3BCA6F39);
begin
  s := 'abcdbcdecdefdefgefghfghighijhij';
  h := TMurmur64aLE.HashBuf(Pointer(s), System.Length(s), seed);
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TMurmur64aTest.HashBuf32Seed1000001;
var
  h: QWord;
  s: ansistring;
const
  seed = 1000001;
  hash: QWord = QWord($190B637C77F0BD38);
begin
  s := 'abcdbcdecdefdefgefghfghighijhijk';
  h := TMurmur64aLE.HashBuf(Pointer(s), System.Length(s), seed);
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TMurmur64aTest.HashBuf35Seed1000001;
var
  h: QWord;
  s: ansistring;
const
  seed = 1000001;
  hash: QWord = QWord($4C9C6236994F7268);
begin
  s := 'abcdbcdecdefdefgefghfghighijhijkijk';
  h := TMurmur64aLE.HashBuf(Pointer(s), System.Length(s), seed);
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TMurmur64aTest.HashBuf36Seed1000001;
var
  h: QWord;
  s: ansistring;
const
  seed = 1000001;
  hash: QWord = QWord($7F63D56E6F9A2824);
begin
  s := 'abcdbcdecdefdefgefghfghighijhijkijkl';
  h := TMurmur64aLE.HashBuf(Pointer(s), System.Length(s), seed);
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TMurmur64aTest.HashBuf39Seed1000001;
var
  h: QWord;
  s: ansistring;
const
  seed = 1000001;
  hash: QWord = QWord($1E102F69B2C7A30E);
begin
  s := 'abcdbcdecdefdefgefghfghighijhijkijkljkl';
  h := TMurmur64aLE.HashBuf(Pointer(s), System.Length(s), seed);
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TMurmur64aTest.HashBuf40Seed1000001;
var
  h: QWord;
  s: ansistring;
const
  seed = 1000001;
  hash: QWord = QWord($66B0F3AD2A6F2BA4);
begin
  s := 'abcdbcdecdefdefgefghfghighijhijkijkljklm';
  h := TMurmur64aLE.HashBuf(Pointer(s), System.Length(s), seed);
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TMurmur64aTest.HashBuf41Seed1000001;
var
  h: QWord;
  s: ansistring;
const
  seed = 1000001;
  hash: QWord = QWord($6A56ED4D6333B5C8);
begin
  s := 'abcdbcdecdefdefgefghfghighijhijkijkljklmk';
  h := TMurmur64aLE.HashBuf(Pointer(s), System.Length(s), seed);
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TMurmur64aTest.HashWord0Seed0;
var
  h: QWord;
  w: Word;
const
  seed = 0;
  hash: QWord = QWord($B04BEBE77B2C52F3);
begin
  w := 0;
  h := TMurmur64aLE.HashWord(w, seed);
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TMurmur64aTest.HashWordSeed0;
var
  h: QWord;
  w: Word;
const
  seed = 0;
  hash: QWord = QWord($DADA35ED9203EBC9);
begin
  w := $2F63;
  h := TMurmur64aLE.HashWord(w, seed);
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TMurmur64aTest.HashWord0Seed1000001;
var
  h: QWord;
  w: Word;
const
  seed = 1000001;
  hash: QWord = QWord($87DED5C6131F288D);
begin
  w := 0;
  h := TMurmur64aLE.HashWord(w, seed);
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TMurmur64aTest.HashWordSeed1000001;
var
  h: QWord;
  w: Word;
const
  seed = 1000001;
  hash: QWord = QWord($DE10EF6D2827CCC1);
begin
  w := $2F63;
  h := TMurmur64aLE.HashWord(w, seed);
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TMurmur64aTest.HashDWord0Seed0;
var
  h: QWord;
  d: DWord;
const
  seed = 0;
  hash: QWord = QWord($6097D7CEF65AA5E7);
begin
  d := 0;
  h := TMurmur64aLE.HashDWord(d, seed);
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TMurmur64aTest.HashDWordSeed0;
var
  h: QWord;
  d: DWord;
const
  seed = 0;
  hash: QWord = QWord($523F0595DDC31E5E);
begin
  d := $484F120C;
  h := TMurmur64aLE.HashDWord(d, seed);
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TMurmur64aTest.HashDWord0Seed1000001;
var
  h: QWord;
  d: DWord;
const
  seed = 1000001;
  hash: QWord = QWord($17CEC677DFE54D2C);
begin
  d := 0;
  h := TMurmur64aLE.HashDWord(d, seed);
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TMurmur64aTest.HashDWordSeed1000001;
var
  h: QWord;
  d: DWord;
const
  seed = 1000001;
  hash: QWord = QWord($4E91703E6EBEDC48);
begin
  d := $484F120C;
  h := TMurmur64aLE.HashDWord(d, seed);
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TMurmur64aTest.HashQWord0Seed0;
var
  h: QWord;
  q: QWord;
const
  seed = 0;
  hash: QWord = QWord($7208F7FA198A2D81);
begin
  q := 0;
  h := TMurmur64aLE.HashQWord(q, seed);
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TMurmur64aTest.HashQWordSeed0;
var
  h: QWord;
  q: QWord;
const
  seed = 0;
  hash: QWord = QWord($8EF2BB08A6FEA535);
begin
  q := $044F5885223A2B;
  h := TMurmur64aLE.HashQWord(q, seed);
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TMurmur64aTest.HashQWord0Seed1000001;
var
  h: QWord;
  q: QWord;
const
  seed = 1000001;
  hash: QWord = QWord($41D7BE0599181197);
begin
  q := 0;
  h := TMurmur64aLE.HashQWord(q, seed);
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TMurmur64aTest.HashQWordSeed1000001;
var
  h: QWord;
  q: QWord;
const
  seed = 1000001;
  hash: QWord = QWord($4605B8968096B9E3);
begin
  q := $044F5885223A2B;
  h := TMurmur64aLE.HashQWord(q, seed);
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TMurmur64aTest.HashGuid0Seed0;
var
  h: QWord;
  g: TGuid;
const
  seed = 0;
  hash: QWord = QWord($FCDEA51A023E4390);
begin
  g := StringToGuid('{00000000-0000-0000-0000-000000000000}');
  h := TMurmur64aLE.HashGuid(g, seed);
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TMurmur64aTest.HashGuidSeed0;
var
  h: QWord;
  g: TGuid;
const
  seed = 0;
  hash: QWord = QWord($2F61236D94242123);
begin
  g := StringToGuid('{8D60E51C-0FEA-4D45-9644-4C4AFC4FC22F}');
  h := TMurmur64aLE.HashGuid(g, seed);
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TMurmur64aTest.HashGuid0Seed1000001;
var
  h: QWord;
  g: TGuid;
const
  seed = 1000001;
  hash: QWord = QWord($7129E39E785EA574);
begin
  g := StringToGuid('{00000000-0000-0000-0000-000000000000}');
  h := TMurmur64aLE.HashGuid(g, seed);
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;

procedure TMurmur64aTest.HashGuidSeed1000001;
var
  h: QWord;
  g: TGuid;
const
  seed = 1000001;
  hash: QWord = QWord($6154019B9E028A79);
begin
  g := StringToGuid('{8D60E51C-0FEA-4D45-9644-4C4AFC4FC22F}');
  h := TMurmur64aLE.HashGuid(g, seed);
  AssertTrue(Format(UnexpFmt, [h]), h = hash);
end;


initialization
  Randomize;
  RegisterTest(TxxHash32Test);
  RegisterTest(TxxHash64Test);
  RegisterTest(TMurmur2Test);
  RegisterTest(TMurmur2ATest);
  RegisterTest(TMurmur3Test);
  RegisterTest(TMurmur64aTest);

end.

