{****************************************************************************
*                                                                           *
*   This file is part of the LGenerics package.                             *
*   Non-cryptographic hash functions for hash table lookup.                 *
*                                                                           *
*   Copyright(c) 2018-2025 A.Koverdyaev(avk)                                *
*                                                                           *
*   This code is free software; you can redistribute it and/or modify it    *
*   under the terms of the Apache License, Version 2.0;                     *
*   You may obtain a copy of the License at                                 *
*     http://www.apache.org/licenses/LICENSE-2.0.                           *
*                                                                           *
*  Unless required by applicable law or agreed to in writing, software      *
*  distributed under the License is distributed on an "AS IS" BASIS,        *
*  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. *
*  See the License for the specific language governing permissions and      *
*  limitations under the License.                                           *
*                                                                           *
*****************************************************************************}

unit lgHash;

{$MODE OBJFPC}{$H+}
{$MODESWITCH ADVANCEDRECORDS}
{$INLINE ON}
{.$DEFINE FPC_REQUIRES_PROPER_ALIGNMENT  for test purpose only}

interface

type

  { TxxHash32LE: little endian implementation of Yann Collet's xxHash32 }
  TxxHash32LE = record
{$IFDEF FPC_REQUIRES_PROPER_ALIGNMENT}
  private
    class function HashBufUnalign(aBuffer: Pointer; aCount: SizeInt; aSeed: DWord = 0): DWord; static;
  public
{$ENDIF FPC_REQUIRES_PROPER_ALIGNMENT}
    class function HashBuf(aBuffer: Pointer; aCount: SizeInt; aSeed: DWord = 0): DWord; static;
    class function HashStr(const aValue: rawbytestring; aSeed: DWord = 0): DWord; static; inline;
    class function HashWord(aValue: Word; aSeed: DWord = 0): DWord; static;
    class function HashDWord(aValue: DWord; aSeed: DWord = 0): DWord; static;
    class function HashQWord(aValue: QWord; aSeed: DWord = 0): DWord; static;
    class function HashGuid(const aValue: TGuid; aSeed: DWord = 0): DWord; static;
  end;

  { TIncXxHash32LE: incremental version of TxxHash32LE }
  TIncXxHash32LE = record
  private
    FV,
    FBuffer: array[0..3] of DWord;
    FSeed,
    FTotalBytes,
    FHash: DWord;
    FBufIndex: Integer;
  public
    procedure Reset(aSeed: DWord = 0);
    procedure Add(const aBuffer; aCount: SizeInt);
    function  Hash: DWord;
  end;

  { TxxHash64LE: little endian implementation of Yann Collet's xxHash64 }
  TxxHash64LE = record
{$IFDEF FPC_REQUIRES_PROPER_ALIGNMENT}
  private
    class function HashBufUnalign(aBuffer: Pointer; aCount: SizeInt; aSeed: QWord = 0): QWord; static;
  public
{$ENDIF FPC_REQUIRES_PROPER_ALIGNMENT}
    class function HashBuf(aBuffer: Pointer; aCount: SizeInt; aSeed: QWord = 0): QWord; static;
    class function HashStr(const aValue: rawbytestring; aSeed: QWord = 0): QWord; static; inline;
    class function HashWord(aValue: Word; aSeed: QWord = 0): QWord; static;
    class function HashDWord(aValue: DWord; aSeed: QWord = 0): QWord; static;
    class function HashQWord(aValue: QWord; aSeed: QWord = 0): QWord; static;
    class function HashGuid(const aValue: TGuid; aSeed: QWord = 0): QWord; static;
  end;

  { TIncXxHash64LE: incremental version of TxxHash64LE }
  TIncXxHash64LE = record
  private
    FV,
    FBuffer: array[0..3] of QWord;
    FSeed,
    FTotalBytes,
    FHash: QWord;
    FBufIndex: Integer;
  public
    procedure Reset(aSeed: QWord = 0);
    procedure Add(const aBuffer; aCount: SizeInt);
    function  Hash: QWord;
  end;

  TMurmur = class
  protected
  type
    TByte2 = array[0..1] of Byte;
    TByte3 = array[0..2] of Byte;
    TByte4 = array[0..3] of Byte;
    TByte5 = array[0..4] of Byte;
    TByte6 = array[0..5] of Byte;
    TByte7 = array[0..6] of Byte;
    PByte2 = ^TByte2;
    PByte3 = ^TByte3;
    PByte4 = ^TByte4;
    PByte5 = ^TByte5;
    PByte6 = ^TByte6;
    PByte7 = ^TByte7;
  end;

  { TMurmur2LE: little endian implementation of Austin Appleby's MurmurHash2 }
  TMurmur2LE = class(TMurmur)
{$IFDEF FPC_REQUIRES_PROPER_ALIGNMENT}
  private
    class function HashBufUnalign(aBuffer: Pointer; aCount: SizeInt; aSeed: DWord = 0): DWord; static;
  public
{$ENDIF FPC_REQUIRES_PROPER_ALIGNMENT}
    class function HashBuf(aBuffer: Pointer; aCount: SizeInt; aSeed: DWord = 0): DWord; static;
    class function HashStr(const aValue: rawbytestring; aSeed: DWord = 0): DWord; static; inline;
    class function HashWord(aValue: Word; aSeed: DWord = 0): DWord; static;
    class function HashDWord(aValue: DWord; aSeed: DWord = 0): DWord; static;
    class function HashQWord(aValue: QWord; aSeed: DWord = 0): DWord; static;
    class function HashGuid(const aValue: TGuid; aSeed: DWord = 0): DWord; static;
  end;

  { TMurmur2aLE: little endian implementation of Austin Appleby's MurmurHash2A }
  TMurmur2aLE = class(TMurmur)
{$IFDEF FPC_REQUIRES_PROPER_ALIGNMENT}
  private
    class function HashBufUnalign(aBuffer: Pointer; aCount: SizeInt; aSeed: DWord = 0): DWord; static;
  public
{$ENDIF FPC_REQUIRES_PROPER_ALIGNMENT}
    class function HashBuf(aBuffer: Pointer; aCount: SizeInt; aSeed: DWord = 0): DWord; static;
    class function HashStr(const aValue: rawbytestring; aSeed: DWord = 0): DWord; static; inline;
    class function HashWord(aValue: Word; aSeed: DWord = 0): DWord; static;
    class function HashDWord(aValue: DWord; aSeed: DWord = 0): DWord; static;
    class function HashQWord(aValue: QWord; aSeed: DWord = 0): DWord; static;
    class function HashGuid(const aValue: TGuid; aSeed: DWord = 0): DWord; static;
  end;

  { TMurmur3LE: little endian implementation of Austin Appleby's MurmurHash3_x86_32 }
  TMurmur3LE = class(TMurmur)
{$IFDEF FPC_REQUIRES_PROPER_ALIGNMENT}
  private
    class function HashBufUnalign(aBuffer: Pointer; aCount: SizeInt; aSeed: DWord = 0): DWord; static;
  public
{$ENDIF FPC_REQUIRES_PROPER_ALIGNMENT}
    class function HashBuf(aBuffer: Pointer; aCount: SizeInt; aSeed: DWord = 0): DWord; static;
    class function HashStr(const aValue: rawbytestring; aSeed: DWord = 0): DWord; static; inline;
    class function HashWord(aValue: Word; aSeed: DWord = 0): DWord; static;
    class function HashDWord(aValue: DWord; aSeed: DWord = 0): DWord; static;
    class function HashQWord(aValue: QWord; aSeed: DWord = 0): DWord; static;
    class function HashGuid(const aValue: TGuid; aSeed: DWord = 0): DWord; static;
  end;

  { TIncMurmur3LE: incremental version of TMurmur3LE}
  TIncMurmur3LE = record
  private
    FBuffer: array[0..3] of DWord;
    FTotalBytes,
    FHash: DWord;
    FBufIndex: Integer;
  public
    procedure Reset(aSeed: DWord = 0);
    procedure Add(const aBuffer; aCount: SizeInt);
    function  Hash: DWord;
  end;

  { TMurmur64aLE: little endian implementation of Austin Appleby's MurmurHash64A }
  TMurmur64aLE = class(TMurmur)
{$IFDEF FPC_REQUIRES_PROPER_ALIGNMENT}
  private
    class function HashBufUnalign(aBuffer: Pointer; aCount: SizeInt; aSeed: QWord = 0): QWord; static;
  public
{$ENDIF FPC_REQUIRES_PROPER_ALIGNMENT}
    class function HashBuf(aBuffer: Pointer; aCount: SizeInt; aSeed: QWord = 0): QWord; static;
    class function HashStr(const aValue: rawbytestring; aSeed: QWord = 0): QWord; static; inline;
    class function HashWord(aValue: Word; aSeed: QWord = 0): QWord; static;
    class function HashDWord(aValue: DWord; aSeed: QWord = 0): QWord; static;
    class function HashQWord(aValue: QWord; aSeed: QWord = 0): QWord; static;
    class function HashGuid(const aValue: TGuid; aSeed: QWord = 0): QWord; static;
  end;


  function JdkHashW(aValue: Word): Word; inline;
  function JdkHash(aValue: DWord): DWord; inline;
  function JdkHashQ(aValue: QWord): DWord; inline;

  { FNV1A_JesteressM: slightly modified FNV1A_Hash_Jesteress from http://www.sanmayce.com/Fastest_Hash}
  function FNV1A_JesteressM(aBuffer: Pointer; aCount: SizeInt; aSeed: DWord = 0): DWord;

implementation

{$Q-}{$R-}{$B-}{$COPERATORS ON}{$MACRO ON}

function JdkHashW(aValue: Word): Word;
begin
  aValue := aValue xor aValue shr 12;
  Result := aValue xor aValue shr 7 xor aValue shr 4;
end;

function JdkHash(aValue: DWord): DWord;
begin
  aValue := aValue xor aValue shr 20 xor aValue shr 12;
  Result := aValue xor aValue shr 7 xor aValue shr 4;
end;

function JdkHashQ(aValue: QWord): DWord;
begin
  Result := JdkHash(aValue xor aValue shr 32);
end;

{$IFDEF FPC_REQUIRES_PROPER_ALIGNMENT}{$PUSH}{$WARN 5057 OFF}
function FNV1A_JesteressMUnalign(aBuffer: Pointer; aCount: SizeInt; aSeed: DWord): DWord;
var
  p: PByte absolute aBuffer;
  buf: array[0..15] of DWord;
begin
  Result := DWord(2166136261) + DWord(aCount) + aSeed;
  while aCount > Pred(SizeOf(buf)) do
    begin
      System.Move(p^, buf, SizeOf(buf));
      Result := (Result xor (RolDWord(buf[ 0], 5) xor buf[ 1])) * DWord(709607);
      Result := (Result xor (RolDWord(buf[ 2], 5) xor buf[ 3])) * DWord(709607);
      Result := (Result xor (RolDWord(buf[ 4], 5) xor buf[ 5])) * DWord(709607);
      Result := (Result xor (RolDWord(buf[ 6], 5) xor buf[ 7])) * DWord(709607);
      Result := (Result xor (RolDWord(buf[ 8], 5) xor buf[ 9])) * DWord(709607);
      Result := (Result xor (RolDWord(buf[10], 5) xor buf[11])) * DWord(709607);
      Result := (Result xor (RolDWord(buf[12], 5) xor buf[13])) * DWord(709607);
      Result := (Result xor (RolDWord(buf[14], 5) xor buf[15])) * DWord(709607);
      p += SizeOf(buf);
      aCount -= SizeOf(buf);
    end;
  if aCount <> 0 then
    begin
      System.Move(p^, buf, aCount);
      p := @buf;
      while aCount > 7 do
        begin
          Result := (Result xor (RolDWord(PDWord(p)^, 5) xor PDWord(p + 4)^)) * DWord(709607);
          p += 8;
          aCount -= 8;
        end;
      if aCount and 4 <> 0 then
        begin
          Result := (Result xor PDWord(p)^) * DWord(709607);
          p += 4;
        end;
      if aCount and 2 <> 0 then
        begin
          Result := (Result xor p^) * DWord(709607);
          Result := (Result xor p[1]) * DWord(709607);
          p += 2;
        end;
      if aCount and 1 <> 0 then
        Result := (Result xor p^) * DWord(709607);
    end;
  Result := Result xor Result shr 16;
end;
{$POP}{$ENDIF FPC_REQUIRES_PROPER_ALIGNMENT}

function FNV1A_JesteressM(aBuffer: Pointer; aCount: SizeInt; aSeed: DWord): DWord;
var
  p: PByte absolute aBuffer;
begin
{$IFDEF FPC_REQUIRES_PROPER_ALIGNMENT}{$PUSH}{$WARN 4055 OFF}
  if SizeUInt(aBuffer) and 3 <> 0 then
    exit(FNV1A_JesteressMUnalign(aBuffer, aCount, aSeed));
{$POP}{$ENDIF FPC_REQUIRES_PROPER_ALIGNMENT}
  Result := DWord(2166136261) + DWord(aCount) + aSeed;
  while aCount > 7 do
    begin
      Result := (Result xor (RolDWord(PDWord(p)^, 5) xor PDWord(p + 4)^)) * DWord(709607);
      p += 8;
      aCount -= 8;
    end;
  if aCount and 4 <> 0 then
    begin
      Result := (Result xor PDWord(p)^) * DWord(709607);
      p += 4;
    end;
  if aCount and 2 <> 0 then
    begin
      Result := (Result xor p^) * DWord(709607);
      Result := (Result xor p[1]) * DWord(709607);
      p += 2;
    end;
  if aCount and 1 <> 0 then
    Result := (Result xor p^) * DWord(709607);
  Result := Result xor Result shr 16;
end;

{$DEFINE c1 := DWord($9e3779b1)}{$DEFINE c2 := DWord($85ebca77)}{$DEFINE c3 := DWord($c2b2ae3d)}
{$DEFINE c4 := DWord($27d4eb2f)}{$DEFINE c5 := DWord($165667b1)}

{$IFDEF FPC_REQUIRES_PROPER_ALIGNMENT}{$PUSH}{$WARN 5057 OFF}
class function TxxHash32LE.HashBufUnalign(aBuffer: Pointer; aCount: SizeInt; aSeed: DWord): DWord;
var
  v1, v2, v3, v4: DWord;
  buf: array[0..3] of DWord;
  p: PByte absolute aBuffer;
begin
  Result := DWord(aCount);
  if aCount >= 16 then
    begin
      v1 := aSeed + c1 + c2;
      v2 := aSeed + c2;
      v3 := aSeed;
      v4 := aSeed - c1;
      while aCount >= 16 do
        begin
          System.Move(p^, buf, 16);
          v1 := RolDWord(v1 + buf[0] * c2, 13) * c1;
          v2 := RolDWord(v2 + buf[1] * c2, 13) * c1;
          v3 := RolDWord(v3 + buf[2] * c2, 13) * c1;
          v4 := RolDWord(v4 + buf[3] * c2, 13) * c1;
          p += 16;
          aCount -= 16;
        end;
      Result += RolDWord(v1, 1) + RolDWord(v2, 7) + RolDWord(v3, 12) + RolDWord(v4, 18);
    end
  else
    Result += aSeed + c5;
  if aCount > 0 then
    begin
      System.Move(p^, buf, aCount);
      p := @buf;
      case aCount shr 2 of
        1:
          begin
            Result := RolDWord(Result + buf[0] * c3, 17) * c4;
            aCount -= 4;
            p += 4;
          end;
        2:
          begin
            Result := RolDWord(Result + buf[0] * c3, 17) * c4;
            Result := RolDWord(Result + buf[1] * c3, 17) * c4;
            aCount -= 8;
            p += 8;
          end;
        3:
          begin
            Result := RolDWord(Result + buf[0] * c3, 17) * c4;
            Result := RolDWord(Result + buf[1] * c3, 17) * c4;
            Result := RolDWord(Result + buf[2] * c3, 17) * c4;
            aCount -= 12;
            p += 12;
          end;
      end;
      case aCount of
        1:
          begin
            Result := RolDWord(Result + DWord(p[0]) * c5, 11) * c1;
          end;
        2:
          begin
            Result := RolDWord(Result + DWord(p[0]) * c5, 11) * c1;
            Result := RolDWord(Result + DWord(p[1]) * c5, 11) * c1;
          end;
        3:
          begin
            Result := RolDWord(Result + DWord(p[0]) * c5, 11) * c1;
            Result := RolDWord(Result + DWord(p[1]) * c5, 11) * c1;
            Result := RolDWord(Result + DWord(p[2]) * c5, 11) * c1;
          end;
      end;
    end;
  Result := (Result xor Result shr 15) * c2;
  Result := (Result xor Result shr 13) * c3;
  Result :=  Result xor Result shr 16;
end;
{$POP}{$ENDIF FPC_REQUIRES_PROPER_ALIGNMENT}

class function TxxHash32LE.HashBuf(aBuffer: Pointer; aCount: SizeInt; aSeed: DWord): DWord;
var
  v1, v2, v3, v4: DWord;
  p: PDWord absolute aBuffer;
begin
{$IFDEF FPC_REQUIRES_PROPER_ALIGNMENT}{$PUSH}{$WARN 4055 OFF}
  if SizeUInt(aBuffer) and 3 <> 0 then
    exit(HashBufUnalign(aBuffer, aCount, aSeed));
{$POP}{$ENDIF FPC_REQUIRES_PROPER_ALIGNMENT}
  Result := DWord(aCount);
  if aCount >= 16 then
    begin
      v1 := aSeed + c1 + c2;
      v2 := aSeed + c2;
      v3 := aSeed;
      v4 := aSeed - c1;
      while aCount >= 16 do
        begin
          v1 := RolDWord(v1 + p[0] * c2, 13) * c1;
          v2 := RolDWord(v2 + p[1] * c2, 13) * c1;
          v3 := RolDWord(v3 + p[2] * c2, 13) * c1;
          v4 := RolDWord(v4 + p[3] * c2, 13) * c1;
          p += 4;
          aCount -= 16;
        end;
      Result += RolDWord(v1, 1) + RolDWord(v2, 7) + RolDWord(v3, 12) + RolDWord(v4, 18);
    end
  else
    Result += aSeed + c5;
  case aCount shr 2 of
    1:
      begin
        Result := RolDWord(Result + p[0] * c3, 17) * c4;
        p += 1;
        aCount -= 4;
      end;
    2:
      begin
        Result := RolDWord(Result + p[0] * c3, 17) * c4;
        Result := RolDWord(Result + p[1] * c3, 17) * c4;
        p += 2;
        aCount -= 8;
      end;
    3:
      begin
        Result := RolDWord(Result + p[0] * c3, 17) * c4;
        Result := RolDWord(Result + p[1] * c3, 17) * c4;
        Result := RolDWord(Result + p[2] * c3, 17) * c4;
        p += 3;
        aCount -= 12;
      end;
  end;
  case aCount of
    1:
      Result := RolDWord(Result + DWord(PByte(p)[0]) * c5, 11) * c1;
    2:
      begin
        Result := RolDWord(Result + DWord(PByte(p)[0]) * c5, 11) * c1;
        Result := RolDWord(Result + DWord(PByte(p)[1]) * c5, 11) * c1;
      end;
    3:
      begin
        Result := RolDWord(Result + DWord(PByte(p)[0]) * c5, 11) * c1;
        Result := RolDWord(Result + DWord(PByte(p)[1]) * c5, 11) * c1;
        Result := RolDWord(Result + DWord(PByte(p)[2]) * c5, 11) * c1;
      end;
  end;
  Result := (Result xor Result shr 15) * c2;
  Result := (Result xor Result shr 13) * c3;
  Result :=  Result xor Result shr 16;
end;

class function TxxHash32LE.HashStr(const aValue: rawbytestring; aSeed: DWord): DWord;
begin
  Result := HashBuf(Pointer(aValue), System.Length(aValue), aSeed);
end;

class function TxxHash32LE.HashWord(aValue: Word; aSeed: DWord): DWord;
begin
  Result := RolDWord(aSeed + c5 + SizeOf(aValue) + (aValue and $ff) * c5, 11) * c1;
  Result := RolDWord(Result + (aValue shr 8) * c5, 11) * c1;
  Result := (Result xor Result shr 15) * c2;
  Result := (Result xor Result shr 13) * c3;
  Result := Result xor Result shr 16;
end;

class function TxxHash32LE.HashDWord(aValue: DWord; aSeed: DWord): DWord;
begin
  Result := RolDWord(aSeed + c5 + SizeOf(aValue) + aValue * c3, 17) * c4;
  Result := (Result xor Result shr 15) * c2;
  Result := (Result xor Result shr 13) * c3;
  Result := Result xor Result shr 16;
end;

class function TxxHash32LE.HashQWord(aValue: QWord; aSeed: DWord): DWord;
begin
  Result := RolDWord(c5 + aSeed + SizeOf(aValue) + PDWord(@aValue)[0] * c3, 17) * c4;
  Result := RolDWord(Result + PDWord(@aValue)[1] * c3, 17) * c4;
  Result := (Result xor Result shr 15) * c2;
  Result := (Result xor Result shr 13) * c3;
  Result := Result xor Result shr 16;
end;

type
  TDWords4 = packed record
    D1, D2, D3, D4: DWord;
  end;

class function TxxHash32LE.HashGuid(const aValue: TGuid; aSeed: DWord): DWord;
var
  g: TDWords4 absolute aValue;
begin
  Result := RolDWord(RolDWord(DWord(aSeed + c1 + c2) {%H-}+ g.D1 * c2, 13) * c1, 1) +
            RolDWord(RolDWord(DWord(aSeed      + c2) {%H-}+ g.D2 * c2, 13) * c1, 7) +
            RolDWord(RolDWord(DWord(aSeed          ) + g.D3 * c2, 13) * c1, 12) +
            RolDWord(RolDWord(DWord(aSeed      - c1) + g.D4 * c2, 13) * c1, 18) + SizeOf(TGuid);
  Result := (Result xor Result shr 15) * c2;
  Result := (Result xor Result shr 13) * c3;
  Result :=  Result xor Result shr 16;
end;

function IntMin(L: Integer; R: SizeInt): Integer; inline;
begin
  if L <= R then
    Result := L
  else
    Result := Integer(R);
end;

{ TIncXxHash32LE }

procedure TIncXxHash32LE.Reset(aSeed: DWord);
begin
  FV[0] := aSeed + c1 + c2;
  FV[1] := aSeed + c2;
  FV[2] := aSeed;
  FV[3] := aSeed - c1;
  FSeed := aSeed;
  FTotalBytes := 0;
  FHash := 0;
  FBufIndex := 0;
end;

procedure TIncXxHash32LE.Add(const aBuffer; aCount: SizeInt);
var
  p: PByte;
  ByteCount: Integer;
begin
  if aCount < 1 then exit;
  Inc(FTotalBytes, aCount);
  p := @aBuffer;
  repeat
    ByteCount := IntMin(SizeOf(FBuffer)-FBufIndex, aCount);
    System.Move(p^, PByte(@FBuffer)[FBufIndex], ByteCount);
    Dec(aCount, ByteCount);
    Inc(FBufIndex, ByteCount);
    Inc(p, ByteCount);
    if FBufIndex = SizeOf(FBuffer) then
      begin
        FV[0] := RolDWord(FV[0] + FBuffer[0] * c2, 13) * c1;
        FV[1] := RolDWord(FV[1] + FBuffer[1] * c2, 13) * c1;
        FV[2] := RolDWord(FV[2] + FBuffer[2] * c2, 13) * c1;
        FV[3] := RolDWord(FV[3] + FBuffer[3] * c2, 13) * c1;
        FBufIndex := 0;
      end;
  until aCount = 0;
end;

function TIncXxHash32LE.Hash: DWord;
var
  p: PByte;
  h: DWord;
  ByteCount: Integer;
begin
  p := @FBuffer;
  ByteCount := FBufIndex;
  if FTotalBytes < SizeOf(FBuffer) then
    h := FTotalBytes + FSeed + c5
  else
    h := RolDWord(FV[0], 1) + RolDWord(FV[1], 7) + RolDWord(FV[2], 12) + RolDWord(FV[3], 18) + FTotalBytes;
  case ByteCount shr 2 of
    1:
      begin
        h := RolDWord(h + FBuffer[0] * c3, 17) * c4;
        p += 4;
        ByteCount -= 4;
      end;
    2:
      begin
        h := RolDWord(h + FBuffer[0] * c3, 17) * c4;
        h := RolDWord(h + FBuffer[1] * c3, 17) * c4;
        p += 8;
        ByteCount -= 8;
      end;
    3:
      begin
        h := RolDWord(h + FBuffer[0] * c3, 17) * c4;
        h := RolDWord(h + FBuffer[1] * c3, 17) * c4;
        h := RolDWord(h + FBuffer[2] * c3, 17) * c4;
        p += 12;
        ByteCount -= 12;
      end;
  end;
  case ByteCount of
    1:
      h := RolDWord(h + DWord(p[0]) * c5, 11) * c1;
    2:
      begin
        h := RolDWord(h + DWord(p[0]) * c5, 11) * c1;
        h := RolDWord(h + DWord(p[1]) * c5, 11) * c1;
      end;
    3:
      begin
        h := RolDWord(h + DWord(p[0]) * c5, 11) * c1;
        h := RolDWord(h + DWord(p[1]) * c5, 11) * c1;
        h := RolDWord(h + DWord(p[2]) * c5, 11) * c1;
      end;
  end;
  h := (h xor h shr 15) * c2;
  h := (h xor h shr 13) * c3;
  Result :=  h xor h shr 16;
end;
{$UNDEF c1}{$UNDEF c2}{$UNDEF c3}{$UNDEF c4}{$UNDEF c5}


{$DEFINE c1 := QWord($9e3779b185ebca87)}{$DEFINE c2 := QWord($c2b2ae3d27d4eb4f)}
{$DEFINE c3 := QWord($165667b19e3779f9)}{$DEFINE c4 := QWord($85ebca77c2b2ae63)}
{$DEFINE c5 := QWord($27d4eb2f165667c5)}{$DEFINE c6 := QWord($60ea27eeadc0b5d6)} //c1+c2

{$IFDEF FPC_REQUIRES_PROPER_ALIGNMENT}{$PUSH}{$WARN 5057 OFF}
class function TxxHash64LE.HashBufUnalign(aBuffer: Pointer; aCount: SizeInt; aSeed: QWord): QWord;
var
  v1, v2, v3, v4: QWord;
  buf: array[0..3] of QWord;
  p: PByte absolute aBuffer;
begin
  if aCount >= 32 then
    begin
      v1 := aSeed + c6;//c1 + c2; due to #0036356
      v2 := aSeed + c2;
      v3 := aSeed;
      v4 := aSeed - c1;
      aSeed := QWord(aCount);
      repeat
        System.Move(p^, buf, 32);
        v1 := RolQWord(v1 + c2 * buf[0], 31) * c1;
        v2 := RolQWord(v2 + c2 * buf[1], 31) * c1;
        v3 := RolQWord(v3 + c2 * buf[2], 31) * c1;
        v4 := RolQWord(v4 + c2 * buf[3], 31) * c1;
        p += 32;
        aCount -= 32;
      until aCount < 32;
      Result := RolQWord(v1, 1) + RolQWord(v2, 7) + RolQWord(v3, 12) + RolQWord(v4, 18);
      Result := (Result xor (RolQWord(v1 * c2, 31) * c1)) * c1 + c4;
      Result := (Result xor (RolQWord(v2 * c2, 31) * c1)) * c1 + c4;
      Result := (Result xor (RolQWord(v3 * c2, 31) * c1)) * c1 + c4;
      Result := (Result xor (RolQWord(v4 * c2, 31) * c1)) * c1 + c4 + aSeed;
    end
  else
    Result := aSeed + QWord(aCount) + c5;
  if aCount > 0 then
    begin
      System.Move(p^, buf, aCount);
      p := @buf;
      case aCount shr 3 of
        1:
          begin
            Result := RolQWord(Result xor (c1 * RolQWord(c2 * PQWord(p)[0], 31)), 27) * c1 + c4;
            aCount -= 8;
            p += 8;
          end;
        2:
          begin
            Result := RolQWord(Result xor (c1 * RolQWord(c2 * PQWord(p)[0], 31)), 27) * c1 + c4;
            Result := RolQWord(Result xor (c1 * RolQWord(c2 * PQWord(p)[1], 31)), 27) * c1 + c4;
            aCount -= 16;
            p += 16;
          end;
        3:
          begin
            Result := RolQWord(Result xor (c1 * RolQWord(c2 * PQWord(p)[0], 31)), 27) * c1 + c4;
            Result := RolQWord(Result xor (c1 * RolQWord(c2 * PQWord(p)[1], 31)), 27) * c1 + c4;
            Result := RolQWord(Result xor (c1 * RolQWord(c2 * PQWord(p)[2], 31)), 27) * c1 + c4;
            aCount -= 24;
            p += 24;
          end;
      end;
      if aCount >= 4 then
        begin
          Result := RolQWord(Result xor (QWord(PDWord(p)^) * c1), 23) * c2 + c3;
          p += 4;
          aCount -= 4;
        end;
      case aCount of
        1:
          begin
            Result := RolQWord(Result xor (QWord(p[0]) * c5), 11) * c1;
          end;
        2:
          begin
            Result := RolQWord(Result xor (QWord(p[0]) * c5), 11) * c1;
            Result := RolQWord(Result xor (QWord(p[1]) * c5), 11) * c1;
          end;
        3:
          begin
            Result := RolQWord(Result xor (QWord(p[0]) * c5), 11) * c1;
            Result := RolQWord(Result xor (QWord(p[1]) * c5), 11) * c1;
            Result := RolQWord(Result xor (QWord(p[2]) * c5), 11) * c1;
          end;
      end;
    end;
  Result := (Result xor Result shr 33) * c2;
  Result := (Result xor Result shr 29) * c3;
  Result :=  Result xor Result shr 32;
end;
{$POP}{$ENDIF FPC_REQUIRES_PROPER_ALIGNMENT}

class function TxxHash64LE.HashBuf(aBuffer: Pointer; aCount: SizeInt; aSeed: QWord): QWord;
var
  v1, v2, v3, v4: QWord;
  p: PByte absolute aBuffer;
begin
{$IFDEF FPC_REQUIRES_PROPER_ALIGNMENT}{$PUSH}{$WARN 4055 OFF}
  if SizeUInt(aBuffer) and 7 <> 0 then
    exit(HashBufUnalign(aBuffer, aCount, aSeed));
{$POP}{$ENDIF FPC_REQUIRES_PROPER_ALIGNMENT}
  if aCount >= 32 then
    begin
      v1 := aSeed + c6;//c1 + c2; due to #0036356
      v2 := aSeed + c2;
      v3 := aSeed;
      v4 := aSeed - c1;
      aSeed := QWord(aCount);
      repeat
        v1 := RolQWord(v1 + PQWord(p)[0] * c2, 31) * c1;
        v2 := RolQWord(v2 + PQWord(p)[1] * c2, 31) * c1;
        v3 := RolQWord(v3 + PQWord(p)[2] * c2, 31) * c1;
        v4 := RolQWord(v4 + PQWord(p)[3] * c2, 31) * c1;
        p += 32;
        aCount -= 32;
      until aCount < 32;
      Result := RolQWord(v1, 1) + RolQWord(v2, 7) + RolQWord(v3, 12) + RolQWord(v4, 18);
      Result := (Result xor (RolQWord(v1 * c2, 31) * c1)) * c1 + c4;
      Result := (Result xor (RolQWord(v2 * c2, 31) * c1)) * c1 + c4;
      Result := (Result xor (RolQWord(v3 * c2, 31) * c1)) * c1 + c4;
      Result := (Result xor (RolQWord(v4 * c2, 31) * c1)) * c1 + c4 + aSeed;
    end
  else
    Result := aSeed + aCount + c5;
  case aCount shr 3 of
    1:
      begin
        Result := RolQWord(Result xor (c1 * RolQWord(PQWord(p)[0] * c2, 31)), 27) * c1 + c4;
        aCount -= 8;
        p += 8;
      end;
    2:
      begin
        Result := RolQWord(Result xor (c1 * RolQWord(PQWord(p)[0] * c2, 31)), 27) * c1 + c4;
        Result := RolQWord(Result xor (c1 * RolQWord(PQWord(p)[1] * c2, 31)), 27) * c1 + c4;
        aCount -= 16;
        p += 16;
      end;
    3:
      begin
        Result := RolQWord(Result xor (c1 * RolQWord(PQWord(p)[0] * c2, 31)), 27) * c1 + c4;
        Result := RolQWord(Result xor (c1 * RolQWord(PQWord(p)[1] * c2, 31)), 27) * c1 + c4;
        Result := RolQWord(Result xor (c1 * RolQWord(PQWord(p)[2] * c2, 31)), 27) * c1 + c4;
        aCount -= 24;
        p += 24;
      end;
  end;
  if aCount >= 4 then
    begin
      Result := RolQWord(Result xor (QWord(PDWord(p)^) * c1), 23) * c2 + c3;
      p += 4;
      aCount -= 4;
    end;
  case aCount of
    1:
      begin
        Result := RolQWord(Result xor (QWord(p[0]) * c5), 11) * c1;
      end;
    2:
      begin
        Result := RolQWord(Result xor (QWord(p[0]) * c5), 11) * c1;
        Result := RolQWord(Result xor (QWord(p[1]) * c5), 11) * c1;
      end;
    3:
      begin
        Result := RolQWord(Result xor (QWord(p[0]) * c5), 11) * c1;
        Result := RolQWord(Result xor (QWord(p[1]) * c5), 11) * c1;
        Result := RolQWord(Result xor (QWord(p[2]) * c5), 11) * c1;
      end;
  end;
  Result := (Result xor Result shr 33) * c2;
  Result := (Result xor Result shr 29) * c3;
  Result :=  Result xor Result shr 32;
end;

class function TxxHash64LE.HashStr(const aValue: rawbytestring; aSeed: QWord): QWord;
begin
  Result := HashBuf(Pointer(aValue), System.Length(aValue), aSeed);
end;

class function TxxHash64LE.HashWord(aValue: Word; aSeed: QWord): QWord;
begin
  Result := RolQWord((aSeed + c5 + 2) xor (QWord(aValue and $ff) * c5), 11) * c1;
  Result := RolQWord(Result xor (QWord(aValue shr 8) * c5), 11) * c1;
  Result := (Result xor Result shr 33) * c2;
  Result := (Result xor Result shr 29) * c3;
  Result :=  Result xor Result shr 32;
end;

class function TxxHash64LE.HashDWord(aValue: DWord; aSeed: QWord): QWord;
begin
  Result := RolQWord((aSeed + c5 + 4) xor (QWord(aValue) * c1), 23) * c2 + c3;
  Result := (Result xor Result shr 33) * c2;
  Result := (Result xor Result shr 29) * c3;
  Result :=  Result xor Result shr 32;
end;

class function TxxHash64LE.HashQWord(aValue: QWord; aSeed: QWord): QWord;
begin
  Result := RolQWord((aSeed + c5 + 8) xor (c1 * RolQWord(c2 * aValue, 31)), 27) * c1 + c4;
  Result := (Result xor Result shr 33) * c2;
  Result := (Result xor Result shr 29) * c3;
  Result :=  Result xor Result shr 32;
end;

{$IFDEF FPC_REQUIRES_PROPER_ALIGNMENT}
class function TxxHash64LE.HashGuid(const aValue: TGuid; aSeed: QWord): QWord; inline;
begin
  Result := HashBuf(@aValue, SizeOf(aValue), aSeed);
end;
{$ELSE FPC_REQUIRES_PROPER_ALIGNMENT}
class function TxxHash64LE.HashGuid(const aValue: TGuid; aSeed: QWord): QWord;
begin
  Result := RolQWord((aSeed + c5 + SizeOf(TGuid))xor(c1 * RolQWord(PQWord(@aValue)[0] * c2, 31)), 27) * c1 + c4;
  Result := RolQWord(Result xor (c1 * RolQWord(PQWord(@aValue)[1] * c2, 31)), 27) * c1 + c4;
  Result := (Result xor Result shr 33) * c2;
  Result := (Result xor Result shr 29) * c3;
  Result :=  Result xor Result shr 32;
end;
{$ENDIF FPC_REQUIRES_PROPER_ALIGNMENT}

{ TIncXxHash64LE }

procedure TIncXxHash64LE.Reset(aSeed: QWord);
begin
  FV[0] := aSeed + c6;
  FV[1] := aSeed + c2;
  FV[2] := aSeed;
  FV[3] := aSeed - c1;
  FSeed := aSeed;
  FTotalBytes := 0;
  FHash := 0;
  FBufIndex := 0;
end;

procedure TIncXxHash64LE.Add(const aBuffer; aCount: SizeInt);
var
  p: PByte;
  ByteCount: Integer;
begin
  if aCount < 1 then exit;
  Inc(FTotalBytes, aCount);
  p := @aBuffer;
  repeat
    ByteCount := IntMin(SizeOf(FBuffer)-FBufIndex, aCount);
    System.Move(p^, PByte(@FBuffer)[FBufIndex], ByteCount);
    Dec(aCount, ByteCount);
    Inc(FBufIndex, ByteCount);
    Inc(p, ByteCount);
    if FBufIndex = SizeOf(FBuffer) then
      begin
        FV[0] := RolQWord(FV[0] + FBuffer[0] * c2, 31) * c1;
        FV[1] := RolQWord(FV[1] + FBuffer[1] * c2, 31) * c1;
        FV[2] := RolQWord(FV[2] + FBuffer[2] * c2, 31) * c1;
        FV[3] := RolQWord(FV[3] + FBuffer[3] * c2, 31) * c1;
        FBufIndex := 0;
      end;
  until aCount = 0;
end;

function TIncXxHash64LE.Hash: QWord;
var
  p: PByte;
  h: QWord;
  ByteCount: Integer;
begin
  p := @FBuffer;
  ByteCount := FBufIndex;
  if FTotalBytes < SizeOf(FBuffer) then
    h := FTotalBytes + FSeed + c5
  else
    begin
      h := RolQWord(FV[0], 1) + RolQWord(FV[1], 7) + RolQWord(FV[2], 12) + RolQWord(FV[3], 18);
      h := (h xor (RolQWord(FV[0] * c2, 31) * c1)) * c1 + c4;
      h := (h xor (RolQWord(FV[1] * c2, 31) * c1)) * c1 + c4;
      h := (h xor (RolQWord(FV[2] * c2, 31) * c1)) * c1 + c4;
      h := (h xor (RolQWord(FV[3] * c2, 31) * c1)) * c1 + c4 + FTotalBytes;
    end;
  case ByteCount shr 3 of
    1:
      begin
        h := RolQWord(h xor (c1 * RolQWord(FBuffer[0] * c2, 31)), 27) * c1 + c4;
        ByteCount -= 8;
        p += 8;
      end;
    2:
      begin
        h := RolQWord(h xor (c1 * RolQWord(FBuffer[0] * c2, 31)), 27) * c1 + c4;
        h := RolQWord(h xor (c1 * RolQWord(FBuffer[1] * c2, 31)), 27) * c1 + c4;
        ByteCount -= 16;
        p += 16;
      end;
    3:
      begin
        h := RolQWord(h xor (c1 * RolQWord(FBuffer[0] * c2, 31)), 27) * c1 + c4;
        h := RolQWord(h xor (c1 * RolQWord(FBuffer[1] * c2, 31)), 27) * c1 + c4;
        h := RolQWord(h xor (c1 * RolQWord(FBuffer[2] * c2, 31)), 27) * c1 + c4;
        ByteCount -= 24;
        p += 24;
      end;
  end;
  if ByteCount >= 4 then
    begin
      h := RolQWord(h xor (QWord(PDWord(p)^) * c1), 23) * c2 + c3;
      p += 4;
      ByteCount -= 4;
    end;
  case ByteCount of
    1:
      begin
        h := RolQWord(h xor (QWord(p[0]) * c5), 11) * c1;
      end;
    2:
      begin
        h := RolQWord(h xor (QWord(p[0]) * c5), 11) * c1;
        h := RolQWord(h xor (QWord(p[1]) * c5), 11) * c1;
      end;
    3:
      begin
        h := RolQWord(h xor (QWord(p[0]) * c5), 11) * c1;
        h := RolQWord(h xor (QWord(p[1]) * c5), 11) * c1;
        h := RolQWord(h xor (QWord(p[2]) * c5), 11) * c1;
      end;
  end;
  h := (h xor h shr 33) * c2;
  h := (h xor h shr 29) * c3;
  Result :=  h xor h shr 32;
end;
{$UNDEF c1}{$UNDEF c2}{$UNDEF c3}{$UNDEF c4}{$UNDEF c5}{$UNDEF c6}

{ TMurmur2LE }

{$DEFINE m32 := DWord($5bd1e995)}

{$IFDEF FPC_REQUIRES_PROPER_ALIGNMENT}{$PUSH}{$WARN 5057 OFF}
class function TMurmur2LE.HashBufUnalign(aBuffer: Pointer; aCount: SizeInt; aSeed: DWord): DWord;
var
  k1, k2, k3, k4: DWord;
  buf: array[0..3] of DWord;
  p: PByte absolute aBuffer;
begin
  Result := aSeed xor DWord(aCount);
  while aCount >= 16 do
    begin
      System.Move(p^, buf, 16);
      k1 := buf[ 0] * m32;
      k2 := buf[ 1] * m32;
      k3 := buf[ 2] * m32;
      k4 := buf[ 3] * m32;
      Result := (((Result * m32) xor ((k1 xor k1 shr 24) * m32)) * m32) xor ((k2 xor k2 shr 24) * m32);
      Result := (((Result * m32) xor ((k3 xor k3 shr 24) * m32)) * m32) xor ((k4 xor k4 shr 24) * m32);
      p += 16;
      aCount -= 16;
    end;
  if aCount > 0 then
    begin
      buf[aCount shr 2] := 0;
      System.Move(p^, buf, aCount);
      case aCount shr 2 of
        1:
          begin
            k1 := buf[0] * m32;
            k4 := buf[1];
            Result := (Result * m32) xor ((k1 xor k1 shr 24) * m32);
            aCount -= 4;
          end;
        2:
          begin
            k1 := buf[0] * m32;
            k2 := buf[1] * m32;
            k4 := buf[2];
            Result := (((Result * m32) xor ((k1 xor k1 shr 24) * m32)) * m32) xor ((k2 xor k2 shr 24) * m32);
            aCount -= 8;
          end;
        3:
          begin
            k1 := buf[0] * m32;
            k2 := buf[1] * m32;
            k3 := buf[2] * m32;
            k4 := buf[3];
            Result := (((Result * m32) xor ((k1 xor k1 shr 24) * m32)) * m32) xor ((k2 xor k2 shr 24) * m32);
            Result := (Result * m32) xor (k3 xor k3 shr 24) * m32;
            aCount -= 12;
          end;
      else
        k4 := buf[0];
      end;
      if aCount > 0 then
        Result := (Result xor k4) * m32;
    end;
  Result := (Result xor Result shr 13) * m32;
  Result :=  Result xor Result shr 15;
end;
{$POP}{$ENDIF FPC_REQUIRES_PROPER_ALIGNMENT}

class function TMurmur2LE.HashBuf(aBuffer: Pointer; aCount: SizeInt; aSeed: DWord): DWord;
var
  k1, k2, k3, k4: DWord;
  p: PDWord absolute aBuffer;
begin
{$IFDEF FPC_REQUIRES_PROPER_ALIGNMENT}{$PUSH}{$WARN 4055 OFF}
  if SizeUInt(aBuffer) and 3 <> 0 then
    exit(HashBufUnalign(aBuffer, aCount, aSeed));
{$POP}{$ENDIF FPC_REQUIRES_PROPER_ALIGNMENT}
  Result := aSeed xor DWord(aCount);
  while aCount >= 16 do
    begin
      k1 := p[0] * m32;
      k2 := p[1] * m32;
      k3 := p[2] * m32;
      k4 := p[3] * m32;
      Result := (((Result * m32) xor ((k1 xor k1 shr 24) * m32)) * m32) xor ((k2 xor k2 shr 24) * m32);
      Result := (((Result * m32) xor ((k3 xor k3 shr 24) * m32)) * m32) xor ((k4 xor k4 shr 24) * m32);
      p += 4;
      aCount -= 16;
    end;
  case aCount shr 2 of
    1:
      begin
        k1 := p[0] * m32;
        Result := (Result * m32) xor ((k1 xor k1 shr 24) * m32);
        p += 1;
        aCount -= 4;
      end;
    2:
      begin
        k1 := p[0] * m32;
        k2 := p[1] * m32;
        Result := (((Result * m32) xor ((k1 xor k1 shr 24) * m32)) * m32) xor ((k2 xor k2 shr 24) * m32);
        p += 2;
        aCount -= 8;
      end;
    3:
      begin
        k1 := p[0] * m32;
        k2 := p[1] * m32;
        k3 := p[2] * m32;
        Result := (((Result * m32) xor ((k1 xor k1 shr 24) * m32)) * m32) xor ((k2 xor k2 shr 24) * m32);
        Result := (Result * m32) xor (k3 xor k3 shr 24) * m32;
        p += 3;
        aCount -= 12;
      end;
  end;
  if aCount > 0 then
    begin
      k4 := 0;
      case aCount of
        1: PByte(@k4)^  := PByte(p)^;
        2: PByte2(@k4)^ := PByte2(p)^;
        3: PByte3(@k4)^ := PByte3(p)^;
      end;
      Result := (Result xor k4) * m32;
    end;
  Result := (Result xor Result shr 13) * m32;
  Result :=  Result xor Result shr 15;
end;

class function TMurmur2LE.HashStr(const aValue: rawbytestring; aSeed: DWord): DWord;
begin
  Result := HashBuf(Pointer(aValue), System.Length(aValue), aSeed);
end;

class function TMurmur2LE.HashWord(aValue: Word; aSeed: DWord): DWord;
begin
  Result := ((aSeed xor 2) xor DWord(aValue)) * m32;
  Result := (Result xor Result shr 13) * m32;
  Result :=  Result xor Result shr 15;
end;

class function TMurmur2LE.HashDWord(aValue: DWord; aSeed: DWord): DWord;
begin
  aValue *= m32;
  Result := ((aSeed xor 4) * m32) xor ((aValue xor aValue shr 24) * m32);
  Result := (Result xor Result shr 13) * m32;
  Result :=  Result xor Result shr 15;
end;

class function TMurmur2LE.HashQWord(aValue: QWord; aSeed: DWord): DWord;
var
  k1, k2: DWord;
begin
  k1 := PDWord(@aValue)[0] * m32;
  k2 := PDWord(@aValue)[1] * m32;
  Result := ((((aSeed xor 8) * m32) xor ((k1 xor k1 shr 24) * m32)) * m32) xor ((k2 xor k2 shr 24) * m32);
  Result := (Result xor Result shr 13) * m32;
  Result :=  Result xor Result shr 15;
end;

class function TMurmur2LE.HashGuid(const aValue: TGuid; aSeed: DWord): DWord;
var
  k1, k2, k3, k4: DWord;
begin
  Result := aSeed xor DWord(SizeOf(aValue));
  k1 := PDWord(@aValue)[0] * m32;
  k2 := PDWord(@aValue)[1] * m32;
  k3 := PDWord(@aValue)[2] * m32;
  k4 := PDWord(@aValue)[3] * m32;
  Result := (((Result * m32) xor ((k1 xor k1 shr 24) * m32)) * m32) xor ((k2 xor k2 shr 24) * m32);
  Result := (((Result * m32) xor ((k3 xor k3 shr 24) * m32)) * m32) xor ((k4 xor k4 shr 24) * m32);
  Result := (Result xor Result shr 13) * m32;
  Result :=  Result xor Result shr 15;
end;

{ TMurmur2aLE }

{$IFDEF FPC_REQUIRES_PROPER_ALIGNMENT}{$PUSH}{$WARN 5057 OFF}
class function TMurmur2aLE.HashBufUnalign(aBuffer: Pointer; aCount: SizeInt; aSeed: DWord): DWord;
var
  k1, k2, k3, k4: DWord;
  buf: array[0..3] of DWord;
  p: PByte absolute aBuffer;
begin
  Result := aSeed;
  aSeed := DWord(aCount) * m32;
  while aCount >= 16 do
    begin
      System.Move(p^, buf, 16);
      k1 := buf[0] * m32;
      k2 := buf[1] * m32;
      k3 := buf[2] * m32;
      k4 := buf[3] * m32;
      Result := (((Result * m32) xor ((k1 xor k1 shr 24) * m32)) * m32) xor ((k2 xor k2 shr 24) * m32);
      Result := (((Result * m32) xor ((k3 xor k3 shr 24) * m32)) * m32) xor ((k4 xor k4 shr 24) * m32);
      p += 16;
      aCount -= 16;
    end;
  if aCount > 0 then
    begin
      buf[aCount shr 2] := 0;
      System.Move(p^, buf, aCount);
      case aCount shr 2 of
        1:
          begin
            k1 := buf[0] * m32;
            k4 := buf[1];
            Result := (Result * m32) xor ((k1 xor k1 shr 24) * m32);
            aCount -= 4;
          end;
        2:
          begin
            k1 := buf[0] * m32;
            k2 := buf[1] * m32;
            k4 := buf[2];
            Result := (((Result * m32) xor ((k1 xor k1 shr 24) * m32)) * m32) xor ((k2 xor k2 shr 24) * m32);
            aCount -= 8;
          end;
        3:
          begin
            k1 := buf[0] * m32;
            k2 := buf[1] * m32;
            k3 := buf[2] * m32;
            k4 := buf[3];
            Result := (((Result * m32) xor ((k1 xor k1 shr 24) * m32)) * m32) xor ((k2 xor k2 shr 24) * m32);
            Result := (Result * m32) xor (k3 xor k3 shr 24) * m32;
            aCount -= 12;
          end;
      else
        k4 := buf[0];
      end;
    end
  else
    k4 := 0;
  k4 := k4 * m32;
  Result := (Result * m32) xor ((k4 xor k4 shr 24) * m32);
  Result := (Result * m32) xor ((aSeed xor aSeed shr 24) * m32);
  Result := (Result xor Result shr 13) * m32;
  Result :=  Result xor Result shr 15;
end;
{$POP}{$ENDIF FPC_REQUIRES_PROPER_ALIGNMENT}

class function TMurmur2aLE.HashBuf(aBuffer: Pointer; aCount: SizeInt; aSeed: DWord): DWord;
var
  k1, k2, k3, k4: DWord;
  p: PDWord absolute aBuffer;
begin
{$IFDEF FPC_REQUIRES_PROPER_ALIGNMENT}{$PUSH}{$WARN 4055 OFF}
  if SizeUInt(aBuffer) and 3 <> 0 then
    exit(HashBufUnalign(aBuffer, aCount, aSeed));
{$POP}{$ENDIF FPC_REQUIRES_PROPER_ALIGNMENT}
  Result := aSeed;
  aSeed := DWord(aCount) * m32;
  while aCount >= 16 do
    begin
      k1 := p[0] * m32;
      k2 := p[1] * m32;
      k3 := p[2] * m32;
      k4 := p[3] * m32;
      Result := (((Result * m32) xor ((k1 xor k1 shr 24) * m32)) * m32) xor ((k2 xor k2 shr 24) * m32);
      Result := (((Result * m32) xor ((k3 xor k3 shr 24) * m32)) * m32) xor ((k4 xor k4 shr 24) * m32);
      p += 4;
      aCount -= 16;
    end;
  case aCount shr 2 of
    1:
      begin
        k1 := p[0] * m32;
        Result := (Result * m32) xor ((k1 xor k1 shr 24) * m32);
        p += 1;
        aCount -= 4;
      end;
    2:
      begin
        k1 := p[0] * m32;
        k2 := p[1] * m32;
        Result := (((Result * m32) xor ((k1 xor k1 shr 24) * m32)) * m32) xor ((k2 xor k2 shr 24) * m32);
        p += 2;
        aCount -= 8;
      end;
    3:
      begin
        k1 := p[0] * m32;
        k2 := p[1] * m32;
        k3 := p[2] * m32;
        Result := (((Result * m32) xor ((k1 xor k1 shr 24) * m32)) * m32) xor ((k2 xor k2 shr 24) * m32);
        Result := (Result * m32) xor (k3 xor k3 shr 24) * m32;
        p += 3;
        aCount -= 12;
      end;
  end;
  Result *= m32;///////////////
  if aCount > 0 then
    begin
      k4 := 0;
      case aCount of
        1: PByte(@k4)^  := PByte(p)^;
        2: PByte2(@k4)^ := PByte2(p)^;
        3: PByte3(@k4)^ := PByte3(p)^;
      end;
      k4 := k4 * m32;
      Result := Result xor (k4 xor k4 shr 24) * m32;
    end;
  Result := (Result * m32) xor ((aSeed xor aSeed shr 24) * m32);  ////
  Result := (Result xor Result shr 13) * m32;
  Result :=  Result xor Result shr 15;
end;

class function TMurmur2aLE.HashStr(const aValue: rawbytestring; aSeed: DWord): DWord;
begin
  Result := HashBuf(PAnsiChar(aValue), System.Length(aValue), aSeed);
end;

class function TMurmur2aLE.HashWord(aValue: Word; aSeed: DWord): DWord;
begin
  //((m32 shl 1) xor (m32 shl 1) shr 24) * m32 = $90210F61
  Result := aSeed * m32;
  aSeed := aValue * m32;
  Result := ((Result xor ((aSeed xor aSeed shr 24) * m32)) * m32) xor $90210F61;
  Result := (Result xor Result shr 13) * m32;
  Result :=  Result xor Result shr 15;
end;

class function TMurmur2aLE.HashDWord(aValue: DWord; aSeed: DWord): DWord;
begin
  //((m32 shl 2) xor (m32 shl 2) shr 24) * m32 = $AA2A7357
  //m32^2 = $286A90B9
  aValue *= m32;
  Result := (((aSeed * m32) xor ((aValue xor aValue shr 24) * m32)) * $286A90B9) xor $AA2A7357;
  Result := (Result xor Result shr 13) * m32;
  Result :=  Result xor Result shr 15;
end;

class function TMurmur2aLE.HashQWord(aValue: QWord; aSeed: DWord): DWord;
var
  k1, k2: DWord;
begin
  //((m32 shl 3) xor (m32 shl 3) shr 24) * m32 = $5454E6AE
  //m32^2 = $286A90B9
  k1 := PDWord(@aValue)[0] * m32;
  k2 := PDWord(@aValue)[1] * m32;
  Result := (((aSeed * m32) xor ((k1 xor k1 shr 24) * m32)) * m32) xor ((k2 xor k2 shr 24) * m32);
  Result := (Result * $286A90B9) xor $5454E6AE;
  Result := (Result xor Result shr 13) * m32;
  Result :=  Result xor Result shr 15;
end;

class function TMurmur2aLE.HashGuid(const aValue: TGuid; aSeed: DWord): DWord;
var
  k1, k2, k3, k4: DWord;
begin
  //((m32 shl 3) xor (m32 shl 3) shr 24) * m32 = $D6654BF1
  //m32^2 = $286A90B9
  k1 := PDWord(@aValue)[0] * m32;
  k2 := PDWord(@aValue)[1] * m32;
  k3 := PDWord(@aValue)[2] * m32;
  k4 := PDWord(@aValue)[3] * m32;
  Result := (((aSeed  * m32) xor ((k1 xor k1 shr 24) * m32)) * m32) xor ((k2 xor k2 shr 24) * m32);
  Result := (((Result * m32) xor ((k3 xor k3 shr 24) * m32)) * m32) xor ((k4 xor k4 shr 24) * m32);
  Result := (Result * $286A90B9) xor $D6654BF1;
  Result := (Result xor Result shr 13) * m32;
  Result :=  Result xor Result shr 15;
end;
{$UNDEF m32}

{ TMurmur3LE }

{$DEFINE c1 := DWord($cc9e2d51)}{$DEFINE c2 := DWord($1b873593)}{$DEFINE c3 := DWord($e6546b64)}
{$DEFINE c4 := DWord($85ebca6b)}{$DEFINE c5 := DWord($c2b2ae35)}

{$IFDEF FPC_REQUIRES_PROPER_ALIGNMENT}{$PUSH}{$WARN 5057 OFF}
class function TMurmur3LE.HashBufUnalign(aBuffer: Pointer; aCount: SizeInt; aSeed: DWord): DWord;
var
  k1, k2, k3, k4: DWord;
  buf: array[0..3] of DWord;
  p: PByte absolute aBuffer;
begin
  Result := aSeed;
  aSeed := DWord(aCount);
  while aCount >= 16 do
    begin
      System.Move(p^, buf, 16);
      k1 := RolDWord(buf[0] * c1, 15) * c2;
      k2 := RolDWord(buf[1] * c1, 15) * c2;
      k3 := RolDWord(buf[2] * c1, 15) * c2;
      k4 := RolDWord(buf[3] * c1, 15) * c2;
      Result := RolDWord((RolDWord(Result xor k1, 13) * 5 + c3) xor k2, 13) * 5 + c3;
      Result := RolDWord((RolDWord(Result xor k3, 13) * 5 + c3) xor k4, 13) * 5 + c3;
      p += 16;
      aCount -= 16;
    end;
  if aCount > 0 then
    begin
      buf[aCount shr 2] := 0;
      System.Move(p^, buf, aCount);
      case aCount shr 2 of
        1:
          begin
            Result := RolDWord(Result xor (RolDWord(buf[0] * c1, 15) * c2), 13) * 5 + c3;
            k4 := buf[1];
            aCount -= 4;
          end;
        2:
          begin
            k1 := RolDWord(buf[0] * c1, 15) * c2;
            k2 := RolDWord(buf[1] * c1, 15) * c2;
            k4 := buf[2];
            Result := RolDWord((RolDWord(Result xor k1, 13) * 5 + c3) xor k2, 13) * 5 + c3;
            aCount -= 8;
          end;
        3:
          begin
            k1 := RolDWord(buf[0] * c1, 15) * c2;
            k2 := RolDWord(buf[1] * c1, 15) * c2;
            k3 := RolDWord(buf[2] * c1, 15) * c2;
            k4 := buf[3];
            Result := RolDWord((RolDWord(Result xor k1, 13) * 5 + c3) xor k2, 13) * 5 + c3;
            Result := RolDWord(Result xor k3, 13) * 5 + c3;
            aCount -= 12;
          end;
      else
        k4 := buf[0];
      end;
    end
  else
    k4 := 0;
  Result := Result xor (RolDWord(k4 * c1, 15) * c2);
  Result :=  Result xor aSeed;
  Result := (Result xor Result shr 16) * c4;
  Result := (Result xor Result shr 13) * c5;
  Result :=  Result xor Result shr 16;
end;
{$POP}{$ENDIF FPC_REQUIRES_PROPER_ALIGNMENT}

class function TMurmur3LE.HashBuf(aBuffer: Pointer; aCount: SizeInt; aSeed: DWord): DWord;
var
  k1, k2, k3, k4: DWord;
  p: PDWord absolute aBuffer;
begin
{$IFDEF FPC_REQUIRES_PROPER_ALIGNMENT}{$PUSH}{$WARN 4055 OFF}
  if SizeUInt(aBuffer) and 3 <> 0 then
    exit(HashBufUnalign(aBuffer, aCount, aSeed));
{$POP}{$ENDIF FPC_REQUIRES_PROPER_ALIGNMENT}
  Result := aSeed;
  aSeed := DWord(aCount);
  while aCount >= 16 do
    begin
      k1 := RolDWord(p[0] * c1, 15) * c2;
      k2 := RolDWord(p[1] * c1, 15) * c2;
      k3 := RolDWord(p[2] * c1, 15) * c2;
      k4 := RolDWord(p[3] * c1, 15) * c2;
      Result := RolDWord((RolDWord(Result xor k1, 13) * 5 + c3) xor k2, 13) * 5 + c3;
      Result := RolDWord((RolDWord(Result xor k3, 13) * 5 + c3) xor k4, 13) * 5 + c3;
      p += 4;
      aCount -= 16;
    end;
  case aCount shr 2 of
    1:
      begin
        Result := RolDWord(Result xor (RolDWord(p[0] * c1, 15) * c2), 13) * 5 + c3;
        p += 1;
        aCount -= 4;
      end;
    2:
      begin
        k1 := RolDWord(p[0] * c1, 15) * c2;
        k2 := RolDWord(p[1] * c1, 15) * c2;
        Result := RolDWord((RolDWord(Result xor k1, 13) * 5 + c3) xor k2, 13) * 5 + c3;
        p += 2;
        aCount -= 8;
      end;
    3:
      begin
        k1 := RolDWord(p[0] * c1, 15) * c2;
        k2 := RolDWord(p[1] * c1, 15) * c2;
        Result := RolDWord((RolDWord(Result xor k1, 13) * 5 + c3) xor k2, 13) * 5 + c3;
        Result := RolDWord(Result xor (RolDWord(p[2] * c1, 15) * c2), 13) * 5 + c3;
        p += 3;
        aCount -= 12;
      end;
  end;
  if aCount > 0 then
    begin
      k4 := 0;
      case aCount of
        1: PByte(@k4)^  := PByte(p)^;
        2: PByte2(@k4)^ := PByte2(p)^;
        3: PByte3(@k4)^ := PByte3(p)^;
      end;
      Result := Result xor (RolDWord(k4 * c1, 15) * c2);
    end;
  Result := Result xor aSeed;
  Result := (Result xor Result shr 16) * c4;
  Result := (Result xor Result shr 13) * c5;
  Result := Result xor Result shr 16;
end;

class function TMurmur3LE.HashStr(const aValue: rawbytestring; aSeed: DWord): DWord;
begin
  Result := HashBuf(Pointer(aValue), System.Length(aValue), aSeed);
end;

class function TMurmur3LE.HashWord(aValue: Word; aSeed: DWord): DWord;
begin
  Result := (aSeed xor (RolDWord(DWord(aValue) * c1, 15) * c2)) xor 2;
  Result := (Result xor Result shr 16) * c4;
  Result := (Result xor Result shr 13) * c5;
  Result := Result xor Result shr 16;
end;

class function TMurmur3LE.HashDWord(aValue: DWord; aSeed: DWord): DWord;
begin
  Result := (RolDWord(aSeed xor (RolDWord(aValue * c1, 15) * c2), 13) * 5 + c3) xor 4;
  Result := (Result xor Result shr 16) * c4;
  Result := (Result xor Result shr 13) * c5;
  Result := Result xor Result shr 16;
end;

class function TMurmur3LE.HashQWord(aValue: QWord; aSeed: DWord): DWord;
var
  k1, k2: DWord;
begin
  k1 := RolDWord(PDWord(@aValue)[0] * c1, 15) * c2;
  k2 := RolDWord(PDWord(@aValue)[1] * c1, 15) * c2;
  Result := (RolDWord((RolDWord(aSeed xor k1, 13) * 5 + c3) xor k2, 13) * 5 + c3) xor 8;
  Result := (Result xor Result shr 16) * c4;
  Result := (Result xor Result shr 13) * c5;
  Result :=  Result xor Result shr 16;
end;

class function TMurmur3LE.HashGuid(const aValue: TGuid; aSeed: DWord): DWord;
var
  g: TDWords4 absolute aValue;
begin

  Result :=  RolDWord((RolDWord(aSeed  xor DWord(RolDWord(g.D1 * c1, 15) * c2), 13) * 5 + c3)
                                  {%H-}xor DWord(RolDWord(g.D2 * c1, 15) * c2), 13) * 5 + c3;
  Result := (RolDWord((RolDWord(Result xor DWord(RolDWord(g.D3 * c1, 15) * c2), 13) * 5 + c3)
                                  {%H-}xor DWord(RolDWord(g.D4 * c1, 15) * c2), 13) * 5 + c3)
                                       xor DWord(SizeOf(aValue));

  Result := (Result xor Result shr 16) * c4;
  Result := (Result xor Result shr 13) * c5;
  Result := Result xor Result shr 16;
end;

{ TIncMurmur3LE }

procedure TIncMurmur3LE.Reset(aSeed: DWord);
begin
  FTotalBytes := 0;
  FHash := aSeed;
  FBufIndex := 0;
end;

procedure TIncMurmur3LE.Add(const aBuffer; aCount: SizeInt);
var
  p: PByte;
  h: DWord;
  ByteCount: Integer;
begin
  if aCount < 1 then exit;
  Inc(FTotalBytes, aCount);
  p := @aBuffer;
  repeat
    ByteCount := IntMin(SizeOf(FBuffer)-FBufIndex, aCount);
    System.Move(p^, PByte(@FBuffer)[FBufIndex], ByteCount);
    Dec(aCount, ByteCount);
    Inc(FBufIndex, ByteCount);
    Inc(p, ByteCount);
    if FBufIndex = SizeOf(FBuffer) then
      begin
        h := RolDWord(FHash xor RolDWord(FBuffer[0] * c1, 15) * c2, 13) * 5 + c3;
        h := RolDWord(h xor RolDWord(FBuffer[1] * c1, 15) * c2, 13) * 5 + c3;
        h := RolDWord(h xor RolDWord(FBuffer[2] * c1, 15) * c2, 13) * 5 + c3;
        FHash := RolDWord(h xor RolDWord(FBuffer[3] * c1, 15) * c2, 13) * 5 + c3;
        FBufIndex := 0;
      end;
  until aCount = 0;
end;

function TIncMurmur3LE.Hash: DWord;
type
  TByte3 = array[0..2] of Byte;
  PByte3 = ^TByte3;
var
  p: PByte;
  h, k: DWord;
  ByteCount: Integer;
begin
  p := @FBuffer;
  ByteCount := FBufIndex;
  h := FHash;
  case ByteCount shr 2 of
    1:
      begin
        h := RolDWord(h xor RolDWord(FBuffer[0] * c1, 15) * c2, 13) * 5 + c3;
        p += 4;
        ByteCount -= 4;
      end;
    2:
      begin
        h := RolDWord(h xor RolDWord(FBuffer[0] * c1, 15) * c2, 13) * 5 + c3;
        h := RolDWord(h xor RolDWord(FBuffer[1] * c1, 15) * c2, 13) * 5 + c3;
        p += 8;
        ByteCount -= 8;
      end;
    3:
      begin
        h := RolDWord(h xor RolDWord(FBuffer[0] * c1, 15) * c2, 13) * 5 + c3;
        h := RolDWord(h xor RolDWord(FBuffer[1] * c1, 15) * c2, 13) * 5 + c3;
        h := RolDWord(h xor RolDWord(FBuffer[2] * c1, 15) * c2, 13) * 5 + c3;
        p += 12;
        ByteCount -= 12;
      end;
  end;
  k := 0;
  case ByteCount of
    1: PByte(@k)^  := PByte(p)^;
    2: PWord(@k)^  := PWord(p)^;
    3: PByte3(@k)^ := PByte3(p)^;
  end;
  h := h xor (RolDWord(k * c1, 15) * c2) xor FTotalBytes;
  h := (h xor h shr 16) * c4;
  h := (h xor h shr 13) * c5;
  Result := h xor h shr 16;
end;
{$UNDEF c1}{$UNDEF c2}{$UNDEF c3}{$UNDEF c4}{$UNDEF c5}

{ TMurmur64aLE }

{$DEFINE m64 := QWord($c6a4a7935bd1e995)}
{$IFDEF FPC_REQUIRES_PROPER_ALIGNMENT}{$PUSH}{$WARN 5057 OFF}
class function TMurmur64aLE.HashBufUnalign(aBuffer: Pointer; aCount: SizeInt; aSeed: QWord): QWord;
var
  k1, k2, k3, k4: QWord;
  buf: array[0..3] of QWord;
  p: PByte absolute aBuffer;
begin
  Result := aSeed xor (QWord(aCount) * m64);
  while aCount >= 32 do
    begin
      System.Move(p^, buf, 32);
      k1 := buf[0] * m64;
      k2 := buf[1] * m64;
      k3 := buf[2] * m64;
      k4 := buf[3] * m64;
      Result := (((Result xor ((k1 xor k1 shr 47) * m64)) * m64) xor ((k2 xor k2 shr 47) * m64)) * m64;
      Result := (((Result xor ((k3 xor k3 shr 47) * m64)) * m64) xor ((k4 xor k4 shr 47) * m64)) * m64;
      p += 32;
      aCount -= 32;
    end;
  if aCount > 0 then
    begin
      buf[aCount shr 3] := 0;
      System.Move(p^, buf, aCount);
      p := @buf;
      case aCount shr 3 of
        1:
          begin
            k1 := buf[0] * m64;
            k4 := buf[1];
            Result := (Result xor ((k1 xor k1 shr 47) * m64)) * m64;
            aCount -= 8;
          end;
        2:
          begin
            k1 := buf[0] * m64;
            k2 := buf[1] * m64;
            k4 := buf[2];
            Result := (((Result xor ((k1 xor k1 shr 47) * m64)) * m64) xor ((k2 xor k2 shr 47) * m64)) * m64;
            aCount -= 16;
          end;
        3:
          begin
            k1 := buf[0] * m64;
            k2 := buf[1] * m64;
            k3 := buf[2] * m64;
            k4 := buf[3];
            Result := (((Result xor ((k1 xor k1 shr 47) * m64)) * m64) xor ((k2 xor k2 shr 47) * m64)) * m64;
            Result := (Result xor ((k3 xor k3 shr 47) * m64)) * m64;
            aCount -= 24;
          end;
      else
        k4 := buf[0];
      end;
      if aCount > 0 then
        Result := (Result xor k4) * m64;
    end;
  Result := (Result xor Result shr 47) * m64;
  Result :=  Result xor Result shr 47;
end;
{$POP}{$ENDIF FPC_REQUIRES_PROPER_ALIGNMENT}

class function TMurmur64aLE.HashBuf(aBuffer: Pointer; aCount: SizeInt; aSeed: QWord): QWord;
var
  k1, k2, k3, k4: QWord;
  p: PQWord absolute aBuffer;
begin
{$IFDEF FPC_REQUIRES_PROPER_ALIGNMENT}{$PUSH}{$WARN 4055 OFF}
  if SizeUInt(aBuffer) and 7 <> 0 then
    exit(HashBufUnalign(aBuffer, aCount, aSeed));
{$POP}{$ENDIF FPC_REQUIRES_PROPER_ALIGNMENT}
  Result := aSeed xor (QWord(aCount) * m64);
  while aCount >= 32 do
    begin
      k1 := p[0] * m64;
      k2 := p[1] * m64;
      k3 := p[2] * m64;
      k4 := p[3] * m64;
      Result := (((Result xor ((k1 xor k1 shr 47) * m64)) * m64) xor ((k2 xor k2 shr 47) * m64)) * m64;
      Result := (((Result xor ((k3 xor k3 shr 47) * m64)) * m64) xor ((k4 xor k4 shr 47) * m64)) * m64;
      p += 4;
      aCount -= 32;
    end;
  case aCount shr 3 of
    1:
      begin
        k1 := p[0] * m64;
        Result := (Result xor ((k1 xor k1 shr 47) * m64)) * m64;
        aCount -= 8;
        p += 1;
      end;
    2:
      begin
        k1 := p[0] * m64;
        k2 := p[1] * m64;
        Result := (((Result xor ((k1 xor k1 shr 47) * m64)) * m64) xor ((k2 xor k2 shr 47) * m64)) * m64;
        aCount -= 16;
        p += 2;
      end;
    3:
      begin
        k1 := p[0] * m64;
        k2 := p[1] * m64;
        k3 := p[2] * m64;
        Result := (((Result xor ((k1 xor k1 shr 47) * m64)) * m64) xor ((k2 xor k2 shr 47) * m64)) * m64;
        Result := (Result xor ((k3 xor k3 shr 47) * m64)) * m64;
        aCount -= 24;
        p += 3;
      end;
  end;
  if aCount > 0 then
    begin
      k4 := 0;
      case aCount of
        1: PByte( @k4)^ := PByte( p)^;
        2: PByte2(@k4)^ := PByte2( p)^;
        3: PByte3(@k4)^ := PByte3(p)^;
        4: PByte4(@k4)^ := PByte4(p)^;
        5: PByte5(@k4)^ := PByte5(p)^;
        6: PByte6(@k4)^ := PByte6(p)^;
        7: PByte7(@k4)^ := PByte7(p)^;
      end;
      Result := (Result xor k4) * m64;
    end;
  Result := (Result xor Result shr 47) * m64;
  Result :=  Result xor Result shr 47;
end;

class function TMurmur64aLE.HashStr(const aValue: rawbytestring; aSeed: QWord): QWord;
begin
  Result := HashBuf(Pointer(aValue), System.Length(aValue), aSeed);
end;

class function TMurmur64aLE.HashWord(aValue: Word; aSeed: QWord): QWord;
begin
  Result := ((aSeed xor m64 shl 1) xor QWord(aValue)) * m64;
  Result := (Result xor Result shr 47) * m64;
  Result :=  Result xor Result shr 47;
end;

class function TMurmur64aLE.HashDWord(aValue: DWord; aSeed: QWord): QWord;
begin
  Result := QWord((aSeed xor m64 shl 2) xor QWord(aValue)) * m64;
  Result := (Result xor Result shr 47) * m64;
  Result :=  Result xor Result shr 47;
end;

class function TMurmur64aLE.HashQWord(aValue: QWord; aSeed: QWord): QWord;
begin
  aValue := aValue * m64;
  Result := QWord((aSeed xor (m64 shl 3)) xor ((aValue xor aValue shr 47) * m64)) * m64;
  Result := (Result xor Result shr 47) * m64;
  Result :=  Result xor Result shr 47;
end;

{$IFDEF FPC_REQUIRES_PROPER_ALIGNMENT}
class function TMurmur64aLE.HashGuid(const aValue: TGuid; aSeed: QWord): QWord;
begin
  Result := HashBuf(@aValue, SizeOf(aValue), aSeed);
end;
{$ELSE FPC_REQUIRES_PROPER_ALIGNMENT}
class function TMurmur64aLE.HashGuid(const aValue: TGuid; aSeed: QWord): QWord;
var
  k1, k2: QWord;
begin
  k1 := PQWord(@aValue)[0] * m64;
  k2 := PQWord(@aValue)[1] * m64;
  Result := ((((aSeed xor QWord(m64 shl 4)) xor ((k1 xor k1 shr 47) * m64)) * m64) xor
              ((k2 xor k2 shr 47) * m64)) * m64;
  Result := (Result xor Result shr 47) * m64;
  Result :=  Result xor Result shr 47;
end;
{$ENDIF FPC_REQUIRES_PROPER_ALIGNMENT}
{$UNDEF m64}

end.

