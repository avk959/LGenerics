unit bench_util;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, lgUtils, lgArrayHelpers, lgVector;

type
  TMapEntry    = specialize TGMapEntry<string, Integer>;
  TEntryHelper = specialize TGArrayHelpUtil<TMapEntry>;
  TClocks      = specialize TGLiteVector<QWord>;
  TClockHelper = specialize TGComparableVectorHelper<QWord>;
  TEntryArray  = array of TMapEntry;
  TKeySize     = (ks8, ks16, ks32, ks64, ks128, ks250);
  TBenchBy     = (bbBest, bbMedian);

const
  DEFAULT_SEED     = 31415926;
  DEFAULT_SIZE     = 1000000;
  DEFAULT_INTERVAL = 5000;
  FAIL_TIME        = -20;

  KeySizes: array[TKeySize] of Integer = (8, 16, 32, 64, 128, 250);
  Colon: Char = ':';

  function  RDTSC: QWord;
  procedure CheckCpuSpeed;
  function  RandomString(aLength: Integer): string;

{$PUSH}{$J+}
const
  CpuClockPeriod: Double = 0.0;
{$POP}

implementation
{$B-}{$COPERATORS ON}

{$ASMMODE INTEL}
function RDTSC: QWord; assembler; nostackframe;
asm
  rdtscp
{$IFDEF CPUAMD64}
  shl rdx, 32
  or  rax, rdx
{$ENDIF}
end;

procedure CheckCpuSpeed;
var
  Start, Stop, m, n: QWord;
begin
  Start := GetTickCount64;
  while Start = GetTickCount64 do;
  m := RDTSC;
  Stop := Start + 1000;
  while GetTickCount64 < Stop do;
  n := RDTSC;
  CpuClockPeriod := 1000.0/(n - m);
end;

function RandomString(aLength: Integer): string;
var
  I: Integer;
  p: PChar;
const
  StartWith: PChar = 'it_is_a_begin_of_long_string_key';
  AlphaLen         = 64;
  Alphabet: PChar  = '1234567890ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz_ ';
begin
  SetLength(Result, aLength);
  p := PChar(Result);
  if aLength > 32 then
    begin
      Move(StartWith^, p^, 32);
      for I := 32 to Pred(aLength) do
        p[I] := Alphabet[Random(AlphaLen)];
    end
  else
    for I := 0 to Pred(aLength) do
      p[I] := Alphabet[Random(AlphaLen)];
end;

end.

