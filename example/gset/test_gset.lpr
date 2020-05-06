program test_gset;

{$mode objfpc}

uses
  SysUtils, DateUtils, LGUtils;

type
  TByteSet  = set of Byte;
  TGByteSet = specialize TGSet<Byte>;
  TMyRange  = Low(Integer) div 33..High(Integer) div 33;

procedure ByteSetInclude;
var
  s: TByteSet;
  I, J: Integer;
begin
  for I := 1 to 1000000 do
    begin
      s := [];
      for J := 0 to 255 do
        Include(s, J);
    end;
end;

procedure GByteSetInclude;
var
  s: TGByteSet;
  I, J: Integer;
begin
  for I := 1 to 1000000 do
    begin
      s.Clear;
      for J := 0 to 255 do
        s.Include(J);
    end;
end;

procedure ByteSetContains;
var
  s: TByteSet;
  I: Integer;
begin
  s := [];
  for I := 1 to 200000000 do
    if (I and 255) in s then;
end;

procedure GByteSetContains;
var
  s: TGByteSet;
  I: Integer;
begin
  for I := 1 to 200000000 do
    if (I and 255) {%H-}in s then;
end;

procedure ByteSetIntersection;
var
  s, s1, s2: TByteSet;
  I: Integer;
begin
  s1 := [];
  s2 := [];
  for I := 0 to 255 do
    if Odd(I) then
      Include(s1, I)
    else
      Include(s2, I);
  for I := 1 to 10000000 do
    s := s1 * s2;
end;

procedure GByteSetIntersection;
var
  s, s1, s2: TGByteSet;
  I: Integer;
begin
  for I := 0 to 255 do
    if Odd(I) then
      {%H-}s1.Include(I)
    else
      {%H-}s2.Include(I);
  for I := 1 to 10000000 do
    s := s1 * s2;
end;

procedure ByteSetIteration;
var
  s: TByteSet;
  I, J: Integer;
begin
  I := 0;
  s := [];
  while I < 256 do
    begin
      Include(s, I);
      Inc(I, 8);
    end;
  for I := 1 to 1000000 do
    for J in s do;
end;

procedure GByteSetIteration;
var
  s: TGByteSet;
  I, J: Integer;
begin
  I := 0;
  while I < 256 do
    begin
      {%H-}s.Include(I);
      Inc(I, 8);
    end;
  for I := 1 to 1000000 do
    for J in s do;
end;

procedure ByteSetIteration2;
var
  s: TByteSet;
  I, J: Integer;
begin
  I := 0;
  s := [];
  for I := 0 to 255 do
    Include(s, I);
  for I := 1 to 1000000 do
    for J in s do;
end;

procedure GByteSetIteration2;
var
  s: TGByteSet;
  I, J: Integer;
begin
  I := 0;
  for I := 0 to 255 do
    {%H-}s.Include(I);
  for I := 1 to 1000000 do
    for J in s.DenseItems do;
end;

var
  s: specialize TGSet<SmallInt>;
  s2: specialize TGSet<TMyRange>;
  I: SmallInt;
  J: TMyRange;
  Start: TDateTime;
begin
  WriteLn('built-in set - size:              ', SizeOf(TByteSet));
  WriteLn('generic set  - size:              ', SizeOf(TGByteSet));
  Start := Now;
  ByteSetInclude;
  WriteLn('built-in set - include:           ', MillisecondsBetween(Now, Start), ' ms');

  Start := Now;
  GByteSetInclude;
  WriteLn('generic set  - include:           ', MillisecondsBetween(Now, Start), ' ms');

  Start := Now;
  ByteSetContains;
  WriteLn('built-in set - contains:          ', MillisecondsBetween(Now, Start), ' ms');

  Start := Now;
  GByteSetContains;
  WriteLn('generic set  - contains:          ', MillisecondsBetween(Now, Start), ' ms');

  Start := Now;
  ByteSetIntersection;
  WriteLn('built-in set - intersection:      ', MillisecondsBetween(Now, Start), ' ms');

  Start := Now;
  GByteSetIntersection;
  WriteLn('generic set  - intersection:      ', MillisecondsBetween(Now, Start), ' ms');

  Start := Now;
  ByteSetIteration;
  WriteLn('built-in set - iteration(sparse): ', MillisecondsBetween(Now, Start), ' ms');

  Start := Now;
  GByteSetIteration;
  WriteLn('generic set  - iteration(sparse): ', MillisecondsBetween(Now, Start), ' ms');

  Start := Now;
  ByteSetIteration2;
  WriteLn('built-in set - iteration(dense):  ', MillisecondsBetween(Now, Start), ' ms');

  Start := Now;
  GByteSetIteration2;
  WriteLn('generic set  - iteration(dense):  ', MillisecondsBetween(Now, Start), ' ms');

  WriteLn('SizeOf(TGSet<SmallInt>) = ', SizeOf(s));
  WriteLn('Let''s try to add some elements(-12001, 20000, -1005, -2500, 1500, 31000)');
  s.IncludeArray([-12001, 20000, -1005, -2500, 1500, 31000]);

  WriteLn('Now test it');
  WriteLn('s.Count = ', s.Count);
  for I in s do
    WriteLn(I);

  WriteLn('TGSet<TMyRange> may contain ', High(TMyRange) - Low(TMyRange) + 1, ' elements');
  WriteLn('SizeOf(TGSet<TMyRange>) = ', SizeOf(s2));

  WriteLn('Let''s try to add High(TMyRange) and Low(TMyRange)(',High(TMyRange),', ',Low(TMyRange),')');
  s2.IncludeArray([High(TMyRange), Low(TMyRange)]);

  WriteLn('Ok, now test it');
  WriteLn('s2.Count = ', s2.Count);
  for J in s2 do
    WriteLn(J);
  ReadLn;
end.

