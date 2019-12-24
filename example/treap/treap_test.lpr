{
  Example to demonstrate using TGLiteSegmentTreap.

  Demo task.
    It is required to support some primitive(and frequently updated) payment database
    and respond(quickly) to requests like:
      1. payment amount for the specified period;
      2. the number of payments exceeding a certain limit for the specified period;
    If the database size exceeds some maximum, the oldest record is deleted;

    We'll simulate data input with a random number generator.
    To make sure that our requests are executed quickly, we will execute quite a lot of them.
    The number of requests can be passed as a parameter.
}
program treap_test;

{$MODE OBJFPC}{$H+}
{$INLINE ON}
{$MODESWITCH ADVANCEDRECORDS}

uses
  SysUtils, DateUtils,
  LGUtils,
  LGTreap;

const
  InitSize   = 1000000;
  MaxSize    = 1500000; // maximum size of database
  MaxPay     = 50000;   // maximum possible payment(for data generation)
  PayLimit   = 40000;   // payment threshold of interest

type
  { stored data }
  TPayment = record
    PayTime: Integer;// time of payment - Unix time(key field)
    Pay: Currency;   // amount
  end;

  { helper type }
  TPayData = record
    Pay: Currency;
    Count: Integer;
    class function Identity: TPayData; static;
    class function BinOp(const L, R: TPayData): TPayData; static;
    constructor Create(aPay: Currency);
  end;

  // for storing payments will use segment tree
  TPayList = specialize TGLiteSegmentTreap<Integer, TPayData, Integer, TPayData>;

var
  PeriodBegin: Integer = 0;
  PeriodEnd: Integer = 0;
  QueryCount: Integer = 1000000;// default number of queries

class function TPayData.Identity: TPayData;
begin
  Result := Default(TPayData);
end;

class function TPayData.BinOp(const L, R: TPayData): TPayData;
begin
  Result.Pay := L.Pay + R.Pay;
  Result.Count := L.Count + R.Count;
end;

constructor TPayData.Create(aPay: Currency);
begin
  Pay := aPay;
  Count := Ord(aPay > PayLimit);
end;

function GenRandomTime: Integer; inline;
begin
  Result := PeriodBegin + Random(PeriodEnd - PeriodBegin);
end;

function GenRandomPay: TPayment; inline;
begin
  Result.PayTime := GenRandomTime;
  Result.Pay := Succ(Random(MaxPay * 100))/100;
end;

var
  PayList: TPayList;

procedure CreateList;
var
  I: Integer;
  p: TPayment;
begin
  for I := 1 to InitSize do
    begin
      p := GenRandomPay;
      PayList[p.PayTime] := TPayData.Create(p.Pay);
    end;
end;

procedure Swap(var L, R: Integer); inline;
var
  I: Integer;
begin
  I := L;
  L := R;
  R := I;
end;

procedure UpdateList;
var
  p: TPayment;
begin
  p := GenRandomPay;
  if not PayList.Contains(p.PayTime) and (PayList.Count = MaxSize) then
    PayList.Remove(PayList.Entries[0].Key);
  PayList[p.PayTime] := TPayData.Create(p.Pay);
end;

procedure Run;
var
  I, RBegin, REnd: Integer;
  pd: TPayData;
begin
  for I := 1 to QueryCount do
    begin
      RBegin := GenRandomTime;
      REnd := GenRandomTime;
      if RBegin > REnd then
        Swap(RBegin, REnd);

      //answer to a query on a segment[RBegin, REnd]
      pd := PayList.RangeQuery(RBegin, Succ(REnd));
      //print some results
      if (I <= 5) or (I > QueryCount - 5) then
        begin
          WriteLn('In the period from ', DateTimeToStr(UnixToDateTime(RBegin)),
                  ' to ',DateTimeToStr(UnixToDateTime(REnd)));
          WriteLn('    amount = ', CurrToStr(pd.Pay), ', count = ', pd.Count);
        end;

      UpdateList;
    end;
end;

procedure ReadInput;
var
  qc: Integer;
begin
  PeriodBegin := DateTimeToUnix(EncodeDateTime(2016, 1, 1, 0, 0, 0, 0));
  PeriodEnd := DateTimeToUnix(EncodeDateTime(2020, 1, 1, 0, 0, 0, 0));
  //check if the number of queries is passed as a parameter; if not, will use the default value;
  if (ParamCount >= 1) and TryStrToInt(ParamStr(1), qc) and (qc > 0) then
    QueryCount := qc;
end;

var
  StartTime: TDateTime;

begin
  ReadInput;
  WriteLn('Starting create database');
  CreateList;
  WriteLn('Database contains ', {%H-}PayList.Count, ' entries');

  WriteLn('Need to answer ', QueryCount, ' queries');
  WriteLn;

  StartTime := Time;
  Run;

  WriteLn;
  WriteLn('Time elapsed: ', MillisecondsBetween(Time, StartTime), ' ms.');
  WriteLn('Database contains ', {%H-}PayList.Count, ' entries');
  PayList.Clear;

  ReadLn;
end.

