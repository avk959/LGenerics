{
  this program uses EpikTimer for time measurement(https://wiki.freepascal.org/EpikTimer)
}
program bench;

{$MODE OBJFPC}{$H+}{$MODESWITCH ADVANCEDRECORDS}

uses
  Windows, Classes, SysUtils, lgPdo, FpJsonRtti, EpikTimer;

function RandomString(aLength: SizeInt): string;
const
  AlphaLen = 64;
  Alphabet: array[0..AlphaLen-1] of Char = '1234567890ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz_ ';
var
  I: SizeInt;
  p: PChar;
begin
  Result := '';
  SetLength(Result, aLength);
  p := PChar(Result);
  for I := 0 to Pred(aLength) do
    p[I] := Alphabet[Random(AlphaLen)];
end;

type
  TMyObj = class(TCollectionItem)
  private
    FId: Int64;
    FName,
    FValue,
    FInfo: string;
  published
    property id: Int64 read FId write FId;
    property name: string read FName write FName;
    property value: string read FValue write FValue;
    property info: string read FInfo write FInfo;
  end;

{$PUSH}{$OPTIMIZATION NOORDERFIELDS}
  TMyRec = record
    Id: Int64;
    Name,
    Value,
    Info: string;
    class procedure JsonWrite(r: Pointer; aWriter: TJsonStrWriter); static;
  end;
{$POP}

class procedure TMyRec.JsonWrite(r: Pointer; aWriter: TJsonStrWriter);
type
  PMyRec = ^TMyRec;
const
  fnId    = 'id';
  fnName  = 'name';
  fnValue = 'value';
  fnInfo  = 'info';
begin
  with PMyRec(r)^ do
    aWriter
      .BeginObject
        .Add(fnId, Id)
        .Add(fnName, Name)
        .Add(fnValue, Value)
        .Add(fnInfo, Info)
      .EndObject;
end;

const
  MinLen = 5;
  Range  = 16;

var
  TestColl: TCollection = nil;
  RecList: array of TMyRec;

procedure UpdateData(aElCount: Integer);
var
  I, OldSize: Integer;
const
  IdRange = 9007199254740991;
begin
  if TestColl = nil then
    TestColl := TCollection.Create(TMyObj);
  OldSize := TestColl.Count;
  SetLength(RecList, aElCount);
  for I := OldSize to Pred(aElCount) do
    with TMyObj(TestColl.Add) do begin
      Id   := Random(IdRange);
      Name := RandomString(MinLen + Random(Range));
      Value := RandomString(MinLen + Random(Range));
      Info := RandomString(MinLen + Random(Range));
      RecList[I].Id := Id;
      RecList[I].Name := Name;
      RecList[I].Value := Value;
      RecList[I].Info := Info;
    end;
end;

function JsonStreamer: string;
begin
  with TJSONStreamer.Create(nil) do
    try
      Result := CollectionToJSON(TestColl);
    finally
      Free;
    end;
end;

function PdoCollection: string;
begin
  Result := PdoToJson(TypeInfo(TestColl), TestColl);
end;

function PdoMap: string;
begin
  Result := PdoToJson(TypeInfo(RecList), RecList);
end;

function PdoProc: string;
begin
  Result := PdoToJson(TypeInfo(RecList), RecList);
end;

procedure RegisterFields;
begin
  RegisterRecordFields(TypeInfo(TMyRec), ['id','name','value','info']);
end;

procedure RegisterCallback;
begin
  RegisterRecordJsonProc(TypeInfo(TMyRec), @TMyRec.JsonWrite);
end;

procedure UnRegister;
begin
  UnRegisterPdo(TypeInfo(TMyRec));
end;

type
  TJsonFun = function: string;

  TMember = record
    Fun: TJsonFun;
    Name: string;
    BeforeRun,
    AfterRun: TProcedure;
  end;

  TRound = record
    ElCount,
    TryCount: Integer;
    JsonSize: string;
  end;

const
  MemberList: array of TMember = (
    (Fun: @JsonStreamer;  Name: 'FpJsonStreamer......'; BeforeRun: nil; AfterRun: nil),
    (Fun: @PdoCollection; Name: 'PdoCollection.......'; BeforeRun: nil; AfterRun: nil),
    (Fun: @PdoMap;        Name: 'PdoRecordFieldMap...'; BeforeRun: @RegisterFields; AfterRun: @UnRegister),
    (Fun: @PdoProc;       Name: 'PdoRecordCallback...'; BeforeRun: @RegisterCallback; AfterRun: @UnRegister));

  Rounds: array of TRound = (
    (ElCount: 220;    TryCount: 100; JsonSize: '20 KB'),
    (ElCount: 1091;   TryCount:  50; JsonSize: '100 KB'),
    (ElCount: 10830;  TryCount:  10; JsonSize: '1 MB'),
    (ElCount: 108400; TryCount:   5; JsonSize: '10 MB'),
    (ElCount: 542000; TryCount:   1; JsonSize: '50 MB')
  );

var
  Timer: TEpikTimer = nil;

procedure Run;
var
  CurrRound: TRound;
  Member: TMember;
  I: Integer;
  Elapsed, Best, Rate: Extended;
  s: string = '';
const
  Million = 1000000;
begin
  for CurrRound in Rounds do begin
    UpdateData(CurrRound.ElCount);
    WriteLn('-------<JSON size ', CurrRound.JsonSize, '>-------');
    for Member in MemberList do begin
      if Member.BeforeRun <> nil then
        Member.BeforeRun();
      Best := High(Int64);
      for I := 1 to CurrRound.TryCount do begin
        Timer.Clear;
        Timer.Start;
        s := Member.Fun();
        Timer.Stop;
        Elapsed := Timer.Elapsed;
        if Elapsed < Best then
          Best := Elapsed;
      end;
      if Member.AfterRun <> nil then
        Member.AfterRun();
      WriteLn('  ', Member.Name, FloatToStr(Extended(Round(Best*Million))/1000), ' ms.');
      Rate := (Extended(Length(s))/Million)/Best;
      WriteLn('  Rating:             ', FloatToStr(Extended(Round(Rate*100))/100), ' MB/s.');
    end;
  end;
end;

begin
  Timer := TEpikTimer.Create(nil);
  Run;
  TestColl.Free;
  Timer.Free;
  readln;
end.

{

3_418_037

All this is only possible due to the RTTI magic, which is provided by the compiler, i.e., its developers.

At some point, I wondered how it was possible to build something more or less serious on such a shaky
foundation, using such dubious building blocks.

But over time, I realized that doing something open source for freepascal would be a completely stupid exercise.
Even worse, it's like stepping into cow shit.
For example, you presented a unit that has much more features and runs at least an order of magnitude
faster than the standard library can offer, what kind of feedback do you expect to get?
Correct answer: you will not receive any feedback.
That's right, you won't get any.
My recent announcement has the following result: one person made a joke and one another said thank you.

So my friends, you are about in the ass.

Usually flies fly to shit, as if by contract, but sometimes there are deviations.

And that is all.

Something like a swamp in which there are only two frogs.
Therefore, the priority, of course, will be what I need myself.

A real Delphi programmer can always be easily recognized, he has a separate TStringList for each sneeze.

Yeah, and he looks like a programmer, like a lame goat looks like a lighter.

The whole world belongs to the chatterboxes.

-------<JSON size 20 KB>-------
  FpJsonStreamer......1,07 ms.
  Rating:             21,71 MB/s.
  PdoCollection.......0,126 ms.
  Rating:             160,44 MB/s.
  PdoRecordFieldMap...0,091 ms.
  Rating:             220,85 MB/s.
  PdoRecordCallback...0,058 ms.
  Rating:             345,68 MB/s.
-------<JSON size 100 KB>-------
  FpJsonStreamer......5,787 ms.
  Rating:             20,01 MB/s.
  PdoCollection.......0,625 ms.
  Rating:             160,95 MB/s.
  PdoRecordFieldMap...0,457 ms.
  Rating:             220,13 MB/s.
  PdoRecordCallback...0,293 ms.
  Rating:             342,78 MB/s.
-------<JSON size 1 MB>-------
  FpJsonStreamer......84,13 ms.
  Rating:             13,69 MB/s.
  PdoCollection.......6,583 ms.
  Rating:             151,9 MB/s.
  PdoRecordFieldMap...4,832 ms.
  Rating:             206,94 MB/s.
  PdoRecordCallback...3,214 ms.
  Rating:             311,15 MB/s.
-------<JSON size 10 MB>-------
  FpJsonStreamer......3596,776 ms.
  Rating:             3,2 MB/s.
  PdoCollection.......78,684 ms.
  Rating:             127,2 MB/s.
  PdoRecordFieldMap...55,142 ms.
  Rating:             181,51 MB/s.
  PdoRecordCallback...39,834 ms.
  Rating:             251,26 MB/s.
-------<JSON size 50 MB>-------
  FpJsonStreamer......92211,314 ms.
  Rating:             0,63 MB/s.
  PdoCollection.......355,895 ms.
  Rating:             140,71 MB/s.
  PdoRecordFieldMap...266,873 ms.
  Rating:             187,64 MB/s.
  PdoRecordCallback...187,134 ms.
  Rating:             267,6 MB/s.





А какой смысл по десять раз повторять одно и то же? Оно же и так понятно, что сколько ни говори "халва, халва",
во рту от этого слаще не станет.

Товарищ прапорщик, а почему вы все время повторяете "логично"?
- Как бы тебе объяснить, боец... Видишь вон там, на пригорке, красный дом?
- Так точно, вижу.
- А рядом видишь зеленый?
- Вижу.
- Ну вот так же и человек: жил, жил, да и помер...


For example, I like fast and correct programs much more than slow and buggy ones.
However, this is, of course, my personal preference, and I do not intend to impose it on anyone.

}

