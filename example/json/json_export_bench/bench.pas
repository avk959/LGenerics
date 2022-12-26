{
  this program uses EpikTimer for time measurement(https://wiki.freepascal.org/EpikTimer)
}
program bench;

{$MODE OBJFPC}{$H+}{$MODESWITCH ADVANCEDRECORDS}

uses
  Classes, SysUtils, lgPdo, FpJsonRtti, EpikTimer;

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
end.

