{
  version of transit-lang-cmp language comparison using fphttpapp, LGenerics primitives
  and manual JSON serialization(TJsonStrWriter), which appeared during a discussion
  on the Lazarus forum: https://forum.lazarus.freepascal.org/index.php/topic,61035
}
program trascal;

{$mode objfpc}{$h+}

uses
{$ifdef UNIX}
  cthreads,
{$endif}
  Classes, SysUtils, DateUtils, Math, FpHttpApp, HttpDefs, HttpRoute,
  lgUtils, lgHashMap, lgVector, lgJson, lgCsvUtils;

type
  TStopTimeCols   = (stcTripId, stcArrival, stcDeparture, stcStopId);
  TStopTimeFields = stcArrival..stcStopId;
  TTripCols       = (tcRouteId, tcService, tcTripId);

{$push}{$J-}
const
  StopNames:    array[TStopTimeCols] of string = ('trip_id', 'arrival_time', 'departure_time', 'stop_id');
  TripNames:    array[TTripCols] of string = ('route_id', 'service_id', 'trip_id');
  StopTimeMask: array[TStopTimeCols] of Boolean = (True, True, True, True);
  TripMask:     array[TTripCols] of Boolean = (True, True, True);
{$pop}
  SchedName = 'schedules';

type
  PRow        = TCsvDoc.PRow;
  TRowList    = specialize TGLiteVector<PRow>;
  TRowHashMap = specialize TGLiteChainHashMap<string, TRowList, string>;
  TRowMap     = TRowHashMap.TMap;
  PRowList    = ^TRowList;

function BuildTripResponse(const ARoute: string; const aStopTimeRowMap, aTripRowMap: TRowMap): string;
var
  Trips, StopTimes: PRowList;
  TripIdx, StopIdx: Integer;
  WriteRef: specialize TGUniqRef<TJsonStrWriter>;
  Writer: TJsonStrWriter;
  p: PRow;
  TripCol: TTripCols;
  StopField: TStopTimeFields;
begin
  {%H-}WriteRef.Instance := TJsonStrWriter.Create;
  Writer := WriteRef;
  Writer.BeginArray;
  if aTripRowMap.TryGetMutValue(ARoute, Trips) then
    for TripIdx := 0 to Pred(Trips^.Count) do begin
      p := Trips^.UncMutable[TripIdx]^;
      Writer.BeginObject;
      for TripCol in TTripCols do
        Writer.Add(TripNames[TripCol], p^[Ord(TripCol)]);
      Writer.AddName(SchedName).BeginArray;
      if aStopTimeRowMap.TryGetMutValue(p^[Ord(tcTripId)], StopTimes) then
        for StopIdx := 0 to Pred(StopTimes^.Count) do begin
          p := StopTimes^.UncMutable[StopIdx]^;
          Writer.BeginObject;
          for StopField in TStopTimeFields do
            Writer.Add(StopNames[StopField], p^[Ord(StopField)]);
          Writer.EndObject;
       end;
      Writer.EndArray;
      Writer.EndObject;
    end;
  Writer.EndArray;
  Result := Writer.JsonString;
end;

procedure GetStopTimes(var aStopTimeRowMap: TRowMap; aDoc: TCsvDoc);
var
  Start, Stop: TDateTime;
  I: Integer;
  pStopList: PRowList;
  p: PRow;
begin
  Start := Now;
  aDoc.LoadFromFileMT('MBTA_GTFS/stop_times.txt', StopTimeMask, Math.Min(TThread.ProcessorCount, 8)){%H-};
  //aDoc.LoadFromFile('MBTA_GTFS/stop_times.txt', StopTimeMask);
  p := aDoc.Rows[0];
  if(p^[Ord(stcTripId)] <> StopNames[stcTripId])or(p^[Ord(stcArrival)] <> StopNames[stcArrival])or
    (p^[Ord(stcDeparture)] <> StopNames[stcDeparture])or(p^[Ord(stcStopId)] <> StopNames[stcStopId]) then begin
    WriteLn('stop_times.txt not in expected format:');
    for I := 0 to aDoc.ColCount[0] - 1 do
      WriteLn(I, ' ' + p^[I]);
    Halt(1);
  end;
  for I := 1 to Pred(aDoc.RowCount) do begin
    p := aDoc.Rows[I];
    aStopTimeRowMap.FindOrAddMutValue(p^[Ord(stcTripId)], pStopList);
    pStopList^.Add(p);
  end;
  Stop := Now;
  WriteLn('parsed ', aDoc.RowCount, ' stop times in ', SecondSpan(Start, Stop):1:3,' seconds');
end;

procedure GetTrips(var aTripRowMap: TRowMap; aDoc: TCsvDoc);
var
  Start, Stop: TDateTime;
  I: Integer;
  pTripList: PRowList;
  p: PRow;
begin
  Start := Now;
  aDoc.LoadFromFile('MBTA_GTFS/trips.txt', TripMask);
  p := aDoc.Rows[0];
  if(p^[Ord(tcRouteId)]<>TripNames[tcRouteId])or(p^[Ord(tcService)]<>TripNames[tcService])or(p^[Ord(tcTripId)]<>TripNames[tcTripId])then begin
    WriteLn('trips.txt not in expected format:');
    for I := 0 to aDoc.ColCount[0] - 1 do
      WriteLn(I, ' ' + p^[I]);
    Halt(1);
  end;
  for I := 1 to Pred(aDoc.RowCount) do begin
    p := aDoc.Rows[I];
    aTripRowMap.FindOrAddMutValue(p^[Ord(tcRouteId)], pTripList);
    pTripList^.Add(p);
  end;
  Stop := Now;
  WriteLn('parsed ', aDoc.RowCount, ' trips in ', SecondSpan(Start, Stop):1:3,' seconds');
end;

var
  StopTimeRowMap, TripRowMap: TRowMap;
  StopTimes, Trips: specialize TGAutoRef<TCsvDoc>;

procedure SchedulesHandler(aRequest: TRequest; aResponse: TResponse);
begin
  aResponse.ContentType := 'application/json';
  aResponse.Content := BuildTripResponse(aRequest.RouteParams['route'], StopTimeRowMap, TripRowMap);
end;

procedure StopHandler(aRequest: TRequest; aResponse: TResponse);
begin
  Application.Terminate;
end;

begin
  GetStopTimes(StopTimeRowMap, StopTimes.Instance);
  GetTrips(TripRowMap, Trips.Instance);

  Application.Port := 4000;
  Application.Threaded := True;
  HTTPRouter.RegisterRoute('/schedules/:route',@SchedulesHandler);
  HTTPRouter.RegisterRoute('/stop', @StopHandler);
  Application.Run;
end.

