{
  version of transit-lang-cmp language comparison using fphttpapp
  and LGenerics primitives, which appeared during a discussion
  on the Lazarus forum: https://forum.lazarus.freepascal.org/index.php/topic,61035
}
program trascal;

{$mode objfpc}{$h+}{$modeswitch advancedrecords}

uses
{$ifdef UNIX}
  cthreads,
{$endif}
  Classes, SysUtils, DateUtils, Math, FpHttpApp, HttpDefs, HttpRoute,
  lgUtils, lgHashMap, lgVector, lgCsvUtils, lgPdo;

type
  TTrip = record
    RouteID, ServiceID, TripID: string;
    constructor Make(const aRouteID, aServiceID, aTripID: string);
  end;
  TTrips = array of TTrip;

  {$push}{$optimization noorderfields}
  TScheduleItem = record
    Arrival, Departure, StopID: string;
    constructor Make(const aArrival, aDeparture, aStopID: string);
  end;
  TSchedules = array of TScheduleItem;

  TResponseItem = record
    RouteID, ServiceID, TripID: string;
    Schedules: TSchedules;
    constructor Make(const aTrip: TTrip);
  end;
  {$pop}
  TTripResponse = array of TResponseItem;

  PRow         = TCsvDoc.PRow;
  TIntList     = specialize TGLiteVector<Integer>;
  TRowHashMap  = specialize TGLiteChainHashMap<string, TIntList, string>;
  TStrIndexMap = TRowHashMap.TMap;
  PIntList     = ^TIntList;

constructor TTrip.Make(const aRouteID, aServiceID, aTripID: string);
begin
  RouteID := aRouteID;
  ServiceID := aServiceID;
  TripID := aTripID;
end;

constructor TScheduleItem.Make(const aArrival, aDeparture, aStopID: string);
begin
  Arrival := aArrival;
  Departure := aDeparture;
  StopID := aStopID;
end;

constructor TResponseItem.Make(const aTrip: TTrip);
begin
  RouteID := aTrip.RouteID;
  ServiceID := aTrip.ServiceID;
  TripID := aTrip.TripID;
  Schedules := nil;
end;

function BuildTripResponse(const aRoute: string; const aStopTimeMap, aTripMap: TStrIndexMap;
  const aTrips: TTrips; const aSchedules: TSchedules): string;
var
  Trips, StopTimes: PIntList;
  TripIdx, StopIdx: Integer;
  Response: TTripResponse = nil;
begin
  Result := '';
  if aTripMap.TryGetMutValue(aRoute, Trips) then begin
    SetLength(Response, Trips^.Count);
    for TripIdx := 0 to Pred(Trips^.Count) do begin
      Response[TripIdx] := TResponseItem.Make(aTrips[Trips^.UncMutable[TripIdx]^]);
      if aStopTimeMap.TryGetMutValue(Response[TripIdx].TripID, StopTimes) then
        with Response[TripIdx] do begin
          SetLength(Schedules, StopTimes^.Count);
          for StopIdx := 0 to Pred(StopTimes^.Count) do
            Schedules[StopIdx] := aSchedules[StopTimes^.UncMutable[StopIdx]^];
        end;
    end;
    Result := PdoToJson(TypeInfo(Response), Response);
  end;
end;

procedure GetStopTimes(var aStopTimeMap: TStrIndexMap; var aShedules: TSchedules);
var
  DocRef: specialize TGAutoRef<TCsvDoc>;
  Doc: TCsvDoc;
  Start, Stop: TDateTime;
  I: Integer;
  pStopList: PIntList;
  p: PRow;
begin
  Start := Now;
  Doc := DocRef.Instance.LoadFromFileMT(
    'MBTA_GTFS/stop_times.txt', [true,true,true,true], Math.Min(TThread.ProcessorCount, 8)){%H-};
  //Doc := DocRef.Instance.LoadFromFile('MBTA_GTFS/stop_times.txt', [true,true,true,true]);
  p := Doc.Rows[0];
  if(p^[0] <> 'trip_id')or(p^[1] <> 'arrival_time')or(p^[2] <> 'departure_time')or(p^[3] <> 'stop_id')then begin
    WriteLn('stop_times.txt not in expected format:');
    for I := 0 to Doc.ColCount[0] - 1 do
      WriteLn(I, ' ' + p^[I]);
    Halt(1);
  end;
  SetLength(aShedules, Pred(Doc.RowCount));
  for I := 1 to Pred(DocRef.Instance.RowCount) do begin
    p := Doc.Rows[I];
    aStopTimeMap.FindOrAddMutValue(p^[0], pStopList);
    pStopList^.Add(Pred(I));
    aShedules[Pred(I)] := TScheduleItem.Make(p^[1], p^[2], p^[3]);
  end;
  Stop := Now;
  WriteLn('parsed ', Length(aShedules), ' stop times in ', SecondSpan(Start, Stop):1:3,' seconds');
end;

procedure GetTrips(var aTripMap: TStrIndexMap; var aTrips: TTrips);
var
  DocRef: specialize TGAutoRef<TCsvDoc>;
  Doc: TCsvDoc;
  Start, Stop: TDateTime;
  I: Integer;
  pTripList: PIntList;
  p: PRow;
begin
  Start := Now;
  Doc := DocRef.Instance.LoadFromFile('MBTA_GTFS/trips.txt', [true, true, true]);
  p := Doc.Rows[0];
  if(p^[0] <> 'route_id') or (p^[1] <> 'service_id') or (p^[2] <> 'trip_id')then begin
    WriteLn('trips.txt not in expected format:');
    for I := 0 to Doc.ColCount[0] - 1 do
      WriteLn(I, ' ' + p^[I]);
    Halt(1);
  end;
  SetLength(aTrips, Pred(Doc.RowCount));
  for I := 1 to Pred(Doc.RowCount) do begin
    p := Doc.Rows[I];
    aTripMap.FindOrAddMutValue(p^[0], pTripList);
    pTripList^.Add(Pred(I));
    aTrips[Pred(I)] := TTrip.Make(p^[0], p^[1], p^[2]);
  end;
  Stop := Now;
  WriteLn('parsed ', Length(aTrips), ' trips in ', SecondSpan(Start, Stop):1:3,' seconds');
end;

var
  StopTimeIdxMap, TripIdxMap: TStrIndexMap;
  Schedules: TSchedules;
  Trips: TTrips;

procedure SchedulesHandler(aRequest: TRequest; aResponse: TResponse);
begin
  aResponse.ContentType := 'application/json';
  aResponse.Content := BuildTripResponse(
    aRequest.RouteParams['route'], StopTimeIdxMap, TripIdxMap, Trips, Schedules);
end;

procedure StopHandler(aRequest: TRequest; aResponse: TResponse);
begin
  Application.Terminate;
end;

begin
  RegisterPdo(TypeInfo(TScheduleItem), ['arrival_time', 'departure_time', 'stop_id']);
  RegisterPdo(TypeInfo(TResponseItem), ['route_id', 'service_id', 'trip_id', 'schedules']);

  GetStopTimes(StopTimeIdxMap, Schedules);
  GetTrips(TripIdxMap, Trips);

  Application.Port := 4000;
  Application.Threaded := True;
  HTTPRouter.RegisterRoute('/schedules/:route',@SchedulesHandler);
  HTTPRouter.RegisterRoute('/stop', @StopHandler);
  Application.Run;
end.

