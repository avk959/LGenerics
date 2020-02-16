{$define RunBodyMacro :=
var
  Map: MapTypeMacro;
  a: TEntryArray;
  Clocks: TClocks;
  TotalStart: TTime;
  Start: QWord;
  eTime: Int64;
  I, J: Integer;
  Fail: Boolean;
const
  MapName = MapNameMacro;
begin
  RandSeed := aSeed;
  SetLength(a, aSize);
  for I := 0 to High(a) do
    a[I] := TMapEntry.Create(RandomString(aKeySize), Succ(I));
  MapCreateMacro;

  //write
  TotalStart := Time;
  while MilliSecondsBetween(Time, TotalStart) < aInterval do
    begin
      MapCleanupMacro;
      Start := RDTSC;
      for I := 0 to High(a) do
        with a[I] do
          AddOrSetValueMacro;
      Clocks.Add(RDTSC - Start);
    end;
  if aBenchBy = bbBest then
    eTime := Round(TClockHelper.NthSmallest(Clocks, 0).Value * CpuClockPeriod)
  else
    eTime := Round(TClockHelper.NthSmallest(Clocks, Clocks.Count div 2).Value * CpuClockPeriod);
  WriteLn(StdErr, aKeySize, ' only_write ', MapName, eTime);
  WriteLn(aKeySize, ' only_write ', MapName, eTime);
  Clocks.Clear;
  TEntryHelper.RandomShuffle(a);

  //reserved write
  TotalStart := Time;
  while MilliSecondsBetween(Time, TotalStart) < aInterval do
    begin
      MapEnsureCapacityMacro;
      Start := RDTSC;
      for I := 0 to High(a) do
        with a[I] do
          AddOrSetValue2Macro;
      Clocks.Add(RDTSC - Start);
    end;
  if aBenchBy = bbBest then
    eTime := Round(TClockHelper.NthSmallest(Clocks, 0).Value * CpuClockPeriod)
  else
    eTime := Round(TClockHelper.NthSmallest(Clocks, Clocks.Count div 2).Value * CpuClockPeriod);
  WriteLn(StdErr, aKeySize, ' reserved_write ', MapName, eTime);
  WriteLn(aKeySize, ' reserved_write ', MapName, eTime);
  Clocks.Clear;
  TEntryHelper.RandomShuffle(a);

  //successful lookup
  Fail := False;
  TotalStart := Time;
  while MilliSecondsBetween(Time, TotalStart) < aInterval do
    begin
      J := 0;
      Start := RDTSC;
      for I := 0 to High(a) do
        J += Ord(MapContainsMacro);
      Clocks.Add(RDTSC - Start);
      if J <> aSize then
        begin
          Fail := True;
          break;
        end;
    end;
  if Fail then
    begin
      WriteLn(StdErr, aKeySize, ' success_lookup ', MapName, FAIL_TIME);
      WriteLn(aKeySize, ' success_lookup ', MapName, FAIL_TIME);
    end
  else
    begin
      if aBenchBy = bbBest then
        eTime := Round(TClockHelper.NthSmallest(Clocks, 0).Value * CpuClockPeriod)
      else
        eTime := Round(TClockHelper.NthSmallest(Clocks, Clocks.Count div 2).Value * CpuClockPeriod);
      WriteLn(StdErr, aKeySize, ' success_lookup ', MapName, eTime);
      WriteLn(aKeySize, ' success_lookup ', MapName, eTime);
    end;
  Clocks.Clear;
  TEntryHelper.RandomShuffle(a);

  //failed lookup
  for I := 0 to High(a) do
    a[I].Key[aKeySize] := Colon;

  Fail := False;
  TotalStart := Time;
  while MilliSecondsBetween(Time, TotalStart) < aInterval do
    begin
      Start := RDTSC;
      for I := 0 to High(a) do
        if MapContainsMacro then
          begin
            Fail := True;
            break;
          end;
      Clocks.Add(RDTSC - Start);
    end;
  if Fail then
    begin
      WriteLn(StdErr, aKeySize, ' fail_lookup ', MapName, FAIL_TIME);
      WriteLn(aKeySize, ' fail_lookup ', MapName, FAIL_TIME);
    end
  else
    begin
      if aBenchBy = bbBest then
        eTime := Round(TClockHelper.NthSmallest(Clocks, 0).Value * CpuClockPeriod)
      else
        eTime := Round(TClockHelper.NthSmallest(Clocks, Clocks.Count div 2).Value * CpuClockPeriod);
      WriteLn(StdErr, aKeySize, ' fail_lookup ', MapName, eTime);
      WriteLn(aKeySize, ' fail_lookup ', MapName, eTime);
    end;
  Clocks.Clear;

  //remove
  MapClearMacro;
  Fail := False;
  TotalStart := Time;
  while MilliSecondsBetween(Time, TotalStart) < aInterval do
    begin
      for I := 0 to High(a) do
        with a[I] do
          AddOrSetValue2Macro;
      TEntryHelper.RandomShuffle(a);
      Start := RDTSC;
      for I := 0 to High(a) do
        MapRemoveMacro;
      Clocks.Add(RDTSC - Start);
      if MapCountMacro <> 0 then
        begin
          Fail := True;
          break;
        end;
    end;
  if Fail then
    begin
      WriteLn(StdErr, aKeySize, ' only_remove ', MapName, FAIL_TIME);
      WriteLn(aKeySize, ' only_remove ', MapName, FAIL_TIME);
    end
  else
    begin
      if aBenchBy = bbBest then
        eTime := Round(TClockHelper.NthSmallest(Clocks, 0).Value * CpuClockPeriod)
      else
        eTime := Round(TClockHelper.NthSmallest(Clocks, Clocks.Count div 2).Value * CpuClockPeriod);
      WriteLn(StdErr, aKeySize, ' only_remove ', MapName, eTime);
      WriteLn(aKeySize, ' only_remove ', MapName, eTime);
    end;
  MapFreeMacro;
}
