unit EpikTimer;

{ Name: EpikTimer
  Description: Precision timer/stopwatch component for Lazarus/FPC
  Author: Tom Lisjac <netdxr@gmail.com>
  Started on: June 24, 2003
  Features:
    Dual selectable timebases: Default:System (uSec timeofday or "now" in Win32)
                               Optional: Pentium Time Stamp Counter.
    Default timebase should work on most Unix systems of any architecture.
    Timebase correlation locks time stamp counter accuracy to system clock.
    Timers can be started, stopped, paused and resumed.
    Unlimited number of timers can be implemented with one component.
    Low resources required: 25 bytes per timer; No CPU overhead.
    Internal call overhead compensation.
    System sleep function
    Designed to support multiple operating systems and Architectures
    Designed to support other hardware tick sources
            
  Credits: Thanks to Martin Waldenburg for a lot of great ideas for using
           the Pentium's RDTSC instruction in wmFastTime and QwmFastTime.
}

{ Copyright (C) 2003-2006 by Tom Lisjac <netdxr@gmail.com>,
   Felipe Monteiro de Carvalho and Marcel Minderhoud

  This library is licensed on the same Modifyed LGPL as Free Pascal RTL and LCL are

  Please contact the author if you'd like to use this component but the Modifyed LGPL
  doesn't work with your project licensing.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE.
  
  Contributor(s):
  
  * Felipe Monteiro de Carvalho (felipemonteiro.carvalho@gmail.com)
  
  * Marcel Minderhoud
  
}
{
 Known Issues

   - Tested on Linux but no other Lazarus/FPC supported Unix platforms
   - If system doesn't have microsecond system clock resolution, the component
     falls back to a single gated measurement of the hardware tick frequency via
     nanosleep. This usually results in poor absolute accuracy due large amounts
     of jitter in nanosleep... but for typical short term measurements, this
     shouldn't be a problem.

}

{$IFDEF FPC}
  {$MODE DELPHI}{$H+}
{$ENDIF}

{$IFNDEF FPC}
  {$DEFINE Windows}
{$ENDIF}

{$IFDEF Win32}
  {$DEFINE Windows}
{$ENDIF}

interface

uses
{$IFDEF Windows}
  Windows, {%H-}MMSystem,
{$ELSE}
  unix, unixutil, BaseUnix,
{$ENDIF}
  Classes, SysUtils, dateutils;

Const
  DefaultSystemTicksPerSecond = 1000000; //Divisor for microsecond resolution
  { HW Tick frequency falls back to gated measurement if the initial system
    clock measurement is outside this range plus or minus.}
  SystemTicksNormalRangeLimit = 100000;

type

  TickType = Int64; // Global declaration for all tick processing routines
  
  FormatPrecision = 1..12; // Number of decimal places in elapsed text format
  
  // Component powers up in System mode to provide some cross-platform safety.
  TickSources = (SystemTimebase, HardwareTimebase); // add others if desired

  (* * * * * * * * * * * Timebase declarations  * * * * * * * * * * *)
  
  { There are two timebases currently implemented in this component but others
    can be added by declaring them as "TickSources", adding a TimebaseData
    variable to the Private area of TEpikTimer and providing a "Ticks" routine
    that returns the current counter value.
    
    Timebases are "calibrated" during initialization by taking samples of the
    execution times of the SystemSleep and Ticks functions measured with in the
    tick period of the selected timebase. At runtime, these values are retrieved
    and used to remove the call overhead to the best degree possible.
    
    System latency is always present and contributes "jitter" to the edges of
    the sample measurements. This is especially true if a microsecond system
    clock isn't detected on the host system and a fallback gated measurement
    (based on nanosleep in Linux and sleep in Win32) is used to determine the
    timebase frequency. This is sufficient for short term measurements where
    high resolution comparisons are desired... but over a long measurement
    period, the hardware and system wall clock will diverge significantly.
    
    If a microsecond system clock is found, timebase correlation is used to
    synchronize the hardware counter and system clock. This is described below.
  }

  TickCallFunc = function: Ticktype; // Ticks interface function
  
  // Contains timebase overhead compensation factors in ticks for each timebase
  TimebaseCalibrationParameters = record
    FreqCalibrated: Boolean; // Indicates that the tickfrequency has been calibrated
    OverheadCalibrated: Boolean; // Indicates that all call overheads have been calibrated
    TicksIterations: Integer; // number of iterations to use when measuring ticks overhead
    SleepIterations: Integer; // number of iterations to use when measuring SystemSleep overhead
    FreqIterations: Integer;  // number of iterations to use when measuring ticks frequency
    FrequencyGateTimeMS: Integer;  // gate time to use when measuring ticks frequency
   end;

  // This record defines the Timebase context
  TimebaseData = record
    CalibrationParms: TimebaseCalibrationParameters; // Calibration data for this timebase
    TicksFrequency: TickType; // Tick frequency of this timebase
    TicksOverhead: Ticktype;  // Ticks call overhead in TicksFrequency for this timebase
    SleepOverhead: Ticktype;   // SystemSleep all overhead in TicksFrequency for this timebase
    Ticks: TickCallFunc; // all methods get their ticks from this function when selected
  end;
  
  TimeBaseSelector = ^TimebaseData;
  
  (*  * * * * * * * * * * Timebase Correlation  * * * * * * * * * * *)

  { The TimeBaseCorrelation record stores snapshot samples of both the system
    ticks (the source of known accuracy) and the hardware tick source (the
    source of high measurement resolution). An initial sample is taken at power
    up. The CorrelationMode property sets where and when updates are acquired.

    When an update snapshot is acquired, the differences between it and the
    startup value can be used to calculate the hardware clock frequency with
    high precision from the accuracy of the accumulated system clocks. The
    longer time that elapses between startup and a call to "CorrelateTimebases",
    the better the accuracy will be. On a 1.6 Ghz P4, it only takes a few
    seconds to achieve measurement certainty down to a few Hertz.

    Of course this system is only as good as your system clock accuracy, so
    it's a good idea to periodically sync it with NTP or against another source
    of known accuracy if you want to maximize the long term of the timers. }

  TimebaseCorrelationData = record
    SystemTicks: TickType;
    HWTicks: TickType;
  end;

  // If the Correlation property is set to automatic, an update sample is taken
  // anytime the user calls Start or Elapsed. If in manual, the correlation
  // update is only done when "CorrelateTimebases" is called. Doing updates
  // with every call adds a small amount of overhead... and after the first few
  // minutes of operation, there won't be very much correcting to do!

  CorrelationModes=(Manual, OnTimebaseSelect, OnGetElapsed);
  
  (* * * * * * * * * * * Timer Data record structure  * * * * * * * * * * *)
  
  // This is the timer data context. There is an internal declaration of this
  // record and overloaded methods if you only want to use the component for a
  // single timer... or you can declare multiple TimerData records in your
  // program and create as many instances as you want with only a single
  // component on the form. See the "Stopwatch" methods in the TEpikTimer class.
  
  // Each timers points to the timebase that started it... so you can mix system
  // and hardware timers in the same application.

  TimerData = record
    Running:Boolean; // Timer is currently running
    TimebaseUsed:TimeBaseSelector; // keeps timer aligned with the source that started it.
    StartTime:TickType; // Ticks sample when timer was started
    TotalTicks:TickType; // Total ticks... for snapshotting and pausing
  end;
  
  TEpikTimer= class(TComponent)
    private
      BuiltInTimer:TimerData; // Used to provide a single built-in timer;
      FHWTickSupportAvailable:Boolean; // True if hardware tick support is available
      FHWCapabilityDataAvailable:Boolean; // True if hardware tick support is available
      FHWTicks:TimeBaseData;     // The hardware timebase
      FSystemTicks:TimeBaseData; // The system timebase
      FSelectedTimebase:TimeBaseSelector; // Pointer to selected database
      
      FTimeBaseSource: TickSources; // use hardware or system timebase
      FWantDays: Boolean; // true if days are to be displayed in string returns
      FWantMS: Boolean; // True to display milliseconds in string formatted calls
      FSPrecision: FormatPrecision; // number of digits to display in string calls
      FMicrosecondSystemClockAvailable:Boolean; // true if system has microsecond clock
      
      StartupCorrelationSample:TimebaseCorrelationData; // Starting ticks correlation snapshot
      UpdatedCorrelationSample:TimebaseCorrelationData; // Snapshot of last correlation sample
      FCorrelationMode: CorrelationModes; // mode to control when correlation updates are performed
    protected
      function GetSelectedTimebase: TimebaseData;
      procedure SetSelectedTimebase(const AValue: TimebaseData);
      procedure SetTimebaseSource(const AValue: TickSources); //setter for TB
      Procedure GetCorrelationSample(Var CorrelationData:TimeBaseCorrelationData);
    public
      {                       Stopwatch emulation routines
        These routines behave exactly like a conventional stopwatch with start,
        stop, elapsed (lap) and clear methods. The timers can be started,
        stopped and resumed. The Elapsed routines provide a "lap" time analog.

        The methods are overloaded to make it easy to simply use the component's
        BuiltInTimer as a single timer... or to declare your own TimerData records
        in order to implement unlimited numbers of timers using a single component
        on the form. The timers are very resource efficient because they consume
        no CPU overhead and only require about 25 bytes of memory.
      }

      // Stops and resets the timer
      procedure Clear; overload;// Call this routine to use the built-in timer record
      procedure Clear(Var T:TimerData); overload; // pass your TimerData record to this one

      //Start or resume a stopped timer
      procedure Start; overload;
      procedure Start(Var T:TimerData); overload;

      //Stop or pause a timer
      procedure Stop; overload;
      procedure Stop(Var T:TimerData); overload;

      //Return elapsed time in seconds as an extended type
      function Elapsed:Extended; overload;
      function Elapsed(var T: TimerData):Extended; overload;

      //Return a string in Day:Hour:Minute:Second format. Milliseconds can be
      //optionally appended via the WantMilliseconds property
      function ElapsedDHMS:String; overload;
      function ElapsedDHMS(var T: TimerData):String; overload;

      //Return a string in the format of seconds.milliseconds
      function ElapsedStr:String; overload;
      function ElapsedStr(var T:TimerData):String; overload;

      function WallClockTime:String; // Return time of day string from system time

      //Overhead compensated system sleep to provide a best possible precision delay
      function SystemSleep(Milliseconds: Integer):integer; Virtual;

      //Diagnostic taps for development and fine grained timebase adjustment
      property HWTimebase: TimeBaseData read FHWTicks write FHWTicks; // The hardware timebase
      property SysTimebase: TimebaseData read FSystemTicks write FSystemTicks;
      function GetHardwareTicks:TickType; // return raw tick value from hardware source
      function GetSystemTicks:Ticktype;   // Return system tick value(in microseconds of Epoch time)
      function GetTimebaseCorrelation:TickType;
      function CalibrateCallOverheads(Var TimeBase:TimebaseData) : Integer; Virtual;
      function CalibrateTickFrequency(Var TimeBase:TimebaseData): Integer; Virtual;

      property MicrosecondSystemClockAvailable:Boolean read FMicrosecondSystemClockAvailable;
      property SelectedTimebase:TimebaseSelector read FSelectedTimebase write FSelectedTimebase;
      property HWTickSupportAvailable:Boolean read FHWTickSupportAvailable;
      property HWCapabilityDataAvailable:Boolean read FHWCapabilityDataAvailable;
      procedure CorrelateTimebases; // Manually call to do timebase correlation snapshot and update

      constructor Create(AOwner:TComponent); Override;
      destructor Destroy; Override;
    Published
      property StringPrecision: FormatPrecision read FSPrecision write FSPrecision;
      property WantMilliseconds: Boolean read FWantMS write FWantMS;
      property WantDays: Boolean read FWantDays write FWantDays;
      property TimebaseSource: TickSources read FTimeBaseSource write SetTimebaseSource;
      property CorrelationMode:CorrelationModes read FCorrelationMode write FCorrelationMode;
  end;


implementation

(* * * * * * * * * * * * * * Timebase Section  * * * * * * * * * * * * *)
{
  There are two tick sources defined in this section. The first uses a hardware
  source which, in this case, is the Pentium's internal 64 Time Stamp Counter.
  The second source (the default) uses the given environment's most precision
  "timeofday" system call so it can work across OS platforms and architectures.
  
  The hardware timer's accuracy depends on the frequency of the timebase tick
  source that drives it... in other words, how many of the timebase's ticks
  there are in a second. This frequency is measured by capturing a sample of the
  timebase ticks for a known period against a source of known accuracy. There
  are two ways to do this.
  
  The first is to capture a large sample of ticks from both the unknown and
  known timing sources. Then the frequency of the unknown tick stream can be
  calculated by: UnknownSampleTicks / (KnownSampleTicks / KnownTickFrequency).
  Over a short period of time, this can provide a precise synchronization
  mechanism that effectively locks the measurements taken with the high
  resolution source to the known accuracy of the system clock.

  The first method depends on the existance of an accurate system time source of
  microsecond resolution. If the host system doesn't provide this, the second
  fallback method is to gate the unknown tick stream by a known time. This isn't
  as good because it usually involves calling a system "delay" routine that
  usually has a lot of overhead "jitter" and non-deterministic behavior. This
  approach is usable, however, for short term, high resolution comparisons where
  absolute accuracy isn't important.
}

(* * * * * * * * Start of i386 Hardware specific code  * * * * * * *)

{$IFDEF CPUI386}
{ Some references for this section can be found at:
      http://www.sandpile.org/ia32/cpuid.htm
      http://www.sandpile.org/ia32/opc_2.htm
      http://www.sandpile.org/ia32/msr.htm
}

// Pentium specific... push and pop the flags and check for CPUID availability
function HasHardwareCapabilityData: Boolean;
begin
  asm
   PUSHFD
   POP    EAX
   MOV    EDX,EAX
   XOR    EAX,$200000
   PUSH   EAX
   POPFD
   PUSHFD
   POP    EAX
   XOR    EAX,EDX
   JZ     @EXIT
   MOV    AL,TRUE
   @EXIT:
  end;
end;

function HasHardwareTickCounter: Boolean;
  var FeatureFlags: Longword;
  begin
    FeatureFlags:=0;
    asm
      PUSH   EBX
      XOR    EAX,EAX
      DW     $A20F
      POP    EBX
      CMP    EAX,1
      JL     @EXIT
      XOR    EAX,EAX
      MOV    EAX,1
      PUSH   EBX
      DW     $A20F
      MOV    FEATUREFLAGS,EDX
      POP    EBX
      @EXIT:
    end;
    Result := (FeatureFlags and $10) <> 0;
  end;

// Execute the Pentium's RDTSC instruction to access the counter value.
function HardwareTicks: TickType; assembler; asm DW 0310FH end;

(* * * * * * * * End of i386 Hardware specific code  * * * * * * *)


// These are here for architectures that don't have a precision hardware
// timing source. They'll return zeros for overhead values. The timers
// will work but there won't be any error compensation for long
// term accuracy.
{$ELSE} // add other architectures and hardware specific tick sources here
function HasHardwareCapabilityData: Boolean; begin Result:=False end;
function HasHardwareTickCounter: Boolean; begin Result:=false end;
function HardwareTicks:TickType; begin result:=0 end;
{$ENDIF}

function NullHardwareTicks:TickType; begin Result:=0 end;

// Return microsecond normalized time source for a given platform.
// This should be sync'able to an external time standard (via NTP, for example).
function SystemTicks: TickType;
{$IFDEF Windows}
begin
  QueryPerformanceCounter(Result{%H-});
  //Result := Int64(TimeStampToMSecs(DateTimeToTimeStamp(Now)) * 1000) // an alternative Win32 timebase
{$ELSE}
var t : timeval;
begin
  fpgettimeofday(@t,nil);
   // Build a 64 bit microsecond tick from the seconds and microsecond longints
  Result := (TickType(t.tv_sec) * 1000000) + t.tv_usec;
{$ENDIF}
end;

function TEpikTimer.SystemSleep(Milliseconds: Integer):Integer;
{$IFDEF Windows}

begin
  Sleep(Milliseconds);
  Result := 0;
end;

{$ELSE}

  {$IFDEF CPUX86_64}

begin
  Sleep(Milliseconds);
  Result := 0;
end;

  {$ELSE}

var
  timerequested, timeremaining: timespec;
begin
  // This is not a very accurate or stable gating source... but it's the
  // only one that's available for making short term measurements.
  timerequested.tv_sec:=Milliseconds div 1000;
  timerequested.tv_nsec:=(Milliseconds mod 1000) * 1000000;
  Result := fpnanosleep(@timerequested, @timeremaining) // returns 0 if ok
end;

  {$ENDIF}

{$ENDIF}

function TEpikTimer.GetHardwareTicks: TickType;
begin
  Result:=FHWTicks.Ticks();
end;

function TEpikTimer.GetSystemTicks: Ticktype;
begin
  Result:=FSystemTicks.Ticks();
end;

procedure TEpikTimer.SetTimebaseSource(const AValue: TickSources);

  procedure UseSystemTimer;
  begin
    FTimeBaseSource := SystemTimebase;
    SelectedTimebase := @FSystemTicks;
  end;

begin
  case AValue of
    HardwareTimebase:
      try
        if HWTickSupportAvailable then
          begin
            SelectedTimebase:=@FHWTicks;
            FTimeBaseSource:=HardwareTimebase;
            If CorrelationMode<>Manual then CorrelateTimebases
          end
      except // If HW init fails, fall back to system tick source
        UseSystemTimer
      end;
    SystemTimeBase: UseSystemTimer
  end
end;

function TEpikTimer.GetSelectedTimebase: TimebaseData;
begin
  Result := FSelectedTimebase^;
end;

procedure TEpikTimer.SetSelectedTimebase(const AValue: TimebaseData);
begin
  FSelectedTimebase^ := AValue;
end;

(* * * * * * * * * * Time measurement core routines * * * * * * * * * *)

procedure TEpikTimer.Clear(var T: TimerData);
begin
  with T do
    begin
      Running:=False; StartTime:=0; TotalTicks:=0; TimeBaseUsed:=FSelectedTimebase
    end;
end;

procedure TEpikTimer.Start(var T: TimerData);
begin
  if not T.running then
    With FSelectedTimebase^ do
    begin
      T.StartTime:=Ticks()-TicksOverhead;
      T.TimebaseUsed:=FSelectedTimebase;
      T.Running:=True
    end
end;

procedure TEpikTimer.Stop(var T: TimerData);
  Var CurTicks:TickType;
Begin
  if T.Running then
    With T.TimebaseUsed^ do
    Begin
      CurTicks:=Ticks()-TicksOverhead; // Back out the call overhead
      T.TotalTicks:=(CurTicks - T.Starttime)+T.TotalTicks; T.Running:=false
    end
end;

function TEpikTimer.Elapsed(var T: TimerData): Extended;
var
  CurTicks: TickType;
begin
  With T.TimebaseUsed^ do
    if T.Running then
      Begin

        CurTicks:=Ticks()-TicksOverhead; // Back out the call overhead
        If CorrelationMode>OnTimebaseSelect then CorrelateTimebases;

        Result := ((CurTicks - T.Starttime)+T.TotalTicks) / TicksFrequency
      End
    Else Result := T.TotalTicks / TicksFrequency;
end;

(* * * * * * * * * * Output formatting routines  * * * * * * * * * *)

function TEpikTimer.ElapsedDHMS(var T: TimerData): String;
var
  Tmp, MS: extended;
  D, H, M, S: Integer;
  P, SM: string;
begin
  Tmp := Elapsed(T);
  P := inttostr(FSPrecision);
  MS := frac(Tmp); SM:=format('%0.'+P+'f',[MS]); delete(SM,1,1);
  D := trunc(Tmp / 84600); Tmp:=Trunc(tmp) mod 84600;
  H := trunc(Tmp / 3600); Tmp:=Trunc(Tmp) mod 3600;
  M := Trunc(Tmp / 60); S:=(trunc(Tmp) mod 60);
  If FWantDays then
    Result := format('%2.3d:%2.2d:%2.2d:%2.2d',[D,H,M,S])
  else
    Result := format('%2.2d:%2.2d:%2.2d',[H,M,S]);
  If FWantMS then Result:=Result+SM;
end;

function TEpikTimer.ElapsedStr(var T: TimerData): string;
begin
  Result := format('%.'+inttostr(FSPrecision)+'f',[Elapsed(T)]);
end;

function TEpikTimer.WallClockTime: string;
var
  Y, D, M, hour, min, sec, ms, us: Word;
{$IFNDEF Windows}
  t: timeval;
{$ENDIF}
begin
{$IFDEF Windows}
  DecodeDatetime(Now, Y, D, M, Hour, min, Sec, ms);
  us:=0;
{$ELSE}
  // "Now" doesn't report milliseconds on Linux... appears to be broken.
  // I opted for this approach which also provides microsecond precision.
  fpgettimeofday(@t,nil);
  EpochToLocal(t.tv_sec, Y, M, D, hour, min, sec);
  ms:=t.tv_usec div 1000; us:=t.tv_usec mod 1000;
{$ENDIF}
  Result:='';
  If FWantDays then
    Result := Format('%4.4d/%2.2d/%2.2d-',[Y,M,D]);
  Result := Result + Format('%2.2d:%2.2d:%2.2d',[hour,min,sec]);
  If FWantMS then
    Result := Result + Format('.%3.3d%3.3d',[ms,us])
end;

(* * * Overloaded methods to use the component's internal timer data * * *)

procedure TEpikTimer.Clear; begin Clear(BuiltInTimer) end;
procedure TEpikTimer.Start; begin Start(BuiltInTimer) end;
procedure TEpikTimer.Stop;  Begin Stop(BuiltInTimer) End;
function  TEpikTimer.Elapsed: Extended; begin Result:=Elapsed(BuiltInTimer) end;
function  TEpikTimer.ElapsedStr: String; Begin Result:=ElapsedStr(BuiltInTimer) end;
function  TEpikTimer.ElapsedDHMS: String; begin Result:=ElapsedDHMS(BuiltInTimer) end;

(* * * * * * * * * * Timebase calibration section  * * * * * * * * * *)

// Set up compensation for call overhead to the Ticks and SystemSleep functions.
// The Timebase record contains Calibration parameters to be used for each
// timebase source. These have to be unique as the output of this measurement
// is measured in "ticks"... which are different periods for each timebase.

function TEpikTimer.CalibrateCallOverheads(Var Timebase:TimebaseData):Integer;
var i:Integer; St,Fin,Total:TickType;
begin
  with Timebase, Timebase.CalibrationParms do
  begin
    Total:=0; Result:=1;
    for I:=1 to TicksIterations do // First get the base tick getting overhead
      begin
        St:=Ticks(); Fin:=Ticks();
        Total:=Total+(Fin-St); // dump the first sample
      end;
    TicksOverhead:=Total div TicksIterations;
    Total:=0;
    For I:=1 to SleepIterations do
    Begin
      St:=Ticks();
      if SystemSleep(0)<>0 then exit;
      Fin:=Ticks();
      Total:=Total+((Fin-St)-TicksOverhead);
    End;
    SleepOverhead:=Total div SleepIterations;
    OverheadCalibrated:=True; Result:=0
  End
end;

// CalibrateTickFrequency is a fallback in case a microsecond resolution system
// clock isn't found. It's still important because the long term accuracy of the
// timers will depend on the determination of the tick frequency... in other words,
// the number of ticks it takes to make a second. If this measurement isn't
// accurate, the counters will proportionately drift over time.
//
// The technique used here is to gate a sample of the tick stream with a known
// time reference which, in this case, is nanosleep. There is a *lot* of jitter
// in a nanosleep call so an attempt is made to compensate for some of it here.

function TEpikTimer.CalibrateTickFrequency(Var Timebase:TimebaseData):Integer;
var
  i: Integer;
  Total, SS, SE: TickType;
  ElapsedTicks, SampleTime: Extended;
begin
  With Timebase, Timebase.CalibrationParms do
  Begin
    Result:=1; //maintain unitialized default in case something goes wrong.
    Total:=0;
    For i:=1 to FreqIterations do
      begin
        SS:=Ticks();
        SystemSleep(FrequencyGateTimeMS);
        SE:=Ticks();
        Total:=Total+((SE-SS)-(SleepOverhead+TicksOverhead))
      End;
    //doing the floating point conversion allows SampleTime parms of < 1 second
    ElapsedTicks:=Total div FreqIterations;
    SampleTime:=FrequencyGateTimeMS;

    TicksFrequency:=Trunc( ElapsedTicks / (SampleTime / 1000));

    FreqCalibrated:=True;
  end;
end;

// Grab a snapshot of the system and hardware tick sources... as quickly as
// possible and with overhead compensation. These samples will be used to
// correct the accuracy of the hardware tick frequency source when precision
// long term measurements are desired.
procedure TEpikTimer.GetCorrelationSample(var CorrelationData: TimeBaseCorrelationData);
Var
  TicksHW, TicksSys: TickType;
  THW, TSYS: TickCallFunc;
begin
  THW:=FHWTicks.Ticks; TSYS:=FSystemTicks.Ticks;
  TicksHW:=THW(); TicksSys:=TSYS();
  With CorrelationData do
    Begin
      SystemTicks:= TicksSys-FSystemTicks.TicksOverhead;
      HWTicks:=TicksHW-FHWTicks.TicksOverhead;
    End
end;

(* * * * * * * * * * Timebase correlation section  * * * * * * * * * *)

{ Get another snapshot of the system and hardware tick sources and compute a
  corrected value for the hardware frequency. In a short amount of time, the
  microsecond system clock accumulates enough ticks to perform a *very*
  accurate frequency measurement of the typically picosecond time stamp counter. }

Function TEpikTimer.GetTimebaseCorrelation:TickType;
Var
  HWDiff, SysDiff, Corrected: Extended;
begin
  If HWtickSupportAvailable then
    Begin
      GetCorrelationSample(UpdatedCorrelationSample);
      HWDiff:=UpdatedCorrelationSample.HWTicks-StartupCorrelationSample.HWTicks;
      SysDiff:=UpdatedCorrelationSample.SystemTicks-StartupCorrelationSample.SystemTicks;
      Corrected:=HWDiff / (SysDiff / DefaultSystemTicksPerSecond);
      Result:=trunc(Corrected)
    End
  else result:=0
end;

{ If an accurate reference is available, update the TicksFrequency of the
  hardware timebase. }
procedure TEpikTimer.CorrelateTimebases;
begin
  If MicrosecondSystemClockAvailable and HWTickSupportAvailable then
    FHWTicks.TicksFrequency:=GetTimebaseCorrelation
end;

(* * * * * * * * Initialization: Constructor and Destructor  * * * * * * *)

constructor TEpikTimer.Create(AOwner: TComponent);

  Procedure InitTimebases;
  Begin

    { Tick frequency rates are different for the system and HW timebases so we
      need to store calibration data in the period format of each one. }
    FSystemTicks.Ticks:=@SystemTicks; // Point to Ticks routine
    With FSystemTicks.CalibrationParms do
      Begin
        FreqCalibrated:=False;
        OverheadCalibrated:=False;
        TicksIterations:=5;
        SleepIterations:=10;
        FrequencyGateTimeMS:=100;
        FreqIterations:=1;
      End;

    // Initialize the HW tick source data
    FHWCapabilityDataAvailable:=False;
    FHWTickSupportAvailable:=False;
    FHWTicks.Ticks:=@NullHardwareTicks; // returns a zero if no HW support
    FHWTicks.TicksFrequency:=1;
    With FHWTicks.CalibrationParms do
      Begin
        FreqCalibrated:=False;
        OverheadCalibrated:=False;
        TicksIterations:=10;
        SleepIterations:=20;
        FrequencyGateTimeMS:=150;
        FreqIterations:=1;
      End;

    if HasHardwareCapabilityData then
      Begin
        FHWCapabilityDataAvailable:=True;
        If HasHardwareTickCounter then
          Begin
            FHWTicks.Ticks:=@HardwareTicks;
            FHWTickSupportAvailable:=CalibrateCallOverheads(FHWTicks)=0
          End
      end;
         
    CalibrateCallOverheads(FSystemTicks);
    CalibrateTickFrequency(FSystemTicks);

    // Overheads are set... get starting timestamps for long term calibration runs
    GetCorrelationSample(StartupCorrelationSample);
    With FSystemTicks do
      If (TicksFrequency>(DefaultSystemTicksPerSecond-SystemTicksNormalRangeLimit)) and
        (TicksFrequency<(DefaultSystemTicksPerSecond+SystemTicksNormalRangeLimit)) then
        Begin // We've got a good microsecond system clock
          FSystemTicks.TicksFrequency:=DefaultSystemTicksPerSecond; // assume it's pure
          FMicrosecondSystemClockAvailable:=True;
          If FHWTickSupportAvailable then
            Begin
              SystemSleep(FHWTicks.CalibrationParms.FrequencyGateTimeMS); // rough gate
              CorrelateTimebases
            End
        end
      else
        Begin
          FMicrosecondSystemClockAvailable:=False;
          If FHWTickSupportAvailable then
            CalibrateTickFrequency(FHWTicks) // sloppy but usable fallback calibration
        End;
 End;

begin
  inherited Create(AOwner);
  StringPrecision:=6; FWantMS:=True; FWantDays:=True;
  InitTimebases;
  CorrelationMode:=OnTimebaseSelect;
  // Default is the safe, cross-platform but less precise system timebase
  TimebaseSource:=SystemTimebase;
  Clear(BuiltInTimer)
end;

destructor TEpikTimer.Destroy;
begin
  inherited Destroy;
  // here in case we need to clean something up in a later version
end;

end.

