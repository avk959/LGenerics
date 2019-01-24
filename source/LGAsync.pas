{****************************************************************************
*                                                                           *
*   This file is part of the LGenerics package.                             *
*   Brief and dirty futures implementation.                                 *
*                                                                           *
*   Copyright(c) 2018-2019 A.Koverdyaev(avk)                                *
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
{
  The futures concept describes an asynchronous single-execution pattern.
  Result is requested at an early stage of execution, but becomes available after it is received.

  This implementation implies that futures are intended for use from the main thread.
}

unit LGAsync;

{$mode objfpc}{$H+}
{$INLINE ON}{$WARN 6058 off : }
{$MODESWITCH ADVANCEDRECORDS}
{$MODESWITCH NESTEDPROCVARS}

interface

uses
  Classes,
  SysUtils,
  LGUtils,
  LGQueue,
  LGVector,
  LGFunction,
  LGStrConst;

type

  TAsyncTask = class abstract
  public
  type
    TTaskState = (tsPending, tsExecuting, tsFinished, tsCancelled);

  strict private
    FAwait: PRtlEvent;
    FException: Exception;
    FState: TTaskState;
    procedure AsyncFree;
  strict protected
    procedure DoExecute; virtual; abstract;
  public
    destructor Destroy; override;
    procedure AfterConstruction; override;
    procedure Cancel;
    procedure Execute;
    procedure WaitFor;
    property  FatalException: Exception read FException;
    property  State: TTaskState read FState;
  end;

  generic TGAsyncTask<T> = class abstract(TAsyncTask)
  strict protected
    FResult: T; //To be setted inside overriden DoExecute
  public
    property Result: T read FResult;
  end;

{$PUSH}{$INTERFACES CORBA}
  IExecutor = interface
  ['{49381C21-82D6-456E-93A2-B8E0DC4B34BA}']
    procedure EnqueueTask(aTask: TAsyncTask);
  end;
{$POP}

  TFutureState = (fsPending, fsExecuting, fsFinished, fsResolved, fsFatal, fsCancelled);

  { TGFuture: takes over the management of the inner async task }
  generic TGFuture<T> = record
  public
  type
    TOptional = specialize TGOptional<T>;

  private
  type
    TTask = specialize TGAsyncTask<T>;

  strict private
    FTask: TTask;
    FTaskResult: T;
    FState: TFutureState;
    procedure Resolve;
  private
    function GetState: TFutureState;
    procedure Start(aTask: TTask; aEx: IExecutor);
    class operator Finalize(var f: TGFuture);
  public
    function  WaitFor: TFutureState;
  { may be impossible, if task already started }
    function  Cancel: Boolean;
  { raises exception if resolving failed }
    function  Value: T;
    function  OptValue: TOptional;
    property  State: TFutureState read GetState;
  end;

  { TGAsyncProc incapsulates method without arguments(which returns void), True indicates execution success }
  TGAsyncProc = class(specialize TGAsyncTask<Boolean>)
  public
  type
    TProcedure = procedure of object;
    TFuture    = specialize TGFuture<Boolean>;

  strict private
    FProc: TProcedure;
  strict protected
    procedure DoExecute; override;
  public
    class function Call(aProc: TProcedure; aEx: IExecutor = nil): TFuture; static;
    constructor Create(aProc: TProcedure);
  end;

  { TAsyncExecutable incapsulates IExecutable, True indicates execution success }
  TAsyncExecutable = class(specialize TGAsyncTask<Boolean>)
  public
  type
    TFuture = specialize TGFuture<Boolean>;

  strict private
    FTask: IExecutable;
  strict protected
    procedure DoExecute; override;
  public
    class function Run(aTask: IExecutable; aEx: IExecutor = nil): TFuture; static;
    constructor Create(aTask: IExecutable);
  end;

  { TGAsyncCallable incapsulates IGCallable}
  generic TGAsyncCallable<T> = class(specialize TGAsyncTask<T>)
  public
  type
    ICallable = specialize IGCallable<T>;
    TFuture   = specialize TGFuture<T>;

  strict private
    FTask: ICallable;
  strict protected
    procedure DoExecute; override;
  public
    class function Run(aTask: ICallable; aEx: IExecutor = nil): TFuture; static;
    constructor Create(aTask: ICallable);
  end;

  { TGAsyncMethod incapsulates niladic method(without arguments which returns T) }
  generic TGAsyncMethod<T> = class(specialize TGAsyncTask<T>)
  public
  type
    TFun    = function: T of object;
    TFuture = specialize TGFuture<T>;

  strict private
    FFun: TFun;
  strict protected
    procedure DoExecute; override;
  public
    class function Call(aFun: TFun; aEx: IExecutor = nil): TFuture; static;
    constructor Create(aFun: TFun);
  end;

  { TGAsyncNestFun incapsulates nested niladic function (without arguments) }
  generic TGAsyncNestFun<T> = class(specialize TGAsyncTask<T>)
  public
  type
    TFun    = function: T is nested;
    TFuture = specialize TGFuture<T>;

  strict private
    FFun: TFun;
  strict protected
    procedure DoExecute; override;
  public
    class function Call(aFun: TFun; aEx: IExecutor = nil): TFuture; static;
    constructor Create(aFun: TFun);
  end;

  { TGAsyncNiladic incapsulates regular niladic function (without arguments) }
  generic TGAsyncNiladic<T> = class(specialize TGAsyncTask<T>)
  strict protected
  public
  type
    TFun    = function: T;
    TFuture = specialize TGFuture<T>;

  strict private
    FFun: TFun;
  strict protected
    procedure DoExecute; override;
  public
    class function Call(aFun: TFun; aEx: IExecutor = nil): TFuture; static;
    constructor Create(aFun: TFun);
  end;

  { TGAsyncMonadic incapsulates regular monadic function (with one argument) }
  generic TGAsyncMonadic<T, TResult> = class(specialize TGAsyncTask<TResult>)
  strict protected
  type
    TCall = specialize TGDeferMonadic<T, TResult>;

  public
  type
    TFun    = TCall.TFun;
    TFuture = specialize TGFuture<TResult>;

  strict private
    FCall: TCall;
  strict protected
    procedure DoExecute; override;
  public
    class function Call(aFun: TFun; constref v: T; aEx: IExecutor = nil): TFuture; static;
    constructor Create(aFun: TFun; constref v: T);
  end;

  { TGAsyncDyadic incapsulates regular dyadic function (with two arguments) }
  generic TGAsyncDyadic<T1, T2, TResult> = class(specialize TGAsyncTask<TResult>)
  strict protected
  type
    TCall = specialize TGDeferDyadic<T1, T2, TResult>;

  public
  type
    TFun    = TCall.TFun;
    TFuture = specialize TGFuture<TResult>;

  strict private
    FCall: TCall;
  strict protected
    procedure DoExecute; override;
  public
    class function Call(aFun: TFun; constref v1: T1; constref v2: T2; aEx: IExecutor = nil): TFuture; static;
    constructor Create(aFun: TFun; constref v1: T1; constref v2: T2);
  end;

  { TGAsyncTriadic incapsulates regular triadic function (with three arguments) }
  generic TGAsyncTriadic<T1, T2, T3, TResult> = class(specialize TGAsyncTask<TResult>)
  strict protected
  type
    TCall = specialize TGDeferTriadic<T1, T2, T3, TResult>;

  public
  type
    TFun    = TCall.TFun;
    TFuture = specialize TGFuture<TResult>;

  strict private
    FCall: TCall;
  strict protected
    procedure DoExecute; override;
  public
    class function Call(aFun: TFun; constref v1: T1; constref v2: T2; constref v3: T3;
                        aEx: IExecutor = nil): TFuture; static;
    constructor Create(aFun: TFun; constref v1: T1; constref v2: T2; constref v3: T3);
  end;

  { TDefaultExecutor executes futures in its own thread pool.
    Enqueue procedure is threadsafe, so futures may use other futures.
    Resizing of thread pool is not threadsafe, so MUST be done from main thread. }
  TDefaultExecutor = class(TObject, IExecutor)
  protected
  type

    TTaskQueue = class
    strict private
    type
      TQueue = specialize TGLiteQueue<TAsyncTask>;

    var
      FQueue: TQueue;
      FReadAwait: PRtlEvent;
      FLock: TRtlCriticalSection;
      FClosed: Boolean;
      procedure Lock; inline;
      procedure UnLock; inline;
      procedure Signaled; inline;
      property  Closed: Boolean read FClosed;
    public
      constructor Create;
      destructor Destroy; override;
      procedure AfterConstruction; override;
      procedure Clear;
      procedure Close;
      procedure Open;
      procedure Enqueue(aTask: TAsyncTask);
      function  Dequeue(out aTask: TAsyncTask): Boolean;
    end;

    TWorkThread = class(TThread)
    strict private
      FQueue: TTaskQueue;
    public
      constructor Create(aQueue: TTaskQueue);
      procedure Execute; override;
    end;

    TThreadPool = specialize TGLiteVector<TWorkThread>;

  var
    FTaskQueue: TTaskQueue;
    FThreadPool: TThreadPool;
    function  ThreadPoolCount: Integer; inline;
    function  AddThread: TWorkThread;
    procedure PoolGrow(aValue: Integer);
    procedure PoolShrink(aValue: Integer);
    procedure EnqueueTask(aTask: TAsyncTask); inline;
    procedure TerminatePool;
    procedure FinalizePool; inline;
    class constructor InitNil;
    class destructor  DoneQueue;
    class function    GetThreadCount: Integer; static; inline;
    class procedure   SetThreadCount(aValue: Integer); static; inline;
  class var
    CFExecutor: TDefaultExecutor; // CF -> Class Field

  public
  const
    DEFAULT_THREAD_COUNT = 4;

    class procedure EnsureThreadCount(aValue: Integer); static;
    class procedure Enqueue(aTask: TAsyncTask); static; inline;
    class function  Instance: IExecutor; static;
    constructor Create; overload;
    constructor Create(aThreadCount: Integer); overload;
    destructor  Destroy; override;
    class property ThreadCount: Integer read GetThreadCount write SetThreadCount;
  end;

  { TGBlockingChannel }

  generic TGBlockingChannel<T, TSize> = class //todo: replace in unit ???
  strict protected
  type
    TBuffer = array[0..Pred(TSize.Size)] of T;

  var
    FBuffer: TBuffer;
    FLock: TRtlCriticalSection;
    FWriteAwait,
    FReadAwait: PRtlEvent;
    FCount,
    FHead: SizeInt;
    FActive: Boolean;
    function  GetCapacity: SizeInt; inline;
    procedure Lock; inline;
    procedure UnLock; inline;
    procedure SignalRead; inline;
    procedure SignalWrite; inline;
    procedure SendData(constref aValue: T);
    function  ReceiveData: T;
    function  TailIndex: SizeInt; inline;
    procedure Enqueue(constref aValue: T);
    function  Dequeue: T;
    function  CanWrite: Boolean; inline;
    function  CanRead: Boolean; inline;
    procedure Panic; inline;
    procedure CheckCanRead; inline;
    procedure CheckCanWrite; inline;
    procedure CleanupBuffer; virtual;
    property  Head: SizeInt read FHead;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AfterConstruction; override;
    function  Send(constref aValue: T): Boolean;
    function  Receive(out aValue: T): Boolean;
    procedure Close;
    procedure Open;
  { if is not Active then Send and Receive will always return False without blocking }
    property  Active: Boolean read FActive;
    property  Count: SizeInt read FCount;
    property  Capacity: SizeInt read GetCapacity;
  end;

  { TGObjectBlockingChannel }

  generic TGObjectBlockingChannel<T: class; TSize> = class(specialize TGBlockingChannel<T, TSize>)
  strict private
    FOwnsObjects: Boolean;
  strict protected
    procedure CleanupBuffer; override;
  public
    constructor Create(aOwnsObjects: Boolean = True);
    property OwnsObjects: Boolean read FOwnsObjects write FOwnsObjects;
  end;

  TDefaultSize = class sealed
  const
    Size = 256;
  end;

  generic TGDefaultChannel<T> = class(specialize TGBlockingChannel<T, TDefaultSize>);

implementation
{$B-}{$COPERATORS ON}

{ TAsyncTask }

procedure TAsyncTask.Cancel;
begin
  if FState = tsPending then
    FState := tsCancelled;
end;

procedure TAsyncTask.AsyncFree;
begin
  Free;
end;

destructor TAsyncTask.Destroy;
begin
  System.RtlEventDestroy(FAwait);
  FAwait := nil;
  inherited;
end;

procedure TAsyncTask.AfterConstruction;
begin
  inherited;
  FAwait := System.RtlEventCreate;
end;

procedure TAsyncTask.Execute;
begin
  if FState = tsPending then
    begin
      FState := tsExecuting;
      WriteBarrier;
      try
        DoExecute;
      except
        on e: Exception do
          FException := Exception(System.AcquireExceptionObject);
      end;
      WriteBarrier;
      FState := tsFinished;
    end;
  System.RtlEventSetEvent(FAwait);
  if FState = tsCancelled then
    TThread.Queue(TThread.CurrentThread, @AsyncFree);
end;

procedure TAsyncTask.WaitFor;
begin
  System.RtlEventWaitFor(FAwait);
end;

{ TGFuture }

procedure TGFuture.Resolve;
var
  e: Exception = nil;
begin
  if Assigned(FTask) and (State < fsResolved) then
    try
      FTask.WaitFor;
      e := FTask.FatalException;
      if Assigned(e) then
        FState := fsFatal
      else
        begin
          FState := fsResolved;
          FTaskResult := FTask.Result;
        end;
    finally
      FreeAndNil(FTask);
      if Assigned(e) then
        raise e;
    end;
end;

function TGFuture.GetState: TFutureState;
begin
  if Assigned(FTask) and (FState < fsResolved) then
    try
      case FTask.State of
        tsExecuting: FState := fsExecuting;
        tsFinished:  FState := fsFinished;
      end;
    except
      FState := fsCancelled;
      FTask := nil;
    end;
  Result := FState;
end;

procedure TGFuture.Start(aTask: TTask; aEx: IExecutor);
begin
  FTask := aTask;
  FState := fsPending;
  if aEx = nil then
    aEx := TDefaultExecutor.Instance;
  aEx.EnqueueTask(FTask);
end;

class operator TGFuture.Finalize(var f: TGFuture);
begin
  f.Cancel;
  f.WaitFor;
end;

function TGFuture.WaitFor: TFutureState;
begin
  try
    Resolve;
  except
  end;
  Result := FState;
end;

function TGFuture.Cancel: Boolean;
begin
  if Assigned(FTask) and (FState = fsPending) then
    begin
      FTask.Cancel;
      Result := FTask.State = tsCancelled;
      if Result then
        begin
          FState := fsCancelled;
          FTask := nil;
        end;
    end
  else
    Result := False;
end;

function TGFuture.Value: T;
begin
  case State of
    fsPending..fsFinished:
      Resolve;
    fsFatal:
      raise ELGFuture.Create(SEResultUnknownFatal);
    fsCancelled:
      raise ELGFuture.Create(SEResultUnknownCancel);
  end;
  Result := FTaskResult;
end;

function TGFuture.OptValue: TOptional;
begin
  if WaitFor = fsResolved then
    Result.Assign(FTaskResult);
end;

{ TGAsyncProc }

procedure TGAsyncProc.DoExecute;
begin
  FProc();
  FResult := FatalException = nil;
end;

class function TGAsyncProc.Call(aProc: TProcedure; aEx: IExecutor): TFuture;
begin
  Result{%H-}.Start(TGAsyncProc.Create(aProc), aEx);
end;

constructor TGAsyncProc.Create(aProc: TProcedure);
begin
  inherited Create;
  FProc := aProc;
end;

{ TAsyncExecutable }

procedure TAsyncExecutable.DoExecute;
begin
  FTask.Execute;
  FResult := FatalException = nil;
end;

class function TAsyncExecutable.Run(aTask: IExecutable; aEx: IExecutor): TFuture;
begin
  Result{%H-}.Start(TAsyncExecutable.Create(aTask), aEx);
end;

constructor TAsyncExecutable.Create(aTask: IExecutable);
begin
  inherited Create;
  FTask := aTask;
end;

{ TGAsyncCallable }

procedure TGAsyncCallable.DoExecute;
begin
  FResult := FTask.Call;
end;

class function TGAsyncCallable.Run(aTask: ICallable; aEx: IExecutor): TFuture;
begin
  Result.Start(TGAsyncCallable.Create(aTask), aEx);
end;

constructor TGAsyncCallable.Create(aTask: ICallable);
begin
  inherited Create;
  FTask := aTask;
end;

{ TGAsyncMethod }

procedure TGAsyncMethod.DoExecute;
begin
  FResult := FFun();
end;

class function TGAsyncMethod.Call(aFun: TFun; aEx: IExecutor): TFuture;
begin
  Result.Start(TGAsyncMethod.Create(aFun), aEx);
end;

constructor TGAsyncMethod.Create(aFun: TFun);
begin
  inherited Create;
  FFun := aFun;
end;

{ TGAsyncNested }

procedure TGAsyncNestFun.DoExecute;
begin
  FResult := FFun();
end;

class function TGAsyncNestFun.Call(aFun: TFun; aEx: IExecutor): TFuture;
begin
  Result.Start(TGAsyncNestFun.Create(aFun), aEx);
end;

constructor TGAsyncNestFun.Create(aFun: TFun);
begin
  inherited Create;
  FFun := aFun;
end;

{ TGAsyncNiladic }

procedure TGAsyncNiladic.DoExecute;
begin
  FResult := FFun();
end;

class function TGAsyncNiladic.Call(aFun: TFun; aEx: IExecutor): TFuture;
begin
  Result.Start(TGAsyncNiladic.Create(aFun), aEx);
end;

constructor TGAsyncNiladic.Create(aFun: TFun);
begin
  inherited Create;
  FFun := aFun;
end;

{ TGAsyncMonadic }

procedure TGAsyncMonadic.DoExecute;
begin
  FResult := FCall.Call;
end;

class function TGAsyncMonadic.Call(aFun: TFun; constref v: T; aEx: IExecutor): TFuture;
begin
  Result{%H-}.Start(TGAsyncMonadic.Create(aFun, v), aEx);
end;

constructor TGAsyncMonadic.Create(aFun: TFun; constref v: T);
begin
  inherited Create;
  FCall := TCall.Construct(aFun, v);
end;

{ TGAsyncDyadic }

procedure TGAsyncDyadic.DoExecute;
begin
  FResult := FCall.Call;
end;

class function TGAsyncDyadic.Call(aFun: TFun; constref v1: T1; constref v2: T2; aEx: IExecutor): TFuture;
begin
  Result.Start(TGAsyncDyadic.Create(aFun, v1, v2), aEx);
end;

constructor TGAsyncDyadic.Create(aFun: TFun; constref v1: T1; constref v2: T2);
begin
  inherited Create;
  FCall := TCall.Construct(aFun, v1, v2);
end;

{ TGAsyncTriadic }

procedure TGAsyncTriadic.DoExecute;
begin
  FResult := FCall.Call;
end;

class function TGAsyncTriadic.Call(aFun: TFun; constref v1: T1; constref v2: T2; constref v3: T3;
  aEx: IExecutor): TFuture;
begin
  Result{%H-}.Start(TGAsyncTriadic.Create(aFun, v1, v2, v3), aEx);
end;

constructor TGAsyncTriadic.Create(aFun: TFun; constref v1: T1; constref v2: T2; constref v3: T3);
begin
  inherited Create;
  FCall := TCall.Construct(aFun, v1, v2, v3);
end;

{ TDefaultExecutor.TTaskQueue }

procedure TDefaultExecutor.TTaskQueue.Lock;
begin
  System.EnterCriticalSection(FLock);
end;

procedure TDefaultExecutor.TTaskQueue.UnLock;
begin
  System.LeaveCriticalSection(FLock);
end;

procedure TDefaultExecutor.TTaskQueue.Signaled;
begin
  System.RtlEventSetEvent(FReadAwait);
end;

constructor TDefaultExecutor.TTaskQueue.Create;
begin
  inherited;
  System.InitCriticalSection(FLock);
end;

destructor TDefaultExecutor.TTaskQueue.Destroy;
begin
  Lock;
  try
    Finalize(FQueue);
    System.RtlEventDestroy(FReadAwait);
    FReadAwait := nil;
    inherited;
  finally
    UnLock;
    System.DoneCriticalSection(FLock);
  end;
end;

procedure TDefaultExecutor.TTaskQueue.AfterConstruction;
begin
  inherited;
  FReadAwait := System.RtlEventCreate;
end;

procedure TDefaultExecutor.TTaskQueue.Clear;
var
  Task: TAsyncTask;
begin
  Lock;
  try
    for Task in FQueue do
      Task.Cancel;
    FQueue.Clear;
  finally
    UnLock;
  end;
end;

procedure TDefaultExecutor.TTaskQueue.Close;
begin
  Lock;
  try
    FClosed := True;
    Signaled;
  finally
    UnLock;
  end;
end;

procedure TDefaultExecutor.TTaskQueue.Open;
begin
  Lock;
  try
    FClosed := False;
  finally
    UnLock;
  end;
end;

procedure TDefaultExecutor.TTaskQueue.Enqueue(aTask: TAsyncTask);
begin
  Lock;
  try
    FQueue.Enqueue(aTask);
    Signaled;
  finally
    UnLock;
  end;
end;

function TDefaultExecutor.TTaskQueue.Dequeue(out aTask: TAsyncTask): Boolean;
begin
  System.RtlEventWaitFor(FReadAwait);
  Lock;
  try
    if not Closed then
      begin
        Result := FQueue.TryDequeue(aTask);
        if FQueue.NonEmpty then
         Signaled;
      end
    else
      begin
        Result := False;
        Signaled;
      end;
  finally
    UnLock;
  end;
end;

{ TDefaultExecutor.TWorkThread }

constructor TDefaultExecutor.TWorkThread.Create(aQueue: TTaskQueue);
begin
  inherited Create(True);
  FQueue := aQueue;
end;

procedure TDefaultExecutor.TWorkThread.Execute;
var
  CurrTask: TAsyncTask;
begin
  while not Terminated do
    if FQueue.Dequeue(CurrTask) then
      CurrTask.Execute;
end;

{ TDefaultExecutor }

function TDefaultExecutor.ThreadPoolCount: Integer;
begin
  Result := FThreadPool.Count;
end;

function TDefaultExecutor.AddThread: TWorkThread;
begin
  Result := TWorkThread.Create(FTaskQueue);
  FThreadPool.Add(Result);
  Result.Start;
end;

procedure TDefaultExecutor.PoolGrow(aValue: Integer);
begin
  while FThreadPool.Count < aValue do
    AddThread;
end;

procedure TDefaultExecutor.PoolShrink(aValue: Integer);
begin
  if aValue < 1 then
    aValue := 1;
  TerminatePool;
  FTaskQueue.Open;
  PoolGrow(aValue);
end;

procedure TDefaultExecutor.EnqueueTask(aTask: TAsyncTask);
begin
  FTaskQueue.Enqueue(aTask);
end;

procedure TDefaultExecutor.TerminatePool;
var
  Thread: TWorkThread;
begin
  for Thread in FThreadPool.Reverse do
    Thread.Terminate;
  FTaskQueue.Close;
  while FThreadPool.Count > 0 do
    begin
      Thread := FThreadPool.Extract(Pred(FThreadPool.Count));
      Thread.WaitFor;
      Thread.Free;
    end;
end;

procedure TDefaultExecutor.FinalizePool;
begin
  TerminatePool;
  FThreadPool.Clear;
end;

class constructor TDefaultExecutor.InitNil;
begin
  CFExecutor := nil;
end;

class destructor TDefaultExecutor.DoneQueue;
begin
  FreeAndNil(CFExecutor);
end;

class function TDefaultExecutor.GetThreadCount: Integer; static;
begin
  if Assigned(CFExecutor) then
    Result := CFExecutor.ThreadPoolCount
  else
    Result := 0;
end;

class procedure TDefaultExecutor.SetThreadCount(aValue: Integer);
begin
  if aValue > ThreadCount then
    EnsureThreadCount(aValue)
  else
    if Assigned(CFExecutor) then
      CFExecutor.PoolShrink(aValue);
end;

class procedure TDefaultExecutor.EnsureThreadCount(aValue: Integer);
begin
  if aValue > ThreadCount then
    if not Assigned(CFExecutor) then
      CFExecutor := TDefaultExecutor.Create(aValue)
    else
      CFExecutor.PoolGrow(aValue);
end;

class procedure TDefaultExecutor.Enqueue(aTask: TAsyncTask);
begin
  if not Assigned(CFExecutor) then
    CFExecutor := TDefaultExecutor.Create;
  CFExecutor.EnqueueTask(aTask);
end;

class function TDefaultExecutor.Instance: IExecutor;
begin
  if not Assigned(CFExecutor) then
    CFExecutor := TDefaultExecutor.Create;
  Result := IExecutor(CFExecutor);
end;

constructor TDefaultExecutor.Create;
begin
  if TThread.ProcessorCount > DEFAULT_THREAD_COUNT then
    Create(TThread.ProcessorCount)
  else
    Create(DEFAULT_THREAD_COUNT);
end;

constructor TDefaultExecutor.Create(aThreadCount: Integer);
begin
  FTaskQueue := TTaskQueue.Create;
  if aThreadCount > 0 then
    PoolGrow(aThreadCount)
  else
    PoolGrow(1);
end;

destructor TDefaultExecutor.Destroy;
begin
  FTaskQueue.Clear;
  FinalizePool;
  FTaskQueue.Free;
  inherited;
end;

{ TGBlockingChannel }

function TGBlockingChannel.GetCapacity: SizeInt;
begin
  Result := TSize.Size;
end;

procedure TGBlockingChannel.Lock;
begin
  System.EnterCriticalSection(FLock);
end;

procedure TGBlockingChannel.UnLock;
begin
  System.LeaveCriticalSection(FLock);
end;

procedure TGBlockingChannel.SignalRead;
begin
  System.RtlEventSetEvent(FReadAwait);
end;

procedure TGBlockingChannel.SignalWrite;
begin
  System.RtlEventSetEvent(FWriteAwait);
end;

procedure TGBlockingChannel.SendData(constref aValue: T);
begin
  Enqueue(aValue);
  SignalRead;
  if CanWrite then
    SignalWrite;
end;

function TGBlockingChannel.ReceiveData: T;
begin
  Result := Dequeue;
  SignalWrite;
  if CanRead then
    SignalRead;
end;

function TGBlockingChannel.TailIndex: SizeInt;
begin
  Result := Head + Count;
  if Result >= Capacity then
    Result -= Capacity;
end;

procedure TGBlockingChannel.Enqueue(constref aValue: T);
begin
  CheckCanWrite;
  FBuffer[TailIndex] := aValue;
  Inc(FCount);
end;

function TGBlockingChannel.Dequeue: T;
begin
  CheckCanRead;
  Result := FBuffer[Head];
  FBuffer[Head] := Default(T);
  Inc(FHead);
  Dec(FCount);
  if Head = Capacity then
    FHead := 0;
end;

function TGBlockingChannel.CanWrite: Boolean;
begin
  Result := Count < Capacity;
end;

function TGBlockingChannel.CanRead: Boolean;
begin
  Result := Count > 0;
end;

procedure TGBlockingChannel.Panic;
begin
  raise ELGPanic.Create(SEInternalDataInconsist);
end;

procedure TGBlockingChannel.CheckCanRead;
begin
  if Count = 0 then
    Panic;
end;

procedure TGBlockingChannel.CheckCanWrite;
begin
  if Count >= Capacity then
    Panic;
end;

procedure TGBlockingChannel.CleanupBuffer;
var
  I: SizeInt;
begin
  for I := 0 to Pred(Count) do
    FBuffer[I] := Default(T);
end;

constructor TGBlockingChannel.Create;
begin
  System.FillChar(FBuffer, SizeOf(FBuffer), 0);
  System.InitCriticalSection(FLock);
  FActive := True;
end;

destructor TGBlockingChannel.Destroy;
begin
  Close;
  Lock;
  try
    CleanupBuffer;
    System.RtlEventDestroy(FWriteAwait);
    FWriteAwait := nil;
    System.RtlEventDestroy(FReadAwait);
    FReadAwait := nil;
    inherited;
  finally
    UnLock;
    System.DoneCriticalSection(FLock);
  end;
end;

procedure TGBlockingChannel.AfterConstruction;
begin
  inherited;
  FWriteAwait := System.RtlEventCreate;
  FReadAwait  := System.RtlEventCreate;
  SignalWrite;
end;

function TGBlockingChannel.Send(constref aValue: T): Boolean;
begin
  System.RtlEventWaitFor(FWriteAwait);
  Lock;
  try
    if Active then
      begin
        Result := CanWrite;
        if Result then
          SendData(aValue);
      end
    else
      begin
        Result := False;
        SignalWrite;
      end;
  finally
    UnLock;
  end;
end;

function TGBlockingChannel.Receive(out aValue: T): Boolean;
begin
  System.RtlEventWaitFor(FReadAwait);
  Lock;
  try
    if Active then
      begin
        Result := CanRead;
        if Result then
          aValue := ReceiveData;
      end
    else
      begin
        Result := False;
        SignalRead;
      end;
  finally
    UnLock;
  end;
end;

procedure TGBlockingChannel.Close;
begin
  Lock;
  try
    if Active then
      begin
        FActive := False;
        SignalRead;
        SignalWrite;
      end;
  finally
    UnLock;
  end;
end;

procedure TGBlockingChannel.Open;
begin
  Lock;
  try
    if not Active then
      begin
        FActive := True;
        if Count > 0 then
          SignalRead;
        if Count < Capacity then
          SignalWrite;
      end;
  finally
    UnLock;
  end;
end;

{ TGObjectBlockingChannel }

procedure TGObjectBlockingChannel.CleanupBuffer;
var
  I: Integer;
begin
  if OwnsObjects then
    for I := 0 to Pred(Count) do
      FBuffer[I].Free;
end;

constructor TGObjectBlockingChannel.Create(aOwnsObjects: Boolean);
begin
  inherited Create;
  FOwnsObjects := aOwnsObjects;
end;

end.

