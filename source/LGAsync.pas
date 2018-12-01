{****************************************************************************
*                                                                           *
*   This file is part of the LGenerics package.                             *
*   Brief and dirty futures implementation.                                 *
*                                                                           *
*   Copyright(c) 2018 A.Koverdyaev(avk)                                     *
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
  LGDeque,
  LGVector,
  LGFunction,
  LGStrConst;

type

  TAbstractAsyncTask = class abstract
  strict private
    FException: Exception;
    FExecAwait: PRtlEvent;
    FCancelled: Boolean;
    FStarted: Boolean;
  private
    procedure Cancel; inline;
    function  GetCancelled: Boolean; inline;
    function  GetStarted: Boolean; inline;
  strict protected
    procedure DoExecute; virtual; abstract;
  public
    destructor Destroy; override;
    procedure AfterConstruction; override;
    procedure Execute;
    procedure WaitFor;
    property  Started: Boolean read GetStarted;
    property  Cancelled: Boolean read GetCancelled;
    property  FatalException: Exception read FException;
  end;

  generic TGAsyncTask<T> = class abstract(TAbstractAsyncTask)
  strict protected
    FResult: T; //To be setted inside overriden DoExecute
  public
    property Result: T read FResult;
  end;

  TFutureState = (fsPending, fsCancelled, fsResolveError, fsResolved);

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
    procedure Start(aTask: TTask);  inline;
    class operator Finalize(var f: TGFuture); inline;
  public
    function  WaitFor: TFutureState; inline;
  { may be impossible, if task already started }
    function  Cancel: Boolean;
  { will raise exception if resolving failed }
    function  Value: T;
    function  GetValue: TOptional; inline;
    property  State: TFutureState read FState;
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
    class function Call(aProc: TProcedure): TFuture; static;
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
    class function Call(aTask: IExecutable): TFuture; static;
    constructor Create(aTask: IExecutable);
  end;

  { TGAsyncCollable incapsulates IGCallable}
  generic TGAsyncCollable<T> = class(specialize TGAsyncTask<T>)
  public
  type
    ICallable = specialize IGCallable<T>;
    TFuture   = specialize TGFuture<T>;

  strict private
    FTask: ICallable;
  strict protected
    procedure DoExecute; override;
  public
    class function Call(aTask: ICallable): TFuture; static;
    constructor Create(aTask: ICallable);
  end;

  { TGAsyncMethod incapsulates method without arguments(which returns T) }
  generic TGAsyncMethod<T> = class(specialize TGAsyncTask<T>)
  public
  type
    TFunction = function: T of object;
    TFuture   = specialize TGFuture<T>;

  strict private
    FFunc: TFunction;
  strict protected
    procedure DoExecute; override;
  public
    class function Call(aFunc: TFunction): TFuture; static;
    constructor Create(aFunc: TFunction);
  end;

  { TGAsyncNestedFunc incapsulates nested niladic function (without arguments) }
  generic TGAsyncNestedFunc<T> = class(specialize TGAsyncTask<T>)
  public
  type
    TFunction = function: T is nested;
    TFuture   = specialize TGFuture<T>;

  strict private
    FFunc: TFunction;
  strict protected
    procedure DoExecute; override;
  public
    class function Call(aFunc: TFunction): TFuture; static;
    constructor Create(aFunc: TFunction);
  end;

  { TGAsyncNAFunc incapsulates regular niladic function (without arguments) }
  generic TGAsyncNAFunc<T> = class(specialize TGAsyncTask<T>)
  strict protected
  public
  type
    TFunction = function: T;
    TFuture   = specialize TGFuture<T>;

  strict private
    FFunc: TFunction;
  strict protected
    procedure DoExecute; override;
  public
    class function Call(aFunc: TFunction): TFuture; static;
    constructor Create(aFunc: TFunction);
  end;

  { TGAsyncFunc incapsulates regular monadic function (with one argument) }
  generic TGAsyncFunc<T, TResult> = class(specialize TGAsyncTask<TResult>)
  strict protected
  type
    TCall = specialize TGDefferedCall<T, TResult>;

  public
  type
    TFunction = TCall.TFunction;
    TFuture   = specialize TGFuture<TResult>;

  strict private
    FCall: TCall;
  strict protected
    procedure DoExecute; override;
  public
    class function Call(aFunc: TFunction; constref v: T): TFuture; static;
    constructor Create(aFunc: TFunction; constref v: T);
  end;

  { TGAsync2AFunc incapsulates regular dyadic function (with two arguments) }
  generic TGAsync2AFunc<T1, T2, TResult> = class(specialize TGAsyncTask<TResult>)
  strict protected
  type
    TCall = specialize TGDefferedCall2A<T1, T2, TResult>;

  public
  type
    TFunction = TCall.TFunction;
    TFuture   = specialize TGFuture<TResult>;

  strict private
    FCall: TCall;
  strict protected
    procedure DoExecute; override;
  public
    class function Call(aFunc: TFunction; constref v1: T1; constref v2: T2): TFuture; static;
    constructor Create(aFunc: TFunction; constref v1: T1; constref v2: T2);
  end;

  { TGAsync3AFunc incapsulates regular triadic function (with three arguments) }
  generic TGAsync3AFunc<T1, T2, T3, TResult> = class(specialize TGAsyncTask<TResult>)
  strict protected
  type
    TCall = specialize TGDefferedCall3A<T1, T2, T3, TResult>;

  public
  type
    TFunction = TCall.TFunction;
    TFuture   = specialize TGFuture<TResult>;

  strict private
    FCall: TCall;
  strict protected
    procedure DoExecute; override;
  public
    class function Call(aFunc: TFunction; constref v1: T1; constref v2: T2; constref v3: T3): TFuture; static;
    constructor Create(aFunc: TFunction; constref v1: T1; constref v2: T2; constref v3: T3);
  end;

  { TTaskExecutor executes futures in its own thread pool.
    Enqueue procedure is threadsafe, so futures may use other futures.
    Resizing of thread pool is not threadsafe, so MUST be done from main thread. }
  TTaskExecutor = class
  protected
  type

    TTaskQueue = class
    strict private
    type
      TQueue = specialize TGLiteDeque<TAbstractAsyncTask>;

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
      procedure Enqueue(aTask: TAbstractAsyncTask);
      function  Dequeue(out aTask: TAbstractAsyncTask): Boolean;
      function  Cancel(aTask: TAbstractAsyncTask): Boolean;
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
    procedure EnqueueTask(aTask: TAbstractAsyncTask); inline;
    procedure TerminatePool;
    procedure FinalizePool; inline;
    function  CancelTask(aTask: TAbstractAsyncTask): Boolean; inline;
    class constructor InitNil;
    class destructor  DoneQueue;
    class function    GetThreadCount: Integer; static; inline;
    class procedure   SetThreadCount(aValue: Integer); static; inline;
  class var
    CFExecutor: TTaskExecutor; // CF -> Class Field

  public
  const
    DEFAULT_THREAD_COUNT = 4;

    class procedure EnsureThreadCount(aValue: Integer); static;
    class procedure Enqueue(aTask: TAbstractAsyncTask); static; inline;
    class function  Cancel(aTask: TAbstractAsyncTask): Boolean; inline;
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

{ TAbstractAsyncTask }

procedure TAbstractAsyncTask.Cancel;
begin
  FCancelled := True;
end;

function TAbstractAsyncTask.GetCancelled: Boolean;
begin
  WriteBarrier;
  Result := FCancelled;
end;

function TAbstractAsyncTask.GetStarted: Boolean;
begin
  WriteBarrier;
  Result := FStarted;
end;

destructor TAbstractAsyncTask.Destroy;
begin
  System.RtlEventDestroy(FExecAwait);
  FExecAwait := nil;
  inherited;
end;

procedure TAbstractAsyncTask.AfterConstruction;
begin
  inherited;
  FExecAwait := System.RtlEventCreate;
end;

procedure TAbstractAsyncTask.Execute;
begin
  if not Cancelled then
    begin
      FStarted := True;
      try
        DoExecute;
      except
        on e: Exception do
           FException := Exception(System.AcquireExceptionObject);
      end;
    end;
  System.RtlEventSetEvent(FExecAwait);
end;

procedure TAbstractAsyncTask.WaitFor;
begin
  if Cancelled then
    exit;
  System.RtlEventWaitFor(FExecAwait);
end;

{ TGFuture }

procedure TGFuture.Resolve;
var
  e: Exception;
begin
  if Assigned(FTask) then
    try
      FTask.WaitFor;
      if not FTask.Cancelled then
        begin
          e := FTask.FatalException;
          if Assigned(e) then
            FState := fsResolveError
          else
            begin
              FState := fsResolved;
              FTaskResult := FTask.Result;
            end;
        end
      else
        FState := fsCancelled;
    finally
      FreeAndNil(FTask);
      if Assigned(e) then
        raise e;
    end;
end;

procedure TGFuture.Start(aTask: TTask);
begin
  FTask := aTask;
  FState := fsPending;
  TTaskExecutor.Enqueue(FTask);
end;

class operator TGFuture.Finalize(var f: TGFuture);
begin
  f.WaitFor;
end;

function TGFuture.WaitFor: TFutureState;
begin
  try Resolve except end;
  Result := State;
end;

function TGFuture.Cancel: Boolean;
begin
  if (State = fsPending) and not FTask.Started then
    begin
      Result := TTaskExecutor.Cancel(FTask);
      if Result then
        begin
          FreeAndNil(FTask);
          FState := fsCancelled;
        end;
    end
  else
    Result := False;
end;

function TGFuture.Value: T;
begin
  case State of
    fsPending:      Resolve;
    fsResolveError,
    fsCancelled:    raise ELGFuture.Create(SEResultUnknown);
  end;
  Result := FTaskResult;
end;

function TGFuture.GetValue: TOptional;
begin
  if WaitFor = fsResolved then
    Result.Assign(FTaskResult);
end;

{ TGAsyncProc }

procedure TGAsyncProc.DoExecute;
begin
  FProc;
  FResult := True;
end;

class function TGAsyncProc.Call(aProc: TProcedure): TFuture;
begin
  Result{%H-}.Start(TGAsyncProc.Create(aProc));
end;

constructor TGAsyncProc.Create(aProc: TProcedure);
begin
  FProc := aProc;
end;

{ TAsyncExecutable }

procedure TAsyncExecutable.DoExecute;
begin
  FTask.Execute;
  FResult := True;
end;

class function TAsyncExecutable.Call(aTask: IExecutable): TFuture;
begin
  Result{%H-}.Start(TAsyncExecutable.Create(aTask));
end;

constructor TAsyncExecutable.Create(aTask: IExecutable);
begin
  FTask := aTask;
end;

{ TGAsyncCollable }

procedure TGAsyncCollable.DoExecute;
begin
  FResult := FTask.Call;
end;

class function TGAsyncCollable.Call(aTask: ICallable): TFuture;
begin
  Result.Start(TGAsyncCollable.Create(aTask));
end;

constructor TGAsyncCollable.Create(aTask: ICallable);
begin
  FTask := aTask;
end;

{ TGAsyncMethod }

procedure TGAsyncMethod.DoExecute;
begin
  FResult := FFunc();
end;

class function TGAsyncMethod.Call(aFunc: TFunction): TFuture;
begin
  Result.Start(TGAsyncMethod.Create(aFunc));
end;

constructor TGAsyncMethod.Create(aFunc: TFunction);
begin
  FFunc := aFunc;
end;

{ TGAsyncNestedFunc }

procedure TGAsyncNestedFunc.DoExecute;
begin
  FResult := FFunc();
end;

class function TGAsyncNestedFunc.Call(aFunc: TFunction): TFuture;
begin
  Result.Start(TGAsyncNestedFunc.Create(aFunc));
end;

constructor TGAsyncNestedFunc.Create(aFunc: TFunction);
begin
  FFunc := aFunc;
end;

{ TGAsyncNAFunc }

procedure TGAsyncNAFunc.DoExecute;
begin
  FResult := FFunc();
end;

class function TGAsyncNAFunc.Call(aFunc: TFunction): TFuture;
begin
  Result.Start(TGAsyncNAFunc.Create(aFunc));
end;

constructor TGAsyncNAFunc.Create(aFunc: TFunction);
begin
  FFunc := aFunc;
end;

{ TGAsyncFunc }

procedure TGAsyncFunc.DoExecute;
begin
  FResult := FCall.Call;
end;

class function TGAsyncFunc.Call(aFunc: TFunction; constref v: T): TFuture;
begin
  Result.Start(TGAsyncFunc.Create(aFunc, v));
end;

constructor TGAsyncFunc.Create(aFunc: TFunction; constref v: T);
begin
  FCall := TCall.Construct(aFunc, v);
end;

{ TGAsync2AFunc }

procedure TGAsync2AFunc.DoExecute;
begin
  FResult := FCall.Call;
end;

class function TGAsync2AFunc.Call(aFunc: TFunction; constref v1: T1; constref v2: T2): TFuture;
begin
  Result.Start(TGAsync2AFunc.Create(aFunc, v1, v2));
end;

constructor TGAsync2AFunc.Create(aFunc: TFunction; constref v1: T1; constref v2: T2);
begin
  FCall := TCall.Construct(aFunc, v1, v2);
end;

{ TGAsync3AFunc }

procedure TGAsync3AFunc.DoExecute;
begin
  FResult := FCall.Call;
end;

class function TGAsync3AFunc.Call(aFunc: TFunction; constref v1: T1; constref v2: T2;
  constref v3: T3): TFuture;
begin
  Result.Start(TGAsync3AFunc.Create(aFunc, v1, v2, v3));
end;

constructor TGAsync3AFunc.Create(aFunc: TFunction; constref v1: T1; constref v2: T2; constref v3: T3);
begin
  FCall := TCall.Construct(aFunc, v1, v2, v3);
end;

{ TTaskExecutor.TTaskQueue }

procedure TTaskExecutor.TTaskQueue.Lock;
begin
  System.EnterCriticalSection(FLock);
end;

procedure TTaskExecutor.TTaskQueue.UnLock;
begin
  System.LeaveCriticalSection(FLock);
end;

procedure TTaskExecutor.TTaskQueue.Signaled;
begin
  System.RtlEventSetEvent(FReadAwait);
end;

constructor TTaskExecutor.TTaskQueue.Create;
begin
  System.InitCriticalSection(FLock);
end;

destructor TTaskExecutor.TTaskQueue.Destroy;
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

procedure TTaskExecutor.TTaskQueue.AfterConstruction;
begin
  inherited;
  FReadAwait := System.RtlEventCreate;
end;

procedure TTaskExecutor.TTaskQueue.Clear;
var
  Task: TAbstractAsyncTask;
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

procedure TTaskExecutor.TTaskQueue.Close;
begin
  Lock;
  try
    FClosed := True;
    Signaled;
  finally
    UnLock;
  end;
end;

procedure TTaskExecutor.TTaskQueue.Open;
begin
  Lock;
  try
    FClosed := False;
  finally
    UnLock;
  end;
end;

procedure TTaskExecutor.TTaskQueue.Enqueue(aTask: TAbstractAsyncTask);
begin
  Lock;
  try
    FQueue.PushLast(aTask);
    Signaled;
  finally
    UnLock;
  end;
end;

function TTaskExecutor.TTaskQueue.Dequeue(out aTask: TAbstractAsyncTask): Boolean;
begin
  System.RtlEventWaitFor(FReadAwait);
  Lock;
  try
    if not Closed then
      begin
        Result := FQueue.TryPopFirst(aTask);
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

function TTaskExecutor.TTaskQueue.Cancel(aTask: TAbstractAsyncTask): Boolean;
var
  I: Integer;
begin
  Lock;
  try
    Result := False;
    for I := 0 to Pred(FQueue.Count) do
      if FQueue[I] = aTask then
        begin
          aTask.Cancel;
          FQueue.Delete(I);
          Result := True;
          break;
        end;
    Signaled; // ???
  finally
    UnLock;
  end;
end;

{ TTaskExecutor.TWorkThread }

constructor TTaskExecutor.TWorkThread.Create(aQueue: TTaskQueue);
begin
  inherited Create(True);
  FQueue := aQueue;
end;

procedure TTaskExecutor.TWorkThread.Execute;
var
  CurrTask: TAbstractAsyncTask;
begin
  while not Terminated do
    if FQueue.Dequeue(CurrTask) then
      CurrTask.Execute;
end;

{ TTaskExecutor }

function TTaskExecutor.ThreadPoolCount: Integer;
begin
  Result := FThreadPool.Count;
end;

function TTaskExecutor.AddThread: TWorkThread;
begin
  Result := TWorkThread.Create(FTaskQueue);
  FThreadPool.Add(Result);
  Result.Start;
end;

procedure TTaskExecutor.PoolGrow(aValue: Integer);
begin
  while FThreadPool.Count < aValue do
    AddThread;
end;

procedure TTaskExecutor.PoolShrink(aValue: Integer);
begin
  if aValue < 1 then
    aValue := 1;
  TerminatePool;
  FTaskQueue.Open;
  PoolGrow(aValue);
end;

procedure TTaskExecutor.EnqueueTask(aTask: TAbstractAsyncTask);
begin
  FTaskQueue.Enqueue(aTask);
end;

procedure TTaskExecutor.TerminatePool;
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

procedure TTaskExecutor.FinalizePool;
begin
  TerminatePool;
  FThreadPool.Clear;
end;

function TTaskExecutor.CancelTask(aTask: TAbstractAsyncTask): Boolean;
begin
  Result := FTaskQueue.Cancel(aTask);
end;

class constructor TTaskExecutor.InitNil;
begin
  CFExecutor := nil;
end;

class destructor TTaskExecutor.DoneQueue;
begin
  FreeAndNil(CFExecutor);
end;

class function TTaskExecutor.GetThreadCount: Integer; static;
begin
  if Assigned(CFExecutor) then
    Result := CFExecutor.ThreadPoolCount
  else
    Result := 0;
end;

class procedure TTaskExecutor.SetThreadCount(aValue: Integer);
begin
  if aValue > ThreadCount then
    EnsureThreadCount(aValue)
  else
    if Assigned(CFExecutor) then
      CFExecutor.PoolShrink(aValue);
end;

class procedure TTaskExecutor.EnsureThreadCount(aValue: Integer);
begin
  if aValue > ThreadCount then
    if not Assigned(CFExecutor) then
      CFExecutor := TTaskExecutor.Create(aValue)
    else
      CFExecutor.PoolGrow(aValue);
end;

class procedure TTaskExecutor.Enqueue(aTask: TAbstractAsyncTask);
begin
  if not Assigned(CFExecutor) then
    CFExecutor := TTaskExecutor.Create;
  CFExecutor.EnqueueTask(aTask);
end;

class function TTaskExecutor.Cancel(aTask: TAbstractAsyncTask): Boolean;
begin
  if Assigned(CFExecutor) then
    Result := CFExecutor.CancelTask(aTask)
  else
    Result := False;
end;

constructor TTaskExecutor.Create;
begin
  if TThread.ProcessorCount > DEFAULT_THREAD_COUNT then
    Create(TThread.ProcessorCount)
  else
    Create(DEFAULT_THREAD_COUNT);
end;

constructor TTaskExecutor.Create(aThreadCount: Integer);
begin
  FTaskQueue := TTaskQueue.Create;
  if aThreadCount > 0 then
    PoolGrow(aThreadCount)
  else
    PoolGrow(1);
end;

destructor TTaskExecutor.Destroy;
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

