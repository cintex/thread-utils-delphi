{ *******************************************************

  Threading utilities for Delphi 2009 and above

  Utilidad para programación concurrente.

  *******************************************************

  2012 Ángel Fernández Pineda. Madrid. Spain.

  This work is licensed under the Creative Commons
  Attribution-ShareAlike 3.0 Unported License. To
  view a copy of this license,
  visit http://creativecommons.org/licenses/by-sa/3.0/
  or send a letter to Creative Commons,
  444 Castro Street, Suite 900,
  Mountain View, California, 94041, USA.

  ******************************************************* }

unit ThreadUtils;

{
  SUMMARY:

  - TCustomThread<ArgType>: simplify the writing of thread code,
  avoiding the need to derive a new class.

  - TThreadKillerHelper: introduces Kill(Timeout,ExitCode) to TThread.

  - TParallel: run code in a thread pool (current implementation
  uses "old" Windows pool API)
}

interface

uses
  Classes, SysUtils, System.SyncObjs;

{ TCustomThread<ArgType>

  PURPOUSE:
  To simplify the writing of thread code, avoiding the need to derive a new
  class. Thread code is written as an event handler or as a reference to
  procedure. Custom data of ArgType will be passed at thread initialization.

  GENERAL USAGE:
  - Write an event handler (TNotifyEvent) or a reference to procedure.
  - Create a TCustomThread instance. Custom data may be passed to the newly
  created thread, which may be accessed later thanks to the CustomData
  property.
  - Use it as any other TThread instance.

  THREAD PROPERTIES:
  Protected TThread properties and methods are made public so you can check
  for thread termination or call the synchronize method.

  EVENT HANDLER, example:
  The argument to TNotifyEvent should be typecasted to TCustomThread<ArgType>.

  procedure TMyClass.ThreadBody(Sender: TObject);
  begin
    with Sender as TCustomThread<integer> do
     while (not Terminated) do
     begin
     ...

  procedure TMyClass.Other;
  begin
    thread := TCustomThread<integer>.Create(true,1,ThreadBody);
    ...

  REFERENCE TO PROCEDURE, example:

  thread := TCustomThread<integer>.Create(
    true,1,
    procedure(instance: TCustomThread<integer>)
    begin
      with instance do
        while (not terminated) do
        begin
        ...
       end)
    ...
}

type
  TCustomThread<ArgType> = class(TThread)
  private
    FOnExecute1: TNotifyEvent;
    FOnExecute2: TProc<TCustomThread<ArgType>>;
    FCustomData: ArgType;
  protected
    procedure Execute; override;
  public
    constructor Create(CreateSuspended: boolean; InitialData: ArgType;
      OnExecute: TNotifyEvent); overload;
    constructor Create(CreateSuspended: boolean; InitialData: ArgType;
      OnExecute: TProc < TCustomThread < ArgType >> ); overload;

    procedure Synchronize(AMethod: TThreadMethod); overload;
    procedure Synchronize(AThreadProc: TThreadProcedure); overload;

    property CustomData: ArgType read FCustomData;

    property Terminated;
  end;

  TCustomThread = TCustomThread<TObject>;

  {
    TThreadKillerHelper

    PURPOUSE:
    Introduces a new method to TThread class:
    Kill(Timeout,ExitCode) will signal the thread to terminate,
    then it will wait for Timeout (milliseconds).
    After that, if the thread has not been terminated, it will force its
    destruction using an operating system primitive.

    Note: usage of Kill may lead to resources not been
    properly deallocated.
 }

type
  TThreadKillerHelper = class helper for TThread
  public
    procedure Kill(Timeout: cardinal = 0; ExitCode: cardinal = 9999);
  end;

  {
    TParallel

    PURPOUSE:
    Run some code in a thread pool. There is no need to manage threads.

    USAGE:
    - Call Run to execute some code in another thread.
    - Call ActiveWaitFor to wait termination of parallel code (at Run/RunFor).
    The calling thread will not freeze, so you must provide some code to run
    while waiting.
    - Check WorkCount to know if code is already running.

    EXAMPLE:

    Tparallel.Run(Procedure1);
    Tparallel.Run(Instace.Procedure2);
    Tparallel.Run(procedure begin ... end);
    ...
    Tparallel.ActiveWaitFor(Application.ProcessMessages);

    INDEXED LOOPS:
    Use TParallelFor or TParallel<integer> to parallelize loop iterations
    For example:

    for i := 0 to count-1 do
      TParallelFor.Run(
        procedure (index: integer)
        begin
          // Do something with index
          // DO NOT use "i" as index
        end);

    NOTES:
    - There is no sense in creating TParallel instances.
    - There is a sperate workcount for each TParallel<T> instance.
    Do not call TParallel.ActiveWaitFor instead of TParallel<T>.ActiveWaitFor
 }

{$IFDEF MSWINDOWS}

type
  TParallel = class
  private
    code: TProc;
  private
    class var FWorkCount: cardinal;
    class function ProcFunc(Param: pointer): integer stdcall; static;
  public
    class constructor Create;
    class procedure Run(const code: TProc; const mayRunLong: boolean = true);
    class procedure ActiveWaitFor(const WaitCode: TProc;
      const Interval: cardinal = 150);
    class property WorkCount: cardinal read FWorkCount;
  end;

type
  TParallel<T> = class
  private
    code: TProc<T>;
    data: T;
  private
    class var FWorkCount: cardinal;
    class function ProcFunc(Param: pointer): integer stdcall; static;
    class function GetFlags(const mayRunLong: boolean): cardinal;
  public
    class constructor Create;
    class procedure Run(const arg: T; const code: TProc<T>;
      const mayRunLong: boolean = true);
    class procedure ActiveWaitFor(const WaitCode: TProc;
      const Interval: cardinal = 150);
    class property WorkCount: cardinal read FWorkCount;
  end;

  TParallelFor = TParallel<integer>;
{$ENDIF}
// -----------------------------------------------------------------------------

implementation

{$IFDEF MSWINDOWS}

uses
  Windows;
{$ENDIF}
// -----------------------------------------------------------------------------
// TCustomThread
// -----------------------------------------------------------------------------

constructor TCustomThread<ArgType>.Create(CreateSuspended: boolean;
  InitialData: ArgType; OnExecute: TNotifyEvent);
begin
  FOnExecute1 := OnExecute;
  FOnExecute2 := nil;
  FCustomData := InitialData;
  inherited Create(CreateSuspended);
end;

// -----------------------------------------------------------------------------

constructor TCustomThread<ArgType>.Create(CreateSuspended: boolean;
  InitialData: ArgType; OnExecute: TProc < TCustomThread < ArgType >> );
begin
  FOnExecute1 := nil;
  FOnExecute2 := OnExecute;
  FCustomData := InitialData;
  inherited Create(CreateSuspended);
end;

// -----------------------------------------------------------------------------

procedure TCustomThread<ArgType>.Execute;
begin
  if (Assigned(FOnExecute1)) then
    FOnExecute1(self)
  else if (Assigned(FOnExecute2)) then
    FOnExecute2(self);
end;

// -----------------------------------------------------------------------------

procedure TCustomThread<ArgType>.Synchronize(AMethod: TThreadMethod);
begin
  inherited Synchronize(AMethod);
end;

// -----------------------------------------------------------------------------

procedure TCustomThread<ArgType>.Synchronize(AThreadProc: TThreadProcedure);
begin
  inherited Synchronize(AThreadProc);
end;

// -----------------------------------------------------------------------------
// TThreadKillerHelper
// -----------------------------------------------------------------------------

procedure TThreadKillerHelper.Kill(Timeout: cardinal; ExitCode: cardinal);
begin
  Terminate;
  sleep(Timeout);
  if (not Terminated) then
    TerminateThread(self.Handle, ExitCode);
end;

// -----------------------------------------------------------------------------
// TParallel
// -----------------------------------------------------------------------------

function GetFlags(const mayRunLong: boolean): cardinal;
begin
  if (mayRunLong) then
    Result := WT_EXECUTEDEFAULT or WT_EXECUTELONGFUNCTION
  else
    Result := WT_EXECUTEDEFAULT;
end;

// -----------------------------------------------------------------------------

class constructor TParallel.Create;
begin
  TParallel.FWorkCount := 0;
end;

// -----------------------------------------------------------------------------

class function TParallel.ProcFunc(Param: pointer): integer stdcall;
var
  Item: TParallel;
begin
  Item := TParallel(Param);
  Result := -1;
  try
    Item.code.Invoke;
  finally
    Item.Free;
    dec(TParallel.FWorkCount);
  end;
end;

// -----------------------------------------------------------------------------

class procedure TParallel.Run(const code: TProc; const mayRunLong: boolean);
var
  Item: TParallel;
begin
  if not Assigned(code) then
    Exit;
  Item := TParallel.Create;
  Item.code := code;
  inc(TParallel.FWorkCount);
  if (not QueueUserWorkItem(ProcFunc, pointer(Item), GetFlags(mayRunLong))) then
  begin
    dec(TParallel.FWorkCount);
    Item.Free;
    raise Exception.Create('Unable to queue work');
  end;
end;

// -----------------------------------------------------------------------------

class procedure TParallel.ActiveWaitFor(const WaitCode: TProc;
  const Interval: cardinal);
begin
  while (TParallel.FWorkCount > 0) do
  begin
    sleep(Interval);
    if (Assigned(WaitCode)) then
      WaitCode;
  end;
end;

{$IFDEF MSWINDOWS}
// -----------------------------------------------------------------------------
// TParallel<T>
// -----------------------------------------------------------------------------

class constructor TParallel<T>.Create;
begin
  TParallel<T>.FWorkCount := 0;
end;

// -----------------------------------------------------------------------------

class function TParallel<T>.GetFlags(const mayRunLong: boolean): cardinal;
begin
  if (mayRunLong) then
    Result := WT_EXECUTEDEFAULT or WT_EXECUTELONGFUNCTION
  else
    Result := WT_EXECUTEDEFAULT;
end;

// -----------------------------------------------------------------------------

class function TParallel<T>.ProcFunc(Param: pointer): integer stdcall;
var
  Item: TParallel<T>;
begin
  Item := TParallel<T>(Param);
  Result := -1;
  try
    Item.code(Item.data);
  finally
    Item.Free;
    dec(TParallel<T>.FWorkCount);
  end;
end;

// -----------------------------------------------------------------------------

class procedure TParallel<T>.Run(const arg: T; const code: TProc<T>;
  const mayRunLong: boolean = true);
var
  Item: TParallel<T>;
begin
  if not Assigned(code) then
    Exit;
  Item := TParallel<T>.Create;
  Item.code := code;
  Item.data := arg;
  inc(TParallel<T>.FWorkCount);
  if (not QueueUserWorkItem(ProcFunc, pointer(Item), GetFlags(mayRunLong))) then
  begin
    dec(TParallel<T>.FWorkCount);
    Item.Free;
    raise Exception.Create('Unable to queue work');
  end;
end;

// -----------------------------------------------------------------------------

class procedure TParallel<T>.ActiveWaitFor(const WaitCode: TProc;
  const Interval: cardinal);
begin
  while (TParallel<T>.FWorkCount > 0) do
  begin
    sleep(Interval);
    if (Assigned(WaitCode)) then
      WaitCode;
  end;
end;

// -----------------------------------------------------------------------------

{$ENDIF}

end.
