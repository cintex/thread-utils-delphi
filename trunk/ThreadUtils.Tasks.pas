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

unit ThreadUtils.Tasks;

{
  SUMMARY:

  - TTaskQueue<ArgType>: have an internal pool of threads work on
  custom data stored in a queue.

}

interface

uses
  Classes, SysUtils, System.SyncObjs, ThreadUtils.Sync,
  ThreadUtils.SafeDataTypes;

  { TTaskQueue<ArgType>

    PURPOUSE:
    To have an internal pool of threads work on custom data stored in a queue.

    GENERAL USAGE:
    - Write a TThreadedTask event handler to set up the code which will
    process queue items.
    - Create a TTaskQueue instance and pass the event handler to it.
    - Add items to the TTaskQueue instance. As long as there are items
    in the queue, they will be eventually processed in a separate thread.

    WAITING FOR THE QUEUE TO BE PROCESSED:
    Call TTaskQueue.WaitFor to lock the calling process until all items in the
    queue has been processed. DO NOT CALL WaitFor from the main thread, unless
    you ensure Windows messages will be processed. For Example:

    while (q.WaitFor(150)=wrTimeout) do Application.ProcessMessages;

    CANCELLING CURRENT TASKS:
    Call TTaskQueue.Clear to prevent pool threads from getting more work,
    but busy threads at the time will continue their work.

    EXAMPLE:

    procedure TMyClass.ProcessQueueItem(Item: integer)
    begin
      // do something with Item
      // New items may be enqueued, too
    end;

    procedure TMyClass.Other;
    begin
      ...
      queue := TTaskQueue<integer>.Create(ProcessQueueItem,5);
      for i := 0 to Count-1 do
        queue.Add(i);
      queue.WaitFor;

 }

type
  TThreadedTask<ArgType> = procedure(CustomData: ArgType) of object;
  TThreadedTask = TThreadedTask<TObject>;

{$WARN SYMBOL_PLATFORM OFF}

type
  TTaskQueue<ArgType> = class
  private
    Queue: TThreadSafeQueue<ArgType>;
    FOnWork: TThreadedTask<ArgType>;
    procedure ThreadExecute(Sender: TObject);
  public
{$IFDEF MSWINDOWS}
    constructor Create(OnWork: TThreadedTask<ArgType>; NumWorkers: cardinal = 2;
      Priority: TThreadPriority = tpNormal); overload;
{$ELSE}
    constructor Create(OnWork: TThreadedTask<ArgType>;
      NumWorkers: cardinal = 2); overload;
{$ENDIF}
    destructor Destroy; override;
    procedure Add(CustomData: ArgType);
    procedure Clear;
    function WaitFor(Timeout: cardinal = INFINITE): TWaitResult;

    property OnWork: TThreadedTask<ArgType> read FOnWork;
  end;

  TTaskQueue = TTaskQueue<TObject>;

// -----------------------------------------------------------------------------

implementation

uses
  ThreadUtils;

// -----------------------------------------------------------------------------
// TTaskQueue
// ------------------------------------------------------------------------------

{$IFDEF MSWINDOWS}

constructor TTaskQueue<ArgType>.Create(OnWork: TThreadedTask<ArgType>;
  NumWorkers: cardinal; Priority: TThreadPriority);
{$ELSE}

constructor TTaskQueue<ArgType>.Create(OnWork: TThreadedTask<ArgType>;
  NumWorkers: cardinal);
{$ENDIF}
var
  i: cardinal;
  Worker: TCustomThread<TObject>;
begin
  if (not Assigned(OnWork)) then
    raise EArgumentException.Create('TTaskQueue: OnWork cant be null');
  FOnWork := OnWork;
  Queue := TThreadSafeQueue<ArgType>.Create;
  for i := 1 to NumWorkers do
  begin
    Worker := TCustomThread<TObject>.Create(true, nil, ThreadExecute);
    Worker.FreeOnTerminate := true;
{$IFDEF MSWINDOWS}
    Worker.Priority := Priority;
{$ENDIF}
    Worker.Start;
  end;
end;

// -----------------------------------------------------------------------------

destructor TTaskQueue<ArgType>.Destroy;
begin
  Queue.Clear;
  Queue.WaitFor;
  Queue.Free;
  inherited;
end;

// -----------------------------------------------------------------------------

procedure TTaskQueue<ArgType>.ThreadExecute(Sender: TObject);
var
  data: ArgType;
begin
  with (Sender as TCustomThread<TObject>) do
  begin
    while (not Terminated) do
      try
        data := self.Queue.Dequeue;
        try
          FOnWork(data);
        except
        end;
      except
        on EAbandoned do
          Terminate;
      end;
  end;
end;

// -----------------------------------------------------------------------------

procedure TTaskQueue<ArgType>.Add(CustomData: ArgType);
begin
  Queue.Enqueue(CustomData);
end;

// -----------------------------------------------------------------------------

procedure TTaskQueue<ArgType>.Clear;
begin
  Queue.Clear;
end;

// -----------------------------------------------------------------------------

function TTaskQueue<ArgType>.WaitFor(Timeout: cardinal): TWaitResult;
begin
  Result := Queue.WaitFor(Timeout);
end;

// -----------------------------------------------------------------------------

end.
