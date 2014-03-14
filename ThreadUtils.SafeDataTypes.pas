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
unit ThreadUtils.SafeDataTypes;

{
  SUMMARY:

  - TThreadSafeQueue: Thread-safe implementation of first-in last-out queue.

  - TThreadSafeStack: Thread-safe implementation of first-in first-out stack.
}

interface

uses
  SyncObjs, ThreadUtils.Sync, System.Generics.Collections;

{ TThreadSafeQueue

  PURPOUSE:
  Thread-safe implementation of first-in last-out queue.

  ENQUEUE:
  Non-blocking primitive. Stores an item into the queue, so it becomes
  non empty.

  DEQUEUE:
  Calling thread is locked while queue is empty, then an item is extracted
  from the queue and returned. Threads are also unlocked by raising an
  EAbandoned exception, which happens at instance destruction.

  WAITFOR:
  Calling thread is locked until queue is empty or an EAbandoned exception
  is raised at instance destruction.

  EXAMPLE:

  while not Terminated do
  try
    item := q.dequeue(item)
    <<do something with item>>
  except
    on EAbandoned do Terminate;
  end;
}

type
  TThreadSafeQueue<T> = class
  private type
    TQueueNode = class
      Next: TQueueNode;
      Item: T;
    end;
  private
    FHead: TQueueNode;
    FTail: TQueueNode;
    CSTail: TCriticalSection;
    CSHead: TCriticalSection;
    NotEmpty: TPrecondition;
    Empty: TPrecondition;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure Enqueue(Item: T);
    function Dequeue: T;
    function isEmpty: boolean;
    function WaitFor(Timeout: cardinal = INFINITE): TWaitResult;
  end;

{ TThreadSafeStack

  PURPOUSE:
  Thread-safe implementation of first-in first-out stack.

  PUSH:
  Non-blocking primitive. Stores an item at the top of the queue,
  so it becomes non empty.

  POP:
  Calling thread is locked while stack is empty, then, top item is extracted
  from the stack and returned. Threads are also unlocked by raising an
  EAbandoned exception, which happens at instance destruction.

  WAITFOR:
  Calling thread is locked until queue is empty or an EAbandoned exception
  is raised at instance destruction.

  EXAMPLE:

  while not Terminated do
  try
    item := stack.pop(item)
    <<do something with item>>
  except
    on EAbandoned do Terminate;
  end;
}

type
  TThreadSafeStack<T> = class
  private
    FImpl: TQueue<T>;
    CS: TCriticalSection;
    NotEmpty: TPrecondition;
    Empty: TPrecondition;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure Push(Item: T);
    function Pop: T;
    function isEmpty: boolean;
    function WaitFor(Timeout: cardinal = INFINITE): TWaitResult;
  end;

type
  EAbandoned = class(ESyncObjectException);

implementation

// -----------------------------------------------------------------------------
// TThreadSafeQueue
// -----------------------------------------------------------------------------

constructor TThreadSafeQueue<T>.Create;
begin
  FHead := TQueueNode.Create;
  FHead.Next := nil;
  FTail := FHead;
  CSHead := TCriticalSection.Create;
  CSTail := TCriticalSection.Create;
  NotEmpty := TPrecondition.Create(
    function: boolean
    begin
      Result := (FHead.Next <> nil)
    end);
  Empty := TPrecondition.Create(
    function: boolean
    begin
      Result := (FHead.Next = nil);
    end);
end;

// -----------------------------------------------------------------------------

function TThreadSafeQueue<T>.isEmpty: boolean;
begin
  Result := (FHead.Next = nil);
end;

// -----------------------------------------------------------------------------

destructor TThreadSafeQueue<T>.Destroy;
begin
  NotEmpty.Cancel;
  Empty.Cancel;
  Clear;
  CSHead.Free;
  CSTail.Free;
  FHead.Free;
  NotEmpty.Free;
  Empty.Free;
  inherited;
end;

// -----------------------------------------------------------------------------

procedure TThreadSafeQueue<T>.Enqueue(Item: T);
var
  NewNode: TQueueNode;
begin
  NewNode := TQueueNode.Create;
  NewNode.Item := Item;
  NewNode.Next := nil;
  CSTail.Enter;
  try
    FTail.Next := NewNode;
    FTail := NewNode;
  finally
    CSTail.Leave;
  end;
  Empty.Update;
  NotEmpty.Update;
end;

// -----------------------------------------------------------------------------

function TThreadSafeQueue<T>.Dequeue: T;
var
  OldNode: TQueueNode;
begin
  OldNode := nil;
  CSHead.Enter;
  try
    if (NotEmpty.WaitFor(CSHead) = wrSignaled) then
    begin
      OldNode := FHead;
      FHead := FHead.Next;
      Result := FHead.Item;
    end;
  finally
    CSHead.Leave;
  end;
  if (OldNode <> nil) then
  begin
    OldNode.Free;
    NotEmpty.Update;
    Empty.Update;
  end
  else
    raise EAbandoned.Create('TThreadSafeQueue was abandoned');
end;

// -----------------------------------------------------------------------------

procedure TThreadSafeQueue<T>.Clear;
var
  oldHead: TQueueNode;
begin
  CSHead.Enter;
  try
    while (FHead.Next <> nil) do
    begin
      oldHead := FHead;
      FHead := FHead.Next;
      oldHead.Free;
    end;
  finally
    CSHead.Leave;
  end;
  NotEmpty.Update;
  Empty.Update;
end;

// -----------------------------------------------------------------------------

function TThreadSafeQueue<T>.WaitFor(Timeout: cardinal = INFINITE): TWaitResult;
begin
  CSHead.Enter;
  try
    Result := Empty.WaitFor(CSHead, Timeout);
    if (Result <> wrSignaled) and (Result <> wrTimeout) then
      raise EAbandoned.Create('TThreadSafeQueue was abandoned');
  finally
    CSHead.Leave;
  end;
end;

// -----------------------------------------------------------------------------
// TThreadSafeStack
// -----------------------------------------------------------------------------

constructor TThreadSafeStack<T>.Create;
begin
  CS := TCriticalSection.Create;
  FImpl := TQueue<T>.Create;
  NotEmpty := TPrecondition.Create(
    function: boolean
    begin
      Result := (FImpl.Count > 0)
    end);
  Empty := TPrecondition.Create(
    function: boolean
    begin
      Result := (FImpl.Count = 0);
    end);
end;

// -----------------------------------------------------------------------------

function TThreadSafeStack<T>.isEmpty: boolean;
begin
  CS.Enter;
  Result := (FImpl.Count = 0);
  CS.Leave;
end;

// -----------------------------------------------------------------------------

destructor TThreadSafeStack<T>.Destroy;
begin
  NotEmpty.Cancel;
  Empty.Cancel;
  FImpl.Free;
  CS.Free;
  NotEmpty.Free;
  Empty.Free;
  inherited;
end;

// -----------------------------------------------------------------------------

procedure TThreadSafeStack<T>.Push(Item: T);
begin
  CS.Enter;
  try
    FImpl.Enqueue(Item);
  finally
    CS.Leave;
  end;
  Empty.Update;
  NotEmpty.Update;
end;

// -----------------------------------------------------------------------------

function TThreadSafeStack<T>.Pop: T;
var
  wr: TWaitResult;
begin
  CS.Enter;
  try
    wr := NotEmpty.WaitFor(CS);
    if (wr = wrSignaled) then
      Result := FImpl.Dequeue
  finally
    CS.Leave;
  end;
  if (wr = wrSignaled) then
  begin
    NotEmpty.Update;
    Empty.Update;
  end
  else
    raise EAbandoned.Create('TThreadSafeStack was abandoned');
end;

// -----------------------------------------------------------------------------

procedure TThreadSafeStack<T>.Clear;
begin
  CS.Enter;
  try
    FImpl.Clear;
  finally
    CS.Leave;
  end;
  NotEmpty.Update;
  Empty.Update;
end;

// -----------------------------------------------------------------------------

function TThreadSafeStack<T>.WaitFor(Timeout: cardinal = INFINITE): TWaitResult;
begin
  CS.Enter;
  try
    Result := Empty.WaitFor(CS, Timeout);
    if (Result <> wrSignaled) and (Result <> wrTimeout) then
      raise EAbandoned.Create('TThreadSafeStack was abandoned');
  finally
    CS.Leave;
  end;
end;

// -----------------------------------------------------------------------------

end.
