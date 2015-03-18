Threading utilities for Delphi 2009 and above. Provides the following classes:

TCustomThread: simplify the writing of thread code, avoiding the need to derive a new class. Thread code is written as an event handler or as a reference to procedure.

ThreadKillerHelper: introduces Kill(Timeout,ExitCode) to TThread.

TParallel: run code in a thread pool (current implementation uses "old" Windows pool API). Windows only.

TThreadSafeQueue and TThreadSafeStack: Efficient and thread-safe implementation queue/stack. With graceful object destruction.

THandleObjectHelper: new primitive MsgWaitFor.

TEvent2: extended behavior for TEvent.

TLockCondition and TPrecondition: lock thread execution until some condition is met. No need for loops. Very useful.

TSyncPoint: wait until a number of threads have reached an execution point

TTaskQueue: have an internal pool of threads work on custom data stored in a queue.