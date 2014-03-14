unit TaskQueueExample_main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ThreadUtils.Tasks, StdCtrls;

type
  TForm1 = class(TForm)
    Memo_main: TMemo;
    Button1: TButton;
    Button2: TButton;
    Btn_WaitFor: TButton;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Btn_WaitForClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    q: TTaskQueue<cardinal>;
    counter: cardinal;
    procedure Task(data: cardinal);
  end;

var
  Form1: TForm1;

implementation
uses
  SyncObjs;

{$R *.dfm}

procedure TForm1.Btn_WaitForClick(Sender: TObject);
begin
  while (q.WaitFor(150)=wrTimeout) do Application.ProcessMessages;
  Memo_main.Lines.Add('Queue is empty');
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  inc(counter);
  q.Add(counter);
  Memo_main.Lines.Add('Adding task ' + IntToStr(counter));
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  q.Clear;
  Memo_main.Lines.Add('Cancelling after task ' + IntToStr(counter));
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  q := TTaskQueue<cardinal>.Create(Task);
  Memo_main.Lines.Clear;
  counter := 0;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  q.Clear;
  q.WaitFor;
  q.Free;
end;

procedure TForm1.Task(data: cardinal);
var
  r: integer;
begin
  r := Random(5000);
  sleep(r);
  Memo_main.Lines.Add(Format('Task %d executed in %d msecs.', [data, r]));
end;

end.
