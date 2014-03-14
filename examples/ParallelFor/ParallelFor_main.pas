unit ParallelFor_main;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Samples.Spin,
  ThreadUtils;

type
  TForm_main = class(TForm)
    Memo_log: TMemo;
    Edt_Iterations: TSpinEdit;
    Btn_For: TButton;
    Btn_Test: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Btn_ForClick(Sender: TObject);
  public
    { Public declarations }

  end;

var
  Form_main: TForm_main;

implementation

uses
  SyncObjs;

{$R *.dfm}

procedure TForm_main.Btn_ForClick(Sender: TObject);
var
  i: integer;
begin
  Enabled := false;
  Memo_log.Clear;

  for i := 1 to Edt_Iterations.Value do
    TParallel<integer>.Run(i,
      procedure(index: integer)
      begin
        Memo_log.Lines.Add('Start of iteration ' + inttostr(index));
        sleep(random(1900) + 100);
        Memo_log.Lines.Add('End of iteration ' + inttostr(index));
      end);

  TParallel<integer>.ActiveWaitFor(Application.ProcessMessages);
  Memo_log.Lines.Add('-- No more iterations left --');
  Enabled := true;
end;

procedure TForm_main.FormCreate(Sender: TObject);
begin
  Memo_log.Clear;
end;

procedure TForm_main.FormDestroy(Sender: TObject);
begin
 //
end;

end.
