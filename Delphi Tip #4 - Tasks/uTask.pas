unit uTask;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, System.Threading,
  System.SyncObjs;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Button2: TButton;
    Label4: TLabel;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }
    tasks: array of ITask;
    procedure Starting;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.Starting;
begin
  Label1.Caption := '--';
  Label2.Caption := '--';
  Label3.Caption := '--';
  Setlength(tasks, 2);
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  Starting;
  tasks[0] := TTask.Create(
    procedure
    begin
      sleep(3000);
      Label1.Caption := Random(10).ToString;
    end);
  tasks[0].Start;

  tasks[1] := TTask.Create(
    procedure()
    begin
      sleep(5000);
      Label2.Caption := Random(10).ToString;
    end);
  tasks[1].Start;

  TTask.WaitForAll(tasks);
  Label3.Caption := Label1.Caption + ' + ' + Label2.Caption;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  Starting;
  tasks[0] := TTask.Run(
    procedure
    begin
      sleep(3000);
      TThread.Synchronize(TThread.CurrentThread,
        procedure
        begin
          Label1.Caption := Random(10).ToString;
        end);
    end);

  tasks[1] := TTask.Run(
    procedure
    begin
      sleep(5000);
      TThread.Synchronize(TThread.CurrentThread,
        procedure
        begin
          Label2.Caption := Random(10).ToString;
        end);
    end);

  TTask.Run(
    procedure
    begin
      TTask.WaitForAll(tasks);
      TThread.Synchronize(TThread.CurrentThread,
        procedure
        begin
          Label3.Caption := Label1.Caption + ' + ' + Label2.Caption;
        end);
    end);
end;

end.
