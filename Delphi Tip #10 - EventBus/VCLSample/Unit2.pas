unit Unit2;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, EventBus, Events;

type
  TForm2 = class(TForm)
    Memo1: TMemo;
    Button1: TButton;
    Label1: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    [Subscribe]
    procedure onMemoChange ( aEvent : TEventMemoChange );
    [Subscribe]
    procedure onCheckBoxClick ( aEvent : TEventCheckBox);
  end;

var
  Form2: TForm2;

implementation

{$R *.dfm}

uses Unit3;

procedure TForm2.Button1Click(Sender: TObject);
begin
  Form3.Show;
end;

procedure TForm2.FormCreate(Sender: TObject);
begin
  GlobalEventBus.RegisterSubscriberForEvents(Self);
end;

procedure TForm2.onCheckBoxClick(aEvent: TEventCheckBox);
begin
  Label1.Caption := 'not Checked';

  if aEvent.Checked then
    Label1.Caption := 'Checked';

end;

procedure TForm2.onMemoChange(aEvent: TEventMemoChange);
begin
  Memo1.Text := aEvent.Text;
  aEvent.Free;
end;

end.
