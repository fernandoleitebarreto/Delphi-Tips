unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TForm1 = class(TForm)
    Memo1: TMemo;
    CheckBox1: TCheckBox;
    Button1: TButton;
    procedure Memo1Change(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses
  EventBus, Events, Unit2;

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
begin
  Form2.Show;
end;

procedure TForm1.CheckBox1Click(Sender: TObject);
var
  Event : TEventCheckBox;
begin
  Event := TEventCheckBox.Create;
  Event.Checked := CheckBox1.Checked;
  GlobalEventBus.Post(Event);
end;

procedure TForm1.Memo1Change(Sender: TObject);
var
  Event : TEventMemoChange;
begin
  Event := TEventMemoChange.Create;
  Event.Text := Memo1.Text;
  GlobalEventBus.Post(Event);
end;

end.
