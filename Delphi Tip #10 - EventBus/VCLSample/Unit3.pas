unit Unit3;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, EventBus, Events, Vcl.StdCtrls;

type
  TForm3 = class(TForm)
    Memo1: TMemo;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    [Subscribe]
    procedure onMemoChange ( aEvent : TEventMemoChange );
  end;

var
  Form3: TForm3;

implementation

{$R *.dfm}

{ TForm3 }

procedure TForm3.FormCreate(Sender: TObject);
begin
  GlobalEventBus.RegisterSubscriberForEvents(Self);
end;

procedure TForm3.onMemoChange(aEvent: TEventMemoChange);
begin
  Memo1.Text := aEvent.Text;
  aEvent.Free;
end;

end.
