unit View.Button;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  EventBus, View.Events, FMX.StdCtrls, FMX.Controls.Presentation, FMX.Objects, FMX.Layouts;

type
  TButtonMenu001 = class(TForm)
    Layout1: TLayout;
    Line1: TLine;
    Label1: TLabel;
    SpeedButton1: TSpeedButton;
    procedure SpeedButton1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    FonClick : TProc;
  public
    { Public declarations }
    function Text ( aValue : String ) : TButtonMenu001;
    function Align ( aValue : TAlignLayout ) : TButtonMenu001;
    function onClick ( aValue : TProc ) : TButtonMenu001;
    function Component : TFMXObject;

    [Subscribe]
    procedure onButtonClick ( aEvent : TEventMenuButton );
  end;

var
  ButtonMenu001: TButtonMenu001;

implementation

{$R *.fmx}

{ TButtonMenu001 }

function TButtonMenu001.Align(aValue: TAlignLayout): TButtonMenu001;
begin
  Result := Self;
  Layout1.Align := aValue;
end;

function TButtonMenu001.Component: TFMXObject;
begin
  Result := Layout1;
end;

procedure TButtonMenu001.FormCreate(Sender: TObject);
begin
  Line1.Visible := False;
  GlobalEventBus.RegisterSubscriber(Self);
end;

procedure TButtonMenu001.onButtonClick(aEvent: TEventMenuButton);
begin
  Line1.Visible := False;

  if aEvent.ButtonClickName = Label1.Text then
    Line1.Visible := True;

  aEvent.Free;
end;

function TButtonMenu001.onClick(aValue: TProc): TButtonMenu001;
begin
  Result := Self;
  FonClick := aValue;
end;

procedure TButtonMenu001.SpeedButton1Click(Sender: TObject);
var
  Event : TEventMenuButton;
begin
  Event := TEventMenuButton.Create;
  Event.ButtonClickName := Label1.Text;
  GlobalEventBus.Post(Event);

  if Assigned(FonClick) then
    FonClick();
end;

function TButtonMenu001.Text(aValue: String): TButtonMenu001;
begin
  Result := Self;
  Label1.Text := aValue;
end;

end.
