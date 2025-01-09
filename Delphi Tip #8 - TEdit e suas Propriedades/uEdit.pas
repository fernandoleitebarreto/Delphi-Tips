unit uEdit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, StrUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TForm1 = class(TForm)
    Edit1: TEdit;
    btnSelStart: TButton;
    Edit2: TEdit;
    Button2: TButton;
    btnSetSelTextBuf: TButton;
    btnSetSelText: TButton;
    btnSetBounds: TButton;
    procedure btnSelStartClick(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure btnSetSelTextBufClick(Sender: TObject);
    procedure btnSetSelTextClick(Sender: TObject);
    procedure btnSetBoundsClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    FTop: Integer;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.btnSetSelTextBufClick(Sender: TObject);
var
  s: array [0 .. 15] of Char;
begin
  s := 'BlaBlaBlaBlaBla';
  Edit1.SetSelTextBuf(s);
end;

procedure TForm1.btnSelStartClick(Sender: TObject);
begin
  if Edit1.CanFocus then
    Edit1.SetFocus;
  Edit1.SelStart := Length(Edit1.Text);
end;

procedure TForm1.btnSetBoundsClick(Sender: TObject);
begin
  FTop := StrToInt(ifthen(FTop = 56, '10', '56'));
  Edit1.SetBounds(48, FTop, 121, 21);
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  if Edit2.CanFocus then
    Edit2.SetFocus;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  FTop := Top;
end;

procedure TForm1.btnSetSelTextClick(Sender: TObject);
begin
  Edit1.SetSelText('teste');
end;

end.
