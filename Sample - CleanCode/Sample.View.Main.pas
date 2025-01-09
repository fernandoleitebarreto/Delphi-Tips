unit Sample.View.Main;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  Sample.Controller.Test.Interfaces;

type
  TForm2 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

uses
  Sample.Controller.Test;

{$R *.dfm}

procedure TForm2.Button1Click(Sender: TObject);
begin
  TController.New.ImportValidateSave;
end;

procedure TForm2.Button2Click(Sender: TObject);
begin
  TController.New.ImportValidateSaveSendEmail;
end;

procedure TForm2.Button3Click(Sender: TObject);
begin
  TController.New.ImportValidateSave_WithoutInterface;

end;

end.
