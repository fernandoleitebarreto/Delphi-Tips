unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses uSmartPoint;

procedure TForm1.Button1Click(Sender: TObject);
var
  SL: TStringlist;
begin
  SL := TStringlist.Create;
  try
    SL.Add('Teste');
    ShowMessage('Total de Registros: ' + IntToStr(SL.Count));
  finally
    SL.Free;
  end;
end;

procedure TForm1.Button2Click(Sender: TObject);
var
  SL: TSmartPoint<TStringlist>;
begin
  SL.Value.Add('Teste');
  ShowMessage('Total de Registro: ' + IntToStr(SL.Value.Count));

end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  ReportMemoryLeaksOnShutdown := True;
end;

end.
