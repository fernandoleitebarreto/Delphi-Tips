unit View.Principal;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Layouts,
  FMX.Controls.Presentation, FMX.StdCtrls;

type
  TForm4 = class(TForm)
    Layout1: TLayout;
    Label1: TLabel;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure Click1;
    procedure Click2;
    procedure Click3;
    procedure Click4;
  end;

var
  Form4: TForm4;

implementation

{$R *.fmx}

uses View.Button, View.Cadastro;

procedure TForm4.Click1;
begin
  ShowMessage('Você Clicou no Botão 1');
end;

procedure TForm4.Click2;
begin
  Label1.Text := 'Você Clicou no botão 2';
end;

procedure TForm4.Click3;
begin
  Form5.Show;
end;

procedure TForm4.Click4;
begin
  ShowMessage('Você clicou no Botão 4');
end;

procedure TForm4.FormCreate(Sender: TObject);
begin
  Layout1.AddObject(
    TButtonMenu001
      .Create(Self)
      .Text('Inicio')
      .Align(TAlignLayout.Left)
      .onClick(Click1)
      .Component
  );

  Layout1.AddObject(
    TButtonMenu001
      .Create(Self)
      .Text('Cadastros')
      .Align(TAlignLayout.Left)
      .onClick(Click2)
      .Component
  );

  Layout1.AddObject(
    TButtonMenu001
      .Create(Self)
      .Text('Relatorios')
      .Align(TAlignLayout.Left)
      .onClick(Click3)
      .Component
  );

   Layout1.AddObject(
    TButtonMenu001
      .Create(Self)
      .Text('Configuração')
      .Align(TAlignLayout.Left)
      .onClick(Click4)
      .Component
  );
end;

end.
