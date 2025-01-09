unit uClassOperator;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TProduto = record
    FValor: Integer;
    FNome: String;

    class operator Add(a, b: TProduto): TProduto;
    class operator Implicit(a: String): TProduto;
    class operator Implicit(a: TProduto): String;
    class operator Implicit(a: Integer): TProduto;
    class operator Positive(Value: TProduto): TProduto;
    class operator Negative(Value: TProduto): TProduto;
    class operator Inc(Value: TProduto): TProduto;
    class operator Equal(a, b: TProduto): Boolean;

    class function Create: TProduto; static;

    property Valor: Integer read FValor;
    property Nome: string read FNome;
  end;

  TfrmClassOperator = class(TForm)
    Button1: TButton;
    btnImplicit: TButton;
    btnAdd: TButton;
    btnImplicit_Integer: TButton;
    btnPositive: TButton;
    Label1: TLabel;
    btnNegative: TButton;
    btnInc: TButton;
    btnEqual: TButton;
    btnImplicit_ShowMessage: TButton;
    procedure Button1Click(Sender: TObject);
    procedure btnImplicitClick(Sender: TObject);
    procedure btnAddClick(Sender: TObject);
    procedure btnImplicit_IntegerClick(Sender: TObject);
    procedure btnNegativeClick(Sender: TObject);
    procedure btnPositiveClick(Sender: TObject);
    procedure btnIncClick(Sender: TObject);
    procedure btnEqualClick(Sender: TObject);
    procedure btnImplicit_ShowMessageClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmClassOperator: TfrmClassOperator;

implementation

{$R *.dfm}

procedure TfrmClassOperator.Button1Click(Sender: TObject);
var
  a, b, c: TProduto;
begin
  a := TProduto.Create;
  b := TProduto.Create;
  c := TProduto.Create;
  a.FValor := 10;
  b.FValor := 30;
  c.FValor := a.Valor + b.Valor;
  ShowMessage(IntToStr(c.Valor));

end;

procedure TfrmClassOperator.btnAddClick(Sender: TObject);
var
  a, b, c: TProduto;
begin
  a.FValor := 10;
  b.FValor := 30;
  c := a + b;
  ShowMessage(IntToStr(c.Valor));

end;

procedure TfrmClassOperator.btnImplicitClick(Sender: TObject);
var
  a: TProduto;
begin
  a := TProduto.Create;
  a := 'Arroz';
  ShowMessage(a);
end;

procedure TfrmClassOperator.btnImplicit_ShowMessageClick(Sender: TObject);
var
  a: TProduto;
begin
  a := TProduto.Create;
  a := 'Arroz';
  a := 10;
  ShowMessage(a);
end;

procedure TfrmClassOperator.btnImplicit_IntegerClick(Sender: TObject);
var
  a: TProduto;
begin
  a := TProduto.Create;
  a := 10;
  ShowMessage(a);

end;

procedure TfrmClassOperator.btnPositiveClick(Sender: TObject);
var
  Prod: TProduto;
begin
  Prod := 10;
  Prod := +Prod;

  ShowMessage(Prod);
end;

procedure TfrmClassOperator.btnNegativeClick(Sender: TObject);
var
  Prod: TProduto;
begin
  Prod := 10;
  Prod := -Prod;

  ShowMessage(Prod);
end;

procedure TfrmClassOperator.btnIncClick(Sender: TObject);
var
  Prod: TProduto;
begin
  Prod := 10;
  Inc(Prod);
  ShowMessage(Prod);
end;

procedure TfrmClassOperator.btnEqualClick(Sender: TObject);
var
  ProdA, ProdB: TProduto;
begin
  ProdA := 'Arroz';
  ProdA := 10;
  ProdB := 'Arroz';
  ProdB := 10;

  if ProdA = ProdB then
    ShowMessage('Igual')
  else
    ShowMessage('Diferente');

end;

{ TProduto }

class function TProduto.Create: TProduto;
begin
  Result.FNome := '';
  Result.FValor := 0;
end;

class operator TProduto.Add(a, b: TProduto): TProduto;
begin
  Result := TProduto.Create;
  Result.FValor := a.FValor + b.FValor;
end;

class operator TProduto.Implicit(a: String): TProduto;
begin
  Result.FNome := a;
end;

class operator TProduto.Implicit(a: Integer): TProduto;
begin
  Result.FValor := a;
end;

class operator TProduto.Implicit(a: TProduto): String;
begin
  Result := '';
  if a.Nome <> '' then
    Result := Result + 'Produto: ' + a.Nome;
  if a.Valor > 0 then
    Result := Result + ' Valor: ' + IntToStr(a.Valor);
end;

class operator TProduto.Positive(Value: TProduto): TProduto;
begin
  Result.FValor := Value.FValor + 1;
end;

class operator TProduto.Negative(Value: TProduto): TProduto;
begin
  Result.FValor := Value.FValor - 1;
end;

class operator TProduto.Inc(Value: TProduto): TProduto;
begin
  Result.FValor := Value.FValor + 1;
end;

class operator TProduto.Equal(a, b: TProduto): Boolean;
begin
  Result := (a.Valor = b.Valor) and (a.Nome = b.Nome);
end;

end.
