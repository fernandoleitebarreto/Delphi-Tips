unit Unit1;

interface

uses Vcl.StdCtrls, System.Classes, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  System.SysUtils;

Type
  IDoingSomething<T> = interface
    ['{057A07C8-4DBF-411B-BD21-7A818A0A6E9A}']
    procedure Add(Value: T);
    function Retorno: T;
  end;

  TDoingSomething<T> = class(TInterfacedObject, IDoingSomething<T>)
    FValue: T;
    procedure Add(Value: T);
    function Retorno: T;
  end;

type
  TForm1 = class(TForm)
    btnString: TButton;
    btnInteger: TButton;
    Label1: TLabel;
    procedure btnStringClick(Sender: TObject);
    procedure btnIntegerClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}
{ TDoingSomething<T> }

procedure TDoingSomething<T>.Add(Value: T);
begin
  FValue := Value;
end;

function TDoingSomething<T>.Retorno: T;
begin
  Result := FValue;
end;

procedure TForm1.btnIntegerClick(Sender: TObject);
var
  Aux: IDoingSomething<Integer>;
begin
  Aux := TDoingSomething<Integer>.Create;
  Aux.Add(1);
  ShowMessage(IntToStr(Aux.Retorno));

end;

procedure TForm1.btnStringClick(Sender: TObject);
var
  Aux: IDoingSomething<String>;
begin
  Aux := TDoingSomething<String>.Create;
  Aux.Add('Teste');
  ShowMessage(Aux.Retorno);

end;

end.
