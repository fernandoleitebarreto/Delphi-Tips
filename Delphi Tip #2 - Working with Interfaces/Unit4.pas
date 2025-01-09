unit Unit4;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,
  Vcl.WinXCtrls;

type

  iDispositivo = interface
    ['{858C39EB-E5FD-4642-9E2C-01AA5CBBD096}']
    function Ligar: iDispositivo;
    function Desligar: iDispositivo;
    function Conectado: Boolean;
  end;

  TLampada = class(TInterfacedObject, iDispositivo)
  public
    function Ligar: iDispositivo;
    function Desligar: iDispositivo;
    function Conectado: Boolean;

    property FConectado: Boolean read Conectado;
  end;

  TVentilador = class(TInterfacedObject, iDispositivo)
    function Ligar: iDispositivo;
    function Desligar: iDispositivo;
    function Conectado: Boolean;

    property FConectado: Boolean read Conectado;
  end;

  TBotao = class
    FDispositivo: iDispositivo;
    FConectado: Boolean;
    constructor Create(Dispositivo: iDispositivo);
    function Acionar: iDispositivo;
    function Desconectar: iDispositivo;
    function Conectado: iDispositivo;
  end;

  TForm4 = class(TForm)
    rgDispositivo: TRadioGroup;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    tgInterruptor_Novo: TToggleSwitch;
    tgInterruptor_Velho: TToggleSwitch;
    procedure tgInterruptor_NovoClick(Sender: TObject);
    procedure tgInterruptor_VelhoClick(Sender: TObject);
  private
    { Private declarations }
    function Verifica_Se_Lampada_Esta_Conectado: Boolean;
    function Verifica_Se_Ventilador_Esta_Conectado: Boolean;
  public
    { Public declarations }
  end;

var
  Form4: TForm4;

implementation

{$R *.dfm}
{ TBotao }

function TBotao.Acionar: iDispositivo;
begin
  FDispositivo.Ligar;
  Result := FDispositivo;
end;

constructor TBotao.Create(Dispositivo: iDispositivo);
begin
  FDispositivo := Dispositivo;
end;

function TBotao.Desconectar: iDispositivo;
begin
  FDispositivo.Desligar;
  Result := FDispositivo;
end;

function TBotao.Conectado: iDispositivo;
begin
  FConectado := FDispositivo.Conectado;
  Result := FDispositivo;
end;

{ TLampada }

function TLampada.Desligar: iDispositivo;
begin
  if FConectado then
    ShowMessage('Lampada Desligada');

  Result := Self;
end;

function TLampada.Ligar: iDispositivo;
begin
  if FConectado then
    ShowMessage('Lampada Ligada');
  Result := Self;
end;

function TLampada.Conectado: Boolean;
begin
  Result := True;
end;

{ TVentilador }

function TVentilador.Desligar: iDispositivo;
begin
  if FConectado then
    ShowMessage('Ventilador Desligado');
  Result := Self;
end;

function TVentilador.Ligar: iDispositivo;
begin
  if FConectado then
    ShowMessage('Ventilador Ligado');
  Result := Self;
end;

function TVentilador.Conectado: Boolean;
begin
  Result := False;
end;

procedure TForm4.tgInterruptor_NovoClick(Sender: TObject);
begin
  if tgInterruptor_Novo.State = tssOff then
  begin
    case rgDispositivo.ItemIndex of
      0:
        // TBotao.Create(TLampada.Create).Conectado.Desligar;
        TBotao.Create(TLampada.Create).Desconectar;
      1:
        // TBotao.Create(TVentilador.Create).Conectado.Desligar;
        TBotao.Create(TVentilador.Create).Desconectar;

    end;

  end
  else
  begin
    case rgDispositivo.ItemIndex of
      0:
        // TBotao.Create(TLampada.Create).Conectado.Ligar;
        TBotao.Create(TLampada.Create).Acionar;
      1:
        // TBotao.Create(TVentilador.Create).Conectado.Ligar;
        TBotao.Create(TVentilador.Create).Acionar;
    end;
  end;
end;

procedure TForm4.tgInterruptor_VelhoClick(Sender: TObject);
begin
  if tgInterruptor_Velho.State = tssOff then
  begin
    case rgDispositivo.ItemIndex of
      0: // Lampada
        begin
          if Verifica_Se_Lampada_Esta_Conectado then
            ShowMessage('Lampada Desligada');
        end;
      1: // Ventilador
        begin
          if Verifica_Se_Ventilador_Esta_Conectado then
            ShowMessage('Ventilador Desligado');
        end;

    end;

  end
  else
  begin
    case rgDispositivo.ItemIndex of
      0:
        begin
          if Verifica_Se_Lampada_Esta_Conectado then
            ShowMessage('Lampada Ligada');
        end;
      1:
        begin
          if Verifica_Se_Ventilador_Esta_Conectado then
            ShowMessage('Ventilador Ligado');
        end;
    end;
  end;
end;

function TForm4.Verifica_Se_Lampada_Esta_Conectado: Boolean;
begin
  Result := True;
end;

function TForm4.Verifica_Se_Ventilador_Esta_Conectado: Boolean;
begin
  Result := False;
end;

end.
