unit TrataException;

interface

uses
  SysUtils, Forms, System.Classes;

type
  TException = class
  private
    FLogFile: String;
  public
    constructor Create;
    procedure TrataException(Sender: TObject; E: Exception);
    procedure GravarLog(Value: String);
  end;

  // Exception	Exceção genérica, usada apenas como ancestral de todas as outras exceções
  // EAbort	Exceção silenciosa, pode ser gerada pelo procedimento Abort e não mostra nenhuma mensagem
  // EAccessViolation	Acesso inválido à memória, geralmente ocorre com objetos não inicializados
  // EConvertError	Erro de conversão de tipos
  // EDivByZero	Divisão de inteiro por zero
  // EInOutError	Erro de Entrada ou Saída reportado pelo sistema operacional
  // EIntOverFlow	Resultado de um cálculo inteiro excedeu o limite
  // EInvalidCast	TypeCast inválido com o operador as
  // EInvalidOp	Operação inválida com número de ponto flutuante
  // EOutOfMemory	Memória insuficiente
  // EOverflow	Resultado de um cálculo com número real excedeu o limite
  // ERangeError	Valor excede o limite do tipo inteiro ao qual foi atribuída
  // EUnderflow	Resultado de um cálculo com número real é menor que a faixa válida
  // EVariantError	Erro em operação com variant
  // EZeroDivide	Divisão de real por zero
  // EDatabaseError	Erro genérico de banco de dados, geralmente não é usado diretamente
  // EDBEngineError	Erro da BDE, descende de EDatabaseError e traz dados que podem identificar o erro
  //
  //
  // Read more: http://www.linhadecodigo.com.br/artigo/1258/delphi-tratamento-de-execucoes-robustas.aspx#ixzz6VPojzMWJ

  TExceptions = (EUnknown = 0, EException, EEAbort, EEConvertError);

  TExceptionsHelper = record helper for TExceptions
    function ToString: string;
    class function Parse(Value: string): TExceptions; static;
  end;

implementation

uses
  Vcl.Dialogs;

{ TException }

constructor TException.Create;
begin
  FLogFile := ChangeFileExt(ParamStr(0), '.log');
  Application.OnException := TrataException;
end;

procedure TException.GravarLog(Value: String);
var
  txtLog: TextFile;
begin
  AssignFile(txtLog, FLogFile);
  if FileExists(FLogFile) then
    Append(txtLog)
  else
    Rewrite(txtLog);
  Writeln(txtLog, FormatDateTime('dd/mm/YY hh:nn:ss - ', Now) + Value);
  CloseFile(txtLog);
end;

procedure TException.TrataException(Sender: TObject; E: Exception);
var
  Form: TForm absolute Sender;
  Component: TComponent absolute Sender;
begin

  GravarLog('===========================================');
  if Sender is TForm then
  begin
    GravarLog('Form: ' + Form.Name);
    GravarLog('Caption: ' + Form.Caption);
    GravarLog('ClassName:' + E.ClassName);
    GravarLog('Message:' + TExceptions.Parse(E.ClassName).ToString);
  end
  else
  begin
    GravarLog('Form: ' + TForm(Component.Owner).Name);
    GravarLog('Caption: ' + TForm(Component.Owner).Caption);
    GravarLog('ClassName:' + E.ClassName);
    GravarLog('Message:' + TExceptions.Parse(E.ClassName).ToString);
  end;
  ShowMessage(E.Message);
end;

{ TExceptionsHelper }

class function TExceptionsHelper.Parse(Value: string): TExceptions;
begin
  if Value = 'Exception' then
    Result := EException
  else if Value = 'EAbort' then
    Result := EEAbort
  else if Value = 'EConvertError' then
    Result := EEConvertError
  else
    Result := EUnknown;
end;

function TExceptionsHelper.ToString: string;
begin
  case self of
    EException:
      Result := 'Exceção Genérica';
    EEAbort:
      Result := 'Exceção Abortada';
    EEConvertError:
      Result := 'Erro de conversão de tipos';
  else
    Result := '';
  end;
end;

var
  MinhaException: TException;

initialization

MinhaException := TException.Create;

finalization

MinhaException.Free;

end.
