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

  // Exception	Exce��o gen�rica, usada apenas como ancestral de todas as outras exce��es
  // EAbort	Exce��o silenciosa, pode ser gerada pelo procedimento Abort e n�o mostra nenhuma mensagem
  // EAccessViolation	Acesso inv�lido � mem�ria, geralmente ocorre com objetos n�o inicializados
  // EConvertError	Erro de convers�o de tipos
  // EDivByZero	Divis�o de inteiro por zero
  // EInOutError	Erro de Entrada ou Sa�da reportado pelo sistema operacional
  // EIntOverFlow	Resultado de um c�lculo inteiro excedeu o limite
  // EInvalidCast	TypeCast inv�lido com o operador as
  // EInvalidOp	Opera��o inv�lida com n�mero de ponto flutuante
  // EOutOfMemory	Mem�ria insuficiente
  // EOverflow	Resultado de um c�lculo com n�mero real excedeu o limite
  // ERangeError	Valor excede o limite do tipo inteiro ao qual foi atribu�da
  // EUnderflow	Resultado de um c�lculo com n�mero real � menor que a faixa v�lida
  // EVariantError	Erro em opera��o com variant
  // EZeroDivide	Divis�o de real por zero
  // EDatabaseError	Erro gen�rico de banco de dados, geralmente n�o � usado diretamente
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
      Result := 'Exce��o Gen�rica';
    EEAbort:
      Result := 'Exce��o Abortada';
    EEConvertError:
      Result := 'Erro de convers�o de tipos';
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
