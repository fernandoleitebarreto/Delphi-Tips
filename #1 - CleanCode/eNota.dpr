program eNota;

uses
  Vcl.Forms,
  eNota.Controller.Invoker in 'eNota.Controller.Invoker.pas',
  eNota.Controller.NotaFiscal.Criar in 'eNota.Controller.NotaFiscal.Criar.pas',
  eNota.Controller.NotaFiscal.Email in 'eNota.Controller.NotaFiscal.Email.pas',
  eNota.Controller.NotaFiscal.Enviar in 'eNota.Controller.NotaFiscal.Enviar.pas',
  eNota.Controller.NotaFiscal.Gravar in 'eNota.Controller.NotaFiscal.Gravar.pas',
  eNota.Controller.NotaFiscal.ImportarPedido in 'eNota.Controller.NotaFiscal.ImportarPedido.pas',
  eNota.Controller.NotaFiscal.Interfaces in 'eNota.Controller.NotaFiscal.Interfaces.pas',
  eNota.Controller.NotaFiscal in 'eNota.Controller.NotaFiscal.pas',
  eNota.Controller.NotaFiscal.Validar in 'eNota.Controller.NotaFiscal.Validar.pas',
  eNota.View.Principal in 'eNota.View.Principal.pas' {Form2};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.
