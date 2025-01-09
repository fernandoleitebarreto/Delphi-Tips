program ClassOperator;

uses
  Vcl.Forms,
  uClassOperator in 'uClassOperator.pas' {frmClassOperator};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmClassOperator, frmClassOperator);
  Application.Run;
end.
