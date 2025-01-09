program CleanCode;

uses
  Vcl.Forms,
  uMain in 'uMain.pas' {Form1},
  uMain.Controller.Interfaces in 'uMain.Controller.Interfaces.pas',
  uMain.Controller in 'uMain.Controller.pas',
  uMain.Controller.Creating in 'uMain.Controller.Creating.pas',
  uMain.Controller.Validating in 'uMain.Controller.Validating.pas',
  uMain.Controller.Invoker in 'uMain.Controller.Invoker.pas',
  uMain.Controller.Saving in 'uMain.Controller.Saving.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
