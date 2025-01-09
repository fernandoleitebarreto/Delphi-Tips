program Sample;

uses
  Vcl.Forms,
  Sample.Controller.Invoker in 'Sample.Controller.Invoker.pas',
  Sample.Controller.Test.Initialize in 'Sample.Controller.Test.Initialize.pas',
  Sample.Controller.Test.Email in 'Sample.Controller.Test.Email.pas',
  Sample.Controller.Test.Send in 'Sample.Controller.Test.Send.pas',
  Sample.Controller.Test.Save in 'Sample.Controller.Test.Save.pas',
  Sample.Controller.Test.Import in 'Sample.Controller.Test.Import.pas',
  Sample.Controller.Test.Interfaces in 'Sample.Controller.Test.Interfaces.pas',
  Sample.Controller.Test in 'Sample.Controller.Test.pas',
  Sample.Controller.Test.Validate in 'Sample.Controller.Test.Validate.pas',
  Sample.View.Main in 'Sample.View.Main.pas' {Form2};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.
