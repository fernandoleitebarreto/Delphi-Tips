program FMXMenu_EventBus;

uses
  System.StartUpCopy,
  FMX.Forms,
  View.Principal in 'View.Principal.pas' {Form4},
  View.Button in 'View.Button.pas' {ButtonMenu001},
  View.Events in 'View.Events.pas',
  View.Cadastro in 'View.Cadastro.pas' {Form5};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm4, Form4);
  Application.CreateForm(TButtonMenu001, ButtonMenu001);
  Application.CreateForm(TForm5, Form5);
  Application.Run;
end.
