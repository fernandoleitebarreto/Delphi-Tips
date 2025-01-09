program SmartPoint;

uses
  Vcl.Forms,
  Unit1 in 'Unit1.pas' {Form1},
  uSmartPoint in 'uSmartPoint.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
