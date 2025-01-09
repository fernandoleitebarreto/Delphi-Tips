program TScrollBarNoTabSheet;

uses
  madExcept,
  madLinkDisAsm,
  madListHardware,
  madListProcesses,
  madListModules,
  Vcl.Forms,
  uScrollBarNoTabSheet in 'uScrollBarNoTabSheet.pas' {FrmScrollBoxTabSheet};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFrmScrollBoxTabSheet, FrmScrollBoxTabSheet);
  Application.Run;

end.
