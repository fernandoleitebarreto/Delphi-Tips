unit uMain.Controller;

interface

uses uMain.Controller.Interfaces;

type

  TControllerCleanCode = class(TInterfacedObject, iCleanCode, iCleanCodeView)
  private

  protected

  public
    constructor Create;
    destructor Destroy; override;
    class function New: iCleanCodeView;
    function Creating: iCleanCode;
    function Validating: iCleanCode;
    function Saving: iCleanCode;
    function Processar: iCleanCode;
    function ProcessarSemValidar: iCleanCode;

  published

  end;

implementation

{ TControllerCleanCode }

uses uMain.Controller.Invoker, uMain.Controller.Creating,
  uMain.Controller.Validating, uMain.Controller.Saving, Vcl.Dialogs;

constructor TControllerCleanCode.Create;
begin
  inherited;

end;

function TControllerCleanCode.Creating: iCleanCode;
begin
  Result := Self;
  ShowMessage('Creating');
end;

destructor TControllerCleanCode.Destroy;
begin

  inherited;
end;

class function TControllerCleanCode.New: iCleanCodeView;
begin
  Result := Self.Create;
end;

function TControllerCleanCode.Processar: iCleanCode;
begin
  Result := Self;
  TControllerInvoker.New.Add(TControllerCleanCodeCreating.New(Self))
    .Add(TControllerCleanCodeValidating.New(Self))
    .Add(TControllerCleanCodeSaving.New(Self)).Execute;
end;

function TControllerCleanCode.ProcessarSemValidar: iCleanCode;
begin
  Result := Self;
  TControllerInvoker.New.Add(TControllerCleanCodeCreating.New(Self))
    .Add(TControllerCleanCodeSaving.New(Self)).Execute;
end;

function TControllerCleanCode.Saving: iCleanCode;
begin
  Result := Self;
  ShowMessage('Saving');
end;

function TControllerCleanCode.Validating: iCleanCode;
begin
  Result := Self;
  ShowMessage('Validating');
end;

end.
