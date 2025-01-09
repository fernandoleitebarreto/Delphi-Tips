unit Sample.Controller.Test;

interface

uses
  Sample.Controller.Test.Interfaces;

type
  TController = class(TInterfacedObject, iTest, iTestView)
    private
    public
      constructor Create;
      destructor Destroy; override;
      class function New: iTestView;
      function Initialize: iTest;
      function Validate: iTest;
      function Send: iTest;
      function Save: iTest;
      function Email: iTest;
      function Import(Value : Integer) : iTest;
      function ImportValidateSaveSendEmail : iTest;
      function ImportValidateSaveSend : iTest;
      function ImportValidateSaveSend_WithoutInterface: iTest;
  end;

implementation

uses
  Sample.Controller.Invoker, Sample.Controller.Test.Initialize,
  Sample.Controller.Test.Validate, Sample.Controller.Test.Send,
  Sample.Controller.Test.Save, Sample.Controller.Test.Email,
  Sample.Controller.Test.Import;

{ TController }

constructor TController.Create;
begin

end;

destructor TController.Destroy;
begin

  inherited;
end;

function TController.Initialize: iTest;
begin
  Result := Self;
end;

function TController.Email: iTest;
begin
  Result := Self;
end;

function TController.Send: iTest;
begin
  Result := Self;
end;

function TController.ImportValidateSaveSendEmail: iTest;
begin
  Result := Self;
  TControllerInvoker.New
    .Add(TControllerInitialize.New(Self))
    .Add(TControllerImport.New(Self, 1))
    .Add(TControllerValidate.New(Self))
    .Add(TControllerSave.New(Self))
    .Add(TControllerSend.New(Self))
    .Add(TControllerEmail.New(Self))
    .Execute;
end;

function TController.ImportValidateSaveSend: iTest;
begin
  Result := Self;
  TControllerInvoker.New
    .Add(TControllerInitialize.New(Self))
    .Add(TControllerImport.New(Self, 2))
    .Add(TControllerValidate.New(Self))
    .Add(TControllerSave.New(Self))
    .Add(TControllerSend.New(Self))
    .Execute;
end;

function TController.ImportValidateSaveSend_WithoutInterface;
begin
  Initialize();
  Validate();
  Save();
  Send();
end;

function TController.Save: iTest;
begin
  Result := Self;
end;

function TController.Import(Value : Integer) : iTest;
begin
  Result := Self;
end;

class function TController.New: iTestView;
begin
  Result := Self.Create;
end;

function TController.Validate: iTest;
begin
  Result := Self;
end;

end.
