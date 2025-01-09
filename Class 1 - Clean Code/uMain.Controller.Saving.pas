unit uMain.Controller.Saving;

interface

uses uMain.Controller.Interfaces;

type

  TControllerCleanCodeSaving = class(TInterfacedObject, iCommand)
  private
    FParent: iCleanCode;
  protected

  public
    constructor Create(Parent: iCleanCode);
    destructor Destroy; override;
    class function New(Parent: iCleanCode): iCommand;
    function Execute: iCommand;

  published

  end;

implementation

uses
  Vcl.Dialogs;

{ TControllerCleanCodeCreating }

constructor TControllerCleanCodeSaving.Create(Parent: iCleanCode);
begin
  FParent := Parent;

end;

destructor TControllerCleanCodeSaving.Destroy;
begin

  inherited;
end;

function TControllerCleanCodeSaving.Execute: iCommand;
begin
  Result := Self;
  FParent.Saving;
end;

class function TControllerCleanCodeSaving.New(Parent: iCleanCode): iCommand;
begin
  Result := Self.Create(Parent);
end;

end.
