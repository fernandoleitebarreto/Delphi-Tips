unit uMain.Controller.Validating;

interface

uses uMain.Controller.Interfaces;

type

  TControllerCleanCodeValidating = class(TInterfacedObject, iCommand)
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

{ TControllerCleanCodeValidating }

constructor TControllerCleanCodeValidating.Create(Parent: iCleanCode);
begin
  FParent := Parent;

end;

destructor TControllerCleanCodeValidating.Destroy;
begin

  inherited;
end;

function TControllerCleanCodeValidating.Execute: iCommand;
begin
  Result := Self;
  FParent.Validating;
end;

class function TControllerCleanCodeValidating.New(Parent: iCleanCode): iCommand;
begin
  Result := Self.Create(Parent);
end;

end.
