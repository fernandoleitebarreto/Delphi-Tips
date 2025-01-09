unit uMain.Controller.Creating;

interface

uses uMain.Controller.Interfaces;

type

  TControllerCleanCodeCreating = class(TInterfacedObject, iCommand)
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

{ TControllerCleanCodeCreating }

constructor TControllerCleanCodeCreating.Create(Parent: iCleanCode);
begin
  FParent := Parent;

end;

destructor TControllerCleanCodeCreating.Destroy;
begin

  inherited;
end;

function TControllerCleanCodeCreating.Execute: iCommand;
begin
  Result := Self;
  FParent.Creating;
end;

class function TControllerCleanCodeCreating.New(Parent: iCleanCode): iCommand;
begin
  Result := Self.Create(Parent);
end;

end.
