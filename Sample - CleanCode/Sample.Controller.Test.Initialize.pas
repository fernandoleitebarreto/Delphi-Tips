unit Sample.Controller.Test.Initialize;

interface

uses
  Sample.Controller.Test.Interfaces;

Type
  TControllerInitialize = class(TInterfacedObject, iCommand)
  private
    FParent: iTest;
  public
    constructor Create(Parent: iTest);
    destructor Destroy; override;
    class function New(Parent: iTest): iCommand;
    function Execute: iCommand;
  end;

implementation

{ TControllerInitialize }

constructor TControllerInitialize.Create(Parent: iTest);
begin
  FParent := Parent;
end;

destructor TControllerInitialize.Destroy;
begin

  inherited;
end;

function TControllerInitialize.Execute: iCommand;
begin
  Result := Self;
  FParent.Initialize;
end;

class function TControllerInitialize.New(Parent: iTest): iCommand;
begin
  Result := Self.Create(Parent);
end;

end.
