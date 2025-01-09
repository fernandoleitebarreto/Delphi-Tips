unit Sample.Controller.Test.Save;

interface

uses
  Sample.Controller.Test.Interfaces;

Type
  TControllerSave = class(TInterfacedObject, iCommand)
    private
      FParent : iTest;
    public
      constructor Create(Parent : iTest);
      destructor Destroy; override;
      class function New(Parent : iTest) : iCommand;
      function Execute : iCommand;
  end;

implementation

{ TControllerSave }

constructor TControllerSave.Create(Parent : iTest);
begin
  FParent := Parent;
end;

destructor TControllerSave.Destroy;
begin

  inherited;
end;

function TControllerSave.Execute: iCommand;
begin
  Result := Self;
  FParent.Save;
end;

class function TControllerSave.New(Parent : iTest) : iCommand;
begin
  Result := Self.Create(Parent);
end;

end.
