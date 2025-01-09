unit Sample.Controller.Test.Validate;

interface

uses
  Sample.Controller.Test.Interfaces;

Type
  TControllerValidate = class(TInterfacedObject, iCommand)
    private
      FParent : iTest;
    public
      constructor Create(Parent : iTest);
      destructor Destroy; override;
      class function New(Parent : iTest) : iCommand;
      function Execute : iCommand;
  end;

implementation

{ TControllerValidate }

constructor TControllerValidate.Create(Parent : iTest);
begin
  FParent := Parent;
end;

destructor TControllerValidate.Destroy;
begin

  inherited;
end;

function TControllerValidate.Execute: iCommand;
begin
  Result := Self;
  FParent.Validate;
end;

class function TControllerValidate.New(Parent : iTest) : iCommand;
begin
  Result := Self.Create(Parent);
end;

end.
