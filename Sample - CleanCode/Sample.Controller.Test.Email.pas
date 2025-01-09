unit Sample.Controller.Test.Email;

interface

uses
  Sample.Controller.Test.Interfaces;

Type
  TControllerEmail = class(TInterfacedObject, iCommand)
    private
      FParent : iTest;
    public
      constructor Create(Parent : iTest);
      destructor Destroy; override;
      class function New(Parent : iTest) : iCommand;
      function Execute : iCommand;
  end;

implementation

{ TControllerEmail }

constructor TControllerEmail.Create(Parent : iTest);
begin
  FParent := Parent;
end;

destructor TControllerEmail.Destroy;
begin

  inherited;
end;

function TControllerEmail.Execute: iCommand;
begin
  Result := Self;
  FParent.Email;
end;

class function TControllerEmail.New(Parent : iTest) : iCommand;
begin
  Result := Self.Create(Parent);
end;

end.
