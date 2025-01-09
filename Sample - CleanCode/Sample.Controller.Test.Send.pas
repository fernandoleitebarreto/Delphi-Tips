unit Sample.Controller.Test.Send;

interface

uses
  Sample.Controller.Test.Interfaces;

Type
  TControllerSend = class(TInterfacedObject, iCommand)
    private
      FParent : iTest;
    public
      constructor Create(Parent : iTest);
      destructor Destroy; override;
      class function New(Parent : iTest) : iCommand;
      function Execute : iCommand;
  end;

implementation

{ TControllerSend }

constructor TControllerSend.Create(Parent : iTest);
begin
  FParent := Parent;
end;

destructor TControllerSend.Destroy;
begin

  inherited;
end;

function TControllerSend.Execute: iCommand;
begin
  Result := Self;
  FParent.Send;
end;

class function TControllerSend.New(Parent : iTest) : iCommand;
begin
  Result := Self.Create(Parent);
end;

end.
