unit Sample.Controller.Test.Import;

interface

uses
  Sample.Controller.Test.Interfaces;

Type
  TControllerImport = class(TInterfacedObject, iCommand)
    private
      FParent : iTest;
      FCode : Integer;
    public
      constructor Create(Parent: iTest; Code: Integer);
      destructor Destroy; override;
      class function New(Parent: iTest; Code: Integer) : iCommand;
      function Execute : iCommand;
  end;

implementation

{ TControllerImport }

constructor TControllerImport.Create(Parent : iTest; Code : Integer);
begin
  FParent := Parent;
  FCode := Code;
end;

destructor TControllerImport.Destroy;
begin

  inherited;
end;

function TControllerImport.Execute: iCommand;
begin
  Result := Self;
  FParent.Import(FCode);
end;

class function TControllerImport.New(Parent : iTest; Code : Integer) : iCommand;
begin
  Result := Self.Create(Parent, Code);
end;

end.
