unit uMain.Controller.Invoker;

interface

uses uMain.Controller.Interfaces, System.Generics.Collections;

type
  TControllerInvoker = class(TInterfacedObject, iInvoker)
  private
    FList: TList<iCommand>;

  protected

  public
    constructor Create;
    destructor Destroy; override;
    class function New: iInvoker;
    function Add(Value: iCommand): iInvoker;
    function Execute: iInvoker;

  published

  end;

implementation

uses
  System.SysUtils;

{ TControllerInvoker }

function TControllerInvoker.Add(Value: iCommand): iInvoker;
begin
  Result := Self;
  FList.Add(Value);

end;

constructor TControllerInvoker.Create;
begin
  FList := TList<iCommand>.Create;
end;

destructor TControllerInvoker.Destroy;
begin
  FreeAndNil(FList);
  inherited;
end;

function TControllerInvoker.Execute: iInvoker;
var
  I: Integer;
begin
  Result := Self;
  for I := 0 to Pred(FList.count) do
    FList[I].Execute;

end;

class function TControllerInvoker.New: iInvoker;
begin
  Result := Self.Create;
end;

end.
