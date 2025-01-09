unit eNota.Controller.NotaFiscal.ImportarPedido;

interface

uses
  eNota.Controller.NotaFiscal.Interfaces;

Type
  TControllerNotaFiscalImportarPedido = class(TInterfacedObject, iCommand)
    private
      FParent : iNotaFiscal;
      FNumeroPedido : Integer;
    public
      constructor Create(Parent : iNotaFiscal; NumeroPedido : Integer);
      destructor Destroy; override;
      class function New(Parent : iNotaFiscal; NumeroPedido : Integer) : iCommand;
      function Execute : iCommand;
  end;

implementation

{ TControllerNotaFiscalImportarPedido }

constructor TControllerNotaFiscalImportarPedido.Create(Parent : iNotaFiscal; NumeroPedido : Integer);
begin
  FParent := Parent;
  FNumeroPedido := NumeroPedido;
end;

destructor TControllerNotaFiscalImportarPedido.Destroy;
begin

  inherited;
end;

function TControllerNotaFiscalImportarPedido.Execute: iCommand;
begin
  Result := Self;
  FParent.ImportarPedido(FNumeroPedido);
end;

class function TControllerNotaFiscalImportarPedido.New(Parent : iNotaFiscal; NumeroPedido : Integer) : iCommand;
begin
  Result := Self.Create(Parent, NumeroPedido);
end;

end.
