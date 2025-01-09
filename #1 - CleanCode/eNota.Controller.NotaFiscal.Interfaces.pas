unit eNota.Controller.NotaFiscal.Interfaces;

interface

type

  iCommand = interface
    ['{613DD4D8-507A-4FAA-875E-7210CC901269}']
    function Execute : iCommand;
  end;

  iInvoker = interface
    ['{092DC2EB-4081-4618-BCD4-938EEE757909}']
    function Add(Value : iCommand) : iInvoker;
    function Execute : iInvoker;
  end;

  iNotaFiscal = interface
    ['{FDE30B91-28FE-4FA4-AF98-68D9EDACA3A5}']
    function Criar : iNotaFiscal;
    function Validar : iNotaFiscal;
    function Enviar : iNotaFiscal;
    function Gravar : iNotaFiscal;
    function Email : iNotaFiscal;
    function ImportarPedido(Value : Integer) : iNotaFiscal;
  end;

  iNotaFiscalView = interface
    ['{7A362359-F610-49BF-824B-5ECD2D03CE05}']
    function EnviarNotaSefaz : iNotaFiscal;
    function EnviarNotaSefazSemEmail : iNotaFiscal;
  end;

implementation

end.
