unit uMain.Controller.Interfaces;

interface

type

  iCommand = interface
    ['{163C94B3-23AB-4EF5-AE65-888647AB3577}']
    function Execute: iCommand;
  end;

  iInvoker = interface
    ['{D002E2D1-B6A3-4378-A927-FD80E7DE1ABD}']
    function Add(Value: iCommand): iInvoker;
    function Execute: iInvoker;
  end;

  iCleanCode = interface
    ['{D70852EE-624C-46B7-BBD7-9D16B0FD85EF}']
    function Creating: iCleanCode;
    function Validating: iCleanCode;
    function Saving: iCleanCode;
  end;

  iCleanCodeView = interface
    ['{90C416F7-C87E-46B6-8593-3AE75BA0FABC}']
    function Processar: iCleanCode;
    function ProcessarSemValidar: iCleanCode;
  end;

implementation

end.
