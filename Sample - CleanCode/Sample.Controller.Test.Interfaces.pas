unit Sample.Controller.Test.Interfaces;

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

  iTest = interface
    ['{FDE30B91-28FE-4FA4-AF98-68D9EDACA3A5}']
    function Initialize : iTest;
    function Validate : iTest;
    function Send : iTest;
    function Save : iTest;
    function Email : iTest;
    function Import(Value : Integer) : iTest;
  end;

  iTestView = interface
    ['{7A362359-F610-49BF-824B-5ECD2D03CE05}']
    function ImportValidateSaveSendEmail : iTest;
    function ImportValidateSave : iTest;
    function ImportValidateSave_WithoutInterface: iTest;
  end;

implementation

end.
