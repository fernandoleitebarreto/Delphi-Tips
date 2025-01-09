unit uThreadVar;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, System.Threading,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TForm1 = class(TForm)
    Memo1: TMemo;
    Button1: TButton;
    Memo2: TMemo;
    Memo3: TMemo;
    Memo4: TMemo;
    Button2: TButton;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

  threadvar MyThreadVar: Integer;
  // threadvar MyCount: Integer;

implementation

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
var
  tasks: array of ITask;
  INumRegPerTask: Integer;
  FIni, FFim: Integer;
  ICountTasks: Integer;
  SLArray : array of TStringList;

begin

  TTask.Run(
    procedure
    begin

      INumRegPerTask := 1000;
      ICountTasks := 100;
      SetLength(SLArray, ICountTasks);
      SetLength(tasks, ICountTasks);


      TParallel.For(0, ICountTasks - 1,
        procedure(Index: Integer)
        begin

          tasks[Index] := TTask.Run(
            procedure
            var
              K: Integer;
            begin

              FIni := (INumRegPerTask * (Index + 1)) - INumRegPerTask;
              FFim := (INumRegPerTask * (Index + 1)) - 1;

              SLArray[Index] := TStringList.Create;
              for K := FIni to FFim do
              begin
                SLArray[Index].Add(K.ToString);

              end;
            end);

        end);


      TTask.Run(
        procedure
        begin
          TTask.WaitForAll(tasks);
          TThread.Synchronize(TThread.CurrentThread,
            procedure
            var
              I: Integer;
            begin
              for I := 0 to ICountTasks - 1 do
              begin
                Memo1.Lines.AddStrings(SLArray[I]);
                FreeAndNil(SLArray[I]);
              end;
            end);
        end);
    end);
end;

end.
