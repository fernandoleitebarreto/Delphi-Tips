unit uStringHelper;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Edit1: TEdit;
    mmOutPut: TMemo;
    Label1: TLabel;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
var
  s, s1: string;
begin
  try
    // Length property
    s := Edit1.Text;
    mmOutPut.Lines.Clear;
    mmOutPut.Lines.Add(Format('O tamanho da string é %d', [s.Length]));
    mmOutPut.Lines.Add('');
    // function Contains
    if s.Contains('Delphi') then
      mmOutPut.Lines.Add(Format('A string "%s" contem a string "%s"',
        [s, 'Delphi']));
    mmOutPut.Lines.Add('');
    // function EndsWith
    if s.EndsWith('10.3') then
      mmOutPut.Lines.Add(Format('A string "%s" termina com a string "%s"', [s, '10.3']));
    mmOutPut.Lines.Add('');
    // function ToLower
    mmOutPut.Lines.Add(Format('Usando ToLower fica "%s"', [s.ToLower]));
    mmOutPut.Lines.Add('');
    // function ToUpper
    mmOutPut.Lines.Add(Format('Usando ToUpper fica "%s"', [s.ToUpper]));
    mmOutPut.Lines.Add('');
    // function IndexOf
    mmOutPut.Lines.Add(Format('O index do H é "%d"', [s.IndexOf('H')]));
    // the value is based in a zero index
    mmOutPut.Lines.Add('');
    // function LastDelimiter
    mmOutPut.Lines.Add(Format('A primeira ocorrência de algum desses caracteres (abcdef) é "%d"',
      [s.IndexOfAny(['a', 'b', 'c', 'd', 'e', 'f'])]));
    // the value is based in a zero index
    mmOutPut.Lines.Add('');
    // function LastDelimiter
    mmOutPut.Lines.Add(Format('A última ocorrência de algum desses caracteres (abcdef)  é "%d"',
      [s.LastDelimiter('abcdef')])); // the value is based in a zero index
    mmOutPut.Lines.Add('');
    // function Remove
    mmOutPut.Lines.Add(Format('Os primeiros 5 caracteres da string são "%s"', [s.Remove(5)]
      )); // the value is based in a zero index
    mmOutPut.Lines.Add('');
    // function Replace
    mmOutPut.Lines.Add(Format('Trocando espaço em branco por "-" fica: %s',
      [s.Replace(' ', '-')])); // the value is based in a zero index
    mmOutPut.Lines.Add('');
    // function split
    mmOutPut.Lines.Add('Testando a função split:');
    for s1 in s.Split([' ']) do
      mmOutPut.Lines.Add(s1);
    mmOutPut.Lines.Add('');
    // function Substring
    mmOutPut.Lines.Add(Format('A sub string começando pela posição 6 é "%s"',
      [s.Substring(6)])); // the value is based in a zero index
  except
    on E: Exception do
      ShowMessage(E.ClassName + ': ' + E.Message);
  end;

end;

end.
