unit uClassRecordHelper;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.StdCtrls;

type
  TSex = (unknown = 0, male, female);

  TSexHelper = record helper for TSex
    function AsByte: byte;
    function AsValue: String;
    function ToString: string;
    class procedure Popula_Combo(str: TStrings); static;
    class procedure SelectCase(str: TStrings; field: string); static;
  end;

  TForm1 = class(TForm)
    cbbCombo: TComboBox;
    Label1: TLabel;
    btnSelect: TButton;
    Memo: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure btnSelectClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}
{ TItemsHelper }

function TSexHelper.AsByte: byte;
begin
  Result := byte(Self);
end;

function TSexHelper.AsValue: String;
begin
  case Self of
    male:
      Result := 'M';
    female:
      Result := 'F';
  else
    Result := '';
  end;
end;

class procedure TSexHelper.Popula_Combo(str: TStrings);
var
  item: TSex;
begin
  str.Clear;
  for item := Low(TSex) to High(TSex) do
    if item <> unknown then
      str.Add(item.ToString);
end;

class procedure TSexHelper.SelectCase(str: TStrings; field: string);
var
  item: TSex;
begin
  str.Add('   case upper(' + field + ')');
  for item := Low(TSex) to High(TSex) do
  begin
    if item <> unknown then
    begin
      str.Add('when ''' + UpperCase(item.AsValue) + ''' then ''' + item.ToString + '''');
    end;
  end;
  str.Add(' end');
end;

function TSexHelper.ToString: string;
begin
  case Self of
    male:
      Result := 'Masculino';
    female:
      Result := 'Feminino';
  else
    Result := '';
  end;
end;

procedure TForm1.btnSelectClick(Sender: TObject);
begin
  Memo.Clear;
  Memo.Lines.Add('select');
  TSex.SelectCase(Memo.Lines, 'SEXO');

  // Memo.Lines.Add('   case upper(SEXO)');
  // Memo.Lines.Add('     when ''M'' then ''Masculino''');
  // Memo.Lines.Add('     when ''F'' then ''Feminino''');
  // Memo.Lines.Add('     else ''Unknown''');
  // Memo.Lines.Add('   end');

  Memo.Lines.Add('from TABELA');
  Memo.Lines.Add('where ID > 0');

end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  TSex.Popula_Combo(cbbCombo.Items);
end;

end.
