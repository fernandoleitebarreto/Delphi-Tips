unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls;

type
  TForm1 = class(TForm)
    Edit1: TEdit;
    Memo1: TMemo;
    RadioGroup1: TRadioGroup;
    ComboBox1: TComboBox;
    Label1: TLabel;
    procedure Maiusculo_Nao_Usando_Absolute(Sender: TObject);
    procedure Maiusculo_Usando_Absolute(Sender: TObject);
    procedure OnExit(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.OnExit(Sender: TObject);
begin
  if RadioGroup1.ItemIndex = 0 then
    Maiusculo_Nao_Usando_Absolute(Sender)
  else
    Maiusculo_Usando_Absolute(Sender);
end;

procedure TForm1.Maiusculo_Nao_Usando_Absolute(Sender: TObject);
var
  Edit: TEdit;
  Memo: TMemo;
  ComboBox: TComboBox;
begin
  if Sender is TEdit then
  begin
    Edit := TEdit(Sender);
    Edit.Text := Uppercase(Edit.Text);
  end
  else if Sender is TMemo then
  begin
    Memo := TMemo(Sender);
    Memo.Text := Uppercase(Memo.Text);
  end
  else if (Sender is TComboBox) then
  begin
    ComboBox := TComboBox(Sender);
    if (ComboBox.Style = csDropDown) then
    begin
      ComboBox.Text := Uppercase(ComboBox.Text);
    end;
  end;

end;

procedure TForm1.Maiusculo_Usando_Absolute(Sender: TObject);
var
  Edit: TEdit absolute Sender;
  Memo: TMemo absolute Sender;
  ComboBox: TComboBox absolute Sender;
begin
  if Sender is TEdit then
  begin
    Edit.Text := Uppercase(Edit.Text);
  end
  else if Sender is TMemo then
  begin
    Memo.Text := Uppercase(Memo.Text);
  end
  else if (Sender is TComboBox) and (ComboBox.Style = csDropDown) then
  begin
    ComboBox.Text := Uppercase(ComboBox.Text);
  end;

end;

end.
