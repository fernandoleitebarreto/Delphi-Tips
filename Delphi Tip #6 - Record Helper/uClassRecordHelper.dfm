object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 201
  ClientWidth = 447
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 136
    Top = 16
    Width = 183
    Height = 19
    Caption = 'Class e Record Helper '
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object cbbCombo: TComboBox
    Left = 22
    Top = 80
    Width = 145
    Height = 21
    TabOrder = 1
    Text = 'Selecione'
  end
  object btnSelect: TButton
    Left = 342
    Top = 152
    Width = 75
    Height = 25
    Caption = 'Select'
    TabOrder = 2
    OnClick = btnSelectClick
  end
  object Memo: TMemo
    Left = 232
    Top = 57
    Width = 185
    Height = 89
    Lines.Strings = (
      'Memo')
    TabOrder = 0
  end
end
