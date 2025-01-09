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
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 88
    Top = 8
    Width = 279
    Height = 25
    Caption = 'Trabalhando com Absolute'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -21
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Edit1: TEdit
    Left = 144
    Top = 40
    Width = 121
    Height = 21
    TabOrder = 0
    Text = 'Edit1'
    OnExit = OnExit
  end
  object Memo1: TMemo
    Left = 144
    Top = 67
    Width = 185
    Height = 89
    Lines.Strings = (
      'Linha 1'
      'Linha 2'
      'Linha 3'
      'Linha 4'
      'Linha 5'
      'Linha 6')
    TabOrder = 1
    OnExit = OnExit
  end
  object RadioGroup1: TRadioGroup
    Left = 32
    Top = 51
    Width = 74
    Height = 105
    ItemIndex = 0
    Items.Strings = (
      'Normal'
      'Absolute')
    TabOrder = 2
  end
  object ComboBox1: TComboBox
    Left = 144
    Top = 162
    Width = 145
    Height = 21
    TabOrder = 3
    Text = 'Item 1'
    OnExit = OnExit
  end
end
