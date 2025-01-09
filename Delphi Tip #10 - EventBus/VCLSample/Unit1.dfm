object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 282
  ClientWidth = 661
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Memo1: TMemo
    Left = 8
    Top = 8
    Width = 645
    Height = 225
    Lines.Strings = (
      'Memo1')
    TabOrder = 0
    OnChange = Memo1Change
  end
  object CheckBox1: TCheckBox
    Left = 8
    Top = 239
    Width = 97
    Height = 17
    Caption = 'CheckBox1'
    TabOrder = 1
    OnClick = CheckBox1Click
  end
  object Button1: TButton
    Left = 578
    Top = 239
    Width = 75
    Height = 25
    Caption = 'Button1'
    TabOrder = 2
    OnClick = Button1Click
  end
end
