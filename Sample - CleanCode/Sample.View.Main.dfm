object Form2: TForm2
  Left = 0
  Top = 0
  Caption = 'Form2'
  ClientHeight = 250
  ClientWidth = 409
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  TextHeight = 13
  object Button1: TButton
    Left = 40
    Top = 66
    Width = 345
    Height = 25
    Caption = 'ImportValidateSaveSend'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 40
    Top = 100
    Width = 345
    Height = 25
    Caption = 'ImportValidateSaveSendEmail'
    TabOrder = 1
    OnClick = Button2Click
  end
  object Button3: TButton
    Left = 40
    Top = 136
    Width = 345
    Height = 25
    Caption = 'ImportValidateSaveSend_WithoutInterface'
    TabOrder = 2
    OnClick = Button3Click
  end
end
