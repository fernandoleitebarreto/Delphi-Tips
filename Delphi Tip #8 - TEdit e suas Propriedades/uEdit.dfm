object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 259
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
  object Edit1: TEdit
    Left = 48
    Top = 56
    Width = 121
    Height = 21
    TabOrder = 0
    Text = 'Edit1'
  end
  object btnSelStart: TButton
    Left = 94
    Top = 94
    Width = 75
    Height = 25
    Caption = 'Com SelStart'
    TabOrder = 2
    OnClick = btnSelStartClick
  end
  object Edit2: TEdit
    Left = 240
    Top = 56
    Width = 121
    Height = 21
    TabOrder = 1
    Text = 'Edit2'
  end
  object Button2: TButton
    Left = 286
    Top = 94
    Width = 75
    Height = 25
    Caption = 'Sem SelStart'
    TabOrder = 3
    OnClick = Button2Click
  end
  object btnSetSelTextBuf: TButton
    Left = 94
    Top = 125
    Width = 75
    Height = 25
    Caption = 'SelTextBuf '
    TabOrder = 4
    OnClick = btnSetSelTextBufClick
  end
  object btnSetSelText: TButton
    Left = 94
    Top = 156
    Width = 75
    Height = 25
    Caption = 'SetSelText'
    TabOrder = 5
    OnClick = btnSetSelTextClick
  end
  object btnSetBounds: TButton
    Left = 94
    Top = 187
    Width = 75
    Height = 25
    Caption = 'SetBounds'
    TabOrder = 6
    OnClick = btnSetBoundsClick
  end
end
