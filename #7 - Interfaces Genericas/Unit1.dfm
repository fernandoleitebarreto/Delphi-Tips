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
    Left = 80
    Top = 32
    Width = 272
    Height = 18
    Caption = 'Trabalhando com Interface Gen'#233'rica'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -15
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object btnString: TButton
    Left = 96
    Top = 80
    Width = 75
    Height = 25
    Caption = 'String'
    TabOrder = 0
    OnClick = btnStringClick
  end
  object btnInteger: TButton
    Left = 240
    Top = 80
    Width = 75
    Height = 25
    Caption = 'Integer'
    TabOrder = 1
    OnClick = btnIntegerClick
  end
end
