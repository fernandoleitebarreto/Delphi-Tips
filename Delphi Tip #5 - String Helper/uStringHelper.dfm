object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 270
  ClientWidth = 766
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
    Left = 328
    Top = 8
    Width = 127
    Height = 23
    Caption = 'String Helper'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -19
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Button1: TButton
    Left = 413
    Top = 37
    Width = 75
    Height = 25
    Caption = 'Processar'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Edit1: TEdit
    Left = 286
    Top = 39
    Width = 121
    Height = 21
    TabOrder = 1
    Text = 'Hello Delphi 10.3'
  end
  object mmOutPut: TMemo
    Left = 0
    Top = 66
    Width = 766
    Height = 204
    Align = alBottom
    TabOrder = 2
  end
end
