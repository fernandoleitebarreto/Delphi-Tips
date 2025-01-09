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
  Scaled = False
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 72
    Top = 112
    Width = 20
    Height = 13
    Caption = '-----'
  end
  object Label2: TLabel
    Left = 212
    Top = 112
    Width = 20
    Height = 13
    Caption = '-----'
  end
  object Label3: TLabel
    Left = 364
    Top = 112
    Width = 20
    Height = 13
    Caption = '-----'
  end
  object Label4: TLabel
    Left = 145
    Top = 24
    Width = 151
    Height = 23
    Caption = 'USANDO TASKS'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -19
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Button1: TButton
    Left = 40
    Top = 72
    Width = 75
    Height = 25
    Caption = 'Sem Thread'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 168
    Top = 72
    Width = 113
    Height = 25
    Caption = 'Com Thread'
    TabOrder = 1
    OnClick = Button2Click
  end
end
