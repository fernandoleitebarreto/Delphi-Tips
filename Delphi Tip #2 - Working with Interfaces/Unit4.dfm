object Form4: TForm4
  Left = 0
  Top = 0
  Caption = 'Form4'
  ClientHeight = 211
  ClientWidth = 635
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object rgDispositivo: TRadioGroup
    Left = 342
    Top = 64
    Width = 105
    Height = 105
    Caption = 'Dispositivo'
    ItemIndex = 0
    Items.Strings = (
      'Lampada'
      'Ventilador')
    TabOrder = 2
  end
  object GroupBox1: TGroupBox
    Left = 104
    Top = 64
    Width = 113
    Height = 105
    Caption = 'M'#233'todo Antigo'
    TabOrder = 0
    object tgInterruptor_Velho: TToggleSwitch
      Left = 17
      Top = 53
      Width = 72
      Height = 20
      TabOrder = 0
      OnClick = tgInterruptor_VelhoClick
    end
  end
  object GroupBox2: TGroupBox
    Left = 223
    Top = 64
    Width = 113
    Height = 105
    Caption = 'M'#233'todo Novo'
    TabOrder = 1
    object tgInterruptor_Novo: TToggleSwitch
      Left = 25
      Top = 53
      Width = 72
      Height = 20
      TabOrder = 0
      OnClick = tgInterruptor_NovoClick
    end
  end
end
