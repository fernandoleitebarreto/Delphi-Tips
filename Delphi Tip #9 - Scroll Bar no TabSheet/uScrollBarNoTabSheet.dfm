object FrmScrollBoxTabSheet: TFrmScrollBoxTabSheet
  Left = 0
  Top = 0
  Caption = 'Scroll Box no TabSheet em Tempo de Execu'#231#227'o'
  ClientHeight = 383
  ClientWidth = 613
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
  object cxPageControl1: TcxPageControl
    Left = 0
    Top = 0
    Width = 613
    Height = 383
    Align = alClient
    TabOrder = 0
    Properties.ActivePage = cxTabSheet1
    Properties.CustomButtons.Buttons = <>
    ExplicitLeft = 40
    ExplicitTop = 48
    ExplicitWidth = 289
    ExplicitHeight = 193
    ClientRectBottom = 379
    ClientRectLeft = 4
    ClientRectRight = 609
    ClientRectTop = 24
    object cxTabSheet1: TcxTabSheet
      Caption = 'cxTabSheet1'
      ImageIndex = 0
      ExplicitLeft = 5
      ExplicitTop = 25
      object cxGroupBox1: TcxGroupBox
        Left = -4
        Top = 288
        Caption = 'cxGroupBox1'
        TabOrder = 0
        Height = 105
        Width = 613
      end
    end
    object cxTabSheet2: TcxTabSheet
      Caption = 'cxTabSheet2'
      ImageIndex = 1
      ExplicitWidth = 281
      ExplicitHeight = 165
      object cxGroupBox2: TcxGroupBox
        Left = 0
        Top = 0
        Align = alClient
        Caption = 'cxGroupBox2'
        TabOrder = 0
        ExplicitLeft = 24
        ExplicitTop = 32
        ExplicitWidth = 185
        ExplicitHeight = 105
        Height = 355
        Width = 605
      end
    end
  end
end
