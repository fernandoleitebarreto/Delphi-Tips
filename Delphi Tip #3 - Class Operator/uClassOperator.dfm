object frmClassOperator: TfrmClassOperator
  Left = 0
  Top = 0
  Caption = 'Class Operator'
  ClientHeight = 315
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
    Left = 62
    Top = 8
    Width = 343
    Height = 25
    Caption = 'Trabalhando com Class Operator'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -21
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Button1: TButton
    Left = 168
    Top = 40
    Width = 107
    Height = 25
    Caption = 'Sem Class Operator'
    TabOrder = 0
    OnClick = Button1Click
  end
  object btnImplicit: TButton
    Left = 168
    Top = 95
    Width = 107
    Height = 25
    Caption = 'Implicit (string)'
    TabOrder = 1
    OnClick = btnImplicitClick
  end
  object btnAdd: TButton
    Left = 168
    Top = 68
    Width = 107
    Height = 25
    Caption = 'Add'
    TabOrder = 2
    OnClick = btnAddClick
  end
  object btnImplicit_Integer: TButton
    Left = 168
    Top = 126
    Width = 107
    Height = 25
    Caption = 'Implicit (integer)'
    TabOrder = 3
    OnClick = btnImplicit_IntegerClick
  end
  object btnPositive: TButton
    Left = 168
    Top = 189
    Width = 107
    Height = 25
    Caption = 'Positive'
    TabOrder = 4
    OnClick = btnPositiveClick
  end
  object btnNegative: TButton
    Left = 168
    Top = 220
    Width = 107
    Height = 25
    Caption = 'Negative'
    TabOrder = 5
    OnClick = btnNegativeClick
  end
  object btnInc: TButton
    Left = 168
    Top = 251
    Width = 107
    Height = 25
    Caption = 'Inc'
    TabOrder = 6
    OnClick = btnIncClick
  end
  object btnEqual: TButton
    Left = 168
    Top = 282
    Width = 107
    Height = 25
    Caption = 'Equal'
    TabOrder = 7
    OnClick = btnEqualClick
  end
  object btnImplicit_ShowMessage: TButton
    Left = 168
    Top = 158
    Width = 107
    Height = 25
    Caption = 'Implicit (Show M.)'
    TabOrder = 8
    OnClick = btnImplicit_ShowMessageClick
  end
end
