object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'TDictionary'
  ClientHeight = 394
  ClientWidth = 855
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 683
    Top = 30
    Width = 19
    Height = 13
    Caption = 'City'
  end
  object Label2: TLabel
    Left = 683
    Top = 75
    Width = 39
    Height = 13
    Caption = 'Country'
  end
  object Label3: TLabel
    Left = 683
    Top = 123
    Width = 39
    Height = 13
    Caption = 'Latitude'
  end
  object Label4: TLabel
    Left = 683
    Top = 171
    Width = 47
    Height = 13
    Caption = 'Longitude'
  end
  object Label5: TLabel
    Left = 710
    Top = 5
    Width = 94
    Height = 19
    Caption = 'TDictionary'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Memo1: TMemo
    Left = 0
    Top = 0
    Width = 353
    Height = 394
    Align = alLeft
    TabOrder = 0
  end
  object Memo2: TMemo
    Left = 353
    Top = 0
    Width = 292
    Height = 394
    Align = alLeft
    TabOrder = 1
  end
  object btnAdd: TButton
    Left = 655
    Top = 241
    Width = 100
    Height = 25
    Caption = 'Add City'
    TabOrder = 6
    OnClick = btnAddClick
  end
  object edCity: TEdit
    Left = 683
    Top = 48
    Width = 121
    Height = 21
    TabOrder = 2
  end
  object edCountry: TEdit
    Left = 683
    Top = 96
    Width = 121
    Height = 21
    TabOrder = 3
  end
  object btnPrintDictionary: TButton
    Left = 655
    Top = 217
    Width = 100
    Height = 25
    Caption = 'Print Dictionary'
    TabOrder = 4
    OnClick = btnPrintDictionaryClick
  end
  object btnCount: TButton
    Left = 655
    Top = 264
    Width = 100
    Height = 25
    Caption = 'Count'
    TabOrder = 8
    OnClick = btnCountClick
  end
  object btnTryGetValue: TButton
    Left = 655
    Top = 287
    Width = 100
    Height = 25
    Caption = 'TryGetValue'
    TabOrder = 11
    OnClick = btnTryGetValueClick
  end
  object btnRemove: TButton
    Left = 754
    Top = 217
    Width = 100
    Height = 25
    Caption = 'Remove'
    TabOrder = 5
    OnClick = btnRemoveClick
  end
  object btnTrimExcess: TButton
    Left = 754
    Top = 241
    Width = 100
    Height = 25
    Caption = 'TrimExcess'
    TabOrder = 7
    OnClick = btnTrimExcessClick
  end
  object btnContainsKey: TButton
    Left = 754
    Top = 264
    Width = 100
    Height = 25
    Caption = 'ContainsKey'
    TabOrder = 9
    OnClick = btnContainsKeyClick
  end
  object btnContainsValue: TButton
    Left = 754
    Top = 287
    Width = 100
    Height = 25
    Caption = 'ContainsValue'
    TabOrder = 13
    OnClick = btnContainsValueClick
  end
  object btnAddOrSetValue: TButton
    Left = 654
    Top = 310
    Width = 100
    Height = 25
    Caption = 'AddOrSetValue'
    TabOrder = 10
    OnClick = btnAddOrSetValueClick
  end
  object btnClear: TButton
    Left = 754
    Top = 310
    Width = 100
    Height = 25
    Caption = 'Clear'
    TabOrder = 12
    OnClick = btnClearClick
  end
  object edLatitude: TEdit
    Left = 683
    Top = 137
    Width = 121
    Height = 21
    NumbersOnly = True
    TabOrder = 14
  end
  object edLongitude: TEdit
    Left = 683
    Top = 184
    Width = 121
    Height = 21
    NumbersOnly = True
    TabOrder = 15
  end
end
