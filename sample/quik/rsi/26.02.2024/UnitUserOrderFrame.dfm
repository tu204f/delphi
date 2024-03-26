object UserOrderFrame: TUserOrderFrame
  Left = 0
  Top = 0
  Width = 122
  Height = 105
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -23
  Font.Name = 'Tahoma'
  Font.Style = [fsBold]
  ParentFont = False
  TabOrder = 0
  object ledValueRSI: TLabeledEdit
    Left = 52
    Top = 37
    Width = 40
    Height = 36
    EditLabel.Width = 51
    EditLabel.Height = 28
    EditLabel.Caption = 'RSI:'
    LabelPosition = lpLeft
    TabOrder = 0
    Text = '50'
  end
  object UpDownStepPrice: TUpDown
    Left = 92
    Top = 216
    Width = 26
    Height = 36
    Associate = ledStepPrice
    Min = -100
    Position = 5
    TabOrder = 1
  end
  object ledStepPrice: TLabeledEdit
    Left = 52
    Top = 216
    Width = 40
    Height = 36
    EditLabel.Width = 43
    EditLabel.Height = 28
    EditLabel.Caption = 'Slip'
    LabelPosition = lpLeft
    TabOrder = 2
    Text = '5'
  end
  object UpDownValueRSI: TUpDown
    Left = 92
    Top = 37
    Width = 26
    Height = 36
    Associate = ledValueRSI
    Position = 50
    TabOrder = 3
  end
  object UpDownQuantity: TUpDown
    Left = 92
    Top = 4
    Width = 26
    Height = 36
    Associate = ledQuantity
    Min = 1
    Position = 1
    TabOrder = 4
  end
  object ledQuantity: TLabeledEdit
    Left = 28
    Top = 4
    Width = 64
    Height = 36
    EditLabel.Width = 24
    EditLabel.Height = 28
    EditLabel.Caption = #1050':'
    LabelPosition = lpLeft
    TabOrder = 5
    Text = '1'
  end
  object CheckBoxActiveOrder: TCheckBox
    Left = 3
    Top = 79
    Width = 61
    Height = 17
    Caption = #1040#1082#1090#1080#1074#1085#1086#1077' '#1089#1086#1089#1090#1086#1103#1085#1080#1077
    TabOrder = 6
  end
  object CheckBoxAuto: TCheckBox
    Left = 79
    Top = 79
    Width = 34
    Height = 17
    Caption = 'Auto'
    TabOrder = 7
  end
  object Button1: TButton
    Left = 15
    Top = 173
    Width = 90
    Height = 37
    Caption = 'Button1'
    TabOrder = 8
    OnClick = Button1Click
  end
  object Timer: TTimer
    Enabled = False
    OnTimer = TimerTimer
    Left = 31
    Top = 40
  end
end
