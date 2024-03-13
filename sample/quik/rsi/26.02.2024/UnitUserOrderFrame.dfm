object UserOrderFrame: TUserOrderFrame
  Left = 0
  Top = 0
  Width = 243
  Height = 121
  TabOrder = 0
  object ledValueRSI: TLabeledEdit
    Left = 114
    Top = 37
    Width = 105
    Height = 21
    EditLabel.Width = 72
    EditLabel.Height = 13
    EditLabel.Caption = #1047#1085#1072#1095#1077#1085#1080#1077' RSI:'
    LabelPosition = lpLeft
    TabOrder = 0
    Text = '50'
  end
  object UpDownStepPrice: TUpDown
    Left = 219
    Top = 65
    Width = 16
    Height = 21
    Associate = ledStepPrice
    Min = -100
    TabOrder = 1
  end
  object ledStepPrice: TLabeledEdit
    Left = 114
    Top = 64
    Width = 105
    Height = 21
    EditLabel.Width = 16
    EditLabel.Height = 13
    EditLabel.Caption = 'Slip'
    LabelPosition = lpLeft
    TabOrder = 2
    Text = '0'
  end
  object UpDownValueRSI: TUpDown
    Left = 219
    Top = 37
    Width = 16
    Height = 21
    Associate = ledValueRSI
    Position = 50
    TabOrder = 3
  end
  object UpDownQuantity: TUpDown
    Left = 219
    Top = 10
    Width = 16
    Height = 21
    Associate = ledQuantity
    Min = 1
    Position = 1
    TabOrder = 4
  end
  object ledQuantity: TLabeledEdit
    Left = 114
    Top = 10
    Width = 105
    Height = 21
    EditLabel.Width = 64
    EditLabel.Height = 13
    EditLabel.Caption = #1050#1086#1083#1080#1095#1077#1089#1090#1074#1086':'
    LabelPosition = lpLeft
    TabOrder = 5
    Text = '1'
  end
  object CheckBoxActiveOrder: TCheckBox
    Left = 16
    Top = 96
    Width = 219
    Height = 17
    Caption = #1040#1082#1090#1080#1074#1085#1086#1077' '#1089#1086#1089#1090#1086#1103#1085#1080#1077
    TabOrder = 6
  end
end
