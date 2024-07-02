object AddOrderForm: TAddOrderForm
  Left = 0
  Top = 0
  Caption = #1044#1086#1073#1072#1074#1080#1090#1100' '#1079#1072#1103#1074#1082#1091
  ClientHeight = 259
  ClientWidth = 306
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object ButtonOK: TButton
    Left = 220
    Top = 227
    Width = 75
    Height = 25
    Caption = #1055#1088#1080#1085#1103#1090#1100
    TabOrder = 0
    OnClick = ButtonOKClick
  end
  object ButtonCancel: TButton
    Left = 139
    Top = 227
    Width = 75
    Height = 25
    Caption = #1054#1090#1084#1077#1085#1072
    TabOrder = 1
    OnClick = ButtonCancelClick
  end
  object ledSecCode: TLabeledEdit
    Left = 170
    Top = 8
    Width = 125
    Height = 21
    Color = clBtnFace
    EditLabel.Width = 61
    EditLabel.Height = 13
    EditLabel.Caption = #1050#1086#1076' '#1082#1083#1072#1089#1089#1072':'
    LabelPosition = lpLeft
    ReadOnly = True
    TabOrder = 2
  end
  object ledCodeClass: TLabeledEdit
    Left = 170
    Top = 35
    Width = 125
    Height = 21
    Color = clBtnFace
    EditLabel.Width = 92
    EditLabel.Height = 13
    EditLabel.Caption = #1050#1086#1076' '#1080#1085#1089#1090#1088#1091#1084#1077#1085#1090#1072':'
    LabelPosition = lpLeft
    TabOrder = 3
  end
  object ledQuantity: TLabeledEdit
    Left = 174
    Top = 62
    Width = 105
    Height = 21
    EditLabel.Width = 64
    EditLabel.Height = 13
    EditLabel.Caption = #1050#1086#1083#1080#1095#1077#1089#1090#1074#1086':'
    LabelPosition = lpLeft
    TabOrder = 4
    Text = '1'
  end
  object UpDownQuantity: TUpDown
    Left = 279
    Top = 62
    Width = 16
    Height = 21
    Associate = ledQuantity
    Min = 1
    Position = 1
    TabOrder = 5
  end
  object ledValueRSI: TLabeledEdit
    Left = 174
    Top = 89
    Width = 105
    Height = 21
    EditLabel.Width = 72
    EditLabel.Height = 13
    EditLabel.Caption = #1047#1085#1072#1095#1077#1085#1080#1077' RSI:'
    LabelPosition = lpLeft
    TabOrder = 6
    Text = '50'
  end
  object UpDownValueRSI: TUpDown
    Left = 279
    Top = 89
    Width = 16
    Height = 21
    Associate = ledValueRSI
    Position = 50
    TabOrder = 7
  end
  object RadioGroupDirection: TRadioGroup
    Left = 161
    Top = 152
    Width = 137
    Height = 69
    Caption = #1055#1077#1088#1077#1089#1077#1095#1077#1085#1080#1077' RSI'
    Items.Strings = (
      #1057#1074#1077#1088#1093#1091
      #1057#1085#1080#1079#1091)
    TabOrder = 8
  end
  object RadioGroupSide: TRadioGroup
    Left = 8
    Top = 148
    Width = 137
    Height = 69
    Caption = #1053#1072#1087#1088#1072#1074#1083#1077#1085#1080#1077
    Items.Strings = (
      #1055#1086#1082#1091#1087#1082#1072
      #1055#1088#1086#1076#1072#1078#1072)
    TabOrder = 9
  end
  object ledStepPrice: TLabeledEdit
    Left = 174
    Top = 117
    Width = 105
    Height = 21
    EditLabel.Width = 154
    EditLabel.Height = 13
    EditLabel.Caption = #1064#1072#1075' '#1086#1090#1089#1090#1091#1087#1072' '#1086#1090' '#1083#1091#1095#1096#1077#1081' '#1094#1077#1085#1099':'
    LabelPosition = lpLeft
    TabOrder = 10
    Text = '0'
  end
  object UpDownStepPrice: TUpDown
    Left = 279
    Top = 117
    Width = 16
    Height = 21
    Associate = ledStepPrice
    Min = -100
    TabOrder = 11
  end
end
