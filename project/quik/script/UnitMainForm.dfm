object ScriptForm: TScriptForm
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'ScriptForm'
  ClientHeight = 200
  ClientWidth = 388
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object ButtonCreateScript: TButton
    Left = 246
    Top = 170
    Width = 134
    Height = 25
    Caption = #1057#1086#1079#1076#1072#1090#1100' '#1089#1082#1088#1080#1087#1090
    TabOrder = 0
    OnClick = ButtonCreateScriptClick
  end
  object ledTitli: TLabeledEdit
    Left = 136
    Top = 8
    Width = 246
    Height = 21
    EditLabel.Width = 117
    EditLabel.Height = 13
    EditLabel.Caption = #1053#1072#1080#1084#1077#1085#1086#1074#1072#1085#1080#1077' '#1089#1082#1088#1080#1087#1090#1072
    LabelPosition = lpLeft
    TabOrder = 1
  end
  object ledDescription: TLabeledEdit
    Left = 136
    Top = 35
    Width = 246
    Height = 21
    EditLabel.Width = 93
    EditLabel.Height = 13
    EditLabel.Caption = #1054#1087#1080#1089#1072#1085#1080#1077' '#1089#1082#1088#1080#1087#1090#1072
    LabelPosition = lpLeft
    TabOrder = 2
  end
  object ledClassCode: TLabeledEdit
    Left = 134
    Top = 62
    Width = 246
    Height = 21
    EditLabel.Width = 57
    EditLabel.Height = 13
    EditLabel.Caption = #1050#1086#1076' '#1082#1083#1072#1089#1089#1072
    LabelPosition = lpLeft
    TabOrder = 3
  end
  object ledSecCode: TLabeledEdit
    Left = 136
    Top = 89
    Width = 246
    Height = 21
    EditLabel.Width = 88
    EditLabel.Height = 13
    EditLabel.Caption = #1050#1086#1076' '#1080#1085#1089#1090#1088#1091#1084#1077#1085#1090#1072
    LabelPosition = lpLeft
    TabOrder = 4
  end
  object cbInterval: TComboBox
    Left = 136
    Top = 116
    Width = 244
    Height = 21
    Style = csDropDownList
    TabOrder = 5
    Items.Strings = (
      '1 '#1084#1080#1085#1091#1090#1082#1072
      '2 '#1084#1080#1085#1091#1090#1099' '
      '3 '#1084#1080#1085#1091#1090#1099
      '4 '#1084#1080#1085#1091#1090#1099
      '5 '#1084#1080#1085#1091#1090#1099
      '6 '#1084#1080#1085#1091#1090#1099
      '10 '#1084#1080#1085#1091#1090#1099
      '15 '#1084#1080#1085#1091#1090#1099
      '20 '#1084#1080#1085#1091#1090#1099
      '30 '#1084#1080#1085#1091#1090#1099
      '60 '#1084#1080#1085#1091#1090#1099
      '2 '#1095#1072#1089#1072
      '4 '#1095#1072#1089#1072)
  end
  object ledCountBar: TLabeledEdit
    Left = 136
    Top = 143
    Width = 229
    Height = 21
    EditLabel.Width = 93
    EditLabel.Height = 13
    EditLabel.Caption = #1050#1086#1083#1080#1095#1077#1089#1090#1074#1086' '#1073#1072#1088#1086#1074
    LabelPosition = lpLeft
    TabOrder = 6
    Text = '20'
  end
  object UpDown: TUpDown
    Left = 365
    Top = 143
    Width = 16
    Height = 21
    Associate = ledCountBar
    Min = 1
    Increment = 20
    Position = 20
    TabOrder = 7
  end
  object SaveDialog: TSaveDialog
    Filter = '|*.qpl'
    Left = 240
    Top = 36
  end
end
