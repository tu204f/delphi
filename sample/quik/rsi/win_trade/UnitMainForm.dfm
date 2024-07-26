object MainForm: TMainForm
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = #1054#1082#1085#1086' '#1087#1088#1080#1082#1072#1079#1072
  ClientHeight = 361
  ClientWidth = 378
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
  object ledQuickTableName: TLabeledEdit
    Left = 8
    Top = 20
    Width = 361
    Height = 21
    Color = clBtnFace
    EditLabel.Width = 154
    EditLabel.Height = 13
    EditLabel.Caption = #1058#1072#1073#1083#1080#1094#1072' '#1090#1088#1072#1085#1089#1083#1103#1094#1080#1080' '#1075#1088#1072#1092#1080#1082#1072':'
    ReadOnly = True
    TabOrder = 0
  end
  object ledClassCode: TLabeledEdit
    Left = 8
    Top = 64
    Width = 105
    Height = 21
    Color = clBtnFace
    EditLabel.Width = 101
    EditLabel.Height = 13
    EditLabel.Caption = #1050#1083#1072#1089#1089' '#1080#1085#1089#1090#1088#1091#1084#1077#1085#1090#1072':'
    ReadOnly = True
    TabOrder = 1
  end
  object ledSecCode: TLabeledEdit
    Left = 119
    Top = 64
    Width = 105
    Height = 21
    Color = clBtnFace
    EditLabel.Width = 92
    EditLabel.Height = 13
    EditLabel.Caption = #1050#1086#1076' '#1080#1085#1089#1090#1088#1091#1084#1077#1085#1090#1072':'
    ReadOnly = True
    TabOrder = 2
  end
  object ledTrdaccID: TLabeledEdit
    Left = 230
    Top = 64
    Width = 139
    Height = 21
    Color = clBtnFace
    EditLabel.Width = 79
    EditLabel.Height = 13
    EditLabel.Caption = #1058#1086#1088#1075#1086#1074#1099#1081' '#1089#1095#1077#1090':'
    ReadOnly = True
    TabOrder = 3
  end
  object LabeledEdit1: TLabeledEdit
    Left = 8
    Top = 108
    Width = 361
    Height = 21
    Color = clBtnFace
    EditLabel.Width = 117
    EditLabel.Height = 13
    EditLabel.Caption = #1055#1091#1090#1100' '#1082' '#1090#1077#1088#1084#1080#1085#1072#1083' QUIK:'
    ReadOnly = True
    TabOrder = 4
  end
  object LabeledEditPrice: TLabeledEdit
    Left = 8
    Top = 164
    Width = 121
    Height = 21
    EditLabel.Width = 26
    EditLabel.Height = 13
    EditLabel.Caption = #1062#1077#1085#1072
    TabOrder = 5
  end
  object LabeledEditQty: TLabeledEdit
    Left = 135
    Top = 164
    Width = 121
    Height = 21
    EditLabel.Width = 60
    EditLabel.Height = 13
    EditLabel.Caption = #1050#1086#1083#1080#1095#1077#1089#1090#1074#1086
    TabOrder = 6
  end
  object ButtonTrade: TButton
    Left = 8
    Top = 227
    Width = 75
    Height = 25
    Caption = #1055#1088#1080#1082#1072#1079
    TabOrder = 7
    OnClick = ButtonTradeClick
  end
  object RadioButtonBuy: TRadioButton
    Left = 8
    Top = 196
    Width = 113
    Height = 17
    Caption = #1055#1086#1082#1091#1087#1082#1072
    TabOrder = 8
  end
  object RadioButtonSell: TRadioButton
    Left = 135
    Top = 196
    Width = 113
    Height = 17
    Caption = #1055#1088#1086#1076#1072#1078#1072
    TabOrder = 9
  end
  object Memo: TMemo
    Left = 8
    Top = 258
    Width = 362
    Height = 89
    Color = clBtnFace
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 10
  end
end
