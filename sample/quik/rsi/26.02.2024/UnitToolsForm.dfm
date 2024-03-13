object ToolsForm: TToolsForm
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'ToolsForm'
  ClientHeight = 152
  ClientWidth = 377
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
  object ledQuickTableName: TLabeledEdit
    Left = 8
    Top = 20
    Width = 361
    Height = 21
    EditLabel.Width = 154
    EditLabel.Height = 13
    EditLabel.Caption = #1058#1072#1073#1083#1080#1094#1072' '#1090#1088#1072#1085#1089#1083#1103#1094#1080#1080' '#1075#1088#1072#1092#1080#1082#1072':'
    TabOrder = 0
  end
  object ButtonCancel: TButton
    Left = 213
    Top = 119
    Width = 75
    Height = 25
    Caption = #1054#1090#1084#1077#1085#1072
    TabOrder = 1
    OnClick = ButtonCancelClick
  end
  object ButtonOK: TButton
    Left = 294
    Top = 119
    Width = 75
    Height = 25
    Caption = #1055#1088#1080#1085#1103#1090#1100
    TabOrder = 2
    OnClick = ButtonOKClick
  end
  object ledClassCode: TLabeledEdit
    Left = 8
    Top = 64
    Width = 153
    Height = 21
    EditLabel.Width = 101
    EditLabel.Height = 13
    EditLabel.Caption = #1050#1083#1072#1089#1089' '#1080#1085#1089#1090#1088#1091#1084#1077#1085#1090#1072':'
    TabOrder = 3
  end
  object ledSecCode: TLabeledEdit
    Left = 180
    Top = 64
    Width = 153
    Height = 21
    EditLabel.Width = 92
    EditLabel.Height = 13
    EditLabel.Caption = #1050#1086#1076' '#1080#1085#1089#1090#1088#1091#1084#1077#1085#1090#1072':'
    TabOrder = 4
  end
end
