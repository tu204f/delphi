object ToolsForm: TToolsForm
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'ToolsForm'
  ClientHeight = 169
  ClientWidth = 377
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  FormStyle = fsStayOnTop
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
    Top = 135
    Width = 75
    Height = 25
    Caption = #1054#1090#1084#1077#1085#1072
    TabOrder = 1
    OnClick = ButtonCancelClick
  end
  object ButtonOK: TButton
    Left = 294
    Top = 135
    Width = 75
    Height = 25
    Caption = #1055#1088#1080#1085#1103#1090#1100
    TabOrder = 2
    OnClick = ButtonOKClick
  end
  object ledClassCode: TLabeledEdit
    Left = 8
    Top = 64
    Width = 105
    Height = 21
    EditLabel.Width = 101
    EditLabel.Height = 13
    EditLabel.Caption = #1050#1083#1072#1089#1089' '#1080#1085#1089#1090#1088#1091#1084#1077#1085#1090#1072':'
    TabOrder = 3
  end
  object ledSecCode: TLabeledEdit
    Left = 119
    Top = 64
    Width = 105
    Height = 21
    EditLabel.Width = 92
    EditLabel.Height = 13
    EditLabel.Caption = #1050#1086#1076' '#1080#1085#1089#1090#1088#1091#1084#1077#1085#1090#1072':'
    TabOrder = 4
  end
  object ledTrdaccID: TLabeledEdit
    Left = 230
    Top = 64
    Width = 139
    Height = 21
    EditLabel.Width = 79
    EditLabel.Height = 13
    EditLabel.Caption = #1058#1086#1088#1075#1086#1074#1099#1081' '#1089#1095#1077#1090':'
    TabOrder = 5
  end
  object LabeledEdit1: TLabeledEdit
    Left = 8
    Top = 108
    Width = 317
    Height = 21
    EditLabel.Width = 117
    EditLabel.Height = 13
    EditLabel.Caption = #1055#1091#1090#1100' '#1082' '#1090#1077#1088#1084#1080#1085#1072#1083' QUIK:'
    TabOrder = 6
  end
  object Button1: TButton
    Left = 328
    Top = 104
    Width = 41
    Height = 25
    Caption = '...'
    TabOrder = 7
    OnClick = Button1Click
  end
  object OpenDialog1: TOpenDialog
    Left = 104
    Top = 116
  end
end
