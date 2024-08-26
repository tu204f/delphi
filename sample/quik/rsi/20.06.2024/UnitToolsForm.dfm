object ToolsForm: TToolsForm
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'ToolsForm'
  ClientHeight = 317
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
  object Label1: TLabel
    Left = 8
    Top = 172
    Width = 69
    Height = 13
    Caption = #1042#1088#1077#1084#1103' '#1085#1072#1095#1072#1083#1086
  end
  object Label2: TLabel
    Left = 139
    Top = 172
    Width = 94
    Height = 13
    Caption = #1042#1088#1077#1084#1103' '#1079#1072#1074#1077#1088#1096#1077#1085#1080#1103
  end
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
    Top = 287
    Width = 75
    Height = 25
    Caption = #1054#1090#1084#1077#1085#1072
    TabOrder = 1
    OnClick = ButtonCancelClick
  end
  object ButtonOK: TButton
    Left = 294
    Top = 287
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
  object CheckBoxLogTrade: TCheckBox
    Left = 8
    Top = 140
    Width = 361
    Height = 17
    Caption = #1042#1082#1083#1102#1095#1080#1090#1100' '#1083#1086#1075#1080#1088#1086#1074#1072#1085#1080#1077' '#1090#1086#1088#1075#1086#1074#1099#1093' '#1087#1088#1080#1082#1072#1079#1086#1074
    TabOrder = 8
  end
  object TimePickerBegin: TTimePicker
    Left = 8
    Top = 191
    Width = 121
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Segoe UI'
    Font.Style = []
    TabOrder = 9
    Time = 0.416666666666666700
    TimeFormat = 'h:mm'
  end
  object TimePickerEnd: TTimePicker
    Left = 135
    Top = 191
    Width = 121
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Segoe UI'
    Font.Style = []
    TabOrder = 10
    Time = 0.750000000000000000
    TimeFormat = 'h:mm'
  end
  object LabeledEditTakeProfit: TLabeledEdit
    Left = 8
    Top = 256
    Width = 121
    Height = 21
    EditLabel.Width = 45
    EditLabel.Height = 13
    EditLabel.Caption = #1055#1088#1080#1073#1099#1083#1100
    Enabled = False
    HideSelection = False
    TabOrder = 11
  end
  object LabeledEditStopLoss: TLabeledEdit
    Left = 135
    Top = 256
    Width = 121
    Height = 21
    EditLabel.Width = 39
    EditLabel.Height = 13
    EditLabel.Caption = #1059#1073#1099#1090#1082#1072
    Enabled = False
    TabOrder = 12
  end
  object OpenDialog1: TOpenDialog
    Left = 104
    Top = 116
  end
end
