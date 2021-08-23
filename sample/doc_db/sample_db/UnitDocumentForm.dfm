object DocumentForm: TDocumentForm
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'DocumentForm'
  ClientHeight = 501
  ClientWidth = 873
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnCreate = FormCreate
  OnShow = FormShow
  DesignSize = (
    873
    501)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 396
    Top = 104
    Width = 115
    Height = 13
    Caption = #1052#1085#1086#1075#1086' '#1089#1090#1088#1086#1095#1085#1099#1081' '#1090#1077#1082#1089#1090
  end
  object ButtonWrite: TButton
    Left = 8
    Top = 468
    Width = 113
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = #1057#1086#1093#1088#1072#1085#1080#1090#1100' '#1079#1072#1087#1080#1089#1100
    TabOrder = 0
    OnClick = ButtonWriteClick
  end
  object SGrid: TStringGrid
    Left = 8
    Top = 8
    Width = 370
    Height = 455
    ColCount = 4
    DefaultColWidth = 85
    FixedCols = 0
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goColSizing, goRowSelect]
    TabOrder = 1
    ColWidths = (
      85
      85
      85
      85)
    RowHeights = (
      24
      24
      24
      24
      24)
  end
  object LabeledEdit1: TLabeledEdit
    Left = 396
    Top = 68
    Width = 389
    Height = 21
    EditLabel.Width = 103
    EditLabel.Height = 13
    EditLabel.Caption = #1058#1077#1082#1089#1090#1086#1074#1086#1077' '#1079#1072#1085#1095#1077#1085#1080#1077
    TabOrder = 2
    Text = #1060#1077#1076#1086#1088' '#1058#1102#1090#1095#1077#1074' '#8212' '#1042#1077#1089#1077#1085#1085#1080#1077' '#1074#1086#1076#1099
  end
  object ButtonAddField1: TButton
    Left = 791
    Top = 66
    Width = 75
    Height = 25
    Caption = #1044#1086#1073#1072#1074#1080#1090#1100
    TabOrder = 3
    OnClick = ButtonAddField1Click
  end
  object Memo: TMemo
    Left = 396
    Top = 123
    Width = 389
    Height = 98
    Lines.Strings = (
      #1045#1097#1077' '#1074' '#1087#1086#1083#1103#1093' '#1073#1077#1083#1077#1077#1090' '#1089#1085#1077#1075','
      #1040' '#1074#1086#1076#1099' '#1091#1078' '#1074#1077#1089#1085#1086#1081' '#1096#1091#1084#1103#1090' '#8212
      #1041#1077#1075#1091#1090' '#1080' '#1073#1091#1076#1103#1090' '#1089#1086#1085#1085#1099#1081' '#1073#1088#1077#1075','
      #1041#1077#1075#1091#1090', '#1080' '#1073#1083#1077#1097#1091#1090', '#1080' '#1075#1083#1072#1089#1103#1090#8230
      ''
      #1054#1085#1080' '#1075#1083#1072#1089#1103#1090' '#1074#1086' '#1074#1089#1077' '#1082#1086#1085#1094#1099':'
      #171#1042#1077#1089#1085#1072' '#1080#1076#1077#1090', '#1074#1077#1089#1085#1072' '#1080#1076#1077#1090','
      #1052#1099' '#1084#1086#1083#1086#1076#1086#1081' '#1074#1077#1089#1085#1099' '#1075#1086#1085#1094#1099','
      #1054#1085#1072' '#1085#1072#1089' '#1074#1099#1089#1083#1072#1083#1072' '#1074#1087#1077#1088#1077#1076'!'
      ''
      #1042#1077#1089#1085#1072' '#1080#1076#1077#1090', '#1074#1077#1089#1085#1072' '#1080#1076#1077#1090','
      #1048' '#1090#1080#1093#1080#1093', '#1090#1077#1087#1083#1099#1093' '#1084#1072#1081#1089#1082#1080#1093' '#1076#1085#1077#1081
      #1056#1091#1084#1103#1085#1099#1081', '#1089#1074#1077#1090#1083#1099#1081' '#1093#1086#1088#1086#1074#1086#1076
      #1058#1086#1083#1087#1080#1090#1089#1103' '#1074#1077#1089#1077#1083#1086' '#1079#1072' '#1085#1077#1081'!..')
    TabOrder = 4
  end
  object ButtonAddField2: TButton
    Left = 791
    Top = 121
    Width = 75
    Height = 25
    Caption = #1044#1086#1073#1072#1074#1080#1090#1100
    TabOrder = 5
    OnClick = ButtonAddField2Click
  end
  object DateTimePicker1: TDateTimePicker
    Left = 396
    Top = 232
    Width = 159
    Height = 25
    Date = 44334.000000000000000000
    Time = 0.365222060187079500
    DateFormat = dfLong
    TabOrder = 6
  end
  object DateTimePicker2: TDateTimePicker
    Left = 642
    Top = 232
    Width = 72
    Height = 25
    Date = 44334.000000000000000000
    Time = 0.365222060187079500
    Kind = dtkTime
    TabOrder = 7
  end
  object ButtonAddField3: TButton
    Left = 561
    Top = 232
    Width = 75
    Height = 25
    Caption = #1044#1086#1073#1072#1074#1080#1090#1100
    TabOrder = 8
    OnClick = ButtonAddField3Click
  end
  object ButtonAddField4: TButton
    Left = 720
    Top = 232
    Width = 75
    Height = 25
    Caption = #1044#1086#1073#1072#1074#1080#1090#1100
    TabOrder = 9
    OnClick = ButtonAddField4Click
  end
  object DateTimePicker3: TDateTimePicker
    Left = 396
    Top = 263
    Width = 159
    Height = 25
    Date = 44334.000000000000000000
    Time = 0.365222060187079500
    DateFormat = dfLong
    TabOrder = 10
  end
  object DateTimePicker4: TDateTimePicker
    Left = 642
    Top = 263
    Width = 72
    Height = 25
    Date = 44334.000000000000000000
    Time = 0.365222060187079500
    Kind = dtkTime
    TabOrder = 11
  end
  object ButtonAddField5: TButton
    Left = 720
    Top = 263
    Width = 75
    Height = 25
    Caption = #1044#1086#1073#1072#1074#1080#1090#1100
    TabOrder = 12
    OnClick = ButtonAddField5Click
  end
  object LabeledEdit2: TLabeledEdit
    Left = 396
    Top = 320
    Width = 373
    Height = 21
    EditLabel.Width = 107
    EditLabel.Height = 13
    EditLabel.Caption = #1063#1080#1089#1083#1086#1074#1086#1077' '#1079#1085#1072#1095#1077#1085#1080#1077' 1'
    TabOrder = 13
    Text = '59'#160'876'
  end
  object ButtonAddField6: TButton
    Left = 791
    Top = 318
    Width = 75
    Height = 25
    Caption = #1044#1086#1073#1072#1074#1080#1090#1100
    TabOrder = 14
    OnClick = ButtonAddField6Click
  end
  object LabeledEdit3: TLabeledEdit
    Left = 396
    Top = 364
    Width = 389
    Height = 21
    EditLabel.Width = 107
    EditLabel.Height = 13
    EditLabel.Caption = #1063#1080#1089#1083#1086#1074#1086#1077' '#1079#1085#1072#1095#1077#1085#1080#1077' 2'
    TabOrder = 15
    Text = '145.98'
  end
  object ButtonAddField7: TButton
    Left = 791
    Top = 362
    Width = 75
    Height = 25
    Caption = #1044#1086#1073#1072#1074#1080#1090#1100
    TabOrder = 16
    OnClick = ButtonAddField7Click
  end
  object ButtonAddField8: TButton
    Left = 791
    Top = 394
    Width = 75
    Height = 25
    Caption = #1044#1086#1073#1072#1074#1080#1090#1100
    TabOrder = 17
    OnClick = ButtonAddField8Click
  end
  object CheckBox1: TCheckBox
    Left = 396
    Top = 398
    Width = 389
    Height = 17
    Caption = #1051#1086#1075#1080#1095#1077#1089#1082#1086#1077' '#1079#1085#1072#1095#1077#1085#1080#1077' '
    TabOrder = 18
  end
  object LabeledEdit4: TLabeledEdit
    Left = 396
    Top = 440
    Width = 389
    Height = 21
    EditLabel.Width = 57
    EditLabel.Height = 13
    EditLabel.Caption = #1060#1072#1081#1083' '#1087#1091#1090#1080':'
    ReadOnly = True
    TabOrder = 19
  end
  object ButtonAddField9: TButton
    Left = 791
    Top = 438
    Width = 75
    Height = 25
    Caption = #1044#1086#1073#1072#1074#1080#1090#1100
    TabOrder = 20
    OnClick = ButtonAddField9Click
  end
  object UpDown1: TUpDown
    Left = 769
    Top = 320
    Width = 16
    Height = 21
    Associate = LabeledEdit2
    Max = 65000
    Position = 59876
    TabOrder = 21
  end
  object LabeledEdit5: TLabeledEdit
    Left = 392
    Top = 24
    Width = 393
    Height = 21
    EditLabel.Width = 100
    EditLabel.Height = 13
    EditLabel.Caption = #1053#1072#1080#1084#1077#1085#1086#1074#1072#1085#1080#1077' '#1087#1086#1083#1077
    TabOrder = 22
    Text = 'field_1'
  end
  object OpenDialog: TOpenDialog
    Left = 324
    Top = 220
  end
end
