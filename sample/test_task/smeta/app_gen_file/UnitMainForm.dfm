object MainForm: TMainForm
  Left = 0
  Top = 0
  Cursor = crHandPoint
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'MainForm'
  ClientHeight = 445
  ClientWidth = 709
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
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 149
    Height = 13
    Caption = #1056#1072#1079#1084#1077#1088' '#1075#1077#1085#1077#1088#1080#1088#1091#1077#1084#1086#1075#1086' '#1092#1072#1081#1083#1072
  end
  object Label2: TLabel
    Left = 8
    Top = 62
    Width = 156
    Height = 13
    Caption = #1057#1083#1086#1074#1072#1088#1100' '#1076#1083#1103' '#1075#1077#1085#1077#1088#1072#1094#1080#1080' '#1092#1072#1081#1083#1072
  end
  object LabelAdd: TLabel
    Left = 247
    Top = 62
    Width = 58
    Height = 13
    Cursor = crHandPoint
    Caption = #1044#1086#1073#1072#1074#1080#1090#1100
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clTeal
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold, fsUnderline]
    ParentFont = False
    OnClick = LabelAddClick
  end
  object LabelDelete: TLabel
    Left = 311
    Top = 62
    Width = 51
    Height = 13
    Cursor = crHandPoint
    Caption = #1059#1076#1072#1083#1080#1090#1100
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clTeal
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold, fsUnderline]
    ParentFont = False
    OnClick = LabelDeleteClick
  end
  object LabelClear: TLabel
    Left = 368
    Top = 62
    Width = 54
    Height = 13
    Cursor = crHandPoint
    Caption = #1054#1095#1080#1089#1090#1080#1090#1100
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clTeal
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold, fsUnderline]
    ParentFont = False
    OnClick = LabelClearClick
  end
  object LabelLoadRef: TLabel
    Left = 428
    Top = 62
    Width = 131
    Height = 13
    Cursor = crHandPoint
    Caption = #1047#1072#1075#1088#1091#1079#1080#1090#1100' '#1089#1087#1088#1072#1074#1086#1095#1085#1080#1082
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clTeal
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold, fsUnderline]
    ParentFont = False
    OnClick = LabelLoadRefClick
  end
  object LabelSaveRef: TLabel
    Left = 565
    Top = 62
    Width = 133
    Height = 13
    Cursor = crHandPoint
    Caption = #1057#1086#1093#1088#1072#1085#1080#1090#1100' '#1089#1087#1088#1072#1074#1086#1095#1085#1080#1082
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clTeal
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold, fsUnderline]
    ParentFont = False
    OnClick = LabelSaveRefClick
  end
  object Bevel: TBevel
    Left = 170
    Top = 8
    Width = 7
    Height = 40
    Shape = bsLeftLine
  end
  object Label3: TLabel
    Left = 183
    Top = 8
    Width = 140
    Height = 13
    Caption = #1055#1077#1088#1077#1076#1077#1083#1099' '#1075#1077#1085#1077#1088#1072#1094#1080#1080' '#1095#1080#1089#1083#1072
  end
  object Label4: TLabel
    Left = 183
    Top = 30
    Width = 12
    Height = 13
    Caption = #1090#1086
  end
  object Label5: TLabel
    Left = 291
    Top = 30
    Width = 12
    Height = 13
    Caption = #1087#1086
  end
  object EditSizeFile: TEdit
    Left = 8
    Top = 27
    Width = 89
    Height = 21
    NumbersOnly = True
    TabOrder = 0
    Text = '1'
  end
  object UpDownSizeFile: TUpDown
    Left = 98
    Top = 27
    Width = 16
    Height = 21
    Associate = EditSizeFile
    Min = 1
    Max = 1000000
    Position = 1
    TabOrder = 1
  end
  object ComboBoxTypeSizeFile: TComboBox
    Left = 120
    Top = 27
    Width = 40
    Height = 21
    Style = csDropDownList
    ItemIndex = 0
    TabOrder = 2
    Text = 'Mb'
    OnChange = ComboBoxTypeSizeFileChange
    Items.Strings = (
      'Mb'
      'Gb')
  end
  object ListBox: TListBox
    Left = 8
    Top = 81
    Width = 693
    Height = 357
    TabStop = False
    ItemHeight = 13
    TabOrder = 3
  end
  object EditForm: TEdit
    Left = 201
    Top = 27
    Width = 68
    Height = 21
    NumbersOnly = True
    TabOrder = 4
    Text = '1'
  end
  object EditTo: TEdit
    Left = 309
    Top = 27
    Width = 68
    Height = 21
    NumbersOnly = True
    TabOrder = 5
    Text = '99'#160'999'
  end
  object UpDownForm: TUpDown
    Left = 269
    Top = 27
    Width = 16
    Height = 21
    Associate = EditForm
    Min = 1
    Max = 99999
    Position = 1
    TabOrder = 6
  end
  object UpDownTo: TUpDown
    Left = 377
    Top = 27
    Width = 16
    Height = 21
    Associate = EditTo
    Min = 1
    Max = 99999
    Position = 99999
    TabOrder = 7
  end
  object ButtonStart: TButton
    Left = 614
    Top = 8
    Width = 87
    Height = 40
    Caption = #1057#1090#1072#1088#1090
    TabOrder = 8
    OnClick = ButtonStartClick
  end
  object PanelProgress: TPanel
    Left = 170
    Top = 184
    Width = 351
    Height = 93
    TabOrder = 9
    object LabelTitleProgress: TLabel
      Left = 18
      Top = 17
      Width = 87
      Height = 13
      Caption = 'LabelTitleProgress'
    end
    object ProgressBar: TProgressBar
      Left = 18
      Top = 36
      Width = 320
      Height = 17
      TabOrder = 0
    end
    object ButtonStop: TButton
      Left = 264
      Top = 59
      Width = 75
      Height = 25
      Caption = #1057#1090#1086#1087
      TabOrder = 1
      OnClick = ButtonStopClick
    end
  end
  object OpenDialog: TOpenDialog
    Filter = '|*.txt'
    Left = 60
    Top = 112
  end
  object SaveDialog: TSaveDialog
    Filter = '|*.txt'
    Left = 60
    Top = 180
  end
  object SaveDialogGen: TSaveDialog
    Filter = '|*.txt'
    Left = 60
    Top = 248
  end
  object Timer: TTimer
    Enabled = False
    Interval = 100
    OnTimer = TimerTimer
    Left = 556
    Top = 136
  end
end
