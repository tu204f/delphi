object MainForm: TMainForm
  Left = 0
  Top = 0
  Caption = 'MainForm'
  ClientHeight = 408
  ClientWidth = 875
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 17
    Top = 41
    Width = 119
    Height = 13
    Caption = #1050#1086#1083#1080#1095#1077#1089#1090#1074#1086' '#1082#1086#1083#1086#1085#1086#1082': N'
  end
  object Label2: TLabel
    Left = 29
    Top = 68
    Width = 107
    Height = 13
    Caption = #1050#1086#1083#1080#1095#1077#1089#1090#1074#1086' '#1089#1090#1088#1086#1082': M'
  end
  object Label3: TLabel
    Left = 289
    Top = 68
    Width = 107
    Height = 13
    Caption = #1050#1086#1083#1080#1095#1077#1089#1090#1074#1086' '#1089#1090#1088#1086#1082': M'
  end
  object Label4: TLabel
    Left = 277
    Top = 41
    Width = 119
    Height = 13
    Caption = #1050#1086#1083#1080#1095#1077#1089#1090#1074#1086' '#1082#1086#1083#1086#1085#1086#1082': N'
  end
  object Button1: TButton
    Left = 8
    Top = 8
    Width = 249
    Height = 25
    Caption = #1057#1086#1079#1076#1072#1090#1100' '#1084#1072#1090#1088#1080#1094#1091
    TabOrder = 0
    OnClick = Button1Click
  end
  object Edit1: TEdit
    Left = 142
    Top = 38
    Width = 99
    Height = 21
    TabOrder = 1
    Text = '1'
  end
  object Edit2: TEdit
    Left = 142
    Top = 65
    Width = 99
    Height = 21
    TabOrder = 2
    Text = '1'
  end
  object StrGrid: TStringGrid
    Left = 8
    Top = 92
    Width = 249
    Height = 311
    DefaultColWidth = 30
    DefaultRowHeight = 18
    FixedCols = 0
    FixedRows = 0
    TabOrder = 3
  end
  object UpDownN: TUpDown
    Left = 241
    Top = 38
    Width = 16
    Height = 21
    Associate = Edit1
    Position = 1
    TabOrder = 4
  end
  object UpDownM: TUpDown
    Left = 241
    Top = 65
    Width = 16
    Height = 21
    Associate = Edit2
    Position = 1
    TabOrder = 5
  end
  object StrGrid2: TStringGrid
    Left = 263
    Top = 92
    Width = 249
    Height = 311
    DefaultColWidth = 30
    DefaultRowHeight = 18
    FixedCols = 0
    FixedRows = 0
    TabOrder = 6
  end
  object UpDownM2: TUpDown
    Left = 501
    Top = 65
    Width = 16
    Height = 21
    Associate = Edit3
    Position = 1
    TabOrder = 7
  end
  object UpDownN2: TUpDown
    Left = 501
    Top = 38
    Width = 16
    Height = 21
    Associate = Edit4
    Position = 1
    TabOrder = 8
  end
  object Edit3: TEdit
    Left = 402
    Top = 65
    Width = 99
    Height = 21
    TabOrder = 9
    Text = '1'
  end
  object Edit4: TEdit
    Left = 402
    Top = 38
    Width = 99
    Height = 21
    TabOrder = 10
    Text = '1'
  end
  object Button2: TButton
    Left = 268
    Top = 8
    Width = 249
    Height = 25
    Caption = #1057#1086#1079#1076#1072#1090#1100' '#1084#1072#1090#1088#1080#1094#1091
    TabOrder = 11
    OnClick = Button2Click
  end
  object Button3: TButton
    Left = 523
    Top = 8
    Width = 75
    Height = 25
    Caption = #1059#1084#1085#1086#1078#1077#1085#1080#1077
    TabOrder = 12
    OnClick = Button3Click
  end
  object StrGridResult: TStringGrid
    Left = 619
    Top = 92
    Width = 249
    Height = 311
    DefaultColWidth = 30
    DefaultRowHeight = 18
    FixedCols = 0
    FixedRows = 0
    TabOrder = 13
  end
end
