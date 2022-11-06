object ConnectQuikForm: TConnectQuikForm
  Left = 0
  Top = 0
  Caption = #1055#1088#1086#1074#1077#1088#1082#1072' '#1089#1086#1077#1076#1080#1085#1077#1085#1080#1077' QUIK'
  ClientHeight = 465
  ClientWidth = 605
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnShow = FormShow
  DesignSize = (
    605
    465)
  PixelsPerInch = 96
  TextHeight = 13
  object LabelStatus: TLabel
    Left = 8
    Top = 443
    Width = 590
    Height = 13
    Anchors = [akLeft, akRight, akBottom]
    AutoSize = False
    Caption = #1057#1086#1089#1090#1086#1085#1080#1077' '#1079#1072#1075#1088#1091#1079#1082#1072':'
    ExplicitTop = 498
    ExplicitWidth = 520
  end
  object Label1: TLabel
    Left = 8
    Top = 11
    Width = 79
    Height = 13
    Caption = #1057#1087#1080#1089#1086#1082' '#1090#1072#1073#1083#1080#1094':'
  end
  object StrGrid: TStringGrid
    Left = 8
    Top = 35
    Width = 589
    Height = 402
    Anchors = [akLeft, akTop, akRight, akBottom]
    DefaultRowHeight = 18
    FixedCols = 0
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goRowSelect, goFixedRowDefAlign]
    TabOrder = 0
  end
  object ComboBoxQuikTables: TComboBox
    Left = 93
    Top = 8
    Width = 241
    Height = 21
    Style = csDropDownList
    TabOrder = 1
  end
  object ButtonUpData: TButton
    Left = 522
    Top = 4
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = #1054#1073#1085#1086#1074#1080#1090#1100
    TabOrder = 2
    OnClick = ButtonUpDataClick
  end
  object StartTimer: TButton
    Left = 441
    Top = 4
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = #1057#1090#1072#1088#1090
    TabOrder = 3
    OnClick = StartTimerClick
  end
  object Timer: TTimer
    Enabled = False
    OnTimer = TimerTimer
    Left = 112
    Top = 128
  end
end
