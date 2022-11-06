object QuikTableGridForm: TQuikTableGridForm
  Left = 0
  Top = 0
  Caption = #1058#1072#1073#1083#1080#1094#1072' '#8212' '#1090#1088#1072#1085#1089#1083#1080#1088#1091#1077#1084#1099#1077' QUIK'
  ClientHeight = 417
  ClientWidth = 767
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
  object TabControl: TTabControl
    Left = 0
    Top = 0
    Width = 767
    Height = 417
    Align = alClient
    TabOrder = 0
    Tabs.Strings = (
      'Security ('#1060#1080#1085#1072#1085#1089#1086#1074#1099#1081' '#1080#1085#1089#1090#1088#1091#1084#1077#1085#1090#1099')'
      'Trades ('#1055#1088#1086#1074#1077#1076#1077#1085#1085#1099#1077' '#1089#1076#1077#1083#1082#1080')'
      'Orders ('#1055#1088#1086#1074#1077#1076#1077#1085#1085#1099#1077' '#1079#1072#1103#1074#1082#1080')'
      'Stop Orders ('#1057#1090#1086#1087' '#1079#1072#1103#1074#1082#1072')')
    TabIndex = 0
    OnChange = TabControlChange
    object StringGrid: TStringGrid
      Left = 4
      Top = 24
      Width = 759
      Height = 389
      Align = alClient
      DefaultRowHeight = 18
      FixedCols = 0
      Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goColSizing, goRowSelect, goFixedRowDefAlign]
      TabOrder = 0
    end
  end
  object Timer: TTimer
    Enabled = False
    Interval = 500
    OnTimer = TimerTimer
    Left = 324
    Top = 132
  end
end
