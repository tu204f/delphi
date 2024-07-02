object QuikTableForm: TQuikTableForm
  Left = 0
  Top = 0
  Caption = #1058#1072#1073#1083#1080#1094#1072' QUIK'
  ClientHeight = 460
  ClientWidth = 757
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnShow = FormShow
  DesignSize = (
    757
    460)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 11
    Width = 231
    Height = 13
    Caption = #1057#1087#1080#1089#1086#1082' '#1090#1072#1073#1083#1080#1094' QUIK, '#1082#1086#1090#1086#1088#1086#1077' '#1090#1088#1072#1085#1089#1083#1080#1088#1091#1077#1090#1089#1103
  end
  object StrGrid: TStringGrid
    Left = 8
    Top = 35
    Width = 741
    Height = 417
    Anchors = [akLeft, akTop, akRight, akBottom]
    DefaultRowHeight = 18
    FixedCols = 0
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goColSizing, goRowSelect, goFixedRowDefAlign]
    TabOrder = 0
  end
  object ComboBoxTableQUIK: TComboBox
    Left = 245
    Top = 8
    Width = 145
    Height = 21
    TabOrder = 1
    OnChange = ComboBoxTableQUIKChange
  end
  object ButtonUpData: TButton
    Left = 674
    Top = 4
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = #1054#1073#1085#1086#1074#1080#1090#1100
    TabOrder = 2
    OnClick = ButtonUpDataClick
  end
  object Timer: TTimer
    OnTimer = TimerTimer
    Left = 316
    Top = 172
  end
end
