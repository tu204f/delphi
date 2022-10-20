object ConnectQuikForm: TConnectQuikForm
  Left = 0
  Top = 0
  Caption = #1055#1088#1086#1074#1077#1088#1082#1072' '#1089#1086#1077#1076#1080#1085#1077#1085#1080#1077' QUIK'
  ClientHeight = 582
  ClientWidth = 535
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object LabelStatus: TLabel
    Left = 8
    Top = 560
    Width = 520
    Height = 13
    AutoSize = False
    Caption = #1057#1086#1089#1090#1086#1085#1080#1077' '#1079#1072#1075#1088#1091#1079#1082#1072':'
  end
  object Button1: TButton
    Left = 8
    Top = 529
    Width = 133
    Height = 25
    Caption = #1043#1077#1085#1077#1088#1072#1090#1086#1088' '#1057#1082#1088#1080#1087#1090#1072' 1'
    TabOrder = 0
  end
  object Button2: TButton
    Left = 147
    Top = 529
    Width = 133
    Height = 25
    Caption = #1043#1077#1085#1077#1088#1072#1090#1086#1088' '#1057#1082#1088#1080#1087#1090#1072' 2'
    TabOrder = 1
    OnClick = Button2Click
  end
  object ButtonSource1: TButton
    Left = 8
    Top = 498
    Width = 133
    Height = 25
    Caption = #1048#1089#1090#1086#1095#1085#1080#1082' '#1076#1072#1085#1085#1099#1093' 1'
    TabOrder = 2
  end
  object StrGrid: TStringGrid
    Left = 8
    Top = 8
    Width = 489
    Height = 484
    DefaultRowHeight = 18
    FixedCols = 0
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goRowSelect, goFixedRowDefAlign]
    TabOrder = 3
  end
  object Timer1: TTimer
    OnTimer = Timer1Timer
    Left = 252
    Top = 32
  end
end
