object MainForm: TMainForm
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'MainForm'
  ClientHeight = 602
  ClientWidth = 829
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object ButtonLoad: TButton
    Left = 8
    Top = 8
    Width = 75
    Height = 25
    Caption = #1047#1072#1075#1088#1091#1079#1082#1072
    TabOrder = 0
    OnClick = ButtonLoadClick
  end
  object MemoLog: TMemo
    Left = 8
    Top = 446
    Width = 813
    Height = 148
    ScrollBars = ssVertical
    TabOrder = 1
  end
  object ButtonStart: TButton
    Left = 84
    Top = 8
    Width = 75
    Height = 25
    Caption = #1057#1090#1072#1088#1090
    TabOrder = 2
    OnClick = ButtonStartClick
  end
  object StrGrid: TStringGrid
    Left = 8
    Top = 99
    Width = 813
    Height = 341
    ColCount = 10
    DefaultRowHeight = 18
    FixedCols = 0
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goRowSelect, goFixedRowDefAlign]
    TabOrder = 3
  end
  object CurrentStrGrid: TStringGrid
    Left = 8
    Top = 39
    Width = 813
    Height = 54
    ColCount = 10
    DefaultRowHeight = 18
    FixedCols = 0
    RowCount = 2
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goRowSelect, goFixedRowDefAlign]
    TabOrder = 4
  end
  object Timer: TTimer
    Enabled = False
    Interval = 500
    OnTimer = TimerTimer
    Left = 280
    Top = 100
  end
end
