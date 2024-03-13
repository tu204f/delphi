object MainForm: TMainForm
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'MainForm'
  ClientHeight = 238
  ClientWidth = 558
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  DesignSize = (
    558
    238)
  PixelsPerInch = 96
  TextHeight = 13
  object ButtonTolls: TButton
    Left = 8
    Top = 8
    Width = 141
    Height = 25
    Caption = #1053#1072#1089#1090#1088#1086#1081#1082#1080' | '#1048#1085#1089#1090#1088#1091#1084#1077#1085#1090
    TabOrder = 0
    OnClick = ButtonTollsClick
  end
  object StrGrid: TStringGrid
    Left = 8
    Top = 39
    Width = 544
    Height = 162
    Anchors = [akLeft, akTop, akRight, akBottom]
    ColCount = 8
    DefaultRowHeight = 18
    FixedCols = 0
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goRowSelect, goFixedRowDefAlign]
    TabOrder = 1
  end
  object EditSecurity: TEdit
    Left = 155
    Top = 10
    Width = 399
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    Color = clBtnFace
    ReadOnly = True
    TabOrder = 2
  end
  object Button1: TButton
    Left = 8
    Top = 207
    Width = 75
    Height = 25
    Caption = #1044#1086#1073#1072#1074#1080#1090#1100
    TabOrder = 3
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 89
    Top = 207
    Width = 75
    Height = 25
    Caption = #1059#1076#1072#1083#1080#1090#1100
    TabOrder = 4
    OnClick = Button2Click
  end
  object ButtonQuikTable: TButton
    Left = 433
    Top = 205
    Width = 117
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = #1058#1072#1073#1083#1080#1094#1072' QUIK'
    TabOrder = 5
    OnClick = ButtonQuikTableClick
  end
  object Timer: TTimer
    Interval = 500
    OnTimer = TimerTimer
    Left = 276
    Top = 108
  end
end
