object MainForm: TMainForm
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = #1069#1082#1089#1087#1086#1088#1090' '#1073#1072#1079#1099' '#1076#1072#1085#1085#1099#1093
  ClientHeight = 380
  ClientWidth = 565
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
  object TreeView: TTreeView
    Left = 8
    Top = 8
    Width = 549
    Height = 197
    Indent = 19
    TabOrder = 0
  end
  object Button1: TButton
    Left = 8
    Top = 216
    Width = 75
    Height = 25
    Caption = 'Button1'
    TabOrder = 1
    OnClick = Button1Click
  end
  object ProgressBar: TProgressBar
    Left = 8
    Top = 247
    Width = 549
    Height = 17
    TabOrder = 2
  end
  object Timer: TTimer
    Enabled = False
    Interval = 100
    OnTimer = TimerTimer
    Left = 340
    Top = 140
  end
end
