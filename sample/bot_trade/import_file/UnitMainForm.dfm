object MainForm: TMainForm
  Left = 0
  Top = 0
  Caption = #1057#1086#1079#1076#1072#1085#1080#1077' '#1092#1072#1081#1083#1072' '#1074#1077#1082#1090#1086#1088#1072
  ClientHeight = 406
  ClientWidth = 689
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  DesignSize = (
    689
    406)
  PixelsPerInch = 96
  TextHeight = 13
  object MemoLines: TMemo
    Left = 8
    Top = 39
    Width = 673
    Height = 359
    Anchors = [akLeft, akTop, akRight, akBottom]
    ScrollBars = ssVertical
    TabOrder = 0
  end
  object ButtonStart: TButton
    Left = 8
    Top = 8
    Width = 75
    Height = 25
    Caption = #1057#1090#1072#1088#1090
    TabOrder = 1
    OnClick = ButtonStartClick
  end
  object ButtonLoad: TButton
    Left = 89
    Top = 8
    Width = 120
    Height = 25
    Caption = #1058#1077#1089#1090#1086#1074#1072#1103' '#1079#1072#1075#1088#1091#1079#1082#1072
    TabOrder = 2
  end
  object ProgressBar1: TProgressBar
    Left = 215
    Top = 8
    Width = 466
    Height = 25
    TabOrder = 3
  end
end
