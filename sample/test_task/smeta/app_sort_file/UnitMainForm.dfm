object MainForm: TMainForm
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'MainForm'
  ClientHeight = 233
  ClientWidth = 383
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object LabelStatus: TLabel
    Left = 8
    Top = 186
    Width = 120
    Height = 13
    Caption = #1057#1086#1089#1090#1086#1103#1085#1080#1077' '#1089#1086#1088#1090#1080#1088#1086#1074#1082#1080':'
  end
  object Label1: TLabel
    Left = 8
    Top = 46
    Width = 58
    Height = 13
    Caption = #1048#1084#1103' '#1092#1072#1081#1083#1072':'
  end
  object Label2: TLabel
    Left = 8
    Top = 92
    Width = 68
    Height = 13
    Caption = #1048#1084#1103' '#1080#1085#1076#1077#1082#1089#1072':'
  end
  object Label3: TLabel
    Left = 8
    Top = 141
    Width = 146
    Height = 13
    Caption = #1048#1084#1103' '#1086#1090#1089#1086#1088#1090#1080#1088#1086#1074#1072#1085#1085#1099#1081' '#1092#1072#1081#1083':'
  end
  object ButtonOpen: TButton
    Left = 4
    Top = 8
    Width = 75
    Height = 25
    Caption = #1054#1090#1082#1088#1099#1090#1100
    TabOrder = 0
    OnClick = ButtonOpenClick
  end
  object ButtonStart: TButton
    Left = 85
    Top = 8
    Width = 75
    Height = 25
    Caption = #1057#1090#1072#1088#1090
    TabOrder = 1
    OnClick = ButtonStartClick
  end
  object EditFileName: TEdit
    Left = 8
    Top = 65
    Width = 367
    Height = 21
    Color = clBtnFace
    ReadOnly = True
    TabOrder = 2
  end
  object EditFileNameIndex: TEdit
    Left = 8
    Top = 111
    Width = 367
    Height = 21
    Color = clBtnFace
    ReadOnly = True
    TabOrder = 3
  end
  object EditFileNameSort: TEdit
    Left = 8
    Top = 158
    Width = 367
    Height = 22
    Color = clBtnFace
    ReadOnly = True
    TabOrder = 4
  end
  object ProgressBar: TProgressBar
    Left = 8
    Top = 204
    Width = 367
    Height = 17
    TabOrder = 5
  end
  object OpenDialog: TOpenDialog
    Filter = '|*.txt'
    Left = 212
    Top = 12
  end
end
