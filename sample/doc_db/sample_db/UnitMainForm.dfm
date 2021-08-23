object MainForm: TMainForm
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'MainForm'
  ClientHeight = 552
  ClientWidth = 602
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 44
    Width = 35
    Height = 13
    Caption = #1055#1072#1087#1082#1080':'
  end
  object Label2: TLabel
    Left = 303
    Top = 44
    Width = 30
    Height = 13
    Caption = #1060#1072#1081#1083':'
  end
  object LabelPath: TLabel
    Left = 8
    Top = 482
    Width = 47
    Height = 13
    Caption = 'LabelPath'
  end
  object LabelPathFolderID: TLabel
    Left = 8
    Top = 501
    Width = 88
    Height = 13
    Caption = 'LabelPathFolderID'
  end
  object ListBoxFolder: TListBox
    Left = 8
    Top = 63
    Width = 289
    Height = 382
    ItemHeight = 13
    TabOrder = 0
    OnClick = ListBoxFolderClick
  end
  object ButtonOpenDB: TButton
    Left = 8
    Top = 8
    Width = 105
    Height = 25
    Caption = #1054#1090#1082#1088#1099#1090#1100
    TabOrder = 1
    OnClick = ButtonOpenDBClick
  end
  object ButtonCloseDB: TButton
    Left = 119
    Top = 8
    Width = 105
    Height = 25
    Caption = #1047#1072#1082#1088#1099#1090#1100
    TabOrder = 2
    OnClick = ButtonCloseDBClick
  end
  object ListBoxDoc: TListBox
    Left = 303
    Top = 63
    Width = 289
    Height = 382
    ItemHeight = 13
    TabOrder = 3
  end
  object ButtonAddFolder: TButton
    Left = 8
    Top = 451
    Width = 105
    Height = 25
    Caption = #1044#1086#1073#1072#1074#1080#1090#1100' '#1087#1072#1087#1082#1091
    TabOrder = 4
    OnClick = ButtonAddFolderClick
  end
  object ButtonRenameFolder: TButton
    Left = 116
    Top = 451
    Width = 105
    Height = 25
    Caption = #1055#1077#1088#1077#1080#1084#1077#1085#1086#1074#1072#1090#1100
    TabOrder = 5
    OnClick = ButtonRenameFolderClick
  end
  object ButtonDeletedFolder: TButton
    Left = 222
    Top = 451
    Width = 75
    Height = 25
    Caption = #1059#1076#1072#1083#1080#1090#1100
    TabOrder = 6
    OnClick = ButtonDeletedFolderClick
  end
  object ButtonOpenFolder: TButton
    Left = 303
    Top = 451
    Width = 114
    Height = 25
    Caption = #1054#1090#1082#1088#1099#1090#1100' '#1087#1072#1087#1082#1091
    TabOrder = 7
    OnClick = ButtonOpenFolderClick
  end
  object ButtonFolderRedo: TButton
    Left = 420
    Top = 451
    Width = 121
    Height = 25
    Caption = #1053#1072#1079#1072#1076
    TabOrder = 8
    OnClick = ButtonFolderRedoClick
  end
  object ButtonNewDocument: TButton
    Left = 8
    Top = 520
    Width = 117
    Height = 25
    Caption = #1053#1086#1074#1099#1081' '#1076#1086#1082#1091#1084#1077#1085#1090
    TabOrder = 9
    OnClick = ButtonNewDocumentClick
  end
  object ButtonOpenDocument: TButton
    Left = 119
    Top = 519
    Width = 117
    Height = 25
    Caption = #1054#1090#1082#1088#1099#1090#1100' '#1076#1086#1082#1091#1084#1077#1085#1090
    TabOrder = 10
    OnClick = ButtonOpenDocumentClick
  end
  object ButtonCloseDocument: TButton
    Left = 240
    Top = 520
    Width = 117
    Height = 25
    Caption = #1047#1072#1082#1088#1099#1090#1100' '#1076#1086#1082#1091#1084#1077#1085#1090
    TabOrder = 11
    OnClick = ButtonCloseDocumentClick
  end
  object ButtonDeleteDocument: TButton
    Left = 356
    Top = 519
    Width = 117
    Height = 25
    Caption = #1059#1076#1072#1083#1080#1090#1100' '#1076#1086#1082#1091#1084#1077#1085#1090
    TabOrder = 12
    OnClick = ButtonDeleteDocumentClick
  end
end
