object MainForm: TMainForm
  Left = 0
  Top = 0
  Caption = #1057#1087#1080#1089#1086#1082' '#1090#1088#1077#1081#1076#1086#1088#1086
  ClientHeight = 374
  ClientWidth = 599
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
  DesignSize = (
    599
    374)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 39
    Width = 98
    Height = 13
    Caption = #1057#1087#1080#1089#1086#1082' '#1090#1088#1077#1081#1076#1077#1088#1086#1074':'
  end
  object Label2: TLabel
    Left = 187
    Top = 61
    Width = 31
    Height = 13
    Caption = 'Label2'
  end
  object ComboBoxTrades: TComboBox
    Left = 8
    Top = 58
    Width = 173
    Height = 21
    TabOrder = 0
    Text = '***'
    OnClick = ComboBoxTradesClick
  end
  object ListBoxTikers: TListBox
    Left = 8
    Top = 85
    Width = 210
    Height = 281
    Anchors = [akLeft, akTop, akBottom]
    ItemHeight = 13
    TabOrder = 1
  end
  object Button1: TButton
    Left = 8
    Top = 8
    Width = 210
    Height = 25
    Caption = #1057#1086#1073#1088#1072#1090#1100' '#1074#1089#1077' '#1089#1076#1077#1083#1082#1080
    TabOrder = 2
    OnClick = Button1Click
  end
end
