object DocumentFileNameForm: TDocumentFileNameForm
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = #1048#1084#1103' '#1089#1082#1088#1080#1087#1090#1072' '
  ClientHeight = 66
  ClientWidth = 543
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object ScriptEdit: TEdit
    Left = 8
    Top = 8
    Width = 527
    Height = 21
    TabOrder = 0
  end
  object ButtonOk: TButton
    Left = 460
    Top = 35
    Width = 75
    Height = 25
    Caption = #1061#1086#1088#1086#1096#1086
    TabOrder = 1
    OnClick = ButtonOkClick
  end
end
