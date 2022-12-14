object MainForm: TMainForm
  Left = 0
  Top = 0
  Caption = 'MainForm'
  ClientHeight = 620
  ClientWidth = 632
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
  object LabelAST: TLabel
    Left = 8
    Top = 117
    Width = 194
    Height = 13
    Caption = #1040#1073#1089#1090#1088#1072#1082#1090#1085#1086#1077' '#1089#1080#1085#1090#1072#1082#1089#1080#1095#1077#1089#1082#1080#1077' '#1076#1077#1088#1077#1074#1086':'
  end
  object LabelError: TLabel
    Left = 8
    Top = 371
    Width = 117
    Height = 13
    Caption = #1057#1086#1086#1073#1097#1077#1085#1080#1077' '#1086#1073#1086#1096#1080#1073#1082#1072#1093
  end
  object TreeView1: TTreeView
    Left = 8
    Top = 136
    Width = 349
    Height = 229
    Indent = 19
    TabOrder = 0
  end
  object ButtonReturn: TButton
    Left = 469
    Top = 41
    Width = 35
    Height = 32
    Caption = '='
    TabOrder = 1
    OnClick = ButtonReturnClick
  end
  object EditReturnValue: TEdit
    Left = 513
    Top = 46
    Width = 111
    Height = 21
    TabOrder = 2
  end
  object MemoError: TMemo
    Left = 8
    Top = 390
    Width = 616
    Height = 222
    ScrollBars = ssVertical
    TabOrder = 3
  end
  object MemoAST: TMemo
    Left = 363
    Top = 136
    Width = 261
    Height = 229
    ScrollBars = ssVertical
    TabOrder = 4
  end
  object MemoScript: TMemo
    Left = 8
    Top = 8
    Width = 455
    Height = 103
    Lines.Strings = (
      'x = 1'
      'y = 2'
      'result = x + y')
    ScrollBars = ssVertical
    TabOrder = 5
  end
end
