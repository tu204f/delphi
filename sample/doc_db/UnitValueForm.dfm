object ValueForm: TValueForm
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'ValueForm'
  ClientHeight = 65
  ClientWidth = 335
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  PixelsPerInch = 96
  TextHeight = 13
  object ButtonApply: TButton
    Left = 171
    Top = 35
    Width = 75
    Height = 25
    Caption = #1054#1050
    ModalResult = 1
    TabOrder = 1
    OnClick = ButtonApplyClick
  end
  object ButtonCancel: TButton
    Left = 252
    Top = 35
    Width = 75
    Height = 25
    Caption = #1054#1090#1084#1077#1085#1072
    ModalResult = 2
    TabOrder = 2
    OnClick = ButtonCancelClick
  end
  object ValueEdit: TEdit
    Left = 8
    Top = 8
    Width = 319
    Height = 21
    Hint = #1042#1074#1080#1076#1080#1090#1077' '#1079#1085#1072#1095#1077#1085#1080#1077
    TabOrder = 0
  end
end
