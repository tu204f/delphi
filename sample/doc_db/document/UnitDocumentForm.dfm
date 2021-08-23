object DocumentForm: TDocumentForm
  Left = 0
  Top = 0
  Caption = #1044#1086#1082#1091#1084#1077#1085#1090
  ClientHeight = 474
  ClientWidth = 725
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  DesignSize = (
    725
    474)
  PixelsPerInch = 96
  TextHeight = 13
  object Memo: TMemo
    Left = 8
    Top = 8
    Width = 709
    Height = 428
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 0
    ExplicitWidth = 702
    ExplicitHeight = 425
  end
  object ButtonExecute: TButton
    Left = 635
    Top = 442
    Width = 82
    Height = 28
    Anchors = [akRight, akBottom]
    Caption = #1042#1099#1087#1086#1083#1100#1085#1080#1090#1100
    TabOrder = 1
  end
  object ButtonLoad: TButton
    Left = 8
    Top = 442
    Width = 75
    Height = 28
    Anchors = [akLeft, akBottom]
    Caption = #1047#1072#1075#1088#1091#1079#1080#1090#1100
    TabOrder = 2
  end
  object ButtonSave: TButton
    Left = 85
    Top = 442
    Width = 75
    Height = 28
    Anchors = [akLeft, akBottom]
    Caption = #1057#1086#1093#1088#1072#1085#1080#1090#1100
    TabOrder = 3
  end
end
