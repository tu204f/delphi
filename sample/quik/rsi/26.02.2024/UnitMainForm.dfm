object MainForm: TMainForm
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'MainForm'
  ClientHeight = 171
  ClientWidth = 507
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
    507
    171)
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
  object EditSecurity: TEdit
    Left = 155
    Top = 8
    Width = 221
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    Color = clBtnFace
    ReadOnly = True
    TabOrder = 1
    ExplicitWidth = 351
  end
  object ButtonQuikTable: TButton
    Left = 382
    Top = 8
    Width = 117
    Height = 25
    Anchors = [akTop, akRight]
    Caption = #1058#1072#1073#1083#1080#1094#1072' QUIK'
    TabOrder = 2
    OnClick = ButtonQuikTableClick
    ExplicitLeft = 512
  end
  object GridPanel: TGridPanel
    Left = 10
    Top = 39
    Width = 489
    Height = 124
    Anchors = [akLeft, akTop, akRight, akBottom]
    ColumnCollection = <
      item
        Value = 50.000000000000000000
      end
      item
        Value = 50.000000000000000000
      end>
    ControlCollection = <
      item
        Column = 0
        Control = PanelLeft
        Row = 0
      end
      item
        Column = 1
        Control = PanelRight
        Row = 0
      end>
    RowCollection = <
      item
        Value = 100.000000000000000000
      end>
    TabOrder = 3
    ExplicitWidth = 494
    ExplicitHeight = 175
    object PanelLeft: TPanel
      Left = 1
      Top = 1
      Width = 244
      Height = 122
      Align = alClient
      BevelInner = bvLowered
      BevelKind = bkFlat
      BevelOuter = bvSpace
      TabOrder = 0
      inline UserOrderFrame1: TUserOrderFrame
        Left = 2
        Top = 2
        Width = 236
        Height = 114
        Align = alClient
        TabOrder = 0
        ExplicitLeft = 3
        ExplicitTop = 52
        inherited ledValueRSI: TLabeledEdit
          EditLabel.ExplicitLeft = 0
          EditLabel.ExplicitTop = -16
          EditLabel.ExplicitWidth = 57
        end
        inherited UpDownStepPrice: TUpDown
          Top = 64
          ExplicitTop = 64
        end
        inherited ledStepPrice: TLabeledEdit
          EditLabel.ExplicitLeft = 0
          EditLabel.ExplicitTop = -16
          EditLabel.ExplicitWidth = 59
        end
        inherited ledQuantity: TLabeledEdit
          EditLabel.ExplicitLeft = 0
          EditLabel.ExplicitTop = -16
          EditLabel.ExplicitWidth = 56
        end
      end
    end
    object PanelRight: TPanel
      Left = 245
      Top = 1
      Width = 243
      Height = 122
      Align = alClient
      BevelInner = bvLowered
      BevelKind = bkFlat
      BevelOuter = bvSpace
      TabOrder = 1
      inline UserOrderFrame2: TUserOrderFrame
        Left = 2
        Top = 2
        Width = 235
        Height = 114
        Align = alClient
        TabOrder = 0
        ExplicitLeft = 3
        ExplicitTop = 52
        inherited ledValueRSI: TLabeledEdit
          EditLabel.ExplicitLeft = 0
          EditLabel.ExplicitTop = -16
          EditLabel.ExplicitWidth = 57
        end
        inherited UpDownStepPrice: TUpDown
          Top = 64
          ExplicitTop = 64
        end
        inherited ledStepPrice: TLabeledEdit
          EditLabel.ExplicitLeft = 0
          EditLabel.ExplicitTop = -16
          EditLabel.ExplicitWidth = 59
        end
        inherited ledQuantity: TLabeledEdit
          EditLabel.ExplicitLeft = 0
          EditLabel.ExplicitTop = -16
          EditLabel.ExplicitWidth = 56
        end
      end
    end
  end
  object Timer: TTimer
    Interval = 500
    OnTimer = TimerTimer
    Left = 264
    Top = 12
  end
end
