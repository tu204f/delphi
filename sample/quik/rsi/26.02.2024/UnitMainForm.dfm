object MainForm: TMainForm
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'MainForm'
  ClientHeight = 141
  ClientWidth = 265
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -15
  Font.Name = 'Tahoma'
  Font.Style = [fsBold]
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnShow = FormShow
  DesignSize = (
    265
    141)
  PixelsPerInch = 96
  TextHeight = 18
  object ButtonTolls: TButton
    Left = 1
    Top = 2
    Width = 107
    Height = 25
    Caption = #1048#1085#1089#1090#1088#1091#1084#1077#1085#1090
    TabOrder = 0
    OnClick = ButtonTollsClick
  end
  object EditSecurity: TEdit
    Left = 114
    Top = 1
    Width = 89
    Height = 36
    Color = clBtnFace
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -23
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    ReadOnly = True
    TabOrder = 1
    Text = '000'
  end
  object ButtonQuikTable: TButton
    Left = 209
    Top = 2
    Width = 47
    Height = 25
    Anchors = [akTop]
    Caption = 'QUIK'
    TabOrder = 2
    OnClick = ButtonQuikTableClick
  end
  object GridPanel: TGridPanel
    Left = 1
    Top = 28
    Width = 262
    Height = 112
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
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -15
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    RowCollection = <
      item
        Value = 100.000000000000000000
      end>
    TabOrder = 3
    object PanelLeft: TPanel
      Left = 1
      Top = 1
      Width = 130
      Height = 110
      Align = alClient
      BevelInner = bvLowered
      BevelKind = bkFlat
      BevelOuter = bvSpace
      TabOrder = 0
      inline UserOrderBuy: TUserOrderFrame
        Left = 2
        Top = 2
        Width = 122
        Height = 102
        Align = alClient
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -21
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 0
        ExplicitLeft = 2
        ExplicitTop = 2
        ExplicitHeight = 102
        inherited ledValueRSI: TLabeledEdit
          Height = 33
          EditLabel.Width = 46
          EditLabel.Height = 25
          EditLabel.ExplicitLeft = 3
          EditLabel.ExplicitTop = 42
          EditLabel.ExplicitWidth = 46
          EditLabel.ExplicitHeight = 25
          ExplicitHeight = 33
        end
        inherited UpDownStepPrice: TUpDown
          Height = 33
          ExplicitHeight = 33
        end
        inherited ledStepPrice: TLabeledEdit
          Height = 33
          EditLabel.Width = 38
          EditLabel.Height = 25
          EditLabel.ExplicitLeft = 11
          EditLabel.ExplicitTop = 221
          EditLabel.ExplicitWidth = 38
          EditLabel.ExplicitHeight = 25
          ExplicitHeight = 33
        end
        inherited UpDownValueRSI: TUpDown
          Height = 33
          ExplicitHeight = 33
        end
        inherited UpDownQuantity: TUpDown
          Height = 33
          ExplicitHeight = 33
        end
        inherited ledQuantity: TLabeledEdit
          Height = 33
          EditLabel.Width = 23
          EditLabel.Height = 25
          EditLabel.ExplicitLeft = 2
          EditLabel.ExplicitTop = 9
          EditLabel.ExplicitWidth = 23
          EditLabel.ExplicitHeight = 25
          ExplicitHeight = 33
        end
      end
    end
    object PanelRight: TPanel
      Left = 131
      Top = 1
      Width = 130
      Height = 110
      Align = alClient
      BevelInner = bvLowered
      BevelKind = bkFlat
      BevelOuter = bvSpace
      TabOrder = 1
      inline UserOrderSell: TUserOrderFrame
        Left = 2
        Top = 2
        Width = 122
        Height = 102
        Align = alClient
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -21
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 0
        ExplicitLeft = 2
        ExplicitTop = 2
        ExplicitHeight = 102
        inherited ledValueRSI: TLabeledEdit
          Height = 33
          EditLabel.Width = 46
          EditLabel.Height = 25
          EditLabel.ExplicitLeft = 3
          EditLabel.ExplicitTop = 42
          EditLabel.ExplicitWidth = 46
          EditLabel.ExplicitHeight = 25
          ExplicitHeight = 33
        end
        inherited UpDownStepPrice: TUpDown
          Height = 33
          ExplicitHeight = 33
        end
        inherited ledStepPrice: TLabeledEdit
          Height = 33
          EditLabel.Width = 38
          EditLabel.Height = 25
          EditLabel.ExplicitLeft = 11
          EditLabel.ExplicitTop = 221
          EditLabel.ExplicitWidth = 38
          EditLabel.ExplicitHeight = 25
          ExplicitHeight = 33
        end
        inherited UpDownValueRSI: TUpDown
          Height = 33
          ExplicitHeight = 33
        end
        inherited UpDownQuantity: TUpDown
          Height = 33
          ExplicitHeight = 33
        end
        inherited ledQuantity: TLabeledEdit
          Height = 33
          EditLabel.Width = 23
          EditLabel.Height = 25
          EditLabel.ExplicitLeft = 2
          EditLabel.ExplicitTop = 9
          EditLabel.ExplicitWidth = 23
          EditLabel.ExplicitHeight = 25
          ExplicitHeight = 33
        end
      end
    end
  end
  object Timer: TTimer
    Interval = 500
    OnTimer = TimerTimer
    Left = 32
    Top = 96
  end
end
